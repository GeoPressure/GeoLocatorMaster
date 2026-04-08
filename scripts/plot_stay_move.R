library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(readr)
library(ggplot2)
library(GeoPressureR)

# --- config ---
# Filter out long stopovers likely not migratory (tune per dataset).
max_migratory_stap_days <- 30
# Toggle: fit a separate hazard curve per species (by scientific_name).
fit_by_species <- FALSE
# Toggle: split per-species fits into spring vs autumn (based on dates).
split_by_season <- TRUE

# --- load + filter data ---
# Run from repo root so data paths resolve (or set your working directory).
staps0 <- read_csv("data/master/staps.csv", show_col_types = FALSE)
tags <- read_csv("data/master/tags.csv", show_col_types = FALSE)

staps <- staps0 %>%
  mutate(duration = stap2duration(staps0, units = "days")) %>%
  left_join(tags |> dplyr::select(tag_id, scientific_name), by = "tag_id") %>%
  filter(duration <= max_migratory_stap_days, is.na(known_lat)) %>%
  dplyr::select(tag_id, stap_id, start, end, scientific_name)

# ---- helper: generate noon timestamps within each stap ----
noon_seq <- function(t0, t1, hour = 12L, tz = "UTC") {
  d0 <- as_date(with_tz(t0, tz))
  d1 <- as_date(with_tz(t1, tz))

  tibble(
    date = seq(d0, d1, by = "day")
  ) %>%
    mutate(noon = as_datetime(date, tz = tz) + hours(hour)) %>%
    filter(noon >= t0, noon < t1)
}

# --- expand staps to daily records + movement flag ---
daily_staps <- staps %>%
  nest(data = c(start, end)) %>%
  mutate(grid = purrr::map(data, ~ noon_seq(.x$start, .x$end, hour = 12L))) %>%
  dplyr::select(-data) %>%
  unnest(grid) %>%
  group_by(tag_id, stap_id) %>%
  mutate(
    stopover_day = as.integer(difftime(noon, min(noon), units = "days"))
  ) %>%
  ungroup() %>%
  group_by(tag_id) %>%
  arrange(noon, .by_group = TRUE) %>%
  mutate(moved_next_day = lead(stap_id) != stap_id) %>%
  ungroup()

# --- simple season label from date (adjust if needed) ---
season_from_date <- function(x) {
  m <- month(x)
  case_when(
    m %in% 3:5 ~ "spring",
    m %in% 8:10 ~ "autumn",
    TRUE ~ NA_character_
  )
}

# Add season for optional splitting (based on daily noon timestamps).
daily_staps <- daily_staps %>%
  mutate(season = if (split_by_season) season_from_date(noon) else "all")
# --- hazard model helpers ---
inv_logit <- function(x) 1 / (1 + exp(-x))

hazard_exp_plateau <- function(t, p0, p_inf, tau) {
  p_inf + (p0 - p_inf) * exp(-t / tau)
}

fit_exp_plateau <- function(t, y, n) {
  # Drop any non-finite rows to avoid optim failures.
  keep <- is.finite(t) & is.finite(y) & is.finite(n) & n > 0
  t <- t[keep]
  y <- y[keep]
  n <- n[keep]
  if (length(t) < 2L) {
    return(list(
      p_inf = NA_real_,
      p0 = NA_real_,
      tau = NA_real_,
      converged = FALSE
    ))
  }

  nll <- function(par) {
    p_inf <- inv_logit(par[1])
    p0 <- inv_logit(par[2])
    tau <- exp(par[3])

    p0 <- p_inf + abs(p0 - p_inf)
    p <- hazard_exp_plateau(t, p0, p_inf, tau)
    p <- pmin(pmax(p, 1e-6), 1 - 1e-6)

    -sum(dbinom(y, n, p, log = TRUE))
  }

  p0_init <- pmin(pmax(y[1] / n[1], 0.2), 0.9)
  p_inf_init <- max(quantile(y / n, 0.2, na.rm = TRUE), 0.01)

  opt <- try(
    optim(
      c(qlogis(p_inf_init), qlogis(p0_init), log(5)),
      nll,
      method = "BFGS"
    ),
    silent = TRUE
  )
  if (inherits(opt, "try-error")) {
    return(list(
      p_inf = NA_real_,
      p0 = NA_real_,
      tau = NA_real_,
      converged = FALSE
    ))
  }

  list(
    p_inf = inv_logit(opt$par[1]),
    p0 = inv_logit(opt$par[2]),
    tau = exp(opt$par[3]),
    converged = opt$convergence == 0
  )
}

# --- empirical hazard + fit + plot ---
hazard_table <- function(df) {
  df %>%
    filter(!is.na(moved_next_day)) %>%
    group_by(stopover_day) %>%
    summarise(n = n(), y = sum(moved_next_day), p = y / n, .groups = "drop")
}

make_pred <- function(t, fit) {
  if (!length(t) || anyNA(c(fit$p0, fit$p_inf, fit$tau))) {
    return(tibble(stopover_day = numeric(0), p = numeric(0)))
  }
  tibble(stopover_day = seq(0, max(t))) %>%
    mutate(p = hazard_exp_plateau(stopover_day, fit$p0, fit$p_inf, fit$tau))
}

if (fit_by_species) {
  # Fit separate curves per species (optionally by season) and facet the plot.
  haz <- daily_staps %>%
    filter(!is.na(scientific_name), !is.na(season)) %>%
    group_by(scientific_name, season) %>%
    group_modify(~ hazard_table(.x)) %>%
    ungroup()

  # Global fit across all species (per season if split) for reference line in each facet.
  haz_all <- daily_staps %>%
    filter(!is.na(scientific_name), !is.na(season)) %>%
    group_by(season) %>%
    group_modify(~ hazard_table(.x)) %>%
    ungroup()

  pred_all <- haz_all %>%
    group_by(season) %>%
    group_modify(
      ~ {
        fit <- fit_exp_plateau(.x$stopover_day, .x$y, .x$n)
        make_pred(.x$stopover_day, fit)
      }
    ) %>%
    ungroup()

  pred <- haz %>%
    group_by(scientific_name, season) %>%
    group_modify(
      ~ {
        fit <- fit_exp_plateau(.x$stopover_day, .x$y, .x$n)
        make_pred(.x$stopover_day, fit)
      }
    ) %>%
    ungroup()

  ggplot(haz, aes(stopover_day, p, colour = season)) +
    geom_point(aes(size = n), alpha = 0.6) +
    geom_line(
      data = pred_all,
      aes(stopover_day, p, colour = season),
      inherit.aes = FALSE,
      linewidth = 0.9,
      linetype = "dashed"
    ) +
    geom_line(
      data = pred,
      aes(stopover_day, p, colour = season),
      linewidth = 1
    ) +
    scale_size(range = c(1.5, 4), guide = "none") +
    labs(
      x = "Stopover duration (days since arrival)",
      y = "Probability of moving next day",
      title = "Stopover departure probability by species and season",
      subtitle = "Exponential decay to plateau"
    ) +
    facet_wrap(~scientific_name) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
} else {
  # Fit a single curve across all tags.
  haz <- hazard_table(daily_staps)
  fit <- fit_exp_plateau(haz$stopover_day, haz$y, haz$n)
  pred <- make_pred(haz$stopover_day, fit)

  eq <- sprintf(
    "p(t) = %.3f + (%.3f − %.3f) · exp(−t / %.2f)",
    fit$p_inf,
    fit$p0,
    fit$p_inf,
    fit$tau
  )

  ggplot(haz, aes(stopover_day, p)) +
    geom_point(aes(size = n), colour = "grey30", alpha = 0.6) +
    geom_line(
      data = pred,
      aes(stopover_day, p),
      colour = "#0072B2",
      linewidth = 1.2
    ) +
    annotate(
      "text",
      x = Inf,
      y = Inf,
      label = eq,
      hjust = 1.05,
      vjust = 1.3,
      size = 4
    ) +
    scale_size(range = c(1.5, 5), guide = "none") +
    labs(
      x = "Stopover duration (days since arrival)",
      y = "Probability of moving next day",
      title = "Stopover departure probability",
      subtitle = "Exponential decay to plateau"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}
