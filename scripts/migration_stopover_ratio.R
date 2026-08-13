library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(readr)
library(ggplot2)
library(GeoPressureR)

staps <- read_csv("data/master_restricted/staps.csv", show_col_types = FALSE) |>
  arrange(tag_id, start, end, stap_id)
tags <- read_csv("data/master_restricted/tags.csv", show_col_types = FALSE)

extended_threshold_days <- 40 # days

staps$duration <- stap2duration(staps)

staps <- staps |>
  group_by(tag_id) |>
  mutate(
    boundary_stap = !is.na(known_lat) | duration >= extended_threshold_days,
    boundary_id = cumsum(boundary_stap),
    n_boundaries = sum(boundary_stap),
    migration_bout_id = if_else(
      !boundary_stap,
      paste(tag_id, boundary_id, sep = "_bout_"),
      NA_character_
    )
  ) |>
  ungroup()

flights <- staps |>
  group_by(tag_id) |>
  group_map(\(.x, .y) {
    stap2flight(.x, units = "days") |>
      as_tibble() |>
      mutate(tag_id = .y$tag_id, .before = 1)
  }) |>
  list_rbind()

flights <- flights |>
  left_join(
    staps |>
      select(tag_id, stap_t = stap_id, migration_bout_t = migration_bout_id),
    by = c("tag_id", "stap_t")
  ) |>
  left_join(
    staps |>
      select(tag_id, stap_s = stap_id, migration_bout_s = migration_bout_id),
    by = c("tag_id", "stap_s")
  ) |>
  mutate(
    migration_bout_id = coalesce(migration_bout_t, migration_bout_s)
  ) |>
  select(-migration_bout_t, -migration_bout_s)

flights |>
  filter(is.na(migration_bout_id))
# Two long stap that follow each other are discarding the flight in between.

## ------------------------------------------------------------
## Ratio per migration bout
## ------------------------------------------------------------

ratio_bout <- staps |>
  filter(!boundary_stap) |>
  group_by(tag_id, migration_bout_id) |>
  summarise(
    stopover_duration = sum(duration),
    n_stopovers = n(),
    .groups = "drop"
  ) |>
  inner_join(
    flights |>
      filter(!is.na(migration_bout_id)) |>
      group_by(tag_id, migration_bout_id) |>
      summarise(
        flight_duration = sum(duration),
        n_flights = sum(n),
        .groups = "drop"
      ),
    by = c("tag_id", "migration_bout_id")
  ) |>
  left_join(
    tags |>
      select(tag_id, scientific_name),
    by = "tag_id"
  ) |>
  mutate(
    ratio = stopover_duration / flight_duration,
    flight_fraction = flight_duration / (stopover_duration + flight_duration)
  ) |>
  filter(
    is.finite(ratio),
    ratio > 0
  )

## ------------------------------------------------------------
## Ratio per tag
## ------------------------------------------------------------

ratio_tag <- ratio_bout |>
  group_by(tag_id, scientific_name) |>
  summarise(
    n_bouts = n(),
    stopover_duration = sum(stopover_duration),
    flight_duration = sum(flight_duration),
    ratio = stopover_duration / flight_duration,
    .groups = "drop"
  )

## ------------------------------------------------------------
## Ratio per species
## ------------------------------------------------------------

ratio_species <- ratio_tag |>
  group_by(scientific_name) |>
  summarise(
    n_tags = n(),
    n_bouts = sum(n_bouts),
    stopover_duration = sum(stopover_duration),
    flight_duration = sum(flight_duration),
    ratio = stopover_duration / flight_duration,
    .groups = "drop"
  ) |>
  arrange(desc(n_tags))


## ------------------------------------------------------------
## Plots
## ------------------------------------------------------------

p_bout <- ratio_bout |>
  mutate(
    total_duration = stopover_duration + flight_duration,
    median_ratio = median(ratio, na.rm = TRUE)
  ) |>
  ggplot(aes(total_duration, ratio)) +
  geom_point(
    colour = "steelblue",
    size = 2,
    alpha = 0.75
  ) +
  geom_hline(
    yintercept = 7,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_hline(
    aes(yintercept = median_ratio),
    linetype = "solid",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = Inf,
    y = 7,
    label = "theoretical = 7",
    hjust = 1.05,
    vjust = -0.5,
    size = 3
  ) +
  annotate(
    "text",
    x = Inf,
    y = median(ratio_bout$ratio, na.rm = TRUE),
    label = glue::glue(
      "median = {round(median(ratio_bout$ratio, na.rm = TRUE), 2)}"
    ),
    hjust = 1.05,
    vjust = 1.4,
    size = 3
  ) +
  scale_y_log10() +
  labs(
    x = "Migration duration (days)",
    y = "Stopover : flight duration",
    title = "Stopover-to-flight ratio vs migration duration",
    subtitle = "Each point represents one migration bout"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank()
  )


p_tag <- ratio_tag |>
  mutate(
    total_duration = stopover_duration + flight_duration,
    median_ratio = median(ratio, na.rm = TRUE),
    tag_id = forcats::fct_reorder(tag_id, ratio)
  ) |>
  ggplot(aes(tag_id, ratio)) +
  geom_point(
    aes(size = total_duration),
    alpha = 0.7,
    colour = "steelblue"
  ) +
  geom_hline(
    yintercept = 7,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_hline(
    aes(yintercept = median_ratio),
    linetype = "solid",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = Inf,
    y = 7,
    label = "theoretical = 7",
    hjust = 1.05,
    vjust = -0.5,
    size = 3
  ) +
  annotate(
    "text",
    x = Inf,
    y = median(ratio_tag$ratio, na.rm = TRUE),
    label = glue::glue(
      "median = {round(median(ratio_tag$ratio, na.rm = TRUE), 2)}"
    ),
    hjust = 1.05,
    vjust = 1.4,
    size = 3
  ) +
  scale_y_log10() +
  labs(
    x = "Tag",
    y = "Stopover : flight duration",
    size = "Migration duration (days)",
    title = "Ratio per tag"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


p_species <- ratio_tag |>
  filter(!is.na(scientific_name)) |>
  mutate(
    total_duration = stopover_duration + flight_duration,
    scientific_name = forcats::fct_reorder(
      scientific_name,
      ratio,
      .fun = median
    )
  ) |>
  ggplot(aes(scientific_name, ratio)) +
  geom_point(
    aes(size = total_duration),
    colour = "grey70",
    alpha = 0.6,
    position = position_jitter(width = 0.15, height = 0)
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    size = 3,
    colour = "black"
  ) +
  geom_hline(yintercept = 7, linetype = "dashed") +
  coord_flip() +
  scale_y_log10() +
  labs(
    x = "Species",
    y = "Stopover : flight duration",
    size = "Migration duration (days)",
    title = "Ratio per species",
    subtitle = "Grey points are tags; black points are species medians"
  ) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


p_bout
p_tag
p_species
