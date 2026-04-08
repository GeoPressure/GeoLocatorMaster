library(dplyr)
library(ggplot2)
library(GeoLocatoR)
library(GeoPressureR)

# --- load aggregated datapackage ---
if (!exists("e")) {
  e <- readr::read_csv(
    "data/master_restricted/edges.csv",
    show_col_types = FALSE
  )
}
if (!exists("tags_df")) {
  tags_df <- readr::read_csv(
    "data/master_restricted/tags.csv",
    show_col_types = FALSE
  )
}

target_datapackage_id <- "https://doi.org/10.5281/zenodo.17631330"
selected_tag_ids <- tags_df %>%
  dplyr::filter(datapackage_id != target_datapackage_id) %>%
  dplyr::distinct(tag_id)

if (nrow(selected_tag_ids) == 0L) {
  stop("No tag_id found for target datapackage_id: ", target_datapackage_id)
}

# --- derive edge metrics across the full dataset ---
edge_metrics <- e %>%
  dplyr::semi_join(selected_tag_ids, by = "tag_id") %>%
  dplyr::filter(n == 1) |>
  dplyr::filter(type == "most_likely") %>%
  dplyr::mutate(
    duration_h = GeoPressureR::stap2duration(., units = "hours"),
    groundspeed = sqrt(gs_u^2 + gs_v^2),
    windspeed = sqrt(ws_u^2 + ws_v^2),
    # Same unit as gs_u/ws_u (km/h in this dataset)
    wind_support = (gs_u * ws_u + gs_v * ws_v) / pmax(windspeed, 1e-6)
  ) %>%
  dplyr::filter(
    is.finite(distance),
    is.finite(groundspeed),
    distance > 0,
    groundspeed > 0
  )

if (nrow(edge_metrics) == 0L) {
  stop("No valid edge records available for plotting.")
}

# Winsorize extreme wind-support values to make the color scale readable.
ws_limits <- c(-50, 100)
edge_metrics_with_wind <- edge_metrics %>%
  dplyr::filter(is.finite(wind_support)) %>%
  dplyr::mutate(
    wind_support_plot = pmin(pmax(wind_support, ws_limits[[1]]), ws_limits[[2]])
  )
edge_metrics_no_wind <- edge_metrics %>%
  dplyr::filter(!is.finite(wind_support))

# --- axis and guide-line helpers ---
x_max_h <- ceiling(max(edge_metrics$duration_h, na.rm = TRUE) / 6) * 6
y_max_km <- max(edge_metrics$distance, na.rm = TRUE)
speed_guides <- dplyr::tibble(speed_kmh = seq(40, 120, by = 20))
speed_labels <- speed_guides %>%
  dplyr::mutate(
    x = pmin(x_max_h * 0.97, (y_max_km * 0.92) / speed_kmh),
    y = speed_kmh * x,
    label = paste0(speed_kmh, " km/h")
  ) %>%
  dplyr::filter(y <= y_max_km * 0.98, x >= 0)

x_label_fun <- function(x) {
  ifelse(
    x == 0,
    "0",
    ifelse(x %% 24 == 0, paste0(x / 24, "d"), paste0(x, "h"))
  )
}

# --- forced-origin fit (passes through 0,0) ---
fit_origin <- stats::lm(distance ~ 0 + duration_h, data = edge_metrics)
fit_slope <- unname(stats::coef(fit_origin)[["duration_h"]])
x_avg_label <- min(x_max_h * 0.95, (y_max_km / fit_slope) * 0.95)
y_avg_label <- fit_slope * x_avg_label
fit_label <- paste0("Average speed: ", sprintf("%.1f", fit_slope), " km/h")

# --- plot ---
speed_distance_wind_plot <- ggplot(
  edge_metrics,
  aes(x = duration_h, y = distance)
) +
  geom_abline(
    data = speed_guides,
    aes(slope = speed_kmh, intercept = 0),
    inherit.aes = FALSE,
    linetype = "dotted",
    color = "#AAB6C5",
    linewidth = 0.5,
    alpha = 0.9
  ) +
  geom_text(
    data = speed_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    color = "#AAB6C5",
    size = 2.8,
    hjust = 1,
    vjust = -0.2
  ) +
  geom_point(
    data = edge_metrics_no_wind,
    aes(shape = "No wind data"),
    color = "#22C55E",
    alpha = 0.4,
    size = 1.5,
    stroke = 0,
    show.legend = TRUE
  ) +
  geom_point(
    data = edge_metrics_with_wind,
    aes(color = wind_support_plot),
    alpha = 0.4,
    size = 1.5,
    shape = 16,
    stroke = 0
  ) +
  geom_abline(
    slope = fit_slope,
    intercept = 0,
    color = "#ffffff",
    linetype = "longdash",
    linewidth = 1.25,
    alpha = 0.95
  ) +
  geom_label(
    data = dplyr::tibble(x = x_avg_label, y = y_avg_label, label = fit_label),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = -0.2,
    color = "#E8EDF5",
    fill = "#111827",
    size = 3.1
  ) +
  scale_color_gradient2(
    low = "#2563eb",
    mid = "#e2e8f0",
    high = "#dc2626",
    midpoint = 0,
    name = "Wind support\n(km/h)"
  ) +
  scale_shape_manual(
    values = c("No wind data" = 16),
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(0, x_max_h),
    breaks = seq(0, x_max_h, by = 6),
    labels = x_label_fun,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = scales::label_number(big.mark = ","),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    y = "Flight distance (km)",
    x = "Flight duration",
    color = "Wind support\n(km/h)"
  ) +
  guides(
    color = guide_colorbar(order = 1),
    shape = guide_legend(
      order = 2,
      override.aes = list(color = "#22C55E", size = 3, alpha = 1)
    )
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.background = element_rect(fill = "#0D111C", color = NA),
    panel.background = element_rect(fill = "#0D111C", color = NA),
    plot.margin = margin(14, 14, 14, 14),
    panel.grid.major = element_line(color = "#2C3445", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.title = element_text(color = "#E8EDF5"),
    axis.text = element_text(color = "#C7D1DD"),
    axis.line = element_line(color = "#2C3445"),
    legend.position = c(0.03, 0.97),
    legend.justification = c(0, 1),
    legend.background = element_rect(
      fill = scales::alpha("#0D111C", 0.82),
      color = "#2C3445"
    ),
    legend.key = element_rect(fill = "#0D111C", color = NA),
    legend.box = "vertical",
    legend.spacing.y = unit(0.15, "cm"),
    legend.text = element_text(color = "#C7D1DD", size = 8),
    legend.title = element_text(color = "#E8EDF5")
  )

print(speed_distance_wind_plot)

ggsave(
  filename = "figures/speed_distance_windsupport.png",
  plot = speed_distance_wind_plot,
  bg = "#0D111C",
  width = 9,
  height = 5,
  units = "in",
  dpi = 300
)

# Figure caption: Relationship between flight duration and distance for most-likely
# migration legs in the master geolocator-dp dataset. Colored points indicate wind
# support where available, green points indicate edges without wind support, dotted
# lines are constant-speed references (40-120 km/h), and the thicker dashed white
# line shows the forced-origin average flight speed fit.
