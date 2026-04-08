#!/usr/bin/env Rscript

library(dplyr)
library(ggplot2)
library(lubridate)
library(GeoLocatoR)
library(GeoPressureR)
library(readr)
library(MASS)

edges <- read_csv("data/master/edges.csv") |>
  filter(type == "most_likely")

edges$duration <- stap2duration(edges, units = "hours")
edges$gs <- sqrt(edges$gs_u^2 + edges$gs_v^2)
edges$distance_trim <- (pmin(edges$duration, 24) * edges$gs) / 24
distance_trim <- edges$distance_trim
distance_trim <- distance_trim[is.finite(distance_trim) & distance_trim > 0]
distance_trim <- pmax(distance_trim, .Machine$double.eps)
gamma_start_shape <- mean(distance_trim)^2 / stats::var(distance_trim)
gamma_start_rate <- mean(distance_trim) / stats::var(distance_trim)
gamma_fit <- MASS::fitdistr(
  distance_trim,
  densfun = "gamma",
  start = list(shape = gamma_start_shape, rate = gamma_start_rate)
)
gamma_shape <- unname(gamma_fit$estimate["shape"])
gamma_rate <- unname(gamma_fit$estimate["rate"])
gamma_scale <- 1 / gamma_rate

edges |>
  mutate(duration = pmin(duration, 24)) |>
  ggplot(aes(x = duration)) +
  geom_histogram(bins = 30, color = "white", fill = "steelblue") +
  labs(
    title = "Flight Duration Histogram",
    x = "Flight duration (hours)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)

edges |>
  mutate(gs = pmin(gs, 150)) |>
  ggplot(aes(x = gs)) +
  geom_histogram(bins = 30, color = "white", fill = "darkorange") +
  labs(
    title = "Flight Speed Histogram",
    x = "Flight speed (km/h)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)


edges |>
  ggplot(aes(x = distance_trim)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    color = "white",
    fill = "darkorange"
  ) +
  stat_function(
    fun = dgamma,
    args = list(shape = gamma_shape, rate = gamma_rate),
    color = "black",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    hjust = 1.05,
    vjust = 1.2,
    label = sprintf(
      "Gamma fit\nshape=%.2f\nscale=%.2f",
      gamma_shape,
      gamma_scale
    )
  ) +
  labs(
    x = "Flight distance per day (km/day)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)
