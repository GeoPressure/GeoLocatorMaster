# Run from the GeoLocatorMaster root with the adjacent GeoLocatoR checkout.
# GeoLocatoR 1.2.1 restores the source catalogue as a tibble when reading the draft.
library(GeoLocatoR)
library(dplyr)
library(ggplot2)

# Load the Master draft and inspect its source catalogue --------------------
# Draft access uses ZENODO_TOKEN from the environment or system keyring.
master <- read_zenodo("18187093", draft = TRUE)
# master <- read_gldp("data/master", drop_measurements = TRUE) # Test the local build offline.
# The default force_read = TRUE keeps derived tables available for analysis.
projects <- master$datapackages
print(
  projects |>
    select(datapackage_id, title, access_status, starts_with("numberTags_"))
)

# Select a species using tag metadata; only actual Master rows are included --
selected_tags <- tags(master) |> filter(scientific_name == "Upupa epops")
selected <- select_gldp(master, selected_tags$tag_id)
print(selected_tags |> count(datapackage_id, scientific_name, name = "tags"))

# Estimate total distance for each individual and each simulated trajectory --
# Sum within a simulation first: each simulation is a complete alternative path.
distances <- edges(selected) |>
  filter(type == "simulation") |>
  group_by(tag_id, j) |>
  summarise(total_distance_km = sum(distance), .groups = "drop")
distance_summary <- distances |>
  group_by(tag_id) |>
  summarise(
    median_km = median(total_distance_km),
    lower_km = quantile(total_distance_km, 0.025),
    upper_km = quantile(total_distance_km, 0.975),
    .groups = "drop"
  ) |>
  left_join(
    selected_tags |> select(tag_id, datapackage_id, scientific_name),
    by = "tag_id"
  )
print(distance_summary)
print(distance_summary |> count(datapackage_id, name = "tags_with_distances"))
print(
  ggplot(distance_summary, aes(x = reorder(tag_id, median_km), y = median_km)) +
    geom_linerange(aes(ymin = lower_km, ymax = upper_km)) +
    geom_point() +
    coord_flip() +
    labs(
      x = "Tag",
      y = "Total distance (km; median and 95% simulation interval)"
    ) +
    theme_minimal()
)

# Explore what is actually available for each species -----------------------
# Catalogue sensor counts describe source datasets, not necessarily Master rows.
availability <- tags(master) |>
  mutate(has_paths = tag_id %in% paths(master)$tag_id,
         has_edges = tag_id %in% edges(master)$tag_id,
         has_altitude = tag_id %in% pressurepaths(master)$tag_id) |>
  group_by(scientific_name) |>
  summarise(tags = n(), paths = sum(has_paths), edges = sum(has_edges),
            altitude = sum(has_altitude), .groups = "drop")
print(availability)

# Filter by project, sex, year or a geographic region ------------------------
# Select IDs first, then select_gldp() keeps the matching rows in every table.
project_tags <- tags(master) |> filter(datapackage_id == selected_tags$datapackage_id[1])
female_tags <- observations(master) |> filter(sex == "F", !is.na(tag_id)) |> distinct(tag_id)
period_tags <- staps(master) |> filter(start < as.POSIXct("2018-01-01", tz = "UTC"),
  end >= as.POSIXct("2017-01-01", tz = "UTC")) |> distinct(tag_id)
region_tags <- paths(master) |> filter(type == "most_likely", between(lat, 0, 60), between(lon, -20, 40)) |> distinct(tag_id)
print(c(project = nrow(project_tags), female = nrow(female_tags),
        period = nrow(period_tags), region = nrow(region_tags)))
# regional <- select_gldp(master, region_tags$tag_id)
# Region membership here means at least one estimated stop, not the whole track.

# Join positions to stationary periods without multiplying simulations -------
stopovers <- paths(selected) |>
  filter(type == "most_likely") |>
  left_join(staps(selected), by = c("tag_id", "stap_id"), relationship = "many-to-one") |>
  mutate(duration_days = as.numeric(difftime(end, start, units = "days")))
print(stopovers |> group_by(tag_id) |>
  summarise(stops = n(), located_stops = sum(!is.na(lat) & !is.na(lon)),
            longest_stop_days = max(duration_days), .groups = "drop"))

# Compare distance-derived speed, ground speed and air speed -----------------
# n > 1 connections span omitted stops and can include long pauses; use n == 1
# for individual-flight calculations. Review skipped connections separately.
# Edges use km and km/h. ERA5 *_of_wind_10m columns in pressurepaths use m/s;
# they also describe near-surface wind, not necessarily wind at flight altitude.
flights <- edges(selected) |>
  filter(type == "most_likely", n == 1) |>
  mutate(duration_h = flight_duration_h,
         distance_speed_kmh = distance / duration_h,
         ground_speed_kmh = sqrt(gs_u^2 + gs_v^2),
         wind_speed_kmh = sqrt(ws_u^2 + ws_v^2),
         air_speed_kmh = sqrt((gs_u - ws_u)^2 + (gs_v - ws_v)^2))
print(flights |> select(tag_id, stap_s, stap_t, n, duration_h, distance,
                       distance_speed_kmh, ground_speed_kmh, air_speed_kmh))
# Review extreme values against the source; do not silently truncate or drop them.
print(flights |> filter(distance_speed_kmh >= 150 | duration_h >= 50))
# Missing wind leaves air speed missing. Vector means and distance/time need not match.

# Inspect altitude, discarded samples and sampling intervals -----------------
pressure <- pressurepaths(selected) |> filter(type == "most_likely")
print(pressure |> group_by(tag_id) |>
  summarise(samples = n(), missing_altitude = sum(is.na(altitude)),
            discarded = sum(label == "discard", na.rm = TRUE), .groups = "drop"))
# Altitude is in metres above sea level; it is not height above the ground.
altitude <- pressure |>
  filter(is.na(label) | label != "discard", !is.na(altitude)) |>
  mutate(in_flight = stap_id != floor(stap_id))
altitude_summary <- altitude |> group_by(tag_id, in_flight) |>
  summarise(samples = n(), median_m = median(altitude), p95_m = quantile(altitude, 0.95),
            max_m = max(altitude), .groups = "drop")
print(altitude_summary)
print(altitude |> filter(altitude <= -200 | altitude >= 7000) |>
  select(tag_id, datetime, stap_id, altitude, label))
# Decimal stap_id identifies migration segments; never round it to join to staps.
sampling <- pressure |> arrange(tag_id, type, j, datetime) |> group_by(tag_id, type, j) |>
  mutate(interval_min = as.numeric(difftime(datetime, lag(datetime), units = "mins"))) |>
  ungroup() |> count(tag_id, interval_min, sort = TRUE)
print(sampling)
# These are sample-weighted altitude summaries. Different sampling intervals and
# missingness prevent interpreting row proportions as time budgets without weighting.
print(ggplot(altitude, aes(datetime, altitude, colour = in_flight)) +
  geom_point(size = 0.3) + facet_wrap(~tag_id, scales = "free_x") +
  labs(x = "Date (UTC)", y = "Altitude above sea level (m)", colour = "In flight") +
  theme_minimal())

# Keep the release reference and identify the source datasets to cite --------
citations <- projects |>
  semi_join(distance_summary, by = "datapackage_id") |>
  select(datapackage_id, title, licenses, bibliographicCitation)
print(paste("Master:", master$id, "version:", master$version))
print(citations, width = Inf)

# For raw measurements, retrieve the selected source packages from Zenodo.
