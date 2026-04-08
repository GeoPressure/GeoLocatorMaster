library(dplyr)
library(readr)
library(GeoLocatoR)
library(GeoPressureR)

# --- load aggregated datapackage ---
if (!exists("pkgs")) {
  pkgs <- GeoLocatoR::read_gldp("data/master_restricted/datapackage.json")
}


# --- tag metadata for record context ---
tag_info <- tags(pkgs) %>%
  dplyr::select(
    tag_id,
    datapackage_id,
    ring_number,
    scientific_name,
    manufacturer,
    model
  )

target_datapackage_id <- "https://doi.org/10.5281/zenodo.17631330"
selected_tag_ids <- tags_df %>%
  dplyr::filter(datapackage_id != target_datapackage_id) %>%
  dplyr::distinct(tag_id)

# --- edge metrics for flight-distance/speed records ---
edge_metrics <- GeoLocatoR::edges(pkgs) %>%
  # dplyr::semi_join(selected_tag_ids, by = "tag_id") %>%
  dplyr::filter(type == "most_likely") %>%
  dplyr::filter(n == 1) %>%
  dplyr::mutate(
    duration_h = GeoPressureR::stap2duration(., units = "hours"),
    avg_speed_kmh = distance / pmax(duration_h, 1e-9),
    windspeed = sqrt(ws_u^2 + ws_v^2),
    wind_support = (gs_u * ws_u + gs_v * ws_v) / pmax(windspeed, 1e-6)
  ) %>%
  dplyr::filter(
    is.finite(distance),
    is.finite(duration_h),
    is.finite(avg_speed_kmh),
    distance > 0,
    duration_h > 0
  )

edge_metrics %>%
  dplyr::slice_max(distance, n = 3, with_ties = FALSE) %>%
  left_join(tags(pkgs), by = "tag_id") |>
  dplyr::select(
    tag_id,
    scientific_name,
    datapackage_id,
    distance,
    duration_h,
    avg_speed_kmh
  )

fastest_flight <- edge_metrics %>%
  dplyr::slice_max(avg_speed_kmh, n = 1, with_ties = FALSE) %>%
  dplyr::transmute(
    record = "fastest_flight_average_speed",
    value = avg_speed_kmh,
    unit = "km/h",
    tag_id,
    datetime = start,
    stap_id = NA_real_,
    stap_s,
    stap_t,
    distance_km = distance,
    duration_h,
    avg_speed_kmh,
    altitude_m = NA_real_,
    wind_support_km_h = wind_support,
    climb_rate_m_h = NA_real_,
    delta_alt_m = NA_real_,
    delta_t_h = NA_real_,
    notes = "Most-likely edge with maximum average speed"
  )

# --- pressurepath metrics for altitude/climb records ---
stap_windows <- staps(pkgs) %>%
  dplyr::select(tag_id, stap_id, start, end)

pp <- pressurepaths(pkgs) %>%
  dplyr::filter(type == "most_likely") %>%
  dplyr::select(tag_id, stap_id, datetime, altitude) %>%
  dplyr::filter(is.finite(altitude)) %>%
  dplyr::left_join(stap_windows, by = c("tag_id", "stap_id")) %>%
  dplyr::mutate(
    in_stopover = !is.na(start) &
      !is.na(end) &
      datetime >= start &
      datetime <= end
  ) %>%
  dplyr::select(tag_id, stap_id, datetime, altitude, in_stopover)

if (nrow(pp) == 0L) {
  stop("No valid pressurepaths altitude records found in pkgs.")
}

flight_alt_pool <- pp %>%
  dplyr::filter(!in_stopover)
if (nrow(flight_alt_pool) == 0L) {
  flight_alt_pool <- pp
}

highest_flight_altitude <- flight_alt_pool %>%
  dplyr::slice_max(altitude, n = 1, with_ties = FALSE) %>%
  dplyr::transmute(
    record = "highest_flight_altitude",
    value = altitude,
    unit = "m",
    tag_id,
    datetime,
    stap_id,
    stap_s = NA_real_,
    stap_t = NA_real_,
    distance_km = NA_real_,
    duration_h = NA_real_,
    avg_speed_kmh = NA_real_,
    altitude_m = altitude,
    wind_support_km_h = NA_real_,
    climb_rate_m_h = NA_real_,
    delta_alt_m = NA_real_,
    delta_t_h = NA_real_,
    notes = "Maximum altitude outside stopover windows (fallback: all pressurepaths)"
  )

stopover_alt_pool <- pp %>%
  dplyr::filter(in_stopover)
if (nrow(stopover_alt_pool) == 0L) {
  stopover_alt_pool <- pp
}

highest_stopover_altitude <- stopover_alt_pool %>%
  dplyr::slice_max(altitude, n = 1, with_ties = FALSE) %>%
  dplyr::transmute(
    record = "highest_stopover_altitude",
    value = altitude,
    unit = "m",
    tag_id,
    datetime,
    stap_id,
    stap_s = NA_real_,
    stap_t = NA_real_,
    distance_km = NA_real_,
    duration_h = NA_real_,
    avg_speed_kmh = NA_real_,
    altitude_m = altitude,
    wind_support_km_h = NA_real_,
    climb_rate_m_h = NA_real_,
    delta_alt_m = NA_real_,
    delta_t_h = NA_real_,
    notes = "Maximum altitude during stopover windows (fallback: all pressurepaths)"
  )

pp_climb <- pp %>%
  dplyr::arrange(tag_id, datetime) %>%
  dplyr::group_by(tag_id) %>%
  dplyr::mutate(
    prev_datetime = dplyr::lag(datetime),
    prev_altitude = dplyr::lag(altitude),
    prev_in_stopover = dplyr::lag(in_stopover),
    delta_alt_m = altitude - prev_altitude,
    delta_t_h = as.numeric(difftime(datetime, prev_datetime, units = "hours")),
    climb_rate_m_h = delta_alt_m / delta_t_h
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    is.finite(climb_rate_m_h),
    is.finite(delta_alt_m),
    is.finite(delta_t_h),
    delta_t_h > 0,
    delta_t_h <= 3,
    delta_alt_m > 0
  )

climb_pool <- pp_climb %>%
  dplyr::filter(!in_stopover, !prev_in_stopover)
if (nrow(climb_pool) == 0L) {
  climb_pool <- pp_climb
}

fastest_climbing_rate <- climb_pool %>%
  dplyr::slice_max(climb_rate_m_h, n = 1, with_ties = FALSE) %>%
  dplyr::transmute(
    record = "fastest_climbing_rate",
    value = climb_rate_m_h,
    unit = "m/h",
    tag_id,
    datetime,
    stap_id,
    stap_s = NA_real_,
    stap_t = NA_real_,
    distance_km = NA_real_,
    duration_h = NA_real_,
    avg_speed_kmh = NA_real_,
    altitude_m = altitude,
    wind_support_km_h = NA_real_,
    climb_rate_m_h,
    delta_alt_m,
    delta_t_h,
    notes = "Maximum positive altitude change per hour (delta_t <= 3h)"
  )

# --- final records table ---
key_records <- dplyr::bind_rows(
  longest_flight,
  fastest_flight,
  highest_flight_altitude,
  highest_stopover_altitude,
  fastest_climbing_rate
) %>%
  dplyr::left_join(tag_info, by = "tag_id") %>%
  dplyr::relocate(
    record,
    value,
    unit,
    tag_id,
    scientific_name,
    ring_number,
    datapackage_id,
    manufacturer,
    model
  )

output_path <- "figures/key_records.csv"
readr::write_csv(key_records, output_path)

message("Saved key records to: ", output_path)
print(key_records, n = Inf)
