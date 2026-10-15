# Screening thresholds follow GeoLocatoR's existing coherence checks.
# Flags request scientific/source review without deleting rows.
test_that("incomplete metadata and scientific screens are preserved as explicit warnings", {
  skip_without_release()
  flights <- master_edges |> dplyr::mutate(
    duration_h = as.numeric(difftime(end, start, units = "hours")),
    distance_speed_kmh = distance / flight_duration_h,
    ground_speed_kmh = sqrt(gs_u^2 + gs_v^2),
    wind_speed_kmh = sqrt(ws_u^2 + ws_v^2),
    air_speed_kmh = sqrt((gs_u - ws_u)^2 + (gs_v - ws_v)^2)) |>
    dplyr::left_join(dplyr::select(master_tags, tag_id, datapackage_id, scientific_name), by = "tag_id")
  unusual_flights <- flights |> dplyr::filter((n == 1 & distance > 0 &
    (flight_duration_h < 5 / 60 | flight_duration_h >= 50)) |
    distance >= 3000 | distance_speed_kmh >= 150 | ground_speed_kmh >= 150 | wind_speed_kmh >= 100)
  unusual_altitude <- master_pressure |>
    dplyr::filter(is.na(label) | label != "discard") |>
    dplyr::filter(altitude <= -200 | altitude >= 7000) |>
    dplyr::left_join(dplyr::select(master_tags, tag_id, datapackage_id, scientific_name), by = "tag_id")
  coverage <- master_pressure |> dplyr::group_by(tag_id, type, j) |>
    dplyr::summarise(rows = dplyr::n(), discarded = sum(label == "discard", na.rm = TRUE),
      missing_altitude = sum(is.na(altitude)), missing_wind = sum(is.na(u_component_of_wind_10m) | is.na(v_component_of_wind_10m)),
      first = min(datetime), last = max(datetime), .groups = "drop") |>
    dplyr::left_join(dplyr::select(master_tags, tag_id, datapackage_id, scientific_name), by = "tag_id")
  directory <- Sys.getenv("GLDP_AUDIT_DIR")
  if (nzchar(directory)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    missing_fields <- list()
    for (resource in master_release$resources) {
      for (field in resource$schema$fields) {
        if (!isTRUE(field$constraints$required) || !anyNA(resource$data[[field$name]])) next
        missing <- resource$data[is.na(resource$data[[field$name]]), , drop = FALSE]
        missing_fields[[length(missing_fields) + 1L]] <- missing |>
          dplyr::count(tag_id, name = "missing_rows") |>
          dplyr::mutate(resource = resource$name, field = field$name) |>
          dplyr::left_join(dplyr::select(master_tags, tag_id, datapackage_id, scientific_name), by = "tag_id")
      }
    }
    readr::write_csv(dplyr::bind_rows(missing_fields), file.path(directory, "missing-required-fields.csv"), na = "")
    readr::write_csv(unusual_flights, file.path(directory, "unusual-flights.csv"), na = "")
    readr::write_csv(unusual_altitude, file.path(directory, "unusual-altitude.csv"), na = "")
    readr::write_csv(coverage, file.path(directory, "pressure-coverage.csv"), na = "")
    readr::write_csv(master_paths |> dplyr::filter(is.na(lat) | is.na(lon)) |>
      dplyr::left_join(master_tags, by = "tag_id"), file.path(directory, "missing-path-coordinates.csv"), na = "")
    jsonlite::write_json(list(id = master_descriptor$id, version = master_descriptor$version,
      directory = master_dir, tested_at = format(Sys.time(), tz = "UTC"),
      GeoLocatoR = as.character(utils::packageVersion("GeoLocatoR")),
      files = as.list(tools::md5sum(file.path(master_dir, c("datapackage.json", "tags.csv", "staps.csv", "paths.csv", "edges.csv", "pressurepaths.csv", "quality.csv"))))),
      file.path(directory, "tested-release.json"), auto_unbox = TRUE, pretty = TRUE)
  }
  quality <- frictionless::read_resource(master_release, "quality")
  expect_true(all(quality$resource %in% frictionless::resource_names(master_release)))
  for (name in unique(quality$resource)) {
    flags <- quality |> dplyr::filter(resource == name)
    data <- master_release$resources[[match(name, vapply(master_release$resources, `[[`, character(1), "name"))]]$data
    expect_equal(flags$tag_id, data$tag_id[flags$row])
  }
  altitude_flags <- quality |> dplyr::filter(resource == "pressurepaths", field == "altitude_m")
  expect_setequal(altitude_flags$row, which((is.na(master_pressure$label) | master_pressure$label != "discard") &
    (master_pressure$altitude <= -200 | master_pressure$altitude >= 7000)))
  speed_flags <- quality |> dplyr::filter(resource == "edges", field == "ground_speed_kmh")
  expect_setequal(speed_flags$row, which(sqrt(master_edges$gs_u^2 + master_edges$gs_v^2) >= 150))
  metadata_flags <- dplyr::filter(quality, flag == "incomplete_metadata")
  expected_metadata <- list()
  for (resource in master_release$resources) {
    for (field in resource$schema$fields) {
      if (!isTRUE(field$constraints$required)) next
      rows <- which(is.na(resource$data[[field$name]]))
      expected_metadata[[length(expected_metadata) + 1L]] <- tibble::tibble(resource = resource$name,
        row = rows, field = paste0("missing:", field$name))
    }
  }
  expect_equal(metadata_flags |> dplyr::select(resource, row, field) |> dplyr::arrange(resource, row, field),
    dplyr::bind_rows(expected_metadata) |> dplyr::arrange(resource, row, field))
  expect_equal(nrow(metadata_flags), master_descriptor$qualityControl$incomplete_metadata_flags)
  expect_equal(sum(quality$flag == "scientific_review"), master_descriptor$qualityControl$scientific_review_flags)
  if (nrow(metadata_flags)) cli::cli_warn("{nrow(metadata_flags)} missing required metadata values remain unknown in the original sources; see quality.csv and missing-required-fields.csv.")
  if (any(quality$flag == "scientific_review")) cli::cli_warn("Scientific review flags remain in quality.csv; estimates are preserved. See unusual-flights.csv and unusual-altitude.csv.")
})
