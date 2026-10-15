#' @noRd
.add_master_quality <- function(pkg) {
  # Sum actual flight gaps, excluding pauses at unlocated intermediate stops.
  if ("edges" %in% frictionless::resource_names(pkg)) {
    periods <- GeoLocatoR::staps(pkg) |>
      dplyr::arrange(.data$tag_id, .data$stap_id) |>
      dplyr::group_by(.data$tag_id) |>
      dplyr::mutate(flight_h = as.numeric(difftime(dplyr::lead(.data$start), .data$end, units = "hours")),
        cumulative_h = cumsum(dplyr::coalesce(dplyr::lag(.data$flight_h), 0))) |>
      dplyr::ungroup()
    i <- match("edges", vapply(pkg$resources, `[[`, character(1), "name"))
    data <- GeoLocatoR::edges(pkg) |>
      dplyr::left_join(dplyr::select(periods, tag_id, stap_s = stap_id, start_h = cumulative_h),
        by = c("tag_id", "stap_s"), relationship = "many-to-one") |>
      dplyr::left_join(dplyr::select(periods, tag_id, stap_t = stap_id, end_h = cumulative_h),
        by = c("tag_id", "stap_t"), relationship = "many-to-one") |>
      dplyr::mutate(flight_duration_h = .data$end_h - .data$start_h) |>
      dplyr::select(-start_h, -end_h)
    pkg$resources[[i]]$data <- data
    pkg$resources[[i]]$path <- NULL
    fields <- pkg$resources[[i]]$schema$fields
    pkg$resources[[i]]$schema$fields <- c(Filter(function(f) f$name != "flight_duration_h", fields), list(list(
      name = "flight_duration_h", type = "number", unit = "h",
      description = "Sum of gaps between consecutive stationary periods from stap_s to stap_t; excludes intermediate stationary time. Derived from staps, not end minus start for multi-flight connections.")))
  }

  issues <- list()
  for (resource in pkg$resources) {
    if (resource$name == "quality") next
    data <- resource$data
    rules <- list()
    for (field in resource$schema$fields) {
      if (isTRUE(field$constraints$required)) {
        rules[[paste0("missing:", field$name)]] <- is.na(data[[field$name]])
      }
    }
    if (resource$name == "edges") {
      rules$ground_speed_kmh <- sqrt(data$gs_u^2 + data$gs_v^2) >= 150
      rules$distance_speed_kmh <- data$distance / data$flight_duration_h >= 150
      rules$wind_speed_kmh <- sqrt(data$ws_u^2 + data$ws_v^2) >= 100
      rules$distance_km <- data$distance >= 3000
      # Five-minute zero-displacement transitions are not implausible flights.
      rules$flight_duration_h <- data$n == 1 & data$distance > 0 &
        (data$flight_duration_h < 5 / 60 | data$flight_duration_h >= 50)
    }
    if (resource$name == "pressurepaths") {
      rules$altitude_m <- data$altitude <= -200 | data$altitude >= 7000
      if ("label" %in% names(data)) rules$altitude_m <- rules$altitude_m & !data$label %in% "discard"
    }
    for (rule in names(rules)) {
      rows <- which(rules[[rule]])
      if (!length(rows)) next
      context <- data[rows, intersect(c("tag_id", "type", "j", "datetime", "stap_id", "stap_s", "stap_t", "n", "observation_type"), names(data)), drop = FALSE]
      issues[[length(issues) + 1L]] <- dplyr::bind_cols(
        tibble::tibble(resource = resource$name, row = rows, field = rule,
          flag = if (startsWith(rule, "missing:")) "incomplete_metadata" else "scientific_review"), context)
    }
  }
  quality <- dplyr::bind_rows(tibble::tibble(resource = character(), row = integer(), field = character(), flag = character(), tag_id = character()),
    dplyr::bind_rows(issues)) |>
    dplyr::left_join(dplyr::select(GeoLocatoR::tags(pkg), tag_id, datapackage_id), by = "tag_id", relationship = "many-to-one")
  # A separate resource preserves canonical source tables and exposes every flag.
  pkg$resources <- Filter(function(r) r$name != "quality", pkg$resources)
  quality_schema <- list(fields = lapply(names(quality), function(name) {
    value <- quality[[name]]
    list(name = name, type = if (inherits(value, "POSIXct")) "datetime" else if (is.integer(value)) "integer" else if (is.numeric(value)) "number" else "string")
  }), primaryKey = c("resource", "row", "field"), fieldsMatch = "exact")
  # frictionless::add_resource() rejects empty tables, which are valid here.
  pkg$resources <- append(pkg$resources, list(list(name = "quality", data = quality,
    schema = quality_schema, profile = "tabular-data-resource", type = "table",
    `$schema` = "https://datapackage.org/profiles/2.0/dataresource.json")))
  pkg$qualityControl <- list(
    policy = "Unknown metadata and scientific review screens are warnings. Identifiers, joins and mathematical validity remain release failures. Original values are preserved.",
    thresholds = list(ground_speed_kmh = 150, wind_speed_kmh = 100, distance_km = 3000,
      altitude_min_m = -200, altitude_max_m = 7000, single_flight_min_minutes = 5, single_flight_max_hours = 50),
    row_reference = "quality.row is the one-based data-row index in the named resource, excluding the CSV header; trajectory keys are also provided.",
    incomplete_metadata_flags = sum(quality$flag == "incomplete_metadata"),
    scientific_review_flags = sum(quality$flag == "scientific_review"))
  pkg
}
