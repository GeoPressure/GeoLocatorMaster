#' @noRd
.add_master_flight_duration <- function(pkg) {
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

  pkg
}

#' @noRd
.master_quality <- function(pkg) {
  pkg <- .add_master_flight_duration(pkg)
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
      rules$altitude_m <- data$altitude <= -200 | data$altitude >= 10000
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
  quality
}
