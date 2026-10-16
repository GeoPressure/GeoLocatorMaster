#' Summarise source packages and their measurement sampling intervals
#'
#' @param pkgs Named source packages from read_dp_packages().
#' @param dp_dir Local source package cache.
#' @return Source metadata with measurement sizes, counts, sensor intervals and
#'   trajectory coverage: the percentage of trajectories whose most likely
#'   flights include wind (`trajectories_with_wind_pct`) and the median number of
#'   simulated trajectories per tracked tag (`simulations_per_trajectory_median`).
#' @importFrom dplyr .data
#' @export
datapackage_overview <- function(pkgs, dp_dir = file.path("data", "dp")) {
  metadata <- GeoLocatoR:::merge_gldp_datapackages(pkgs)
  summaries <- list()
  for (record_label in names(pkgs)) {
    pkg <- pkgs[[record_label]]
    intervals <- list()
    rows <- bytes <- 0
    for (resource in pkg$source_measurements) {
      path <- file.path(dp_dir, record_label, resource$path)
      if (!file.exists(path)) next
      summary <- summarise_measurement_file(path)
      intervals[[length(intervals) + 1]] <- summary$intervals
      rows <- rows + summary$rows
      bytes <- bytes + file.info(path)$size
    }
    # Trajectory coverage: tracked tags whose most likely flights have wind, and
    # simulations per tracked tag (missing for packages without local paths).
    coverage <- tibble::tibble(trajectories_with_wind_pct = NA_real_, simulations_per_trajectory_median = NA_real_)
    if (all(c("paths", "edges") %in% frictionless::resource_names(pkg))) {
      paths <- GeoLocatoR::paths(pkg)
      edges <- GeoLocatoR::edges(pkg)
      tracked <- unique(paths$tag_id[paths$type %in% c("most_likely", "geopressureviz")])
      with_wind <- unique(edges$tag_id[edges$type == "most_likely" & !is.na(edges$ws_u %||% NA)])
      n_sim <- vapply(tracked, function(id) dplyr::n_distinct(paths$j[paths$type == "simulation" & paths$tag_id == id]), numeric(1))
      if (length(tracked)) coverage <- tibble::tibble(
        trajectories_with_wind_pct = round(100 * mean(tracked %in% with_wind), 1),
        simulations_per_trajectory_median = stats::median(n_sim))
    }
    interval_table <- dplyr::bind_rows(intervals)
    if (nrow(interval_table)) {
      interval_table <- interval_table |>
        dplyr::group_by(.data$sensor, .data$interval_minutes) |>
        dplyr::summarise(n_intervals = sum(.data$n_intervals), .groups = "drop") |>
        dplyr::group_by(.data$sensor) |>
        dplyr::arrange(dplyr::desc(.data$n_intervals), .data$interval_minutes, .by_group = TRUE) |>
        dplyr::mutate(cumulative = cumsum(.data$n_intervals)) |>
        dplyr::filter(.data$cumulative - .data$n_intervals < 0.95 * sum(.data$n_intervals)) |>
        dplyr::summarise(interval_minutes = paste(.data$interval_minutes, collapse = "; "), .groups = "drop") |>
        dplyr::mutate(sensor = paste0("samplingInterval_", .data$sensor, "_min")) |>
        tidyr::pivot_wider(names_from = .data$sensor, values_from = .data$interval_minutes)
    }
    summaries[[record_label]] <- dplyr::bind_cols(
      tibble::tibble(datapackage_id = pkg$id, measurement_rows_cached = rows, measurement_bytes_cached = bytes),
      coverage,
      interval_table)
  }
  dplyr::left_join(metadata, dplyr::bind_rows(summaries), by = "datapackage_id")
}

#' @noRd
summarise_measurement_file <- function(path, chunk_size = 250000) {
  fingerprint <- file.info(path)[, c("size", "mtime")]
  cache <- paste0(path, ".summary.rds")
  if (file.exists(cache)) {
    summary <- readRDS(cache)
    if (identical(summary$fingerprint, fingerprint)) return(summary)
  }
  cli::cli_inform("Summarising measurements: {basename(dirname(path))}/{basename(path)}")
  counts <- tibble::tibble(sensor = character(), interval_minutes = numeric(), n_intervals = numeric())
  previous <- tibble::tibble(tag_id = character(), sensor = character(), previous_datetime = as.POSIXct(character()))
  rows <- 0
  callback <- readr::SideEffectChunkCallback$new(function(data, position) {
    rows <<- rows + nrow(data)
    data <- data |>
      dplyr::arrange(.data$tag_id, .data$sensor, .data$datetime) |>
      dplyr::left_join(previous, by = c("tag_id", "sensor")) |>
      dplyr::group_by(.data$tag_id, .data$sensor) |>
      dplyr::mutate(interval_minutes = round(as.numeric(difftime(
        .data$datetime, dplyr::lag(.data$datetime, default = dplyr::first(.data$previous_datetime)), units = "mins")), 6)) |>
      dplyr::ungroup()
    # Chunk boundaries must preserve chronological order within a tag/sensor.
    if (any(data$interval_minutes < 0, na.rm = TRUE)) {
      cli::cli_abort("Measurements are not chronological within tag/sensor in {path}; sort the source before computing intervals.")
    }
    previous <<- dplyr::bind_rows(
      data |> dplyr::group_by(.data$tag_id, .data$sensor) |>
        dplyr::summarise(previous_datetime = dplyr::last(.data$datetime), .groups = "drop"), previous) |>
      dplyr::distinct(.data$tag_id, .data$sensor, .keep_all = TRUE)
    counts <<- dplyr::bind_rows(counts,
      data |> dplyr::filter(!is.na(.data$interval_minutes), .data$interval_minutes > 0) |>
        dplyr::count(.data$sensor, .data$interval_minutes, name = "n_intervals")) |>
      dplyr::group_by(.data$sensor, .data$interval_minutes) |>
      dplyr::summarise(n_intervals = sum(.data$n_intervals), .groups = "drop")
  })
  readr::read_csv_chunked(path, callback, chunk_size = chunk_size,
    col_types = readr::cols_only(tag_id = "c", sensor = "c", datetime = "T"), progress = FALSE)
  summary <- list(fingerprint = fingerprint, rows = rows, intervals = counts)
  saveRDS(summary, cache)
  summary
}
