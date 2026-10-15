test_that("users can select tags by species, project, sex, time, region and available data", {
  skip_without_release()
  observations <- GeoLocatoR::observations(master_release)
  selections <- list(
    species = master_tags$tag_id[master_tags$scientific_name == "Upupa epops"],
    project = master_tags$tag_id[master_tags$datapackage_id == master_tags$datapackage_id[1]],
    sex = observations$tag_id[which(observations$sex == "F")],
    year = master_staps$tag_id[format(master_staps$start, "%Y") == "2017"],
    region = master_paths$tag_id[which(dplyr::between(master_paths$lat, 0, 60) & dplyr::between(master_paths$lon, -20, 40))],
    with_edges = unique(master_edges$tag_id)[1],
    with_altitude = unique(master_pressure$tag_id)[1],
    metadata_only = setdiff(master_tags$tag_id, unique(master_paths$tag_id))[1]
  )
  for (name in names(selections)) {
    ids <- unique(stats::na.omit(selections[[name]]))
    expect_true(length(ids) > 0L, info = paste("No tags exercise", name))
    if (!length(ids)) next
    selected <- GeoLocatoR::select_gldp(master_release, ids)
    expect_setequal(GeoLocatoR::tags(selected)$tag_id, ids)
    expect_equal(selected$numberTags$tags, length(ids), info = name)
    for (i in seq_along(master_release$resources)) {
      original <- master_release$resources[[i]]$data
      if (!"tag_id" %in% names(original)) next
      expect_equal(selected$resources[[i]]$data, original[original$tag_id %in% ids, , drop = FALSE],
        info = paste(name, master_release$resources[[i]]$name))
    }
    # The catalogue is release-wide; select source citations through selected tags.
    citations <- dplyr::semi_join(selected$datapackages, GeoLocatoR::tags(selected), by = "datapackage_id")
    expect_setequal(citations$datapackage_id, GeoLocatoR::tags(selected)$datapackage_id)
  }
})

test_that("single, repeated and excluded tag selections preserve the expected rows", {
  skip_without_release()
  ids <- unique(master_edges$tag_id)[1:2]
  selected <- GeoLocatoR::select_gldp(master_release, c(ids, ids[1]))
  expect_equal(nrow(GeoLocatoR::tags(selected)), 2L)
  single <- GeoLocatoR::select_gldp(selected, ids[1])
  excluded <- GeoLocatoR::select_gldp(selected, -ids[2])
  expect_equal(GeoLocatoR::tags(single), GeoLocatoR::tags(excluded))
  expect_equal(GeoLocatoR::edges(single), GeoLocatoR::edges(excluded))
  expect_error(GeoLocatoR::select_gldp(selected, "__unknown_tag__"), "not found")
})

test_that("users can sum complete alternative trajectories without mixing simulations", {
  skip_without_release()
  distances <- master_edges |> dplyr::filter(type == "simulation") |>
    dplyr::group_by(tag_id, j) |>
    dplyr::summarise(total_km = sum(distance), segments = dplyr::n(), .groups = "drop")
  expect_gt(nrow(distances), 0L)
  expect_true(all(is.finite(distances$total_km) & distances$total_km >= 0))
  coverage <- distances |> dplyr::group_by(tag_id) |>
    dplyr::summarise(min_segments = min(segments), max_segments = max(segments), .groups = "drop")
  expect_no_rows(dplyr::filter(coverage, min_segments != max_segments), "Incomplete edge simulations")
  summary <- distances |> dplyr::group_by(tag_id) |>
    dplyr::summarise(lower = stats::quantile(total_km, 0.025), median = stats::median(total_km),
      upper = stats::quantile(total_km, 0.975), .groups = "drop") |>
    dplyr::left_join(master_tags, by = "tag_id", relationship = "many-to-one")
  expect_equal(nrow(summary), dplyr::n_distinct(distances$tag_id))
  expect_true(all(summary$lower <= summary$median & summary$median <= summary$upper))
  expect_false(anyNA(summary$datapackage_id))
})

test_that("flight speed and wind calculations have explicit compatible units", {
  skip_without_release()
  resource <- master_descriptor$resources[[match("edges", vapply(master_descriptor$resources, `[[`, character(1), "name"))]]
  fields <- stats::setNames(resource$schema$fields, vapply(resource$schema$fields, `[[`, character(1), "name"))
  expect_identical(fields$distance$unit, "km")
  for (name in c("gs_u", "gs_v", "ws_u", "ws_v")) expect_identical(fields[[name]]$unit, "km/h")
  flights <- master_edges |> dplyr::mutate(
    duration_h = as.numeric(difftime(end, start, units = "hours")),
    distance_speed_kmh = distance / flight_duration_h,
    ground_speed_kmh = sqrt(gs_u^2 + gs_v^2),
    air_speed_kmh = sqrt((gs_u - ws_u)^2 + (gs_v - ws_v)^2))
  expect_true(all(is.finite(flights$distance_speed_kmh)))
  expect_true(all(flights$flight_duration_h > 0 & flights$flight_duration_h <= flights$duration_h + 1e-8))
  expect_true(all(is.finite(flights$ground_speed_kmh)))
  expect_true(all(is.na(flights$air_speed_kmh) | is.finite(flights$air_speed_kmh)))
  # Distances and vector means use different geometry; equality is not assumed.
  expect_identical(is.na(flights$air_speed_kmh), is.na(flights$ws_u) | is.na(flights$ws_v))
})

test_that("altitude summaries retain missingness, discard labels and sampling coverage", {
  skip_without_release()
  usable <- master_pressure |> dplyr::filter(is.na(label) | label != "discard") |>
    dplyr::filter(type == "most_likely", !is.na(altitude))
  expect_gt(nrow(usable), 0L)
  summary <- usable |> dplyr::group_by(tag_id) |>
    dplyr::summarise(samples = dplyr::n(), median_m = stats::median(altitude),
      p95_m = stats::quantile(altitude, 0.95), max_m = max(altitude), .groups = "drop")
  expect_true(all(summary$median_m <= summary$p95_m & summary$p95_m <= summary$max_m))
  expect_true(all(summary$tag_id %in% master_tags$tag_id))
  # Decimal stap_id denotes a migration segment; an integer denotes a stop.
  expect_true(any(usable$stap_id != floor(usable$stap_id), na.rm = TRUE))
  # Do not round flight fractions and join them to stationary-period coordinates.
  joined <- master_paths |> dplyr::left_join(master_staps, by = c("tag_id", "stap_id"), relationship = "many-to-one")
  expect_equal(nrow(joined), nrow(master_paths))
  expect_false(anyNA(joined[c("start", "end")]))
})

test_that("writing and reopening a selected bird preserves its derived tables", {
  skip_without_release()
  selected <- GeoLocatoR::select_gldp(master_release, master_edges$tag_id[1])
  directory <- tempfile("master-reuse-")
  on.exit(unlink(directory, recursive = TRUE))
  GeoLocatoR::write_gldp(selected, directory)
  reopened <- GeoLocatoR::read_gldp(directory)
  for (name in c("tags", "staps", "paths", "edges", "pressurepaths")) {
    expect_equal(getExportedValue("GeoLocatoR", name)(reopened), getExportedValue("GeoLocatoR", name)(selected), info = name)
  }
  expect_equal(reopened$datapackages, selected$datapackages)
})

test_that("the Quarto reuse example runs against the exact release under test", {
  skip_without_release()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("knitr")
  code <- tempfile(fileext = ".R")
  on.exit(unlink(code))
  knitr::purl(testthat::test_path("..", "..", "examples", "reuse_master.qmd"), output = code, quiet = TRUE)
  script <- parse(code)
  example <- new.env(parent = environment())
  # Substitute only the read: execute every analytical expression unchanged.
  for (i in seq_along(script)) {
    if (is.call(script[[i]]) && identical(script[[i]][[1]], as.name("<-")) &&
        identical(script[[i]][[2]], as.name("master"))) script[[i]][[3]] <- quote(master_release)
  }
  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit({ grDevices::dev.off(); unlink(plot_file) })
  expect_no_error(utils::capture.output(for (expression in script) eval(expression, example)))
  expect_gt(nrow(example$distance_summary), 0L)
  expect_gt(nrow(example$altitude_summary), 0L)
  expect_true(all(example$flights$n == 1))
})
