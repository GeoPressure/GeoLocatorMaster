test_that("streaming preserves identifiers, values and intervals across chunks", {
  expect_no_error(source(testthat::test_path("..", "test_pipeline.R"), local = TRUE, chdir = FALSE))
})

test_that("creator reconciliation preserves identities without Zenodo writes", {
  expect_no_error(source(testthat::test_path("..", "test_creators.R"), local = TRUE, chdir = FALSE))
})

test_that("colliding tags are renamed in every derived resource without changing their values", {
  directory <- tempfile("master-generation-")
  on.exit(unlink(directory, recursive = TRUE))
  sources <- list(`100_101` = make_master_source(file.path(directory, "100_101"), "source-101"),
                  `200_201` = make_master_source(file.path(directory, "200_201"), "source-201"))
  prepared <- suppressWarnings(prepare_dp_packages(sources, quiet = TRUE))
  expect_identical(GeoLocatoR::tags(sources[[2]])$tag_id, "AA")
  for (name in c("tags", "staps", "paths", "pressurepaths")) {
    expect_identical(getExportedValue("GeoLocatoR", name)(prepared[[2]])$tag_id, "AA_200")
  }
  merged <- suppressWarnings(merge_without_measurements(sources, quiet = TRUE))
  expect_setequal(GeoLocatoR::tags(merged)$tag_id, c("AA", "AA_200"))
  expect_equal(GeoLocatoR::pressurepaths(merged)$altitude, c(1250, 1250))
  expect_equal(GeoLocatoR::pressurepaths(merged)$temperature_2m, c(275.5, 275.5))
  expect_true(all(GeoLocatoR::tags(merged)$manufacturer == "Lund University"))
})

test_that("source numeric field types are restored before exporting the master", {
  directory <- tempfile("master-types-")
  on.exit(unlink(directory, recursive = TRUE))
  source <- make_master_source(file.path(directory, "100_101"), "source-101")
  resource <- match("pressurepaths", vapply(source$resources, `[[`, character(1), "name"))
  field <- match("temperature_2m", vapply(source$resources[[resource]]$schema$fields, `[[`, character(1), "name"))
  # Simulate the schema drift that prompted restore_master_field_types().
  descriptor <- jsonlite::read_json(file.path(directory, "100_101", "datapackage.json"))
  descriptor$resources[[resource]]$schema$fields[[field]]$type <- "number"
  jsonlite::write_json(descriptor, file.path(directory, "100_101", "datapackage.json"), auto_unbox = TRUE)
  source$resources[[resource]]$schema$fields[[field]]$type <- "string"
  source$resources[[resource]]$data$temperature_2m <- "275.5"
  restored <- GeoLocatorMaster:::restore_master_field_types(source, list(`100_101` = source), directory)
  expect_identical(restored$resources[[resource]]$schema$fields[[field]]$type, "number")
  expect_equal(GeoLocatoR::pressurepaths(restored)$temperature_2m, 275.5)
})

test_that("source citations are generated after attaching authoritative Zenodo metadata", {
  directory <- tempfile("master-citation-")
  on.exit(unlink(directory, recursive = TRUE))
  make_master_source(file.path(directory, "100_101"), "source-101")
  record <- list(id = "101", doi = "10.5281/zenodo.101", created = "2020-01-01T00:00:00Z",
    access = list(status = "open"), metadata = list(title = "A source dataset", version = "1.0",
      creators = list(list(person_or_org = list(name = "Researcher, Alex", given_name = "Alex", family_name = "Researcher")))))
  jsonlite::write_json(record, file.path(directory, "100_101", "zenodo_record.json"), auto_unbox = TRUE)
  source <- read_dp_packages(dp_dir = directory, quiet = TRUE, force_read = FALSE)[[1]]
  expect_match(source$bibliographicCitation, "A source dataset")
  expect_match(source$bibliographicCitation, "10.5281/zenodo.101", fixed = TRUE)
  expect_match(source$bibliographicCitation, "Researcher")
})

test_that("file-backed selection filters the same birds as loaded selection", {
  directory <- tempfile("master-lazy-")
  on.exit(unlink(directory, recursive = TRUE))
  sources <- list(`100_101` = make_master_source(file.path(directory, "100_101"), "source-101", "AA"),
                  `200_201` = make_master_source(file.path(directory, "200_201"), "source-201", "BB"))
  merged <- merge_without_measurements(sources, quiet = TRUE)
  GeoLocatoR::write_gldp(merged, file.path(directory, "master"))
  lazy <- GeoLocatoR::read_gldp(file.path(directory, "master"), force_read = FALSE)
  selected <- GeoLocatoR::select_gldp(lazy, "AA")
  expect_identical(GeoLocatoR::tags(selected)$tag_id, "AA")
  expect_identical(GeoLocatoR::paths(selected)$tag_id, "AA")
})

test_that("master flight duration excludes pauses at omitted intermediate stops", {
  pkg <- GeoLocatoR::create_gldp()
  pkg <- GeoLocatoR::add_gldp_resource(pkg, "tags", tibble::tibble(tag_id = "AA", datapackage_id = "source",
    ring_number = "R1", scientific_name = "Upupa epops", manufacturer = "test", model = "test"))
  origin <- as.POSIXct("2020-01-01", tz = "UTC")
  pkg <- GeoLocatoR::add_gldp_resource(pkg, "staps", tibble::tibble(tag_id = "AA", stap_id = 1:3,
    start = origin + c(0, 7200, 90000), end = origin + c(3600, 86400, 93600)))
  pkg <- GeoLocatoR::add_gldp_resource(pkg, "edges", tibble::tibble(tag_id = "AA", type = "most_likely", j = 1L,
    stap_s = 1, stap_t = 3, start = origin + 3600, end = origin + 90000, n = 2L,
    lat_s = 45, lon_s = 7, lat_t = 46, lon_t = 8, distance = 100, gs_u = 50, gs_v = 0))
  flagged <- GeoLocatorMaster:::.add_master_quality(pkg)
  expect_equal(GeoLocatoR::edges(flagged)$flight_duration_h, 2)
  expect_equal(as.numeric(difftime(GeoLocatoR::edges(flagged)$end, GeoLocatoR::edges(flagged)$start, units = "hours")), 24)
  expect_identical(GeoLocatoR::edges(flagged)$distance, GeoLocatoR::edges(pkg)$distance)
  directory <- tempfile("master-duration-")
  on.exit(unlink(directory, recursive = TRUE))
  GeoLocatoR::write_gldp(flagged, directory)
  reopened <- GeoLocatoR::read_gldp(directory)
  expect_equal(GeoLocatoR::edges(reopened)$flight_duration_h, 2)
})

test_that("scientific screens preserve flagged values and respect discard labels", {
  directory <- tempfile("master-quality-")
  on.exit(unlink(directory, recursive = TRUE))
  pkg <- make_master_source(directory, "source")
  i <- match("pressurepaths", vapply(pkg$resources, `[[`, character(1), "name"))
  pkg$resources[[i]]$data$altitude <- -500
  pkg$resources[[i]]$data$label <- NA_character_
  flagged <- GeoLocatorMaster:::.add_master_quality(pkg)
  quality <- frictionless::read_resource(flagged, "quality")
  expect_true(any(quality$field == "altitude_m"))
  expect_identical(GeoLocatoR::pressurepaths(flagged)$altitude, -500)
  pkg$resources[[i]]$data$label <- "discard"
  expect_false(any(frictionless::read_resource(GeoLocatorMaster:::.add_master_quality(pkg), "quality")$field == "altitude_m"))
})

test_that("five-minute zero-displacement transitions do not trigger a flight-duration warning", {
  directory <- tempfile("master-zero-flight-")
  on.exit(unlink(directory, recursive = TRUE))
  pkg <- make_master_source(directory, "source")
  origin <- as.POSIXct("2020-01-01", tz = "UTC")
  pkg <- GeoLocatoR::add_gldp_resource(pkg, "staps", tibble::tibble(tag_id = "AA", stap_id = 1:2,
    start = origin + c(0, 3900), end = origin + c(3600, 7200)), replace = TRUE)
  pkg <- GeoLocatoR::add_gldp_resource(pkg, "edges", tibble::tibble(tag_id = "AA", type = "most_likely", j = 1L,
    stap_s = 1, stap_t = 2, start = origin + 3600, end = origin + 3900, n = 1L,
    lat_s = 45, lon_s = 7, lat_t = 45, lon_t = 7, distance = 0, gs_u = 0, gs_v = 0))
  quality <- frictionless::read_resource(GeoLocatorMaster:::.add_master_quality(pkg), "quality")
  expect_false(any(quality$field == "flight_duration_h"))
})
