# Focused checks for streaming, identifier collisions and sampling intervals.
if (dir.exists("../GeoLocatoR")) pkgload::load_all("../GeoLocatoR", quiet = TRUE)
library(GeoLocatoR)
library(dplyr)
library(readr)
if (file.exists("DESCRIPTION")) pkgload::load_all(quiet = TRUE) else library(GeoLocatorMaster)
fixture_dir <- tempfile("master-pipeline-")
dir.create(fixture_dir)

make_source <- function(label, doi) {
  directory <- file.path(fixture_dir, "dp", label)
  dir.create(directory, recursive = TRUE)
  data <- tibble(tag_id = "AA", sensor = "pressure",
    datetime = c("2020-01-01T00:00:00Z", "2020-01-01T00:01:00Z", "2020-01-01T00:03:00Z"),
    value = c(900, 901, 902), label = NA_character_)
  write_csv(data, file.path(directory, "measurements.csv"), na = "")
  pkg <- create_gldp()
  pkg <- add_gldp_resource(pkg, "tags", tibble(tag_id = "AA", ring_number = "R1",
    scientific_name = "", datapackage_id = doi), cast_type = TRUE)
  pkg$id <- doi
  pkg$access_status <- "open"
  pkg$numberTags <- list(tags = 1L, measurements = 1L, pressure = 1L)
  pkg$temporal <- list(start = "2020-01-01", end = "2020-01-01")
  pkg$source_measurements <- list(list(name = "measurements", path = "measurements.csv"))
  pkg
}
sources <- list(`100_101` = make_source("100_101", "https://doi.org/10.5281/zenodo.101"),
                `200_201` = make_source("200_201", "https://doi.org/10.5281/zenodo.201"))
merged <- suppressWarnings(merge_without_measurements(sources))
stopifnot(setequal(tags(merged)$tag_id, c("AA", "AA_200")),
          !"measurements" %in% frictionless::resource_names(merged),
          merged$measurementMetadata$numberTags$pressure == 2)

output <- file.path(fixture_dir, "master")
write_gldp(merged, output)
streamed <- GeoLocatorMaster:::write_master_measurements(merged, sources, output, file.path(fixture_dir, "dp"))
measurements <- read_csv(file.path(output, "measurements.csv"), show_col_types = FALSE)
stopifnot(nrow(measurements) == 6, setequal(measurements$tag_id, tags(merged)$tag_id),
          sum(measurements$value) == 5406,
          "measurements" %in% frictionless::resource_names(streamed))
reopened <- read_gldp(output)
stopifnot(nrow(GeoLocatoR::measurements(reopened)) == 6,
          reopened$numberTags$pressure == 2)

# The second interval spans a chunk boundary; neither interval may be lost.
summary <- GeoLocatorMaster:::summarise_measurement_file(file.path(fixture_dir, "dp/100_101/measurements.csv"), chunk_size = 2)
stopifnot(summary$rows == 3, setequal(summary$intervals$interval_minutes, c(1, 2)),
          sum(summary$intervals$n_intervals) == 2,
          identical(summary, GeoLocatorMaster:::summarise_measurement_file(file.path(fixture_dir, "dp/100_101/measurements.csv"))))
unlink(fixture_dir, recursive = TRUE)
cat("Streaming, collision and interval checks passed.\n")
