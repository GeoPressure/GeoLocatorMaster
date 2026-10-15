# Small valid resources keep generation regressions independent of source downloads.
make_master_source <- function(directory, id, tag_id = "AA") {
  pkg <- GeoLocatoR::create_gldp()
  pkg$id <- id
  pkg$access_status <- "open"
  pkg <- GeoLocatoR::add_gldp_resource(pkg, "tags", tibble::tibble(tag_id = tag_id,
    ring_number = paste0("ring-", id), scientific_name = "", manufacturer = "Lund", model = "test", datapackage_id = id))
  pkg <- GeoLocatoR::add_gldp_resource(pkg, "staps", tibble::tibble(tag_id = tag_id,
    stap_id = 1, start = as.POSIXct("2020-01-01", tz = "UTC"), end = as.POSIXct("2020-01-02", tz = "UTC")))
  pkg <- GeoLocatoR::add_gldp_resource(pkg, "paths", tibble::tibble(tag_id = tag_id,
    stap_id = 1, type = "most_likely", j = 1L, lat = 47, lon = 8))
  pkg <- GeoLocatoR::add_gldp_resource(pkg, "pressurepaths", tibble::tibble(tag_id = tag_id,
    stap_id = 1, type = "most_likely", j = 1L, datetime = as.POSIXct("2020-01-01", tz = "UTC"),
    lat = 47, lon = 8, altitude = 1250, temperature_2m = 275.5))
  GeoLocatoR::write_gldp(pkg, directory)
  pkg
}
