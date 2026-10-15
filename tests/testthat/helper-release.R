# Release tests are opt-in; the dedicated runner requires a real package.
master_dir <- Sys.getenv("GLDP_MASTER_DIR")
master_release <- NULL
if (nzchar(master_dir)) {
  master_descriptor <- jsonlite::read_json(file.path(master_dir, "datapackage.json"))
  master_release <- GeoLocatoR::read_gldp(master_dir, drop_measurements = TRUE)
  master_tags <- GeoLocatoR::tags(master_release)
  master_staps <- GeoLocatoR::staps(master_release)
  master_paths <- GeoLocatoR::paths(master_release)
  master_edges <- GeoLocatoR::edges(master_release)
  master_pressure <- GeoLocatoR::pressurepaths(master_release)
}

skip_without_release <- function() {
  testthat::skip_if(is.null(master_release), "Run Rscript tests/test_release.R to test a built Master")
}

# Include a few offending keys in failures without printing entire resources.
expect_no_rows <- function(data, reason) {
  testthat::expect_equal(nrow(data), 0L,
    info = paste(reason, paste(utils::capture.output(print(utils::head(data, 5))), collapse = "\n"), sep = "\n"))
}
