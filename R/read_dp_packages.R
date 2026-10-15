#' Read local GeoLocator datapackages into a list
#'
#' @param dp_dir Local source cache.
#' @param include_embargo Include authorised embargoed source data.
#' @param quiet Suppress progress messages.
#' @param force_read Passed to `GeoLocatoR::read_gldp()`. When `FALSE`, resource
#'   data are not retained in the returned package objects.
#' @param record_labels Current inventory folders to read; NULL reads all folders.
#' @details Raw measurements are always omitted from the in-memory objects;
#'   their descriptors are kept in source_measurements for optional chunked export.
#'
#' @return A named list of GeoLocatoR datapackage objects.
#' @export
read_dp_packages <- function(
  dp_dir = file.path("data", "dp"),
  include_embargo = FALSE,
  quiet = FALSE,
  force_read = TRUE,
  record_labels = NULL
) {
  dp_names <- list.files(
    dp_dir,
    pattern = "^[0-9]+_[0-9]+$",
    full.names = FALSE
  )
  if (!is.null(record_labels)) dp_names <- intersect(dp_names, record_labels)

  pkgs <- list()

  for (i in seq_along(dp_names)) {
    record_label <- dp_names[[i]]
    dp_json <- file.path(dp_dir, record_label, "datapackage.json")

    if (!quiet) {
      cli::cli_progress_step(
        msg = "Reading datapackage {.field {record_label}} ({i}/{length(dp_names)})...",
        msg_done = "Read datapackage {.field {record_label}}.",
        msg_failed = "Failed to read datapackage {.field {record_label}}."
      )
    }

    descriptor <- jsonlite::read_json(dp_json)
    # Skip raw measurements before update_gldp() can read them. Source summaries
    # describe the source package, including sensors not retained in the Master.
    pkg <- GeoLocatoR::read_gldp(dp_json, force_read = force_read, drop_measurements = TRUE)
    pkg <- restore_master_field_types(pkg, stats::setNames(list(pkg), record_label), dp_dir)
    record_path <- file.path(dp_dir, record_label, "zenodo_record.json")
    if (file.exists(record_path)) {
      pkg <- GeoLocatoR:::read_zenodo_attach_metadata(pkg, jsonlite::read_json(record_path))
      pkg <- GeoLocatoR::update_gldp_bibliographic_citation(pkg)
    }
    source_tags <- GeoLocatoR::tags(pkg)
    source_tags$datapackage_id <- pkg$id
    pkg <- GeoLocatoR::add_gldp_resource(pkg, "tags", source_tags, cast_type = TRUE, replace = TRUE)
    for (field in c("temporal", "taxonomic")) pkg[[field]] <- descriptor[[field]]
    pkg$numberTags <- utils::modifyList(pkg$numberTags %||% list(), descriptor$numberTags %||% list())
    pkg$numberTags$tags <- nrow(source_tags)
    pkg$source_measurements <- Filter(function(r) grepl("^measurements", r$name), descriptor$resources)
    if (!include_embargo && is_pkg_embargoed(pkg)) {
      cli::cli_warn(
        "Datapackage {.field {record_label}} is under embargo. Skipping..."
      )
      next
    }

    pkgs[[record_label]] <- pkg
  }

  pkgs
}
