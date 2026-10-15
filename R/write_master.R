#' Write a consistent Master descriptor and supplementary tables
#'
#' @param pkg Merged package without measurements.
#' @param overview Metadata for all source packages, including embargoed records.
#' @param all_sources Source packages used to construct the species catalogue.
#' @param directory Output directory.
#' @param version Dataset release version.
#' @param doi Release DOI, or NULL for a local restricted copy.
#' @param include_measurements Whether to stream raw measurements into the output.
#' @param sources Sources whose data are included in this output.
#' @param metadata Fixed descriptor fields, kept outside the generation code.
#' @return The written Master object. Measurements, if included, remain on disk.
#' @export
write_master <- function(pkg, overview, all_sources, directory, version, doi,
                         include_measurements = FALSE, sources,
                         metadata = jsonlite::read_json("docs/master-metadata.json")) {
  pkg <- restore_master_field_types(pkg, sources)
  pkg <- .add_master_quality(pkg)
  for (field in names(metadata)) pkg[[field]] <- metadata[[field]]
  pkg$id <- doi
  if (is.null(doi)) pkg$conceptid <- NULL
  pkg$version <- version
  pkg$access_status <- if (is.null(doi)) "restricted" else "open"
  pkg$datapackages <- overview
  # Metadata catalogues cover all projects; resource tables contain only sources.
  species_pkg <- GeoLocatoR::create_gldp()
  species_pkg <- GeoLocatoR::add_gldp_resource(species_pkg, "tags",
    dplyr::bind_rows(lapply(all_sources, GeoLocatoR::tags)), cast_type = TRUE)
  pkg$species <- .add_species(species_pkg)$species

  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  # Remove only previous standard exports, so optional resources cannot linger.
  old_descriptor <- file.path(directory, "datapackage.json")
  if (file.exists(old_descriptor)) {
    old <- jsonlite::read_json(old_descriptor)
    unlink(file.path(directory, vapply(old$resources, function(r) r$path, character(1))))
  }
  unlink(file.path(directory, "measurements.csv"))
  GeoLocatoR::write_gldp(pkg, directory)
  readr::write_csv(pkg$datapackages, file.path(directory, "datapackages.csv"), na = "")
  readr::write_csv(pkg$species, file.path(directory, "species.csv"), na = "")
  if (pkg$qualityControl$incomplete_metadata_flags + pkg$qualityControl$scientific_review_flags > 0) {
    cli::cli_warn("Master quality.csv contains {pkg$qualityControl$incomplete_metadata_flags} incomplete-metadata and {pkg$qualityControl$scientific_review_flags} scientific-review flags; values are retained for review.")
  }
  if (include_measurements) pkg <- write_master_measurements(pkg, sources, directory)
  cli::cli_inform("Wrote {directory}: {pkg$numberTags$tags} tags, release {version}.")
  pkg
}

#' @noRd
restore_master_field_types <- function(pkg, sources, dp_dir = file.path("data", "dp")) {
  # GeoLocatoR upgrades can infer string types for extra ERA5 columns. Preserve
  # the explicit numeric/date types declared by the original source schemas.
  descriptors <- lapply(names(sources), function(label)
    jsonlite::read_json(file.path(dp_dir, label, "datapackage.json")))
  for (i in seq_along(pkg$resources)) {
    resource <- pkg$resources[[i]]
    for (j in seq_along(resource$schema$fields)) {
      field <- resource$schema$fields[[j]]
      if (!identical(field$type, "string")) next
      types <- unique(unlist(lapply(descriptors, function(descriptor) {
        original <- Filter(function(r) identical(r$name, resource$name), descriptor$resources)
        if (!length(original)) return(NULL)
        original <- Filter(function(f) identical(f$name, field$name), original[[1]]$schema$fields)
        if (length(original)) original[[1]]$type else NULL
      })))
      if (length(types) && all(types %in% c("integer", "number"))) {
        resource$schema$fields[[j]]$type <- "number"
      } else if (length(types) == 1 && types %in% c("date", "datetime", "boolean")) {
        resource$schema$fields[[j]]$type <- types
      }
    }
    if (is.data.frame(resource$data)) resource$data <- GeoLocatoR:::cast_table(resource$data, resource$schema)
    pkg$resources[[i]] <- resource
  }
  pkg
}

#' @noRd
write_master_measurements <- function(pkg, sources, directory, dp_dir = file.path("data", "dp")) {
  # Stream CSV chunks instead of retaining the combined measurement table in R.
  template <- GeoLocatoR::add_gldp_resource(GeoLocatoR::create_gldp(), "measurements",
    tibble::tibble(tag_id = "template", sensor = "pressure", datetime = "2000-01-01T00:00:00Z", value = 0, label = NA_character_))
  resource <- template$resources[[1]]
  resource$data <- NULL
  resource$path <- "measurements.csv"
  columns <- vapply(resource$schema$fields, function(f) f$name, character(1))
  destination <- file.path(directory, "measurements.csv")
  readr::write_csv(as.data.frame(stats::setNames(rep(list(character()), length(columns)), columns)), destination, na = "")
  master_tags <- GeoLocatoR::tags(pkg)
  for (record_label in names(sources)) {
    source_pkg <- sources[[record_label]]
    original_ids <- GeoLocatoR::tags(source_pkg)$tag_id
    final_ids <- master_tags$tag_id[master_tags$datapackage_id == source_pkg$id]
    renamed_ids <- ifelse(original_ids %in% final_ids, original_ids,
                          paste0(original_ids, "_", sub("_.*$", "", record_label)))
    for (measurement in source_pkg$source_measurements) {
      path <- file.path(dp_dir, record_label, measurement$path)
      if (!file.exists(path)) cli::cli_abort("Missing measurements required for this build: {path}")
      callback <- readr::SideEffectChunkCallback$new(function(data, position) {
        data$tag_id <- renamed_ids[match(data$tag_id, original_ids)]
        for (column in setdiff(columns, names(data))) data[[column]] <- NA_character_
        readr::write_csv(data[, columns], destination, append = TRUE, na = "")
      })
      readr::read_csv_chunked(path, callback, chunk_size = 250000,
        col_types = readr::cols(.default = "c"), progress = FALSE)
    }
  }
  descriptor <- jsonlite::read_json(file.path(directory, "datapackage.json"))
  descriptor$resources <- append(descriptor$resources, list(resource), after = 2)
  jsonlite::write_json(descriptor, file.path(directory, "datapackage.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA)
  resource$path <- normalizePath(destination)
  pkg$resources <- append(pkg$resources, list(resource), after = 2)
  pkg
}
