#' Merge datapackages without retaining measurements
#'
#' Preserves metadata derived from measurements while omitting all
#' `measurements*` resources from the merged datapackage. Packages are prepared
#' with [prepare_dp_packages()] and the merged result is enhanced with
#' [enhanced_pkgs()]. The summaries are also stored in
#' `measurementMetadata`, where they survive a later [GeoLocatoR::read_gldp()]
#' call that clears standard measurement-derived fields when no measurement
#' resource is present.
#'
#' @param pkgs A list of GeoLocatoR datapackage objects, usually read with
#'   `read_dp_packages(force_read = FALSE)`.
#' @param quiet When `FALSE`, emits preparation progress messages.
#'
#' @return A merged and enhanced GeoLocatoR datapackage without measurement
#'   resources.
#' @export
merge_without_measurements <- function(pkgs, quiet = FALSE) {
  pkgs <- Filter(Negate(is.null), pkgs)
  if (length(pkgs) == 0) {
    return(NULL)
  }

  datapackages <- GeoLocatoR:::merge_gldp_datapackages(pkgs)
  measurement_metadata <- .measurement_metadata(pkgs)

  pkgs <- lapply(pkgs, .remove_measurement_descriptors)
  pkgs <- prepare_dp_packages(pkgs, quiet = quiet)
  merged <- GeoLocatoR::merge_gldp(pkgs)
  merged <- enhanced_pkgs(merged)

  merged$datapackages <- datapackages
  .restore_measurement_metadata(merged, measurement_metadata)
}

#' @noRd
.remove_measurement_descriptors <- function(pkg) {
  pkg$resources <- Filter(
    function(resource) {
      name <- as.character(resource$name %||% "")[1]
      !grepl("^measurements", name)
    },
    pkg$resources %||% list()
  )
  pkg
}

#' @noRd
.measurement_metadata <- function(pkgs) {
  count_names <- c(
    "measurements", "light", "pressure", "activity",
    "temperature_external", "temperature_internal", "magnetic",
    "wet_count", "conductivity"
  )

  counts <- lapply(count_names, function(name) {
    values <- vapply(pkgs, function(pkg) {
      value <- pkg$numberTags[[name]] %||% NA_integer_
      as.integer(value)[1]
    }, integer(1))
    if (all(is.na(values))) NA_integer_ else sum(values, na.rm = TRUE)
  })
  names(counts) <- count_names

  starts <- vapply(pkgs, function(pkg) {
    as.character(pkg$temporal$start %||% NA_character_)[1]
  }, character(1))
  ends <- vapply(pkgs, function(pkg) {
    as.character(pkg$temporal$end %||% NA_character_)[1]
  }, character(1))
  starts <- starts[!is.na(starts) & nzchar(starts)]
  ends <- ends[!is.na(ends) & nzchar(ends)]

  taxonomic <- unique(unlist(lapply(pkgs, function(pkg) {
    as.character(pkg$taxonomic %||% character())
  }), use.names = FALSE))
  taxonomic <- taxonomic[!is.na(taxonomic) & nzchar(taxonomic)]

  list(
    temporal = if (length(starts) > 0 && length(ends) > 0) {
      list(start = min(starts), end = max(ends))
    } else {
      NULL
    },
    taxonomic = if (length(taxonomic) > 0) taxonomic else NULL,
    numberTags = counts
  )
}

#' @noRd
.restore_measurement_metadata <- function(pkg, metadata) {
  pkg$temporal <- metadata$temporal
  pkg$taxonomic <- metadata$taxonomic

  counts <- metadata$numberTags
  counts <- counts[!vapply(counts, function(value) is.na(value), logical(1))]
  pkg$numberTags <- utils::modifyList(pkg$numberTags %||% list(), counts)
  pkg$measurementMetadata <- metadata
  pkg
}
