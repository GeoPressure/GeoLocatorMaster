# Check identifiers, attribution and descriptor/CSV consistency -------------
stopifnot(!anyDuplicated(GeoLocatoR::tags(master)$tag_id),
          !anyDuplicated(occurrences$occurrenceID),
          all(occurrences$datasetID == master_doi),
          all(occurrences$associatedReferences %in% vapply(public_sources, function(pkg) pkg$id, character(1))),
          all(is.na(occurrences$decimalLatitude) | between(occurrences$decimalLatitude, -90, 90)),
          all(is.na(occurrences$decimalLongitude) | between(occurrences$decimalLongitude, -180, 180)),
          all(!is.na(occurrences$license)),
          all(is.na(occurrences$minimumElevationInMeters) | is.finite(occurrences$minimumElevationInMeters)),
          all(is.na(occurrences$maximumElevationInMeters) | is.finite(occurrences$maximumElevationInMeters)))
for (directory in "data/master") {
  descriptor <- jsonlite::read_json(file.path(directory, "datapackage.json"), simplifyVector = TRUE)
  for (table in c("datapackages", "species")) {
    csv <- read_csv(file.path(directory, paste0(table, ".csv")),
                    col_types = cols(.default = "c"), show_col_types = FALSE)
    embedded <- as_tibble(descriptor[[table]])
    stopifnot(nrow(csv) == nrow(embedded), identical(names(csv), names(embedded)))
    # Compare written values, allowing only CSV type inference and empty cells.
    for (column in names(csv)) {
      a <- as.character(csv[[column]]); a[is.na(a)] <- ""
      b <- as.character(embedded[[column]]); b[is.na(b)] <- ""
      stopifnot(identical(a, b))
    }
  }
}

cli::cli_inform("Release checks passed.")
