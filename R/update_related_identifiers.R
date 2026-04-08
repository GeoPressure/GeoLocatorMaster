update_related_identifiers <- function(
  pkgs,
  dry_run = TRUE
) {
  related <- lapply(pkgs$datapackages$conceptid, function(doi_value) {
    list(
      identifier = doi_value,
      scheme = "doi",
      relation_type = list(id = "iscompiledby"),
      resource_type = list(id = "dataset")
    )
  })

  if (length(related) == 0) {
    message("No DOI values found in pkgs$datapackages.")
    return(invisible(list(related = related)))
  }

  if (dry_run) {
    message(sprintf(
      "Dry run: would set %d related identifiers on Zenodo.",
      length(related)
    ))
    return(invisible(list(related = related)))
  }

  zenodo <- ZenodoManager$new(
    token = keyring::key_get(service = "ZENODO_TOKEN")
  )
  z <- zenodo$getDepositionByConceptDOI(.master_concept_doi)
  edited <- try(zenodo$editRecord(z$id), silent = TRUE)
  if (!inherits(edited, "try-error") && !is.null(edited$id)) {
    z <- edited
  }

  z$metadata$related_identifiers <- related
  zenodo$depositRecord(z, reserveDOI = FALSE, publish = FALSE)

  invisible(list(related = related))
}
