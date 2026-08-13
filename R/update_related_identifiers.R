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

  .update_master_record_metadata(list(related_identifiers = related))

  invisible(list(related = related))
}
