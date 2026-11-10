#' Update the Master Zenodo source relationships
#'
#' @param pkgs A Master package containing a datapackages catalogue.
#' @param dry_run If TRUE, return proposed relationships without updating Zenodo.
#' @return A list containing related, invisibly.
#' @details Replaces the Master draft's related identifiers with IsCompiledBy
#'   relationships to the version DOIs of the catalogue's open sources. To
#'   preserve other relationships, use [update_master_draft()].
#' @export
update_related_identifiers <- function(
  pkgs,
  dry_run = TRUE
) {
  # Embargoed sources are catalogued but their data are not in the release.
  open_ids <- pkgs$datapackages$datapackage_id[pkgs$datapackages$access_status %in% "open"]
  related <- lapply(sub("^https://doi.org/", "", open_ids), function(doi_value) {
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
