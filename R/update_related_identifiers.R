#' Update the Master Zenodo source relationships
#'
#' @param pkgs A Master package containing a datapackages catalogue.
#' @param dry_run If TRUE, return proposed relationships without updating Zenodo.
#' @return A list containing related, invisibly.
#' @details Replaces the Master draft's related identifiers with compiles
#'   relationships to the catalogue's source concept DOIs. For release-specific
#'   source DOIs and preservation of other relationships, use [update_master_draft()].
#' @export
update_related_identifiers <- function(
  pkgs,
  dry_run = TRUE
) {
  related <- lapply(pkgs$datapackages$conceptid, function(doi_value) {
    list(
      identifier = doi_value,
      scheme = "doi",
      relation_type = list(id = "compiles"),
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
