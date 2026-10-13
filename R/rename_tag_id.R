#' Rename a tag identifier across all resources
#'
#' Renames one `tag_id` value to another in every resource that contains a
#' `tag_id` column.
#'
#' @param pkg A GeoLocatoR datapackage object.
#' @param old_tag_id Existing `tag_id` to replace.
#' @param new_tag_id New `tag_id` value.
#'
#' @return Updated datapackage object.
#' @export
rename_tag_id <- function(pkg, old_tag_id, new_tag_id) {
  supported_resources <- c(
    "tags", "observations", "measurements", "staps",
    "twilights", "paths", "edges", "pressurepaths"
  )
  resources <- intersect(frictionless::resources(pkg), supported_resources)

  for (resource_name in resources) {
    resource_data <- getExportedValue("GeoLocatoR", resource_name)(pkg)
    if (!"tag_id" %in% names(resource_data)) {
      next
    }

    if (!any(resource_data$tag_id == old_tag_id, na.rm = TRUE)) {
      next
    }

    resource_data$tag_id[resource_data$tag_id == old_tag_id] <- new_tag_id

    pkg <- GeoLocatoR::add_gldp_resource(
      pkg = pkg,
      resource_name = resource_name,
      data = resource_data,
      cast_type = TRUE,
      replace = TRUE
    )
  }

  pkg
}
