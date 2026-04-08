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
  resources <- frictionless::resources(pkg)

  for (resource_name in resources) {
    resource_data <- frictionless::read_resource(pkg, resource_name = resource_name)
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

