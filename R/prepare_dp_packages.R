#' Combine a list of GeoLocator datapackages
#'
#' @param pkgs A list of GeoLocatoR datapackage objects, usually from
#'   [read_dp_packages()].
#' @param quiet When `FALSE`, emits merge progress messages.
#' @param keep_measurements When `FALSE`, resources named `measurements*` are
#'   removed from each datapackage before merging.
#'
#' @return A merged GeoLocatoR datapackage object, or `NULL` when `pkgs` is
#'   empty.
#' @export
prepare_dp_packages <- function(
  pkgs,
  quiet = FALSE,
  keep_measurements = TRUE
) {
  pkgs <- Filter(Negate(is.null), pkgs)

  if (length(pkgs) == 0) {
    return(NULL)
  }

  prepared_pkgs <- vector("list", length(pkgs))
  seen_tag_ids <- character(0)
  supported_resources <- c(
    "tags", "observations", "measurements", "staps",
    "twilights", "paths", "edges", "pressurepaths"
  )

  for (i in seq_along(pkgs)) {
    pkg <- pkgs[[i]]
    record_label <- names(pkgs)[[i]]
    if (is.null(record_label) || identical(record_label, "")) {
      record_label <- as.character(i)
    }

    if (!keep_measurements) {
      pkg <- .drop_measurements_for_merge(pkg)
    }

    pkg$resources <- Filter(
      function(resource) (resource$name %||% "") %in% supported_resources,
      pkg$resources %||% list()
    )

    current_tag_ids <- unique(GeoLocatoR::tags(pkg)$tag_id)
    current_tag_ids <- current_tag_ids[
      !is.na(current_tag_ids) & nzchar(current_tag_ids)
    ]
    duplicated_tag_ids <- intersect(current_tag_ids, seen_tag_ids)

    if (length(duplicated_tag_ids) > 0) {
      cli::cli_warn(c(
        "Duplicate {.field tag_id} detected in package {.field {record_label}}.",
        ">" = "Renaming tag_id with suffix {.field _{i}}: {duplicated_tag_ids}"
      ))
      pkg <- rename_conflicting_tag_ids(
        pkg = pkg,
        tag_ids = duplicated_tag_ids,
        suffix = i
      )
      current_tag_ids <- unique(GeoLocatoR::tags(pkg)$tag_id)
      current_tag_ids <- current_tag_ids[
        !is.na(current_tag_ids) & nzchar(current_tag_ids)
      ]
    }

    seen_tag_ids <- unique(c(seen_tag_ids, current_tag_ids))
    prepared_pkgs[[i]] <- pkg
  }

  prepared_pkgs
}

#' @noRd
.drop_measurements_for_merge <- function(pkg) {
  resources <- pkg$resources %||% list()
  if (length(resources) == 0) {
    return(pkg)
  }

  keep <- vapply(
    resources,
    function(resource) {
      name <- as.character(resource$name %||% "")[1]
      !grepl("^measurements", name)
    },
    logical(1)
  )

  pkg$resources <- resources[keep]
  GeoLocatoR::update_gldp(pkg)
}

is_pkg_embargoed <- function(pkg) {
  if (pkg$access_status == "open") {
    return(FALSE)
  } else if (pkg$access_status == "embargoed") {
    return(TRUE)
  } else {
    print(pkg$access_status)
  }
  TRUE
}

#' @noRd
rename_conflicting_tag_ids <- function(pkg, tag_ids, suffix) {
  if (length(tag_ids) == 0) {
    return(pkg)
  }

  for (tag_id in tag_ids) {
    pkg <- rename_tag_id(
      pkg = pkg,
      old_tag_id = tag_id,
      new_tag_id = paste0(tag_id, "_", suffix)
    )
  }

  pkg
}
