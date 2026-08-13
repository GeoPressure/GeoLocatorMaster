#' Read local GeoLocator datapackages into a list
#'
#' @inheritParams aggregate_dp
#' @param force_read Passed to [GeoLocatoR::read_gldp()]. When `FALSE`, resource
#'   data are not retained in the returned package objects.
#'
#' @return A named list of GeoLocatoR datapackage objects.
#' @export
read_dp_packages <- function(
  dp_dir = file.path("data", "dp"),
  include_embargo = FALSE,
  quiet = FALSE,
  force_read = TRUE
) {
  dp_names <- list.files(
    dp_dir,
    pattern = "^[0-9]+_[0-9]+$",
    full.names = FALSE
  )

  pkgs <- list()

  for (i in seq_along(dp_names)) {
    record_label <- dp_names[[i]]
    dp_json <- file.path(dp_dir, record_label, "datapackage.json")

    if (!quiet) {
      cli::cli_progress_step(
        msg = "Reading datapackage {.field {record_label}} ({i}/{length(dp_names)})...",
        msg_done = "Read datapackage {.field {record_label}}.",
        msg_failed = "Failed to read datapackage {.field {record_label}}."
      )
    }

    pkg <- GeoLocatoR::read_gldp(dp_json, force_read = force_read)
    if (!include_embargo && is_pkg_embargoed(pkg)) {
      cli::cli_warn(
        "Datapackage {.field {record_label}} is under embargo. Skipping..."
      )
      next
    }

    pkgs[[record_label]] <- pkg
  }

  pkgs
}
