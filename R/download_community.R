#' Download every GeoLocator datapackage within a Zenodo community
#'
#' Queries Zenodo with `httr2`, retrieves all records belonging to a community,
#' and downloads each one with [download_record()].
#'
#' @param community Zenodo community identifier (without the `communities:`
#'   prefix).
#' @param token Optional Zenodo personal access token.
#' @param dp_dir Directory where datapackages are stored locally.
#' @param overwrite Should existing records be overwritten?
#' @param quiet When `FALSE`, progress updates are emitted with `cli`.
#' @param include_embargo When `FALSE`, embargoed records are skipped.
#' @param include_measurements When `TRUE`, keep `measurements*` resources.
#'
#' @return Invisibly returns the list of Zenodo records returned by the API.
#' @seealso [download_record()]
#' @export
download_community <- function(
  community = "geolocator-dp",
  token = NULL,
  dp_dir = file.path("data", "dp"),
  overwrite = FALSE,
  quiet = FALSE,
  include_embargo = TRUE,
  include_measurements = TRUE
) {
  token <- GeoLocatoR:::resolve_zenodo_token(token = token)
  dir.create(dp_dir, recursive = TRUE, showWarnings = FALSE)

  records <- NULL
  if (!quiet) {
    cli::cli_progress_step(
      msg = "Discovering records in community {.val {community}}...",
      msg_done = "Discovered {length(records)} records in {.val {community}}.",
      msg_failed = "Record discovery failed for {.val {community}}."
    )
  }

  records <- .list_community_records(
    community = community,
    token = token
  )

  if (length(records) == 0) {
    cli::cli_warn(
      c(
        "!" = "No records were found in community {.val {community}}.",
        "i" = "Nothing was downloaded."
      )
    )
    return(invisible(list()))
  }

  for (i in seq_along(records)) {
    record <- records[[i]]
    record_label <- .record_label_from_zenodo(record)

    if (!quiet) {
      cli::cli_progress_step(
        msg = "Processing record {.field {record_label}} ({i}/{length(records)})...",
        msg_done = "Processed record {.field {record_label}}.",
        msg_failed = "Failed to process record {.field {record_label}}."
      )
    }

    download_record(
      rec = record,
      dp_dir = dp_dir,
      overwrite = overwrite,
      quiet = quiet,
      token = token,
      include_embargo = include_embargo,
      include_measurements = include_measurements
    )
  }

  invisible(records)
}

#' @noRd
.list_community_records <- function(community, token = NULL) {
  url <- "https://zenodo.org/api/records"
  query <- list(q = glue::glue("communities:{community}"), size = 100L)
  records <- list()

  repeat {
    payload <- .zenodo_get(url = url, token = token, query = query)
    query <- list()

    hits <- payload$hits$hits %||% list()
    if (length(hits) == 0) {
      break
    }
    records <- c(records, hits)

    url <- payload$links$`next` %||% NULL
    if (is.null(url) || !nzchar(url)) {
      break
    }
  }

  records
}

#' @noRd
.zenodo_get <- function(url, token = NULL, query = list()) {
  req <- httr2::request(url) |>
    httr2::req_user_agent(
      glue::glue(
        "GeoLocatorAggregator/{utils::packageVersion('GeoLocatorAggregator')}"
      )
    ) |>
    httr2::req_headers(Accept = "application/vnd.inveniordm.v1+json") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 2)

  if (length(query) > 0) {
    req <- do.call(httr2::req_url_query, c(list(.req = req), query))
  }

  if (!is.null(token) && nzchar(as.character(token)[1])) {
    req <- httr2::req_auth_bearer_token(req, token = token)
  }

  req |>
    httr2::req_error(
      body = function(resp) {
        payload <- tryCatch(
          httr2::resp_body_json(resp, simplifyVector = FALSE),
          error = function(...) NULL
        )
        msg <- payload$message %||% httr2::resp_status_desc(resp)
        glue::glue(
          "Zenodo API request to {url} failed (HTTP {httr2::resp_status(resp)}): {msg}"
        )
      }
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
}
