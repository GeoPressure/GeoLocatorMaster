#' @noRd
.update_master_record_metadata <- function(metadata) {
  token <- keyring::key_get(service = "ZENODO_TOKEN")
  concept_id <- sub("^.*zenodo\\.", "", .master_concept_doi)
  record <- httr2::request("https://zenodo.org/api/user/records") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_headers(Accept = "application/vnd.inveniordm.v1+json") |>
    httr2::req_url_query(
      q = glue::glue("conceptrecid:{concept_id}"),
      size = 1,
      allversions = 1
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)

  record_id <- record$hits$hits[[1]]$id
  draft_url <- glue::glue("https://zenodo.org/api/records/{record_id}/draft")
  draft_response <- httr2::request(draft_url) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_headers(Accept = "application/vnd.inveniordm.v1+json") |>
    httr2::req_error(is_error = function(response) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(draft_response) == 404) {
    draft_response <- httr2::request(draft_url) |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_headers(Accept = "application/vnd.inveniordm.v1+json") |>
      httr2::req_method("POST") |>
      httr2::req_perform()
  }

  draft <- httr2::resp_body_json(draft_response, simplifyVector = FALSE)
  draft$metadata <- utils::modifyList(draft$metadata, metadata)

  httr2::request(draft_url) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_headers(Accept = "application/vnd.inveniordm.v1+json") |>
    httr2::req_method("PUT") |>
    httr2::req_body_json(list(metadata = draft$metadata)) |>
    httr2::req_perform()

  invisible(draft$metadata)
}
