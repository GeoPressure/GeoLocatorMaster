#' Synchronise the existing Master Zenodo draft
#'
#' @param record_id Existing draft record ID.
#' @param pkg Public Master package.
#' @param sources Public source packages used in this release.
#' @param files Local public Master files to upload.
#' @param update_metadata Set FALSE to resume file uploads after metadata was updated.
#' @param audit_dir Optional directory for before/after draft snapshots; NULL writes none.
#' @return The refreshed draft record, invisibly. Does not publish the draft.
#' @details Files absent from the supplied list are removed from the draft.
#'   Supply the complete set of public release files, not a partial selection.
#' @export
update_master_draft <- function(record_id, pkg, sources, files, update_metadata = TRUE, audit_dir = NULL) {
  token <- GeoLocatoR:::resolve_zenodo_token()
  draft_url <- glue::glue("https://zenodo.org/api/records/{record_id}/draft")
  draft <- .zenodo_get(draft_url, token)
  if (!is.null(audit_dir)) {
    dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(draft, file.path(audit_dir, "zenodo_before.json"),
                         auto_unbox = TRUE, pretty = TRUE, null = "null")
  }
  draft$metadata$version <- pkg$version
  draft$metadata$creators <- order_creators(get_creators_from_pkgs(pkg))$creators
  # Registered licences accept only an ID, not the returned display fields.
  for (i in seq_along(draft$metadata$rights)) {
    if (!is.null(draft$metadata$rights[[i]]$id)) {
      draft$metadata$rights[[i]] <- list(id = draft$metadata$rights[[i]]$id)
    } else {
      draft$metadata$rights[[i]]$icon <- NULL
    }
  }
  # A compilation compiles its source datasets; the inverse belongs on sources.
  draft$metadata$related_identifiers <- unname(c(
    Filter(function(x) !x$relation_type$id %in% c("compiles", "iscompiledby"),
           draft$metadata$related_identifiers),
    lapply(sources, function(source) list(identifier = sub("^https://doi.org/", "", source$id),
      scheme = "doi", relation_type = list(id = "compiles"), resource_type = list(id = "dataset")))))
  if (update_metadata) httr2::request(draft_url) |>
    httr2::req_options(ipresolve = 1L) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_method("PUT") |>
    httr2::req_body_json(list(metadata = draft$metadata)) |>
    httr2::req_perform()

  # Reuse identical uploads; replace changed files and remove obsolete exports.
  entries <- .zenodo_get(paste0(draft_url, "/files"), token)$entries
  entries <- stats::setNames(entries, vapply(entries, function(file) file$key, character(1)))
  for (key in setdiff(names(entries), basename(files))) {
    httr2::request(glue::glue("{draft_url}/files/{utils::URLencode(key, reserved = TRUE)}")) |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_method("DELETE") |>
      httr2::req_perform()
  }
  for (path in files) {
    key <- basename(path)
    checksum <- paste0("md5:", unname(tools::md5sum(path)))
    existing <- entries[[key]]
    if (identical(existing$checksum, checksum) && identical(existing$status, "completed")) next
    file_url <- glue::glue("{draft_url}/files/{utils::URLencode(key, reserved = TRUE)}")
    if (!is.null(existing)) {
      httr2::request(file_url) |> httr2::req_auth_bearer_token(token) |>
        httr2::req_method("DELETE") |> httr2::req_perform()
    }
    httr2::request(paste0(draft_url, "/files")) |>
      httr2::req_auth_bearer_token(token) |> httr2::req_method("POST") |>
      httr2::req_body_json(list(list(key = key))) |> httr2::req_perform()
    cli::cli_inform("Uploading {key} ({round(file.info(path)$size / 1e6, 1)} MB)")
    httr2::request(paste0(file_url, "/content")) |>
      httr2::req_auth_bearer_token(token) |> httr2::req_method("PUT") |>
      httr2::req_options(ipresolve = 1L, http_version = 2L, connecttimeout = 20L) |>
      httr2::req_timeout(300) |>
      httr2::req_body_file(path, type = "application/octet-stream") |> httr2::req_perform()
    uploaded <- httr2::request(paste0(file_url, "/commit")) |>
      httr2::req_auth_bearer_token(token) |> httr2::req_method("POST") |>
      httr2::req_perform() |> httr2::resp_body_json()
    if (!identical(uploaded$checksum, checksum)) cli::cli_abort("Uploaded checksum differs for {key}.")
  }
  draft <- .zenodo_get(draft_url, token)
  if (!is.null(audit_dir)) jsonlite::write_json(draft, file.path(audit_dir, "zenodo_after.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null")
  cli::cli_inform("Zenodo draft {record_id} updated; version {pkg$version}. It remains unpublished.")
  invisible(draft)
}
