#' Download a single GeoLocator datapackage record from Zenodo
#'
#' Downloads missing files directly to disk without materialising measurement
#' tables in R. Existing files are reused unless overwrite is requested. Source
#' metadata and access status are refreshed even when files are cached.
#' The record folder is named `<concept_id>_<record_id>`.
#'
#' @param rec Zenodo record id/DOI/URL or a Zenodo record object (list) returned
#'   by the API.
#' @param dp_dir Directory where datapackages should be stored locally.
#' @param overwrite Should an existing local record folder be overwritten?
#' @param quiet When `FALSE`, emits CLI progress messages.
#' @param token Optional Zenodo personal access token. If `NULL`, token
#'   resolution follows GeoLocatoR defaults (`ZENODO_TOKEN`/`ZENODO_PAT` and
#'   corresponding keyring entries).
#' @param include_embargo When `FALSE`, embargoed records are skipped.
#' @param include_measurements When `FALSE`, measurements files are not downloaded.
#'
#' @return Invisibly returns the output directory of the downloaded record.
#' @export
download_record <- function(
  rec,
  dp_dir = file.path("data", "dp"),
  overwrite = FALSE,
  quiet = TRUE,
  token = NULL,
  include_embargo = TRUE,
  include_measurements = FALSE
) {
  token <- GeoLocatoR:::resolve_zenodo_token(token = token)
  record <- if (is.list(rec)) {
    rec
  } else {
    .read_zenodo_record(rec = rec, token = token)
  }

  record_id <- .record_id_from_zenodo(record = record, rec = rec)
  record_label <- .record_label_from_zenodo(record)

  if (!include_embargo && .is_record_embargoed(record)) {
    cli::cli_warn(
      "Record {.field {record_label}} is under embargo. Skipping download..."
    )
    return(invisible(NULL))
  }

  record_dir <- file.path(dp_dir, record_label)
  dir.create(record_dir, recursive = TRUE, showWarnings = FALSE)

  # Cache files directly on disk: read_zenodo() materialises every measurement.
  # Existing version folders are reused; metadata/access status are refreshed.
  for (file in record$files$entries) {
    name <- file$key %||% file$filename
    if (!include_measurements && grepl("^measurements", name)) next
    destination <- file.path(record_dir, name)
    if (!overwrite && (file.exists(destination) ||
        (grepl("\\.gz$", name) && file.exists(sub("\\.gz$", "", destination))))) next
    if (!quiet) cli::cli_inform("Downloading {record_label}/{name}")
    url <- file$links$content %||% glue::glue("https://zenodo.org/api/records/{record_id}/files/{utils::URLencode(name, reserved = TRUE)}/content")
    request <- httr2::request(url) |>
      httr2::req_timeout(1800) |>
      httr2::req_retry(max_tries = 3)
    if (!is.null(token)) request <- httr2::req_auth_bearer_token(request, token)
    httr2::req_perform(request, path = paste0(destination, ".part"))
    checksum <- sub("^md5:", "", file$checksum)
    if (length(checksum) && unname(tools::md5sum(paste0(destination, ".part"))) != checksum) {
      cli::cli_abort("Checksum mismatch for {record_label}/{name}.")
    }
    file.rename(paste0(destination, ".part"), destination)
  }

  jsonlite::write_json(.record_metadata_for_disk(record),
    file.path(record_dir, "zenodo_metadata.json"), pretty = TRUE,
    auto_unbox = TRUE, null = "null", na = "null")
  jsonlite::write_json(record, file.path(record_dir, "zenodo_record.json"),
    pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  if (!file.exists(file.path(record_dir, "datapackage.json"))) {
    cli::cli_abort("No cached or downloadable datapackage.json for {record_label}.")
  }
  invisible(record_dir)
}

#' @noRd
.read_zenodo_record <- function(rec, token = NULL) {
  args <- c(list(id = rec, draft = FALSE), .zenodo_token_arg(token))

  tryCatch(
    do.call(GeoLocatoR:::read_zenodo_fetch_record, args),
    error = function(e) {
      if (length(args) > 2 && grepl("unused argument", conditionMessage(e))) {
        return(GeoLocatoR:::read_zenodo_fetch_record(id = rec, draft = FALSE))
      }
      stop(e)
    }
  )
}

#' @noRd
.read_zenodo_gldp <- function(record_id, token = NULL) {
  args <- c(list(id = record_id), .zenodo_token_arg(token))

  tryCatch(
    do.call(GeoLocatoR::read_zenodo, args),
    error = function(e) {
      if (length(args) > 1 && grepl("unused argument", conditionMessage(e))) {
        return(GeoLocatoR::read_zenodo(id = record_id))
      }
      stop(e)
    }
  )
}

#' @noRd
.zenodo_token_arg <- function(token) {
  token_chr <- as.character(token %||% "")[1]
  if (!nzchar(token_chr)) {
    return(list())
  }
  list(token = token_chr)
}

#' @noRd
.is_record_embargoed <- function(record) {
  status <- tolower(as.character(record$access$status %||% "")[1])
  active <- isTRUE(record$access$embargo$active %||% FALSE)
  identical(status, "embargoed") || active
}

#' @noRd
.record_id_from_zenodo <- function(record, rec = NULL) {
  id <- as.character(record$id %||% "")[1]
  if (nzchar(id)) {
    return(id)
  }
  GeoLocatoR:::parse_zenodo_id(rec)
}

#' @noRd
.record_concept_id_from_zenodo <- function(record, record_id = NULL) {
  concept_id <- as.character(
    record$parent$id %||%
      record$conceptrecid %||%
      record$concept_id %||%
      NA_character_
  )[1]
  if (!is.na(concept_id) && nzchar(concept_id)) {
    return(concept_id)
  }

  doi <- as.character(
    record$conceptdoi %||%
      record$concept_doi %||%
      record$doi %||%
      NA_character_
  )[1]
  if (!is.na(doi) && nzchar(doi) && grepl("zenodo\\.[0-9]+$", doi)) {
    return(sub("^.*zenodo\\.([0-9]+)$", "\\1", doi))
  }

  as.character(record_id %||% .record_id_from_zenodo(record))[1]
}

#' @noRd
.record_label_from_zenodo <- function(record) {
  record_id <- .record_id_from_zenodo(record)
  concept_id <- .record_concept_id_from_zenodo(record, record_id = record_id)
  glue::glue("{concept_id}_{record_id}")
}

#' @noRd
.record_metadata_for_disk <- function(record) {
  metadata <- record$metadata %||% list()
  metadata$access <- record$access %||% list()
  metadata$repository <- record$custom_fields$`code:codeRepository` %||%
    record$metadata$custom$`code:codeRepository` %||%
    NA_character_
  metadata
}

#' @noRd
.drop_measurements_resources <- function(pkg) {
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
