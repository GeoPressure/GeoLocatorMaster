#' Update the Master Zenodo creators from package contributors
#'
#' @param pkgs A Master package containing contributors.
#' @param dry_run If TRUE, return proposed creators without updating Zenodo.
#' @return The merged creators and their identity keys, invisibly.
#' @details Preserves accents and corrects seven known name-only source entries.
#'   Duplicates are merged upstream by `GeoLocatoR::merge_gldp()`.
#'   With dry_run = FALSE, updates the Master draft without publishing it.
#' @export
update_creators <- function(pkgs, dry_run = TRUE) {
  creators <- get_creators_from_pkgs(pkgs)
  apply_creators_update(creators = creators, dry_run = dry_run)
}

get_creators_from_pkgs <- function(pkgs) {
  if (is.null(pkgs$contributors)) {
    cli::cli_abort(
      "{.arg pkgs} must contain a {.field contributors} element."
    )
  }

  contributors <- normalize_contributors(pkgs$contributors)
  Filter(
    Negate(is.null),
    lapply(contributors, contributor_to_zenodo_creator)
  )
}

normalize_contributors <- function(contributors) {
  if (is.data.frame(contributors)) {
    rows <- split(contributors, seq_len(nrow(contributors)))
    return(lapply(rows, as.list))
  }

  if (is.list(contributors) && !is.null(names(contributors))) {
    fields <- c(
      "title",
      "name",
      "givenName",
      "given_name",
      "familyName",
      "family_name",
      "organization",
      "path"
    )
    if (any(fields %in% names(contributors))) {
      return(list(contributors))
    }
  }

  if (!is.list(contributors)) {
    return(as.list(contributors))
  }

  contributors
}

contributor_to_zenodo_creator <- function(contributor) {
  if (is.null(contributor)) {
    return(NULL)
  }
  if (!is.list(contributor)) {
    contributor <- as.list(contributor)
  }

  given_name <- get_scalar(first_non_null(contributor$givenName, contributor$given_name))
  family_name <- get_scalar(first_non_null(contributor$familyName, contributor$family_name))
  name <- get_scalar(first_non_null(contributor$title, contributor$name))

  # These source creators contain only a full name; preserve compound surnames.
  if (family_name == "") {
    corrected <- switch(name,
      "Fanny Rey" = c("Fanny", "Rey"),
      "Fr\u00e9d\u00e9ric Jiguet" = c("Fr\u00e9d\u00e9ric", "Jiguet"),
      "Paul Doniol-Valcroze" = c("Paul", "Doniol-Valcroze"),
      "Pierrick Bocher" = c("Pierrick", "Bocher"),
      "Rose Delacroix" = c("Rose", "Delacroix"),
      "Sophie de Grissac" = c("Sophie", "de Grissac"),
      "Yannig Coulomb" = c("Yannig", "Coulomb"))
    if (!is.null(corrected)) {
      given_name <- corrected[1]
      family_name <- corrected[2]
      name <- paste(family_name, given_name, sep = ", ")
    }
  }

  affiliations <- split_affiliations(contributor$organization)
  if (name == "") {
    parts <- c(family_name, given_name)
    parts <- parts[nzchar(parts)]
    if (length(parts) > 0) {
      name <- paste(parts, collapse = ", ")
    } else if (length(affiliations) > 0) {
      name <- affiliations[[1]]$name
    }
  }
  if (name == "") {
    return(NULL)
  }

  person_or_org <- list(type = "personal", name = name)
  if (given_name != "") {
    person_or_org$given_name <- given_name
  }
  if (family_name != "") {
    person_or_org$family_name <- family_name
  }

  orcid <- extract_orcid_from_path(contributor$path)
  if (orcid != "") {
    person_or_org$identifiers <- list(
      list(identifier = orcid, scheme = "orcid")
    )
  }

  creator <- list(person_or_org = person_or_org)
  if (length(affiliations) > 0) {
    creator$affiliations <- affiliations
  }

  creator
}

apply_creators_update <- function(creators, dry_run = TRUE) {
  merged <- order_creators(creators)

  if (length(merged$creators) == 0) {
    message("No creators found.")
    return(invisible(merged))
  }

  if (dry_run) {
    message(sprintf(
      "Dry run: would set %d creators on Zenodo.",
      length(merged$creators)
    ))
    return(invisible(merged))
  }

  .update_master_record_metadata(list(creators = merged$creators))

  invisible(merged)
}

# Contributors are merged per person (ORCID or unambiguous name, with all roles
# and affiliations) by GeoLocatoR::merge_gldp(); here they are only ordered.
order_creators <- function(creators) {
  if (length(creators) == 0) {
    return(list(creators = list(), warnings = character()))
  }
  creators <- creators[order(vapply(creators, get_creator_name, character(1)))]
  creators <- reorder_creators_by_orcid(creators, first_orcid = "0000-0002-8185-1020")
  list(creators = creators, warnings = character())
}

creator_orcid <- function(entry) {
  ids <- entry$person_or_org$identifiers
  if (is.list(ids) && !is.null(names(ids)) && any(c("scheme", "identifier") %in% names(ids))) {
    ids <- list(ids)
  }

  if (is.list(ids)) {
    for (id in ids) {
      if (!is.list(id)) {
        next
      }

      scheme <- tolower(get_scalar(id$scheme))
      identifier <- get_scalar(id$identifier)
      if (scheme == "orcid" && identifier != "") {
        return(identifier)
      }
    }
  }

  extract_orcid_from_path(entry$path)
}

reorder_creators_by_orcid <- function(creators, first_orcid, last_orcid = NULL) {
  if (length(creators) == 0) {
    return(creators)
  }

  orcids <- vapply(creators, creator_orcid, character(1))
  first_idx <- which(orcids == first_orcid)
  last_idx <- integer(0)
  if (!is.null(last_orcid) && nzchar(last_orcid)) {
    last_idx <- which(orcids == last_orcid)
  }

  if (length(first_idx) == 0 && length(last_idx) == 0) {
    return(creators)
  }

  remaining <- seq_along(creators)
  if (length(first_idx) > 0) {
    remaining <- setdiff(remaining, first_idx[1])
  }
  if (length(last_idx) > 0) {
    remaining <- setdiff(remaining, last_idx[1])
  }

  out <- list()
  if (length(first_idx) > 0) {
    out <- c(out, list(creators[[first_idx[1]]]))
  }
  if (length(remaining) > 0) {
    out <- c(out, creators[remaining])
  }
  if (length(last_idx) > 0) {
    out <- c(out, list(creators[[last_idx[1]]]))
  }

  out
}

get_creator_name <- function(entry) {
  name <- get_scalar(first_non_null(entry$person_or_org$name, entry$title, entry$name))
  if (name != "") {
    return(name)
  }

  given <- get_scalar(first_non_null(
    entry$givenName,
    entry$given_name,
    entry$person_or_org$given_name,
    entry$person_or_org$givenName
  ))
  family <- get_scalar(first_non_null(
    entry$familyName,
    entry$family_name,
    entry$person_or_org$family_name,
    entry$person_or_org$familyName
  ))

  if (given != "" || family != "") {
    parts <- c(family, given)
    parts <- parts[nzchar(parts)]
    return(paste(parts, collapse = ", "))
  }

  get_scalar(entry$organization)
}

extract_orcid_from_path <- function(path) {
  value <- get_scalar(path)
  if (!nzchar(value) || !grepl("orcid.org/", value, fixed = TRUE)) {
    return("")
  }

  value <- sub(".*orcid.org/", "", value)
  value <- sub("/.*$", "", value)
  trimws(value)
}

split_affiliations <- function(organization) {
  value <- get_scalar(organization)
  if (!nzchar(value)) {
    return(list())
  }

  # Organizations are separated by ";" (GeoLocatoR::merge_gldp() convention); a
  # comma belongs to the address, e.g. "University of Cape Town, FitzPatrick Institute".
  parts <- strsplit(value, "\\s*;\\s*")[[1]]
  parts <- unique(trimws(parts))
  parts <- parts[nzchar(parts)]
  lapply(parts, function(x) list(name = x))
}

first_non_null <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value)) {
      return(value)
    }
  }
  NULL
}

get_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }
  as.character(x)[1]
}
