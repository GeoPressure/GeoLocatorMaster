#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(writexl)
})

args <- commandArgs(trailingOnly = TRUE)
default_csv <- "GeoLocator Consortium - Form responses 1.csv"
if (!file.exists(default_csv)) {
  alt_csv <- "data/GeoLocator Consortium - Form responses 1.csv"
  default_csv <- if (file.exists(alt_csv)) alt_csv else default_csv
}
input_csv <- ifelse(length(args) >= 1, args[[1]], default_csv)

col_submitter_email <- "Email address"
col_given <- "Given Name"
col_family <- "Family Name"
col_institutional_email <- "Institutional email"
col_primary_affiliation <- "Primary affiliations (including name, country, city)"
col_secondary_affiliation <- "Secondary affiliations (including name, country, city)"
col_orcid <- "Your ORCID (Open Researcher and Contributor ID)"
col_contrib <- "How would you describe your past or planned contributions to the Consortium"

responses_raw <- read_csv(input_csv, show_col_types = FALSE)

get_col <- function(name) {
  if (name %in% names(responses_raw)) {
    responses_raw[[name]]
  } else {
    rep(NA_character_, nrow(responses_raw))
  }
}

required_cols <- c(col_given, col_family, col_contrib)
missing_cols <- setdiff(required_cols, names(responses_raw))
if (length(missing_cols) > 0) {
  stop(
    sprintf(
      "Missing required column(s): %s",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

responses <- tibble(
  author_id = seq_len(nrow(responses_raw)),
  given_name = str_squish(get_col(col_given)),
  family_name = str_squish(get_col(col_family)),
  submitter_email = str_squish(get_col(col_submitter_email)),
  institutional_email = str_squish(get_col(col_institutional_email)),
  primary_affiliation = str_squish(get_col(col_primary_affiliation)),
  secondary_affiliations = str_squish(get_col(col_secondary_affiliation)),
  orcid = str_squish(get_col(col_orcid)),
  contribution_raw = str_squish(get_col(col_contrib))
) %>%
  mutate(full_name = str_squish(paste(given_name, family_name))) %>%
  filter(full_name != "", contribution_raw != "")

mapping <- tibble::tribble(
  ~pattern                                                                          , ~contribution_type       , ~manuscript_phrase                                         ,
  "Published DP: performed GeoPressureR analysis"                                   , "Formal analysis"        , "Project level analysis (GeoPressureR)"                                                  ,
  "Published DP: created/published data package"                                    , "Data curation"          , "Created and published a Data Package (GeoLocatoR and Zenodo)"                           ,
  "Published DP: administration\\s*&\\s*management"                                 , "Leadership"             , "Administration, supervision, and funding"                                               ,
  "Published DP: administration, management and funding"                            , "Leadership"             , "Administration, supervision, and funding"                                               ,
  "Published DP: field work"                                                        , "Investigation"          , "Field work"                                                                             ,
  "Code contribution: GeoLocatoR R package"                                         , "Software"               , "GeoLocatoR R package"                                                                   ,
  "Code contribution: GeoLocator Data Package specification"                        , "Methodology"            , "GeoLocator Data Package specification"                                                  ,
  "Prospective contributor: Submit DP by 1st of March with raw data only"           , "Data curation"          , "Created and published a Data Package (GeoLocatoR and Zenodo)"                           ,
  "Prospective contributor: Submit DP by 1st of March with raw data only"           , "Investigation"          , "Field work"                                                                             ,
  "Prospective contributor: Submit a DP by 1st of March with GeoPressureR analysis" , "Formal analysis"        , "Project level analysis (GeoPressureR)"                                                  ,
  "Prospective contributor: Submit a DP by 1st of March with GeoPressureR analysis" , "Data curation"          , "Created and published a Data Package (GeoLocatoR and Zenodo)"
)

matched <- tidyr::crossing(responses, mapping) %>%
  filter(str_detect(contribution_raw, regex(pattern, ignore_case = TRUE))) %>%
  distinct(author_id, full_name, contribution_type, manuscript_phrase)

if (nrow(matched) == 0) {
  stop(
    "No contribution patterns were matched. Check mapping patterns.",
    call. = FALSE
  )
}

build_initial_map <- function(names_vec) {
  name_tbl <- tibble(full_name = sort(unique(names_vec))) %>%
    mutate(
      clean_name = str_squish(str_replace_all(
        full_name,
        "[^[:alpha:]\\s-]",
        " "
      )),
      clean_name = iconv(clean_name, from = "", to = "ASCII//TRANSLIT"),
      tokens = str_split(clean_name, "[\\s-]+"),
      has_hyphen = str_detect(full_name, "-"),
      given = vapply(tokens, function(x) x[1], character(1)),
      family = vapply(tokens, function(x) x[length(x)], character(1)),
      hyphen_initials = vapply(tokens, function(x) {
        parts <- x[seq_len(min(3, length(x)))]
        paste0(toupper(substr(parts, 1, 1)), collapse = "")
      }, character(1)),
      base = if_else(
        has_hyphen,
        hyphen_initials,
        paste0(toupper(substr(given, 1, 1)), toupper(substr(family, 1, 1)))
      )
    ) %>%
    group_by(base) %>%
    mutate(
      initials = if (n() == 1 || all(has_hyphen)) {
        base
      } else {
        paste0(toupper(substr(given, 1, 1)), toupper(substr(family, 1, 2)))
      }
    ) %>%
    ungroup()

  setNames(name_tbl$initials, name_tbl$full_name)
}

collapse_people <- function(x) {
  x <- sort(unique(x))
  x <- unname(initial_map[x])
  n <- length(x)
  if (n == 1) {
    return(x)
  }
  if (n == 2) {
    return(paste(x, collapse = " and "))
  }
  paste0(paste(x[-n], collapse = ", "), ", and ", x[n])
}

initial_map <- build_initial_map(matched$full_name)

order_levels <- c(
  "GeoLocator Data Package specification",
  "GeoLocatoR R package",
  "Project level analysis (GeoPressureR)",
  "Created and published a Data Package (GeoLocatoR and Zenodo)",
  "Field work",
  "Administration, supervision, and funding"
)

clauses <- matched %>%
  group_by(manuscript_phrase) %>%
  summarise(authors = collapse_people(full_name), .groups = "drop") %>%
  mutate(
    manuscript_phrase = factor(manuscript_phrase, levels = order_levels)
  ) %>%
  arrange(manuscript_phrase) %>%
  mutate(clause = paste0("- ", as.character(manuscript_phrase), ": ", authors))

cat("Author contributions:\n")
cat(paste(clauses$clause, collapse = "\n"), "\n")

author_export <- responses %>%
  semi_join(matched, by = "author_id") %>%
  distinct(author_id, .keep_all = TRUE) %>%
  mutate(
    Initials = unname(initial_map[full_name]),
    email = na_if(institutional_email, ""),
    ORCID = na_if(
      str_remove(orcid, "^https?://orcid\\.org/"),
      ""
    )
  ) %>%
  transmute(
    given_name,
    family_name,
    Initials,
    email,
    primary_affiliation,
    secondary_affiliations,
    ORCID
  ) %>%
  arrange(family_name, given_name)

output_xlsx <- file.path("data", "author_contributions.xlsx")
dir.create(dirname(output_xlsx), recursive = TRUE, showWarnings = FALSE)
write_xlsx(
  list(author_contributions = author_export),
  path = output_xlsx
)
