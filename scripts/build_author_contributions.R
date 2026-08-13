#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(writexl)
})

col_submitter_email <- "Email address"
col_given <- "Given Name"
col_family <- "Family Name"
col_institutional_email <- "Institutional email"
col_primary_affiliation <- "Primary affiliations (including name, country, city)"
col_secondary_affiliation <- "Secondary affiliations (including name, country, city)"
col_orcid <- "Your ORCID (Open Researcher and Contributor ID)"
col_contrib <- "How would you describe your past or planned contributions to the Consortium"

responses_raw <- read_csv(
  "data/GeoLocator Consortium - Form responses 1.csv",
  show_col_types = FALSE
)

responses <- responses_raw %>%
  transmute(
    author_id = row_number(),
    given_name = str_squish(.data[[col_given]]),
    family_name = str_squish(.data[[col_family]]),
    submitter_email = str_squish(.data[[col_submitter_email]]),
    institutional_email = str_squish(.data[[col_institutional_email]]),
    primary_affiliation = str_squish(.data[[col_primary_affiliation]]),
    secondary_affiliations = str_squish(.data[[col_secondary_affiliation]]),
    orcid = str_squish(.data[[col_orcid]]),
    contribution_raw = str_squish(.data[[col_contrib]])
  ) %>%
  mutate(full_name = str_squish(paste(given_name, family_name))) %>%
  filter(full_name != "", contribution_raw != "")

mapping <- tibble::tribble(
  ~pattern                                                                          , ~contribution_type , ~manuscript_phrase                                             ,
  "Published DP: performed GeoPressureR analysis"                                   , "Formal analysis"  , "Project level analysis (GeoPressureR)"                        ,
  "Published DP: created/published data package"                                    , "Data curation"    , "Created and published a Data Package (GeoLocatoR and Zenodo)" ,
  "Published DP: administration\\s*&\\s*management"                                 , "Leadership"       , "Administration, supervision, and funding"                     ,
  "Published DP: administration, management and funding"                            , "Leadership"       , "Administration, supervision, and funding"                     ,
  "Published DP: field work"                                                        , "Investigation"    , "Field work"                                                   ,
  "Code contribution: GeoLocatoR R package"                                         , "Software"         , "GeoLocatoR R package"                                         ,
  "Code contribution: GeoLocator Data Package specification"                        , "Methodology"      , "GeoLocator Data Package specification"                        ,
  "Prospective contributor: Submit DP by 1st of March with raw data only"           , "Data curation"    , "Created and published a Data Package (GeoLocatoR and Zenodo)" ,
  "Prospective contributor: Submit DP by 1st of March with raw data only"           , "Investigation"    , "Field work"                                                   ,
  "Prospective contributor: Submit a DP by 1st of March with GeoPressureR analysis" , "Formal analysis"  , "Project level analysis (GeoPressureR)"                        ,
  "Prospective contributor: Submit a DP by 1st of March with GeoPressureR analysis" , "Data curation"    , "Created and published a Data Package (GeoLocatoR and Zenodo)"
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

initial_from_part <- function(part) {
  clean_part <- part %>%
    str_replace_all("[^[:alpha:]-]", " ") %>%
    str_squish() %>%
    iconv(from = "", to = "ASCII//TRANSLIT")

  if (is.na(clean_part) || clean_part == "") {
    return("")
  }

  groups <- str_split(clean_part, "\\s+")[[1]]
  pieces <- unlist(str_split(groups, "-", n = Inf), use.names = FALSE)
  separators <- unlist(
    lapply(groups, function(group) {
      group_pieces <- str_split(group, "-", n = Inf)[[1]]
      c(rep(".-", max(length(group_pieces) - 1, 0)), ".")
    }),
    use.names = FALSE
  )

  paste0(toupper(substr(pieces, 1, 1)), separators, collapse = "")
}

family_disambiguator <- function(family_names) {
  keys <- family_names %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("[^[:alpha:]]", "")

  if (any(keys == "") || anyDuplicated(keys)) {
    return(rep(NA_character_, length(keys)))
  }

  vapply(
    seq_along(keys),
    function(i) {
      key <- keys[[i]]
      other_keys <- keys[-i]

      for (len in seq.int(2, nchar(key))) {
        prefix <- substr(key, 1, len)
        if (!any(substr(other_keys, 1, len) == prefix)) {
          return(paste0(str_to_title(prefix), "."))
        }
      }

      NA_character_
    },
    character(1)
  )
}

build_initial_map <- function(author_tbl) {
  name_tbl <- author_tbl %>%
    distinct(full_name, given_name, family_name) %>%
    arrange(full_name) %>%
    mutate(
      given_initials = vapply(given_name, initial_from_part, character(1)),
      family_initials = vapply(family_name, initial_from_part, character(1)),
      base_initials = paste0(given_initials, family_initials)
    ) %>%
    group_by(base_initials) %>%
    mutate(
      compact_family = family_disambiguator(family_name),
      initials = if (n() == 1) {
        base_initials
      } else if (!anyDuplicated(family_name) && all(!is.na(compact_family))) {
        paste0(given_initials, compact_family)
      } else {
        full_name
      }
    ) %>%
    ungroup() %>%
    select(full_name, initials)

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

initial_map <- build_initial_map(
  responses %>% semi_join(matched, by = "author_id")
)

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
