#' Apply manual metadata enhancements to a datapackage object
#'
#' Currently this function standardizes known typo/variant values in
#' `tags$manufacturer`.
#'
#' @param pkgs A GeoLocatoR datapackage object.
#'
#' @return Updated datapackage object.
#' @export
enhanced_pkgs <- function(pkgs) {
  pkgs <- .normalize_tag_manufacturer(pkgs)

  pkgs <- .add_species(pkgs)
}

.normalize_tag_manufacturer <- function(pkgs) {
  t <- GeoLocatoR::tags(pkgs)

  replacements <- list(
    "Migrate Technology Limited" = c(
      "Migrate Technology",
      "Migratech Technology"
    ),
    "Swiss Ornithological Institute" = c("Swiss Ornithological Institue"),
    "Lund University" = c("Lund CAnMove", "Lund")
  )

  lookup <- unlist(
    lapply(names(replacements), \(new) {
      stats::setNames(
        rep(new, length(replacements[[new]])),
        replacements[[new]]
      )
    }),
    use.names = TRUE
  )

  t$manufacturer <- dplyr::coalesce(
    unname(lookup[t$manufacturer]),
    t$manufacturer
  )
  GeoLocatoR::tags(pkgs) <- t
  pkgs
}

.add_species <- function(pkgs) {
  normalize_species_name <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(character())
    }

    x <- as.character(x)
    x <- trimws(x)
    x <- gsub("\\s+", " ", x)
    x
  }

  empty_species_table <- function() {
    data.frame(
      scientific_name_input = character(),
      Scientific_name = character(),
      English_name_AviList = character(),
      Order = character(),
      Family = character(),
      Family_English_name = character(),
      Sequence = character(),
      Species_code_Cornell_Lab = character(),
      AvibaseID = character(),
      IUCN_Red_List_Category = character(),
      Birds_of_the_World_URL = character(),
      BirdLife_DataZone_URL = character(),
      in_ebirdst = logical(),
      stringsAsFactors = FALSE
    )
  }

  scientific_names <- GeoLocatoR::tags(pkgs)$scientific_name
  scientific_names <- normalize_species_name(scientific_names)
  scientific_names <- scientific_names[nzchar(scientific_names)]
  scientific_names <- unique(scientific_names)

  if (length(scientific_names) == 0) {
    pkgs$species <- empty_species_table()
    return(pkgs)
  }

  typo_corrections <- c(
    "Caprimulgus europeaus" = "Caprimulgus europaeus",
    "Setophaga coronata coronata" = "Setophaga coronata"
  )

  species_input <- data.frame(
    scientific_name_input = scientific_names,
    stringsAsFactors = FALSE
  )
  species_input$match_name <- ifelse(
    species_input$scientific_name_input %in% names(typo_corrections),
    unname(typo_corrections[species_input$scientific_name_input]),
    species_input$scientific_name_input
  )
  species_input$match_key <- tolower(normalize_species_name(
    species_input$match_name
  ))

  if (!requireNamespace("readxl", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg readxl} is required to read AviList taxonomy.",
        "i" = "Install it with {.code install.packages('readxl')}."
      )
    )
  }

  avilist_path <- file.path("data", "AviList-v2025-11Jun-extended.xlsx")
  if (!file.exists(avilist_path)) {
    cli::cli_abort("AviList taxonomy file not found: {.file {avilist_path}}.")
  }

  avilist <- tryCatch(
    as.data.frame(
      readxl::read_excel(avilist_path, sheet = "AviList v2025 extended"),
      stringsAsFactors = FALSE
    ),
    error = function(e) {
      cli::cli_abort("Could not read AviList taxonomy file: {e$message}.")
    }
  )

  required_cols <- c(
    "Sequence",
    "Order",
    "Family",
    "Family_English_name",
    "Scientific_name",
    "English_name_AviList",
    "Species_code_Cornell_Lab",
    "AvibaseID",
    "IUCN_Red_List_Category",
    "Birds_of_the_World_URL",
    "BirdLife_DataZone_URL"
  )
  missing_cols <- setdiff(required_cols, names(avilist))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "AviList file is missing required columns: {paste(missing_cols, collapse = ', ')}."
    )
  }

  avilist$match_key <- tolower(normalize_species_name(avilist$Scientific_name))
  avilist <- avilist[
    nzchar(avilist$match_key),
    c(required_cols, "match_key"),
    drop = FALSE
  ]

  duplicated_keys <- unique(avilist$match_key[duplicated(avilist$match_key)])
  if (length(duplicated_keys) > 0) {
    cli::cli_abort(
      "AviList contains duplicated scientific names for matching."
    )
  }

  match_idx <- match(species_input$match_key, avilist$match_key)
  unmatched <- species_input$scientific_name_input[is.na(match_idx)]
  if (length(unmatched) > 0) {
    cli::cli_abort(
      "Unmatched species in {.field tags(pkgs)$scientific_name}: {paste(unmatched, collapse = ', ')}."
    )
  }

  matched <- avilist[match_idx, required_cols, drop = FALSE]
  rownames(matched) <- NULL

  if (!requireNamespace("ebirdst", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg ebirdst} is required to build {.field pkgs$species}.",
        "i" = "Install it with {.code install.packages('ebirdst')}."
      )
    )
  }

  ebirdst_runs <- tryCatch(
    as.data.frame(ebirdst::ebirdst_runs, stringsAsFactors = FALSE),
    error = function(e) {
      cli::cli_abort("Could not load {.pkg ebirdst} runs data: {e$message}.")
    }
  )

  sci_candidates <- c("scientific_name", "sciName", "sci_name")
  ebirdst_sci_col <- sci_candidates[sci_candidates %in% names(ebirdst_runs)][1]
  if (is.na(ebirdst_sci_col) || !nzchar(ebirdst_sci_col)) {
    cli::cli_abort(
      "`ebirdst::ebirdst_runs` does not contain a scientific-name column."
    )
  }

  ebirdst_keys <- unique(tolower(normalize_species_name(ebirdst_runs[[
    ebirdst_sci_col
  ]])))
  ebirdst_keys <- ebirdst_keys[nzchar(ebirdst_keys)]

  out <- data.frame(
    scientific_name_input = species_input$scientific_name_input,
    Scientific_name = matched$Scientific_name,
    English_name_AviList = matched$English_name_AviList,
    Order = matched$Order,
    Family = matched$Family,
    Family_English_name = matched$Family_English_name,
    Sequence = matched$Sequence,
    Species_code_Cornell_Lab = matched$Species_code_Cornell_Lab,
    AvibaseID = matched$AvibaseID,
    IUCN_Red_List_Category = matched$IUCN_Red_List_Category,
    Birds_of_the_World_URL = matched$Birds_of_the_World_URL,
    BirdLife_DataZone_URL = matched$BirdLife_DataZone_URL,
    in_ebirdst = species_input$match_key %in% ebirdst_keys,
    stringsAsFactors = FALSE
  )
  seq_num <- suppressWarnings(as.numeric(out$Sequence))
  pkgs$species <- out[order(seq_num, out$Scientific_name, na.last = TRUE), , drop = FALSE]

  pkgs
}
