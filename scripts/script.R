# Run manually, section by section, from the GeoLocatorMaster project root.

# Load packages and choose the release --------------------------------------
# Use the adjacent GeoLocatoR development checkout for the DwC taxonomy option.
pkgload::load_all("../GeoLocatoR", quiet = TRUE)
# library(GeoLocatoR) # Use instead when scientific_name_ids is released.
library(dplyr)
library(readr)
pkgload::load_all(quiet = TRUE)
release_version <- "2026.0"
master_record_id <- "18187093"
master_doi <- paste0("https://doi.org/10.5281/zenodo.", master_record_id)

# Refresh sources -----------------------------------------------------------
# Reuse downloaded files and refresh their access status and Zenodo metadata.
# Caching measurements does not include them in the Master; no format decision is made here.
records <- download_community(include_embargo = TRUE, include_measurements = TRUE)
# records <- jsonlite::read_json("data/dp/community_records.json") # Work offline.
record_labels <- vapply(records, .record_label_from_zenodo, character(1))
pkg_list <- read_dp_packages(record_labels = record_labels, include_embargo = TRUE)
overview <- datapackage_overview(pkg_list)
# download_record(records[[1]], overwrite = TRUE) # Deliberately refresh one source.

# Write the public Master ---------------------------------------------------
# Catalogues describe every project; data tables contain only open sources.
# Fixed descriptor metadata lives in docs/master-metadata.json.
public_sources <- Filter(function(pkg) identical(pkg$access_status, "open"), pkg_list)
master <- merge_without_measurements(public_sources)
master <- write_master(master, overview, pkg_list, "data/master", release_version,
                       master_doi, sources = public_sources)

# Optional: write a separate local copy including authorised embargoed data.
# restricted <- merge_without_measurements(pkg_list)
# restricted <- write_master(restricted, overview, pkg_list, "data/master_restricted",
#                            release_version, NULL, sources = pkg_list)
# Optional: include cached measurements by adding include_measurements = TRUE
# to either write_master() call. This streams the CSV rather than loading it all.

# Write the GBIF review CSV --------------------------------------------------
# Leave taxonomy IDs blank for GBIF; retain Avibase identifiers in species.csv.
# Publisher, institutional and IPT metadata are maintained separately; see docs/gbif.md.
dir.create("data/dwc", showWarnings = FALSE)
occurrences <- GeoLocatoR::gldp_to_dwc(master, "data/dwc")

# Add each occurrence's source DOI and licence, rather than a blanket licence.
provenance <- GeoLocatoR::paths(master) |>
  filter(type == "most_likely") |>
  left_join(GeoLocatoR::tags(master), by = "tag_id") |>
  transmute(occurrenceID = glue::glue("{ring_number}_{tag_id}_{stap_id}"),
            associatedReferences = datapackage_id)
source_licences <- bind_rows(lapply(public_sources, function(pkg) {
  tibble(associatedReferences = pkg$id,
         source_license = pkg$licenses[[1]]$path %||% pkg$licenses[[1]]$name)
}))
occurrences <- occurrences |>
  select(-any_of("associatedReferences")) |>
  left_join(provenance, by = "occurrenceID", relationship = "one-to-one") |>
  left_join(source_licences, by = "associatedReferences") |>
  mutate(datasetID = master_doi, license = source_license) |> select(-source_license)
write_csv(occurrences, "data/dwc/occurrence.csv", na = "")

# Write dataset metadata separately from the occurrence CSV.
# Any fixed publication text is maintained in docs/master-metadata.json.
eml_pkg <- master
eml_pkg$temporal <- list(start = min(substr(occurrences$eventDate, 1, 10)),
                         end = max(substr(sub(".*/", "", occurrences$eventDate), 1, 10)))
eml_pkg$taxonomic <- unique(occurrences$scientificName)
longitude_range <- range(occurrences$decimalLongitude, na.rm = TRUE)
latitude_range <- range(occurrences$decimalLatitude, na.rm = TRUE)
eml_pkg$spatial <- list(type = "Polygon", coordinates = list(list(
  c(longitude_range[1], latitude_range[1]), c(longitude_range[2], latitude_range[1]),
  c(longitude_range[2], latitude_range[2]), c(longitude_range[1], latitude_range[2]),
  c(longitude_range[1], latitude_range[1]))))
eml <- GeoLocatoR::gldp_to_eml(eml_pkg, "data/dwc")

# Review the outputs --------------------------------------------------------
overview |> count(access_status)
GeoLocatoR::tags(master) |> count(scientific_name, sort = TRUE)
occurrences |> summarise(occurrences = n(), missing_coordinates = sum(is.na(decimalLatitude) | is.na(decimalLongitude)))
# source("tests/check_release.R") # Additional consistency checks, no extra outputs.
# Run Rscript tests/test_release.R from the project root before draft synchronisation.
# It tests the written package and saves diagnostics in output/release-audit/.

# Optional: synchronise public files to the existing Zenodo draft -------------
# Run after reviewing the local exports. This does not publish the draft.
# update_master_draft(master_record_id, master, public_sources,
#                     list.files("data/master", full.names = TRUE))
# update_creators(master, dry_run = TRUE) # Inspect creator updates separately.
