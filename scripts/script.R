library(GeoLocatoR)
library(tidyverse)
library(pkgload)
load_all()

# Download all datapackage in data/dp
download_community(include_embargo = TRUE)

# Combine all datapacakge from data/dp
pkg_list <- read_dp_packages(include_embargo = FALSE, force_read = FALSE)
pkgs <- merge_without_measurements(pkg_list)
rm(pkg_list)
gc(full = TRUE)

# Get Master DP Zendo record

unlink(file.path("data", "master", "measurements.csv"))
write_gldp(pkgs, "data/master")
# We don't export those because they don't have the restricted/embargoed records (See below)
# write_csv(pkgs$datapackages, "data/master/datapackages.csv")
# write_csv(pkgs$species, "data/master/species.csv")

# With embargoed record
pkg_list_res <- read_dp_packages(include_embargo = TRUE, force_read = FALSE)
pkgs_res <- merge_without_measurements(pkg_list_res)
rm(pkg_list_res)
gc(full = TRUE)

# Export all datapacakge metadata
write_csv(pkgs_res$datapackages, "data/master/datapackages.csv", na = "")
write_csv(pkgs_res$species, "data/master/species.csv", na = "")

# For personal use only!!
unlink(file.path("data", "master_restricted", "measurements.csv"))
write_gldp(pkgs_res, "data/master_restricted")
write_csv(
    pkgs_res$datapackages,
    "data/master_restricted/datapackages.csv",
    na = ""
)
write_csv(pkgs_res$species, "data/master_restricted/species.csv", na = "")

# Update creator metadata on the master Zenodo record
# https://doi.org/10.5281/zenodo.18187092
update_creators(pkgs, dry_run = FALSE)

# Update related work
update_related_identifiers(pkgs, dry_run = FALSE)


# Update GeoLocatorExplorer
files <- list.files(
    "data/master",
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
)
files <- files[basename(files) != "measurements.csv"]

file.copy(
    from = files,
    to = "/Users/rafnuss/Documents/GitHub/GeoLocatorExplorer/raw_data",
    recursive = TRUE,
    overwrite = TRUE
)


# Export for GBIF

pkgs <- read_gldp("data/master/datapackage.json")

load_all("/Users/rafnuss/Documents/GitHub/GeoLocatoR/")
pkgs$title <- "Master GeoLocator Data Package"
gldp_to_dwc(pkgs, "data/dwc")


tags <- tags(pkgs)
scientific_name_ids <- tags |>
    dplyr::distinct(.data$scientific_name)
if (requireNamespace("movepub", quietly = TRUE)) {
    scientific_name_ids <- scientific_name_ids |>
        dplyr::mutate(
            scientificNameID = purrr::map_chr(
                .data$scientific_name,
                \(scientific_name) {
                    aphia <- movepub::get_aphia_id(scientific_name)
                    aphia_lsid <- aphia$aphia_lsid[!is.na(aphia$aphia_lsid)]
                    if (length(aphia_lsid) == 0) {
                        NA_character_
                    } else {
                        aphia_lsid[[1]]
                    }
                }
            )
        )
} else {
    scientific_name_ids <- scientific_name_ids |>
        dplyr::mutate(scientificNameID = NA_character_)
}
