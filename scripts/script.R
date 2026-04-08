library(GeoLocatoR)
library(zen4R)
library(tidyverse)
library(pkgload)
load_all()

# Download all datapackage in data/dp
download_community(include_embargo = TRUE)

# Combine all datapacakge from data/dp
pkg_list <- read_dp_packages(include_embargo = FALSE)
pkg_list <- prepare_dp_packages(pkg_list)
pkgs <- GeoLocatoR::merge_gldp(pkg_list)
pkgs <- enhanced_pkgs(pkgs)

# Get Master DP Zendo record

write_gldp(pkgs, "data/master")
# We don't export those because they don't have the restricted/embargoed records (See below)
# write_csv(pkgs$datapackages, "data/master/datapackages.csv")
# write_csv(pkgs$species, "data/master/species.csv")

# With embargoed record
pkg_list_res <- read_dp_packages(include_embargo = TRUE)
pkg_list_res <- prepare_dp_packages(pkg_list_res)
pkgs_res <- GeoLocatoR::merge_gldp(pkg_list_res)
pkgs_res <- enhanced_pkgs(pkgs_res)

# Export all datapacakge metadata
write_csv(pkgs_res$datapackages, "data/master/datapackages.csv")
write_csv(pkgs_res$species, "data/master/species.csv")

# For personal use only!!
write_gldp(pkgs_res, "data/master_restricted")
write_csv(pkgs_res$datapackages, "data/master_restricted/datapackages.csv")
write_csv(pkgs_res$species, "data/master_restricted/species.csv")

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
files <- files[basename(files) != "measurement.csv"]

file.copy(
    from = files,
    to = "/Users/rafnuss/Documents/GitHub/GeoLocatorExplorer/raw_data",
    recursive = TRUE,
    overwrite = TRUE
)
