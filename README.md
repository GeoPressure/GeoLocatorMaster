# GeoLocatorMaster

`GeoLocatorMaster` is a project repository for building and maintaining the
master GeoLocator datapackage from records in the
[Zenodo `geolocator-dp` community](https://zenodo.org/communities/geolocator-dp/).
The master datapackage maintained by this repository is the concept DOI
[`10.5281/zenodo.18187092`](https://doi.org/10.5281/zenodo.18187092).

## Workflow

The main workflow is implemented in [`scripts/script.R`](scripts/script.R):

1. Download GeoLocator datapackages into `data/dp/`
2. Read them from disk into R
3. Prepare them for aggregation by resolving conflicting `tag_id` values
4. Merge them into a single GeoLocatoR object
5. Enrich the merged object with harmonized metadata and a species table
6. Write public and restricted master outputs
7. Optionally update metadata on the Zenodo master record

## Project functions

Reusable functions live in [`R/`](R), including:

- `download_community()`
- `download_record()`
- `read_dp_packages()`
- `prepare_dp_packages()`
- `enhanced_pkgs()`
- `rename_tag_id()`
- `update_creators()`
- `update_related_identifiers()`

This repository keeps minimal package metadata so `pkgload::load_all()` works
for local development, but it is not maintained as a released R package.

## Requirements

Some steps also require:

- `data/AviList-v2025-11Jun-extended.xlsx` for species enrichment
- a valid Zenodo token configured locally for Zenodo metadata updates

## Notes

- Embargoed records can be included or excluded depending on function arguments.
- Measurement resources can be kept or dropped at download and preparation stages.
- Species enrichment depends on the local AviList taxonomy file.
