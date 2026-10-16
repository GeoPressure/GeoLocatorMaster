# Workflow and functions

The routine workflow is in `scripts/script.R`. It calls `download_community()`, `read_dp_packages()`, `datapackage_overview()`, `merge_without_measurements()`, `write_master()` and `GeoLocatoR::gldp_to_dwc()`. These represent separate steps with useful intermediate objects. Only the Zenodo upload is an optional remote write; source refresh downloads and caches input files.

The release workflow is consolidated in the main script; analysis and plotting scripts are maintained in the separate GeoLocatorAnalysis project. Release checks remain available in `tests/check_release.R`, without generating audit files. Existing functions and their flexibility are retained, including separate creator/relationship updates, package preparation, identifier correction and optional measurement export. Most functions in `R/` are internal helpers for these operations; they do not need to be called manually. The new `write_master()` and `update_master_draft()` functions keep descriptor generation and draft synchronisation explicit.

## Catalogue columns

`datapackages.csv` includes identifiers, titles, versions, access status, contributors (separated by `; `), licences, related identifiers, temporal/species coverage, resource/sensor tag counts and trajectory coverage: `trajectories_with_wind_pct` (share of tracked tags whose most likely flights include wind) and `simulations_per_trajectory_median`. The catalogue and species table cover all contributing projects, including embargoed ones; public resource rows come only from open projects. Both tables also appear in `datapackage.json`.

`samplingInterval_<sensor>_min` lists the most frequent positive within-tag intervals accounting for 95% of consecutive pairs, in decreasing frequency order. Summaries include labelled measurements. `measurement_rows_cached` and `measurement_bytes_cached` describe source files on disk, rather than rows included in the Master. Small `*.summary.rds` caches beside source measurements avoid rescanning gigabytes on every build.

Measurement-derived coverage and sensor counts are also retained in `measurementMetadata` when measurements are omitted. Reopening with GeoLocatoR can recompute its usual summary fields from included resources; use the catalogue or preserved metadata for source measurement coverage.

## Package API and checks

The routine build uses exported functions so they are also available after loading the package. `write_master()` reads fixed metadata from `docs/master-metadata.json` relative to the project root, writes resource tables and catalogues, and returns the Master object. `update_master_draft()` updates an existing draft's version, creators, source relationships and complete file set; it deletes obsolete draft files and does not publish.

`update_creators()` and `update_related_identifiers()` default to dry runs. The former returns the proposed merged creators; the latter proposes source concept-DOI relationships and replaces the full relationship list when applied. Prefer `update_master_draft()` for release-specific source DOIs and preservation of unrelated relationships.

Use `Rscript tests/test_creators.R` and `Rscript tests/test_pipeline.R` from the project root for focused checks without source data or the taxonomy workbook. `tests/check_release.R` runs in the main workflow's R session because it inspects `master`, `occurrences`, `public_sources` and `master_doi`; it is excluded from package builds. A complete release build requires the development dependency and local inputs.

## Local caches and analysis files

On the maintainer’s machine, debugging outputs, backups and storage benchmarks were archived in `../GeoLocatorAnalysis/archive/master-build-2026-10-13/`. The measurement benchmark script lives in `../GeoLocatorAnalysis/scripts/profile_measurements.R`. These local paths are historical notes, not inputs distributed with this repository.

`data/dp/community_records.json` supports offline rebuilding of the last discovered inventory. Keep this file and the source folders for reproducibility. Source folders may include older versions; the main script selects only those listed in the inventory. Deleting the cache forces fresh downloads and may lose access to embargoed files that are available only locally.

The main script does not produce logs, intermediate RDS objects, checksums or release reports. The existing upload helper can save draft snapshots when explicitly given `audit_dir`; they are otherwise omitted.
