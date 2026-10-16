# GeoLocatorMaster

Build and maintain the [Master GeoLocator Data Package](https://doi.org/10.5281/zenodo.18187092) from the Zenodo `geolocator-dp` community, and prepare Darwin Core and EML files for GBIF review. This is an internal R project package with an interactive release workflow.

## Setup

Use R 4.4 or later and clone this repository beside a `GeoLocatoR` checkout. The current workflow requires the GeoLocatoR development API, including `read_gldp(drop_measurements = TRUE)`, `gldp_to_dwc()` and `gldp_to_eml()`. It was checked with GeoLocatoR commit `c381e0536a0614ce341726da48ad58a83131c57e` on `dev`; an older installed release may not provide these features.

Install the dependencies listed in [`DESCRIPTION`](DESCRIPTION), including `pkgload` for local development and `ebirdst` for species enrichment. Place the taxonomy workbook at `data/AviList-v2025-11Jun-extended.xlsx`; it is a required local input and is not distributed in this repository. The reuse walkthrough requires Quarto, `knitr`, `rmarkdown` and `ggplot2`. Arrow is optional for the separate measurement-storage investigation.

Public source downloads do not require credentials. Authorised embargoed downloads and draft updates use a Zenodo token resolved by GeoLocatoR from `ZENODO_TOKEN`/`ZENODO_PAT` or the corresponding system-keyring entries. The standalone creator/relationship update helpers currently use the `ZENODO_TOKEN` keyring entry directly.

## Run the workflow

Open [`scripts/script.R`](scripts/script.R) and run it **section by section from this project root**. Choose the release version and draft record ID first. The script refreshes the source inventory, builds the public Master, exports GBIF review files and exposes intermediate objects for inspection.

Commented calls show how to reuse the inventory offline, refresh a source, build a separate local restricted copy, include measurements, run release checks and synchronise files to the Zenodo draft. Draft synchronisation is a manual step and does not publish. Supply the complete public file list: the helper removes draft files absent from that list.

## Inputs and outputs

- `data/dp/`: source packages, Zenodo metadata and small measurement-summary caches. `community_records.json` identifies the current inventory and excludes older cached versions from the routine build.
- `data/master/`: public resource tables, descriptor and source/species catalogues. Catalogues describe all contributing projects, including embargoed projects; public resource tables include only open sources.
- `data/master_restricted/`: optional local output including authorised embargoed data.
- `data/dwc/occurrence.csv` and `data/dwc/eml.xml`: public Darwin Core and EML review exports. Generation does not publish to GBIF or configure an IPT.

Source data, credentials and generated exports are not committed to Git. Raw measurements are omitted from the routine Master build; optional inclusion streams cached CSVs without retaining the combined table in memory. Measurement storage and distribution remain under review.

## Documentation and reuse

- [`docs/workflow.md`](docs/workflow.md): build steps, exported functions, catalogue columns and local caches.
- [`docs/metadata.md`](docs/metadata.md) and [`docs/master-metadata.json`](docs/master-metadata.json): fixed descriptor fields, DOI/version handling and Zenodo updates.
- [`docs/gbif.md`](docs/gbif.md): current Darwin Core mapping and publication questions.
- [`docs/measurements.md`](docs/measurements.md): local storage benchmark and optional measurement inclusion.
- GeoPressureManual: [Use the Master](https://geopressure.org/GeoPressureManual/geolocator-master.html) shows how to read a release, select birds, cite their source packages and compare species, and [Read and use](https://geopressure.org/GeoPressureManual/geolocator-read.html) covers a single package.
- [`examples/reuse_master.qmd`](examples/reuse_master.qmd): illustrated walkthrough of filtering, trajectory and altitude summaries, flight speeds and source citations. Render with `quarto render examples/reuse_master.qmd --output-dir ../output/reuse-master-vignette`.

Analysis and plotting scripts have moved to the separate GeoLocatorAnalysis project. They are outside this repository's build workflow. Package help is generated from the roxygen comments in `R/` and committed in `man/`.

## Check changes

Run these focused checks from the project root with the adjacent GeoLocatoR checkout available; the fixtures do not require downloaded source data or the taxonomy workbook:

```sh
Rscript tests/test_creators.R
Rscript tests/test_pipeline.R
```

Run the `testthat` suite against a local build or downloaded release:

```sh
Rscript tests/test_release.R
# Or: Rscript tests/test_release.R /path/to/downloaded/master output/release-audit
```

Checks cover integrity, filtering, joins, simulations, speed, altitude and round trips. Diagnostics go to `output/release-audit/`. Unknown metadata and scientific flags produce warnings and a local audit `quality.csv`; structural failures stop the release checks. Source values are retained. Fixture tests also run with `devtools::test()`; release tests require `GLDP_MASTER_DIR` or the runner above.

The creator checks make no Zenodo requests. Pipeline checks use temporary fixture files and cover streamed measurements, tag-ID collisions, chunk boundaries and summary-cache reuse. After running the main workflow, run `source("tests/check_release.R")` in the same R session to check the generated release objects and descriptor/CSV consistency.

Code is licensed under MIT. Dataset licences and source attribution are maintained in the descriptors and occurrence export.
