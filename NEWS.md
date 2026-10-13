# GeoLocatorMaster 0.0.0.9000

- Consolidate source refresh, public Master generation and GBIF review exports in `scripts/script.R`; move analysis scripts to the separate GeoLocatorAnalysis project.
- Cache Zenodo files directly, retain a current source inventory and omit raw measurements from in-memory source objects.
- Add source catalogues, sampling-interval caches and optional streamed measurement export.
- Add `write_master()` and `update_master_draft()` for consistent descriptors and manual draft synchronisation.
- Correct seven known name-only creators, preserve accents and compound surnames, and merge unambiguous ORCID/non-ORCID duplicates.
- Send registered Zenodo licences by ID and use `compiles` relationships from the Master to its sources.
- Document setup, package functions, reuse examples and the current GBIF review decisions; add focused creator and pipeline checks.
