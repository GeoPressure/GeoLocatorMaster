# GeoLocatorMaster 0.0.0.9000

- Consolidate source refresh, public Master generation and GBIF review exports in `scripts/script.R`; move analysis scripts to the separate GeoLocatorAnalysis project.
- Cache Zenodo files directly, retain a current source inventory and omit raw measurements from in-memory source objects.
- Add source catalogues, sampling-interval caches and optional streamed measurement export.
- Write the species table as a custom `species` table resource, restored by `read_gldp()`, instead of a top-level `species` property.
- Add `contributor_roles` to the source catalogue, listing each contributor with their Zenodo roles (e.g. the contact person).
- Add `write_master()` and `update_master_draft()` for consistent descriptors and manual draft synchronisation.
- Correct seven known name-only creators, preserve accents and compound surnames, and merge unambiguous ORCID/non-ORCID duplicates.
- Send registered Zenodo licences by ID and relate the Master to the open source versions it compiles with `IsCompiledBy`, without copying the sources' own related identifiers.
- Document setup, package functions, reuse examples and the current GBIF review decisions; add focused creator and pipeline checks.
