# Measurement storage and reuse

The release pipeline keeps measurement inclusion optional. Raw measurements can be cached locally without being loaded into the aggregation process, and an optional chunked export can include them in either Master output. This document records a storage investigation on 13 October 2026; it does not change the manuscript or the data standard. No decision has been made to adopt a new measurement format or distribution. The Parquet files are local benchmark outputs only.

## Where the size comes from

The 33 cached source packages contain 313,334,985 measurement rows occupying 12.68 GB as uncompressed CSV. The Alpine swift package contributes 193,534,681 rows for 562 tags and 7.81 GB, or 62% of all cached measurement bytes. It is currently embargoed. Open packages contribute 71,216,790 rows and 2.82 GB; the remaining embargoed packages contribute 9.86 GB including the swifts.

The other largest measurement files are Swiss Hoopoes (1.12 GB, open), Latvian Common Starlings (0.63 GB, embargoed) and Great Reed Warblers (0.56 GB, open). Adding measurements is therefore a substantial cost even without the swifts.

The swift row count is dominated by light (98.11 million rows; 50.7%), activity (37.28 million; 19.3%) and mean acceleration z (37.28 million; 19.3%). Pressure contributes 11.74 million rows. Representative sampling intervals are 2 and 5 minutes for light, 5 minutes for activity and mean acceleration z, and 15 and 30 minutes for pressure. The volume mainly reflects many individuals and long records, rather than uniformly extreme sampling rates.

Activity and mean acceleration z have the same number of rows. A comparison for tag 12GE found 98,158 shared timestamps but only 2,413 identical values. Equal row counts do not establish redundant measurements; both streams were retained. No measurements were downsampled, rounded or removed during this investigation.

## Complete swift dataset benchmark

The comparison used the complete 7.81 GB CSV, not a small extrapolated sample. `../GeoLocatorAnalysis/scripts/profile_measurements.R` writes gzip at level 6 and Parquet with zstd at level 3, partitioned by tag and sensor. Timestamp resolution was milliseconds; the swift source timestamps have whole-second resolution. The Parquet output was checked against the CSV for total row count and the aggregate measurement-value sum.

| Format | Size | Conversion time |
|---|---:|---:|
| Uncompressed CSV | 7.810 GB | — |
| Gzip CSV | 0.677 GB | 100.8 s |
| Parquet partitioned by tag and sensor | 0.980 GB | 33.8 s |

Gzip reduced storage by 91.3%; Parquet reduced it by 87.5%. Gzip was smaller in this comparison. Parquet was more convenient for selective queries: counting the 16,338 pressure measurements for tag 12GE took 28.7 s from CSV and 0.031 s from the partitioned Parquet dataset using Arrow. These are single local runs after conversion, with operating-system caching; they demonstrate selective scanning, not a general speed guarantee for every analysis or remote access.

Gzip addresses transfer and storage size, but users still need to decompress and scan a table to filter it. Partitioned Parquet lets Arrow skip other tags and sensors. Neither option requires reducing sampling resolution or changing measurement values. The current GeoLocatoR standard reader does not transparently replace a CSV resource with a Parquet dataset; adding a Parquet distribution would be an additional access route and requires documentation.

## Recommended approach

1. Keep measurement inclusion configurable and avoid materialising raw tables during the ordinary Master build. The updated pipeline does this and streams the optional CSV export in bounded chunks.
2. Encourage users to download only the source packages, sensors and individuals needed. The lightweight reuse example reads selected derived CSV tables and does not load pressurepaths or measurements.
3. Use compressed CSV for archival transfer when raw tables are included. Several existing Zenodo source records already distribute gzip CSVs, including the Swiss Hoopoe package. Compression solves much of the byte-size problem without introducing another schema.
4. Consider an optional partitioned Parquet distribution when regular raw-data queries justify maintaining it. Initially this could be project-level, especially for the swifts, with a conversion script and DOI-linked provenance. A bucket or query service is not required to obtain the local performance benefit.
5. Keep full-resolution scientific data. Any downsampling, sensor removal or rounding should be a separately documented derived product, with an explicit scientific reason.

The current public Master without raw measurements is around 1.19 GB, almost all derived pressurepaths. Adding all current open raw measurements would give roughly 4.01 GB uncompressed before minor CSV/metadata differences. The local restricted equivalent would be roughly 14.67 GB. The swift package has no standard pressurepaths table in the current aggregation, so its raw-data size does not explain the size of the Master when measurements are excluded.

A smaller download for common reuse can already be achieved by selecting individual CSV files. A future optional Parquet distribution could cover pressurepaths as well as measurements. The default Master contents and manuscript remain open to the measurement-inclusion decision.

## Reproduction and generated files

Run `Rscript scripts/profile_measurements.R` from the separate GeoLocatorAnalysis project. It requires the optional Arrow dependency and the authorised local swift CSV. Detailed sizes, timings, per-sensor counts and the additional tag comparison are stored in `../GeoLocatorAnalysis/archive/master-build-2026-10-13/measurement-investigation/`; these files and the embargoed benchmark data are ignored by Git. The main build caches sampling-interval summaries beside each source file as `*.summary.rds`, invalidated by file size or modification time.
