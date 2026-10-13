# Darwin Core export

`GeoLocatoR::gldp_to_dwc()` owns the generic transformation from a GeoLocator Data Package to Darwin Core. The GBIF section of `scripts/script.R` adds the Master release DOI, source DOI references and source-specific licences. `GeoLocatoR::gldp_to_eml()` writes the matching dataset metadata, with coverage computed from the exported occurrences.

The workflow requires the adjacent GeoLocatoR development checkout described in the README. It generates `data/dwc/occurrence.csv` and `data/dwc/eml.xml` for review. It does not publish to GBIF or configure an IPT. A valid EML file alone does not establish that the occurrence mapping is ready for publication.

## Current mapping

Each stationary period in the most-likely trajectory produces one occurrence. The current converter retains `type = Event`, occurrence/event identifiers and parent-event identifiers. Raw measurements and embargoed resource rows are excluded from this export.

The occurrence export leaves `scientificNameID` blank following validator failures for Avibase identifiers. Scientific names remain available for GBIF matching, and Avibase IDs remain in the Master species catalogue. There is no automatic network lookup; the converter accepts an explicit identifier table when needed.

Each occurrence carries the Master release DOI as `datasetID`, its source release DOI as `associatedReferences`, and its source licence. Missing rights-holder metadata is not invented. Tag collisions use the source concept ID as a suffix; occurrence IDs also include ring numbers and can change following source corrections.

## Coordinate uncertainty

Known locations use the radius enclosing the analysis grid cell. `params.json` supplies `tag_set_map$scale` in pixels per degree, giving a cell width of `1 / scale` degrees. Missing scales use the ERA5 0.25-degree grid (scale 4). The radius is the spherical distance from the cell centre to its furthest corner, rounded up in metres. This approximates grid resolution rather than measured capture-site error.

Reconstructed locations use finite simulation distances from the most-likely position. With at least 100 positions, the converter uses the 95th percentile; with 1–99, it uses the maximum available distance. Both are bounded below by the grid-cell radius. Without finite simulation distances, uncertainty is blank. The 100-position threshold is a project convention, not a GBIF requirement. Simulation distances currently use a local latitude/longitude approximation, whose adequacy for widely dispersed simulations still needs review.

In the local export reviewed on 13 October 2026, 6,775 occurrences comprised 214 reconstructed records using the 95th percentile, 6,081 using the maximum, 379 known-location grid radii and 101 with unavailable uncertainty. These counts describe that build and are not fixed expectations for later releases.

## Publication questions

The reusable converter and Master publication workflow remain separate. Further review is tracked in [GeoLocatoR issue 32](https://github.com/GeoPressure/GeoLocatoR/issues/32).

Before publication, resolve:

- Observation matching, sex and EURING age representation, and optional attribution fields.
- Treatment of missing-coordinate records and stability of identifiers across corrected source releases.
- Suitability of the simulation-distance approximation.
- Publisher/institution codes, contacts, EML publication text, archive descriptors and final IPT setup.
- Whether the conversion functions remain exported in GeoLocatoR or move to GeoLocatorMaster.

Local CSV and ZIP uploads to the Swiss IPT previously failed with a generic upload error. No server-side cause has been confirmed. The occurrence CSV remains a review export.
