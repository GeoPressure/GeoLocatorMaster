# Fixed metadata

`master-metadata.json` contains the Master title, concept DOI, code repository, licence and record type. `write_master()` reads these fields when writing the descriptor; edit the file rather than the R code. They are required to describe the package but do not need to be redefined in each build. A different descriptor can be supplied with the `metadata` argument. Restricted copies omit the public DOI and concept DOI.

The Master concept DOI is [10.5281/zenodo.18187092](https://doi.org/10.5281/zenodo.18187092). The existing revision draft is [Zenodo draft 18187093](https://zenodo.org/uploads/18187093); the release under preparation is `2026.0`.

Use the two DOIs for different purposes:

- **Version DOI** (`id`, e.g. 10.5281/zenodo.18187093 for `2026.0`): for citation, for reading a release (`read_zenodo("18187093")`) and wherever results must be traced to fixed data. This applies to the Master and to each source package (`datapackage_id`).
- **Concept DOI** (`conceptid`, 10.5281/zenodo.18187092 for the Master): wherever a link must hold across versions, such as "get the latest release" links, and to identify a source package independently of its version (`datapackages$conceptid`), which avoids counting two versions of the same package twice.

Fixed Zenodo fields such as description, keywords, community membership and publisher information stay on the existing record. `update_master_draft()` only updates changing version, creators, source DOI relationships and files. It does not publish. Optional `audit_dir` saves before/after snapshots outside the normal outputs.

GBIF publisher/institution codes, dataset description, contact details, EML and IPT configuration belong in the publication setup, not the CSV build script. These have not been agreed or invented here. See [gbif.md](gbif.md) for the open questions. Source DOI references and licences vary between packages, so the script computes these for each occurrence.

`update_master_draft()` resolves Zenodo credentials through GeoLocatoR from the environment or system keyring. The standalone `update_creators()` and `update_related_identifiers()` helpers use the `ZENODO_TOKEN` keyring entry directly. Credentials are not stored in this repository.

Creator updates preserve accented names. Seven known full-name-only source entries have explicit given/family-name corrections, including `Frédéric Jiguet`, `Paul Doniol-Valcroze` and `Sophie de Grissac`; other ambiguous names are not split automatically. Duplicate contributors are merged once, upstream, by `GeoLocatoR::merge_gldp()`: entries sharing an ORCID, or an ORCID-less entry whose name matches a single ORCID, become one person with all roles and affiliations; conflicting ORCIDs stay separate. The creator update only converts and orders them. Each source package's own contributor list stays in `datapackages$contributors`. Personal creators require a family name, so source metadata should supply `givenName` and `familyName` for new entries.

Both metadata update paths send registered licences as their IDs only, excluding the display fields returned by Zenodo. The Master uses `compiles` relationships to its source datasets. Draft updates do not publish the record.
