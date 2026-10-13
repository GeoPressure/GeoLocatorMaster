# Fixed metadata

`master-metadata.json` contains the Master title, concept DOI, code repository, licence and record type. `write_master()` reads these fields when writing the descriptor; edit the file rather than the R code. They are required to describe the package but do not need to be redefined in each build. A different descriptor can be supplied with the `metadata` argument. Restricted copies omit the public DOI and concept DOI.

The Master concept DOI is [10.5281/zenodo.18187092](https://doi.org/10.5281/zenodo.18187092). The existing revision draft is [Zenodo draft 18187093](https://zenodo.org/uploads/18187093); the release under preparation is `2026.0-beta`.

Fixed Zenodo fields such as description, keywords, community membership and publisher information stay on the existing record. `update_master_draft()` only updates changing version, creators, source DOI relationships and files. It does not publish. Optional `audit_dir` saves before/after snapshots outside the normal outputs.

GBIF publisher/institution codes, dataset description, contact details, EML and IPT configuration belong in the publication setup, not the CSV build script. These have not been agreed or invented here. See [gbif.md](gbif.md) for the open questions. Source DOI references and licences vary between packages, so the script computes these for each occurrence.

`update_master_draft()` resolves Zenodo credentials through GeoLocatoR from the environment or system keyring. The standalone `update_creators()` and `update_related_identifiers()` helpers use the `ZENODO_TOKEN` keyring entry directly. Credentials are not stored in this repository.

Creator updates preserve accented names. Seven known full-name-only source entries have explicit given/family-name corrections, including `Frédéric Jiguet`, `Paul Doniol-Valcroze` and `Sophie de Grissac`; other ambiguous names are not split automatically. Unambiguous duplicates with and without ORCIDs merge while preserving affiliations; conflicting ORCIDs stay separate. Personal creators require a family name, so source metadata should supply `givenName` and `familyName` for new entries.

Both metadata update paths send registered licences as their IDs only, excluding the display fields returned by Zenodo. The Master uses `compiles` relationships to its source datasets. Draft updates do not publish the record.
