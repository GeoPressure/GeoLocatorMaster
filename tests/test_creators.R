# Run from the project root; no Zenodo requests are made.
if (file.exists("R/update_creators.R")) {
  source("R/update_creators.R")
} else {
  get_creators_from_pkgs <- GeoLocatorMaster:::get_creators_from_pkgs
  merge_creators <- GeoLocatorMaster:::merge_creators
  creator_orcid <- GeoLocatorMaster:::creator_orcid
}

# Correct full names without guessing where compound surnames begin.
creators <- get_creators_from_pkgs(list(contributors = list(
  list(title = "Frédéric Jiguet"), list(title = "Sophie de Grissac"))))
stopifnot(identical(creators[[1]]$person_or_org$given_name, "Frédéric"),
          identical(creators[[1]]$person_or_org$family_name, "Jiguet"),
          identical(creators[[2]]$person_or_org$family_name, "de Grissac"))
stopifnot(identical(creators, jsonlite::fromJSON(
  jsonlite::toJSON(creators, auto_unbox = TRUE), simplifyVector = FALSE)))

# Keep the ORCID and combined affiliations regardless of input order.
creators <- get_creators_from_pkgs(list(contributors = list(
  list(title = "Bauer, Silke", givenName = "Silke", familyName = "Bauer", organization = "Institute A"),
  list(title = "Bauer, Silke", givenName = "Silke", familyName = "Bauer",
       path = "https://orcid.org/0000-0002-0844-164X", organization = "Institute B"))))
for (input in list(creators, rev(creators))) {
  merged <- merge_creators(input)$creators
  stopifnot(length(merged) == 1,
            identical(creator_orcid(merged[[1]]), "0000-0002-0844-164X"),
            setequal(vapply(merged[[1]]$affiliations, function(x) x$name, character(1)),
                     c("Institute A", "Institute B")))
}

# Equal names with conflicting ORCIDs must remain separate.
creators[[1]]$person_or_org$identifiers <- list(list(scheme = "orcid", identifier = "0000-0002-9434-9056"))
stopifnot(length(merge_creators(creators)$creators) == 2)
cat("Creator correction, UTF-8 and identity checks passed.\n")
