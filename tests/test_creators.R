# Run from the project root; no Zenodo requests are made.
if (file.exists("R/update_creators.R")) {
  source("R/update_creators.R")
} else {
  get_creators_from_pkgs <- GeoLocatorMaster:::get_creators_from_pkgs
  order_creators <- GeoLocatorMaster:::order_creators
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

# Identity merging happens in GeoLocatoR::merge_gldp(); here creators are only
# ordered, with the maintainer first.
creators <- get_creators_from_pkgs(list(contributors = list(
  list(title = "Bauer, Silke", givenName = "Silke", familyName = "Bauer"),
  list(title = "Nussbaumer, Raphaël", givenName = "Raphaël", familyName = "Nussbaumer",
       path = "https://orcid.org/0000-0002-8185-1020"))))
ordered <- order_creators(rev(creators))$creators
stopifnot(length(ordered) == 2, identical(creator_orcid(ordered[[1]]), "0000-0002-8185-1020"))
cat("Creator correction, UTF-8 and ordering checks passed.\n")
