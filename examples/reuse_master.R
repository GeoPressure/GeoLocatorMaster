# Run from the GeoLocatorMaster root after downloading a specific Master release.
library(dplyr)
library(readr)
library(ggplot2)
library(jsonlite)

# Inspect a release and its source catalogue -------------------------------
master_dir <- "data/master"
release <- read_json(file.path(master_dir, "datapackage.json"))
projects <- read_csv(file.path(master_dir, "datapackages.csv"), show_col_types = FALSE)
tags <- read_csv(file.path(master_dir, "tags.csv"), show_col_types = FALSE)
print(projects |> select(datapackage_id, title, access_status, starts_with("numberTags_")))

# Select a species using tag metadata; only actual Master rows are included --
selected_tags <- tags |> filter(scientific_name == "Upupa epops")
print(selected_tags |> count(datapackage_id, scientific_name, name = "tags"))

# Read only the table needed for this analysis ------------------------------
# For remote use, replace this path with the edges.csv download URL of the
# pinned Zenodo release. Reading one CSV avoids loading pressurepaths or raw data.
edges <- read_csv(file.path(master_dir, "edges.csv"), show_col_types = FALSE) |>
  semi_join(selected_tags, by = "tag_id")

# Estimate total distance for each individual and each simulated trajectory --
# Sum within a simulation first: each simulation is a complete alternative path.
distances <- edges |>
  filter(type == "simulation") |>
  group_by(tag_id, j) |>
  summarise(total_distance_km = sum(distance), .groups = "drop")
distance_summary <- distances |>
  group_by(tag_id) |>
  summarise(median_km = median(total_distance_km),
            lower_km = quantile(total_distance_km, 0.025),
            upper_km = quantile(total_distance_km, 0.975), .groups = "drop") |>
  left_join(selected_tags |> select(tag_id, datapackage_id, scientific_name), by = "tag_id")
print(distance_summary)
print(ggplot(distance_summary, aes(x = reorder(tag_id, median_km), y = median_km)) +
  geom_linerange(aes(ymin = lower_km, ymax = upper_km)) + geom_point() +
  coord_flip() + labs(x = "Tag", y = "Total distance (km; median and 95% simulation interval)") +
  theme_minimal())

# Keep the release reference and identify the source datasets to cite --------
citations <- projects |> semi_join(selected_tags, by = "datapackage_id") |>
  select(datapackage_id, title, licenses, bibliographicCitation)
print(paste("Master:", release$id, "version:", release$version))
print(citations, width = Inf)

# Optional: work with a complete GeoLocatoR object --------------------------
# library(GeoLocatoR)
# master <- read_gldp(master_dir)
# selected <- select_gldp(master, selected_tags$tag_id)
# This loads the full Master; it is unnecessary for the table-based example.
# For raw measurements, retrieve the selected source packages from Zenodo.
