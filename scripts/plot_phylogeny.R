library(dplyr)
library(clootl)
library(ape)
library(stringr)
library(ggplot2)
library(ggtree)
# --- load aggregated datapackage ---
pkgs <- read_gldp("data/master_restricted/datapackage.json")


# --- species list + labels ---
tracks_by_tag <- paths(pkgs) %>%
  filter(.data$type == "most_likely") %>%
  distinct(tag_id) %>%
  mutate(has_track = TRUE)

species_meta <- tags(pkgs) %>%
  dplyr::select(tidyselect::any_of(c("tag_id", "scientific_name"))) %>%
  left_join(tracks_by_tag, by = "tag_id") %>%
  filter(.data$has_track %in% TRUE, !is.na(scientific_name)) %>%
  mutate(
    scientific_name = stringr::str_squish(scientific_name),
    sci_binom = stringr::str_replace(
      scientific_name,
      "^([A-Za-z]+\\s+[A-Za-z]+).*$",
      "\\1"
    )
  ) %>%
  group_by(sci_binom) %>%
  summarise(
    n_tracks = dplyr::n_distinct(tag_id),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(sci_binom, " (n=", n_tracks, ")")
  )

species_names <- species_meta$sci_binom

if (length(species_names) > 1L) {
  phylo_tree <- clootl::extractTree(
    species = species_names,
    label_type = "scientific"
  )

  label_map <- setNames(species_meta$label, species_meta$sci_binom)
  if (
    is.null(phylo_tree$edge.length) || any(!is.finite(phylo_tree$edge.length))
  ) {
    phylo_tree$edge.length <- rep(1, nrow(phylo_tree$edge))
  }

  tip_data <- tibble(
    name = phylo_tree$tip.label,
    tip_label = ifelse(
      is.na(label_map[phylo_tree$tip.label]),
      phylo_tree$tip.label,
      label_map[phylo_tree$tip.label]
    )
  )

  phylo_plot <- ggtree(phylo_tree, layout = "circular") %<+%
    tip_data +
    geom_tree(color = "#C7D1DD", linewidth = 0.4) +
    geom_tiplab(aes(label = tip_label), size = 2.4, color = "#E8EDF5") +
    labs(
      title = "Phylogeny of tracked species",
      subtitle = paste0("Date: ", format(Sys.Date(), "%Y-%m-%d"))
    ) +
    theme_void(base_family = "Helvetica") +
    theme(
      plot.background = element_rect(fill = "#0D111C", color = NA),
      panel.background = element_rect(fill = "#0D111C", color = NA),
      plot.title = element_text(color = "#E8EDF5", face = "bold", size = 16),
      plot.subtitle = element_text(color = "#AAB6C5", size = 11),
      legend.position = "none"
    )

  phylo_plot
} else {
  message("Not enough species for a phylogeny plot.")
}
