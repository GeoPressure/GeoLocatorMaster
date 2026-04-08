library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(GeoLocatoR)

# --- load aggregated datapackage ---
if (!exists("pkgs")) {
  pkgs <- GeoLocatoR::read_gldp("data/master_restricted/datapackage.json")
}

# --- metadata ---
tag_species <- tags(pkgs) %>%
  dplyr::select(tag_id, scientific_name) %>%
  dplyr::mutate(scientific_name = stringr::str_squish(scientific_name)) %>%
  dplyr::filter(!is.na(tag_id), !is.na(scientific_name), scientific_name != "")

# --- unique tags with measurements per year ---
tag_year <- measurements(pkgs) %>%
  dplyr::select(tag_id, datetime) %>%
  dplyr::filter(!is.na(tag_id), !is.na(datetime)) %>%
  dplyr::mutate(year = lubridate::year(datetime)) %>%
  dplyr::filter(!is.na(year)) %>%
  dplyr::distinct(year, tag_id)

tag_year_species <- tag_year %>%
  dplyr::left_join(tag_species, by = "tag_id") %>%
  dplyr::filter(!is.na(scientific_name))

year_species_counts <- tag_year_species %>%
  dplyr::count(year, scientific_name, name = "n_tags") %>%
  dplyr::arrange(year, scientific_name)

if (nrow(year_species_counts) == 0L) {
  stop("No measurement records with valid year and species were found.")
}

# --- same species palette logic as plot_map.R ---
species <- unique(year_species_counts$scientific_name)
species <- species[!is.na(species)]
n_species <- length(species)

base_colors <- c(
  "#E41A1C",
  "#377EB8",
  "#4DAF4A",
  "#984EA3",
  "#FF7F00",
  "#FFFF33",
  "#A65628",
  "#F781BF",
  "#999999",
  "#66C2A5",
  "#FC8D62",
  "#8DA0CB",
  "#E78AC3",
  "#A6D854",
  "#FFD92F",
  "#E5C494",
  "#B3B3B3"
)

palette_fun <- grDevices::colorRampPalette(base_colors)
color_palette <- palette_fun(max(n_species, length(base_colors)))
names(color_palette) <- species

# --- plot ---
species_year_plot <- ggplot(
  year_species_counts,
  aes(x = factor(year), y = n_tags, fill = scientific_name)
) +
  geom_col(width = 0.82) +
  scale_fill_manual(values = color_palette) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE, title.position = "top")) +
  labs(
    title = "Tags with Measurements per Year",
    subtitle = paste0(
      "Unique tag_id counted per year from measurements(pkgs) - Total tags: ",
      dplyr::n_distinct(tag_year_species$tag_id),
      " - Species: ",
      dplyr::n_distinct(tag_year_species$scientific_name)
    ),
    x = "Year",
    y = "Number of tags",
    fill = "Species"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.background = element_rect(fill = "#0D111C", color = NA),
    panel.background = element_rect(fill = "#0D111C", color = NA),
    plot.margin = margin(14, 14, 14, 14),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#2C3445", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.title = element_text(color = "#E8EDF5"),
    axis.text = element_text(color = "#C7D1DD"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "#2C3445"),
    plot.title = element_text(color = "#E8EDF5", face = "bold", size = 12),
    plot.subtitle = element_text(
      color = "#AAB6C5",
      size = 8,
      margin = margin(b = 6)
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.justification = "center",
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.32, "cm"),
    legend.spacing.x = unit(0.2, "cm"),
    legend.title.align = 0.5,
    legend.background = element_rect(fill = "#0D111C", color = NA),
    legend.box.background = element_rect(fill = "#0D111C", color = NA),
    legend.margin = margin(6, 12, 6, 12),
    legend.key = element_rect(fill = "#0D111C", color = NA),
    legend.text = element_text(color = "#C7D1DD", size = 8),
    legend.title = element_text(color = "#E8EDF5")
  )

print(species_year_plot)

ggsave(
  filename = "figures/species_year.png",
  plot = species_year_plot,
  bg = "#0D111C",
  width = 9,
  height = 5,
  units = "in",
  dpi = 300
)
