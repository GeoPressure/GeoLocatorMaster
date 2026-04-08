library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(GeoLocatoR)

# --- load aggregated datapackage ---
if (!exists("pkgs")) {
  pkgs <- GeoLocatoR::read_gldp("data/master_restricted/datapackage.json")
}

# --- keep only tags with pressure measurements ---
pressure_tags <- measurements(pkgs) %>%
  dplyr::filter(
    !is.na(tag_id),
    !is.na(sensor),
    stringr::str_to_lower(stringr::str_squish(sensor)) == "pressure"
  ) %>%
  dplyr::distinct(tag_id)

# --- metadata ---
tag_species <- tags(pkgs) %>%
  dplyr::select(tag_id, scientific_name) %>%
  dplyr::mutate(scientific_name = stringr::str_squish(scientific_name)) %>%
  dplyr::filter(!is.na(tag_id), !is.na(scientific_name), scientific_name != "") %>%
  dplyr::semi_join(pressure_tags, by = "tag_id")

obs <- observations(pkgs) %>%
  dplyr::select(tag_id, datetime, observation_type) %>%
  dplyr::filter(
    !is.na(tag_id),
    !is.na(datetime),
    !is.na(observation_type),
    observation_type == "retrieval"
  ) %>%
  dplyr::semi_join(pressure_tags, by = "tag_id") %>%
  dplyr::mutate(date = as.Date(datetime))

# --- first retrieval date per species ---
species_first <- obs %>%
  dplyr::left_join(tag_species, by = "tag_id") %>%
  dplyr::filter(!is.na(scientific_name)) %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::summarise(first_date = min(date), .groups = "drop") %>%
  dplyr::arrange(first_date, scientific_name) %>%
  dplyr::mutate(cumulative_species = dplyr::row_number())

if (nrow(species_first) == 0L) {
  stop("No species with valid observation dates were found.")
}

# --- same species palette logic as plot_map.R ---
species <- unique(species_first$scientific_name)
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

# --- cumulative species time series ---
cumulative_species <- species_first %>%
  dplyr::count(first_date, name = "new_species") %>%
  dplyr::arrange(first_date) %>%
  tidyr::complete(
    first_date = seq(min(first_date), max(first_date), by = "day"),
    fill = list(new_species = 0)
  ) %>%
  dplyr::mutate(cumulative_species = cumsum(new_species))

# --- plot ---
cum_plot <- ggplot(
  cumulative_species,
  aes(x = first_date, y = cumulative_species)
) +
  geom_step(linewidth = 1.1, color = "#C7D1DD") +
  geom_point(
    data = species_first,
    aes(
      x = first_date,
      y = cumulative_species,
      color = scientific_name
    ),
    size = 1.8,
    alpha = 0.9
  ) +
  scale_color_manual(values = color_palette) +
  guides(color = guide_legend(nrow = 4, byrow = TRUE, title.position = "top")) +
  labs(
    title = "Cumulative Number of Retrieved Species",
    subtitle = paste0(
      "From retrieval observations, restricted to pressure tags - Total species: ",
      max(cumulative_species$cumulative_species)
    ),
    x = "Date of first observation for species",
    y = "Cumulative species",
    color = "Species"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.background = element_rect(fill = "#0D111C", color = NA),
    panel.background = element_rect(fill = "#0D111C", color = NA),
    plot.margin = margin(14, 14, 14, 14),
    panel.grid.major = element_line(color = "#2C3445", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.title = element_text(color = "#E8EDF5"),
    axis.text = element_text(color = "#C7D1DD"),
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

print(cum_plot)

ggsave(
  filename = "figures/cumulative_species_retrieved.png",
  plot = cum_plot,
  bg = "#0D111C",
  width = 9,
  height = 5,
  units = "in",
  dpi = 300
)
