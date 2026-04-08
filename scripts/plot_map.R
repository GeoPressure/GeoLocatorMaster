library(dplyr)
library(ggplot2)
library(sf)
library(GeoPressureR)

# --- load aggregated datapackage ---
# pkgs <- read_gldp("data/master_restricted/datapackage.json")

# --- derive paths + stap metadata ---
p <- paths(pkgs) %>%
  filter(.data$type == "most_likely") %>%
  left_join(staps(pkgs), by = c("stap_id", "tag_id")) %>%
  left_join(
    tags(pkgs) %>% select(tag_id, scientific_name),
    by = "tag_id"
  )

p$duration <- GeoPressureR::stap2duration(p)

# Keep only valid coordinates for plotting
p <- p %>% filter(!is.na(.data$lat) & !is.na(.data$lon))

# --- color palette per species ---
species <- unique(p$scientific_name)
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

# --- world basemap (plane / 2D) ---
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# --- styling helpers ---
scale_radius <- function(x) {
  x <- log1p(x)
  min_r <- 0.1
  max_r <- 2
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  if (!is.finite(min_x) || !is.finite(max_x) || max_x == min_x) {
    return(rep(mean(c(min_r, max_r)), length(x)))
  }
  (x - min_x) / (max_x - min_x) * (max_r - min_r) + min_r
}

p$radius <- scale_radius(p$duration)

# --- plot ---
map_plot <- ggplot() +
  geom_sf(
    data = world,
    fill = "#1A1F2C",
    color = "#2C3445",
    linewidth = 0.3
  ) +
  geom_path(
    data = p,
    aes(x = lon, y = lat, group = interaction(tag_id), color = scientific_name),
    linewidth = 0.5,
    alpha = 0.3,
    lineend = "round"
  ) +
  geom_point(
    data = p,
    aes(x = lon, y = lat, color = scientific_name, size = radius),
    alpha = 0.4
  ) +
  scale_color_manual(values = color_palette) +
  scale_size_identity() +
  coord_sf(
    expand = FALSE,
    ylim = c(-60, 82)
  ) +
  guides(color = guide_legend(nrow = 4, byrow = TRUE, title.position = "top")) +
  labs(
    title = "Bird migration trajectories",
    subtitle = paste0(
      "Most-likely paths from multi-sensor geoloctor • ",
      "Date: ",
      format(Sys.Date(), "%Y-%m-%d"),
      " • ",
      "Tags: ",
      dplyr::n_distinct(p$tag_id),
      " • ",
      "Species: ",
      dplyr::n_distinct(p$scientific_name)
    ),
    color = "Species"
  ) +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.background = element_rect(fill = "#0D111C", color = NA),
    panel.background = element_rect(fill = "#0D111C", color = NA),
    plot.margin = margin(14, 14, 14, 14),
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

map_plot

ggsave(
  "figures/map.eps",
  map_plot,
  device = grDevices::cairo_ps,
  bg = "#0D111C",
  width = 7.5,
  height = 7.5 * 9 / 16,
  units = "in"
)
