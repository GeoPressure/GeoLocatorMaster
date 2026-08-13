suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(rnaturalearth))

metadata <- read_csv("data/master/datapackages.csv", show_col_types = FALSE)
species_ref <- read_csv("data/master/species.csv", show_col_types = FALSE)

count_rows <- function(path) {
  if (!file.exists(path)) {
    return(NA_integer_)
  }

  wc_output <- system2("wc", c("-l", path), stdout = TRUE)
  row_count <- strsplit(trimws(wc_output), "[[:space:]]+")[[1]][1]

  as.integer(row_count) - 1L
}

format_n <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

collapse_values <- function(x, max_n = Inf) {
  x <- sort(unique(stats::na.omit(x)))

  if (!length(x)) {
    return("none")
  }

  if (length(x) > max_n) {
    shown <- paste(x[seq_len(max_n)], collapse = ", ")
    return(paste0(shown, " (+", length(x) - max_n, " more)"))
  }

  paste(x, collapse = ", ")
}

get_geographic_coverage <- function(data_dir) {
  observations <- read_csv(
    file.path(data_dir, "observations.csv"),
    show_col_types = FALSE
  ) |>
    transmute(lat = latitude, lon = longitude)

  paths <- read_csv(
    file.path(data_dir, "paths.csv"),
    show_col_types = FALSE
  ) |>
    transmute(lat = lat, lon = lon)

  points <- bind_rows(observations, paths) |>
    filter(
      !is.na(lat),
      !is.na(lon),
      between(lat, -90, 90),
      between(lon, -180, 180)
    ) |>
    distinct()

  if (!nrow(points)) {
    return(
      list(
        continents = character(),
        regions = character(),
        unmatched = 0L
      )
    )
  }

  world <- ne_countries(scale = "small", returnclass = "sf") |>
    st_make_valid() |>
    select(continent, region_un) |>
    st_transform(3857)

  point_sf <- st_as_sf(
    points,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  ) |>
    st_transform(3857)

  joined <- st_join(
    point_sf,
    world,
    join = st_intersects,
    left = TRUE
  ) |>
    st_drop_geometry()

  list(
    continents = sort(unique(stats::na.omit(joined$continent))),
    regions = sort(unique(stats::na.omit(joined$region_un))),
    unmatched = sum(is.na(joined$continent))
  )
}

get_temporal_coverage <- function(datapackages) {
  tibble(
    start = min(datapackages$temporal_start, na.rm = TRUE),
    end = max(datapackages$temporal_end, na.rm = TRUE)
  )
}

get_taxonomic_coverage <- function(data_dir) {
  tags <- read_csv(
    file.path(data_dir, "tags.csv"),
    show_col_types = FALSE
  ) |>
    filter(!is.na(scientific_name), scientific_name != "") |>
    distinct(scientific_name)

  taxa <- tags |>
    left_join(
      species_ref |>
        transmute(
          scientific_name = Scientific_name,
          order = Order,
          family = Family
        ),
      by = "scientific_name"
    )

  tibble(
    n_species = nrow(taxa),
    n_orders = n_distinct(taxa$order, na.rm = TRUE),
    n_families = n_distinct(taxa$family, na.rm = TRUE),
    species_list = collapse_values(taxa$scientific_name, max_n = 20),
    orders = collapse_values(taxa$order),
    families = collapse_values(taxa$family)
  )
}

get_dataset_size <- function(data_dir, datapackages) {
  tags <- read_csv(file.path(data_dir, "tags.csv"), show_col_types = FALSE)

  record_counts <- c(
    tags = nrow(tags),
    observations = count_rows(file.path(data_dir, "observations.csv")),
    measurements = count_rows(file.path(data_dir, "measurements.csv")),
    paths = count_rows(file.path(data_dir, "paths.csv")),
    pressurepaths = count_rows(file.path(data_dir, "pressurepaths.csv")),
    twilights = count_rows(file.path(data_dir, "twilights.csv")),
    staps = count_rows(file.path(data_dir, "staps.csv")),
    edges = count_rows(file.path(data_dir, "edges.csv"))
  )

  tibble(
    n_datapackages = nrow(datapackages),
    n_tags = record_counts[["tags"]],
    n_individuals = n_distinct(tags$ring_number, na.rm = TRUE),
    n_records_total = sum(record_counts, na.rm = TRUE),
    n_observations = record_counts[["observations"]],
    n_measurements = record_counts[["measurements"]],
    n_paths = record_counts[["paths"]],
    n_pressurepaths = record_counts[["pressurepaths"]],
    n_twilights = record_counts[["twilights"]],
    n_staps = record_counts[["staps"]],
    n_edges = record_counts[["edges"]]
  )
}

print_summary <- function(label, data_dir, datapackages) {
  temporal <- get_temporal_coverage(datapackages)
  taxonomic <- get_taxonomic_coverage(data_dir)
  geographic <- get_geographic_coverage(data_dir)
  dataset_size <- get_dataset_size(data_dir, datapackages)

  cat("\n", label, "\n", sep = "")
  cat(strrep("=", nchar(label)), "\n", sep = "")

  cat(
    "* Datapackages: ", format_n(dataset_size$n_datapackages), "\n",
    "* Geographic coverage (continents): ",
    collapse_values(geographic$continents), "\n",
    "* Geographic coverage (regions): ",
    collapse_values(geographic$regions), "\n",
    "* Geographic coverage (unmatched offshore points): ",
    format_n(geographic$unmatched), "\n",
    "* Temporal coverage: ",
    format(temporal$start), " to ", format(temporal$end), "\n",
    "* Taxonomic coverage: ",
    format_n(taxonomic$n_species), " species across ",
    format_n(taxonomic$n_orders), " orders and ",
    format_n(taxonomic$n_families), " families\n",
    "* Species: ", taxonomic$species_list, "\n",
    "* Orders: ", taxonomic$orders, "\n",
    "* Families: ", taxonomic$families, "\n",
    "* Dataset size: ",
    format_n(dataset_size$n_tags), " tags / ",
    format_n(dataset_size$n_individuals), " individuals / ",
    format_n(dataset_size$n_records_total), " total records\n",
    "* Records by table: observations=", format_n(dataset_size$n_observations),
    ", measurements=", format_n(dataset_size$n_measurements),
    ", paths=", format_n(dataset_size$n_paths),
    ", pressurepaths=", format_n(dataset_size$n_pressurepaths),
    ", twilights=", format_n(dataset_size$n_twilights),
    ", staps=", format_n(dataset_size$n_staps),
    ", edges=", format_n(dataset_size$n_edges), "\n",
    sep = ""
  )
}

print_summary(
  label = "Open datapackages",
  data_dir = "data/master",
  datapackages = filter(metadata, access_status == "open")
)

print_summary(
  label = "All datapackages (including embargoed)",
  data_dir = "data/master_restricted",
  datapackages = metadata
)
