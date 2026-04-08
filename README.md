# GeoLocatorAggregator

This project provides a set of Python scripts to download, aggregate, and analyze geolocator data from the [Zenodo `geolocator-dp` community](https://zenodo.org/communities/geolocator-dp/). It also includes R scripts for plotting the aggregated data.

## Installation

This project uses `uv` for managing the Python environment.

```bash
uv pip install .
```

## Usage

The data processing is a two-step process:

### Step 1: Download Data from Zenodo

The `download_zenodo_data.py` script downloads data packages from the `geolocator-dp` community on Zenodo.

By default, it downloads all available data packages.

```bash
python download_zenodo_data.py
```

To download specific data packages, provide their record IDs as arguments:

```bash
python download_zenodo_data.py --records 14641780 14883861
```

**Note on Zenodo API Token:**
For frequent use, it is recommended to set a Zenodo personal access token. The script will automatically use a token named `ZENODO_PAT` from your system's keyring. This will help avoid rate limits.

### Step 2: Combine Data

After downloading the data, run the `combine_data.py` script to aggregate the individual data packages into a single master dataset.

```bash
python combine_data.py
```

This will create a set of master CSV files in the `data/master/` directory.

## Plotting

The `scripts/` directory contains R scripts for plotting the aggregated data. These scripts read from the `data/master/` directory.

### R Dependencies

Before running the R scripts, you need to install the following R packages:

```R
install.packages(c("dplyr", "ggplot2", "lubridate", "GeoLocatoR", "GeoPressureR", "readr", "MASS"))
```

### Running the Scripts

To generate the plots, you can run the R scripts from the command line:

```bash
Rscript scripts/plot_flight_duration_hist.R
Rscript scripts/plot_stay_move.R
```

The plots will be saved to the project's root directory.

## R: Downloading Zenodo data packages

In addition to the Python downloader, there is an R function using **zen4R**:

```r
source("R/download_zenodo_geolocator_dp.R")

# Download all latest-version records from the community
idx <- download_geolocator_dp(out_dir = "data/dp")

# Or download specific record ids
idx_one <- download_geolocator_dp(record_ids = c(14641780, 14883861), out_dir = "data/dp")
```

What it does:

- Downloads only these resources (if present):
  `datapackage.json`, `tags.csv`, `staps.csv`, `paths.csv`, `pressurepaths.csv`, `edges.csv`
- Skips `measurements.csv` (too large)
- Writes `zenodo_record.json` and `zenodo_metadata.json` in each record folder (includes creators, grants/funding, etc.)
- Writes an index file `data/dp/records_index.csv` for quick inspection
