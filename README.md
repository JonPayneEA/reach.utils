# reach.utils

Utility functions for the [reach](https://github.com/JonPayneEA) ecosystem.
Provides shared helpers for datetime handling, configuration loading, structured
logging, file paths, quality control, and project scaffolding — designed for use
in Environment Agency hydrological pipelines.

## Installation

```r
# install.packages("remotes")
remotes::install_github("JonPayneEA/reach.utils")
```

## Overview

| Module | Functions | Purpose |
|---|---|---|
| `datetime` | `as_utc`, `parse_datetime`, `floor_to`, `snap_to_datetime`, `seq_datetime`, `water_year`, `detect_gaps`, `is_complete_series`, `format_duration` | Parse, coerce, round, and validate datetime series |
| `config` | `load_config`, `merge_configs`, `get_config_val`, `config_val_as`, `expand_config_paths`, `validate_config`, `config_from_env` | Load and query YAML/JSON configuration files |
| `logging` | `log_info`, `log_warn`, `log_error`, `log_debug`, `log_section`, `log_timed`, `log_progress`, `log_to_file` | Timestamped structured console and file logging |
| `path` | `ensure_dir`, `check_file_exists`, `resolve_path`, `find_latest_file`, `swap_ext` | File path utilities |
| `qc` | `check_bounds`, `check_duplicates`, `check_flatline`, `check_monotonic`, `check_na_runs`, `check_rate_of_change`, `qc_series` | Quality control checks for numeric time series |
| `templates` | `create_project`, `create_config`, `create_script`, `create_script_template`, `create_readme`, `create_report`, `create_function` | Project scaffolding and file templates |
| `assert` | `assert_posixct`, `assert_numeric`, `assert_scalar`, `assert_length`, `assert_same_length`, `assert_choice` | Pipeline argument guards with informative errors |
| `epoch` | `water_year_bounds`, `water_year_seq`, `split_water_years`, `complete_water_years`, `label_season` | Water-year calendars, splitting, and season labelling |

## Usage

### Datetime

```r
library(reach.utils)

# Coerce to UTC POSIXct
as_utc("2024-01-15 09:30:00")

# Floor a series to 15-minute intervals
floor_to(x, "15 minutes")

# Detect gaps in a regular series
detect_gaps(timestamps, step = "15 minutes")

# Check a series is complete with no missing steps
is_complete_series(timestamps, step = "15 minutes")

# Format an elapsed time readably
format_duration(3723)  # "1h 2m 3s"
```

### Configuration

```r
# Load a YAML config file
cfg <- load_config("config/pipeline.yml")

# Retrieve a nested value with type coercion
thresh <- get_config_val(cfg, c("qc", "max_rate_of_change"), as = "numeric")

# Merge a base config with environment-specific overrides
cfg <- merge_configs("config/base.yml", "config/prod.yml")

# Override config values from environment variables
# e.g. REACH__QC__THRESHOLD=5.0 maps to cfg$qc$threshold
cfg <- config_from_env(cfg)
```

### Logging

```r
log_info("Pipeline started for site {.val {site_id}}")
log_warn("Missing values detected: {n} gaps found")
log_error("Input file not found: {.path {path}}")

# Time a block of code
log_timed("Data import", {
  df <- read.csv("data/observations.csv")
})

# Progress through a loop
for (i in seq_along(sites)) {
  log_progress(i, length(sites))
}

# Mirror output to a file
log_to_file("logs/run.log")
```

### Paths

```r
# Create a directory if it doesn't exist
ensure_dir("outputs/plots")

# Abort with a clear error if a file is missing
check_file_exists("data/observations.csv")

# Find the most recently modified CSV in a folder
find_latest_file("outputs/", "*.csv")

# Swap a file extension
swap_ext("data/flow.csv", "parquet")  # "data/flow.parquet"
```

### Quality control

```r
# Run individual checks — each returns a data frame of flagged rows
check_bounds(values, min = 0, max = 1000)
check_duplicates(timestamps)
check_flatline(values, window = 6L)
check_rate_of_change(values, timestamps, max_rate = 50)

# Run all checks in one call
flags <- qc_series(
  values,
  timestamps,
  bounds    = c(0, 1000),
  max_rate  = 50,
  flatline  = 6L
)
```

### Templates

```r
# Scaffold a full project directory in one call
create_project(author = "Forecasting and Warning Team")
# Creates: R/, data/raw/, data/processed/, config/, outputs/, logs/,
#          tests/testthat/, .gitignore, config/pipeline.yml

# Create a starter YAML config (also called automatically by create_project)
create_config(site_id = "42001")

# Create a new R script pre-filled with the reach.io header
create_script(
  file_name = "data_import",
  file_path = "R",
  author    = "Forecasting and Warning Team",
  email     = "forecasting@environment-agency.gov.uk"
)

# Create a new function stub with a Roxygen2 skeleton
create_function(
  "aggregate_flow",
  parameters = c("x", "values", "by")
)

# Set the RStudio new-script template to the reach.io standard header
create_script_template()

# Create a README.qmd skeleton
create_readme(
  format       = "github",
  author       = "Forecasting and Warning Team",
  readme_title = "My Reach Project"
)

# Create a formal analytical report template
create_report(
  format       = "html",
  file_name    = "flood_analysis_2024",
  report_title = "Flood Frequency Analysis — River Avon at Evesham",
  author       = "Forecasting and Warning Team"
)
```

### Assertions

```r
# Guard a function entry point
assert_posixct(timestamps)
assert_numeric(values, allow_na = FALSE)
assert_scalar(threshold)
assert_same_length(timestamps, values, args = c("timestamps", "values"))
assert_choice(method, c("linear", "locf", "constant"))
```

### Epoch (water-year calendar)

```r
# Get the start/end instants of a water year
water_year_bounds(2023)
# $start  2023-10-01 00:00:00 UTC
# $end    2024-09-30 23:59:59 UTC

# Start datetimes for a range of water years
water_year_seq(2020, 2024)

# Split a series into per-water-year subsets
chunks <- split_water_years(timestamps, values)

# Which water years have complete 15-min coverage?
complete_water_years(timestamps, by = "15 mins")

# Assign EA quarter labels (Q1 Oct-Dec, Q2 Jan-Mar, Q3 Apr-Jun, Q4 Jul-Sep)
label_season(timestamps, scheme = "ea_quarter")

# Or meteorological / hydrological (wet/dry) seasons
label_season(timestamps, scheme = "meteorological")
label_season(timestamps, scheme = "hydrological")
```

## Dependencies

**Required:** `cli` (>= 3.6.0), `lubridate`, `yaml`

**Optional:** `quarto` (for `create_readme` rendering)

## License

MIT — see [LICENSE](LICENSE).
