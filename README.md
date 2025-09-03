MetaLab
================

# MetaLab <img alt="USYD Meta Lab" src="https://usyd-meta-lab.github.io/files/usyd.ico" height="28" align="right"/>

Utility functions for the USYD **Meta Lab** workflow — from running
jsPsych studies with DataPipe to pulling, cleaning, and reporting data
from **OSF**.

- Streamlined data pulls from OSF components
- JSON → tidy data helpers for jsPsych exports
- Quick, reproducible reporting utilities (tables/figures)

> Install from GitHub with
> `devtools::install_github("usyd-meta-lab/MetaLab")`.

------------------------------------------------------------------------

## Installation

``` r
install.packages("devtools")
devtools::install_github("usyd-meta-lab/MetaLab")
```

If you’re on a fresh machine, you may need system build tools (Rtools on
Windows / Xcode CLT on macOS).

------------------------------------------------------------------------

## Quick start

### 1) Download all CSV data from an OSF project

Use this after setting up DataPipe → OSF. It grabs all CSVs in a
component (or project) and binds them into a single file.

``` r
library(MetaLab)

# Pull and combine all CSVs from an OSF project to a local Data/ folder
combine_osf_csvs(
  osf_project_id = "YOUR_OSF_ID",  # e.g., "xyztnu"
  folder = "Data"
)
```

### 2) Tidy jsPsych JSON logs

Turn nested JSON blobs (e.g., embedded in a `json` column or saved as
files) into analysis-ready long tables.

``` r
# Example: convert a JSON column to long form
long <- json_to_long(
  data = df,              # a data.frame with a JSON column
  json_col = "json",      # column containing per-trial JSON
  id_cols = c("pid","timepoint")
)
```

### 3) Publication-ready correlation table (APA-style)

``` r
create_correlation_table(
  data = df,
  vars  = c("accuracy", "confidence", "rt"),
  digits = 2,
  method = "pearson",
  caption = "Correlations among key task variables"
)
```

------------------------------------------------------------------------

## Typical lab workflow

1)  **Host** a jsPsych study on GitHub Pages and save data with
    **DataPipe** → **OSF** (see the lab guide).  
2)  **Pull** all OSF CSVs with `combine_osf_csvs()` into a local `Data/`
    directory for analysis.  
3)  **Tidy** raw logs with `json_to_long()` and compute derived
    variables.  
4)  **Report** using `create_correlation_table()` and your standard
    plotting scripts.

------------------------------------------------------------------------

## Selected functions

- `combine_osf_csvs(osf_project_id, folder)` – Download & row-bind all
  CSVs from an OSF project/component.  
- `json_to_long(data, json_col, id_cols)` – Flatten nested jsPsych JSON
  into tidy long format.  
- `create_correlation_table(data, vars, ...)` – APA-style correlation
  table for manuscripts.

> Tip: When you’re launching a study, note your **OSF Project ID**;
> you’ll need it for `combine_osf_csvs()`.

------------------------------------------------------------------------

## Function index (auto-generated on render)

| Function                       | Title |
|--------------------------------|-------|
| `bind_csvs_from_folder`        | —     |
| `bind_csvs_from_OSF`           | —     |
| `calculate_group_descriptives` | —     |
| `compute_meta_sdt`             | —     |
| `create_correlation_table`     | —     |
| `create_MetaLab_project`       | —     |
| `find_duplicate_attempts`      | —     |
| `independent_t_test_apa`       | —     |
| `json_to_long`                 | —     |
| `plot_numeric_histograms`      | —     |
| `sample_word_pairs`            | —     |

------------------------------------------------------------------------

## Minimal example

``` r
library(MetaLab)
library(dplyr)

# 1) Pull
combine_osf_csvs("xyztnu", folder = "Data")

# 2) Read & tidy
raw <- read.csv("Data/all_data.csv")
long <- json_to_long(raw, json_col = "json", id_cols = c("participant_id"))

# 3) Quick QC
long %>%
  count(trial_type) %>%
  arrange(desc(n))

# 4) Correlations
create_correlation_table(
  data = long,
  vars = c("accuracy","confidence","rt")
)
```

------------------------------------------------------------------------

## Project structure (recommended)

    your-project/
    ├─ Data/               # created by combine_osf_csvs()
    ├─ R/                  # analysis scripts
    ├─ figures/            # exported plots
    ├─ reports/            # Rmd/Quarto reports
    └─ renv/               # optional: reproducible env

------------------------------------------------------------------------

## Contributing

- Open an issue with a minimal reproducible example (reprex).
- Use feature branches; submit a focused PR per feature/fix.
- Add `@examples` to exported functions and update `NEWS.md` if
  user-facing.

------------------------------------------------------------------------

## License

Unless otherwise stated, data you access via the MetaLab platform is
typically licensed **CC BY 4.0**; consult the source project for
specifics. Package source code license is listed in `DESCRIPTION`.

------------------------------------------------------------------------

## Links

- Lab package install snippet (website)  
- Launching studies with DataPipe, OSF, SONA/Prolific (step-by-step)
