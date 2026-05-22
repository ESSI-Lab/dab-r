# dabr examples

Runnable demos for the [GeoDAB OM-API](https://his-central.geodab.eu/gs-service/om-api/) using **HIS-Central**.

## Quick start

```bash
cd examples
cp his_central_config.json.example his_central_config.json
# edit his_central_config.json — set your token
Rscript his_central_observation_plot.R
```

Optional: pass a custom config path:

```bash
Rscript his_central_observation_plot.R /path/to/his_central_config.json
```

## Files

| File | Purpose |
|------|---------|
| `his_central_setup.R` | Loads config, installs `dabr` if needed (`github` or `local`), `library(dabr)` |
| `his_central_observation_plot.R` | End-to-end workflow: search areas, shapefile, map, observations, time series |
| `his_central_config.json.example` | Template for `his_central_config.json` (gitignored) |

## Config (`his_central_config.json`)

Only **credentials and install** belong in the config. Search parameters (search area label, dates, limits) are at the top of the plot script so you can experiment without touching JSON.

| Field | Required | Description |
|-------|----------|-------------|
| `token` | yes | GeoDAB / HIS-Central API token |
| `install.source` | yes | `"github"` or `"local"` |
| `install.github_repo` | no | Default `ESSI-Lab/dab-r` |
| `install.github_ref` | no | Branch, tag, or commit (empty = default branch) |
| `install.force` | no | Reinstall even if `dabr` is already installed (default `false`) |

**`install.source`**

- **`github`** — `remotes::install_github()` (typical for developers who only clone this repo for the example).
- **`local`** — `remotes::install_local()` from the package root; use when developing `dabr` in this repository.

## Already have `dabr` installed?

Skip setup and work interactively:

```r
library(dabr)
library(ggplot2)

token <- "your-token"
client <- HISCentralClient(token = token)

areas <- client$get_properties(PREDEFINED_SEARCH_AREA, limit = 10)
areas$fetch_all_pages()
areas$print_values()

constraints <- Constraints(
  predefinedSearchArea = areas[[1]]$value,
  beginPosition = "2025-04-01T00:00:00Z",
  endPosition = "2025-04-30T23:59:59Z"
)

obs <- client$get_observations(constraints)
obs$fetch_all_pages()
obs$to_df_all()
```

Or source only the installer helper:

```r
source("his_central_setup.R")
cfg <- init_dabr_example("his_central_config.json")
```

## What `his_central_observation_plot.R` does

1. **Predefined search areas** — `get_properties(PREDEFINED_SEARCH_AREA)` with `fetch_all_pages()`.
2. **Shapefile download** — `download_observations()` with `format = "SHAPEFILE"` and `includeData = FALSE`.
3. **Map** — reads the shapefile with **sf**, basemap with **rosm** (optional if tiles fail).
4. **Observation list** — `get_observations()` + pagination, prints `to_df_all()`.
5. **Time series** — `get_observation_with_data()` for the first observation, **ggplot2** line plot.

Edit the **Parameters** block in the script (search area label, date range, pagination limits). Optional `Constraints()` fields are documented in comments there.

## Extra R packages

The setup script installs **ggplot2** when needed. The plot script also uses **sf** and **rosm** for the map step (installed on demand via `ensure_cran_pkg()` in `his_central_setup.R`).
