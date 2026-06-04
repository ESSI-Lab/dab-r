# dab-r

R bindings for [GeoDAB](https://www.geodab.eu/) Data Access Bridge (DAB), focused on the **OM-JSON API** (WHOS and HIS-Central) and the **Terms API**.

## Installation

Install from GitHub with [remotes](https://cran.r-project.org/package=remotes):

```r
install.packages("remotes")
remotes::install_github("ESSI-Lab/dab-r")
```

Dependencies (`httr2`, `jsonlite`, `R6`) are installed automatically. For time-series plots, install `ggplot2` separately (optional).

## DAB Terms API

```r
library(dab.r)

api <- TermsAPI(token = "my-token", view = "blue-cloud-terms")
terms <- api$get_terms(type = "instrument", max = 10)
```

## DAB OM-API (features, observations, downloads)

### WHOS or HIS-Central

```r
library(dab.r)

# WHOS
client <- WHOSClient(token = "my-token")

# HIS-Central (supports asynchronous downloads)
# client <- HISCentralClient(token = "my-token")

# Generic preproduction endpoint
# client <- DABClient(token = "my-token", view = "whos")

# List predefined search areas, then query by area + time
areas <- client$get_properties(PREDEFINED_SEARCH_AREA, limit = 10)
areas$print_values()
constraints <- Constraints(
  predefinedSearchArea = areas$get_item(1)$value,
  beginPosition = "2026-04-01T00:00:00Z",
  endPosition = "2026-04-30T23:59:59Z"
)

# Observation geometries as shapefile (ZIP)
shape <- client$download_observations(Constraints(
  predefinedSearchArea = areas$get_item(1)$value,
  beginPosition = "2026-04-01T00:00:00Z",
  endPosition = "2026-04-30T23:59:59Z",
  format = "SHAPEFILE",
  includeData = FALSE
))
# shape$shp_path — read with sf::st_read(); example script plots on OpenStreetMap

features <- client$get_features(constraints)
features_df <- features$to_df()

# Pagination (Python: features.next() → R: features$next_page())
# features$next_page()

observations <- client$get_observations(constraints)
observations_df <- observations$to_df()

obs_with_data <- client$get_observation_with_data(
  observations[[1]]$id,
  begin = "2025-01-01T00:00:00Z",
  end = "2025-02-01T00:00:00Z"
)
points_df <- points_to_df(obs_with_data)

# Optional plot (requires ggplot2)
# client$plot_observation(obs_with_data, title = "Example time series")
```

### Asynchronous downloads (HIS-Central)

```r
download_constraints <- DownloadConstraints(
  bbox = c(41.777, 12.392, 41.832, 12.456),
  asynchDownloadName = "download_example"
)

download <- client$create_download(download_constraints)
status <- client$get_download_status(download$id)
status$to_df()

# Full workflow: submit → poll → save file
# path <- client$create_save_download(download_constraints, filename = "data.zip")

# client$delete_download(download$id)
```

## API reference

| Python (`dabpy`) | R (`dab.r`) |
|------------------|------------|
| `TermsAPI(token, view)` | `TermsAPI(token, view)` |
| `DABClient` / `WHOSClient` / `HISCentralClient` | `DABClient()` / `WHOSClient()` / `HISCentralClient()` |
| `Constraints(...)` | `Constraints(...)` |
| `DownloadConstraints(...)` | `DownloadConstraints(...)` |
| `constraints.to_query()` | `constraints_to_query(constraints)` |
| `client.get_features()` | `client$get_features()` |
| `collection[0]` | `collection[[1]]` or `collection$get_item(1)` |
| `collection.next()` | `collection$next_page()` |
| `collection.to_df()` | `collection$to_df()` |
| `client.plot_observation()` | `client$plot_observation()` |

## Documentation

- OM-API (preproduction): https://gs-service-preproduction.geodab.eu/gs-service/om-api/
- WHOS: https://whos.geodab.eu/gs-service/om-api
- HIS-Central: https://his-central.geodab.eu/gs-service/om-api/
- Python tutorial notebook: https://github.com/ESSI-Lab/dab-pynb

## Example script

End-to-end HIS-Central demo: predefined search area, shapefile map, observation
list (paginated), and a time-series plot for the previous calendar month.

```bash
cp examples/his_central_config.json.example examples/his_central_config.json
# edit his_central_config.json — token and install.source (github or local)
Rscript examples/his_central_observation_plot.R
```

- **Config** (`examples/his_central_config.json`, gitignored): token and install
  only — see [`examples/his_central_config.json.example`](examples/his_central_config.json.example).
- **Workflow** (layer, dates, limits): edit the Parameters block in
  [`examples/his_central_observation_plot.R`](examples/his_central_observation_plot.R).
- **Setup helper**: [`examples/his_central_setup.R`](examples/his_central_setup.R)
  installs/loads `dab.r`; reuse with `init_dab.r_example()` in your own scripts.
- **Full guide**: [`examples/README.md`](examples/README.md).

## License

AGPL-3.0 — see [LICENSE](LICENSE).
