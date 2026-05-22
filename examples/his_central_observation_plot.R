#!/usr/bin/env Rscript
# Example: HIS-Central observation search and time-series plot
#
# Setup:
#   cp examples/his_central_config.json.example examples/his_central_config.json
#   # edit his_central_config.json (token and optional install settings)
#
# Usage:
#   Rscript examples/his_central_observation_plot.R
#   Rscript examples/his_central_observation_plot.R path/to/other_config.json
#
# Script structure:
#   1. Read config JSON (token, install options) — no dabr required yet
#   2. Install / load dabr, ggplot2, sf, rosm
#   3. Query HIS-Central (predefined layer, shapefile, observations, plot)

# --- 0. Config file path -------------------------------------------------------
script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/"))
  } else {
    normalizePath(getwd(), winslash = "/")
  }
})

default_config_path <- file.path(script_dir, "his_central_config.json")
config_path <- if (length(commandArgs(trailingOnly = TRUE)) > 0) {
  normalizePath(commandArgs(trailingOnly = TRUE)[1], winslash = "/")
} else {
  default_config_path
}

if (!file.exists(config_path)) {
  stop(
    "Config file not found: ", config_path, "\n",
    "Copy the template and set your token:\n",
    "  cp ", file.path(script_dir, "his_central_config.json.example"),
    "     ", default_config_path,
    call. = FALSE
  )
}

# Use a default when a JSON field is missing or NA (plain R; dabr is not loaded yet).
config_or <- function(value, default) {
  if (is.null(value) || (length(value) == 1L && is.na(value))) default else value
}

load_config <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite", repos = "https://cloud.r-project.org")
  }
  cfg <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  if (is.null(cfg$token)) {
    stop("Config missing required field: token", call. = FALSE)
  }
  cfg
}

config <- load_config(config_path)

token <- as.character(config$token)
if (!nzchar(token) || grepl("REPLACE_WITH", token, fixed = TRUE)) {
  stop("Set a valid GeoDAB token in ", config_path, call. = FALSE)
}

install_cfg <- config_or(config$install, list())
dab_repo <- config_or(install_cfg$github_repo, "ESSI-Lab/dab-r")
dab_ref <- config_or(install_cfg$github_ref, "")
if (!nzchar(dab_ref)) {
  dab_ref <- NA_character_
}
force_install <- isTRUE(install_cfg$force)
install_source <- config_or(install_cfg$source, "auto")

# --- 1. Install and load packages --------------------------------------------
cran <- "https://cloud.r-project.org"

find_dabr_pkg <- function() {
  for (dir in unique(normalizePath(c(
    file.path(script_dir, ".."),
    dirname(config_path),
    getwd()
  ), winslash = "/", mustWork = FALSE))) {
    if (file.exists(file.path(dir, "DESCRIPTION")) &&
        any(grepl("^Package:\\s*dabr\\s*$", readLines(
          file.path(dir, "DESCRIPTION"), n = 20, warn = FALSE
        )))) {
      return(dir)
    }
  }
  NULL
}

load_dabr <- function() {
  if ("package:dabr" %in% search()) {
    tryCatch(detach("package:dabr", unload = TRUE, character.only = TRUE), error = function(e) NULL)
  }
  if ("dabr" %in% loadedNamespaces()) {
    tryCatch(unloadNamespace("dabr"), error = function(e) NULL)
  }
  library(dabr)
}

dabr_is_usable <- function() {
  if (!requireNamespace("dabr", quietly = TRUE)) {
    return(FALSE)
  }
  ns <- asNamespace("dabr")
  if (!exists("param_get", envir = ns, inherits = FALSE)) {
    return(FALSE)
  }
  url <- dabr::HISCentralClient(token = "check")$base_url
  !grepl("/token/\\{token\\}/", url)
}

install_dabr <- function(local_dir = NULL, force = FALSE) {
  if (!is.null(local_dir)) {
    message("Installing dabr from: ", local_dir)
    remotes::install_local(local_dir, upgrade = "always", force = TRUE)
  } else {
    message("Installing dabr from GitHub: ", dab_repo)
    remotes::install_github(
      dab_repo,
      ref = if (nzchar(dab_ref)) dab_ref,
      upgrade = "always",
      force = force
    )
  }
}

ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing ", pkg, " ...")
    install.packages(pkg, repos = cran)
  }
}

# 1) remotes (needed to install dabr from GitHub or a local clone)
ensure_pkg("remotes")

local_dabr <- find_dabr_pkg()
prefer_local <- install_source == "local" ||
  (install_source == "auto" && !is.null(local_dabr))

# 2) install dabr if missing, or when config requests force = true
if (!requireNamespace("dabr", quietly = TRUE) || force_install) {
  install_dabr(local_dir = if (prefer_local) local_dabr else NULL, force = force_install)
}

# 3) load a fresh copy (needed when re-running / debugSource in the same R session)
load_dabr()

# 4) if the loaded copy is still too old, reinstall once and reload
if (!dabr_is_usable()) {
  message("dabr install looks outdated; reinstalling ...")
  install_dabr(
    local_dir = if (!is.null(local_dabr)) local_dabr else NULL,
    force = TRUE
  )
  load_dabr()
}

if (!dabr_is_usable()) {
  stop(
    "Could not load a working 'dabr' package.\n",
    "Try: remotes::install_local(\"<path-to-dab-r>\", force = TRUE)\n",
    "Or set \"force\": true under \"install\" in ", config_path,
    call. = FALSE
  )
}

ensure_pkg("ggplot2")
library(ggplot2)
message("Loaded dabr from: ", system.file(package = "dabr"))

# --- 2. Search parameters (edit here) ----------------------------------------
properties_page_limit <- 10L
max_properties_pages <- NULL
predefined_layer_label <- "Torrente Evenson"

# Temporal extent: previous calendar month (UTC, ISO 8601)
today <- Sys.Date()
first_of_this_month <- as.Date(format(today, "%Y-%m-01"))
last_of_prev_month <- first_of_this_month - 1L
first_of_prev_month <- as.Date(format(last_of_prev_month, "%Y-%m-01"))

begin_position <- paste0(format(first_of_prev_month, "%Y-%m-%d"), "T00:00:00Z")
end_position <- paste0(format(last_of_prev_month, "%Y-%m-%d"), "T23:59:59Z")
time_label <- format(first_of_prev_month, "%B %Y")

message("Using config: ", config_path)

# --- 3. HIS-Central client: list predefined layers, then search observations ---
client <- HISCentralClient(token = token)

predefined_layers <- client$get_properties(
  property = "predefinedLayer",
  limit = properties_page_limit
)
message("\nFetching all predefined search areas ...")
predefined_layers$fetch_all_pages(max_pages = max_properties_pages)
message(
  "Total predefined layers: ", length(predefined_layers),
  " (", predefined_layers$page, " page(s), completed = ",
  predefined_layers$completed, ")"
)
predefined_layers$print_values()
print(predefined_layers$to_df_all())

if (length(predefined_layers$entries) == 0) {
  stop("No predefined search areas returned by the properties API.", call. = FALSE)
}

layer_labels <- vapply(
  predefined_layers$entries,
  function(e) e$label,
  character(1)
)
layer_index <- match(predefined_layer_label, layer_labels)
if (is.na(layer_index)) {
  stop(
    "Predefined layer not found: \"", predefined_layer_label, "\".\n",
    "Available labels:\n  ",
    paste(layer_labels, collapse = "\n  "),
    call. = FALSE
  )
}

selected_layer <- predefined_layers$get_item(layer_index)

message("\nHIS-Central observation search")
message("  predefinedLayer: ", selected_layer$label)
message("  value: ", selected_layer$value)
message("  time: ", begin_position, " — ", end_position)

base_constraints <- Constraints(
  predefinedLayer = selected_layer$value,
  beginPosition = begin_position,
  endPosition = end_position
  # bbox = c(41.777, 12.392, 41.832, 12.456),  # south, west, north, east
  # observedProperty = "http://his-central-ontology.geodab.eu/hydro-ontology/concept/65",
  # ontology = "http://his-central-ontology.geodab.eu/hydro-ontology",
  # country = "IT",
  # provider = "Regione Valle d'Aosta",
  # feature = "<feature-id>",
  # localFeatureIdentifier = "<local-feature-id>",
  # observationIdentifier = "<observation-id>",
  # spatialRelation = "within",
  # timeInterpolation = "sum",
  # intendedObservationSpacing = "P1D",
  # aggregationDuration = "P1M",
  # limit = 100L,
  # format = "OM_JSON",
  # includeData = TRUE
)

# --- 3b. Download observation geometries as shapefile ---
shape_constraints <- Constraints(
  predefinedLayer = base_constraints$predefinedLayer,
  beginPosition = base_constraints$beginPosition,
  endPosition = base_constraints$endPosition,
  format = "SHAPEFILE",
  includeData = FALSE
  # bbox = c(41.777, 12.392, 41.832, 12.456),  # south, west, north, east
  # observedProperty = "http://his-central-ontology.geodab.eu/hydro-ontology/concept/65",
  # ontology = "http://his-central-ontology.geodab.eu/hydro-ontology",
  # country = "IT",
  # provider = "Regione Valle d'Aosta",
  # feature = "<feature-id>",
  # localFeatureIdentifier = "<local-feature-id>",
  # observationIdentifier = "<observation-id>",
  # spatialRelation = "within",
  # timeInterpolation = "sum",
  # intendedObservationSpacing = "P1D",
  # aggregationDuration = "P1M",
  # limit = 100L
  # format = "OM_JSON"  # alternative to SHAPEFILE
  # includeData = TRUE    # set FALSE for geometry-only downloads
)

shapefile_zip <- file.path(
  script_dir,
  paste0(
    gsub("[^A-Za-z0-9]+", "_", predefined_layer_label),
    "_observations.zip"
  )
)

shape_download <- client$download_observations(
  shape_constraints,
  save_path = shapefile_zip
)
message("\nShapefile saved to: ", shape_download$zip_path)
message("Extracted shapefile: ", shape_download$shp_path)

ensure_pkg("sf")
ensure_pkg("rosm")

plot_observations_osm <- function(shp_path, title) {
  obs_sf <- sf::st_read(shp_path, quiet = TRUE)
  if (nrow(obs_sf) == 0) {
    stop("Shapefile contains no features.", call. = FALSE)
  }
  obs_sf <- sf::st_transform(obs_sf, 3857)

  p <- ggplot2::ggplot(obs_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(color = "Observations"),
      fill = NA,
      linewidth = 0.8
    ) +
    ggplot2::coord_sf() +
    ggplot2::labs(title = title, color = NULL) +
    ggplot2::theme_minimal()

  tiles <- tryCatch(
    rosm::get_tiles(sf::st_bbox(obs_sf), type = "osm", zoom = 11, crop = TRUE),
    error = function(e) {
      message("OpenStreetMap basemap unavailable: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(tiles)) {
    p <- rosm::autoplot(tiles) +
      ggplot2::geom_sf(
        data = obs_sf,
        inherit.aes = FALSE,
        color = "#d62728",
        fill = grDevices::adjustcolor("#d62728", alpha.f = 0.15),
        linewidth = 0.7
      ) +
      ggplot2::labs(title = title) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")
  } else {
    message("Plotting without basemap (install 'rosm' for OpenStreetMap tiles).")
  }

  p
}

p_map <- plot_observations_osm(
  shape_download$shp_path,
  title = paste0("Observations — ", predefined_layer_label, " (", time_label, ")")
)
print(p_map)

# --- 4. Observation list (OM-JSON), all pages ---
constraints <- base_constraints
max_observation_pages <- NULL

observations <- client$get_observations(constraints)

if (length(observations$current_page_obs) == 0) {
  stop(
    "No observations found for the selected predefined layer and time range.",
    call. = FALSE
  )
}

message("\nFetching all observation pages ...")
observations$fetch_all_pages(max_pages = max_observation_pages)

message(
  "\nTotal observations retrieved: ", length(observations),
  " (", observations$page, " page(s), completed = ", observations$completed, ")"
)

obs_df <- observations$to_df_all()
print(obs_df)

first_obs <- observations[[1]]
message("\nFirst observation ID: ", first_obs$id)
message("  property: ", first_obs$observed_property)

# --- 5. Fetch data points for the first observation ---
obs_with_data <- client$get_observation_with_data(
  observation_id = first_obs$id,
  begin = begin_position,
  end = end_position
)

points_df <- points_to_df(obs_with_data)
if (nrow(points_df) == 0) {
  stop(
    "No plottable data points for observation ", first_obs$id,
    " in ", begin_position, " — ", end_position,
    call. = FALSE
  )
}

message("Data points retrieved: ", nrow(points_df))
print(head(points_df))

# --- 6. Plot time series ---
points_df$Time <- as.POSIXct(
  points_df$Time,
  format = "%Y-%m-%dT%H:%M:%OSZ",
  tz = "UTC"
)

plot_title <- paste0(first_obs$observed_property, " (", time_label, ")")

p <- ggplot(points_df, aes(x = Time, y = Value)) +
  geom_line() +
  geom_point(size = 1.5) +
  labs(
    title = plot_title,
    subtitle = paste0("Observation: ", first_obs$id),
    x = "Time (UTC)",
    y = "Value"
  ) +
  theme_bw()

print(p)

message("\nDone.")

