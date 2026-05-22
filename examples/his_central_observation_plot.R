#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# HIS-Central demo: predefined search area, observations, shapefile, plots
# ------------------------------------------------------------------------------
#
# Prerequisites
#   1. Copy and edit the config file (token only; install settings optional):
#        cp his_central_config.json.example his_central_config.json
#   2. Run from the examples/ directory, or pass a config path as first argument.
#
# Usage
#   Rscript his_central_observation_plot.R
#   Rscript his_central_observation_plot.R /path/to/his_central_config.json
#
# If dabr is already installed, you can skip the setup and start with:
#   library(dabr); library(ggplot2)
#   token <- "your-token"
  # client <- HISCentralClient(token = token)
#
# What this script demonstrates (dabr OM-API)
#   A. List predefined search areas  — get_properties(PREDEFINED_SEARCH_AREA)
#   B. Download observation footprints — download_observations(SHAPEFILE)
#   C. Map on OpenStreetMap            — sf + rosm
#   D. List all observations           — get_observations() + pagination
#   E. Time series for one observation — get_observation_with_data()
#
# API docs: https://his-central.geodab.eu/gs-service/om-api/
# ------------------------------------------------------------------------------

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/"))
} else {
  normalizePath(getwd(), winslash = "/")
}

config_arg <- commandArgs(trailingOnly = TRUE)[1]
source(file.path(script_dir, "his_central_setup.R"))
cfg <- init_dabr_example(config_path = config_arg, script_dir = script_dir)

token <- cfg$token
script_dir <- cfg$script_dir

ensure_cran_pkg("ggplot2")
library(ggplot2)

# ==============================================================================
# Parameters (edit for your experiment)
# ==============================================================================

search_area_label <- "Torrente Evenson"
properties_page_limit <- 10L
max_properties_pages <- NULL
max_observation_pages <- NULL

today <- Sys.Date()
first_of_month <- as.Date(format(today, "%Y-%m-01"))
last_month_end <- first_of_month - 1L
last_month_start <- as.Date(format(last_month_end, "%Y-%m-01"))
begin_position <- paste0(format(last_month_start, "%Y-%m-%d"), "T00:00:00Z")
end_position <- paste0(format(last_month_end, "%Y-%m-%d"), "T23:59:59Z")
time_label <- format(last_month_start, "%B %Y")

# Optional Constraints() fields (uncomment in base_constraints / shape_constraints):
#   bbox = c(south, west, north, east)
#   observedProperty, ontology, country, provider, feature
#   localFeatureIdentifier, observationIdentifier, spatialRelation
#   timeInterpolation, intendedObservationSpacing, aggregationDuration, limit

client <- HISCentralClient(token = token)

# ==============================================================================
# A. Predefined search areas (properties API, all pages)
# ==============================================================================

search_areas <- client$get_properties(PREDEFINED_SEARCH_AREA, limit = properties_page_limit)
search_areas$fetch_all_pages(max_pages = max_properties_pages)

message(
  "Predefined search areas loaded: ", length(search_areas),
  " (", search_areas$page, " page(s))"
)
search_areas$print_values()

area_labels <- vapply(search_areas$entries, function(e) e$label, character(1))
area_index <- match(search_area_label, area_labels)
if (is.na(area_index)) {
  stop(
    "Search area not found: \"", search_area_label, "\".\n",
    "Available:\n  ", paste(area_labels, collapse = "\n  "),
    call. = FALSE
  )
}
selected_area <- search_areas$get_item(area_index)

base_constraints <- Constraints(
  predefinedSearchArea = selected_area$value,
  beginPosition = begin_position,
  endPosition = end_position
)

# ==============================================================================
# B. Observation footprints as shapefile (format = SHAPEFILE, includeData = FALSE)
# ==============================================================================

shape_download <- client$download_observations(
  Constraints(
    predefinedSearchArea = base_constraints$predefinedSearchArea,
    beginPosition = base_constraints$beginPosition,
    endPosition = base_constraints$endPosition,
    format = "SHAPEFILE",
    includeData = FALSE
  ),
  save_path = file.path(
    script_dir,
    paste0(gsub("[^A-Za-z0-9]+", "_", search_area_label), "_observations.zip")
  )
)
message("Shapefile: ", shape_download$shp_path)

# ==============================================================================
# C. Map shapefile on OpenStreetMap
# ==============================================================================

ensure_cran_pkg("sf")
ensure_cran_pkg("rosm")

obs_sf <- sf::st_read(shape_download$shp_path, quiet = TRUE)
obs_sf <- sf::st_transform(obs_sf, 3857)
map_title <- paste0("Observations — ", search_area_label, " (", time_label, ")")

p_map <- ggplot2::ggplot(obs_sf) +
  ggplot2::geom_sf(ggplot2::aes(color = "Observations"), fill = NA, linewidth = 0.8) +
  ggplot2::coord_sf() +
  ggplot2::labs(title = map_title) +
  ggplot2::theme_minimal()

tiles <- tryCatch(
  rosm::get_tiles(sf::st_bbox(obs_sf), type = "osm", zoom = 11, crop = TRUE),
  error = function(e) NULL
)
if (!is.null(tiles)) {
  p_map <- rosm::autoplot(tiles) +
    ggplot2::geom_sf(
      data = obs_sf,
      inherit.aes = FALSE,
      color = "#d62728",
      fill = grDevices::adjustcolor("#d62728", alpha.f = 0.15),
      linewidth = 0.7
    ) +
    ggplot2::labs(title = map_title) +
    ggplot2::theme_minimal()
}
print(p_map)

# ==============================================================================
# D. Observation metadata (OM-JSON, all pages)
# ==============================================================================

observations <- client$get_observations(base_constraints)
if (length(observations$current_page_obs) == 0) {
  stop("No observations for this search area and time range.", call. = FALSE)
}
observations$fetch_all_pages(max_pages = max_observation_pages)

message(
  "Observations loaded: ", length(observations),
  " (", observations$page, " page(s))"
)
print(observations$to_df_all())

# ==============================================================================
# E. Time series for the first observation in the list
# ==============================================================================

first_obs <- observations[[1]]
obs_with_data <- client$get_observation_with_data(
  observation_id = first_obs$id,
  begin = begin_position,
  end = end_position
)

points_df <- points_to_df(obs_with_data)
if (nrow(points_df) == 0) {
  stop("No data points for observation ", first_obs$id, " in ", time_label, call. = FALSE)
}

points_df$Time <- as.POSIXct(points_df$Time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")

print(ggplot(points_df, aes(x = Time, y = Value)) +
  geom_line() +
  geom_point() +
  labs(
    title = paste(first_obs$observed_property, "—", time_label),
    subtitle = first_obs$id,
    x = "Time (UTC)",
    y = "Value"
  ) +
  theme_bw())

message("Done.")
