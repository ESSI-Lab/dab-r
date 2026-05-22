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

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1L && is.na(x))) y else x

config <- load_config(config_path)

token <- as.character(config$token)
if (!nzchar(token) || grepl("REPLACE_WITH", token, fixed = TRUE)) {
  stop("Set a valid GeoDAB token in ", config_path, call. = FALSE)
}

install_cfg <- config$install %||% list()
dab_repo <- install_cfg$github_repo %||% "ESSI-Lab/dab-r"
dab_ref <- install_cfg$github_ref %||% ""
if (length(dab_ref) == 0L || is.na(dab_ref)) {
  dab_ref <- NA_character_
}

# --- 1. Install dabr from GitHub via remotes ---
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

force_install <- isTRUE(install_cfg$force)
if (!requireNamespace("dabr", quietly = TRUE) || force_install) {
  message("Installing dabr from GitHub (", dab_repo, ") ...")
  if (nzchar(dab_ref)) {
    remotes::install_github(dab_repo, ref = dab_ref, upgrade = "always")
  } else {
    remotes::install_github(dab_repo, upgrade = "always")
  }
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Installing ggplot2 for plotting ...")
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}

library(dabr)
library(ggplot2)

# Fail fast if an outdated dabr build leaves {token}/{view} in URLs
url_check <- HISCentralClient(token = "url-check")$base_url
if (grepl("/token/\\{token\\}/", url_check)) {
  stop(
    "Installed 'dabr' does not substitute token/view in API URLs.\n",
    "Reinstall: remotes::install_github(\"", dab_repo, "\", upgrade = \"always\")\n",
    "Or set \"force\": true under install in ", config_path,
    call. = FALSE
  )
}

# --- 2. Search parameters (edit here) ---
# Bounding box (south, west, north, east) — Rome area (HIS-Central demo extent)
bbox <- c(
  south = 41.777,
  west = 12.392,
  north = 41.832,
  east = 12.456
)

# Temporal extent: previous calendar month (UTC, ISO 8601)
today <- Sys.Date()
first_of_this_month <- as.Date(format(today, "%Y-%m-01"))
last_of_prev_month <- first_of_this_month - 1L
first_of_prev_month <- as.Date(format(last_of_prev_month, "%Y-%m-01"))

begin_position <- paste0(format(first_of_prev_month, "%Y-%m-%d"), "T00:00:00Z")
end_position <- paste0(format(last_of_prev_month, "%Y-%m-%d"), "T23:59:59Z")
time_label <- format(first_of_prev_month, "%B %Y")

message("Using config: ", config_path)
message("HIS-Central observation search")
message("  bbox: south=", bbox["south"], ", west=", bbox["west"],
        ", north=", bbox["north"], ", east=", bbox["east"])
message("  time: ", begin_position, " — ", end_position)

# --- 3. HIS-Central client and observation search ---
client <- HISCentralClient(token = token)

constraints <- Constraints(
  bbox = c(bbox["south"], bbox["west"], bbox["north"], bbox["east"]),
  beginPosition = begin_position,
  endPosition = end_position
)

observations <- client$get_observations(constraints)
message("\nObservations on first page: ", length(observations$current_page_obs))

if (length(observations$current_page_obs) == 0) {
  stop("No observations found for the given bbox and time range.", call. = FALSE)
}

obs_df <- observations$to_df()
print(obs_df)

first_obs <- observations[[1]]
message("\nFirst observation ID: ", first_obs$id)
message("  property: ", first_obs$observed_property)

# --- 4. Fetch data points for the first observation ---
obs_with_data <- client$get_observation_with_data(
  observation_id = first_obs$id,
  begin = begin_position,
  end = end_position
)

if (is.null(obs_with_data) || length(obs_with_data$points) == 0) {
  stop("No data points returned for the first observation.", call. = FALSE)
}

points_df <- points_to_df(obs_with_data)
message("Data points retrieved: ", nrow(points_df))
print(head(points_df))

# --- 5. Plot time series ---
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
