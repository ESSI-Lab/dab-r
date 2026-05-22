#!/usr/bin/env Rscript
# Example: HIS-Central observation search and time-series plot
#
# Usage:
#   export DAB_TOKEN="your-token"
#   Rscript examples/his_central_observation_plot.R
#
# Optional environment variables:
#   DAB_REPO   GitHub repo for remotes::install_github (default: ESSI-Lab/dab-r)
#   DAB_REF    Git ref to install (default: HEAD / main)

# --- 1. Install dabr from GitHub via remotes ---
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

dab_repo <- Sys.getenv("DAB_REPO", "ESSI-Lab/dab-r")
dab_ref <- Sys.getenv("DAB_REF", unset = NA_character_)

if (!requireNamespace("dabr", quietly = TRUE)) {
  message("Installing dabr from GitHub (", dab_repo, ") ...")
  if (nzchar(dab_ref)) {
    remotes::install_github(dab_repo, ref = dab_ref, upgrade = "never")
  } else {
    remotes::install_github(dab_repo, upgrade = "never")
  }
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Installing ggplot2 for plotting ...")
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}

library(dabr)
library(ggplot2)

# --- 2. Credentials and search parameters ---
token <- Sys.getenv("DAB_TOKEN")
if (!nzchar(token)) {
  stop(
    "Set your GeoDAB token in the DAB_TOKEN environment variable.\n",
    "Example: export DAB_TOKEN='my-token'",
    call. = FALSE
  )
}

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

plot_title <- paste0(
  first_obs$observed_property,
  " (", format(first_of_prev_month, "%B %Y"), ")"
)

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
