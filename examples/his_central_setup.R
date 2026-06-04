# Load config and install dab.r (used by his_central_observation_plot.R).
# Can also be sourced in an interactive session:
#   cfg <- init_dab.r_example("his_central_config.json")

init_dab.r_example <- function(config_path = NULL, script_dir = NULL) {
  cran <- "https://cloud.r-project.org"

  if (is.null(script_dir)) {
    script_dir <- example_script_dir()
  }

  print(script_dir)

  if (is.null(config_path)) {
    config_path <- file.path(script_dir, "his_central_config.json")
  } else {
    config_path <- normalizePath(config_path, winslash = "/")
  }

  if (!file.exists(config_path)) {
    stop(
      "Config not found: ", config_path, "\n",
      "  cp his_central_config.json.example his_central_config.json\n",
      "  # set token and install.source (github or local)",
      call. = FALSE
    )
  }

  config <- read_example_config(config_path)
  token <- as.character(config$token)
  if (!nzchar(token) || grepl("REPLACE_WITH", token, fixed = TRUE)) {
    stop("Set your GeoDAB token in ", config_path, call. = FALSE)
  }

  install_cfg <- config$install %||% list()
  install_source <- tolower(install_cfg$source %||% "")
  if (!install_source %in% c("github", "local")) {
    stop('install.source must be "github" or "local" in ', config_path, call. = FALSE)
  }

  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = cran)
  }

  local_pkg <- find_dab.r_package_dir(script_dir, config_path)
  if (install_source == "local" && is.null(local_pkg)) {
    stop(
      'install.source is "local" but this directory is not inside the dab-r package.\n',
      'Use install.source = "github" or run from the repository clone.',
      call. = FALSE
    )
  }

  if (!requireNamespace("dab.r", quietly = TRUE) || isTRUE(install_cfg$force)) {
    if (install_source == "local") {
      message("Installing dab.r from: ", local_pkg)
      remotes::install_local(local_pkg, upgrade = "always", force = TRUE)
    } else {
      repo <- install_cfg$github_repo %||% "ESSI-Lab/dab-r"
      ref <- install_cfg$github_ref %||% ""
      message("Installing dab.r from GitHub: ", repo)
      remotes::install_github(
        repo,
        ref = if (nzchar(ref)) ref else NULL,
        upgrade = "always",
        force = isTRUE(install_cfg$force)
      )
    }
  }

  library(dab.r)

  invisible(list(
    token = token,
    script_dir = script_dir,
    config_path = config_path
  ))
}

example_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/"))
  } else {
    normalizePath(getwd(), winslash = "/")
  }
}

read_example_config <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite", repos = "https://cloud.r-project.org")
  }
  cfg <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  if (is.null(cfg$token)) {
    stop("Config missing required field: token", call. = FALSE)
  }
  cfg
}

find_dab.r_package_dir <- function(script_dir, config_path) {
  for (dir in unique(normalizePath(c(
    script_dir,
    file.path(script_dir, ".."),
    dirname(config_path),
    getwd()
  ), winslash = "/", mustWork = FALSE))) {
    desc <- file.path(dir, "DESCRIPTION")
    if (file.exists(desc) &&
        any(grepl("^Package:\\s*dab\\.r\\s*$", readLines(desc, n = 20, warn = FALSE)))) {
      return(dir)
    }
  }
  NULL
}

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1L && is.na(x))) y else x

ensure_cran_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
