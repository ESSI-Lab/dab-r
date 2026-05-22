#' @keywords internal
#' @noRd
DABClientClass <- R6::R6Class(
  "DABClient",
  public = list(
    token = NULL,
    view = NULL,
    base_url_template = NULL,
    base_url = NULL,

    initialize = function(
        token = "{token}",
        view = "{view}",
        base_url_template = NULL) {
      self$token <- token
      self$view <- view
      self$base_url_template <- base_url_template %||%
        "https://gs-service-preproduction.geodab.eu/gs-service/services/essi/token/{token}/view/{view}/om-api/"

      if (!grepl("\\{token\\}", self$token) && !grepl("\\{view\\}", self$view)) {
        self$base_url <- gsub(
          "{token}", self$token,
          gsub("{view}", self$view, self$base_url_template, fixed = TRUE),
          fixed = TRUE
        )
      } else {
        self$base_url <- self$base_url_template
      }
    },

    obfuscate_url = function(url) {
      url <- gsub(self$token, "***", url, fixed = TRUE)
      if (!grepl("id=", url, fixed = TRUE)) {
        return(url)
      }
      parts <- strsplit(url, "id=", fixed = TRUE)[[1]]
      prefix <- parts[1]
      id_part <- parts[2]
      if (grepl("%3A", id_part, fixed = TRUE)) {
        uuid_part <- sub("^[^%]+%3A", "", id_part)
        return(paste0(prefix, "id=***%3A", uuid_part))
      }
      if (grepl(":", id_part, fixed = TRUE)) {
        uuid_part <- sub("^[^:]+:", "", id_part)
        return(paste0(prefix, "id=***:", uuid_part))
      }
      url
    },

    get_features = function(constraints, verbose = TRUE) {
      url <- paste0(self$base_url, "features?", constraints_to_query(constraints))
      if (verbose) {
        message("Retrieving page 1: ", self$obfuscate_url(url))
      }
      data <- dab_json(url, method = "GET")
      features_list <- lapply(data$results %||% list(), Feature$new)
      resumption_token <- parse_resumption_token(data$resumptionToken)
      collection <- FeaturesCollection$new(
        self,
        constraints,
        initial_features = features_list,
        resumption_token = resumption_token,
        page = 1L,
        verbose = verbose
      )
      collection$completed <- isTRUE(data$completed)
      collection
    },

    get_observations = function(constraints, verbose = TRUE) {
      url <- paste0(
        self$base_url,
        "observations?",
        constraints_to_query(constraints)
      )
      if (verbose) {
        message("Retrieving page 1: ", self$obfuscate_url(url))
      }
      data <- dab_json(url, method = "GET")
      obs_list <- lapply(data$member %||% list(), Observation$new)
      resumption_token <- parse_resumption_token(data$resumptionToken)
      collection <- ObservationsCollection$new(
        self,
        constraints,
        initial_obs = obs_list,
        resumption_token = resumption_token,
        page = 1L,
        verbose = verbose
      )
      collection$completed <- isTRUE(data$completed)
      collection
    },

    get_observation_with_data = function(
        observation_id,
        begin = NULL,
        end = NULL) {
      url <- paste0(
        self$base_url,
        "observations?includeData=true&observationIdentifier=",
        encode_query_value(observation_id)
      )
      if (!is.null(begin)) {
        url <- paste0(url, "&beginPosition=", encode_query_value(begin))
      }
      if (!is.null(end)) {
        url <- paste0(url, "&endPosition=", encode_query_value(end))
      }
      message("Retrieving ", self$obfuscate_url(url))
      data <- dab_json(url, method = "GET")
      member <- data$member %||% list()
      if (length(member) == 0) {
        message("No observation data available for the requested time range.")
        return(NULL)
      }
      Observation$new(member[[1]])
    },

    create_download = function(download_constraints) {
      if (!inherits(download_constraints, "DownloadConstraints")) {
        stop(
          "download_constraints must be a DownloadConstraints object.",
          call. = FALSE
        )
      }
      query <- download_constraints_to_query(download_constraints)
      url <- paste0(self$base_url, "downloads?", query)
      message("DOWNLOAD URL: ", self$obfuscate_url(url))
      data <- dab_json(url, method = "PUT")
      download_obj <- Download$new(data, client = self)
      message(
        'File "', download_obj$downloadName,
        '" is ', download_obj$status, '.\nID = "', download_obj$id, '"'
      )
      download_obj
    },

    get_download_status = function(download_id = NULL, verbose = TRUE) {
      if (!is.null(download_id)) {
        url <- paste0(
          self$base_url,
          "downloads?id=",
          encode_query_value(download_id)
        )
      } else {
        url <- paste0(self$base_url, "downloads")
      }
      if (verbose) {
        message("STATUS URL: ", self$obfuscate_url(url))
      }
      data <- dab_json(url, method = "GET")
      downloads_list <- lapply(data$results %||% list(), function(d) {
        Download$new(d, client = self)
      })
      DownloadsCollection$new(downloads_list)
    },

    delete_download = function(download_id) {
      if (is.null(download_id) || !nzchar(download_id)) {
        stop("download_id is required", call. = FALSE)
      }
      url <- paste0(
        self$base_url,
        "downloads?id=",
        encode_query_value(download_id)
      )
      message(
        'Deleting ID "', download_id, '" ...\nDELETE URL: ',
        self$obfuscate_url(url)
      )
      dab_request(url, method = "DELETE")
      DeleteResult$new(download_id)
    },

    wait_for_download = function(download_id, poll_interval = 3) {
      cat("Status: ", sep = "")
      previous_status <- NULL
      normalize <- function(status) {
        if (status %in% c("Submitted", "Started", "Completed")) {
          status
        } else {
          "Downloading..."
        }
      }
      repeat {
        obj <- self$get_download_status(download_id, verbose = FALSE)[[1]]
        current <- normalize(obj$status)
        if (!identical(current, previous_status)) {
          if (is.null(previous_status)) {
            cat(current)
          } else {
            cat(" ⟶ ", current, sep = "")
          }
          previous_status <- current
        }
        if (tolower(obj$status) == "completed") {
          cat("\nDownload link: ", obj$locator, "\n", sep = "")
          return(obj)
        }
        Sys.sleep(poll_interval)
      }
    },

    save_locator = function(locator, filename = NULL, save_dir = NULL) {
      save_dir <- save_dir %||% file.path(Sys.getenv("HOME"), "Downloads")
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      }

      if (is.null(filename)) {
        path <- basename(sub("\\?.*$", "", locator))
        if (!nzchar(path)) {
          path <- "download.dat"
        }
        filename <- path
      }

      save_path <- file.path(save_dir, filename)
      if (file.exists(save_path)) {
        base <- tools::file_path_sans_ext(filename)
        ext <- tools::file_ext(filename)
        ext_part <- if (nzchar(ext)) paste0(".", ext) else ""
        i <- 1L
        while (file.exists(save_path)) {
          save_path <- file.path(
            save_dir,
            paste0(base, " (", i, ")", ext_part)
          )
          i <- i + 1L
        }
      }

      resp <- dab_request(locator, method = "GET")
      writeBin(httr2::resp_body_raw(resp), save_path)
      message("Download complete!\nFile saved to: ", save_path)
      save_path
    },

    save_download = function(download_id, filename = NULL, save_dir = NULL) {
      obj <- self$get_download_status(download_id, verbose = FALSE)[[1]]
      if (tolower(obj$status) != "completed") {
        stop(
          'Download "', download_id, '" is not completed yet (status: ',
          obj$status, ")",
          call. = FALSE
        )
      }
      self$save_locator(obj$locator, filename = filename, save_dir = save_dir)
    },

    create_save_download = function(
        download_constraints,
        poll_interval = 5,
        filename = NULL,
        save_dir = NULL) {
      download <- self$create_download(download_constraints)
      completed <- self$wait_for_download(download$id, poll_interval)
      self$save_download(
        completed$id,
        filename = filename,
        save_dir = save_dir
      )
    },

    plot_observation = function(obs, title = NULL) {
      if (is.null(obs) || length(obs$points) == 0) {
        message("No data points available for this observation.")
        return(invisible(NULL))
      }
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
          "Package 'ggplot2' is required for plot_observation(). ",
          "Install it with install.packages('ggplot2').",
          call. = FALSE
        )
      }
      df <- points_to_df(obs)
      df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Time, y = .data$Value)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::labs(
          title = title %||% paste(obs$observed_property, "time series"),
          x = "Date",
          y = "Value"
        ) +
        ggplot2::theme_bw()
      print(p)
      invisible(p)
    }
  )
)

#' Create a generic DAB OM-API client
#'
#' @param token GeoDAB token embedded in the URL path.
#' @param view GeoDAB view (e.g. \code{"whos"}, \code{"his-central"}).
#' @param base_url_template Optional URL template with \code{\{token\}} and
#'   \code{\{view\}} placeholders.
#' @return A [DABClient] R6 object.
#' @export
#' @examples
#' \dontrun{
#' client <- DABClient(token = "my-token", view = "whos")
#' features <- client$get_features(Constraints(bbox = c(60.4, 22.1, 60.7, 22.7)))
#' }
DABClient <- function(
    token = "{token}",
    view = "{view}",
    base_url_template = NULL) {
  DABClientClass$new(
    token = token,
    view = view,
    base_url_template = base_url_template
  )
}

#' Create a WHOS OM-API client
#'
#' @inheritParams DABClient
#' @return A [DABClient] R6 object configured for WHOS.
#' @export
WHOSClient <- function(token, view = "whos") {
  DABClientClass$new(
    token = token,
    view = view,
    base_url_template = "https://whos.geodab.eu/gs-service/services/essi/token/{token}/view/{view}/om-api/"
  )
}

#' Create a HIS-Central OM-API client
#'
#' @inheritParams DABClient
#' @return A [DABClient] R6 object configured for HIS-Central.
#' @export
HISCentralClient <- function(token, view = "his-central") {
  DABClientClass$new(
    token = token,
    view = view,
    base_url_template = "https://his-central.geodab.eu/gs-service/services/essi/token/{token}/view/{view}/om-api/"
  )
}
