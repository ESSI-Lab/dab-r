#' @keywords internal
#' @noRd
Feature <- R6::R6Class(
  "Feature",
  public = list(
    id = NULL,
    name = NULL,
    coordinates = NULL,
    parameters = NULL,
    related_party = NULL,
    contact_name = NULL,
    contact_email = NULL,

    initialize = function(feature_json) {
      self$id <- feature_json$id
      self$name <- feature_json$name
      self$coordinates <- feature_json$shape$coordinates
      self$parameters <- params_from_json_array(feature_json$parameter)
      self$related_party <- feature_json$relatedParty %||% list()
      if (length(self$related_party) > 0) {
        party <- self$related_party[[1]]
        self$contact_name <- party$individualName %||% ""
        self$contact_email <- party$electronicMailAddress %||% ""
      } else {
        self$contact_name <- ""
        self$contact_email <- ""
      }
    },

    to_list = function() {
      coords <- self$coordinates
      coord_str <- if (length(coords) >= 2) {
        paste(coords[1], coords[2], sep = ", ")
      } else {
        ""
      }
      list(
        ID = self$id,
        Name = self$name,
        Coordinates = coord_str,
        Source = param_get(self$parameters, "source", ""),
        Identifier = param_get(self$parameters, "identifier", ""),
        `Contact Name` = self$contact_name,
        `Contact Email` = self$contact_email
      )
    },

    print = function() {
      cat("<Feature id=", self$id, " name=", self$name, ">\n", sep = "")
      invisible(self)
    }
  )
)

#' @keywords internal
#' @noRd
Observation <- R6::R6Class(
  "Observation",
  public = list(
    id = NULL,
    type = NULL,
    parameters = NULL,
    source = NULL,
    source_id = NULL,
    observed_property = NULL,
    observed_property_href = NULL,
    original_observed_property = NULL,
    observed_property_definition = NULL,
    feature_of_interest_title = NULL,
    feature_of_interest_href = NULL,
    phenomenon_time_begin = NULL,
    phenomenon_time_end = NULL,
    uom = NULL,
    points = NULL,

    initialize = function(obs_json) {
      self$parameters <- params_from_json_array(obs_json$parameter)
      self$id <- obs_json$id
      self$type <- obs_json$type %||% NULL
      self$source <- param_get(self$parameters, "source")
      self$source_id <- param_get(self$parameters, "sourceId")
      self$observed_property_definition <- param_get(
        self$parameters,
        "observedPropertyDefinition"
      )
      self$original_observed_property <- param_get(
        self$parameters,
        "originalObservedProperty"
      )
      self$observed_property <- json_list_field(obs_json$observedProperty, "title")
      self$observed_property_href <- json_list_field(obs_json$observedProperty, "href")
      self$feature_of_interest_title <- json_list_field(obs_json$featureOfInterest, "title")
      self$feature_of_interest_href <- json_list_field(obs_json$featureOfInterest, "href")
      self$phenomenon_time_begin <- json_list_field(obs_json$phenomenonTime, "begin")
      self$phenomenon_time_end <- json_list_field(obs_json$phenomenonTime, "end")
      result <- obs_json$result %||% list()
      self$points <- result$points %||% list()
      metadata <- result$defaultPointMetadata %||% list()
      self$uom <- json_list_field(metadata, "uom")
    },

    to_list = function() {
      list(
        ID = self$id %||% "",
        Type = self$type %||% "",
        Source = self$source %||% "",
        `Source ID` = self$source_id %||% "",
        `Observed Property` = self$observed_property %||% "",
        `Observed Property Href` = self$observed_property_href %||% "",
        `Original Observed Property` = self$original_observed_property %||% "",
        `Observed Property Definition` = self$observed_property_definition %||% "",
        `Feature Of Interest` = self$feature_of_interest_title %||% "",
        `Feature Of Interest Href` = self$feature_of_interest_href %||% "",
        UOM = self$uom %||% "",
        `Phenomenon Time Begin` = self$phenomenon_time_begin %||% "",
        `Phenomenon Time End` = self$phenomenon_time_end %||% ""
      )
    },

    print = function() {
      cat(
        "<Observation id=", self$id,
        " property=", self$observed_property, ">\n",
        sep = ""
      )
      invisible(self)
    }
  )
)

#' @keywords internal
#' @noRd
Download <- R6::R6Class(
  "Download",
  public = list(
    client = NULL,
    downloadName = NULL,
    sizeInMB = NULL,
    status = NULL,
    timestamp = NULL,
    locator = NULL,
    id = NULL,

    initialize = function(download_json, client = NULL) {
      self$client <- client
      self$downloadName <- download_json$downloadName
      self$sizeInMB <- download_json$sizeInMB
      self$status <- download_json$status
      self$timestamp <- download_json$timestamp
      self$locator <- download_json$locator
      self$id <- download_json$id
    },

    to_list = function() {
      list(
        `File Name` = self$downloadName,
        ID = self$id,
        Status = self$status,
        `Download Link` = self$locator,
        `Size (in MB)` = self$sizeInMB,
        Timestamp = self$timestamp
      )
    },

    delete = function() {
      if (is.null(self$client)) {
        stop("Download is not attached to a client.", call. = FALSE)
      }
      self$client$delete_download(self$id)
    },

    print = function() {
      cat(
        "<Download id=", self$id,
        " name=", self$downloadName,
        " status=", self$status, ">\n",
        sep = ""
      )
      invisible(self)
    }
  )
)

#' @keywords internal
#' @noRd
DeleteResult <- R6::R6Class(
  "DeleteResult",
  public = list(
    status = NULL,
    id = NULL,

    initialize = function(download_id, status = "deleted") {
      self$status <- status
      self$id <- download_id
    },

    to_list = function() {
      list(status = self$status, id = self$id)
    },

    print = function() {
      cat("ID = ", self$id, " | status = ", self$status, "\n", sep = "")
      invisible(self)
    }
  )
)

#' Convert features to a data frame (current page)
#'
#' @param collection Object returned by \code{client$get_features()}.
#' @return A data frame.
#' @export
features_to_df <- function(collection) {
  if (length(collection$current_page_features) == 0) {
    return(data.frame())
  }
  do.call(rbind, lapply(collection$current_page_features, function(f) {
    as.data.frame(f$to_list(), stringsAsFactors = FALSE, check.names = FALSE)
  }))
}

#' Convert observations to a data frame (current page)
#'
#' @param collection Object returned by \code{client$get_observations()}.
#' @return A data frame for \code{collection$current_page_obs} only.
#' @seealso [observations_all_to_df()] for all pages after \code{$next_page()}.
#' @export
observations_to_df <- function(collection) {
  if (length(collection$current_page_obs) == 0) {
    return(data.frame())
  }
  do.call(rbind, lapply(collection$current_page_obs, function(o) {
    as.data.frame(o$to_list(), stringsAsFactors = FALSE, check.names = FALSE)
  }))
}

#' Convert all paginated observations to a data frame
#'
#' @param collection Object returned by \code{client$get_observations()}. Fetch
#'   further pages with \code{collection$next_page()} before calling this
#'   function.
#' @return A data frame of every observation accumulated in the collection.
#' @export
observations_all_to_df <- function(collection) {
  if (length(collection$observations) == 0) {
    return(data.frame())
  }
  do.call(rbind, lapply(collection$observations, function(o) {
    as.data.frame(o$to_list(), stringsAsFactors = FALSE, check.names = FALSE)
  }))
}

#' Convert observation points to a data frame
#'
#' @param observation Object returned by \code{client$get_observation_with_data()}.
#' @return A data frame with columns \code{Time} and \code{Value}.
#' @export
points_to_df <- function(observation) {
  rows <- observation_points_list(observation)
  if (length(rows) == 0) {
    return(data.frame(Time = character(), Value = numeric()))
  }
  data.frame(
    Time = vapply(rows, `[[`, character(1), "Time"),
    Value = vapply(rows, `[[`, numeric(1), "Value"),
    stringsAsFactors = FALSE
  )
}

#' Convert downloads to a data frame
#'
#' @param collection Object returned by \code{client$get_download_status()}.
#' @return A data frame.
#' @export
downloads_to_df <- function(collection) {
  if (length(collection$downloads) == 0) {
    return(data.frame())
  }
  do.call(rbind, lapply(collection$downloads, function(d) {
    as.data.frame(d$to_list(), stringsAsFactors = FALSE, check.names = FALSE)
  }))
}
