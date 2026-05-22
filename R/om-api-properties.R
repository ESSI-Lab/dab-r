#' @keywords internal
#' @noRd
PropertyEntry <- R6::R6Class(
  "PropertyEntry",
  public = list(
    observation_count = NULL,
    label = NULL,
    value = NULL,

    initialize = function(entry_json) {
      self$observation_count <- entry_json$observationCount %||% NA_integer_
      self$label <- entry_json$label %||% ""
      self$value <- entry_json$value %||% ""
    },

    to_list = function() {
      list(
        Label = self$label,
        Value = self$value,
        `Observation Count` = self$observation_count
      )
    }
  )
)

#' @keywords internal
#' @noRd
PropertiesResult <- R6::R6Class(
  "PropertiesResult",
  public = list(
    property = NULL,
    entries = NULL,
    completed = NULL,

    initialize = function(data, property) {
      self$property <- property
      raw_items <- data[[property]] %||% list()
      if (!is.list(raw_items)) {
        raw_items <- list()
      }
      self$entries <- lapply(raw_items, PropertyEntry$new)
      self$completed <- isTRUE(data$completed)
    },

    length = function() length(self$entries),

    get_item = function(i) {
      i <- as.integer(i)[1]
      if (is.na(i) || i < 1L) {
        stop("Index must be a positive integer.", call. = FALSE)
      }
      if (i > length(self$entries)) {
        stop("Index ", i, " out of range (length = ", length(self$entries), ").",
             call. = FALSE)
      }
      self$entries[[i]]
    },

    print_values = function() {
      title <- switch(
        self$property,
        predefinedLayer = "Predefined search areas",
        paste0("Property values: ", self$property)
      )
      message(title, " (", length(self$entries), " entries):")
      if (length(self$entries) == 0) {
        message("  (none)")
        return(invisible(self))
      }
      for (entry in self$entries) {
        message(
          "  ", entry$label,
          "  [", entry$observation_count, " observations]",
          "\n    value: ", entry$value
        )
      }
      invisible(self)
    },

    to_df = function() properties_to_df(self)
  )
)

#' Convert a properties result to a data frame
#'
#' @param properties Object returned by \code{client$get_properties()}.
#' @return A data frame with columns Label, Value, and Observation Count.
#' @export
properties_to_df <- function(properties) {
  if (length(properties$entries) == 0) {
    return(data.frame(
      Label = character(),
      Value = character(),
      `Observation Count` = integer(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  do.call(rbind, lapply(properties$entries, function(e) {
    as.data.frame(e$to_list(), stringsAsFactors = FALSE, check.names = FALSE)
  }))
}

#' Fetch OM-API property values
#'
#' @keywords internal
#' @noRd
fetch_om_properties <- function(client, property, limit = NULL, verbose = TRUE) {
  if (is.null(property) || !nzchar(property)) {
    stop("property is required (e.g. 'predefinedLayer').", call. = FALSE)
  }
  url <- paste0(
    client$base_url,
    "properties?property=",
    encode_query_value(property)
  )
  if (!is.null(limit)) {
    url <- paste0(url, "&limit=", as.integer(limit))
  }
  if (verbose) {
    message("Retrieving properties: ", client$obfuscate_url(url))
  }
  data <- dab_json(url, method = "GET")
  PropertiesResult$new(data, property = property)
}

#' Query OM-API property values
#'
#' Retrieves distinct values for a constraint property (e.g. predefined search
#' areas via \code{property = "predefinedLayer"}). Call on a [DABClient],
#' [WHOSClient], or [HISCentralClient] object as
#' \code{client$get_properties(property, limit, verbose)}.
#'
#' @param property Property name (e.g. \code{"predefinedLayer"}).
#' @param limit Optional maximum number of values.
#' @param verbose Print the request URL (token obfuscated).
#'
#' @return A \code{PropertiesResult} object. Use \code{$print_values()} to print
#'   labels and values, \code{$to_df()} or [properties_to_df()] for a table,
#'   and \code{$get_item(i)} to select an entry (use its \code{$value} in
#'   [Constraints()]).
#'
#' @examples
#' \dontrun{
#' client <- HISCentralClient(token = "my-token")
#' layers <- client$get_properties("predefinedLayer", limit = 10)
#' layers$print_values()
#' constraints <- Constraints(predefinedLayer = layers$get_item(1)$value)
#' }
#'
#' @seealso [Constraints()], [properties_to_df()]
#' @name get_properties
NULL
