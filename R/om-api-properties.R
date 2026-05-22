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
    client = NULL,
    property = NULL,
    entries = NULL,
    current_page_entries = NULL,
    resumption_token = NULL,
    completed = FALSE,
    page = 1,
    limit = NULL,
    verbose = TRUE,

    initialize = function(
        client,
        property,
        data,
        limit = NULL,
        page = 1,
        verbose = TRUE) {
      self$client <- client
      self$property <- property
      self$limit <- limit
      self$page <- page
      self$verbose <- verbose
      self$append_page(data)
    },

    append_page = function(data) {
      raw_items <- data[[self$property]] %||% list()
      if (!is.list(raw_items)) {
        raw_items <- list()
      }
      new_entries <- lapply(raw_items, PropertyEntry$new)
      self$current_page_entries <- new_entries
      self$entries <- c(self$entries, new_entries)
      self$resumption_token <- parse_resumption_token(data$resumptionToken)
      self$completed <- isTRUE(data$completed) || is.null(self$resumption_token)
      if (self$verbose) {
        self$print_page_summary(length(new_entries))
      }
      invisible(self)
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

    next_page = function() {
      if (self$completed || is.null(self$resumption_token)) {
        message("No more property values to fetch.")
        return(invisible(self))
      }

      url <- paste0(
        self$client$base_url,
        "properties?property=",
        encode_query_value(self$property)
      )
      if (!is.null(self$limit)) {
        url <- paste0(url, "&limit=", as.integer(self$limit))
      }
      url <- paste0(url, "&resumptionToken=", encode_query_value(self$resumption_token))
      self$page <- self$page + 1L
      if (self$verbose) {
        message(
          "Retrieving properties page ", self$page, ": ",
          self$client$obfuscate_url(url)
        )
      }

      data <- dab_json(url, method = "GET")
      self$append_page(data)
      invisible(self)
    },

    fetch_all_pages = function(max_pages = NULL) {
      while (!self$completed) {
        if (!is.null(max_pages) && self$page >= max_pages) {
          message(
            "Stopped properties pagination at max_pages = ", max_pages,
            " (", length(self$entries), " entries so far)."
          )
          break
        }
        self$next_page()
      }
      invisible(self)
    },

    print_page_summary = function(n_returned) {
      prefix <- if (self$page == 1L) "first" else "next"
      title <- switch(
        self$property,
        predefinedSearchArea = "predefined search areas",
        paste0("property values (", self$property, ")")
      )
      msg <- paste0("Returned ", prefix, " ", n_returned, " ", title)
      if (self$completed) {
        message(msg, " (completed).")
      } else if (!is.null(self$resumption_token)) {
        message(
          msg,
          " (not completed).\n",
          "Use $next_page() or $fetch_all_pages() for more."
        )
      } else {
        message(msg, " (completed).")
      }
    },

    print_values = function() {
      title <- switch(
        self$property,
        predefinedSearchArea = "Predefined search areas",
        paste0("Property values: ", self$property)
      )
      message(title, " (", length(self$entries), " entries total):")
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

    to_df = function() properties_to_df(self),

    to_df_all = function() properties_all_to_df(self)
  )
)

#' Convert a properties result to a data frame (current page)
#'
#' @param properties Object returned by \code{client$get_properties()}.
#' @return A data frame for the current page only.
#' @export
properties_to_df <- function(properties) {
  if (length(properties$current_page_entries) == 0) {
    return(data.frame(
      Label = character(),
      Value = character(),
      `Observation Count` = integer(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  do.call(rbind, lapply(properties$current_page_entries, function(e) {
    as.data.frame(e$to_list(), stringsAsFactors = FALSE, check.names = FALSE)
  }))
}

#' Convert all paginated property values to a data frame
#'
#' @param properties Object returned by \code{client$get_properties()}. Fetch
#'   further pages with \code{properties$next_page()} or
#'   \code{properties$fetch_all_pages()} first.
#' @return A data frame of every property entry accumulated in the result.
#' @export
properties_all_to_df <- function(properties) {
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
    stop(
      "property is required (e.g. PREDEFINED_SEARCH_AREA).",
      call. = FALSE
    )
  }
  property <- normalize_om_api_property(property)
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
  PropertiesResult$new(
    client = client,
    property = property,
    data = data,
    limit = limit,
    verbose = verbose
  )
}

#' Query OM-API property values
#'
#' Retrieves distinct values for a constraint property (e.g. predefined search
#' areas via \code{property = PREDEFINED_SEARCH_AREA}). Supports pagination via
#' \code{$next_page()} and \code{$fetch_all_pages()} when the API returns a
#' \code{resumptionToken}.
#'
#' Call on a [DABClient], [WHOSClient], or [HISCentralClient] object as
#' \code{client$get_properties(property, limit, verbose)}.
#'
#' @param property Property name (e.g. [PREDEFINED_SEARCH_AREA]).
#' @param limit Optional page size (maximum number of values per request).
#' @param verbose Print request URLs and page summaries.
#'
#' @return A \code{PropertiesResult} object. Use \code{$print_values()} to print
#'   all accumulated labels and values, \code{$to_df_all()} or
#'   [properties_all_to_df()] for a full table, and \code{$get_item(i)} to select
#'   an entry (use its \code{$value} in [Constraints()]).
#'
#' @examples
#' \dontrun{
#' client <- HISCentralClient(token = "my-token")
#' areas <- client$get_properties(PREDEFINED_SEARCH_AREA, limit = 10)
#' areas$fetch_all_pages()
#' areas$print_values()
#' constraints <- Constraints(predefinedSearchArea = areas$get_item(1)$value)
#' }
#'
#' @seealso [Constraints()], [properties_to_df()], [properties_all_to_df()]
#' @name get_properties
NULL
