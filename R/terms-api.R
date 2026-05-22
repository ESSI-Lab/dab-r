#' @keywords internal
#' @noRd
TermsAPIClass <- R6::R6Class(
  "TermsAPI",
  public = list(
    #' @field token GeoDAB API token.
    token = NULL,
    #' @field view GeoDAB view name (e.g. \code{"blue-cloud-terms"}).
    view = NULL,

    #' @description Create a Terms API client.
    #' @param token GeoDAB token.
    #' @param view GeoDAB view.
    initialize = function(token, view) {
      self$token <- token
      self$view <- view
    },

    #' @description Retrieve controlled vocabulary terms.
    #' @param type Term type (e.g. \code{"instrument"}).
    #' @param max Maximum number of terms to print; all received terms are returned.
    #' @param verbose Print summary to console.
    #' @return A \code{Terms} object.
    get_terms = function(type, max, verbose = TRUE) {
      url <- sprintf(
        "https://gs-service-preproduction.geodab.eu/gs-service/services/essi/token/%s/view/%s/terms-api/terms?type=%s&max=%s",
        self$token,
        self$view,
        type,
        max
      )
      resp <- dab_request(url, method = "GET")
      data <- httr2::resp_body_json(resp, simplifyVector = FALSE)
      terms <- Terms$new()

      if (!is.null(data$terms)) {
        for (term_data in data$terms) {
          if (!is.null(term_data$count) && !is.null(term_data$value)) {
            terms$add(Term$new(term_data$count, term_data$value))
          } else if (verbose) {
            message("Skipping term_data due to missing keys: ", paste(names(term_data), collapse = ", "))
          }
        }
      }

      if (verbose) {
        message("Number of terms received from API: ", terms$length())
        message("")
        message("Terms from API (showing up to max):")
        shown <- terms$items[seq_len(min(max, terms$length()))]
        for (term in shown) {
          message("Value: ", term$get_value(), ", Count: ", term$get_count())
        }
      }

      terms
    }
  )
)

#' @keywords internal
#' @noRd
Term <- R6::R6Class(
  "Term",
  public = list(
    #' @field count Term occurrence count.
    count = NULL,
    #' @field value Term value.
    value = NULL,

    initialize = function(count, value) {
      self$count <- count
      self$value <- value
    },

    get_count = function() self$count,
    get_value = function() self$value
  )
)

#' @keywords internal
#' @noRd
Terms <- R6::R6Class(
  "Terms",
  public = list(
    items = list(),

    initialize = function() {
      self$items <- list()
    },

    add = function(term) {
      self$items <- c(self$items, list(term))
      invisible(self)
    },

    get_terms = function() self$items,

    get_next_terms = function(max = NULL) {
      if (is.null(max)) {
        if (length(self$items) == 0) {
          return(NULL)
        }
        self$items[[1]]
      } else {
        self$items[seq_len(min(max, length(self$items)))]
      }
    },

    length = function() length(self$items)
  )
)

#' Create a DAB Terms API client
#'
#' @param token GeoDAB token.
#' @param view GeoDAB view (e.g. \code{"blue-cloud-terms"}).
#' @return A [TermsAPI] R6 object.
#' @export
#' @examples
#' \dontrun{
#' api <- TermsAPI(token = "my-token", view = "blue-cloud-terms")
#' terms <- api$get_terms(type = "instrument", max = 10)
#' }
TermsAPI <- function(token, view) {
  TermsAPIClass$new(token = token, view = view)
}
