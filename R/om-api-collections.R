#' @keywords internal
#' @noRd
FeaturesCollection <- R6::R6Class(
  "FeaturesCollection",
  public = list(
    client = NULL,
    constraints = NULL,
    features = NULL,
    current_page_features = NULL,
    resumption_token = NULL,
    completed = FALSE,
    page = 1,
    verbose = TRUE,

    initialize = function(
        client,
        constraints,
        initial_features = list(),
        resumption_token = NULL,
        page = 1,
        verbose = TRUE) {
      self$client <- client
      self$constraints <- constraints
      self$features <- initial_features
      self$current_page_features <- initial_features
      self$resumption_token <- resumption_token
      self$page <- page
      self$verbose <- verbose
      if (verbose) {
        self$print_summary(length(self$current_page_features))
      }
    },

    length = function() length(self$features),

    get_item = function(i) {
      i <- as.integer(i)[1]
      if (is.na(i) || i < 1L) {
        stop("Index must be a positive integer.", call. = FALSE)
      }
      if (i > length(self$features)) {
        stop("Index ", i, " out of range (length = ", length(self$features), ").",
             call. = FALSE)
      }
      self$features[[i]]
    },

    next_page = function() {
      if (self$completed || is.null(self$resumption_token)) {
        message("No more data to fetch.")
        return(invisible(self))
      }

      query <- constraints_to_query(self$constraints)
      token <- encode_query_value(self$resumption_token)
      url <- paste0(self$client$base_url, "features?", query, "&resumptionToken=", token)
      self$page <- self$page + 1L
      if (self$verbose) {
        message(
          "Retrieving page ", self$page, ": ",
          self$client$obfuscate_url(url)
        )
      }

      data <- dab_json(url, method = "GET")
      new_features <- lapply(data$results %||% list(), Feature$new)
      self$current_page_features <- new_features
      self$features <- c(self$features, new_features)
      self$resumption_token <- parse_resumption_token(data$resumptionToken)
      self$completed <- isTRUE(data$completed) || is.null(self$resumption_token)

      if (self$verbose) {
        self$print_summary(length(new_features))
      }
      invisible(self)
    },

    to_df = function() features_to_df(self),

    print_summary = function(n_returned) {
      prefix <- if (self$page == 1L) "first" else "next"
      msg <- paste0("Returned ", prefix, " ", n_returned, " features")
      if (self$completed) {
        message(msg, " (completed, data finished).")
      } else if (!is.null(self$resumption_token)) {
        message(
          msg,
          " (not completed, more data available).\n",
          "Use $next_page() to move to the next page."
        )
      } else {
        message(msg, " (completed, data finished).")
      }
    }
  )
)

#' @keywords internal
#' @noRd
ObservationsCollection <- R6::R6Class(
  "ObservationsCollection",
  public = list(
    client = NULL,
    constraints = NULL,
    observations = NULL,
    current_page_obs = NULL,
    resumption_token = NULL,
    completed = FALSE,
    page = 1,
    verbose = TRUE,

    initialize = function(
        client,
        constraints,
        initial_obs = list(),
        resumption_token = NULL,
        page = 1,
        verbose = TRUE) {
      self$client <- client
      self$constraints <- constraints
      self$observations <- initial_obs
      self$current_page_obs <- initial_obs
      self$resumption_token <- resumption_token
      self$page <- page
      self$verbose <- verbose
      if (verbose) {
        self$print_summary(length(self$current_page_obs))
      }
    },

    length = function() length(self$observations),

    get_item = function(i) {
      i <- as.integer(i)[1]
      if (is.na(i) || i < 1L) {
        stop("Index must be a positive integer.", call. = FALSE)
      }
      if (i > length(self$observations)) {
        stop("Index ", i, " out of range (length = ", length(self$observations), ").",
             call. = FALSE)
      }
      self$observations[[i]]
    },

    next_page = function() {
      if (self$completed || is.null(self$resumption_token)) {
        message("No more data to fetch.")
        return(invisible(self))
      }

      query <- constraints_to_query(self$constraints)
      token <- encode_query_value(self$resumption_token)
      url <- paste0(
        self$client$base_url,
        "observations?",
        query,
        "&resumptionToken=",
        token
      )
      self$page <- self$page + 1L
      if (self$verbose) {
        message(
          "Retrieving page ", self$page, ": ",
          self$client$obfuscate_url(url)
        )
      }

      data <- dab_json(url, method = "GET")
      new_obs <- lapply(data$member %||% list(), Observation$new)
      self$current_page_obs <- new_obs
      self$observations <- c(self$observations, new_obs)
      self$resumption_token <- parse_resumption_token(data$resumptionToken)
      self$completed <- isTRUE(data$completed) || is.null(self$resumption_token)

      if (self$verbose) {
        self$print_summary(length(new_obs))
      }
      invisible(self)
    },

    to_df = function() observations_to_df(self),

    to_df_all = function() observations_all_to_df(self),

    fetch_all_pages = function(max_pages = NULL) {
      pages_fetched <- 0L
      while (!self$completed) {
        if (!is.null(max_pages) && self$page >= max_pages) {
          message(
            "Stopped pagination at max_pages = ", max_pages,
            " (", length(self$observations), " observations so far)."
          )
          break
        }
        self$next_page()
        pages_fetched <- pages_fetched + 1L
      }
      invisible(self)
    },

    print_summary = function(n_returned) {
      prefix <- if (self$page == 1L) "first" else "next"
      msg <- paste0("Returned ", prefix, " ", n_returned, " observations")
      if (self$completed) {
        message(msg, " (completed, data finished).")
      } else if (!is.null(self$resumption_token)) {
        message(
          msg,
          " (not completed, more data available).\n",
          "Use $next_page() to move to the next page."
        )
      } else {
        message(msg, " (completed, data finished).")
      }
    }
  )
)

#' @keywords internal
#' @noRd
DownloadsCollection <- R6::R6Class(
  "DownloadsCollection",
  public = list(
    downloads = NULL,

    initialize = function(downloads_list = list()) {
      self$downloads <- downloads_list
    },

    length = function() length(self$downloads),

    get_item = function(i) {
      i <- as.integer(i)[1]
      if (is.na(i) || i < 1L) {
        stop("Index must be a positive integer.", call. = FALSE)
      }
      if (i > length(self$downloads)) {
        stop("Index ", i, " out of range (length = ", length(self$downloads), ").",
             call. = FALSE)
      }
      self$downloads[[i]]
    },

    to_df = function() downloads_to_df(self),

    print = function() {
      cat("<DownloadsCollection count=", length(self$downloads), ">\n", sep = "")
      invisible(self)
    }
  )
)
