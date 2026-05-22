#' @keywords internal
#' @noRd
.null <- function(...) invisible(NULL)

#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @keywords internal
#' @noRd
format_om_api_base_url <- function(template, token, view) {
  if (identical(token, "{token}") || identical(view, "{view}")) {
    return(template)
  }
  url <- gsub("{view}", as.character(view), template, fixed = TRUE)
  gsub("{token}", as.character(token), url, fixed = TRUE)
}

#' @keywords internal
#' @noRd
encode_query_value <- function(x) {
  utils::URLencode(as.character(x), reserved = TRUE)
}

#' @keywords internal
#' @noRd
parse_resumption_token <- function(token) {
  if (is.null(token) || !nzchar(token)) {
    return(NULL)
  }
  strsplit(token, ",", fixed = TRUE)[[1]][1]
}

#' @keywords internal
#' @noRd
dab_request <- function(url, method = c("GET", "PUT", "DELETE")) {
  method <- match.arg(method)
  req <- switch(
    method,
    GET = httr2::request(url) |> httr2::req_method("GET"),
    PUT = httr2::request(url) |> httr2::req_method("PUT"),
    DELETE = httr2::request(url) |> httr2::req_method("DELETE")
  )
  resp <- httr2::req_perform(req)
  httr2::resp_check_status(resp)
  resp
}

#' @keywords internal
#' @noRd
dab_json <- function(url, method = "GET") {
  resp <- dab_request(url, method = method)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' @keywords internal
#' @noRd
params_from_json_array <- function(parameter) {
  if (is.null(parameter) || length(parameter) == 0) {
    return(list())
  }
  stats::setNames(
    vapply(parameter, function(p) p$value, character(1)),
    vapply(parameter, function(p) p$name, character(1))
  )
}
