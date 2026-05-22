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
normalize_om_api_property <- function(property) {
  if (is.null(property) || !nzchar(property)) {
    return(property)
  }
  p <- as.character(property)[1]
  if (p %in% c(
    PREDEFINED_SEARCH_AREA,
    OM_API_PREDEFINED_SEARCH_AREA_PROPERTY,
    "predefinedLayer"
  )) {
    OM_API_PREDEFINED_SEARCH_AREA_PROPERTY
  } else {
    p
  }
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
dab_download_binary <- function(url, save_path = NULL) {
  resp <- httr2::req_perform(httr2::request(url) |> httr2::req_method("GET"))
  httr2::resp_check_status(resp)
  content_type <- httr2::resp_content_type(resp) %||% ""
  if (grepl("json", content_type, ignore.case = TRUE)) {
    body <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) "<unable to read body>"
    )
    stop(
      "Expected a binary file (e.g. shapefile archive), but the API returned JSON:\n",
      body,
      call. = FALSE
    )
  }
  if (is.null(save_path) || !nzchar(save_path)) {
    ext <- if (grepl("zip", content_type, ignore.case = TRUE)) ".zip" else ".bin"
    save_path <- tempfile(fileext = ext)
  }
  dir <- dirname(save_path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  writeBin(httr2::resp_body_raw(resp), save_path)
  save_path
}

#' @keywords internal
#' @noRd
unzip_shapefile_archive <- function(zip_path, extract_dir = NULL) {
  if (is.null(extract_dir)) {
    extract_dir <- tempfile("dabr_shp_")
    dir.create(extract_dir, recursive = TRUE)
  }
  utils::unzip(zip_path, exdir = extract_dir)
  shp_files <- list.files(
    extract_dir,
    pattern = "\\.shp$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(shp_files) == 0) {
    stop("No .shp file found in archive: ", zip_path, call. = FALSE)
  }
  list(
    zip_path = zip_path,
    extract_dir = extract_dir,
    shp_path = shp_files[1]
  )
}

#' @keywords internal
#' @noRd
params_from_json_array <- function(parameter) {
  if (is.null(parameter) || length(parameter) == 0) {
    return(list())
  }
  if (is.data.frame(parameter)) {
    if (!all(c("name", "value") %in% names(parameter))) {
      return(list())
    }
    return(as.list(setNames(
      as.character(parameter$value),
      as.character(parameter$name)
    )))
  }
  if (is.list(parameter) && !is.null(parameter$name) && !is.null(parameter$value) &&
      !is.list(parameter$name)) {
    return(as.list(setNames(
      as.character(parameter$value),
      as.character(parameter$name)
    )))
  }
  if (!is.list(parameter)) {
    return(list())
  }
  as.list(setNames(
    vapply(parameter, function(p) as.character(p$value %||% ""), character(1)),
    vapply(parameter, function(p) as.character(p$name %||% ""), character(1))
  ))
}

#' @keywords internal
#' @noRd
param_get <- function(params, name, default = NULL) {
  if (length(params) == 0 || is.null(name) || !name %in% names(params)) {
    return(default)
  }
  value <- params[[name]]
  if (is.null(value) || (length(value) == 1L && is.na(value))) default else value
}

#' @keywords internal
#' @noRd
json_list_field <- function(obj, field, default = NULL) {
  if (is.null(obj) || !is.list(obj)) {
    return(default)
  }
  value <- obj[[field]]
  if (is.null(value) || (length(value) == 1L && is.na(value))) default else value
}

#' @keywords internal
#' @noRd
parse_observation_point <- function(p) {
  if (is.null(p) || !is.list(p)) {
    return(NULL)
  }
  instant <- NA_character_
  if (!is.null(p$time)) {
    if (is.list(p$time)) {
      instant <- as.character(p$time$instant %||% p$time$time %||% NA_character_)
    } else {
      instant <- as.character(p$time)
    }
  }
  value <- NA_real_
  if (!is.null(p$value)) {
    num <- suppressWarnings(as.numeric(p$value))
    if (length(num) > 0L && !is.na(num[1L])) {
      value <- num[1L]
    }
  }
  list(Time = instant, Value = value)
}

#' @keywords internal
#' @noRd
observation_points_list <- function(observation) {
  if (is.null(observation) || is.null(observation$points)) {
    return(list())
  }
  points <- observation$points
  if (!is.list(points)) {
    return(list())
  }
  parsed <- lapply(points, parse_observation_point)
  Filter(Negate(is.null), parsed)
}
