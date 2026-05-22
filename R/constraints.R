#' Build query constraints for DAB OM-API requests
#'
#' @description
#' Mirrors [dab-py constraints](https://github.com/ESSI-Lab/dab-r/tree/main/dab-py).
#' Only non-empty fields are included in the query string.
#'
#' @param bbox Numeric vector of length 4: south, west, north, east.
#' @param observedProperty,ontology,country,provider,feature,localFeatureIdentifier,
#'   observationIdentifier,beginPosition,endPosition,spatialRelation,predefinedLayer,
#'   timeInterpolation,intendedObservationSpacing,aggregationDuration,format
#'   Optional query parameters (character or numeric as appropriate).
#' @param limit Maximum number of results.
#'
#' @return An object of class \code{Constraints}.
#' @export
#' @examples
#' \dontrun{
#' c <- Constraints(bbox = c(60.398, 22.149, 60.690, 22.730))
#' constraints_to_query(c)
#' }
Constraints <- function(
    bbox = NULL,
    observedProperty = NULL,
    ontology = NULL,
    country = NULL,
    provider = NULL,
    feature = NULL,
    localFeatureIdentifier = NULL,
    observationIdentifier = NULL,
    beginPosition = NULL,
    endPosition = NULL,
    spatialRelation = NULL,
    predefinedLayer = NULL,
    timeInterpolation = NULL,
    intendedObservationSpacing = NULL,
    aggregationDuration = NULL,
    limit = NULL,
    format = NULL) {
  structure(
    list(
      bbox = bbox,
      observedProperty = observedProperty,
      ontology = ontology,
      country = country,
      provider = provider,
      feature = feature,
      localFeatureIdentifier = localFeatureIdentifier,
      observationIdentifier = observationIdentifier,
      beginPosition = beginPosition,
      endPosition = endPosition,
      spatialRelation = spatialRelation,
      predefinedLayer = predefinedLayer,
      timeInterpolation = timeInterpolation,
      intendedObservationSpacing = intendedObservationSpacing,
      aggregationDuration = aggregationDuration,
      limit = limit,
      format = format
    ),
    class = "Constraints"
  )
}

#' Build a query string from a Constraints object
#'
#' @param constraints A [Constraints()] object.
#' @return Query string without leading \code{?}.
#' @export
constraints_to_query <- function(constraints) {
  if (!inherits(constraints, "Constraints")) {
    stop("Expected a Constraints object.", call. = FALSE)
  }
  parts <- character()

  if (!is.null(constraints$bbox)) {
    if (length(constraints$bbox) != 4) {
      stop("bbox must have length 4: south, west, north, east.", call. = FALSE)
    }
    south <- constraints$bbox[1]
    west <- constraints$bbox[2]
    north <- constraints$bbox[3]
    east <- constraints$bbox[4]
    parts <- c(
      parts,
      paste0("west=", west),
      paste0("south=", south),
      paste0("east=", east),
      paste0("north=", north)
    )
  }

  scalar_fields <- c(
    "observedProperty", "ontology", "country", "provider", "feature",
    "localFeatureIdentifier", "observationIdentifier", "beginPosition",
    "endPosition", "spatialRelation", "predefinedLayer", "timeInterpolation",
    "intendedObservationSpacing", "aggregationDuration", "format"
  )
  for (field in scalar_fields) {
    value <- constraints[[field]]
    if (!is.null(value) && nzchar(as.character(value))) {
      parts <- c(parts, paste0(field, "=", value))
    }
  }

  if (!is.null(constraints$limit)) {
    parts <- c(parts, paste0("limit=", constraints$limit))
  }

  paste(parts, collapse = "&")
}

#' Download-specific constraints for asynchronous OM-API downloads
#'
#' @param base_constraints Optional existing \code{Constraints} object to copy from.
#' @param asynchDownloadName Name for the asynchronous download (required for PUT).
#' @param eMailNotifications Logical; encoded as \code{true}/\code{false}.
#' @param useCache Logical; encoded as \code{true}/\code{false}.
#' @param ... Arguments passed to [Constraints()] when \code{base_constraints} is
#'   \code{NULL}.
#' @return An object of class \code{c("DownloadConstraints", "Constraints")}.
#' @export
DownloadConstraints <- function(
    base_constraints = NULL,
    asynchDownloadName = NULL,
    eMailNotifications = NULL,
    useCache = NULL,
    ...) {
  if (!is.null(base_constraints)) {
    if (!inherits(base_constraints, "Constraints")) {
      stop("base_constraints must be a Constraints object.", call. = FALSE)
    }
    base <- base_constraints
  } else {
    base <- Constraints(...)
  }
  structure(
    c(
      base,
      list(
        asynchDownloadName = asynchDownloadName,
        eMailNotifications = eMailNotifications,
        useCache = useCache
      )
    ),
    class = c("DownloadConstraints", "Constraints")
  )
}

#' Build a query string from a DownloadConstraints object
#'
#' @param download_constraints A [DownloadConstraints()] object.
#' @return Query string without leading \code{?}.
#' @export
download_constraints_to_query <- function(download_constraints) {
  if (!inherits(download_constraints, "DownloadConstraints")) {
    stop("Expected a DownloadConstraints object.", call. = FALSE)
  }
  query <- constraints_to_query(download_constraints)
  extra <- character()

  if (!is.null(download_constraints$asynchDownloadName) &&
      nzchar(download_constraints$asynchDownloadName)) {
    extra <- c(
      extra,
      paste0("asynchDownloadName=", download_constraints$asynchDownloadName)
    )
  }
  if (!is.null(download_constraints$eMailNotifications)) {
    extra <- c(
      extra,
      paste0(
        "eMailNotifications=",
        tolower(as.character(download_constraints$eMailNotifications))
      )
    )
  }
  if (!is.null(download_constraints$useCache)) {
    extra <- c(
      extra,
      paste0("useCache=", tolower(as.character(download_constraints$useCache)))
    )
  }

  if (length(extra) == 0) {
    return(query)
  }
  if (nzchar(query)) {
    paste(query, paste(extra, collapse = "&"), sep = "&")
  } else {
    paste(extra, collapse = "&")
  }
}
