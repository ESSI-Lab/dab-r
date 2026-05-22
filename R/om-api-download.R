#' Download observations as a binary file (e.g. shapefile archive)
#'
#' @keywords internal
#' @noRd
fetch_observations_download <- function(
    client,
    constraints,
    save_path = NULL,
    unzip = TRUE,
    verbose = TRUE) {
  url <- paste0(client$base_url, "observations?", constraints_to_query(constraints))
  if (verbose) {
    message("Downloading observations: ", client$obfuscate_url(url))
  }
  zip_path <- dab_download_binary(url, save_path = save_path)
  if (!unzip) {
    return(list(zip_path = zip_path, extract_dir = NULL, shp_path = NULL))
  }
  unzip_shapefile_archive(zip_path)
}

#' Download observations (e.g. as a shapefile)
#'
#' Requests the OM-API \code{observations} endpoint with constraints such as
#' \code{format = "SHAPEFILE"} and \code{includeData = FALSE}. The response is
#' saved as a file (typically a ZIP archive) and, by default, extracted to locate
#' the \code{.shp} file.
#'
#' Call as \code{client$download_observations(constraints, save_path, unzip, verbose)}
#' on a [DABClient], [WHOSClient], or [HISCentralClient] object.
#'
#' @param constraints A [Constraints()] object including \code{format} and
#'   \code{includeData} as needed.
#' @param save_path Optional path for the downloaded archive. A temporary file
#'   is used when omitted.
#' @param unzip If \code{TRUE}, unzip the archive and locate a \code{.shp} file.
#' @param verbose Print the request URL (token obfuscated).
#'
#' @return A list with \code{zip_path}, and when \code{unzip = TRUE} also
#'   \code{extract_dir} and \code{shp_path}.
#'
#' @examples
#' \dontrun{
#' client <- HISCentralClient(token = "my-token")
#' layers <- client$get_properties("predefinedLayer", limit = 10)
#' dl <- client$download_observations(Constraints(
#'   predefinedLayer = layers$get_item(1)$value,
#'   beginPosition = "2026-04-01T00:00:00Z",
#'   endPosition = "2026-04-30T23:59:59Z",
#'   format = "SHAPEFILE",
#'   includeData = FALSE
#' ))
#' }
#'
#' @seealso [Constraints()]
#' @name download_observations
NULL
