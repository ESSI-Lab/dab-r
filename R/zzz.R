#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  registerS3method("[[", "FeaturesCollection", `[[.FeaturesCollection`, envir = ns)
  registerS3method("[[", "ObservationsCollection", `[[.ObservationsCollection`, envir = ns)
  registerS3method("[[", "DownloadsCollection", `[[.DownloadsCollection`, envir = ns)
  registerS3method("[", "FeaturesCollection", `[.FeaturesCollection`, envir = ns)
  registerS3method("[", "ObservationsCollection", `[.ObservationsCollection`, envir = ns)
  registerS3method("[", "DownloadsCollection", `[.DownloadsCollection`, envir = ns)
  registerS3method("length", "FeaturesCollection", length.FeaturesCollection, envir = ns)
  registerS3method("length", "ObservationsCollection", length.ObservationsCollection, envir = ns)
  registerS3method("length", "DownloadsCollection", length.DownloadsCollection, envir = ns)
}
