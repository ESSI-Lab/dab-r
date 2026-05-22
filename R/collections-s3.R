#' @keywords internal
#' @noRd
collection_get_item <- function(x, i) {
  x$get_item(i)
}

`[[.FeaturesCollection` <- function(x, i, ...) {
  collection_get_item(x, i)
}

`[[.ObservationsCollection` <- function(x, i, ...) {
  collection_get_item(x, i)
}

`[[.DownloadsCollection` <- function(x, i, ...) {
  collection_get_item(x, i)
}

`[.FeaturesCollection` <- function(x, i, ...) {
  collection_get_item(x, i)
}

`[.ObservationsCollection` <- function(x, i, ...) {
  collection_get_item(x, i)
}

`[.DownloadsCollection` <- function(x, i, ...) {
  collection_get_item(x, i)
}

length.FeaturesCollection <- function(x) {
  x$length()
}

length.ObservationsCollection <- function(x) {
  x$length()
}

length.DownloadsCollection <- function(x) {
  x$length()
}
