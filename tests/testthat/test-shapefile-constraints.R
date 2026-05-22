test_that("constraints_to_query supports shapefile download parameters", {
  c <- Constraints(
    predefinedSearchArea = "opensearch://layer/1",
    beginPosition = "2026-04-01T00:00:00Z",
    endPosition = "2026-04-30T23:59:59Z",
    format = "SHAPEFILE",
    includeData = FALSE
  )
  q <- constraints_to_query(c)
  expect_match(q, "format=SHAPEFILE", fixed = TRUE)
  expect_match(q, "includeData=false", fixed = TRUE)
  expect_match(q, "predefinedSearchArea=", fixed = TRUE)
})

test_that("unzip_shapefile_archive finds shp in zip", {
  skip_if_not_installed <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      skip(paste("requires", pkg))
    }
  }
  skip_if_not_installed("sf")
  td <- tempfile("dabr_zip_test_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  shp_dir <- file.path(td, "layer")
  dir.create(shp_dir)
  pt <- sf::st_sf(
    id = "a",
    geometry = sf::st_sfc(sf::st_point(c(12.4, 41.8)), crs = 4326)
  )
  shp_path <- file.path(shp_dir, "obs.shp")
  sf::st_write(pt, shp_path, quiet = TRUE)
  zip_path <- file.path(td, "obs.zip")
  shp_files <- list.files(shp_dir, full.names = TRUE)
  owd <- setwd(shp_dir)
  on.exit(setwd(owd), add = TRUE)
  utils::zip(zipfile = zip_path, files = basename(shp_files))

  out <- dabr:::unzip_shapefile_archive(zip_path, extract_dir = file.path(td, "out"))
  expect_true(file.exists(out$shp_path))
})
