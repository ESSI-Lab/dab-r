test_that("ObservationsCollection supports [[ indexing via S3", {
  obs_json <- list(
    id = "obs-1",
    parameter = list(list(name = "source", value = "test")),
    observedProperty = list(title = "Temperature"),
    phenomenonTime = list(begin = "2020-01-01T00:00:00Z", end = "2020-12-31T00:00:00Z"),
    result = list(points = list())
  )
  coll <- dab.r:::ObservationsCollection$new(
    client = NULL,
    constraints = Constraints(),
    initial_obs = list(dab.r:::Observation$new(obs_json))
  )
  expect_equal(coll[[1]]$id, "obs-1")
  expect_equal(coll$get_item(1)$id, "obs-1")
  expect_equal(length(coll), 1L)
})

test_that("observations_all_to_df combines all paginated observations", {
  obs_json <- function(id) {
    list(
      id = id,
      parameter = list(list(name = "source", value = "test")),
      observedProperty = list(title = "Temperature"),
      phenomenonTime = list(begin = "2020-01-01T00:00:00Z", end = "2020-12-31T00:00:00Z"),
      result = list(points = list())
    )
  }
  coll <- dab.r:::ObservationsCollection$new(
    NULL,
    Constraints(),
    initial_obs = list(dab.r:::Observation$new(obs_json("obs-1")))
  )
  coll$observations <- c(
    coll$observations,
    list(dab.r:::Observation$new(obs_json("obs-2")))
  )
  coll$current_page_obs <- list(dab.r:::Observation$new(obs_json("obs-2")))
  df <- observations_all_to_df(coll)
  expect_equal(nrow(df), 2L)
  expect_equal(df$ID, c("obs-1", "obs-2"))
})
