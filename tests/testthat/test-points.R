test_that("points_to_df handles missing value without data.frame error", {
  obs <- dabr:::Observation$new(list(
    id = "obs-1",
    parameter = list(),
    result = list(
      points = list(
        list(time = list(instant = "2026-04-01T12:00:00Z"), value = 1.5),
        list(time = list(instant = "2026-04-02T12:00:00Z")),
        list(time = list(instant = "2026-04-03T12:00:00Z"), value = NULL)
      )
    )
  ))
  df <- points_to_df(obs)
  expect_equal(nrow(df), 3L)
  expect_equal(df$Value[1], 1.5)
  expect_true(is.na(df$Value[2]))
  expect_true(is.na(df$Value[3]))
})

test_that("points_to_df returns empty frame when no valid points", {
  obs <- dabr:::Observation$new(list(
    id = "obs-1",
    parameter = list(),
    result = list(points = list(NULL))
  ))
  df <- points_to_df(obs)
  expect_equal(nrow(df), 0L)
})
