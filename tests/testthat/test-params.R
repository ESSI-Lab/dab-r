test_that("params_from_json_array returns a list usable with $", {
  parameter <- list(
    list(name = "source", value = "HIS-Central"),
    list(name = "identifier", value = "station-1")
  )
  params <- dabr:::params_from_json_array(parameter)
  expect_type(params, "list")
  expect_equal(params$source, "HIS-Central")
  expect_equal(params$identifier, "station-1")
})

test_that("param_get works on named vectors from legacy conversion", {
  params <- c(source = "HIS-Central", identifier = "x")
  expect_equal(dabr:::param_get(params, "source"), "HIS-Central")
  expect_null(dabr:::param_get(params, "missing"))
})
