test_that("PropertiesResult parses predefinedLayer JSON", {
  data <- list(
    predefinedLayer = list(
      list(
        observationCount = 1,
        label = "Sesia alto fiume",
        value = "opensearch://shapeFiles:selected-basins_Selected-Basins.1"
      ),
      list(
        observationCount = 2,
        label = "Torrente Lys",
        value = "opensearch://shapeFiles:selected-basins_Selected-Basins.3"
      )
    ),
    completed = TRUE
  )
  result <- dabr:::PropertiesResult$new(data, property = "predefinedLayer")
  expect_equal(length(result$entries), 2L)
  expect_equal(result$get_item(1)$label, "Sesia alto fiume")
  expect_equal(
    result$get_item(2)$value,
    "opensearch://shapeFiles:selected-basins_Selected-Basins.3"
  )
  df <- properties_to_df(result)
  expect_equal(nrow(df), 2L)
})

test_that("constraints_to_query URL-encodes predefinedLayer", {
  value <- "opensearch://shapeFiles:selected-basins_Selected-Basins.1"
  c <- Constraints(predefinedLayer = value)
  q <- constraints_to_query(c)
  expect_false(grepl("://", q, fixed = TRUE))
  expect_true(grepl("predefinedLayer=", q, fixed = TRUE))
})
