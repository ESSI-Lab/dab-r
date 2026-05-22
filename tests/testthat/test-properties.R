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
  client <- list(base_url = "https://x/", obfuscate_url = function(url) url)
  result <- dabr:::PropertiesResult$new(
    client,
    "predefinedLayer",
    data,
    verbose = FALSE
  )
  expect_equal(length(result$entries), 2L)
  expect_equal(result$get_item(1)$label, "Sesia alto fiume")
  expect_equal(
    result$get_item(2)$value,
    "opensearch://shapeFiles:selected-basins_Selected-Basins.3"
  )
  df <- properties_all_to_df(result)
  expect_equal(nrow(df), 2L)
  expect_equal(nrow(properties_to_df(result)), 2L)
})

test_that("PropertiesResult accumulates multiple pages", {
  client <- list(
    base_url = "https://example.test/om-api/",
    obfuscate_url = function(url) url
  )
  page1 <- list(
    predefinedLayer = list(
      list(observationCount = 1, label = "A", value = "layer-a")
    ),
    resumptionToken = "token-2",
    completed = FALSE
  )
  page2 <- list(
    predefinedLayer = list(
      list(observationCount = 2, label = "B", value = "layer-b")
    ),
    completed = TRUE
  )

  result <- dabr:::PropertiesResult$new(
    client,
    "predefinedLayer",
    page1,
    limit = 1L,
    verbose = FALSE
  )
  result$append_page(page2)
  expect_equal(length(result$entries), 2L)
  expect_true(result$completed)
  df <- properties_all_to_df(result)
  expect_equal(df$Label, c("A", "B"))
})

test_that("constraints_to_query URL-encodes predefinedLayer", {
  value <- "opensearch://shapeFiles:selected-basins_Selected-Basins.1"
  c <- Constraints(predefinedLayer = value)
  q <- constraints_to_query(c)
  expect_false(grepl("://", q, fixed = TRUE))
  expect_true(grepl("predefinedLayer=", q, fixed = TRUE))
})
