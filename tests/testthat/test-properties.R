test_that("PropertiesResult parses predefinedSearchArea JSON", {
  data <- list(
    predefinedSearchArea = list(
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
    "predefinedSearchArea",
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
    predefinedSearchArea = list(
      list(observationCount = 1, label = "A", value = "layer-a")
    ),
    resumptionToken = "token-2",
    completed = FALSE
  )
  page2 <- list(
    predefinedSearchArea = list(
      list(observationCount = 2, label = "B", value = "layer-b")
    ),
    completed = TRUE
  )

  result <- dabr:::PropertiesResult$new(
    client,
    "predefinedSearchArea",
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

test_that("constraints_to_query URL-encodes predefinedSearchArea", {
  value <- "opensearch://shapeFiles:selected-basins_Selected-Basins.1"
  c <- Constraints(predefinedSearchArea = value)
  q <- constraints_to_query(c)
  expect_false(grepl("://", q, fixed = TRUE))
  expect_true(grepl("predefinedSearchArea=", q, fixed = TRUE))
})

test_that("Constraints accepts predefinedLayer alias", {
  value <- "opensearch://layer/1"
  c <- Constraints(predefinedLayer = value)
  expect_equal(c$predefinedSearchArea, value)
  expect_true(grepl("predefinedSearchArea=", constraints_to_query(c), fixed = TRUE))
})

test_that("normalize_om_api_property maps predefined search area names", {
  expect_equal(
    dabr:::normalize_om_api_property(PREDEFINED_SEARCH_AREA),
    "predefinedSearchArea"
  )
  expect_equal(
    dabr:::normalize_om_api_property("predefinedLayer"),
    "predefinedSearchArea"
  )
  expect_equal(
    dabr:::normalize_om_api_property("country"),
    "country"
  )
})
