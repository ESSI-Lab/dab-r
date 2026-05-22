test_that("Observation to_list includes extended OM-JSON fields", {
  obs_json <- list(
    id = "258FEB298491C727A6029B40FF6D5B874D59D669",
    type = "TimeSeriesObservation",
    parameter = list(
      list(name = "source", value = "Regione Valle d'Aosta"),
      list(name = "sourceId", value = "ita-sir-val-d-aosta"),
      list(
        name = "observedPropertyDefinition",
        value = "Precipitazione ufficiale (mm)"
      ),
      list(name = "originalObservedProperty", value = "Precipitazione ufficiale")
    ),
    observedProperty = list(
      href = "http://his-central-ontology.geodab.eu/hydro-ontology/concept/65",
      title = "Precipitation"
    ),
    phenomenonTime = list(
      begin = "2009-10-05T00:00:00Z",
      end = "2026-05-22T13:00:48Z"
    ),
    featureOfInterest = list(
      href = "19DA5591755012357F277196E9665D2CFECF3B29",
      title = "Ayas - Alpe Aventine"
    ),
    result = list(
      defaultPointMetadata = list(uom = "mm"),
      points = list()
    )
  )
  obs <- dabr:::Observation$new(obs_json)
  row <- obs$to_list()

  expect_equal(row$`Source ID`, "ita-sir-val-d-aosta")
  expect_equal(row$`Observed Property Definition`, "Precipitazione ufficiale (mm)")
  expect_equal(row$`Original Observed Property`, "Precipitazione ufficiale")
  expect_equal(row$`Feature Of Interest`, "Ayas - Alpe Aventine")
  expect_equal(row$UOM, "mm")
  expect_equal(row$Type, "TimeSeriesObservation")

  coll <- dabr:::ObservationsCollection$new(
    NULL,
    Constraints(),
    initial_obs = list(obs)
  )
  df <- observations_to_df(coll)
  expect_true("Feature Of Interest" %in% names(df))
  expect_equal(df$`Source ID`[1], "ita-sir-val-d-aosta")
})
