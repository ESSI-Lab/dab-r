test_that("Constraints builds bbox query in correct order", {
  c <- Constraints(bbox = c(60.398, 22.149, 60.690, 22.730))
  q <- constraints_to_query(c)
  expect_equal(
    q,
    "west=22.149&south=60.398&east=22.73&north=60.69"
  )
})

test_that("DownloadConstraints adds download fields", {
  dc <- DownloadConstraints(
    bbox = c(41.777, 12.392, 41.832, 12.456),
    asynchDownloadName = "example",
    eMailNotifications = TRUE,
    useCache = FALSE
  )
  q <- download_constraints_to_query(dc)
  expect_match(q, "asynchDownloadName=example")
  expect_match(q, "eMailNotifications=true")
  expect_match(q, "useCache=false")
})

test_that("DABClient formats base URL", {
  client <- DABClient(token = "tok", view = "whos")
  expect_match(client$base_url, "/token/tok/view/whos/om-api/")
})

test_that("WHOSClient uses WHOS host", {
  client <- WHOSClient(token = "tok")
  expect_match(client$base_url, "whos.geodab.eu")
})
