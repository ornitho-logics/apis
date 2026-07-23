test_that("ecotopia_data_by_day returns a null data.table for an empty body", {
  local_mocked_bindings(
    req_perform = function(req) httr2::response(status_code = 204),
    .package = "apis"
  )

  result <- ecotopia_data_by_day(
    "token",
    "tag-1",
    "2023-01-02",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(dim(result), c(0, 0))
})

test_that("ecotopia_data_by_day treats HTTP 404 as an empty result", {
  local_mocked_bindings(
    req_perform = function(req) {
      response <- httr2::response(
        status_code = 404,
        headers = list("content-type" = "text/plain"),
        body = charToRaw("Not Found")
      )

      expect_false(req$policies$error_is_error(response))
      response
    },
    .package = "apis"
  )

  result <- ecotopia_data_by_day(
    "token",
    "tag-1",
    "2023-01-02",
    what = "sms",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(dim(result), c(0, 0))
})

test_that("ecotopia_data_by_day parses a populated body", {
  local_mocked_bindings(
    req_perform = function(req) {
      httr2::response(
        headers = list("content-type" = "application/json"),
        body = charToRaw('[{"id":"row-1"}]')
      )
    },
    .package = "apis"
  )

  result <- ecotopia_data_by_day(
    "token",
    "tag-1",
    "2023-01-02",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(result$id, "row-1")
})

test_that("ecotopia_devlist follows the count and device ID cursor", {
  json_response <- function(body) {
    httr2::response(
      headers = list("content-type" = "application/json"),
      body = charToRaw(body)
    )
  }

  local_mocked_bindings(
    req_perform = function(req) {
      url <- as.character(req$url)

      if (endsWith(url, "/device/count")) {
        return(httr2::response(
          headers = list("content-type" = "text/plain"),
          body = charToRaw("3")
        ))
      }

      expect_equal(req$headers[["X-result-sort"]], "-_id")

      if (endsWith(url, "/device/page")) {
        return(json_response('[{"id":"tag-c"},{"id":"tag-b"}]'))
      }

      if (endsWith(url, "/device/page/tag-b")) {
        return(json_response('[{"id":"tag-a"}]'))
      }

      stop(glue::glue("Unexpected request: {url}"))
    },
    .package = "apis"
  )

  result <- ecotopia_devlist("token", verbose = FALSE)

  expect_equal(result$id, c("tag-c", "tag-b", "tag-a"))
})

test_that("ecotopia_data_all_by_id includes the start date through today", {
  local_mocked_bindings(
    ecotopia_data_by_day = function(logstring, id, date, what, verbose) {
      data.table::data.table(
        device_id = id,
        query_date = as.character(date),
        value = what
      )
    },
    .package = "apis"
  )

  start_date <- Sys.Date() - 1
  result <- ecotopia_data_all_by_id(
    "token",
    id = "tag-1",
    start_date = start_date,
    what = "gps",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(result$device_id, c("tag-1", "tag-1"))
  expect_equal(
    result$query_date,
    as.character(seq.Date(start_date, Sys.Date(), by = "day"))
  )
  expect_equal(unique(result$value), "gps")
})
