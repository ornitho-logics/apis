kineis_json_response <- function(body) {
  httr2::response(
    headers = list("content-type" = "application/json"),
    body = charToRaw(body)
  )
}


test_that("kineis_data follows bulk pagination and preserves long IDs", {
  requested_cursors <- list()

  local_mocked_bindings(
    req_perform = function(req) {
      payload <- req$body$data
      requested_cursors[[length(requested_cursors) + 1]] <<- if (
        is.null(payload$pagination$after)
      ) {
        NA_character_
      } else {
        payload$pagination$after
      }

      expect_equal(
        as.character(req$url),
        "https://api.example/telemetry/api/v1/retrieve-bulk"
      )
      expect_equal(payload$pagination$first, 2L)
      expect_equal(payload$deviceRefs, list("device-a"))
      expect_equal(
        payload$fromDatetime,
        "2026-07-01T00:00:00.000Z"
      )
      expect_equal(
        payload$toDatetime,
        "2026-07-02T00:00:00.000Z"
      )
      expect_true(payload$retrieveAdditionnalProperties)

      if (is.null(payload$pagination$after)) {
        return(kineis_json_response(
          paste0(
            '{"contents":[',
            '{"deviceMsgUid":9223372036854775806,',
            '"deviceRef":"device-a",',
            '"kineisMetadata":{"sat":"1A"},',
            '"sensors":{"TEMP":"7.6"}}',
            '],"pageInfo":{"hasNextPage":true,"endCursor":"0"}}'
          )
        ))
      }

      kineis_json_response(
        paste0(
          '{"contents":[',
          '{"deviceMsgUid":9223372036854775805,',
          '"deviceRef":"device-a",',
          '"kineisMetadata":{"sat":"1B"},',
          '"sensors":{"POWER":"5.2"}}',
          '],"pageInfo":{"hasNextPage":false,"endCursor":"1"}}'
        )
      )
    },
    .package = "apis"
  )

  result <- kineis_data(
    token = "secret-token",
    api_telemetry_url = "https://api.example/telemetry/api/v1/",
    datetime = "2026-07-01T00:00:00.000Z",
    end_datetime = "2026-07-02T00:00:00.000Z",
    device_refs = "device-a",
    page_size = 2,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(
    result$deviceMsgUid,
    c("9223372036854775806", "9223372036854775805")
  )
  expect_equal(result$kineisMetadata.sat, c("1A", "1B"))
  expect_equal(result$sensors.TEMP, c("7.6", NA))
  expect_equal(result$sensors.POWER, c(NA, "5.2"))
  expect_true(is.na(requested_cursors[[1]]))
  expect_equal(requested_cursors[[2]], "0")
})


test_that("kineis_devlist requests and parses the device list", {
  local_mocked_bindings(
    req_perform = function(req) {
      expect_equal(
        as.character(req$url),
        "https://api.example/telemetry/api/v1/retrieve-device-list"
      )
      expect_equal(rawToChar(req$body$data), "{}")
      kineis_json_response(
        '[{"deviceUid":123,"deviceRef":"device-a"}]'
      )
    },
    .package = "apis"
  )

  result <- kineis_devlist(
    list(access_token = "secret-token"),
    "https://api.example/telemetry/api/v1",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(result$deviceUid, "123")
  expect_equal(result$deviceRef, "device-a")
})


test_that("kineis_data returns an empty data.table", {
  local_mocked_bindings(
    req_perform = function(req) {
      kineis_json_response(
        '{"contents":[],"pageInfo":{"hasNextPage":false}}'
      )
    },
    .package = "apis"
  )

  result <- kineis_data(
    "secret-token",
    "https://api.example/telemetry/api/v1",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(dim(result), c(0, 0))
})


test_that("kineis_data rejects a stalled pagination cursor", {
  local_mocked_bindings(
    req_perform = function(req) {
      kineis_json_response(
        paste0(
          '{"contents":[],"pageInfo":',
          '{"hasNextPage":true,"endCursor":null}}'
        )
      )
    },
    .package = "apis"
  )

  expect_error(
    kineis_data(
      "secret-token",
      "https://api.example/telemetry/api/v1",
      verbose = FALSE
    ),
    "returned no new cursor"
  )
})


test_that("verbose Kineis requests redact the bearer token", {
  req <- httr2::request("https://api.example") |>
    httr2::req_headers(Authorization = "Bearer secret-token")

  output <- capture.output(.kineis_print_request(req))

  expect_false(any(grepl("secret-token", output, fixed = TRUE)))
  expect_true(any(grepl("<REDACTED>", output, fixed = TRUE)))
})
