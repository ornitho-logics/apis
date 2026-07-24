json_response <- function(body) {
  httr2::response(
    headers = list("content-type" = "application/json"),
    body = charToRaw(body)
  )
}


test_that("verbose requests redact the Ecotopia token", {
  req <- httr2::request("https://www.ecotopiago.com") |>
    httr2::req_headers("X-Druid-Authentication" = "secret-token")

  output <- capture.output(.ecotopia_print_request(req))

  expect_false(any(grepl("secret-token", output, fixed = TRUE)))
  expect_true(any(grepl("<redacted>", output, fixed = TRUE)))
})


test_that("ecotopia_device_page requests a cursor page", {
  local_mocked_bindings(
    req_perform = function(req) {
      expect_equal(
        as.character(req$url),
        "https://www.ecotopiago.com/api/v3/device/page/tag-1"
      )
      expect_equal(req$headers[["X-result-limit"]], 1000)
      expect_equal(req$headers[["X-result-sort"]], "-_id")
      json_response('[{"id":"tag-2"}]')
    },
    .package = "apis"
  )

  result <- ecotopia_device_page(
    "token",
    cursor = "tag-1",
    verbose = FALSE
  )

  expect_equal(result$id, "tag-2")
})


test_that("Ecotopia data aliases select the intended API routes", {
  expect_equal(.ecotopia_data_type("gps"), "gps")
  expect_equal(.ecotopia_data_type("env"), "env")
  expect_equal(.ecotopia_data_type("sms"), "sms")
  expect_equal(.ecotopia_data_type("odba"), "behavior2")
  expect_equal(.ecotopia_data_type("behavior2"), "behavior2")
  expect_equal(.ecotopia_data_type("structured"), "structured")
  expect_equal(.ecotopia_data_type("behavior"), "structured")
  expect_equal(.ecotopia_data_type("behaviour"), "structured")
  expect_equal(.ecotopia_data_type("celltower"), "celltower")
  expect_equal(.ecotopia_data_type("origin"), "origin")
  expect_equal(.ecotopia_data_type("beacon"), "beacon")
})


test_that("ecotopia_data paginates structured data and preserves raw fields", {
  requested_urls <- character()

  local_mocked_bindings(
    req_perform = function(req) {
      requested_urls <<- c(requested_urls, as.character(req$url))

      expect_equal(req$headers[["X-result-limit"]], 1000)
      expect_equal(req$headers[["X-result-sort"]], "timestamp")

      if (grepl("T00:00:00Z$", req$url)) {
        return(json_response(
          '[{"id":"record-1","device_id":"tag-1","timestamp":"2026-01-01T01:00:00Z","algorithm":"feeding","confidence":0.95},{"id":"record-2","device_id":"tag-1","timestamp":"2026-01-01T02:00:00Z","algorithm":"walking","confidence":0.80}]'
        ))
      }

      if (grepl("T02:00:00Z$", req$url)) {
        return(json_response(
          '[{"id":"record-2","device_id":"tag-1","timestamp":"2026-01-01T02:00:00Z","algorithm":"walking","confidence":0.80},{"id":"record-3","device_id":"tag-1","timestamp":"2026-01-01T03:00:00Z","algorithm":"resting","confidence":0.99}]'
        ))
      }

      json_response("[]")
    },
    .package = "apis"
  )

  result <- ecotopia_data(
    "token",
    "tag-1",
    datetime = "2026-01-01T00:00:00Z",
    what = "structured",
    verbose = FALSE
  )

  expect_length(requested_urls, 3)
  expect_true(all(grepl(
    "/api/v2/structured/device/tag-1/page/",
    requested_urls,
    fixed = TRUE
  )))
  expect_equal(result$id, c("record-1", "record-2", "record-3"))
  expect_equal(result$algorithm, c("feeding", "walking", "resting"))
  expect_equal(result$confidence, c(0.95, 0.80, 0.99))
})


test_that("ecotopia_data uses an exclusive end timestamp", {
  local_mocked_bindings(
    req_perform = function(req) {
      json_response(
        '[{"id":"record-1","timestamp":"2026-01-01T01:00:00Z"},{"id":"record-2","timestamp":"2026-01-01T02:00:00Z"}]'
      )
    },
    .package = "apis"
  )

  result <- ecotopia_data(
    "token",
    "tag-1",
    datetime = "2026-01-01T00:00:00Z",
    what = "structured",
    verbose = FALSE,
    end_datetime = "2026-01-01T02:00:00Z"
  )

  expect_equal(result$id, "record-1")
})


test_that("ecotopia_data returns an empty data.table for an empty page", {
  local_mocked_bindings(
    req_perform = function(req) json_response("[]"),
    .package = "apis"
  )

  result <- ecotopia_data(
    "token",
    "tag-1",
    what = "structured",
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)
})


structured_payload <- function(duration, count, values, size) {
  header <- writeBin(
    as.integer(c(duration, count)),
    raw(),
    size = 2,
    endian = "little"
  )
  body <- writeBin(
    as.integer(values),
    raw(),
    size = size,
    endian = "little"
  )

  as.character(openssl::base64_encode(c(header, body)))
}


structured_record <- function(type, data) {
  data.table::data.table(
    id = as.character(glue::glue("record-{type}")),
    device_id = "device-1",
    uuid = "uuid-1",
    updated_at = "2026-06-01T01:00:00.125Z",
    timestamp = "2026-06-01T00:00:00Z",
    firmware_version = 101,
    version = 1,
    type = type,
    data = data
  )
}


test_that("structured encoding type 2 decodes packed predictions", {
  predictions <- c(1, 2, 3, 4, 1, 6)
  packed <- c(
    sum(predictions[1:5] * 2^(seq.int(0, 4) * 3)),
    predictions[6]
  )
  behavior <- structured_record(
    2,
    structured_payload(10, length(predictions), packed, 2)
  )

  result <- ecotopia_postprocess_structured(
    behavior,
    show_progress = FALSE
  )

  expect_equal(result$behavior_id, rep("record-2", 6))
  expect_equal(result$prediction, predictions)
  expect_equal(
    as.numeric(difftime(
      result$time,
      result$sample_start,
      units = "secs"
    )),
    seq.int(0, 10, by = 2)
  )
  expect_equal(
    format(result$upload_time[1], "%OS3", tz = "UTC"),
    "00.125"
  )
})


test_that("structured encoding type 4 decodes classes and scores", {
  votes <- c(
    127 +
      bitwShiftL(1, 7) +
      bitwShiftL(64, 10) +
      bitwShiftL(2, 17) +
      bitwShiftL(3, 27),
    bitwShiftL(4, 7) +
      bitwShiftL(127, 10) +
      bitwShiftL(5, 17) +
      bitwShiftL(32, 20) +
      bitwShiftL(6, 27)
  )
  behavior <- structured_record(
    4,
    structured_payload(4, length(votes), votes, 4)
  )

  result <- ecotopia_postprocess_structured(
    behavior,
    show_progress = FALSE
  )

  expect_equal(result$class_1, c(1, 4))
  expect_equal(result$score_1, c(1, 0))
  expect_equal(result$class_2, c(2, 5))
  expect_equal(result$score_2, c(round(64 / 127, 4), 1))
  expect_equal(result$class_3, c(3, 6))
  expect_equal(result$score_3, c(0, round(32 / 127, 4)))
  expect_equal(
    as.numeric(difftime(
      result$time,
      result$sample_start,
      units = "secs"
    )),
    c(0, 4)
  )
})


test_that("structured postprocessing filters records and skips bad payloads", {
  valid <- structured_record(
    2,
    structured_payload(0, 1, 3, 2)
  )
  invalid <- data.table::copy(valid)
  invalid[, let(
    id = "record-invalid",
    data = "AA=="
  )]
  outside <- data.table::copy(valid)
  outside[, let(
    id = "record-outside",
    timestamp = "2026-06-02T00:00:00Z"
  )]

  result <- ecotopia_postprocess_structured(
    data.table::rbindlist(list(valid, invalid, outside)),
    to = "2026-06-01T12:00:00Z",
    show_progress = FALSE
  )

  expect_equal(result$behavior_id, "record-2")
  expect_equal(result$prediction, 3)
})


test_that("structured postprocessing updates an explicit progress bar", {
  behavior <- structured_record(
    2,
    structured_payload(0, 1, 3, 2)
  )

  expect_no_error(
    capture.output(
      ecotopia_postprocess_structured(
        behavior,
        show_progress = TRUE
      )
    )
  )
})
