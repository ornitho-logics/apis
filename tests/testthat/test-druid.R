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
