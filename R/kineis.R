# KINEIS telemetry API

#' Kineis login token (JWT) ----
#'
#' @param un Username.
#' @param pwd Password.
#' @param auth_url OAuth token endpoint.
#' @param client_id OAuth client ID.
#' @param verbose Print the [httr2::request] object. Defaults to
#'   [interactive()].
#' @note Store credentials in a configuration file or enter them
#'   interactively. Do not hardcode them in scripts.
#'
#' @return A list containing `access_token` and the other token fields.
#' @export
#' @examples
#' \dontrun{
#' crd <- config::get(config = "kineis_api")
#' token <- kineis_login(
#'   un = crd$un,
#'   pwd = crd$pwd,
#'   auth_url = crd$auth_url
#' )
#' }
kineis_login <- function(
  un,
  pwd,
  auth_url,
  client_id = "api-telemetry",
  verbose = interactive()
) {
  x <- httr2::request(auth_url) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(
      grant_type = "password",
      client_id = client_id,
      username = un,
      password = pwd,
      .ptype = "application/x-www-form-urlencoded"
    )

  if (verbose) {
    print(x)
  }

  token <- httr2::req_perform(x) |>
    httr2::resp_body_json(simplifyVector = TRUE)

  token$obtained_at <- lubridate::with_tz(Sys.time(), "UTC")
  token
}


.kineis_access_token <- function(token, force = FALSE) {
  if (is.function(token)) {
    token <- token(force = force)
  }

  if (
    is.character(token) &&
      length(token) == 1 &&
      !is.na(token) &&
      nzchar(token)
  ) {
    return(token)
  }

  if (
    is.list(token) &&
      is.character(token$access_token) &&
      length(token$access_token) == 1 &&
      !is.na(token$access_token) &&
      nzchar(token$access_token)
  ) {
    return(token$access_token)
  }

  stop(
    "`token` must be an access-token string or the result of kineis_login().",
    call. = FALSE
  )
}


.kineis_perform_authenticated <- function(token, request_factory) {
  perform <- function(force = FALSE) {
    access_token <- .kineis_access_token(token, force = force)
    request_factory(access_token) |>
      req_perform()
  }

  tryCatch(
    perform(),
    httr2_http_401 = function(e) {
      if (!is.function(token)) {
        stop(e)
      }

      perform(force = TRUE)
    }
  )
}


.kineis_url <- function(api_telemetry_url, endpoint) {
  paste0(sub("/+$", "", api_telemetry_url), "/", endpoint)
}


.kineis_request_policy <- function(x) {
  x |>
    httr2::req_throttle(
      capacity = 1L,
      fill_time_s = 5,
      realm = "kineis-telemetry-api"
    ) |>
    httr2::req_retry(
      max_tries = 8L,
      max_seconds = 600L,
      retry_on_failure = TRUE,
      failure_realm = "kineis-telemetry-api"
    )
}


.kineis_print_request <- function(x) {
  x |>
    req_headers(Authorization = "Bearer <redacted>") |>
    print()
}


.kineis_response_body <- function(response) {
  resp_body_json(
    response,
    simplifyVector = TRUE,
    bigint_as_char = TRUE,
    flatten = TRUE
  )
}


.kineis_table <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(data.table())
  }

  x <- as.data.table(x)
  id_columns <- intersect(
    c("deviceMsgUid", "providerMsgId", "deviceUid", "dopplerLocId"),
    names(x)
  )

  if (length(id_columns) > 0) {
    x[, (id_columns) := lapply(.SD, as.character), .SDcols = id_columns]
  }

  x
}


.kineis_contents <- function(body) {
  .kineis_table(body$contents)
}


.kineis_retrieval_payload <- function(
  device_refs,
  retrieve_metadata,
  retrieve_raw_data,
  retrieve_doppler,
  retrieve_gps_loc,
  retrieve_sensors,
  retrieve_additional_properties,
  datetime_format
) {
  list(
    retrieveMetadata = isTRUE(retrieve_metadata),
    retrieveRawData = isTRUE(retrieve_raw_data),
    retrieveDoppler = isTRUE(retrieve_doppler),
    retrieveGpsLoc = isTRUE(retrieve_gps_loc),
    retrieveSensors = isTRUE(retrieve_sensors),
    retrieveAdditionnalProperties = isTRUE(
      retrieve_additional_properties
    ),
    deviceRefs = as.list(device_refs),
    datetimeFormat = datetime_format,
    sortBy = list(list(
      field = "msgDatetime",
      sortDirection = "ASC"
    ))
  )
}


#' List all Kineis devices for the login profile ----
#'
#' Requests are paced to one per five seconds. HTTP 429 and 503 responses are retried,
#' honoring the server's `Retry-After` header when present.
#'
#' @param token Access-token string, the full result of [kineis_login()], or a
#'   cached token-provider function accepting a logical `force` argument.
#' @param api_telemetry_url Kineis telemetry API base URL.
#' @param verbose Print the [httr2::request] object. Defaults to
#'   [interactive()].
#'
#' @return A data.table containing all devices available to the login profile.
#' @export
#' @examples
#' \dontrun{
#' crd <- config::get(config = "kineis_api")
#' token <- kineis_login(crd$un, crd$pwd, crd$auth_url)
#' devices <- kineis_devlist(token, crd$api_telemetry_url)
#' }
kineis_devlist <- function(
  token,
  api_telemetry_url,
  verbose = interactive()
) {
  body <- .kineis_perform_authenticated(
    token,
    function(access_token) {
      x <- request(
        .kineis_url(api_telemetry_url, "retrieve-device-list")
      ) |>
        req_headers(
          accept = "application/json",
          Authorization = glue("Bearer {access_token}")
        ) |>
        req_body_raw(charToRaw("{}"), type = "application/json") |>
        .kineis_request_policy()

      if (verbose) {
        .kineis_print_request(x)
      }

      x
    }
  ) |>
    .kineis_response_body()

  if (is.data.frame(body)) {
    return(.kineis_table(body))
  }

  if (!is.null(body$contents)) {
    return(.kineis_contents(body))
  }

  if (!is.null(body$devices)) {
    return(.kineis_table(body$devices))
  }

  if (length(body) == 0) {
    return(data.table())
  }

  .kineis_table(body)
}


.kineis_data_page <- function(
  token,
  api_telemetry_url,
  datetime,
  end_datetime,
  cursor,
  page_size,
  device_refs,
  retrieve_metadata,
  retrieve_raw_data,
  retrieve_doppler,
  retrieve_gps_loc,
  retrieve_sensors,
  retrieve_additional_properties,
  datetime_format,
  verbose
) {
  pagination <- list(first = as.integer(page_size))

  if (!is.null(cursor)) {
    pagination$after <- as.character(cursor)
  }

  payload <- .kineis_retrieval_payload(
    device_refs = device_refs,
    retrieve_metadata = retrieve_metadata,
    retrieve_raw_data = retrieve_raw_data,
    retrieve_doppler = retrieve_doppler,
    retrieve_gps_loc = retrieve_gps_loc,
    retrieve_sensors = retrieve_sensors,
    retrieve_additional_properties = retrieve_additional_properties,
    datetime_format = datetime_format
  )
  payload$pagination <- pagination
  payload$fromDatetime <- datetime

  if (!is.null(end_datetime)) {
    payload$toDatetime <- end_datetime
  }

  .kineis_perform_authenticated(
    token,
    function(access_token) {
      x <- request(.kineis_url(api_telemetry_url, "retrieve-bulk")) |>
        req_headers(
          accept = "application/json",
          Authorization = glue("Bearer {access_token}")
        ) |>
        req_body_json(payload) |>
        .kineis_request_policy()

      if (verbose) {
        .kineis_print_request(x)
      }

      x
    }
  ) |>
    .kineis_response_body()
}


#' Download Kineis telemetry for a time interval ----
#'
#' Uses the bulk endpoint and follows `pageInfo$endCursor` until every page in
#' the requested interval has been retrieved. All fields returned by the API
#' are preserved. Nested metadata and sensor fields are flattened with their
#' parent name (for example, `kineisMetadata.sat` and `sensors.TEMP`). Identifier
#' columns use character storage so 64-bit values remain exact.
#' Messages are requested in ascending `msgDatetime` order. Requests are paced
#' to one per five seconds. HTTP 429 and 503 responses are retried
#' up to eight times within ten minutes, honoring the server's `Retry-After`
#' header when present.
#'
#' @param token Access-token string, the full result of [kineis_login()], or a
#'   cached token-provider function accepting a logical `force` argument.
#' @param api_telemetry_url Kineis telemetry API base URL.
#' @param datetime Inclusive start time in UTC, formatted as RFC 3339 text.
#' @param end_datetime Optional end time in UTC, formatted as RFC 3339 text.
#'   When omitted, the API uses the current time.
#' @param device_refs Character vector of device references. An empty vector
#'   retrieves every device available to the login profile.
#' @param page_size Number of records requested per page.
#' @param retrieve_metadata,retrieve_raw_data,retrieve_doppler,retrieve_gps_loc,retrieve_sensors Logical
#'   flags selecting message fields.
#' @param retrieve_additional_properties Include domains and program
#'   properties. This maps to the API's misspelled
#'   `retrieveAdditionnalProperties` field.
#' @param datetime_format Response timestamp format. Defaults to `"DATETIME"`.
#' @param verbose Print the [httr2::request] objects. Defaults to
#'   [interactive()].
#' @param page_handler Optional function called with each non-empty page before
#'   the next page is requested. This permits incremental database writes. A
#'   handler with a second argument also receives the page's `pageInfo`.
#' @param collect Retain and return all downloaded pages. Set to `FALSE` when a
#'   `page_handler` persists each page and the combined result is not needed.
#' @param after_cursor Optional `endCursor` returned by an earlier request for
#'   the same time interval and filters. This resumes an interrupted
#'   pagination pass.
#'
#' @return A data.table with one row per telemetry message.
#' @export
#' @examples
#' \dontrun{
#' crd <- config::get(config = "kineis_api")
#' token <- kineis_login(crd$un, crd$pwd, crd$auth_url)
#' telemetry <- kineis_data(
#'   token,
#'   crd$api_telemetry_url,
#'   datetime = "2026-07-01T00:00:00.000Z",
#'   end_datetime = "2026-07-02T00:00:00.000Z"
#' )
#' }
kineis_data <- function(
  token,
  api_telemetry_url,
  datetime = "2026-01-01T00:00:00.000Z",
  end_datetime = NULL,
  device_refs = character(),
  page_size = 100L,
  retrieve_metadata = TRUE,
  retrieve_raw_data = TRUE,
  retrieve_doppler = TRUE,
  retrieve_gps_loc = TRUE,
  retrieve_sensors = TRUE,
  retrieve_additional_properties = TRUE,
  datetime_format = "DATETIME",
  verbose = interactive(),
  page_handler = NULL,
  collect = TRUE,
  after_cursor = NULL
) {
  if (
    !is.numeric(page_size) ||
      length(page_size) != 1 ||
      is.na(page_size) ||
      !is.finite(page_size) ||
      page_size < 1 ||
      page_size > .Machine$integer.max ||
      page_size != floor(page_size)
  ) {
    stop("`page_size` must be one positive integer.", call. = FALSE)
  }

  if (!is.null(page_handler) && !is.function(page_handler)) {
    stop("`page_handler` must be a function or NULL.", call. = FALSE)
  }

  if (!is.logical(collect) || length(collect) != 1 || is.na(collect)) {
    stop("`collect` must be TRUE or FALSE.", call. = FALSE)
  }

  if (
    !is.null(after_cursor) &&
      (
        !is.character(after_cursor) ||
          length(after_cursor) != 1 ||
          is.na(after_cursor) ||
          !nzchar(after_cursor)
      )
  ) {
    stop("`after_cursor` must be one non-empty string or NULL.", call. = FALSE)
  }

  pages <- list()
  cursor <- after_cursor

  repeat {
    body <- .kineis_data_page(
      token = token,
      api_telemetry_url = api_telemetry_url,
      datetime = datetime,
      end_datetime = end_datetime,
      cursor = cursor,
      page_size = page_size,
      device_refs = device_refs,
      retrieve_metadata = retrieve_metadata,
      retrieve_raw_data = retrieve_raw_data,
      retrieve_doppler = retrieve_doppler,
      retrieve_gps_loc = retrieve_gps_loc,
      retrieve_sensors = retrieve_sensors,
      retrieve_additional_properties = retrieve_additional_properties,
      datetime_format = datetime_format,
      verbose = verbose
    )

    page <- .kineis_contents(body)
    page_info <- body$pageInfo
    has_next_page <- !is.null(page_info) &&
      isTRUE(page_info$hasNextPage)
    next_cursor <- NULL

    if (has_next_page) {
      next_cursor <- page_info$endCursor

      if (
        is.null(next_cursor) ||
          length(next_cursor) != 1 ||
          is.na(next_cursor) ||
          !nzchar(next_cursor) ||
          identical(as.character(next_cursor), cursor)
      ) {
        stop(
          paste(
            "Kineis pagination indicated another page but returned",
            "no new cursor."
          ),
          call. = FALSE
        )
      }

      next_cursor <- as.character(next_cursor)
    }

    if (nrow(page) > 0) {
      if (collect) {
        pages[[length(pages) + 1]] <- page
      }

      if (!is.null(page_handler)) {
        handler_arguments <- formals(page_handler)

        if (
          "..." %in% names(handler_arguments) ||
            length(handler_arguments) >= 2
        ) {
          page_handler(page, page_info)
        } else {
          page_handler(page)
        }
      }
    }

    if (!has_next_page) {
      break
    }

    cursor <- next_cursor
  }

  rbindlist(pages, use.names = TRUE, fill = TRUE)
}
