# DRUID Ecotopia API

#' ecotopia login token
#'
#' @param un   username
#' @param pwd  password
#' @param kw1  keyword 1
#' @param kw2  keyword 2
#' @param verbose      print [httr2::request] string. Default to TRUE.
#' @note For security, credentials should either be stored in a configuration file or entered interactively at runtime.
#' Avoid hardcoding credentials directly in your scripts. See examples for more details.
#' @return a character string
#' @export
#' @examples
#' \dontrun{
#' crd = config::get(config = "druid_api")
#' logstring = ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' }
#'
ecotopia_login <- function(un, pwd, kw1, kw2, verbose = TRUE) {
  x <- request("https://www.ecotopiago.com/api/v2/login") |>
    req_body_json(list(
      username = un,
      password = sha256(glue("{un} + {kw1} + {pwd} + {kw2}")) |> as.character()
    ))

  if (verbose) {
    print(x)
  }

  o <- req_perform(x)

  o$headers$`x-druid-authentication`
}


.ecotopia_device_count <- function(logstring, verbose) {
  x <- request("https://www.ecotopiago.com/api/v3/device/count") |>
    req_headers("X-Druid-Authentication" = logstring)

  if (verbose) {
    print(x)
  }

  req_perform(x) |>
    resp_body_string() |>
    as.numeric()
}

.ecotopia_device_page <- function(logstring, cursor = NULL, verbose) {
  x <- request("https://www.ecotopiago.com/api/v3/device/page") |>
    req_headers(
      "X-Druid-Authentication" = logstring,
      "X-result-limit" = 1000,
      "X-result-sort" = "-_id"
    )

  if (!is.null(cursor)) {
    x <- req_url_path_append(x, cursor)
  }

  if (verbose) {
    print(x)
  }

  req_perform(x) |>
    resp_body_json(simplifyVector = TRUE) |>
    as.data.table()
}

#' List all devices for the given login profile
#'
#' Uses the device count endpoint and follows the paginated device endpoint
#' with the stable device ID cursor until every device has been retrieved.
#'
#' @param logstring   login string as returned by [apis::ecotopia_login]
#' @param verbose      print [httr2::request] string. Default to TRUE.
#'
#' @return a data.table
#' @export
#' @examples
#' \dontrun{
#' crd = config::get(config = "druid_api")
#' logstring <- ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' dl = ecotopia_devlist(logstring)
#' }
#'
ecotopia_devlist <- function(logstring, verbose = TRUE) {
  total <- .ecotopia_device_count(logstring, verbose)
  pages <- list()
  retrieved <- 0
  cursor <- NULL

  while (retrieved < total) {
    page <- .ecotopia_device_page(logstring, cursor, verbose)

    if (nrow(page) == 0) {
      break
    }

    pages[[length(pages) + 1]] <- page
    retrieved <- retrieved + nrow(page)
    cursor <- page$id[nrow(page)]
  }

  rbindlist(pages, use.names = TRUE, fill = TRUE)
}


.ecotopia_data <- function(logstring, id, datetime, what, verbose) {
  type <- switch(
    what,
    gps = "gps",
    odba = "behavior2",
    env = "env",
    sms = "sms"
  )

  x <- request(glue("https://www.ecotopiago.com/api/v2/{type}/device")) |>
    req_headers(
      "X-Druid-Authentication" = logstring,
      "X-result-limit" = 1000,
      "X-result-sort" = "timestamp"
    ) |>
    req_url_path_append(glue("/{id}/page/")) |>
    req_url_path_append(datetime)

  if (verbose) {
    print(x)
  }

  o <- req_perform(x) |>
    resp_body_json(simplifyVector = TRUE) |>
    as.data.table()

  # remove unnecessary columns
  if (nrow(o) > 0 & type == "gps") {
    o <- o[, .(
      device_id,
      uuid,
      updated_at,
      timestamp,
      latitude,
      longitude,
      altitude,
      hdop,
      vdop
    )]
  }

  if (nrow(o) > 0 & type == "behavior2") {
    o <- o[, .(device_id, uuid, updated_at, timestamp, odba)]
  }

  if (nrow(o) > 0 & type == "env") {
    o <- o[, .(
      device_id,
      uuid,
      updated_at,
      timestamp,
      ambient_light,
      inner_pressure,
      battery_power,
      battery_voltage
    )]
  }

  o
}

#' Download data for a given tag and calendar day
#'
#' Uses the Ecotopia daily endpoint. An empty response body or HTTP 404 response
#' returns an empty data.table with zero rows and zero columns.
#'
#' @param logstring login string as returned by [apis::ecotopia_login]
#' @param id        device ID (e.g. 640dab1c6f2d20ea33538465)
#' @param date      calendar date ("2023-01-01")
#' @param what      data type: "gps", "odba", "sms" or "env"
#' @param verbose   print [httr2::request] string. Default to TRUE.
#'
#' @return a data.table
#' @export
#' @examples
#' \dontrun{
#' crd = config::get(config = "druid_api")
#' logstring = ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' dl = ecotopia_devlist(logstring)
#' x = ecotopia_data_by_day(logstring, dl$id[204],  Sys.Date() -1, what = "gps")
#' }
#'
ecotopia_data_by_day <- function(
  logstring,
  id,
  date,
  what = "gps",
  verbose = TRUE
) {
  type <- switch(
    what,
    gps = "gps",
    odba = "behavior2",
    env = "env",
    sms = "sms"
  )

  x <- request(glue(
    "https://www.ecotopiago.com/api/v2/{type}/device/{id}/day/{date}"
  )) |>
    req_headers("X-Druid-Authentication" = logstring) |>
    req_error(is_error = function(response) {
      status <- resp_status(response)
      status >= 400 && status != 404
    })

  if (verbose) {
    print(x)
  }

  response <- req_perform(x)

  if (resp_status(response) == 404 || !resp_has_body(response)) {
    return(data.table())
  }

  response |>
    resp_body_json(simplifyVector = TRUE) |>
    as.data.table()
}

#' Download all daily data for a device
#'
#' Calls [ecotopia_data_by_day()] for every date from `start_date` through
#' today, including `start_date`.
#'
#' @param logstring login string as returned by [apis::ecotopia_login]
#' @param id        device ID (e.g. 640dab1c6f2d20ea33538465)
#' @param start_date first calendar date to retrieve
#' @param what      data type: "gps", "odba", "sms" or "env"
#' @param verbose   print [httr2::request] strings. Default to TRUE.
#'
#' @return a data.table
#' @export
#' @examples
#' \dontrun{
#' crd = config::get(config = "druid_api")
#' logstring = ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' x = ecotopia_data_all_by_id(
#'   logstring,
#'   id = "650d43ed44c2dcc1a0dc30a9",
#'   start_date = Sys.Date() - 2,
#'   what = "gps"
#' )
#' }
#'
ecotopia_data_all_by_id <- function(
  logstring,
  id,
  start_date,
  what = "gps",
  verbose = TRUE
) {
  dates <- seq.Date(as.Date(start_date), Sys.Date(), by = "day")

  results <- lapply(dates, function(date) {
    ecotopia_data_by_day(
      logstring = logstring,
      id = id,
      date = date,
      what = what,
      verbose = verbose
    )
  })

  rbindlist(results, use.names = TRUE, fill = TRUE)
}

#' Download gps data of a given id after a given datetime
#'
#' @param logstring login string as returned by [apis::ecotopia_login]
#' @param id        device ID (e.g 640dab1c6f2d20ea33538465)
#' @param datetime  datetime string ("2000-01-01T00:00:00Z")
#' @param what      data type: "gps", "odba" , "sms" or "env"
#' @param verbose   print [httr2::request] string. Default to TRUE.
#'
#' @return a data.table
#' @export
#' @examples
#' \dontrun{
#' crd = config::get(config = "druid_api")
#' logstring <- ecotopia_login(crd$generic$un, crd$generic$pwd, crd$kw1, crd$kw2)
#' dl = ecotopia_devlist(logstring)
#' x = ecotopia_data(logstring, dl$id[100], what = "gps")
#' x = ecotopia_data(logstring, dl$id[100], what = "odba")
#' x = ecotopia_data(logstring, dl$id[100], what = "sms")
#' }
#'
ecotopia_data <- function(
  logstring,
  id,
  datetime = "2000-01-01T00:00:00Z",
  what = "gps",
  verbose = TRUE
) {
  x <- .ecotopia_data(
    logstring = logstring,
    id = id,
    datetime = datetime,
    what = what,
    verbose = verbose
  )

  cli_progress_bar(
    "Querying API (1000 rows):",
    type = "iterator",
    clear = FALSE
  )

  while (nrow(x) > 0) {
    cli_progress_update()
    last_timestamp <- ymd_hms(x$timestamp) |> with_tz("UTC")
    last_timestamp <- x[which(last_timestamp == max(last_timestamp)), timestamp]

    x_next <- .ecotopia_data(
      logstring = logstring,
      id = id,
      datetime = last_timestamp,
      what = what,
      verbose = FALSE
    )

    if (nrow(x_next) == 0) {
      break
    }

    x <- rbind(x, x_next)
  }

  cli_progress_done()

  x
}
