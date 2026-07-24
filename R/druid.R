# DRUID Ecotopia API

#' Ecotopia login token ----
#'
#' @param un Username.
#' @param pwd Password.
#' @param kw1 First password-signature keyword.
#' @param kw2 Second password-signature keyword.
#' @param verbose Print the [httr2::request] object. Defaults to
#'   [interactive()].
#' @note Store credentials in a configuration file or enter them
#'   interactively. Do not hardcode them in scripts.
#'
#' @return A character authentication token.
#' @export
#' @examples
#' \dontrun{
#' crd <- config::get(config = "druid_api")
#' logstring <- ecotopia_login(
#'   crd$generic$un,
#'   crd$generic$pwd,
#'   crd$kw1,
#'   crd$kw2
#' )
#' }
ecotopia_login <- function(un, pwd, kw1, kw2, verbose = interactive()) {
  x <- request("https://www.ecotopiago.com/api/v2/login") |>
    req_body_json(list(
      username = un,
      password = sha256(glue("{un} + {kw1} + {pwd} + {kw2}")) |>
        as.character()
    ))

  if (verbose) {
    print(x)
  }

  response <- req_perform(x)

  response$headers$`x-druid-authentication`
}


.ecotopia_print_request <- function(x) {
  x |>
    req_headers("X-Druid-Authentication" = "<redacted>") |>
    print()
}

.ecotopia_device_count <- function(logstring, verbose) {
  x <- request("https://www.ecotopiago.com/api/v3/device/count") |>
    req_headers("X-Druid-Authentication" = logstring)

  if (verbose) {
    .ecotopia_print_request(x)
  }

  req_perform(x) |>
    resp_body_string() |>
    as.numeric()
}

#' Download one page of Ecotopia devices
#'
#' @param logstring Login string returned by [ecotopia_login()].
#' @param cursor Device ID cursor returned on the previous page. Use `NULL` for
#'   the first page.
#' @param verbose Print the [httr2::request] object. Defaults to
#'   [interactive()].
#'
#' @return A data.table containing one page of devices.
#' @export
ecotopia_device_page <- function(
  logstring,
  cursor = NULL,
  verbose = interactive()
) {
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
    .ecotopia_print_request(x)
  }

  req_perform(x) |>
    resp_body_json(simplifyVector = TRUE) |>
    as.data.table()
}


#' List all devices for the given login profile ----
#'
#' Uses the device count endpoint and follows the paginated device endpoint
#' with the stable device ID cursor until every device has been retrieved.
#'
#' @param logstring Login string returned by [ecotopia_login()].
#' @param verbose Print the [httr2::request] objects. Defaults to
#'   [interactive()].
#'
#' @return A data.table containing all devices available to the login profile.
#' @export
#' @examples
#' \dontrun{
#' crd <- config::get(config = "druid_api")
#' logstring <- ecotopia_login(
#'   crd$generic$un,
#'   crd$generic$pwd,
#'   crd$kw1,
#'   crd$kw2
#' )
#' devices <- ecotopia_devlist(logstring)
#' }
ecotopia_devlist <- function(logstring, verbose = interactive()) {
  total <- .ecotopia_device_count(logstring, verbose)
  pages <- list()
  retrieved <- 0
  cursor <- NULL

  while (retrieved < total) {
    page <- ecotopia_device_page(logstring, cursor, verbose)

    if (nrow(page) == 0) {
      break
    }

    pages[[length(pages) + 1]] <- page
    retrieved <- retrieved + nrow(page)
    cursor <- page$id[nrow(page)]
  }

  rbindlist(pages, use.names = TRUE, fill = TRUE)
}


.ecotopia_data_type <- function(what) {
  switch(
    what,
    odba = "behavior2",
    behavior = "structured",
    behaviour = "structured",
    what
  )
}


.ecotopia_data_page <- function(
  logstring,
  id,
  cursor,
  type,
  verbose
) {
  url <- glue(
    "https://www.ecotopiago.com/api/v2/{type}/device/{id}/page"
  ) |>
    as.character()

  x <- request(url) |>
    req_url_path_append(cursor) |>
    req_headers(
      "X-Druid-Authentication" = logstring,
      "X-result-limit" = 1000,
      "X-result-sort" = "timestamp"
    )

  if (verbose) {
    .ecotopia_print_request(x)
  }

  req_perform(x) |>
    resp_body_json(simplifyVector = TRUE) |>
    as.data.table()
}


#' Download all data of one type for an Ecotopia device
#'
#' Follows the timestamp-cursor endpoint until no more records are available.
#' The API record ID and all other returned fields are preserved.
#'
#' @param logstring Login string returned by [ecotopia_login()].
#' @param id Ecotopia device ID.
#' @param datetime Start timestamp in RFC 3339 format.
#' @param what API data route, including `"gps"`, `"env"`, `"sms"`,
#'   `"behavior2"`, `"structured"`, `"celltower"`, `"origin"` and `"beacon"`.
#'   `"odba"` is an alias for `"behavior2"`; `"behavior"` and `"behaviour"` are
#'   aliases for `"structured"`. Structured algorithm behaviour is Ecotopia data type 18.
#' @param verbose Print the [httr2::request] objects. Defaults to
#'   [interactive()].
#' @param end_datetime Optional exclusive end timestamp in RFC 3339 format.
#'
#' @return A data.table containing the raw API records.
#' @export
#' @examples
#' \dontrun{
#' crd <- config::get(config = "druid_api")
#' logstring <- ecotopia_login(
#'   crd$generic$un,
#'   crd$generic$pwd,
#'   crd$kw1,
#'   crd$kw2
#' )
#' devices <- ecotopia_devlist(logstring)
#' behavior <- ecotopia_data(
#'   logstring,
#'   '68d4a842c3d77b735bfd9d96',
#'   datetime = "2026-06-01T00:00:00Z",
#'   what = "structured"
#' )
#' }
ecotopia_data <- function(
  logstring,
  id,
  datetime = "2000-01-01T00:00:00Z",
  what = "gps",
  verbose = interactive(),
  end_datetime = NULL
) {
  type <- .ecotopia_data_type(what)
  cursor <- datetime
  end_time <- if (is.null(end_datetime)) NULL else ymd_hms(end_datetime)
  pages <- list()

  cli_progress_bar(
    "Querying Ecotopia API:",
    type = "iterator",
    clear = FALSE
  )

  repeat {
    page <- .ecotopia_data_page(
      logstring = logstring,
      id = id,
      cursor = cursor,
      type = type,
      verbose = verbose
    )

    if (nrow(page) == 0) {
      break
    }

    pages[[length(pages) + 1]] <- page
    cli_progress_update()

    page_time <- ymd_hms(page$timestamp)
    next_cursor <- page$timestamp[which.max(page_time)]

    if (!is.null(end_time) && max(page_time) >= end_time) {
      break
    }

    if (identical(next_cursor, cursor)) {
      break
    }

    cursor <- next_cursor
  }

  cli_progress_done()

  data <- rbindlist(pages, use.names = TRUE, fill = TRUE)

  if (nrow(data) == 0) {
    return(data)
  }

  data <- unique(data, by = "id")

  if (!is.null(end_time)) {
    data <- data[ymd_hms(data$timestamp) < end_time]
  }

  data
}
