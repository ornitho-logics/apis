#' KINEIS login token (JWT)
#'
#' @param un username (from config if missing)
#' @param pwd password (from config if missing)
#' @param auth_url token endpoint (from config if missing)
#' @param client_id oauth client id
#' @param verbose print [httr2::request] string. Default to TRUE.
#'
#' @return a list with `access_token` plus other token fields
#' @export
#'
kineis_login <- function(
  un,
  pwd,
  auth_url,
  client_id = "api-telemetry",
  verbose = TRUE
) {
  req = httr2::request(auth_url) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(
      grant_type = "password",
      client_id = client_id,
      username = un,
      password = pwd,
      .ptype = "application/x-www-form-urlencoded"
    )

  if (verbose) {
    print(req)
  }

  tok = httr2::req_perform(req) |>
    httr2::resp_body_json(simplifyVector = TRUE)

  tok$obtained_at = lubridate::with_tz(Sys.time(), "UTC")
  tok
}


#' Retrieve KINEIS realtime telemetry from a checkpoint
#'
#' @param token either a character access token, or the full list returned by [kineis_login]
#' @param api_telemetry_url API base url (usually from config)
#' @param checkpoint initial checkpoint (0 typically means "last ~6 hours" per KINEIS example)
#' @param deviceRefs character vector of device refs; empty means "all available"
#' @param datetimeFormat "DATETIME" unless you know other accepted values
#' @param retrieveMetadata,retrieveRawData,retrieveDoppler,retrieveGpsLoc,retrieveSensors,retrieveAdditionnalProperties flags
#' @param verbose Default to TRUE.
#'
#' @return a list with `checkpoint`, `data` (data.table, possibly empty), and `raw` (full parsed JSON)
#' @examples
#' \dontrun{
#' crd = config::get(config = "kineis_api")
#' tok = kineis_login(un = crd$un, pwd = crd$pwd, auth_url = crd$auth_url)
#' out = kineis_retrieve_realtime(tok, api_telemetry_url = crd$api_telemetry_url)
#' out = kineis_retrieve_realtime(tok, api_telemetry_url = crd$api_telemetry_url,
#'        checkpoint = attributes(out)$checkpoint )
#' }
#' @export
kineis_retrieve_realtime <- function(
  token,
  api_telemetry_url,
  checkpoint = 0,
  deviceRefs = character(),
  retrieveMetadata = TRUE,
  retrieveRawData = TRUE,
  retrieveDoppler = TRUE,
  retrieveGpsLoc = TRUE,
  retrieveSensors = TRUE,
  retrieveAdditionnalProperties = TRUE,
  datetimeFormat = "DATETIME",
  verbose = TRUE
) {
  access_token = token$access_token

  req_url = glue::glue("{api_telemetry_url}/retrieve-realtime")

  payload = list(
    retrieveMetadata = isTRUE(retrieveMetadata),
    retrieveRawData = isTRUE(retrieveRawData),
    retrieveDoppler = isTRUE(retrieveDoppler),
    retrieveGpsLoc = isTRUE(retrieveGpsLoc),
    retrieveSensors = isTRUE(retrieveSensors),
    retrieveAdditionnalProperties = isTRUE(retrieveAdditionnalProperties),
    deviceRefs = as.list(deviceRefs),
    datetimeFormat = datetimeFormat,
    fromCheckpoint = as.character(checkpoint)
  )

  x = httr2::request(req_url) |>
    httr2::req_headers(
      accept = "application/json",
      Authorization = glue::glue("Bearer {access_token}"),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(payload)

  if (verbose) {
    print(x)
  }

  o = httr2::req_perform(x)

  o = httr2::resp_body_json(o, simplifyVector = FALSE)

  new_checkpoint = o$checkpoint

  X = lapply(o[[2]], data.frame)
  X = rbindlist(X, fill = TRUE)

  setattr(X, "checkpoint", new_checkpoint)

  X
}

#' @export
kineis_db_ini <- function(path = '~/kineis_local_db.sqlite') {
  cmd =
    glue(
      "sqlite3 {shQuote({path}|>path.expand())} \".read {shQuote(system.file('kineis_local_db.SQL', package = 'apis'))}\""
    )
  system(cmd)
}


#' Save KINEIS realtime telemetry to disk (incremental)
#'
#' @examples
#' \dontrun{
#' crd = config::get(config = "kineis_api")
#'
#' kineis_save_real_time(
#'   un = crd$un,
#'   pwd = crd$pwd,
#'   auth_url = crd$auth_url,
#'   api_telemetry_url = crd$api_telemetry_url
#' )
#'
#' }
#' @export
kineis_save_real_time <- function(
  path = '~/kineis_local_db.sqlite',
  un,
  pwd,
  auth_url,
  api_telemetry_url
) {
  tok = kineis_login(un = un, pwd = pwd, auth_url = auth_url)

  con = dbConnect(SQLite(), path)
  on.exit(dbDisconnect(con))

  db_checkpoint = dbGetQuery(
    con,
    "SELECT checkpoint FROM checkpoint
      ORDER BY id DESC
      LIMIT 1"
  )
  if (nrow(db_checkpoint) == 0) {
    db_checkpoint = 0
  } else {
    db_checkpoint = db_checkpoint$checkpoint |> as.numeric()
  }

  out = kineis_retrieve_realtime(
    tok,
    api_telemetry_url = crd$api_telemetry_url,
    checkpoint = db_checkpoint
  )

  new_checkpoint = attributes(out)$checkpoint

  d = out[, .(
    deviceUid,
    msgDatetime,
    acqDatetime,
    dopplerDatetime,
    dopplerLocLon,
    dopplerLocLat,
    dopplerLocAlt,
    dopplerLocErrorRadius,
    dopplerLocClass
  )]

  sz = out[, .(deviceUid, msgDatetime, sensors)]
  sz[, sensors := paste0("{", sensors, "}")]

  szw = lapply(sz$sensors, \(s) fromJSON(s)) |>
    rbindlist(fill = TRUE)
  szw = cbind(sz[, .(deviceUid, msgDatetime)], szw)

  szl = melt(
    szw,
    id.vars = c("deviceUid", "msgDatetime"),
    measure.vars = patterns("^SENSOR"),
    variable.name = "sensor",
    value.name = "value"
  )

  # Prepare for upload
  szl[, sensor := str_extract(sensor, "\\d+")]
  d = d[!is.na(dopplerLocLon)]
  checkpoint = data.table(checkpoint = new_checkpoint, updated_at = Sys.time())

  # write
  a = dbAppendTable(con, "sensors", szl)
  b = dbAppendTable(con, "doppler", d)
  c = dbAppendTable(con, "checkpoint", checkpoint)
  c(sensors = a, doppler = b, checkpoint = c)
}
