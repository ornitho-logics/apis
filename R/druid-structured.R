.ecotopia_parse_api_time <- function(x) {
  as.POSIXct(
    x,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )
}


.ecotopia_decode_base64 <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return(NULL)
  }

  tryCatch(
    openssl::base64_decode(x),
    error = function(e) NULL
  )
}


.ecotopia_sample_offsets <- function(duration, count) {
  if (count == 1) {
    return(0)
  }

  seq.int(0, count - 1) * duration / (count - 1)
}


.ecotopia_process_structured_v2 <- function(row) {
  payload <- .ecotopia_decode_base64(row$data)

  if (is.null(payload) || length(payload) < 4) {
    return(NULL)
  }

  header <- readBin(
    payload[seq_len(4)],
    what = integer(),
    n = 2,
    size = 2,
    signed = TRUE,
    endian = "little"
  )

  duration <- header[1]
  count <- header[2]

  if (is.na(count) || count <= 0) {
    return(NULL)
  }

  n_words <- (count + 4) %/% 5
  expected_length <- 4 + n_words * 2

  if (length(payload) != expected_length) {
    return(NULL)
  }

  packed <- readBin(
    payload[seq.int(5, length(payload))],
    what = integer(),
    n = n_words,
    size = 2,
    signed = TRUE,
    endian = "little"
  )

  sample_index <- seq.int(0, count - 1)
  word_index <- sample_index %/% 5 + 1
  bit_offset <- (sample_index %% 5) * 3

  prediction <- bitwAnd(
    bitwShiftR(packed[word_index], bit_offset),
    7
  )

  data.table(
    behavior_id = row$id,
    device_id = row$device_id,
    uuid = row$uuid,
    firmware_version = row$firmware_version,
    payload_version = row$version,
    encoding_type = row$type,
    upload_time = row$upload_time,
    sample_start = row$sample_time,
    time = row$sample_time +
      .ecotopia_sample_offsets(duration, count),
    prediction = prediction
  )
}


.ecotopia_process_structured_v4 <- function(row) {
  payload <- .ecotopia_decode_base64(row$data)

  if (is.null(payload) || length(payload) < 4) {
    return(NULL)
  }

  header <- readBin(
    payload[seq_len(4)],
    what = integer(),
    n = 2,
    size = 2,
    signed = TRUE,
    endian = "little"
  )

  duration <- header[1]
  count <- header[2]

  if (is.na(count) || count <= 0) {
    return(NULL)
  }

  expected_length <- 4 + count * 4

  if (length(payload) != expected_length) {
    return(NULL)
  }

  votes <- readBin(
    payload[seq.int(5, length(payload))],
    what = integer(),
    n = count,
    size = 4,
    signed = TRUE,
    endian = "little"
  )

  data.table(
    behavior_id = row$id,
    device_id = row$device_id,
    uuid = row$uuid,
    firmware_version = row$firmware_version,
    payload_version = row$version,
    encoding_type = row$type,
    upload_time = row$upload_time,
    sample_start = row$sample_time,
    time = row$sample_time +
      .ecotopia_sample_offsets(duration, count),
    class_1 = bitwAnd(bitwShiftR(votes, 7), 7),
    score_1 = round(bitwAnd(votes, 127) / 127, 4),
    class_2 = bitwAnd(bitwShiftR(votes, 17), 7),
    score_2 = round(
      bitwAnd(bitwShiftR(votes, 10), 127) / 127,
      4
    ),
    class_3 = bitwAnd(bitwShiftR(votes, 27), 7),
    score_3 = round(
      bitwAnd(bitwShiftR(votes, 20), 127) / 127,
      4
    )
  )
}


.ecotopia_process_structured_row <- function(row) {
  switch(
    as.character(row$type),
    "2" = .ecotopia_process_structured_v2(row),
    "4" = .ecotopia_process_structured_v4(row),
    NULL
  )
}


#' Decode structured Ecotopia behavior data
#'
#' Decodes the base64 `data` payload returned by
#' `ecotopia_data(..., what = "structured")`. Encoding type 2 produces one
#' predicted class per sample. Encoding type 4 produces the three leading
#' classes and their normalized scores per sample.
#'
#' @param behavior Raw structured behavior records returned by
#'   [ecotopia_data()].
#' @param from Optional inclusive start time, as RFC 3339 text or a POSIX time.
#' @param to Optional inclusive end time, as RFC 3339 text or a POSIX time.
#' @param show_progress Display decoding progress. Defaults to [interactive()].
#'
#' @return A data.table with one row per decoded behavior sample.
#' @export
ecotopia_postprocess_structured <- function(
  behavior,
  from = NULL,
  to = NULL,
  show_progress = interactive()
) {
  x <- data.table::copy(as.data.table(behavior))

  required_columns <- c(
    "id",
    "device_id",
    "uuid",
    "updated_at",
    "timestamp",
    "firmware_version",
    "version",
    "type",
    "data"
  )

  missing_columns <- setdiff(required_columns, names(x))

  if (length(missing_columns) > 0) {
    stop(
      glue("Missing columns: {toString(missing_columns)}"),
      call. = FALSE
    )
  }

  x[, let(
    upload_time = .ecotopia_parse_api_time(updated_at),
    sample_time = .ecotopia_parse_api_time(timestamp)
  )]

  x <- x[!is.na(sample_time)]

  if (!is.null(from)) {
    from <- if (inherits(from, "POSIXt")) {
      as.POSIXct(from, tz = "UTC")
    } else {
      .ecotopia_parse_api_time(from)
    }

    x <- x[sample_time >= from]
  }

  if (!is.null(to)) {
    to <- if (inherits(to, "POSIXt")) {
      as.POSIXct(to, tz = "UTC")
    } else {
      .ecotopia_parse_api_time(to)
    }

    x <- x[sample_time <= to]
  }

  if (nrow(x) == 0) {
    return(data.table())
  }

  progress_id <- NULL

  if (show_progress) {
    progress_id <- cli_progress_bar(
      "Decoding structured behavior:",
      total = nrow(x),
      clear = FALSE
    )
  }

  decoded <- lapply(seq_len(nrow(x)), function(i) {
    result <- .ecotopia_process_structured_row(as.list(x[i]))

    if (show_progress) {
      cli_progress_update(id = progress_id)
    }

    result
  })

  if (show_progress) {
    cli_progress_done(id = progress_id)
  }

  decoded <- Filter(Negate(is.null), decoded)

  if (length(decoded) == 0) {
    return(data.table())
  }

  rbindlist(
    decoded,
    use.names = TRUE,
    fill = TRUE
  )
}
