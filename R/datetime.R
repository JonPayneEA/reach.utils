#' Coerce a vector to UTC POSIXct
#'
#' Converts character strings or existing POSIXct objects to POSIXct with
#' UTC timezone, which is the standard used across the flode ecosystem.
#'
#' @param x A character vector, POSIXct, or POSIXlt object.
#' @param fmt Optional format string passed to [base::strptime()]. If `NULL`,
#'   [lubridate::as_datetime()] is used for automatic parsing.
#'
#' @return A POSIXct vector in UTC.
#' @export
#'
#' @examples
#' as_utc("2024-01-15 06:00:00")
#' as_utc("15/01/2024 06:00", fmt = "%d/%m/%Y %H:%M")
as_utc <- function(x, fmt = NULL) {
  if (!is.null(fmt)) {
    parsed <- as.POSIXct(strptime(x, format = fmt, tz = "UTC"))
  } else {
    parsed <- lubridate::as_datetime(x, tz = "UTC")
  }
  if (all(is.na(parsed)) && length(x) > 0) {
    cli::cli_warn("as_utc: all values failed to parse. Check your format string.")
  }
  parsed
}

#' Parse datetimes using common EA formats
#'
#' Attempts to parse datetime strings using a prioritised list of formats
#' commonly encountered in Environment Agency hydrometric data, falling back
#' gracefully if a format does not match.
#'
#' @param x A character vector of datetime strings.
#'
#' @return A POSIXct vector in UTC. Values that could not be parsed are `NA`.
#' @export
#'
#' @examples
#' parse_datetime("2024-01-15T06:00:00Z")
#' parse_datetime("15/01/2024 06:00")
parse_datetime <- function(x) {
  ea_formats <- c(
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%d/%m/%Y",
    "%Y-%m-%d"
  )

  result <- rep(as.POSIXct(NA, tz = "UTC"), length(x))

  for (fmt in ea_formats) {
    missing_idx <- which(is.na(result))
    if (length(missing_idx) == 0L) break
    attempt <- as.POSIXct(strptime(x[missing_idx], format = fmt, tz = "UTC"))
    parsed_idx <- which(!is.na(attempt))
    result[missing_idx[parsed_idx]] <- attempt[parsed_idx]
  }

  n_fail <- sum(is.na(result))
  if (n_fail > 0) {
    cli::cli_warn(
      "{n_fail} value{?s} could not be parsed and {?was/were} set to NA."
    )
  }
  result
}

#' Floor a datetime vector to a given time unit
#'
#' A thin wrapper around [lubridate::floor_date()] using the same interface
#' as the rest of the reach.utils datetime helpers.
#'
#' @param x A POSIXct vector.
#' @param unit A string accepted by [lubridate::floor_date()], such as
#'   `"15 minutes"`, `"1 hour"`, `"day"`.
#'
#' @return A POSIXct vector floored to `unit`.
#' @export
#'
#' @examples
#' x <- as_utc("2024-01-15 06:37:22")
#' floor_to(x, "15 minutes")
#' floor_to(x, "1 hour")
floor_to <- function(x, unit) {
  lubridate::floor_date(x, unit = unit)
}

#' Generate a regular datetime sequence
#'
#' Creates a sequence of POSIXct datetimes from `from` to `to` at the
#' specified interval.
#'
#' @param from Start datetime (POSIXct or character coercible via [as_utc()]).
#' @param to End datetime (POSIXct or character coercible via [as_utc()]).
#' @param by Interval string, e.g. `"15 mins"`, `"1 hour"`, `"1 day"`.
#'
#' @return A POSIXct vector in UTC.
#' @export
#'
#' @examples
#' seq_datetime("2024-01-01", "2024-01-02", by = "1 hour")
seq_datetime <- function(from, to, by) {
  if (!inherits(from, "POSIXct")) from <- as_utc(from)
  if (!inherits(to,   "POSIXct")) to   <- as_utc(to)
  seq(from, to, by = by)
}
