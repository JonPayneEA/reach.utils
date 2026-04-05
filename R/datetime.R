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

#' Extract the EA hydrological water year
#'
#' Returns the Environment Agency water year for each datetime in `x`. The EA
#' water year runs from 1 October to 30 September and is labelled by the year
#' in which it begins (e.g. `2023-10-01` falls in water year 2023).
#'
#' @param x A POSIXct vector, or a character vector coercible via [as_utc()].
#'
#' @return An integer vector of water years.
#' @export
#'
#' @examples
#' water_year(as_utc(c("2024-09-30", "2024-10-01")))
#' # [1] 2023 2024
water_year <- function(x) {
  if (!inherits(x, "POSIXct")) x <- as_utc(x)
  yr  <- as.integer(format(x, "%Y", tz = "UTC"))
  mon <- as.integer(format(x, "%m", tz = "UTC"))
  ifelse(mon >= 10L, yr, yr - 1L)
}

#' Detect gaps in a regular datetime sequence
#'
#' Checks a POSIXct vector for intervals larger than the expected `by` step
#' and returns a data frame describing each gap found. Useful for QA of
#' sensor or telemetry data before processing.
#'
#' @param x A POSIXct vector, expected to be in ascending order.
#' @param by Expected interval as a string accepted by [base::seq.POSIXt()],
#'   e.g. `"15 mins"`, `"1 hour"`, `"1 day"`.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{`gap_start`}{POSIXct — the last timestamp before the gap.}
#'     \item{`gap_end`}{POSIXct — the first timestamp after the gap.}
#'     \item{`n_missing`}{integer — number of expected timestamps absent.}
#'   }
#'   Returns zero rows if no gaps are detected.
#' @export
#'
#' @examples
#' x <- as_utc(c("2024-01-01 00:00", "2024-01-01 00:15",
#'               "2024-01-01 01:00", "2024-01-01 01:15"))
#' detect_gaps(x, "15 mins")
detect_gaps <- function(x, by) {
  if (!inherits(x, "POSIXct")) x <- as_utc(x)
  x <- sort(x)

  empty <- data.frame(
    gap_start = as.POSIXct(character(), tz = "UTC"),
    gap_end   = as.POSIXct(character(), tz = "UTC"),
    n_missing = integer()
  )

  if (length(x) < 2L) return(empty)

  ref_seq       <- seq(x[1], x[1] + 2 * 86400, by = by)
  expected_secs <- as.numeric(difftime(ref_seq[2], ref_seq[1], units = "secs"))
  actual_secs   <- as.numeric(difftime(x[-1L], x[-length(x)], units = "secs"))

  gap_idx <- which(actual_secs > expected_secs + .Machine$double.eps)
  if (length(gap_idx) == 0L) return(empty)

  data.frame(
    gap_start = x[gap_idx],
    gap_end   = x[gap_idx + 1L],
    n_missing = as.integer(round(actual_secs[gap_idx] / expected_secs)) - 1L
  )
}

#' Format a duration as a human-readable string
#'
#' Converts a numeric duration in seconds to a concise string such as
#' `"2h 14m 30s"`. Sub-second durations are shown with two decimal places.
#' Primarily useful for logging pipeline runtimes via [log_timed()].
#'
#' @param seconds A non-negative numeric value representing a duration in
#'   seconds.
#'
#' @return A character string.
#' @export
#'
#' @examples
#' format_duration(8070)   # "2h 14m 30s"
#' format_duration(90)     # "1m 30s"
#' format_duration(0.4)    # "0.40s"
format_duration <- function(seconds) {
  if (seconds < 1) return(sprintf("%.2fs", seconds))
  s_int <- as.integer(round(seconds))
  h <- s_int %/% 3600L
  m <- (s_int %% 3600L) %/% 60L
  s <- s_int %% 60L
  if (h > 0L) {
    sprintf("%dh %dm %ds", h, m, s)
  } else if (m > 0L) {
    sprintf("%dm %ds", m, s)
  } else {
    sprintf("%ds", s)
  }
}
