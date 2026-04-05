#' Check for duplicate timestamps
#'
#' Returns any timestamps that appear more than once. Duplicates frequently
#' arise from data joins, API re-ingestion, or overlapping file ranges.
#'
#' @param x A POSIXct vector (or character coercible via [as_utc()]).
#'
#' @return A data frame with columns `timestamp` and `n_occurrences`.
#'   Zero rows if no duplicates are found.
#' @export
#'
#' @examples
#' x <- as_utc(c("2024-01-01 00:00", "2024-01-01 00:15",
#'               "2024-01-01 00:15", "2024-01-01 00:30"))
#' check_duplicates(x)
check_duplicates <- function(x) {
  if (!inherits(x, "POSIXct")) x <- as_utc(x)

  empty <- data.frame(
    timestamp     = as.POSIXct(character(), tz = "UTC"),
    n_occurrences = integer()
  )

  if (length(x) < 2L) return(empty)

  counts  <- table(as.numeric(x))
  dup_num <- as.numeric(names(counts[counts > 1L]))
  if (length(dup_num) == 0L) return(empty)

  data.frame(
    timestamp     = as.POSIXct(dup_num, origin = "1970-01-01", tz = "UTC"),
    n_occurrences = as.integer(counts[counts > 1L])
  )
}

#' Check that timestamps are in strict ascending order
#'
#' Returns any timestamps that are not greater than the preceding value.
#' Out-of-order timestamps will silently corrupt any lag, diff, or join
#' operation on a time series.
#'
#' @param x A POSIXct vector (or character coercible via [as_utc()]).
#'
#' @return A data frame with columns `timestamp` and `index`.
#'   Zero rows if timestamps are strictly ascending.
#' @export
#'
#' @examples
#' x <- as_utc(c("2024-01-01 00:00", "2024-01-01 00:30",
#'               "2024-01-01 00:15", "2024-01-01 00:45"))
#' check_monotonic(x)
check_monotonic <- function(x) {
  if (!inherits(x, "POSIXct")) x <- as_utc(x)

  empty <- data.frame(
    timestamp = as.POSIXct(character(), tz = "UTC"),
    index     = integer()
  )

  if (length(x) < 2L) return(empty)

  bad_idx <- which(diff(as.numeric(x)) <= 0) + 1L
  if (length(bad_idx) == 0L) return(empty)

  data.frame(timestamp = x[bad_idx], index = bad_idx)
}

#' Check that values stay within physical bounds
#'
#' Returns timestamps where `values` falls outside the interval `[min, max]`.
#' `NA` values are skipped. Use this for physical plausibility checks such as
#' negative stage or flow exceeding a credible maximum.
#'
#' @param x A POSIXct vector (or character coercible via [as_utc()]).
#' @param values A numeric vector the same length as `x`.
#' @param min Lower bound (inclusive).
#' @param max Upper bound (inclusive).
#'
#' @return A data frame with columns `timestamp` and `value`.
#'   Zero rows if all non-NA values are within bounds.
#' @export
#'
#' @examples
#' x      <- seq_datetime("2024-01-01", "2024-01-01 01:00", "15 mins")
#' values <- c(0.5, 1.2, -0.1, 0.8, 15.0)
#' check_bounds(x, values, min = 0, max = 10)
check_bounds <- function(x, values, min, max) {
  if (!inherits(x, "POSIXct")) x <- as_utc(x)

  empty <- data.frame(
    timestamp = as.POSIXct(character(), tz = "UTC"),
    value     = numeric()
  )

  bad_idx <- which(!is.na(values) & (values < min | values > max))
  if (length(bad_idx) == 0L) return(empty)

  data.frame(timestamp = x[bad_idx], value = values[bad_idx])
}

#' Check for flatlined (frozen) sensor values
#'
#' Detects runs of `n` or more consecutive identical non-NA values, which
#' typically indicate a stuck or frozen sensor. Returns one row per run.
#'
#' @param x A POSIXct vector (or character coercible via [as_utc()]).
#' @param values A numeric vector the same length as `x`.
#' @param n Minimum run length to flag. Defaults to `3L`.
#'
#' @return A data frame with columns `run_start`, `run_end`, `value`, and
#'   `run_length`. Zero rows if no flatlines are found.
#' @export
#'
#' @examples
#' x      <- seq_datetime("2024-01-01", "2024-01-01 01:00", "15 mins")
#' values <- c(1.0, 2.0, 2.0, 2.0, 2.0, 3.0)
#' check_flatline(x, values, n = 3)
check_flatline <- function(x, values, n = 3L) {
  if (!inherits(x, "POSIXct")) x <- as_utc(x)
  n <- as.integer(n)

  empty <- data.frame(
    run_start  = as.POSIXct(character(), tz = "UTC"),
    run_end    = as.POSIXct(character(), tz = "UTC"),
    value      = numeric(),
    run_length = integer()
  )

  r         <- rle(values)
  ends      <- cumsum(r$lengths)
  starts    <- ends - r$lengths + 1L
  long_idx  <- which(r$lengths >= n & !is.na(r$values))

  if (length(long_idx) == 0L) return(empty)

  data.frame(
    run_start  = x[starts[long_idx]],
    run_end    = x[ends[long_idx]],
    value      = r$values[long_idx],
    run_length = as.integer(r$lengths[long_idx])
  )
}

#' Check for excessive rate-of-change between consecutive values
#'
#' Returns timestamps where the absolute step change from the previous value
#' exceeds `max_change`. Useful for catching spikes and abrupt data dropouts.
#' `NA` steps are skipped. The flagged timestamp is the second point of the
#' offending pair.
#'
#' @param x A POSIXct vector (or character coercible via [as_utc()]).
#' @param values A numeric vector the same length as `x`.
#' @param max_change Maximum permitted absolute change between consecutive
#'   non-NA values.
#'
#' @return A data frame with columns `timestamp`, `value`, and `change`.
#'   Zero rows if no step exceeds `max_change`.
#' @export
#'
#' @examples
#' x      <- seq_datetime("2024-01-01", "2024-01-01 01:00", "15 mins")
#' values <- c(1.0, 1.1, 8.5, 1.2, 1.3, 1.1)
#' check_rate_of_change(x, values, max_change = 1.0)
check_rate_of_change <- function(x, values, max_change) {
  if (!inherits(x, "POSIXct")) x <- as_utc(x)

  empty <- data.frame(
    timestamp = as.POSIXct(character(), tz = "UTC"),
    value     = numeric(),
    change    = numeric()
  )

  if (length(values) < 2L) return(empty)

  changes <- abs(diff(values))
  bad_idx <- which(!is.na(changes) & changes > max_change) + 1L

  if (length(bad_idx) == 0L) return(empty)

  data.frame(
    timestamp = x[bad_idx],
    value     = values[bad_idx],
    change    = changes[bad_idx - 1L]
  )
}

#' Check for extended runs of NA values
#'
#' Returns runs of `NA` in `values` whose length exceeds `max_run`.
#' Distinguishes acceptable short gaps (e.g. a single missing reading) from
#' extended outages that may require special handling. Returns one row per run.
#'
#' @param x A POSIXct vector (or character coercible via [as_utc()]).
#' @param values A numeric vector the same length as `x`.
#' @param max_run Maximum permitted consecutive `NA` count before flagging.
#'   Defaults to `1L` (flag any run of 2 or more NAs).
#'
#' @return A data frame with columns `run_start`, `run_end`, and `run_length`.
#'   Zero rows if no NA run exceeds `max_run`.
#' @export
#'
#' @examples
#' x      <- seq_datetime("2024-01-01", "2024-01-01 01:00", "15 mins")
#' values <- c(1.0, NA, NA, NA, 1.5, 1.6)
#' check_na_runs(x, values, max_run = 1)
check_na_runs <- function(x, values, max_run = 1L) {
  if (!inherits(x, "POSIXct")) x <- as_utc(x)
  max_run <- as.integer(max_run)

  empty <- data.frame(
    run_start  = as.POSIXct(character(), tz = "UTC"),
    run_end    = as.POSIXct(character(), tz = "UTC"),
    run_length = integer()
  )

  r        <- rle(is.na(values))
  ends     <- cumsum(r$lengths)
  starts   <- ends - r$lengths + 1L
  long_idx <- which(r$values & r$lengths > max_run)

  if (length(long_idx) == 0L) return(empty)

  data.frame(
    run_start  = x[starts[long_idx]],
    run_end    = x[ends[long_idx]],
    run_length = as.integer(r$lengths[long_idx])
  )
}

#' Run a suite of time series quality checks
#'
#' Executes a configurable set of QC checks on a datetime vector and its
#' associated values, returning a named list of results — one element per
#' check run. Each element is a data frame in the format returned by the
#' corresponding `check_*` function. Zero rows means the check passed.
#'
#' Checks requiring a threshold parameter (`"bounds"`, `"rate_of_change"`)
#' are silently skipped if their parameter is not supplied.
#'
#' @param x A POSIXct vector (or character coercible via [as_utc()]).
#' @param values A numeric vector the same length as `x`.
#' @param checks Character vector of checks to run. Defaults to all available
#'   checks: `"duplicates"`, `"monotonic"`, `"bounds"`, `"flatline"`,
#'   `"rate_of_change"`, `"na_runs"`.
#' @param bounds Numeric vector `c(min, max)` for the bounds check.
#'   Required to run the `"bounds"` check.
#' @param flatline_n Minimum flatline run length to flag. Defaults to `3L`.
#' @param max_change Maximum permitted absolute step change for the
#'   rate-of-change check. Required to run the `"rate_of_change"` check.
#' @param max_na_run Maximum permitted consecutive NA run length. Defaults
#'   to `1L`.
#'
#' @return A named list of data frames, one per check that was run.
#' @export
#'
#' @examples
#' x      <- seq_datetime("2024-01-01", "2024-01-01 01:00", "15 mins")
#' values <- c(1.0, 1.1, 1.1, 1.1, NA, 9.9)
#' qc_series(x, values, bounds = c(0, 5), max_change = 2, flatline_n = 3)
qc_series <- function(x, values,
                      checks     = c("duplicates", "monotonic", "bounds",
                                     "flatline", "rate_of_change", "na_runs"),
                      bounds     = NULL,
                      flatline_n = 3L,
                      max_change = NULL,
                      max_na_run = 1L) {
  checks  <- match.arg(checks, several.ok = TRUE)
  results <- list()

  if ("duplicates" %in% checks)
    results$duplicates <- check_duplicates(x)

  if ("monotonic" %in% checks)
    results$monotonic <- check_monotonic(x)

  if ("bounds" %in% checks) {
    if (is.null(bounds)) {
      cli::cli_warn(
        "Skipping {.val bounds} check: supply {.arg bounds = c(min, max)}."
      )
    } else {
      results$bounds <- check_bounds(x, values, bounds[1], bounds[2])
    }
  }

  if ("flatline" %in% checks)
    results$flatline <- check_flatline(x, values, flatline_n)

  if ("rate_of_change" %in% checks) {
    if (is.null(max_change)) {
      cli::cli_warn(
        "Skipping {.val rate_of_change} check: supply {.arg max_change}."
      )
    } else {
      results$rate_of_change <- check_rate_of_change(x, values, max_change)
    }
  }

  if ("na_runs" %in% checks)
    results$na_runs <- check_na_runs(x, values, max_na_run)

  results
}
