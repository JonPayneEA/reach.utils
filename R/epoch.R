#' Get the start and end of an EA water year
#'
#' Returns the inclusive start and end instants of the Environment Agency water
#' year that begins on 1 October of `year`. The water year runs from
#' `YYYY-10-01 00:00:00` to `(YYYY+1)-09-30 23:59:59`.
#'
#' @param year Integer. The year in which the water year begins (e.g. `2023`
#'   for the October 2023 – September 2024 water year).
#' @param tz Time zone string. Default `"UTC"`.
#'
#' @return A named list with elements `start` and `end`, both POSIXct.
#' @export
#'
#' @examples
#' water_year_bounds(2023)
#' # $start  2023-10-01 UTC
#' # $end    2024-09-30 23:59:59 UTC
water_year_bounds <- function(year, tz = "UTC") {
  start <- as.POSIXct(paste0(year, "-10-01 00:00:00"), tz = tz)
  end   <- as.POSIXct(paste0(year + 1L, "-10-01 00:00:00"), tz = tz) - 1L
  list(start = start, end = end)
}

#' Generate water-year start datetimes for a range of years
#'
#' Returns a POSIXct vector containing the start instant (`YYYY-10-01
#' 00:00:00`) for every water year from `from_year` to `to_year`, inclusive.
#' Useful as iteration boundaries in multi-year pipeline loops.
#'
#' @param from_year Integer. First water year in the sequence.
#' @param to_year Integer. Last water year in the sequence.
#' @param tz Time zone string. Default `"UTC"`.
#'
#' @return A POSIXct vector, one element per water year.
#' @export
#'
#' @examples
#' water_year_seq(2020, 2023)
water_year_seq <- function(from_year, to_year, tz = "UTC") {
  years <- seq.int(from_year, to_year)
  as.POSIXct(paste0(years, "-10-01 00:00:00"), tz = tz)
}

#' Split a datetime vector (and optional values) by water year
#'
#' Partitions `x` into per-water-year subsets using [water_year()]. Returns a
#' named list keyed by water year integer.
#'
#' @param x A POSIXct vector, or a character vector coercible via [as_utc()].
#' @param values Optional numeric vector parallel to `x`. When supplied, each
#'   list element is a `data.frame(timestamp, value)`; when `NULL`, each element
#'   is a POSIXct vector.
#'
#' @return A named list with one element per water year present in `x`.
#' @export
#'
#' @examples
#' x <- as_utc(c("2023-06-01", "2023-11-01", "2024-03-01", "2024-11-01"))
#' split_water_years(x)
#'
#' split_water_years(x, values = c(1.1, 2.2, 3.3, 4.4))
split_water_years <- function(x, values = NULL) {
  if (!inherits(x, "POSIXct")) x <- as_utc(x)
  wy    <- water_year(x)
  years <- sort(unique(wy))
  out   <- lapply(years, function(yr) {
    idx <- which(wy == yr)
    if (is.null(values)) {
      x[idx]
    } else {
      data.frame(timestamp = x[idx], value = values[idx])
    }
  })
  names(out) <- as.character(years)
  out
}

#' Find water years with a complete regular series
#'
#' Returns the water years for which `x` contains every expected timestamp from
#' 1 October through 30 September at the given step. Uses [water_year_bounds()]
#' and [is_complete_series()] internally.
#'
#' @param x A POSIXct vector, or a character vector coercible via [as_utc()].
#' @param by Expected interval string, e.g. `"15 mins"`, `"1 hour"`. Default
#'   `"15 mins"`.
#'
#' @return An integer vector of water years that have complete coverage.
#'   Returns `integer(0)` if none are complete.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- seq_datetime("2022-10-01", "2024-09-30 23:45:00", by = "15 mins")
#' complete_water_years(x)
#' # [1] 2022 2023
#' }
complete_water_years <- function(x, by = "15 mins") {
  if (!inherits(x, "POSIXct")) x <- as_utc(x)
  wy    <- water_year(x)
  years <- sort(unique(wy))
  Filter(function(yr) {
    bounds <- water_year_bounds(yr)
    idx    <- which(wy == yr)
    is_complete_series(x[idx], from = bounds$start, to = bounds$end, by = by)
  }, years)
}

#' Label datetimes with a hydrological season
#'
#' Assigns each element of `x` to a season according to one of three schemes:
#'
#' * `"ea_quarter"` — EA water-year quarters: Q1 (Oct–Dec), Q2 (Jan–Mar),
#'   Q3 (Apr–Jun), Q4 (Jul–Sep).
#' * `"meteorological"` — standard UK met seasons: Winter (Dec–Feb),
#'   Spring (Mar–May), Summer (Jun–Aug), Autumn (Sep–Nov).
#' * `"hydrological"` — EA wet/dry halves: wet (Oct–Mar), dry (Apr–Sep).
#'
#' @param x A POSIXct vector.
#' @param scheme Season labelling scheme. One of `"ea_quarter"`,
#'   `"meteorological"`, or `"hydrological"`. Default `"ea_quarter"`.
#'
#' @return A factor the same length as `x`, with levels ordered from the start
#'   of the water year.
#' @export
#'
#' @examples
#' x <- as_utc(c("2024-01-15", "2024-04-15", "2024-07-15", "2024-10-15"))
#' label_season(x)
#' label_season(x, scheme = "meteorological")
#' label_season(x, scheme = "hydrological")
label_season <- function(x, scheme = c("ea_quarter", "meteorological", "hydrological")) {
  scheme <- match.arg(scheme)
  m <- as.integer(format(x, "%m", tz = "UTC"))

  if (scheme == "ea_quarter") {
    lookup <- c("Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4","Q1","Q1","Q1")
    lvls   <- c("Q1", "Q2", "Q3", "Q4")
  } else if (scheme == "meteorological") {
    lookup <- c("Winter","Winter","Spring","Spring","Spring",
                "Summer","Summer","Summer","Autumn","Autumn","Autumn","Winter")
    lvls   <- c("Winter", "Spring", "Summer", "Autumn")
  } else {
    lookup <- c("wet","wet","wet","dry","dry","dry","dry","dry","dry","wet","wet","wet")
    lvls   <- c("wet", "dry")
  }

  factor(lookup[m], levels = lvls)
}
