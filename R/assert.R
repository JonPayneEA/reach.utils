#' Assert that an object is a POSIXct vector
#'
#' Aborts with an informative error if `x` does not inherit from `"POSIXct"`.
#' Designed to guard pipeline entry points so failures surface early with clear
#' messages rather than as cryptic downstream errors.
#'
#' @param x The object to check.
#' @param arg Name of the argument, used in the error message. Defaults to the
#'   expression passed as `x`.
#'
#' @return `x` invisibly if the assertion passes.
#' @export
#'
#' @examples
#' ts <- as.POSIXct("2024-01-01", tz = "UTC")
#' assert_posixct(ts)
assert_posixct <- function(x, arg = deparse(substitute(x))) {
  if (!inherits(x, "POSIXct")) {
    cli::cli_abort(
      "{.arg {arg}} must be a {.cls POSIXct} vector, not {.cls {class(x)[[1L]]}}."
    )
  }
  invisible(x)
}

#' Assert that an object is numeric
#'
#' Aborts with an informative error if `x` is not a numeric vector. Optionally
#' also aborts if `x` contains any `NA` values.
#'
#' @param x The object to check.
#' @param arg Name of the argument, used in the error message.
#' @param allow_na Logical. If `FALSE`, aborts when `x` contains any `NA`.
#'   Default `TRUE`.
#'
#' @return `x` invisibly if the assertion passes.
#' @export
#'
#' @examples
#' assert_numeric(c(1.2, 3.4, 5.6))
#' assert_numeric(c(1, 2, NA), allow_na = TRUE)
assert_numeric <- function(x, arg = deparse(substitute(x)), allow_na = TRUE) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a numeric vector, not {.cls {class(x)[[1L]]}}."
    )
  }
  if (!allow_na && anyNA(x)) {
    cli::cli_abort("{.arg {arg}} must not contain {.val NA} values.")
  }
  invisible(x)
}

#' Assert that an object has length one
#'
#' Aborts if `length(x) != 1`. Useful for guarding scalar arguments such as
#' thresholds and configuration values.
#'
#' @param x The object to check.
#' @param arg Name of the argument, used in the error message.
#'
#' @return `x` invisibly if the assertion passes.
#' @export
#'
#' @examples
#' assert_scalar(42)
#' assert_scalar("hourly")
assert_scalar <- function(x, arg = deparse(substitute(x))) {
  if (length(x) != 1L) {
    cli::cli_abort(
      "{.arg {arg}} must be a scalar (length 1), not length {length(x)}."
    )
  }
  invisible(x)
}

#' Assert that an object has a specific length
#'
#' Aborts if `length(x) != n`. More specific than [assert_scalar()] when an
#' exact fixed length is required.
#'
#' @param x The object to check.
#' @param n Expected length (single positive integer).
#' @param arg Name of the argument, used in the error message.
#'
#' @return `x` invisibly if the assertion passes.
#' @export
#'
#' @examples
#' assert_length(c(0, 1), n = 2L)
assert_length <- function(x, n, arg = deparse(substitute(x))) {
  if (length(x) != n) {
    cli::cli_abort(
      "{.arg {arg}} must have length {n}, not {length(x)}."
    )
  }
  invisible(x)
}

#' Assert that all supplied vectors have the same length
#'
#' Aborts if any two of the vectors passed via `...` differ in length. Designed
#' to guard `(x, values)` pairs that must be parallel — the most common source
#' of silent data misalignment in pipeline code.
#'
#' @param ... Two or more vectors to compare.
#' @param args Optional character vector of argument names used in the error
#'   message. When `NULL`, positional labels (`arg1`, `arg2`, …) are used.
#'
#' @return `NULL` invisibly if the assertion passes.
#' @export
#'
#' @examples
#' x      <- as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC")
#' values <- c(1.2, 3.4)
#' assert_same_length(x, values, args = c("x", "values"))
assert_same_length <- function(..., args = NULL) {
  dots <- list(...)
  lens <- lengths(dots)
  if (length(unique(lens)) == 1L) return(invisible(NULL))
  labels <- if (is.null(args)) paste0("arg", seq_along(dots)) else args
  cli::cli_abort(
    c(
      "All arguments must have the same length.",
      "i" = "Lengths: {paste(paste0(labels, ' = ', lens), collapse = ', ')}"
    )
  )
}

#' Assert that a value is one of a set of valid choices
#'
#' Aborts with an informative error listing the valid options if `x` is not
#' contained in `choices`. More informative than [base::match.arg()] for
#' pipeline error messages.
#'
#' @param x A scalar value to check.
#' @param choices Character vector of valid values.
#' @param arg Name of the argument, used in the error message.
#'
#' @return `x` invisibly if the assertion passes.
#' @export
#'
#' @examples
#' assert_choice("linear", c("linear", "locf", "constant"))
assert_choice <- function(x, choices, arg = deparse(substitute(x))) {
  if (!x %in% choices) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be one of {.or {.val {choices}}}.",
        "x" = "Got {.val {x}}."
      )
    )
  }
  invisible(x)
}
