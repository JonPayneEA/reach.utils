#' Log an informational message
#'
#' Emits a timestamped informational message via [cli::cli_inform()].
#'
#' @param msg A cli-formatted message string.
#' @param ... Additional arguments passed to [cli::cli_inform()].
#'
#' @return Invisibly `NULL`.
#' @export
#'
#' @examples
#' log_info("Pipeline started for site {.val ABCD}")
log_info <- function(msg, ...) {
  ts <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  cli::cli_inform(paste(ts, msg), ...)
  invisible(NULL)
}

#' Log a warning message
#'
#' Emits a timestamped warning message via [cli::cli_warn()].
#'
#' @param msg A cli-formatted message string.
#' @param ... Additional arguments passed to [cli::cli_warn()].
#'
#' @return Invisibly `NULL`.
#' @export
#'
#' @examples
#' log_warn("Missing values detected in site {.val ABCD}")
log_warn <- function(msg, ...) {
  ts <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  cli::cli_warn(paste(ts, msg), ...)
  invisible(NULL)
}

#' Log an error message and abort
#'
#' Emits a timestamped error and aborts execution via [cli::cli_abort()].
#'
#' @param msg A cli-formatted message string.
#' @param ... Additional arguments passed to [cli::cli_abort()].
#'
#' @return Does not return; always throws an error.
#' @export
#'
#' @examples
#' \dontrun{
#' log_error("Could not connect to API for site {.val ABCD}")
#' }
log_error <- function(msg, ...) {
  ts <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  cli::cli_abort(paste(ts, msg), ...)
}

#' Append a log message to a file
#'
#' Writes a plain-text timestamped log line to a file, creating it if it does
#' not exist. Designed for use in long-running pipeline jobs where console
#' output may not be captured.
#'
#' @param path Path to the log file.
#' @param msg A plain character string to write (cli markup is stripped).
#'
#' @return Invisibly `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' log_to_file("logs/pipeline.log", "Backfill completed for site ABCD")
#' }
log_to_file <- function(path, msg) {
  ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  cat(paste0("[", ts, "] ", msg, "\n"), file = path, append = TRUE)
  invisible(NULL)
}

#' Log a debug message
#'
#' Emits a timestamped debug message via [cli::cli_inform()], but only when
#' the option `reach.utils.log_level` is set to `"debug"`. Use this for
#' verbose diagnostic output that should be suppressed in production.
#'
#' @param msg A cli-formatted message string.
#' @param ... Additional arguments passed to [cli::cli_inform()].
#'
#' @return Invisibly `NULL`.
#' @export
#'
#' @examples
#' options(reach.utils.log_level = "debug")
#' log_debug("Processing {.val 42} rows")
#' options(reach.utils.log_level = NULL)
log_debug <- function(msg, ...) {
  if (!identical(getOption("reach.utils.log_level"), "debug")) {
    return(invisible(NULL))
  }
  ts <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  cli::cli_inform(paste(ts, "[DEBUG]", msg), ...)
  invisible(NULL)
}

#' Log the execution time of an expression
#'
#' Evaluates `expr`, logs the elapsed wall-clock time with a label via
#' [log_info()], and returns the result of `expr` invisibly. Useful for
#' timing slow pipeline steps without cluttering call sites with boilerplate.
#'
#' @param expr An R expression to evaluate and time.
#' @param label A character string identifying the timed block in the log.
#'   Defaults to `"operation"`.
#'
#' @return The result of `expr`, invisibly.
#' @export
#'
#' @examples
#' log_timed(Sys.sleep(0.05), "short pause")
log_timed <- function(expr, label = "operation") {
  start   <- proc.time()[["elapsed"]]
  result  <- expr
  elapsed <- proc.time()[["elapsed"]] - start
  log_info("{label} completed in {format_duration(elapsed)}")
  invisible(result)
}
