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
