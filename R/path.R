#' Ensure a directory exists
#'
#' Creates a directory (and all necessary parents) if it does not already
#' exist. Returns the path invisibly so calls can be chained inline.
#'
#' @param path Path to the directory to create.
#'
#' @return `path` invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' ensure_dir("outputs/plots")
#' write.csv(df, file.path(ensure_dir("outputs"), "results.csv"))
#' }
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

#' Check that a file exists
#'
#' Validates that a file exists at `path` and raises an informative error if
#' it does not. Designed to give clear failure messages at pipeline entry
#' points rather than cryptic downstream errors.
#'
#' @param path Path to the file.
#'
#' @return `path` invisibly if the file exists.
#' @export
#'
#' @examples
#' \dontrun{
#' check_file_exists("data/observations.csv")
#' }
check_file_exists <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }
  invisible(path)
}

#' Resolve and normalise a file path
#'
#' Combines path components with [base::file.path()] and normalises the
#' result via [base::normalizePath()], expanding `~` and collapsing redundant
#' `.` and `..` segments. Unlike [base::normalizePath()], the path does not
#' need to exist on disk.
#'
#' @param base A base directory or root path.
#' @param ... Additional path components to append.
#'
#' @return A single normalised character path string.
#' @export
#'
#' @examples
#' resolve_path("~", "projects", "reach", "data")
#' resolve_path("/srv/data", "..", "outputs")
resolve_path <- function(base, ...) {
  normalizePath(file.path(base, ...), mustWork = FALSE)
}
