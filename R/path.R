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

#' Find the most recently modified file matching a pattern
#'
#' Lists files in `dir` whose names match `pattern` (a glob) and returns the
#' path of the one with the latest modification time. Useful in pipelines that
#' consume "latest output" files without hardcoded names.
#'
#' @param dir Path to the directory to search.
#' @param pattern A glob pattern, e.g. `"*.csv"` or `"flow_*.parquet"`.
#'   Defaults to `"*"` (all files).
#'
#' @return A single character path string.
#' @export
#'
#' @examples
#' \dontrun{
#' find_latest_file("outputs/", "flow_*.csv")
#' }
find_latest_file <- function(dir, pattern = "*") {
  if (!dir.exists(dir)) {
    cli::cli_abort("Directory not found: {.path {dir}}")
  }
  files <- list.files(dir, pattern = glob2rx(pattern), full.names = TRUE)
  if (length(files) == 0L) {
    cli::cli_abort(
      "No files matching {.val {pattern}} found in {.path {dir}}"
    )
  }
  files[which.max(file.mtime(files))]
}

#' Swap the extension of a file path
#'
#' Replaces the file extension of `path` with `ext`, leaving the directory and
#' base name unchanged. Useful when deriving an output path from an input path
#' (e.g. reading a `.csv` and writing a `.parquet`).
#'
#' @param path A character file path.
#' @param ext The new extension, with or without a leading dot
#'   (e.g. `"parquet"` or `".parquet"`).
#'
#' @return A character path with the extension replaced.
#' @export
#'
#' @examples
#' swap_ext("data/flow.csv", "parquet")   # "data/flow.parquet"
#' swap_ext("outputs/run.tar.gz", "zip")  # "outputs/run.zip"
swap_ext <- function(path, ext) {
  ext  <- sub("^\\.+", "", ext)
  base <- tools::file_path_sans_ext(path)
  paste0(base, ".", ext)
}
