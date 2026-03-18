#' Load a YAML or JSON configuration file
#'
#' Reads a `.yaml`, `.yml`, or `.json` configuration file and returns a named
#' list. The file type is determined by the file extension.
#'
#' @param path Path to the config file.
#'
#' @return A named list of configuration values.
#' @export
#'
#' @examples
#' \dontrun{
#' cfg <- load_config("config/pipeline.yaml")
#' }
load_config <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Config file not found: {.path {path}}")
  }

  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("yaml", "yml")) {
    cfg <- yaml::read_yaml(path)
  } else if (ext == "json") {
    cfg <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  } else {
    cli::cli_abort(
      "Unsupported config format {.val {ext}}. Use .yaml, .yml, or .json."
    )
  }

  cli::cli_inform("Config loaded from {.path {path}}")
  cfg
}

#' Safely extract a value from a config list
#'
#' Retrieves a value from a named config list by key, returning a default if
#' the key is absent. Supports nested keys using `$` notation as a character
#' vector.
#'
#' @param cfg A named list, typically from [load_config()].
#' @param key A character string (top-level key) or character vector
#'   (nested path, e.g. `c("database", "host")`).
#' @param default Value to return if `key` is not found. Defaults to `NULL`.
#'
#' @return The config value, or `default` if not found.
#' @export
#'
#' @examples
#' cfg <- list(database = list(host = "localhost", port = 5432))
#' get_config_val(cfg, c("database", "host"))
#' get_config_val(cfg, "missing_key", default = "fallback")
get_config_val <- function(cfg, key, default = NULL) {
  val <- tryCatch(
    Reduce(`[[`, key, init = cfg),
    error = function(e) NULL
  )
  if (is.null(val)) default else val
}

#' Validate that required keys are present in a config list
#'
#' Checks a config list for a set of required top-level keys and raises an
#' informative error listing any that are missing.
#'
#' @param cfg A named list, typically from [load_config()].
#' @param required_keys A character vector of required key names.
#'
#' @return `cfg` invisibly if all keys are present.
#' @export
#'
#' @examples
#' cfg <- list(api_url = "https://example.com", timeout = 30)
#' validate_config(cfg, c("api_url", "timeout"))
validate_config <- function(cfg, required_keys) {
  missing <- setdiff(required_keys, names(cfg))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Config is missing required key{?s}:",
      setNames(paste0("{.val ", missing, "}"), rep("x", length(missing)))
    ))
  }
  invisible(cfg)
}
