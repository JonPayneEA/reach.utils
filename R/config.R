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

#' Recursively merge configuration lists
#'
#' Merges two or more named lists, with values in later arguments taking
#' precedence over earlier ones. Nested lists are merged recursively, so only
#' the keys that differ need to be specified in an override config.
#'
#' @param ... Two or more named lists to merge (typically from [load_config()]).
#'
#' @return A single merged named list.
#' @export
#'
#' @examples
#' base    <- list(db = list(host = "localhost", port = 5432L), timeout = 30L)
#' overlay <- list(db = list(host = "prod-server"))
#' merge_configs(base, overlay)
merge_configs <- function(...) {
  configs <- list(...)
  Reduce(.merge_two_configs, configs)
}

.merge_two_configs <- function(x, y) {
  for (key in names(y)) {
    if (is.list(x[[key]]) && is.list(y[[key]])) {
      x[[key]] <- .merge_two_configs(x[[key]], y[[key]])
    } else {
      x[[key]] <- y[[key]]
    }
  }
  x
}

#' Retrieve and coerce a config value to a target type
#'
#' Extracts a value from a config list using [get_config_val()] and coerces
#' it to the specified R type. Raises an informative error if the value cannot
#' be coerced. Handles common YAML/JSON quirks such as logical values stored
#' as `"true"`/`"false"` strings.
#'
#' @param cfg A named list, typically from [load_config()].
#' @param key A character string or vector passed to [get_config_val()].
#' @param type One of `"character"`, `"integer"`, `"numeric"`, `"logical"`,
#'   or `"Date"`.
#' @param default Value to return if the key is absent (before coercion).
#'   Defaults to `NULL`.
#'
#' @return The config value coerced to `type`, or `NULL` if absent and no
#'   `default` is supplied.
#' @export
#'
#' @examples
#' cfg <- list(timeout = "30", debug = "true", start = "2024-01-01")
#' config_val_as(cfg, "timeout", "integer")
#' config_val_as(cfg, "debug",   "logical")
#' config_val_as(cfg, "start",   "Date")
config_val_as <- function(cfg, key, type, default = NULL) {
  val <- get_config_val(cfg, key, default = default)
  if (is.null(val)) return(NULL)

  result <- tryCatch(
    switch(type,
      character = as.character(val),
      integer   = as.integer(val),
      numeric   = as.numeric(val),
      logical   = {
        if (is.character(val)) {
          lv <- tolower(trimws(val))
          if      (lv %in% c("true",  "yes", "1")) TRUE
          else if (lv %in% c("false", "no",  "0")) FALSE
          else stop("cannot coerce")
        } else {
          as.logical(val)
        }
      },
      Date = as.Date(val),
      cli::cli_abort(
        "Unknown type {.val {type}}. Use character, integer, numeric, logical, or Date."
      )
    ),
    error = function(e) {
      cli::cli_abort(
        "Cannot coerce config key {.val {paste(key, collapse = '$')}} \\
         value {.val {val}} to {.val {type}}."
      )
    }
  )
  result
}

#' Build a config list from environment variables
#'
#' Reads environment variables whose names start with `<PREFIX>__` and
#' constructs a nested config list. Double underscores (`__`) in the variable
#' name denote nesting levels; all keys are lowercased.
#'
#' For example, with `prefix = "REACH"`:
#' - `REACH__DB__HOST=localhost` → `list(db = list(host = "localhost"))`
#' - `REACH__TIMEOUT=30`        → `list(timeout = "30")`
#'
#' Values are always character strings; use [config_val_as()] to coerce types.
#'
#' @param prefix A character string prefix (case-insensitive), without trailing
#'   underscores.
#'
#' @return A named list, or an empty list if no matching variables are found.
#' @export
#'
#' @examples
#' Sys.setenv(REACH__DB__HOST = "localhost", REACH__TIMEOUT = "30")
#' config_from_env("REACH")
#' Sys.unsetenv(c("REACH__DB__HOST", "REACH__TIMEOUT"))
config_from_env <- function(prefix) {
  pattern <- paste0("^", toupper(prefix), "__")
  all_env  <- Sys.getenv()
  matching <- all_env[grepl(pattern, names(all_env))]

  if (length(matching) == 0L) {
    cli::cli_warn(
      "No environment variables found with prefix {.val {toupper(prefix)}__}"
    )
    return(list())
  }

  names(matching) <- sub(pattern, "", names(matching))

  cfg <- list()
  for (var in names(matching)) {
    keys <- tolower(strsplit(var, "__", fixed = TRUE)[[1]])
    cfg  <- .set_nested(cfg, keys, matching[[var]])
  }
  cfg
}

.set_nested <- function(lst, keys, value) {
  if (length(keys) == 1L) {
    lst[[keys]] <- value
  } else {
    child        <- if (is.null(lst[[keys[1]]])) list() else lst[[keys[1]]]
    lst[[keys[1]]] <- .set_nested(child, keys[-1L], value)
  }
  lst
}

#' Expand relative paths in a config list
#'
#' Walks a config list recursively and resolves any character value that looks
#' like a relative path (contains `/` and is not a URL or absolute path)
#' against `base_dir`. Values that do not look like paths are left unchanged.
#'
#' @param cfg A named list, typically from [load_config()].
#' @param base_dir The base directory to resolve relative paths against.
#'   Defaults to the current working directory.
#'
#' @return A config list with relative path values expanded to absolute paths.
#' @export
#'
#' @examples
#' cfg <- list(input = "./data/flow.csv", db = list(host = "localhost"))
#' expand_config_paths(cfg, "/srv/pipeline")
expand_config_paths <- function(cfg, base_dir = getwd()) {
  .expand_paths_recursive(cfg, base_dir)
}

.expand_paths_recursive <- function(x, base_dir) {
  if (is.list(x)) {
    lapply(x, .expand_paths_recursive, base_dir = base_dir)
  } else if (is.character(x) && length(x) == 1L && .is_relative_path(x)) {
    normalizePath(file.path(base_dir, x), mustWork = FALSE)
  } else {
    x
  }
}

.is_relative_path <- function(x) {
  grepl("^\\./|^\\.\\./", x) ||
    (grepl("/", x, fixed = TRUE) &&
       !grepl("^https?://", x) &&
       !grepl("^/", x))
}
