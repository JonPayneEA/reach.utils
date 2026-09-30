# ============================================================ #
# Tool:         Pipeline runner
# Description:  Loads a pipeline YAML config and dispatches each listed
#               activity, in order, to its registered function
# Flode Module: reach.utils
# Author:       Forecasting and Warning Team, forecasting@environment-agency.gov.uk
# Created:      2026-09-30
# Modified:     2026-09-30 - JP: initial version
# Tier:         3
# Inputs:       Path to a pipeline YAML config file
# Outputs:      Return values of each dispatched activity (a named list)
# Dependencies: cli, yaml (via load_config/expand_config_paths)
# ============================================================ #

#' Run a pipeline of registered activities from a YAML config
#'
#' Loads a pipeline YAML config, resolves a shared `global` block, then
#' dispatches each entry in `activities` in order to the function registered
#' for it via [register_activity()]. Stops immediately on the first failing
#' activity, or if any listed activity cannot be resolved.
#'
#' Each entry in `activities` must have `name` and `package` keys and may
#' have an `args` key (a named list passed to the target function). Any of
#' the target function's arguments left unset in `args` are filled in from
#' the top-level `global` block when a same-named key exists there. Relative
#' paths in `global` are expanded against the config file's own directory.
#' A `global$log_level` value, if present, is applied via
#' `options(reach.utils.log_level = ...)` before any activity runs.
#'
#' @param path Path to a pipeline YAML config file.
#'
#' @return A named list of each activity's return value, named
#'   `<package>::<name>` in run order. Returned invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' run_pipeline("config/pipeline.yaml")
#' }
run_pipeline <- function(path) {
  cfg <- load_config(path)
  validate_config(cfg, "activities")

  global <- cfg$global
  if (is.null(global)) global <- list()
  global <- expand_config_paths(global, base_dir = dirname(normalizePath(path)))
  if (!is.null(global$log_level)) {
    options(reach.utils.log_level = global$log_level)
  }

  activities <- cfg$activities
  if (!is.list(activities) || length(activities) == 0L) {
    cli::cli_abort("Config key {.val activities} must be a non-empty list.")
  }

  # Resolve every activity up front so a typo further down the pipeline
  # aborts before any earlier activity has run.
  plan <- lapply(activities, .resolve_activity, global = global)

  results <- list()
  for (step in plan) {
    log_section(step$key)
    log_info("Starting activity {.val {step$key}}")
    result <- tryCatch(
      log_timed(do.call(step$fn, step$args), label = step$key),
      error = function(e) {
        cli::cli_abort(
          "Activity {.val {step$key}} failed: {conditionMessage(e)}",
          call = NULL
        )
      }
    )
    results[[step$key]] <- result
    log_info("Completed activity {.val {step$key}}")
  }

  invisible(results)
}

.resolve_activity <- function(activity, global) {
  if (is.null(activity$name) || is.null(activity$package)) {
    cli::cli_abort("Each activity must have {.field name} and {.field package} keys.")
  }

  if (!requireNamespace(activity$package, quietly = TRUE)) {
    cli::cli_abort("Package {.pkg {activity$package}} is not installed.")
  }

  fn  <- .get_activity(activity$package, activity$name)
  key <- paste0(activity$package, "::", activity$name)

  args <- activity$args
  if (is.null(args)) args <- list()
  args <- .inject_globals(fn, args, global)

  list(key = key, fn = fn, args = args)
}

.inject_globals <- function(fn, args, global) {
  if (length(global) == 0L) return(args)
  missing_formals <- setdiff(names(formals(fn)), c(names(args), "..."))
  for (nm in missing_formals) {
    if (!is.null(global[[nm]])) args[[nm]] <- global[[nm]]
  }
  args
}
