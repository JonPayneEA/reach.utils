# ============================================================ #
# Tool:         Activity registry
# Description:  Package-level registry mapping <package>::<name> activity
#               keys to callable functions, for dispatch from run_pipeline()
# Flode Module: reach.utils
# Author:       Forecasting and Warning Team, forecasting@environment-agency.gov.uk
# Created:      2026-09-30
# Modified:     2026-09-30 - JP: initial version
# Tier:         3
# Inputs:       Activity name, function, and registering package name
# Outputs:      None (side effect: populates the in-session registry)
# Dependencies: cli
# ============================================================ #

.activity_registry <- new.env(parent = emptyenv())

#' Register an activity for dispatch from a pipeline YAML
#'
#' Adds a function to the package-level activity registry under a
#' `<package>::<name>` key, so [run_pipeline()] can dispatch to it by name.
#' Intended to be called once from the registering package's own
#' `.onLoad()`, using the `pkgname` argument `.onLoad` receives.
#'
#' @param name A character string activity name, unique within `package`.
#' @param fn A function to call when the activity is dispatched.
#' @param package A character string naming the registering package.
#'
#' @return Invisibly `TRUE`.
#' @export
#'
#' @examples
#' register_activity("noop", function() invisible(NULL), package = "reach.utils")
#' list_activities()
register_activity <- function(name, fn, package) {
  if (!is.function(fn)) {
    cli::cli_abort("{.arg fn} must be a function, not {.cls {class(fn)}}.")
  }
  key <- paste0(package, "::", name)
  assign(key, fn, envir = .activity_registry)
  invisible(TRUE)
}

#' List all currently registered activities
#'
#' Returns the `<package>::<name>` keys of every activity registered via
#' [register_activity()] in the current R session.
#'
#' @return A sorted character vector of registered activity keys. An empty
#'   character vector if none are registered.
#' @export
#'
#' @examples
#' register_activity("noop", function() invisible(NULL), package = "reach.utils")
#' list_activities()
list_activities <- function() {
  sort(ls(envir = .activity_registry))
}

.get_activity <- function(package, name) {
  key <- paste0(package, "::", name)
  if (!exists(key, envir = .activity_registry, inherits = FALSE)) {
    cli::cli_abort(c(
      "No activity {.val {name}} registered for package {.pkg {package}}.",
      "i" = "Registered activities: {.val {list_activities()}}"
    ))
  }
  get(key, envir = .activity_registry, inherits = FALSE)
}
