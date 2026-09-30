.write_pipeline_yaml <- function(lines) {
  path <- tempfile(fileext = ".yml")
  writeLines(lines, path)
  path
}

test_that("run_pipeline dispatches activities in order and returns their results", {
  register_activity("t_run_double", function(x) x * 2, package = "reach.utils")
  register_activity("t_run_square", function(x) x ^ 2, package = "reach.utils")

  path <- .write_pipeline_yaml(c(
    "activities:",
    "  - name: t_run_double",
    "    package: reach.utils",
    "    args:",
    "      x: 3",
    "  - name: t_run_square",
    "    package: reach.utils",
    "    args:",
    "      x: 4"
  ))

  result <- run_pipeline(path)
  expect_equal(result[["reach.utils::t_run_double"]], 6)
  expect_equal(result[["reach.utils::t_run_square"]], 16)
  expect_equal(names(result), c("reach.utils::t_run_double", "reach.utils::t_run_square"))
})

test_that("run_pipeline injects global values into unset matching formals", {
  register_activity(
    "t_run_scale",
    function(x, multiplier) x * multiplier,
    package = "reach.utils"
  )

  path <- .write_pipeline_yaml(c(
    "global:",
    "  multiplier: 10",
    "activities:",
    "  - name: t_run_scale",
    "    package: reach.utils",
    "    args:",
    "      x: 5"
  ))

  result <- run_pipeline(path)
  expect_equal(result[["reach.utils::t_run_scale"]], 50)
})

test_that("run_pipeline lets an explicit arg override a same-named global value", {
  register_activity(
    "t_run_override",
    function(x, multiplier) x * multiplier,
    package = "reach.utils"
  )

  path <- .write_pipeline_yaml(c(
    "global:",
    "  multiplier: 10",
    "activities:",
    "  - name: t_run_override",
    "    package: reach.utils",
    "    args:",
    "      x: 5",
    "      multiplier: 2"
  ))

  result <- run_pipeline(path)
  expect_equal(result[["reach.utils::t_run_override"]], 10)
})

test_that("run_pipeline errors when the activities key is missing", {
  path <- .write_pipeline_yaml(c("global:", "  multiplier: 10"))
  expect_error(run_pipeline(path), regexp = "activities")
})

test_that("run_pipeline errors when activities is an empty list", {
  path <- .write_pipeline_yaml(c("activities: []"))
  expect_error(run_pipeline(path), regexp = "non-empty")
})

test_that("run_pipeline errors when an activity entry is missing package", {
  path <- .write_pipeline_yaml(c(
    "activities:",
    "  - name: t_run_double"
  ))
  expect_error(run_pipeline(path), regexp = "name.+package|package.+name")
})

test_that("run_pipeline errors when an activity is not registered", {
  path <- .write_pipeline_yaml(c(
    "activities:",
    "  - name: t_does_not_exist_anywhere",
    "    package: reach.utils"
  ))
  expect_error(run_pipeline(path), regexp = "No activity")
})

test_that("run_pipeline stops immediately and does not run later activities on failure", {
  ran_second <- FALSE
  register_activity(
    "t_run_fails",
    function() stop("deliberate failure"),
    package = "reach.utils"
  )
  register_activity(
    "t_run_after_failure",
    function() ran_second <<- TRUE,
    package = "reach.utils"
  )

  path <- .write_pipeline_yaml(c(
    "activities:",
    "  - name: t_run_fails",
    "    package: reach.utils",
    "  - name: t_run_after_failure",
    "    package: reach.utils"
  ))

  expect_error(run_pipeline(path), regexp = "t_run_fails.*failed")
  expect_false(ran_second)
})

test_that("run_pipeline applies global log_level via options()", {
  register_activity("t_run_loglevel", function() NULL, package = "reach.utils")
  on.exit(options(reach.utils.log_level = NULL))

  path <- .write_pipeline_yaml(c(
    "global:",
    "  log_level: debug",
    "activities:",
    "  - name: t_run_loglevel",
    "    package: reach.utils"
  ))

  run_pipeline(path)
  expect_equal(getOption("reach.utils.log_level"), "debug")
})
