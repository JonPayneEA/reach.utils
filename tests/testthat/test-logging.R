test_that("log_info returns invisibly NULL", {
  expect_invisible(log_info("test info message"))
})

test_that("log_warn returns invisibly NULL (as warning)", {
  expect_warning(
    expect_invisible(log_warn("test warning")),
    regexp = "test warning"
  )
})

test_that("log_error throws an error", {
  expect_error(log_error("test error"), regexp = "test error")
})

test_that("log_to_file creates file and appends content", {
  tmp <- tempfile(fileext = ".log")
  on.exit(unlink(tmp))
  log_to_file(tmp, "first line")
  log_to_file(tmp, "second line")
  lines <- readLines(tmp)
  expect_length(lines, 2L)
  expect_true(grepl("first line",  lines[1]))
  expect_true(grepl("second line", lines[2]))
})

test_that("log_debug returns invisibly NULL when log level is not debug", {
  options(reach.utils.log_level = NULL)
  expect_invisible(log_debug("should not appear"))
})

test_that("log_debug emits a message when log level is debug", {
  options(reach.utils.log_level = "debug")
  on.exit(options(reach.utils.log_level = NULL))
  expect_message(log_debug("debug output"), regexp = "DEBUG")
})

test_that("log_timed returns the expression result invisibly", {
  result <- log_timed(1 + 1, "addition")
  expect_equal(result, 2L)
})

test_that("log_timed logs a message containing the label", {
  expect_message(log_timed(Sys.sleep(0), "test step"), regexp = "test step")
})

test_that("log_progress returns invisibly NULL", {
  expect_invisible(log_progress(1L, 10L, "site ABCD"))
})

test_that("log_progress message contains i, n, and label", {
  expect_message(
    log_progress(3L, 47L, label = "ABCD"),
    regexp = "3/47"
  )
})

test_that("log_section returns invisibly NULL", {
  expect_invisible(log_section("Test section"))
})
