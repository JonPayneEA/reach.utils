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
