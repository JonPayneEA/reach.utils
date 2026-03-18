test_that("as_utc parses ISO 8601 strings to UTC POSIXct", {
  x <- as_utc("2024-01-15 06:00:00")
  expect_s3_class(x, "POSIXct")
  expect_equal(attr(x, "tzone"), "UTC")
})

test_that("as_utc accepts a custom format string", {
  x <- as_utc("15/01/2024 06:00", fmt = "%d/%m/%Y %H:%M")
  expect_s3_class(x, "POSIXct")
  expect_equal(format(x, "%Y-%m-%d %H:%M", tz = "UTC"), "2024-01-15 06:00")
})

test_that("parse_datetime handles multiple EA formats", {
  formats <- c(
    "2024-01-15T06:00:00Z",
    "2024-01-15 06:00:00",
    "15/01/2024 06:00",
    "2024-01-15"
  )
  result <- parse_datetime(formats)
  expect_s3_class(result, "POSIXct")
  expect_equal(sum(is.na(result)), 0L)
})

test_that("floor_to floors datetime to 15 minutes", {
  x   <- as_utc("2024-01-15 06:37:22")
  out <- floor_to(x, "15 minutes")
  expect_equal(format(out, "%H:%M", tz = "UTC"), "06:30")
})

test_that("seq_datetime generates correct length sequence", {
  s <- seq_datetime("2024-01-01", "2024-01-01 02:00:00", by = "1 hour")
  expect_length(s, 3L)
  expect_s3_class(s, "POSIXct")
})
