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

test_that("water_year labels October onwards as the current year", {
  x <- as_utc(c("2024-10-01", "2025-09-30"))
  expect_equal(water_year(x), c(2024L, 2024L))
})

test_that("water_year labels pre-October as the previous year", {
  x <- as_utc("2024-09-30")
  expect_equal(water_year(x), 2023L)
})

test_that("detect_gaps returns zero rows when no gaps exist", {
  x <- as_utc(c("2024-01-01 00:00", "2024-01-01 00:15", "2024-01-01 00:30"))
  result <- detect_gaps(x, "15 mins")
  expect_equal(nrow(result), 0L)
})

test_that("detect_gaps identifies a single gap correctly", {
  x <- as_utc(c("2024-01-01 00:00", "2024-01-01 00:15",
                "2024-01-01 01:00", "2024-01-01 01:15"))
  result <- detect_gaps(x, "15 mins")
  expect_equal(nrow(result), 1L)
  expect_equal(result$n_missing, 2L)
})

test_that("detect_gaps returns empty data frame for fewer than 2 points", {
  result <- detect_gaps(as_utc("2024-01-01"), "15 mins")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("gap_start", "gap_end", "n_missing"))
})

test_that("format_duration formats hours, minutes, seconds", {
  expect_equal(format_duration(8070), "2h 14m 30s")
})

test_that("format_duration formats minutes and seconds only", {
  expect_equal(format_duration(90), "1m 30s")
})

test_that("format_duration formats seconds only", {
  expect_equal(format_duration(45), "45s")
})

test_that("format_duration formats sub-second durations", {
  expect_match(format_duration(0.4), "^0\\.40s$")
})
