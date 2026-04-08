x_reg <- function() seq_datetime("2024-01-01", "2024-01-01 01:00", "15 mins")

# check_duplicates --------------------------------------------------------

test_that("check_duplicates returns zero rows for unique timestamps", {
  expect_equal(nrow(check_duplicates(x_reg())), 0L)
})

test_that("check_duplicates flags a repeated timestamp", {
  x <- as_utc(c("2024-01-01 00:00", "2024-01-01 00:15",
                "2024-01-01 00:15", "2024-01-01 00:30"))
  result <- check_duplicates(x)
  expect_equal(nrow(result), 1L)
  expect_equal(result$n_occurrences, 2L)
})

test_that("check_duplicates returns expected columns when empty", {
  expect_named(check_duplicates(x_reg()), c("timestamp", "n_occurrences"))
})

# check_monotonic ---------------------------------------------------------

test_that("check_monotonic returns zero rows for ascending timestamps", {
  expect_equal(nrow(check_monotonic(x_reg())), 0L)
})

test_that("check_monotonic flags an out-of-order timestamp", {
  x <- as_utc(c("2024-01-01 00:00", "2024-01-01 00:30",
                "2024-01-01 00:15", "2024-01-01 00:45"))
  result <- check_monotonic(x)
  expect_equal(nrow(result), 1L)
  expect_equal(format(result$timestamp, "%H:%M", tz = "UTC"), "00:15")
})

test_that("check_monotonic returns expected columns when empty", {
  expect_named(check_monotonic(x_reg()), c("timestamp", "index"))
})

# check_bounds ------------------------------------------------------------

test_that("check_bounds returns zero rows when all values are in range", {
  x      <- x_reg()
  values <- rep(1.0, length(x))
  expect_equal(nrow(check_bounds(x, values, min = 0, max = 5)), 0L)
})

test_that("check_bounds flags values below min", {
  x      <- x_reg()
  values <- c(-0.5, rep(1.0, length(x) - 1L))
  result <- check_bounds(x, values, min = 0, max = 5)
  expect_equal(nrow(result), 1L)
  expect_equal(result$value, -0.5)
})

test_that("check_bounds flags values above max", {
  x      <- x_reg()
  values <- c(rep(1.0, length(x) - 1L), 99.0)
  result <- check_bounds(x, values, min = 0, max = 5)
  expect_equal(nrow(result), 1L)
})

test_that("check_bounds skips NA values", {
  x      <- x_reg()
  values <- c(NA, rep(1.0, length(x) - 1L))
  expect_equal(nrow(check_bounds(x, values, min = 0, max = 5)), 0L)
})

# check_flatline ----------------------------------------------------------

test_that("check_flatline returns zero rows when no flatline exists", {
  x      <- x_reg()
  values <- seq_len(length(x)) * 0.1
  expect_equal(nrow(check_flatline(x, values, n = 3)), 0L)
})

test_that("check_flatline detects a run of identical values", {
  x      <- x_reg()
  values <- c(1.0, 2.0, 2.0, 2.0, 2.0, 3.0)
  result <- check_flatline(x, values, n = 3)
  expect_equal(nrow(result), 1L)
  expect_equal(result$run_length, 4L)
  expect_equal(result$value, 2.0)
})

test_that("check_flatline skips NA values", {
  x      <- x_reg()
  values <- c(1.0, NA, NA, NA, NA, 2.0)
  expect_equal(nrow(check_flatline(x, values, n = 3)), 0L)
})

test_that("check_flatline returns expected columns when empty", {
  x <- x_reg()
  expect_named(
    check_flatline(x, rep(1:length(x) * 1.0), n = 99),
    c("run_start", "run_end", "value", "run_length")
  )
})

# check_rate_of_change ----------------------------------------------------

test_that("check_rate_of_change returns zero rows when all steps are small", {
  x      <- x_reg()
  values <- seq(1, 1 + (length(x) - 1) * 0.1, by = 0.1)
  expect_equal(nrow(check_rate_of_change(x, values, max_change = 1)), 0L)
})

test_that("check_rate_of_change flags a spike", {
  x      <- x_reg()
  values <- c(1.0, 1.1, 8.5, 1.2, 1.3, 1.1)
  result <- check_rate_of_change(x, values, max_change = 1.0)
  expect_equal(nrow(result), 2L)  # both the spike and the recovery
})

test_that("check_rate_of_change skips steps involving NA", {
  x      <- x_reg()
  values <- c(1.0, NA, 1.1, 1.2, 1.3, 1.1)
  expect_equal(nrow(check_rate_of_change(x, values, max_change = 1.0)), 0L)
})

test_that("check_rate_of_change returns expected columns when empty", {
  x      <- x_reg()
  values <- rep(1.0, length(x))
  expect_named(
    check_rate_of_change(x, values, max_change = 99),
    c("timestamp", "value", "change")
  )
})

# check_na_runs -----------------------------------------------------------

test_that("check_na_runs returns zero rows when no long NA runs exist", {
  x      <- x_reg()
  values <- c(1.0, NA, 1.1, 1.2, 1.3, 1.1)
  expect_equal(nrow(check_na_runs(x, values, max_run = 1)), 0L)
})

test_that("check_na_runs flags a run exceeding max_run", {
  x      <- x_reg()
  values <- c(1.0, NA, NA, NA, 1.5, 1.6)
  result <- check_na_runs(x, values, max_run = 1)
  expect_equal(nrow(result), 1L)
  expect_equal(result$run_length, 3L)
})

test_that("check_na_runs returns expected columns when empty", {
  x      <- x_reg()
  values <- rep(1.0, length(x))
  expect_named(
    check_na_runs(x, values),
    c("run_start", "run_end", "run_length")
  )
})

# qc_series ---------------------------------------------------------------

test_that("qc_series returns a named list with one element per check", {
  x      <- x_reg()
  values <- rep(1.0, length(x))
  result <- qc_series(x, values,
                      checks = c("duplicates", "flatline"),
                      flatline_n = 3L)
  expect_type(result, "list")
  expect_named(result, c("duplicates", "flatline"))
})

test_that("qc_series warns and skips bounds check when bounds not supplied", {
  x      <- x_reg()
  values <- rep(1.0, length(x))
  expect_warning(
    result <- qc_series(x, values, checks = "bounds"),
    regexp = "bounds"
  )
  expect_null(result$bounds)
})

test_that("qc_series warns and skips rate_of_change when max_change not supplied", {
  x      <- x_reg()
  values <- rep(1.0, length(x))
  expect_warning(
    result <- qc_series(x, values, checks = "rate_of_change"),
    regexp = "rate_of_change"
  )
  expect_null(result$rate_of_change)
})

test_that("qc_series passes all checks for clean data", {
  x      <- x_reg()
  values <- seq(1, length(x)) * 0.1
  result <- qc_series(x, values,
                      bounds     = c(0, 10),
                      max_change = 1,
                      flatline_n = 3L)
  expect_true(all(vapply(result, function(d) nrow(d) == 0L, logical(1L))))
})
