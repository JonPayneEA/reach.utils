# assert_posixct ------------------------------------------------------------

test_that("assert_posixct passes for a POSIXct vector", {
  ts <- as.POSIXct("2024-01-01", tz = "UTC")
  expect_identical(assert_posixct(ts), ts)
})

test_that("assert_posixct errors for a non-POSIXct object", {
  expect_error(assert_posixct("2024-01-01"), regexp = "POSIXct")
})

# assert_numeric --------------------------------------------------------------

test_that("assert_numeric passes for a numeric vector", {
  x <- c(1.2, 3.4, 5.6)
  expect_identical(assert_numeric(x), x)
})

test_that("assert_numeric errors for a non-numeric object", {
  expect_error(assert_numeric("not numeric"), regexp = "numeric")
})

test_that("assert_numeric allows NA by default", {
  expect_silent(assert_numeric(c(1, 2, NA)))
})

test_that("assert_numeric errors on NA when allow_na is FALSE", {
  expect_error(assert_numeric(c(1, 2, NA), allow_na = FALSE), regexp = "NA")
})

# assert_scalar -----------------------------------------------------------

test_that("assert_scalar passes for length-one objects", {
  expect_identical(assert_scalar(42), 42)
  expect_identical(assert_scalar("hourly"), "hourly")
})

test_that("assert_scalar errors for a vector of length != 1", {
  expect_error(assert_scalar(c(1, 2)), regexp = "length 1")
})

# assert_length -------------------------------------------------------------

test_that("assert_length passes when length matches n", {
  x <- c(0, 1)
  expect_identical(assert_length(x, n = 2L), x)
})

test_that("assert_length errors when length does not match n", {
  expect_error(assert_length(c(0, 1, 2), n = 2L), regexp = "length 2")
})

# assert_same_length --------------------------------------------------------

test_that("assert_same_length passes when all vectors share a length", {
  x      <- as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC")
  values <- c(1.2, 3.4)
  expect_null(assert_same_length(x, values))
})

test_that("assert_same_length errors when lengths differ", {
  x      <- as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC")
  values <- c(1.2, 3.4, 5.6)
  expect_error(assert_same_length(x, values), regexp = "same length")
})

test_that("assert_same_length uses supplied argument names in the error", {
  expect_error(
    assert_same_length(1:2, 1:3, args = c("x", "values")),
    regexp = "x = 2, values = 3"
  )
})

# assert_choice -------------------------------------------------------------

test_that("assert_choice passes when x is one of choices", {
  expect_identical(assert_choice("linear", c("linear", "locf", "constant")), "linear")
})

test_that("assert_choice errors when x is not one of choices", {
  expect_error(
    assert_choice("cubic", c("linear", "locf", "constant")),
    regexp = "cubic"
  )
})
