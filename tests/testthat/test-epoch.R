# water_year_bounds -----------------------------------------------------------

test_that("water_year_bounds returns the correct start and end instants", {
  bounds <- water_year_bounds(2023)
  expect_equal(format(bounds$start, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2023-10-01 00:00:00")
  expect_equal(format(bounds$end, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2024-09-30 23:59:59")
})

test_that("water_year_bounds returns POSIXct elements", {
  bounds <- water_year_bounds(2023)
  expect_s3_class(bounds$start, "POSIXct")
  expect_s3_class(bounds$end, "POSIXct")
})

# water_year_seq ----------------------------------------------------------

test_that("water_year_seq generates one start instant per water year", {
  s <- water_year_seq(2020, 2023)
  expect_length(s, 4L)
  expect_equal(format(s, "%Y-%m-%d", tz = "UTC"),
               c("2020-10-01", "2021-10-01", "2022-10-01", "2023-10-01"))
})

# split_water_years -----------------------------------------------------------

test_that("split_water_years partitions a datetime vector by water year", {
  x <- as_utc(c("2023-06-01", "2023-11-01", "2024-03-01", "2024-11-01"))
  result <- split_water_years(x)
  expect_named(result, c("2022", "2023", "2024"))
  expect_length(result[["2022"]], 1L)
  expect_length(result[["2023"]], 2L)
  expect_length(result[["2024"]], 1L)
})

test_that("split_water_years attaches parallel values as a data frame per year", {
  x      <- as_utc(c("2023-06-01", "2023-11-01", "2024-03-01", "2024-11-01"))
  values <- c(1.1, 2.2, 3.3, 4.4)
  result <- split_water_years(x, values = values)
  expect_s3_class(result[["2022"]], "data.frame")
  expect_equal(result[["2022"]]$value, 1.1)
  expect_equal(result[["2023"]]$value, c(2.2, 3.3))
  expect_equal(result[["2024"]]$value, 4.4)
})

# complete_water_years ------------------------------------------------------

test_that("complete_water_years flags only water years with full coverage", {
  bounds_2022 <- water_year_bounds(2022)
  x_complete  <- seq_datetime(bounds_2022$start, bounds_2022$end, by = "1 day")

  bounds_2023   <- water_year_bounds(2023)
  x_incomplete  <- seq_datetime(bounds_2023$start, bounds_2023$end, by = "1 day")
  x_incomplete  <- x_incomplete[-2L]

  x <- c(x_complete, x_incomplete)

  expect_equal(complete_water_years(x, by = "1 day"), 2022L)
})

test_that("complete_water_years returns integer(0) when no year is complete", {
  bounds_2022  <- water_year_bounds(2022)
  x_incomplete <- seq_datetime(bounds_2022$start, bounds_2022$end, by = "1 day")[-1L]

  expect_equal(complete_water_years(x_incomplete, by = "1 day"), integer(0))
})

# label_season --------------------------------------------------------------

test_that("label_season assigns EA water-year quarters by default", {
  x <- as_utc(c("2024-01-15", "2024-04-15", "2024-07-15", "2024-10-15"))
  result <- label_season(x)
  expect_equal(as.character(result), c("Q2", "Q3", "Q4", "Q1"))
  expect_equal(levels(result), c("Q1", "Q2", "Q3", "Q4"))
})

test_that("label_season assigns UK meteorological seasons", {
  x <- as_utc(c("2024-01-15", "2024-04-15", "2024-07-15", "2024-10-15"))
  result <- label_season(x, scheme = "meteorological")
  expect_equal(as.character(result), c("Winter", "Spring", "Summer", "Autumn"))
})

test_that("label_season assigns EA wet/dry halves", {
  x <- as_utc(c("2024-01-15", "2024-04-15", "2024-07-15", "2024-10-15"))
  result <- label_season(x, scheme = "hydrological")
  expect_equal(as.character(result), c("wet", "dry", "dry", "wet"))
})

test_that("label_season errors on an invalid scheme", {
  x <- as_utc("2024-01-15")
  expect_error(label_season(x, scheme = "monthly"))
})
