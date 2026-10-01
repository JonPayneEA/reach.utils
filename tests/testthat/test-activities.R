test_that("register_activity stores a function under package::name", {
  register_activity("t_reg_basic", function() "hello", package = "reach.utils")
  expect_true("reach.utils::t_reg_basic" %in% list_activities())
})

test_that("register_activity errors when fn is not a function", {
  expect_error(
    register_activity("t_reg_bad", "not a function", package = "reach.utils"),
    regexp = "function"
  )
})

test_that("list_activities returns a sorted character vector", {
  register_activity("t_list_b", function() NULL, package = "reach.utils")
  register_activity("t_list_a", function() NULL, package = "reach.utils")
  activities <- list_activities()
  expect_type(activities, "character")
  expect_equal(activities, sort(activities))
})

test_that("a registered activity can be looked up and called", {
  register_activity("t_lookup", function(x) x * 2, package = "reach.utils")
  fn <- reach.utils:::.get_activity("reach.utils", "t_lookup")
  expect_equal(fn(21), 42)
})

test_that("looking up an unregistered activity errors informatively", {
  expect_error(
    reach.utils:::.get_activity("reach.utils", "t_does_not_exist"),
    regexp = "No activity"
  )
})
