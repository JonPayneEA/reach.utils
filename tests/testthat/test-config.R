test_that("get_config_val retrieves a top-level key", {
  cfg <- list(api_url = "https://example.com", timeout = 30L)
  expect_equal(get_config_val(cfg, "api_url"), "https://example.com")
})

test_that("get_config_val retrieves a nested key", {
  cfg <- list(database = list(host = "localhost", port = 5432L))
  expect_equal(get_config_val(cfg, c("database", "host")), "localhost")
})

test_that("get_config_val returns default for missing key", {
  cfg <- list(a = 1)
  expect_equal(get_config_val(cfg, "missing", default = "fallback"), "fallback")
})

test_that("validate_config passes when all keys present", {
  cfg <- list(api_url = "x", timeout = 30L)
  expect_invisible(validate_config(cfg, c("api_url", "timeout")))
})

test_that("validate_config errors on missing keys", {
  cfg <- list(api_url = "x")
  expect_error(validate_config(cfg, c("api_url", "timeout")))
})
