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

test_that("merge_configs later values take precedence over earlier ones", {
  base    <- list(host = "localhost", port = 5432L)
  overlay <- list(host = "prod-server")
  result  <- merge_configs(base, overlay)
  expect_equal(result$host, "prod-server")
  expect_equal(result$port, 5432L)
})

test_that("merge_configs merges nested lists recursively", {
  base    <- list(db = list(host = "localhost", port = 5432L))
  overlay <- list(db = list(host = "prod-server"))
  result  <- merge_configs(base, overlay)
  expect_equal(result$db$host, "prod-server")
  expect_equal(result$db$port, 5432L)
})

test_that("merge_configs handles three or more configs", {
  a <- list(x = 1L, y = 1L)
  b <- list(x = 2L)
  c <- list(y = 3L)
  result <- merge_configs(a, b, c)
  expect_equal(result$x, 2L)
  expect_equal(result$y, 3L)
})

test_that("config_val_as coerces string to integer", {
  cfg <- list(timeout = "30")
  expect_equal(config_val_as(cfg, "timeout", "integer"), 30L)
})

test_that("config_val_as coerces 'true' string to logical TRUE", {
  cfg <- list(debug = "true")
  expect_true(config_val_as(cfg, "debug", "logical"))
})

test_that("config_val_as coerces 'false' string to logical FALSE", {
  cfg <- list(debug = "false")
  expect_false(config_val_as(cfg, "debug", "logical"))
})

test_that("config_val_as coerces string to Date", {
  cfg <- list(start = "2024-01-15")
  result <- config_val_as(cfg, "start", "Date")
  expect_s3_class(result, "Date")
  expect_equal(format(result), "2024-01-15")
})

test_that("config_val_as returns NULL for absent key with no default", {
  cfg <- list(a = 1)
  expect_null(config_val_as(cfg, "missing", "integer"))
})

test_that("config_val_as errors on uncoercible value", {
  cfg <- list(timeout = "not-a-number")
  expect_error(config_val_as(cfg, "timeout", "integer"))
})

test_that("config_from_env builds a flat config list from env vars", {
  Sys.setenv(REACH__TIMEOUT = "30")
  on.exit(Sys.unsetenv("REACH__TIMEOUT"))
  cfg <- config_from_env("REACH")
  expect_equal(cfg$timeout, "30")
})

test_that("config_from_env builds nested list from double-underscore vars", {
  Sys.setenv(REACH__DB__HOST = "localhost", REACH__DB__PORT = "5432")
  on.exit(Sys.unsetenv(c("REACH__DB__HOST", "REACH__DB__PORT")))
  cfg <- config_from_env("REACH")
  expect_equal(cfg$db$host, "localhost")
  expect_equal(cfg$db$port, "5432")
})

test_that("config_from_env returns empty list and warns when no vars match", {
  expect_warning(
    result <- config_from_env("ZZZNOMATCH"),
    regexp = "ZZZNOMATCH"
  )
  expect_equal(result, list())
})

test_that("expand_config_paths expands relative slash paths", {
  cfg    <- list(input = "data/flow.csv", host = "localhost")
  result <- expand_config_paths(cfg, "/srv/pipeline")
  expect_true(startsWith(result$input, "/"))
  expect_true(grepl("data/flow.csv", result$input))
})

test_that("expand_config_paths leaves non-path values unchanged", {
  cfg    <- list(host = "localhost", port = 5432L)
  result <- expand_config_paths(cfg, "/srv/pipeline")
  expect_equal(result$host, "localhost")
  expect_equal(result$port, 5432L)
})

test_that("expand_config_paths expands dot-relative paths", {
  cfg    <- list(out = "./outputs/results.csv")
  result <- expand_config_paths(cfg, "/srv/pipeline")
  expect_equal(result$out, "/srv/pipeline/outputs/results.csv")
})

test_that("expand_config_paths leaves URLs unchanged", {
  cfg    <- list(api = "https://example.com/data")
  result <- expand_config_paths(cfg, "/srv/pipeline")
  expect_equal(result$api, "https://example.com/data")
})
