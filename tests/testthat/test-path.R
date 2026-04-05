test_that("ensure_dir creates a directory that does not exist", {
  tmp <- file.path(tempdir(), paste0("reach_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE))
  ensure_dir(tmp)
  expect_true(dir.exists(tmp))
})

test_that("ensure_dir creates nested directories recursively", {
  tmp <- file.path(tempdir(), paste0("reach_test_", Sys.getpid()), "a", "b", "c")
  on.exit(unlink(strsplit(tmp, "/a/")[[1]][1], recursive = TRUE))
  ensure_dir(tmp)
  expect_true(dir.exists(tmp))
})

test_that("ensure_dir returns path invisibly", {
  tmp <- file.path(tempdir(), paste0("reach_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE))
  expect_invisible(ensure_dir(tmp))
  expect_equal(ensure_dir(tmp), tmp)
})

test_that("check_file_exists returns path invisibly for existing file", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  file.create(tmp)
  expect_invisible(check_file_exists(tmp))
})

test_that("check_file_exists errors for missing file", {
  expect_error(check_file_exists("/nonexistent/path/file.csv"))
})

test_that("resolve_path combines and normalises components", {
  result <- resolve_path(tempdir(), "subdir", "file.txt")
  expect_true(is.character(result))
  expect_true(grepl("subdir", result))
  expect_true(grepl("file.txt", result))
})

test_that("resolve_path expands tilde", {
  result <- resolve_path("~", "reach")
  expect_false(startsWith(result, "~"))
})

test_that("find_latest_file returns the most recently modified file", {
  tmp <- file.path(tempdir(), paste0("reach_ftest_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp)
  writeLines("a", file.path(tmp, "old.csv"))
  Sys.sleep(0.05)
  writeLines("b", file.path(tmp, "new.csv"))
  result <- find_latest_file(tmp, "*.csv")
  expect_true(grepl("new\\.csv$", result))
})

test_that("find_latest_file errors when directory does not exist", {
  expect_error(find_latest_file("/nonexistent/dir", "*.csv"))
})

test_that("find_latest_file errors when no files match pattern", {
  tmp <- file.path(tempdir(), paste0("reach_ftest_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp)
  expect_error(find_latest_file(tmp, "*.csv"))
})

test_that("swap_ext replaces the file extension", {
  expect_equal(swap_ext("data/flow.csv", "parquet"), "data/flow.parquet")
})

test_that("swap_ext accepts extension with a leading dot", {
  expect_equal(swap_ext("data/flow.csv", ".parquet"), "data/flow.parquet")
})

test_that("swap_ext replaces only the last extension", {
  expect_equal(swap_ext("outputs/run.tar.gz", "zip"), "outputs/run.tar.zip")
})
