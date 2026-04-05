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
