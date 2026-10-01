# create_script_template -----------------------------------------------------
#
# These tests redirect HOME/R_USER/USERPROFILE to a disposable temp directory
# so the function's hard-coded template paths never touch the real home
# directory. R_USER must be overridden alongside HOME: on Windows,
# path.expand("~") consults R_USER first, and CI runners already set it, so
# overriding HOME alone silently does nothing there.

.with_temp_home <- function(code) {
  old_home   <- Sys.getenv("HOME", unset = NA)
  old_r_user <- Sys.getenv("R_USER", unset = NA)
  old_up     <- Sys.getenv("USERPROFILE", unset = NA)
  tmp_home   <- tempfile("home")
  dir.create(tmp_home)
  Sys.setenv(HOME = tmp_home, R_USER = tmp_home, USERPROFILE = tmp_home)
  on.exit({
    if (is.na(old_home))   Sys.unsetenv("HOME") else Sys.setenv(HOME = old_home)
    if (is.na(old_r_user)) Sys.unsetenv("R_USER") else Sys.setenv(R_USER = old_r_user)
    if (is.na(old_up))     Sys.unsetenv("USERPROFILE") else Sys.setenv(USERPROFILE = old_up)
    unlink(tmp_home, recursive = TRUE)
  }, add = TRUE)
  force(code)
}

test_that("create_script_template writes the default template", {
  .with_temp_home({
    out <- create_script_template()
    expect_true(file.exists(out))
    lines <- readLines(out)
    expect_true(any(grepl("Environment Agency", lines)))
    expect_true(any(grepl("library\\(reach.utils\\)", lines)))
  })
})

test_that("create_script_template writes a custom template", {
  .with_temp_home({
    out <- create_script_template(
      format   = "custom",
      template = c("## Script: ", "## Author: ")
    )
    expect_true(file.exists(out))
    expect_equal(readLines(out), c("## Script: ", "## Author: "))
  })
})

test_that("create_script_template errors when custom has no template supplied", {
  .with_temp_home({
    expect_error(create_script_template(format = "custom"), regexp = "template")
  })
})

test_that("create_script_template warns when removing a template that does not exist", {
  .with_temp_home({
    expect_warning(create_script_template(format = "blank"), regexp = "No template file found")
  })
})

test_that("create_script_template removes an existing template", {
  .with_temp_home({
    out <- create_script_template()
    expect_true(file.exists(out))
    create_script_template(format = "blank")
    expect_false(file.exists(out))
  })
})

test_that("create_script_template errors on an invalid format", {
  expect_error(create_script_template(format = "nonsense"), regexp = "Invalid")
})

# create_script ---------------------------------------------------------------

test_that("create_script writes a header-prefixed R script", {
  old_wd <- getwd()
  tmp    <- tempfile("scriptdir")
  dir.create(tmp)
  setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  out <- create_script(file_name = "my_script", author = "Jane", email = "jane@example.com")
  expect_true(file.exists(out))
  lines <- readLines(out)
  expect_true(any(grepl("Jane", lines)))
  expect_true(any(grepl("jane@example.com", lines)))
})

test_that("create_script falls back to placeholder author/email", {
  old_wd <- getwd()
  tmp    <- tempfile("scriptdir2")
  dir.create(tmp)
  setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  out   <- create_script(file_name = "another_script")
  lines <- readLines(out)
  expect_true(any(grepl("Author: Name", lines)))
  expect_true(any(grepl("Email: email", lines)))
})

# create_function -------------------------------------------------------------

test_that("create_function writes a roxygen-skeleton stub with one @param per parameter", {
  old_wd <- getwd()
  tmp    <- tempfile("funcdir")
  dir.create(tmp)
  setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  out   <- create_function("aggregate_flow", parameters = c("x", "values", "by"))
  expect_true(file.exists(out))
  lines <- readLines(out)
  expect_equal(sum(grepl("^#' @param", lines)), 3L)
  expect_true(any(grepl("aggregate_flow <- function", lines)))
})

# create_project ----------------------------------------------------------

test_that("create_project scaffolds the standard directory layout", {
  root <- tempfile("project")
  create_project(root, config = TRUE, readme = FALSE)

  expect_true(dir.exists(file.path(root, "R")))
  expect_true(dir.exists(file.path(root, "data", "raw")))
  expect_true(dir.exists(file.path(root, "data", "processed")))
  expect_true(dir.exists(file.path(root, "config")))
  expect_true(dir.exists(file.path(root, "outputs")))
  expect_true(dir.exists(file.path(root, "logs")))
  expect_true(dir.exists(file.path(root, "tests", "testthat")))
  expect_true(file.exists(file.path(root, ".gitignore")))
  expect_true(file.exists(file.path(root, "config", "pipeline.yml")))
})

test_that("create_project skips writing .gitignore when one already exists", {
  root <- tempfile("project2")
  dir.create(root, recursive = TRUE)
  writeLines("custom", file.path(root, ".gitignore"))

  create_project(root, config = FALSE, gitignore = TRUE)

  expect_equal(readLines(file.path(root, ".gitignore")), "custom")
})

# create_config -----------------------------------------------------------

test_that("create_config writes a pipeline.yml with the expected sections", {
  dest <- tempfile("config")
  out  <- create_config(file_path = dest, site_id = "42001")

  expect_true(file.exists(out))
  txt <- readLines(out)
  expect_true(any(grepl('id:.*"42001"', txt)))
  expect_true(any(grepl("^site:", txt)))
  expect_true(any(grepl("^paths:", txt)))
  expect_true(any(grepl("^qc:", txt)))
  expect_true(any(grepl("^logging:", txt)))
})

test_that("create_config refuses to overwrite an existing file by default", {
  dest <- tempfile("config2")
  create_config(file_path = dest)
  expect_error(create_config(file_path = dest), regexp = "already exists")
})

test_that("create_config overwrites when overwrite = TRUE", {
  dest <- tempfile("config3")
  create_config(file_path = dest, site_id = "1")
  expect_no_error(create_config(file_path = dest, site_id = "2", overwrite = TRUE))
  txt <- readLines(file.path(dest, "pipeline.yml"))
  expect_true(any(grepl('"2"', txt)))
})

# create_readme -----------------------------------------------------------
#
# requireNamespace is mocked to FALSE so these tests are deterministic
# regardless of whether the quarto package happens to be installed in CI.

test_that("create_readme writes a qmd file and warns when quarto is unavailable", {
  local_mocked_bindings(.quarto_available = function() FALSE)
  dest <- tempfile("readme")

  expect_warning(
    out <- create_readme(
      format       = "markdown",
      file_path    = dest,
      author       = "Forecasting Team",
      readme_title = "Test Project"
    ),
    regexp = "quarto"
  )
  expect_true(file.exists(out))
  txt <- readLines(out)
  expect_true(any(grepl("Test Project", txt)))
  expect_true(any(grepl("Forecasting Team", txt)))
})

test_that("create_readme errors on an invalid format", {
  expect_error(create_readme(format = "pdf"), regexp = "Invalid")
})

# create_report -------------------------------------------------------------

test_that("create_report writes a qmd report and warns when quarto is unavailable", {
  local_mocked_bindings(.quarto_available = function() FALSE)
  dest <- tempfile("report")

  expect_warning(
    out <- create_report(
      format       = "html",
      file_path    = dest,
      report_title = "Flood Report",
      author       = "Forecasting Team"
    ),
    regexp = "quarto"
  )
  expect_true(file.exists(out))
  txt <- readLines(out)
  expect_true(any(grepl("Flood Report", txt)))
})

test_that("create_report errors on an invalid format", {
  expect_error(create_report(format = "docx"), regexp = "Invalid")
})
