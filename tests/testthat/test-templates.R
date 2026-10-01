# create_script_template -----------------------------------------------------
#
# create_script_template()'s template paths are hard-coded under "~", and
# path.expand("~") turned out not to be redirectable from within a running
# test: R resolves the Windows home directory once via the OS "Documents"
# special folder, ignoring HOME/R_USER/USERPROFILE set with Sys.setenv()
# after the process has started (confirmed by the CI log: every test wrote
# to the real "C:/Users/runneradmin/Documents/..." path regardless of the
# override). So instead of trying to redirect the path, these tests operate
# on the function's real resolved path directly, backing up and restoring
# any file that already exists there so a developer's own RStudio template
# is never lost by running the test suite locally.

.default_template_path <- function() {
  path.expand("~/AppData/Roaming/RStudio/templates/default.R")
}

.with_clean_template_file <- function(code) {
  path   <- .default_template_path()
  backup <- NULL
  if (file.exists(path)) {
    backup <- tempfile()
    file.copy(path, backup)
    file.remove(path)
  }
  on.exit({
    if (file.exists(path)) file.remove(path)
    if (!is.null(backup)) {
      if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
      file.copy(backup, path, overwrite = TRUE)
      file.remove(backup)
    }
  }, add = TRUE)
  force(code)
}

test_that("create_script_template writes the default template", {
  .with_clean_template_file({
    out <- create_script_template()
    expect_true(file.exists(out))
    lines <- readLines(out)
    expect_true(any(grepl("Environment Agency", lines)))
    expect_true(any(grepl("library\\(reach.utils\\)", lines)))
  })
})

test_that("create_script_template writes a custom template", {
  .with_clean_template_file({
    out <- create_script_template(
      format   = "custom",
      template = c("## Script: ", "## Author: ")
    )
    expect_true(file.exists(out))
    expect_equal(readLines(out), c("## Script: ", "## Author: "))
  })
})

test_that("create_script_template errors when custom has no template supplied", {
  .with_clean_template_file({
    expect_error(create_script_template(format = "custom"), regexp = "template")
  })
})

test_that("create_script_template warns when removing a template that does not exist", {
  .with_clean_template_file({
    expect_warning(create_script_template(format = "blank"), regexp = "No template file found")
  })
})

test_that("create_script_template removes an existing template", {
  .with_clean_template_file({
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
