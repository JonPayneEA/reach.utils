#' Set the default RStudio new-script template
#'
#' Creates or modifies the RStudio default R script template so that every new
#' script opens with a standard header. The template file is stored at the
#' location RStudio reads on startup:
#'
#' * Local RStudio: `~/AppData/Roaming/RStudio/templates/default.R`
#' * DASH platform: `~/.config/rstudio/templates/default.R`
#'
#' Four modes are supported:
#'
#' * `"default"` — writes the reach.io standard header template.
#' * `"custom"` — writes a caller-supplied character vector as the template.
#' * `"manual_edit"` — opens the existing template file for interactive
#'   editing (requires an active RStudio session).
#' * `"blank"` — deletes the template, reverting new scripts to blank files.
#'
#' @param format One of `"default"`, `"custom"`, `"manual_edit"`, or
#'   `"blank"`.
#' @param template A character vector of template lines. Required when
#'   `format = "custom"`, ignored otherwise.
#' @param dash Logical. If `TRUE`, uses the DASH platform template path;
#'   defaults to the local RStudio path.
#'
#' @return The path to the template file, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Apply the reach.io standard header on a local install
#' create_script_template()
#'
#' # Apply a custom template on DASH
#' create_script_template(
#'   format   = "custom",
#'   template = c("## Script: ", "## Author: ", "## Date:   "),
#'   dash     = TRUE
#' )
#'
#' # Remove the template entirely
#' create_script_template(format = "blank")
#' }
create_script_template <- function(
  format   = "default",
  template = NULL,
  dash     = FALSE
) {
  valid <- c("default", "custom", "manual_edit", "blank")
  if (!format %in% valid) {
    cli::cli_abort(
      c(
        "Invalid {.arg format}: {.val {format}}",
        "i" = "Must be one of {.or {.val {valid}}}"
      )
    )
  }

  templates_dir <- if (dash) {
    path.expand("~/.config/rstudio/templates")
  } else {
    path.expand("~/AppData/Roaming/RStudio/templates")
  }
  template_file <- file.path(templates_dir, "default.R")

  if (format == "blank") {
    if (!file.exists(template_file)) {
      cli::cli_warn("No template file found at {.path {template_file}}; nothing to remove.")
      return(invisible(template_file))
    }
    file.remove(template_file)
    cli::cli_inform("Template removed: {.path {template_file}}")
    return(invisible(template_file))
  }

  if (!dir.exists(templates_dir)) dir.create(templates_dir, recursive = TRUE)

  if (format == "default") {
    lines <- c(
      "## - - - - - - - - - - - - - -",
      "##",
      "## Organisation: Environment Agency",
      "##",
      "## Project:",
      "##",
      "## Script name:",
      "##",
      "## Purpose of script:",
      "##",
      "## Author:",
      "##",
      "## Email:",
      "##",
      "## Date Created:",
      "##",
      "## - - - - - - - - - - - - - -",
      "## Notes:",
      "##",
      "##",
      "## - - - - - - - - - - - - - -",
      "## Packages:",
      "",
      "library(reach.utils)",
      "",
      "## - - - - - - - - - - - - - -",
      "## Sourced files/functions:",
      "",
      "## - - - - - - - - - - - - - -",
      ""
    )
    writeLines(lines, template_file)
  } else if (format == "custom") {
    if (is.null(template)) {
      cli::cli_abort("{.arg template} must be supplied when {.arg format} is {.val custom}.")
    }
    writeLines(template, template_file)
  } else if (format == "manual_edit") {
    if (!file.exists(template_file)) file.create(template_file)
    utils::file.edit(template_file)
  }

  cli::cli_inform("Template saved: {.path {template_file}}")
  invisible(template_file)
}

#' Create a new R script pre-filled with the reach.io header
#'
#' Generates an R script containing the standard reach.io documentation header.
#' The file is written to the project working directory or a sub-folder. All
#' metadata fields default to placeholder text and can be set at call time.
#'
#' @param file_name Name of the new script, without the `.R` extension.
#'   Defaults to `"new_script"`.
#' @param file_path Optional sub-folder relative to the working directory. If
#'   `NULL` the script is written directly to `getwd()`.
#' @param author Author name to insert in the header.
#' @param email Author email to insert in the header.
#' @param date Date string for the "Date Created" field. Defaults to today in
#'   `DD/MM/YYYY` format.
#'
#' @return The path to the created script, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a script in the project root
#' create_script(file_name = "data_import")
#'
#' # Create a script in a sub-folder with metadata
#' create_script(
#'   file_name = "qc_checks",
#'   file_path = "R",
#'   author    = "Forecasting and Warning Team",
#'   email     = "forecasting@environment-agency.gov.uk"
#' )
#' }
create_script <- function(
  file_name = "new_script",
  file_path = NULL,
  author    = NULL,
  email     = NULL,
  date      = format(Sys.Date(), "%d/%m/%Y")
) {
  author_name <- if (is.null(author)) "Name" else author
  email_addr  <- if (is.null(email))  "email" else email

  header <- paste0(
    "## - - - - - - - - - - - - - -\n",
    "##\n",
    "## Organisation: Environment Agency\n",
    "##\n",
    "## Project:\n",
    "##\n",
    "## Script name:\n",
    "##\n",
    "## Purpose of script:\n",
    "##\n",
    "## Author: ", author_name, "\n",
    "##\n",
    "## Email: ", email_addr, "\n",
    "##\n",
    "## Date Created: ", date, "\n",
    "##\n",
    "## - - - - - - - - - - - - - -\n",
    "## Notes:\n",
    "##\n",
    "##\n",
    "## - - - - - - - - - - - - - -\n",
    "## Packages:\n",
    "\n",
    "library(reach.utils)\n",
    "\n",
    "## - - - - - - - - - - - - - -\n",
    "## Sourced files/functions:\n",
    "\n",
    "## - - - - - - - - - - - - - -\n",
    "\n"
  )

  dest_dir <- if (is.null(file_path)) getwd() else file.path(getwd(), file_path)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  out <- file.path(dest_dir, paste0(file_name, ".R"))

  writeLines(header, out)
  cli::cli_inform("Script created: {.path {out}}")
  invisible(out)
}

#' Create a README from a Quarto template
#'
#' Writes a `.qmd` file containing a standard reach.io README skeleton and,
#' if the `quarto` package is installed, renders it to the requested output
#' format. The template includes sections for introduction, project structure,
#' and run instructions.
#'
#' Supported output formats:
#'
#' * `"markdown"` — plain Markdown (`.md`).
#' * `"github"` — GitHub Flavoured Markdown (GFM).
#' * `"html"` — self-contained HTML.
#'
#' @param format Output format. One of `"markdown"` (default), `"github"`, or
#'   `"html"`.
#' @param file_path Directory in which to save the README. Defaults to the
#'   current working directory.
#' @param author Author name inserted into the YAML front matter.
#' @param readme_title Title inserted into the YAML front matter.
#'
#' @return The path to the created `.qmd` file, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a Markdown README in the working directory
#' create_readme(
#'   format       = "markdown",
#'   author       = "Forecasting and Warning Team",
#'   readme_title = "My Reach Project"
#' )
#'
#' # Create an HTML README in a specific directory
#' create_readme(
#'   format    = "html",
#'   file_path = "~/projects/my-reach-project",
#'   author    = "Forecasting and Warning Team"
#' )
#' }
create_readme <- function(
  format       = "markdown",
  file_path    = NULL,
  author       = NULL,
  readme_title = NULL
) {
  valid_formats <- c("markdown", "github", "html")
  if (!format %in% valid_formats) {
    cli::cli_abort(
      c(
        "Invalid {.arg format}: {.val {format}}",
        "i" = "Must be one of {.or {.val {valid_formats}}}"
      )
    )
  }

  out_format <- switch(
    format,
    github   = "  gfm: default\n",
    html     = "  html:\n    self-contained: true\n",
    markdown = "  markdown: default\n"
  )

  author_str <- if (is.null(author)) "add author" else author
  title_str  <- if (is.null(readme_title)) "README (edit title)" else readme_title

  readme_txt <- paste0(
    "---\n",
    "title: \"", title_str, "\"\n",
    "author: \"", author_str, "\"\n",
    "date: today\n",
    "date-format: \"DD/MM/YYYY\"\n",
    "format:\n",
    out_format,
    "toc: true\n",
    "editor_options:\n",
    "  chunk_output_type: console\n",
    "---\n",
    "\n",
    "## Introduction\n",
    "\n",
    "Introduce your project here. Suggested points to cover:\n",
    "\n",
    "- Motivation and purpose of the project\n",
    "- Key inputs and data sources\n",
    "- Where the project is used within the reach ecosystem\n",
    "- Key outputs and where they are consumed\n",
    "\n",
    "## Project structure\n",
    "\n",
    "Describe the directory layout here. A table showing each folder and its\n",
    "role is often useful.\n",
    "\n",
    "## How to run\n",
    "\n",
    "Explain how to run the project: script execution order, any required\n",
    "environment variables or configuration files, and expected outputs.\n",
    "\n"
  )

  dest_dir <- if (is.null(file_path)) getwd() else path.expand(file_path)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  qmd_path <- file.path(dest_dir, "README.qmd")

  writeLines(readme_txt, qmd_path)
  cli::cli_inform("README template written: {.path {qmd_path}}")

  if (requireNamespace("quarto", quietly = TRUE)) {
    quarto::quarto_render(qmd_path, quiet = TRUE)
    cli::cli_inform("README rendered ({.val {format}}).")
  } else {
    cli::cli_warn(
      c(
        "Package {.pkg quarto} is not installed; skipping render.",
        "i" = "Run {.code quarto::quarto_render({.path {qmd_path}})} to render manually."
      )
    )
  }

  invisible(qmd_path)
}
