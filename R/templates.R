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

#' Scaffold a standard reach.io project directory structure
#'
#' Creates the conventional directory layout for a reach.io pipeline project
#' and optionally generates a starter config file, README, and `.gitignore`.
#' Intended to be called once inside a freshly created, empty project folder.
#'
#' The directories created are: `R/`, `data/raw/`, `data/processed/`,
#' `config/`, `outputs/`, `logs/`, and `tests/testthat/`.
#'
#' @param path Root directory of the project. Defaults to the current working
#'   directory.
#' @param config Logical. If `TRUE` (default), writes a starter
#'   `config/pipeline.yml` via [create_config()].
#' @param readme Logical. If `TRUE`, writes a `README.qmd` via
#'   [create_readme()]. Default `FALSE`.
#' @param gitignore Logical. If `TRUE` (default), writes a `.gitignore`
#'   suited to R projects. Skipped if a `.gitignore` already exists.
#' @param author Author name forwarded to [create_readme()] when
#'   `readme = TRUE`.
#'
#' @return `path` invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Scaffold in the current working directory
#' create_project()
#'
#' # Scaffold a new sub-directory
#' create_project("~/projects/flood-analysis", author = "Forecasting and Warning Team")
#' }
create_project <- function(
  path      = getwd(),
  config    = TRUE,
  readme    = FALSE,
  gitignore = TRUE,
  author    = NULL
) {
  path <- path.expand(path)
  dirs <- c(
    "R",
    "data/raw",
    "data/processed",
    "config",
    "outputs",
    "logs",
    "tests/testthat"
  )
  for (d in dirs) ensure_dir(file.path(path, d))

  if (gitignore) {
    gi_path <- file.path(path, ".gitignore")
    if (!file.exists(gi_path)) {
      writeLines(
        c(
          ".Rhistory",
          ".RData",
          ".Rproj.user/",
          "renv/library/",
          "renv/staging/",
          "",
          "# runtime outputs and logs (regenerable)",
          "outputs/",
          "logs/",
          "",
          "# environment-specific config (keep out of version control)",
          "config/local.yml",
          "config/prod.yml",
          ".env"
        ),
        gi_path
      )
      cli::cli_inform("Created: {.path {gi_path}}")
    }
  }

  if (config) create_config(file_path = file.path(path, "config"))
  if (readme) create_readme(format = "github", file_path = path, author = author)

  cli::cli_inform("Project scaffolded at {.path {path}}")
  invisible(path)
}

#' Create a starter YAML pipeline configuration file
#'
#' Writes a `pipeline.yml` template pre-populated with the sections expected
#' by the reach.utils config module: site metadata, standard paths, QC
#' thresholds, and logging settings. Designed to be read immediately with
#' [load_config()] and validated with [validate_config()].
#'
#' @param file_path Directory in which to write the config file. Defaults to
#'   `config/` inside the current working directory.
#' @param file_name Name of the config file without extension. Default
#'   `"pipeline"`.
#' @param site_id Optional site identifier inserted into the `site.id` field.
#' @param overwrite Logical. If `FALSE` (default), aborts rather than
#'   overwriting an existing file.
#'
#' @return The path to the created `.yml` file, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' create_config()
#'
#' create_config(file_name = "site_42", site_id = "42001")
#' }
create_config <- function(
  file_path = NULL,
  file_name = "pipeline",
  site_id   = NULL,
  overwrite = FALSE
) {
  dest_dir <- if (is.null(file_path)) file.path(getwd(), "config") else file_path
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  out <- file.path(dest_dir, paste0(file_name, ".yml"))
  if (file.exists(out) && !overwrite) {
    cli::cli_abort(
      c(
        "Config file already exists: {.path {out}}",
        "i" = "Set {.arg overwrite = TRUE} to replace it."
      )
    )
  }

  id_val <- if (is.null(site_id)) '""' else paste0('"', site_id, '"')

  cfg_txt <- paste0(
    "# Pipeline configuration\n",
    "# Edit values to match your site and environment.\n",
    "\n",
    "site:\n",
    "  id:        ", id_val, "\n",
    "  name:      \"\"\n",
    "  catchment: \"\"\n",
    "\n",
    "paths:\n",
    "  data_raw:        \"data/raw\"\n",
    "  data_processed:  \"data/processed\"\n",
    "  outputs:         \"outputs\"\n",
    "  logs:            \"logs\"\n",
    "\n",
    "qc:\n",
    "  bounds_min:         0\n",
    "  bounds_max:         10000\n",
    "  max_rate_of_change: 500\n",
    "  flatline_n:         6\n",
    "  max_na_run:         4\n",
    "\n",
    "logging:\n",
    "  level:    \"info\"\n",
    "  to_file:  false\n",
    "  log_file: \"logs/pipeline.log\"\n"
  )

  writeLines(cfg_txt, out)
  cli::cli_inform("Config template written: {.path {out}}")
  invisible(out)
}

#' Create an analytical report from a Quarto template
#'
#' Writes a `.qmd` file structured as a formal analytical report — distinct
#' from [create_readme()] in that it targets a shareable output document
#' rather than project documentation. The template includes sections for
#' executive summary, data and methods, results, discussion, and a code
#' appendix. A setup chunk pre-loads `reach.utils` and `load_config()`.
#'
#' Supported output formats:
#'
#' * `"html"` (default) — self-contained HTML with left-hand TOC.
#' * `"pdf"` — PDF via LaTeX.
#' * `"github"` — GitHub Flavoured Markdown.
#'
#' @param format Output format. One of `"html"` (default), `"pdf"`, or
#'   `"github"`.
#' @param file_path Directory in which to save the report. Defaults to the
#'   current working directory.
#' @param file_name Name of the `.qmd` file without extension. Default
#'   `"report"`.
#' @param report_title Title inserted into the YAML front matter.
#' @param author Author name inserted into the YAML front matter.
#'
#' @return The path to the created `.qmd` file, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' create_report(
#'   format       = "html",
#'   file_name    = "flood_analysis_2024",
#'   report_title = "Flood Frequency Analysis — River Avon at Evesham",
#'   author       = "Forecasting and Warning Team"
#' )
#' }
create_report <- function(
  format       = "html",
  file_path    = NULL,
  file_name    = "report",
  report_title = NULL,
  author       = NULL
) {
  valid_formats <- c("html", "pdf", "github")
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
    html   = "  html:\n    self-contained: true\n    toc-location: left\n",
    pdf    = "  pdf:\n    toc: true\n",
    github = "  gfm: default\n"
  )

  author_str <- if (is.null(author)) "add author" else author
  title_str  <- if (is.null(report_title)) "Report Title" else report_title

  report_txt <- paste0(
    "---\n",
    "title: \"", title_str, "\"\n",
    "author: \"", author_str, "\"\n",
    "date: today\n",
    "date-format: \"DD/MM/YYYY\"\n",
    "format:\n",
    out_format,
    "number-sections: true\n",
    "execute:\n",
    "  echo: false\n",
    "  warning: false\n",
    "  message: false\n",
    "editor_options:\n",
    "  chunk_output_type: console\n",
    "---\n",
    "\n",
    "```{r}\n",
    "#| label: setup\n",
    "library(reach.utils)\n",
    "cfg <- load_config(\"config/pipeline.yml\")\n",
    "```\n",
    "\n",
    "## Executive summary\n",
    "\n",
    "Briefly state the purpose of the analysis, key inputs, and main findings.\n",
    "\n",
    "## Data and methods\n",
    "\n",
    "### Data sources\n",
    "\n",
    "Describe the input datasets: site(s), period of record, temporal resolution,\n",
    "and any pre-processing applied before this analysis.\n",
    "\n",
    "### Methods\n",
    "\n",
    "Describe the analytical approach.\n",
    "\n",
    "## Results\n",
    "\n",
    "Present findings. Use sub-sections for separate themes or sites.\n",
    "\n",
    "## Discussion\n",
    "\n",
    "Interpret the results and note any caveats or limitations.\n",
    "\n",
    "## Appendix {.appendix}\n",
    "\n",
    "```{r}\n",
    "#| label: appendix\n",
    "#| echo: true\n",
    "```\n",
    "\n"
  )

  dest_dir <- if (is.null(file_path)) getwd() else path.expand(file_path)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  qmd_path <- file.path(dest_dir, paste0(file_name, ".qmd"))

  writeLines(report_txt, qmd_path)
  cli::cli_inform("Report template written: {.path {qmd_path}}")

  if (requireNamespace("quarto", quietly = TRUE)) {
    quarto::quarto_render(qmd_path, quiet = TRUE)
    cli::cli_inform("Report rendered ({.val {format}}).")
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

#' Create a new R function stub with a Roxygen2 skeleton
#'
#' Writes a `.R` file containing a bare function definition pre-filled with
#' the reach.utils Roxygen2 documentation skeleton: title, description,
#' `@param` lines (one per parameter), `@return`, `@export`, and `@examples`.
#'
#' @param function_name Name of the function to create (also used as the file
#'   name by default).
#' @param file_name Name of the `.R` file without extension. Defaults to
#'   `function_name`.
#' @param file_path Sub-folder relative to the working directory in which to
#'   save the file. Default `"R"`.
#' @param parameters Character vector of parameter names. Default `"x"`.
#'
#' @return The path to the created `.R` file, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' create_function("compute_runoff")
#'
#' create_function(
#'   "aggregate_flow",
#'   parameters = c("x", "values", "by")
#' )
#' }
create_function <- function(
  function_name,
  file_name  = function_name,
  file_path  = "R",
  parameters = "x"
) {
  param_lines <- paste(
    paste0("#' @param ", parameters, " Description."),
    collapse = "\n"
  )
  sig <- paste(parameters, collapse = ",\n  ")

  stub <- paste0(
    "#' Title\n",
    "#'\n",
    "#' Description.\n",
    "#'\n",
    param_lines, "\n",
    "#'\n",
    "#' @return Description.\n",
    "#' @export\n",
    "#'\n",
    "#' @examples\n",
    "#' \\dontrun{\n",
    "#' ", function_name, "(", parameters[[1L]], ")\n",
    "#' }\n",
    function_name, " <- function(\n",
    "  ", sig, "\n",
    ") {\n",
    "\n",
    "}\n"
  )

  dest_dir <- if (is.null(file_path)) getwd() else file.path(getwd(), file_path)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  out <- file.path(dest_dir, paste0(file_name, ".R"))

  writeLines(stub, out)
  cli::cli_inform("Function stub created: {.path {out}}")
  invisible(out)
}
