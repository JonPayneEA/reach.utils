#!/usr/bin/env Rscript
# ============================================================ #
# Tool:         run_pipeline CLI
# Description:  Command-line wrapper around reach.utils::run_pipeline(),
#               for use from cron entries and external schedulers
# Flode Module: reach.utils
# Author:       Forecasting and Warning Team, forecasting@environment-agency.gov.uk
# Created:      2026-09-30
# Modified:     2026-09-30 - JP: initial version
# Tier:         3
# Inputs:       A single command-line argument: path to a pipeline YAML config
# Outputs:      Whatever the dispatched activities themselves produce
# Dependencies: reach.utils
# ============================================================ #

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L) {
  stop("Usage: Rscript run_pipeline.R <path/to/pipeline.yml>", call. = FALSE)
}

reach.utils::run_pipeline(args[[1]])
