#!/usr/bin/env Rscript

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- script_args[grep("^--file=", script_args)]
if (length(script_path) == 0) {
  stop("Could not resolve the swfscfish workflow script path.", call. = FALSE)
}

script_path <- sub("^--file=", "", script_path[[1]])
workflow_path <- file.path(dirname(script_path), "swfscfish_workflow.R")

if (!file.exists(workflow_path)) {
  stop("The packaged swfscfish workflow wrapper could not be found.", call. = FALSE)
}

# Preserve the short script name as a compatibility entry point while routing
# execution through the canonical swfscfish workflow wrapper.
source(workflow_path, chdir = FALSE)
