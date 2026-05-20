#' Parse command-line workflow arguments
#'
#' Parses the command-line interface used by the packaged workflow wrapper.
#'
#' @param arguments Character vector, usually `commandArgs(trailingOnly = TRUE)`.
#'
#' @return A list describing the requested command-line action.
#'
#' @export
parse_command_line <- function(arguments = commandArgs(trailingOnly = TRUE)) {
  # Accept either a config-driven invocation or the older positional-argument
  # form so the packaged workflow wrapper can remain backward compatible.
  if (length(arguments) == 0) {
    return(list(
      action = "run",
      config_path = NULL,
      input_file = NULL,
      out_root = NULL,
      cache_dir = NULL,
      strict_length_pdf = NULL,
      run_multiplier_model = NULL
    ))
  }

  if (identical(arguments[[1]], "--write-template")) {
    if (length(arguments) < 2 || !nzchar(arguments[[2]])) {
      stop("Use '--write-template <path>' to scaffold a workflow YAML file.", call. = FALSE)
    }

    return(list(action = "write_template", path = arguments[[2]]))
  }

  if (identical(arguments[[1]], "--config")) {
    if (length(arguments) < 2 || !nzchar(arguments[[2]])) {
      stop("Use '--config <path>' to supply a workflow YAML file.", call. = FALSE)
    }

    return(list(action = "run", config_path = arguments[[2]]))
  }

  # Parse the positional fallback exactly once so older command lines still map
  # onto the current workflow config structure.
  list(
    action = "run",
    config_path = NULL,
    input_file = arguments[[1]] %||% NULL,
    out_root = arguments[[2]] %||% NULL,
    cache_dir = arguments[[3]] %||% NULL,
    strict_length_pdf = arguments[[4]] %||% NULL,
    run_multiplier_model = arguments[[5]] %||% NULL
  )
}

#' Resolve one command-line workflow config
#'
#' Converts parsed command-line inputs to a validated normalized workflow config
#' or writes a new workflow template when requested.
#'
#' @param arguments Character vector, usually `commandArgs(trailingOnly = TRUE)`.
#' @param base_dir Base directory for relative paths.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path.
#'
#' @return A normalized workflow config, or the written template path.
#'
#' @export
resolve_command_line <- function(arguments = commandArgs(trailingOnly = TRUE),
                                 base_dir = getwd(),
                                 registry_path = NULL,
                                 policy_path = NULL) {
  # Dispatch from the parsed command-line action so template generation and
  # workflow execution share one argument parser.
  cli_values <- parse_command_line(arguments = arguments)

  if (identical(cli_values$action, "write_template")) {
    return(write_workflow_yaml(cli_values$path))
  }

  if (!is.null(cli_values$config_path)) {
    workflow_config <- read_workflow_config(cli_values$config_path)
    return(
      normalize_workflow(
        config = workflow_config,
        base_dir = dirname(path_absolute(cli_values$config_path, base_dir = base_dir)),
        registry_path = registry_path,
        policy_path = policy_path
      )
    )
  }

  # Build the fallback positional config from the packaged defaults, then
  # normalize it through the same validator and path resolver.
  workflow_config <- default_workflow_config(
    input_file = cli_values$input_file %||% "fishery_survey_tsl.xlsx",
    out_root = cli_values$out_root %||% "outputs_swfscfish",
    cache_dir = cli_values$cache_dir %||% "cache"
  )
  workflow_config$workflow$strict_length_pdf <- if (is.null(cli_values$strict_length_pdf)) {
    workflow_config$workflow$strict_length_pdf
  } else {
    command_line_true(cli_values$strict_length_pdf)
  }
  workflow_config$workflow$run_multiplier_model <- if (is.null(cli_values$run_multiplier_model)) {
    workflow_config$workflow$run_multiplier_model
  } else {
    command_line_true(cli_values$run_multiplier_model)
  }

  normalize_workflow(
    config = workflow_config,
    base_dir = base_dir,
    registry_path = registry_path,
    policy_path = policy_path
  )
}

#' Write the workflow YAML template
#'
#' Copies the packaged SWFSC fish workflow template to a caller-specified path.
#'
#' @param path Output YAML path.
#' @param overwrite Logical scalar. If `TRUE`, overwrite an existing file.
#'
#' @return The written path, invisibly.
#'
#' @export
write_workflow_yaml <- function(path,
                                overwrite = FALSE) {
  # Copy the packaged template directly so users can start from a validated
  # file rather than reconstructing the workflow schema by hand.
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single output YAML path.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("Workflow YAML already exists. Set 'overwrite = TRUE' to replace it.", call. = FALSE)
  }

  ensure_parent_path(path)
  file.copy(
    from = installed_template_path("swfscfish_config.yaml"),
    to = path,
    overwrite = overwrite
  )

  invisible(path)
}

#' Build one workflow script call
#'
#' Builds the `Rscript` command and arguments needed to run the packaged
#' workflow wrapper with a validated YAML config.
#'
#' @param config_path Workflow YAML path.
#' @param script_name Packaged workflow-wrapper script name.
#' @param rscript_path Optional `Rscript` executable path.
#'
#' @return A list with `command` and `args`.
#'
#' @export
workflow_script_call <- function(config_path,
                                 script_name = "swfscfish.R",
                                 rscript_path = NULL) {
  # Validate the workflow YAML before constructing the external call so the
  # shell runner fails early on malformed config files.
  if (is.null(rscript_path)) {
    rscript_path <- file.path(R.home("bin"), "Rscript")
  }

  workflow_config <- read_workflow_config(config_path)
  normalize_workflow(
    config = workflow_config,
    base_dir = dirname(path_absolute(config_path)),
    registry_path = NULL,
    policy_path = NULL
  )

  list(
    command = rscript_path,
    args = c(
      shQuote(installed_script_path(script_name)),
      "--config",
      shQuote(path_absolute(config_path))
    )
  )
}

#' Run the packaged workflow script
#'
#' Launches the packaged workflow wrapper with a validated workflow YAML file.
#'
#' @param config_path Workflow YAML path.
#' @param script_name Packaged workflow-wrapper script name.
#' @param rscript_path Optional `Rscript` executable path.
#' @param wait Logical scalar. If `TRUE`, wait for the script to finish.
#'
#' @return The `system2()` exit status.
#'
#' @export
run_workflow_script <- function(config_path,
                                script_name = "swfscfish.R",
                                rscript_path = NULL,
                                wait = TRUE) {
  # Build the validated script call first, then hand it to `system2()` without
  # duplicating the path and config checks here.
  if (!is.logical(wait) || length(wait) != 1 || is.na(wait)) {
    stop("'wait' must be TRUE or FALSE.", call. = FALSE)
  }

  workflow_call <- workflow_script_call(
    config_path = config_path,
    script_name = script_name,
    rscript_path = rscript_path
  )

  system2(
    command = workflow_call$command,
    args = workflow_call$args,
    wait = wait
  )
}

#' Emit a timestamped workflow message
#'
#' @param ... Message components.
#' @param timestamp Boolean that dictates whether to prepend with timestamp.
#' @param appendLF Passed to `base::message()`.
#' @return Invisibly returns `NULL`.
#' @keywords internal
tsb_message <- function(..., timestamp = TRUE, appendLF = TRUE) {
  if (isTRUE(timestamp)) {
    base::message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = ""), appendLF = appendLF)
  } else {
    base::message(paste0(..., collapse = ""), appendLF = appendLF)
  }  
}