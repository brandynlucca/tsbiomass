#' Parse command-line arguments
#'
#' Parses the command-line interface used by the packaged script wrapper.
#'
#' @param arguments Character vector, usually `commandArgs(trailingOnly = TRUE)`.
#'
#' @return A list describing the requested command-line action.
#'
#' @export
parse_command_line <- function(arguments = commandArgs(trailingOnly = TRUE)) {
  # Accept either a config-driven invocation or the older positional-argument
  # form so the packaged script wrapper can remain backward compatible.
  if (length(arguments) == 0) {
    return(list(
      action = "run",
      config_path = NULL,
      input_file = NULL,
      output_root = NULL,
      cache_folder = NULL,
      strict_length_pdf = NULL,
      run_multiplier_model = NULL
    ))
  }

  if (identical(arguments[[1]], "--write-template")) {
    if (length(arguments) < 2 || !nzchar(arguments[[2]])) {
      stop("Use '--write-template <path>' to scaffold a config YAML file.", call. = FALSE)
    }

    return(list(action = "write_template", path = arguments[[2]]))
  }

  if (identical(arguments[[1]], "--config")) {
    if (length(arguments) < 2 || !nzchar(arguments[[2]])) {
      stop("Use '--config <path>' to supply a config YAML file.", call. = FALSE)
    }

    return(list(action = "run", config_path = arguments[[2]]))
  }

  # Parse the positional fallback exactly once so older command lines still map
  # onto the current config structure.
  list(
    action = "run",
    config_path = NULL,
    input_file = arguments[[1]] %||% NULL,
    output_root = arguments[[2]] %||% NULL,
    cache_folder = arguments[[3]] %||% NULL,
    strict_length_pdf = arguments[[4]] %||% NULL,
    run_multiplier_model = arguments[[5]] %||% NULL
  )
}

#' Resolve one command-line config
#'
#' Converts parsed command-line inputs to a validated normalized config
#' or writes a new template when requested.
#'
#' @param arguments Character vector, usually `commandArgs(trailingOnly = TRUE)`.
#' @param base_dir Base directory for relative paths.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path.
#'
#' @return A normalized config, or the written template path.
#'
#' @export
resolve_command_line <- function(arguments = commandArgs(trailingOnly = TRUE),
                                 base_dir = getwd(),
                                 registry_path = NULL,
                                 policy_path = NULL) {
  # Dispatch from the parsed command-line action so template generation and
  # script execution share one argument parser.
  cli_values <- parse_command_line(arguments = arguments)

  if (identical(cli_values$action, "write_template")) {
    return(write_config_yaml(cli_values$path))
  }

  if (!is.null(cli_values$config_path)) {
    return(
      read_config(
        path = cli_values$config_path,
        base_dir = dirname(path_absolute(cli_values$config_path, base_dir = base_dir)),
        registry_path = registry_path,
        policy_path = policy_path
      )
    )
  }

  # Build the fallback positional config from the generic registry-derived
  # baseline, then normalize it through the same validator and path resolver.
  config_data <- default_config(
    input_file = cli_values$input_file %||% "input.xlsx",
    output_root = cli_values$output_root %||% "outputs",
    cache_folder = cli_values$cache_folder %||% "cache"
  )
  config_data$execution$strict_length_pdf <- if (is.null(cli_values$strict_length_pdf)) {
    config_data$execution$strict_length_pdf
  } else {
    command_line_true(cli_values$strict_length_pdf)
  }
  config_data$execution$run_multiplier_model <- if (is.null(cli_values$run_multiplier_model)) {
    config_data$execution$run_multiplier_model
  } else {
    command_line_true(cli_values$run_multiplier_model)
  }

  normalize_config(
    config = config_data,
    base_dir = base_dir,
    registry_path = registry_path,
    policy_path = policy_path
  )
}

#' Write a config YAML template
#'
#' Writes a generic config YAML template to a caller-specified path.
#'
#' @param path Output YAML path.
#' @param overwrite Logical scalar. If `TRUE`, overwrite an existing file.
#' @param input_file Input workbook path placeholder.
#' @param output_root Output root directory placeholder.
#' @param cache_folder Cache folder placeholder.
#' @param registry_path Optional trait-registry path used to derive default
#'   trait names.
#' @param policy_path Optional policy-registry path used to derive one default
#'   active policy.
#'
#' @return The written path, invisibly.
#'
#' @export
write_config_yaml <- function(path,
                                overwrite = FALSE,
                                input_file = "input.xlsx",
                                output_root = "outputs",
                                cache_folder = "cache",
                                registry_path = NULL,
                                policy_path = NULL) {
  # Materialize a generic config template instead of copying an analysis-
  # specific packaged YAML file.
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single output YAML path.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("Config YAML already exists. Set 'overwrite = TRUE' to replace it.", call. = FALSE)
  }

  ensure_parent_path(path)
  yaml::write_yaml(
    x = default_config(
      input_file = input_file,
      output_root = output_root,
      cache_folder = cache_folder,
      registry_path = registry_path,
      policy_path = policy_path
    ),
    file = path
  )

  invisible(path)
}

#' Build one packaged script call
#'
#' Builds the `Rscript` command and arguments needed to run the packaged
#' script wrapper with a validated YAML config.
#'
#' @param config_path Config YAML path.
#' @param script_name Packaged script name.
#' @param rscript_path Optional `Rscript` executable path.
#'
#' @return A list with `command` and `args`.
#'
#' @export
script_call_from_config <- function(config_path,
                                 script_name = "swfscfish.R",
                                 rscript_path = NULL) {
  # Validate the config YAML before constructing the external call so the
  # shell runner fails early on malformed config files.
  if (is.null(rscript_path)) {
    rscript_path <- file.path(R.home("bin"), "Rscript")
  }

  config_data <- read_config(config_path)

  list(
    command = rscript_path,
    args = c(
      shQuote(installed_script_path(script_name)),
      "--config",
      shQuote(path_absolute(config_path))
    )
  )
}

#' Run the packaged script
#'
#' Launches the packaged script wrapper with a validated config YAML file.
#'
#' @param config_path Config YAML path.
#' @param script_name Packaged script name.
#' @param rscript_path Optional `Rscript` executable path.
#' @param wait Logical scalar. If `TRUE`, wait for the script to finish.
#'
#' @return The `system2()` exit status.
#'
#' @export
run_script_from_config <- function(config_path,
                                script_name = "swfscfish.R",
                                rscript_path = NULL,
                                wait = TRUE) {
  # Build the validated script call first, then hand it to `system2()` without
  # duplicating the path and config checks here.
  if (!is.logical(wait) || length(wait) != 1 || is.na(wait)) {
    stop("'wait' must be TRUE or FALSE.", call. = FALSE)
  }

  script_call <- script_call_from_config(
    config_path = config_path,
    script_name = script_name,
    rscript_path = rscript_path
  )

  system2(
    command = script_call$command,
    args = script_call$args,
    wait = wait
  )
}

#' Emit a timestamped message
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


