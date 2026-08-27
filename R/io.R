#' IO communication for logging, CLI parsing, and console display headers

#' Build a run logger
#'
#' Creates a simple logger object that always writes to the console and can
#' optionally mirror messages to a log file.
#'
#' @param log_file Optional log-file path.
#' @param write_log Logical scalar. If `TRUE`, append messages to `log_file`.
#' @param append Logical scalar. If `TRUE`, append to an existing log file.
#'
#' @return A logger list.
#'
#' @keywords internal
#' @noRd
build_run_logger <- function(log_file = NULL,
                             write_log = FALSE,
                             append = FALSE) {
  # Keep file logging optional because the console stream is already the
  # primary console output during command-line execution.
  if (!is.logical(write_log) || length(write_log) != 1 || is.na(write_log)) {
    stop("'write_log' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(append) || length(append) != 1 || is.na(append)) {
    stop("'append' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(log_file) &&
    (!is.character(log_file) || length(log_file) != 1 || !nzchar(log_file))) {
    stop("'log_file' must be NULL or a single non-empty path.", call. = FALSE)
  }

  # Resolve the effective file path only when file logging is enabled so YAML
  # configs can omit `paths.log_file` entirely when they do not want a log.
  if (!isTRUE(write_log)) {
    return(list(write_log = FALSE, log_file = NULL, append = FALSE))
  }
  if (is.null(log_file)) {
    stop("A 'log_file' path is required when 'write_log = TRUE'.", call. = FALSE)
  }

  ensure_parent_path(log_file)
  if (!isTRUE(append) && file.exists(log_file)) {
    file.remove(log_file)
  }

  list(
    write_log = TRUE,
    log_file = log_file,
    append = append
  )
}

#' Write one run log message
#'
#' Emits a message to the console and, when enabled, appends the same message
#' to the run log file.
#'
#' @param logger Logger object from `build_run_logger()`.
#' @param ... Message fragments.
#' @param timestamp Logical scalar. If `TRUE`, prefix the file log entry with a
#'   timestamp.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
log_message <- function(logger,
                        ...,
                        timestamp = TRUE) {
  # Build the message text once so the console and optional file log stay in
  # sync.
  if (!is.list(logger) || is.null(logger$write_log)) {
    stop("'logger' must be a logger object returned by 'build_run_logger()'.", call. = FALSE)
  }
  if (!is.logical(timestamp) || length(timestamp) != 1 || is.na(timestamp)) {
    stop("'timestamp' must be TRUE or FALSE.", call. = FALSE)
  }

  message_text <- paste0(...)
  console_text <- if (isTRUE(timestamp)) {
    paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", message_text)
  } else {
    message_text
  }
  message(console_text)

  # Append the same message to the log file only when file logging is enabled.
  if (isTRUE(logger$write_log)) {
    line_text <- if (isTRUE(timestamp)) {
      paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", message_text)
    } else {
      message_text
    }
    cat(line_text, "\n", file = logger$log_file, append = TRUE)
  }

  invisible(NULL)
}

#' Collapse values for compact display
#'
#' @param values Character vector.
#' @param limit Maximum number of values to display.
#'
#' @return Character scalar.
#'
#' @keywords internal
#' @noRd
preview_values <- function(values,
                           limit = 5L) {
  values <- unique(as.character(values %||% character(0)))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) {
    return("none")
  }
  if (length(values) <= limit) {
    return(paste(values, collapse = ", "))
  }

  paste0(
    paste(values[seq_len(limit)], collapse = ", "),
    ", ... (",
    length(values),
    " total)"
  )
}

#' Parse command-line arguments
#'
#' Parses the command-line interface used by the packaged script wrapper.
#'
#' @param arguments Character vector, usually `commandArgs(trailingOnly = TRUE)`.
#'
#' @return A list describing the requested command-line action.
#'
#' @keywords internal
#' @noRd
parse_command_line <- function(arguments = commandArgs(trailingOnly = TRUE)) {
  if (length(arguments) == 0) {
    stop("Use '--config <path>' to supply a config YAML file.", call. = FALSE)
  }

  if (identical(arguments[[1]], "--write-template")) {
    if (length(arguments) != 2 || !nzchar(arguments[[2]])) {
      stop("Use '--write-template <path>' to scaffold a config YAML file.", call. = FALSE)
    }

    return(list(action = "write_template", path = arguments[[2]]))
  }

  if (identical(arguments[[1]], "--config")) {
    if (length(arguments) != 2 || !nzchar(arguments[[2]])) {
      stop("Use '--config <path>' to supply a config YAML file.", call. = FALSE)
    }

    return(list(action = "run", config_path = arguments[[2]]))
  }

  stop(
    "Unsupported command-line arguments. Use '--config <path>' or '--write-template <path>'.",
    call. = FALSE
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
#' @keywords internal
#' @noRd
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
    x = build_configuration_template(
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

#' Read a `write_scorecard()` report
#'
#' Parses the plain-text report written by [write_scorecard()] back into a
#' tibble, one row per anchor. This is a parser for that specific fixed
#' format (`## <anchor>` headings, `- Label: value` fields); it is not a
#' general Markdown reader.
#'
#' @param path Report file path, as written by [write_scorecard()].
#'
#' @return A tibble with one row per anchor and columns `anchor_species`,
#'   `anchor_is_external`, `selected_policy_display`, `selected_equation_branch_filter`,
#'   `selection_tier`, `realized_donor_fingerprint`, `realized_n_unique_donors`,
#'   `selected_realized_transfer_display`, `selected_donor_model_ids`,
#'   `selected_donor_model_summary`, `selected_donor_model_details`,
#'   `policy_slope_len`, `policy_intercept_len`, `multiplier_pred`,
#'   `meta_post_selection_multiplier_lo`, `meta_post_selection_multiplier_hi`,
#'   `prediction_error_message`, `meta_q_abs_log_total`.
#'
#' @export
read_scorecard <- function(path) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single report file path.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Report file does not exist: ", path, call. = FALSE)
  }

  lines <- readLines(path, warn = FALSE)
  heading_idx <- grep("^## ", lines)
  if (length(heading_idx) == 0) {
    return(tibble::tibble(
      anchor_species = character(0),
      anchor_is_external = logical(0),
      selected_policy_display = character(0),
      selected_equation_branch_filter = character(0),
      selection_tier = character(0),
      selected_realized_transfer_display = character(0),
      realized_donor_fingerprint = character(0),
      realized_n_unique_donors = numeric(0),
      selected_donor_model_ids = character(0),
      selected_donor_model_summary = character(0),
      selected_donor_model_details = character(0),
      policy_slope_len = numeric(0),
      policy_intercept_len = numeric(0),
      multiplier_pred = numeric(0),
      meta_post_selection_multiplier_lo = numeric(0),
      meta_post_selection_multiplier_hi = numeric(0),
      prediction_error_message = character(0),
      meta_q_abs_log_total = numeric(0)
    ))
  }

  block_end <- c(heading_idx[-1] - 1, length(lines))
  rows <- lapply(seq_along(heading_idx), function(i) {
    block <- lines[heading_idx[[i]]:block_end[[i]]]
    heading <- sub("^## ", "", block[[1]])
    is_external <- grepl("\\(external query\\)$", heading)
    species <- stringr::str_trim(sub("\\s*\\(external query\\)$", "", heading))

    field <- function(pattern, group = 1) {
      hit <- stringr::str_match(block, pattern)[, group + 1]
      hit <- hit[!is.na(hit)]
      if (length(hit) == 0) NA_character_ else hit[[1]]
    }
    as_num <- function(x) suppressWarnings(as.numeric(x))

    policy_field <- field("^- Selected policy: (.*) \\[(.*)\\]$", group = 1)
    branch_field <- field("^- Selected policy: (.*) \\[(.*)\\]$", group = 2)
    realized_transfer <- field("^- Realized transfer: (.*)$")
    tier_field <- field("^- Selection tier: (.*)$")
    donor_line <- field("^- Donors: (.*) \\((\\d+) unique\\)$", group = 1)
    donor_n <- field("^- Donors: (.*) \\((\\d+) unique\\)$", group = 2)
    donor_model_ids_line <- field("^- Donor model IDs: (.*)$")
    donor_model_summary <- field("^- Donor summary: (.*)$")
    donor_model_details <- field("^- Donor model details: (.*)$")
    donor_fp <- if (!is.na(donor_line) && identical(donor_line, "none")) {
      NA_character_
    } else if (!is.na(donor_line)) {
      paste(stringr::str_split(donor_line, ", ")[[1]], collapse = "|")
    } else {
      NA_character_
    }

    slope_field <- field("^- TS-length equation: slope = (\\S+), intercept = (\\S+)$", group = 1)
    intercept_field <- field("^- TS-length equation: slope = (\\S+), intercept = (\\S+)$", group = 2)

    mult_field <- field("^- Biomass multiplier: (\\S+) \\((\\S+) - (\\S+)\\)$", group = 1)
    mult_lo_field <- field("^- Biomass multiplier: (\\S+) \\((\\S+) - (\\S+)\\)$", group = 2)
    mult_hi_field <- field("^- Biomass multiplier: (\\S+) \\((\\S+) - (\\S+)\\)$", group = 3)
    err_field <- field("^- Biomass multiplier: not computable \\((.*)\\)$")

    q_field <- field("^- Uncertainty \\(log scale\\): (\\S+)$")

    tibble::tibble(
      anchor_species = species,
      anchor_is_external = is_external,
      selected_policy_display = policy_field,
      selected_equation_branch_filter = branch_field,
      selection_tier = tier_field,
      selected_realized_transfer_display = realized_transfer,
      realized_donor_fingerprint = donor_fp,
      realized_n_unique_donors = as_num(donor_n),
      selected_donor_model_ids = if (!is.na(donor_model_ids_line) && !identical(donor_model_ids_line, "none")) {
        paste(stringr::str_split(donor_model_ids_line, ", ")[[1]], collapse = "|")
      } else {
        NA_character_
      },
      selected_donor_model_summary = donor_model_summary,
      selected_donor_model_details = donor_model_details,
      policy_slope_len = as_num(slope_field),
      policy_intercept_len = as_num(intercept_field),
      multiplier_pred = as_num(mult_field),
      meta_post_selection_multiplier_lo = as_num(mult_lo_field),
      meta_post_selection_multiplier_hi = as_num(mult_hi_field),
      prediction_error_message = err_field,
      meta_q_abs_log_total = as_num(q_field)
    )
  })

  dplyr::bind_rows(rows)
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
#' @keywords internal
#' @noRd
script_call_from_config <- function(config_path,
                                    script_name = "swfscfish.R",
                                    rscript_path = NULL) {
  # Validate the config YAML before constructing the external call so the
  # shell runner fails early on malformed config files.
  if (is.null(rscript_path)) {
    rscript_path <- file.path(R.home("bin"), "Rscript")
  }

  config_data <- read_configuration(config_path)

  # Force evaluation
  force(config_data)

  list(
    command = rscript_path,
    args = c(
      shQuote(installed_script_path(script_name)),
      "--config",
      shQuote(path_absolute(config_path))
    )
  )
}

#' Emit a timestamped message
#'
#' @param ... Message components.
#' @param timestamp Boolean that dictates whether to prepend with timestamp.
#' @param appendLF Passed to `base::message()`.
#' @return Invisibly returns `NULL`.
#' @export
tsb_message <- function(..., timestamp = TRUE, appendLF = TRUE) {
  if (isTRUE(timestamp)) {
    base::message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = ""), appendLF = appendLF)
  } else {
    base::message(paste0(..., collapse = ""), appendLF = appendLF)
  }
}
