#' Build a workflow logger
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
#' @export
build_workflow_logger <- function(log_file = NULL,
                                  write_log = FALSE,
                                  append = FALSE) {
  # Keep file logging optional because the console stream is already the
  # primary workflow output during command-line execution.
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

#' Write one workflow log message
#'
#' Emits a message to the console and, when enabled, appends the same message
#' to the workflow log file.
#'
#' @param logger Logger object from [build_workflow_logger()].
#' @param ... Message fragments.
#' @param timestamp Logical scalar. If `TRUE`, prefix the file log entry with a
#'   timestamp.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
log_message <- function(logger,
                        ...,
                        timestamp = TRUE) {
  # Build the message text once so the console and optional file log stay in
  # sync.
  if (!is.list(logger) || is.null(logger$write_log)) {
    stop("'logger' must be a logger object returned by 'build_workflow_logger()'.", call. = FALSE)
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

#' Log a workflow header
#'
#' Writes a standard workflow header block through a workflow logger.
#'
#' @param logger Logger object from [build_workflow_logger()].
#' @param title Header title text.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
log_header <- function(logger,
                       title = "TS Biomass Model Transferability Workflow") {
  # Emit a small standard header so command-line runs have a recognizable
  # start marker in both console and optional file logs.
  log_message(logger, "", timestamp = FALSE)
  log_message(logger, "====================================================================", timestamp = FALSE)
  log_message(logger, title, timestamp = FALSE)
  log_message(logger, "====================================================================", timestamp = FALSE)
  invisible(NULL)
}
