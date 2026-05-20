# Utility helpers

#' Return a fallback value for `NULL` or empty input
#'
#' @param x Primary object.
#' @param y Fallback object.
#' @return `x` when present, otherwise `y`.
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}


#' Recursively merge two lists
#'
#' @param x Base list.
#' @param y Override list.
#' @return A recursively merged list.
#' @keywords internal
merge_cfg <- function(x, y) {
  if (is.null(y)) {
    return(x)
  }

  out <- x
  for (nm in names(y)) {
    x_val <- out[[nm]]
    y_val <- y[[nm]]
    out[[nm]] <- if (is.list(x_val) && is.list(y_val)) {
      merge_cfg(x_val, y_val)
    } else {
      y_val
    }
  }
  out
}

#' Resolve an absolute path
#'
#' Resolves a relative or absolute path against a base directory without
#' hard-coding any machine-specific locations.
#'
#' @param path Character scalar path.
#' @param base_dir Base directory used for relative paths.
#' @param must_work Logical scalar. If `TRUE`, require the resolved path to
#'   exist.
#'
#' @return Character scalar absolute path.
#' @keywords internal
path_absolute <- function(path,
                          base_dir = getwd(),
                          must_work = FALSE) {
  # Validate both path inputs before resolution so failures point to the caller
  # arguments rather than to `normalizePath()`.
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single non-empty path.", call. = FALSE)
  }
  if (!is.character(base_dir) || length(base_dir) != 1 || !nzchar(base_dir)) {
    stop("'base_dir' must be a single non-empty path.", call. = FALSE)
  }
  if (!is.logical(must_work) || length(must_work) != 1 || is.na(must_work)) {
    stop("'must_work' must be TRUE or FALSE.", call. = FALSE)
  }

  # Resolve relative paths against the supplied base directory and normalize
  # separators so downstream workflow code sees one stable path form.
  if (!fs::is_absolute_path(path)) {
    path <- file.path(base_dir, path)
  }

  normalizePath(path, winslash = "/", mustWork = must_work)
}

#' Create a parent directory
#'
#' Creates the parent directory for a file path when it does not already exist.
#'
#' @param path Character scalar file path.
#'
#' @return The input path, invisibly.
#' @keywords internal
ensure_parent_path <- function(path) {
  # Create only the containing directory so callers can safely prepare output
  # file paths before writing logs, caches, or templates.
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single non-empty path.", call. = FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

#' Parse a command-line boolean
#'
#' Converts common command-line boolean strings to `TRUE` or `FALSE`.
#'
#' @param value Value to parse.
#'
#' @return Logical scalar.
#' @keywords internal
command_line_true <- function(value) {
  # Accept the common command-line truthy spellings so the workflow wrapper can
  # keep its positional interface simple.
  if (is.logical(value) && length(value) == 1 && !is.na(value)) {
    return(value)
  }

  value <- tolower(stringr::str_squish(as.character(value %||% "")))
  value %in% c("true", "t", "1", "yes", "y")
}

#' Resolve an installed template path
#'
#' @param name Template filename under `inst/templates`.
#'
#' @return Character scalar path.
#' @keywords internal
installed_template_path <- function(name) {
  # Look up packaged templates only through `system.file()` so no machine-local
  # directory assumptions leak into the package runtime.
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    stop("'name' must be a single template filename.", call. = FALSE)
  }

  template_path <- system.file("templates", name, package = "tsbiomass")
  if (!nzchar(template_path)) {
    stop(sprintf("Packaged template not found: %s", name), call. = FALSE)
  }

  template_path
}

#' Resolve an installed script path
#'
#' @param name Script filename under `inst/scripts`.
#'
#' @return Character scalar path.
#' @keywords internal
installed_script_path <- function(name) {
  # Resolve packaged command-line scripts through the installed package layout.
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    stop("'name' must be a single script filename.", call. = FALSE)
  }

  script_path <- system.file("scripts", name, package = "tsbiomass")
  if (!nzchar(script_path)) {
    stop(sprintf("Packaged script not found: %s", name), call. = FALSE)
  }

  script_path
}

#' Resolve policy names from a table
#'
#' @param tbl Data frame or tibble.
#'
#' @return Character vector.
#' @keywords internal
resolve_policy_names <- function(tbl) {
  out <- tibble::as_tibble(tbl)

  if ("policy" %in% names(out)) {
    return(as.character(out$policy))
  }

  rep(NA_character_, nrow(out))
}

#' Resolve displayed selected policy names from a table
#'
#' @param tbl Data frame or tibble.
#'
#' @return Character vector.
#' @keywords internal
resolve_selected_policy_values <- function(tbl) {
  out <- tibble::as_tibble(tbl)

  if ("selected_policy" %in% names(out)) {
    return(as.character(out$selected_policy))
  }

  rep(NA_character_, nrow(out))
}

#' Resolve displayed selected policy names from a table
#'
#' @param tbl Data frame or tibble.
#'
#' @return Character vector.
#' @keywords internal
resolve_selected_policy_names <- function(tbl) {
  out <- tibble::as_tibble(tbl)

  if ("selected_policy_display" %in% names(out)) {
    return(as.character(out$selected_policy_display))
  }
  if ("selected_policy" %in% names(out)) {
    return(as.character(out$selected_policy))
  }

  rep(NA_character_, nrow(out))
}

#' Resolve equivalent policy sets from a table
#'
#' @param tbl Data frame or tibble.
#'
#' @return Character vector.
#' @keywords internal
resolve_equivalent_policy_sets <- function(tbl) {
  out <- tibble::as_tibble(tbl)

  if ("equivalent_policy_set" %in% names(out)) {
    return(as.character(out$equivalent_policy_set))
  }

  rep(NA_character_, nrow(out))
}

#' Initialize a parallel cluster
#'
#' Starts a PSOCK cluster and loads the current package source on each worker
#' when running from a source checkout.
#'
#' @param workers Number of workers to start.
#' @param package_dir Optional package source directory used for
#'   `pkgload::load_all()` when the package is not installed.
#' @param package_name Installed package name to load when available.
#'
#' @return A cluster object, or `NULL` when `workers` is `1`.
#' @keywords internal
initialize_parallel_cluster <- function(workers,
                                        package_dir = NULL,
                                        package_name = "tsbiomass") {
  # Keep the parallel setup logic in one place so benchmark and sensitivity
  # reruns both initialize workers the same way.
  if (!is.numeric(workers) || length(workers) != 1 || !is.finite(workers) || workers < 1) {
    stop("'workers' must be one finite number >= 1.", call. = FALSE)
  }
  if (!is.null(package_dir) &&
      (!is.character(package_dir) || length(package_dir) != 1 || !nzchar(package_dir))) {
    stop("'package_dir' must be NULL or a single non-empty path.", call. = FALSE)
  }
  if (!is.character(package_name) || length(package_name) != 1 || !nzchar(package_name)) {
    stop("'package_name' must be a single non-empty package name.", call. = FALSE)
  }

  workers <- as.integer(workers)
  if (workers <= 1L) {
    return(NULL)
  }

  # When the caller did not supply a source path explicitly, try to detect one
  # from a live pkgload/devtools session so workers use the same source tree as
  # the current process instead of falling back to a stale installed copy.
  if (is.null(package_dir) && package_name %in% loadedNamespaces()) {
    ns_obj <- asNamespace(package_name)
    ns_path <- tryCatch(
      getNamespaceInfo(ns_obj, "path"),
      error = function(e) NULL
    )
    if (is.character(ns_path) &&
        length(ns_path) == 1 &&
        nzchar(ns_path) &&
        file.exists(file.path(ns_path, "DESCRIPTION"))) {
      package_dir <- ns_path
    }
  }

  cluster_obj <- parallel::makePSOCKcluster(workers)
  parallel::clusterExport(
    cluster_obj,
    c("package_dir", "package_name"),
    envir = environment()
  )

  tryCatch(
    {
      parallel::clusterEvalQ(
        cluster_obj,
        {
          if (!is.null(package_dir) &&
              file.exists(file.path(package_dir, "DESCRIPTION")) &&
              requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(
              package_dir,
              export_all = TRUE,
              helpers = FALSE,
              quiet = TRUE
            )
          } else if (requireNamespace(package_name, quietly = TRUE)) {
            suppressPackageStartupMessages(
              library(package_name, character.only = TRUE)
            )
          } else {
            stop(
              "Parallel workers could not load the package source or an installed package copy.",
              call. = FALSE
            )
          }

          NULL
        }
      )
    },
    error = function(e) {
      parallel::stopCluster(cluster_obj)
      stop(conditionMessage(e), call. = FALSE)
    }
  )

  cluster_obj
}
