# Utility helpers

#' @importFrom rlang .data .env :=
NULL

#' @importFrom grDevices chull
#' @importFrom stats frequency reorder sd setNames
#' @importFrom utils data getFromNamespace head
NULL

#' Return a fallback value for `NULL` or empty input
#'
#' @param x Primary object.
#' @param y Fallback object.
#' @return `x` when present, otherwise `y`.
#' @name null_coalesce
#' @aliases %||%
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Identify missing biological species identities
#'
#' Candidate ingestion represents generalized equations with a constructed
#' `species_name` such as `"NA NA"`. That display placeholder is not a
#' biological species and must never define a same-species donor pool, a
#' species-block fold, or a species-level benchmark group.
#'
#' @param x Species-identity labels.
#'
#' @return Logical vector; `TRUE` for missing or constructed-NA identities.
#' @keywords internal
#' @noRd
is_missing_species_identity <- function(x) {
  value <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  is.na(value) | !nzchar(value) | grepl("^na(?:\\s+na)*$", value, perl = TRUE)
}

#' Return canonical ordination context fields
#'
#' @param ordination Ordination bundle stored on a `Candidates` or `Alchemist`
#'   object.
#'
#' @return List with `model_scores`, `species_lookup`, and `points_missing_df`.
#' @keywords internal
#' @noRd
policy_selector_ordination_context <- function(ordination) {
  ordination <- ordination %||% list()
  model_obj <- ordination$model %||% list()
  list(
    model_scores = model_obj$model_scores %||%
      model_obj$scores %||%
      ordination$model_scores %||%
      NULL,
    species_lookup = ordination$species_lookup %||% NULL,
    points_missing_df = model_obj$points_missing %||% tibble::tibble()
  )
}

#' Check whether an object inherits from a loaded S7 class
#'
#' @param object Object to test.
#' @param class_name Name of the S7 class object.
#'
#' @return `TRUE` when `object` is an instance of the named loaded S7 class.
#' @keywords internal
#' @noRd
is_s7_instance <- function(object, class_name) {
  if (!inherits(object, "S7_object")) {
    return(FALSE)
  }

  class_obj <- get0(class_name, inherits = TRUE)
  if (is.null(class_obj)) {
    return(FALSE)
  }

  isTRUE(
    tryCatch(
      S7::S7_inherits(object, class_obj),
      error = function(e) FALSE
    )
  )
}

#' Compute grid-cell widths for length-density integration
#'
#' @param x Numeric grid locations.
#'
#' @return Numeric vector of positive cell widths aligned to `x`.
#' @keywords internal
#' @noRd
length_pdf_cell_widths <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  n <- length(x)
  if (n == 0L) {
    return(numeric())
  }
  if (n == 1L) {
    return(1)
  }
  ord <- order(x)
  xs <- x[ord]
  widths <- numeric(n)
  widths[ord[1]] <- max((xs[2] - xs[1]) / 2, .Machine$double.eps)
  widths[ord[n]] <- max((xs[n] - xs[n - 1]) / 2, .Machine$double.eps)
  if (n > 2L) {
    mids <- (xs[3:n] - xs[1:(n - 2)]) / 2
    widths[ord[2:(n - 1)]] <- pmax(mids, .Machine$double.eps)
  }
  widths
}

#' Estimate an empirical length PDF on a regular grid
#'
#' @param length_cm Positive empirical lengths in centimeters.
#' @param n Number of grid points in the estimated density.
#'
#' @return Tibble with `length_cm`, integration weights `f_len`, and
#'   pointwise `pdf_density`.
#' @keywords internal
#' @noRd
estimate_length_pdf_grid <- function(length_cm,
                                     n = 512L) {
  length_cm <- suppressWarnings(as.numeric(length_cm))
  length_cm <- length_cm[is.finite(length_cm) & length_cm > 0]
  if (length(length_cm) == 0L) {
    return(tibble::tibble(length_cm = numeric(), f_len = numeric(), pdf_density = numeric()))
  }
  if (length(unique(length_cm)) < 2L) {
    return(tibble::tibble(length_cm = length_cm[[1]], f_len = 1, pdf_density = NA_real_))
  }
  dens <- stats::density(
    x = length_cm,
    n = as.integer(n),
    from = min(length_cm, na.rm = TRUE),
    to = max(length_cm, na.rm = TRUE),
    na.rm = TRUE
  )
  density_y <- pmax(as.numeric(dens$y), 0)
  grid_x <- as.numeric(dens$x)
  cell_widths <- length_pdf_cell_widths(grid_x)
  f_len <- density_y * cell_widths
  if (sum(f_len, na.rm = TRUE) <= 0) {
    f_len <- rep(1 / length(grid_x), length(grid_x))
  } else {
    f_len <- f_len / sum(f_len, na.rm = TRUE)
  }
  tibble::tibble(
    length_cm = grid_x,
    f_len = f_len,
    pdf_density = density_y
  )
}

#' Normalize one anchor length PDF input
#'
#' @param x Raw empirical lengths or an explicit PDF-like table.
#'
#' @return Tibble with `length_cm` and normalized `f_len`.
#' @keywords internal
#' @noRd
normalize_anchor_pdf_input <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(tibble::tibble(length_cm = numeric(), f_len = numeric()))
  }

  if (is.numeric(x)) {
    length_cm <- suppressWarnings(as.numeric(x))
    length_cm <- length_cm[is.finite(length_cm) & length_cm > 0]
    if (length(length_cm) == 0) {
      stop("Raw reference length distributions must contain positive numeric lengths.", call. = FALSE)
    }
    return(estimate_length_pdf_grid(length_cm))
  }

  if (is.data.frame(x)) {
    x <- tibble::as_tibble(x)
    if (!"length_cm" %in% names(x)) {
      stop("Reference PDF tables must contain a 'length_cm' column.", call. = FALSE)
    }
    length_cm <- suppressWarnings(as.numeric(x$length_cm))
    density_col <- intersect(c("pdf_density", "density"), names(x))
    pdf_density <- if (length(density_col) > 0L) {
      suppressWarnings(as.numeric(x[[density_col[[1]]]]))
    } else {
      rep(NA_real_, length(length_cm))
    }
    if ("f_len" %in% names(x)) {
      f_len <- suppressWarnings(as.numeric(x$f_len))
    } else if (any(is.finite(pdf_density) & pdf_density > 0)) {
      f_len <- pmax(pdf_density, 0) * length_pdf_cell_widths(length_cm)
    } else {
      f_len <- rep(1, length(length_cm))
    }
    out <- tibble::tibble(length_cm = length_cm, f_len = f_len, pdf_density = pdf_density) |>
      dplyr::filter(
        is.finite(.data$length_cm),
        .data$length_cm > 0,
        is.finite(.data$f_len),
        .data$f_len >= 0
      ) |>
      dplyr::group_by(.data$length_cm) |>
      dplyr::summarise(
        f_len = sum(.data$f_len),
        pdf_density = if (any(is.finite(.data$pdf_density))) {
          stats::weighted.mean(
            .data$pdf_density,
            w = pmax(.data$f_len, 0),
            na.rm = TRUE
          )
        } else {
          NA_real_
        },
        .groups = "drop"
      )
    if (nrow(out) == 0 || sum(out$f_len, na.rm = TRUE) <= 0) {
      stop("Reference PDF tables must contain positive finite support.", call. = FALSE)
    }
    out$f_len <- out$f_len / sum(out$f_len, na.rm = TRUE)
    return(dplyr::arrange(out, .data$length_cm))
  }

  stop(
    "Reference PDFs must be supplied as numeric length vectors or tables with 'length_cm'.",
    call. = FALSE
  )
}

#' Extract a stored user PDF from one anchor row
#'
#' @param anchor_row One-row anchor table.
#'
#' @return Tibble with `length_cm` and `f_len`, or zero rows.
#' @keywords internal
#' @noRd
anchor_pdf_from_stored_row <- function(anchor_row) {
  anchor_row <- tibble::as_tibble(anchor_row)
  if (nrow(anchor_row) == 0 || !"length_pdf_data" %in% names(anchor_row)) {
    return(tibble::tibble(length_cm = numeric(), f_len = numeric()))
  }

  pdf_now <- anchor_row$length_pdf_data[[1]] %||% NULL
  if (is.null(pdf_now)) {
    return(tibble::tibble(length_cm = numeric(), f_len = numeric()))
  }

  normalize_anchor_pdf_input(pdf_now)
}

#' Build an anchor length PDF from supplied data or study-length metadata
#'
#' @param anchor_row One-row anchor table.
#' @param config Optional normalized configuration. When supplied, its anchor
#'   field mapping determines the study-length columns.
#' @param n Number of points for an interval PDF.
#' @param on_missing Whether unavailable or invalid study-length support errors
#'   or returns an empty PDF.
#'
#' @return Tibble with `length_cm` and `f_len`.
#' @keywords internal
#' @noRd
build_anchor_length_pdf <- function(anchor_row,
                                    config = NULL,
                                    n = 400L,
                                    on_missing = c("error", "empty")) {
  anchor_row <- tibble::as_tibble(anchor_row)
  on_missing <- match.arg(on_missing)
  n <- suppressWarnings(as.integer(n[[1]]))
  if (!is.finite(n) || n < 1L) {
    stop("'n' must be a positive integer.", call. = FALSE)
  }

  user_pdf <- anchor_pdf_from_stored_row(anchor_row)
  if (nrow(user_pdf) > 0L) {
    return(user_pdf)
  }

  empty_pdf <- function() {
    tibble::tibble(length_cm = numeric(), f_len = numeric())
  }
  unavailable <- function(message, reason_code) {
    if (identical(on_missing, "empty")) {
      return(empty_pdf())
    }
    abort_unscorable_anchor(message, reason_code = reason_code)
  }
  anchor_values <- function(column) {
    if (!column %in% names(anchor_row)) {
      return(numeric())
    }
    suppressWarnings(as.numeric(anchor_row[[column]]))
  }

  if (is.null(config)) {
    len_min_col <- "study_length_min"
    len_max_col <- "study_length_max"
    len_mid_col <- "study_length_midpoint"
  } else {
    len_min_col <- build_anchor_field(config, "length_min")
    len_max_col <- build_anchor_field(config, "length_max")
    len_mid_col <- build_anchor_field(config, "length_midpoint")
  }

  mins <- anchor_values(len_min_col)
  maxs <- anchor_values(len_max_col)
  finite_mins <- mins[is.finite(mins)]
  finite_maxs <- maxs[is.finite(maxs)]
  if (length(finite_mins) > 0L && length(finite_maxs) > 0L) {
    lmin <- min(pmin(finite_mins, finite_maxs))
    lmax <- max(pmax(finite_mins, finite_maxs))
    if (lmin <= 0 || lmax <= 0) {
      return(unavailable(
        "Anchor length interval must be positive and finite.",
        reason_code = "invalid_study_length_interval"
      ))
    }
    if (lmax > lmin) {
      grid <- seq(lmin, lmax, length.out = n)
      return(tibble::tibble(
        length_cm = grid,
        f_len = rep(1 / length(grid), length(grid))
      ))
    }
    return(tibble::tibble(length_cm = lmin, f_len = 1))
  }

  mids <- anchor_values(len_mid_col)
  mids <- mids[is.finite(mids) & mids > 0]
  if (length(mids) > 0L) {
    return(tibble::tibble(length_cm = mids[[1]], f_len = 1))
  }
  unavailable(
    "No valid anchor study length interval or midpoint was available.",
    reason_code = "missing_study_length_support"
  )
}

#' Emit one progress message
#'
#' @param progress Logical scalar.
#' @param ... Message fragments.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
report_progress <- function(progress,
                            ...) {
  if (!isTRUE(progress)) {
    return(invisible(NULL))
  }

  tsb_message(...)
  invisible(NULL)
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
#' @noRd
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
  # separators so downstream code sees one stable path form.
  if (!fs::is_absolute_path(path)) {
    path <- file.path(base_dir, path)
  }

  if (!must_work && !file.exists(path)) {
    ancestor <- path
    suffix <- character(0)
    while (!file.exists(ancestor)) {
      parent <- dirname(ancestor)
      if (identical(parent, ancestor)) {
        break
      }
      suffix <- c(basename(ancestor), suffix)
      ancestor <- parent
    }
    if (file.exists(ancestor)) {
      resolved <- normalizePath(ancestor, winslash = "/", mustWork = TRUE)
      if (length(suffix) > 0L) {
        resolved <- do.call(file.path, c(list(resolved), as.list(suffix)))
      }
      return(gsub("\\\\", "/", resolved))
    }
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
#' @noRd
ensure_parent_path <- function(path) {
  # Create only the containing directory so callers can safely prepare output
  # file paths before writing logs, caches, or templates.
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single non-empty path.", call. = FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

#' Parse a strict environment flag
#'
#' @param value Logical scalar or one environment-string value.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
parse_environment_flag <- function(value) {
  if (is.logical(value) && length(value) == 1L && !is.na(value)) {
    return(value)
  }
  if (!is.character(value) || length(value) != 1L) {
    stop("Environment flag must be one logical or character value.", call. = FALSE)
  }
  normalized <- stringr::str_to_lower(stringr::str_squish(value))
  if (normalized %in% c("true", "t", "1", "yes", "y")) {
    return(TRUE)
  }
  if (normalized %in% c("false", "f", "0", "no", "n")) {
    return(FALSE)
  }
  stop("Environment flag must be a recognized true/false value.", call. = FALSE)
}

#' Resolve an installed template path
#'
#' @param name Template filename under `inst/templates`.
#'
#' @return Character scalar path.
#' @keywords internal
#' @noRd
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
#' @noRd
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

#' Initialize a parallel cluster
#'
#' Starts a PSOCK cluster and loads the installed package namespace on each
#' worker.
#'
#' @param workers Number of workers to start.
#' @param package_name Installed package name to load when available.
#' @param worker_output Logical scalar. When `TRUE`, PSOCK worker output is
#'   relayed to the parent console. Fork clusters already share the console.
#'
#' @return A cluster object, or `NULL` when `workers` is `1`.
#' @keywords internal
#' @noRd
initialize_parallel_cluster <- function(workers,
                                        package_name = "tsbiomass",
                                        worker_output = FALSE) {
  # Keep the parallel setup logic in one place so benchmark and sensitivity
  # reruns both initialize workers the same way.
  if (!is.numeric(workers) || length(workers) != 1 || !is.finite(workers) || workers < 1) {
    stop("'workers' must be one finite number >= 1.", call. = FALSE)
  }
  if (!is.character(package_name) || length(package_name) != 1 || !nzchar(package_name)) {
    stop("'package_name' must be a single non-empty package name.", call. = FALSE)
  }
  if (!is.logical(worker_output) || length(worker_output) != 1L || is.na(worker_output)) {
    stop("'worker_output' must be TRUE or FALSE.", call. = FALSE)
  }

  workers <- as.integer(workers)
  if (workers <= 1L) {
    return(NULL)
  }

  # Fork-based clusters on Unix share the parent process memory via
  # copy-on-write - no serialization, no package loading, near-zero startup.
  # PSOCK is used on Windows where fork is unavailable.
  # Wrap in tryCatch to fall back gracefully in environments that disable
  # forking (e.g. some RStudio configurations on macOS).
  if (.Platform$OS.type == "unix") {
    cluster_obj <- tryCatch(
      parallel::makeForkCluster(workers),
      error = function(e) NULL
    )
    if (!is.null(cluster_obj)) {
      attr(cluster_obj, "cluster_type") <- "fork"
      return(cluster_obj)
    }
  }

  # Some cross-platform shells inject the Unix-only `C.UTF-8` locale into the
  # Windows process environment. Windows R warns once for every PSOCK worker
  # when it inherits those values. Temporarily remove only that invalid value
  # while workers are spawned, then restore the parent environment unchanged.
  if (.Platform$OS.type == "windows") {
    locale_names <- c("LANG", "LC_ALL", "LC_CTYPE", "LC_COLLATE", "LC_MONETARY", "LC_TIME")
    locale_values <- Sys.getenv(locale_names, unset = NA_character_)
    invalid_locale <- !is.na(locale_values) &
      toupper(gsub("[^A-Z0-9]", "", locale_values)) == "CUTF8"
    if (any(invalid_locale)) {
      invalid_names <- locale_names[invalid_locale]
      invalid_values <- locale_values[invalid_locale]
      Sys.unsetenv(invalid_names)
      on.exit(
        do.call(Sys.setenv, as.list(stats::setNames(invalid_values, invalid_names))),
        add = TRUE
      )
    }
  }

  cluster_obj <- if (isTRUE(worker_output)) {
    parallel::makePSOCKcluster(workers, outfile = "")
  } else {
    parallel::makePSOCKcluster(workers)
  }
  attr(cluster_obj, "cluster_type") <- "psock"
  library_paths <- .libPaths()
  namespace_path <- normalizePath(
    getNamespaceInfo(asNamespace(package_name), "path"),
    winslash = "/",
    mustWork = FALSE
  )
  installed_candidates <- file.path(library_paths, package_name)
  installed_candidates <- installed_candidates[dir.exists(installed_candidates)]
  installed_path <- if (length(installed_candidates) > 0L) {
    normalizePath(installed_candidates[[1]], winslash = "/", mustWork = FALSE)
  } else {
    NA_character_
  }
  # PSOCK workers otherwise resolve a prior installed package during local
  # development, rather than the source currently loaded in the parent.
  development_path <- if (
    file.exists(file.path(namespace_path, "DESCRIPTION")) &&
      (is.na(installed_path) || !identical(namespace_path, installed_path))
  ) namespace_path else NA_character_
  parallel::clusterExport(
    cluster_obj,
    c("library_paths", "package_name", "development_path"),
    envir = environment()
  )

  tryCatch(
    {
      parallel::clusterEvalQ(
        cluster_obj,
        {
          .libPaths(unique(c(library_paths, .libPaths())))
          loadNamespace("graphics")
          loadNamespace("stats")
          loadNamespace("methods")
          if (!is.na(development_path)) {
            if (package_name %in% loadedNamespaces()) {
              unloadNamespace(package_name)
            }
            pkgload::load_all(
              development_path,
              quiet = TRUE,
              export_all = FALSE,
              helpers = FALSE
            )
          } else {
            package_libraries <- library_paths[
              file.exists(file.path(library_paths, package_name, "DESCRIPTION"))
            ]
            package_library <- if (length(package_libraries) > 0L) {
              package_libraries[[1]]
            } else {
              NA_character_
            }
            if (package_name %in% loadedNamespaces()) {
              loaded_path <- normalizePath(
                getNamespaceInfo(asNamespace(package_name), "path"),
                winslash = "/",
                mustWork = FALSE
              )
              target_path <- normalizePath(
                file.path(package_library, package_name),
                winslash = "/",
                mustWork = FALSE
              )
              if (!identical(loaded_path, target_path)) {
                unloadNamespace(package_name)
              }
            }
            if (is.na(package_library) || !dir.exists(file.path(package_library, package_name))) {
              if (!requireNamespace(package_name, quietly = TRUE)) {
                stop(
                  sprintf("Parallel workers could not load installed package '%s'.", package_name),
                  call. = FALSE
                )
              }
            } else if (!requireNamespace(package_name, lib.loc = package_library, quietly = TRUE)) {
              stop(
                sprintf(
                  "Parallel workers could not load installed package '%s' from '%s'.",
                  package_name,
                  package_library
                ),
                call. = FALSE
              )
            }
            suppressPackageStartupMessages(
              if (is.na(package_library)) {
                library(package_name, character.only = TRUE)
              } else {
                library(package_name, character.only = TRUE, lib.loc = package_library)
              }
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


#' @keywords internal
#' @noRd
tsb_cluster_export <- function(cl, varlist, envir = parent.frame()) {
  if (is.null(cl) || identical(attr(cl, "cluster_type"), "fork")) {
    return(invisible(NULL))
  }
  parallel::clusterExport(cl, varlist, envir = envir)
}

#' Describe a parallel cluster
#'
#' @param cl Cluster object returned by [initialize_parallel_cluster()].
#'
#' @return One short human-readable cluster description.
#' @keywords internal
#' @noRd
parallel_cluster_description <- function(cl) {
  if (is.null(cl)) {
    return("sequential")
  }
  cluster_type <- attr(cl, "cluster_type") %||% "unknown"
  n_workers <- length(cl)
  sprintf("%s cluster, %d worker(s)", cluster_type, n_workers)
}
