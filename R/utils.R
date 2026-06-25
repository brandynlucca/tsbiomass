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

#' Resolve config data
#'
#' @param config Config input.
#'
#' @return A config list.
#' @keywords internal
config_data <- function(config) {
  if ((inherits(config, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
    return(config@data)
  }
  if ((inherits(config, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Candidates), error = function(e) FALSE)))) {
    return(candidates_config_data(config) %||% list())
  }
  if ((inherits(config, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, PolicySelector), error = function(e) FALSE)))) {
    return(config@config %||% list())
  }
  if ((inherits(config, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, PolicyLearner), error = function(e) FALSE)))) {
    return(policy_learner_config(config))
  }
  if ((inherits(config, "S7_object") && exists("PolicySimulator", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, PolicySimulator), error = function(e) FALSE)))) {
    return(merge_cfg(
      config@selector@config,
      config@config
    ))
  }
  if (is.list(config)) {
    return(config)
  }

  list()
}

#' Resolve one config setting
#'
#' @param config Config input.
#' @param key Setting name.
#' @param sections Optional nested sections searched after the top level.
#'
#' @return The resolved value or `NULL`.
#' @keywords internal
config_value <- function(config,
                           key,
                           sections = character()) {
  cfg <- config_data(config)
  if (!is.list(cfg)) {
    return(NULL)
  }

  if (!is.null(cfg[[key]])) {
    return(cfg[[key]])
  }

  for (section in sections) {
    if (is.list(cfg[[section]]) && !is.null(cfg[[section]][[key]])) {
      return(cfg[[section]][[key]])
    }
  }

  NULL
}

#' Emit one progress message
#'
#' @param progress Logical scalar.
#' @param ... Message fragments.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
report_progress <- function(progress,
                              ...) {
  if (!isTRUE(progress)) {
    return(invisible(NULL))
  }

  tsb_message(...)
  invisible(NULL)
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
  # separators so downstream code sees one stable path form.
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
  # Accept the common command-line truthy spellings so the script wrapper can
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
  if ("policy" %in% names(tbl)) {
    return(canonical_policy_name(as.character(tbl$policy)))
  }
  rep(NA_character_, nrow(tbl))
}

#' Standardize policy branch filters from a table
#'
#' @param data Data frame or tibble.
#' @param branch_col Branch-filter column name.
#'
#' @return Character vector.
#' @keywords internal
standardize_branches <- function(data,
                                 branch_col = "equation_branch_filter") {
  if (branch_col %in% names(data)) {
    branch_values <- as.character(data[[branch_col]])
    branch_values[is.na(branch_values) | !nzchar(branch_values)] <- "all"
    return(normalize_policy_equation_branch_filters(branch_values))
  }
  rep("all", nrow(data))
}

#' Standardize one policy-identity table
#'
#' @param data Data frame or tibble.
#' @param policy_col Policy column name.
#' @param branch_col Branch-filter column name.
#'
#' @return Tibble with normalized policy-identity columns.
#' @keywords internal
standardize_policies <- function(data,
                                 policy_col = "policy",
                                 branch_col = "equation_branch_filter") {
  out <- if (is.data.frame(data)) data else tibble::as_tibble(data)
  if (policy_col %in% names(out)) {
    out[[policy_col]] <- as.character(out[[policy_col]])
  }
  out[[branch_col]] <- standardize_branches(out, branch_col = branch_col)
  out
}

#' Resolve selected-policy branch filters from a table
#'
#' @param data Data frame or tibble.
#'
#' @return Character vector.
#' @keywords internal
selected_branches <- function(data) {
  out <- tibble::as_tibble(data)

  if ("selected_equation_branch_filter" %in% names(out)) {
    selected_vals <- as.character(out$selected_equation_branch_filter)
    if ("equation_branch_filter" %in% names(out)) {
      branch_vals <- as.character(out$equation_branch_filter)
      selected_vals[is.na(selected_vals) | !nzchar(selected_vals)] <- branch_vals[is.na(selected_vals) | !nzchar(selected_vals)]
    }
    selected_vals[is.na(selected_vals) | !nzchar(selected_vals)] <- "all"
    return(normalize_policy_equation_branch_filters(selected_vals))
  }
  if ("equation_branch_filter" %in% names(out)) {
    return(standardize_branches(out, branch_col = "equation_branch_filter"))
  }

  rep("all", nrow(out))
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

#' Resolve displayed policy names from a table
#'
#' @param tbl Data frame or tibble.
#'
#' @return Character vector.
#' @keywords internal
resolve_policy_display_names <- function(tbl) {
  out <- tibble::as_tibble(tbl)
  if ("policy_display" %in% names(out)) {
    display_vals <- as.character(out$policy_display)
    if (all(!is.na(display_vals) & nzchar(display_vals))) {
      return(display_vals)
    }
  }
  branch_vals <- standardize_branches(out)

  if (all(c("candidate_pool", "aggregation_method") %in% names(out))) {
    branch_defs <- read_policy_registry()$policy_branches %||% list()
    branch_tags <- stats::setNames(
      vapply(branch_defs, function(x) as.character(x$display_tag %||% x$key %||% NA_character_), character(1)),
      vapply(branch_defs, function(x) as.character(x$key %||% NA_character_), character(1))
    )
    pool_vals <- dplyr::recode(
      as.character(out$candidate_pool),
      all_admissible = "All-models",
      closest_study_cell = "Study-cell",
      generalized_models_only = "Generalized",
      nearest_phylogenetic = "Phylogenetic",
      phylogenetic_neighborhood = "Related-species",
      same_derivation = "Derivation",
      same_equation_form = "Equation-form",
      same_family = "Family",
      same_fao_area = "FAO-area",
      same_genus = "Genus",
      same_ocean_basin = "Ocean-basin",
      same_order = "Order",
      same_species = "Species",
      .default = stringr::str_to_title(stringr::str_replace_all(as.character(out$candidate_pool), "_", " "))
    )
    agg_vals <- dplyr::recode(
      as.character(out$aggregation_method),
      arithmetic_mean = "averaged",
      equal_weight_mean = "averaged",
      nearest = "nearest",
      nearest_by_combined_distance = "nearest",
      nearest_by_trait_gower_distance = "trait-nearest",
      nearest_study_then_model = "study-nearest",
      .default = stringr::str_to_lower(stringr::str_replace_all(as.character(out$aggregation_method), "_", "-"))
    )
    branch_labs <- unname(branch_tags[branch_vals])
    branch_labs[is.na(branch_labs) | !nzchar(branch_labs)] <- branch_vals[is.na(branch_labs) | !nzchar(branch_labs)]
    alias_vals <- paste0(pool_vals, " ", agg_vals, " [", branch_labs, "]")
    if (all(!is.na(alias_vals) & nzchar(alias_vals))) {
      return(alias_vals)
    }
  }

  if ("policy" %in% names(out)) {
    return(policy_display_label(
      canonical_policy_name(as.character(out$policy)),
      branch_vals
    ))
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
  branch_vals <- if ("selected_equation_branch_filter" %in% names(out)) {
    standardize_branches(out, branch_col = "selected_equation_branch_filter")
  } else {
    standardize_branches(out)
  }
  branch_defs <- read_policy_registry()$policy_branches %||% list()
  branch_tags <- stats::setNames(
    vapply(branch_defs, function(x) as.character(x$display_tag %||% x$key %||% NA_character_), character(1)),
    vapply(branch_defs, function(x) as.character(x$key %||% NA_character_), character(1))
  )
  branch_labs <- unname(branch_tags[branch_vals])
  branch_labs[is.na(branch_labs) | !nzchar(branch_labs)] <- branch_vals[is.na(branch_labs) | !nzchar(branch_labs)]
  if ("selected_policy_display" %in% names(out)) {
    display_vals <- as.character(out$selected_policy_display)
    if (all(!is.na(display_vals) & nzchar(display_vals))) {
      needs_branch <- !grepl("\\s*\\[[^]]+\\]$", display_vals)
      if (any(needs_branch, na.rm = TRUE)) {
        display_vals[needs_branch] <- paste0(display_vals[needs_branch], " [", branch_labs[needs_branch], "]")
      }
      return(display_vals)
    }
  }
  if (all(c("candidate_pool", "aggregation_method") %in% names(out))) {
    pool_vals <- dplyr::recode(
      as.character(out$candidate_pool),
      all_admissible = "All-models",
      closest_study_cell = "Study-cell",
      generalized_models_only = "Generalized",
      nearest_phylogenetic = "Phylogenetic",
      phylogenetic_neighborhood = "Related-species",
      same_derivation = "Derivation",
      same_equation_form = "Equation-form",
      same_family = "Family",
      same_fao_area = "FAO-area",
      same_genus = "Genus",
      same_ocean_basin = "Ocean-basin",
      same_order = "Order",
      same_species = "Species",
      .default = stringr::str_to_title(stringr::str_replace_all(as.character(out$candidate_pool), "_", " "))
    )
    agg_vals <- dplyr::recode(
      as.character(out$aggregation_method),
      arithmetic_mean = "averaged",
      equal_weight_mean = "averaged",
      nearest = "nearest",
      nearest_by_combined_distance = "nearest",
      nearest_by_trait_gower_distance = "trait-nearest",
      nearest_study_then_model = "study-nearest",
      .default = stringr::str_to_lower(stringr::str_replace_all(as.character(out$aggregation_method), "_", "-"))
    )
    branch_labs <- unname(branch_tags[branch_vals])
    branch_labs[is.na(branch_labs) | !nzchar(branch_labs)] <- branch_vals[is.na(branch_labs) | !nzchar(branch_labs)]
    alias_vals <- paste0(pool_vals, " ", agg_vals, " [", branch_labs, "]")
    if (all(!is.na(alias_vals) & nzchar(alias_vals))) {
      return(alias_vals)
    }
  }

  if ("selected_policy" %in% names(out)) {
    return(policy_display_label(canonical_policy_name(as.character(out$selected_policy)), branch_vals))
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

  # Fork-based clusters on Unix share the parent process memory via
  # copy-on-write — no serialization, no package loading, near-zero startup.
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

  cluster_obj <- parallel::makePSOCKcluster(workers)
  attr(cluster_obj, "cluster_type") <- "psock"
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
          suppressPackageStartupMessages(library(graphics))
          suppressPackageStartupMessages(library(stats))
          suppressPackageStartupMessages(library(methods))
          # PSOCK workers on Windows must load the live source tree when the
          # parent session is running from devtools/pkgload. Otherwise workers
          # silently fall back to an older installed copy that does not contain
          # the current learner registry, which then breaks parallel cross-fit
          # for newly added super-learner methods.
          if (is.character(package_dir) &&
            length(package_dir) == 1 &&
            nzchar(package_dir) &&
            file.exists(file.path(package_dir, "DESCRIPTION")) &&
            requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(
              path = package_dir,
              quiet = TRUE,
              helpers = FALSE,
              attach_testthat = FALSE,
              export_all = TRUE
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


#' @keywords internal
tsb_cluster_export <- function(cl, varlist, envir = parent.frame()) {
  if (is.null(cl) || identical(attr(cl, "cluster_type"), "fork")) return(invisible(NULL))
  parallel::clusterExport(cl, varlist, envir = envir)
}


