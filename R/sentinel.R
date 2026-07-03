#' Sentinel S7 Class
#'
#' `Sentinel` orchestrates outer-loop validation runs around a caller-supplied
#' workflow function. It owns the row-level candidate table, scenario grid,
#' split specification, and the disk-backed manifest used to resume or collect
#' fold outputs.
#'
#' The object is intentionally orchestration-focused. It does not assume a
#' specific downstream scientific workflow beyond the workflow function
#' contract accepted by [run_sentinel()].
#'
#' @name Sentinel-class
#' @usage NULL
#' @aliases Sentinel
NULL

Sentinel <- S7::new_class(
  "Sentinel",
  properties = list(
    data = S7::new_property(CandidatesDataFrame),
    config = S7::new_property(S7::class_list),
    workflow_fn = S7::new_property(S7::class_any),
    split_mode = S7::new_property(S7::class_character),
    split_col = S7::new_property(S7::class_character),
    scenario_grid = S7::new_property(S7::class_list),
    manifest = S7::new_property(CandidatesDataFrame),
    results = S7::new_property(CandidatesDataFrame),
    output_dir = S7::new_property(S7::class_character),
    case_studies = S7::new_property(S7::class_character),
    options = S7::new_property(S7::class_list)
  ),
  validator = function(self) {
    tryCatch(
      {
        if (!is.data.frame(self@data)) {
          return("`data` must be a data frame.")
        }
        if (!is.list(self@config)) {
          return("`config` must be a list.")
        }
        if (!is.null(self@workflow_fn) && !is.function(self@workflow_fn)) {
          return("`workflow_fn` must be NULL or a function.")
        }
        if (!is.character(self@split_mode) || length(self@split_mode) != 1L || !nzchar(self@split_mode[[1]])) {
          return("`split_mode` must be one non-empty string.")
        }
        if (!is.character(self@split_col) || length(self@split_col) != 1L || !nzchar(self@split_col[[1]])) {
          return("`split_col` must be one non-empty string.")
        }
        if (!self@split_col[[1]] %in% names(self@data)) {
          return(sprintf("`split_col` '%s' was not found in `data`.", self@split_col[[1]]))
        }
        if (!is.list(self@scenario_grid) || is.null(names(self@scenario_grid)) || any(!nzchar(names(self@scenario_grid)))) {
          return("`scenario_grid` must be a named list.")
        }
        if (!is.data.frame(self@manifest)) {
          return("`manifest` must be a data frame.")
        }
        if (!is.data.frame(self@results)) {
          return("`results` must be a data frame.")
        }
        if (!is.character(self@output_dir) || length(self@output_dir) != 1L || !nzchar(self@output_dir[[1]])) {
          return("`output_dir` must be one non-empty path.")
        }
        if (!is.character(self@case_studies)) {
          return("`case_studies` must be a character vector.")
        }
        if (!is.list(self@options)) {
          return("`options` must be a list.")
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(Sentinel)

#' Rebuild a `Sentinel`
#'
#' @param object A [Sentinel] object.
#' @param data Optional replacement candidate-model table.
#' @param config Optional replacement config list.
#' @param workflow_fn Optional replacement workflow function.
#' @param split_mode Optional replacement split mode.
#' @param split_col Optional replacement split column.
#' @param scenario_grid Optional replacement scenario grid.
#' @param manifest Optional replacement manifest table.
#' @param results Optional replacement results index table.
#' @param output_dir Optional replacement output directory.
#' @param case_studies Optional replacement case-study vector.
#' @param options Optional replacement options list.
#'
#' @return A rebuilt [Sentinel] object.
#'
#' @keywords internal
#' @noRd
sentinel_rebuild <- function(object,
                             data = object@data,
                             config = object@config,
                             workflow_fn = object@workflow_fn,
                             split_mode = object@split_mode,
                             split_col = object@split_col,
                             scenario_grid = object@scenario_grid,
                             manifest = object@manifest,
                             results = object@results,
                             output_dir = object@output_dir,
                             case_studies = object@case_studies,
                             options = object@options) {
  # Rehydrate the S7 object from normalized table/list components.
  Sentinel(
    data = tibble::as_tibble(data),
    config = config,
    workflow_fn = workflow_fn,
    split_mode = split_mode,
    split_col = split_col,
    scenario_grid = scenario_grid,
    manifest = tibble::as_tibble(manifest),
    results = tibble::as_tibble(results),
    output_dir = output_dir,
    case_studies = as.character(case_studies %||% character(0)),
    options = options
  )
}

#' Resolve Sentinel runtime options
#'
#' @param options Optional user-supplied options list.
#'
#' @return A merged options list.
#'
#' @keywords internal
#' @noRd
sentinel_default_options <- function(options = NULL) {
  # Start from one stable default option bundle for all Sentinel entry points.
  defaults <- list(
    seed = 1L,
    workers = 1L,
    outer_repeats = 1L,
    throttle_inner_workers = TRUE,
    fast_validation = FALSE,
    fast_nmds_args = list(try = 3, trymax = 6),
    progress = FALSE,
    manifest_file = "sentinel_manifest.csv",
    results_index_file = "sentinel_results_index.csv",
    summary_dir = "summaries",
    artifact_dir = "artifacts",
    cache_root_dir = "fold_cache",
    cache_refresh = FALSE,
    save_case_artifacts = TRUE
  )
  if (is.null(options)) {
    return(defaults)
  }
  if (!is.list(options)) {
    stop("'options' must be NULL or a list.", call. = FALSE)
  }
  merge_config_sections(defaults, options)
}

#' Convert a Sentinel label to a portable file/name stem
#'
#' @param x Character-like label.
#'
#' @return A non-empty character scalar.
#'
#' @keywords internal
#' @noRd
sentinel_safe_stem <- function(x) {
  out <- stringr::str_squish(as.character(x %||% "unknown")[[1]])
  out <- gsub("[^[:alnum:]_]+", "_", out)
  out <- gsub("^_+|_+$", "", out)
  if (nzchar(out)) out else "unknown"
}

#' Resolve a Sentinel data source to a candidate-model table
#'
#' @param data Candidate-model table, [Candidates], or [PolicySelector].
#'
#' @return A tibble of candidate-model rows.
#'
#' @keywords internal
#' @noRd
sentinel_resolve_data <- function(data) {
  # Pull the shared candidate-model table regardless of the staged wrapper.
  if (is_s7_instance(data, "PolicySelector")) {
    return(tibble::as_tibble(data@candidates@candidate_models))
  }
  if (is_s7_instance(data, "Candidates")) {
    return(tibble::as_tibble(data@candidate_models))
  }
  if (is.data.frame(data)) {
    return(tibble::as_tibble(data))
  }

  stop(
    "'data' must be a data frame, `Candidates`, or `PolicySelector` object.",
    call. = FALSE
  )
}

#' Resolve workflow config for a Sentinel run
#'
#' @param data Original Sentinel data input.
#' @param config Optional explicit config override.
#'
#' @return A config list.
#'
#' @keywords internal
#' @noRd
sentinel_resolve_config <- function(data,
                                    config = NULL) {
  # Prefer the explicit config override, then fall back to staged-object config.
  if (!is.null(config)) {
    cfg <- resolve_config_data(config)
    return(if (is.list(cfg) && !is.data.frame(cfg)) cfg else list())
  }

  if (is_s7_instance(data, "PolicySelector")) {
    return(data@config %||% list())
  }
  if (is_s7_instance(data, "Candidates")) {
    return(candidates_configuration(data) %||% list())
  }

  list()
}

#' Normalize one Sentinel split mode
#'
#' @param split_mode Candidate split-mode value.
#'
#' @return One validated split-mode string.
#'
#' @keywords internal
#' @noRd
sentinel_resolve_split_mode <- function(split_mode) {
  # Restrict split modes to the supported outer-validation estimands.
  match.arg(
    as.character(split_mode %||% "anchor_row_holdout"),
    c(
      "anchor_row_holdout",
      "study_holdout",
      "study_cell_holdout",
      "species_holdout",
      "group_holdout"
    )
  )
}

#' Guess the split column for one Sentinel run
#'
#' @param data Candidate-model table.
#' @param split_mode Validated Sentinel split mode.
#' @param split_col Optional explicit split-column override.
#'
#' @return One column name.
#'
#' @keywords internal
#' @noRd
sentinel_guess_split_col <- function(data,
                                     split_mode,
                                     split_col = NULL) {
  # Honor an explicit split column before attempting heuristic inference.
  if (!is.null(split_col)) {
    split_col <- as.character(split_col)[[1]]
    if (!split_col %in% names(data)) {
      stop(sprintf("Requested split column '%s' was not found in `data`.", split_col), call. = FALSE)
    }
    return(split_col)
  }

  candidates <- switch(split_mode,
    anchor_row_holdout = c("model_id", "model_id_chr", "anchor_model_id"),
    study_holdout = c("citation", "study_reference_id", "reference_tsl_short", "study_id"),
    study_cell_holdout = c("study_cell_id", "study_cell"),
    species_holdout = c("species_name", "anchor_species"),
    group_holdout = character(0)
  )
  candidate_matches <- candidates[candidates %in% names(data)]
  split_col_now <- if (length(candidate_matches) > 0L) candidate_matches[[1]] else NULL
  if (is.null(split_col_now)) {
    stop(
      sprintf(
        "Could not infer a split column for split_mode '%s'. Supply `split_col` explicitly.",
        split_mode
      ),
      call. = FALSE
    )
  }
  split_col_now
}

#' Normalize one Sentinel scenario specification
#'
#' @param spec Scenario specification list or `NULL`.
#'
#' @return A normalized scenario list.
#'
#' @keywords internal
#' @noRd
sentinel_normalize_scenario_spec <- function(spec) {
  # Canonicalize list-valued scenario fields once at construction time.
  if (is.null(spec)) {
    return(list())
  }
  if (!is.list(spec)) {
    stop("Each Sentinel scenario specification must be a list or NULL.", call. = FALSE)
  }
  if (!is.null(spec$drop_columns)) {
    spec$drop_columns <- unique(as.character(spec$drop_columns))
  }
  if (!is.null(spec$drop_rows)) {
    if (!is.list(spec$drop_rows) || is.null(names(spec$drop_rows)) || any(!nzchar(names(spec$drop_rows)))) {
      stop("Sentinel scenario field `drop_rows` must be a named list.", call. = FALSE)
    }
    spec$drop_rows <- lapply(spec$drop_rows, function(x) unique(as.character(x)))
  }
  spec
}

#' Resolve a Sentinel deployment target and split mode
#'
#' @param deployment_target Optional deployment-target label understood by the
#'   internal Sentinel target specification.
#' @param split_mode Optional explicit split mode override.
#'
#' @return A list with `deployment_target` and `split_mode`.
#'
#' @keywords internal
#' @noRd
sentinel_resolve_target <- function(deployment_target = NULL,
                                    split_mode = NULL) {
  # When the caller names a deployment target, treat that as the estimand and
  # derive the split mode from it unless they explicitly override the split.
  if (!is.null(deployment_target)) {
    target_spec <- sentinel_target_spec(deployment_target)
    split_mode_now <- sentinel_resolve_split_mode(
      split_mode %||% target_spec$split_mode
    )
    return(list(
      deployment_target = target_spec$deployment_target,
      split_mode = split_mode_now
    ))
  }

  list(
    deployment_target = "custom",
    split_mode = sentinel_resolve_split_mode(split_mode)
  )
}

#' Build a named Sentinel scenario grid
#'
#' @param trait_ablations Optional character vector for automatic one-at-a-time
#'   ablations, or a named list of character vectors for grouped ablations.
#'   Each element becomes one scenario with `drop_columns` set to that vector.
#' @param model_ablations Optional named list of named lists. Each element
#'   becomes one scenario with `drop_rows` set to the supplied candidate-model
#'   column-value filters, so matching models are removed from the fold-local
#'   train/test slices before the workflow runs.
#' @param schema_scenarios Optional named list of additional scenario specs.
#' @param baseline_label Baseline scenario name.
#'
#' @return Named list.
#'
#' @examples
#' scenarios <- build_sentinel_scenarios(
#'   trait_ablations = list(
#'     no_taxonomy = c("family", "genus")
#'   ),
#'   model_ablations = list(
#'     no_fixed_slope = list(equation_form = "fixed_slope"),
#'     no_policy_a = list(policy = "policy_a")
#'   )
#' )
#' names(scenarios)
#' scenarios$no_policy_a$drop_rows
#'
#' @export
build_sentinel_scenarios <- function(trait_ablations = NULL,
                                     model_ablations = NULL,
                                     schema_scenarios = NULL,
                                     baseline_label = "baseline") {
  # Seed every grid with a baseline scenario so comparisons stay aligned.
  if (!is.character(baseline_label) || length(baseline_label) != 1L || !nzchar(baseline_label)) {
    stop("'baseline_label' must be one non-empty string.", call. = FALSE)
  }

  out <- list()
  out[[baseline_label]] <- list(
    scenario_type = "baseline",
    ablated_traits = character(0)
  )

  if (!is.null(trait_ablations)) {
    # A plain character vector is the common leave-one-trait-out case. Give it
    # stable, filesystem-safe scenario names while retaining named lists for
    # grouped ablations (for example, taxonomy = c("family", "genus")).
    if (is.character(trait_ablations)) {
      trait_ablations <- stats::setNames(
        lapply(unique(trait_ablations), function(x) x),
        paste0("without_", vapply(unique(trait_ablations), sentinel_safe_stem, character(1)))
      )
    }
    if (!is.list(trait_ablations) || is.null(names(trait_ablations)) || any(!nzchar(names(trait_ablations)))) {
      stop("'trait_ablations' must be a named list of character vectors.", call. = FALSE)
    }
    for (nm in names(trait_ablations)) {
      traits_now <- unique(as.character(trait_ablations[[nm]]))
      out[[nm]] <- list(
        drop_columns = traits_now,
        scenario_type = "trait_ablation",
        ablated_traits = traits_now
      )
    }
  }

  # Materialize every model-ablation scenario into the canonical
  # drop_rows field used by the fold-preparation pipeline.
  if (!is.null(model_ablations)) {
    if (!is.list(model_ablations) || is.null(names(model_ablations)) || any(!nzchar(names(model_ablations)))) {
      stop("'model_ablations' must be a named list.", call. = FALSE)
    }
    for (nm in names(model_ablations)) {
      out[[nm]] <- sentinel_normalize_scenario_spec(
        list(
          drop_rows = model_ablations[[nm]],
          scenario_type = "model_ablation"
        )
      )
    }
  }

  if (!is.null(schema_scenarios)) {
    if (!is.list(schema_scenarios) || is.null(names(schema_scenarios)) || any(!nzchar(names(schema_scenarios)))) {
      stop("'schema_scenarios' must be a named list.", call. = FALSE)
    }
    for (nm in names(schema_scenarios)) {
      out[[nm]] <- sentinel_normalize_scenario_spec(schema_scenarios[[nm]])
    }
  }

  out
}

#' Resolve every configured Sentinel trait available in the data
#'
#' @param config Workflow configuration.
#' @param available_columns Candidate-model column names.
#'
#' @return Character vector of configured species and study traits.
#'
#' @keywords internal
#' @noRd
sentinel_configured_traits <- function(config,
                                       available_columns = NULL) {
  config <- resolve_config_data(config)
  if (!is.list(config)) {
    return(character(0))
  }
  trait_names_from <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }
    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      return(names(x)[nzchar(names(x))])
    }
    as.character(unlist(x, use.names = FALSE))
  }
  sections <- c("similarity", "alchemist", "policy", "admissibility")
  blocks <- c(list(config), lapply(sections, function(section) config[[section]] %||% list()))
  traits <- unique(unlist(lapply(blocks, function(block) {
    if (!is.list(block)) {
      return(character(0))
    }
    c(
      trait_names_from(block$species_traits %||% NULL),
      trait_names_from(block$study_traits %||% NULL)
    )
  }), use.names = FALSE))
  traits <- traits[!is.na(traits) & nzchar(traits)]
  if (is.null(available_columns)) traits else intersect(traits, available_columns)
}

#' Resolve the Sentinel output root
#'
#' @param config Workflow config list.
#' @param output_dir Optional explicit output directory.
#'
#' @return An absolute output path.
#'
#' @keywords internal
#' @noRd
sentinel_resolve_output_dir <- function(config,
                                        output_dir = NULL) {
  # Prefer an explicit directory, then fall back to the workflow path section.
  if (!is.null(output_dir)) {
    return(path_absolute(output_dir, must_work = FALSE))
  }

  path_now <- resolve_config_value(config, "out_root", sections = "paths") %||%
    resolve_config_value(config, "output_root", sections = "paths") %||%
    "sentinel_output"
  path_absolute(file.path(path_now, "sentinel"), must_work = FALSE)
}

#' Resolve the standard Sentinel output paths
#'
#' @param object A [Sentinel] object.
#'
#' @return A named list of manifest, results, summary, and artifact paths.
#'
#' @keywords internal
#' @noRd
sentinel_output_paths <- function(object) {
  # Centralize all standard output paths so the file layout is easy to audit.
  list(
    manifest_file = file.path(
      object@output_dir,
      object@options$manifest_file %||% "sentinel_manifest.csv"
    ),
    results_index_file = file.path(
      object@output_dir,
      object@options$results_index_file %||% "sentinel_results_index.csv"
    ),
    summary_dir = file.path(
      object@output_dir,
      object@options$summary_dir %||% "summaries"
    ),
    artifact_dir = file.path(
      object@output_dir,
      object@options$artifact_dir %||% "artifacts"
    ),
    cache_root_dir = file.path(
      object@output_dir,
      object@options$cache_root_dir %||% "fold_cache"
    )
  )
}

#' Write one Sentinel table to disk
#'
#' @param tbl Table to write.
#' @param path Output CSV path.
#'
#' @return Invisibly returns `path`.
#'
#' @keywords internal
#' @noRd
sentinel_write_table <- function(tbl,
                                 path) {
  # Create parent directories first so fold writes can be resumed safely.
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(tbl), path, row.names = FALSE, na = "")
  invisible(path)
}

#' Read one Sentinel CSV table from disk
#'
#' @param path Input CSV path.
#'
#' @return A tibble, or an empty tibble when the file is absent.
#'
#' @keywords internal
#' @noRd
sentinel_read_table <- function(path) {
  # Missing files are treated as empty tables so resume/collect stay robust.
  if (!file.exists(path) || isTRUE(file.info(path)$size == 0L)) {
    return(tibble::tibble())
  }
  first_lines <- readLines(path, n = 5L, warn = FALSE)
  if (length(first_lines) == 0L || all(trimws(first_lines) %in% c("", '""'))) {
    return(tibble::tibble())
  }
  tryCatch(
    tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)),
    error = function(e) {
      stop(sprintf("Could not read Sentinel table '%s': %s", path, conditionMessage(e)), call. = FALSE)
    }
  )
}

#' Precompute one Sentinel split plan
#'
#' @param data Candidate-model table.
#' @param split_col Active split column.
#' @param split_mode Active split mode.
#' @param options Sentinel options list.
#'
#' @return A list with holdout IDs, row counts, and fold row indices.
#'
#' @keywords internal
#' @noRd
sentinel_split_plan <- function(data,
                                split_col,
                                split_mode = "anchor_row_holdout",
                                options = list()) {
  data <- tibble::as_tibble(data)
  split_mode <- sentinel_resolve_split_mode(split_mode)
  all_rows <- seq_len(nrow(data))

  if (identical(split_mode, "species_holdout")) {
    # Precompute both the test rows and the retained training rows once so
    # cold-start folds do not rescan every species-role column on each run.
    test_col <- sentinel_species_holdout_test_col(
      data = data,
      split_col = split_col,
      test_col = options$species_holdout_test_col %||% NULL
    )
    purge_cols <- sentinel_species_holdout_purge_cols(
      data = data,
      split_col = split_col,
      purge_cols = options$species_holdout_train_purge_cols %||% NULL
    )
    holdout_ids <- sort(unique(stats::na.omit(as.character(data[[test_col]]))))
    test_vals <- as.character(data[[test_col]])
    purge_vals <- lapply(purge_cols, function(col_now) as.character(data[[col_now]]))

    test_indices <- vector("list", length(holdout_ids))
    train_indices <- vector("list", length(holdout_ids))
    names(test_indices) <- holdout_ids
    names(train_indices) <- holdout_ids

    for (holdout_id in holdout_ids) {
      test_idx <- which(test_vals %in% holdout_id)
      train_drop_mask <- Reduce(
        `|`,
        lapply(purge_vals, function(col_vals) col_vals %in% holdout_id)
      )
      test_indices[[holdout_id]] <- test_idx
      train_indices[[holdout_id]] <- all_rows[!train_drop_mask]
    }

    return(list(
      holdout_ids = holdout_ids,
      holdout_n = stats::setNames(vapply(test_indices, length, integer(1)), holdout_ids),
      test_indices = test_indices,
      train_indices = train_indices
    ))
  }

  # For row, study, study-cell, and explicit group holdouts, cache the row
  # membership for each block once and derive training rows by complement.
  split_vals <- as.character(data[[split_col]])
  split_groups <- split(all_rows, split_vals)
  split_groups <- split_groups[!is.na(names(split_groups)) & nzchar(names(split_groups))]
  holdout_ids <- sort(names(split_groups))
  test_indices <- split_groups[holdout_ids]

  list(
    holdout_ids = holdout_ids,
    holdout_n = stats::setNames(vapply(test_indices, length, integer(1)), holdout_ids),
    test_indices = test_indices,
    train_indices = NULL
  )
}

#' Resolve the species-holdout test column
#'
#' @param data Candidate-model table.
#' @param split_col Active split column.
#' @param test_col Optional explicit test-column override.
#'
#' @return One column name.
#'
#' @keywords internal
#' @noRd
sentinel_species_holdout_test_col <- function(data,
                                              split_col,
                                              test_col = NULL) {
  # Use the explicit test column when supplied, otherwise infer a species field.
  if (!is.null(test_col)) {
    test_col <- as.character(test_col)[[1]]
    if (!test_col %in% names(data)) {
      stop(sprintf("Requested Sentinel species-holdout test column '%s' was not found.", test_col), call. = FALSE)
    }
    return(test_col)
  }

  candidates <- unique(c(
    split_col,
    "anchor_species",
    "species_name",
    "reference_anchor_species"
  ))
  matches <- candidates[candidates %in% names(data)]
  if (length(matches) == 0L) {
    stop(
      "Could not infer a Sentinel species-holdout test column. Supply `options$species_holdout_test_col`.",
      call. = FALSE
    )
  }
  matches[[1]]
}

#' Resolve the species-holdout training purge columns
#'
#' @param data Candidate-model table.
#' @param split_col Active split column.
#' @param purge_cols Optional explicit purge-column override.
#'
#' @return A character vector of purge-column names.
#'
#' @keywords internal
#' @noRd
sentinel_species_holdout_purge_cols <- function(data,
                                                split_col,
                                                purge_cols = NULL) {
  # Purge every configured species role so the cold-start split is strict.
  if (!is.null(purge_cols)) {
    purge_cols <- unique(as.character(purge_cols))
    missing_cols <- setdiff(purge_cols, names(data))
    if (length(missing_cols) > 0L) {
      stop(
        sprintf(
          "Requested Sentinel species-holdout purge column(s) were not found: %s",
          paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(purge_cols)
  }

  unique(c(
    split_col,
    intersect(
      c(
        "species_name",
        "anchor_species",
        "donor_species",
        "receiving_species",
        "reference_anchor_species"
      ),
      names(data)
    )
  ))
}

#' Count the held-out test rows for one Sentinel block
#'
#' @param data Candidate-model table.
#' @param split_mode Active split mode.
#' @param split_col Active split column.
#' @param holdout_id Holdout block identifier.
#' @param options Sentinel options list.
#' @param split_plan Optional precomputed split plan.
#'
#' @return Integer-like row count.
#'
#' @keywords internal
#' @noRd
sentinel_holdout_test_n <- function(data,
                                    split_mode,
                                    split_col,
                                    holdout_id,
                                    options = list(),
                                    split_plan = NULL) {
  # Species holdout uses its dedicated test column; other modes use split_col.
  holdout_id <- as.character(holdout_id)
  if (!is.null(split_plan)) {
    return(as.integer(split_plan$holdout_n[[holdout_id]] %||% 0L))
  }
  if (identical(split_mode, "species_holdout")) {
    test_col <- sentinel_species_holdout_test_col(
      data = data,
      split_col = split_col,
      test_col = options$species_holdout_test_col %||% NULL
    )
    return(sum(as.character(data[[test_col]]) %in% holdout_id, na.rm = TRUE))
  }
  sum(as.character(data[[split_col]]) %in% holdout_id, na.rm = TRUE)
}

#' Partition one Sentinel fold into train and test slices
#'
#' @param data Candidate-model table.
#' @param split_col Active split column.
#' @param holdout_id Holdout block identifier.
#' @param split_mode Active split mode.
#' @param options Sentinel options list.
#' @param split_plan Optional precomputed split plan.
#'
#' @return A list with `train_data` and `test_data`.
#'
#' @keywords internal
#' @noRd
sentinel_partition_data <- function(data,
                                    split_col,
                                    holdout_id,
                                    split_mode = "anchor_row_holdout",
                                    options = list(),
                                    split_plan = NULL) {
  # Build train/test slices according to the active outer-validation target.
  holdout_id <- as.character(holdout_id)
  split_mode <- sentinel_resolve_split_mode(split_mode)

  if (!is.null(split_plan)) {
    test_idx <- split_plan$test_indices[[holdout_id]] %||% integer(0)
    train_idx <- split_plan$train_indices[[holdout_id]] %||% NULL

    if (identical(split_mode, "species_holdout")) {
      return(list(
        train_data = tibble::as_tibble(data[train_idx, , drop = FALSE]),
        test_data = tibble::as_tibble(data[test_idx, , drop = FALSE])
      ))
    }

    return(list(
      train_data = tibble::as_tibble(data[-test_idx, , drop = FALSE]),
      test_data = tibble::as_tibble(data[test_idx, , drop = FALSE])
    ))
  }

  if (identical(split_mode, "species_holdout")) {
    test_col <- sentinel_species_holdout_test_col(
      data = data,
      split_col = split_col,
      test_col = options$species_holdout_test_col %||% NULL
    )
    purge_cols <- sentinel_species_holdout_purge_cols(
      data = data,
      split_col = split_col,
      purge_cols = options$species_holdout_train_purge_cols %||% NULL
    )

    test_mask <- as.character(data[[test_col]]) %in% holdout_id
    train_drop_mask <- Reduce(
      `|`,
      lapply(purge_cols, function(col_now) {
        as.character(data[[col_now]]) %in% holdout_id
      })
    )

    return(list(
      train_data = tibble::as_tibble(data[!train_drop_mask, , drop = FALSE]),
      test_data = tibble::as_tibble(data[test_mask, , drop = FALSE])
    ))
  }

  split_vals <- as.character(data[[split_col]])
  test_mask <- split_vals %in% holdout_id

  list(
    train_data = tibble::as_tibble(data[!test_mask, , drop = FALSE]),
    test_data = tibble::as_tibble(data[test_mask, , drop = FALSE])
  )
}

#' Drop scenario-defined columns from a Sentinel fold
#'
#' @param train_data Training candidate-model rows.
#' @param test_data Held-out candidate-model rows.
#' @param drop_columns Character vector of columns to remove.
#'
#' @return A list with filtered `train_data` and `test_data`.
#'
#' @keywords internal
#' @noRd
sentinel_apply_drop_columns <- function(train_data,
                                        test_data,
                                        drop_columns = NULL) {
  # Apply simple column ablations before any custom scenario hook runs.
  drop_columns <- unique(as.character(drop_columns %||% character(0)))
  if (length(drop_columns) == 0L) {
    return(list(train_data = train_data, test_data = test_data))
  }

  keep_train <- setdiff(names(train_data), drop_columns)
  keep_test <- setdiff(names(test_data), drop_columns)
  list(
    train_data = tibble::as_tibble(train_data[, keep_train, drop = FALSE]),
    test_data = tibble::as_tibble(test_data[, keep_test, drop = FALSE])
  )
}

#' Drop scenario-defined row subsets from a Sentinel fold
#'
#' @param train_data Training candidate-model rows.
#' @param test_data Held-out candidate-model rows.
#' @param drop_rows Named list mapping column names to values that should be
#'   excluded from both train and test slices.
#'
#' @return A list with filtered `train_data` and `test_data`.
#'
#' @keywords internal
#' @noRd
sentinel_apply_drop_rows <- function(train_data,
                                     test_data,
                                     drop_rows = NULL) {
  drop_rows <- drop_rows %||% list()
  if (length(drop_rows) == 0L) {
    return(list(train_data = train_data, test_data = test_data))
  }
  if (!is.list(drop_rows) || is.null(names(drop_rows)) || any(!nzchar(names(drop_rows)))) {
    stop("'drop_rows' must be a named list.", call. = FALSE)
  }

  # Build one exclusion mask per table so action-space stress can remove whole
  # policy families or proxy classes without custom workflow code.
  filter_one <- function(tbl) {
    tbl <- tibble::as_tibble(tbl)
    if (nrow(tbl) == 0L) {
      return(tbl)
    }
    drop_mask <- rep(FALSE, nrow(tbl))
    for (col_now in names(drop_rows)) {
      if (!col_now %in% names(tbl)) {
        next
      }
      drop_vals <- unique(as.character(drop_rows[[col_now]] %||% character(0)))
      if (length(drop_vals) == 0L) {
        next
      }
      drop_mask <- drop_mask | as.character(tbl[[col_now]]) %in% drop_vals
    }
    tbl[!drop_mask, , drop = FALSE]
  }

  list(
    train_data = filter_one(train_data),
    test_data = filter_one(test_data)
  )
}

#' Patch fold-local config implied by one Sentinel split
#'
#' @param config Workflow config list.
#' @param split_mode Active Sentinel split mode.
#' @param manifest_row Optional one-row manifest tibble.
#' @param object Optional [Sentinel] object.
#'
#' @return A config list patched for the current outer-validation target.
#'
#' @keywords internal
#' @noRd
sentinel_patch_fold_config <- function(config,
                                       split_mode = "anchor_row_holdout",
                                       manifest_row = NULL,
                                       object = NULL) {
  cfg_now <- resolve_config_data(config)
  if (!is.list(cfg_now)) {
    return(cfg_now)
  }

  # Apply the strict Stage 1 OOF mode implied by the outer estimand unless
  # the caller already pinned one explicitly in the workflow config.
  oof_mode_now <- sentinel_alchemist_oof_mode(
    config = cfg_now,
    split_mode = split_mode
  )
  cfg_now$alchemist <- cfg_now$alchemist %||% list()
  cfg_now$alchemist$learner <- cfg_now$alchemist$learner %||% list()
  cfg_now$alchemist$learner$oof_mode <- oof_mode_now

  # Route any standard package caches into a fold-local directory so expensive
  # Stage 1 and benchmark artifacts can be reused on Sentinel reruns.
  if (!is.null(manifest_row) && !is.null(object) && "cache_dir" %in% names(manifest_row)) {
    cache_dir_now <- as.character(manifest_row$cache_dir[[1]] %||% "")
    if (nzchar(cache_dir_now)) {
      dir.create(cache_dir_now, recursive = TRUE, showWarnings = FALSE)
      cfg_now$paths <- cfg_now$paths %||% list()
      cfg_now$paths$cache_dir <- cache_dir_now
      cfg_now$cache <- cfg_now$cache %||% list()
      cfg_now$cache$folder <- cache_dir_now
      if (is.null(cfg_now$cache$refresh)) {
        cfg_now$cache$refresh <- isTRUE(object@options$cache_refresh %||% FALSE)
      }
      cfg_now <- apply_cache_defaults(cfg_now)
    }
  }

  # Avoid nested parallel oversubscription when Sentinel is already running
  # multiple outer folds concurrently.
  options_now <- if (!is.null(object) && inherits(object, "S7_object")) {
    object@options %||% list()
  } else {
    list()
  }

  if (isTRUE(options_now$throttle_inner_workers %||% FALSE) &&
    isTRUE((options_now$workers %||% 1L) > 1L)) {
    cfg_now$benchmark <- cfg_now$benchmark %||% list()
    cfg_now$benchmark$workers <- 1L

    cfg_now$metalearner <- cfg_now$metalearner %||% list()
    cfg_now$metalearner$workers <- 1L

    cfg_now$simulation <- cfg_now$simulation %||% list()
    cfg_now$simulation$workers <- 1L

    cfg_now$alchemist <- cfg_now$alchemist %||% list()
    cfg_now$alchemist$learner <- cfg_now$alchemist$learner %||% list()
    cfg_now$alchemist$learner$workers <- 1L
    cfg_now$alchemist$distill_workers <- 1L
  }

  # Allow a fold-level fast-validation ordination budget so expensive outer
  # validation runs can use a smaller NMDS restart schedule.
  if (isTRUE(options_now$fast_validation %||% FALSE)) {
    cfg_now$benchmark <- cfg_now$benchmark %||% list()
    cfg_now$benchmark$include_ts_error <- FALSE

    cfg_now$ordination <- cfg_now$ordination %||% list()
    cfg_now$ordination$nmds_args <- merge_config_sections(
      cfg_now$ordination$nmds_args %||% list(),
      options_now$fast_nmds_args %||% list(try = 3, trymax = 6)
    )
  }

  cfg_now
}

#' Prune dropped or unavailable trait fields from a Sentinel fold config
#'
#' @param config Workflow config list.
#' @param drop_columns Optional character vector of columns explicitly removed
#'   by the current scenario.
#' @param available_columns Optional character vector of columns that are
#'   actually present in the current train/test slice.
#'
#' @return A config list with unavailable trait references removed.
#'
#' @keywords internal
#' @noRd
sentinel_prune_trait_config <- function(config,
                                        drop_columns = NULL,
                                        available_columns = NULL) {
  # Normalize the exclusion set once so both explicit drops and absent columns
  # are removed from every trait-bearing config section.
  drop_columns <- unique(as.character(drop_columns %||% character(0)))
  available_columns <- unique(as.character(available_columns %||% character(0)))
  if (!is.list(config)) {
    return(config)
  }

  prune_keys <- c(
    "species_traits", "study_traits",
    "admissibility_species_traits", "admissibility_study_traits",
    "feature_cols", "uncertainty_feature_cols",
    "metadata_traits", "drop_columns"
  )
  section_names <- c(
    "similarity", "alchemist", "admissibility", "policy",
    "benchmark", "metalearner", "policy_learner", "selection", "candidates"
  )

  # Compute the full set of fields that cannot be used in this fold.
  blocked_columns <- drop_columns
  if (length(available_columns) > 0L) {
    trait_refs <- character(0)

    # Read trait references from top-level aliases first.
    for (key in prune_keys) {
      value <- config[[key]] %||% NULL
      if (is.character(value)) {
        trait_refs <- c(trait_refs, value)
      } else if (is.atomic(value) && !is.null(names(value))) {
        trait_refs <- c(trait_refs, names(value))
      } else if (is.list(value) && !is.null(names(value))) {
        trait_refs <- c(trait_refs, names(value))
      }
    }

    # Read trait references from nested workflow sections next.
    for (section in section_names) {
      section_value <- config[[section]] %||% NULL
      if (!is.list(section_value)) {
        next
      }
      for (key in prune_keys) {
        value <- section_value[[key]] %||% NULL
        if (is.character(value)) {
          trait_refs <- c(trait_refs, value)
        } else if (is.atomic(value) && !is.null(names(value))) {
          trait_refs <- c(trait_refs, names(value))
        } else if (is.list(value) && !is.null(names(value))) {
          trait_refs <- c(trait_refs, names(value))
        }
      }
    }

    trait_refs <- unique(as.character(trait_refs[!is.na(trait_refs) & nzchar(trait_refs)]))
    blocked_columns <- unique(c(blocked_columns, setdiff(trait_refs, available_columns)))
  }
  if (length(blocked_columns) == 0L) {
    return(config)
  }

  # Remove blocked field names while preserving either vectors or named lists.
  prune_one <- function(x) {
    if (is.null(x)) {
      return(x)
    }
    if (is.character(x)) {
      return(setdiff(x, blocked_columns))
    }
    if (is.atomic(x) && !is.null(names(x))) {
      keep_names <- setdiff(names(x), blocked_columns)
      return(x[keep_names])
    }
    if (is.list(x) && !is.null(names(x))) {
      keep_names <- setdiff(names(x), blocked_columns)
      return(x[keep_names])
    }
    x
  }

  out <- config

  # Prune top-level aliases that older workflow paths still inspect directly.
  for (key in prune_keys) {
    if (!is.null(out[[key]])) {
      out[[key]] <- prune_one(out[[key]])
    }
  }

  # Prune nested workflow sections used by the staged objects.
  for (section in section_names) {
    if (!is.list(out[[section]])) {
      next
    }
    for (key in prune_keys) {
      if (!is.null(out[[section]][[key]])) {
        out[[section]][[key]] <- prune_one(out[[section]][[key]])
      }
    }
  }

  out
}

#' Apply one Sentinel scenario to a train/test fold
#'
#' @param train_data Training candidate-model rows.
#' @param test_data Held-out candidate-model rows.
#' @param config Workflow config list.
#' @param scenario_spec Normalized scenario specification.
#' @param manifest_row One-row manifest tibble for the active fold.
#' @param object A [Sentinel] object.
#'
#' @return A list with `train_data`, `test_data`, and `config`.
sentinel_apply_scenario <- function(train_data,
                                    test_data,
                                    config,
                                    scenario_spec,
                                    manifest_row,
                                    object) {
  # Apply config patches, column drops, row drops, and optional user hooks.
  scenario_spec <- sentinel_normalize_scenario_spec(scenario_spec)
  config_now <- merge_config_sections(config, scenario_spec$config_patch %||% list())
  config_now <- sentinel_patch_fold_config(
    config = config_now,
    split_mode = object@split_mode,
    manifest_row = manifest_row,
    object = object
  )

  dropped <- sentinel_apply_drop_columns(
    train_data = train_data,
    test_data = test_data,
    drop_columns = scenario_spec$drop_columns %||% NULL
  )
  train_now <- dropped$train_data
  test_now <- dropped$test_data
  dropped_rows <- sentinel_apply_drop_rows(
    train_data = train_now,
    test_data = test_now,
    drop_rows = scenario_spec$drop_rows %||% NULL
  )
  train_now <- dropped_rows$train_data
  test_now <- dropped_rows$test_data
  config_now <- sentinel_prune_trait_config(
    config = config_now,
    drop_columns = scenario_spec$drop_columns %||% NULL,
    available_columns = union(names(train_now), names(test_now))
  )

  prepare_fn <- scenario_spec$prepare %||% NULL
  if (!is.null(prepare_fn)) {
    if (!is.function(prepare_fn)) {
      stop("Sentinel scenario field `prepare` must be a function.", call. = FALSE)
    }

    # Pass only the arguments the user hook actually accepts so scenario
    # preparation remains flexible instead of requiring one exact signature.
    prepare_args <- list(
      train_data = train_now,
      test_data = test_now,
      config = config_now,
      manifest_row = manifest_row,
      object = object,
      sentinel = object,
      scenario_spec = scenario_spec,
      scenario = scenario_spec
    )
    prepare_formals <- names(formals(prepare_fn) %||% list())
    if (!("..." %in% prepare_formals)) {
      prepare_args <- prepare_args[intersect(names(prepare_args), prepare_formals)]
    }
    prepared <- do.call(prepare_fn, prepare_args)

    if (!is.list(prepared) || is.null(prepared$train_data) || is.null(prepared$test_data)) {
      stop(
        "Sentinel scenario `prepare` must return a list with `train_data` and `test_data`.",
        call. = FALSE
      )
    }
    train_now <- tibble::as_tibble(prepared$train_data)
    test_now <- tibble::as_tibble(prepared$test_data)
    config_now <- prepared$config %||% config_now
  }

  list(
    train_data = train_now,
    test_data = test_now,
    config = config_now
  )
}

#' Resolve the Stage 1 OOF mode for a Sentinel deployment target
#'
#' @param config Workflow config list or staged config object.
#' @param split_mode Sentinel split mode.
#'
#' @return A single OOF mode string.
#'
#' @keywords internal
#' @noRd
sentinel_alchemist_oof_mode <- function(config,
                                        split_mode = "anchor_row_holdout") {
  # Respect explicit workflow configuration before applying Sentinel defaults.
  cfg <- resolve_config_data(config)
  explicit_mode <- ((((cfg$alchemist) %||% list())$learner %||% list())$oof_mode %||% NULL)
  if (!is.null(explicit_mode) && nzchar(as.character(explicit_mode)[[1]])) {
    return(as.character(explicit_mode)[[1]])
  }
  if (identical(as.character(split_mode %||% ""), "species_holdout")) {
    return("species_purged")
  }
  "anchor_species"
}

#' Build a fold-local Candidates object for Sentinel
#'
#' @param candidate_models Candidate-model table for the current training fold.
#' @param reference_anchors Held-out rows exposed to the workflow only as
#'   reference anchors.
#' @param config Optional workflow config.
#' @param anchor_selector Optional anchor selector override.
#'
#' @return A [Candidates] object.
#'
#' @keywords internal
#' @noRd
sentinel_build_candidates <- function(candidate_models,
                                      reference_anchors = NULL,
                                      config = NULL,
                                      anchor_selector = NULL) {
  # Rebuild the smallest valid Candidates object for one outer fold. Held-out
  # rows are reference anchors, never candidate models, so package-native
  # workflows can predict them without leaking them into training.
  candidate_models <- tibble::as_tibble(candidate_models)
  reference_anchors <- tibble::as_tibble(
    reference_anchors %||% candidate_models[0, , drop = FALSE]
  )
  metadata_models <- dplyr::bind_rows(candidate_models, reference_anchors)
  cfg <- resolve_config_data(config)
  # Rebuild only the minimal staged Candidates spec needed inside the fold.
  spec <- list(
    config_data = cfg,
    study = list(),
    sources = list(),
    enrich = list(),
    prepare = list(),
    anchors = ((((cfg$candidates) %||% list())$anchors) %||% ((cfg$anchors) %||% list()))
  )
  # Build the compact study metadata table expected by Candidates.
  study_keep <- intersect(
    c("species_name", "regional_body", "citation", "study_cell_id"),
    names(metadata_models)
  )
  study_db <- if (length(study_keep) == 0L) {
    tibble::tibble()
  } else {
    metadata_models |>
      dplyr::select(dplyr::all_of(study_keep)) |>
      dplyr::distinct()
  }
  # Build the compact species metadata table expected by Candidates.
  species_keep <- intersect(
    c("species_name", "family", "family_name", "genus", "species"),
    names(metadata_models)
  )
  species_db <- if (length(species_keep) == 0L) {
    tibble::tibble()
  } else {
    metadata_models |>
      dplyr::select(dplyr::all_of(species_keep)) |>
      dplyr::distinct()
  }
  species_vector <- if ("species_name" %in% names(candidate_models)) {
    sort(unique(stats::na.omit(as.character(candidate_models$species_name))))
  } else {
    character(0)
  }
  candidates <- Candidates(
    spec = spec,
    study_db = study_db,
    species_vector = species_vector,
    source_dbs = list(),
    species_db = species_db,
    candidate_models = candidate_models,
    reference_anchors = reference_anchors,
    similarity_matrix = list(),
    gower_distances = list(),
    ordination = list(),
    admissibility = list(),
    similarity_tuning = list()
  )

  # Recover the configured anchor selector from either supported config layout.
  anchor_selector <- anchor_selector %||%
    ((((cfg$candidates) %||% list())$anchors) %||% list())$selector %||%
    (((cfg$anchors) %||% list())$selector %||% NULL)
  if (nrow(reference_anchors) > 0L) {
    return(candidates)
  }
  if (!is.null(anchor_selector)) {
    # Allow split-specific folds to recover later when the configured selector
    # has no matches in the current training slice.
    candidates <- set_reference_anchors(
      candidates,
      selector = anchor_selector,
      require_selection = FALSE
    )
  } else if ("model_id" %in% names(candidate_models) || "model_id_chr" %in% names(candidate_models)) {
    id_col <- if ("model_id" %in% names(candidate_models)) "model_id" else "model_id_chr"
    candidates <- set_reference_anchors(
      candidates,
      model_ids = as.character(candidate_models[[id_col]]),
      model_id_col = id_col
    )
  }

  candidates
}

#' Compute log-interval widths for Sentinel prediction rows
#'
#' @param tbl Prediction table with multiplier bounds or conformal widths.
#'
#' @return Numeric width vector on the log scale.
#'
#' @keywords internal
#' @noRd
sentinel_interval_log_width <- function(tbl) {
  # Prefer explicit multiplier bounds, then fall back to log-radius columns.
  tbl <- tibble::as_tibble(tbl)
  n <- nrow(tbl)
  if (n == 0L) {
    return(numeric(0))
  }

  lower <- if ("multiplier_lo" %in% names(tbl)) suppressWarnings(as.numeric(tbl$multiplier_lo)) else rep(NA_real_, n)
  upper <- if ("multiplier_hi" %in% names(tbl)) suppressWarnings(as.numeric(tbl$multiplier_hi)) else rep(NA_real_, n)
  width <- rep(NA_real_, n)
  ok <- is.finite(lower) & is.finite(upper) & lower > 0 & upper > 0
  width[ok] <- abs(log(upper[ok]) - log(lower[ok]))

  q_cols <- c("meta_q_abs_log_total", "meta_q_abs_log", "q_abs_log_total", "q_abs_log")
  for (nm in q_cols) {
    if (!nm %in% names(tbl)) {
      next
    }
    qv <- suppressWarnings(as.numeric(tbl[[nm]]))
    replace <- !is.finite(width) & is.finite(qv)
    width[replace] <- 2 * qv[replace]
  }

  width
}

#' Derive anchor-level evaluation metrics from Sentinel predictions
#'
#' @param selected_tbl Selected-policy prediction table.
#' @param intervals_tbl Optional full candidate-policy interval table.
#'
#' @return A tibble with error, width, coverage, and regret fields.
#'
#' @keywords internal
#' @noRd
sentinel_selected_metrics <- function(selected_tbl,
                                      intervals_tbl = NULL) {
  # Standardize row-level diagnostics so workflows can return a common result shape.
  selected_tbl <- tibble::as_tibble(selected_tbl)
  intervals_tbl <- tibble::as_tibble(intervals_tbl %||% tibble::tibble())
  if (nrow(selected_tbl) == 0L) {
    return(tibble::tibble())
  }

  if (!"valid_prediction" %in% names(selected_tbl)) {
    selected_tbl$valid_prediction <- is.finite(suppressWarnings(as.numeric(selected_tbl$multiplier_pred))) &
      suppressWarnings(as.numeric(selected_tbl$multiplier_pred)) > 0
  }
  # Compute absolute log-error against the unit multiplier directly on the
  # selected predictions.
  multiplier_pred <- suppressWarnings(as.numeric(selected_tbl$multiplier_pred))
  selected_tbl$error_abs_log <- rep(NA_real_, length(multiplier_pred))
  ok_mult <- is.finite(multiplier_pred) & multiplier_pred > 0
  selected_tbl$error_abs_log[ok_mult] <- abs(log(multiplier_pred[ok_mult]))
  selected_tbl$interval_log_width <- sentinel_interval_log_width(selected_tbl)
  selected_tbl$covered <- if ("q_abs_log" %in% names(selected_tbl) || "meta_q_abs_log" %in% names(selected_tbl) ||
    "meta_q_abs_log_total" %in% names(selected_tbl) || "q_abs_log_total" %in% names(selected_tbl) ||
    all(c("multiplier_lo", "multiplier_hi") %in% names(selected_tbl))) {
    is.finite(selected_tbl$error_abs_log) &
      is.finite(selected_tbl$interval_log_width) &
      selected_tbl$error_abs_log <= (selected_tbl$interval_log_width / 2)
  } else {
    NA
  }

  if (nrow(intervals_tbl) > 0L && all(c("anchor_model_id", "multiplier_pred") %in% names(intervals_tbl))) {
    oracle_tbl <- intervals_tbl |>
      dplyr::mutate(
        # Compute oracle loss directly from the predicted multiplier.
        error_abs_log = dplyr::if_else(
          is.finite(suppressWarnings(as.numeric(.data$multiplier_pred))) &
            suppressWarnings(as.numeric(.data$multiplier_pred)) > 0,
          abs(log(suppressWarnings(as.numeric(.data$multiplier_pred)))),
          NA_real_
        ),
        valid_prediction = dplyr::coalesce(.data$valid_prediction, is.finite(.data$error_abs_log))
      ) |>
      dplyr::filter(.data$valid_prediction, is.finite(.data$error_abs_log)) |>
      dplyr::group_by(.data$anchor_model_id) |>
      dplyr::arrange(.data$error_abs_log, .data$policy, .by_group = TRUE) |>
      dplyr::summarise(
        oracle_abs_log_error = dplyr::first(.data$error_abs_log),
        oracle_policy = dplyr::first(.data$policy),
        n_candidate_policies = dplyr::n(),
        .groups = "drop"
      )
    selected_tbl <- selected_tbl |>
      dplyr::left_join(oracle_tbl, by = "anchor_model_id") |>
      dplyr::mutate(
        selection_regret_abs_log = .data$error_abs_log - .data$oracle_abs_log_error
      )
  } else {
    selected_tbl$oracle_abs_log_error <- NA_real_
    selected_tbl$oracle_policy <- NA_character_
    selected_tbl$n_candidate_policies <- NA_integer_
    selected_tbl$selection_regret_abs_log <- NA_real_
  }

  selected_tbl
}

#' Resolve the fold-specific Sentinel output files
#'
#' @param object A [Sentinel] object.
#' @param fold_id Fold identifier.
#' @param scenario Scenario label.
#' @param holdout_id Holdout block identifier.
#'
#' @return A list with `summary_file`, `artifact_file`, and `cache_dir`.
#'
#' @keywords internal
#' @noRd
sentinel_manifest_row_files <- function(object,
                                        fold_id,
                                        scenario,
                                        holdout_id) {
  # Encode the fold/scenario/holdout triple directly into the file names.
  paths_now <- sentinel_output_paths(object)
  fold_label <- sprintf("%04d", as.integer(fold_id))
  # Collapse whitespace and punctuation so fold file names remain portable.
  scenario_label <- stringr::str_squish(as.character(scenario %||% "unknown"))
  scenario_label <- gsub("[^[:alnum:]_]+", "_", scenario_label)
  scenario_label <- gsub("^_+|_+$", "", scenario_label)
  if (!nzchar(scenario_label)) {
    scenario_label <- "unknown"
  }
  holdout_label <- stringr::str_squish(as.character(holdout_id %||% "unknown"))
  holdout_label <- gsub("[^[:alnum:]_]+", "_", holdout_label)
  holdout_label <- gsub("^_+|_+$", "", holdout_label)
  if (!nzchar(holdout_label)) {
    holdout_label <- "unknown"
  }

  list(
    summary_file = file.path(
      paths_now$summary_dir,
      paste0(fold_label, "_", scenario_label, "_", holdout_label, ".csv")
    ),
    artifact_file = file.path(
      paths_now$artifact_dir,
      paste0(fold_label, "_", scenario_label, "_", holdout_label, ".rds")
    ),
    cache_dir = file.path(
      paths_now$cache_root_dir,
      paste0(fold_label, "_", scenario_label, "_", holdout_label)
    )
  )
}

#' Resolve one named Sentinel deployment target
#'
#' @param deployment_target One supported deployment-target label.
#'
#' @return A list describing the implied split mode and study description.
#'
#' @keywords internal
#' @noRd
sentinel_target_spec <- function(deployment_target) {
  # Map user-facing deployment targets onto explicit outer-validation splits.
  target <- match.arg(
    as.character(deployment_target %||% "seen_species_new_row"),
    c(
      "seen_species_new_row",
      "seen_species_new_study",
      "seen_species_new_study_cell",
      "cold_start_species"
    )
  )

  switch(target,
    seen_species_new_row = list(
      deployment_target = target,
      split_mode = "anchor_row_holdout",
      description = "Hold out one target row while keeping other rows from the same species in training."
    ),
    seen_species_new_study = list(
      deployment_target = target,
      split_mode = "study_holdout",
      description = "Hold out one study/reference block while keeping the represented species library otherwise intact."
    ),
    seen_species_new_study_cell = list(
      deployment_target = target,
      split_mode = "study_cell_holdout",
      description = "Hold out one study-cell block while keeping other rows from the same species and study universe in training."
    ),
    cold_start_species = list(
      deployment_target = target,
      split_mode = "species_holdout",
      description = "Hold out one species entirely so the full fold is trained without that species."
    )
  )
}

#' Build a `Sentinel`
#'
#' @param data Candidate-model table, [Candidates], or [PolicySelector].
#' @param workflow_fn User-supplied workflow function that owns the complete
#'   fold-local analysis. Its public signature is
#'   `function(candidates, workflow_config_s7)`: `candidates` contains only
#'   outer-training rows in `@candidate_models` and held-out rows in
#'   `@reference_anchors`; `workflow_config_s7` is the scenario-specific
#'   [Configurer]. The function may directly return a [Referee], [Scorecard],
#'   or a result list containing `metrics` or `selected`. It may be deferred
#'   and supplied to [run_sentinel()] instead.
#' @param config Optional config list or staged object carrying config.
#' @param deployment_target Optional deployment target. One of
#'   `"seen_species_new_row"`, `"seen_species_new_study"`,
#'   `"seen_species_new_study_cell"`, or `"cold_start_species"`. When
#'   supplied, Sentinel derives the default `split_mode` from this target.
#' @param split_mode Split mode. One of `"anchor_row_holdout"`,
#'   `"study_holdout"`, `"study_cell_holdout"`, `"species_holdout"`, or
#'   `"group_holdout"`.
#' @param split_col Optional explicit split column. Required for
#'   `"group_holdout"` and optional otherwise.
#' @param scenario_grid Optional named list of scenario specifications.
#' @param output_dir Optional output directory.
#' @param case_studies Optional character vector of holdout IDs whose artifacts
#'   should be persisted.
#' @param options Optional options list.
#' @param trait_ablations Optional character vector for automatic leave-one-
#'   trait-out scenarios, or a named list for grouped trait ablations. Set to
#'   `TRUE` to ablate every configured species and study trait present in
#'   `data`. Cannot be combined with `scenario_grid`.
#'
#' @return A `Sentinel` object.
#'
#' @export
build_sentinel <- function(data,
                           workflow_fn = NULL,
                           config = NULL,
                           deployment_target = NULL,
                           split_mode = NULL,
                           split_col = NULL,
                           scenario_grid = NULL,
                           output_dir = NULL,
                           case_studies = character(0),
                           options = NULL,
                           trait_ablations = NULL) {
  data_tbl <- sentinel_resolve_data(data)
  config_tbl <- sentinel_resolve_config(data = data, config = config)
  target_now <- sentinel_resolve_target(
    deployment_target = deployment_target,
    split_mode = split_mode
  )
  split_mode <- target_now$split_mode
  if (identical(split_mode, "group_holdout") && is.null(split_col)) {
    stop("`split_col` is required when `split_mode = 'group_holdout'`.", call. = FALSE)
  }
  split_col <- sentinel_guess_split_col(data_tbl, split_mode = split_mode, split_col = split_col)
  configured_ablation_traits <- character(0)
  missing_ablation_traits <- character(0)
  if (!is.null(scenario_grid) && !is.null(trait_ablations)) {
    stop("Supply either `scenario_grid` or `trait_ablations`, not both.", call. = FALSE)
  }
  if (is.logical(trait_ablations)) {
    if (length(trait_ablations) != 1L || is.na(trait_ablations) || !isTRUE(trait_ablations)) {
      stop("Logical `trait_ablations` must be TRUE.", call. = FALSE)
    }
    configured_ablation_traits <- sentinel_configured_traits(config = config_tbl)
    trait_ablations <- intersect(configured_ablation_traits, names(data_tbl))
    missing_ablation_traits <- setdiff(configured_ablation_traits, trait_ablations)
    if (length(trait_ablations) == 0L) {
      stop("No configured species or study traits were present in `data`.", call. = FALSE)
    }
  }
  scenario_grid <- scenario_grid %||% build_sentinel_scenarios(
    trait_ablations = trait_ablations
  )
  scenario_grid <- lapply(scenario_grid, sentinel_normalize_scenario_spec)
  output_dir <- sentinel_resolve_output_dir(config_tbl, output_dir = output_dir)
  options <- sentinel_default_options(options)
  configurer_input <- if (is_s7_instance(config, "Configurer")) {
    config
  } else if (is_s7_instance(data, "Candidates") &&
      is_s7_instance(data@spec$configurer %||% NULL, "Configurer")) {
    data@spec$configurer
  } else {
    NULL
  }
  options$config_base_dir <- if (!is.null(configurer_input)) configurer_input@base_dir else getwd()
  options$config_registry_path <- if (!is.null(configurer_input)) configurer_input@registry_path else NA_character_
  options$config_policy_path <- if (!is.null(configurer_input)) configurer_input@policy_path else NA_character_
  options$deployment_target <- target_now$deployment_target
  options$trait_ablation_traits <- unique(as.character(unlist(
    trait_ablations %||% character(0),
    use.names = FALSE
  )))
  options$trait_ablation_missing <- missing_ablation_traits

  # Construct the orchestration object once all inputs are normalized.
  object <- Sentinel(
    data = tibble::as_tibble(data_tbl),
    config = config_tbl,
    workflow_fn = workflow_fn,
    split_mode = split_mode,
    split_col = split_col,
    scenario_grid = scenario_grid,
    manifest = tibble::tibble(),
    results = tibble::tibble(),
    output_dir = output_dir,
    case_studies = as.character(case_studies %||% character(0)),
    options = options
  )

  # Materialize the standard output layout immediately for resumable runs.
  paths_now <- sentinel_output_paths(object)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths_now$summary_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths_now$artifact_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths_now$cache_root_dir, recursive = TRUE, showWarnings = FALSE)

  object
}


#' Build the Sentinel outer-loop manifest
#'
#' @param object A [Sentinel] object.
#' @param refresh Logical scalar. Rebuild even when a manifest already exists.
#'
#' @return An updated [Sentinel] object.
#'
#' @keywords internal
#' @noRd
build_sentinel_manifest <- function(object,
                                    refresh = FALSE) {
  # Reuse any existing manifest unless the caller explicitly refreshes it.
  if (!is_s7_instance(object, "Sentinel")) {
    stop("'object' must be a `Sentinel` object.", call. = FALSE)
  }

  if (!refresh && nrow(object@manifest) > 0L) {
    return(object)
  }

  split_plan <- sentinel_split_plan(
    data = object@data,
    split_col = object@split_col,
    split_mode = object@split_mode,
    options = object@options
  )
  block_values <- split_plan$holdout_ids
  if (length(block_values) == 0L) {
    stop("No non-missing holdout blocks were available for the Sentinel manifest.", call. = FALSE)
  }

  outer_repeats <- as.integer(object@options$outer_repeats %||% 1L)
  if (!is.finite(outer_repeats) || outer_repeats < 1L) {
    stop("`options$outer_repeats` must be one integer >= 1.", call. = FALSE)
  }
  manifest_rows <- vector(
    "list",
    length(block_values) * length(object@scenario_grid) * outer_repeats
  )
  k <- 0L
  scenario_names <- names(object@scenario_grid)
  for (repeat_id in seq_len(outer_repeats)) {
    for (scenario_nm in scenario_names) {
      spec_now <- object@scenario_grid[[scenario_nm]]
      for (block_idx in seq_along(block_values)) {
        holdout_id <- block_values[[block_idx]]
        k <- k + 1L
        outer_fold_id <- (repeat_id - 1L) * length(block_values) + block_idx
        files_now <- sentinel_manifest_row_files(
          object = object,
          fold_id = k,
          scenario = scenario_nm,
          holdout_id = holdout_id
        )
        manifest_rows[[k]] <- tibble::tibble(
          fold_id = k,
          outer_fold_id = outer_fold_id,
          repeat_id = repeat_id,
          deployment_target = as.character(object@options$deployment_target %||% "custom"),
          scenario = scenario_nm,
          scenario_type = as.character(spec_now$scenario_type %||% "custom"),
          ablated_traits = paste(as.character(spec_now$ablated_traits %||% spec_now$drop_columns %||% character(0)), collapse = "|"),
          split_mode = object@split_mode,
          split_col = object@split_col,
          holdout_id = as.character(holdout_id),
          holdout_n = sentinel_holdout_test_n(
            data = object@data,
            split_mode = object@split_mode,
            split_col = object@split_col,
            holdout_id = holdout_id,
            options = object@options,
            split_plan = split_plan
          ),
          drop_columns = paste(as.character(spec_now$drop_columns %||% character(0)), collapse = "|"),
          summary_file = files_now$summary_file,
          artifact_file = files_now$artifact_file,
          cache_dir = files_now$cache_dir,
          case_study = as.character(holdout_id) %in% object@case_studies,
          status = "pending",
          error_message = NA_character_,
          started_at = NA_character_,
          completed_at = NA_character_
        )
      }
    }
  }

  manifest_tbl <- dplyr::bind_rows(manifest_rows)
  sentinel_write_table(manifest_tbl, sentinel_output_paths(object)$manifest_file)
  sentinel_rebuild(object, manifest = manifest_tbl)
}

#' Resume a Sentinel run from disk
#'
#' @param object A [Sentinel] object.
#'
#' @return An updated [Sentinel] object.
#'
#' @keywords internal
#' @noRd
resume_sentinel <- function(object) {
  # Reload both the manifest and result index from the canonical disk paths.
  if (!is_s7_instance(object, "Sentinel")) {
    stop("'object' must be a `Sentinel` object.", call. = FALSE)
  }

  paths_now <- sentinel_output_paths(object)
  manifest_tbl <- sentinel_read_table(paths_now$manifest_file)
  results_tbl <- sentinel_read_table(paths_now$results_index_file)

  sentinel_rebuild(
    object,
    manifest = manifest_tbl,
    results = results_tbl
  )
}

#' Update one row of the Sentinel manifest table
#'
#' @param manifest_tbl Manifest tibble.
#' @param fold_id Fold identifier to update.
#' @param fields Named list of replacement values.
#'
#' @return Updated manifest tibble.
#'
#' @keywords internal
#' @noRd
sentinel_update_manifest_row <- function(manifest_tbl,
                                         fold_id,
                                         fields) {
  # Update manifest rows by fold ID so resumable state stays append-free.
  if (nrow(manifest_tbl) == 0L) {
    return(manifest_tbl)
  }
  row_idx <- which(as.integer(manifest_tbl$fold_id) == as.integer(fold_id))
  if (length(row_idx) != 1L) {
    return(manifest_tbl)
  }
  for (nm in names(fields)) {
    manifest_tbl[[nm]][row_idx] <- fields[[nm]]
  }
  manifest_tbl
}

#' Validate one Sentinel metrics table
#'
#' @param metrics Workflow-supplied metrics table.
#'
#' @return A tibble with no list-columns.
#'
#' @keywords internal
#' @noRd
sentinel_validate_metrics <- function(metrics) {
  # Keep persisted fold summaries rectangular and CSV-safe.
  if (!is.data.frame(metrics)) {
    stop("Sentinel workflow output `metrics` must be a data frame.", call. = FALSE)
  }
  bad_cols <- names(metrics)[vapply(metrics, is.list, logical(1))]
  if (length(bad_cols) > 0L) {
    stop(
      sprintf(
        "Sentinel workflow metrics cannot contain list-columns. Offending columns: %s",
        paste(bad_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  tibble::as_tibble(metrics)
}

#' Validate one Sentinel prediction table
#'
#' @param tbl Prediction table supplied by a user workflow.
#' @param field_name User-facing field label used in error messages.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
sentinel_validate_prediction_table <- function(tbl,
                                               field_name) {
  # Keep the prediction contract table-based so outer workflows can hand back
  # selected policies or full interval tables without custom S7 wrappers.
  if (!is.data.frame(tbl)) {
    stop(
      sprintf("Sentinel workflow output `%s` must be a data frame.", field_name),
      call. = FALSE
    )
  }
  out <- tibble::as_tibble(tbl)
  bad_cols <- names(out)[vapply(out, is.list, logical(1))]
  if (length(bad_cols) > 0L) {
    stop(
      sprintf(
        "Sentinel workflow table `%s` cannot contain list-columns. Offending columns: %s",
        field_name,
        paste(bad_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  out
}

#' Build the fold-local Configurer passed to a Sentinel workflow
#'
#' @param config Scenario-specific normalized config list.
#' @param object Parent [Sentinel] object.
#'
#' @return A [Configurer] object.
#'
#' @keywords internal
#' @noRd
sentinel_fold_configurer <- function(config,
                                     object) {
  registry_path <- object@options$config_registry_path %||% NA_character_
  policy_path <- object@options$config_policy_path %||% NA_character_
  Configurer(
    data = resolve_config_data(config),
    base_dir = as.character(object@options$config_base_dir %||% getwd())[[1]],
    registry_path = registry_path,
    policy_path = policy_path
  )
}

#' Test whether a Sentinel workflow uses the package-native object contract
#'
#' @param workflow_fn User workflow function.
#'
#' @return Logical scalar.
#'
#' @keywords internal
#' @noRd
sentinel_object_workflow <- function(workflow_fn) {
  formal_names <- names(formals(workflow_fn)) %||% character(0)
  any(c("candidates", "workflow_config_s7") %in% formal_names) ||
    (length(setdiff(formal_names, "...")) <= 2L &&
      !any(c("train_data", "test_data", "manifest_row", "sentinel") %in% formal_names))
}

#' Invoke a Sentinel workflow through its public or legacy callback contract
#'
#' @param workflow_fn User workflow function.
#' @param candidates Fold-local [Candidates] object.
#' @param workflow_config_s7 Fold-local [Configurer] object.
#' @param train_data,test_data Fold tables retained for legacy callbacks.
#' @param params Scenario metadata retained for legacy callbacks.
#' @param config Scenario config list retained for legacy callbacks.
#' @param manifest_row One-row manifest table.
#' @param sentinel Parent [Sentinel] object.
#'
#' @return Raw workflow result.
#'
#' @keywords internal
#' @noRd
sentinel_invoke_workflow <- function(workflow_fn,
                                     candidates,
                                     workflow_config_s7,
                                     train_data,
                                     test_data,
                                     params,
                                     config,
                                     manifest_row,
                                     sentinel) {
  if (sentinel_object_workflow(workflow_fn)) {
    return(workflow_fn(candidates, workflow_config_s7))
  }

  # Retain compatibility with the original expanded callback while the public
  # interface moves to package-native objects.
  workflow_fn(
    train_data = train_data,
    test_data = test_data,
    params = params,
    config = config,
    manifest_row = manifest_row,
    sentinel = sentinel
  )
}

#' Normalize one Sentinel workflow return object
#'
#' @param result Raw workflow return object.
#'
#' @return A normalized list with `metrics`, `selected`, `intervals`, and
#'   `artifacts`.
#'
#' @keywords internal
#' @noRd
sentinel_normalize_result <- function(result) {
  # Accept either an explicit metric table or a standard selected/intervals
  # bundle that Sentinel can summarize automatically. Package-native workflow
  # functions may directly return a Referee or Scorecard.
  workflow_artifact <- NULL
  if (is_s7_instance(result, "Referee")) {
    workflow_artifact <- result
    result <- result@scorecard
  }
  if (is_s7_instance(result, "Scorecard")) {
    scorecard <- result
    result <- list(
      selected = scorecard@selected,
      intervals = scorecard@intervals,
      artifacts = list(scorecard = scorecard, workflow = workflow_artifact)
    )
  }
  if (!is.list(result)) {
    stop(
      "Sentinel workflow must return a Referee, Scorecard, or result list.",
      call. = FALSE
    )
  }

  metrics_tbl <- NULL
  selected_tbl <- NULL
  intervals_tbl <- NULL

  if (!is.null(result$metrics)) {
    metrics_tbl <- sentinel_validate_metrics(result$metrics)
  }

  selected_raw <- result$selected %||% result$selected_tbl %||% NULL
  intervals_raw <- result$intervals %||% result$intervals_tbl %||% NULL
  if (!is.null(selected_raw)) {
    selected_tbl <- sentinel_validate_prediction_table(
      tbl = selected_raw,
      field_name = "selected"
    )
  }
  if (!is.null(intervals_raw)) {
    intervals_tbl <- sentinel_validate_prediction_table(
      tbl = intervals_raw,
      field_name = "intervals"
    )
  }

  # Derive the standard anchor-level evaluation table when the workflow hands
  # back selected predictions instead of pre-summarized metrics.
  if (is.null(metrics_tbl) && !is.null(selected_tbl)) {
    metrics_tbl <- sentinel_selected_metrics(
      selected_tbl = selected_tbl,
      intervals_tbl = intervals_tbl
    )
  }
  if (is.null(metrics_tbl)) {
    stop(
      paste(
        "Sentinel workflow function must return either `metrics`, or",
        "`selected` with an optional `intervals` table."
      ),
      call. = FALSE
    )
  }

  list(
    metrics = metrics_tbl,
    selected = selected_tbl,
    intervals = intervals_tbl,
    artifacts = result$artifacts %||% NULL
  )
}

#' Run one Sentinel fold
#'
#' @param object A [Sentinel] object.
#' @param manifest_row One-row manifest tibble.
#' @param workflow_fn User-supplied workflow function.
#' @param progress Logical scalar controlling progress messages.
#' @param split_plan Optional precomputed split plan.
#'
#' @return A list with normalized metrics and artifact-save status.
#'
#' @keywords internal
#' @noRd
sentinel_run_one_fold <- function(object,
                                  manifest_row,
                                  workflow_fn,
                                  progress = FALSE,
                                  split_plan = NULL) {
  # Partition the candidate table first, then apply the active scenario.
  scenario_nm <- as.character(manifest_row$scenario[[1]])
  scenario_spec <- object@scenario_grid[[scenario_nm]] %||% list()
  partitioned <- sentinel_partition_data(
    data = object@data,
    split_col = object@split_col,
    holdout_id = manifest_row$holdout_id[[1]],
    split_mode = object@split_mode,
    options = object@options,
    split_plan = split_plan
  )
  prepared <- sentinel_apply_scenario(
    train_data = partitioned$train_data,
    test_data = partitioned$test_data,
    config = object@config,
    scenario_spec = scenario_spec,
    manifest_row = manifest_row,
    object = object
  )

  report_progress(
    progress,
    "Sentinel fold ", manifest_row$fold_id[[1]],
    ": scenario=", scenario_nm,
    ", holdout=", manifest_row$holdout_id[[1]],
    ", train=", nrow(prepared$train_data),
    ", test=", nrow(prepared$test_data)
  )

  params <- merge_config_sections(
    scenario_spec,
    list(
      deployment_target = as.character(
        manifest_row$deployment_target[[1]] %||%
          object@options$deployment_target %||%
          "custom"
      ),
      split_mode = object@split_mode,
      split_col = object@split_col,
      holdout_id = as.character(manifest_row$holdout_id[[1]])
    )
  )
  uses_object_workflow <- sentinel_object_workflow(workflow_fn)
  fold_candidates <- if (uses_object_workflow) {
    sentinel_build_candidates(
      candidate_models = prepared$train_data,
      reference_anchors = prepared$test_data,
      config = prepared$config
    )
  } else {
    NULL
  }
  fold_configurer <- if (uses_object_workflow) {
    sentinel_fold_configurer(
      config = prepared$config,
      object = object
    )
  } else {
    NULL
  }
  result <- sentinel_invoke_workflow(
    workflow_fn = workflow_fn,
    candidates = fold_candidates,
    workflow_config_s7 = fold_configurer,
    train_data = prepared$train_data,
    test_data = prepared$test_data,
    params = params,
    config = prepared$config,
    manifest_row = manifest_row,
    sentinel = object
  )
  result <- sentinel_normalize_result(result)

  # Attach fold metadata to the normalized metric table before writing it.
  metrics_tbl <- sentinel_validate_metrics(result$metrics) |>
    dplyr::mutate(
      fold_id = as.integer(manifest_row$fold_id[[1]]),
      outer_fold_id = as.integer((manifest_row$outer_fold_id %||% manifest_row$fold_id)[[1]]),
      repeat_id = as.integer((manifest_row$repeat_id %||% 1L)[[1]]),
      deployment_target = as.character(
        manifest_row$deployment_target[[1]] %||%
          object@options$deployment_target %||%
          "custom"
      ),
      scenario = scenario_nm,
      scenario_type = as.character((manifest_row$scenario_type %||% scenario_spec$scenario_type %||% "custom")[[1]]),
      ablated_traits = as.character((manifest_row$ablated_traits %||% paste(scenario_spec$drop_columns %||% character(0), collapse = "|"))[[1]]),
      split_mode = as.character(manifest_row$split_mode[[1]]),
      split_col = as.character(manifest_row$split_col[[1]]),
      holdout_id = as.character(manifest_row$holdout_id[[1]]),
      .before = 1
    )

  sentinel_write_table(metrics_tbl, manifest_row$summary_file[[1]])

  # Persist one canonical case-study bundle so downstream inspection can rely
  # on stable field names even when the workflow also supplies custom artifacts.
  save_artifacts <- isTRUE(object@options$save_case_artifacts) &&
    isTRUE(manifest_row$case_study[[1]]) &&
    (!is.null(result$artifacts) || !is.null(result$selected) || !is.null(result$intervals))
  if (save_artifacts) {
    dir.create(dirname(manifest_row$artifact_file[[1]]), recursive = TRUE, showWarnings = FALSE)
    saveRDS(
      list(
        metrics = metrics_tbl,
        selected = result$selected %||% tibble::tibble(),
        intervals = result$intervals %||% tibble::tibble(),
        artifacts = result$artifacts %||% NULL
      ),
      manifest_row$artifact_file[[1]]
    )
  }

  list(
    metrics = metrics_tbl,
    artifacts_saved = save_artifacts
  )
}

#' Run Sentinel outer-loop validation
#'
#' @param object A [Sentinel] object.
#' @param workflow_fn Optional workflow override.
#' @param refresh Logical scalar. When `TRUE`, rerun all folds and reset
#'   manifest status.
#' @param max_folds Optional integer cap for this invocation.
#' @param progress Optional logical override.
#' @param workers Optional number of outer-fold workers. Defaults to
#'   `object@options$workers`.
#'
#' @return An updated [Sentinel] object.
#'
#' @export
run_sentinel <- function(object,
                         workflow_fn = NULL,
                         refresh = FALSE,
                         max_folds = NULL,
                         progress = NULL,
                         workers = NULL) {
  # Resolve the workflow function and ensure the manifest exists before running.
  if (!is_s7_instance(object, "Sentinel")) {
    stop("'object' must be a `Sentinel` object.", call. = FALSE)
  }

  workflow_fn <- workflow_fn %||% object@workflow_fn
  if (is.null(workflow_fn) || !is.function(workflow_fn)) {
    stop("`workflow_fn` must be supplied either on the object or at call time.", call. = FALSE)
  }

  progress <- isTRUE(progress %||% object@options$progress %||% FALSE)
  workers <- as.integer(workers %||% object@options$workers %||% 1L)
  if (!is.finite(workers) || workers < 1L) {
    stop("'workers' must be one integer >= 1.", call. = FALSE)
  }
  runtime_options <- object@options
  runtime_options$workers <- workers
  object <- sentinel_rebuild(
    object,
    workflow_fn = workflow_fn,
    options = runtime_options
  )
  object <- if (nrow(object@manifest) == 0L) build_sentinel_manifest(object) else object
  if (refresh) {
    # Refresh resets manifest state and drops the prior in-memory result index.
    manifest_tbl <- object@manifest |>
      dplyr::mutate(
        status = "pending",
        error_message = NA_character_,
        started_at = NA_character_,
        completed_at = NA_character_
      )
    object <- sentinel_rebuild(object, manifest = manifest_tbl, results = tibble::tibble())
    sentinel_write_table(object@manifest, sentinel_output_paths(object)$manifest_file)
  }

  manifest_tbl <- object@manifest
  split_plan <- sentinel_split_plan(
    data = object@data,
    split_col = object@split_col,
    split_mode = object@split_mode,
    options = object@options
  )
  pending_rows <- which(as.character(manifest_tbl$status) != "completed")
  if (!is.null(max_folds)) {
    max_folds <- as.integer(max_folds)
    pending_rows <- head(pending_rows, max_folds)
  }

  results_rows <- if (nrow(object@results) > 0L) {
    split(object@results, seq_len(nrow(object@results)))
  } else {
    list()
  }

  if (length(pending_rows) == 0L) {
    sentinel_write_table(object@results, sentinel_output_paths(object)$results_index_file)
    return(sentinel_rebuild(object, manifest = manifest_tbl, results = object@results))
  }

  # Mark the selected rows as running in one manifest write so resumable state
  # stays accurate without rewriting the full file before every fold.
  started_at_now <- as.character(Sys.time())
  manifest_tbl$status[pending_rows] <- "running"
  manifest_tbl$started_at[pending_rows] <- started_at_now
  manifest_tbl$error_message[pending_rows] <- NA_character_
  sentinel_write_table(manifest_tbl, sentinel_output_paths(object)$manifest_file)

  task_rows <- lapply(pending_rows, function(row_idx) {
    tibble::as_tibble(manifest_tbl[row_idx, , drop = FALSE])
  })
  worker_object <- sentinel_rebuild(
    object,
    workflow_fn = workflow_fn,
    manifest = tibble::tibble(),
    results = tibble::tibble()
  )

  # Keep fold execution isolated from manifest updates so the same code path
  # can run sequentially or through outer-fold workers.
  run_task <- function(row_now) {
    fold_id_now <- as.integer(row_now$fold_id[[1]])
    tryCatch(
      list(
        ok = TRUE,
        fold_id = fold_id_now,
        row = row_now,
        fold_result = sentinel_run_one_fold(
          object = worker_object,
          manifest_row = row_now,
          workflow_fn = workflow_fn,
          progress = progress,
          split_plan = split_plan
        )
      ),
      error = function(e) {
        list(
          ok = FALSE,
          fold_id = fold_id_now,
          row = row_now,
          error_message = conditionMessage(e)
        )
      }
    )
  }

  fold_outputs <- if (workers > 1L && length(task_rows) > 1L) {
    report_progress(
      progress,
      "Running Sentinel outer folds in parallel with ",
      min(workers, length(task_rows)),
      " workers."
    )
    cluster_obj <- initialize_parallel_cluster(
      workers = min(workers, length(task_rows)),
      package_dir = object@options$package_dir %||% NULL
    )
    on.exit(parallel::stopCluster(cluster_obj), add = TRUE)
    tsb_cluster_export(
      cluster_obj,
      c("worker_object", "workflow_fn", "progress", "split_plan"),
      envir = environment()
    )
    parallel::parLapplyLB(cluster_obj, task_rows, run_task)
  } else {
    lapply(task_rows, run_task)
  }

  for (task_out in fold_outputs) {
    row_now <- tibble::as_tibble(task_out$row)
    fold_id_now <- as.integer(task_out$fold_id)

    if (!isTRUE(task_out$ok)) {
      manifest_tbl <- sentinel_update_manifest_row(
        manifest_tbl,
        fold_id = fold_id_now,
        fields = list(
          status = "failed",
          error_message = task_out$error_message %||% "unknown error",
          completed_at = as.character(Sys.time())
        )
      )
      next
    }

    fold_result <- task_out$fold_result
    row_now$outer_fold_id <- row_now$outer_fold_id %||% row_now$fold_id
    row_now$repeat_id <- row_now$repeat_id %||% 1L
    row_now$scenario_type <- row_now$scenario_type %||% "custom"
    row_now$ablated_traits <- row_now$ablated_traits %||% ""
    results_rows[[length(results_rows) + 1L]] <- row_now |>
      dplyr::transmute(
        fold_id = as.integer(.data$fold_id),
        outer_fold_id = as.integer(.data$outer_fold_id),
        repeat_id = as.integer(.data$repeat_id),
        scenario = as.character(.data$scenario),
        scenario_type = as.character(.data$scenario_type),
        ablated_traits = as.character(.data$ablated_traits),
        holdout_id = as.character(.data$holdout_id),
        summary_file = as.character(.data$summary_file),
        artifact_file = as.character(.data$artifact_file),
        artifacts_saved = isTRUE(fold_result$artifacts_saved)
      )

    manifest_tbl <- sentinel_update_manifest_row(
      manifest_tbl,
      fold_id = fold_id_now,
      fields = list(
        status = "completed",
        completed_at = as.character(Sys.time())
      )
    )
  }
  sentinel_write_table(manifest_tbl, sentinel_output_paths(object)$manifest_file)

  results_tbl <- if (length(results_rows) > 0L) {
    dplyr::bind_rows(results_rows)
  } else {
    tibble::tibble()
  }
  sentinel_write_table(results_tbl, sentinel_output_paths(object)$results_index_file)

  sentinel_rebuild(
    object,
    manifest = manifest_tbl,
    results = results_tbl
  )
}

#' Collect Sentinel summary outputs from disk
#'
#' @param object A [Sentinel] object.
#' @param completed_only Logical scalar.
#'
#' @return Tibble.
#'
#' @keywords internal
#' @noRd
collect_sentinel_results <- function(object,
                                     completed_only = TRUE) {
  # Always resume from disk first so collection reflects the latest fold state.
  if (!is_s7_instance(object, "Sentinel")) {
    stop("'object' must be a `Sentinel` object.", call. = FALSE)
  }

  object <- resume_sentinel(object)
  manifest_tbl <- object@manifest
  if (completed_only && "status" %in% names(manifest_tbl)) {
    manifest_tbl <- manifest_tbl |>
      dplyr::filter(as.character(.data$status) == "completed")
  }
  if (nrow(manifest_tbl) == 0L) {
    return(tibble::tibble())
  }

  purrr::map_dfr(seq_len(nrow(manifest_tbl)), function(i) {
    summary_path <- as.character(manifest_tbl$summary_file[[i]] %||% NA_character_)
    if (!nzchar(summary_path) || !file.exists(summary_path)) {
      return(tibble::tibble())
    }
    sentinel_read_table(summary_path)
  })
}

#' Collect results from a named Sentinel suite
#'
#' @param suite Named list of [Sentinel] objects.
#' @param completed_only Logical scalar forwarded to the per-object result
#'   collector.
#'
#' @return Tibble combining collected results across the suite with one added
#'   `suite_name` column.
#'
#' @keywords internal
#' @noRd
collect_sentinel_suite_results <- function(suite,
                                           completed_only = TRUE) {
  # Validate the suite container once so collection fails clearly before any
  # partial reads from disk occur.
  if (!is.list(suite) || length(suite) == 0L || is.null(names(suite)) || any(!nzchar(names(suite)))) {
    stop("'suite' must be a non-empty named list of `Sentinel` objects.", call. = FALSE)
  }
  if (!is.logical(completed_only) || length(completed_only) != 1L || is.na(completed_only)) {
    stop("'completed_only' must be TRUE or FALSE.", call. = FALSE)
  }

  # Collect each Sentinel separately, then annotate the suite member name and
  # split metadata so cross-split comparisons stay rectangular.
  purrr::map_dfr(names(suite), function(suite_name) {
    object <- suite[[suite_name]]
    if (!is_s7_instance(object, "Sentinel")) {
      stop(sprintf("Suite member '%s' is not a `Sentinel` object.", suite_name), call. = FALSE)
    }

    out <- collect_sentinel_results(object, completed_only = completed_only)
    if (nrow(out) == 0L) {
      return(tibble::tibble())
    }
    out |>
      dplyr::mutate(
        suite_name = suite_name,
        suite_split_mode = object@split_mode,
        suite_split_col = object@split_col,
        .before = 1
      )
  })
}

#' Build a Scorecard from Sentinel summary tables
#'
#' @param summary_tbl Primary summary table.
#' @param detail_tbl Fold-level detail table.
#' @param summary_type Summary discriminator.
#' @param message Status message.
#'
#' @return A [Scorecard] object.
#'
#' @keywords internal
#' @noRd
sentinel_scorecard <- function(summary_tbl = tibble::tibble(),
                               detail_tbl = tibble::tibble(),
                               summary_type = "sentinel_validation",
                               message = NULL) {
  summary_tbl <- tibble::as_tibble(summary_tbl)
  detail_tbl <- tibble::as_tibble(detail_tbl)
  if (!"summary_type" %in% names(summary_tbl)) {
    summary_tbl$summary_type <- rep(summary_type, nrow(summary_tbl))
  }
  if (!"summary_type" %in% names(detail_tbl)) {
    detail_tbl$summary_type <- rep(summary_type, nrow(detail_tbl))
  }
  status_message <- message %||% sprintf(
    "%s summary contains %d summary row(s) and %d fold-detail row(s).",
    summary_type,
    nrow(summary_tbl),
    nrow(detail_tbl)
  )

  Scorecard(
    intervals = tibble::tibble(),
    selected = tibble::tibble(),
    ts_panel = tibble::tibble(),
    recommendation_cards = summary_tbl,
    surrogate_rules = tibble::tibble(),
    consensus = tibble::tibble(),
    anchor_summary = summary_tbl,
    anchor_audit = if (identical(summary_type, "sentinel_ablation")) summary_tbl else tibble::tibble(),
    species_coverage = tibble::tibble(),
    selection_diagnostics = detail_tbl,
    key_missing_overall = tibble::tibble(),
    key_missing_by_field = tibble::tibble(),
    key_missing_by_model = tibble::tibble(),
    anchor_missing_gate = tibble::tibble(),
    status = tibble::tibble(
      component = summary_type,
      status = if (nrow(summary_tbl) > 0L) "ok" else "empty",
      message = status_message
    )
  )
}

#' Compute a rectangular Sentinel validation summary
#'
#' @param results Collected Sentinel result table.
#' @param group_cols Grouping columns.
#' @param metric_cols Optional metric columns.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
sentinel_validation_summary_table <- function(results,
                                              group_cols,
                                              metric_cols = NULL) {
  results <- tibble::as_tibble(results)
  if (nrow(results) == 0L) {
    return(tibble::tibble())
  }
  if (!is.character(group_cols) || length(group_cols) == 0L || any(!nzchar(group_cols))) {
    stop("'group_cols' must be a non-empty character vector.", call. = FALSE)
  }
  missing_groups <- setdiff(group_cols, names(results))
  if (length(missing_groups) > 0L) {
    stop(
      sprintf("Grouping column(s) were not found in `results`: %s", paste(missing_groups, collapse = ", ")),
      call. = FALSE
    )
  }
  if (is.null(metric_cols)) {
    metric_cols <- setdiff(names(results), group_cols)
    metric_cols <- metric_cols[vapply(
      results[metric_cols],
      function(x) is.numeric(x) || is.logical(x),
      logical(1)
    )]
  } else {
    metric_cols <- unique(as.character(metric_cols))
    missing_metrics <- setdiff(metric_cols, names(results))
    if (length(missing_metrics) > 0L) {
      stop(
        sprintf("Metric column(s) were not found in `results`: %s", paste(missing_metrics, collapse = ", ")),
        call. = FALSE
      )
    }
  }
  metric_numeric <- metric_cols[vapply(results[metric_cols], is.numeric, logical(1))]
  metric_logical <- metric_cols[vapply(results[metric_cols], is.logical, logical(1))]
  has_holdout_id <- "holdout_id" %in% names(results)
  results |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      n_holdouts = if (isTRUE(has_holdout_id)) dplyr::n_distinct(.data$holdout_id) else NA_integer_,
      dplyr::across(
        dplyr::all_of(metric_numeric),
        ~ mean(.x, na.rm = TRUE),
        .names = "mean_{.col}"
      ),
      dplyr::across(
        dplyr::all_of(metric_logical),
        ~ mean(as.numeric(.x), na.rm = TRUE),
        .names = "prop_{.col}"
      ),
      .groups = "drop"
    )
}

#' Summarize collected Sentinel suite results
#'
#' @param results Tibble returned by the Sentinel result collectors.
#' @param group_cols Character vector of grouping columns retained in the
#'   summary.
#' @param metric_cols Optional character vector of metric columns to summarize.
#'   When `NULL`, all non-group numeric and logical columns are summarized.
#'
#' @return A [Scorecard] containing the suite summary in
#'   `@recommendation_cards` and collected rows in `@selection_diagnostics`.
#'
#' @keywords internal
#' @noRd
summarize_sentinel_suite_results <- function(results,
                                             group_cols = c("suite_name", "suite_split_mode", "scenario"),
                                             metric_cols = NULL) {
  results <- tibble::as_tibble(results)
  summary_tbl <- sentinel_validation_summary_table(results, group_cols, metric_cols)
  sentinel_scorecard(
    summary_tbl = summary_tbl,
    detail_tbl = results,
    summary_type = "sentinel_suite"
  )
}

#' Resolve Sentinel results for plotting and ablation summaries
#'
#' @param x A [Sentinel] object or collected Sentinel result table.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
sentinel_results_data <- function(x) {
  if (is_s7_instance(x, "Sentinel")) {
    return(collect_sentinel_results(x))
  }
  if (!is.data.frame(x)) {
    stop("`x` must be a `Sentinel` object or a collected result table.", call. = FALSE)
  }
  tibble::as_tibble(x)
}

#' Summarize Sentinel outer-loop validation
#'
#' @param x A [Sentinel] object or collected Sentinel result table.
#' @param group_cols Optional grouping columns. Defaults to the available
#'   deployment-target, split-mode, and scenario fields.
#' @param metric_cols Optional numeric or logical metric columns.
#'
#' @return A [Scorecard] with aggregate validation rows in
#'   `@recommendation_cards` and fold results in `@selection_diagnostics`.
#'
#' @export
summarize_sentinel_validation <- function(x,
                                          group_cols = NULL,
                                          metric_cols = NULL) {
  results <- sentinel_results_data(x)
  group_cols <- group_cols %||% intersect(
    c("deployment_target", "split_mode", "scenario"),
    names(results)
  )
  if (length(group_cols) == 0L && nrow(results) > 0L) {
    stop("No Sentinel validation grouping columns were available.", call. = FALSE)
  }
  summary_tbl <- sentinel_validation_summary_table(
    results = results,
    group_cols = group_cols,
    metric_cols = metric_cols
  )
  sentinel_scorecard(
    summary_tbl = summary_tbl,
    detail_tbl = results,
    summary_type = "sentinel_validation"
  )
}

#' Summarize paired Sentinel ablations
#'
#' Compares every ablation scenario with the baseline on the same outer folds.
#' Positive importance means that removing the trait or component worsened
#' performance. Confidence intervals are paired, nonparametric bootstrap
#' intervals over outer folds.
#'
#' @param x A [Sentinel] object or a table returned by the Sentinel collectors.
#' @param metric Numeric metric column to compare.
#' @param baseline_label Name of the baseline scenario.
#' @param direction Whether lower or higher metric values are preferable. When
#'   `NULL`, common coverage and accuracy metrics are inferred as higher-is-
#'   better; all other metrics default to lower-is-better.
#' @param fold_summary Function used to reduce multiple metric rows within an
#'   outer fold; one of `"mean"` or `"median"`.
#' @param conf_level Bootstrap confidence level.
#' @param n_boot Number of paired bootstrap replicates.
#' @param seed Bootstrap seed.
#'
#' @return A [Scorecard] with baseline-paired importance estimates in
#'   `@recommendation_cards` and fold comparisons in
#'   `@selection_diagnostics`.
#'
#' @examples
#' \dontrun{
#' ablation <- summarize_sentinel_ablation(
#'   sentinel,
#'   metric = "error_abs_log",
#'   direction = "lower"
#' )
#' }
#'
#' @export
summarize_sentinel_ablation <- function(x,
                                        metric = "error_abs_log",
                                        baseline_label = "baseline",
                                        direction = NULL,
                                        fold_summary = c("mean", "median"),
                                        conf_level = 0.95,
                                        n_boot = 2000L,
                                        seed = 1L) {
  results <- sentinel_results_data(x)
  if (!is.character(metric) || length(metric) != 1L || !nzchar(metric)) {
    stop("`metric` must be one non-empty column name.", call. = FALSE)
  }
  if (!metric %in% names(results) || !(is.numeric(results[[metric]]) || is.logical(results[[metric]]))) {
    stop(sprintf("Numeric or logical metric column '%s' was not found in Sentinel results.", metric), call. = FALSE)
  }
  if (!"scenario" %in% names(results)) {
    stop("Sentinel results must contain a `scenario` column.", call. = FALSE)
  }
  if (!baseline_label %in% as.character(results$scenario)) {
    stop(sprintf("Baseline scenario '%s' was not found.", baseline_label), call. = FALSE)
  }
  fold_summary <- match.arg(fold_summary)
  if (is.null(direction)) {
    higher_metrics <- c("covered", "coverage", "empirical_coverage", "accuracy", "r2", "valid_prediction")
    direction <- if (tolower(metric) %in% higher_metrics || grepl("coverage|accuracy", metric, ignore.case = TRUE)) "higher" else "lower"
  }
  direction <- match.arg(direction, c("lower", "higher"))
  if (!is.numeric(conf_level) || length(conf_level) != 1L || !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be one number strictly between zero and one.", call. = FALSE)
  }
  n_boot <- as.integer(n_boot)
  if (!is.finite(n_boot) || n_boot < 1L) {
    stop("`n_boot` must be one integer >= 1.", call. = FALSE)
  }

  pair_cols <- intersect(
    c("deployment_target", "split_mode", "repeat_id", "outer_fold_id", "holdout_id"),
    names(results)
  )
  if (!"holdout_id" %in% pair_cols && !"outer_fold_id" %in% pair_cols) {
    stop("Sentinel results require `holdout_id` or `outer_fold_id` for paired ablation analysis.", call. = FALSE)
  }
  metadata_cols <- intersect(c("scenario_type", "ablated_traits"), names(results))
  group_cols <- unique(c("scenario", metadata_cols, pair_cols))
  fold_fun <- if (identical(fold_summary, "mean")) {
    function(z) mean(z, na.rm = TRUE)
  } else {
    function(z) stats::median(z, na.rm = TRUE)
  }

  fold_values <- results |>
    dplyr::filter(is.finite(.data[[metric]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(metric_value = fold_fun(.data[[metric]]), .groups = "drop")
  if (nrow(fold_values) == 0L) {
    return(sentinel_scorecard(summary_type = "sentinel_ablation"))
  }

  baseline <- fold_values |>
    dplyr::filter(as.character(.data$scenario) == baseline_label) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(pair_cols))) |>
    dplyr::summarise(baseline_value = mean(.data$metric_value, na.rm = TRUE), .groups = "drop")
  ablated <- fold_values |>
    dplyr::filter(as.character(.data$scenario) != baseline_label) |>
    dplyr::left_join(baseline, by = pair_cols) |>
    dplyr::filter(is.finite(.data$baseline_value), is.finite(.data$metric_value)) |>
    dplyr::mutate(
      importance = if (identical(direction, "lower")) {
        .data$metric_value - .data$baseline_value
      } else {
        .data$baseline_value - .data$metric_value
      }
    )
  if (nrow(ablated) == 0L) {
    return(sentinel_scorecard(
      detail_tbl = fold_values,
      summary_type = "sentinel_ablation"
    ))
  }

  set.seed(as.integer(seed))
  alpha <- (1 - conf_level) / 2
  split_rows <- split(ablated, as.character(ablated$scenario))
  summary_tbl <- purrr::map_dfr(split_rows, function(one) {
    effects <- as.numeric(one$importance)
    boot_mean <- replicate(
      n_boot,
      mean(sample(effects, length(effects), replace = TRUE), na.rm = TRUE)
    )
    baseline_mean <- mean(one$baseline_value, na.rm = TRUE)
    traits <- if ("ablated_traits" %in% names(one)) {
      unique(stats::na.omit(as.character(one$ablated_traits)))
    } else {
      character(0)
    }
    scenario_type <- if ("scenario_type" %in% names(one)) {
      scenario_types <- unique(stats::na.omit(as.character(one$scenario_type)))
      if (length(scenario_types) > 0L) scenario_types[[1]] else "custom"
    } else {
      "custom"
    }
    estimate <- mean(effects, na.rm = TRUE)
    tibble::tibble(
      scenario = as.character(one$scenario[[1]]),
      scenario_type = scenario_type,
      ablated_traits = paste(traits[nzchar(traits)], collapse = "|"),
      metric = metric,
      direction = direction,
      n_pairs = length(effects),
      baseline_mean = baseline_mean,
      ablated_mean = mean(one$metric_value, na.rm = TRUE),
      importance = estimate,
      importance_se = if (length(effects) > 1L) stats::sd(effects) / sqrt(length(effects)) else NA_real_,
      conf_low = as.numeric(stats::quantile(boot_mean, probs = alpha, names = FALSE, na.rm = TRUE)),
      conf_high = as.numeric(stats::quantile(boot_mean, probs = 1 - alpha, names = FALSE, na.rm = TRUE)),
      importance_pct = if (is.finite(baseline_mean) && abs(baseline_mean) > .Machine$double.eps) {
        100 * estimate / abs(baseline_mean)
      } else {
        NA_real_
      }
    )
  }) |>
    dplyr::arrange(dplyr::desc(.data$importance), .data$scenario)

  sentinel_scorecard(
    summary_tbl = summary_tbl,
    detail_tbl = ablated,
    summary_type = "sentinel_ablation"
  )
}

#' Plot paired Sentinel ablation importance from a Scorecard
#'
#' @param x A Sentinel-ablation [Scorecard].
#' @param labels Optional named character vector mapping scenario names to
#'   display labels.
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#'
plot_sentinel_ablation_scorecard <- function(x,
                                             labels = NULL,
                                             title = NULL) {
  summary_tbl <- tibble::as_tibble(x@anchor_audit)
  if (nrow(summary_tbl) == 0L) {
    summary_tbl <- tibble::as_tibble(x@recommendation_cards)
  }
  if (nrow(summary_tbl) == 0L) {
    stop("No paired ablation results were available to plot.", call. = FALSE)
  }
  trait_labels <- summary_tbl$ablated_traits %||% rep("", nrow(summary_tbl))
  display_label <- ifelse(
    !is.na(trait_labels) & nzchar(trait_labels),
    gsub("_", " ", gsub("\\|", ", ", trait_labels)),
    gsub("_", " ", summary_tbl$scenario)
  )
  if (!is.null(labels)) {
    mapped <- unname(labels[as.character(summary_tbl$scenario)])
    display_label[!is.na(mapped)] <- mapped[!is.na(mapped)]
  }
  if (anyDuplicated(display_label)) {
    duplicated_labels <- display_label %in% display_label[duplicated(display_label)]
    display_label[duplicated_labels] <- paste0(
      display_label[duplicated_labels],
      " (", summary_tbl$scenario[duplicated_labels], ")"
    )
  }
  summary_tbl$display_label <- factor(
    display_label,
    levels = unique(display_label[order(summary_tbl$importance)])
  )
  metric_values <- unique(as.character(summary_tbl$metric %||% "metric"))
  metric_label <- if (length(metric_values) > 0L) metric_values[[1]] else "metric"
  direction_values <- unique(as.character(summary_tbl$direction %||% "lower"))
  direction_label <- if (length(direction_values) > 0L) direction_values[[1]] else "lower"
  x_label <- if (identical(direction_label, "lower")) {
    paste0("Delta ", metric_label, " (ablated - baseline)")
  } else {
    paste0("Performance loss in ", metric_label, " (baseline - ablated)")
  }

  ggplot2::ggplot(summary_tbl, ggplot2::aes(y = .data$display_label, x = .data$importance)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey55", linewidth = 0.5, linetype = 2) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$conf_low, xend = .data$conf_high, yend = .data$display_label),
      linewidth = 0.8,
      color = "#3F3F3F"
    ) +
    ggplot2::geom_point(size = 2.7, color = "#3F3F3F") +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Plot Sentinel outer-fold performance from a Scorecard
#'
#' @param x A Sentinel-validation [Scorecard].
#' @param metric Numeric metric column.
#' @param fold_summary Within-fold reduction, `"mean"` or `"median"`.
#' @param baseline_label Baseline scenario label highlighted in the plot.
#' @param title Optional title.
#'
#' @return A ggplot object.
#'
plot_sentinel_validation_scorecard <- function(x,
                                               metric = "error_abs_log",
                                               fold_summary = c("mean", "median"),
                                               baseline_label = "baseline",
                                               title = NULL) {
  results <- tibble::as_tibble(x@selection_diagnostics)
  if (!metric %in% names(results) || !(is.numeric(results[[metric]]) || is.logical(results[[metric]]))) {
    stop(sprintf("Numeric or logical metric column '%s' was not found in Sentinel results.", metric), call. = FALSE)
  }
  fold_summary <- match.arg(fold_summary)
  pair_cols <- intersect(c("scenario", "repeat_id", "outer_fold_id", "holdout_id"), names(results))
  fold_fun <- if (identical(fold_summary, "mean")) mean else stats::median
  plot_tbl <- results |>
    dplyr::filter(is.finite(.data[[metric]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(pair_cols))) |>
    dplyr::summarise(metric_value = fold_fun(.data[[metric]], na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      is_baseline = as.character(.data$scenario) == baseline_label,
      scenario_label = gsub("_", " ", as.character(.data$scenario))
    )
  if (nrow(plot_tbl) == 0L) {
    stop("No finite Sentinel validation results were available to plot.", call. = FALSE)
  }

  ggplot2::ggplot(plot_tbl, ggplot2::aes(x = stats::reorder(.data$scenario_label, .data$metric_value, stats::median), y = .data$metric_value)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = .data$is_baseline), outlier.shape = NA, width = 0.65) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.12, height = 0), alpha = 0.45, size = 1.4) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = "#B6422E", `FALSE` = "#5B7F95"),
      breaks = c(TRUE, FALSE),
      labels = c("Baseline", "Trait ablation"),
      name = NULL
    ) +
    ggplot2::labs(title = title, x = NULL, y = metric) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

#' Summarize a `Sentinel`
#'
#' @param object A [Sentinel] object.
#' @param type Summary type, `"validation"` or `"ablation"`.
#' @param ... Arguments forwarded to the selected summarizer.
#'
#' @return A [Scorecard].
#'
#' @keywords internal
#' @noRd
S7::method(summary_generic, Sentinel) <- function(object,
                                                   type = c("validation", "ablation"),
                                                   ...) {
  type <- match.arg(type)
  if (identical(type, "ablation")) {
    return(summarize_sentinel_ablation(object, ...))
  }
  summarize_sentinel_validation(object, ...)
}
