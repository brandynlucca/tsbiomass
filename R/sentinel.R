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

#' @export
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
    species_folds = NULL,
    include_ts_error = NULL,
    throttle_inner_workers = TRUE,
    fast_validation = FALSE,
    fast_nmds_args = list(try = 3, trymax = 6),
    progress = FALSE,
    logging = FALSE,
    cache_dir = NULL,
    log_file = "sentinel.log",
    manifest_file = "sentinel_manifest.csv",
    results_index_file = "sentinel_results_index.csv",
    timings_file = "sentinel_fold_timings.csv",
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
  # Pull the shared candidate-model table regardless of the prepared wrapper.
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
  # Prefer the explicit config override, then fall back to object-level config.
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
  if (!is.null(spec$exclude_similarity_traits)) {
    spec$exclude_similarity_traits <- unique(as.character(spec$exclude_similarity_traits))
  }
  if (!is.null(spec$exclude_admissibility_traits)) {
    spec$exclude_admissibility_traits <- unique(as.character(spec$exclude_admissibility_traits))
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

#' Create a named Sentinel scenario grid
#'
#' @param trait_ablations Optional character vector for automatic one-at-a-time
#'   ablations, or a named list of character vectors for grouped ablations.
#'   Each element becomes one scenario with `exclude_similarity_traits` set to
#'   that vector. The source columns and downstream policy action space remain
#'   unchanged.
#' @param gate_ablations Optional named list of character vectors. Each element
#'   becomes one scenario with `exclude_admissibility_traits` set to that vector,
#'   relaxing the corresponding hard admissibility gate. Distinct from
#'   `trait_ablations`, which prunes similarity/distance features.
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
#' scenarios <- create_scenarios(
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
create_scenarios <- function(trait_ablations = NULL,
                             gate_ablations = NULL,
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
    ablated_traits = character(0),
    ablation_component = character(0)
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
      component_now <- if (length(traits_now) == 1L) {
        traits_now
      } else {
        sub("^without_", "", nm)
      }
      out[[nm]] <- list(
        exclude_similarity_traits = traits_now,
        scenario_type = "trait_ablation",
        ablated_traits = traits_now,
        ablation_component = component_now
      )
    }
  }

  if (!is.null(gate_ablations)) {
    # A plain character vector is the common relax-one-gate case; name each
    # scenario relax_gate_<trait>. Named lists allow relaxing several gates at once.
    if (is.character(gate_ablations)) {
      gate_ablations <- stats::setNames(
        lapply(unique(gate_ablations), function(x) x),
        paste0("relax_gate_", vapply(unique(gate_ablations), sentinel_safe_stem, character(1)))
      )
    }
    if (!is.list(gate_ablations) || is.null(names(gate_ablations)) || any(!nzchar(names(gate_ablations)))) {
      stop("'gate_ablations' must be a named list of character vectors.", call. = FALSE)
    }
    for (nm in names(gate_ablations)) {
      traits_now <- unique(as.character(gate_ablations[[nm]]))
      component_now <- if (length(traits_now) == 1L) {
        traits_now
      } else {
        sub("^relax_gate_", "", nm)
      }
      out[[nm]] <- list(
        exclude_admissibility_traits = traits_now,
        scenario_type = "gate_ablation",
        ablated_traits = traits_now,
        ablation_component = component_now
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
  # Only enumerate traits the ablation can actually act on. Ablation is applied by
  # sentinel_exclude_similarity_traits(), which prunes species/study traits from
  # the `similarity` and `alchemist` sections only. Reading `admissibility` here
  # would emit no-op scenarios (e.g. `without_swimbladder_type`) whose gate trait
  # is never in the similarity feature set, so the ablation reproduces baseline
  # exactly while spending a full fold and reporting a spurious ~0 importance.
  # Admissibility gates are a distinct estimand and require their own scenario.
  sections <- c("similarity", "alchemist")
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

#' Convert configured source traits into effective similarity-feature ablations
#'
#' @param config Workflow configuration.
#' @param traits Configured source-trait names available in the candidate data.
#'
#' @return A named list accepted by [create_scenarios()].
#'
#' @keywords internal
#' @noRd
sentinel_effective_trait_ablations <- function(config,
                                               traits) {
  config <- resolve_config_data(config)
  traits <- unique(as.character(traits %||% character(0)))
  traits <- traits[!is.na(traits) & nzchar(traits)]
  if (length(traits) == 0L) {
    return(list())
  }

  taxonomic_distance <- is.list(config) &&
    is.list(config$alchemist) &&
    isTRUE(config$alchemist$taxonomic_distance)
  taxonomic_traits <- if (taxonomic_distance) {
    traits[tolower(traits) %in% .ALCH_TAX_RANKS]
  } else {
    character(0)
  }

  out <- list()
  emitted_taxonomic_distance <- FALSE
  for (trait_now in traits) {
    if (trait_now %in% taxonomic_traits) {
      if (!emitted_taxonomic_distance) {
        # Alchemist replaces every configured taxonomic rank with one
        # `.dist_tax` feature. Ablate that effective feature as one unit while
        # retaining the raw rank columns for downstream policy definitions.
        out[["without_taxonomic_distance"]] <- taxonomic_traits
        emitted_taxonomic_distance <- TRUE
      }
      next
    }
    out[[paste0("without_", sentinel_safe_stem(trait_now))]] <- trait_now
  }
  out
}

#' Resolve every configured admissibility gate trait available in the data
#'
#' Reads the `admissibility` section's species/study traits. These are hard gates
#' (not similarity features), so they are enumerated for the separate gate-ablation
#' path rather than the similarity-trait ablation path.
#'
#' @param config Workflow configuration.
#' @param available_columns Candidate-model column names.
#'
#' @return Character vector of configured admissibility gate traits.
#'
#' @keywords internal
#' @noRd
sentinel_configured_gate_traits <- function(config,
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
  block <- config[["admissibility"]] %||% list()
  traits <- if (is.list(block)) {
    c(
      trait_names_from(block$species_traits %||% NULL),
      trait_names_from(block$study_traits %||% NULL)
    )
  } else {
    character(0)
  }
  traits <- unique(traits[!is.na(traits) & nzchar(traits)])
  if (is.null(available_columns)) traits else intersect(traits, available_columns)
}

#' Convert configured gate traits into effective gate-relaxation ablations
#'
#' @param traits Configured admissibility gate traits present in the data.
#'
#' @return A named list of `relax_gate_<trait>` -> trait, accepted by
#'   [create_scenarios()] as `gate_ablations`.
#'
#' @keywords internal
#' @noRd
sentinel_effective_gate_ablations <- function(traits) {
  traits <- unique(as.character(traits %||% character(0)))
  traits <- traits[!is.na(traits) & nzchar(traits)]
  if (length(traits) == 0L) {
    return(list())
  }
  out <- list()
  for (trait_now in traits) {
    out[[paste0("relax_gate_", sentinel_safe_stem(trait_now))]] <- trait_now
  }
  out
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
  cache_parent <- as.character(object@options$cache_dir %||% "")[[1]]
  cache_parent <- if (nzchar(cache_parent)) {
    path_absolute(cache_parent, must_work = FALSE)
  } else {
    ""
  }
  cache_root <- if (nzchar(cache_parent)) {
    file.path(
      cache_parent,
      sentinel_safe_stem(basename(object@output_dir)),
      object@options$cache_root_dir %||% "fold_cache"
    )
  } else {
    file.path(
      object@output_dir,
      object@options$cache_root_dir %||% "fold_cache"
    )
  }
  cache_root <- path_absolute(cache_root, must_work = FALSE)
  log_file <- path_absolute(
    file.path(cache_root, object@options$log_file %||% "sentinel.log"),
    must_work = FALSE
  )
  list(
    manifest_file = file.path(
      object@output_dir,
      object@options$manifest_file %||% "sentinel_manifest.csv"
    ),
    results_index_file = file.path(
      object@output_dir,
      object@options$results_index_file %||% "sentinel_results_index.csv"
    ),
    timings_file = file.path(
      object@output_dir,
      object@options$timings_file %||% "sentinel_fold_timings.csv"
    ),
    summary_dir = file.path(
      object@output_dir,
      object@options$summary_dir %||% "summaries"
    ),
    artifact_dir = file.path(
      object@output_dir,
      object@options$artifact_dir %||% "artifacts"
    ),
    cache_root_dir = cache_root,
    log_file = log_file
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
    species_ids <- sort(unique(stats::na.omit(as.character(data[[test_col]]))))
    species_ids <- species_ids[!is_missing_species_identity(species_ids)]
    species_folds <- options$species_folds %||% NULL
    if (!is.null(species_folds)) {
      if (!is.numeric(species_folds) || length(species_folds) != 1L ||
        !is.finite(species_folds) || species_folds < 2L) {
        stop("Sentinel option `species_folds` must be NULL or one integer >= 2.", call. = FALSE)
      }
      species_folds <- min(as.integer(species_folds), length(species_ids))
    }
    grouped_species <- if (is.null(species_folds) || species_folds >= length(species_ids)) {
      stats::setNames(as.list(species_ids), species_ids)
    } else {
      seed_now <- suppressWarnings(as.integer(options$seed %||% 1L))
      if (!is.finite(seed_now)) {
        seed_now <- 1L
      }
      set.seed(seed_now)
      shuffled_species <- sample(species_ids, length(species_ids), replace = FALSE)
      fold_id <- rep(seq_len(species_folds), length.out = length(shuffled_species))
      split_species <- split(shuffled_species, fold_id)
      stats::setNames(
        unname(split_species),
        sprintf("species_fold_%02d", seq_along(split_species))
      )
    }
    holdout_ids <- names(grouped_species)
    test_vals <- as.character(data[[test_col]])
    purge_vals <- lapply(purge_cols, function(col_now) as.character(data[[col_now]]))

    test_indices <- vector("list", length(holdout_ids))
    train_indices <- vector("list", length(holdout_ids))
    names(test_indices) <- holdout_ids
    names(train_indices) <- holdout_ids

    for (holdout_id in holdout_ids) {
      holdout_species <- grouped_species[[holdout_id]]
      test_idx <- which(test_vals %in% holdout_species)
      train_drop_mask <- Reduce(
        `|`,
        lapply(purge_vals, function(col_vals) col_vals %in% holdout_species)
      )
      test_indices[[holdout_id]] <- test_idx
      train_indices[[holdout_id]] <- all_rows[!train_drop_mask]
    }

    return(list(
      holdout_ids = holdout_ids,
      holdout_n = stats::setNames(vapply(test_indices, length, integer(1)), holdout_ids),
      test_indices = test_indices,
      train_indices = train_indices,
      holdout_groups = grouped_species
    ))
  }

  # For row, study, study-cell, and explicit group holdouts, cache the row
  # membership for each block once and derive training rows by complement.
  split_vals <- as.character(data[[split_col]])
  split_groups <- split(all_rows, split_vals)
  split_groups <- split_groups[!is.na(names(split_groups)) & nzchar(names(split_groups))]
  holdout_ids <- sort(names(split_groups))
  holdout_folds <- options$holdout_folds %||% NULL
  if (!is.null(holdout_folds)) {
    if (!is.numeric(holdout_folds) || length(holdout_folds) != 1L ||
      !is.finite(holdout_folds) || holdout_folds < 2L) {
      stop("Sentinel option `holdout_folds` must be NULL or one integer >= 2.", call. = FALSE)
    }
    holdout_folds <- min(as.integer(holdout_folds), length(holdout_ids))
  }

  if (!is.null(holdout_folds) && holdout_folds < length(holdout_ids)) {
    seed_now <- suppressWarnings(as.integer(options$seed %||% 1L))
    if (!is.finite(seed_now)) {
      seed_now <- 1L
    }
    set.seed(seed_now)
    shuffled_ids <- sample(holdout_ids, length(holdout_ids), replace = FALSE)
    fold_id <- rep(seq_len(holdout_folds), length.out = length(shuffled_ids))
    split_holdouts <- split(shuffled_ids, fold_id)
    fold_stem <- switch(split_mode,
      anchor_row_holdout = "anchor_row",
      study_holdout = "study",
      study_cell_holdout = "study_cell",
      group_holdout = "group",
      "holdout"
    )
    grouped_holdouts <- stats::setNames(
      unname(split_holdouts),
      sprintf("%s_fold_%02d", fold_stem, seq_along(split_holdouts))
    )
    holdout_ids <- names(grouped_holdouts)
    test_indices <- lapply(grouped_holdouts, function(ids_now) {
      unlist(split_groups[ids_now], use.names = FALSE)
    })
    names(test_indices) <- holdout_ids
    return(list(
      holdout_ids = holdout_ids,
      holdout_n = stats::setNames(vapply(test_indices, length, integer(1)), holdout_ids),
      test_indices = test_indices,
      train_indices = NULL,
      holdout_groups = grouped_holdouts
    ))
  }

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

#' Exclude traits from Sentinel's fold-local similarity model
#'
#' @param config Workflow config list.
#' @param traits Character vector of similarity traits to exclude.
#'
#' @return Config list with only similarity-model trait declarations pruned.
#'
#' @keywords internal
#' @noRd
sentinel_exclude_similarity_traits <- function(config,
                                               traits = NULL) {
  traits <- unique(as.character(traits %||% character(0)))
  traits <- traits[!is.na(traits) & nzchar(traits)]
  if (!is.list(config) || length(traits) == 0L) {
    return(config)
  }

  prune_traits <- function(x) {
    if (is.null(x)) {
      return(x)
    }
    if (!is.null(names(x)) && any(!is.na(names(x))) && any(nzchar(names(x)))) {
      return(x[setdiff(names(x), traits)])
    }
    values <- as.character(unlist(x, use.names = FALSE))
    setdiff(values, traits)
  }

  out <- config
  for (section in c("similarity", "alchemist")) {
    if (!is.list(out[[section]])) {
      next
    }
    for (key in c("species_traits", "study_traits")) {
      if (!is.null(out[[section]][[key]])) {
        out[[section]][[key]] <- prune_traits(out[[section]][[key]])
      }
    }
  }
  out
}

#' Relax admissibility gate traits for a Sentinel gate-ablation scenario
#'
#' Removes the named traits from the `admissibility` section only, so the
#' corresponding hard gate no longer restricts the donor pool. This is a distinct
#' estimand from similarity-trait exclusion: it does not touch the
#' similarity/distance feature set, it changes which donors are *admissible*. Used
#' by the `relax_gate_*` scenarios to quantify what a gate buys (or costs).
#'
#' @param config Workflow configuration list.
#' @param traits Character vector of admissibility gate traits to relax.
#'
#' @return The configuration with the named gate traits removed from
#'   `admissibility$species_traits` and `admissibility$study_traits`.
#'
#' @keywords internal
#' @noRd
sentinel_relax_admissibility_traits <- function(config,
                                                traits = NULL) {
  traits <- unique(as.character(traits %||% character(0)))
  traits <- traits[!is.na(traits) & nzchar(traits)]
  if (!is.list(config) || length(traits) == 0L) {
    return(config)
  }

  prune_traits <- function(x) {
    if (is.null(x)) {
      return(x)
    }
    if (!is.null(names(x)) && any(!is.na(names(x))) && any(nzchar(names(x)))) {
      return(x[setdiff(names(x), traits)])
    }
    values <- as.character(unlist(x, use.names = FALSE))
    setdiff(values, traits)
  }

  out <- config
  if (is.list(out[["admissibility"]])) {
    for (key in c("species_traits", "study_traits")) {
      if (!is.null(out[["admissibility"]][[key]])) {
        out[["admissibility"]][[key]] <- prune_traits(out[["admissibility"]][[key]])
      }
    }
  }
  out
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
      cache_names <- cfg_now$cache$names %||% list()
      cache_path_for <- function(section, key) {
        section_now <- cfg_now[[section]] %||% list()
        file_name <- cache_names[[key]] %||% basename(section_now$cache_path %||% paste0(key, ".rds"))
        file.path(cache_dir_now, file_name)
      }
      for (pair in list(
        c("similarity", "similarity_tuning"),
        c("admissibility", "anchor_admissibility"),
        c("benchmark", "policy_benchmark"),
        c("uncertainty", "policy_conformal"),
        c("selection", "policy_selection"),
        c("simulation", "policy_sensitivity")
      )) {
        section_name <- pair[[1]]
        key_name <- pair[[2]]
        cfg_now[[section_name]] <- cfg_now[[section_name]] %||% list()
        cfg_now[[section_name]]$cache_path <- cache_path_for(section_name, key_name)
      }
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

    # Base learners may own native thread pools independently of the R worker
    # count. Pin worker counts and every xgboost settings layer for both the
    # selection and uncertainty learners so their fits cannot oversubscribe the
    # outer Sentinel workers.
    for (stage_name in c("selection", "uncertainty")) {
      cfg_now[[stage_name]] <- cfg_now[[stage_name]] %||% list()
      cfg_now[[stage_name]]$workers <- 1L
      cfg_now[[stage_name]]$method_settings <-
        cfg_now[[stage_name]]$method_settings %||% list()
      cfg_now[[stage_name]]$method_settings$xgboost <-
        cfg_now[[stage_name]]$method_settings$xgboost %||% list()
      cfg_now[[stage_name]]$method_settings$xgboost$nthread <- 1L
    }

    cfg_now$simulation <- cfg_now$simulation %||% list()
    cfg_now$simulation$workers <- 1L

    cfg_now$alchemist <- cfg_now$alchemist %||% list()
    cfg_now$alchemist$learner <- cfg_now$alchemist$learner %||% list()
    cfg_now$alchemist$learner$workers <- 1L
    cfg_now$alchemist$distill_workers <- 1L
    cfg_now$sentinel <- cfg_now$sentinel %||% list()
    cfg_now$sentinel$inner_worker_policy <- "outer_parallel_inner_workers_1"
  }

  # TS-error reconstruction is independent of the scalar replacement-error
  # metrics used by Sentinel. Honor this setting without coupling it to the
  # separate fast-NMDS option.
  if (!is.null(options_now$include_ts_error)) {
    cfg_now$benchmark <- cfg_now$benchmark %||% list()
    cfg_now$benchmark$include_ts_error <- isTRUE(options_now$include_ts_error)
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
    "benchmark", "uncertainty", "policy_learner", "selection", "candidates"
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

  # Prune nested workflow sections used by the package objects.
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
  config_now <- sentinel_exclude_similarity_traits(
    config = config_now,
    traits = scenario_spec$exclude_similarity_traits %||% NULL
  )
  config_now <- sentinel_relax_admissibility_traits(
    config = config_now,
    traits = scenario_spec$exclude_admissibility_traits %||% NULL
  )
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
#' @param config Workflow config list or prepared config object.
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
  # Rebuild only the minimal candidates spec needed inside the fold.
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
#' @return A list with `summary_file`, `artifact_file`, `cache_dir`, and
#'   `log_file`.
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
    ),
    log_file = file.path(
      paths_now$cache_root_dir,
      paste0(fold_label, "_", scenario_label, "_", holdout_label),
      "sentinel_fold.log"
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
#'   outer-training and held-out [Candidates] rows; `workflow_config_s7` is the
#'   scenario-specific [Configurer]. The function may directly return a [Referee], [Scorecard],
#'   or a result list containing `metrics` or `selected`. It may be deferred
#'   and supplied to [run_sentinel()] instead.
#' @param config Optional config list or package object carrying config.
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
#'   `data`. Automatic ablations operate on effective similarity features: when
#'   `alchemist.taxonomic_distance` is enabled, configured taxonomic ranks are
#'   grouped into one `without_taxonomic_distance` scenario. Cannot be combined
#'   with `scenario_grid`.
#' @param gate_ablations Optional character vector, or `TRUE` to relax every
#'   configured admissibility gate trait present in `data`. Each becomes a
#'   `relax_gate_<trait>` scenario that removes the trait's hard admissibility
#'   gate (a distinct estimand from `trait_ablations`, which prunes similarity
#'   features). May be combined with `trait_ablations` but not `scenario_grid`.
#' @param cache_dir Optional Sentinel cache parent. An explicit value takes
#'   precedence over `sentinel.cache_dir` and `paths.cache_dir` in the config.
#' @param logging Optional logical scalar controlling per-fold log files.
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
                           trait_ablations = NULL,
                           gate_ablations = NULL,
                           cache_dir = NULL,
                           logging = NULL) {
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
  if (!is.null(scenario_grid) && (!is.null(trait_ablations) || !is.null(gate_ablations))) {
    stop("Supply either `scenario_grid` or `trait_ablations`/`gate_ablations`, not both.", call. = FALSE)
  }
  if (is.logical(trait_ablations)) {
    if (length(trait_ablations) != 1L || is.na(trait_ablations) || !isTRUE(trait_ablations)) {
      stop("Logical `trait_ablations` must be TRUE.", call. = FALSE)
    }
    configured_ablation_traits <- sentinel_configured_traits(config = config_tbl)
    available_ablation_traits <- intersect(configured_ablation_traits, names(data_tbl))
    missing_ablation_traits <- setdiff(configured_ablation_traits, available_ablation_traits)
    if (length(available_ablation_traits) == 0L) {
      stop("No configured species or study traits were present in `data`.", call. = FALSE)
    }
    trait_ablations <- sentinel_effective_trait_ablations(
      config = config_tbl,
      traits = available_ablation_traits
    )
  }
  if (is.logical(gate_ablations)) {
    if (length(gate_ablations) != 1L || is.na(gate_ablations) || !isTRUE(gate_ablations)) {
      stop("Logical `gate_ablations` must be TRUE.", call. = FALSE)
    }
    configured_gate_traits <- sentinel_configured_gate_traits(config = config_tbl)
    available_gate_traits <- intersect(configured_gate_traits, names(data_tbl))
    if (length(available_gate_traits) == 0L) {
      stop("No configured admissibility gate traits were present in `data`.", call. = FALSE)
    }
    gate_ablations <- sentinel_effective_gate_ablations(traits = available_gate_traits)
  }
  scenario_grid <- scenario_grid %||% create_scenarios(
    trait_ablations = trait_ablations,
    gate_ablations = gate_ablations
  )
  scenario_grid <- lapply(scenario_grid, sentinel_normalize_scenario_spec)
  output_dir <- sentinel_resolve_output_dir(config_tbl, output_dir = output_dir)
  options <- sentinel_default_options(merge_config_sections(
    config_tbl$sentinel %||% list(),
    options %||% list()
  ))
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
  options$progress <- isTRUE(options$progress %||% FALSE)
  options$logging <- isTRUE(logging %||% options$logging %||% FALSE)
  cache_dir_now <- cache_dir %||%
    options$cache_dir %||%
    resolve_config_value(config_tbl, "cache_dir", sections = "paths")
  options$cache_dir <- if (is.null(cache_dir_now) || !nzchar(as.character(cache_dir_now)[[1]])) {
    NULL
  } else {
    path_absolute(
      as.character(cache_dir_now)[[1]],
      base_dir = options$config_base_dir,
      must_work = FALSE
    )
  }
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
          ablation_component = paste(as.character(spec_now$ablation_component %||% character(0)), collapse = "|"),
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
          log_file = files_now$log_file,
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

#' Run one fold while capturing its console stream
#'
#' @param work Zero-argument function that executes the fold.
#' @param log_file Fold-specific log path.
#' @param progress Logical scalar controlling whether captured output is also
#'   relayed to the worker console.
#' @param fold_label Human-readable fold label.
#'
#' @return The value returned by `work()`.
#'
#' @keywords internal
#' @noRd
sentinel_with_fold_logging <- function(work,
                                       log_file,
                                       progress = FALSE,
                                       fold_label = "unknown") {
  if (!is.function(work)) {
    stop("'work' must be a function.", call. = FALSE)
  }
  if (!is.character(log_file) || length(log_file) != 1L || !nzchar(log_file)) {
    stop("'log_file' must be one non-empty path.", call. = FALSE)
  }
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
  connection <- file(log_file, open = "wt", encoding = "UTF-8")
  output_sink_depth <- sink.number(type = "output")
  sink(connection, split = isTRUE(progress), type = "output")
  on.exit(
    {
      while (sink.number(type = "output") > output_sink_depth) {
        sink(type = "output")
      }
      close(connection)
    },
    add = TRUE
  )

  write_condition <- function(kind, condition) {
    text <- conditionMessage(condition)
    cat(
      sprintf(
        "[%s] [%s] %s: %s\n",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        fold_label,
        kind,
        text
      ),
      file = connection
    )
    flush(connection)
  }

  cat(
    sprintf(
      "[%s] [%s] fold started\n",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      fold_label
    )
  )
  flush(connection)

  result <- withCallingHandlers(
    tryCatch(
      work(),
      error = function(e) {
        write_condition("ERROR", e)
        stop(e)
      }
    ),
    message = function(m) {
      write_condition("MESSAGE", m)
      if (!isTRUE(progress)) {
        invokeRestart("muffleMessage")
      }
    },
    warning = function(w) {
      write_condition("WARNING", w)
      if (!isTRUE(progress)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  cat(
    sprintf(
      "[%s] [%s] fold completed\n",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      fold_label
    )
  )
  flush(connection)
  result
}

#' Compile isolated fold logs into one deterministic Sentinel log
#'
#' @param manifest_tbl Sentinel manifest.
#' @param path Combined log destination.
#'
#' @return Invisibly returns `path`.
#'
#' @keywords internal
#' @noRd
sentinel_compile_fold_logs <- function(manifest_tbl,
                                       path) {
  manifest_tbl <- tibble::as_tibble(manifest_tbl)
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("'path' must be one non-empty log path.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  compiled <- character(0)
  for (i in seq_len(nrow(manifest_tbl))) {
    row_now <- manifest_tbl[i, , drop = FALSE]
    log_file <- if ("log_file" %in% names(row_now)) {
      as.character(row_now$log_file[[1]] %||% "")
    } else if ("cache_dir" %in% names(row_now)) {
      file.path(as.character(row_now$cache_dir[[1]]), "sentinel_fold.log")
    } else {
      ""
    }
    if (!nzchar(log_file) || !file.exists(log_file)) {
      next
    }
    lines <- readLines(log_file, warn = FALSE, encoding = "UTF-8")
    if (length(lines) == 0L) {
      next
    }
    prefix <- sprintf(
      "[fold=%04d scenario=%s] ",
      as.integer(row_now$fold_id[[1]]),
      as.character(row_now$scenario[[1]])
    )
    compiled <- c(compiled, paste0(prefix, lines))
  }
  writeLines(compiled, con = path, useBytes = TRUE)
  invisible(path)
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
  fold_total_start <- proc.time()
  timing_rows <- list()
  add_timing <- function(stage, start_time, extra = list()) {
    timing_rows[[length(timing_rows) + 1L]] <<- c(
      list(
        stage = stage,
        seconds = unname((proc.time() - start_time)[["elapsed"]])
      ),
      extra
    )
    invisible(NULL)
  }

  # Partition the candidate table first, then apply the active scenario.
  scenario_nm <- as.character(manifest_row$scenario[[1]])
  scenario_spec <- object@scenario_grid[[scenario_nm]] %||% list()
  stage_start <- proc.time()
  partitioned <- sentinel_partition_data(
    data = object@data,
    split_col = object@split_col,
    holdout_id = manifest_row$holdout_id[[1]],
    split_mode = object@split_mode,
    options = object@options,
    split_plan = split_plan
  )
  add_timing("partition_data", stage_start)
  stage_start <- proc.time()
  prepared <- sentinel_apply_scenario(
    train_data = partitioned$train_data,
    test_data = partitioned$test_data,
    config = object@config,
    scenario_spec = scenario_spec,
    manifest_row = manifest_row,
    object = object
  )
  add_timing("apply_scenario", stage_start)

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
  fold_candidates <- NULL
  fold_configurer <- NULL
  if (uses_object_workflow) {
    stage_start <- proc.time()
    fold_cache_file <- ""
    if ("cache_dir" %in% names(manifest_row)) {
      cache_dir_now <- as.character(manifest_row$cache_dir[[1]] %||% "")
      if (nzchar(cache_dir_now)) {
        dir.create(cache_dir_now, recursive = TRUE, showWarnings = FALSE)
        fold_cache_file <- file.path(cache_dir_now, "sentinel_fold_inputs.rds")
      }
    }
    cache_hit <- FALSE
    if (nzchar(fold_cache_file) &&
      file.exists(fold_cache_file) &&
      !isTRUE(object@options$cache_refresh %||% FALSE)) {
      cached_inputs <- readRDS(fold_cache_file)
      fold_candidates <- cached_inputs$candidates
      fold_configurer <- cached_inputs$configurer
      cache_hit <- TRUE
    } else {
      fold_candidates <- sentinel_build_candidates(
        candidate_models = prepared$train_data,
        reference_anchors = prepared$test_data,
        config = prepared$config
      )
      fold_configurer <- sentinel_fold_configurer(
        config = prepared$config,
        object = object
      )
      if (nzchar(fold_cache_file)) {
        saveRDS(
          list(
            candidates = fold_candidates,
            configurer = fold_configurer
          ),
          fold_cache_file
        )
      }
    }
    add_timing("prepare_object_workflow_inputs", stage_start, list(cache_hit = cache_hit))
  }

  stage_start <- proc.time()
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
  add_timing("workflow", stage_start)
  stage_start <- proc.time()
  result <- sentinel_normalize_result(result)
  add_timing("normalize_result", stage_start)

  # Attach fold metadata to the normalized metric table before writing it.
  stage_start <- proc.time()
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
      ablation_component = as.character((manifest_row$ablation_component %||% scenario_spec$ablation_component %||% "")[[1]]),
      split_mode = as.character(manifest_row$split_mode[[1]]),
      split_col = as.character(manifest_row$split_col[[1]]),
      holdout_id = as.character(manifest_row$holdout_id[[1]]),
      .before = 1
    )

  sentinel_write_table(metrics_tbl, manifest_row$summary_file[[1]])
  add_timing("write_summary", stage_start)

  # Persist one canonical case-study bundle so downstream inspection can rely
  # on stable field names even when the workflow also supplies custom artifacts.
  save_artifacts <- isTRUE(object@options$save_case_artifacts) &&
    isTRUE(manifest_row$case_study[[1]]) &&
    (!is.null(result$artifacts) || !is.null(result$selected) || !is.null(result$intervals))
  if (save_artifacts) {
    stage_start <- proc.time()
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
    add_timing("write_artifacts", stage_start)
  }

  timing_tbl <- dplyr::bind_rows(lapply(timing_rows, tibble::as_tibble)) |>
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
      split_mode = as.character(manifest_row$split_mode[[1]]),
      holdout_id = as.character(manifest_row$holdout_id[[1]]),
      .before = 1
    )
  timing_tbl <- dplyr::bind_rows(
    timing_tbl,
    tibble::tibble(
      fold_id = as.integer(manifest_row$fold_id[[1]]),
      outer_fold_id = as.integer((manifest_row$outer_fold_id %||% manifest_row$fold_id)[[1]]),
      repeat_id = as.integer((manifest_row$repeat_id %||% 1L)[[1]]),
      deployment_target = as.character(
        manifest_row$deployment_target[[1]] %||%
          object@options$deployment_target %||%
          "custom"
      ),
      scenario = scenario_nm,
      split_mode = as.character(manifest_row$split_mode[[1]]),
      holdout_id = as.character(manifest_row$holdout_id[[1]]),
      stage = "total",
      seconds = unname((proc.time() - fold_total_start)[["elapsed"]]),
      cache_hit = NA
    )
  )

  list(
    metrics = metrics_tbl,
    artifacts_saved = save_artifacts,
    timings = timing_tbl
  )
}

#' Format Sentinel fold tasks for progress messages
#'
#' @param tasks List of one-row Sentinel manifest tables.
#' @param max_tasks Maximum number of task labels to print before truncating.
#'
#' @return Single character string.
#' @keywords internal
#' @noRd
format_sentinel_fold_tasks <- function(tasks,
                                       max_tasks = 12L) {
  tasks <- Filter(Negate(is.null), tasks)
  if (length(tasks) == 0L) {
    return("none")
  }
  max_tasks <- max(1L, as.integer(max_tasks %||% 12L))
  shown <- head(tasks, max_tasks)
  labels <- vapply(shown, function(task) {
    row_now <- tibble::as_tibble(task)
    sprintf(
      "fold=%s scenario=%s holdout=%s",
      as.character(row_now$fold_id[[1]] %||% NA_character_),
      as.character(row_now$scenario[[1]] %||% NA_character_),
      as.character(row_now$holdout_id[[1]] %||% NA_character_)
    )
  }, character(1))
  if (length(tasks) > length(shown)) {
    labels <- c(labels, sprintf("... +%d more", length(tasks) - length(shown)))
  }
  paste(labels, collapse = ", ")
}

#' Split a Sentinel socket-failure suspect set
#'
#' @param tasks List of in-flight Sentinel tasks.
#'
#' @return List of one or two task groups.
#' @keywords internal
#' @noRd
split_sentinel_suspect_group <- function(tasks) {
  tasks <- Filter(Negate(is.null), tasks)
  n_tasks <- length(tasks)
  if (n_tasks <= 1L) {
    return(list(tasks))
  }
  cut <- floor(n_tasks / 2L)
  list(tasks[seq_len(cut)], tasks[(cut + 1L):n_tasks])
}

#' Run one Sentinel fold queue attempt
#'
#' @param tasks List of Sentinel manifest rows to dispatch.
#' @param workers Number of workers for this attempt.
#' @param run_task Fold task function.
#' @param progress Logical. Emit progress messages.
#' @param retry_mode Logical. Whether this attempt is isolating a suspect set.
#'
#' @return List with completed outputs, failed active tasks, unstarted tasks,
#'   and an optional socket error message.
#' @keywords internal
#' @noRd
run_sentinel_fold_queue_once <- function(tasks,
                                         workers,
                                         run_task,
                                         progress = FALSE,
                                         retry_mode = FALSE) {
  tasks <- Filter(Negate(is.null), tasks)
  if (length(tasks) == 0L) {
    return(list(completed = list(), failed_active = list(), unstarted = list(), error = NULL))
  }
  workers <- max(1L, min(as.integer(workers %||% 1L), length(tasks)))
  report_progress(
    progress,
    sprintf(
      "Sentinel outer-fold queue: dispatching %d %sfold(s) across %d worker(s).",
      length(tasks),
      if (retry_mode) "retry " else "",
      workers
    )
  )

  if (workers <= 1L && .Platform$OS.type != "unix") {
    completed <- lapply(tasks, run_task)
    return(list(completed = completed, failed_active = list(), unstarted = list(), error = NULL))
  }

  cluster_obj <- if (workers <= 1L && .Platform$OS.type == "unix") {
    cl <- parallel::makeForkCluster(1L)
    attr(cl, "cluster_type") <- "fork"
    cl
  } else {
    initialize_parallel_cluster(workers = workers, worker_output = progress)
  }
  on.exit(try(parallel::stopCluster(cluster_obj), silent = TRUE), add = TRUE)

  pending <- tasks
  active <- vector("list", length(cluster_obj))
  names(active) <- as.character(seq_along(cluster_obj))
  parallel_send_call <- utils::getFromNamespace("sendCall", "parallel")
  parallel_recv_one_result <- utils::getFromNamespace("recvOneResult", "parallel")

  submit_task <- function(node_id, task) {
    parallel_send_call(cluster_obj[[node_id]], run_task, list(task))
    active[[as.character(node_id)]] <<- task
    invisible(NULL)
  }

  for (node_id in seq_along(cluster_obj)) {
    if (length(pending) == 0L) {
      break
    }
    submit_task(node_id, pending[[1L]])
    pending <- pending[-1L]
  }
  report_progress(
    progress,
    "Sentinel outer-fold queue active folds: ",
    format_sentinel_fold_tasks(Filter(Negate(is.null), active))
  )

  completed <- list()
  queue_error <- NULL
  while (length(Filter(Negate(is.null), active)) > 0L) {
    received <- tryCatch(
      parallel_recv_one_result(cluster_obj),
      error = function(e) e
    )
    if (inherits(received, "error")) {
      queue_error <- conditionMessage(received)
      break
    }
    node_id <- as.character(received$node)
    task_done <- active[[node_id]]
    active[node_id] <- list(NULL)
    result <- if (inherits(received$value, "try-error") || inherits(received$value, "snow-try-error")) {
      row_now <- tibble::as_tibble(task_done)
      list(
        ok = FALSE,
        fold_id = as.integer(row_now$fold_id[[1]]),
        row = row_now,
        error_message = as.character(received$value %||% "worker returned an error")
      )
    } else {
      received$value
    }
    completed[[length(completed) + 1L]] <- result
    if (length(pending) > 0L) {
      submit_task(as.integer(node_id), pending[[1L]])
      pending <- pending[-1L]
    }
  }

  list(
    completed = completed,
    failed_active = Filter(Negate(is.null), active),
    unstarted = pending,
    error = queue_error
  )
}

#' Run Sentinel outer folds through a socket-failure-aware queue
#'
#' @param tasks List of one-row Sentinel manifest tables.
#' @param workers Requested worker budget.
#' @param run_task Fold task function.
#' @param progress Logical. Emit progress messages.
#'
#' @return List of Sentinel fold task outputs.
#' @keywords internal
#' @noRd
run_sentinel_fold_tasks <- function(tasks,
                                    workers,
                                    run_task,
                                    progress = FALSE) {
  total_tasks <- length(tasks)
  if (total_tasks == 0L) {
    return(list())
  }
  requested_workers <- max(1L, min(as.integer(workers %||% 1L), total_tasks))
  if (requested_workers <= 1L) {
    report_progress(progress, "Running Sentinel outer folds sequentially.")
    return(lapply(tasks, run_task))
  }

  report_progress(
    progress,
    "Running Sentinel outer folds in parallel with ",
    requested_workers,
    " workers."
  )
  report_progress(
    progress,
    "Sentinel outer-fold scheduler: queue mode, ",
    total_tasks,
    " total fold(s), up to ",
    requested_workers,
    " worker(s)."
  )

  completed_outputs <- list()
  pending_tasks <- tasks
  retry_groups <- list()

  while (length(pending_tasks) > 0L || length(retry_groups) > 0L) {
    retry_mode <- length(retry_groups) > 0L
    if (retry_mode) {
      queue_tasks <- retry_groups[[1L]]
      retry_groups <- retry_groups[-1L]
    } else {
      queue_tasks <- pending_tasks
    }
    attempt_workers <- min(requested_workers, length(queue_tasks))

    queue_result <- run_sentinel_fold_queue_once(
      tasks = queue_tasks,
      workers = attempt_workers,
      run_task = run_task,
      progress = progress,
      retry_mode = retry_mode
    )
    completed_outputs <- c(completed_outputs, queue_result$completed)

    if (is.null(queue_result$error)) {
      if (retry_mode) {
        if (length(queue_result$unstarted) > 0L) {
          retry_groups <- c(retry_groups, list(queue_result$unstarted))
        }
      } else {
        pending_tasks <- list()
      }
      next
    }

    if (length(queue_result$failed_active) == 0L) {
      if (length(queue_result$unstarted) > 0L) {
        if (retry_mode) {
          retry_groups <- c(list(queue_result$unstarted), retry_groups)
        } else {
          pending_tasks <- queue_result$unstarted
        }
        next
      }
      stop(
        paste(
          "Sentinel outer-fold queue failed without an active fold to isolate:",
          queue_result$error
        ),
        call. = FALSE
      )
    }

    isolated_failure <- retry_mode &&
      length(queue_result$failed_active) == 1L &&
      length(queue_tasks) == 1L
    if (isolated_failure) {
      row_now <- tibble::as_tibble(queue_result$failed_active[[1L]])
      fold_id_now <- as.integer(row_now$fold_id[[1]])
      error_message <- paste("socket failure isolated to this Sentinel fold:", queue_result$error)
      completed_outputs[[length(completed_outputs) + 1L]] <- list(
        ok = FALSE,
        fold_id = fold_id_now,
        row = row_now,
        error_message = error_message
      )
      report_progress(
        progress,
        sprintf(
          "Sentinel outer-fold task isolated after socket failure [fold=%d scenario=%s holdout=%s].\n%s",
          fold_id_now,
          as.character(row_now$scenario[[1]] %||% NA_character_),
          as.character(row_now$holdout_id[[1]] %||% NA_character_),
          error_message
        )
      )
    } else {
      suspect_groups <- split_sentinel_suspect_group(queue_result$failed_active)
      if (retry_mode) {
        retry_groups <- c(
          suspect_groups,
          if (length(queue_result$unstarted) > 0L) list(queue_result$unstarted) else list(),
          retry_groups
        )
      } else {
        retry_groups <- c(suspect_groups, retry_groups)
        pending_tasks <- queue_result$unstarted
      }
      report_progress(
        progress,
        sprintf(
          "Sentinel outer-fold queue failed while active folds were [%s]; retaining completed folds and splitting suspect set into %d group(s); %d pending fold(s) remain at full worker budget: %s",
          format_sentinel_fold_tasks(queue_result$failed_active),
          length(suspect_groups),
          length(queue_result$unstarted),
          queue_result$error
        )
      )
    }
  }

  completed_outputs
}

#' Run Sentinel outer-loop validation
#'
#' @param object A [Sentinel] object.
#' @param workflow_fn Optional workflow override.
#' @param refresh Logical scalar. When `TRUE`, rerun all folds and reset
#'   manifest status.
#' @param max_folds Optional integer cap for this invocation.
#' @param progress Optional logical override controlling console progress from
#'   both the parent and outer workers.
#' @param workers Optional number of outer-fold workers. Defaults to
#'   the Sentinel worker option.
#' @param logging Optional logical override controlling isolated per-fold logs
#'   and the compiled Sentinel log.
#'
#' @return An updated [Sentinel] object.
#'
#' @export
run_sentinel <- function(object,
                         workflow_fn = NULL,
                         refresh = FALSE,
                         max_folds = NULL,
                         progress = NULL,
                         workers = NULL,
                         logging = NULL) {
  # Resolve the workflow function and ensure the manifest exists before running.
  if (!is_s7_instance(object, "Sentinel")) {
    stop("'object' must be a `Sentinel` object.", call. = FALSE)
  }

  workflow_fn <- workflow_fn %||% object@workflow_fn
  if (is.null(workflow_fn) || !is.function(workflow_fn)) {
    stop("`workflow_fn` must be supplied either on the object or at call time.", call. = FALSE)
  }

  progress <- isTRUE(progress %||% object@options$progress %||% FALSE)
  logging <- isTRUE(logging %||% object@options$logging %||% FALSE)
  workers <- as.integer(workers %||% object@options$workers %||% 1L)
  if (!is.finite(workers) || workers < 1L) {
    stop("'workers' must be one integer >= 1.", call. = FALSE)
  }
  runtime_options <- object@options
  runtime_options$workers <- workers
  runtime_options$progress <- progress
  runtime_options$logging <- logging
  runtime_options$throttle_inner_workers <- isTRUE(runtime_options$throttle_inner_workers %||% FALSE)
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
  if (logging && !"log_file" %in% names(manifest_tbl)) {
    manifest_tbl$log_file <- file.path(
      as.character(manifest_tbl$cache_dir),
      "sentinel_fold.log"
    )
    object <- sentinel_rebuild(object, manifest = manifest_tbl)
  }
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
    if (file.exists(sentinel_output_paths(object)$timings_file)) {
      report_progress(progress, "Sentinel timing table: ", sentinel_output_paths(object)$timings_file)
    }
    if (logging) {
      sentinel_compile_fold_logs(
        manifest_tbl = manifest_tbl,
        path = sentinel_output_paths(object)$log_file
      )
    }
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
  report_progress(
    progress,
    "Sentinel runtime: pending_folds=", length(task_rows),
    ", requested_workers=", workers,
    ", effective_workers=", min(workers, length(task_rows)),
    ", throttle_inner_workers=", runtime_options$throttle_inner_workers,
    "."
  )
  if (runtime_options$throttle_inner_workers && workers > 1L) {
    report_progress(
      progress,
      "Sentinel worker policy: outer folds parallelized; fold-local benchmark, Alchemist, selection, uncertainty, and simulation workers forced to 1."
    )
  }
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
    log_file_now <- if ("log_file" %in% names(row_now)) {
      as.character(row_now$log_file[[1]] %||% "")
    } else {
      ""
    }
    if (!nzchar(log_file_now)) {
      log_file_now <- file.path(
        as.character(row_now$cache_dir[[1]]),
        "sentinel_fold.log"
      )
    }
    fold_label <- sprintf(
      "fold=%04d scenario=%s holdout=%s",
      fold_id_now,
      as.character(row_now$scenario[[1]]),
      as.character(row_now$holdout_id[[1]])
    )
    execute_fold <- function() {
      sentinel_run_one_fold(
        object = worker_object,
        manifest_row = row_now,
        workflow_fn = workflow_fn,
        progress = progress,
        split_plan = split_plan
      )
    }
    tryCatch(
      list(
        ok = TRUE,
        fold_id = fold_id_now,
        row = row_now,
        fold_result = if (logging) {
          sentinel_with_fold_logging(
            work = execute_fold,
            log_file = log_file_now,
            progress = progress,
            fold_label = fold_label
          )
        } else {
          execute_fold()
        }
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

  fold_outputs <- run_sentinel_fold_tasks(
    tasks = task_rows,
    workers = min(workers, length(task_rows)),
    run_task = run_task,
    progress = progress
  )

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
    row_now$ablation_component <- row_now$ablation_component %||% ""
    row_now$log_file <- row_now$log_file %||% NA_character_
    results_rows[[length(results_rows) + 1L]] <- row_now |>
      dplyr::transmute(
        fold_id = as.integer(.data$fold_id),
        outer_fold_id = as.integer(.data$outer_fold_id),
        repeat_id = as.integer(.data$repeat_id),
        scenario = as.character(.data$scenario),
        scenario_type = as.character(.data$scenario_type),
        ablated_traits = as.character(.data$ablated_traits),
        ablation_component = as.character(.data$ablation_component),
        holdout_id = as.character(.data$holdout_id),
        summary_file = as.character(.data$summary_file),
        artifact_file = as.character(.data$artifact_file),
        log_file = as.character(.data$log_file),
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
  if (logging) {
    sentinel_compile_fold_logs(
      manifest_tbl = manifest_tbl,
      path = sentinel_output_paths(object)$log_file
    )
  }

  results_tbl <- if (length(results_rows) > 0L) {
    dplyr::bind_rows(results_rows)
  } else {
    tibble::tibble()
  }
  sentinel_write_table(results_tbl, sentinel_output_paths(object)$results_index_file)

  timing_rows <- dplyr::bind_rows(lapply(
    fold_outputs,
    function(task_out) {
      if (!isTRUE(task_out$ok)) {
        row_now <- tibble::as_tibble(task_out$row)
        return(tibble::tibble(
          fold_id = as.integer(task_out$fold_id),
          outer_fold_id = as.integer((row_now$outer_fold_id %||% row_now$fold_id)[[1]]),
          repeat_id = as.integer((row_now$repeat_id %||% 1L)[[1]]),
          deployment_target = as.character(row_now$deployment_target[[1]] %||% object@options$deployment_target %||% "custom"),
          scenario = as.character(row_now$scenario[[1]]),
          split_mode = as.character(row_now$split_mode[[1]]),
          holdout_id = as.character(row_now$holdout_id[[1]]),
          stage = "failed",
          seconds = NA_real_,
          cache_hit = NA,
          error_message = task_out$error_message %||% "unknown error"
        ))
      }
      if (!is.null(task_out$fold_result$timings) &&
        is.data.frame(task_out$fold_result$timings) &&
        nrow(task_out$fold_result$timings) > 0L) {
        return(task_out$fold_result$timings)
      }
      row_now <- tibble::as_tibble(task_out$row)
      tibble::tibble(
        fold_id = as.integer(task_out$fold_id),
        outer_fold_id = as.integer((row_now$outer_fold_id %||% row_now$fold_id)[[1]]),
        repeat_id = as.integer((row_now$repeat_id %||% 1L)[[1]]),
        deployment_target = as.character(row_now$deployment_target[[1]] %||% object@options$deployment_target %||% "custom"),
        scenario = as.character(row_now$scenario[[1]]),
        split_mode = as.character(row_now$split_mode[[1]]),
        holdout_id = as.character(row_now$holdout_id[[1]]),
        stage = "timing_unavailable",
        seconds = NA_real_,
        cache_hit = NA,
        error_message = NA_character_
      )
    }
  ))
  if (nrow(timing_rows) > 0L) {
    sentinel_write_table(timing_rows, sentinel_output_paths(object)$timings_file)
    report_progress(progress, "Sentinel timing table: ", sentinel_output_paths(object)$timings_file)
  }

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
    out <- sentinel_read_table(summary_path)
    fingerprint_cols <- names(out)[grepl("fingerprint", names(out), fixed = TRUE)]
    for (col in fingerprint_cols) {
      out[[col]] <- as.character(out[[col]])
    }
    out
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
    anchor_audit = if (summary_type %in% c(
      "sentinel_ablation",
      "sentinel_ablation_decomposition"
    )) {
      summary_tbl
    } else {
      tibble::tibble()
    },
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
#' @return A [Scorecard] containing the suite summary and collected rows.
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

#' Resolve configured reference-anchor species from a Sentinel source table
#'
#' @keywords internal
#' @noRd
sentinel_reference_anchor_species <- function(x) {
  if (!is_s7_instance(x, "Sentinel")) {
    return(character(0))
  }
  config <- x@config %||% list()
  anchor_spec <- (((config$candidates %||% list())$anchors) %||% list())
  if (length(anchor_spec) == 0L) {
    anchor_spec <- (config$anchors %||% list())
  }
  selector <- anchor_spec$selector %||% NULL
  model_ids <- anchor_spec$model_ids %||% NULL
  if (is.null(selector) && is.null(model_ids)) {
    return(character(0))
  }
  source <- tibble::as_tibble(x@data)
  reference_rows <- tryCatch(
    {
      if (!is.null(selector)) {
        set_reference_anchors(
          source,
          selector = selector,
          require_selection = FALSE
        )
      } else {
        set_reference_anchors(
          source,
          model_ids = model_ids,
          model_id_col = anchor_spec$model_id_col %||% "model_id",
          require_selection = FALSE
        )
      }
    },
    error = function(e) tibble::tibble()
  )
  species_col <- intersect(c("species_name", "anchor_species"), names(reference_rows))
  if (length(species_col) == 0L) {
    return(character(0))
  }
  species <- unique(as.character(reference_rows[[species_col[[1]]]]))
  species[!is.na(species) & nzchar(species)]
}

#' Mark configured reference-anchor species in Sentinel results
#'
#' @keywords internal
#' @noRd
sentinel_add_reference_anchor_flag <- function(x,
                                               results) {
  results <- tibble::as_tibble(results)
  if ("is_reference_anchor_species" %in% names(results)) {
    return(results)
  }
  reference_species <- sentinel_reference_anchor_species(x)
  species_col <- intersect(c("anchor_species", "species_name"), names(results))
  if (length(species_col) == 0L) {
    results$is_reference_anchor_species <- rep(FALSE, nrow(results))
    return(results)
  }
  species <- as.character(results[[species_col[[1]]]])
  results$is_reference_anchor_species <- !is.na(species) & species %in% reference_species
  results
}

#' Summarize Sentinel outer-loop validation
#'
#' @param x A [Sentinel] object or collected Sentinel result table.
#' @param group_cols Optional grouping columns. Defaults to the available
#'   deployment-target, split-mode, and scenario fields.
#' @param metric_cols Optional numeric or logical metric columns.
#'
#' @return A [Scorecard] with aggregate validation rows and fold results.
#'
#' @keywords internal
#' @noRd
summarize_sentinel_validation <- function(x,
                                          group_cols = NULL,
                                          metric_cols = NULL) {
  results <- sentinel_results_data(x)
  results <- sentinel_add_reference_anchor_flag(x, results)
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

#' Wilson score interval for a binomial proportion
#'
#' @keywords internal
#' @noRd
sentinel_wilson_interval <- function(successes,
                                     trials,
                                     confidence_level) {
  successes <- as.numeric(successes)
  trials <- as.numeric(trials)
  if (!is.finite(successes) || !is.finite(trials) || trials <= 0L) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  proportion <- successes / trials
  z <- stats::qnorm(1 - (1 - confidence_level) / 2)
  denominator <- 1 + z^2 / trials
  center <- (proportion + z^2 / (2 * trials)) / denominator
  radius <- z * sqrt(
    proportion * (1 - proportion) / trials + z^2 / (4 * trials^2)
  ) / denominator
  c(lower = max(0, center - radius), upper = min(1, center + radius))
}

#' Resolve nominal Sentinel interval coverage
#'
#' @keywords internal
#' @noRd
sentinel_nominal_coverage <- function(x,
                                      nominal = NULL) {
  if (!is.null(nominal)) {
    if (!is.numeric(nominal) || length(nominal) != 1L ||
      !is.finite(nominal) || nominal <= 0 || nominal >= 1) {
      stop("`nominal` must be NULL or one number strictly between zero and one.", call. = FALSE)
    }
    return(as.numeric(nominal))
  }
  if (!is_s7_instance(x, "Sentinel")) {
    return(NA_real_)
  }
  config <- x@config %||% list()
  alpha_candidates <- c(
    (config$policy %||% list())$conformal_alpha %||% NA_real_,
    (config$similarity %||% list())$conformal_alpha %||% NA_real_,
    config$conformal_alpha %||% NA_real_
  )
  alpha_candidates <- suppressWarnings(as.numeric(alpha_candidates))
  alpha_candidates <- alpha_candidates[
    is.finite(alpha_candidates) & alpha_candidates > 0 & alpha_candidates < 1
  ]
  if (length(alpha_candidates) == 0L) NA_real_ else 1 - alpha_candidates[[1]]
}

#' Summarize Sentinel prediction and interval coverage
#'
#' Separates conditional interval coverage among estimable predictions from
#' operational coverage, which counts a non-estimable prediction as uncovered.
#'
#' @param x A [Sentinel] object or collected Sentinel result table.
#' @param nominal Optional nominal coverage. When `NULL`, it is derived from the
#'   Sentinel conformal-alpha configuration when available.
#' @param group_cols Optional grouping columns.
#' @param confidence_level Confidence level for row-level Wilson intervals.
#'
#' @return A [Scorecard] with coverage summaries and collected results.
#'
#' @keywords internal
#' @noRd
summarize_sentinel_coverage <- function(x,
                                        nominal = NULL,
                                        group_cols = NULL,
                                        confidence_level = 0.95) {
  results <- sentinel_results_data(x)
  results <- sentinel_add_reference_anchor_flag(x, results)
  if (!"covered" %in% names(results)) {
    stop("Sentinel results do not contain the `covered` interval indicator.", call. = FALSE)
  }
  if (!is.numeric(confidence_level) || length(confidence_level) != 1L ||
    !is.finite(confidence_level) || confidence_level <= 0 || confidence_level >= 1) {
    stop("`confidence_level` must be one number strictly between zero and one.", call. = FALSE)
  }
  nominal <- sentinel_nominal_coverage(x, nominal)
  group_cols <- group_cols %||% intersect(
    c("deployment_target", "split_mode", "scenario"),
    names(results)
  )
  group_cols <- sentinel_inference_names(
    group_cols,
    arg = "group_cols",
    allow_null = FALSE,
    allow_empty = TRUE
  )
  missing_groups <- setdiff(group_cols, names(results))
  if (length(missing_groups) > 0L) {
    stop(sprintf(
      "Coverage grouping column(s) were not found: %s.",
      paste(missing_groups, collapse = ", ")
    ), call. = FALSE)
  }

  valid_prediction <- if ("valid_prediction" %in% names(results)) {
    as.logical(results$valid_prediction) %in% TRUE
  } else if ("error_abs_log" %in% names(results)) {
    is.finite(suppressWarnings(as.numeric(results$error_abs_log)))
  } else {
    rep(FALSE, nrow(results))
  }
  finite_error <- if ("error_abs_log" %in% names(results)) {
    is.finite(suppressWarnings(as.numeric(results$error_abs_log)))
  } else {
    valid_prediction
  }
  finite_interval <- if ("interval_log_width" %in% names(results)) {
    is.finite(suppressWarnings(as.numeric(results$interval_log_width)))
  } else {
    !is.na(results$covered)
  }
  results$.sentinel_estimable_interval <- valid_prediction & finite_error & finite_interval
  results$.sentinel_interval_covered <- as.logical(results$covered) %in% TRUE
  interval_width <- if ("interval_log_width" %in% names(results)) {
    suppressWarnings(as.numeric(results$interval_log_width))
  } else {
    rep(NA_real_, nrow(results))
  }
  results$.sentinel_interval_width <- interval_width

  summary_tbl <- results |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_total = dplyr::n(),
      n_estimable = sum(.data$.sentinel_estimable_interval),
      n_covered = sum(
        .data$.sentinel_estimable_interval & .data$.sentinel_interval_covered
      ),
      estimability_rate = .data$n_estimable / .data$n_total,
      conditional_coverage = dplyr::if_else(
        .data$n_estimable > 0,
        .data$n_covered / .data$n_estimable,
        NA_real_
      ),
      operational_coverage = .data$n_covered / .data$n_total,
      mean_interval_log_width = mean(
        .data$.sentinel_interval_width[.data$.sentinel_estimable_interval],
        na.rm = TRUE
      ),
      median_interval_log_width = stats::median(
        .data$.sentinel_interval_width[.data$.sentinel_estimable_interval],
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  if (nrow(summary_tbl) > 0L) {
    conditional_intervals <- t(vapply(seq_len(nrow(summary_tbl)), function(i) {
      sentinel_wilson_interval(
        summary_tbl$n_covered[[i]],
        summary_tbl$n_estimable[[i]],
        confidence_level
      )
    }, numeric(2)))
    operational_intervals <- t(vapply(seq_len(nrow(summary_tbl)), function(i) {
      sentinel_wilson_interval(
        summary_tbl$n_covered[[i]],
        summary_tbl$n_total[[i]],
        confidence_level
      )
    }, numeric(2)))
    estimability_intervals <- t(vapply(seq_len(nrow(summary_tbl)), function(i) {
      sentinel_wilson_interval(
        summary_tbl$n_estimable[[i]],
        summary_tbl$n_total[[i]],
        confidence_level
      )
    }, numeric(2)))
    summary_tbl$conditional_lower <- conditional_intervals[, "lower"]
    summary_tbl$conditional_upper <- conditional_intervals[, "upper"]
    summary_tbl$operational_lower <- operational_intervals[, "lower"]
    summary_tbl$operational_upper <- operational_intervals[, "upper"]
    summary_tbl$estimability_lower <- estimability_intervals[, "lower"]
    summary_tbl$estimability_upper <- estimability_intervals[, "upper"]
  }
  summary_tbl$nominal_coverage <- nominal
  summary_tbl$conditional_coverage_gap <- summary_tbl$conditional_coverage - nominal
  summary_tbl$confidence_level <- confidence_level
  summary_tbl$interval_method <- "wilson_row_level"

  sentinel_scorecard(
    summary_tbl = summary_tbl,
    detail_tbl = results |>
      dplyr::select(-dplyr::starts_with(".sentinel_")),
    summary_type = "sentinel_coverage"
  )
}

#' Validate column-name vectors used by Sentinel inference
#'
#' @keywords internal
#' @noRd
sentinel_inference_names <- function(x,
                                     arg,
                                     allow_null = TRUE,
                                     allow_empty = FALSE) {
  if (is.null(x)) {
    if (allow_null) {
      return(NULL)
    }
    stop(sprintf("`%s` cannot be NULL.", arg), call. = FALSE)
  }
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    stop(sprintf("`%s` must be a character vector of non-empty column names.", arg), call. = FALSE)
  }
  x <- unique(x)
  if (!allow_empty && length(x) == 0L) {
    stop(sprintf("`%s` must contain at least one column name.", arg), call. = FALSE)
  }
  x
}

#' Add Sentinel source columns to collected fold results
#'
#' @keywords internal
#' @noRd
sentinel_ablation_add_source_columns <- function(x,
                                                 results,
                                                 column_names) {
  missing_names <- setdiff(column_names, names(results))
  if (length(missing_names) == 0L || !is_s7_instance(x, "Sentinel")) {
    return(results)
  }
  source <- tibble::as_tibble(x@data)
  missing_names <- intersect(missing_names, names(source))
  if (length(missing_names) == 0L) {
    return(results)
  }
  result_key <- intersect(c("anchor_model_id", "model_id", "model_id_chr"), names(results))
  source_key <- intersect(c("model_id", "model_id_chr"), names(source))
  if (length(result_key) == 0L || length(source_key) == 0L) {
    return(results)
  }
  result_key <- result_key[[1]]
  source_key <- source_key[[1]]
  source_lookup <- source |>
    dplyr::transmute(
      .sentinel_source_key = as.character(.data[[source_key]]),
      dplyr::across(dplyr::all_of(missing_names))
    ) |>
    dplyr::distinct(.data$.sentinel_source_key, .keep_all = TRUE)
  results |>
    dplyr::mutate(.sentinel_source_key = as.character(.data[[result_key]])) |>
    dplyr::left_join(source_lookup, by = ".sentinel_source_key") |>
    dplyr::select(-dplyr::all_of(".sentinel_source_key"))
}

#' Resolve the default Sentinel bootstrap cluster
#'
#' @keywords internal
#' @noRd
sentinel_ablation_cluster_names <- function(x,
                                            results,
                                            cluster_names) {
  explicit <- !is.null(cluster_names)
  cluster_names <- sentinel_inference_names(
    cluster_names,
    arg = "cluster_names",
    allow_null = TRUE,
    allow_empty = FALSE
  )
  if (explicit) {
    return(cluster_names)
  }

  split_name <- NULL
  if (is_s7_instance(x, "Sentinel") && length(x@split_col) == 1L && nzchar(x@split_col)) {
    split_name <- as.character(x@split_col)
  } else if ("split_col" %in% names(results)) {
    candidates <- unique(stats::na.omit(as.character(results$split_col)))
    candidates <- candidates[nzchar(candidates)]
    if (length(candidates) == 1L) {
      split_name <- candidates[[1]]
    }
  }
  aliases <- list(
    species_name = c("species_name", "anchor_species"),
    study_reference_id = c("study_reference_id", "anchor_study_reference_id"),
    study_cell_id = c("study_cell_id", "anchor_study_cell_id"),
    model_id = c("anchor_model_id", "model_id", "model_id_chr"),
    model_id_chr = c("anchor_model_id", "model_id_chr", "model_id")
  )
  if (!is.null(split_name)) {
    available <- aliases[[split_name]] %||% split_name
    available <- intersect(available, names(results))
    if (length(available) > 0L) {
      return(available[[1]])
    }
    return(split_name)
  }
  fallback <- intersect(
    c("anchor_species", "species_name", "study_reference_id", "study_cell_id", "anchor_model_id", "holdout_id"),
    names(results)
  )
  if (length(fallback) == 0L) {
    stop(
      "`cluster_names = NULL` could not be resolved; supply an explicit cluster column.",
      call. = FALSE
    )
  }
  fallback[[1]]
}

#' Construct a stable joint key from one or more columns
#'
#' @keywords internal
#' @noRd
sentinel_joint_key <- function(data,
                               column_names) {
  if (length(column_names) == 0L) {
    return(rep(".all", nrow(data)))
  }
  values <- lapply(data[column_names], function(z) {
    z <- as.character(z)
    z[is.na(z)] <- "<NA>"
    z
  })
  do.call(paste, c(values, sep = "\r"))
}

#' Estimate a stratified cluster mean and standard error
#'
#' @keywords internal
#' @noRd
sentinel_stratified_mean_se <- function(values,
                                        strata) {
  keep <- is.finite(values) & !is.na(strata)
  values <- as.numeric(values[keep])
  strata <- as.character(strata[keep])
  if (length(values) == 0L) {
    return(c(estimate = NA_real_, se = NA_real_, n = 0))
  }
  groups <- split(values, strata)
  n_h <- vapply(groups, length, integer(1))
  mean_h <- vapply(groups, mean, numeric(1))
  total_n <- sum(n_h)
  estimate <- sum(n_h * mean_h) / total_n
  if (any(n_h < 2L)) {
    return(c(estimate = estimate, se = NA_real_, n = total_n))
  }
  var_h <- vapply(groups, stats::var, numeric(1))
  se <- sqrt(sum(n_h * var_h) / (total_n^2))
  c(estimate = estimate, se = se, n = total_n)
}

#' Estimate a stratified mean while preserving fixed design weights
#'
#' @keywords internal
#' @noRd
sentinel_fixed_stratum_mean <- function(values,
                                        strata,
                                        stratum_weights) {
  values <- as.numeric(values)
  strata <- as.character(strata)
  weight_names <- names(stratum_weights)
  stratum_weights <- as.numeric(stratum_weights)
  names(stratum_weights) <- weight_names
  if (length(stratum_weights) == 0L || is.null(weight_names) || any(!nzchar(weight_names))) {
    stop("Fixed stratum weights must be a named numeric vector.", call. = FALSE)
  }
  means <- vapply(names(stratum_weights), function(stratum_now) {
    values_now <- values[strata == stratum_now & is.finite(values)]
    if (length(values_now) == 0L) NA_real_ else mean(values_now)
  }, numeric(1))
  if (any(!is.finite(means))) {
    return(NA_real_)
  }
  sum(stratum_weights * means)
}

#' Convert a two-sided bootstrap p-value to finite Monte Carlo form
#'
#' @keywords internal
#' @noRd
sentinel_bootstrap_tail_probability <- function(statistics,
                                                observed) {
  statistics <- abs(as.numeric(statistics[is.finite(statistics)]))
  if (length(statistics) == 0L || !is.finite(observed)) {
    return(NA_real_)
  }
  (1 + sum(statistics >= abs(observed))) / (length(statistics) + 1)
}

#' Summarize paired Sentinel ablations
#'
#' Compares every ablation scenario with the baseline on the same outer folds.
#' Positive importance means that removing the trait or component worsened
#' performance. Optional confidence intervals preserve baseline-ablation
#' pairing and resample user-defined holdout clusters within design strata.
#'
#' @param x A [Sentinel] object or a table returned by the Sentinel collectors.
#' @param metric Numeric metric column to compare.
#' @param baseline_label Name of the baseline scenario.
#' @param direction Whether lower or higher metric values are preferable. When
#'   `NULL`, common coverage and accuracy metrics are inferred as higher-is-
#'   better; all other metrics default to lower-is-better.
#' @param fold_summary Function used to reduce multiple metric rows within an
#'   outer fold; one of `"mean"` or `"median"`.
#' @param conf_level Bootstrap confidence level, recorded as
#'   `confidence_level` in the returned Scorecard.
#' @param bootstrap Logical scalar. When `TRUE`, calculate paired clustered
#'   bootstrap intervals and, where defined by the selected method, p-values.
#'   When `FALSE`, return point estimates without resampling.
#' @param cluster_names Character vector naming columns whose joint values
#'   identify one resampling cluster. `NULL` resolves the Sentinel split column
#'   (or an unambiguous holdout identifier for a plain result table).
#' @param strata_names Character vector naming columns within which clusters
#'   are resampled separately. Use `character(0)` for no stratification.
#' @param bootstrap_method Bootstrap interval algorithm: `"studentized"`,
#'   `"bca"`, `"percentile"`, `"basic"`, or `"normal"`.
#' @param bootstrap_adjustment Multiplicity adjustment: `"max_t"`,
#'   `"bonferroni"`, or `"none"`. `"max_t"` requires a studentized
#'   bootstrap.
#' @param n_realizations Number of bootstrap realizations.
#' @param seed Bootstrap seed.
#'
#' @return A [Scorecard] with baseline-paired importance estimates and fold
#'   comparisons.
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
#' @keywords internal
#' @noRd
summarize_sentinel_ablation <- function(x,
                                        metric = "error_abs_log",
                                        baseline_label = "baseline",
                                        direction = NULL,
                                        fold_summary = c("mean", "median"),
                                        conf_level = 0.95,
                                        bootstrap = FALSE,
                                        cluster_names = NULL,
                                        strata_names = "outer_fold_id",
                                        bootstrap_method = c("studentized", "bca", "percentile", "basic", "normal"),
                                        bootstrap_adjustment = c("max_t", "bonferroni", "none"),
                                        n_realizations = 1000L,
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
  if (!is.logical(bootstrap) || length(bootstrap) != 1L || is.na(bootstrap)) {
    stop("`bootstrap` must be TRUE or FALSE.", call. = FALSE)
  }
  bootstrap_method <- match.arg(bootstrap_method)
  bootstrap_adjustment <- match.arg(bootstrap_adjustment)
  if (identical(bootstrap_adjustment, "max_t") && !identical(bootstrap_method, "studentized")) {
    stop("`bootstrap_adjustment = \"max_t\"` requires `bootstrap_method = \"studentized\"`.", call. = FALSE)
  }
  if (!is.numeric(n_realizations) || length(n_realizations) != 1L || !is.finite(n_realizations) ||
    n_realizations != as.integer(n_realizations) || n_realizations < 2L) {
    stop("`n_realizations` must be one integer >= 2.", call. = FALSE)
  }
  n_realizations <- as.integer(n_realizations)
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) || seed != as.integer(seed)) {
    stop("`seed` must be one finite integer.", call. = FALSE)
  }
  seed <- as.integer(seed)

  strata_names <- sentinel_inference_names(
    strata_names,
    arg = "strata_names",
    allow_null = FALSE,
    allow_empty = TRUE
  )
  cluster_names <- sentinel_ablation_cluster_names(x, results, cluster_names)
  results <- sentinel_ablation_add_source_columns(
    x,
    results,
    unique(c(cluster_names, strata_names))
  )
  missing_cluster <- setdiff(cluster_names, names(results))
  missing_strata <- setdiff(strata_names, names(results))
  if (length(missing_cluster) > 0L) {
    stop(sprintf(
      "Cluster column(s) were not found in Sentinel results: %s.",
      paste(missing_cluster, collapse = ", ")
    ), call. = FALSE)
  }
  if (length(missing_strata) > 0L) {
    stop(sprintf(
      "Strata column(s) were not found in Sentinel results: %s.",
      paste(missing_strata, collapse = ", ")
    ), call. = FALSE)
  }
  if (any(!stats::complete.cases(results[cluster_names]))) {
    stop("Bootstrap cluster identifiers cannot be missing.", call. = FALSE)
  }
  if (length(strata_names) > 0L && any(!stats::complete.cases(results[strata_names]))) {
    stop("Bootstrap stratum identifiers cannot be missing.", call. = FALSE)
  }

  context_cols <- intersect(
    c("deployment_target", "split_mode", "repeat_id", "outer_fold_id", "holdout_id"),
    names(results)
  )
  if (!"holdout_id" %in% context_cols && !"outer_fold_id" %in% context_cols) {
    stop("Sentinel results require `holdout_id` or `outer_fold_id` for paired ablation analysis.", call. = FALSE)
  }
  target_cols <- intersect(c("anchor_model_id", "model_id", "model_id_chr"), names(results))
  if (length(target_cols) > 1L) {
    target_cols <- target_cols[[1]]
  }
  pair_cols <- unique(c(context_cols, target_cols, cluster_names, strata_names))
  metadata_cols <- intersect(
    c("scenario_type", "ablated_traits", "ablation_component"),
    names(results)
  )
  group_cols <- unique(c("scenario", metadata_cols, pair_cols))
  fold_fun <- if (identical(fold_summary, "mean")) {
    function(z) mean(z, na.rm = TRUE)
  } else {
    function(z) stats::median(z, na.rm = TRUE)
  }

  paired_values <- results |>
    dplyr::filter(is.finite(.data[[metric]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(metric_value = fold_fun(.data[[metric]]), .groups = "drop")
  if (nrow(paired_values) == 0L) {
    return(sentinel_scorecard(summary_type = "sentinel_ablation"))
  }

  baseline <- paired_values |>
    dplyr::filter(as.character(.data$scenario) == baseline_label) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(pair_cols))) |>
    dplyr::summarise(baseline_value = mean(.data$metric_value, na.rm = TRUE), .groups = "drop")
  ablated <- paired_values |>
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
      detail_tbl = paired_values,
      summary_type = "sentinel_ablation"
    ))
  }

  cluster_group_cols <- unique(c("scenario", metadata_cols, cluster_names, strata_names))
  cluster_values <- ablated |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cluster_group_cols))) |>
    dplyr::summarise(
      metric_value = fold_fun(.data$metric_value),
      baseline_value = fold_fun(.data$baseline_value),
      importance = fold_fun(.data$importance),
      n_atomic_pairs = dplyr::n(),
      .groups = "drop"
    )

  key_cols <- unique(c(strata_names, cluster_names))
  key_tbl <- cluster_values |>
    dplyr::select(dplyr::all_of(key_cols)) |>
    dplyr::distinct()
  key_tbl$.sentinel_cluster_key <- sentinel_joint_key(key_tbl, cluster_names)
  key_tbl$.sentinel_stratum_key <- sentinel_joint_key(key_tbl, strata_names)
  cluster_values$.sentinel_joint_key <- sentinel_joint_key(cluster_values, key_cols)
  key_tbl$.sentinel_joint_key <- sentinel_joint_key(key_tbl, key_cols)

  scenarios <- unique(as.character(cluster_values$scenario))
  effects <- matrix(
    NA_real_,
    nrow = nrow(key_tbl),
    ncol = length(scenarios),
    dimnames = list(NULL, scenarios)
  )
  effect_rows <- match(cluster_values$.sentinel_joint_key, key_tbl$.sentinel_joint_key)
  effect_cols <- match(as.character(cluster_values$scenario), scenarios)
  effects[cbind(effect_rows, effect_cols)] <- as.numeric(cluster_values$importance)
  strata_key <- key_tbl$.sentinel_stratum_key
  observed <- vapply(seq_along(scenarios), function(j) {
    sentinel_stratified_mean_se(effects[, j], strata_key)
  }, numeric(3))
  if (is.null(dim(observed))) {
    observed <- matrix(observed, nrow = 3L)
  }
  rownames(observed) <- c("estimate", "se", "n")

  metadata <- purrr::map_dfr(scenarios, function(scenario_now) {
    one <- cluster_values[as.character(cluster_values$scenario) == scenario_now, , drop = FALSE]
    traits <- if ("ablated_traits" %in% names(one)) {
      unique(stats::na.omit(as.character(one$ablated_traits)))
    } else {
      character(0)
    }
    scenario_types <- if ("scenario_type" %in% names(one)) {
      unique(stats::na.omit(as.character(one$scenario_type)))
    } else {
      character(0)
    }
    components <- if ("ablation_component" %in% names(one)) {
      unique(stats::na.omit(as.character(one$ablation_component)))
    } else {
      character(0)
    }
    tibble::tibble(
      scenario = scenario_now,
      scenario_type = if (length(scenario_types) > 0L) scenario_types[[1]] else "custom",
      ablated_traits = paste(traits[nzchar(traits)], collapse = "|"),
      ablation_component = if (length(components[nzchar(components)]) > 0L) {
        components[nzchar(components)][[1]]
      } else {
        ""
      },
      baseline_mean = mean(one$baseline_value, na.rm = TRUE),
      ablated_mean = mean(one$metric_value, na.rm = TRUE),
      n_atomic_pairs = sum(one$n_atomic_pairs),
      n_clusters = nrow(one)
    )
  })

  conf_low <- rep(NA_real_, length(scenarios))
  conf_high <- rep(NA_real_, length(scenarios))
  p_value <- rep(NA_real_, length(scenarios))
  p_adjusted <- rep(NA_real_, length(scenarios))
  valid_realizations <- rep(0L, length(scenarios))
  inference_status <- rep(if (bootstrap) "pending" else "not_requested", length(scenarios))
  names(conf_low) <- names(conf_high) <- names(p_value) <- names(p_adjusted) <- scenarios
  names(inference_status) <- scenarios

  if (bootstrap) {
    set.seed(seed)
    stratum_rows <- split(seq_len(nrow(key_tbl)), strata_key)
    bootstrap_estimate <- matrix(NA_real_, nrow = n_realizations, ncol = length(scenarios))
    bootstrap_se <- matrix(NA_real_, nrow = n_realizations, ncol = length(scenarios))
    for (b in seq_len(n_realizations)) {
      sampled_rows <- unlist(lapply(stratum_rows, function(rows) {
        sample(rows, length(rows), replace = TRUE)
      }), use.names = FALSE)
      sampled_strata <- strata_key[sampled_rows]
      for (j in seq_along(scenarios)) {
        stats_now <- sentinel_stratified_mean_se(effects[sampled_rows, j], sampled_strata)
        bootstrap_estimate[b, j] <- stats_now[["estimate"]]
        bootstrap_se[b, j] <- stats_now[["se"]]
      }
    }
    colnames(bootstrap_estimate) <- colnames(bootstrap_se) <- scenarios
    valid_realizations <- colSums(is.finite(bootstrap_estimate))
    alpha_family <- 1 - conf_level
    tail_alpha <- if (identical(bootstrap_adjustment, "bonferroni")) {
      alpha_family / (2 * length(scenarios))
    } else {
      alpha_family / 2
    }

    if (identical(bootstrap_method, "studentized")) {
      invalid_se <- !is.finite(observed["se", ]) | observed["se", ] <= 0
      t_boot <- sweep(bootstrap_estimate, 2L, observed["estimate", ], "-") / bootstrap_se
      t_boot[!is.finite(t_boot)] <- NA_real_
      t_observed <- observed["estimate", ] / observed["se", ]
      active <- !invalid_se
      valid_realizations <- colSums(is.finite(t_boot))
      inference_status[active] <- "ok"
      degenerate_zero <- invalid_se & is.finite(observed["estimate", ]) &
        abs(observed["estimate", ]) <= sqrt(.Machine$double.eps)
      if (any(degenerate_zero)) {
        conf_low[degenerate_zero] <- observed["estimate", degenerate_zero]
        conf_high[degenerate_zero] <- observed["estimate", degenerate_zero]
        p_value[degenerate_zero] <- 1
        p_adjusted[degenerate_zero] <- 1
        valid_realizations[degenerate_zero] <- 0L
        inference_status[degenerate_zero] <- "degenerate_zero"
      }
      inference_status[invalid_se & !degenerate_zero] <- "not_estimable"
      if (identical(bootstrap_adjustment, "max_t") && any(active)) {
        complete_t <- stats::complete.cases(t_boot[, active, drop = FALSE])
        if (!any(complete_t)) {
          stop("No complete studentized bootstrap realizations were available for max-t adjustment.", call. = FALSE)
        }
        maxima <- apply(abs(t_boot[complete_t, active, drop = FALSE]), 1L, max)
        critical <- as.numeric(stats::quantile(maxima, probs = conf_level, names = FALSE, na.rm = TRUE))
        conf_low[active] <- observed["estimate", active] - critical * observed["se", active]
        conf_high[active] <- observed["estimate", active] + critical * observed["se", active]
        for (j in which(active)) {
          p_value[[j]] <- sentinel_bootstrap_tail_probability(t_boot[, j], t_observed[[j]])
          p_adjusted[[j]] <- sentinel_bootstrap_tail_probability(maxima, t_observed[[j]])
        }
      } else {
        for (j in seq_along(scenarios)) {
          if (!active[[j]]) {
            next
          }
          probability <- if (identical(bootstrap_adjustment, "bonferroni")) {
            1 - alpha_family / length(scenarios)
          } else {
            conf_level
          }
          critical <- as.numeric(stats::quantile(abs(t_boot[, j]), probs = probability, names = FALSE, na.rm = TRUE))
          conf_low[[j]] <- observed["estimate", j] - critical * observed["se", j]
          conf_high[[j]] <- observed["estimate", j] + critical * observed["se", j]
          p_value[[j]] <- sentinel_bootstrap_tail_probability(t_boot[, j], t_observed[[j]])
          p_adjusted[[j]] <- if (identical(bootstrap_adjustment, "bonferroni")) {
            min(1, length(scenarios) * p_value[[j]])
          } else {
            p_value[[j]]
          }
        }
      }
    } else if (identical(bootstrap_method, "normal")) {
      bootstrap_sd <- apply(bootstrap_estimate, 2L, stats::sd, na.rm = TRUE)
      critical <- stats::qnorm(1 - tail_alpha)
      conf_low <- observed["estimate", ] - critical * bootstrap_sd
      conf_high <- observed["estimate", ] + critical * bootstrap_sd
      z_observed <- observed["estimate", ] / bootstrap_sd
      p_value <- 2 * stats::pnorm(-abs(z_observed))
      p_adjusted <- if (identical(bootstrap_adjustment, "bonferroni")) {
        pmin(1, length(scenarios) * p_value)
      } else {
        p_value
      }
      inference_status[] <- ifelse(is.finite(conf_low) & is.finite(conf_high), "ok", "not_estimable")
    } else if (identical(bootstrap_method, "bca")) {
      if (nrow(key_tbl) < 3L) {
        stop("BCa intervals require at least three resampling clusters.", call. = FALSE)
      }
      stratum_sizes <- table(strata_key)
      if (any(stratum_sizes < 2L)) {
        stop("BCa intervals require at least two resampling clusters in every stratum.", call. = FALSE)
      }
      stratum_weights <- as.numeric(stratum_sizes) / sum(stratum_sizes)
      names(stratum_weights) <- names(stratum_sizes)
      jackknife <- matrix(NA_real_, nrow = nrow(key_tbl), ncol = length(scenarios))
      for (i in seq_len(nrow(key_tbl))) {
        keep <- setdiff(seq_len(nrow(key_tbl)), i)
        for (j in seq_along(scenarios)) {
          jackknife[i, j] <- sentinel_fixed_stratum_mean(
            effects[keep, j],
            strata_key[keep],
            stratum_weights
          )
        }
      }
      for (j in seq_along(scenarios)) {
        boot_j <- bootstrap_estimate[is.finite(bootstrap_estimate[, j]), j]
        jack_j <- jackknife[is.finite(jackknife[, j]), j]
        if (length(boot_j) == 0L || length(jack_j) < 3L) {
          next
        }
        proportion_less <- (sum(boot_j < observed["estimate", j]) + 0.5) / (length(boot_j) + 1)
        z0 <- stats::qnorm(proportion_less)
        jack_mean <- mean(jack_j)
        influence <- jack_mean - jack_j
        denominator <- 6 * sum(influence^2)^(3 / 2)
        acceleration <- if (is.finite(denominator) && denominator > 0) sum(influence^3) / denominator else 0
        adjusted_probability <- function(probability) {
          z <- stats::qnorm(probability)
          stats::pnorm(z0 + (z0 + z) / (1 - acceleration * (z0 + z)))
        }
        probabilities <- adjusted_probability(c(tail_alpha, 1 - tail_alpha))
        interval <- stats::quantile(boot_j, probs = probabilities, names = FALSE, na.rm = TRUE)
        conf_low[[j]] <- interval[[1]]
        conf_high[[j]] <- interval[[2]]
      }
      inference_status[] <- ifelse(is.finite(conf_low) & is.finite(conf_high), "ok", "not_estimable")
    } else {
      for (j in seq_along(scenarios)) {
        boot_j <- bootstrap_estimate[is.finite(bootstrap_estimate[, j]), j]
        if (length(boot_j) == 0L) {
          next
        }
        interval <- stats::quantile(boot_j, probs = c(tail_alpha, 1 - tail_alpha), names = FALSE, na.rm = TRUE)
        if (identical(bootstrap_method, "basic")) {
          interval <- c(
            2 * observed["estimate", j] - interval[[2]],
            2 * observed["estimate", j] - interval[[1]]
          )
        }
        conf_low[[j]] <- interval[[1]]
        conf_high[[j]] <- interval[[2]]
      }
      inference_status[] <- ifelse(is.finite(conf_low) & is.finite(conf_high), "ok", "not_estimable")
    }
  }

  summary_tbl <- metadata |>
    dplyr::mutate(
      metric = .env$metric,
      direction = .env$direction,
      n_pairs = as.integer(observed["n", match(.data$scenario, scenarios)]),
      importance = as.numeric(observed["estimate", match(.data$scenario, scenarios)]),
      importance_se = as.numeric(observed["se", match(.data$scenario, scenarios)]),
      conf_low = as.numeric(conf_low[match(.data$scenario, scenarios)]),
      conf_high = as.numeric(conf_high[match(.data$scenario, scenarios)]),
      p_value = as.numeric(p_value[match(.data$scenario, scenarios)]),
      p_adjusted = as.numeric(p_adjusted[match(.data$scenario, scenarios)]),
      confidence_level = .env$conf_level,
      bootstrap = .env$bootstrap,
      bootstrap_method = if (.env$bootstrap) .env$bootstrap_method else NA_character_,
      bootstrap_adjustment = if (.env$bootstrap) .env$bootstrap_adjustment else NA_character_,
      n_realizations = if (.env$bootstrap) .env$n_realizations else 0L,
      n_valid_realizations = as.integer(valid_realizations[match(.data$scenario, scenarios)]),
      inference_status = as.character(inference_status[match(.data$scenario, scenarios)]),
      cluster_names = paste(.env$cluster_names, collapse = "|"),
      strata_names = paste(.env$strata_names, collapse = "|"),
      interval_excludes_zero = is.finite(.data$conf_low) & is.finite(.data$conf_high) &
        (.data$conf_low > 0 | .data$conf_high < 0),
      importance_pct = dplyr::if_else(
        is.finite(.data$baseline_mean) & abs(.data$baseline_mean) > .Machine$double.eps,
        100 * .data$importance / abs(.data$baseline_mean),
        NA_real_
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$importance), .data$scenario)

  sentinel_scorecard(
    summary_tbl = summary_tbl,
    detail_tbl = cluster_values |>
      dplyr::select(-dplyr::any_of(".sentinel_joint_key")),
    summary_type = "sentinel_ablation"
  )
}

#' Summarize the end-to-end, oracle, and selection-regret ablation decomposition
#'
#' Applies the same paired estimator and bootstrap design to
#' `error_abs_log`, `oracle_abs_log_error`, and
#' `selection_regret_abs_log`. For mean-based summaries, the point estimates
#' obey `delta_raw = delta_oracle + delta_regret` because selection regret is
#' defined row-wise as raw error minus oracle error.
#'
#' @inheritParams summarize_sentinel_ablation
#'
#' @return A [Scorecard] whose recommendation cards contain one row per
#'   ablation and decomposition component.
#'
#' @keywords internal
#' @noRd
summarize_sentinel_ablation_decomposition <- function(
  x,
  baseline_label = "baseline",
  fold_summary = c("mean", "median"),
  conf_level = 0.95,
  bootstrap = FALSE,
  cluster_names = NULL,
  strata_names = "outer_fold_id",
  bootstrap_method = c("studentized", "bca", "percentile", "basic", "normal"),
  bootstrap_adjustment = c("max_t", "bonferroni", "none"),
  n_realizations = 1000L,
  seed = 1L
) {
  fold_summary <- match.arg(fold_summary)
  bootstrap_method <- match.arg(bootstrap_method)
  bootstrap_adjustment <- match.arg(bootstrap_adjustment)
  results <- sentinel_results_data(x)
  metrics <- c(
    raw = "error_abs_log",
    oracle = "oracle_abs_log_error",
    regret = "selection_regret_abs_log"
  )
  missing_metrics <- setdiff(unname(metrics), names(results))
  if (length(missing_metrics) > 0L) {
    stop(
      sprintf(
        "Ablation decomposition requires metric column(s): %s.",
        paste(missing_metrics, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  component_labels <- c(
    raw = "End-to-end error",
    oracle = "Oracle error",
    regret = "Selection regret"
  )
  cards <- vector("list", length(metrics))
  details <- vector("list", length(metrics))
  for (i in seq_along(metrics)) {
    component_now <- names(metrics)[[i]]
    metric_now <- unname(metrics[[i]])
    scorecard_now <- summarize_sentinel_ablation(
      results,
      metric = metric_now,
      baseline_label = baseline_label,
      direction = "lower",
      fold_summary = fold_summary,
      conf_level = conf_level,
      bootstrap = bootstrap,
      cluster_names = cluster_names,
      strata_names = strata_names,
      bootstrap_method = bootstrap_method,
      bootstrap_adjustment = bootstrap_adjustment,
      n_realizations = n_realizations,
      seed = seed
    )
    cards[[i]] <- tibble::as_tibble(scorecard_now@recommendation_cards) |>
      dplyr::select(-dplyr::any_of("summary_type")) |>
      dplyr::mutate(
        component = component_now,
        component_label = unname(component_labels[[component_now]]),
        component_order = i
      )
    details[[i]] <- tibble::as_tibble(scorecard_now@selection_diagnostics) |>
      dplyr::select(-dplyr::any_of("summary_type")) |>
      dplyr::mutate(
        metric = metric_now,
        component = component_now,
        component_label = unname(component_labels[[component_now]]),
        component_order = i
      )
  }

  summary_tbl <- dplyr::bind_rows(cards)
  detail_tbl <- dplyr::bind_rows(details)
  identity_tbl <- summary_tbl |>
    dplyr::select("scenario", "component", "importance") |>
    tidyr::pivot_wider(names_from = "component", values_from = "importance") |>
    dplyr::mutate(
      decomposition_residual = .data$raw - .data$oracle - .data$regret
    ) |>
    dplyr::select("scenario", "decomposition_residual")
  summary_tbl <- summary_tbl |>
    dplyr::left_join(identity_tbl, by = "scenario") |>
    dplyr::arrange(.data$component_order, dplyr::desc(.data$importance), .data$scenario)

  sentinel_scorecard(
    summary_tbl = summary_tbl,
    detail_tbl = detail_tbl,
    summary_type = "sentinel_ablation_decomposition"
  )
}

#' Plot the Sentinel ablation decomposition
#'
#' @param x A Sentinel-ablation-decomposition [Scorecard].
#' @param labels Optional named character vector mapping scenario names to
#'   display labels.
#'
#' @return A ggplot object.
#'
plot_sentinel_ablation_decomposition_scorecard <- function(x,
                                                           labels = NULL) {
  summary_tbl <- tibble::as_tibble(x@anchor_audit)
  if (nrow(summary_tbl) == 0L) {
    summary_tbl <- tibble::as_tibble(x@recommendation_cards)
  }
  required <- c("scenario", "component", "component_label", "importance")
  missing_required <- setdiff(required, names(summary_tbl))
  if (nrow(summary_tbl) == 0L || length(missing_required) > 0L) {
    stop(
      "No three-component Sentinel ablation decomposition was available to plot.",
      call. = FALSE
    )
  }

  component_labels <- summary_tbl$ablation_component %||% rep("", nrow(summary_tbl))
  trait_labels <- summary_tbl$ablated_traits %||% rep("", nrow(summary_tbl))
  display_label <- ifelse(
    !is.na(component_labels) & nzchar(component_labels),
    gsub("_", " ", component_labels),
    ifelse(
      !is.na(trait_labels) & nzchar(trait_labels),
      gsub("_", " ", gsub("\\|", ", ", trait_labels)),
      gsub("_", " ", summary_tbl$scenario)
    )
  )
  if (!is.null(labels)) {
    mapped <- unname(labels[as.character(summary_tbl$scenario)])
    display_label[!is.na(mapped)] <- mapped[!is.na(mapped)]
  }
  display_label <- ifelse(
    nzchar(display_label),
    paste0(toupper(substr(display_label, 1L, 1L)), substr(display_label, 2L, nchar(display_label))),
    display_label
  )
  raw_order <- summary_tbl |>
    dplyr::filter(.data$component == "raw") |>
    dplyr::arrange(.data$importance) |>
    dplyr::pull(.data$scenario)
  if (length(raw_order) == 0L) {
    raw_order <- unique(as.character(summary_tbl$scenario))
  }
  scenario_labels <- stats::setNames(
    display_label[match(raw_order, summary_tbl$scenario)],
    raw_order
  )
  summary_tbl$display_label <- factor(
    unname(scenario_labels[as.character(summary_tbl$scenario)]),
    levels = unname(scenario_labels)
  )
  summary_tbl$component_label <- factor(
    as.character(summary_tbl$component_label),
    levels = c("End-to-end error", "Oracle error", "Selection regret")
  )

  has_intervals <- "conf_low" %in% names(summary_tbl) &&
    "conf_high" %in% names(summary_tbl)
  finite_interval <- if (has_intervals) {
    is.finite(summary_tbl$conf_low) & is.finite(summary_tbl$conf_high)
  } else {
    rep(FALSE, nrow(summary_tbl))
  }
  summary_tbl$interval_result <- ifelse(
    finite_interval & summary_tbl$conf_low > 0,
    "Statistically > 0",
    ifelse(
      finite_interval & summary_tbl$conf_high < 0,
      "Statistically < 0",
      "Not distinguishable from 0"
    )
  )
  summary_tbl$interval_result <- factor(
    summary_tbl$interval_result,
    levels = c("Statistically > 0", "Statistically < 0", "Not distinguishable from 0")
  )
  result_colors <- c(
    "Statistically > 0" = "palegreen3",
    "Statistically < 0" = "tomato3",
    "Not distinguishable from 0" = "azure3"
  )

  plot_object <- ggplot2::ggplot(
    summary_tbl,
    ggplot2::aes(y = .data$display_label, x = .data$importance)
  ) +
    ggplot2::geom_vline(xintercept = 0, color = "grey55", linewidth = 0.5, linetype = 2)
  if (any(finite_interval)) {
    plot_object <- plot_object +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = .data$conf_low,
          xend = .data$conf_high,
          yend = .data$display_label,
          color = .data$interval_result
        ),
        linewidth = 0.8
      )
  }
  plot_object +
    ggplot2::geom_point(ggplot2::aes(color = .data$interval_result), size = 2.7) +
    ggplot2::scale_color_manual(
      values = result_colors,
      limits = names(result_colors),
      drop = FALSE,
      name = NULL
    ) +
    ggplot2::facet_grid(. ~ component_label) +
    ggplot2::labs(
      title = NULL,
      subtitle = NULL,
      x = "Change in absolute log error (ablated - baseline)",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
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
  component_labels <- summary_tbl$ablation_component %||% rep("", nrow(summary_tbl))
  trait_labels <- summary_tbl$ablated_traits %||% rep("", nrow(summary_tbl))
  display_label <- ifelse(
    !is.na(component_labels) & nzchar(component_labels),
    gsub("_", " ", component_labels),
    ifelse(
      !is.na(trait_labels) & nzchar(trait_labels),
      gsub("_", " ", gsub("\\|", ", ", trait_labels)),
      gsub("_", " ", summary_tbl$scenario)
    )
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
  display_label <- ifelse(
    nzchar(display_label),
    paste0(toupper(substr(display_label, 1L, 1L)), substr(display_label, 2L, nchar(display_label))),
    display_label
  )
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

  plot_object <- ggplot2::ggplot(summary_tbl, ggplot2::aes(y = .data$display_label, x = .data$importance)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey55", linewidth = 0.5, linetype = 2)
  has_intervals <- "conf_low" %in% names(summary_tbl) && "conf_high" %in% names(summary_tbl) &&
    any(is.finite(summary_tbl$conf_low) & is.finite(summary_tbl$conf_high))
  finite_interval <- if (has_intervals) {
    is.finite(summary_tbl$conf_low) & is.finite(summary_tbl$conf_high)
  } else {
    rep(FALSE, nrow(summary_tbl))
  }
  summary_tbl$interval_result <- ifelse(
    finite_interval & summary_tbl$conf_low > 0,
    "Statistically > 0",
    ifelse(
      finite_interval & summary_tbl$conf_high < 0,
      "Statistically < 0",
      "Not distinguishable from 0"
    )
  )
  summary_tbl$interval_result <- factor(
    summary_tbl$interval_result,
    levels = c("Statistically > 0", "Statistically < 0", "Not distinguishable from 0")
  )
  result_colors <- c(
    "Statistically > 0" = "palegreen3",
    "Statistically < 0" = "tomato3",
    "Not distinguishable from 0" = "azure3"
  )
  if (has_intervals) {
    plot_object <- plot_object +
      ggplot2::geom_segment(
        data = summary_tbl,
        ggplot2::aes(
          x = .data$conf_low,
          xend = .data$conf_high,
          yend = .data$display_label,
          color = .data$interval_result
        ),
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = summary_tbl,
        ggplot2::aes(color = .data$interval_result),
        size = 2.7
      ) +
      ggplot2::scale_color_manual(
        values = result_colors,
        limits = names(result_colors),
        drop = FALSE,
        name = NULL
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(color = unname(result_colors), shape = 16, linewidth = 0.8)
        )
      )
  } else {
    plot_object <- plot_object +
      ggplot2::geom_point(
        data = summary_tbl,
        ggplot2::aes(color = .data$interval_result),
        size = 2.7
      ) +
      ggplot2::scale_color_manual(
        values = result_colors,
        limits = names(result_colors),
        drop = FALSE,
        name = NULL
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(color = unname(result_colors), shape = 16)
        )
      )
  }

  plot_object +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

#' Plot Sentinel outer-fold performance from a Scorecard
#'
#' @param x A Sentinel-validation [Scorecard].
#' @param metric Numeric metric column.
#' @param view Validation display. `"distribution"` draws an empirical
#'   cumulative distribution over held-out species, `"ranked"` draws a compact
#'   landscape rank plot, `"ranked_species"` draws the fully labeled diagnostic,
#'   and `"fold"` draws outer-fold summaries.
#' @param summary_method Reduction applied to multiple equations for the same
#'   species or fold, `"mean"` or `"median"`.
#' @param metric_scale Metric-axis transformation, `"identity"` or
#'   `"pseudo_log"`. The latter retains zero while making long-tailed errors
#'   legible.
#' @param scenario_names Optional scenarios to retain.
#' @param species_names Optional species to retain in the ranked-species view.
#' @param label_species Optional additional species names to annotate in the
#'   compact ranked view. Species selected as reference anchors by the workflow
#'   configuration are always labeled automatically.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
sentinel_validation_split_label <- function(split_mode) {
  split_mode <- as.character(split_mode)
  labels <- c(
    anchor_row_holdout = "Reference-equation holdout",
    study_holdout = "Study holdout",
    study_cell_holdout = "Study-cell holdout",
    species_holdout = "Species holdout"
  )
  out <- unname(labels[split_mode])
  missing <- is.na(out) | !nzchar(out)
  out[missing] <- gsub("_", " ", split_mode[missing], fixed = TRUE)
  out[is.na(out) | !nzchar(out)] <- "Validation"
  ifelse(
    nzchar(out),
    paste0(toupper(substr(out, 1L, 1L)), substr(out, 2L, nchar(out))),
    "Validation"
  )
}

sentinel_validation_scenario_label <- function(scenario) {
  scenario <- as.character(scenario)
  out <- gsub("_", " ", scenario, fixed = TRUE)
  out <- sub("^without ", "Without ", out)
  paste0(toupper(substr(out, 1L, 1L)), substr(out, 2L, nchar(out)))
}

sentinel_validation_metric_label <- function(metric) {
  labels <- c(
    error_abs_log = "Absolute log multiplier error",
    oracle_abs_log_error = "Oracle absolute log multiplier error",
    selection_regret_abs_log = "Selection regret (absolute log scale)",
    covered = "Coverage proportion",
    valid_prediction = "Valid-prediction proportion"
  )
  out <- unname(labels[metric])
  if (length(out) == 1L && !is.na(out)) {
    return(out)
  }
  out <- gsub("_", " ", metric, fixed = TRUE)
  paste0(toupper(substr(out, 1L, 1L)), substr(out, 2L, nchar(out)))
}

sentinel_validation_series <- function(results) {
  split_mode <- if ("split_mode" %in% names(results)) {
    as.character(results$split_mode)
  } else {
    rep("validation", nrow(results))
  }
  scenario <- if ("scenario" %in% names(results)) {
    as.character(results$scenario)
  } else {
    rep("baseline", nrow(results))
  }
  split_label <- sentinel_validation_split_label(split_mode)
  scenario_label <- sentinel_validation_scenario_label(scenario)
  ifelse(
    is.na(scenario) | !nzchar(scenario) | scenario == "baseline",
    paste0(split_label, " (all traits)"),
    paste0(split_label, " - ", scenario_label)
  )
}

sentinel_validation_species_values <- function(results,
                                               metric,
                                               summary_method) {
  species_col <- if ("anchor_species" %in% names(results) &&
    any(!is.na(results$anchor_species) & nzchar(as.character(results$anchor_species)))) {
    "anchor_species"
  } else if ("holdout_id" %in% names(results)) {
    "holdout_id"
  } else {
    stop(
      "Species-level validation plots require `anchor_species` or `holdout_id`.",
      call. = FALSE
    )
  }
  summary_fun <- if (identical(summary_method, "mean")) mean else stats::median
  reference_flag <- if ("is_reference_anchor_species" %in% names(results)) {
    as.logical(results$is_reference_anchor_species) %in% TRUE
  } else {
    rep(FALSE, nrow(results))
  }
  results |>
    dplyr::mutate(
      .sentinel_species = as.character(.data[[species_col]]),
      .sentinel_series = sentinel_validation_series(results),
      .sentinel_metric = as.numeric(.data[[metric]]),
      .sentinel_reference_anchor = reference_flag
    ) |>
    dplyr::filter(!is.na(.data$.sentinel_species), nzchar(.data$.sentinel_species)) |>
    dplyr::group_by(.data$.sentinel_series, .data$.sentinel_species) |>
    dplyr::summarise(
      n_equations = dplyr::n(),
      n_estimable = sum(is.finite(.data$.sentinel_metric)),
      coverage = mean(is.finite(.data$.sentinel_metric)),
      is_reference_anchor_species = any(.data$.sentinel_reference_anchor),
      metric_value = if (any(is.finite(.data$.sentinel_metric))) {
        summary_fun(.data$.sentinel_metric[is.finite(.data$.sentinel_metric)], na.rm = TRUE)
      } else {
        NA_real_
      },
      metric_q25 = if (any(is.finite(.data$.sentinel_metric))) {
        as.numeric(stats::quantile(
          .data$.sentinel_metric[is.finite(.data$.sentinel_metric)],
          probs = 0.25,
          names = FALSE,
          type = 7
        ))
      } else {
        NA_real_
      },
      metric_q75 = if (any(is.finite(.data$.sentinel_metric))) {
        as.numeric(stats::quantile(
          .data$.sentinel_metric[is.finite(.data$.sentinel_metric)],
          probs = 0.75,
          names = FALSE,
          type = 7
        ))
      } else {
        NA_real_
      },
      .groups = "drop"
    )
}

sentinel_validation_metric_scale <- function(plot,
                                             metric_scale,
                                             axis = c("x", "y")) {
  axis <- match.arg(axis)
  if (!identical(metric_scale, "pseudo_log")) {
    return(plot)
  }
  scale <- if (identical(axis, "x")) ggplot2::scale_x_continuous else ggplot2::scale_y_continuous
  plot + scale(
    trans = scales::pseudo_log_trans(sigma = 1, base = 10),
    labels = scales::label_number(accuracy = 0.01)
  )
}

plot_sentinel_validation_scorecard <- function(x,
                                               metric = "error_abs_log",
                                               view = NULL,
                                               summary_method = c("mean", "median"),
                                               metric_scale = c("identity", "pseudo_log"),
                                               scenario_names = NULL,
                                               species_names = NULL,
                                               label_species = NULL) {
  results <- tibble::as_tibble(x@selection_diagnostics)
  if (!metric %in% names(results) || !(is.numeric(results[[metric]]) || is.logical(results[[metric]]))) {
    stop(sprintf("Numeric or logical metric column '%s' was not found in Sentinel results.", metric), call. = FALSE)
  }
  view <- match.arg(
    view %||% "distribution",
    c("distribution", "ranked", "ranked_species", "fold")
  )
  summary_method <- match.arg(summary_method)
  metric_scale <- match.arg(metric_scale)
  if (!is.null(scenario_names)) {
    if (!is.character(scenario_names) || anyNA(scenario_names) || any(!nzchar(scenario_names))) {
      stop("`scenario_names` must be NULL or non-empty character values.", call. = FALSE)
    }
    if (!"scenario" %in% names(results)) {
      stop("`scenario_names` was supplied, but the Scorecard has no `scenario` column.", call. = FALSE)
    }
    results <- dplyr::filter(results, as.character(.data$scenario) %in% scenario_names)
  }
  if (nrow(results) == 0L) {
    stop("No Sentinel validation results remained after filtering.", call. = FALSE)
  }
  metric_label <- sentinel_validation_metric_label(metric)
  if (identical(metric_scale, "pseudo_log")) {
    metric_label <- paste0(metric_label, " (pseudo-log scale)")
  }

  if (view %in% c("distribution", "ranked", "ranked_species")) {
    species_tbl <- sentinel_validation_species_values(results, metric, summary_method)
    if (!is.null(species_names)) {
      if (!is.character(species_names) || anyNA(species_names) || any(!nzchar(species_names))) {
        stop("`species_names` must be NULL or non-empty character values.", call. = FALSE)
      }
      species_tbl <- dplyr::filter(
        species_tbl,
        .data$.sentinel_species %in% species_names
      )
    }
    if (nrow(species_tbl) == 0L) {
      stop("No held-out species remained after filtering.", call. = FALSE)
    }

    if (identical(view, "distribution")) {
      distribution_tbl <- species_tbl |>
        dplyr::group_by(.data$.sentinel_series) |>
        dplyr::group_modify(function(.x, .y) {
          n_total <- nrow(.x)
          out <- .x |>
            dplyr::filter(is.finite(.data$metric_value)) |>
            dplyr::arrange(.data$metric_value)
          n_estimable <- nrow(out)
          out$cumulative_species <- seq_len(n_estimable) / n_total
          out$n_species_total <- n_total
          out$n_species_estimable <- n_estimable
          out
        }) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          series_display = paste0(
            .data$.sentinel_series,
            " (", .data$n_species_estimable, "/", .data$n_species_total,
            " species estimable)"
          )
        )
      if (nrow(distribution_tbl) == 0L) {
        stop("No held-out species had a finite validation metric.", call. = FALSE)
      }
      plot <- ggplot2::ggplot(
        distribution_tbl,
        ggplot2::aes(
          x = .data$metric_value,
          y = .data$cumulative_species,
          colour = .data$series_display,
          group = .data$series_display
        )
      ) +
        ggplot2::geom_step(direction = "hv", linewidth = 0.85) +
        ggplot2::geom_point(size = 1.25, alpha = 0.65) +
        ggplot2::scale_y_continuous(
          limits = c(0, 1),
          labels = scales::label_percent(accuracy = 1)
        ) +
        ggplot2::labs(
          title = NULL,
          subtitle = NULL,
          x = metric_label,
          y = "Cumulative proportion of held-out species",
          colour = NULL,
          caption = paste0(
            "Each species contributes equally; within-species values are ",
            summary_method,
            "s across equations. Curve height includes species without a finite estimate."
          )
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "bottom"
        )
      return(sentinel_validation_metric_scale(plot, metric_scale))
    }

    if (identical(view, "ranked")) {
      if (!is.null(label_species) &&
        (!is.character(label_species) || anyNA(label_species) || any(!nzchar(label_species)))) {
        stop("`label_species` must be NULL or non-empty character values.", call. = FALSE)
      }
      compact_tbl <- species_tbl |>
        dplyr::group_by(.data$.sentinel_series) |>
        dplyr::mutate(
          n_species_total = dplyr::n(),
          n_species_estimable = sum(is.finite(.data$metric_value))
        ) |>
        dplyr::filter(is.finite(.data$metric_value)) |>
        dplyr::arrange(.data$metric_value, .by_group = TRUE) |>
        dplyr::mutate(species_rank = dplyr::row_number()) |>
        dplyr::ungroup()
      if (nrow(compact_tbl) == 0L) {
        stop("No held-out species had a finite validation metric.", call. = FALSE)
      }
      label_species <- unique(c(
        as.character(label_species %||% character(0)),
        compact_tbl$.sentinel_species[compact_tbl$is_reference_anchor_species]
      ))
      label_tbl <- dplyr::filter(
        compact_tbl,
        .data$.sentinel_species %in% label_species
      )
      coverage_caption <- compact_tbl |>
        dplyr::distinct(
          .data$.sentinel_series,
          .data$n_species_estimable,
          .data$n_species_total
        ) |>
        dplyr::transmute(
          text = paste0(
            .data$.sentinel_series, ": ",
            .data$n_species_estimable, "/", .data$n_species_total,
            " species estimable."
          )
        ) |>
        dplyr::pull(.data$text) |>
        paste(collapse = " ")
      rank_max <- max(compact_tbl$species_rank, na.rm = TRUE)
      rank_breaks <- unique(c(
        1,
        pretty(c(1, rank_max), n = 5),
        rank_max
      ))
      rank_breaks <- rank_breaks[rank_breaks >= 1 & rank_breaks <= rank_max]

      plot <- ggplot2::ggplot(
        compact_tbl,
        ggplot2::aes(
          x = .data$species_rank,
          y = .data$metric_value,
          colour = .data$coverage,
          size = .data$n_equations
        )
      ) +
        ggplot2::geom_point(alpha = 0.9) +
        ggrepel::geom_text_repel(
          data = label_tbl,
          ggplot2::aes(label = .data$.sentinel_species),
          size = 3,
          nudge_y = 0.12,
          box.padding = 0.6,
          point.padding = 0.35,
          force = 2,
          max.iter = 20000,
          min.segment.length = 0,
          max.overlaps = Inf,
          segment.colour = "grey35",
          segment.size = 0.35,
          seed = 1,
          show.legend = FALSE
        ) +
        ggplot2::scale_colour_gradient(
          low = "tomato3",
          high = "palegreen3",
          limits = c(0, 1),
          labels = scales::label_percent(accuracy = 1),
          name = "Estimable equations"
        ) +
        ggplot2::scale_size_continuous(name = "Held-out equations") +
        ggplot2::scale_x_continuous(
          limits = c(1, rank_max),
          breaks = rank_breaks,
          expand = ggplot2::expansion(mult = c(0.01, 0.04))
        ) +
        ggplot2::labs(
          title = NULL,
          subtitle = NULL,
          x = paste0("Held-out species ranked by ", summary_method, " error"),
          y = metric_label,
          caption = coverage_caption
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "bottom"
        )
      if (dplyr::n_distinct(compact_tbl$.sentinel_series) > 1L) {
        plot <- plot + ggplot2::facet_wrap(~.sentinel_series, scales = "free_x")
      }
      return(sentinel_validation_metric_scale(plot, metric_scale, axis = "y"))
    }

    ranked_tbl <- species_tbl |>
      dplyr::filter(is.finite(.data$metric_value)) |>
      dplyr::mutate(
        species_display = stats::reorder(
          .data$.sentinel_species,
          .data$metric_value,
          FUN = mean
        )
      )
    if (nrow(ranked_tbl) == 0L) {
      stop("No held-out species had a finite validation metric.", call. = FALSE)
    }
    species_col <- if ("anchor_species" %in% names(results) &&
      any(!is.na(results$anchor_species) & nzchar(as.character(results$anchor_species)))) {
      "anchor_species"
    } else {
      "holdout_id"
    }
    equation_tbl <- results |>
      dplyr::mutate(
        .sentinel_species = as.character(.data[[species_col]]),
        .sentinel_series = sentinel_validation_series(results),
        equation_metric = as.numeric(.data[[metric]])
      ) |>
      dplyr::filter(
        .data$.sentinel_species %in% ranked_tbl$.sentinel_species,
        is.finite(.data$equation_metric)
      ) |>
      dplyr::mutate(
        species_display = factor(
          .data$.sentinel_species,
          levels = levels(ranked_tbl$species_display)
        )
      )
    omitted <- nrow(species_tbl) - nrow(ranked_tbl)
    plot <- ggplot2::ggplot(
      ranked_tbl,
      ggplot2::aes(x = .data$metric_value, y = .data$species_display)
    ) +
      ggplot2::geom_point(
        data = equation_tbl,
        ggplot2::aes(x = .data$equation_metric, y = .data$species_display),
        inherit.aes = FALSE,
        position = ggplot2::position_jitter(width = 0, height = 0.12),
        colour = "grey65",
        size = 0.85,
        alpha = 0.6
      ) +
      ggplot2::geom_point(
        ggplot2::aes(colour = .data$coverage, size = .data$n_equations),
        alpha = 0.9
      ) +
      ggplot2::scale_colour_gradient(
        low = "tomato3",
        high = "palegreen3",
        limits = c(0, 1),
        labels = scales::label_percent(accuracy = 1),
        name = "Estimable equations"
      ) +
      ggplot2::scale_size_continuous(name = "Held-out equations") +
      ggplot2::labs(
        title = NULL,
        subtitle = NULL,
        x = metric_label,
        y = NULL,
        caption = paste0(
          "Small grey points are individual equations; colored points are species ",
          summary_method, "s. ",
          omitted, " species without a finite estimate are omitted."
        )
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "bottom"
      )
    if (dplyr::n_distinct(ranked_tbl$.sentinel_series) > 1L) {
      plot <- plot + ggplot2::facet_wrap(~.sentinel_series, scales = "free_y")
    }
    return(sentinel_validation_metric_scale(plot, metric_scale))
  }

  fold_cols <- intersect(c("repeat_id", "outer_fold_id", "holdout_id"), names(results))
  if (length(fold_cols) == 0L) {
    stop("Fold validation plots require an outer-fold or holdout identifier.", call. = FALSE)
  }
  summary_fun <- if (identical(summary_method, "mean")) mean else stats::median
  plot_tbl <- results |>
    dplyr::mutate(
      .sentinel_series = sentinel_validation_series(results),
      .sentinel_metric = as.numeric(.data[[metric]])
    ) |>
    dplyr::filter(is.finite(.data$.sentinel_metric)) |>
    dplyr::group_by(.data$.sentinel_series, dplyr::across(dplyr::all_of(fold_cols))) |>
    dplyr::summarise(
      metric_value = summary_fun(.data$.sentinel_metric, na.rm = TRUE),
      .groups = "drop"
    )
  if (nrow(plot_tbl) == 0L) {
    stop("No finite Sentinel validation results were available to plot.", call. = FALSE)
  }
  plot <- ggplot2::ggplot(
    plot_tbl,
    ggplot2::aes(x = .data$.sentinel_series, y = .data$metric_value)
  ) +
    ggplot2::geom_boxplot(fill = "azure3", colour = "grey25", outlier.shape = NA, width = 0.65) +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(width = 0.12, height = 0),
      colour = "grey25",
      alpha = 0.65,
      size = 1.6
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = NULL, subtitle = NULL, x = NULL, y = metric_label) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  sentinel_validation_metric_scale(plot, metric_scale, axis = "y")
}

#' Plot Sentinel interval coverage from a Scorecard
#'
#' @param x A Sentinel-coverage [Scorecard].
#' @param estimand Coverage quantity: conditional interval coverage among
#'   estimable predictions, operational coverage over every held-out equation,
#'   or the estimability rate itself.
#'
#' @return A ggplot object.
#'
plot_sentinel_coverage_scorecard <- function(
  x,
  estimand = c("conditional", "operational", "estimability")
) {
  estimand <- match.arg(estimand)
  coverage <- tibble::as_tibble(x@recommendation_cards)
  fields <- switch(estimand,
    conditional = c("conditional_coverage", "conditional_lower", "conditional_upper"),
    operational = c("operational_coverage", "operational_lower", "operational_upper"),
    estimability = c("estimability_rate", "estimability_lower", "estimability_upper")
  )
  missing_fields <- setdiff(fields, names(coverage))
  if (length(missing_fields) > 0L) {
    stop(
      sprintf(
        "The Scorecard does not contain coverage field(s): %s.",
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  coverage <- coverage |>
    dplyr::mutate(
      estimate = as.numeric(.data[[fields[[1]]]]),
      lower = as.numeric(.data[[fields[[2]]]]),
      upper = as.numeric(.data[[fields[[3]]]]),
      series_display = sentinel_validation_series(coverage)
    ) |>
    dplyr::filter(is.finite(.data$estimate))
  if (nrow(coverage) == 0L) {
    stop("No finite Sentinel coverage results were available to plot.", call. = FALSE)
  }
  axis_label <- switch(estimand,
    conditional = "Conditional interval coverage",
    operational = "Operational interval coverage",
    estimability = "Estimable predictions"
  )
  plot <- ggplot2::ggplot(
    coverage,
    ggplot2::aes(
      x = .data$estimate,
      y = stats::reorder(.data$series_display, .data$estimate)
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
      orientation = "y",
      width = 0.18,
      colour = "grey35"
    ) +
    ggplot2::geom_point(size = 2.8, colour = "grey15") +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      labels = scales::label_percent(accuracy = 1)
    ) +
    ggplot2::labs(
      title = NULL,
      subtitle = NULL,
      x = axis_label,
      y = NULL,
      linetype = NULL,
      caption = "Intervals are row-level Wilson score intervals."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  if (!identical(estimand, "estimability") &&
    "nominal_coverage" %in% names(coverage)) {
    nominal <- unique(coverage$nominal_coverage[is.finite(coverage$nominal_coverage)])
    if (length(nominal) > 0L) {
      nominal_tbl <- tibble::tibble(
        nominal = nominal[[1]],
        reference = "Nominal coverage"
      )
      plot <- plot +
        ggplot2::geom_vline(
          data = nominal_tbl,
          ggplot2::aes(xintercept = .data$nominal, linetype = .data$reference),
          colour = "tomato3",
          linewidth = 0.7
        ) +
        ggplot2::scale_linetype_manual(
          values = c("Nominal coverage" = "dashed")
        )
    }
  }
  plot
}

#' Print a `Sentinel`
#'
#' @name print.Sentinel
#' @usage NULL
#'
#' @param x A [Sentinel] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @noRd
S7::method(print_generic, Sentinel) <- function(x, ...) {
  scenario_names <- names(x@scenario_grid)
  n_scenarios <- length(x@scenario_grid)
  n_units <- if (x@split_col %in% names(x@data)) {
    length(unique(x@data[[x@split_col]]))
  } else {
    NA_integer_
  }

  cat("Sentinel\n")
  cat("  split_mode: ", x@split_mode, "\n", sep = "")
  cat("  split_col: ", x@split_col, " (", n_units, " units)\n", sep = "")
  cat("  candidate_rows: ", nrow(x@data), "\n", sep = "")
  if (n_scenarios > 0L) {
    shown <- utils::head(scenario_names, 6L)
    more <- if (n_scenarios > length(shown)) {
      sprintf(", +%d more", n_scenarios - length(shown))
    } else {
      ""
    }
    cat("  scenarios: ", n_scenarios, " (", paste(shown, collapse = ", "), more, ")\n", sep = "")
  } else {
    cat("  scenarios: 0\n")
  }
  cat("  manifest_rows: ", nrow(x@manifest), "\n", sep = "")
  cat("  results_rows: ", nrow(x@results), "\n", sep = "")
  cat("  case_studies: ", length(x@case_studies), "\n", sep = "")
  cat("  output_dir: ", x@output_dir, "\n", sep = "")
  invisible(x)
}

#' Show a `Sentinel`
#'
#' @name show.Sentinel
#' @usage NULL
#'
#' @param object A [Sentinel] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
#' @noRd
S7::method(show_generic, Sentinel) <- function(object) {
  print(object)
  invisible(object)
}

#' Summarize a `Sentinel`
#'
#' @param object A [Sentinel] object.
#' @param type Summary type: `"validation"`, `"ablation"`,
#'   `"ablation_decomposition"`, or `"coverage"`.
#' @param ... Arguments forwarded to the selected summarizer.
#'
#' @return A [Scorecard].
#'
#' @keywords internal
#' @noRd
S7::method(summary_generic, Sentinel) <- function(object,
                                                  type = c(
                                                    "validation",
                                                    "ablation",
                                                    "ablation_decomposition",
                                                    "coverage"
                                                  ),
                                                  ...) {
  type <- match.arg(type)
  if (identical(type, "ablation")) {
    return(summarize_sentinel_ablation(object, ...))
  }
  if (identical(type, "ablation_decomposition")) {
    return(summarize_sentinel_ablation_decomposition(object, ...))
  }
  if (identical(type, "coverage")) {
    return(summarize_sentinel_coverage(object, ...))
  }
  summarize_sentinel_validation(object, ...)
}
