#' Run benchmark scenarios
#'
#' Reruns a caller-supplied benchmark function across a named scenario list and
#' returns a named list of scenario benchmark objects.
#'
#' @param sensitivity_specs Named list of scenario specifications. Each element must
#'   contain at least `candidate_models` and `config` (or legacy `cfg`).
#' @param benchmark_fun Benchmark function called once per scenario.
#' @param baseline_obj Optional precomputed baseline benchmark object.
#' @param benchmark_args Optional named list of extra arguments passed to
#'   `benchmark_fun` for every non-baseline scenario.
#' @param workers Number of parallel workers. Use `1` for sequential execution.
#' @param package_dir Optional package source directory used to load the
#'   development package on parallel workers when running from source.
#' @param config Optional JSON path or list with sensitivity settings.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#' @param progress Logical scalar. If `TRUE`, emit lightweight progress updates.
#'
#' @return Named list of benchmark objects.
#'
#' @export
run_sensitivity_tests <- function(sensitivity_specs,
                                  benchmark_fun,
                                  baseline_obj = NULL,
                                  benchmark_args = list(),
                                  workers = 1L,
                                  package_dir = NULL,
                                  config = NULL,
                                  cache_path = NULL,
                                  refresh = FALSE,
                                  progress = FALSE) {
  # Validate the scenario list, benchmark function, and cache settings before
  # entering the rerun loop.
  if (!is.list(sensitivity_specs) || is.null(names(sensitivity_specs)) || any(!nzchar(names(sensitivity_specs)))) {
    stop("'sensitivity_specs' must be a named list.", call. = FALSE)
  }
  if (!is.function(benchmark_fun)) {
    stop("'benchmark_fun' must be a function.", call. = FALSE)
  }
  if (!is.list(benchmark_args)) {
    stop("'benchmark_args' must be a named list.", call. = FALSE)
  }
  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(workers) || length(workers) != 1 || !is.finite(workers) || workers < 1) {
    stop("'workers' must be one finite number >= 1.", call. = FALSE)
  }
  if (!is.null(package_dir) &&
      (!is.character(package_dir) || length(package_dir) != 1 || !nzchar(package_dir))) {
    stop("'package_dir' must be NULL or a single non-empty path.", call. = FALSE)
  }
  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("'progress' must be TRUE or FALSE.", call. = FALSE)
  }

  # Reuse a cached sensitivity benchmark map when available unless a refresh
  # was explicitly requested.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Inline the small set of sensitivity defaults here instead of routing them
  # through a separate wrapper function.
  config_values <- merge_cfg(
    list(
      baseline_label = "baseline",
      tolerance = 0.05,
      n_boot = 500L
    ),
    read_similarity_config(config)
  )
  sens_map <- list()

  # Seed the baseline object first when the caller supplied one so the
  # benchmark rerun loop can skip recomputing it.
  if (!is.null(baseline_obj)) {
    sens_map[[config_values$baseline_label]] <- baseline_obj
  }

  # Rerun each non-baseline scenario through the caller-supplied benchmark
  # function.
  scenario_names <- names(sensitivity_specs)
  if (!is.null(baseline_obj)) {
    scenario_names <- setdiff(scenario_names, config_values$baseline_label)
  }
  workers <- as.integer(workers)

  if (workers <= 1L) {
    for (scenario_nm in scenario_names) {
      spec <- sensitivity_specs[[scenario_nm]]
      spec_config <- spec$config %||% spec$cfg
      if (!is.list(spec) || is.null(spec$candidate_models) || is.null(spec_config)) {
        stop(sprintf("Scenario '%s' must contain 'candidate_models' and 'config'.", scenario_nm), call. = FALSE)
      }

      bench_args <- c(
        list(
          candidate_models = spec$candidate_models,
          config = spec_config,
          cfg = spec_config,
          tolerance = config_values$tolerance,
          n_boot = config_values$n_boot
        ),
        benchmark_args
      )
      bench_args <- bench_args[names(bench_args) %in% names(formals(benchmark_fun))]

      sens_map[[scenario_nm]] <- do.call(
        what = benchmark_fun,
        args = bench_args
      )

      if (isTRUE(progress)) {
        base::message(
          "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
          "Sensitivity progress: ", length(sens_map), "/", length(sensitivity_specs),
          " scenarios completed."
        )
      }
    }
  } else if (length(scenario_names) > 0) {
    cluster_obj <- initialize_parallel_cluster(
      workers = workers,
      package_dir = package_dir
    )
    on.exit(parallel::stopCluster(cluster_obj), add = TRUE)

    parallel::clusterExport(
      cluster_obj,
      c(
        "sensitivity_specs",
        "benchmark_fun",
        "benchmark_args",
        "config_values"
      ),
      envir = environment()
    )

    if (isTRUE(progress)) {
      base::message(
        "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
        "Sensitivity reruns running in parallel with ", workers, " workers."
      )
    }

    scenario_chunks <- split(
      scenario_names,
      ceiling(seq_along(scenario_names) / workers)
    )
    processed <- length(sens_map)

    for (scenario_chunk in scenario_chunks) {
      chunk_results <- parallel::parLapplyLB(
        cluster_obj,
        scenario_chunk,
        function(scenario_nm) {
          spec <- sensitivity_specs[[scenario_nm]]
          spec_config <- spec$config %||% spec$cfg
          if (!is.list(spec) || is.null(spec$candidate_models) || is.null(spec_config)) {
            stop(sprintf("Scenario '%s' must contain 'candidate_models' and 'config'.", scenario_nm), call. = FALSE)
          }

          bench_args <- c(
            list(
              candidate_models = spec$candidate_models,
              config = spec_config,
              cfg = spec_config,
              tolerance = config_values$tolerance,
              n_boot = config_values$n_boot
            ),
            benchmark_args
          )
          bench_args <- bench_args[names(bench_args) %in% names(formals(benchmark_fun))]

          list(
            scenario = scenario_nm,
            result = do.call(what = benchmark_fun, args = bench_args)
          )
        }
      )

      for (one_result in chunk_results) {
        sens_map[[one_result$scenario]] <- one_result$result
      }

      processed <- processed + length(scenario_chunk)
      if (isTRUE(progress)) {
        base::message(
          "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
          "Sensitivity progress: ", processed, "/", length(sensitivity_specs),
          " scenarios completed."
        )
      }
    }
  }

  # Cache the full scenario benchmark map only when a cache path was supplied.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(sens_map, cache_path)
  }

  sens_map
}

#' Build a scenario manifest
#'
#' Summarizes the scenario specifications and the corresponding benchmark
#' outputs into one scenario-level manifest table.
#'
#' @param sensitivity_specs Named scenario-specification list.
#' @param sensitivity_map Named scenario benchmark map.
#' @param config Optional JSON path or list with sensitivity settings.
#'
#' @return A tibble.
#'
#' @export
build_sensitivity_table <- function(sensitivity_specs,
                                    sensitivity_map,
                                    config = NULL) {
  # Combine the scenario inputs and the benchmark outputs into one compact
  # scenario-level manifest for later audits.
  # Resolve the scenario-summary defaults directly at the call site to avoid
  # an extra config helper with only a few scalars.
  config_values <- merge_cfg(
    list(
      baseline_label = "baseline",
      tolerance = 0.05,
      n_boot = 500L
    ),
    read_similarity_config(config)
  )

  purrr::imap_dfr(sensitivity_specs, function(spec, scenario_nm) {
    bench_obj <- sensitivity_map[[scenario_nm]]
    selection_tbl <- tibble::as_tibble(bench_obj$selection_ref)
    spec_config <- spec$config %||% spec$cfg %||% list()

    # Handle undersized or failed scenarios cleanly so the manifest still
    # records the scenario inputs even when no benchmark reference could be built.
    if (nrow(selection_tbl) == 0) {
      best_policy <- NA_character_
      best_mean_error <- NA_real_
      eq_best <- tibble::tibble(policy = character(0))
    } else {
      selection_tbl$policy <- resolve_policy_names(selection_tbl)
      best_row <- selection_tbl |>
        dplyr::arrange(mean_species_median_abs_log, policy) |>
        dplyr::slice(1)
      best_policy <- best_row$policy[[1]]
      best_mean_error <- best_row$mean_species_median_abs_log[[1]]
      eq_best <- selection_tbl |>
        dplyr::filter(equivalent_to_best_global) |>
        dplyr::arrange(policy)
    }

    tibble::tibble(
      scenario = scenario_nm,
      n_candidate_models = nrow(spec$candidate_models),
      n_species = dplyr::n_distinct(spec$candidate_models$species_name),
      include_generalized_models = if ("is_group_model" %in% names(spec$candidate_models)) any(spec$candidate_models$is_group_model, na.rm = TRUE) else NA,
      alpha = spec_config$alpha %||% NA_real_,
      k_species = spec_config$k_species %||% NA_real_,
      k_study = spec_config$k_study %||% NA_real_,
      frequency_coherence_mode = spec_config$frequency_coherence_mode %||% "numeric",
      min_length_overlap_fraction = spec_config$min_length_overlap_fraction %||% NA_real_,
      min_depth_overlap_fraction = spec_config$min_depth_overlap_fraction %||% NA_real_,
      missing_key_metadata_max_fraction = spec_config$missing_key_metadata_max_fraction %||% NA_real_,
      benchmark_best_policy = best_policy,
      benchmark_best_mean_species_median_abs_log = best_mean_error,
      benchmark_equivalent_best_set = paste(eq_best$policy, collapse = "; "),
      benchmark_equivalent_best_set_n = nrow(eq_best),
      baseline_label = config_values$baseline_label
    )
  })
}

#' Bind scenario benchmark tables
#'
#' Binds selection, pairwise equivalence, equivalence-class, and conformal
#' tables across all scenario benchmark objects.
#'
#' @param sensitivity_map Named scenario benchmark map.
#'
#' @return A list of bound tibbles.
#'
#' @export
bind_sensitivity_data <- function(sensitivity_map) {
  # Bind the benchmark reference tables across scenarios while keeping the
  # scenario name as the leading key column.
  list(
    select_ref = purrr::imap_dfr(sensitivity_map, function(obj, scenario_nm) {
      tibble::as_tibble(obj$selection_ref) |>
        dplyr::mutate(scenario = scenario_nm, .before = 1)
    }),
    equiv_pairs = purrr::imap_dfr(sensitivity_map, function(obj, scenario_nm) {
      tibble::as_tibble(obj$equivalence_pairs) |>
        dplyr::mutate(scenario = scenario_nm, .before = 1)
    }),
    equiv_sets = purrr::imap_dfr(sensitivity_map, function(obj, scenario_nm) {
      tibble::as_tibble(obj$equivalence_classes) |>
        dplyr::mutate(scenario = scenario_nm, .before = 1)
    }),
    conf_cal = purrr::imap_dfr(sensitivity_map, function(obj, scenario_nm) {
      tibble::as_tibble(obj$conf_cal) |>
        dplyr::mutate(scenario = scenario_nm, .before = 1)
    })
  )
}

#' Summarize policy sensitivity
#'
#' Compares policy-selection outputs against the baseline scenario and
#' summarizes how often the selected policy, display label, and equivalent
#' policy set change.
#'
#' @param sensitivity_table Policy-sensitivity table with one row per anchor-scenario.
#' @param baseline_label Baseline scenario label.
#'
#' @return A list with `detail` and `summary`.
#'
#' @export
summarize_sensitivity <- function(sensitivity_table,
                                  baseline_label = "baseline") {
  # Join each non-baseline row to its baseline row first so all sensitivity
  # change flags are computed relative to the same reference scenario.
  out <- tibble::as_tibble(sensitivity_table)
  if (nrow(out) == 0) {
    return(list(detail = tibble::tibble(), summary = tibble::tibble()))
  }

  out$selected_policy <- resolve_selected_policy_values(out)
  out$selected_policy_display <- resolve_selected_policy_names(out)
  out$equivalent_policy_set <- resolve_equivalent_policy_sets(out)

  detail <- out |>
    dplyr::filter(scenario != baseline_label) |>
    dplyr::left_join(
      out |>
        dplyr::filter(scenario == baseline_label) |>
        dplyr::select(
          anchor_model_id,
          baseline_policy = selected_policy,
          baseline_display = selected_policy_display,
          baseline_equiv_set = equivalent_policy_set
        ),
      by = "anchor_model_id"
    ) |>
    dplyr::mutate(
      policy_changed = selected_policy != baseline_policy,
      display_changed = selected_policy_display != baseline_display,
      equiv_set_changed = equivalent_policy_set != baseline_equiv_set
    )

  # Summarize the cross-anchor change rates by scenario after the row-level
  # baseline comparison has been computed.
  summary <- detail |>
    dplyr::group_by(scenario) |>
    dplyr::summarise(
      n_anchors = dplyr::n(),
      n_policy_changed = sum(policy_changed, na.rm = TRUE),
      n_display_changed = sum(display_changed, na.rm = TRUE),
      n_equiv_set_changed = sum(equiv_set_changed, na.rm = TRUE),
      prop_policy_changed = mean(policy_changed, na.rm = TRUE),
      prop_display_changed = mean(display_changed, na.rm = TRUE),
      prop_equiv_set_changed = mean(equiv_set_changed, na.rm = TRUE),
      .groups = "drop"
    )

  list(detail = detail, summary = summary)
}

#' Build policy-sensitivity scenarios
#'
#' Builds the standard scenario list used to stress-test policy selection
#' against overlap thresholds, generalized-model availability, missingness, and
#' frequency/coherence settings.
#'
#' @param candidate_models Candidate-model table.
#' @param config Policy/admissibility config list.
#'
#' @return A named list of scenario specifications.
#'
#' @export
build_policy_sensitivity_scenarios <- function(candidate_models,
                                               config) {
  # Normalize the config first so every derived scenario starts from one
  # consistent baseline representation.
  config_values <- read_similarity_config(config)
  models_tbl <- tibble::as_tibble(candidate_models)

  # Use the same selected species/study traits as the admissibility screen to
  # compute one missingness fraction per model before any scenario filtering.
  key_cols <- unique(c(
    names(config_values$species_traits %||% list()),
    names(config_values$study_traits %||% list())
  ))
  models_tbl <- screen_missing_metadata(
    candidate_models = models_tbl,
    key_cols = key_cols
  )

  # Derive the generalized-model flag on the fly when the prepared model table
  # does not already carry it.
  if (!"is_group_model" %in% names(models_tbl)) {
    has_species <- "species_name" %in% names(models_tbl)
    has_genus <- "genus" %in% names(models_tbl)
    has_species_epithet <- "species" %in% names(models_tbl)

    models_tbl$is_group_model <- dplyr::case_when(
      has_species & is.na(models_tbl$species_name) ~ TRUE,
      has_species & !nzchar(as.character(models_tbl$species_name)) ~ TRUE,
      has_species & as.character(models_tbl$species_name) == "NA NA" ~ TRUE,
      has_genus & has_species_epithet &
        (is.na(models_tbl$genus) | !nzchar(as.character(models_tbl$genus)) |
           is.na(models_tbl$species) | !nzchar(as.character(models_tbl$species))) ~ TRUE,
      TRUE ~ FALSE
    )
  }

  zero_species_environment <- function(weight_list) {
    out <- weight_list %||% list()
    for (nm in c("temperature_midpoint", "temperature_range", "ocean_basin", "trophic")) {
      if (nm %in% names(out)) {
        out[[nm]] <- 0
      }
    }
    out
  }

  list(
    baseline = list(candidate_models = models_tbl, config = config_values),
    stricter_overlap_0_35 = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        min_length_overlap_fraction = 0.35,
        min_depth_overlap_fraction = 0.35
      ))
    ),
    stricter_overlap_0_50 = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        min_length_overlap_fraction = 0.50,
        min_depth_overlap_fraction = 0.50
      ))
    ),
    no_generalized_models = list(
      candidate_models = models_tbl |>
        dplyr::filter(!dplyr::coalesce(is_group_model, FALSE)),
      config = config_values
    ),
    no_high_missingness_0_10 = list(
      candidate_models = models_tbl |>
        dplyr::filter(dplyr::coalesce(key_metadata_missing_fraction, 0) <= 0.10),
      config = config_values
    ),
    frequency_penalty_soft = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        frequency_coherence_mode = "soft_sqrt"
      ))
    ),
    frequency_penalty_strict = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        frequency_coherence_mode = "strict_squared"
      ))
    ),
    without_species_environment = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        species_traits = zero_species_environment(config_values$species_traits)
      ))
    ),
    without_anchor_length_coherence = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        length_overlap_weight = 0
      ))
    ),
    without_anchor_depth_coherence = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        depth_overlap_weight = 0
      ))
    )
  )
}

#' Rerun one policy benchmark scenario
#'
#' Rebuilds ordination support objects, reruns the benchmark layer, recalibrates
#' conformal uncertainty, and rebuilds the global policy-selection summaries for
#' one scenario-specific candidate-model table.
#'
#' @param candidate_models Scenario-specific candidate-model table.
#' @param config Scenario-specific policy/admissibility config list.
#' @param policies Active policy names.
#' @param reference_ids Optional reference-anchor model IDs.
#' @param tolerance Practical equivalence tolerance.
#' @param n_boot Number of bootstrap resamples used in policy selection.
#' @param registry_path Optional trait-registry path.
#' @param include_ts_error Logical scalar. If `TRUE`, retain the TS-error
#'   benchmark table for the scenario rerun.
#' @param progress Logical scalar. If `TRUE`, show benchmark progress.
#'
#' @return A list containing ordination context, benchmark tables, conformal
#'   calibration, and policy-selection summaries.
#'
#' @export
run_policy_sensitivity_reference <- function(candidate_models,
                                             config,
                                             policies = NULL,
                                             reference_ids = NULL,
                                             tolerance = 0.05,
                                             n_boot = 500L,
                                             registry_path = NULL,
                                             include_ts_error = FALSE,
                                             progress = FALSE) {
  # Rebuild the similarity object for this scenario from its own policy and
  # admissibility settings so the ordination and benchmark layers stay aligned.
  config_values <- read_similarity_config(config)
  coherence_config <- list(
    length_coherence = list(method = "overlap", weight = config_values$length_overlap_weight %||% 0),
    depth_coherence = list(method = "overlap", weight = config_values$depth_overlap_weight %||% 0),
    frequency_coherence = list(
      method = config_values$frequency_coherence_mode %||% "numeric",
      weight = config_values$frequency_coherence_weight %||% 0
    )
  )

  similarity_obj <- prepare_similarity_matrix(
    candidate_models = candidate_models,
    species_traits = as.list(config_values$species_traits %||% list()),
    study_traits = as.list(config_values$study_traits %||% list()),
    alpha = config_values$alpha,
    k_species = config_values$k_species,
    k_study = config_values$k_study,
    config = coherence_config,
    registry_path = registry_path
  )

  # Recompute the model-level and species-level ordinations for the scenario so
  # any ordination-dependent policies use a scenario-consistent neighborhood.
  distance_obj <- build_gower_distances(similarity_obj)
  ordination_obj <- run_ordination(
    dist_mat = distance_obj$combined_dist,
    trait_table = candidate_models |>
      dplyr::select(dplyr::all_of(distance_obj$trait_cols))
  )
  ordination_points <- join_ordination_points(
    ordination_points = ordination_obj$points,
    candidate_models = candidate_models,
    reference_ids = reference_ids
  )
  model_scores <- extract_ordination_scores(ordination_points)
  points_missing_df <- add_ordination_missing(
    points_df = ordination_points,
    candidate_models = candidate_models,
    trait_cols = distance_obj$trait_cols
  )

  species_ordination_obj <- run_ordination(
    dist_mat = distance_obj$species_dist,
    trait_table = similarity_obj$species_profiles |>
      dplyr::select(-species_name)
  )
  species_points <- assign_ordination_groups(
    points_df = species_ordination_obj$points |>
      dplyr::rename(species_name = model_id),
    cluster_col = "species_cluster_id"
  )
  species_points <- refine_species_clusters(
    species_points_df = species_points,
    dist_mat = distance_obj$species_dist
  )$points
  species_lookup <- build_species_lookup(
    species_points_df = species_points,
    candidate_models = candidate_models
  )$lookup

  # Rerun the benchmark, conformal calibration, and policy selection from the
  # scenario-specific ordination and candidate-model state.
  benchmark_obj <- run_policy_benchmark(
    candidate_models = candidate_models,
    model_scores = model_scores,
    species_lookup = species_lookup,
    reference_ids = reference_ids,
    policies = policies,
    config = config_values,
    include_ts_error = include_ts_error,
    registry_path = registry_path,
    progress = progress
  )
  conformal_obj <- run_anchor_conformal(
    policy_perf = benchmark_obj$policy_perf,
    species_performance_table = benchmark_obj$species_block_perf,
    ts_error = benchmark_obj$policy_ts_error,
    alpha = config_values$conformal_alpha %||% 0.10
  )
  selection_obj <- run_policy_selection(
    species_performance_table = benchmark_obj$species_block_perf,
    config = list(
      tolerance = tolerance,
      n_boot = n_boot
    )
  )

  list(
    ord_ctx = list(
      model_scores = model_scores,
      species_lookup = species_lookup,
      points_missing_df = points_missing_df
    ),
    policy_perf = benchmark_obj$policy_perf,
    species_block_perf = benchmark_obj$species_block_perf,
    conf_cal = conformal_obj$conf_cal,
    selection_ref = selection_obj$final_ref,
    equivalence_pairs = selection_obj$equiv_ref$pairs,
    equivalence_classes = selection_obj$equiv_sets
  )
}
