#' Finite-sample conformal quantile
#'
#' @param x Numeric residual vector.
#' @param alpha Miscoverage level.
#'
#' @return Numeric scalar.
#' @keywords internal
nested_conformal_quantile <- function(x,
                                      alpha) {
  x <- sort(x[is.finite(x)])
  n <- length(x)
  if (n == 0) {
    return(NA_real_)
  }

  q_idx <- ceiling((n + 1) * (1 - alpha))
  if (q_idx > n) {
    return(Inf)
  }

  x[[q_idx]]
}

#' Split species for one nested validation fold
#'
#' @param species Character vector of species names.
#' @param outer_species Species held out for final testing.
#' @param selection_fraction Fraction of non-test species assigned to policy selection.
#' @param calibration_fraction Fraction of non-test species assigned to conformal calibration.
#' @param seed Integer seed.
#'
#' @return A list with `tune`, `selection`, and `calibration` species vectors.
#' @keywords internal
split_nested_species <- function(species,
                                 outer_species,
                                 selection_fraction,
                                 calibration_fraction,
                                 seed) {
  species <- sort(unique(stats::na.omit(as.character(species))))
  remaining <- setdiff(species, outer_species)
  if (length(remaining) < 4) {
    stop("Nested validation requires at least four non-test species.", call. = FALSE)
  }

  set.seed(as.integer(seed))
  remaining <- sample(remaining, length(remaining), replace = FALSE)

  n_remaining <- length(remaining)
  n_selection <- max(1L, floor(n_remaining * selection_fraction))
  n_calibration <- max(1L, floor(n_remaining * calibration_fraction))
  if (n_selection + n_calibration > n_remaining - 2L) {
    n_calibration <- max(1L, n_remaining - n_selection - 2L)
  }

  selection <- remaining[seq_len(n_selection)]
  calibration <- remaining[seq.int(n_selection + 1L, n_selection + n_calibration)]
  tune <- setdiff(remaining, c(selection, calibration))

  if (length(tune) < 2L || length(selection) < 1L || length(calibration) < 1L) {
    stop("Nested validation split produced an empty tune, selection, or calibration set.", call. = FALSE)
  }

  list(
    tune = tune,
    selection = selection,
    calibration = calibration
  )
}

#' Build a nested-validation anchor evaluation config
#'
#' @param similarity_config Tuned similarity configuration.
#' @param base_config User/config list.
#'
#' @return A list accepted by the internal single-anchor admissibility screen.
#' @keywords internal
nested_anchor_config <- function(similarity_config,
                                 base_config) {
  list(
    species_traits = as.list(similarity_config$species_weights),
    study_traits = as.list(similarity_config$study_weights),
    alpha = similarity_config$alpha,
    k_species = similarity_config$k_species,
    k_study = similarity_config$k_study,
    length_overlap_weight = similarity_config$coherence$length_coherence$weight,
    depth_overlap_weight = similarity_config$coherence$depth_coherence$weight,
    frequency_coherence_weight = similarity_config$coherence$frequency_coherence$weight,
    frequency_coherence_mode = similarity_config$coherence$frequency_coherence$method,
    min_length_overlap_fraction = base_config$min_length_overlap_fraction,
    min_depth_overlap_fraction = base_config$min_depth_overlap_fraction,
    missing_key_metadata_max_fraction = base_config$missing_key_metadata_max_fraction,
    core_weight_cutoff = base_config$core_weight_cutoff
  )
}

#' Evaluate transfer policies on held-out anchors
#'
#' @param anchor_models Held-out anchor rows.
#' @param donor_models Donor/training rows.
#' @param policies Policy names.
#' @param similarity_config Tuned similarity configuration.
#' @param base_config Policy config.
#' @param policy_path Optional policy registry path.
#' @param registry_path Optional trait registry path.
#'
#' @return A tibble of policy predictions and residuals.
#' @keywords internal
evaluate_nested_transfer_set <- function(anchor_models,
                                         donor_models,
                                         policies,
                                         similarity_config,
                                         base_config,
                                         policy_path = NULL,
                                         registry_path = NULL) {
  if (nrow(anchor_models) == 0 || nrow(donor_models) == 0) {
    return(tibble::tibble())
  }

  eval_config <- nested_anchor_config(similarity_config, base_config)
  empty_ordination <- list(
    model_scores = tibble::tibble(model_id_chr = character(), nmds_cluster = character()),
    anchor_cluster = NA_character_,
    species_ellipse_ids = character()
  )

  purrr::map_dfr(seq_len(nrow(anchor_models)), function(i) {
    anchor_row <- anchor_models[i, , drop = FALSE]

    # Include the anchor only as a query row so Gower distances can be computed
    # from known target metadata. Self-donations are removed by the standard
    # admissibility gate.
    query_pool <- dplyr::bind_rows(
      tibble::as_tibble(donor_models),
      tibble::as_tibble(anchor_row)
    ) |>
      dplyr::distinct(model_id_chr, .keep_all = TRUE)

    eval_obj <- tryCatch(
      screen_one_anchor_admissibility(
        anchor_row = anchor_row,
        candidate_models = query_pool,
        config = eval_config,
        registry_path = registry_path
      ),
      error = function(e) NULL
    )
    if (is.null(eval_obj)) {
      return(tibble::tibble())
    }

    evaluate_policies(
      eval_obj = eval_obj,
      ordination_info = empty_ordination,
      policies = policies,
      policy_path = policy_path
    ) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        anchor_model_id = as.character(anchor_row$model_id[[1]]),
        anchor_species = as.character(anchor_row$species_name[[1]]),
        valid_prediction = is.finite(multiplier_pred) & multiplier_pred > 0,
        error_log = dplyr::if_else(valid_prediction, log(multiplier_pred), NA_real_),
        error_abs_log = abs(error_log)
      )
  })
}

#' Summarize nested policy residuals
#'
#' @param perf_tbl Nested transfer residual table.
#' @param alpha Miscoverage level.
#' @param min_n Minimum valid residual count.
#'
#' @return A policy summary table.
#' @keywords internal
summarize_nested_residuals <- function(perf_tbl,
                                       alpha,
                                       min_n = 10L) {
  perf_tbl <- tibble::as_tibble(perf_tbl)
  if (nrow(perf_tbl) == 0) {
    return(tibble::tibble())
  }
  perf_tbl$policy <- resolve_policy_names(perf_tbl)

  perf_tbl |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log)) |>
    dplyr::mutate(
      anchor_selection_local_distance = dplyr::coalesce(
        local_weighted_mean_combined_distance,
        local_min_combined_distance
      )
    ) |>
    dplyr::group_by(policy) |>
    dplyr::summarise(
      n = dplyr::n(),
      q_abs_log = nested_conformal_quantile(error_abs_log, alpha),
      interval_log_width = dplyr::if_else(is.finite(q_abs_log), 2 * q_abs_log, NA_real_),
      median_abs_log_error = stats::median(error_abs_log, na.rm = TRUE),
      mean_abs_log_error = mean(error_abs_log, na.rm = TRUE),
      median_local_combined_distance = stats::median(anchor_selection_local_distance, na.rm = TRUE),
      min_local_combined_distance = suppressWarnings(min(anchor_selection_local_distance, na.rm = TRUE)),
      median_local_effective_support = stats::median(local_effective_support, na.rm = TRUE),
      eligible_n = n >= min_n,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      median_local_combined_distance = dplyr::if_else(
        is.infinite(median_local_combined_distance),
        NA_real_,
        median_local_combined_distance
      ),
      min_local_combined_distance = dplyr::if_else(
        is.infinite(min_local_combined_distance),
        NA_real_,
        min_local_combined_distance
      ),
      median_local_effective_support = dplyr::if_else(
        is.infinite(median_local_effective_support),
        NA_real_,
        median_local_effective_support
      )
    )
}

#' Run one nested policy-validation fold
#'
#' @param task Fold task metadata.
#' @inheritParams run_nested_policy_validation
#' @param species_all All species names.
#' @param base_policy_config Policy config block.
#' @param tuning_config Tuning config block.
#'
#' @return A list of fold tables.
#' @keywords internal
run_nested_policy_fold <- function(task,
                                   candidate_models,
                                   species_all,
                                   policies,
                                   base_policy_config,
                                   tuning_config,
                                   selection_fraction,
                                   calibration_fraction,
                                   min_selection_n,
                                   min_calibration_n,
                                   selection_config = list(),
                                   alpha,
                                   seed,
                                   policy_path = NULL,
                                   registry_path = NULL) {
  split_obj <- split_nested_species(
    species = species_all,
    outer_species = task$outer_species,
    selection_fraction = selection_fraction,
    calibration_fraction = calibration_fraction,
    seed = as.integer(seed) + 1000L * task$repeat_id + task$fold_id
  )

  tune_models <- candidate_models |>
    dplyr::filter(species_name %in% split_obj$tune)
  selection_models <- candidate_models |>
    dplyr::filter(species_name %in% split_obj$selection)
  calibration_models <- candidate_models |>
    dplyr::filter(species_name %in% split_obj$calibration)
  test_models <- candidate_models |>
    dplyr::filter(species_name == task$outer_species)

  tune_obj <- tune_similarity_matrix(
    candidate_models = tune_models,
    species_traits = base_policy_config$species_traits,
    study_traits = base_policy_config$study_traits,
    alpha = base_policy_config$alpha,
    k_species = base_policy_config$k_species,
    k_study = base_policy_config$k_study,
    max_models_per_species = tuning_config$max_models_per_species %||% 2L,
    n_resamples = tuning_config$nested_n_resamples %||% NULL,
    seed = as.integer(seed) + task$fold_id,
    config = list(
      alpha_grid = base_policy_config$alpha_grid,
      k_species_grid = base_policy_config$k_species_grid,
      k_study_grid = base_policy_config$k_study_grid,
      n_cores = tuning_config$nested_inner_cores %||% 1L,
      length_coherence = list(
        method = "overlap",
        weight = base_policy_config$length_overlap_weight
      ),
      depth_coherence = list(
        method = "overlap",
        weight = base_policy_config$depth_overlap_weight
      ),
      frequency_coherence = list(
        method = base_policy_config$frequency_coherence_mode,
        weight = base_policy_config$frequency_coherence_weight
      )
    ),
    registry_path = registry_path,
    refresh = TRUE
  )

  similarity_config <- tune_obj$config_tuned

  selection_perf <- evaluate_nested_transfer_set(
    anchor_models = selection_models,
    donor_models = tune_models,
    policies = policies,
    similarity_config = similarity_config,
    base_config = base_policy_config,
    policy_path = policy_path,
    registry_path = registry_path
  ) |>
    dplyr::mutate(
      fold_id = task$fold_id,
      repeat_id = task$repeat_id,
      split = "selection",
      tuned_alpha = similarity_config$alpha,
      tuned_k_species = similarity_config$k_species,
      tuned_k_study = similarity_config$k_study
    )
  calibration_perf <- evaluate_nested_transfer_set(
    anchor_models = calibration_models,
    donor_models = tune_models,
    policies = policies,
    similarity_config = similarity_config,
    base_config = base_policy_config,
    policy_path = policy_path,
    registry_path = registry_path
  ) |>
    dplyr::mutate(
      fold_id = task$fold_id,
      repeat_id = task$repeat_id,
      split = "calibration",
      tuned_alpha = similarity_config$alpha,
      tuned_k_species = similarity_config$k_species,
      tuned_k_study = similarity_config$k_study
    )

  selection_summary <- summarize_nested_residuals(
    selection_perf,
    alpha = alpha,
    min_n = min_selection_n
  ) |>
    dplyr::mutate(
      fold_id = task$fold_id,
      repeat_id = task$repeat_id,
      tuned_alpha = similarity_config$alpha,
      tuned_k_species = similarity_config$k_species,
      tuned_k_study = similarity_config$k_study
    )
  calibration_summary <- summarize_nested_residuals(
    calibration_perf,
    alpha = alpha,
    min_n = min_calibration_n
  ) |>
    dplyr::mutate(
      fold_id = task$fold_id,
      repeat_id = task$repeat_id,
      tuned_alpha = similarity_config$alpha,
      tuned_k_species = similarity_config$k_species,
      tuned_k_study = similarity_config$k_study
    )

  eligible_selection <- selection_summary |>
    dplyr::filter(eligible_n, is.finite(interval_log_width))
  if (nrow(eligible_selection) == 0) {
    selected_policy_tbl <- tibble::tibble(
      fold_id = task$fold_id,
      repeat_id = task$repeat_id,
      selected_policy = NA_character_,
      selection_interval_log_width = NA_real_,
      tuned_alpha = similarity_config$alpha,
      tuned_k_species = similarity_config$k_species,
      tuned_k_study = similarity_config$k_study,
      selection_tier = "no_selection_policy"
    )
  } else {
    selected_policy_tbl <- eligible_selection |>
      dplyr::mutate(
        valid_prediction = TRUE,
        uncertainty_eligible = eligible_n,
        uncertainty_cost_log_width = interval_log_width,
        species_block_median_abs_log_error = median_abs_log_error,
        local_weighted_mean_combined_distance = median_local_combined_distance,
        local_min_combined_distance = min_local_combined_distance,
        local_effective_support = median_local_effective_support
      ) |>
      select_anchor_policies(
        uncertainty_rule = normalize_uncertainty_rule(selection_config$uncertainty_rule %||% "tolerance"),
        u_tol_rel = selection_config$u_tol_rel %||% selection_config$uncertainty_relative_tolerance %||% 0.25,
        u_tol_abs = selection_config$u_tol_abs %||% selection_config$uncertainty_absolute_tolerance %||% 0.05,
        local_distance_tolerance = selection_config$local_distance_tolerance %||% 1e-12
      ) |>
      dplyr::transmute(
        fold_id,
        repeat_id,
        selected_policy = policy,
        selection_interval_log_width = uncertainty_cost_log_width,
        selection_n = n,
        selection_median_abs_log_error = median_abs_log_error,
        selection_local_distance = anchor_selection_local_distance,
        tuned_alpha,
        tuned_k_species,
        tuned_k_study,
        selection_tier
      )
  }

  test_perf <- evaluate_nested_transfer_set(
    anchor_models = test_models,
    donor_models = tune_models,
    policies = policies,
    similarity_config = similarity_config,
    base_config = base_policy_config,
    policy_path = policy_path,
    registry_path = registry_path
  ) |>
    dplyr::mutate(
      fold_id = task$fold_id,
      repeat_id = task$repeat_id,
      split = "outer_test",
      tuned_alpha = similarity_config$alpha,
      tuned_k_species = similarity_config$k_species,
      tuned_k_study = similarity_config$k_study
    ) |>
    dplyr::inner_join(
      selected_policy_tbl |>
        dplyr::filter(!is.na(selected_policy)) |>
        dplyr::select(fold_id, repeat_id, selected_policy, selection_interval_log_width),
      by = c("fold_id", "repeat_id", "policy" = "selected_policy")
    ) |>
    dplyr::left_join(
      calibration_summary |>
        dplyr::select(fold_id, repeat_id, policy, calibration_n = n, q_abs_log, calibration_interval_log_width = interval_log_width),
      by = c("fold_id", "repeat_id", "policy")
    ) |>
    dplyr::mutate(
      multiplier_lo = dplyr::if_else(valid_prediction & is.finite(q_abs_log), multiplier_pred * exp(-q_abs_log), NA_real_),
      multiplier_hi = dplyr::if_else(valid_prediction & is.finite(q_abs_log), multiplier_pred * exp(q_abs_log), NA_real_),
      covered = valid_prediction & is.finite(q_abs_log) & error_abs_log <= q_abs_log
    )

  alpha_k_audit <- dplyr::bind_rows(
    selection_perf |>
      dplyr::select(fold_id, repeat_id, tuned_alpha, tuned_k_species, tuned_k_study),
    calibration_perf |>
      dplyr::select(fold_id, repeat_id, tuned_alpha, tuned_k_species, tuned_k_study),
    test_perf |>
      dplyr::select(fold_id, repeat_id, tuned_alpha, tuned_k_species, tuned_k_study)
  ) |>
    dplyr::distinct()

  if (nrow(alpha_k_audit) > 1L) {
    stop(
      sprintf(
        "Fold %s repeat %s produced multiple alpha/k settings across policies.",
        task$fold_id,
        task$repeat_id
      ),
      call. = FALSE
    )
  }

  list(
    folds = tibble::tibble(
      fold_id = task$fold_id,
      repeat_id = task$repeat_id,
      outer_species = task$outer_species,
      n_tune_species = length(split_obj$tune),
      n_selection_species = length(split_obj$selection),
      n_calibration_species = length(split_obj$calibration),
      n_test_models = nrow(test_models),
      alpha = similarity_config$alpha,
      k_species = similarity_config$k_species,
      k_study = similarity_config$k_study
    ),
    selection_summary = selection_summary,
    calibration_summary = calibration_summary,
    selected_policies = selected_policy_tbl,
    outer_test = test_perf
  )
}

#' Run nested policy validation
#'
#' Tunes similarity on a training species split, selects policies on an
#' independent policy-selection split by minimum conformal width, calibrates
#' residual quantiles on a separate calibration split, and evaluates coverage on
#' outer-held-out species.
#'
#' @param candidate_models Prepared candidate-model table.
#' @param policies Policy names to evaluate.
#' @param config Policy/tuning configuration list.
#' @param n_outer_folds Number of outer species to evaluate. `NULL` uses all species.
#' @param n_repeats Number of repeated outer-species shuffles.
#' @param selection_fraction Fraction of non-test species used for policy selection.
#' @param calibration_fraction Fraction of non-test species used for conformal calibration.
#' @param min_selection_n Minimum valid residuals required for policy selection.
#' @param min_calibration_n Minimum valid residuals required for conformal calibration.
#' @param alpha Miscoverage level.
#' @param seed Integer seed.
#' @param workers Number of outer-fold workers. Inner tuning uses
#'   `tuning$nested_inner_cores`, defaulting to one, to avoid oversubscription.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar.
#' @param policy_path Optional policy-registry path.
#' @param registry_path Optional trait-registry path.
#'
#' @return A list of nested validation tables.
#'
#' @examples
#' \dontrun{
#' nested_obj <- run_nested_policy_validation(
#'   candidate_models = selector,
#'   config = selector@config
#' )
#' nested_obj$coverage_summary
#' }
#'
#' @export
run_nested_policy_validation <- function(candidate_models,
                                         policies = NULL,
                                         config = NULL,
                                         n_outer_folds = NULL,
                                         n_repeats = 1L,
                                         selection_fraction = 0.25,
                                         calibration_fraction = 0.25,
                                         min_selection_n = 10L,
                                         min_calibration_n = 10L,
                                         alpha = 0.10,
                                         seed = NULL,
                                         workers = 1L,
                                         cache_path = NULL,
                                         refresh = FALSE,
                                         policy_path = NULL,
                                         registry_path = NULL) {
  if ((inherits(config, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
    config <- config@data
  }
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }
  selector_obj <- if ((inherits(candidate_models, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidate_models, PolicySelector), error = function(e) FALSE)))) candidate_models else NULL
  candidates_obj <- if ((inherits(candidate_models, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidate_models, Candidates), error = function(e) FALSE)))) candidate_models else NULL
  if (!is.null(selector_obj)) {
    config <- config %||% selector_obj@config
    candidates_obj <- selector_obj@candidates
    candidate_models <- selector_obj@candidates@candidate_models
  } else if (!is.null(candidates_obj)) {
    config <- config %||% list(
      policy = merge_cfg(
        default_anchor_config(list()),
        policy_selector_similarity_defaults(candidates_obj)
      )
    )
    candidate_models <- candidates_obj@candidate_models
  }
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }
  registered_policies <- policy_names(policy_path = policy_path)
  policies <- policies %||% registered_policies
  if (!is.character(policies) || any(!nzchar(policies))) {
    stop("'policies' must be a non-empty character vector.", call. = FALSE)
  }
  missing_policies <- setdiff(registered_policies, policies)
  extra_policies <- setdiff(policies, registered_policies)
  if (length(missing_policies) > 0 || length(extra_policies) > 0) {
    stop(
      sprintf(
        "Nested validation must evaluate the complete registered policy set. Missing: %s; extra: %s",
        paste(missing_policies, collapse = ", "),
        paste(extra_policies, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!is.list(config)) {
    stop("'config' must be a list.", call. = FALSE)
  }
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }
  if (!is.numeric(workers) || length(workers) != 1 || !is.finite(workers) || workers < 1) {
    stop("'workers' must be one finite number >= 1.", call. = FALSE)
  }
  workers <- as.integer(workers)

  candidate_models <- tibble::as_tibble(candidate_models)
  species_all <- sort(unique(stats::na.omit(as.character(candidate_models$species_name))))
  if (length(species_all) < 5L) {
    stop("Nested validation requires at least five species.", call. = FALSE)
  }

  n_repeats <- as.integer(n_repeats)
  if (!is.finite(n_repeats) || n_repeats < 1L) {
    stop("'n_repeats' must be one integer >= 1.", call. = FALSE)
  }
  if (is.null(n_outer_folds)) {
    n_outer_folds <- length(species_all)
  }
  n_outer_folds <- min(as.integer(n_outer_folds), length(species_all))
  if (!is.finite(n_outer_folds) || n_outer_folds < 1L) {
    stop("'n_outer_folds' must be NULL or one integer >= 1.", call. = FALSE)
  }

  base_policy_config <- config$policy %||% config
  tuning_config <- config$tuning %||% list()
  selection_config <- config$selection %||% list()

  task_rows <- list()
  fold_id <- 0L
  for (rep_id in seq_len(n_repeats)) {
    set.seed(as.integer(seed) + rep_id)
    outer_species_vec <- sample(species_all, length(species_all), replace = FALSE)
    outer_species_vec <- outer_species_vec[seq_len(n_outer_folds)]

    for (outer_species in outer_species_vec) {
      fold_id <- fold_id + 1L
      task_rows[[length(task_rows) + 1L]] <- list(
        fold_id = fold_id,
        repeat_id = rep_id,
        outer_species = outer_species
      )
    }
  }

  run_one <- function(task) {
    run_nested_policy_fold(
      task = task,
      candidate_models = candidate_models,
      species_all = species_all,
      policies = policies,
      base_policy_config = base_policy_config,
      tuning_config = tuning_config,
      selection_fraction = selection_fraction,
      calibration_fraction = calibration_fraction,
      min_selection_n = min_selection_n,
      min_calibration_n = min_calibration_n,
      selection_config = selection_config,
      alpha = alpha,
      seed = seed,
      policy_path = policy_path,
      registry_path = registry_path
    )
  }

  if (workers > 1L && length(task_rows) > 1L) {
    cluster_obj <- initialize_parallel_cluster(workers = min(workers, length(task_rows)))
    on.exit(parallel::stopCluster(cluster_obj), add = TRUE)
    tsb_cluster_export(
      cluster_obj,
      c(
        "task_rows", "candidate_models", "species_all", "policies",
        "base_policy_config", "tuning_config", "selection_fraction",
        "calibration_fraction", "min_selection_n", "min_calibration_n",
        "selection_config", "alpha", "seed", "policy_path", "registry_path",
        "run_nested_policy_fold"
      ),
      envir = environment()
    )
    fold_results <- parallel::parLapplyLB(cluster_obj, task_rows, run_one)
  } else {
    fold_results <- lapply(task_rows, run_one)
  }

  test_tbl <- dplyr::bind_rows(lapply(fold_results, `[[`, "outer_test"))
  coverage_summary <- test_tbl |>
    dplyr::filter(valid_prediction, is.finite(q_abs_log)) |>
    dplyr::group_by(policy) |>
    dplyr::summarise(
      n_test = dplyr::n(),
      empirical_coverage = mean(covered, na.rm = TRUE),
      median_interval_log_width = stats::median(calibration_interval_log_width, na.rm = TRUE),
      median_abs_log_error = stats::median(error_abs_log, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(median_interval_log_width, policy)

  result <- list(
    folds = dplyr::bind_rows(lapply(fold_results, `[[`, "folds")),
    selection_summary = dplyr::bind_rows(lapply(fold_results, `[[`, "selection_summary")),
    calibration_summary = dplyr::bind_rows(lapply(fold_results, `[[`, "calibration_summary")),
    selected_policies = dplyr::bind_rows(lapply(fold_results, `[[`, "selected_policies")),
    outer_test = test_tbl,
    coverage_summary = coverage_summary
  )

  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  result
}


