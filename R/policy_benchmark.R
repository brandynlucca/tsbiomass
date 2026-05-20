#' Resolve one benchmark field name
#'
#' @param config Benchmark config list.
#' @param key Field-map key to resolve.
#'
#' @return Character scalar.
#'
#' @keywords internal
benchmark_field <- function(config,
                            key) {
  # Keep model-column lookup centralized so the benchmark layer can work with
  # remapped prepared-model tables.
  field_nm <- config$fields[[key]]

  if (!is.character(field_nm) || length(field_nm) != 1 || !nzchar(field_nm)) {
    stop(sprintf("Benchmark config field '%s' must be a single column name.", key), call. = FALSE)
  }

  field_nm
}

#' Normalize one reference-ID vector
#'
#' @param reference_ids Optional reference-model identifier vector.
#'
#' @return Character vector.
#'
#' @keywords internal
normalize_reference_ids <- function(reference_ids) {
  # Standardize the optional reference-model IDs once so benchmark annotations
  # do not depend on caller whitespace or duplicates.
  if (is.null(reference_ids)) {
    return(character(0))
  }

  ref_ids <- stringr::str_squish(as.character(reference_ids))
  unique(ref_ids[!is.na(ref_ids) & nzchar(ref_ids)])
}

#' Build one ordination-context object for benchmarking
#'
#' @param anchor_row One-row anchor table.
#' @param model_scores Optional model-score table from ordination output.
#' @param species_lookup Optional species lookup from ordination output.
#' @param config Benchmark config list.
#'
#' @return `NULL` or an ordination-context list.
#'
#' @keywords internal
resolve_ordination_info <- function(anchor_row,
                                    model_scores,
                                    species_lookup,
                                    config) {
  # Only attempt the ordination-dependent policies when both the model-score table
  # and the species lookup are available.
  if (is.null(model_scores) || is.null(species_lookup)) {
    return(NULL)
  }

  build_anchor_ordination(
    anchor_row = anchor_row,
    model_scores = model_scores,
    species_lookup = species_lookup,
    anchor_id_col = benchmark_field(config, "model_id"),
    score_id_col = benchmark_field(config, "model_id_chr"),
    species_col = benchmark_field(config, "species")
  )
}

#' Choose the best policy for one anchor
#'
#' @param policy_tbl Policy table for one anchor.
#'
#' @return Character scalar or `NA`.
#'
#' @keywords internal
pick_best_policy <- function(policy_tbl) {
  # Restrict the winner search to finite positive predictions so invalid
  # policies do not enter the benchmark ranking.
  valid <- tibble::as_tibble(policy_tbl)
  valid$policy <- resolve_policy_names(valid)
  valid <- valid |>
    dplyr::filter(valid_prediction)

  if (nrow(valid) == 0) {
    return(NA_character_)
  }

  valid |>
    dplyr::arrange(error_abs_log, policy) |>
    dplyr::slice(1) |>
    dplyr::pull(policy)
}

#' Build one benchmark feature row
#'
#' @param eval_obj Anchor evaluation object.
#' @param anchor_row One-row anchor table.
#' @param best_policy_name Best policy label.
#' @param is_reference Logical scalar.
#' @param config Benchmark config list.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
build_benchmark_row <- function(eval_obj,
                                anchor_row,
                                best_policy_name,
                                is_reference,
                                config) {
  # Collapse the admissible set to one row of anchor-level features used for
  # policy benchmarking and later policy-selection summaries.
  species_col <- benchmark_field(config, "species")
  family_col <- benchmark_field(config, "family")
  id_col <- benchmark_field(config, "model_id")
  admissible <- eval_obj$admissible_df

  tibble::tibble(
    anchor_model_id = as.character(anchor_row[[id_col]][[1]]),
    anchor_species = as.character(anchor_row[[species_col]][[1]]),
    anchor_family = as.character(anchor_row[[family_col]][[1]]),
    is_reference = is_reference,
    anchor_group = ifelse(is_reference, "reference", "candidate"),
    n_admissible = nrow(admissible),
    nearest_distance = if (nrow(admissible) > 0) min(admissible$combined_distance, na.rm = TRUE) else NA_real_,
    nearest_taxonomic_distance = if (nrow(admissible) > 0 && "taxonomic_distance_to_anchor" %in% names(admissible)) min(admissible$taxonomic_distance_to_anchor, na.rm = TRUE) else NA_real_,
    nearest_same_species_distance = {
      tmp <- admissible |>
        dplyr::filter(overlap_same_species)
      if (nrow(tmp) > 0) min(tmp$combined_distance, na.rm = TRUE) else NA_real_
    },
    nearest_same_family_distance = {
      tmp <- admissible |>
        dplyr::filter(overlap_same_family)
      if (nrow(tmp) > 0) min(tmp$combined_distance, na.rm = TRUE) else NA_real_
    },
    top10_weight_same_species = {
      tmp <- admissible |>
        dplyr::arrange(dplyr::desc(w_adm)) |>
        dplyr::slice_head(n = 10)
      if (nrow(tmp) > 0) sum(tmp$w_adm[tmp$overlap_same_species], na.rm = TRUE) else NA_real_
    },
    top10_weight_same_family = {
      tmp <- admissible |>
        dplyr::arrange(dplyr::desc(w_adm)) |>
        dplyr::slice_head(n = 10)
      if (nrow(tmp) > 0) sum(tmp$w_adm[tmp$overlap_same_family], na.rm = TRUE) else NA_real_
    },
    best_policy = best_policy_name
  )
}

#' Compute TS-length values
#'
#' @param slope Numeric slope vector.
#' @param intercept Numeric intercept vector.
#' @param length_cm Numeric length vector.
#'
#' @return Numeric vector.
#'
#' @keywords internal
ts_from_length <- function(slope,
                        intercept,
                        length_cm) {
  # Keep the TS-length transformation local to the benchmark layer so curve
  # error summaries do not depend on another source file being loaded.
  as.numeric(slope) * log10(length_cm) + as.numeric(intercept)
}

#' Build one policy TS-error table
#'
#' @param anchor_row One-row anchor table.
#' @param eval_obj Anchor evaluation object.
#' @param policy_tbl Policy table for the anchor.
#' @param ordination_info Optional ordination-context list.
#' @param curve_fun Policy-curve prediction function.
#' @param config Benchmark config list.
#'
#' @return A tibble.
#'
#' @keywords internal
build_ts_errors <- function(anchor_row,
                            eval_obj,
                            policy_tbl,
                            ordination_info,
                            curve_fun,
                            config) {
  # Skip the TS-length error summary entirely when no curve predictor was
  # supplied or the anchor lacks a usable standardized length-form equation.
  if (is.null(curve_fun)) {
    return(tibble::tibble())
  }

  slope_col <- benchmark_field(config, "slope")
  intercept_col <- benchmark_field(config, "intercept")
  id_col <- benchmark_field(config, "model_id")
  species_col <- benchmark_field(config, "species")

  if (!all(c(slope_col, intercept_col) %in% names(anchor_row))) {
    return(tibble::tibble())
  }

  slope_val <- suppressWarnings(as.numeric(anchor_row[[slope_col]][[1]]))
  intercept_val <- suppressWarnings(as.numeric(anchor_row[[intercept_col]][[1]]))
  if (!is.finite(slope_val) || !is.finite(intercept_val)) {
    return(tibble::tibble())
  }

  # Use the anchor PDF span as the standardized evaluation domain, then score
  # all policies on the same 0-1 relative-length grid.
  length_pdf <- eval_obj$anchor_pdf
  lmin <- min(length_pdf$length_cm, na.rm = TRUE)
  lmax <- max(length_pdf$length_cm, na.rm = TRUE)
  u_grid <- seq(0, 1, length.out = 41)
  eval_lengths <- if (is.finite(lmin) && is.finite(lmax) && lmax > lmin) {
    lmin + u_grid * (lmax - lmin)
  } else {
    rep(lmin, length(u_grid))
  }

  ts_obs <- ts_from_length(slope_val, intercept_val, eval_lengths)

  policy_tbl <- tibble::as_tibble(policy_tbl)
  policy_tbl$policy <- resolve_policy_names(policy_tbl)

  purrr::map_dfr(policy_tbl$policy, function(policy_name) {
    ts_pred <- tryCatch(
      curve_fun(
        policy = policy_name,
        eval_obj = eval_obj,
        ordination_info = ordination_info,
        lengths_cm = eval_lengths
      ),
      error = function(e) rep(NA_real_, length(eval_lengths))
    )

    sigma_obs <- 10^(ts_obs / 10)
    sigma_pred <- 10^(ts_pred / 10)

    tibble::tibble(
      anchor_model_id = as.character(anchor_row[[id_col]][[1]]),
      anchor_species = as.character(anchor_row[[species_col]][[1]]),
      policy = policy_name,
      u = u_grid,
      length_cm = eval_lengths,
      ts_obs = ts_obs,
      ts_pred = ts_pred,
      ts_error = ts_pred - ts_obs,
      abs_ts_error = abs(ts_pred - ts_obs),
      sigma_obs = sigma_obs,
      sigma_pred = sigma_pred,
      log_sigma_residual = log(sigma_obs / sigma_pred)
    )
  }) |>
    dplyr::filter(
      is.finite(ts_error),
      is.finite(abs_ts_error),
      is.finite(log_sigma_residual)
    )
}

#' Remove same-species support from one anchor evaluation
#'
#' @param eval_obj Anchor evaluation object.
#' @param anchor_row One-row anchor table.
#' @param config Benchmark config list.
#'
#' @return Modified anchor evaluation object.
#'
#' @keywords internal
remove_species_support <- function(eval_obj,
                                   anchor_row,
                                   config) {
  # Rebuild the admissible support set after removing same-species donor rows
  # so the leave-one-species-out benchmark uses a properly renormalized pool.
  species_col <- benchmark_field(config, "species")
  anchor_species <- as.character(anchor_row[[species_col]][[1]])
  out <- eval_obj

  a_out <- tibble::as_tibble(eval_obj$admissible_df) |>
    dplyr::filter(!overlap_same_species) |>
    dplyr::arrange(dplyr::desc(w_adm))

  if (nrow(a_out) > 0) {
    a_out <- a_out |>
      dplyr::mutate(
        w_adm = w_adm / sum(w_adm, na.rm = TRUE),
        cumulative_w_adm = cumsum(w_adm),
        support_set = dplyr::if_else(
          cumulative_w_adm <= config$core_weight_cutoff,
          "core",
          "tail"
        )
      )
  }

  out$admissible_df <- a_out
  out$model_eval <- tibble::as_tibble(eval_obj$model_eval) |>
    dplyr::filter(.data[[species_col]] != anchor_species)

  out
}

#' Benchmark one anchor
#'
#' @param anchor_row One-row anchor table.
#' @param candidate_models Candidate-model table.
#' @param policy_fun Policy-extraction function.
#' @param curve_fun Optional policy-curve function.
#' @param model_scores Optional ordination score table.
#' @param species_lookup Optional species lookup table/list.
#' @param reference_ids Optional reference-model IDs.
#' @param policies Optional vector of policy names to evaluate.
#' @param policy_params Optional named list of extra policy parameters.
#' @param policy_path Optional path to a policy-registry JSON file.
#' @param sim_obj Optional prebuilt similarity object for the full benchmark
#'   scenario.
#' @param dist_obj Optional prebuilt distance object for the full benchmark
#'   scenario.
#' @param candidate_models_scored Optional candidate-model table that already
#'   contains `key_metadata_missing_fraction`.
#' @param config Benchmark config list.
#' @param registry_path Optional registry path.
#' @param scheme Validation-scheme label.
#' @param species_block Logical scalar. If `TRUE`, exclude same-species donors.
#'
#' @return A list with `perf`, `features`, and `ts_error`.
#'
#' @keywords internal
benchmark_one_anchor <- function(anchor_row,
                                 candidate_models,
                                 policy_fun,
                                 curve_fun,
                                 model_scores,
                                 species_lookup,
                                 reference_ids,
                                 policies,
                                 policy_params,
                                 policy_path,
                                 sim_obj,
                                 dist_obj,
                                 candidate_models_scored,
                                 config,
                                 registry_path,
                                 scheme,
                                 species_block = FALSE) {
  # Evaluate one anchor first, optionally rebuild the donor pool without same-
  # species rows, then extract the policy benchmark tables.
  eval_obj <- tryCatch(
    evaluate_anchor_models(
      anchor_row = anchor_row,
      candidate_models = candidate_models,
      config = config,
      registry_path = registry_path,
      sim_obj = sim_obj,
      dist_obj = dist_obj,
      candidate_models_scored = candidate_models_scored
    ),
    error = function(e) NULL
  )
  if (is.null(eval_obj)) {
    return(NULL)
  }

  if (isTRUE(species_block)) {
    eval_obj <- remove_species_support(eval_obj, anchor_row, config)
  }

  ordination_info <- resolve_ordination_info(
    anchor_row = anchor_row,
    model_scores = model_scores,
    species_lookup = species_lookup,
    config = config
  )

  # Pass the workflow-selected policy set straight into the package policy
  # layer so the workflow script does not need local wrapper functions.
  policy_args <- list(
      eval_obj = eval_obj,
      ordination_info = ordination_info,
      policies = policies,
      policy_params = policy_params,
      policy_path = policy_path
    )
  policy_args <- policy_args[names(policy_args) %in% names(formals(policy_fun))]
  policy_tbl <- do.call(policy_fun, policy_args)
  policy_tbl <- tibble::as_tibble(policy_tbl)
  policy_tbl$policy <- resolve_policy_names(policy_tbl)

  if (!all(c("policy", "multiplier_pred") %in% names(policy_tbl))) {
    stop("'policy_fun' must return columns named 'policy' and 'multiplier_pred'.", call. = FALSE)
  }

  id_col <- benchmark_field(config, "model_id")
  species_col <- benchmark_field(config, "species")
  family_col <- benchmark_field(config, "family")
  anchor_id <- as.character(anchor_row[[id_col]][[1]])
  is_ref <- anchor_id %in% reference_ids

  # Add the benchmark annotations once so the same policy table can feed the
  # best-policy summary and any later conformal evaluation.
  policy_tbl <- policy_tbl |>
    dplyr::mutate(
      anchor_model_id = anchor_id,
      anchor_species = as.character(anchor_row[[species_col]][[1]]),
      anchor_family = as.character(anchor_row[[family_col]][[1]]),
      is_reference = is_ref,
      anchor_group = ifelse(is_ref, "reference", "candidate"),
      validation_scheme = scheme,
      error_abs_log = abs(log(multiplier_pred)),
      valid_prediction = is.finite(multiplier_pred) & multiplier_pred > 0
    )

  best_policy_name <- pick_best_policy(policy_tbl)
  feature_row <- build_benchmark_row(
    eval_obj = eval_obj,
    anchor_row = anchor_row,
    best_policy_name = best_policy_name,
    is_reference = is_ref,
    config = config
  ) |>
    dplyr::mutate(validation_scheme = scheme)

    curve_runner <- NULL
    if (is.function(curve_fun)) {
      curve_runner <- function(policy, eval_obj, ordination_info, lengths_cm) {
        curve_args <- list(
          policy = policy,
          eval_obj = eval_obj,
          lengths_cm = lengths_cm,
          ordination_info = ordination_info,
          policy_params = policy_params,
          policy_path = policy_path
        )
        curve_args <- curve_args[names(curve_args) %in% names(formals(curve_fun))]
        do.call(curve_fun, curve_args)
      }
    }

    ts_error <- build_ts_errors(
      anchor_row = anchor_row,
      eval_obj = eval_obj,
      policy_tbl = policy_tbl,
      ordination_info = ordination_info,
      curve_fun = curve_runner,
      config = config
    ) |>
      dplyr::mutate(validation_scheme = scheme)

  list(
    perf = policy_tbl,
    features = feature_row,
    ts_error = ts_error
  )
}

#' Build one best-policy table
#'
#' @param perf_tbl Policy-performance table.
#'
#' @return A tibble.
#'
#' @keywords internal
bind_best_policy_rows <- function(perf_tbl) {
  # Pick the best valid policy per anchor and validation scheme using the
  # smallest absolute log-error, with policy name as the deterministic tiebreak.
  out <- tibble::as_tibble(perf_tbl)
  if (nrow(out) == 0 || !"valid_prediction" %in% names(out)) {
    return(tibble::tibble())
  }

  out$policy <- resolve_policy_names(out)
  out |>
    dplyr::filter(valid_prediction) |>
    dplyr::group_by(
      anchor_model_id,
      anchor_species,
      anchor_family,
      is_reference,
      anchor_group,
      validation_scheme
    ) |>
    dplyr::arrange(error_abs_log, policy, .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      anchor_model_id,
      anchor_species,
      anchor_family,
      is_reference,
      anchor_group,
      validation_scheme,
      best_policy = policy,
      best_multiplier_pred = multiplier_pred,
      best_error_abs_log = error_abs_log
    )
}

#' Run the policy benchmark
#'
#' Evaluates every prepared model as a pseudo-anchor, applies a caller-supplied
#' policy extractor, and returns in-memory benchmark tables for both the full
#' donor pool and the leave-one-species-out donor pool.
#'
#' @param candidate_models Prepared candidate-model table.
#' @param policy_fun Policy-extraction function. It must accept `eval_obj`
#'   plus optional ordination context, and return at least `policy` and
#'   `multiplier_pred`.
#' @param curve_fun Optional policy-curve function for TS-length error
#'   summaries. It must accept `policy`, `eval_obj`, optional ordination
#'   context, and `lengths_cm`.
#' @param model_scores Optional ordination score table.
#' @param species_lookup Optional species lookup object used by ordination-dependent
#'   policies.
#' @param reference_ids Optional vector of reference-model IDs used only to
#'   annotate the output tables.
#' @param config Optional JSON path or list with benchmark/admissibility
#'   settings.
#' @param include_ts_error Logical scalar. If `TRUE`, compute the relative-length
#'   TS error table used by the TS conformal summaries.
#' @param workers Number of parallel workers. Use `1` for sequential execution.
#' @param package_dir Optional package source directory used to load the
#'   development package on parallel workers when running from source.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#' @param progress Logical scalar. If `TRUE`, emit lightweight progress updates
#'   during the anchor loop.
#' @param registry_path Optional path to the trait-registry JSON.
#'
#' @return A list containing full-pool and leave-one-species-out benchmark
#'   tables.
#'
#' @export
run_policy_benchmark <- function(candidate_models,
                                 policy_fun = evaluate_policies,
                                 curve_fun = predict_policy_curve,
                                 model_scores = NULL,
                                 species_lookup = NULL,
                                 reference_ids = NULL,
                                 policies = NULL,
                                 policy_params = list(),
                                 policy_path = NULL,
                                 config = NULL,
                                 include_ts_error = TRUE,
                                 workers = 1L,
                                 package_dir = NULL,
                                 cache_path = NULL,
                                 refresh = FALSE,
                                 progress = FALSE,
                                 registry_path = NULL) {
  # Validate the benchmark inputs once before any anchor loop or cache work.
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.function(policy_fun)) {
    stop("'policy_fun' must be a function.", call. = FALSE)
  }
  if (!is.null(curve_fun) && !is.function(curve_fun)) {
    stop("'curve_fun' must be NULL or a function.", call. = FALSE)
  }
  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("'progress' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(include_ts_error) || length(include_ts_error) != 1 || is.na(include_ts_error)) {
    stop("'include_ts_error' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(workers) || length(workers) != 1 || !is.finite(workers) || workers < 1) {
    stop("'workers' must be one finite number >= 1.", call. = FALSE)
  }
  if (!is.null(package_dir) &&
      (!is.character(package_dir) || length(package_dir) != 1 || !nzchar(package_dir))) {
    stop("'package_dir' must be NULL or a single non-empty path.", call. = FALSE)
  }

  # Reuse the cached benchmark object when available unless the caller asked
  # for a refresh.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Inline the benchmark defaults here so the benchmark layer does not carry a
  # separate default-config helper.
  config_values <- merge_cfg(
    list(
      fields = list(
        model_id = "model_id",
        model_id_chr = "model_id_chr",
        species = "species_name",
        family = "family",
        slope = "slope_len",
        intercept = "intercept_len"
      ),
      core_weight_cutoff = 0.8,
      species_block_label = "leave_one_species_out"
    ),
    read_similarity_config(config)
  )
  ref_ids <- normalize_reference_ids(reference_ids)

  # Build the scenario-level similarity objects once so every anchor in the
  # benchmark loop can reuse the same prepared trait matrices and distance
  # matrices instead of rebuilding them repeatedly.
  sim_obj <- prepare_similarity_matrix(
    candidate_models = candidate_models,
    species_traits = config_values$species_traits %||% NULL,
    study_traits = config_values$study_traits %||% NULL,
    alpha = config_values$alpha %||% NULL,
    k_species = config_values$k_species %||% NULL,
    k_study = config_values$k_study %||% NULL,
    config = config_values,
    registry_path = registry_path,
    seed = config_values$seed %||% NULL
  )
  dist_obj <- build_gower_distances(sim_obj)
  candidate_models_scored <- screen_missing_metadata(
    candidate_models = sim_obj$candidate_models,
    key_cols = unique(c(sim_obj$species_traits, sim_obj$study_traits))
  )

  perf_rows <- list()
  feat_rows <- list()
  err_rows <- list()
  sb_perf_rows <- list()
  sb_feat_rows <- list()
  total_anchors <- nrow(candidate_models)
  progress_step <- max(1L, ceiling(total_anchors / 20))
  workers <- as.integer(workers)
  chunk_index <- split(
    seq_len(total_anchors),
    ceiling(seq_len(total_anchors) / progress_step)
  )

  # Evaluate every candidate as an anchor under both the full donor pool and
  # the leave-one-species-out donor pool, optionally in parallel.
  if (workers <= 1L) {
    for (i in seq_len(total_anchors)) {
      anchor_row <- candidate_models[i, , drop = FALSE]

      bench_obj <- benchmark_one_anchor(
        anchor_row = anchor_row,
        candidate_models = candidate_models,
        policy_fun = policy_fun,
        curve_fun = if (isTRUE(include_ts_error)) curve_fun else NULL,
        model_scores = model_scores,
        species_lookup = species_lookup,
        reference_ids = ref_ids,
        policies = policies,
        policy_params = policy_params,
        policy_path = policy_path,
        sim_obj = sim_obj,
        dist_obj = dist_obj,
        candidate_models_scored = candidate_models_scored,
        config = config_values,
        registry_path = registry_path,
        scheme = "pseudo_anchor",
        species_block = FALSE
      )

      if (!is.null(bench_obj)) {
        perf_rows[[length(perf_rows) + 1]] <- bench_obj$perf
        feat_rows[[length(feat_rows) + 1]] <- bench_obj$features
        err_rows[[length(err_rows) + 1]] <- bench_obj$ts_error
      }

      sb_obj <- benchmark_one_anchor(
        anchor_row = anchor_row,
        candidate_models = candidate_models,
        policy_fun = policy_fun,
        curve_fun = NULL,
        model_scores = model_scores,
        species_lookup = species_lookup,
        reference_ids = ref_ids,
        policies = policies,
        policy_params = policy_params,
        policy_path = policy_path,
        sim_obj = sim_obj,
        dist_obj = dist_obj,
        candidate_models_scored = candidate_models_scored,
        config = config_values,
        registry_path = registry_path,
        scheme = config_values$species_block_label,
        species_block = TRUE
      )

      if (!is.null(sb_obj)) {
        sb_perf_rows[[length(sb_perf_rows) + 1]] <- sb_obj$perf
        sb_feat_rows[[length(sb_feat_rows) + 1]] <- sb_obj$features
      }

      if (isTRUE(progress) && (i %% progress_step == 0L || i == total_anchors)) {
        base::message(
          "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
          "Policy benchmark progress: ", i, "/", total_anchors, " anchors processed."
        )
      }
    }
  } else {
    cluster_obj <- initialize_parallel_cluster(
      workers = workers,
      package_dir = package_dir
    )
    on.exit(parallel::stopCluster(cluster_obj), add = TRUE)

    parallel::clusterExport(
      cluster_obj,
      c(
        "candidate_models",
        "policy_fun",
        "curve_fun",
        "model_scores",
        "species_lookup",
        "ref_ids",
        "policies",
        "policy_params",
        "policy_path",
        "sim_obj",
        "dist_obj",
        "candidate_models_scored",
        "config_values",
        "registry_path",
        "include_ts_error"
      ),
      envir = environment()
    )

    if (isTRUE(progress)) {
      base::message(
        "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
        "Policy benchmark running in parallel with ", workers, " workers."
      )
    }

    processed <- 0L
    for (ids in chunk_index) {
      chunk_results <- parallel::parLapplyLB(
        cluster_obj,
        ids,
        function(i) {
          anchor_row <- candidate_models[i, , drop = FALSE]

          bench_obj <- benchmark_one_anchor(
            anchor_row = anchor_row,
            candidate_models = candidate_models,
            policy_fun = policy_fun,
            curve_fun = if (isTRUE(include_ts_error)) curve_fun else NULL,
            model_scores = model_scores,
            species_lookup = species_lookup,
            reference_ids = ref_ids,
            policies = policies,
            policy_params = policy_params,
            policy_path = policy_path,
            sim_obj = sim_obj,
            dist_obj = dist_obj,
            candidate_models_scored = candidate_models_scored,
            config = config_values,
            registry_path = registry_path,
            scheme = "pseudo_anchor",
            species_block = FALSE
          )

          sb_obj <- benchmark_one_anchor(
            anchor_row = anchor_row,
            candidate_models = candidate_models,
            policy_fun = policy_fun,
            curve_fun = NULL,
            model_scores = model_scores,
            species_lookup = species_lookup,
            reference_ids = ref_ids,
            policies = policies,
            policy_params = policy_params,
            policy_path = policy_path,
            sim_obj = sim_obj,
            dist_obj = dist_obj,
            candidate_models_scored = candidate_models_scored,
            config = config_values,
            registry_path = registry_path,
            scheme = config_values$species_block_label,
            species_block = TRUE
          )

          list(bench = bench_obj, species_block = sb_obj)
        }
      )

      for (one_result in chunk_results) {
        if (!is.null(one_result$bench)) {
          perf_rows[[length(perf_rows) + 1]] <- one_result$bench$perf
          feat_rows[[length(feat_rows) + 1]] <- one_result$bench$features
          err_rows[[length(err_rows) + 1]] <- one_result$bench$ts_error
        }
        if (!is.null(one_result$species_block)) {
          sb_perf_rows[[length(sb_perf_rows) + 1]] <- one_result$species_block$perf
          sb_feat_rows[[length(sb_feat_rows) + 1]] <- one_result$species_block$features
        }
      }

      processed <- processed + length(ids)
      if (isTRUE(progress)) {
        base::message(
          "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
          "Policy benchmark progress: ", processed, "/", total_anchors, " anchors processed."
        )
      }
    }
  }

  perf_tbl <- dplyr::bind_rows(perf_rows)
  feat_tbl <- dplyr::bind_rows(feat_rows)
  err_tbl <- dplyr::bind_rows(err_rows)
  sb_perf_tbl <- dplyr::bind_rows(sb_perf_rows)
  sb_feat_tbl <- dplyr::bind_rows(sb_feat_rows)

  result <- list(
    policy_perf = perf_tbl,
    anchor_features = feat_tbl,
    best_policy = bind_best_policy_rows(perf_tbl),
    policy_ts_error = err_tbl,
    species_block_perf = sb_perf_tbl,
    species_block_features = sb_feat_tbl,
    species_block_best = bind_best_policy_rows(sb_perf_tbl)
  )

  # Persist the in-memory benchmark object only when the caller requested a
  # cache path.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  result
}
