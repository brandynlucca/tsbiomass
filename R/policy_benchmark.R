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
  valid <- standardize_policies(policy_tbl)
  valid <- valid |>
    dplyr::filter(valid_prediction)

  if (nrow(valid) == 0) {
    return(tibble::tibble(
      best_policy = NA_character_,
      best_equation_branch_filter = NA_character_
    ))
  }

  valid |>
    dplyr::arrange(error_abs_log, policy, equation_branch_filter) |>
    dplyr::slice(1) |>
    dplyr::transmute(
      best_policy = policy,
      best_equation_branch_filter = equation_branch_filter
    )
}

#' Build one benchmark feature row
#'
#' @param eval_obj Anchor evaluation object.
#' @param anchor_row One-row anchor table.
#' @param best_policy_name Best policy label.
#' @param best_equation_branch_filter Best branch-filter label.
#' @param is_reference Logical scalar.
#' @param config Benchmark config list.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
build_benchmark_row <- function(eval_obj,
                                anchor_row,
                                best_policy_name,
                                best_equation_branch_filter,
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
    best_policy = best_policy_name,
    best_equation_branch_filter = best_equation_branch_filter
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
                            policy_tbl,
                            config) {
  empty_ts_tbl <- function() {
    tibble::tibble(
      anchor_model_id = character(),
      anchor_species = character(),
      policy = character(),
      equation_branch_filter = character(),
      policy_slope_len = numeric(),
      policy_intercept_len = numeric(),
      u = numeric(),
      length_cm = numeric(),
      ts_obs = numeric(),
      ts_pred = numeric(),
      ts_error = numeric(),
      abs_ts_error = numeric(),
      sigma_obs = numeric(),
      sigma_pred = numeric(),
      log_sigma_residual = numeric()
    )
  }

  # Skip the TS-length error summary entirely when no curve predictor was
  # supplied or the anchor lacks a usable standardized length-form equation.
  slope_col <- benchmark_field(config, "slope")
  intercept_col <- benchmark_field(config, "intercept")
  id_col <- benchmark_field(config, "model_id")
  species_col <- benchmark_field(config, "species")

  if (!all(c(slope_col, intercept_col) %in% names(anchor_row))) {
    return(empty_ts_tbl())
  }

  slope_val <- suppressWarnings(as.numeric(anchor_row[[slope_col]][[1]]))
  intercept_val <- suppressWarnings(as.numeric(anchor_row[[intercept_col]][[1]]))
  if (!is.finite(slope_val) || !is.finite(intercept_val)) {
    return(empty_ts_tbl())
  }

  # Use the anchor study-length span as the standardized evaluation domain,
  # then score all policies on the same 0-1 relative-length grid.
  lmin <- suppressWarnings(as.numeric(anchor_row$study_length_min[[1]]))
  lmax <- suppressWarnings(as.numeric(anchor_row$study_length_max[[1]]))
  if (!is.finite(lmin) || !is.finite(lmax) || lmax <= lmin) {
    return(empty_ts_tbl())
  }
  u_grid <- seq(0, 1, length.out = 11)
  eval_lengths <- lmin + u_grid * (lmax - lmin)

  ts_obs <- ts_from_length(slope_val, intercept_val, eval_lengths)
  if (!all(is.finite(ts_obs)) || any(ts_obs >= 0, na.rm = TRUE)) {
    anchor_id_val <- if (length(id_col) > 0 && id_col %in% names(anchor_row)) {
      as.character(anchor_row[[id_col]][[1]])
    } else {
      "unknown"
    }
    warning(
      sprintf(
        "Anchor '%s': TS values are non-negative or non-finite on the study length grid. ",
        anchor_id_val
      ),
      "TS must always be negative for real acoustic scatterers. ",
      "TS-error benchmarks for this anchor will be empty.",
      call. = FALSE
    )
    return(empty_ts_tbl())
  }

  policy_tbl <- policy_tbl |>
    dplyr::distinct(policy, equation_branch_filter, .keep_all = TRUE) |>
    dplyr::filter(
      is.finite(policy_slope_len),
      is.finite(policy_intercept_len)
    )
  if (nrow(policy_tbl) == 0) {
    return(empty_ts_tbl())
  }

  slopes <- suppressWarnings(as.numeric(policy_tbl$policy_slope_len))
  intercepts <- suppressWarnings(as.numeric(policy_tbl$policy_intercept_len))
  log_lengths <- log10(eval_lengths)
  ts_pred_mat <- outer(log_lengths, slopes, "*") +
    matrix(intercepts, nrow = length(eval_lengths), ncol = length(intercepts), byrow = TRUE)
  valid_policy <- apply(
    ts_pred_mat,
    2,
    function(x) all(is.finite(x)) && !any(x >= 0, na.rm = TRUE)
  )
  if (!any(valid_policy)) {
    return(empty_ts_tbl())
  }

  policy_tbl <- policy_tbl[valid_policy, , drop = FALSE]
  ts_pred_mat <- ts_pred_mat[, valid_policy, drop = FALSE]
  sigma_obs <- 10^(ts_obs / 10)
  out <- purrr::map_dfr(seq_len(ncol(ts_pred_mat)), function(i) {
    ts_pred <- ts_pred_mat[, i]
    sigma_pred <- 10^(ts_pred / 10)
    tibble::tibble(
      anchor_model_id = as.character(anchor_row[[id_col]][[1]]),
      anchor_species = as.character(anchor_row[[species_col]][[1]]),
      policy = as.character(policy_tbl$policy[[i]]),
      equation_branch_filter = as.character(policy_tbl$equation_branch_filter[[i]]),
      policy_slope_len = suppressWarnings(as.numeric(policy_tbl$policy_slope_len[[i]])),
      policy_intercept_len = suppressWarnings(as.numeric(policy_tbl$policy_intercept_len[[i]])),
      local_min_combined_distance = suppressWarnings(as.numeric(policy_tbl$local_min_combined_distance[[i]] %||% NA_real_)),
      local_weighted_mean_combined_distance = suppressWarnings(as.numeric(policy_tbl$local_weighted_mean_combined_distance[[i]] %||% NA_real_)),
      local_effective_support = suppressWarnings(as.numeric(policy_tbl$local_effective_support[[i]] %||% NA_real_)),
      local_mean_length_overlap = suppressWarnings(as.numeric(policy_tbl$local_mean_length_overlap[[i]] %||% NA_real_)),
      local_mean_depth_overlap = suppressWarnings(as.numeric(policy_tbl$local_mean_depth_overlap[[i]] %||% NA_real_)),
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
  })
  dplyr::filter(
    out,
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

remove_group_support <- function(eval_obj,
                                 anchor_row,
                                 group_col) {
  if (is.null(group_col) || !nzchar(group_col) ||
    !group_col %in% names(anchor_row) ||
    !group_col %in% names(eval_obj$model_eval)) {
    return(eval_obj)
  }
  anchor_group <- as.character(anchor_row[[group_col]][[1]])
  if (is.na(anchor_group) || !nzchar(anchor_group)) {
    return(eval_obj)
  }

  out <- eval_obj
  out$model_eval <- tibble::as_tibble(eval_obj$model_eval) |>
    dplyr::filter(as.character(.data[[group_col]]) != anchor_group)
  a_out <- tibble::as_tibble(eval_obj$admissible_df)
  if (group_col %in% names(a_out)) {
    a_out <- a_out |>
      dplyr::filter(as.character(.data[[group_col]]) != anchor_group) |>
      dplyr::arrange(dplyr::desc(w_adm))
  }
  if (nrow(a_out) > 0 && "w_adm" %in% names(a_out)) {
    denom <- sum(a_out$w_adm, na.rm = TRUE)
    if (is.finite(denom) && denom > 0) {
      a_out <- a_out |>
        dplyr::mutate(
          w_adm = w_adm / denom,
          cumulative_w_adm = cumsum(w_adm)
        )
    }
  }
  out$admissible_df <- a_out
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
#' @param eval_obj Optional precomputed anchor-evaluation object. Supplying this
#'   avoids rebuilding the same admissible donor pool for multiple validation
#'   schemes.
#' @param config Benchmark config list.
#' @param registry_path Optional registry path.
#' @param scheme Validation-scheme label.
#' @param species_block Logical scalar. If `TRUE`, exclude same-species donors.
#' @param ordination_info Optional precomputed ordination context for the
#'   current anchor.
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
                                 eval_obj = NULL,
                                 config,
                                 registry_path,
                                 scheme,
                                 species_block = FALSE,
                                 group_block_col = NULL,
                                 ordination_info = NULL) {
  # Evaluate one anchor first, optionally rebuild the donor pool without same-
  # species rows, then extract the policy benchmark tables.
  if (is.null(eval_obj)) {
    eval_obj <- tryCatch(
      screen_one_anchor_admissibility(
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
  }
  if (is.null(eval_obj)) {
    return(NULL)
  }

  if (isTRUE(species_block)) {
    eval_obj <- remove_species_support(eval_obj, anchor_row, config)
  }
  if (!is.null(group_block_col)) {
    eval_obj <- remove_group_support(eval_obj, anchor_row, group_block_col)
  }

  ordination_info <- ordination_info %||% resolve_ordination_info(
    anchor_row = anchor_row,
    model_scores = model_scores,
    species_lookup = species_lookup,
    config = config
  )

  # Pass the selected policy set straight into the package policy
  # layer so the packaged script does not need local wrapper functions.
  policy_args <- list(
    eval_obj = eval_obj,
    ordination_info = ordination_info,
    policies = policies,
    policy_params = policy_params,
    policy_path = policy_path
  )
  policy_args <- policy_args[names(policy_args) %in% names(formals(policy_fun))]
  policy_tbl <- do.call(policy_fun, policy_args)
  policy_tbl <- standardize_policies(policy_tbl)
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
  policy_tbl$anchor_model_id    <- anchor_id
  policy_tbl$anchor_species     <- as.character(anchor_row[[species_col]][[1]])
  policy_tbl$anchor_family      <- as.character(anchor_row[[family_col]][[1]])
  policy_tbl$anchor_group_block <- if (!is.null(group_block_col) && group_block_col %in% names(anchor_row)) {
    as.character(anchor_row[[group_block_col]][[1]])
  } else {
    NA_character_
  }
  policy_tbl$is_reference       <- is_ref
  policy_tbl$anchor_group       <- if (is_ref) "reference" else "candidate"
  policy_tbl$validation_scheme  <- scheme
  policy_tbl$error_abs_log      <- abs(log(policy_tbl$multiplier_pred))
  policy_tbl$valid_prediction   <- is.finite(policy_tbl$multiplier_pred) & policy_tbl$multiplier_pred > 0

  best_policy_row <- pick_best_policy(policy_tbl)
  feature_row <- build_benchmark_row(
    eval_obj = eval_obj,
    anchor_row = anchor_row,
    best_policy_name = best_policy_row$best_policy[[1]],
    best_equation_branch_filter = best_policy_row$best_equation_branch_filter[[1]],
    is_reference = is_ref,
    config = config
  ) |>
    dplyr::mutate(
      validation_scheme = scheme,
      anchor_group_block = if (!is.null(group_block_col) && group_block_col %in% names(anchor_row)) {
        as.character(anchor_row[[group_block_col]][[1]])
      } else {
        NA_character_
      }
    )

  ts_error <- if (is.function(curve_fun)) {
    build_ts_errors(
      anchor_row = anchor_row,
      policy_tbl = policy_tbl,
      config = config
    ) |>
      dplyr::mutate(validation_scheme = scheme) |>
      dplyr::select(dplyr::any_of(c(
        "anchor_model_id", "policy", "equation_branch_filter",
        "policy_slope_len", "policy_intercept_len",
        "local_min_combined_distance",
        "local_weighted_mean_combined_distance",
        "local_effective_support",
        "local_mean_length_overlap",
        "local_mean_depth_overlap",
        "validation_scheme", "u", "ts_error", "log_sigma_residual"
      )))
  } else {
    tibble::tibble()
  }

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
  out <- standardize_policies(perf_tbl)
  if (nrow(out) == 0 || !"valid_prediction" %in% names(out)) {
    return(tibble::tibble())
  }
  if (!"anchor_group_block" %in% names(out)) {
    out$anchor_group_block <- NA_character_
  }

  out$policy <- resolve_policy_names(out)
  out |>
    dplyr::filter(valid_prediction) |>
    dplyr::group_by(
      dplyr::across(dplyr::any_of(c(
        "anchor_model_id",
        "anchor_species",
        "anchor_family",
        "anchor_group_block",
        "is_reference",
        "anchor_group",
        "validation_scheme"
      )))
    ) |>
    dplyr::arrange(error_abs_log, policy, equation_branch_filter, .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      anchor_model_id,
      anchor_species,
      anchor_family,
      anchor_group_block,
      is_reference,
      anchor_group,
      validation_scheme,
      best_policy = policy,
      best_equation_branch_filter = equation_branch_filter,
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
#' @param candidate_models Prepared candidate-model table or a [Candidates]
#'   object.
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
#' @param benchmark_schemes Validation schemes to compute. Supported values are
#'   `"pseudo_anchor"`, `"species_block"`, and `"group_block"`.
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
                                 benchmark_schemes = c("pseudo_anchor", "species_block"),
                                 workers = 1L,
                                 package_dir = NULL,
                                 cache_path = NULL,
                                 refresh = FALSE,
                                 progress = FALSE,
                                 group_block_col = NULL,
                                 group_block_label = "leave_one_group_out",
                                 registry_path = NULL) {
  # Support the staged `Candidates` object directly so the benchmark layer can
  # reuse prepared similarity, distance, ordination, and reference-anchor
  # state when those objects already exist.
  candidates_obj <- if ((inherits(candidate_models, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidate_models, Candidates), error = function(e) FALSE)))) candidate_models else NULL
  if (!is.null(candidates_obj)) {
    if (is.null(model_scores) &&
      length(candidates_obj@ordination) > 0 &&
      is.list((candidates_obj@ordination)$model) &&
      is.data.frame(((candidates_obj@ordination)$model)$model_scores %||% NULL)) {
      model_scores <- ((candidates_obj@ordination)$model)$model_scores
    }
    if (is.null(species_lookup) &&
      length(candidates_obj@ordination) > 0 &&
      is.list((candidates_obj@ordination)$species_lookup)) {
      species_lookup <- (candidates_obj@ordination)$species_lookup
    }
    if (is.null(reference_ids) && nrow(candidates_obj@reference_anchors) > 0) {
      if ("model_id_chr" %in% names(candidates_obj@reference_anchors)) {
        reference_ids <- as.character(candidates_obj@reference_anchors$model_id_chr)
      } else if ("model_id" %in% names(candidates_obj@reference_anchors)) {
        reference_ids <- as.character(candidates_obj@reference_anchors$model_id)
      }
    }
    candidate_models <- tibble::as_tibble(candidates_obj@candidate_models)
  }

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
  benchmark_schemes <- if (missing(benchmark_schemes) && !is.null(group_block_col)) {
    c("pseudo_anchor", "species_block", "group_block")
  } else {
    benchmark_schemes %||% c("pseudo_anchor", "species_block")
  }
  benchmark_schemes <- unique(as.character(benchmark_schemes))
  valid_schemes <- c("pseudo_anchor", "species_block", "group_block", group_block_label)
  if (length(benchmark_schemes) == 0 ||
    any(is.na(benchmark_schemes) | !nzchar(benchmark_schemes)) ||
    any(!benchmark_schemes %in% valid_schemes)) {
    stop(
      "'benchmark_schemes' must contain only 'pseudo_anchor', 'species_block', or 'group_block'.",
      call. = FALSE
    )
  }
  run_pseudo_anchor <- "pseudo_anchor" %in% benchmark_schemes
  run_species_block <- "species_block" %in% benchmark_schemes
  run_group_block <- !is.null(group_block_col) &&
    any(c("group_block", group_block_label) %in% benchmark_schemes)
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
  config_branch_filters <- NULL
  config_policy_params <- list()
  if ((inherits(config, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
    config_branch_filters <- (((config@data)$policies) %||% list())$equation_branch_filters %||% NULL
    config_policy_params <- (((config@data)$policies) %||% list())$policy_params %||% list()
  } else if (is.list(config)) {
    config_branch_filters <- config$equation_branch_filters %||%
      config$policies$equation_branch_filters %||%
      NULL
    config_policy_params <- config$policy_params %||%
      config$policies$policy_params %||%
      list()
  }
  policy_params <- merge_cfg(
    config_policy_params,
    merge_cfg(
      list(equation_branch_filters = config_branch_filters),
      policy_params
    )
  )
  ref_ids <- normalize_reference_ids(reference_ids)

  # Build the scenario-level similarity objects once so every anchor in the
  # benchmark loop can reuse the same prepared trait matrices and distance
  # matrices instead of rebuilding them repeatedly.
  if (is.null(config) &&
    !is.null(candidates_obj) &&
    length(candidates_obj@similarity_matrix) > 0) {
    sim_obj <- candidates_obj@similarity_matrix
  } else {
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
  }
  if (is.null(config) &&
    !is.null(candidates_obj) &&
    length(candidates_obj@gower_distances) > 0) {
    dist_obj <- candidates_obj@gower_distances
  } else {
    dist_obj <- build_gower_distances(sim_obj)
  }
  candidate_models_prepared <- tibble::as_tibble(sim_obj$candidate_models %||% candidate_models)
  candidate_models_scored <- screen_missing_metadata(
    candidate_models = candidate_models_prepared,
    key_cols = admissibility_key_metadata_cols(config_values)
  )
  # Slim both objects to only what workers need before cluster creation.
  # screen_one_anchor_admissibility accesses: sim_obj$alpha, $k_species,
  # $k_study, $frequency_span (all scalars) and dist_obj$species_dist_model,
  # $study_dist (the two NxN matrices). Everything else is dead weight.
  # On fork clusters this also reduces the parent-process memory footprint.
  sim_obj <- list(
    alpha          = sim_obj$alpha,
    k_species      = sim_obj$k_species,
    k_study        = sim_obj$k_study,
    frequency_span = sim_obj$frequency_span
  )
  dist_obj <- list(
    species_dist_model = dist_obj$species_dist_model,
    study_dist         = dist_obj$study_dist
  )

  perf_rows <- list()
  feat_rows <- list()
  err_rows <- list()
  sb_perf_rows <- list()
  sb_feat_rows <- list()
  gb_perf_rows <- list()
  gb_feat_rows <- list()
  total_anchors <- nrow(candidate_models)
  n_branch_filters <- length(normalize_policy_equation_branch_filters(
    policy_params$equation_branch_filters %||% NULL
  ))
  if (!is.finite(n_branch_filters) || n_branch_filters < 1L) {
    n_branch_filters <- 1L
  }
  scheme_count <- sum(c(run_pseudo_anchor, run_species_block, run_group_block))
  estimated_policy_rows <- total_anchors * max(1L, length(policies)) * n_branch_filters * max(1L, scheme_count)
  report_progress(
    progress,
    "Policy benchmark workload: ",
    total_anchors,
    " anchor(s), ",
    length(policies),
    " policy/ies, ",
    n_branch_filters,
    " branch filter(s), ",
    max(1L, scheme_count),
    " validation scheme(s), ~",
    estimated_policy_rows,
    " anchor-policy evaluation row(s)."
  )
  workers <- as.integer(workers)

  # Static column sets stripped from the worker payload and rejoined in main.
  .policy_meta_cols <- c(
    "policy_base", "policy_display", "plain_language_definition",
    "policy_family", "aggregation_method", "aggregation_definition",
    "candidate_pool", "candidate_pool_definition",
    "display_name", "grouping_key", "metric_key", "requested_policy"
  )
  .anchor_meta_cols <- c(
    "anchor_species", "anchor_family", "anchor_group", "anchor_group_block", "is_reference"
  )

  # Build the anchor-metadata lookup from candidate_models (no computation needed).
  .id_col_nm  <- benchmark_field(config_values, "model_id")
  .spc_col_nm <- benchmark_field(config_values, "species")
  .fam_col_nm <- benchmark_field(config_values, "family")
  anchor_meta_lookup <- data.frame(
    anchor_model_id  = as.character(candidate_models[[.id_col_nm]]),
    anchor_species   = as.character(candidate_models[[.spc_col_nm]]),
    anchor_family    = as.character(candidate_models[[.fam_col_nm]]),
    is_reference     = as.character(candidate_models[[.id_col_nm]]) %in% ref_ids,
    stringsAsFactors = FALSE
  )
  anchor_meta_lookup$anchor_group <- ifelse(
    anchor_meta_lookup$is_reference, "reference", "candidate"
  )
  anchor_meta_lookup$anchor_group_block <- if (
    !is.null(group_block_col) && group_block_col %in% names(candidate_models)
  ) {
    as.character(candidate_models[[group_block_col]])
  } else {
    NA_character_
  }
  anchor_meta_lookup <- unique(anchor_meta_lookup)
  rm(.id_col_nm, .spc_col_nm, .fam_col_nm)

  # Build the policy-metadata lookup by running policy_fun once on the first
  # admissible anchor. Policy metadata is identical across all anchors.
  policy_meta_lookup <- NULL
  for (.pi in seq_len(min(10L, total_anchors))) {
    .peobj <- tryCatch(
      screen_one_anchor_admissibility(
        anchor_row            = candidate_models[.pi, , drop = FALSE],
        candidate_models      = candidate_models,
        config                = config_values,
        registry_path         = registry_path,
        sim_obj               = sim_obj,
        dist_obj              = dist_obj,
        candidate_models_scored = candidate_models_scored
      ), error = function(e) NULL
    )
    if (is.null(.peobj)) next
    .pargs <- list(
      eval_obj      = .peobj,
      policies      = policies,
      policy_params = policy_params,
      policy_path   = policy_path
    )
    .pargs <- .pargs[names(.pargs) %in% names(formals(policy_fun))]
    .ptbl <- tryCatch(do.call(policy_fun, .pargs), error = function(e) NULL)
    if (!is.null(.ptbl) && nrow(.ptbl) > 0L) {
      .keep <- intersect(
        c("policy", "equation_branch_filter", .policy_meta_cols), names(.ptbl)
      )
      policy_meta_lookup <- unique(as.data.frame(.ptbl[, .keep, drop = FALSE]))
      break
    }
  }
  suppressWarnings(rm(.pi, .peobj, .pargs, .ptbl))

  worker_strip_cols <- c(
    .anchor_meta_cols,
    if (!is.null(policy_meta_lookup)) .policy_meta_cols else character(0L),
    "validation_scheme", "error_abs_log", "valid_prediction"
  )

  # Helper: rejoin stripped perf-table metadata after result collection.
  .rejoin_perf <- function(tbl, pmeta, ameta) {
    if (is.null(tbl) || nrow(tbl) == 0L) return(tbl)
    if (!is.null(pmeta) && nrow(pmeta) > 0L && "policy" %in% names(tbl)) {
      add_cols <- setdiff(names(pmeta), c("policy", "equation_branch_filter", names(tbl)))
      if (length(add_cols) > 0L) {
        tbl <- dplyr::left_join(
          tbl,
          pmeta[, c("policy", "equation_branch_filter", add_cols), drop = FALSE],
          by = c("policy", "equation_branch_filter")
        )
      }
    }
    if (!is.null(ameta) && nrow(ameta) > 0L && "anchor_model_id" %in% names(tbl)) {
      add_cols <- setdiff(names(ameta), c("anchor_model_id", names(tbl)))
      if (length(add_cols) > 0L) {
        tbl <- dplyr::left_join(
          tbl,
          ameta[, c("anchor_model_id", add_cols), drop = FALSE],
          by = "anchor_model_id"
        )
      }
    }
    tbl
  }

  progress_step <- max(1L, ceiling(total_anchors / 20L))
  t_start <- if (isTRUE(progress)) Sys.time() else NULL

  .progress_msg <- function(done) {
    elapsed_s <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    rate      <- done / max(elapsed_s, 0.001)
    eta_s     <- if (done < total_anchors) (total_anchors - done) / rate else 0
    eta_str   <- if (done >= total_anchors) "done" else if (eta_s < 60) sprintf("%.0fs", eta_s) else if (eta_s < 3600) sprintf("%.1fmin", eta_s / 60) else sprintf("%.1fh", eta_s / 3600)
    tsb_message(sprintf(
      "Progress: %d/%d (%d%%) | elapsed: %.0fs | %.2f anchors/s | ETA: ~%s",
      done, total_anchors, as.integer(100 * done / total_anchors),
      elapsed_s, rate, eta_str
    ))
  }

  # Evaluate every candidate as an anchor under both the full donor pool and
  # the leave-one-species-out donor pool, optionally in parallel.
  if (workers <= 1L) {
    for (i in seq_len(total_anchors)) {
      anchor_row <- candidate_models[i, , drop = FALSE]
      base_eval_obj <- tryCatch(
        screen_one_anchor_admissibility(
          anchor_row = anchor_row,
          candidate_models = candidate_models,
          config = config_values,
          registry_path = registry_path,
          sim_obj = sim_obj,
          dist_obj = dist_obj,
          candidate_models_scored = candidate_models_scored
        ),
        error = function(e) {
          if (isTRUE(progress)) {
            tsb_message(
              "Anchor ", i, " skipped (admissibility error): ",
              conditionMessage(e)
            )
          }
          NULL
        }
      )
      if (is.null(base_eval_obj)) {
        if (isTRUE(progress) && (i %% progress_step == 0L || i == total_anchors)) {
          .progress_msg(i)
        }
        next
      }
      ordination_info_now <- resolve_ordination_info(
        anchor_row = anchor_row,
        model_scores = model_scores,
        species_lookup = species_lookup,
        config = config_values
      )

      bench_obj <- if (isTRUE(run_pseudo_anchor)) {
        benchmark_one_anchor(
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
          eval_obj = base_eval_obj,
          config = config_values,
          registry_path = registry_path,
          scheme = "pseudo_anchor",
          species_block = FALSE,
          ordination_info = ordination_info_now
        )
      } else {
        NULL
      }

      .seq_strip <- function(tbl) {
        if (is.null(tbl) || length(worker_strip_cols) == 0L) return(tbl)
        tbl[, !names(tbl) %in% worker_strip_cols, drop = FALSE]
      }

      if (!is.null(bench_obj)) {
        perf_rows[[length(perf_rows) + 1]] <- .seq_strip(bench_obj$perf)
        feat_rows[[length(feat_rows) + 1]] <- bench_obj$features
        err_rows[[length(err_rows) + 1]] <- bench_obj$ts_error
      }

      sb_obj <- if (isTRUE(run_species_block)) {
        benchmark_one_anchor(
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
          eval_obj = base_eval_obj,
          config = config_values,
          registry_path = registry_path,
          scheme = config_values$species_block_label,
          species_block = TRUE,
          ordination_info = ordination_info_now
        )
      } else {
        NULL
      }

      if (!is.null(sb_obj)) {
        sb_perf_rows[[length(sb_perf_rows) + 1]] <- .seq_strip(sb_obj$perf)
        sb_feat_rows[[length(sb_feat_rows) + 1]] <- sb_obj$features
      }

      if (isTRUE(run_group_block)) {
        gb_obj <- benchmark_one_anchor(
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
          eval_obj = base_eval_obj,
          config = config_values,
          registry_path = registry_path,
          scheme = group_block_label,
          species_block = FALSE,
          group_block_col = group_block_col,
          ordination_info = ordination_info_now
        )

        if (!is.null(gb_obj)) {
          gb_perf_rows[[length(gb_perf_rows) + 1]] <- .seq_strip(gb_obj$perf)
          gb_feat_rows[[length(gb_feat_rows) + 1]] <- gb_obj$features
        }
      }

      if (isTRUE(progress) && (i %% progress_step == 0L || i == total_anchors)) {
        .progress_msg(i)
      }
    }
  } else {
    cluster_obj <- initialize_parallel_cluster(
      workers = workers,
      package_dir = package_dir
    )
    on.exit(parallel::stopCluster(cluster_obj), add = TRUE)

    tsb_cluster_export(
      cluster_obj,
      c(
        "candidate_models",
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
        "include_ts_error",
        "run_pseudo_anchor",
        "run_species_block",
        "run_group_block",
        "group_block_col",
        "group_block_label",
        "worker_strip_cols",
        "policy_meta_lookup",
        "anchor_meta_lookup"
      ),
      envir = environment()
    )

    if (isTRUE(progress)) {
      tsb_message("Policy benchmark running in parallel with ", workers, " workers.")
    }

    chunk_step <- max(as.integer(workers) * 4L, min(100L, total_anchors))
    chunk_index <- split(
      seq_len(total_anchors),
      ceiling(seq_len(total_anchors) / chunk_step)
    )
    processed <- 0L
    for (ids in chunk_index) {
      chunk_results <- parallel::parLapplyLB(
        cluster_obj,
        as.list(ids),
        function(i) {
          {
            anchor_row <- candidate_models[i, , drop = FALSE]
            adm_result <- tryCatch(
              list(obj = tsbiomass:::screen_one_anchor_admissibility(
                anchor_row = anchor_row,
                candidate_models = candidate_models,
                config = config_values,
                registry_path = registry_path,
                sim_obj = sim_obj,
                dist_obj = dist_obj,
                candidate_models_scored = candidate_models_scored
              ), err = NULL),
              error = function(e) list(obj = NULL, err = conditionMessage(e))
            )
            if (is.null(adm_result$obj)) {
              return(list(
                bench = NULL, species_block = NULL, group_block = NULL,
                adm_error = adm_result$err, anchor_index = i
              ))
            }
            base_eval_obj <- adm_result$obj
            ordination_info_now <- tsbiomass:::resolve_ordination_info(
              anchor_row = anchor_row,
              model_scores = model_scores,
              species_lookup = species_lookup,
              config = config_values
            )

            bench_obj <- if (isTRUE(run_pseudo_anchor)) {
              tsbiomass:::benchmark_one_anchor(
                anchor_row = anchor_row,
                candidate_models = candidate_models,
                policy_fun = tsbiomass:::evaluate_policies,
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
                eval_obj = base_eval_obj,
                config = config_values,
                registry_path = registry_path,
                scheme = "pseudo_anchor",
                species_block = FALSE,
                ordination_info = ordination_info_now
              )
            } else {
              NULL
            }

            sb_obj <- if (isTRUE(run_species_block)) {
              tsbiomass:::benchmark_one_anchor(
                anchor_row = anchor_row,
                candidate_models = candidate_models,
                policy_fun = tsbiomass:::evaluate_policies,
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
                eval_obj = base_eval_obj,
                config = config_values,
                registry_path = registry_path,
                scheme = config_values$species_block_label,
                species_block = TRUE,
                ordination_info = ordination_info_now
              )
            } else {
              NULL
            }

            gb_obj <- NULL
            if (isTRUE(run_group_block)) {
              gb_obj <- tsbiomass:::benchmark_one_anchor(
                anchor_row = anchor_row,
                candidate_models = candidate_models,
                policy_fun = tsbiomass:::evaluate_policies,
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
                eval_obj = base_eval_obj,
                config = config_values,
                registry_path = registry_path,
                scheme = group_block_label,
                species_block = FALSE,
                group_block_col = group_block_col,
                ordination_info = ordination_info_now
              )
            }

            .strip_cols <- function(tbl) {
              if (is.null(tbl) || length(worker_strip_cols) == 0L) return(tbl)
              drop <- names(tbl) %in% worker_strip_cols
              tbl[, !drop, drop = FALSE]
            }
            if (!is.null(bench_obj)) bench_obj$perf <- .strip_cols(bench_obj$perf)
            if (!is.null(sb_obj))    sb_obj$perf    <- .strip_cols(sb_obj$perf)
            if (!is.null(gb_obj))    gb_obj$perf    <- .strip_cols(gb_obj$perf)

            list(bench = bench_obj, species_block = sb_obj, group_block = gb_obj)
          }
        }
      )

      for (one_result in chunk_results) {
        if (!is.null(one_result$bench)) {
          perf_rows[[length(perf_rows) + 1]] <- one_result$bench$perf
          feat_rows[[length(feat_rows) + 1]] <- one_result$bench$features
          err_rows[[length(err_rows) + 1]] <- one_result$bench$ts_error
        } else if (isTRUE(progress) && !is.null(one_result$adm_error)) {
          tsb_message(
            "Anchor ", one_result$anchor_index, " skipped (admissibility error): ",
            one_result$adm_error
          )
        }
        if (!is.null(one_result$species_block)) {
          sb_perf_rows[[length(sb_perf_rows) + 1]] <- one_result$species_block$perf
          sb_feat_rows[[length(sb_feat_rows) + 1]] <- one_result$species_block$features
        }
        if (!is.null(one_result$group_block)) {
          gb_perf_rows[[length(gb_perf_rows) + 1]] <- one_result$group_block$perf
          gb_feat_rows[[length(gb_feat_rows) + 1]] <- one_result$group_block$features
        }
        processed <- processed + 1L
      }
      if (isTRUE(progress)) .progress_msg(processed)
    }
  }

  perf_tbl     <- dplyr::bind_rows(perf_rows)
  feat_tbl     <- dplyr::bind_rows(feat_rows)
  err_tbl      <- dplyr::bind_rows(err_rows)
  sb_perf_tbl  <- dplyr::bind_rows(sb_perf_rows)
  sb_feat_tbl  <- dplyr::bind_rows(sb_feat_rows)
  gb_perf_tbl  <- dplyr::bind_rows(gb_perf_rows)
  gb_feat_tbl  <- dplyr::bind_rows(gb_feat_rows)

  if (nrow(perf_tbl)    > 0L) perf_tbl$validation_scheme    <- "pseudo_anchor"
  if (nrow(sb_perf_tbl) > 0L) sb_perf_tbl$validation_scheme <- config_values$species_block_label
  if (nrow(gb_perf_tbl) > 0L) gb_perf_tbl$validation_scheme <- group_block_label

  if (ncol(perf_tbl) == 0L) {
    warning(
      "Policy benchmark produced no rows. All ", total_anchors, " anchor(s) ",
      "failed admissibility screening. Check that 'candidate_models' has valid ",
      "study length data (study_length_min / study_length_max) and that the ",
      "similarity traits in the config match the available columns. ",
      "Set 'progress = TRUE' and inspect per-anchor diagnostic messages.",
      call. = FALSE
    )
  }

  perf_tbl    <- .rejoin_perf(perf_tbl,    policy_meta_lookup, anchor_meta_lookup)
  sb_perf_tbl <- .rejoin_perf(sb_perf_tbl, policy_meta_lookup, anchor_meta_lookup)
  gb_perf_tbl <- .rejoin_perf(gb_perf_tbl, policy_meta_lookup, anchor_meta_lookup)

  .recompute_perf_derived <- function(tbl) {
    if (is.null(tbl) || nrow(tbl) == 0L || !"multiplier_pred" %in% names(tbl)) return(tbl)
    tbl$error_abs_log    <- abs(log(tbl$multiplier_pred))
    tbl$valid_prediction <- is.finite(tbl$multiplier_pred) & tbl$multiplier_pred > 0
    tbl
  }
  perf_tbl    <- .recompute_perf_derived(perf_tbl)
  sb_perf_tbl <- .recompute_perf_derived(sb_perf_tbl)
  gb_perf_tbl <- .recompute_perf_derived(gb_perf_tbl)

  if (nrow(err_tbl) > 0L && "anchor_model_id" %in% names(err_tbl) &&
      "anchor_species" %in% names(anchor_meta_lookup) &&
      !"anchor_species" %in% names(err_tbl)) {
    err_tbl <- dplyr::left_join(
      err_tbl,
      anchor_meta_lookup[, c("anchor_model_id", "anchor_species"), drop = FALSE],
      by = "anchor_model_id"
    )
  }

  result <- list(
    policy_perf = perf_tbl,
    anchor_features = feat_tbl,
    best_policy = bind_best_policy_rows(perf_tbl),
    policy_ts_error = err_tbl,
    species_block_perf = sb_perf_tbl,
    species_block_features = sb_feat_tbl,
    species_block_best = bind_best_policy_rows(sb_perf_tbl),
    group_block_perf = gb_perf_tbl,
    group_block_features = gb_feat_tbl,
    group_block_best = bind_best_policy_rows(gb_perf_tbl),
    group_block_col = group_block_col,
    group_block_label = group_block_label
  )

  # Persist the in-memory benchmark object only when the caller requested a
  # cache path.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  result
}


