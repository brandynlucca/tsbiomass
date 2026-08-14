#' Compute the finite-sample conformal quantile
#'
#' Returns the order-statistic conformal threshold used throughout the package.
#'
#' @param x Numeric residual vector.
#' @param alpha Miscoverage level. May be a scalar or numeric vector.
#'
#' @return Numeric scalar when `alpha` has length 1, otherwise a named numeric
#'   vector.
#'
#' @keywords internal
#' @noRd
conformal_quantile <- function(x,
                               alpha) {
  # Sort once so vector alpha inputs reuse the same ordered residuals.
  x_ <- sort(x[is.finite(x)])
  n <- length(x_)
  if (n == 0) {
    alpha_ <- suppressWarnings(as.numeric(alpha))
    out <- rep(NA_real_, length(alpha_))
    if (length(out) == 1L) {
      return(NA_real_)
    }
    names(out) <- if (!is.null(names(alpha))) {
      names(alpha)
    } else {
      paste0(
        "alpha_",
        format(alpha_, trim = TRUE, scientific = FALSE)
      )
    }
    return(out)
  }

  # Evaluate one finite-sample conformal order statistic per alpha.
  alpha_ <- suppressWarnings(as.numeric(alpha))
  out <- vapply(
    alpha_,
    function(alpha_now) {
      if (!is.finite(alpha_now)) {
        return(NA_real_)
      }
      q_idx <- ceiling((n + 1) * (1 - alpha_now))
      if (q_idx > n) {
        # The strict split-conformal order statistic is +Inf in this edge case.
        # That is mathematically conservative, but it is not operationally
        # useful for this package because it destroys downstream interval
        # objects and plots when the calibration sample is small. Use the
        # largest observed finite residual instead.
        return(x_[[n]])
      }
      x_[[q_idx]]
    },
    numeric(1)
  )

  if (length(out) == 1L) {
    return(out[[1]])
  }

  names(out) <- if (!is.null(names(alpha))) {
    names(alpha)
  } else {
    paste0(
      "alpha_",
      format(alpha_, trim = TRUE, scientific = FALSE)
    )
  }
  out
}


#' Compute a post-selection support score
#'
#' @param data Selected policy rows.
#'
#' @return Numeric vector; larger values indicate stronger local support.
#'
#' @keywords internal
#' @noRd
post_selection_support_score <- function(data) {
  data <- tibble::as_tibble(data)
  n <- nrow(data)
  if (n == 0) {
    return(numeric())
  }

  # Pull the available support, distance, and structural-spread fields through
  # one small accessor so the score can work across both benchmark and anchor
  # prediction tables.
  col_or_na <- function(nm) {
    if (nm %in% names(data)) {
      data[[nm]]
    } else {
      rep(NA_real_, n)
    }
  }

  local_distance <- dplyr::coalesce(
    col_or_na("anchor_selection_local_distance"),
    col_or_na("local_weighted_mean_combined_distance"),
    col_or_na("local_min_combined_distance")
  )
  effective_support <- dplyr::coalesce(
    col_or_na("local_effective_support"),
    col_or_na("n_valid_models"),
    col_or_na("n_models")
  )
  if (any(is.finite(effective_support))) {
    score <- dplyr::percent_rank(log1p(effective_support))
    score[!is.finite(score)] <- NA_real_
    return(score)
  }

  n_valid <- dplyr::coalesce(
    col_or_na("n_valid_models"),
    col_or_na("n_models")
  )
  structural_spread <- dplyr::coalesce(
    col_or_na("local_structural_q_abs_log"),
    col_or_na("donor_log_sigma_abs_dev_q90")
  )

  distance_component <- dplyr::percent_rank(-local_distance)
  support_component <- dplyr::percent_rank(log1p(effective_support))
  count_component <- dplyr::percent_rank(log1p(n_valid))
  structural_component <- dplyr::percent_rank(-structural_spread)

  score <- rowMeans(
    cbind(distance_component, support_component, count_component, structural_component),
    na.rm = TRUE
  )
  score[!is.finite(score)] <- NA_real_
  score
}

#' Rescale a continuous support component onto a bounded score
#'
#' @param x Numeric vector to rescale.
#' @param direction Whether larger or smaller values imply stronger support.
#'
#' @return Numeric vector.
#'
#' @keywords internal
#' @noRd
continuous_support_component <- function(x,
                                         direction = c("high", "low")) {
  direction_ <- match.arg(direction)
  x_ <- suppressWarnings(as.numeric(x))
  out <- rep(NA_real_, length(x_))
  ok <- is.finite(x_)
  if (!any(ok)) {
    return(out)
  }
  if (identical(direction_, "high")) {
    out[ok] <- 1 - exp(-pmax(x_[ok], 0))
  } else {
    out[ok] <- 1 / (1 + pmax(x_[ok], 0))
  }
  out
}

#' Compute continuous local support scores
#'
#' @param data Policy prediction or calibration rows.
#'
#' @return Numeric vector approximately between 0 and 1; larger values indicate
#'   stronger transfer support.
#'
#' @keywords internal
#' @noRd
continuous_support_score <- function(data) {
  data <- tibble::as_tibble(data)
  n <- nrow(data)
  if (n == 0) {
    return(numeric())
  }
  num_col <- function(nm) {
    if (nm %in% names(data)) {
      suppressWarnings(as.numeric(data[[nm]]))
    } else {
      rep(NA_real_, n)
    }
  }
  first_finite <- function(...) {
    vals <- list(...)
    out <- rep(NA_real_, n)
    for (v in vals) {
      replace <- !is.finite(out) & is.finite(v)
      out[replace] <- v[replace]
    }
    out
  }

  distance <- first_finite(
    num_col("weighted_mean_combined_distance"),
    num_col("min_combined_distance"),
    num_col("local_weighted_mean_combined_distance"),
    num_col("local_min_combined_distance"),
    num_col("anchor_selection_local_distance")
  )
  source_cells <- first_finite(
    num_col("n_independent_source_cells"),
    num_col("effective_donor_n"),
    num_col("local_effective_support"),
    num_col("n_donor_models"),
    num_col("n_models")
  )
  effective_n <- first_finite(
    num_col("effective_donor_n"),
    num_col("local_effective_support"),
    num_col("n_independent_source_cells"),
    num_col("n_donor_models"),
    num_col("n_models")
  )
  structural_spread <- first_finite(
    num_col("local_structural_q_abs_log"),
    num_col("q_abs_log_structural"),
    num_col("donor_log_sigma_abs_dev_q90"),
    num_col("donor_curve_rmse_q90")
  )
  length_overlap <- first_finite(
    num_col("min_length_overlap_fraction"),
    num_col("local_mean_length_overlap"),
    num_col("length_overlap_fraction")
  )
  depth_overlap <- first_finite(
    num_col("min_depth_overlap_fraction"),
    num_col("local_mean_depth_overlap"),
    num_col("depth_overlap_fraction")
  )

  components <- cbind(
    continuous_support_component(distance, "low"),
    continuous_support_component(source_cells / 5, "high"),
    continuous_support_component(effective_n / 5, "high"),
    continuous_support_component(structural_spread, "low"),
    pmin(pmax(length_overlap, 0), 1),
    pmin(pmax(depth_overlap, 0), 1)
  )
  score <- rowMeans(components, na.rm = TRUE)
  score[!is.finite(score)] <- NA_real_
  score
}

#' Return default post-selection support labels
#'
#' @param n_bins Number of support bins.
#'
#' @return Character vector of default labels.
#'
#' @keywords internal
#' @noRd
default_post_selection_support_labels <- function(n_bins) {
  n_bins_ <- max(1L, as.integer(n_bins)[[1]])
  switch(as.character(n_bins_),
    `1` = c("All support"),
    `2` = c("Lower support", "Higher support"),
    `3` = c("Lower support", "Moderate support", "Higher support"),
    `4` = c(
      "Lowest support",
      "Lower support",
      "Higher support",
      "Highest support"
    ),
    `5` = c(
      "Lowest support",
      "Lower support",
      "Middle support",
      "Higher support",
      "Highest support"
    ),
    paste("Support tier", seq_len(n_bins_))
  )
}

#' Resolve validated post-selection support labels
#'
#' @param labels Optional label vector supplied by the caller.
#' @param n_bins Number of support bins.
#'
#' @return Named character vector keyed by support-bin code.
#'
#' @keywords internal
#' @noRd
resolve_post_selection_support_labels <- function(labels = NULL,
                                                  n_bins = 3L) {
  # Validate the configured labels against the resolved bin count.
  n_bins_ <- max(1L, as.integer(n_bins)[[1]])
  labels_ <- if (is.null(labels)) {
    default_post_selection_support_labels(n_bins_)
  } else {
    labels
  }
  labels_ <- stringr::str_squish(as.character(unlist(labels_, use.names = FALSE)))
  labels_ <- labels_[!is.na(labels_) & nzchar(labels_)]
  if (length(labels_) != n_bins_) {
    stop(
      sprintf(
        "'support_bin_labels' must contain exactly %d non-empty label(s).",
        n_bins_
      ),
      call. = FALSE
    )
  }
  if (anyDuplicated(labels_)) {
    stop("'support_bin_labels' must be unique.", call. = FALSE)
  }
  stats::setNames(labels_, paste0("support_bin_", seq_len(n_bins_)))
}

#' Assign generic support bins
#'
#' @param data Input table.
#' @param score Numeric support score vector aligned with `data`.
#' @param score_name Name of the output score column.
#' @param bin_name Name of the output support-bin column.
#' @param cutpoints Optional numeric cutpoints. When `NULL`, quantile cutpoints
#'   are computed from `score`.
#' @param n_bins Number of ordered support bins.
#' @param all_bin Label used when all rows collapse into one support tier.
#' @param missing_bin Label used when a row has missing support.
#'
#' @return `data` with appended score and bin columns.
#'
#' @keywords internal
#' @noRd
assign_support_bins <- function(data,
                                score,
                                score_name,
                                bin_name,
                                cutpoints = NULL,
                                n_bins = 3L,
                                all_bin = "all_support",
                                missing_bin = "support_bin_missing") {
  # Standardize the input table and score before deriving cutpoints.
  data <- tibble::as_tibble(data)
  n_bins_ <- max(1L, as.integer(n_bins)[[1]])
  score_ <- suppressWarnings(as.numeric(score))

  if (nrow(data) == 0L) {
    data[[score_name]] <- numeric()
    data[[bin_name]] <- character()
    return(data)
  }

  # Build quantile cutpoints unless the caller passed fixed breaks.
  cutpoints_ <- if (is.null(cutpoints)) {
    probs <- seq(0, 1, length.out = n_bins_ + 1L)
    unique(stats::quantile(
      score_,
      probs = probs,
      na.rm = TRUE,
      names = FALSE,
      type = 8
    ))
  } else {
    suppressWarnings(as.numeric(cutpoints))
  }

  # Collapse to one bin when the score has no usable spread.
  if (length(cutpoints_) < 2L || all(!is.finite(cutpoints_))) {
    bin <- rep(all_bin, nrow(data))
  } else {
    cutpoints_[1] <- -Inf
    cutpoints_[length(cutpoints_)] <- Inf
    bin_id <- as.integer(cut(
      score_,
      breaks = cutpoints_,
      include.lowest = TRUE,
      labels = FALSE
    ))
    bin <- paste0("support_bin_", bin_id)
    bin[!is.finite(bin_id)] <- missing_bin
  }

  data[[score_name]] <- score_
  data[[bin_name]] <- bin
  data
}

#' Attach display labels to post-selection support bins
#'
#' @param data Table containing `post_selection_support_bin`.
#' @param labels Optional support-bin labels.
#' @param n_bins Optional number of bins.
#'
#' @return Tibble with `post_selection_support_label`.
#'
#' @keywords internal
#' @noRd
label_post_selection_support_bins <- function(data,
                                              labels = NULL,
                                              n_bins = NULL) {
  # Attach human-readable labels after the structural bins are present.
  data <- tibble::as_tibble(data)
  if (!"post_selection_support_bin" %in% names(data)) {
    return(data)
  }
  bin_values <- as.character(data$post_selection_support_bin)
  n_bins_ <- n_bins %||% max(
    suppressWarnings(as.integer(gsub("^support_bin_", "", bin_values))),
    na.rm = TRUE
  )
  if (!is.finite(n_bins_) || n_bins_ < 1) {
    n_bins_ <- 3L
  }
  label_map <- resolve_post_selection_support_labels(labels = labels, n_bins = n_bins_)
  data$post_selection_support_label <- unname(label_map[bin_values])
  missing_labels <- is.na(data$post_selection_support_label)
  data$post_selection_support_label[
    missing_labels & bin_values == "support_bin_missing"
  ] <- "Missing support"
  data$post_selection_support_label[
    missing_labels & bin_values == "all_support"
  ] <- "All support"
  data
}

#' Assign post-selection support bins
#'
#' @param data Selected policy rows.
#' @param cutpoints Optional numeric cutpoints. When `NULL`, cutpoints are
#'   estimated from `data`.
#' @param n_bins Number of ordered support bins.
#'
#' @return `data` with `post_selection_support_score` and
#'   `post_selection_support_bin`.
#'
#' @keywords internal
#' @noRd
assign_post_selection_support_bins <- function(data,
                                               cutpoints = NULL,
                                               n_bins = 3L,
                                               labels = NULL) {
  # Score the rows first, then assign support bins from that score.
  data <- tibble::as_tibble(data)
  if (nrow(data) == 0L) {
    data$post_selection_support_score <- numeric()
    data$post_selection_support_bin <- character()
    data$post_selection_support_label <- character()
    return(data)
  }

  n_bins_ <- max(1L, as.integer(n_bins))
  data <- assign_support_bins(
    data = data,
    score = post_selection_support_score(data),
    score_name = "post_selection_support_score",
    bin_name = "post_selection_support_bin",
    cutpoints = cutpoints,
    n_bins = n_bins_,
    all_bin = "all_support",
    missing_bin = "support_bin_missing"
  )
  label_post_selection_support_bins(data, labels = labels, n_bins = n_bins_)
}


#' Split species for one nested validation fold
#'
#' @param species Character vector of species names.
#' @param outer_species Species held out for final testing.
#' @param selection_fraction Fraction of non-test species assigned to policy
#'   selection.
#' @param calibration_fraction Fraction of non-test species assigned to
#'   conformal calibration.
#' @param seed Integer seed.
#'
#' @return A list with `tune`, `selection`, and `calibration` species vectors.
#' @keywords internal
#' @noRd
split_nested_species <- function(species,
                                 outer_species,
                                 selection_fraction,
                                 calibration_fraction,
                                 seed) {
  species_ <- sort(unique(stats::na.omit(as.character(species))))
  remaining <- setdiff(species_, outer_species)
  if (length(remaining) < 4) {
    stop(
      "Nested validation requires at least four non-test species.",
      call. = FALSE
    )
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
    stop(
      paste(
        "Nested validation split produced an empty tune,",
        "selection, or calibration set."
      ),
      call. = FALSE
    )
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
#' @noRd
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
#' @noRd
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
    model_scores = tibble::tibble(model_id = character(), nmds_cluster = character()),
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
      dplyr::distinct(.data$model_id, .keep_all = TRUE)

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
        valid_prediction = is.finite(.data$multiplier_pred) & .data$multiplier_pred > 0,
        error_log = dplyr::if_else(.data$valid_prediction, log(.data$multiplier_pred), NA_real_),
        error_abs_log = abs(.data$error_log)
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
#' @noRd
summarize_nested_residuals <- function(perf_tbl,
                                       alpha,
                                       min_n = 10L) {
  perf_tbl_ <- tibble::as_tibble(perf_tbl)
  if (nrow(perf_tbl_) == 0) {
    return(tibble::tibble())
  }
  perf_tbl_$policy <- resolve_policy_names(perf_tbl_)

  perf_tbl_ |>
    dplyr::filter(.data$valid_prediction, is.finite(.data$error_abs_log)) |>
    dplyr::mutate(
      anchor_selection_local_distance = dplyr::coalesce(
        .data$local_weighted_mean_combined_distance,
        .data$local_min_combined_distance
      )
    ) |>
    dplyr::group_by(.data$policy) |>
    dplyr::summarise(
      n = dplyr::n(),
      q_abs_log = conformal_quantile(.data$error_abs_log, alpha),
      interval_log_width = dplyr::if_else(is.finite(.data$q_abs_log), 2 * .data$q_abs_log, NA_real_),
      median_abs_log_error = stats::median(.data$error_abs_log, na.rm = TRUE),
      mean_abs_log_error = mean(.data$error_abs_log, na.rm = TRUE),
      median_local_combined_distance = stats::median(.data$anchor_selection_local_distance, na.rm = TRUE),
      min_local_combined_distance = suppressWarnings(min(.data$anchor_selection_local_distance, na.rm = TRUE)),
      median_local_effective_support = stats::median(.data$local_effective_support, na.rm = TRUE),
      eligible_n = .data$n >= min_n,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      median_local_combined_distance = dplyr::if_else(
        is.infinite(.data$median_local_combined_distance),
        NA_real_,
        .data$median_local_combined_distance
      ),
      min_local_combined_distance = dplyr::if_else(
        is.infinite(.data$min_local_combined_distance),
        NA_real_,
        .data$min_local_combined_distance
      ),
      median_local_effective_support = dplyr::if_else(
        is.infinite(.data$median_local_effective_support),
        NA_real_,
        .data$median_local_effective_support
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
#' @noRd
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
    dplyr::filter(.data$species_name %in% split_obj$tune)
  selection_models <- candidate_models |>
    dplyr::filter(.data$species_name %in% split_obj$selection)
  calibration_models <- candidate_models |>
    dplyr::filter(.data$species_name %in% split_obj$calibration)
  test_models <- candidate_models |>
    dplyr::filter(.data$species_name == task$outer_species)

  tune_obj <- tune_similarities(
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
    dplyr::filter(.data$eligible_n, is.finite(.data$interval_log_width))
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
        uncertainty_eligible = .data$eligible_n,
        uncertainty_cost_log_width = .data$interval_log_width,
        species_block_median_abs_log_error = .data$median_abs_log_error,
        local_weighted_mean_combined_distance = .data$median_local_combined_distance,
        local_min_combined_distance = .data$min_local_combined_distance,
        local_effective_support = .data$median_local_effective_support
      ) |>
      select_anchor_policies(
        uncertainty_rule = normalize_uncertainty_rule(selection_config$uncertainty_rule %||% "tolerance"),
        u_tol_rel = selection_config$u_tol_rel %||% selection_config$uncertainty_relative_tolerance %||% 0.25,
        u_tol_abs = selection_config$u_tol_abs %||% selection_config$uncertainty_absolute_tolerance %||% 0.05,
        one_se_multiplier = selection_config$one_se_multiplier %||% 1,
        local_distance_tolerance = selection_config$local_distance_tolerance %||% 1e-12
      ) |>
      dplyr::transmute(
        fold_id = .data$fold_id,
        repeat_id = .data$repeat_id,
        selected_policy = .data$policy,
        selection_interval_log_width = .data$uncertainty_cost_log_width,
        selection_n = .data$n,
        selection_median_abs_log_error = .data$median_abs_log_error,
        selection_local_distance = .data$anchor_selection_local_distance,
        tuned_alpha = .data$tuned_alpha,
        tuned_k_species = .data$tuned_k_species,
        tuned_k_study = .data$tuned_k_study,
        selection_tier = .data$selection_tier
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
        dplyr::filter(!is.na(.data$selected_policy)) |>
        dplyr::select("fold_id", "repeat_id", "selected_policy", "selection_interval_log_width"),
      by = c("fold_id", "repeat_id", "policy" = "selected_policy")
    ) |>
    dplyr::left_join(
      calibration_summary |>
        dplyr::select("fold_id", "repeat_id", "policy", calibration_n = "n", q_abs_log = "q_abs_log", calibration_interval_log_width = "interval_log_width"),
      by = c("fold_id", "repeat_id", "policy")
    ) |>
    dplyr::mutate(
      multiplier_lo = dplyr::if_else(.data$valid_prediction & is.finite(.data$q_abs_log), .data$multiplier_pred * exp(-.data$q_abs_log), NA_real_),
      multiplier_hi = dplyr::if_else(.data$valid_prediction & is.finite(.data$q_abs_log), .data$multiplier_pred * exp(.data$q_abs_log), NA_real_),
      covered = .data$valid_prediction & is.finite(.data$q_abs_log) & .data$error_abs_log <= .data$q_abs_log
    )

  alpha_k_audit <- dplyr::bind_rows(
    selection_perf |>
      dplyr::select("fold_id", "repeat_id", "tuned_alpha", "tuned_k_species", "tuned_k_study"),
    calibration_perf |>
      dplyr::select("fold_id", "repeat_id", "tuned_alpha", "tuned_k_species", "tuned_k_study"),
    test_perf |>
      dplyr::select("fold_id", "repeat_id", "tuned_alpha", "tuned_k_species", "tuned_k_study")
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
#' @keywords internal
#' @noRd
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
  config_ <- if (is_s7_instance(config, "Configurer")) {
    config@data
  } else {
    config
  }
  seed_ <- if (is.null(seed)) {
    sample.int(.Machine$integer.max, 1)
  } else {
    seed
  }
  selector_obj <- if (is_s7_instance(candidate_models, "PolicySelector")) {
    candidate_models
  } else {
    NULL
  }
  candidates_obj <- if (is_s7_instance(candidate_models, "Candidates")) {
    candidate_models
  } else {
    NULL
  }
  candidate_models_ <- candidate_models
  if (!is.null(selector_obj)) {
    config_ <- config_ %||% selector_obj@config
    candidates_obj <- selector_obj@candidates
    candidate_models_ <- selector_obj@candidates
  } else if (!is.null(candidates_obj)) {
    config_ <- config_ %||% list(
      policy = merge_config_sections(
        default_anchor_config(list()),
        policy_selector_similarity_defaults(candidates_obj)
      )
    )
    candidate_models_ <- candidates_obj@candidate_models
  }
  if (!is.data.frame(candidate_models_)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }
  registered_policies <- available_policy_names(policy_path = policy_path)
  policies_ <- policies %||% registered_policies
  if (!is.character(policies_) || any(!nzchar(policies_))) {
    stop("'policies' must be a non-empty character vector.", call. = FALSE)
  }
  missing_policies <- setdiff(registered_policies, policies_)
  extra_policies <- setdiff(policies_, registered_policies)
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
  if (!is.list(config_)) {
    stop("'config' must be a list.", call. = FALSE)
  }
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }
  if (!is.numeric(workers) || length(workers) != 1 || !is.finite(workers) || workers < 1) {
    stop("'workers' must be one finite number >= 1.", call. = FALSE)
  }
  workers_ <- as.integer(workers)

  candidate_models_ <- tibble::as_tibble(candidate_models_)
  species_all <- sort(unique(stats::na.omit(as.character(candidate_models_$species_name))))
  if (length(species_all) < 5L) {
    stop("Nested validation requires at least five species.", call. = FALSE)
  }

  n_repeats_ <- as.integer(n_repeats)
  if (!is.finite(n_repeats_) || n_repeats_ < 1L) {
    stop("'n_repeats' must be one integer >= 1.", call. = FALSE)
  }
  n_outer_folds_ <- if (is.null(n_outer_folds)) {
    length(species_all)
  } else {
    n_outer_folds
  }
  n_outer_folds_ <- min(as.integer(n_outer_folds_), length(species_all))
  if (!is.finite(n_outer_folds_) || n_outer_folds_ < 1L) {
    stop("'n_outer_folds' must be NULL or one integer >= 1.", call. = FALSE)
  }

  base_policy_config <- config_$policy %||% config_
  tuning_config <- config_$tuning %||% list()
  selection_config <- config_$selection %||% list()

  task_rows <- list()
  fold_id <- 0L
  for (rep_id in seq_len(n_repeats_)) {
    set.seed(as.integer(seed_) + rep_id)
    outer_species_vec <- sample(species_all, length(species_all), replace = FALSE)
    outer_species_vec <- outer_species_vec[seq_len(n_outer_folds_)]

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
      candidate_models = candidate_models_,
      species_all = species_all,
      policies = policies_,
      base_policy_config = base_policy_config,
      tuning_config = tuning_config,
      selection_fraction = selection_fraction,
      calibration_fraction = calibration_fraction,
      min_selection_n = min_selection_n,
      min_calibration_n = min_calibration_n,
      selection_config = selection_config,
      alpha = alpha,
      seed = seed_,
      policy_path = policy_path,
      registry_path = registry_path
    )
  }

  if (workers_ > 1L && length(task_rows) > 1L) {
    cluster_obj <- initialize_parallel_cluster(workers = min(workers_, length(task_rows)))
    on.exit(parallel::stopCluster(cluster_obj), add = TRUE)
    tsb_cluster_export(
      cluster_obj,
      c(
        "task_rows", "candidate_models_", "species_all", "policies_",
        "base_policy_config", "tuning_config", "selection_fraction",
        "calibration_fraction", "min_selection_n", "min_calibration_n",
        "selection_config", "alpha", "seed_", "policy_path", "registry_path",
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
    dplyr::filter(.data$valid_prediction, is.finite(.data$q_abs_log)) |>
    dplyr::group_by(.data$policy) |>
    dplyr::summarise(
      n_test = dplyr::n(),
      empirical_coverage = mean(.data$covered, na.rm = TRUE),
      median_interval_log_width = stats::median(.data$calibration_interval_log_width, na.rm = TRUE),
      median_abs_log_error = stats::median(.data$error_abs_log, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$median_interval_log_width, .data$policy)

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


#' Compute conformal calibration scores
#'
#' Summarizes policy-specific absolute log-error quantiles from a benchmark
#' table so multiplicative conformal intervals can be applied later.
#'
#' @param policy_perf Policy-performance table.
#' @param alpha Miscoverage level.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
compute_conformal_scores <- function(policy_perf,
                                     alpha = 0.10) {
  # Restrict the calibration set to valid finite policy predictions before
  # summarizing the per-policy absolute log-error distribution.
  policy_perf_ <- normalize_policy_columns(policy_perf)
  policy_perf_$policy <- resolve_policy_names(policy_perf_)
  if (nrow(policy_perf_) == 0L || !"valid_prediction" %in% names(policy_perf_)) {
    return(tibble::tibble(
      policy = character(),
      equation_branch_filter = character(),
      n = integer(),
      q_abs_log = numeric(),
      median_abs_log = numeric()
    ))
  }
  policy_perf_ |>
    dplyr::filter(.data$valid_prediction, is.finite(.data$error_abs_log)) |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter) |>
    dplyr::summarise(
      n = dplyr::n(),
      q_abs_log = {
        # Split conformal requires the ceiling-based order statistic:
        # q = sort(scores)[min(n, ceiling((n+1)*(1-alpha)))]
        # stats::quantile(..., type=8) interpolates between order stats and
        # can return a value below the required index, violating coverage.
        s <- sort(.data$error_abs_log)
        n_cal <- length(s)
        idx <- min(n_cal, ceiling((n_cal + 1L) * (1 - alpha)))
        if (n_cal == 0L) NA_real_ else s[[idx]]
      },
      median_abs_log = stats::median(.data$error_abs_log, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Summarize conformal performance
#'
#' Summarizes empirical conformal coverage, interval width, and signed/absolute
#' log-error by policy overall and by anchor species.
#'
#' @param policy_perf Policy-performance table.
#' @param conf_cal Conformal calibration table.
#' @param bench_label Benchmark label to attach to the summaries.
#'
#' @return A list with `overall` and `by_species`.
#'
#' @keywords internal
#' @noRd
summarize_conformal <- function(policy_perf,
                                conf_cal,
                                bench_label = "pseudo_anchor") {
  # Join the per-policy calibration quantiles back onto the benchmark table
  # before computing coverage and interval-width summaries.
  perf_aug <- normalize_policy_columns(policy_perf)
  perf_aug$policy <- resolve_policy_names(perf_aug)
  conf_cal_ <- normalize_policy_columns(conf_cal)
  if (nrow(perf_aug) == 0L || !"valid_prediction" %in% names(perf_aug)) {
    empty <- tibble::tibble(
      policy = character(),
      equation_branch_filter = character(),
      benchmark_label = character(),
      n = integer(),
      empirical_coverage = numeric(),
      median_interval_log_width = numeric(),
      mean_signed_log_error = numeric(),
      median_signed_log_error = numeric(),
      mean_abs_log_error = numeric(),
      median_abs_log_error = numeric()
    )
    return(list(overall = empty, by_species = empty))
  }
  perf_aug <- perf_aug |>
    dplyr::filter(
      .data$valid_prediction,
      is.finite(.data$multiplier_pred),
      .data$multiplier_pred > 0,
      is.finite(.data$error_abs_log)
    ) |>
    dplyr::left_join(
      tibble::as_tibble(conf_cal_) |>
        dplyr::select("policy", "equation_branch_filter", "q_abs_log"),
      by = c("policy", "equation_branch_filter")
    ) |>
    dplyr::mutate(
      covered = is.finite(.data$q_abs_log) & .data$error_abs_log <= .data$q_abs_log,
      interval_log_width = 2 * .data$q_abs_log,
      signed_log_error = log(.data$multiplier_pred)
    )

  # Summarize the calibrated benchmark at the policy level first.
  overall <- perf_aug |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter) |>
    dplyr::summarise(
      benchmark_label = bench_label,
      n = dplyr::n(),
      empirical_coverage = mean(.data$covered, na.rm = TRUE),
      median_interval_log_width = stats::median(.data$interval_log_width, na.rm = TRUE),
      mean_signed_log_error = mean(.data$signed_log_error, na.rm = TRUE),
      median_signed_log_error = stats::median(.data$signed_log_error, na.rm = TRUE),
      mean_abs_log_error = mean(.data$error_abs_log, na.rm = TRUE),
      median_abs_log_error = stats::median(.data$error_abs_log, na.rm = TRUE),
      .groups = "drop"
    )

  # Repeat the same summary by anchor species so species-level calibration
  # differences can be inspected downstream.
  by_species <- perf_aug |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter, .data$anchor_species) |>
    dplyr::summarise(
      benchmark_label = bench_label,
      n = dplyr::n(),
      empirical_coverage = mean(.data$covered, na.rm = TRUE),
      median_interval_log_width = stats::median(.data$interval_log_width, na.rm = TRUE),
      mean_signed_log_error = mean(.data$signed_log_error, na.rm = TRUE),
      median_signed_log_error = stats::median(.data$signed_log_error, na.rm = TRUE),
      mean_abs_log_error = mean(.data$error_abs_log, na.rm = TRUE),
      median_abs_log_error = stats::median(.data$error_abs_log, na.rm = TRUE),
      .groups = "drop"
    )

  list(overall = overall, by_species = by_species)
}

#' Smooth TS calibration curves
#'
#' Applies spline smoothing to policy-by-relative-length calibration summaries.
#'
#' @param ts_cal Policy-by-relative-length calibration table.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
smooth_ts_calibration <- function(ts_cal) {
  # Return early when no TS calibration rows were available to smooth.
  if (nrow(ts_cal) == 0) {
    return(tibble::as_tibble(ts_cal))
  }

  smooth_one <- function(x, y) {
    # Preserve the original values when too few finite points exist for a
    # stable spline fit.
    keep <- is.finite(x) & is.finite(y)
    if (sum(keep) < 4) {
      return(y)
    }

    fit <- tryCatch(
      stats::smooth.spline(x[keep], y[keep], spar = 0.6),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(y)
    }

    out <- y
    out[keep] <- stats::predict(fit, x = x[keep])$y
    out
  }

  # Smooth each policy independently so the relative-length calibration shape
  # is preserved within policy.
  tibble::as_tibble(ts_cal) |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter) |>
    dplyr::arrange(.data$u, .by_group = TRUE) |>
    dplyr::mutate(
      median_ts_error_smooth = smooth_one(.data$u, .data$median_ts_error),
      q80_ts_abs_raw_smooth = smooth_one(.data$u, .data$q80_ts_abs_raw),
      q90_ts_abs_raw_smooth = smooth_one(.data$u, .data$q90_ts_abs_raw),
      q95_ts_abs_raw_smooth = smooth_one(.data$u, .data$q95_ts_abs_raw),
      q99_ts_abs_raw_smooth = smooth_one(.data$u, .data$q99_ts_abs_raw),
      q80_ts_abs_dev_smooth = smooth_one(.data$u, .data$q80_ts_abs_dev),
      q90_ts_abs_dev_smooth = smooth_one(.data$u, .data$q90_ts_abs_dev),
      q95_ts_abs_dev_smooth = smooth_one(.data$u, .data$q95_ts_abs_dev),
      q99_ts_abs_dev_smooth = smooth_one(.data$u, .data$q99_ts_abs_dev),
      median_log_sigma_residual_smooth = smooth_one(.data$u, .data$median_log_sigma_residual),
      q10_log_sigma_residual_smooth = smooth_one(.data$u, .data$q10_log_sigma_residual),
      q90_log_sigma_residual_smooth = smooth_one(.data$u, .data$q90_log_sigma_residual),
      q05_log_sigma_residual_smooth = smooth_one(.data$u, .data$q05_log_sigma_residual),
      q95_log_sigma_residual_smooth = smooth_one(.data$u, .data$q95_log_sigma_residual),
      q025_log_sigma_residual_smooth = smooth_one(.data$u, .data$q025_log_sigma_residual),
      q975_log_sigma_residual_smooth = smooth_one(.data$u, .data$q975_log_sigma_residual),
      q005_log_sigma_residual_smooth = smooth_one(.data$u, .data$q005_log_sigma_residual),
      q995_log_sigma_residual_smooth = smooth_one(.data$u, .data$q995_log_sigma_residual)
    ) |>
    dplyr::ungroup()
}

#' Summarize TS conformal calibration
#'
#' Summarizes TS and sigma-residual error by policy and relative length, then
#' smooths the resulting calibration curves.
#'
#' @param ts_error Policy TS-error table.
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
summarize_ts_calibration <- function(ts_error) {
  # Return an empty tibble when no TS-length error rows are available.
  if (nrow(ts_error) == 0) {
    return(tibble::tibble())
  }

  # Summarize the raw TS and sigma residuals by policy and relative length
  # before applying the smoother.
  ts_cal <- normalize_policy_columns(ts_error)
  ts_cal$policy <- resolve_policy_names(ts_cal)
  ts_cal <- ts_cal |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter, .data$u) |>
    dplyr::summarise(
      n = dplyr::n(),
      .calibration_summary = list(calibration_summary_values(
        ts_error = .data$ts_error,
        log_sigma_residual = .data$log_sigma_residual
      )),
      .groups = "drop"
    ) |>
    tidyr::unnest_wider(.data$.calibration_summary)

  smooth_ts_calibration(ts_cal)
}

#' Smooth selected-policy TS calibration curves
#'
#' @param ts_cal Selected-policy TS calibration table.
#'
#' @return Smoothed calibration tibble.
#'
#' @keywords internal
#' @noRd
smooth_selected_ts_calibration <- function(ts_cal) {
  # Smooth each calibration summary against normalized length within the selected-policy strata.
  if (nrow(ts_cal) == 0) {
    return(tibble::as_tibble(ts_cal))
  }

  ts_cal_ <- tibble::as_tibble(ts_cal)
  for (nm in c(
    "q80_log_sigma_abs_dev",
    "q90_log_sigma_abs_dev",
    "q95_log_sigma_abs_dev",
    "q99_log_sigma_abs_dev"
  )) {
    if (!nm %in% names(ts_cal_)) {
      ts_cal_[[nm]] <- NA_real_
    }
  }

  smooth_one <- function(x, y) {
    keep <- is.finite(x) & is.finite(y)
    if (sum(keep) < 4) {
      return(y)
    }
    fit <- tryCatch(
      stats::smooth.spline(x[keep], y[keep], spar = 0.6),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(y)
    }
    out <- y
    out[keep] <- stats::predict(fit, x = x[keep])$y
    out
  }

  ts_cal_ |>
    dplyr::group_by(
      dplyr::across(dplyr::any_of(c(
        "anchor_model_id",
        "anchor_species",
        "anchor_family",
        "policy",
        "post_selection_support_bin",
        "equation_branch_filter"
      )))
    ) |>
    dplyr::arrange(.data$u, .by_group = TRUE) |>
    dplyr::mutate(
      median_ts_error_smooth = smooth_one(.data$u, .data$median_ts_error),
      q80_ts_abs_raw_smooth = smooth_one(.data$u, .data$q80_ts_abs_raw),
      q90_ts_abs_raw_smooth = smooth_one(.data$u, .data$q90_ts_abs_raw),
      q95_ts_abs_raw_smooth = smooth_one(.data$u, .data$q95_ts_abs_raw),
      q99_ts_abs_raw_smooth = smooth_one(.data$u, .data$q99_ts_abs_raw),
      q80_ts_abs_dev_smooth = smooth_one(.data$u, .data$q80_ts_abs_dev),
      q90_ts_abs_dev_smooth = smooth_one(.data$u, .data$q90_ts_abs_dev),
      q95_ts_abs_dev_smooth = smooth_one(.data$u, .data$q95_ts_abs_dev),
      q99_ts_abs_dev_smooth = smooth_one(.data$u, .data$q99_ts_abs_dev),
      median_log_sigma_residual_smooth = smooth_one(.data$u, .data$median_log_sigma_residual),
      q80_log_sigma_abs_dev_smooth = smooth_one(.data$u, .data$q80_log_sigma_abs_dev),
      q90_log_sigma_abs_dev_smooth = smooth_one(.data$u, .data$q90_log_sigma_abs_dev),
      q95_log_sigma_abs_dev_smooth = smooth_one(.data$u, .data$q95_log_sigma_abs_dev),
      q99_log_sigma_abs_dev_smooth = smooth_one(.data$u, .data$q99_log_sigma_abs_dev),
      q10_log_sigma_residual_smooth = smooth_one(.data$u, .data$q10_log_sigma_residual),
      q90_log_sigma_residual_smooth = smooth_one(.data$u, .data$q90_log_sigma_residual),
      q05_log_sigma_residual_smooth = smooth_one(.data$u, .data$q05_log_sigma_residual),
      q95_log_sigma_residual_smooth = smooth_one(.data$u, .data$q95_log_sigma_residual),
      q025_log_sigma_residual_smooth = smooth_one(.data$u, .data$q025_log_sigma_residual),
      q975_log_sigma_residual_smooth = smooth_one(.data$u, .data$q975_log_sigma_residual),
      q005_log_sigma_residual_smooth = smooth_one(.data$u, .data$q005_log_sigma_residual),
      q995_log_sigma_residual_smooth = smooth_one(.data$u, .data$q995_log_sigma_residual)
    ) |>
    dplyr::ungroup()
}

#' Summarize selected-policy TS calibration
#'
#' Restricts TS residual calibration to the cross-fitted winning-policy rows,
#' then smooths the residual envelopes within support bin and branch.
#'
#' @param ts_error Policy TS-error table.
#' @param selected_tbl Selected-policy table.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
summarize_selected_ts_calibration <- function(ts_error,
                                              selected_tbl,
                                              policy_perf = NULL) {
  ts_error_ <- tibble::as_tibble(ts_error)
  selected_tbl_ <- tibble::as_tibble(selected_tbl)
  if (nrow(ts_error_) == 0 || nrow(selected_tbl_) == 0) {
    return(tibble::tibble())
  }

  selected_policy_values <- if ("selected_policy" %in% names(selected_tbl_)) {
    as.character(selected_tbl_$selected_policy)
  } else if ("policy" %in% names(selected_tbl_)) {
    as.character(selected_tbl_$policy)
  } else {
    rep(NA_character_, nrow(selected_tbl_))
  }
  selected_keys <- selected_tbl_ |>
    dplyr::transmute(
      policy = selected_policy_values,
      equation_branch_filter = resolve_selected_policy_branches(selected_tbl_)
    ) |>
    dplyr::filter(
      !is.na(.data$policy),
      !is.na(.data$equation_branch_filter)
    ) |>
    dplyr::distinct(.data$policy, .data$equation_branch_filter)
  if (nrow(selected_keys) == 0) {
    return(tibble::tibble())
  }

  selected_ts <- normalize_policy_columns(ts_error_)
  selected_ts$policy <- resolve_policy_names(selected_ts)
  selected_ts <- selected_ts |>
    dplyr::mutate(
      anchor_model_id = as.character(.data$anchor_model_id),
      anchor_species = if ("anchor_species" %in% names(selected_ts)) as.character(.data$anchor_species) else rep(NA_character_, dplyr::n()),
      anchor_family = if ("anchor_family" %in% names(selected_ts)) as.character(.data$anchor_family) else rep(NA_character_, dplyr::n())
    )
  selected_ts_joined <- selected_ts |>
    dplyr::inner_join(
      selected_keys,
      by = c("policy", "equation_branch_filter")
    )
  if (nrow(selected_ts_joined) == 0) {
    return(tibble::tibble())
  }

  selected_ts_joined |>
    dplyr::mutate(
      u = suppressWarnings(as.numeric(.data$u)),
      ts_error = suppressWarnings(as.numeric(.data$ts_error)),
      log_sigma_residual = suppressWarnings(as.numeric(.data$log_sigma_residual))
    ) |>
    dplyr::filter(
      is.finite(.data$u),
      is.finite(.data$ts_error),
      is.finite(.data$log_sigma_residual)
    )
}

#' Summarize selected-policy coefficient calibration
#'
#' Stores historical slope/intercept residual pairs for cross-fitted winning
#' policies so anchor-facing TS panels can be built from coefficient-space
#' uncertainty instead of multiplier-space uncertainty.
#'
#' @param selected_tbl Selected-policy table.
#' @param candidate_models Candidate-model table containing the true anchor
#'   coefficients.
#' @param ts_error Optional policy TS-error table used to recover policy
#'   coefficients when `selected_tbl` does not carry them directly.
#'
#' @return A tibble of residual pairs.
#'
#' @keywords internal
#' @noRd
summarize_coeff_calibration <- function(selected_tbl,
                                        candidate_models,
                                        ts_error = NULL) {
  selected_tbl_ <- tibble::as_tibble(selected_tbl)
  candidate_models_ <- tibble::as_tibble(candidate_models)
  ts_error_ <- tibble::as_tibble(ts_error %||% tibble::tibble())
  if (nrow(selected_tbl_) == 0 || nrow(candidate_models_) == 0 || nrow(ts_error_) == 0) {
    return(tibble::tibble())
  }

  id_col <- reference_anchor_id_column(candidate_models_)
  selected_policy_values <- if ("selected_policy" %in% names(selected_tbl_)) {
    as.character(selected_tbl_$selected_policy)
  } else if ("policy" %in% names(selected_tbl_)) {
    as.character(selected_tbl_$policy)
  } else {
    rep(NA_character_, nrow(selected_tbl_))
  }
  selected_support_bins <- if ("post_selection_support_bin" %in% names(selected_tbl_)) {
    as.character(selected_tbl_$post_selection_support_bin)
  } else {
    rep(NA_character_, nrow(selected_tbl_))
  }
  candidate_field_or_na <- function(nm, mode = c("numeric", "character")) {
    mode_ <- match.arg(mode)
    if (!nm %in% names(candidate_models_)) {
      if (identical(mode_, "character")) {
        return(rep(NA_character_, nrow(candidate_models_)))
      }
      return(rep(NA_real_, nrow(candidate_models_)))
    }
    if (identical(mode_, "character")) {
      return(as.character(candidate_models_[[nm]]))
    }
    suppressWarnings(as.numeric(candidate_models_[[nm]]))
  }

  truth_tbl <- candidate_models_ |>
    dplyr::transmute(
      anchor_model_id = as.character(.data[[id_col]]),
      anchor_species = candidate_field_or_na("species_name", "character"),
      anchor_genus = dplyr::coalesce(
        candidate_field_or_na("genus", "character"),
        stringr::word(candidate_field_or_na("species_name", "character"), 1)
      ),
      anchor_family = dplyr::coalesce(
        candidate_field_or_na("family_name", "character"),
        candidate_field_or_na("family", "character")
      ),
      anchor_frequency_khz = suppressWarnings(as.numeric(
        dplyr::coalesce(
          candidate_field_or_na("frequency_khz"),
          candidate_field_or_na("frequency")
        )
      )),
      anchor_length_min = suppressWarnings(as.numeric(
        dplyr::coalesce(
          candidate_field_or_na("length_min"),
          candidate_field_or_na("study_length_min"),
          candidate_field_or_na("length_midpoint"),
          candidate_field_or_na("study_length_midpoint")
        )
      )),
      anchor_length_max = suppressWarnings(as.numeric(
        dplyr::coalesce(
          candidate_field_or_na("length_max"),
          candidate_field_or_na("study_length_max"),
          candidate_field_or_na("length_midpoint"),
          candidate_field_or_na("study_length_midpoint")
        )
      )),
      anchor_depth_min = suppressWarnings(as.numeric(
        dplyr::coalesce(
          candidate_field_or_na("depth_min"),
          candidate_field_or_na("study_depth_min"),
          candidate_field_or_na("depth_midpoint"),
          candidate_field_or_na("study_depth_midpoint")
        )
      )),
      anchor_depth_max = suppressWarnings(as.numeric(
        dplyr::coalesce(
          candidate_field_or_na("depth_max"),
          candidate_field_or_na("study_depth_max"),
          candidate_field_or_na("depth_midpoint"),
          candidate_field_or_na("study_depth_midpoint")
        )
      )),
      anchor_slope_len = candidate_field_or_na("slope_len"),
      anchor_intercept_len = candidate_field_or_na("intercept_len")
    )

  selected_keys <- selected_tbl |>
    dplyr::transmute(
      policy = selected_policy_values,
      equation_branch_filter = resolve_selected_policy_branches(selected_tbl),
      post_selection_support_bin = selected_support_bins
    ) |>
    dplyr::filter(
      !is.na(.data$policy),
      !is.na(.data$equation_branch_filter)
    )
  selected_keys <- selected_keys |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter) |>
    dplyr::summarise(
      post_selection_support_bin = {
        x <- stats::na.omit(.data$post_selection_support_bin)
        if (length(x) > 0) as.character(x[[1]]) else NA_character_
      },
      .groups = "drop"
    )
  if (nrow(selected_keys) == 0) {
    return(tibble::tibble())
  }

  ts_error <- normalize_policy_columns(ts_error)
  ts_error$policy <- resolve_policy_names(ts_error)
  if (all(c("policy_slope_len", "policy_intercept_len") %in% names(ts_error))) {
    benchmark_coefficients <- ts_error |>
      dplyr::mutate(
        anchor_model_id = as.character(.data$anchor_model_id),
        policy_slope_len = suppressWarnings(as.numeric(.data$policy_slope_len)),
        policy_intercept_len = suppressWarnings(as.numeric(.data$policy_intercept_len)),
        local_min_combined_distance = if ("local_min_combined_distance" %in% names(ts_error)) suppressWarnings(as.numeric(.data$local_min_combined_distance)) else NA_real_,
        local_weighted_mean_combined_distance = if ("local_weighted_mean_combined_distance" %in% names(ts_error)) suppressWarnings(as.numeric(.data$local_weighted_mean_combined_distance)) else NA_real_,
        local_min_species_distance = if ("local_min_species_distance" %in% names(ts_error)) suppressWarnings(as.numeric(.data$local_min_species_distance)) else NA_real_,
        local_weighted_mean_species_distance = if ("local_weighted_mean_species_distance" %in% names(ts_error)) suppressWarnings(as.numeric(.data$local_weighted_mean_species_distance)) else NA_real_,
        local_min_trait_gower_distance = if ("local_min_trait_gower_distance" %in% names(ts_error)) suppressWarnings(as.numeric(.data$local_min_trait_gower_distance)) else NA_real_,
        local_weighted_mean_trait_gower_distance = if ("local_weighted_mean_trait_gower_distance" %in% names(ts_error)) suppressWarnings(as.numeric(.data$local_weighted_mean_trait_gower_distance)) else NA_real_,
        local_effective_support = if ("local_effective_support" %in% names(ts_error)) suppressWarnings(as.numeric(.data$local_effective_support)) else NA_real_,
        local_mean_length_overlap = if ("local_mean_length_overlap" %in% names(ts_error)) suppressWarnings(as.numeric(.data$local_mean_length_overlap)) else NA_real_,
        local_mean_depth_overlap = if ("local_mean_depth_overlap" %in% names(ts_error)) suppressWarnings(as.numeric(.data$local_mean_depth_overlap)) else NA_real_
      ) |>
      dplyr::group_by(.data$anchor_model_id, .data$policy, .data$equation_branch_filter) |>
      dplyr::summarise(
        policy_slope_len = stats::median(.data$policy_slope_len, na.rm = TRUE),
        policy_intercept_len = stats::median(.data$policy_intercept_len, na.rm = TRUE),
        local_min_combined_distance = stats::median(.data$local_min_combined_distance, na.rm = TRUE),
        local_weighted_mean_combined_distance = stats::median(.data$local_weighted_mean_combined_distance, na.rm = TRUE),
        local_min_species_distance = stats::median(.data$local_min_species_distance, na.rm = TRUE),
        local_weighted_mean_species_distance = stats::median(.data$local_weighted_mean_species_distance, na.rm = TRUE),
        local_min_trait_gower_distance = stats::median(.data$local_min_trait_gower_distance, na.rm = TRUE),
        local_weighted_mean_trait_gower_distance = stats::median(.data$local_weighted_mean_trait_gower_distance, na.rm = TRUE),
        local_effective_support = stats::median(.data$local_effective_support, na.rm = TRUE),
        local_mean_length_overlap = stats::median(.data$local_mean_length_overlap, na.rm = TRUE),
        local_mean_depth_overlap = stats::median(.data$local_mean_depth_overlap, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(
        is.finite(.data$policy_slope_len),
        is.finite(.data$policy_intercept_len)
      )
  } else if (all(c("length_cm", "ts_pred") %in% names(ts_error))) {
    benchmark_coefficients <- ts_error |>
      dplyr::mutate(
        anchor_model_id = as.character(.data$anchor_model_id),
        length_cm = suppressWarnings(as.numeric(.data$length_cm)),
        ts_pred = suppressWarnings(as.numeric(.data$ts_pred))
      ) |>
      dplyr::group_by(.data$anchor_model_id, .data$policy, .data$equation_branch_filter) |>
      dplyr::summarise(
        policy_slope_len = {
          keep <- is.finite(.data$length_cm) & .data$length_cm > 0 & is.finite(.data$ts_pred)
          if (sum(keep) < 2L) {
            NA_real_
          } else {
            x <- log10(.data$length_cm[keep])
            y <- .data$ts_pred[keep]
            slope_var <- stats::var(x)
            if (!is.finite(slope_var) || slope_var <= 0) {
              NA_real_
            } else {
              stats::cov(x, y) / slope_var
            }
          }
        },
        policy_intercept_len = {
          keep <- is.finite(.data$length_cm) & .data$length_cm > 0 & is.finite(.data$ts_pred)
          if (sum(keep) < 2L) {
            NA_real_
          } else {
            x <- log10(.data$length_cm[keep])
            y <- .data$ts_pred[keep]
            slope_var <- stats::var(x)
            if (!is.finite(slope_var) || slope_var <= 0) {
              NA_real_
            } else {
              slope_now <- stats::cov(x, y) / slope_var
              mean(y) - slope_now * mean(x)
            }
          }
        },
        .groups = "drop"
      ) |>
      dplyr::filter(
        is.finite(.data$policy_slope_len),
        is.finite(.data$policy_intercept_len)
      )
  } else {
    benchmark_coefficients <- tibble::tibble()
  }
  if (nrow(benchmark_coefficients) == 0) {
    return(tibble::tibble())
  }

  benchmark_selected <- benchmark_coefficients |>
    dplyr::inner_join(
      dplyr::distinct(
        selected_keys,
        dplyr::pick("policy", "equation_branch_filter", "post_selection_support_bin")
      ),
      by = c("policy", "equation_branch_filter")
    )
  benchmark_selected |>
    dplyr::inner_join(truth_tbl, by = "anchor_model_id") |>
    dplyr::mutate(
      slope_resid = .data$anchor_slope_len - .data$policy_slope_len,
      intercept_resid = .data$anchor_intercept_len - .data$policy_intercept_len
    ) |>
    dplyr::filter(
      is.finite(.data$policy_slope_len),
      is.finite(.data$policy_intercept_len),
      is.finite(.data$anchor_slope_len),
      is.finite(.data$anchor_intercept_len),
      is.finite(.data$slope_resid),
      is.finite(.data$intercept_resid),
      !is.na(.data$policy),
      !is.na(.data$equation_branch_filter)
    ) |>
    dplyr::select(
      "anchor_model_id",
      "anchor_species",
      "anchor_genus",
      "anchor_family",
      "anchor_frequency_khz",
      "anchor_length_min",
      "anchor_length_max",
      "anchor_depth_min",
      "anchor_depth_max",
      "policy",
      "equation_branch_filter",
      "post_selection_support_bin",
      "local_min_combined_distance",
      "local_weighted_mean_combined_distance",
      "local_min_species_distance",
      "local_weighted_mean_species_distance",
      "local_min_trait_gower_distance",
      "local_weighted_mean_trait_gower_distance",
      "local_effective_support",
      "local_mean_length_overlap",
      "local_mean_depth_overlap",
      "policy_slope_len",
      "policy_intercept_len",
      "slope_resid",
      "intercept_resid"
    )
}

#' Resolve the first available numeric column from a candidate set
#'
#' @param tbl Input table.
#' @param candidates Ordered vector of candidate column names.
#'
#' @return Numeric vector aligned to `tbl`.
#'
#' @keywords internal
#' @noRd
resolve_numeric_candidates <- function(tbl,
                                       candidates) {
  tbl <- tibble::as_tibble(tbl)
  for (nm in candidates) {
    if (nm %in% names(tbl)) {
      return(suppressWarnings(as.numeric(tbl[[nm]])))
    }
  }
  rep(NA_real_, nrow(tbl))
}

#' Compute a weighted mean with NA fallback
#'
#' @param x Numeric values.
#' @param w Numeric weights.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
weighted_mean_or_na <- function(x,
                                w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[keep], w[keep], na.rm = TRUE)
}

#' Compute a weighted quantile with NA fallback
#'
#' @param x Numeric values.
#' @param w Numeric weights.
#' @param prob Quantile probability.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
weighted_quantile_or_na <- function(x,
                                    w,
                                    prob) {
  # Compute one weighted quantile after dropping unusable observations.
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  total_w <- sum(w)
  if (!is.finite(total_w) || total_w <= 0) {
    return(NA_real_)
  }
  prob <- suppressWarnings(as.numeric(prob)[[1]])
  if (!is.finite(prob)) {
    return(NA_real_)
  }
  prob <- min(max(prob, 0), 1)
  if (length(x) == 1L) {
    return(x[[1]])
  }
  w <- w / total_w
  mid_p <- cumsum(w) - 0.5 * w
  mid_p <- pmin(pmax(mid_p, 0), 1)
  mid_p <- cummax(mid_p)
  stats::approx(
    x = mid_p,
    y = x,
    xout = prob,
    method = "linear",
    ties = "ordered",
    rule = 2
  )$y[[1]]
}

#' Build continuous support-weighted conformal calibration state
#'
#' @keywords internal
#' @noRd
build_continuous_support_conformal <- function(calibration_rows,
                                               alpha,
                                               bandwidth = NULL) {
  calibration_rows <- tibble::as_tibble(calibration_rows)
  required_cols <- c("continuous_support_score", "abs_log_residual")
  missing_cols <- setdiff(required_cols, names(calibration_rows))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf(
        "Continuous support conformal calibration is missing column(s): %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  alpha <- suppressWarnings(as.numeric(alpha %||% NA_real_)[[1]])
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be a finite value strictly between 0 and 1.", call. = FALSE)
  }
  rows <- calibration_rows |>
    dplyr::transmute(
      continuous_support_score = suppressWarnings(as.numeric(.data$continuous_support_score)),
      abs_log_residual = suppressWarnings(as.numeric(.data$abs_log_residual))
    ) |>
    dplyr::filter(
      is.finite(.data$continuous_support_score),
      is.finite(.data$abs_log_residual),
      .data$abs_log_residual >= 0
    )
  if (nrow(rows) < 2L) {
    stop("Continuous support conformal calibration requires at least two finite residual rows.", call. = FALSE)
  }
  bandwidth <- suppressWarnings(as.numeric(bandwidth %||% NA_real_)[[1]])
  if (!is.finite(bandwidth) || bandwidth <= 0) {
    stop("Continuous support conformal calibration requires an explicit positive bandwidth.", call. = FALSE)
  }
  list(
    rows = rows,
    alpha = alpha,
    bandwidth = bandwidth,
    n_calibration_rows = nrow(rows)
  )
}

#' Look up support-weighted conformal quantiles for query rows
#'
#' @keywords internal
#' @noRd
continuous_support_conformal_quantiles <- function(calibration,
                                                   query_support_score) {
  if (!is.list(calibration) || is.null(calibration$rows) ||
    is.null(calibration$alpha) || is.null(calibration$bandwidth)) {
    stop("Continuous support conformal prediction requires stored calibration state.", call. = FALSE)
  }
  rows <- tibble::as_tibble(calibration$rows)
  alpha <- suppressWarnings(as.numeric(calibration$alpha %||% NA_real_)[[1]])
  bandwidth <- suppressWarnings(as.numeric(calibration$bandwidth %||% NA_real_)[[1]])
  if (!all(c("continuous_support_score", "abs_log_residual") %in% names(rows)) ||
    nrow(rows) < 2L || !is.finite(alpha) || alpha <= 0 || alpha >= 1 ||
    !is.finite(bandwidth) || bandwidth <= 0) {
    stop("Stored continuous support conformal calibration is invalid.", call. = FALSE)
  }
  query_support_score <- suppressWarnings(as.numeric(query_support_score))
  if (any(!is.finite(query_support_score))) {
    stop("Continuous support conformal prediction requires finite query support scores.", call. = FALSE)
  }
  lookup_one <- function(score) {
    weights <- exp(-abs(rows$continuous_support_score - score) / bandwidth)
    weight_sum <- sum(weights)
    if (!is.finite(weight_sum) || weight_sum <= 0) {
      stop("Continuous support conformal weighting produced no positive calibration weight.", call. = FALSE)
    }
    normalized_weights <- weights / weight_sum
    tibble::tibble(
      continuous_conformal_q_abs_log = weighted_quantile_or_na(
        rows$abs_log_residual,
        weights,
        prob = 1 - alpha
      ),
      continuous_conformal_effective_n = 1 / sum(normalized_weights^2),
      continuous_conformal_weight_max = max(normalized_weights),
      continuous_conformal_n_scores = nrow(rows)
    )
  }
  purrr::map_dfr(query_support_score, lookup_one)
}

#' Compute multiple weighted central interval quantiles
#'
#' @param x Numeric values.
#' @param w Numeric weights.
#' @param widths Interval widths.
#'
#' @return Named numeric vector.
#'
#' @keywords internal
#' @noRd
weighted_interval_quantiles <- function(x,
                                        w,
                                        widths = c(0.80, 0.90, 0.95, 0.99)) {
  # Convert interval widths into paired lower and upper tail probabilities.
  widths_ <- suppressWarnings(as.numeric(widths))
  widths_ <- widths_[is.finite(widths_)]
  if (!length(widths_)) {
    return(numeric())
  }
  tails <- (1 - widths_) / 2
  probs <- as.vector(rbind(tails, 1 - tails))
  labels <- as.vector(rbind(
    paste0("lo", widths_ * 100),
    paste0("hi", widths_ * 100)
  ))
  stats::setNames(
    vapply(probs, function(prob) weighted_quantile_or_na(x, w, prob), numeric(1)),
    labels
  )
}

#' Compute multiple weighted upper quantiles
#'
#' @param x Numeric values.
#' @param w Numeric weights.
#' @param levels Upper-tail quantile levels.
#'
#' @return Named numeric vector.
#'
#' @keywords internal
#' @noRd
weighted_upper_quantiles <- function(x,
                                     w,
                                     levels = c(0.80, 0.90, 0.95, 0.99)) {
  # Evaluate the requested upper-tail levels against one weighted sample.
  levels_ <- suppressWarnings(as.numeric(levels))
  levels_ <- levels_[is.finite(levels_)]
  if (!length(levels_)) {
    return(numeric())
  }
  stats::setNames(
    vapply(levels_, function(level) weighted_quantile_or_na(x, w, level), numeric(1)),
    paste0("q", levels_ * 100)
  )
}

#' Compute finite quantiles with type-8 interpolation
#'
#' @param x Numeric values.
#' @param probs Quantile probabilities.
#' @param labels Optional output names.
#'
#' @return Named numeric vector.
#'
#' @keywords internal
#' @noRd
finite_quantiles_type8 <- function(x,
                                   probs,
                                   labels = NULL) {
  # Restrict to finite values so each caller gets the same quantile behavior.
  x_ <- suppressWarnings(as.numeric(x))
  x_ <- x_[is.finite(x_)]
  probs_ <- suppressWarnings(as.numeric(probs))
  if (!length(probs_)) {
    return(numeric())
  }
  out <- if (!length(x_)) {
    rep(NA_real_, length(probs_))
  } else {
    stats::quantile(
      x_,
      probs = probs_,
      na.rm = TRUE,
      names = FALSE,
      type = 8
    )
  }
  if (!is.null(labels)) {
    names(out) <- labels
  }
  out
}

#' Summarize calibration residual distributions
#'
#' @param ts_error Numeric TS residual vector.
#' @param log_sigma_residual Numeric sigma residual vector.
#' @param include_sigma_abs_dev Logical; include absolute sigma-deviation
#'   quantiles when `TRUE`.
#'
#' @return Named numeric vector.
#'
#' @keywords internal
#' @noRd
calibration_summary_values <- function(ts_error,
                                       log_sigma_residual,
                                       include_sigma_abs_dev = FALSE) {
  # Standardize both residual inputs before building the shared summary fields.
  ts_error_ <- suppressWarnings(as.numeric(ts_error))
  sigma_ <- suppressWarnings(as.numeric(log_sigma_residual))

  median_ts <- if (any(is.finite(ts_error_))) {
    stats::median(ts_error_, na.rm = TRUE)
  } else {
    NA_real_
  }
  median_sigma <- if (any(is.finite(sigma_))) {
    stats::median(sigma_, na.rm = TRUE)
  } else {
    NA_real_
  }

  # Reuse the same quantile templates across raw and selected calibration paths.
  out <- c(
    median_ts_error = median_ts,
    finite_quantiles_type8(
      abs(ts_error_),
      probs = c(0.80, 0.90, 0.95, 0.99),
      labels = c("q80_ts_abs_raw", "q90_ts_abs_raw", "q95_ts_abs_raw", "q99_ts_abs_raw")
    ),
    finite_quantiles_type8(
      abs(ts_error_ - median_ts),
      probs = c(0.80, 0.90, 0.95, 0.99),
      labels = c("q80_ts_abs_dev", "q90_ts_abs_dev", "q95_ts_abs_dev", "q99_ts_abs_dev")
    ),
    median_log_sigma_residual = median_sigma
  )

  # Add sigma absolute-deviation summaries only where the caller needs them.
  if (isTRUE(include_sigma_abs_dev)) {
    out <- c(
      out,
      finite_quantiles_type8(
        abs(sigma_ - median_sigma),
        probs = c(0.80, 0.90, 0.95, 0.99),
        labels = c(
          "q80_log_sigma_abs_dev",
          "q90_log_sigma_abs_dev",
          "q95_log_sigma_abs_dev",
          "q99_log_sigma_abs_dev"
        )
      )
    )
  }

  c(
    out,
    finite_quantiles_type8(
      sigma_,
      probs = c(0.10, 0.90, 0.05, 0.95, 0.025, 0.975, 0.005, 0.995),
      labels = c(
        "q10_log_sigma_residual",
        "q90_log_sigma_residual",
        "q05_log_sigma_residual",
        "q95_log_sigma_residual",
        "q025_log_sigma_residual",
        "q975_log_sigma_residual",
        "q005_log_sigma_residual",
        "q995_log_sigma_residual"
      )
    )
  )
}

#' Compute a weighted correlation with NA fallback
#'
#' @param x Numeric values.
#' @param y Numeric values.
#' @param w Numeric weights.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
weighted_cor_or_na <- function(x,
                               y,
                               w) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  w <- suppressWarnings(as.numeric(w))
  keep <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(keep) < 2L) {
    return(NA_real_)
  }
  x <- x[keep]
  y <- y[keep]
  w <- w[keep]
  w_sum <- sum(w)
  if (!is.finite(w_sum) || w_sum <= 0) {
    return(NA_real_)
  }
  w <- w / w_sum
  x_bar <- sum(w * x)
  y_bar <- sum(w * y)
  x_ctr <- x - x_bar
  y_ctr <- y - y_bar
  vx <- sum(w * x_ctr^2)
  vy <- sum(w * y_ctr^2)
  if (!is.finite(vx) || !is.finite(vy) || vx <= 0 || vy <= 0) {
    return(NA_real_)
  }
  sum(w * x_ctr * y_ctr) / sqrt(vx * vy)
}

#' Return the predictor columns used for locality weighting
#'
#' @param tbl Training table.
#' @param include_u Logical; include normalized length ratio if available.
#'
#' @return Character vector of predictor columns.
#'
#' @keywords internal
#' @noRd
similarity_weight_predictors <- function(tbl,
                                         include_u = TRUE) {
  tbl <- tibble::as_tibble(tbl)
  candidate_cols <- c(
    if (isTRUE(include_u)) "u" else character(),
    "local_min_combined_distance",
    "local_weighted_mean_combined_distance",
    "local_min_species_distance",
    "local_weighted_mean_species_distance",
    "local_min_trait_gower_distance",
    "local_weighted_mean_trait_gower_distance",
    "log_local_effective_support",
    "local_mean_length_overlap",
    "local_mean_depth_overlap",
    "policy_slope_len",
    "policy_intercept_len"
  )
  candidate_cols <- candidate_cols[candidate_cols %in% names(tbl)]
  candidate_cols[vapply(
    candidate_cols,
    function(nm) {
      vals <- suppressWarnings(as.numeric(tbl[[nm]]))
      vals <- vals[is.finite(vals)]
      length(unique(vals)) >= 2L
    },
    logical(1)
  )]
}

#' Compute locality weights for one target row
#'
#' @param training_tbl Training table.
#' @param row_now One-row target table.
#' @param target_u Optional normalized-length target.
#' @param include_u Logical; include normalized length ratio if available.
#'
#' @return Numeric weight vector.
#'
#' @keywords internal
#' @noRd
locality_similarity_weights <- function(training_tbl,
                                        row_now,
                                        target_u = NA_real_,
                                        include_u = TRUE) {
  # Compute continuous similarity weights from the realized donor geometry.
  # Rows close to the selected anchor in distance/overlap/support space receive
  # larger weights; rows far away are smoothly downweighted rather than being
  # removed by a hard-coded group fallback.
  training_tbl <- tibble::as_tibble(training_tbl)
  row_now <- tibble::as_tibble(row_now)
  if (nrow(training_tbl) == 0 || nrow(row_now) == 0) {
    return(numeric())
  }

  predictor_cols <- similarity_weight_predictors(
    tbl = training_tbl,
    include_u = include_u
  )
  if (length(predictor_cols) == 0L) {
    return(rep(1, nrow(training_tbl)))
  }

  target_map <- setNames(vector("list", length(predictor_cols)), predictor_cols)
  for (nm in predictor_cols) {
    target_value <- if (identical(nm, "u")) {
      suppressWarnings(as.numeric(target_u)[[1]])
    } else if (identical(nm, "local_min_combined_distance") &&
      "anchor_selection_local_distance" %in% names(row_now)) {
      suppressWarnings(as.numeric(row_now$anchor_selection_local_distance[[1]]))
    } else if (identical(nm, "local_weighted_mean_combined_distance") &&
      "anchor_selection_local_distance" %in% names(row_now)) {
      suppressWarnings(as.numeric(row_now$anchor_selection_local_distance[[1]]))
    } else if (nm %in% names(row_now)) {
      suppressWarnings(as.numeric(row_now[[nm]][[1]]))
    } else if (identical(nm, "log_local_effective_support") &&
      "local_effective_support" %in% names(row_now)) {
      suppressWarnings(log1p(pmax(as.numeric(row_now$local_effective_support[[1]]), 0)))
    } else {
      NA_real_
    }
    target_map[[nm]] <- target_value
  }

  dist_components <- vector("list", length(predictor_cols))
  component_names <- character(0)
  for (nm in predictor_cols) {
    vals <- suppressWarnings(as.numeric(training_tbl[[nm]]))
    vals_ok <- vals[is.finite(vals)]
    if (length(vals_ok) < 2L) {
      next
    }
    predictor_scale <- stats::mad(vals_ok, center = stats::median(vals_ok), constant = 1, na.rm = TRUE)
    if (!is.finite(predictor_scale) || predictor_scale <= 0) {
      predictor_scale <- stats::IQR(vals_ok, na.rm = TRUE) / 1.349
    }
    if (!is.finite(predictor_scale) || predictor_scale <= 0) {
      predictor_scale <- stats::sd(vals_ok, na.rm = TRUE)
    }
    if (!is.finite(predictor_scale) || predictor_scale <= 0) {
      next
    }

    reference_value <- suppressWarnings(as.numeric(target_map[[nm]] %||% NA_real_))
    if (!is.finite(reference_value)) {
      reference_value <- stats::median(vals_ok, na.rm = TRUE)
    }
    if (!is.finite(reference_value)) {
      next
    }

    filled_values <- vals
    filled_values[!is.finite(filled_values)] <- reference_value
    dist_components[[nm]] <- ((filled_values - reference_value) / predictor_scale)^2
    component_names <- c(component_names, nm)
  }

  if (length(component_names) == 0L) {
    return(rep(1, nrow(training_tbl)))
  }

  dist_mat <- do.call(
    cbind,
    dist_components[component_names]
  )
  if (is.null(dim(dist_mat))) {
    dist_mat <- matrix(dist_mat, ncol = 1L)
  }
  d2 <- rowMeans(dist_mat, na.rm = TRUE)
  d2[!is.finite(d2)] <- stats::median(d2[is.finite(d2)], na.rm = TRUE)
  d2[!is.finite(d2)] <- 0
  weights <- exp(-0.5 * d2)
  weights[!is.finite(weights)] <- 0
  if (!any(weights > 0)) {
    weights <- rep(1, nrow(training_tbl))
  }
  weights
}

#' Smooth one weighted curve against normalized length
#'
#' @param u Numeric length-ratio grid.
#' @param y Numeric response values.
#' @param spar Spline smoothing parameter.
#'
#' @return Numeric vector.
#'
#' @keywords internal
#' @noRd
smooth_weighted_curve <- function(u,
                                  y,
                                  spar = 0.6) {
  u <- suppressWarnings(as.numeric(u))
  y <- suppressWarnings(as.numeric(y))
  keep <- is.finite(u) & is.finite(y)
  if (sum(keep) < 4L) {
    return(y)
  }
  ord <- order(u[keep])
  u_fit <- u[keep][ord]
  y_fit <- y[keep][ord]
  smoothed <- tryCatch(
    stats::predict(
      stats::smooth.spline(x = u_fit, y = y_fit, spar = spar),
      x = u
    )$y,
    error = function(e) y
  )
  smoothed
}

#' Collapse selected TS calibration across anchors
#'
#' @param ts_calibration Selected-policy TS calibration table.
#'
#' @return Collapsed calibration tibble.
#'
#' @keywords internal
#' @noRd
collapse_selected_ts_calibration <- function(ts_calibration) {
  # Pool selected-policy calibration rows so downstream lookup can fall back from anchor-specific to shared support.
  ts_calibration <- tibble::as_tibble(ts_calibration)
  if (nrow(ts_calibration) == 0 || !"u" %in% names(ts_calibration)) {
    return(tibble::tibble())
  }

  if (all(c("ts_error", "log_sigma_residual") %in% names(ts_calibration))) {
    collapsed_raw <- ts_calibration |>
      dplyr::mutate(
        u = suppressWarnings(as.numeric(.data$u)),
        ts_error = suppressWarnings(as.numeric(.data$ts_error)),
        log_sigma_residual = suppressWarnings(as.numeric(.data$log_sigma_residual))
      ) |>
      dplyr::filter(
        is.finite(.data$u),
        is.finite(.data$ts_error),
        is.finite(.data$log_sigma_residual)
      ) |>
      dplyr::group_by(.data$u) |>
      dplyr::summarise(
        n = dplyr::n(),
        .calibration_summary = list(calibration_summary_values(
          ts_error = .data$ts_error,
          log_sigma_residual = .data$log_sigma_residual,
          include_sigma_abs_dev = TRUE
        )),
        .groups = "drop"
      ) |>
      tidyr::unnest_wider(.data$.calibration_summary)
    if (nrow(collapsed_raw) == 0) {
      return(collapsed_raw)
    }
    collapsed_raw <- smooth_selected_ts_calibration(collapsed_raw)
    return(
      collapsed_raw |>
        dplyr::mutate(
          q80_ts_abs_raw = pmax(.data$q80_ts_abs_raw, 0, na.rm = TRUE),
          q90_ts_abs_raw = pmax(.data$q90_ts_abs_raw, .data$q80_ts_abs_raw, 0, na.rm = TRUE),
          q95_ts_abs_raw = pmax(.data$q95_ts_abs_raw, .data$q90_ts_abs_raw, 0, na.rm = TRUE),
          q99_ts_abs_raw = pmax(.data$q99_ts_abs_raw, .data$q95_ts_abs_raw, 0, na.rm = TRUE),
          q80_ts_abs_dev = pmax(.data$q80_ts_abs_dev, 0, na.rm = TRUE),
          q90_ts_abs_dev = pmax(.data$q90_ts_abs_dev, .data$q80_ts_abs_dev, 0, na.rm = TRUE),
          q95_ts_abs_dev = pmax(.data$q95_ts_abs_dev, .data$q90_ts_abs_dev, 0, na.rm = TRUE),
          q99_ts_abs_dev = pmax(.data$q99_ts_abs_dev, .data$q95_ts_abs_dev, 0, na.rm = TRUE),
          q80_log_sigma_abs_dev = pmax(.data$q80_log_sigma_abs_dev, 0, na.rm = TRUE),
          q90_log_sigma_abs_dev = pmax(.data$q90_log_sigma_abs_dev, .data$q80_log_sigma_abs_dev, 0, na.rm = TRUE),
          q95_log_sigma_abs_dev = pmax(.data$q95_log_sigma_abs_dev, .data$q90_log_sigma_abs_dev, 0, na.rm = TRUE),
          q99_log_sigma_abs_dev = pmax(.data$q99_log_sigma_abs_dev, .data$q95_log_sigma_abs_dev, 0, na.rm = TRUE)
        )
    )
  }

  collapsed <- ts_calibration |>
    dplyr::mutate(
      u = suppressWarnings(as.numeric(.data$u)),
      .weight_n = dplyr::coalesce(
        if ("n" %in% names(ts_calibration)) suppressWarnings(as.numeric(.data$n)) else rep(NA_real_, dplyr::n()),
        1
      ),
      .weight_n = dplyr::if_else(is.finite(.data$.weight_n) & .data$.weight_n > 0, .data$.weight_n, 1),
      median_ts_error_selected = resolve_numeric_candidates(ts_calibration, c("median_ts_error_smooth", "median_ts_error")),
      q80_ts_abs_raw_selected = resolve_numeric_candidates(ts_calibration, c("q80_ts_abs_raw_smooth", "q80_ts_abs_raw")),
      q90_ts_abs_raw_selected = resolve_numeric_candidates(ts_calibration, c("q90_ts_abs_raw_smooth", "q90_ts_abs_raw")),
      q95_ts_abs_raw_selected = resolve_numeric_candidates(ts_calibration, c("q95_ts_abs_raw_smooth", "q95_ts_abs_raw")),
      q99_ts_abs_raw_selected = resolve_numeric_candidates(ts_calibration, c("q99_ts_abs_raw_smooth", "q99_ts_abs_raw")),
      q80_ts_abs_dev_selected = resolve_numeric_candidates(ts_calibration, c("q80_ts_abs_dev_smooth", "q80_ts_abs_dev")),
      q90_ts_abs_dev_selected = resolve_numeric_candidates(ts_calibration, c("q90_ts_abs_dev_smooth", "q90_ts_abs_dev")),
      q95_ts_abs_dev_selected = resolve_numeric_candidates(ts_calibration, c("q95_ts_abs_dev_smooth", "q95_ts_abs_dev")),
      q99_ts_abs_dev_selected = resolve_numeric_candidates(ts_calibration, c("q99_ts_abs_dev_smooth", "q99_ts_abs_dev")),
      median_log_sigma_residual_selected = resolve_numeric_candidates(ts_calibration, c("median_log_sigma_residual_smooth", "median_log_sigma_residual")),
      q10_log_sigma_residual_selected = resolve_numeric_candidates(ts_calibration, c("q10_log_sigma_residual_smooth", "q10_log_sigma_residual")),
      q90_log_sigma_residual_selected = resolve_numeric_candidates(ts_calibration, c("q90_log_sigma_residual_smooth", "q90_log_sigma_residual")),
      q05_log_sigma_residual_selected = resolve_numeric_candidates(ts_calibration, c("q05_log_sigma_residual_smooth", "q05_log_sigma_residual")),
      q95_log_sigma_residual_selected = resolve_numeric_candidates(ts_calibration, c("q95_log_sigma_residual_smooth", "q95_log_sigma_residual")),
      q025_log_sigma_residual_selected = resolve_numeric_candidates(ts_calibration, c("q025_log_sigma_residual_smooth", "q025_log_sigma_residual")),
      q975_log_sigma_residual_selected = resolve_numeric_candidates(ts_calibration, c("q975_log_sigma_residual_smooth", "q975_log_sigma_residual")),
      q005_log_sigma_residual_selected = resolve_numeric_candidates(ts_calibration, c("q005_log_sigma_residual_smooth", "q005_log_sigma_residual")),
      q995_log_sigma_residual_selected = resolve_numeric_candidates(ts_calibration, c("q995_log_sigma_residual_smooth", "q995_log_sigma_residual"))
    ) |>
    dplyr::filter(is.finite(.data$u)) |>
    dplyr::group_by(.data$u) |>
    dplyr::summarise(
      n = sum(.data$.weight_n, na.rm = TRUE),
      dplyr::across(
        dplyr::ends_with("_selected"),
        ~ weighted_mean_or_na(.x, .data$.weight_n)
      ),
      .groups = "drop"
    ) |>
    dplyr::rename_with(
      ~ sub("_selected$", "", .x),
      dplyr::ends_with("_selected")
    ) |>
    dplyr::arrange(.data$u)

  if (nrow(collapsed) == 0) {
    return(collapsed)
  }

  collapsed |>
    dplyr::mutate(
      q80_ts_abs_raw = pmax(.data$q80_ts_abs_raw, 0, na.rm = TRUE),
      q90_ts_abs_raw = pmax(.data$q90_ts_abs_raw, .data$q80_ts_abs_raw, 0, na.rm = TRUE),
      q95_ts_abs_raw = pmax(.data$q95_ts_abs_raw, .data$q90_ts_abs_raw, 0, na.rm = TRUE),
      q99_ts_abs_raw = pmax(.data$q99_ts_abs_raw, .data$q95_ts_abs_raw, 0, na.rm = TRUE),
      q80_ts_abs_dev = pmax(.data$q80_ts_abs_dev, 0, na.rm = TRUE),
      q90_ts_abs_dev = pmax(.data$q90_ts_abs_dev, .data$q80_ts_abs_dev, 0, na.rm = TRUE),
      q95_ts_abs_dev = pmax(.data$q95_ts_abs_dev, .data$q90_ts_abs_dev, 0, na.rm = TRUE),
      q99_ts_abs_dev = pmax(.data$q99_ts_abs_dev, .data$q95_ts_abs_dev, 0, na.rm = TRUE)
    )
}

#' Enrich TS calibration with locality summaries
#'
#' @param ts_calibration TS calibration table.
#' @param policy_perf Policy-performance table.
#'
#' @return TS calibration tibble with locality fields added.
#'
#' @keywords internal
#' @noRd
enrich_ts_calibration_locality <- function(ts_calibration,
                                           policy_perf) {
  ts_calibration <- tibble::as_tibble(ts_calibration)
  policy_perf <- tibble::as_tibble(policy_perf)
  if (nrow(ts_calibration) == 0 || nrow(policy_perf) == 0) {
    return(ts_calibration)
  }

  locality_cols <- c(
    "local_min_combined_distance",
    "local_weighted_mean_combined_distance",
    "local_min_species_distance",
    "local_weighted_mean_species_distance",
    "local_min_trait_gower_distance",
    "local_weighted_mean_trait_gower_distance",
    "local_effective_support",
    "local_mean_length_overlap",
    "local_mean_depth_overlap",
    "policy_slope_len",
    "policy_intercept_len"
  )
  missing_locality <- setdiff(locality_cols, names(ts_calibration))
  all_missing <- any(vapply(
    intersect(locality_cols, names(ts_calibration)),
    function(nm) all(!is.finite(suppressWarnings(as.numeric(ts_calibration[[nm]])))),
    logical(1)
  ))
  if (length(missing_locality) == 0L && !all_missing) {
    return(ts_calibration)
  }

  key_cols <- c("anchor_model_id", "policy", "equation_branch_filter")
  if (!all(key_cols %in% names(ts_calibration)) || !all(key_cols %in% names(policy_perf))) {
    return(ts_calibration)
  }

  lookup <- normalize_policy_columns(policy_perf) |>
    dplyr::mutate(
      anchor_model_id = as.character(.data$anchor_model_id),
      policy = resolve_policy_names(policy_perf),
      equation_branch_filter = resolve_policy_branch_filters(policy_perf)
    )

  if (!all(key_cols %in% names(lookup))) {
    return(ts_calibration)
  }
  for (nm in locality_cols) {
    if (!nm %in% names(lookup)) {
      lookup[[nm]] <- NA_real_
    }
  }

  lookup <- lookup |>
    dplyr::transmute(
      anchor_model_id = .data$anchor_model_id,
      policy = .data$policy,
      equation_branch_filter = .data$equation_branch_filter,
      lookup_local_min_combined_distance = suppressWarnings(as.numeric(.data$local_min_combined_distance)),
      lookup_local_weighted_mean_combined_distance = suppressWarnings(as.numeric(.data$local_weighted_mean_combined_distance)),
      lookup_local_min_species_distance = suppressWarnings(as.numeric(.data$local_min_species_distance)),
      lookup_local_weighted_mean_species_distance = suppressWarnings(as.numeric(.data$local_weighted_mean_species_distance)),
      lookup_local_min_trait_gower_distance = suppressWarnings(as.numeric(.data$local_min_trait_gower_distance)),
      lookup_local_weighted_mean_trait_gower_distance = suppressWarnings(as.numeric(.data$local_weighted_mean_trait_gower_distance)),
      lookup_local_effective_support = suppressWarnings(as.numeric(.data$local_effective_support)),
      lookup_local_mean_length_overlap = suppressWarnings(as.numeric(.data$local_mean_length_overlap)),
      lookup_local_mean_depth_overlap = suppressWarnings(as.numeric(.data$local_mean_depth_overlap)),
      lookup_policy_slope_len = suppressWarnings(as.numeric(.data$policy_slope_len)),
      lookup_policy_intercept_len = suppressWarnings(as.numeric(.data$policy_intercept_len))
    ) |>
    dplyr::group_by(.data$anchor_model_id, .data$policy, .data$equation_branch_filter) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::starts_with("lookup_"),
        ~ {
          x <- .x[is.finite(.x)]
          if (length(x) == 0L) NA_real_ else stats::median(x)
        }
      ),
      .groups = "drop"
    )

  out <- normalize_policy_columns(ts_calibration) |>
    dplyr::mutate(
      anchor_model_id = as.character(.data$anchor_model_id),
      policy = resolve_policy_names(ts_calibration),
      equation_branch_filter = resolve_policy_branch_filters(ts_calibration)
    ) |>
    dplyr::left_join(lookup, by = key_cols)

  for (nm in locality_cols) {
    lookup_nm <- paste0("lookup_", nm)
    if (!nm %in% names(out)) {
      out[[nm]] <- out[[lookup_nm]]
    } else {
      out[[nm]] <- dplyr::coalesce(
        suppressWarnings(as.numeric(out[[nm]])),
        suppressWarnings(as.numeric(out[[lookup_nm]]))
      )
    }
    out[[lookup_nm]] <- NULL
  }

  out
}

#' Compute a conformal multiplier from scaled residual scores
#'
#' @param x Numeric score vector.
#' @param level Coverage level.
#'
#' @return Numeric scalar or vector.
#'
#' @keywords internal
#' @noRd
scaled_functional_conformal_quantile <- function(x,
                                                 level) {
  # Use the finite-sample conformal order statistic instead of an interpolated
  # sample quantile so the resulting multiplier remains tied to the benchmark
  # calibration anchors.
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  level_ <- suppressWarnings(as.numeric(level))
  if (length(x) == 0L) {
    out <- rep(NA_real_, length(level_))
    if (length(out) == 1L) {
      return(NA_real_)
    }
    names(out) <- if (!is.null(names(level))) names(level) else NULL
    return(out)
  }

  # Evaluate one order statistic per requested coverage level.
  x_sorted <- sort(x)
  out <- vapply(
    level_,
    function(level_now) {
      if (!is.finite(level_now)) {
        return(NA_real_)
      }
      level_now <- min(max(level_now, 0), 1)
      k <- min(length(x_sorted), ceiling((length(x_sorted) + 1) * level_now))
      x_sorted[[k]]
    },
    numeric(1)
  )

  if (length(out) == 1L) {
    return(out[[1]])
  }
  names(out) <- if (!is.null(names(level))) names(level) else NULL
  out
}

#' Fit a smooth residual-scale curve
#'
#' @param calibration_rows Calibration table.
#' @param value_col Residual-scale column.
#' @param case_weight_col Optional case-weight column.
#' @param smooth_spar Spline smoothing parameter.
#'
#' @return Tibble describing the fitted scale curve.
#'
#' @keywords internal
#' @noRd
fit_scaled_functional_residual_curve <- function(calibration_rows,
                                                 value_col,
                                                 case_weight_col = NULL,
                                                 smooth_spar = 0.6) {
  # Estimate the typical residual size as a smooth function of normalized
  # length position. The scale is modeled with an RMS residual so the fitted
  # curve stays positive without imposing an arbitrary floor.
  #
  # The fitted trend is estimated on the log-scale with calibration-count
  # weights. This stabilizes short-length edge behaviour by borrowing strength
  # across neighboring support positions rather than letting one noisy endpoint
  # dominate the shape allocator.
  calibration_rows <- tibble::as_tibble(calibration_rows)
  if (nrow(calibration_rows) == 0 || !all(c("u", value_col) %in% names(calibration_rows))) {
    return(tibble::tibble())
  }

  scale_tbl <- calibration_rows |>
    dplyr::transmute(
      u = suppressWarnings(as.numeric(.data$u)),
      residual_value = suppressWarnings(as.numeric(.data[[value_col]])),
      case_weight = if (!is.null(case_weight_col) && case_weight_col %in% names(calibration_rows)) {
        suppressWarnings(as.numeric(.data[[case_weight_col]]))
      } else {
        1
      }
    ) |>
    dplyr::filter(
      is.finite(.data$u),
      is.finite(.data$residual_value),
      is.finite(.data$case_weight),
      .data$case_weight > 0
    ) |>
    dplyr::group_by(.data$u) |>
    dplyr::summarise(
      n = sum(.data$case_weight, na.rm = TRUE),
      scale_raw = sqrt(stats::weighted.mean(.data$residual_value^2, w = .data$case_weight, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$u)

  if (nrow(scale_tbl) == 0) {
    return(scale_tbl)
  }

  scale_tbl <- scale_tbl |>
    dplyr::mutate(
      scale_fit = .data$scale_raw
    )

  # Smooth the RMS scale over normalized length only when enough support
  # positions exist for a stable fit. Prefer a weighted GAM on the log-scale so
  # endpoint spikes are shrunk toward the neighbouring residual trend in a
  # standard, data-adaptive way. Fall back to the original spline when GAM is
  # unavailable or fails.
  if (nrow(scale_tbl) >= 4L) {
    # Cap the GAM basis dimension at the available support points so the
    # smoother stays well-posed on short calibration grids.
    k_now <- min(10L, max(3L, nrow(scale_tbl) - 1L))
    scale_fit <- NULL
    if (requireNamespace("mgcv", quietly = TRUE)) {
      # Format GAM formula
      gam_formula <- substitute(
        log(pmax(scale_raw, sqrt(.Machine$double.eps))) ~ mgcv::s(u, bs = "cs", k = k_val),
        list(k_val = k_now)
      )
      # Run fit
      gam_fit <- tryCatch(
        mgcv::gam(
          formula = gam_formula,
          data = scale_tbl,
          weights = pmax(scale_tbl$n, 1),
          method = "REML"
        ),
        error = function(e) NULL
      )

      if (!is.null(gam_fit)) {
        scale_fit <- tryCatch(
          exp(as.numeric(stats::predict(gam_fit, newdata = scale_tbl))),
          error = function(e) NULL
        )
      }
    }

    if (is.null(scale_fit)) {
      fit <- tryCatch(
        stats::smooth.spline(
          x = scale_tbl$u,
          y = log(pmax(scale_tbl$scale_raw, sqrt(.Machine$double.eps))),
          w = pmax(scale_tbl$n, 1),
          spar = smooth_spar
        ),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        scale_fit <- tryCatch(
          exp(stats::predict(fit, x = scale_tbl$u)$y),
          error = function(e) NULL
        )
      }
    }

    if (!is.null(scale_fit) && length(scale_fit) == nrow(scale_tbl)) {
      scale_tbl$scale_fit <- scale_fit
    }
  }

  scale_tbl |>
    dplyr::mutate(
      scale_fit = pmax(scale_fit, sqrt(.Machine$double.eps))
    )
}

#' Predict one smooth residual-scale curve
#'
#' @param scale_curve Fitted scale-curve object.
#' @param target_u Target normalized-length values.
#'
#' @return Numeric vector.
#'
#' @keywords internal
#' @noRd
predict_scaled_functional_curve <- function(scale_curve,
                                            target_u) {
  # Evaluate the smooth residual-scale curve on an arbitrary normalized-length
  # grid while preserving endpoint values outside the observed knots.
  scale_curve <- tibble::as_tibble(scale_curve)
  target_u <- suppressWarnings(as.numeric(target_u))
  if (nrow(scale_curve) == 0 || !"scale_fit" %in% names(scale_curve)) {
    return(rep(NA_real_, length(target_u)))
  }

  curve_u <- suppressWarnings(as.numeric(scale_curve$u))
  curve_scale <- suppressWarnings(as.numeric(scale_curve$scale_fit))
  keep <- is.finite(curve_u) & is.finite(curve_scale)
  if (!any(keep)) {
    return(rep(NA_real_, length(target_u)))
  }
  if (sum(keep) == 1L) {
    return(rep(curve_scale[keep][[1]], length(target_u)))
  }

  stats::approx(
    x = curve_u[keep],
    y = curve_scale[keep],
    xout = target_u,
    rule = 2,
    ties = "ordered"
  )$y
}

#' Build a full scaled functional conformal curve
#'
#' @param calibration_rows Calibration table.
#' @param value_col Residual-scale column.
#' @param levels Coverage levels to calibrate.
#' @param smooth_spar Spline smoothing parameter.
#'
#' @return Tibble containing the fitted scale curve and calibrated envelopes.
#'
#' @keywords internal
#' @noRd
build_scaled_functional_conformal_curve <- function(calibration_rows,
                                                    value_col,
                                                    levels = c(0.80, 0.90, 0.95, 0.99),
                                                    smooth_spar = 0.6) {
  # Cross-fit the residual-scale curve by leaving one benchmark anchor out at
  # a time, then conformalize the held-out supremum scores. The final envelope
  # shape comes from the full-data scale curve and the conformal multipliers
  # come from the out-of-fold scores.
  calibration_rows <- tibble::as_tibble(calibration_rows)
  if (nrow(calibration_rows) == 0 ||
    !all(c("anchor_model_id", "u", value_col) %in% names(calibration_rows))) {
    return(tibble::tibble())
  }

  levels <- suppressWarnings(as.numeric(levels))
  levels <- levels[is.finite(levels) & levels > 0 & levels < 1]
  if (length(levels) == 0L) {
    return(tibble::tibble())
  }

  rows_now <- calibration_rows |>
    dplyr::transmute(
      anchor_model_id = as.character(.data$anchor_model_id),
      u = suppressWarnings(as.numeric(.data$u)),
      residual_value = suppressWarnings(as.numeric(.data[[value_col]]))
    ) |>
    dplyr::filter(
      !is.na(.data$anchor_model_id),
      nzchar(.data$anchor_model_id),
      is.finite(.data$u),
      is.finite(.data$residual_value)
    )
  if (nrow(rows_now) == 0) {
    return(tibble::tibble())
  }

  anchor_ids <- unique(rows_now$anchor_model_id)
  if (length(anchor_ids) < 2L) {
    return(tibble::tibble())
  }

  score_tbl <- purrr::map_dfr(anchor_ids, function(anchor_id_now) {
    # Fit the scale curve on every anchor except the held-out one so each
    # conformal score is genuinely out-of-fold.
    train_rows <- rows_now |>
      dplyr::filter(.data$anchor_model_id != anchor_id_now)
    holdout_rows <- rows_now |>
      dplyr::filter(.data$anchor_model_id == anchor_id_now)
    if (nrow(train_rows) == 0 || nrow(holdout_rows) == 0) {
      return(tibble::tibble())
    }

    scale_curve <- fit_scaled_functional_residual_curve(
      calibration_rows = train_rows,
      value_col = "residual_value",
      smooth_spar = smooth_spar
    )
    if (nrow(scale_curve) == 0) {
      return(tibble::tibble())
    }

    scale_pred <- predict_scaled_functional_curve(
      scale_curve = scale_curve,
      target_u = holdout_rows$u
    )
    score_value <- max(
      abs(holdout_rows$residual_value) /
        pmax(scale_pred, sqrt(.Machine$double.eps)),
      na.rm = TRUE
    )
    if (!is.finite(score_value)) {
      return(tibble::tibble())
    }

    tibble::tibble(
      anchor_model_id = anchor_id_now,
      conformal_score = score_value
    )
  })
  if (nrow(score_tbl) == 0) {
    return(tibble::tibble())
  }

  full_scale_curve <- fit_scaled_functional_residual_curve(
    calibration_rows = rows_now,
    value_col = "residual_value",
    smooth_spar = smooth_spar
  )
  if (nrow(full_scale_curve) == 0) {
    return(tibble::tibble())
  }

  out <- full_scale_curve |>
    dplyr::transmute(
      u = suppressWarnings(as.numeric(.data$u)),
      n = suppressWarnings(as.numeric(.data$n)),
      scale_fit = suppressWarnings(as.numeric(.data$scale_fit))
    ) |>
    dplyr::arrange(.data$u)

  # Convert the out-of-fold score distribution into level-specific envelope
  # multipliers, then map those multipliers back onto the smooth scale curve.
  for (level_now in levels) {
    suffix <- paste0(formatC(round(level_now * 100), format = "d", width = 0))
    multiplier_now <- scaled_functional_conformal_quantile(
      x = score_tbl$conformal_score,
      level = level_now
    )
    out[[paste0("q", suffix, "_abs_dev")]] <- multiplier_now * out$scale_fit
    out[[paste0("score_multiplier_", suffix)]] <- multiplier_now
  }

  out |>
    dplyr::mutate(
      score_n = nrow(score_tbl),
      anchor_n = length(anchor_ids)
    )
}

#' Build scaled functional TS calibration objects
#'
#' @param ts_rows TS calibration rows.
#' @param smooth_spar Spline smoothing parameter.
#'
#' @return Tibble with paired TS-error and sigma-residual envelopes.
#'
#' @keywords internal
#' @noRd
build_scaled_functional_ts_calibration <- function(ts_rows,
                                                   smooth_spar = 0.6) {
  # Build paired TS-scale and log-sigma-scale envelopes from the same selected
  # benchmark rows so the acoustic panels and multiplier curves remain tied to
  # the same cross-fit residual pool.
  ts_rows <- tibble::as_tibble(ts_rows)
  if (nrow(ts_rows) == 0 || !all(c("anchor_model_id", "u", "ts_error", "log_sigma_residual") %in% names(ts_rows))) {
    return(tibble::tibble())
  }

  ts_curve <- build_scaled_functional_conformal_curve(
    calibration_rows = ts_rows,
    value_col = "ts_error",
    levels = c(0.80, 0.90, 0.95, 0.99),
    smooth_spar = smooth_spar
  )
  sigma_curve <- build_scaled_functional_conformal_curve(
    calibration_rows = ts_rows,
    value_col = "log_sigma_residual",
    levels = c(0.80, 0.90, 0.95, 0.99),
    smooth_spar = smooth_spar
  )
  if (nrow(ts_curve) == 0 || nrow(sigma_curve) == 0) {
    return(tibble::tibble())
  }

  out <- ts_curve |>
    dplyr::rename(
      ts_scale_fit = .data$scale_fit,
      q80_ts_abs_dev = .data$q80_abs_dev,
      q90_ts_abs_dev = .data$q90_abs_dev,
      q95_ts_abs_dev = .data$q95_abs_dev,
      q99_ts_abs_dev = .data$q99_abs_dev,
      ts_score_multiplier_80 = .data$score_multiplier_80,
      ts_score_multiplier_90 = .data$score_multiplier_90,
      ts_score_multiplier_95 = .data$score_multiplier_95,
      ts_score_multiplier_99 = .data$score_multiplier_99,
      ts_score_n = .data$score_n,
      ts_anchor_n = .data$anchor_n
    ) |>
    dplyr::left_join(
      sigma_curve |>
        dplyr::rename(
          log_sigma_scale_fit = .data$scale_fit,
          q80_log_sigma_abs_dev = .data$q80_abs_dev,
          q90_log_sigma_abs_dev = .data$q90_abs_dev,
          q95_log_sigma_abs_dev = .data$q95_abs_dev,
          q99_log_sigma_abs_dev = .data$q99_abs_dev,
          sigma_score_multiplier_80 = .data$score_multiplier_80,
          sigma_score_multiplier_90 = .data$score_multiplier_90,
          sigma_score_multiplier_95 = .data$score_multiplier_95,
          sigma_score_multiplier_99 = .data$score_multiplier_99,
          sigma_score_n = .data$score_n,
          sigma_anchor_n = .data$anchor_n
        ) |>
        dplyr::select(
          "u",
          "log_sigma_scale_fit",
          "q80_log_sigma_abs_dev",
          "q90_log_sigma_abs_dev",
          "q95_log_sigma_abs_dev",
          "q99_log_sigma_abs_dev",
          "sigma_score_multiplier_80",
          "sigma_score_multiplier_90",
          "sigma_score_multiplier_95",
          "sigma_score_multiplier_99",
          "sigma_score_n",
          "sigma_anchor_n"
        ),
      by = "u"
    ) |>
    dplyr::mutate(
      # The selected-policy line is the center of the displayed band, so the
      # residual envelopes are symmetric around zero rather than bias-shifted.
      median_ts_error = 0,
      q80_ts_abs_raw = .data$q80_ts_abs_dev,
      q90_ts_abs_raw = .data$q90_ts_abs_dev,
      q95_ts_abs_raw = .data$q95_ts_abs_dev,
      q99_ts_abs_raw = .data$q99_ts_abs_dev,
      median_log_sigma_residual = 0,
      q10_log_sigma_residual = -.data$q90_log_sigma_abs_dev,
      q90_log_sigma_residual = .data$q90_log_sigma_abs_dev,
      q05_log_sigma_residual = -.data$q95_log_sigma_abs_dev,
      q95_log_sigma_residual = .data$q95_log_sigma_abs_dev,
      q025_log_sigma_residual = -.data$q95_log_sigma_abs_dev,
      q975_log_sigma_residual = .data$q95_log_sigma_abs_dev,
      q005_log_sigma_residual = -.data$q99_log_sigma_abs_dev,
      q995_log_sigma_residual = .data$q99_log_sigma_abs_dev
    )

  out
}

#' Prepare residual-scale training data
#'
#' @param calibration_rows Calibration table.
#' @param response_col Response column to retain.
#'
#' @return Training tibble.
#'
#' @keywords internal
#' @noRd
prepare_residual_scale_training <- function(calibration_rows,
                                            response_col) {
  # Build one numeric training frame for anchor-conditional residual-width
  # modeling. The response is the absolute residual magnitude, while the
  # predictors describe the realized donor geometry for each benchmark anchor.
  calibration_rows <- tibble::as_tibble(calibration_rows)
  if (nrow(calibration_rows) == 0 || !response_col %in% names(calibration_rows)) {
    return(tibble::tibble())
  }

  out <- calibration_rows |>
    dplyr::transmute(
      anchor_model_id = as.character(.data$anchor_model_id),
      u = if ("u" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$u)) else NA_real_,
      abs_residual = abs(suppressWarnings(as.numeric(.data[[response_col]]))),
      local_min_combined_distance = if ("local_min_combined_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$local_min_combined_distance)) else NA_real_,
      local_weighted_mean_combined_distance = if ("local_weighted_mean_combined_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$local_weighted_mean_combined_distance)) else NA_real_,
      local_min_species_distance = if ("local_min_species_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$local_min_species_distance)) else NA_real_,
      local_weighted_mean_species_distance = if ("local_weighted_mean_species_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$local_weighted_mean_species_distance)) else NA_real_,
      local_min_trait_gower_distance = if ("local_min_trait_gower_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$local_min_trait_gower_distance)) else NA_real_,
      local_weighted_mean_trait_gower_distance = if ("local_weighted_mean_trait_gower_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$local_weighted_mean_trait_gower_distance)) else NA_real_,
      local_effective_support = if ("local_effective_support" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$local_effective_support)) else NA_real_,
      local_mean_length_overlap = if ("local_mean_length_overlap" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$local_mean_length_overlap)) else NA_real_,
      local_mean_depth_overlap = if ("local_mean_depth_overlap" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$local_mean_depth_overlap)) else NA_real_,
      policy_slope_len = if ("policy_slope_len" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$policy_slope_len)) else NA_real_,
      policy_intercept_len = if ("policy_intercept_len" %in% names(calibration_rows)) suppressWarnings(as.numeric(.data$policy_intercept_len)) else NA_real_
    ) |>
    dplyr::mutate(
      log_abs_residual = log1p(pmax(.data$abs_residual, 0)),
      log_local_effective_support = dplyr::if_else(
        is.finite(.data$local_effective_support),
        log1p(pmax(.data$local_effective_support, 0)),
        NA_real_
      )
    ) |>
    dplyr::filter(
      !is.na(.data$anchor_model_id),
      nzchar(.data$anchor_model_id),
      is.finite(.data$abs_residual)
    )

  out
}

#' Return predictor columns for residual-scale models
#'
#' @param training_tbl Residual-scale training table.
#' @param include_u Logical; include normalized length ratio if available.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
residual_scale_predictor_columns <- function(training_tbl,
                                             include_u = TRUE) {
  # Keep only predictors that actually vary within the calibration pool so the
  # quantile models remain estimable without bespoke fallback branches.
  training_tbl <- tibble::as_tibble(training_tbl)
  if (nrow(training_tbl) == 0) {
    return(character())
  }

  candidate_cols <- c(
    "local_min_combined_distance",
    "local_weighted_mean_combined_distance",
    "local_min_species_distance",
    "local_weighted_mean_species_distance",
    "local_min_trait_gower_distance",
    "local_weighted_mean_trait_gower_distance",
    "log_local_effective_support",
    "local_mean_length_overlap",
    "local_mean_depth_overlap",
    "policy_slope_len",
    "policy_intercept_len"
  )
  predictor_cols <- candidate_cols[candidate_cols %in% names(training_tbl)]
  predictor_cols <- predictor_cols[vapply(
    predictor_cols,
    function(nm) {
      vals <- suppressWarnings(as.numeric(training_tbl[[nm]]))
      vals <- vals[is.finite(vals)]
      length(unique(vals)) >= 2L
    },
    logical(1)
  )]

  if (isTRUE(include_u) && "u" %in% names(training_tbl)) {
    u_vals <- suppressWarnings(as.numeric(training_tbl$u))
    if (length(unique(u_vals[is.finite(u_vals)])) >= 2L) {
      predictor_cols <- c("u", predictor_cols)
    }
  }

  predictor_cols
}

#' Fit a quantile model for residual scale
#'
#' @param calibration_rows Calibration table.
#' @param response_col Response column to model.
#' @param tau Quantile level.
#' @param include_u Logical; include normalized length ratio if available.
#'
#' @return List describing the fitted quantile model.
#'
#' @keywords internal
#' @noRd
fit_residual_scale_quantile_model <- function(calibration_rows,
                                              response_col,
                                              tau,
                                              include_u = TRUE) {
  # Fit one quantile-regression model for absolute residual magnitude. The fit
  # is anchor-conditional because donor-distance and overlap summaries are part
  # of the design matrix rather than being handled by policy-class lookup.
  training_tbl <- prepare_residual_scale_training(
    calibration_rows = calibration_rows,
    response_col = response_col
  )
  if (nrow(training_tbl) < 2L) {
    return(NULL)
  }

  predictor_cols <- residual_scale_predictor_columns(
    training_tbl = training_tbl,
    include_u = include_u
  )
  use_u <- "u" %in% predictor_cols
  base_predictors <- setdiff(predictor_cols, "u")
  term_labels <- character(0)
  if (use_u) {
    n_unique_u <- length(unique(training_tbl$u[is.finite(training_tbl$u)]))
    if (n_unique_u >= 4L) {
      term_labels <- c(term_labels, sprintf("splines::ns(u, df = %d)", min(4L, n_unique_u - 1L)))
    } else {
      term_labels <- c(term_labels, "u")
    }
  }
  term_labels <- c(term_labels, base_predictors)
  if (length(term_labels) == 0L) {
    term_labels <- "1"
  }

  formula_now <- stats::as.formula(
    paste("log_abs_residual ~", paste(term_labels, collapse = " + "))
  )
  fit_now <- tryCatch(
    quantreg::rq(
      formula_now,
      tau = suppressWarnings(as.numeric(tau)[[1]]),
      data = training_tbl,
      method = "fn"
    ),
    error = function(e) NULL
  )
  if (is.null(fit_now)) {
    return(NULL)
  }

  default_map <- lapply(base_predictors, function(nm) {
    vals <- suppressWarnings(as.numeric(training_tbl[[nm]]))
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0L) {
      NA_real_
    } else {
      stats::median(vals, na.rm = TRUE)
    }
  })
  names(default_map) <- base_predictors
  range_map <- lapply(c(if (use_u) "u" else character(), base_predictors), function(nm) {
    vals <- suppressWarnings(as.numeric(training_tbl[[nm]]))
    vals <- vals[is.finite(vals)]
    c(
      min = if (length(vals) == 0L) NA_real_ else min(vals, na.rm = TRUE),
      max = if (length(vals) == 0L) NA_real_ else max(vals, na.rm = TRUE)
    )
  })
  names(range_map) <- c(if (use_u) "u" else character(), base_predictors)

  list(
    fit = fit_now,
    predictor_cols = base_predictors,
    include_u = use_u,
    default_values = default_map,
    predictor_ranges = range_map,
    anchor_n = length(unique(training_tbl$anchor_model_id)),
    row_n = nrow(training_tbl)
  )
}

#' Predict from a residual-scale quantile model
#'
#' @param model_obj Quantile-model object.
#' @param row_now One-row target table.
#' @param target_u Optional normalized-length target.
#'
#' @return Numeric vector.
#'
#' @keywords internal
#' @noRd
predict_residual_scale_quantile_model <- function(model_obj,
                                                  row_now,
                                                  target_u = NULL) {
  # Evaluate the fitted residual-width model for one selected row across an
  # optional normalized-length grid. Predictors missing on the selected row use
  # the calibration-pool median recorded at fit time.
  if (is.null(model_obj) || nrow(tibble::as_tibble(row_now)) == 0) {
    return(rep(NA_real_, length(target_u %||% 1)))
  }

  row_now <- tibble::as_tibble(row_now)
  target_u <- suppressWarnings(as.numeric(target_u %||% NA_real_))
  if (!length(target_u)) {
    target_u <- NA_real_
  }
  new_tbl <- tibble::tibble(u = target_u)
  if (isTRUE(model_obj$include_u)) {
    u_range <- suppressWarnings(as.numeric(model_obj$predictor_ranges$u %||% c(NA_real_, NA_real_)))
    if (length(u_range) >= 2L && all(is.finite(u_range))) {
      new_tbl$u <- pmin(pmax(new_tbl$u, u_range[[1]]), u_range[[2]])
    }
  }
  for (nm in model_obj$predictor_cols %||% character()) {
    value_now <- if (nm %in% names(row_now)) {
      suppressWarnings(as.numeric(row_now[[nm]][[1]]))
    } else if (identical(nm, "log_local_effective_support") &&
      "local_effective_support" %in% names(row_now)) {
      suppressWarnings(log1p(pmax(as.numeric(row_now$local_effective_support[[1]]), 0)))
    } else {
      NA_real_
    }
    if (!is.finite(value_now)) {
      value_now <- suppressWarnings(as.numeric(model_obj$default_values[[nm]] %||% NA_real_))
    }
    value_range <- suppressWarnings(as.numeric(model_obj$predictor_ranges[[nm]] %||% c(NA_real_, NA_real_)))
    if (length(value_range) >= 2L && all(is.finite(value_range)) && is.finite(value_now)) {
      value_now <- min(max(value_now, value_range[[1]]), value_range[[2]])
    }
    new_tbl[[nm]] <- rep(value_now, nrow(new_tbl))
  }

  pred_now <- tryCatch(
    suppressWarnings(as.numeric(stats::predict(model_obj$fit, newdata = new_tbl))),
    error = function(e) rep(NA_real_, nrow(new_tbl))
  )
  pred_now <- pmax(expm1(pred_now), 0, na.rm = TRUE)
  pred_now
}

#' Build one anchor-specific TS width curve
#'
#' @param ts_rows TS calibration rows.
#' @param row_now One-row target table.
#' @param target_u Normalized-length target grid.
#' @param smooth_spar Spline smoothing parameter.
#'
#' @return Tibble describing the calibrated TS width curve.
#'
#' @keywords internal
#' @noRd
build_anchor_specific_ts_width_curve <- function(ts_rows,
                                                 row_now,
                                                 target_u,
                                                 smooth_spar = 0.6) {
  # Build descriptive TS and log-sigma width curves from the full selected
  # policy/branch residual pool while continuously downweighting benchmark
  # anchors whose realized donor geometry differs from the selected row.
  ts_rows <- tibble::as_tibble(ts_rows)
  row_now <- tibble::as_tibble(row_now)
  target_u <- suppressWarnings(as.numeric(target_u))
  if (nrow(ts_rows) == 0 || nrow(row_now) == 0 || !length(target_u)) {
    return(tibble::tibble())
  }

  target_u <- pmin(pmax(target_u, 0), 1)
  count_at_target_u <- function(u_source,
                                target_u) {
    # Count the realized calibration rows contributing to each support point.
    u_source <- suppressWarnings(as.numeric(u_source))
    u_grid <- sort(unique(u_source[is.finite(u_source)]))
    if (!length(u_grid)) {
      return(rep(0, length(target_u)))
    }
    vapply(
      target_u,
      function(u_now) {
        u_match <- u_grid[[which.min(abs(u_grid - u_now))]]
        sum(is.finite(u_source) & abs(u_source - u_match) <= 1e-8)
      },
      numeric(1)
    )
  }
  predictor_candidates <- c(
    "u",
    "local_min_combined_distance",
    "local_weighted_mean_combined_distance",
    "local_mean_length_overlap",
    "local_mean_depth_overlap",
    "local_effective_support"
  )
  predictor_available <- intersect(predictor_candidates, names(ts_rows))
  if (!"u" %in% predictor_available ||
    !"ts_error" %in% names(ts_rows) ||
    !"log_sigma_residual" %in% names(ts_rows)) {
    return(tibble::tibble())
  }
  build_weighted_curve <- function(value_col,
                                   signed = FALSE) {
    training_tbl <- ts_rows |>
      dplyr::transmute(
        anchor_model_id = as.character(.data$anchor_model_id),
        u = suppressWarnings(as.numeric(.data$u)),
        local_min_combined_distance = if ("local_min_combined_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_min_combined_distance)) else NA_real_,
        local_weighted_mean_combined_distance = if ("local_weighted_mean_combined_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_weighted_mean_combined_distance)) else NA_real_,
        local_min_species_distance = if ("local_min_species_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_min_species_distance)) else NA_real_,
        local_weighted_mean_species_distance = if ("local_weighted_mean_species_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_weighted_mean_species_distance)) else NA_real_,
        local_min_trait_gower_distance = if ("local_min_trait_gower_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_min_trait_gower_distance)) else NA_real_,
        local_weighted_mean_trait_gower_distance = if ("local_weighted_mean_trait_gower_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_weighted_mean_trait_gower_distance)) else NA_real_,
        local_mean_length_overlap = if ("local_mean_length_overlap" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_mean_length_overlap)) else NA_real_,
        local_mean_depth_overlap = if ("local_mean_depth_overlap" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_mean_depth_overlap)) else NA_real_,
        log_local_effective_support = if ("local_effective_support" %in% names(ts_rows)) suppressWarnings(log1p(pmax(as.numeric(.data$local_effective_support), 0))) else NA_real_,
        policy_slope_len = if ("policy_slope_len" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$policy_slope_len)) else NA_real_,
        policy_intercept_len = if ("policy_intercept_len" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$policy_intercept_len)) else NA_real_,
        residual = suppressWarnings(as.numeric(.data[[value_col]]))
      ) |>
      dplyr::filter(
        !is.na(.data$anchor_model_id),
        nzchar(.data$anchor_model_id),
        is.finite(.data$u),
        is.finite(.data$residual)
      )
    if (nrow(training_tbl) < 2L) {
      return(NULL)
    }

    # Anchor-level similarity should determine which benchmark curves matter
    # for the selected row. Length dependence itself should then be estimated
    # pointwise over the shared normalized u-grid, rather than being diluted by
    # averaging over every u-value at once.
    base_weights <- locality_similarity_weights(
      training_tbl = training_tbl,
      row_now = row_now,
      target_u = stats::median(target_u, na.rm = TRUE),
      include_u = FALSE
    )
    if (length(base_weights) != nrow(training_tbl) || !any(base_weights > 0, na.rm = TRUE)) {
      base_weights <- rep(1, nrow(training_tbl))
    }

    u_grid <- sort(unique(training_tbl$u[is.finite(training_tbl$u)]))
    if (!length(u_grid)) {
      return(NULL)
    }

    q_mat <- vapply(
      target_u,
      function(target_position) {
        source_position <- u_grid[[which.min(abs(u_grid - target_position))]]
        source_index <- which(is.finite(training_tbl$u) & abs(training_tbl$u - source_position) <= 1e-8)
        if (!length(source_index)) {
          if (isTRUE(signed)) {
            return(c(lo80 = NA_real_, hi80 = NA_real_, lo90 = NA_real_, hi90 = NA_real_, lo95 = NA_real_, hi95 = NA_real_, lo99 = NA_real_, hi99 = NA_real_))
          }
          return(c(q80 = NA_real_, q90 = NA_real_, q95 = NA_real_, q99 = NA_real_))
        }
        point_weights <- base_weights[source_index]
        point_residuals <- training_tbl$residual[source_index]
        if (isTRUE(signed)) {
          weighted_interval_quantiles(point_residuals, point_weights)
        } else {
          point_residuals <- abs(point_residuals)
          weighted_upper_quantiles(point_residuals, point_weights)
        }
      },
      if (isTRUE(signed)) numeric(8) else numeric(4)
    )
    center_values <- if (isTRUE(signed)) {
      vapply(
        target_u,
        function(target_position) {
          source_position <- u_grid[[which.min(abs(u_grid - target_position))]]
          source_index <- which(is.finite(training_tbl$u) & abs(training_tbl$u - source_position) <= 1e-8)
          if (!length(source_index)) {
            return(NA_real_)
          }
          stats::median(training_tbl$residual[source_index], na.rm = TRUE)
        },
        numeric(1)
      )
    } else {
      NULL
    }
    if (is.null(dim(q_mat))) {
      q_mat <- matrix(q_mat, nrow = if (isTRUE(signed)) 8L else 4L)
    }
    if (isTRUE(signed)) {
      lo80 <- smooth_weighted_curve(target_u, q_mat[1, ], spar = smooth_spar)
      hi80 <- smooth_weighted_curve(target_u, q_mat[2, ], spar = smooth_spar)
      lo90 <- smooth_weighted_curve(target_u, q_mat[3, ], spar = smooth_spar)
      hi90 <- smooth_weighted_curve(target_u, q_mat[4, ], spar = smooth_spar)
      lo95 <- smooth_weighted_curve(target_u, q_mat[5, ], spar = smooth_spar)
      hi95 <- smooth_weighted_curve(target_u, q_mat[6, ], spar = smooth_spar)
      lo99 <- smooth_weighted_curve(target_u, q_mat[7, ], spar = smooth_spar)
      hi99 <- smooth_weighted_curve(target_u, q_mat[8, ], spar = smooth_spar)
      lo80 <- pmin(lo80, 0, na.rm = TRUE)
      lo90 <- pmin(lo90, lo80, 0, na.rm = TRUE)
      lo95 <- pmin(lo95, lo90, 0, na.rm = TRUE)
      lo99 <- pmin(lo99, lo95, 0, na.rm = TRUE)
      hi80 <- pmax(hi80, 0, na.rm = TRUE)
      hi90 <- pmax(hi90, hi80, 0, na.rm = TRUE)
      hi95 <- pmax(hi95, hi90, 0, na.rm = TRUE)
      hi99 <- pmax(hi99, hi95, 0, na.rm = TRUE)
      return(list(
        anchor_n = dplyr::n_distinct(training_tbl$anchor_model_id),
        score_n = nrow(training_tbl),
        center = center_values,
        lo80 = lo80,
        hi80 = hi80,
        lo90 = lo90,
        hi90 = hi90,
        lo95 = lo95,
        hi95 = hi95,
        lo99 = lo99,
        hi99 = hi99,
        base_weight_n = sum(base_weights > 0, na.rm = TRUE)
      ))
    }
    q80 <- smooth_weighted_curve(target_u, q_mat[1, ], spar = smooth_spar)
    q90 <- smooth_weighted_curve(target_u, q_mat[2, ], spar = smooth_spar)
    q95 <- smooth_weighted_curve(target_u, q_mat[3, ], spar = smooth_spar)
    q99 <- smooth_weighted_curve(target_u, q_mat[4, ], spar = smooth_spar)
    q80 <- pmax(q80, 0, na.rm = TRUE)
    q90 <- pmax(q90, q80, 0, na.rm = TRUE)
    q95 <- pmax(q95, q90, 0, na.rm = TRUE)
    q99 <- pmax(q99, q95, 0, na.rm = TRUE)
    list(
      anchor_n = dplyr::n_distinct(training_tbl$anchor_model_id),
      score_n = nrow(training_tbl),
      q80 = q80,
      q90 = q90,
      q95 = q95,
      q99 = q99,
      base_weight_n = sum(base_weights > 0, na.rm = TRUE)
    )
  }

  build_weighted_scale_fit <- function(value_col) {
    training_tbl <- ts_rows |>
      dplyr::transmute(
        anchor_model_id = as.character(.data$anchor_model_id),
        u = suppressWarnings(as.numeric(.data$u)),
        local_min_combined_distance = if ("local_min_combined_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_min_combined_distance)) else NA_real_,
        local_weighted_mean_combined_distance = if ("local_weighted_mean_combined_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_weighted_mean_combined_distance)) else NA_real_,
        local_min_species_distance = if ("local_min_species_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_min_species_distance)) else NA_real_,
        local_weighted_mean_species_distance = if ("local_weighted_mean_species_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_weighted_mean_species_distance)) else NA_real_,
        local_min_trait_gower_distance = if ("local_min_trait_gower_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_min_trait_gower_distance)) else NA_real_,
        local_weighted_mean_trait_gower_distance = if ("local_weighted_mean_trait_gower_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_weighted_mean_trait_gower_distance)) else NA_real_,
        local_mean_length_overlap = if ("local_mean_length_overlap" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_mean_length_overlap)) else NA_real_,
        local_mean_depth_overlap = if ("local_mean_depth_overlap" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$local_mean_depth_overlap)) else NA_real_,
        log_local_effective_support = if ("local_effective_support" %in% names(ts_rows)) suppressWarnings(log1p(pmax(as.numeric(.data$local_effective_support), 0))) else NA_real_,
        policy_slope_len = if ("policy_slope_len" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$policy_slope_len)) else NA_real_,
        policy_intercept_len = if ("policy_intercept_len" %in% names(ts_rows)) suppressWarnings(as.numeric(.data$policy_intercept_len)) else NA_real_,
        residual = suppressWarnings(as.numeric(.data[[value_col]]))
      ) |>
      dplyr::filter(
        !is.na(.data$anchor_model_id),
        nzchar(.data$anchor_model_id),
        is.finite(.data$u),
        is.finite(.data$residual)
      )
    if (nrow(training_tbl) < 2L) {
      return(NULL)
    }

    base_weights <- locality_similarity_weights(
      training_tbl = training_tbl,
      row_now = row_now,
      target_u = stats::median(target_u, na.rm = TRUE),
      include_u = FALSE
    )
    if (length(base_weights) != nrow(training_tbl) || !any(base_weights > 0, na.rm = TRUE)) {
      base_weights <- rep(1, nrow(training_tbl))
    }
    training_tbl$local_weight <- base_weights

    fit_now <- fit_scaled_functional_residual_curve(
      calibration_rows = training_tbl,
      value_col = "residual",
      case_weight_col = "local_weight",
      smooth_spar = smooth_spar
    )
    if (nrow(fit_now) == 0) {
      return(NULL)
    }

    tibble::tibble(
      u = target_u,
      scale_fit = predict_scaled_functional_curve(
        scale_curve = fit_now,
        target_u = target_u
      )
    )
  }

  ts_scale <- build_weighted_curve("ts_error", signed = TRUE)
  sigma_scale <- build_weighted_curve("log_sigma_residual")
  sigma_scale_fit <- build_weighted_scale_fit("log_sigma_residual")
  if (is.null(ts_scale) || is.null(sigma_scale) || is.null(sigma_scale_fit)) {
    return(tibble::tibble())
  }

  tibble::tibble(
    u = target_u,
    n = count_at_target_u(ts_rows$u, target_u),
    ts_anchor_n = ts_scale$anchor_n,
    ts_score_n = ts_scale$score_n,
    sigma_anchor_n = sigma_scale$anchor_n,
    sigma_score_n = sigma_scale$score_n,
    median_ts_error = ts_scale$center,
    q10_ts_error = ts_scale$lo80,
    q90_ts_error = ts_scale$hi80,
    q05_ts_error = ts_scale$lo90,
    q95_ts_error = ts_scale$hi90,
    q025_ts_error = ts_scale$lo95,
    q975_ts_error = ts_scale$hi95,
    q005_ts_error = ts_scale$lo99,
    q995_ts_error = ts_scale$hi99,
    q80_ts_abs_raw = 0.5 * (ts_scale$hi80 - ts_scale$lo80),
    q90_ts_abs_raw = 0.5 * (ts_scale$hi90 - ts_scale$lo90),
    q95_ts_abs_raw = 0.5 * (ts_scale$hi95 - ts_scale$lo95),
    q99_ts_abs_raw = 0.5 * (ts_scale$hi99 - ts_scale$lo99),
    q80_ts_abs_dev = 0.5 * (ts_scale$hi80 - ts_scale$lo80),
    q90_ts_abs_dev = 0.5 * (ts_scale$hi90 - ts_scale$lo90),
    q95_ts_abs_dev = 0.5 * (ts_scale$hi95 - ts_scale$lo95),
    q99_ts_abs_dev = 0.5 * (ts_scale$hi99 - ts_scale$lo99),
    median_log_sigma_residual = 0,
    log_sigma_scale_fit = sigma_scale_fit$scale_fit,
    q80_log_sigma_abs_dev = sigma_scale$q80,
    q90_log_sigma_abs_dev = sigma_scale$q90,
    q95_log_sigma_abs_dev = sigma_scale$q95,
    q99_log_sigma_abs_dev = sigma_scale$q99,
    q10_log_sigma_residual = -sigma_scale$q90,
    q90_log_sigma_residual = sigma_scale$q90,
    q05_log_sigma_residual = -sigma_scale$q95,
    q95_log_sigma_residual = sigma_scale$q95,
    q025_log_sigma_residual = -sigma_scale$q95,
    q975_log_sigma_residual = sigma_scale$q95,
    q005_log_sigma_residual = -sigma_scale$q99,
    q995_log_sigma_residual = sigma_scale$q99
  )
}

#' Select one TS calibration curve for the current row
#'
#' @param ts_calibration TS calibration table.
#' @param row_now One-row target table.
#' @param target_u Optional normalized-length target grid.
#' @param min_anchor_neighbors Minimum number of nearby anchors retained.
#'
#' @return Tibble describing the selected TS calibration curve.
#'
#' @keywords internal
#' @noRd
select_ts_calibration_curve <- function(ts_calibration,
                                        row_now,
                                        target_u = NULL,
                                        min_anchor_neighbors = 10L) {
  # Restrict the calibration pool to the selected policy and branch, then fit
  # one locality-conditioned residual-scale model on that full pool. The
  # resulting curve remains descriptive, anchor-specific, and free of any
  # support-bin or fallback broadening logic.
  ts_calibration <- tibble::as_tibble(ts_calibration)
  row_now <- tibble::as_tibble(row_now)
  if (nrow(ts_calibration) == 0 || nrow(row_now) == 0) {
    return(tibble::tibble())
  }

  policy_value <- as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_)
  branch_value <- resolve_selected_policy_branches(row_now)[[1]]
  if ("policy" %in% names(ts_calibration)) {
    ts_calibration$policy <- as.character(ts_calibration$policy)
  } else {
    ts_calibration$policy <- NA_character_
  }
  ts_calibration$equation_branch_filter <- resolve_policy_branch_filters(ts_calibration)
  if (is.na(policy_value) || !nzchar(policy_value) ||
    is.na(branch_value) || !nzchar(branch_value)) {
    return(tibble::tibble())
  }

  pool_now <- ts_calibration |>
    dplyr::filter(
      .data$policy == !!policy_value,
      .data$equation_branch_filter == !!branch_value
    )
  if (nrow(pool_now) == 0 || dplyr::n_distinct(pool_now$anchor_model_id) < 2L) {
    return(tibble::tibble())
  }

  # When donor-locality fields are available, keep only the nearest anchor
  # neighbors before building the curve so the calibration pool is genuinely
  # local rather than merely reweighted after the fact.
  min_anchor_neighbors <- suppressWarnings(as.integer(min_anchor_neighbors)[[1]])
  if (!is.finite(min_anchor_neighbors) || min_anchor_neighbors < 1L) {
    min_anchor_neighbors <- 1L
  }
  locality_cols <- intersect(
    c(
      "local_min_combined_distance",
      "local_weighted_mean_combined_distance",
      "local_min_species_distance",
      "local_weighted_mean_species_distance",
      "local_min_trait_gower_distance",
      "local_weighted_mean_trait_gower_distance",
      "local_effective_support",
      "local_mean_length_overlap",
      "local_mean_depth_overlap",
      "policy_slope_len",
      "policy_intercept_len"
    ),
    names(pool_now)
  )
  if (length(locality_cols) > 0L &&
    dplyr::n_distinct(pool_now$anchor_model_id) >= min_anchor_neighbors) {
    anchor_pool <- pool_now |>
      dplyr::group_by(.data$anchor_model_id) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(locality_cols),
          ~ {
            x <- suppressWarnings(as.numeric(.x))
            x <- x[is.finite(x)]
            if (!length(x)) NA_real_ else stats::median(x, na.rm = TRUE)
          }
        ),
        .groups = "drop"
      )
    anchor_weights <- locality_similarity_weights(
      training_tbl = anchor_pool,
      row_now = row_now,
      include_u = FALSE
    )
    if (length(anchor_weights) == nrow(anchor_pool) &&
      any(is.finite(anchor_weights)) &&
      length(unique(round(anchor_weights[is.finite(anchor_weights)], 12))) > 1L) {
      keep_anchor_ids <- anchor_pool$anchor_model_id[order(anchor_weights, decreasing = TRUE)][seq_len(min_anchor_neighbors)]
      pool_now <- pool_now |>
        dplyr::filter(.data$anchor_model_id %in% keep_anchor_ids) |>
        dplyr::select(-dplyr::any_of(locality_cols))
    }
  }

  # Default to the observed support grid unless the caller asks for a dense
  # evaluation grid explicitly.
  target_u <- target_u %||% sort(unique(suppressWarnings(as.numeric(pool_now$u))))
  target_u <- target_u[is.finite(target_u)]
  if (!length(target_u)) {
    return(tibble::tibble())
  }
  curve_now <- build_anchor_specific_ts_width_curve(
    ts_rows = pool_now,
    row_now = row_now,
    target_u = target_u
  )
  if (nrow(curve_now) == 0) {
    return(tibble::tibble())
  }

  curve_now
}

#' Summarize local log-sigma calibration for one anchor
#'
#' @param row_now One-row target table.
#' @param ts_calibration TS calibration table.
#' @param candidate_models Candidate-model table.
#' @param length_grid_n Number of support points.
#' @param min_anchor_neighbors Minimum number of nearby anchors retained.
#'
#' @return List with local q95/q99 summaries, or `NULL`.
#'
#' @keywords internal
#' @noRd
anchor_local_log_sigma_calibration <- function(row_now,
                                               ts_calibration,
                                               candidate_models,
                                               length_grid_n = 400L,
                                               min_anchor_neighbors = 10L) {
  row_now <- tibble::as_tibble(row_now)
  candidate_models <- tibble::as_tibble(candidate_models)
  if (nrow(row_now) == 0 || nrow(candidate_models) == 0) {
    return(NULL)
  }
  curve <- select_ts_calibration_curve(
    ts_calibration = ts_calibration,
    row_now = row_now,
    min_anchor_neighbors = min_anchor_neighbors
  )
  if (nrow(curve) == 0) {
    return(NULL)
  }
  id_col <- reference_anchor_id_column(candidate_models)
  anchor_id_chr <- as.character(row_now$anchor_model_id[[1]] %||% NA_character_)
  anchor_row <- candidate_models |>
    dplyr::filter(as.character(.data[[id_col]]) == anchor_id_chr) |>
    dplyr::slice(1)
  if (nrow(anchor_row) == 0) {
    return(NULL)
  }
  anchor_pdf <- anchor_pdf_from_row(anchor_row, n = length_grid_n)
  if (nrow(anchor_pdf) == 0) {
    return(NULL)
  }
  target_u <- seq(0, 1, length.out = nrow(anchor_pdf))
  approx_curve <- function(candidates) {
    values <- resolve_numeric_candidates(curve, candidates)
    keep <- is.finite(curve$u) & is.finite(values)
    if (!any(keep)) {
      return(rep(NA_real_, length(target_u)))
    }
    if (sum(keep) == 1) {
      return(rep(values[keep][[1]], length(target_u)))
    }
    stats::approx(
      x = curve$u[keep],
      y = values[keep],
      xout = target_u,
      rule = 2,
      ties = "ordered"
    )$y
  }
  pdf_weights <- suppressWarnings(as.numeric(anchor_pdf$f_len))
  if (!any(is.finite(pdf_weights) & pdf_weights > 0)) {
    pdf_weights <- rep(1 / nrow(anchor_pdf), nrow(anchor_pdf))
  } else {
    pdf_weights <- pdf_weights / sum(pdf_weights, na.rm = TRUE)
  }
  q95_abs_dev <- approx_curve(c("q95_log_sigma_abs_dev_smooth", "q95_log_sigma_abs_dev"))
  q99_abs_dev <- approx_curve(c("q99_log_sigma_abs_dev_smooth", "q99_log_sigma_abs_dev"))
  median_shift <- approx_curve(c("median_log_sigma_residual_smooth", "median_log_sigma_residual"))
  list(
    q95 = stats::weighted.mean(q95_abs_dev, pdf_weights, na.rm = TRUE),
    q99 = stats::weighted.mean(q99_abs_dev, pdf_weights, na.rm = TRUE),
    median_shift = stats::weighted.mean(median_shift, pdf_weights, na.rm = TRUE)
  )
}

#' Select the coefficient residual pool for one row
#'
#' @param coefficient_calibration Coefficient-calibration table.
#' @param row_now One-row target table.
#' @param min_rows Minimum required rows.
#'
#' @return Tibble of residual rows.
#'
#' @keywords internal
#' @noRd
select_coefficient_residual_pool <- function(coefficient_calibration,
                                             row_now,
                                             min_rows = 2L) {
  coefficient_calibration <- tibble::as_tibble(coefficient_calibration)
  row_now <- tibble::as_tibble(row_now)
  if (nrow(coefficient_calibration) == 0 || nrow(row_now) == 0) {
    return(tibble::tibble())
  }

  coefficient_calibration <- coefficient_calibration |>
    dplyr::mutate(
      anchor_model_id = if ("anchor_model_id" %in% names(coefficient_calibration)) as.character(.data$anchor_model_id) else NA_character_,
      anchor_species = if ("anchor_species" %in% names(coefficient_calibration)) as.character(.data$anchor_species) else NA_character_,
      anchor_family = if ("anchor_family" %in% names(coefficient_calibration)) as.character(.data$anchor_family) else NA_character_,
      policy = as.character(.data$policy),
      equation_branch_filter = resolve_policy_branch_filters(coefficient_calibration),
      post_selection_support_bin = as.character(.data$post_selection_support_bin),
      slope_resid = suppressWarnings(as.numeric(.data$slope_resid)),
      intercept_resid = suppressWarnings(as.numeric(.data$intercept_resid))
    ) |>
    dplyr::filter(
      is.finite(.data$slope_resid),
      is.finite(.data$intercept_resid)
    )
  if (nrow(coefficient_calibration) == 0) {
    return(tibble::tibble())
  }

  policy_value <- as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_)
  branch_value <- resolve_selected_policy_branches(row_now)[[1]]
  if (is.na(policy_value) || !nzchar(policy_value) ||
    is.na(branch_value) || !nzchar(branch_value)) {
    return(tibble::tibble())
  }
  anchor_id_chr <- as.character(row_now$anchor_model_id[[1]] %||% NA_character_)
  anchor_species_chr <- as.character(row_now$anchor_species[[1]] %||% NA_character_)

  pool_now <- coefficient_calibration |>
    dplyr::filter(
      .data$policy == !!policy_value,
      .data$equation_branch_filter == !!branch_value,
      is.na(.data$anchor_model_id) |
        is.na(!!anchor_id_chr) |
        .data$anchor_model_id != !!anchor_id_chr
    )
  # Coefficient residuals are leave-species-out diagnostics. When the target
  # species appears in the calibration table, exclude it from its own local
  # display interval to avoid species-level leakage.
  if (!is.na(anchor_species_chr) && nzchar(anchor_species_chr)) {
    species_excluded <- pool_now |>
      dplyr::filter(is.na(.data$anchor_species) | .data$anchor_species != !!anchor_species_chr)
    if (nrow(species_excluded) >= as.integer(min_rows)) {
      pool_now <- species_excluded
    }
  }
  if (nrow(pool_now) < as.integer(min_rows)) {
    return(tibble::tibble())
  }
  pool_now
}

#' Compute the selected-policy coefficient covariance
#'
#' @param coefficient_calibration Coefficient-calibration table.
#' @param row_now One-row target table.
#' @param min_rows Minimum required rows.
#'
#' @return List containing covariance and support count, or `NULL`.
#'
#' @keywords internal
#' @noRd
selected_coefficient_covariance <- function(coefficient_calibration,
                                            row_now,
                                            min_rows = 2L) {
  pool <- select_coefficient_residual_pool(
    coefficient_calibration = coefficient_calibration,
    row_now = row_now,
    min_rows = min_rows
  )
  if (nrow(pool) < 2L) {
    return(NULL)
  }

  sigma_now <- tryCatch(
    stats::cov(
      cbind(
        suppressWarnings(as.numeric(pool$slope_resid)),
        suppressWarnings(as.numeric(pool$intercept_resid))
      ),
      use = "complete.obs"
    ),
    error = function(e) NULL
  )
  if (is.null(sigma_now) || !is.matrix(sigma_now) || any(!is.finite(sigma_now))) {
    return(NULL)
  }

  sigma_now <- (sigma_now + t(sigma_now)) / 2
  diag(sigma_now) <- pmax(diag(sigma_now), 0)
  list(
    covariance = sigma_now,
    n = nrow(pool)
  )
}

#' Summarize selected-policy coefficient conformal radii
#'
#' @param coefficient_calibration Coefficient-calibration table.
#' @param row_now One-row target table.
#' @param level Coverage level.
#' @param min_rows Minimum required rows.
#'
#' @return List containing coefficient radii and covariance, or `NULL`.
#'
#' @keywords internal
#' @noRd
selected_coefficient_conformal_summary <- function(coefficient_calibration,
                                                   row_now,
                                                   level = 0.95,
                                                   min_rows = 2L) {
  pool <- select_coefficient_residual_pool(
    coefficient_calibration = coefficient_calibration,
    row_now = row_now,
    min_rows = min_rows
  )
  if (nrow(pool) < max(2L, as.integer(min_rows))) {
    return(NULL)
  }

  probs <- suppressWarnings(as.numeric(level))
  if (!is.finite(probs) || probs <= 0 || probs >= 1) {
    probs <- 0.95
  }

  slope_resid <- suppressWarnings(as.numeric(pool$slope_resid))
  intercept_resid <- suppressWarnings(as.numeric(pool$intercept_resid))
  keep <- is.finite(slope_resid) & is.finite(intercept_resid)
  if (sum(keep) < max(2L, as.integer(min_rows))) {
    return(NULL)
  }

  slope_resid <- slope_resid[keep]
  intercept_resid <- intercept_resid[keep]
  sigma_now <- tryCatch(
    stats::cov(cbind(slope_resid, intercept_resid), use = "complete.obs"),
    error = function(e) NULL
  )
  if (!is.null(sigma_now) && is.matrix(sigma_now) && all(is.finite(sigma_now))) {
    sigma_now <- (sigma_now + t(sigma_now)) / 2
    diag(sigma_now) <- pmax(diag(sigma_now), 0)
  } else {
    sigma_now <- matrix(c(NA_real_, NA_real_, NA_real_, NA_real_), 2, 2)
  }

  list(
    slope_radius = stats::quantile(abs(slope_resid), probs = probs, na.rm = TRUE, names = FALSE, type = 8),
    intercept_radius = stats::quantile(abs(intercept_resid), probs = probs, na.rm = TRUE, names = FALSE, type = 8),
    covariance = sigma_now,
    n = sum(keep)
  )
}

#' Fit one centered coefficient-width model
#'
#' @param coefficient_calibration Coefficient-calibration table.
#' @param row_now One-row target table.
#' @param response_col Residual column to model.
#' @param tau Quantile level.
#'
#' @return List describing the fitted width model, or `NULL`.
#'
#' @keywords internal
#' @noRd
fit_centered_coefficient_width_model <- function(coefficient_calibration,
                                                 row_now,
                                                 response_col,
                                                 tau = 0.95) {
  # Fit one centered residual-width model for slope or intercept uncertainty.
  # The interval remains centered on the selected point estimate; only the
  # half-width varies with the realized donor geometry for that row.
  pool_now <- select_coefficient_residual_pool(
    coefficient_calibration = coefficient_calibration,
    row_now = row_now,
    min_rows = 2L
  )
  if (nrow(pool_now) < 2L || !response_col %in% names(pool_now)) {
    return(NULL)
  }

  training_tbl <- prepare_residual_scale_training(
    calibration_rows = pool_now,
    response_col = response_col
  )
  if (nrow(training_tbl) < 2L) {
    return(NULL)
  }
  weight_now <- locality_similarity_weights(
    training_tbl = training_tbl,
    row_now = row_now,
    include_u = FALSE
  )
  local_n <- ceiling(sqrt(nrow(training_tbl)))
  local_n <- max(2L, min(nrow(training_tbl), as.integer(local_n)))
  local_order <- order(weight_now, decreasing = TRUE, na.last = TRUE)
  local_keep <- local_order[seq_len(local_n)]
  training_local <- training_tbl[local_keep, , drop = FALSE]
  pool_local <- pool_now[local_keep, , drop = FALSE]
  weight_local <- weight_now[local_keep]
  residual_local <- suppressWarnings(as.numeric(training_local$abs_residual))
  residual_center <- stats::median(residual_local, na.rm = TRUE)
  residual_scale <- stats::mad(residual_local, center = residual_center, constant = 1, na.rm = TRUE)
  if (!is.finite(residual_scale) || residual_scale <= 0) {
    residual_scale <- stats::IQR(residual_local, na.rm = TRUE) / 1.349
  }
  if (!is.finite(residual_scale) || residual_scale <= 0) {
    residual_scale <- stats::sd(residual_local, na.rm = TRUE)
  }
  if (is.finite(residual_center) && is.finite(residual_scale) && residual_scale > 0) {
    robust_keep <- is.finite(residual_local) &
      residual_local <= residual_center + 3 * residual_scale
    if (sum(robust_keep, na.rm = TRUE) >= 2L) {
      training_local <- training_local[robust_keep, , drop = FALSE]
      pool_local <- pool_local[robust_keep, , drop = FALSE]
      weight_local <- weight_local[robust_keep]
    }
  }
  if (!any(is.finite(weight_local) & weight_local > 0)) {
    weight_local <- rep(1, nrow(training_local))
  }
  width_now <- weighted_quantile_or_na(
    x = training_local$abs_residual,
    w = weight_local,
    prob = suppressWarnings(as.numeric(tau)[[1]])
  )

  list(
    width = suppressWarnings(as.numeric(width_now[[1]] %||% NA_real_)),
    support_n = dplyr::n_distinct(training_local$anchor_model_id),
    row_n = nrow(training_local),
    pool = pool_local |>
      dplyr::mutate(locality_weight = weight_local)
  )
}

#' Return the interval critical value for a support size
#'
#' @param support_n Effective support size.
#' @param level Coverage level.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
coefficient_interval_critical_value <- function(support_n,
                                                level = 0.95) {
  support_n <- suppressWarnings(as.numeric(support_n))
  alpha <- 1 - suppressWarnings(as.numeric(level))
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    alpha <- 0.05
  }
  tail_prob <- 1 - alpha / 2
  if (is.finite(support_n) && support_n >= 2) {
    return(stats::qt(tail_prob, df = max(support_n - 1, 1)))
  }
  stats::qnorm(tail_prob)
}

#' Extract realized donor identifiers from a selected policy row
#'
#' @param row_now One-row selected policy table.
#'
#' @return Character vector of donor model identifiers.
#'
#' @keywords internal
#' @noRd
policy_row_realized_donor_ids <- function(row_now) {
  row_now <- tibble::as_tibble(row_now)
  if (nrow(row_now) == 0L || !"realized_donor_fingerprint" %in% names(row_now)) {
    return(character(0))
  }
  fingerprint <- as.character(row_now$realized_donor_fingerprint[[1]] %||% NA_character_)
  if (is.na(fingerprint) || !nzchar(fingerprint)) {
    return(character(0))
  }
  ids <- unlist(strsplit(fingerprint, "\\|", fixed = FALSE), use.names = FALSE)
  ids <- stringr::str_squish(as.character(ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  unique(ids)
}

#' Return the policy-competition pool for one row
#'
#' @param policy_tbl Policy candidate table.
#' @param row_now One-row target table.
#' @param config Optional config object or list.
#'
#' @return Tibble of near-tied competitor rows.
#'
#' @keywords internal
#' @noRd
selection_competition_pool <- function(policy_tbl,
                                       row_now,
                                       config = NULL) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  row_now <- tibble::as_tibble(row_now)
  if (nrow(policy_tbl) == 0 || nrow(row_now) == 0) {
    return(tibble::tibble())
  }

  anchor_id_chr <- as.character(row_now$anchor_model_id[[1]] %||% NA_character_)
  if (is.na(anchor_id_chr) || !nzchar(anchor_id_chr)) {
    return(tibble::tibble())
  }

  # Helper coalesce to extract the most relevant validation error metric available for each row.
  tidy_coalesce <- function(col, type_fn = as.numeric) {
    if (col %in% names(policy_tbl)) suppressWarnings(type_fn(policy_tbl[[col]])) else NA
  }

  validation_error <- dplyr::coalesce(
    tidy_coalesce("anchor_selection_validation_error"),
    tidy_coalesce("species_block_median_abs_log_error"),
    tidy_coalesce("mean_species_median_abs_log"),
    tidy_coalesce("median_abs_log"),
    tidy_coalesce(".meta_predicted_score")
  )

  uncertainty_width <- dplyr::coalesce(
    tidy_coalesce("uncertainty_cost_log_width"),
    tidy_coalesce("interval_log_width")
  )

  selection_valid <- dplyr::coalesce(
    tidy_coalesce("selection_valid", as.logical),
    tidy_coalesce("n_valid_models") > 0,
    tidy_coalesce("n_models") > 0,
    tidy_coalesce("valid_prediction", as.logical),
    FALSE
  )

  uncertainty_eligible <- dplyr::coalesce(
    tidy_coalesce("uncertainty_eligible", as.logical),
    FALSE
  )

  pool <- policy_tbl |>
    dplyr::mutate(
      .anchor_model_id = as.character(.data$anchor_model_id),
      .validation_error = validation_error,
      .uncertainty_width = uncertainty_width,
      .selection_valid = selection_valid,
      .uncertainty_eligible = uncertainty_eligible
    ) |>
    dplyr::filter(
      .data$.anchor_model_id == anchor_id_chr,
      .data$.selection_valid,
      is.finite(.data$.validation_error),
      is.finite(.data$.uncertainty_width)
    )
  if (nrow(pool) == 0) {
    return(pool)
  }

  eligible_pool <- pool |>
    dplyr::filter(dplyr::coalesce(.data$.uncertainty_eligible, FALSE))
  if (nrow(eligible_pool) > 0) {
    pool <- eligible_pool
  }

  min_validation_error <- suppressWarnings(as.numeric(
    row_now$anchor_selection_min_validation_error[[1]] %||% NA_real_
  ))
  if (!is.finite(min_validation_error)) {
    min_validation_error <- suppressWarnings(min(pool$.validation_error, na.rm = TRUE))
  }
  if (!is.finite(min_validation_error)) {
    return(pool[0, , drop = FALSE])
  }

  validation_threshold <- suppressWarnings(as.numeric(
    row_now$anchor_selection_validation_threshold[[1]] %||% NA_real_
  ))
  if (!is.finite(validation_threshold)) {
    validation_threshold <- min_validation_error
  }

  # Match the current selector's accepted score band exactly. The selected-row
  # post-selection summaries should propagate the same near-tied competitor set
  # that the selector considered, rather than reintroducing separate width or
  # locality screens after the fact.
  score_pool <- pool |>
    dplyr::filter(.data$.validation_error <= validation_threshold + 1e-12)
  if (nrow(score_pool) == 0) {
    score_pool <- pool |>
      dplyr::filter(.data$.validation_error <= min_validation_error + 1e-12)
  }
  if (nrow(score_pool) == 0) {
    return(pool[0, , drop = FALSE])
  }
  if (!"bootstrap_median_rank" %in% names(score_pool)) {
    score_pool$bootstrap_median_rank <- NA_real_
  }

  selected_policy_value <- as.character(
    row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_
  )
  selected_branch_value <- as.character(
    row_now$selected_equation_branch_filter[[1]] %||%
      row_now$equation_branch_filter[[1]] %||%
      resolve_selected_policy_branches(row_now)[[1]] %||%
      NA_character_
  )

  score_pool |>
    dplyr::filter(
      !(as.character(.data$policy) == selected_policy_value &
        as.character(.data$equation_branch_filter) == selected_branch_value)
    ) |>
    dplyr::arrange(
      .data$bootstrap_median_rank,
      .data$.validation_error,
      .data$policy
    )
}

#' Compute competition covariance across near-tied policies
#'
#' @param policy_tbl Policy candidate table.
#' @param row_now One-row target table.
#' @param config Optional config object or list.
#'
#' @return List containing covariance and support count, or `NULL`.
#'
#' @keywords internal
#' @noRd
selection_competition_covariance <- function(policy_tbl,
                                             row_now,
                                             config = NULL) {
  pool <- selection_competition_pool(
    policy_tbl = policy_tbl,
    row_now = row_now,
    config = config
  )
  if (nrow(pool) < 2L) {
    return(NULL)
  }

  slope_vals <- suppressWarnings(as.numeric(pool$policy_slope_len))
  intercept_vals <- suppressWarnings(as.numeric(pool$policy_intercept_len))
  keep <- is.finite(slope_vals) & is.finite(intercept_vals)
  if (sum(keep) < 2L) {
    return(NULL)
  }

  sigma_now <- tryCatch(
    stats::cov(cbind(slope_vals[keep], intercept_vals[keep])),
    error = function(e) NULL
  )
  if (is.null(sigma_now) || !is.matrix(sigma_now) || any(!is.finite(sigma_now))) {
    return(NULL)
  }

  sigma_now <- (sigma_now + t(sigma_now)) / 2
  diag(sigma_now) <- pmax(diag(sigma_now), 0)
  list(
    covariance = sigma_now,
    n = sum(keep)
  )
}

#' Build anchor PDF directly from one anchor row
#'
#' @param anchor_row One-row anchor table.
#' @param n Number of support points.
#'
#' @return A tibble with `length_cm` and `f_len`.
#'
#' @keywords internal
#' @noRd
anchor_pdf_from_row <- function(anchor_row,
                                n = 400L) {
  user_pdf <- anchor_pdf_from_stored_row(anchor_row)
  if (nrow(user_pdf) > 0) {
    return(user_pdf)
  }
  mins <- suppressWarnings(as.numeric(anchor_row$study_length_min))
  maxs <- suppressWarnings(as.numeric(anchor_row$study_length_max))
  mids <- suppressWarnings(as.numeric(anchor_row$study_length_midpoint))
  mins <- mins[is.finite(mins) & mins > 0]
  maxs <- maxs[is.finite(maxs) & maxs > 0]
  mids <- mids[is.finite(mids) & mids > 0]
  if (length(mins) > 0 && length(maxs) > 0) {
    lmin <- min(pmin(mins, maxs))
    lmax <- max(pmax(mins, maxs))
    if (is.finite(lmin) && is.finite(lmax) && lmax > lmin) {
      grid <- seq(lmin, lmax, length.out = as.integer(n))
      return(tibble::tibble(length_cm = grid, f_len = rep(1 / length(grid), length(grid))))
    }
    if (is.finite(lmin)) {
      return(tibble::tibble(length_cm = lmin, f_len = 1))
    }
  }
  if (length(mids) > 0) {
    return(tibble::tibble(length_cm = mids[[1]], f_len = 1))
  }
  tibble::tibble()
}

#' Build a donor support shape over the anchor length grid
#'
#' @param anchor_scores Anchor-score table.
#' @param anchor_id_chr Anchor model identifier.
#' @param length_grid Anchor length grid.
#' @param pdf_weights Anchor PDF weights.
#' @param donor_ids Optional realized donor model identifiers.
#'
#' @return Numeric vector on the length grid, or `NA` values when unavailable.
#'
#' @keywords internal
#' @noRd
donor_length_support_shape <- function(anchor_scores,
                                       anchor_id_chr,
                                       length_grid,
                                       pdf_weights,
                                       donor_ids = NULL) {
  anchor_scores <- tibble::as_tibble(anchor_scores)
  length_grid <- suppressWarnings(as.numeric(length_grid))
  pdf_weights <- suppressWarnings(as.numeric(pdf_weights))
  if (
    nrow(anchor_scores) == 0 ||
      !"anchor_model_id" %in% names(anchor_scores) ||
      length(length_grid) == 0L
  ) {
    return(rep(NA_real_, length(length_grid)))
  }

  donor_pool <- anchor_scores |>
    dplyr::filter(as.character(.data$anchor_model_id) == anchor_id_chr)
  donor_id_col <- intersect(c("model_id"), names(donor_pool))
  if (length(donor_id_col) > 0L) {
    donor_pool <- donor_pool |>
      dplyr::filter(as.character(.data[[donor_id_col[[1]]]]) != anchor_id_chr)
    donor_ids_ <- unique(as.character(donor_ids %||% character(0)))
    donor_ids_ <- donor_ids_[!is.na(donor_ids_) & nzchar(donor_ids_)]
    if (length(donor_ids_) > 0L) {
      donor_pool <- donor_pool |>
        dplyr::filter(as.character(.data[[donor_id_col[[1]]]]) %in% donor_ids_)
    }
  }
  if (nrow(donor_pool) == 0L) {
    return(rep(NA_real_, length(length_grid)))
  }

  donor_min_vals <- candidate_coalesce_column(
    donor_pool,
    c("study_length_min", "species_length_min", "length_min", "length_minimum")
  )
  donor_max_vals <- candidate_coalesce_column(
    donor_pool,
    c("study_length_max", "species_length_max", "length_max", "length_maximum")
  )
  if (is.null(donor_min_vals) || length(donor_min_vals) == 0L) {
    donor_min_vals <- rep(NA_real_, nrow(donor_pool))
  }
  if (is.null(donor_max_vals) || length(donor_max_vals) == 0L) {
    donor_max_vals <- rep(NA_real_, nrow(donor_pool))
  }
  donor_min <- suppressWarnings(as.numeric(donor_min_vals))
  donor_max <- suppressWarnings(as.numeric(donor_max_vals))
  donor_lo <- pmin(donor_min, donor_max, na.rm = FALSE)
  donor_hi <- pmax(donor_min, donor_max, na.rm = FALSE)
  keep <- is.finite(donor_lo) & is.finite(donor_hi) & donor_hi > 0 & donor_lo > 0 & donor_hi >= donor_lo
  if (!any(keep)) {
    return(rep(NA_real_, length(length_grid)))
  }
  donor_lo <- donor_lo[keep]
  donor_hi <- donor_hi[keep]
  donor_pool <- donor_pool[keep, , drop = FALSE]

  donor_weights <- if ("w_adm" %in% names(donor_pool)) {
    suppressWarnings(as.numeric(donor_pool$w_adm))
  } else {
    rep(1, nrow(donor_pool))
  }
  donor_weights[!is.finite(donor_weights) | donor_weights <= 0] <- 1
  donor_weights <- donor_weights / sum(donor_weights, na.rm = TRUE)

  cover_mat <- vapply(
    seq_along(donor_lo),
    function(j) {
      length_grid >= donor_lo[[j]] & length_grid <= donor_hi[[j]]
    },
    logical(length(length_grid))
  )
  if (is.null(dim(cover_mat))) {
    cover_mat <- matrix(cover_mat, ncol = 1L)
  }
  support_raw <- as.numeric(cover_mat %*% donor_weights)
  support_raw[!is.finite(support_raw) | support_raw < 0] <- NA_real_
  positive <- is.finite(support_raw) & support_raw > 0
  if (!any(positive)) {
    return(rep(NA_real_, length(length_grid)))
  }
  if (sum(positive) >= 2L && any(!positive)) {
    support_raw <- stats::approx(
      x = length_grid[positive],
      y = support_raw[positive],
      xout = length_grid,
      method = "linear",
      rule = 2,
      ties = "ordered"
    )$y
  }
  smooth_keep <- is.finite(length_grid) & is.finite(support_raw) & support_raw > 0
  if (sum(smooth_keep) >= 4L) {
    support_spline <- tryCatch(
      stats::smooth.spline(
        x = length_grid[smooth_keep],
        y = support_raw[smooth_keep],
        spar = 0.6
      ),
      error = function(e) NULL
    )
    if (!is.null(support_spline)) {
      support_raw <- stats::predict(
        support_spline,
        x = length_grid
      )$y
      support_raw[!is.finite(support_raw) | support_raw <= 0] <- NA_real_
      positive <- is.finite(support_raw) & support_raw > 0
      if (sum(positive) >= 2L && any(!positive)) {
        support_raw <- stats::approx(
          x = length_grid[positive],
          y = support_raw[positive],
          xout = length_grid,
          method = "linear",
          rule = 2,
          ties = "ordered"
        )$y
      }
    }
  }
  if (!all(is.finite(support_raw))) {
    return(rep(NA_real_, length(length_grid)))
  }
  if (length(pdf_weights) != length(length_grid) || !any(is.finite(pdf_weights) & pdf_weights > 0)) {
    pdf_weights <- rep(1 / length(length_grid), length(length_grid))
  } else {
    pdf_weights <- pdf_weights / sum(pdf_weights, na.rm = TRUE)
  }
  support_mean <- stats::weighted.mean(support_raw, pdf_weights, na.rm = TRUE)
  if (!is.finite(support_mean) || support_mean <= 0) {
    return(rep(NA_real_, length(length_grid)))
  }
  support_norm <- support_raw / support_mean
  support_shape <- 1 / support_norm
  shape_mean <- stats::weighted.mean(support_shape, pdf_weights, na.rm = TRUE)
  if (!is.finite(shape_mean) || shape_mean <= 0) {
    return(rep(NA_real_, length(length_grid)))
  }
  support_shape / shape_mean
}

#' Add anchor-level length context to a policy table
#'
#' @param policy_tbl Policy table.
#' @param candidate_models Candidate-model table.
#' @param length_grid_n Number of support points.
#'
#' @return Policy table with anchor-length context columns.
#'
#' @keywords internal
#' @noRd
augment_anchor_length_context <- function(policy_tbl,
                                          candidate_models,
                                          length_grid_n = 400L) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  if (nrow(policy_tbl) == 0 || !"anchor_model_id" %in% names(policy_tbl)) {
    return(policy_tbl)
  }
  drop_cols <- intersect(
    c("expected_length_cm", "length_support_min_cm", "length_support_max_cm"),
    names(policy_tbl)
  )
  if (length(drop_cols) > 0) {
    policy_tbl <- dplyr::select(policy_tbl, -dplyr::all_of(drop_cols))
  }
  id_col <- reference_anchor_id_column(candidate_models)
  policy_tbl$anchor_model_id <- as.character(policy_tbl$anchor_model_id)
  anchor_context <- purrr::map_dfr(unique(policy_tbl$anchor_model_id), function(anchor_id_chr) {
    anchor_row <- candidate_models |>
      dplyr::filter(as.character(.data[[id_col]]) == anchor_id_chr) |>
      dplyr::slice(1)
    if (nrow(anchor_row) == 0) {
      return(tibble::tibble(
        anchor_model_id = anchor_id_chr,
        expected_length_cm = NA_real_,
        length_support_min_cm = NA_real_,
        length_support_max_cm = NA_real_
      ))
    }
    anchor_pdf <- anchor_pdf_from_row(anchor_row, n = length_grid_n)
    if (nrow(anchor_pdf) == 0) {
      return(tibble::tibble(
        anchor_model_id = anchor_id_chr,
        expected_length_cm = NA_real_,
        length_support_min_cm = NA_real_,
        length_support_max_cm = NA_real_
      ))
    }
    tibble::tibble(
      anchor_model_id = anchor_id_chr,
      expected_length_cm = stats::weighted.mean(anchor_pdf$length_cm, anchor_pdf$f_len, na.rm = TRUE),
      length_support_min_cm = min(anchor_pdf$length_cm, na.rm = TRUE),
      length_support_max_cm = max(anchor_pdf$length_cm, na.rm = TRUE)
    )
  })
  policy_tbl |>
    dplyr::left_join(anchor_context, by = "anchor_model_id")
}

#' Join anchor coefficient values onto a policy table
#'
#' @param policy_tbl Policy summary table with `anchor_model_id`.
#' @param candidate_models Candidate-model table containing anchor equations.
#'
#' @return The input table with anchor coefficient columns added.
#'
#' @keywords internal
#' @noRd
augment_anchor_coefficient_context <- function(policy_tbl,
                                               candidate_models) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  if (nrow(policy_tbl) == 0 || !"anchor_model_id" %in% names(policy_tbl)) {
    return(policy_tbl)
  }
  id_col <- reference_anchor_id_column(candidate_models)
  drop_cols <- intersect(
    c("anchor_slope_standard", "anchor_intercept_standard"),
    names(policy_tbl)
  )
  if (length(drop_cols) > 0) {
    policy_tbl <- dplyr::select(policy_tbl, -dplyr::all_of(drop_cols))
  }
  candidate_slope <- candidate_coalesce_column(
    candidate_models,
    c("slope_standard", "slope_len")
  )
  if (is.null(candidate_slope)) {
    candidate_slope <- rep(NA_real_, nrow(candidate_models))
  }
  candidate_intercept <- candidate_coalesce_column(
    candidate_models,
    c("intercept_standard", "intercept_len")
  )
  if (is.null(candidate_intercept)) {
    candidate_intercept <- rep(NA_real_, nrow(candidate_models))
  }
  anchor_context <- candidate_models |>
    dplyr::transmute(
      anchor_model_id = as.character(.data[[id_col]]),
      anchor_slope_standard = suppressWarnings(as.numeric(candidate_slope)),
      anchor_intercept_standard = suppressWarnings(as.numeric(candidate_intercept))
    ) |>
    dplyr::group_by(.data$anchor_model_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  policy_tbl$anchor_model_id <- as.character(policy_tbl$anchor_model_id)
  policy_tbl |>
    dplyr::left_join(anchor_context, by = "anchor_model_id")
}

#' Resolve q-scalars used by the strategy uncertainty layer
#'
#' @param row_now One-row target table.
#'
#' @return Named list of q-scalars.
#'
#' @keywords internal
#' @noRd
strategy_q_scalars <- function(row_now) {
  q95 <- suppressWarnings(as.numeric(
    (
      row_now$meta_post_selection_interval_log_width[[1]] %||%
        row_now$interval_log_width[[1]] %||%
        NA_real_
    ) / 2
  ))
  if (!is.finite(q95) || q95 <= 0) {
    q95 <- suppressWarnings(as.numeric(
      row_now$meta_q_abs_log_total[[1]] %||%
        row_now$q_abs_log_conformal[[1]] %||%
        row_now$meta_q_abs_log[[1]] %||%
        row_now$q_abs_log[[1]] %||%
        row_now$q_abs_log_total[[1]] %||%
        NA_real_
    ))
  }
  if (!is.finite(q95) || q95 <= 0) {
    multiplier_pred <- suppressWarnings(as.numeric(
      row_now$multiplier_pred[[1]] %||% NA_real_
    ))
    multiplier_lo <- suppressWarnings(as.numeric(
      row_now$meta_post_selection_multiplier_lo[[1]] %||%
        row_now$multiplier_lo[[1]] %||%
        NA_real_
    ))
    multiplier_hi <- suppressWarnings(as.numeric(
      row_now$meta_post_selection_multiplier_hi[[1]] %||%
        row_now$multiplier_hi[[1]] %||%
        NA_real_
    ))
    if (is.finite(multiplier_pred) &&
      is.finite(multiplier_lo) &&
      is.finite(multiplier_hi) &&
      multiplier_pred > 0 &&
      multiplier_lo > 0 &&
      multiplier_hi > 0) {
      q95 <- max(
        abs(log(multiplier_pred / multiplier_lo)),
        abs(log(multiplier_hi / multiplier_pred)),
        na.rm = TRUE
      )
    }
  }
  # q80/q90/q99 are derived from the same per-anchor sigma_base as q95, so all
  # four levels stay self-consistently species-conditioned. An earlier version
  # additionally floored q99 with meta_q_abs_log_simultaneous_total, a single
  # ratio pooled across every species in the dataset; that let one poorly
  # calibrated species set the 99% band width for every other species,
  # regardless of how tightly that species' own calibration behaved.
  z80 <- stats::qnorm(0.90)
  z90 <- stats::qnorm(0.95)
  z95 <- stats::qnorm(0.975)
  z99 <- stats::qnorm(0.995)
  sigma_base <- ifelse(is.finite(q95) & q95 > 0, q95 / z95, NA_real_)
  q80 <- ifelse(is.finite(sigma_base), sigma_base * z80, NA_real_)
  q90 <- ifelse(is.finite(sigma_base), sigma_base * z90, NA_real_)
  q99 <- ifelse(is.finite(sigma_base), sigma_base * z99, NA_real_)
  list(q80 = q80, q90 = q90, q95 = q95, q99 = q99)
}

#' Convert log-multiplier half-width to TS dB half-width
#'
#' @param q_log Log-scale half-width.
#'
#' @return Numeric scalar or vector.
#'
#' @keywords internal
#' @noRd
log_multiplier_halfwidth_to_ts_db <- function(q_log) {
  q_log <- suppressWarnings(as.numeric(q_log))
  ifelse(is.finite(q_log), (10 / log(10)) * q_log, NA_real_)
}

#' Compute lognormal mean-centered lower and upper shifts
#'
#' @param q_log Log-scale half-width.
#' @param z_value Z-score corresponding to the target coverage level.
#'
#' @return Named list with lower and upper shifts.
#'
#' @keywords internal
#' @noRd
lognormal_mean_centered_shifts <- function(q_log, z_value) {
  q_log <- suppressWarnings(as.numeric(q_log))
  z_value <- suppressWarnings(as.numeric(z_value))
  sigma_log <- ifelse(is.finite(q_log) & q_log > 0 & is.finite(z_value) & z_value > 0, q_log / z_value, NA_real_)
  lower_shift <- ifelse(is.finite(sigma_log), z_value * sigma_log + 0.5 * sigma_log^2, NA_real_)
  upper_shift <- ifelse(is.finite(sigma_log), z_value * sigma_log - 0.5 * sigma_log^2, NA_real_)
  list(lower = lower_shift, upper = pmax(upper_shift, 0))
}

#' Compute a coefficient shape modifier over length
#'
#' @param covariance 2x2 coefficient covariance matrix.
#' @param length_grid Length grid.
#' @param pdf_weights Length-support weights.
#'
#' @return Numeric vector of shape modifiers.
#'
#' @keywords internal
#' @noRd
coefficient_shape_modifier <- function(covariance,
                                       length_grid,
                                       pdf_weights) {
  covariance <- suppressWarnings(as.matrix(covariance))
  x <- suppressWarnings(as.numeric(log10(length_grid)))
  pdf_weights <- suppressWarnings(as.numeric(pdf_weights))
  keep <- is.finite(x)
  if (!is.matrix(covariance) || !identical(dim(covariance), c(2L, 2L)) || sum(keep) < 3L) {
    return(rep(1, length(length_grid)))
  }
  if (length(pdf_weights) != length(length_grid) || !any(is.finite(pdf_weights) & pdf_weights > 0)) {
    pdf_weights <- rep(1 / length(length_grid), length(length_grid))
  } else {
    pdf_weights <- pdf_weights / sum(pdf_weights, na.rm = TRUE)
  }
  X <- cbind(x[keep], 1)
  pred_var <- rowSums((X %*% covariance) * X)
  pred_sd <- sqrt(pmax(pred_var, 0))
  half_width <- stats::qnorm(0.975) * pred_sd
  avg_half_width <- stats::weighted.mean(half_width, pdf_weights[keep], na.rm = TRUE)
  out <- rep(1, length(length_grid))
  if (is.finite(avg_half_width) && avg_half_width > 0) {
    out[keep] <- half_width / avg_half_width
  }
  out[!is.finite(out)] <- 1
  out
}

#' Rescale coefficient covariance to match a target q95 width
#'
#' @param covariance 2x2 coefficient covariance matrix.
#' @param length_grid Length grid.
#' @param pdf_weights Length-support weights.
#' @param q95_scalar Target average q95 half-width.
#'
#' @return List with rescaled covariance and shape modifier.
#'
#' @keywords internal
#' @noRd
rescale_coefficient_covariance <- function(covariance,
                                           length_grid,
                                           pdf_weights,
                                           q95_scalar) {
  covariance <- suppressWarnings(as.matrix(covariance))
  x <- suppressWarnings(as.numeric(log10(length_grid)))
  pdf_weights <- suppressWarnings(as.numeric(pdf_weights))
  q95_scalar <- suppressWarnings(as.numeric(q95_scalar))
  keep <- is.finite(x)
  if (!is.matrix(covariance) || !identical(dim(covariance), c(2L, 2L)) || sum(keep) < 3L) {
    return(list(
      covariance = matrix(NA_real_, nrow = 2, ncol = 2),
      shape_modifier = rep(NA_real_, length(length_grid))
    ))
  }
  if (length(pdf_weights) != length(length_grid) || !any(is.finite(pdf_weights) & pdf_weights > 0)) {
    pdf_weights <- rep(1 / length(length_grid), length(length_grid))
  } else {
    pdf_weights <- pdf_weights / sum(pdf_weights, na.rm = TRUE)
  }
  X <- cbind(x[keep], 1)
  pred_var <- rowSums((X %*% covariance) * X)
  pred_sd <- sqrt(pmax(pred_var, 0))
  z95 <- stats::qnorm(0.975)
  half_width <- z95 * pred_sd
  avg_half_width <- stats::weighted.mean(half_width, pdf_weights[keep], na.rm = TRUE)
  scale_factor <- if (is.finite(q95_scalar) && q95_scalar > 0 && is.finite(avg_half_width) && avg_half_width > 0) {
    q95_scalar / avg_half_width
  } else {
    1
  }
  shape_modifier <- rep(NA_real_, length(length_grid))
  shape_modifier[keep] <- if (is.finite(avg_half_width) && avg_half_width > 0) {
    half_width / avg_half_width
  } else {
    rep(1, sum(keep))
  }
  list(
    covariance = (scale_factor^2) * covariance,
    shape_modifier = shape_modifier
  )
}

#' Check whether a coefficient covariance matrix is sane
#'
#' @param covariance 2x2 coefficient covariance matrix.
#' @param slope_hat Fitted slope.
#' @param intercept_hat Fitted intercept.
#' @param slope_half_width_max Maximum allowed slope half-width.
#' @param intercept_hi_max Maximum allowed upper intercept bound.
#'
#' @return Logical scalar.
#'
#' @keywords internal
#' @noRd
coefficient_covariance_is_sane <- function(covariance,
                                           slope_hat,
                                           intercept_hat,
                                           slope_half_width_max = 10,
                                           intercept_hi_max = -50) {
  covariance <- suppressWarnings(as.matrix(covariance))
  slope_hat <- suppressWarnings(as.numeric(slope_hat))
  intercept_hat <- suppressWarnings(as.numeric(intercept_hat))
  if (!is.matrix(covariance) || !identical(dim(covariance), c(2L, 2L)) || any(!is.finite(covariance))) {
    return(FALSE)
  }
  slope_se <- sqrt(max(covariance[1, 1], 0))
  intercept_se <- sqrt(max(covariance[2, 2], 0))
  z95 <- stats::qnorm(0.975)
  slope_lo <- slope_hat - z95 * slope_se
  slope_hi <- slope_hat + z95 * slope_se
  intercept_hi <- intercept_hat + z95 * intercept_se
  slope_half_width <- (slope_hi - slope_lo) / 2

  is.finite(slope_lo) &&
    is.finite(slope_hi) &&
    is.finite(intercept_hi) &&
    slope_lo > 0 &&
    slope_half_width <= slope_half_width_max &&
    intercept_hi <= intercept_hi_max
}

#' Build a fallback calibrated coefficient covariance
#'
#' @param length_grid Length grid.
#' @param pdf_weights Length-support weights.
#' @param q95_scalar Target average q95 half-width.
#'
#' @return List with covariance and shape modifier.
#'
#' @keywords internal
#' @noRd
calibrated_coefficient_covariance <- function(length_grid,
                                              pdf_weights,
                                              q95_scalar) {
  x <- suppressWarnings(as.numeric(log10(length_grid)))
  pdf_weights <- suppressWarnings(as.numeric(pdf_weights))
  q95_scalar <- suppressWarnings(as.numeric(q95_scalar))
  keep <- is.finite(x)
  if (sum(keep) < 3) {
    return(list(
      covariance = matrix(NA_real_, nrow = 2, ncol = 2),
      shape_modifier = rep(NA_real_, length(length_grid))
    ))
  }
  if (length(pdf_weights) != length(length_grid) || !any(is.finite(pdf_weights) & pdf_weights > 0)) {
    pdf_weights <- rep(1 / length(length_grid), length(length_grid))
  } else {
    pdf_weights <- pdf_weights / sum(pdf_weights, na.rm = TRUE)
  }
  X <- cbind(x[keep], 1)
  w <- pdf_weights[keep]
  info <- crossprod(X * sqrt(w), X * sqrt(w))
  Sigma_shape <- tryCatch(
    solve(info + diag(1e-8, nrow(info))),
    error = function(e) {
      eig <- eigen(info + diag(1e-8, nrow(info)), symmetric = TRUE)
      vals <- eig$values
      inv_vals <- ifelse(vals > 1e-10, 1 / vals, 0)
      eig$vectors %*% diag(inv_vals, nrow = length(inv_vals)) %*% t(eig$vectors)
    }
  )
  pred_var_shape <- rowSums((X %*% Sigma_shape) * X)
  pred_sd_shape <- sqrt(pmax(pred_var_shape, 0))
  z95 <- stats::qnorm(0.975)
  shape_half_width <- z95 * pred_sd_shape
  avg_half_width <- stats::weighted.mean(shape_half_width, w, na.rm = TRUE)
  scale_factor <- if (is.finite(q95_scalar) && q95_scalar > 0 && is.finite(avg_half_width) && avg_half_width > 0) {
    q95_scalar / avg_half_width
  } else {
    0
  }
  Sigma <- (scale_factor^2) * Sigma_shape
  shape_modifier <- rep(NA_real_, length(length_grid))
  shape_modifier[keep] <- if (is.finite(avg_half_width) && avg_half_width > 0) {
    shape_half_width / avg_half_width
  } else {
    rep(1, sum(keep))
  }
  list(covariance = Sigma, shape_modifier = shape_modifier)
}

#' Build the full strategy uncertainty context for one row
#'
#' @param row_now One-row target table.
#' @param candidate_models Candidate-model table.
#' @param anchor_scores Anchor score table.
#' @param policy_tbl Optional policy candidate table.
#' @param ts_calibration Optional TS calibration table.
#' @param coefficient_calibration Optional coefficient calibration table.
#' @param config Optional config object or list.
#' @param model_scores Optional model-score table.
#' @param species_lookup Optional species lookup.
#' @param policy_lookup Policy lookup table.
#' @param policy_path Optional policy registry path.
#' @param include_competition Logical; include near-tied competitors.
#' @param lock_fixed_slope Deprecated. Fixed-slope strategies retain coefficient
#'   prediction uncertainty from their leave-species-out residuals.
#' @param length_grid_n Number of support points.
#' @param ts_band_method TS-band construction method.
#'
#' @return List describing the full uncertainty context, or `NULL`.
#'
#' @keywords internal
#' @noRd
strategy_uncertainty_context <- function(row_now,
                                         candidate_models,
                                         anchor_scores,
                                         policy_tbl = NULL,
                                         ts_calibration = NULL,
                                         coefficient_calibration = NULL,
                                         config = NULL,
                                         model_scores = NULL,
                                         species_lookup = NULL,
                                         policy_lookup,
                                         policy_path = NULL,
                                         include_competition = TRUE,
                                         lock_fixed_slope = FALSE,
                                         length_grid_n = 400L,
                                         ts_band_method = c("current", "smooth_scale_shape")) {
  ts_band_method <- match.arg(ts_band_method)
  id_col <- reference_anchor_id_column(candidate_models)
  anchor_id_chr <- as.character(row_now$anchor_model_id[[1]])
  anchor_row <- candidate_models |>
    dplyr::filter(as.character(.data[[id_col]]) == anchor_id_chr) |>
    dplyr::slice(1)
  if (nrow(anchor_row) == 0) {
    return(NULL)
  }
  anchor_pdf <- anchor_pdf_from_row(anchor_row, n = length_grid_n)
  if (nrow(anchor_pdf) == 0) {
    return(NULL)
  }
  length_grid <- suppressWarnings(as.numeric(anchor_pdf$length_cm))
  pdf_weights <- suppressWarnings(as.numeric(anchor_pdf$f_len))
  anchor_slope_vals <- candidate_coalesce_column(anchor_row, c("slope_standard", "slope_len"))
  anchor_intercept_vals <- candidate_coalesce_column(anchor_row, c("intercept_standard", "intercept_len"))
  anchor_slope <- suppressWarnings(as.numeric(
    if (is.null(anchor_slope_vals) || length(anchor_slope_vals) == 0) NA_real_ else anchor_slope_vals[[1]]
  ))
  anchor_intercept <- suppressWarnings(as.numeric(
    if (is.null(anchor_intercept_vals) || length(anchor_intercept_vals) == 0) NA_real_ else anchor_intercept_vals[[1]]
  ))
  anchor_species_name <- if ("species_name" %in% names(anchor_row)) {
    as.character(anchor_row$species_name[[1]])
  } else {
    NA_character_
  }
  anchor_genus <- if ("genus" %in% names(anchor_row)) {
    as.character(anchor_row$genus[[1]] %||% stringr::word(anchor_species_name, 1))
  } else {
    stringr::word(anchor_species_name, 1)
  }
  anchor_family <- if ("family_name" %in% names(anchor_row)) {
    as.character(anchor_row$family_name[[1]] %||% if ("family" %in% names(anchor_row)) anchor_row$family[[1]] else NA_character_)
  } else if ("family" %in% names(anchor_row)) {
    as.character(anchor_row$family[[1]])
  } else {
    NA_character_
  }
  anchor_frequency_khz <- suppressWarnings(as.numeric(
    anchor_row$frequency_khz[[1]] %||% anchor_row$frequency[[1]] %||% NA_real_
  ))
  anchor_depth_min <- suppressWarnings(as.numeric(
    anchor_row$depth_min[[1]] %||% anchor_row$study_depth_min[[1]] %||%
      anchor_row$depth_midpoint[[1]] %||% anchor_row$study_depth_midpoint[[1]] %||%
      NA_real_
  ))
  anchor_depth_max <- suppressWarnings(as.numeric(
    anchor_row$depth_max[[1]] %||% anchor_row$study_depth_max[[1]] %||%
      anchor_row$depth_midpoint[[1]] %||% anchor_row$study_depth_midpoint[[1]] %||%
      NA_real_
  ))
  ts_anchor <- anchor_slope * log10(length_grid) + anchor_intercept
  policy_name <- as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]])
  branch_value <- resolve_selected_policy_branches(row_now)[[1]]
  is_fixed_branch <- identical(branch_value, "fixed20_only")
  selected_donor_ids <- policy_row_realized_donor_ids(row_now)
  q_scalars <- strategy_q_scalars(row_now)
  q_scalars_ts <- lapply(q_scalars, log_multiplier_halfwidth_to_ts_db)
  ts_min_anchor_neighbors <- suppressWarnings(as.integer(
    policy_selector_config_value(
      config,
      "min_bin_scores",
      sections = c("selection")
    ) %||% 10L
  ))
  if (!is.finite(ts_min_anchor_neighbors) || ts_min_anchor_neighbors < 1L) {
    ts_min_anchor_neighbors <- 10L
  }
  ts_curve_calibration <- select_ts_calibration_curve(
    ts_calibration = ts_calibration,
    row_now = row_now,
    target_u = seq(0, 1, length.out = length(length_grid)),
    min_anchor_neighbors = ts_min_anchor_neighbors
  )
  competition_sigma <- if (isTRUE(include_competition)) {
    selection_competition_covariance(
      policy_tbl = policy_tbl %||% tibble::tibble(),
      row_now = row_now,
      config = config
    )
  } else {
    NULL
  }
  selected_sigma <- selected_coefficient_covariance(
    coefficient_calibration = coefficient_calibration,
    row_now = row_now
  )
  slope_hat <- suppressWarnings(as.numeric(row_now$policy_slope_len[[1]]))
  intercept_hat <- suppressWarnings(as.numeric(row_now$policy_intercept_len[[1]]))
  selected_sigma_ok <- !is.null(selected_sigma) &&
    is.matrix(selected_sigma$covariance) &&
    identical(dim(selected_sigma$covariance), c(2L, 2L)) &&
    all(is.finite(selected_sigma$covariance)) &&
    is.finite(selected_sigma$n) &&
    selected_sigma$n >= 2L
  raw_covariance <- NULL
  coefficient_support_n <- NA_real_
  coefficient_covariance_source <- NA_character_
  if (selected_sigma_ok) {
    raw_covariance <- selected_sigma$covariance
    coefficient_support_n <- selected_sigma$n %||% NA_real_
    coefficient_covariance_source <- "empirical_selected_policy_residuals"
  }

  competition_support_n <- competition_sigma$n %||% NA_real_
  if (!is.null(competition_sigma) &&
    is.matrix(competition_sigma$covariance) &&
    all(is.finite(competition_sigma$covariance))) {
    raw_covariance <- if (is.null(raw_covariance)) {
      competition_sigma$covariance
    } else {
      raw_covariance + competition_sigma$covariance
    }
    coefficient_covariance_source <- if (is.na(coefficient_covariance_source)) {
      "near_tie_competition"
    } else {
      paste0(coefficient_covariance_source, "+near_tie_competition")
    }
  }

  support_candidates <- c(coefficient_support_n, competition_support_n)
  support_candidates <- support_candidates[is.finite(support_candidates)]
  coefficient_support_n <- if (length(support_candidates) > 0) {
    min(support_candidates)
  } else {
    NA_real_
  }

  if (!is.null(raw_covariance) && all(is.finite(raw_covariance))) {
    sigma_result <- rescale_coefficient_covariance(
      covariance = raw_covariance,
      length_grid = length_grid,
      pdf_weights = pdf_weights,
      q95_scalar = q_scalars_ts$q95
    )
    Sigma <- sigma_result$covariance
    shape_modifier <- sigma_result$shape_modifier
  } else {
    # Every selected policy carries post-selection uncertainty. When its
    # leave-species-out coefficient residuals cannot identify a covariance
    # direction, map that *same conditional radius* through the target's
    # length design. This is not a global or donor-spread substitute.
    covariance_result <- calibrated_coefficient_covariance(
      length_grid = length_grid,
      pdf_weights = pdf_weights,
      q95_scalar = q_scalars_ts$q95
    )
    shape_modifier <- covariance_result$shape_modifier
    Sigma <- covariance_result$covariance
    coefficient_covariance_source <- "policy_conditional_ts_geometry"
  }

  if (length(shape_modifier) != length(length_grid) || !all(is.finite(shape_modifier))) {
    shape_modifier <- rep(1, length(length_grid))
  }
  normalize_support_shape <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != length(length_grid)) {
      return(rep(1, length(length_grid)))
    }
    x[!is.finite(x) | x <= 0] <- NA_real_
    if (!any(is.finite(x))) {
      return(rep(1, length(length_grid)))
    }
    x[!is.finite(x)] <- stats::median(x[is.finite(x)], na.rm = TRUE)
    mean_x <- stats::weighted.mean(x, pdf_weights, na.rm = TRUE)
    if (!is.finite(mean_x) || mean_x <= 0) {
      mean_x <- mean(x[is.finite(x)], na.rm = TRUE)
    }
    if (!is.finite(mean_x) || mean_x <= 0) {
      return(rep(1, length(length_grid)))
    }
    x / mean_x
  }
  donor_support_shape <- donor_length_support_shape(
    anchor_scores = anchor_scores,
    anchor_id_chr = anchor_id_chr,
    length_grid = length_grid,
    pdf_weights = pdf_weights,
    donor_ids = selected_donor_ids
  )
  shape_strength_target <- suppressWarnings(as.numeric(
    policy_selector_config_value(
      policy_selector_config_data(config),
      "uncertainty_shape_min_scores",
      sections = c("selection", "policy_learner")
    ) %||%
      policy_selector_config_value(
        policy_selector_config_data(config),
        "min_bin_scores",
        sections = c("selection", "policy_learner")
      ) %||%
      10
  ))
  if (!is.finite(shape_strength_target) || shape_strength_target <= 0) {
    shape_strength_target <- 10
  }
  shape_strength_n <- suppressWarnings(as.numeric(
    row_now$meta_q_abs_log_n_scores[[1]] %||%
      row_now$selected_q_abs_log_n[[1]] %||%
      NA_real_
  ))
  support_shape_strength <- if (is.finite(shape_strength_n) && shape_strength_n > 0) {
    pmin(1, shape_strength_n / shape_strength_target)
  } else {
    0
  }
  shape_strength_source <- as.character(
    if ("meta_q_abs_log_source" %in% names(row_now)) {
      row_now$meta_q_abs_log_source[[1]]
    } else if ("selected_q_abs_log_source" %in% names(row_now)) {
      row_now$selected_q_abs_log_source[[1]]
    } else {
      NA_character_
    }
  )
  if (!is.na(shape_strength_source) && nzchar(shape_strength_source)) {
    if (grepl("shrunk", shape_strength_source, ignore.case = TRUE)) {
      support_shape_strength <- support_shape_strength * 0.75
    }
  }
  donor_support_relative <- rep(NA_real_, length(length_grid))
  if (all(is.finite(donor_support_shape))) {
    support_shape <- normalize_support_shape(donor_support_shape)
    donor_support_relative <- 1 / support_shape
    rel_max <- max(donor_support_relative, na.rm = TRUE)
    if (is.finite(rel_max) && rel_max > 0) {
      donor_support_relative <- donor_support_relative / rel_max
    }
  } else {
    support_shape <- normalize_support_shape(shape_modifier)
  }
  support_shape <- 1 + support_shape_strength * (support_shape - 1)

  # For the displayed TS(L) panel, keep the selected policy as the center and
  # combine:
  # 1) a selected-policy residual half-width that varies with length, and
  # 2) an asymmetric offset from the accepted tie-policy curves relative to the
  #    selected line.
  target_u <- seq(0, 1, length.out = length(length_grid))
  resolve_ts_curve <- function(candidate_names, fallback = 0) {
    if (nrow(ts_curve_calibration) <= 0) {
      return(rep(fallback, length(length_grid)))
    }
    curve_source <- resolve_numeric_candidates(ts_curve_calibration, candidate_names)
    keep_curve <- is.finite(ts_curve_calibration$u) & is.finite(curve_source)
    if (sum(keep_curve) == 1L) {
      return(rep(curve_source[keep_curve][[1]], length(length_grid)))
    }
    if (sum(keep_curve) >= 2L) {
      return(stats::approx(
        x = ts_curve_calibration$u[keep_curve],
        y = curve_source[keep_curve],
        xout = target_u,
        rule = 2,
        ties = "ordered"
      )$y)
    }
    rep(fallback, length(length_grid))
  }
  # Build the selected-policy width layer. The default path preserves the
  # existing tail-shaped construction. The alternate path uses a single smooth
  # residual-scale shape and only lets the signed residual balance determine
  # the lower-vs-upper split.
  build_signed_shape <- function(lower_names, upper_names) {
    lower_raw <- abs(resolve_ts_curve(lower_names, fallback = NA_real_))
    upper_raw <- resolve_ts_curve(upper_names, fallback = NA_real_)
    lower_raw[!is.finite(lower_raw) | lower_raw <= 0] <- NA_real_
    upper_raw[!is.finite(upper_raw) | upper_raw <= 0] <- NA_real_
    if (!any(is.finite(lower_raw))) {
      lower_raw <- rep(1, length(length_grid))
    }
    if (!any(is.finite(upper_raw))) {
      upper_raw <- rep(1, length(length_grid))
    }
    lower_mean <- stats::weighted.mean(lower_raw, pdf_weights, na.rm = TRUE)
    upper_mean <- stats::weighted.mean(upper_raw, pdf_weights, na.rm = TRUE)
    if (!is.finite(lower_mean) || lower_mean <= 0) {
      lower_mean <- mean(lower_raw[is.finite(lower_raw)], na.rm = TRUE)
    }
    if (!is.finite(upper_mean) || upper_mean <= 0) {
      upper_mean <- mean(upper_raw[is.finite(upper_raw)], na.rm = TRUE)
    }
    if (!is.finite(lower_mean) || lower_mean <= 0) {
      lower_mean <- 1
    }
    if (!is.finite(upper_mean) || upper_mean <= 0) {
      upper_mean <- 1
    }
    total_mean <- lower_mean + upper_mean
    if (!is.finite(total_mean) || total_mean <= 0) {
      total_mean <- 2
    }
    list(
      lower_shape = lower_raw / lower_mean,
      upper_shape = upper_raw / upper_mean,
      lower_share = lower_mean / total_mean,
      upper_share = upper_mean / total_mean
    )
  }

  build_smooth_scale_shape <- function(lower_names, upper_names) {
    base_raw <- resolve_ts_curve(
      c("log_sigma_scale_fit", "q80_log_sigma_abs_dev", "q90_log_sigma_abs_dev"),
      fallback = NA_real_
    )
    base_raw[!is.finite(base_raw) | base_raw <= 0] <- NA_real_
    if (!any(is.finite(base_raw))) {
      base_raw <- rep(1, length(length_grid))
    }
    base_mean <- stats::weighted.mean(base_raw, pdf_weights, na.rm = TRUE)
    if (!is.finite(base_mean) || base_mean <= 0) {
      base_mean <- mean(base_raw[is.finite(base_raw)], na.rm = TRUE)
    }
    if (!is.finite(base_mean) || base_mean <= 0) {
      base_mean <- 1
    }

    lower_raw <- abs(resolve_ts_curve(lower_names, fallback = NA_real_))
    upper_raw <- resolve_ts_curve(upper_names, fallback = NA_real_)
    lower_raw[!is.finite(lower_raw) | lower_raw < 0] <- NA_real_
    upper_raw[!is.finite(upper_raw) | upper_raw < 0] <- NA_real_
    lower_mean <- stats::weighted.mean(lower_raw, pdf_weights, na.rm = TRUE)
    upper_mean <- stats::weighted.mean(upper_raw, pdf_weights, na.rm = TRUE)
    if (!is.finite(lower_mean) || lower_mean < 0) {
      lower_mean <- mean(lower_raw[is.finite(lower_raw)], na.rm = TRUE)
    }
    if (!is.finite(upper_mean) || upper_mean < 0) {
      upper_mean <- mean(upper_raw[is.finite(upper_raw)], na.rm = TRUE)
    }
    if (!is.finite(lower_mean) || lower_mean < 0) {
      lower_mean <- 0
    }
    if (!is.finite(upper_mean) || upper_mean < 0) {
      upper_mean <- 0
    }
    total_mean <- lower_mean + upper_mean
    if (!is.finite(total_mean) || total_mean <= 0) {
      lower_mean <- upper_mean <- 0.5
      total_mean <- 1
    }

    list(
      lower_shape = base_raw / base_mean,
      upper_shape = base_raw / base_mean,
      lower_share = lower_mean / total_mean,
      upper_share = upper_mean / total_mean
    )
  }

  shape_builder <- if (identical(ts_band_method, "smooth_scale_shape")) {
    build_smooth_scale_shape
  } else {
    build_signed_shape
  }

  shape80 <- shape_builder(
    c("q10_log_sigma_residual_smooth", "q10_log_sigma_residual"),
    c("q90_log_sigma_residual_smooth", "q90_log_sigma_residual")
  )
  shape90 <- shape_builder(
    c("q05_log_sigma_residual_smooth", "q05_log_sigma_residual"),
    c("q95_log_sigma_residual_smooth", "q95_log_sigma_residual")
  )
  shape95 <- shape_builder(
    c("q025_log_sigma_residual_smooth", "q025_log_sigma_residual"),
    c("q975_log_sigma_residual_smooth", "q975_log_sigma_residual")
  )
  shape99 <- shape_builder(
    c("q005_log_sigma_residual_smooth", "q005_log_sigma_residual"),
    c("q995_log_sigma_residual_smooth", "q995_log_sigma_residual")
  )

  combine_shape <- function(base_shape) {
    out <- suppressWarnings(as.numeric(base_shape))
    if (length(out) != length(length_grid)) {
      out <- rep(1, length(length_grid))
    }
    out[!is.finite(out) | out <= 0] <- 1
    out <- out * support_shape
    out_mean <- stats::weighted.mean(out, pdf_weights, na.rm = TRUE)
    if (!is.finite(out_mean) || out_mean <= 0) {
      out_mean <- mean(out[is.finite(out)], na.rm = TRUE)
    }
    if (!is.finite(out_mean) || out_mean <= 0) {
      return(rep(1, length(length_grid)))
    }
    out / out_mean
  }

  shape80$lower_shape <- combine_shape(shape80$lower_shape)
  shape80$upper_shape <- combine_shape(shape80$upper_shape)
  shape90$lower_shape <- combine_shape(shape90$lower_shape)
  shape90$upper_shape <- combine_shape(shape90$upper_shape)
  shape95$lower_shape <- combine_shape(shape95$lower_shape)
  shape95$upper_shape <- combine_shape(shape95$upper_shape)
  shape99$lower_shape <- combine_shape(shape99$lower_shape)
  shape99$upper_shape <- combine_shape(shape99$upper_shape)

  lower80_log_sigma <- (2 * q_scalars$q80 * shape80$lower_share) * shape80$lower_shape
  upper80_log_sigma <- (2 * q_scalars$q80 * shape80$upper_share) * shape80$upper_shape
  lower90_log_sigma <- (2 * q_scalars$q90 * shape90$lower_share) * shape90$lower_shape
  upper90_log_sigma <- (2 * q_scalars$q90 * shape90$upper_share) * shape90$upper_shape
  lower95_log_sigma <- (2 * q_scalars$q95 * shape95$lower_share) * shape95$lower_shape
  upper95_log_sigma <- (2 * q_scalars$q95 * shape95$upper_share) * shape95$upper_shape
  lower99_log_sigma <- (2 * q_scalars$q99 * shape99$lower_share) * shape99$lower_shape
  upper99_log_sigma <- (2 * q_scalars$q99 * shape99$upper_share) * shape99$upper_shape

  q80_L_lower <- (10 / log(10)) * lower80_log_sigma
  q80_L_upper <- (10 / log(10)) * upper80_log_sigma
  q90_L_lower <- (10 / log(10)) * lower90_log_sigma
  q90_L_upper <- (10 / log(10)) * upper90_log_sigma
  q95_L_lower <- (10 / log(10)) * lower95_log_sigma
  q95_L_upper <- (10 / log(10)) * upper95_log_sigma
  q99_L_lower <- (10 / log(10)) * lower99_log_sigma
  q99_L_upper <- (10 / log(10)) * upper99_log_sigma

  # Coverage bands must remain nested pointwise. The quantile-specific shape
  # fits can otherwise cross locally, which makes the 99% band collapse inside
  # the 95% band even when the scalar 99% width is larger.
  gap_eps <- 1e-6
  q90_L_lower <- pmax(q90_L_lower, q80_L_lower + gap_eps)
  q95_L_lower <- pmax(q95_L_lower, q90_L_lower + gap_eps)
  q99_L_lower <- pmax(q99_L_lower, q95_L_lower + gap_eps)
  q90_L_upper <- pmax(q90_L_upper, q80_L_upper + gap_eps)
  q95_L_upper <- pmax(q95_L_upper, q90_L_upper + gap_eps)
  q99_L_upper <- pmax(q99_L_upper, q95_L_upper + gap_eps)

  q80_L <- 0.5 * (q80_L_lower + q80_L_upper)
  q90_L <- 0.5 * (q90_L_lower + q90_L_upper)
  q95_L <- 0.5 * (q95_L_lower + q95_L_upper)
  q99_L <- 0.5 * (q99_L_lower + q99_L_upper)
  ts_pred_raw <- slope_hat * log10(length_grid) + intercept_hat
  ts_center <- ts_pred_raw
  competitor_pool <- if (isTRUE(include_competition)) {
    selection_competition_pool(
      policy_tbl = policy_tbl %||% tibble::tibble(),
      row_now = row_now,
      config = config
    )
  } else {
    tibble::tibble()
  }
  comp_lo80 <- comp_hi80 <- rep(0, length(length_grid))
  comp_lo90 <- comp_hi90 <- rep(0, length(length_grid))
  comp_lo95 <- comp_hi95 <- rep(0, length(length_grid))
  comp_lo99 <- comp_hi99 <- rep(0, length(length_grid))
  if (nrow(competitor_pool) > 0 &&
    all(c("policy_slope_len", "policy_intercept_len") %in% names(competitor_pool))) {
    comp_slope <- suppressWarnings(as.numeric(competitor_pool$policy_slope_len))
    comp_intercept <- suppressWarnings(as.numeric(competitor_pool$policy_intercept_len))
    comp_score <- suppressWarnings(as.numeric(
      dplyr::coalesce(
        competitor_pool$.validation_error %||% NULL,
        competitor_pool$anchor_selection_validation_error %||% NULL
      )
    ))
    keep_comp <- is.finite(comp_slope) & is.finite(comp_intercept)
    if (sum(keep_comp) > 0) {
      comp_weights <- rep(1, sum(keep_comp))
      score_keep <- comp_score[keep_comp]
      if (length(score_keep) == length(comp_weights) && any(is.finite(score_keep))) {
        score_min <- min(score_keep, na.rm = TRUE)
        score_thr <- suppressWarnings(as.numeric(
          row_now$anchor_selection_validation_threshold[[1]] %||% NA_real_
        ))
        if (!is.finite(score_thr)) {
          score_thr <- max(score_keep, na.rm = TRUE)
        }
        score_scale <- score_thr - score_min
        if (!is.finite(score_scale) || score_scale <= 0) {
          score_scale <- stats::sd(score_keep, na.rm = TRUE)
        }
        if (!is.finite(score_scale) || score_scale <= 0) {
          score_scale <- 1
        }
        comp_weights <- exp(-(score_keep - score_min) / score_scale)
        comp_weights[!is.finite(comp_weights) | comp_weights <= 0] <- 1
      }
      competitor_ts_curves <- vapply(
        seq_len(sum(keep_comp)),
        function(j) comp_slope[keep_comp][[j]] * log10(length_grid) + comp_intercept[keep_comp][[j]],
        numeric(length(length_grid))
      )
      if (is.null(dim(competitor_ts_curves))) {
        competitor_ts_curves <- matrix(competitor_ts_curves, nrow = length(length_grid), ncol = 1)
      }
      deviation_matrix <- sweep(competitor_ts_curves, 1, ts_center, "-")
      # The selected policy line should remain the weighted 50th-quantile
      # reference of the policy-competition layer. Recenter using the weighted
      # median under the same score-proximity weights that define the outer
      # competition spread.
      comp_median <- apply(
        deviation_matrix,
        1,
        function(x) weighted_quantile_or_na(x, comp_weights, 0.5)
      )
      comp_median[!is.finite(comp_median)] <- 0
      centered_dev_matrix <- sweep(deviation_matrix, 1, comp_median, "-")
      build_signed_quantile <- function(prob, dev_matrix) {
        apply(dev_matrix, 1, function(x) weighted_quantile_or_na(x, comp_weights, prob))
      }
      comp_lo80 <- build_signed_quantile(0.10, centered_dev_matrix)
      comp_hi80 <- build_signed_quantile(0.90, centered_dev_matrix)
      comp_lo90 <- build_signed_quantile(0.05, centered_dev_matrix)
      comp_hi90 <- build_signed_quantile(0.95, centered_dev_matrix)
      comp_lo95 <- build_signed_quantile(0.025, centered_dev_matrix)
      comp_hi95 <- build_signed_quantile(0.975, centered_dev_matrix)
      comp_lo99 <- build_signed_quantile(0.005, centered_dev_matrix)
      comp_hi99 <- build_signed_quantile(0.995, centered_dev_matrix)
      comp_lo80[!is.finite(comp_lo80)] <- 0
      comp_hi80[!is.finite(comp_hi80)] <- 0
      comp_lo90[!is.finite(comp_lo90)] <- 0
      comp_hi90[!is.finite(comp_hi90)] <- 0
      comp_lo95[!is.finite(comp_lo95)] <- 0
      comp_hi95[!is.finite(comp_hi95)] <- 0
      comp_lo99[!is.finite(comp_lo99)] <- 0
      comp_hi99[!is.finite(comp_hi99)] <- 0
    }
  }
  ts_lo_80 <- ts_center + comp_lo80 - q80_L_lower
  ts_hi_80 <- ts_center + comp_hi80 + q80_L_upper
  ts_lo_90 <- ts_center + comp_lo90 - q90_L_lower
  ts_hi_90 <- ts_center + comp_hi90 + q90_L_upper
  ts_lo_95 <- ts_center + comp_lo95 - q95_L_lower
  ts_hi_95 <- ts_center + comp_hi95 + q95_L_upper
  ts_lo_99 <- ts_center + comp_lo99 - q99_L_lower
  ts_hi_99 <- ts_center + comp_hi99 + q99_L_upper
  # Nest each side (lower/upper distance from the center) independently
  # instead of collapsing both to a common symmetric half-width. The signed
  # construction above (comp_lo/hi and q_L_lower/upper) is asymmetric by
  # design; forcing both sides to match the larger of the two discarded that
  # asymmetry and could only ever widen the tighter side, never reflect it.
  nest_side <- function(dist, prior_dist) {
    valid <- is.finite(dist)
    if (!is.null(prior_dist)) {
      valid <- valid & is.finite(prior_dist)
      dist[valid] <- pmax(dist[valid], prior_dist[valid] + gap_eps)
    }
    dist
  }
  lo_dist_80 <- pmax(ts_center - ts_lo_80, 0)
  hi_dist_80 <- pmax(ts_hi_80 - ts_center, 0)
  lo_dist_90 <- nest_side(pmax(ts_center - ts_lo_90, 0), lo_dist_80)
  hi_dist_90 <- nest_side(pmax(ts_hi_90 - ts_center, 0), hi_dist_80)
  lo_dist_95 <- nest_side(pmax(ts_center - ts_lo_95, 0), lo_dist_90)
  hi_dist_95 <- nest_side(pmax(ts_hi_95 - ts_center, 0), hi_dist_90)
  lo_dist_99 <- nest_side(pmax(ts_center - ts_lo_99, 0), lo_dist_95)
  hi_dist_99 <- nest_side(pmax(ts_hi_99 - ts_center, 0), hi_dist_95)
  ts_lo_80 <- ts_center - lo_dist_80
  ts_hi_80 <- ts_center + hi_dist_80
  ts_lo_90 <- ts_center - lo_dist_90
  ts_hi_90 <- ts_center + hi_dist_90
  ts_lo_95 <- ts_center - lo_dist_95
  ts_hi_95 <- ts_center + hi_dist_95
  ts_lo_99 <- ts_center - lo_dist_99
  ts_hi_99 <- ts_center + hi_dist_99
  top_candidate_fields <- c(
    "anchor_model_id",
    "combined_distance",
    "w_adm"
  )
  top_row <- if (all(top_candidate_fields %in% names(anchor_scores))) {
    anchor_scores |>
      dplyr::filter(as.character(.data$anchor_model_id) == anchor_id_chr) |>
      (\(df) {
        donor_id_col <- if ("model_id" %in% names(df)) {
          "model_id"
        } else if ("model_id" %in% names(df)) {
          "model_id"
        } else {
          NA_character_
        }
        if (is.na(donor_id_col)) {
          df
        } else {
          df <- dplyr::filter(df, as.character(.data[[donor_id_col]]) != anchor_id_chr)
          if (length(selected_donor_ids) > 0L) {
            df <- dplyr::filter(
              df,
              as.character(.data[[donor_id_col]]) %in% selected_donor_ids
            )
          }
          df
        }
      })() |>
      dplyr::mutate(
        combined_distance = suppressWarnings(as.numeric(.data$combined_distance)),
        w_adm = suppressWarnings(as.numeric(.data$w_adm))
      ) |>
      (\(df) {
        slope_vals <- candidate_coalesce_column(df, c("slope_standard", "slope_len"))
        intercept_vals <- candidate_coalesce_column(df, c("intercept_standard", "intercept_len"))
        if (is.null(slope_vals) || length(slope_vals) == 0) {
          slope_vals <- rep(NA_real_, nrow(df))
        }
        if (is.null(intercept_vals) || length(intercept_vals) == 0) {
          intercept_vals <- rep(NA_real_, nrow(df))
        }
        df$slope_standard <- suppressWarnings(as.numeric(slope_vals))
        df$intercept_standard <- suppressWarnings(as.numeric(intercept_vals))
        df
      })() |>
      dplyr::filter(is.finite(.data$slope_standard), is.finite(.data$intercept_standard)) |>
      dplyr::arrange(.data$combined_distance, dplyr::desc(.data$w_adm)) |>
      dplyr::slice(1)
  } else {
    tibble::tibble()
  }
  ts_top_candidate <- if (nrow(top_row) == 1) {
    as.numeric(top_row$slope_standard[[1]]) * log10(length_grid) + as.numeric(top_row$intercept_standard[[1]])
  } else {
    rep(NA_real_, length(length_grid))
  }
  list(
    anchor_model_id = anchor_id_chr,
    anchor_species = as.character(row_now$anchor_species[[1]]),
    selected_policy = policy_name,
    length_cm = length_grid,
    u = seq(0, 1, length.out = length(length_grid)),
    ts_pred = ts_pred_raw,
    ts_pred_raw = ts_pred_raw,
    ts_center = ts_center,
    ts_anchor = ts_anchor,
    ts_top_candidate = ts_top_candidate,
    ts_lo_80 = ts_lo_80,
    ts_hi_80 = ts_hi_80,
    ts_lo_90 = ts_lo_90,
    ts_hi_90 = ts_hi_90,
    ts_lo_95 = ts_lo_95,
    ts_hi_95 = ts_hi_95,
    ts_lo_99 = ts_lo_99,
    ts_hi_99 = ts_hi_99,
    support_modifier = q95_L,
    support_shape = support_shape,
    support_shape_strength = support_shape_strength,
    support_calibration_n = shape_strength_n,
    support_relative = donor_support_relative,
    q80_L = q80_L,
    q90_L = q90_L,
    q95_L = q95_L,
    q99_L = q99_L,
    pdf_weight = pdf_weights,
    anchor_genus = anchor_genus,
    anchor_family = anchor_family,
    anchor_frequency_khz = anchor_frequency_khz,
    anchor_length_min = min(length_grid, na.rm = TRUE),
    anchor_length_max = max(length_grid, na.rm = TRUE),
    anchor_depth_min = anchor_depth_min,
    anchor_depth_max = anchor_depth_max,
    ts_calibration_score_n = if ("ts_score_n" %in% names(ts_curve_calibration)) {
      suppressWarnings(as.numeric(stats::median(ts_curve_calibration$ts_score_n, na.rm = TRUE)))
    } else {
      NA_real_
    },
    ts_calibration_anchor_n = if ("ts_anchor_n" %in% names(ts_curve_calibration)) {
      suppressWarnings(as.numeric(stats::median(ts_curve_calibration$ts_anchor_n, na.rm = TRUE)))
    } else {
      NA_real_
    },
    coefficient_covariance = Sigma,
    coefficient_support_n = coefficient_support_n,
    coefficient_covariance_source = coefficient_covariance_source,
    policy_selection_competitor_n = competition_support_n
  )
}

#' Recover a feasible coefficient region from a TS envelope
#'
#' @param length_grid Length grid.
#' @param ts_lo Lower TS envelope.
#' @param ts_hi Upper TS envelope.
#' @param fixed_slope Optional fixed slope.
#' @param slope_floor Minimum allowed slope.
#' @param intercept_ceiling Maximum allowed intercept.
#' @param tol Numerical tolerance.
#'
#' @return List of feasible slope/intercept bounds, or `NULL`.
#'
#' @keywords internal
#' @noRd
coefficient_region_from_ts_envelope <- function(length_grid,
                                                ts_lo,
                                                ts_hi,
                                                fixed_slope = NA_real_,
                                                slope_floor = 0,
                                                intercept_ceiling = 0,
                                                tol = 1e-8) {
  x <- suppressWarnings(as.numeric(log10(length_grid)))
  lo <- suppressWarnings(as.numeric(ts_lo))
  hi <- suppressWarnings(as.numeric(ts_hi))
  keep <- is.finite(x) & is.finite(lo) & is.finite(hi)
  if (sum(keep) < 2L) {
    return(NULL)
  }
  x <- x[keep]
  lo <- lo[keep]
  hi <- hi[keep]
  if (any(hi < lo - tol)) {
    return(NULL)
  }

  lower_envelope <- function(m) {
    max(lo - m * x)
  }
  upper_envelope <- function(m) {
    min(c(hi - m * x, intercept_ceiling))
  }

  if (is.finite(fixed_slope)) {
    b_lo <- lower_envelope(fixed_slope)
    b_hi <- upper_envelope(fixed_slope)
    if (!is.finite(b_lo) || !is.finite(b_hi) || b_lo > b_hi + tol) {
      return(NULL)
    }
    return(list(
      slope_lo = fixed_slope,
      slope_hi = fixed_slope,
      intercept_lo = b_lo,
      intercept_hi = b_hi
    ))
  }

  slope_lo <- suppressWarnings(as.numeric(slope_floor))
  slope_hi <- Inf

  if (!is.finite(slope_lo)) {
    slope_lo <- 0
  }

  for (i in seq_along(x)) {
    if (x[[i]] <= tol) {
      if (lo[[i]] > intercept_ceiling + tol) {
        return(NULL)
      }
    } else {
      slope_lo <- max(slope_lo, (lo[[i]] - intercept_ceiling) / x[[i]])
    }
  }

  n_x <- length(x)
  for (i in seq_len(n_x)) {
    for (j in seq_len(n_x)) {
      dx <- x[[j]] - x[[i]]
      rhs <- hi[[j]] - lo[[i]]
      if (abs(dx) <= tol) {
        if (rhs < -tol) {
          return(NULL)
        }
      } else {
        bound <- rhs / dx
        if (dx > 0) {
          slope_hi <- min(slope_hi, bound)
        } else {
          slope_lo <- max(slope_lo, bound)
        }
      }
    }
  }

  if (!is.finite(slope_hi) || slope_lo > slope_hi + tol) {
    return(NULL)
  }

  candidate_m <- c(slope_lo, slope_hi, 0.5 * (slope_lo + slope_hi))
  if (n_x >= 2L) {
    for (i in seq_len(n_x - 1L)) {
      for (j in (i + 1L):n_x) {
        dx <- x[[j]] - x[[i]]
        if (abs(dx) > tol) {
          candidate_m <- c(
            candidate_m,
            (lo[[j]] - lo[[i]]) / dx,
            (hi[[j]] - hi[[i]]) / dx,
            hi[[i]] / x[[i]],
            hi[[j]] / x[[j]]
          )
        }
      }
    }
  }
  candidate_m <- unique(candidate_m[is.finite(candidate_m)])
  candidate_m <- candidate_m[candidate_m >= slope_lo - tol & candidate_m <= slope_hi + tol]
  if (length(candidate_m) == 0L) {
    return(NULL)
  }

  feasible_m <- candidate_m[vapply(candidate_m, function(m) {
    lower_envelope(m) <= upper_envelope(m) + tol
  }, logical(1))]
  if (length(feasible_m) == 0L) {
    return(NULL)
  }

  intercept_lo <- min(vapply(feasible_m, lower_envelope, numeric(1)))
  intercept_hi <- max(vapply(feasible_m, upper_envelope, numeric(1)))
  if (!is.finite(intercept_lo) || !is.finite(intercept_hi) || intercept_lo > intercept_hi + tol) {
    return(NULL)
  }

  list(
    slope_lo = slope_lo,
    slope_hi = slope_hi,
    intercept_lo = intercept_lo,
    intercept_hi = intercept_hi
  )
}

#' Convert a TS band into coefficient bounds
#'
#' @param ctx Strategy uncertainty context.
#' @param branch_value Equation-branch label.
#' @param slope_hat Fitted slope.
#' @param intercept_hat Fitted intercept.
#' @param level Coverage level.
#' @param tol Numerical tolerance.
#'
#' @return List of coefficient bounds, or `NULL`.
#'
#' @keywords internal
#' @noRd
coefficient_bounds_from_ts_band <- function(ctx,
                                            branch_value,
                                            slope_hat,
                                            intercept_hat,
                                            level = 0.95,
                                            tol = 1e-10) {
  if (is.null(ctx)) {
    return(NULL)
  }

  suffix <- paste0("_", formatC(round(level * 100), format = "d", width = 0))
  lo_name <- paste0("ts_lo", suffix)
  hi_name <- paste0("ts_hi", suffix)
  if (!all(c("length_cm", lo_name, hi_name) %in% names(ctx))) {
    return(NULL)
  }

  length_cm <- suppressWarnings(as.numeric(ctx$length_cm))
  ts_lo <- suppressWarnings(as.numeric(ctx[[lo_name]]))
  ts_hi <- suppressWarnings(as.numeric(ctx[[hi_name]]))
  keep <- is.finite(length_cm) &
    length_cm > 0 &
    is.finite(ts_lo) &
    is.finite(ts_hi)
  if (sum(keep) < 2L) {
    return(NULL)
  }

  x <- log10(length_cm[keep])
  lo <- ts_lo[keep]
  hi <- ts_hi[keep]
  ord <- order(x)
  x <- x[ord]
  lo <- lo[ord]
  hi <- hi[ord]

  lower_fn <- function(m) {
    max(lo - m * x)
  }
  upper_fn <- function(m) {
    min(hi - m * x)
  }

  candidate_vertices <- tibble::tibble()

  dx <- outer(x, x, FUN = function(x_i, x_j) x_j - x_i)
  rhs <- outer(lo, hi, FUN = function(lo_i, hi_j) hi_j - lo_i)

  upper_candidates <- rhs[dx > tol] / dx[dx > tol]
  lower_candidates <- rhs[dx < -tol] / dx[dx < -tol]
  slope_lo <- if (length(lower_candidates) > 0) max(lower_candidates, na.rm = TRUE) else -Inf
  slope_hi <- if (length(upper_candidates) > 0) min(upper_candidates, na.rm = TRUE) else Inf

  if (!is.finite(slope_lo) || !is.finite(slope_hi) || slope_lo > slope_hi + tol) {
    return(NULL)
  }

  lower_intersections <- {
    num <- outer(lo, lo, FUN = function(a, b) b - a)
    den <- dx
    vals <- num[abs(den) > tol] / den[abs(den) > tol]
    vals[is.finite(vals)]
  }
  upper_intersections <- {
    num <- outer(hi, hi, FUN = function(a, b) b - a)
    den <- dx
    vals <- num[abs(den) > tol] / den[abs(den) > tol]
    vals[is.finite(vals)]
  }

  candidate_m <- unique(c(
    slope_lo,
    slope_hi,
    lower_intersections,
    upper_intersections,
    slope_hat
  ))
  candidate_m <- candidate_m[
    is.finite(candidate_m) &
      candidate_m >= slope_lo - tol &
      candidate_m <= slope_hi + tol
  ]
  if (length(candidate_m) == 0L) {
    return(NULL)
  }

  lower_vals <- vapply(candidate_m, lower_fn, numeric(1))
  upper_vals <- vapply(candidate_m, upper_fn, numeric(1))
  feasible <- is.finite(lower_vals) &
    is.finite(upper_vals) &
    lower_vals <= upper_vals + tol
  if (!any(feasible)) {
    return(NULL)
  }

  candidate_m <- candidate_m[feasible]
  lower_vals <- lower_vals[feasible]
  upper_vals <- upper_vals[feasible]

  intercept_lo <- min(lower_vals, na.rm = TRUE)
  intercept_hi <- max(upper_vals, na.rm = TRUE)
  if (!is.finite(intercept_lo) || !is.finite(intercept_hi) || intercept_lo > intercept_hi + tol) {
    return(NULL)
  }

  candidate_vertices <- tibble::tibble(
    slope = c(candidate_m, candidate_m),
    intercept = c(lower_vals, upper_vals)
  )

  list(
    slope_lo = slope_lo,
    slope_hi = slope_hi,
    intercept_lo = intercept_lo,
    intercept_hi = intercept_hi,
    vertices = candidate_vertices
  )
}

#' Build one coefficient interval summary from an uncertainty context
#'
#' @param row_now One-row target table.
#' @param ctx Strategy uncertainty context.
#' @param coefficient_calibration Optional coefficient calibration table.
#'
#' @return List describing coefficient intervals, or `NULL`.
#'
#' @keywords internal
#' @noRd
coefficient_interval_from_context <- function(row_now,
                                              ctx,
                                              coefficient_calibration = NULL) {
  row_now <- tibble::as_tibble(row_now)
  if (nrow(row_now) == 0) {
    return(NULL)
  }

  branch_value <- resolve_selected_policy_branches(row_now)[[1]]
  slope_hat <- suppressWarnings(as.numeric(row_now$policy_slope_len[[1]] %||% NA_real_))
  intercept_hat <- suppressWarnings(as.numeric(row_now$policy_intercept_len[[1]] %||% NA_real_))
  ctx_support_n <- suppressWarnings(as.numeric(
    ctx$coefficient_support_n %||%
      ctx$ts_calibration_anchor_n %||%
      NA_real_
  ))
  Sigma_ctx <- tryCatch(
    suppressWarnings(as.matrix(ctx$coefficient_covariance)),
    error = function(e) NULL
  )
  use_ctx_covariance <- !is.null(Sigma_ctx) &&
    is.matrix(Sigma_ctx) &&
    identical(dim(Sigma_ctx), c(2L, 2L)) &&
    all(is.finite(Sigma_ctx)) &&
    is.finite(ctx_support_n) &&
    ctx_support_n >= 2
  crit <- stats::qnorm(0.975)

  if (isTRUE(use_ctx_covariance)) {
    Sigma <- (Sigma_ctx + t(Sigma_ctx)) / 2
    diag(Sigma) <- pmax(diag(Sigma), 0)
    slope_se <- sqrt(Sigma[1, 1])
    intercept_se <- sqrt(Sigma[2, 2])
    slope_half_width <- crit * slope_se
    intercept_half_width <- crit * intercept_se
    corr <- if (Sigma[1, 1] > 0 && Sigma[2, 2] > 0) {
      Sigma[1, 2] / sqrt(Sigma[1, 1] * Sigma[2, 2])
    } else {
      0
    }
    support_n <- ctx_support_n
    interval_source <- "donor_covariance"
  } else {
    return(NULL)
  }

  if (!is.finite(intercept_half_width)) {
    return(NULL)
  }
  if (!is.finite(slope_half_width)) {
    return(NULL)
  }

  slope_lo <- slope_hat - slope_half_width
  slope_hi <- slope_hat + slope_half_width
  intercept_lo <- intercept_hat - intercept_half_width
  intercept_hi <- intercept_hat + intercept_half_width

  ts_region <- coefficient_bounds_from_ts_band(
    ctx = ctx,
    branch_value = branch_value,
    slope_hat = slope_hat,
    intercept_hat = intercept_hat,
    level = 0.95
  )
  if (!is.null(ts_region)) {
    region_slope_lo <- suppressWarnings(as.numeric(ts_region$slope_lo %||% NA_real_))
    region_slope_hi <- suppressWarnings(as.numeric(ts_region$slope_hi %||% NA_real_))
    region_intercept_lo <- suppressWarnings(as.numeric(ts_region$intercept_lo %||% NA_real_))
    region_intercept_hi <- suppressWarnings(as.numeric(ts_region$intercept_hi %||% NA_real_))
    if (is.finite(region_slope_lo) && is.finite(region_slope_hi)) {
      slope_lo <- max(slope_lo, region_slope_lo, na.rm = TRUE)
      slope_hi <- min(slope_hi, region_slope_hi, na.rm = TRUE)
    }
    if (is.finite(region_intercept_lo) && is.finite(region_intercept_hi)) {
      intercept_lo <- max(intercept_lo, region_intercept_lo, na.rm = TRUE)
      intercept_hi <- min(intercept_hi, region_intercept_hi, na.rm = TRUE)
    }
  }

  slope_half_width <- if (is.finite(slope_lo) && is.finite(slope_hi) && slope_lo <= slope_hi) {
    (slope_hi - slope_lo) / 2
  } else {
    NA_real_
  }
  intercept_half_width <- if (is.finite(intercept_lo) && is.finite(intercept_hi) && intercept_lo <= intercept_hi) {
    (intercept_hi - intercept_lo) / 2
  } else {
    NA_real_
  }
  slope_se <- if (is.finite(slope_half_width)) slope_half_width / crit else NA_real_
  intercept_se <- if (is.finite(intercept_half_width)) intercept_half_width / crit else NA_real_

  if (!is.finite(slope_lo) || !is.finite(slope_hi) || slope_lo > slope_hi) {
    return(NULL)
  }
  if (!is.finite(intercept_lo) || !is.finite(intercept_hi) || intercept_lo > intercept_hi) {
    return(NULL)
  }
  if (!is.finite(corr)) {
    corr <- 0
  }
  cov12 <- if (is.finite(slope_se) && is.finite(intercept_se)) {
    corr * slope_se * intercept_se
  } else {
    NA_real_
  }
  Sigma <- matrix(
    c(
      slope_se^2,
      cov12,
      cov12,
      intercept_se^2
    ),
    nrow = 2,
    byrow = TRUE
  )

  list(
    slope_hat = slope_hat,
    intercept_hat = intercept_hat,
    slope_lo = slope_lo,
    slope_hi = slope_hi,
    intercept_lo = intercept_lo,
    intercept_hi = intercept_hi,
    slope_se = slope_se,
    intercept_se = intercept_se,
    covariance = Sigma,
    correlation = corr,
    support_n = support_n,
    source = as.character(ctx$coefficient_covariance_source %||% interval_source),
    q95 = max(
      slope_half_width,
      intercept_half_width,
      na.rm = TRUE
    ),
    candidates = tibble::tibble(
      slope = c(slope_lo, slope_hat, slope_hi),
      intercept = c(intercept_lo, intercept_hat, intercept_hi),
      score = c(abs(slope_lo - slope_hat), 0, abs(slope_hi - slope_hat))
    )
  )
}

#' Build per-anchor TS conformal panel data
#'
#' Reconstructs selected-policy TS curves and their calibrated interval bands
#' for each anchor retained in a policy-selection table.
#'
#' @param selected_tbl Selected-policy table.
#' @param ts_calibration Optional selected-row TS residual calibration table.
#' @param coefficient_calibration Optional selected-row coefficient residual
#'   calibration table used as a fallback when TS calibration is unavailable.
#' @param candidate_models Candidate-model table.
#' @param anchor_scores Anchor-score table.
#' @param config Optional config list.
#' @param length_grid_n Number of grid points per anchor.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
build_ts_conformal_panel_data <- function(selected_tbl,
                                          ts_calibration,
                                          coefficient_calibration = NULL,
                                          candidate_models,
                                          anchor_scores,
                                          competition_policy_tbl = NULL,
                                          config = NULL,
                                          model_scores = NULL,
                                          species_lookup = NULL,
                                          policy_path = NULL,
                                          progress = NULL,
                                          length_grid_n = 400L,
                                          ts_band_method = c("current", "smooth_scale_shape")) {
  ts_band_method <- match.arg(ts_band_method)
  selected_tbl <- tibble::as_tibble(selected_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  anchor_scores <- tibble::as_tibble(anchor_scores)
  competition_policy_tbl <- tibble::as_tibble(competition_policy_tbl %||% tibble::tibble())
  if (nrow(selected_tbl) == 0 || nrow(candidate_models) == 0) {
    return(tibble::tibble())
  }
  policy_lookup <- policy_lookup_table(policy_path = policy_path)
  selected_tbl$selected_policy <- dplyr::coalesce(selected_tbl$selected_policy %||% NULL, selected_tbl$policy %||% NULL)
  selected_tbl$selected_equation_branch_filter <- resolve_selected_policy_branches(selected_tbl)
  selected_tbl <- selected_tbl |>
    dplyr::filter(
      !is.na(.data$anchor_model_id),
      !is.na(.data$selected_policy),
      is.finite(.data$policy_slope_len),
      is.finite(.data$policy_intercept_len)
    )
  n_selected <- nrow(selected_tbl)
  purrr::map_dfr(seq_len(n_selected), function(i) {
    row_now <- selected_tbl[i, , drop = FALSE]
    report_progress(
      progress,
      "[Referee]   TS envelope [", i, "/", n_selected, "] ",
      as.character(row_now$anchor_species[[1]] %||% row_now$anchor_model_id[[1]] %||% NA_character_)
    )
    ctx <- strategy_uncertainty_context(
      row_now = row_now,
      candidate_models = candidate_models,
      anchor_scores = anchor_scores,
      policy_tbl = competition_policy_tbl,
      ts_calibration = ts_calibration,
      coefficient_calibration = coefficient_calibration,
      config = config,
      model_scores = model_scores,
      species_lookup = species_lookup,
      policy_lookup = policy_lookup,
      policy_path = policy_path,
      include_competition = FALSE,
      lock_fixed_slope = FALSE,
      length_grid_n = length_grid_n,
      ts_band_method = ts_band_method
    )
    if (is.null(ctx)) {
      return(tibble::tibble())
    }
    tibble::tibble(
      anchor_model_id = ctx$anchor_model_id,
      anchor_species = ctx$anchor_species,
      selected_policy = ctx$selected_policy,
      length_cm = ctx$length_cm,
      u = ctx$u,
      ts_pred = ctx$ts_pred,
      ts_pred_raw = ctx$ts_pred_raw,
      ts_center = ctx$ts_center,
      ts_anchor = ctx$ts_anchor,
      ts_top_candidate = ctx$ts_top_candidate,
      support_modifier = ctx$support_modifier,
      support_shape = ctx$support_shape,
      support_shape_strength = ctx$support_shape_strength,
      support_calibration_n = ctx$support_calibration_n,
      support_relative = ctx$support_relative,
      q80_log_length = ctx$q80_L,
      q90_log_length = ctx$q90_L,
      q95_log_length = ctx$q95_L,
      q99_log_length = ctx$q99_L,
      ts_lo_80 = ctx$ts_lo_80,
      ts_hi_80 = ctx$ts_hi_80,
      ts_lo_90 = ctx$ts_lo_90,
      ts_hi_90 = ctx$ts_hi_90,
      ts_lo_95 = ctx$ts_lo_95,
      ts_hi_95 = ctx$ts_hi_95,
      ts_lo_99 = ctx$ts_lo_99,
      ts_hi_99 = ctx$ts_hi_99
    )
  })
}

#' Add coefficient interval summaries to a policy table
#'
#' @param policy_tbl Policy table.
#' @param candidate_models Candidate-model table.
#' @param anchor_scores Anchor score table.
#' @param competition_policy_tbl Optional policy table used for competitor context.
#' @param ts_calibration Optional TS calibration table.
#' @param coefficient_calibration Optional coefficient calibration table.
#' @param config Optional config object or list.
#' @param model_scores Optional model-score table.
#' @param species_lookup Optional species lookup.
#' @param policy_path Optional policy registry path.
#' @param length_grid_n Number of support points.
#'
#' @return Policy table with coefficient interval columns added.
#'
#' @keywords internal
#' @noRd
augment_policy_coefficient_intervals <- function(policy_tbl,
                                                 candidate_models,
                                                 anchor_scores,
                                                 competition_policy_tbl = NULL,
                                                 ts_calibration = NULL,
                                                 coefficient_calibration = NULL,
                                                 config = NULL,
                                                 model_scores = NULL,
                                                 species_lookup = NULL,
                                                 policy_path = NULL,
                                                 length_grid_n = 400L) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  competition_policy_tbl <- tibble::as_tibble(competition_policy_tbl %||% policy_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  anchor_scores <- tibble::as_tibble(anchor_scores)
  if (nrow(policy_tbl) == 0) {
    return(policy_tbl)
  }
  drop_cols <- grep(
    "^policy_(slope|intercept)_len_(se|lo_95|hi_95)(\\..+)?$|^policy_coefficient_(covariance|correlation|support_n|competitor_n|source)(\\..+)?$",
    names(policy_tbl),
    value = TRUE
  )
  if (length(drop_cols) > 0) {
    policy_tbl <- dplyr::select(policy_tbl, -dplyr::all_of(drop_cols))
  }
  if ("anchor_model_id" %in% names(policy_tbl)) {
    policy_tbl$anchor_model_id <- as.character(policy_tbl$anchor_model_id)
  }
  if ("policy" %in% names(policy_tbl)) {
    policy_tbl$policy <- as.character(policy_tbl$policy)
  }
  if ("equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$equation_branch_filter <- as.character(policy_tbl$equation_branch_filter)
  }
  if ("selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  if ("selected_equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$selected_equation_branch_filter <- as.character(policy_tbl$selected_equation_branch_filter)
  }
  policy_lookup <- policy_lookup_table(policy_path = policy_path)
  if (!"selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- if ("policy" %in% names(policy_tbl)) {
      as.character(policy_tbl$policy)
    } else {
      rep(NA_character_, nrow(policy_tbl))
    }
  } else if ("policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- dplyr::coalesce(
      as.character(policy_tbl$selected_policy),
      as.character(policy_tbl$policy)
    )
  } else {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  policy_tbl$selected_equation_branch_filter <- resolve_selected_policy_branches(policy_tbl)
  conditional_cache <- new.env(parent = emptyenv())
  conditional_key <- function(row_now) {
    paste(
      as.character(row_now$anchor_model_id[[1]] %||% NA_character_),
      as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_),
      as.character(resolve_selected_policy_branches(row_now)[[1]] %||% NA_character_),
      sep = "|"
    )
  }
  conditional_summary_for_row <- function(row_now) {
    key <- conditional_key(row_now)
    if (exists(key, envir = conditional_cache, inherits = FALSE)) {
      return(get(key, envir = conditional_cache, inherits = FALSE))
    }
    ctx_now <- strategy_uncertainty_context(
      row_now = row_now,
      candidate_models = candidate_models,
      anchor_scores = anchor_scores,
      policy_tbl = tibble::tibble(),
      ts_calibration = ts_calibration,
      coefficient_calibration = coefficient_calibration,
      config = config,
      model_scores = model_scores,
      species_lookup = species_lookup,
      policy_lookup = policy_lookup,
      policy_path = policy_path,
      include_competition = FALSE,
      lock_fixed_slope = FALSE,
      length_grid_n = length_grid_n
    )
    out <- coefficient_interval_from_context(
      row_now = row_now,
      ctx = ctx_now,
      coefficient_calibration = coefficient_calibration
    )
    assign(key, out, envir = conditional_cache)
    out
  }
  summary_rows <- purrr::map(seq_len(nrow(policy_tbl)), function(i) {
    row_now <- policy_tbl[i, , drop = FALSE]
    base_summary <- conditional_summary_for_row(row_now)
    if (is.null(base_summary)) {
      return(list(
        policy_slope_len_se = NA_real_,
        policy_intercept_len_se = NA_real_,
        policy_slope_len_lo_95 = NA_real_,
        policy_slope_len_hi_95 = NA_real_,
        policy_intercept_len_lo_95 = NA_real_,
        policy_intercept_len_hi_95 = NA_real_,
        policy_coefficient_covariance = NA_real_,
        policy_coefficient_correlation = NA_real_,
        policy_coefficient_support_n = NA_real_,
        policy_coefficient_competitor_n = NA_real_
      ))
    }
    competitor_pool <- selection_competition_pool(
      policy_tbl = competition_policy_tbl,
      row_now = row_now,
      config = config
    )
    competitor_summaries <- purrr::map(
      seq_len(nrow(competitor_pool)),
      function(j) conditional_summary_for_row(competitor_pool[j, , drop = FALSE])
    )
    competitor_summaries <- competitor_summaries[!vapply(competitor_summaries, is.null, logical(1))]
    slope_lo <- suppressWarnings(as.numeric(base_summary$slope_lo %||% NA_real_))
    slope_hi <- suppressWarnings(as.numeric(base_summary$slope_hi %||% NA_real_))
    intercept_lo <- suppressWarnings(as.numeric(base_summary$intercept_lo %||% NA_real_))
    intercept_hi <- suppressWarnings(as.numeric(base_summary$intercept_hi %||% NA_real_))
    slope_se <- if (is.finite(slope_lo) && is.finite(slope_hi)) (slope_hi - slope_lo) / (2 * stats::qnorm(0.975)) else NA_real_
    intercept_se <- if (is.finite(intercept_lo) && is.finite(intercept_hi)) (intercept_hi - intercept_lo) / (2 * stats::qnorm(0.975)) else NA_real_
    Sigma <- base_summary$covariance
    list(
      policy_slope_len_se = slope_se,
      policy_intercept_len_se = intercept_se,
      policy_slope_len_lo_95 = slope_lo,
      policy_slope_len_hi_95 = slope_hi,
      policy_intercept_len_lo_95 = intercept_lo,
      policy_intercept_len_hi_95 = intercept_hi,
      policy_coefficient_covariance = Sigma[1, 2],
      policy_coefficient_correlation = base_summary$correlation %||% NA_real_,
      policy_coefficient_support_n = suppressWarnings(as.numeric(base_summary$support_n %||% NA_real_)),
      policy_coefficient_source = as.character(base_summary$source %||% NA_character_),
      policy_coefficient_competitor_n = length(competitor_summaries) + 1L
    )
  })
  summaries <- dplyr::bind_cols(
    tibble::tibble(
      anchor_model_id = as.character(policy_tbl$anchor_model_id),
      policy = as.character(policy_tbl$selected_policy %||% policy_tbl$policy),
      equation_branch_filter = as.character(
        policy_tbl$selected_equation_branch_filter %||% policy_tbl$equation_branch_filter
      )
    ),
    dplyr::bind_rows(summary_rows)
  )
  join_cols <- intersect(c("anchor_model_id", "policy", "equation_branch_filter"), intersect(names(policy_tbl), names(summaries)))
  policy_tbl |>
    dplyr::left_join(summaries, by = join_cols)
}

#' Add conditional coefficient interval summaries to a policy table
#'
#' @param policy_tbl Policy table.
#' @param candidate_models Candidate-model table.
#' @param anchor_scores Anchor score table.
#' @param ts_calibration Optional TS calibration table.
#' @param coefficient_calibration Optional coefficient calibration table.
#' @param config Optional config object or list.
#' @param model_scores Optional model-score table.
#' @param species_lookup Optional species lookup.
#' @param policy_path Optional policy registry path.
#' @param length_grid_n Number of support points.
#'
#' @return Policy table with conditional coefficient interval columns added.
#'
#' @keywords internal
#' @noRd
augment_conditional_coeff_intervals <- function(policy_tbl,
                                                candidate_models,
                                                anchor_scores,
                                                ts_calibration = NULL,
                                                coefficient_calibration = NULL,
                                                config = NULL,
                                                model_scores = NULL,
                                                species_lookup = NULL,
                                                policy_path = NULL,
                                                length_grid_n = 400L) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  anchor_scores <- tibble::as_tibble(anchor_scores)
  if (nrow(policy_tbl) == 0) {
    return(policy_tbl)
  }
  drop_cols <- grep(
    "^conditional_policy_(slope|intercept)_len_(se|lo_95|hi_95)$|^conditional_policy_coefficient_(covariance|correlation|support_n|competitor_n|source)$",
    names(policy_tbl),
    value = TRUE
  )
  if (length(drop_cols) > 0) {
    policy_tbl <- dplyr::select(policy_tbl, -dplyr::all_of(drop_cols))
  }
  if ("anchor_model_id" %in% names(policy_tbl)) {
    policy_tbl$anchor_model_id <- as.character(policy_tbl$anchor_model_id)
  }
  if ("policy" %in% names(policy_tbl)) {
    policy_tbl$policy <- as.character(policy_tbl$policy)
  }
  if ("equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$equation_branch_filter <- as.character(policy_tbl$equation_branch_filter)
  }
  if ("selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  if ("selected_equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$selected_equation_branch_filter <- as.character(policy_tbl$selected_equation_branch_filter)
  }
  policy_lookup <- policy_lookup_table(policy_path = policy_path)
  if (!"selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- if ("policy" %in% names(policy_tbl)) {
      as.character(policy_tbl$policy)
    } else {
      rep(NA_character_, nrow(policy_tbl))
    }
  } else if ("policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- dplyr::coalesce(
      as.character(policy_tbl$selected_policy),
      as.character(policy_tbl$policy)
    )
  } else {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  policy_tbl$selected_equation_branch_filter <- resolve_selected_policy_branches(policy_tbl)
  conditional_cache <- new.env(parent = emptyenv())
  conditional_key <- function(row_now) {
    paste(
      as.character(row_now$anchor_model_id[[1]] %||% NA_character_),
      as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_),
      as.character(resolve_selected_policy_branches(row_now)[[1]] %||% NA_character_),
      sep = "|"
    )
  }
  conditional_summary_for_row <- function(row_now) {
    key <- conditional_key(row_now)
    if (exists(key, envir = conditional_cache, inherits = FALSE)) {
      return(get(key, envir = conditional_cache, inherits = FALSE))
    }
    ctx_now <- strategy_uncertainty_context(
      row_now = row_now,
      candidate_models = candidate_models,
      anchor_scores = anchor_scores,
      policy_tbl = tibble::tibble(),
      ts_calibration = ts_calibration,
      coefficient_calibration = coefficient_calibration,
      config = config,
      model_scores = model_scores,
      species_lookup = species_lookup,
      policy_lookup = policy_lookup,
      policy_path = policy_path,
      include_competition = FALSE,
      lock_fixed_slope = FALSE,
      length_grid_n = length_grid_n
    )
    out <- coefficient_interval_from_context(
      row_now = row_now,
      ctx = ctx_now,
      coefficient_calibration = coefficient_calibration
    )
    assign(key, out, envir = conditional_cache)
    out
  }
  summary_rows <- purrr::map(seq_len(nrow(policy_tbl)), function(i) {
    row_now <- policy_tbl[i, , drop = FALSE]
    base_summary <- conditional_summary_for_row(row_now)
    if (is.null(base_summary)) {
      return(list(
        conditional_policy_slope_len_se = NA_real_,
        conditional_policy_intercept_len_se = NA_real_,
        conditional_policy_slope_len_lo_95 = NA_real_,
        conditional_policy_slope_len_hi_95 = NA_real_,
        conditional_policy_intercept_len_lo_95 = NA_real_,
        conditional_policy_intercept_len_hi_95 = NA_real_,
        conditional_policy_coefficient_covariance = NA_real_,
        conditional_policy_coefficient_correlation = NA_real_,
        conditional_policy_coefficient_support_n = NA_real_,
        conditional_policy_coefficient_competitor_n = NA_real_
      ))
    }
    Sigma <- base_summary$covariance
    slope_se <- base_summary$slope_se
    intercept_se <- base_summary$intercept_se
    list(
      conditional_policy_slope_len_se = slope_se,
      conditional_policy_intercept_len_se = intercept_se,
      conditional_policy_slope_len_lo_95 = base_summary$slope_lo,
      conditional_policy_slope_len_hi_95 = base_summary$slope_hi,
      conditional_policy_intercept_len_lo_95 = base_summary$intercept_lo,
      conditional_policy_intercept_len_hi_95 = base_summary$intercept_hi,
      conditional_policy_coefficient_covariance = Sigma[1, 2],
      conditional_policy_coefficient_correlation = base_summary$correlation %||% NA_real_,
      conditional_policy_coefficient_support_n = suppressWarnings(as.numeric(base_summary$support_n %||% NA_real_)),
      conditional_policy_coefficient_source = as.character(base_summary$source %||% NA_character_),
      conditional_policy_coefficient_competitor_n = 1
    )
  })
  summaries <- dplyr::bind_cols(
    tibble::tibble(
      anchor_model_id = as.character(policy_tbl$anchor_model_id),
      policy = as.character(policy_tbl$selected_policy %||% policy_tbl$policy),
      equation_branch_filter = as.character(
        policy_tbl$selected_equation_branch_filter %||% policy_tbl$equation_branch_filter
      )
    ),
    dplyr::bind_rows(summary_rows)
  )
  join_cols <- intersect(c("anchor_model_id", "policy", "equation_branch_filter"), intersect(names(policy_tbl), names(summaries)))
  policy_tbl |>
    dplyr::left_join(summaries, by = join_cols)
}

#' Add TS-envelope summary features to a policy table
#'
#' @param policy_tbl Policy table.
#' @param candidate_models Candidate-model table.
#' @param anchor_scores Anchor score table.
#' @param ts_calibration Optional TS calibration table.
#' @param coefficient_calibration Optional coefficient calibration table.
#' @param config Optional config object or list.
#' @param model_scores Optional model-score table.
#' @param species_lookup Optional species lookup.
#' @param policy_path Optional policy registry path.
#' @param length_grid_n Number of support points.
#'
#' @return Policy table with TS-envelope summary columns added.
#'
#' @keywords internal
#' @noRd
augment_policy_ts_envelope_summary <- function(policy_tbl,
                                               candidate_models,
                                               anchor_scores,
                                               ts_calibration = NULL,
                                               coefficient_calibration = NULL,
                                               config = NULL,
                                               model_scores = NULL,
                                               species_lookup = NULL,
                                               policy_path = NULL,
                                               length_grid_n = 400L) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  anchor_scores <- tibble::as_tibble(anchor_scores)
  if (nrow(policy_tbl) == 0) {
    return(policy_tbl)
  }

  drop_cols <- grep(
    "^candidate_ts_(q95|q99)_(mean|max)$|^candidate_ts_(hi99_max|lo99_min)$|^candidate_ts_interval99_mean$",
    names(policy_tbl),
    value = TRUE
  )
  if (length(drop_cols) > 0) {
    policy_tbl <- dplyr::select(policy_tbl, -dplyr::all_of(drop_cols))
  }

  if ("anchor_model_id" %in% names(policy_tbl)) {
    policy_tbl$anchor_model_id <- as.character(policy_tbl$anchor_model_id)
  }
  if ("policy" %in% names(policy_tbl)) {
    policy_tbl$policy <- as.character(policy_tbl$policy)
  }
  if ("equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$equation_branch_filter <- as.character(policy_tbl$equation_branch_filter)
  }
  if ("selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  if ("selected_equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$selected_equation_branch_filter <- as.character(policy_tbl$selected_equation_branch_filter)
  }
  policy_lookup <- policy_lookup_table(policy_path = policy_path)
  if (!"selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- if ("policy" %in% names(policy_tbl)) {
      as.character(policy_tbl$policy)
    } else {
      rep(NA_character_, nrow(policy_tbl))
    }
  } else if ("policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- dplyr::coalesce(
      as.character(policy_tbl$selected_policy),
      as.character(policy_tbl$policy)
    )
  } else {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  policy_tbl$selected_equation_branch_filter <- resolve_selected_policy_branches(policy_tbl)

  summary_cache <- new.env(parent = emptyenv())
  summary_key <- function(row_now) {
    paste(
      as.character(row_now$anchor_model_id[[1]] %||% NA_character_),
      as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_),
      as.character(resolve_selected_policy_branches(row_now)[[1]] %||% NA_character_),
      sep = "|"
    )
  }
  ts_summary_for_row <- function(row_now) {
    key <- summary_key(row_now)
    if (exists(key, envir = summary_cache, inherits = FALSE)) {
      return(get(key, envir = summary_cache, inherits = FALSE))
    }
    ctx_now <- strategy_uncertainty_context(
      row_now = row_now,
      candidate_models = candidate_models,
      anchor_scores = anchor_scores,
      policy_tbl = tibble::tibble(),
      ts_calibration = ts_calibration,
      coefficient_calibration = coefficient_calibration,
      config = config,
      model_scores = model_scores,
      species_lookup = species_lookup,
      policy_lookup = policy_lookup,
      policy_path = policy_path,
      include_competition = FALSE,
      lock_fixed_slope = FALSE,
      length_grid_n = length_grid_n
    )
    if (is.null(ctx_now)) {
      out <- NULL
    } else {
      weights <- suppressWarnings(as.numeric(ctx_now$pdf_weight %||% rep(NA_real_, length(ctx_now$length_cm))))
      if (!any(is.finite(weights) & weights > 0)) {
        weights <- rep(1 / length(ctx_now$length_cm), length(ctx_now$length_cm))
      } else {
        weights[!is.finite(weights) | weights < 0] <- 0
        weights <- weights / sum(weights, na.rm = TRUE)
      }
      q95_vals <- suppressWarnings(as.numeric(ctx_now$q95_L))
      q99_vals <- suppressWarnings(as.numeric(ctx_now$q99_L))
      hi99_vals <- suppressWarnings(as.numeric(ctx_now$ts_hi_99))
      lo99_vals <- suppressWarnings(as.numeric(ctx_now$ts_lo_99))
      out <- list(
        candidate_ts_q95_mean = weighted_mean_or_na(q95_vals, weights),
        candidate_ts_q99_mean = weighted_mean_or_na(q99_vals, weights),
        candidate_ts_q95_max = {
          q95_keep <- q95_vals[is.finite(q95_vals)]
          if (length(q95_keep) == 0L) NA_real_ else max(q95_keep)
        },
        candidate_ts_q99_max = {
          q99_keep <- q99_vals[is.finite(q99_vals)]
          if (length(q99_keep) == 0L) NA_real_ else max(q99_keep)
        },
        candidate_ts_hi99_max = {
          hi_keep <- hi99_vals[is.finite(hi99_vals)]
          if (length(hi_keep) == 0L) NA_real_ else max(hi_keep)
        },
        candidate_ts_lo99_min = {
          lo_keep <- lo99_vals[is.finite(lo99_vals)]
          if (length(lo_keep) == 0L) NA_real_ else min(lo_keep)
        }
      )
      out$candidate_ts_interval99_mean <- if (is.finite(out$candidate_ts_q99_mean)) {
        2 * out$candidate_ts_q99_mean
      } else {
        NA_real_
      }
    }
    assign(key, out, envir = summary_cache)
    out
  }

  summary_rows <- purrr::map(seq_len(nrow(policy_tbl)), function(i) {
    row_now <- policy_tbl[i, , drop = FALSE]
    ts_summary <- ts_summary_for_row(row_now)
    if (is.null(ts_summary)) {
      ts_summary <- list(
        candidate_ts_q95_mean = NA_real_,
        candidate_ts_q99_mean = NA_real_,
        candidate_ts_q95_max = NA_real_,
        candidate_ts_q99_max = NA_real_,
        candidate_ts_hi99_max = NA_real_,
        candidate_ts_lo99_min = NA_real_,
        candidate_ts_interval99_mean = NA_real_
      )
    }
    list(
      candidate_ts_q95_mean = ts_summary$candidate_ts_q95_mean,
      candidate_ts_q99_mean = ts_summary$candidate_ts_q99_mean,
      candidate_ts_q95_max = ts_summary$candidate_ts_q95_max,
      candidate_ts_q99_max = ts_summary$candidate_ts_q99_max,
      candidate_ts_hi99_max = ts_summary$candidate_ts_hi99_max,
      candidate_ts_lo99_min = ts_summary$candidate_ts_lo99_min,
      candidate_ts_interval99_mean = ts_summary$candidate_ts_interval99_mean
    )
  })
  summaries <- dplyr::bind_cols(
    tibble::tibble(
      anchor_model_id = as.character(policy_tbl$anchor_model_id),
      policy = as.character(policy_tbl$selected_policy %||% policy_tbl$policy),
      equation_branch_filter = as.character(
        policy_tbl$selected_equation_branch_filter %||% policy_tbl$equation_branch_filter
      )
    ),
    dplyr::bind_rows(summary_rows)
  )

  join_cols <- intersect(c("anchor_model_id", "policy", "equation_branch_filter"), intersect(names(policy_tbl), names(summaries)))
  policy_tbl |>
    dplyr::left_join(summaries, by = join_cols)
}

#' Run anchor conformal summaries
#'
#' Builds policy-level conformal calibration, benchmark-level conformal
#' coverage summaries, and relative-length TS calibration from pseudo-anchor
#' benchmark outputs.
#'
#' @param policy_perf Pseudo-anchor policy-performance table.
#' @param species_performance_table Optional leave-one-species-out policy-performance table.
#' @param ts_error Optional policy TS-error table.
#' @param alpha Miscoverage level.
#' @param pseudo_label Label for the pseudo-anchor benchmark summary.
#' @param species_label Label for the leave-one-species-out benchmark summary.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#'
#' @return A list containing calibration and summary tables.
#'
#' @keywords internal
#' @noRd
run_anchor_conformal <- function(policy_perf,
                                 species_performance_table = NULL,
                                 ts_error = NULL,
                                 alpha = 0.10,
                                 pseudo_label = "pseudo_anchor",
                                 species_label = "species_block",
                                 cache_path = NULL,
                                 refresh = FALSE) {
  # Validate the benchmark inputs and cache controls before any conformal
  # summaries are computed.
  if (!is.data.frame(policy_perf)) {
    stop("'policy_perf' must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.null(species_performance_table) && !is.data.frame(species_performance_table)) {
    stop("'species_performance_table' must be NULL or a data frame/tibble.", call. = FALSE)
  }
  if (!is.null(ts_error) && !is.data.frame(ts_error)) {
    stop("'ts_error' must be NULL or a data frame/tibble.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be one finite number strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.null(cache_path) &&
    (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Reuse the cached conformal object when available unless a refresh was
  # explicitly requested.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  conf_cal <- compute_conformal_scores(
    policy_perf = policy_perf,
    alpha = alpha
  )

  # Build the pseudo-anchor summary first because it is always required.
  pseudo_sum <- summarize_conformal(
    policy_perf = policy_perf,
    conf_cal = conf_cal,
    bench_label = pseudo_label
  )

  # Optionally summarize the species-block benchmark with the same calibration
  # table so the two validation schemes stay directly comparable.
  species_sum <- if (is.null(species_performance_table)) {
    list(overall = tibble::tibble(), by_species = tibble::tibble())
  } else {
    summarize_conformal(
      policy_perf = species_performance_table,
      conf_cal = conf_cal,
      bench_label = species_label
    )
  }

  # Optionally build the relative-length TS calibration summary.
  ts_cal <- if (is.null(ts_error)) {
    tibble::tibble()
  } else {
    summarize_ts_calibration(ts_error)
  }

  result <- list(
    conf_cal = conf_cal,
    pseudo_sum = pseudo_sum,
    species_sum = species_sum,
    overall_sum = dplyr::bind_rows(pseudo_sum$overall, species_sum$overall),
    species_cov = dplyr::bind_rows(pseudo_sum$by_species, species_sum$by_species),
    ts_cal = ts_cal
  )

  # Cache the in-memory conformal summaries only when a cache path was
  # supplied.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  result
}
