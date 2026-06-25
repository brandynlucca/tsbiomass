#' Compute weighted quantiles
#'
#' @param x Numeric values.
#' @param w Numeric weights.
#' @param probs Numeric probability vector.
#'
#' @return Numeric vector.
#'
#' @keywords internal
weighted_quantile <- function(x,
                              w,
                              probs = c(0.05, 0.50, 0.95)) {
  # Restrict the calculation to finite values with finite non-negative weights
  # before building the weighted empirical distribution.
  keep <- is.finite(x) & is.finite(w) & w >= 0
  x <- x[keep]
  w <- w[keep]

  if (length(x) == 0 || sum(w) <= 0) {
    return(rep(NA_real_, length(probs)))
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord] / sum(w)
  cw <- cumsum(w)

  # Interpolate each requested quantile on the weighted cumulative
  # distribution.
  vapply(probs, function(p) {
    idx <- which(cw >= p)[1]
    if (is.na(idx)) {
      return(x[[length(x)]])
    }
    x[[idx]]
  }, numeric(1))
}

#' Predict TS values on a shared length grid
#'
#' Builds a length-by-model matrix of TS values from the standardized slope and
#' intercept columns.
#'
#' @param models_df Model table.
#' @param lengths_cm Numeric length grid in cm.
#' @param slope_col Slope column name.
#' @param intercept_col Intercept column name.
#' @param id_col Model identifier column name.
#'
#' @return A numeric matrix with one column per model.
#'
#' @export
predict_ts_matrix <- function(models_df,
                              lengths_cm,
                              slope_col = "slope_len",
                              intercept_col = "intercept_len",
                              id_col = "model_id") {
  # Evaluate each model on the shared length grid so downstream ribbon and
  # pivot summaries can work from one aligned TS matrix.
  mat <- vapply(
    seq_len(nrow(models_df)),
    function(i) {
      as.numeric(models_df[[slope_col]][[i]]) * log10(lengths_cm) +
        as.numeric(models_df[[intercept_col]][[i]])
    },
    numeric(length(lengths_cm))
  )

  # Keep the return value matrix-shaped even when only one model was supplied.
  if (is.null(dim(mat))) {
    mat <- matrix(mat, ncol = 1)
  }

  colnames(mat) <- as.character(models_df[[id_col]])
  mat
}

#' Summarize a weighted TS ribbon
#'
#' Combines model-specific TS curves into a weighted mean and normal-approximate
#' uncertainty band on a shared length grid.
#'
#' @param models_df Model table.
#' @param model_weights Numeric weight vector aligned to `models_df`.
#' @param lengths_cm Numeric length grid in cm.
#' @param slope_col Slope column name.
#' @param intercept_col Intercept column name.
#'
#' @return A tibble.
#'
#' @export
summarize_weighted_ts_curve <- function(models_df,
                                        model_weights,
                                        lengths_cm,
                                        slope_col = "slope_len",
                                        intercept_col = "intercept_len") {
  # Restrict the ribbon summary to models with finite coefficients and finite
  # weights before computing the weighted slope/intercept moments.
  valid <- which(
    is.finite(models_df[[slope_col]]) &
      is.finite(models_df[[intercept_col]]) &
      is.finite(model_weights)
  )

  if (length(valid) == 0) {
    return(tibble::tibble(
      length_cm = lengths_cm,
      ts_mean = NA_real_,
      ts_sd = NA_real_,
      ts_lo = NA_real_,
      ts_hi = NA_real_,
      band_method = "weighted_moment_normal_90",
      z_band = 1.645
    ))
  }

  slopes <- as.numeric(models_df[[slope_col]][valid])
  intercepts <- as.numeric(models_df[[intercept_col]][valid])
  w <- as.numeric(model_weights[valid])
  w_sum <- sum(w, na.rm = TRUE)
  if (!is.finite(w_sum) || w_sum <= 0) {
    stop("'model_weights' must sum to a finite positive value.", call. = FALSE)
  }
  w <- w / w_sum

  # Propagate the weighted slope/intercept moments over the log-length grid to
  # obtain the ribbon mean and variance at each length.
  mu_a <- sum(w * slopes)
  mu_b <- sum(w * intercepts)
  var_a <- sum(w * (slopes - mu_a)^2)
  var_b <- sum(w * (intercepts - mu_b)^2)
  cov_ab <- sum(w * (slopes - mu_a) * (intercepts - mu_b))

  x <- log10(lengths_cm)
  ts_mean <- mu_a * x + mu_b
  ts_var <- pmax(var_a * x^2 + 2 * cov_ab * x + var_b, 0)
  ts_sd <- sqrt(ts_var)
  z_band <- 1.645

  tibble::tibble(
    length_cm = lengths_cm,
    ts_mean = ts_mean,
    ts_sd = ts_sd,
    ts_lo = ts_mean - z_band * ts_sd,
    ts_hi = ts_mean + z_band * ts_sd,
    band_method = "weighted_moment_normal_90",
    z_band = z_band
  )
}

#' Compute a pivot profile
#'
#' Identifies the length where weighted between-model TS variance is minimized
#' and summarizes pairwise model intersection lengths over the same grid.
#'
#' @param models_df Model table.
#' @param model_weights Numeric model-weight vector keyed by model ID.
#' @param length_density Length-density tibble with `length_cm`.
#' @param slope_col Slope column name.
#' @param intercept_col Intercept column name.
#' @param id_col Model identifier column name.
#' @param species_col Species label column name.
#'
#' @return A list with `profile`, `pairwise`, and `summary`.
#'
#' @export
compute_pivot_profile <- function(models_df,
                                  model_weights,
                                  length_density,
                                  slope_col = "slope_len",
                                  intercept_col = "intercept_len",
                                  id_col = "model_id",
                                  species_col = "species_name") {
  # Extract the shared length grid and align the model weights to the supplied
  # model rows before any variance or pairwise calculations begin.
  length_grid <- length_density$length_cm
  grid_min <- min(length_grid, na.rm = TRUE)
  grid_max <- max(length_grid, na.rm = TRUE)
  grid_span <- grid_max - grid_min
  w <- as.numeric(model_weights[as.character(models_df[[id_col]])])
  w[!is.finite(w)] <- 0

  if (sum(w) <= 0) {
    stop("No positive model weights available for pivot analysis.", call. = FALSE)
  }

  # Compute the weighted TS mean and between-model variance profile over the
  # shared grid, then identify the minimum-variance pivot length.
  ts_mat <- predict_ts_matrix(
    models_df = models_df,
    lengths_cm = length_grid,
    slope_col = slope_col,
    intercept_col = intercept_col,
    id_col = id_col
  )
  w_norm <- w / sum(w)
  ts_weighted_mean <- as.numeric(ts_mat %*% w_norm)
  ts_centered <- sweep(ts_mat, 1, ts_weighted_mean, FUN = "-")
  variance_v <- rowSums(sweep(ts_centered^2, 2, w_norm, FUN = "*"), na.rm = TRUE)

  pivot_idx <- which.min(variance_v)
  pivot_length_cm <- length_grid[pivot_idx]

  # Derive pairwise intersection lengths so boundary pivots can still be
  # diagnosed with an interior pairwise median and IQR.
  valid_rows <- which(is.finite(models_df[[slope_col]]) & is.finite(models_df[[intercept_col]]))
  pairwise <- purrr::map_dfr(utils::combn(valid_rows, 2, simplify = FALSE), function(idx) {
    i <- idx[[1]]
    j <- idx[[2]]
    m1 <- models_df[[slope_col]][i]
    m2 <- models_df[[slope_col]][j]
    b1 <- models_df[[intercept_col]][i]
    b2 <- models_df[[intercept_col]][j]

    if (!is.finite(m1) || !is.finite(m2) || !is.finite(b1) || !is.finite(b2) || abs(m1 - m2) < 1e-8) {
      return(NULL)
    }

    log10_lpivot <- (b2 - b1) / (m1 - m2)
    lpivot_cm <- 10^log10_lpivot
    if (!is.finite(lpivot_cm) || lpivot_cm <= 0) {
      return(NULL)
    }

    tibble::tibble(
      model_id_1 = models_df[[id_col]][i],
      model_id_2 = models_df[[id_col]][j],
      species_1 = models_df[[species_col]][i],
      species_2 = models_df[[species_col]][j],
      lpivot_cm = lpivot_cm,
      log10_lpivot = log10_lpivot,
      pair_weight = w_norm[i] * w_norm[j]
    )
  })

  pairwise <- pairwise |>
    dplyr::filter(
      is.finite(lpivot_cm),
      is.finite(pair_weight),
      pair_weight > 0
    )

  # Clamp the raw pairwise pivots to a biologically plausible window around
  # the anchor domain before computing their weighted summaries.
  domain_lo <- max(grid_min * 0.5, 1e-6)
  domain_hi <- max(grid_max * 2, domain_lo * 1.01)
  if (nrow(pairwise) > 0) {
    pairwise <- pairwise |>
      dplyr::mutate(
        lpivot_raw_cm = lpivot_cm,
        lpivot_cm = pmin(pmax(lpivot_cm, domain_lo), domain_hi),
        log10_lpivot = log10(lpivot_cm)
      ) |>
      dplyr::filter(is.finite(lpivot_cm), lpivot_cm > 0)
  }

  pairwise_q25 <- NA_real_
  pairwise_q50 <- NA_real_
  pairwise_q75 <- NA_real_
  if (nrow(pairwise) > 0) {
    pairwise_q <- weighted_quantile(
      x = pairwise$lpivot_cm,
      w = pairwise$pair_weight,
      probs = c(0.25, 0.50, 0.75)
    )
    pairwise_q25 <- pairwise_q[[1]]
    pairwise_q50 <- pairwise_q[[2]]
    pairwise_q75 <- pairwise_q[[3]]
  }

  # Mark boundary pivots explicitly and expose the display pivot that should be
  # used in later summaries and plots.
  edge_tol_cm <- max(grid_span * 0.025, 0.5)
  pivot_at_boundary <- isTRUE(
    is.finite(pivot_length_cm) &&
      (pivot_length_cm <= grid_min + edge_tol_cm || pivot_length_cm >= grid_max - edge_tol_cm)
  )
  display_pivot_length_cm <- if (pivot_at_boundary && is.finite(pairwise_q50)) {
    pairwise_q50
  } else {
    pivot_length_cm
  }
  display_pivot_source <- if (pivot_at_boundary && is.finite(pairwise_q50)) {
    "pairwise_weighted_median"
  } else {
    "ensemble_variance_minimum"
  }

  profile <- tibble::tibble(
    length_cm = length_grid,
    ts_weighted_mean = ts_weighted_mean,
    weighted_variance_v = variance_v
  )
  summary <- tibble::tibble(
    pivot_length_cm = pivot_length_cm,
    pivot_log10_length = log10(pivot_length_cm),
    pivot_min_variance_v = variance_v[pivot_idx],
    ts_consensus_at_pivot = ts_weighted_mean[pivot_idx],
    n_pairwise_intersections = nrow(pairwise),
    pairwise_pivot_q25_cm = pairwise_q25,
    pairwise_pivot_q50_cm = pairwise_q50,
    pairwise_pivot_q75_cm = pairwise_q75,
    pivot_at_boundary = pivot_at_boundary,
    pivot_display_length_cm = display_pivot_length_cm,
    pivot_display_source = display_pivot_source
  )

  list(profile = profile, pairwise = pairwise, summary = summary)
}

#' Compute the biological leverage profile
#'
#' Combines the anchor length distribution with the ensemble-mean
#' backscattering cross-section per unit mass so the package can identify the
#' length range where TS mismatch has the largest biomass consequence.
#'
#' @param models_df Model table containing standardized slopes and intercepts.
#' @param model_weights Named or aligned model-weight vector.
#' @param length_density Length-density tibble with `length_cm` and `f_len`.
#' @param pivot_summary Optional pivot summary table.
#' @param slope_col Slope column name.
#' @param intercept_col Intercept column name.
#' @param id_col Model identifier column name.
#' @param length_weight_a_col Length-weight `a` column name.
#' @param length_weight_b_col Length-weight `b` column name.
#'
#' @return A list containing leverage `profile` and `summary` tables.
#'
#' @export
compute_biological_leverage <- function(models_df,
                                        model_weights,
                                        length_density,
                                        pivot_summary = NULL,
                                        slope_col = "slope_len",
                                        intercept_col = "intercept_len",
                                        id_col = "model_id",
                                        length_weight_a_col = "lw_a_g",
                                        length_weight_b_col = "lw_b") {
  # Align the model weights and length grid first so all later TS and biomass
  # calculations operate on one common support.
  length_grid <- length_density$length_cm
  f_len <- length_density$f_len
  w <- as.numeric(model_weights[as.character(models_df[[id_col]])])
  w[!is.finite(w)] <- 0

  if (sum(w) <= 0) {
    stop("No positive model weights available for leverage analysis.", call. = FALSE)
  }

  # Predict TS across the shared length grid, then convert the TS curves to
  # backscattering cross-sections for each candidate model.
  ts_mat <- predict_ts_matrix(
    models_df = models_df,
    lengths_cm = length_grid,
    slope_col = slope_col,
    intercept_col = intercept_col,
    id_col = id_col
  )
  sigma_mat <- 10^(ts_mat / 10)

  # Pull the length-weight parameters when they exist, otherwise fall back to
  # a conservative generic relation so the leverage profile can still be
  # computed for package-native candidate tables that omit those columns.
  lw_a <- if (length_weight_a_col %in% names(models_df)) {
    suppressWarnings(as.numeric(models_df[[length_weight_a_col]]))
  } else {
    rep(NA_real_, nrow(models_df))
  }
  lw_b <- if (length_weight_b_col %in% names(models_df)) {
    suppressWarnings(as.numeric(models_df[[length_weight_b_col]]))
  } else {
    rep(NA_real_, nrow(models_df))
  }
  lw_a[!is.finite(lw_a) | lw_a <= 0] <- 0.01
  lw_b[!is.finite(lw_b) | lw_b <= 0] <- 3.0

  # Standardize each model to sigma_bs per unit mass before averaging across
  # the admissible support set.
  phi_mat <- sapply(seq_len(nrow(models_df)), function(i) {
    sigma_mat[, i] / (lw_a[[i]] * (length_grid^lw_b[[i]]))
  })
  if (is.vector(phi_mat)) {
    phi_mat <- matrix(phi_mat, ncol = 1)
  }

  # Aggregate the per-model leverage kernels and quantify the biomass effect
  # of a one-decibel TS perturbation across length.
  w_norm <- w / sum(w)
  phi_bar <- as.numeric(phi_mat %*% w_norm)
  lambda_l <- phi_bar * f_len
  delta_1db <- lambda_l * (10^(1 / 10) - 1)
  peak_idx <- which.max(lambda_l)

  # Carry the pivot diagnostics forward when the caller provides them so later
  # plots can align leverage peaks against the pivot summaries.
  pivot_length_cm <- if (!is.null(pivot_summary) && "pivot_length_cm" %in% names(pivot_summary)) {
    pivot_summary$pivot_length_cm[[1]]
  } else {
    NA_real_
  }
  pivot_display_length_cm <- if (!is.null(pivot_summary) && "pivot_display_length_cm" %in% names(pivot_summary)) {
    pivot_summary$pivot_display_length_cm[[1]]
  } else {
    pivot_length_cm
  }
  pairwise_q25 <- if (!is.null(pivot_summary) && "pairwise_pivot_q25_cm" %in% names(pivot_summary)) {
    pivot_summary$pairwise_pivot_q25_cm[[1]]
  } else {
    NA_real_
  }
  pairwise_q75 <- if (!is.null(pivot_summary) && "pairwise_pivot_q75_cm" %in% names(pivot_summary)) {
    pivot_summary$pairwise_pivot_q75_cm[[1]]
  } else {
    NA_real_
  }
  pivot_display_source <- if (!is.null(pivot_summary) && "pivot_display_source" %in% names(pivot_summary)) {
    pivot_summary$pivot_display_source[[1]]
  } else {
    "ensemble_variance_minimum"
  }

  profile <- tibble::tibble(
    length_cm = length_grid,
    phi_bar = phi_bar,
    f_len = f_len,
    lambda_l = lambda_l,
    delta_biomass_1db = delta_1db
  )
  summary <- tibble::tibble(
    lambda_bar = sum(lambda_l, na.rm = TRUE),
    peak_length_cm = length_grid[[peak_idx]],
    peak_lambda = lambda_l[[peak_idx]],
    peak_delta_1db = delta_1db[[peak_idx]],
    pivot_length_cm = pivot_length_cm,
    pivot_display_length_cm = pivot_display_length_cm,
    pairwise_pivot_q25_cm = pairwise_q25,
    pairwise_pivot_q75_cm = pairwise_q75,
    pivot_display_source = pivot_display_source,
    pivot_offset_cm = if (is.finite(pivot_length_cm)) length_grid[[peak_idx]] - pivot_length_cm else NA_real_
  )

  list(profile = profile, summary = summary)
}

#' Summarize one evaluation object
#'
#' Computes consensus, weighted quantile, and spread summaries from an anchor
#' evaluation object.
#'
#' @param eval_obj Anchor evaluation object.
#' @param probs Quantile probabilities.
#'
#' @return A one-row tibble.
#'
#' @export
summarize_evaluation <- function(eval_obj,
                                 probs = c(0.05, 0.50, 0.95)) {
  # Return a fully missing one-row summary when no admissible support exists.
  if (is.null(eval_obj) || nrow(eval_obj$admissible_df) == 0) {
    return(tibble::tibble(
      n_admissible = 0L,
      consensus_multiplier = NA_real_,
      multiplier_q05 = NA_real_,
      multiplier_q50 = NA_real_,
      multiplier_q95 = NA_real_,
      log_spread = NA_real_
    ))
  }

  # Compute weighted multiplier summaries from the admissible donor pool.
  q <- weighted_quantile(
    x = eval_obj$admissible_df$biomass_multiplier_if_replace,
    w = eval_obj$admissible_df$w_adm,
    probs = probs
  )

  tibble::tibble(
    n_admissible = nrow(eval_obj$admissible_df),
    consensus_multiplier = sum(
      eval_obj$admissible_df$w_adm * eval_obj$admissible_df$biomass_multiplier_if_replace,
      na.rm = TRUE
    ),
    multiplier_q05 = q[[1]],
    multiplier_q50 = q[[2]],
    multiplier_q95 = q[[3]],
    log_spread = if (is.finite(q[[1]]) && q[[1]] > 0 && is.finite(q[[3]]) && q[[3]] > 0) log(q[[3]] / q[[1]]) else NA_real_
  )
}

#' Summarize anchor missingness mix
#'
#' Computes weighted and unweighted missingness summaries for one admissible
#' anchor donor pool.
#'
#' @param admissible_df Admissible donor table.
#' @param miss_tbl Missingness table.
#' @param id_col Join identifier column.
#' @param miss_col Missingness fraction column.
#'
#' @return A one-row tibble.
#'
#' @export
summarize_missing_mix <- function(admissible_df,
                                  miss_tbl,
                                  id_col = "model_id_chr",
                                  miss_col = "missing_trait_fraction") {
  # Join missingness values onto the admissible donor pool before computing the
  # weighted and unweighted missingness summaries.
  tibble::as_tibble(admissible_df) |>
    dplyr::left_join(
      tibble::as_tibble(miss_tbl) |>
        dplyr::select(dplyr::all_of(id_col), dplyr::all_of(miss_col)),
      by = id_col
    ) |>
    dplyr::summarise(
      weighted_missingness = sum(w_adm * dplyr::coalesce(.data[[miss_col]], 0), na.rm = TRUE),
      mean_missingness = mean(dplyr::coalesce(.data[[miss_col]], 0), na.rm = TRUE),
      .groups = "drop"
    )
}

#' Build anchor-level similarity ablation diagnostics
#'
#' Recomputes each stored anchor admissibility screen after zeroing one active
#' similarity component at a time, then stores the resulting spread, consensus,
#' and admissible-support deltas for downstream `plot(candidates, ...)`
#' diagnostics.
#'
#' @param candidates A [Candidates] object with stored similarity and
#'   admissibility state.
#' @param config Optional config or anchor-config overrides.
#' @param registry_path Optional trait-registry path.
#' @param progress Optional logical scalar.
#'
#' @return An updated [Candidates] object with
#'   `@admissibility$uncertainty_diagnostics`.
#'
#' @keywords internal
build_candidates_uncertainty_diagnostics <- function(candidates,
                                                     config = NULL,
                                                     registry_path = NULL,
                                                     progress = NULL) {
  if (!(inherits(candidates, "S7_object") &&
    exists("Candidates", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(candidates, Candidates), error = function(e) FALSE)))) {
    stop("'candidates' must be a `Candidates` object.", call. = FALSE)
  }
  if (length(candidates@similarity_matrix) == 0) {
    stop(
      "No prepared similarity state is stored on this `Candidates` object. Run `prepare_similarity_matrix()` first.",
      call. = FALSE
    )
  }
  if (length(candidates@admissibility) == 0 || length((candidates@admissibility)$anchors %||% list()) == 0) {
    stop(
      "No anchor admissibility results are stored on this `Candidates` object. Run `screen_admissibility()` first.",
      call. = FALSE
    )
  }

  progress <- progress %||% FALSE
  sim_obj <- candidates@similarity_matrix
  tuning_obj <- candidates@similarity_tuning %||% list()
  cfg_base <- default_anchor_config(config %||% candidates)

  # Start from the operational admissibility configuration. Only the distance
  # hyperparameters fall back to the prepared similarity object because the
  # ablation reruns still need a concrete similarity surface to evaluate the
  # configured admissibility traits.
  cfg_base$species_traits <- as.list(cfg_base$species_traits %||% list())
  cfg_base$study_traits <- as.list(cfg_base$study_traits %||% list())
  cfg_base$alpha <- cfg_base$alpha %||% sim_obj$alpha %||% NULL
  cfg_base$k_species <- cfg_base$k_species %||% sim_obj$k_species %||% NULL
  cfg_base$k_study <- cfg_base$k_study %||% sim_obj$k_study %||% NULL

  component_tbl <- dplyr::bind_rows(
    tibble::tibble(
      component = names(cfg_base$species_traits),
      component_type = "species_trait",
      active_weight = suppressWarnings(as.numeric(unlist(cfg_base$species_traits)))
    ),
    tibble::tibble(
      component = names(cfg_base$study_traits),
      component_type = "study_trait",
      active_weight = suppressWarnings(as.numeric(unlist(cfg_base$study_traits)))
    ),
    tibble::tibble(
      component = c("length_coherence", "depth_coherence", "frequency_coherence"),
      component_type = "coherence",
      active_weight = c(
        suppressWarnings(as.numeric(cfg_base$length_overlap_weight)),
        suppressWarnings(as.numeric(cfg_base$depth_overlap_weight)),
        suppressWarnings(as.numeric(cfg_base$frequency_coherence_weight))
      )
    )
  ) |>
    dplyr::filter(!is.na(component), nzchar(component), is.finite(active_weight), active_weight > 0)

  if (nrow(component_tbl) == 0) {
    stop("No active similarity components were available for anchor ablation.", call. = FALSE)
  }

  global_component_order <- tibble::as_tibble(tuning_obj$component_impact_summary %||% tibble::tibble()) |>
    dplyr::filter(
      component != "full_model",
      !is.na(component),
      nzchar(component)
    ) |>
    dplyr::arrange(dplyr::desc(.data$delta_rmse), component) |>
    dplyr::pull(component)
  global_component_order <- unique(c(global_component_order, component_tbl$component))
  component_rank <- stats::setNames(seq_along(global_component_order), global_component_order)

  candidate_models_scored <- screen_missing_metadata(
    candidate_models = candidates@candidate_models,
    key_cols = admissibility_key_metadata_cols(candidates_config_data(candidates))
  )
  excluded_model_ids <- if ("model_id_chr" %in% names(candidates@reference_anchors)) {
    as.character(candidates@reference_anchors$model_id_chr)
  } else if ("model_id" %in% names(candidates@reference_anchors)) {
    as.character(candidates@reference_anchors$model_id)
  } else {
    character(0)
  }
  excluded_model_ids <- unique(excluded_model_ids[!is.na(excluded_model_ids) & nzchar(excluded_model_ids)])
  points_missing_df <- tibble::as_tibble((candidates@ordination$model$points_missing %||% tibble::tibble()))
  anchor_results <- (candidates@admissibility)$anchors %||% list()

  report_progress(progress, "Building anchor-level similarity ablation diagnostics.")

  context_rows <- list()
  ablation_rows <- list()

  for (anchor_id in names(anchor_results)) {
    anchor_result <- anchor_results[[anchor_id]]
    anchor_row <- tibble::as_tibble(anchor_result$anchor %||% tibble::tibble())
    baseline_eval <- anchor_result$evaluation %||% NULL
    if (nrow(anchor_row) == 0 || is.null(baseline_eval)) {
      next
    }

    anchor_species <- as.character(anchor_row$species_name[[1]] %||% anchor_id)
    report_progress(progress, "Ablating similarity components for anchor '", anchor_species, "'.")

    baseline_summary <- summarize_evaluation(baseline_eval)
    baseline_missing <- if (nrow(points_missing_df) > 0 && nrow(tibble::as_tibble(baseline_eval$admissible_df)) > 0) {
      summarize_missing_mix(
        admissible_df = baseline_eval$admissible_df,
        miss_tbl = points_missing_df,
        id_col = "model_id_chr",
        miss_col = "missing_trait_fraction"
      )
    } else {
      tibble::tibble(
        weighted_missingness = NA_real_,
        mean_missingness = NA_real_
      )
    }

    context_rows[[length(context_rows) + 1L]] <- tibble::tibble(
      anchor_model_id = as.character(anchor_id),
      anchor_species = anchor_species,
      weighted_missingness = baseline_missing$weighted_missingness[[1]],
      mean_missingness = baseline_missing$mean_missingness[[1]],
      n_admissible = baseline_summary$n_admissible[[1]],
      consensus_multiplier = baseline_summary$consensus_multiplier[[1]],
      multiplier_q05 = baseline_summary$multiplier_q05[[1]],
      multiplier_q50 = baseline_summary$multiplier_q50[[1]],
      multiplier_q95 = baseline_summary$multiplier_q95[[1]],
      log_spread = baseline_summary$log_spread[[1]]
    )

    baseline_consensus <- suppressWarnings(as.numeric(baseline_summary$consensus_multiplier[[1]]))
    baseline_n <- suppressWarnings(as.integer(baseline_summary$n_admissible[[1]]))
    baseline_spread <- suppressWarnings(as.numeric(baseline_summary$log_spread[[1]]))

    # Rebuild the anchor screen after dropping each active component so the
    # stored deltas are tied to the operational admissibility pipeline.
    for (j in seq_len(nrow(component_tbl))) {
      component_name <- as.character(component_tbl$component[[j]])
      component_type <- as.character(component_tbl$component_type[[j]])
      cfg_now <- cfg_base

      if (identical(component_type, "species_trait")) {
        cfg_now$species_traits[[component_name]] <- 0
      } else if (identical(component_type, "study_trait")) {
        cfg_now$study_traits[[component_name]] <- 0
      } else if (identical(component_name, "length_coherence")) {
        cfg_now$length_overlap_weight <- 0
      } else if (identical(component_name, "depth_coherence")) {
        cfg_now$depth_overlap_weight <- 0
      } else if (identical(component_name, "frequency_coherence")) {
        cfg_now$frequency_coherence_weight <- 0
      }

      eval_now <- tryCatch(
        screen_one_anchor_admissibility(
          anchor_row = anchor_row,
          candidate_models = candidates@candidate_models,
          config = cfg_now,
          registry_path = registry_path,
          sim_obj = candidates@similarity_matrix,
          dist_obj = candidates@gower_distances,
          candidate_models_scored = candidate_models_scored,
          excluded_model_ids = excluded_model_ids
        ),
        error = function(e) NULL
      )
      summary_now <- summarize_evaluation(eval_now)
      spread_now <- suppressWarnings(as.numeric(summary_now$log_spread[[1]]))
      consensus_now <- suppressWarnings(as.numeric(summary_now$consensus_multiplier[[1]]))
      n_now <- suppressWarnings(as.integer(summary_now$n_admissible[[1]]))

      delta_log_consensus <- if (is.finite(consensus_now) &&
        consensus_now > 0 &&
        is.finite(baseline_consensus) &&
        baseline_consensus > 0) {
        log(consensus_now) - log(baseline_consensus)
      } else {
        NA_real_
      }
      delta_log_spread <- spread_now - baseline_spread
      delta_n_admissible <- n_now - baseline_n

      ablation_rows[[length(ablation_rows) + 1L]] <- tibble::tibble(
        anchor_model_id = as.character(anchor_id),
        anchor_species = anchor_species,
        component = component_name,
        component_type = component_type,
        component_rank_global = component_rank[[component_name]] %||% length(component_rank) + 1L,
        baseline_log_spread = baseline_spread,
        baseline_consensus_multiplier = baseline_consensus,
        baseline_n_admissible = baseline_n,
        ablated_log_spread = spread_now,
        ablated_consensus_multiplier = consensus_now,
        ablated_n_admissible = n_now,
        delta_log_spread = delta_log_spread,
        delta_log_consensus = delta_log_consensus,
        delta_n_admissible = delta_n_admissible,
        importance_score = pmax(delta_log_spread, 0, na.rm = TRUE) +
          0.5 * abs(delta_log_consensus) +
          0.02 * pmax(-delta_n_admissible, 0, na.rm = TRUE)
      )
    }
  }

  anchor_ablation <- dplyr::bind_rows(ablation_rows) |>
    dplyr::arrange(anchor_species, component_rank_global, dplyr::desc(.data$importance_score), component)
  overall_tbl <- anchor_ablation |>
    dplyr::group_by(component, component_type, component_rank_global) |>
    dplyr::summarise(
      importance_score = mean(importance_score, na.rm = TRUE),
      delta_log_spread = mean(delta_log_spread, na.rm = TRUE),
      delta_log_consensus = mean(delta_log_consensus, na.rm = TRUE),
      delta_n_admissible = mean(delta_n_admissible, na.rm = TRUE),
      n_anchors = dplyr::n_distinct(anchor_model_id),
      .groups = "drop"
    ) |>
    dplyr::arrange(component_rank_global, dplyr::desc(.data$importance_score), component)

  report_progress(progress, "Finished anchor-level similarity ablation diagnostics.")

  updated_admissibility <- candidates@admissibility
  updated_admissibility$uncertainty_diagnostics <- list(
    context = dplyr::bind_rows(context_rows),
    anchor_ablation = anchor_ablation,
    overall = overall_tbl
  )
  candidates_with_admissibility(candidates, updated_admissibility)
}

#' Check empirical conformal coverage
#'
#' Computes the empirical fraction of calibration residuals that fall at or
#' below the conformal quantile and compares it against the nominal 1-alpha
#' target. The finite-sample guarantee is marginal coverage >=
#' 1 - alpha - 1/(n+1), so the function also returns that lower bound.
#'
#' @param calibration_residuals Numeric vector of absolute log-backscatter
#'   residuals from the calibration split (|log(sigma_pred / sigma_obs)|).
#' @param q Conformal quantile radius (scalar or per-row numeric vector).
#' @param alpha Miscoverage rate used to compute the nominal 1-alpha target.
#'   Defaults to 0.1 (90 percent nominal coverage).
#'
#' @return A one-row tibble with empirical and nominal coverage, their
#'   difference, and a logical `covered` flag.
#'
#' @export
check_conformal_coverage <- function(calibration_residuals,
                                     q,
                                     alpha = 0.1) {
  residuals <- as.numeric(calibration_residuals)
  residuals <- residuals[is.finite(residuals)]
  n <- length(residuals)
  q_val <- suppressWarnings(as.numeric(q[[1]]))
  nominal <- 1 - as.numeric(alpha)
  finite_sample_floor <- max(0, nominal - 1 / (n + 1))

  if (n == 0 || !is.finite(q_val)) {
    return(tibble::tibble(
      n_calibration = n,
      q = q_val,
      nominal_coverage = nominal,
      finite_sample_floor = finite_sample_floor,
      empirical_coverage = NA_real_,
      coverage_gap = NA_real_,
      covered = NA
    ))
  }

  emp_cov <- mean(residuals <= q_val)
  tibble::tibble(
    n_calibration = n,
    q = q_val,
    nominal_coverage = nominal,
    finite_sample_floor = finite_sample_floor,
    empirical_coverage = emp_cov,
    coverage_gap = emp_cov - nominal,
    covered = emp_cov >= finite_sample_floor
  )
}
