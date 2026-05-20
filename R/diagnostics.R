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
#' backscattering cross-section per unit mass so the workflow can identify the
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

#' Build one uncertainty context row
#'
#' @param anchor_row One-row anchor table.
#' @param base_eval Baseline anchor evaluation object.
#' @param miss_tbl Optional missingness table.
#' @param species_ids Optional character vector of neighborhood model IDs.
#' @param pivot_sum Optional pivot summary table.
#' @param leverage_sum Optional leverage summary table.
#' @param species_col Species label column.
#' @param id_col Model identifier column.
#'
#' @return A one-row tibble.
#'
#' @export
build_uncertainty_context <- function(anchor_row,
                                      base_eval,
                                      miss_tbl = NULL,
                                      species_ids = NULL,
                                      pivot_sum = NULL,
                                      leverage_sum = NULL,
                                      species_col = "species_name",
                                      id_col = "model_id_chr") {
  # Summarize the baseline evaluation first, then layer on missingness,
  # neighborhood, pivot, and leverage context.
  base_stats <- summarize_evaluation(base_eval)
  anchor_species <- as.character(anchor_row[[species_col]][[1]])

  miss_sum <- if (is.null(miss_tbl)) {
    tibble::tibble(weighted_missingness = NA_real_, mean_missingness = NA_real_)
  } else {
    summarize_missing_mix(base_eval$admissible_df, miss_tbl, id_col = id_col)
  }

  nhood_df <- if (is.null(species_ids)) {
    base_eval$admissible_df[0, , drop = FALSE]
  } else {
    tibble::as_tibble(base_eval$admissible_df) |>
      dplyr::filter(.data[[id_col]] %in% species_ids)
  }

  tibble::tibble(
    anchor_species = anchor_species,
    weighted_missingness = miss_sum$weighted_missingness[[1]],
    mean_missingness = miss_sum$mean_missingness[[1]],
    neighborhood_models = nrow(nhood_df),
    neighborhood_species = if ("species_name" %in% names(nhood_df)) dplyr::n_distinct(nhood_df$species_name) else NA_integer_,
    anchor_log_spread = base_stats$log_spread[[1]],
    anchor_n_admissible = base_stats$n_admissible[[1]],
    pivot_length_cm = if (!is.null(pivot_sum) && "pivot_length_cm" %in% names(pivot_sum)) pivot_sum$pivot_length_cm[[1]] else NA_real_,
    pairwise_pivot_iqr_cm = if (!is.null(pivot_sum) && all(c("pairwise_pivot_q75_cm", "pairwise_pivot_q25_cm") %in% names(pivot_sum))) {
      pivot_sum$pairwise_pivot_q75_cm[[1]] - pivot_sum$pairwise_pivot_q25_cm[[1]]
    } else {
      NA_real_
    },
    pivot_at_boundary = if (!is.null(pivot_sum) && "pivot_at_boundary" %in% names(pivot_sum)) pivot_sum$pivot_at_boundary[[1]] else NA,
    leverage_peak_length_cm = if (!is.null(leverage_sum) && "peak_length_cm" %in% names(leverage_sum)) leverage_sum$peak_length_cm[[1]] else NA_real_,
    leverage_pivot_offset_cm = if (!is.null(leverage_sum) && "pivot_offset_cm" %in% names(leverage_sum)) leverage_sum$pivot_offset_cm[[1]] else NA_real_
  )
}

#' Build uncertainty block summaries
#'
#' Compares a baseline evaluation to a named list of block-specific evaluation
#' objects and returns block-level uncertainty deltas.
#'
#' @param anchor_row One-row anchor table.
#' @param base_eval Baseline anchor evaluation object.
#' @param block_evals Named list of evaluation objects by block.
#' @param species_col Species label column.
#'
#' @return A tibble.
#'
#' @export
build_uncertainty_drop <- function(anchor_row,
                                   base_eval,
                                   block_evals,
                                   species_col = "species_name") {
  # Summarize the baseline once, then compare each block-specific evaluation to
  # that fixed baseline.
  base_stats <- summarize_evaluation(base_eval)
  anchor_species <- as.character(anchor_row[[species_col]][[1]])

  purrr::imap_dfr(block_evals, function(eval_obj, block_nm) {
    block_stats <- summarize_evaluation(eval_obj)

    tibble::tibble(
      anchor_species = anchor_species,
      block = block_nm,
      delta_log_spread = block_stats$log_spread[[1]] - base_stats$log_spread[[1]],
      delta_log_consensus = log(block_stats$consensus_multiplier[[1]]) - log(base_stats$consensus_multiplier[[1]]),
      delta_n_admissible = block_stats$n_admissible[[1]] - base_stats$n_admissible[[1]]
    )
  }) |>
    dplyr::mutate(
      importance_score = pmax(delta_log_spread, 0, na.rm = TRUE) +
        0.5 * abs(delta_log_consensus) +
        0.02 * pmax(-delta_n_admissible, 0, na.rm = TRUE)
    ) |>
    dplyr::arrange(dplyr::desc(importance_score), dplyr::desc(delta_log_spread))
}
