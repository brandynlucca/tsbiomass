#' Rank policy specificity
#'
#' Returns an ordinal specificity ranking used to break ties when multiple
#' policies have nearly identical benchmark error.
#'
#' @param policy Character vector of policy names.
#' @param candidate_pool Optional candidate-pool labels used to refine
#'   specificity ties.
#' @param aggregation_method Optional aggregation-method labels used to refine
#'   specificity ties.
#' @param policy_family Optional policy-family labels used to refine
#'   specificity ties.
#' @param equation_branch_filter Optional equation-branch filters used to
#'   refine specificity ties.
#'
#' @return Integer vector.
#'
#' @export
policy_specificity_rank <- function(policy,
                                    candidate_pool = NULL,
                                    aggregation_method = NULL,
                                    policy_family = NULL,
                                    equation_branch_filter = NULL) {
  policy <- stringr::str_to_lower(stringr::str_squish(as.character(policy)))
  candidate_pool <- stringr::str_to_lower(stringr::str_squish(as.character(candidate_pool %||% rep(NA_character_, length(policy)))))
  aggregation_method <- stringr::str_to_lower(stringr::str_squish(as.character(aggregation_method %||% rep(NA_character_, length(policy)))))
  policy_family <- stringr::str_to_lower(stringr::str_squish(as.character(policy_family %||% rep(NA_character_, length(policy)))))
  equation_branch_filter <- stringr::str_to_lower(stringr::str_squish(as.character(equation_branch_filter %||% rep(NA_character_, length(policy)))))

  policy_pool_rank <- dplyr::case_when(
    stringr::str_detect(policy, "same_species|within_species|study_cell_species") ~ 1L,
    stringr::str_detect(policy, "same_genus|within_genus|study_cell_genus|genus_fao|genus_") ~ 2L,
    stringr::str_detect(policy, "same_family|within_family|study_cell_family|family_ocean_basin|family_") ~ 3L,
    stringr::str_detect(policy, "same_order|within_order") ~ 4L,
    stringr::str_detect(policy, "study_cell|within_study_cell|closest_study_cell") ~ 5L,
    stringr::str_detect(policy, "same_fao|within_fao|_fao$") ~ 6L,
    stringr::str_detect(policy, "same_ocean_basin|within_ocean_basin|ocean_basin") ~ 7L,
    stringr::str_detect(policy, "all_admissible|across_all_admissible") ~ 8L,
    TRUE ~ 9L
  )
  candidate_pool_rank <- dplyr::case_when(
    candidate_pool %in% c("same_species", "within_species") ~ 1L,
    candidate_pool %in% c("same_genus", "within_genus") ~ 2L,
    candidate_pool %in% c("same_family", "within_family") ~ 3L,
    candidate_pool %in% c("same_order", "within_order") ~ 4L,
    candidate_pool %in% c("same_study_cell", "within_study_cell", "closest_study_cell") ~ 5L,
    candidate_pool %in% c("same_fao_area", "within_fao", "within_fao_area") ~ 6L,
    candidate_pool %in% c("same_ocean_basin", "within_ocean_basin") ~ 7L,
    candidate_pool %in% c("all_admissible", "across_all_admissible") ~ 8L,
    TRUE ~ 9L
  )
  pool_rank <- pmin(policy_pool_rank, candidate_pool_rank, na.rm = TRUE)
  pool_rank[!is.finite(pool_rank)] <- 9L

  aggregation_method <- dplyr::coalesce(
    aggregation_method,
    dplyr::case_when(
      stringr::str_detect(policy, "closest|distance|nearest") ~ "nearest",
      stringr::str_detect(policy, "weighted_mean") ~ "weighted_mean",
      stringr::str_detect(policy, "unweighted_mean") ~ "unweighted_mean",
      stringr::str_detect(policy, "random_baseline|random_draw") ~ "random_baseline",
      TRUE ~ NA_character_
    )
  )

  aggregation_rank <- dplyr::case_when(
    aggregation_method %in% c(
      "nearest",
      "nearest_by_combined_distance",
      "nearest_by_trait_gower_distance",
      "nearest_by_taxonomic_distance",
      "nearest_by_species_distance",
      "nearest_study_then_model"
    ) ~ 1L,
    aggregation_method %in% c(
      "kernel_weighted_mean",
      "distance_weighted_mean",
      "study_kernel_weighted_mean",
      "weighted_mean"
    ) ~ 2L,
    aggregation_method %in% c(
      "study_equal_weight_mean",
      "unweighted_mean"
    ) ~ 3L,
    aggregation_method %in% c("random_baseline", "random_draw") ~ 4L,
    TRUE ~ 3L
  )
  branch_penalty <- 0L
  family_penalty <- dplyr::case_when(
    policy_family %in% c("single_model", "study_hierarchical_single_model") ~ 0L,
    policy_family %in% c("baseline", "random_baseline") ~ 20L,
    TRUE ~ 5L
  )

  pool_rank * 100L + aggregation_rank * 10L + branch_penalty + family_penalty
}

normalize_uncertainty_rule <- function(x, default = "tolerance") {
  rule <- stringr::str_to_lower(stringr::str_squish(as.character(x %||% default)[[1]] %||% default))
  if (!nzchar(rule) || is.na(rule)) {
    rule <- default
  }
  if (identical(rule, "minimum_width")) {
    rule <- "min"
  }
  if (identical(rule, "within_width_tolerance")) {
    rule <- "tolerance"
  }
  if (identical(rule, "one_standard_error")) {
    rule <- "one_se"
  }
  if (!rule %in% c("min", "tolerance", "one_se")) {
    stop(
      "Selection field 'uncertainty_rule' must be one of: 'min', 'tolerance', 'one_se'.",
      call. = FALSE
    )
  }
  rule
}

uncertainty_width_se <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) < 2L) {
    return(0)
  }
  stats::sd(x, na.rm = TRUE) / sqrt(length(x))
}

conformal_order_statistic <- function(x, level = 0.95) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0L) {
    return(NA_real_)
  }
  k <- min(n, ceiling((n + 1) * level))
  sort(x)[[k]]
}

weighted_species_mean <- function(x,
                                  w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[ok], w = w[ok], na.rm = TRUE)
}

weighted_species_sd <- function(x,
                                w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) < 2L) {
    return(NA_real_)
  }
  x <- x[ok]
  w <- w[ok]
  mu <- stats::weighted.mean(x, w = w, na.rm = TRUE)
  w_sum <- sum(w)
  w_sq_sum <- sum(w^2)
  denom <- w_sum - (w_sq_sum / w_sum)
  if (!is.finite(denom) || denom <= 0) {
    return(NA_real_)
  }
  sqrt(sum(w * (x - mu)^2) / denom)
}

weighted_species_se <- function(x,
                                w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) < 2L) {
    return(NA_real_)
  }
  sd_now <- weighted_species_sd(x[ok], w[ok])
  if (!is.finite(sd_now)) {
    return(NA_real_)
  }
  eff_n <- (sum(w[ok])^2) / sum(w[ok]^2)
  if (!is.finite(eff_n) || eff_n <= 1) {
    return(NA_real_)
  }
  sd_now / sqrt(eff_n)
}

augment_species_block_meta_features <- function(species_performance_table) {
  species_tbl <- standardize_policies(species_performance_table)
  species_tbl$policy <- resolve_policy_names(species_tbl)
  species_tbl <- tibble::as_tibble(species_tbl)
  if (nrow(species_tbl) == 0L ||
      !all(c("policy", "equation_branch_filter", "anchor_species") %in% names(species_tbl))) {
    return(species_tbl)
  }

  if (!"valid_prediction" %in% names(species_tbl)) {
    if ("multiplier_pred" %in% names(species_tbl)) {
      species_tbl$valid_prediction <- is.finite(species_tbl$multiplier_pred) &
        species_tbl$multiplier_pred > 0
    } else {
      species_tbl$valid_prediction <- FALSE
    }
  }
  if (!"error_abs_log" %in% names(species_tbl) && "multiplier_pred" %in% names(species_tbl)) {
    species_tbl$error_abs_log <- dplyr::if_else(
      is.finite(species_tbl$multiplier_pred) & species_tbl$multiplier_pred > 0,
      abs(log(species_tbl$multiplier_pred)),
      NA_real_
    )
  }

  n_total_folds <- dplyr::n_distinct(species_tbl$anchor_species)
  if (!is.finite(n_total_folds) || n_total_folds < 1L) {
    return(species_tbl)
  }

  # Collapse each policy-by-species fold once so the meta-learner can use
  # policy-level fold validity and leave-current-species-out error summaries
  # without rebuilding these quantities during every fit.
  species_fold_tbl <- species_tbl |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log)) |>
    dplyr::group_by(policy, equation_branch_filter, anchor_species) |>
    dplyr::summarise(
      species_median_abs_log = stats::median(error_abs_log, na.rm = TRUE),
      n_anchor_models = dplyr::n(),
      .groups = "drop"
    )
  if (nrow(species_fold_tbl) == 0L) {
    species_tbl$prop_valid_folds <- 0
    species_tbl$n_valid_folds <- 0L
    species_tbl$policy_loco_mean_abs_log <- NA_real_
    species_tbl$policy_loco_sd_abs_log <- NA_real_
    species_tbl$policy_loco_q75_abs_log <- NA_real_
    species_tbl$policy_loco_q90_abs_log <- NA_real_
    return(species_tbl)
  }

  fold_validity_tbl <- species_fold_tbl |>
    dplyr::group_by(policy, equation_branch_filter) |>
    dplyr::summarise(
      n_valid_folds = dplyr::n_distinct(anchor_species),
      prop_valid_folds = n_valid_folds / n_total_folds,
      .groups = "drop"
    )

  loco_tbl <- species_fold_tbl |>
    dplyr::group_by(policy, equation_branch_filter) |>
    dplyr::group_modify(function(.x, .y) {
      n_now <- nrow(.x)
      if (n_now <= 1L) {
        .x$policy_loco_mean_abs_log <- NA_real_
        .x$policy_loco_sd_abs_log <- NA_real_
        .x$policy_loco_q75_abs_log <- NA_real_
        .x$policy_loco_q90_abs_log <- NA_real_
        return(.x)
      }

      x_now <- suppressWarnings(as.numeric(.x$species_median_abs_log))
      w_now <- pmax(1, suppressWarnings(as.numeric(.x$n_anchor_models)))
      sum_wx <- sum(w_now * x_now, na.rm = TRUE)
      sum_w <- sum(w_now, na.rm = TRUE)

      .x$policy_loco_mean_abs_log <- vapply(seq_len(n_now), function(i) {
        denom <- sum_w - w_now[[i]]
        if (!is.finite(denom) || denom <= 0) {
          return(NA_real_)
        }
        (sum_wx - (w_now[[i]] * x_now[[i]])) / denom
      }, numeric(1))
      .x$policy_loco_sd_abs_log <- vapply(seq_len(n_now), function(i) {
        weighted_species_sd(x_now[-i], w_now[-i])
      }, numeric(1))
      .x$policy_loco_q75_abs_log <- vapply(seq_len(n_now), function(i) {
        stats::quantile(x_now[-i], probs = 0.75, na.rm = TRUE, names = FALSE, type = 8)
      }, numeric(1))
      .x$policy_loco_q90_abs_log <- vapply(seq_len(n_now), function(i) {
        stats::quantile(x_now[-i], probs = 0.90, na.rm = TRUE, names = FALSE, type = 8)
      }, numeric(1))
      .x
    }) |>
    dplyr::ungroup() |>
    dplyr::select(
      policy,
      equation_branch_filter,
      anchor_species,
      policy_loco_mean_abs_log,
      policy_loco_sd_abs_log,
      policy_loco_q75_abs_log,
      policy_loco_q90_abs_log
    )

  species_tbl |>
    dplyr::left_join(
      fold_validity_tbl,
      by = c("policy", "equation_branch_filter")
    ) |>
    dplyr::left_join(
      loco_tbl,
      by = c("policy", "equation_branch_filter", "anchor_species")
    ) |>
    dplyr::mutate(
      n_valid_folds = dplyr::coalesce(n_valid_folds, 0L),
      prop_valid_folds = dplyr::coalesce(prop_valid_folds, 0)
    )
}

policy_coefficient_stability_summary <- function(species_performance_table,
                                                 candidate_models,
                                                 level = 0.95) {
  perf_tbl <- standardize_policies(species_performance_table)
  perf_tbl$policy <- resolve_policy_names(perf_tbl)
  perf_tbl <- tibble::as_tibble(perf_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  if (nrow(perf_tbl) == 0 || nrow(candidate_models) == 0) {
    return(tibble::tibble())
  }
  if (!all(c("anchor_model_id", "policy", "equation_branch_filter", "policy_slope_len", "policy_intercept_len") %in% names(perf_tbl))) {
    return(tibble::tibble())
  }

  id_col <- if ("model_id_chr" %in% names(candidate_models)) "model_id_chr" else if ("model_id" %in% names(candidate_models)) "model_id" else NA_character_
  if (is.na(id_col)) {
    return(tibble::tibble())
  }

  candidate_num_or_na <- function(nm) {
    if (!nm %in% names(candidate_models)) {
      return(rep(NA_real_, nrow(candidate_models)))
    }
    suppressWarnings(as.numeric(candidate_models[[nm]]))
  }

  truth_tbl <- candidate_models |>
    dplyr::transmute(
      anchor_model_id = as.character(.data[[id_col]]),
      anchor_slope_len = candidate_num_or_na("slope_len"),
      anchor_intercept_len = candidate_num_or_na("intercept_len")
    ) |>
    dplyr::filter(
      is.finite(anchor_slope_len),
      is.finite(anchor_intercept_len)
    )
  if (nrow(truth_tbl) == 0) {
    return(tibble::tibble())
  }

  perf_tbl |>
    dplyr::transmute(
      anchor_model_id = as.character(anchor_model_id),
      policy = as.character(policy),
      equation_branch_filter = standardize_branches(perf_tbl),
      policy_slope_len = suppressWarnings(as.numeric(policy_slope_len)),
      policy_intercept_len = suppressWarnings(as.numeric(policy_intercept_len))
    ) |>
    dplyr::filter(
      !is.na(policy),
      !is.na(equation_branch_filter),
      is.finite(policy_slope_len),
      is.finite(policy_intercept_len)
    ) |>
    dplyr::group_by(anchor_model_id, policy, equation_branch_filter) |>
    dplyr::summarise(
      policy_slope_len = stats::median(policy_slope_len, na.rm = TRUE),
      policy_intercept_len = stats::median(policy_intercept_len, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::inner_join(truth_tbl, by = "anchor_model_id") |>
    dplyr::mutate(
      coefficient_slope_abs_resid = abs(anchor_slope_len - policy_slope_len),
      coefficient_intercept_abs_resid = abs(anchor_intercept_len - policy_intercept_len)
    ) |>
    dplyr::group_by(policy, equation_branch_filter) |>
    dplyr::summarise(
      coefficient_slope_q95 = conformal_order_statistic(coefficient_slope_abs_resid, level = level),
      coefficient_intercept_q95 = conformal_order_statistic(coefficient_intercept_abs_resid, level = level),
      coefficient_stability_n = dplyr::n(),
      .groups = "drop"
    )
}

uncertainty_width_threshold <- function(min_width,
                                        uncertainty_rule = "tolerance",
                                        u_tol_abs = 0.05,
                                        u_tol_rel = 0.25,
                                        width_se = NA_real_,
                                        one_se_multiplier = 1) {
  uncertainty_rule <- normalize_uncertainty_rule(uncertainty_rule)
  u_tol_abs <- suppressWarnings(as.numeric(u_tol_abs)[[1]])
  u_tol_rel <- suppressWarnings(as.numeric(u_tol_rel)[[1]])
  width_se <- suppressWarnings(as.numeric(width_se)[[1]])
  one_se_multiplier <- suppressWarnings(as.numeric(one_se_multiplier)[[1]])
  if (!is.finite(u_tol_abs)) {
    u_tol_abs <- 0
  }
  if (!is.finite(u_tol_rel)) {
    u_tol_rel <- 0
  }
  if (!is.finite(width_se)) {
    width_se <- 0
  }
  if (!is.finite(one_se_multiplier)) {
    one_se_multiplier <- 1
  }
  min_width <- suppressWarnings(as.numeric(min_width))
  out <- rep(NA_real_, length(min_width))
  finite_idx <- is.finite(min_width)
  if (!any(finite_idx)) {
    return(out)
  }
  if (identical(uncertainty_rule, "min")) {
    out[finite_idx] <- min_width[finite_idx]
    return(out)
  }
  if (identical(uncertainty_rule, "one_se")) {
    out[finite_idx] <- min_width[finite_idx] + pmax(0, one_se_multiplier * width_se)
    return(out)
  }
  out[finite_idx] <- min_width[finite_idx] + pmax(
    u_tol_abs,
    abs(min_width[finite_idx]) * u_tol_rel
  )
  out
}

has_exact_branch_conformal_support <- function(source,
                                               n_scores,
                                               q_value) {
  source <- as.character(source %||% NA_character_)
  n_scores <- suppressWarnings(as.numeric(n_scores))
  q_value <- suppressWarnings(as.numeric(q_value))
  source %in% c("species_policy_branch", "family_policy_branch", "policy_branch") &
    is.finite(q_value) &
    is.finite(n_scores) &
    n_scores >= 2
}

first_non_missing_character <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) {
    return(NA_character_)
  }
  x[[1]]
}

#' Add calibrated interval columns to anchor-policy rows
#'
#' Attaches the calibrated prediction interval to each policy row. The
#' operational interval combines the conformal log-radius (`q_col`) and the
#' donor structural spread (`local_structural_q_abs_log`) in quadrature:
#' `q_total = sqrt(q_conformal^2 + (weight * q_structural)^2)`.
#' Structural spread - the 90th-percentile weighted deviation of donor
#' log-backscatter values from the policy prediction - penalises strategies
#' that rely on heterogeneous donor pools (e.g. ocean-basin means), ensuring
#' that narrower, more homogeneous donor sets are preferred when their
#' conformal error is similar.
#'
#' @param policy_tbl Anchor-policy prediction table.
#' @param structural_uncertainty_weight Multiplicative weight applied to the
#'   structural spread component before combining with the conformal radius.
#'   Default 1 gives equal weight to both sources.
#' @param q_col Name of the conformal log-radius column.
#' @param prediction_col Name of the predicted biomass multiplier column.
#'
#' @return `policy_tbl` with `q_abs_log_conformal`, `q_abs_log_structural`,
#'   `q_abs_log_total`, `multiplier_lo`, `multiplier_hi`, `interval_log_width`,
#'   and `uncertainty_cost_log_width`.
#'
#' @examples
#' interval_tbl <- add_policy_intervals(
#'   tibble::tibble(
#'     policy = "same_species_closest",
#'     multiplier_pred = 1.2,
#'     q_abs_log = 0.15,
#'     local_structural_q_abs_log = 0.05
#'   )
#' )
#' interval_tbl$interval_log_width
#'
#' @export
add_policy_intervals <- function(policy_tbl,
                                 structural_uncertainty_weight = 1,
                                 q_col = "q_abs_log",
                                 prediction_col = "multiplier_pred") {
  out <- tibble::as_tibble(policy_tbl)

  if (!q_col %in% names(out)) {
    out[[q_col]] <- NA_real_
  }
  if (!"local_structural_q_abs_log" %in% names(out)) {
    out$local_structural_q_abs_log <- 0
  }
  if (!"valid_prediction" %in% names(out)) {
    if (!prediction_col %in% names(out)) {
      out$valid_prediction <- FALSE
    } else {
      out$valid_prediction <- is.finite(out[[prediction_col]]) &
        out[[prediction_col]] > 0
    }
  }
  if (!"uncertainty_cost_log_width" %in% names(out)) {
    out$uncertainty_cost_log_width <- NA_real_
  }

  q_conformal <- suppressWarnings(as.numeric(out[[q_col]]))
  q_structural_raw <- if ("local_structural_q_abs_log" %in% names(out)) {
    suppressWarnings(as.numeric(out$local_structural_q_abs_log))
  } else {
    rep(NA_real_, nrow(out))
  }
  # Treat missing structural spread as zero so conformal alone drives the
  # interval when structural information is unavailable.
  q_structural_eff <- dplyr::coalesce(q_structural_raw, 0)
  # RSS combination: structural spread penalises heterogeneous donor pools so
  # that broad ensemble strategies (ocean-basin mean, all-admissible mean) carry
  # wider operational intervals than same-species or same-genus strategies with
  # homogeneous donors.
  q_total <- sqrt(q_conformal^2 + (structural_uncertainty_weight * q_structural_eff)^2)

  out$q_abs_log_conformal  <- q_conformal
  out$q_abs_log_structural <- q_structural_raw
  out$q_abs_log_total      <- q_total
  # Display uses conformal-only q. Structural spread is large for cross-taxa
  # pools and would produce 10,000x display bands that are uninterpretable.
  # q_total (structural + conformal) drives uncertainty_cost_log_width below
  # so that diverse donor pools are correctly penalised in policy selection.
  q_display <- dplyr::coalesce(q_conformal, q_total)
  out$multiplier_lo <- dplyr::if_else(
    out$valid_prediction & is.finite(q_display),
    out[[prediction_col]] * exp(-q_display),
    NA_real_
  )
  out$multiplier_hi <- dplyr::if_else(
    out$valid_prediction & is.finite(q_display),
    out[[prediction_col]] * exp(q_display),
    NA_real_
  )
  out$interval_log_width <- dplyr::if_else(
    is.finite(q_total),
    2 * q_total,
    NA_real_
  )
  out$uncertainty_cost_log_width <- dplyr::if_else(
    is.finite(out$interval_log_width),
    out$interval_log_width,
    out$uncertainty_cost_log_width
  )

  out
}

#' Select anchor-facing policies from calibrated policy predictions
#'
#' @param policy_tbl Anchor-policy prediction table.
#' @param uncertainty_rule Final uncertainty-screening rule. Use `"min"` for
#'   strict minimum calibrated width or `"tolerance"` to retain policies within
#'   an absolute/relative width band around that minimum.
#' @param u_tol_rel Relative width tolerance around the minimum calibrated
#'   interval width after empirical-score screening.
#' @param u_tol_abs Absolute log-width tolerance around the minimum calibrated
#'   interval width after empirical-score screening.
#' @param score_tol_abs Optional absolute benchmark-score tolerance.
#' @param one_se_multiplier Multiplier applied to one-standard-error selection
#'   thresholds.
#' @param local_distance_tolerance Absolute tolerance for retaining local
#'   support-distance ties after empirical-score and uncertainty screening.
#' @param uncertainty_relative_tolerance Legacy alias for `u_tol_rel`.
#' @param uncertainty_absolute_tolerance Legacy alias for `u_tol_abs`.
#'
#' @return A tibble of selected policy rows.
#'
#' @examples
#' selected <- select_anchor_policies(
#'   tibble::tibble(
#'     policy = c("a", "b"),
#'     multiplier_pred = c(1.1, 1.2),
#'     valid_prediction = c(TRUE, TRUE),
#'     selection_valid = c(TRUE, TRUE),
#'     uncertainty_eligible = c(TRUE, TRUE),
#'     uncertainty_cost_log_width = c(0.20, 0.35),
#'     local_weighted_mean_combined_distance = c(0.05, 0.20)
#'   )
#' )
#' selected$selected_policy
#' @export
select_anchor_policies <- function(policy_tbl,
                                   uncertainty_rule = "tolerance",
                                   u_tol_rel = 0.25,
                                   u_tol_abs = 0.05,
                                   score_tol_abs = NULL,
                                   one_se_multiplier = 1,
                                   local_distance_tolerance = 1e-12,
                                   uncertainty_relative_tolerance = NULL,
                                   uncertainty_absolute_tolerance = NULL) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  if (nrow(policy_tbl) == 0) {
    return(policy_tbl)
  }

  uncertainty_rule <- normalize_uncertainty_rule(uncertainty_rule)
  legacy_u_tol_rel <- suppressWarnings(as.numeric(uncertainty_relative_tolerance))
  if (length(legacy_u_tol_rel) >= 1L && is.finite(legacy_u_tol_rel[[1]])) {
    u_tol_rel <- legacy_u_tol_rel[[1]]
  }
  legacy_u_tol_abs <- suppressWarnings(as.numeric(uncertainty_absolute_tolerance))
  if (length(legacy_u_tol_abs) >= 1L && is.finite(legacy_u_tol_abs[[1]])) {
    u_tol_abs <- legacy_u_tol_abs[[1]]
  }
  one_se_multiplier <- suppressWarnings(as.numeric(one_se_multiplier[[1]] %||% NA_real_))
  if (!is.finite(one_se_multiplier)) {
    one_se_multiplier <- 1
  }

  if (!"valid_prediction" %in% names(policy_tbl)) {
    policy_tbl$valid_prediction <- is.finite(policy_tbl$multiplier_pred) &
      policy_tbl$multiplier_pred > 0
  }
  if ("selection_valid" %in% names(policy_tbl)) {
    policy_tbl$selection_valid <- as.logical(policy_tbl$selection_valid)
  } else if ("n_valid_models" %in% names(policy_tbl)) {
    policy_tbl$selection_valid <- is.finite(policy_tbl$n_valid_models) &
      policy_tbl$n_valid_models > 0
  } else if ("n_models" %in% names(policy_tbl)) {
    policy_tbl$selection_valid <- is.finite(policy_tbl$n_models) &
      policy_tbl$n_models > 0
  } else {
    policy_tbl$selection_valid <- policy_tbl$valid_prediction
  }
  # A policy cannot be eligible for selection unless it yields a finite,
  # positive anchor-level multiplier prediction. Donor counts alone are not a
  # valid substitute once the selector reaches the final ranking stage.
  policy_tbl$selection_valid <- dplyr::coalesce(policy_tbl$selection_valid, FALSE) &
    dplyr::coalesce(policy_tbl$valid_prediction, FALSE)
  if (!"uncertainty_eligible" %in% names(policy_tbl)) {
    policy_tbl$uncertainty_eligible <- FALSE
  }
  if (!"uncertainty_cost_log_width" %in% names(policy_tbl)) {
    policy_tbl$uncertainty_cost_log_width <- NA_real_
  }
  if (!"species_block_median_abs_log_error" %in% names(policy_tbl)) {
    policy_tbl$species_block_median_abs_log_error <- NA_real_
  }
  if (!"mean_species_median_abs_log" %in% names(policy_tbl)) {
    policy_tbl$mean_species_median_abs_log <- NA_real_
  }
  if (!"local_weighted_mean_combined_distance" %in% names(policy_tbl)) {
    policy_tbl$local_weighted_mean_combined_distance <- NA_real_
  }
  if (!"local_min_combined_distance" %in% names(policy_tbl)) {
    policy_tbl$local_min_combined_distance <- NA_real_
  }
  if (!"local_effective_support" %in% names(policy_tbl)) {
    policy_tbl$local_effective_support <- NA_real_
  }
  if (!"acceptable_global" %in% names(policy_tbl)) {
    policy_tbl$acceptable_global <- NA
  }
  if (!"equivalent_to_best_global" %in% names(policy_tbl)) {
    policy_tbl$equivalent_to_best_global <- NA
  }
  if (!"bootstrap_median_rank" %in% names(policy_tbl)) {
    policy_tbl$bootstrap_median_rank <- NA_real_
  }
  if (!"best_mean_species_median_abs_log" %in% names(policy_tbl)) {
    policy_tbl$best_mean_species_median_abs_log <- NA_real_
  }
  if (!"coefficient_slope_q95" %in% names(policy_tbl)) {
    policy_tbl$coefficient_slope_q95 <- NA_real_
  }
  if (!"coefficient_intercept_q95" %in% names(policy_tbl)) {
    policy_tbl$coefficient_intercept_q95 <- NA_real_
  }
  if (!"candidate_slope_interval_width" %in% names(policy_tbl)) {
    if (all(c("policy_slope_len_lo_95", "policy_slope_len_hi_95") %in% names(policy_tbl))) {
      policy_tbl$candidate_slope_interval_width <- suppressWarnings(
        as.numeric(policy_tbl$policy_slope_len_hi_95) -
          as.numeric(policy_tbl$policy_slope_len_lo_95)
      )
    } else {
      policy_tbl$candidate_slope_interval_width <- NA_real_
    }
  }
  if (!"candidate_intercept_interval_width" %in% names(policy_tbl)) {
    if (all(c("policy_intercept_len_lo_95", "policy_intercept_len_hi_95") %in% names(policy_tbl))) {
      policy_tbl$candidate_intercept_interval_width <- suppressWarnings(
        as.numeric(policy_tbl$policy_intercept_len_hi_95) -
          as.numeric(policy_tbl$policy_intercept_len_lo_95)
      )
    } else {
      policy_tbl$candidate_intercept_interval_width <- NA_real_
    }
  }
  if (!"candidate_ts_q95_mean" %in% names(policy_tbl)) {
    policy_tbl$candidate_ts_q95_mean <- NA_real_
  }
  if (!"candidate_ts_q99_mean" %in% names(policy_tbl)) {
    policy_tbl$candidate_ts_q99_mean <- NA_real_
  }
  if (!"candidate_ts_q95_max" %in% names(policy_tbl)) {
    policy_tbl$candidate_ts_q95_max <- NA_real_
  }
  if (!"candidate_ts_q99_max" %in% names(policy_tbl)) {
    policy_tbl$candidate_ts_q99_max <- NA_real_
  }
  if (!"candidate_ts_hi99_max" %in% names(policy_tbl)) {
    policy_tbl$candidate_ts_hi99_max <- NA_real_
  }
  if (!"candidate_ts_lo99_min" %in% names(policy_tbl)) {
    policy_tbl$candidate_ts_lo99_min <- NA_real_
  }
  candidate_pool_values <- if ("candidate_pool" %in% names(policy_tbl)) {
    policy_tbl$candidate_pool
  } else {
    NULL
  }
  aggregation_values <- if ("aggregation_method" %in% names(policy_tbl)) {
    policy_tbl$aggregation_method
  } else {
    NULL
  }
  policy_family_values <- if ("policy_family" %in% names(policy_tbl)) {
    policy_tbl$policy_family
  } else {
    NULL
  }
  branch_values <- if ("equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$equation_branch_filter
  } else {
    NULL
  }
  policy_tbl$specificity_rank <- policy_specificity_rank(
    policy = policy_tbl$policy,
    candidate_pool = candidate_pool_values,
    aggregation_method = aggregation_values,
    policy_family = policy_family_values,
    equation_branch_filter = branch_values
  )
  local_distance <- dplyr::coalesce(
    policy_tbl$local_weighted_mean_combined_distance,
    policy_tbl$local_min_combined_distance
  )
  uses_meta_score <- ".meta_predicted_score" %in% names(policy_tbl) &&
    any(is.finite(suppressWarnings(as.numeric(policy_tbl$.meta_predicted_score))), na.rm = TRUE)
  benchmark_validation_error <- dplyr::coalesce(
    policy_tbl$species_block_median_abs_log_error,
    policy_tbl$mean_species_median_abs_log
  )
  validation_error <- if (uses_meta_score) {
    suppressWarnings(as.numeric(policy_tbl$.meta_predicted_score))
  } else {
    benchmark_validation_error
  }
  policy_tbl$anchor_selection_local_distance <- local_distance
  policy_tbl$anchor_selection_benchmark_error <- benchmark_validation_error
  policy_tbl$anchor_selection_validation_error <- validation_error
  policy_tbl$anchor_selection_global_screen <- FALSE
  policy_tbl$direct_branch_conformal_support <- has_exact_branch_conformal_support(
    source = if ("meta_q_abs_log_source" %in% names(policy_tbl)) policy_tbl$meta_q_abs_log_source else NA_character_,
    n_scores = if ("meta_q_abs_log_n_scores" %in% names(policy_tbl)) policy_tbl$meta_q_abs_log_n_scores else NA_real_,
    q_value = if ("meta_q_abs_log" %in% names(policy_tbl)) policy_tbl$meta_q_abs_log else NA_real_
  )

  eligible <- policy_tbl |>
    dplyr::filter(
      selection_valid,
      dplyr::coalesce(uncertainty_eligible, FALSE),
      is.finite(uncertainty_cost_log_width)
    )

  calibrated <- policy_tbl |>
    dplyr::filter(selection_valid, is.finite(uncertainty_cost_log_width))

  if (nrow(eligible) > 0) {
    candidate_tbl <- eligible
    tier <- "benchmark_screened_calibrated_uncertainty"
  } else if (nrow(calibrated) > 0) {
    candidate_tbl <- calibrated
    tier <- "benchmark_screened_below_coverage_target"
  } else {
    selected <- policy_tbl |>
      dplyr::filter(selection_valid) |>
      dplyr::arrange(
        anchor_selection_validation_error,
        bootstrap_median_rank,
        policy
      ) |>
      dplyr::slice(1) |>
      dplyr::mutate(
        selected_policy = policy,
        selected_policy_display = policy,
        selection_tier = "fallback_valid_empirical_score",
        anchor_selection_min_uncertainty_width = NA_real_,
        anchor_selection_uncertainty_threshold = NA_real_,
        anchor_selection_min_validation_error = suppressWarnings(
          min(anchor_selection_validation_error, na.rm = TRUE)
        ),
        anchor_selection_validation_threshold = NA_real_
      )
    return(selected)
  }

  # Respect the precomputed global benchmark acceptability screen whenever it
  # When the meta-learner provides anchor-specific scores, the global
  # acceptability screen is redundant: the meta-score already incorporates
  # cross-species benchmark signal, and applying a global pre-filter here
  # eliminates anchor-optimal policies that the meta-learner correctly prefers.
  # Mirror the same pattern used for the benchmark_one_se gate below.
  if (!uses_meta_score &&
      "acceptable_global" %in% names(candidate_tbl) &&
      any(!is.na(candidate_tbl$acceptable_global))) {
    globally_acceptable <- candidate_tbl |>
      dplyr::filter(dplyr::coalesce(acceptable_global, FALSE))
    if (nrow(globally_acceptable) > 0) {
      candidate_tbl <- globally_acceptable
      tier <- paste0(tier, "_acceptable_global")
    }
  }

  benchmark_ranked <- candidate_tbl |>
    dplyr::filter(is.finite(anchor_selection_benchmark_error))
  # In the meta-policy path, `.meta_predicted_score` is already the learned
  # anchor-specific ranking target. Re-applying an anchor-level benchmark
  # one-SE gate on the global species-block summary can override the learned
  # local ranking and force broad, globally average policies to survive over
  # tighter local matches. Keep this secondary benchmark gate only for the
  # deterministic path where no learned local score is available.
  if (!uses_meta_score && nrow(benchmark_ranked) > 0) {
    benchmark_min <- suppressWarnings(min(benchmark_ranked$anchor_selection_benchmark_error, na.rm = TRUE))
    benchmark_global_best <- suppressWarnings(
      min(policy_tbl$best_mean_species_median_abs_log, na.rm = TRUE)
    )
    benchmark_global_threshold <- suppressWarnings(min(policy_tbl$one_se_threshold, na.rm = TRUE))
    benchmark_one_se_slack <- if (is.finite(benchmark_global_best) &&
      is.finite(benchmark_global_threshold)) {
      pmax(0, benchmark_global_threshold - benchmark_global_best)
    } else {
      0
    }
    benchmark_threshold <- benchmark_min + benchmark_one_se_slack

    candidate_tbl$anchor_selection_global_screen <-
      is.finite(candidate_tbl$anchor_selection_benchmark_error) &
      candidate_tbl$anchor_selection_benchmark_error <= benchmark_threshold

    globally_screened <- candidate_tbl |>
      dplyr::filter(anchor_selection_global_screen)
    if (nrow(globally_screened) > 0) {
      candidate_tbl <- globally_screened
      tier <- paste0(tier, "_benchmark_one_se")
    }
  }

  score_ranked <- candidate_tbl |>
    dplyr::filter(is.finite(anchor_selection_validation_error))
  if (nrow(score_ranked) == 0) {
    score_ranked <- candidate_tbl
  }

  min_validation_error <- suppressWarnings(
    min(score_ranked$anchor_selection_validation_error, na.rm = TRUE)
  )
  if (!is.finite(min_validation_error)) {
    min_validation_error <- NA_real_
  }
  score_tol_abs <- suppressWarnings(as.numeric(score_tol_abs[[1]] %||% NA_real_))
  benchmark_score_slack <- suppressWarnings(
    min(
      score_ranked$one_se_threshold - score_ranked$best_mean_species_median_abs_log,
      na.rm = TRUE
    )
  )
  if (!is.finite(benchmark_score_slack)) {
    benchmark_score_slack <- 0
  }
  # In the meta-policy path, the proper one-standard-error width comes from
  # the leave-one-species-out benchmark that produced
  # `best_mean_species_median_abs_log` and `one_se_threshold`. Reuse that
  # benchmark slack on the meta-score scale instead of estimating a pseudo-SE
  # from the spread across competing policies for this anchor.
  score_threshold <- if (is.finite(min_validation_error)) {
    if (is.finite(score_tol_abs)) {
      min_validation_error + pmax(0, score_tol_abs)
    } else if (uses_meta_score) {
      min_validation_error + pmax(0, one_se_multiplier * benchmark_score_slack)
    } else {
      min_validation_error + pmax(
        0,
        one_se_multiplier * uncertainty_width_se(score_ranked$anchor_selection_validation_error)
      )
    }
  } else {
    NA_real_
  }

  if (is.finite(min_validation_error)) {
    best_score_rows <- score_ranked |>
      dplyr::filter(
        anchor_selection_validation_error <= score_threshold
      )
  } else {
    best_score_rows <- score_ranked
  }

  selected <- best_score_rows

  min_width <- suppressWarnings(min(selected$uncertainty_cost_log_width, na.rm = TRUE))
  if (!is.finite(min_width)) {
    return(candidate_tbl[0, , drop = FALSE])
  }

  selected <- selected |>
    dplyr::arrange(
      anchor_selection_validation_error,
      uncertainty_cost_log_width,
      coefficient_slope_q95,
      coefficient_intercept_q95,
      anchor_selection_local_distance,
      specificity_rank,
      bootstrap_median_rank,
      policy
    )

  selected |>
    dplyr::slice(1) |>
    dplyr::mutate(
      selected_policy = policy,
      selected_policy_display = policy,
      selection_tier = tier,
      anchor_selection_min_validation_error = min_validation_error,
      anchor_selection_validation_threshold = score_threshold,
      anchor_selection_min_uncertainty_width = min_width,
      anchor_selection_uncertainty_threshold = min_width
    )
}

#' Summarize one species-block benchmark table
#'
#' Collapses the leave-one-species-out benchmark rows to one species-level
#' summary per policy.
#'
#' @param species_performance_table Species-block benchmark table.
#'
#' @return A tibble.
#'
#' @keywords internal
species_performance <- function(species_performance_table) {
  # Restrict the summary to finite valid predictions before collapsing anchor
  # rows to one species-level error summary per policy.
  {
    species_tbl <- standardize_policies(species_performance_table)
    species_tbl$policy <- resolve_policy_names(species_tbl)
    if (!"valid_prediction" %in% names(species_tbl)) {
      if ("multiplier_pred" %in% names(species_tbl)) {
        species_tbl$valid_prediction <- is.finite(species_tbl$multiplier_pred) &
          species_tbl$multiplier_pred > 0
      } else {
        species_tbl$valid_prediction <- FALSE
      }
    }
    if (!"error_abs_log" %in% names(species_tbl) && "multiplier_pred" %in% names(species_tbl)) {
      species_tbl$error_abs_log <- dplyr::if_else(
        is.finite(species_tbl$multiplier_pred) & species_tbl$multiplier_pred > 0,
        abs(log(species_tbl$multiplier_pred)),
        NA_real_
      )
    }
    species_tbl
  } |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log)) |>
    dplyr::group_by(policy, equation_branch_filter, anchor_species) |>
    dplyr::summarise(
      species_median_abs_log = stats::median(error_abs_log, na.rm = TRUE),
      species_mean_abs_log = mean(error_abs_log, na.rm = TRUE),
      n_anchor_models = dplyr::n(),
      candidate_pool = if ("candidate_pool" %in% names(species_tbl)) {
        first_non_missing_character(candidate_pool)
      } else {
        NA_character_
      },
      aggregation_method = if ("aggregation_method" %in% names(species_tbl)) {
        first_non_missing_character(aggregation_method)
      } else {
        NA_character_
      },
      policy_family = if ("policy_family" %in% names(species_tbl)) {
        first_non_missing_character(policy_family)
      } else {
        NA_character_
      },
      .groups = "drop"
    )
}

#' Build the policy-selection reference table
#'
#' Converts the leave-one-species-out benchmark results to a global policy
#' comparison table using a one-standard-error rule plus bootstrap stability.
#'
#' @param species_performance_table Species-block benchmark table.
#' @param candidate_models Optional candidate-model metadata used to enrich
#'   policy labels.
#' @param one_se_multiplier Multiplier applied to one-standard-error
#'   thresholds.
#' @param equivalence_tolerance Practical equivalence tolerance used by paired
#'   comparisons.
#' @param n_boot Number of bootstrap resamples across species.
#' @param seed Integer bootstrap seed.
#' @param progress Logical scalar controlling stage messages.
#'
#' @return A tibble.
#'
#' @export
build_selection_table <- function(species_performance_table,
                                  candidate_models = NULL,
                                  one_se_multiplier = 1,
                                  equivalence_tolerance = 0.05,
                                  n_boot = 500L,
                                  seed = NULL,
                                  progress = FALSE) {
  # Summarize the benchmark at the species level first so the selection rule
  # aligns with the species-block validation design.
  species_level <- species_performance(species_performance_table)
  if (nrow(species_level) == 0) {
    return(tibble::tibble())
  }

  # Build the global per-policy summary that the later one-SE and bootstrap
  # rules operate on.
  select_ref <- species_level |>
    dplyr::group_by(policy, equation_branch_filter) |>
    dplyr::summarise(
      n_species = dplyr::n(),
      total_anchor_models = sum(n_anchor_models, na.rm = TRUE),
      median_species_median_abs_log = stats::median(species_median_abs_log, na.rm = TRUE),
      mean_species_median_abs_log = weighted_species_mean(species_median_abs_log, n_anchor_models),
      sd_species_median_abs_log = weighted_species_sd(species_median_abs_log, n_anchor_models),
      se_species_median_abs_log = weighted_species_se(species_median_abs_log, n_anchor_models),
      unweighted_mean_species_median_abs_log = mean(species_median_abs_log, na.rm = TRUE),
      unweighted_sd_species_median_abs_log = stats::sd(species_median_abs_log, na.rm = TRUE),
      unweighted_se_species_median_abs_log = dplyr::if_else(
        n_species > 1,
        unweighted_sd_species_median_abs_log / sqrt(n_species),
        NA_real_
      ),
      candidate_pool = first_non_missing_character(candidate_pool),
      aggregation_method = first_non_missing_character(aggregation_method),
      policy_family = first_non_missing_character(policy_family),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      specificity_rank = policy_specificity_rank(
        policy = policy,
        candidate_pool = candidate_pool,
        aggregation_method = aggregation_method,
        policy_family = policy_family,
        equation_branch_filter = equation_branch_filter
      ),
      policy_display = policy_display_label(policy, equation_branch_filter)
    )

  coefficient_stability <- if (!is.null(candidate_models)) {
    policy_coefficient_stability_summary(
      species_performance_table = species_performance_table,
      candidate_models = candidate_models,
      level = 0.95
    )
  } else {
    tibble::tibble()
  }
  if (nrow(coefficient_stability) > 0) {
    select_ref <- select_ref |>
      dplyr::left_join(
        coefficient_stability,
        by = c("policy", "equation_branch_filter")
      )
  } else {
    select_ref <- select_ref |>
      dplyr::mutate(
        coefficient_slope_q95 = NA_real_,
        coefficient_intercept_q95 = NA_real_,
        coefficient_stability_n = NA_real_
      )
  }

  # Identify the best current policy and the one-SE acceptability threshold
  # before the bootstrap summaries are computed.
  best_row <- select_ref |>
    dplyr::arrange(
      mean_species_median_abs_log,
      median_species_median_abs_log,
      specificity_rank,
      policy,
      equation_branch_filter
    ) |>
    dplyr::slice(1)
  if (is.na(best_row$se_species_median_abs_log[[1]])) {
    warning(
      "Only 1 anchor species in the benchmark; SE is NA and the one-SE acceptance ",
      "threshold collapses to the best mean error. The equivalence band may be too ",
      "narrow. Consider adding more anchor species to the benchmark.",
      call. = FALSE
    )
  }
  threshold <- best_row$mean_species_median_abs_log[[1]] +
    as.numeric(one_se_multiplier) * dplyr::coalesce(best_row$se_species_median_abs_log[[1]], 0)

  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }
  set.seed(as.integer(seed))

  n_policies_sel <- nrow(dplyr::distinct(species_level, policy, equation_branch_filter))
  n_species_sel  <- nrow(dplyr::distinct(species_level, anchor_species))
  report_progress(
    progress,
    "[Selection] Bootstrapping ", as.integer(n_boot),
    " resamples across ", n_policies_sel, " policies and ", n_species_sel, " species..."
  )

  # Pivot species_level to a wide matrix (species rows x policy columns) once
  # so all bootstrap resamples are vectorized across policies simultaneously.
  species_level_keyed <- dplyr::mutate(
    species_level,
    .policy_key = paste(
      as.character(policy),
      normalize_policy_equation_branch_filters(equation_branch_filter),
      sep = "|"
    )
  )
  key_lookup <- dplyr::distinct(
    species_level_keyed,
    .policy_key,
    policy,
    equation_branch_filter,
    candidate_pool,
    aggregation_method,
    policy_family
  )
  species_wide <- tidyr::pivot_wider(
    species_level_keyed,
    id_cols = anchor_species,
    names_from = .policy_key,
    values_from = species_median_abs_log
  )
  policy_key_order <- setdiff(names(species_wide), "anchor_species")
  policy_mat   <- as.matrix(species_wide[, policy_key_order, drop = FALSE])
  n_spp_mat    <- nrow(policy_mat)
  n_boot_int   <- as.integer(n_boot)

  # Single bulk sample.int call produces all bootstrap indices; colMeans over
  # the resampled matrix gives bootstrap means for every policy in one pass.
  boot_idx      <- matrix(sample.int(n_spp_mat, n_spp_mat * n_boot_int, replace = TRUE), nrow = n_spp_mat)
  boot_means_mat <- apply(boot_idx, 2, function(idx) {
    colMeans(policy_mat[idx, , drop = FALSE], na.rm = TRUE)
  })
  rownames(boot_means_mat) <- policy_key_order

  # boot_sum: per-policy quantiles and threshold probability from matrix rows.
  boot_sum <- dplyr::left_join(
    tibble::tibble(.policy_key = policy_key_order),
    key_lookup, by = ".policy_key"
  ) |>
    dplyr::mutate(
      bootstrap_mean_q05 = apply(
        boot_means_mat, 1, stats::quantile, probs = 0.05, na.rm = TRUE, names = FALSE, type = 8
      ),
      bootstrap_mean_q50 = apply(
        boot_means_mat, 1, stats::quantile, probs = 0.50, na.rm = TRUE, names = FALSE, type = 8
      ),
      bootstrap_mean_q95 = apply(
        boot_means_mat, 1, stats::quantile, probs = 0.95, na.rm = TRUE, names = FALSE, type = 8
      ),
      bootstrap_prob_within_threshold = rowMeans(boot_means_mat <= threshold, na.rm = TRUE)
    ) |>
    dplyr::select(-.policy_key)

  # boot_rank: rank policies within each bootstrap draw using a tiny numeric
  # secondary key to break ties by specificity_rank (matches original ordering).
  spec_ranks <- dplyr::left_join(
    tibble::tibble(.policy_key = policy_key_order),
    key_lookup,
    by = ".policy_key"
  ) |>
    dplyr::mutate(
      specificity_rank = policy_specificity_rank(
        policy = policy,
        candidate_pool = candidate_pool,
        aggregation_method = aggregation_method,
        policy_family = policy_family,
        equation_branch_filter = equation_branch_filter
      )
    ) |>
    dplyr::pull(specificity_rank)
  secondary_key <- (spec_ranks / (max(spec_ranks, na.rm = TRUE) + 1L)) * 1e-10
  rank_mat      <- apply(boot_means_mat + secondary_key, 2, rank, ties.method = "first")
  boot_rank <- dplyr::left_join(
    tibble::tibble(.policy_key = policy_key_order),
    key_lookup, by = ".policy_key"
  ) |>
    dplyr::mutate(
      bootstrap_prob_best   = rowMeans(rank_mat == 1L, na.rm = TRUE),
      bootstrap_median_rank = apply(rank_mat, 1, stats::median, na.rm = TRUE)
    ) |>
    dplyr::select(-.policy_key)

  # Merge the bootstrap diagnostics back onto the policy summary and record
  # the final acceptability calls.
  select_ref |>
    dplyr::left_join(boot_sum, by = c("policy", "equation_branch_filter")) |>
    dplyr::left_join(boot_rank, by = c("policy", "equation_branch_filter")) |>
    dplyr::mutate(
      best_policy_global = best_row$policy[[1]],
      best_equation_branch_filter_global = best_row$equation_branch_filter[[1]],
      best_mean_species_median_abs_log = best_row$mean_species_median_abs_log[[1]],
      one_se_multiplier = as.numeric(one_se_multiplier),
      one_se_threshold = threshold,
      acceptable_one_se = mean_species_median_abs_log <= threshold,
      acceptable_bootstrap = dplyr::coalesce(bootstrap_prob_within_threshold, 0) >= 0.50,
      acceptable_global = acceptable_one_se & acceptable_bootstrap,
      equivalence_tolerance = as.numeric(equivalence_tolerance)
    ) |>
    dplyr::arrange(
      dplyr::desc(acceptable_global),
      dplyr::desc(acceptable_one_se),
      mean_species_median_abs_log,
      specificity_rank,
      policy,
      equation_branch_filter
    )
}

#' Build pairwise policy-equivalence summaries
#'
#' Compares paired species-block benchmark errors and records whether each
#' policy pair is practically and statistically indistinguishable.
#'
#' @param species_performance_table Species-block benchmark table.
#' @param select_ref Global policy-selection reference table.
#' @param tolerance Practical equivalence tolerance on mean paired error
#'   difference.
#' @param n_boot Number of paired bootstrap resamples across species.
#' @param seed Integer bootstrap seed.
#' @param progress Logical scalar controlling stage messages.
#'
#' @return A list with `pairs` and `best_flags`.
#'
#' @export
build_equivalence_table <- function(species_performance_table,
                                    select_ref,
                                    tolerance = 0.05,
                                    n_boot = 500L,
                                    seed = NULL,
                                    progress = FALSE) {
  # Reuse the species-level benchmark summary so equivalence is assessed on the
  # same validation scale as the global selection table.
  species_level <- species_performance(species_performance_table) |>
    dplyr::select(policy, equation_branch_filter, anchor_species, species_median_abs_log)

  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }

  if (nrow(species_level) == 0 || nrow(select_ref) == 0) {
    return(list(
      pairs = tibble::tibble(),
      best_flags = tibble::tibble(
        policy = character(0),
        equation_branch_filter = character(0),
        equivalent_to_best_global = logical(0),
        paired_mean_diff_to_best = numeric(0)
      )
    ))
  }

  policy_nodes <- species_level |>
    dplyr::distinct(policy, equation_branch_filter) |>
    dplyr::arrange(policy, equation_branch_filter)
  policy_keys <- paste(
    as.character(policy_nodes$policy),
    normalize_policy_equation_branch_filters(policy_nodes$equation_branch_filter),
    sep = "|"
  )

  # Pre-pivot species_level to a wide matrix once so each pair extracts two
  # columns directly instead of re-filtering and pivoting the full table.
  eq_wide_tbl <- species_level |>
    dplyr::mutate(
      .policy_key = paste(
        as.character(policy),
        normalize_policy_equation_branch_filters(equation_branch_filter),
        sep = "|"
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = anchor_species,
      names_from = .policy_key,
      values_from = species_median_abs_log
    )
  eq_wide_mat  <- as.matrix(eq_wide_tbl[, setdiff(names(eq_wide_tbl), "anchor_species"), drop = FALSE])
  n_boot_int_eq <- as.integer(n_boot)

  if (nrow(policy_nodes) < 2) {
    return(list(
      pairs = tibble::tibble(),
      best_flags = policy_nodes |>
        dplyr::mutate(
          equivalent_to_best_global = TRUE,
          paired_mean_diff_to_best = 0
        )
    ))
  }

  # Identify the best current policy first so equivalence-to-best can be
  # derived directly from the pairwise comparison table later.
  best_policy <-
    {
      select_ref_tbl <- standardize_policies(select_ref)
      select_ref_tbl$policy <- resolve_policy_names(select_ref_tbl)
      select_ref_tbl
    } |>
    dplyr::arrange(mean_species_median_abs_log, specificity_rank, policy, equation_branch_filter) |>
    dplyr::slice(1)
  best_key <- paste(
    as.character(best_policy$policy[[1]]),
    normalize_policy_equation_branch_filters(best_policy$equation_branch_filter[[1]]),
    sep = "|"
  )

  n_pairs <- choose(length(policy_keys), 2L)
  report_progress(
    progress,
    "[Selection] Computing pairwise equivalence across ", n_pairs, " policy pairs (",
    length(policy_keys), " policies, ", as.integer(n_boot), " bootstrap resamples each)..."
  )

  pair_tbl <- utils::combn(policy_keys, 2, simplify = FALSE) |>
    purrr::map_dfr(function(pair) {
      lhs_key <- pair[[1]]
      rhs_key <- pair[[2]]
      lhs_idx <- match(lhs_key, policy_keys)
      rhs_idx <- match(rhs_key, policy_keys)
      lhs <- policy_nodes$policy[[lhs_idx]]
      lhs_branch <- policy_nodes$equation_branch_filter[[lhs_idx]]
      rhs <- policy_nodes$policy[[rhs_idx]]
      rhs_branch <- policy_nodes$equation_branch_filter[[rhs_idx]]

      # Look up the two policy columns directly from the pre-pivoted matrix.
      lhs_col     <- eq_wide_mat[, lhs_key]
      rhs_col     <- eq_wide_mat[, rhs_key]
      both_finite <- is.finite(lhs_col) & is.finite(rhs_col)
      n_common    <- sum(both_finite)

      if (n_common == 0L) {
        return(tibble::tibble(
          policy_a = lhs,
          equation_branch_filter_a = lhs_branch,
          policy_b = rhs,
          equation_branch_filter_b = rhs_branch,
          n_species_common = 0L,
          paired_mean_diff = NA_real_,
          paired_median_diff = NA_real_,
          paired_boot_q025 = NA_real_,
          paired_boot_q975 = NA_real_,
          equivalent_pair = FALSE,
          pair_decision = "inconclusive",
          better_policy = NA_character_
        ))
      }

      # Vectorized bootstrap: single sample.int + matrix colMeans replaces
      # replicate(n_boot, { sample + mean }) for a ~20-50x speedup per pair.
      diff_vec <- (lhs_col - rhs_col)[both_finite]
      set.seed(as.integer(seed) + sum(utf8ToInt(paste(lhs_key, rhs_key, sep = "|"))))
      n_d          <- length(diff_vec)
      boot_idx_eq  <- matrix(sample.int(n_d, n_d * n_boot_int_eq, replace = TRUE), nrow = n_d)
      boot_means   <- colMeans(matrix(diff_vec[boot_idx_eq], nrow = n_d), na.rm = TRUE)

      q025 <- stats::quantile(boot_means, probs = 0.025, na.rm = TRUE, names = FALSE, type = 8)
      q975 <- stats::quantile(boot_means, probs = 0.975, na.rm = TRUE, names = FALSE, type = 8)
      mean_diff <- mean(diff_vec, na.rm = TRUE)
      med_diff <- stats::median(diff_vec, na.rm = TRUE)

      # Treat overlap with the tolerance boundary as inconclusive. A pair is
      # equivalent only when the full bootstrap interval lies inside the
      # practical-equivalence band; a policy is better only when the full
      # interval lies beyond that band on one side.
      eq_pair <- is.finite(q025) &&
        is.finite(q975) &&
        q025 >= -tolerance &&
        q975 <= tolerance
      lhs_better <- is.finite(q975) &&
        q975 < -tolerance
      rhs_better <- is.finite(q025) &&
        q025 > tolerance
      pair_decision <- dplyr::case_when(
        eq_pair ~ "equivalent",
        lhs_better ~ "lhs_better",
        rhs_better ~ "rhs_better",
        TRUE ~ "inconclusive"
      )

      better <- dplyr::case_when(
        pair_decision == "lhs_better" ~ lhs,
        pair_decision == "rhs_better" ~ rhs,
        TRUE ~ NA_character_
      )

      tibble::tibble(
        policy_a = lhs,
        equation_branch_filter_a = lhs_branch,
        policy_b = rhs,
        equation_branch_filter_b = rhs_branch,
        n_species_common = n_common,
        paired_mean_diff = mean_diff,
        paired_median_diff = med_diff,
        paired_boot_q025 = q025,
        paired_boot_q975 = q975,
        equivalent_pair = eq_pair,
        pair_decision = pair_decision,
        better_policy = better
      )
    })

  # Convert the pairwise table to one row per policy showing whether it is
  # equivalent to the global best policy.
  best_flags <- policy_nodes |>
    dplyr::mutate(
      .policy_key = paste(
        as.character(policy),
        normalize_policy_equation_branch_filters(equation_branch_filter),
        sep = "|"
      ),
      equivalent_to_best_global = purrr::map_lgl(.policy_key, function(policy_key_now) {
        if (identical(policy_key_now, best_key)) {
          return(TRUE)
        }

        row <- pair_tbl |>
          dplyr::filter(
            (paste(
              as.character(policy_a),
              normalize_policy_equation_branch_filters(equation_branch_filter_a),
              sep = "|"
            ) == best_key &
              paste(
                as.character(policy_b),
                normalize_policy_equation_branch_filters(equation_branch_filter_b),
                sep = "|"
              ) == policy_key_now) |
              (paste(
                as.character(policy_b),
                normalize_policy_equation_branch_filters(equation_branch_filter_b),
                sep = "|"
              ) == best_key &
                paste(
                  as.character(policy_a),
                  normalize_policy_equation_branch_filters(equation_branch_filter_a),
                  sep = "|"
                ) == policy_key_now)
          ) |>
          dplyr::slice(1)

        nrow(row) == 1 && isTRUE(row$equivalent_pair[[1]])
      }),
      paired_mean_diff_to_best = purrr::map_dbl(.policy_key, function(policy_key_now) {
        if (identical(policy_key_now, best_key)) {
          return(0)
        }

        row <- pair_tbl |>
          dplyr::filter(
            (paste(
              as.character(policy_a),
              normalize_policy_equation_branch_filters(equation_branch_filter_a),
              sep = "|"
            ) == best_key &
              paste(
                as.character(policy_b),
                normalize_policy_equation_branch_filters(equation_branch_filter_b),
                sep = "|"
              ) == policy_key_now) |
              (paste(
                as.character(policy_b),
                normalize_policy_equation_branch_filters(equation_branch_filter_b),
                sep = "|"
              ) == best_key &
                paste(
                  as.character(policy_a),
                  normalize_policy_equation_branch_filters(equation_branch_filter_a),
                  sep = "|"
                ) == policy_key_now)
          ) |>
          dplyr::slice(1)

        if (nrow(row) == 0 || !is.finite(row$paired_mean_diff[[1]])) {
          return(NA_real_)
        }

        if (identical(
          paste(
            as.character(row$policy_a[[1]]),
            normalize_policy_equation_branch_filters(row$equation_branch_filter_a[[1]]),
            sep = "|"
          ),
          best_key
        )) {
          return(row$paired_mean_diff[[1]])
        }

        -row$paired_mean_diff[[1]]
      }),
      best_policy_global = best_policy$policy[[1]],
      best_equation_branch_filter_global = best_policy$equation_branch_filter[[1]]
    ) |>
    dplyr::select(-.policy_key)

  list(pairs = pair_tbl, best_flags = best_flags)
}

#' Build policy-equivalence classes
#'
#' Converts pairwise equivalence calls to connected equivalence classes.
#'
#' @param select_ref Global policy-selection reference table.
#' @param pair_tbl Pairwise policy-equivalence table.
#'
#' @return A tibble.
#'
#' @export
build_equivalence_sets <- function(select_ref,
                                   pair_tbl) {
  # Start from the policy list in the selection table so the class summary
  # always covers every benchmarked policy.
  select_ref <- standardize_policies(select_ref)
  select_ref$policy <- resolve_policy_names(select_ref)
  policy_nodes <- select_ref |>
    dplyr::distinct(policy, equation_branch_filter) |>
    dplyr::arrange(policy, equation_branch_filter)
  if (nrow(policy_nodes) == 0) {
    return(tibble::tibble())
  }

  node_keys <- paste(
    as.character(policy_nodes$policy),
    normalize_policy_equation_branch_filters(policy_nodes$equation_branch_filter),
    sep = "|"
  )
  adjacency <- stats::setNames(vector("list", length(node_keys)), node_keys)
  for (policy_key_now in node_keys) {
    adjacency[[policy_key_now]] <- character(0)
  }

  # Build an undirected adjacency list from the pairwise equivalence calls.
  if (nrow(pair_tbl) > 0) {
    eq_pairs <- pair_tbl |>
      dplyr::filter(equivalent_pair) |>
      dplyr::select(policy_a, equation_branch_filter_a, policy_b, equation_branch_filter_b)

    if (nrow(eq_pairs) > 0) {
      for (i in seq_len(nrow(eq_pairs))) {
        lhs <- paste(
          as.character(eq_pairs$policy_a[[i]]),
          normalize_policy_equation_branch_filters(eq_pairs$equation_branch_filter_a[[i]]),
          sep = "|"
        )
        rhs <- paste(
          as.character(eq_pairs$policy_b[[i]]),
          normalize_policy_equation_branch_filters(eq_pairs$equation_branch_filter_b[[i]]),
          sep = "|"
        )
        adjacency[[lhs]] <- unique(c(adjacency[[lhs]], rhs))
        adjacency[[rhs]] <- unique(c(adjacency[[rhs]], lhs))
      }
    }
  }

  # Walk the connected components so each equivalence class is represented once
  # and each policy gets one membership row.
  visited <- stats::setNames(rep(FALSE, length(node_keys)), node_keys)
  class_rows <- list()
  class_id <- 0L

  for (root in node_keys) {
    if (visited[[root]]) {
      next
    }

    class_id <- class_id + 1L
    queue <- root
    members <- character(0)

    while (length(queue) > 0) {
      node <- queue[[1]]
      queue <- queue[-1]
      if (visited[[node]]) {
        next
      }

      visited[[node]] <- TRUE
      members <- c(members, node)
      nbrs <- adjacency[[node]] %||% character(0)
      queue <- unique(c(queue, nbrs[!visited[nbrs]]))
    }

    members <- sort(unique(members))
    member_idx <- match(members, node_keys)
    member_tbl <- policy_nodes[member_idx, , drop = FALSE]
    class_rows[[length(class_rows) + 1]] <- tibble::tibble(
      policy = member_tbl$policy,
      equation_branch_filter = member_tbl$equation_branch_filter,
      equivalence_class_id = paste0("class_", class_id),
      equivalence_class_size = length(members),
      equivalence_class_members = paste(
        policy_display_label(member_tbl$policy, member_tbl$equation_branch_filter),
        collapse = "; "
      )
    )
  }

  dplyr::bind_rows(class_rows) |>
    dplyr::arrange(equivalence_class_id, policy, equation_branch_filter)
}

#' Run the policy-selection summary
#'
#' Builds the global policy-selection table, pairwise equivalence summary,
#' and equivalence-class table from the species-block benchmark results.
#'
#' @param species_performance_table Species-block benchmark table.
#' @param candidate_models Optional candidate-model metadata used to enrich
#'   policy labels.
#' @param config Optional JSON path or list with `tolerance`, `n_boot`, and
#'   `seed`. A [Configurer] object is also accepted.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#' @param progress Logical scalar controlling stage messages.
#'
#' @return A list containing the selection table, pairwise equivalence table,
#'   equivalence-class table, and merged final selection table.
#'
#' @examples
#' \dontrun{
#' selection_obj <- run_policy_selection(
#'   species_block_perf,
#'   config = list(tolerance = 0.01, n_boot = 100L)
#' )
#' selection_obj$final_ref
#' }
#'
#' @export
run_policy_selection <- function(species_performance_table,
                                 candidate_models = NULL,
                                 config = NULL,
                                 cache_path = NULL,
                                 refresh = FALSE,
                                 progress = FALSE) {
  # Validate cache control and the benchmark input before any bootstrap work
  # begins.
  if (!is.data.frame(species_performance_table)) {
    stop("'species_performance_table' must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.null(cache_path) &&
    (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Reuse the cached selection object when available unless a refresh was
  # explicitly requested.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Inline the policy-selection defaults here so the function resolves its own
  # fallback settings without an extra config helper.
  config_values <- merge_cfg(
    list(
      one_se_multiplier = 1,
      equivalence_tolerance = 0.05,
      tolerance = 0.05,
      n_boot = 500L,
      seed = NULL
    ),
    read_similarity_config(config)
  )
  config_values$equivalence_tolerance <- config_values$equivalence_tolerance %||% config_values$tolerance

  # Build the base selection summary first, then layer on the pairwise and
  # equivalence-class summaries before assembling the final merged table.
  select_ref <- build_selection_table(
    species_performance_table = species_performance_table,
    candidate_models = candidate_models,
    one_se_multiplier = config_values$one_se_multiplier,
    equivalence_tolerance = config_values$equivalence_tolerance,
    n_boot = config_values$n_boot,
    seed = config_values$seed,
    progress = progress
  )
  equiv_ref <- build_equivalence_table(
    species_performance_table = species_performance_table,
    select_ref = select_ref,
    tolerance = config_values$equivalence_tolerance,
    n_boot = config_values$n_boot,
    seed = if (!is.null(config_values$seed)) config_values$seed + 3L else NULL,
    progress = progress
  )
  equiv_sets <- build_equivalence_sets(
    select_ref = select_ref,
    pair_tbl = equiv_ref$pairs
  )

  final_ref <- select_ref |>
    dplyr::left_join(equiv_ref$best_flags, by = c("policy", "equation_branch_filter")) |>
    dplyr::left_join(equiv_sets, by = c("policy", "equation_branch_filter")) |>
    dplyr::mutate(
      equivalent_to_best_global = dplyr::coalesce(equivalent_to_best_global, FALSE),
      paired_mean_diff_to_best = dplyr::coalesce(paired_mean_diff_to_best, NA_real_)
    )

  result <- list(
    select_ref = select_ref,
    equiv_ref = equiv_ref,
    equiv_sets = equiv_sets,
    final_ref = final_ref
  )

  # Cache the in-memory selection summaries only when a cache path was
  # supplied.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  result
}
