#' Rank policy specificity
#'
#' Returns an ordinal specificity ranking used to break ties when multiple
#' policies have nearly identical benchmark error.
#'
#' @param policy Character vector of policy names.
#'
#' @return Integer vector.
#'
#' @export
policy_specificity_rank <- function(policy) {
  # Keep the global policy-selection rule agnostic to policy family. Benchmark
  # error, bootstrap stability, and equivalence summaries determine support;
  # policy name is used later only as a deterministic final tie-break.
  rep.int(1L, length(policy))
}

#' Add calibrated interval columns to anchor-policy rows
#'
#' Attaches the conformal prediction interval to each policy row. The
#' operational interval is derived from the conformal log-radius alone
#' (`q_col`), which is calibrated from held-out transfer errors and already
#' reflects the quality of the donor pool for species like the anchor. Donor
#' structural spread (`local_structural_q_abs_log`) is retained as a
#' diagnostic column but is not combined into the interval: doing so would
#' require assuming independence between the two sources, an assumption that
#' is violated because both increase together when the donor pool is
#' heterogeneous.
#'
#' @param policy_tbl Anchor-policy prediction table.
#' @param structural_uncertainty_weight Ignored. Retained for backward
#'   compatibility only; structural spread is no longer combined into the
#'   operational interval.
#' @param q_col Name of the conformal log-radius column.
#' @param prediction_col Name of the predicted biomass multiplier column.
#'
#' @return `policy_tbl` with `q_abs_log_conformal`, `q_abs_log_structural`
#'   (diagnostic only), `q_abs_log_total`, `multiplier_lo`, `multiplier_hi`,
#'   `interval_log_width`, and `uncertainty_cost_log_width`.
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
  # Normalize the incoming table once so all downstream workflows compute the
  # same interval columns from the same conformal-plus-structural formula.
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
  # Structural spread is the 90th-percentile weighted deviation of donor
  # log-backscatter values from the policy mean. It is retained as a
  # diagnostic column to indicate why a given interval is wide, but is NOT
  # combined into the operational interval — the conformal radius already
  # reflects transfer error calibrated from anchors with comparably sized
  # donor pools, so adding structural spread would double-count that source.
  q_structural_diagnostic <- if ("local_structural_q_abs_log" %in% names(out)) {
    suppressWarnings(as.numeric(out$local_structural_q_abs_log))
  } else {
    rep(NA_real_, nrow(out))
  }

  # Materialize the multiplicative interval bounds here so plotting and policy
  # selection layers can share the exact same uncertainty columns.
  out$q_abs_log_conformal  <- q_conformal
  out$q_abs_log_structural <- q_structural_diagnostic
  out$q_abs_log_total      <- q_conformal
  out$multiplier_lo <- dplyr::if_else(
    out$valid_prediction & is.finite(q_conformal),
    out[[prediction_col]] * exp(-q_conformal),
    NA_real_
  )
  out$multiplier_hi <- dplyr::if_else(
    out$valid_prediction & is.finite(q_conformal),
    out[[prediction_col]] * exp(q_conformal),
    NA_real_
  )
  out$interval_log_width <- dplyr::if_else(
    is.finite(q_conformal),
    2 * q_conformal,
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
#' @param uncertainty_relative_tolerance Relative width tolerance around the
#'   minimum calibrated interval width after empirical-score screening.
#' @param uncertainty_absolute_tolerance Absolute log-width tolerance around the
#'   minimum calibrated interval width after empirical-score screening.
#' @param local_distance_tolerance Absolute tolerance for retaining local
#'   support-distance ties after empirical-score and uncertainty screening.
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
                                   uncertainty_relative_tolerance = 0.25,
                                   uncertainty_absolute_tolerance = 0.05,
                                   local_distance_tolerance = 1e-12) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  if (nrow(policy_tbl) == 0) {
    return(policy_tbl)
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
  if (!"specificity_rank" %in% names(policy_tbl)) {
    policy_tbl$specificity_rank <- policy_specificity_rank(policy_tbl$policy)
  }

  local_distance <- dplyr::coalesce(
    policy_tbl$local_weighted_mean_combined_distance,
    policy_tbl$local_min_combined_distance
  )
  validation_error <- dplyr::coalesce(
    policy_tbl$species_block_median_abs_log_error,
    policy_tbl$mean_species_median_abs_log
  )
  policy_tbl$anchor_selection_local_distance <- local_distance
  policy_tbl$anchor_selection_validation_error <- validation_error
  policy_tbl$anchor_selection_global_screen <- dplyr::coalesce(
    as.logical(policy_tbl$acceptable_global),
    as.logical(policy_tbl$equivalent_to_best_global),
    FALSE
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
        anchor_selection_local_distance,
        dplyr::desc(local_effective_support),
        specificity_rank,
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

  globally_screened <- candidate_tbl |>
    dplyr::filter(anchor_selection_global_screen)
  if (nrow(globally_screened) > 0) {
    candidate_tbl <- globally_screened
    tier <- paste0(tier, "_global_accept")
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

  if (is.finite(min_validation_error)) {
    best_score_rows <- score_ranked |>
      dplyr::filter(
        anchor_selection_validation_error <= min_validation_error + as.numeric(local_distance_tolerance)
      )
  } else {
    best_score_rows <- score_ranked
  }

  min_width <- suppressWarnings(min(best_score_rows$uncertainty_cost_log_width, na.rm = TRUE))
  if (!is.finite(min_width)) {
    return(candidate_tbl[0, , drop = FALSE])
  }

  width_threshold <- min_width + max(
    as.numeric(uncertainty_absolute_tolerance),
    abs(min_width) * as.numeric(uncertainty_relative_tolerance),
    na.rm = TRUE
  )

  near_min_uncertainty <- best_score_rows |>
    dplyr::filter(uncertainty_cost_log_width <= width_threshold)
  if (nrow(near_min_uncertainty) == 0) {
    near_min_uncertainty <- best_score_rows |>
      dplyr::filter(abs(uncertainty_cost_log_width - min_width) <= 1e-12)
  }

  # After empirical benchmark score and conditional uncertainty have screened the
  # candidate set, use locality only as the final contextual tie-break.
  min_local <- suppressWarnings(min(near_min_uncertainty$anchor_selection_local_distance, na.rm = TRUE))
  if (is.finite(min_local)) {
    selected <- near_min_uncertainty |>
      dplyr::filter(
        is.finite(anchor_selection_local_distance),
        anchor_selection_local_distance <= min_local + as.numeric(local_distance_tolerance)
      )
  } else {
    selected <- near_min_uncertainty
  }

  if (nrow(selected) == 0) {
    selected <- near_min_uncertainty
  }

  selected |>
    dplyr::arrange(
      anchor_selection_validation_error,
      uncertainty_cost_log_width,
      anchor_selection_local_distance,
      bootstrap_median_rank,
      dplyr::desc(local_effective_support),
      specificity_rank,
      policy
    ) |>
    dplyr::mutate(
      selected_policy = policy,
      selected_policy_display = policy,
      selection_tier = tier,
      anchor_selection_min_validation_error = min_validation_error,
      anchor_selection_validation_threshold = min_validation_error,
      anchor_selection_min_uncertainty_width = min_width,
      anchor_selection_uncertainty_threshold = width_threshold
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
      .groups = "drop"
    )
}

#' Build the policy-selection reference table
#'
#' Converts the leave-one-species-out benchmark results to a global policy
#' comparison table using a one-standard-error rule plus bootstrap stability.
#'
#' @param species_performance_table Species-block benchmark table.
#' @param tolerance Optional tolerance value retained in the output for
#'   reference.
#' @param n_boot Number of bootstrap resamples across species.
#' @param seed Integer bootstrap seed.
#'
#' @return A tibble.
#'
#' @export
build_selection_table <- function(species_performance_table,
                                  one_se_multiplier = 1,
                                  equivalence_tolerance = 0.05,
                                  n_boot = 500L,
                                  seed = NULL) {
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
      median_species_median_abs_log = stats::median(species_median_abs_log, na.rm = TRUE),
      mean_species_median_abs_log = mean(species_median_abs_log, na.rm = TRUE),
      sd_species_median_abs_log = stats::sd(species_median_abs_log, na.rm = TRUE),
      se_species_median_abs_log = dplyr::if_else(
        n_species > 1,
        sd_species_median_abs_log / sqrt(n_species),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      specificity_rank = policy_specificity_rank(policy),
      policy_display = policy_display_label(policy, equation_branch_filter)
    )

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

  # Bootstrap the species-level summaries rather than the anchor rows so the
  # stability rule stays aligned with the validation design.
  boot_tbl <- purrr::map_dfr(seq_len(as.integer(n_boot)), function(boot_id) {
    species_level |>
      dplyr::group_by(policy, equation_branch_filter) |>
      dplyr::summarise(
        boot_mean_species_median_abs_log = mean(
          sample(species_median_abs_log, size = dplyr::n(), replace = TRUE),
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(boot_id = boot_id)
  })

  # Summarize the bootstrap distribution for each policy relative to the
  # one-SE threshold.
  boot_sum <- boot_tbl |>
    dplyr::group_by(policy, equation_branch_filter) |>
    dplyr::summarise(
      bootstrap_mean_q05 = stats::quantile(
        boot_mean_species_median_abs_log,
        probs = 0.05,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),
      bootstrap_mean_q50 = stats::quantile(
        boot_mean_species_median_abs_log,
        probs = 0.50,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),
      bootstrap_mean_q95 = stats::quantile(
        boot_mean_species_median_abs_log,
        probs = 0.95,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),
      bootstrap_prob_within_threshold = mean(
        boot_mean_species_median_abs_log <= threshold,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  # Rank policies within each bootstrap draw to estimate how often each one
  # is the best available choice.
  boot_rank <- boot_tbl |>
    dplyr::mutate(specificity_rank = policy_specificity_rank(policy)) |>
    dplyr::group_by(boot_id) |>
    dplyr::arrange(
      boot_mean_species_median_abs_log,
      specificity_rank,
      policy,
      equation_branch_filter,
      .by_group = TRUE
    ) |>
    dplyr::mutate(rank_boot = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(policy, equation_branch_filter) |>
    dplyr::summarise(
      bootstrap_prob_best = mean(rank_boot == 1, na.rm = TRUE),
      bootstrap_median_rank = stats::median(rank_boot, na.rm = TRUE),
      .groups = "drop"
    )

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
#'
#' @return A list with `pairs` and `best_flags`.
#'
#' @export
build_equivalence_table <- function(species_performance_table,
                                    select_ref,
                                    tolerance = 0.05,
                                    n_boot = 500L,
                                    seed = NULL) {
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

      wide <- species_level |>
        dplyr::mutate(
          .policy_key = paste(
            as.character(policy),
            normalize_policy_equation_branch_filters(equation_branch_filter),
            sep = "|"
          )
        ) |>
        dplyr::filter(.policy_key %in% c(lhs_key, rhs_key)) |>
        dplyr::select(.policy_key, anchor_species, species_median_abs_log) |>
        tidyr::pivot_wider(names_from = .policy_key, values_from = species_median_abs_log) |>
        dplyr::filter(is.finite(.data[[lhs_key]]), is.finite(.data[[rhs_key]]))

      if (nrow(wide) == 0) {
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
          better_policy = NA_character_
        ))
      }

      # Bootstrap the paired species-level differences so equivalence reflects
      # both practical magnitude and bootstrap uncertainty.
      diff_vec <- wide[[lhs_key]] - wide[[rhs_key]]
      set.seed(as.integer(seed) + sum(utf8ToInt(paste(lhs_key, rhs_key, sep = "|"))))
      boot_means <- replicate(as.integer(n_boot), {
        idx <- sample.int(length(diff_vec), size = length(diff_vec), replace = TRUE)
        mean(diff_vec[idx], na.rm = TRUE)
      })

      q025 <- stats::quantile(boot_means, probs = 0.025, na.rm = TRUE, names = FALSE, type = 8)
      q975 <- stats::quantile(boot_means, probs = 0.975, na.rm = TRUE, names = FALSE, type = 8)
      mean_diff <- mean(diff_vec, na.rm = TRUE)
      med_diff <- stats::median(diff_vec, na.rm = TRUE)

      eq_pair <- is.finite(mean_diff) &&
        abs(mean_diff) <= tolerance &&
        is.finite(q025) &&
        is.finite(q975) &&
        q025 <= 0 &&
        q975 >= 0

      better <- dplyr::case_when(
        eq_pair ~ NA_character_,
        is.finite(mean_diff) && mean_diff < 0 ~ lhs,
        is.finite(mean_diff) && mean_diff > 0 ~ rhs,
        TRUE ~ NA_character_
      )

      tibble::tibble(
        policy_a = lhs,
        equation_branch_filter_a = lhs_branch,
        policy_b = rhs,
        equation_branch_filter_b = rhs_branch,
        n_species_common = nrow(wide),
        paired_mean_diff = mean_diff,
        paired_median_diff = med_diff,
        paired_boot_q025 = q025,
        paired_boot_q975 = q975,
        equivalent_pair = eq_pair,
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
#' @param config Optional JSON path or list with `tolerance`, `n_boot`, and
#'   `seed`. A [Configurer] object is also accepted.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
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
                                 config = NULL,
                                 cache_path = NULL,
                                 refresh = FALSE) {
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
    one_se_multiplier = config_values$one_se_multiplier,
    equivalence_tolerance = config_values$equivalence_tolerance,
    n_boot = config_values$n_boot,
    seed = config_values$seed
  )
  equiv_ref <- build_equivalence_table(
    species_performance_table = species_performance_table,
    select_ref = select_ref,
    tolerance = config_values$equivalence_tolerance,
    n_boot = config_values$n_boot,
    seed = config_values$seed + 3L
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


