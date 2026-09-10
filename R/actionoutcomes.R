# Counterfactual action outcomes -------------------------------------------

#' Decode one canonical action donor footprint
#'
#' @keywords internal
#' @noRd
decode_action_footprint <- function(action_row) {
  action_row <- tibble::as_tibble(action_row)
  if (nrow(action_row) != 1L) {
    stop("'action_row' must contain exactly one canonical action.", call. = FALSE)
  }
  required <- c("donor_ids", "donor_weights", "n_donors")
  missing <- setdiff(required, names(action_row))
  if (length(missing) > 0L) {
    stop(
      "Canonical action is missing footprint field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  donor_ids_text <- as.character(action_row$donor_ids[[1]])
  donor_weights_text <- as.character(action_row$donor_weights[[1]])
  expected_n <- suppressWarnings(as.integer(action_row$n_donors[[1]]))
  if (!is.finite(expected_n) || expected_n < 1L ||
      is.na(donor_ids_text) || !nzchar(donor_ids_text) ||
      is.na(donor_weights_text) || !nzchar(donor_weights_text)) {
    stop("Canonical action has an empty or malformed donor footprint.", call. = FALSE)
  }
  ids <- strsplit(donor_ids_text, ";", fixed = TRUE)[[1]]
  weights <- suppressWarnings(as.numeric(
    strsplit(donor_weights_text, ";", fixed = TRUE)[[1]]
  ))
  if (length(ids) != expected_n || length(weights) != expected_n ||
      anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids) ||
      any(!is.finite(weights)) || any(weights <= 0)) {
    stop("Canonical action donor IDs or weights do not match 'n_donors'.", call. = FALSE)
  }
  weight_sum <- sum(weights)
  if (!is.finite(weight_sum) || weight_sum <= 0) {
    stop("Canonical action donor weights do not have positive finite mass.", call. = FALSE)
  }
  weights <- weights / weight_sum
  order_idx <- order(ids, method = "radix")
  tibble::tibble(
    donor_model_id = ids[order_idx],
    donor_weight = weights[order_idx]
  )
}

#' Compute a weighted finite summary
#'
#' @keywords internal
#' @noRd
weighted_action_summary <- function(x, weights) {
  x <- suppressWarnings(as.numeric(x))
  weights <- suppressWarnings(as.numeric(weights))
  keep <- is.finite(x) & is.finite(weights) & weights > 0
  if (!any(keep)) {
    return(c(min = NA_real_, mean = NA_real_, max = NA_real_))
  }
  x <- x[keep]
  weights <- weights[keep]
  c(min = min(x), mean = stable_weighted_mean(x, weights), max = max(x))
}

#' Remove donors sharing a declared provenance unit with a pseudoanchor
#'
#' Unlike a predictive trait gate, this block prevents a pseudoanchor from
#' being evaluated using another equation from the same source unit. The
#' provenance field is mandatory and no alternate field is substituted.
#'
#' @keywords internal
#' @noRd
block_pseudoanchor_provenance <- function(eval_obj,
                                          anchor_row,
                                          group_col = "study_reference_id") {
  if (!is.character(group_col) || length(group_col) != 1L ||
      is.na(group_col) || !nzchar(group_col)) {
    stop("'group_col' must be one explicit provenance column.", call. = FALSE)
  }
  anchor_row <- tibble::as_tibble(anchor_row)
  if (nrow(anchor_row) != 1L || !group_col %in% names(anchor_row)) {
    stop(
      "Pseudoanchor is missing the declared provenance column '",
      group_col,
      "'.",
      call. = FALSE
    )
  }
  if (!is.list(eval_obj) || !is.data.frame(eval_obj$model_eval) ||
      !is.data.frame(eval_obj$admissible_df) ||
      !group_col %in% names(eval_obj$model_eval) ||
      !group_col %in% names(eval_obj$admissible_df)) {
    stop(
      "Donor evaluation is missing the declared provenance column '",
      group_col,
      "'.",
      call. = FALSE
    )
  }
  anchor_group <- as.character(anchor_row[[group_col]][[1]])
  if (is.na(anchor_group) || !nzchar(anchor_group)) {
    stop(
      "Pseudoanchor has no value for declared provenance column '",
      group_col,
      "'.",
      call. = FALSE
    )
  }

  before_model <- tibble::as_tibble(eval_obj$model_eval)
  before_admissible <- tibble::as_tibble(eval_obj$admissible_df)
  same_model <- as.character(before_model[[group_col]]) == anchor_group
  same_admissible <- as.character(before_admissible[[group_col]]) == anchor_group
  same_model[is.na(same_model)] <- FALSE
  same_admissible[is.na(same_admissible)] <- FALSE
  out <- remove_group_support(eval_obj, anchor_row, group_col)

  list(
    evaluation = out,
    audit = tibble::tibble(
      provenance_group_col = group_col,
      provenance_group_value = anchor_group,
      model_rows_before = nrow(before_model),
      model_rows_excluded = sum(same_model),
      admissible_rows_before = nrow(before_admissible),
      admissible_rows_excluded = sum(same_admissible),
      admissible_rows_after = nrow(out$admissible_df)
    )
  )
}

#' Resolve donor rows for one canonical action
#'
#' @keywords internal
#' @noRd
action_donor_rows <- function(action_row, eval_obj) {
  if (!is.list(eval_obj) || !is.data.frame(eval_obj$admissible_df)) {
    stop("'eval_obj' must contain an 'admissible_df' data frame.", call. = FALSE)
  }
  footprint <- decode_action_footprint(action_row)
  donors <- tibble::as_tibble(eval_obj$admissible_df)
  if (!"model_id" %in% names(donors)) {
    stop("Admissible donor rows require 'model_id'.", call. = FALSE)
  }
  donor_ids <- as.character(donors$model_id)
  if (anyDuplicated(donor_ids)) {
    stop("Admissible donor rows require unique model IDs.", call. = FALSE)
  }
  idx <- match(footprint$donor_model_id, donor_ids)
  if (anyNA(idx)) {
    stop(
      "Canonical action references donor(s) absent from its admissible pool: ",
      paste(footprint$donor_model_id[is.na(idx)], collapse = ", "),
      call. = FALSE
    )
  }
  out <- donors[idx, , drop = FALSE]
  out$donor_model_id <- footprint$donor_model_id
  out$donor_weight <- footprint$donor_weight
  out
}

#' Compute outcomes and structural diagnostics for canonical actions
#'
#' Every row compares one realized action with the pseudoanchor's own
#' standardized equation over the pseudoanchor length distribution. The
#' predeclared scalar loss is `abs(log(multiplier))`; no clipping, winsorizing,
#' or display bound is applied here.
#'
#' @param eval_obj One-pseudoanchor admissibility evaluation.
#' @param anchor_row One-row pseudoanchor table.
#' @param action_bundle Output from [enumerate_action_universe()].
#'
#' @return A list with one-row-per-action `outcomes` and an enriched long
#'   `footprints` table.
#'
#' @keywords internal
#' @noRd
compute_counterfactual_action_outcomes <- function(eval_obj,
                                                   anchor_row,
                                                   action_bundle) {
  if (!is.list(eval_obj) || is.null(eval_obj$anchor_pdf) ||
      !is.data.frame(eval_obj$admissible_df)) {
    stop("'eval_obj' is not a complete pseudoanchor evaluation.", call. = FALSE)
  }
  anchor_row <- tibble::as_tibble(anchor_row)
  if (nrow(anchor_row) != 1L) {
    stop("'anchor_row' must contain exactly one row.", call. = FALSE)
  }
  if (!is.list(action_bundle) || !is.data.frame(action_bundle$actions)) {
    stop("'action_bundle' must contain canonical 'actions'.", call. = FALSE)
  }
  actions <- tibble::as_tibble(action_bundle$actions)
  if (nrow(actions) == 0L) {
    return(list(outcomes = actions, footprints = tibble::tibble()))
  }

  anchor_id <- as.character(anchor_row$model_id[[1]])
  anchor_species <- if ("species_name" %in% names(anchor_row)) {
    as.character(anchor_row$species_name[[1]])
  } else {
    NA_character_
  }
  anchor_slope <- suppressWarnings(as.numeric(anchor_row$slope_len[[1]]))
  anchor_intercept <- suppressWarnings(as.numeric(anchor_row$intercept_len[[1]]))
  anchor_sigma <- suppressWarnings(as.numeric(eval_obj$anchor_sigma)[[1]])
  pdf <- tibble::as_tibble(eval_obj$anchor_pdf)
  pdf_ok <- all(c("length_cm", "f_len") %in% names(pdf)) &&
    any(is.finite(pdf$length_cm) & pdf$length_cm > 0 &
      is.finite(pdf$f_len) & pdf$f_len >= 0) &&
    sum(pdf$f_len[is.finite(pdf$f_len) & pdf$f_len >= 0]) > 0
  if (!is.finite(anchor_slope) || !is.finite(anchor_intercept) ||
      !is.finite(anchor_sigma) || anchor_sigma <= 0 || !pdf_ok) {
    stop(
      "Pseudoanchor requires finite coefficients, positive backscatter, and valid length support.",
      call. = FALSE
    )
  }
  pdf_keep <- is.finite(pdf$length_cm) & pdf$length_cm > 0 &
    is.finite(pdf$f_len) & pdf$f_len >= 0
  pdf <- pdf[pdf_keep, , drop = FALSE]
  pdf_w <- as.numeric(pdf$f_len) / sum(pdf$f_len)
  log_length <- log10(as.numeric(pdf$length_cm))
  anchor_curve <- anchor_slope * log_length + anchor_intercept

  distance_fields <- c(
    combined_distance = "combined_distance",
    species_distance = "d_species",
    survey_distance = "d_study",
    taxonomic_distance = "taxonomic_distance_to_anchor",
    frequency_coherence_distance = "frequency_coherence_distance",
    length_coherence_distance = "length_coherence_distance",
    depth_coherence_distance = "depth_coherence_distance",
    learned_distance_disagreement = "learned_distance_disagreement"
  )

  outcome_rows <- vector("list", nrow(actions))
  footprint_rows <- vector("list", nrow(actions))
  for (i in seq_len(nrow(actions))) {
    action <- actions[i, , drop = FALSE]
    donors <- action_donor_rows(action, eval_obj)
    weights <- as.numeric(donors$donor_weight)
    slope <- suppressWarnings(as.numeric(action$policy_slope_len[[1]]))
    intercept <- suppressWarnings(as.numeric(action$policy_intercept_len[[1]]))
    sigma <- equation_sigma_mean(slope, intercept, pdf)
    multiplier <- if (is.finite(sigma) && sigma > 0) anchor_sigma / sigma else NA_real_
    signed_log_multiplier <- if (is.finite(multiplier) && multiplier > 0) {
      log(multiplier)
    } else {
      NA_real_
    }
    action_curve <- slope * log_length + intercept
    curve_delta <- action_curve - anchor_curve

    donor_slopes <- suppressWarnings(as.numeric(donors$slope_len))
    donor_intercepts <- suppressWarnings(as.numeric(donors$intercept_len))
    donor_curve_sq <- vapply(seq_len(nrow(donors)), function(j) {
      donor_curve <- donor_slopes[[j]] * log_length + donor_intercepts[[j]]
      sum(pdf_w * (donor_curve - action_curve)^2)
    }, numeric(1))
    donor_sigma <- vapply(seq_len(nrow(donors)), function(j) {
      equation_sigma_mean(donor_slopes[[j]], donor_intercepts[[j]], pdf)
    }, numeric(1))
    donor_signed_log_multiplier <- if (
      is.finite(anchor_sigma) && anchor_sigma > 0 &&
        all(is.finite(donor_sigma) & donor_sigma > 0)
    ) {
      log(anchor_sigma / donor_sigma)
    } else {
      rep(NA_real_, nrow(donors))
    }
    expected_member_abs_log_loss <- if (
      all(is.finite(donor_signed_log_multiplier))
    ) {
      sum(weights * abs(donor_signed_log_multiplier))
    } else {
      NA_real_
    }
    donor_sources <- as.character(donors$study_reference_id)
    source_identity_estimable <- all(!is.na(donor_sources) & nzchar(donor_sources))
    source_weights <- if (source_identity_estimable) {
      vapply(split(weights, donor_sources), sum, numeric(1))
    } else {
      numeric()
    }
    effective_source_count <- if (length(source_weights) > 0L) {
      1 / sum(source_weights^2)
    } else {
      NA_real_
    }
    log_sigma_delta <- if (is.finite(sigma) && sigma > 0) {
      log(donor_sigma) - log(sigma)
    } else {
      rep(NA_real_, nrow(donors))
    }

    distance_values <- list()
    for (output_name in names(distance_fields)) {
      input_name <- unname(distance_fields[[output_name]])
      values <- if (input_name %in% names(donors)) donors[[input_name]] else NA_real_
      summary_now <- weighted_action_summary(values, weights)
      distance_values[[paste0("min_", output_name)]] <- unname(summary_now[["min"]])
      distance_values[[paste0("weighted_mean_", output_name)]] <- unname(summary_now[["mean"]])
      distance_values[[paste0("max_", output_name)]] <- unname(summary_now[["max"]])
    }

    valid_outcome <- all(is.finite(c(
      slope, intercept, sigma, multiplier, signed_log_multiplier
    ))) && sigma > 0 && multiplier > 0
    outcome_status <- if (valid_outcome) "estimable" else "nonfinite_action_outcome"
    base <- tibble::tibble(
      anchor_model_id = anchor_id,
      anchor_species = anchor_species,
      anchor_study_reference_id = if ("study_reference_id" %in% names(anchor_row)) {
        as.character(anchor_row$study_reference_id[[1]])
      } else {
        NA_character_
      },
      anchor_study_cell_id = if ("study_cell_id" %in% names(anchor_row)) {
        as.character(anchor_row$study_cell_id[[1]])
      } else {
        NA_character_
      },
      action_id = as.character(action$action_id[[1]]),
      equation_branch_filter = as.character(action$equation_branch_filter[[1]]),
      n_donors = as.integer(nrow(donors)),
      effective_donor_count = 1 / sum(weights^2),
      max_donor_weight = max(weights),
      donor_weight_hhi = sum(weights^2),
      policy_slope_len = slope,
      policy_intercept_len = intercept,
      anchor_slope_len = anchor_slope,
      anchor_intercept_len = anchor_intercept,
      slope_error = slope - anchor_slope,
      intercept_error = intercept - anchor_intercept,
      policy_sigma_bs_mean = sigma,
      anchor_sigma_bs_mean = anchor_sigma,
      multiplier_pred = multiplier,
      signed_log_multiplier = signed_log_multiplier,
      selection_loss_abs_log = abs(signed_log_multiplier),
      expected_member_abs_log_loss = expected_member_abs_log_loss,
      ensemble_cancellation_gap_abs_log =
        expected_member_abs_log_loss - abs(signed_log_multiplier),
      weighted_curve_bias_db = sum(pdf_w * curve_delta),
      weighted_curve_rmse_db = sqrt(sum(pdf_w * curve_delta^2)),
      donor_slope_rms_heterogeneity = sqrt(sum(weights * (donor_slopes - slope)^2)),
      donor_intercept_rms_heterogeneity = sqrt(sum(weights * (donor_intercepts - intercept)^2)),
      donor_curve_rms_heterogeneity_db = sqrt(sum(weights * donor_curve_sq)),
      donor_log_sigma_rms_heterogeneity = if (all(is.finite(log_sigma_delta))) {
        sqrt(sum(weights * log_sigma_delta^2))
      } else {
        NA_real_
      },
      effective_source_count = effective_source_count,
      ensemble_log_sigma_standard_error = if (
        all(is.finite(log_sigma_delta)) && is.finite(effective_source_count) &&
          effective_source_count > 0
      ) {
        sqrt(sum(weights * log_sigma_delta^2)) / sqrt(effective_source_count)
      } else {
        NA_real_
      },
      ensemble_epistemic_risk_abs_log = if (
        all(is.finite(log_sigma_delta)) && is.finite(effective_source_count) &&
          effective_source_count > 0
      ) {
        sqrt(
          abs(signed_log_multiplier)^2 +
            (sqrt(sum(weights * log_sigma_delta^2)) / sqrt(effective_source_count))^2
        )
      } else {
        NA_real_
      },
      outcome_estimable = valid_outcome,
      outcome_status = outcome_status
    )
    outcome_rows[[i]] <- dplyr::bind_cols(base, tibble::as_tibble(distance_values)) |>
      dplyr::left_join(
        action |>
          dplyr::select(
            "action_id", "action_signature", "donor_footprint",
            "policy_aliases", "candidate_pools", "aggregation_methods",
            "n_policy_aliases", "includes_exhaustive_singleton"
          ),
        by = "action_id"
      )

    footprint_rows[[i]] <- donors |>
      dplyr::transmute(
        anchor_model_id = anchor_id,
        anchor_species = anchor_species,
        anchor_study_reference_id = if ("study_reference_id" %in% names(anchor_row)) {
          as.character(anchor_row$study_reference_id[[1]])
        } else {
          NA_character_
        },
        anchor_study_cell_id = if ("study_cell_id" %in% names(anchor_row)) {
          as.character(anchor_row$study_cell_id[[1]])
        } else {
          NA_character_
        },
        action_id = as.character(action$action_id[[1]]),
        equation_branch_filter = as.character(action$equation_branch_filter[[1]]),
        donor_model_id = as.character(.data$donor_model_id),
        donor_weight = as.numeric(.data$donor_weight),
        donor_species = as.character(.data$species_name %||% NA_character_),
        donor_study_reference_id = as.character(.data$study_reference_id %||% NA_character_),
        donor_study_cell_id = as.character(.data$study_cell_id %||% NA_character_),
        donor_slope_len = as.numeric(.data$slope_len),
        donor_intercept_len = as.numeric(.data$intercept_len),
        donor_sigma_bs_mean = .env$donor_sigma,
        donor_signed_log_multiplier = .env$donor_signed_log_multiplier,
        donor_abs_log_multiplier = abs(.env$donor_signed_log_multiplier),
        combined_distance = as.numeric(.data$combined_distance %||% NA_real_),
        species_distance = as.numeric(.data$d_species %||% NA_real_),
        survey_distance = as.numeric(.data$d_study %||% NA_real_),
        taxonomic_distance = as.numeric(.data$taxonomic_distance_to_anchor %||% NA_real_)
      )
  }

  list(
    outcomes = dplyr::bind_rows(outcome_rows) |>
      dplyr::arrange(.data$anchor_model_id, .data$action_id),
    footprints = dplyr::bind_rows(footprint_rows) |>
      dplyr::arrange(.data$anchor_model_id, .data$action_id, .data$donor_model_id)
  )
}
