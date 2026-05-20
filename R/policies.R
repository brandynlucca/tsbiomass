#' Read the policy registry
#'
#' Reads the packaged policy registry JSON or a caller-supplied registry file.
#'
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return A parsed registry list.
#'
#' @export
read_policy_registry <- function(policy_path = NULL) {
  # Resolve the packaged registry path only when the caller did not supply an
  # override so the package keeps one canonical registry source.
  if (is.null(policy_path)) {
    policy_path <- system.file("templates", "policy_registry.json", package = "tsbiomass")
  }

  if (!is.character(policy_path) || length(policy_path) != 1 || !nzchar(policy_path)) {
    stop("'policy_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!file.exists(policy_path)) {
    stop(sprintf("Policy registry not found: %s", policy_path), call. = FALSE)
  }

  registry <- read_json_file(policy_path)
  if (!is.list(registry) || is.null(registry$policies) || !is.list(registry$policies)) {
    stop("The policy registry must contain a top-level 'policies' list.", call. = FALSE)
  }

  registry
}

#' List policy names
#'
#' Returns the coded policy names defined in the policy registry.
#'
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return Character vector of policy names.
#'
#' @export
policy_names <- function(policy_path = NULL) {
  # Pull just the coded names so callers can validate policy selections without
  # reimplementing the registry parsing logic.
  registry <- read_policy_registry(policy_path = policy_path)
  vapply(registry$policies, function(x) x$coded_name, character(1))
}

#' Resolve policy ordination context
#'
#' Normalizes optional ordination context for the policy layer.
#'
#' @param ordination_info Optional ordination-context list.
#'
#' @return A normalized context list.
#'
#' @keywords internal
resolve_policy_context <- function(ordination_info = NULL) {
  # Resolve the optional ordination context once, then fill the missing pieces
  # with empty defaults used by the policy selectors.
  context <- ordination_info %||% list()

  if (!is.list(context)) {
    stop("Ordination context must be NULL or a list.", call. = FALSE)
  }

  list(
    model_scores = context$model_scores %||% NULL,
    anchor_cluster = context$anchor_cluster %||% NA_character_,
    species_ellipse_ids = context$species_ellipse_ids %||% character(0)
  )
}

#' Normalize policy parameters
#'
#' Combines fixed registry parameters with caller-supplied overrides for one
#' policy. Caller overrides are expected as `policy_params[[policy_name]]`.
#'
#' @param policy_name Policy coded name.
#' @param policy_def One policy-definition list from the registry.
#' @param policy_params Optional named list of per-policy parameter overrides.
#'
#' @return A named list of resolved policy parameters.
#'
#' @keywords internal
policy_parameters <- function(policy_name,
                              policy_def,
                              policy_params = NULL) {
  # Start from registry-fixed values, then merge caller overrides so policy
  # defaults stay centralized in the registry but remain overridable.
  params <- policy_def$fixed_parameters %||% list()
  if (!is.list(params)) {
    params <- list()
  }

  override <- policy_params[[policy_name]] %||% list()
  if (!is.list(override)) {
    stop(sprintf("Policy parameters for '%s' must be a list.", policy_name), call. = FALSE)
  }
  params <- utils::modifyList(params, override)

  # Fill the generic tunable defaults only when the registry or caller did not
  # already supply them for the selected policy.
  if (is.null(params$support_cutoff) && identical(policy_def$candidate_pool, "core_support_subset")) {
    params$support_cutoff <- 0.8
  }
  if (is.null(params$k) && identical(policy_def$candidate_pool, "top_k_admissible")) {
    params$k <- 5L
  }
  if (is.null(params$phylo_radius) && identical(policy_def$candidate_pool, "phylogenetic_neighborhood")) {
    params$phylo_radius <- NULL
  }

  params
}

#' Normalize model weights
#'
#' Converts one numeric weight vector to nonnegative normalized weights.
#'
#' @param weight_values Numeric vector.
#'
#' @return Numeric vector summing to one, or `numeric(0)` if unusable.
#'
#' @keywords internal
normalized_weights <- function(weight_values) {
  # Clamp invalid weights to zero before normalization so later weighted
  # predictions do not have to repeat the same safety logic.
  weights <- as.numeric(weight_values)
  weights[!is.finite(weights) | weights < 0] <- 0
  weight_sum <- sum(weights, na.rm = TRUE)

  if (!is.finite(weight_sum) || weight_sum <= 0) {
    return(numeric(0))
  }

  weights / weight_sum
}

#' Filter valid multiplier rows
#'
#' Keeps only rows with a positive finite biomass multiplier.
#'
#' @param rows Candidate-policy row table.
#'
#' @return Tibble.
#'
#' @keywords internal
valid_multiplier_rows <- function(rows) {
  # Centralize the multiplier-validity filter so all policy aggregators apply
  # the same numerical screening rules.
  tibble::as_tibble(rows) |>
    dplyr::filter(
      is.finite(biomass_multiplier_if_replace),
      biomass_multiplier_if_replace > 0
    )
}

#' Filter valid curve rows
#'
#' Keeps only rows with finite length-form slope and intercept values.
#'
#' @param rows Candidate-policy row table.
#'
#' @return Tibble.
#'
#' @keywords internal
valid_curve_rows <- function(rows) {
  # Restrict curve prediction to rows that can actually produce a TS-length
  # curve in standardized slope-intercept form.
  tibble::as_tibble(rows) |>
    dplyr::filter(is.finite(slope_len), is.finite(intercept_len))
}

#' Identify generalized-model rows
#'
#' @param rows Candidate-policy row table.
#'
#' @return Tibble.
#'
#' @keywords internal
group_model_rows <- function(rows) {
  # Treat either the prepared boolean flag or the legacy method tag as evidence
  # that a row represents a generalized or grouped model.
  out <- tibble::as_tibble(rows)

  # Evaluate the optional grouping flags against the materialized tibble so the
  # filter does not depend on the magrittr pronoun.
  has_group_flag <- "is_group_model" %in% names(out)
  has_method_flag <- "method_type" %in% names(out)

  out |>
    dplyr::filter(
      (if (has_group_flag) is_group_model else FALSE) |
        (if (has_method_flag) method_type == "group" else FALSE)
    )
}

#' Build the top-support subset
#'
#' Recomputes a cumulative-weight support subset using `w_adm` and an arbitrary
#' support cutoff, rather than relying on one precomputed support flag.
#'
#' @param rows Candidate-policy row table.
#' @param support_cutoff Numeric cutoff between zero and one.
#'
#' @return Tibble.
#'
#' @keywords internal
top_support_rows <- function(rows,
                             support_cutoff) {
  # Rebuild the support subset from sorted final weights so policies with
  # alternative support cutoffs remain fully configurable.
  if (!is.numeric(support_cutoff) || length(support_cutoff) != 1 ||
      !is.finite(support_cutoff) || support_cutoff <= 0 || support_cutoff > 1) {
    stop("'support_cutoff' must be one finite number in (0, 1].", call. = FALSE)
  }

  keep_rows <- valid_multiplier_rows(rows) |>
    dplyr::filter(is.finite(w_adm), w_adm > 0) |>
    dplyr::arrange(dplyr::desc(w_adm), combined_distance)

  if (nrow(keep_rows) == 0) {
    return(keep_rows)
  }

  weights <- normalized_weights(keep_rows$w_adm)
  if (length(weights) == 0) {
    return(keep_rows[0, , drop = FALSE])
  }

  keep_rows$w_adm <- weights
  keep_rows$cumulative_w_adm <- cumsum(weights)

  keep_rows |>
    dplyr::filter(cumulative_w_adm <= support_cutoff | dplyr::row_number() == 1L)
}

#' Select policy rows
#'
#' Filters the admissible donor pool for one policy according to its registry
#' candidate pool and any resolved parameters.
#'
#' @param rows Admissible candidate rows.
#' @param policy_def One policy-definition list.
#' @param policy_params Resolved parameters for the policy.
#' @param ordination_info Optional ordination-context list.
#'
#' @return Tibble.
#'
#' @keywords internal
policy_rows <- function(rows,
                        policy_def,
                        policy_params,
                        ordination_info) {
  # Use the registry candidate-pool definition as the single policy-routing
  # switch so selection logic stays aligned with the registry names.
  pool_name <- as.character(policy_def$candidate_pool)[[1]]
  policy_rows <- tibble::as_tibble(rows)

  if (!"model_id_chr" %in% names(policy_rows) && "model_id" %in% names(policy_rows)) {
    policy_rows$model_id_chr <- as.character(policy_rows$model_id)
  }

  if (identical(pool_name, "all_admissible")) {
    return(policy_rows)
  }
  if (identical(pool_name, "same_species")) {
    return(dplyr::filter(policy_rows, overlap_same_species))
  }
  if (identical(pool_name, "phylogenetic_neighborhood")) {
    out <- dplyr::filter(policy_rows, !overlap_same_species)
    if (!is.null(policy_params$phylo_radius)) {
      out <- dplyr::filter(
        out,
        is.finite(taxonomic_distance_to_anchor),
        taxonomic_distance_to_anchor <= as.numeric(policy_params$phylo_radius)
      )
    }
    return(out)
  }
  if (identical(pool_name, "same_genus")) {
    return(dplyr::filter(policy_rows, overlap_same_genus, !overlap_same_species))
  }
  if (identical(pool_name, "same_family")) {
    return(dplyr::filter(policy_rows, overlap_same_family, !overlap_same_genus))
  }
  if (identical(pool_name, "same_order")) {
    return(dplyr::filter(policy_rows, overlap_same_order, !overlap_same_family))
  }
  if (identical(pool_name, "same_swimbladder")) {
    return(dplyr::filter(policy_rows, overlap_same_swimbladder, !overlap_same_species))
  }
  if (identical(pool_name, "same_nmds_cluster")) {
    cluster_ids <- tibble::as_tibble(ordination_info$model_scores %||% tibble::tibble()) |>
      dplyr::filter(nmds_cluster == ordination_info$anchor_cluster) |>
      dplyr::pull(model_id_chr) |>
      unique()
    return(dplyr::filter(policy_rows, model_id_chr %in% cluster_ids))
  }
  if (identical(pool_name, "same_species_ellipse")) {
    return(dplyr::filter(
      policy_rows,
      model_id_chr %in% ordination_info$species_ellipse_ids,
      !overlap_same_species
    ))
  }
  if (identical(pool_name, "generalized_models_only")) {
    return(group_model_rows(policy_rows))
  }
  if (identical(pool_name, "core_support_subset")) {
    return(top_support_rows(policy_rows, support_cutoff = as.numeric(policy_params$support_cutoff)))
  }
  if (identical(pool_name, "top_k_admissible")) {
    top_k <- as.integer(policy_params$k)
    if (!is.finite(top_k) || top_k < 1) {
      stop("'k' must be one integer >= 1 for top-k policies.", call. = FALSE)
    }
    return(
      tibble::as_tibble(policy_rows) |>
        dplyr::arrange(dplyr::desc(w_adm), combined_distance) |>
        dplyr::slice_head(n = top_k)
    )
  }
  if (identical(pool_name, "same_fao_area")) {
    return(dplyr::filter(policy_rows, overlap_same_fao_area))
  }
  if (identical(pool_name, "same_ocean_basin")) {
    return(dplyr::filter(policy_rows, overlap_same_ocean_basin))
  }

  stop(sprintf("Unsupported candidate pool: %s", pool_name), call. = FALSE)
}

#' Compute one nearest prediction
#'
#' @param rows Candidate-policy row table.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
nearest_prediction <- function(rows) {
  # Use combined distance as the primary ordering and taxonomic distance as a
  # secondary tiebreak when both are available.
  keep_rows <- valid_multiplier_rows(rows)

  # Fall back to combined distance alone when the evaluation table does not
  # carry an explicit taxonomic-distance column.
  if ("taxonomic_distance_to_anchor" %in% names(keep_rows)) {
    keep_rows <- keep_rows |>
      dplyr::arrange(taxonomic_distance_to_anchor, combined_distance)
  } else {
    keep_rows <- keep_rows |>
      dplyr::arrange(combined_distance)
  }

  if (nrow(keep_rows) == 0) {
    return(NA_real_)
  }

  as.numeric(keep_rows$biomass_multiplier_if_replace[[1]])
}

#' Compute one weighted prediction
#'
#' @param rows Candidate-policy row table.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
weighted_prediction <- function(rows) {
  # Normalize the final admissibility weights once, then compute the weighted
  # mean multiplier across the valid donor pool.
  keep_rows <- valid_multiplier_rows(rows) |>
    dplyr::filter(is.finite(w_adm), w_adm > 0)
  weights <- normalized_weights(keep_rows$w_adm)

  if (nrow(keep_rows) == 0 || length(weights) == 0) {
    return(NA_real_)
  }

  sum(weights * keep_rows$biomass_multiplier_if_replace)
}

#' Compute one arithmetic-mean prediction
#'
#' @param rows Candidate-policy row table.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
arithmetic_prediction <- function(rows) {
  # Use the straight arithmetic mean for unweighted ensemble policies.
  keep_rows <- valid_multiplier_rows(rows)

  if (nrow(keep_rows) == 0) {
    return(NA_real_)
  }

  mean(keep_rows$biomass_multiplier_if_replace, na.rm = TRUE)
}

#' Compute one median prediction
#'
#' @param rows Candidate-policy row table.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
median_prediction <- function(rows) {
  # Use the median multiplier to provide a more robust ensemble summary.
  keep_rows <- valid_multiplier_rows(rows)

  if (nrow(keep_rows) == 0) {
    return(NA_real_)
  }

  stats::median(keep_rows$biomass_multiplier_if_replace, na.rm = TRUE)
}

#' Compute one equal-weight prediction
#'
#' @param rows Candidate-policy row table.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
equal_prediction <- function(rows) {
  # Equal-weight mean is distinct in the registry even when its multiplier
  # summary matches the arithmetic mean.
  arithmetic_prediction(rows)
}

#' Compute TS at length
#'
#' @param slope Numeric slope value.
#' @param intercept Numeric intercept value.
#' @param lengths_cm Numeric length vector in centimeters.
#'
#' @return Numeric vector of TS values.
#'
#' @keywords internal
ts_at_length <- function(slope,
                         intercept,
                         lengths_cm) {
  # Evaluate the standardized TS-length model directly in log10-length space.
  as.numeric(slope) * log10(lengths_cm) + as.numeric(intercept)
}

#' Compute one weighted curve
#'
#' @param rows Candidate-policy row table.
#' @param lengths_cm Numeric length vector in centimeters.
#'
#' @return Numeric vector.
#'
#' @keywords internal
weighted_curve_prediction <- function(rows,
                                      lengths_cm) {
  # Blend valid donor curves using normalized admissibility weights so the
  # returned TS curve matches the weighted multiplier policy.
  keep_rows <- valid_curve_rows(rows) |>
    dplyr::filter(is.finite(w_adm), w_adm > 0)
  weights <- normalized_weights(keep_rows$w_adm)

  if (nrow(keep_rows) == 0 || length(weights) == 0) {
    return(rep(NA_real_, length(lengths_cm)))
  }

  curve_matrix <- vapply(
    seq_len(nrow(keep_rows)),
    function(i) ts_at_length(keep_rows$slope_len[[i]], keep_rows$intercept_len[[i]], lengths_cm),
    numeric(length(lengths_cm))
  )

  if (is.vector(curve_matrix)) {
    curve_matrix <- matrix(curve_matrix, ncol = 1)
  }

  as.numeric(curve_matrix %*% weights)
}

#' Compute one average curve
#'
#' @param rows Candidate-policy row table.
#' @param lengths_cm Numeric length vector in centimeters.
#'
#' @return Numeric vector.
#'
#' @keywords internal
average_curve_prediction <- function(rows,
                                     lengths_cm) {
  # Average the valid donor curves equally for the unweighted ensemble
  # policies and any explicit equal-weight scan policy.
  keep_rows <- valid_curve_rows(rows)

  if (nrow(keep_rows) == 0) {
    return(rep(NA_real_, length(lengths_cm)))
  }

  curve_matrix <- vapply(
    seq_len(nrow(keep_rows)),
    function(i) ts_at_length(keep_rows$slope_len[[i]], keep_rows$intercept_len[[i]], lengths_cm),
    numeric(length(lengths_cm))
  )

  if (is.vector(curve_matrix)) {
    curve_matrix <- matrix(curve_matrix, ncol = 1)
  }

  rowMeans(curve_matrix, na.rm = TRUE)
}

#' Compute one median curve
#'
#' @param rows Candidate-policy row table.
#' @param lengths_cm Numeric length vector in centimeters.
#'
#' @return Numeric vector.
#'
#' @keywords internal
median_curve_prediction <- function(rows,
                                    lengths_cm) {
  # Take the pointwise median across valid donor curves for robust ensemble
  # policies.
  keep_rows <- valid_curve_rows(rows)

  if (nrow(keep_rows) == 0) {
    return(rep(NA_real_, length(lengths_cm)))
  }

  curve_matrix <- vapply(
    seq_len(nrow(keep_rows)),
    function(i) ts_at_length(keep_rows$slope_len[[i]], keep_rows$intercept_len[[i]], lengths_cm),
    numeric(length(lengths_cm))
  )

  if (is.vector(curve_matrix)) {
    curve_matrix <- matrix(curve_matrix, ncol = 1)
  }

  apply(curve_matrix, 1, stats::median, na.rm = TRUE)
}

#' Compute one nearest curve
#'
#' @param rows Candidate-policy row table.
#' @param lengths_cm Numeric length vector in centimeters.
#'
#' @return Numeric vector.
#'
#' @keywords internal
nearest_curve_prediction <- function(rows,
                                     lengths_cm) {
  # Reuse the same nearest-row ordering as the nearest-model multiplier policy.
  keep_rows <- valid_curve_rows(rows) |>
    dplyr::arrange(taxonomic_distance_to_anchor, combined_distance)

  if (nrow(keep_rows) == 0) {
    return(rep(NA_real_, length(lengths_cm)))
  }

  ts_at_length(keep_rows$slope_len[[1]], keep_rows$intercept_len[[1]], lengths_cm)
}

#' Compute one policy prediction
#'
#' @param rows Candidate-policy row table.
#' @param policy_def One policy-definition list.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
policy_prediction <- function(rows,
                              policy_def) {
  # Dispatch to the requested aggregation method after the donor pool has
  # already been filtered for the selected policy.
  method_name <- as.character(policy_def$aggregation_method)[[1]]

  if (identical(method_name, "nearest_by_combined_distance")) {
    return(nearest_prediction(rows))
  }
  if (identical(method_name, "kernel_weighted_mean")) {
    return(weighted_prediction(rows))
  }
  if (identical(method_name, "arithmetic_mean")) {
    return(arithmetic_prediction(rows))
  }
  if (identical(method_name, "median")) {
    return(median_prediction(rows))
  }
  if (identical(method_name, "equal_weight_mean")) {
    return(equal_prediction(rows))
  }

  stop(sprintf("Unsupported aggregation method: %s", method_name), call. = FALSE)
}

#' Compute one policy curve
#'
#' @param rows Candidate-policy row table.
#' @param policy_def One policy-definition list.
#' @param lengths_cm Numeric length vector in centimeters.
#'
#' @return Numeric vector.
#'
#' @keywords internal
policy_curve <- function(rows,
                         policy_def,
                         lengths_cm) {
  # Use a curve aggregator that mirrors the multiplier-level policy family.
  method_name <- as.character(policy_def$aggregation_method)[[1]]

  if (identical(method_name, "nearest_by_combined_distance")) {
    return(nearest_curve_prediction(rows, lengths_cm))
  }
  if (identical(method_name, "kernel_weighted_mean")) {
    return(weighted_curve_prediction(rows, lengths_cm))
  }
  if (identical(method_name, "arithmetic_mean")) {
    return(average_curve_prediction(rows, lengths_cm))
  }
  if (identical(method_name, "median")) {
    return(median_curve_prediction(rows, lengths_cm))
  }
  if (identical(method_name, "equal_weight_mean")) {
    return(average_curve_prediction(rows, lengths_cm))
  }

  stop(sprintf("Unsupported aggregation method: %s", method_name), call. = FALSE)
}

#' Evaluate registered policies
#'
#' Evaluates one or more policies against the admissible candidate pool for one
#' anchor and returns benchmark-ready predictions.
#'
#' @param eval_obj Evaluation object returned by [evaluate_anchor_models()].
#' @param ordination_info Optional ordination-context list.
#' @param policies Optional character vector of policy names. `NULL` means use
#'   all policies from the registry.
#' @param policy_params Optional named list of per-policy parameter overrides.
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return A tibble with `policy`, `multiplier_pred`, and `n_models`.
#'
#' @export
evaluate_policies <- function(eval_obj,
                              ordination_info = NULL,
                              policies = NULL,
                              policy_params = list(),
                              policy_path = NULL) {
  # Validate the evaluation object and read the registry once before iterating
  # over the selected policy set.
  if (!is.list(eval_obj) || is.null(eval_obj$admissible_df) || !is.data.frame(eval_obj$admissible_df)) {
    stop("'eval_obj' must contain an 'admissible_df' data frame.", call. = FALSE)
  }

  registry <- read_policy_registry(policy_path = policy_path)
  policy_defs <- registry$policies
  policy_lookup <- stats::setNames(policy_defs, vapply(policy_defs, function(x) x$coded_name, character(1)))
  selected <- policies %||% names(policy_lookup)

  if (!is.character(selected) || any(!nzchar(selected))) {
    stop("'policies' must be NULL or a character vector of policy names.", call. = FALSE)
  }
  unknown <- setdiff(selected, names(policy_lookup))
  if (length(unknown) > 0) {
    stop(sprintf("Unknown policy name(s): %s", paste(unknown, collapse = ", ")), call. = FALSE)
  }

  context <- resolve_policy_context(ordination_info = ordination_info)
  admissible_rows <- tibble::as_tibble(eval_obj$admissible_df)

  # Evaluate each selected policy independently so the returned table is ready
  # for the benchmark layer without any additional reshaping.
  purrr::map_dfr(selected, function(policy_name) {
    policy_def <- policy_lookup[[policy_name]]
    params <- policy_parameters(policy_name, policy_def, policy_params = policy_params)
    donor_rows <- policy_rows(
      rows = admissible_rows,
      policy_def = policy_def,
      policy_params = params,
      ordination_info = context
    )

    tibble::tibble(
      policy = policy_name,
      multiplier_pred = policy_prediction(donor_rows, policy_def),
      n_models = as.integer(nrow(donor_rows)),
      policy_family = as.character(policy_def$policy_family)[[1]],
      candidate_pool = as.character(policy_def$candidate_pool)[[1]],
      aggregation_method = as.character(policy_def$aggregation_method)[[1]]
    )
  })
}

#' Predict one policy TS curve
#'
#' Predicts one length-specific TS curve for a selected policy using the same
#' donor pool and aggregation rule as [evaluate_policies()].
#'
#' @param policy Policy name.
#' @param eval_obj Evaluation object returned by [evaluate_anchor_models()].
#' @param lengths_cm Numeric length vector in centimeters.
#' @param ordination_info Optional ordination-context list.
#' @param policy_params Optional named list of per-policy parameter overrides.
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return Numeric vector.
#'
#' @export
predict_policy_curve <- function(policy,
                                 eval_obj,
                                 lengths_cm,
                                 ordination_info = NULL,
                                 policy_params = list(),
                                 policy_path = NULL) {
  # Validate the requested policy first so any unsupported name fails before
  # touching the donor rows or the curve data.
  if (!is.character(policy) || length(policy) != 1 || !nzchar(policy)) {
    stop("'policy' must be a single policy name.", call. = FALSE)
  }
  if (!is.numeric(lengths_cm) || any(!is.finite(lengths_cm)) || any(lengths_cm <= 0)) {
    stop("'lengths_cm' must be a numeric vector of positive finite values.", call. = FALSE)
  }
  if (!is.list(eval_obj) || is.null(eval_obj$admissible_df) || !is.data.frame(eval_obj$admissible_df)) {
    stop("'eval_obj' must contain an 'admissible_df' data frame.", call. = FALSE)
  }

  registry <- read_policy_registry(policy_path = policy_path)
  policy_lookup <- stats::setNames(registry$policies, vapply(registry$policies, function(x) x$coded_name, character(1)))

  if (!policy %in% names(policy_lookup)) {
    stop(sprintf("Unknown policy name: %s", policy), call. = FALSE)
  }

  context <- resolve_policy_context(ordination_info = ordination_info)
  policy_def <- policy_lookup[[policy]]
  params <- policy_parameters(policy, policy_def, policy_params = policy_params)
  donor_rows <- policy_rows(
    rows = eval_obj$admissible_df,
    policy_def = policy_def,
    policy_params = params,
    ordination_info = context
  )

  policy_curve(donor_rows, policy_def, lengths_cm)
}
