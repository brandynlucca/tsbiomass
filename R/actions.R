# Action-universe construction ---------------------------------------------

#' Normalize one realized donor footprint
#'
#' @keywords internal
#' @noRd
normalize_action_footprint <- function(rows,
                                       id_col = "model_id",
                                       weight_col = ".structural_weight") {
  rows <- tibble::as_tibble(rows)
  if (nrow(rows) == 0L) {
    return(tibble::tibble(donor_model_id = character(), donor_weight = numeric()))
  }
  if (!id_col %in% names(rows)) {
    stop("A realized action footprint requires the configured donor ID column.", call. = FALSE)
  }
  donor_ids <- as.character(rows[[id_col]])
  if (anyNA(donor_ids) || any(!nzchar(donor_ids))) {
    stop("A realized action footprint contains a missing donor ID.", call. = FALSE)
  }
  if (anyDuplicated(donor_ids)) {
    stop("A realized action footprint contains duplicate donor IDs.", call. = FALSE)
  }
  raw_weights <- if (weight_col %in% names(rows)) {
    suppressWarnings(as.numeric(rows[[weight_col]]))
  } else {
    rep(1, nrow(rows))
  }
  keep <- is.finite(raw_weights) & raw_weights > 0
  if (!any(keep)) {
    return(tibble::tibble(donor_model_id = character(), donor_weight = numeric()))
  }
  donor_ids <- donor_ids[keep]
  weights <- raw_weights[keep] / sum(raw_weights[keep])
  order_idx <- order(donor_ids, method = "radix")
  tibble::tibble(
    donor_model_id = donor_ids[order_idx],
    donor_weight = weights[order_idx]
  )
}

#' Encode a realized donor footprint without losing numerical precision
#'
#' @keywords internal
#' @noRd
encode_action_footprint <- function(footprint) {
  footprint <- tibble::as_tibble(footprint)
  if (nrow(footprint) == 0L) {
    return(NA_character_)
  }
  paste(
    paste0(
      as.character(footprint$donor_model_id),
      "@",
      sprintf("%.17g", as.numeric(footprint$donor_weight))
    ),
    collapse = ";"
  )
}

#' Construct the invariant identity of one realized action
#'
#' Policy labels and numerical equation coefficients are deliberately omitted.
#' Policies that produce the same donor footprint, weights, declared branch,
#' and aggregation operator form one realized-action equivalence class and
#' remain available through the alias table. The footprint already identifies
#' the donor equations; including their coefficients again would permit
#' coefficients to affect stable ordering. The aggregation operator is required
#' because two operators can transform the same donor set differently.
#'
#' @keywords internal
#' @noRd
realized_action_identity <- function(equation_branch_filter,
                                     footprint,
                                     aggregation_method,
                                     slope,
                                     intercept) {
  footprint_text <- encode_action_footprint(footprint)
  identity_operator <- if (nrow(footprint) == 1L) {
    "single_donor"
  } else {
    as.character(aggregation_method)
  }
  signature <- paste(
    "realized_action_v2_coefficient_free",
    as.character(equation_branch_filter),
    identity_operator,
    footprint_text,
    sep = "|"
  )
  list(
    action_signature = signature,
    action_id = paste0("action_", admissibility_audit_fingerprint(signature))
  )
}

#' Explain why a configured policy branch could not realize an action
#'
#' @keywords internal
#' @noRd
action_invalid_reason <- function(donor_rows,
                                  structural_rows,
                                  policy_def,
                                  slope,
                                  intercept) {
  method <- as.character(policy_def$aggregation_method %||% NA_character_)[[1]]
  if (identical(method, "random_draw")) {
    return("non_actionable_random_benchmark")
  }
  if (nrow(donor_rows) == 0L) {
    return("empty_configured_pool_or_slope_branch")
  }
  required_distance <- c(
    nearest_by_combined_distance = "combined_distance",
    nearest = "combined_distance",
    nearest_by_trait_gower_distance = "trait_gower_distance",
    nearest_by_survey_distance = "d_study",
    nearest_by_taxonomic_distance = "taxonomic_distance_to_anchor",
    nearest_by_species_distance = "d_species"
  )
  if (method %in% names(required_distance)) {
    field <- unname(required_distance[[method]])
    if (!field %in% names(donor_rows) ||
      !any(is.finite(suppressWarnings(as.numeric(donor_rows[[field]]))))) {
      return(paste0("required_distance_unavailable:", field))
    }
  }
  if (method %in% c("kernel_weighted_mean", "distance_weighted_mean") &&
    (!"w_adm" %in% names(donor_rows) ||
      !any(is.finite(donor_rows$w_adm) & donor_rows$w_adm > 0))) {
    return("no_positive_configured_weights")
  }
  if (nrow(structural_rows) == 0L) {
    return("no_contributing_donors")
  }
  if (!is.finite(slope) || !is.finite(intercept)) {
    return("nonfinite_realized_equation")
  }
  NA_character_
}

#' Enumerate exhaustive singleton and configured policy actions
#'
#' Builds every individually admissible donor action plus every action realized
#' by the configured policy grammar. Genuinely identical realized actions are
#' deduplicated for later fitting, while a separate alias table retains every
#' policy, donor scope, aggregation method, and slope branch that produced it.
#'
#' @param eval_obj One-anchor admissibility evaluation containing
#'   `admissible_df`.
#' @param anchor_row One-row target/anchor table.
#' @param policies Configured policy names.
#' @param policy_params Configured policy parameters, including slope classes.
#' @param policy_path Optional policy-registry path.
#' @param ordination_info Optional fitted ordination context.
#' @param execution_plan Optional prebuilt policy execution plan.
#'
#' @return A list containing canonical `actions`, one-row-per-alias `aliases`,
#'   `raw_actions`, `invalid_singleton_donors`, and
#'   `invalid_policy_branches`.
#'
#' @keywords internal
#' @noRd
enumerate_action_universe <- function(eval_obj,
                                      anchor_row,
                                      policies,
                                      policy_params = list(),
                                      policy_path = NULL,
                                      ordination_info = NULL,
                                      execution_plan = NULL) {
  if (!is.list(eval_obj) || !is.data.frame(eval_obj$admissible_df)) {
    stop("'eval_obj' must contain an 'admissible_df' data frame.", call. = FALSE)
  }
  anchor_row <- tibble::as_tibble(anchor_row)
  if (nrow(anchor_row) != 1L) {
    stop("'anchor_row' must contain exactly one row.", call. = FALSE)
  }
  anchor_id_col <- if ("model_id" %in% names(anchor_row)) "model_id" else "model_id_chr"
  anchor_id <- as.character(anchor_row[[anchor_id_col]][[1]])
  anchor_species <- if ("species_name" %in% names(anchor_row)) {
    as.character(anchor_row$species_name[[1]])
  } else {
    NA_character_
  }

  execution_plan <- execution_plan %||% build_policy_execution_plan(
    policies = policies,
    policy_params = policy_params,
    policy_path = policy_path
  )
  plan <- tibble::as_tibble(execution_plan$plan)
  gate_source <- if (is.data.frame(eval_obj$model_eval)) {
    tibble::as_tibble(eval_obj$model_eval)
  } else {
    tibble::as_tibble(eval_obj$admissible_df)
  }
  admissible_gate_rows <- gate_source |>
    dplyr::filter(.data$admissible %in% TRUE)
  if (!"model_id" %in% names(admissible_gate_rows)) {
    stop("Admissible action rows require 'model_id'.", call. = FALSE)
  }
  if (anyDuplicated(as.character(admissible_gate_rows$model_id))) {
    stop("Admissible action rows require unique donor model IDs.", call. = FALSE)
  }
  slope_values <- suppressWarnings(as.numeric(admissible_gate_rows$slope_len))
  intercept_values <- suppressWarnings(as.numeric(admissible_gate_rows$intercept_len))
  invalid_equation <- !is.finite(slope_values) | !is.finite(intercept_values)
  invalid_singleton_donors <- admissible_gate_rows[invalid_equation, , drop = FALSE] |>
    dplyr::transmute(
      anchor_model_id = anchor_id,
      anchor_species = anchor_species,
      donor_model_id = as.character(.data$model_id),
      donor_species = as.character(.data$species_name %||% NA_character_),
      invalid_reason = "nonfinite_standardized_equation"
    ) |>
    dplyr::arrange(.data$donor_model_id)
  # `admissible_df` is the policy-ready table and may already exclude models
  # with unusable equations. Audit those exclusions from `model_eval`, then use
  # the policy-ready table for action construction.
  admissible <- tibble::as_tibble(eval_obj$admissible_df) |>
    dplyr::filter(.data$admissible %in% TRUE) |>
    valid_equation_rows()
  admissible$.action_stable_key <- policy_stable_row_key(admissible)
  admissible <- admissible |>
    dplyr::arrange(.data$.action_stable_key)

  if (nrow(admissible) == 0L) {
    empty_actions <- tibble::tibble(
      anchor_model_id = character(), anchor_species = character(),
      action_id = character(), action_signature = character(),
      equation_branch_filter = character(), donor_ids = character(),
      donor_weights = character(), donor_footprint = character(),
      n_donors = integer(), policy_slope_len = numeric(),
      policy_intercept_len = numeric(), policy_aliases = character(),
      candidate_pools = character(), aggregation_methods = character(),
      n_policy_aliases = integer(), includes_exhaustive_singleton = logical()
    )
    empty_aliases <- tibble::tibble(
      anchor_model_id = character(), anchor_species = character(),
      action_id = character(), source_type = character(), policy = character(),
      policy_display = character(), policy_family = character(),
      candidate_pool = character(), aggregation_method = character(),
      equation_branch_filter = character()
    )
    invalid <- plan |>
      dplyr::transmute(
        anchor_model_id = anchor_id,
        anchor_species = anchor_species,
        policy = as.character(.data$policy),
        policy_display = as.character(.data$policy_display),
        candidate_pool = as.character(.data$candidate_pool),
        aggregation_method = as.character(.data$aggregation_method),
        equation_branch_filter = as.character(.data$equation_branch_filter),
        available_pool_rows = 0L,
        invalid_reason = "no_admissible_donors"
      ) |>
      dplyr::arrange(.data$policy, .data$equation_branch_filter)
    return(list(
      actions = empty_actions,
      aliases = empty_aliases,
      raw_actions = empty_aliases,
      invalid_singleton_donors = invalid_singleton_donors,
      invalid_policy_branches = invalid,
      execution_plan = plan
    ))
  }

  context <- resolve_policy_context(ordination_info)
  configured_branches <- unique(as.character(plan$equation_branch_filter))
  raw_rows <- list()
  invalid_rows <- list()
  raw_index <- 1L
  invalid_index <- 1L

  append_realized <- function(source_type,
                              policy,
                              policy_display,
                              policy_family,
                              candidate_pool,
                              aggregation_method,
                              branch,
                              structural_rows,
                              slope,
                              intercept) {
    footprint <- normalize_action_footprint(structural_rows)
    identity <- realized_action_identity(
      branch, footprint, aggregation_method, slope, intercept
    )
    tibble::tibble(
      anchor_model_id = anchor_id,
      anchor_species = anchor_species,
      action_id = identity$action_id,
      action_signature = identity$action_signature,
      source_type = source_type,
      policy = policy,
      policy_display = policy_display,
      policy_family = policy_family,
      candidate_pool = candidate_pool,
      aggregation_method = aggregation_method,
      equation_branch_filter = branch,
      donor_ids = paste(footprint$donor_model_id, collapse = ";"),
      donor_weights = paste(sprintf("%.17g", footprint$donor_weight), collapse = ";"),
      donor_footprint = encode_action_footprint(footprint),
      n_donors = nrow(footprint),
      policy_slope_len = as.numeric(slope),
      policy_intercept_len = as.numeric(intercept)
    )
  }

  # Exhaustive singleton actions are independent of whether a named policy
  # happens to choose the donor. Each donor is represented in the all-slope
  # branch and its scientifically compatible intrinsic branch, when configured.
  donor_branch <- classify_equation_branch(admissible)
  intrinsic_map <- c(fixed20 = "fixed20_only", free_slope = "free_slope_only")
  for (i in seq_len(nrow(admissible))) {
    branches <- character(0)
    if ("all" %in% configured_branches) {
      branches <- c(branches, "all")
    }
    intrinsic <- unname(intrinsic_map[donor_branch[[i]]])
    if (!is.na(intrinsic) && intrinsic %in% configured_branches) {
      branches <- c(branches, intrinsic)
    }
    for (branch in unique(branches)) {
      donor <- admissible[i, , drop = FALSE]
      donor$.structural_weight <- 1
      raw_rows[[raw_index]] <- append_realized(
        source_type = "exhaustive_singleton",
        policy = "exhaustive_singleton_frontier",
        policy_display = "Exhaustive admissible singleton",
        policy_family = "singleton_frontier",
        candidate_pool = "all_admissible",
        aggregation_method = "single_donor",
        branch = branch,
        structural_rows = donor,
        slope = donor$slope_len[[1]],
        intercept = donor$intercept_len[[1]]
      )
      raw_index <- raw_index + 1L
    }
  }

  branch_cache <- stats::setNames(
    lapply(configured_branches, function(branch) policy_branch_filter(admissible, branch)),
    configured_branches
  )
  for (i in seq_len(nrow(plan))) {
    plan_row <- plan[i, , drop = FALSE]
    policy_def <- plan_row$policy_def[[1]]
    params <- plan_row$policy_params[[1]]
    branch <- as.character(plan_row$equation_branch_filter[[1]])
    pool <- policy_rows(
      rows = branch_cache[[branch]],
      policy_def = policy_def,
      policy_params = params,
      ordination_info = context
    ) |>
      valid_equation_rows()
    summary_rows <- policy_summary_rows(pool, policy_def)
    structural_rows <- policy_structural_rows(pool, policy_def, summary_rows = summary_rows)
    equation <- policy_equation(pool, policy_def)
    slope <- as.numeric(equation$policy_slope_len[[1]])
    intercept <- as.numeric(equation$policy_intercept_len[[1]])
    invalid_reason <- action_invalid_reason(pool, structural_rows, policy_def, slope, intercept)
    if (!is.na(invalid_reason)) {
      invalid_rows[[invalid_index]] <- tibble::tibble(
        anchor_model_id = anchor_id,
        anchor_species = anchor_species,
        policy = as.character(plan_row$policy[[1]]),
        policy_display = as.character(plan_row$policy_display[[1]]),
        candidate_pool = as.character(plan_row$candidate_pool[[1]]),
        aggregation_method = as.character(plan_row$aggregation_method[[1]]),
        equation_branch_filter = branch,
        available_pool_rows = nrow(pool),
        invalid_reason = invalid_reason
      )
      invalid_index <- invalid_index + 1L
      next
    }
    raw_rows[[raw_index]] <- append_realized(
      source_type = "configured_policy",
      policy = as.character(plan_row$policy[[1]]),
      policy_display = as.character(plan_row$policy_display[[1]]),
      policy_family = as.character(plan_row$policy_family[[1]]),
      candidate_pool = as.character(plan_row$candidate_pool[[1]]),
      aggregation_method = as.character(plan_row$aggregation_method[[1]]),
      branch = branch,
      structural_rows = structural_rows,
      slope = slope,
      intercept = intercept
    )
    raw_index <- raw_index + 1L
  }

  raw_actions <- dplyr::bind_rows(raw_rows) |>
    dplyr::arrange(
      .data$anchor_model_id, .data$action_id, .data$source_type,
      .data$policy, .data$equation_branch_filter
    )
  aliases <- raw_actions |>
    dplyr::select(
      "anchor_model_id", "anchor_species", "action_id", "source_type",
      "policy", "policy_display", "policy_family", "candidate_pool",
      "aggregation_method", "equation_branch_filter"
    ) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$anchor_model_id, .data$action_id, .data$policy)
  actions <- raw_actions |>
    dplyr::group_by(
      .data$anchor_model_id, .data$anchor_species, .data$action_id,
      .data$action_signature, .data$equation_branch_filter,
      .data$donor_ids, .data$donor_weights, .data$donor_footprint,
      .data$n_donors, .data$policy_slope_len, .data$policy_intercept_len
    ) |>
    dplyr::summarise(
      policy_aliases = paste(sort(unique(.data$policy)), collapse = ";"),
      candidate_pools = paste(sort(unique(.data$candidate_pool)), collapse = ";"),
      aggregation_methods = paste(sort(unique(.data$aggregation_method)), collapse = ";"),
      n_policy_aliases = dplyr::n_distinct(.data$policy),
      includes_exhaustive_singleton = any(.data$source_type == "exhaustive_singleton"),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$anchor_model_id, .data$action_id)

  invalid_policy_branches <- dplyr::bind_rows(invalid_rows)
  if (nrow(invalid_policy_branches) == 0L) {
    invalid_policy_branches <- tibble::tibble(
      anchor_model_id = character(),
      anchor_species = character(),
      policy = character(),
      policy_display = character(),
      candidate_pool = character(),
      aggregation_method = character(),
      equation_branch_filter = character(),
      available_pool_rows = integer(),
      invalid_reason = character()
    )
  } else {
    invalid_policy_branches <- invalid_policy_branches |>
      dplyr::arrange(.data$anchor_model_id, .data$policy, .data$equation_branch_filter)
  }

  list(
    actions = actions,
    aliases = aliases,
    raw_actions = raw_actions,
    invalid_singleton_donors = invalid_singleton_donors,
    invalid_policy_branches = invalid_policy_branches,
    execution_plan = plan
  )
}
