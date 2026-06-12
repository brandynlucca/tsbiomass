#' Referee and Scorecard S7 Classes
#'
#' `Referee` is the post-prediction recommendation layer for the main
#' policy-transfer pipeline. It consumes a [PolicySelector], an optional
#' [PolicyLearner], and the resulting [PolicyPredictions] bundle from
#' [predict()] on the selector. It does not rerun a parallel recommendation
#' policy engine. Instead, it turns the already selected anchor-policy results
#' into a typed [Scorecard] object.
#'
#' `Scorecard` stores the anchor-facing recommendation outputs and diagnostics
#' produced from the selector's prediction bundle. This includes the selected
#' rows, the full interval table, consensus summaries, anchor audit tables,
#' coverage summaries, missingness summaries, and explicit component-status
#' rows when partial output is allowed.
#'
#' Typical use is:
#' - run the staged selector pipeline
#' - optionally attach a [PolicyLearner]
#' - call [predict()] on the selector
#' - pass that prediction bundle into `Referee`
#' - call [predict()] on `Referee` to return a [Scorecard]
#'
#' @examples
#' \dontrun{
#' selector <- as_policy_selector(candidates)
#' selector <- benchmark(selector)
#' selector <- calibrate_uncertainty(selector)
#' selector <- select_policies(selector)
#' learner <- as_policy_learner(selector)
#' learner <- crossfit(learner)
#' learner <- fit(learner)
#' learner <- calibrate_uncertainty(learner)
#' predictions <- predict(selector, learner = learner)
#' referee <- as_referee(selector, learner = learner, predictions = predictions)
#' scorecard <- predict(referee)
#' scorecard@selected
#' }
#'
#' @name Referee-class
#' @aliases Referee
NULL

#' Typed recommendation scorecard
#'
#' Stores the post-prediction recommendation tables and diagnostics built by
#' [predict()] on a [Referee].
#'
#' @examples
#' \dontrun{
#' scorecard <- predict(as_referee(selector, predictions = predictions))
#' scorecard@anchor_summary
#' scorecard@selection_diagnostics
#' }
#'
#' @name Scorecard-class
#' @aliases Scorecard
NULL

#' @rdname Scorecard-class
Scorecard <- S7::new_class(
  "Scorecard",
  properties = list(
    intervals = S7::new_property(CandidatesDataFrame),
    selected = S7::new_property(CandidatesDataFrame),
    ts_panel = S7::new_property(CandidatesDataFrame),
    recommendation_cards = S7::new_property(CandidatesDataFrame),
    surrogate_rules = S7::new_property(CandidatesDataFrame),
    consensus = S7::new_property(CandidatesDataFrame),
    anchor_summary = S7::new_property(CandidatesDataFrame),
    anchor_audit = S7::new_property(CandidatesDataFrame),
    species_coverage = S7::new_property(CandidatesDataFrame),
    selection_diagnostics = S7::new_property(CandidatesDataFrame),
    key_missing_overall = S7::new_property(CandidatesDataFrame),
    key_missing_by_field = S7::new_property(CandidatesDataFrame),
    key_missing_by_model = S7::new_property(CandidatesDataFrame),
    anchor_missing_gate = S7::new_property(CandidatesDataFrame),
    status = S7::new_property(CandidatesDataFrame)
  ),
  validator = function(self) {
    tryCatch(
      {
        required_status <- c("component", "status", "message")
        field_values <- list(
          intervals = self@intervals,
          selected = self@selected,
          ts_panel = self@ts_panel,
          recommendation_cards = self@recommendation_cards,
          surrogate_rules = self@surrogate_rules,
          consensus = self@consensus,
          anchor_summary = self@anchor_summary,
          anchor_audit = self@anchor_audit,
          species_coverage = self@species_coverage,
          selection_diagnostics = self@selection_diagnostics,
          key_missing_overall = self@key_missing_overall,
          key_missing_by_field = self@key_missing_by_field,
          key_missing_by_model = self@key_missing_by_model,
          anchor_missing_gate = self@anchor_missing_gate,
          status = self@status
        )
        for (field_name in names(field_values)) {
          if (!is.data.frame(field_values[[field_name]])) {
            return(sprintf("`%s` must be a data frame.", field_name))
          }
        }
        if (nrow(self@status) > 0 &&
          !all(required_status %in% names(self@status))) {
          return("`status` must contain 'component', 'status', and 'message' columns.")
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(Scorecard)

#' Create an empty `Scorecard`
#'
#' @return A `Scorecard` object with empty tables.
#'
#' @keywords internal
empty_scorecard <- function() {
  Scorecard(
    intervals = tibble::tibble(),
    selected = tibble::tibble(),
    ts_panel = tibble::tibble(),
    recommendation_cards = tibble::tibble(),
    surrogate_rules = tibble::tibble(),
    consensus = tibble::tibble(),
    anchor_summary = tibble::tibble(),
    anchor_audit = tibble::tibble(),
    species_coverage = tibble::tibble(),
    selection_diagnostics = tibble::tibble(),
    key_missing_overall = tibble::tibble(),
    key_missing_by_field = tibble::tibble(),
    key_missing_by_model = tibble::tibble(),
    anchor_missing_gate = tibble::tibble(),
    status = tibble::tibble(
      component = character(),
      status = character(),
      message = character()
    )
  )
}

#' @rdname Referee-class
Referee <- S7::new_class(
  "Referee",
  properties = list(
    selector = S7::new_property(S7::class_any),
    learner = S7::new_property(S7::class_any),
    predictions = S7::new_property(S7::class_any),
    config = S7::new_property(S7::class_list),
    scorecard = S7::new_property(Scorecard)
  ),
  validator = function(self) {
    tryCatch(
      {
        if (!(inherits(self@selector, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(self@selector, PolicySelector), error = function(e) FALSE)))) {
          return("`selector` must be a `PolicySelector` object.")
        }
        if (!is.null(self@learner) && !(inherits(self@learner, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(self@learner, PolicyLearner), error = function(e) FALSE)))) {
          return("`learner` must be NULL or a `PolicyLearner` object.")
        }
        if (!is.null(self@predictions) &&
          !isTRUE(tryCatch(S7::S7_inherits(self@predictions, PolicyPredictions), error = function(e) FALSE))) {
          return("`predictions` must be NULL or a `PolicyPredictions` object.")
        }
        if (!is.list(self@config)) {
          return("`config` must be a list.")
        }
        if (!isTRUE(tryCatch(S7::S7_inherits(self@scorecard, Scorecard), error = function(e) FALSE))) {
          return("`scorecard` must be a `Scorecard` object.")
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(Referee)

#' Test whether an object is a `Referee` instance
#'
#' Rebuild a `Referee`
#'
#' @param object A [Referee] object.
#' @param selector Optional replacement [PolicySelector].
#' @param learner Optional replacement [PolicyLearner].
#' @param predictions Optional replacement [PolicyPredictions].
#' @param config Optional replacement config list.
#' @param scorecard Optional replacement [Scorecard].
#'
#' @return A `Referee` object.
#'
#' @keywords internal
referee_rebuild <- function(object,
                            selector = object@selector,
                            learner = object@learner,
                            predictions = object@predictions,
                            config = object@config,
                            scorecard = object@scorecard) {
  Referee(
    selector = selector,
    learner = learner,
    predictions = predictions,
    config = config,
    scorecard = scorecard
  )
}

#' Build a `Referee`
#'
#' @param selector A [PolicySelector] object.
#' @param learner Optional [PolicyLearner] object used for selector prediction.
#' @param predictions Optional [PolicyPredictions] bundle. When omitted, the
#'   `Referee` can still be advanced later with [predict()] to compute it from
#'   the stored selector and learner.
#' @param config Optional config list or [Configurer] object.
#'
#' @return A `Referee` object.
#'
#' @examples
#' \dontrun{
#' predictions <- predict(selector, learner = learner)
#' referee <- as_referee(selector, learner = learner, predictions = predictions)
#' referee
#' }
#'
#' @export
create_referee <- function(selector,
                           learner = NULL,
                           predictions = NULL,
                           config = NULL) {
  if ((inherits(selector, "S7_object") && exists("Referee", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(selector, Referee), error = function(e) FALSE)))) {
    return(selector)
  }
  if (!(inherits(selector, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(selector, PolicySelector), error = function(e) FALSE)))) {
    stop("'selector' must be a `PolicySelector` object.", call. = FALSE)
  }
  if (!is.null(learner) && !(inherits(learner, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(learner, PolicyLearner), error = function(e) FALSE)))) {
    stop("'learner' must be NULL or a `PolicyLearner` object.", call. = FALSE)
  }
  if (!is.null(predictions) &&
    !isTRUE(tryCatch(S7::S7_inherits(predictions, PolicyPredictions), error = function(e) FALSE))) {
    stop("'predictions' must be NULL or a `PolicyPredictions` object.", call. = FALSE)
  }

  Referee(
    selector = selector,
    learner = learner,
    predictions = predictions,
    config = policy_selector_config_data(config),
    scorecard = empty_scorecard()
  )
}

#' Coerce selector state to `Referee`
#'
#' @param selector A [PolicySelector] object.
#' @param learner Optional [PolicyLearner] object.
#' @param predictions Optional [PolicyPredictions] object.
#' @param config Optional config list or [Configurer] object.
#'
#' @return A `Referee` object.
#'
#' @export
as_referee <- function(selector,
                       learner = NULL,
                       predictions = NULL,
                       config = NULL) {
  create_referee(
    selector = selector,
    learner = learner,
    predictions = predictions,
    config = config
  )
}

#' Validate prediction provenance for a `Referee`
#'
#' @param selector A [PolicySelector] object.
#' @param predictions A [PolicyPredictions] object.
#'
#' @return Invisibly returns `TRUE` when the prediction bundle is consistent
#'   with the selector's reference anchors and internal table relationships.
#'
#' @keywords internal
validate_referee_provenance <- function(selector,
                                        predictions) {
  if (!(inherits(selector, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(selector, PolicySelector), error = function(e) FALSE)))) {
    stop("'selector' must be a `PolicySelector` object.", call. = FALSE)
  }
  if (!isTRUE(tryCatch(S7::S7_inherits(predictions, PolicyPredictions), error = function(e) FALSE))) {
    stop("'predictions' must be a `PolicyPredictions` object.", call. = FALSE)
  }

  anchor_tbl <- tibble::as_tibble(selector@candidates@reference_anchors)
  selected_tbl <- tibble::as_tibble(predictions@selections)
  intervals_tbl <- tibble::as_tibble(predictions@intervals)
  consensus_tbl <- tibble::as_tibble(predictions@consensus)
  candidate_models <- tibble::as_tibble(selector@candidates@candidate_models)
  anchor_scores <- tibble::as_tibble((selector@candidates@admissibility)$all_scores %||% tibble::tibble())
  ordination_scores <- (selector@candidates@ordination)$model_scores %||% NULL
  ordination_species_lookup <- (selector@candidates@ordination)$species_lookup %||% NULL
  policy_cfg <- selector@config$policy %||% selector@config

  if (!all(c("anchor_model_id", "anchor_species") %in% names(selected_tbl))) {
    stop("`predictions@selections` must contain 'anchor_model_id' and 'anchor_species'.", call. = FALSE)
  }
  if (!all(c("anchor_model_id", "anchor_species") %in% names(consensus_tbl))) {
    stop("`predictions@consensus` must contain 'anchor_model_id' and 'anchor_species'.", call. = FALSE)
  }
  if (!all(c("anchor_model_id", "policy") %in% names(intervals_tbl))) {
    stop("`predictions@intervals` must contain 'anchor_model_id' and 'policy'.", call. = FALSE)
  }

  anchor_ids <- if ("model_id_chr" %in% names(anchor_tbl)) {
    as.character(anchor_tbl$model_id_chr)
  } else {
    as.character(anchor_tbl$model_id)
  }
  selected_anchor_ids <- unique(as.character(selected_tbl$anchor_model_id))
  consensus_anchor_ids <- unique(as.character(consensus_tbl$anchor_model_id))

  if (!setequal(selected_anchor_ids, anchor_ids)) {
    stop(
      "`predictions@selections` does not match the selector's reference-anchor ids.",
      call. = FALSE
    )
  }
  if (!setequal(consensus_anchor_ids, anchor_ids)) {
    stop(
      "`predictions@consensus` does not match the selector's reference-anchor ids.",
      call. = FALSE
    )
  }

  selected_keys <- selected_tbl |>
    dplyr::transmute(
      anchor_model_id = as.character(anchor_model_id),
      policy = if ("selected_policy" %in% names(selected_tbl)) {
        as.character(selected_policy)
      } else if ("policy" %in% names(selected_tbl)) {
        as.character(policy)
      } else {
        rep(NA_character_, dplyr::n())
      },
      equation_branch_filter = if ("selected_equation_branch_filter" %in% names(selected_tbl)) {
        dplyr::coalesce(selected_equation_branch_filter, equation_branch_filter)
      } else if ("equation_branch_filter" %in% names(selected_tbl)) {
        equation_branch_filter
      } else {
        rep(NA_character_, dplyr::n())
      }
    ) |>
    dplyr::filter(!is.na(policy), nzchar(policy))

  interval_keys <- intervals_tbl |>
    dplyr::transmute(
      anchor_model_id = as.character(anchor_model_id),
      policy = as.character(policy),
      equation_branch_filter = equation_branch_filter
    ) |>
    dplyr::distinct()

  missing_keys <- dplyr::anti_join(
    dplyr::distinct(selected_keys),
    interval_keys,
    by = intersect(names(selected_keys), names(interval_keys))
  )
  if (nrow(missing_keys) > 0) {
    stop(
      "Selected anchor-policy rows are missing from `predictions@intervals`.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Evaluate one `Referee` component
#'
#' @param component Component label.
#' @param expr Expression to evaluate.
#' @param allow_partial Logical scalar.
#' @param empty_value Empty fallback value used only when partial output is
#'   explicitly allowed.
#'
#' @return A list with `value` and one-row `status`.
#'
#' @keywords internal
referee_component <- function(component,
                              expr,
                              allow_partial = FALSE,
                              empty_value = tibble::tibble()) {
  tryCatch(
    {
      value <- force(expr)
      list(
        value = value,
        status = tibble::tibble(
          component = component,
          status = "ok",
          message = NA_character_
        )
      )
    },
    error = function(e) {
      if (!isTRUE(allow_partial)) {
        stop(
          sprintf("Referee component '%s' failed: %s", component, conditionMessage(e)),
          call. = FALSE
        )
      }
      warning(
        sprintf("Referee component '%s' failed: %s", component, conditionMessage(e)),
        call. = FALSE
      )
      list(
        value = empty_value,
        status = tibble::tibble(
          component = component,
          status = "partial_failure",
          message = conditionMessage(e)
        )
      )
    }
  )
}

scorecard_pick <- function(tbl,
                           candidates,
                           default = NA_real_) {
  n <- if (is.data.frame(tbl)) nrow(tbl) else 0L
  fallback <- rep(default, n)
  if (!is.data.frame(tbl) || length(candidates) == 0) {
    return(fallback)
  }
  for (candidate in candidates) {
    if (candidate %in% names(tbl)) {
      return(tbl[[candidate]])
    }
  }
  fallback
}

build_recommendation_cards <- function(selected_tbl,
                                       intervals_tbl,
                                       selection_diagnostics = tibble::tibble(),
                                       anchor_missing_gate = tibble::tibble()) {
  selected_tbl <- tibble::as_tibble(selected_tbl)
  intervals_tbl <- tibble::as_tibble(intervals_tbl)
  selection_diagnostics <- tibble::as_tibble(selection_diagnostics)
  anchor_missing_gate <- tibble::as_tibble(anchor_missing_gate)

  if (nrow(selected_tbl) == 0) {
    return(tibble::tibble())
  }

  width_values <- suppressWarnings(as.numeric(scorecard_pick(
    selected_tbl,
    c("meta_post_selection_interval_log_width", "interval_log_width")
  )))
  support_values <- suppressWarnings(as.numeric(scorecard_pick(
    selected_tbl,
    c("local_effective_support", "combined_local_effective_support")
  )))
  wide_width_cut <- if (sum(is.finite(width_values)) > 0) {
    stats::quantile(width_values[is.finite(width_values)], probs = 0.75, names = FALSE, na.rm = TRUE)
  } else {
    NA_real_
  }
  low_support_cut <- if (sum(is.finite(support_values)) > 0) {
    stats::quantile(support_values[is.finite(support_values)], probs = 0.25, names = FALSE, na.rm = TRUE)
  } else {
    NA_real_
  }

  runner_up_tbl <- tibble::tibble()
  if (nrow(intervals_tbl) > 0 && "anchor_model_id" %in% names(intervals_tbl)) {
    intervals_work <- intervals_tbl
    intervals_work$policy_display_resolved <- as.character(scorecard_pick(
      intervals_work,
      c("policy_display", "selected_policy_display", "policy")
    ))
    intervals_work$predicted_transfer_error <- suppressWarnings(as.numeric(scorecard_pick(
      intervals_work,
      c(".meta_predicted_score", "predicted_transfer_error", "species_median_abs_log_error")
    )))
    intervals_work$interval_log_width_resolved <- suppressWarnings(as.numeric(scorecard_pick(
      intervals_work,
      c("meta_post_selection_interval_log_width", "interval_log_width")
    )))
    intervals_work$local_distance_resolved <- suppressWarnings(as.numeric(scorecard_pick(
      intervals_work,
      c("local_min_combined_distance", "local_weighted_mean_combined_distance", "combined_distance")
    )))
    intervals_work$is_selected_resolved <- as.logical(scorecard_pick(
      intervals_work,
      c("is_selected"),
      default = FALSE
    ))
    runner_up_tbl <- intervals_work |>
      dplyr::filter(!isTRUE(is_selected_resolved)) |>
      dplyr::group_by(anchor_model_id) |>
      dplyr::arrange(
        predicted_transfer_error,
        interval_log_width_resolved,
        local_distance_resolved,
        .by_group = TRUE
      ) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        anchor_model_id = as.character(anchor_model_id),
        runner_up_policy = policy_display_resolved,
        runner_up_branch = if ("equation_branch_filter" %in% names(intervals_work)) {
          as.character(equation_branch_filter)
        } else {
          NA_character_
        },
        runner_up_predicted_transfer_error = predicted_transfer_error,
        runner_up_interval_log_width = interval_log_width_resolved,
        runner_up_local_distance = local_distance_resolved
      )
  }

  cards <- selected_tbl
  cards$anchor_model_id <- as.character(cards$anchor_model_id)
  cards$anchor_species <- as.character(scorecard_pick(cards, c("anchor_species", "species", "species_name")))
  cards$recommended_policy <- as.character(scorecard_pick(
    cards,
    c("selected_policy_display", "selected_policy", "policy_display", "policy")
  ))
  cards$recommended_policy_code <- as.character(scorecard_pick(cards, c("selected_policy", "policy")))
  cards$recommended_branch <- as.character(scorecard_pick(
    cards,
    c("selected_equation_branch_filter", "equation_branch_filter")
  ))
  cards$selection_tier <- as.character(scorecard_pick(cards, c("selection_tier")))
  cards$support_bin_code <- as.character(scorecard_pick(cards, c("post_selection_support_bin")))
  cards$support_bin <- as.character(scorecard_pick(
    cards,
    c("post_selection_support_label", "post_selection_support_bin")
  ))
  cards$biomass_multiplier <- suppressWarnings(as.numeric(scorecard_pick(cards, c("multiplier_pred"))))
  cards$biomass_multiplier_lo <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("meta_post_selection_multiplier_lo", "multiplier_lo")
  )))
  cards$biomass_multiplier_hi <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("meta_post_selection_multiplier_hi", "multiplier_hi")
  )))
  cards$predicted_transfer_error <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c(".meta_predicted_score", "predicted_transfer_error", "species_median_abs_log_error")
  )))
  cards$total_uncertainty_log <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("meta_q_abs_log_total", "q_abs_log_total")
  )))
  cards$uncertainty_budget_log <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("meta_q_abs_log_total", "q_abs_log_total")
  )))
  cards$interval_log_width <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("meta_post_selection_interval_log_width", "interval_log_width")
  )))
  cards$uncertainty_source <- as.character(scorecard_pick(
    cards,
    c("meta_uncertainty_source", "width_prediction_source", "uncertainty_prediction_source")
  ))
  cards$uncertainty_fallback <- as.logical(scorecard_pick(
    cards,
    c("meta_uncertainty_fallback"),
    default = FALSE
  ))
  cards$uncertainty_warning <- as.character(scorecard_pick(
    cards,
    c("meta_uncertainty_warning", "uncertainty_warning"),
    default = NA_character_
  ))
  cards$uncertainty_conformal_factor <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("meta_q_abs_log_conformal_factor")
  )))
  cards$uncertainty_bin_q_log <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("meta_q_abs_log")
  )))
  cards$local_effective_support <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("local_effective_support", "combined_local_effective_support")
  )))
  cards$local_distance <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("local_min_combined_distance", "local_weighted_mean_combined_distance", "combined_distance")
  )))
  cards$expected_length_cm <- suppressWarnings(as.numeric(scorecard_pick(cards, c("expected_length_cm"))))
  cards$length_support_min_cm <- suppressWarnings(as.numeric(scorecard_pick(cards, c("length_support_min_cm"))))
  cards$length_support_max_cm <- suppressWarnings(as.numeric(scorecard_pick(cards, c("length_support_max_cm"))))
  cards$policy_slope_len <- suppressWarnings(as.numeric(scorecard_pick(cards, c("policy_slope_len"))))
  cards$policy_intercept_len <- suppressWarnings(as.numeric(scorecard_pick(cards, c("policy_intercept_len"))))
  cards$policy_slope_len_lo_95 <- suppressWarnings(as.numeric(scorecard_pick(cards, c("policy_slope_len_lo_95"))))
  cards$policy_slope_len_hi_95 <- suppressWarnings(as.numeric(scorecard_pick(cards, c("policy_slope_len_hi_95"))))
  cards$policy_intercept_len_lo_95 <- suppressWarnings(as.numeric(scorecard_pick(cards, c("policy_intercept_len_lo_95"))))
  cards$policy_intercept_len_hi_95 <- suppressWarnings(as.numeric(scorecard_pick(cards, c("policy_intercept_len_hi_95"))))
  cards <- cards |>
    dplyr::select(
      anchor_model_id,
      anchor_species,
      recommended_policy,
      recommended_policy_code,
      recommended_branch,
      selection_tier,
      support_bin,
      support_bin_code,
      biomass_multiplier,
      biomass_multiplier_lo,
      biomass_multiplier_hi,
      predicted_transfer_error,
      total_uncertainty_log,
      uncertainty_budget_log,
      interval_log_width,
      uncertainty_source,
      uncertainty_fallback,
      uncertainty_warning,
      uncertainty_conformal_factor,
      uncertainty_bin_q_log,
      local_effective_support,
      local_distance,
      expected_length_cm,
      length_support_min_cm,
      length_support_max_cm,
      policy_slope_len,
      policy_intercept_len,
      policy_slope_len_lo_95,
      policy_slope_len_hi_95,
      policy_intercept_len_lo_95,
      policy_intercept_len_hi_95
    ) |>
    dplyr::left_join(runner_up_tbl, by = "anchor_model_id")

  if (nrow(selection_diagnostics) > 0 && "anchor_model_id" %in% names(selection_diagnostics)) {
    diag_selected <- selection_diagnostics
    if ("is_selected" %in% names(diag_selected)) {
      diag_selected <- diag_selected |>
        dplyr::filter(dplyr::coalesce(is_selected, FALSE))
    }
    diag_selected$species_oracle_best_policy <- as.character(scorecard_pick(
      diag_selected,
      c("species_oracle_best_policy")
    ))
    diag_selected$selected_delta_to_species_oracle <- suppressWarnings(as.numeric(scorecard_pick(
      diag_selected,
      c("selected_delta_to_species_oracle")
    )))
    cards <- cards |>
      dplyr::left_join(
        diag_selected |>
          dplyr::transmute(
            anchor_model_id = as.character(anchor_model_id),
            species_oracle_best_policy = species_oracle_best_policy,
            selected_delta_to_species_oracle = selected_delta_to_species_oracle
          ) |>
          dplyr::distinct(anchor_model_id, .keep_all = TRUE),
        by = "anchor_model_id"
      )
  }

  if (nrow(anchor_missing_gate) > 0 && "anchor_model_id" %in% names(anchor_missing_gate)) {
    anchor_missing_gate$n_candidates_total <- suppressWarnings(as.numeric(scorecard_pick(
      anchor_missing_gate,
      c("n_candidates_total")
    )))
    anchor_missing_gate$n_candidates_admissible <- suppressWarnings(as.numeric(scorecard_pick(
      anchor_missing_gate,
      c("n_candidates_admissible")
    )))
    anchor_missing_gate$prop_fail_missing_metadata <- suppressWarnings(as.numeric(scorecard_pick(
      anchor_missing_gate,
      c("prop_fail_missing_metadata")
    )))
    cards <- cards |>
      dplyr::left_join(
        anchor_missing_gate |>
          dplyr::transmute(
            anchor_model_id = as.character(anchor_model_id),
            n_candidates_total = n_candidates_total,
            n_candidates_admissible = n_candidates_admissible,
            prop_fail_missing_metadata = prop_fail_missing_metadata
          ),
        by = "anchor_model_id"
      )
  } else {
    cards$prop_fail_missing_metadata <- NA_real_
  }

  cards <- cards |>
    dplyr::mutate(
      recommendation_warning = vapply(
        seq_len(dplyr::n()),
        function(i) {
          values <- c(
            if (is.finite(cards$prop_fail_missing_metadata[[i]]) &&
              cards$prop_fail_missing_metadata[[i]] >= 0.5) {
              "high_missing_metadata"
            },
            if (is.finite(cards$local_effective_support[[i]]) &&
              is.finite(low_support_cut) &&
              cards$local_effective_support[[i]] <= low_support_cut) {
              "low_support"
            },
            if (is.finite(cards$interval_log_width[[i]]) &&
              is.finite(wide_width_cut) &&
              cards$interval_log_width[[i]] >= wide_width_cut) {
              "wide_interval"
            },
            if (is.finite(cards$expected_length_cm[[i]]) &&
              is.finite(cards$length_support_min_cm[[i]]) &&
              is.finite(cards$length_support_max_cm[[i]]) &&
              (cards$expected_length_cm[[i]] < cards$length_support_min_cm[[i]] ||
                cards$expected_length_cm[[i]] > cards$length_support_max_cm[[i]])) {
              "expected_length_outside_support"
            }
          )
          values <- unique(as.character(values))
          values <- values[!is.na(values) & nzchar(values) & values != "none"]
          if (length(values) == 0) {
            return("none")
          }
          paste(values, collapse = "; ")
        },
        character(1)
      ),
      recommended_action = dplyr::case_when(
        recommendation_warning == "high_missing_metadata" ~ "Resolve missing metadata before relying on the recommendation.",
        grepl("low_support", recommendation_warning, fixed = TRUE) ~ "Seek closer metadata-matched proxy models to improve support.",
        grepl("wide_interval", recommendation_warning, fixed = TRUE) ~ "Treat the multiplier as unstable and compare runner-up policies.",
        grepl("expected_length_outside_support", recommendation_warning, fixed = TRUE) ~ "Avoid extrapolating beyond the supported length range.",
        TRUE ~ "Use the selected policy as-is."
      )
    )

  cards
}

build_surrogate_rules <- function(object,
                                  selection_diagnostics = tibble::tibble()) {
  if (!(inherits(object@learner, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(object@learner, PolicyLearner), error = function(e) FALSE))) || !requireNamespace("rpart", quietly = TRUE)) {
    return(tibble::tibble())
  }

  source_tbl <- tibble::as_tibble(object@learner@calibration$selected %||% tibble::tibble())
  if (nrow(source_tbl) == 0) {
    source_tbl <- tibble::as_tibble(selection_diagnostics)
  }
  if (nrow(source_tbl) < 10) {
    return(tibble::tibble())
  }

  target_values <- as.character(scorecard_pick(
    source_tbl,
    c("selected_policy_display", "selected_policy", "policy_display", "policy")
  ))
  keep <- !is.na(target_values) & nzchar(target_values)
  source_tbl <- source_tbl[keep, , drop = FALSE]
  target_values <- target_values[keep]
  if (nrow(source_tbl) < 10 || length(unique(target_values)) < 2) {
    return(tibble::tibble())
  }

  feature_cols <- c(
    "local_effective_support",
    "local_min_combined_distance",
    "local_weighted_mean_combined_distance",
    "local_mean_length_overlap",
    "local_mean_depth_overlap",
    "n_valid_models",
    "local_n_same_species",
    "local_n_same_genus",
    "local_n_same_family",
    "post_selection_support_score",
    "meta_q_abs_log_total",
    "interval_log_width",
    "expected_length_cm"
  )
  feature_cols <- feature_cols[feature_cols %in% names(source_tbl)]
  feature_cols <- feature_cols[
    vapply(
      feature_cols,
      function(col_name) {
        values <- source_tbl[[col_name]]
        is.numeric(values) && length(unique(values[is.finite(values)])) > 1
      },
      logical(1)
    )
  ]
  if (length(feature_cols) == 0) {
    return(tibble::tibble())
  }

  tree_data <- source_tbl[, feature_cols, drop = FALSE]
  tree_data$.target <- factor(target_values)
  minbucket <- max(5L, floor(nrow(tree_data) * 0.05))
  surrogate_fit <- rpart::rpart(
    stats::as.formula(paste(".target ~", paste(feature_cols, collapse = " + "))),
    data = tree_data,
    method = "class",
    control = rpart::rpart.control(
      maxdepth = 3L,
      minbucket = minbucket,
      cp = 0.01,
      xval = 0L
    )
  )

  frame <- surrogate_fit$frame
  if (is.null(frame) || nrow(frame) == 0) {
    return(tibble::tibble())
  }

  leaf_rows <- which(frame$var == "<leaf>")
  if (length(leaf_rows) == 0) {
    return(tibble::tibble())
  }
  leaf_ids <- as.integer(row.names(frame)[leaf_rows])
  leaf_paths <- rpart::path.rpart(surrogate_fit, nodes = leaf_ids, print.it = FALSE)
  path_strings <- vapply(
    leaf_paths,
    function(path_vec) {
      if (length(path_vec) <= 1) {
        return("all rows")
      }
      paste(path_vec[-1], collapse = " & ")
    },
    character(1)
  )
  pred_levels <- surrogate_fit$ylevels %||% levels(tree_data$.target)
  pred_policy <- pred_levels[frame$yval[leaf_rows]]
  class_prob <- if (!is.null(frame$yval2)) {
    apply(frame$yval2[leaf_rows, , drop = FALSE], 1, max, na.rm = TRUE)
  } else {
    rep(NA_real_, length(leaf_rows))
  }

  importance_tbl <- if (!is.null(surrogate_fit$variable.importance)) {
    tibble::tibble(
      row_type = "importance",
      rank = seq_along(surrogate_fit$variable.importance),
      node_id = NA_integer_,
      predicted_policy = NA_character_,
      n_obs = NA_integer_,
      class_probability = NA_real_,
      rule = NA_character_,
      feature = names(surrogate_fit$variable.importance),
      importance = as.numeric(surrogate_fit$variable.importance)
    )
  } else {
    tibble::tibble()
  }

  dplyr::bind_rows(
    tibble::tibble(
      row_type = "rule",
      rank = seq_along(leaf_ids),
      node_id = leaf_ids,
      predicted_policy = as.character(pred_policy),
      n_obs = as.integer(frame$n[leaf_rows]),
      class_probability = suppressWarnings(as.numeric(class_prob)),
      rule = unname(path_strings),
      feature = NA_character_,
      importance = NA_real_
    ),
    importance_tbl
  ) |>
    dplyr::mutate(
      surrogate_model = "rpart",
      target = "selected_policy",
      training_rows = nrow(tree_data),
      feature_set = paste(feature_cols, collapse = ", ")
    ) |>
    dplyr::select(
      surrogate_model,
      target,
      row_type,
      rank,
      node_id,
      predicted_policy,
      n_obs,
      class_probability,
      rule,
      feature,
      importance,
      training_rows,
      feature_set
    )
}

#' Build a post-prediction `Scorecard`
#'
#' @param object A [Referee] object.
#' @param predictions A [PolicyPredictions] object.
#' @param allow_partial Logical scalar. If `FALSE`, any failed downstream
#'   component stops the build. If `TRUE`, failures are recorded in the
#'   returned `Scorecard` status table.
#'
#' @return A `Scorecard` object.
#'
#' @keywords internal
build_referee_scorecard <- function(object,
                                    predictions,
                                    allow_partial = FALSE) {
  selector <- object@selector
  predictions <- predictions %||% object@predictions
  if (is.null(predictions) ||
    !isTRUE(tryCatch(S7::S7_inherits(predictions, PolicyPredictions), error = function(e) FALSE))) {
    stop("`Referee` requires a `PolicyPredictions` bundle.", call. = FALSE)
  }
  validate_referee_provenance(selector, predictions)

  selected_tbl <- tibble::as_tibble(predictions@selections)
  intervals_tbl <- tibble::as_tibble(predictions@intervals)
  consensus_tbl <- tibble::as_tibble(predictions@consensus)
  candidate_models <- tibble::as_tibble(selector@candidates@candidate_models)
  anchor_scores <- tibble::as_tibble((selector@candidates@admissibility)$all_scores %||% tibble::tibble())
  ordination_scores <- (selector@candidates@ordination)$model_scores %||% NULL
  ordination_species_lookup <- (selector@candidates@ordination)$species_lookup %||% NULL
  policy_cfg <- selector@config$policy %||% selector@config

  if (!"selected_policy" %in% names(selected_tbl)) {
    selected_tbl$selected_policy <- dplyr::coalesce(
      selected_tbl$policy,
      if ("selected_policy_display" %in% names(selected_tbl)) selected_tbl$selected_policy_display else rep(NA_character_, nrow(selected_tbl))
    )
  }
  if (!"selected_equation_branch_filter" %in% names(selected_tbl)) {
    selected_tbl$selected_equation_branch_filter <- if ("equation_branch_filter" %in% names(selected_tbl)) {
      as.character(selected_tbl$equation_branch_filter)
    } else {
      rep(NA_character_, nrow(selected_tbl))
    }
  }
  if (!"selected_policy" %in% names(intervals_tbl)) {
    intervals_tbl$selected_policy <- dplyr::coalesce(
      intervals_tbl$policy,
      if ("policy_display" %in% names(intervals_tbl)) intervals_tbl$policy_display else rep(NA_character_, nrow(intervals_tbl))
    )
  }
  selected_tbl$selected_policy_display <- resolve_selected_policy_names(selected_tbl)
  intervals_tbl$policy_display <- resolve_policy_display_names(intervals_tbl)

  selected_keys <- selected_tbl |>
    dplyr::transmute(
      anchor_model_id,
      policy = if ("selected_policy" %in% names(selected_tbl)) {
        as.character(selected_policy)
      } else if ("policy" %in% names(selected_tbl)) {
        as.character(policy)
      } else {
        rep(NA_character_, dplyr::n())
      },
      equation_branch_filter = if ("selected_equation_branch_filter" %in% names(selected_tbl)) {
        as.character(selected_equation_branch_filter)
      } else if ("equation_branch_filter" %in% names(selected_tbl)) {
        as.character(equation_branch_filter)
      } else {
        rep(NA_character_, dplyr::n())
      },
      is_selected = TRUE
    ) |>
    dplyr::distinct()

  if (nrow(intervals_tbl) > 0 && all(c("anchor_model_id", "policy") %in% names(intervals_tbl))) {
    intervals_tbl <- intervals_tbl |>
      dplyr::left_join(
        selected_keys,
        by = intersect(
          c("anchor_model_id", "policy", "equation_branch_filter"),
          intersect(names(intervals_tbl), names(selected_keys))
        )
      )
    intervals_tbl$is_selected <- dplyr::coalesce(
      if ("is_selected.y" %in% names(intervals_tbl)) as.logical(intervals_tbl$is_selected.y) else rep(NA, nrow(intervals_tbl)),
      if ("is_selected" %in% names(intervals_tbl)) as.logical(intervals_tbl$is_selected) else rep(NA, nrow(intervals_tbl)),
      if ("is_selected.x" %in% names(intervals_tbl)) as.logical(intervals_tbl$is_selected.x) else rep(NA, nrow(intervals_tbl)),
      FALSE
    )
    intervals_tbl <- intervals_tbl |>
      dplyr::select(-dplyr::any_of(c("is_selected.x", "is_selected.y")))
  }

  selected_tbl <- augment_policy_coefficient_intervals(
    policy_tbl = selected_tbl,
    candidate_models = candidate_models,
    anchor_scores = anchor_scores,
    config = policy_cfg,
    model_scores = ordination_scores,
    species_lookup = ordination_species_lookup
  )
  selected_tbl <- augment_anchor_length_context(
    policy_tbl = selected_tbl,
    candidate_models = candidate_models
  )
  intervals_tbl <- augment_policy_coefficient_intervals(
    policy_tbl = intervals_tbl,
    candidate_models = candidate_models,
    anchor_scores = anchor_scores,
    config = policy_cfg,
    model_scores = ordination_scores,
    species_lookup = ordination_species_lookup,
    length_grid_n = 200L
  )
  intervals_tbl <- augment_anchor_length_context(
    policy_tbl = intervals_tbl,
    candidate_models = candidate_models,
    length_grid_n = 200L
  )
  ts_panel_tbl <- if (all(c("policy_slope_len", "policy_intercept_len") %in% names(selected_tbl))) {
    build_ts_conformal_panel_data(
      selected_tbl = selected_tbl,
      ts_calibration = tibble::tibble(),
      candidate_models = candidate_models,
      anchor_scores = anchor_scores,
      config = policy_cfg,
      model_scores = ordination_scores,
      species_lookup = ordination_species_lookup
    )
  } else {
    tibble::tibble()
  }

  anchor_summary <- selected_tbl |>
    dplyr::select(dplyr::any_of(c(
      "anchor_model_id",
      "anchor_species",
      "selected_policy",
      "selected_policy_display",
      "selected_equation_branch_filter",
      "selection_tier",
      "multiplier_pred",
      "multiplier_lo",
      "multiplier_hi",
      "interval_log_width",
      "expected_length_cm",
      "length_support_min_cm",
      "length_support_max_cm",
      "policy_slope_len",
      "policy_intercept_len",
      "policy_slope_len_lo_95",
      "policy_slope_len_hi_95",
      "policy_intercept_len_lo_95",
      "policy_intercept_len_hi_95"
    ))) |>
    dplyr::left_join(
      consensus_tbl |>
        dplyr::transmute(
          anchor_model_id,
          combined_consensus_multiplier = consensus_multiplier,
          combined_multiplier_q05 = multiplier_q05,
          combined_multiplier_q50 = multiplier_q50,
          combined_multiplier_q95 = multiplier_q95,
          local_support_mass = if ("local_support_mass" %in% names(consensus_tbl)) {
            local_support_mass
          } else {
            NA_real_
          },
          local_effective_support = if ("local_effective_support" %in% names(consensus_tbl)) {
            local_effective_support
          } else {
            NA_real_
          }
        ),
      by = "anchor_model_id"
    )

  species_coverage_result <- referee_component(
    "species_coverage",
    build_species_coverage(selector),
    allow_partial = allow_partial
  )
  anchor_audit_result <- referee_component(
    "anchor_audit",
    build_anchor_audit(predictions, selector = selector),
    allow_partial = allow_partial
  )
  key_missing_result <- referee_component(
    "key_missing",
    summarize_key_missing(selector),
    allow_partial = allow_partial,
    empty_value = list(
      overall = tibble::tibble(),
      by_field = tibble::tibble(),
      by_model = tibble::tibble()
    )
  )
  anchor_missing_gate_result <- referee_component(
    "anchor_missing_gate",
    summarize_missing_gate(selector),
    allow_partial = allow_partial
  )

  species_policy_performance_result <- referee_component(
    "species_policy_performance",
    summarize_species_policy_performance((selector@benchmark)$species_block_perf %||% tibble::tibble()),
    allow_partial = allow_partial
  )
  selection_source <- if ((inherits(object@learner, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(object@learner, PolicyLearner), error = function(e) FALSE)))) {
    "meta_policy_selection"
  } else {
    "deterministic_policy_selection"
  }
  selection_diagnostics_result <- referee_component(
    "selection_diagnostics",
    {
      species_policy_performance <- species_policy_performance_result$value
      if (nrow(species_policy_performance) == 0 || nrow(selected_tbl) == 0) {
        tibble::tibble()
      } else {
        compare_selected_policy_species_rank(
          species_policy_tbl = species_policy_performance,
          selected_tbl = selected_tbl,
          selection_source = selection_source
        )
      }
    },
    allow_partial = allow_partial
  )
  recommendation_cards_result <- referee_component(
    "recommendation_cards",
    build_recommendation_cards(
      selected_tbl = selected_tbl,
      intervals_tbl = intervals_tbl,
      selection_diagnostics = tibble::as_tibble(selection_diagnostics_result$value),
      anchor_missing_gate = tibble::as_tibble(anchor_missing_gate_result$value)
    ),
    allow_partial = allow_partial
  )
  surrogate_rules_result <- referee_component(
    "surrogate_rules",
    build_surrogate_rules(
      object = object,
      selection_diagnostics = tibble::as_tibble(selection_diagnostics_result$value)
    ),
    allow_partial = allow_partial
  )

  status_tbl <- dplyr::bind_rows(
    tibble::tibble(component = "intervals", status = "ok", message = NA_character_),
    tibble::tibble(component = "selected", status = "ok", message = NA_character_),
    tibble::tibble(component = "ts_panel", status = "ok", message = NA_character_),
    tibble::tibble(component = "consensus", status = "ok", message = NA_character_),
    tibble::tibble(component = "anchor_summary", status = "ok", message = NA_character_),
    species_coverage_result$status,
    anchor_audit_result$status,
    key_missing_result$status,
    anchor_missing_gate_result$status,
    species_policy_performance_result$status,
    selection_diagnostics_result$status,
    recommendation_cards_result$status,
    surrogate_rules_result$status
  )

  key_missing_value <- key_missing_result$value
  Scorecard(
    intervals = intervals_tbl,
    selected = selected_tbl,
    ts_panel = ts_panel_tbl,
    recommendation_cards = tibble::as_tibble(recommendation_cards_result$value),
    surrogate_rules = tibble::as_tibble(surrogate_rules_result$value),
    consensus = consensus_tbl,
    anchor_summary = anchor_summary,
    anchor_audit = tibble::as_tibble(anchor_audit_result$value),
    species_coverage = tibble::as_tibble(species_coverage_result$value),
    selection_diagnostics = tibble::as_tibble(selection_diagnostics_result$value),
    key_missing_overall = tibble::as_tibble(key_missing_value$overall %||% tibble::tibble()),
    key_missing_by_field = tibble::as_tibble(key_missing_value$by_field %||% tibble::tibble()),
    key_missing_by_model = tibble::as_tibble(key_missing_value$by_model %||% tibble::tibble()),
    anchor_missing_gate = tibble::as_tibble(anchor_missing_gate_result$value),
    status = status_tbl
  )
}

#' Build a Scorecard from a Referee
#'
#' Consumes a [PolicyPredictions] bundle produced by a [PolicySelector] and
#' builds the typed [Scorecard] report layer.
#'
#' @param object A [Referee] object.
#' @param predictions Optional [PolicyPredictions] override. When omitted, the
#'   stored predictions are used, or they are generated from the selector.
#' @param allow_partial Logical scalar. If `TRUE`, flagged partial scorecards are
#'   allowed when non-critical report components fail.
#'
#' @return A populated [Scorecard].
#'
#' Build a Scorecard from a Referee
#'
#' Consumes a [PolicyPredictions] bundle produced by a [PolicySelector] and
#' builds the typed [Scorecard] report layer.
#'
#' @param object A [Referee] object.
#' @param predictions Optional [PolicyPredictions] override. When omitted, the
#'   stored predictions are used, or they are generated from the selector.
#' @param allow_partial Logical scalar. If `TRUE`, flagged partial scorecards are
#'   allowed when non-critical report components fail.
#'
#' @return A populated [Scorecard].
#' @name predict.Referee
S7::method(predict_generic, Referee) <- function(object,
                                                 predictions = NULL,
                                                 allow_partial = FALSE,
                                                 progress = NULL) {
  cfg      <- merge_cfg(object@selector@config, list())
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = c("selection", "benchmark")) %||%
    FALSE

  prediction_bundle <- predictions %||% object@predictions
  if (is.null(prediction_bundle)) {
    prediction_bundle <- if ((inherits(object@learner, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(object@learner, PolicyLearner), error = function(e) FALSE)))) {
      stats::predict(object@selector, learner = object@learner, progress = progress)
    } else {
      stats::predict(object@selector, progress = progress)
    }
  }

  report_progress(progress, "[Referee] Building scorecard...")
  sc <- build_referee_scorecard(
    object = referee_rebuild(
      object,
      predictions = prediction_bundle
    ),
    predictions = prediction_bundle,
    allow_partial = allow_partial
  )
  report_progress(progress, "[Referee] Scorecard complete.")
  sc
}

#' Collapse one `Scorecard` to a console summary
#'
#' @param x A [Scorecard] object.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
scorecard_console_summary <- function(x) {
  selected_tbl <- tibble::as_tibble(x@selected)
  intervals_tbl <- tibble::as_tibble(x@intervals)
  consensus_tbl <- tibble::as_tibble(x@consensus)
  ts_panel_tbl <- tibble::as_tibble(x@ts_panel)
  recommendation_cards_tbl <- tibble::as_tibble(x@recommendation_cards)
  surrogate_rules_tbl <- tibble::as_tibble(x@surrogate_rules)
  status_tbl <- tibble::as_tibble(x@status)

  anchor_ids <- unique(c(
    if ("anchor_model_id" %in% names(selected_tbl)) as.character(selected_tbl$anchor_model_id) else character(0),
    if ("anchor_model_id" %in% names(consensus_tbl)) as.character(consensus_tbl$anchor_model_id) else character(0),
    if ("anchor_model_id" %in% names(intervals_tbl)) as.character(intervals_tbl$anchor_model_id) else character(0)
  ))
  anchor_ids <- anchor_ids[!is.na(anchor_ids) & nzchar(anchor_ids)]

  policy_values <- unique(c(
    if ("selected_policy" %in% names(selected_tbl)) as.character(selected_tbl$selected_policy) else character(0),
    if ("policy" %in% names(selected_tbl)) as.character(selected_tbl$policy) else character(0),
    if ("policy" %in% names(intervals_tbl)) as.character(intervals_tbl$policy) else character(0)
  ))
  policy_values <- policy_values[!is.na(policy_values) & nzchar(policy_values)]

  selected_policy_values <- if ("selected_policy_display" %in% names(selected_tbl)) {
    as.character(selected_tbl$selected_policy_display)
  } else if ("selected_policy" %in% names(selected_tbl)) {
    as.character(selected_tbl$selected_policy)
  } else if ("policy" %in% names(selected_tbl)) {
    as.character(selected_tbl$policy)
  } else {
    character(0)
  }
  selected_policy_values <- selected_policy_values[!is.na(selected_policy_values) & nzchar(selected_policy_values)]

  status_summary <- "none"
  failed_components <- character(0)
  if (nrow(status_tbl) > 0 && "status" %in% names(status_tbl)) {
    status_counts <- sort(table(as.character(status_tbl$status)), decreasing = TRUE)
    status_summary <- paste(
      paste0(names(status_counts), "=", as.integer(status_counts)),
      collapse = ", "
    )
    if (all(c("component", "status") %in% names(status_tbl))) {
      failed_components <- as.character(status_tbl$component[status_tbl$status != "ok"])
      failed_components <- unique(failed_components[!is.na(failed_components) & nzchar(failed_components)])
    }
  }

  cat("Scorecard\n")
  cat("  anchors: ", length(anchor_ids), "\n", sep = "")
  cat("  interval_rows: ", nrow(intervals_tbl), "\n", sep = "")
  cat("  selected_rows: ", nrow(selected_tbl), "\n", sep = "")
  cat("  ts_panel_rows: ", nrow(ts_panel_tbl), "\n", sep = "")
  cat("  recommendation_cards: ", nrow(recommendation_cards_tbl), "\n", sep = "")
  cat("  surrogate_rows: ", nrow(surrogate_rules_tbl), "\n", sep = "")
  cat("  consensus_rows: ", nrow(consensus_tbl), "\n", sep = "")
  cat("  policies_evaluated: ", preview_values(policy_values), "\n", sep = "")
  cat("  selected_policies: ", preview_values(selected_policy_values), "\n", sep = "")
  cat("  status: ", status_summary, "\n", sep = "")
  if (length(failed_components) > 0) {
    cat("  failed_components: ", preview_values(failed_components), "\n", sep = "")
  }
  invisible(x)
}

#' Print a `Scorecard`
#'
#' @name print.Scorecard
#'
#' @param x A [Scorecard] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
S7::method(print_generic, Scorecard) <- function(x, ...) {
  scorecard_console_summary(x)
}

#' Show a `Scorecard`
#'
#' @name show.Scorecard
#'
#' @param object A [Scorecard] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
S7::method(show_generic, Scorecard) <- function(object) {
  scorecard_console_summary(object)
}

#' Print a `Referee`
#'
#' @name print.Referee
#'
#' @param x A [Referee] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
S7::method(print_generic, Referee) <- function(x, ...) {
  cat("Referee\n")
  cat("  anchors: ", nrow(x@selector@candidates@reference_anchors), "\n", sep = "")
  cat("  prediction_ready: ", if (is.null(x@predictions)) "no" else "yes", "\n", sep = "")
  cat("  scorecard_ready: ", if (nrow(x@scorecard@status) == 0 && nrow(x@scorecard@selected) == 0) "no" else "yes", "\n", sep = "")
  cat("  selected_rows: ", nrow(x@scorecard@selected), "\n", sep = "")
  cat("  status_rows: ", nrow(x@scorecard@status), "\n", sep = "")
  invisible(x)
}

#' Show a `Referee`
#'
#' @name show.Referee
#'
#' @param object A [Referee] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
S7::method(show_generic, Referee) <- function(object) {
  print(object)
  invisible(object)
}

#' Plot a `Scorecard`
#'
#' Uses the package's S7 method on [base::plot()] so post-prediction report
#' tables can be visualized directly from the scorecard object.
#'
#' @name plot.Scorecard
#'
#' @param x A [Scorecard] object.
#' @param y Unused.
#' @param type Figure family to draw.
#' @param scale Output scale used for `type = "ts_length"`.
#' @param view Secondary plot selector used for
#'   `type = "selected_policy_counts"`.
#' @param anchor_model_id Optional anchor model ID used for
#'   `type = "ts_length_bands"` and `type = "strategy_competition"`.
#' @param anchor_species Optional anchor species used when `anchor_model_id` is
#'   not supplied.
#' @param show_top_candidate Logical scalar indicating whether the TS-length
#'   plots should overlay the top-ranked admissible candidate curve when it is
#'   available.
#' @param reference_label Reference label used for
#'   `type = "selected_intervals"`.
#' @param ... Unused additional arguments.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' scorecard <- predict(as_referee(selector, predictions = predictions))
#' plot(scorecard, type = "ts_length_conformal")
#' plot(scorecard, type = "coefficient_uncertainty")
#' plot(scorecard, type = "field_missingness")
#' }
S7::method(plot_generic, Scorecard) <- function(x,
                                                y = NULL,
                                                type = c(
                                                  "ts_length",
                                                  "ts_length_conformal",
                                                  "ts_length_multiplier",
                                                  "ts_length_bands",
                                                  "coefficient_uncertainty",
                                                  "selection_rank",
                                                  "selected_intervals",
                                                  "selected_multiplier_summary",
                                                  "selected_policy_counts",
                                                  "strategy_competition",
                                                  "field_missingness"
                                                ),
                                                scale = c("ts", "multiplier"),
                                                view = NULL,
                                                anchor_model_id = NULL,
                                                anchor_species = NULL,
                                                show_top_candidate = FALSE,
                                                reference_label = "Reference",
                                                ...) {
  type <- match.arg(type)
  scale <- match.arg(scale)

  if (identical(type, "ts_length")) {
    type <- if (identical(scale, "ts")) "ts_length_conformal" else "ts_length_multiplier"
  }

  if (identical(type, "ts_length_conformal")) {
    return(plot_ts_panel(x@ts_panel, show_top_candidate = show_top_candidate))
  }
  if (identical(type, "ts_length_multiplier")) {
    return(plot_multiplier_length_spectrum(x@ts_panel))
  }
  if (identical(type, "ts_length_bands")) {
    curve_tbl <- tibble::as_tibble(x@ts_panel)
    if (nrow(curve_tbl) == 0) {
      stop(
        "No TS panel is stored on this `Scorecard`. Run `predict(referee)` first.",
        call. = FALSE
      )
    }
    anchor_id_value <- if (!is.null(anchor_model_id)) as.character(anchor_model_id[[1]]) else NULL
    anchor_species_value <- if (!is.null(anchor_species)) as.character(anchor_species[[1]]) else NULL
    if (!is.null(anchor_id_value) && "anchor_model_id" %in% names(curve_tbl)) {
      curve_tbl <- curve_tbl |>
        dplyr::filter(as.character(.data$anchor_model_id) == anchor_id_value)
    }
    if (!is.null(anchor_species_value) && "anchor_species" %in% names(curve_tbl)) {
      curve_tbl <- curve_tbl |>
        dplyr::filter(as.character(.data$anchor_species) == anchor_species_value)
    }
    if (nrow(curve_tbl) == 0) {
      stop("No TS panel rows matched the requested anchor.", call. = FALSE)
    }
    if ("anchor_model_id" %in% names(curve_tbl)) {
      first_anchor_id <- as.character(curve_tbl$anchor_model_id[[1]])
      curve_tbl <- curve_tbl |>
        dplyr::filter(as.character(.data$anchor_model_id) == first_anchor_id)
    } else if ("anchor_species" %in% names(curve_tbl)) {
      first_anchor_species <- as.character(curve_tbl$anchor_species[[1]])
      curve_tbl <- curve_tbl |>
        dplyr::filter(as.character(.data$anchor_species) == first_anchor_species)
    }
    anchor_label <- if ("anchor_species" %in% names(curve_tbl)) {
      as.character(curve_tbl$anchor_species[[1]])
    } else {
      "Reference"
    }
    policy_label <- if ("selected_policy" %in% names(curve_tbl)) {
      as.character(curve_tbl$selected_policy[[1]])
    } else {
      "Selected policy"
    }
    band_tbl <- dplyr::bind_rows(
      curve_tbl |>
        dplyr::transmute(length_cm, ts_anchor, ts_top_candidate, ts_pred, band = "99%", ymin = ts_lo_99, ymax = ts_hi_99),
      curve_tbl |>
        dplyr::transmute(length_cm, ts_anchor, ts_top_candidate, ts_pred, band = "95%", ymin = ts_lo_95, ymax = ts_hi_95),
      curve_tbl |>
        dplyr::transmute(length_cm, ts_anchor, ts_top_candidate, ts_pred, band = "90%", ymin = ts_lo_90, ymax = ts_hi_90),
      curve_tbl |>
        dplyr::transmute(length_cm, ts_anchor, ts_top_candidate, ts_pred, band = "80%", ymin = ts_lo_80, ymax = ts_hi_80)
    )
    return(plot_ts_bands(
      band_tbl = band_tbl,
      curve_tbl = curve_tbl,
      anchor_label = anchor_label,
      policy_label = policy_label,
      show_top_candidate = show_top_candidate
    ))
  }
  if (identical(type, "coefficient_uncertainty")) {
    return(plot_policy_coefficients(x@selected))
  }
  if (identical(type, "selection_rank")) {
    return(plot_selected_policy_species_rank(x@selection_diagnostics))
  }
  if (identical(type, "selected_intervals")) {
    return(plot_selected_intervals(x@selected, reference_label = reference_label))
  }
  if (identical(type, "selected_multiplier_summary")) {
    plot_df <- tibble::as_tibble(x@selected)
    if (nrow(plot_df) == 0) {
      stop(
        "No selected-policy rows are stored on this `Scorecard`. Run `predict(referee)` first.",
        call. = FALSE
      )
    }

    plot_df$selected_policy_display <- resolve_selected_policy_names(plot_df)
    plot_df$multiplier_lo <- dplyr::coalesce(
      if ("meta_post_selection_multiplier_lo" %in% names(plot_df)) as.numeric(plot_df$meta_post_selection_multiplier_lo) else rep(NA_real_, nrow(plot_df)),
      if ("multiplier_lo" %in% names(plot_df)) as.numeric(plot_df$multiplier_lo) else rep(NA_real_, nrow(plot_df))
    )
    plot_df$multiplier_hi <- dplyr::coalesce(
      if ("meta_post_selection_multiplier_hi" %in% names(plot_df)) as.numeric(plot_df$meta_post_selection_multiplier_hi) else rep(NA_real_, nrow(plot_df)),
      if ("multiplier_hi" %in% names(plot_df)) as.numeric(plot_df$multiplier_hi) else rep(NA_real_, nrow(plot_df))
    )
    plot_df$support_tier <- if ("post_selection_support_label" %in% names(plot_df)) {
      as.character(plot_df$post_selection_support_label)
    } else if ("post_selection_support_bin" %in% names(plot_df)) {
      as.character(plot_df$post_selection_support_bin)
    } else {
      rep("not_available", nrow(plot_df))
    }
    plot_df <- plot_df |>
      dplyr::filter(
        !is.na(anchor_species),
        is.finite(multiplier_pred),
        is.finite(multiplier_lo),
        is.finite(multiplier_hi),
        multiplier_pred > 0,
        multiplier_lo > 0,
        multiplier_hi > 0
      ) |>
      dplyr::mutate(
        anchor_label = paste(anchor_species, selected_policy_display, sep = " | "),
        anchor_label = stats::reorder(anchor_label, multiplier_pred)
      )
    if (nrow(plot_df) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(
            title = "Meta-policy selected biomass multipliers",
            subtitle = "Required plotting fields were not available.",
            x = NULL,
            y = "Biomass multiplier"
          ) +
          ggplot2::theme_minimal(base_size = 11)
      )
    }

    return(
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          y = multiplier_pred,
          x = anchor_label,
          ymin = multiplier_lo,
          ymax = multiplier_hi,
          colour = support_tier
        )
      ) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey45") +
        ggplot2::geom_errorbar(width = 0.22, linewidth = 0.75, alpha = 0.85) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          y = "Biomass multiplier relative to reference anchor",
          x = NULL,
          colour = "Support tier",
          title = "Meta-policy selected biomass multipliers",
          subtitle = "Intervals are calibrated from cross-fitted meta-policy selected residuals."
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 9, colour = "grey35"))
    )
  }
  if (identical(type, "selected_policy_counts")) {
    view <- match.arg(view %||% "by_policy", c("by_policy", "by_anchor"))
    plot_df <- tibble::as_tibble(x@selected)
    if (nrow(plot_df) == 0) {
      stop(
        "No selected-policy rows are stored on this `Scorecard`. Run `predict(referee)` first.",
        call. = FALSE
      )
    }

    plot_df$selected_policy_display <- resolve_selected_policy_names(plot_df)

    if (identical(view, "by_policy")) {
      count_tbl <- plot_df |>
        dplyr::count(selected_policy_display, sort = TRUE) |>
        dplyr::mutate(selected_policy_display = stats::reorder(selected_policy_display, n))
      return(
        ggplot2::ggplot(
          count_tbl,
          ggplot2::aes(x = selected_policy_display, y = n)
        ) +
          ggplot2::geom_col(fill = "#5d6f89") +
          ggplot2::coord_flip() +
          ggplot2::labs(
            x = NULL,
            y = "Selected anchor-model count",
            title = "Selected policies"
          ) +
          ggplot2::theme_minimal(base_size = 12)
      )
    }

    count_tbl <- plot_df |>
      dplyr::count(anchor_species, selected_policy_display) |>
      dplyr::group_by(anchor_species) |>
      dplyr::mutate(anchor_total = sum(n)) |>
      dplyr::ungroup() |>
      dplyr::mutate(anchor_species = stats::reorder(anchor_species, anchor_total))
    return(
      ggplot2::ggplot(
        count_tbl,
        ggplot2::aes(x = anchor_species, y = n, fill = selected_policy_display)
      ) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::scale_x_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
        ggplot2::labs(
          x = NULL,
          y = "Selected tied rows",
          fill = "Selected policy",
          title = "Meta-policy selected policies by reference anchor"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
    )
  }
  if (identical(type, "strategy_competition")) {
    interval_tbl <- tibble::as_tibble(x@intervals)
    anchor_id_value <- if (!is.null(anchor_model_id)) as.character(anchor_model_id[[1]]) else NULL
    anchor_species_value <- if (!is.null(anchor_species)) as.character(anchor_species[[1]]) else NULL
    if (!is.null(anchor_id_value) && "anchor_model_id" %in% names(interval_tbl)) {
      interval_tbl <- interval_tbl |>
        dplyr::filter(as.character(.data$anchor_model_id) == anchor_id_value)
    }
    if (!is.null(anchor_species_value) && "anchor_species" %in% names(interval_tbl)) {
      interval_tbl <- interval_tbl |>
        dplyr::filter(as.character(.data$anchor_species) == anchor_species_value)
    }
    if (!is.null(anchor_id_value) || !is.null(anchor_species_value)) {
      anchor_label <- if (nrow(interval_tbl) > 0 && "anchor_species" %in% names(interval_tbl)) {
        as.character(interval_tbl$anchor_species[[1]])
      } else {
        reference_label
      }
      return(plot_all_intervals(interval_tbl = interval_tbl, reference_name = anchor_label))
    }
    return(plot_interval_panel(interval_tbl))
  }
  if (identical(type, "field_missingness")) {
    return(plot_field_missing(x@key_missing_by_field))
  }
  plot_field_missing(x@key_missing_by_field)
}

#' Plot a `Referee`
#'
#' Uses the package's S7 method on [base::plot()] so the integrated
#' recommendation-stage summaries can be drawn from the full referee object when
#' both selector context and scorecard tables are needed.
#'
#' @name plot.Referee
#'
#' @param x A [Referee] object.
#' @param y Unused.
#' @param type Figure family to draw.
#' @param view Secondary plot selector used for
#'   `type = "anchor_multiplier_summary"` and `type = "length_density"`.
#' @param anchor_model_id Optional anchor model ID used for anchor-specific
#'   diagnostics.
#' @param anchor_species Optional anchor species used to restrict
#'   `type = "strategy_competition"` to one reference.
#' @param reference_name Optional display label used when plotting one anchor's
#'   policy competition panel.
#' @param ... Unused additional arguments.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' referee <- as_referee(selector, predictions = predictions)
#' scorecard <- predict(referee)
#' referee <- referee_rebuild(referee, scorecard = scorecard)
#' plot(referee, type = "anchor_multiplier_summary")
#' plot(referee, type = "length_density", anchor_species = "Sardinops sagax")
#' }
S7::method(plot_generic, Referee) <- function(x,
                                              y = NULL,
                                              type = c(
                                                "anchor_multiplier_summary",
                                                "strategy_competition",
                                                "length_density"
                                              ),
                                              view = NULL,
                                              anchor_model_id = NULL,
                                              anchor_species = NULL,
                                              reference_name = NULL,
                                              ...) {
  type <- match.arg(type)
  if (nrow(x@scorecard@anchor_summary) == 0 && nrow(x@scorecard@intervals) == 0) {
    stop(
      "No scorecard results are stored on this `Referee`. Run `predict(referee)` and store the returned `Scorecard` first.",
      call. = FALSE
    )
  }

  if (identical(type, "anchor_multiplier_summary")) {
    view <- match.arg(view %||% "summary", c("summary", "strategy_competition"))
    candidate_scores <- if (length(x@selector@candidates@admissibility) == 0) {
      tibble::tibble()
    } else {
      (x@selector@candidates@admissibility)$all_scores %||% tibble::tibble()
    }
    if (identical(view, "strategy_competition")) {
      type <- "strategy_competition"
    } else {
      return(plot_anchor_summary(
        integrated_tbl = x@scorecard@anchor_summary,
        score_tbl = candidate_scores,
        interval_tbl = x@scorecard@intervals
      ))
    }
  }

  if (identical(type, "strategy_competition")) {
    interval_tbl <- tibble::as_tibble(x@scorecard@intervals)
    anchor_id_value <- if (!is.null(anchor_model_id)) as.character(anchor_model_id[[1]]) else NULL
    if (!is.null(anchor_id_value) && "anchor_model_id" %in% names(interval_tbl)) {
      interval_tbl <- interval_tbl |>
        dplyr::filter(as.character(.data$anchor_model_id) == anchor_id_value)
    }
    if (!is.null(anchor_species)) {
      interval_tbl <- interval_tbl |>
        dplyr::filter(.data$anchor_species == as.character(anchor_species[[1]]))
      return(plot_all_intervals(
        interval_tbl = interval_tbl,
        reference_name = reference_name %||% as.character(anchor_species[[1]])
      ))
    }
    return(plot_interval_panel(interval_tbl))
  }

  # These anchor diagnostics combine the scorecard's selected row with the
  # selector's admissible donor pool so the plotted support set matches the
  # final recommendation context exactly.
  if (identical(type, "length_density")) {
    selected_tbl <- tibble::as_tibble(x@scorecard@selected)
    if (nrow(selected_tbl) == 0) {
      stop(
        "No selected-policy rows are stored on this `Referee`. Run `predict(referee)` first.",
        call. = FALSE
      )
    }
    anchor_id_value <- if (!is.null(anchor_model_id)) as.character(anchor_model_id[[1]]) else NULL
    anchor_species_value <- if (!is.null(anchor_species)) as.character(anchor_species[[1]]) else NULL
    if (!is.null(anchor_id_value) && "anchor_model_id" %in% names(selected_tbl)) {
      selected_tbl <- selected_tbl |>
        dplyr::filter(as.character(.data$anchor_model_id) == anchor_id_value)
    }
    if (!is.null(anchor_species_value) && "anchor_species" %in% names(selected_tbl)) {
      selected_tbl <- selected_tbl |>
        dplyr::filter(as.character(.data$anchor_species) == anchor_species_value)
    }
    if (nrow(selected_tbl) == 0) {
      stop("No selected-policy rows matched the requested anchor.", call. = FALSE)
    }
    density_view <- match.arg(
      view %||% if (is.null(anchor_id_value) && is.null(anchor_species_value)) "panel" else "single",
      c("panel", "single")
    )
    candidate_models <- tibble::as_tibble(x@selector@candidates@candidate_models)
    id_col <- if ("model_id_chr" %in% names(candidate_models)) "model_id_chr" else "model_id"
    if (identical(density_view, "panel")) {
      panel_tbl <- purrr::map_dfr(unique(as.character(selected_tbl$anchor_model_id)), function(anchor_id_now) {
        one_selected <- selected_tbl |>
          dplyr::filter(as.character(.data$anchor_model_id) == anchor_id_now) |>
          dplyr::slice(1)
        anchor_row <- candidate_models |>
          dplyr::filter(as.character(.data[[id_col]]) == anchor_id_now) |>
          dplyr::slice(1)
        if (nrow(one_selected) == 0 || nrow(anchor_row) == 0) {
          return(tibble::tibble())
        }
        anchor_pdf <- anchor_pdf_from_row(anchor_row)
        if (nrow(anchor_pdf) == 0) {
          return(tibble::tibble())
        }
        anchor_pdf |>
          dplyr::mutate(
            anchor_model_id = anchor_id_now,
            anchor_species = as.character(one_selected$anchor_species[[1]])
          )
      })
      if (nrow(panel_tbl) == 0) {
        stop(
          "No anchor length-support metadata were available for the requested panel.",
          call. = FALSE
        )
      }
      return(plot_length_density_panel(panel_tbl))
    }

    selected_row <- selected_tbl |>
      dplyr::slice(1)
    anchor_id <- as.character(selected_row$anchor_model_id[[1]])
    anchor_label <- if ("anchor_species" %in% names(selected_row)) {
      as.character(selected_row$anchor_species[[1]])
    } else {
      anchor_id
    }
    anchor_row <- candidate_models |>
      dplyr::filter(as.character(.data[[id_col]]) == anchor_id) |>
      dplyr::slice(1)
    if (nrow(anchor_row) == 0) {
      stop("The requested anchor was not present in the selector candidate table.", call. = FALSE)
    }
    anchor_pdf <- anchor_pdf_from_row(anchor_row)
    if (nrow(anchor_pdf) == 0) {
      stop(
        "The requested anchor does not contain stored length-support metadata needed for this diagnostic plot.",
        call. = FALSE
      )
    }
    return(plot_length_density(anchor_pdf, anchor_label))
  }

  candidate_scores <- if (length(x@selector@candidates@admissibility) == 0) {
    tibble::tibble()
  } else {
    (x@selector@candidates@admissibility)$all_scores %||% tibble::tibble()
  }
  plot_anchor_summary(
    integrated_tbl = x@scorecard@anchor_summary,
    score_tbl = candidate_scores,
    interval_tbl = x@scorecard@intervals
  )
}


