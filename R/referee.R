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
#' - run the prepared selector pipeline
#' - optionally attach a [PolicyLearner]
#' - call [predict()] on the selector
#' - pass that prediction bundle into `Referee`
#' - call [predict()] on `Referee` to return a [Scorecard]
#'
#' @examples
#' \dontrun{
#' selector <- as_policyselector(candidates)
#' selector <- benchmark(selector)
#' selector <- calibrate_uncertainty(selector)
#' selector <- select_policies(selector)
#' learner <- as_policylearner(selector)
#' learner <- crossfit(learner)
#' learner <- fit(learner)
#' learner <- calibrate_uncertainty(learner)
#' predictions <- predict(selector, learner = learner)
#' referee <- as_referee(selector, learner = learner, predictions = predictions)
#' scorecard <- predict(referee)
#' scorecard
#' }
#'
#' @name Referee-class
#' @usage NULL
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
#' scorecard
#' scorecard
#' }
#'
#' @name Scorecard-class
#' @usage NULL
#' @aliases Scorecard
NULL

#' @export
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
#' @noRd
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

#' @export
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
        if (!is_s7_instance(self@selector, "PolicySelector")) {
          return("`selector` must be a `PolicySelector` object.")
        }
        if (!is.null(self@learner) &&
          !is_s7_instance(self@learner, "PolicyLearner") &&
          !inherits(self@learner, "tsb_shared_policy_learner")) {
          return("`learner` must be NULL, a `PolicyLearner` object, or a wrapped learner reference.")
        }
        if (!is.null(self@predictions) &&
          !is_s7_instance(self@predictions, "PolicyPredictions")) {
          return("`predictions` must be NULL or a `PolicyPredictions` object.")
        }
        if (!is.list(self@config)) {
          return("`config` must be a list.")
        }
        if (!is_s7_instance(self@scorecard, "Scorecard")) {
          return("`scorecard` must be a `Scorecard` object.")
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(Referee)

#' Rebuild a `Referee`
#'
#' Reconstructs a [Referee] object, optionally replacing one or more of its
#' component objects.
#'
#' @param object A [Referee] object.
#' Collapse a selector's distance learner onto its canonical shared reference
#'
#' `referee_rebuild()` is the point in the package where a `Referee` most
#' commonly gets reconstructed from independently-sourced pieces - including,
#' in principle, a `selector` whose `Alchemist` fit was reloaded from a cache
#' file in a different session than whatever else is being combined with it
#' here. `share_distance_learner()` already guarantees a single reference
#' within one continuous session; this is the complementary step for
#' artifacts built in separate sessions that share the same underlying fit
#' (same fingerprint) being brought together. See
#' `canonicalize_distance_learner()` for the (cheap, fingerprint-only)
#' comparison this relies on.
#'
#' @param selector A [PolicySelector] object, or anything else (returned
#'   unchanged).
#'
#' @return `selector`, with its stored distance learner swapped for the
#'   canonical shared reference if one was already registered this session.
#' @keywords internal
#' @noRd
canonicalize_referee_distance_learner <- function(selector) {
  if (!is_s7_instance(selector, "PolicySelector")) {
    return(selector)
  }
  gower <- selector@candidates@gower_distances
  if (!is.list(gower) || !"distance_learner" %in% names(gower)) {
    return(selector)
  }
  learner_now <- gower$distance_learner
  canonical <- canonicalize_distance_learner(learner_now)
  if (identical(canonical, learner_now)) {
    return(selector)
  }
  gower$distance_learner <- canonical
  selector@candidates@gower_distances <- gower
  selector
}

#' @param selector Optional replacement [PolicySelector].
#' @param learner Optional replacement [PolicyLearner].
#' @param predictions Optional replacement [PolicyPredictions].
#' @param config Optional replacement config list.
#' @param scorecard Optional replacement [Scorecard].
#'
#' @return A `Referee` object.
#'
#' @export
referee_rebuild <- function(object,
                            selector = NULL,
                            learner = NULL,
                            predictions = NULL,
                            config = NULL,
                            scorecard = NULL) {
  selector <- selector %||% object@selector
  selector <- canonicalize_referee_distance_learner(selector)
  predictions <- predictions %||% object@predictions
  learner_now <- resolve_policy_learner(learner %||% object@learner)
  if (!is.null(predictions) && is_s7_instance(learner_now, "PolicyLearner")) {
    # Re-slim so re-attaching the full learner here can't re-inflate it.
    learner_now <- slim_referee_learner(learner_now, keep_prediction_state = FALSE)
  }
  learner <- share_policy_learner(learner_now)
  config <- config %||% object@config
  scorecard <- scorecard %||% object@scorecard
  Referee(
    selector = selector,
    learner = learner,
    predictions = predictions,
    config = config,
    scorecard = scorecard
  )
}

#' Build a slim selector bundle for referee scoring
#'
#' @param selector A [PolicySelector] object.
#'
#' @return A reduced [PolicySelector] object that retains only the state needed
#'   for scorecard validation and construction.
#'
#' @keywords internal
#' @noRd
slim_referee_selector <- function(selector) {
  # Keep only the selector payload required once policy predictions already
  # exist, rather than retaining the full benchmark and candidate-workflow
  # state inside the Referee.
  candidates_now <- selector@candidates
  slim_candidates <- Candidates(
    spec = candidates_now@spec,
    study_db = tibble::tibble(),
    species_vector = candidates_now@species_vector,
    source_dbs = list(),
    species_db = tibble::tibble(),
    candidate_models = candidates_now@candidate_models,
    reference_anchors = candidates_now@reference_anchors,
    similarity_matrix = list(),
    gower_distances = list(),
    ordination = candidates_now@ordination,
    admissibility = list(
      all_scores = (candidates_now@admissibility %||% list())$all_scores %||% tibble::tibble(),
      anchor_failures = (candidates_now@admissibility %||% list())$anchor_failures %||% tibble::tibble()
    ),
    similarity_tuning = list()
  )

  policy_selector_rebuild(
    selector,
    candidates = slim_candidates,
    benchmark = list(
      policy_perf = (selector@benchmark %||% list())$policy_perf %||% tibble::tibble(),
      policy_ts_error = (selector@benchmark %||% list())$policy_ts_error %||% tibble::tibble(),
      species_block_perf = (selector@benchmark %||% list())$species_block_perf %||% tibble::tibble()
    ),
    uncertainty = list(
      conf_cal = (selector@uncertainty %||% list())$conf_cal %||% tibble::tibble(),
      species_sum = (selector@uncertainty %||% list())$species_sum %||% list()
    ),
    selection = list(
      final_ref = (selector@selection %||% list())$final_ref %||% tibble::tibble()
    )
  )
}

#' Build a slim learner bundle for referee scoring
#'
#' @param learner A [PolicyLearner] object.
#' @param keep_prediction_state Logical scalar. When `TRUE`, retain the fitted
#'   model and cross-fit payload; otherwise keep only calibration state.
#'
#' @return A reduced [PolicyLearner] object.
#'
#' @keywords internal
#' @noRd
slim_referee_learner <- function(learner,
                                 keep_prediction_state = FALSE) {
  # Keep only the calibration payload once selector predictions are already
  # materialized; otherwise preserve the state needed to score the learner.
  policy_learner_rebuild(
    learner,
    selector = NULL,
    training_data = tibble::tibble(),
    crossfit = if (isTRUE(keep_prediction_state)) learner@crossfit else list(),
    fitted_model = if (isTRUE(keep_prediction_state)) learner@fitted_model else list(),
    calibration = learner@calibration
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
as_referee <- function(selector,
                       learner = NULL,
                       predictions = NULL,
                       config = NULL) {
  if (!is_s7_instance(selector, "PolicySelector")) {
    stop("'selector' must be a `PolicySelector` object.", call. = FALSE)
  }
  if (!is.null(learner) &&
    !is_s7_instance(learner, "PolicyLearner")) {
    stop("'learner' must be NULL or a `PolicyLearner` object.", call. = FALSE)
  }
  if (!is.null(predictions) &&
    !is_s7_instance(predictions, "PolicyPredictions")) {
    stop("'predictions' must be NULL or a `PolicyPredictions` object.", call. = FALSE)
  }
  selector <- canonicalize_referee_distance_learner(selector)
  if (!is.null(predictions)) {
    # Once predictions exist, trim the retained selector and learner state so
    # the Referee does not duplicate the full upstream workflow payload.
    selector <- slim_referee_selector(selector)
    if (!is.null(learner)) {
      learner <- slim_referee_learner(learner, keep_prediction_state = FALSE)
    }
  }

  Referee(
    selector = selector,
    learner = share_policy_learner(learner),
    predictions = predictions,
    config = policy_selector_config_data(config),
    scorecard = empty_scorecard()
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
#' @noRd
validate_referee_provenance <- function(selector,
                                        predictions) {
  if (!is_s7_instance(selector, "PolicySelector")) {
    stop("'selector' must be a `PolicySelector` object.", call. = FALSE)
  }
  if (!is_s7_instance(predictions, "PolicyPredictions")) {
    stop("'predictions' must be a `PolicyPredictions` object.", call. = FALSE)
  }

  anchor_tbl <- tibble::as_tibble(selector@candidates@reference_anchors)
  selected_tbl <- tibble::as_tibble(predictions@selections)
  intervals_tbl <- tibble::as_tibble(predictions@intervals)
  consensus_tbl <- tibble::as_tibble(predictions@consensus)

  if (!all(c("anchor_model_id", "anchor_species") %in% names(selected_tbl)) &&
    nrow(selected_tbl) > 0) {
    stop("`predictions` must contain 'anchor_model_id' and 'anchor_species'.", call. = FALSE)
  }
  if (!all(c("anchor_model_id", "anchor_species") %in% names(consensus_tbl)) &&
    nrow(consensus_tbl) > 0) {
    stop("`predictions` must contain 'anchor_model_id' and 'anchor_species'.", call. = FALSE)
  }
  if (!all(c("anchor_model_id", "policy") %in% names(intervals_tbl)) &&
    nrow(intervals_tbl) > 0) {
    stop("`predictions` must contain 'anchor_model_id' and 'policy'.", call. = FALSE)
  }

  anchor_ids <- if ("model_id" %in% names(anchor_tbl)) {
    as.character(anchor_tbl$model_id)
  } else {
    as.character(anchor_tbl$model_id)
  }
  selected_anchor_ids <- unique(as.character(selected_tbl$anchor_model_id))
  consensus_anchor_ids <- unique(as.character(consensus_tbl$anchor_model_id))

  external_anchor_ids <- function(tbl, table_name) {
    if (!"anchor_is_external" %in% names(tbl)) {
      return(character(0))
    }
    marker <- as.logical(tbl$anchor_is_external)
    if (anyNA(marker)) {
      stop(
        sprintf("`predictions` has missing `anchor_is_external` values in %s.", table_name),
        call. = FALSE
      )
    }
    unique(as.character(tbl$anchor_model_id[marker]))
  }
  selected_external_ids <- external_anchor_ids(selected_tbl, "selections")
  consensus_external_ids <- external_anchor_ids(consensus_tbl, "consensus")
  interval_external_ids <- external_anchor_ids(intervals_tbl, "intervals")
  external_id_sets <- list(
    selections = selected_external_ids,
    consensus = consensus_external_ids,
    intervals = interval_external_ids
  )
  if (!all(vapply(
    external_id_sets,
    function(ids) setequal(ids, selected_external_ids),
    logical(1)
  ))) {
    stop(
      "`anchor_is_external` must identify the same external anchor IDs in intervals, selections, and consensus.",
      call. = FALSE
    )
  }
  external_ids <- unique(c(selected_external_ids, consensus_external_ids, interval_external_ids))
  if (length(intersect(external_ids, anchor_ids)) > 0L) {
    stop("`predictions` marks selector reference-anchor IDs as external.", call. = FALSE)
  }
  allowed_anchor_ids <- unique(c(anchor_ids, external_ids))

  if (!all(selected_anchor_ids %in% allowed_anchor_ids)) {
    stop(
      "`predictions` does not match the selector's reference-anchor ids.",
      call. = FALSE
    )
  }
  if (!all(consensus_anchor_ids %in% allowed_anchor_ids)) {
    stop(
      "`predictions` does not match the selector's reference-anchor ids.",
      call. = FALSE
    )
  }

  selected_keys <- selected_tbl |>
    dplyr::transmute(
      anchor_model_id = as.character(.data$anchor_model_id),
      policy = if ("selected_policy" %in% names(selected_tbl)) {
        as.character(.data$selected_policy)
      } else if ("policy" %in% names(selected_tbl)) {
        as.character(.data$policy)
      } else {
        rep(NA_character_, dplyr::n())
      },
      equation_branch_filter = if ("selected_equation_branch_filter" %in% names(selected_tbl)) {
        dplyr::coalesce(.data$selected_equation_branch_filter, .data$equation_branch_filter)
      } else if ("equation_branch_filter" %in% names(selected_tbl)) {
        .data$equation_branch_filter
      } else {
        rep(NA_character_, dplyr::n())
      }
    ) |>
    dplyr::filter(!is.na(.data$policy), nzchar(.data$policy))

  interval_keys <- intervals_tbl |>
    dplyr::transmute(
      anchor_model_id = as.character(.data$anchor_model_id),
      policy = as.character(.data$policy),
      equation_branch_filter = .data$equation_branch_filter
    ) |>
    dplyr::distinct()

  missing_keys <- dplyr::anti_join(
    dplyr::distinct(selected_keys),
    interval_keys,
    by = intersect(names(selected_keys), names(interval_keys))
  )
  if (nrow(missing_keys) > 0) {
    stop(
      "Selected anchor-policy rows are missing from `predictions`.",
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
#' @noRd
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

#' Pick the first available scorecard column
#'
#' @param tbl Data frame to inspect.
#' @param candidates Ordered vector of candidate column names.
#' @param default Fallback value used when no candidate column exists.
#'
#' @return Vector aligned to the number of rows in `tbl`.
#'
#' @keywords internal
#' @noRd
scorecard_pick <- function(tbl,
                           candidates,
                           default = NA_real_) {
  # Return the first matching column so scorecard builders can tolerate
  # slightly different upstream naming conventions.
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

#' Normalize scorecard multiplier interval columns
#'
#' @param tbl Data frame.
#'
#' @return Tibble with normalized multiplier interval columns.
#'
#' @keywords internal
#' @noRd
scorecard_normalize_multiplier_columns <- function(tbl) {
  tbl <- tibble::as_tibble(tbl)
  if (nrow(tbl) == 0) {
    return(tbl)
  }

  multiplier_pred <- suppressWarnings(as.numeric(scorecard_pick(
    tbl,
    c("multiplier_pred")
  )))
  multiplier_lo <- suppressWarnings(as.numeric(scorecard_pick(
    tbl,
    c("multiplier_lo")
  )))
  multiplier_hi <- suppressWarnings(as.numeric(scorecard_pick(
    tbl,
    c("multiplier_hi")
  )))
  interval_log_width <- suppressWarnings(as.numeric(scorecard_pick(
    tbl,
    c("interval_log_width")
  )))

  can_derive_width <- is.finite(multiplier_lo) &
    is.finite(multiplier_hi) &
    multiplier_lo > 0 &
    multiplier_hi > 0
  interval_log_width[!is.finite(interval_log_width) & can_derive_width] <-
    log(multiplier_hi[!is.finite(interval_log_width) & can_derive_width] /
      multiplier_lo[!is.finite(interval_log_width) & can_derive_width])

  tbl$multiplier_pred <- multiplier_pred
  tbl$multiplier_lo <- multiplier_lo
  tbl$multiplier_hi <- multiplier_hi
  tbl$interval_log_width <- interval_log_width

  tbl
}

#' Build recommendation cards from selected policies
#'
#' @param selected_tbl Selected-policy summary table.
#' @param intervals_tbl Full interval table.
#' @param selection_diagnostics Optional selection-diagnostics table.
#' @param anchor_missing_gate Optional missing-metadata summary table.
#'
#' @return Tibble of recommendation cards.
#'
#' @keywords internal
#' @noRd
build_recommendation_cards <- function(selected_tbl,
                                       intervals_tbl,
                                       selection_diagnostics = tibble::tibble(),
                                       anchor_missing_gate = tibble::tibble()) {
  # Standardize the incoming tables before deriving human-facing card fields.
  selected_tbl_ <- tibble::as_tibble(selected_tbl)
  intervals_tbl_ <- tibble::as_tibble(intervals_tbl)
  selection_diagnostics_ <- tibble::as_tibble(selection_diagnostics)
  anchor_missing_gate_ <- tibble::as_tibble(anchor_missing_gate)

  if (nrow(selected_tbl_) == 0) {
    return(tibble::tibble())
  }

  # Derive the warning cut points used to annotate wide or weakly supported recommendations.
  width_values <- suppressWarnings(as.numeric(scorecard_pick(
    selected_tbl_,
    c("interval_log_width")
  )))
  support_values <- suppressWarnings(as.numeric(scorecard_pick(
    selected_tbl_,
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

  # Recover the best non-selected runner-up policy for each anchor where available.
  runner_up_tbl <- tibble::tibble()
  if (nrow(intervals_tbl_) > 0 && "anchor_model_id" %in% names(intervals_tbl_)) {
    intervals_work <- intervals_tbl_
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
      c("interval_log_width")
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
      dplyr::filter(!isTRUE(.data$is_selected_resolved)) |>
      dplyr::group_by(.data$anchor_model_id) |>
      dplyr::arrange(
        .data$predicted_transfer_error,
        .data$interval_log_width_resolved,
        .data$local_distance_resolved,
        .by_group = TRUE
      ) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        anchor_model_id = as.character(.data$anchor_model_id),
        runner_up_policy = .data$policy_display_resolved,
        runner_up_branch = if ("equation_branch_filter" %in% names(intervals_work)) {
          as.character(.data$equation_branch_filter)
        } else {
          NA_character_
        },
        runner_up_predicted_transfer_error = .data$predicted_transfer_error,
        runner_up_interval_log_width = .data$interval_log_width_resolved,
        runner_up_local_distance = .data$local_distance_resolved
      )
  }

  # Materialize the main recommendation card fields from the selected rows.
  cards <- selected_tbl_
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
    c("multiplier_lo")
  )))
  cards$biomass_multiplier_hi <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("multiplier_hi")
  )))
  cards$predicted_transfer_error <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c(".meta_predicted_score", "predicted_transfer_error", "species_median_abs_log_error")
  )))
  cards$total_uncertainty_log <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("q_abs_log_total", "q_abs_log_conformal", "q_abs_log")
  )))
  cards$uncertainty_budget_log <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("q_abs_log_total", "q_abs_log_conformal", "q_abs_log")
  )))
  cards$interval_log_width <- suppressWarnings(as.numeric(scorecard_pick(
    cards,
    c("interval_log_width")
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
      "anchor_model_id",
      "anchor_species",
      "recommended_policy",
      "recommended_policy_code",
      "recommended_branch",
      "selection_tier",
      "support_bin",
      "support_bin_code",
      "biomass_multiplier",
      "biomass_multiplier_lo",
      "biomass_multiplier_hi",
      "predicted_transfer_error",
      "total_uncertainty_log",
      "uncertainty_budget_log",
      "interval_log_width",
      "uncertainty_source",
      "uncertainty_fallback",
      "uncertainty_warning",
      "uncertainty_conformal_factor",
      "uncertainty_bin_q_log",
      "local_effective_support",
      "local_distance",
      "expected_length_cm",
      "length_support_min_cm",
      "length_support_max_cm",
      "policy_slope_len",
      "policy_intercept_len",
      "policy_slope_len_lo_95",
      "policy_slope_len_hi_95",
      "policy_intercept_len_lo_95",
      "policy_intercept_len_hi_95"
    ) |>
    dplyr::left_join(runner_up_tbl, by = "anchor_model_id")

  # Attach optional diagnostics describing species-oracle and missingness context.
  if (nrow(selection_diagnostics_) > 0 && "anchor_model_id" %in% names(selection_diagnostics_)) {
    diag_selected <- selection_diagnostics_
    if ("is_selected" %in% names(diag_selected)) {
      diag_selected <- diag_selected |>
        dplyr::filter(dplyr::coalesce(.data$is_selected, FALSE))
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
            anchor_model_id = as.character(.data$anchor_model_id),
            species_oracle_best_policy = .data$species_oracle_best_policy,
            selected_delta_to_species_oracle = .data$selected_delta_to_species_oracle
          ) |>
          dplyr::distinct(.data$anchor_model_id, .keep_all = TRUE),
        by = "anchor_model_id"
      )
  }

  if (nrow(anchor_missing_gate_) > 0 && "anchor_model_id" %in% names(anchor_missing_gate_)) {
    anchor_missing_gate_$n_candidates_total <- suppressWarnings(as.numeric(scorecard_pick(
      anchor_missing_gate_,
      c("n_candidates_total")
    )))
    anchor_missing_gate_$n_candidates_admissible <- suppressWarnings(as.numeric(scorecard_pick(
      anchor_missing_gate_,
      c("n_candidates_admissible")
    )))
    anchor_missing_gate_$prop_fail_missing_metadata <- suppressWarnings(as.numeric(scorecard_pick(
      anchor_missing_gate_,
      c("prop_fail_missing_metadata")
    )))
    cards <- cards |>
      dplyr::left_join(
        anchor_missing_gate_ |>
          dplyr::transmute(
            anchor_model_id = as.character(.data$anchor_model_id),
            n_candidates_total = .data$n_candidates_total,
            n_candidates_admissible = .data$n_candidates_admissible,
            prop_fail_missing_metadata = .data$prop_fail_missing_metadata
          ),
        by = "anchor_model_id"
      )
  } else {
    cards$prop_fail_missing_metadata <- NA_real_
  }

  # Collapse the warning flags into a user-facing recommendation action.
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

#' Build surrogate decision rules from learner output
#'
#' @param object A [Referee] object.
#' @param selection_diagnostics Optional selection-diagnostics table.
#'
#' @return Tibble describing surrogate rules, or an empty tibble.
#'
#' @keywords internal
#' @noRd
build_surrogate_rules <- function(object,
                                  selection_diagnostics = tibble::tibble()) {
  learner_now <- resolve_policy_learner(object@learner)
  # Refuse surrogate modeling unless the learner exists and the tree backend is available.
  if (!is_s7_instance(learner_now, "PolicyLearner") ||
    !requireNamespace("rpart", quietly = TRUE)) {
    return(tibble::tibble())
  }

  # Prefer learner calibration rows and fall back to explicit diagnostics when needed.
  source_tbl <- tibble::as_tibble(learner_now@calibration$selected %||% tibble::tibble())
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

  # Restrict the surrogate fit to interpretable local-support and geometry features.
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
      "surrogate_model",
      "target",
      "row_type",
      "rank",
      "node_id",
      "predicted_policy",
      "n_obs",
      "class_probability",
      "rule",
      "feature",
      "importance",
      "training_rows",
      "feature_set"
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
#' @noRd
build_referee_scorecard <- function(object,
                                    predictions,
                                    allow_partial = FALSE,
                                    progress = NULL) {
  selector <- object@selector
  predictions_ <- predictions %||% object@predictions
  if (is.null(predictions_) ||
    !is_s7_instance(predictions_, "PolicyPredictions")) {
    stop("`Referee` requires a `PolicyPredictions` bundle.", call. = FALSE)
  }
  validate_referee_provenance(selector, predictions_)

  selected_tbl <- tibble::as_tibble(predictions_@selections)
  intervals_tbl <- tibble::as_tibble(predictions_@intervals)
  consensus_tbl <- tibble::as_tibble(predictions_@consensus)
  selected_tbl <- scorecard_normalize_multiplier_columns(selected_tbl)
  intervals_tbl <- scorecard_normalize_multiplier_columns(intervals_tbl)
  candidate_models <- tibble::as_tibble(selector@candidates@candidate_models)
  anchor_scores <- tibble::as_tibble((selector@candidates@admissibility)$all_scores %||% tibble::tibble())
  ordination_context <- policy_selector_ordination_context(
    selector@candidates@ordination
  )
  ordination_scores <- ordination_context$model_scores
  ordination_species_lookup <- ordination_context$species_lookup
  policy_cfg <- selector@config$policy %||% selector@config
  learner_now <- resolve_policy_learner(object@learner)
  learner_selected_calibration <- if (is_s7_instance(
    learner_now,
    "PolicyLearner"
  )) {
    tibble::as_tibble(learner_now@calibration$selected %||% tibble::tibble())
  } else {
    tibble::tibble()
  }
  selected_policy_keys <- selected_tbl |>
    dplyr::transmute(
      policy = dplyr::coalesce(
        if ("selected_policy" %in% names(selected_tbl)) as.character(.data$selected_policy) else NA_character_,
        if ("policy" %in% names(selected_tbl)) as.character(.data$policy) else NA_character_
      ),
      equation_branch_filter = dplyr::coalesce(
        if ("selected_equation_branch_filter" %in% names(selected_tbl)) as.character(.data$selected_equation_branch_filter) else NA_character_,
        if ("equation_branch_filter" %in% names(selected_tbl)) as.character(.data$equation_branch_filter) else NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$policy), !is.na(.data$equation_branch_filter)) |>
    dplyr::distinct()
  benchmark_ts_error <- tibble::as_tibble((selector@benchmark)$policy_ts_error %||% tibble::tibble())
  benchmark_policy_perf <- tibble::as_tibble((selector@benchmark)$policy_perf %||% tibble::tibble())
  if (nrow(selected_policy_keys) > 0 && nrow(benchmark_ts_error) > 0) {
    benchmark_ts_error <- normalize_policy_columns(benchmark_ts_error)
    benchmark_ts_error$policy <- resolve_policy_names(benchmark_ts_error)
    benchmark_ts_error <- benchmark_ts_error |>
      dplyr::inner_join(
        selected_policy_keys,
        by = c("policy", "equation_branch_filter")
      )
  }
  benchmark_ts_error <- enrich_ts_calibration_locality(
    ts_calibration = benchmark_ts_error,
    policy_perf = benchmark_policy_perf
  )
  benchmark_selected_calibration <- tibble::tibble()
  report_progress(progress, "[Referee] Preparing calibration sources...")
  if (nrow(selected_tbl) == 0 && nrow(learner_selected_calibration) == 0) {
    report_progress(progress, "[Referee] Deriving fallback benchmark-selected calibration...")
    benchmark_selected_calibration <- derive_benchmark_selected_calibration(
      policy_perf = benchmark_policy_perf,
      config = policy_cfg,
      learner = learner_now
    )
  }
  ts_calibration_source <- if (nrow(selected_tbl) > 0) {
    tibble::as_tibble(selected_tbl)
  } else if (nrow(learner_selected_calibration) > 0) {
    tibble::as_tibble(learner_selected_calibration)
  } else if (nrow(benchmark_selected_calibration) > 0) {
    tibble::as_tibble(benchmark_selected_calibration)
  } else {
    {
      tibble::as_tibble(intervals_tbl)
    } |>
      dplyr::distinct()
  }
  report_progress(progress, "[Referee] Summarizing selected-row TS calibration...")
  selected_ts_calibration <- if (nrow(ts_calibration_source) > 0 && nrow(benchmark_ts_error) > 0) {
    summarize_selected_ts_calibration(
      ts_error = benchmark_ts_error,
      selected_tbl = ts_calibration_source
    )
  } else {
    tibble::tibble()
  }
  coefficient_calibration_source <- if (nrow(selected_tbl) > 0) {
    tibble::as_tibble(selected_tbl)
  } else if (nrow(learner_selected_calibration) > 0) {
    tibble::as_tibble(learner_selected_calibration)
  } else if (nrow(benchmark_selected_calibration) > 0) {
    tibble::as_tibble(benchmark_selected_calibration)
  } else {
    {
      tibble::as_tibble(intervals_tbl)
    } |>
      dplyr::distinct()
  }
  if (
    !all(c("policy_slope_len", "policy_intercept_len") %in% names(coefficient_calibration_source)) ||
      !any(
        suppressWarnings(is.finite(as.numeric(coefficient_calibration_source$policy_slope_len))) &
          suppressWarnings(is.finite(as.numeric(coefficient_calibration_source$policy_intercept_len)))
      )
  ) {
    coefficient_calibration_source <- selected_tbl
  }
  report_progress(progress, "[Referee] Summarizing selected-row coefficient calibration...")
  selected_coefficient_calibration <- if (nrow(coefficient_calibration_source) > 0) {
    summarize_coeff_calibration(
      selected_tbl = coefficient_calibration_source,
      candidate_models = candidate_models,
      ts_error = benchmark_policy_perf
    )
  } else {
    tibble::tibble()
  }

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
      anchor_model_id = .data$anchor_model_id,
      policy = if ("selected_policy" %in% names(selected_tbl)) {
        as.character(.data$selected_policy)
      } else if ("policy" %in% names(selected_tbl)) {
        as.character(.data$policy)
      } else {
        rep(NA_character_, dplyr::n())
      },
      equation_branch_filter = if ("selected_equation_branch_filter" %in% names(selected_tbl)) {
        as.character(.data$selected_equation_branch_filter)
      } else if ("equation_branch_filter" %in% names(selected_tbl)) {
        as.character(.data$equation_branch_filter)
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

  report_progress(progress, "[Referee] Reconstructing selected-row coefficient intervals...")
  selected_tbl <- augment_policy_coefficient_intervals(
    policy_tbl = selected_tbl,
    candidate_models = candidate_models,
    anchor_scores = anchor_scores,
    competition_policy_tbl = intervals_tbl,
    ts_calibration = selected_ts_calibration,
    coefficient_calibration = selected_coefficient_calibration,
    config = policy_cfg,
    model_scores = ordination_scores,
    species_lookup = ordination_species_lookup
  )
  report_progress(progress, "[Referee] Reconstructing selected-row conditional coefficient intervals...")
  selected_tbl <- augment_conditional_coeff_intervals(
    policy_tbl = selected_tbl,
    candidate_models = candidate_models,
    anchor_scores = anchor_scores,
    ts_calibration = selected_ts_calibration,
    coefficient_calibration = selected_coefficient_calibration,
    config = policy_cfg,
    model_scores = ordination_scores,
    species_lookup = ordination_species_lookup
  )
  selected_tbl <- augment_anchor_length_context(
    policy_tbl = selected_tbl,
    candidate_models = candidate_models
  )
  selected_tbl <- augment_anchor_coefficient_context(
    policy_tbl = selected_tbl,
    candidate_models = candidate_models
  )
  selected_tbl <- scorecard_normalize_multiplier_columns(selected_tbl)
  intervals_tbl <- scorecard_normalize_multiplier_columns(intervals_tbl)
  intervals_tbl <- augment_anchor_length_context(
    policy_tbl = intervals_tbl,
    candidate_models = candidate_models,
    length_grid_n = 200L
  )
  report_progress(progress, "[Referee] Reconstructing selected-row TS envelopes...")
  ts_panel_tbl <- if (all(c("policy_slope_len", "policy_intercept_len") %in% names(selected_tbl))) {
    build_ts_conformal_panel_data(
      selected_tbl = selected_tbl,
      ts_calibration = selected_ts_calibration,
      coefficient_calibration = selected_coefficient_calibration,
      candidate_models = candidate_models,
      anchor_scores = anchor_scores,
      competition_policy_tbl = intervals_tbl,
      config = policy_cfg,
      model_scores = ordination_scores,
      species_lookup = ordination_species_lookup,
      progress = progress
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
          anchor_model_id = .data$anchor_model_id,
          combined_consensus_multiplier = .data$consensus_multiplier,
          combined_multiplier_q05 = .data$multiplier_q05,
          combined_multiplier_q50 = .data$multiplier_q50,
          combined_multiplier_q95 = .data$multiplier_q95,
          local_support_mass = if ("local_support_mass" %in% names(consensus_tbl)) {
            .data$local_support_mass
          } else {
            NA_real_
          },
          local_effective_support = if ("local_effective_support" %in% names(consensus_tbl)) {
            .data$local_effective_support
          } else {
            NA_real_
          }
        ),
      by = "anchor_model_id"
    )

  species_coverage_result <- referee_component(
    "species_coverage",
    construct_species_coverage(selector),
    allow_partial = allow_partial
  )
  anchor_audit_result <- referee_component(
    "anchor_audit",
    construct_anchor_audit(predictions_, selector = selector),
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
  selection_source <- if (is_s7_instance(resolve_policy_learner(object@learner), "PolicyLearner")) {
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
  report_progress(progress, "[Referee] Assembling plot/report tables...")
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
    {
      failure_messages <- if ("prediction_error_message" %in% names(selected_tbl)) {
        as.character(selected_tbl$prediction_error_message)
      } else {
        rep(NA_character_, nrow(selected_tbl))
      }
      n_unscorable <- sum(!is.na(failure_messages) & nzchar(failure_messages))
      tibble::tibble(
        component = "anchor_scoring",
        status = if (n_unscorable > 0L) "partial" else "ok",
        message = if (n_unscorable > 0L) {
          sprintf(
            "%d of %d reference anchors were unscorable; see `anchor_audit` for explicit reasons.",
            n_unscorable,
            nrow(selected_tbl)
          )
        } else {
          NA_character_
        }
      )
    },
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
#' @param progress Optional logical scalar controlling stage messages.
#'
#' @keywords internal
#' @noRd
.predict_referee <- function(object,
                             predictions = NULL,
                             allow_partial = FALSE,
                             progress = NULL,
                             ...) {
  dots <- list(...)
  if ("predictions" %in% names(dots)) {
    predictions <- dots[["predictions"]]
  }
  if ("allow_partial" %in% names(dots)) {
    allow_partial <- dots[["allow_partial"]]
  }
  if ("progress" %in% names(dots)) {
    progress <- dots[["progress"]]
  }
  dot_names <- names(dots) %||% rep("", length(dots))
  positional_dots <- dots[!nzchar(dot_names)]
  if (is.null(predictions) && length(positional_dots) > 0L) {
    predictions <- positional_dots[[1L]]
  }
  cfg <- merge_config_sections(object@selector@config, list())
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = c("selection", "benchmark")) %||%
    FALSE

  prediction_bundle <- predictions %||% object@predictions
  if (is.null(prediction_bundle)) {
    learner_now <- resolve_policy_learner(object@learner)
    prediction_bundle <- if (is_s7_instance(learner_now, "PolicyLearner")) {
      stats::predict(object@selector, learner = learner_now, progress = progress)
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
    allow_partial = allow_partial,
    progress = progress
  )
  report_progress(progress, "[Referee] Scorecard complete.")
  sc
}

#' Predict a referee scorecard
#'
#' @return A populated [Scorecard].
#' @name predict.Referee
#' @usage NULL
S7::method(predict_generic, Referee) <- .predict_referee

#' Derive fallback calibration from benchmark-selected policies
#'
#' @param policy_perf Policy-performance table.
#' @param config Optional config object or config list.
#' @param learner Optional [PolicyLearner] used for interval augmentation.
#'
#' @return Tibble containing benchmark-selected calibration rows.
#'
#' @keywords internal
#' @noRd
derive_benchmark_selected_calibration <- function(policy_perf,
                                                  config = NULL,
                                                  learner = NULL) {
  # Validate the benchmark table before deriving any fallback uncertainty rows.
  policy_perf <- tibble::as_tibble(policy_perf)
  if (nrow(policy_perf) == 0) {
    return(tibble::tibble())
  }
  if (!all(c("anchor_model_id", "anchor_species") %in% names(policy_perf))) {
    return(tibble::tibble())
  }

  # Normalize scalar config values so the fallback path follows the active selector rules.
  scalar_num <- function(x, default = NA_real_) {
    y <- suppressWarnings(as.numeric(x))
    if (length(y) == 0L) {
      return(default)
    }
    y[[1]]
  }

  uncertainty_rule <- normalize_uncertainty_rule(
    policy_selector_config_value(config, "uncertainty_rule", sections = c("selection", "policy")) %||% "tolerance"
  )
  one_se_multiplier <- scalar_num(
    policy_selector_config_value(config, "one_se_multiplier", sections = c("selection", "policy")) %||% 1
  )
  u_tol_rel <- scalar_num(
    policy_selector_config_value(config, "u_tol_rel", sections = c("selection", "policy")) %||%
      policy_selector_config_value(config, "uncertainty_relative_tolerance", sections = c("selection", "policy"))
  )
  u_tol_abs <- scalar_num(
    policy_selector_config_value(config, "u_tol_abs", sections = c("selection", "policy")) %||%
      policy_selector_config_value(config, "uncertainty_absolute_tolerance", sections = c("selection", "policy"))
  )
  local_distance_tolerance <- scalar_num(
    policy_selector_config_value(config, "local_distance_tolerance", sections = c("selection", "policy"))
  )
  if (!is.finite(one_se_multiplier)) {
    one_se_multiplier <- 1
  }
  if (!is.finite(u_tol_rel)) {
    u_tol_rel <- 0
  }
  if (!is.finite(u_tol_abs)) {
    u_tol_abs <- 0
  }
  has_learner <- is_s7_instance(learner, "PolicyLearner")

  policy_perf |>
    dplyr::group_split(.data$anchor_model_id, .data$anchor_species, .keep = TRUE) |>
    purrr::map_dfr(function(.x) {
      selected_row <- if (has_learner) {
        predicted <- tryCatch(
          stats::predict(
            learner,
            .x,
            use_support_bin_intervals = FALSE
          ),
          error = function(e) NULL
        )
        if (is.null(predicted)) {
          tibble::tibble()
        } else {
          tibble::as_tibble(predicted) |>
            dplyr::filter(dplyr::coalesce(.data$valid_prediction, FALSE), dplyr::coalesce(.data$is_selected, FALSE))
        }
      } else {
        tibble::tibble()
      }

      if (nrow(selected_row) == 0) {
        selected_row <- select_anchor_policies(
          policy_tbl = .x,
          uncertainty_rule = uncertainty_rule,
          u_tol_rel = u_tol_rel,
          u_tol_abs = u_tol_abs,
          one_se_multiplier = one_se_multiplier,
          local_distance_tolerance = local_distance_tolerance
        )
      }

      selected_row |>
        dplyr::mutate(
          selected_policy = dplyr::coalesce(
            if ("selected_policy" %in% names(selected_row)) as.character(.data$selected_policy) else NA_character_,
            if ("policy" %in% names(selected_row)) as.character(.data$policy) else NA_character_
          ),
          selected_equation_branch_filter = dplyr::coalesce(
            if ("selected_equation_branch_filter" %in% names(selected_row)) as.character(.data$selected_equation_branch_filter) else NA_character_,
            if ("equation_branch_filter" %in% names(selected_row)) as.character(.data$equation_branch_filter) else NA_character_
          )
        )
    }) |>
    dplyr::filter(
      !is.na(.data$anchor_model_id),
      !is.na(.data$selected_policy),
      !is.na(.data$selected_equation_branch_filter)
    )
}

#' Collapse one `Scorecard` to a console summary
#'
#' @param x A [Scorecard] object.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @noRd
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
#' @usage NULL
#'
#' @param x A [Scorecard] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @noRd
S7::method(print_generic, Scorecard) <- function(x, ...) {
  scorecard_console_summary(x)
}

#' Show a `Scorecard`
#'
#' @name show.Scorecard
#' @usage NULL
#'
#' @param object A [Scorecard] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
#' @noRd
S7::method(show_generic, Scorecard) <- function(object) {
  scorecard_console_summary(object)
}

#' Coerce `Scorecard` to a tibble
#'
#' Returns the canonical recommendation-card result table.
#'
#' @param x A [Scorecard] object.
#' @param ... Unused.
#'
#' @return Tibble of recommendation cards.
#'
#' @examples
#' \dontrun{
#' referee <- as_referee(selector)
#' scorecard <- predict(referee)
#' tibble::as_tibble(scorecard)
#' }
#'
#' @keywords internal
#' @noRd
as_tibble_scorecard <- function(x, ...) {
  tibble::as_tibble(x@recommendation_cards)
}

#' Print a `Referee`
#'
#' @name print.Referee
#' @usage NULL
#'
#' @param x A [Referee] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @noRd
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
#' @usage NULL
#'
#' @param object A [Referee] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
#' @noRd
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
#' @param view Secondary plot selector. For `type = "validation"`, supported
#'   values are `"distribution"`, `"ranked"`, `"ranked_species"`, and `"fold"`.
#' @param anchor_model_id Optional anchor model ID used for
#'   `type = "ts_length_bands"` and `type = "strategy_competition"`.
#' @param anchor_species Optional anchor species used when `anchor_model_id` is
#'   not supplied.
#' @param show_top_candidate Logical scalar indicating whether the TS-length
#'   plots should overlay the top-ranked admissible candidate curve when it is
#'   available.
#' @param reference_label Reference label used for
#'   `type = "selected_intervals"`.
#' @param ... Additional arguments used by Sentinel scorecards, including
#'   `metric`, `summary_method`, `metric_scale`, `scenario_names`, and
#'   `species_names`, `label_species`, and coverage-display arguments.
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
#' @usage
#' \method{plot}{Scorecard}(
#'   x,
#'   y = NULL,
#'   type = c(
#'     "ts_length",
#'     "ts_length_conformal",
#'     "ts_length_multiplier",
#'     "ts_length_bands",
#'     "coefficient_uncertainty",
#'     "selected_intervals",
#'     "selected_multiplier_summary",
#'     "selected_policy_counts",
#'     "strategy_competition",
#'     "field_missingness",
#'     "ablation",
#'     "validation",
#'     "coverage",
#'     "ablation_decomposition",
#'     "sentinel_ablation",
#'     "sentinel_ablation_decomposition",
#'     "sentinel_validation",
#'     "sentinel_coverage"
#'   ),
#'   scale = c("ts", "multiplier"),
#'   view = NULL,
#'   anchor_model_id = NULL,
#'   anchor_species = NULL,
#'   show_top_candidate = FALSE,
#'   reference_label = "Reference",
#'   ...
#' )
NULL

#' Current Scorecard plot type names
#'
#' @return Character vector of supported plot type names.
#' @keywords internal
#' @noRd
scorecard_plot_types <- function() {
  c(
    "ts_length",
    "ts_length_conformal",
    "ts_length_multiplier",
    "ts_length_bands",
    "coefficient_uncertainty",
    "selected_intervals",
    "selected_multiplier_summary",
    "selected_policy_counts",
    "strategy_competition",
    "field_missingness",
    "ablation",
    "validation",
    "coverage",
    "ablation_decomposition",
    "sentinel_ablation",
    "sentinel_ablation_decomposition",
    "sentinel_validation",
    "sentinel_coverage"
  )
}

.plot_scorecard <- function(x,
                            y = NULL,
                            type = c(
                              "ts_length",
                              "ts_length_conformal",
                              "ts_length_multiplier",
                              "ts_length_bands",
                              "coefficient_uncertainty",
                              "selected_intervals",
                              "selected_multiplier_summary",
                              "selected_policy_counts",
                              "strategy_competition",
                              "field_missingness",
                              "ablation",
                              "validation",
                              "coverage",
                              "ablation_decomposition",
                              "sentinel_ablation",
                              "sentinel_ablation_decomposition",
                              "sentinel_validation",
                              "sentinel_coverage"
                            ),
                            scale = c("ts", "multiplier"),
                            view = NULL,
                            anchor_model_id = NULL,
                            anchor_species = NULL,
                            show_top_candidate = FALSE,
                            reference_label = "Reference",
                            ...) {
  valid_types <- scorecard_plot_types()
  type <- as.character(type %||% valid_types[[1]])[[1]]
  if (!type %in% valid_types) {
    return(plot_report_placeholder(
      title = "Scorecard Plot Unavailable",
      subtitle = sprintf(
        "Plot type '%s' is not a current Scorecard plot type.",
        type
      )
    ))
  }
  scale <- match.arg(scale)

  if (type %in% c("ablation", "sentinel_ablation")) {
    return(plot_sentinel_ablation_scorecard(x, ...))
  }
  if (type %in% c("ablation_decomposition", "sentinel_ablation_decomposition")) {
    return(plot_sentinel_ablation_decomposition_scorecard(x, ...))
  }
  if (type %in% c("validation", "sentinel_validation")) {
    return(plot_sentinel_validation_scorecard(x, view = view, ...))
  }
  if (type %in% c("coverage", "sentinel_coverage")) {
    return(plot_sentinel_coverage_scorecard(x, ...))
  }

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
      return(plot_report_placeholder(
        title = "Weighted TS Ribbon",
        subtitle = "No TS panel is stored on this Scorecard.",
        x = "Length (cm)",
        y = "TS (dB re 1 m^2)"
      ))
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
      return(plot_report_placeholder(
        title = "Weighted TS Ribbon",
        subtitle = "No TS panel rows matched the requested anchor.",
        x = "Length (cm)",
        y = "TS (dB re 1 m^2)"
      ))
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
    curve_tbl$ts_center <- dplyr::coalesce(
      if ("ts_center" %in% names(curve_tbl)) suppressWarnings(as.numeric(curve_tbl$ts_center)) else rep(NA_real_, nrow(curve_tbl)),
      suppressWarnings(as.numeric(curve_tbl$ts_pred))
    )
    band_tbl <- dplyr::bind_rows(
      curve_tbl |>
        dplyr::transmute(.data$length_cm, .data$ts_anchor, .data$ts_top_candidate, .data$ts_center, band = "99%", ymin = .data$ts_lo_99, ymax = .data$ts_hi_99),
      curve_tbl |>
        dplyr::transmute(.data$length_cm, .data$ts_anchor, .data$ts_top_candidate, .data$ts_center, band = "95%", ymin = .data$ts_lo_95, ymax = .data$ts_hi_95),
      curve_tbl |>
        dplyr::transmute(.data$length_cm, .data$ts_anchor, .data$ts_top_candidate, .data$ts_center, band = "90%", ymin = .data$ts_lo_90, ymax = .data$ts_hi_90),
      curve_tbl |>
        dplyr::transmute(.data$length_cm, .data$ts_anchor, .data$ts_top_candidate, .data$ts_center, band = "80%", ymin = .data$ts_lo_80, ymax = .data$ts_hi_80)
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
  if (identical(type, "selected_intervals")) {
    return(plot_selected_intervals(x@selected, reference_label = reference_label))
  }
  if (identical(type, "selected_multiplier_summary")) {
    plot_df <- tibble::as_tibble(x@selected)
    if (nrow(plot_df) == 0) {
      return(plot_report_placeholder(
        title = "Meta-policy selected biomass multipliers",
        subtitle = "No selected-policy rows are stored on this Scorecard.",
        x = NULL,
        y = "Biomass multiplier"
      ))
    }

    plot_df$selected_policy_display <- resolve_selected_policy_names(plot_df)
    branch_display <- {
      branch_vals <- dplyr::coalesce(
        if ("selected_equation_branch_filter" %in% names(plot_df)) as.character(plot_df$selected_equation_branch_filter) else rep(NA_character_, nrow(plot_df)),
        if ("equation_branch_filter" %in% names(plot_df)) as.character(plot_df$equation_branch_filter) else rep(NA_character_, nrow(plot_df))
      )
      branch_defs <- read_policy_registry()$policy_branches %||% list()
      branch_tags <- stats::setNames(
        vapply(branch_defs, function(x) as.character(x$display_tag %||% x$key %||% NA_character_), character(1)),
        vapply(branch_defs, function(x) as.character(x$key %||% NA_character_), character(1))
      )
      branch_labs <- unname(branch_tags[branch_vals])
      branch_labs[is.na(branch_labs) | !nzchar(branch_labs)] <- branch_vals[is.na(branch_labs) | !nzchar(branch_labs)]
      branch_labs
    }
    plot_df$selected_policy_display <- ifelse(
      grepl("\\s*\\[[^]]+\\]$", plot_df$selected_policy_display),
      plot_df$selected_policy_display,
      paste0(plot_df$selected_policy_display, " [", branch_display, "]")
    )
    plot_df$multiplier_lo <- dplyr::coalesce(
      if ("meta_post_selection_multiplier_lo" %in% names(plot_df)) as.numeric(plot_df$meta_post_selection_multiplier_lo) else rep(NA_real_, nrow(plot_df)),
      if ("multiplier_lo" %in% names(plot_df)) as.numeric(plot_df$multiplier_lo) else rep(NA_real_, nrow(plot_df))
    )
    plot_df$multiplier_hi <- dplyr::coalesce(
      if ("meta_post_selection_multiplier_hi" %in% names(plot_df)) as.numeric(plot_df$meta_post_selection_multiplier_hi) else rep(NA_real_, nrow(plot_df)),
      if ("multiplier_hi" %in% names(plot_df)) as.numeric(plot_df$multiplier_hi) else rep(NA_real_, nrow(plot_df))
    )
    plot_df <- plot_df |>
      dplyr::filter(
        !is.na(.data$anchor_species),
        is.finite(.data$multiplier_pred),
        is.finite(.data$multiplier_lo),
        is.finite(.data$multiplier_hi),
        .data$multiplier_pred > 0,
        .data$multiplier_lo > 0,
        .data$multiplier_hi > 0
      ) |>
      dplyr::mutate(
        anchor_label = paste(.data$anchor_species, .data$selected_policy_display, sep = " | "),
        anchor_label = stats::reorder(.data$anchor_label, .data$multiplier_pred)
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
          y = .data$multiplier_pred,
          x = anchor_label,
          ymin = .data$multiplier_lo,
          ymax = .data$multiplier_hi
        )
      ) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey45") +
        ggplot2::geom_errorbar(width = 0.22, linewidth = 0.75, alpha = 0.85, colour = "#7a7a7a") +
        ggplot2::geom_point(size = 2.5, colour = "#7a7a7a") +
        ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          y = "Biomass multiplier relative to reference anchor",
          x = NULL,
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
      return(plot_report_placeholder(
        title = "Selected policies",
        subtitle = "No selected-policy rows are stored on this Scorecard.",
        x = NULL,
        y = "Selected anchor-model count"
      ))
    }

    plot_df$selected_policy_display <- resolve_selected_policy_names(plot_df)

    if (identical(view, "by_policy")) {
      count_tbl <- plot_df |>
        dplyr::count(.data$selected_policy_display, sort = TRUE) |>
        dplyr::mutate(selected_policy_display = stats::reorder(.data$selected_policy_display, .data$n))
      return(
        ggplot2::ggplot(
          count_tbl,
          ggplot2::aes(x = .data$selected_policy_display, y = .data$n)
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
      dplyr::count(.data$anchor_species, .data$selected_policy_display) |>
      dplyr::group_by(.data$anchor_species) |>
      dplyr::mutate(anchor_total = sum(.data$n)) |>
      dplyr::ungroup() |>
      dplyr::mutate(anchor_species = stats::reorder(.data$anchor_species, .data$anchor_total))
    return(
      ggplot2::ggplot(
        count_tbl,
        ggplot2::aes(x = .data$anchor_species, y = .data$n, fill = .data$selected_policy_display)
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
    if (identical(view, "components") || identical(view, "component")) {
      return(plot_strategy_component_competition(interval_tbl))
    }
    return(plot_interval_panel(interval_tbl))
  }
  if (identical(type, "field_missingness")) {
    return(plot_field_missing(x@key_missing_by_field))
  }
  plot_field_missing(x@key_missing_by_field)
}

#' Register the `Scorecard` plot method
#'
#' @name plot.Scorecard
#' @usage NULL
#'
#' @keywords internal
#' @noRd
plot.Scorecard <- .plot_scorecard
S7::method(plot_generic, Scorecard) <- .plot_scorecard

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
#'   `type = "biomass_change"` and `type = "length_density"`.
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
#' plot(referee, type = "biomass_change")
#' plot(referee, type = "length_density", anchor_species = "Sardinops sagax")
#' }
#' @usage
#' \method{plot}{Referee}(
#'   x,
#'   y = NULL,
#'   type = "biomass_change",
#'   view = NULL,
#'   anchor_model_id = NULL,
#'   anchor_species = NULL,
#'   reference_name = NULL,
#'   ...
#' )
NULL

#' Current Referee plot type names
#'
#' @return Character vector of supported plot type names.
#' @keywords internal
#' @noRd
referee_plot_types <- function() {
  c("biomass_change", "strategy_competition", "length_density", "anchor_multiplier_summary")
}

.plot_referee <- function(x,
                          y = NULL,
                          type = "biomass_change",
                          view = NULL,
                          anchor_model_id = NULL,
                          anchor_species = NULL,
                          reference_name = NULL,
                          ...) {
  valid_types <- referee_plot_types()
  type <- as.character(type %||% valid_types[[1]])[[1]]
  if (!type %in% valid_types) {
    return(plot_report_placeholder(
      title = "Referee Plot Unavailable",
      subtitle = sprintf(
        "Plot type '%s' is not a current Referee plot type.",
        type
      )
    ))
  }
  if (identical(type, "anchor_multiplier_summary")) {
    type <- "biomass_change"
  }
  scorecard_obj <- x@scorecard
  predictions_obj <- if (is_s7_instance(x@predictions, "PolicyPredictions")) {
    x@predictions
  } else {
    NULL
  }
  scorecard_ready <- nrow(scorecard_obj@anchor_summary) > 0 ||
    nrow(scorecard_obj@intervals) > 0 ||
    nrow(scorecard_obj@selected) > 0
  candidate_scores <- if (length(x@selector@candidates@admissibility) == 0) {
    tibble::tibble()
  } else {
    (x@selector@candidates@admissibility)$all_scores %||% tibble::tibble()
  }
  use_scorecard_path <- identical(type, "biomass_change") ||
    identical(type, "anchor_multiplier_summary")

  if (scorecard_ready) {
    anchor_summary_tbl <- tibble::as_tibble(scorecard_obj@anchor_summary)
    interval_tbl <- tibble::as_tibble(scorecard_obj@intervals)
    selected_tbl <- tibble::as_tibble(scorecard_obj@selected)
  } else if (!is.null(predictions_obj) && !use_scorecard_path) {
    selected_tbl <- tibble::as_tibble(predictions_obj@selections)
    interval_tbl <- tibble::as_tibble(predictions_obj@intervals)
    consensus_tbl <- tibble::as_tibble(predictions_obj@consensus)

    if (nrow(interval_tbl) > 0 && all(c("anchor_model_id", "policy") %in% names(interval_tbl))) {
      selected_keys <- selected_tbl |>
        dplyr::transmute(
          anchor_model_id,
          policy = if ("selected_policy" %in% names(selected_tbl)) {
            as.character(.data$selected_policy)
          } else if ("policy" %in% names(selected_tbl)) {
            as.character(.data$policy)
          } else {
            rep(NA_character_, dplyr::n())
          },
          equation_branch_filter = if ("selected_equation_branch_filter" %in% names(selected_tbl)) {
            as.character(.data$selected_equation_branch_filter)
          } else if ("equation_branch_filter" %in% names(selected_tbl)) {
            as.character(.data$equation_branch_filter)
          } else {
            rep(NA_character_, dplyr::n())
          },
          is_selected = TRUE
        ) |>
        dplyr::distinct()

      interval_tbl <- interval_tbl |>
        dplyr::left_join(
          selected_keys,
          by = intersect(
            c("anchor_model_id", "policy", "equation_branch_filter"),
            intersect(names(interval_tbl), names(selected_keys))
          )
        )
      interval_tbl$is_selected <- dplyr::coalesce(
        if ("is_selected.y" %in% names(interval_tbl)) as.logical(interval_tbl$is_selected.y) else rep(NA, nrow(interval_tbl)),
        if ("is_selected" %in% names(interval_tbl)) as.logical(interval_tbl$is_selected) else rep(NA, nrow(interval_tbl)),
        if ("is_selected.x" %in% names(interval_tbl)) as.logical(interval_tbl$is_selected.x) else rep(NA, nrow(interval_tbl)),
        FALSE
      )
      interval_tbl <- interval_tbl |>
        dplyr::select(-dplyr::any_of(c("is_selected.x", "is_selected.y")))
    }

    anchor_summary_tbl <- selected_tbl |>
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
        "interval_log_width"
      ))) |>
      dplyr::left_join(
        consensus_tbl |>
          dplyr::transmute(
            .data$anchor_model_id,
            combined_multiplier_q05 = .data$multiplier_q05,
            combined_multiplier_q50 = .data$multiplier_q50,
            combined_multiplier_q95 = .data$multiplier_q95
          ),
        by = "anchor_model_id"
      )
  } else {
    scorecard_obj <- stats::predict(x, progress = FALSE)
    anchor_summary_tbl <- tibble::as_tibble(scorecard_obj@anchor_summary)
    interval_tbl <- tibble::as_tibble(scorecard_obj@intervals)
    selected_tbl <- tibble::as_tibble(scorecard_obj@selected)
  }

  anchor_summary_tbl <- scorecard_normalize_multiplier_columns(anchor_summary_tbl)
  interval_tbl <- scorecard_normalize_multiplier_columns(interval_tbl)
  selected_tbl <- scorecard_normalize_multiplier_columns(selected_tbl)

  if (nrow(anchor_summary_tbl) == 0 && nrow(interval_tbl) == 0 && nrow(selected_tbl) == 0) {
    return(plot_report_placeholder(
      title = "Referee Plot",
      subtitle = "No plot-ready prediction results are available on this Referee."
    ))
  }

  if (identical(type, "biomass_change")) {
    view <- match.arg(view %||% "summary", c("summary", "strategy_competition"))
    if (identical(view, "strategy_competition")) {
      type <- "strategy_competition"
    } else {
      base_plot <- plot_anchor_summary(
        integrated_tbl = anchor_summary_tbl,
        score_tbl = candidate_scores,
        interval_tbl = interval_tbl
      )
      return(base_plot)
    }
  }

  if (identical(type, "strategy_competition")) {
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
    if (identical(view, "components") || identical(view, "component")) {
      return(plot_strategy_component_competition(interval_tbl))
    }
    return(plot_interval_panel(interval_tbl))
  }

  # These anchor diagnostics combine the scorecard's selected row with the
  # selector's admissible donor pool so the plotted support set matches the
  # final recommendation context exactly.
  if (identical(type, "length_density")) {
    if (nrow(selected_tbl) == 0) {
      return(plot_report_placeholder(
        title = "Reference Anchor Length Density",
        subtitle = "No selected-policy rows are stored on this Referee.",
        x = "Length (cm)",
        y = "f(L)"
      ))
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
      return(plot_report_placeholder(
        title = "Reference Anchor Length Density",
        subtitle = "No selected-policy rows matched the requested anchor.",
        x = "Length (cm)",
        y = "f(L)"
      ))
    }
    density_view <- match.arg(
      view %||% if (is.null(anchor_id_value) && is.null(anchor_species_value)) "panel" else "single",
      c("panel", "single")
    )
    candidate_models <- tibble::as_tibble(x@selector@candidates@candidate_models)
    id_col <- reference_anchor_id_column(candidate_models)
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
        anchor_pdf <- build_anchor_length_pdf(anchor_row, on_missing = "empty")
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
        return(plot_report_placeholder(
          title = "Reference Anchor Length Density",
          subtitle = "No anchor length-support metadata were available for the requested panel.",
          x = "Length (cm)",
          y = "f(L)"
        ))
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
      return(plot_report_placeholder(
        title = "Reference Anchor Length Density",
        subtitle = "The requested anchor was not present in the selector candidate table.",
        x = "Length (cm)",
        y = "f(L)"
      ))
    }
    anchor_pdf <- build_anchor_length_pdf(anchor_row, on_missing = "empty")
    if (nrow(anchor_pdf) == 0) {
      return(plot_report_placeholder(
        title = "Reference Anchor Length Density",
        subtitle = "The requested anchor does not contain stored length-support metadata needed for this diagnostic plot.",
        x = "Length (cm)",
        y = "f(L)"
      ))
    }
    return(plot_length_density(anchor_pdf, anchor_label))
  }

  plot_anchor_summary(
    integrated_tbl = anchor_summary_tbl,
    score_tbl = candidate_scores,
    interval_tbl = interval_tbl
  )
}

#' Register the `Referee` plot method
#'
#' @name plot.Referee
#' @usage NULL
#'
#' @keywords internal
#' @noRd
plot.Referee <- .plot_referee
S7::method(plot_generic, Referee) <- .plot_referee
