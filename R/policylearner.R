#' Policy Learner S7 Class
#'
#' `PolicyLearner` wraps the meta-policy and super-learner pipeline around a
#' [PolicySelector]. It owns the cross-fitted meta-policy benchmark,
#' final learner fit, and post-selection calibration state used to rank
#' anchor-policy predictions by predicted transferability score.
#'
#' The learner is designed to plug back into [predict()] on a
#' [PolicySelector], so the selector remains the single high-level source of
#' policy predictions.
#'
#' @examples
#' \dontrun{
#' selector <- as_policyselector(candidates)
#' selector <- benchmark(selector)
#' learner <- as_policylearner(selector)
#' learner <- crossfit(learner)
#' learner <- fit(learner)
#' learner <- calibrate_uncertainty(learner)
#' }
#'
#' @name PolicyLearner-class
#' @usage NULL
#' @aliases PolicyLearner
NULL

#' @export
PolicyLearner <- S7::new_class(
  "PolicyLearner",
  properties = list(
    selector = S7::new_property(S7::class_any),
    config = S7::new_property(S7::class_list),
    training_data = S7::new_property(CandidatesDataFrame),
    crossfit = S7::new_property(S7::class_list),
    fitted_model = S7::new_property(S7::class_list),
    calibration = S7::new_property(S7::class_list)
  ),
  validator = function(self) {
    tryCatch(
      {
        if (!is.null(self@selector) &&
          !is_s7_instance(self@selector, "PolicySelector")) {
          return("`selector` must be a `PolicySelector` object or NULL.")
        }
        if (!is.list(self@config)) {
          return("`config` must be a list.")
        }
        if (!is.data.frame(self@training_data)) {
          return("`training_data` must be a data frame.")
        }
        if (!is.list(self@crossfit)) {
          return("`crossfit` must be a list.")
        }
        if (!is.list(self@fitted_model)) {
          return("`fitted_model` must be a list.")
        }
        if (!is.list(self@calibration)) {
          return("`calibration` must be a list.")
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(PolicyLearner)

#' Test whether an object is a `PolicyLearner` instance
#'
#' Rebuild a `PolicyLearner`
#'
#' @param object A [PolicyLearner] object.
#' @param selector Optional replacement [PolicySelector].
#' @param config Optional replacement config list.
#' @param training_data Optional replacement training data.
#' @param crossfit Optional replacement cross-fit result.
#' @param fitted_model Optional replacement fitted-model bundle.
#' @param calibration Optional replacement calibration bundle.
#'
#' @return A `PolicyLearner` object.
#'
#' @keywords internal
#' @noRd
policy_learner_rebuild <- function(object,
                                   selector = object@selector,
                                   config = object@config,
                                   training_data = object@training_data,
                                   crossfit = object@crossfit,
                                   fitted_model = object@fitted_model,
                                   calibration = object@calibration) {
  # If a full PolicySelector is supplied, extract its config and benchmark data
  # rather than storing the full object. This keeps the learner slim.
  if (!is.null(selector) &&
    is_s7_instance(selector, "PolicySelector")) {
    selector_config <- tryCatch(
      policy_selector_config_data(selector@config),
      error = function(e) list()
    )
    config <- merge_config_sections(selector_config, config)
    if (is.null(crossfit$species_block_perf) || nrow(crossfit$species_block_perf %||% tibble::tibble()) == 0) {
      crossfit$species_block_perf <- tryCatch(
        tibble::as_tibble(selector@benchmark$species_block_perf %||% tibble::tibble()),
        error = function(e) tibble::tibble()
      )
    }
    selector <- NULL
  }
  PolicyLearner(
    selector = selector,
    config = config,
    training_data = tibble::as_tibble(training_data),
    crossfit = crossfit,
    fitted_model = fitted_model,
    calibration = calibration
  )
}

#' Build a `PolicyLearner`
#'
#' @param selector A [PolicySelector] object.
#' @param config Optional learner config list or [Configurer] object.
#'
#' @return A `PolicyLearner` object.
#'
#' @examples
#' \dontrun{
#' learner <- as_policylearner(selector)
#' learner
#' }
#'
#' @export
as_policylearner <- function(selector,
                             config = NULL) {
  if (!is_s7_instance(selector, "PolicySelector")) {
    stop("'selector' must be a `PolicySelector` object.", call. = FALSE)
  }

  # Extract only what is needed from the selector rather than storing the full
  # object. The selector can be 200+ MB; the learner only needs the config and
  # the species-block performance table (for meta-policy training).
  selector_config <- tryCatch(
    policy_selector_config_data(selector@config),
    error = function(e) list()
  )
  merged_config <- merge_config_sections(selector_config, policy_selector_config_data(config))
  species_block_perf <- tryCatch(
    tibble::as_tibble(selector@benchmark$species_block_perf %||% tibble::tibble()),
    error = function(e) tibble::tibble()
  )
  if (nrow(species_block_perf) > 0) {
    random_intercepts <- policy_learner_selection_random_intercepts(merged_config)
    species_block_perf <- attach_meta_policy_random_intercepts(
      policy_perf = species_block_perf,
      candidate_models = selector@candidates@candidate_models,
      random_intercepts = setdiff(random_intercepts, ".split_group")
    )

    # Attach fold-validity and leave-current-species-out policy summaries once
    # before learner fitting so the meta-learner can penalize fragile policy
    # classes without recomputing these summaries in every cross-fit call.
    species_block_perf <- augment_species_block_meta_features(species_block_perf)

    # Augment the stored benchmark rows with a multiplier-domain interval loss
    # before any learner fitting happens. This keeps policy selection in the
    # biomass-multiplier domain and directly penalizes broad policies whose
    # intervals widen under donor averaging.
    benchmark_conf_tbl <- tryCatch(
      tibble::as_tibble(selector@uncertainty$conf_cal %||% tibble::tibble()),
      error = function(e) tibble::tibble()
    )
    structural_uncertainty_weight <- policy_selector_config_value(
      merged_config,
      "structural_uncertainty_weight",
      sections = c("policy", "selection")
    ) %||% 1
    interval_alpha <- policy_selector_config_value(
      merged_config,
      "alpha",
      sections = c("selection", "uncertainty", "policy")
    ) %||% 0.10
    species_block_perf <- augment_multiplier_scores(
      perf_tbl = species_block_perf,
      conf_tbl = benchmark_conf_tbl,
      structural_uncertainty_weight = structural_uncertainty_weight,
      alpha = interval_alpha
    )

    coefficient_diag <- tryCatch(
      policy_coefficient_stability_summary(
        species_performance_table = species_block_perf,
        candidate_models = selector@candidates@candidate_models %||% tibble::tibble(),
        level = 0.95
      ),
      error = function(e) tibble::tibble()
    )
    if (nrow(coefficient_diag) > 0) {
      species_block_perf <- species_block_perf |>
        dplyr::left_join(
          coefficient_diag,
          by = intersect(
            c("policy", "equation_branch_filter"),
            intersect(names(species_block_perf), names(coefficient_diag))
          )
        )
    }
  }

  PolicyLearner(
    selector = NULL,
    config = merged_config,
    training_data = tibble::tibble(),
    crossfit = list(species_block_perf = species_block_perf),
    fitted_model = list(),
    calibration = list()
  )
}

#' Resolve `PolicyLearner` config values
#'
#' @param object A [PolicyLearner] object.
#' @param config Optional config overrides.
#'
#' @return A merged config list.
#'
#' @keywords internal
#' @noRd
policy_learner_config <- function(object,
                                  config = NULL) {
  # selector@config is merged into object@config at construction time, so we
  # no longer need to access the selector here.
  cfg <- merge_config_sections(
    create_configuration_template(),
    merge_config_sections(object@config, policy_selector_config_data(config))
  )
  cfg$selection <- normalize_learner_section(cfg$selection %||% list())
  cfg$uncertainty <- normalize_learner_section(cfg$uncertainty %||% list())
  cfg
}

#' Resolve selection-stage method settings for the policy learner
#'
#' @param cfg Normalized config list.
#' @param fallback Optional fallback settings.
#'
#' @return Normalized method-settings list.
#'
#' @keywords internal
#' @noRd
policy_learner_selection_method_settings <- function(cfg,
                                                     fallback = NULL) {
  settings_now <- policy_selector_config_value(
    cfg,
    "method_settings",
    sections = c("selection", "policy_learner")
  ) %||% fallback

  normalize_meta_policy_method_settings(settings_now)
}

#' Resolve uncertainty-stage method settings for the policy learner
#'
#' @param cfg Normalized config list.
#' @param fallback Optional fallback settings.
#'
#' @return Normalized method-settings list.
#'
#' @keywords internal
#' @noRd
policy_learner_uncertainty_method_settings <- function(cfg,
                                                       fallback = NULL) {
  settings_now <- policy_selector_config_value(
    cfg,
    "method_settings",
    sections = c("uncertainty", "policy_learner")
  ) %||% fallback

  normalize_meta_policy_method_settings(settings_now)
}

#' Resolve selection-stage Super Learner methods
#'
#' @param cfg Normalized config list.
#' @param fallback Optional fallback methods.
#'
#' @return Character vector or `NULL`.
#'
#' @keywords internal
#' @noRd
policy_learner_selection_super_methods <- function(cfg,
                                                   fallback = NULL) {
  policy_selector_config_value(
    cfg,
    "super_methods",
    sections = c("selection", "policy_learner")
  ) %||% fallback
}

#' Resolve uncertainty-stage Super Learner methods
#'
#' @param cfg Normalized config list.
#' @param fallback Optional fallback methods.
#'
#' @return Character vector or `NULL`.
#'
#' @keywords internal
#' @noRd
policy_learner_uncertainty_super_methods <- function(cfg,
                                                     fallback = NULL) {
  policy_selector_config_value(
    cfg,
    "super_methods",
    sections = c("uncertainty", "policy_learner")
  ) %||% fallback
}

#' Resolve active selection-learner random-intercept columns
#'
#' @param cfg Policy-learner configuration.
#'
#' @return Character vector of explicitly configured random-intercept columns.
#'
#' @keywords internal
#' @noRd
policy_learner_selection_random_intercepts <- function(cfg) {
  selection_method <- policy_selector_config_value(
    cfg,
    "method",
    sections = c("selection", "policy_learner")
  ) %||% "super_learner"
  method_settings <- policy_learner_selection_method_settings(cfg)
  methods <- if (identical(selection_method, "super_learner")) {
    available_meta_policy_super_methods(
      policy_learner_selection_super_methods(cfg),
      method_settings = method_settings
    )
  } else {
    selection_method
  }
  active_meta_policy_lmm_random_intercepts(
    methods,
    method_settings = method_settings
  )
}

#' Attach configured random-intercept metadata to benchmark rows
#'
#' @param policy_perf Policy benchmark rows keyed by `anchor_model_id`.
#' @param candidate_models Candidate metadata keyed by `model_id`.
#' @param random_intercepts Configured random-intercept column names.
#'
#' @return Benchmark rows with each configured random-intercept column added.
#'
#' @keywords internal
#' @noRd
attach_meta_policy_random_intercepts <- function(policy_perf,
                                                 candidate_models,
                                                 random_intercepts) {
  policy_perf <- tibble::as_tibble(policy_perf)
  candidate_models <- tibble::as_tibble(candidate_models)
  random_intercepts <- unique(as.character(unlist(
    random_intercepts %||% character(0),
    use.names = FALSE
  )))
  random_intercepts <- random_intercepts[
    !is.na(random_intercepts) & nzchar(random_intercepts)
  ]
  if (length(random_intercepts) == 0L || nrow(policy_perf) == 0L) {
    return(policy_perf)
  }
  if (!"anchor_model_id" %in% names(policy_perf)) {
    stop(
      "Policy benchmark rows must contain `anchor_model_id` to attach random-intercept metadata.",
      call. = FALSE
    )
  }
  if (!"model_id" %in% names(candidate_models)) {
    stop(
      "Candidate models must contain `model_id` to attach random-intercept metadata.",
      call. = FALSE
    )
  }
  missing_intercepts <- setdiff(random_intercepts, names(candidate_models))
  if (length(missing_intercepts) > 0L) {
    stop(
      sprintf(
        "Configured LMM random-intercept column(s) are absent from candidate models: %s",
        paste(missing_intercepts, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  lookup <- candidate_models |>
    dplyr::transmute(
      anchor_model_id = as.character(.data$model_id),
      dplyr::across(
        dplyr::all_of(random_intercepts),
        ~ as.character(.x)
      )
    ) |>
    dplyr::distinct()
  if (anyDuplicated(lookup$anchor_model_id)) {
    stop(
      "Candidate model IDs do not map uniquely to configured random-intercept metadata.",
      call. = FALSE
    )
  }

  existing_intercepts <- intersect(random_intercepts, names(policy_perf))
  if (length(existing_intercepts) > 0L) {
    policy_perf <- policy_perf |>
      dplyr::select(-dplyr::all_of(existing_intercepts))
  }
  policy_perf |>
    dplyr::mutate(anchor_model_id = as.character(.data$anchor_model_id)) |>
    dplyr::left_join(lookup, by = "anchor_model_id")
}

#' Require stored benchmark rows for a `PolicyLearner`
#'
#' @param object A [PolicyLearner] object.
#'
#' @return Species-block performance table.
#'
#' @keywords internal
#' @noRd
policy_learner_species_perf <- function(object) {
  # species_block_perf is extracted from the selector at construction time and
  # stored in the crossfit list so the full selector need not be retained.
  perf_tbl <- tibble::as_tibble(
    object@crossfit$species_block_perf %||% tibble::tibble()
  )
  if (nrow(perf_tbl) == 0) {
    stop(
      "No species-block benchmark rows are stored on this `PolicyLearner`. ",
      "Re-create the learner from the `PolicySelector` using `as_policylearner()`.",
      call. = FALSE
    )
  }
  perf_tbl
}

#' Attach multiplier-domain interval losses to benchmark policy rows
#'
#' Builds benchmark multiplier intervals from the stored conformal radius and
#' structural spread, then evaluates each benchmark row against the known
#' reference multiplier of 1. The resulting interval score stays entirely in the
#' biomass-multiplier domain and penalizes broad ensemble policies when their
#' interval width expands even if the center error shrinks under averaging.
#'
#' The score is computed on the log-multiplier scale rather than on the raw
#' multiplier scale. This keeps the training target numerically stable when a
#' benchmark interval spans several orders of magnitude, while preserving the
#' exact inferential target of whether the multiplier interval covers the true
#' value of 1.
#'
#' @param perf_tbl Benchmark policy-performance table.
#' @param conf_tbl Policy-level conformal calibration table.
#' @param structural_uncertainty_weight Numeric scalar applied to structural
#'   spread in `add_policy_intervals()`.
#' @param alpha Miscoverage level used for the interval-score penalty.
#'
#' @return A tibble with multiplier interval diagnostics added.
#'
#' @keywords internal
#' @noRd
augment_multiplier_scores <- function(perf_tbl,
                                      conf_tbl,
                                      structural_uncertainty_weight = 1,
                                      alpha = 0.10) {
  perf_tbl <- normalize_policy_columns(tibble::as_tibble(perf_tbl))
  conf_tbl <- normalize_policy_columns(tibble::as_tibble(conf_tbl))
  if (nrow(perf_tbl) == 0) {
    return(perf_tbl)
  }

  alpha <- suppressWarnings(as.numeric(alpha)[[1]])
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    alpha <- 0.10
  }

  # Join the benchmark rows to the policy-level conformal radius once, then
  # rebuild the benchmark multiplier intervals using the same helper that the
  # anchor-facing selector uses downstream.
  conf_join <- intersect(
    c("policy", "equation_branch_filter", "q_abs_log", "n", "median_abs_log"),
    names(conf_tbl)
  )
  if (all(c("policy", "equation_branch_filter", "q_abs_log") %in% conf_join)) {
    perf_tbl <- perf_tbl |>
      dplyr::left_join(
        conf_tbl |>
          dplyr::select(dplyr::all_of(conf_join)),
        by = c("policy", "equation_branch_filter")
      )
  } else if (!"q_abs_log" %in% names(perf_tbl)) {
    perf_tbl$q_abs_log <- NA_real_
  }

  perf_tbl <- add_policy_intervals(
    policy_tbl = perf_tbl,
    structural_uncertainty_weight = structural_uncertainty_weight
  )

  # The true benchmark multiplier is 1, so the natural interval-scoring target
  # is 0 on the log-multiplier scale.
  log_truth <- 0
  log_lo <- dplyr::if_else(
    is.finite(perf_tbl$multiplier_lo) & perf_tbl$multiplier_lo > 0,
    log(perf_tbl$multiplier_lo),
    NA_real_
  )
  log_hi <- dplyr::if_else(
    is.finite(perf_tbl$multiplier_hi) & perf_tbl$multiplier_hi > 0,
    log(perf_tbl$multiplier_hi),
    NA_real_
  )
  lower_miss_log <- pmax(log_lo - log_truth, 0)
  upper_miss_log <- pmax(log_truth - log_hi, 0)

  perf_tbl$multiplier_interval_width <- dplyr::if_else(
    is.finite(perf_tbl$interval_log_width),
    pmax(perf_tbl$interval_log_width, 0),
    NA_real_
  )
  perf_tbl$multiplier_interval_score <- dplyr::if_else(
    perf_tbl$valid_prediction &
      is.finite(log_lo) &
      is.finite(log_hi),
    perf_tbl$multiplier_interval_width +
      (2 / alpha) * (lower_miss_log + upper_miss_log),
    NA_real_
  )

  perf_tbl
}

#' Resolve the selection-outcome column for the meta-policy learner
#'
#' @param policy_perf Benchmark policy-performance table.
#' @param cfg Config list.
#' @param outcome_col Optional explicit override.
#'
#' @return Character scalar naming the selection outcome column.
#'
#' @keywords internal
#' @noRd
policy_learner_selection_outcome_col <- function(policy_perf,
                                                 cfg,
                                                 outcome_col = NULL) {
  explicit_outcome_col <- outcome_col %||%
    policy_selector_config_value(
      cfg,
      "outcome_col",
      sections = c("selection", "policy_learner")
    )

  if (!is.null(explicit_outcome_col) &&
    nzchar(as.character(explicit_outcome_col[[1]]))) {
    return(as.character(explicit_outcome_col[[1]]))
  }

  # The selector must learn anchor-level replacement accuracy, not a composite
  # target that already bakes strategy-level interval width into the outcome.
  # Width enters only after the 1-SE score gate, so default back to the pure
  # multiplier-center error unless the config explicitly overrides it.
  if ("error_abs_log" %in% names(policy_perf)) {
    return("error_abs_log")
  }

  "error_abs_log"
}

#' Resolve the uncertainty-calibration outcome column for the policy learner
#'
#' @param predictions Cross-fit prediction table.
#' @param cfg Config list.
#' @param crossfit_obj Stored cross-fit bundle.
#' @param outcome_col Optional explicit override.
#'
#' @return Character scalar naming the uncertainty-calibration outcome column.
#'
#' @keywords internal
#' @noRd
policy_learner_uncertainty_outcome_col <- function(predictions,
                                                   cfg,
                                                   crossfit_obj,
                                                   outcome_col = NULL) {
  predictions <- tibble::as_tibble(predictions)
  outcome_col <- outcome_col %||%
    policy_selector_config_value(cfg, "outcome_col", sections = c("uncertainty", "policy_learner")) %||%
    "error_abs_log"

  outcome_col <- as.character(outcome_col[[1]])
  if (outcome_col %in% names(predictions)) {
    return(outcome_col)
  }
  if ("error_abs_log" %in% names(predictions)) {
    return("error_abs_log")
  }
  if (!is.null(crossfit_obj$outcome_col) && crossfit_obj$outcome_col %in% names(predictions)) {
    return(crossfit_obj$outcome_col)
  }
  if (".outcome" %in% names(predictions)) {
    return(".outcome")
  }

  outcome_col
}

#' Resolve meta-policy feature columns
#'
#' @param cfg Config list.
#' @param policy_perf Policy-performance table.
#'
#' @return Character vector or `NULL`.
#'
#' @keywords internal
#' @noRd
policy_learner_feature_cols <- function(cfg,
                                        policy_perf) {
  feature_cols <- policy_selector_config_value(
    cfg, "feature_cols",
    sections = c("selection", "policy_learner")
  )
  if (is.null(feature_cols)) {
    feature_cols <- default_meta_policy_features(policy_perf)
  } else {
    feature_cols <- as.character(unlist(feature_cols, use.names = FALSE))
    feature_cols <- intersect(unique(feature_cols[!is.na(feature_cols) & nzchar(feature_cols)]), names(policy_perf))
  }
  sanitize_meta_policy_feature_cols(feature_cols)
}

#' Test whether one column is eligible as a default policy-uncertainty feature
#'
#' The uncertainty learner should rely on the unified distance summaries rather
#' than decomposed taxonomic, overlap, or component-distance columns. This keeps
#' the conditional uncertainty model aligned with the package's single combined
#' distance concept instead of re-injecting lower-level pieces by hand. It also
#' keeps the width model anchored to continuous local geometry/support signals
#' rather than sparse categorical policy labels that can destabilize the
#' post-selection interval scale.
#'
#' @param name Column name.
#' @param x Column vector.
#'
#' @return Logical scalar.
#'
#' @keywords internal
#' @noRd
is_default_policy_uncertainty_feature <- function(name,
                                                  x) {
  if (!is_default_meta_policy_feature(name, x)) {
    return(FALSE)
  }

  name_lower <- tolower(name)
  if (is.character(x) || is.factor(x)) {
    return(FALSE)
  }
  if (grepl("taxonomic", name_lower) ||
    grepl("overlap", name_lower) ||
    grepl("species_distance", name_lower) ||
    grepl("trait_gower_distance", name_lower)) {
    return(FALSE)
  }

  TRUE
}

#' Resolve conditional-width feature columns
#'
#' @param cfg Config list.
#' @param policy_perf Policy-performance table.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
policy_learner_uncertainty_feature_cols <- function(cfg,
                                                    policy_perf) {
  feature_cols <- policy_selector_config_value(
    cfg, "feature_cols",
    sections = c("uncertainty", "policy_learner")
  )
  if (is.null(feature_cols)) {
    policy_perf <- tibble::as_tibble(policy_perf)
    return(names(policy_perf)[vapply(
      names(policy_perf),
      function(nm) is_default_policy_uncertainty_feature(nm, policy_perf[[nm]]),
      logical(1)
    )])
  }

  feature_cols <- as.character(unlist(feature_cols, use.names = FALSE))
  intersect(unique(feature_cols[!is.na(feature_cols) & nzchar(feature_cols)]), names(policy_perf))
}

#' Cross-fit a `PolicyLearner`
#'
#' Builds the out-of-fold prediction layer for a policy learner. The method
#' prepares benchmark-derived training rows from the parent selector, resolves
#' feature columns and learner controls, splits rows by the requested anchor or
#' group blocking column, and fits the configured meta-policy learner on each
#' training fold.
#'
#' The stored cross-fit bundle contains the fold predictions, the prepared
#' training data, the selected outcome column, feature columns, fold settings,
#' and active learner methods. Those outputs are required by
#' [calibrate_uncertainty()] because post-selection uncertainty is estimated
#' from predictions made on held-out benchmark rows.
#'
#' @name crossfit.PolicyLearner
#' @usage NULL
#'
#' @param object A [PolicyLearner] object.
#' @param policy_perf Optional species-block performance table override. When
#'   omitted, the learner uses the benchmark rows stored on its selector.
#' @param group_col Optional grouping column used for fold blocking so related
#'   rows are kept in the same outer fold.
#' @param n_folds Optional number of outer cross-validation folds.
#' @param selection_method Optional meta-policy learner method, including
#'   `"super_learner"` or one of the registered base learner aliases.
#' @param seed Optional integer seed.
#' @param feature_cols Optional feature-column override for the meta-policy
#'   learner design matrix.
#' @param outcome_col Optional outcome-column override for the target being
#'   learned.
#' @param outcome_transform Optional outcome transform.
#' @param lambda_rule Optional glmnet lambda-selection rule.
#' @param alpha Optional elastic-net alpha.
#' @param inner_folds Optional number of inner tuning folds.
#' @param selection_super_methods Optional super-learner base methods.
#' @param metalearner_loss Optional super-learner loss name.
#' @param selection_method_settings Optional selection-learner method-settings
#'   override.
#' @param method_settings Shared method-settings override.
#' @param workers Optional worker count.
#' @param progress Optional logical scalar controlling progress messages.
#' @param config Optional config override.
#'
#' @return An updated [PolicyLearner] object with stored out-of-fold
#'   predictions, prepared training data, and resolved cross-fit settings.
S7::method(crossfit, PolicyLearner) <- function(object,
                                                policy_perf = NULL,
                                                group_col = NULL,
                                                n_folds = NULL,
                                                selection_method = NULL,
                                                seed = NULL,
                                                feature_cols = NULL,
                                                outcome_col = NULL,
                                                outcome_transform = NULL,
                                                lambda_rule = NULL,
                                                alpha = NULL,
                                                inner_folds = NULL,
                                                selection_super_methods = NULL,
                                                metalearner_loss = NULL,
                                                selection_method_settings = NULL,
                                                method_settings = NULL,
                                                workers = NULL,
                                                progress = NULL,
                                                config = NULL) {
  cfg <- policy_learner_config(object, config)
  policy_perf <- tibble::as_tibble(policy_perf %||% policy_learner_species_perf(object))
  # Resolve the learner controls once so the training-frame preparation and
  # the fold fit both use one coherent config snapshot.
  feature_cols <- feature_cols %||% policy_learner_feature_cols(cfg, policy_perf)
  outcome_col <- policy_learner_selection_outcome_col(
    policy_perf = policy_perf,
    cfg = cfg,
    outcome_col = outcome_col
  )
  outcome_clip_quantile <- policy_selector_config_value(
    cfg,
    "outcome_clip_quantile",
    sections = c("selection", "policy_learner")
  )
  group_col <- group_col %||%
    policy_selector_config_value(cfg, "group_col", sections = c("selection", "policy_learner"))
  selection_method <- selection_method %||%
    policy_selector_config_value(cfg, "method", sections = c("selection", "policy_learner"))
  outcome_transform <- outcome_transform %||%
    policy_selector_config_value(cfg, "outcome_transform", sections = c("selection", "policy_learner"))
  lambda_rule <- lambda_rule %||%
    policy_selector_config_value(cfg, "lambda_rule", sections = c("selection", "policy_learner"))
  alpha <- alpha %||%
    policy_selector_config_value(cfg, "alpha", sections = c("selection", "policy_learner"))
  n_folds <- n_folds %||%
    policy_selector_config_value(cfg, "n_folds", sections = c("selection", "policy_learner"))
  seed <- seed %||%
    policy_selector_config_value(cfg, "seed", sections = c("selection", "policy_learner"))
  inner_folds <- inner_folds %||%
    policy_selector_config_value(cfg, "inner_folds", sections = c("selection", "policy_learner"))
  selection_super_methods <- selection_super_methods %||%
    policy_learner_selection_super_methods(cfg)
  metalearner_loss <- metalearner_loss %||%
    policy_selector_config_value(cfg, "loss", sections = c("selection", "policy_learner"))
  # Resolve the selection learner settings explicitly so the cross-fit path no
  # longer depends on the shared `method_settings` alias.
  selection_method_settings <- selection_method_settings %||%
    method_settings %||%
    policy_learner_selection_method_settings(cfg)
  selection_active_methods <- if (identical(selection_method, "super_learner")) {
    available_meta_policy_super_methods(
      selection_super_methods,
      method_settings = selection_method_settings
    )
  } else {
    selection_method
  }
  selection_lmm_random_intercepts <- active_meta_policy_lmm_random_intercepts(
    selection_active_methods,
    method_settings = selection_method_settings
  )
  workers <- workers %||%
    policy_selector_config_value(cfg, "workers", sections = c("selection", "policy_learner"))
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = c("selection", "policy_learner"))
  report_progress(
    progress,
    sprintf(
      "Cross-fitting policy learner: method=%s, %d folds, %d workers, %d benchmark rows.",
      selection_method %||% "super_learner",
      n_folds %||% 10L,
      workers %||% 1L,
      nrow(policy_perf)
    )
  )
  group_col <- resolve_meta_policy_group_col(policy_perf, group_col = group_col)

  # Materialize the derived training frame before fitting so the prepared data
  # are retained alongside the out-of-fold predictions.
  training_data <- prepare_meta_policy_data(
    policy_perf = policy_perf,
    outcome_col = outcome_col,
    feature_cols = feature_cols,
    outcome_clip_quantile = outcome_clip_quantile,
    retain_cols = c(group_col, selection_lmm_random_intercepts)
  )

  crossfit_obj <- crossfit_meta_policy_learner(
    policy_perf = policy_perf,
    group_col = group_col,
    n_folds = n_folds,
    method = selection_method,
    seed = seed,
    feature_cols = feature_cols,
    outcome_col = outcome_col,
    outcome_clip_quantile = outcome_clip_quantile,
    outcome_transform = outcome_transform,
    lambda_rule = lambda_rule,
    alpha = alpha,
    inner_folds = inner_folds,
    super_methods = selection_super_methods,
    metalearner_loss = metalearner_loss,
    method_settings = selection_method_settings,
    workers = workers,
    progress = progress
  )
  report_progress(progress, "Completed policy-learner cross-fit.")

  # Persist both the cross-fit bundle and the resolved controls so later fit
  # and calibration stages can reuse the exact same settings.
  policy_learner_rebuild(
    object,
    config = cfg,
    training_data = training_data,
    crossfit = list(
      species_block_perf = object@crossfit$species_block_perf,
      result = crossfit_obj,
      outcome_col = outcome_col,
      uncertainty_outcome_col = policy_learner_uncertainty_outcome_col(
        predictions = tibble::as_tibble(crossfit_obj$predictions %||% tibble::tibble()),
        cfg = cfg,
        crossfit_obj = crossfit_obj,
        outcome_col = NULL
      ),
      outcome_clip_quantile = outcome_clip_quantile,
      outcome_clip_cap = crossfit_obj$outcome_clip_cap %||% attr(training_data, "outcome_clip_cap"),
      feature_cols = feature_cols,
      group_col = group_col,
      selection_method = selection_method,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      alpha = alpha,
      inner_folds = inner_folds,
      selection_super_methods = selection_super_methods,
      metalearner_loss = metalearner_loss,
      selection_method_settings = selection_method_settings,
      method_settings = selection_method_settings,
      n_folds = as.integer(n_folds),
      seed = as.integer(seed)
    ),
    fitted_model = list(),
    calibration = list()
  )
}

#' Fit a `PolicyLearner`
#'
#' Trains the final meta-policy learner on the full benchmark-derived training
#' table. Unlike [crossfit()], this step does not hold out folds; it uses all
#' available training rows to fit the model that will score candidate policies
#' for new reference anchors.
#'
#' The method reuses the feature columns, outcome transformation, learner
#' method, and method settings resolved during cross-fitting unless explicit
#' overrides are supplied. The returned learner stores the fitted model and
#' clears stale prediction state that depends on an earlier fit.
#'
#' @name fit.PolicyLearner
#' @usage NULL
#'
#' @param object A [PolicyLearner] object.
#' @param training_data Optional prepared learner training table. When omitted,
#'   the method uses the training table stored during [crossfit()].
#' @param selection_method Optional meta-policy learner method override.
#' @param feature_cols Optional feature-column override for the final fit.
#' @param outcome_transform Optional outcome transform.
#' @param alpha Optional elastic-net alpha.
#' @param lambda_rule Optional glmnet lambda-selection rule.
#' @param inner_folds Optional number of inner tuning folds.
#' @param seed Optional integer seed.
#' @param selection_super_methods Optional super-learner base methods.
#' @param metalearner_loss Optional super-learner loss name.
#' @param selection_method_settings Optional selection-learner method-settings
#'   override.
#' @param method_settings Shared method-settings override.
#' @param progress Optional logical scalar controlling progress messages.
#' @param config Optional config override.
#'
#' @return An updated [PolicyLearner] object with the fitted final learner,
#'   model metadata, and resolved fit settings.
S7::method(fit, PolicyLearner) <- function(object,
                                           training_data = NULL,
                                           selection_method = NULL,
                                           feature_cols = NULL,
                                           outcome_transform = NULL,
                                           alpha = NULL,
                                           lambda_rule = NULL,
                                           inner_folds = NULL,
                                           seed = NULL,
                                           selection_super_methods = NULL,
                                           metalearner_loss = NULL,
                                           selection_method_settings = NULL,
                                           method_settings = NULL,
                                           progress = NULL,
                                           config = NULL) {
  cfg <- policy_learner_config(object, config)
  policy_perf <- policy_learner_species_perf(object)
  crossfit_obj <- object@crossfit
  feature_cols <- feature_cols %||% crossfit_obj$feature_cols %||% policy_learner_feature_cols(cfg, policy_perf)
  outcome_col <- policy_learner_selection_outcome_col(
    policy_perf = policy_perf,
    cfg = cfg,
    outcome_col = crossfit_obj$outcome_col
  )
  outcome_clip_quantile <- crossfit_obj$outcome_clip_quantile %||%
    policy_selector_config_value(cfg, "outcome_clip_quantile", sections = c("selection", "policy_learner"))
  group_col <- crossfit_obj$group_col %||%
    policy_selector_config_value(cfg, "group_col", sections = c("selection", "policy_learner"))
  selection_method <- selection_method %||% crossfit_obj$selection_method %||%
    policy_selector_config_value(cfg, "method", sections = c("selection", "policy_learner"))
  outcome_transform <- outcome_transform %||% crossfit_obj$outcome_transform %||%
    policy_selector_config_value(cfg, "outcome_transform", sections = c("selection", "policy_learner"))
  lambda_rule <- lambda_rule %||% crossfit_obj$lambda_rule %||%
    policy_selector_config_value(cfg, "lambda_rule", sections = c("selection", "policy_learner"))
  alpha <- alpha %||% crossfit_obj$alpha %||%
    policy_selector_config_value(cfg, "alpha", sections = c("selection", "policy_learner"))
  inner_folds <- inner_folds %||% crossfit_obj$inner_folds %||%
    policy_selector_config_value(cfg, "inner_folds", sections = c("selection", "policy_learner"))
  seed <- seed %||% crossfit_obj$seed %||%
    policy_selector_config_value(cfg, "seed", sections = c("selection", "policy_learner"))
  selection_super_methods <- selection_super_methods %||% crossfit_obj$selection_super_methods %||%
    policy_learner_selection_super_methods(cfg)
  metalearner_loss <- metalearner_loss %||% crossfit_obj$metalearner_loss %||%
    policy_selector_config_value(cfg, "loss", sections = c("selection", "policy_learner"))
  # Prefer the explicit selection-stage override, then fall back to the stored
  # cross-fit settings and finally the config defaults.
  selection_method_settings <- selection_method_settings %||%
    method_settings %||% crossfit_obj$selection_method_settings %||%
    crossfit_obj$method_settings %||%
    policy_learner_selection_method_settings(cfg)
  selection_active_methods <- if (identical(selection_method, "super_learner")) {
    available_meta_policy_super_methods(
      selection_super_methods,
      method_settings = selection_method_settings
    )
  } else {
    selection_method
  }
  selection_lmm_random_intercepts <- active_meta_policy_lmm_random_intercepts(
    selection_active_methods,
    method_settings = selection_method_settings
  )
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = c("selection", "policy_learner"))
  report_progress(progress, "Fitting final policy learner.")
  group_col <- resolve_meta_policy_group_col(policy_perf, group_col = group_col)

  # Reuse the stored prepared frame when available; otherwise rebuild it from
  # the benchmark rows with the resolved feature and outcome settings.
  training_data <- tibble::as_tibble(training_data %||% object@training_data)
  if (nrow(training_data) == 0) {
    training_data <- prepare_meta_policy_data(
      policy_perf = policy_perf,
      outcome_col = outcome_col,
      feature_cols = feature_cols,
      outcome_clip_quantile = outcome_clip_quantile,
      retain_cols = c(group_col, selection_lmm_random_intercepts)
    )
  }
  # Restore the grouping column without duplicating the training table.
  # The prepared selector frame already carries anchor-level identifiers such as
  # `anchor_species`; when that grouping column is present we can assign
  # `.split_group` directly. Joining only on `policy` + branch explodes the
  # training table and silently turns one benchmark row into many duplicates.
  if (!".split_group" %in% names(training_data)) {
    if (group_col %in% names(training_data)) {
      training_data$.split_group <- training_data[[group_col]]
    } else if (group_col %in% names(policy_perf)) {
      group_map <- tibble::as_tibble(policy_perf) |>
        dplyr::select(
          dplyr::any_of(c(
            "anchor_model_id",
            "anchor_species",
            "reference_anchor_model_id",
            "reference_anchor_species",
            "policy",
            "equation_branch_filter"
          )),
          .split_group = dplyr::all_of(group_col)
        ) |>
        dplyr::distinct()

      preferred_keys <- c(
        "anchor_model_id",
        "reference_anchor_model_id",
        "anchor_species",
        "reference_anchor_species",
        "policy",
        "equation_branch_filter"
      )
      join_keys <- preferred_keys[
        preferred_keys %in% names(group_map) &
          preferred_keys %in% names(training_data)
      ]

      if (length(join_keys) > 0) {
        training_data <- training_data |>
          dplyr::left_join(group_map, by = join_keys)
      }
    }
  }

  # Fit the final learner on the full prepared table after any required group
  # metadata have been joined back in.
  report_progress(
    progress,
    sprintf("Fitting final policy learner on %d rows (method=%s) ...", nrow(training_data), selection_method)
  )
  final_model <- fit_meta_policy_learner(
    training_data = training_data,
    method = selection_method,
    feature_cols = feature_cols,
    outcome_transform = outcome_transform,
    alpha = alpha,
    lambda_rule = lambda_rule,
    inner_folds = inner_folds,
    seed = seed,
    super_methods = selection_super_methods,
    metalearner_loss = metalearner_loss,
    method_settings = selection_method_settings,
    progress = isTRUE(progress)
  )
  report_progress(progress, "Completed final policy-learner fit.")

  # Store the final learner and the controls needed by downstream prediction
  # and uncertainty-calibration stages.
  policy_learner_rebuild(
    object,
    config = cfg,
    training_data = training_data,
    fitted_model = list(
      model = final_model,
      outcome_col = outcome_col,
      outcome_clip_quantile = outcome_clip_quantile,
      outcome_clip_cap = attr(training_data, "outcome_clip_cap"),
      feature_cols = feature_cols,
      group_col = group_col,
      selection_method = selection_method,
      selection_super_methods = selection_super_methods,
      selection_method_settings = selection_method_settings,
      method_settings = selection_method_settings
    ),
    calibration = list()
  )
}

#' Extract anchor lookup keys from a calibration table
#'
#' @param tbl Calibration-like table.
#'
#' @return Distinct anchor lookup tibble.
#'
#' @keywords internal
#' @noRd
policy_learner_anchor_lookup <- function(tbl) {
  tbl <- tibble::as_tibble(tbl)
  if (nrow(tbl) == 0) {
    return(tibble::tibble(
      anchor_model_id = character(),
      anchor_species = character(),
      anchor_family = character()
    ))
  }

  tibble::tibble(
    anchor_model_id = if ("anchor_model_id" %in% names(tbl)) as.character(tbl$anchor_model_id) else rep(NA_character_, nrow(tbl)),
    anchor_species = if ("anchor_species" %in% names(tbl)) as.character(tbl$anchor_species) else rep(NA_character_, nrow(tbl)),
    anchor_family = if ("anchor_family" %in% names(tbl)) as.character(tbl$anchor_family) else rep(NA_character_, nrow(tbl))
  ) |>
    dplyr::filter(
      !is.na(.data$anchor_model_id) | !is.na(.data$anchor_species) | !is.na(.data$anchor_family)
    ) |>
    dplyr::distinct()
}

#' Normalize one policy-learner context table
#'
#' @param tbl Input table.
#' @param anchor_lookup Optional anchor lookup table.
#'
#' @return Tibble with normalized policy context columns.
#'
#' @keywords internal
#' @noRd
policy_learner_prepare_context <- function(tbl,
                                           anchor_lookup = NULL) {
  # Normalize the anchor keys and policy labels used by the lookup cascade.
  out <- tibble::as_tibble(tbl)
  if (nrow(out) == 0) {
    return(out)
  }

  if (!"anchor_model_id" %in% names(out)) {
    out$anchor_model_id <- NA_character_
  }
  if (!"anchor_species" %in% names(out)) {
    out$anchor_species <- NA_character_
  }
  if (!"anchor_family" %in% names(out)) {
    out$anchor_family <- NA_character_
  }
  out$anchor_model_id <- as.character(out$anchor_model_id)
  out$anchor_species <- as.character(out$anchor_species)
  out$anchor_family <- as.character(out$anchor_family)

  anchor_lookup <- tibble::as_tibble(anchor_lookup %||% tibble::tibble())
  if (nrow(anchor_lookup) > 0) {
    anchor_lookup <- anchor_lookup |>
      dplyr::mutate(
        anchor_model_id = as.character(.data$anchor_model_id),
        anchor_species = as.character(.data$anchor_species),
        anchor_family = as.character(.data$anchor_family)
      )
    if ("anchor_model_id" %in% names(out)) {
      out <- out |>
        dplyr::left_join(
          anchor_lookup |>
            dplyr::select("anchor_model_id", lookup_anchor_species = "anchor_species", lookup_anchor_family = "anchor_family") |>
            dplyr::distinct(),
          by = "anchor_model_id"
        ) |>
        dplyr::mutate(
          anchor_species = dplyr::coalesce(.data$anchor_species, .data$lookup_anchor_species),
          anchor_family = dplyr::coalesce(.data$anchor_family, .data$lookup_anchor_family)
        ) |>
        dplyr::select(-dplyr::any_of(c("lookup_anchor_species", "lookup_anchor_family")))
    }
    if ("anchor_species" %in% names(out)) {
      out <- out |>
        dplyr::left_join(
          anchor_lookup |>
            dplyr::select("anchor_species", lookup_anchor_family_by_species = "anchor_family") |>
            dplyr::filter(!is.na(.data$anchor_species), !is.na(.data$lookup_anchor_family_by_species)) |>
            dplyr::distinct(),
          by = "anchor_species"
        ) |>
        dplyr::mutate(
          anchor_family = dplyr::coalesce(.data$anchor_family, .data$lookup_anchor_family_by_species)
        ) |>
        dplyr::select(-dplyr::any_of("lookup_anchor_family_by_species"))
    }
  }

  if ("policy" %in% names(out)) {
    out$policy <- as.character(out$policy)
  } else {
    out$policy <- NA_character_
  }
  if ("equation_branch_filter" %in% names(out)) {
    out$equation_branch_filter <- resolve_policy_branch_filters(out)
  } else {
    out$equation_branch_filter <- NA_character_
  }
  if ("post_selection_support_bin" %in% names(out)) {
    out$post_selection_support_bin <- as.character(out$post_selection_support_bin)
  } else {
    out$post_selection_support_bin <- NA_character_
  }
  if (!"candidate_pool" %in% names(out)) {
    out$candidate_pool <- NA_character_
  } else {
    out$candidate_pool <- as.character(out$candidate_pool)
  }
  if (!"aggregation_method" %in% names(out)) {
    out$aggregation_method <- NA_character_
  } else {
    out$aggregation_method <- as.character(out$aggregation_method)
  }
  if (!"policy_family" %in% names(out)) {
    out$policy_family <- NA_character_
  } else {
    out$policy_family <- as.character(out$policy_family)
  }
  if (!"local_weighted_mean_combined_distance" %in% names(out)) {
    out$local_weighted_mean_combined_distance <- NA_real_
  }
  if (!"local_min_combined_distance" %in% names(out)) {
    out$local_min_combined_distance <- NA_real_
  }
  if (!"local_effective_support" %in% names(out)) {
    out$local_effective_support <- NA_real_
  }

  out$specificity_rank <- policy_specificity_rank(
    policy = out$policy,
    candidate_pool = out$candidate_pool,
    aggregation_method = out$aggregation_method,
    policy_family = out$policy_family,
    equation_branch_filter = out$equation_branch_filter %||% NULL
  )

  out
}

#' Select one calibration row per anchor after meta-score filtering
#'
#' @param tbl Candidate calibration rows.
#' @param max_selection_tolerance Optional absolute score slack.
#' @param one_se_multiplier Multiplier used for benchmark slack.
#'
#' @return Tibble of selected calibration rows.
#'
#' @keywords internal
#' @noRd
policy_learner_select_calibration_rows <- function(tbl,
                                                   max_selection_tolerance = NULL,
                                                   one_se_multiplier = 1) {
  # Reuse the same anchor-level selection logic used by the runtime selector.
  tbl <- policy_learner_prepare_context(tbl)
  if (nrow(tbl) == 0) {
    return(tbl)
  }

  tbl$anchor_selection_local_distance <- dplyr::coalesce(
    tbl$local_weighted_mean_combined_distance,
    tbl$local_min_combined_distance
  )
  has_benchmark_slack <- all(
    c("one_se_threshold", "best_mean_species_median_abs_log") %in% names(tbl)
  )
  if (!has_benchmark_slack) {
    tbl$one_se_threshold <- NA_real_
    tbl$best_mean_species_median_abs_log <- NA_real_
  }

  tbl |>
    dplyr::filter(.data$selection_valid, is.finite(.data$.meta_predicted_score)) |>
    dplyr::group_by(.data$anchor_model_id, .data$anchor_species) |>
    dplyr::mutate(
      .meta_score_min = min(.data$.meta_predicted_score, na.rm = TRUE),
      .meta_benchmark_score_slack = if (has_benchmark_slack) {
        suppressWarnings(
          min(.data$one_se_threshold - .data$best_mean_species_median_abs_log, na.rm = TRUE)
        )
      } else {
        0
      },
      .meta_benchmark_score_slack = dplyr::if_else(
        is.finite(.data$.meta_benchmark_score_slack),
        .data$.meta_benchmark_score_slack,
        0
      ),
      .meta_score_threshold = if (is.finite(max_selection_tolerance)) {
        .data$.meta_score_min + pmax(0, max_selection_tolerance)
      } else {
        # Match the anchor-level selector by reusing the leave-one-species-out
        # benchmark one-standard-error slack on the meta-score scale.
        .data$.meta_score_min + pmax(0, one_se_multiplier * .data$.meta_benchmark_score_slack)
      }
    ) |>
    dplyr::filter(
      .data$.meta_predicted_score <= .data$.meta_score_threshold
    ) |>
    dplyr::arrange(
      .data$.meta_predicted_score,
      .data$policy,
      .data$equation_branch_filter,
      .by_group = TRUE
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}

#' Return the ordered lookup specifications for local calibration
#'
#' @return Named list of grouping-column definitions.
#'
#' @keywords internal
#' @noRd
policy_learner_lookup_specs <- function() {
  list(
    species_policy_branch = c("anchor_species", "policy", "equation_branch_filter"),
    species_policy = c("anchor_species", "policy"),
    policy_branch = c("policy", "equation_branch_filter"),
    policy = c("policy"),
    species_branch = c("anchor_species", "equation_branch_filter"),
    species = c("anchor_species"),
    family_policy_branch = c("anchor_family", "policy", "equation_branch_filter"),
    family_policy = c("anchor_family", "policy"),
    family_branch = c("anchor_family", "equation_branch_filter"),
    family = c("anchor_family"),
    branch = c("equation_branch_filter")
  )
}

#' Summarize one local conformal lookup table
#'
#' @param tbl Calibration tibble containing the lookup value.
#' @param group_cols Grouping columns defining one lookup stratum.
#' @param value_col Numeric calibration column to summarize.
#' @param alpha Miscoverage level used for the conformal quantile.
#' @param source_label Character label describing the lookup stratum.
#' @param min_scores Minimum support used to control shrinkage strength.
#' @param global_value Global conformal fallback used as the shrinkage target.
#'
#' @return A tibble with one pooled conformal factor per lookup stratum.
#'
#' @keywords internal
#' @noRd
policy_learner_summarize_lookup_table <- function(tbl,
                                                  group_cols,
                                                  value_col,
                                                  alpha,
                                                  source_label,
                                                  min_scores = 1L,
                                                  global_value = NA_real_) {
  tbl <- tibble::as_tibble(tbl)
  if (nrow(tbl) == 0 || !value_col %in% names(tbl) || !all(group_cols %in% names(tbl))) {
    return(tibble::tibble())
  }

  # Keep only finite calibration values before any grouped quantile work.
  value_vec <- suppressWarnings(as.numeric(tbl[[value_col]]))
  tbl[[value_col]] <- value_vec
  tbl <- tbl |>
    dplyr::filter(is.finite(.data[[value_col]]))
  if (nrow(tbl) == 0) {
    return(tibble::tibble())
  }

  min_scores <- suppressWarnings(as.integer(min_scores[[1]] %||% 1L))
  if (!is.finite(min_scores) || min_scores < 1L) {
    min_scores <- 1L
  }
  global_value <- suppressWarnings(as.numeric(global_value[[1]] %||% NA_real_))

  # Pool thin local groups back toward the global conformal factor rather than
  # letting a one-row subgroup dominate the lookup cascade.
  tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_scores = dplyr::n(),
      .cal_value_raw = conformal_quantile(.data[[value_col]], alpha = alpha),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      .cal_weight = pmin(1, .data$n_scores / min_scores),
      .cal_value = dplyr::if_else(
        is.finite(global_value) & is.finite(.data$.cal_value_raw),
        .data$.cal_weight * .data$.cal_value_raw + (1 - .data$.cal_weight) * global_value,
        dplyr::if_else(
          is.finite(.data$.cal_value_raw),
          .data$.cal_value_raw,
          global_value
        )
      ),
      .cal_source = dplyr::if_else(
        .data$.cal_weight < 1,
        paste0(source_label, "_shrunk"),
        source_label
      )
    ) |>
    dplyr::select(-dplyr::any_of(c(".cal_value_raw", ".cal_weight")))
}

#' Build pooled local conformal lookup tables
#'
#' @param tbl Calibration tibble containing local context columns.
#' @param value_col Numeric calibration column to summarize.
#' @param alpha Miscoverage level used for the conformal quantile.
#' @param global_default Global fallback value used when no local summary exists.
#' @param min_scores Minimum support used to control shrinkage strength.
#'
#' @return A list containing the global fallback and grouped lookup tables.
#'
#' @keywords internal
#' @noRd
policy_learner_build_local_lookup <- function(tbl,
                                              value_col,
                                              alpha,
                                              global_default = NA_real_,
                                              min_scores = 1L) {
  valid <- policy_learner_prepare_context(tbl)
  if (!value_col %in% names(valid)) {
    return(list(
      global_value = suppressWarnings(as.numeric(global_default)),
      tables = list(),
      anchor_lookup = policy_learner_anchor_lookup(valid)
    ))
  }

  valid[[value_col]] <- suppressWarnings(as.numeric(valid[[value_col]]))
  valid <- valid |>
    dplyr::filter(is.finite(.data[[value_col]]))

  global_value <- if (nrow(valid) > 0) {
    conformal_quantile(valid[[value_col]], alpha = alpha)
  } else {
    NA_real_
  }
  if (!is.finite(global_value)) {
    global_value <- suppressWarnings(as.numeric(global_default))
  }

  lookup_specs <- policy_learner_lookup_specs()
  lookup_tables <- purrr::imap(
    lookup_specs,
    function(group_cols, source_label) {
      # Build one lookup per context grain so prediction can fall back from
      # specific to broad strata without re-fitting anything.
      policy_learner_summarize_lookup_table(
        tbl = valid,
        group_cols = group_cols,
        value_col = value_col,
        alpha = alpha,
        source_label = source_label,
        min_scores = min_scores,
        global_value = global_value
      )
    }
  )

  list(
    global_value = global_value,
    tables = lookup_tables,
    anchor_lookup = policy_learner_anchor_lookup(valid)
  )
}

#' Apply pooled local conformal lookups to a prediction table
#'
#' @param tbl Prediction or calibration tibble.
#' @param lookup Lookup bundle returned by `policy_learner_build_local_lookup()`.
#' @param value_col Output column receiving the resolved conformal factor.
#' @param source_col Output column receiving the lookup source label.
#' @param n_col Output column receiving the lookup support count.
#'
#' @return A tibble with resolved local conformal factors attached.
#'
#' @keywords internal
#' @noRd
policy_learner_apply_local_lookup <- function(tbl,
                                              lookup,
                                              value_col,
                                              source_col,
                                              n_col) {
  out <- policy_learner_prepare_context(
    tbl,
    anchor_lookup = lookup$anchor_lookup %||% NULL
  )
  if (nrow(out) == 0) {
    return(out)
  }

  lookup_specs <- policy_learner_lookup_specs()
  used_specs <- character(0)
  for (spec_name in names(lookup_specs)) {
    # Join every available lookup grain once, then resolve precedence after the
    # joins so the most specific usable match wins.
    join_tbl <- tibble::as_tibble((lookup$tables %||% list())[[spec_name]] %||% tibble::tibble())
    if (nrow(join_tbl) == 0) {
      next
    }
    by_cols <- lookup_specs[[spec_name]]
    if (!all(by_cols %in% names(out)) || !all(by_cols %in% names(join_tbl))) {
      next
    }
    temp_value_col <- paste0(".", value_col, "__", spec_name)
    temp_source_col <- paste0(".", source_col, "__", spec_name)
    temp_n_col <- paste0(".", n_col, "__", spec_name)
    names(join_tbl)[names(join_tbl) == ".cal_value"] <- temp_value_col
    names(join_tbl)[names(join_tbl) == ".cal_source"] <- temp_source_col
    names(join_tbl)[names(join_tbl) == "n_scores"] <- temp_n_col
    out <- dplyr::left_join(out, join_tbl, by = by_cols)
    used_specs <- c(used_specs, spec_name)
  }

  default_value <- suppressWarnings(as.numeric(lookup$global_value %||% NA_real_))
  out[[value_col]] <- rep(default_value, nrow(out))
  out[[source_col]] <- rep("global_scale_conformal", nrow(out))
  out[[n_col]] <- rep(NA_real_, nrow(out))

  # Apply the lookup tables from broadest to most specific so later passes can
  # override earlier fallback values when a richer local match exists.
  for (spec_name in rev(used_specs)) {
    temp_value_col <- paste0(".", value_col, "__", spec_name)
    temp_source_col <- paste0(".", source_col, "__", spec_name)
    temp_n_col <- paste0(".", n_col, "__", spec_name)
    temp_value <- suppressWarnings(as.numeric(out[[temp_value_col]]))
    keep <- is.finite(temp_value)
    if (!any(keep)) {
      next
    }
    out[[value_col]][keep] <- temp_value[keep]
    out[[source_col]][keep] <- as.character(out[[temp_source_col]][keep])
    out[[n_col]][keep] <- suppressWarnings(as.numeric(out[[temp_n_col]][keep]))
  }

  drop_cols <- unlist(lapply(used_specs, function(spec_name) {
    c(
      paste0(".", value_col, "__", spec_name),
      paste0(".", source_col, "__", spec_name),
      paste0(".", n_col, "__", spec_name)
    )
  }), use.names = FALSE)
  if (length(drop_cols) > 0) {
    out <- out |>
      dplyr::select(-dplyr::any_of(drop_cols))
  }

  out
}

#' Calibrate post-selection uncertainty for a `PolicyLearner`
#'
#' Converts cross-fitted learner predictions into post-selection conformal
#' calibration tables. The method scores the stored out-of-fold predictions,
#' keeps the rows that would be selected under the learner's meta-policy score,
#' estimates absolute log-residual quantiles, and optionally builds support-bin
#' calibration so intervals can widen when an anchor has little local support.
#'
#' This method expects [crossfit()] to have populated the learner's cross-fit
#' results. It does not refit the meta-policy learner; it uses the cross-fitted
#' predictions to quantify how much post-selection error remains after the
#' learner chooses policies.
#'
#' @name calibrate_uncertainty.PolicyLearner
#' @usage NULL
#'
#' @param object A [PolicyLearner] object.
#' @param predictions Optional cross-fit prediction table override. When
#'   omitted, stored cross-fit predictions are used.
#' @param outcome_col Optional outcome-column override used to compute
#'   residuals.
#' @param max_selection_tolerance Optional score-tie tolerance used when
#'   retaining calibration rows.
#' @param alpha Optional marginal conformal alpha for the global
#'   post-selection residual threshold.
#' @param bin_alpha Optional support-bin conformal alpha for local-support
#'   residual thresholds.
#' @param min_bin_scores Optional minimum score count required before a support
#'   bin gets its own threshold.
#' @param n_bins Optional number of support bins used to stratify selected rows.
#' @param uncertainty_method Optional uncertainty-learner method override.
#' @param uncertainty_super_methods Optional uncertainty-learner super-learner
#'   base methods override.
#' @param uncertainty_method_settings Optional uncertainty-learner
#'   method-settings override.
#' @param progress Optional logical scalar controlling progress messages.
#' @param config Optional config override.
#'
#' @return An updated [PolicyLearner] object whose calibration property contains
#'   selected calibration rows, residual quantiles, support-bin thresholds,
#'   optional uncertainty learner state, and lookup metadata used by
#'   [stats::predict()] on the learner.
S7::method(calibrate_uncertainty, PolicyLearner) <- function(object,
                                                             predictions = NULL,
                                                             outcome_col = NULL,
                                                             max_selection_tolerance = NULL,
                                                             alpha = NULL,
                                                             bin_alpha = NULL,
                                                             min_bin_scores = NULL,
                                                             n_bins = NULL,
                                                             uncertainty_method = NULL,
                                                             uncertainty_super_methods = NULL,
                                                             uncertainty_method_settings = NULL,
                                                             progress = NULL,
                                                             config = NULL) {
  cfg <- policy_learner_config(object, config)
  if (length(object@crossfit) == 0) {
    stop("No cross-fit results are stored on this `PolicyLearner`.", call. = FALSE)
  }
  crossfit_obj <- object@crossfit
  cal_obj <- object@calibration %||% list()
  # Pull the saved cross-fit predictions and calibration controls before
  # reducing them to the score-minimizing rows used for post-selection
  # conformal calibration.
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = c("uncertainty", "selection", "policy_learner"))
  report_progress(progress, "Calibrating learner uncertainty.")
  predictions <- tibble::as_tibble(predictions %||% crossfit_obj$result$predictions %||% tibble::tibble())
  outcome_col <- policy_learner_uncertainty_outcome_col(
    predictions = predictions,
    cfg = cfg,
    crossfit_obj = crossfit_obj,
    outcome_col = outcome_col %||% crossfit_obj$uncertainty_outcome_col %||% NULL
  )
  if (!outcome_col %in% names(predictions) && ".outcome" %in% names(predictions)) {
    predictions[[outcome_col]] <- predictions$.outcome
  }
  if (!outcome_col %in% names(predictions)) {
    stop(sprintf("Outcome column '%s' was not found in cross-fit predictions.", outcome_col), call. = FALSE)
  }
  calibration_outcome_col <- if (".outcome_raw" %in% names(predictions)) {
    ".outcome_raw"
  } else if (outcome_col %in% names(predictions)) {
    outcome_col
  } else if (".outcome" %in% names(predictions)) {
    ".outcome"
  } else {
    outcome_col
  }

  if ("n_valid_models" %in% names(predictions)) {
    predictions$selection_valid <- is.finite(predictions$n_valid_models) &
      predictions$n_valid_models > 0
  } else if ("n_models" %in% names(predictions)) {
    predictions$selection_valid <- is.finite(predictions$n_models) &
      predictions$n_models > 0
  } else {
    predictions$selection_valid <- is.finite(predictions$.meta_predicted_score)
  }

  max_selection_tolerance <- as.numeric(
    max_selection_tolerance %||%
      cal_obj$max_selection_tolerance %||%
      policy_selector_config_value(cfg, "max_selection_tolerance", sections = c("selection", "policy_learner"))
  )
  one_se_multiplier <- as.numeric(
    policy_selector_config_value(cfg, "one_se_multiplier", sections = c("selection", "policy")) %||% 1
  )
  if (!is.finite(one_se_multiplier)) {
    one_se_multiplier <- 1
  }
  n_bins <- as.integer(
    n_bins %||%
      policy_selector_config_value(cfg, "n_bins", sections = c("selection"))
  )
  support_bin_labels <- policy_selector_config_value(
    cfg,
    "support_bin_labels",
    sections = c("selection")
  )
  predictions <- policy_learner_prepare_context(predictions)
  anchor_lookup <- policy_learner_anchor_lookup(predictions)
  meta_selected <- policy_learner_select_calibration_rows(
    tbl = predictions,
    max_selection_tolerance = if (is.finite(max_selection_tolerance)) max_selection_tolerance else NULL,
    one_se_multiplier = one_se_multiplier
  )

  # Calibration only makes sense if cross-fitting produced at least one
  # usable selected row after the score-minimization step.
  if (nrow(meta_selected) == 0) {
    stop(
      "The cross-fit predictions did not yield any selectable rows for post-selection calibration.",
      call. = FALSE
    )
  }

  predictions <- assign_post_selection_support_bins(
    predictions,
    n_bins = n_bins,
    labels = support_bin_labels
  )
  meta_selected <- assign_post_selection_support_bins(
    meta_selected,
    n_bins = n_bins,
    labels = support_bin_labels
  )
  predictions <- policy_learner_prepare_context(
    predictions,
    anchor_lookup = anchor_lookup
  )
  meta_selected <- policy_learner_prepare_context(
    meta_selected,
    anchor_lookup = anchor_lookup
  )
  meta_support_cutpoints <- unique(stats::quantile(
    meta_selected$post_selection_support_score,
    probs = seq(0, 1, length.out = as.integer(n_bins) + 1L),
    na.rm = TRUE,
    names = FALSE,
    type = 8
  ))

  meta_summary <- meta_selected |>
    dplyr::filter(is.finite(.data[[calibration_outcome_col]])) |>
    dplyr::summarise(
      n_selected_rows = dplyr::n(),
      n_anchor_models = dplyr::n_distinct(anchor_model_id),
      n_species = dplyr::n_distinct(anchor_species),
      median_outcome = stats::median(.data[[calibration_outcome_col]], na.rm = TRUE),
      mean_outcome = mean(.data[[calibration_outcome_col]], na.rm = TRUE),
      median_predicted_score = stats::median(.data$.meta_predicted_score, na.rm = TRUE),
      mean_predicted_score = mean(.data$.meta_predicted_score, na.rm = TRUE),
      .groups = "drop"
    )

  alpha <- alpha %||%
    policy_selector_config_value(cfg, "conformal_alpha", sections = c("selection", "policy")) %||%
    policy_selector_config_value(cfg, "alpha", sections = c("selection"))

  meta_calibration <- meta_selected |>
    dplyr::filter(is.finite(.data[[calibration_outcome_col]])) |>
    dplyr::transmute(
      selected_score_abs_log = .data[[calibration_outcome_col]],
      post_selection_support_bin,
      n_selected_rows = 1L
    )
  meta_simultaneous_calibration <- meta_selected |>
    dplyr::filter(is.finite(.data[[calibration_outcome_col]])) |>
    dplyr::group_by(anchor_species) |>
    dplyr::summarise(
      selected_score_abs_log = max(.data[[calibration_outcome_col]], na.rm = TRUE),
      n_selected_rows = dplyr::n(),
      .groups = "drop"
    )
  meta_q_global <- conformal_quantile(
    meta_calibration$selected_score_abs_log,
    alpha = alpha
  )
  meta_q_simultaneous_global <- conformal_quantile(
    meta_simultaneous_calibration$selected_score_abs_log,
    alpha = alpha
  )
  bin_alpha <- bin_alpha %||%
    policy_selector_config_value(cfg, "bin_alpha", sections = c("selection")) %||%
    policy_selector_config_value(cfg, "conformal_alpha", sections = c("selection", "policy"))
  min_bin_scores <- as.integer(
    min_bin_scores %||%
      policy_selector_config_value(cfg, "min_bin_scores", sections = c("selection"))
  )

  meta_bin_quantiles <- meta_calibration |>
    dplyr::group_by(post_selection_support_bin) |>
    dplyr::summarise(
      n_scores = dplyr::n(),
      q_abs_log_raw = conformal_quantile(
        selected_score_abs_log,
        alpha = bin_alpha
      ),
      median_score_abs_log = stats::median(selected_score_abs_log, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      q_abs_log = dplyr::if_else(
        n_scores >= min_bin_scores & is.finite(q_abs_log_raw),
        q_abs_log_raw,
        meta_q_global
      ),
      used_global_fallback = !(n_scores >= min_bin_scores & is.finite(q_abs_log_raw)),
      calibration_scope = "marginal_selected_row",
      simultaneous_species_max_q_abs_log = meta_q_simultaneous_global,
      interval_log_width = dplyr::if_else(is.finite(q_abs_log), 2 * q_abs_log, NA_real_)
    )

  meta_calibration_coverage <- tibble::tibble(
    n_selected_rows = nrow(meta_selected),
    n_calibration_species = dplyr::n_distinct(meta_selected$anchor_species),
    marginal_q_abs_log = meta_q_global,
    simultaneous_species_max_q_abs_log = meta_q_simultaneous_global,
    empirical_row_coverage = NA_real_,
    empirical_row_coverage_under_simultaneous_interval = NA_real_,
    median_abs_log_error = stats::median(meta_selected[[calibration_outcome_col]], na.rm = TRUE),
    median_interval_log_width = if (is.finite(meta_q_global)) 2 * meta_q_global else NA_real_
  )

  width_feature_cols <- policy_learner_uncertainty_feature_cols(cfg, meta_selected)
  # Resolve the uncertainty learner independently from the selection learner so
  # calibration can use a simpler or differently tuned model family.
  width_method <- uncertainty_method %||% policy_selector_config_value(
    cfg, "method",
    sections = c("uncertainty", "policy_learner")
  ) %||% (object@fitted_model)$selection_method %||% crossfit_obj$selection_method
  uncertainty_method_settings <- uncertainty_method_settings %||%
    policy_learner_uncertainty_method_settings(
      cfg,
      fallback = (object@fitted_model)$selection_method_settings %||%
        crossfit_obj$selection_method_settings %||%
        (object@fitted_model)$method_settings %||%
        crossfit_obj$method_settings
    )
  width_super_methods <- uncertainty_super_methods %||% policy_learner_uncertainty_super_methods(
    cfg,
    fallback = (object@fitted_model)$selection_super_methods %||%
      crossfit_obj$selection_super_methods
  )
  width_group_col <- crossfit_obj$group_col %||% NULL
  if (!is.null(width_group_col) && !width_group_col %in% names(meta_selected)) {
    width_group_col <- NULL
  }

  width_crossfit <- list()
  width_model <- list()
  width_prediction_source <- NA_character_
  width_fit_error <- NULL
  width_warning <- NULL
  width_selected <- meta_selected
  configured_min_width_rows <- policy_selector_config_value(
    cfg,
    "min_selected_rows",
    sections = c("uncertainty", "policy_learner")
  )
  configured_min_bin_scores <- if (length(min_bin_scores) > 0L) min_bin_scores else NULL
  min_width_rows <- as.integer(configured_min_width_rows %||% configured_min_bin_scores %||% 25L)
  if (!is.finite(min_width_rows) || min_width_rows < 1L) {
    min_width_rows <- 25L
  }
  meta_local_q_lookup <- policy_learner_build_local_lookup(
    tbl = meta_selected |>
      dplyr::filter(is.finite(.data[[calibration_outcome_col]])) |>
      dplyr::mutate(.selected_q_abs_log_value = .data[[calibration_outcome_col]]),
    value_col = ".selected_q_abs_log_value",
    alpha = alpha,
    global_default = meta_q_global,
    min_scores = min_bin_scores
  )
  meta_selected_local_q <- policy_learner_apply_local_lookup(
    tbl = meta_selected,
    lookup = meta_local_q_lookup,
    value_col = "selected_q_abs_log_local",
    source_col = "selected_q_abs_log_source",
    n_col = "selected_q_abs_log_n"
  ) |>
    dplyr::mutate(
      selected_q_abs_log_local = dplyr::coalesce(
        suppressWarnings(as.numeric(.data$selected_q_abs_log_local)),
        meta_q_global
      ),
      selected_q_abs_log_source = dplyr::coalesce(
        as.character(.data$selected_q_abs_log_source),
        "global_selected_conformal"
      ),
      selected_q_abs_log_n = dplyr::coalesce(
        suppressWarnings(as.numeric(.data$selected_q_abs_log_n)),
        suppressWarnings(as.numeric(nrow(meta_selected)))
      )
    )
  if (is.null(configured_min_width_rows) && is.null(configured_min_bin_scores)) {
    min_width_rows <- max(
      min_width_rows,
      as.integer(crossfit_obj$n_folds %||% 1L) + 1L
    )
  }

  if (nrow(meta_selected) < min_width_rows) {
    width_warning <- sprintf(
      paste(
        "Skipping conditional-uncertainty regression because only %d selected calibration row(s) were available,",
        "below the minimum of %d. Using pooled selected-row conformal width instead."
      ),
      nrow(meta_selected),
      min_width_rows
    )
    width_selected <- meta_selected |>
      dplyr::mutate(.width_predicted_q_abs_log = 1)
    width_model <- list(model = NULL, feature_cols = width_feature_cols, method = NA_character_)
    width_prediction_source <- "global_selected_conformal"
  } else if (length(width_feature_cols) > 0) {
    attempt_width_method <- function(method_now) {
      width_active_methods <- if (identical(method_now, "super_learner")) {
        available_meta_policy_super_methods(
          width_super_methods,
          method_settings = uncertainty_method_settings
        )
      } else {
        method_now
      }
      width_lmm_random_intercepts <- active_meta_policy_lmm_random_intercepts(
        width_active_methods,
        method_settings = uncertainty_method_settings
      )
      width_crossfit_result <- crossfit_meta_policy_learner(
        policy_perf = meta_selected,
        group_col = width_group_col,
        n_folds = crossfit_obj$n_folds %||% policy_selector_config_value(cfg, "n_folds", sections = c("uncertainty", "policy_learner")),
        method = method_now,
        seed = crossfit_obj$seed %||% policy_selector_config_value(cfg, "seed", sections = c("uncertainty", "policy_learner")),
        feature_cols = width_feature_cols,
        outcome_col = calibration_outcome_col,
        outcome_clip_quantile = NULL,
        outcome_transform = crossfit_obj$outcome_transform %||% policy_selector_config_value(cfg, "outcome_transform", sections = c("uncertainty", "policy_learner")),
        lambda_rule = crossfit_obj$lambda_rule %||% policy_selector_config_value(cfg, "lambda_rule", sections = c("uncertainty", "policy_learner")),
        alpha = crossfit_obj$alpha,
        inner_folds = crossfit_obj$inner_folds %||% policy_selector_config_value(cfg, "inner_folds", sections = c("uncertainty", "policy_learner")),
        super_methods = width_super_methods,
        metalearner_loss = crossfit_obj$metalearner_loss %||% policy_selector_config_value(cfg, "loss", sections = c("uncertainty", "policy_learner")),
        method_settings = uncertainty_method_settings,
        # Keep the uncertainty-width fit in-process. On the current Windows
        # development path, PSOCK workers do not inherit the freshly loaded
        # custom learner registry, which causes a silent fallback away from the
        # learned width model and explodes the multiplier intervals.
        workers = 1L,
        progress = progress
      )

      width_training <- prepare_meta_policy_data(
        policy_perf = meta_selected,
        outcome_col = calibration_outcome_col,
        feature_cols = width_feature_cols,
        outcome_clip_quantile = NULL,
        retain_cols = c(width_group_col, width_lmm_random_intercepts)
      )
      if (!".split_group" %in% names(width_training) &&
        width_group_col %in% names(width_training)) {
        width_training$.split_group <- width_training[[width_group_col]]
      }
      width_fit_result <- fit_meta_policy_learner(
        training_data = width_training,
        method = method_now,
        feature_cols = width_feature_cols,
        outcome_transform = crossfit_obj$outcome_transform %||% policy_selector_config_value(cfg, "outcome_transform", sections = c("uncertainty", "policy_learner")),
        alpha = crossfit_obj$alpha,
        lambda_rule = crossfit_obj$lambda_rule %||% policy_selector_config_value(cfg, "lambda_rule", sections = c("uncertainty", "policy_learner")),
        inner_folds = crossfit_obj$inner_folds %||% policy_selector_config_value(cfg, "inner_folds", sections = c("uncertainty", "policy_learner")),
        seed = crossfit_obj$seed %||% policy_selector_config_value(cfg, "seed", sections = c("uncertainty", "policy_learner")),
        super_methods = width_super_methods,
        metalearner_loss = crossfit_obj$metalearner_loss %||% policy_selector_config_value(cfg, "loss", sections = c("uncertainty", "policy_learner")),
        method_settings = uncertainty_method_settings
      )

      width_predictions <- tibble::as_tibble(width_crossfit_result$predictions %||% tibble::tibble())
      if (!calibration_outcome_col %in% names(width_predictions) && ".outcome" %in% names(width_predictions)) {
        width_predictions[[calibration_outcome_col]] <- width_predictions$.outcome
      }
      width_predictions <- width_predictions |>
        dplyr::mutate(
          .width_predicted_q_abs_log = pmax(.data$.meta_predicted_score, 0)
        )

      join_keys <- intersect(
        c("anchor_model_id", "anchor_species", "policy", "equation_branch_filter"),
        intersect(names(meta_selected), names(width_predictions))
      )
      width_selected_rows <- meta_selected |>
        dplyr::left_join(
          width_predictions |>
            dplyr::select(
              dplyr::all_of(join_keys),
              ".width_predicted_q_abs_log"
            ),
          by = join_keys
        )

      list(
        crossfit = width_crossfit_result,
        model = width_fit_result,
        selected = width_selected_rows,
        method = method_now
      )
    }

    width_methods_to_try <- unique(c(
      width_method,
      (object@fitted_model)$selection_method %||% crossfit_obj$selection_method
    ))
    width_methods_to_try <- width_methods_to_try[
      !is.na(width_methods_to_try) & nzchar(width_methods_to_try)
    ]
    report_progress(
      progress,
      sprintf(
        "Fitting conditional uncertainty width model: %d selected rows, %d features.",
        nrow(meta_selected), length(width_feature_cols)
      )
    )
    width_attempt <- NULL
    width_errors <- character(0)
    for (method_now in width_methods_to_try) {
      width_attempt <- tryCatch(
        attempt_width_method(method_now),
        error = function(e) {
          width_errors[[method_now]] <<- conditionMessage(e)
          NULL
        }
      )
      if (!is.null(width_attempt)) {
        break
      }
    }

    if (!is.null(width_attempt)) {
      width_crossfit <- width_attempt$crossfit
      width_model <- list(
        model = width_attempt$model,
        feature_cols = width_feature_cols,
        method = width_attempt$method
      )
      width_selected <- tibble::as_tibble(width_attempt$selected)
      width_prediction_source <- if (identical(width_attempt$method, width_method)) {
        "learned_conditional_width"
      } else {
        "learned_conditional_width_retry"
      }
      if (!identical(width_attempt$method, width_method)) {
        width_warning <- sprintf(
          paste(
            "The conditional-uncertainty learner failed with method '%s' and was retried with '%s'.",
            "Using the learned retry fit instead of point-score fallback.",
            "Original reason: %s"
          ),
          width_method,
          width_attempt$method,
          width_errors[[width_method]] %||% "unknown error"
        )
      }
    } else {
      width_fit_error <- paste(
        sprintf("%s: %s", names(width_errors), unname(width_errors)),
        collapse = " | "
      )
      width_warning <- sprintf(
        paste(
          "Falling back to point-score-scaled uncertainty because the conditional-uncertainty learner",
          "could not be fit with method '%s'. Reason: %s"
        ),
        width_method,
        width_fit_error
      )
      width_selected <- meta_selected |>
        dplyr::mutate(.width_predicted_q_abs_log = pmax(.data$.meta_predicted_score, 0))
      width_model <- list(model = NULL, feature_cols = width_feature_cols, method = width_method)
      width_prediction_source <- "point_score_fallback"
    }
  } else {
    width_fit_error <- "No conditional-uncertainty feature columns were available."
    width_warning <- paste(
      "Falling back to point-score-scaled uncertainty because no",
      "conditional-uncertainty feature columns were available."
    )
    width_selected <- meta_selected |>
      dplyr::mutate(.width_predicted_q_abs_log = pmax(.data$.meta_predicted_score, 0))
    width_model <- list(model = NULL, feature_cols = width_feature_cols, method = width_method)
    width_prediction_source <- "point_score_fallback"
  }

  if (!is.null(width_warning)) {
    warning(width_warning, call. = FALSE)
  }

  if (!".width_predicted_q_abs_log" %in% names(width_selected)) {
    stop(
      "Conditional-uncertainty calibration did not produce '.width_predicted_q_abs_log'.",
      call. = FALSE
    )
  }

  width_selected <- width_selected |>
    dplyr::mutate(
      .width_predicted_q_abs_log = suppressWarnings(as.numeric(.width_predicted_q_abs_log)),
      .width_predicted_q_abs_log = dplyr::if_else(
        is.finite(.width_predicted_q_abs_log) & .width_predicted_q_abs_log > 0,
        .width_predicted_q_abs_log,
        NA_real_
      )
    )

  width_calibration <- width_selected |>
    dplyr::filter(
      is.finite(.data[[calibration_outcome_col]]),
      is.finite(.width_predicted_q_abs_log),
      .width_predicted_q_abs_log > 0
    ) |>
    dplyr::transmute(
      anchor_model_id = as.character(.data$anchor_model_id),
      anchor_species,
      anchor_family = if ("anchor_family" %in% names(width_selected)) as.character(anchor_family) else rep(NA_character_, dplyr::n()),
      policy = if ("policy" %in% names(width_selected)) as.character(policy) else rep(NA_character_, dplyr::n()),
      equation_branch_filter = if ("equation_branch_filter" %in% names(width_selected)) as.character(equation_branch_filter) else rep(NA_character_, dplyr::n()),
      selected_score_abs_log = .data[[calibration_outcome_col]],
      predicted_width_abs_log = .width_predicted_q_abs_log,
      width_ratio = selected_score_abs_log / predicted_width_abs_log
    )

  width_factor_global <- conformal_quantile(
    width_calibration$width_ratio,
    alpha = alpha
  )
  width_factor_simultaneous_global <- width_calibration |>
    dplyr::group_by(anchor_species) |>
    dplyr::summarise(
      width_ratio = max(width_ratio, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::pull(width_ratio) |>
    conformal_quantile(alpha = alpha)

  # Build pooled local conformal factors so thin policy/species cells borrow
  # strength from the global width ratio instead of using one raw local ratio.
  width_local_lookup <- policy_learner_build_local_lookup(
    tbl = width_calibration,
    value_col = "width_ratio",
    alpha = alpha,
    global_default = width_factor_global,
    min_scores = min_bin_scores
  )
  width_selected_local <- policy_learner_apply_local_lookup(
    tbl = width_selected,
    lookup = width_local_lookup,
    value_col = "width_ratio_factor_local",
    source_col = "width_ratio_factor_source",
    n_col = "width_ratio_factor_n"
  ) |>
    dplyr::mutate(
      width_ratio_factor_local = dplyr::coalesce(
        suppressWarnings(as.numeric(.data$width_ratio_factor_local)),
        width_factor_global
      ),
      width_ratio_factor_source = dplyr::coalesce(
        as.character(.data$width_ratio_factor_source),
        "global_scale_conformal"
      ),
      width_ratio_factor_n = dplyr::coalesce(
        suppressWarnings(as.numeric(.data$width_ratio_factor_n)),
        suppressWarnings(as.numeric(nrow(width_calibration)))
      )
    )

  width_coverage <- width_selected_local |>
    dplyr::filter(is.finite(.data[[calibration_outcome_col]]), is.finite(.width_predicted_q_abs_log)) |>
    dplyr::mutate(
      learned_q_abs_log = .width_predicted_q_abs_log * .data$width_ratio_factor_local,
      learned_q_simultaneous_abs_log = .width_predicted_q_abs_log * pmax(
        .data$width_ratio_factor_local,
        width_factor_simultaneous_global,
        na.rm = TRUE
      ),
      marginal_covered = .data[[calibration_outcome_col]] <= learned_q_abs_log,
      simultaneous_covered = .data[[calibration_outcome_col]] <= learned_q_simultaneous_abs_log
    ) |>
    dplyr::summarise(
      n_selected_rows = dplyr::n(),
      n_calibration_species = dplyr::n_distinct(anchor_species),
      conformal_factor = width_factor_global,
      simultaneous_conformal_factor = width_factor_simultaneous_global,
      mean_local_conformal_factor = mean(.data$width_ratio_factor_local, na.rm = TRUE),
      median_local_conformal_factor = stats::median(.data$width_ratio_factor_local, na.rm = TRUE),
      n_local_lookup_sources = dplyr::n_distinct(.data$width_ratio_factor_source),
      empirical_row_coverage = mean(marginal_covered, na.rm = TRUE),
      empirical_row_coverage_under_simultaneous_interval = mean(simultaneous_covered, na.rm = TRUE),
      median_abs_log_error = stats::median(.data[[calibration_outcome_col]], na.rm = TRUE),
      median_interval_log_width = stats::median(2 * learned_q_abs_log, na.rm = TRUE),
      .groups = "drop"
    )
  report_progress(progress, "Completed learner uncertainty calibration.")

  policy_learner_rebuild(
    object,
    config = cfg,
    calibration = list(
      predictions = predictions,
      selected = meta_selected,
      summary = meta_summary,
      anchor_lookup = anchor_lookup,
      support_cutpoints = meta_support_cutpoints,
      q_global = meta_q_global,
      q_simultaneous_global = meta_q_simultaneous_global,
      bin_quantiles = meta_bin_quantiles,
      local_q_lookup = meta_local_q_lookup,
      coverage = meta_calibration_coverage,
      width_predictions = tibble::as_tibble(width_selected_local),
      width_feature_cols = width_feature_cols,
      width_model = width_model,
      width_crossfit = width_crossfit,
      method_settings = uncertainty_method_settings,
      uncertainty_method_settings = uncertainty_method_settings,
      uncertainty_super_methods = width_super_methods,
      width_prediction_source = width_prediction_source,
      width_fit_error = width_fit_error,
      width_warning = width_warning,
      uncertainty_feature_cols = width_feature_cols,
      uncertainty_model = width_model,
      uncertainty_crossfit = width_crossfit,
      uncertainty_prediction_source = width_prediction_source,
      uncertainty_fit_error = width_fit_error,
      uncertainty_warning = width_warning,
      uncertainty_method = width_method,
      local_width_lookup = width_local_lookup,
      width_factor_global = width_factor_global,
      width_factor_simultaneous_global = width_factor_simultaneous_global,
      width_coverage = width_coverage,
      uncertainty_coverage = width_coverage,
      outcome_col = calibration_outcome_col,
      raw_outcome_col = outcome_col,
      max_selection_tolerance = max_selection_tolerance,
      use_support_bin_intervals = policy_selector_config_value(
        cfg,
        "use_support_bin_intervals",
        sections = c("selection", "policy_learner")
      ) %||% FALSE,
      n_bins = as.integer(n_bins),
      support_bin_labels = resolve_post_selection_support_labels(
        labels = support_bin_labels,
        n_bins = as.integer(n_bins)
      )
    )
  )
}

#' Score anchor-policy rows with a PolicyLearner
#'
#' Applies the fitted meta-selection learner and the calibrated uncertainty
#' model to an anchor-policy table.
#'
#' @param object A [PolicyLearner] object with fitted selection and uncertainty
#'   state.
#' @param new_policy_tbl Anchor-policy table to score.
#' @param use_support_bin_intervals Logical scalar controlling whether support
#'   bins are used when available for conformal lookups.
#' @param max_selection_tolerance Optional numeric tie tolerance for retaining
#'   score-minimizing rows within an anchor.
#'
#' @return A scored tibble.
#'
#' Score anchor-policy rows with a PolicyLearner
#'
#' Applies the fitted meta-selection learner and the calibrated uncertainty
#' model to an anchor-policy table.
#'
#' @param object A [PolicyLearner] object with fitted selection and uncertainty
#'   state.
#' @param new_policy_tbl Anchor-policy table to score.
#' @param use_support_bin_intervals Logical scalar controlling whether support
#'   bins are used when available for conformal lookups.
#' @param max_selection_tolerance Optional numeric tie tolerance for retaining
#'   score-minimizing rows within an anchor.
#'
#' @keywords internal
#' @noRd
.predict_policy_learner <- function(object,
                                    new_policy_tbl = NULL,
                                    use_support_bin_intervals = NULL,
                                    max_selection_tolerance = NULL,
                                    ...) {
  dots <- list(...)
  if ("new_policy_tbl" %in% names(dots)) {
    new_policy_tbl <- dots[["new_policy_tbl"]]
  }
  if ("use_support_bin_intervals" %in% names(dots)) {
    use_support_bin_intervals <- dots[["use_support_bin_intervals"]]
  }
  if ("max_selection_tolerance" %in% names(dots)) {
    max_selection_tolerance <- dots[["max_selection_tolerance"]]
  }
  dot_names <- names(dots) %||% rep("", length(dots))
  positional_dots <- dots[!nzchar(dot_names)]
  if (is.null(new_policy_tbl) && length(positional_dots) > 0L) {
    new_policy_tbl <- positional_dots[[1L]]
  }
  if (is.null(new_policy_tbl)) {
    stop("'new_policy_tbl' is required.", call. = FALSE)
  }
  cfg <- policy_learner_config(object)
  if (length(object@fitted_model) == 0 ||
    !inherits((object@fitted_model)$model %||% NULL, "tsb_meta_policy_learner")) {
    stop("No fitted meta-policy learner is stored on this `PolicyLearner`.", call. = FALSE)
  }
  fit_obj <- object@fitted_model
  if (length(object@calibration) == 0) {
    stop("No calibration bundle is stored on this `PolicyLearner`.", call. = FALSE)
  }
  cal_obj <- object@calibration
  support_bin_quantiles <- tibble::as_tibble(cal_obj$bin_quantiles %||% tibble::tibble())
  has_support_bin_calibration <- nrow(support_bin_quantiles) > 0 &&
    "q_abs_log" %in% names(support_bin_quantiles)
  cfg_use_support_bin_intervals <- policy_selector_config_value(
    cfg,
    "use_support_bin_intervals",
    sections = c("selection", "policy_learner")
  )
  # Honor explicit config first, then use stored support-bin calibration when
  # it is available on the learner.
  use_support_bin_intervals <- use_support_bin_intervals %||%
    cal_obj$use_support_bin_intervals %||%
    has_support_bin_calibration %||%
    cfg_use_support_bin_intervals %||%
    FALSE

  scored <- predict_meta_policy_score(fit_obj$model, new_policy_tbl)
  use_direct_support_bin_intervals <- isTRUE(use_support_bin_intervals) &&
    has_support_bin_calibration

  if (!"valid_prediction" %in% names(scored)) {
    if ("n_valid_models" %in% names(scored)) {
      scored$valid_prediction <- is.finite(scored$n_valid_models) & scored$n_valid_models > 0
    } else if ("n_models" %in% names(scored)) {
      scored$valid_prediction <- is.finite(scored$n_models) & scored$n_models > 0
    } else {
      scored$valid_prediction <- is.finite(scored$multiplier_pred) & scored$multiplier_pred > 0
    }
  }

  if (isTRUE(use_support_bin_intervals)) {
    scored <- assign_post_selection_support_bins(
      scored,
      cutpoints = cal_obj$support_cutpoints %||% NULL,
      n_bins = cal_obj$n_bins %||%
        policy_selector_config_value(cfg, "n_bins", sections = c("selection")),
      labels = cal_obj$support_bin_labels %||% NULL
    )
  } else {
    scored$post_selection_support_score <- NA_real_
    scored$post_selection_support_bin <- NA_character_
    scored$post_selection_support_label <- NA_character_
  }
  scored <- policy_learner_prepare_context(
    scored,
    anchor_lookup = cal_obj$anchor_lookup %||% NULL
  )

  if (isTRUE(use_support_bin_intervals)) {
    scored <- scored |>
      dplyr::left_join(
        tibble::as_tibble(cal_obj$bin_quantiles %||% tibble::tibble()) |>
          dplyr::select(
            dplyr::any_of(c(
              "post_selection_support_bin",
              "q_abs_log",
              "simultaneous_species_max_q_abs_log"
            ))
          ) |>
          dplyr::rename(
            meta_q_abs_log = "q_abs_log",
            meta_q_abs_log_simultaneous = "simultaneous_species_max_q_abs_log"
          ),
        by = "post_selection_support_bin"
      )
  } else {
    scored$meta_q_abs_log <- NA_real_
    scored$meta_q_abs_log_simultaneous <- NA_real_
  }

  # Keep the prediction interface table-first rather than requiring that every
  # caller pre-join presentation labels. When no explicit display label is
  # supplied, fall back to the policy identifier itself.
  if (!"policy_display" %in% names(scored)) {
    scored$policy_display <- scored$policy
  }
  if (!"local_weighted_mean_combined_distance" %in% names(scored)) {
    scored$local_weighted_mean_combined_distance <- NA_real_
  }
  if (!"local_min_combined_distance" %in% names(scored)) {
    scored$local_min_combined_distance <- NA_real_
  }
  if (!"q_abs_log_structural" %in% names(scored)) {
    scored$q_abs_log_structural <- 0
  }
  if (!"coefficient_slope_q95" %in% names(scored)) {
    scored$coefficient_slope_q95 <- NA_real_
  }
  if (!"coefficient_intercept_q95" %in% names(scored)) {
    scored$coefficient_intercept_q95 <- NA_real_
  }

  width_model_now <- cal_obj$width_model$model %||% NULL
  configured_width_source <- cal_obj$uncertainty_prediction_source %||%
    cal_obj$width_prediction_source %||%
    NA_character_
  if (identical(configured_width_source, "global_selected_conformal")) {
    scored$.width_predicted_q_abs_log <- rep(1, nrow(scored))
    meta_width_source <- "global_selected_conformal"
  } else if (inherits(width_model_now, "tsb_meta_policy_learner")) {
    width_scored <- predict_meta_policy_score(width_model_now, scored)
    scored$.width_predicted_q_abs_log <- pmax(width_scored$.meta_predicted_score, 0)
    meta_width_source <- configured_width_source %||% "learned_conditional_width"
  } else {
    scored$.width_predicted_q_abs_log <- pmax(scored$.meta_predicted_score, 0)
    meta_width_source <- configured_width_source %||% "point_score_fallback"
  }
  meta_uncertainty_warning_value <- cal_obj$uncertainty_warning %||% cal_obj$width_warning %||% NA_character_
  scored$meta_q_abs_log_local <- NA_real_
  scored$meta_q_abs_log_source <- NA_character_
  scored$meta_q_abs_log_n_scores <- NA_real_
  scored$meta_q_abs_log_conformal_factor <- rep(NA_real_, nrow(scored))
  scored$meta_q_abs_log_factor_source <- rep(NA_character_, nrow(scored))
  scored$meta_q_abs_log_factor_n_scores <- rep(NA_real_, nrow(scored))

  # Reapply the pooled local width lookup at prediction time so the learned
  # width model receives a context-aware conformal multiplier with global
  # fallback when the query lands outside any supported local slice.
  local_width_lookup <- cal_obj$local_width_lookup %||% NULL
  if (is.list(local_width_lookup) && length(local_width_lookup) > 0) {
    scored_local_width <- policy_learner_apply_local_lookup(
      tbl = scored,
      lookup = local_width_lookup,
      value_col = "meta_q_abs_log_conformal_factor",
      source_col = "meta_q_abs_log_factor_source",
      n_col = "meta_q_abs_log_factor_n_scores"
    )
    scored$meta_q_abs_log_conformal_factor <- suppressWarnings(
      as.numeric(scored_local_width$meta_q_abs_log_conformal_factor)
    )
    scored$meta_q_abs_log_factor_source <- as.character(
      scored_local_width$meta_q_abs_log_factor_source
    )
    scored$meta_q_abs_log_factor_n_scores <- suppressWarnings(
      as.numeric(scored_local_width$meta_q_abs_log_factor_n_scores)
    )
  }

  # Finish with the stored global fallback so unsupported contexts still return
  # a valid interval scale.
  scored$meta_q_abs_log_conformal_factor <- dplyr::coalesce(
    scored$meta_q_abs_log_conformal_factor,
    suppressWarnings(as.numeric(cal_obj$width_factor_global %||% 1))
  )
  scored$meta_q_abs_log_factor_source <- dplyr::coalesce(
    scored$meta_q_abs_log_factor_source,
    "global_scale_conformal"
  )
  scored$meta_q_abs_log_factor_n_scores <- dplyr::coalesce(
    scored$meta_q_abs_log_factor_n_scores,
    suppressWarnings(as.numeric(nrow(tibble::as_tibble(cal_obj$selected %||% tibble::tibble()))))
  )

  local_q_lookup <- cal_obj$local_q_lookup %||% NULL
  if (is.list(local_q_lookup) && length(local_q_lookup) > 0) {
    scored_local_q <- policy_learner_apply_local_lookup(
      tbl = scored,
      lookup = local_q_lookup,
      value_col = "meta_q_abs_log_local",
      source_col = "meta_q_abs_log_source",
      n_col = "meta_q_abs_log_n_scores"
    )
    scored$meta_q_abs_log_local <- suppressWarnings(
      as.numeric(scored_local_q$meta_q_abs_log_local)
    )
    scored$meta_q_abs_log_source <- as.character(
      scored_local_q$meta_q_abs_log_source
    )
    scored$meta_q_abs_log_n_scores <- suppressWarnings(
      as.numeric(scored_local_q$meta_q_abs_log_n_scores)
    )
  }
  scored$meta_q_abs_log_local <- dplyr::coalesce(
    scored$meta_q_abs_log_local,
    suppressWarnings(as.numeric(cal_obj$q_global %||% NA_real_))
  )
  scored$meta_q_abs_log_source <- dplyr::coalesce(
    scored$meta_q_abs_log_source,
    "global_selected_conformal"
  )
  scored$meta_q_abs_log_n_scores <- dplyr::coalesce(
    scored$meta_q_abs_log_n_scores,
    suppressWarnings(as.numeric(nrow(tibble::as_tibble(cal_obj$selected %||% tibble::tibble()))))
  )

  # Compute the anchor-level minimum predicted score once, then join it back so
  # groups with no valid predictions stay explicit and do not trigger empty-set
  # minima during the selection step.
  min_score_tbl <- scored |>
    dplyr::group_by(.data$anchor_model_id, .data$anchor_species) |>
    dplyr::summarise(
      .meta_min_predicted_score = {
        score_values <- .data$.meta_predicted_score[.data$valid_prediction & is.finite(.data$.meta_predicted_score)]
        if (length(score_values) == 0) {
          NA_real_
        } else {
          min(score_values)
        }
      },
      .groups = "drop"
    )

  max_selection_tolerance <- as.numeric(
    max_selection_tolerance %||%
      policy_selector_config_value(cfg, "max_selection_tolerance", sections = c("selection", "policy_learner"))
  )
  uncertainty_rule <- normalize_uncertainty_rule(
    policy_selector_config_value(cfg, "uncertainty_rule", sections = c("selection", "policy")) %||% "tolerance"
  )
  one_se_multiplier <- as.numeric(
    policy_selector_config_value(cfg, "one_se_multiplier", sections = c("selection", "policy")) %||% 1
  )
  u_tol_rel <- as.numeric(
    policy_selector_config_value(cfg, "u_tol_rel", sections = c("selection", "policy")) %||%
      policy_selector_config_value(cfg, "uncertainty_relative_tolerance", sections = c("selection", "policy"))
  )
  u_tol_abs <- as.numeric(
    policy_selector_config_value(cfg, "u_tol_abs", sections = c("selection", "policy")) %||%
      policy_selector_config_value(cfg, "uncertainty_absolute_tolerance", sections = c("selection", "policy"))
  )
  local_distance_tolerance <- as.numeric(
    policy_selector_config_value(cfg, "local_distance_tolerance", sections = c("selection", "policy"))
  )
  if (!is.finite(u_tol_abs)) {
    u_tol_abs <- 0
  }
  if (!is.finite(u_tol_rel)) {
    u_tol_rel <- 0
  }
  if (!is.finite(one_se_multiplier)) {
    one_se_multiplier <- 1
  }
  scored <- scored |>
    dplyr::left_join(min_score_tbl, by = c("anchor_model_id", "anchor_species")) |>
    dplyr::group_by(.data$anchor_model_id, .data$anchor_species) |>
    dplyr::mutate(
      meta_policy_rank = dplyr::min_rank(.data$.meta_predicted_score),
      meta_q_abs_log_width = dplyr::coalesce(
        dplyr::if_else(
          isTRUE(use_direct_support_bin_intervals) & is.finite(.data$meta_q_abs_log) & .data$meta_q_abs_log > 0,
          .data$meta_q_abs_log,
          NA_real_
        ),
        dplyr::if_else(
          is.finite(.data$.width_predicted_q_abs_log) & .data$.width_predicted_q_abs_log > 0,
          .data$.width_predicted_q_abs_log,
          pmax(.data$.meta_predicted_score, 0)
        )
      ),
      meta_q_abs_log_conformal_factor = dplyr::coalesce(.data$meta_q_abs_log_conformal_factor, cal_obj$width_factor_global, 1),
      meta_q_abs_log_width_total = .data$meta_q_abs_log_width * .data$meta_q_abs_log_conformal_factor,
      meta_q_abs_log = .data$meta_q_abs_log_width_total,
      meta_interval_calibration = paste0(
        "direct_",
        dplyr::coalesce(.data$meta_q_abs_log_factor_source, meta_width_source, "learned_conditional_width")
      ),
      meta_uncertainty_source = .data$meta_interval_calibration,
      meta_uncertainty_fallback = identical(meta_width_source, "point_score_fallback"),
      meta_uncertainty_warning = rep(as.character(meta_uncertainty_warning_value), dplyr::n()),
      meta_q_abs_log_simultaneous_factor = dplyr::coalesce(
        cal_obj$width_factor_simultaneous_global,
        .data$meta_q_abs_log_conformal_factor
      ),
      # Preserve the earlier diagnostic columns, but make them reflect the new
      # conditional-width decomposition rather than a hand-built score penalty.
      meta_q_abs_log_calibration = .data$meta_q_abs_log_width_total,
      meta_q_abs_log_score = dplyr::if_else(
        is.finite(.data$.meta_predicted_score) & .data$.meta_predicted_score > 0,
        .data$.meta_predicted_score,
        0
      ),
      meta_q_abs_log_simultaneous = dplyr::coalesce(
        .data$meta_q_abs_log_simultaneous,
        .data$meta_q_abs_log_width * .data$meta_q_abs_log_simultaneous_factor
      ),
      meta_q_abs_log_simultaneous_calibration = .data$meta_q_abs_log_simultaneous,
      meta_q_abs_log_total = sqrt(
        .data$meta_q_abs_log_width_total^2 +
          dplyr::coalesce(.data$q_abs_log_structural, 0)^2
      ),
      meta_q_abs_log_simultaneous_total = sqrt(
        pmax(
          .data$meta_q_abs_log_width_total,
          dplyr::coalesce(.data$meta_q_abs_log_simultaneous, .data$meta_q_abs_log_width * .data$meta_q_abs_log_simultaneous_factor),
          na.rm = TRUE
        )^2 +
          dplyr::coalesce(.data$q_abs_log_structural, 0)^2
      ),
      meta_simultaneous_interval_factor = dplyr::if_else(
        is.finite(.data$meta_q_abs_log_simultaneous_total),
        exp(.data$meta_q_abs_log_simultaneous_total),
        NA_real_
      ),
      meta_post_selection_multiplier_lo = dplyr::if_else(
        is.finite(.data$multiplier_pred) & .data$multiplier_pred > 0 & is.finite(.data$meta_q_abs_log_total),
        .data$multiplier_pred * exp(-.data$meta_q_abs_log_total),
        NA_real_
      ),
      meta_post_selection_multiplier_hi = dplyr::if_else(
        is.finite(.data$multiplier_pred) & .data$multiplier_pred > 0 & is.finite(.data$meta_q_abs_log_total),
        .data$multiplier_pred * exp(.data$meta_q_abs_log_total),
        NA_real_
      ),
      meta_post_selection_interval_log_width = dplyr::if_else(
        is.finite(.data$meta_q_abs_log_total),
        2 * .data$meta_q_abs_log_total,
        NA_real_
      ),
      meta_interval_factor = dplyr::if_else(
        is.finite(.data$meta_q_abs_log_total),
        exp(.data$meta_q_abs_log_total),
        NA_real_
      ),
      selected_policy = .data$policy,
      selected_policy_display = dplyr::coalesce(.data$policy_display, .data$policy),
      anchor_selection_local_distance = dplyr::coalesce(
        .data$local_weighted_mean_combined_distance,
        .data$local_min_combined_distance
      ),
      selection_tier = "meta_policy_lexicographic_selection"
    ) |>
    dplyr::ungroup()
  if (!isTRUE(use_direct_support_bin_intervals)) {
    scored <- scored |>
      dplyr::mutate(
        meta_q_abs_log_width_total = pmax(
          .data$meta_q_abs_log_width_total,
          dplyr::coalesce(.data$meta_q_abs_log_local, 0),
          na.rm = TRUE
        ),
        meta_q_abs_log = .data$meta_q_abs_log_width_total,
        meta_q_abs_log_calibration = .data$meta_q_abs_log_width_total,
        meta_q_abs_log_total = sqrt(
          .data$meta_q_abs_log_width_total^2 +
            dplyr::coalesce(.data$q_abs_log_structural, 0)^2
        ),
        meta_q_abs_log_simultaneous_total = sqrt(
          pmax(
            .data$meta_q_abs_log_width_total,
            dplyr::coalesce(
              .data$meta_q_abs_log_simultaneous,
              .data$meta_q_abs_log_width * .data$meta_q_abs_log_simultaneous_factor
            ),
            na.rm = TRUE
          )^2 +
            dplyr::coalesce(.data$q_abs_log_structural, 0)^2
        ),
        meta_simultaneous_interval_factor = dplyr::if_else(
          is.finite(.data$meta_q_abs_log_simultaneous_total),
          exp(.data$meta_q_abs_log_simultaneous_total),
          NA_real_
        ),
        meta_post_selection_multiplier_lo = dplyr::if_else(
          is.finite(.data$multiplier_pred) & .data$multiplier_pred > 0 & is.finite(.data$meta_q_abs_log_total),
          .data$multiplier_pred * exp(-.data$meta_q_abs_log_total),
          NA_real_
        ),
        meta_post_selection_multiplier_hi = dplyr::if_else(
          is.finite(.data$multiplier_pred) & .data$multiplier_pred > 0 & is.finite(.data$meta_q_abs_log_total),
          .data$multiplier_pred * exp(.data$meta_q_abs_log_total),
          NA_real_
        ),
        meta_post_selection_interval_log_width = dplyr::if_else(
          is.finite(.data$meta_q_abs_log_total),
          2 * .data$meta_q_abs_log_total,
          NA_real_
        ),
        meta_interval_factor = dplyr::if_else(
          is.finite(.data$meta_q_abs_log_total),
          exp(.data$meta_q_abs_log_total),
          NA_real_
        )
      )
  }
  scored <- scored |>
    dplyr::mutate(
      .policy_row_id = seq_len(dplyr::n()),
      meta_policy_predicted_score = .data$.meta_predicted_score,
      uncertainty_cost_log_width = .data$meta_post_selection_interval_log_width,
      uncertainty_eligible = dplyr::coalesce(.data$valid_prediction, FALSE) &
        is.finite(.data$meta_post_selection_interval_log_width) &
        .data$meta_post_selection_interval_log_width > 0
    )

  selected_rows <- scored |>
    dplyr::group_split(.data$anchor_model_id, .data$anchor_species, .keep = TRUE) |>
    purrr::map_dfr(function(.x) {
      selection_tbl <- .x
      if (isTRUE(use_direct_support_bin_intervals)) {
        selection_tbl$.meta_predicted_score <- selection_tbl$uncertainty_cost_log_width
      }
      select_anchor_policies(
        policy_tbl = selection_tbl,
        uncertainty_rule = uncertainty_rule,
        u_tol_rel = u_tol_rel,
        u_tol_abs = u_tol_abs,
        one_se_multiplier = one_se_multiplier,
        local_distance_tolerance = local_distance_tolerance
      ) |>
        dplyr::mutate(
          selected_policy_display = dplyr::coalesce(.data$selected_policy_display, .data$policy_display, .data$selected_policy),
          selection_tier = dplyr::coalesce(.data$selection_tier, "meta_policy_lexicographic_selection")
        ) |>
        dplyr::select(
          "anchor_model_id",
          "anchor_species",
          ".policy_row_id",
          "selected_policy",
          "selected_policy_display",
          "selection_tier",
          "anchor_selection_min_validation_error",
          "anchor_selection_validation_threshold",
          "anchor_selection_min_uncertainty_width",
          "anchor_selection_uncertainty_threshold"
        )
    })

  selection_diag <- selected_rows |>
    dplyr::select(
      "anchor_model_id",
      "anchor_species",
      "anchor_selection_min_validation_error",
      "anchor_selection_validation_threshold",
      "anchor_selection_min_uncertainty_width",
      "anchor_selection_uncertainty_threshold"
    ) |>
    dplyr::distinct()

  scored <- scored |>
    dplyr::left_join(
      selected_rows |>
        dplyr::select(
          ".policy_row_id",
          .selected_policy = "selected_policy",
          .selected_policy_display = "selected_policy_display",
          .selected_selection_tier = "selection_tier"
        ) |>
        dplyr::mutate(is_selected = TRUE),
      by = ".policy_row_id"
    ) |>
    dplyr::left_join(
      selection_diag,
      by = c("anchor_model_id", "anchor_species")
    ) |>
    dplyr::mutate(
      is_selected = dplyr::coalesce(.data$is_selected, FALSE),
      selected_policy = dplyr::if_else(.data$is_selected, dplyr::coalesce(.data$selected_policy, .data$policy), NA_character_),
      selected_policy_display = dplyr::if_else(
        .data$is_selected,
        dplyr::coalesce(.data$selected_policy_display, .data$policy_display, .data$policy),
        NA_character_
      ),
      selection_tier = dplyr::coalesce(.data$.selected_selection_tier, .data$selection_tier, "meta_policy_lexicographic_selection")
    ) |>
    dplyr::select(-dplyr::any_of(c(
      ".policy_row_id",
      ".meta_min_predicted_score",
      ".selected_policy",
      ".selected_policy_display",
      ".selected_selection_tier"
    ))) |>
    dplyr::arrange(
      .data$anchor_species,
      .data$.meta_predicted_score,
      .data$policy
    )

  scored
}

#' Predict policy transfer scores
#'
#' @return A scored tibble.
#' @name predict.PolicyLearner
#' @usage NULL
S7::method(predict_generic, PolicyLearner) <- .predict_policy_learner

#' Plot a `PolicyLearner`
#'
#' Uses the package's S7 method on [base::plot()] so the stored cross-fitted
#' learner predictions and post-selection calibration summaries can be drawn
#' directly from the learner object without rebuilding diagnostic tables in user
#' code.
#'
#' @name plot.PolicyLearner
#'
#' @param x A [PolicyLearner] object.
#' @param y Unused.
#' @param type Figure family to draw.
#' @param view Secondary plot selector used for `type = "residuals"` and
#'   `type = "selected_policy_counts"`.
#' @param outcome Outcome variant used for learner calibration diagnostics.
#'   `"modeled"` uses the modeled target stored in `.outcome` when available.
#'   `"raw"` uses `.outcome_raw` when available and otherwise falls back to the
#'   configured outcome column.
#' @param rows Row subset used for learner diagnostics. `"all"` uses all stored
#'   cross-fitted prediction rows. `"selected"` uses the score-minimizing rows
#'   retained during post-selection calibration.
#' @param n_bins Number of quantile bins used for
#'   `type = "calibration_curve"`.
#' @param ... Unused additional arguments.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' learner <- as_policylearner(selector)
#' learner <- crossfit(learner)
#' learner <- fit(learner)
#' learner <- calibrate_uncertainty(learner)
#' plot(learner, type = "predicted_vs_observed")
#' plot(learner, type = "calibration_curve", outcome = "raw")
#' plot(learner, type = "residuals", view = "by_policy", outcome = "raw")
#' }
#' @usage
#' plot(
#'   x,
#'   y = NULL,
#'   type = c(
#'     "predicted_vs_observed",
#'     "calibration_curve",
#'     "residuals",
#'     "score_by_policy",
#'     "support_bin_error",
#'     "selected_policy_counts",
#'     "recommendation_stability"
#'   ),
#'   view = NULL,
#'   outcome = c("modeled", "raw"),
#'   rows = c("all", "selected"),
#'   n_bins = 10L,
#'   ...
#' )
NULL

.plot_policy_learner <- function(x,
                                 y = NULL,
                                 type = c(
                                   "predicted_vs_observed",
                                   "calibration_curve",
                                   "residuals",
                                   "score_by_policy",
                                   "support_bin_error",
                                   "selected_policy_counts",
                                   "recommendation_stability"
                                 ),
                                 view = NULL,
                                 outcome = c("modeled", "raw"),
                                 rows = c("all", "selected"),
                                 n_bins = 10L,
                                 ...) {
  type <- match.arg(type)
  outcome <- match.arg(outcome)
  rows <- match.arg(rows)

  all_predictions <- tibble::as_tibble(
    x@calibration$predictions %||%
      x@crossfit$result$predictions %||%
      tibble::tibble()
  )
  selected_predictions <- tibble::as_tibble(
    x@calibration$selected %||%
      tibble::tibble()
  )
  if (nrow(all_predictions) == 0 && nrow(selected_predictions) == 0) {
    stop(
      "No learner prediction rows are stored on this `PolicyLearner`. Run `crossfit()` and `calibrate_uncertainty()` first.",
      call. = FALSE
    )
  }

  outcome_col <- x@calibration$outcome_col %||% x@crossfit$outcome_col %||% "error_abs_log"
  if (!outcome_col %in% names(all_predictions) && ".outcome" %in% names(all_predictions)) {
    all_predictions[[outcome_col]] <- all_predictions$.outcome
  }
  if (!outcome_col %in% names(selected_predictions) && ".outcome" %in% names(selected_predictions)) {
    selected_predictions[[outcome_col]] <- selected_predictions$.outcome
  }

  plot_tbl <- if (identical(rows, "selected")) selected_predictions else all_predictions
  if (nrow(plot_tbl) == 0) {
    stop(
      sprintf("No '%s' learner rows are stored on this `PolicyLearner`.", rows),
      call. = FALSE
    )
  }

  observed_col <- outcome_col
  observed_label <- "Observed transferability target"
  if (identical(outcome, "modeled")) {
    if (".outcome" %in% names(plot_tbl)) {
      observed_col <- ".outcome"
    }
    observed_label <- "Modeled transferability target"
  } else {
    if (".outcome_raw" %in% names(plot_tbl)) {
      observed_col <- ".outcome_raw"
    } else if (outcome_col %in% names(plot_tbl)) {
      observed_col <- outcome_col
    } else if (".outcome" %in% names(plot_tbl)) {
      observed_col <- ".outcome"
    }
    observed_label <- "Observed raw absolute log error"
  }

  if (identical(type, "predicted_vs_observed")) {
    plot_df <- plot_tbl |>
      dplyr::filter(
        is.finite(.data$.meta_predicted_score),
        is.finite(.data[[observed_col]])
      )
    if (nrow(plot_df) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(
            title = "Meta-policy score calibration",
            subtitle = "Required plotting fields were not available.",
            x = "Cross-fitted predicted transferability score",
            y = observed_label
          ) +
          ggplot2::theme_minimal(base_size = 11)
      )
    }

    clip_subtitle <- NULL
    if (identical(outcome, "modeled") && ".outcome_was_clipped" %in% names(plot_df)) {
      clipped_fraction <- mean(as.logical(plot_df$.outcome_was_clipped), na.rm = TRUE)
      if (is.finite(clipped_fraction) && clipped_fraction > 0) {
        clip_subtitle <- sprintf(
          "Modeled target uses clipped transfer error; %.1f%% of rows were clipped.",
          100 * clipped_fraction
        )
      }
    }

    return(
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = .data$.meta_predicted_score, y = .data[[observed_col]])
      ) +
        ggplot2::geom_point(alpha = 0.35, size = 1.2, colour = "#1f5a5f") +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#9f4f2f") +
        ggplot2::labs(
          x = "Cross-fitted predicted transferability score",
          y = observed_label,
          title = if (identical(rows, "selected")) "Selected-row score calibration" else "Meta-policy score calibration",
          subtitle = clip_subtitle
        ) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }

  if (identical(type, "calibration_curve")) {
    calibration_tbl <- plot_tbl |>
      dplyr::filter(
        is.finite(.data$.meta_predicted_score),
        is.finite(.data[[observed_col]])
      )
    if (nrow(calibration_tbl) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(
            title = "Meta-policy calibration curve",
            subtitle = "Required plotting fields were not available.",
            x = "Median predicted transferability score",
            y = observed_label
          ) +
          ggplot2::theme_minimal(base_size = 11)
      )
    }

    n_bins_now <- suppressWarnings(as.integer(n_bins)[[1]])
    if (!is.finite(n_bins_now)) {
      n_bins_now <- 10L
    }
    n_bins_now <- min(max(3L, n_bins_now), nrow(calibration_tbl))
    calibration_tbl <- calibration_tbl |>
      dplyr::mutate(calibration_bin = dplyr::ntile(.data$.meta_predicted_score, n_bins_now)) |>
      dplyr::group_by(.data$calibration_bin) |>
      dplyr::summarise(
        n = dplyr::n(),
        predicted_mean = mean(.data$.meta_predicted_score, na.rm = TRUE),
        predicted_median = stats::median(.data$.meta_predicted_score, na.rm = TRUE),
        observed_mean = mean(.data[[observed_col]], na.rm = TRUE),
        observed_median = stats::median(.data[[observed_col]], na.rm = TRUE),
        observed_q25 = stats::quantile(.data[[observed_col]], probs = 0.25, na.rm = TRUE, names = FALSE),
        observed_q75 = stats::quantile(.data[[observed_col]], probs = 0.75, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )

    return(
      ggplot2::ggplot(
        calibration_tbl,
        ggplot2::aes(x = .data$predicted_median, y = .data$observed_median)
      ) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#9f4f2f") +
        ggplot2::geom_linerange(
          ggplot2::aes(ymin = .data$observed_q25, ymax = .data$observed_q75),
          linewidth = 0.9,
          colour = "#5d6f89",
          alpha = 0.8
        ) +
        ggplot2::geom_line(colour = "#1f5a5f", linewidth = 0.9) +
        ggplot2::geom_point(colour = "#1f5a5f", size = 2) +
        ggplot2::labs(
          x = "Median predicted transferability score",
          y = observed_label,
          title = if (identical(rows, "selected")) {
            if (identical(outcome, "raw")) {
              "Selected-row calibration against raw transfer error"
            } else {
              "Selected-row calibration by predicted-score decile"
            }
          } else if (identical(outcome, "raw")) {
            "Meta-policy calibration against raw transfer error"
          } else {
            "Meta-policy calibration by predicted-score decile"
          }
        ) +
        ggplot2::theme_minimal(base_size = 12)
    )
  }

  if (identical(type, "residuals")) {
    view <- match.arg(view %||% "by_policy", c("by_policy", "by_branch"))
    residual_tbl <- plot_tbl |>
      dplyr::mutate(policy_display = resolve_policy_display_names(plot_tbl)) |>
      dplyr::filter(
        is.finite(.data$.meta_predicted_score),
        is.finite(.data[[observed_col]])
      ) |>
      dplyr::mutate(
        residual_value = .data[[observed_col]] - .data$.meta_predicted_score,
        abs_residual_value = abs(.data$residual_value)
      )
    if (nrow(residual_tbl) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(
            title = "Meta-policy residual summary",
            subtitle = "Required plotting fields were not available.",
            x = NULL,
            y = "Absolute residual"
          ) +
          ggplot2::theme_minimal(base_size = 11)
      )
    }

    if (identical(view, "by_policy")) {
      residual_tbl <- residual_tbl |>
        dplyr::mutate(
          plot_residual = pmax(.data$abs_residual_value, .Machine$double.xmin),
          policy_display = stats::reorder(.data$policy_display, .data$plot_residual, stats::median, na.rm = TRUE)
        )
      return(
        ggplot2::ggplot(
          residual_tbl,
          ggplot2::aes(x = .data$policy_display, y = .data$plot_residual)
        ) +
          ggplot2::geom_boxplot(outlier.alpha = 0.2, fill = "#d9c9a3", colour = "#3a3226") +
          ggplot2::coord_flip() +
          ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
          ggplot2::annotation_logticks(sides = "b") +
          ggplot2::labs(
            x = NULL,
            y = "Absolute residual"
          ) +
          ggplot2::theme_minimal(base_size = 10)
      )
    }

    if (!"equation_branch_filter" %in% names(residual_tbl)) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(
            title = "Meta-policy residuals by branch",
            subtitle = "Equation-branch labels were not available.",
            x = "Equation branch filter",
            y = "Absolute residual"
          ) +
          ggplot2::theme_minimal(base_size = 11)
      )
    }

    residual_tbl <- residual_tbl |>
      dplyr::mutate(plot_residual = pmax(.data$abs_residual_value, .Machine$double.xmin))
    return(
      ggplot2::ggplot(
        residual_tbl,
        ggplot2::aes(x = .data$equation_branch_filter, y = .data$plot_residual)
      ) +
        ggplot2::geom_boxplot(outlier.alpha = 0.2, fill = "#b7c8a5", colour = "#26351e") +
        ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
        ggplot2::annotation_logticks(sides = "l") +
        ggplot2::labs(
          x = "Equation branch filter",
          y = "Absolute residual"
        ) +
        ggplot2::theme_minimal(base_size = 11)
    )
  }

  if (identical(type, "score_by_policy")) {
    plot_df <- plot_tbl |>
      dplyr::mutate(policy_display = resolve_policy_display_names(plot_tbl)) |>
      dplyr::filter(is.finite(.data$.meta_predicted_score)) |>
      dplyr::mutate(
        policy_display = stats::reorder(.data$policy_display, .data$.meta_predicted_score, stats::median, na.rm = TRUE)
      )
    if (nrow(plot_df) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(
            title = "Predicted transferability score by candidate policy",
            subtitle = "Required plotting fields were not available.",
            x = NULL,
            y = "Cross-fitted predicted transferability score"
          ) +
          ggplot2::theme_minimal(base_size = 11)
      )
    }

    return(
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = .data$policy_display, y = .data$.meta_predicted_score)
      ) +
        ggplot2::geom_boxplot(outlier.alpha = 0.25, fill = "#d9c9a3", colour = "#3a3226") +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = NULL,
          y = "Cross-fitted predicted transferability score",
          title = if (identical(rows, "selected")) {
            "Predicted transferability score by selected policy"
          } else {
            "Predicted transferability score by candidate policy"
          }
        ) +
        ggplot2::theme_minimal(base_size = 10)
    )
  }

  if (identical(type, "support_bin_error")) {
    support_col <- if ("post_selection_support_label" %in% names(plot_tbl)) {
      "post_selection_support_label"
    } else if ("post_selection_support_bin" %in% names(plot_tbl)) {
      "post_selection_support_bin"
    } else {
      NA_character_
    }
    if (!is.character(support_col) || !nzchar(support_col)) {
      stop(
        "No post-selection support-bin labels are stored on this `PolicyLearner`.",
        call. = FALSE
      )
    }

    plot_df <- plot_tbl |>
      dplyr::filter(
        is.finite(.data[[observed_col]]),
        !is.na(.data[[support_col]])
      )
    if (nrow(plot_df) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(
            title = "Observed transfer error by support bin",
            subtitle = "Required plotting fields were not available.",
            x = "Support bin",
            y = observed_label
          ) +
          ggplot2::theme_minimal(base_size = 11)
      )
    }

    return({
      support_levels <- c(
        "Lowest support",
        "Lower support",
        "Middle support",
        "Higher support",
        "Highest support"
      )
      legacy_support_map <- c(
        support_bin_1 = "Lowest support",
        support_bin_2 = "Lower support",
        support_bin_3 = "Middle support",
        support_bin_4 = "Higher support",
        support_bin_5 = "Highest support"
      )
      support_chr <- as.character(plot_df[[support_col]])
      plot_df[[support_col]] <- dplyr::if_else(
        support_chr %in% support_levels,
        support_chr,
        dplyr::coalesce(unname(legacy_support_map[support_chr]), support_chr)
      )
      plot_df[[support_col]] <- factor(
        plot_df[[support_col]],
        levels = c(
          support_levels,
          sort(setdiff(unique(as.character(plot_df[[support_col]])), support_levels))
        )
      )
      plot_df$plot_observed <- pmax(plot_df[[observed_col]], .Machine$double.xmin)
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = .data[[support_col]], y = .data$plot_observed)
      ) +
        ggplot2::geom_boxplot(fill = "#b7c8a5", colour = "#26351e", outlier.alpha = 0.25) +
        ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
        ggplot2::annotation_logticks(sides = "l") +
        ggplot2::labs(
          x = "Support bin",
          y = observed_label
        ) +
        ggplot2::theme_minimal(base_size = 12)
    })
  }

  if (identical(type, "selected_policy_counts")) {
    view <- match.arg(view %||% "by_policy", c("by_policy", "by_anchor"))
    selected_tbl <- selected_predictions
    if (nrow(selected_tbl) == 0) {
      stop(
        "No selected calibration rows are stored on this `PolicyLearner`.",
        call. = FALSE
      )
    }

    selected_tbl$selected_policy_display <- resolve_selected_policy_names(selected_tbl)

    if (identical(view, "by_policy")) {
      count_tbl <- selected_tbl |>
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
            y = "Selected row count",
            title = "Cross-fitted meta-policy selections"
          ) +
          ggplot2::theme_minimal(base_size = 12)
      )
    }

    count_tbl <- selected_tbl |>
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
          y = "Selected row count",
          fill = "Selected policy",
          title = "Cross-fitted selections by anchor species"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
    )
  }

  selected_tbl <- selected_predictions
  if (nrow(selected_tbl) == 0) {
    stop(
      "No selected calibration rows are stored on this `PolicyLearner`.",
      call. = FALSE
    )
  }

  # Summarize the cross-fitted winning policy frequencies by anchor species so
  # the plot can show both the dominant recommendation and its margin over the
  # next-most-frequent competitor.
  selected_tbl$policy_display <- resolve_selected_policy_names(selected_tbl)
  selected_tbl$branch_display <- dplyr::coalesce(
    if ("selected_equation_branch_filter" %in% names(selected_tbl)) as.character(selected_tbl$selected_equation_branch_filter) else rep(NA_character_, nrow(selected_tbl)),
    if ("equation_branch_filter" %in% names(selected_tbl)) as.character(selected_tbl$equation_branch_filter) else rep(NA_character_, nrow(selected_tbl))
  )
  stability_summary <- selected_tbl |>
    dplyr::count(.data$anchor_species, .data$policy_display, .data$branch_display, name = "n_selected") |>
    dplyr::group_by(.data$anchor_species) |>
    dplyr::mutate(
      total_selected = sum(.data$n_selected),
      win_fraction = .data$n_selected / .data$total_selected
    ) |>
    dplyr::arrange(dplyr::desc(.data$win_fraction), .data$policy_display, .by_group = TRUE) |>
    dplyr::summarise(
      top_policy = dplyr::first(.data$policy_display),
      top_branch = dplyr::first(.data$branch_display),
      top_win_fraction = dplyr::first(.data$win_fraction),
      second_policy = dplyr::nth(.data$policy_display, 2, default = NA_character_),
      second_branch = dplyr::nth(.data$branch_display, 2, default = NA_character_),
      second_win_fraction = dplyr::nth(.data$win_fraction, 2, default = NA_real_),
      stability_margin = .data$top_win_fraction - dplyr::coalesce(.data$second_win_fraction, 0),
      .groups = "drop"
    )
  if (nrow(stability_summary) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          title = "Recommendation stability by species",
          subtitle = "Required plotting fields were not available.",
          x = "Cross-fitted win fraction",
          y = NULL
        ) +
        ggplot2::theme_minimal(base_size = 11)
    )
  }

  ggplot2::ggplot(
    stability_summary,
    ggplot2::aes(
      x = .data$top_win_fraction,
      y = stats::reorder(.data$anchor_species, .data$top_win_fraction),
      xmin = pmax(0, .data$top_win_fraction - .data$stability_margin),
      xmax = .data$top_win_fraction
    )
  ) +
    ggplot2::geom_linerange(linewidth = 1, colour = "#9ecae1") +
    ggplot2::geom_point(size = 2.8, colour = "#08519c") +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$sprintf("%s (%.0f%%)", .data$top_policy, 100 * .data$top_win_fraction)),
      nudge_x = 0.02,
      hjust = 0,
      size = 3
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.15)) +
    ggplot2::scale_y_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
    ggplot2::labs(
      x = "Cross-fitted win fraction",
      y = NULL,
      title = "Recommendation stability by species",
      subtitle = "Range shows margin over the next-most-frequent competing policy."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Register the `PolicyLearner` plot method
#'
#' @name plot.PolicyLearner
#' @usage NULL
#'
#' @keywords internal
#' @noRd
S7::method(plot_generic, PolicyLearner) <- .plot_policy_learner

#' Print a `PolicyLearner`
#'
#' @name print.PolicyLearner
#' @usage NULL
#'
#' @param x A [PolicyLearner] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @noRd
S7::method(print_generic, PolicyLearner) <- function(x, ...) {
  fitted_method <- as.character((x@fitted_model)$method %||% "none")[[1]]
  calibration_rows <- nrow(tibble::as_tibble((x@calibration)$selected %||% tibble::tibble()))

  cat("PolicyLearner\n")
  cat("  training_rows: ", nrow(x@training_data), "\n", sep = "")
  cat("  crossfit_ready: ", if (is.data.frame(x@crossfit)) as.character(nrow(x@crossfit)) else if (is.list(x@crossfit)) if (length(x@crossfit) > 0) "yes" else "no" else if (is.null(x@crossfit)) "no" else "yes", "\n", sep = "")
  cat("  fitted_method: ", fitted_method, "\n", sep = "")
  cat("  calibration_rows: ", calibration_rows, "\n", sep = "")
  invisible(x)
}

#' Show a `PolicyLearner`
#'
#' @name show.PolicyLearner
#' @usage NULL
#'
#' @param object A [PolicyLearner] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
#' @noRd
S7::method(show_generic, PolicyLearner) <- function(object) {
  print(object)
  invisible(object)
}
