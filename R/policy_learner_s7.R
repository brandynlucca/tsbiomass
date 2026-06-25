#' Policy Learner S7 Class
#'
#' `PolicyLearner` wraps the meta-policy and super-learner pipeline around a
#' staged [PolicySelector]. It owns the cross-fitted meta-policy benchmark,
#' final learner fit, and post-selection calibration state used to rank
#' anchor-policy predictions by predicted transferability score.
#'
#' The learner is designed to plug back into [predict()] on a
#' [PolicySelector], so the selector remains the single high-level source of
#' policy predictions.
#'
#' @examples
#' \dontrun{
#' selector <- as_policy_selector(candidates)
#' selector <- benchmark(selector)
#' learner <- as_policy_learner(selector)
#' learner <- crossfit(learner)
#' learner <- fit(learner)
#' learner <- calibrate_uncertainty(learner)
#' }
#'
#' @name PolicyLearner-class
#' @aliases PolicyLearner
NULL

#' @rdname PolicyLearner-class
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
            !(inherits(self@selector, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(self@selector, PolicySelector), error = function(e) FALSE)))) {
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
      inherits(selector, "S7_object") &&
      isTRUE(tryCatch(S7::S7_inherits(selector, PolicySelector), error = function(e) FALSE))) {
    selector_config <- tryCatch(
      policy_selector_config_data(selector@config),
      error = function(e) list()
    )
    config <- merge_cfg(selector_config, config)
    if (is.null(crossfit$species_block_perf) || nrow(crossfit$species_block_perf %||% tibble::tibble()) == 0) {
      crossfit$species_block_perf <- tryCatch(
        tibble::as_tibble(selector@benchmark$species_block_perf %||% tibble::tibble()),
        error = function(e) tibble::tibble()
      )
    }
    selector <- NULL
  }
  PolicyLearner(
    selector      = selector,
    config        = config,
    training_data = tibble::as_tibble(training_data),
    crossfit      = crossfit,
    fitted_model  = fitted_model,
    calibration   = calibration
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
#' learner <- as_policy_learner(selector)
#' learner
#' }
#'
#' @export
create_policy_learner <- function(selector,
                                  config = NULL) {
  if ((inherits(selector, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(selector, PolicyLearner), error = function(e) FALSE)))) {
    return(selector)
  }
  if (!(inherits(selector, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(selector, PolicySelector), error = function(e) FALSE)))) {
    stop("'selector' must be a `PolicySelector` object.", call. = FALSE)
  }

  # Extract only what is needed from the selector rather than storing the full
  # object. The selector can be 200+ MB; the learner only needs the config and
  # the species-block performance table (for meta-policy training).
  selector_config <- tryCatch(
    policy_selector_config_data(selector@config),
    error = function(e) list()
  )
  merged_config <- merge_cfg(selector_config, policy_selector_config_data(config))
  species_block_perf <- tryCatch(
    tibble::as_tibble(selector@benchmark$species_block_perf %||% tibble::tibble()),
    error = function(e) tibble::tibble()
  )
  if (nrow(species_block_perf) > 0) {
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
      sections = c("selection", "post_selection_conformal", "uncertainty", "policy")
    ) %||% 0.10
    species_block_perf <- augment_benchmark_multiplier_interval_scores(
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
    selector      = NULL,
    config        = merged_config,
    training_data = tibble::tibble(),
    crossfit      = list(species_block_perf = species_block_perf),
    fitted_model  = list(),
    calibration   = list()
  )
}

#' Coerce a selector to `PolicyLearner`
#'
#' @param selector A [PolicySelector] object.
#' @param config Optional learner config list or [Configurer] object.
#'
#' @return A `PolicyLearner` object.
#'
#' @export
as_policy_learner <- function(selector,
                              config = NULL) {
  create_policy_learner(
    selector = selector,
    config = config
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
policy_learner_config <- function(object,
                                  config = NULL) {
  # selector@config is merged into object@config at construction time, so we
  # no longer need to access the selector here.
  cfg <- merge_cfg(
    default_config(use_canonical_names = TRUE),
    merge_cfg(object@config, policy_selector_config_data(config))
  )
  cfg$metalearner <- normalize_metalearner_section(cfg$metalearner %||% list())
  cfg
}

#' Require stored benchmark rows for a `PolicyLearner`
#'
#' @param object A [PolicyLearner] object.
#'
#' @return Species-block performance table.
#'
#' @keywords internal
policy_learner_species_perf <- function(object) {
  # species_block_perf is extracted from the selector at construction time and
  # stored in the crossfit list so the full selector need not be retained.
  perf_tbl <- tibble::as_tibble(
    object@crossfit$species_block_perf %||% tibble::tibble()
  )
  if (nrow(perf_tbl) == 0) {
    stop(
      "No species-block benchmark rows are stored on this `PolicyLearner`. ",
      "Re-create the learner from the `PolicySelector` using `as_policy_learner()`.",
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
#'   spread in [add_policy_intervals()].
#' @param alpha Miscoverage level used for the interval-score penalty.
#'
#' @return A tibble with multiplier interval diagnostics added.
#'
#' @keywords internal
augment_benchmark_multiplier_interval_scores <- function(perf_tbl,
                                                         conf_tbl,
                                                         structural_uncertainty_weight = 1,
                                                         alpha = 0.10) {
  perf_tbl <- standardize_policies(tibble::as_tibble(perf_tbl))
  conf_tbl <- standardize_policies(tibble::as_tibble(conf_tbl))
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
policy_learner_selection_outcome_col <- function(policy_perf,
                                                 cfg,
                                                 outcome_col = NULL) {
  explicit_outcome_col <- outcome_col %||%
    policy_selector_config_value(
      cfg,
      "selection_outcome_col",
      sections = c("metalearner", "policy_learner")
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
policy_learner_uncertainty_outcome_col <- function(predictions,
                                                   cfg,
                                                   crossfit_obj,
                                                   outcome_col = NULL) {
  predictions <- tibble::as_tibble(predictions)
  outcome_col <- outcome_col %||%
    policy_selector_config_value(cfg, "uncertainty_outcome_col", sections = c("metalearner", "policy_learner")) %||%
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
policy_learner_feature_cols <- function(cfg,
                                        policy_perf) {
  feature_cols <- policy_selector_config_value(
    cfg, "feature_cols",
    sections = c("metalearner", "policy_learner")
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
policy_learner_uncertainty_feature_cols <- function(cfg,
                                                    policy_perf) {
  feature_cols <- policy_selector_config_value(
    cfg, "uncertainty_feature_cols",
    sections = c("metalearner", "policy_learner")
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
#' @name crossfit.PolicyLearner
#'
#' @param object A [PolicyLearner] object.
#' @param policy_perf Optional species-block performance table override.
#' @param group_col Optional grouping column used for fold blocking.
#' @param n_folds Optional number of outer cross-validation folds.
#' @param selection_method Optional meta-policy learner method.
#' @param seed Optional integer seed.
#' @param feature_cols Optional feature-column override.
#' @param outcome_col Optional outcome-column override.
#' @param outcome_transform Optional outcome transform.
#' @param lambda_rule Optional glmnet lambda-selection rule.
#' @param alpha Optional elastic-net alpha.
#' @param inner_folds Optional number of inner tuning folds.
#' @param selection_super_methods Optional super-learner base methods.
#' @param metalearner_loss Optional super-learner loss name.
#' @param method_settings Optional method-settings override.
#' @param workers Optional worker count.
#' @param package_dir Optional package source directory for worker bootstrap.
#' @param progress Optional logical scalar controlling progress messages.
#' @param config Optional config override.
#'
#' @return An updated [PolicyLearner] object with stored cross-fit results.
#'
#' Cross-fit a `PolicyLearner` against the selector benchmark rows and store
#' the out-of-fold prediction bundle plus the derived training frame.
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
                                                method_settings = NULL,
                                                workers = NULL,
                                                package_dir = NULL,
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
    sections = c("metalearner", "policy_learner")
  )
  group_col <- group_col %||%
    policy_selector_config_value(cfg, "group_col", sections = c("metalearner", "policy_learner"))
  selection_method <- selection_method %||%
    policy_selector_config_value(cfg, "selection_method", sections = c("metalearner", "policy_learner"))
  outcome_transform <- outcome_transform %||%
    policy_selector_config_value(cfg, "outcome_transform", sections = c("metalearner", "policy_learner"))
  lambda_rule <- lambda_rule %||%
    policy_selector_config_value(cfg, "lambda_rule", sections = c("metalearner", "policy_learner"))
  alpha <- alpha %||%
    policy_selector_config_value(cfg, "alpha", sections = c("metalearner", "policy_learner"))
  n_folds <- n_folds %||%
    policy_selector_config_value(cfg, "n_folds", sections = c("metalearner", "policy_learner"))
  seed <- seed %||%
    policy_selector_config_value(cfg, "seed", sections = c("metalearner", "policy_learner"))
  inner_folds <- inner_folds %||%
    policy_selector_config_value(cfg, "inner_folds", sections = c("metalearner", "policy_learner"))
  selection_super_methods <- selection_super_methods %||%
    policy_selector_config_value(cfg, "selection_super_methods", sections = c("metalearner", "policy_learner"))
  metalearner_loss <- metalearner_loss %||%
    policy_selector_config_value(cfg, "metalearner_loss", sections = c("metalearner", "policy_learner"))
  method_settings <- method_settings %||% normalize_meta_policy_method_settings(
    policy_selector_config_value(
      cfg, "method_settings",
      sections = c("metalearner", "policy_learner")
    )
  )
  workers <- workers %||%
    policy_selector_config_value(cfg, "workers", sections = c("metalearner", "policy_learner"))
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = c("metalearner", "policy_learner"))
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
    outcome_clip_quantile = outcome_clip_quantile
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
    method_settings = method_settings,
    workers = workers,
    package_dir = package_dir,
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
      method_settings = method_settings,
      n_folds = as.integer(n_folds),
      seed = as.integer(seed)
    ),
    fitted_model = list(),
    calibration = list()
  )
}

#' Fit a `PolicyLearner`
#'
#' @name fit.PolicyLearner
#'
#' @param object A [PolicyLearner] object.
#' @param training_data Optional prepared learner training table.
#' @param selection_method Optional meta-policy learner method.
#' @param feature_cols Optional feature-column override.
#' @param outcome_transform Optional outcome transform.
#' @param alpha Optional elastic-net alpha.
#' @param lambda_rule Optional glmnet lambda-selection rule.
#' @param inner_folds Optional number of inner tuning folds.
#' @param seed Optional integer seed.
#' @param selection_super_methods Optional super-learner base methods.
#' @param metalearner_loss Optional super-learner loss name.
#' @param method_settings Optional method-settings override.
#' @param progress Optional logical scalar controlling progress messages.
#' @param config Optional config override.
#'
#' @return An updated [PolicyLearner] object with a fitted final learner.
#'
#' Fit the final `PolicyLearner` on the selector's full benchmark-derived
#' training table and store the fitted learner on the object.
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
    policy_selector_config_value(cfg, "outcome_clip_quantile", sections = c("metalearner", "policy_learner"))
  group_col <- crossfit_obj$group_col %||%
    policy_selector_config_value(cfg, "group_col", sections = c("metalearner", "policy_learner"))
  selection_method <- selection_method %||% crossfit_obj$selection_method %||%
    policy_selector_config_value(cfg, "selection_method", sections = c("metalearner", "policy_learner"))
  outcome_transform <- outcome_transform %||% crossfit_obj$outcome_transform %||%
    policy_selector_config_value(cfg, "outcome_transform", sections = c("metalearner", "policy_learner"))
  lambda_rule <- lambda_rule %||% crossfit_obj$lambda_rule %||%
    policy_selector_config_value(cfg, "lambda_rule", sections = c("metalearner", "policy_learner"))
  alpha <- alpha %||% crossfit_obj$alpha %||%
    policy_selector_config_value(cfg, "alpha", sections = c("metalearner", "policy_learner"))
  inner_folds <- inner_folds %||% crossfit_obj$inner_folds %||%
    policy_selector_config_value(cfg, "inner_folds", sections = c("metalearner", "policy_learner"))
  seed <- seed %||% crossfit_obj$seed %||%
    policy_selector_config_value(cfg, "seed", sections = c("metalearner", "policy_learner"))
  selection_super_methods <- selection_super_methods %||% crossfit_obj$selection_super_methods %||%
    policy_selector_config_value(cfg, "selection_super_methods", sections = c("metalearner", "policy_learner"))
  metalearner_loss <- metalearner_loss %||% crossfit_obj$metalearner_loss %||%
    policy_selector_config_value(cfg, "metalearner_loss", sections = c("metalearner", "policy_learner"))
  method_settings <- method_settings %||% crossfit_obj$method_settings %||%
    normalize_meta_policy_method_settings(
      policy_selector_config_value(
        cfg, "method_settings",
        sections = c("metalearner", "policy_learner")
      )
    )
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = c("metalearner", "policy_learner"))
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
      outcome_clip_quantile = outcome_clip_quantile
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
    method_settings = method_settings,
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
      method_settings = method_settings
    ),
    calibration = list()
  )
}

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
      !is.na(anchor_model_id) | !is.na(anchor_species) | !is.na(anchor_family)
    ) |>
    dplyr::distinct()
}

policy_learner_prepare_context <- function(tbl,
                                           anchor_lookup = NULL) {
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
        anchor_model_id = as.character(anchor_model_id),
        anchor_species = as.character(anchor_species),
        anchor_family = as.character(anchor_family)
      )
    if ("anchor_model_id" %in% names(out)) {
      out <- out |>
        dplyr::left_join(
          anchor_lookup |>
            dplyr::select(anchor_model_id, lookup_anchor_species = anchor_species, lookup_anchor_family = anchor_family) |>
            dplyr::distinct(),
          by = "anchor_model_id"
        ) |>
        dplyr::mutate(
          anchor_species = dplyr::coalesce(anchor_species, lookup_anchor_species),
          anchor_family = dplyr::coalesce(anchor_family, lookup_anchor_family)
        ) |>
        dplyr::select(-dplyr::any_of(c("lookup_anchor_species", "lookup_anchor_family")))
    }
    if ("anchor_species" %in% names(out)) {
      out <- out |>
        dplyr::left_join(
          anchor_lookup |>
            dplyr::select(anchor_species, lookup_anchor_family_by_species = anchor_family) |>
            dplyr::filter(!is.na(anchor_species), !is.na(lookup_anchor_family_by_species)) |>
            dplyr::distinct(),
          by = "anchor_species"
        ) |>
        dplyr::mutate(
          anchor_family = dplyr::coalesce(anchor_family, lookup_anchor_family_by_species)
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
    out$equation_branch_filter <- standardize_branches(out)
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

policy_learner_select_calibration_rows <- function(tbl,
                                                   max_selection_tolerance = NULL,
                                                   one_se_multiplier = 1) {
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
    dplyr::filter(selection_valid, is.finite(.meta_predicted_score)) |>
    dplyr::group_by(anchor_model_id, anchor_species) |>
    dplyr::mutate(
      .meta_score_min = min(.meta_predicted_score, na.rm = TRUE),
      .meta_benchmark_score_slack = if (has_benchmark_slack) {
        suppressWarnings(
          min(one_se_threshold - best_mean_species_median_abs_log, na.rm = TRUE)
        )
      } else {
        0
      },
      .meta_benchmark_score_slack = dplyr::if_else(
        is.finite(.meta_benchmark_score_slack),
        .meta_benchmark_score_slack,
        0
      ),
      .meta_score_threshold = if (is.finite(max_selection_tolerance)) {
        .meta_score_min + pmax(0, max_selection_tolerance)
      } else {
        # Match the anchor-level selector by reusing the leave-one-species-out
        # benchmark one-standard-error slack on the meta-score scale.
        .meta_score_min + pmax(0, one_se_multiplier * .meta_benchmark_score_slack)
      }
    ) |>
    dplyr::filter(
      .meta_predicted_score <= .meta_score_threshold
    ) |>
    dplyr::arrange(
      .meta_predicted_score,
      policy,
      equation_branch_filter,
      .by_group = TRUE
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}

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

policy_learner_summarize_lookup_table <- function(tbl,
                                                  group_cols,
                                                  value_col,
                                                  alpha,
                                                  source_label) {
  tbl <- tibble::as_tibble(tbl)
  if (nrow(tbl) == 0 || !value_col %in% names(tbl) || !all(group_cols %in% names(tbl))) {
    return(tibble::tibble())
  }

  value_vec <- suppressWarnings(as.numeric(tbl[[value_col]]))
  tbl[[value_col]] <- value_vec
  tbl <- tbl |>
    dplyr::filter(is.finite(.data[[value_col]]))
  if (nrow(tbl) == 0) {
    return(tibble::tibble())
  }

  tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_scores = dplyr::n(),
      .cal_value = post_selection_quantile(.data[[value_col]], alpha = alpha),
      .groups = "drop"
    ) |>
    dplyr::mutate(.cal_source = source_label)
}

policy_learner_build_local_lookup <- function(tbl,
                                              value_col,
                                              alpha,
                                              global_default = NA_real_) {
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
    post_selection_quantile(valid[[value_col]], alpha = alpha)
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
      policy_learner_summarize_lookup_table(
        tbl = valid,
        group_cols = group_cols,
        value_col = value_col,
        alpha = alpha,
        source_label = source_label
      )
    }
  )

  list(
    global_value = global_value,
    tables = lookup_tables,
    anchor_lookup = policy_learner_anchor_lookup(valid)
  )
}

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
  out[[source_col]] <- rep("global", nrow(out))
  out[[n_col]] <- rep(NA_real_, nrow(out))

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

#' Calibrate `PolicyLearner` uncertainty
#'
#' @name calibrate_uncertainty.PolicyLearner
#'
#' @param object A [PolicyLearner] object.
#' @param predictions Optional cross-fit prediction table override.
#' @param outcome_col Optional outcome-column override.
#' @param max_selection_tolerance Optional score-tie tolerance used when
#'   retaining calibration rows.
#' @param alpha Optional marginal conformal alpha.
#' @param bin_alpha Optional support-bin conformal alpha.
#' @param min_bin_scores Optional minimum score count per support bin.
#' @param n_bins Optional number of support bins.
#' @param progress Optional logical scalar controlling progress messages.
#' @param config Optional config override.
#'
#' @return An updated [PolicyLearner] object with stored calibration outputs.
#'
#' Build the post-selection uncertainty calibration stored on a
#' `PolicyLearner`.
S7::method(calibrate_uncertainty, PolicyLearner) <- function(object,
                                                             predictions = NULL,
                                                             outcome_col = NULL,
                                                             max_selection_tolerance = NULL,
                                                             alpha = NULL,
                                                             bin_alpha = NULL,
                                                             min_bin_scores = NULL,
                                                             n_bins = NULL,
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
    policy_selector_config_value(cfg, "progress", sections = c("metalearner", "selection", "post_selection_conformal", "policy_learner"))
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
  calibration_outcome_col <- if (".outcome" %in% names(predictions)) {
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
      policy_selector_config_value(cfg, "max_selection_tolerance", sections = c("metalearner", "policy_learner"))
  )
  one_se_multiplier <- as.numeric(
    policy_selector_config_value(cfg, "one_se_multiplier", sections = c("selection", "policy")) %||% 1
  )
  if (!is.finite(one_se_multiplier)) {
    one_se_multiplier <- 1
  }
  n_bins <- as.integer(
    n_bins %||%
      policy_selector_config_value(cfg, "n_bins", sections = c("selection", "post_selection_conformal", "metalearner"))
  )
  support_bin_labels <- policy_selector_config_value(
    cfg,
    "support_bin_labels",
    sections = c("selection", "post_selection_conformal", "metalearner")
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
      median_predicted_score = stats::median(.meta_predicted_score, na.rm = TRUE),
      mean_predicted_score = mean(.meta_predicted_score, na.rm = TRUE),
      .groups = "drop"
    )

  alpha <- alpha %||%
    policy_selector_config_value(cfg, "conformal_alpha", sections = c("selection", "post_selection_conformal", "policy", "metalearner")) %||%
    policy_selector_config_value(cfg, "alpha", sections = c("selection", "post_selection_conformal", "metalearner"))

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
  meta_q_global <- post_selection_quantile(
    meta_calibration$selected_score_abs_log,
    alpha = alpha
  )
  meta_q_simultaneous_global <- post_selection_quantile(
    meta_simultaneous_calibration$selected_score_abs_log,
    alpha = alpha
  )
  # The multiplier-interval calibration now uses one global conformalized scale
  # model rather than a nested lookup chain. Keep the selected-row table in
  # place for downstream summaries, but do not build policy/family/branch
  # lookup buckets for anchor-time fallback.
  meta_local_q_lookup <- NULL
  meta_selected_local_q <- meta_selected |>
    dplyr::mutate(
      selected_q_abs_log_local = NA_real_,
      selected_q_abs_log_source = NA_character_,
      selected_q_abs_log_n = NA_real_
    )

  bin_alpha <- bin_alpha %||%
    policy_selector_config_value(cfg, "bin_alpha", sections = c("selection", "post_selection_conformal", "metalearner")) %||%
    policy_selector_config_value(cfg, "conformal_alpha", sections = c("selection", "post_selection_conformal", "policy", "metalearner"))
  min_bin_scores <- as.integer(
    min_bin_scores %||%
      policy_selector_config_value(cfg, "min_bin_scores", sections = c("selection", "post_selection_conformal", "metalearner"))
  )

  meta_bin_quantiles <- meta_calibration |>
    dplyr::group_by(post_selection_support_bin) |>
    dplyr::summarise(
      n_scores = dplyr::n(),
      q_abs_log_raw = post_selection_quantile(
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
  width_method <- policy_selector_config_value(
    cfg, "uncertainty_method",
    sections = c("metalearner", "policy_learner")
  ) %||% (object@fitted_model)$selection_method %||% crossfit_obj$selection_method
  method_settings <- (object@fitted_model)$method_settings %||%
    crossfit_obj$method_settings %||%
    normalize_meta_policy_method_settings(
      policy_selector_config_value(
        cfg, "method_settings",
        sections = c("metalearner", "policy_learner")
      )
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

  if (length(width_feature_cols) > 0) {
    attempt_width_method <- function(method_now) {
      width_crossfit_result <- crossfit_meta_policy_learner(
        policy_perf = meta_selected,
        group_col = width_group_col,
        n_folds = crossfit_obj$n_folds %||% policy_selector_config_value(cfg, "n_folds", sections = c("metalearner", "policy_learner")),
        method = method_now,
        seed = crossfit_obj$seed %||% policy_selector_config_value(cfg, "seed", sections = c("metalearner", "policy_learner")),
        feature_cols = width_feature_cols,
        outcome_col = calibration_outcome_col,
        outcome_clip_quantile = crossfit_obj$outcome_clip_quantile %||% NULL,
        outcome_transform = crossfit_obj$outcome_transform %||% policy_selector_config_value(cfg, "outcome_transform", sections = c("metalearner", "policy_learner")),
        lambda_rule = crossfit_obj$lambda_rule %||% policy_selector_config_value(cfg, "lambda_rule", sections = c("metalearner", "policy_learner")),
        alpha = crossfit_obj$alpha,
        inner_folds = crossfit_obj$inner_folds %||% policy_selector_config_value(cfg, "inner_folds", sections = c("metalearner", "policy_learner")),
        super_methods = crossfit_obj$selection_super_methods,
        metalearner_loss = crossfit_obj$metalearner_loss %||% policy_selector_config_value(cfg, "metalearner_loss", sections = c("metalearner", "policy_learner")),
        method_settings = method_settings,
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
        outcome_clip_quantile = crossfit_obj$outcome_clip_quantile %||% NULL
      )
      width_fit_result <- fit_meta_policy_learner(
        training_data = width_training,
        method = method_now,
        feature_cols = width_feature_cols,
        outcome_transform = crossfit_obj$outcome_transform %||% policy_selector_config_value(cfg, "outcome_transform", sections = c("metalearner", "policy_learner")),
        alpha = crossfit_obj$alpha,
        lambda_rule = crossfit_obj$lambda_rule %||% policy_selector_config_value(cfg, "lambda_rule", sections = c("metalearner", "policy_learner")),
        inner_folds = crossfit_obj$inner_folds %||% policy_selector_config_value(cfg, "inner_folds", sections = c("metalearner", "policy_learner")),
        seed = crossfit_obj$seed %||% policy_selector_config_value(cfg, "seed", sections = c("metalearner", "policy_learner")),
        super_methods = crossfit_obj$selection_super_methods,
        metalearner_loss = crossfit_obj$metalearner_loss %||% policy_selector_config_value(cfg, "metalearner_loss", sections = c("metalearner", "policy_learner")),
        method_settings = method_settings
      )

      width_predictions <- tibble::as_tibble(width_crossfit_result$predictions %||% tibble::tibble())
      if (!calibration_outcome_col %in% names(width_predictions) && ".outcome" %in% names(width_predictions)) {
        width_predictions[[calibration_outcome_col]] <- width_predictions$.outcome
      }
      width_predictions <- width_predictions |>
        dplyr::mutate(
          .width_predicted_q_abs_log = pmax(.meta_predicted_score, 0)
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
              .width_predicted_q_abs_log
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
        dplyr::mutate(.width_predicted_q_abs_log = pmax(.meta_predicted_score, 0))
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
      dplyr::mutate(.width_predicted_q_abs_log = pmax(.meta_predicted_score, 0))
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
      anchor_model_id = as.character(anchor_model_id),
      anchor_species,
      anchor_family = if ("anchor_family" %in% names(width_selected)) as.character(anchor_family) else rep(NA_character_, dplyr::n()),
      policy = if ("policy" %in% names(width_selected)) as.character(policy) else rep(NA_character_, dplyr::n()),
      equation_branch_filter = if ("equation_branch_filter" %in% names(width_selected)) as.character(equation_branch_filter) else rep(NA_character_, dplyr::n()),
      selected_score_abs_log = .data[[calibration_outcome_col]],
      predicted_width_abs_log = .width_predicted_q_abs_log,
      width_ratio = selected_score_abs_log / predicted_width_abs_log
    )

  width_factor_global <- post_selection_quantile(
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
  post_selection_quantile(alpha = alpha)
  width_local_lookup <- NULL
  width_selected_local <- width_selected |>
    dplyr::mutate(
      width_ratio_factor_local = width_factor_global,
      width_ratio_factor_source = "global_scale_conformal",
      width_ratio_factor_n = nrow(width_calibration)
    )

  width_coverage <- width_selected_local |>
    dplyr::filter(is.finite(.data[[calibration_outcome_col]]), is.finite(.width_predicted_q_abs_log)) |>
    dplyr::mutate(
      learned_q_abs_log = .width_predicted_q_abs_log * width_factor_global,
      learned_q_simultaneous_abs_log = .width_predicted_q_abs_log * pmax(
        width_factor_global,
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
      method_settings = method_settings,
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
        sections = c("selection", "post_selection_conformal", "metalearner", "policy_learner")
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
.predict_policy_learner <- function(object,
                                    new_policy_tbl,
                                    use_support_bin_intervals = NULL,
                                    max_selection_tolerance = NULL) {
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
  use_support_bin_intervals <- use_support_bin_intervals %||%
    cal_obj$use_support_bin_intervals %||%
    policy_selector_config_value(
      cfg,
      "use_support_bin_intervals",
      sections = c("selection", "post_selection_conformal", "metalearner", "policy_learner")
    ) %||%
    FALSE

  scored <- predict_meta_policy_score(fit_obj$model, new_policy_tbl)

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
        policy_selector_config_value(cfg, "n_bins", sections = c("selection", "post_selection_conformal", "metalearner")),
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
            meta_q_abs_log = q_abs_log,
            meta_q_abs_log_simultaneous = simultaneous_species_max_q_abs_log
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
  if (inherits(width_model_now, "tsb_meta_policy_learner")) {
    width_scored <- predict_meta_policy_score(width_model_now, scored)
    scored$.width_predicted_q_abs_log <- pmax(width_scored$.meta_predicted_score, 0)
    meta_width_source <- cal_obj$uncertainty_prediction_source %||% cal_obj$width_prediction_source %||% "learned_conditional_width"
  } else {
    scored$.width_predicted_q_abs_log <- pmax(scored$.meta_predicted_score, 0)
    meta_width_source <- cal_obj$uncertainty_prediction_source %||%
      cal_obj$width_prediction_source %||%
      "point_score_fallback"
  }
  meta_uncertainty_warning_value <- cal_obj$uncertainty_warning %||% cal_obj$width_warning %||% NA_character_
  scored$meta_q_abs_log_local <- NA_real_
  scored$meta_q_abs_log_source <- NA_character_
  scored$meta_q_abs_log_n_scores <- NA_real_
  scored$meta_q_abs_log_conformal_factor <- rep(
    suppressWarnings(as.numeric(cal_obj$width_factor_global %||% 1)),
    nrow(scored)
  )
  scored$meta_q_abs_log_factor_source <- rep("global_scale_conformal", nrow(scored))
  scored$meta_q_abs_log_factor_n_scores <- rep(
    suppressWarnings(as.numeric(nrow(tibble::as_tibble(cal_obj$selected %||% tibble::tibble())))),
    nrow(scored)
  )

  # Compute the anchor-level minimum predicted score once, then join it back so
  # groups with no valid predictions stay explicit and do not trigger empty-set
  # minima during the selection step.
  min_score_tbl <- scored |>
    dplyr::group_by(anchor_model_id, anchor_species) |>
    dplyr::summarise(
      .meta_min_predicted_score = {
        score_values <- .meta_predicted_score[valid_prediction & is.finite(.meta_predicted_score)]
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
      policy_selector_config_value(cfg, "max_selection_tolerance", sections = c("metalearner", "policy_learner"))
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
    dplyr::group_by(anchor_model_id, anchor_species) |>
    dplyr::mutate(
      meta_policy_rank = dplyr::min_rank(.meta_predicted_score),
      meta_q_abs_log_width = dplyr::if_else(
        is.finite(.width_predicted_q_abs_log) & .width_predicted_q_abs_log > 0,
        .width_predicted_q_abs_log,
        pmax(.meta_predicted_score, 0)
      ),
      meta_q_abs_log_conformal_factor = dplyr::coalesce(meta_q_abs_log_conformal_factor, cal_obj$width_factor_global, 1),
      meta_q_abs_log_width_total = meta_q_abs_log_width * meta_q_abs_log_conformal_factor,
      meta_q_abs_log = meta_q_abs_log_width_total,
      meta_interval_calibration = paste0(
        "direct_",
        dplyr::coalesce(meta_q_abs_log_factor_source, meta_width_source, "learned_conditional_width")
      ),
      meta_uncertainty_source = meta_interval_calibration,
      meta_uncertainty_fallback = identical(meta_width_source, "point_score_fallback"),
      meta_uncertainty_warning = rep(as.character(meta_uncertainty_warning_value), dplyr::n()),
      meta_q_abs_log_simultaneous_factor = dplyr::coalesce(
        cal_obj$width_factor_simultaneous_global,
        meta_q_abs_log_conformal_factor
      ),
      # Preserve the earlier diagnostic columns, but make them reflect the new
      # conditional-width decomposition rather than a hand-built score penalty.
      meta_q_abs_log_calibration = meta_q_abs_log_width_total,
      meta_q_abs_log_score = dplyr::if_else(
        is.finite(.meta_predicted_score) & .meta_predicted_score > 0,
        .meta_predicted_score,
        0
      ),
      meta_q_abs_log_simultaneous = dplyr::coalesce(
        meta_q_abs_log_simultaneous,
        meta_q_abs_log_width * meta_q_abs_log_simultaneous_factor
      ),
      meta_q_abs_log_simultaneous_calibration = meta_q_abs_log_simultaneous,
      meta_q_abs_log_total = meta_q_abs_log_width_total,
      meta_q_abs_log_simultaneous_total = pmax(
        meta_q_abs_log_width_total,
        dplyr::coalesce(meta_q_abs_log_simultaneous, meta_q_abs_log_width * meta_q_abs_log_simultaneous_factor),
        na.rm = TRUE
      ),
      meta_simultaneous_interval_factor = dplyr::if_else(
        is.finite(meta_q_abs_log_simultaneous_total),
        exp(meta_q_abs_log_simultaneous_total),
        NA_real_
      ),
      meta_post_selection_multiplier_lo = dplyr::if_else(
        is.finite(multiplier_pred) & multiplier_pred > 0 & is.finite(meta_q_abs_log_total),
        multiplier_pred * exp(-meta_q_abs_log_total),
        NA_real_
      ),
      meta_post_selection_multiplier_hi = dplyr::if_else(
        is.finite(multiplier_pred) & multiplier_pred > 0 & is.finite(meta_q_abs_log_total),
        multiplier_pred * exp(meta_q_abs_log_total),
        NA_real_
      ),
      meta_post_selection_interval_log_width = dplyr::if_else(
        is.finite(meta_q_abs_log_total),
        2 * sqrt(meta_q_abs_log_total^2 +
                   dplyr::coalesce(q_abs_log_structural, 0)^2),
        NA_real_
      ),
      meta_interval_factor = dplyr::if_else(
        is.finite(meta_q_abs_log_total),
        exp(meta_q_abs_log_total),
        NA_real_
      ),
      selected_policy = policy,
      selected_policy_display = dplyr::coalesce(policy_display, policy),
      anchor_selection_local_distance = dplyr::coalesce(
        local_weighted_mean_combined_distance,
        local_min_combined_distance
      ),
      selection_tier = "meta_policy_lexicographic_selection"
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      .policy_row_id = seq_len(dplyr::n()),
      meta_policy_predicted_score = .meta_predicted_score,
      uncertainty_cost_log_width = meta_post_selection_interval_log_width,
      uncertainty_eligible = dplyr::coalesce(valid_prediction, FALSE) &
        is.finite(meta_post_selection_interval_log_width) &
        meta_post_selection_interval_log_width > 0
    )

  selected_rows <- scored |>
    dplyr::group_split(anchor_model_id, anchor_species, .keep = TRUE) |>
    purrr::map_dfr(function(.x) {
      select_anchor_policies(
        policy_tbl = .x,
        uncertainty_rule = uncertainty_rule,
        u_tol_rel = u_tol_rel,
        u_tol_abs = u_tol_abs,
        one_se_multiplier = one_se_multiplier,
        local_distance_tolerance = local_distance_tolerance
      ) |>
        dplyr::mutate(
          selected_policy_display = dplyr::coalesce(selected_policy_display, policy_display, selected_policy),
          selection_tier = dplyr::coalesce(selection_tier, "meta_policy_lexicographic_selection")
        ) |>
        dplyr::select(
          anchor_model_id,
          anchor_species,
          .policy_row_id,
          selected_policy,
          selected_policy_display,
          selection_tier,
          anchor_selection_min_validation_error,
          anchor_selection_validation_threshold,
          anchor_selection_min_uncertainty_width,
          anchor_selection_uncertainty_threshold
        )
    })

  selection_diag <- selected_rows |>
    dplyr::select(
      anchor_model_id,
      anchor_species,
      anchor_selection_min_validation_error,
      anchor_selection_validation_threshold,
      anchor_selection_min_uncertainty_width,
      anchor_selection_uncertainty_threshold
    ) |>
    dplyr::distinct()

  scored <- scored |>
    dplyr::left_join(
      selected_rows |>
        dplyr::select(
          .policy_row_id,
          .selected_policy = selected_policy,
          .selected_policy_display = selected_policy_display,
          .selected_selection_tier = selection_tier
        ) |>
        dplyr::mutate(is_selected = TRUE),
      by = ".policy_row_id"
    ) |>
    dplyr::left_join(
      selection_diag,
      by = c("anchor_model_id", "anchor_species")
    ) |>
    dplyr::mutate(
      is_selected = dplyr::coalesce(is_selected, FALSE),
      selected_policy = dplyr::if_else(is_selected, dplyr::coalesce(.selected_policy, policy), NA_character_),
      selected_policy_display = dplyr::if_else(
        is_selected,
        dplyr::coalesce(.selected_policy_display, policy_display, policy),
        NA_character_
      ),
      selection_tier = dplyr::coalesce(.selected_selection_tier, selection_tier, "meta_policy_lexicographic_selection")
    ) |>
    dplyr::select(-dplyr::any_of(c(
      ".policy_row_id",
      ".meta_min_predicted_score",
      ".selected_policy",
      ".selected_policy_display",
      ".selected_selection_tier"
    ))) |>
    dplyr::arrange(
      anchor_species,
      .meta_predicted_score,
      policy
    )

  scored
}

#' @return A scored tibble.
#' @name predict.PolicyLearner
S7::method(predict_generic, PolicyLearner) <- .predict_policy_learner

methods::setMethod(
  "predict",
  signature(object = "tsbiomass::PolicyLearner"),
  function(object,
           new_policy_tbl,
           use_support_bin_intervals = NULL,
           max_selection_tolerance = NULL,
           ...) {
    .predict_policy_learner(
      object = object,
      new_policy_tbl = new_policy_tbl,
      use_support_bin_intervals = use_support_bin_intervals,
      max_selection_tolerance = max_selection_tolerance
      )
    }
  )

`predict.tsbiomass::PolicyLearner` <- function(object,
                                               new_policy_tbl,
                                               use_support_bin_intervals = NULL,
                                               max_selection_tolerance = NULL,
                                               ...) {
  .predict_policy_learner(
    object = object,
    new_policy_tbl = new_policy_tbl,
    use_support_bin_intervals = use_support_bin_intervals,
    max_selection_tolerance = max_selection_tolerance
  )
}

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
#' learner <- as_policy_learner(selector)
#' learner <- crossfit(learner)
#' learner <- fit(learner)
#' learner <- calibrate_uncertainty(learner)
#' plot(learner, type = "predicted_vs_observed")
#' plot(learner, type = "calibration_curve", outcome = "raw")
#' plot(learner, type = "residuals", view = "by_policy", outcome = "raw")
#' }
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
        is.finite(.meta_predicted_score),
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
        ggplot2::aes(x = .meta_predicted_score, y = .data[[observed_col]])
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
        is.finite(.meta_predicted_score),
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
      dplyr::mutate(calibration_bin = dplyr::ntile(.meta_predicted_score, n_bins_now)) |>
      dplyr::group_by(calibration_bin) |>
      dplyr::summarise(
        n = dplyr::n(),
        predicted_mean = mean(.meta_predicted_score, na.rm = TRUE),
        predicted_median = stats::median(.meta_predicted_score, na.rm = TRUE),
        observed_mean = mean(.data[[observed_col]], na.rm = TRUE),
        observed_median = stats::median(.data[[observed_col]], na.rm = TRUE),
        observed_q25 = stats::quantile(.data[[observed_col]], probs = 0.25, na.rm = TRUE, names = FALSE),
        observed_q75 = stats::quantile(.data[[observed_col]], probs = 0.75, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )

    return(
      ggplot2::ggplot(
        calibration_tbl,
        ggplot2::aes(x = predicted_median, y = observed_median)
      ) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#9f4f2f") +
        ggplot2::geom_linerange(
          ggplot2::aes(ymin = observed_q25, ymax = observed_q75),
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
        is.finite(.meta_predicted_score),
        is.finite(.data[[observed_col]])
      ) |>
      dplyr::mutate(
        residual_value = .data[[observed_col]] - .meta_predicted_score,
        abs_residual_value = abs(residual_value)
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
          plot_residual = pmax(abs_residual_value, .Machine$double.xmin),
          policy_display = stats::reorder(policy_display, plot_residual, stats::median, na.rm = TRUE)
        )
      return(
        ggplot2::ggplot(
          residual_tbl,
          ggplot2::aes(x = policy_display, y = plot_residual)
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
      dplyr::mutate(plot_residual = pmax(abs_residual_value, .Machine$double.xmin))
    return(
      ggplot2::ggplot(
        residual_tbl,
        ggplot2::aes(x = equation_branch_filter, y = plot_residual)
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
      dplyr::filter(is.finite(.meta_predicted_score)) |>
      dplyr::mutate(
        policy_display = stats::reorder(policy_display, .meta_predicted_score, stats::median, na.rm = TRUE)
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
        ggplot2::aes(x = policy_display, y = .meta_predicted_score)
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

    return(
      {
        support_levels <- c(
          "Lowest support",
          "Lower support",
          "Middle support",
          "Higher support",
          "Highest support"
        )
        plot_df[[support_col]] <- dplyr::case_when(
          plot_df[[support_col]] %in% support_levels ~ plot_df[[support_col]],
          as.character(plot_df[[support_col]]) == "support_bin_1" ~ "Lowest support",
          as.character(plot_df[[support_col]]) == "support_bin_2" ~ "Lower support",
          as.character(plot_df[[support_col]]) == "support_bin_3" ~ "Middle support",
          as.character(plot_df[[support_col]]) == "support_bin_4" ~ "Higher support",
          as.character(plot_df[[support_col]]) == "support_bin_5" ~ "Highest support",
          TRUE ~ as.character(plot_df[[support_col]])
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
      }
    )
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
            y = "Selected row count",
            title = "Cross-fitted meta-policy selections"
          ) +
          ggplot2::theme_minimal(base_size = 12)
      )
    }

    count_tbl <- selected_tbl |>
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
    dplyr::count(anchor_species, policy_display, branch_display, name = "n_selected") |>
    dplyr::group_by(anchor_species) |>
    dplyr::mutate(
      total_selected = sum(n_selected),
      win_fraction = n_selected / total_selected
    ) |>
    dplyr::arrange(dplyr::desc(win_fraction), policy_display, .by_group = TRUE) |>
    dplyr::summarise(
      top_policy = dplyr::first(policy_display),
      top_branch = dplyr::first(branch_display),
      top_win_fraction = dplyr::first(win_fraction),
      second_policy = dplyr::nth(policy_display, 2, default = NA_character_),
      second_branch = dplyr::nth(branch_display, 2, default = NA_character_),
      second_win_fraction = dplyr::nth(win_fraction, 2, default = NA_real_),
      stability_margin = top_win_fraction - dplyr::coalesce(second_win_fraction, 0),
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
      x = top_win_fraction,
      y = stats::reorder(anchor_species, top_win_fraction),
      xmin = pmax(0, top_win_fraction - stability_margin),
      xmax = top_win_fraction
    )
  ) +
    ggplot2::geom_linerange(linewidth = 1, colour = "#9ecae1") +
    ggplot2::geom_point(size = 2.8, colour = "#08519c") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%s (%.0f%%)", top_policy, 100 * top_win_fraction)),
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

S7::method(plot_generic, PolicyLearner) <- .plot_policy_learner

`plot.tsbiomass::PolicyLearner` <- function(x, y = NULL, ...) {
  .plot_policy_learner(x, y, ...)
}

#' Print a `PolicyLearner`
#'
#' @name print.PolicyLearner
#'
#' @param x A [PolicyLearner] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
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
#'
#' @param object A [PolicyLearner] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
S7::method(show_generic, PolicyLearner) <- function(object) {
  print(object)
  invisible(object)
}
