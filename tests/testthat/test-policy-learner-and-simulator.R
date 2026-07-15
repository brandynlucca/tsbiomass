test_that("PolicyLearner stores crossfit, fit, and calibration state", {
  selector <- make_selector(benchmark = list(species_block_perf = minimal_policy_performance()))
  learner <- as_policylearner(selector)

  testthat::local_mocked_bindings(
    prepare_meta_policy_data = function(policy_perf, ...) {
      tibble::as_tibble(policy_perf)
    },
    crossfit_meta_policy_learner = function(...) {
      list(predictions = minimal_crossfit_predictions())
    },
    .package = "tsbiomass"
  )
  learner <- crossfit(learner)

  expect_true(nrow(learner@training_data) > 0)
  expect_true(length(learner@crossfit) > 0)

  testthat::local_mocked_bindings(
    fit_meta_policy_learner = function(...) {
      structure(list(method = "glm"), class = "tsb_meta_policy_learner")
    },
    .package = "tsbiomass"
  )
  learner <- fit(learner)

  expect_s3_class(learner@fitted_model$model, "tsb_meta_policy_learner")

  learner <- calibrate_uncertainty(learner, min_bin_scores = 1L)

  expect_true(length(learner@calibration) > 0)
  expect_true(all(c(
    "selected",
    "bin_quantiles",
    "coverage",
    "local_width_lookup",
    "width_factor_global",
    "width_prediction_source",
    "width_coverage",
    "uncertainty_prediction_source",
    "uncertainty_method",
    "uncertainty_coverage"
  ) %in% names(learner@calibration)))
})

test_that("PolicyLearner warns and records explicit uncertainty fallback state", {
  selector <- make_selector(benchmark = list(species_block_perf = minimal_policy_performance()))
  learner <- as_policylearner(selector)

  testthat::local_mocked_bindings(
    prepare_meta_policy_data = function(policy_perf, ...) {
      tibble::as_tibble(policy_perf)
    },
    crossfit_meta_policy_learner = function(...) {
      list(predictions = minimal_crossfit_predictions())
    },
    .package = "tsbiomass"
  )
  learner <- crossfit(learner)

  testthat::local_mocked_bindings(
    fit_meta_policy_learner = function(...) {
      structure(list(method = "glm"), class = "tsb_meta_policy_learner")
    },
    .package = "tsbiomass"
  )
  learner <- fit(learner)

  learner@config$selection <- list(method = "glm")
  learner@config$uncertainty <- list(method = "glm", feature_cols = character())

  expect_warning(
    learner <- calibrate_uncertainty(learner, min_bin_scores = 1L),
    "Falling back to point-score-scaled uncertainty"
  )
  expect_equal(learner@calibration$uncertainty_prediction_source, "point_score_fallback")
  expect_true(is.character(learner@calibration$uncertainty_warning))
})

test_that("PolicyLearner uncertainty calibration respects uncertainty-specific learner settings", {
  selector <- make_selector(benchmark = list(species_block_perf = minimal_policy_performance()))
  learner <- as_policylearner(selector)

  testthat::local_mocked_bindings(
    prepare_meta_policy_data = function(policy_perf, ...) {
      tibble::as_tibble(policy_perf)
    },
    crossfit_meta_policy_learner = function(...) {
      list(predictions = minimal_crossfit_predictions())
    },
    fit_meta_policy_learner = function(method = "glm", ...) {
      structure(list(method = method), class = "tsb_meta_policy_learner")
    },
    .package = "tsbiomass"
  )
  learner <- crossfit(learner)
  learner <- fit(learner)

  learner@config$selection <- list(
    method = "glm",
    super_methods = c("glm", "rpart"),
    method_settings = list(rf = list(num_trees = 50L))
  )
  learner@config$uncertainty <- list(
    method = "super_learner",
    super_methods = "rf",
    method_settings = list(rf = list(num_trees = 7L))
  )

  captured <- new.env(parent = emptyenv())
  captured$crossfit_super_methods <- NULL
  captured$fit_super_methods <- NULL
  captured$crossfit_num_trees <- NULL
  captured$fit_num_trees <- NULL

  testthat::local_mocked_bindings(
    prepare_meta_policy_data = function(policy_perf, ...) {
      tibble::as_tibble(policy_perf)
    },
    crossfit_meta_policy_learner = function(super_methods = NULL, method_settings = NULL, ...) {
      captured$crossfit_super_methods <- super_methods
      captured$crossfit_num_trees <- method_settings$rf$num_trees
      list(predictions = minimal_crossfit_predictions())
    },
    fit_meta_policy_learner = function(method = "glm", super_methods = NULL, method_settings = NULL, ...) {
      captured$fit_super_methods <- super_methods
      captured$fit_num_trees <- method_settings$rf$num_trees
      structure(list(method = method), class = "tsb_meta_policy_learner")
    },
    .package = "tsbiomass"
  )

  learner <- calibrate_uncertainty(learner, min_bin_scores = 1L)

  expect_equal(captured$crossfit_super_methods, "rf")
  expect_equal(captured$fit_super_methods, "rf")
  expect_equal(captured$crossfit_num_trees, 7L)
  expect_equal(captured$fit_num_trees, 7L)
  expect_equal(learner@calibration$uncertainty_super_methods, "rf")
  expect_equal(learner@calibration$uncertainty_method_settings$rf$num_trees, 7L)
})

test_that("PolicyLearner uncertainty screening infers missing selection validity", {
  cfg <- minimal_config_data()
  cfg$selection$method <- "super_learner"
  cfg$selection$super_methods <- c("glm", "rpart")
  cfg$uncertainty$method <- "super_learner"
  cfg$uncertainty$super_methods <- c("glm", "rpart")
  cfg$uncertainty$screen_learners <- list(n_folds = 2L, seed = 7L)

  selector <- make_selector(benchmark = list(species_block_perf = minimal_policy_performance()))
  learner <- as_policylearner(selector, config = cfg)
  learner@crossfit <- list(
    result = list(predictions = minimal_crossfit_predictions()),
    outcome_col = "error_abs_log",
    uncertainty_outcome_col = "error_abs_log",
    outcome_transform = "identity",
    lambda_rule = "lambda.1se",
    alpha = 1,
    inner_folds = 2L,
    metalearner_loss = "squared_error",
    group_col = NULL,
    selection_method_settings = cfg$selection$method_settings,
    selection_super_methods = cfg$selection$super_methods
  )

  captured <- new.env(parent = emptyenv())
  captured$policy_perf <- NULL
  testthat::local_mocked_bindings(
    crossfit_meta_policy_learner = function(policy_perf, ...) {
      captured$policy_perf <- tibble::as_tibble(policy_perf)
      list(
        learner_timings = tibble::tibble(
          outer_fold = c(1L, 2L),
          method = c("glm", "glm"),
          succeeded_oof = TRUE,
          succeeded_refit = TRUE,
          weight = c(0.5, 0.5),
          test_mae = c(0.1, 0.2),
          test_rmse = c(0.2, 0.3),
          total_seconds = c(0.01, 0.01),
          error = NA_character_
        )
      )
    },
    .package = "tsbiomass"
  )

  scorecard <- screen_learners(learner, stage = "uncertainty")

  expect_s7_class(scorecard, Scorecard)
  expect_true("selection_valid" %in% names(captured$policy_perf))
  expect_true(all(captured$policy_perf$selection_valid))
})

test_that("PolicyLearner selection fit respects selection-specific learner settings", {
  selector <- make_selector(benchmark = list(species_block_perf = minimal_policy_performance()))
  learner <- as_policylearner(selector)

  learner@config$selection <- list(
    method = "super_learner",
    super_methods = c("glm", "rf"),
    method_settings = list(rf = list(num_trees = 11L))
  )
  learner@config$uncertainty <- list(
    method = "glm",
    super_methods = "rpart",
    method_settings = list(rf = list(num_trees = 7L))
  )

  captured <- new.env(parent = emptyenv())
  captured$crossfit_super_methods <- NULL
  captured$fit_super_methods <- NULL
  captured$crossfit_num_trees <- NULL
  captured$fit_num_trees <- NULL

  testthat::local_mocked_bindings(
    prepare_meta_policy_data = function(policy_perf, ...) {
      tibble::as_tibble(policy_perf)
    },
    crossfit_meta_policy_learner = function(super_methods = NULL, method_settings = NULL, ...) {
      captured$crossfit_super_methods <- super_methods
      captured$crossfit_num_trees <- method_settings$rf$num_trees
      list(predictions = minimal_crossfit_predictions())
    },
    fit_meta_policy_learner = function(method = "glm", super_methods = NULL, method_settings = NULL, ...) {
      captured$fit_super_methods <- super_methods
      captured$fit_num_trees <- method_settings$rf$num_trees
      structure(list(method = method), class = "tsb_meta_policy_learner")
    },
    .package = "tsbiomass"
  )

  learner <- crossfit(learner)
  learner <- fit(learner)

  expect_equal(captured$crossfit_super_methods, c("glm", "rf"))
  expect_equal(captured$fit_super_methods, c("glm", "rf"))
  expect_equal(captured$crossfit_num_trees, 11L)
  expect_equal(captured$fit_num_trees, 11L)
  expect_equal(learner@crossfit$selection_method_settings$rf$num_trees, 11L)
  expect_equal(learner@fitted_model$selection_method_settings$rf$num_trees, 11L)
  expect_equal(learner@fitted_model$selection_super_methods, c("glm", "rf"))
})

test_that("PolicyLearner uncertainty calibration honors explicit override args", {
  selector <- make_selector(benchmark = list(species_block_perf = minimal_policy_performance()))
  learner <- as_policylearner(selector)

  testthat::local_mocked_bindings(
    prepare_meta_policy_data = function(policy_perf, ...) {
      tibble::as_tibble(policy_perf)
    },
    crossfit_meta_policy_learner = function(...) {
      list(predictions = minimal_crossfit_predictions())
    },
    fit_meta_policy_learner = function(method = "glm", ...) {
      structure(list(method = method), class = "tsb_meta_policy_learner")
    },
    .package = "tsbiomass"
  )
  learner <- crossfit(learner)
  learner <- fit(learner)

  learner@config$selection <- list(method = "glm")
  learner@config$uncertainty <- list(
    method = "super_learner",
    super_methods = c("glm", "rpart"),
    method_settings = list(rf = list(num_trees = 3L))
  )

  captured <- new.env(parent = emptyenv())
  captured$crossfit_method <- NULL
  captured$crossfit_super_methods <- NULL
  captured$crossfit_num_trees <- NULL
  captured$fit_method <- NULL
  captured$fit_super_methods <- NULL
  captured$fit_num_trees <- NULL

  testthat::local_mocked_bindings(
    prepare_meta_policy_data = function(policy_perf, ...) {
      tibble::as_tibble(policy_perf)
    },
    crossfit_meta_policy_learner = function(method = NULL, super_methods = NULL, method_settings = NULL, ...) {
      captured$crossfit_method <- method
      captured$crossfit_super_methods <- super_methods
      captured$crossfit_num_trees <- method_settings$rf$num_trees
      list(predictions = minimal_crossfit_predictions())
    },
    fit_meta_policy_learner = function(method = "glm", super_methods = NULL, method_settings = NULL, ...) {
      captured$fit_method <- method
      captured$fit_super_methods <- super_methods
      captured$fit_num_trees <- method_settings$rf$num_trees
      structure(list(method = method), class = "tsb_meta_policy_learner")
    },
    .package = "tsbiomass"
  )

  learner <- calibrate_uncertainty(
    learner,
    min_bin_scores = 1L,
    uncertainty_method = "super_learner",
    uncertainty_super_methods = "rf",
    uncertainty_method_settings = list(rf = list(num_trees = 9L))
  )

  expect_equal(captured$crossfit_method, "super_learner")
  expect_equal(captured$fit_method, "super_learner")
  expect_equal(captured$crossfit_super_methods, "rf")
  expect_equal(captured$fit_super_methods, "rf")
  expect_equal(captured$crossfit_num_trees, 9L)
  expect_equal(captured$fit_num_trees, 9L)
  expect_equal(learner@calibration$uncertainty_super_methods, "rf")
  expect_equal(learner@calibration$uncertainty_method_settings$rf$num_trees, 9L)
})

test_that("PolicyLearner prediction uses pooled local width conformal factors", {
  local_lookup <- policy_learner_build_local_lookup(
    tbl = tibble::tibble(
      anchor_model_id = c("1", "2", "3"),
      anchor_species = c("Alpha alpha", "Alpha alpha", "Gamma gamma"),
      anchor_family = c("Alphaidae", "Alphaidae", "Gammaidae"),
      policy = c("closest_within_species", "closest_within_species", "weighted_mean_within_genus"),
      equation_branch_filter = "all",
      width_ratio = c(2.0, 2.0, 1.2)
    ),
    value_col = "width_ratio",
    alpha = 0.50,
    global_default = 1.5,
    min_scores = 2L
  )

  learner <- PolicyLearner(
    selector = make_selector(),
    config = list(),
    training_data = tibble::tibble(),
    crossfit = list(),
    fitted_model = list(model = structure(list(method = "glm"), class = "tsb_meta_policy_learner")),
    calibration = list(
      width_model = list(model = NULL),
      width_factor_global = 1.5,
      width_factor_simultaneous_global = 1.8,
      local_width_lookup = local_lookup,
      uncertainty_prediction_source = "point_score_fallback",
      support_cutpoints = c(0, 0.5, 1),
      n_bins = 2L,
      support_bin_labels = c(
        support_bin_1 = "Lower support",
        support_bin_2 = "Higher support"
      ),
      bin_quantiles = tibble::tibble(
        post_selection_support_bin = c("support_bin_1", "support_bin_2"),
        q_abs_log = c(0.10, 0.20),
        simultaneous_species_max_q_abs_log = c(0.20, 0.30)
      ),
      q_global = 0.15,
      q_simultaneous_global = 0.25,
      max_selection_tolerance = 1e-12
    )
  )

  new_policy_tbl <- tibble::tibble(
    anchor_model_id = c("1", "3", "4"),
    anchor_species = c("Alpha alpha", "Gamma gamma", "Delta delta"),
    anchor_family = c("Alphaidae", "Gammaidae", "Deltaidae"),
    policy = c("closest_within_species", "weighted_mean_within_genus", "closest_within_species"),
    equation_branch_filter = c("all", "all", "all"),
    multiplier_pred = c(1.10, 1.15, 1.20),
    n_valid_models = c(3L, 3L, 3L),
    local_weighted_mean_combined_distance = c(0.10, 0.12, 0.15),
    local_effective_support = c(3.0, 2.0, 3.0),
    local_structural_q_abs_log = c(0.05, 0.05, 0.05)
  )

  testthat::local_mocked_bindings(
    predict_meta_policy_score = function(object, new_policy_tbl) {
      new_policy_tbl$.meta_predicted_score <- c(0.12, 0.12, 0.12)
      new_policy_tbl
    },
    .package = "tsbiomass"
  )

  scored <- predict(learner, new_policy_tbl)
  scored_lookup <- scored |>
    dplyr::select(
      "anchor_model_id",
      "meta_q_abs_log_factor_source",
      "meta_q_abs_log_conformal_factor"
    ) |>
    dplyr::arrange(.data$anchor_model_id)

  expect_equal(
    scored_lookup$meta_q_abs_log_factor_source,
    c("species_policy_branch", "species_policy_branch_shrunk", "policy_branch")
  )
  expect_equal(
    round(scored_lookup$meta_q_abs_log_conformal_factor, 3),
    c(2.0, 1.6, 2.0)
  )
})

test_that("PolicyLearner calibration uses modeled clipped outcomes when available", {
  predictions <- tibble::tibble(
    anchor_model_id = c("1", "2"),
    anchor_species = c("Alpha alpha", "Gamma gamma"),
    policy = c("closest_within_species", "closest_within_species"),
    equation_branch_filter = c("all", "all"),
    valid_prediction = c(TRUE, TRUE),
    n_valid_models = c(1L, 1L),
    local_effective_support = c(1, 1),
    .meta_predicted_score = c(0.20, 0.30),
    error_abs_log = c(100, 1),
    .outcome = c(1, 1),
    .outcome_raw = c(100, 1),
    .outcome_was_clipped = c(TRUE, FALSE)
  )
  learner <- PolicyLearner(
    selector = NULL,
    config = list(selection = list()),
    training_data = tibble::tibble(),
    crossfit = list(
      result = list(
        predictions = predictions,
        outcome_clip_quantile = 0.5
      ),
      outcome_col = "error_abs_log"
    ),
    fitted_model = list(),
    calibration = list()
  )

  testthat::local_mocked_bindings(
    policy_learner_uncertainty_feature_cols = function(...) character(),
    .package = "tsbiomass"
  )

  expect_warning(
    learner <- calibrate_uncertainty(
      learner,
      max_selection_tolerance = 1e-12,
      min_bin_scores = 1L
    ),
    "Falling back to point-score-scaled uncertainty"
  )

  expect_equal(learner@calibration$outcome_col, ".outcome_raw")
  expect_equal(learner@calibration$raw_outcome_col, "error_abs_log")
  expect_true(all(learner@calibration$selected$.outcome_raw %in% c(1, 100)))
  expect_true(any(learner@calibration$selected$.outcome_raw > learner@calibration$selected$.outcome))
  expect_equal(learner@calibration$summary$median_outcome[[1]], 50.5)
  expect_equal(learner@calibration$coverage$median_abs_log_error[[1]], 50.5)
})

test_that("PolicyLearner predict ranks and selects anchor-policy rows", {
  selector <- make_selector()
  learner <- PolicyLearner(
    selector = selector,
    config = list(),
    training_data = tibble::tibble(),
    crossfit = list(),
    fitted_model = list(model = structure(list(method = "glm"), class = "tsb_meta_policy_learner")),
    calibration = list(
      support_cutpoints = c(0, 0.5, 1),
      n_bins = 2L,
      support_bin_labels = c(
        support_bin_1 = "Lower support",
        support_bin_2 = "Higher support"
      ),
      bin_quantiles = tibble::tibble(
        post_selection_support_bin = c("support_bin_1", "support_bin_2"),
        q_abs_log = c(0.10, 0.20),
        simultaneous_species_max_q_abs_log = c(0.20, 0.30)
      ),
      q_global = 0.15,
      q_simultaneous_global = 0.25,
      max_selection_tolerance = 1e-12
    )
  )

  new_policy_tbl <- tibble::tibble(
    anchor_model_id = c("1", "1", "4", "4"),
    anchor_species = c("Alpha alpha", "Alpha alpha", "Gamma gamma", "Gamma gamma"),
    policy = c("closest_within_species", "weighted_mean_within_genus", "closest_within_species", "weighted_mean_within_genus"),
    multiplier_pred = c(1.10, 1.30, 1.15, 1.25),
    n_valid_models = c(3L, 4L, 2L, 5L),
    local_weighted_mean_combined_distance = c(0.10, 0.25, 0.15, 0.20),
    local_effective_support = c(3.0, 4.0, 2.0, 5.0),
    local_structural_q_abs_log = c(0.05, 0.10, 0.04, 0.08)
  )

  testthat::local_mocked_bindings(
    predict_meta_policy_score = function(object, new_policy_tbl) {
      new_policy_tbl$.meta_predicted_score <- c(0.10, 0.30, 0.20, 0.05)
      new_policy_tbl
    },
    .package = "tsbiomass"
  )

  scored <- predict(learner, new_policy_tbl)

  expect_true(all(c(".meta_predicted_score", "is_selected", "meta_post_selection_multiplier_lo") %in% names(scored)))
  expect_true("post_selection_support_label" %in% names(scored))
  expect_setequal(unique(scored$post_selection_support_label), c("Lower support", "Higher support"))
  expect_equal(sum(scored$is_selected), 2)
  expect_equal(scored$policy[scored$is_selected], c("closest_within_species", "closest_within_species"))
  expect_true(all(c(
    "meta_q_abs_log_width",
    "meta_q_abs_log_conformal_factor",
    "meta_q_abs_log_total",
    "meta_uncertainty_source",
    "meta_uncertainty_fallback"
  ) %in% names(scored)))
  expect_true(all(is.finite(scored$meta_q_abs_log_total)))
  expect_equal(
    scored$meta_post_selection_interval_log_width,
    2 * scored$meta_q_abs_log_total
  )
  expect_equal(
    scored$meta_post_selection_multiplier_lo,
    scored$multiplier_pred * exp(-scored$meta_q_abs_log_total)
  )
  expect_equal(
    scored$meta_post_selection_multiplier_hi,
    scored$multiplier_pred * exp(scored$meta_q_abs_log_total)
  )
  expect_true(all(startsWith(scored$meta_uncertainty_source, "direct_")))
  alpha_rows <- scored[scored$anchor_model_id == "1", , drop = FALSE]
  expect_gte(
    alpha_rows$meta_q_abs_log_total[alpha_rows$policy == "weighted_mean_within_genus"],
    alpha_rows$meta_q_abs_log_total[alpha_rows$policy == "closest_within_species"]
  )
})

test_that("conditional-uncertainty features exclude taxonomic and overlap summaries", {
  width_features <- policy_learner_uncertainty_feature_cols(
    list(),
    tibble::tibble(
      local_weighted_mean_combined_distance = c(0.1, 0.2),
      weighted_mean_taxonomic_distance = c(0.4, 0.5),
      min_length_overlap_fraction = c(0.7, 0.8),
      local_min_species_distance = c(0.2, 0.3),
      local_min_trait_gower_distance = c(0.2, 0.3),
      local_effective_support = c(2, 3),
      policy = c("a", "b")
    )
  )

  expect_true("local_weighted_mean_combined_distance" %in% width_features)
  expect_true("local_effective_support" %in% width_features)
  expect_false("weighted_mean_taxonomic_distance" %in% width_features)
  expect_false("min_length_overlap_fraction" %in% width_features)
  expect_false("local_min_species_distance" %in% width_features)
  expect_false("local_min_trait_gower_distance" %in% width_features)
})

test_that("meta-policy learner supports gam, rf, and xgboost base methods", {
  training_data <- tibble::tibble(
    .outcome = c(0.10, 0.20, 0.15, 0.12, 0.25, 0.18, 0.14, 0.22, 0.11, 0.19),
    feature_a = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    feature_b = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
    .split_group = rep(c("g1", "g2", "g3", "g4", "g5"), each = 2)
  )

  for (method_now in c("gam", "rf", "xgboost")) {
    learner <- fit_meta_policy_learner(
      training_data = training_data,
      method = method_now,
      feature_cols = c("feature_a", "feature_b"),
      inner_folds = 3L,
      seed = 20260524L
    )
    scored <- predict_meta_policy_score(learner, training_data)
    expect_s3_class(learner, "tsb_meta_policy_learner")
    expect_true(all(is.finite(scored$.meta_predicted_score)))
  }
})

test_that("meta-policy learner supports lmm with unseen grouping levels", {
  testthat::skip_if_not_installed("lme4")

  training_data <- tibble::tibble(
    .outcome = c(0.10, 0.20, 0.15, 0.12, 0.25, 0.18, 0.14, 0.22, 0.11, 0.19),
    feature_a = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    feature_b = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
    .split_group = rep(c("g1", "g2", "g3", "g4", "g5"), each = 2)
  )

  learner <- suppressMessages(
    fit_meta_policy_learner(
      training_data = training_data,
      method = "lmm",
      feature_cols = c("feature_a", "feature_b"),
      inner_folds = 3L,
      seed = 20260524L,
      method_settings = list(
        lmm = list(
          fit_method = "ML",
          random_intercept = ".split_group"
        )
      )
    )
  )
  scored_train <- predict_meta_policy_score(learner, training_data)
  scored_new <- predict_meta_policy_score(
    learner,
    tibble::tibble(
      feature_a = c(2.5, 8.5),
      feature_b = c(8.5, 2.5),
      .split_group = c("g1", "novel_group")
    )
  )

  expect_s3_class(learner, "tsb_meta_policy_learner")
  expect_equal(learner$method_family, "lmm")
  expect_equal(learner$group_col, ".split_group")
  expect_true(all(is.finite(scored_train$.meta_predicted_score)))
  expect_true(all(is.finite(scored_new$.meta_predicted_score)))
})

test_that("lmm preserves explicitly configured non-trait groups through crossfit", {
  testthat::skip_if_not_installed("lme4")
  set.seed(1L)
  policy_perf <- tibble::tibble(
    outer_group = rep(paste0("outer_", seq_len(6L)), each = 6L),
    study_group = rep(rep(c("site_a", "site_b", "site_c"), each = 2L), 6L),
    policy = "test_policy",
    valid_prediction = TRUE,
    error_abs_log = stats::runif(36L, 0.1, 0.5),
    feature_a = stats::rnorm(36L),
    feature_b = stats::rnorm(36L)
  )

  result <- suppressMessages(tsbiomass:::crossfit_meta_policy_learner(
    policy_perf = policy_perf,
    group_col = "outer_group",
    n_folds = 3L,
    method = "lmm",
    feature_cols = c("feature_a", "feature_b"),
    outcome_col = "error_abs_log",
    outcome_transform = "identity",
    inner_folds = 2L,
    method_settings = list(
      lmm = list(fit_method = "ML", random_intercept = "study_group")
    ),
    workers = 1L,
    progress = FALSE
  ))

  expect_equal(nrow(result$predictions), nrow(policy_perf))
  expect_true(all(c("outer_group", "study_group") %in% names(result$predictions)))
  expect_true(all(is.finite(result$predictions$.meta_predicted_score)))
})

test_that("lmm rejects missing, constant, or multiple random intercepts", {
  training_data <- tibble::tibble(
    .outcome = c(0.1, 0.2, 0.3, 0.4),
    feature_a = seq_len(4L),
    configured_group = "one_level"
  )

  expect_error(
    tsbiomass:::resolve_meta_policy_lmm_group(training_data, "absent_group"),
    "absent from the training data"
  )
  expect_error(
    tsbiomass:::resolve_meta_policy_lmm_group(training_data, "configured_group"),
    "fewer than two observed levels"
  )
  expect_error(
    tsbiomass:::resolve_meta_policy_lmm_group(
      training_data,
      c("configured_group", "absent_group")
    ),
    "one explicit `random_intercept` column"
  )
})

test_that("super learner does not silently remove lmm when its random intercept is unavailable", {
  training_data <- tibble::tibble(
    .outcome = seq(0.10, 0.30, length.out = 18L),
    feature_a = seq_len(18L),
    anchor_family = NA_character_,
    .split_group = rep(paste0("fold_", seq_len(6L)), each = 3L)
  )

  expect_error(
    fit_meta_policy_super_learner(
      training_data = training_data,
      feature_cols = "feature_a",
      outcome_transform = "identity",
      lambda_rule = "lambda.min",
      inner_folds = 3L,
      seed = 42L,
      super_methods = c("mean", "lmm"),
      method_settings = list(
        lmm = list(fit_method = "REML", random_intercept = "anchor_family")
      ),
      progress = FALSE
    ),
    "fewer than two observed levels"
  )
})

test_that("configured random intercept metadata is joined by anchor model ID", {
  policy_perf <- tibble::tibble(
    anchor_model_id = c("model_b", "model_a", "model_b"),
    policy = c("p1", "p1", "p2")
  )
  candidate_models <- tibble::tibble(
    model_id = c("model_a", "model_b"),
    arbitrary_study_group = c("study_1", "study_2")
  )

  out <- tsbiomass:::attach_meta_policy_random_intercepts(
    policy_perf = policy_perf,
    candidate_models = candidate_models,
    random_intercepts = "arbitrary_study_group"
  )

  expect_equal(out$arbitrary_study_group, c("study_2", "study_1", "study_2"))
  expect_error(
    tsbiomass:::attach_meta_policy_random_intercepts(
      policy_perf = policy_perf,
      candidate_models = candidate_models,
      random_intercepts = "not_present"
    ),
    "absent from candidate models"
  )
})

test_that("meta-policy learner applies method-specific tuning settings", {
  training_data <- tibble::tibble(
    .outcome = c(0.10, 0.20, 0.15, 0.12, 0.25, 0.18, 0.14, 0.22, 0.11, 0.19),
    feature_a = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    feature_b = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
    .split_group = rep(c("g1", "g2", "g3", "g4", "g5"), each = 2)
  )

  method_settings <- list(
    gam = list(fit_method = "ML", select_terms = FALSE),
    rf = list(num_trees = 25L, min_node_size = 2L, sample_fraction = 0.8, replace = TRUE),
    xgboost = list(nrounds = 7L, nthread = 2L, eta = 0.2, max_depth = 3L)
  )

  gam_fit <- fit_meta_policy_learner(
    training_data = training_data,
    method = "gam",
    feature_cols = c("feature_a", "feature_b"),
    inner_folds = 3L,
    seed = 20260524L,
    method_settings = method_settings
  )
  expect_equal(gam_fit$method_arguments$method, "ML")

  rf_fit <- fit_meta_policy_learner(
    training_data = training_data,
    method = "rf",
    feature_cols = c("feature_a", "feature_b"),
    inner_folds = 3L,
    seed = 20260524L,
    method_settings = method_settings
  )
  expect_equal(rf_fit$method_arguments$num.trees, 25L)

  xgboost_fit <- fit_meta_policy_learner(
    training_data = training_data,
    method = "xgboost",
    feature_cols = c("feature_a", "feature_b"),
    inner_folds = 3L,
    seed = 20260524L,
    method_settings = method_settings
  )
  expect_equal(xgboost_fit$method_arguments$nrounds, 7L)
  expect_equal(xgboost_fit$method_arguments$params$nthread, 2L)
})

test_that("study-level structural uncertainty preserves within-group donor heterogeneity", {
  anchor_pdf <- tibble::tibble(
    length_cm = c(10, 20, 30),
    f_len = c(0.3, 0.4, 0.3)
  )
  rows <- tibble::tibble(
    study_reference_id = c("study_a", "study_a"),
    species_name = c("Alpha alpha", "Alpha alpha"),
    equation_form = c("length", "length"),
    slope_len = c(20, 24),
    intercept_len = c(-70, -66),
    w_adm = c(0.6, 0.4),
    combined_distance = c(0.10, 0.12),
    trait_gower_distance = c(0.20, 0.25)
  )
  pred <- tibble::tibble(
    policy_slope_len = 21.6,
    policy_intercept_len = -68.4,
    policy_sigma_bs_mean = tsbiomass:::equation_sigma_mean(21.6, -68.4, anchor_pdf)
  )
  policy_def <- list(aggregation_method = "study_equal_weight_mean")

  structural <- tsbiomass:::policy_structural_summary(
    rows = rows,
    policy_def = policy_def,
    pred = pred,
    anchor_pdf = anchor_pdf
  )

  expect_gt(structural$donor_slope_sd[[1]], 0)
  expect_gt(structural$donor_intercept_sd[[1]], 0)
  expect_true(is.finite(structural$local_structural_q_abs_log[[1]]))
})

test_that("policy prediction can reuse a precomputed equation row", {
  anchor_pdf <- tibble::tibble(
    length_cm = c(10, 20, 30),
    f_len = c(0.3, 0.4, 0.3)
  )
  equation_row <- tibble::tibble(
    policy_slope_len = 21.0,
    policy_intercept_len = -68.0
  )

  testthat::local_mocked_bindings(
    policy_equation = function(...) {
      stop("policy_prediction() should reuse the supplied equation row")
    },
    .package = "tsbiomass"
  )

  pred <- tsbiomass:::policy_prediction(
    rows = tibble::tibble(slope_len = 20, intercept_len = -70),
    policy_def = list(aggregation_method = "nearest_by_combined_distance"),
    anchor_sigma = 1,
    anchor_pdf = anchor_pdf,
    equation_row = equation_row
  )

  expect_equal(pred$policy_slope_len[[1]], equation_row$policy_slope_len[[1]])
  expect_equal(pred$policy_intercept_len[[1]], equation_row$policy_intercept_len[[1]])
  expect_true(is.finite(pred$multiplier_pred[[1]]))
})

test_that("policy summaries can reuse precomputed donor subsets", {
  anchor_pdf <- tibble::tibble(
    length_cm = c(10, 20, 30),
    f_len = c(0.3, 0.4, 0.3)
  )
  summary_rows <- tibble::tibble(
    model_id_chr = c("d1", "d2"),
    slope_len = c(20, 22),
    intercept_len = c(-70, -69),
    w_adm = c(0.6, 0.4),
    combined_distance = c(0.10, 0.12),
    trait_gower_distance = c(0.20, 0.25),
    d_species = c(0.30, 0.35),
    length_overlap_fraction = c(0.80, 0.75),
    depth_overlap_fraction = c(0.70, 0.65),
    overlap_same_species = c(FALSE, FALSE),
    biomass_multiplier_if_replace = c(1.05, 0.95)
  )
  structural_rows <- dplyr::mutate(summary_rows, .structural_weight = c(0.6, 0.4))
  pred <- tibble::tibble(
    policy_slope_len = 21.0,
    policy_intercept_len = -69.5,
    policy_sigma_bs_mean = tsbiomass:::equation_sigma_mean(21.0, -69.5, anchor_pdf)
  )

  testthat::local_mocked_bindings(
    policy_summary_rows = function(...) {
      stop("policy_support_summary() should reuse the supplied summary rows")
    },
    policy_structural_rows = function(...) {
      stop("policy_structural_summary() should reuse the supplied structural rows")
    },
    .package = "tsbiomass"
  )

  support <- tsbiomass:::policy_support_summary(
    rows = tibble::tibble(),
    policy_def = list(aggregation_method = "kernel_weighted_mean"),
    summary_rows = summary_rows
  )
  structural <- tsbiomass:::policy_structural_summary(
    rows = tibble::tibble(),
    policy_def = list(aggregation_method = "kernel_weighted_mean"),
    pred = pred,
    anchor_pdf = anchor_pdf,
    structural_rows = structural_rows
  )

  expect_equal(support$n_valid_models[[1]], 2L)
  expect_true(is.finite(support$local_effective_support[[1]]))
  expect_true(is.finite(structural$donor_slope_sd[[1]]))
  expect_true(is.finite(structural$local_structural_q_abs_log[[1]]))
})

test_that("PolicySimulator stores sensitivity reruns and exposes bound tables", {
  candidates <- set_reference_anchors(make_candidates(), selector = list(regional_body = "SWFSC"))
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(
      policy_perf = minimal_policy_performance(),
      species_block_perf = minimal_policy_performance()
    ),
    uncertainty = minimal_uncertainty(),
    selection = list(
      final_ref = minimal_selection_ref(),
      equiv_ref = list(pairs = tibble::tibble(policy = "closest_within_species")),
      equiv_sets = tibble::tibble(policy = c("closest_within_species", "weighted_mean_within_genus"))
    )
  )

  simulator <- as_policysimulator(selector)

  testthat::local_mocked_bindings(
    build_policy_sensitivity_scenarios = function(...) {
      list(
        baseline = list(
          candidate_models = selector@candidates@candidate_models,
          config = list(alpha = 0.65, k_species = 2, k_study = 1)
        )
      )
    },
    run_sensitivity_tests = function(...) {
      list(
        baseline = list(
          selection_ref = minimal_selection_ref(),
          equivalence_pairs = tibble::tibble(policy = "closest_within_species"),
          equivalence_classes = tibble::tibble(policy = c("closest_within_species", "weighted_mean_within_genus")),
          conf_cal = minimal_uncertainty()$conf_cal
        )
      )
    },
    construct_sensitivity_table = function(scenario_specifications, scenario_results, config = NULL) {
      tibble::tibble(
        scenario = "baseline",
        benchmark_best_policy = "closest_within_species"
      )
    },
    collect_sensitivity_results = function(scenario_results) {
      list(
        select_ref = tibble::tibble(scenario = "baseline", policy = "closest_within_species"),
        equiv_pairs = tibble::tibble(scenario = "baseline", policy = "closest_within_species"),
        equiv_sets = tibble::tibble(scenario = "baseline", policy = "closest_within_species"),
        conf_cal = tibble::tibble(scenario = "baseline", policy = "closest_within_species")
      )
    },
    .package = "tsbiomass"
  )

  simulator <- simulate(simulator)

  expect_true(S7::S7_inherits(simulator, PolicySimulator))
  expect_equal(simulator@manifest$scenario[[1]], "baseline")
  expect_true("select_ref" %in% names(simulator@tables))
  expect_equal(simulator@tables$select_ref$policy[[1]], "closest_within_species")
})

test_that("sensitivity generics return stored simulator outputs directly", {
  selector <- make_selector()
  simulator <- PolicySimulator(
    selector = selector,
    config = list(),
    scenarios = list(),
    results = list(),
    manifest = tibble::tibble(scenario = "baseline", benchmark_best_policy = "closest_within_species"),
    tables = list(
      select_ref = tibble::tibble(scenario = "baseline", policy = "closest_within_species"),
      equiv_pairs = tibble::tibble(scenario = "baseline", policy = "closest_within_species"),
      equiv_sets = tibble::tibble(scenario = "baseline", policy = "closest_within_species"),
      conf_cal = tibble::tibble(scenario = "baseline", policy = "closest_within_species")
    )
  )

  expect_equal(construct_sensitivity_table(simulator)$scenario[[1]], "baseline")
  expect_equal(collect_sensitivity_results(simulator)$select_ref$policy[[1]], "closest_within_species")
})
