test_that("PolicyLearner stores crossfit, fit, and calibration state", {
  selector <- make_selector(benchmark = list(species_block_perf = minimal_policy_performance()))
  learner <- as_policy_learner(selector)

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
  learner <- as_policy_learner(selector)

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

  learner@config$metalearner <- list(
    selection_method = "glm",
    uncertainty_method = "glm",
    uncertainty_feature_cols = character()
  )

  expect_warning(
    learner <- calibrate_uncertainty(learner, min_bin_scores = 1L),
    "Falling back to point-score-scaled uncertainty"
  )
  expect_equal(learner@calibration$uncertainty_prediction_source, "point_score_fallback")
  expect_true(is.character(learner@calibration$uncertainty_warning))
})

test_that("PolicyLearner calibration uses modeled clipped outcomes when available", {
  predictions <- tibble::tibble(
    anchor_model_id = c("1", "2"),
    anchor_species = c("Alpha alpha", "Gamma gamma"),
    policy = c("same_species_closest", "same_species_closest"),
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
    config = list(metalearner = list()),
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

  expect_equal(learner@calibration$outcome_col, ".outcome")
  expect_equal(learner@calibration$raw_outcome_col, "error_abs_log")
  expect_true(all(learner@calibration$selected$.outcome == 1))
  expect_equal(learner@calibration$summary$median_outcome[[1]], 1)
  expect_equal(learner@calibration$coverage$median_abs_log_error[[1]], 1)
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
    policy = c("same_species_closest", "same_genus_weighted", "same_species_closest", "same_genus_weighted"),
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
  expect_equal(scored$policy[scored$is_selected], c("same_species_closest", "same_species_closest"))
  expect_true(all(c(
    "meta_q_abs_log_width",
    "meta_q_abs_log_conformal_factor",
    "meta_q_abs_log_total",
    "meta_uncertainty_source",
    "meta_uncertainty_fallback"
  ) %in% names(scored)))
  expect_true(all(is.finite(scored$meta_q_abs_log_total)))
  expect_true(all(startsWith(scored$meta_uncertainty_source, "direct_")))
  alpha_rows <- scored[scored$anchor_model_id == "1", , drop = FALSE]
  expect_gte(
    alpha_rows$meta_q_abs_log_total[alpha_rows$policy == "same_genus_weighted"],
    alpha_rows$meta_q_abs_log_total[alpha_rows$policy == "same_species_closest"]
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

test_that("meta-policy learner supports gam, ranger, and xgboost base methods", {
  training_data <- tibble::tibble(
    .outcome = c(0.10, 0.20, 0.15, 0.12, 0.25, 0.18, 0.14, 0.22, 0.11, 0.19),
    feature_a = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    feature_b = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
    .split_group = rep(c("g1", "g2", "g3", "g4", "g5"), each = 2)
  )

  for (method_now in c("gam", "ranger", "xgboost")) {
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

test_that("meta-policy learner applies method-specific tuning settings", {
  training_data <- tibble::tibble(
    .outcome = c(0.10, 0.20, 0.15, 0.12, 0.25, 0.18, 0.14, 0.22, 0.11, 0.19),
    feature_a = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    feature_b = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
    .split_group = rep(c("g1", "g2", "g3", "g4", "g5"), each = 2)
  )

  method_settings <- list(
    gam = list(fit_method = "ML", select_terms = FALSE),
    ranger = list(num_trees = 25L, min_node_size = 2L, sample_fraction = 0.8, replace = TRUE),
    xgboost = list(nrounds = 7L, eta = 0.2, max_depth = 3L)
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

  ranger_fit <- fit_meta_policy_learner(
    training_data = training_data,
    method = "ranger",
    feature_cols = c("feature_a", "feature_b"),
    inner_folds = 3L,
    seed = 20260524L,
    method_settings = method_settings
  )
  expect_equal(ranger_fit$method_arguments$num.trees, 25L)

  xgboost_fit <- fit_meta_policy_learner(
    training_data = training_data,
    method = "xgboost",
    feature_cols = c("feature_a", "feature_b"),
    inner_folds = 3L,
    seed = 20260524L,
    method_settings = method_settings
  )
  expect_equal(xgboost_fit$method_arguments$nrounds, 7L)
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
      equiv_ref = list(pairs = tibble::tibble(policy = "same_species_closest")),
      equiv_sets = tibble::tibble(policy = c("same_species_closest", "same_genus_weighted"))
    )
  )

  simulator <- as_policy_simulator(selector)

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
          equivalence_pairs = tibble::tibble(policy = "same_species_closest"),
          equivalence_classes = tibble::tibble(policy = c("same_species_closest", "same_genus_weighted")),
          conf_cal = minimal_uncertainty()$conf_cal
        )
      )
    },
    build_sensitivity_table = function(sensitivity_specs, sensitivity_map, config = NULL) {
      tibble::tibble(
        scenario = "baseline",
        benchmark_best_policy = "same_species_closest"
      )
    },
    bind_sensitivity_data = function(sensitivity_map) {
      list(
        select_ref = tibble::tibble(scenario = "baseline", policy = "same_species_closest"),
        equiv_pairs = tibble::tibble(scenario = "baseline", policy = "same_species_closest"),
        equiv_sets = tibble::tibble(scenario = "baseline", policy = "same_species_closest"),
        conf_cal = tibble::tibble(scenario = "baseline", policy = "same_species_closest")
      )
    },
    .package = "tsbiomass"
  )

  simulator <- simulate(simulator)

  expect_true(S7::S7_inherits(simulator, PolicySimulator))
  expect_equal(simulator@manifest$scenario[[1]], "baseline")
  expect_true("select_ref" %in% names(simulator@tables))
  expect_equal(simulator@tables$select_ref$policy[[1]], "same_species_closest")
})

test_that("sensitivity generics return stored simulator outputs directly", {
  selector <- make_selector()
  simulator <- PolicySimulator(
    selector = selector,
    config = list(),
    scenarios = list(),
    results = list(),
    manifest = tibble::tibble(scenario = "baseline", benchmark_best_policy = "same_species_closest"),
    tables = list(
      select_ref = tibble::tibble(scenario = "baseline", policy = "same_species_closest"),
      equiv_pairs = tibble::tibble(scenario = "baseline", policy = "same_species_closest"),
      equiv_sets = tibble::tibble(scenario = "baseline", policy = "same_species_closest"),
      conf_cal = tibble::tibble(scenario = "baseline", policy = "same_species_closest")
    )
  )

  expect_equal(build_sensitivity_table(simulator)$scenario[[1]], "baseline")
  expect_equal(bind_sensitivity_data(simulator)$select_ref$policy[[1]], "same_species_closest")
})
