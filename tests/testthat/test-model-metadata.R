test_that("set_model_metadata updates candidate and selected anchor rows", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  candidates <- set_reference_anchors(candidates, model_ids = "1")
  candidates <- Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = candidates@candidate_models,
    reference_anchors = candidates@reference_anchors,
    similarity_matrix = list(prepared = TRUE),
    gower_distances = list(distance_mode = "empirical_gower"),
    ordination = list(points = tibble::tibble()),
    admissibility = list(all_scores = tibble::tibble()),
    similarity_tuning = candidates@similarity_tuning
  )

  updated <- set_model_metadata(
    candidates,
    tibble::tibble(
      model_id = "1",
      slope_standard = 19.3,
      intercept_standard = -65.1
    )
  )

  model_row <- dplyr::filter(updated@candidate_models, as.character(.data$model_id) == "1")
  anchor_row <- dplyr::filter(updated@reference_anchors, as.character(.data$model_id) == "1")
  expect_equal(model_row$slope_standard, 19.3)
  expect_equal(model_row$intercept_standard, -65.1)
  expect_equal(anchor_row$slope_standard, 19.3)
  expect_equal(anchor_row$intercept_standard, -65.1)
  expect_length(updated@similarity_matrix, 0L)
  expect_length(updated@gower_distances, 0L)
  expect_length(updated@ordination, 0L)
  expect_length(updated@admissibility, 0L)
})

test_that("set_model_metadata restandardizes weight-referenced model metadata", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  candidates <- set_model_metadata(
    candidates,
    tibble::tibble(
      model_id = "1",
      equation_form = "mlog10_kg",
      slope = -16,
      intercept = -35,
      lw_a_g = 0.002,
      lw_b = 3.0
    )
  )
  candidates <- set_reference_anchors(candidates, model_ids = "1")

  updated <- set_model_metadata(
    candidates,
    tibble::tibble(
      model_id = "1",
      lw_a_g = 0.004,
      lw_b = 3.25
    )
  )

  expected_slope <- -16 + 10 * 3.25
  expected_intercept <- -35 + 10 * (log10(0.004) - 3)

  model_row <- dplyr::filter(updated@candidate_models, as.character(.data$model_id) == "1")
  anchor_row <- dplyr::filter(updated@reference_anchors, as.character(.data$model_id) == "1")

  expect_equal(model_row$lw_a, 0.004)
  expect_equal(anchor_row$lw_a, 0.004)
  expect_equal(model_row$slope_len, expected_slope)
  expect_equal(model_row$intercept_len, expected_intercept)
  expect_equal(model_row$slope_standard, expected_slope)
  expect_equal(model_row$intercept_standard, expected_intercept)
  expect_equal(anchor_row$slope_standard, expected_slope)
  expect_equal(anchor_row$intercept_standard, expected_intercept)
})

test_that("set_model_metadata preserves generalized model keys", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  models <- candidates@candidate_models
  models$model_id <- as.character(models$model_id)
  models <- dplyr::bind_rows(
    models,
    tibble::tibble(
      model_id = "generalized_one",
      species_name = "NA NA",
      genus = NA_character_,
      species = NA_character_,
      equation_form = "20log10_ind",
      slope = 20,
      intercept = -68,
      slope_len = 20,
      intercept_len = -68,
      slope_standard = 20,
      intercept_standard = -68
    )
  )
  candidates <- Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = models,
    reference_anchors = candidates@reference_anchors,
    similarity_matrix = candidates@similarity_matrix,
    gower_distances = candidates@gower_distances,
    ordination = candidates@ordination,
    admissibility = candidates@admissibility,
    similarity_tuning = candidates@similarity_tuning
  )

  updated <- set_model_metadata(
    candidates,
    tibble::tibble(
      model_id = "generalized_one",
      intercept = -69
    )
  )
  model_row <- dplyr::filter(
    updated@candidate_models,
    as.character(.data$model_id) == "generalized_one"
  )

  expect_true(model_row$is_group_model[[1]])
  expect_true(is.na(model_row$species_name[[1]]))
  expect_equal(model_row$model_species_label[[1]], "Generalized model")
  expect_false(any(updated@candidate_models$species_name == "NA NA", na.rm = TRUE))
})

test_that("candidate standardization synchronizes length-weight aliases", {
  rows <- tibble::tibble(
    model_id = "1",
    species_name = "Sardinops sagax",
    lw_a = 0.00762,
    lw_a_g = 0.00351860493755,
    lw_b = 3.253
  )

  out <- tsbiomass:::standardize_candidate_columns(rows)

  expect_equal(out$lw_a, out$lw_a_g)
  expect_equal(out$lw_a, 0.00351860493755)
})

test_that("weight-referenced FishBase metadata converts to standardized length coefficients", {
  rows <- tibble::tibble(
    scientific_name = "Sardinops sagax",
    equation_form = "mlog10_kg",
    slope = -14.9,
    intercept = -13.21,
    lw_b = 3.253,
    lw_a_g = 0.00351860493755
  )

  converted <- tsbiomass:::convert_to_length_form(rows)

  expect_equal(converted$slope_len, -14.9 + 10 * 3.253)
  expect_equal(
    converted$intercept_len,
    -13.21 + 10 * (log10(0.00351860493755) - 3)
  )
  expect_false(isTRUE(all.equal(converted$slope_len, 15.95, tolerance = 1e-8)))
  expect_false(isTRUE(all.equal(converted$intercept_len, -64.39045, tolerance = 1e-8)))
})

test_that("a query anchor accepts a literal length PDF list-column", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  length_pdf <- tibble::tibble(
    length_cm = c(10, 20, 30),
    f_len = c(0.2, 0.5, 0.3)
  )
  new_anchor <- candidates@candidate_models |>
    dplyr::filter(as.character(.data$model_id) == "1") |>
    dplyr::slice(1L)
  new_anchor$model_id <- "query_model"
  new_anchor$length_pdf_data <- list(length_pdf)
  density <- tsbiomass:::build_anchor_length_pdf(
    new_anchor,
    tsbiomass:::default_anchor_config()
  )

  expect_equal(density$length_cm, length_pdf$length_cm)
  expect_equal(density$f_len, length_pdf$f_len)
  new_anchor$length_pdf_data <- list(tibble::tibble(
    length_cm = c(10, 20),
    f_len = c(0, 0)
  ))
  expect_error(
    tsbiomass:::build_anchor_length_pdf(
      new_anchor,
      tsbiomass:::default_anchor_config()
    ),
    "positive finite support"
  )
})

test_that("a query anchor accepts raw empirical lengths in length_pdf_data", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  raw_lengths <- c(6, 7, 8, 8, 9, 10)
  new_anchor <- candidates@candidate_models |>
    dplyr::filter(as.character(.data$model_id) == "1") |>
    dplyr::slice(1L)
  new_anchor$model_id <- "query_model_raw_lengths"
  new_anchor$length_pdf_data <- list(raw_lengths)

  density <- tsbiomass:::build_anchor_length_pdf(
    new_anchor,
    tsbiomass:::default_anchor_config()
  )

  expect_equal(density, tsbiomass:::normalize_anchor_pdf_input(raw_lengths))
  expect_equal(range(density$length_cm), range(raw_lengths))
  expect_true(all(is.finite(density$f_len) & density$f_len >= 0))
})

test_that("set_model_metadata validates literal length PDF list-columns", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  expect_error(
    set_model_metadata(
      candidates,
      tibble::tibble(
        model_id = "1",
        length_pdf_data = list(tibble::tibble(
          length_cm = c(10, 20),
          f_len = c(0, 0)
        ))
      )
    ),
    "positive finite support"
  )
})

test_that("Alchemist query-distance augmentation uses the stored learner", {
  models <- tibble::tibble(
    model_id = c("a", "b", "query"),
    family = c("A", "B", "B")
  )
  existing <- matrix(
    c(0, 1, 1, 0),
    nrow = 2,
    dimnames = list(c("a", "b"), c("a", "b"))
  )
  learner <- structure(
    list(feature_cols = ".dist_family", weights = c(.dist_family = 1)),
    class = "Mahalanobis"
  )
  distance_state <- list(
    distance_mode = "alchemist_super_learner",
    distance_learner = learner,
    learned_directed_dist = existing,
    species_trait_names = "family",
    study_trait_names = character(0),
    feature_type = "gower",
    coherence_config = list(),
    taxonomic_distance = FALSE,
    feature_normalization = list(family = list(scale = NA_real_)),
    feature_cols = ".dist_family"
  )

  augmented <- tsbiomass:::augment_alchemist_query_distances(
    models,
    distance_state,
    query_model_ids = "query"
  )

  expect_equal(augmented$learned_directed_dist["a", "query"], 1)
  expect_equal(augmented$learned_directed_dist["b", "query"], 0)
  expect_error(
    tsbiomass:::augment_alchemist_query_distances(
      models,
      utils::modifyList(distance_state, list(distance_learner = NULL)),
      query_model_ids = "query"
    ),
    "fitted distance learner"
  )
})

test_that("Super Learner distances retain weighted disagreement diagnostics", {
  train_a <- data.frame(.dist_family = c(0, 1), outcome = c(0, 1))
  train_b <- data.frame(.dist_family = c(0, 1), outcome = c(0, 2))
  base_a <- structure(
    list(
      family = "glm",
      fit = stats::lm(outcome ~ .dist_family, data = train_a),
      feature_cols = ".dist_family"
    ),
    class = "BaseLearner"
  )
  base_b <- structure(
    list(
      family = "glm",
      fit = stats::lm(outcome ~ .dist_family, data = train_b),
      feature_cols = ".dist_family"
    ),
    class = "BaseLearner"
  )
  learner <- structure(
    list(
      feature_cols = ".dist_family",
      fit = list(base_a = base_a, base_b = base_b),
      weights = c(base_a = 0.5, base_b = 0.5),
      outcome_transform = "identity"
    ),
    class = "SuperLearner"
  )

  predicted <- tsbiomass:::predict_distance(
    learner,
    tibble::tibble(.dist_family = c(0, 1))
  )

  expect_true(isTRUE(attr(predicted, "distance_diagnostic_available")))
  expect_equal(attr(predicted, "distance_weighted_disagreement"), c(0, 0.5))
  expect_equal(dim(attr(predicted, "distance_base_predictions")), c(2L, 2L))
})

test_that("meta-policy Super Learner scores retain weighted disagreement diagnostics", {
  set.seed(20260814)
  training_data <- tibble::tibble(
    .outcome = abs(stats::rnorm(30L)),
    feature_a = stats::rnorm(30L)
  )
  learner <- tsbiomass:::fit_meta_policy_learner(
    training_data = training_data,
    method = "super_learner",
    feature_cols = "feature_a",
    super_methods = c("glm", "rpart"),
    inner_folds = 3L,
    seed = 20260814L
  )

  scored <- tsbiomass:::predict_meta_policy_score(learner, training_data)

  expect_true(all(scored$.meta_score_diagnostic_available))
  expect_true(all(is.finite(scored$.meta_score_weighted_disagreement)))
})

test_that("continuous support conformal uses only kernel-weighted calibration rows", {
  calibration <- tsbiomass:::build_continuous_support_conformal(
    calibration_rows = tibble::tibble(
      continuous_support_score = c(0, 0.5, 1),
      abs_log_residual = c(0.1, 0.3, 1.2)
    ),
    alpha = 0.1,
    bandwidth = 0.2
  )
  lookup <- tsbiomass:::continuous_support_conformal_quantiles(
    calibration,
    query_support_score = c(0, 1)
  )

  expect_equal(nrow(lookup), 2L)
  expect_true(all(is.finite(lookup$continuous_conformal_q_abs_log)))
  expect_true(all(lookup$continuous_conformal_effective_n > 1))
  expect_gt(
    lookup$continuous_conformal_q_abs_log[[2]],
    lookup$continuous_conformal_q_abs_log[[1]]
  )
  expect_error(
    tsbiomass:::continuous_support_conformal_quantiles(
      calibration,
      query_support_score = NA_real_
    ),
    "finite query support scores"
  )
  expect_error(
    tsbiomass:::build_continuous_support_conformal(
      calibration_rows = tibble::tibble(
        continuous_support_score = c(0, 1),
        abs_log_residual = c(0.1, 0.2)
      ),
      alpha = 0.1
    ),
    "explicit positive bandwidth"
  )
})

test_that("ensemble disagreement uncertainty is explicit and validates diagnostics", {
  components <- tsbiomass:::policy_learner_ensemble_disagreement_uncertainty(
    tibble::tibble(
      .meta_score_diagnostic_available = c(TRUE, TRUE, FALSE),
      .meta_score_weighted_disagreement = c(0.1, 0.2, NA_real_),
      learned_distance_diagnostic_available = c(TRUE, FALSE, TRUE),
      local_weighted_mean_learned_distance_disagreement = c(0.4, NA_real_, 0.5)
    ),
    enabled = TRUE,
    distance_weight = 2,
    meta_score_weight = 3
  )

  expect_true(all(components$ensemble_disagreement_uncertainty_enabled))
  expect_equal(components$meta_q_abs_log_distance_disagreement, c(0.8, 0, 1))
  expect_equal(components$meta_q_abs_log_meta_score_disagreement, c(0.3, 0.6, 0))
  expect_equal(
    components$meta_q_abs_log_disagreement,
    sqrt(c(0.8^2 + 0.3^2, 0.6^2, 1^2))
  )

  expect_error(
    tsbiomass:::policy_learner_ensemble_disagreement_uncertainty(
      tibble::tibble(
        .meta_score_diagnostic_available = TRUE,
        .meta_score_weighted_disagreement = NA_real_,
        learned_distance_diagnostic_available = FALSE,
        local_weighted_mean_learned_distance_disagreement = NA_real_
      ),
      enabled = TRUE,
      distance_weight = 1,
      meta_score_weight = 1
    ),
    "inconsistent"
  )
  expect_error(
    tsbiomass:::policy_learner_ensemble_disagreement_uncertainty(
      tibble::tibble(),
      enabled = TRUE,
      distance_weight = NULL,
      meta_score_weight = 1
    ),
    "explicit non-negative `distance_disagreement_weight`"
  )
  disabled <- tsbiomass:::policy_learner_ensemble_disagreement_uncertainty(
    tibble::tibble(row_id = 1:2),
    enabled = FALSE
  )
  expect_false(any(disabled$ensemble_disagreement_uncertainty_enabled))
  expect_equal(disabled$meta_q_abs_log_disagreement, c(0, 0))
})

test_that("PolicySelector dispatches an external existing-species query through stored learned distances", {
  candidates <- make_candidates(
    seed_similarity_tuning = FALSE,
    gower_distances = list(
      distance_mode = "alchemist_super_learner",
      distance_learner = structure(list(), class = "Mahalanobis")
    ),
    admissibility = list(all_scores = minimal_admissibility_scores())
  )
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(policy_perf = minimal_policy_performance()),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )
  learner <- PolicyLearner(
    selector = selector,
    config = list(),
    training_data = tibble::tibble(),
    crossfit = list(),
    fitted_model = list(model = structure(list(method = "glm"), class = "tsb_meta_policy_learner")),
    calibration = list(
      width_model = list(model = NULL),
      width_factor_global = 1,
      width_factor_simultaneous_global = 1,
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
  new_anchor <- candidates@candidate_models |>
    dplyr::filter(.data$model_id == 1L) |>
    dplyr::slice(1L) |>
    dplyr::mutate(model_id = 999L, model_id_chr = "999")
  captured <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    augment_alchemist_query_distances = function(candidate_models,
                                                 distance_state,
                                                 query_model_ids) {
      captured$learner_state <- distance_state$distance_learner
      captured$query_model_ids <- query_model_ids
      list(distance_mode = "alchemist_super_learner", query_distances_added = TRUE)
    },
    screen_one_anchor_admissibility = function(anchor_row, candidate_models, ...) {
      captured$screen_candidate_ids <- candidate_models@candidate_models$model_id
      captured$screen_distance_mode <- candidate_models@gower_distances$distance_mode
      list()
    },
    evaluate_policies = function(...) {
      tibble::tibble(
        policy = "closest_within_species",
        multiplier_pred = 1.1,
        n_valid_models = 2L,
        local_weighted_mean_combined_distance = 0.1,
        local_effective_support = 2,
        local_structural_q_abs_log = 0.05
      )
    },
    summarize_evaluation = function(...) {
      tibble::tibble(
        consensus_multiplier = 1.1,
        multiplier_q05 = 1.0,
        multiplier_q50 = 1.1,
        multiplier_q95 = 1.2,
        local_support_mass = 0.8,
        local_effective_support = 2
      )
    },
    predict_meta_policy_score = function(object, new_policy_tbl) {
      new_policy_tbl$.meta_predicted_score <- rep(0.1, nrow(new_policy_tbl))
      new_policy_tbl$.meta_score_diagnostic_available <- rep(FALSE, nrow(new_policy_tbl))
      new_policy_tbl$.meta_score_weighted_disagreement <- rep(NA_real_, nrow(new_policy_tbl))
      new_policy_tbl
    },
    .package = "tsbiomass"
  )

  predictions <- predict(
    selector,
    learner = learner,
    reference_anchors = new_anchor,
    reuse_admissibility = FALSE
  )

  expect_equal(captured$query_model_ids, 999L)
  expect_s3_class(captured$learner_state, "Mahalanobis")
  expect_equal(captured$screen_distance_mode, "alchemist_super_learner")
  expect_true(999L %in% captured$screen_candidate_ids)
  expect_equal(predictions@selections$anchor_model_id, "999")
  expect_equal(predictions@selections$anchor_species, "Alpha alpha")
  expect_true(predictions@selections$anchor_is_external)

  scorecard <- predict(as_referee(selector, learner = learner, predictions = predictions))
  expect_s7_class(scorecard, Scorecard)
  expect_equal(scorecard@selected$anchor_model_id, "999")

  inconsistent_predictions <- PolicyPredictions(
    intervals = dplyr::mutate(predictions@intervals, anchor_is_external = FALSE),
    selections = predictions@selections,
    consensus = predictions@consensus
  )
  expect_error(
    predict(as_referee(
      selector,
      learner = learner,
      predictions = inconsistent_predictions
    )),
    "must identify the same external anchor IDs"
  )
})

test_that("a referee scorecard builds for an external query anchor when the selector already has its own reference anchors", {
  candidates <- make_candidates(
    seed_similarity_tuning = FALSE,
    gower_distances = list(
      distance_mode = "alchemist_super_learner",
      distance_learner = structure(list(), class = "Mahalanobis")
    ),
    admissibility = list(all_scores = minimal_admissibility_scores()),
    reference_anchors = minimal_candidate_models() |>
      dplyr::filter(.data$model_id %in% c(1L, 2L))
  )
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(policy_perf = minimal_policy_performance()),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )
  learner <- PolicyLearner(
    selector = selector,
    config = list(),
    training_data = tibble::tibble(),
    crossfit = list(),
    fitted_model = list(model = structure(list(method = "glm"), class = "tsb_meta_policy_learner")),
    calibration = list(
      width_model = list(model = NULL),
      width_factor_global = 1,
      width_factor_simultaneous_global = 1,
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
  new_anchor <- candidates@candidate_models |>
    dplyr::filter(.data$model_id == 1L) |>
    dplyr::slice(1L) |>
    dplyr::mutate(model_id = 999L, model_id_chr = "999")

  testthat::local_mocked_bindings(
    augment_alchemist_query_distances = function(candidate_models, distance_state, query_model_ids) {
      list(distance_mode = "alchemist_super_learner", query_distances_added = TRUE)
    },
    screen_one_anchor_admissibility = function(anchor_row, candidate_models, ...) {
      list()
    },
    evaluate_policies = function(...) {
      tibble::tibble(
        policy = "closest_within_species",
        multiplier_pred = 1.1,
        n_valid_models = 2L,
        local_weighted_mean_combined_distance = 0.1,
        local_effective_support = 2,
        local_structural_q_abs_log = 0.05
      )
    },
    summarize_evaluation = function(...) {
      tibble::tibble(
        consensus_multiplier = 1.1,
        multiplier_q05 = 1.0,
        multiplier_q50 = 1.1,
        multiplier_q95 = 1.2,
        local_support_mass = 0.8,
        local_effective_support = 2
      )
    },
    predict_meta_policy_score = function(object, new_policy_tbl) {
      new_policy_tbl$.meta_predicted_score <- rep(0.1, nrow(new_policy_tbl))
      new_policy_tbl$.meta_score_diagnostic_available <- rep(FALSE, nrow(new_policy_tbl))
      new_policy_tbl$.meta_score_weighted_disagreement <- rep(NA_real_, nrow(new_policy_tbl))
      new_policy_tbl
    },
    .package = "tsbiomass"
  )

  predictions <- predict(
    selector,
    learner = learner,
    reference_anchors = new_anchor,
    reuse_admissibility = FALSE
  )
  expect_equal(predictions@selections$anchor_model_id, "999")
  expect_true(predictions@selections$anchor_is_external)

  scorecard <- predict(as_referee(selector, learner = learner, predictions = predictions))
  expect_s7_class(scorecard, Scorecard)
  expect_equal(scorecard@selected$anchor_model_id, "999")
})

test_that("PolicySelector dispatches an external query through prepared empirical Gower state", {
  candidates <- make_candidates(
    seed_similarity_tuning = FALSE,
    similarity_matrix = list(
      species_weights = c(genus = 1),
      study_weights = c(frequency = 1),
      alpha = 0.5,
      k_species = 2L,
      k_study = 2L
    ),
    gower_distances = list(distance_mode = "empirical_gower")
  )
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(policy_perf = minimal_policy_performance()),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )
  new_anchor <- candidates@candidate_models |>
    dplyr::filter(.data$model_id == 3L) |>
    dplyr::slice(1L) |>
    dplyr::mutate(model_id = 998L, model_id_chr = "998")
  captured <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    augment_alchemist_query_distances = function(...) {
      stop("learned distance augmentation must not run for empirical Gower")
    },
    prepare_similarities = function(candidate_models, ...) {
      captured$prepared_candidate_ids <- candidate_models@candidate_models$model_id
      list(candidate_models = candidate_models@candidate_models)
    },
    construct_gower_distances = function(similarity, ...) {
      captured$gower_constructed <- TRUE
      list(distance_mode = "empirical_gower", query_distances_added = TRUE)
    },
    screen_one_anchor_admissibility = function(anchor_row, candidate_models, ...) {
      captured$screen_distance_mode <- candidate_models@gower_distances$distance_mode
      list()
    },
    evaluate_policies = function(...) {
      tibble::tibble(
        policy = "closest_within_species",
        multiplier_pred = 1.1
      )
    },
    summarize_evaluation = function(...) {
      tibble::tibble(consensus_multiplier = 1.1)
    },
    .package = "tsbiomass"
  )

  predictions <- predict(
    selector,
    reference_anchors = new_anchor,
    reuse_admissibility = FALSE
  )

  expect_true(998L %in% captured$prepared_candidate_ids)
  expect_true(captured$gower_constructed)
  expect_equal(captured$screen_distance_mode, "empirical_gower")
  expect_equal(predictions@selections$anchor_model_id, "998")
})

test_that("PolicySelector errors for an external query without stored distance state", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(policy_perf = minimal_policy_performance()),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )
  new_anchor <- candidates@candidate_models |>
    dplyr::filter(.data$model_id == 1L) |>
    dplyr::slice(1L) |>
    dplyr::mutate(model_id = 997L, model_id_chr = "997")

  expect_error(
    predict(selector, reference_anchors = new_anchor, reuse_admissibility = FALSE),
    "stored distance mode"
  )
})

test_that("PolicySelector requires unique external query model IDs", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(policy_perf = minimal_policy_performance()),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )
  new_anchor <- candidates@candidate_models |>
    dplyr::filter(.data$model_id == 1L) |>
    dplyr::slice(1L) |>
    dplyr::mutate(model_id = 994L, model_id_chr = "994")

  expect_error(
    predict(
      selector,
      reference_anchors = dplyr::bind_rows(new_anchor, new_anchor),
      reuse_admissibility = FALSE
    ),
    "unique `model_id` values"
  )
})

test_that("full anchor screening rejects missing query equation coefficients", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  new_anchor <- candidates@candidate_models |>
    dplyr::filter(.data$model_id == 1L) |>
    dplyr::slice(1L) |>
    dplyr::mutate(
      model_id = 996L,
      model_id_chr = "996",
      slope_standard = NA_real_,
      intercept_standard = NA_real_
    )

  expect_error(
    tsbiomass:::screen_one_anchor_admissibility(
      anchor_row = new_anchor,
      candidate_models = candidates@candidate_models,
      config = minimal_config_data(),
      registry_path = trait_registry_path(),
      excluded_model_ids = "996"
    ),
    "no finite positive anchor backscatter"
  )
})

test_that("screening a novel-species query with no backscatter still finds an admissible donor pool", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  new_anchor <- candidates@candidate_models |>
    dplyr::filter(.data$model_id == 1L) |>
    dplyr::slice(1L) |>
    dplyr::mutate(
      model_id = 993L,
      model_id_chr = "993",
      slope_standard = NA_real_,
      intercept_standard = NA_real_
    )
  cfg <- minimal_config_data()
  cfg$admissibility$coherence$depth$min <- NA_real_

  eval_obj <- tsbiomass:::screen_one_anchor_admissibility(
    anchor_row = new_anchor,
    candidate_models = candidates@candidate_models,
    config = cfg,
    registry_path = trait_registry_path(),
    excluded_model_ids = "993",
    require_backscatter = FALSE
  )

  expect_true(is.na(eval_obj$anchor_sigma))
  expect_gt(nrow(eval_obj$admissible_df), 0)
  expect_true(all(is.na(eval_obj$admissible_df$biomass_multiplier_if_replace)))
})

test_that("full anchor screening rejects malformed direct query PDF data", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  new_anchor <- candidates@candidate_models |>
    dplyr::filter(.data$model_id == 1L) |>
    dplyr::slice(1L) |>
    dplyr::mutate(model_id = 995L, model_id_chr = "995")
  new_anchor$length_pdf_data <- list(tibble::tibble(
    length_cm = c(8, 9),
    f_len = c(0, 0)
  ))

  expect_error(
    tsbiomass:::screen_one_anchor_admissibility(
      anchor_row = new_anchor,
      candidate_models = candidates@candidate_models,
      config = minimal_config_data(),
      registry_path = trait_registry_path(),
      excluded_model_ids = "995"
    ),
    "positive finite support"
  )
})
