test_that("PolicySelector methods store benchmark, uncertainty, and selection state", {
  candidates <- set_reference_anchors(make_candidates(), selector = list(regional_body = "SWFSC"))
  selector <- as_policyselector(candidates)

  testthat::local_mocked_bindings(
    run_policy_benchmark = function(...) {
      list(
        policy_perf = minimal_policy_performance(),
        species_block_perf = minimal_policy_performance()
      )
    },
    .package = "tsbiomass"
  )
  selector <- benchmark(selector)
  expect_true(length(selector@benchmark) > 0)
  expect_true("policy_perf" %in% names(selector@benchmark))

  testthat::local_mocked_bindings(
    run_anchor_conformal = function(...) minimal_uncertainty(),
    .package = "tsbiomass"
  )
  selector <- calibrate_uncertainty(selector)
  expect_true(length(selector@uncertainty) > 0)
  expect_true("conf_cal" %in% names(selector@uncertainty))

  testthat::local_mocked_bindings(
    run_policy_selection = function(...) {
      list(
        final_ref = minimal_selection_ref(),
        equiv_ref = list(pairs = tibble::tibble(policy = "closest_within_species")),
        equiv_sets = tibble::tibble(policy = c("closest_within_species", "weighted_mean_within_genus"))
      )
    },
    .package = "tsbiomass"
  )
  selector <- select_policies(selector)

  expect_true(length(selector@selection) > 0)
  expect_true("final_ref" %in% names(selector@selection))
})

test_that("as_policyselector inherits candidate config when config is omitted", {
  candidates_base <- make_candidates(seed_similarity_tuning = FALSE)
  cfg_defaults <- list(
    policies = list(active = "closest_within_species"),
    selection = list(one_se_multiplier = 2)
  )
  candidates <- Candidates(
    spec = c(candidates_base@spec, list(config_data = cfg_defaults)),
    study_db = candidates_base@study_db,
    species_vector = candidates_base@species_vector,
    source_dbs = candidates_base@source_dbs,
    species_db = candidates_base@species_db,
    candidate_models = candidates_base@candidate_models,
    reference_anchors = candidates_base@reference_anchors,
    similarity_matrix = candidates_base@similarity_matrix,
    gower_distances = candidates_base@gower_distances,
    ordination = candidates_base@ordination,
    admissibility = candidates_base@admissibility,
    similarity_tuning = candidates_base@similarity_tuning
  )

  selector <- as_policyselector(candidates)

  expect_equal(selector@config$policies$active, "closest_within_species")
  expect_equal(selector@config$selection$one_se_multiplier, 2)
})

test_that("as_policyselector inherits Alchemist config when config is omitted", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  cfg_defaults <- list(
    policies = list(active = "closest_within_species"),
    selection = list(one_se_multiplier = 3)
  )
  model_ids <- as.character(candidates@candidate_models$model_id)
  n_models <- length(model_ids)
  distance_matrix <- matrix(0, nrow = n_models, ncol = n_models, dimnames = list(model_ids, model_ids))
  distance_matrix[upper.tri(distance_matrix) | lower.tri(distance_matrix)] <- 1
  alchemist <- Alchemist(
    candidates = candidates,
    config = list(config_data = cfg_defaults),
    learner = list(method = "test", feature_cols = character()),
    distance_matrix = list(
      combined_dist = distance_matrix,
      dist_matrix = distance_matrix,
      directed_dist_matrix = distance_matrix,
      learned_kernel_bandwidth = 1,
      feature_type = "difference"
    ),
    trait_importance = list(),
    ordination = list(),
    admissibility = list()
  )

  selector <- as_policyselector(alchemist)

  expect_equal(selector@config$policies$active, "closest_within_species")
  expect_equal(selector@config$selection$one_se_multiplier, 3)
})

test_that("policy selector anchor config replaces explicit trait maps", {
  candidates_base <- make_candidates(seed_similarity_tuning = FALSE)
  cfg_defaults <- list(
    similarity = list(
      species_traits = list(class = 1, family = 1, genus = 1),
      study_traits = list(fao_area = 1, frequency = 1)
    )
  )
  candidates <- Candidates(
    spec = c(candidates_base@spec, list(config_data = cfg_defaults)),
    study_db = candidates_base@study_db,
    species_vector = candidates_base@species_vector,
    source_dbs = candidates_base@source_dbs,
    species_db = candidates_base@species_db,
    candidate_models = candidates_base@candidate_models,
    reference_anchors = candidates_base@reference_anchors,
    similarity_matrix = list(),
    gower_distances = candidates_base@gower_distances,
    ordination = candidates_base@ordination,
    admissibility = candidates_base@admissibility,
    similarity_tuning = list()
  )

  selector <- make_selector(
    candidates = candidates,
    config = list(
      similarity = list(
        species_traits = list(genus = 1, swimbladder_type = 1),
        study_traits = list(equation_form = 1)
      ),
      admissibility = list(
        species_traits = "swimbladder_type"
      )
    )
  )

  anchor_cfg <- tsbiomass:::policy_selector_anchor_config(selector)

  expect_named(anchor_cfg$species_traits, c("genus", "swimbladder_type"))
  expect_named(anchor_cfg$study_traits, "equation_form")
  expect_equal(anchor_cfg$admissibility_species_traits, "swimbladder_type")
})

test_that("policy selector anchor config retains the admissibility frequency gap", {
  selector <- make_selector(
    candidates = make_candidates(seed_similarity_tuning = FALSE),
    config = list(
      policy = list(
        frequency_coherence_mode = "overlap"
      ),
      admissibility = list(
        frequency_gap = 60
      )
    )
  )

  anchor_cfg <- tsbiomass:::policy_selector_anchor_config(selector)

  expect_equal(anchor_cfg$frequency_coherence_mode, "overlap")
  expect_equal(anchor_cfg$frequency_gap, 60)
})

test_that("prediction-time admissibility profiles rescreen anchors without refitting", {
  candidates <- set_reference_anchors(make_candidates(), selector = list(regional_body = "SWFSC"))
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(policy_perf = minimal_policy_performance()),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref()),
    config = list(
      prediction = list(
        admissibility_profiles = list(
          strict_overlap = list(
            admissibility = list(
              coherence = list(
                length = list(min = 0.75),
                depth = list(min = 0.50)
              )
            )
          )
        )
      )
    )
  )
  seen_configs <- list()

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(..., config) {
      seen_configs[[length(seen_configs) + 1L]] <<- config
      list()
    },
    evaluate_policies = function(...) {
      tibble::tibble(
        policy = c("closest_within_species", "weighted_mean_within_genus"),
        multiplier_pred = c(1.10, 1.30)
      )
    },
    summarize_evaluation = function(...) {
      tibble::tibble(
        consensus_multiplier = 1.20,
        local_support_mass = 0.80,
        local_effective_support = 3.00
      )
    },
    .package = "tsbiomass"
  )

  predictions <- predict(
    selector,
    admissibility_override = "strict_overlap",
    refresh = TRUE
  )

  expect_length(seen_configs, nrow(candidates@reference_anchors))
  expect_true(all(vapply(seen_configs, function(x) identical(x$min_length_overlap_fraction, 0.75), logical(1))))
  expect_true(all(vapply(seen_configs, function(x) identical(x$min_depth_overlap_fraction, 0.50), logical(1))))
  expect_true(all(predictions@intervals$admissibility_profile == "strict_overlap"))
  expect_true(all(predictions@selections$admissibility_override_applied))
})

test_that("anchor-provided admissibility profiles resolve independently", {
  cfg <- list(
    prediction = list(
      admissibility_profiles = list(
        strict = list(min_length_overlap_fraction = 0.80)
      )
    )
  )
  anchor <- tibble::tibble(admissibility_profile = "strict")

  resolved <- tsbiomass:::policy_selector_resolve_admissibility_override(cfg, anchor)

  expect_equal(resolved$profile, "strict")
  expect_true(resolved$applied)
  expect_equal(resolved$values$min_length_overlap_fraction, 0.80)
})

test_that("inline admissibility trait overrides do not partially match nested sections", {
  resolved <- tsbiomass:::policy_selector_normalize_admissibility_override(list(
    min_length_overlap_fraction = 0.01,
    min_depth_overlap_fraction = 0.01,
    admissibility_species_traits = c("swimbladder_type", "ocean_basin")
  ))

  expect_equal(resolved$min_length_overlap_fraction, 0.01)
  expect_equal(resolved$min_depth_overlap_fraction, 0.01)
  expect_equal(
    resolved$admissibility_species_traits,
    c("swimbladder_type", "ocean_basin")
  )
})

test_that("installed-style S3 bridges are registered for base predict and plot", {
  expect_true(is.function(utils::getS3method("predict", "tsbiomass::PolicySelector", optional = TRUE)))
  expect_true(is.function(utils::getS3method("predict", "tsbiomass::PolicyLearner", optional = TRUE)))
  expect_true(is.function(utils::getS3method("predict", "tsbiomass::Referee", optional = TRUE)))
  expect_true(is.function(utils::getS3method("plot", "tsbiomass::Alchemist", optional = TRUE)))
  expect_true(is.function(utils::getS3method("plot", "tsbiomass::Candidates", optional = TRUE)))
  expect_true(is.function(utils::getS3method("plot", "tsbiomass::PolicySelector", optional = TRUE)))
  expect_true(is.function(utils::getS3method("plot", "tsbiomass::PolicyPredictions", optional = TRUE)))
  expect_true(is.function(utils::getS3method("plot", "tsbiomass::PolicyLearner", optional = TRUE)))
  expect_true(is.function(utils::getS3method("plot", "tsbiomass::Scorecard", optional = TRUE)))
  expect_true(is.function(utils::getS3method("plot", "tsbiomass::Referee", optional = TRUE)))
  expect_true(is.function(utils::getS3method("plot", "tsbiomass::PolicySimulator", optional = TRUE)))
  expect_true(is.function(utils::getS3method("plot", "tsbiomass::Conjurer", optional = TRUE)))
  expect_true("update_referee" %in% getNamespaceExports("tsbiomass"))
})

test_that("as_tibble returns canonical result tables for predictions and scorecards", {
  predictions <- PolicyPredictions(
    intervals = tibble::tibble(policy = "p1", multiplier_pred = 1.1),
    selections = tibble::tibble(
      anchor_model_id = "1",
      anchor_species = "Alpha alpha",
      selected_policy = "p1"
    ),
    consensus = tibble::tibble(anchor_model_id = "1", consensus_multiplier = 1.1)
  )
  scorecard <- empty_scorecard()
  scorecard@recommendation_cards <- tibble::tibble(
    anchor_species = "Alpha alpha",
    recommended_policy = "p1"
  )

  pred_tbl <- tibble::as_tibble(predictions)
  score_tbl <- tibble::as_tibble(scorecard)

  expect_s3_class(pred_tbl, "tbl_df")
  expect_s3_class(score_tbl, "tbl_df")
  expect_named(pred_tbl, c("anchor_model_id", "anchor_species", "selected_policy"))
  expect_named(score_tbl, c("anchor_species", "recommended_policy"))
})

test_that("predict returns PolicyPredictions and summary helpers use selector state", {
  candidates <- set_reference_anchors(make_candidates(), selector = list(regional_body = "SWFSC"))
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(policy_perf = minimal_policy_performance()),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) list(),
    evaluate_policies = function(...) {
      tibble::tibble(
        policy = c("closest_within_species", "weighted_mean_within_genus"),
        multiplier_pred = c(1.10, 1.30)
      )
    },
    summarize_evaluation = function(...) {
      tibble::tibble(
        consensus_multiplier = 1.20,
        local_support_mass = 0.80,
        local_effective_support = 3.00
      )
    },
    .package = "tsbiomass"
  )

  predictions <- predict(selector)

  expect_true(S7::S7_inherits(predictions, PolicyPredictions))
  expect_equal(nrow(predictions@selections), nrow(selector@candidates@reference_anchors))
  expect_true(all(c("multiplier_lo", "multiplier_hi", "selected_policy") %in% names(predictions@selections)))
  expect_false(any(grepl("__", predictions@intervals$policy, fixed = TRUE)))
  expect_true("equation_branch_filter" %in% names(predictions@intervals))

  coverage <- construct_species_coverage(selector)
  expect_equal(nrow(coverage), 2)
  expect_true(all(c("policy", "equation_branch_filter", "empirical_coverage") %in% names(coverage)))

  audit <- construct_anchor_audit(predictions, selector = selector)
  expect_equal(nrow(audit), nrow(predictions@selections))
  expect_true(all(c("anchor_model_id", "selected_policy", "bootstrap_median_rank") %in% names(audit)))
})

test_that("PolicySelector prediction bundles are cached and reused", {
  cache_base <- tempfile(fileext = ".rds")
  candidates <- set_reference_anchors(make_candidates(), selector = list(regional_body = "SWFSC"))
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(policy_perf = minimal_policy_performance()),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref()),
    config = list(
      selection = list(
        prediction_cache_path = cache_base,
        refresh = FALSE
      ),
      policies = list(
        active = c("closest_within_species", "weighted_mean_within_genus")
      )
    )
  )

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) list(),
    evaluate_policies = function(...) {
      tibble::tibble(
        policy = c("closest_within_species", "weighted_mean_within_genus"),
        multiplier_pred = c(1.10, 1.30)
      )
    },
    summarize_evaluation = function(...) {
      tibble::tibble(
        consensus_multiplier = 1.20,
        local_support_mass = 0.80,
        local_effective_support = 3.00
      )
    },
    .package = "tsbiomass"
  )
  first <- predict(selector)
  selector_cache <- tsbiomass:::policy_selector_prediction_cache_path(
    cache_base,
    anchor_ids = candidates@reference_anchors$model_id
  )
  expect_true(tsb_cache_exists(selector_cache))

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) {
      stop("cached prediction should not rescreen anchors")
    },
    evaluate_policies = function(...) {
      stop("cached prediction should not reevaluate policies")
    },
    summarize_evaluation = function(...) {
      stop("cached prediction should not resummarize anchor evaluations")
    },
    .package = "tsbiomass"
  )
  second <- predict(selector)

  expect_true(S7::S7_inherits(second, PolicyPredictions))
  expect_equal(second@selections, first@selections)
  expect_equal(second@intervals, first@intervals)
  expect_equal(second@consensus, first@consensus)
})

test_that("cached anchor admissibility is returned even when config is supplied", {
  candidates <- set_reference_anchors(make_candidates(), selector = list(regional_body = "SWFSC"))
  anchor_ids <- as.character(candidates@reference_anchors$model_id_chr)
  cached_admissibility <- list(
    anchors = stats::setNames(
      lapply(anchor_ids, function(anchor_id) {
        list(evaluation = list(anchor_id = anchor_id, cached = TRUE))
      }),
      anchor_ids
    )
  )
  candidates <- candidates_with_admissibility(candidates, cached_admissibility)
  selector <- make_selector(candidates = candidates)

  anchor_row <- selector@candidates@reference_anchors[1, , drop = FALSE]
  cached_eval <- policy_selector_cached_anchor_evaluation(
    object = selector,
    anchor_row = anchor_row,
    config_supplied = TRUE
  )

  expect_true(is.list(cached_eval))
  expect_true(isTRUE(cached_eval$cached))
  expect_equal(cached_eval$anchor_id, as.character(anchor_row$model_id_chr[[1]]))
})

test_that("cached unscorable anchors become explicit invalid prediction rows", {
  candidates <- set_reference_anchors(
    make_candidates(),
    selector = list(regional_body = "SWFSC")
  )
  failed_anchor <- candidates@reference_anchors[1, , drop = FALSE]
  failed_id <- as.character(failed_anchor$model_id[[1]])
  failed_species <- as.character(failed_anchor$species_name[[1]])
  cached_admissibility <- list(
    anchors = list(),
    anchor_failures = tibble::tibble(
      anchor_model_id = failed_id,
      anchor_species = failed_species,
      failure_stage = "anchor_density",
      failure_code = "missing_study_length_support",
      failure_message = "No valid anchor study length interval or midpoint was available."
    )
  )
  candidates <- candidates_with_admissibility(candidates, cached_admissibility)
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(policy_perf = minimal_policy_performance()),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) {
      stop("Only the uncached anchors should be screened in this test.")
    },
    .package = "tsbiomass"
  )

  predictions <- predict(selector)
  failed_prediction <- predictions@selections |>
    dplyr::filter(.data$anchor_model_id == failed_id)
  audit <- construct_anchor_audit(predictions, selector = selector)
  failed_audit <- audit |>
    dplyr::filter(.data$anchor_model_id == failed_id)

  expect_equal(nrow(failed_prediction), 1L)
  expect_false(failed_prediction$valid_prediction)
  expect_equal(failed_prediction$prediction_error_stage, "anchor_density")
  expect_equal(failed_prediction$prediction_error_code, "missing_study_length_support")
  expect_match(failed_prediction$prediction_error_message, "No valid anchor study length interval")
  expect_equal(failed_audit$prediction_error_code, "missing_study_length_support")
  expect_match(failed_audit$prediction_error_message, "No valid anchor study length interval")
})

test_that("predict reuses cached anchor admissibility instead of rescreening donors", {
  candidates <- set_reference_anchors(make_candidates(), selector = list(regional_body = "SWFSC"))
  anchor_ids <- as.character(candidates@reference_anchors$model_id_chr)
  cached_admissibility <- list(
    anchors = stats::setNames(
      lapply(anchor_ids, function(anchor_id) {
        list(evaluation = list(anchor_id = anchor_id, cached = TRUE))
      }),
      anchor_ids
    )
  )
  candidates <- candidates_with_admissibility(candidates, cached_admissibility)
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(policy_perf = minimal_policy_performance()),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) {
      stop("predict() should reuse cached anchor admissibility here")
    },
    evaluate_policies = function(eval_obj, ...) {
      tibble::tibble(
        policy = c("closest_within_species", "weighted_mean_within_genus"),
        equation_branch_filter = "all",
        multiplier_pred = c(1.10, 1.30),
        cached_anchor_id = eval_obj$anchor_id %||% NA_character_
      )
    },
    summarize_evaluation = function(eval_obj, ...) {
      tibble::tibble(
        consensus_multiplier = 1.20,
        local_support_mass = 0.80,
        local_effective_support = 3.00,
        cached_anchor_id = eval_obj$anchor_id %||% NA_character_
      )
    },
    .package = "tsbiomass"
  )

  predictions <- predict(selector)

  expect_true(S7::S7_inherits(predictions, PolicyPredictions))
  expect_false(any(is.na(predictions@intervals$cached_anchor_id)))
  expect_setequal(
    unique(predictions@intervals$cached_anchor_id),
    anchor_ids
  )
  expect_setequal(
    unique(predictions@consensus$cached_anchor_id),
    anchor_ids
  )
})

test_that("predict with config overrides still reuses cached anchor admissibility", {
  candidates <- set_reference_anchors(make_candidates(), selector = list(regional_body = "SWFSC"))
  anchor_ids <- as.character(candidates@reference_anchors$model_id_chr)
  cached_admissibility <- list(
    anchors = stats::setNames(
      lapply(anchor_ids, function(anchor_id) {
        list(evaluation = list(anchor_id = anchor_id, cached = TRUE))
      }),
      anchor_ids
    )
  )
  candidates <- candidates_with_admissibility(candidates, cached_admissibility)
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(policy_perf = minimal_policy_performance()),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )

  cfg_override <- list(
    selection = list(
      uncertainty_rule = "one_se"
    )
  )

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) {
      stop("predict() should not rescreen donors when only selection config changes")
    },
    evaluate_policies = function(eval_obj, ...) {
      tibble::tibble(
        policy = c("closest_within_species", "weighted_mean_within_genus"),
        equation_branch_filter = "all",
        multiplier_pred = c(1.10, 1.30),
        cached_anchor_id = eval_obj$anchor_id %||% NA_character_
      )
    },
    summarize_evaluation = function(eval_obj, ...) {
      tibble::tibble(
        consensus_multiplier = 1.20,
        local_support_mass = 0.80,
        local_effective_support = 3.00,
        cached_anchor_id = eval_obj$anchor_id %||% NA_character_
      )
    },
    .package = "tsbiomass"
  )

  predictions <- predict(selector, config = cfg_override)

  expect_true(S7::S7_inherits(predictions, PolicyPredictions))
  expect_false(any(is.na(predictions@intervals$cached_anchor_id)))
  expect_setequal(unique(predictions@intervals$cached_anchor_id), anchor_ids)
})

test_that("Referee consumes PolicyPredictions after selector prediction", {
  candidates <- set_reference_anchors(
    make_candidates(admissibility = list(all_scores = minimal_admissibility_scores())),
    selector = list(regional_body = "SWFSC")
  )
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(
      policy_perf = minimal_policy_performance(),
      species_block_perf = minimal_policy_performance()
    ),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) list(),
    evaluate_policies = function(...) {
      tibble::tibble(
        policy = c("closest_within_species", "weighted_mean_within_genus"),
        equation_branch_filter = "all",
        multiplier_pred = c(1.10, 1.30)
      )
    },
    summarize_evaluation = function(...) {
      tibble::tibble(
        consensus_multiplier = 1.20,
        multiplier_q05 = 1.00,
        multiplier_q50 = 1.20,
        multiplier_q95 = 1.40,
        local_support_mass = 0.80,
        local_effective_support = 3.00
      )
    },
    .package = "tsbiomass"
  )

  predictions <- predict(selector)
  predictions <- PolicyPredictions(
    intervals = predictions@intervals,
    selections = dplyr::select(predictions@selections, -dplyr::any_of("policy")),
    consensus = predictions@consensus
  )
  referee <- as_referee(selector, predictions = predictions)
  scorecard <- predict(referee)

  expect_true(S7::S7_inherits(referee, Referee))
  expect_true(S7::S7_inherits(referee@predictions, PolicyPredictions))
  expect_true(S7::S7_inherits(scorecard, Scorecard))
  expect_true(all(c(
    "anchor_summary",
    "anchor_audit",
    "species_coverage",
    "selection_diagnostics",
    "recommendation_cards",
    "surrogate_rules",
    "key_missing_overall",
    "key_missing_by_field",
    "key_missing_by_model",
    "anchor_missing_gate",
    "status"
  ) %in% names(S7::props(scorecard))))
  expect_equal(
    nrow(scorecard@selected),
    nrow(predictions@selections)
  )
  expect_equal(nrow(scorecard@recommendation_cards), nrow(predictions@selections))
  expect_true(is.data.frame(scorecard@surrogate_rules))
  expect_true(all(c(
    "uncertainty_source",
    "uncertainty_fallback",
    "uncertainty_warning",
    "uncertainty_conformal_factor",
    "uncertainty_bin_q_log"
  ) %in% names(scorecard@recommendation_cards)))
  expect_true("combined_consensus_multiplier" %in% names(scorecard@anchor_summary))
  expect_true(all(scorecard@status$status == "ok"))
})

test_that("Referee enforces prediction provenance against selector anchors", {
  candidates <- set_reference_anchors(
    make_candidates(admissibility = list(all_scores = minimal_admissibility_scores())),
    selector = list(regional_body = "SWFSC")
  )
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(
      policy_perf = minimal_policy_performance(),
      species_block_perf = minimal_policy_performance()
    ),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) list(),
    evaluate_policies = function(...) {
      tibble::tibble(
        policy = c("closest_within_species", "weighted_mean_within_genus"),
        equation_branch_filter = "all",
        multiplier_pred = c(1.10, 1.30)
      )
    },
    summarize_evaluation = function(...) {
      tibble::tibble(
        consensus_multiplier = 1.20,
        multiplier_q05 = 1.00,
        multiplier_q50 = 1.20,
        multiplier_q95 = 1.40,
        local_support_mass = 0.80,
        local_effective_support = 3.00
      )
    },
    .package = "tsbiomass"
  )

  predictions <- predict(selector)
  bad_predictions <- PolicyPredictions(
    intervals = predictions@intervals,
    selections = dplyr::mutate(predictions@selections, anchor_model_id = "bad-anchor"),
    consensus = predictions@consensus
  )

  referee <- as_referee(selector, predictions = bad_predictions)
  expect_error(
    predict(referee),
    "does not match the selector's reference-anchor ids"
  )
})

test_that("Referee only permits empty-fallback components when partial output is explicit", {
  candidates <- set_reference_anchors(
    make_candidates(admissibility = list(all_scores = minimal_admissibility_scores())),
    selector = list(regional_body = "SWFSC")
  )
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(
      policy_perf = minimal_policy_performance(),
      species_block_perf = minimal_policy_performance()
    ),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) list(),
    evaluate_policies = function(...) {
      tibble::tibble(
        policy = c("closest_within_species", "weighted_mean_within_genus"),
        equation_branch_filter = "all",
        multiplier_pred = c(1.10, 1.30)
      )
    },
    summarize_evaluation = function(...) {
      tibble::tibble(
        consensus_multiplier = 1.20,
        multiplier_q05 = 1.00,
        multiplier_q50 = 1.20,
        multiplier_q95 = 1.40,
        local_support_mass = 0.80,
        local_effective_support = 3.00
      )
    },
    summarize_missing_gate = function(...) {
      stop("forced failure")
    },
    .package = "tsbiomass"
  )

  predictions <- predict(selector)
  referee <- as_referee(selector, predictions = predictions)

  expect_error(
    predict(referee),
    "Referee component 'anchor_missing_gate' failed"
  )

  expect_warning(
    scorecard_partial <- predict(referee, allow_partial = TRUE),
    "Referee component 'anchor_missing_gate' failed"
  )
  expect_true(any(scorecard_partial@status$status == "partial_failure"))
})

test_that("summarize_missing_gate dispatches through Candidates and selectors", {
  candidates <- make_candidates(admissibility = list(all_scores = minimal_admissibility_scores()))
  selector <- make_selector(candidates = candidates)

  candidate_gate <- summarize_missing_gate(candidates)
  selector_gate <- summarize_missing_gate(selector)

  expect_equal(candidate_gate, selector_gate)
  expect_true(all(c("anchor_model_id", "n_candidates_admissible") %in% names(candidate_gate)))
})

test_that("deterministic selection prefers empirical benchmark score before width", {
  selected <- select_anchor_policies(tibble::tibble(
    policy = c("narrow_but_worse", "slightly_wider_but_better"),
    multiplier_pred = c(1.10, 1.15),
    valid_prediction = c(TRUE, TRUE),
    selection_valid = c(TRUE, TRUE),
    uncertainty_eligible = c(TRUE, TRUE),
    uncertainty_cost_log_width = c(0.10, 0.12),
    mean_species_median_abs_log = c(0.20, 0.08),
    local_weighted_mean_combined_distance = c(0.05, 0.06),
    local_effective_support = c(3, 3),
    acceptable_global = c(TRUE, TRUE),
    equivalent_to_best_global = c(FALSE, TRUE)
  ))

  expect_equal(selected$selected_policy[[1]], "slightly_wider_but_better")
  expect_true(grepl("^benchmark_screened_", selected$selection_tier[[1]]))
})

test_that("deterministic selection does not use coefficient diagnostics as tie breakers", {
  selected <- select_anchor_policies(tibble::tibble(
    policy = c("a_higher_coefficient_width", "z_lower_coefficient_width"),
    equation_branch_filter = c("all", "all"),
    multiplier_pred = c(1.10, 1.12),
    valid_prediction = c(TRUE, TRUE),
    selection_valid = c(TRUE, TRUE),
    uncertainty_eligible = c(TRUE, TRUE),
    uncertainty_cost_log_width = c(0.20, 0.20),
    mean_species_median_abs_log = c(0.08, 0.08),
    local_weighted_mean_combined_distance = c(0.05, 0.05),
    local_effective_support = c(3, 3),
    acceptable_global = c(TRUE, TRUE),
    equivalent_to_best_global = c(TRUE, TRUE),
    coefficient_slope_q95 = c(18, 4),
    coefficient_intercept_q95 = c(20, 6)
  ))

  expect_equal(selected$selected_policy[[1]], "a_higher_coefficient_width")
})

test_that("selection ranks predicted score before uncertainty width", {
  selected <- select_anchor_policies(tibble::tibble(
    policy = c("better_score_wider", "worse_score_narrower"),
    candidate_pool = c("same_family", "same_family"),
    aggregation_method = c("nearest_by_combined_distance", "nearest_by_combined_distance"),
    policy_family = c("single_model", "single_model"),
    multiplier_pred = c(1.10, 1.12),
    valid_prediction = c(TRUE, TRUE),
    selection_valid = c(TRUE, TRUE),
    uncertainty_eligible = c(TRUE, TRUE),
    uncertainty_cost_log_width = c(0.20, 0.18),
    mean_species_median_abs_log = c(0.08, 0.08),
    local_weighted_mean_combined_distance = c(0.10, 0.10),
    local_min_combined_distance = c(0.10, 0.10),
    local_effective_support = c(1, 1),
    acceptable_global = c(TRUE, TRUE),
    equivalent_to_best_global = c(TRUE, TRUE),
    .meta_predicted_score = c(0.05, 0.06),
    best_mean_species_median_abs_log = c(0.05, 0.05),
    one_se_threshold = c(0.30, 0.30)
  ))

  expect_equal(selected$selected_policy[[1]], "better_score_wider")
  expect_match(selected$selection_tier[[1]], "score_band_burden")
  expect_equal(selected$anchor_selection_min_uncertainty_width[[1]], 0.18)
})

test_that("selection screens score-equivalent policies by biomass uncertainty before burden", {
  selected <- select_anchor_policies(tibble::tibble(
    policy = c("lower_burden_wider", "higher_burden_narrower"),
    multiplier_pred = c(1.10, 1.12),
    valid_prediction = c(TRUE, TRUE),
    selection_valid = c(TRUE, TRUE),
    uncertainty_eligible = c(TRUE, TRUE),
    uncertainty_cost_log_width = c(0.40, 0.05),
    mean_species_median_abs_log = c(0.08, 0.08),
    local_structural_q_abs_log = c(0.00, 0.40),
    local_weighted_q90_combined_distance = c(0.01, 0.30),
    local_weighted_mean_combined_distance = c(0.01, 0.30),
    acceptable_global = c(TRUE, TRUE),
    .meta_predicted_score = c(0.05, 0.06),
    bootstrap_median_rank = c(2, 1)
  ), score_tol_abs = 0.10)

  expect_equal(selected$selected_policy[[1]], "higher_burden_narrower")
  expect_match(selected$selection_tier[[1]], "uncertainty_tolerance_score_band_burden")
  expect_equal(selected$anchor_selection_min_uncertainty_width[[1]], 0.05)
  expect_equal(selected$anchor_selection_uncertainty_threshold[[1]], 0.10)
})

test_that("target selection does not treat across-policy score spread as a one-SE band", {
  selected <- select_anchor_policies(tibble::tibble(
    policy = c("point_best", "lower_burden_competitor"),
    equation_branch_filter = "all",
    multiplier_pred = 1,
    valid_prediction = TRUE,
    selection_valid = TRUE,
    acceptable_global = TRUE,
    .meta_predicted_score = c(0.40, 0.405),
    local_weighted_q90_combined_distance = c(1.1, 0.8),
    local_structural_q_abs_log = c(0.7, 0),
    local_weighted_mean_combined_distance = c(1.0, 0.8),
    local_effective_species_support = c(3, 1)
  ))

  expect_equal(selected$selected_policy[[1]], "point_best")
  expect_match(selected$selection_tier[[1]], "score_band_burden")
})

test_that("target selection uses benchmark one-SE slack for score equivalence", {
  selected <- select_anchor_policies(tibble::tibble(
    policy = c("broad_mean", "specific_single", "outside_one_se"),
    equation_branch_filter = "all",
    candidate_pool = c("same_genus", "same_genus", "same_genus"),
    aggregation_method = c(
      "arithmetic_mean",
      "nearest_by_combined_distance",
      "nearest_by_combined_distance"
    ),
    policy_family = c("ensemble", "single_model", "single_model"),
    multiplier_pred = 1,
    valid_prediction = TRUE,
    selection_valid = TRUE,
    .meta_predicted_score = c(0.40, 0.49, 0.56),
    one_se_threshold = c(0.50, 0.50, 0.50),
    best_mean_species_median_abs_log = c(0.40, 0.40, 0.40),
    local_structural_q_abs_log = c(1.2, 0, 0),
    local_weighted_q90_combined_distance = c(1.1, 0.8, 0.7),
    local_weighted_mean_combined_distance = c(1.0, 0.8, 0.7),
    local_effective_support = c(8, 1, 1)
  ))

  expect_equal(selected$selected_policy[[1]], "specific_single")
  expect_equal(selected$anchor_selection_validation_threshold[[1]], 0.50)
})

test_that("burden ordering penalizes structural spread and effective support", {
  selected <- select_anchor_policies(tibble::tibble(
    policy = c("unweighted_mean_within_genus", "closest_within_genus"),
    equation_branch_filter = "fixed20_only",
    candidate_pool = c("same_genus", "same_genus"),
    aggregation_method = c("arithmetic_mean", "nearest_by_combined_distance"),
    policy_family = c("ensemble", "single_model"),
    multiplier_pred = 1,
    valid_prediction = TRUE,
    selection_valid = TRUE,
    .meta_predicted_score = c(0.40, 0.45),
    local_structural_q_abs_log = c(0.5, 0),
    local_weighted_q90_combined_distance = c(0.7, 0.7),
    local_weighted_mean_combined_distance = c(0.7, 0.7),
    local_effective_support = c(8, 1)
  ), score_tol_abs = 0.10)

  expect_equal(selected$selected_policy[[1]], "closest_within_genus")
  expect_equal(selected$anchor_selection_burden_disagreement[[1]], 0)
})

test_that("aggregation parsimony does not override lower realized donor burden", {
  selected <- select_anchor_policies(tibble::tibble(
    policy = c("unweighted_mean_within_genus", "closest_within_genus"),
    equation_branch_filter = "fixed20_only",
    candidate_pool = c("same_genus", "same_genus"),
    aggregation_method = c("arithmetic_mean", "nearest_by_combined_distance"),
    policy_family = c("ensemble", "single_model"),
    multiplier_pred = 1,
    valid_prediction = TRUE,
    selection_valid = TRUE,
    .meta_predicted_score = c(0.40, 0.45),
    local_structural_q_abs_log = c(0.01, 0.30),
    local_weighted_q90_combined_distance = c(0.40, 0.70),
    local_weighted_mean_combined_distance = c(0.35, 0.70),
    local_effective_support = c(4, 1)
  ), score_tol_abs = 0.10)

  expect_equal(selected$selected_policy[[1]], "unweighted_mean_within_genus")
  expect_equal(selected$anchor_selection_burden_disagreement[[1]], 0.01)
})

test_that("singleton ensembles are not eligible for selection", {
  selected <- select_anchor_policies(tibble::tibble(
    policy = c("unweighted_mean_within_study_cell", "closest_within_study_cell"),
    equation_branch_filter = "fixed20_only",
    candidate_pool = c("same_study_cell", "same_study_cell"),
    aggregation_method = c("arithmetic_mean", "nearest_by_combined_distance"),
    policy_family = c("unweighted_ensemble", "single_model"),
    multiplier_pred = 1,
    valid_prediction = TRUE,
    selection_valid = TRUE,
    .meta_predicted_score = c(0.10, 0.30),
    realized_n_unique_donors = c(1, 1),
    realized_donor_fingerprint = c("575", "575"),
    local_structural_q_abs_log = c(0, 0),
    local_weighted_q90_combined_distance = c(0.1, 0.1),
    local_weighted_mean_combined_distance = c(0.1, 0.1),
    local_effective_support = c(1, 1)
  ), score_tol_abs = 0.25)

  expect_equal(selected$selected_policy[[1]], "closest_within_study_cell")
  expect_false(isTRUE(selected$selection_invalid_singleton_ensemble[[1]]))
})

test_that("policy intervals use the calibrated radius once", {
  intervals <- tsbiomass:::add_policy_intervals(tibble::tibble(
    multiplier_pred = 2,
    q_abs_log = 0.2,
    local_structural_q_abs_log = 0.9,
    valid_prediction = TRUE
  ))

  expect_equal(intervals$q_abs_log_conformal, 0.2)
  expect_equal(intervals$q_abs_log_structural, 0.9)
  expect_equal(intervals$q_abs_log_total, 0.2)
  expect_equal(intervals$multiplier_lo, 2 * exp(-0.2))
  expect_equal(intervals$multiplier_hi, 2 * exp(0.2))
})

test_that("coefficient and TS scalars ignore a stale displayed-width field", {
  scalars <- strategy_q_scalars(tibble::tibble(
    multiplier_pred = 2,
    q_abs_log = 0.2,
    interval_log_width = 9
  ))

  expect_equal(scalars$q95, 0.2)
})

test_that("ordination context accepts canonical and legacy score fields", {
  scores <- tibble::tibble(model_id = "m1", nmds_cluster_id = "cluster_1")

  expect_equal(
    policy_selector_ordination_context(list(model = list(model_scores = scores)))$model_scores,
    scores
  )
  expect_equal(
    policy_selector_ordination_context(list(model = list(scores = scores)))$model_scores,
    scores
  )
})

test_that("write_scorecard reports and reads selected donor model details", {
  scorecard <- empty_scorecard()
  scorecard@selected <- tibble::tibble(
    anchor_species = "Anchor species",
    selected_policy_display = "Selected policy",
    selected_equation_branch_filter = "all",
    selection_tier = "test",
    realized_donor_fingerprint = "d1|d2",
    realized_n_unique_donors = 2,
    selected_realized_transfer_display = "2 donors realized from Selected policy [all]",
    selected_donor_model_ids = "d1|d2",
    selected_donor_model_summary = "n=2; effective_n=1.8; slope_sd=1.4",
    selected_donor_model_details = paste(
      "d1 [Species one; slope=20; intercept=-70]",
      "d2 [Species two; slope=22; intercept=-72]",
      sep = " | "
    ),
    policy_slope_len = 21,
    policy_intercept_len = -71,
    valid_prediction = TRUE,
    multiplier_pred = 1.2,
    multiplier_lo = 0.9,
    multiplier_hi = 1.5,
    q_abs_log_total = 0.2
  )

  path <- tempfile(fileext = ".md")
  write_scorecard(scorecard, path = path)
  lines <- readLines(path, warn = FALSE)
  expect_true(any(grepl("^- Realized transfer: 2 donors realized from Selected policy \\[all\\]$", lines)))
  expect_true(any(grepl("^- Donor model IDs: d1, d2$", lines)))
  expect_true(any(grepl("^- Donor summary: n=2; effective_n=1.8; slope_sd=1.4$", lines)))
  expect_true(any(grepl("slope=20; intercept=-70", lines, fixed = TRUE)))

  parsed <- read_scorecard(path)
  expect_equal(parsed$selected_realized_transfer_display[[1]], "2 donors realized from Selected policy [all]")
  expect_equal(parsed$selected_donor_model_ids[[1]], "d1|d2")
  expect_equal(parsed$selected_donor_model_summary[[1]], "n=2; effective_n=1.8; slope_sd=1.4")
  expect_match(parsed$selected_donor_model_details[[1]], "d2 \\[Species two")
})

test_that("Scorecard and Configurer show compact console summaries", {
  scorecard_output <- capture.output(show(empty_scorecard()))
  expect_true(any(grepl("^Scorecard$", scorecard_output)))
  expect_true(any(grepl("selected_rows:", scorecard_output, fixed = TRUE)))
  expect_false(any(grepl("@selected", scorecard_output, fixed = TRUE)))

  cfg_path <- tempfile(fileext = ".yaml")
  write_config_yaml(cfg_path, overwrite = TRUE)
  cfg <- build_configurer(read_configuration(cfg_path, base_dir = dirname(cfg_path)))
  cfg_output <- capture.output(show(cfg))
  expect_true(any(grepl("^Configurer$", cfg_output)))
  expect_true(any(grepl("species_traits:", cfg_output, fixed = TRUE)))
  expect_false(any(grepl("@data", cfg_output, fixed = TRUE)))
})
