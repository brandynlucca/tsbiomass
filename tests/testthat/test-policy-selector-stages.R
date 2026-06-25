test_that("PolicySelector methods store benchmark, uncertainty, and selection state", {
  candidates <- set_reference_anchors(make_candidates(), selector = list(regional_body = "SWFSC"))
  selector <- as_policy_selector(candidates)

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
        equiv_ref = list(pairs = tibble::tibble(policy = "same_species_closest")),
        equiv_sets = tibble::tibble(policy = c("same_species_closest", "same_genus_weighted"))
      )
    },
    .package = "tsbiomass"
  )
  selector <- select_policies(selector)

  expect_true(length(selector@selection) > 0)
  expect_true("final_ref" %in% names(selector@selection))
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
  expect_true("referee_rebuild" %in% getNamespaceExports("tsbiomass"))
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
        policy = c("same_species_closest", "same_genus_weighted"),
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

  coverage <- build_species_coverage(selector)
  expect_equal(nrow(coverage), 2)
  expect_true(all(c("policy", "equation_branch_filter", "empirical_coverage") %in% names(coverage)))

  audit <- build_anchor_audit(predictions, selector = selector)
  expect_equal(nrow(audit), nrow(predictions@selections))
  expect_true(all(c("anchor_model_id", "selected_policy", "bootstrap_median_rank") %in% names(audit)))
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
        policy = c("same_species_closest", "same_genus_weighted"),
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
        policy = c("same_species_closest", "same_genus_weighted"),
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
        policy = c("same_species_closest", "same_genus_weighted"),
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
        policy = c("same_species_closest", "same_genus_weighted"),
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
        policy = c("same_species_closest", "same_genus_weighted"),
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

test_that("deterministic selection uses coefficient stability after score and width", {
  selected <- select_anchor_policies(tibble::tibble(
    policy = c("free_slope_unstable", "fixed_slope_stable"),
    equation_branch_filter = c("free_slope_only", "fixed20_only"),
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

  expect_equal(selected$selected_policy[[1]], "fixed_slope_stable")
})

test_that("recommendation overlap support summary respects configured traits", {
  rows <- tibble::tibble(
    overlap_same_species = c(TRUE, FALSE),
    overlap_same_family = c(TRUE, TRUE),
    overlap_same_swimbladder = c(FALSE, TRUE)
  )
  config <- list(
    similarity = list(
      species_traits = list(species_name = 1, swimbladder_type = 1),
      study_traits = list()
    ),
    policy = list(
      species_traits = list(species_name = 1, swimbladder_type = 1),
      study_traits = list()
    )
  )

  out <- recommendation_overlap_support_summary(rows, config = config)

  expect_equal(
    names(out),
    c("n_same_species_donors", "n_same_swimbladder_donors")
  )
  expect_equal(out$n_same_species_donors[[1]], 1L)
  expect_equal(out$n_same_swimbladder_donors[[1]], 1L)
})

test_that("Scorecard and Configurer show compact console summaries", {
  scorecard_output <- capture.output(show(empty_scorecard()))
  expect_true(any(grepl("^Scorecard$", scorecard_output)))
  expect_true(any(grepl("selected_rows:", scorecard_output, fixed = TRUE)))
  expect_false(any(grepl("@selected", scorecard_output, fixed = TRUE)))

  cfg_path <- system.file("templates", "swfscfish_config.yaml", package = "tsbiomass")
  if (!nzchar(cfg_path)) {
    cfg_path <- file.path(pkgload::pkg_path(), "inst", "templates", "swfscfish_config.yaml")
  }
  cfg <- as_configurer(read_config(
    cfg_path,
    base_dir = dirname(cfg_path)
  ))
  cfg_output <- capture.output(show(cfg))
  expect_true(any(grepl("^Configurer$", cfg_output)))
  expect_true(any(grepl("species_traits:", cfg_output, fixed = TRUE)))
  expect_false(any(grepl("@data", cfg_output, fixed = TRUE)))
})

