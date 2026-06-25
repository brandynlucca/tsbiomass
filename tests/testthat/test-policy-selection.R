make_species_performance_table <- function(lhs_errors,
                                           rhs_errors,
                                           lhs = "alpha_policy",
                                           rhs = "beta_policy") {
  stopifnot(length(lhs_errors) == length(rhs_errors))

  species_ids <- paste("Species", seq_along(lhs_errors))
  tibble::tibble(
    anchor_species = rep(species_ids, each = 2),
    policy = rep(c(lhs, rhs), times = length(species_ids)),
    equation_branch_filter = "all",
    error_abs_log = c(rbind(lhs_errors, rhs_errors)),
    valid_prediction = TRUE
  )
}

make_selection_reference <- function(lhs_errors,
                                     rhs_errors,
                                     lhs = "alpha_policy",
                                     rhs = "beta_policy") {
  tibble::tibble(
    policy = c(lhs, rhs),
    equation_branch_filter = "all",
    mean_species_median_abs_log = c(mean(lhs_errors), mean(rhs_errors)),
    specificity_rank = c(1L, 1L)
  )
}

test_that("build_equivalence_table marks full margin containment as equivalent", {
  perf_tbl <- make_species_performance_table(
    lhs_errors = c(0.10, 0.11, 0.12, 0.13),
    rhs_errors = c(0.12, 0.13, 0.14, 0.15)
  )
  select_ref <- make_selection_reference(
    lhs_errors = c(0.10, 0.11, 0.12, 0.13),
    rhs_errors = c(0.12, 0.13, 0.14, 0.15)
  )

  equiv <- build_equivalence_table(
    species_performance_table = perf_tbl,
    select_ref = select_ref,
    tolerance = 0.05,
    n_boot = 200L,
    seed = 1L
  )

  expect_equal(equiv$pairs$pair_decision[[1]], "equivalent")
  expect_true(equiv$pairs$equivalent_pair[[1]])
  expect_true(is.na(equiv$pairs$better_policy[[1]]))
  expect_true(equiv$best_flags$equivalent_to_best_global[[2]])
})

test_that("build_equivalence_table leaves boundary-overlap cases inconclusive", {
  perf_tbl <- make_species_performance_table(
    lhs_errors = rep(0.10, 6),
    rhs_errors = c(0.18, 0.18, 0.18, 0.12, 0.12, 0.12)
  )
  select_ref <- make_selection_reference(
    lhs_errors = rep(0.10, 6),
    rhs_errors = c(0.18, 0.18, 0.18, 0.12, 0.12, 0.12)
  )

  equiv <- build_equivalence_table(
    species_performance_table = perf_tbl,
    select_ref = select_ref,
    tolerance = 0.05,
    n_boot = 1000L,
    seed = 2L
  )

  expect_equal(equiv$pairs$pair_decision[[1]], "inconclusive")
  expect_false(equiv$pairs$equivalent_pair[[1]])
  expect_true(is.na(equiv$pairs$better_policy[[1]]))
  expect_false(equiv$best_flags$equivalent_to_best_global[[2]])
  expect_lt(equiv$pairs$paired_boot_q025[[1]], -0.05)
  expect_gt(equiv$pairs$paired_boot_q975[[1]], -0.05)
})

test_that("build_equivalence_table only names a better policy when the full interval clears the margin", {
  perf_tbl <- make_species_performance_table(
    lhs_errors = c(0.10, 0.11, 0.10, 0.11),
    rhs_errors = c(0.18, 0.19, 0.18, 0.19)
  )
  select_ref <- make_selection_reference(
    lhs_errors = c(0.10, 0.11, 0.10, 0.11),
    rhs_errors = c(0.18, 0.19, 0.18, 0.19)
  )

  equiv <- build_equivalence_table(
    species_performance_table = perf_tbl,
    select_ref = select_ref,
    tolerance = 0.05,
    n_boot = 200L,
    seed = 3L
  )

  expect_equal(equiv$pairs$pair_decision[[1]], "lhs_better")
  expect_false(equiv$pairs$equivalent_pair[[1]])
  expect_equal(equiv$pairs$better_policy[[1]], "alpha_policy")
})

test_that("default_anchor_config is idempotent for normalized anchor configs", {
  cfg <- tsbiomass:::default_anchor_config(minimal_config_data())
  cfg2 <- tsbiomass:::default_anchor_config(cfg)

  expect_equal(cfg2$species_traits, character(0))
  expect_equal(cfg2$study_traits, character(0))
  expect_equal(cfg2$frequency_coherence_mode, "none")
  expect_equal(cfg2$min_length_overlap_fraction, 0.25)
  expect_equal(cfg2$missing_key_metadata_max_fraction, 0.25)
})

test_that("summarize_selected_coefficient_calibration recovers coefficients from TS benchmark metadata", {
  selected_tbl <- tibble::tibble(
    anchor_model_id = c("11", "12"),
    anchor_species = c("Alpha alpha", "Beta beta"),
    policy = c("policy_alpha", "policy_beta"),
    equation_branch_filter = c("all", "fixed20_only"),
    post_selection_support_bin = c("support_bin_2", "support_bin_2")
  )
  candidate_models <- tibble::tibble(
    model_id_chr = c("11", "12"),
    species_name = c("Alpha alpha", "Beta beta"),
    family_name = c("Alphaidae", "Betidae"),
    slope_len = c(20, 22),
    intercept_len = c(-70, -72)
  )
  ts_error <- tibble::tibble(
    anchor_model_id = c("11", "11", "12", "12"),
    policy = c("policy_alpha", "policy_alpha", "policy_beta", "policy_beta"),
    equation_branch_filter = c("all", "all", "fixed20_only", "fixed20_only"),
    policy_slope_len = c(18, 18, 20, 20),
    policy_intercept_len = c(-69, -69, -68, -68),
    u = c(0, 1, 0, 1),
    ts_error = c(1, 1, 2, 2)
  )

  out <- tsbiomass:::summarize_selected_coefficient_calibration(
    selected_tbl = selected_tbl,
    candidate_models = candidate_models,
    ts_error = ts_error
  )

  expect_equal(nrow(out), 2)
  expect_equal(out$policy, c("policy_alpha", "policy_beta"))
  expect_equal(out$equation_branch_filter, c("all", "fixed20_only"))
  expect_equal(out$slope_resid, c(2, 2))
  expect_equal(out$intercept_resid, c(-1, -4))
})

test_that("select_coefficient_residual_pool stays within the selected policy and branch", {
  coefficient_calibration <- tibble::tibble(
    anchor_model_id = as.character(seq_len(5)),
    anchor_species = c("Alpha alpha", "Gamma gamma", "Delta delta", "Epsilon eps", "Zeta zeta"),
    anchor_family = rep(NA_character_, 5),
    policy = c("policy_alpha", "policy_alpha", "other_policy", "other_policy", "third_policy"),
    equation_branch_filter = c("fixed20_only", "fixed20_only", "all", "all", "all"),
    post_selection_support_bin = rep("support_bin_2", 5),
    slope_resid = c(1, 2, 3, 4, 5),
    intercept_resid = c(-1, -2, -3, -4, -5)
  )
  row_now <- tibble::tibble(
    anchor_model_id = "999",
    anchor_species = "Omega omega",
    anchor_family = NA_character_,
    selected_policy = "policy_alpha",
    selected_equation_branch_filter = "fixed20_only",
    post_selection_support_bin = "support_bin_2"
  )

  pool <- tsbiomass:::select_coefficient_residual_pool(
    coefficient_calibration = coefficient_calibration,
    row_now = row_now,
    min_rows = 4L
  )

  expect_equal(nrow(pool), 0L)
})

test_that("conditional coefficient intervals lock the slope for fixed20 policies", {
  policy_tbl <- tibble::tibble(
    anchor_model_id = "11",
    anchor_species = "Alpha alpha",
    anchor_family = "Alphaidae",
    selected_policy = "policy_alpha",
    selected_equation_branch_filter = "fixed20_only",
    policy_slope_len = 20,
    policy_intercept_len = -70,
    q_abs_log = 0.5,
    meta_q_abs_log_total = 0.5
  )
  candidate_models <- tibble::tibble(
    model_id_chr = "11",
    species_name = "Alpha alpha",
    family_name = "Alphaidae",
    slope_len = 22,
    intercept_len = -72,
    study_length_min = 10,
    study_length_max = 30
  )
  coefficient_calibration <- tibble::tibble(
    anchor_model_id = c("21", "22", "23", "24"),
    anchor_species = rep("Alpha alpha", 4),
    anchor_family = rep("Alphaidae", 4),
    policy = rep("policy_alpha", 4),
    equation_branch_filter = rep("fixed20_only", 4),
    post_selection_support_bin = rep(NA_character_, 4),
    slope_resid = c(2, 1, -1, -2),
    intercept_resid = c(-3, -2, 2, 3)
  )

  out <- tsbiomass:::augment_policy_conditional_coefficient_intervals(
    policy_tbl = policy_tbl,
    candidate_models = candidate_models,
    anchor_scores = tibble::tibble(),
    ts_calibration = tibble::tibble(),
    coefficient_calibration = coefficient_calibration,
    config = list()
  )

  expect_equal(out$conditional_policy_slope_len_lo_95, 20)
  expect_equal(out$conditional_policy_slope_len_hi_95, 20)
  expect_equal(out$conditional_policy_slope_len_se, 0)
  expect_true(is.finite(out$conditional_policy_intercept_len_lo_95))
  expect_true(is.finite(out$conditional_policy_intercept_len_hi_95))
})
