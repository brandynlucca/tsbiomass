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

test_that("species summaries exclude generalized equation placeholders", {
  perf_tbl <- tibble::tibble(
    anchor_species = c("NA NA", "Gadus morhua", "Clupea harengus"),
    policy = "test_policy",
    equation_branch_filter = "all",
    error_abs_log = c(0.001, 0.2, 0.3),
    valid_prediction = TRUE
  )

  summarized <- species_performance(perf_tbl)

  expect_setequal(
    summarized$anchor_species,
    c("Gadus morhua", "Clupea harengus")
  )
})

test_that("one-SE selection threshold ignores policies with undefined SE", {
  perf_tbl <- tibble::tibble(
    anchor_species = c("Species one", "Species one", "Species two"),
    policy = c("single_species_policy", "estimable_policy", "estimable_policy"),
    equation_branch_filter = "all",
    error_abs_log = c(0.10, 0.50, 0.70),
    valid_prediction = TRUE
  )

  select_ref <- build_selection_table(
    species_performance_table = perf_tbl,
    one_se_multiplier = 1,
    n_boot = 50L,
    seed = 99L
  )

  single_row <- select_ref |>
    dplyr::filter(.data$policy == "single_species_policy")
  estimable_row <- select_ref |>
    dplyr::filter(.data$policy == "estimable_policy")

  expect_equal(
    unique(select_ref$best_policy_global),
    "estimable_policy"
  )
  expect_true(is.na(single_row$se_species_median_abs_log[[1]]))
  expect_false(single_row$acceptable_one_se[[1]])
  expect_true(estimable_row$acceptable_one_se[[1]])
  expect_gt(select_ref$one_se_threshold[[1]], select_ref$best_mean_species_median_abs_log[[1]])
})

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

test_that("compiled pairwise bootstrap matches the R oracle exactly", {
  policies <- c("alpha_policy", "beta_policy", "gamma_policy", "delta_policy")
  species <- paste("Species", seq_len(8))
  error_matrix <- cbind(
    alpha_policy = c(.10, .11, .12, .13, .12, .11, .10, .14),
    beta_policy = c(.12, .13, .14, .15, .13, .12, .11, .16),
    gamma_policy = c(.20, .18, .19, .21, .20, .17, .22, .18),
    delta_policy = c(.09, .12, .11, .14, .13, .10, .12, .15)
  )
  perf_tbl <- tidyr::expand_grid(
    anchor_species = species,
    policy = policies
  ) |>
    dplyr::mutate(
      equation_branch_filter = "all",
      error_abs_log = error_matrix[cbind(
        match(.data$anchor_species, species),
        match(.data$policy, policies)
      )],
      valid_prediction = !(.data$anchor_species == "Species 8" & .data$policy == "gamma_policy")
    )
  select_ref <- tibble::tibble(
    policy = policies,
    equation_branch_filter = "all",
    mean_species_median_abs_log = colMeans(error_matrix),
    specificity_rank = seq_along(policies)
  )

  r_result <- build_equivalence_table(
    species_performance_table = perf_tbl,
    select_ref = select_ref,
    tolerance = 0.05,
    n_boot = 250L,
    seed = 1979L,
    engine = "r"
  )
  cpp_result <- build_equivalence_table(
    species_performance_table = perf_tbl,
    select_ref = select_ref,
    tolerance = 0.05,
    n_boot = 250L,
    seed = 1979L,
    engine = "cpp"
  )

  pair_numeric_cols <- names(cpp_result$pairs)[vapply(cpp_result$pairs, is.numeric, logical(1))]
  pair_other_cols <- setdiff(names(cpp_result$pairs), pair_numeric_cols)
  expect_equal(cpp_result$pairs[pair_other_cols], r_result$pairs[pair_other_cols])
  for (col in pair_numeric_cols) {
    expect_equal(cpp_result$pairs[[col]], r_result$pairs[[col]], tolerance = 1e-10)
  }

  flag_numeric_cols <- names(cpp_result$best_flags)[vapply(cpp_result$best_flags, is.numeric, logical(1))]
  flag_other_cols <- setdiff(names(cpp_result$best_flags), flag_numeric_cols)
  expect_equal(cpp_result$best_flags[flag_other_cols], r_result$best_flags[flag_other_cols])
  for (col in flag_numeric_cols) {
    expect_equal(cpp_result$best_flags[[col]], r_result$best_flags[[col]], tolerance = 1e-10)
  }
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

test_that("summarize_coeff_calibration recovers coefficients from TS benchmark metadata", {
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

  out <- tsbiomass:::summarize_coeff_calibration(
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

test_that("strategy uncertainty uses the policy-conditioned geometry when residual covariance is unavailable", {
  row_now <- tibble::tibble(
    anchor_model_id = "a1",
    anchor_species = "Alpha alpha",
    selected_policy = "selected_policy",
    selected_equation_branch_filter = "all",
    policy_slope_len = 20,
    policy_intercept_len = -70,
    meta_q_abs_log_total = 0.1,
    realized_donor_fingerprint = "d1|d2"
  )
  candidate_models <- tibble::tibble(
    model_id = "a1",
    species_name = "Alpha alpha",
    genus = "Alpha",
    family_name = "Alphaidae",
    frequency_khz = 38,
    slope_len = 20,
    intercept_len = -70,
    study_length_min = 10,
    study_length_max = 30
  )
  anchor_scores <- tibble::tibble(
    anchor_model_id = rep("a1", 3),
    model_id = c("d1", "d2", "d3"),
    slope_len = c(18, 22, 40),
    intercept_len = c(-68, -72, -95),
    combined_distance = c(0.1, 0.2, 0.3),
    w_adm = c(1, 1, 1),
    study_length_min = c(10, 10, 10),
    study_length_max = c(30, 30, 30)
  )

  ctx <- tsbiomass:::strategy_uncertainty_context(
    row_now = row_now,
    candidate_models = candidate_models,
    anchor_scores = anchor_scores,
    policy_tbl = tibble::tibble(),
    ts_calibration = tibble::tibble(),
    coefficient_calibration = tibble::tibble(),
    config = list(),
    policy_lookup = list(),
    include_competition = FALSE,
    length_grid_n = 5L
  )

  expect_true(is.na(ctx$coefficient_support_n))
  expect_true(all(is.finite(ctx$coefficient_covariance)))
  expect_identical(ctx$coefficient_covariance_source, "policy_conditional_ts_geometry")
})

test_that("empirical coefficient covariance is rescaled to the selected biomass radius", {
  candidate_models <- tibble::tibble(
    model_id = "a1",
    species_name = "Alpha alpha",
    genus = "Alpha",
    family_name = "Alphaidae",
    frequency_khz = 38,
    slope_len = 20,
    intercept_len = -70,
    study_length_min = 10,
    study_length_max = 30
  )
  coefficient_calibration <- tibble::tibble(
    anchor_model_id = c("c1", "c2", "c3", "c4"),
    anchor_species = rep("Beta beta", 4),
    policy = rep("selected_policy", 4),
    equation_branch_filter = rep("all", 4),
    post_selection_support_bin = rep(NA_character_, 4),
    slope_resid = c(-1, 0, 1, 2),
    intercept_resid = c(-2, -1, 1, 2)
  )
  make_ctx <- function(q) {
    tsbiomass:::strategy_uncertainty_context(
      row_now = tibble::tibble(
        anchor_model_id = "a1",
        anchor_species = "Alpha alpha",
        selected_policy = "selected_policy",
        selected_equation_branch_filter = "all",
        policy_slope_len = 20,
        policy_intercept_len = -70,
        q_abs_log_total = q,
        realized_donor_fingerprint = "d1|d2"
      ),
      candidate_models = candidate_models,
      anchor_scores = tibble::tibble(anchor_model_id = "a1", model_id = "d1"),
      policy_tbl = tibble::tibble(),
      ts_calibration = tibble::tibble(),
      coefficient_calibration = coefficient_calibration,
      config = list(),
      policy_lookup = list(),
      include_competition = FALSE,
      length_grid_n = 5L
    )
  }

  narrow <- make_ctx(0.2)
  wide <- make_ctx(2)

  expect_identical(narrow$coefficient_covariance_source, "empirical_selected_policy_residuals")
  expect_identical(wide$coefficient_covariance_source, "empirical_selected_policy_residuals")
  expect_false(isTRUE(all.equal(wide$coefficient_covariance, narrow$coefficient_covariance)))
  expect_gt(wide$coefficient_covariance[1, 1], narrow$coefficient_covariance[1, 1])

  interval <- tsbiomass:::coefficient_interval_from_context(
    row_now = tibble::tibble(
      anchor_model_id = "a1",
      anchor_species = "Alpha alpha",
      selected_policy = "selected_policy",
      selected_equation_branch_filter = "all",
      policy_slope_len = 20,
      policy_intercept_len = -70
    ),
    ctx = narrow,
    coefficient_calibration = coefficient_calibration
  )

  expect_gt(interval$slope_hi, interval$slope_lo)
  expect_gt(interval$intercept_hi, interval$intercept_lo)
})

test_that("coefficient interval does not silently use nearest residual calibration", {
  row_now <- tibble::tibble(
    anchor_model_id = "a1",
    anchor_species = "Alpha alpha",
    selected_policy = "selected_policy",
    selected_equation_branch_filter = "all",
    policy_slope_len = 20,
    policy_intercept_len = -70,
    meta_q_abs_log_total = 2,
    realized_n_unique_donors = 1,
    realized_donor_fingerprint = "d1"
  )
  ctx <- list(
    coefficient_covariance = matrix(NA_real_, nrow = 2, ncol = 2),
    coefficient_support_n = NA_real_,
    ts_calibration_anchor_n = NA_real_
  )
  coefficient_calibration <- tibble::tibble(
    anchor_model_id = c("c1", "c2", "c3"),
    anchor_species = c("Alpha one", "Alpha two", "Alpha three"),
    policy = "selected_policy",
    equation_branch_filter = "all",
    post_selection_support_bin = NA_character_,
    local_min_combined_distance = c(0.1, 0.2, 0.3),
    local_weighted_mean_combined_distance = c(0.1, 0.2, 0.3),
    slope_resid = c(-2, 1, 3),
    intercept_resid = c(-5, 2, 4)
  )

  out <- tsbiomass:::coefficient_interval_from_context(
    row_now = row_now,
    ctx = ctx,
    coefficient_calibration = coefficient_calibration
  )

  expect_null(out)
})

test_that("fixed20 coefficient intervals retain empirical intercept uncertainty", {
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

  out <- tsbiomass:::augment_conditional_coeff_intervals(
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
