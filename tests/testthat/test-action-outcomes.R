test_that("counterfactual action outcomes preserve the declared biomass direction", {
  pdf <- tibble::tibble(length_cm = c(10, 20), f_len = c(0.25, 0.75))
  donors <- tibble::tibble(
    model_id = c("d1", "d2"),
    species_name = c("Donor one", "Donor two"),
    study_reference_id = c("study_1", "study_2"),
    study_cell_id = c("cell_1", "cell_2"),
    slope_len = c(20, 22),
    intercept_len = c(-70, -72),
    admissible = TRUE,
    combined_distance = c(0.2, 0.4),
    d_species = c(0.1, 0.3),
    d_study = c(0.3, 0.5),
    taxonomic_distance_to_anchor = c(0.2, 0.4),
    frequency_coherence_distance = c(0, 0.2),
    length_coherence_distance = c(0.1, 0.3),
    depth_coherence_distance = c(0.2, 0.4),
    learned_distance_disagreement = c(0.01, 0.02)
  )
  anchor <- tibble::tibble(
    model_id = "a",
    species_name = "Anchor species",
    study_reference_id = "anchor_study",
    study_cell_id = "anchor_cell",
    slope_len = 20,
    intercept_len = -70
  )
  anchor_sigma <- tsbiomass:::equation_sigma_mean(20, -70, pdf)
  eval_obj <- list(anchor_pdf = pdf, anchor_sigma = anchor_sigma, admissible_df = donors)
  actions <- tibble::tibble(
    anchor_model_id = "a",
    anchor_species = "Anchor species",
    action_id = c("singleton", "ensemble"),
    action_signature = c("s1", "s2"),
    equation_branch_filter = "all",
    donor_ids = c("d1", "d1;d2"),
    donor_weights = c("1", "0.25;0.75"),
    donor_footprint = c("d1@1", "d1@0.25;d2@0.75"),
    n_donors = c(1L, 2L),
    policy_slope_len = c(20, 21.5),
    policy_intercept_len = c(-70, -71.5),
    policy_aliases = c("single", "weighted"),
    candidate_pools = "all_admissible",
    aggregation_methods = c("single_donor", "kernel_weighted_mean"),
    n_policy_aliases = 1L,
    includes_exhaustive_singleton = c(TRUE, FALSE)
  )

  result <- tsbiomass:::compute_counterfactual_action_outcomes(
    eval_obj = eval_obj,
    anchor_row = anchor,
    action_bundle = list(actions = actions)
  )

  expect_equal(nrow(result$outcomes), 2L)
  expect_equal(nrow(result$footprints), 3L)
  singleton <- result$outcomes[result$outcomes$action_id == "singleton", ]
  expect_equal(singleton$multiplier_pred, 1, tolerance = 1e-12)
  expect_equal(singleton$signed_log_multiplier, 0, tolerance = 1e-12)
  expect_equal(singleton$selection_loss_abs_log, 0, tolerance = 1e-12)
  expect_equal(singleton$expected_member_abs_log_loss, 0, tolerance = 1e-12)
  expect_equal(singleton$ensemble_cancellation_gap_abs_log, 0, tolerance = 1e-12)
  expect_equal(singleton$effective_source_count, 1, tolerance = 1e-12)
  expect_equal(singleton$ensemble_log_sigma_standard_error, 0, tolerance = 1e-12)
  expect_equal(singleton$ensemble_epistemic_risk_abs_log, 0, tolerance = 1e-12)
  expect_equal(singleton$weighted_curve_rmse_db, 0, tolerance = 1e-12)
  expect_equal(singleton$effective_donor_count, 1)
  expect_equal(singleton$donor_curve_rms_heterogeneity_db, 0, tolerance = 1e-12)

  ensemble <- result$outcomes[result$outcomes$action_id == "ensemble", ]
  expected_sigma <- tsbiomass:::equation_sigma_mean(21.5, -71.5, pdf)
  expect_equal(ensemble$multiplier_pred, anchor_sigma / expected_sigma, tolerance = 1e-12)
  expect_equal(ensemble$selection_loss_abs_log, abs(log(anchor_sigma / expected_sigma)), tolerance = 1e-12)
  member_sigma <- c(
    tsbiomass:::equation_sigma_mean(20, -70, pdf),
    tsbiomass:::equation_sigma_mean(22, -72, pdf)
  )
  expected_member_loss <- sum(c(0.25, 0.75) * abs(log(anchor_sigma / member_sigma)))
  expect_equal(
    ensemble$expected_member_abs_log_loss,
    expected_member_loss,
    tolerance = 1e-12
  )
  expect_gte(
    ensemble$expected_member_abs_log_loss,
    ensemble$selection_loss_abs_log
  )
  expected_source_count <- 1 / (0.25^2 + 0.75^2)
  expected_log_sigma_rms <- sqrt(sum(
    c(0.25, 0.75) *
      (log(member_sigma) - log(expected_sigma))^2
  ))
  expected_standard_error <- expected_log_sigma_rms / sqrt(expected_source_count)
  expect_equal(ensemble$effective_source_count, expected_source_count, tolerance = 1e-12)
  expect_equal(
    ensemble$ensemble_log_sigma_standard_error,
    expected_standard_error,
    tolerance = 1e-12
  )
  expect_equal(
    ensemble$ensemble_epistemic_risk_abs_log,
    sqrt(ensemble$selection_loss_abs_log^2 + expected_standard_error^2),
    tolerance = 1e-12
  )
  expect_equal(
    result$footprints$donor_abs_log_multiplier[
      result$footprints$action_id == "ensemble"
    ],
    abs(log(anchor_sigma / member_sigma)),
    tolerance = 1e-12
  )
  expect_equal(ensemble$effective_donor_count, 1 / (0.25^2 + 0.75^2))
  expect_equal(ensemble$weighted_mean_combined_distance, 0.35, tolerance = 1e-12)
  expect_gt(ensemble$donor_curve_rms_heterogeneity_db, 0)
  expect_true(all(result$outcomes$outcome_estimable))

  reversed <- tsbiomass:::compute_counterfactual_action_outcomes(
    eval_obj = eval_obj,
    anchor_row = anchor,
    action_bundle = list(actions = actions[2:1, , drop = FALSE])
  )
  expect_identical(result$outcomes, reversed$outcomes)
  expect_identical(result$footprints, reversed$footprints)
})

test_that("identical donor burdens remain exactly invariant under aggregation", {
  burden <- 0.77794791762296789
  summary <- tsbiomass:::weighted_action_summary(
    rep(burden, 3L),
    rep(1 / 3, 3L)
  )

  expect_identical(unname(summary[["mean"]]), burden)
  expect_identical(
    tsbiomass:::stable_weighted_mean(rep(burden, 3L), rep(1 / 3, 3L)),
    burden
  )
})

test_that("canonical footprint decoding rejects missing and duplicate donors", {
  action <- tibble::tibble(
    donor_ids = "d1;d1",
    donor_weights = "0.5;0.5",
    n_donors = 2L
  )
  expect_error(
    tsbiomass:::decode_action_footprint(action),
    "duplicate|do not match"
  )

  eval_obj <- list(admissible_df = tibble::tibble(model_id = "d1"))
  missing_action <- tibble::tibble(
    donor_ids = "d2",
    donor_weights = "1",
    n_donors = 1L
  )
  expect_error(
    tsbiomass:::action_donor_rows(missing_action, eval_obj),
    "absent from its admissible pool"
  )
})

test_that("pseudoanchor provenance blocking is explicit and renormalizes support", {
  anchor <- tibble::tibble(model_id = "a", study_reference_id = "study_a")
  rows <- tibble::tibble(
    model_id = c("d1", "d2", "d3"),
    study_reference_id = c("study_a", "study_b", "study_b"),
    w_adm = c(0.5, 0.3, 0.2)
  )
  eval_obj <- list(model_eval = rows, admissible_df = rows)
  result <- tsbiomass:::block_pseudoanchor_provenance(
    eval_obj,
    anchor,
    group_col = "study_reference_id"
  )

  expect_equal(result$audit$admissible_rows_excluded, 1L)
  expect_equal(result$audit$admissible_rows_after, 2L)
  expect_equal(result$evaluation$admissible_df$model_id, c("d2", "d3"))
  expect_equal(sum(result$evaluation$admissible_df$w_adm), 1, tolerance = 1e-12)
  expect_error(
    tsbiomass:::block_pseudoanchor_provenance(eval_obj, anchor, "missing_group"),
    "missing the declared provenance"
  )
})
