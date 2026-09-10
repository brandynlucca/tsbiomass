make_action_risk_fixture <- function() {
  contract <- tsbiomass:::action_risk_feature_contract()
  out <- tibble::tibble(
    anchor_model_id = rep(c("a1", "a2"), each = 2L),
    anchor_species = rep(c("Species one", "Species two"), each = 2L),
    anchor_study_reference_id = rep(c("source_1", "source_2"), each = 2L),
    action_id = rep(c("good", "bad"), 2L),
    selection_loss_abs_log = rep(c(0.1, 1), 2L),
    outcome_estimable = TRUE,
    equation_branch_filter = rep(c("all", "fixed20_only"), 2L),
    policy_sigma_bs_mean = rep(c(1e-6, 1e-4), 2L),
    donor_footprint = rep(c("d1@1", "d2@1"), 2L),
    policy_aliases = rep(c("p1", "p2"), 2L)
  )
  for (nm in contract$feature) {
    if (identical(nm, "n_donors")) {
      out[[nm]] <- 1L
    } else {
      out[[nm]] <- rep(c(0.1, 0.9), 2L)
    }
  }
  out
}

test_that("action-risk preparation gives every pseudoanchor equal total weight", {
  fixture <- make_action_risk_fixture()
  prepared <- tsbiomass:::prepare_action_risk_data(fixture)
  totals <- prepared |>
    dplyr::group_by(.data$.anchor_id) |>
    dplyr::summarise(total = sum(.data$.case_weight), .groups = "drop")

  expect_equal(totals$total, rep(1, nrow(totals)))
  expect_identical(
    attr(prepared, "action_risk_weight_estimand"),
    "equal_total_weight_per_pseudoanchor"
  )
  expect_error(
    tsbiomass:::prepare_action_risk_data(fixture, feature_cols = "anchor_species"),
    "outside the audited"
  )
  expect_error(
    tsbiomass:::prepare_action_risk_data(
      fixture, feature_cols = "equation_branch_filter"
    ),
    "construction-only"
  )
  fixture$policy_slope_len <- 20
  fixture$policy_intercept_len <- -70
  fixture$donor_slope_rms_heterogeneity <- 0
  fixture$donor_intercept_rms_heterogeneity <- 0
  fixture$donor_curve_rms_heterogeneity_db <- 0
  fixture$donor_log_sigma_rms_heterogeneity <- 0
  expect_error(
    tsbiomass:::prepare_action_risk_data(
      fixture,
      feature_cols = "policy_slope_len"
    ),
    "outside the audited"
  )
  expect_error(
    tsbiomass:::prepare_action_risk_data(
      fixture,
      feature_cols = "policy_intercept_len"
    ),
    "outside the audited"
  )
  expect_error(
    tsbiomass:::prepare_action_risk_data(
      fixture,
      feature_cols = "donor_curve_rms_heterogeneity_db"
    ),
    "outside the audited"
  )
  for (forbidden_feature in c(
    "donor_slope_rms_heterogeneity",
    "donor_intercept_rms_heterogeneity",
    "donor_log_sigma_rms_heterogeneity"
  )) {
    expect_error(
      tsbiomass:::prepare_action_risk_data(
        fixture,
        feature_cols = forbidden_feature
      ),
      "outside the audited"
    )
  }
  prepared$policy_slope_len <- 20
  expect_error(
    tsbiomass:::fit_action_risk_base(
      prepared,
      method = "mean",
      feature_cols = "policy_slope_len"
    ),
    "outside the audited"
  )

  regret <- tsbiomass:::prepare_action_risk_data(
    fixture,
    outcome_target = "within_anchor_regret"
  )
  expect_equal(regret$.evaluation_loss, fixture$selection_loss_abs_log)
  expect_equal(regret$.outcome, rep(c(0, 0.9), 2L))
  expect_identical(attr(regret, "action_risk_outcome_target"), "within_anchor_regret")

  expect_error(
    tsbiomass:::prepare_action_risk_data(
      fixture, outcome_target = "expected_member_absolute_loss"
    ),
    "requires 'expected_member_abs_log_loss'"
  )
  expect_error(
    tsbiomass:::prepare_action_risk_data(
      fixture, outcome_target = "ensemble_epistemic_risk"
    ),
    "requires 'ensemble_epistemic_risk_abs_log'"
  )

  augmented <- fixture |>
    dplyr::mutate(
      expected_member_abs_log_loss = .data$selection_loss_abs_log + 0.2,
      ensemble_epistemic_risk_abs_log = sqrt(
        .data$selection_loss_abs_log^2 + 0.3^2
      )
    )
  member_risk <- tsbiomass:::prepare_action_risk_data(
    augmented, outcome_target = "expected_member_absolute_loss"
  )
  epistemic_risk <- tsbiomass:::prepare_action_risk_data(
    augmented, outcome_target = "ensemble_epistemic_risk"
  )
  epistemic_regret <- tsbiomass:::prepare_action_risk_data(
    augmented, outcome_target = "ensemble_epistemic_regret"
  )
  expect_equal(
    member_risk$.evaluation_loss,
    augmented$expected_member_abs_log_loss
  )
  expect_equal(
    epistemic_risk$.evaluation_loss,
    augmented$ensemble_epistemic_risk_abs_log
  )
  expect_identical(
    attr(epistemic_risk, "action_risk_outcome_target"),
    "ensemble_epistemic_risk"
  )
  expect_equal(
    epistemic_regret$.evaluation_loss,
    augmented$ensemble_epistemic_risk_abs_log
  )
  expect_equal(
    epistemic_regret$.outcome,
    rep(c(0, diff(sqrt(c(0.1, 1)^2 + 0.3^2))), 2L)
  )
  expect_identical(
    attr(epistemic_regret, "action_risk_outcome_target"),
    "ensemble_epistemic_regret"
  )
})

test_that("slope branch is construction-only in the action-risk design", {
  fixture <- make_action_risk_fixture()[1, , drop = FALSE]
  fixture <- dplyr::bind_rows(
    dplyr::mutate(fixture, action_id = "same_all", equation_branch_filter = "all"),
    dplyr::mutate(
      fixture,
      action_id = "same_fixed",
      equation_branch_filter = "fixed20_only"
    )
  )
  prepared <- tsbiomass:::prepare_action_risk_data(fixture)
  expect_false("equation_branch_filter" %in%
    attr(prepared, "action_risk_feature_cols"))
  design <- tsbiomass:::action_risk_matrix(
    prepared,
    attr(prepared, "action_risk_feature_cols")
  )$x
  expect_equal(unname(design[1, ]), unname(design[2, ]))
})

test_that("action-risk preparation projects absent configured action levels as zero", {
  fixture <- make_action_risk_fixture()
  fixture$candidate_pools <- "same_family"
  fixture$aggregation_methods <- "nearest_by_taxonomic_distance"
  prepared <- tsbiomass:::prepare_action_risk_data(
    fixture,
    feature_cols = c(
      "action_scope__same_family",
      "action_scope__same_species"
    )
  )

  expect_true(all(prepared$action_scope__same_family))
  expect_true(all(prepared$action_scope__same_species == 0))
  expect_identical(
    attr(prepared, "action_risk_feature_cols"),
    c("action_scope__same_family", "action_scope__same_species")
  )
})

test_that("policy stable identity never depends on equation coefficients", {
  rows <- tibble::tibble(
    model_id = c("a", "b"),
    species_name = c("Species one", "Species two"),
    slope_len = c(18, 27),
    intercept_len = c(-60, -90)
  )
  original <- tsbiomass:::policy_stable_row_key(rows)
  rows$slope_len <- rev(rows$slope_len)
  rows$intercept_len <- rev(rows$intercept_len)
  expect_identical(tsbiomass:::policy_stable_row_key(rows), original)

  footprint <- tibble::tibble(donor_model_id = "a", donor_weight = 1)
  identity_a <- tsbiomass:::realized_action_identity(
    "all", footprint, "single_donor", slope = 18, intercept = -60
  )
  identity_b <- tsbiomass:::realized_action_identity(
    "all", footprint, "single_donor", slope = 27, intercept = -90
  )
  expect_identical(identity_a, identity_b)
  ensemble_footprint <- tibble::tibble(
    donor_model_id = c("a", "b"), donor_weight = c(0.5, 0.5)
  )
  identity_mean <- tsbiomass:::realized_action_identity(
    "all", ensemble_footprint, "arithmetic_mean", slope = 18, intercept = -60
  )
  identity_median <- tsbiomass:::realized_action_identity(
    "all", ensemble_footprint, "median", slope = 18, intercept = -60
  )
  expect_false(identical(identity_mean, identity_median))
})

test_that("balanced provenance folds never split a source", {
  groups <- rep(paste0("g", 1:7), times = c(4, 3, 7, 2, 5, 1, 6))
  units <- paste0(groups, "_", ave(seq_along(groups), groups, FUN = seq_along))
  folds <- tsbiomass:::balanced_provenance_foldid(groups, units, n_folds = 3L, seed = 11L)

  expect_equal(length(folds), length(groups))
  expect_equal(length(unique(folds)), 3L)
  expect_true(all(vapply(split(folds, groups), function(x) length(unique(x)) == 1L, logical(1))))
  expect_error(
    tsbiomass:::balanced_provenance_foldid(c("g1", NA), c("a", "b")),
    "no fallback"
  )
})

test_that("target context is config-derived and excludes target equations", {
  candidates <- tibble::tibble(
    model_id = c("a1", "a2"),
    body_shape = c("fusiform", "elongate"),
    season = c("summer", "winter"),
    study_length_min = c(5, 7),
    study_length_max = c(10, 12),
    species_length_min = c(3, 4),
    species_length_max = c(20, 25),
    frequency = c(38, 70),
    slope_len = c(20, 19)
  )
  config <- list(
    species_traits = c(body_shape = 1),
    study_traits = c(season = 1),
    coherence = list(
      length = list(mode = "overlap", source = "both"),
      frequency = list(mode = "overlap")
    )
  )
  cols <- tsbiomass:::action_risk_target_context_columns(config, candidates)
  expect_setequal(
    cols,
    c(
      "body_shape", "season", "study_length_min", "study_length_max",
      "species_length_min", "species_length_max", "frequency"
    )
  )
  augmented <- tsbiomass:::augment_action_risk_target_context(
    make_action_risk_fixture(), candidates, cols
  )
  prepared <- tsbiomass:::prepare_action_risk_data(augmented)
  expect_true(all(paste0("target_context__", cols) %in%
    attr(prepared, "action_risk_feature_cols")))
  expect_error(
    tsbiomass:::augment_action_risk_target_context(
      make_action_risk_fixture(), candidates, "slope_len"
    ),
    "forbidden"
  )
})

test_that("configured species context uses the full binomial", {
  candidates <- tibble::tibble(
    model_id = c("a1", "a2", "a3", "a4"),
    genus = c("Scomber", "Trachurus", "Engraulis", "Osmerus"),
    species = c("japonicus", "japonicus", "mordax", "mordax")
  )
  outcomes <- tibble::tibble(anchor_model_id = candidates$model_id)
  augmented <- tsbiomass:::augment_action_risk_target_context(
    outcomes,
    candidates,
    "species"
  )

  expect_identical(
    augmented$target_context__species,
    c(
      "Scomber japonicus", "Trachurus japonicus",
      "Engraulis mordax", "Osmerus mordax"
    )
  )
  expect_equal(length(unique(augmented$target_context__species)), 4L)
})

test_that("regret metalearner favors the learner that selects lower-loss actions", {
  fixture <- tsbiomass:::prepare_action_risk_data(make_action_risk_fixture())
  pred <- cbind(
    good = rep(c(0.1, 0.9), 2L),
    bad = rep(c(0.9, 0.1), 2L)
  )
  fitted <- tsbiomass:::fit_action_risk_regret_weights(pred, fixture, denominator = 10L)

  expect_gt(fitted$weights[["good"]], fitted$weights[["bad"]])
  selected <- tsbiomass:::selected_action_rows(
    fixture,
    as.numeric(pred %*% fitted$weights)
  )
  expect_equal(selected$selected_loss, rep(0.1, 2L))
  expect_equal(selected$regret, rep(0, 2L))
})

test_that("mean action-risk learner is weighted and does not cap predictions", {
  fixture <- tsbiomass:::prepare_action_risk_data(make_action_risk_fixture())
  learner <- tsbiomass:::fit_action_risk_base(
    fixture,
    method = "mean",
    feature_cols = attr(fixture, "action_risk_feature_cols")
  )
  pred <- tsbiomass:::predict_action_risk(learner, fixture)

  expect_length(pred, nrow(fixture))
  expect_true(all(is.finite(pred)))
  expect_true(all(pred >= 0))
  expect_equal(length(unique(pred)), 1L)
})

test_that("action-risk monotonicity is explicit and contract-checked", {
  skip_if_not_installed("xgboost")
  fixture <- make_action_risk_fixture()
  fixture$ensemble_epistemic_risk_abs_log <- fixture$min_combined_distance
  prepared <- tsbiomass:::prepare_action_risk_data(
    fixture,
    feature_cols = c(
      "min_combined_distance", "weighted_mean_combined_distance",
      "max_combined_distance"
    ),
    outcome_target = "ensemble_epistemic_risk"
  )
  old <- options(tsbiomass.action_risk_xgboost_rounds = 10L)
  on.exit(options(old), add = TRUE)
  learner <- tsbiomass:::fit_action_risk_base(
    prepared,
    method = "xgboost",
    monotone_features = c(
      "min_combined_distance", "weighted_mean_combined_distance",
      "max_combined_distance"
    )
  )
  expect_setequal(
    learner$monotone_features,
    c(
      "min_combined_distance", "weighted_mean_combined_distance",
      "max_combined_distance"
    )
  )
  expect_error(
    tsbiomass:::fit_action_risk_base(
      prepared,
      method = "xgboost",
      monotone_features = "taxonomic_distance_not_configured"
    ),
    "outside the fitted feature contract"
  )
})

test_that("quantile action-risk learner is explicit and produces finite risk", {
  skip_if_not_installed("xgboost")
  fixture <- make_action_risk_fixture()
  fixture$ensemble_epistemic_risk_abs_log <-
    sqrt(fixture$selection_loss_abs_log^2 + 0.3^2)
  prepared <- tsbiomass:::prepare_action_risk_data(
    fixture,
    outcome_target = "ensemble_epistemic_regret"
  )
  old <- options(
    tsbiomass.action_risk_xgboost_rounds = 10L,
    tsbiomass.action_risk_quantile_alpha = 0.75
  )
  on.exit(options(old), add = TRUE)
  learner <- tsbiomass:::fit_action_risk_base(
    prepared,
    method = "xgboost_quantile"
  )
  prediction <- tsbiomass:::predict_action_risk(learner, prepared)

  expect_identical(learner$method, "xgboost_quantile")
  expect_length(prediction, nrow(prepared))
  expect_true(all(is.finite(prediction)))
  expect_true(all(prediction >= 0))
})

test_that("burden resolution is explicit, invariant, and has no missing fallback", {
  actions <- tibble::tibble(
    action_id = c("ensemble", "singleton"),
    max_distance = c(0.5, 0.3),
    heterogeneity = c(0.1, 0)
  )
  resolved <- tsbiomass:::resolve_action_burden(
    actions,
    burden_fields = c("max_distance", "heterogeneity"),
    directions = c("min", "min")
  )
  reversed <- tsbiomass:::resolve_action_burden(
    actions[2:1, ],
    burden_fields = c("max_distance", "heterogeneity"),
    directions = c("min", "min")
  )
  expect_equal(resolved$selected$action_id, "singleton")
  expect_equal(reversed$selected$action_id, "singleton")
  expect_identical(resolved$audit$burden_field, c("max_distance", "heterogeneity"))
  expect_error(
    tsbiomass:::resolve_action_burden(
      dplyr::mutate(actions, max_distance = c(NA, 0.3)),
      "max_distance",
      "min"
    ),
    "no fallback"
  )
})

test_that("competitive action selector is source-held-out and coefficient-free", {
  oof <- tidyr::expand_grid(
    .anchor_id = paste0("a", 1:4),
    action_id = c("near", "far")
  ) |>
    dplyr::mutate(
      .split_group = paste0("source", sub("a", "", .data$.anchor_id)),
      predicted_risk = dplyr::if_else(.data$action_id == "near", 0.2, 0.5),
      .evaluation_loss = dplyr::if_else(
        .data$action_id == "near" & .data$.anchor_id != "a4", 0.1, 0.4
      ),
      burden_upper = dplyr::if_else(.data$action_id == "near", 0.2, 0.6),
      burden_mean = burden_upper / 2
    )
  deployment <- oof |>
    dplyr::filter(.data$.anchor_id == "a1") |>
    dplyr::mutate(.anchor_id = "target") |>
    dplyr::select(-".evaluation_loss", -".split_group")

  fitted <- tsbiomass:::calibrate_action_competitive_selector(
    oof_actions = oof,
    deployment_actions = deployment,
    burden_fields = c("burden_upper", "burden_mean"),
    directions = c("min", "min"),
    level = 0.90
  )

  expect_s3_class(fitted, "tsb_action_competitive_selector")
  expect_equal(nrow(fitted$calibration), 4L)
  expect_equal(nrow(fitted$oof_selections), 4L)
  expect_equal(nrow(fitted$deployment_selections), 1L)
  expect_identical(fitted$deployment_selections$action_id, "near")
  expect_true(all(fitted$oof_selections$.calibration_sources == 3L))
  expect_true(all(fitted$oof_selections$.selected_regret >= 0))
  expect_false(any(grepl(
    "slope|intercept|coefficient|equation|multiplier",
    names(fitted), ignore.case = TRUE
  )))
})

test_that("marginal multiplier intervals share selected-action regret calibration", {
  oof <- tidyr::expand_grid(
    .anchor_id = paste0("a", 1:5),
    action_id = c("near", "far")
  ) |>
    dplyr::mutate(
      .split_group = paste0("source", sub("a", "", .data$.anchor_id)),
      predicted_risk = dplyr::if_else(.data$action_id == "near", 0.2, 0.5),
      .evaluation_loss = dplyr::case_when(
        .data$action_id == "near" ~ c(0.1, 0.2, 0.3, 0.4, 0.5)[match(.data$.anchor_id, paste0("a", 1:5))],
        TRUE ~ 0.6
      ),
      burden_upper = dplyr::if_else(.data$action_id == "near", 0.2, 0.6),
      multiplier_pred = dplyr::if_else(.data$action_id == "near", 1.2, 0.8)
    )
  deployment <- oof |>
    dplyr::filter(.data$.anchor_id == "a1") |>
    dplyr::mutate(.anchor_id = "target", multiplier_pred = 2) |>
    dplyr::select(-".evaluation_loss", -".split_group")
  selector <- tsbiomass:::calibrate_action_competitive_selector(
    oof,
    deployment,
    burden_fields = "burden_upper",
    directions = "min"
  )
  intervals <- tsbiomass:::calibrate_action_marginal_multiplier_intervals(
    selector,
    levels = c(0.8, 0.9)
  )

  expect_s3_class(intervals, "tsb_action_marginal_intervals")
  expect_false(intervals$literal_biomass_truth_interval)
  expect_equal(nrow(intervals$intervals), 2L)
  expect_true(all(intervals$intervals$selected_inside))
  expect_equal(
    intervals$intervals$multiplier_hi /
      intervals$intervals$multiplier_pred,
    intervals$intervals$multiplier_departure_factor
  )
  expect_equal(
    intervals$intervals$multiplier_pred /
      intervals$intervals$multiplier_lo,
    intervals$intervals$multiplier_departure_factor
  )
})

test_that("competitive action selection has no missing-burden fallback", {
  actions <- tibble::tibble(
    action_id = c("a", "b"),
    predicted_risk = c(0.1, 0.2),
    configured_burden = c(NA_real_, 0.1)
  )
  expect_error(
    tsbiomass:::resolve_action_competitive_set(
      actions,
      radius = 0.2,
      burden_fields = "configured_burden",
      directions = "min"
    ),
    "no fallback"
  )
  expect_error(
    tsbiomass:::calibrate_action_competitive_selector(
      oof_actions = dplyr::mutate(
        actions,
        .anchor_id = "one",
        .split_group = "only_source",
        .evaluation_loss = c(0.1, 0.2)
      ),
      deployment_actions = dplyr::mutate(actions, .anchor_id = "target"),
      burden_fields = "configured_burden",
      directions = "min"
    ),
    "at least two distinct sources"
  )
})
