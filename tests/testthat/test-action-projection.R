test_that("fixed-slope projection preserves slope and round-trips biomass endpoints", {
  intervals <- tibble::tibble(
    .anchor_id = "one",
    anchor_species = "Alpha alpha",
    level = c(0.8, 0.9),
    policy_slope_len = 20,
    policy_intercept_len = -70,
    equation_branch_filter = "fixed",
    multiplier_pred = 1,
    multiplier_lo = exp(c(-0.25, -0.5)),
    multiplier_hi = exp(c(0.25, 0.5))
  )
  reference <- tibble::tibble(
    .anchor_id = "one",
    slope_len = 20,
    intercept_len = -71,
    length_pdf_data = list(tibble::tibble(
      length_cm = c(10, 15, 20),
      f_len = c(1, 2, 1)
    ))
  )
  projection <- tsbiomass:::project_action_multiplier_intervals(
    intervals,
    reference,
    fixed_slope_branches = "fixed"
  )

  expect_s3_class(projection, "tsb_action_interval_projection")
  expect_true(all(projection$coefficients$fixed_slope))
  expect_equal(projection$coefficients$slope_lo, rep(20, 2))
  expect_equal(projection$coefficients$slope_hi, rep(20, 2))
  expect_lt(max(projection$roundtrip$maximum_log_scale_error), 1e-7)
  expect_true(all(projection$curves$ts_lo <= projection$curves$ts_selected))
  expect_true(all(projection$curves$ts_hi >= projection$curves$ts_selected))
})

test_that("free-slope projection varies both coefficients without a second uncertainty fit", {
  intervals <- tibble::tibble(
    .anchor_id = "one",
    level = 0.9,
    policy_slope_len = 24,
    policy_intercept_len = -69,
    equation_branch_filter = "free",
    multiplier_pred = 1.5,
    multiplier_lo = 0.75,
    multiplier_hi = 3
  )
  reference <- tibble::tibble(
    .anchor_id = "one",
    slope_len = 20,
    intercept_len = -70,
    length_pdf_data = list(tibble::tibble(
      length_cm = c(8, 12, 16, 20),
      f_len = c(1, 3, 2, 1)
    ))
  )
  projection <- tsbiomass:::project_action_multiplier_intervals(
    intervals,
    reference,
    fixed_slope_branches = "fixed"
  )

  expect_false(projection$coefficients$fixed_slope)
  expect_lt(projection$coefficients$slope_lo, 24)
  expect_gt(projection$coefficients$slope_hi, 24)
  expect_lt(projection$coefficients$intercept_lo, -69)
  expect_gt(projection$coefficients$intercept_hi, -69)
  expect_lt(projection$roundtrip$maximum_log_scale_error, 1e-7)
})

test_that("action projection refuses missing reference support", {
  intervals <- tibble::tibble(
    .anchor_id = "missing",
    level = 0.9,
    policy_slope_len = 20,
    policy_intercept_len = -70,
    equation_branch_filter = "fixed",
    multiplier_pred = 1,
    multiplier_lo = 0.5,
    multiplier_hi = 2
  )
  reference <- tibble::tibble(
    .anchor_id = "other",
    slope_len = 20,
    intercept_len = -70,
    length_pdf_data = list(tibble::tibble(length_cm = 10, f_len = 1))
  )
  expect_error(
    tsbiomass:::project_action_multiplier_intervals(
      intervals,
      reference,
      fixed_slope_branches = "fixed"
    ),
    "no fallback"
  )
})
