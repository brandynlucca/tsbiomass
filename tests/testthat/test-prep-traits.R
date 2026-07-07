test_that("convert_to_length_form preserves finite compatible coefficients without heuristic cutoffs", {
  tbl <- tibble::tibble(
    slope = c(20, -10, 55, 6.9),
    intercept = c(-70, -60, -90, -8.4),
    equation_form = rep("mlog10_ind", 4),
    study_length_min = c(10, 10, 10, 20),
    study_length_max = c(30, 30, 30, 50)
  )

  out <- tsbiomass:::convert_to_length_form(tbl)

  expect_false(out$invalid_ts_length_curve[[1]])
  expect_false(out$implausible_ts_length_coefficients[[1]])
  expect_equal(out$slope_len[[1]], 20)
  expect_equal(out$intercept_len[[1]], -70)

  expect_false(out$invalid_ts_length_curve[[2]])
  expect_false(out$implausible_ts_length_coefficients[[2]])
  expect_equal(out$slope_len[[2]], -10)
  expect_equal(out$intercept_len[[2]], -60)

  expect_false(out$invalid_ts_length_curve[[3]])
  expect_false(out$implausible_ts_length_coefficients[[3]])
  expect_equal(out$slope_len[[3]], 55)
  expect_equal(out$intercept_len[[3]], -90)

  # This curve reaches non-negative TS on its reported support, which remains
  # a directly evaluated physical invalidity rather than a coefficient cutoff.
  expect_true(out$invalid_ts_length_curve[[4]])
  expect_false(out$implausible_ts_length_coefficients[[4]])
  expect_true(is.na(out$slope_len[[4]]))
  expect_true(is.na(out$intercept_len[[4]]))
})
