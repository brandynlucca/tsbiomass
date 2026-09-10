test_that("convert_to_length_form preserves finite compatible coefficients without heuristic cutoffs", {
  tbl <- tibble::tibble(
    slope = c(20, -10, 55, 6.9),
    intercept = c(-70, -60, -90, -8.4),
    equation_form = rep("mlog10_ind", 4),
    equation_length_unit = rep("cm", 4),
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

test_that("convert_to_length_form standardizes explicit equation length units", {
  rows <- tibble::tibble(
    slope = c(20, 20, 20),
    intercept = c(-70, -90, -30),
    equation_form = rep("mlog10_ind", 3),
    equation_length_unit = c("cm", "mm", "m"),
    study_length_min = rep(10, 3),
    study_length_max = rep(30, 3)
  )

  out <- tsbiomass:::convert_to_length_form(rows)

  expect_equal(out$slope_len, rep(20, 3))
  expect_equal(out$intercept_len, rep(-70, 3))
})

test_that("convert_to_length_form rejects missing units and excludes inverse equations", {
  missing_unit <- tibble::tibble(
    slope = 20, intercept = -70, equation_form = "mlog10_ind"
  )
  expect_error(
    tsbiomass:::convert_to_length_form(missing_unit),
    "equation_length_unit"
  )

  inverse <- tibble::tibble(
    slope = 6.9, intercept = -8.4, equation_form = "mlog10_ind",
    equation_length_unit = "cm",
    misc_factors = "reported as log L = a TS_kg + b",
    study_length_min = 20, study_length_max = 50
  )
  out <- tsbiomass:::convert_to_length_form(inverse)
  expect_true(out$inverse_length_equation_flag)
  expect_true(is.na(out$slope_len))
  expect_true(is.na(out$intercept_len))
})
