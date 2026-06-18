test_that("convert_to_length_form invalidates implausible TS-length coefficients", {
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

  expect_true(all(out$invalid_ts_length_curve[2:4]))
  expect_true(all(out$implausible_ts_length_coefficients[2:4]))
  expect_true(all(is.na(out$slope_len[2:4])))
  expect_true(all(is.na(out$intercept_len[2:4])))
})
