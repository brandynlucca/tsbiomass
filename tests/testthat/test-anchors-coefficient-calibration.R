test_that("selected coefficient calibration pools by policy and branch", {
  candidate_models <- tibble::tibble(
    model_id = c("101", "202"),
    species_name = c("Alpha alpha", "Beta beta"),
    family_name = c("Fam1", "Fam2"),
    slope_len = c(20, 22),
    intercept_len = c(-70, -68)
  )

  ts_error <- tibble::tibble(
    anchor_model_id = c("101", "101", "202", "202"),
    policy = c("closest_within_species", "closest_within_species", "closest_within_species", "closest_within_species"),
    equation_branch_filter = "all",
    policy_slope_len = c(19, 19, 21, 21),
    policy_intercept_len = c(-71, -71, -69, -69)
  )

  selected_tbl <- tibble::tibble(
    anchor_model_id = "999",
    selected_policy = "closest_within_species",
    equation_branch_filter = "all"
  )

  out <- summarize_selected_coefficient_calibration(
    selected_tbl = selected_tbl,
    candidate_models = candidate_models,
    ts_error = ts_error
  )

  expect_equal(nrow(out), 2L)
  expect_setequal(unique(as.character(out$anchor_model_id)), c("101", "202"))
  expect_true(all(as.character(out$policy) == "closest_within_species"))
  expect_true(all(as.character(out$equation_branch_filter) == "all"))
})
