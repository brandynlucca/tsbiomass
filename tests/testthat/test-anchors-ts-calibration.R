test_that("selected TS calibration pools by policy and branch rather than anchor id", {
  ts_error <- tibble::tibble(
    anchor_model_id = c("101", "101", "202", "202"),
    anchor_species = c("Alpha alpha", "Alpha alpha", "Beta beta", "Beta beta"),
    anchor_family = c("Fam1", "Fam1", "Fam2", "Fam2"),
    policy = c("closest_within_species", "closest_within_species", "closest_within_species", "closest_within_species"),
    equation_branch_filter = "all",
    u = c(0.1, 0.9, 0.1, 0.9),
    ts_error = c(0.2, 0.1, -0.1, 0.05),
    log_sigma_residual = c(0.02, 0.01, -0.01, 0.005)
  )

  selected_tbl <- tibble::tibble(
    anchor_model_id = "999",
    anchor_species = "Gamma gamma",
    anchor_family = "Fam3",
    selected_policy = "closest_within_species",
    equation_branch_filter = "all"
  )

  out <- summarize_selected_ts_calibration(
    ts_error = ts_error,
    selected_tbl = selected_tbl
  )

  expect_equal(nrow(out), 4L)
  expect_setequal(unique(as.character(out$anchor_model_id)), c("101", "202"))
  expect_true(all(as.character(out$policy) == "closest_within_species"))
  expect_true(all(as.character(out$equation_branch_filter) == "all"))
})
