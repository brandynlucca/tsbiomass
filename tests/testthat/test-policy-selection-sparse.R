test_that("policy selection returns typed empty tables for sparse inputs", {
  out <- run_policy_selection(
    species_performance_table = tibble::tibble(
      policy = character(0),
      equation_branch_filter = character(0),
      anchor_species = character(0),
      valid_prediction = logical(0),
      error_abs_log = numeric(0),
      multiplier_pred = numeric(0)
    ),
    candidate_models = tibble::tibble(),
    config = list()
  )

  expect_true(all(c("policy", "equation_branch_filter") %in% names(out$select_ref)))
  expect_true(all(c("policy", "equation_branch_filter") %in% names(out$final_ref)))
  expect_true(all(c("policy", "equation_branch_filter") %in% names(out$equiv_sets)))
  expect_true(all(c("policy", "equation_branch_filter") %in% names(out$equiv_ref$best_flags)))
})
