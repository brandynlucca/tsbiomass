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

test_that("TS calibration curve does not fall back to anchor-specific subsets", {
  ts_calibration <- tibble::tibble(
    anchor_model_id = c(rep("101", 4), rep("202", 4)),
    anchor_species = c(rep("Alpha alpha", 4), rep("Beta beta", 4)),
    anchor_family = c(rep("Fam1", 4), rep("Fam2", 4)),
    policy = "closest_within_species",
    equation_branch_filter = "all",
    u = c(0, 1, 0, 1, 0, 1, 0, 1),
    ts_error = c(0.5, 0.5, -0.5, -0.5, 1.5, 1.5, -1.5, -1.5),
    log_sigma_residual = c(0.05, 0.05, -0.05, -0.05, 0.15, 0.15, -0.15, -0.15)
  )

  row_now <- tibble::tibble(
    anchor_model_id = "101",
    anchor_species = "Alpha alpha",
    anchor_family = "Fam1",
    selected_policy = "closest_within_species",
    equation_branch_filter = "all"
  )

  out <- select_ts_calibration_curve(
    ts_calibration = ts_calibration,
    row_now = row_now,
    min_anchor_neighbors = 4L
  )

  expect_equal(nrow(out), 2L)
  expect_equal(sort(out$n), c(4, 4))
  expect_true(all(is.finite(out$q95_ts_abs_dev)))
})

test_that("stale TS calibration tables recover donor locality from policy rows", {
  ts_calibration <- tibble::tibble(
    anchor_model_id = c("101", "101", "202", "202"),
    policy = "closest_within_species",
    equation_branch_filter = "all",
    u = c(0.1, 0.9, 0.1, 0.9),
    ts_error = c(0.2, 0.1, -0.1, 0.05),
    log_sigma_residual = c(0.02, 0.01, -0.01, 0.005)
  )

  policy_perf <- tibble::tibble(
    anchor_model_id = c("101", "202"),
    policy = "closest_within_species",
    equation_branch_filter = "all",
    local_min_combined_distance = c(0.15, 0.85),
    local_weighted_mean_combined_distance = c(0.20, 0.90),
    local_effective_support = c(1, 2),
    local_mean_length_overlap = c(0.75, 0.25),
    local_mean_depth_overlap = c(0.60, 0.40)
  )

  out <- enrich_ts_calibration_locality(
    ts_calibration = ts_calibration,
    policy_perf = policy_perf
  )

  expect_true(all(c(
    "local_min_combined_distance",
    "local_weighted_mean_combined_distance",
    "local_effective_support",
    "local_mean_length_overlap",
    "local_mean_depth_overlap"
  ) %in% names(out)))
  expect_equal(
    out$local_min_combined_distance[out$anchor_model_id == "101"],
    c(0.15, 0.15)
  )
  expect_equal(
    out$local_min_combined_distance[out$anchor_model_id == "202"],
    c(0.85, 0.85)
  )
})

test_that("TS calibration curve localizes by donor difficulty when available", {
  anchor_ids <- sprintf("%03d", 1:8)
  difficulties <- seq(0.1, 0.8, by = 0.1)
  ts_calibration <- purrr::map2_dfr(anchor_ids, difficulties, function(id, d) {
    tibble::tibble(
      anchor_model_id = id,
      policy = "closest_within_species",
      equation_branch_filter = "all",
      u = c(0, 1),
      ts_error = c(d, d),
      log_sigma_residual = c(d / 10, d / 10),
      local_min_combined_distance = d
    )
  })

  row_now <- tibble::tibble(
    anchor_model_id = "999",
    selected_policy = "closest_within_species",
    equation_branch_filter = "all",
    anchor_selection_local_distance = 0.82
  )

  out <- select_ts_calibration_curve(
    ts_calibration = ts_calibration,
    row_now = row_now,
    min_anchor_neighbors = 4L
  )

  expect_equal(nrow(out), 2L)
  expect_equal(sort(out$n), c(4, 4))
  expect_equal(out$median_ts_error, c(0.65, 0.65))
})
