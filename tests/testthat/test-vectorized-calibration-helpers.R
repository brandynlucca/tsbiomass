test_that("conformal quantile helpers accept vector levels", {
  x <- c(0.2, 0.5, 1.1, 1.6, 2.2)

  out <- conformal_quantile(x, alpha = c(a10 = 0.10, a20 = 0.20))
  expect_equal(out[["a10"]], conformal_quantile(x, alpha = 0.10))
  expect_equal(out[["a20"]], conformal_quantile(x, alpha = 0.20))

  scaled <- scaled_functional_conformal_quantile(x, level = c(l90 = 0.90, l95 = 0.95))
  expect_equal(scaled[["l90"]], scaled_functional_conformal_quantile(x, level = 0.90))
  expect_equal(scaled[["l95"]], scaled_functional_conformal_quantile(x, level = 0.95))
})

test_that("shared support binning preserves post-selection outputs", {
  tbl <- tibble::tibble(
    local_effective_support = c(1, 5, 10, 20),
    local_min_combined_distance = c(0.4, 0.3, 0.2, 0.1)
  )

  out <- assign_post_selection_support_bins(tbl, n_bins = 3L)
  expect_true(all(c(
    "post_selection_support_score",
    "post_selection_support_bin",
    "post_selection_support_label"
  ) %in% names(out)))
  expect_length(out$post_selection_support_bin, nrow(tbl))
})

test_that("shared support binning preserves recommendation outputs", {
  tbl <- tibble::tibble(
    anchor_selection_local_distance = c(0.4, 0.3, 0.2, 0.1),
    source_n_cells = c(1, 2, 3, 4),
    local_effective_support = c(2, 5, 8, 10),
    local_structural_q_abs_log = c(0.6, 0.5, 0.3, 0.2),
    min_length_overlap_fraction = c(0.2, 0.5, 0.7, 0.9),
    min_depth_overlap_fraction = c(0.3, 0.6, 0.8, 1.0)
  )

  out <- assign_recommendation_support_bins(tbl, n_bins = 4L)
  expect_true(all(c(
    "recommendation_support_score",
    "recommendation_support_bin"
  ) %in% names(out)))
  expect_length(out$recommendation_support_bin, nrow(tbl))
})

test_that("weighted quantile helpers match manual calls", {
  x <- c(-2, -1, 0, 1, 2)
  w <- c(1, 2, 3, 2, 1)

  interval_out <- weighted_interval_quantiles(x, w)
  expect_equal(interval_out[["lo80"]], weighted_quantile_or_na(x, w, 0.10))
  expect_equal(interval_out[["hi80"]], weighted_quantile_or_na(x, w, 0.90))
  expect_equal(interval_out[["lo95"]], weighted_quantile_or_na(x, w, 0.025))
  expect_equal(interval_out[["hi99"]], weighted_quantile_or_na(x, w, 0.995))

  upper_out <- weighted_upper_quantiles(abs(x), w)
  expect_equal(upper_out[["q80"]], weighted_quantile_or_na(abs(x), w, 0.80))
  expect_equal(upper_out[["q99"]], weighted_quantile_or_na(abs(x), w, 0.99))
})
