test_that("as_conjurer builds a staged Conjurer object", {
  selector <- make_selector(candidates = make_candidates(seed_similarity_tuning = TRUE))

  conjurer <- as_conjurer(selector)

  expect_true(S7::S7_inherits(conjurer, Conjurer))
  expect_true((inherits(conjurer@selector, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(conjurer@selector, PolicySelector), error = function(e) FALSE))))
  expect_null(conjurer@learner)
  expect_true(is.list(conjurer@results))
  expect_equal(nrow(conjurer@manifest), 0)
  expect_equal(nrow(conjurer@draws), 0)
  expect_equal(nrow(conjurer@summary), 0)
})

test_that("simulate.Conjurer stores draw-level and summary outputs", {
  selector <- make_selector(candidates = make_candidates(seed_similarity_tuning = TRUE))
  conjurer <- as_conjurer(selector, config = list(conjurer = list(n_draws = 2L, seed = 7L)))

  baseline_predictions <- PolicyPredictions(
    intervals = tibble::tibble(),
    selections = tibble::tibble(
      anchor_model_id = c("1", "2"),
      anchor_species = c("Alpha alpha", "Beta beta"),
      selected_policy = c("policy_a", "policy_b"),
      selected_policy_display = c("Policy A", "Policy B"),
      multiplier_pred = c(1.1, 0.9)
    ),
    consensus = tibble::tibble(
      anchor_model_id = c("1", "2"),
      anchor_species = c("Alpha alpha", "Beta beta"),
      n_admissible = c(4L, 3L),
      consensus_multiplier = c(1.1, 0.9),
      log_spread = c(0.2, 0.3)
    )
  )

  testthat::local_mocked_bindings(
    predict = function(object,
                       ...) {
      baseline_predictions
    },
    .package = "stats"
  )
  testthat::local_mocked_bindings(
    conjurer_trait_draw_results = function(object,
                                           trait_name,
                                           n_draws,
                                           config = NULL,
                                           seed = 1L,
                                           progress = FALSE,
                                           registry_path = NULL,
                                           policy_path = NULL) {
      selected_tbl <- tibble::tibble(
        trait = trait_name,
        draw_id = c(1L, 1L, 2L, 2L),
        anchor_model_id = c("1", "2", "1", "2"),
        anchor_species = c("Alpha alpha", "Beta beta", "Alpha alpha", "Beta beta"),
        selected_policy = c("policy_a", "policy_b", if (trait_name == "frequency") "policy_c" else "policy_a", "policy_b"),
        selected_policy_display = c("Policy A", "Policy B", if (trait_name == "frequency") "Policy C" else "Policy A", "Policy B"),
        multiplier_pred = c(1.1, 0.9, 1.3, 1.0)
      )
      consensus_tbl <- tibble::tibble(
        trait = trait_name,
        draw_id = c(1L, 1L, 2L, 2L),
        anchor_model_id = c("1", "2", "1", "2"),
        anchor_species = c("Alpha alpha", "Beta beta", "Alpha alpha", "Beta beta"),
        n_admissible = c(4L, 3L, 5L, 4L),
        consensus_multiplier = c(1.1, 0.9, 1.2, 1.0),
        log_spread = c(0.2, 0.3, 0.4, 0.2)
      )
      list(
        selected = selected_tbl,
        consensus = consensus_tbl,
        manifest = tibble::tibble(
          trait = trait_name,
          n_missing = 2L,
          n_observed = 3L,
          n_draws = n_draws,
          status = "ok"
        )
      )
    },
    .package = "tsbiomass"
  )

  conjured <- simulate(conjurer)

  expect_true(S7::S7_inherits(conjured, Conjurer))
  expect_equal(sort(conjured@manifest$trait), c("fao_area", "frequency"))
  expect_equal(nrow(conjured@draws), 8)
  expect_true(all(c(
    "trait", "anchor_model_id", "anchor_species", "modal_selected_policy",
    "switch_rate_vs_baseline", "mean_n_admissible", "mean_abs_db_shift",
    "sd_db_shift", "q95_abs_db_shift"
  ) %in% names(conjured@summary)))

  freq_alpha <- conjured@summary |>
    dplyr::filter(trait == "frequency", anchor_model_id == "1")
  expect_equal(freq_alpha$switch_rate_vs_baseline[[1]], 0.5)
  expect_true(is.finite(freq_alpha$sd_log_multiplier[[1]]))
  expect_equal(
    freq_alpha$mean_abs_db_shift[[1]],
    0.5 * 10 * log10(1.3 / 1.1),
    tolerance = 1e-8
  )
})

test_that("plot.Conjurer renders from the graphics plot generic", {
  selector <- make_selector(candidates = make_candidates(seed_similarity_tuning = TRUE))
  conjurer <- as_conjurer(selector)
  conjurer <- conjurer_rebuild(
    conjurer,
    summary = tibble::tibble(
      trait = c("frequency", "fao_area"),
      anchor_model_id = c("1", "1"),
      anchor_species = c("Alpha alpha", "Alpha alpha"),
      mean_abs_db_shift = c(0.25, 0.10),
      q95_abs_db_shift = c(0.60, 0.22),
      switch_rate_vs_baseline = c(0.30, 0.10),
      sd_n_admissible = c(1.2, 0.4)
    )
  )

  p1 <- plot(conjurer)
  p2 <- plot(conjurer, metric = "switch_rate_vs_baseline")

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("show.Conjurer prints a compact analysis summary", {
  selector <- make_selector(candidates = make_candidates(seed_similarity_tuning = TRUE))
  conjurer <- as_conjurer(selector, config = list(conjurer = list(n_draws = 12L)))
  conjurer <- conjurer_rebuild(
    conjurer,
    manifest = tibble::tibble(
      trait = c("frequency", "fao_area"),
      n_missing = c(2L, 1L),
      n_observed = c(3L, 4L),
      n_draws = c(12L, 12L),
      status = c("ok", "no_missing_rows")
    ),
    draws = tibble::tibble(
      trait = c("frequency", "frequency"),
      draw_id = c(1L, 2L),
      anchor_model_id = c("1", "1"),
      anchor_species = c("Alpha alpha", "Alpha alpha"),
      selected_policy = c("policy_a", "policy_b"),
      selected_policy_display = c("Policy A", "Policy B"),
      multiplier_pred = c(1.1, 1.2)
    ),
    summary = tibble::tibble(
      trait = c("frequency", "fao_area"),
      anchor_model_id = c("1", "1"),
      anchor_species = c("Alpha alpha", "Alpha alpha"),
      mean_abs_db_shift = c(0.42, 0.10),
      switch_rate_vs_baseline = c(0.25, 0.05)
    )
  )

  output <- paste(capture.output(show(conjurer)), collapse = "\n")

  expect_match(output, "Conjurer")
  expect_match(output, "stage: simulated")
  expect_match(output, "reference_species: Alpha alpha")
  expect_match(output, "draws_per_trait: 12")
  expect_match(output, "trait_status: no_missing_rows=1, ok=1|trait_status: ok=1, no_missing_rows=1")
  expect_match(output, "largest_mean_abs_db_shift: Alpha alpha / frequency = 0.42")
  expect_match(output, "largest_policy_switch_rate: Alpha alpha / frequency = 25%")
})
