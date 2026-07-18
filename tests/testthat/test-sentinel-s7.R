test_that("Sentinel infers split columns and builds manifests", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2", "m3", "m4"),
    species_name = c("sp1", "sp1", "sp2", "sp2"),
    study_reference_id = c("study_a", "study_a", "study_b", "study_c"),
    study_cell_id = c("cell_1", "cell_1", "cell_2", "cell_3"),
    value = c(1, 2, 3, 4)
  )

  scenarios <- create_scenarios(
    trait_ablations = list(no_value = "value")
  )
  out_dir <- file.path(tempdir(), paste0("sentinel-test-", as.integer(Sys.time())))

  sentinel <- build_sentinel(
    data = candidate_models,
    split_mode = "species_holdout",
    scenario_grid = scenarios,
    output_dir = out_dir
  )

  expect_equal(sentinel@split_col, "species_name")
  expect_setequal(names(sentinel@scenario_grid), c("baseline", "no_value"))

  sentinel <- build_sentinel_manifest(sentinel)
  expect_equal(nrow(sentinel@manifest), 4L)
  expect_true(all(file.exists(sentinel_output_paths(sentinel)$manifest_file)))
  expect_true("cache_dir" %in% names(sentinel@manifest))
  expect_true(all(nzchar(sentinel@manifest$cache_dir)))
  expect_setequal(unique(sentinel@manifest$scenario), c("baseline", "no_value"))
  expect_setequal(unique(sentinel@manifest$holdout_id), c("sp1", "sp2"))
})

test_that("Sentinel scenario grids can encode row-filter ablations", {
  scenarios <- create_scenarios(
    model_ablations = list(
      no_policy_a = list(policy = "policy_a")
    )
  )

  expect_true("baseline" %in% names(scenarios))
  expect_equal(scenarios$no_policy_a$drop_rows$policy, "policy_a")
})

test_that("Sentinel trait scenarios support automatic leave-one-trait-out grids", {
  scenarios <- create_scenarios(
    trait_ablations = c("family", "mean_depth")
  )

  expect_setequal(names(scenarios), c("baseline", "without_family", "without_mean_depth"))
  expect_equal(scenarios$without_family$scenario_type, "trait_ablation")
  expect_equal(scenarios$without_family$ablated_traits, "family")
  expect_equal(scenarios$without_family$exclude_similarity_traits, "family")
  expect_null(scenarios$without_family$drop_columns)

  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2"),
    species_name = c("sp1", "sp2"),
    family = c("f1", "f2"),
    mean_depth = c(10, 20)
  )
  sentinel <- build_sentinel(
    candidate_models,
    config = list(alchemist = list(
      species_traits = list(family = 1),
      study_traits = list(mean_depth = 1, absent_trait = 1)
    )),
    split_mode = "species_holdout",
    trait_ablations = TRUE,
    output_dir = file.path(tempdir(), paste0("sentinel-all-traits-", as.integer(Sys.time())))
  )
  expect_setequal(
    names(sentinel@scenario_grid),
    c("baseline", "without_family", "without_mean_depth")
  )
  expect_equal(sentinel@options$trait_ablation_missing, "absent_trait")
})

test_that("Sentinel collapses configured taxonomic ranks into one effective-feature ablation", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:4),
    species_name = paste0("sp", 1:4),
    family = c("f1", "f1", "f2", "f2"),
    genus = c("g1", "g2", "g3", "g4"),
    species = paste0("Species ", 1:4),
    mean_depth = c(10, 20, 30, 40)
  )
  sentinel <- build_sentinel(
    candidate_models,
    config = list(
      alchemist = list(
        taxonomic_distance = TRUE,
        species_traits = c("family", "genus", "species"),
        study_traits = "mean_depth"
      ),
      policies = list(
        group = list(family = NULL, genus = NULL, species = NULL)
      )
    ),
    split_mode = "species_holdout",
    trait_ablations = TRUE,
    output_dir = file.path(tempdir(), paste0("sentinel-taxonomic-ablation-", as.integer(Sys.time())))
  )

  expect_setequal(
    names(sentinel@scenario_grid),
    c("baseline", "without_taxonomic_distance", "without_mean_depth")
  )
  taxonomy <- sentinel@scenario_grid$without_taxonomic_distance
  expect_setequal(taxonomy$exclude_similarity_traits, c("family", "genus", "species"))
  expect_setequal(taxonomy$ablated_traits, c("family", "genus", "species"))
  expect_equal(taxonomy$ablation_component, "taxonomic_distance")

  fold_config <- sentinel_exclude_similarity_traits(
    config = sentinel@config,
    traits = taxonomy$exclude_similarity_traits
  )
  expect_length(fold_config$alchemist$species_traits, 0L)
  expect_identical(fold_config$policies, sentinel@config$policies)
  expect_true(all(c("family", "genus", "species") %in% names(sentinel@data)))
})

test_that("Sentinel manifests pair scenarios on stable outer-fold IDs", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:4),
    species_name = rep(c("sp1", "sp2"), each = 2),
    trait_a = 1:4
  )
  sentinel <- build_sentinel(
    data = candidate_models,
    split_mode = "species_holdout",
    scenario_grid = create_scenarios(trait_ablations = "trait_a"),
    output_dir = file.path(tempdir(), paste0("sentinel-pairing-", as.integer(Sys.time())))
  )
  sentinel <- build_sentinel_manifest(sentinel)

  pairing <- sentinel@manifest |>
    dplyr::select("scenario", "holdout_id", "outer_fold_id") |>
    tidyr::pivot_wider(names_from = "scenario", values_from = "outer_fold_id")
  expect_equal(pairing$baseline, pairing$without_trait_a)
  expect_true(all(c("scenario_type", "ablated_traits", "repeat_id") %in% names(sentinel@manifest)))
})

test_that("Sentinel ablation summaries are paired and plot-ready", {
  results <- tibble::tibble(
    scenario = rep(c("baseline", "without_depth", "without_family"), each = 4),
    scenario_type = rep(c("baseline", "trait_ablation", "trait_ablation"), each = 4),
    ablated_traits = rep(c("", "depth", "family"), each = 4),
    outer_fold_id = rep(1:4, 3),
    holdout_id = rep(paste0("sp", 1:4), 3),
    error_abs_log = c(
      0.10, 0.20, 0.30, 0.40,
      0.20, 0.40, 0.40, 0.60,
      0.05, 0.25, 0.25, 0.45
    )
  )

  scorecard <- summarize_sentinel_ablation(results, bootstrap = FALSE, seed = 4L)
  summary_tbl <- scorecard@recommendation_cards
  depth <- dplyr::filter(summary_tbl, .data$scenario == "without_depth")
  family <- dplyr::filter(summary_tbl, .data$scenario == "without_family")

  expect_true(S7::S7_inherits(scorecard, Scorecard))
  expect_equal(depth$importance, 0.15)
  expect_equal(family$importance, 0)
  expect_equal(depth$n_pairs, 4L)
  ablation_plot <- plot(scorecard, type = "ablation")
  expect_s3_class(ablation_plot, "ggplot")
  expect_null(ablation_plot$labels$title)
  expect_null(ablation_plot$labels$subtitle)

  validation_card <- summarize_sentinel_validation(results, metric_cols = "error_abs_log")
  expect_true(S7::S7_inherits(validation_card, Scorecard))
  validation_plot <- plot(validation_card, type = "validation")
  expect_s3_class(validation_plot, "ggplot")
  expect_null(validation_plot$labels$title)
  expect_null(validation_plot$labels$subtitle)
  expect_true(any(grepl(
    "Validation (all traits)",
    unique(validation_plot$data$series_display),
    fixed = TRUE
  )))
})

test_that("Sentinel ablation decomposition returns a three-panel Scorecard plot", {
  baseline_raw <- c(0.20, 0.30, 0.40, 0.50)
  baseline_oracle <- c(0.10, 0.20, 0.30, 0.40)
  results <- dplyr::bind_rows(
    tibble::tibble(
      scenario = "baseline",
      scenario_type = "baseline",
      ablated_traits = "",
      ablation_component = "",
      outer_fold_id = 1:4,
      holdout_id = paste0("sp", 1:4),
      error_abs_log = baseline_raw,
      oracle_abs_log_error = baseline_oracle,
      selection_regret_abs_log = baseline_raw - baseline_oracle
    ),
    tibble::tibble(
      scenario = "without_depth",
      scenario_type = "trait_ablation",
      ablated_traits = "depth",
      ablation_component = "depth",
      outer_fold_id = 1:4,
      holdout_id = paste0("sp", 1:4),
      error_abs_log = baseline_raw + 0.09,
      oracle_abs_log_error = baseline_oracle + 0.04,
      selection_regret_abs_log = baseline_raw - baseline_oracle + 0.05
    ),
    tibble::tibble(
      scenario = "without_taxonomic_distance",
      scenario_type = "trait_ablation",
      ablated_traits = "family|genus|species",
      ablation_component = "taxonomic_distance",
      outer_fold_id = 1:4,
      holdout_id = paste0("sp", 1:4),
      error_abs_log = baseline_raw - 0.03,
      oracle_abs_log_error = baseline_oracle - 0.01,
      selection_regret_abs_log = baseline_raw - baseline_oracle - 0.02
    )
  )

  scorecard <- summarize_sentinel_ablation_decomposition(
    results,
    bootstrap = FALSE,
    cluster_names = "holdout_id",
    strata_names = character(0)
  )
  cards <- scorecard@recommendation_cards

  expect_true(S7::S7_inherits(scorecard, Scorecard))
  expect_equal(nrow(cards), 6L)
  expect_setequal(cards$component, c("raw", "oracle", "regret"))
  expect_lt(max(abs(cards$decomposition_residual)), 1e-12)
  expect_equal(
    cards$importance[cards$scenario == "without_depth" & cards$component == "raw"],
    0.09
  )

  decomposition_plot <- plot(scorecard, type = "ablation_decomposition")
  expect_s3_class(decomposition_plot, "ggplot")
  expect_null(decomposition_plot$labels$title)
  expect_null(decomposition_plot$labels$subtitle)
  expect_equal(length(decomposition_plot$facet$params$cols), 1L)
  expect_true("Taxonomic distance" %in% levels(decomposition_plot$data$display_label))
  expect_silent(ggplot2::ggplot_build(decomposition_plot))
})

test_that("Sentinel validation plots expose grouped species holdout results", {
  results <- tibble::tibble(
    scenario = "baseline",
    split_mode = "species_holdout",
    repeat_id = 1L,
    outer_fold_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    holdout_id = c(
      "species_fold_01", "species_fold_01", "species_fold_01",
      "species_fold_02", "species_fold_02", "species_fold_02"
    ),
    anchor_species = c("sp1", "sp1", "sp2", "sp3", "sp3", "sp3"),
    anchor_model_id = seq_len(6L),
    error_abs_log = c(0.1, 0.3, NA, 0.2, 0.4, 0.6)
  )
  scorecard <- summarize_sentinel_validation(
    results,
    metric_cols = "error_abs_log"
  )

  distribution <- plot(scorecard, type = "validation")
  expect_s3_class(distribution, "ggplot")
  expect_equal(nrow(distribution$data), 2L)
  expect_equal(max(distribution$data$cumulative_species), 2 / 3)
  expect_match(
    unique(distribution$data$series_display),
    "Species holdout \\(all traits\\) \\(2/3 species estimable\\)"
  )
  expect_equal(
    distribution$labels$y,
    "Cumulative proportion of held-out species"
  )
  expect_null(distribution$labels$title)

  compact <- plot(
    scorecard,
    type = "validation",
    view = "ranked",
    metric_scale = "pseudo_log",
    label_species = "sp3"
  )
  expect_s3_class(compact, "ggplot")
  expect_equal(nrow(compact$data), 2L)
  expect_setequal(compact$data$species_rank, c(1L, 2L))
  expect_equal(nrow(compact$layers[[2]]$data), 1L)
  expect_match(compact$labels$caption, "2/3 species estimable")
  expect_null(compact$labels$title)
  expect_silent(ggplot2::ggplot_build(compact))

  ranked <- plot(
    scorecard,
    type = "validation",
    view = "ranked_species",
    metric_scale = "pseudo_log"
  )
  expect_s3_class(ranked, "ggplot")
  expect_equal(nrow(ranked$data), 2L)
  expect_match(ranked$labels$caption, "1 species without a finite estimate")
  expect_match(ranked$labels$caption, "individual equations")
  expect_equal(nrow(ranked$layers[[1]]$data), 5L)
  expect_null(ranked$labels$title)
  expect_silent(ggplot2::ggplot_build(ranked))

  fold <- plot(scorecard, type = "validation", view = "fold")
  expect_s3_class(fold, "ggplot")
  expect_equal(nrow(fold$data), 2L)
  expect_setequal(fold$data$.sentinel_series, "Species holdout (all traits)")
  expect_null(fold$labels$title)
})

test_that("Sentinel validation marks configured reference-anchor species", {
  source <- tibble::tibble(
    model_id = 1:4,
    species_name = c("sp1", "sp2", "sp3", "sp4"),
    regional_body = c("reference", "candidate", "reference", "candidate")
  )
  sentinel <- build_sentinel(
    data = source,
    config = list(
      candidates = list(
        anchors = list(selector = list(regional_body = "reference"))
      )
    ),
    split_mode = "species_holdout",
    output_dir = file.path(tempdir(), paste0("sentinel-reference-labels-", as.integer(Sys.time())))
  )
  expect_setequal(
    sentinel_reference_anchor_species(sentinel),
    c("sp1", "sp3")
  )
  flagged <- sentinel_add_reference_anchor_flag(
    sentinel,
    tibble::tibble(anchor_species = c("sp1", "sp2", "sp3"))
  )
  expect_identical(
    flagged$is_reference_anchor_species,
    c(TRUE, FALSE, TRUE)
  )
})

test_that("Sentinel coverage separates conditional, operational, and estimability rates", {
  results <- tibble::tibble(
    scenario = "baseline",
    split_mode = "species_holdout",
    outer_fold_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    anchor_species = paste0("sp", 1:6),
    anchor_model_id = 1:6,
    error_abs_log = c(0.1, 0.2, NA, 0.3, 0.4, NA),
    interval_log_width = c(0.5, 0.5, NA, 0.6, 0.6, NA),
    valid_prediction = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
    covered = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
  )
  scorecard <- summarize_sentinel_coverage(
    results,
    nominal = 0.9,
    confidence_level = 0.95
  )
  coverage <- scorecard@recommendation_cards

  expect_true(S7::S7_inherits(scorecard, Scorecard))
  expect_equal(coverage$n_total, 6L)
  expect_equal(coverage$n_estimable, 4L)
  expect_equal(coverage$n_covered, 3L)
  expect_equal(coverage$conditional_coverage, 0.75)
  expect_equal(coverage$operational_coverage, 0.5)
  expect_equal(coverage$estimability_rate, 2 / 3)
  expect_equal(coverage$nominal_coverage, 0.9)
  expect_true(all(is.finite(c(
    coverage$conditional_lower,
    coverage$conditional_upper,
    coverage$operational_lower,
    coverage$operational_upper,
    coverage$estimability_lower,
    coverage$estimability_upper
  ))))

  conditional_plot <- plot(scorecard, type = "coverage", estimand = "conditional")
  operational_plot <- plot(scorecard, type = "coverage", estimand = "operational")
  estimability_plot <- plot(scorecard, type = "coverage", estimand = "estimability")
  expect_s3_class(conditional_plot, "ggplot")
  expect_s3_class(operational_plot, "ggplot")
  expect_s3_class(estimability_plot, "ggplot")
  expect_null(conditional_plot$labels$title)
  expect_silent(ggplot2::ggplot_build(conditional_plot))
  expect_silent(ggplot2::ggplot_build(operational_plot))
  expect_silent(ggplot2::ggplot_build(estimability_plot))
})

test_that("Sentinel ablation summaries support paired clustered bootstrap inference", {
  species <- paste0("sp", 1:8)
  folds <- rep(1:2, each = 4)
  baseline_error <- c(0.20, 0.25, 0.30, 0.35, 0.22, 0.27, 0.32, 0.37)
  results <- dplyr::bind_rows(
    tibble::tibble(
      scenario = "baseline",
      scenario_type = "baseline",
      ablated_traits = "",
      outer_fold_id = folds,
      holdout_id = paste0("species_fold_", folds),
      anchor_species = species,
      anchor_model_id = seq_along(species),
      error_abs_log = baseline_error
    ),
    tibble::tibble(
      scenario = "without_depth",
      scenario_type = "trait_ablation",
      ablated_traits = "depth",
      outer_fold_id = folds,
      holdout_id = paste0("species_fold_", folds),
      anchor_species = species,
      anchor_model_id = seq_along(species),
      error_abs_log = baseline_error + c(0.04, 0.05, 0.06, 0.07, 0.05, 0.06, 0.07, 0.08)
    ),
    tibble::tibble(
      scenario = "without_family",
      scenario_type = "trait_ablation",
      ablated_traits = "family",
      outer_fold_id = folds,
      holdout_id = paste0("species_fold_", folds),
      anchor_species = species,
      anchor_model_id = seq_along(species),
      error_abs_log = baseline_error + c(-0.01, 0.00, 0.01, 0.00, -0.01, 0.00, 0.01, 0.00)
    )
  )

  scorecard <- summarize_sentinel_ablation(
    results,
    bootstrap = TRUE,
    cluster_names = "anchor_species",
    strata_names = "outer_fold_id",
    bootstrap_method = "studentized",
    bootstrap_adjustment = "max_t",
    n_realizations = 200L,
    seed = 12L
  )
  cards <- scorecard@recommendation_cards
  depth <- dplyr::filter(cards, .data$scenario == "without_depth")

  expect_true(all(cards$bootstrap))
  expect_true(all(cards$bootstrap_method == "studentized"))
  expect_true(all(cards$bootstrap_adjustment == "max_t"))
  expect_true(all(cards$n_realizations == 200L))
  expect_true(all(cards$confidence_level == 0.95))
  expect_false("alpha_level" %in% names(cards))
  expect_true(all(cards$n_clusters == 8L))
  expect_true(all(is.finite(cards$conf_low)))
  expect_true(all(is.finite(cards$conf_high)))
  expect_true(all(is.finite(cards$p_adjusted)))
  expect_equal(depth$importance, 0.06)
  expect_true(depth$interval_excludes_zero)
  ablation_plot <- plot(scorecard, type = "ablation")
  built_plot <- ggplot2::ggplot_build(ablation_plot)
  plotted_colors <- unique(unlist(lapply(built_plot$data, function(layer) layer$colour %||% character(0))))
  expect_true("palegreen3" %in% plotted_colors)
  expect_true("azure3" %in% plotted_colors)
  display_labels <- levels(ablation_plot$layers[[2]]$data$display_label)
  expect_true(all(substr(display_labels, 1L, 1L) == toupper(substr(display_labels, 1L, 1L))))

  expect_error(
    summarize_sentinel_ablation(
      results,
      bootstrap = TRUE,
      cluster_names = "anchor_species",
      bootstrap_method = "bca",
      bootstrap_adjustment = "max_t"
    ),
    "requires.*studentized"
  )
  expect_error(
    summarize_sentinel_ablation(
      results,
      bootstrap = TRUE,
      cluster_names = "missing_cluster"
    ),
    "Cluster column"
  )
  expect_error(
    summarize_sentinel_ablation(
      results,
      bootstrap = TRUE,
      cluster_names = "anchor_species",
      n_realizations = 1L
    ),
    "integer >= 2"
  )

  binary_results <- results |>
    dplyr::mutate(covered = TRUE)
  binary_scorecard <- summarize_sentinel_ablation(
    binary_results,
    metric = "covered",
    bootstrap = TRUE,
    cluster_names = "anchor_species",
    bootstrap_method = "studentized",
    bootstrap_adjustment = "max_t",
    n_realizations = 50L,
    seed = 13L
  )
  expect_true(all(binary_scorecard@recommendation_cards$inference_status == "degenerate_zero"))
  expect_true(all(binary_scorecard@recommendation_cards$conf_low == 0))
  expect_true(all(binary_scorecard@recommendation_cards$conf_high == 0))
  expect_true(all(binary_scorecard@recommendation_cards$p_adjusted == 1))
})

test_that("Sentinel run persists fold outputs and supports resume/collect", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2", "m3"),
    species_name = c("sp1", "sp1", "sp2"),
    study_reference_id = c("study_a", "study_b", "study_c"),
    study_cell_id = c("cell_1", "cell_2", "cell_3"),
    score = c(10, 20, 30)
  )

  workflow_fn <- function(train_data, test_data, params, config, manifest_row, sentinel) {
    list(
      metrics = tibble::tibble(
        train_n = nrow(train_data),
        test_n = nrow(test_data),
        score_delta = mean(test_data$score) - mean(train_data$score)
      ),
      artifacts = list(
        holdout = manifest_row$holdout_id[[1]],
        params = params
      )
    )
  }

  out_dir <- file.path(tempdir(), paste0("sentinel-run-", as.integer(Sys.time())))
  sentinel <- build_sentinel(
    data = candidate_models,
    workflow_fn = workflow_fn,
    split_mode = "anchor_row_holdout",
    output_dir = out_dir,
    case_studies = "m2"
  )
  expect_identical(sentinel@workflow_fn, workflow_fn)

  sentinel <- run_sentinel(sentinel, max_folds = 2L)
  expect_equal(sum(sentinel@manifest$status == "completed"), 2L)
  expect_equal(nrow(sentinel@results), 2L)

  collected <- collect_sentinel_results(sentinel)
  expect_equal(nrow(collected), 2L)
  expect_true(all(c("fold_id", "scenario", "holdout_id", "train_n", "test_n") %in% names(collected)))

  validation_card <- summary(
    sentinel,
    type = "validation",
    metric_cols = c("train_n", "test_n", "score_delta")
  )
  expect_true(S7::S7_inherits(validation_card, Scorecard))
  expect_s3_class(
    plot(validation_card, type = "validation", metric = "score_delta"),
    "ggplot"
  )

  resumed <- resume_sentinel(sentinel_rebuild(sentinel, manifest = tibble::tibble(), results = tibble::tibble()))
  expect_equal(sum(resumed@manifest$status == "completed"), 2L)
  expect_equal(nrow(resumed@results), 2L)

  artifact_path <- resumed@manifest |>
    dplyr::filter(.data$holdout_id == "m2") |>
    dplyr::pull(.data$artifact_file)
  expect_true(length(artifact_path) == 1L)
  expect_true(file.exists(artifact_path))
})

test_that("Sentinel folds can be externally orchestrated and combined", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2", "m3"),
    species_name = c("sp1", "sp1", "sp2"),
    study_reference_id = c("study_a", "study_b", "study_c"),
    study_cell_id = c("cell_1", "cell_2", "cell_3"),
    score = c(10, 20, 30)
  )

  workflow_fn <- function(train_data, test_data, params, config, manifest_row, sentinel) {
    list(metrics = tibble::tibble(
      train_n = nrow(train_data),
      test_n = nrow(test_data),
      score_delta = mean(test_data$score) - mean(train_data$score)
    ))
  }

  sentinel <- build_sentinel(
    data = candidate_models,
    workflow_fn = workflow_fn,
    split_mode = "anchor_row_holdout",
    output_dir = file.path(tempdir(), paste0("sentinel-fold-bridge-", as.integer(Sys.time())))
  )
  sentinel <- tsbiomass:::build_sentinel_manifest(sentinel)
  fold_outputs <- lapply(seq_len(2L), function(i) {
    run_sentinel_fold(sentinel, sentinel@manifest[i, , drop = FALSE])
  })
  expect_true(all(vapply(fold_outputs, `[[`, logical(1), "ok")))

  combined <- combine_sentinel_folds(sentinel, fold_outputs)
  expect_equal(sum(combined@manifest$status == "completed"), 2L)
  expect_equal(nrow(combined@results), 2L)
  expect_true(all(file.exists(combined@results$summary_file)))

  validation_card <- summary(
    combined,
    type = "validation",
    metric_cols = c("train_n", "test_n", "score_delta")
  )
  expect_true(S7::S7_inherits(validation_card, Scorecard))
})

test_that("Sentinel object workflow receives fold Candidates and Configurer", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2", "m3"),
    species_name = c("sp1", "sp2", "sp3"),
    class = c("Actinopterygii", "Actinopterygii", "Actinopterygii"),
    fao_area = c("27", "67", "77"),
    study_reference_id = c("study_a", "study_b", "study_c"),
    study_cell_id = c("cell_1", "cell_2", "cell_3")
  )
  config_s7 <- build_configurer(create_configuration_template(
    input_file = file.path(tempdir(), "input.xlsx"),
    output_root = file.path(tempdir(), "outputs"),
    cache_folder = file.path(tempdir(), "cache")
  ))

  workflow_fun <- function(candidates, workflow_config_s7) {
    expect_true(S7::S7_inherits(candidates, Candidates))
    expect_true(S7::S7_inherits(workflow_config_s7, Configurer))
    expect_equal(nrow(candidates@candidate_models), 2L)
    expect_equal(nrow(candidates@reference_anchors), 1L)
    expect_false(any(
      candidates@candidate_models$model_id_chr %in%
        candidates@reference_anchors$model_id_chr
    ))

    list(metrics = tibble::tibble(
      train_n = nrow(candidates@candidate_models),
      test_n = nrow(candidates@reference_anchors)
    ))
  }

  sentinel <- build_sentinel(
    data = candidate_models,
    workflow_fn = workflow_fun,
    config = config_s7,
    split_mode = "anchor_row_holdout",
    output_dir = file.path(tempdir(), paste0("sentinel-object-contract-", as.integer(Sys.time())))
  )
  sentinel <- run_sentinel(sentinel, max_folds = 1L)
  result <- collect_sentinel_results(sentinel)

  expect_equal(result$train_n, 2L)
  expect_equal(result$test_n, 1L)
})

test_that("Sentinel object workflow can return a Scorecard directly", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2"),
    species_name = c("sp1", "sp2"),
    class = c("Actinopterygii", "Actinopterygii"),
    fao_area = c("27", "67"),
    study_reference_id = c("study_a", "study_b"),
    study_cell_id = c("cell_1", "cell_2")
  )
  config_s7 <- build_configurer(create_configuration_template(
    input_file = file.path(tempdir(), "input.xlsx"),
    output_root = file.path(tempdir(), "outputs"),
    cache_folder = file.path(tempdir(), "cache")
  ))

  workflow_fun <- function(candidates, workflow_config_s7) {
    scorecard <- tsbiomass:::empty_scorecard()
    scorecard@selected <- tibble::tibble(
      anchor_model_id = candidates@reference_anchors$model_id_chr,
      anchor_species = candidates@reference_anchors$species_name,
      policy = "closest",
      multiplier_pred = 1.1,
      multiplier_lo = 0.9,
      multiplier_hi = 1.3,
      valid_prediction = TRUE
    )
    scorecard@intervals <- scorecard@selected
    scorecard
  }

  sentinel <- build_sentinel(
    data = candidate_models,
    workflow_fn = workflow_fun,
    config = config_s7,
    split_mode = "anchor_row_holdout",
    output_dir = file.path(tempdir(), paste0("sentinel-scorecard-contract-", as.integer(Sys.time())))
  )
  sentinel <- run_sentinel(sentinel, max_folds = 1L)
  result <- collect_sentinel_results(sentinel)

  expect_equal(nrow(result), 1L)
  expect_true(is.finite(result$error_abs_log))
})

test_that("Sentinel patches fold-local cache paths into workflow config", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2", "m3"),
    species_name = c("sp1", "sp1", "sp2"),
    study_reference_id = c("study_a", "study_b", "study_c"),
    study_cell_id = c("cell_1", "cell_2", "cell_3")
  )

  workflow_fn <- function(train_data, test_data, params, config, manifest_row, sentinel) {
    list(
      metrics = tibble::tibble(
        train_n = nrow(train_data),
        test_n = nrow(test_data),
        cache_dir = as.character(config$paths$cache_dir %||% NA_character_),
        benchmark_cache = as.character(config$benchmark$cache_path %||% NA_character_),
        manifest_cache_dir = as.character(manifest_row$cache_dir[[1]] %||% NA_character_)
      )
    )
  }

  out_dir <- file.path(tempdir(), paste0("sentinel-cache-", as.integer(Sys.time())))
  sentinel <- build_sentinel(
    data = candidate_models,
    workflow_fn = workflow_fn,
    split_mode = "anchor_row_holdout",
    output_dir = out_dir
  )

  sentinel <- run_sentinel(sentinel, max_folds = 1L)
  collected <- collect_sentinel_results(sentinel)

  expect_equal(nrow(collected), 1L)
  expect_true(nzchar(collected$cache_dir[[1]]))
  expect_equal(collected$cache_dir[[1]], collected$manifest_cache_dir[[1]])
  expect_true(grepl("fold_cache", collected$cache_dir[[1]], fixed = TRUE))
  expect_true(grepl(collected$cache_dir[[1]], collected$benchmark_cache[[1]], fixed = TRUE))
})

test_that("Sentinel can derive metrics from selected and interval tables", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2"),
    species_name = c("sp1", "sp2"),
    study_reference_id = c("study_a", "study_b"),
    study_cell_id = c("cell_1", "cell_2")
  )

  workflow_fn <- function(train_data, test_data, params, config, manifest_row, sentinel) {
    selected_tbl <- tibble::tibble(
      anchor_model_id = test_data$model_id_chr,
      anchor_species = test_data$species_name,
      policy = "p1",
      multiplier_pred = 1.10,
      multiplier_lo = 0.95,
      multiplier_hi = 1.25,
      valid_prediction = TRUE
    )
    intervals_tbl <- tibble::tibble(
      anchor_model_id = test_data$model_id_chr,
      policy = c("p1"),
      multiplier_pred = 1.10,
      valid_prediction = TRUE
    )

    list(
      selected = selected_tbl,
      intervals = intervals_tbl
    )
  }

  out_dir <- file.path(tempdir(), paste0("sentinel-derived-", as.integer(Sys.time())))
  sentinel <- build_sentinel(
    data = candidate_models,
    workflow_fn = workflow_fn,
    split_mode = "anchor_row_holdout",
    output_dir = out_dir,
    case_studies = "m1"
  )

  sentinel <- run_sentinel(sentinel, max_folds = 1L)
  collected <- collect_sentinel_results(sentinel)

  expect_true(all(c(
    "error_abs_log",
    "interval_log_width",
    "covered",
    "selection_regret_abs_log"
  ) %in% names(collected)))

  artifact_path <- sentinel@manifest |>
    dplyr::filter(.data$holdout_id == "m1") |>
    dplyr::pull(.data$artifact_file)
  artifact_bundle <- readRDS(artifact_path[[1]])

  expect_true(all(c("metrics", "selected", "intervals", "artifacts") %in% names(artifact_bundle)))
  expect_equal(nrow(artifact_bundle$selected), 1L)
})

test_that("Sentinel prunes dropped trait columns from config sections", {
  cfg <- list(
    similarity = list(
      species_traits = c("family", "genus", "species_name"),
      study_traits = c("study_reference_id", "study_cell_id")
    ),
    alchemist = list(
      species_traits = c("family", "genus"),
      study_traits = c("study_reference_id")
    ),
    selection = list(
      feature_cols = c("family", "depth", "study_cell_id")
    )
  )

  out <- sentinel_prune_trait_config(
    config = cfg,
    drop_columns = c("family", "study_cell_id")
  )

  expect_false("family" %in% out$similarity$species_traits)
  expect_false("study_cell_id" %in% out$similarity$study_traits)
  expect_false("family" %in% out$alchemist$species_traits)
  expect_false("family" %in% out$selection$feature_cols)
})

test_that("Sentinel trait pruning drops unavailable fold columns", {
  cfg <- list(
    similarity = list(
      species_traits = c("family", "genus"),
      study_traits = c("study_reference_id", "fao_area")
    ),
    selection = list(
      feature_cols = c("family", "depth", "study_cell_id")
    )
  )

  out <- sentinel_prune_trait_config(
    config = cfg,
    available_columns = c("genus", "study_reference_id", "depth")
  )

  expect_equal(out$similarity$species_traits, "genus")
  expect_equal(out$similarity$study_traits, "study_reference_id")
  expect_equal(out$selection$feature_cols, "depth")
})

test_that("Sentinel similarity ablation retains source columns and policy groups", {
  cfg <- list(
    similarity = list(
      species_traits = c("species", "ocean_basin"),
      study_traits = "season"
    ),
    alchemist = list(
      species_traits = c("species", "ocean_basin"),
      study_traits = "season"
    ),
    policies = list(
      group = list(
        species = list(joint = list("ocean_basin")),
        ocean_basin = NULL,
        all = NULL
      )
    )
  )

  out <- sentinel_exclude_similarity_traits(
    config = cfg,
    traits = "ocean_basin"
  )

  expect_equal(out$similarity$species_traits, "species")
  expect_equal(out$alchemist$species_traits, "species")
  expect_identical(out$policies, cfg$policies)

  data <- tibble::tibble(
    model_id = c("m1", "m2"),
    species = c("sp1", "sp2"),
    ocean_basin = c("pacific", "atlantic")
  )
  dropped <- sentinel_apply_drop_columns(
    train_data = data,
    test_data = data,
    drop_columns = NULL
  )
  expect_true("ocean_basin" %in% names(dropped$train_data))
  expect_true("ocean_basin" %in% names(dropped$test_data))
})

test_that("Sentinel-built candidates retain fold config data", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2"),
    species_name = c("sp1", "sp2"),
    study_reference_id = c("study_a", "study_b"),
    genus = c("g1", "g2"),
    family = c("f1", "f2")
  )
  cfg <- list(
    similarity = list(
      species_traits = list(genus = 1, family = 1)
    )
  )

  candidates <- tsbiomass:::sentinel_build_candidates(
    candidate_models = candidate_models,
    config = cfg
  )

  expect_equal(
    names(tsbiomass:::candidates_configuration(candidates)$similarity$species_traits),
    c("genus", "family")
  )
})

test_that("Sentinel candidate build tolerates unmatched anchor selectors", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2"),
    species_name = c("sp1", "sp2"),
    regional_body = c("A", "B")
  )

  candidates <- tsbiomass:::sentinel_build_candidates(
    candidate_models = candidate_models,
    config = list(),
    anchor_selector = list(regional_body = "SWFSC")
  )

  expect_s3_class(tibble::as_tibble(candidates@reference_anchors), "tbl_df")
  expect_equal(nrow(candidates@reference_anchors), 0L)
})

test_that("Reduced Sentinel fixture keeps anchor species and top extra species", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:8),
    species_name = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp3", "sp4"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC"),
    study_reference_id = paste0("study_", 1:8)
  )

  out <- build_reduced_validation_fixture(
    data = candidate_models,
    config = list(
      candidates = list(
        anchors = list(
          selector = list(regional_body = "SWFSC")
        )
      )
    ),
    extra_species = 1L
  )

  expect_setequal(unique(out$species_name), c("sp1", "sp3"))
  expect_equal(nrow(out), 5L)
})

test_that("Reduced Sentinel fixture drops blank species labels when requested", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:5),
    species_name = c("sp1", " ", NA, "sp2", "sp2"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "AFSC", "AFSC"),
    study_reference_id = paste0("study_", 1:5)
  )

  out <- build_reduced_validation_fixture(
    data = candidate_models,
    config = list(
      candidates = list(
        anchors = list(
          selector = list(regional_body = "SWFSC")
        )
      )
    ),
    extra_species = 1L,
    drop_missing_species = TRUE
  )

  expect_false(any(is.na(out$species_name)))
  expect_false(any(trimws(out$species_name) == ""))
  expect_setequal(unique(out$species_name), c("sp1", "sp2"))
})

test_that("Sentinel selected metrics compute error, coverage, and regret", {
  selected_tbl <- tibble::tibble(
    anchor_model_id = c("a1", "a2"),
    anchor_species = c("sp1", "sp2"),
    policy = c("p1", "p2"),
    multiplier_pred = c(1.10, 0.80),
    multiplier_lo = c(0.95, 0.60),
    multiplier_hi = c(1.28, 1.05),
    valid_prediction = c(TRUE, TRUE)
  )
  intervals_tbl <- tibble::tibble(
    anchor_model_id = c("a1", "a1", "a2", "a2"),
    policy = c("p1", "p3", "p2", "p4"),
    multiplier_pred = c(1.10, 1.03, 0.80, 0.78),
    valid_prediction = c(TRUE, TRUE, TRUE, TRUE)
  )

  out <- sentinel_selected_metrics(selected_tbl, intervals_tbl)

  expect_equal(nrow(out), 2L)
  expect_true(all(c("error_abs_log", "interval_log_width", "covered", "oracle_abs_log_error", "selection_regret_abs_log") %in% names(out)))
  expect_true(all(is.finite(out$error_abs_log)))
  expect_true(all(out$selection_regret_abs_log >= -1e-12, na.rm = TRUE))
})

test_that("Sentinel deployment targets map to the expected split modes", {
  expect_equal(sentinel_target_spec("seen_species_new_row")$split_mode, "anchor_row_holdout")
  expect_equal(sentinel_target_spec("seen_species_new_study")$split_mode, "study_holdout")
  expect_equal(sentinel_target_spec("seen_species_new_study_cell")$split_mode, "study_cell_holdout")
  expect_equal(sentinel_target_spec("cold_start_species")$split_mode, "species_holdout")
})

test_that("Sentinel can resolve deployment targets directly at construction", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2", "m3"),
    species_name = c("sp1", "sp1", "sp2"),
    study_reference_id = c("study_a", "study_b", "study_c"),
    study_cell_id = c("cell_1", "cell_2", "cell_3")
  )

  sentinel <- build_sentinel(
    data = candidate_models,
    deployment_target = "cold_start_species",
    output_dir = file.path(tempdir(), paste0("sentinel-target-", as.integer(Sys.time())))
  )
  sentinel <- build_sentinel_manifest(sentinel)

  expect_equal(sentinel@split_mode, "species_holdout")
  expect_equal(sentinel@split_col, "species_name")
  expect_true(all(sentinel@manifest$deployment_target == "cold_start_species"))
})

test_that("Sentinel infers strict Alchemist OOF mode for species holdout", {
  expect_equal(
    tsbiomass:::sentinel_alchemist_oof_mode(list(), split_mode = "species_holdout"),
    "species_purged"
  )
  expect_equal(
    tsbiomass:::sentinel_alchemist_oof_mode(list(), split_mode = "anchor_row_holdout"),
    "anchor_species"
  )
  expect_equal(
    tsbiomass:::sentinel_alchemist_oof_mode(
      list(alchemist = list(learner = list(oof_mode = "anchor_species"))),
      split_mode = "species_holdout"
    ),
    "anchor_species"
  )
})

test_that("Sentinel patches fold config to the implied Stage 1 OOF mode", {
  cold_cfg <- sentinel_patch_fold_config(
    config = list(),
    split_mode = "species_holdout"
  )
  row_cfg <- sentinel_patch_fold_config(
    config = list(),
    split_mode = "anchor_row_holdout"
  )
  explicit_cfg <- sentinel_patch_fold_config(
    config = list(alchemist = list(learner = list(oof_mode = "anchor_species"))),
    split_mode = "species_holdout"
  )

  expect_equal(cold_cfg$alchemist$learner$oof_mode, "species_purged")
  expect_equal(row_cfg$alchemist$learner$oof_mode, "anchor_species")
  expect_equal(explicit_cfg$alchemist$learner$oof_mode, "anchor_species")
})

test_that("Sentinel can throttle inner workers and apply fast ordination settings", {
  sentinel <- build_sentinel(
    data = tibble::tibble(
      model_id_chr = c("m1", "m2"),
      species_name = c("sp1", "sp2"),
      study_reference_id = c("study_a", "study_b"),
      study_cell_id = c("cell_1", "cell_2")
    ),
    output_dir = file.path(tempdir(), paste0("sentinel-patch-", as.integer(Sys.time()))),
    options = list(
      workers = 3L,
      throttle_inner_workers = TRUE,
      fast_validation = TRUE,
      fast_nmds_args = list(try = 2, trymax = 4)
    )
  )

  manifest_row <- tibble::tibble(
    cache_dir = file.path(tempdir(), "sentinel-fold-cache")
  )
  cfg <- sentinel_patch_fold_config(
    config = list(
      benchmark = list(workers = 8L),
      selection = list(
        workers = 5L,
        method_settings = list(xgboost = list(nthread = 7L))
      ),
      uncertainty = list(
        workers = 6L,
        method_settings = list(xgboost = list(nthread = 6L))
      ),
      simulation = list(workers = 4L),
      alchemist = list(
        learner = list(workers = 6L),
        distill_workers = 7L
      ),
      ordination = list(
        nmds_args = list(try = 9L, trymax = 12L)
      )
    ),
    split_mode = "species_holdout",
    manifest_row = manifest_row,
    object = sentinel
  )

  expect_equal(cfg$benchmark$workers, 1L)
  expect_false(cfg$benchmark$include_ts_error)
  expect_equal(cfg$selection$workers, 1L)
  expect_equal(cfg$selection$method_settings$xgboost$nthread, 1L)
  expect_equal(cfg$uncertainty$workers, 1L)
  expect_equal(cfg$uncertainty$method_settings$xgboost$nthread, 1L)
  expect_equal(cfg$simulation$workers, 1L)
  expect_equal(cfg$alchemist$learner$workers, 1L)
  expect_equal(cfg$alchemist$distill_workers, 1L)
  expect_equal(cfg$ordination$nmds_args$try, 2)
  expect_equal(cfg$ordination$nmds_args$trymax, 4)
  expect_true(grepl("sentinel-fold-cache", cfg$paths$cache_dir, fixed = TRUE))
})

test_that("Sentinel reads TS-error control from workflow config independently", {
  sentinel <- build_sentinel(
    data = tibble::tibble(
      model_id_chr = c("m1", "m2"),
      species_name = c("sp1", "sp2")
    ),
    config = list(
      sentinel = list(
        include_ts_error = FALSE,
        fast_validation = FALSE
      )
    ),
    output_dir = file.path(tempdir(), paste0("sentinel-ts-config-", as.integer(Sys.time())))
  )

  cfg <- sentinel_patch_fold_config(
    config = list(
      benchmark = list(include_ts_error = TRUE),
      ordination = list(nmds_args = list(try = 9L, trymax = 12L))
    ),
    split_mode = "species_holdout",
    object = sentinel
  )

  expect_false(sentinel@options$include_ts_error)
  expect_false(cfg$benchmark$include_ts_error)
  expect_equal(cfg$ordination$nmds_args$try, 9L)
  expect_equal(cfg$ordination$nmds_args$trymax, 12L)
})

test_that("Sentinel species holdout purges held-out species from all configured roles", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2", "m3", "m4"),
    species_name = c("sp1", "sp2", "sp3", "sp4"),
    anchor_species = c("sp1", "sp2", "sp3", "sp4"),
    donor_species = c("sp2", "sp1", "sp4", "sp3"),
    study_reference_id = c("study_a", "study_b", "study_c", "study_d")
  )

  split <- tsbiomass:::sentinel_partition_data(
    data = candidate_models,
    split_col = "species_name",
    holdout_id = "sp1",
    split_mode = "species_holdout"
  )

  expect_equal(as.character(split$test_data$species_name), "sp1")
  expect_false(any(split$train_data$species_name == "sp1"))
  expect_false(any(split$train_data$anchor_species == "sp1"))
  expect_false(any(split$train_data$donor_species == "sp1"))
})

test_that("Sentinel split plan precomputes species holdout row membership", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2", "m3", "m4"),
    species_name = c("sp1", "sp2", "sp3", "sp4"),
    anchor_species = c("sp1", "sp2", "sp3", "sp4"),
    donor_species = c("sp2", "sp1", "sp4", "sp3"),
    study_reference_id = c("study_a", "study_b", "study_c", "study_d")
  )

  split_plan <- tsbiomass:::sentinel_split_plan(
    data = candidate_models,
    split_col = "species_name",
    split_mode = "species_holdout"
  )
  split <- tsbiomass:::sentinel_partition_data(
    data = candidate_models,
    split_col = "species_name",
    holdout_id = "sp1",
    split_mode = "species_holdout",
    split_plan = split_plan
  )

  expect_equal(split_plan$holdout_n[["sp1"]], 1L)
  expect_equal(as.character(split$test_data$species_name), "sp1")
  expect_false(any(split$train_data$species_name == "sp1"))
  expect_false(any(split$train_data$anchor_species == "sp1"))
  expect_false(any(split$train_data$donor_species == "sp1"))
})

test_that("Sentinel species folds exclude generalized equation placeholders", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2", "g1", "g2"),
    species_name = c("sp1", "sp2", "NA NA", "NA NA")
  )

  split_plan <- tsbiomass:::sentinel_split_plan(
    data = candidate_models,
    split_col = "species_name",
    split_mode = "species_holdout"
  )

  expect_setequal(split_plan$holdout_ids, c("sp1", "sp2"))
  expect_false("NA NA" %in% split_plan$holdout_ids)
  expect_true(all(vapply(
    split_plan$train_indices,
    function(idx) all(c(3L, 4L) %in% idx),
    logical(1)
  )))
})

test_that("Sentinel species holdout supports deterministic grouped K-fold", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", seq_len(12L)),
    species_name = rep(paste0("sp", seq_len(6L)), each = 2L),
    anchor_species = rep(paste0("sp", seq_len(6L)), each = 2L),
    donor_species = rep(paste0("sp", seq_len(6L)), each = 2L)
  )

  split_plan <- tsbiomass:::sentinel_split_plan(
    data = candidate_models,
    split_col = "species_name",
    split_mode = "species_holdout",
    options = list(species_folds = 3L, seed = 42L)
  )

  expect_equal(length(split_plan$holdout_ids), 3L)
  expect_setequal(unlist(split_plan$holdout_groups), paste0("sp", seq_len(6L)))
  expect_true(all(lengths(split_plan$holdout_groups) == 2L))

  for (holdout_id in split_plan$holdout_ids) {
    heldout_species <- split_plan$holdout_groups[[holdout_id]]
    split <- tsbiomass:::sentinel_partition_data(
      data = candidate_models,
      split_col = "species_name",
      holdout_id = holdout_id,
      split_mode = "species_holdout",
      options = list(species_folds = 3L, seed = 42L),
      split_plan = split_plan
    )
    expect_setequal(unique(split$test_data$species_name), heldout_species)
    expect_false(any(split$train_data$species_name %in% heldout_species))
    expect_false(any(split$train_data$anchor_species %in% heldout_species))
    expect_false(any(split$train_data$donor_species %in% heldout_species))
  }
})

test_that("Sentinel can drop action-space rows from both train and test slices", {
  train_tbl <- tibble::tibble(
    policy = c("p1", "p2", "p3"),
    value = c(1, 2, 3)
  )
  test_tbl <- tibble::tibble(
    policy = c("p2", "p4"),
    value = c(4, 5)
  )

  out <- sentinel_apply_drop_rows(
    train_data = train_tbl,
    test_data = test_tbl,
    drop_rows = list(policy = "p2")
  )

  expect_false(any(out$train_data$policy == "p2"))
  expect_false(any(out$test_data$policy == "p2"))
  expect_equal(nrow(out$train_data), 2L)
  expect_equal(nrow(out$test_data), 1L)
})

test_that("Reduced Sentinel builder reduces candidate rows before construction", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:8),
    species_name = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp3", "sp4"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC"),
    study_reference_id = paste0("study_", 1:8),
    study_cell_id = paste0("cell_", 1:8)
  )

  sentinel <- build_reduced_sentinel(
    data = candidate_models,
    workflow_fn = function(train_data, test_data, params, config, manifest_row, sentinel) {
      list(metrics = tibble::tibble(train_n = nrow(train_data), test_n = nrow(test_data)))
    },
    config = list(
      candidates = list(
        anchors = list(
          selector = list(regional_body = "SWFSC")
        )
      )
    ),
    split_mode = "study_holdout",
    extra_species = 1L,
    output_dir = file.path(tempdir(), paste0("sentinel-reduced-", as.integer(Sys.time())))
  )

  expect_equal(sentinel@split_mode, "study_holdout")
  expect_setequal(unique(sentinel@data$species_name), c("sp1", "sp3"))
  expect_equal(nrow(sentinel@data), 5L)
})

test_that("Reduced Sentinel runner can build the reduced path without executing folds", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:8),
    species_name = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp3", "sp4"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC"),
    study_reference_id = paste0("study_", 1:8),
    study_cell_id = paste0("cell_", 1:8)
  )

  sentinel <- run_reduced_sentinel(
    data = candidate_models,
    workflow_fn = function(train_data, test_data, params, config, manifest_row, sentinel) {
      list(metrics = tibble::tibble(train_n = nrow(train_data), test_n = nrow(test_data)))
    },
    config = list(
      candidates = list(
        anchors = list(
          selector = list(regional_body = "SWFSC")
        )
      )
    ),
    split_mode = "study_cell_holdout",
    extra_species = 1L,
    output_dir = file.path(tempdir(), paste0("sentinel-run-reduced-", as.integer(Sys.time()))),
    max_folds = 0L
  )

  expect_true(inherits(sentinel, "S7_object"))
  expect_true(S7::S7_inherits(sentinel, Sentinel))
  expect_equal(sentinel@split_mode, "study_cell_holdout")
  expect_equal(sum(sentinel@manifest$status == "completed"), 0L)
  expect_true(nrow(sentinel@manifest) > 0L)
})

test_that("Reduced Sentinel suite runs all requested split specs in no-op mode", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:10),
    species_name = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp3", "sp4", "sp4", "sp5"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC"),
    study_reference_id = paste0("study_", c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
    study_cell_id = paste0("cell_", 1:10)
  )

  suite <- run_reduced_sentinel_suite(
    data = candidate_models,
    workflow_fn = function(train_data, test_data, params, config, manifest_row, sentinel) {
      list(metrics = tibble::tibble(train_n = nrow(train_data), test_n = nrow(test_data)))
    },
    config = list(
      candidates = list(
        anchors = list(
          selector = list(regional_body = "SWFSC")
        )
      )
    ),
    split_specs = list(
      anchor_row = list(split_mode = "anchor_row_holdout"),
      study = list(split_mode = "study_holdout")
    ),
    extra_species = 1L,
    output_dir = file.path(tempdir(), paste0("sentinel-reduced-suite-", as.integer(Sys.time()))),
    max_folds = 0L
  )

  expect_true(is.list(suite))
  expect_setequal(names(suite), c("anchor_row", "study"))
  expect_true(all(vapply(suite, function(x) inherits(x, "S7_object"), logical(1))))
  expect_true(all(vapply(suite, function(x) S7::S7_inherits(x, Sentinel), logical(1))))
  expect_true(all(vapply(suite, function(x) nrow(x@manifest) > 0L, logical(1))))
  expect_true(all(vapply(suite, function(x) sum(x@manifest$status == "completed") == 0L, logical(1))))
})

test_that("Sentinel suite can run generic species holdout and scenario stress workflows", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:8),
    species_name = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp4", "sp4"),
    anchor_species = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp4", "sp4"),
    donor_species = c("sp2", "sp3", "sp1", "sp4", "sp1", "sp2", "sp2", "sp3"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC"),
    study_reference_id = c("study_a", "study_a", "study_b", "study_c", "study_d", "study_e", "study_f", "study_g"),
    study_cell_id = paste0("cell_", 1:8),
    keep_col = seq_len(8),
    drop_me = seq_len(8) * 10
  )

  workflow_fn <- function(train_data, test_data, params, config, manifest_row, sentinel) {
    holdout_species <- as.character(manifest_row$holdout_id[[1]])
    list(
      metrics = tibble::tibble(
        train_n = nrow(train_data),
        test_n = nrow(test_data),
        holdout_id = holdout_species,
        deployment_target = as.character(params$deployment_target %||% NA_character_),
        species_purged = !any(train_data$species_name == holdout_species) &&
          !any(train_data$anchor_species == holdout_species) &&
          !any(train_data$donor_species == holdout_species),
        dropped_column_absent = !"drop_me" %in% names(train_data),
        action_drop_applied = !any(train_data$donor_species == "sp3") &&
          !any(test_data$donor_species == "sp3"),
        schema_flag_present = "schema_flag" %in% names(train_data),
        fold_oof_mode = as.character(config$alchemist$learner$oof_mode %||% NA_character_)
      )
    )
  }

  scenarios <- list(
    baseline = list(),
    no_drop_me = list(drop_columns = "drop_me"),
    no_policy_spread = list(drop_rows = list(donor_species = "sp3")),
    schema_flag = list(
      prepare = function(train_data, test_data, config, scenario_spec, manifest_row, object) {
        train_data$schema_flag <- "train"
        test_data$schema_flag <- "test"
        list(train_data = train_data, test_data = test_data, config = config)
      }
    )
  )

  suite <- run_reduced_sentinel_suite(
    data = candidate_models,
    workflow_fn = workflow_fn,
    config = list(
      candidates = list(
        anchors = list(
          selector = list(regional_body = "SWFSC")
        )
      )
    ),
    split_specs = list(
      cold_start = list(
        deployment_target = "cold_start_species",
        split_mode = "species_holdout",
        scenario_grid = scenarios
      )
    ),
    extra_species = 1L,
    output_dir = file.path(tempdir(), paste0("sentinel-reduced-suite-run-", as.integer(Sys.time()))),
    max_folds = 3L
  )

  collected <- collect_sentinel_suite_results(suite)

  expect_true(nrow(collected) >= 3L)
  expect_true(all(collected$suite_name == "cold_start"))
  expect_true(all(collected$suite_split_mode == "species_holdout"))
  expect_true(all(collected$deployment_target == "cold_start_species"))

  baseline_rows <- collected |> dplyr::filter(.data$scenario == "baseline")
  dropped_rows <- collected |> dplyr::filter(.data$scenario == "no_drop_me")
  action_rows <- collected |> dplyr::filter(.data$scenario == "no_policy_spread")
  schema_rows <- collected |> dplyr::filter(.data$scenario == "schema_flag")

  expect_true(all(baseline_rows$species_purged))
  expect_true(all(baseline_rows$fold_oof_mode == "species_purged"))
  expect_true(all(dropped_rows$dropped_column_absent))
  expect_true(all(action_rows$action_drop_applied))
  expect_true(all(schema_rows$schema_flag_present))
})

test_that("Sentinel writes isolated fold logs beneath the configured cache", {
  root <- file.path(tempdir(), paste0("sentinel-logging-", as.integer(Sys.time())))
  configured_cache <- file.path(root, "configured-cache")
  explicit_cache <- file.path(root, "explicit-cache")
  output_dir <- file.path(root, "validation")
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2", "m3", "m4"),
    species_name = c("sp1", "sp1", "sp2", "sp2")
  )
  workflow_fn <- function(train_data, test_data, params, config, manifest_row, sentinel) {
    message("fold-message")
    cat("fold-output\n")
    list(metrics = tibble::tibble(train_n = nrow(train_data), test_n = nrow(test_data)))
  }

  configured <- build_sentinel(
    data = candidate_models,
    workflow_fn = workflow_fn,
    config = list(
      paths = list(cache_dir = configured_cache),
      execution = list(progress = FALSE),
      sentinel = list(logging = TRUE)
    ),
    split_mode = "species_holdout",
    output_dir = output_dir
  )
  expect_true(startsWith(
    tsbiomass:::sentinel_output_paths(configured)$cache_root_dir,
    normalizePath(configured_cache, winslash = "/", mustWork = FALSE)
  ))

  sentinel <- build_sentinel(
    data = candidate_models,
    workflow_fn = workflow_fn,
    config = list(paths = list(cache_dir = configured_cache)),
    split_mode = "species_holdout",
    output_dir = output_dir,
    cache_dir = explicit_cache,
    logging = TRUE
  )
  expect_true(startsWith(
    tsbiomass:::sentinel_output_paths(sentinel)$cache_root_dir,
    normalizePath(explicit_cache, winslash = "/", mustWork = FALSE)
  ))

  sentinel <- run_sentinel(sentinel, max_folds = 2L, workers = 2L)
  completed <- sentinel@manifest |>
    dplyr::filter(.data$status == "completed")
  expect_equal(nrow(completed), 2L)
  expect_true(all(file.exists(completed$log_file)))
  fold_lines <- unlist(lapply(completed$log_file, readLines, warn = FALSE))
  expect_true(any(grepl("fold-message", fold_lines, fixed = TRUE)))
  expect_true(any(grepl("fold-output", fold_lines, fixed = TRUE)))

  combined_log <- tsbiomass:::sentinel_output_paths(sentinel)$log_file
  expect_true(file.exists(combined_log))
  combined_lines <- readLines(combined_log, warn = FALSE)
  expect_true(any(grepl("fold=0001", combined_lines, fixed = TRUE)))
  expect_true(any(grepl("fold=0002", combined_lines, fixed = TRUE)))
  expect_true(any(grepl("fold-message", combined_lines, fixed = TRUE)))
})

test_that("Sentinel progress logging relays messages while retaining them", {
  log_file <- tempfile(fileext = ".log")
  utils::capture.output(
    expect_message(
      tsbiomass:::sentinel_with_fold_logging(
        work = function() {
          message("visible-worker-message")
          1L
        },
        log_file = log_file,
        progress = TRUE,
        fold_label = "fold=0001"
      ),
      "visible-worker-message"
    ),
    type = "output"
  )
  expect_true(any(grepl(
    "visible-worker-message",
    readLines(log_file, warn = FALSE),
    fixed = TRUE
  )))
})

test_that("Sentinel suite summaries aggregate numeric and logical metrics", {
  results <- tibble::tibble(
    suite_name = c("cold_start", "cold_start", "study"),
    suite_split_mode = c("species_holdout", "species_holdout", "study_holdout"),
    scenario = c("baseline", "baseline", "baseline"),
    holdout_id = c("sp1", "sp2", "study_a"),
    score = c(1, 3, 5),
    covered = c(TRUE, FALSE, TRUE)
  )

  out <- summarize_sentinel_suite_results(results)
  expect_true(S7::S7_inherits(out, Scorecard))
  out_tbl <- out@recommendation_cards

  cold_row <- out_tbl |> dplyr::filter(.data$suite_name == "cold_start")
  study_row <- out_tbl |> dplyr::filter(.data$suite_name == "study")

  expect_equal(cold_row$n_rows, 2L)
  expect_equal(cold_row$n_holdouts, 2L)
  expect_equal(cold_row$mean_score, 2)
  expect_equal(cold_row$prop_covered, 0.5)
  expect_equal(study_row$mean_score, 5)
  expect_equal(study_row$prop_covered, 1)
})
