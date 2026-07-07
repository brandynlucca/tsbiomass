test_that("policy benchmark reuses cached admissibility in sequential runs", {
  candidates <- make_candidates(
    seed_similarity_tuning = FALSE,
    similarity_matrix = minimal_similarity_matrix(),
    gower_distances = minimal_gower_distances()
  )
  anchor_ids <- as.character(candidates@candidate_models$model_id_chr)
  cached_anchors <- stats::setNames(
    lapply(anchor_ids, function(anchor_id) {
      list(
        evaluation = list(
          anchor_pdf = tibble::tibble(length_cm = 10, density = 1),
          anchor_sigma = 1,
          admissible_df = tibble::tibble(
            model_id_chr = "donor_1",
            overlap_same_species = FALSE,
            w_adm = 1,
            cumulative_w_adm = 1,
            support_set = "core"
          ),
          model_eval = tibble::tibble(
            model_id_chr = "donor_1",
            species_name = "Other species"
          )
        )
      )
    }),
    anchor_ids
  )
  candidates <- candidates_with_admissibility(
    candidates,
    list(
      anchors = cached_anchors,
      all_scores = minimal_admissibility_scores()
    )
  )

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) {
      stop("benchmark should reuse cached admissibility")
    },
    resolve_ordination_info = function(...) NULL,
    build_benchmark_row = function(eval_obj, anchor_row, best_policy_name, best_equation_branch_filter, is_reference, config) {
      tibble::tibble(
        anchor_model_id = as.character(anchor_row$model_id[[1]]),
        anchor_species = as.character(anchor_row$species_name[[1]]),
        anchor_family = as.character(anchor_row$family[[1]]),
        local_min_combined_distance = 0.1,
        local_weighted_mean_combined_distance = 0.2,
        local_effective_support = 1
      )
    },
    .package = "tsbiomass"
  )

  out <- run_policy_benchmark(
    candidate_models = candidates,
    policy_fun = function(eval_obj, ...) {
      tibble::tibble(
        policy = "closest_within_species",
        equation_branch_filter = "all",
        multiplier_pred = 1.1
      )
    },
    curve_fun = NULL,
    config = list(
      species_traits = list(genus = 1),
      study_traits = list(fao_area = 1),
      frequency_coherence_mode = "none",
      admissibility_species_traits = character(0),
      admissibility_study_traits = character(0)
    ),
    include_ts_error = FALSE,
    benchmark_schemes = "pseudo_anchor",
    workers = 1L
  )

  expect_equal(nrow(out$policy_perf), nrow(candidates@candidate_models))
  expect_true(all(out$policy_perf$valid_prediction))
})

test_that("policy benchmark preserves missing optional family metadata", {
  candidate_models <- make_candidates(seed_similarity_tuning = FALSE)@candidate_models |>
    dplyr::select(-dplyr::any_of("family"))
  benchmark_config <- list(
    fields = list(
      model_id = "model_id",
      species = "species_name",
      family = "family",
      slope = "slope_standard",
      intercept = "intercept_standard"
    )
  )
  optional_family <- benchmark_optional_field_values(
    candidate_models,
    benchmark_config,
    "family"
  )
  eval_obj <- list(
    admissible_df = tibble::tibble(
      combined_distance = 0.2,
      taxonomic_distance_to_anchor = 1,
      overlap_same_species = FALSE,
      overlap_same_family = FALSE,
      w_adm = 1
    )
  )
  feature_row <- build_benchmark_row(
    eval_obj = eval_obj,
    anchor_row = candidate_models[1, , drop = FALSE],
    best_policy_name = "all_models_weighted_mean",
    best_equation_branch_filter = "all",
    is_reference = FALSE,
    config = benchmark_config
  )

  expect_length(optional_family, nrow(candidate_models))
  expect_true(all(is.na(optional_family)))
  expect_true(is.na(feature_row$anchor_family))
})

test_that("policy benchmark skips ordination rebuild when active pools do not need it", {
  candidates <- make_candidates(
    seed_similarity_tuning = FALSE,
    similarity_matrix = minimal_similarity_matrix(),
    gower_distances = minimal_gower_distances()
  )
  anchor_ids <- as.character(candidates@candidate_models$model_id_chr)
  cached_anchors <- stats::setNames(
    lapply(anchor_ids, function(anchor_id) {
      list(
        evaluation = list(
          anchor_pdf = tibble::tibble(length_cm = 10, density = 1),
          anchor_sigma = 1,
          admissible_df = tibble::tibble(
            model_id_chr = "donor_1",
            overlap_same_species = FALSE,
            w_adm = 1,
            cumulative_w_adm = 1,
            support_set = "core"
          ),
          model_eval = tibble::tibble(
            model_id_chr = "donor_1",
            species_name = "Other species"
          )
        )
      )
    }),
    anchor_ids
  )
  candidates <- candidates_with_admissibility(
    candidates,
    list(
      anchors = cached_anchors,
      all_scores = minimal_admissibility_scores()
    )
  )

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) {
      stop("benchmark should reuse cached admissibility")
    },
    resolve_ordination_info = function(...) {
      stop("ordination context should be skipped for non-ordination pools")
    },
    build_benchmark_row = function(eval_obj, anchor_row, best_policy_name, best_equation_branch_filter, is_reference, config) {
      tibble::tibble(
        anchor_model_id = as.character(anchor_row$model_id[[1]]),
        anchor_species = as.character(anchor_row$species_name[[1]]),
        anchor_family = as.character(anchor_row$family[[1]]),
        local_min_combined_distance = 0.1,
        local_weighted_mean_combined_distance = 0.2,
        local_effective_support = 1
      )
    },
    .package = "tsbiomass"
  )

  out <- run_policy_benchmark(
    candidate_models = candidates,
    policy_fun = function(eval_obj, ...) {
      tibble::tibble(
        policy = "closest_within_species",
        candidate_pool = "all_admissible",
        equation_branch_filter = "all",
        multiplier_pred = 1.1
      )
    },
    curve_fun = NULL,
    config = list(
      species_traits = list(genus = 1),
      study_traits = list(fao_area = 1),
      frequency_coherence_mode = "none",
      admissibility_species_traits = character(0),
      admissibility_study_traits = character(0)
    ),
    include_ts_error = FALSE,
    benchmark_schemes = "pseudo_anchor",
    workers = 1L
  )

  expect_equal(nrow(out$policy_perf), nrow(candidates@candidate_models))
  expect_true(all(out$policy_perf$valid_prediction))
})

test_that("policy benchmark can resume from anchor cache shards without monolithic cache", {
  candidates <- make_candidates(
    seed_similarity_tuning = FALSE,
    similarity_matrix = minimal_similarity_matrix(),
    gower_distances = minimal_gower_distances()
  )
  cache_path <- file.path(tempdir(), paste0("policy-benchmark-", as.integer(Sys.time()), ".rds"))

  testthat::local_mocked_bindings(
    resolve_ordination_info = function(...) NULL,
    build_benchmark_row = function(eval_obj, anchor_row, best_policy_name, best_equation_branch_filter, is_reference, config) {
      tibble::tibble(
        anchor_model_id = as.character(anchor_row$model_id[[1]]),
        anchor_species = as.character(anchor_row$species_name[[1]]),
        anchor_family = as.character(anchor_row$family[[1]]),
        local_min_combined_distance = 0.1,
        local_weighted_mean_combined_distance = 0.2,
        local_effective_support = 1
      )
    },
    .package = "tsbiomass"
  )

  out_first <- run_policy_benchmark(
    candidate_models = candidates,
    policy_fun = function(eval_obj, ...) {
      tibble::tibble(
        policy = "closest_within_species",
        equation_branch_filter = "all",
        multiplier_pred = 1.1
      )
    },
    curve_fun = NULL,
    config = list(
      species_traits = list(genus = 1),
      study_traits = list(fao_area = 1),
      frequency_coherence_mode = "none",
      admissibility_species_traits = character(0),
      admissibility_study_traits = character(0)
    ),
    include_ts_error = FALSE,
    benchmark_schemes = "pseudo_anchor",
    workers = 1L,
    cache_path = cache_path
  )

  shard_dir <- paste0(cache_path, "_parts")
  shard_files <- list.files(shard_dir, pattern = "^anchor_.*\\.rds$", full.names = TRUE)
  expect_true(file.exists(cache_path))
  expect_true(dir.exists(shard_dir))
  expect_equal(length(shard_files), nrow(candidates@candidate_models))

  file.remove(cache_path)

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) {
      stop("benchmark should resume from anchor shards")
    },
    resolve_ordination_info = function(...) NULL,
    build_benchmark_row = function(eval_obj, anchor_row, best_policy_name, best_equation_branch_filter, is_reference, config) {
      tibble::tibble(
        anchor_model_id = as.character(anchor_row$model_id[[1]]),
        anchor_species = as.character(anchor_row$species_name[[1]]),
        anchor_family = as.character(anchor_row$family[[1]]),
        local_min_combined_distance = 0.1,
        local_weighted_mean_combined_distance = 0.2,
        local_effective_support = 1
      )
    },
    .package = "tsbiomass"
  )

  out_second <- run_policy_benchmark(
    candidate_models = candidates,
    policy_fun = function(eval_obj, ...) {
      tibble::tibble(
        policy = "closest_within_species",
        equation_branch_filter = "all",
        multiplier_pred = 1.1
      )
    },
    curve_fun = NULL,
    config = list(
      species_traits = list(genus = 1),
      study_traits = list(fao_area = 1),
      frequency_coherence_mode = "none",
      admissibility_species_traits = character(0),
      admissibility_study_traits = character(0)
    ),
    include_ts_error = FALSE,
    benchmark_schemes = "pseudo_anchor",
    workers = 1L,
    cache_path = cache_path
  )

  expect_equal(nrow(out_first$policy_perf), nrow(out_second$policy_perf))
  expect_true(file.exists(cache_path))
})

test_that("policy benchmark rebuilds TS-error output from cached anchor shards", {
  candidates <- make_candidates(
    seed_similarity_tuning = FALSE,
    similarity_matrix = minimal_similarity_matrix(),
    gower_distances = minimal_gower_distances()
  )
  cache_path <- file.path(tempdir(), paste0("policy-benchmark-ts-", as.integer(Sys.time()), ".rds"))
  ts_error_stub <- tibble::tibble(
    anchor_model_id = "1",
    anchor_species = "Alpha alpha",
    policy = "closest_within_species",
    equation_branch_filter = "all",
    policy_slope_len = 20,
    policy_intercept_len = -70,
    local_min_combined_distance = 0.1,
    local_weighted_mean_combined_distance = 0.2,
    local_effective_support = 1,
    local_mean_length_overlap = 0.8,
    local_mean_depth_overlap = 0.7,
    validation_scheme = "pseudo_anchor",
    u = 0.5,
    ts_error = 0.1,
    log_sigma_residual = 0.01
  )

  testthat::local_mocked_bindings(
    resolve_ordination_info = function(...) NULL,
    build_benchmark_row = function(eval_obj, anchor_row, best_policy_name, best_equation_branch_filter, is_reference, config) {
      tibble::tibble(
        anchor_model_id = as.character(anchor_row$model_id[[1]]),
        anchor_species = as.character(anchor_row$species_name[[1]]),
        anchor_family = as.character(anchor_row$family[[1]]),
        local_min_combined_distance = 0.1,
        local_weighted_mean_combined_distance = 0.2,
        local_effective_support = 1
      )
    },
    build_benchmark_ts_error_table = function(...) ts_error_stub,
    .package = "tsbiomass"
  )

  out_first <- run_policy_benchmark(
    candidate_models = candidates,
    policy_fun = function(eval_obj, ...) {
      tibble::tibble(
        policy = "closest_within_species",
        equation_branch_filter = "all",
        multiplier_pred = 1.1
      )
    },
    curve_fun = predict_policy_curve,
    config = list(
      species_traits = list(genus = 1),
      study_traits = list(fao_area = 1),
      frequency_coherence_mode = "none",
      admissibility_species_traits = character(0),
      admissibility_study_traits = character(0)
    ),
    include_ts_error = TRUE,
    benchmark_schemes = "pseudo_anchor",
    workers = 1L,
    cache_path = cache_path
  )

  expect_equal(nrow(out_first$policy_ts_error), 1L)
  file.remove(cache_path)

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) {
      stop("benchmark should resume policy performance from anchor shards")
    },
    resolve_ordination_info = function(...) NULL,
    build_benchmark_row = function(eval_obj, anchor_row, best_policy_name, best_equation_branch_filter, is_reference, config) {
      tibble::tibble(
        anchor_model_id = as.character(anchor_row$model_id[[1]]),
        anchor_species = as.character(anchor_row$species_name[[1]]),
        anchor_family = as.character(anchor_row$family[[1]]),
        local_min_combined_distance = 0.1,
        local_weighted_mean_combined_distance = 0.2,
        local_effective_support = 1
      )
    },
    build_benchmark_ts_error_table = function(...) ts_error_stub,
    .package = "tsbiomass"
  )

  out_second <- run_policy_benchmark(
    candidate_models = candidates,
    policy_fun = function(eval_obj, ...) {
      tibble::tibble(
        policy = "closest_within_species",
        equation_branch_filter = "all",
        multiplier_pred = 1.1
      )
    },
    curve_fun = predict_policy_curve,
    config = list(
      species_traits = list(genus = 1),
      study_traits = list(fao_area = 1),
      frequency_coherence_mode = "none",
      admissibility_species_traits = character(0),
      admissibility_study_traits = character(0)
    ),
    include_ts_error = TRUE,
    benchmark_schemes = "pseudo_anchor",
    workers = 1L,
    cache_path = cache_path
  )

  expect_equal(nrow(out_second$policy_perf), nrow(candidates@candidate_models))
  expect_equal(nrow(out_second$policy_ts_error), 1L)
})

test_that("policy benchmark builds one shared execution plan per run", {
  candidates <- make_candidates(
    seed_similarity_tuning = FALSE,
    similarity_matrix = minimal_similarity_matrix(),
    gower_distances = minimal_gower_distances()
  )
  anchor_ids <- as.character(candidates@candidate_models$model_id_chr)
  cached_anchors <- stats::setNames(
    lapply(anchor_ids, function(anchor_id) {
      list(
        evaluation = list(
          anchor_pdf = tibble::tibble(length_cm = 10, density = 1),
          anchor_sigma = 1,
          admissible_df = tibble::tibble(
            model_id_chr = "donor_1",
            overlap_same_species = FALSE,
            w_adm = 1,
            cumulative_w_adm = 1,
            support_set = "core"
          ),
          model_eval = tibble::tibble(
            model_id_chr = "donor_1",
            species_name = "Other species"
          )
        )
      )
    }),
    anchor_ids
  )
  candidates <- candidates_with_admissibility(
    candidates,
    list(
      anchors = cached_anchors,
      all_scores = minimal_admissibility_scores()
    )
  )

  plan_builds <- 0L

  testthat::local_mocked_bindings(
    build_policy_execution_plan = function(...) {
      plan_builds <<- plan_builds + 1L
      list(plan = tibble::tibble(dummy = 1))
    },
    screen_one_anchor_admissibility = function(...) {
      stop("benchmark should reuse cached admissibility")
    },
    resolve_ordination_info = function(...) NULL,
    build_benchmark_row = function(eval_obj, anchor_row, best_policy_name, best_equation_branch_filter, is_reference, config) {
      tibble::tibble(
        anchor_model_id = as.character(anchor_row$model_id[[1]]),
        anchor_species = as.character(anchor_row$species_name[[1]]),
        anchor_family = as.character(anchor_row$family[[1]]),
        local_min_combined_distance = 0.1,
        local_weighted_mean_combined_distance = 0.2,
        local_effective_support = 1
      )
    },
    .package = "tsbiomass"
  )

  out <- run_policy_benchmark(
    candidate_models = candidates,
    policy_fun = function(eval_obj, execution_plan = NULL, ...) {
      if (is.null(execution_plan) || !is.list(execution_plan) || !is.data.frame(execution_plan$plan)) {
        stop("benchmark should pass a shared execution plan to anchor evaluation")
      }
      tibble::tibble(
        policy = "closest_within_species",
        equation_branch_filter = "all",
        multiplier_pred = 1.1
      )
    },
    curve_fun = NULL,
    config = list(
      species_traits = list(genus = 1),
      study_traits = list(fao_area = 1),
      frequency_coherence_mode = "none",
      admissibility_species_traits = character(0),
      admissibility_study_traits = character(0)
    ),
    include_ts_error = FALSE,
    benchmark_schemes = "pseudo_anchor",
    workers = 1L
  )

  expect_equal(plan_builds, 1L)
  expect_equal(nrow(out$policy_perf), nrow(candidates@candidate_models))
})
test_that("species-block benchmarking skips generalized equations", {
  cfg <- tsbiomass:::default_anchor_config(minimal_config_data())
  cfg$fields <- list(
    model_id = "model_id",
    species = "species_name"
  )
  generic_anchor <- tibble::tibble(
    model_id = "generic_1",
    species_name = "NA NA"
  )

  result <- tsbiomass:::benchmark_one_anchor(
    anchor_row = generic_anchor,
    candidate_models = tibble::tibble(),
    policy_fun = tsbiomass:::evaluate_policies,
    curve_fun = NULL,
    model_scores = NULL,
    species_lookup = NULL,
    reference_ids = character(0),
    policies = character(0),
    policy_params = list(),
    policy_path = NULL,
    sim_obj = NULL,
    dist_obj = NULL,
    candidate_models_scored = NULL,
    config = cfg,
    registry_path = NULL,
    scheme = "species_block",
    species_block = TRUE
  )

  expect_null(result)
})
