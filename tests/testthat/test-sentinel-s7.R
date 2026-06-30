test_that("Sentinel infers split columns and builds manifests", {
  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2", "m3", "m4"),
    species_name = c("sp1", "sp1", "sp2", "sp2"),
    study_reference_id = c("study_a", "study_a", "study_b", "study_c"),
    study_cell_id = c("cell_1", "cell_1", "cell_2", "cell_3"),
    value = c(1, 2, 3, 4)
  )

  scenarios <- build_sentinel_scenarios(
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
  scenarios <- build_sentinel_scenarios(
    model_ablations = list(
      no_policy_a = list(policy = "policy_a")
    )
  )

  expect_true("baseline" %in% names(scenarios))
  expect_equal(scenarios$no_policy_a$drop_rows$policy, "policy_a")
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

  sentinel <- run_sentinel(sentinel, max_folds = 2L)
  expect_equal(sum(sentinel@manifest$status == "completed"), 2L)
  expect_equal(nrow(sentinel@results), 2L)

  collected <- collect_sentinel_results(sentinel)
  expect_equal(nrow(collected), 2L)
  expect_true(all(c("fold_id", "scenario", "holdout_id", "train_n", "test_n") %in% names(collected)))

  resumed <- resume_sentinel(sentinel_rebuild(sentinel, manifest = tibble::tibble(), results = tibble::tibble()))
  expect_equal(sum(resumed@manifest$status == "completed"), 2L)
  expect_equal(nrow(resumed@results), 2L)

  artifact_path <- resumed@manifest |>
    dplyr::filter(.data$holdout_id == "m2") |>
    dplyr::pull(.data$artifact_file)
  expect_true(length(artifact_path) == 1L)
  expect_true(file.exists(artifact_path))
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
    metalearner = list(
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
  expect_false("family" %in% out$metalearner$feature_cols)
})

test_that("Sentinel trait pruning drops unavailable fold columns", {
  cfg <- list(
    similarity = list(
      species_traits = c("family", "genus"),
      study_traits = c("study_reference_id", "fao_area")
    ),
    metalearner = list(
      feature_cols = c("family", "depth", "study_cell_id")
    )
  )

  out <- sentinel_prune_trait_config(
    config = cfg,
    available_columns = c("genus", "study_reference_id", "depth")
  )

  expect_equal(out$similarity$species_traits, "genus")
  expect_equal(out$similarity$study_traits, "study_reference_id")
  expect_equal(out$metalearner$feature_cols, "depth")
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

test_that("Sentinel smoke fixture keeps anchor species and top extra species", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:8),
    species_name = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp3", "sp4"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC"),
    study_reference_id = paste0("study_", 1:8)
  )

  out <- build_sentinel_smoke_fixture(
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

test_that("Sentinel smoke fixture drops blank species labels when requested", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:5),
    species_name = c("sp1", " ", NA, "sp2", "sp2"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "AFSC", "AFSC"),
    study_reference_id = paste0("study_", 1:5)
  )

  out <- build_sentinel_smoke_fixture(
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
      metalearner = list(workers = 5L),
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
  expect_equal(cfg$metalearner$workers, 1L)
  expect_equal(cfg$simulation$workers, 1L)
  expect_equal(cfg$alchemist$learner$workers, 1L)
  expect_equal(cfg$alchemist$distill_workers, 1L)
  expect_equal(cfg$ordination$nmds_args$try, 2)
  expect_equal(cfg$ordination$nmds_args$trymax, 4)
  expect_true(grepl("sentinel-fold-cache", cfg$paths$cache_dir, fixed = TRUE))
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

test_that("Sentinel smoke builder reduces candidate rows before construction", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:8),
    species_name = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp3", "sp4"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC"),
    study_reference_id = paste0("study_", 1:8),
    study_cell_id = paste0("cell_", 1:8)
  )

  sentinel <- build_sentinel_smoke(
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
    output_dir = file.path(tempdir(), paste0("sentinel-smoke-", as.integer(Sys.time())))
  )

  expect_equal(sentinel@split_mode, "study_holdout")
  expect_setequal(unique(sentinel@data$species_name), c("sp1", "sp3"))
  expect_equal(nrow(sentinel@data), 5L)
})

test_that("Sentinel smoke runner can build the reduced path without executing folds", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:8),
    species_name = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp3", "sp4"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC"),
    study_reference_id = paste0("study_", 1:8),
    study_cell_id = paste0("cell_", 1:8)
  )

  sentinel <- run_sentinel_smoke(
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
    output_dir = file.path(tempdir(), paste0("sentinel-run-smoke-", as.integer(Sys.time()))),
    max_folds = 0L
  )

  expect_true(inherits(sentinel, "S7_object"))
  expect_true(S7::S7_inherits(sentinel, Sentinel))
  expect_equal(sentinel@split_mode, "study_cell_holdout")
  expect_equal(sum(sentinel@manifest$status == "completed"), 0L)
  expect_true(nrow(sentinel@manifest) > 0L)
})

test_that("Sentinel smoke suite runs all requested split specs in no-op mode", {
  candidate_models <- tibble::tibble(
    model_id_chr = paste0("m", 1:10),
    species_name = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3", "sp3", "sp4", "sp4", "sp5"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC", "AFSC"),
    study_reference_id = paste0("study_", c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
    study_cell_id = paste0("cell_", 1:10)
  )

  suite <- run_sentinel_smoke_suite(
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
    output_dir = file.path(tempdir(), paste0("sentinel-suite-", as.integer(Sys.time()))),
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

  suite <- run_sentinel_smoke_suite(
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
    output_dir = file.path(tempdir(), paste0("sentinel-suite-run-", as.integer(Sys.time()))),
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

  cold_row <- out |> dplyr::filter(.data$suite_name == "cold_start")
  study_row <- out |> dplyr::filter(.data$suite_name == "study")

  expect_equal(cold_row$n_rows, 2L)
  expect_equal(cold_row$n_holdouts, 2L)
  expect_equal(cold_row$mean_score, 2)
  expect_equal(cold_row$prop_covered, 0.5)
  expect_equal(study_row$mean_score, 5)
  expect_equal(study_row$prop_covered, 1)
})
