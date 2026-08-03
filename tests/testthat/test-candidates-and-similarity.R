test_that("reference anchors can be assigned by dynamic selector", {
  candidates <- make_candidates()
  anchored <- set_reference_anchors(candidates, selector = list(regional_body = "SWFSC"))

  expect_true(S7::S7_inherits(anchored, Candidates))
  expect_equal(sort(anchored@reference_anchors$model_id), c(1L, 2L, 4L))
  expect_true(all(anchored@reference_anchors$regional_body == "SWFSC"))
})

test_that("candidate source ids infer built-in adapters without type or engine", {
  tmp_dir <- tempfile("candidate-config-")
  dir.create(tmp_dir)

  study_path <- file.path(tmp_dir, "input.xlsx")
  azores_path <- file.path(tmp_dir, "azores.tab")
  dir.create(file.path(tmp_dir, "supplemental"))
  file.create(study_path)
  file.create(azores_path)

  spec <- tsbiomass:::normalize_candidates_config(
    list(
      data = list(
        list(id = "study_metadata", path = study_path),
        list(id = "worms"),
        list(id = "Fishbase"),
        list(id = "AzoresTraits", path = azores_path),
        list(id = "PelagicTraits", alias = "pelagic", path = file.path(tmp_dir, "supplemental"))
      ),
      enrich = list(
        precedence = c("pelagic", "azorestraits", "fishbase", "worms")
      ),
      prepare = list()
    ),
    base_dir = tmp_dir
  )

  expect_equal(spec$study$path, normalizePath(study_path, winslash = "/", mustWork = FALSE))
  expect_equal(names(spec$sources), c("worms", "fishbase", "azorestraits", "pelagic"))
  expect_equal(spec$sources$worms$adapter, "worms")
  expect_equal(spec$sources$fishbase$adapter, "fishbase")
  expect_equal(spec$sources$azorestraits$adapter, "azores")
  expect_equal(spec$sources$pelagic$adapter, "pelagic")
  expect_equal(spec$enrich$precedence, c("pelagic", "azorestraits", "fishbase", "worms"))
})

test_that("candidate body-shape labels are normalized to canonical categories", {
  models <- minimal_candidate_models()
  models$body_shape <- c("fusiform / normal", "short and/or deep", "eel-like", "elongated")

  standardized <- tsbiomass:::standardize_candidate_columns(models)

  expect_equal(
    standardized$body_shape,
    c("fusiform", "short_deep", "eel_like", "elongated")
  )
})

test_that("prepare_similarities stores prepared state on Candidates", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)

  prepared <- prepare_similarities(
    candidate_models = candidates,
    config = minimal_similarity_config()
  )

  expect_true(S7::S7_inherits(prepared, Candidates))
  expect_true(length(prepared@similarity_matrix) > 0)
  expect_true(all(c("species_weights", "study_weights", "candidate_models") %in% names(prepared@similarity_matrix)))
  expect_true(any(grepl("^fao_area__", names(prepared@similarity_matrix[["candidate_models"]]))))
  expect_false(any(grepl("^fao_area__", names(prepared@candidate_models))))
})

test_that("prepare_similarities preserves tuning state and trims stored candidate table", {
  candidates <- make_candidates(seed_similarity_tuning = TRUE)
  candidates@spec$config_data <- tsbiomass:::merge_config_sections(
    minimal_config_data(),
    list(
      similarity = list(
        species_traits = c("genus", "family"),
        study_traits = c("frequency", "fao_area")
      )
    )
  )

  prepared <- prepare_similarities(
    candidate_models = candidates,
    config = minimal_similarity_config()
  )

  expect_true(length(prepared@similarity_tuning) > 0)
  expect_true("config_tuned" %in% names(prepared@similarity_tuning))
  expect_false("ocean_basin" %in% names(prepared@candidate_models))
  expect_false("class" %in% names(prepared@candidate_models))
  expect_true("genus" %in% names(prepared@candidate_models))
  expect_true("family" %in% names(prepared@candidate_models))
  expect_true("frequency" %in% names(prepared@candidate_models))
  expect_true("fao_area" %in% names(prepared@candidate_models))
})

test_that("construct_gower_distances stores distance matrices on Candidates", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  candidates <- prepare_similarities(
    candidate_models = candidates,
    config = minimal_similarity_config()
  )

  distance_candidates <- construct_gower_distances(candidates)

  expect_true(S7::S7_inherits(distance_candidates, Candidates))
  expect_true(length(distance_candidates@gower_distances) > 0)
  expect_equal(
    dim(distance_candidates@gower_distances[["combined_dist"]]),
    c(nrow(distance_candidates@candidate_models), nrow(distance_candidates@candidate_models))
  )
  expect_equal(
    unname(diag(distance_candidates@gower_distances[["combined_dist"]])),
    rep(0, nrow(distance_candidates@candidate_models))
  )
})

test_that("similarity preparation and distances support species-only and study-only traits", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)

  species_only_cfg <- minimal_similarity_config()
  species_only_cfg$species_traits <- list(genus = 1)
  species_only_cfg$study_traits <- list()

  species_only <- prepare_similarities(
    candidate_models = candidates,
    config = species_only_cfg
  )
  species_only <- construct_gower_distances(species_only)

  expect_equal(species_only@similarity_matrix$species_traits, "genus")
  expect_equal(species_only@similarity_matrix$study_traits, character(0))
  expect_equal(
    dim(species_only@gower_distances$combined_dist),
    c(nrow(species_only@candidate_models), nrow(species_only@candidate_models))
  )
  expect_true(all(is.finite(diag(species_only@gower_distances$combined_dist))))

  study_only_cfg <- minimal_similarity_config()
  study_only_cfg$species_traits <- list()
  study_only_cfg$study_traits <- list(fao_area = 1)

  study_only <- prepare_similarities(
    candidate_models = candidates,
    config = study_only_cfg
  )
  study_only <- construct_gower_distances(study_only)

  expect_equal(study_only@similarity_matrix$species_traits, character(0))
  expect_equal(study_only@similarity_matrix$study_traits, "fao_area")
  expect_equal(
    dim(study_only@gower_distances$combined_dist),
    c(nrow(study_only@candidate_models), nrow(study_only@candidate_models))
  )
  expect_true(all(is.finite(diag(study_only@gower_distances$combined_dist))))
})

test_that("forge_distances invalidates stale downstream alchemist state", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  alchemist <- as_alchemist(candidates)
  alchemist <- tsbiomass:::alchemist_rebuild(
    alchemist,
    learner = list(old = TRUE),
    trait_importance = list(old = TRUE),
    ordination = list(old = TRUE),
    admissibility = list(old = TRUE)
  )

  testthat::local_mocked_bindings(
    build_pair_data = function(models_df,
                               sp_names,
                               st_names,
                               coherence_cfg = NULL,
                               taxonomic_distance = FALSE,
                               feature_type = "gower",
                               progress = FALSE) {
      list(
        training_data = tibble::tibble(
          .donor_idx = c(1L, 1L, 2L, 2L),
          .anchor_idx = c(2L, 3L, 3L, 4L),
          .dist_family = c(0.1, 0.2, 0.3, 0.4)
        ),
        feature_cols = ".dist_family",
        all_traits = "family",
        species_feature_cols = ".dist_family",
        trait_mats = list(),
        donor_sigma_matrix = matrix(1, nrow = 4, ncol = 4),
        target_sigma = rep(1, 4),
        model_ids = as.character(seq_len(nrow(models_df))),
        n_models = nrow(models_df)
      )
    },
    fit_super_learner = function(...) {
      list(
        oof_ensemble_prediction = c(0.2, 0.3, 0.4, 0.5),
        oof_performance = tibble::tibble(method = "mock", rmse = 0.1, mae = 0.1)
      )
    },
    .package = "tsbiomass"
  )

  rebuilt <- forge_distances(alchemist)

  expect_true(length(rebuilt@learner) > 0)
  expect_true(length(rebuilt@distance_matrix) > 0)
  expect_equal(length(rebuilt@trait_importance), 0L)
  expect_equal(length(rebuilt@ordination), 0L)
  expect_equal(length(rebuilt@admissibility), 0L)
})

test_that("as_alchemist resolves trait settings from a full workflow config list", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  cfg <- minimal_config_data()

  alchemist <- as_alchemist(candidates, config = cfg)

  expect_true(length(alchemist@config$species_traits) > 0L)
  expect_true(length(alchemist@config$study_traits) > 0L)
  expect_true(all(c("genus", "family") %in% names(alchemist@config$species_traits)))
  expect_true(all(c("frequency", "fao_area") %in% names(alchemist@config$study_traits)))
})

test_that("Alchemist species-purged OOF splits remove held-out species from donor and anchor roles", {
  training_data <- tibble::tibble(
    .anchor_species = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3"),
    .donor_species = c("sp2", "sp3", "sp1", "sp3", "sp1", "sp2"),
    .split_group = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3"),
    .outcome = seq_len(6),
    .dist_family = seq(0.1, 0.6, by = 0.1)
  )

  splits <- tsbiomass:::build_alchemist_oof_splits(
    training_data = training_data,
    inner_folds = 3L,
    seed = 1L,
    oof_mode = "species_purged"
  )

  expect_length(splits, 3L)
  for (fold_now in splits) {
    heldout <- as.character(fold_now$holdout_groups)
    train_rows <- training_data[fold_now$train_idx, , drop = FALSE]
    valid_rows <- training_data[fold_now$valid_idx, , drop = FALSE]

    expect_true(all(valid_rows$.anchor_species %in% heldout))
    expect_false(any(train_rows$.anchor_species %in% heldout))
    expect_false(any(train_rows$.donor_species %in% heldout))
  }
})

test_that("Alchemist fit_super_learner accepts species-purged OOF splits", {
  training_data <- tibble::tibble(
    .anchor_species = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3"),
    .donor_species = c("sp2", "sp3", "sp1", "sp3", "sp1", "sp2"),
    .split_group = c("sp1", "sp1", "sp2", "sp2", "sp3", "sp3"),
    .outcome = c(0.1, 0.2, 0.3, 0.2, 0.25, 0.35),
    .dist_family = c(0.1, 0.2, 0.3, 0.2, 0.4, 0.5)
  )

  fit <- tsbiomass:::fit_super_learner(
    training_data = training_data,
    feature_cols = ".dist_family",
    methods = "glm",
    outcome_transform = "identity",
    inner_folds = 3L,
    seed = 1L,
    oof_mode = "species_purged",
    workers = 1L,
    progress = FALSE
  )

  expect_true(all(is.finite(fit$oof_ensemble_prediction)))
  expect_equal(length(fit$oof_ensemble_prediction), nrow(training_data))
})

test_that("Alchemist rf honors configured forest controls", {
  testthat::skip_if_not_installed("ranger")
  set.seed(1L)
  x_train <- matrix(stats::rnorm(240), nrow = 80L, ncol = 3L)
  colnames(x_train) <- paste0("feature_", seq_len(ncol(x_train)))
  y_train <- stats::rnorm(nrow(x_train))

  learner <- tsbiomass:::fit_base_learner(
    x_train = x_train,
    y_train = y_train,
    method = "rf",
    method_settings = list(
      rf = list(
        num_trees = 5L,
        mtry = 1L,
        min_node_size = 2L,
        max_depth = 3L,
        sample_fraction = 0.632,
        replace = FALSE,
        respect_unordered_factors = "ignore"
      )
    ),
    seed = 1L
  )

  expect_equal(learner$fit$num.trees, 5L)
  expect_equal(learner$fit$mtry, 1L)
  expect_equal(learner$fit$min.node.size, 2L)
  expect_equal(learner$fit$max.depth, 3L)
  expect_false(learner$fit$replace)

  learner_without_seed <- tsbiomass:::fit_base_learner(
    x_train = x_train,
    y_train = y_train,
    method = "rf",
    method_settings = list(
      rf = list(
        num_trees = 5L,
        min_node_size = 2L,
        max_depth = 3L,
        sample_fraction = 0.632,
        replace = FALSE
      )
    ),
    seed = NULL
  )
  expect_s3_class(learner_without_seed$fit, "ranger")
})

test_that("run_ordination works on Alchemist distance objects with model trait tables", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  alchemist <- as_alchemist(candidates, config = minimal_config_data())

  testthat::local_mocked_bindings(
    build_pair_data = function(models_df,
                               species_trait_names,
                               study_trait_names,
                               coherence_cfg = NULL,
                               taxonomic_distance = FALSE,
                               feature_type = "gower",
                               progress = FALSE) {
      list(
        training_data = tibble::tibble(
          .donor_idx = c(1L, 1L, 2L, 2L),
          .anchor_idx = c(2L, 3L, 3L, 4L),
          .anchor_species = c("Alpha alpha", "Beta beta", "Beta beta", "Gamma gamma"),
          .donor_species = c("Beta beta", "Gamma gamma", "Alpha alpha", "Alpha alpha"),
          .split_group = c("Beta beta", "Beta beta", "Gamma gamma", "Gamma gamma"),
          .outcome = c(0.2, 0.3, 0.4, 0.5),
          .dist_family = c(0.1, 0.2, 0.3, 0.4)
        ),
        feature_cols = ".dist_family",
        all_traits = c("family", "frequency"),
        species_trait_names = "family",
        species_feature_cols = ".dist_family",
        n_models = nrow(models_df),
        model_ids = as.character(seq_len(nrow(models_df))),
        donor_sigma_matrix = matrix(1, nrow = 4, ncol = 4),
        target_sigma = rep(1, 4),
        trait_mats = list(),
        feature_type = feature_type
      )
    },
    fit_super_learner = function(...) {
      list(
        fit = list(),
        weights = c(glm = 1),
        feature_cols = ".dist_family",
        outcome_transform = "identity",
        lambda_rule = "lambda.1se",
        inner_fold_splits = list(),
        oof_predictions = tibble::tibble(glm = c(0.2, 0.3, 0.4, 0.5)),
        oof_ensemble_prediction = c(0.2, 0.3, 0.4, 0.5),
        oof_performance = tibble::tibble(method = "mock", rmse = 0.1, mae = 0.1)
      )
    },
    .package = "tsbiomass"
  )

  alchemist <- forge_distances(alchemist)
  expect_no_error(
    ord <- suppressWarnings(
      run_ordination(alchemist, include_loadings = FALSE, include_centroids = FALSE)
    )
  )
  expect_true(length(ord@ordination) > 0L)
})

test_that("assign_ordination_groups can use a fixed cluster count", {
  points <- tibble::tibble(
    model_id = as.character(seq_len(8)),
    MDS1 = c(-2, -1.8, -1.6, 0, 0.2, 0.4, 2, 2.2),
    MDS2 = c(0, 0.1, -0.1, 2, 2.2, 1.8, -1, -1.1)
  )

  clustered <- tsbiomass:::assign_ordination_groups(points, k = 3)

  expect_equal(unique(clustered$nmds_cluster_k), 3L)
  expect_equal(dplyr::n_distinct(clustered$nmds_cluster_id), 3L)
})

test_that("ordination hulls are explicitly closed by cluster", {
  points <- tibble::tibble(
    model_id = as.character(seq_len(4)),
    nmds_cluster_id = "cluster_1",
    MDS1 = c(0, 1, 1, 0),
    MDS2 = c(0, 0, 1, 1)
  )

  hull <- tsbiomass:::build_ordination_hulls(points)

  expect_equal(nrow(hull), 5L)
  expect_equal(hull$MDS1[[1]], hull$MDS1[[nrow(hull)]])
  expect_equal(hull$MDS2[[1]], hull$MDS2[[nrow(hull)]])
})

test_that("Alchemist learner methods resolve through the Alchemist method catalog", {
  expect_equal(tsbiomass:::alchemist_method_spec("glm_elastic")$family, "glm_penalized")
  expect_equal(tsbiomass:::alchemist_method_spec("mars")$family, "mars")
  expect_equal(tsbiomass:::alchemist_method_spec("rf")$family, "rf")
  expect_error(
    tsbiomass:::alchemist_method_spec("rebart"),
    "Unknown Alchemist base learner method"
  )
})

test_that("automatic ordination cluster selection can prefer granular near-optimal silhouettes", {
  scores <- tibble::tibble(
    k = c(2L, 3L, 4L),
    silhouette = c(0.60, 0.58, 0.56)
  )

  expect_equal(
    tsbiomass:::select_ordination_cluster_k(
      scores,
      selection_rule = "granular_silhouette",
      silhouette_tolerance = 0.05
    ),
    4L
  )
  expect_equal(
    tsbiomass:::select_ordination_cluster_k(
      scores,
      selection_rule = "max_silhouette",
      silhouette_tolerance = 0.05
    ),
    2L
  )
})

test_that("screen_admissibility preserves arbitrary configured trait gates", {
  cfg <- minimal_config_data()
  cfg$admissibility$species_traits <- c("family")
  cfg$admissibility$study_traits <- character(0)
  cfg$admissibility$coherence$frequency$mode <- "none"
  cfg_obj <- build_configurer(cfg, base_dir = tempdir())

  candidates <- set_reference_anchors(
    make_candidates(seed_similarity_tuning = FALSE),
    selector = list(regional_body = "SWFSC")
  )
  screened <- screen_admissibility(
    candidate_models = candidates,
    config = cfg_obj,
    refresh = TRUE,
    progress = FALSE
  )
  scores <- tibble::as_tibble(screened@admissibility$all_scores)

  expect_true(tsbiomass:::admissibility_bundle_is_current(screened@admissibility, cfg_obj))
  expect_true("gate_trait_family" %in% names(scores))
  expect_true(any(scores$inadmissible_reason == "trait_mismatch:family", na.rm = TRUE))
})

test_that("missingness summaries dispatch through Candidates and PolicySelector", {
  candidates <- make_candidates()
  screened <- tsbiomass:::screen_missing_metadata(candidates)

  expect_true("key_metadata_missing_fraction" %in% names(screened))
  expect_equal(nrow(screened), nrow(candidates@candidate_models))

  selector <- make_selector(candidates = candidates)
  summary_obj <- summarize_key_missing(selector)

  expect_true(all(c("overall", "by_field", "by_model") %in% names(summary_obj)))
  expect_true("missing_fraction" %in% names(summary_obj$by_field))
})

test_that("tune_similarities writes tuning results back to Candidates", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)

  testthat::local_mocked_bindings(
    prepare_similarities = function(candidate_models,
                                    species_traits = NULL,
                                    study_traits = NULL,
                                    alpha = NULL,
                                    k_species = NULL,
                                    k_study = NULL,
                                    ...) {
      candidate_tbl <- if (inherits(candidate_models, "S7_object")) {
        candidate_models@candidate_models
      } else {
        tibble::as_tibble(candidate_models)
      }

      species_weights_now <- unlist(species_traits %||% list(genus = 1, family = 0.5))
      study_weights_now <- unlist(study_traits %||% list(frequency = 1, fao_area = 1))
      sim_obj <- minimal_similarity_matrix(candidate_tbl)
      sim_obj$species_traits <- names(species_weights_now)
      sim_obj$study_traits <- names(study_weights_now)
      sim_obj$species_weights <- species_weights_now
      sim_obj$study_weights <- study_weights_now
      sim_obj$species_matrix_weights <- species_weights_now
      sim_obj$study_matrix_weights <- study_weights_now
      sim_obj$species_profiles <- candidate_tbl |>
        dplyr::select(dplyr::all_of(unique(c("species_name", names(species_weights_now))))) |>
        dplyr::distinct()
      sim_obj$study_data <- candidate_tbl |>
        dplyr::select(dplyr::all_of(names(study_weights_now)))
      sim_obj$species_component_lookup <- stats::setNames(
        names(species_weights_now),
        names(species_weights_now)
      )
      sim_obj$study_component_lookup <- stats::setNames(
        names(study_weights_now),
        names(study_weights_now)
      )
      sim_obj$alpha <- alpha %||% 0.65
      sim_obj$kernel_scale <- k_species %||% k_study %||% 2
      sim_obj$k_species <- k_species %||% 2
      sim_obj$k_study <- k_study %||% 2
      sim_obj$seed <- 42L
      sim_obj$frequency_span <- 1
      sim_obj$config <- list(
        alpha_grid = c(0.45, 0.65, 0.85),
        kernel_scale_grid = c(1, 2, 3),
        length_coherence = list(method = "overlap", weight = 1),
        depth_coherence = list(method = "overlap", weight = 1),
        frequency_coherence = list(method = "overlap", weight = 1)
      )

      if (inherits(candidate_models, "S7_object")) {
        return(tsbiomass:::candidates_with_similarity_matrix(candidate_models, sim_obj))
      }

      sim_obj
    },
    build_tuning_subset = function(candidate_models, ...) {
      tibble::as_tibble(candidate_models)
    },
    run_tuning_grid_search = function(...) {
      list(
        alpha_best = 0.75,
        kernel_scale_best = 3,
        k_species_best = 3,
        k_study_best = 3,
        baseline = tibble::tibble(stage = "baseline", rmse = 0.20),
        grid_scores = tibble::tibble(stage = "grid", rmse = 0.10)
      )
    },
    run_component_dropout = function(...) {
      tibble::tibble(component = "full_model", rmse = 0.10, mae = 0.08, n_eval = 4L)
    },
    apply_component_weights = function(...) {
      list(
        species_weights = c(genus = 2, family = 1),
        study_weights = c(frequency = 0.5, fao_area = 1.5),
        config = list(
          length_coherence = list(method = "overlap", weight = 2),
          depth_coherence = list(method = "overlap", weight = 1),
          frequency_coherence = list(method = "numeric", weight = 1)
        )
      )
    },
    score_similarity_config = function(...) {
      tibble::tibble(rmse = 0.09, mae = 0.07, n_eval = 4L)
    },
    .package = "tsbiomass"
  )

  tuned <- tune_similarities(candidates)

  expect_true(S7::S7_inherits(tuned, Candidates))
  expect_true(length(tuned@similarity_tuning) > 0)
  expect_true(length(tuned@similarity_matrix) > 0)
  expect_equal(tuned@similarity_tuning$config_tuned$alpha, 0.75)
  expect_equal(tuned@similarity_tuning$config_tuned$kernel_scale, 3)
  expect_equal(tuned@similarity_tuning$config_tuned$species_weights[["genus"]], 2)
  expect_equal(tuned@similarity_matrix[["alpha"]], 0.75)
  expect_equal(tuned@similarity_matrix[["kernel_scale"]], 3)
  expect_equal(tuned@similarity_matrix[["species_weights"]][["genus"]], 2)
  expect_true("stability_summary" %in% names(tuned@similarity_tuning))
  expect_true(all(c("component", "component_type", "median_value") %in% names(tuned@similarity_tuning$stability_summary)))
})

test_that("run_tuning_grid_search uses screening and refinement", {
  search_object <- tsbiomass:::run_tuning_grid_search(
    tune_models = minimal_candidate_models(),
    base_sim = minimal_similarity_matrix(),
    registry_path = NULL,
    n_cores = 1L
  )

  expect_true(all(c("baseline", "grid_scores", "alpha_best", "kernel_scale_best", "k_species_best", "k_study_best") %in% names(search_object)))
  expect_true(any(grepl("^search_screen_", search_object$grid_scores$stage)))
  expect_true(any(grepl("^search_full$", search_object$grid_scores$stage)))
  expect_true(is.finite(search_object$alpha_best))
  expect_true(is.finite(search_object$kernel_scale_best))
  expect_true(is.finite(search_object$k_species_best))
  expect_true(is.finite(search_object$k_study_best))
  expect_equal(search_object$k_species_best, search_object$k_study_best)
})

test_that("similarity scoring integrates over the held-out length distribution", {
  models_subset <- minimal_candidate_models() |>
    dplyr::mutate(
      study_length_min = length_min,
      study_length_max = length_max
    )
  score_basis <- tsbiomass:::prepare_similarity_score_basis(
    models_subset = models_subset,
    species_weights = c(genus = 1, family = 0.5),
    study_weights = c(frequency = 1, fao_area = 1),
    alpha_now = 0.65,
    k_species_now = 2,
    k_study_now = 1,
    cfg_now = minimal_similarity_config(),
    registry_path = NULL,
    seed_now = 42L
  )

  row_one <- models_subset[1, , drop = FALSE]
  manual_pdf <- tibble::tibble(
    length_cm = seq(row_one$study_length_min[[1]], row_one$study_length_max[[1]], length.out = 400),
    f_len = rep(1 / 400, 400)
  )
  expected_sigma <- tsbiomass:::equation_sigma_mean(
    slope = row_one$slope_len[[1]],
    intercept = row_one$intercept_len[[1]],
    anchor_pdf = manual_pdf
  )
  midpoint_sigma <- 10^(
    tsbiomass:::ts_at_length(
      slope = row_one$slope_len[[1]],
      intercept = row_one$intercept_len[[1]],
      lengths_cm = mean(c(row_one$study_length_min[[1]], row_one$study_length_max[[1]]))
    ) / 10
  )

  expect_equal(score_basis$target_sigma[[1]], expected_sigma, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(score_basis$target_sigma[[1]], midpoint_sigma, tolerance = 1e-8)))
})

test_that("similarity tuning basis excludes same-species donors from scoring", {
  models_subset <- minimal_candidate_models() |>
    dplyr::mutate(
      species_name = c("species_a", "species_a", "species_b", "species_c"),
      genus = c("genus_a", "genus_a", "genus_b", "genus_c"),
      species = c("a", "a", "b", "c")
    )

  score_basis <- tsbiomass:::prepare_similarity_score_basis(
    models_subset = models_subset,
    species_weights = c(genus = 1, family = 0.5),
    study_weights = c(frequency = 1, fao_area = 1),
    alpha_now = 0.65,
    k_species_now = 2,
    k_study_now = 1,
    cfg_now = minimal_similarity_config(),
    registry_path = NULL,
    seed_now = 42L
  )

  expect_true(isTRUE(score_basis$same_species_mask[1, 2]))
  expect_true(isTRUE(score_basis$same_species_mask[2, 1]))
  expect_false(isTRUE(score_basis$same_species_mask[1, 1]))
  expect_false(isTRUE(score_basis$same_species_mask[1, 3]))
})

test_that("anchor density uses study interval or midpoint without Lmax fallback", {
  cfg <- tsbiomass:::default_anchor_config()

  interval_anchor <- tibble::tibble(
    study_length_min = 10,
    study_length_max = 30,
    study_length_midpoint = NA_real_
  )
  interval_pdf <- tsbiomass:::build_anchor_density(interval_anchor, cfg, n = 5)
  expect_equal(interval_pdf$length_cm, seq(10, 30, length.out = 5))
  expect_equal(interval_pdf$f_len, rep(0.2, 5))

  midpoint_anchor <- tibble::tibble(
    study_length_min = NA_real_,
    study_length_max = NA_real_,
    study_length_midpoint = 17
  )
  midpoint_pdf <- tsbiomass:::build_anchor_density(midpoint_anchor, cfg, n = 5)
  expect_equal(midpoint_pdf$length_cm, 17)
  expect_equal(midpoint_pdf$f_len, 1)

  missing_anchor <- tibble::tibble(
    study_length_min = NA_real_,
    study_length_max = NA_real_,
    study_length_midpoint = NA_real_,
    species_length_max = 44
  )
  expect_error(
    tsbiomass:::build_anchor_density(missing_anchor, cfg, n = 5),
    "No valid anchor study length interval or midpoint was available",
    class = "tsbiomass_unscorable_anchor"
  )
})

test_that("admissibility records unscorable anchors without dropping valid anchors", {
  cfg <- minimal_config_data()
  cfg$admissibility$species_traits <- c("family")
  cfg$admissibility$study_traits <- character(0)
  cfg$admissibility$coherence$frequency$mode <- "none"
  cfg_obj <- build_configurer(cfg, base_dir = tempdir())

  candidates <- set_reference_anchors(
    make_candidates(seed_similarity_tuning = FALSE),
    model_ids = c("1", "4")
  )
  anchors <- candidates@reference_anchors
  failed_id <- as.character(anchors$model_id[[1]])
  anchors$study_length_min[[1]] <- NA_real_
  anchors$study_length_max[[1]] <- NA_real_
  anchors$study_length_midpoint[[1]] <- NA_real_
  candidates <- Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = candidates@candidate_models,
    reference_anchors = anchors,
    similarity_matrix = candidates@similarity_matrix,
    gower_distances = candidates@gower_distances,
    ordination = candidates@ordination,
    admissibility = list(),
    similarity_tuning = candidates@similarity_tuning
  )

  screened <- screen_admissibility(
    candidate_models = candidates,
    config = cfg_obj,
    refresh = TRUE,
    progress = FALSE
  )
  failures <- tibble::as_tibble(screened@admissibility$anchor_failures)

  expect_equal(nrow(failures), 1L)
  expect_equal(failures$anchor_model_id, failed_id)
  expect_equal(failures$failure_code, "missing_study_length_support")
  expect_match(failures$failure_message, "No valid anchor study length interval")
  expect_equal(length(screened@admissibility$anchors), 1L)
})

test_that("admissibility records anchors with invalid backscatter as unscorable", {
  cfg <- minimal_config_data()
  cfg$admissibility$species_traits <- character(0)
  cfg$admissibility$study_traits <- character(0)
  cfg$admissibility$coherence$frequency$mode <- "none"
  cfg_obj <- build_configurer(cfg, base_dir = tempdir())

  candidates <- set_reference_anchors(
    make_candidates(seed_similarity_tuning = FALSE),
    model_ids = c("1", "4")
  )
  models <- candidates@candidate_models
  models$slope_standard <- models$slope_len
  models$intercept_standard <- models$intercept_len
  models$slope_standard[models$model_id_chr == "1"] <- NA_real_
  models$intercept_standard[models$model_id_chr == "1"] <- NA_real_
  anchors <- models[models$model_id_chr %in% c("1", "4"), , drop = FALSE]
  failed_id <- "1"
  candidates <- Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = models,
    reference_anchors = anchors,
    similarity_matrix = candidates@similarity_matrix,
    gower_distances = candidates@gower_distances,
    ordination = candidates@ordination,
    admissibility = list(),
    similarity_tuning = candidates@similarity_tuning
  )

  screened <- screen_admissibility(
    candidate_models = candidates,
    config = cfg_obj,
    refresh = TRUE,
    progress = FALSE
  )
  failures <- tibble::as_tibble(screened@admissibility$anchor_failures)

  expect_equal(nrow(failures), 1L)
  expect_equal(failures$anchor_model_id, failed_id)
  expect_equal(failures$failure_stage, "anchor_backscatter")
  expect_equal(failures$failure_code, "invalid_anchor_backscatter")
  expect_match(failures$failure_message, "no finite positive anchor backscatter")
  expect_equal(length(screened@admissibility$anchors), 1L)
})

test_that("reference anchor PDFs can be set from raw empirical lengths", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  candidates <- set_reference_anchors(candidates, model_ids = c("1", "4"))

  anchors_before <- fetch_reference_anchors(candidates)
  expect_true(all(anchors_before$length_pdf == "uniform"))

  candidates <- set_reference_length_pdf(
    candidates,
    length_pdf = list("1" = c(10, 10, 20, 30))
  )

  anchors_after <- fetch_reference_anchors(candidates)
  expect_equal(
    anchors_after$length_pdf[anchors_after$model_id_chr == "1"],
    "user"
  )
  expect_equal(
    anchors_after$length_pdf[anchors_after$model_id_chr == "4"],
    "uniform"
  )

  row_one <- dplyr::filter(anchors_after, .data$model_id_chr == "1")
  pdf_one <- tsbiomass:::build_anchor_density(row_one, tsbiomass:::default_anchor_config())
  expect_gt(nrow(pdf_one), 3L)
  expect_true(all(c("length_cm", "f_len", "pdf_density") %in% names(pdf_one)))
  expect_equal(min(pdf_one$length_cm), 10, tolerance = 1e-8)
  expect_equal(max(pdf_one$length_cm), 30, tolerance = 1e-8)
  expect_equal(sum(pdf_one$f_len), 1, tolerance = 1e-8)
  expect_true(any(pdf_one$pdf_density > 0))
})

test_that("user reference PDFs record invalid anchor length-form coefficients", {
  cfg <- minimal_config_data()
  cfg$admissibility$species_traits <- c("family")
  cfg$admissibility$study_traits <- character(0)
  cfg$admissibility$coherence$frequency$mode <- "none"
  cfg_obj <- build_configurer(cfg, base_dir = tempdir())

  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  candidates <- set_reference_anchors(candidates, model_ids = "1")
  candidates <- set_reference_length_pdf(
    candidates,
    length_pdf = list("1" = c(10, 12, 14, 16, 18))
  )

  candidate_models <- candidates@candidate_models
  reference_anchors <- candidates@reference_anchors
  candidate_idx <- as.character(candidate_models$model_id) == "1"
  anchor_idx <- as.character(reference_anchors$model_id) == "1"
  candidate_models$slope_len[candidate_idx] <- NA_real_
  candidate_models$intercept_len[candidate_idx] <- NA_real_
  candidate_models$lw_a_g[candidate_idx] <- NA_real_
  candidate_models$lw_b[candidate_idx] <- NA_real_
  reference_anchors$slope_len[anchor_idx] <- NA_real_
  reference_anchors$intercept_len[anchor_idx] <- NA_real_
  reference_anchors$lw_a_g[anchor_idx] <- NA_real_
  reference_anchors$lw_b[anchor_idx] <- NA_real_

  candidates <- Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = candidate_models,
    reference_anchors = reference_anchors,
    similarity_matrix = candidates@similarity_matrix,
    gower_distances = candidates@gower_distances,
    ordination = candidates@ordination,
    admissibility = candidates@admissibility,
    similarity_tuning = candidates@similarity_tuning
  )

  screened <- screen_admissibility(candidates, config = cfg_obj, refresh = TRUE, progress = FALSE)
  failures <- tibble::as_tibble(screened@admissibility$anchor_failures)

  expect_equal(nrow(failures), 1L)
  expect_equal(failures$anchor_model_id, "1")
  expect_equal(failures$failure_stage, "anchor_backscatter")
  expect_equal(failures$failure_code, "invalid_anchor_backscatter_user_pdf")
  expect_match(failures$failure_message, "no finite positive anchor backscatter")
  expect_equal(length(screened@admissibility$anchors), 0L)
})

test_that("candidate trimming removes unused configured traits", {
  candidate_specification <- list(
    config_data = tsbiomass:::merge_config_sections(
      minimal_config_data(),
      list(
        similarity = list(
          species_traits = list(
            swimbladder_type = 5,
            body_shape = 3,
            family = 1,
            genus = 1,
            species = 1,
            fao_area = 1,
            trophic = 1,
            habitat_vertical = 1
          ),
          study_traits = list(
            length_metric = 1,
            fao_area = 1,
            equation_form = 1,
            pressure_corrected = 1,
            season = 1,
            diel = 1
          )
        )
      )
    ),
    registry_path = trait_registry_path()
  )

  species_table <- tibble::tibble(
    species_name = "Alpha alpha",
    genus = "Alpha",
    species = "alpha",
    family = "Fam1",
    order = "Ord1",
    swimbladder_type = "physoclist",
    fao_area = "61",
    trophic = 3.5,
    habitat_vertical = "pelagic",
    temperature_min = 5,
    temperature_max = 14,
    sample_size = 20,
    tags = "test",
    source_type = "fishbase",
    lw_a_g = 0.01,
    lw_b = 3.0
  )

  trimmed_species <- tsbiomass:::trim_species_data(
    data_table = species_table,
    candidate_specification = candidate_specification
  )

  expect_false("temperature_min" %in% names(trimmed_species))
  expect_false("temperature_max" %in% names(trimmed_species))
  expect_true(all(c("swimbladder_type", "family", "genus", "species", "fao_area", "trophic", "habitat_vertical", "lw_a_g", "lw_b") %in% names(trimmed_species)))
  expect_true(all(c("sample_size", "tags", "source_type") %in% names(trimmed_species)))

  candidate_table <- tibble::tibble(
    model_id = 1L,
    model_id_chr = "1",
    species_name = "Alpha alpha",
    genus = "Alpha",
    species = "alpha",
    family = "Fam1",
    order = "Ord1",
    swimbladder_type = "physoclist",
    slope_len = 20,
    intercept_len = -70,
    frequency = 38,
    equation_form = "standardized_length",
    fao_area = "61",
    trophic = 3.5,
    habitat_vertical = "pelagic",
    season = "summer",
    diel = "day",
    pressure_corrected = TRUE,
    temperature_min = 5,
    temperature_max = 14,
    sample_size = 20
  )

  trimmed_candidates <- tsbiomass:::trim_candidate_data(
    data_table = candidate_table,
    candidate_specification = candidate_specification
  )

  expect_false("temperature_min" %in% names(trimmed_candidates))
  expect_false("temperature_max" %in% names(trimmed_candidates))
  expect_false("sample_size" %in% names(trimmed_candidates))
  expect_true(all(c("model_id", "slope_standard", "intercept_standard", "frequency", "equation_form", "fao_area", "season", "diel", "pressure_corrected") %in% names(trimmed_candidates)))
})

test_that("class printing is compact and informative", {
  candidates <- make_candidates()
  selector <- make_selector(candidates = candidates)
  learner <- as_policylearner(selector)

  expect_output(print(candidates), "^Candidates")
  expect_output(print(selector), "^PolicySelector")
  expect_output(print(learner), "^PolicyLearner")
})

test_that("equal-weight starts and range bounds are read from config", {
  cfg <- minimal_config_data()
  parsed <- tsbiomass:::read_similarity_config(cfg)
  normalized <- tsbiomass:::resolve_similarity_setup(
    cfg_user = parsed,
    alpha = parsed$alpha %||% 0.8,
    k_species = parsed$kernel_scale %||% parsed$k_species %||% 4,
    k_study = parsed$kernel_scale %||% parsed$k_study %||% 4
  )

  expect_true(isTRUE(parsed$equal_start_weights))
  expect_equal(normalized$alpha_range, c(0.1, 0.9))
  expect_equal(normalized$kernel_scale_range, c(1, 8))
  expect_equal(normalized$k_species_range, c(1, 8))
  expect_equal(normalized$k_study_range, c(1, 8))
})

test_that("equal-weight starts preserve trait names from named numeric maps", {
  trait_map <- c(swimbladder_type = 5, body_shape = 3, family = 1)
  out <- tsbiomass:::equal_start_weights(trait_map)

  expect_equal(out, c(swimbladder_type = 1, body_shape = 1, family = 1))
})

test_that("tune_similarities accepts Configurer trait maps under equal starts", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  cfg <- build_configurer(minimal_config_data(), base_dir = tempdir())

  testthat::local_mocked_bindings(
    build_tuning_subset = function(candidate_models, ...) {
      tibble::as_tibble(candidate_models)
    },
    run_tuning_grid_search = function(...) {
      list(
        alpha_best = 0.75,
        kernel_scale_best = 3,
        k_species_best = 3,
        k_study_best = 3,
        baseline = tibble::tibble(stage = "baseline", rmse = 0.20),
        grid_scores = tibble::tibble(stage = "grid", rmse = 0.10)
      )
    },
    run_component_dropout = function(...) {
      tibble::tibble(component = "full_model", rmse = 0.10, mae = 0.08, n_eval = 4L)
    },
    apply_component_weights = function(base_sim, ...) {
      expect_equal(base_sim$species_weights, c(genus = 1, family = 1))
      expect_equal(base_sim$study_weights, c(frequency = 1, fao_area = 1))
      list(
        species_weights = c(genus = 2, family = 1),
        study_weights = c(frequency = 0.5, fao_area = 1.5),
        config = list(
          length_coherence = list(method = "overlap", weight = 2),
          depth_coherence = list(method = "overlap", weight = 1),
          frequency_coherence = list(method = "overlap", weight = 1)
        )
      )
    },
    score_similarity_config = function(...) {
      tibble::tibble(rmse = 0.09, mae = 0.07, n_eval = 4L)
    },
    .package = "tsbiomass"
  )

  tuned <- tune_similarities(
    candidate_models = candidates,
    config = cfg
  )

  expect_true(S7::S7_inherits(tuned, Candidates))
})
