# Resolve an inst/ file path in both installed-package and source-package tests.
tsbiomass_test_file <- function(...) {
  pkg_path <- system.file(..., package = "tsbiomass")
  if (nzchar(pkg_path)) {
    return(pkg_path)
  }

  normalizePath(
    file.path(testthat::test_path("..", ".."), "inst", ...),
    winslash = "/",
    mustWork = TRUE
  )
}

trait_registry_path <- function() {
  tsbiomass_test_file("templates", "trait_registry.json")
}

policy_registry_path <- function() {
  tsbiomass_test_file("templates", "policy_registry.json")
}

minimal_config_data <- function() {
  list(
    paths = list(
      input = "input.xlsx",
      output_root = "outputs",
      cache_folder = "cache",
      support_folder = "supplemental",
      area_file = "fao.csv",
      log_path = "logs/run.log"
    ),
    execution = list(
      strict_pdf = FALSE,
      run_multiplier = FALSE,
      write_log = FALSE
    ),
    tuning = list(
      species_model_limit = 2L,
      resamples = 3L,
      equal_start_weights = TRUE
    ),
    similarity = list(
      alpha = 0.8,
      kernel_scale = 4,
      alpha_range = list(from = 0.1, to = 0.9),
      kernel_scale_range = list(from = 1, to = 8),
      core_weight_cutoff = 0.8,
      conformal_alpha = 0.10,
      species_traits = list(genus = 1, family = 0.5),
      study_traits = list(frequency = 1, fao_area = 1),
      coherence = list(
        length = list(mode = "overlap", weight = 2),
        depth = list(mode = "overlap", weight = 1),
        frequency = list(mode = "overlap", weight = 1, gap = 60)
      )
    ),
    policy = list(
      alpha = 0.8,
      k_species = 4,
      k_study = 4,
      frequency_mode = "overlap",
      exact_frequency = FALSE,
      frequency_gap = 60,
      length_overlap_min = 0.25,
      depth_overlap_min = 0.25,
      key_metadata_max = 0.25,
      length_weight = 2,
      depth_weight = 1,
      frequency_weight = 1,
      core_weight_cutoff = 0.8,
      conformal_alpha = 0.10,
      species_traits = list(genus = 1, family = 0.5),
      study_traits = list(frequency = 1, fao_area = 1)
    ),
    admissibility = list(
      key_metadata_max = 0.25,
      species_traits = character(0),
      study_traits = character(0),
      coherence = list(
        length = list(mode = "overlap", min = 0.25),
        depth = list(mode = "overlap", min = 0.25),
        frequency = list(mode = "none", gap = 60)
      )
    ),
    policies = list(
      active = c("closest_within_species", "weighted_mean_within_genus"),
      equation_branch_filters = c("all", "fixed20_only")
    ),
    metalearner = list(
      selection_method = "glm",
      n_folds = 5L,
      inner_folds = 5L,
      workers = 1L,
      seed = 20260524L,
      outcome_col = "error_abs_log",
      outcome_transform = "log1p",
      lambda_rule = "lambda.1se",
      metalearner_loss = "squared_error",
      max_selection_tolerance = 1e-12,
      method_settings = list(
        gam = list(
          fit_method = "REML",
          select_terms = TRUE
        ),
        ranger = list(
          num_trees = 200L,
          min_node_size = 3L,
          sample_fraction = 0.8,
          replace = TRUE,
          respect_unordered_factors = "order"
        ),
        xgboost = list(
          nrounds = 25L,
          eta = 0.2,
          max_depth = 4L,
          min_child_weight = 1,
          subsample = 0.9,
          colsample_bytree = 0.9,
          lambda = 1,
          alpha = 0
        )
      )
    )
  )
}

minimal_similarity_config <- function() {
  list(
    species_traits = list(genus = 1, family = 1),
    study_traits = list(frequency = 1, fao_area = 1),
    alpha = 0.65,
    kernel_scale = 2,
    alpha_range = list(from = 0.45, to = 0.85),
    kernel_scale_range = list(from = 1, to = 3),
    length_coherence = list(method = "overlap", weight = 1),
    depth_coherence = list(method = "overlap", weight = 1),
    frequency_coherence = list(method = "overlap", weight = 1),
    seed = 42L
  )
}

minimal_candidate_models <- function() {
  tibble::tibble(
    model_id = 1:4,
    model_id_chr = as.character(1:4),
    species_name = c("Alpha alpha", "Alpha alpha", "Beta beta", "Gamma gamma"),
    genus = c("Alpha", "Alpha", "Beta", "Gamma"),
    species = c("alpha", "alpha", "beta", "gamma"),
    family = c("Fam1", "Fam1", "Fam2", "Fam3"),
    order = c("Ord1", "Ord1", "Ord2", "Ord3"),
    common = c("Alpha fish", "Alpha fish", "Beta fish", "Gamma fish"),
    swimbladder_type = c("physoclist", "physoclist", "physostome", "physoclist"),
    fao_area = c("61", "61", "67", "61"),
    ocean_basin = c("pacific", "pacific", "atlantic", "pacific"),
    frequency = c(38, 38, 70, 120),
    freq_label = c("38", "38", "70", "120"),
    regional_body = c("SWFSC", "SWFSC", "AFSC", "SWFSC"),
    equation_form = rep("standardized_length", 4),
    derivation_type = rep("empirical", 4),
    reference_tsl_short = c("study_a", "study_b", "study_c", "study_d"),
    is_group_model = FALSE,
    length_min = c(10, 12, 20, 15),
    length_max = c(30, 25, 35, 28),
    study_length_min = c(10, 12, 20, 15),
    study_length_max = c(30, 25, 35, 28),
    study_length_midpoint = c(20, 18.5, 27.5, 21.5),
    depth_min = c(5, 10, 15, 8),
    depth_max = c(50, 60, 80, 55),
    slope_len = c(20, 18, 20, 17),
    intercept_len = c(-70, -68, -65, -67),
    lw_a_g = c(0.012, 0.013, 0.011, 0.014),
    lw_b = c(3.0, 3.1, 2.9, 3.0)
  )
}

minimal_similarity_tuning <- function() {
  list(
    config_tuned = list(
      species_weights = c(genus = 1, family = 0.5),
      study_weights = c(frequency = 1, fao_area = 1),
      alpha = 0.65,
      kernel_scale = 2,
      k_species = 2,
      k_study = 2,
      coherence = list(
        length_coherence = list(method = "overlap", weight = 1),
        depth_coherence = list(method = "overlap", weight = 1),
        frequency_coherence = list(method = "overlap", weight = 1)
      )
    ),
    component_impact_summary = tibble::tibble(
      component = c("full_model", "genus", "family", "frequency"),
      delta_rmse = c(0.00, 0.14, 0.09, 0.05),
      delta_mae = c(0.00, 0.11, 0.07, 0.04)
    ),
    component_weights = tibble::tibble(
      resample_id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
      component = c("genus", "family", "frequency", "genus", "family", "frequency", "genus", "family", "frequency"),
      component_type = c("species_trait", "species_trait", "study_trait", "species_trait", "species_trait", "study_trait", "species_trait", "species_trait", "study_trait"),
      base_weight = c(1.0, 0.5, 1.0, 1.0, 0.5, 1.0, 1.0, 0.5, 1.0),
      tuned_weight = c(1.20, 0.45, 0.90, 1.10, 0.50, 1.05, 0.95, 0.55, 1.10),
      multiplier = c(1.20, 0.90, 0.90, 1.10, 1.00, 1.05, 0.95, 1.10, 1.10),
      delta_rmse = c(0.14, 0.09, 0.05, 0.14, 0.09, 0.05, 0.14, 0.09, 0.05),
      delta_mae = c(0.11, 0.07, 0.04, 0.11, 0.07, 0.04, 0.11, 0.07, 0.04)
    )
  )
}

minimal_similarity_matrix <- function(candidate_models = minimal_candidate_models()) {
  list(
    candidate_models = candidate_models,
    species_traits = c("genus", "family"),
    study_traits = c("frequency", "fao_area"),
    species_weights = c(genus = 1, family = 0.5),
    study_weights = c(frequency = 1, fao_area = 1),
    alpha = 0.65,
    kernel_scale = 2,
    k_species = 2,
    k_study = 2,
    config = list(
      length_coherence = list(method = "overlap", weight = 1),
      depth_coherence = list(method = "overlap", weight = 1),
      frequency_coherence = list(method = "overlap", weight = 1)
    ),
    species_profiles = candidate_models |>
      dplyr::distinct(.data$species_name, .data$genus, .data$family),
    species_matrix_weights = c(genus = 1, family = 0.5),
    study_data = candidate_models |>
      dplyr::select(frequency, fao_area),
    study_matrix_weights = c(frequency = 1, fao_area = 1),
    species_trait_defs = list(),
    study_trait_defs = list(),
    species_component_lookup = c(genus = "genus", family = "family"),
    study_component_lookup = c(frequency = "frequency", fao_area = "fao_area")
  )
}

minimal_gower_distances <- function(candidate_models = minimal_candidate_models()) {
  n <- nrow(candidate_models)
  model_ids <- candidate_models$model_id_chr
  species_names <- unique(candidate_models$species_name)

  combined <- matrix(
    c(
      0, 0.1, 0.6, 0.4,
      0.1, 0, 0.55, 0.35,
      0.6, 0.55, 0, 0.7,
      0.4, 0.35, 0.7, 0
    ),
    nrow = n,
    byrow = TRUE,
    dimnames = list(model_ids, model_ids)
  )

  species_dist <- matrix(
    c(
      0, 0.8, 0.9,
      0.8, 0, 0.7,
      0.9, 0.7, 0
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(species_names, species_names)
  )

  list(
    species_dist = species_dist,
    study_dist = combined,
    species_dist_model = combined,
    combined_dist = combined,
    trait_cols = c("genus", "family", "frequency", "fao_area")
  )
}

minimal_admissibility_scores <- function() {
  tibble::tibble(
    anchor_model_id = c("1", "1", "2", "2", "4", "4"),
    anchor_species = c(
      "Alpha alpha",
      "Alpha alpha",
      "Alpha alpha",
      "Alpha alpha",
      "Gamma gamma",
      "Gamma gamma"
    ),
    admissible = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    gate_missing_key_metadata = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
}

minimal_policy_performance <- function() {
  tibble::tibble(
    anchor_model_id = c("1", "1", "4", "4"),
    anchor_species = c("Alpha alpha", "Alpha alpha", "Gamma gamma", "Gamma gamma"),
    policy = c(
      "closest_within_species",
      "weighted_mean_within_genus",
      "closest_within_species",
      "weighted_mean_within_genus"
    ),
    equation_branch_filter = "all",
    error_abs_log = c(0.10, 0.22, 0.08, 0.18),
    multiplier_pred = c(1.10, 1.30, 1.15, 1.25),
    valid_prediction = TRUE,
    n_valid_models = c(3L, 4L, 2L, 5L),
    local_weighted_mean_combined_distance = c(0.10, 0.25, 0.15, 0.20),
    local_effective_support = c(3.0, 4.0, 2.0, 5.0),
    local_structural_q_abs_log = c(0.05, 0.10, 0.04, 0.08)
  )
}

minimal_crossfit_predictions <- function() {
  tibble::tibble(
    anchor_model_id = c("1", "1", "4", "4"),
    anchor_species = c("Alpha alpha", "Alpha alpha", "Gamma gamma", "Gamma gamma"),
    policy = c(
      "closest_within_species",
      "weighted_mean_within_genus",
      "closest_within_species",
      "weighted_mean_within_genus"
    ),
    equation_branch_filter = "all",
    error_abs_log = c(0.11, 0.24, 0.09, 0.19),
    .meta_predicted_score = c(0.12, 0.30, 0.20, 0.10),
    n_valid_models = c(3L, 4L, 2L, 5L),
    local_weighted_mean_combined_distance = c(0.10, 0.25, 0.15, 0.20),
    local_effective_support = c(3.0, 4.0, 2.0, 5.0),
    local_structural_q_abs_log = c(0.05, 0.10, 0.04, 0.08)
  )
}

minimal_selection_ref <- function() {
  tibble::tibble(
    policy = c("closest_within_species", "weighted_mean_within_genus"),
    equation_branch_filter = "all",
    mean_species_median_abs_log = c(0.10, 0.20),
    acceptable_one_se = c(TRUE, FALSE),
    acceptable_bootstrap = c(TRUE, FALSE),
    acceptable_global = c(TRUE, FALSE),
    equivalent_to_best_global = c(TRUE, FALSE),
    paired_mean_diff_to_best = c(0, 0.10),
    one_se_threshold = c(0.15, 0.15),
    bootstrap_prob_within_threshold = c(0.90, 0.40),
    bootstrap_prob_best = c(0.85, 0.15),
    bootstrap_median_rank = c(1, 2),
    specificity_rank = c(1, 2),
    equivalence_class_id = c("eq1", "eq2"),
    equivalence_class_size = c(1, 1),
    equivalence_class_members = c("closest_within_species", "weighted_mean_within_genus")
  )
}

minimal_uncertainty <- function() {
  list(
    conf_cal = tibble::tibble(
      policy = c("closest_within_species", "weighted_mean_within_genus"),
      equation_branch_filter = "all",
      q_abs_log = c(0.10, 0.20),
      n = c(5L, 5L),
      median_abs_log = c(0.08, 0.18)
    ),
    pseudo_sum = list(
      overall = tibble::tibble(
        policy = c("closest_within_species", "weighted_mean_within_genus"),
        equation_branch_filter = "all",
        benchmark_label = "pseudo_anchor",
        empirical_coverage = c(0.95, 0.80),
        median_interval_log_width = c(0.20, 0.40)
      )
    ),
    species_sum = list(
      overall = tibble::tibble(
        policy = c("closest_within_species", "weighted_mean_within_genus"),
        equation_branch_filter = "all",
        benchmark_label = "species_block",
        empirical_coverage = c(0.90, 0.75),
        median_interval_log_width = c(0.20, 0.40)
      )
    )
  )
}

minimal_predictions <- function() {
  PolicyPredictions(
    intervals = tibble::tibble(
      anchor_model_id = c("1", "1"),
      anchor_species = c("Alpha alpha", "Alpha alpha"),
      policy = c("closest_within_species", "weighted_mean_within_genus"),
      equation_branch_filter = "all",
      multiplier_pred = c(1.1, 1.3)
    ),
    selections = tibble::tibble(
      anchor_model_id = c("1", "4"),
      anchor_species = c("Alpha alpha", "Gamma gamma"),
      selected_policy = c("closest_within_species", "closest_within_species"),
      selected_policy_display = c("closest_within_species", "closest_within_species"),
      selected_equation_branch_filter = "all",
      equivalent_policy_set = c("closest_within_species", "closest_within_species"),
      equivalent_policy_set_n = c(1L, 1L),
      equivalence_class_id = c("eq1", "eq1"),
      equivalence_class_size = c(1L, 1L),
      equivalence_class_members = c("closest_within_species", "closest_within_species"),
      multiplier_pred = c(1.1, 1.2),
      multiplier_lo = c(1.0, 1.1),
      multiplier_hi = c(1.2, 1.3),
      interval_log_width = c(0.2, 0.2),
      local_support_mass = c(0.8, 0.7),
      local_effective_support = c(3, 2),
      local_mean_combined_distance = c(0.1, 0.15),
      local_mean_length_overlap = c(0.7, 0.6),
      local_mean_depth_overlap = c(0.8, 0.5),
      local_weighted_missingness = c(0.1, 0.2)
    ),
    consensus = tibble::tibble(
      anchor_model_id = c("1", "4"),
      anchor_species = c("Alpha alpha", "Gamma gamma"),
      consensus_multiplier = c(1.15, 1.18)
    )
  )
}

make_candidates <- function(seed_similarity_tuning = TRUE,
                            similarity_matrix = list(),
                            gower_distances = list(),
                            ordination = list(),
                            admissibility = list()) {
  models <- minimal_candidate_models()
  species_db <- models |>
    dplyr::distinct(.data$species_name, .data$genus, .data$family, .data$order, .data$swimbladder_type, .data$fao_area, .data$ocean_basin)

  Candidates(
    spec = list(
      study = list(path = "input.xlsx"),
      sources = list(),
      enrich = list(precedence = character()),
      prepare = list(),
      anchors = list(selector = list(regional_body = "SWFSC"))
    ),
    study_db = models |>
      dplyr::distinct(.data$species_name, .data$regional_body),
    species_vector = unique(models$species_name),
    source_dbs = list(),
    species_db = species_db,
    candidate_models = models,
    reference_anchors = models[0, , drop = FALSE],
    similarity_matrix = similarity_matrix,
    gower_distances = gower_distances,
    ordination = ordination,
    admissibility = admissibility,
    similarity_tuning = if (seed_similarity_tuning) minimal_similarity_tuning() else list()
  )
}

make_selector <- function(candidates = make_candidates(),
                          benchmark = list(),
                          uncertainty = list(),
                          selection = list(),
                          config = NULL) {
  if (is.null(config)) {
    config <- list(
      policy = list(
        missing_key_metadata_max_fraction = 0.25
      ),
      policies = list(
        active = c("closest_within_species", "weighted_mean_within_genus")
      )
    )
  }

  PolicySelector(
    candidates = candidates,
    config = config,
    benchmark = benchmark,
    uncertainty = uncertainty,
    selection = selection
  )
}
