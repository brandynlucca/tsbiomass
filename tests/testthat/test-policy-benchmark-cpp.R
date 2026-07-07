cpp_policy_eval_fixture <- function() {
  donors <- tibble::tibble(
    model_id = paste0("d", 1:8),
    species_name = c("A a", "A a", "B b", "B b", "C c", "D d", "E e", "F f"),
    slope_len = c(20, 19, 21, 18, 20, 17, 22, 16),
    intercept_len = c(-70, -68, -72, -66, -69, -65, -74, -64),
    w_adm = c(0.24, 0.18, 0.16, 0.14, 0.11, 0.08, 0.06, 0.03),
    combined_distance = c(0.05, 0.12, 0.20, 0.28, 0.35, 0.44, 0.55, 0.70),
    trait_gower_distance = c(0.03, 0.10, 0.17, 0.25, 0.31, 0.40, 0.49, 0.62),
    taxonomic_distance_to_anchor = c(0, 0, 0.25, 0.25, 0.5, 0.7, 0.8, 0.9),
    d_species = c(0, 0, 0.2, 0.2, 0.45, 0.65, 0.75, 0.85),
    length_overlap_fraction = c(1, .9, .8, .75, .7, .65, .6, .5),
    depth_overlap_fraction = c(.8, .85, .7, .65, .6, .55, .5, .4),
    biomass_multiplier_if_replace = c(1.00, 1.05, .90, 1.15, .95, 1.20, .85, 1.30),
    overlap_same_species = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    overlap_same_genus = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    overlap_same_family = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    overlap_same_fao_area = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    overlap_same_ocean_basin = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
    overlap_same_season = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    study_cell_id = c("s1", "s1", "s2", "s2", "s3", "s4", "s5", "s6"),
    is_group_model = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
    equation_form = "standardized_length",
    derivation_type = "empirical"
  )
  list(
    admissible_df = donors,
    anchor_sigma = 1e-7,
    anchor_pdf = tibble::tibble(
      length_cm = seq(10, 40, length.out = 101),
      f_len = rep(1 / 101, 101)
    )
  )
}

test_that("compiled policy plan collapses repeated donor pools", {
  policies <- c(
    "closest_within_species",
    "weighted_mean_within_species",
    "unweighted_mean_within_species",
    "survey_distance_within_species",
    "closest_within_genus",
    "weighted_mean_within_genus",
    "taxon_distance_within_genus",
    "species_distance_within_genus"
  )
  plan <- tsbiomass:::build_policy_execution_plan(
    policies = policies,
    policy_params = list(equation_branch_filters = "all")
  )
  compiled <- tsbiomass:::compile_policy_execution_plan_cpp(plan)

  expect_s3_class(compiled, "tsb_compiled_policy_plan")
  expect_lt(length(compiled$pool_specs), nrow(plan$plan))
  expect_true(all(vapply(compiled$cpp, is.atomic, logical(1))))
})

test_that("C++ policy engine matches the R oracle column by column", {
  policies <- c(
    "closest_within_species",
    "weighted_mean_within_species",
    "unweighted_mean_within_species",
    "survey_distance_within_species",
    "closest_within_genus",
    "weighted_mean_within_genus",
    "taxon_distance_within_genus",
    "species_distance_within_genus"
  )
  plan <- tsbiomass:::build_policy_execution_plan(
    policies = policies,
    policy_params = list(equation_branch_filters = "all")
  )
  compiled <- tsbiomass:::compile_policy_execution_plan_cpp(plan)
  eval_obj <- cpp_policy_eval_fixture()

  r_result <- tsbiomass:::evaluate_policies(
    eval_obj = eval_obj,
    execution_plan = plan
  )
  cpp_result <- tsbiomass:::evaluate_policies_cpp(
    eval_obj = eval_obj,
    compiled_plan = compiled
  )

  key_cols <- c("policy", "equation_branch_filter", "aggregation_method", "candidate_pool")
  expect_equal(cpp_result[key_cols], r_result[key_cols])
  expect_equal(cpp_result$n_models, r_result$n_models)

  exact_cols <- c(
    "n_valid_models", "realized_n_unique_donors", "realized_donor_fingerprint",
    "local_n_slope20", "local_n_non_slope20", "policy_is_constructed_ensemble"
  )
  for (name in exact_cols) {
    expect_equal(cpp_result[[name]], r_result[[name]], info = name)
  }

  numeric_cols <- c(
    "policy_slope_len", "policy_intercept_len", "policy_sigma_bs_mean", "multiplier_pred",
    "local_min_combined_distance", "local_median_combined_distance",
    "local_weighted_mean_combined_distance", "local_min_trait_gower_distance",
    "local_weighted_mean_trait_gower_distance", "local_min_species_distance",
    "local_weighted_mean_species_distance", "local_mean_length_overlap",
    "local_mean_depth_overlap", "local_effective_support", "local_max_weight",
    "donor_slope_sd", "donor_intercept_sd", "donor_slope_iqr", "donor_intercept_iqr",
    "donor_log_multiplier_abs_dev_median", "donor_log_multiplier_abs_dev_q90",
    "donor_log_sigma_abs_dev_median", "donor_log_sigma_abs_dev_q90",
    "donor_curve_rmse_median", "donor_curve_rmse_q90", "local_structural_q_abs_log"
  )
  for (name in numeric_cols) {
    expect_equal(cpp_result[[name]], r_result[[name]], tolerance = 1e-10, info = name)
  }

  overlap_cols <- grep("^local_n_", names(r_result), value = TRUE)
  for (name in overlap_cols) {
    expect_equal(cpp_result[[name]], r_result[[name]], info = name)
  }
})

test_that("C++ engine matches the complete production policy plan", {
  config <- read_configuration(system.file(
    "templates", "swfscfish_config.yaml",
    package = "tsbiomass"
  ))
  policies <- tsbiomass:::policy_selector_active_policies(config, NULL)
  plan <- tsbiomass:::build_policy_execution_plan(
    policies = policies,
    policy_params = list(
      equation_branch_filters = config$policies$branch
    )
  )
  compiled <- tsbiomass:::compile_policy_execution_plan_cpp(plan)
  eval_obj <- cpp_policy_eval_fixture()

  r_result <- tsbiomass:::evaluate_policies(
    eval_obj = eval_obj,
    execution_plan = plan
  )
  cpp_result <- tsbiomass:::evaluate_policies_cpp(
    eval_obj = eval_obj,
    compiled_plan = compiled
  )

  expect_equal(nrow(cpp_result), 366L)
  expect_equal(
    cpp_result[c("policy", "equation_branch_filter", "candidate_pool", "aggregation_method")],
    r_result[c("policy", "equation_branch_filter", "candidate_pool", "aggregation_method")]
  )
  expect_equal(cpp_result$n_models, r_result$n_models)
  expect_equal(cpp_result$n_valid_models, r_result$n_valid_models)
  expect_equal(cpp_result$realized_donor_fingerprint, r_result$realized_donor_fingerprint)

  numeric_cols <- c(
    "policy_slope_len", "policy_intercept_len", "policy_sigma_bs_mean", "multiplier_pred",
    "local_weighted_mean_combined_distance", "local_effective_support",
    "donor_log_multiplier_abs_dev_q90", "donor_log_sigma_abs_dev_q90",
    "donor_curve_rmse_q90", "local_structural_q_abs_log"
  )
  for (name in numeric_cols) {
    expect_equal(cpp_result[[name]], r_result[[name]], tolerance = 1e-10, info = name)
  }
})

test_that("C++ engine preserves R fallback semantics for absent distance columns", {
  policies <- c(
    "closest_across_all_admissible",
    "taxon_distance_across_all_admissible",
    "species_distance_across_all_admissible"
  )
  plan <- tsbiomass:::build_policy_execution_plan(
    policies = policies,
    policy_params = list(equation_branch_filters = "all")
  )
  compiled <- tsbiomass:::compile_policy_execution_plan_cpp(plan)
  eval_obj <- cpp_policy_eval_fixture()
  eval_obj$admissible_df$taxonomic_distance_to_anchor <- NULL
  eval_obj$admissible_df$d_species <- NULL

  r_result <- tsbiomass:::evaluate_policies(
    eval_obj = eval_obj,
    execution_plan = plan
  )
  cpp_result <- tsbiomass:::evaluate_policies_cpp(
    eval_obj = eval_obj,
    compiled_plan = compiled
  )

  expect_setequal(names(cpp_result), names(r_result))
  expect_equal(cpp_result[names(r_result)], r_result, tolerance = 1e-10)
})

test_that("C++ TS-error reconstruction matches the R oracle", {
  candidates <- tibble::tibble(
    model_id = c("a1", "a2"),
    species_name = c("Species one", "Species two"),
    slope_standard = c(20, 41.6),
    intercept_standard = c(-70, -104.7),
    study_length_min = c(10, 18),
    study_length_max = c(30, 55)
  )
  policy_perf <- tibble::tibble(
    anchor_model_id = c("a1", "a1", "a1", "a2"),
    policy = c(
      "closest_within_species", "weighted_mean_within_genus",
      "closest_within_species", "closest_within_species"
    ),
    equation_branch_filter = c("all", "all", "all", "all"),
    policy_slope_len = c(19, 21, 19, 40.93),
    policy_intercept_len = c(-69, -72, -69, -103.5),
    local_min_combined_distance = c(0.1, 0.2, 0.1, 0.3),
    local_weighted_mean_combined_distance = c(0.15, 0.25, 0.15, 0.35),
    local_effective_support = c(2, 3, 2, 1),
    local_mean_length_overlap = c(0.8, 0.7, 0.8, 0.9),
    local_mean_depth_overlap = c(0.6, 0.5, 0.6, 0.4)
  )
  config <- tsbiomass:::merge_config_sections(
    list(fields = list(
      model_id = "model_id",
      species = "species_name",
      slope = "slope_standard",
      intercept = "intercept_standard"
    )),
    tsbiomass:::default_anchor_config(NULL)
  )

  r_result <- tsbiomass:::build_benchmark_ts_error_table(
    candidate_models = candidates,
    policy_perf = policy_perf,
    config = config
  )
  cpp_result <- tsbiomass:::build_benchmark_ts_error_table_cpp(
    candidate_models = candidates,
    policy_perf = policy_perf,
    config = config
  )

  expect_identical(names(cpp_result), names(r_result))
  character_cols <- c(
    "anchor_model_id", "anchor_species", "policy", "equation_branch_filter"
  )
  expect_equal(cpp_result[character_cols], r_result[character_cols])
  for (name in setdiff(names(r_result), character_cols)) {
    expect_equal(cpp_result[[name]], r_result[[name]], tolerance = 1e-12, info = name)
  }
})

test_that("species blocking does not purge generalized models as one synthetic species", {
  eval_obj <- list(
    admissible_df = tibble::tibble(
      model_id = c("g1", "g2"),
      species_name = c("NA NA", "NA NA"),
      overlap_same_species = c(TRUE, TRUE),
      w_adm = c(0.6, 0.4),
      cumulative_w_adm = c(0.6, 1)
    ),
    model_eval = tibble::tibble(
      model_id = c("g1", "g2"),
      species_name = c("NA NA", "NA NA")
    )
  )
  anchor <- tibble::tibble(
    model_id = "g0",
    species_name = "NA NA",
    is_group_model = TRUE
  )
  config <- tsbiomass:::merge_config_sections(
    list(fields = list(species = "species_name")),
    tsbiomass:::default_anchor_config(NULL)
  )

  blocked <- tsbiomass:::remove_species_support(eval_obj, anchor, config)
  expect_equal(blocked, eval_obj)
})
