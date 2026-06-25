#!/usr/bin/env Rscript

# Cross-fitted meta-policy run for a configured TS-length model-transfer
# benchmark.

# pkgload::load_all(getwd(), export_all = TRUE, helpers = FALSE, quiet = TRUE)
install.packages(
  "C:/Users/Brandyn/Desktop/ts-model-transferability/tsbiomass",
  repos = NULL, type = "source"
)
library(tsbiomass)

# Resolve the workflow YAML explicitly from the environment so this script
# never hard-codes one packaged config file.
# workflow_config_path <- Sys.getenv("TSBIOMASS_WORKFLOW_CONFIG", "")
# if (!nzchar(workflow_config_path)) {
workflow_config_path <- file.path(getwd(), "inst", "templates", "swfscfish_config.yaml")
# }
# Ingestion now validates and normalizes the YAML immediately so the script
# starts from one checked workflow object.
workflow_config <- read_config(
  workflow_config_path,
  base_dir = dirname(tsbiomass:::path_absolute(workflow_config_path))
  # base_dir = workflow_config_path
)
workflow_config_s7 <- as_configurer(workflow_config)
cache_dir <- workflow_config$paths$cache_dir
output_dir <- workflow_config$paths$out_root

# if (command_line_true(Sys.getenv("TSBIOMASS_META_CLEAR_FIGURES", "true"))) {
#   stale_figures <- list.files(
#     output_dir,
#     pattern = "\\.(png|pdf|svg)$",
#     recursive = TRUE,
#     full.names = TRUE,
#     ignore.case = TRUE
#   )
#   if (length(stale_figures) > 0) {
#     unlink(stale_figures, force = TRUE)
#   }
#   tsb_message("Cleared ", length(stale_figures), " stale figure file(s) from ", output_dir, ".")
# }
# meta_cfg <- workflow_config$metalearner %||% list()
meta_output_dir <- file.path(output_dir, "meta_policy_outputs")
# if (command_line_true(Sys.getenv("TSBIOMASS_META_CLEAR_FIGURES", "true"))) {
#   obsolete_output_dirs <- file.path(
#     meta_output_dir,
#     c(
#       "meta_policy_admissible_anchor_ranges",
#       "meta_policy_anchor_missingness",
#       "meta_policy_biological_leverage_by_anchor",
#       "meta_policy_biomass_sensitivity_by_anchor",
#       "meta_policy_pairwise_pivots_by_anchor",
#       "meta_policy_pivot_variance_by_anchor",
#       "meta_policy_length_density_by_anchor",
#       "meta_policy_top_candidates_by_anchor",
#       "meta_policy_top_ten_weights_by_anchor",
#       "meta_policy_ts_ribbons_by_anchor"
#     )
#   )
#   stale_dir_count <- sum(dir.exists(obsolete_output_dirs))
#   if (stale_dir_count > 0) {
#     unlink(obsolete_output_dirs[dir.exists(obsolete_output_dirs)], recursive = TRUE, force = TRUE)
#   }
#   tsb_message("Cleared ", stale_dir_count, " obsolete meta-policy output director", if (identical(stale_dir_count, 1L)) "y" else "ies", ".")
# }
# meta_use_support_bin_intervals <- isTRUE(meta_cfg$use_support_bin_intervals %||% FALSE)
# refresh_workflow <- TRUE #command_line_true(Sys.getenv("TSBIOMASS_REFRESH", "true"))
# meta_ts_panels_enabled <- TRUE # command_line_true(Sys.getenv("TSBIOMASS_META_TS_PANELS", "true"))
# meta_conjurer_draws_env <- "" # suppressWarnings(as.integer(Sys.getenv("TSBIOMASS_META_CONJURER_DRAWS", "")))
# meta_conjurer_draws <- if (length(meta_conjurer_draws_env) == 1 &&
#   is.finite(meta_conjurer_draws_env) &&
#   meta_conjurer_draws_env >= 1) {
#   as.integer(meta_conjurer_draws_env)
# } else {
#   NULL
# }
#
# benchmark_path <- workflow_config$benchmark$cache_path %||% file.path(cache_dir, "policy_benchmark.rds")
# anchor_interval_path <- file.path(output_dir, "anchor_policy_intervals.csv")
# rebuild_benchmark_for_ts <- TRUE
# if (!refresh_workflow && file.exists(benchmark_path) && file.exists(anchor_interval_path)) {
#   benchmark_cache <- readRDS(benchmark_path)
#   if (isTRUE(meta_ts_panels_enabled) &&
#       (is.null(benchmark_cache$policy_ts_error) || nrow(benchmark_cache$policy_ts_error) == 0)) {
#     tsb_message("Benchmark cache lacks TS-error calibration rows; rebuilding benchmark for TS conformal panels.")
#     rebuild_benchmark_for_ts <- TRUE
#   }
# }
#
# workflow_config$similarity$refresh <- refresh_workflow
# workflow_config$admissibility$refresh <- refresh_workflow
# workflow_config$uncertainty$refresh <- refresh_workflow
# workflow_config$selection$refresh <- refresh_workflow
# workflow_config$simulation$refresh <- refresh_workflow
# workflow_config$benchmark$refresh <- isTRUE(refresh_workflow || rebuild_benchmark_for_ts)
# workflow_config$benchmark$include_ts_error <- meta_ts_panels_enabled
# workflow_config_s7@data <- workflow_config


dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(meta_output_dir, recursive = TRUE, showWarnings = FALSE)

# deprecated_meta_outputs <- file.path(
#   meta_output_dir,
#   c(
#     "meta_policy_admissible_anchor_ranges.png",
#     "meta_policy_anchor_audit.png",
#     "meta_policy_anchor_missingness.png",
#     "meta_policy_candidate_multiplier_lines.png",
#     "meta_policy_multiplier_vs_expected_length.png",
#     "meta_policy_slope_distribution.png",
#     "meta_policy_sensitivity_summary.png",
#     "meta_policy_uncertainty_driver_heatmap.png",
#     "meta_policy_uncertainty_importance_overall.png",
#     "selected_policy_species_rank_diagnostics.png"
#   )
# )
# deprecated_meta_dirs <- file.path(
#   meta_output_dir,
#   c(
#     "meta_policy_top_candidates_by_anchor",
#     "meta_policy_top_ten_weights_by_anchor",
#     "meta_policy_ts_ribbons_by_anchor",
#     "meta_policy_uncertainty_driver_by_anchor"
#   )
# )
# file.remove(deprecated_meta_outputs[file.exists(deprecated_meta_outputs)])
# for (deprecated_dir in deprecated_meta_dirs) {
#   if (dir.exists(deprecated_dir)) {
#     unlink(deprecated_dir, recursive = TRUE, force = TRUE)
#   }
# }

tsbiomass:::tsb_message("Meta-policy run: building staged candidate object.")
candidates <- as_candidates(workflow_config_s7)
candidates <- set_reference_anchors(candidates)

candidate_models <- candidates@candidate_models
reference_anchors <- candidates@reference_anchors
anchor_ids <- if ("model_id_chr" %in% names(reference_anchors)) {
  as.character(reference_anchors$model_id_chr)
} else {
  as.character(reference_anchors$model_id)
}

# LEARN PAIRWISE DISTANCES VIA SUPERVISED METRIC LEARNING
tsbiomass:::tsb_message("Constructing Alchemist and learning pairwise distances...")
alchemist <- as_alchemist(candidates, config = workflow_config_s7)
alchemist <- forge_distances(alchemist)
alchemist <- distill_traits(alchemist)
alchemist <- run_ordination(alchemist, include_loadings = TRUE, include_centroids = TRUE)
alchemist <- screen_admissibility(alchemist)

plot(candidates)
plot(alchemist, type = "trait_importance")
plot(alchemist, type = "admissibility")
plot(alchemist, type = "admissibility", view = "overlap_profile")
plot(alchemist, type = "ordination")
plot(alchemist, type = "ordination", view = "vectors")
plot(alchemist, type = "ordination", view = "centers")

sel <- as_selector(alchemist, config = workflow_config_s7)
sel <- benchmark(
  sel,
  include_ts_error = TRUE
)
sel <- calibrate_uncertainty(
  sel
)
learn <- as_policy_learner(sel, config = workflow_config_s7)
learn <- crossfit(learn)
learn <- fit(learn)
learn <- calibrate_uncertainty(learn)
sel <- select_policies(
  sel
)
sel_preds <- predict(sel)
pred_bundle <- predict(
  sel,
  learner = learn
)
ref <- as_referee(
  sel,
  learner = learn,
  predictions = pred_bundle,
  config = workflow_config_s7
)
sc <- predict(ref)
ref <- referee_rebuild(ref, scorecard = sc)

plot(sc, type = "coefficient_uncertainty")
plot(sc, type = "selected_intervals", reference_label = "Super Learner Meta-Policy")
plot(sc, type = "field_missingness")
plot(sc, type = "ts_length_conformal")
plot(sc, type = "ts_length_multiplier")
plot(sc, type = "strategy_competition")
plot(ref, type = "biomass_change")


save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_selected_intervals.png"),
  plot(scorecard, type = "selected_intervals", reference_label = "Super Learner Meta-Policy"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_field_missingness.png"),
  plot(scorecard, type = "field_missingness"),
  width = 8,
  height = 5
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_anchor_multiplier_summary.png"),
  plot(referee, type = "anchor_multiplier_summary"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_all_strategy_intervals_panel.png"),
  plot(scorecard, type = "strategy_competition"),
  width = 15,
  height = 7.5
)

if (nrow(meta_anchor_selected) > 0) {
  save_plot_if_possible(
    file.path(meta_output_dir, "swfsc_ts_conformal_panels.png"),
    plot(scorecard, type = "ts_length_conformal"),
    width = 11.5,
    height = 9.2
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_multiplier_length_spectrum.png"),
    plot(scorecard, type = "ts_length_multiplier"),
    width = 11,
    height = 8.5
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_anchor_biomass_multipliers.png"),
    plot(scorecard, type = "selected_multiplier_summary"),
    width = 9,
    height = 6
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_anchor_length_density_panel.png"),
    plot(referee, type = "length_density"),
    width = 10,
    height = 7
  )



trait_importance_tbl <- alchemist@trait_importance$importance_tbl
utils::write.csv(
  trait_importance_tbl,
  file.path(output_dir, "alchemist_trait_importance.csv"),
  row.names = FALSE
)
tsbiomass:::tsb_message(
  "Alchemist distances forged. alpha-equivalent = ",
  round(alchemist@trait_importance$alpha_equiv, 3),
  timestamp = FALSE
)
# candidate_models <- candidates@candidate_models


# tsbiomass:::tsb_message("Meta-policy run: preparing similarity, tuning similarity, ordination, and admissibility.")
# candidates <- prepare_similarity_matrix(
#   candidate_models = candidates,
#   config = workflow_config_s7
# )
# candidates <- tune_similarity_matrix(
#   candidate_models = candidates,
#   config = workflow_config_s7
# )
# similarity_tuning_obj <- candidates@similarity_tuning
# similarity_config <- similarity_tuning_obj$config_tuned
# # workflow_config_s7@data$ordination$nmds_args <- list(try=6, trymax=12, autotransform = FALSE, trace=  FALSE)

# candidates <- build_gower_distances(candidates)
# candidates <- run_ordination(candidates)
# candidates <- screen_admissibility(
#   candidate_models = candidates,
#   config = workflow_config_s7
# )
# candidates <- build_candidates_uncertainty_diagnostics(
#   candidates = candidates,
#   config = workflow_config_s7
# )

candidate_models <- candidates@candidate_models
reference_anchors <- candidates@reference_anchors
anchor_admissibility_obj <- candidates@admissibility
model_scores <- candidates@ordination$model$model_scores %||% tibble::tibble()
species_lookup <- candidates@ordination$species_lookup %||% tibble::tibble()

if (length(similarity_tuning_obj) > 0) {
  utils::write.csv(
    tibble::as_tibble(similarity_tuning_obj$component_weights %||% tibble::tibble()),
    file.path(meta_output_dir, "similarity_tuned_component_weights.csv"),
    row.names = FALSE
  )
}

save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_fao_study_distribution.png"),
  plot(candidates),
  width = 10,
  height = 7
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_fao_model_distribution.png"),
  plot(candidates, count_type = "models"),
  width = 10,
  height = 7
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_ordination_combined_clusters.png"),
  plot(candidates, type = "ordination", dissimilarity = "combined", view = "clusters"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_ordination_combined_vectors.png"),
  plot(candidates, type = "ordination", dissimilarity = "combined", view = "vectors"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_ordination_combined_centers.png"),
  plot(candidates, type = "ordination", dissimilarity = "combined", view = "centers"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_ordination_species.png"),
  plot(candidates, type = "ordination", dissimilarity = "species"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_ordination_study_clusters.png"),
  plot(candidates, type = "ordination", dissimilarity = "study", view = "clusters"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_overlap_heatmap.png"),
  plot(candidates, type = "admissibility", view = "overlap_profile"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_admissibility_gate_composition.png"),
  plot(candidates, type = "admissibility", view = "gate_composition"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_empirical_similarity_component_importance.png"),
  plot(candidates, type = "similarity_tuning", view = "component_importance"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_similarity_tuning_variation.png"),
  plot(candidates, type = "similarity_tuning", view = "tuning_variation"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_component_importance_by_anchor.png"),
  plot(candidates, type = "component_importance", view = "panel"),
  width = 11,
  height = 8.5
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_slope_group.png"),
  plot(candidates, type = "slope_group"),
  width = 8,
  height = 5
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_slope_support.png"),
  plot(candidates, type = "slope_support"),
  width = 8,
  height = 5
)
LI
tsb_message("Meta-policy run: benchmarking, calibrating, selecting, and predicting policies.")
selector <- as_policy_selector(candidates, config = workflow_config_s7)
selector@config$benchmark$workers <- 8
devtools::load_all()
selector <- benchmark(
  selector,
  include_ts_error = TRUE
)
selector <- calibrate_uncertainty(
  selector
)

selector@benchmark$policy_perf



selector <- select_policies(
  selector
)
predictions <- predict(selector)

benchmark_obj <- selector@benchmark
policy_perf <- benchmark_obj$policy_perf
species_block_perf <- benchmark_obj$species_block_perf
policy_ts_error_long <- benchmark_obj$policy_ts_error %||% tibble::tibble()

conformal_obj <- selector@uncertainty
conf_cal <- conformal_obj$conf_cal
species_conformal_overall <- if (is.list(conformal_obj$species_sum) &&
                                 "overall" %in% names(conformal_obj$species_sum)) {
  tibble::as_tibble(conformal_obj$species_sum$overall)
} else {
  tibble::tibble(policy = character(), empirical_coverage = numeric(), median_abs_log_error = numeric())
}
if (!all(c("policy", "empirical_coverage", "median_abs_log_error") %in% names(species_conformal_overall))) {
  species_conformal_overall <- tibble::tibble(policy = character(), empirical_coverage = numeric(), median_abs_log_error = numeric())
}
policy_uncertainty_ref <- conf_cal |>
  dplyr::mutate(
    conformal_coverage_target = 1 - workflow_config$policy$conformal_alpha,
    uncertainty_cost_log_width = dplyr::if_else(is.finite(q_abs_log), 2 * q_abs_log, NA_real_)
  ) |>
  dplyr::left_join(
    species_conformal_overall |>
      dplyr::select(policy, equation_branch_filter, empirical_coverage, median_abs_log_error) |>
      dplyr::rename(
        species_block_conformal_coverage = empirical_coverage,
        species_block_median_abs_log_error = median_abs_log_error
      ),
    by = c("policy", "equation_branch_filter")
  ) |>
  dplyr::mutate(
    uncertainty_eligible = is.finite(uncertainty_cost_log_width) &
      dplyr::coalesce(species_block_conformal_coverage, 0) >= conformal_coverage_target
  )

selection_obj <- selector@selection
policy_selection_final <- tibble::as_tibble(selection_obj$final_ref %||% tibble::tibble()) |>
  dplyr::left_join(
    policy_uncertainty_ref |>
      dplyr::select(dplyr::any_of(c(
        "policy",
        "equation_branch_filter",
        "uncertainty_cost_log_width",
        "conformal_coverage_target",
        "species_block_conformal_coverage",
        "species_block_median_abs_log_error",
        "uncertainty_eligible",
        "calibration_source"
      ))),
    by = c("policy", "equation_branch_filter")
  )

selected_policies <- predictions@selections
policy_intervals <- predictions@intervals
consensus_multipliers <- predictions@consensus
utils::write.csv(selected_policies, file.path(output_dir, "anchor_selected_policies.csv"), row.names = FALSE)
utils::write.csv(policy_intervals, anchor_interval_path, row.names = FALSE)
saveRDS(
  list(
    selected_policies = selected_policies,
    policy_intervals = policy_intervals,
    consensus_multipliers = consensus_multipliers
  ),
  file.path(cache_dir, "meta_policy_anchor_application.rds")
)

save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_policy_benchmark_boxplot.png"),
  plot(selector, type = "policy_benchmark"),
  width = 8,
  height = 5
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_policy_branch_heatmap.png"),
  plot(selector, type = "strategy_error_heatmap"),
  width = 10,
  height = 7
)
save_plot_if_possible(
  file.path(meta_output_dir, "species_policy_performance_ranked.png"),
  plot(selector, type = "species_policy_ranked"),
  width = 18,
  height = 30,
  limitsize = FALSE
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_conformal_scores.png"),
  plot(selector, type = "conformal_scores"),
  width = 10,
  height = 7
)

simulator <- NULL
if (command_line_true(Sys.getenv("TSBIOMASS_META_RUN_SENSITIVITY", "false"))) {
  tsb_message("Meta-policy run: running optional sensitivity scenarios.")
  simulator <- as_policy_simulator(selector, config = workflow_config_s7)
  simulator <- simulate(simulator)
  if (!is.null(simulator) && nrow(simulator@manifest) > 0) {
    save_plot_if_possible(
      file.path(meta_output_dir, "meta_policy_policy_stability_heatmap.png"),
      plot(simulator, type = "policy_stability"),
      width = 9,
      height = 6
    )
    save_plot_if_possible(
      file.path(meta_output_dir, "meta_policy_multiplier_change_heatmap.png"),
      plot(simulator, type = "multiplier_drift"),
      width = 9,
      height = 6
    )
  }
}

if (is.null(benchmark_obj$species_block_perf)) {
  stop("Policy benchmark cache does not contain 'species_block_perf'.", call. = FALSE)
}

species_block_perf <- benchmark_obj$species_block_perf
policy_ts_error_long <- benchmark_obj$policy_ts_error %||% tibble::tibble()
ts_conf_cal <- if (isTRUE(meta_ts_panels_enabled) && nrow(policy_ts_error_long) > 0) {
  summarize_ts_calibration(policy_ts_error_long)
} else {
  tibble::tibble()
}

workers <- as.integer(Sys.getenv(
  "TSBIOMASS_META_WORKERS",
  as.character(meta_cfg$workers %||% 1L)
))

tsb_message(
  "Cross-fitting meta-policy learner with ", workers, " worker(s)..."
)

learner <- as_policy_learner(selector, config = workflow_config_s7)
learner <- crossfit(learner)
learner <- fit(learner)
learner <- calibrate_uncertainty(learner)
conjurer <- tryCatch(
  {
    tsb_message("Meta-policy run: simulating Conjurer missingness diagnostics.")
    simulate(
      as_conjurer(selector, learner = learner, config = workflow_config_s7),
      n_draws = meta_conjurer_draws,
      progress = FALSE
    )
  },
  error = function(e) {
    tsb_message("Skipping Conjurer heatmaps: ", conditionMessage(e))
    NULL
  }
)
if (!is.null(conjurer) && nrow(conjurer@summary) > 0) {
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_conjurer_mean_abs_db_shift_heatmap.png"),
    plot(conjurer, metric = "mean_abs_db_shift"),
    width = 10,
    height = 5.5
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_conjurer_q95_abs_db_shift_heatmap.png"),
    plot(conjurer, metric = "q95_abs_db_shift"),
    width = 10,
    height = 5.5
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_conjurer_switch_rate_heatmap.png"),
    plot(conjurer, metric = "switch_rate_vs_baseline"),
    width = 10,
    height = 5.5
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_conjurer_sd_log_multiplier_heatmap.png"),
    plot(conjurer, metric = "sd_log_multiplier"),
    width = 10,
    height = 5.5
  )
}

final_learner <- learner@fitted_model$model
if (identical(final_learner$method %||% NULL, "super_learner")) {
  dir.create(meta_output_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    tibble::tibble(
      method = names(final_learner$weights),
      weight = as.numeric(final_learner$weights)
    ),
    file.path(meta_output_dir, "meta_policy_super_learner_weights.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    final_learner$oof_performance %||% tibble::tibble(),
    file.path(meta_output_dir, "meta_policy_super_learner_oof_performance.csv"),
    row.names = FALSE
  )
}

crossfit_obj <- learner@crossfit$result
meta_predictions <- tibble::as_tibble(
  learner@calibration$predictions %||%
    crossfit_obj$predictions %||%
    tibble::tibble()
)
meta_training <- learner@training_data
outcome_col <- learner@calibration$outcome_col %||%
  learner@crossfit$outcome_col %||%
  meta_cfg$outcome_col %||%
  "error_abs_log"
if (!outcome_col %in% names(meta_predictions) && ".outcome" %in% names(meta_predictions)) {
  # Keep the configured outcome name available for plotting and CSV outputs
  # even when the cross-fit helper only emitted the generic training column.
  meta_predictions[[outcome_col]] <- meta_predictions$.outcome
}
meta_selected <- tibble::as_tibble(learner@calibration$selected %||% tibble::tibble())
meta_support_cutpoints <- learner@calibration$support_cutpoints %||% numeric()
meta_summary <- tibble::as_tibble(learner@calibration$summary %||% tibble::tibble())
meta_q_global <- learner@calibration$q_global %||% NA_real_
meta_q_simultaneous_global <- learner@calibration$q_simultaneous_global %||% NA_real_
meta_bin_quantiles <- tibble::as_tibble(learner@calibration$bin_quantiles %||% tibble::tibble())
meta_calibration_coverage <- tibble::as_tibble(learner@calibration$coverage %||% tibble::tibble())
clip_quantile_now <- learner@crossfit$outcome_clip_quantile %||%
  policy_selector_config_value(workflow_config_s7@data, "outcome_clip_quantile", sections = c("metalearner", "policy_learner"))
clip_cap_now <- learner@crossfit$outcome_clip_cap %||%
  learner@calibration$outcome_clip_cap %||%
  attr(learner@training_data, "outcome_clip_cap")

append_target_summary <- function(label,
                                  predictions_tbl,
                                  selected_tbl,
                                  outcome_name,
                                  clip_quantile = NA_real_,
                                  clip_cap = NA_real_) {
  modeled_col_now <- if (".outcome" %in% names(predictions_tbl)) ".outcome" else outcome_name
  raw_col_now <- if (".outcome_raw" %in% names(predictions_tbl)) ".outcome_raw" else modeled_col_now
  modeled_summary <- summarize_meta_policy_predictions(predictions_tbl, outcome_col = modeled_col_now)
  if (nrow(modeled_summary) == 0) {
    return(tibble::tibble())
  }
  selected_summary <- summarize_meta_policy_predictions(selected_tbl, outcome_col = raw_col_now)
  tibble::as_tibble(modeled_summary) |>
    dplyr::mutate(
      target_variant = label,
      outcome_clip_quantile = suppressWarnings(as.numeric(clip_quantile)[[1]]),
      outcome_clip_cap = suppressWarnings(as.numeric(clip_cap)[[1]]),
      selected_n = nrow(selected_tbl),
      selected_raw_mae = selected_summary$raw_mae %||% NA_real_,
      selected_raw_rmse = selected_summary$raw_rmse %||% NA_real_,
      selected_raw_bias = selected_summary$raw_bias %||% NA_real_,
      selected_raw_spearman = selected_summary$raw_spearman %||% NA_real_,
      selected_raw_pearson = selected_summary$raw_pearson %||% NA_real_
    ) |>
    dplyr::relocate(target_variant)
}

target_sensitivity_summary <- append_target_summary(
  label = "operational_clipped_target",
  predictions_tbl = meta_predictions,
  selected_tbl = meta_selected,
  outcome_name = outcome_col,
  clip_quantile = clip_quantile_now,
  clip_cap = clip_cap_now
)
target_sensitivity_recommendations <- tibble::tibble()
meta_prediction_bundle <- predict(
  selector,
  learner = learner
)
referee <- as_referee(
  selector,
  learner = learner,
  predictions = meta_prediction_bundle,
  config = workflow_config_s7
)
scorecard <- predict(referee)
referee <- referee_rebuild(referee, scorecard = scorecard)
meta_anchor_scored <- scorecard@intervals
meta_anchor_ranked <- scorecard@intervals
meta_anchor_selected <- scorecard@selected
meta_anchor_summary <- scorecard@anchor_summary
meta_anchor_audit <- scorecard@anchor_audit
selection_diagnostics <- scorecard@selection_diagnostics
recommendation_cards <- tibble::as_tibble(scorecard@recommendation_cards)
surrogate_rules <- tibble::as_tibble(scorecard@surrogate_rules)

if (is.finite(suppressWarnings(as.numeric(clip_quantile_now)[[1]])) &&
    suppressWarnings(as.numeric(clip_quantile_now)[[1]]) < 1) {
  raw_cfg <- workflow_config_s7@data
  raw_cfg$metalearner$outcome_clip_quantile <- 1
  raw_cfg$metalearner$progress <- FALSE
  raw_cfg_s7 <- as_configurer(raw_cfg)
  raw_learner <- as_policy_learner(selector, config = raw_cfg_s7)
  raw_learner <- crossfit(raw_learner)
  raw_learner <- fit(raw_learner)
  raw_learner <- calibrate_uncertainty(raw_learner)

  raw_predictions <- tibble::as_tibble(
    raw_learner@calibration$predictions %||%
      raw_learner@crossfit$result$predictions %||%
      tibble::tibble()
  )
  if (!outcome_col %in% names(raw_predictions) && ".outcome" %in% names(raw_predictions)) {
    raw_predictions[[outcome_col]] <- raw_predictions$.outcome
  }
  raw_selected <- tibble::as_tibble(raw_learner@calibration$selected %||% tibble::tibble())
  target_sensitivity_summary <- dplyr::bind_rows(
    target_sensitivity_summary,
    append_target_summary(
      label = "raw_unclipped_target",
      predictions_tbl = raw_predictions,
      selected_tbl = raw_selected,
      outcome_name = outcome_col,
      clip_quantile = raw_learner@crossfit$outcome_clip_quantile %||% 1,
      clip_cap = raw_learner@crossfit$outcome_clip_cap %||% NA_real_
    )
  )

  raw_prediction_bundle <- predict(selector, learner = raw_learner)
  raw_referee <- as_referee(
    selector,
    learner = raw_learner,
    predictions = raw_prediction_bundle,
    config = raw_cfg_s7
  )
  raw_scorecard <- predict(raw_referee)
  raw_anchor_selected <- tibble::as_tibble(raw_scorecard@selected)
  target_sensitivity_recommendations <- meta_anchor_selected |>
    dplyr::transmute(
      anchor_model_id = as.character(anchor_model_id),
      anchor_species = as.character(anchor_species),
      clipped_policy = as.character(dplyr::coalesce(selected_policy_display, policy_display, selected_policy, policy)),
      clipped_branch = as.character(dplyr::coalesce(selected_equation_branch_filter, equation_branch_filter)),
      clipped_multiplier = as.numeric(multiplier_pred)
    ) |>
    dplyr::left_join(
      raw_anchor_selected |>
        dplyr::transmute(
          anchor_model_id = as.character(anchor_model_id),
          raw_policy = as.character(dplyr::coalesce(selected_policy_display, policy_display, selected_policy, policy)),
          raw_branch = as.character(dplyr::coalesce(selected_equation_branch_filter, equation_branch_filter)),
          raw_multiplier = as.numeric(multiplier_pred)
        ),
      by = "anchor_model_id"
    ) |>
    dplyr::mutate(
      policy_changed = clipped_policy != raw_policy | clipped_branch != raw_branch,
      log_multiplier_delta = dplyr::if_else(
        is.finite(clipped_multiplier) & clipped_multiplier > 0 &
          is.finite(raw_multiplier) & raw_multiplier > 0,
        log(raw_multiplier) - log(clipped_multiplier),
        NA_real_
      )
  )
}

save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_selected_coefficients.png"),
  plot(scorecard, type = "coefficient_uncertainty"),
  width = 8.5,
  height = 6.5
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_selected_intervals.png"),
  plot(scorecard, type = "selected_intervals", reference_label = "Super Learner Meta-Policy"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_field_missingness.png"),
  plot(scorecard, type = "field_missingness"),
  width = 8,
  height = 5
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_anchor_multiplier_summary.png"),
  plot(referee, type = "anchor_multiplier_summary"),
  width = 9,
  height = 6
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_all_strategy_intervals_panel.png"),
  plot(scorecard, type = "strategy_competition"),
  width = 15,
  height = 7.5
)

if (nrow(meta_anchor_selected) > 0) {
  save_plot_if_possible(
    file.path(meta_output_dir, "swfsc_ts_conformal_panels.png"),
    plot(scorecard, type = "ts_length_conformal"),
    width = 11.5,
    height = 9.2
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_multiplier_length_spectrum.png"),
    plot(scorecard, type = "ts_length_multiplier"),
    width = 11,
    height = 8.5
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_anchor_biomass_multipliers.png"),
    plot(scorecard, type = "selected_multiplier_summary"),
    width = 9,
    height = 6
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_anchor_length_density_panel.png"),
    plot(referee, type = "length_density"),
    width = 10,
    height = 7
  )

  most_similar_dir <- file.path(meta_output_dir, "meta_policy_most_similar_by_anchor")
  similarity_map_dir <- file.path(meta_output_dir, "meta_policy_similarity_maps_by_anchor")
  weight_dir_generic <- file.path(meta_output_dir, "meta_policy_model_weights_by_anchor")
  biomass_map_dir_generic <- file.path(meta_output_dir, "meta_policy_candidate_biomass_maps_by_anchor")
  component_dir_generic <- file.path(meta_output_dir, "meta_policy_component_importance_by_anchor")
  strategy_dir_generic <- file.path(meta_output_dir, "meta_policy_strategy_intervals_by_anchor")
  ts_band_dir <- file.path(meta_output_dir, "meta_policy_ts_bands_by_anchor")
  dir.create(most_similar_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(similarity_map_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(weight_dir_generic, recursive = TRUE, showWarnings = FALSE)
  dir.create(biomass_map_dir_generic, recursive = TRUE, showWarnings = FALSE)
  dir.create(component_dir_generic, recursive = TRUE, showWarnings = FALSE)
  dir.create(strategy_dir_generic, recursive = TRUE, showWarnings = FALSE)
  dir.create(ts_band_dir, recursive = TRUE, showWarnings = FALSE)

  for (anchor_id in unique(as.character(meta_anchor_selected$anchor_model_id))) {
    anchor_row <- meta_anchor_selected |>
      dplyr::filter(as.character(anchor_model_id) == anchor_id) |>
      dplyr::slice(1)
    if (nrow(anchor_row) == 0) {
      next
    }
    anchor_label <- as.character(anchor_row$anchor_species[[1]])
    anchor_file_label <- gsub("[^A-Za-z0-9]+", "_", paste(anchor_label, anchor_id, sep = "_"))

    save_plot_if_possible(
      file.path(most_similar_dir, paste0(anchor_file_label, "_most_similar.png")),
      plot(candidates, type = "candidate_review", view = "most_similar", anchor_model_id = anchor_id),
      width = 8,
      height = 5
    )
    save_plot_if_possible(
      file.path(similarity_map_dir, paste0(anchor_file_label, "_similarity_map.png")),
      plot(candidates, type = "candidate_review", view = "similarity_map", anchor_model_id = anchor_id),
      width = 8,
      height = 5
    )
    save_plot_if_possible(
      file.path(weight_dir_generic, paste0(anchor_file_label, "_model_weights.png")),
      plot(candidates, type = "model_weights", anchor_model_id = anchor_id),
      width = 8,
      height = 5
    )
    save_plot_if_possible(
      file.path(biomass_map_dir_generic, paste0(anchor_file_label, "_candidate_biomass_response.png")),
      plot(candidates, type = "candidate_biomass_response", anchor_model_id = anchor_id),
      width = 8,
      height = 5
    )
    save_plot_if_possible(
      file.path(component_dir_generic, paste0(anchor_file_label, "_component_importance.png")),
      plot(candidates, type = "component_importance", view = "anchor", anchor_model_id = anchor_id),
      width = 8.5,
      height = 5.5
    )
    save_plot_if_possible(
      file.path(strategy_dir_generic, paste0(anchor_file_label, "_strategy_intervals.png")),
      plot(referee, type = "strategy_competition", anchor_model_id = anchor_id, reference_name = anchor_label),
      width = 9,
      height = 6
    )
    save_plot_if_possible(
      file.path(ts_band_dir, paste0(anchor_file_label, "_ts_bands.png")),
      plot(scorecard, type = "ts_length_bands", anchor_model_id = anchor_id),
      width = 9,
      height = 6
    )
  }
}

if (isTRUE(meta_ts_panels_enabled)) {
  meta_ts_panel_tbl <- tibble::as_tibble(scorecard@ts_panel)
  if (nrow(meta_ts_panel_tbl) > 0) {
    utils::write.csv(
      meta_ts_panel_tbl,
      file.path(meta_output_dir, "meta_policy_ts_conformal_panel_data.csv"),
      row.names = FALSE
    )
  } else {
    tsb_message("Skipping TS conformal panels: scorecard TS panel is unavailable.")
  }
} else {
  meta_ts_panel_tbl <- tibble::tibble()
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(meta_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

meta_summary <- meta_summary |>
  dplyr::mutate(
    outcome_clip_quantile = suppressWarnings(as.numeric(clip_quantile_now)[[1]]),
    outcome_clip_cap = suppressWarnings(as.numeric(clip_cap_now)[[1]]),
    uncertainty_prediction_source = learner@calibration$uncertainty_prediction_source %||% NA_character_,
    width_prediction_source = learner@calibration$width_prediction_source %||% NA_character_,
    uncertainty_warning = learner@calibration$uncertainty_warning %||% NA_character_
  )

utils::write.csv(
  meta_predictions,
  file.path(meta_output_dir, "meta_policy_crossfit_predictions.csv"),
  row.names = FALSE
)
utils::write.csv(
  meta_selected,
  file.path(meta_output_dir, "meta_policy_crossfit_selected.csv"),
  row.names = FALSE
)
utils::write.csv(
  meta_summary,
  file.path(meta_output_dir, "meta_policy_crossfit_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  meta_bin_quantiles,
  file.path(meta_output_dir, "meta_policy_conformal_bin_quantiles.csv"),
  row.names = FALSE
)
utils::write.csv(
  meta_calibration_coverage,
  file.path(meta_output_dir, "meta_policy_conformal_calibration_coverage.csv"),
  row.names = FALSE
)
utils::write.csv(
  meta_anchor_selected,
  file.path(meta_output_dir, "meta_policy_anchor_selected_policies.csv"),
  row.names = FALSE
)
utils::write.csv(
  meta_anchor_ranked,
  file.path(meta_output_dir, "meta_policy_anchor_all_policy_intervals.csv"),
  row.names = FALSE
)
utils::write.csv(
  recommendation_cards,
  file.path(meta_output_dir, "meta_policy_recommendation_cards.csv"),
  row.names = FALSE
)
utils::write.csv(
  surrogate_rules,
  file.path(meta_output_dir, "meta_policy_surrogate_rules.csv"),
  row.names = FALSE
)
utils::write.csv(
  target_sensitivity_summary,
  file.path(meta_output_dir, "meta_policy_target_sensitivity_summary.csv"),
  row.names = FALSE
)
utils::write.csv(
  target_sensitivity_recommendations,
  file.path(meta_output_dir, "meta_policy_target_sensitivity_recommendations.csv"),
  row.names = FALSE
)

if (length(similarity_tuning_obj) > 0) {
  utils::write.csv(
    tibble::as_tibble(similarity_tuning_obj$tuning_history %||% tibble::tibble()),
    file.path(meta_output_dir, "similarity_tuning_history.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    tibble::as_tibble(similarity_tuning_obj$stability_summary %||% tibble::tibble()),
    file.path(meta_output_dir, "similarity_tuning_stability.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    tibble::as_tibble(similarity_tuning_obj$boundary_summary %||% tibble::tibble()),
    file.path(meta_output_dir, "similarity_tuning_boundary_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    tibble::as_tibble(similarity_tuning_obj$resample_summary %||% tibble::tibble()),
    file.path(meta_output_dir, "similarity_tuning_resample_summary.csv"),
    row.names = FALSE
  )
}

uncertainty_support_bin <- if ("post_selection_support_label" %in% names(meta_anchor_selected)) {
  meta_anchor_selected$post_selection_support_label
} else if ("post_selection_support_bin" %in% names(meta_anchor_selected)) {
  meta_anchor_selected$post_selection_support_bin
} else {
  rep(NA_character_, nrow(meta_anchor_selected))
}
uncertainty_warning_now <- if ("meta_uncertainty_warning" %in% names(meta_anchor_selected)) {
  as.character(meta_anchor_selected$meta_uncertainty_warning)
} else {
  rep(NA_character_, nrow(meta_anchor_selected))
}
uncertainty_provenance_tbl <- meta_anchor_selected |>
  dplyr::transmute(
    anchor_model_id = as.character(anchor_model_id),
    anchor_species = as.character(anchor_species),
    selected_policy = as.character(dplyr::coalesce(selected_policy_display, policy_display, selected_policy, policy)),
    selected_branch = as.character(dplyr::coalesce(selected_equation_branch_filter, equation_branch_filter)),
    support_bin = as.character(uncertainty_support_bin),
    support_bin_code = as.character(post_selection_support_bin),
    uncertainty_source = as.character(meta_uncertainty_source),
    uncertainty_fallback = as.logical(meta_uncertainty_fallback),
    uncertainty_warning = uncertainty_warning_now,
    uncertainty_budget_log = as.numeric(meta_q_abs_log_total),
    interval_log_width = as.numeric(meta_post_selection_interval_log_width),
    uncertainty_conformal_factor = as.numeric(meta_q_abs_log_conformal_factor),
    uncertainty_bin_q_log = as.numeric(meta_q_abs_log)
  )
utils::write.csv(
  uncertainty_provenance_tbl,
  file.path(meta_output_dir, "meta_policy_uncertainty_provenance.csv"),
  row.names = FALSE
)

species_policy_performance <- summarize_species_policy_performance(species_block_perf)
utils::write.csv(
  species_policy_performance,
  file.path(meta_output_dir, "species_policy_performance_ranked.csv"),
  row.names = FALSE
)
utils::write.csv(
  selection_diagnostics,
  file.path(meta_output_dir, "selected_policy_species_rank_diagnostics.csv"),
  row.names = FALSE
)
recommendation_stability_source <- meta_selected
recommendation_stability_source$policy_display <- resolve_selected_policy_names(recommendation_stability_source)
recommendation_stability_source$branch_display <- if ("selected_equation_branch_filter" %in% names(recommendation_stability_source)) {
  as.character(recommendation_stability_source$selected_equation_branch_filter)
} else {
  as.character(recommendation_stability_source$equation_branch_filter)
}
recommendation_stability_tbl <- recommendation_stability_source |>
  dplyr::count(anchor_species, policy_display, branch_display, name = "n_selected") |>
  dplyr::group_by(anchor_species) |>
  dplyr::mutate(
    total_selected = sum(n_selected),
    win_fraction = n_selected / total_selected,
    rank = dplyr::min_rank(dplyr::desc(win_fraction))
  ) |>
  dplyr::ungroup()
recommendation_stability_summary <- recommendation_stability_tbl |>
  dplyr::group_by(anchor_species) |>
  dplyr::arrange(dplyr::desc(win_fraction), policy_display, .by_group = TRUE) |>
  dplyr::summarise(
    n_anchor_models = sum(n_selected),
    n_policies_seen = dplyr::n(),
    top_policy = dplyr::first(policy_display),
    top_branch = dplyr::first(branch_display),
    top_win_fraction = dplyr::first(win_fraction),
    second_policy = dplyr::nth(policy_display, 2, default = NA_character_),
    second_branch = dplyr::nth(branch_display, 2, default = NA_character_),
    second_win_fraction = dplyr::nth(win_fraction, 2, default = NA_real_),
    stability_margin = top_win_fraction - dplyr::coalesce(second_win_fraction, 0),
    .groups = "drop"
  )
if (nrow(recommendation_stability_summary) > 0 && nrow(recommendation_cards) > 0) {
  recommendation_cards <- recommendation_cards |>
    dplyr::left_join(
      recommendation_stability_summary |>
        dplyr::select(
          anchor_species,
          top_win_fraction,
          second_win_fraction,
          stability_margin
        ) |>
        dplyr::rename(
          recommendation_win_fraction = top_win_fraction,
          recommendation_runner_up_fraction = second_win_fraction,
          recommendation_stability_margin = stability_margin
        ),
      by = "anchor_species"
    )
  utils::write.csv(
    recommendation_cards,
    file.path(meta_output_dir, "meta_policy_recommendation_cards.csv"),
    row.names = FALSE
  )
}
utils::write.csv(
  recommendation_stability_tbl,
  file.path(meta_output_dir, "meta_policy_recommendation_stability_by_policy.csv"),
  row.names = FALSE
)
utils::write.csv(
  recommendation_stability_summary,
  file.path(meta_output_dir, "meta_policy_recommendation_stability_summary.csv"),
  row.names = FALSE
)
save_plot_if_possible(
  file.path(meta_output_dir, "meta_policy_recommendation_stability.png"),
  plot(learner, type = "recommendation_stability"),
  width = 9,
  height = 6
)

plot_tbl <- meta_predictions |>
  dplyr::filter(is.finite(.meta_predicted_score), is.finite(.data[[if (".outcome" %in% names(meta_predictions)) ".outcome" else outcome_col]]))
selected_plot_tbl <- meta_selected |>
  dplyr::filter(is.finite(.meta_predicted_score), is.finite(.data[[if (".outcome" %in% names(meta_selected)) ".outcome" else outcome_col]]))
modeled_outcome_col <- if (".outcome" %in% names(meta_predictions)) ".outcome" else outcome_col
raw_outcome_col <- if (".outcome_raw" %in% names(meta_predictions)) ".outcome_raw" else if (outcome_col %in% names(meta_predictions)) outcome_col else modeled_outcome_col
selected_raw_outcome_col <- if (".outcome_raw" %in% names(meta_selected)) ".outcome_raw" else if (outcome_col %in% names(meta_selected)) outcome_col else if (".outcome" %in% names(meta_selected)) ".outcome" else outcome_col

if (nrow(plot_tbl) > 0) {
  plot_metrics <- summarize_meta_policy_predictions(meta_predictions, outcome_col = modeled_outcome_col)
  if (raw_outcome_col %in% names(meta_predictions)) {
    raw_metrics <- summarize_meta_policy_predictions(meta_predictions, outcome_col = raw_outcome_col)
    plot_metrics <- dplyr::bind_cols(
      plot_metrics,
      raw_metrics |>
        dplyr::select(raw_mae, raw_rmse, raw_bias, raw_spearman, raw_pearson)
    )
  }
  calibration_tbl <- meta_predictions |>
    dplyr::filter(
      is.finite(.meta_predicted_score),
      is.finite(.data[[modeled_outcome_col]])
    )
  if (nrow(calibration_tbl) > 0) {
    n_bins_now <- min(10L, nrow(calibration_tbl))
    calibration_tbl <- calibration_tbl |>
      dplyr::mutate(calibration_bin = dplyr::ntile(.meta_predicted_score, n_bins_now)) |>
      dplyr::group_by(calibration_bin) |>
      dplyr::summarise(
        n = dplyr::n(),
        predicted_mean = mean(.meta_predicted_score, na.rm = TRUE),
        predicted_median = stats::median(.meta_predicted_score, na.rm = TRUE),
        observed_mean = mean(.data[[modeled_outcome_col]], na.rm = TRUE),
        observed_median = stats::median(.data[[modeled_outcome_col]], na.rm = TRUE),
        observed_q25 = stats::quantile(.data[[modeled_outcome_col]], probs = 0.25, na.rm = TRUE, names = FALSE),
        observed_q75 = stats::quantile(.data[[modeled_outcome_col]], probs = 0.75, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  }
  utils::write.csv(plot_metrics, file.path(meta_output_dir, "meta_policy_diagnostics.csv"), row.names = FALSE)
  utils::write.csv(
    calibration_tbl,
    file.path(meta_output_dir, "meta_policy_calibration_curve.csv"),
    row.names = FALSE
  )

  if (raw_outcome_col %in% names(meta_predictions) && raw_outcome_col != modeled_outcome_col) {
    raw_calibration_tbl <- meta_predictions |>
      dplyr::filter(
        is.finite(.meta_predicted_score),
        is.finite(.data[[raw_outcome_col]])
      )
    if (nrow(raw_calibration_tbl) > 0) {
      n_bins_now <- min(10L, nrow(raw_calibration_tbl))
      raw_calibration_tbl <- raw_calibration_tbl |>
        dplyr::mutate(calibration_bin = dplyr::ntile(.meta_predicted_score, n_bins_now)) |>
        dplyr::group_by(calibration_bin) |>
        dplyr::summarise(
          n = dplyr::n(),
          predicted_mean = mean(.meta_predicted_score, na.rm = TRUE),
          predicted_median = stats::median(.meta_predicted_score, na.rm = TRUE),
          observed_mean = mean(.data[[raw_outcome_col]], na.rm = TRUE),
          observed_median = stats::median(.data[[raw_outcome_col]], na.rm = TRUE),
          observed_q25 = stats::quantile(.data[[raw_outcome_col]], probs = 0.25, na.rm = TRUE, names = FALSE),
          observed_q75 = stats::quantile(.data[[raw_outcome_col]], probs = 0.75, na.rm = TRUE, names = FALSE),
          .groups = "drop"
        )
    }
    utils::write.csv(
      raw_calibration_tbl,
      file.path(meta_output_dir, "meta_policy_raw_calibration_curve.csv"),
      row.names = FALSE
    )
  }

  residual_tbl <- meta_predictions |>
    dplyr::mutate(policy_display = resolve_policy_display_names(meta_predictions)) |>
    dplyr::filter(is.finite(.meta_predicted_score), is.finite(.data[[raw_outcome_col]])) |>
    dplyr::mutate(
      residual_raw = .data[[raw_outcome_col]] - .meta_predicted_score,
      abs_residual_raw = abs(residual_raw)
    )
  utils::write.csv(
    residual_tbl |>
      dplyr::arrange(dplyr::desc(abs_residual_raw)) |>
      dplyr::slice_head(n = 100),
    file.path(meta_output_dir, "meta_policy_worst_outliers.csv"),
    row.names = FALSE
  )
  residual_by_policy <- residual_tbl |>
    dplyr::group_by(policy_display) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_abs_residual = stats::median(abs_residual_raw, na.rm = TRUE),
      mean_abs_residual = mean(abs_residual_raw, na.rm = TRUE),
      median_residual = stats::median(residual_raw, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(median_abs_residual))
  utils::write.csv(
    residual_by_policy,
    file.path(meta_output_dir, "meta_policy_residual_summary_by_policy.csv"),
    row.names = FALSE
  )

  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_predicted_vs_observed.png"),
    plot(learner, type = "predicted_vs_observed"),
    width = 7,
    height = 5
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_calibration_curve.png"),
    plot(learner, type = "calibration_curve", outcome = "modeled"),
    width = 7,
    height = 5
  )
  if (raw_outcome_col %in% names(meta_predictions) && raw_outcome_col != modeled_outcome_col) {
    save_plot_if_possible(
      file.path(meta_output_dir, "meta_policy_raw_calibration_curve.png"),
      plot(learner, type = "calibration_curve", outcome = "raw"),
      width = 7,
      height = 5
    )
  }
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_residual_by_branch.png"),
    plot(learner, type = "residuals", view = "by_branch", outcome = "raw"),
    width = 7,
    height = 5
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_score_by_policy.png"),
    plot(learner, type = "score_by_policy"),
    width = 8,
    height = 10
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_support_bin_error.png"),
    plot(learner, type = "support_bin_error", outcome = "raw"),
    width = 7,
    height = 5
  )
}

if (nrow(selected_plot_tbl) > 0) {
  selected_calibration_tbl <- meta_selected |>
    dplyr::filter(
      is.finite(.meta_predicted_score),
      is.finite(.data[[selected_raw_outcome_col]])
    )
  if (nrow(selected_calibration_tbl) > 0) {
    n_bins_now <- min(10L, nrow(selected_calibration_tbl))
    selected_calibration_tbl <- selected_calibration_tbl |>
      dplyr::mutate(calibration_bin = dplyr::ntile(.meta_predicted_score, n_bins_now)) |>
      dplyr::group_by(calibration_bin) |>
      dplyr::summarise(
        n = dplyr::n(),
        predicted_mean = mean(.meta_predicted_score, na.rm = TRUE),
        predicted_median = stats::median(.meta_predicted_score, na.rm = TRUE),
        observed_mean = mean(.data[[selected_raw_outcome_col]], na.rm = TRUE),
        observed_median = stats::median(.data[[selected_raw_outcome_col]], na.rm = TRUE),
        observed_q25 = stats::quantile(.data[[selected_raw_outcome_col]], probs = 0.25, na.rm = TRUE, names = FALSE),
        observed_q75 = stats::quantile(.data[[selected_raw_outcome_col]], probs = 0.75, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      )
  }
  utils::write.csv(
    selected_calibration_tbl,
    file.path(meta_output_dir, "meta_policy_selected_calibration_curve.csv"),
    row.names = FALSE
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_selected_calibration_curve.png"),
    plot(learner, type = "calibration_curve", rows = "selected", outcome = "raw"),
    width = 7,
    height = 5
  )
  save_plot_if_possible(
    file.path(meta_output_dir, "meta_policy_selected_policy_counts.png"),
    plot(learner, type = "selected_policy_counts"),
    width = 7,
    height = 5
  )
}

saveRDS(
  list(
    config = meta_cfg,
    crossfit = crossfit_obj,
    final_learner = final_learner,
    scorecard = scorecard,
    selected = meta_selected,
    anchor_ranked = meta_anchor_ranked,
    anchor_selected = meta_anchor_selected,
    ts_conformal_panel = meta_ts_panel_tbl,
    conformal_bin_quantiles = meta_bin_quantiles,
    conformal_calibration_coverage = meta_calibration_coverage,
    target_sensitivity_summary = target_sensitivity_summary,
    target_sensitivity_recommendations = target_sensitivity_recommendations,
    uncertainty_provenance = uncertainty_provenance_tbl,
    recommendation_stability = recommendation_stability_summary,
    summary = meta_summary,
    output_dir = meta_output_dir
  ),
  file.path(cache_dir, "meta_policy_crossfit.rds")
)
saveRDS(final_learner, file.path(cache_dir, "meta_policy_learner.rds"))

tsb_message(
  "Meta-policy run complete. Selected rows: ", nrow(meta_selected), "."
)
