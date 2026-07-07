suppressPackageStartupMessages(library(tsbiomass))
`%||%` <- function(x, y) if (is.null(x)) y else x

# Directory used for cached intermediate objects.
cache_dir <- file.path(getwd(), "inst", "cache")

# ---- Refresh cache?
REFRESH <- command_line_true(Sys.getenv("TSBIOMASS_REFRESH", "false"))

# Path to the workflow configuration file.
workflow_config_path <- file.path(getwd(), "inst", "templates", "swfscfish_config.yaml")

# Directory where workflow outputs will be written.
output_dir <- file.path(getwd(), "inst", "outputs_swfscfish")

# Step 2: read the TSL table and the species-level source databases.
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
workflow_plots <- list()

################################################################################
# Reading and enriching the model table...

tsb_message(
  "Begin SWFSC TS-Length model analysis."
)

# ------------------------------------------------------------------------------
# READ CONFIG
tsb_message(
  "Reading YAML configuration {", workflow_config_path, "} ..."
)
workflow_config <- read_workflow_config(workflow_config_path)
workflow_config_s7 <- as_configurer(workflow_config)

# ------------------------------------------------------------------------------
# BUILD THE Candidates object
tsb_message(
  "Reading, enriching, and preparing the candidate-model object..."
)
candidates <- as_candidates(workflow_config_s7)
candidates <- set_reference_anchors(candidates)

tsl <- candidates@study_db
species_vector <- candidates@species_vector
species_db <- candidates@species_db
candidate_models <- candidates@candidate_models
reference_anchors <- candidates@reference_anchors
anchor_ids <- if ("model_id_chr" %in% names(reference_anchors)) {
  as.character(reference_anchors$model_id_chr)
} else {
  as.character(reference_anchors$model_id)
}

worms_db <- candidates@source_dbs$worms %||% tibble::tibble()
fishbase_db <- candidates@source_dbs$fishbase %||% tibble::tibble()
pelagic_db <- candidates@source_dbs$pelagic %||% tibble::tibble()
azores_db <- candidates@source_dbs$azores %||% tibble::tibble()
continental_db <- candidates@source_dbs$continental %||% tibble::tibble()
mstraits_db <- candidates@source_dbs$mstraits %||% tibble::tibble()

tsb_message(
  length(species_vector), " species detected in TS-length table...",
  timestamp = FALSE
)

tsb_message(
  "Creating map showing distribution of studies and models per FAO major area...\n",
  "Study counts: ",
  file.path(output_dir, workflow_config$outputs$fao_distribution$studies), ".png.\n",
  "Model counts: ",
  file.path(output_dir, workflow_config$outputs$fao_distribution$models), ".png."
)
study_counts <- plot_area_distribution(tsl, count_type = "studies")
model_counts <- plot_area_distribution(tsl, count_type = "models")
workflow_plots$area_distribution <- study_counts
workflow_plots$model_area_distribution <- model_counts
ggplot2::ggsave(
  filename = file.path(output_dir,
                       paste0(workflow_config$outputs$fao_distribution$studies,
                              ".png")),
  plot = study_counts,
  width = 9,
  height = 6,
  dpi = 300
)
ggplot2::ggsave(
  filename = file.path(output_dir,
                       paste0(workflow_config$outputs$fao_distribution$models,
                              ".png")),
  plot = model_counts,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# SOURCE COVERAGE DIAGNOSTICS
worms_spp <- unique(paste0(worms_db$genus, " ", worms_db$species))
tsb_message(
  length(worms_spp), " species detected in WORMS database...",
  timestamp = FALSE
)

fb_spp <- unique(paste0(fishbase_db$genus, " ", fishbase_db$species))
tsb_message(
  length(fb_spp), " species detected in Fishebase database...",
  timestamp = FALSE
)

ms_spp <- unique(paste0(mstraits_db$genus, " ", mstraits_db$species))

tsb_message(
  sum(rowSums(is.na(mstraits_db)) != ncol(mstraits_db) - 2),
  " species detected in marineSpeciesTraits database...",
  timestamp = FALSE
)

tsb_message(
  sum(rowSums(is.na(pelagic_db)) != ncol(pelagic_db) - 2),
  " species detected in pelagic species traits database...",
  timestamp = FALSE
)

tsb_message(
  sum(rowSums(is.na(continental_db)) != ncol(continental_db) - 2),
  " species detected in N.E. Pacific and N. Atlantic continental shelf trait collection...",
  timestamp = FALSE
)

tsb_message(
  sum(rowSums(is.na(azores_db)) != ncol(azores_db) - 2),
  " species detected in Azores functional traits database...",
  timestamp = FALSE
)

tsb_message(
  nrow(species_db), " species retained in enriched species trait table...",
  timestamp = FALSE
)

tsb_message(
  nrow(candidate_models), " candidate models retained in prepared model table...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# SUMMARIZE TS-LENGTH MODEL SLOPE DEPENDENCE
tsb_message(
  "Summarizing TS-length model slope dependence by review-group tags..."
)
if (!file.exists(file.path(cache_dir, "slope_dependence_summary.rds")) || REFRESH) {
  slope_dependence <- summarize_slope_effect(candidate_models)
  saveRDS(slope_dependence, file.path(cache_dir, "slope_dependence_summary.rds"))
} else {
  slope_dependence <- readRDS(file.path(cache_dir, "slope_dependence_summary.rds"))
}
tsb_message(
  nrow(slope_dependence$study_cell_level), " study cells retained in slope-dependence summary...",
  timestamp = FALSE
)

tsb_message(
  "Plotting slope dependence histogram... writing figure to: ",
  file.path(output_dir, workflow_config$outputs$slope_dependence$distribution),
  ".png"
)
slope_dependence_hist <- plot_slope_distribution(slope_dependence$study_cell_level)
workflow_plots$slope_distribution <- slope_dependence_hist

ggplot2::ggsave(
  filename = file.path(output_dir,
                       paste0(workflow_config$outputs$slope_dependence$distribution,
                              ".png")),
  plot = slope_dependence_hist,
  width = 9,
  height = 6,
  dpi = 300
)

tsb_message(
  "Plotting slope dependence grouped boxplot... writing figure to: ",
  file.path(output_dir, workflow_config$outputs$slope_dependence$boxplot),
  ".png"
)
slope_dependence_boxplot <- plot_slope_group(slope_dependence$study_cell_level)
workflow_plots$slope_group <- slope_dependence_boxplot

ggplot2::ggsave(
  filename = file.path(output_dir,
                       paste0(workflow_config$outputs$slope_dependence$boxplot,
                              ".png")),
  plot = slope_dependence_boxplot,
  width = 9,
  height = 6,
  dpi = 300
)

tsb_message(
  "Plotting slope dependence grouped support... writing figure to: ",
  file.path(output_dir, workflow_config$outputs$slope_dependence$support),
  ".png"
)
slope_dependence_support <- plot_slope_support(slope_dependence$deviation_support_by_group)
workflow_plots$slope_support <- slope_dependence_support

ggplot2::ggsave(
  filename = file.path(output_dir,
                       paste0(workflow_config$outputs$slope_dependence$support,
                              ".png")),
  plot = slope_dependence_support,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# SET REFERENCE ANCHORS
tsb_message(
  "Setting reference survey anchors..."
)
tsb_message(
  length(anchor_ids), " reference survey anchors extracted...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# PREPARE AND TUNE THE SIMILARITY CONFIGURATION
tsb_message(
  "Preparing and tuning similarity configuration..."
)
candidates <- prepare_similarities(
  candidate_models = candidates,
  config = workflow_config_s7
)
candidates <- tune_similarities(
  candidate_models = candidates,
  config = workflow_config_s7
)
similarity_tuning <- candidates@similarity_tuning
similarity_config <- similarity_tuning$config_tuned
utils::write.csv(
  tibble::as_tibble(similarity_tuning$component_weights %||% tibble::tibble()),
  file.path(output_dir, "similarity_tuned_component_weights.csv"),
  row.names = FALSE
)
tsb_message(
  "Similarity configuration tuning succeeded...\n",
  "<alpha> = ", workflow_config$policy$alpha, " --> ",
  similarity_config$alpha, "\n",
  "<kernel_scale> = ", workflow_config$similarity$kernel_scale, " --> ",
  similarity_config$kernel_scale,
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# PREPARED SIMILARITY INPUTS
tsb_message(
  "Prepared similarity inputs and ordination..."
)
similarity_obj <- candidates@similarity_matrix
candidate_models <- candidates@candidate_models

# ------------------------------------------------------------------------------
# CALCULATE GOWER DSITANCES
tsb_message(
  "Computing Gower distances..."
)
candidates <- construct_gower_distances(candidates)
distance_obj <- candidates@gower_distances

tsb_message(
  "Run two-dimensional NMDS on the distance matrix...\n",
  "loadings: False\n",
  "centroids: False"
)
candidates <- run_ordination(
  dist_mat = candidates
)
ordination_obj <- candidates@ordination
ordination_points <- ordination_obj$model$points
model_scores <- ordination_obj$model$model_scores
tsb_message(
  nrow(model_scores), " ordinated model scores retained...",
  timestamp = FALSE
)

tsb_message(
  "Plotting combined ordination... writing figure to: ",
  file.path(output_dir, workflow_config$outputs$ordination$combined),
  ".png"
)
combined_ordination_plot <- plot_ordination_clusters(ordination_points)
workflow_plots$ordination_clusters <- combined_ordination_plot

ggplot2::ggsave(
  filename = file.path(output_dir,
                       paste0(workflow_config$outputs$ordination$combined, ".png")),
  plot = combined_ordination_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# SUMMARIZE SIMILARITY TRAIT MISSINGNESS
tsb_message(
  "Characterizing similarity trait missingness..."
)
points_missing_df <- ordination_obj$model$points_missing
tsb_message(
  sum(points_missing_df$missing_trait_count == 0), " out of ",
  nrow(points_missing_df), " models have complete trait sets.\n",
  "Mean missing trait set proportion (of models with incomplete sets): ",
  round(mean(
    points_missing_df$missing_trait_fraction[
      points_missing_df$missing_trait_count > 0
    ]
  ), 2), ".",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# SPECIES-LEVEL ORDINATION
tsb_message(
  "Running species-level ordination..."
)
species_points <- ordination_obj$species$points
tsb_message(
  "Plotting species ordination... writing figure to: ",
  file.path(output_dir, workflow_config$outputs$ordination$species),
  ".png"
)
species_ordination_plot <- plot_species_ordination(species_points)
workflow_plots$species_ordination <- species_ordination_plot

ggplot2::ggsave(
  filename = file.path(output_dir,
                       paste0(workflow_config$outputs$ordination$species, ".png")),
  plot = species_ordination_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# BUILD ANCHOR-SPECIES LOOK-UP
tsb_message(
  "Build anchor-species lookup table..."
)
species_lookup <- ordination_obj$species_lookup

# ------------------------------------------------------------------------------
# EVALUATE THE APPLICABILITY AND ADMISSIBILITY OF MODELS
tsb_message(
  "Screening candidate admissibility for each reference-anchor model..."
)
candidates <- screen_admissibility(
  candidate_models = candidates,
  config = workflow_config_s7
)
anchor_admissibility_obj <- candidates@admissibility
# ---- Retain the bound cross-anchor tables for downstream use
anchor_scores <- anchor_admissibility_obj$all_scores
anchor_overlap <- anchor_admissibility_obj$all_overlap
anchor_gates <- anchor_admissibility_obj$all_gates
anchor_summary <- anchor_admissibility_obj$anchor_summary
tsb_message(
  nrow(anchor_summary), " anchors evaluated across the candidate-model pool...",
  timestamp = FALSE
)
workflow_plots$overlap_heatmap <- plot_overlap_heatmap(anchor_overlap)
ggplot2::ggsave(
  filename = file.path(output_dir, "overlap_heatmap.png"),
  plot = workflow_plots$overlap_heatmap,
  width = 9,
  height = 6,
  dpi = 300
)
workflow_plots$gate_composition <- plot_gate_composition(anchor_gates)
ggplot2::ggsave(
  filename = file.path(output_dir, "gate_composition.png"),
  plot = workflow_plots$gate_composition,
  width = 9,
  height = 6,
  dpi = 300
)
for (i in seq_len(nrow(anchor_summary))) {
  tsb_message(
    anchor_summary$anchor_species[i], ": ",
    anchor_summary$n_admissible[i], " admissible out of ",
    nrow(candidate_models), " models.",
    timestamp = FALSE
  )
}

# ------------------------------------------------------------------------------
# BENCHMARK ALL ACTIVE POLICIES ACROSS PSEUDO-ANCHOR AND SPECIES-BLOCK TESTS
tsb_message(
  "Benchmarking active policies across pseudo-anchor and species-block tests..."
)
selector <- as_policy_selector(candidates, config = workflow_config_s7)
selector <- benchmark(
  selector
)
benchmark_obj <- selector@benchmark
# Keep the benchmark tables bound in memory for the conformal and policy-choice
# steps that follow.
# `policy_perf` is the pseudo-anchor benchmark table.
# `species_block_perf` is the leave-one-species-out benchmark table.
# `policy_ts_error_long` stores the relative-length TS prediction errors used
# later for the TS conformal ribbons.
policy_perf <- benchmark_obj$policy_perf
anchor_features <- benchmark_obj$anchor_features
best_policy_df <- benchmark_obj$best_policy
policy_ts_error_long <- benchmark_obj$policy_ts_error
species_block_perf <- benchmark_obj$species_block_perf
species_block_features <- benchmark_obj$species_block_features
species_block_best <- benchmark_obj$species_block_best
tsb_message(
  nrow(policy_perf), " pseudo-anchor policy evaluations and ",
  nrow(species_block_perf), " species-block policy evaluations retained...",
  timestamp = FALSE
)
workflow_plots$policy_benchmark <- plot_policy_boxplot(policy_perf)
ggplot2::ggsave(
  filename = file.path(output_dir, "policy_benchmark.png"),
  plot = workflow_plots$policy_benchmark,
  width = 9,
  height = 6,
  dpi = 300
)
workflow_plots$species_block_benchmark <- plot_species_boxplot(species_block_perf)
ggplot2::ggsave(
  filename = file.path(output_dir, "species_block_benchmark.png"),
  plot = workflow_plots$species_block_benchmark,
  width = 9,
  height = 6,
  dpi = 300
)
workflow_plots$policy_heatmap <- plot_policy_heatmap(species_block_perf)
ggplot2::ggsave(
  filename = file.path(output_dir, "policy_heatmap.png"),
  plot = workflow_plots$policy_heatmap,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# CALIBRATE POLICY-WISE CONFORMAL INTERVALS
tsb_message(
  "Calibrating policy-wise conformal uncertainty..."
)
selector <- calibrate_uncertainty(
  selector
)
conformal_obj <- selector@uncertainty
conf_cal <- conformal_obj$conf_cal
conformal_perf_pseudo <- conformal_obj$pseudo_sum
conformal_perf_species_block <- conformal_obj$species_sum
ts_conf_cal <- conformal_obj$ts_cal
# `conf_cal` is the table later joined onto each anchor-policy prediction so
# multiplier intervals can be constructed from the calibrated absolute log error.
tsb_message(
  nrow(conf_cal), " policy calibration rows retained...",
  timestamp = FALSE
)
workflow_plots$conformal_scores <- plot_conformal_scores(conf_cal)
ggplot2::ggsave(
  filename = file.path(output_dir, "conformal_scores.png"),
  plot = workflow_plots$conformal_scores,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# BUILD THE POLICY-SELECTION REFERENCE FROM THE SPECIES-BLOCK BENCHMARK
tsb_message(
  "Building policy-selection reference from species-block benchmark..."
)
selector <- select_policies(
  selector,
  config = workflow_config_s7
)
selection_obj <- selector@selection
policy_selection_ref <- selection_obj$select_ref
policy_equivalence_ref <- selection_obj$equiv_ref
policy_equivalence_sets <- selection_obj$equiv_sets
policy_selection_final <- selection_obj$final_ref
# `policy_selection_final` is the main lookup used in the real-anchor loop
# below. It carries the acceptability flags, tie/equivalence information, and
# benchmark ranking summaries for each active policy.
tsb_message(
  nrow(policy_selection_final), " policies retained in the final selection table...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# APPLY THE ACTIVE POLICIES TO THE REFERENCE ANCHORS
tsb_message(
  "Applying active policies to the reference anchors..."
)
predictions <- predict(selector)
policy_intervals <- predictions@intervals
selected_policies <- predictions@selections
consensus_multipliers <- predictions@consensus
utils::write.csv(
  selected_policies,
  file.path(output_dir, "anchor_selected_policies.csv"),
  row.names = FALSE
)
utils::write.csv(
  policy_intervals,
  file.path(output_dir, "anchor_policy_intervals.csv"),
  row.names = FALSE
)
workflow_plots$selected_policy_intervals <- plot_selected_intervals(selected_policies)
ggplot2::ggsave(
  filename = file.path(output_dir, "selected_policy_intervals.png"),
  plot = workflow_plots$selected_policy_intervals,
  width = 9,
  height = 6,
  dpi = 300
)
# Join the selected-policy output to the donor-pool consensus summary so the
# anchor-facing multiplier figure can compare the chosen transfer against the
# broader admissible-pool baseline.
anchor_multiplier_summary <- selected_policies |>
  dplyr::select(anchor_model_id, anchor_species, multiplier_pred, .multiplier_lo, multiplier_hi) |>
  dplyr::left_join(
    consensus_multipliers |>
      dplyr::transmute(
        anchor_model_id,
        combined_consensus_multiplier = consensus_multiplier,
        combined_multiplier_q05 = multiplier_q05,
        combined_multiplier_q50 = multiplier_q50,
        combined_multiplier_q95 = multiplier_q95
      ),
    by = "anchor_model_id"
  )
workflow_plots$anchor_multiplier_summary <- plot_anchor_summary(
  integrated_tbl = anchor_multiplier_summary,
  score_tbl = anchor_scores,
  interval_tbl = policy_intervals
)
ggplot2::ggsave(
  filename = file.path(output_dir, "anchor_multiplier_summary.png"),
  plot = workflow_plots$anchor_multiplier_summary,
  width = 9,
  height = 6,
  dpi = 300
)
tsb_message(
  nrow(selected_policies), " reference-anchor policy rows retained...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# RERUN THE POLICY STACK ACROSS STANDARD SENSITIVITY SCENARIOS
tsb_message(
  "Rerunning policy benchmark and selection across sensitivity scenarios..."
)
simulator <- as_policy_simulator(selector, config = workflow_config_s7)
simulator <- simulate(
  simulator
)
policy_sensitivity_specs <- simulator@scenarios
policy_sensitivity_map <- simulator@results
policy_sensitivity_manifest <- simulator@manifest
policy_sensitivity_tables <- simulator@tables
scenario_policy_selection_ref <- policy_sensitivity_tables$select_ref
scenario_policy_equivalence_pairs <- policy_sensitivity_tables$equiv_pairs
scenario_policy_equivalence_sets <- policy_sensitivity_tables$equiv_sets
scenario_policy_conformal <- policy_sensitivity_tables$conf_cal
tsb_message(
  nrow(policy_sensitivity_manifest), " sensitivity scenarios retained...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# BUILD WORKFLOW-WIDE SUMMARY TABLES
tsb_message(
  "Building workflow-wide summary tables..."
)
# Recompute the model-level key-field missingness summary using the same trait
# columns that drove the tuned similarity and admissibility layers above.
candidate_models_missing <- screen_missing_metadata(
  candidate_models = candidate_models,
  key_cols = unique(c(
    names(similarity_config$species_weights),
    names(similarity_config$study_weights)
  ))
)

# Summarize the species-block conformal coverage by policy so the anchor-level
# audit can compare each selected policy against its benchmark coverage.
species_block_coverage <- construct_species_coverage(
  coverage_summary = conformal_perf_pseudo,
  species_sum = conformal_perf_species_block
)

# Collapse the selected anchor-policy rows and the benchmark/global policy
# summaries into one anchor-facing audit table.
anchor_policy_audit <- construct_anchor_audit(
  policy_intervals = selected_policies,
  select_ref = policy_selection_final,
  cover_tbl = species_block_coverage
)

# Summarize missingness at three levels:
# 1) overall across the candidate-model table
# 2) by key field
# 3) by model
key_missing_summary <- summarize_key_missing(
  candidate_models = candidate_models_missing,
  key_cols = unique(c(
    names(similarity_config$species_weights),
    names(similarity_config$study_weights)
  )),
  threshold = workflow_config$policy$missing_key_metadata_max_fraction
)
key_missing_overall <- key_missing_summary$overall
key_missing_by_field <- key_missing_summary$by_field
key_missing_by_model <- key_missing_summary$by_model

# Reduce the candidate-level admissibility output to one missing-metadata gate
# row per anchor. The candidate rows are required here because they retain the
# actual gate pass/fail flags.
anchor_missing_gate <- summarize_missing_gate(
  adm_tbl = anchor_scores
)

# Count distinct studies by FAO area and build the inset-tile table used by the
# FAO-distribution figure later on.
area_study_summary <- summarize_area_studies(
  candidate_models = candidate_models
)
area_inset_tiles <- build_area_inset_tiles(
  count_tbl = area_study_summary
)
tsb_message(
  nrow(anchor_policy_audit), " anchor policy-audit rows and ",
  nrow(area_study_summary), " FAO area summary rows retained...",
  timestamp = FALSE
)
workflow_plots$anchor_policy_audit <- plot_anchor_audit(anchor_policy_audit)
ggplot2::ggsave(
  filename = file.path(output_dir, "anchor_policy_audit.png"),
  plot = workflow_plots$anchor_policy_audit,
  width = 9,
  height = 6,
  dpi = 300
)
workflow_plots$field_missingness <- plot_field_missing(key_missing_by_field)
ggplot2::ggsave(
  filename = file.path(output_dir, "field_missingness.png"),
  plot = workflow_plots$field_missingness,
  width = 9,
  height = 6,
  dpi = 300
)
workflow_plots$anchor_missingness <- plot_anchor_missing(anchor_missing_gate)
ggplot2::ggsave(
  filename = file.path(output_dir, "anchor_missingness.png"),
  plot = workflow_plots$anchor_missingness,
  width = 9,
  height = 6,
  dpi = 300
)
saveRDS(
  workflow_plots,
  file.path(output_dir, "workflow_plots.rds")
)
tsb_message(
  length(workflow_plots), " workflow figures written to {", output_dir, "} ...",
  timestamp = FALSE
)

