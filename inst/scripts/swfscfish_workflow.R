devtools::load_all()

# Directory containing the workflow input files.
input_dir <- "C:/Users/Brandyn/Downloads/"

TSL_file <- "fishery_survey_tsl (2).xlsx"
mstraits_file <- "marineSpeciesTraits.RData"
continental_file <- "TraitCollectionFishNAtlanticNEPacificContShelf.xlsx"
azores_file <- "Azores_FishTraits_2023.tab"

# Directory used for cached intermediate objects.
cache_dir <- file.path(getwd(), "inst", "cache")
worms_cache <- "worms_species_traits.rds"
fishbase_cache <- "fishbase_species_traits.rds"
pelagic_cache <- "pelagic_species_traits.rds"
azores_cache <- "azores_species_traits.rds"
continental_cache <- "continental_species_traits.rds"
mstraits_cache <- "mstraits_species_traits.rds"
species_cache <- "species_traits_enriched.rds"

# ---- Refresh cache?
REFRESH <- TRUE

# Path to the workflow configuration file.
workflow_config_path <- file.path(getwd(), "inst", "templates", "swfscfish_config.yaml")

# Directory where workflow outputs will be written.
output_dir <- file.path(getwd(), "inst", "outputs_swfscfish")

# Step 2: load the package from the working directory.
pkgload::load_all(getwd(), export_all = TRUE, helpers = FALSE, quiet = TRUE)

# Step 3: read the TSL table and the species-level source databases.
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Formatted in the yaml as something like `regional_body = SWFSC`
anchor_filter <- list(
  regional_body = "SWFSC"
)

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

# ------------------------------------------------------------------------------
# READING TS-L SOURCE TABLE
tsb_message(
  "Reading TS-length source table {", input_dir, "/", TSL_file, "} ..."
)
tsl <- read_tsl_table(file.path(input_dir, TSL_file))

# GRAB SPECIES LIST FROM TABLE
species_vector <- unique(stringr::str_trim(paste(tsl$genus, tsl$species)))
species_vector <- species_vector[!is.na(species_vector) & nzchar(species_vector) & species_vector != "NA NA"]
species_vector <- sort(species_vector)
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
# FETCHING FROM WORMS
tsb_message(
  "Fetching species information from WORMS database.\n",
  "Cache file: ", cache_dir, "/", worms_cache
)
worms_db <- fetch_worms(
  species = species_vector,
  cache_path = file.path(cache_dir, worms_cache),
  refresh = REFRESH
)
worms_spp <- unique(paste0(worms_db$genus, " ", worms_db$species))
tsb_message(
  length(worms_spp), " species detected in WORMS database...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# FETCHING FROM FISHBASE
tsb_message(
  "Fetching species information from Fishbase database.\n",
  "Cache file: ", cache_dir, "/", fishbase_cache
)
fishbase_db <- fetch_fishbase(
  species = species_vector,
  cache_path = file.path(cache_dir, fishbase_cache),
  refresh = REFRESH
)
fb_spp <- unique(paste0(fishbase_db$genus, " ", fishbase_db$species))
tsb_message(
  length(fb_spp), " species detected in Fishebase database...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# READ FROM MARINESPECIESTRAITS DATABASE
tsb_message(
  "Reading marineSpeciesTraits database.\n",
  "Cache file: ", cache_dir, "/", mstraits_cache
)
mstraits_db <- read_mstraits_db(
  species = species_vector,
  db_path = file.path(input_dir, mstraits_file),
  cache_path = file.path(cache_dir, mstraits_cache),
  refresh = REFRESH
)
ms_spp <- unique(paste0(mstraits_db$genus, " ", mstraits_db$species))

tsb_message(
  sum(rowSums(is.na(mstraits_db)) != ncol(mstraits_db) - 2),
  " species detected in marineSpeciesTraits database...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# READ FROM PELAGIC SPECIES TRAITS DATABASE
tsb_message(
  "Reading pelagic species traits database.\n",
  "Cache file: ", cache_dir, "/", pelagic_cache
)
pelagic_db <- read_pelagic_db(
  species = species_vector,
  dl_path = input_dir,
  cache_path = file.path(cache_dir, pelagic_cache),
  refresh = REFRESH
)
tsb_message(
  sum(rowSums(is.na(pelagic_db)) != ncol(pelagic_db) - 2),
  " species detected in pelagic species traits database...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# READ FROM NE PACIFIC AND N ATLATIC CONTINENTAL SHELF TRAIT COLLECTION
tsb_message(
  "Reading N.E. Pacific and N. Atlantic continental shelf trait collection.\n",
  "Cache file: ", cache_dir, "/", continental_cache
)
continental_db <- read_continental_db(
  species = species_vector,
  db_path = file.path(input_dir, continental_file),
  cache_path = file.path(cache_dir, continental_cache),
  refresh = REFRESH
)
tsb_message(
  sum(rowSums(is.na(continental_db)) != ncol(continental_db) - 2),
  " species detected in N.E. Pacific and N. Atlantic continental shelf trait collection...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# READ FROM AZORES TRAITS DATABASE
tsb_message(
  "Reading Azores functional traits database.\n",
  "Cache file: ", cache_dir, "/", azores_cache
)
azores_db <- read_azores_db(
  species = species_vector,
  db_path = file.path(input_dir, azores_file),
  cache_path = file.path(cache_dir, azores_cache),
  refresh = REFRESH
)
tsb_message(
  sum(rowSums(is.na(azores_db)) != ncol(azores_db) - 2),
  " species detected in Azores functional traits database...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# ENRICH SPECIES TRAIT TABLE ACROSS SOURCES
tsb_message(
  "Enriching species trait table across all source databases..."
)
species_db <- enrich_species_db(
  db_list = list(
    pelagic = pelagic_db,
    mstraits = mstraits_db,
    azores = azores_db,
    continental = continental_db,
    fishbase = fishbase_db,
    worms = worms_db
  ),
  precedence = c("pelagic", "mstraits", "azores", "continental", "fishbase", "worms"),
  cache_path = file.path(cache_dir, species_cache)
)
tsb_message(
  nrow(species_db), " species retained in enriched species trait table...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# PREPARE THE UNIFIED CANDIDATE-MODEL TABLE
tsb_message(
  "Preparing unified candidate-model table..."
)
candidate_models <- prepare_traits(
  species_db = species_db,
  study_db = tsl,
  cache_path = file.path(cache_dir, "candidate_models_prepared.rds"),
  refresh = REFRESH
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
anchor_keep <- rep(TRUE, nrow(candidate_models))
for (nm in names(anchor_filter)) {
  anchor_keep <- anchor_keep &
    !is.na(candidate_models[[nm]]) &
    candidate_models[[nm]] %in% anchor_filter[[nm]]
}
anchor_ids <- as.character(candidate_models$model_id[anchor_keep])
reference_anchors <- set_reference_anchors(
  candidate_models = candidate_models,
  model_ids = anchor_ids
)
tsb_message(
  length(anchor_ids), " reference survey anchors extracted...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# TUNE THE SIMILARITY CONFIGURATION
tsb_message(
  "Tuning similarity configuration..."
)
tuning_obj <- tune_similarity_empirical(
  candidate_models = candidate_models,
  n_resamples = 20L,
  config = list(
    species_traits = workflow_config$policy$species_traits,
    study_traits = workflow_config$policy$study_traits,
    alpha = workflow_config$policy$alpha,
    k_species = workflow_config$policy$k_species,
    k_study = workflow_config$policy$k_study,
    max_models_per_species = workflow_config$tuning$max_models_per_species,
    n_cores = max(1L, parallel::detectCores(logical = FALSE) - 1L),
    length_coherence = list(method = "overlap",
                            weight = workflow_config$policy$length_overlap_weight),
    depth_coherence = list(method = "overlap",
                           weight = workflow_config$policy$depth_overlap_weight),
    frequency_coherence = list(method = workflow_config$policy$frequency_coherence_mode,
                               weight = workflow_config$policy$frequency_coherence_weight)
  ),
  cache_path = file.path(cache_dir, "similarity_tuning.rds"),
  refresh = REFRESH
)
similarity_config <- tuning_obj$config_tuned
tsb_message(
  "Similarity configuration tuning succeeded...\n",
  "<alpha> = ", workflow_config$policy$alpha, " --> ",
  similarity_config$alpha, "\n",
  "<k_species> = ", workflow_config$policy$k_species, " --> ",
  similarity_config$k_species, "\n",
  "<k_study> = ", workflow_config$policy$k_study, " --> ",
  similarity_config$k_study,
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# PREPARE SIMILARITY INPUTS
tsb_message(
  "Preparing similarity inputs and ordination..."
)
# Rebuild the prepared similarity object using the tuned trait weights and
# scalar parameters from the empirical tuning step.
similarity_obj <- prepare_similarity_matrix(
  candidate_models = candidate_models,
  species_traits = as.list(similarity_config$species_weights),
  study_traits = as.list(similarity_config$study_weights),
  alpha = similarity_config$alpha,
  k_species = similarity_config$k_species,
  k_study = similarity_config$k_study,
  config = similarity_config$coherence
)

# ------------------------------------------------------------------------------
# CALCULATE GOWER DSITANCES
tsb_message(
  "Computing Gower distances..."
)
distance_obj <- build_gower_distances(similarity_obj)

tsb_message(
  "Run two-dimensional NMDS on the distance matrix...\n",
  "loadings: False\n",
  "centroids: False"
)
ordination_obj <- run_ordination(
  dist_mat = distance_obj$combined_dist,
  trait_table = similarity_obj$candidate_models |>
    dplyr::select(dplyr::all_of(distance_obj$trait_cols)),
  include_loadings = TRUE,
  include_centroids = TRUE
)
ordination_points <- join_ordination_points(
  ordination_points = ordination_obj$points,
  candidate_models = candidate_models,
  reference_ids = anchor_ids
)
model_scores <- extract_ordination_scores(ordination_points)
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
points_missing_df <- add_ordination_missing(
  points_df = ordination_points,
  candidate_models = similarity_obj$candidate_models,
  trait_cols = distance_obj$trait_cols
)
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
species_profiles_for_ordination <- similarity_obj$species_profiles |>
  dplyr::filter(!is.na(species_name), species_name != "NA NA")
species_dist_for_ordination <- distance_obj$species_dist[
  species_profiles_for_ordination$species_name,
  species_profiles_for_ordination$species_name,
  drop = FALSE
]
species_ordination_obj <- run_ordination(
  dist_mat = species_dist_for_ordination,
  trait_table = species_profiles_for_ordination |>
    dplyr::select(-species_name),
  include_loadings = TRUE,
  include_centroids = TRUE,
  nmds_args = list(permutations = 999)
)

# ------------------------------------------------------------------------------
# WARD HIERARCHICAL CLUSTERING
species_points <- assign_ordination_groups(
  points_df = species_ordination_obj$points |>
    dplyr::rename(species_name = model_id) |>
    dplyr::left_join(
      species_profiles_for_ordination |>
        dplyr::select(species_name, dplyr::any_of("swimbladder_type")),
      by = "species_name"
    ) |>
    dplyr::mutate(
      is_reference = species_name %in% unique(reference_anchors$species_name)
    ),
  cluster_col = "species_cluster_id"
)
species_cluster_refinement <- refine_species_clusters(
  species_points_df = species_points,
  dist_mat = species_dist_for_ordination
)
species_points <- species_cluster_refinement$points
tsb_message(
  "Plotting species ordination... writing figure to: ",
  file.path(output_dir, workflow_config$outputs$ordination$species),
  ".png"
)
species_ordination_plot <- plot_species_ordination(species_points)

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
species_lookup_obj <- build_species_lookup(
  species_points_df = species_points,
  candidate_models = candidate_models
)
species_lookup <- species_lookup_obj$lookup

# ------------------------------------------------------------------------------
# EVALUATE THE APPLICABILITY AND ADMISSIBILITY OF MODELS
tsb_message(
  "Screening candidate admissibility for each reference-anchor model..."
)
anchor_evaluation_obj <- evaluate_anchor_set(
  reference_anchors = reference_anchors,
  candidate_models = candidate_models,
  config = list(
    species_traits = as.list(similarity_config$species_weights),
    study_traits = as.list(similarity_config$study_weights),
    alpha = similarity_config$alpha,
    k_species = similarity_config$k_species,
    k_study = similarity_config$k_study,
    length_overlap_weight = similarity_config$coherence$length_coherence$weight,
    depth_overlap_weight = similarity_config$coherence$depth_coherence$weight,
    frequency_coherence_weight = similarity_config$coherence$frequency_coherence$weight,
    frequency_coherence_mode = similarity_config$coherence$frequency_coherence$method,
    min_length_overlap_fraction = workflow_config$policy$min_length_overlap_fraction,
    min_depth_overlap_fraction = workflow_config$policy$min_depth_overlap_fraction,
    missing_key_metadata_max_fraction = workflow_config$policy$missing_key_metadata_max_fraction,
    core_weight_cutoff = workflow_config$policy$core_weight_cutoff
  ),
  cache_path = file.path(cache_dir, "anchor_evaluation.rds"),
  refresh = REFRESH
)
# ---- Retain the bound cross-anchor tables for downstream use
anchor_scores <- anchor_evaluation_obj$all_scores
anchor_overlap <- anchor_evaluation_obj$all_overlap
anchor_gates <- anchor_evaluation_obj$all_gates
anchor_summary <- anchor_evaluation_obj$anchor_summary
tsb_message(
  nrow(anchor_summary), " anchors evaluated across the candidate-model pool...",
  timestamp = FALSE
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
# Run the package benchmark layer directly so the full candidate-model set is
# evaluated under both the unrestricted donor pool and the leave-one-species-out
# donor pool.
# The returned object keeps both validation schemes together:
# 1) pseudo-anchor: every model is treated as if it were a reference anchor
# 2) species-block: same-species donors are removed to test cross-species transfer
# The later conformal and policy-selection steps both consume these benchmark
# tables, so they are bound immediately after the benchmark finishes.
benchmark_obj <- run_policy_benchmark(
  candidate_models = candidate_models,
  model_scores = model_scores,
  species_lookup = species_lookup,
  reference_ids = anchor_ids,
  policies = workflow_config$policies$active,
  config = list(
    species_traits = as.list(similarity_config$species_weights),
    study_traits = as.list(similarity_config$study_weights),
    alpha = similarity_config$alpha,
    k_species = similarity_config$k_species,
    k_study = similarity_config$k_study,
    length_overlap_weight = similarity_config$coherence$length_coherence$weight,
    depth_overlap_weight = similarity_config$coherence$depth_coherence$weight,
    frequency_coherence_weight = similarity_config$coherence$frequency_coherence$weight,
    frequency_coherence_mode = similarity_config$coherence$frequency_coherence$method,
    min_length_overlap_fraction = workflow_config$policy$min_length_overlap_fraction,
    min_depth_overlap_fraction = workflow_config$policy$min_depth_overlap_fraction,
    missing_key_metadata_max_fraction = workflow_config$policy$missing_key_metadata_max_fraction,
    core_weight_cutoff = workflow_config$policy$core_weight_cutoff
  ),
  workers = max(1L, parallel::detectCores(logical = FALSE) - 1L),
  cache_path = file.path(cache_dir, "policy_benchmark.rds"),
  refresh = REFRESH,
  progress = TRUE
)
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

# ------------------------------------------------------------------------------
# CALIBRATE POLICY-WISE CONFORMAL INTERVALS
tsb_message(
  "Calibrating policy-wise conformal uncertainty..."
)
# Use the pseudo-anchor benchmark as the calibration set, then summarize how the
# same policy intervals cover both the pseudo-anchor and species-block results.
# This yields one multiplicative uncertainty width per policy, plus an optional
# relative-length calibration table for TS-curve uncertainty bands.
conformal_obj <- run_anchor_conformal(
  policy_perf = policy_perf,
  species_performance_table = species_block_perf,
  ts_error = policy_ts_error_long,
  alpha = workflow_config$policy$conformal_alpha,
  cache_path = file.path(cache_dir, "policy_conformal.rds"),
  refresh = REFRESH
)
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

# ------------------------------------------------------------------------------
# BUILD THE POLICY-SELECTION REFERENCE FROM THE SPECIES-BLOCK BENCHMARK
tsb_message(
  "Building policy-selection reference from species-block benchmark..."
)
# Use the leave-one-species-out benchmark as the policy-choice validation set,
# then summarize near-ties and equivalence classes across the active policies.
# This is the global decision table that says which policies are broadly
# acceptable, which are practically tied, and which should usually be avoided.
selection_obj <- run_policy_selection(
  species_performance_table = species_block_perf,
  config = list(
    tolerance = 0.05,
    n_boot = 500L
  ),
  cache_path = file.path(cache_dir, "policy_selection.rds"),
  refresh = REFRESH
)
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
# Evaluate the active policy set for each retained reference anchor, join the
# conformal calibration and the global policy-selection metadata, and then keep
# one displayed policy row per anchor.
# Three cross-anchor outputs are accumulated here:
# 1) `policy_intervals`: every anchor-policy prediction with uncertainty bounds
# 2) `selected_policies`: the one displayed policy row kept for each anchor
# 3) `consensus_multipliers`: the admissible-pool weighted multiplier summary
#    for each anchor, independent of the discrete policy choices
all_policy_intervals <- list()
selected_policy_rows <- list()
consensus_multiplier_rows <- list()

for (i in seq_len(nrow(reference_anchors))) {
  # Pull the current anchor row and its key identifiers once so every
  # downstream table for this iteration carries the same anchor keys.
  anchor_row <- reference_anchors[i, , drop = FALSE]
  anchor_id <- as.character(anchor_row$model_id[[1]])
  anchor_species <- as.character(anchor_row$species_name[[1]])

  tsb_message(
    "Reference anchor ", i, "/", nrow(reference_anchors), ": ", anchor_species
  )

  # Re-evaluate the admissible donor pool for this specific anchor so the
  # downstream policy predictions use the same tuned similarity settings and
  # admissibility gates as the earlier anchor-screening step.
  eval_obj <- evaluate_anchor_models(
    anchor_row = anchor_row,
    candidate_models = candidate_models,
    config = list(
      species_traits = as.list(similarity_config$species_weights),
      study_traits = as.list(similarity_config$study_weights),
      alpha = similarity_config$alpha,
      k_species = similarity_config$k_species,
      k_study = similarity_config$k_study,
      length_overlap_weight = similarity_config$coherence$length_coherence$weight,
      depth_overlap_weight = similarity_config$coherence$depth_coherence$weight,
      frequency_coherence_weight = similarity_config$coherence$frequency_coherence$weight,
      frequency_coherence_mode = similarity_config$coherence$frequency_coherence$method,
      min_length_overlap_fraction = workflow_config$policy$min_length_overlap_fraction,
      min_depth_overlap_fraction = workflow_config$policy$min_depth_overlap_fraction,
      missing_key_metadata_max_fraction = workflow_config$policy$missing_key_metadata_max_fraction,
      core_weight_cutoff = workflow_config$policy$core_weight_cutoff
    )
  )

  # Build the anchor-specific ordination context that certain policies use to
  # define their local neighborhood around the current reference anchor.
  ordination_info <- build_anchor_ordination(
    anchor_row = anchor_row,
    model_scores = model_scores,
    species_lookup = species_lookup
  )

  # Generate one prediction row per active policy, then join:
  # 1) conformal calibration widths for multiplier intervals
  # 2) global benchmark summaries from the policy-selection table
  # The first mutate adds anchor identifiers and a simple validity flag so
  # broken or non-positive multiplier predictions can be screened later.
  policy_tbl <- evaluate_policies(
    eval_obj = eval_obj,
    ordination_info = ordination_info,
    policies = workflow_config$policies$active
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      anchor_model_id = anchor_id,
      anchor_species = anchor_species,
      valid_prediction = is.finite(multiplier_pred) & multiplier_pred > 0
    ) |>
    # Join the policy-specific conformal calibration so each point prediction
    # can be expanded into a multiplicative uncertainty interval.
    dplyr::left_join(
      conf_cal |>
        dplyr::select(policy, q_abs_log, n, median_abs_log),
      by = "policy"
    ) |>
    # Convert the calibrated absolute log error into lower and upper multiplier
    # bounds and keep the log-width as a compact uncertainty summary.
    dplyr::mutate(
      multiplier_lo = dplyr::if_else(
        valid_prediction & is.finite(q_abs_log),
        multiplier_pred * exp(-q_abs_log),
        NA_real_
      ),
      multiplier_hi = dplyr::if_else(
        valid_prediction & is.finite(q_abs_log),
        multiplier_pred * exp(q_abs_log),
        NA_real_
      ),
      interval_log_width = dplyr::if_else(
        is.finite(q_abs_log),
        2 * q_abs_log,
        NA_real_
      )
    ) |>
    # Join the global validation summaries so each anchor-policy row knows how
    # that policy performed across the species-block benchmark.
    dplyr::left_join(
      policy_selection_final |>
        dplyr::select(
          policy,
          mean_species_median_abs_log,
          acceptable_one_se,
          acceptable_bootstrap,
          acceptable_global,
          equivalent_to_best_global,
          paired_mean_diff_to_best,
          one_se_threshold,
          bootstrap_prob_within_threshold,
          bootstrap_prob_best,
          bootstrap_median_rank,
          specificity_rank,
          equivalence_class_id,
          equivalence_class_size,
          equivalence_class_members
        ),
      by = "policy"
    )

  # Keep one displayed policy row per anchor by favoring policies that passed
  # the global selection screen, then breaking ties by benchmark error and the
  # policy specificity ranking.
  # The resulting row is the anchor-facing recommendation table, not the full
  # policy comparison table retained in `policy_intervals`.
  selected_row <- policy_tbl |>
    dplyr::filter(valid_prediction) |>
    dplyr::arrange(
      dplyr::desc(acceptable_global),
      dplyr::desc(equivalent_to_best_global),
      mean_species_median_abs_log,
      specificity_rank,
      policy
    ) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      selected_policy = policy,
      selected_policy_display = policy
    )

  # Preserve an explicit NA row when no policy produced a usable multiplier so
  # the final anchor-level outputs still retain one row per reference anchor.
  if (nrow(selected_row) == 0) {
    selected_row <- tibble::tibble(
      anchor_model_id = anchor_id,
      anchor_species = anchor_species,
      selected_policy = NA_character_,
      selected_policy_display = NA_character_,
      multiplier_pred = NA_real_,
      multiplier_lo = NA_real_,
      multiplier_hi = NA_real_
    )
  }

  # Store the weighted consensus summary from the admissible donor pool
  # alongside the discrete policy predictions. This is the continuous baseline
  # against which the later policy summaries can be compared.
  # This summary does not choose among policies; it only reflects the weighted
  # donor-pool distribution for the current anchor.
  consensus_row <- summarize_evaluation(eval_obj) |>
    dplyr::mutate(
      anchor_model_id = anchor_id,
      anchor_species = anchor_species,
      .before = 1
    )

  # Append the three anchor-level products to their cross-anchor collectors:
  # the full policy table, the selected display row, and the donor-pool
  # consensus summary.
  all_policy_intervals[[length(all_policy_intervals) + 1]] <- policy_tbl
  selected_policy_rows[[length(selected_policy_rows) + 1]] <- selected_row
  consensus_multiplier_rows[[length(consensus_multiplier_rows) + 1]] <- consensus_row
}

# Bind the anchor-level lists only after every reference anchor has been
# processed so the downstream summary and plotting sections can work from
# simple full-workflow tables.
policy_intervals <- dplyr::bind_rows(all_policy_intervals)
selected_policies <- dplyr::bind_rows(selected_policy_rows)
consensus_multipliers <- dplyr::bind_rows(consensus_multiplier_rows)
tsb_message(
  nrow(selected_policies), " reference-anchor policy rows retained...",
  timestamp = FALSE
)

# ------------------------------------------------------------------------------
# RERUN THE POLICY STACK ACROSS STANDARD SENSITIVITY SCENARIOS
tsb_message(
  "Rerunning policy benchmark and selection across sensitivity scenarios..."
)
# Build the standard scenario set from the current candidate-model table and
# the tuned policy/admissibility settings used above.
policy_sensitivity_specs <- build_policy_sensitivity_scenarios(
  candidate_models = candidate_models,
  config = list(
    species_traits = as.list(similarity_config$species_weights),
    study_traits = as.list(similarity_config$study_weights),
    alpha = similarity_config$alpha,
    k_species = similarity_config$k_species,
    k_study = similarity_config$k_study,
    length_overlap_weight = similarity_config$coherence$length_coherence$weight,
    depth_overlap_weight = similarity_config$coherence$depth_coherence$weight,
    frequency_coherence_weight = similarity_config$coherence$frequency_coherence$weight,
    frequency_coherence_mode = similarity_config$coherence$frequency_coherence$method,
    min_length_overlap_fraction = workflow_config$policy$min_length_overlap_fraction,
    min_depth_overlap_fraction = workflow_config$policy$min_depth_overlap_fraction,
    missing_key_metadata_max_fraction = workflow_config$policy$missing_key_metadata_max_fraction,
    core_weight_cutoff = workflow_config$policy$core_weight_cutoff,
    conformal_alpha = workflow_config$policy$conformal_alpha
  )
)

# Seed the scenario map with the already-computed baseline benchmark,
# conformal, and policy-selection objects so only the non-baseline scenarios
# need to be rerun.
policy_sensitivity_map <- run_sensitivity_tests(
  sensitivity_specs = policy_sensitivity_specs,
  benchmark_fun = run_policy_sensitivity_reference,
  baseline_obj = list(
      ord_ctx = list(
        model_scores = model_scores,
        species_lookup = species_lookup,
        points_missing_df = points_missing_df
      ),
      policy_perf = policy_perf,
      species_block_perf = species_block_perf,
      conf_cal = conf_cal,
    selection_ref = policy_selection_final,
    equivalence_pairs = policy_equivalence_ref$pairs,
    equivalence_classes = policy_equivalence_sets
  ),
  benchmark_args = list(
    policies = workflow_config$policies$active,
    reference_ids = anchor_ids,
    include_ts_error = FALSE,
    progress = TRUE
  ),
  workers = max(1L, parallel::detectCores(logical = FALSE) - 1L),
  cache_path = file.path(cache_dir, "policy_sensitivity.rds"),
  refresh = REFRESH,
  progress = TRUE
)

# Collapse the scenario map to one manifest plus the scenario-bound policy,
# equivalence, and conformal tables used later in the summary layer.
policy_sensitivity_manifest <- build_sensitivity_table(
  sensitivity_specs = policy_sensitivity_specs,
  sensitivity_map = policy_sensitivity_map
)
policy_sensitivity_tables <- bind_sensitivity_data(
  sensitivity_map = policy_sensitivity_map
)
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
species_block_coverage <- build_species_coverage(
  pseudo_sum = conformal_perf_pseudo,
  species_sum = conformal_perf_species_block
)

# Collapse the selected anchor-policy rows and the benchmark/global policy
# summaries into one anchor-facing audit table.
anchor_policy_audit <- build_anchor_audit(
  sel_tbl = selected_policies,
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

# ------------------------------------------------------------------------------
# RENDER THE MAIN WORKFLOW FIGURES
tsb_message(
  "Rendering workflow figures..."
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Join the selected policy result to the donor-pool consensus summary so the
# integrated anchor-level multiplier figure can show both discrete policy
# recommendations and the weighted admissible-pool baseline.
anchor_multiplier_summary <- selected_policies |>
  dplyr::select(anchor_model_id, anchor_species, multiplier_pred, multiplier_lo, multiplier_hi) |>
  dplyr::left_join(
    consensus_multipliers |>
      dplyr::transmute(
        anchor_model_id,
        hybrid_consensus_multiplier = consensus_multiplier,
        hybrid_multiplier_q05 = multiplier_q05,
        hybrid_multiplier_q50 = multiplier_q50,
        hybrid_multiplier_q95 = multiplier_q95
      ),
    by = "anchor_model_id"
  )

# Build the main figure objects directly from the workflow summary tables.
workflow_plots <- list(
  slope_distribution = plot_slope_distribution(slope_dependence$study_cell_level),
  slope_group = plot_slope_group(slope_dependence$study_cell_level),
  slope_support = plot_slope_support(slope_dependence$deviation_support_by_group),
  ordination_clusters = plot_ordination_clusters(ordination_points),
  species_ordination = plot_species_ordination(species_points),
  overlap_heatmap = plot_overlap_heatmap(anchor_overlap),
  gate_composition = plot_gate_composition(anchor_gates),
  policy_benchmark = plot_policy_boxplot(policy_perf),
  species_block_benchmark = plot_species_boxplot(species_block_perf),
  policy_heatmap = plot_policy_heatmap(species_block_perf),
  conformal_scores = plot_conformal_scores(conf_cal),
  selected_policy_intervals = plot_selected_intervals(selected_policies),
  anchor_multiplier_summary = plot_anchor_summary(
    integrated_tbl = anchor_multiplier_summary,
    score_tbl = anchor_scores,
    interval_tbl = policy_intervals
  ),
  area_distribution = plot_area_distribution(
    count_tbl = area_study_summary,
    inset_tbl = area_inset_tiles
  ),
  anchor_policy_audit = plot_anchor_audit(anchor_policy_audit),
  field_missingness = plot_field_missing(key_missing_by_field),
  anchor_missingness = plot_anchor_missing(anchor_missing_gate)
)

# Persist the rendered figures to the configured output directory and also
# keep the plot list in memory for interactive review in RStudio.
for (plot_name in names(workflow_plots)) {
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(plot_name, ".png")),
    plot = workflow_plots[[plot_name]],
    width = 9,
    height = 6,
    dpi = 300
  )
}
saveRDS(
  workflow_plots,
  file.path(output_dir, "workflow_plots.rds")
)
tsb_message(
  length(workflow_plots), " workflow figures written to {", output_dir, "} ...",
  timestamp = FALSE
)
