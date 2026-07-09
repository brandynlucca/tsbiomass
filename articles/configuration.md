# Configuration Reference

The `tsbiomass` pipeline is entirely configuration-driven. A single YAML
or list object — validated and normalized into a `Configurer` —
establishes the full run contract before any modeling begins. This page
describes every valid top-level section and key.

``` r

library(tsbiomass)

# From a YAML file
cfg <- build_configurer("path/to/config.yaml")

# From a list (useful for scripted runs)
cfg <- build_configurer(create_configuration_template("input.xlsx"))
```

  

## `paths`

File and directory locations consumed by the pipeline.

| Key | Type | Required | Description |
|----|----|----|----|
| `input_file` | string | **yes** | Primary study workbook (`.xlsx`). |
| `out_root` | string | **yes** | Root output folder. |
| `cache_dir` | string | **yes** | Default cache folder. |
| `supplemental_dir` | string | no | Folder containing supplemental trait files. |
| `log_file` | string | no | Log-file path. Written only when `execution.write_log = true`. |
| `fao_polygon_csv` | string | no | FAO major-area polygon CSV for spatial admissibility. |

External YAML aliases accepted: `input` → `input_file`, `output_root` →
`out_root`, `cache_folder` → `cache_dir`, `support_folder` →
`supplemental_dir`, `area_file` → `fao_polygon_csv`, `log_path` →
`log_file`.

------------------------------------------------------------------------

## `execution`

Runtime behavior switches.

| Key | Type | Default | Description |
|----|----|----|----|
| `strict_length_pdf` | logical | `false` | Requires finite length-support bounds for every model. When `true`, models with missing bounds are excluded from PDF construction. |
| `run_multiplier_model` | logical | `false` | Enables the biomass-multiplier workflow branch. |
| `write_log` | logical | `false` | Writes progress and error messages to `paths.log_file`. |

External YAML aliases: `strict_pdf` → `strict_length_pdf`,
`run_multiplier` → `run_multiplier_model`.

------------------------------------------------------------------------

## `cache`

Cache folder and per-product filename overrides.

| Key | Type | Default | Description |
|----|----|----|----|
| `folder` | string | `"cache"` | Base folder for all cache files. |
| `refresh` | logical | `false` | Global default: when `true`, every cached product is recomputed. Individual sections can override this. |
| `names` | named list | (packaged defaults) | Per-product filename overrides. Keys: `worms`, `fishbase`, `pelagic`, `azores`, `continental`, `mstraits`, `species_enriched`, `candidate_models`, `similarity_tuning`, `anchor_admissibility`, `policy_benchmark`, `policy_conformal`, `policy_selection`, `policy_sensitivity`. |
| `defaults_path` | string | (packaged JSON) | Optional path to a custom cache-defaults JSON file. |

------------------------------------------------------------------------

## `data`

Trait source declarations. Each entry is one dataset. At minimum you
need a `study_metadata` entry pointing at your study workbook.

| Key | Type | Required | Description |
|----|----|----|----|
| `id` | string | **yes** | Dataset identifier (case-insensitive). Built-in IDs: `study_metadata`, `worms`, `fishbase`, `pelagic` / `pelagictraits`, `azores` / `azorestraits`, `continental` / `continentaltraits`, `mstraits`. |
| `alias` | string | no | Alternate slug for use in `candidates.enrich.precedence`. |
| `type` | string | no | External type descriptor (e.g. `single_file`, `directory`). Back-filled for built-in IDs. |
| `engine` | string | no | Engine descriptor (e.g. `r_package`, `rdata`). Back-filled for built-in IDs. |
| `path` | string | conditional | File or folder path. Required for sources that read from disk. |
| `cache_path` | string | no | Explicit cache-path override for this source. |
| `refresh` | logical | no | Per-source refresh override. |

------------------------------------------------------------------------

## `candidates`

Candidate-pool orchestration layered on top of `data`.

### `candidates.enrich`

| Key | Type | Description |
|----|----|----|
| `precedence` | character vector | Source priority order during trait enrichment. May use normalized `id` or `alias`. |
| `missing_tokens` | character vector | Values treated as missing (e.g. `"-9999"`, `"unknown"`). |
| `cache_path` | string | Explicit cache override for the enriched species table. |

### `candidates.prepare`

| Key | Type | Description |
|----|----|----|
| `missing_tokens` | character vector | Missing-value tokens applied during trait preparation. |
| `cache_path` | string | Explicit cache override for the prepared candidate table. |
| `refresh` | logical | Per-stage refresh override. |

### `candidates.anchors`

Reference-anchor selection settings passed to
[`set_reference_anchors()`](https://brandynlucca.github.io/tsbiomass/reference/set_reference_anchors.md).

| Key | Type | Description |
|----|----|----|
| `selector` | named list | Dynamic filter (e.g. `{regional_body: SWFSC}`). |
| `model_ids` | character vector | Explicit anchor model IDs (alternative to `selector`). |
| `model_id_col` | string | Column name for anchor IDs when using `model_ids`. |
| `require_selection` | logical | Whether zero matches should error (default `true`). |

------------------------------------------------------------------------

## `alchemist`

Settings for the `Alchemist` supervised distance learner. Inherits trait
names from `similarity` when not specified.

| Key | Type | Default | Description |
|----|----|----|----|
| `feature_type` | string | `"gower"` | Pairwise feature representation. Options: `"gower"` (unsigned Gower distances), `"difference"` (signed standardized differences), `"mahalanobis"` (squared standardized differences). |
| `taxonomic_distance` | logical | `false` | Replace individual family/genus/species Gower features with a single continuous phylogenetic distance (Open Tree of Life, with rank-based fallback). |
| `species_traits` | character vector | (from `similarity`) | Species traits used as pair-level features. |
| `study_traits` | character vector | (from `similarity`) | Study traits used as pair-level features. |
| `distill_workers` | integer | `1` | Worker count for sigma-dropout sensitivity in [`distill_traits()`](https://brandynlucca.github.io/tsbiomass/reference/distill_traits.md). |
| `progress` | logical | `false` | Enable progress messages. |

### `alchemist.learner`

Controls the Super Learner trained inside
[`forge_distances()`](https://brandynlucca.github.io/tsbiomass/reference/forge_distances.md).
See the [Super Learners
vignette](https://brandynlucca.github.io/tsbiomass/articles/super-learners.md)
for guidance on method selection.

| Key | Type | Default | Description |
|----|----|----|----|
| `methods` | character vector | `["glm_elastic", "rf", "xgboost"]` | Base learner methods. See [`list_learners()`](https://brandynlucca.github.io/tsbiomass/reference/list_learners.md) for valid names. |
| `inner_folds` | integer | `5` | Inner cross-validation folds for out-of-fold predictions. |
| `seed` | integer | `NULL` | Reproducibility seed. |
| `outcome_transform` | string | `"log1p"` | Transform applied to the acoustic-distance outcome before fitting. Options: `"identity"`, `"log1p"`, `"sqrt"`. |
| `lambda_rule` | string | `"lambda.1se"` | Penalized-model lambda selection. Options: `"lambda.min"`, `"lambda.1se"`. |
| `oof_mode` | string | `"anchor_species"` | Out-of-fold split strategy. `"anchor_species"` groups by anchor species; `"species_purged"` excludes both anchor and donor species from training. |
| `workers` | integer | `1` | Parallel worker count for multi-method fitting. |
| `method_settings` | named list | (defaults) | Per-family hyper-parameter overrides (see `uncertainty.method_settings` for the shared schema). |

------------------------------------------------------------------------

## `similarity`

Trait selection, coherence, and kernel settings for the similarity
matrix. This is the primary tuning surface.

| Key | Type | Default | Description |
|----|----|----|----|
| `alpha` | number | `0.8` | Species-versus-study blend in the combined distance. `1.0` = species only, `0.0` = study only. |
| `kernel_scale` | number | `4` | Global kernel scale. Higher values concentrate weight on nearer donors. |
| `species_traits` | character vector or named list | (registry defaults) | Species traits included in distance computation. Named list form sets per-trait starting weights. |
| `study_traits` | character vector or named list | (registry defaults) | Study traits included in distance computation. |
| `conformal_alpha` | number | `0.1` | Target miscoverage level for conformal interval calibration. |
| `core_weight_cutoff` | number | `NULL` | Similarity-weight threshold for identifying high-support donor subsets. |
| `alpha_range` | `{from, to}` | `{0.1, 0.9}` | Search bounds for alpha during tuning. |
| `kernel_scale_range` | `{from, to}` | `{1, 8}` | Search bounds for kernel scale during tuning. |
| `cache_path` | string | (derived) | Explicit cache override. |
| `refresh` | logical | `false` | Force recompute of the similarity cache. |
| `progress` | logical | `false` | Enable stage messages. |

### `similarity.coherence`

Measurement-overlap features that supplement trait distances. Each
sub-section (`length`, `depth`, `frequency`) accepts `mode` and
`weight`, plus `gap` for `frequency`.

| Key | Type | Options | Description |
|----|----|----|----|
| `length.mode` | string | `overlap`, `literal`, `none` | How length ranges are compared for the coherence feature. |
| `length.weight` | number | — | Feature weight for length coherence. |
| `length.source` | string | `best`, `study`, `species`, `both` | Which length columns back this feature. `"best"` prefers study-level bounds with species-level fallback; `"both"` produces separate `_study` and `_species` features. |
| `depth.mode` | string | `overlap`, `literal`, `none` | How depth ranges are compared. |
| `depth.weight` | number | — | Feature weight for depth coherence. |
| `depth.source` | string | `best`, `study`, `species`, `both` | Which depth columns to use. |
| `frequency.mode` | string | `overlap`, `literal`, `none` | How acoustic frequencies are compared. |
| `frequency.weight` | number | — | Feature weight for frequency coherence. |
| `frequency.gap` | number | `60` | Maximum frequency gap (kHz) treated as overlapping under `mode: overlap`. |

------------------------------------------------------------------------

## `ordination`

NMDS ordination settings, applied after
[`forge_distances()`](https://brandynlucca.github.io/tsbiomass/reference/forge_distances.md).

| Key | Type | Default | Description |
|----|----|----|----|
| `include_loadings` | logical | `false` | Include envfit vector loadings in the ordination output. |
| `include_centroids` | logical | `false` | Include factor-level centroids. |
| `progress` | logical | `false` | Enable ordination messages. |
| `nmds_args` | named list | — | Arguments forwarded to [`vegan::metaMDS()`](https://vegandevs.github.io/vegan/reference/metaMDS.html). |
| `envfit_args` | named list | — | Arguments forwarded to [`vegan::envfit()`](https://vegandevs.github.io/vegan/reference/envfit.html). |

------------------------------------------------------------------------

## `admissibility`

Hard binary gates that exclude transfer paths before any policy is
applied. Donors that fail an admissibility check are removed from the
candidate pool for the affected target.

| Key | Type | Description |
|----|----|----|
| `species_traits` | character vector | Categorical or binary species traits that must match exactly between donor and target. |
| `study_traits` | character vector | Study-level traits used as exact-match gates. |
| `key_metadata_max` | number | Maximum tolerated missing fraction across key study-level metadata fields. |
| `cache_path` | string | Explicit cache override. |
| `refresh` | logical | Force recompute. |
| `progress` | logical | Enable messages. |

### `admissibility.coherence`

| Key              | Type   | Description                                 |
|------------------|--------|---------------------------------------------|
| `length.mode`    | string | `overlap`, `literal`, or `none`.            |
| `length.min`     | number | Minimum required fractional length overlap. |
| `depth.mode`     | string | `overlap`, `literal`, or `none`.            |
| `depth.min`      | number | Minimum required fractional depth overlap.  |
| `frequency.mode` | string | `none`, `literal`, or `overlap`.            |
| `frequency.gap`  | number | Gap threshold (kHz) under `mode: overlap`.  |

------------------------------------------------------------------------

## `policies`

Constructor-style policy declarations. Use the `group` + `metric` +
`branch` form to generate named policy sets from the registry, or supply
`active` as an explicit list of policy name strings.

### Constructor form

``` yaml
policies:
  metric: [closest, weighted_mean, unweighted_mean]
  branch: [all, fixed_slope]
  group:
    species:
      include_base: true
      metric: [closest, weighted_mean]
      joint:
        - fao
        - [ocean_basin, season]
    genus:
      include_base: true
    family:
```

| Key | Type | Description |
|----|----|----|
| `metric` | character vector | Global metric filter. Supported: `closest`, `weighted_mean`, `unweighted_mean`, `survey_distance`, `taxon_distance`, `species_distance`, `random`. |
| `branch` | character vector | Equation branch filters. Supported: `all`, `fixed_slope`, `free_slope`. |
| `group` | named list | One entry per donor-pool grouping. Keys are group names; values configure per-group metric/branch overrides and optional `joint` conjunctions. |
| `group.<name>.include_base` | logical | When `joint` variants are listed, keep the plain root group as well (default `true`). |
| `group.<name>.metric` | character vector | Per-group metric override. |
| `group.<name>.joint` | list | One or more trait names to conjoin with the root group key. Each entry becomes a separate `<group>_<trait>` policy variant. |

### Explicit form

``` yaml
policies:
  active: [species_closest_all, genus_weighted_mean_all]
  equation_branch_filters: [all]
```

------------------------------------------------------------------------

## `benchmark`

Settings for the policy-benchmarking stage.

| Key | Type | Default | Description |
|----|----|----|----|
| `workers` | integer | `1` | Worker count for parallel anchor evaluation. |
| `engine` | string | `"cpp"` | Evaluation engine. `"cpp"` uses the compiled C++ backend; `"r"` uses the pure-R path. |
| `include_ts_error` | logical | `false` | Include TS-curve reconstruction error in benchmark outputs. |
| `cache_path` | string | (derived) | Explicit cache override. |
| `refresh` | logical | `false` | Force recompute. |
| `progress` | logical | `false` | Enable messages. |

------------------------------------------------------------------------

## `uncertainty`

The conditional-uncertainty Super Learner. Trains a cross-fitted model
to predict expected absolute log-error, which is used to calibrate
prediction interval widths. See the [Super Learners
vignette](https://brandynlucca.github.io/tsbiomass/articles/super-learners.md).

| Key | Type | Default | Description |
|----|----|----|----|
| `method` | string | `"glm"` | Learner method. Use `"super_learner"` to enable ensemble mode. |
| `super_methods` | character vector | `NULL` | Base learner library when `method = "super_learner"`. |
| `n_folds` | integer | `5` | Outer cross-fitting folds. |
| `inner_folds` | integer | `3` | Inner folds for penalized learners. |
| `workers` | integer | `1` | Worker count for cross-fitting. |
| `outcome_col` | string | `"error_abs_log"` | Benchmark column used as the regression target. |
| `outcome_clip_quantile` | number | `0.99` | Upper quantile used to clip extreme training outcomes. |
| `outcome_transform` | string | `"log1p"` | Transform applied to the outcome before fitting. |
| `lambda_rule` | string | `"lambda.1se"` | Lambda selection rule for penalized learners. |
| `loss` | string | `"squared_error"` | Super-learner combiner loss. Must be `"squared_error"` with NNLS. |
| `method_settings` | named list | (defaults) | Per-family hyper-parameter overrides (see schema below). |
| `cache_path` | string | (derived) | Explicit cache override. |
| `refresh` | logical | `false` | Force recompute. |
| `progress` | logical | `false` | Enable messages. |

------------------------------------------------------------------------

## `selection`

Post-benchmarking policy selection, plus the optional meta-learner used
by `PolicyLearner`. See the [Super Learners
vignette](https://brandynlucca.github.io/tsbiomass/articles/super-learners.md).

| Key | Type | Default | Description |
|----|----|----|----|
| `one_se_multiplier` | number | `1` | Multiplier on the best policy’s standard error for the one-SE screen. |
| `equivalence_tolerance` | number | `0.05` | Practical tolerance for pairwise equivalence summaries. |
| `n_boot` | integer | `500` | Bootstrap replicates for uncertainty in selection. |
| `seed` | integer | `NULL` | Selection seed. |
| `uncertainty_rule` | string | `"tolerance"` | Final width filter. `"min"` keeps only the narrowest interval; `"tolerance"` keeps widths within the configured absolute/relative band. |
| `u_tol_rel` | number | `0.25` | Relative width tolerance (fraction of minimum width) under `uncertainty_rule = tolerance`. |
| `u_tol_abs` | number | `0.05` | Absolute log-width tolerance under `uncertainty_rule = tolerance`. |
| `conformal_alpha` | number | `0.10` | Miscoverage level for post-selection conformal calibration. |
| `method` | string | `"glm"` | Meta-learner method used by `PolicyLearner`. Use `"super_learner"` for ensemble mode. |
| `super_methods` | character vector | `NULL` | Base learner library when `method = "super_learner"`. |
| `n_folds` | integer | `5` | Outer cross-fitting folds. |
| `inner_folds` | integer | `5` | Inner folds for penalized learners. |
| `workers` | integer | `1` | Cross-fitting worker count. |
| `outcome_col` | string | `"error_abs_log"` | Benchmark column used as the learner target. |
| `outcome_transform` | string | `"log1p"` | Outcome transform. |
| `lambda_rule` | string | `"lambda.1se"` | Lambda rule. |
| `loss` | string | `"squared_error"` | Combiner loss. Must be `"squared_error"` with NNLS. |
| `method_settings` | named list | (defaults) | Per-family hyper-parameter overrides. |
| `cache_path` | string | (derived) | Explicit cache override. |
| `refresh` | logical | `false` | Force recompute. |
| `progress` | logical | `false` | Enable messages. |

------------------------------------------------------------------------

## `method_settings` schema

Both `uncertainty` and `selection` (and `alchemist.learner`) accept a
`method_settings` block. Each top-level key is a method family.

**`glm_penalized`**

| Key            | Default | Description                                 |
|----------------|---------|---------------------------------------------|
| `standardize`  | `true`  | Standardize features before fitting.        |
| `type_measure` | `"mse"` | Cross-validation loss for lambda selection. |

**`gam`**

| Key | Default | Description |
|----|----|----|
| `fit_method` | `"GCV.Cp"` | GAM smoothing method. |
| `select_terms` | `false` | Use `select = TRUE` in [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) for additional shrinkage. |

**`rpart`**

| Key | Default | Description |
|----|----|----|
| `cp` | `0.01` | Complexity parameter. |
| `minsplit` | `10` | Minimum observations to attempt a split. |
| `minbucket` | `5` | Minimum observations in a leaf. |
| `maxdepth` | `10` | Maximum tree depth. |
| `variants` | [`{}`](https://rdrr.io/r/base/Paren.html) | Named variant overrides (e.g. `shallow`, `deep`). Each variant becomes a distinct learner named `rpart_<variant>`. |

**`rf`**

| Key | Default | Description |
|----|----|----|
| `num_trees` | `500` | Number of trees. |
| `mtry` | `NULL` | Features sampled per split (auto if `NULL`). |
| `min_node_size` | `5` | Minimum node size. |
| `sample_fraction` | `0.8` | Row-sampling fraction. |
| `replace` | `false` | Sample with replacement. |
| `max_depth` | `NULL` | Maximum tree depth (`NULL` = unlimited). |
| `variants` | [`{}`](https://rdrr.io/r/base/Paren.html) | Named variant overrides (e.g. `shallow`, `deep`). |

**`xgboost`**

| Key | Default | Description |
|----|----|----|
| `nrounds` | `500` | Boosting rounds. |
| `eta` | `0.05` | Learning rate. |
| `max_depth` | `6` | Maximum tree depth. |
| `min_child_weight` | `5` | Minimum child weight. |
| `subsample` | `0.8` | Row-sampling fraction. |
| `colsample_bytree` | `0.8` | Column-sampling fraction per tree. |
| `lambda` | `1.0` | L2 regularization. |
| `alpha` | `0.0` | L1 regularization. |
| `variants` | [`{}`](https://rdrr.io/r/base/Paren.html) | Named variant overrides (e.g. `conservative`, `flexible`). |

**`qreg`** *(quantile regression)*

| Key | Description |
|----|----|
| `variants` | Named variant overrides with a `tau` field (e.g. `{q75: {tau: 0.75}}`). Each variant becomes `qreg_<name>`. |

**`qrf`** *(quantile regression forest)*

| Key               | Default | Description            |
|-------------------|---------|------------------------|
| `num_trees`       | `500`   | Number of trees.       |
| `min_node_size`   | `5`     | Minimum node size.     |
| `sample_fraction` | `0.8`   | Row-sampling fraction. |
| `quantile`        | `0.9`   | Prediction quantile.   |

**`gpr`** *(Gaussian process regression)*

| Key   | Default | Description                              |
|-------|---------|------------------------------------------|
| `var` | `0.001` | Nugget variance for numerical stability. |

------------------------------------------------------------------------

## `sentinel`

Outer-loop validation and ablation settings for
[`build_sentinel()`](https://brandynlucca.github.io/tsbiomass/reference/build_sentinel.md)
/
[`run_sentinel()`](https://brandynlucca.github.io/tsbiomass/reference/run_sentinel.md).

| Key | Type | Default | Description |
|----|----|----|----|
| `baseline_species_folds` | integer | `10` | Number of species-disjoint outer folds for the headline validation run. Set to the total species count for full LOSO. |
| `ablation_species_folds` | integer | `5` | Folds used per ablation scenario. |
| `workers` | integer | `1` | Outer workflow worker count. |
| `batch_size` | integer | `1` | Workflows checkpointed per batch. |
| `include_ts_error` | logical | `false` | Include TS-curve error in fold-local benchmarks. |
| `throttle_inner_workers` | logical | `true` | Prevent inner workers from oversubscribing cores allocated to outer workers. |
| `fast_validation` | logical | `false` | Reduce NMDS schedule when `true`. |
| `progress` | logical | `false` | Enable fold-level console output. |
| `logging` | logical | `false` | Enable fold-level file logging. |
| `save_case_artifacts` | logical | `false` | Persist fold-level RDS artifacts for debugging. |

------------------------------------------------------------------------

## `simulation`

Settings for `PolicySimulator` sensitivity runs.

| Key          | Type    | Default   | Description              |
|--------------|---------|-----------|--------------------------|
| `workers`    | integer | `1`       | Worker count.            |
| `cache_path` | string  | (derived) | Explicit cache override. |
| `refresh`    | logical | `false`   | Force recompute.         |
| `progress`   | logical | `false`   | Enable messages.         |

------------------------------------------------------------------------

## `tuning`

Similarity-tuning hyper-parameters.

| Key | Type | Default | Description |
|----|----|----|----|
| `max_models_per_species` | integer | `2` | Maximum retained models per species in tuning subsets. |
| `n_resamples` | integer | `8` | Number of empirical tuning resamples. |
| `n_cores` | integer | `1` | Worker count for tuning runs. |
| `seed` | integer | `NULL` | Reproducibility seed. |
| `grid_refinement_levels` | integer | `1` | Local search-refinement passes after the coarse grid. |
| `response_surface_top_n` | integer | `20` | Candidate points kept during response-surface refinement. |
| `equal_start_weights` | logical | `false` | Initialize all trait weights equally rather than using configured magnitudes as priors. |
| `progress` | logical | `false` | Enable tuning messages. |

External YAML aliases: `species_model_limit` → `max_models_per_species`,
`resamples` → `n_resamples`, `n_cores` → `n_cores`.
