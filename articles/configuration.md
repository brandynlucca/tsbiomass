# Configuration Reference

The `tsbiomass` pipeline is entirely configuration-driven. A single YAML
file or R list, validated and normalized into a `Configurer`,
establishes the full run contract before any modeling begins. This page
documents every valid section and key.

``` r

library(tsbiomass)

# From a YAML file
cfg <- build_configurer("path/to/config.yaml")

# From the default list template (useful for scripted runs)
cfg <- build_configurer(create_configuration_template())
```

Configuration keys are grouped by the pipeline stage they control. Every
key expands to its own dropdown describing its type and behavior. The
complete set of default values is collected in one place at the bottom
of this page; see [Default configuration](#default-configuration).

  

## Run setup

Infrastructure settings that establish paths, runtime behavior, and
caching before any modeling begins.

### File & directory locations (`paths`)

Where the pipeline reads inputs and writes outputs. Set these once;
everything else resolves relative to them.

Parameters

`input_file`: primary study workbook

*string · **required***: The `.xlsx` workbook holding your study
metadata and candidate models. External YAML alias: `input`.

`out_root`: root output folder

*string · **required***: Root directory under which all products are
written. External YAML alias: `output_root`.

`cache_dir`: default cache folder

*string · **required***: Base folder for cached intermediate products.
External YAML alias: `cache_folder`.

`supplemental_dir`: supplemental trait files

*string*: Folder containing supplemental trait files referenced by
`data` sources. External YAML alias: `support_folder`.

`log_file`: log-file path

*string*: Where progress and error messages are written. Used only when
`execution.write_log = true`. External YAML alias: `log_path`.

### Runtime behavior (`execution`)

Global switches that change how the pipeline runs, independent of any
single modeling stage.

Parameters

`strict_length_pdf`: require finite length support

*logical*: When `true`, models with missing length-support bounds are
excluded from PDF construction rather than back-filled. External YAML
alias: `strict_pdf`.

`run_multiplier_model`: enable biomass-multiplier branch

*logical*: Enables the biomass-multiplier workflow branch. External YAML
alias: `run_multiplier`.

`write_log`: write a run log

*logical*: Writes progress and error messages to `paths.log_file`.

`progress`: progress messages

*logical*: Enables package progress messages globally. Individual
function arguments can still override this for one call.

### Caching (`cache`)

Controls where intermediate products are stored and when they are
recomputed. Each expensive stage caches its result; a global `refresh`
(or per-stage override) forces recomputation.

Parameters

`folder`: base cache folder

*string*: Base folder for all cache files.

`refresh`: global recompute switch

*logical*: When `true`, every cached product is recomputed. Individual
sections can override this with their own `refresh`.

`names`: per-product filename overrides

*named list*: Override the filename used for any cached product. Valid
keys:

| Key                                  | Product                         |
|--------------------------------------|---------------------------------|
| `worms`                              | WoRMS taxonomy pull             |
| `fishbase`                           | FishBase trait pull             |
| `pelagic` / `azores` / `continental` | Regional trait tables           |
| `mstraits`                           | Morphometric/soft-tissue traits |
| `species_enriched`                   | Enriched species table          |
| `candidate_models`                   | Prepared candidate pool         |
| `similarity_tuning`                  | Similarity-tuning results       |
| `anchor_admissibility`               | Anchor admissibility screen     |
| `policy_benchmark`                   | Policy benchmark                |
| `policy_conformal`                   | Conformal calibration           |
| `policy_selection`                   | Selected policies               |
| `policy_sensitivity`                 | Sensitivity simulation          |

`defaults_path`: custom cache-defaults JSON

*string*: Path to a custom cache-defaults JSON file, replacing the
packaged defaults.

## Candidate data

Trait sources and the candidate pool assembled from them.

### Trait data sources (`data`)

Declares the trait datasets fed into candidate enrichment. Each entry is
one dataset; at minimum you need a `study_metadata` entry pointing at
your study workbook. Built-in IDs are back-filled automatically.

Parameters

`id`: dataset identifier

*string · **required***: Case-insensitive dataset identifier. Built-in
IDs: `study_metadata`, `worms`, `fishbase`, `pelagic` / `pelagictraits`,
`azores` / `azorestraits`, `continental` / `continentaltraits`,
`mstraits`.

`alias`: alternate slug

*string*: Alternate slug usable in `candidates.enrich.precedence`.

`type`: source type descriptor

*string*: External type descriptor (e.g. `single_file`, `directory`).
Back-filled for built-in IDs.

`engine`: read engine

*string*: Engine descriptor (e.g. `r_package`, `rdata`). Back-filled for
built-in IDs.

`path`: file or folder path

*string*: File or folder path. Required for sources that read from disk.

`cache_path`: per-source cache override

*string*: Explicit cache-path override for this source.

`refresh`: per-source refresh

*logical*: Per-source refresh override.

### Candidate pool (`candidates`)

Orchestrates how the candidate pool is enriched, prepared, and anchored
on top of the declared `data` sources.

Parameters

`enrich`: trait enrichment

Merges trait sources into a single enriched species table.

`precedence`

*character vector*: Source priority order during enrichment. May use
normalized `id` or `alias`. Example:
`[study_metadata, fishbase, worms]`.

`missing_tokens`

*character vector*: Values treated as missing. Example:
`["-9999", "unknown"]`.

`cache_path`

*string*: Explicit cache override for the enriched species table.

`prepare`: candidate preparation

Normalizes the enriched table into the prepared candidate pool.

`missing_tokens`

*character vector*: Missing-value tokens applied during trait
preparation.

`cache_path`

*string*: Explicit cache override for the prepared candidate table.

`refresh`

*logical*: Per-stage refresh override.

`anchors`: reference-anchor selection

Settings passed to
[`set_reference_anchors()`](https://brandynlucca.github.io/tsbiomass/reference/set_reference_anchors.md).
Choose anchors either dynamically with `selector` or explicitly with
`model_ids`.

`selector`

*named list*: Dynamic filter over candidate columns. Each key is a
column and each value the required match. Common filter columns:

| Column | Selects by |
|----|----|
| `regional_body` | Managing body / survey program (e.g. `SWFSC`) |
| `ocean_basin` | Ocean basin |
| `fao` | FAO major area |
| `species` / `genus` / `family` | Taxonomic level |
| `season` | Survey season |

Example: `{regional_body: SWFSC}`.

`model_ids`

*character vector*: Explicit anchor model IDs (alternative to
`selector`).

`model_id_col`

*string*: Column name holding anchor IDs when using `model_ids`.

`require_selection`

*logical*: Whether zero matches should raise an error.

## Similarity distances

How donor-target relatedness is learned, weighted, tuned, and
visualized. See the [Super
Learners](https://brandynlucca.github.io/tsbiomass/articles/super-learners.md)
documentation for guidance on learner choices.

### Supervised distance learner (`alchemist`)

Settings for the `Alchemist` supervised distance learner, which turns
per-trait pairwise distances into the acoustic-distance matrix. Trait
names default to those in `similarity` when not specified here.

Parameters

`feature_type`: pairwise feature representation

*string*: How a donor-anchor pair is encoded as features. Options:
`"gower"` (unsigned Gower distances), `"difference"` (signed
standardized differences), `"mahalanobis"` (squared standardized
differences).

`taxonomic_distance`: collapse taxonomy to one distance

*logical*: Replace individual family/genus/species Gower features with a
single continuous phylogenetic distance (Open Tree of Life, with
rank-based fallback).

`species_traits`: species pair-level features

*character vector · default from `similarity`*: Species-level traits
used as pair-level features for the distance learner.

`study_traits`: study pair-level features

*character vector · default from `similarity`*: Study-level traits used
as pair-level features for the distance learner.

`distill_workers`: sensitivity worker count

*integer*: Worker count for sigma-dropout sensitivity in
[`distill_traits()`](https://brandynlucca.github.io/tsbiomass/reference/distill_traits.md).

`learner`: Super Learner library

Controls the stacked ensemble trained inside
[`forge_distances()`](https://brandynlucca.github.io/tsbiomass/reference/forge_distances.md).
See
[`list_learners()`](https://brandynlucca.github.io/tsbiomass/reference/list_learners.md)
for valid method names.

`methods`

*character vector*: Base learner methods in the library.

`inner_folds`

*integer*: Inner cross-validation folds for out-of-fold predictions.

`seed`

*integer*: Reproducibility seed.

`outcome_transform`

*string*: Transform applied to the acoustic-distance outcome before
fitting. Options: `"identity"`, `"log1p"`, `"sqrt"`.

`lambda_rule`

*string*: Penalized-model lambda selection. Options: `"min"`
(`lambda.min`), `"1se"` (`lambda.1se`).

`oof_mode`

*string*: Out-of-fold split strategy. `"anchor_species"` groups by
anchor species; `"species_purged"` excludes both anchor and donor
species from training.

`workers`

*integer*: Parallel worker count for multi-method fitting.

`method_settings`

*named list*: Per-method hyper-parameter overrides for the base learners
in `methods`. Each top-level key is a public learner method name; supply
only the fields you want to change. Parameter names and defaults are
documented per method in the Alchemist section of the [Super Learners
vignette](https://brandynlucca.github.io/tsbiomass/articles/super-learners.html#alchemist).

### Similarity matrix (`similarity`)

Trait selection, coherence, and kernel settings for the similarity
matrix. This section defines the baseline distance surface. Empirical
search ranges and resampling controls live in
[`tuning`](#similarity-tuning-tuning); the supervised Alchemist path is
described in [Alchemist distance
learning](https://brandynlucca.github.io/tsbiomass/articles/alchemist-distance-learning.md).

Parameters

`alpha`: species-vs-study blend

*number*: Blend between species and study distances. `1.0` = species
only, `0.0` = study only.

`kernel_scale`: kernel concentration

*number*: Global kernel scale. Higher values concentrate weight on
nearer donors.

`species_traits`: species traits in the distance

*character vector or named list*: Species-level traits included in
distance computation. The named-list form sets per-trait starting
weights, e.g. `{length: 1.0, depth: 0.5}`.

`study_traits`: study traits in the distance

*character vector or named list*: Study-level traits included in
distance computation. The named-list form sets per-trait starting
weights.

`conformal_alpha`: interval miscoverage target

*number*: Target miscoverage level for conformal interval calibration.

`core_weight_cutoff`: high-support donor threshold

*number*: Similarity-weight threshold for identifying high-support donor
subsets.

`cache_path`: cache override

*string*: Explicit cache-path override for the similarity matrix.

`refresh`: force recompute

*logical*: Force recompute of the similarity cache.

`coherence`: measurement-overlap features

Measurement-overlap features supplementing trait distances.

`length`

Length-overlap coherence feature.

- `mode` *(string)*: `overlap`, `literal`, or `none`.
- `weight` *(number)*: Feature weight.

`depth`

Depth-overlap coherence feature.

- `mode` *(string)*: `overlap`, `literal`, or `none`.
- `weight` *(number)*: Feature weight.

`frequency`

Acoustic-frequency-overlap coherence feature.

- `mode` *(string)*: `overlap`, `literal`, or `none`.
- `weight` *(number)*: Feature weight.
- `gap` *(number)*: Maximum frequency gap (kHz) treated as overlapping
  under `mode: overlap`.

### Similarity tuning (`tuning`)

Hyper-parameters for the empirical similarity-tuning search over trait
weights, coherence weights, `alpha`, and `kernel_scale`. These settings
apply to
[`tune_similarities()`](https://brandynlucca.github.io/tsbiomass/reference/tune_similarities.md)
and the empirical Candidates path described in [Empirical similarity
tuning](https://brandynlucca.github.io/tsbiomass/articles/empirical-similarity-tuning.md).

Parameters

`max_models_per_species`: models per species in subsets

*integer*: Maximum retained models per species in tuning subsets.
External YAML alias: `species_model_limit`.

`n_resamples`: empirical resamples

*integer*: Number of empirical tuning resamples. External YAML alias:
`resamples`.

`n_cores`: worker count

*integer*: Worker count for tuning runs.

`seed`: reproducibility seed

*integer*: Reproducibility seed.

`alpha_range`: alpha search bounds

*`{from, to}`*: Empirical tuning bounds for the species-versus-study
blend.

`kernel_scale_range`: kernel-scale search bounds

*`{from, to}`*: Empirical tuning bounds for the global kernel scale.

`coherence`: coherence-weight search bounds

*named list*: Optional empirical tuning bounds for coherence weights.
Supported entries are `length.range`, `depth.range`, and
`frequency.range`, each as `{from, to}`.

`grid_refinement_levels`: local refinement passes

*integer*: Local search-refinement passes after the coarse grid.

`response_surface_top_n`: retained candidate points

*integer*: Candidate points kept during response-surface refinement.

`rmse_tolerance`: convergence tolerance

*number*: RMSE improvement below which refinement is considered
converged.

`support_strata_bins`: support strata

*integer*: Number of support strata used when balancing tuning
resamples.

`regularization`: search regularization

*named list*: Regularization strengths that keep the tuned parameters
near their priors. Fields: `alpha`, `kernel_scale`, `coherence_scale`,
`stability`.

`equal_start_weights`: equal initial trait weights

*logical*: Initialize all trait weights equally rather than using
configured magnitudes as priors.

### Ordination (`ordination`)

NMDS ordination settings, applied after
[`forge_distances()`](https://brandynlucca.github.io/tsbiomass/reference/forge_distances.md)
to visualize proxy model diversity.

Parameters

`include_loadings`: envfit vector loadings

*logical*: Include envfit vector loadings in the ordination output.

`include_centroids`: factor-level centroids

*logical*: Include factor-level centroids.

`nmds_args`: metaMDS arguments

*named list*: Arguments forwarded to
[`vegan::metaMDS()`](https://vegandevs.github.io/vegan/reference/metaMDS.html).

`envfit_args`: envfit arguments

*named list*: Arguments forwarded to
[`vegan::envfit()`](https://vegandevs.github.io/vegan/reference/envfit.html).

## Policies & admissibility

Which transfer paths are admissible, which policies are declared, and
how they are benchmarked.

### Admissibility gates (`admissibility`)

Hard binary gates that exclude transfer paths before any policy is
applied. Donors that fail a check are removed from the candidate pool
for the affected target.

Parameters

`species_traits`: species exact-match gates

*character vector*: Categorical or binary species traits that must match
exactly between donor and target.

`study_traits`: study exact-match gates

*character vector*: Study-level traits used as exact-match gates.

`key_metadata_max`: missingness tolerance

*number*: Maximum tolerated missing fraction across key study-level
metadata fields.

`cache_path`: cache override

*string*: Explicit cache-path override for the admissibility screen.

`refresh`: force recompute

*logical*: Force recompute of the admissibility cache.

`coherence`: measurement-overlap gates

Minimum measurement-overlap requirements applied as gates.

`length`

Length-overlap gate.

- `mode` *(string)*: `overlap`, `literal`, or `none`.
- `min` *(number)*: Minimum required fractional length overlap.

`depth`

Depth-overlap gate.

- `mode` *(string)*: `overlap`, `literal`, or `none`.
- `min` *(number)*: Minimum required fractional depth overlap.

`frequency`

Frequency-overlap gate.

- `mode` *(string)*: `none`, `literal`, or `overlap`.
- `gap` *(number)*: Gap threshold (kHz) under `mode: overlap`.

### Transfer policies (`policies`)

Declares the transfer policies to benchmark. Use the **constructor
form** (`metric` + `slope_class` + `group`) to generate named policy
sets from the registry, or the **explicit form** (`active`) to list
policy names directly.

Parameters

`metric`: donor-aggregation metric filter

*character vector*: Global metric filter. Supported: `closest`,
`weighted_mean`, `unweighted_mean`, `survey_distance`, `taxon_distance`,
`species_distance`, `random`.

`slope_class`: slope-class filter

*character vector*: Slope classes. Supported: `all`, `fixed_slope`,
`free_slope`.

`group`: per-grouping overrides

*named list*: One entry per donor-pool grouping (e.g. `species`,
`genus`, `family`). Each value configures per-group overrides:

`group.<name>.include_base`

*logical*: When `joint` variants are listed, keep the plain root group
as well.

`group.<name>.metric`

*character vector*: Per-group metric override.

`group.<name>.joint`

*list*: One or more trait names to conjoin with the root group key. Each
entry becomes a separate `<group>_<trait>` policy variant.

`active`: explicit policy list

*character vector*: Explicit policy name strings (explicit form).
Example: `[species_closest_all, genus_weighted_mean_all]`.

### Policy benchmarking (`benchmark`)

Settings for the policy-benchmarking stage, which scores each policy
against the reference anchors.

Parameters

`workers`: parallel anchor evaluation

*integer*: Worker count for parallel anchor evaluation.

`engine`: evaluation backend

*string*: `"cpp"` uses the compiled C++ backend; `"r"` uses the pure-R
path.

`include_ts_error`: TS reconstruction error

*logical*: Include TS-curve reconstruction error in benchmark outputs.

`cache_path`: cache override

*string*: Explicit cache-path override for the benchmark.

`refresh`: force recompute

*logical*: Force recompute of the benchmark cache.

## Uncertainty & policy selection

The two post-benchmark Super Learners: one calibrates
prediction-interval widths, the other selects policies and powers the
meta-learner. See the [Super Learners
vignette](https://brandynlucca.github.io/tsbiomass/articles/super-learners.md).

### Conditional uncertainty (`uncertainty`)

Trains a cross-fitted model to predict expected absolute log-error, used
to calibrate prediction-interval widths.

Parameters

`method`: learner method

*string*: `"glm"` for a single generalized linear model, or
`"super_learner"` to enable stacked-ensemble mode.

`super_methods`: ensemble library

*character vector*: Base learner library used when
`method = "super_learner"`.

`n_folds`: outer folds

*integer*: Outer cross-fitting folds.

`inner_folds`: inner folds

*integer*: Inner folds for penalized learners.

`workers`: cross-fitting workers

*integer*: Worker count for cross-fitting.

`outcome_col`: regression target

*string*: Benchmark column used as the regression target.

`outcome_clip_quantile`: extreme-outcome clipping

*number*: Upper quantile used to clip extreme training outcomes.

`outcome_transform`: outcome transform

*string*: Transform applied to the outcome before fitting.

`lambda_rule`: lambda selection rule

*string*: Lambda selection rule for penalized learners.

`loss`: combiner loss

*string*: Super-learner combiner loss. Must be `"squared_error"` with
NNLS.

`method_settings`: per-method overrides

*named list*: Per-method hyper-parameter overrides for the base learners
in `super_methods`. Each top-level key is a public learner method name;
supply only the fields you want to change. Parameter names and defaults
are documented per method in the Uncertainty section of the [Super
Learners
vignette](https://brandynlucca.github.io/tsbiomass/articles/super-learners.html#learner-2-uncertainty-uncertainty).

`cache_path`: cache override

*string*: Explicit cache-path override for the uncertainty learner.

`refresh`: force recompute

*logical*: Force recompute of the uncertainty cache.

### Policy selection & meta-learner (`selection`)

Post-benchmarking policy selection, plus the optional meta-learner used
by `PolicyLearner`. The policy-selection keys are listed first; the
meta-learner keys mirror `uncertainty`.

Parameters

`one_se_multiplier`: one-SE screen width

*number*: Multiplier on the best policy’s standard error for the one-SE
screen.

`equivalence_tolerance`: practical-equivalence band

*number*: Practical tolerance for pairwise equivalence summaries.

`n_boot`: bootstrap replicates

*integer*: Bootstrap replicates for selection uncertainty.

`seed`: selection seed

*integer*: Reproducibility seed for selection.

`uncertainty_rule`: final width filter

*string*: `"min"` keeps only the narrowest interval; `"tolerance"` keeps
widths within the configured absolute/relative band; `"one_se"` uses the
one-SE screen.

`u_tol_rel`: relative width tolerance

*number*: Relative width tolerance (fraction of minimum width) under
`uncertainty_rule = tolerance`.

`u_tol_abs`: absolute width tolerance

*number*: Absolute log-width tolerance under
`uncertainty_rule = tolerance`.

`conformal_alpha`: post-selection miscoverage

*number*: Miscoverage level for post-selection conformal calibration.

`method`: meta-learner method

*string*: Meta-learner method used by `PolicyLearner`. `"glm"` or
`"super_learner"` for ensemble mode.

`super_methods`: meta-learner library

*character vector*: Base learner library when
`method = "super_learner"`.

`n_folds`: outer folds

*integer*: Outer cross-fitting folds for the meta-learner.

`inner_folds`: inner folds

*integer*: Inner folds for penalized meta-learners.

`workers`: cross-fitting workers

*integer*: Cross-fitting worker count for the meta-learner.

`outcome_col`: learner target

*string*: Benchmark column used as the meta-learner target.

`outcome_transform`: outcome transform

*string*: Transform applied to the meta-learner outcome before fitting.

`lambda_rule`: lambda selection rule

*string*: Lambda selection rule for penalized meta-learners.

`loss`: combiner loss

*string*: Meta-learner combiner loss. Must be `"squared_error"` with
NNLS.

`method_settings`: per-method overrides

*named list*: Per-method hyper-parameter overrides for the meta-learner
base learners. Each top-level key is a public learner method name;
supply only the fields you want to change. Parameter names and defaults
are documented per method in the Selection section of the [Super
Learners
vignette](https://brandynlucca.github.io/tsbiomass/articles/super-learners.html#learner-3-selection-policylearner-selection).

`cache_path`: cache override

*string*: Explicit cache-path override for selection.

`refresh`: force recompute

*logical*: Force recompute of the selection cache.

## Validation & sensitivity

Outer-loop holdout validation and scenario-based sensitivity analysis.

### Outer-loop validation (`sentinel`)

Outer-loop holdout validation and ablation settings for
[`build_sentinel()`](https://brandynlucca.github.io/tsbiomass/reference/build_sentinel.md)
/
[`run_sentinel()`](https://brandynlucca.github.io/tsbiomass/reference/run_sentinel.md).

Parameters

`baseline_species_folds`: headline validation folds

*integer*: Species-disjoint outer folds for the headline validation run.
Set to the total species count for full leave-one-species-out.

`ablation_species_folds`: folds per ablation

*integer*: Folds used per ablation scenario.

`workers`: outer worker count

*integer*: Outer workflow worker count.

`batch_size`: checkpoint batch size

*integer*: Workflows checkpointed per batch.

`include_ts_error`: TS error in fold benchmarks

*logical*: Include TS-curve error in fold-local benchmarks.

`throttle_inner_workers`: prevent oversubscription

*logical*: Prevent inner workers from oversubscribing cores allocated to
outer workers.

`fast_validation`: reduced NMDS schedule

*logical*: Reduce the NMDS schedule for faster runs.

`logging`: file logging

*logical*: Enable fold-level file logging.

`save_case_artifacts`: persist fold artifacts

*logical*: Persist fold-level RDS artifacts for debugging.

### Sensitivity simulation (`simulation`)

Settings for `PolicySimulator` sensitivity runs, which rerun
benchmarking across perturbed scenarios.

Parameters

`workers`: worker count

*integer*: Worker count for scenario reruns.

`cache_path`: cache override

*string*: Explicit cache-path override for the sensitivity simulation.

`refresh`: force recompute

*logical*: Force recompute of the sensitivity cache.

## Default configuration

The complete default configuration produced by
[`create_configuration_template()`](https://brandynlucca.github.io/tsbiomass/reference/create_configuration_template.md),
shown in YAML and R-list form. Every value below is the packaged
default; override only the keys you need.

💻 Default configuration

YAML R list

``` yaml
paths:
  input_file: input.xlsx
  out_root: outputs
  cache_dir: cache
  supplemental_dir: supplemental
  log_file: outputs/tsbiomass_run.log
execution:
  strict_length_pdf: false
  run_multiplier_model: false
  write_log: false
  progress: false
tuning:
  max_models_per_species: 2
  n_resamples: 8
  n_cores: 1
  seed: ~
  alpha_range:
    from: 0.1
    to: 0.9
  kernel_scale_range:
    from: 1.0
    to: 8.0
  coherence:
    length:
      range:
        from: 0.5
        to: 6.0
    depth:
      range:
        from: 0.5
        to: 6.0
    frequency:
      range:
        from: 0.5
        to: 6.0
  grid_refinement_levels: 1
  response_surface_top_n: 20
  rmse_tolerance: 0.01
  support_strata_bins: 4
  regularization:
    alpha: 0.05
    kernel_scale: 0.05
    coherence_scale: 0.05
    stability: 0.02
  equal_start_weights: false
similarity:
  alpha: 0.8
  kernel_scale: 4.0
  species_traits:
    class: 1.0
  study_traits:
    fao_area: 1.0
  coherence:
    length:
      mode: overlap
      weight: 2.0
    depth:
      mode: overlap
      weight: 3.0
    frequency:
      mode: overlap
      weight: 2.0
      gap: 60.0
  conformal_alpha: 0.1
ordination:
  include_loadings: false
  include_centroids: false
policies:
  group:
  - species
  metric:
  - closest
  slope_class:
  - all
cache:
  folder: cache
  refresh: false
  names:
    worms: worms_species_traits.rds
    fishbase: fishbase_species_traits.rds
    pelagic: pelagic_species_traits.rds
    azores: azores_species_traits.rds
    continental: continental_species_traits.rds
    mstraits: mstraits_species_traits.rds
    species_enriched: species_traits_enriched.rds
    candidate_models: candidate_models_prepared.rds
    similarity_tuning: similarity_tuning.rds
    anchor_admissibility: anchor_admissibility.rds
    policy_benchmark: policy_benchmark.rds
    policy_conformal: policy_conformal.rds
    policy_selection: policy_selection.rds
    policy_sensitivity: policy_sensitivity.rds
benchmark:
  workers: 1
  engine: cpp
  include_ts_error: false
admissibility:
  species_traits: []
  study_traits: []
  coherence:
    length:
      mode: overlap
      min: 0.25
    depth:
      mode: overlap
      min: 0.25
    frequency:
      mode: none
      gap: 60.0
  key_metadata_max: 0.25
uncertainty:
  method: glm
  super_methods: ~
  method_settings:
    glm: []
    glm_ridge:
      standardize: true
      type_measure: mae
      alpha: 0
    glm_lasso:
      standardize: true
      type_measure: mae
      alpha: 1
    glm_elastic:
      standardize: true
      type_measure: mae
      alpha: 0.25
    qreg:
      tau: 0.5
      fit_method: fn
    gam:
      fit_method: REML
      select_terms: true
    lmm:
      fit_method: REML
      random_intercept: .split_group
    rpart:
      cp: 0.01
      minsplit: 20
      minbucket: 7
      maxdepth: 30
    rf:
      num_trees: 500
      mtry: ~
      min_node_size: 5
      max_depth: ~
      sample_fraction: 1
      replace: true
      respect_unordered_factors: order
    xgboost:
      nrounds: 100
      eta: 0.3
      max_depth: 6
      min_child_weight: 1
      subsample: 1
      colsample_bytree: 1
      lambda: 1
      alpha: 0
      nthread: 1
    mars:
      degree: 2
      penalty: 3
      nprune: ~
      pmethod: backward
    bart:
      num_trees: 75
      alpha: 0.95
      beta: 2
      min_samples_leaf: 5
      max_depth: 10
      keep_gfr: true
      variance_forest_num_trees: 50
      random_effects_group: .split_group
      num_gfr: 0
      num_burnin: 100
      num_mcmc: 200
      variance_forest: false
      random_effects: false
    xbart:
      num_trees: 75
      alpha: 0.95
      beta: 2
      min_samples_leaf: 5
      max_depth: 10
      keep_gfr: true
      variance_forest_num_trees: 50
      random_effects_group: .split_group
      num_gfr: 40
      num_burnin: 0
      num_mcmc: 0
      variance_forest: false
      random_effects: false
    wsbart:
      num_trees: 75
      alpha: 0.95
      beta: 2
      min_samples_leaf: 5
      max_depth: 10
      keep_gfr: true
      variance_forest_num_trees: 50
      random_effects_group: .split_group
      num_gfr: 20
      num_burnin: 0
      num_mcmc: 200
      variance_forest: false
      random_effects: false
    vfbart:
      num_trees: 75
      alpha: 0.95
      beta: 2
      min_samples_leaf: 5
      max_depth: 10
      keep_gfr: true
      variance_forest_num_trees: 50
      random_effects_group: .split_group
      num_gfr: 0
      num_burnin: 100
      num_mcmc: 200
      variance_forest: true
      random_effects: false
    rebart:
      num_trees: 75
      alpha: 0.95
      beta: 2
      min_samples_leaf: 5
      max_depth: 10
      keep_gfr: true
      variance_forest_num_trees: 50
      random_effects_group: .split_group
      num_gfr: 0
      num_burnin: 100
      num_mcmc: 200
      variance_forest: false
      random_effects: true
    knn:
      k: 10
    cubist:
      committees: 1
      neighbors: 0
    svr:
      C: 1
      epsilon: 0.1
    qrf:
      num_trees: 500
      mtry: ~
      min_node_size: 10
      max_depth: ~
      sample_fraction: 1
      replace: true
      quantile: 0.9
    gpr:
      var: 0.001
    mean: []
  n_folds: 5
  inner_folds: 5
  workers: 1
  outcome_col: error_abs_log
  outcome_clip_quantile: 0.99
  outcome_transform: log1p
  lambda_rule: lambda.1se
  loss: squared_error
selection:
  one_se_multiplier: 1.0
  equivalence_tolerance: 0.05
  n_boot: 500
  seed: ~
  uncertainty_rule: tolerance
  u_tol_rel: 0.25
  u_tol_abs: 0.05
  uncertainty_relative_tolerance: 0.25
  uncertainty_absolute_tolerance: 0.05
  local_distance_tolerance: 1.0e-12
  conformal_alpha: 0.1
  bin_alpha: 0.1
  min_bin_scores: 10
  n_bins: 3
  use_support_bin_intervals: false
  support_bin_labels:
  - Lower support
  - Moderate support
  - Higher support
  method: glm
  super_methods: ~
  method_settings:
    glm: []
    glm_ridge:
      standardize: true
      type_measure: mae
      alpha: 0
    glm_lasso:
      standardize: true
      type_measure: mae
      alpha: 1
    glm_elastic:
      standardize: true
      type_measure: mae
      alpha: 0.25
    qreg:
      tau: 0.5
      fit_method: fn
    gam:
      fit_method: REML
      select_terms: true
    lmm:
      fit_method: REML
      random_intercept: .split_group
    rpart:
      cp: 0.01
      minsplit: 20
      minbucket: 7
      maxdepth: 30
    rf:
      num_trees: 500
      mtry: ~
      min_node_size: 5
      max_depth: ~
      sample_fraction: 1
      replace: true
      respect_unordered_factors: order
    xgboost:
      nrounds: 100
      eta: 0.3
      max_depth: 6
      min_child_weight: 1
      subsample: 1
      colsample_bytree: 1
      lambda: 1
      alpha: 0
      nthread: 1
    mars:
      degree: 2
      penalty: 3
      nprune: ~
      pmethod: backward
    bart:
      num_trees: 75
      alpha: 0.95
      beta: 2
      min_samples_leaf: 5
      max_depth: 10
      keep_gfr: true
      variance_forest_num_trees: 50
      random_effects_group: .split_group
      num_gfr: 0
      num_burnin: 100
      num_mcmc: 200
      variance_forest: false
      random_effects: false
    xbart:
      num_trees: 75
      alpha: 0.95
      beta: 2
      min_samples_leaf: 5
      max_depth: 10
      keep_gfr: true
      variance_forest_num_trees: 50
      random_effects_group: .split_group
      num_gfr: 40
      num_burnin: 0
      num_mcmc: 0
      variance_forest: false
      random_effects: false
    wsbart:
      num_trees: 75
      alpha: 0.95
      beta: 2
      min_samples_leaf: 5
      max_depth: 10
      keep_gfr: true
      variance_forest_num_trees: 50
      random_effects_group: .split_group
      num_gfr: 20
      num_burnin: 0
      num_mcmc: 200
      variance_forest: false
      random_effects: false
    vfbart:
      num_trees: 75
      alpha: 0.95
      beta: 2
      min_samples_leaf: 5
      max_depth: 10
      keep_gfr: true
      variance_forest_num_trees: 50
      random_effects_group: .split_group
      num_gfr: 0
      num_burnin: 100
      num_mcmc: 200
      variance_forest: true
      random_effects: false
    rebart:
      num_trees: 75
      alpha: 0.95
      beta: 2
      min_samples_leaf: 5
      max_depth: 10
      keep_gfr: true
      variance_forest_num_trees: 50
      random_effects_group: .split_group
      num_gfr: 0
      num_burnin: 100
      num_mcmc: 200
      variance_forest: false
      random_effects: true
    knn:
      k: 10
    cubist:
      committees: 1
      neighbors: 0
    svr:
      C: 1
      epsilon: 0.1
    qrf:
      num_trees: 500
      mtry: ~
      min_node_size: 10
      max_depth: ~
      sample_fraction: 1
      replace: true
      quantile: 0.9
    gpr:
      var: 0.001
    mean: []
  n_folds: 5
  inner_folds: 5
  workers: 1
  outcome_col: error_abs_log
  outcome_clip_quantile: 0.99
  outcome_transform: log1p
  lambda_rule: lambda.1se
  loss: squared_error
  max_selection_tolerance: 1.0e-12
simulation:
  workers: 1
```

``` r
list(
  paths = list(
    input_file = "input.xlsx",
    out_root = "outputs",
    cache_dir = "cache",
    supplemental_dir = "supplemental",
    log_file = "outputs/tsbiomass_run.log"
  ),
  execution = list(
    strict_length_pdf = FALSE,
    run_multiplier_model = FALSE,
    write_log = FALSE,
    progress = FALSE
  ),
  tuning = list(
    max_models_per_species = 2,
    n_resamples = 8,
    n_cores = 1,
    seed = NULL,
    alpha_range = list(
      from = 0.1,
      to = 0.9
    ),
    kernel_scale_range = list(
      from = 1,
      to = 8
    ),
    coherence = list(
      length = list(range = list(from = 0.5, to = 6)),
      depth = list(range = list(from = 0.5, to = 6)),
      frequency = list(range = list(from = 0.5, to = 6))
    ),
    grid_refinement_levels = 1,
    response_surface_top_n = 20,
    rmse_tolerance = 0.01,
    support_strata_bins = 4,
    regularization = list(
      alpha = 0.05,
      kernel_scale = 0.05,
      coherence_scale = 0.05,
      stability = 0.02
    ),
    equal_start_weights = FALSE
  ),
  similarity = list(
    alpha = 0.8,
    kernel_scale = 4,
    species_traits = list(
      class = 1
    ),
    study_traits = list(
      fao_area = 1
    ),
    coherence = list(
      length = list(
        mode = "overlap",
        weight = 2
      ),
      depth = list(
        mode = "overlap",
        weight = 3
      ),
      frequency = list(
        mode = "overlap",
        weight = 2,
        gap = 60
      )
    ),
    conformal_alpha = 0.1
  ),
  ordination = list(
    include_loadings = FALSE,
    include_centroids = FALSE
  ),
  policies = list(
    group = list("species"),
    metric = list("closest"),
    slope_class = list("all")
  ),
  cache = list(
    folder = "cache",
    refresh = FALSE,
    names = list(
      worms = "worms_species_traits.rds",
      fishbase = "fishbase_species_traits.rds",
      pelagic = "pelagic_species_traits.rds",
      azores = "azores_species_traits.rds",
      continental = "continental_species_traits.rds",
      mstraits = "mstraits_species_traits.rds",
      species_enriched = "species_traits_enriched.rds",
      candidate_models = "candidate_models_prepared.rds",
      similarity_tuning = "similarity_tuning.rds",
      anchor_admissibility = "anchor_admissibility.rds",
      policy_benchmark = "policy_benchmark.rds",
      policy_conformal = "policy_conformal.rds",
      policy_selection = "policy_selection.rds",
      policy_sensitivity = "policy_sensitivity.rds"
    )
  ),
  benchmark = list(
    workers = 1,
    engine = "cpp",
    include_ts_error = FALSE
  ),
  admissibility = list(
    species_traits = character(0),
    study_traits = character(0),
    coherence = list(
      length = list(
        mode = "overlap",
        min = 0.25
      ),
      depth = list(
        mode = "overlap",
        min = 0.25
      ),
      frequency = list(
        mode = "none",
        gap = 60
      )
    ),
    key_metadata_max = 0.25
  ),
  uncertainty = list(
    method = "glm",
    super_methods = NULL,
    method_settings = list(
      glm = list(),
      glm_ridge = list(
        standardize = TRUE,
        type_measure = "mae",
        alpha = 0
      ),
      glm_lasso = list(
        standardize = TRUE,
        type_measure = "mae",
        alpha = 1
      ),
      glm_elastic = list(
        standardize = TRUE,
        type_measure = "mae",
        alpha = 0.25
      ),
      qreg = list(
        tau = 0.5,
        fit_method = "fn"
      ),
      gam = list(
        fit_method = "REML",
        select_terms = TRUE
      ),
      lmm = list(
        fit_method = "REML",
        random_intercept = ".split_group"
      ),
      rpart = list(
        cp = 0.01,
        minsplit = 20,
        minbucket = 7,
        maxdepth = 30
      ),
      rf = list(
        num_trees = 500,
        mtry = NULL,
        min_node_size = 5,
        max_depth = NULL,
        sample_fraction = 1,
        replace = TRUE,
        respect_unordered_factors = "order"
      ),
      xgboost = list(
        nrounds = 100,
        eta = 0.3,
        max_depth = 6,
        min_child_weight = 1,
        subsample = 1,
        colsample_bytree = 1,
        lambda = 1,
        alpha = 0,
        nthread = 1
      ),
      mars = list(
        degree = 2,
        penalty = 3,
        nprune = NULL,
        pmethod = "backward"
      ),
      bart = list(
        num_trees = 75,
        alpha = 0.95,
        beta = 2,
        min_samples_leaf = 5,
        max_depth = 10,
        keep_gfr = TRUE,
        variance_forest_num_trees = 50,
        random_effects_group = ".split_group",
        num_gfr = 0,
        num_burnin = 100,
        num_mcmc = 200,
        variance_forest = FALSE,
        random_effects = FALSE
      ),
      xbart = list(
        num_trees = 75,
        alpha = 0.95,
        beta = 2,
        min_samples_leaf = 5,
        max_depth = 10,
        keep_gfr = TRUE,
        variance_forest_num_trees = 50,
        random_effects_group = ".split_group",
        num_gfr = 40,
        num_burnin = 0,
        num_mcmc = 0,
        variance_forest = FALSE,
        random_effects = FALSE
      ),
      wsbart = list(
        num_trees = 75,
        alpha = 0.95,
        beta = 2,
        min_samples_leaf = 5,
        max_depth = 10,
        keep_gfr = TRUE,
        variance_forest_num_trees = 50,
        random_effects_group = ".split_group",
        num_gfr = 20,
        num_burnin = 0,
        num_mcmc = 200,
        variance_forest = FALSE,
        random_effects = FALSE
      ),
      vfbart = list(
        num_trees = 75,
        alpha = 0.95,
        beta = 2,
        min_samples_leaf = 5,
        max_depth = 10,
        keep_gfr = TRUE,
        variance_forest_num_trees = 50,
        random_effects_group = ".split_group",
        num_gfr = 0,
        num_burnin = 100,
        num_mcmc = 200,
        variance_forest = TRUE,
        random_effects = FALSE
      ),
      rebart = list(
        num_trees = 75,
        alpha = 0.95,
        beta = 2,
        min_samples_leaf = 5,
        max_depth = 10,
        keep_gfr = TRUE,
        variance_forest_num_trees = 50,
        random_effects_group = ".split_group",
        num_gfr = 0,
        num_burnin = 100,
        num_mcmc = 200,
        variance_forest = FALSE,
        random_effects = TRUE
      ),
      knn = list(
        k = 10
      ),
      cubist = list(
        committees = 1,
        neighbors = 0
      ),
      svr = list(
        C = 1,
        epsilon = 0.1
      ),
      qrf = list(
        num_trees = 500,
        mtry = NULL,
        min_node_size = 10,
        max_depth = NULL,
        sample_fraction = 1,
        replace = TRUE,
        quantile = 0.9
      ),
      gpr = list(
        var = 0.001
      ),
      mean = list()
    ),
    n_folds = 5,
    inner_folds = 5,
    workers = 1,
    outcome_col = "error_abs_log",
    outcome_clip_quantile = 0.99,
    outcome_transform = "log1p",
    lambda_rule = "lambda.1se",
    loss = "squared_error"
  ),
  selection = list(
    one_se_multiplier = 1,
    equivalence_tolerance = 0.05,
    n_boot = 500,
    seed = NULL,
    uncertainty_rule = "tolerance",
    u_tol_rel = 0.25,
    u_tol_abs = 0.05,
    uncertainty_relative_tolerance = 0.25,
    uncertainty_absolute_tolerance = 0.05,
    local_distance_tolerance = 1e-12,
    conformal_alpha = 0.1,
    bin_alpha = 0.1,
    min_bin_scores = 10,
    n_bins = 3,
    use_support_bin_intervals = FALSE,
    support_bin_labels = c("Lower support", "Moderate support", "Higher support"),
    method = "glm",
    super_methods = NULL,
    method_settings = list(
      glm = list(),
      glm_ridge = list(
        standardize = TRUE,
        type_measure = "mae",
        alpha = 0
      ),
      glm_lasso = list(
        standardize = TRUE,
        type_measure = "mae",
        alpha = 1
      ),
      glm_elastic = list(
        standardize = TRUE,
        type_measure = "mae",
        alpha = 0.25
      ),
      qreg = list(
        tau = 0.5,
        fit_method = "fn"
      ),
      gam = list(
        fit_method = "REML",
        select_terms = TRUE
      ),
      lmm = list(
        fit_method = "REML",
        random_intercept = ".split_group"
      ),
      rpart = list(
        cp = 0.01,
        minsplit = 20,
        minbucket = 7,
        maxdepth = 30
      ),
      rf = list(
        num_trees = 500,
        mtry = NULL,
        min_node_size = 5,
        max_depth = NULL,
        sample_fraction = 1,
        replace = TRUE,
        respect_unordered_factors = "order"
      ),
      xgboost = list(
        nrounds = 100,
        eta = 0.3,
        max_depth = 6,
        min_child_weight = 1,
        subsample = 1,
        colsample_bytree = 1,
        lambda = 1,
        alpha = 0,
        nthread = 1
      ),
      mars = list(
        degree = 2,
        penalty = 3,
        nprune = NULL,
        pmethod = "backward"
      ),
      bart = list(
        num_trees = 75,
        alpha = 0.95,
        beta = 2,
        min_samples_leaf = 5,
        max_depth = 10,
        keep_gfr = TRUE,
        variance_forest_num_trees = 50,
        random_effects_group = ".split_group",
        num_gfr = 0,
        num_burnin = 100,
        num_mcmc = 200,
        variance_forest = FALSE,
        random_effects = FALSE
      ),
      xbart = list(
        num_trees = 75,
        alpha = 0.95,
        beta = 2,
        min_samples_leaf = 5,
        max_depth = 10,
        keep_gfr = TRUE,
        variance_forest_num_trees = 50,
        random_effects_group = ".split_group",
        num_gfr = 40,
        num_burnin = 0,
        num_mcmc = 0,
        variance_forest = FALSE,
        random_effects = FALSE
      ),
      wsbart = list(
        num_trees = 75,
        alpha = 0.95,
        beta = 2,
        min_samples_leaf = 5,
        max_depth = 10,
        keep_gfr = TRUE,
        variance_forest_num_trees = 50,
        random_effects_group = ".split_group",
        num_gfr = 20,
        num_burnin = 0,
        num_mcmc = 200,
        variance_forest = FALSE,
        random_effects = FALSE
      ),
      vfbart = list(
        num_trees = 75,
        alpha = 0.95,
        beta = 2,
        min_samples_leaf = 5,
        max_depth = 10,
        keep_gfr = TRUE,
        variance_forest_num_trees = 50,
        random_effects_group = ".split_group",
        num_gfr = 0,
        num_burnin = 100,
        num_mcmc = 200,
        variance_forest = TRUE,
        random_effects = FALSE
      ),
      rebart = list(
        num_trees = 75,
        alpha = 0.95,
        beta = 2,
        min_samples_leaf = 5,
        max_depth = 10,
        keep_gfr = TRUE,
        variance_forest_num_trees = 50,
        random_effects_group = ".split_group",
        num_gfr = 0,
        num_burnin = 100,
        num_mcmc = 200,
        variance_forest = FALSE,
        random_effects = TRUE
      ),
      knn = list(
        k = 10
      ),
      cubist = list(
        committees = 1,
        neighbors = 0
      ),
      svr = list(
        C = 1,
        epsilon = 0.1
      ),
      qrf = list(
        num_trees = 500,
        mtry = NULL,
        min_node_size = 10,
        max_depth = NULL,
        sample_fraction = 1,
        replace = TRUE,
        quantile = 0.9
      ),
      gpr = list(
        var = 0.001
      ),
      mean = list()
    ),
    n_folds = 5,
    inner_folds = 5,
    workers = 1,
    outcome_col = "error_abs_log",
    outcome_clip_quantile = 0.99,
    outcome_transform = "log1p",
    lambda_rule = "lambda.1se",
    loss = "squared_error",
    max_selection_tolerance = 1e-12,
    refresh = FALSE
  ),
  simulation = list(
    workers = 1
  )
)
```
