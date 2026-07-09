# Build a `Sentinel`

Build a `Sentinel`

## Usage

``` r
build_sentinel(
  data,
  workflow_fn = NULL,
  config = NULL,
  deployment_target = NULL,
  split_mode = NULL,
  split_col = NULL,
  scenario_grid = NULL,
  output_dir = NULL,
  case_studies = character(0),
  options = NULL,
  trait_ablations = NULL,
  gate_ablations = NULL,
  cache_dir = NULL,
  logging = NULL
)
```

## Arguments

- data:

  Candidate-model table,
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md),
  or
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md).

- workflow_fn:

  User-supplied workflow function that owns the complete fold-local
  analysis. Its public signature is
  `function(candidates, workflow_config_s7)`: `candidates` contains only
  outer-training and held-out
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  rows; `workflow_config_s7` is the scenario-specific
  [Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md).
  The function may directly return a
  [Referee](https://brandynlucca.github.io/tsbiomass/reference/Referee-class.md),
  [Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md),
  or a result list containing `metrics` or `selected`. It may be
  deferred and supplied to
  [`run_sentinel()`](https://brandynlucca.github.io/tsbiomass/reference/run_sentinel.md)
  instead.

- config:

  Optional config list or package object carrying config.

- deployment_target:

  Optional deployment target. One of `"seen_species_new_row"`,
  `"seen_species_new_study"`, `"seen_species_new_study_cell"`, or
  `"cold_start_species"`. When supplied, Sentinel derives the default
  `split_mode` from this target.

- split_mode:

  Split mode. One of `"anchor_row_holdout"`, `"study_holdout"`,
  `"study_cell_holdout"`, `"species_holdout"`, or `"group_holdout"`.

- split_col:

  Optional explicit split column. Required for `"group_holdout"` and
  optional otherwise.

- scenario_grid:

  Optional named list of scenario specifications.

- output_dir:

  Optional output directory.

- case_studies:

  Optional character vector of holdout IDs whose artifacts should be
  persisted.

- options:

  Optional options list.

- trait_ablations:

  Optional character vector for automatic leave-one- trait-out
  scenarios, or a named list for grouped trait ablations. Set to `TRUE`
  to ablate every configured species and study trait present in `data`.
  Automatic ablations operate on effective similarity features: when
  `alchemist.taxonomic_distance` is enabled, configured taxonomic ranks
  are grouped into one `without_taxonomic_distance` scenario. Cannot be
  combined with `scenario_grid`.

- gate_ablations:

  Optional character vector, or `TRUE` to relax every configured
  admissibility gate trait present in `data`. Each becomes a
  `relax_gate_<trait>` scenario that removes the trait's hard
  admissibility gate (a distinct estimand from `trait_ablations`, which
  prunes similarity features). May be combined with `trait_ablations`
  but not `scenario_grid`.

- cache_dir:

  Optional Sentinel cache parent. An explicit value takes precedence
  over `sentinel.cache_dir` and `paths.cache_dir` in the config.

- logging:

  Optional logical scalar controlling per-fold log files.

## Value

A `Sentinel` object.
