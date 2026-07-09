# Evaluate transparent recommendation policies for one target

Evaluate transparent recommendation policies for one target

## Usage

``` r
recommend_ts_model(
  target_species,
  survey_metadata = list(),
  candidate_models = NULL,
  config = NULL,
  context = NULL,
  policies = recommendation_policy_set(),
  equation_branches = NULL,
  support_fraction = 0.8,
  validation_summary = NULL,
  local_conformal_calibration = NULL,
  selection_config = list(),
  registry_path = NULL
)
```

## Arguments

- target_species:

  Scientific name.

- survey_metadata:

  Named list or one-row data frame.

- candidate_models:

  Prepared candidate model table, a
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object, or a
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  object.

- config:

  Similarity/admissibility config with tuned values frozen.

- context:

  Optional output from `prepare_recommendation_context()`.

- policies:

  Policy definition table.

- equation_branches:

  Branches to evaluate.

- support_fraction:

  Local-support ensemble fraction.

- validation_summary:

  Optional policy-by-branch validation table.

- local_conformal_calibration:

  Optional output from `calibrate_local_recommendation_conformal()`.

- selection_config:

  List of support and tie thresholds.

- registry_path:

  Optional trait registry path.

## Value

A list with ranked policies, selected recommendations, donor rows, and
diagnostics.

## Examples

``` r
if (FALSE) { # \dontrun{
recommendation <- recommend_ts_model(
  target_species = "Sardinops sagax",
  survey_metadata = list(frequency = 38),
  candidate_models = selector
)
recommendation$selected
} # }
```
