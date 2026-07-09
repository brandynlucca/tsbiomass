# Apply one Sentinel scenario to a train/test fold

Apply one Sentinel scenario to a train/test fold

## Usage

``` r
sentinel_apply_scenario(
  train_data,
  test_data,
  config,
  scenario_spec,
  manifest_row,
  object
)
```

## Arguments

- train_data:

  Training candidate-model rows.

- test_data:

  Held-out candidate-model rows.

- config:

  Workflow config list.

- scenario_spec:

  Normalized scenario specification.

- manifest_row:

  One-row manifest tibble for the active fold.

- object:

  A
  [Sentinel](https://brandynlucca.github.io/tsbiomass/reference/Sentinel-class.md)
  object.

## Value

A list with `train_data`, `test_data`, and `config`.
