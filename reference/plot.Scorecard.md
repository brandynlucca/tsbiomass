# Plot a `Scorecard`

Uses the package's S7 method on
[`base::plot()`](https://rdrr.io/r/base/plot.html) so post-prediction
report tables can be visualized directly from the scorecard object.

## Usage

``` r
.plot_scorecard(
  x,
  y = NULL,
  type = c("ts_length", "ts_length_conformal", "ts_length_multiplier", "ts_length_bands",
    "coefficient_uncertainty", "selected_intervals", "selected_multiplier_summary",
    "selected_policy_counts", "strategy_competition", "field_missingness", "ablation",
    "validation", "coverage", "ablation_decomposition", "sentinel_ablation",
    "sentinel_ablation_decomposition", "sentinel_validation", "sentinel_coverage"),
  scale = c("ts", "multiplier"),
  view = NULL,
  anchor_model_id = NULL,
  anchor_species = NULL,
  show_top_candidate = FALSE,
  reference_label = "Reference",
  ...
)
```

## Arguments

- x:

  A
  [Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
  object.

- y:

  Unused.

- type:

  Figure family to draw.

- scale:

  Output scale used for `type = "ts_length"`.

- view:

  Secondary plot selector. For `type = "validation"`, supported values
  are `"distribution"`, `"ranked"`, `"ranked_species"`, and `"fold"`.

- anchor_model_id:

  Optional anchor model ID used for `type = "ts_length_bands"` and
  `type = "strategy_competition"`.

- anchor_species:

  Optional anchor species used when `anchor_model_id` is not supplied.

- show_top_candidate:

  Logical scalar indicating whether the TS-length plots should overlay
  the top-ranked admissible candidate curve when it is available.

- reference_label:

  Reference label used for `type = "selected_intervals"`.

- ...:

  Additional arguments used by Sentinel scorecards, including `metric`,
  `summary_method`, `metric_scale`, `scenario_names`, and
  `species_names`, `label_species`, and coverage-display arguments.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
scorecard <- predict(as_referee(selector, predictions = predictions))
plot(scorecard, type = "ts_length_conformal")
plot(scorecard, type = "coefficient_uncertainty")
plot(scorecard, type = "field_missingness")
} # }
```
