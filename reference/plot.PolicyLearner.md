# Plot a `PolicyLearner`

Uses the package's S7 method on
[`base::plot()`](https://rdrr.io/r/base/plot.html) so the stored
cross-fitted learner predictions and post-selection calibration
summaries can be drawn directly from the learner object without
rebuilding diagnostic tables in user code.

## Usage

``` r
plot(
  x,
  y = NULL,
  type = c(
    "predicted_vs_observed",
    "calibration_curve",
    "residuals",
    "score_by_policy",
    "support_bin_error",
    "selected_policy_counts",
    "recommendation_stability"
  ),
  view = NULL,
  outcome = c("modeled", "raw"),
  rows = c("all", "selected"),
  n_bins = 10L,
  ...
)
```

## Arguments

- x:

  A
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
  object.

- y:

  Unused.

- type:

  Figure family to draw.

- view:

  Secondary plot selector used for `type = "residuals"` and
  `type = "selected_policy_counts"`.

- outcome:

  Outcome variant used for learner calibration diagnostics. `"modeled"`
  uses the modeled target stored in `.outcome` when available. `"raw"`
  uses `.outcome_raw` when available and otherwise falls back to the
  configured outcome column.

- rows:

  Row subset used for learner diagnostics. `"all"` uses all stored
  cross-fitted prediction rows. `"selected"` uses the score-minimizing
  rows retained during post-selection calibration.

- n_bins:

  Number of quantile bins used for `type = "calibration_curve"`.

- ...:

  Unused additional arguments.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
learner <- as_policylearner(selector)
learner <- crossfit(learner)
learner <- fit(learner)
learner <- calibrate_uncertainty(learner)
plot(learner, type = "predicted_vs_observed")
plot(learner, type = "calibration_curve", outcome = "raw")
plot(learner, type = "residuals", view = "by_policy", outcome = "raw")
} # }
```
