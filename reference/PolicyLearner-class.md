# Policy Learner S7 Class

`PolicyLearner` wraps the meta-policy and super-learner pipeline around
a
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md).
It owns the cross-fitted meta-policy benchmark, final learner fit, and
post-selection calibration state used to rank anchor-policy predictions
by predicted transferability score.

## Details

The learner is designed to plug back into
[`predict()`](https://rdrr.io/r/stats/predict.html) on a
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md),
so the selector remains the single high-level source of policy
predictions.

## Examples

``` r
if (FALSE) { # \dontrun{
selector <- as_policyselector(candidates)
selector <- benchmark(selector)
learner <- as_policylearner(selector)
learner <- crossfit(learner)
learner <- fit(learner)
learner <- calibrate_uncertainty(learner)
} # }
```
