# Referee and Scorecard S7 Classes

`Referee` is the post-prediction recommendation layer for the main
policy-transfer pipeline. It consumes a
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md),
an optional
[PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md),
and the resulting
[PolicyPredictions](https://brandynlucca.github.io/tsbiomass/reference/PolicyPredictions-class.md)
bundle from [`predict()`](https://rdrr.io/r/stats/predict.html) on the
selector. It does not rerun a parallel recommendation policy engine.
Instead, it turns the already selected anchor-policy results into a
typed
[Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
object.

## Details

`Scorecard` stores the anchor-facing recommendation outputs and
diagnostics produced from the selector's prediction bundle. This
includes the selected rows, the full interval table, consensus
summaries, anchor audit tables, coverage summaries, missingness
summaries, and explicit component-status rows when partial output is
allowed.

Typical use is:

- run the prepared selector pipeline

- optionally attach a
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)

- call [`predict()`](https://rdrr.io/r/stats/predict.html) on the
  selector

- pass that prediction bundle into `Referee`

- call [`predict()`](https://rdrr.io/r/stats/predict.html) on `Referee`
  to return a
  [Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
selector <- as_policyselector(candidates)
selector <- benchmark(selector)
selector <- calibrate_uncertainty(selector)
selector <- select_policies(selector)
learner <- as_policylearner(selector)
learner <- crossfit(learner)
learner <- fit(learner)
learner <- calibrate_uncertainty(learner)
predictions <- predict(selector, learner = learner)
referee <- as_referee(selector, learner = learner, predictions = predictions)
scorecard <- predict(referee)
scorecard
} # }
```
