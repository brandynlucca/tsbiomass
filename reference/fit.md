# Fit a final policy learner

Fits the final learner used for future policy scoring after
cross-fitting has established the training recipe and calibration
target. For a
[PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md),
this trains the configured meta-policy model on the full
benchmark-derived training table rather than on held-out folds.

## Usage

``` r
fit(object, ...)
```

## Arguments

- object:

  A
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
  object, usually after
  [`crossfit()`](https://brandynlucca.github.io/tsbiomass/reference/crossfit.md).

- ...:

  Fit controls such as training-data override, learner method, feature
  columns, tuning folds, seed, method settings, or progress display.

## Value

The supplied
[PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
updated with the fitted final meta-policy learner and resolved fit
settings.

## Details

The fitted model is stored on the returned learner and is consumed by
[`stats::predict()`](https://rdrr.io/r/stats/predict.html) when scoring
new anchor-policy rows. Fitting does not perform uncertainty
calibration; run
[`calibrate_uncertainty()`](https://brandynlucca.github.io/tsbiomass/reference/calibrate_uncertainty.md)
after `fit()` when calibrated intervals are needed.

## Examples

``` r
if (FALSE) { # \dontrun{
learner <- as_policylearner(selector)
learner <- fit(learner)
} # }
```
