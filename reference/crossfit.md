# Cross-fit a policy learner

Trains the policy learner in outer folds and stores out-of-fold
predictions for benchmark rows. Cross-fitting estimates how the
meta-policy learner behaves on anchors it did not train on, which is the
calibration target used later by
[`calibrate_uncertainty()`](https://brandynlucca.github.io/tsbiomass/reference/calibrate_uncertainty.md)
for post-selection intervals.

## Usage

``` r
crossfit(object, ...)
```

## Arguments

- object:

  A
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
  object created from a benchmarked
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md).

- ...:

  Cross-fitting controls such as benchmark table overrides, fold count,
  group column, learner method, feature columns, seed, workers, or
  progress display.

## Value

The supplied
[PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
updated with cross-fit predictions, prepared training data, and resolved
learner settings.

## Details

The method prepares the benchmark-derived training frame, resolves
feature columns and learner controls from the object configuration, fits
one learner per fold, and stores fold predictions plus the resolved
training settings on the returned
[PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md).
It clears any final fit or calibration state because those depend on the
cross-fit results.

## Examples

``` r
if (FALSE) { # \dontrun{
learner <- as_policylearner(selector)
learner <- crossfit(learner)
} # }
```
