# Calibrate policy-transfer uncertainty

Builds the conformal uncertainty layer used by downstream policy
selection, prediction, and reporting. The generic dispatches on the
supplied object:
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
methods calibrate interval widths from benchmarked pseudo-anchor and
species-block errors, while
[PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
methods calibrate post-selection intervals from stored cross-fitted
learner predictions.

## Usage

``` r
calibrate_uncertainty(object, ...)
```

## Arguments

- object:

  A
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  with benchmark results or a
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
  with cross-fitted prediction results.

- ...:

  Additional calibration controls such as coverage level, calibration
  table overrides, cache controls, or learner-specific support-bin
  settings.

## Value

The same class of object supplied in `object`, updated with an
uncertainty calibration bundle.

## Details

Calibration does not refit candidate models or rerun the policy
benchmark. It consumes already-computed benchmark or cross-fit tables,
estimates residual quantiles at the configured coverage level, and
stores the resulting lookup tables, conformal thresholds, diagnostics,
and selected calibration rows on the returned object.
