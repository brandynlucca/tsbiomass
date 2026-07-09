# Select benchmark-supported policies

Chooses the policy or policies retained for each reference anchor after
benchmarking and uncertainty calibration. For a
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md),
selection ranks policies using benchmark error, uncertainty width, donor
support, and configured equivalence rules, then stores anchor-level
selected rows and diagnostics for prediction and reporting.

## Usage

``` r
select_policies(object, ...)
```

## Arguments

- object:

  A
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  object.

- ...:

  Selection controls such as policy-performance overrides, tolerance
  settings, cache path, refresh flag, or progress display.

## Value

The supplied
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
updated with selected policies and selection diagnostics.

## Details

Selection requires benchmark results and calibrated uncertainty. It does
not rebuild candidates or refit learners; it consumes the selector's
current benchmark and uncertainty layers.
