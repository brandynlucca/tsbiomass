# Distill trait importances from a fitted Alchemist

Computes sigma dropout sensitivity for each trait from the fitted
distance learner. Dropout sensitivity zeros one feature at a time,
re-predicts pairwise distances, and measures the change in
kernel-weighted sigma RMSE - the same objective minimised by the
empirical similarity tuning step.

## Usage

``` r
distill_traits(object, ...)
```

## Arguments

- object:

  An
  [Alchemist](https://brandynlucca.github.io/tsbiomass/reference/Alchemist-class.md)
  object after
  [`forge_distances()`](https://brandynlucca.github.io/tsbiomass/reference/forge_distances.md).

- ...:

  Trait-importance controls such as dropout scope, cache controls, or
  progress display.

## Value

An updated
[Alchemist](https://brandynlucca.github.io/tsbiomass/reference/Alchemist-class.md)
object containing trait-importance diagnostics.
