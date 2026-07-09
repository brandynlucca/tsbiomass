# Plot Sentinel interval coverage from a Scorecard

Plot Sentinel interval coverage from a Scorecard

## Usage

``` r
plot_sentinel_coverage_scorecard(
  x,
  estimand = c("conditional", "operational", "estimability")
)
```

## Arguments

- x:

  A Sentinel-coverage
  [Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md).

- estimand:

  Coverage quantity: conditional interval coverage among estimable
  predictions, operational coverage over every held-out equation, or the
  estimability rate itself.

## Value

A ggplot object.
