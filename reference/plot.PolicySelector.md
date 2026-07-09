# Plot a `PolicySelector`

Uses the package's S7 method on
[`base::plot()`](https://rdrr.io/r/base/plot.html) so selector-stage
summaries can be drawn directly from the package object.

## Usage

``` r
graphics::plot(
  x,
  y = NULL,
  type = c(
    "strategy_error_heatmap",
    "conformal_scores",
    "policy_benchmark",
    "species_policy_ranked"
  ),
  ...
)
```

## Arguments

- x:

  A
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  object.

- y:

  Unused.

- type:

  Figure family to draw.

- ...:

  Unused additional arguments.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
selector <- benchmark(as_policyselector(candidates))
plot(selector, type = "policy_benchmark")
plot(selector, type = "strategy_error_heatmap")
} # }
```
