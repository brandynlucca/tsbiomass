# Plot a `PolicyPredictions`

Uses the package's S7 method on
[`base::plot()`](https://rdrr.io/r/base/plot.html) so prediction bundles
can be inspected directly without extracting the stored interval tables
by hand.

## Usage

``` r
plot(
  x,
  y = NULL,
  type = c("selected_intervals", "strategy_competition"),
  anchor_species = NULL,
  reference_name = NULL,
  ...
)
```

## Arguments

- x:

  A
  [PolicyPredictions](https://brandynlucca.github.io/tsbiomass/reference/PolicyPredictions-class.md)
  object.

- y:

  Unused.

- type:

  Figure family to draw.

- anchor_species:

  Optional anchor species used to restrict
  `type = "strategy_competition"` to one reference.

- reference_name:

  Optional display label used when plotting one anchor's policy
  competition panel.

- ...:

  Unused additional arguments.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
predictions <- predict(selector)
plot(predictions, type = "selected_intervals")
plot(predictions, type = "strategy_competition")
} # }
```
