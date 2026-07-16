# Plot a `PolicySelector`

Uses the package's S7 method on
[`base::plot()`](https://rdrr.io/r/base/plot.html) so selector-stage
summaries can be drawn directly from the package object.

## Usage

``` r
# S3 method for class 'PolicySelector'
plot(
  x,
  y = NULL,
  type = c(
    "strategy_error_heatmap",
    "conformal_scores",
    "policy_benchmark",
    "species_policy_ranked"
  ),
  anchor_species,
  max_policies = 30L,
  show_values = NULL,
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

- anchor_species:

  Optional anchor species to plot. When omitted, the selector's
  configured reference-anchor species are used when available. Use
  `anchor_species = NULL` explicitly to keep all species.

- max_policies:

  Maximum number of policies shown in dense benchmark and uncertainty
  plots.

- show_values:

  Optional logical scalar controlling cell labels in heatmaps. `NULL`
  lets the plotting helper decide from grid size.

- ...:

  Additional plotting arguments.

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
