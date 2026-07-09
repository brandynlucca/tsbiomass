# Plot `Conjurer` missingness uncertainty summaries

Plot `Conjurer` missingness uncertainty summaries

## Usage

``` r
# S3 method for class 'Conjurer'
plot(
  x,
  y = NULL,
  metric = "mean_abs_db_shift",
  trait_labs = NULL,
  anchor_order = NULL,
  trait_order = NULL,
  title = NULL,
  subtitle = NULL,
  fill_lab = NULL,
  show_values = TRUE,
  na_value = "grey90",
  tile_colour = "white",
  ...
)
```

## Arguments

- x:

  A
  [Conjurer](https://brandynlucca.github.io/tsbiomass/reference/Conjurer-class.md)
  object.

- y:

  Unused.

- metric:

  Summary metric to plot.

- trait_labs:

  Optional named vector mapping stored trait names to display labels.

- anchor_order:

  Optional anchor-species order.

- trait_order:

  Optional trait order.

- title:

  Optional plot title.

- subtitle:

  Optional plot subtitle.

- fill_lab:

  Optional legend title.

- show_values:

  Logical scalar indicating whether to print cell labels.

- na_value:

  Tile color for missing cells.

- tile_colour:

  Tile border color.

- ...:

  Unused additional arguments.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot(conjurer)
plot(conjurer, metric = "q95_abs_db_shift")
plot(conjurer, metric = "switch_rate_vs_baseline")
} # }
```
