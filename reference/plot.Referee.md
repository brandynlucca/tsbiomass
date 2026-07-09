# Plot a `Referee`

Uses the package's S7 method on
[`base::plot()`](https://rdrr.io/r/base/plot.html) so the integrated
recommendation-stage summaries can be drawn from the full referee object
when both selector context and scorecard tables are needed.

## Usage

``` r
plot(
  x,
  y = NULL,
  type = "biomass_change",
  view = NULL,
  anchor_model_id = NULL,
  anchor_species = NULL,
  reference_name = NULL,
  ...
)
```

## Arguments

- x:

  A
  [Referee](https://brandynlucca.github.io/tsbiomass/reference/Referee-class.md)
  object.

- y:

  Unused.

- type:

  Figure family to draw.

- view:

  Secondary plot selector used for `type = "biomass_change"` and
  `type = "length_density"`.

- anchor_model_id:

  Optional anchor model ID used for anchor-specific diagnostics.

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
referee <- as_referee(selector, predictions = predictions)
scorecard <- predict(referee)
referee <- tsbiomass:::referee_rebuild(referee, scorecard = scorecard)
plot(referee, type = "biomass_change")
plot(referee, type = "length_density", anchor_species = "Sardinops sagax")
} # }
```
