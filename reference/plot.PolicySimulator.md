# Plot a `PolicySimulator`

Uses the package's S7 method on
[`base::plot()`](https://rdrr.io/r/base/plot.html) so stored
policy-sensitivity scenario summaries can be drawn directly from the
simulator object.

## Usage

``` r
plot(
  x,
  y = NULL,
  type = c("sensitivity_overview", "policy_stability", "multiplier_drift"),
  baseline_label = "baseline",
  ...
)
```

## Arguments

- x:

  A
  [PolicySimulator](https://brandynlucca.github.io/tsbiomass/reference/PolicySimulator-class.md)
  object.

- y:

  Unused.

- type:

  Figure family to draw.

- baseline_label:

  Scenario label treated as the baseline reference.

- ...:

  Unused additional arguments.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
simulator <- simulate(as_policysimulator(selector))
plot(simulator)
plot(simulator, type = "multiplier_drift")
} # }
```
