# Construct a sensitivity scenario manifest

Summarizes each sensitivity scenario and its benchmark outcome in one
scenario-level table. The manifest records the scenario inputs,
candidate model counts, configured admissibility/similarity settings,
the benchmark's best policy, and the set of policies equivalent to the
best policy.

## Usage

``` r
construct_sensitivity_table(scenario_specifications, ...)
```

## Arguments

- scenario_specifications:

  Named scenario-specification list.

- ...:

  Scenario benchmark results and optional configuration overrides, or a
  [PolicySimulator](https://brandynlucca.github.io/tsbiomass/reference/PolicySimulator-class.md)
  object containing stored scenarios and results.

## Value

A tibble with one row per sensitivity scenario.

## Examples

``` r
if (FALSE) { # \dontrun{
simulator <- as_policysimulator(selector)
simulator <- simulate(simulator)
construct_sensitivity_table(simulator)
} # }
```
