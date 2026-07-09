# Collect scenario benchmark tables

Binds the benchmark tables produced by sensitivity scenarios into a
common result bundle. The returned list keeps selection references,
selected anchor rows, pairwise equivalence tests, equivalence classes,
and conformal summaries separated while adding the scenario name to each
table.

## Usage

``` r
collect_sensitivity_results(scenario_results, ...)
```

## Arguments

- scenario_results:

  Named scenario benchmark result list or a
  [PolicySimulator](https://brandynlucca.github.io/tsbiomass/reference/PolicySimulator-class.md)
  object.

- ...:

  Reserved for method-specific inputs.

## Value

A named list of scenario-indexed tibbles.

## Examples

``` r
if (FALSE) { # \dontrun{
simulator <- as_policysimulator(selector)
simulator <- simulate(simulator)
collect_sensitivity_results(simulator)
} # }
```
