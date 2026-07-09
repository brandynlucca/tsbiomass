# Policy Simulator S7 Class

`PolicySimulator` wraps the policy-sensitivity pipeline around a
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md).
It owns the scenario specifications, the rerun benchmark results, and
the scenario manifest/tables used for downstream sensitivity summaries.

## Properties

- `selector`: Source
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md).

- `config`: Simulator configuration.

- `scenarios`: Scenario specifications.

- `results`: Scenario benchmark results.

- `manifest`: Scenario manifest table.

- `tables`: Collected scenario result tables.

The simulator is intentionally selector-centered: it rebuilds
sensitivity scenarios from the selector's current candidate pool and
policy/admissibility settings, then stores the scenario reruns for later
inspection.

## Examples

``` r
if (FALSE) { # \dontrun{
selector <- as_policyselector(candidates)
selector <- benchmark(selector)
selector <- calibrate_uncertainty(selector)
selector <- select_policies(selector)

simulator <- as_policysimulator(selector)
simulator <- simulate(simulator)
construct_sensitivity_table(simulator)
collect_sensitivity_results(simulator)
} # }
```
