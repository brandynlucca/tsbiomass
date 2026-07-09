# Conjurer S7 Class

`Conjurer` runs a targeted missing-study-metadata uncertainty analysis
on a
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
and an optional fitted
[PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md).
It keeps the fitted selector state fixed, disables only the missingness
admissibility gate for the auxiliary analysis, imputes one selected
study trait at a time, and reruns the downstream recommendation path
across repeated stochastic draws.

## Details

The resulting object stores the raw draw-level recommendation outputs
and a compact anchor-by-trait instability summary.

## Properties

- `selector`: Source
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md).

- `learner`: Optional fitted
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md).

- `config`: Conjurer analysis configuration.

- `results`: Raw simulation result bundle.

- `manifest`: Draw manifest.

- `draws`: Draw-level recommendation rows.

- `summary`: Anchor-by-trait instability summary.

## Examples

``` r
if (FALSE) { # \dontrun{
selector <- as_policyselector(candidates)
selector <- benchmark(selector)
selector <- calibrate_uncertainty(selector)
selector <- select_policies(selector)
learner <- as_policylearner(selector)
learner <- crossfit(learner)
learner <- fit(learner)
learner <- calibrate_uncertainty(learner)

conjurer <- as_conjurer(selector, learner = learner)
conjurer <- simulate(conjurer)
conjurer
} # }
```
