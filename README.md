# tsbiomass

`tsbiomass` is an R package for TS-model transferability workflows. The package interface is class-based and stage-based: you build package objects, advance them with generic methods, and use `Sentinel` for outer-loop validation. The scripts under `inst/scripts/` are examples and orchestration assets, not the package API.

## Public interface

The package exposes constructors/coercions for the main workflow objects:

- `build_configurer()`
- `build_candidates()`
- `as_alchemist()`
- `as_policyselector()`
- `as_policylearner()`
- `as_policysimulator()`
- `as_conjurer()`
- `as_referee()`
- `build_sentinel()`

The main staged methods are:

- `set_reference_anchors()`
- `forge_distances()`
- `distill_traits()`
- `run_ordination()`
- `screen_admissibility()`
- `benchmark()`
- `calibrate_uncertainty()`
- `select_policies()`
- `crossfit()`
- `fit()`
- `run_sentinel()`

Plotting is exposed through `plot()` methods on package classes. Sentinel also
exports focused validation and ablation plotting helpers.

## Example object pipeline

```r
library(tsbiomass)

config <- build_configurer(
  read_config("path/to/config.yaml")
)

candidates <- build_candidates(config)
candidates <- set_reference_anchors(candidates)

alchemist <- as_alchemist(candidates)
alchemist <- forge_distances(alchemist)
alchemist <- distill_traits(alchemist)
alchemist <- run_ordination(alchemist)
alchemist <- screen_admissibility(alchemist)

selector <- as_policyselector(alchemist)
selector <- benchmark(selector)
selector <- calibrate_uncertainty(selector)
selector <- select_policies(selector)

learner <- as_policylearner(selector)
learner <- crossfit(learner)
learner <- fit(learner)
learner <- calibrate_uncertainty(learner)

predictions <- predict(selector, learner = learner)

referee <- as_referee(
  selector,
  learner = learner,
  predictions = predictions
)

scorecard <- predict(referee)
```

This is one package-native object pipeline. It is not the only valid analysis structure, and `Alchemist` is only one available route for the similarity/distance stage.

## Outer-loop validation with Sentinel

`Sentinel` is the package entry point for outer-loop validation. It is designed around a user-supplied workflow function rather than a package-hard-coded analysis workflow.

```r
sentinel <- build_sentinel(
  data = candidates,
  workflow_fn = my_validation_workflow,
  config = workflow_config_s7,
  split_mode = "species_holdout"
)

sentinel <- run_sentinel(sentinel)
```

Sentinel owns the outer split and constructs fold-local package objects. The
workflow function receives a `Candidates` object and a scenario-specific
`Configurer`; it can be the same staged analysis a user would run normally.
Held-out rows are available as `candidates@reference_anchors` and are excluded
from `candidates@candidate_models`.

```r
workflow_fun <- function(candidates, workflow_config_s7) {
  alchemist <- as_alchemist(candidates, config = workflow_config_s7)
  alchemist <- forge_distances(alchemist)
  alchemist <- screen_admissibility(alchemist)

  sel <- as_policyselector(alchemist, config = workflow_config_s7)
  sel <- benchmark(sel, include_ts_error = TRUE)
  sel <- calibrate_uncertainty(sel)

  learn <- as_policylearner(sel, config = workflow_config_s7)
  learn <- crossfit(learn)
  learn <- fit(learn)
  learn <- calibrate_uncertainty(learn)

  sel <- select_policies(sel)
  predictions <- predict(sel, learner = learn)
  referee <- as_referee(
    sel,
    learner = learn,
    predictions = predictions,
    config = workflow_config_s7
  )
  scorecard <- predict(referee)
  referee_rebuild(referee, scorecard = scorecard)
}

sentinel <- build_sentinel(
  data = candidates,
  workflow_fn = workflow_fun,
  config = workflow_config_s7,
  split_mode = "species_holdout",
  trait_ablations = TRUE # every configured trait present in the data
)

sentinel <- run_sentinel(sentinel)

ablation_card <- summary(sentinel, type = "ablation", metric = "error_abs_log")
validation_card <- summary(sentinel, type = "validation")

plot(ablation_card, type = "ablation")
plot(validation_card, type = "validation", metric = "error_abs_log")
```

A positive ablation importance means held-out performance worsened when the
trait was removed. Estimates and bootstrap intervals are paired by outer fold.
All Sentinel summary entry points return `Scorecard` objects.

## Configuration and registries

The configuration helpers are:

- `default_config()`
- `read_config()`
- `trait_names()`
- `trait_definition()`
- `policy_names()`
