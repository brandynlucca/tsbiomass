# tsbiomass

<p align="center">
  <img src="man/figures/tsbiomass-hex.svg" alt="tsbiomass hex logo" width="340" />
</p>

`tsbiomass` is an R package for TS-model transferability analysis, policy benchmarking, uncertainty calibration, and outer-loop validation.

The package is object-based and stage-based: construct objects, run explicit stages, then score and validate results. This README focuses on practical usage with exported functions only.

## Installation

Install from GitHub:

```r
install.packages("pak")
pak::pak("brandynlucca/tsbiomass")
```

Or install from a local clone:

```r
install.packages("devtools")
devtools::install(".")
```

## What You Can Do With tsbiomass

- Build and normalize candidate TS models from configured inputs.
- Benchmark transferability policies and calibrate uncertainty.
- Train and fit policy learners for recommendation.
- Produce scored predictions through `Referee`.
- Run outer-loop validation and ablations with `Sentinel`.

## Quick Start (End-to-End)

```r
library(tsbiomass)

# 1) Start from a template config and build a Configurer
cfg_list <- create_configuration_template(
  input_file = "input.xlsx",
  output_root = "outputs",
  cache_folder = "cache"
)

cfg <- build_configurer(cfg_list, base_dir = getwd())

# 2) Build candidates and choose reference anchors
candidates <- build_candidates(cfg)
candidates <- set_reference_anchors(
  candidates,
  selector = list(regional_body = "SWFSC")
)

# Optional: inspect selected anchors via exported accessor
anchors <- fetch_reference_anchors(candidates)

# 3) Similarity + admissibility stages
alchemist <- as_alchemist(candidates)
alchemist <- forge_distances(alchemist)
alchemist <- distill_traits(alchemist)
alchemist <- run_ordination(alchemist)
alchemist <- screen_admissibility(alchemist)

# 4) Policy selection stages
selector <- as_policyselector(alchemist)
selector <- benchmark(selector)
selector <- calibrate_uncertainty(selector)
selector <- select_policies(selector)

# 5) Learner stages
learner <- as_policylearner(selector)
learner <- crossfit(learner)
learner <- fit(learner)
learner <- calibrate_uncertainty(learner)

# 6) Predict and score
predictions <- predict(selector, learner = learner)
referee <- as_referee(selector, learner = learner, predictions = predictions)
scorecard <- predict(referee)

scorecard
```

## Outer-Loop Validation With Sentinel

`Sentinel` runs fold-based validation around a user-supplied workflow function.

```r
workflow_fun <- function(candidates, workflow_config_s7) {
  alchemist <- as_alchemist(candidates, config = workflow_config_s7)
  alchemist <- forge_distances(alchemist)
  alchemist <- screen_admissibility(alchemist)

  selector <- as_policyselector(alchemist, config = workflow_config_s7)
  selector <- benchmark(selector)
  selector <- calibrate_uncertainty(selector)
  selector <- select_policies(selector)

  learner <- as_policylearner(selector, config = workflow_config_s7)
  learner <- crossfit(learner)
  learner <- fit(learner)
  learner <- calibrate_uncertainty(learner)

  predictions <- predict(selector, learner = learner)
  as_referee(selector, learner = learner, predictions = predictions)
}

sentinel <- build_sentinel(
  data = candidates,
  workflow_fn = workflow_fun,
  config = cfg,
  split_mode = "species_holdout",
  trait_ablations = TRUE
)

sentinel <- run_sentinel(sentinel)

validation_card <- summary(sentinel, type = "validation")
ablation_card <- summary(sentinel, type = "ablation", metric = "error_abs_log")

plot(validation_card, type = "validation", metric = "error_abs_log")
plot(ablation_card, type = "ablation")
```

## Configuration Helpers

- `create_configuration_template()` builds a complete baseline config list.
- `read_configuration()` reads and normalizes YAML config files.
- `build_configurer()` validates and materializes config into a `Configurer`.
- `trait_names()` and `trait_definition()` expose trait registry metadata.

## Main Exported Entry Points

- Object builders/coercions: `build_configurer()`, `build_candidates()`, `as_alchemist()`, `as_policyselector()`, `as_policylearner()`, `as_policysimulator()`, `as_referee()`, `build_sentinel()`.
- Workflow stages: `set_reference_anchors()`, `forge_distances()`, `distill_traits()`, `run_ordination()`, `screen_admissibility()`, `benchmark()`, `calibrate_uncertainty()`, `select_policies()`, `crossfit()`, `fit()`, `run_sentinel()`.
- Utility/accessors: `fetch_reference_anchors()`, `recommend_ts_model()`, `list_learners()`, `available_policies()`.

## Notes

- Use exported accessors and helpers for object interaction.
- Avoid relying on internal implementation details.
- Scripts in `inst/scripts/` are orchestration examples, not the package API.
