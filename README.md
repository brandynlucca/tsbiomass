# tsbiomass <img src="man/figures/tsbiomass-hex.svg" align="right" height="158" alt="tsbiomass hex logo" />

`tsbiomass` is an R package for TS-model transferability and TS-to-biomass decision support, covering candidate-model ingestion, reference-anchor selection, policy benchmarking, and uncertainty calibration.

It provides an end-to-end workflow for model recommendation and scored evaluation, including outer-loop holdout validation and ablation analysis with Sentinel.

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

## Workflow At A Glance

<table>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/configurer.svg" width="100" alt="Configurer" /> <p style="text-align: center";> <code>Configurer</code> </p></td>
    <td><strong>Configure analysis settings</strong><br />Specify paths, trait/policy registries, and runtime controls in a single configuration surface. This establishes a reproducible run contract before any modeling begins.</td>
  </tr>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/candidates.svg" width="100" alt="Candidates" /> <code>Candidates</code></td>
    <td><strong>Assemble candidate model data</strong><br />Ingest and normalize candidate-model records, then identify reference anchors. This produces the standardized input set used for transferability evaluation.</td>
  </tr>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/alchemist.svg" width="100" alt="Alchemist" /> <code>Alchemist</code> <img src="man/figures/classes/conjurer.svg" width="100" alt="Conjurer" /> <code>Conjurer</code></td>
    <td><strong>Build similarity context and admissibility filters</strong><br />Quantify donor-target relatedness and remove unsupported transfer paths. This constrains downstream policy decisions to credible support regions.</td>
  </tr>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/policyselector.svg" width="100" alt="PolicySelector" /> <code>PolicySelector</code> <img src="man/figures/classes/policylearner.svg" width="100" alt="PolicyLearner" /> <code>PolicyLearner</code></td>
    <td><strong>Select and learn transfer policies</strong><br />Benchmark policy families, calibrate uncertainty, and fit meta-learners for context-aware policy choice. This stage determines which transfer rule is applied where.</td>
  </tr>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/policypredictions.svg" width="100" alt="PolicyPredictions" /> <p style="text-align: center";> <code>PolicyPredictions</code> </p> <img src="man/figures/classes/referee.svg" width="100" alt="Referee" /> <p style="text-align: center";> <code>Referee</code> </p> <img src="man/figures/classes/scorecard.svg" width="100" alt="Scorecard" /> <p style="text-align: center";> <code>Scorecard</code> </p></td>
    <td><strong>Generate predictions and score outcomes</strong><br />Generate policy-level predictions, aggregate them via referee logic, and summarize performance and interval behavior in scorecards for direct comparison.</td>
  </tr>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/sentinel.svg" width="100" alt="Sentinel" /> <p style="text-align: center";> <code>Sentinel</code> </p></td>
    <td><strong>Validate under holdout scenarios</strong><br />Run outer-loop holdouts and ablations to quantify robustness across species/study splits and sensitivity to trait and gate assumptions.</td>
  </tr>
</table>

## Quick Start (End-to-End)

```r
# Load package
library(tsbiomass)
```

```r
# Start from a baseline configuration
cfg_list <- create_configuration_template(
  input_file = "input.xlsx",
  output_root = "outputs",
  cache_folder = "cache"
)
cfg <- build_configurer(cfg_list, base_dir = getwd())
```

```r
# Build candidates and set anchor selection
candidates <- build_candidates(cfg)
candidates <- set_reference_anchors(
  candidates,
  selector = list(regional_body = "SWFSC")
)
anchors <- fetch_reference_anchors(candidates)
```

```r
# Similarity + admissibility stage
alchemist <- as_alchemist(candidates)
alchemist <- forge_distances(alchemist)
alchemist <- distill_traits(alchemist)
alchemist <- run_ordination(alchemist)
alchemist <- screen_admissibility(alchemist)
```

```r
# Policy selection stage
selector <- as_policyselector(alchemist)
selector <- benchmark(selector)
selector <- calibrate_uncertainty(selector)
selector <- select_policies(selector)
```

```r
# Learner stage
learner <- as_policylearner(selector)
learner <- crossfit(learner)
learner <- fit(learner)
learner <- calibrate_uncertainty(learner)
```

```r
# Predict and score
predictions <- predict(selector, learner = learner)
referee <- as_referee(selector, learner = learner, predictions = predictions)
scorecard <- predict(referee)
scorecard
```

## Outer-Loop Validation With Sentinel

`Sentinel` runs fold-based validation around a user-supplied workflow function.

```r
# Define a fold-local workflow
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
```

```r
# Build and run Sentinel with holdout validation + ablations
sentinel <- build_sentinel(
  data = candidates,
  workflow_fn = workflow_fun,
  config = cfg,
  split_mode = "species_holdout",
  trait_ablations = TRUE
)
sentinel <- run_sentinel(sentinel)
```

```r
# Summarize and visualize validation outputs
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

- Workflow stages: `set_reference_anchors()`, `forge_distances()`, `distill_traits()`, `run_ordination()`, `screen_admissibility()`, `benchmark()`, `calibrate_uncertainty()`, `select_policies()`, `crossfit()`, `fit()`, `run_sentinel()`.
- Utility/accessors: `fetch_reference_anchors()`, `recommend_ts_model()`, `list_learners()`, `available_policies()`.

## Notes

- Use exported accessors and helpers for object interaction.
- Avoid relying on internal implementation details.
- Scripts in `inst/scripts/` are orchestration examples, not the package API.
