# tsbiomass

`tsbiomass` is an R package for TS-length model transferability and
TS-to-biomass decision support, covering candidate-model ingestion,
reference-anchor selection, policy benchmarking, and uncertainty
calibration.

It provides an end-to-end workflow for model recommendation and scored
evaluation, including outer-loop holdout validation and ablation
analysis with Sentinel.

## Installation

Install from GitHub:

``` r

install.packages("pak")
pak::pak("brandynlucca/tsbiomass")
```

Or install from a local clone:

``` r

install.packages("devtools")
devtools::install(".")
```

## What You Can Do With tsbiomass

- Build and normalize candidate TS-length models from configured inputs.
- Benchmark transferability policies and calibrate uncertainty.
- Train and fit policy learners for recommendation.
- Produce scored predictions through `Referee`.
- Run outer-loop validation and ablations with `Sentinel`.

## Workflow At A Glance

[TABLE]

## Quick Start

``` r

library(tsbiomass)

# Configure
cfg <- build_configurer(create_configuration_template("input.xlsx"), base_dir = getwd())

# Candidates + anchors
candidates <- build_candidates(cfg)
candidates <- set_reference_anchors(candidates, selector = list(regional_body = "SWFSC"))

# Similarity + admissibility
alchemist <- as_alchemist(candidates) |> forge_distances() |> screen_admissibility()

# Policy selection + learning
selector <- as_policyselector(alchemist) |> benchmark() |> calibrate_uncertainty() |> select_policies()
learner <- as_policylearner(selector) |> crossfit() |> fit() |> calibrate_uncertainty()

# Score
predictions <- predict(selector, learner = learner)
scorecard <- predict(as_referee(selector, learner = learner, predictions = predictions))
```

## Outer-Loop Validation With Sentinel

Supply a fold-local workflow function and run holdout validation in one
call:

``` r

sentinel <- build_sentinel(
  data = candidates,
  workflow_fn = my_workflow_fun,   # function(candidates, cfg) -> Referee
  config = cfg,
  split_mode = "species_holdout",
  trait_ablations = TRUE
)
sentinel <- run_sentinel(sentinel)

plot(summary(sentinel, type = "validation"), metric = "error_abs_log")
```
