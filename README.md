# tsbiomass <a href="https://brandynlucca.github.io/tsbiomass/"><img src="man/figures/logo.png" align="right" height="158" alt="tsbiomass website" /></a>


`tsbiomass` is an R package for TS-length model transferability and TS-to-biomass decision support, covering candidate-model ingestion, reference-anchor selection, policy benchmarking, and uncertainty calibration.

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

- Build and normalize candidate TS-length models from configured inputs.
- Benchmark transferability policies and calibrate uncertainty.
- Train and fit policy learners for recommendation.
- Produce scored predictions through `Referee`.
- Run outer-loop validation and ablations with `Sentinel`.

## Workflow At A Glance

<table>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/configurer.svg" width="100" alt="Configurer" /> <p style="text-align: center;"> <code>Configurer</code> </p></td>
    <td><strong>Configure analysis settings</strong><br />Specify paths, trait/policy registries, and runtime controls in a single configuration surface. This establishes a reproducible run contract before any modeling begins. </td>
  </tr>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/candidates.svg" width="100" alt="Candidates" /> <code>Candidates</code></td>
    <td><strong>Assemble candidate model data</strong><br />Ingest and normalize candidate-model records, then identify reference anchors. This produces the standardized input set used for transferability evaluation. Various methods enable empirical approaches for constructing similarity matrices and estimating proxy model transferability tax.</td>
  </tr>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/alchemist.svg" width="100" alt="Alchemist" /> <code>Alchemist</code></td>
    <td><strong>Build similarity context and admissibility filters</strong><br />Quantify donor-target relatedness and remove unsupported transfer paths using a Super Learner ensemble to construct similarity and transferability matrices, with optional <i>post hoc</i> ordination to visualize proxy model diversity. This constrains downstream policy decisions to credible support regions.</td>
  </tr>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/policyselector.svg" width="100" alt="PolicySelector" /> <code>PolicySelector</code> <img src="man/figures/classes/policylearner.svg" width="100" alt="PolicyLearner" /> <code>PolicyLearner</code></td>
    <td><strong>Select and learn transfer policies</strong><br />Benchmark policy families, calibrate uncertainty, and fit meta-learners for context-aware policy choice. This stage determines which transfer rule is applied where.<br /> - <code>PolicySelector</code>: benchmarks and ranks available transfer policies against reference anchors, then calibrates prediction intervals.<br /> - <code>PolicyLearner</code>: fits a context-aware meta-learner that recommends the best policy for a given target based on cross-validated performance.</td>
  </tr>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/policypredictions.svg" width="100" alt="PolicyPredictions" /> <p style="text-align: center;"> <code>PolicyPredictions</code> </p> <img src="man/figures/classes/referee.svg" width="100" alt="Referee" /> <p style="text-align: center;"> <code>Referee</code> </p> <img src="man/figures/classes/scorecard.svg" width="100" alt="Scorecard" /> <p style="text-align: center;"> <code>Scorecard</code> </p></td>
    <td><strong>Generate predictions and score outcomes</strong><br />Generate policy-level predictions, aggregate them via referee logic, and summarize performance and interval behavior in scorecards for direct comparison.<br /> - <code>PolicyPredictions</code>: holds raw policy-level point predictions and uncertainty intervals for each target.<br /> - <code>Referee</code>: aggregates predictions across policies using learned weights and resolves conflicts into a final recommended estimate.<br /> - <code>Scorecard</code>: computes scored performance metrics (e.g., interval score, log-error) for comparing policies and learner configurations.</td>
  </tr>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/conjurer.svg" width="100" alt="Conjurer" /> <code>Conjurer</code> <img src="man/figures/classes/policysimulator.svg" width="100" alt="PolicySimulator" /> <code>PolicySimulator</code></td>
    <td><strong>Probe sensitivity to assumptions</strong><br />Stress-test the pipeline by perturbing inputs and settings before committing to a recommendation.<br /> - <code>Conjurer</code>: evaluates the effect of trait missingness on TS-length proxy model uncertainty.<br /> - <code>PolicySimulator</code>: reruns policy benchmarking across perturbed scenarios to quantify how sensitive transfer-policy choice is to candidate-pool and admissibility settings.</td>
  </tr>
  <tr>
    <td width="75" valign="top" style="text-align: center;"><img src="man/figures/classes/sentinel.svg" width="100" alt="Sentinel" /> <p style="text-align: center;"> <code>Sentinel</code> </p></td>
    <td><strong>Validate under holdout scenarios</strong><br />Run outer-loop holdouts and ablations to quantify robustness across species/study splits and sensitivity to trait and gate assumptions.</td>
  </tr>
</table>

## Quick Start

```r
library(tsbiomass)

# Configure
cfg <- build_configurer(create_configuration_template(), base_dir = getwd())

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

Supply a fold-local workflow function and run holdout validation in one call:

```r
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
