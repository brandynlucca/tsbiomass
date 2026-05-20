# tsbiomass

`tsbiomass` is an R package of reusable functions for TS model transferability analyses. The actual workflow application lives in the CLI/orchestration scripts under `inst/scripts/`, while the package source keeps the domain logic, config parsing, and helper functions.

## What lives where

- `R/ingest.R`
  Reading, enrichment, trait harmonization, group-model tagging, and length-PDF construction.
- `R/admissibility.R`
  Applicability screening, overlap calculations, admissibility gates, and anchor-level candidate evaluation.
- `R/similarity.R`
  Trait distance, hybrid weighting, ordination, clustering, and neighborhood lookup logic.
- `R/strategy.R`
  Strategy definitions, benchmark comparison, conformal multiplier intervals, and strategy selection utilities.
- `R/sensitivity.R`
  Tuning, resampling, scenario sweeps, and uncertainty-driver evaluation.
- `R/graphics.R`
  Workflow figure/table helpers that are used directly by the orchestration script.
- `R/config.R`
  YAML/JSON workflow configuration defaults, allow-lists, parsing, and validation.
- `inst/scripts/run_paper_policy_workflow.R`
  CLI application entrypoint.
- `inst/scripts/run_paper_policy_workflow_body.R`
  External orchestration script. This is intentionally not part of the package `R/` source tree.

## Config-driven usage

Create a YAML or JSON workflow config. A starter YAML template is provided at:

- `inst/templates/swfscfish_config.yml`


- `inst/templates/trait_registry.json`

From the command line:

```powershell
Rscript inst/scripts/run_paper_policy_workflow.R --config inst/templates/workflow_config.yml
```

Legacy positional-argument mode is still supported:

```powershell
Rscript inst/scripts/run_paper_policy_workflow.R fishery_survey_tsl.xlsx outputs_paper_workflow cache false false
```

## Configuration notes

The workflow config is designed to centralize:

- input, output, cache, logging, and ancillary file paths
- tuning parameters
- admissibility thresholds
- similarity hyperparameters
- active strategies
- active species/study trait columns and their weights

The trait registry JSON provides a portable definition of the accepted trait universe, including:

- species-level traits
- study-level traits
- controlled vocabularies
- multi-value and binary-expansion metadata

Use these helpers to inspect what is allowed:

```r
trait_names()
policy_names()
```
