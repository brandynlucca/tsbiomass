# Candidate-Model Ingest and Preparation S7 Class

`Candidates` stores the study records, species metadata, candidate
models, selected reference anchors, and downstream
similarity/admissibility results used by transferability analysis.

## Details

Construction is explicit. Callers must provide either:

- a complete candidate-ingest config list,

- a YAML path containing that config, or

- a
  [Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
  object containing the required candidate-ingest sections.

## Properties

- `spec`: Normalized candidate-ingest specification.

- `study_db`: Study metadata table read from the configured input.

- `species_vector`: Character vector of species names queried during
  metadata enrichment.

- `source_dbs`: Named list of source-specific species tables.

- `species_db`: Consolidated species metadata table after source
  precedence rules are applied.

- `candidate_models`: Final candidate-model table used by similarity,
  admissibility, benchmarking, and policy selection.

- `reference_anchors`: Candidate-model rows selected as reference
  anchors.

- `similarity_matrix`: Prepared similarity state from
  [`prepare_similarities()`](https://brandynlucca.github.io/tsbiomass/reference/prepare_similarities.md).

- `gower_distances`: Distance bundle from
  [`construct_gower_distances()`](https://brandynlucca.github.io/tsbiomass/reference/construct_gower_distances.md).

- `ordination`: Ordination results from
  [`run_ordination()`](https://brandynlucca.github.io/tsbiomass/reference/run_ordination.md).

- `admissibility`: Admissibility-screen results from
  [`screen_admissibility()`](https://brandynlucca.github.io/tsbiomass/reference/screen_admissibility.md).

- `similarity_tuning`: Similarity-tuning diagnostics from
  [`tune_similarities()`](https://brandynlucca.github.io/tsbiomass/reference/tune_similarities.md).

## Examples

``` r
cfg <- list(
  data = list(
    list(id = "study_metadata", path = "input.xlsx"),
    list(id = "worms", type = "remote", engine = "r_package"),
    list(id = "fishbase", type = "remote", engine = "r_package")
  ),
  enrich = list(
    precedence = c("fishbase", "worms")
  ),
  prepare = list(),
  anchors = list(
    selector = list(regional_body = "SWFSC")
  )
)

# This example is not run because it expects real files and, optionally,
# live API access for remote sources.
if (FALSE) { # \dontrun{
candidates <- build_candidates(cfg)
candidates
} # }
```
