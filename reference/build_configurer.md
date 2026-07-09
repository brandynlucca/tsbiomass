# Create a `Configurer`

Create a `Configurer`

## Usage

``` r
build_configurer(
  config,
  base_dir = getwd(),
  registry_path = NULL,
  policy_path = NULL
)
```

## Arguments

- config:

  A config list or a YAML path.

- base_dir:

  Base directory used to resolve relative paths.

- registry_path:

  Optional trait-registry path used during validation.

- policy_path:

  Optional policy-registry path used during validation.

## Value

A validated
[Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
object containing the normalized config.

## Examples

``` r
cfg <- build_configurer(list(
  paths = list(
    input = "input.xlsx",
    output_root = "outputs",
    cache_folder = "cache",
    support_folder = "supplemental",
    log_path = "outputs/run.log"
  ),
  execution = list(
    strict_pdf = FALSE,
    run_multiplier = FALSE,
    write_log = FALSE
  ),
  tuning = list(
    species_model_limit = 2L,
    resamples = 8L
  ),
  similarity = list(
    alpha = 0.8,
    kernel_scale = 4,
    core_weight_cutoff = 0.8,
    conformal_alpha = 0.1,
    species_traits = list(genus = 2, family = 1),
    study_traits = list(frequency = 1, fao_area = 1),
    coherence = list(
      length = list(mode = "overlap", weight = 2),
      depth = list(mode = "overlap", weight = 3),
      frequency = list(mode = "overlap", weight = 2, gap = 60)
    )
  ),
  admissibility = list(
    key_metadata_max = 0.25,
    coherence = list(
      length = list(mode = "overlap", min = 0.25),
      depth = list(mode = "overlap", min = 0.25),
      frequency = list(mode = "overlap")
    )
  ),
  policies = list(
    active = "closest_within_species"
  ),
  selection = list(
    method = "glm"
  )
))
cfg
#> Configurer
#>   base_dir: /home/runner/work/tsbiomass/tsbiomass/docs/reference
#>   input_file: /home/runner/work/tsbiomass/tsbiomass/docs/reference/input.xlsx
#>   out_root: /home/runner/work/tsbiomass/tsbiomass/docs/reference/outputs
#>   cache_dir: /home/runner/work/tsbiomass/tsbiomass/docs/reference/cache
#>   species_traits: genus, family
#>   study_traits: frequency, fao_area
#>   active_policies: closest_within_species
#>   equation_branch_filters: all
#>   selection_method: glm
#>   uncertainty_method: glm
#>   alpha: 0.8
#>   kernel_scale: 4
#>   strict_length_pdf: FALSE
#>   refresh_benchmark: FALSE
#>   sections: paths, execution, tuning, similarity, ordination, ... (13 total)
```
