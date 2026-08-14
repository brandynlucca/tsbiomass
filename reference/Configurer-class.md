# Generalized Configurer S7 Class

`Configurer` stores one validated normalized configuration as an S7
object. Callers can supply either a YAML path or a config list. Missing
fields are filled from the package defaults during normalization.

## Properties

- `data`: Normalized configuration list.

- `base_dir`: Base directory used to resolve relative paths.

- `registry_path`: Trait-registry path used for validation, or
  `NA_character_` when the packaged registry is used.

- `policy_path`: Policy-registry path used for validation, or
  `NA_character_` when the packaged registry is used.

## Examples

``` r
cfg <- build_configurer(list(
  paths = list(
    input_file = "input.xlsx",
    out_root = "outputs",
    cache_dir = "cache",
    supplemental_dir = "supplemental",
    log_file = "outputs/run.log"
  ),
  execution = list(
    strict_length_pdf = FALSE,
    run_multiplier_model = FALSE,
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
#>   slope_class: none
#>   selection_method: glm
#>   uncertainty_method: glm
#>   alpha: 0.8
#>   kernel_scale: 4
#>   strict_length_pdf: FALSE
#>   refresh_benchmark: FALSE
#>   sections: paths, execution, tuning, similarity, ordination, ... (13 total)

if (FALSE) { # \dontrun{
cfg <- build_configurer("path/to/config.yaml")
} # }
```
