# Generalized Configurer S7 Class

`Configurer` stores one validated normalized configuration as an S7
object. Callers can supply either a YAML path or a config list. Missing
fields are filled from the package defaults during normalization. See
[`build_configurer()`](https://brandynlucca.github.io/tsbiomass/reference/build_configurer.md)
for a complete field-by-field example.

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
  paths = list(input_file = "input.xlsx", out_root = "outputs", cache_dir = "cache"),
  selection = list(method = "glm")
))
cfg
#> Configurer
#>   base_dir: /home/runner/work/tsbiomass/tsbiomass/docs/reference
#>   input_file: /home/runner/work/tsbiomass/tsbiomass/docs/reference/input.xlsx
#>   out_root: /home/runner/work/tsbiomass/tsbiomass/docs/reference/outputs
#>   cache_dir: /home/runner/work/tsbiomass/tsbiomass/docs/reference/cache
#>   species_traits: class
#>   study_traits: fao_area
#>   active_policies: closest_within_species
#>   slope_class: all
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
