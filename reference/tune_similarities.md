# Empirically tune the similarity configuration

Builds a reduced tuning subset, prepares the selected similarity inputs,
evaluates the supplied similarity configuration by leave-one-out error,
tunes the alpha and kernel parameters on a local grid, then drops each
selected trait and enabled coherence term to derive tuned weight
multipliers.

## Usage

``` r
tune_similarities(
  candidate_models,
  species_traits = NULL,
  study_traits = NULL,
  alpha = NULL,
  k_species = NULL,
  k_study = NULL,
  max_models_per_species = NULL,
  n_resamples = NULL,
  seed = NULL,
  config = NULL,
  cache_path = NULL,
  refresh = NULL,
  progress = NULL,
  registry_path = NULL
)
```

## Arguments

- candidate_models:

  Prepared candidate-model table.

- species_traits:

  Optional species-trait specification. See
  [`prepare_similarities()`](https://brandynlucca.github.io/tsbiomass/reference/prepare_similarities.md)
  for the accepted forms. When `NULL`, a config-supplied value is used
  when present.

- study_traits:

  Optional study-trait specification. See
  [`prepare_similarities()`](https://brandynlucca.github.io/tsbiomass/reference/prepare_similarities.md)
  for the accepted forms. When `NULL`, a config-supplied value is used
  when present.

- alpha:

  Optional starting species-versus-study mixing parameter. When `NULL`,
  a config-supplied value is used when present.

- k_species:

  Optional starting species-distance kernel parameter. When `NULL`, a
  config-supplied value is used when present.

- k_study:

  Optional starting study-distance kernel parameter. When `NULL`, a
  config-supplied value is used when present.

- max_models_per_species:

  Maximum number of retained tuning models per species. When `NULL`, a
  config-supplied value is used when present.

- n_resamples:

  Optional number of resampled tuning subsets.

- seed:

  Optional integer seed. When `NULL`, a config-supplied value is used
  when present; otherwise one is generated and returned in the output
  object.

- config:

  Optional JSON path or list with similarity options. Supported entries
  are `species_traits`, `study_traits`, `alpha`, `k_species`, `k_study`,
  `max_models_per_species`, `seed`, `length_coherence`,
  `depth_coherence`, `frequency_coherence`, `alpha_grid`,
  `k_species_grid`, `k_study_grid`, and `grid_refinement_levels`. A
  [Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
  object is also accepted.

- cache_path:

  Optional path to an `.rds` cache file.

- refresh:

  Logical scalar. If `TRUE`, ignore any existing cache file.

- progress:

  Logical scalar controlling stage messages.

- registry_path:

  Optional path to a trait-registry JSON file.

## Value

When `candidate_models` is a
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object, returns that object with the tuning result attached. Otherwise,
returns a list containing the tuned configuration, tuning subset, score
history, and per-trait component-impact summary.

## Examples

``` r
if (FALSE) { # \dontrun{
candidates <- build_candidates(list(
  study = list(path = "input.xlsx"),
  anchors = list(selector = list(regional_body = "SWFSC"))
))

cfg_data <- build_configurer(list(
  paths = list(
    input_file = "input.xlsx",
    out_root = "outputs",
    cache_dir = "cache"
  ),
  execution = list(),
  tuning = list(
    max_models_per_species = 2L,
    n_resamples = 8L,
    grid_refinement_levels = 1L
  ),
  policy = list(
    alpha = 0.8,
    k_species = 4,
    k_study = 2,
    frequency_coherence_mode = "overlap",
    length_overlap_weight = 2,
    depth_overlap_weight = 2,
    frequency_coherence_weight = 1,
    species_traits = list(genus = 1, family = 1),
    study_traits = list(frequency = 1, fao_area = 1)
  ),
  policies = list(active = "closest_within_species")
))

tune_obj <- tune_similarities(
  candidate_models = candidates,
  config = cfg_data
)
tune_obj
} # }
```
