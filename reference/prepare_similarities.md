# Prepare similarity inputs

Selects registry-defined species and study traits, applies starting
weights, expands set-valued traits to binary membership columns, and
returns the prepared species-level and study-level matrices needed for
later similarity calculations.

## Usage

``` r
prepare_similarities(
  candidate_models,
  species_traits = NULL,
  study_traits = NULL,
  alpha = NULL,
  k_species = NULL,
  k_study = NULL,
  config = NULL,
  registry_path = NULL,
  seed = NULL,
  progress = NULL
)
```

## Arguments

- candidate_models:

  Prepared candidate-model table or a
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object.

- species_traits:

  Optional species-trait specification. Use `NULL` to use all eligible
  species traits at weight `1`; a character vector to use only those
  traits at weight `1`; a named list or named numeric vector to set
  explicit starting weights; or a data frame with `trait`/`weight`
  columns. When `NULL`, a config-supplied value is used when present.

- study_traits:

  Optional study-trait specification. Follows the same rules as
  `species_traits`. When `NULL`, a config-supplied value is used when
  present.

- alpha:

  Optional starting species-versus-study mixing parameter. When `NULL`,
  a config-supplied value is used when present.

- k_species:

  Optional starting species-distance kernel parameter. When `NULL`, a
  config-supplied value is used when present.

- k_study:

  Optional starting study-distance kernel parameter. When `NULL`, a
  config-supplied value is used when present.

- config:

  Optional JSON path or list with similarity options. Supported entries
  are `species_traits`, `study_traits`, `alpha`, `k_species`, `k_study`,
  `seed`, `length_coherence`, `depth_coherence`, `frequency_coherence`,
  `alpha_grid`, `k_species_grid`, and `k_study_grid`.

- registry_path:

  Optional path to a trait-registry JSON file.

- seed:

  Optional integer seed. When `NULL`, a config-supplied value is used
  when present; otherwise one is generated and returned in the output
  object.

- progress:

  Logical scalar controlling stage messages.

## Value

When `candidate_models` is a
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object, returns that object with prepared similarity state and an
expanded candidate-model table. Otherwise, returns a list containing the
normalized tuning configuration, selected traits, starting weights,
expanded species/study matrices, and collapsed species profiles.

## Examples

``` r
if (FALSE) { # \dontrun{
candidates <- build_candidates(list(
  study = list(path = "input.xlsx"),
  anchors = list(selector = list(regional_body = "SWFSC"))
))

candidates <- prepare_similarities(
  candidate_models = candidates,
  config = build_configurer(list(
    paths = list(
      input_file = "input.xlsx",
      out_root = "outputs",
      cache_dir = "cache"
    ),
    execution = list(),
    tuning = list(),
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
)

candidates
} # }
```
