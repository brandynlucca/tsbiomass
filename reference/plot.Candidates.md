# Plot a `Candidates`

Uses the package's S7 method on
[`base::plot()`](https://rdrr.io/r/base/plot.html) so Candidates objects
can be plotted directly with `plot(candidates, ...)`.

## Usage

``` r
graphics::plot(
  x,
  y = NULL,
  type = c(
    "area_distribution",
    "ordination",
    "admissibility",
    "candidate_review",
    "uncertainty_importance",
    "similarity_tuning",
    "most_similar",
    "candidate_biomass_response",
    "model_weights",
    "similarity_map",
    "component_importance",
    "tuning_variation",
    "slope_support",
    "slope_group"
  ),
  count_type = c("studies", "models"),
  dissimilarity = c("combined", "species", "study"),
  view = NULL,
  anchor_model_id = NULL,
  anchor_species = NULL,
  include_hulls = TRUE,
  ...
)
```

## Arguments

- x:

  A
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object.

- y:

  Unused.

- type:

  Figure family to draw.

- count_type:

  Count definition used for `type = "area_distribution"`.

- dissimilarity:

  Dissimilarity layer used for `type = "ordination"`.

- view:

  Secondary plot selector used for `type = "ordination"`,
  `type = "admissibility"`, `type = "candidate_review"`,
  `type = "uncertainty_importance"`, `type = "component_importance"`,
  `type = "similarity_tuning"`, and `type = "slope_support"`.

- anchor_model_id:

  Optional anchor model ID for anchor-specific candidate plots. When
  omitted, the first stored anchor is used.

- anchor_species:

  Optional anchor species used when `anchor_model_id` is not supplied.

- include_hulls:

  Logical scalar controlling whether the combined-distance or
  study-distance ordination uses cluster hulls when available.

- ...:

  Unused additional arguments.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
candidates <- build_candidates(config_data_s7)
plot(candidates)
plot(candidates, type = "ordination", dissimilarity = "species", view = "vectors")
plot(candidates, type = "admissibility", view = "overlap_profile")
plot(candidates, type = "candidate_review", view = "similarity_map")
} # }
```
