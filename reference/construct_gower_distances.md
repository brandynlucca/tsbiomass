# Build Gower distance matrices

Builds the species-level and study-level Gower distance matrices from a
prepared similarity object, expands the species matrix back to model
rows, and combines the two blocks with the prepared alpha value.

## Usage

``` r
construct_gower_distances(similarity, progress = NULL)
```

## Arguments

- similarity:

  Prepared similarity object returned by
  [`prepare_similarities()`](https://brandynlucca.github.io/tsbiomass/reference/prepare_similarities.md)
  or a
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object with prepared similarity state.

- progress:

  Logical scalar controlling stage messages.

## Value

When `similarity` is a
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object, returns that object with the Gower-distance bundle attached.
Otherwise, returns a list containing `species_dist`, `study_dist`,
`species_dist_model`, `combined_dist`, and `trait_cols`.

## Examples

``` r
if (FALSE) { # \dontrun{
candidates <- build_candidates(list(
  study = list(path = "input.xlsx"),
  anchors = list(selector = list(regional_body = "SWFSC"))
))
candidates <- prepare_similarities(candidate_models = candidates)
candidates <- construct_gower_distances(candidates)
candidates
} # }
```
