# Screen admissibility across a reference-anchor set

Screens every reference anchor against the candidate-model pool and
returns the bound score, overlap, gate, and anchor-summary tables.

## Usage

``` r
screen_admissibility(
  reference_anchors = NULL,
  candidate_models,
  config = NULL,
  cache_path = NULL,
  refresh = NULL,
  progress = NULL,
  registry_path = NULL
)
```

## Arguments

- reference_anchors:

  Anchor table, typically from
  [`set_reference_anchors()`](https://brandynlucca.github.io/tsbiomass/reference/set_reference_anchors.md).
  When `candidate_models` is a
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object, this may be left `NULL` to use its reference anchors.

- candidate_models:

  Candidate-model table or a
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object.

- config:

  Optional JSON path or list with similarity/anchor settings.

- cache_path:

  Optional `.rds` cache path.

- refresh:

  Logical scalar. If `TRUE`, ignore any existing cache.

- progress:

  Logical scalar controlling stage messages.

- registry_path:

  Optional path to the trait-registry JSON.

## Value

When `candidate_models` is a
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object, returns that object containing the admissibility-screen result.
Otherwise, returns a list containing per-anchor results plus bound
score/summary tables.

## Examples

``` r
if (FALSE) { # \dontrun{
candidates <- build_candidates(list(
  study = list(path = "input.xlsx"),
  anchors = list(selector = list(regional_body = "SWFSC"))
))
candidates <- prepare_similarities(candidates)
candidates <- forge_distances(candidates)
candidates <- screen_admissibility(candidate_models = candidates)
candidates
} # }
```
