# Update candidate metadata by model ID

Applies explicit metadata edits to selected rows of a
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object. Every supplied `model_id` must identify exactly one candidate
model. Matching selected reference-anchor rows are updated in lockstep.
Because candidate metadata determines the prepared similarity and
distance geometry, all derived similarity, distance, ordination, and
admissibility state is cleared.

## Usage

``` r
set_model_metadata(object, updates, model_id_col = "model_id")
```

## Arguments

- object:

  A
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object.

- updates:

  A data frame with a `model_id` column and one or more metadata columns
  to replace. List-columns, including `length_pdf_data`, are supported.

- model_id_col:

  Candidate-model identifier column. Defaults to `"model_id"`.

## Value

Updated
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object with derived state invalidated.

## Examples

``` r
if (FALSE) { # \dontrun{
candidates <- set_model_metadata(
  candidates,
  tibble::tibble(
    model_id = "anchor_model_12",
    slope_standard = 19.3,
    intercept_standard = -65.1
  )
)
} # }
```
