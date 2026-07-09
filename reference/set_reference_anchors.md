# Set reference anchors on a candidate table or `Candidates` object

Filters a candidate-model table down to an explicit set of
reference-anchor model IDs. When `object` is a
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
instance, the selected anchor rows are stored on the returned object.
When `object` is a data frame, the filtered anchor rows are returned
directly.

## Usage

``` r
set_reference_anchors(
  object = NULL,
  model_ids = NULL,
  selector = NULL,
  model_id_col = "model_id",
  require_selection = TRUE
)
```

## Arguments

- object:

  A candidate-model data frame/tibble or a
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object.

- model_ids:

  Character vector of model IDs to retain as reference anchors.

- selector:

  Optional named list of dynamic anchor-selection rules. A compact
  pipeline-style selector such as `list(regional_body = "SWFSC")`
  performs exact membership matching. Structured rules may also be
  supplied, for example
  `list(regional_body = list(mode = "regex", pattern = "SWFSC"))`.

- model_id_col:

  Name of the model-ID column.

- require_selection:

  Whether zero selected anchors should raise an error.

## Value

If `object` is a data frame, a tibble containing only the selected
reference-anchor rows. If `object` is a
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object, an updated
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object.

## Examples

``` r
anchor_tbl <- set_reference_anchors(
  tibble::tibble(model_id = c("12", "18", "24"), x = 1:3),
  model_ids = c("12", "24")
)

dynamic_anchor_tbl <- set_reference_anchors(
  tibble::tibble(
    model_id = c("12", "18", "24"),
    regional_body = c("SWFSC", "AFSC", "SWFSC")
  ),
  selector = list(regional_body = "SWFSC")
)

if (FALSE) { # \dontrun{
set_reference_anchors(
  candidate_models,
  model_ids = c("12", "18", "24")
)

candidates <- build_candidates(cfg)
candidates <- set_reference_anchors(
  candidates,
  selector = list(regional_body = "SWFSC")
)
fetch_reference_anchors(candidates)

configured_candidates <- build_candidates(list(
  study = list(path = "input.xlsx"),
  anchors = list(
    selector = list(regional_body = "SWFSC")
  )
))
configured_candidates <- set_reference_anchors(configured_candidates)
} # }
```
