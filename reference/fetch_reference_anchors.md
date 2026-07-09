# Fetch the selected reference anchors

Fetch the selected reference anchors

## Usage

``` r
fetch_reference_anchors(object)
```

## Arguments

- object:

  A
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object.

## Value

Tibble of selected anchor rows with a `length_pdf` status column.

## Examples

``` r
if (FALSE) { # \dontrun{
anchors <- fetch_reference_anchors(candidates)
anchors[, c("model_id", "species_name", "length_pdf")]
} # }
```
