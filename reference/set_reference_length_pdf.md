# Set user-supplied reference length PDFs

Set user-supplied reference length PDFs

## Usage

``` r
set_reference_length_pdf(object, length_pdf)
```

## Arguments

- object:

  A
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object.

- length_pdf:

  Named list keyed by selected reference `model_id`.

## Value

Updated
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object.

## Examples

``` r
if (FALSE) { # \dontrun{
candidates <- set_reference_length_pdf(
  candidates,
  length_pdf = list("12" = c(10, 11, 12, 12, 13))
)
} # }
```
