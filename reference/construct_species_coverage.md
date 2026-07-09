# Build a species-block coverage table

Extracts empirical coverage and interval-width summaries for
species-block benchmark rows. The helper accepts either stored conformal
summary lists or a
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
with calibrated uncertainty and returns the policy-level coverage view
used by scorecards and anchor audits.

## Usage

``` r
construct_species_coverage(coverage_summary, ...)
```

## Arguments

- coverage_summary:

  Pseudo-anchor conformal summary list or a
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  object.

- ...:

  Additional inputs such as the species-block conformal summary or
  benchmark label when `coverage_summary` is a list.

## Value

A tibble with policy identifiers, branch filters, empirical coverage,
and median interval log width.

## Examples

``` r
if (FALSE) { # \dontrun{
construct_species_coverage(selector)
} # }
```
