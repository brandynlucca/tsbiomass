# Write a Scorecard report

Writes one plain-text record per anchor from a
[Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)'s
selected policies: the selected policy and its branch, the selection
tier, the donor models actually used, donor model details when
available, the predicted TS-length equation, the biomass multiplier and
its interval (or why it is not computable, for external anchors with no
baseline TS model), and the total post-selection uncertainty.

## Usage

``` r
write_scorecard(object, ...)
```

## Arguments

- object:

  A
  [Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
  object.

- ...:

  Method-specific arguments such as `path` and `overwrite`.

## Value

The written path, invisibly.

## Details

The format is a small, fixed subset of Markdown (`## <anchor>` headings,
`- Label: value` fields) - readable as plain text on its own, and simple
enough for
[`read_scorecard()`](https://brandynlucca.github.io/tsbiomass/reference/read_scorecard.md)
to parse back into a tibble. It is not a full diagnostic export; see the
`Scorecard` slots directly for the full per-policy tables.
