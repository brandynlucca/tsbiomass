# Update a workflow object's component pieces in place

Reconstructs a workflow object, optionally replacing one or more of its
component objects while leaving the rest of its state untouched. For a
[Referee](https://brandynlucca.github.io/tsbiomass/reference/Referee-class.md),
this rebuilds the object from its `selector`, `learner`, `predictions`,
`config`, and `scorecard` pieces, canonicalizing the distance learner
reference along the way.

## Usage

``` r
update_referee(object, ...)
```

## Arguments

- object:

  A workflow object such as
  [Referee](https://brandynlucca.github.io/tsbiomass/reference/Referee-class.md).

- ...:

  Component replacements. For
  [Referee](https://brandynlucca.github.io/tsbiomass/reference/Referee-class.md),
  these are `selector`, `learner`, `predictions`, `config`, and
  `scorecard`.

## Value

`object`, reconstructed with any supplied replacement components.
