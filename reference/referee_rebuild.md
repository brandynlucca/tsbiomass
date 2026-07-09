# Rebuild a `Referee`

Reconstructs a
[Referee](https://brandynlucca.github.io/tsbiomass/reference/Referee-class.md)
object, optionally replacing one or more of its component objects.

## Usage

``` r
referee_rebuild(
  object,
  selector = NULL,
  learner = NULL,
  predictions = NULL,
  config = NULL,
  scorecard = NULL
)
```

## Arguments

- object:

  A
  [Referee](https://brandynlucca.github.io/tsbiomass/reference/Referee-class.md)
  object.

- selector:

  Optional replacement
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md).

- learner:

  Optional replacement
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md).

- predictions:

  Optional replacement
  [PolicyPredictions](https://brandynlucca.github.io/tsbiomass/reference/PolicyPredictions-class.md).

- config:

  Optional replacement config list.

- scorecard:

  Optional replacement
  [Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md).

## Value

A `Referee` object.
