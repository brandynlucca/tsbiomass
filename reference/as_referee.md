# Build a `Referee`

Build a `Referee`

## Usage

``` r
as_referee(selector, learner = NULL, predictions = NULL, config = NULL)
```

## Arguments

- selector:

  A
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  object.

- learner:

  Optional
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
  object used for selector prediction.

- predictions:

  Optional
  [PolicyPredictions](https://brandynlucca.github.io/tsbiomass/reference/PolicyPredictions-class.md)
  bundle. When omitted, the `Referee` can still be advanced later with
  [`predict()`](https://rdrr.io/r/stats/predict.html) to compute it from
  the stored selector and learner.

- config:

  Optional config list or
  [Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
  object.

## Value

A `Referee` object.

## Examples

``` r
if (FALSE) { # \dontrun{
predictions <- predict(selector, learner = learner)
referee <- as_referee(selector, learner = learner, predictions = predictions)
referee
} # }
```
