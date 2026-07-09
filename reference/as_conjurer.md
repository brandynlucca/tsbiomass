# Build a `Conjurer`

Build a `Conjurer`

## Usage

``` r
as_conjurer(selector, learner = NULL, config = NULL)
```

## Arguments

- selector:

  A
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  object.

- learner:

  Optional fitted
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md).

- config:

  Optional conjurer config list or
  [Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
  object.

## Value

A `Conjurer` object.

## Examples

``` r
if (FALSE) { # \dontrun{
conjurer <- as_conjurer(selector)
conjurer
} # }
```
