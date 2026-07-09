# Build an Alchemist from a Candidates object

Build an Alchemist from a Candidates object

## Usage

``` r
as_alchemist(candidates, config = NULL, ...)
```

## Arguments

- candidates:

  A
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object with candidate-model rows. Prepared similarity state is used to
  inherit trait names when no explicit config is supplied.

- config:

  Optional alchemist config list, YAML path, or
  [Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
  object. When `NULL`, trait names and learner settings are inherited
  from `candidates`.

- ...:

  Unused.

## Value

An
[Alchemist](https://brandynlucca.github.io/tsbiomass/reference/Alchemist-class.md)
object.
