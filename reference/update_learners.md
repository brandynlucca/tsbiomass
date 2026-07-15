# Update configured Super Learner libraries from screening scorecards

Applies one or more learner-screening
[Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
objects to a
[Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md).
The method updates only the parent `super_methods` entries identified by
the scorecards, leaving method settings and other learner controls
unchanged.

## Usage

``` r
update_learners(object, ...)
```

## Arguments

- object:

  A
  [Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md).

- ...:

  One or more
  [Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
  objects, or a `scorecards` list.

## Value

A new
[Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
with updated `selection$super_methods` and/or
`uncertainty$super_methods`.
