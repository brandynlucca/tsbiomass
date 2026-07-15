# Screen a Super Learner library

Runs an explicitly configured reduced-fold diagnostic screen for a Super
Learner library. For a
[PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md),
the method reads `screen_learners` from the requested parent learner
section (`selection` or `uncertainty`), inherits the parent learner
definition, and returns a
[Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
containing fold-level and learner-level diagnostics.

## Usage

``` r
screen_learners(object, ...)
```

## Arguments

- object:

  A workflow object such as
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md).

- ...:

  Screening controls. For
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md),
  these include `stage`, optional `n_folds`/`seed` overrides, and
  `progress`.

## Value

A
[Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
containing learner-screening diagnostics.

## Details

This is a screening diagnostic, not a replacement for
[`crossfit()`](https://brandynlucca.github.io/tsbiomass/reference/crossfit.md),
[`fit()`](https://brandynlucca.github.io/tsbiomass/reference/fit.md), or
Sentinel validation. The returned
[Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
can be passed to
[`update_learners()`](https://brandynlucca.github.io/tsbiomass/reference/update_learners.md)
to create a new
[Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
with pruned `super_methods`.
