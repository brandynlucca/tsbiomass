# Screen `PolicyLearner` base learners

Runs a reduced-fold diagnostic screen for the configured `selection` or
`uncertainty` Super Learner library and returns the results as a regular
[Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md).
The method is intentionally explicit: it only runs when the requested
parent config section contains `screen_learners` or the caller supplies
a screening override such as `n_folds`.

## Arguments

- object:

  A
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md).

- stage:

  Parent learner section to screen: `"selection"` or `"uncertainty"`.

- n_folds:

  Optional explicit screening-fold override. When omitted, the method
  requires `<stage>$screen_learners$n_folds`.

- seed:

  Optional screening seed override. When omitted, the method uses
  `<stage>$screen_learners$seed`, then the parent learner seed.

- progress:

  Optional logical scalar controlling progress messages.

- weight_tolerance:

  Numeric tolerance for treating screening weights as nonzero in the
  scorecard recommendation flag.

- config:

  Optional config override.

## Value

A
[Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
with learner-screening diagnostics.

## Details

The screen inherits the parent learner's method, `super_methods`,
`method_settings`, inner folds, outcome, feature columns, workers, and
other fitting controls. It should not be reported as final validation
evidence and should not be used to tune learner libraries after
inspecting final Sentinel results. Use the returned scorecard with
[`update_learners()`](https://brandynlucca.github.io/tsbiomass/reference/update_learners.md)
to produce a pruned
[Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md),
then run the normal full workflow with that fixed configuration.
