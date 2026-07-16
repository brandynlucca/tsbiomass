# Update a `Configurer` from learner-screening scorecards

Applies one or more learner-screening
[Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
objects to a
[Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
by replacing the corresponding parent `super_methods` vector with the
methods marked `recommended_keep` in each scorecard. Only
`selection$super_methods` and `uncertainty$super_methods` are modified;
learner method settings, folds, outcomes, and all other config entries
are preserved.

## Arguments

- object:

  A
  [Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md).

- ...:

  One or more
  [Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
  objects.

- scorecards:

  Optional list of
  [Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
  objects.

- stages:

  Optional stage filter. Defaults to all stages represented in the
  scorecards.

- progress:

  Logical scalar. If `TRUE`, emit update messages.

## Value

A new
[Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
with pruned Super Learner method libraries.
