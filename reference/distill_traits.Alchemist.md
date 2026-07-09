# Distill trait importance for an `Alchemist`

Computes dropout-style trait importance from the fitted Alchemist
distance learner. For each trait group, the method zeros that trait's
pairwise feature columns, re-predicts learned distances, and measures
how much kernel-weighted sigma RMSE changes relative to the full-feature
baseline.

## Arguments

- object:

  An
  [Alchemist](https://brandynlucca.github.io/tsbiomass/reference/Alchemist-class.md)
  object after
  [`forge_distances()`](https://brandynlucca.github.io/tsbiomass/reference/forge_distances.md).

- kernel_scale:

  Positive numeric bandwidth used by the kernel-weighted sigma RMSE
  objective.

- workers:

  Optional worker count for parallel dropout calculations.

- progress:

  Optional logical scalar controlling progress messages.

- ...:

  Reserved for additional trait-importance controls.

## Value

An updated
[Alchemist](https://brandynlucca.github.io/tsbiomass/reference/Alchemist-class.md)
object with trait-importance diagnostics.

## Details

The result identifies which traits materially support the learned
transfer geometry. It requires
[`forge_distances()`](https://brandynlucca.github.io/tsbiomass/reference/forge_distances.md)
because it uses the fitted learner, pairwise training data, donor sigma
matrix, and target sigma vector stored in the Alchemist distance bundle.
