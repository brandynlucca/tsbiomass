# Learn the distance matrix for an `Alchemist`

Fits the Alchemist distance learner and writes the learned geometry back
to the object. The method expands candidate models into directed model
pairs, constructs trait and coherence features, trains either the
configured ensemble learner or diagonal Mahalanobis learner, and
predicts pairwise transfer distances for the full candidate set.

## Arguments

- object:

  An
  [Alchemist](https://brandynlucca.github.io/tsbiomass/reference/Alchemist-class.md)
  object with candidate models and configured species/study traits.

- progress:

  Optional logical scalar controlling progress messages.

- feature_type:

  Optional pairwise feature representation. Supported values include the
  configured default, `"gower"`, `"difference"`, and `"mahalanobis"`.

- ...:

  Additional learner-specific controls forwarded to the distance fitting
  stage.

## Value

An updated
[Alchemist](https://brandynlucca.github.io/tsbiomass/reference/Alchemist-class.md)
object with fitted learner state and learned distance-matrix outputs.

## Details

The returned object contains the fitted learner and a distance bundle
with the learned matrix, pairwise training data, feature columns, trait
matrices, out-of-fold performance, and sigma matrices used by later
trait-importance and policy-support diagnostics. Re-running this method
clears stale trait-importance, ordination, and admissibility results
because those layers depend on the learned distance geometry.
