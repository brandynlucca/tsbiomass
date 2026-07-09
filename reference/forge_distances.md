# Learn an Alchemist distance matrix

Trains the Alchemist's distance learner on pairwise acoustic differences
and predicts a model-by-model transfer-distance matrix. The learned
matrix is the empirical similarity surface used by admissibility
screening, trait importance, ordination, and policy scoring.

## Usage

``` r
forge_distances(object, ...)
```

## Arguments

- object:

  An
  [Alchemist](https://brandynlucca.github.io/tsbiomass/reference/Alchemist-class.md)
  object with candidate models and configured similarity traits.

- ...:

  Distance-learner controls such as method choices, feature overrides,
  seed, cache controls, or progress display.

## Value

The supplied
[Alchemist](https://brandynlucca.github.io/tsbiomass/reference/Alchemist-class.md)
updated with a learned distance matrix, learner state, and distance
diagnostics.

## Details

The method builds pairwise training rows from the object's candidate
models, fits the configured Super Learner or single learner, predicts
learned distances for all candidate pairs, and stores the fitted learner
plus the distance bundle on the returned
[Alchemist](https://brandynlucca.github.io/tsbiomass/reference/Alchemist-class.md).
