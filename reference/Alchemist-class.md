# Learned-Distance Alchemist S7 Class

`Alchemist` replaces the Gower-distance tuning pipeline with a
supervised metric-learning approach. A Super Learner is trained to
predict pairwise acoustic distances (log-sigma_bs differences integrated
over the receiving anchor's length distribution) from per-trait Gower
distance features. The resulting N x N learned distance matrix is the
direct input to ordination, admissibility kernel weighting, and policy
selection strategies.

## Details

Construction requires a fully ingested
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object. Configuration is either inherited from the `Candidates` config
(alchemist or similarity section) or supplied directly as a list or YAML
path.

## Properties

- `candidates`: Source
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object.

- `config`: Normalized Alchemist configuration.

- `learner`: Fitted distance learner metadata from
  [`forge_distances()`](https://brandynlucca.github.io/tsbiomass/reference/forge_distances.md).

- `distance_matrix`: Learned model-by-model distance bundle from
  [`forge_distances()`](https://brandynlucca.github.io/tsbiomass/reference/forge_distances.md).

- `trait_importance`: Trait-importance diagnostics from
  [`distill_traits()`](https://brandynlucca.github.io/tsbiomass/reference/distill_traits.md).

- `ordination`: Ordination results from
  [`run_ordination()`](https://brandynlucca.github.io/tsbiomass/reference/run_ordination.md).

- `admissibility`: Admissibility-screen results from
  [`screen_admissibility()`](https://brandynlucca.github.io/tsbiomass/reference/screen_admissibility.md).

## Examples

``` r
if (FALSE) { # \dontrun{
alchemist <- as_alchemist(candidates)
alchemist <- forge_distances(alchemist)
alchemist <- distill_traits(alchemist)
alchemist <- run_ordination(alchemist)
alchemist <- screen_admissibility(alchemist)
selector <- as_policyselector(alchemist)
} # }
```
