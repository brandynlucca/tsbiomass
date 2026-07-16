# Plot an `Alchemist`

Dispatches `plot(alchemist, ...)` to one of several figure families
based on `type`. Mirrors the `plot.Candidates` interface so both object
types behave consistently at the REPL.

## Usage

``` r
# S3 method for class 'Alchemist'
plot(
  x,
  y = NULL,
  type = c("ordination", "trait_importance", "admissibility"),
  dissimilarity = c("combined", "species"),
  view = NULL,
  include_hulls = TRUE,
  ...
)
```

## Arguments

- x:

  An
  [Alchemist](https://brandynlucca.github.io/tsbiomass/reference/Alchemist-class.md)
  object.

- y:

  Unused.

- type:

  Figure family to draw. `"ordination"` requires a prior call to
  [`run_ordination()`](https://brandynlucca.github.io/tsbiomass/reference/run_ordination.md);
  `"trait_importance"` requires
  [`distill_traits()`](https://brandynlucca.github.io/tsbiomass/reference/distill_traits.md);
  `"admissibility"` requires
  [`screen_admissibility()`](https://brandynlucca.github.io/tsbiomass/reference/screen_admissibility.md).

- dissimilarity:

  Ordination dissimilarity view. `"combined"` plots the model-level
  ordination; `"species"` plots the species-level ordination.

- view:

  Secondary plot selector for `type = "ordination"` or
  `type = "admissibility"`. One of `"clusters"`, `"cluster_hulls"`,
  `"vectors"`, or `"centers"` for combined ordination; species
  ordination also accepts `"overview"`. For admissibility, one of
  `"gate_composition"` or `"overlap_profile"`.

- include_hulls:

  Logical. When `TRUE` (default) and hull data are available,
  `view = NULL` defaults to `"cluster_hulls"` for `type = "ordination"`.

- ...:

  Unused additional arguments.

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
alchemist <- run_ordination(alchemist)
plot(alchemist)
plot(alchemist, type = "ordination", view = "vectors")
alchemist <- distill_traits(alchemist)
plot(alchemist, type = "trait_importance")
} # }
```
