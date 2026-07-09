# Run an NMDS ordination

Runs a two-dimensional NMDS on a distance matrix and optionally fits
trait vectors and factor centroids with
[`vegan::envfit()`](https://vegandevs.github.io/vegan/reference/envfit.html).

## Usage

``` r
run_ordination(
  dist_mat,
  trait_table = NULL,
  nmds_args = NULL,
  include_loadings = NULL,
  include_centroids = NULL,
  envfit_args = NULL,
  reference_ids = NULL,
  join_cols = c("species_name", "common", "swimbladder_type", "family", "regional_body",
    "is_group_model"),
  cluster_args = list(),
  species_cluster_args = list(cluster_col = "species_cluster_id"),
  species_refine_args = list(cluster_col = "species_cluster_id"),
  model_id_col = "model_id",
  progress = NULL
)
```

## Arguments

- dist_mat:

  A distance matrix, `dist` object, or a
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object with constructed Gower distances.

- trait_table:

  Optional trait table aligned to `dist_mat`.

- nmds_args:

  Optional named list of arguments passed to
  [`vegan::metaMDS()`](https://vegandevs.github.io/vegan/reference/metaMDS.html).

- include_loadings:

  Logical scalar. If `TRUE`, return envfit vector loadings for numeric
  traits.

- include_centroids:

  Logical scalar. If `TRUE`, return envfit centroids for factor traits.

- envfit_args:

  Optional named list of arguments passed to
  [`vegan::envfit()`](https://vegandevs.github.io/vegan/reference/envfit.html).

- reference_ids:

  Optional vector of reference model IDs. When `dist_mat` is a
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object and `reference_ids` is `NULL`, the stored reference-anchor
  table is used automatically.

- join_cols:

  Additional model metadata columns to join onto the stored model-level
  ordination points when `dist_mat` is a
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object.

- cluster_args:

  Optional named list passed to `assign_ordination_groups()` for the
  model-level ordination.

- species_cluster_args:

  Optional named list passed to `assign_ordination_groups()` for the
  species-level ordination.

- species_refine_args:

  Optional named list passed to `refine_species_clusters()` for the
  species-level ordination.

- model_id_col:

  Model-ID column name used when `dist_mat` is a
  [Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
  object.

- progress:

  Logical scalar controlling stage messages.

## Value

When `dist_mat` is a
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object, returns that object with a downstream-ready ordination bundle.
Otherwise, returns a list with `ordination`, `points`, `loadings`, and
`centroids`.

## Examples

``` r
if (FALSE) { # \dontrun{
candidates <- build_candidates(list(
  study = list(path = "input.xlsx"),
  anchors = list(selector = list(regional_body = "SWFSC"))
))
candidates <- prepare_similarities(candidate_models = candidates)
candidates <- construct_gower_distances(candidates)
candidates <- run_ordination(candidates)
candidates
} # }
```
