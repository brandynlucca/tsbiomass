# Benchmark a `PolicySelector`

Runs the empirical benchmark stage for a selector. The method evaluates
the active policy set against pseudo-anchor and species-block holdout
schemes, optionally computes TS-error curves, and stores the resulting
policy performance tables on the selector.

## Arguments

- object:

  A
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  object.

- policies:

  Optional character vector of policy names to evaluate.

- policy_fun:

  Optional policy-evaluation function used to score one anchor-policy
  combination.

- curve_fun:

  Optional curve-prediction function used for TS-error evaluation.

- model_scores:

  Optional ordination model-score table override.

- species_lookup:

  Optional ordination species-lookup table override.

- reference_ids:

  Optional reference-anchor id override.

- policy_params:

  Optional policy-parameter overrides applied during policy evaluation.

- policy_path:

  Optional policy-registry path.

- config:

  Optional config override.

- include_ts_error:

  Optional logical scalar controlling time-series error evaluation.

- benchmark_schemes:

  Character vector of benchmark scheme names.

- workers:

  Optional worker count.

- engine:

  Policy benchmark engine, `"r"` or `"cpp"`.

- cache_path:

  Optional cache path for reusable benchmark artifacts.

- refresh:

  Optional logical scalar controlling whether cached benchmark artifacts
  are ignored and rebuilt.

- progress:

  Optional logical scalar controlling progress messages.

- group_block_col:

  Optional blocking column for grouped species holds.

- group_block_label:

  Label attached to grouped holdout rows.

- registry_path:

  Optional trait-registry path.

## Value

An updated
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
object with benchmark policy-performance tables, species-block
summaries, optional TS-error rows, and benchmark metadata.

## Details

Benchmarking consumes the selector's
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object, learned similarity context, active policy configuration, and
reference-anchor identifiers. A new benchmark invalidates any stored
uncertainty calibration and policy selection, so those downstream layers
are cleared on the returned selector.
