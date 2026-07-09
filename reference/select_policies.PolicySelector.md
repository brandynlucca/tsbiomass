# Select benchmark-supported policies from a `PolicySelector`

Applies the selector's policy-selection rules to the calibrated
benchmark evidence. The method starts from the species-block performance
table, ranks policies by benchmark error and specificity, applies
equivalence and one-standard-error style tolerance rules, and stores the
selected policy set plus selection diagnostics on the selector.

## Arguments

- object:

  A
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  object.

- species_performance_table:

  Optional species-block performance table override. When omitted, the
  selector's stored benchmark table is used.

- config:

  Optional config override.

- cache_path:

  Optional cache path for reusable selection artifacts.

- refresh:

  Optional logical scalar controlling whether cached selection artifacts
  are ignored and rebuilt.

- progress:

  Optional logical scalar controlling progress messages.

## Value

An updated
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
object with selected policies, selection reference tables, and
anchor-level selection diagnostics.

## Details

This step expects
[`benchmark()`](https://brandynlucca.github.io/tsbiomass/reference/benchmark.md)
and
[`calibrate_uncertainty()`](https://brandynlucca.github.io/tsbiomass/reference/calibrate_uncertainty.md)
to have already populated the selector. It does not rerun policy
evaluation; it reduces the existing benchmark and uncertainty layers to
the policies considered acceptable for prediction and scorecard
reporting.
