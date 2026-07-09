# Calibrate benchmark-based uncertainty for a `PolicySelector`

Uses the selector's stored benchmark tables to estimate policy-transfer
uncertainty before final policy selection. The method combines
pseudo-anchor policy-performance rows, species-block performance rows,
and optional time-series error rows, then builds conformal calibration
summaries for multiplier intervals and TS-error envelopes.

## Arguments

- object:

  A
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  object.

- alpha:

  Optional conformal alpha. Smaller values produce wider intervals; when
  omitted, the value is read from the selector configuration.

- policy_perf:

  Optional policy-performance table override. By default, the method
  uses the selector's stored benchmark policy-performance table.

- species_performance_table:

  Optional species-block performance table override used to estimate
  species-held-out residual behavior.

- ts_error:

  Optional time-series error table override used for functional TS-error
  envelopes.

- pseudo_label:

  Label assigned to pseudo-anchor calibration rows.

- species_label:

  Label assigned to species-block calibration rows.

- config:

  Optional config override.

- cache_path:

  Optional cache path for reusable calibration artifacts.

- refresh:

  Optional logical scalar controlling whether cached calibration
  artifacts are ignored and rebuilt.

- progress:

  Optional logical scalar controlling progress messages.

## Value

An updated
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
object whose uncertainty property contains conformal thresholds,
calibration rows, TS-error summaries, and diagnostics derived from the
benchmark tables.

## Details

The returned selector keeps the original candidates and benchmark
results and replaces the uncertainty layer with a fresh calibration
bundle. Downstream calls to
[`select_policies()`](https://brandynlucca.github.io/tsbiomass/reference/select_policies.md),
[`stats::predict()`](https://rdrr.io/r/stats/predict.html), and
[`as_referee()`](https://brandynlucca.github.io/tsbiomass/reference/as_referee.md)
use this bundle to attach interval bounds, support diagnostics, and
calibration provenance to selected policies.
