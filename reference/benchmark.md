# Benchmark candidate policy transfers

Evaluates candidate transfer policies against held-out benchmark rows
before uncertainty calibration or final policy selection. For a
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md),
this computes policy performance tables, species-block summaries,
optional TS-error tables, and cache metadata from the selector's
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object.

## Usage

``` r
benchmark(object, ...)
```

## Arguments

- object:

  A
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  object.

- ...:

  Benchmark controls such as policy overrides, benchmark schemes, worker
  count, cache path, refresh flag, or progress display.

## Value

The supplied
[PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
updated with benchmark results.

## Details

The benchmark layer is the empirical evidence used by
[`calibrate_uncertainty()`](https://brandynlucca.github.io/tsbiomass/reference/calibrate_uncertainty.md),
[`select_policies()`](https://brandynlucca.github.io/tsbiomass/reference/select_policies.md),
and
[`as_policylearner()`](https://brandynlucca.github.io/tsbiomass/reference/as_policylearner.md).
Running a new benchmark replaces downstream uncertainty and selection
state because those layers depend on the benchmark tables.
