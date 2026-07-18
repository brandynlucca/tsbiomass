# Combine externally orchestrated Sentinel folds

Combines outputs from
[`run_sentinel_fold()`](https://brandynlucca.github.io/tsbiomass/reference/run_sentinel_fold.md)
into a Sentinel object, updates the manifest/result index, writes
Sentinel timing tables, and optionally compiles fold logs. This function
exists so external DAG schedulers can own worker allocation while
Sentinel still owns validation result structure.

## Usage

``` r
combine_sentinel_folds(object, fold_outputs, logging = NULL)
```

## Arguments

- object:

  A
  [Sentinel](https://brandynlucca.github.io/tsbiomass/reference/Sentinel-class.md)
  object with a materialized manifest.

- fold_outputs:

  A list of outputs returned by
  [`run_sentinel_fold()`](https://brandynlucca.github.io/tsbiomass/reference/run_sentinel_fold.md).

- logging:

  Optional logical override controlling compiled Sentinel logs.

## Value

An updated
[Sentinel](https://brandynlucca.github.io/tsbiomass/reference/Sentinel-class.md)
object.
