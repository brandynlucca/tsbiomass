# Run one Sentinel fold

Runs exactly one row from a Sentinel manifest and returns the fold
output without updating the parent Sentinel manifest or result index.
This is a low-level bridge for external orchestrators such as `targets`;
ordinary interactive use should call
[`run_sentinel()`](https://brandynlucca.github.io/tsbiomass/reference/run_sentinel.md)
instead.

## Usage

``` r
run_sentinel_fold(
  object,
  manifest_row,
  workflow_fn = NULL,
  progress = NULL,
  logging = NULL
)
```

## Arguments

- object:

  A
  [Sentinel](https://brandynlucca.github.io/tsbiomass/reference/Sentinel-class.md)
  object.

- manifest_row:

  One-row Sentinel manifest table, usually from `object@manifest`.

- workflow_fn:

  Optional workflow override. Defaults to the workflow stored on
  `object`.

- progress:

  Optional logical override controlling console progress.

- logging:

  Optional logical override controlling isolated fold logging.

## Value

A list with `ok`, `fold_id`, `row`, and either `fold_result` or
`error_message`.
