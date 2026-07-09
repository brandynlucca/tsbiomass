# Run Sentinel outer-loop validation

Run Sentinel outer-loop validation

## Usage

``` r
run_sentinel(
  object,
  workflow_fn = NULL,
  refresh = FALSE,
  max_folds = NULL,
  progress = NULL,
  workers = NULL,
  logging = NULL
)
```

## Arguments

- object:

  A
  [Sentinel](https://brandynlucca.github.io/tsbiomass/reference/Sentinel-class.md)
  object.

- workflow_fn:

  Optional workflow override.

- refresh:

  Logical scalar. When `TRUE`, rerun all folds and reset manifest
  status.

- max_folds:

  Optional integer cap for this invocation.

- progress:

  Optional logical override controlling console progress from both the
  parent and outer workers.

- workers:

  Optional number of outer-fold workers. Defaults to the Sentinel worker
  option.

- logging:

  Optional logical override controlling isolated per-fold logs and the
  compiled Sentinel log.

## Value

An updated
[Sentinel](https://brandynlucca.github.io/tsbiomass/reference/Sentinel-class.md)
object.
