# Plot a `Sentinel`

Runs `summary(x, type = type)` to build the underlying
[Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
report, then plots it. Equivalent to calling
`plot(summary(x, type = type), ...)` directly, without needing to know
the two-step summarize-then-plot path or the `sentinel_*` type aliases
[plot.Scorecard](https://brandynlucca.github.io/tsbiomass/reference/plot.Scorecard.md)
accepts.

## Usage

``` r
# S3 method for class 'Sentinel'
plot(x, y = NULL, type = "validation", ...)
```

## Arguments

- x:

  A
  [Sentinel](https://brandynlucca.github.io/tsbiomass/reference/Sentinel-class.md)
  object.

- y:

  Unused.

- type:

  Report type: `"validation"`, `"ablation"`, `"ablation_decomposition"`,
  or `"coverage"`.

- ...:

  Additional arguments forwarded to the
  [Scorecard](https://brandynlucca.github.io/tsbiomass/reference/Scorecard-class.md)
  plot method (for example `view`, `anchor_model_id`, `scale`).

## Value

A ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
sentinel <- run_sentinel(object, scenarios)
plot(sentinel, type = "ablation")
} # }
```
