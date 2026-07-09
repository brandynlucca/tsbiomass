# Policy Selection S7 Classes

`PolicySelector` is the object-oriented wrapper around the policy
benchmarking, uncertainty calibration, global policy selection, and
anchor-facing policy prediction layers.

## Details

It is constructed from a
[Candidates](https://brandynlucca.github.io/tsbiomass/reference/Candidates-class.md)
object and then advanced through four high-level methods:

- [`benchmark()`](https://brandynlucca.github.io/tsbiomass/reference/benchmark.md)
  to run the policy benchmark

- [`calibrate_uncertainty()`](https://brandynlucca.github.io/tsbiomass/reference/calibrate_uncertainty.md)
  to calibrate policy intervals

- [`select_policies()`](https://brandynlucca.github.io/tsbiomass/reference/select_policies.md)
  to summarize global policy performance

- [`predict()`](https://rdrr.io/r/stats/predict.html) to generate
  per-anchor policy predictions

The final [`predict()`](https://rdrr.io/r/stats/predict.html) call
returns a
[PolicyPredictions](https://brandynlucca.github.io/tsbiomass/reference/PolicyPredictions-class.md)
object rather than modifying the selector in place.

## Examples

``` r
if (FALSE) { # \dontrun{
selector <- as_policyselector(candidates)
selector <- benchmark(selector)
selector <- calibrate_uncertainty(selector)
selector <- select_policies(selector)
predictions <- predict(selector)
predictions
} # }
```
