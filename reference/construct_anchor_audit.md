# Build the anchor support audit

Combines selected-policy interval rows with policy-level benchmark,
conformal coverage, and optional sensitivity-drift diagnostics. The
audit is an anchor-level support table: it shows which policy was
selected, how that policy performed globally, how well it covered
species-block benchmarks, and whether sensitivity scenarios changed the
recommendation.

## Usage

``` r
construct_anchor_audit(policy_intervals, ...)
```

## Arguments

- policy_intervals:

  Selected-policy interval table or a
  [PolicyPredictions](https://brandynlucca.github.io/tsbiomass/reference/PolicyPredictions-class.md)
  object.

- ...:

  Audit inputs such as selection reference tables, coverage tables,
  sensitivity details, baseline scenario label, or a
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  used to retrieve stored summaries.

## Value

A tibble with one row per audited anchor-policy interval.

## Examples

``` r
if (FALSE) { # \dontrun{
predictions <- predict(selector)
construct_anchor_audit(predictions, selector = selector)
} # }
```
