# Evaluate a complete policy plan for one anchor

Evaluate a complete policy plan for one anchor

## Usage

``` r
cpp_evaluate_policy_plan(
  donors,
  pool_masks,
  plan,
  length_cm,
  pdf_weight,
  anchor_sigma
)
```

## Arguments

- donors:

  Flat donor-vector payload.

- pool_masks:

  Logical matrix: unique pools by donor rows.

- plan:

  Flat compiled policy plan.

- length_cm:

  Anchor PDF length grid.

- pdf_weight:

  Anchor PDF weights.

- anchor_sigma:

  Anchor truth mean backscatter.

## Value

A data frame of core policy predictions and diagnostics.
