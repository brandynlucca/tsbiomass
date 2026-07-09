# Build TS-error rows for a complete pseudo-anchor benchmark

Build TS-error rows for a complete pseudo-anchor benchmark

## Usage

``` r
cpp_build_benchmark_ts_errors(anchors, policies, grid_size = 11L)
```

## Arguments

- anchors:

  Flat anchor vectors containing IDs, species, standardized
  coefficients, and study-length bounds.

- policies:

  Flat policy-performance vectors containing anchor IDs, policy
  metadata, standardized coefficients, and local diagnostics.

- grid_size:

  Number of equally spaced relative-length points.

## Value

A data frame with one row per valid policy and relative-length point.
