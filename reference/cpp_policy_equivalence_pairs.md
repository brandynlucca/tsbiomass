# Compute every paired policy-equivalence bootstrap in one compiled call

Compute every paired policy-equivalence bootstrap in one compiled call

## Usage

``` r
cpp_policy_equivalence_pairs(
  policy_matrix,
  policy,
  branch,
  policy_key,
  tolerance,
  n_boot,
  seed
)
```

## Arguments

- policy_matrix:

  Species-by-policy error matrix ordered like the policy metadata
  vectors.

- policy:

  Policy names.

- branch:

  Equation-branch labels.

- policy_key:

  Stable policy-and-branch keys.

- tolerance:

  Practical equivalence tolerance.

- n_boot:

  Number of paired bootstrap resamples.

- seed:

  Base integer seed.

## Value

Pairwise policy-equivalence data frame.
