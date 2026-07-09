# Explain one or more policies

Returns the canonical policy identifier, display label, policy family,
candidate-pool definition, aggregation definition, and plain-language
description for one or more policies defined in the policy registry.

## Usage

``` r
explain(policy, policy_path = NULL)
```

## Arguments

- policy:

  Character vector of canonical policy names.

- policy_path:

  Optional path to a policy registry JSON file.

## Value

A tibble with one row per requested policy.

## Examples

``` r
explain(c("closest_within_species", "closest_generalized"))
#> # A tibble: 2 × 11
#>   requested_policy       policy        display_name policy_family candidate_pool
#>   <chr>                  <chr>         <chr>        <chr>         <chr>         
#> 1 closest_within_species closest_with… Closest wit… single_model  same_species  
#> 2 closest_generalized    closest_gene… Closest gen… single_model  generalized_m…
#> # ℹ 6 more variables: aggregation_method <chr>, grouping_key <chr>,
#> #   metric_key <chr>, candidate_pool_definition <chr>,
#> #   aggregation_definition <chr>, plain_language_definition <chr>
```
