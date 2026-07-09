# Create a named Sentinel scenario grid

Create a named Sentinel scenario grid

## Usage

``` r
create_scenarios(
  trait_ablations = NULL,
  gate_ablations = NULL,
  model_ablations = NULL,
  schema_scenarios = NULL,
  baseline_label = "baseline"
)
```

## Arguments

- trait_ablations:

  Optional character vector for automatic one-at-a-time ablations, or a
  named list of character vectors for grouped ablations. Each element
  becomes one scenario with `exclude_similarity_traits` set to that
  vector. The source columns and downstream policy action space remain
  unchanged.

- gate_ablations:

  Optional named list of character vectors. Each element becomes one
  scenario with `exclude_admissibility_traits` set to that vector,
  relaxing the corresponding hard admissibility gate. Distinct from
  `trait_ablations`, which prunes similarity/distance features.

- model_ablations:

  Optional named list of named lists. Each element becomes one scenario
  with `drop_rows` set to the supplied candidate-model column-value
  filters, so matching models are removed from the fold-local train/test
  slices before the workflow runs.

- schema_scenarios:

  Optional named list of additional scenario specs.

- baseline_label:

  Baseline scenario name.

## Value

Named list.

## Examples

``` r
scenarios <- create_scenarios(
  trait_ablations = list(
    no_taxonomy = c("family", "genus")
  ),
  model_ablations = list(
    no_fixed_slope = list(equation_form = "fixed_slope"),
    no_policy_a = list(policy = "policy_a")
  )
)
names(scenarios)
#> [1] "baseline"       "no_taxonomy"    "no_fixed_slope" "no_policy_a"   
scenarios$no_policy_a$drop_rows
#> $policy
#> [1] "policy_a"
#> 
```
