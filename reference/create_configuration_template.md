# Create a configuration template

Builds a pipeline-agnostic baseline configuration with neutral path
placeholders and registry-derived default trait and policy selections.

## Usage

``` r
create_configuration_template(
  input_file = "input.xlsx",
  output_root = "outputs",
  cache_folder = "cache",
  registry_path = NULL,
  policy_path = NULL
)
```

## Arguments

- input_file:

  Input workbook path.

- output_root:

  Output root directory.

- cache_folder:

  Cache folder.

- registry_path:

  Optional trait-registry path used to derive default trait names.

- policy_path:

  Optional policy-registry path used to derive one default active
  policy.

## Value

A config list.
