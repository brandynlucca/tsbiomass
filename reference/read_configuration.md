# Read a configuration YAML file

Reads, validates, and normalizes a caller-supplied config YAML file at
ingestion time.

## Usage

``` r
read_configuration(
  path,
  base_dir = dirname(path_absolute(path)),
  registry_path = NULL,
  policy_path = NULL
)
```

## Arguments

- path:

  Config YAML path.

- base_dir:

  Base directory used to resolve relative paths. Defaults to the YAML
  file directory.

- registry_path:

  Optional trait-registry path.

- policy_path:

  Optional policy-registry path.

## Value

A validated normalized config list.
