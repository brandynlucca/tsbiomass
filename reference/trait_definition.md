# Return one trait definition

Looks up a single trait definition by coded name from the packaged
trait-registry JSON.

## Usage

``` r
trait_definition(coded_name, registry_path = NULL)
```

## Arguments

- coded_name:

  Coded trait name to retrieve.

- registry_path:

  Optional path to a trait-registry JSON file.

## Value

A named list describing the requested trait.
