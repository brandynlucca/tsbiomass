# Return allowed trait names

Returns the coded trait names defined in the packaged trait-registry
JSON.

## Usage

``` r
trait_names(scope = c("all", "species", "study"), registry_path = NULL)
```

## Arguments

- scope:

  Trait scope to return. Use `"species"`, `"study"`, or `"all"`.

- registry_path:

  Optional path to a trait-registry JSON file.

## Value

A character vector of coded trait names.
