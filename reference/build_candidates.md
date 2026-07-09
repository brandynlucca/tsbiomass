# Build a `Candidates` object

Build a `Candidates` object

## Usage

``` r
build_candidates(
  config,
  base_dir = getwd(),
  registry_path = NULL,
  policy_path = NULL
)
```

## Arguments

- config:

  Candidate-ingest config list, YAML path, or
  [Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
  object.

- base_dir:

  Base directory used to resolve relative paths.

- registry_path:

  Optional trait-registry path.

- policy_path:

  Optional policy-registry path, used only when `config` is a
  [Configurer](https://brandynlucca.github.io/tsbiomass/reference/Configurer-class.md)
  YAML/list and must first be validated as such.

## Value

A fully materialized `Candidates` object.

## Examples

``` r
cfg <- list(
  study = list(path = "input.xlsx"),
  sources = list(
    list(id = "worms", type = "remote", engine = "r_package"),
    list(id = "fishbase", type = "remote", engine = "r_package")
  ),
  enrich = list(precedence = c("remote_fishbase", "remote_worms")),
  prepare = list(),
  anchors = list(
    selector = list(regional_body = "SWFSC")
  )
)

if (FALSE) { # \dontrun{
candidates <- build_candidates(cfg)
candidates
} # }
```
