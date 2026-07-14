# Emit a timestamped message

Emit a timestamped message

## Usage

``` r
tsb_message(..., timestamp = TRUE, appendLF = TRUE)
```

## Arguments

- ...:

  Message components.

- timestamp:

  Boolean that dictates whether to prepend with timestamp.

- appendLF:

  Passed to [`base::message()`](https://rdrr.io/r/base/message.html).

## Value

Invisibly returns `NULL`.
