# Sentinel S7 Class

`Sentinel` orchestrates outer-loop validation runs around a
caller-supplied workflow function. It owns the row-level candidate
table, scenario grid, split specification, and the disk-backed manifest
used to resume or collect fold outputs.

## Details

The object is intentionally orchestration-focused. It does not assume a
specific downstream scientific workflow beyond the workflow function
contract accepted by
[`run_sentinel()`](https://brandynlucca.github.io/tsbiomass/reference/run_sentinel.md).
