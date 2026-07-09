# Build an anchor support audit from selected-policy interval tables

Build an anchor support audit from selected-policy interval tables

## Arguments

- policy_intervals:

  Selected-policy interval table.

- select_ref:

  Policy-selection reference table.

- cover_tbl:

  Policy-level coverage summary table.

- sens_detail:

  Optional sensitivity-detail table.

- sens_tbl:

  Optional full policy-sensitivity table.

- baseline_label:

  Baseline scenario label.

- selector:

  Optional
  [PolicySelector](https://brandynlucca.github.io/tsbiomass/reference/PolicySelector-class.md)
  object used when `policy_intervals` is a
  [PolicyPredictions](https://brandynlucca.github.io/tsbiomass/reference/PolicyPredictions-class.md)
  object.
