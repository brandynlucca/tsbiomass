# Read a `write_scorecard()` report

Parses the plain-text report written by
[`write_scorecard()`](https://brandynlucca.github.io/tsbiomass/reference/write_scorecard.md)
back into a tibble, one row per anchor. This is a parser for that
specific fixed format (`## <anchor>` headings, `- Label: value` fields);
it is not a general Markdown reader.

## Usage

``` r
read_scorecard(path)
```

## Arguments

- path:

  Report file path, as written by
  [`write_scorecard()`](https://brandynlucca.github.io/tsbiomass/reference/write_scorecard.md).

## Value

A tibble with one row per anchor and columns `anchor_species`,
`anchor_is_external`, `selected_policy_display`,
`selected_equation_branch_filter`, `selection_tier`,
`realized_donor_fingerprint`, `realized_n_unique_donors`,
`selected_realized_transfer_display`, `selected_donor_model_ids`,
`selected_donor_model_summary`, `selected_donor_model_details`,
`policy_slope_len`, `policy_intercept_len`, `multiplier_pred`,
`meta_post_selection_multiplier_lo`,
`meta_post_selection_multiplier_hi`, `prediction_error_message`,
`meta_q_abs_log_total`.
