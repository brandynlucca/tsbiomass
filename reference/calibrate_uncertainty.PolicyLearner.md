# Calibrate post-selection uncertainty for a `PolicyLearner`

Converts cross-fitted learner predictions into post-selection conformal
calibration tables. The method scores the stored out-of-fold
predictions, keeps the rows that would be selected under the learner's
meta-policy score, estimates absolute log-residual quantiles, and
optionally builds support-bin calibration so intervals can widen when an
anchor has little local support.

## Arguments

- object:

  A
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
  object.

- predictions:

  Optional cross-fit prediction table override. When omitted, stored
  cross-fit predictions are used.

- outcome_col:

  Optional outcome-column override used to compute residuals.

- max_selection_tolerance:

  Optional score-tie tolerance used when retaining calibration rows.

- alpha:

  Optional marginal conformal alpha for the global post-selection
  residual threshold.

- bin_alpha:

  Optional support-bin conformal alpha for local-support residual
  thresholds.

- min_bin_scores:

  Optional minimum score count required before a support bin gets its
  own threshold.

- n_bins:

  Optional number of support bins used to stratify selected rows.

- uncertainty_method:

  Optional uncertainty-learner method override.

- uncertainty_super_methods:

  Optional uncertainty-learner super-learner base methods override.

- uncertainty_method_settings:

  Optional uncertainty-learner method-settings override.

- uncertainty_crossfit_result:

  Optional precomputed uncertainty learner cross-fit result from an
  external task scheduler. When `NULL`, the method runs the configured
  uncertainty learner normally.

- progress:

  Optional logical scalar controlling progress messages.

- config:

  Optional config override.

## Value

An updated
[PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
object whose calibration property contains selected calibration rows,
residual quantiles, support-bin thresholds, optional uncertainty learner
state, and lookup metadata used by
[`stats::predict()`](https://rdrr.io/r/stats/predict.html) on the
learner.

## Details

This method expects
[`crossfit()`](https://brandynlucca.github.io/tsbiomass/reference/crossfit.md)
to have populated the learner's cross-fit results. It does not refit the
meta-policy learner; it uses the cross-fitted predictions to quantify
how much post-selection error remains after the learner chooses
policies.
