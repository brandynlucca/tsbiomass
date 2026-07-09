# Cross-fit a `PolicyLearner`

Builds the out-of-fold prediction layer for a policy learner. The method
prepares benchmark-derived training rows from the parent selector,
resolves feature columns and learner controls, splits rows by the
requested anchor or group blocking column, and fits the configured
meta-policy learner on each training fold.

## Arguments

- object:

  A
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
  object.

- policy_perf:

  Optional species-block performance table override. When omitted, the
  learner uses the benchmark rows stored on its selector.

- group_col:

  Optional grouping column used for fold blocking so related rows are
  kept in the same outer fold.

- n_folds:

  Optional number of outer cross-validation folds.

- selection_method:

  Optional meta-policy learner method, including `"super_learner"` or
  one of the registered base learner aliases.

- seed:

  Optional integer seed.

- feature_cols:

  Optional feature-column override for the meta-policy learner design
  matrix.

- outcome_col:

  Optional outcome-column override for the target being learned.

- outcome_transform:

  Optional outcome transform.

- lambda_rule:

  Optional glmnet lambda-selection rule.

- alpha:

  Optional elastic-net alpha.

- inner_folds:

  Optional number of inner tuning folds.

- selection_super_methods:

  Optional super-learner base methods.

- metalearner_loss:

  Optional super-learner loss name.

- selection_method_settings:

  Optional selection-learner method-settings override.

- method_settings:

  Shared method-settings override.

- workers:

  Optional worker count.

- progress:

  Optional logical scalar controlling progress messages.

- config:

  Optional config override.

## Value

An updated
[PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
object with stored out-of-fold predictions, prepared training data, and
resolved cross-fit settings.

## Details

The stored cross-fit bundle contains the fold predictions, the prepared
training data, the selected outcome column, feature columns, fold
settings, and active learner methods. Those outputs are required by
[`calibrate_uncertainty()`](https://brandynlucca.github.io/tsbiomass/reference/calibrate_uncertainty.md)
because post-selection uncertainty is estimated from predictions made on
held-out benchmark rows.
