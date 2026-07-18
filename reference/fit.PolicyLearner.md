# Fit a `PolicyLearner`

Trains the final meta-policy learner on the full benchmark-derived
training table. Unlike
[`crossfit()`](https://brandynlucca.github.io/tsbiomass/reference/crossfit.md),
this step does not hold out folds; it uses all available training rows
to fit the model that will score candidate policies for new reference
anchors.

## Arguments

- object:

  A
  [PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
  object.

- training_data:

  Optional prepared learner training table. When omitted, the method
  uses the training table stored during
  [`crossfit()`](https://brandynlucca.github.io/tsbiomass/reference/crossfit.md).

- selection_method:

  Optional meta-policy learner method override.

- feature_cols:

  Optional feature-column override for the final fit.

- outcome_transform:

  Optional outcome transform.

- alpha:

  Optional elastic-net alpha.

- lambda_rule:

  Optional glmnet lambda-selection rule.

- inner_folds:

  Optional number of inner tuning folds.

- seed:

  Optional integer seed.

- selection_super_methods:

  Optional super-learner base methods.

- metalearner_loss:

  Optional super-learner loss name.

- selection_method_settings:

  Optional selection-learner method-settings override.

- method_settings:

  Shared method-settings override.

- workers:

  Optional worker count for the final Super Learner inner OOF task grid.
  When omitted, the value stored by
  [`crossfit()`](https://brandynlucca.github.io/tsbiomass/reference/crossfit.md)
  or configured in the selection stage is used.

- progress:

  Optional logical scalar controlling progress messages.

- config:

  Optional config override.

## Value

An updated
[PolicyLearner](https://brandynlucca.github.io/tsbiomass/reference/PolicyLearner-class.md)
object with the fitted final learner, model metadata, and resolved fit
settings.

## Details

The method reuses the feature columns, outcome transformation, learner
method, and method settings resolved during cross-fitting unless
explicit overrides are supplied. The returned learner stores the fitted
model and clears prediction state that depends on an earlier fit.
