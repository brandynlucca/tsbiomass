#' Benchmark candidate policy transfers
#'
#' Evaluates candidate transfer policies against held-out benchmark rows before
#' uncertainty calibration or final policy selection. For a [PolicySelector],
#' this computes policy performance tables, species-block summaries, optional
#' TS-error tables, and cache metadata from the selector's [Candidates] object.
#'
#' The benchmark layer is the empirical evidence used by
#' [calibrate_uncertainty()], [select_policies()], and [as_policylearner()].
#' Running a new benchmark replaces downstream uncertainty and selection state
#' because those layers depend on the benchmark tables.
#'
#' @param object A [PolicySelector] object.
#' @param ... Benchmark controls such as policy overrides, benchmark schemes,
#'   worker count, cache path, refresh flag, or progress display.
#'
#' @return The supplied [PolicySelector] updated with benchmark results.
#'
#' @importFrom methods show
#' @importFrom stats predict simulate
#' @export
benchmark <- S7::new_generic("benchmark", "object")

predict_generic <- S7::new_external_generic("stats", "predict", "object")
simulate_generic <- S7::new_external_generic("stats", "simulate", "object")
plot_generic <- S7::new_external_generic("base", "plot", "x")
print_generic <- S7::new_external_generic("base", "print", "x")
show_generic <- S7::new_external_generic("methods", "show", "object")
summary_generic <- S7::new_external_generic("base", "summary", "object")

#' Dispatch the package S7 prediction generic from internal code
#'
#' @param object Object to predict from.
#' @param ... Arguments passed to the S7 prediction method.
#'
#' @return Method-specific prediction result.
#'
#' @keywords internal
#' @noRd
predict_s7 <- function(object, ...) {
  do.call(predict_generic, c(list(object), list(...)))
}

#' Calibrate policy-transfer uncertainty
#'
#' Builds the conformal uncertainty layer used by downstream policy selection,
#' prediction, and reporting. The generic dispatches on the supplied object:
#' [PolicySelector] methods calibrate interval widths from benchmarked
#' pseudo-anchor and species-block errors, while [PolicyLearner] methods
#' calibrate post-selection intervals from stored cross-fitted learner
#' predictions.
#'
#' Calibration does not refit candidate models or rerun the policy benchmark.
#' It consumes already-computed benchmark or cross-fit tables, estimates
#' residual quantiles at the configured coverage level, and stores the resulting
#' lookup tables, conformal thresholds, diagnostics, and selected calibration
#' rows on the returned object.
#'
#' @param object A [PolicySelector] with benchmark results or a [PolicyLearner]
#'   with cross-fitted prediction results.
#' @param ... Additional calibration controls such as coverage level,
#'   calibration table overrides, cache controls, or learner-specific
#'   support-bin settings.
#'
#' @return The same class of object supplied in `object`, updated with an
#'   uncertainty calibration bundle.
#'
#' @export
calibrate_uncertainty <- S7::new_generic("calibrate_uncertainty", "object")

#' Select benchmark-supported policies
#'
#' Chooses the policy or policies retained for each reference anchor after
#' benchmarking and uncertainty calibration. For a [PolicySelector], selection
#' ranks policies using benchmark error, uncertainty width, donor support, and
#' configured equivalence rules, then stores anchor-level selected rows and
#' diagnostics for prediction and reporting.
#'
#' Selection requires benchmark results and calibrated uncertainty. It does not
#' rebuild candidates or refit learners; it consumes the selector's current
#' benchmark and uncertainty layers.
#'
#' @param object A [PolicySelector] object.
#' @param ... Selection controls such as policy-performance overrides,
#'   tolerance settings, cache path, refresh flag, or progress display.
#'
#' @return The supplied [PolicySelector] updated with selected policies and
#'   selection diagnostics.
#'
#' @export
select_policies <- S7::new_generic("select_policies", "object")

#' Cross-fit a policy learner
#'
#' Trains the policy learner in outer folds and stores out-of-fold predictions
#' for benchmark rows. Cross-fitting estimates how the meta-policy learner
#' behaves on anchors it did not train on, which is the calibration target used
#' later by [calibrate_uncertainty()] for post-selection intervals.
#'
#' The method prepares the benchmark-derived training frame, resolves feature
#' columns and learner controls from the object configuration, fits one learner
#' per fold, and stores fold predictions plus the resolved training settings on
#' the returned [PolicyLearner]. It clears any final fit or calibration state
#' because those depend on the cross-fit results.
#'
#' @param object A [PolicyLearner] object created from a benchmarked
#'   [PolicySelector].
#' @param ... Cross-fitting controls such as benchmark table overrides, fold
#'   count, group column, learner method, feature columns, seed, workers, or
#'   progress display.
#'
#' @return The supplied [PolicyLearner] updated with cross-fit predictions,
#'   prepared training data, and resolved learner settings.
#'
#' @examples
#' \dontrun{
#' learner <- as_policylearner(selector)
#' learner <- crossfit(learner)
#' }
#'
#' @export
crossfit <- S7::new_generic("crossfit", "object")

#' Fit a final policy learner
#'
#' Fits the final learner used for future policy scoring after cross-fitting
#' has established the training recipe and calibration target. For a
#' [PolicyLearner], this trains the configured meta-policy model on the full
#' benchmark-derived training table rather than on held-out folds.
#'
#' The fitted model is stored on the returned learner and is consumed by
#' [stats::predict()] when scoring new anchor-policy rows. Fitting does not
#' perform uncertainty calibration; run [calibrate_uncertainty()] after
#' [fit()] when calibrated intervals are needed.
#'
#' @param object A [PolicyLearner] object, usually after [crossfit()].
#' @param ... Fit controls such as training-data override, learner method,
#'   feature columns, tuning folds, seed, method settings, or progress display.
#'
#' @return The supplied [PolicyLearner] updated with the fitted final
#'   meta-policy learner and resolved fit settings.
#'
#' @examples
#' \dontrun{
#' learner <- as_policylearner(selector)
#' learner <- fit(learner)
#' }
#'
#' @export
fit <- S7::new_generic("fit", "object")

#' Screen a Super Learner library
#'
#' Runs an explicitly configured reduced-fold diagnostic screen for a
#' Super Learner library. For a [PolicyLearner], the method reads
#' `screen_learners` from the requested parent learner section (`selection` or
#' `uncertainty`), inherits the parent learner definition, and returns a
#' [Scorecard] containing fold-level and learner-level diagnostics.
#'
#' This is a screening diagnostic, not a replacement for [crossfit()],
#' [fit()], or Sentinel validation. The returned [Scorecard] can be passed to
#' [update_learners()] to create a new [Configurer] with pruned
#' `super_methods`.
#'
#' @param object A workflow object such as [PolicyLearner].
#' @param ... Screening controls. For [PolicyLearner], these include `stage`,
#'   optional `n_folds`/`seed` overrides, and `progress`.
#'
#' @return A [Scorecard] containing learner-screening diagnostics.
#'
#' @export
screen_learners <- S7::new_generic("screen_learners", "object")

#' Update configured Super Learner libraries from screening scorecards
#'
#' Applies one or more learner-screening [Scorecard] objects to a
#' [Configurer]. The method updates only the parent `super_methods` entries
#' identified by the scorecards, leaving method settings and other learner
#' controls unchanged.
#'
#' @param object A [Configurer].
#' @param ... One or more [Scorecard] objects, or a `scorecards` list.
#'
#' @return A new [Configurer] with updated `selection$super_methods` and/or
#'   `uncertainty$super_methods`.
#'
#' @export
update_learners <- S7::new_generic("update_learners", "object")

#' Update a workflow object's component pieces in place
#'
#' Reconstructs a workflow object, optionally replacing one or more of its
#' component objects while leaving the rest of its state untouched. For a
#' [Referee], this rebuilds the object from its `selector`, `learner`,
#' `predictions`, `config`, and `scorecard` pieces, canonicalizing the
#' distance learner reference along the way.
#'
#' @param object A workflow object such as [Referee].
#' @param ... Component replacements. For [Referee], these are `selector`,
#'   `learner`, `predictions`, `config`, and `scorecard`.
#'
#' @return `object`, reconstructed with any supplied replacement components.
#'
#' @export
update_referee <- S7::new_generic("update_referee", "object")

#' Learn an Alchemist distance matrix
#'
#' Trains the Alchemist's distance learner on pairwise acoustic differences and
#' predicts a model-by-model transfer-distance matrix. The learned matrix is the
#' empirical similarity surface used by admissibility screening, trait
#' importance, ordination, and policy scoring.
#'
#' The method builds pairwise training rows from the object's candidate models,
#' fits the configured Super Learner or single learner, predicts learned
#' distances for all candidate pairs, and stores the fitted learner plus the
#' distance bundle on the returned [Alchemist].
#'
#' @param object An [Alchemist] object with candidate models and configured
#'   similarity traits.
#' @param ... Distance-learner controls such as method choices, feature
#'   overrides, seed, cache controls, or progress display.
#'
#' @return The supplied [Alchemist] updated with a learned distance matrix,
#'   learner state, and distance diagnostics.
#'
#' @export
forge_distances <- S7::new_generic("forge_distances", "object")

#' Distill trait importances from a fitted Alchemist
#'
#' Computes sigma dropout sensitivity for each trait from the fitted distance
#' learner. Dropout sensitivity zeros one feature at a time, re-predicts
#' pairwise distances, and measures the change in kernel-weighted sigma RMSE -
#' the same objective minimised by the empirical similarity tuning step.
#'
#' @param object An [Alchemist] object after [forge_distances()].
#' @param ... Trait-importance controls such as dropout scope, cache controls,
#'   or progress display.
#'
#' @return An updated [Alchemist] object containing trait-importance
#'   diagnostics.
#'
#' @export
distill_traits <- S7::new_generic("distill_traits", "object")

#' Write a Scorecard report
#'
#' Writes one plain-text record per anchor from a [Scorecard]'s selected
#' policies: the selected policy and its branch, the selection tier, the donor
#' models actually used, donor model details when available, the predicted
#' TS-length equation, the biomass multiplier and its interval (or why it is
#' not computable, for external anchors with no baseline TS model), and the
#' total post-selection uncertainty.
#'
#' The format is a small, fixed subset of Markdown (`## <anchor>` headings,
#' `- Label: value` fields) - readable as plain text on its own, and simple
#' enough for [read_scorecard()] to parse back into a tibble. It is not a
#' full diagnostic export; see the `Scorecard` slots directly for the full
#' per-policy tables.
#'
#' @param object A [Scorecard] object.
#' @param ... Method-specific arguments such as `path` and `overwrite`.
#'
#' @return The written path, invisibly.
#'
#' @export
write_scorecard <- S7::new_generic("write_scorecard", "object")
