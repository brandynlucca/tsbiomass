#' Benchmark a policy selector
#'
#' Runs benchmark evaluation for the supplied object using the method
#' associated with its class.
#'
#' @param object A package object such as a [PolicySelector].
#' @param ... Method-specific arguments.
#'
#' @return The updated object.
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

#' Calibrate uncertainty
#'
#' Dispatches to the appropriate uncertainty-calibration method for the
#' supplied object.
#'
#' @param object A package object such as a [PolicySelector] or
#'   [PolicyLearner].
#' @param ... Method-specific arguments.
#'
#' @return The updated object.
#'
#' @export
calibrate_uncertainty <- S7::new_generic("calibrate_uncertainty", "object")

#' Select policies
#'
#' Dispatches to the appropriate policy-selection method for the supplied
#' object.
#'
#' @param object A package object such as a [PolicySelector].
#' @param ... Method-specific arguments.
#'
#' @return The updated object.
#'
#' @export
select_policies <- S7::new_generic("select_policies", "object")

#' Cross-fit a policy learner
#'
#' Estimates out-of-fold policy-performance predictions for a learner object.
#'
#' @param object A [PolicyLearner] object.
#' @param ... Method-specific arguments.
#'
#' @return The updated [PolicyLearner] object.
#'
#' @examples
#' \dontrun{
#' learner <- as_policylearner(selector)
#' learner <- crossfit(learner)
#' }
#'
#' @export
crossfit <- S7::new_generic("crossfit", "object")

#' Fit a model object
#'
#' Fits the supplied object using the method associated with its class.
#'
#' @param object A package object such as a [PolicyLearner].
#' @param ... Method-specific arguments.
#'
#' @return The updated object.
#'
#' @examples
#' \dontrun{
#' learner <- as_policylearner(selector)
#' learner <- fit(learner)
#' }
#'
#' @export
fit <- S7::new_generic("fit", "object")

#' Learn an Alchemist distance matrix
#'
#' Fits a Super Learner to pairwise acoustic distances and stores the resulting
#' model-by-model learned distance matrix on the object.
#'
#' @param object An [Alchemist] object.
#' @param ... Method-specific arguments.
#'
#' @return An updated [Alchemist] object containing the learned distance
#'   bundle and fitted learner state.
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
#' @param ... Method-specific arguments.
#'
#' @return An updated [Alchemist] object containing trait-importance
#'   diagnostics.
#'
#' @export
distill_traits <- S7::new_generic("distill_traits", "object")
