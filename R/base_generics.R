#' Benchmark a staged workflow object
#'
#' Dispatches to the appropriate benchmark method for the supplied staged
#' workflow object.
#'
#' @param object A staged workflow object such as a [PolicySelector].
#' @param ... Method-specific arguments.
#'
#' @return An updated staged workflow object.
#'
#' @export
benchmark <- S7::new_generic("benchmark", "object")

predict_generic <- S7::new_external_generic("stats", "predict", "object")
simulate_generic <- S7::new_external_generic("stats", "simulate", "object")
plot_generic <- S7::new_external_generic("base", "plot", "x")
print_generic <- S7::new_external_generic("base", "print", "x")
show_generic <- S7::new_external_generic("methods", "show", "object")

#' Calibrate uncertainty on a staged workflow object
#'
#' Dispatches to the appropriate uncertainty-calibration method for the
#' supplied staged workflow object.
#'
#' @param object A staged workflow object such as a [PolicySelector] or
#'   [PolicyLearner].
#' @param ... Method-specific arguments.
#'
#' @return An updated staged workflow object.
#'
#' @export
calibrate_uncertainty <- S7::new_generic("calibrate_uncertainty", "object")

#' Select policies from a staged workflow object
#'
#' Dispatches to the appropriate policy-selection method for the supplied
#' staged workflow object.
#'
#' @param object A staged workflow object such as a [PolicySelector].
#' @param ... Method-specific arguments.
#'
#' @return An updated staged workflow object.
#'
#' @export
select_policies <- S7::new_generic("select_policies", "object")

#' Cross-fit a staged workflow object
#'
#' Cross-fits the supplied staged workflow object using the method associated
#' with its class.
#'
#' @param object A staged workflow object such as a [PolicyLearner].
#' @param ... Method-specific arguments.
#'
#' @return An updated staged workflow object.
#'
#' @examples
#' \dontrun{
#' learner <- as_policy_learner(selector)
#' learner <- crossfit(learner)
#' }
#'
#' @export
crossfit <- S7::new_generic("crossfit", "object")

#' Fit a staged workflow object
#'
#' Fits the supplied staged workflow object using the method associated with its
#' class.
#'
#' @param object A staged workflow object such as a [PolicyLearner].
#' @param ... Method-specific arguments.
#'
#' @return An updated staged workflow object.
#'
#' @examples
#' \dontrun{
#' learner <- as_policy_learner(selector)
#' learner <- fit(learner)
#' }
#'
#' @export
fit <- S7::new_generic("fit", "object")

#' Forge a learned distance matrix from an Alchemist
#'
#' Fits a Super Learner to pairwise acoustic distances and stores the resulting
#' N x N learned distance matrix on the object.
#'
#' @param object An [Alchemist] object.
#' @param ... Method-specific arguments.
#'
#' @return An updated [Alchemist] object with `@distance_matrix` and `@learner`
#'   populated.
#'
#' @export
forge_distances <- S7::new_generic("forge_distances", "object")

#' Distill trait importances from a fitted Alchemist
#'
#' Computes permutation importance for each trait from the fitted Super Learner
#' and derives the data-driven alpha-equivalent (species vs. survey balance).
#'
#' @param object An [Alchemist] object with `@learner` populated.
#' @param ... Method-specific arguments.
#'
#' @return An updated [Alchemist] object with `@trait_importance` populated.
#'
#' @export
distill_traits <- S7::new_generic("distill_traits", "object")


