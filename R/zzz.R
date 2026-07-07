#' Register S7 external methods when the package loads
#'
#' @param libname Library path supplied by the package loader.
#' @param pkgname Package name supplied by the package loader.
#'
#' @return Invisible `NULL`.
#'
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)

  # Ensure the package S7 classes are registered with S4 before external
  # method registration runs during namespace load in an installed package.
  for (class_name in c(
    "Configurer",
    "Candidates",
    "Alchemist",
    "Conjurer",
    "PolicyPredictions",
    "PolicySelector",
    "PolicyLearner",
    "PolicySimulator",
    "Scorecard",
    "Referee",
    "Sentinel"
  )) {
    class_obj <- get0(class_name, envir = ns, inherits = FALSE)
    if (!is.null(class_obj)) {
      suppressWarnings(S7::S4_register(class_obj))
    }
  }

  # Register S7 methods for external generics such as stats::predict().
  S7::methods_register()

  # Register tibble coercions for namespaced S7 classes. These classes expose a
  # single canonical rectangular result, so `tibble::as_tibble()` is a useful
  # extraction path without flattening the prepared workflow objects.
  if (requireNamespace("tibble", quietly = TRUE)) {
    registerS3method(
      "as_tibble",
      "tsbiomass::PolicyPredictions",
      as_tibble_policy_predictions,
      envir = asNamespace("tibble")
    )
    registerS3method(
      "as_tibble",
      "tsbiomass::Scorecard",
      as_tibble_scorecard,
      envir = asNamespace("tibble")
    )
    registerS3method(
      "as_tibble",
      "tsbiomass::Conjurer",
      as_tibble_conjurer,
      envir = asNamespace("tibble")
    )
  }

  invisible(NULL)
}
