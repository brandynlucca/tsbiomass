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
  # Register S7 methods for external generics such as stats::predict().
  S7::methods_register()

  invisible(NULL)
}
