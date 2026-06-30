#' Compute the finite-sample conformal quantile
#'
#' Returns the order-statistic conformal threshold used throughout the package.
#'
#' @param x Numeric residual vector.
#' @param alpha Miscoverage level. May be a scalar or numeric vector.
#'
#' @return Numeric scalar when `alpha` has length 1, otherwise a named numeric
#'   vector.
#'
#' @keywords internal
conformal_quantile <- function(x,
                               alpha) {
  # Sort once so vector alpha inputs reuse the same ordered residuals.
  x_ <- sort(x[is.finite(x)])
  n <- length(x_)
  if (n == 0) {
    alpha_ <- suppressWarnings(as.numeric(alpha))
    out <- rep(NA_real_, length(alpha_))
    if (length(out) == 1L) {
      return(NA_real_)
    }
    names(out) <- if (!is.null(names(alpha))) names(alpha) else paste0(
      "alpha_",
      format(alpha_, trim = TRUE, scientific = FALSE)
    )
    return(out)
  }

  # Evaluate one finite-sample conformal order statistic per alpha.
  alpha_ <- suppressWarnings(as.numeric(alpha))
  out <- vapply(
    alpha_,
    function(alpha_now) {
      if (!is.finite(alpha_now)) {
        return(NA_real_)
      }
      q_idx <- ceiling((n + 1) * (1 - alpha_now))
      if (q_idx > n) {
        return(Inf)
      }
      x_[[q_idx]]
    },
    numeric(1)
  )

  if (length(out) == 1L) {
    return(out[[1]])
  }

  names(out) <- if (!is.null(names(alpha))) names(alpha) else paste0(
    "alpha_",
    format(alpha_, trim = TRUE, scientific = FALSE)
  )
  out
}
