#' Collapse values for compact display
#'
#' @param values Character vector.
#' @param limit Maximum number of values to display.
#'
#' @return Character scalar.
#'
#' @keywords internal
preview_values <- function(values,
                           limit = 5L) {
  values <- unique(as.character(values %||% character(0)))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) {
    return("none")
  }
  if (length(values) <= limit) {
    return(paste(values, collapse = ", "))
  }

  paste0(
    paste(values[seq_len(limit)], collapse = ", "),
    ", ... (",
    length(values),
    " total)"
  )
}
