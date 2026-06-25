#' Finite-sample post-selection conformal quantile
#'
#' @param x Numeric residual vector.
#' @param alpha Miscoverage level.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
post_selection_quantile <- function(x,
                                    alpha) {
  x <- sort(x[is.finite(x)])
  n <- length(x)
  if (n == 0) {
    return(NA_real_)
  }

  q_idx <- ceiling((n + 1) * (1 - alpha))
  if (q_idx > n) {
    return(Inf)
  }

  x[[q_idx]]
}

#' Compute a post-selection support score
#'
#' @param tbl Selected policy rows.
#'
#' @return Numeric vector; larger values indicate stronger local support.
#'
#' @keywords internal
post_selection_support_score <- function(tbl) {
  tbl <- tibble::as_tibble(tbl)
  n <- nrow(tbl)
  if (n == 0) {
    return(numeric())
  }

  # Pull the available support, distance, and structural-spread fields through
  # one small accessor so the score can work across both benchmark and anchor
  # prediction tables.
  col_or_na <- function(nm) {
    if (nm %in% names(tbl)) {
      tbl[[nm]]
    } else {
      rep(NA_real_, n)
    }
  }

  local_distance <- dplyr::coalesce(
    col_or_na("anchor_selection_local_distance"),
    col_or_na("local_weighted_mean_combined_distance"),
    col_or_na("local_min_combined_distance")
  )
  effective_support <- dplyr::coalesce(
    col_or_na("local_effective_support"),
    col_or_na("n_valid_models"),
    col_or_na("n_models")
  )
  if (any(is.finite(effective_support))) {
    score <- dplyr::percent_rank(log1p(effective_support))
    score[!is.finite(score)] <- NA_real_
    return(score)
  }

  n_valid <- dplyr::coalesce(
    col_or_na("n_valid_models"),
    col_or_na("n_models")
  )
  structural_spread <- dplyr::coalesce(
    col_or_na("local_structural_q_abs_log"),
    col_or_na("donor_log_sigma_abs_dev_q90")
  )

  distance_component <- dplyr::percent_rank(-local_distance)
  support_component <- dplyr::percent_rank(log1p(effective_support))
  count_component <- dplyr::percent_rank(log1p(n_valid))
  structural_component <- dplyr::percent_rank(-structural_spread)

  score <- rowMeans(
    cbind(distance_component, support_component, count_component, structural_component),
    na.rm = TRUE
  )
  score[!is.finite(score)] <- NA_real_
  score
}

default_post_selection_support_labels <- function(n_bins) {
  n_bins <- max(1L, as.integer(n_bins)[[1]])
  switch(as.character(n_bins),
    `1` = c("All support"),
    `2` = c("Lower support", "Higher support"),
    `3` = c("Lower support", "Moderate support", "Higher support"),
    `4` = c("Lowest support", "Lower support", "Higher support", "Highest support"),
    `5` = c("Lowest support", "Lower support", "Middle support", "Higher support", "Highest support"),
    paste("Support tier", seq_len(n_bins))
  )
}

resolve_post_selection_support_labels <- function(labels = NULL,
                                                  n_bins = 3L) {
  n_bins <- max(1L, as.integer(n_bins)[[1]])
  if (is.null(labels)) {
    labels <- default_post_selection_support_labels(n_bins)
  }
  labels <- stringr::str_squish(as.character(unlist(labels, use.names = FALSE)))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) != n_bins) {
    stop(
      sprintf(
        "'support_bin_labels' must contain exactly %d non-empty label(s).",
        n_bins
      ),
      call. = FALSE
    )
  }
  if (anyDuplicated(labels)) {
    stop("'support_bin_labels' must be unique.", call. = FALSE)
  }
  stats::setNames(labels, paste0("support_bin_", seq_len(n_bins)))
}

label_post_selection_support_bins <- function(tbl,
                                              labels = NULL,
                                              n_bins = NULL) {
  tbl <- tibble::as_tibble(tbl)
  if (!"post_selection_support_bin" %in% names(tbl)) {
    return(tbl)
  }
  bin_values <- as.character(tbl$post_selection_support_bin)
  n_bins <- n_bins %||% max(
    suppressWarnings(as.integer(gsub("^support_bin_", "", bin_values))),
    na.rm = TRUE
  )
  if (!is.finite(n_bins) || n_bins < 1) {
    n_bins <- 3L
  }
  label_map <- resolve_post_selection_support_labels(labels = labels, n_bins = n_bins)
  tbl$post_selection_support_label <- unname(label_map[bin_values])
  tbl$post_selection_support_label[is.na(tbl$post_selection_support_label) & bin_values == "support_bin_missing"] <- "Missing support"
  tbl$post_selection_support_label[is.na(tbl$post_selection_support_label) & bin_values == "all_support"] <- "All support"
  tbl
}

#' Assign post-selection support bins
#'
#' @param tbl Selected policy rows.
#' @param cutpoints Optional numeric cutpoints. When `NULL`, cutpoints are
#'   estimated from `tbl`.
#' @param n_bins Number of ordered support bins.
#'
#' @return `tbl` with `post_selection_support_score` and
#'   `post_selection_support_bin`.
#'
#' @keywords internal
assign_post_selection_support_bins <- function(tbl,
                                               cutpoints = NULL,
                                               n_bins = 3L,
                                               labels = NULL) {
  tbl <- tibble::as_tibble(tbl)
  if (nrow(tbl) == 0) {
    tbl$post_selection_support_score <- numeric()
    tbl$post_selection_support_bin <- character()
    tbl$post_selection_support_label <- character()
    return(tbl)
  }

  n_bins <- max(1L, as.integer(n_bins))
  score <- post_selection_support_score(tbl)
  if (is.null(cutpoints)) {
    probs <- seq(0, 1, length.out = n_bins + 1L)
    cutpoints <- unique(stats::quantile(
      score,
      probs = probs,
      na.rm = TRUE,
      names = FALSE,
      type = 8
    ))
  }
  if (length(cutpoints) < 2L || all(!is.finite(cutpoints))) {
    bin <- rep("all_support", nrow(tbl))
  } else {
    cutpoints[1] <- -Inf
    cutpoints[length(cutpoints)] <- Inf
    bin_id <- as.integer(cut(
      score,
      breaks = cutpoints,
      include.lowest = TRUE,
      labels = FALSE
    ))
    bin <- paste0("support_bin_", bin_id)
    bin[!is.finite(bin_id)] <- "support_bin_missing"
  }

  tbl$post_selection_support_score <- score
  tbl$post_selection_support_bin <- bin
  label_post_selection_support_bins(tbl, labels = labels, n_bins = n_bins)
}

