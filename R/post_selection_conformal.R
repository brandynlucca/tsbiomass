#' Compute a post-selection support score
#'
#' @param tbl Selected policy rows.
#'
#' @return Numeric vector; larger values indicate stronger local support.
#'
#' @keywords internal
post_selection_support_score <- function(tbl) {
  tbl_ <- tibble::as_tibble(tbl)
  n <- nrow(tbl_)
  if (n == 0) {
    return(numeric())
  }

  # Pull the available support, distance, and structural-spread fields through
  # one small accessor so the score can work across both benchmark and anchor
  # prediction tables.
  col_or_na <- function(nm) {
    if (nm %in% names(tbl_)) {
      tbl_[[nm]]
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
  n_bins_ <- max(1L, as.integer(n_bins)[[1]])
  switch(as.character(n_bins_),
    `1` = c("All support"),
    `2` = c("Lower support", "Higher support"),
    `3` = c("Lower support", "Moderate support", "Higher support"),
    `4` = c("Lowest support", "Lower support", "Higher support", "Highest support"),
    `5` = c("Lowest support", "Lower support", "Middle support", "Higher support", "Highest support"),
    paste("Support tier", seq_len(n_bins_))
  )
}

resolve_post_selection_support_labels <- function(labels = NULL,
                                                  n_bins = 3L) {
  # Validate the configured labels against the resolved bin count.
  n_bins_ <- max(1L, as.integer(n_bins)[[1]])
  labels_ <- if (is.null(labels)) {
    default_post_selection_support_labels(n_bins_)
  } else {
    labels
  }
  labels_ <- stringr::str_squish(as.character(unlist(labels_, use.names = FALSE)))
  labels_ <- labels_[!is.na(labels_) & nzchar(labels_)]
  if (length(labels_) != n_bins_) {
    stop(
      sprintf(
        "'support_bin_labels' must contain exactly %d non-empty label(s).",
        n_bins_
      ),
      call. = FALSE
    )
  }
  if (anyDuplicated(labels_)) {
    stop("'support_bin_labels' must be unique.", call. = FALSE)
  }
  stats::setNames(labels_, paste0("support_bin_", seq_len(n_bins_)))
}

#' Assign generic support bins
#'
#' @param tbl Input table.
#' @param score Numeric support score vector aligned with `tbl`.
#' @param score_col Name of the output score column.
#' @param bin_col Name of the output support-bin column.
#' @param cutpoints Optional numeric cutpoints. When `NULL`, quantile cutpoints
#'   are computed from `score`.
#' @param n_bins Number of ordered support bins.
#' @param all_bin Label used when all rows collapse into one support tier.
#' @param missing_bin Label used when a row has missing support.
#'
#' @return `tbl` with appended score and bin columns.
#'
#' @keywords internal
assign_support_bins <- function(tbl,
                                score,
                                score_col,
                                bin_col,
                                cutpoints = NULL,
                                n_bins = 3L,
                                all_bin = "all_support",
                                missing_bin = "support_bin_missing") {
  # Standardize the input table and score before deriving cutpoints.
  tbl_ <- tibble::as_tibble(tbl)
  n_bins_ <- max(1L, as.integer(n_bins)[[1]])
  score_ <- suppressWarnings(as.numeric(score))

  if (nrow(tbl_) == 0) {
    tbl_[[score_col]] <- numeric()
    tbl_[[bin_col]] <- character()
    return(tbl_)
  }

  # Build quantile cutpoints unless the caller passed fixed breaks.
  cutpoints_ <- if (is.null(cutpoints)) {
    probs <- seq(0, 1, length.out = n_bins_ + 1L)
    unique(stats::quantile(
      score_,
      probs = probs,
      na.rm = TRUE,
      names = FALSE,
      type = 8
    ))
  } else {
    suppressWarnings(as.numeric(cutpoints))
  }

  # Collapse to one bin when the score has no usable spread.
  if (length(cutpoints_) < 2L || all(!is.finite(cutpoints_))) {
    bin <- rep(all_bin, nrow(tbl_))
  } else {
    cutpoints_[1] <- -Inf
    cutpoints_[length(cutpoints_)] <- Inf
    bin_id <- as.integer(cut(
      score_,
      breaks = cutpoints_,
      include.lowest = TRUE,
      labels = FALSE
    ))
    bin <- paste0("support_bin_", bin_id)
    bin[!is.finite(bin_id)] <- missing_bin
  }

  tbl_[[score_col]] <- score_
  tbl_[[bin_col]] <- bin
  tbl_
}

label_post_selection_support_bins <- function(tbl,
                                              labels = NULL,
                                              n_bins = NULL) {
  # Attach human-readable labels after the structural bins are present.
  tbl_ <- tibble::as_tibble(tbl)
  if (!"post_selection_support_bin" %in% names(tbl_)) {
    return(tbl_)
  }
  bin_values <- as.character(tbl_$post_selection_support_bin)
  n_bins_ <- n_bins %||% max(
    suppressWarnings(as.integer(gsub("^support_bin_", "", bin_values))),
    na.rm = TRUE
  )
  if (!is.finite(n_bins_) || n_bins_ < 1) {
    n_bins_ <- 3L
  }
  label_map <- resolve_post_selection_support_labels(labels = labels, n_bins = n_bins_)
  tbl_$post_selection_support_label <- unname(label_map[bin_values])
  tbl_$post_selection_support_label[is.na(tbl_$post_selection_support_label) & bin_values == "support_bin_missing"] <- "Missing support"
  tbl_$post_selection_support_label[is.na(tbl_$post_selection_support_label) & bin_values == "all_support"] <- "All support"
  tbl_
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
  # Score the rows first, then assign support bins from that score.
  tbl_ <- tibble::as_tibble(tbl)
  if (nrow(tbl_) == 0) {
    tbl_$post_selection_support_score <- numeric()
    tbl_$post_selection_support_bin <- character()
    tbl_$post_selection_support_label <- character()
    return(tbl_)
  }

  n_bins_ <- max(1L, as.integer(n_bins))
  tbl_ <- assign_support_bins(
    tbl = tbl_,
    score = post_selection_support_score(tbl_),
    score_col = "post_selection_support_score",
    bin_col = "post_selection_support_bin",
    cutpoints = cutpoints,
    n_bins = n_bins_,
    all_bin = "all_support",
    missing_bin = "support_bin_missing"
  )
  label_post_selection_support_bins(tbl_, labels = labels, n_bins = n_bins_)
}
