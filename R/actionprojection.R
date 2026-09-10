#' Project calibrated action uncertainty into TS coefficients and curves
#'
#' These helpers preserve one uncertainty process. Biomass-multiplier endpoints
#' are calibrated first; coefficient and TS-length endpoints are deterministic
#' representations of those same endpoints. No separate coefficient or curve
#' residual model is fitted here.
#'
#' @keywords internal
#' @noRd
action_projection_normalize_weights <- function(weights) {
  weights <- suppressWarnings(as.numeric(weights))
  if (length(weights) < 1L || any(!is.finite(weights)) ||
      any(weights < 0) || sum(weights) <= 0) {
    stop("Anchor length-distribution weights must be finite, nonnegative, and have positive mass.", call. = FALSE)
  }
  weights / sum(weights)
}

#' @keywords internal
#' @noRd
action_projection_log_mean_sigma <- function(slope,
                                             intercept,
                                             log10_length,
                                             weights) {
  acoustic_scale <- log(10) / 10
  linear_predictor <- acoustic_scale *
    (slope * log10_length + intercept)
  maximum <- max(linear_predictor)
  maximum + log(sum(weights * exp(linear_predictor - maximum)))
}

#' @keywords internal
#' @noRd
action_projection_direction <- function(slope,
                                        intercept,
                                        log10_length,
                                        weights,
                                        fixed_slope) {
  if (isTRUE(fixed_slope)) {
    return(c(slope = 0, intercept = 1))
  }
  acoustic_scale <- log(10) / 10
  sigma_weight <- weights * exp(
    acoustic_scale * (slope * log10_length + intercept)
  )
  sigma_weight <- sigma_weight / sum(sigma_weight)
  gradient <- c(
    slope = sum(sigma_weight * log10_length),
    intercept = 1
  )
  gradient / sum(gradient^2)
}

#' @keywords internal
#' @noRd
action_projection_solve_position <- function(target_change,
                                             slope,
                                             intercept,
                                             direction,
                                             log10_length,
                                             weights) {
  target_change <- suppressWarnings(as.numeric(target_change)[[1]])
  if (!is.finite(target_change)) {
    stop("A multiplier endpoint implies a nonfinite backscatter change.", call. = FALSE)
  }
  if (target_change == 0) return(0)
  baseline <- action_projection_log_mean_sigma(
    slope, intercept, log10_length, weights
  )
  objective <- function(position) {
    action_projection_log_mean_sigma(
      slope + position * direction[["slope"]],
      intercept + position * direction[["intercept"]],
      log10_length,
      weights
    ) - baseline - target_change
  }
  bound <- max(1, abs(target_change) / (log(10) / 10))
  bracketed <- FALSE
  for (iteration in seq_len(60L)) {
    endpoint <- c(objective(-bound), objective(bound))
    if (all(is.finite(endpoint)) && endpoint[[1]] * endpoint[[2]] <= 0) {
      bracketed <- TRUE
      break
    }
    bound <- 2 * bound
  }
  if (!bracketed) {
    stop("A biomass endpoint could not be projected onto the selected coefficient branch.", call. = FALSE)
  }
  stats::uniroot(objective, c(-bound, bound), tol = 1e-11)$root
}

#' Project action multiplier intervals onto coefficients and TS-length curves
#'
#' @param intervals Interval table returned in the `intervals` element of
#'   [calibrate_action_marginal_multiplier_intervals()].
#' @param reference_anchors Reference-anchor metadata containing length PDFs.
#' @param fixed_slope_branches Explicit branch identifiers whose slope cannot
#'   vary during projection.
#' @param anchor_col Anchor identifier column shared by both inputs.
#' @param species_col Optional target-species display column.
#' @param slope_col,intercept_col Selected coefficient columns.
#' @param branch_col Selected slope-branch column.
#' @param multiplier_col Selected biomass-multiplier column.
#' @param pdf_col List-column containing anchor length distributions.
#' @param pdf_length_col,pdf_weight_col Columns within each length distribution.
#' @param reference_slope_col,reference_intercept_col Reference equation columns.
#' @param curve_points Number of curve locations across the supported length
#'   interval.
#'
#' @return List containing coefficient intervals, TS-length curves, and exact
#'   endpoint round-trip diagnostics.
#'
#' @keywords internal
#' @noRd
project_action_multiplier_intervals <- function(intervals,
                                                reference_anchors,
                                                fixed_slope_branches,
                                                anchor_col = ".anchor_id",
                                                species_col = "anchor_species",
                                                slope_col = "policy_slope_len",
                                                intercept_col = "policy_intercept_len",
                                                branch_col = "equation_branch_filter",
                                                multiplier_col = "multiplier_pred",
                                                pdf_col = "length_pdf_data",
                                                pdf_length_col = "length_cm",
                                                pdf_weight_col = "f_len",
                                                reference_slope_col = "slope_len",
                                                reference_intercept_col = "intercept_len",
                                                curve_points = 200L) {
  intervals <- tibble::as_tibble(intervals)
  reference_anchors <- tibble::as_tibble(reference_anchors)
  fixed_slope_branches <- as.character(fixed_slope_branches)
  curve_points <- suppressWarnings(as.integer(curve_points)[[1]])
  interval_required <- c(
    anchor_col, "level", slope_col, intercept_col, branch_col,
    multiplier_col, "multiplier_lo", "multiplier_hi"
  )
  reference_required <- c(
    anchor_col, pdf_col, reference_slope_col, reference_intercept_col
  )
  absent_interval <- setdiff(interval_required, names(intervals))
  absent_reference <- setdiff(reference_required, names(reference_anchors))
  if (length(absent_interval) > 0L || length(absent_reference) > 0L) {
    stop(
      paste0(
        "Action projection input is incomplete.",
        if (length(absent_interval) > 0L) paste0(" Intervals: ", paste(absent_interval, collapse = ", "), ".") else "",
        if (length(absent_reference) > 0L) paste0(" References: ", paste(absent_reference, collapse = ", "), ".") else ""
      ),
      call. = FALSE
    )
  }
  if (length(fixed_slope_branches) < 1L || anyNA(fixed_slope_branches) ||
      any(!nzchar(fixed_slope_branches))) {
    stop("Fixed-slope branch identifiers must be supplied explicitly.", call. = FALSE)
  }
  if (is.na(curve_points) || curve_points < 2L) {
    stop("'curve_points' must be an integer of at least two.", call. = FALSE)
  }
  interval_anchor <- as.character(intervals[[anchor_col]])
  reference_anchor <- as.character(reference_anchors[[anchor_col]])
  if (anyNA(interval_anchor) || any(!nzchar(interval_anchor)) ||
      anyNA(reference_anchor) || any(!nzchar(reference_anchor)) ||
      anyDuplicated(reference_anchor)) {
    stop("Anchor identifiers must be complete, and reference-anchor identifiers must be unique.", call. = FALSE)
  }
  reference_index <- match(interval_anchor, reference_anchor)
  if (anyNA(reference_index)) {
    stop("Every action interval requires an explicit reference anchor; no fallback is permitted.", call. = FALSE)
  }
  coefficient_rows <- vector("list", nrow(intervals))
  curve_rows <- vector("list", nrow(intervals))
  roundtrip_rows <- vector("list", nrow(intervals))
  for (i in seq_len(nrow(intervals))) {
    interval <- intervals[i, , drop = FALSE]
    reference <- reference_anchors[reference_index[[i]], , drop = FALSE]
    pdf <- tibble::as_tibble(reference[[pdf_col]][[1]])
    if (!all(c(pdf_length_col, pdf_weight_col) %in% names(pdf))) {
      stop("An anchor length distribution lacks its declared length or weight column.", call. = FALSE)
    }
    length_cm <- suppressWarnings(as.numeric(pdf[[pdf_length_col]]))
    pdf_weight <- suppressWarnings(as.numeric(pdf[[pdf_weight_col]]))
    keep <- is.finite(length_cm) & length_cm > 0 &
      is.finite(pdf_weight) & pdf_weight >= 0
    length_cm <- length_cm[keep]
    pdf_weight <- pdf_weight[keep]
    if (length(length_cm) < 1L) {
      stop("An anchor length distribution has no valid positive lengths.", call. = FALSE)
    }
    ordering <- order(length_cm, method = "radix")
    length_cm <- length_cm[ordering]
    weights <- action_projection_normalize_weights(pdf_weight[ordering])
    log10_length <- log10(length_cm)
    slope <- suppressWarnings(as.numeric(interval[[slope_col]][[1]]))
    intercept <- suppressWarnings(as.numeric(interval[[intercept_col]][[1]]))
    multiplier <- suppressWarnings(as.numeric(interval[[multiplier_col]][[1]]))
    multiplier_endpoints <- suppressWarnings(as.numeric(c(
      interval$multiplier_lo[[1]], interval$multiplier_hi[[1]]
    )))
    if (any(!is.finite(c(slope, intercept, multiplier, multiplier_endpoints))) ||
        multiplier <= 0 || any(multiplier_endpoints <= 0)) {
      stop("Selected coefficients and multiplier endpoints must be finite and positive where required.", call. = FALSE)
    }
    branch <- as.character(interval[[branch_col]][[1]])
    if (is.na(branch) || !nzchar(branch)) {
      stop("Every selected action requires an explicit slope branch.", call. = FALSE)
    }
    fixed_slope <- branch %in% fixed_slope_branches
    direction <- action_projection_direction(
      slope, intercept, log10_length, weights, fixed_slope
    )
    # multiplier = reference mean sigma / policy mean sigma.
    target_changes <- -log(multiplier_endpoints / multiplier)
    positions <- vapply(target_changes, function(target_change) {
      action_projection_solve_position(
        target_change, slope, intercept, direction,
        log10_length, weights
      )
    }, numeric(1))
    endpoint_slope <- slope + positions * direction[["slope"]]
    endpoint_intercept <- intercept + positions * direction[["intercept"]]
    achieved_change <- vapply(seq_along(endpoint_slope), function(j) {
      action_projection_log_mean_sigma(
        endpoint_slope[[j]], endpoint_intercept[[j]],
        log10_length, weights
      ) - action_projection_log_mean_sigma(
        slope, intercept, log10_length, weights
      )
    }, numeric(1))
    curve_length <- seq(min(length_cm), max(length_cm), length.out = curve_points)
    endpoint_curve <- outer(log10(curve_length), endpoint_slope, `*`)
    endpoint_curve <- sweep(endpoint_curve, 2L, endpoint_intercept, `+`)
    selected_curve <- slope * log10(curve_length) + intercept
    reference_slope <- suppressWarnings(as.numeric(reference[[reference_slope_col]][[1]]))
    reference_intercept <- suppressWarnings(as.numeric(reference[[reference_intercept_col]][[1]]))
    if (any(!is.finite(c(reference_slope, reference_intercept)))) {
      stop("A reference anchor lacks finite TS coefficients; no fallback is permitted.", call. = FALSE)
    }
    identity <- tibble::tibble(
      !!anchor_col := interval_anchor[[i]],
      level = as.numeric(interval$level[[1]])
    )
    if (species_col %in% names(intervals)) {
      identity[[species_col]] <- as.character(interval[[species_col]][[1]])
    }
    coefficient_rows[[i]] <- dplyr::bind_cols(
      identity,
      tibble::tibble(
        selected_slope = slope,
        selected_intercept = intercept,
        slope_lo = min(c(endpoint_slope, slope)),
        slope_hi = max(c(endpoint_slope, slope)),
        intercept_lo = min(c(endpoint_intercept, intercept)),
        intercept_hi = max(c(endpoint_intercept, intercept)),
        fixed_slope = fixed_slope,
        direction_slope = direction[["slope"]],
        direction_intercept = direction[["intercept"]]
      )
    )
    curve_rows[[i]] <- dplyr::bind_cols(
      identity[rep(1L, curve_points), , drop = FALSE],
      tibble::tibble(
        length_cm = curve_length,
        ts_selected = selected_curve,
        ts_reference = reference_slope * log10(curve_length) +
          reference_intercept,
        ts_lo = apply(endpoint_curve, 1L, min),
        ts_hi = apply(endpoint_curve, 1L, max)
      )
    )
    roundtrip_rows[[i]] <- dplyr::bind_cols(
      identity,
      tibble::tibble(
        maximum_log_scale_error = max(abs(
          achieved_change - target_changes
        ))
      )
    )
  }
  roundtrip <- dplyr::bind_rows(roundtrip_rows)
  if (any(roundtrip$maximum_log_scale_error > 1e-7)) {
    stop("Biomass-to-curve endpoint projection failed its round-trip check.", call. = FALSE)
  }
  structure(
    list(
      estimand = "deterministic projection of calibrated action-decision multiplier endpoints",
      coefficients = dplyr::bind_rows(coefficient_rows),
      curves = dplyr::bind_rows(curve_rows),
      roundtrip = roundtrip
    ),
    class = "tsb_action_interval_projection"
  )
}
