#' Select reference-anchor models from a candidate table
#'
#' Filters a candidate-model table down to an explicit set of reference-anchor
#' model IDs. This is the generalized replacement for hard-coded anchor-label
#' filtering such as `grepl("SWFSC", regional_body)`.
#'
#' @param candidate_models Data frame or tibble containing candidate models.
#' @param model_ids Character vector of model IDs to retain as reference
#'   anchors.
#' @param model_id_col Name of the model-ID column in `candidate_models`.
#'
#' @return A tibble containing only the selected reference-anchor rows.
#'
#' @examples
#' \dontrun{
#' set_reference_anchors(
#'   candidate_models,
#'   model_ids = c("12", "18", "24")
#' )
#' }
#'
#' @export
set_reference_anchors <- function(candidate_models,
                                  model_ids,
                                  model_id_col = "model_id") {
  # Validate the candidate table and the model-ID selection inputs before
  # attempting any filtering.
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.character(model_ids)) {
    stop("'model_ids' must be a character vector.", call. = FALSE)
  }

  if (!is.character(model_id_col) || length(model_id_col) != 1 || !nzchar(model_id_col)) {
    stop("'model_id_col' must be a single column name.", call. = FALSE)
  }

  if (!model_id_col %in% names(candidate_models)) {
    stop(
      sprintf("Column '%s' was not found in 'candidate_models'.", model_id_col),
      call. = FALSE
    )
  }

  # Standardize the requested model IDs so blanks and duplicates do not affect
  # the anchor selection.
  model_ids <- stringr::str_squish(model_ids)
  model_ids <- unique(model_ids[!is.na(model_ids) & nzchar(model_ids)])

  if (length(model_ids) == 0) {
    stop("No valid 'model_ids' were supplied.", call. = FALSE)
  }

  candidate_models <- tibble::as_tibble(candidate_models)
  candidate_models[[model_id_col]] <- as.character(candidate_models[[model_id_col]])

  # Retain only the requested model IDs and fail clearly when the resulting
  # anchor set is empty.
  anchor_models <- candidate_models |>
    dplyr::filter(.data[[model_id_col]] %in% model_ids)

  if (nrow(anchor_models) == 0) {
    stop(
      sprintf(
        "No reference-anchor models matched the supplied IDs in column '%s'.",
        model_id_col
      ),
      call. = FALSE
    )
  }

  anchor_models
}

#' Compute conformal calibration scores
#'
#' Summarizes policy-specific absolute log-error quantiles from a benchmark
#' table so multiplicative conformal intervals can be applied later.
#'
#' @param policy_perf Policy-performance table.
#' @param alpha Miscoverage level.
#'
#' @return A tibble.
#'
#' @export
compute_conformal_scores <- function(policy_perf,
                                     alpha = 0.10) {
  # Restrict the calibration set to valid finite policy predictions before
  # summarizing the per-policy absolute log-error distribution.
  {
    policy_perf <- tibble::as_tibble(policy_perf)
    policy_perf$policy <- resolve_policy_names(policy_perf)
    policy_perf
  } |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log)) |>
    dplyr::group_by(policy) |>
    dplyr::summarise(
      n = dplyr::n(),
      q_abs_log = stats::quantile(
        error_abs_log,
        probs = 1 - alpha,
        na.rm = TRUE,
        type = 8
      ),
      median_abs_log = stats::median(error_abs_log, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Summarize conformal performance
#'
#' Summarizes empirical conformal coverage, interval width, and signed/absolute
#' log-error by policy overall and by anchor species.
#'
#' @param policy_perf Policy-performance table.
#' @param conf_cal Conformal calibration table.
#' @param bench_label Benchmark label to attach to the summaries.
#'
#' @return A list with `overall` and `by_species`.
#'
#' @export
summarize_conformal <- function(policy_perf,
                                conf_cal,
                                bench_label = "pseudo_anchor") {
  # Join the per-policy calibration quantiles back onto the benchmark table
  # before computing coverage and interval-width summaries.
  perf_aug <- tibble::as_tibble(policy_perf)
  perf_aug$policy <- resolve_policy_names(perf_aug)
  perf_aug <- perf_aug |>
    dplyr::filter(
      valid_prediction,
      is.finite(multiplier_pred),
      multiplier_pred > 0,
      is.finite(error_abs_log)
    ) |>
    dplyr::left_join(
      tibble::as_tibble(conf_cal) |>
        dplyr::select(policy, q_abs_log),
      by = "policy"
    ) |>
    dplyr::mutate(
      covered = is.finite(q_abs_log) & error_abs_log <= q_abs_log,
      interval_log_width = 2 * q_abs_log,
      signed_log_error = log(multiplier_pred)
    )

  # Summarize the calibrated benchmark at the policy level first.
  overall <- perf_aug |>
    dplyr::group_by(policy) |>
    dplyr::summarise(
      benchmark_label = bench_label,
      n = dplyr::n(),
      empirical_coverage = mean(covered, na.rm = TRUE),
      median_interval_log_width = stats::median(interval_log_width, na.rm = TRUE),
      mean_signed_log_error = mean(signed_log_error, na.rm = TRUE),
      median_signed_log_error = stats::median(signed_log_error, na.rm = TRUE),
      mean_abs_log_error = mean(error_abs_log, na.rm = TRUE),
      median_abs_log_error = stats::median(error_abs_log, na.rm = TRUE),
      .groups = "drop"
    )

  # Repeat the same summary by anchor species so species-level calibration
  # differences can be inspected downstream.
  by_species <- perf_aug |>
    dplyr::group_by(policy, anchor_species) |>
    dplyr::summarise(
      benchmark_label = bench_label,
      n = dplyr::n(),
      empirical_coverage = mean(covered, na.rm = TRUE),
      median_interval_log_width = stats::median(interval_log_width, na.rm = TRUE),
      mean_signed_log_error = mean(signed_log_error, na.rm = TRUE),
      median_signed_log_error = stats::median(signed_log_error, na.rm = TRUE),
      mean_abs_log_error = mean(error_abs_log, na.rm = TRUE),
      median_abs_log_error = stats::median(error_abs_log, na.rm = TRUE),
      .groups = "drop"
    )

  list(overall = overall, by_species = by_species)
}

#' Smooth TS calibration curves
#'
#' Applies spline smoothing to policy-by-relative-length calibration summaries.
#'
#' @param ts_cal Policy-by-relative-length calibration table.
#'
#' @return A tibble.
#'
#' @export
smooth_ts_calibration <- function(ts_cal) {
  # Return early when no TS calibration rows were available to smooth.
  if (nrow(ts_cal) == 0) {
    return(tibble::as_tibble(ts_cal))
  }

  smooth_one <- function(x, y) {
    # Preserve the original values when too few finite points exist for a
    # stable spline fit.
    keep <- is.finite(x) & is.finite(y)
    if (sum(keep) < 4) {
      return(y)
    }

    fit <- tryCatch(
      stats::smooth.spline(x[keep], y[keep], spar = 0.6),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(y)
    }

    out <- y
    out[keep] <- stats::predict(fit, x = x[keep])$y
    out
  }

  # Smooth each policy independently so the relative-length calibration shape
  # is preserved within policy.
  tibble::as_tibble(ts_cal) |>
    dplyr::group_by(policy) |>
    dplyr::arrange(u, .by_group = TRUE) |>
    dplyr::mutate(
      median_ts_error_smooth = smooth_one(u, median_ts_error),
      q05_ts_error_smooth = smooth_one(u, q05_ts_error),
      q95_ts_error_smooth = smooth_one(u, q95_ts_error),
      median_log_sigma_residual_smooth = smooth_one(u, median_log_sigma_residual),
      q10_log_sigma_residual_smooth = smooth_one(u, q10_log_sigma_residual),
      q90_log_sigma_residual_smooth = smooth_one(u, q90_log_sigma_residual),
      q05_log_sigma_residual_smooth = smooth_one(u, q05_log_sigma_residual),
      q95_log_sigma_residual_smooth = smooth_one(u, q95_log_sigma_residual),
      q025_log_sigma_residual_smooth = smooth_one(u, q025_log_sigma_residual),
      q975_log_sigma_residual_smooth = smooth_one(u, q975_log_sigma_residual),
      q005_log_sigma_residual_smooth = smooth_one(u, q005_log_sigma_residual),
      q995_log_sigma_residual_smooth = smooth_one(u, q995_log_sigma_residual)
    ) |>
    dplyr::ungroup()
}

#' Summarize TS conformal calibration
#'
#' Summarizes TS and sigma-residual error by policy and relative length, then
#' smooths the resulting calibration curves.
#'
#' @param ts_error Policy TS-error table.
#'
#' @return A tibble.
#'
#' @keywords internal
summarize_ts_calibration <- function(ts_error) {
  # Return an empty tibble when no TS-length error rows are available.
  if (nrow(ts_error) == 0) {
    return(tibble::tibble())
  }

  # Summarize the raw TS and sigma residuals by policy and relative length
  # before applying the smoother.
  ts_cal <- tibble::as_tibble(ts_error)
  ts_cal$policy <- resolve_policy_names(ts_cal)
  ts_cal <- ts_cal |>
    dplyr::group_by(policy, u) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_ts_error = stats::median(ts_error, na.rm = TRUE),
      q05_ts_error = stats::quantile(ts_error, probs = 0.05, na.rm = TRUE, names = FALSE, type = 8),
      q95_ts_error = stats::quantile(ts_error, probs = 0.95, na.rm = TRUE, names = FALSE, type = 8),
      median_log_sigma_residual = stats::median(log_sigma_residual, na.rm = TRUE),
      q10_log_sigma_residual = stats::quantile(log_sigma_residual, probs = 0.10, na.rm = TRUE, names = FALSE, type = 8),
      q90_log_sigma_residual = stats::quantile(log_sigma_residual, probs = 0.90, na.rm = TRUE, names = FALSE, type = 8),
      q05_log_sigma_residual = stats::quantile(log_sigma_residual, probs = 0.05, na.rm = TRUE, names = FALSE, type = 8),
      q95_log_sigma_residual = stats::quantile(log_sigma_residual, probs = 0.95, na.rm = TRUE, names = FALSE, type = 8),
      q025_log_sigma_residual = stats::quantile(log_sigma_residual, probs = 0.025, na.rm = TRUE, names = FALSE, type = 8),
      q975_log_sigma_residual = stats::quantile(log_sigma_residual, probs = 0.975, na.rm = TRUE, names = FALSE, type = 8),
      q005_log_sigma_residual = stats::quantile(log_sigma_residual, probs = 0.005, na.rm = TRUE, names = FALSE, type = 8),
      q995_log_sigma_residual = stats::quantile(log_sigma_residual, probs = 0.995, na.rm = TRUE, names = FALSE, type = 8),
      .groups = "drop"
    )

  smooth_ts_calibration(ts_cal)
}

#' Run anchor conformal summaries
#'
#' Builds policy-level conformal calibration, benchmark-level conformal
#' coverage summaries, and relative-length TS calibration from pseudo-anchor
#' benchmark outputs.
#'
#' @param policy_perf Pseudo-anchor policy-performance table.
#' @param species_performance_table Optional leave-one-species-out policy-performance table.
#' @param ts_error Optional policy TS-error table.
#' @param alpha Miscoverage level.
#' @param pseudo_label Label for the pseudo-anchor benchmark summary.
#' @param species_label Label for the leave-one-species-out benchmark summary.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#'
#' @return A list containing calibration and summary tables.
#'
#' @export
run_anchor_conformal <- function(policy_perf,
                                 species_performance_table = NULL,
                                 ts_error = NULL,
                                 alpha = 0.10,
                                 pseudo_label = "pseudo_anchor",
                                 species_label = "species_block",
                                 cache_path = NULL,
                                 refresh = FALSE) {
  # Validate the benchmark inputs and cache controls before any conformal
  # summaries are computed.
  if (!is.data.frame(policy_perf)) {
    stop("'policy_perf' must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.null(species_performance_table) && !is.data.frame(species_performance_table)) {
    stop("'species_performance_table' must be NULL or a data frame/tibble.", call. = FALSE)
  }
  if (!is.null(ts_error) && !is.data.frame(ts_error)) {
    stop("'ts_error' must be NULL or a data frame/tibble.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be one finite number strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Reuse the cached conformal object when available unless a refresh was
  # explicitly requested.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  conf_cal <- compute_conformal_scores(
    policy_perf = policy_perf,
    alpha = alpha
  )

  # Build the pseudo-anchor summary first because it is always required.
  pseudo_sum <- summarize_conformal(
    policy_perf = policy_perf,
    conf_cal = conf_cal,
    bench_label = pseudo_label
  )

  # Optionally summarize the species-block benchmark with the same calibration
  # table so the two validation schemes stay directly comparable.
  species_sum <- if (is.null(species_performance_table)) {
    list(overall = tibble::tibble(), by_species = tibble::tibble())
  } else {
    summarize_conformal(
      policy_perf = species_performance_table,
      conf_cal = conf_cal,
      bench_label = species_label
    )
  }

  # Optionally build the relative-length TS calibration summary.
  ts_cal <- if (is.null(ts_error)) {
    tibble::tibble()
  } else {
    summarize_ts_calibration(ts_error)
  }

  result <- list(
    conf_cal = conf_cal,
    pseudo_sum = pseudo_sum,
    species_sum = species_sum,
    overall_sum = dplyr::bind_rows(pseudo_sum$overall, species_sum$overall),
    species_cov = dplyr::bind_rows(pseudo_sum$by_species, species_sum$by_species),
    ts_cal = ts_cal
  )

  # Cache the in-memory conformal summaries only when a cache path was
  # supplied.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  result
}
