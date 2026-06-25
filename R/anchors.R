#' Filter reference-anchor rows from a candidate table
#'
#' @param candidate_models Data frame or tibble containing candidate models.
#' @param model_ids Character vector of model IDs to retain as reference
#'   anchors.
#' @param model_id_col Name of the model-ID column in `candidate_models`.
#'
#' @return A tibble containing only the selected reference-anchor rows.
#'
#' @keywords internal
filter_reference_anchor_rows <- function(candidate_models,
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

#' Normalize one anchor-selection rule
#'
#' @param rule Anchor-selection rule.
#' @param field_name Field label used in error messages.
#'
#' @return A normalized rule list.
#'
#' @keywords internal
normalize_anchor_selector_rule <- function(rule,
                                           field_name) {
  # Support both the compact pipeline-style form `list(field = value)` and a
  # richer structured rule form with explicit matching options.
  if (!is.list(rule) || is.null(names(rule))) {
    return(list(
      mode = "in",
      values = rule,
      ignore_case = FALSE,
      negate = FALSE,
      require_non_missing = TRUE
    ))
  }

  mode <- as.character(rule$mode %||% "in")[[1]]
  mode <- stringr::str_to_lower(stringr::str_squish(mode))
  if (!mode %in% c("in", "regex", "fixed")) {
    stop(
      sprintf(
        "Anchor selector rule '%s' has unsupported mode '%s'.",
        field_name,
        mode
      ),
      call. = FALSE
    )
  }

  values <- rule$values %||% rule$value %||% rule$pattern %||% NULL
  if (mode %in% c("in", "fixed") && is.null(values)) {
    stop(
      sprintf("Anchor selector rule '%s' must supply 'values'.", field_name),
      call. = FALSE
    )
  }
  if (mode == "regex" && is.null(values)) {
    stop(
      sprintf("Anchor selector rule '%s' must supply 'pattern' or 'value'.", field_name),
      call. = FALSE
    )
  }

  list(
    mode = mode,
    values = values,
    ignore_case = isTRUE(rule$ignore_case),
    negate = isTRUE(rule$negate),
    require_non_missing = !isFALSE(rule$require_non_missing)
  )
}

#' Build a logical mask from an anchor selector
#'
#' @param candidate_models Data frame or tibble containing candidate models.
#' @param selector Named list of anchor-selection rules.
#'
#' @return Logical vector with one element per row in `candidate_models`.
#'
#' @keywords internal
reference_anchor_selector_mask <- function(candidate_models,
                                           selector) {
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.list(selector) || length(selector) == 0) {
    stop("'selector' must be a non-empty named list.", call. = FALSE)
  }
  if (is.null(names(selector)) || any(is.na(names(selector))) || any(!nzchar(names(selector)))) {
    stop("'selector' must be a named list.", call. = FALSE)
  }

  candidate_models <- tibble::as_tibble(candidate_models)
  keep <- rep(TRUE, nrow(candidate_models))

  # Apply every configured rule cumulatively so the final anchor set reflects
  # the intersection of the requested column-level constraints.
  for (field_name in names(selector)) {
    if (!field_name %in% names(candidate_models)) {
      stop(
        sprintf("Anchor selector field '%s' was not found in 'candidate_models'.", field_name),
        call. = FALSE
      )
    }

    rule <- normalize_anchor_selector_rule(selector[[field_name]], field_name)
    values <- rule$values
    column <- candidate_models[[field_name]]
    match_vec <- rep(FALSE, length(column))

    if (rule$mode == "in") {
      values_chr <- as.character(values)
      values_chr <- stringr::str_squish(values_chr)
      values_chr <- unique(values_chr[!is.na(values_chr) & nzchar(values_chr)])
      if (length(values_chr) == 0) {
        stop(
          sprintf("Anchor selector field '%s' produced no valid values.", field_name),
          call. = FALSE
        )
      }

      column_chr <- stringr::str_squish(as.character(column))
      if (isTRUE(rule$ignore_case)) {
        match_vec <- !is.na(column_chr) &
          stringr::str_to_lower(column_chr) %in% stringr::str_to_lower(values_chr)
      } else {
        match_vec <- !is.na(column_chr) & column_chr %in% values_chr
      }
    } else if (rule$mode == "fixed") {
      values_chr <- as.character(values)
      values_chr <- values_chr[!is.na(values_chr)]
      if (length(values_chr) == 0) {
        stop(
          sprintf("Anchor selector field '%s' produced no valid fixed patterns.", field_name),
          call. = FALSE
        )
      }

      column_chr <- as.character(column)
      for (pattern in values_chr) {
        match_vec <- match_vec | (
          !is.na(column_chr) &
            stringr::str_detect(
              string = column_chr,
              pattern = stringr::fixed(pattern, ignore_case = rule$ignore_case)
            )
        )
      }
    } else if (rule$mode == "regex") {
      pattern <- as.character(values)[[1]]
      if (is.na(pattern) || !nzchar(pattern)) {
        stop(
          sprintf("Anchor selector field '%s' must supply a non-empty regex pattern.", field_name),
          call. = FALSE
        )
      }

      column_chr <- as.character(column)
      match_vec <- !is.na(column_chr) &
        stringr::str_detect(
          string = column_chr,
          pattern = stringr::regex(pattern, ignore_case = rule$ignore_case)
        )
    }

    if (isTRUE(rule$require_non_missing)) {
      match_vec <- match_vec & !is.na(column)
    }
    if (isTRUE(rule$negate)) {
      match_vec <- !match_vec
      if (isTRUE(rule$require_non_missing)) {
        match_vec <- match_vec & !is.na(column)
      }
    }

    keep <- keep & match_vec
  }

  keep
}

#' Filter reference-anchor rows from a dynamic selector
#'
#' @param candidate_models Data frame or tibble containing candidate models.
#' @param selector Named list of anchor-selection rules.
#' @param require_selection Whether zero matching rows should raise an error.
#'
#' @return A tibble containing only the selected reference-anchor rows.
#'
#' @keywords internal
filter_reference_anchor_rows_by_selector <- function(candidate_models,
                                                     selector,
                                                     require_selection = TRUE) {
  if (!is.logical(require_selection) || length(require_selection) != 1 || is.na(require_selection)) {
    stop("'require_selection' must be TRUE or FALSE.", call. = FALSE)
  }

  candidate_models <- tibble::as_tibble(candidate_models)
  keep <- reference_anchor_selector_mask(
    candidate_models = candidate_models,
    selector = selector
  )
  anchor_models <- candidate_models[keep, , drop = FALSE]

  if (isTRUE(require_selection) && nrow(anchor_models) == 0) {
    stop("No reference-anchor models matched the supplied selector.", call. = FALSE)
  }

  anchor_models
}

#' Set reference anchors on a candidate table or `Candidates` object
#'
#' Filters a candidate-model table down to an explicit set of reference-anchor
#' model IDs. When `object` is a [Candidates] instance, the selected anchor
#' rows are stored in its `@reference_anchors` slot and a rebuilt object is
#' returned. When `object` is a data frame, the filtered anchor rows are
#' returned directly.
#'
#' @param object A candidate-model data frame/tibble or a [Candidates] object.
#' @param candidate_models Deprecated compatibility alias for `object`.
#' @param model_ids Character vector of model IDs to retain as reference
#'   anchors.
#' @param selector Optional named list of dynamic anchor-selection rules. A
#'   compact pipeline-style selector such as `list(regional_body = "SWFSC")`
#'   performs exact membership matching. Structured rules may also be supplied,
#'   for example `list(regional_body = list(mode = "regex", pattern = "SWFSC"))`.
#' @param model_id_col Name of the model-ID column.
#' @param require_selection Whether zero selected anchors should raise an
#'   error.
#'
#' @return If `object` is a data frame, a tibble containing only the selected
#'   reference-anchor rows. If `object` is a [Candidates] object, a rebuilt
#'   `Candidates` object with `@reference_anchors` set.
#'
#' @examples
#' anchor_tbl <- set_reference_anchors(
#'   tibble::tibble(model_id = c("12", "18", "24"), x = 1:3),
#'   model_ids = c("12", "24")
#' )
#'
#' dynamic_anchor_tbl <- set_reference_anchors(
#'   tibble::tibble(
#'     model_id = c("12", "18", "24"),
#'     regional_body = c("SWFSC", "AFSC", "SWFSC")
#'   ),
#'   selector = list(regional_body = "SWFSC")
#' )
#'
#' \dontrun{
#' set_reference_anchors(
#'   candidate_models,
#'   model_ids = c("12", "18", "24")
#' )
#'
#' candidates <- as_candidates(cfg)
#' candidates <- set_reference_anchors(
#'   candidates,
#'   selector = list(regional_body = "SWFSC")
#' )
#' candidates@reference_anchors
#'
#' configured_candidates <- as_candidates(list(
#'   study = list(path = "input.xlsx"),
#'   anchors = list(
#'     selector = list(regional_body = "SWFSC")
#'   )
#' ))
#' configured_candidates <- set_reference_anchors(configured_candidates)
#' }
#'
#' @export
set_reference_anchors <- function(object = NULL,
                                  model_ids = NULL,
                                  selector = NULL,
                                  model_id_col = "model_id",
                                  require_selection = TRUE,
                                  candidate_models = NULL) {
  if (is.null(object) && !is.null(candidate_models)) {
    object <- candidate_models
  }

  if (!is.null(object) && !is.null(candidate_models)) {
    stop(
      "Supply either 'object' or 'candidate_models', not both.",
      call. = FALSE
    )
  }

  # Support both direct table filtering and object-level anchor assignment so
  # the same public function can be used during ad hoc analysis or inside the
  # staged `Candidates` staged object.
  if ((inherits(object, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(object, Candidates), error = function(e) FALSE)))) {
    # When no selector is passed explicitly, fall back to the anchor-selection
    # spec stored on the `Candidates` object itself.
    if (is.null(model_ids) && is.null(selector)) {
      selector <- ((object@spec)$anchors)$selector %||% NULL
      model_ids <- ((object@spec)$anchors)$model_ids %||% NULL
      model_id_col <- ((object@spec)$anchors)$model_id_col %||% model_id_col
      require_selection <- ((object@spec)$anchors)$require_selection %||% require_selection
    }
  }

  if (is.null(model_ids) && is.null(selector)) {
    stop(
      "Supply either 'model_ids' or 'selector', or store an anchor spec on the `Candidates` object.",
      call. = FALSE
    )
  }
  if (!is.null(model_ids) && !is.null(selector)) {
    stop(
      "Supply either 'model_ids' or 'selector', not both.",
      call. = FALSE
    )
  }

  if ((inherits(object, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(object, Candidates), error = function(e) FALSE)))) {
    anchor_models <- if (!is.null(selector)) {
      filter_reference_anchor_rows_by_selector(
        candidate_models = object@candidate_models,
        selector = selector,
        require_selection = require_selection
      )
    } else {
      filter_reference_anchor_rows(
        candidate_models = object@candidate_models,
        model_ids = model_ids,
        model_id_col = model_id_col
      )
    }
    preserved_admissibility <- if (candidate_admissibility_matches_anchors(object, anchor_models)) {
      object@admissibility
    } else {
      list()
    }

    return(Candidates(
      spec = object@spec,
      study_db = object@study_db,
      species_vector = object@species_vector,
      source_dbs = object@source_dbs,
      species_db = object@species_db,
      candidate_models = object@candidate_models,
      reference_anchors = tibble::as_tibble(anchor_models),
      similarity_matrix = object@similarity_matrix,
      gower_distances = object@gower_distances,
      ordination = list(),
      admissibility = preserved_admissibility,
      similarity_tuning = object@similarity_tuning
    ))
  }

  if (!is.data.frame(object)) {
    stop(
      "'object' must be a candidate-model data frame/tibble or a `Candidates` object.",
      call. = FALSE
    )
  }

  if (!is.null(selector)) {
    return(filter_reference_anchor_rows_by_selector(
      candidate_models = object,
      selector = selector,
      require_selection = require_selection
    ))
  }

  filter_reference_anchor_rows(
    candidate_models = object,
    model_ids = model_ids,
    model_id_col = model_id_col
  )
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
  policy_perf <- standardize_policies(policy_perf)
  policy_perf$policy <- resolve_policy_names(policy_perf)
  if (nrow(policy_perf) == 0L || !"valid_prediction" %in% names(policy_perf)) {
    return(tibble::tibble(
      policy                 = character(),
      equation_branch_filter = character(),
      n                      = integer(),
      q_abs_log              = numeric(),
      median_abs_log         = numeric()
    ))
  }
  policy_perf |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log)) |>
    dplyr::group_by(policy, equation_branch_filter) |>
    dplyr::summarise(
      n = dplyr::n(),
      q_abs_log = {
        # Split conformal requires the ceiling-based order statistic:
        # q = sort(scores)[min(n, ceiling((n+1)*(1-alpha)))]
        # stats::quantile(..., type=8) interpolates between order stats and
        # can return a value below the required index, violating coverage.
        s <- sort(error_abs_log)
        n_cal <- length(s)
        idx <- min(n_cal, ceiling((n_cal + 1L) * (1 - alpha)))
        if (n_cal == 0L) NA_real_ else s[[idx]]
      },
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
  perf_aug <- standardize_policies(policy_perf)
  perf_aug$policy <- resolve_policy_names(perf_aug)
  conf_cal <- standardize_policies(conf_cal)
  if (nrow(perf_aug) == 0L || !"valid_prediction" %in% names(perf_aug)) {
    empty <- tibble::tibble(
      policy                    = character(),
      equation_branch_filter    = character(),
      benchmark_label           = character(),
      n                         = integer(),
      empirical_coverage        = numeric(),
      median_interval_log_width = numeric(),
      mean_signed_log_error     = numeric(),
      median_signed_log_error   = numeric(),
      mean_abs_log_error        = numeric(),
      median_abs_log_error      = numeric()
    )
    return(list(overall = empty, by_species = empty))
  }
  perf_aug <- perf_aug |>
    dplyr::filter(
      valid_prediction,
      is.finite(multiplier_pred),
      multiplier_pred > 0,
      is.finite(error_abs_log)
    ) |>
    dplyr::left_join(
      tibble::as_tibble(conf_cal) |>
        dplyr::select(policy, equation_branch_filter, q_abs_log),
      by = c("policy", "equation_branch_filter")
    ) |>
    dplyr::mutate(
      covered = is.finite(q_abs_log) & error_abs_log <= q_abs_log,
      interval_log_width = 2 * q_abs_log,
      signed_log_error = log(multiplier_pred)
    )

  # Summarize the calibrated benchmark at the policy level first.
  overall <- perf_aug |>
    dplyr::group_by(policy, equation_branch_filter) |>
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
    dplyr::group_by(policy, equation_branch_filter, anchor_species) |>
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
    dplyr::group_by(policy, equation_branch_filter) |>
    dplyr::arrange(u, .by_group = TRUE) |>
    dplyr::mutate(
      median_ts_error_smooth = smooth_one(u, median_ts_error),
      q80_ts_abs_raw_smooth = smooth_one(u, q80_ts_abs_raw),
      q90_ts_abs_raw_smooth = smooth_one(u, q90_ts_abs_raw),
      q95_ts_abs_raw_smooth = smooth_one(u, q95_ts_abs_raw),
      q99_ts_abs_raw_smooth = smooth_one(u, q99_ts_abs_raw),
      q80_ts_abs_dev_smooth = smooth_one(u, q80_ts_abs_dev),
      q90_ts_abs_dev_smooth = smooth_one(u, q90_ts_abs_dev),
      q95_ts_abs_dev_smooth = smooth_one(u, q95_ts_abs_dev),
      q99_ts_abs_dev_smooth = smooth_one(u, q99_ts_abs_dev),
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
  ts_cal <- standardize_policies(ts_error)
  ts_cal$policy <- resolve_policy_names(ts_cal)
  ts_cal <- ts_cal |>
    dplyr::group_by(policy, equation_branch_filter, u) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_ts_error = stats::median(ts_error, na.rm = TRUE),
      q80_ts_abs_raw = stats::quantile(
        abs(ts_error),
        probs = 0.80, na.rm = TRUE, names = FALSE, type = 8
      ),
      q90_ts_abs_raw = stats::quantile(
        abs(ts_error),
        probs = 0.90, na.rm = TRUE, names = FALSE, type = 8
      ),
      q95_ts_abs_raw = stats::quantile(
        abs(ts_error),
        probs = 0.95, na.rm = TRUE, names = FALSE, type = 8
      ),
      q99_ts_abs_raw = stats::quantile(
        abs(ts_error),
        probs = 0.99, na.rm = TRUE, names = FALSE, type = 8
      ),
      q80_ts_abs_dev = stats::quantile(
        abs(ts_error - stats::median(ts_error, na.rm = TRUE)),
        probs = 0.80, na.rm = TRUE, names = FALSE, type = 8
      ),
      q90_ts_abs_dev = stats::quantile(
        abs(ts_error - stats::median(ts_error, na.rm = TRUE)),
        probs = 0.90, na.rm = TRUE, names = FALSE, type = 8
      ),
      q95_ts_abs_dev = stats::quantile(
        abs(ts_error - stats::median(ts_error, na.rm = TRUE)),
        probs = 0.95, na.rm = TRUE, names = FALSE, type = 8
      ),
      q99_ts_abs_dev = stats::quantile(
        abs(ts_error - stats::median(ts_error, na.rm = TRUE)),
        probs = 0.99, na.rm = TRUE, names = FALSE, type = 8
      ),
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

smooth_selected_ts_calibration <- function(ts_cal) {
  if (nrow(ts_cal) == 0) {
    return(tibble::as_tibble(ts_cal))
  }

  ts_cal <- tibble::as_tibble(ts_cal)
  for (nm in c(
    "q80_log_sigma_abs_dev",
    "q90_log_sigma_abs_dev",
    "q95_log_sigma_abs_dev",
    "q99_log_sigma_abs_dev"
  )) {
    if (!nm %in% names(ts_cal)) {
      ts_cal[[nm]] <- NA_real_
    }
  }

  smooth_one <- function(x, y) {
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

  ts_cal |>
    dplyr::group_by(
      dplyr::across(dplyr::any_of(c(
        "anchor_model_id",
        "anchor_species",
        "anchor_family",
        "policy",
        "post_selection_support_bin",
        "equation_branch_filter"
      )))
    ) |>
    dplyr::arrange(u, .by_group = TRUE) |>
    dplyr::mutate(
      median_ts_error_smooth = smooth_one(u, median_ts_error),
      q80_ts_abs_raw_smooth = smooth_one(u, q80_ts_abs_raw),
      q90_ts_abs_raw_smooth = smooth_one(u, q90_ts_abs_raw),
      q95_ts_abs_raw_smooth = smooth_one(u, q95_ts_abs_raw),
      q99_ts_abs_raw_smooth = smooth_one(u, q99_ts_abs_raw),
      q80_ts_abs_dev_smooth = smooth_one(u, q80_ts_abs_dev),
      q90_ts_abs_dev_smooth = smooth_one(u, q90_ts_abs_dev),
      q95_ts_abs_dev_smooth = smooth_one(u, q95_ts_abs_dev),
      q99_ts_abs_dev_smooth = smooth_one(u, q99_ts_abs_dev),
      median_log_sigma_residual_smooth = smooth_one(u, median_log_sigma_residual),
      q80_log_sigma_abs_dev_smooth = smooth_one(u, q80_log_sigma_abs_dev),
      q90_log_sigma_abs_dev_smooth = smooth_one(u, q90_log_sigma_abs_dev),
      q95_log_sigma_abs_dev_smooth = smooth_one(u, q95_log_sigma_abs_dev),
      q99_log_sigma_abs_dev_smooth = smooth_one(u, q99_log_sigma_abs_dev),
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

#' Summarize selected-policy TS calibration
#'
#' Restricts TS residual calibration to the cross-fitted winning-policy rows,
#' then smooths the residual envelopes within support bin and branch.
#'
#' @param ts_error Policy TS-error table.
#' @param selected_tbl Selected-policy table.
#'
#' @return A tibble.
#'
#' @keywords internal
summarize_selected_ts_calibration <- function(ts_error,
                                              selected_tbl,
                                              policy_perf = NULL) {
  ts_error <- tibble::as_tibble(ts_error)
  selected_tbl <- tibble::as_tibble(selected_tbl)
  if (nrow(ts_error) == 0 || nrow(selected_tbl) == 0) {
    return(tibble::tibble())
  }

  selected_policy_values <- if ("selected_policy" %in% names(selected_tbl)) {
    as.character(selected_tbl$selected_policy)
  } else if ("policy" %in% names(selected_tbl)) {
    as.character(selected_tbl$policy)
  } else {
    rep(NA_character_, nrow(selected_tbl))
  }
  selected_keys <- selected_tbl |>
    dplyr::transmute(
      policy = selected_policy_values,
      equation_branch_filter = selected_branches(selected_tbl)
    ) |>
    dplyr::filter(
      !is.na(policy),
      !is.na(equation_branch_filter)
    ) |>
    dplyr::distinct(policy, equation_branch_filter)
  if (nrow(selected_keys) == 0) {
    return(tibble::tibble())
  }

  selected_ts <- standardize_policies(ts_error)
  selected_ts$policy <- resolve_policy_names(selected_ts)
  selected_ts <- selected_ts |>
    dplyr::mutate(
      anchor_model_id = as.character(anchor_model_id),
      anchor_species = if ("anchor_species" %in% names(selected_ts)) as.character(anchor_species) else rep(NA_character_, dplyr::n()),
      anchor_family = if ("anchor_family" %in% names(selected_ts)) as.character(anchor_family) else rep(NA_character_, dplyr::n())
    )
  selected_ts_joined <- selected_ts |>
    dplyr::inner_join(
      selected_keys,
      by = c("policy", "equation_branch_filter")
    )
  if (nrow(selected_ts_joined) == 0) {
    return(tibble::tibble())
  }

  selected_ts_joined |>
    dplyr::mutate(
      u = suppressWarnings(as.numeric(u)),
      ts_error = suppressWarnings(as.numeric(ts_error)),
      log_sigma_residual = suppressWarnings(as.numeric(log_sigma_residual))
    ) |>
    dplyr::filter(
      is.finite(u),
      is.finite(ts_error),
      is.finite(log_sigma_residual)
    )
}

#' Summarize selected-policy coefficient calibration
#'
#' Stores historical slope/intercept residual pairs for cross-fitted winning
#' policies so anchor-facing TS panels can be built from coefficient-space
#' uncertainty instead of multiplier-space uncertainty.
#'
#' @param selected_tbl Selected-policy table.
#' @param candidate_models Candidate-model table containing the true anchor
#'   coefficients.
#' @param ts_error Optional policy TS-error table used to recover policy
#'   coefficients when `selected_tbl` does not carry them directly.
#'
#' @return A tibble of residual pairs.
#'
#' @keywords internal
summarize_selected_coefficient_calibration <- function(selected_tbl,
                                                       candidate_models,
                                                       ts_error = NULL) {
  selected_tbl <- tibble::as_tibble(selected_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  ts_error <- tibble::as_tibble(ts_error %||% tibble::tibble())
  if (nrow(selected_tbl) == 0 || nrow(candidate_models) == 0 || nrow(ts_error) == 0) {
    return(tibble::tibble())
  }

  id_col <- if ("model_id_chr" %in% names(candidate_models)) "model_id_chr" else "model_id"
  selected_policy_values <- if ("selected_policy" %in% names(selected_tbl)) {
    as.character(selected_tbl$selected_policy)
  } else if ("policy" %in% names(selected_tbl)) {
    as.character(selected_tbl$policy)
  } else {
    rep(NA_character_, nrow(selected_tbl))
  }
  selected_support_bins <- if ("post_selection_support_bin" %in% names(selected_tbl)) {
    as.character(selected_tbl$post_selection_support_bin)
  } else {
    rep(NA_character_, nrow(selected_tbl))
  }
  candidate_field_or_na <- function(nm, mode = c("numeric", "character")) {
    mode <- match.arg(mode)
    if (!nm %in% names(candidate_models)) {
      if (identical(mode, "character")) {
        return(rep(NA_character_, nrow(candidate_models)))
      }
      return(rep(NA_real_, nrow(candidate_models)))
    }
    if (identical(mode, "character")) {
      return(as.character(candidate_models[[nm]]))
    }
    suppressWarnings(as.numeric(candidate_models[[nm]]))
  }

  truth_tbl <- candidate_models |>
    dplyr::transmute(
      anchor_model_id = as.character(.data[[id_col]]),
      anchor_species = candidate_field_or_na("species_name", "character"),
      anchor_genus = dplyr::coalesce(
        candidate_field_or_na("genus", "character"),
        stringr::word(candidate_field_or_na("species_name", "character"), 1)
      ),
      anchor_family = dplyr::coalesce(
        candidate_field_or_na("family_name", "character"),
        candidate_field_or_na("family", "character")
      ),
      anchor_frequency_khz = suppressWarnings(as.numeric(
        dplyr::coalesce(
          candidate_field_or_na("frequency_khz"),
          candidate_field_or_na("frequency")
        )
      )),
      anchor_length_min = suppressWarnings(as.numeric(
        dplyr::coalesce(
          candidate_field_or_na("length_min"),
          candidate_field_or_na("study_length_min"),
          candidate_field_or_na("length_midpoint"),
          candidate_field_or_na("study_length_midpoint")
        )
      )),
      anchor_length_max = suppressWarnings(as.numeric(
        dplyr::coalesce(
          candidate_field_or_na("length_max"),
          candidate_field_or_na("study_length_max"),
          candidate_field_or_na("length_midpoint"),
          candidate_field_or_na("study_length_midpoint")
        )
      )),
      anchor_depth_min = suppressWarnings(as.numeric(
        dplyr::coalesce(
          candidate_field_or_na("depth_min"),
          candidate_field_or_na("study_depth_min"),
          candidate_field_or_na("depth_midpoint"),
          candidate_field_or_na("study_depth_midpoint")
        )
      )),
      anchor_depth_max = suppressWarnings(as.numeric(
        dplyr::coalesce(
          candidate_field_or_na("depth_max"),
          candidate_field_or_na("study_depth_max"),
          candidate_field_or_na("depth_midpoint"),
          candidate_field_or_na("study_depth_midpoint")
        )
      )),
      anchor_slope_len = candidate_field_or_na("slope_len"),
      anchor_intercept_len = candidate_field_or_na("intercept_len")
    )

  selected_keys <- selected_tbl |>
    dplyr::transmute(
      policy = selected_policy_values,
      equation_branch_filter = selected_branches(selected_tbl),
      post_selection_support_bin = selected_support_bins
    ) |>
    dplyr::filter(
      !is.na(policy),
      !is.na(equation_branch_filter)
    )
  selected_keys <- selected_keys |>
    dplyr::group_by(policy, equation_branch_filter) |>
    dplyr::summarise(
      post_selection_support_bin = {
        x <- stats::na.omit(post_selection_support_bin)
        if (length(x) > 0) as.character(x[[1]]) else NA_character_
      },
      .groups = "drop"
    )
  if (nrow(selected_keys) == 0) {
    return(tibble::tibble())
  }

  ts_error <- standardize_policies(ts_error)
  ts_error$policy <- resolve_policy_names(ts_error)
  if (all(c("policy_slope_len", "policy_intercept_len") %in% names(ts_error))) {
    benchmark_coefficients <- ts_error |>
      dplyr::mutate(
        anchor_model_id = as.character(anchor_model_id),
        policy_slope_len = suppressWarnings(as.numeric(policy_slope_len)),
        policy_intercept_len = suppressWarnings(as.numeric(policy_intercept_len)),
        local_min_combined_distance = if ("local_min_combined_distance" %in% names(ts_error)) suppressWarnings(as.numeric(local_min_combined_distance)) else NA_real_,
        local_weighted_mean_combined_distance = if ("local_weighted_mean_combined_distance" %in% names(ts_error)) suppressWarnings(as.numeric(local_weighted_mean_combined_distance)) else NA_real_,
        local_min_species_distance = if ("local_min_species_distance" %in% names(ts_error)) suppressWarnings(as.numeric(local_min_species_distance)) else NA_real_,
        local_weighted_mean_species_distance = if ("local_weighted_mean_species_distance" %in% names(ts_error)) suppressWarnings(as.numeric(local_weighted_mean_species_distance)) else NA_real_,
        local_min_trait_gower_distance = if ("local_min_trait_gower_distance" %in% names(ts_error)) suppressWarnings(as.numeric(local_min_trait_gower_distance)) else NA_real_,
        local_weighted_mean_trait_gower_distance = if ("local_weighted_mean_trait_gower_distance" %in% names(ts_error)) suppressWarnings(as.numeric(local_weighted_mean_trait_gower_distance)) else NA_real_,
        local_effective_support = if ("local_effective_support" %in% names(ts_error)) suppressWarnings(as.numeric(local_effective_support)) else NA_real_,
        local_mean_length_overlap = if ("local_mean_length_overlap" %in% names(ts_error)) suppressWarnings(as.numeric(local_mean_length_overlap)) else NA_real_,
        local_mean_depth_overlap = if ("local_mean_depth_overlap" %in% names(ts_error)) suppressWarnings(as.numeric(local_mean_depth_overlap)) else NA_real_
      ) |>
      dplyr::group_by(anchor_model_id, policy, equation_branch_filter) |>
      dplyr::summarise(
        policy_slope_len = stats::median(policy_slope_len, na.rm = TRUE),
        policy_intercept_len = stats::median(policy_intercept_len, na.rm = TRUE),
        local_min_combined_distance = stats::median(local_min_combined_distance, na.rm = TRUE),
        local_weighted_mean_combined_distance = stats::median(local_weighted_mean_combined_distance, na.rm = TRUE),
        local_min_species_distance = stats::median(local_min_species_distance, na.rm = TRUE),
        local_weighted_mean_species_distance = stats::median(local_weighted_mean_species_distance, na.rm = TRUE),
        local_min_trait_gower_distance = stats::median(local_min_trait_gower_distance, na.rm = TRUE),
        local_weighted_mean_trait_gower_distance = stats::median(local_weighted_mean_trait_gower_distance, na.rm = TRUE),
        local_effective_support = stats::median(local_effective_support, na.rm = TRUE),
        local_mean_length_overlap = stats::median(local_mean_length_overlap, na.rm = TRUE),
        local_mean_depth_overlap = stats::median(local_mean_depth_overlap, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(
        is.finite(policy_slope_len),
        is.finite(policy_intercept_len)
      )
  } else if (all(c("length_cm", "ts_pred") %in% names(ts_error))) {
    benchmark_coefficients <- ts_error |>
      dplyr::mutate(
        anchor_model_id = as.character(anchor_model_id),
        length_cm = suppressWarnings(as.numeric(length_cm)),
        ts_pred = suppressWarnings(as.numeric(ts_pred))
      ) |>
      dplyr::group_by(anchor_model_id, policy, equation_branch_filter) |>
      dplyr::summarise(
        policy_slope_len = {
          keep <- is.finite(length_cm) & length_cm > 0 & is.finite(ts_pred)
          if (sum(keep) < 2L) {
            NA_real_
          } else {
            x <- log10(length_cm[keep])
            y <- ts_pred[keep]
            slope_var <- stats::var(x)
            if (!is.finite(slope_var) || slope_var <= 0) {
              NA_real_
            } else {
              stats::cov(x, y) / slope_var
            }
          }
        },
        policy_intercept_len = {
          keep <- is.finite(length_cm) & length_cm > 0 & is.finite(ts_pred)
          if (sum(keep) < 2L) {
            NA_real_
          } else {
            x <- log10(length_cm[keep])
            y <- ts_pred[keep]
            slope_var <- stats::var(x)
            if (!is.finite(slope_var) || slope_var <= 0) {
              NA_real_
            } else {
              slope_now <- stats::cov(x, y) / slope_var
              mean(y) - slope_now * mean(x)
            }
          }
        },
        .groups = "drop"
      ) |>
      dplyr::filter(
        is.finite(policy_slope_len),
        is.finite(policy_intercept_len)
      )
  } else {
    benchmark_coefficients <- tibble::tibble()
  }
  if (nrow(benchmark_coefficients) == 0) {
    return(tibble::tibble())
  }

  benchmark_selected <- benchmark_coefficients |>
    dplyr::inner_join(
      dplyr::distinct(selected_keys, policy, equation_branch_filter, post_selection_support_bin),
      by = c("policy", "equation_branch_filter")
    )
  benchmark_selected |>
    dplyr::inner_join(truth_tbl, by = "anchor_model_id") |>
    dplyr::mutate(
      slope_resid = anchor_slope_len - policy_slope_len,
      intercept_resid = anchor_intercept_len - policy_intercept_len
    ) |>
    dplyr::filter(
      is.finite(policy_slope_len),
      is.finite(policy_intercept_len),
      is.finite(anchor_slope_len),
      is.finite(anchor_intercept_len),
      is.finite(slope_resid),
      is.finite(intercept_resid),
      !is.na(policy),
      !is.na(equation_branch_filter)
    ) |>
    dplyr::select(
      anchor_model_id,
      anchor_species,
      anchor_genus,
      anchor_family,
      anchor_frequency_khz,
      anchor_length_min,
      anchor_length_max,
      anchor_depth_min,
      anchor_depth_max,
      policy,
      equation_branch_filter,
      post_selection_support_bin,
      local_min_combined_distance,
      local_weighted_mean_combined_distance,
      local_min_species_distance,
      local_weighted_mean_species_distance,
      local_min_trait_gower_distance,
      local_weighted_mean_trait_gower_distance,
      local_effective_support,
      local_mean_length_overlap,
      local_mean_depth_overlap,
      policy_slope_len,
      policy_intercept_len,
      slope_resid,
      intercept_resid
    )
}

resolve_numeric_candidates <- function(tbl,
                                       candidates) {
  tbl <- tibble::as_tibble(tbl)
  for (nm in candidates) {
    if (nm %in% names(tbl)) {
      return(suppressWarnings(as.numeric(tbl[[nm]])))
    }
  }
  rep(NA_real_, nrow(tbl))
}

weighted_mean_or_na <- function(x,
                                w) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[keep], w[keep], na.rm = TRUE)
}

weighted_quantile_or_na <- function(x,
                                    w,
                                    prob) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  total_w <- sum(w)
  if (!is.finite(total_w) || total_w <= 0) {
    return(NA_real_)
  }
  prob <- suppressWarnings(as.numeric(prob)[[1]])
  if (!is.finite(prob)) {
    return(NA_real_)
  }
  prob <- min(max(prob, 0), 1)
  if (length(x) == 1L) {
    return(x[[1]])
  }
  w <- w / total_w
  mid_p <- cumsum(w) - 0.5 * w
  mid_p <- pmin(pmax(mid_p, 0), 1)
  mid_p <- cummax(mid_p)
  stats::approx(
    x = mid_p,
    y = x,
    xout = prob,
    method = "linear",
    ties = "ordered",
    rule = 2
  )$y[[1]]
}

weighted_cor_or_na <- function(x,
                               y,
                               w) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  w <- suppressWarnings(as.numeric(w))
  keep <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(keep) < 2L) {
    return(NA_real_)
  }
  x <- x[keep]
  y <- y[keep]
  w <- w[keep]
  w_sum <- sum(w)
  if (!is.finite(w_sum) || w_sum <= 0) {
    return(NA_real_)
  }
  w <- w / w_sum
  x_bar <- sum(w * x)
  y_bar <- sum(w * y)
  x_ctr <- x - x_bar
  y_ctr <- y - y_bar
  vx <- sum(w * x_ctr^2)
  vy <- sum(w * y_ctr^2)
  if (!is.finite(vx) || !is.finite(vy) || vx <= 0 || vy <= 0) {
    return(NA_real_)
  }
  sum(w * x_ctr * y_ctr) / sqrt(vx * vy)
}

similarity_weight_predictors <- function(tbl,
                                         include_u = TRUE) {
  tbl <- tibble::as_tibble(tbl)
  candidate_cols <- c(
    if (isTRUE(include_u)) "u" else character(),
    "local_min_combined_distance",
    "local_weighted_mean_combined_distance",
    "local_min_species_distance",
    "local_weighted_mean_species_distance",
    "local_min_trait_gower_distance",
    "local_weighted_mean_trait_gower_distance",
    "log_local_effective_support",
    "local_mean_length_overlap",
    "local_mean_depth_overlap",
    "policy_slope_len",
    "policy_intercept_len"
  )
  candidate_cols <- candidate_cols[candidate_cols %in% names(tbl)]
  candidate_cols[vapply(
    candidate_cols,
    function(nm) {
      vals <- suppressWarnings(as.numeric(tbl[[nm]]))
      vals <- vals[is.finite(vals)]
      length(unique(vals)) >= 2L
    },
    logical(1)
  )]
}

locality_similarity_weights <- function(training_tbl,
                                        row_now,
                                        target_u = NA_real_,
                                        include_u = TRUE) {
  # Compute continuous similarity weights from the realized donor geometry.
  # Rows close to the selected anchor in distance/overlap/support space receive
  # larger weights; rows far away are smoothly downweighted rather than being
  # removed by a hard-coded group fallback.
  training_tbl <- tibble::as_tibble(training_tbl)
  row_now <- tibble::as_tibble(row_now)
  if (nrow(training_tbl) == 0 || nrow(row_now) == 0) {
    return(numeric())
  }

  predictor_cols <- similarity_weight_predictors(
    tbl = training_tbl,
    include_u = include_u
  )
  if (length(predictor_cols) == 0L) {
    return(rep(1, nrow(training_tbl)))
  }

  target_map <- setNames(vector("list", length(predictor_cols)), predictor_cols)
  for (nm in predictor_cols) {
    value_now <- if (identical(nm, "u")) {
      suppressWarnings(as.numeric(target_u)[[1]])
    } else if (nm %in% names(row_now)) {
      suppressWarnings(as.numeric(row_now[[nm]][[1]]))
    } else if (identical(nm, "log_local_effective_support") &&
               "local_effective_support" %in% names(row_now)) {
      suppressWarnings(log1p(pmax(as.numeric(row_now$local_effective_support[[1]]), 0)))
    } else {
      NA_real_
    }
    target_map[[nm]] <- value_now
  }

  dist_components <- vector("list", length(predictor_cols))
  component_names <- character(0)
  for (nm in predictor_cols) {
    vals <- suppressWarnings(as.numeric(training_tbl[[nm]]))
    vals_ok <- vals[is.finite(vals)]
    if (length(vals_ok) < 2L) {
      next
    }
    scale_now <- stats::mad(vals_ok, center = stats::median(vals_ok), constant = 1, na.rm = TRUE)
    if (!is.finite(scale_now) || scale_now <= 0) {
      scale_now <- stats::IQR(vals_ok, na.rm = TRUE) / 1.349
    }
    if (!is.finite(scale_now) || scale_now <= 0) {
      scale_now <- stats::sd(vals_ok, na.rm = TRUE)
    }
    if (!is.finite(scale_now) || scale_now <= 0) {
      next
    }

    target_now <- suppressWarnings(as.numeric(target_map[[nm]] %||% NA_real_))
    if (!is.finite(target_now)) {
      target_now <- stats::median(vals_ok, na.rm = TRUE)
    }
    if (!is.finite(target_now)) {
      next
    }

    vals_use <- vals
    vals_use[!is.finite(vals_use)] <- target_now
    dist_components[[nm]] <- ((vals_use - target_now) / scale_now)^2
    component_names <- c(component_names, nm)
  }

  if (length(component_names) == 0L) {
    return(rep(1, nrow(training_tbl)))
  }

  dist_mat <- do.call(
    cbind,
    dist_components[component_names]
  )
  if (is.null(dim(dist_mat))) {
    dist_mat <- matrix(dist_mat, ncol = 1L)
  }
  d2 <- rowMeans(dist_mat, na.rm = TRUE)
  d2[!is.finite(d2)] <- stats::median(d2[is.finite(d2)], na.rm = TRUE)
  d2[!is.finite(d2)] <- 0
  weights <- exp(-0.5 * d2)
  weights[!is.finite(weights)] <- 0
  if (!any(weights > 0)) {
    weights <- rep(1, nrow(training_tbl))
  }
  weights
}

smooth_weighted_curve <- function(u,
                                  y,
                                  spar = 0.6) {
  u <- suppressWarnings(as.numeric(u))
  y <- suppressWarnings(as.numeric(y))
  keep <- is.finite(u) & is.finite(y)
  if (sum(keep) < 4L) {
    return(y)
  }
  ord <- order(u[keep])
  u_fit <- u[keep][ord]
  y_fit <- y[keep][ord]
  smoothed <- tryCatch(
    stats::predict(
      stats::smooth.spline(x = u_fit, y = y_fit, spar = spar),
      x = u
    )$y,
    error = function(e) y
  )
  smoothed
}

collapse_selected_ts_calibration <- function(ts_calibration) {
  ts_calibration <- tibble::as_tibble(ts_calibration)
  if (nrow(ts_calibration) == 0 || !"u" %in% names(ts_calibration)) {
    return(tibble::tibble())
  }

  if (all(c("ts_error", "log_sigma_residual") %in% names(ts_calibration))) {
    collapsed_raw <- ts_calibration |>
      dplyr::mutate(
        u = suppressWarnings(as.numeric(u)),
        ts_error = suppressWarnings(as.numeric(ts_error)),
        log_sigma_residual = suppressWarnings(as.numeric(log_sigma_residual))
      ) |>
      dplyr::filter(
        is.finite(u),
        is.finite(ts_error),
        is.finite(log_sigma_residual)
      ) |>
      dplyr::group_by(u) |>
      dplyr::summarise(
        n = dplyr::n(),
        median_ts_error = stats::median(ts_error, na.rm = TRUE),
        q80_ts_abs_raw = stats::quantile(abs(ts_error), probs = 0.80, na.rm = TRUE, names = FALSE, type = 8),
        q90_ts_abs_raw = stats::quantile(abs(ts_error), probs = 0.90, na.rm = TRUE, names = FALSE, type = 8),
        q95_ts_abs_raw = stats::quantile(abs(ts_error), probs = 0.95, na.rm = TRUE, names = FALSE, type = 8),
        q99_ts_abs_raw = stats::quantile(abs(ts_error), probs = 0.99, na.rm = TRUE, names = FALSE, type = 8),
        q80_ts_abs_dev = stats::quantile(abs(ts_error - stats::median(ts_error, na.rm = TRUE)), probs = 0.80, na.rm = TRUE, names = FALSE, type = 8),
        q90_ts_abs_dev = stats::quantile(abs(ts_error - stats::median(ts_error, na.rm = TRUE)), probs = 0.90, na.rm = TRUE, names = FALSE, type = 8),
        q95_ts_abs_dev = stats::quantile(abs(ts_error - stats::median(ts_error, na.rm = TRUE)), probs = 0.95, na.rm = TRUE, names = FALSE, type = 8),
        q99_ts_abs_dev = stats::quantile(abs(ts_error - stats::median(ts_error, na.rm = TRUE)), probs = 0.99, na.rm = TRUE, names = FALSE, type = 8),
        median_log_sigma_residual = stats::median(log_sigma_residual, na.rm = TRUE),
        q80_log_sigma_abs_dev = stats::quantile(abs(log_sigma_residual - stats::median(log_sigma_residual, na.rm = TRUE)), probs = 0.80, na.rm = TRUE, names = FALSE, type = 8),
        q90_log_sigma_abs_dev = stats::quantile(abs(log_sigma_residual - stats::median(log_sigma_residual, na.rm = TRUE)), probs = 0.90, na.rm = TRUE, names = FALSE, type = 8),
        q95_log_sigma_abs_dev = stats::quantile(abs(log_sigma_residual - stats::median(log_sigma_residual, na.rm = TRUE)), probs = 0.95, na.rm = TRUE, names = FALSE, type = 8),
        q99_log_sigma_abs_dev = stats::quantile(abs(log_sigma_residual - stats::median(log_sigma_residual, na.rm = TRUE)), probs = 0.99, na.rm = TRUE, names = FALSE, type = 8),
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
    if (nrow(collapsed_raw) == 0) {
      return(collapsed_raw)
    }
    collapsed_raw <- smooth_selected_ts_calibration(collapsed_raw)
    return(
      collapsed_raw |>
        dplyr::mutate(
          q80_ts_abs_raw = pmax(q80_ts_abs_raw, 0, na.rm = TRUE),
          q90_ts_abs_raw = pmax(q90_ts_abs_raw, q80_ts_abs_raw, 0, na.rm = TRUE),
          q95_ts_abs_raw = pmax(q95_ts_abs_raw, q90_ts_abs_raw, 0, na.rm = TRUE),
          q99_ts_abs_raw = pmax(q99_ts_abs_raw, q95_ts_abs_raw, 0, na.rm = TRUE),
          q80_ts_abs_dev = pmax(q80_ts_abs_dev, 0, na.rm = TRUE),
          q90_ts_abs_dev = pmax(q90_ts_abs_dev, q80_ts_abs_dev, 0, na.rm = TRUE),
          q95_ts_abs_dev = pmax(q95_ts_abs_dev, q90_ts_abs_dev, 0, na.rm = TRUE),
          q99_ts_abs_dev = pmax(q99_ts_abs_dev, q95_ts_abs_dev, 0, na.rm = TRUE),
          q80_log_sigma_abs_dev = pmax(q80_log_sigma_abs_dev, 0, na.rm = TRUE),
          q90_log_sigma_abs_dev = pmax(q90_log_sigma_abs_dev, q80_log_sigma_abs_dev, 0, na.rm = TRUE),
          q95_log_sigma_abs_dev = pmax(q95_log_sigma_abs_dev, q90_log_sigma_abs_dev, 0, na.rm = TRUE),
          q99_log_sigma_abs_dev = pmax(q99_log_sigma_abs_dev, q95_log_sigma_abs_dev, 0, na.rm = TRUE)
        )
    )
  }

  collapsed <- ts_calibration |>
    dplyr::mutate(
      u = suppressWarnings(as.numeric(u)),
      .weight_n = dplyr::coalesce(
        if ("n" %in% names(ts_calibration)) suppressWarnings(as.numeric(n)) else rep(NA_real_, dplyr::n()),
        1
      ),
      .weight_n = dplyr::if_else(is.finite(.weight_n) & .weight_n > 0, .weight_n, 1),
      median_ts_error_use = resolve_numeric_candidates(ts_calibration, c("median_ts_error_smooth", "median_ts_error")),
      q80_ts_abs_raw_use = resolve_numeric_candidates(ts_calibration, c("q80_ts_abs_raw_smooth", "q80_ts_abs_raw")),
      q90_ts_abs_raw_use = resolve_numeric_candidates(ts_calibration, c("q90_ts_abs_raw_smooth", "q90_ts_abs_raw")),
      q95_ts_abs_raw_use = resolve_numeric_candidates(ts_calibration, c("q95_ts_abs_raw_smooth", "q95_ts_abs_raw")),
      q99_ts_abs_raw_use = resolve_numeric_candidates(ts_calibration, c("q99_ts_abs_raw_smooth", "q99_ts_abs_raw")),
      q80_ts_abs_dev_use = resolve_numeric_candidates(ts_calibration, c("q80_ts_abs_dev_smooth", "q80_ts_abs_dev")),
      q90_ts_abs_dev_use = resolve_numeric_candidates(ts_calibration, c("q90_ts_abs_dev_smooth", "q90_ts_abs_dev")),
      q95_ts_abs_dev_use = resolve_numeric_candidates(ts_calibration, c("q95_ts_abs_dev_smooth", "q95_ts_abs_dev")),
      q99_ts_abs_dev_use = resolve_numeric_candidates(ts_calibration, c("q99_ts_abs_dev_smooth", "q99_ts_abs_dev")),
      median_log_sigma_residual_use = resolve_numeric_candidates(ts_calibration, c("median_log_sigma_residual_smooth", "median_log_sigma_residual")),
      q10_log_sigma_residual_use = resolve_numeric_candidates(ts_calibration, c("q10_log_sigma_residual_smooth", "q10_log_sigma_residual")),
      q90_log_sigma_residual_use = resolve_numeric_candidates(ts_calibration, c("q90_log_sigma_residual_smooth", "q90_log_sigma_residual")),
      q05_log_sigma_residual_use = resolve_numeric_candidates(ts_calibration, c("q05_log_sigma_residual_smooth", "q05_log_sigma_residual")),
      q95_log_sigma_residual_use = resolve_numeric_candidates(ts_calibration, c("q95_log_sigma_residual_smooth", "q95_log_sigma_residual")),
      q025_log_sigma_residual_use = resolve_numeric_candidates(ts_calibration, c("q025_log_sigma_residual_smooth", "q025_log_sigma_residual")),
      q975_log_sigma_residual_use = resolve_numeric_candidates(ts_calibration, c("q975_log_sigma_residual_smooth", "q975_log_sigma_residual")),
      q005_log_sigma_residual_use = resolve_numeric_candidates(ts_calibration, c("q005_log_sigma_residual_smooth", "q005_log_sigma_residual")),
      q995_log_sigma_residual_use = resolve_numeric_candidates(ts_calibration, c("q995_log_sigma_residual_smooth", "q995_log_sigma_residual"))
    ) |>
    dplyr::filter(is.finite(u)) |>
    dplyr::group_by(u) |>
    dplyr::summarise(
      n = sum(.weight_n, na.rm = TRUE),
      median_ts_error = weighted_mean_or_na(median_ts_error_use, .weight_n),
      q80_ts_abs_raw = weighted_mean_or_na(q80_ts_abs_raw_use, .weight_n),
      q90_ts_abs_raw = weighted_mean_or_na(q90_ts_abs_raw_use, .weight_n),
      q95_ts_abs_raw = weighted_mean_or_na(q95_ts_abs_raw_use, .weight_n),
      q99_ts_abs_raw = weighted_mean_or_na(q99_ts_abs_raw_use, .weight_n),
      q80_ts_abs_dev = weighted_mean_or_na(q80_ts_abs_dev_use, .weight_n),
      q90_ts_abs_dev = weighted_mean_or_na(q90_ts_abs_dev_use, .weight_n),
      q95_ts_abs_dev = weighted_mean_or_na(q95_ts_abs_dev_use, .weight_n),
      q99_ts_abs_dev = weighted_mean_or_na(q99_ts_abs_dev_use, .weight_n),
      median_log_sigma_residual = weighted_mean_or_na(median_log_sigma_residual_use, .weight_n),
      q10_log_sigma_residual = weighted_mean_or_na(q10_log_sigma_residual_use, .weight_n),
      q90_log_sigma_residual = weighted_mean_or_na(q90_log_sigma_residual_use, .weight_n),
      q05_log_sigma_residual = weighted_mean_or_na(q05_log_sigma_residual_use, .weight_n),
      q95_log_sigma_residual = weighted_mean_or_na(q95_log_sigma_residual_use, .weight_n),
      q025_log_sigma_residual = weighted_mean_or_na(q025_log_sigma_residual_use, .weight_n),
      q975_log_sigma_residual = weighted_mean_or_na(q975_log_sigma_residual_use, .weight_n),
      q005_log_sigma_residual = weighted_mean_or_na(q005_log_sigma_residual_use, .weight_n),
      q995_log_sigma_residual = weighted_mean_or_na(q995_log_sigma_residual_use, .weight_n),
      .groups = "drop"
    ) |>
    dplyr::arrange(u)

  if (nrow(collapsed) == 0) {
    return(collapsed)
  }

  collapsed |>
    dplyr::mutate(
      q80_ts_abs_raw = pmax(q80_ts_abs_raw, 0, na.rm = TRUE),
      q90_ts_abs_raw = pmax(q90_ts_abs_raw, q80_ts_abs_raw, 0, na.rm = TRUE),
      q95_ts_abs_raw = pmax(q95_ts_abs_raw, q90_ts_abs_raw, 0, na.rm = TRUE),
      q99_ts_abs_raw = pmax(q99_ts_abs_raw, q95_ts_abs_raw, 0, na.rm = TRUE),
      q80_ts_abs_dev = pmax(q80_ts_abs_dev, 0, na.rm = TRUE),
      q90_ts_abs_dev = pmax(q90_ts_abs_dev, q80_ts_abs_dev, 0, na.rm = TRUE),
      q95_ts_abs_dev = pmax(q95_ts_abs_dev, q90_ts_abs_dev, 0, na.rm = TRUE),
      q99_ts_abs_dev = pmax(q99_ts_abs_dev, q95_ts_abs_dev, 0, na.rm = TRUE)
    )
}

enrich_ts_calibration_locality <- function(ts_calibration,
                                           policy_perf) {
  ts_calibration <- tibble::as_tibble(ts_calibration)
  policy_perf <- tibble::as_tibble(policy_perf)
  if (nrow(ts_calibration) == 0 || nrow(policy_perf) == 0) {
    return(ts_calibration)
  }

  locality_cols <- c(
    "local_min_combined_distance",
    "local_weighted_mean_combined_distance",
    "local_min_species_distance",
    "local_weighted_mean_species_distance",
    "local_min_trait_gower_distance",
    "local_weighted_mean_trait_gower_distance",
    "local_effective_support",
    "local_mean_length_overlap",
    "local_mean_depth_overlap",
    "policy_slope_len",
    "policy_intercept_len"
  )
  missing_locality <- setdiff(locality_cols, names(ts_calibration))
  all_missing <- any(vapply(
    intersect(locality_cols, names(ts_calibration)),
    function(nm) all(!is.finite(suppressWarnings(as.numeric(ts_calibration[[nm]])))),
    logical(1)
  ))
  if (length(missing_locality) == 0L && !all_missing) {
    return(ts_calibration)
  }

  key_cols <- c("anchor_model_id", "policy", "equation_branch_filter")
  if (!all(key_cols %in% names(ts_calibration)) || !all(key_cols %in% names(policy_perf))) {
    return(ts_calibration)
  }

  lookup <- standardize_policies(policy_perf) |>
    dplyr::mutate(
      anchor_model_id = as.character(anchor_model_id),
      policy = resolve_policy_names(policy_perf),
      equation_branch_filter = standardize_branches(policy_perf)
    )

  if (!all(key_cols %in% names(lookup))) {
    return(ts_calibration)
  }
  for (nm in locality_cols) {
    if (!nm %in% names(lookup)) {
      lookup[[nm]] <- NA_real_
    }
  }

  lookup <- lookup |>
    dplyr::transmute(
      anchor_model_id,
      policy,
      equation_branch_filter,
      lookup_local_min_combined_distance = suppressWarnings(as.numeric(local_min_combined_distance)),
      lookup_local_weighted_mean_combined_distance = suppressWarnings(as.numeric(local_weighted_mean_combined_distance)),
      lookup_local_min_species_distance = suppressWarnings(as.numeric(local_min_species_distance)),
      lookup_local_weighted_mean_species_distance = suppressWarnings(as.numeric(local_weighted_mean_species_distance)),
      lookup_local_min_trait_gower_distance = suppressWarnings(as.numeric(local_min_trait_gower_distance)),
      lookup_local_weighted_mean_trait_gower_distance = suppressWarnings(as.numeric(local_weighted_mean_trait_gower_distance)),
      lookup_local_effective_support = suppressWarnings(as.numeric(local_effective_support)),
      lookup_local_mean_length_overlap = suppressWarnings(as.numeric(local_mean_length_overlap)),
      lookup_local_mean_depth_overlap = suppressWarnings(as.numeric(local_mean_depth_overlap)),
      lookup_policy_slope_len = suppressWarnings(as.numeric(policy_slope_len)),
      lookup_policy_intercept_len = suppressWarnings(as.numeric(policy_intercept_len))
    ) |>
    dplyr::group_by(anchor_model_id, policy, equation_branch_filter) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::starts_with("lookup_"),
        ~ {
          x <- .x[is.finite(.x)]
          if (length(x) == 0L) NA_real_ else stats::median(x)
        }
      ),
      .groups = "drop"
    )

  out <- standardize_policies(ts_calibration) |>
    dplyr::mutate(
      anchor_model_id = as.character(anchor_model_id),
      policy = resolve_policy_names(ts_calibration),
      equation_branch_filter = standardize_branches(ts_calibration)
    ) |>
    dplyr::left_join(lookup, by = key_cols)

  for (nm in locality_cols) {
    lookup_nm <- paste0("lookup_", nm)
    if (!nm %in% names(out)) {
      out[[nm]] <- out[[lookup_nm]]
    } else {
      out[[nm]] <- dplyr::coalesce(
        suppressWarnings(as.numeric(out[[nm]])),
        suppressWarnings(as.numeric(out[[lookup_nm]]))
      )
    }
    out[[lookup_nm]] <- NULL
  }

  out
}

scaled_functional_conformal_quantile <- function(x,
                                                 level) {
  # Use the finite-sample conformal order statistic instead of an interpolated
  # sample quantile so the resulting multiplier remains tied to the benchmark
  # calibration anchors.
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  level <- suppressWarnings(as.numeric(level[[1]] %||% NA_real_))
  if (length(x) == 0L || !is.finite(level)) {
    return(NA_real_)
  }
  level <- min(max(level, 0), 1)
  k <- min(length(x), ceiling((length(x) + 1) * level))
  sort(x)[[k]]
}

fit_scaled_functional_residual_curve <- function(calibration_rows,
                                                 value_col,
                                                 case_weight_col = NULL,
                                                 smooth_spar = 0.6) {
  # Estimate the typical residual size as a smooth function of normalized
  # length position. The scale is modeled with an RMS residual so the fitted
  # curve stays positive without imposing an arbitrary floor.
  #
  # The fitted trend is estimated on the log-scale with calibration-count
  # weights. This stabilizes short-length edge behaviour by borrowing strength
  # across neighboring support positions rather than letting one noisy endpoint
  # dominate the shape allocator.
  calibration_rows <- tibble::as_tibble(calibration_rows)
  if (nrow(calibration_rows) == 0 || !all(c("u", value_col) %in% names(calibration_rows))) {
    return(tibble::tibble())
  }

  scale_tbl <- calibration_rows |>
    dplyr::transmute(
      u = suppressWarnings(as.numeric(u)),
      residual_value = suppressWarnings(as.numeric(.data[[value_col]])),
      case_weight = if (!is.null(case_weight_col) && case_weight_col %in% names(calibration_rows)) {
        suppressWarnings(as.numeric(.data[[case_weight_col]]))
      } else {
        1
      }
    ) |>
    dplyr::filter(
      is.finite(u),
      is.finite(residual_value),
      is.finite(case_weight),
      case_weight > 0
    ) |>
    dplyr::group_by(u) |>
    dplyr::summarise(
      n = sum(case_weight, na.rm = TRUE),
      scale_raw = sqrt(stats::weighted.mean(residual_value^2, w = case_weight, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::arrange(u)

  if (nrow(scale_tbl) == 0) {
    return(scale_tbl)
  }

  scale_tbl <- scale_tbl |>
    dplyr::mutate(
      scale_fit = scale_raw
    )

  # Smooth the RMS scale over normalized length only when enough support
  # positions exist for a stable fit. Prefer a weighted GAM on the log-scale so
  # endpoint spikes are shrunk toward the neighbouring residual trend in a
  # standard, data-adaptive way. Fall back to the original spline when GAM is
  # unavailable or fails.
  if (nrow(scale_tbl) >= 4L) {
    scale_fit <- NULL
    if (requireNamespace("mgcv", quietly = TRUE)) {
      k_now <- max(4L, min(12L, nrow(scale_tbl) - 1L))
      gam_fit <- tryCatch(
        mgcv::gam(
          log(pmax(scale_raw, sqrt(.Machine$double.eps))) ~
            mgcv::s(u, bs = "cs", k = k_now),
          data = scale_tbl,
          weights = pmax(n, 1),
          method = "REML"
        ),
        error = function(e) NULL
      )
      if (!is.null(gam_fit)) {
        scale_fit <- tryCatch(
          exp(as.numeric(stats::predict(gam_fit, newdata = scale_tbl))),
          error = function(e) NULL
        )
      }
    }

    if (is.null(scale_fit)) {
      fit <- tryCatch(
        stats::smooth.spline(
          x = scale_tbl$u,
          y = log(pmax(scale_tbl$scale_raw, sqrt(.Machine$double.eps))),
          w = pmax(scale_tbl$n, 1),
          spar = smooth_spar
        ),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        scale_fit <- tryCatch(
          exp(stats::predict(fit, x = scale_tbl$u)$y),
          error = function(e) NULL
        )
      }
    }

    if (!is.null(scale_fit) && length(scale_fit) == nrow(scale_tbl)) {
      scale_tbl$scale_fit <- scale_fit
    }
  }

  scale_tbl |>
    dplyr::mutate(
      scale_fit = pmax(scale_fit, sqrt(.Machine$double.eps))
    )
}

predict_scaled_functional_curve <- function(scale_curve,
                                            target_u) {
  # Evaluate the smooth residual-scale curve on an arbitrary normalized-length
  # grid while preserving endpoint values outside the observed knots.
  scale_curve <- tibble::as_tibble(scale_curve)
  target_u <- suppressWarnings(as.numeric(target_u))
  if (nrow(scale_curve) == 0 || !"scale_fit" %in% names(scale_curve)) {
    return(rep(NA_real_, length(target_u)))
  }

  curve_u <- suppressWarnings(as.numeric(scale_curve$u))
  curve_scale <- suppressWarnings(as.numeric(scale_curve$scale_fit))
  keep <- is.finite(curve_u) & is.finite(curve_scale)
  if (!any(keep)) {
    return(rep(NA_real_, length(target_u)))
  }
  if (sum(keep) == 1L) {
    return(rep(curve_scale[keep][[1]], length(target_u)))
  }

  stats::approx(
    x = curve_u[keep],
    y = curve_scale[keep],
    xout = target_u,
    rule = 2,
    ties = "ordered"
  )$y
}

build_scaled_functional_conformal_curve <- function(calibration_rows,
                                                    value_col,
                                                    levels = c(0.80, 0.90, 0.95, 0.99),
                                                    smooth_spar = 0.6) {
  # Cross-fit the residual-scale curve by leaving one benchmark anchor out at
  # a time, then conformalize the held-out supremum scores. The final envelope
  # shape comes from the full-data scale curve and the conformal multipliers
  # come from the out-of-fold scores.
  calibration_rows <- tibble::as_tibble(calibration_rows)
  if (nrow(calibration_rows) == 0 ||
      !all(c("anchor_model_id", "u", value_col) %in% names(calibration_rows))) {
    return(tibble::tibble())
  }

  levels <- suppressWarnings(as.numeric(levels))
  levels <- levels[is.finite(levels) & levels > 0 & levels < 1]
  if (length(levels) == 0L) {
    return(tibble::tibble())
  }

  rows_now <- calibration_rows |>
    dplyr::transmute(
      anchor_model_id = as.character(anchor_model_id),
      u = suppressWarnings(as.numeric(u)),
      residual_value = suppressWarnings(as.numeric(.data[[value_col]]))
    ) |>
    dplyr::filter(
      !is.na(anchor_model_id),
      nzchar(anchor_model_id),
      is.finite(u),
      is.finite(residual_value)
    )
  if (nrow(rows_now) == 0) {
    return(tibble::tibble())
  }

  anchor_ids <- unique(rows_now$anchor_model_id)
  if (length(anchor_ids) < 2L) {
    return(tibble::tibble())
  }

  score_tbl <- purrr::map_dfr(anchor_ids, function(anchor_id_now) {
    # Fit the scale curve on every anchor except the held-out one so each
    # conformal score is genuinely out-of-fold.
    train_rows <- rows_now |>
      dplyr::filter(anchor_model_id != anchor_id_now)
    holdout_rows <- rows_now |>
      dplyr::filter(anchor_model_id == anchor_id_now)
    if (nrow(train_rows) == 0 || nrow(holdout_rows) == 0) {
      return(tibble::tibble())
    }

    scale_curve <- fit_scaled_functional_residual_curve(
      calibration_rows = train_rows,
      value_col = "residual_value",
      smooth_spar = smooth_spar
    )
    if (nrow(scale_curve) == 0) {
      return(tibble::tibble())
    }

    scale_pred <- predict_scaled_functional_curve(
      scale_curve = scale_curve,
      target_u = holdout_rows$u
    )
    score_value <- max(
      abs(holdout_rows$residual_value) /
        pmax(scale_pred, sqrt(.Machine$double.eps)),
      na.rm = TRUE
    )
    if (!is.finite(score_value)) {
      return(tibble::tibble())
    }

    tibble::tibble(
      anchor_model_id = anchor_id_now,
      conformal_score = score_value
    )
  })
  if (nrow(score_tbl) == 0) {
    return(tibble::tibble())
  }

  full_scale_curve <- fit_scaled_functional_residual_curve(
    calibration_rows = rows_now,
    value_col = "residual_value",
    smooth_spar = smooth_spar
  )
  if (nrow(full_scale_curve) == 0) {
    return(tibble::tibble())
  }

  out <- full_scale_curve |>
    dplyr::transmute(
      u = suppressWarnings(as.numeric(u)),
      n = suppressWarnings(as.numeric(n)),
      scale_fit = suppressWarnings(as.numeric(scale_fit))
    ) |>
    dplyr::arrange(u)

  # Convert the out-of-fold score distribution into level-specific envelope
  # multipliers, then map those multipliers back onto the smooth scale curve.
  for (level_now in levels) {
    suffix <- paste0(formatC(round(level_now * 100), format = "d", width = 0))
    multiplier_now <- scaled_functional_conformal_quantile(
      x = score_tbl$conformal_score,
      level = level_now
    )
    out[[paste0("q", suffix, "_abs_dev")]] <- multiplier_now * out$scale_fit
    out[[paste0("score_multiplier_", suffix)]] <- multiplier_now
  }

  out |>
    dplyr::mutate(
      score_n = nrow(score_tbl),
      anchor_n = length(anchor_ids)
    )
}

build_scaled_functional_ts_calibration <- function(ts_rows,
                                                   smooth_spar = 0.6) {
  # Build paired TS-scale and log-sigma-scale envelopes from the same selected
  # benchmark rows so the acoustic panels and multiplier curves remain tied to
  # the same cross-fit residual pool.
  ts_rows <- tibble::as_tibble(ts_rows)
  if (nrow(ts_rows) == 0 || !all(c("anchor_model_id", "u", "ts_error", "log_sigma_residual") %in% names(ts_rows))) {
    return(tibble::tibble())
  }

  ts_curve <- build_scaled_functional_conformal_curve(
    calibration_rows = ts_rows,
    value_col = "ts_error",
    levels = c(0.80, 0.90, 0.95, 0.99),
    smooth_spar = smooth_spar
  )
  sigma_curve <- build_scaled_functional_conformal_curve(
    calibration_rows = ts_rows,
    value_col = "log_sigma_residual",
    levels = c(0.80, 0.90, 0.95, 0.99),
    smooth_spar = smooth_spar
  )
  if (nrow(ts_curve) == 0 || nrow(sigma_curve) == 0) {
    return(tibble::tibble())
  }

  out <- ts_curve |>
    dplyr::rename(
      ts_scale_fit = scale_fit,
      q80_ts_abs_dev = q80_abs_dev,
      q90_ts_abs_dev = q90_abs_dev,
      q95_ts_abs_dev = q95_abs_dev,
      q99_ts_abs_dev = q99_abs_dev,
      ts_score_multiplier_80 = score_multiplier_80,
      ts_score_multiplier_90 = score_multiplier_90,
      ts_score_multiplier_95 = score_multiplier_95,
      ts_score_multiplier_99 = score_multiplier_99,
      ts_score_n = score_n,
      ts_anchor_n = anchor_n
    ) |>
    dplyr::left_join(
      sigma_curve |>
        dplyr::rename(
          log_sigma_scale_fit = scale_fit,
          q80_log_sigma_abs_dev = q80_abs_dev,
          q90_log_sigma_abs_dev = q90_abs_dev,
          q95_log_sigma_abs_dev = q95_abs_dev,
          q99_log_sigma_abs_dev = q99_abs_dev,
          sigma_score_multiplier_80 = score_multiplier_80,
          sigma_score_multiplier_90 = score_multiplier_90,
          sigma_score_multiplier_95 = score_multiplier_95,
          sigma_score_multiplier_99 = score_multiplier_99,
          sigma_score_n = score_n,
          sigma_anchor_n = anchor_n
        ) |>
        dplyr::select(
          u,
          log_sigma_scale_fit,
          q80_log_sigma_abs_dev,
          q90_log_sigma_abs_dev,
          q95_log_sigma_abs_dev,
          q99_log_sigma_abs_dev,
          sigma_score_multiplier_80,
          sigma_score_multiplier_90,
          sigma_score_multiplier_95,
          sigma_score_multiplier_99,
          sigma_score_n,
          sigma_anchor_n
        ),
      by = "u"
    ) |>
    dplyr::mutate(
      # The selected-policy line is the center of the displayed band, so the
      # residual envelopes are symmetric around zero rather than bias-shifted.
      median_ts_error = 0,
      q80_ts_abs_raw = q80_ts_abs_dev,
      q90_ts_abs_raw = q90_ts_abs_dev,
      q95_ts_abs_raw = q95_ts_abs_dev,
      q99_ts_abs_raw = q99_ts_abs_dev,
      median_log_sigma_residual = 0,
      q10_log_sigma_residual = -q90_log_sigma_abs_dev,
      q90_log_sigma_residual = q90_log_sigma_abs_dev,
      q05_log_sigma_residual = -q95_log_sigma_abs_dev,
      q95_log_sigma_residual = q95_log_sigma_abs_dev,
      q025_log_sigma_residual = -q99_log_sigma_abs_dev,
      q975_log_sigma_residual = q99_log_sigma_abs_dev,
      q005_log_sigma_residual = -q99_log_sigma_abs_dev,
      q995_log_sigma_residual = q99_log_sigma_abs_dev
    )

  out
}

prepare_residual_scale_training <- function(calibration_rows,
                                            response_col) {
  # Build one numeric training frame for anchor-conditional residual-width
  # modeling. The response is the absolute residual magnitude, while the
  # predictors describe the realized donor geometry for each benchmark anchor.
  calibration_rows <- tibble::as_tibble(calibration_rows)
  if (nrow(calibration_rows) == 0 || !response_col %in% names(calibration_rows)) {
    return(tibble::tibble())
  }

  out <- calibration_rows |>
    dplyr::transmute(
      anchor_model_id = as.character(anchor_model_id),
      u = if ("u" %in% names(calibration_rows)) suppressWarnings(as.numeric(u)) else NA_real_,
      abs_residual = abs(suppressWarnings(as.numeric(.data[[response_col]]))),
      local_min_combined_distance = if ("local_min_combined_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(local_min_combined_distance)) else NA_real_,
      local_weighted_mean_combined_distance = if ("local_weighted_mean_combined_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(local_weighted_mean_combined_distance)) else NA_real_,
      local_min_species_distance = if ("local_min_species_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(local_min_species_distance)) else NA_real_,
      local_weighted_mean_species_distance = if ("local_weighted_mean_species_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(local_weighted_mean_species_distance)) else NA_real_,
      local_min_trait_gower_distance = if ("local_min_trait_gower_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(local_min_trait_gower_distance)) else NA_real_,
      local_weighted_mean_trait_gower_distance = if ("local_weighted_mean_trait_gower_distance" %in% names(calibration_rows)) suppressWarnings(as.numeric(local_weighted_mean_trait_gower_distance)) else NA_real_,
      local_effective_support = if ("local_effective_support" %in% names(calibration_rows)) suppressWarnings(as.numeric(local_effective_support)) else NA_real_,
      local_mean_length_overlap = if ("local_mean_length_overlap" %in% names(calibration_rows)) suppressWarnings(as.numeric(local_mean_length_overlap)) else NA_real_,
      local_mean_depth_overlap = if ("local_mean_depth_overlap" %in% names(calibration_rows)) suppressWarnings(as.numeric(local_mean_depth_overlap)) else NA_real_,
      policy_slope_len = if ("policy_slope_len" %in% names(calibration_rows)) suppressWarnings(as.numeric(policy_slope_len)) else NA_real_,
      policy_intercept_len = if ("policy_intercept_len" %in% names(calibration_rows)) suppressWarnings(as.numeric(policy_intercept_len)) else NA_real_
    ) |>
    dplyr::mutate(
      log_abs_residual = log1p(pmax(abs_residual, 0)),
      log_local_effective_support = dplyr::if_else(
        is.finite(local_effective_support),
        log1p(pmax(local_effective_support, 0)),
        NA_real_
      )
    ) |>
    dplyr::filter(
      !is.na(anchor_model_id),
      nzchar(anchor_model_id),
      is.finite(abs_residual)
    )

  out
}

residual_scale_predictor_columns <- function(training_tbl,
                                             include_u = TRUE) {
  # Keep only predictors that actually vary within the calibration pool so the
  # quantile models remain estimable without bespoke fallback branches.
  training_tbl <- tibble::as_tibble(training_tbl)
  if (nrow(training_tbl) == 0) {
    return(character())
  }

  candidate_cols <- c(
    "local_min_combined_distance",
    "local_weighted_mean_combined_distance",
    "local_min_species_distance",
    "local_weighted_mean_species_distance",
    "local_min_trait_gower_distance",
    "local_weighted_mean_trait_gower_distance",
    "log_local_effective_support",
    "local_mean_length_overlap",
    "local_mean_depth_overlap",
    "policy_slope_len",
    "policy_intercept_len"
  )
  predictor_cols <- candidate_cols[candidate_cols %in% names(training_tbl)]
  predictor_cols <- predictor_cols[vapply(
    predictor_cols,
    function(nm) {
      vals <- suppressWarnings(as.numeric(training_tbl[[nm]]))
      vals <- vals[is.finite(vals)]
      length(unique(vals)) >= 2L
    },
    logical(1)
  )]

  if (isTRUE(include_u) && "u" %in% names(training_tbl)) {
    u_vals <- suppressWarnings(as.numeric(training_tbl$u))
    if (length(unique(u_vals[is.finite(u_vals)])) >= 2L) {
      predictor_cols <- c("u", predictor_cols)
    }
  }

  predictor_cols
}

fit_residual_scale_quantile_model <- function(calibration_rows,
                                              response_col,
                                              tau,
                                              include_u = TRUE) {
  # Fit one quantile-regression model for absolute residual magnitude. The fit
  # is anchor-conditional because donor-distance and overlap summaries are part
  # of the design matrix rather than being handled by policy-class lookup.
  training_tbl <- prepare_residual_scale_training(
    calibration_rows = calibration_rows,
    response_col = response_col
  )
  if (nrow(training_tbl) < 2L) {
    return(NULL)
  }

  predictor_cols <- residual_scale_predictor_columns(
    training_tbl = training_tbl,
    include_u = include_u
  )
  use_u <- "u" %in% predictor_cols
  base_predictors <- setdiff(predictor_cols, "u")
  term_labels <- character(0)
  if (use_u) {
    n_unique_u <- length(unique(training_tbl$u[is.finite(training_tbl$u)]))
    if (n_unique_u >= 4L) {
      term_labels <- c(term_labels, sprintf("splines::ns(u, df = %d)", min(4L, n_unique_u - 1L)))
    } else {
      term_labels <- c(term_labels, "u")
    }
  }
  term_labels <- c(term_labels, base_predictors)
  if (length(term_labels) == 0L) {
    term_labels <- "1"
  }

  formula_now <- stats::as.formula(
    paste("log_abs_residual ~", paste(term_labels, collapse = " + "))
  )
  fit_now <- tryCatch(
    quantreg::rq(
      formula_now,
      tau = suppressWarnings(as.numeric(tau)[[1]]),
      data = training_tbl,
      method = "fn"
    ),
    error = function(e) NULL
  )
  if (is.null(fit_now)) {
    return(NULL)
  }

  default_map <- lapply(base_predictors, function(nm) {
    vals <- suppressWarnings(as.numeric(training_tbl[[nm]]))
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0L) {
      NA_real_
    } else {
      stats::median(vals, na.rm = TRUE)
    }
  })
  names(default_map) <- base_predictors
  range_map <- lapply(c(if (use_u) "u" else character(), base_predictors), function(nm) {
    vals <- suppressWarnings(as.numeric(training_tbl[[nm]]))
    vals <- vals[is.finite(vals)]
    c(
      min = if (length(vals) == 0L) NA_real_ else min(vals, na.rm = TRUE),
      max = if (length(vals) == 0L) NA_real_ else max(vals, na.rm = TRUE)
    )
  })
  names(range_map) <- c(if (use_u) "u" else character(), base_predictors)

  list(
    fit = fit_now,
    predictor_cols = base_predictors,
    include_u = use_u,
    default_values = default_map,
    predictor_ranges = range_map,
    anchor_n = length(unique(training_tbl$anchor_model_id)),
    row_n = nrow(training_tbl)
  )
}

predict_residual_scale_quantile_model <- function(model_obj,
                                                  row_now,
                                                  target_u = NULL) {
  # Evaluate the fitted residual-width model for one selected row across an
  # optional normalized-length grid. Predictors missing on the selected row use
  # the calibration-pool median recorded at fit time.
  if (is.null(model_obj) || nrow(tibble::as_tibble(row_now)) == 0) {
    return(rep(NA_real_, length(target_u %||% 1)))
  }

  row_now <- tibble::as_tibble(row_now)
  target_u <- suppressWarnings(as.numeric(target_u %||% NA_real_))
  if (!length(target_u)) {
    target_u <- NA_real_
  }
  new_tbl <- tibble::tibble(u = target_u)
  if (isTRUE(model_obj$include_u)) {
    u_range <- suppressWarnings(as.numeric(model_obj$predictor_ranges$u %||% c(NA_real_, NA_real_)))
    if (length(u_range) >= 2L && all(is.finite(u_range))) {
      new_tbl$u <- pmin(pmax(new_tbl$u, u_range[[1]]), u_range[[2]])
    }
  }
  for (nm in model_obj$predictor_cols %||% character()) {
    value_now <- if (nm %in% names(row_now)) {
      suppressWarnings(as.numeric(row_now[[nm]][[1]]))
    } else if (identical(nm, "log_local_effective_support") &&
               "local_effective_support" %in% names(row_now)) {
      suppressWarnings(log1p(pmax(as.numeric(row_now$local_effective_support[[1]]), 0)))
    } else {
      NA_real_
    }
    if (!is.finite(value_now)) {
      value_now <- suppressWarnings(as.numeric(model_obj$default_values[[nm]] %||% NA_real_))
    }
    value_range <- suppressWarnings(as.numeric(model_obj$predictor_ranges[[nm]] %||% c(NA_real_, NA_real_)))
    if (length(value_range) >= 2L && all(is.finite(value_range)) && is.finite(value_now)) {
      value_now <- min(max(value_now, value_range[[1]]), value_range[[2]])
    }
    new_tbl[[nm]] <- rep(value_now, nrow(new_tbl))
  }

  pred_now <- tryCatch(
    suppressWarnings(as.numeric(stats::predict(model_obj$fit, newdata = new_tbl))),
    error = function(e) rep(NA_real_, nrow(new_tbl))
  )
  pred_now <- pmax(expm1(pred_now), 0, na.rm = TRUE)
  pred_now
}

build_anchor_specific_ts_width_curve <- function(ts_rows,
                                                 row_now,
                                                 target_u,
                                                 smooth_spar = 0.6) {
  # Build descriptive TS and log-sigma width curves from the full selected
  # policy/branch residual pool while continuously downweighting benchmark
  # anchors whose realized donor geometry differs from the selected row.
  ts_rows <- tibble::as_tibble(ts_rows)
  row_now <- tibble::as_tibble(row_now)
  target_u <- suppressWarnings(as.numeric(target_u))
  if (nrow(ts_rows) == 0 || nrow(row_now) == 0 || !length(target_u)) {
    return(tibble::tibble())
  }

  target_u <- pmin(pmax(target_u, 0), 1)
  predictor_candidates <- c(
    "u",
    "local_min_combined_distance",
    "local_weighted_mean_combined_distance",
    "local_mean_length_overlap",
    "local_mean_depth_overlap",
    "local_effective_support"
  )
  predictor_available <- intersect(predictor_candidates, names(ts_rows))
  if (!"u" %in% predictor_available ||
      !"ts_error" %in% names(ts_rows) ||
      !"log_sigma_residual" %in% names(ts_rows)) {
    return(tibble::tibble())
  }
  build_weighted_curve <- function(value_col,
                                   signed = FALSE) {
    training_tbl <- ts_rows |>
      dplyr::transmute(
        anchor_model_id = as.character(anchor_model_id),
        u = suppressWarnings(as.numeric(u)),
        local_min_combined_distance = if ("local_min_combined_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_min_combined_distance)) else NA_real_,
        local_weighted_mean_combined_distance = if ("local_weighted_mean_combined_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_weighted_mean_combined_distance)) else NA_real_,
        local_min_species_distance = if ("local_min_species_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_min_species_distance)) else NA_real_,
        local_weighted_mean_species_distance = if ("local_weighted_mean_species_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_weighted_mean_species_distance)) else NA_real_,
        local_min_trait_gower_distance = if ("local_min_trait_gower_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_min_trait_gower_distance)) else NA_real_,
        local_weighted_mean_trait_gower_distance = if ("local_weighted_mean_trait_gower_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_weighted_mean_trait_gower_distance)) else NA_real_,
        local_mean_length_overlap = if ("local_mean_length_overlap" %in% names(ts_rows)) suppressWarnings(as.numeric(local_mean_length_overlap)) else NA_real_,
        local_mean_depth_overlap = if ("local_mean_depth_overlap" %in% names(ts_rows)) suppressWarnings(as.numeric(local_mean_depth_overlap)) else NA_real_,
        log_local_effective_support = if ("local_effective_support" %in% names(ts_rows)) suppressWarnings(log1p(pmax(as.numeric(local_effective_support), 0))) else NA_real_,
        policy_slope_len = if ("policy_slope_len" %in% names(ts_rows)) suppressWarnings(as.numeric(policy_slope_len)) else NA_real_,
        policy_intercept_len = if ("policy_intercept_len" %in% names(ts_rows)) suppressWarnings(as.numeric(policy_intercept_len)) else NA_real_,
        residual = suppressWarnings(as.numeric(.data[[value_col]]))
      ) |>
      dplyr::filter(
        !is.na(anchor_model_id),
        nzchar(anchor_model_id),
        is.finite(u),
        is.finite(residual)
      )
    if (nrow(training_tbl) < 2L) {
      return(NULL)
    }

    # Anchor-level similarity should determine which benchmark curves matter
    # for the selected row. Length dependence itself should then be estimated
    # pointwise over the shared normalized u-grid, rather than being diluted by
    # averaging over every u-value at once.
    base_weights <- locality_similarity_weights(
      training_tbl = training_tbl,
      row_now = row_now,
      target_u = stats::median(target_u, na.rm = TRUE),
      include_u = FALSE
    )
    if (length(base_weights) != nrow(training_tbl) || !any(base_weights > 0, na.rm = TRUE)) {
      base_weights <- rep(1, nrow(training_tbl))
    }

    u_grid <- sort(unique(training_tbl$u[is.finite(training_tbl$u)]))
    if (!length(u_grid)) {
      return(NULL)
    }

    q_mat <- vapply(
      target_u,
      function(u_now) {
        u_match <- u_grid[[which.min(abs(u_grid - u_now))]]
        idx_now <- which(is.finite(training_tbl$u) & abs(training_tbl$u - u_match) <= 1e-8)
        if (!length(idx_now)) {
          if (isTRUE(signed)) {
            return(c(lo80 = NA_real_, hi80 = NA_real_, lo90 = NA_real_, hi90 = NA_real_, lo95 = NA_real_, hi95 = NA_real_, lo99 = NA_real_, hi99 = NA_real_))
          }
          return(c(q80 = NA_real_, q90 = NA_real_, q95 = NA_real_, q99 = NA_real_))
        }
        w_now <- base_weights[idx_now]
        residual_now <- training_tbl$residual[idx_now]
        if (isTRUE(signed)) {
          c(
            lo80 = weighted_quantile_or_na(residual_now, w_now, 0.10),
            hi80 = weighted_quantile_or_na(residual_now, w_now, 0.90),
            lo90 = weighted_quantile_or_na(residual_now, w_now, 0.05),
            hi90 = weighted_quantile_or_na(residual_now, w_now, 0.95),
            lo95 = weighted_quantile_or_na(residual_now, w_now, 0.025),
            hi95 = weighted_quantile_or_na(residual_now, w_now, 0.975),
            lo99 = weighted_quantile_or_na(residual_now, w_now, 0.005),
            hi99 = weighted_quantile_or_na(residual_now, w_now, 0.995)
          )
        } else {
          residual_now <- abs(residual_now)
          c(
            q80 = weighted_quantile_or_na(residual_now, w_now, 0.80),
            q90 = weighted_quantile_or_na(residual_now, w_now, 0.90),
            q95 = weighted_quantile_or_na(residual_now, w_now, 0.95),
            q99 = weighted_quantile_or_na(residual_now, w_now, 0.99)
          )
        }
      },
      if (isTRUE(signed)) numeric(8) else numeric(4)
    )
    if (is.null(dim(q_mat))) {
      q_mat <- matrix(q_mat, nrow = if (isTRUE(signed)) 8L else 4L)
    }
    if (isTRUE(signed)) {
      lo80 <- smooth_weighted_curve(target_u, q_mat[1, ], spar = smooth_spar)
      hi80 <- smooth_weighted_curve(target_u, q_mat[2, ], spar = smooth_spar)
      lo90 <- smooth_weighted_curve(target_u, q_mat[3, ], spar = smooth_spar)
      hi90 <- smooth_weighted_curve(target_u, q_mat[4, ], spar = smooth_spar)
      lo95 <- smooth_weighted_curve(target_u, q_mat[5, ], spar = smooth_spar)
      hi95 <- smooth_weighted_curve(target_u, q_mat[6, ], spar = smooth_spar)
      lo99 <- smooth_weighted_curve(target_u, q_mat[7, ], spar = smooth_spar)
      hi99 <- smooth_weighted_curve(target_u, q_mat[8, ], spar = smooth_spar)
      lo80 <- pmin(lo80, 0, na.rm = TRUE)
      lo90 <- pmin(lo90, lo80, 0, na.rm = TRUE)
      lo95 <- pmin(lo95, lo90, 0, na.rm = TRUE)
      lo99 <- pmin(lo99, lo95, 0, na.rm = TRUE)
      hi80 <- pmax(hi80, 0, na.rm = TRUE)
      hi90 <- pmax(hi90, hi80, 0, na.rm = TRUE)
      hi95 <- pmax(hi95, hi90, 0, na.rm = TRUE)
      hi99 <- pmax(hi99, hi95, 0, na.rm = TRUE)
      return(list(
        anchor_n = dplyr::n_distinct(training_tbl$anchor_model_id),
        score_n = nrow(training_tbl),
        lo80 = lo80,
        hi80 = hi80,
        lo90 = lo90,
        hi90 = hi90,
        lo95 = lo95,
        hi95 = hi95,
        lo99 = lo99,
        hi99 = hi99,
        base_weight_n = sum(base_weights > 0, na.rm = TRUE)
      ))
    }
    q80 <- smooth_weighted_curve(target_u, q_mat[1, ], spar = smooth_spar)
    q90 <- smooth_weighted_curve(target_u, q_mat[2, ], spar = smooth_spar)
    q95 <- smooth_weighted_curve(target_u, q_mat[3, ], spar = smooth_spar)
    q99 <- smooth_weighted_curve(target_u, q_mat[4, ], spar = smooth_spar)
    q80 <- pmax(q80, 0, na.rm = TRUE)
    q90 <- pmax(q90, q80, 0, na.rm = TRUE)
    q95 <- pmax(q95, q90, 0, na.rm = TRUE)
    q99 <- pmax(q99, q95, 0, na.rm = TRUE)
    list(
      anchor_n = dplyr::n_distinct(training_tbl$anchor_model_id),
      score_n = nrow(training_tbl),
      q80 = q80,
      q90 = q90,
      q95 = q95,
      q99 = q99,
      base_weight_n = sum(base_weights > 0, na.rm = TRUE)
    )
  }

  build_weighted_scale_fit <- function(value_col) {
    training_tbl <- ts_rows |>
      dplyr::transmute(
        anchor_model_id = as.character(anchor_model_id),
        u = suppressWarnings(as.numeric(u)),
        local_min_combined_distance = if ("local_min_combined_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_min_combined_distance)) else NA_real_,
        local_weighted_mean_combined_distance = if ("local_weighted_mean_combined_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_weighted_mean_combined_distance)) else NA_real_,
        local_min_species_distance = if ("local_min_species_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_min_species_distance)) else NA_real_,
        local_weighted_mean_species_distance = if ("local_weighted_mean_species_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_weighted_mean_species_distance)) else NA_real_,
        local_min_trait_gower_distance = if ("local_min_trait_gower_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_min_trait_gower_distance)) else NA_real_,
        local_weighted_mean_trait_gower_distance = if ("local_weighted_mean_trait_gower_distance" %in% names(ts_rows)) suppressWarnings(as.numeric(local_weighted_mean_trait_gower_distance)) else NA_real_,
        local_mean_length_overlap = if ("local_mean_length_overlap" %in% names(ts_rows)) suppressWarnings(as.numeric(local_mean_length_overlap)) else NA_real_,
        local_mean_depth_overlap = if ("local_mean_depth_overlap" %in% names(ts_rows)) suppressWarnings(as.numeric(local_mean_depth_overlap)) else NA_real_,
        log_local_effective_support = if ("local_effective_support" %in% names(ts_rows)) suppressWarnings(log1p(pmax(as.numeric(local_effective_support), 0))) else NA_real_,
        policy_slope_len = if ("policy_slope_len" %in% names(ts_rows)) suppressWarnings(as.numeric(policy_slope_len)) else NA_real_,
        policy_intercept_len = if ("policy_intercept_len" %in% names(ts_rows)) suppressWarnings(as.numeric(policy_intercept_len)) else NA_real_,
        residual = suppressWarnings(as.numeric(.data[[value_col]]))
      ) |>
      dplyr::filter(
        !is.na(anchor_model_id),
        nzchar(anchor_model_id),
        is.finite(u),
        is.finite(residual)
      )
    if (nrow(training_tbl) < 2L) {
      return(NULL)
    }

    base_weights <- locality_similarity_weights(
      training_tbl = training_tbl,
      row_now = row_now,
      target_u = stats::median(target_u, na.rm = TRUE),
      include_u = FALSE
    )
    if (length(base_weights) != nrow(training_tbl) || !any(base_weights > 0, na.rm = TRUE)) {
      base_weights <- rep(1, nrow(training_tbl))
    }
    training_tbl$local_weight <- base_weights

    fit_now <- fit_scaled_functional_residual_curve(
      calibration_rows = training_tbl,
      value_col = "residual",
      case_weight_col = "local_weight",
      smooth_spar = smooth_spar
    )
    if (nrow(fit_now) == 0) {
      return(NULL)
    }

    tibble::tibble(
      u = target_u,
      scale_fit = predict_scaled_functional_curve(
        scale_curve = fit_now,
        target_u = target_u
      )
    )
  }

  ts_scale <- build_weighted_curve("ts_error", signed = TRUE)
  sigma_scale <- build_weighted_curve("log_sigma_residual")
  sigma_scale_fit <- build_weighted_scale_fit("log_sigma_residual")
  if (is.null(ts_scale) || is.null(sigma_scale) || is.null(sigma_scale_fit)) {
    return(tibble::tibble())
  }

  tibble::tibble(
    u = target_u,
    n = nrow(ts_rows),
    ts_anchor_n = ts_scale$anchor_n,
    ts_score_n = ts_scale$score_n,
    sigma_anchor_n = sigma_scale$anchor_n,
    sigma_score_n = sigma_scale$score_n,
    median_ts_error = 0,
    q10_ts_error = ts_scale$lo80,
    q90_ts_error = ts_scale$hi80,
    q05_ts_error = ts_scale$lo90,
    q95_ts_error = ts_scale$hi90,
    q025_ts_error = ts_scale$lo95,
    q975_ts_error = ts_scale$hi95,
    q005_ts_error = ts_scale$lo99,
    q995_ts_error = ts_scale$hi99,
    q80_ts_abs_raw = 0.5 * (ts_scale$hi80 - ts_scale$lo80),
    q90_ts_abs_raw = 0.5 * (ts_scale$hi90 - ts_scale$lo90),
    q95_ts_abs_raw = 0.5 * (ts_scale$hi95 - ts_scale$lo95),
    q99_ts_abs_raw = 0.5 * (ts_scale$hi99 - ts_scale$lo99),
    q80_ts_abs_dev = 0.5 * (ts_scale$hi80 - ts_scale$lo80),
    q90_ts_abs_dev = 0.5 * (ts_scale$hi90 - ts_scale$lo90),
    q95_ts_abs_dev = 0.5 * (ts_scale$hi95 - ts_scale$lo95),
    q99_ts_abs_dev = 0.5 * (ts_scale$hi99 - ts_scale$lo99),
    median_log_sigma_residual = 0,
    log_sigma_scale_fit = sigma_scale_fit$scale_fit,
    q80_log_sigma_abs_dev = sigma_scale$q80,
    q90_log_sigma_abs_dev = sigma_scale$q90,
    q95_log_sigma_abs_dev = sigma_scale$q95,
    q99_log_sigma_abs_dev = sigma_scale$q99,
    q10_log_sigma_residual = -sigma_scale$q90,
    q90_log_sigma_residual = sigma_scale$q90,
    q05_log_sigma_residual = -sigma_scale$q95,
    q95_log_sigma_residual = sigma_scale$q95,
    q025_log_sigma_residual = -sigma_scale$q99,
    q975_log_sigma_residual = sigma_scale$q99,
    q005_log_sigma_residual = -sigma_scale$q99,
    q995_log_sigma_residual = sigma_scale$q99
  )
}

select_ts_calibration_curve <- function(ts_calibration,
                                        row_now,
                                        target_u = seq(0, 1, length.out = 400L),
                                        min_anchor_neighbors = 10L) {
  # Restrict the calibration pool to the selected policy and branch, then fit
  # one locality-conditioned residual-scale model on that full pool. The
  # resulting curve remains descriptive, anchor-specific, and free of any
  # support-bin or fallback broadening logic.
  ts_calibration <- tibble::as_tibble(ts_calibration)
  row_now <- tibble::as_tibble(row_now)
  if (nrow(ts_calibration) == 0 || nrow(row_now) == 0) {
    return(tibble::tibble())
  }

  policy_value <- as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_)
  branch_value <- selected_branches(row_now)[[1]]
  if ("policy" %in% names(ts_calibration)) {
    ts_calibration$policy <- as.character(ts_calibration$policy)
  } else {
    ts_calibration$policy <- NA_character_
  }
  ts_calibration$equation_branch_filter <- standardize_branches(ts_calibration)
  if (is.na(policy_value) || !nzchar(policy_value) ||
      is.na(branch_value) || !nzchar(branch_value)) {
    return(tibble::tibble())
  }

  pool_now <- ts_calibration |>
    dplyr::filter(
      policy == !!policy_value,
      equation_branch_filter == !!branch_value
    )
  if (nrow(pool_now) == 0 || dplyr::n_distinct(pool_now$anchor_model_id) < 2L) {
    return(tibble::tibble())
  }
  curve_now <- build_anchor_specific_ts_width_curve(
    ts_rows = pool_now,
    row_now = row_now,
    target_u = target_u
  )
  if (nrow(curve_now) == 0) {
    return(tibble::tibble())
  }

  curve_now
}

anchor_local_log_sigma_calibration <- function(row_now,
                                               ts_calibration,
                                               candidate_models,
                                               length_grid_n = 400L,
                                               min_anchor_neighbors = 10L) {
  row_now <- tibble::as_tibble(row_now)
  candidate_models <- tibble::as_tibble(candidate_models)
  if (nrow(row_now) == 0 || nrow(candidate_models) == 0) {
    return(NULL)
  }
  curve <- select_ts_calibration_curve(
    ts_calibration = ts_calibration,
    row_now = row_now,
    min_anchor_neighbors = min_anchor_neighbors
  )
  if (nrow(curve) == 0) {
    return(NULL)
  }
  id_col <- if ("model_id_chr" %in% names(candidate_models)) "model_id_chr" else "model_id"
  anchor_id_chr <- as.character(row_now$anchor_model_id[[1]] %||% NA_character_)
  anchor_row <- candidate_models |>
    dplyr::filter(as.character(.data[[id_col]]) == anchor_id_chr) |>
    dplyr::slice(1)
  if (nrow(anchor_row) == 0) {
    return(NULL)
  }
  anchor_pdf <- anchor_pdf_from_row(anchor_row, n = length_grid_n)
  if (nrow(anchor_pdf) == 0) {
    return(NULL)
  }
  target_u <- seq(0, 1, length.out = nrow(anchor_pdf))
  approx_curve <- function(candidates) {
    values <- resolve_numeric_candidates(curve, candidates)
    keep <- is.finite(curve$u) & is.finite(values)
    if (!any(keep)) {
      return(rep(NA_real_, length(target_u)))
    }
    if (sum(keep) == 1) {
      return(rep(values[keep][[1]], length(target_u)))
    }
    stats::approx(
      x = curve$u[keep],
      y = values[keep],
      xout = target_u,
      rule = 2,
      ties = "ordered"
    )$y
  }
  pdf_weights <- suppressWarnings(as.numeric(anchor_pdf$f_len))
  if (!any(is.finite(pdf_weights) & pdf_weights > 0)) {
    pdf_weights <- rep(1 / nrow(anchor_pdf), nrow(anchor_pdf))
  } else {
    pdf_weights <- pdf_weights / sum(pdf_weights, na.rm = TRUE)
  }
  q95_abs_dev <- approx_curve(c("q95_log_sigma_abs_dev_smooth", "q95_log_sigma_abs_dev"))
  q99_abs_dev <- approx_curve(c("q99_log_sigma_abs_dev_smooth", "q99_log_sigma_abs_dev"))
  median_shift <- approx_curve(c("median_log_sigma_residual_smooth", "median_log_sigma_residual"))
  list(
    q95 = stats::weighted.mean(q95_abs_dev, pdf_weights, na.rm = TRUE),
    q99 = stats::weighted.mean(q99_abs_dev, pdf_weights, na.rm = TRUE),
    median_shift = stats::weighted.mean(median_shift, pdf_weights, na.rm = TRUE)
  )
}

select_coefficient_residual_pool <- function(coefficient_calibration,
                                             row_now,
                                             min_rows = 2L) {
  coefficient_calibration <- tibble::as_tibble(coefficient_calibration)
  row_now <- tibble::as_tibble(row_now)
  if (nrow(coefficient_calibration) == 0 || nrow(row_now) == 0) {
    return(tibble::tibble())
  }

  coefficient_calibration <- coefficient_calibration |>
    dplyr::mutate(
      anchor_model_id = if ("anchor_model_id" %in% names(coefficient_calibration)) as.character(anchor_model_id) else NA_character_,
      anchor_species = if ("anchor_species" %in% names(coefficient_calibration)) as.character(anchor_species) else NA_character_,
      anchor_family = if ("anchor_family" %in% names(coefficient_calibration)) as.character(anchor_family) else NA_character_,
      policy = as.character(policy),
      equation_branch_filter = standardize_branches(coefficient_calibration),
      post_selection_support_bin = as.character(post_selection_support_bin),
      slope_resid = suppressWarnings(as.numeric(slope_resid)),
      intercept_resid = suppressWarnings(as.numeric(intercept_resid))
    ) |>
    dplyr::filter(
      is.finite(slope_resid),
      is.finite(intercept_resid)
    )
  if (nrow(coefficient_calibration) == 0) {
    return(tibble::tibble())
  }

  policy_value <- as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_)
  branch_value <- selected_branches(row_now)[[1]]
  if (is.na(policy_value) || !nzchar(policy_value) ||
      is.na(branch_value) || !nzchar(branch_value)) {
    return(tibble::tibble())
  }

  pool_now <- coefficient_calibration |>
    dplyr::filter(
      policy == !!policy_value,
      equation_branch_filter == !!branch_value
    )
  if (nrow(pool_now) < as.integer(min_rows)) {
    return(tibble::tibble())
  }
  pool_now
}

selected_coefficient_covariance <- function(coefficient_calibration,
                                            row_now,
                                            min_rows = 2L) {
  pool <- select_coefficient_residual_pool(
    coefficient_calibration = coefficient_calibration,
    row_now = row_now,
    min_rows = min_rows
  )
  if (nrow(pool) < 2L) {
    return(NULL)
  }

  sigma_now <- tryCatch(
    stats::cov(
      cbind(
        suppressWarnings(as.numeric(pool$slope_resid)),
        suppressWarnings(as.numeric(pool$intercept_resid))
      ),
      use = "complete.obs"
    ),
    error = function(e) NULL
  )
  if (is.null(sigma_now) || !is.matrix(sigma_now) || any(!is.finite(sigma_now))) {
    return(NULL)
  }

  sigma_now <- (sigma_now + t(sigma_now)) / 2
  diag(sigma_now) <- pmax(diag(sigma_now), 0)
  list(
    covariance = sigma_now,
    n = nrow(pool)
  )
}

selected_coefficient_conformal_summary <- function(coefficient_calibration,
                                                   row_now,
                                                   level = 0.95,
                                                   min_rows = 2L) {
  pool <- select_coefficient_residual_pool(
    coefficient_calibration = coefficient_calibration,
    row_now = row_now,
    min_rows = min_rows
  )
  if (nrow(pool) < max(2L, as.integer(min_rows))) {
    return(NULL)
  }

  probs <- suppressWarnings(as.numeric(level))
  if (!is.finite(probs) || probs <= 0 || probs >= 1) {
    probs <- 0.95
  }

  slope_resid <- suppressWarnings(as.numeric(pool$slope_resid))
  intercept_resid <- suppressWarnings(as.numeric(pool$intercept_resid))
  keep <- is.finite(slope_resid) & is.finite(intercept_resid)
  if (sum(keep) < max(2L, as.integer(min_rows))) {
    return(NULL)
  }

  slope_resid <- slope_resid[keep]
  intercept_resid <- intercept_resid[keep]
  sigma_now <- tryCatch(
    stats::cov(cbind(slope_resid, intercept_resid), use = "complete.obs"),
    error = function(e) NULL
  )
  if (!is.null(sigma_now) && is.matrix(sigma_now) && all(is.finite(sigma_now))) {
    sigma_now <- (sigma_now + t(sigma_now)) / 2
    diag(sigma_now) <- pmax(diag(sigma_now), 0)
  } else {
    sigma_now <- matrix(c(NA_real_, NA_real_, NA_real_, NA_real_), 2, 2)
  }

  list(
    slope_radius = stats::quantile(abs(slope_resid), probs = probs, na.rm = TRUE, names = FALSE, type = 8),
    intercept_radius = stats::quantile(abs(intercept_resid), probs = probs, na.rm = TRUE, names = FALSE, type = 8),
    covariance = sigma_now,
    n = sum(keep)
  )
}

fit_centered_coefficient_width_model <- function(coefficient_calibration,
                                                 row_now,
                                                 response_col,
                                                 tau = 0.95) {
  # Fit one centered residual-width model for slope or intercept uncertainty.
  # The interval remains centered on the selected point estimate; only the
  # half-width varies with the realized donor geometry for that row.
  pool_now <- select_coefficient_residual_pool(
    coefficient_calibration = coefficient_calibration,
    row_now = row_now,
    min_rows = 2L
  )
  if (nrow(pool_now) < 2L || !response_col %in% names(pool_now)) {
    return(NULL)
  }

  training_tbl <- prepare_residual_scale_training(
    calibration_rows = pool_now,
    response_col = response_col
  )
  if (nrow(training_tbl) < 2L) {
    return(NULL)
  }
  weight_now <- locality_similarity_weights(
    training_tbl = training_tbl,
    row_now = row_now,
    include_u = FALSE
  )
  width_now <- weighted_quantile_or_na(
    x = training_tbl$abs_residual,
    w = weight_now,
    prob = suppressWarnings(as.numeric(tau)[[1]])
  )

  list(
    width = suppressWarnings(as.numeric(width_now[[1]] %||% NA_real_)),
    support_n = dplyr::n_distinct(training_tbl$anchor_model_id),
    row_n = nrow(training_tbl),
    pool = pool_now |>
      dplyr::mutate(locality_weight = weight_now)
  )
}

coefficient_interval_critical_value <- function(support_n,
                                                level = 0.95) {
  support_n <- suppressWarnings(as.numeric(support_n))
  alpha <- 1 - suppressWarnings(as.numeric(level))
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    alpha <- 0.05
  }
  tail_prob <- 1 - alpha / 2
  if (is.finite(support_n) && support_n >= 2) {
    return(stats::qt(tail_prob, df = max(support_n - 1, 1)))
  }
  stats::qnorm(tail_prob)
}

selection_competition_pool <- function(policy_tbl,
                                       row_now,
                                       config = NULL) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  row_now <- tibble::as_tibble(row_now)
  if (nrow(policy_tbl) == 0 || nrow(row_now) == 0) {
    return(tibble::tibble())
  }

  anchor_id_chr <- as.character(row_now$anchor_model_id[[1]] %||% NA_character_)
  if (is.na(anchor_id_chr) || !nzchar(anchor_id_chr)) {
    return(tibble::tibble())
  }

  validation_error <- dplyr::coalesce(
    if ("anchor_selection_validation_error" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$anchor_selection_validation_error)) else rep(NA_real_, nrow(policy_tbl)),
    if ("species_block_median_abs_log_error" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$species_block_median_abs_log_error)) else rep(NA_real_, nrow(policy_tbl)),
    if ("mean_species_median_abs_log" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$mean_species_median_abs_log)) else rep(NA_real_, nrow(policy_tbl)),
    if ("median_abs_log" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$median_abs_log)) else rep(NA_real_, nrow(policy_tbl)),
    if (".meta_predicted_score" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$.meta_predicted_score)) else rep(NA_real_, nrow(policy_tbl))
  )
  uncertainty_width <- dplyr::coalesce(
    if ("uncertainty_cost_log_width" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$uncertainty_cost_log_width)) else rep(NA_real_, nrow(policy_tbl)),
    if ("interval_log_width" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$interval_log_width)) else rep(NA_real_, nrow(policy_tbl))
  )
  selection_valid <- dplyr::coalesce(
    if ("selection_valid" %in% names(policy_tbl)) as.logical(policy_tbl$selection_valid) else rep(NA, nrow(policy_tbl)),
    if ("n_valid_models" %in% names(policy_tbl)) {
      n_valid <- suppressWarnings(as.numeric(policy_tbl$n_valid_models))
      is.finite(n_valid) & n_valid > 0
    } else {
      rep(NA, nrow(policy_tbl))
    },
    if ("n_models" %in% names(policy_tbl)) {
      n_models <- suppressWarnings(as.numeric(policy_tbl$n_models))
      is.finite(n_models) & n_models > 0
    } else {
      rep(NA, nrow(policy_tbl))
    },
    if ("valid_prediction" %in% names(policy_tbl)) as.logical(policy_tbl$valid_prediction) else rep(FALSE, nrow(policy_tbl)),
    FALSE
  )
  uncertainty_eligible <- dplyr::coalesce(
    if ("uncertainty_eligible" %in% names(policy_tbl)) as.logical(policy_tbl$uncertainty_eligible) else rep(NA, nrow(policy_tbl)),
    FALSE
  )

  pool <- policy_tbl |>
    dplyr::mutate(
      .anchor_model_id_chr = as.character(anchor_model_id),
      .validation_error = validation_error,
      .uncertainty_width = uncertainty_width,
      .selection_valid = selection_valid,
      .uncertainty_eligible = uncertainty_eligible
    ) |>
    dplyr::filter(
      .anchor_model_id_chr == anchor_id_chr,
      .selection_valid,
      is.finite(.validation_error),
      is.finite(.uncertainty_width)
    )
  if (nrow(pool) == 0) {
    return(pool)
  }

  eligible_pool <- pool |>
    dplyr::filter(dplyr::coalesce(.uncertainty_eligible, FALSE))
  if (nrow(eligible_pool) > 0) {
    pool <- eligible_pool
  }

  min_validation_error <- suppressWarnings(as.numeric(
    row_now$anchor_selection_min_validation_error[[1]] %||% NA_real_
  ))
  if (!is.finite(min_validation_error)) {
    min_validation_error <- suppressWarnings(min(pool$.validation_error, na.rm = TRUE))
  }
  if (!is.finite(min_validation_error)) {
    return(pool[0, , drop = FALSE])
  }

  validation_threshold <- suppressWarnings(as.numeric(
    row_now$anchor_selection_validation_threshold[[1]] %||% NA_real_
  ))
  if (!is.finite(validation_threshold)) {
    validation_threshold <- min_validation_error
  }

  # Match the current selector's accepted score band exactly. The selected-row
  # post-selection summaries should propagate the same near-tied competitor set
  # that the selector considered, rather than reintroducing separate width or
  # locality screens after the fact.
  score_pool <- pool |>
    dplyr::filter(.validation_error <= validation_threshold + 1e-12)
  if (nrow(score_pool) == 0) {
    score_pool <- pool |>
      dplyr::filter(.validation_error <= min_validation_error + 1e-12)
  }
  if (nrow(score_pool) == 0) {
    return(pool[0, , drop = FALSE])
  }
  if (!"bootstrap_median_rank" %in% names(score_pool)) {
    score_pool$bootstrap_median_rank <- NA_real_
  }

  selected_policy_value <- as.character(
    row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_
  )
  selected_branch_value <- as.character(
    row_now$selected_equation_branch_filter[[1]] %||%
      row_now$equation_branch_filter[[1]] %||%
      selected_branches(row_now)[[1]] %||%
      NA_character_
  )

  score_pool |>
    dplyr::filter(
      !(as.character(policy) == selected_policy_value &
          as.character(equation_branch_filter) == selected_branch_value)
    ) |>
    dplyr::arrange(
      bootstrap_median_rank,
      .validation_error,
      policy
    )
}

selection_competition_covariance <- function(policy_tbl,
                                             row_now,
                                             config = NULL) {
  pool <- selection_competition_pool(
    policy_tbl = policy_tbl,
    row_now = row_now,
    config = config
  )
  if (nrow(pool) < 2L) {
    return(NULL)
  }

  slope_vals <- suppressWarnings(as.numeric(pool$policy_slope_len))
  intercept_vals <- suppressWarnings(as.numeric(pool$policy_intercept_len))
  keep <- is.finite(slope_vals) & is.finite(intercept_vals)
  if (sum(keep) < 2L) {
    return(NULL)
  }

  sigma_now <- tryCatch(
    stats::cov(cbind(slope_vals[keep], intercept_vals[keep])),
    error = function(e) NULL
  )
  if (is.null(sigma_now) || !is.matrix(sigma_now) || any(!is.finite(sigma_now))) {
    return(NULL)
  }

  sigma_now <- (sigma_now + t(sigma_now)) / 2
  diag(sigma_now) <- pmax(diag(sigma_now), 0)
  list(
    covariance = sigma_now,
    n = sum(keep)
  )
}

#' Build anchor PDF directly from one anchor row
#'
#' @param anchor_row One-row anchor table.
#' @param n Number of support points.
#'
#' @return A tibble with `length_cm` and `f_len`.
#'
#' @keywords internal
anchor_pdf_from_row <- function(anchor_row,
                                n = 400L) {
  mins <- suppressWarnings(as.numeric(anchor_row$study_length_min))
  maxs <- suppressWarnings(as.numeric(anchor_row$study_length_max))
  mids <- suppressWarnings(as.numeric(anchor_row$study_length_midpoint))
  mins <- mins[is.finite(mins) & mins > 0]
  maxs <- maxs[is.finite(maxs) & maxs > 0]
  mids <- mids[is.finite(mids) & mids > 0]
  if (length(mins) > 0 && length(maxs) > 0) {
    lmin <- min(pmin(mins, maxs))
    lmax <- max(pmax(mins, maxs))
    if (is.finite(lmin) && is.finite(lmax) && lmax > lmin) {
      grid <- seq(lmin, lmax, length.out = as.integer(n))
      return(tibble::tibble(length_cm = grid, f_len = rep(1 / length(grid), length(grid))))
    }
    if (is.finite(lmin)) {
      return(tibble::tibble(length_cm = lmin, f_len = 1))
    }
  }
  if (length(mids) > 0) {
    return(tibble::tibble(length_cm = mids[[1]], f_len = 1))
  }
  tibble::tibble()
}

augment_anchor_length_context <- function(policy_tbl,
                                          candidate_models,
                                          length_grid_n = 400L) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  if (nrow(policy_tbl) == 0 || !"anchor_model_id" %in% names(policy_tbl)) {
    return(policy_tbl)
  }
  drop_cols <- intersect(
    c("expected_length_cm", "length_support_min_cm", "length_support_max_cm"),
    names(policy_tbl)
  )
  if (length(drop_cols) > 0) {
    policy_tbl <- dplyr::select(policy_tbl, -dplyr::all_of(drop_cols))
  }
  id_col <- if ("model_id_chr" %in% names(candidate_models)) "model_id_chr" else "model_id"
  policy_tbl$anchor_model_id <- as.character(policy_tbl$anchor_model_id)
  anchor_context <- purrr::map_dfr(unique(policy_tbl$anchor_model_id), function(anchor_id_chr) {
    anchor_row <- candidate_models |>
      dplyr::filter(as.character(.data[[id_col]]) == anchor_id_chr) |>
      dplyr::slice(1)
    if (nrow(anchor_row) == 0) {
      return(tibble::tibble(
        anchor_model_id = anchor_id_chr,
        expected_length_cm = NA_real_,
        length_support_min_cm = NA_real_,
        length_support_max_cm = NA_real_
      ))
    }
    anchor_pdf <- anchor_pdf_from_row(anchor_row, n = length_grid_n)
    if (nrow(anchor_pdf) == 0) {
      return(tibble::tibble(
        anchor_model_id = anchor_id_chr,
        expected_length_cm = NA_real_,
        length_support_min_cm = NA_real_,
        length_support_max_cm = NA_real_
      ))
    }
    tibble::tibble(
      anchor_model_id = anchor_id_chr,
      expected_length_cm = stats::weighted.mean(anchor_pdf$length_cm, anchor_pdf$f_len, na.rm = TRUE),
      length_support_min_cm = min(anchor_pdf$length_cm, na.rm = TRUE),
      length_support_max_cm = max(anchor_pdf$length_cm, na.rm = TRUE)
    )
  })
  policy_tbl |>
    dplyr::left_join(anchor_context, by = "anchor_model_id")
}

strategy_q_scalars <- function(row_now) {
  q95 <- suppressWarnings(as.numeric(
    row_now$meta_q_abs_log_total[[1]] %||%
      row_now$q_abs_log_conformal[[1]] %||%
      row_now$meta_q_abs_log[[1]] %||%
      row_now$q_abs_log[[1]] %||%
      row_now$q_abs_log_total[[1]] %||%
      NA_real_
  ))
  q99_raw <- suppressWarnings(as.numeric(
    row_now$meta_q_abs_log_simultaneous_total[[1]] %||% NA_real_
  ))
  z80 <- stats::qnorm(0.90)
  z90 <- stats::qnorm(0.95)
  z95 <- stats::qnorm(0.975)
  z99 <- stats::qnorm(0.995)
  sigma_base <- ifelse(is.finite(q95) & q95 > 0, q95 / z95, NA_real_)
  q80 <- ifelse(is.finite(sigma_base), sigma_base * z80, NA_real_)
  q90 <- ifelse(is.finite(sigma_base), sigma_base * z90, NA_real_)
  q99 <- dplyr::coalesce(
    ifelse(is.finite(q99_raw) & q99_raw > 0, pmax(q99_raw, q95), NA_real_),
    ifelse(is.finite(sigma_base), sigma_base * z99, NA_real_)
  )
  list(q80 = q80, q90 = q90, q95 = q95, q99 = q99)
}

log_multiplier_halfwidth_to_ts_db <- function(q_log) {
  q_log <- suppressWarnings(as.numeric(q_log))
  ifelse(is.finite(q_log), (10 / log(10)) * q_log, NA_real_)
}

lognormal_mean_centered_shifts <- function(q_log, z_value) {
  q_log <- suppressWarnings(as.numeric(q_log))
  z_value <- suppressWarnings(as.numeric(z_value))
  sigma_log <- ifelse(is.finite(q_log) & q_log > 0 & is.finite(z_value) & z_value > 0, q_log / z_value, NA_real_)
  lower_shift <- ifelse(is.finite(sigma_log), z_value * sigma_log + 0.5 * sigma_log^2, NA_real_)
  upper_shift <- ifelse(is.finite(sigma_log), z_value * sigma_log - 0.5 * sigma_log^2, NA_real_)
  list(lower = lower_shift, upper = pmax(upper_shift, 0))
}

coefficient_shape_modifier <- function(covariance,
                                       length_grid,
                                       pdf_weights) {
  covariance <- suppressWarnings(as.matrix(covariance))
  x <- suppressWarnings(as.numeric(log10(length_grid)))
  pdf_weights <- suppressWarnings(as.numeric(pdf_weights))
  keep <- is.finite(x)
  if (!is.matrix(covariance) || !identical(dim(covariance), c(2L, 2L)) || sum(keep) < 3L) {
    return(rep(1, length(length_grid)))
  }
  if (length(pdf_weights) != length(length_grid) || !any(is.finite(pdf_weights) & pdf_weights > 0)) {
    pdf_weights <- rep(1 / length(length_grid), length(length_grid))
  } else {
    pdf_weights <- pdf_weights / sum(pdf_weights, na.rm = TRUE)
  }
  X <- cbind(x[keep], 1)
  pred_var <- rowSums((X %*% covariance) * X)
  pred_sd <- sqrt(pmax(pred_var, 0))
  half_width <- stats::qnorm(0.975) * pred_sd
  avg_half_width <- stats::weighted.mean(half_width, pdf_weights[keep], na.rm = TRUE)
  out <- rep(1, length(length_grid))
  if (is.finite(avg_half_width) && avg_half_width > 0) {
    out[keep] <- half_width / avg_half_width
  }
  out[!is.finite(out)] <- 1
  out
}

rescale_coefficient_covariance <- function(covariance,
                                           length_grid,
                                           pdf_weights,
                                           q95_scalar) {
  covariance <- suppressWarnings(as.matrix(covariance))
  x <- suppressWarnings(as.numeric(log10(length_grid)))
  pdf_weights <- suppressWarnings(as.numeric(pdf_weights))
  q95_scalar <- suppressWarnings(as.numeric(q95_scalar))
  keep <- is.finite(x)
  if (!is.matrix(covariance) || !identical(dim(covariance), c(2L, 2L)) || sum(keep) < 3L) {
    return(list(
      covariance = matrix(NA_real_, nrow = 2, ncol = 2),
      shape_modifier = rep(NA_real_, length(length_grid))
    ))
  }
  if (length(pdf_weights) != length(length_grid) || !any(is.finite(pdf_weights) & pdf_weights > 0)) {
    pdf_weights <- rep(1 / length(length_grid), length(length_grid))
  } else {
    pdf_weights <- pdf_weights / sum(pdf_weights, na.rm = TRUE)
  }
  X <- cbind(x[keep], 1)
  pred_var <- rowSums((X %*% covariance) * X)
  pred_sd <- sqrt(pmax(pred_var, 0))
  z95 <- stats::qnorm(0.975)
  half_width <- z95 * pred_sd
  avg_half_width <- stats::weighted.mean(half_width, pdf_weights[keep], na.rm = TRUE)
  scale_factor <- if (is.finite(q95_scalar) && q95_scalar > 0 && is.finite(avg_half_width) && avg_half_width > 0) {
    q95_scalar / avg_half_width
  } else {
    1
  }
  shape_modifier <- rep(NA_real_, length(length_grid))
  shape_modifier[keep] <- if (is.finite(avg_half_width) && avg_half_width > 0) {
    half_width / avg_half_width
  } else {
    rep(1, sum(keep))
  }
  list(
    covariance = (scale_factor^2) * covariance,
    shape_modifier = shape_modifier
  )
}

coefficient_covariance_is_sane <- function(covariance,
                                           slope_hat,
                                           intercept_hat,
                                           slope_half_width_max = 10,
                                           intercept_hi_max = -50) {
  covariance <- suppressWarnings(as.matrix(covariance))
  slope_hat <- suppressWarnings(as.numeric(slope_hat))
  intercept_hat <- suppressWarnings(as.numeric(intercept_hat))
  if (!is.matrix(covariance) || !identical(dim(covariance), c(2L, 2L)) || any(!is.finite(covariance))) {
    return(FALSE)
  }
  slope_se <- sqrt(max(covariance[1, 1], 0))
  intercept_se <- sqrt(max(covariance[2, 2], 0))
  z95 <- stats::qnorm(0.975)
  slope_lo <- slope_hat - z95 * slope_se
  slope_hi <- slope_hat + z95 * slope_se
  intercept_hi <- intercept_hat + z95 * intercept_se
  slope_half_width <- (slope_hi - slope_lo) / 2

  is.finite(slope_lo) &&
    is.finite(slope_hi) &&
    is.finite(intercept_hi) &&
    slope_lo > 0 &&
    slope_half_width <= slope_half_width_max &&
    intercept_hi <= intercept_hi_max
}

calibrated_coefficient_covariance <- function(length_grid,
                                              pdf_weights,
                                              q95_scalar) {
  x <- suppressWarnings(as.numeric(log10(length_grid)))
  pdf_weights <- suppressWarnings(as.numeric(pdf_weights))
  q95_scalar <- suppressWarnings(as.numeric(q95_scalar))
  keep <- is.finite(x)
  if (sum(keep) < 3) {
    return(list(
      covariance = matrix(NA_real_, nrow = 2, ncol = 2),
      shape_modifier = rep(NA_real_, length(length_grid))
    ))
  }
  if (length(pdf_weights) != length(length_grid) || !any(is.finite(pdf_weights) & pdf_weights > 0)) {
    pdf_weights <- rep(1 / length(length_grid), length(length_grid))
  } else {
    pdf_weights <- pdf_weights / sum(pdf_weights, na.rm = TRUE)
  }
  X <- cbind(x[keep], 1)
  w <- pdf_weights[keep]
  info <- crossprod(X * sqrt(w), X * sqrt(w))
  Sigma_shape <- tryCatch(
    solve(info + diag(1e-8, nrow(info))),
    error = function(e) {
      eig <- eigen(info + diag(1e-8, nrow(info)), symmetric = TRUE)
      vals <- eig$values
      inv_vals <- ifelse(vals > 1e-10, 1 / vals, 0)
      eig$vectors %*% diag(inv_vals, nrow = length(inv_vals)) %*% t(eig$vectors)
    }
  )
  pred_var_shape <- rowSums((X %*% Sigma_shape) * X)
  pred_sd_shape <- sqrt(pmax(pred_var_shape, 0))
  z95 <- stats::qnorm(0.975)
  shape_half_width <- z95 * pred_sd_shape
  avg_half_width <- stats::weighted.mean(shape_half_width, w, na.rm = TRUE)
  scale_factor <- if (is.finite(q95_scalar) && q95_scalar > 0 && is.finite(avg_half_width) && avg_half_width > 0) {
    q95_scalar / avg_half_width
  } else {
    0
  }
  Sigma <- (scale_factor^2) * Sigma_shape
  shape_modifier <- rep(NA_real_, length(length_grid))
  shape_modifier[keep] <- if (is.finite(avg_half_width) && avg_half_width > 0) {
    shape_half_width / avg_half_width
  } else {
    rep(1, sum(keep))
  }
  list(covariance = Sigma, shape_modifier = shape_modifier)
}

strategy_uncertainty_context <- function(row_now,
                                         candidate_models,
                                         anchor_scores,
                                         policy_tbl = NULL,
                                         ts_calibration = NULL,
                                         coefficient_calibration = NULL,
                                         config = NULL,
                                         model_scores = NULL,
                                         species_lookup = NULL,
                                         policy_lookup,
                                         policy_path = NULL,
                                         include_competition = TRUE,
                                         lock_fixed_slope = FALSE,
                                         length_grid_n = 400L,
                                         ts_band_method = c("current", "smooth_scale_shape")) {
  ts_band_method <- match.arg(ts_band_method)
  id_col <- if ("model_id_chr" %in% names(candidate_models)) "model_id_chr" else "model_id"
  anchor_id_chr <- as.character(row_now$anchor_model_id[[1]])
  anchor_row <- candidate_models |>
    dplyr::filter(as.character(.data[[id_col]]) == anchor_id_chr) |>
    dplyr::slice(1)
  if (nrow(anchor_row) == 0) {
    return(NULL)
  }
  anchor_pdf <- anchor_pdf_from_row(anchor_row, n = length_grid_n)
  if (nrow(anchor_pdf) == 0) {
    return(NULL)
  }
  length_grid <- suppressWarnings(as.numeric(anchor_pdf$length_cm))
  pdf_weights <- suppressWarnings(as.numeric(anchor_pdf$f_len))
  anchor_slope <- suppressWarnings(as.numeric(anchor_row$slope_len[[1]]))
  anchor_intercept <- suppressWarnings(as.numeric(anchor_row$intercept_len[[1]]))
  anchor_species_name <- if ("species_name" %in% names(anchor_row)) {
    as.character(anchor_row$species_name[[1]])
  } else {
    NA_character_
  }
  anchor_genus <- if ("genus" %in% names(anchor_row)) {
    as.character(anchor_row$genus[[1]] %||% stringr::word(anchor_species_name, 1))
  } else {
    stringr::word(anchor_species_name, 1)
  }
  anchor_family <- if ("family_name" %in% names(anchor_row)) {
    as.character(anchor_row$family_name[[1]] %||% if ("family" %in% names(anchor_row)) anchor_row$family[[1]] else NA_character_)
  } else if ("family" %in% names(anchor_row)) {
    as.character(anchor_row$family[[1]])
  } else {
    NA_character_
  }
  anchor_frequency_khz <- suppressWarnings(as.numeric(
    anchor_row$frequency_khz[[1]] %||% anchor_row$frequency[[1]] %||% NA_real_
  ))
  anchor_depth_min <- suppressWarnings(as.numeric(
    anchor_row$depth_min[[1]] %||% anchor_row$study_depth_min[[1]] %||%
      anchor_row$depth_midpoint[[1]] %||% anchor_row$study_depth_midpoint[[1]] %||%
      NA_real_
  ))
  anchor_depth_max <- suppressWarnings(as.numeric(
    anchor_row$depth_max[[1]] %||% anchor_row$study_depth_max[[1]] %||%
      anchor_row$depth_midpoint[[1]] %||% anchor_row$study_depth_midpoint[[1]] %||%
      NA_real_
  ))
  ts_anchor <- anchor_slope * log10(length_grid) + anchor_intercept
  policy_name <- as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]])
  branch_value <- selected_branches(row_now)[[1]]
  is_fixed_branch <- identical(branch_value, "fixed20_only")
  q_scalars <- strategy_q_scalars(row_now)
  q_scalars_ts <- lapply(q_scalars, log_multiplier_halfwidth_to_ts_db)
  ts_min_anchor_neighbors <- suppressWarnings(as.integer(
    policy_selector_config_value(
      config,
      "min_bin_scores",
      sections = c("selection", "post_selection_conformal", "metalearner")
    ) %||% 10L
  ))
  if (!is.finite(ts_min_anchor_neighbors) || ts_min_anchor_neighbors < 1L) {
    ts_min_anchor_neighbors <- 10L
  }
  ts_curve_calibration <- select_ts_calibration_curve(
    ts_calibration = ts_calibration,
    row_now = row_now,
    target_u = seq(0, 1, length.out = length(length_grid)),
    min_anchor_neighbors = ts_min_anchor_neighbors
  )
  competition_sigma <- if (isTRUE(include_competition)) {
    selection_competition_covariance(
      policy_tbl = policy_tbl %||% tibble::tibble(),
      row_now = row_now,
      config = config
    )
  } else {
    NULL
  }
  selected_sigma <- selected_coefficient_covariance(
    coefficient_calibration = coefficient_calibration,
    row_now = row_now
  )
  slope_hat <- suppressWarnings(as.numeric(row_now$policy_slope_len[[1]]))
  intercept_hat <- suppressWarnings(as.numeric(row_now$policy_intercept_len[[1]]))
  selected_sigma_ok <- !is.null(selected_sigma) &&
    coefficient_covariance_is_sane(
      covariance = selected_sigma$covariance,
      slope_hat = slope_hat,
      intercept_hat = intercept_hat
    )
  raw_covariance <- NULL
  coefficient_support_n <- NA_real_
  if (selected_sigma_ok) {
    raw_covariance <- selected_sigma$covariance
    coefficient_support_n <- selected_sigma$n %||% NA_real_
  } else {
    donor_pool <- if (
      !is.null(anchor_scores) &&
        is.data.frame(anchor_scores) &&
        nrow(anchor_scores) > 0 &&
        all(c("anchor_model_id", "slope_len", "intercept_len") %in% names(anchor_scores))
    ) {
      anc_id_col_d <- intersect(c("model_id_chr", "model_id"), names(anchor_scores))
      df_d <- dplyr::filter(
        tibble::as_tibble(anchor_scores),
        as.character(anchor_model_id) == anchor_id_chr
      )
      if (length(anc_id_col_d) > 0) {
        df_d <- dplyr::filter(df_d, as.character(.data[[anc_id_col_d[[1]]]]) != anchor_id_chr)
      }
      dplyr::filter(df_d, is.finite(slope_len), is.finite(intercept_len))
    } else {
      tibble::tibble()
    }
    n_pool <- nrow(donor_pool)
    if (n_pool >= 2) {
      d_s <- suppressWarnings(as.numeric(donor_pool$slope_len))
      d_b <- suppressWarnings(as.numeric(donor_pool$intercept_len))
      wts_raw <- if ("w_adm" %in% names(donor_pool)) {
        suppressWarnings(as.numeric(donor_pool$w_adm))
      } else {
        rep(1, n_pool)
      }
      wts_raw[!is.finite(wts_raw) | wts_raw <= 0] <- 1
      wts <- wts_raw / sum(wts_raw)
      m_s <- sum(wts * d_s)
      m_b <- sum(wts * d_b)
      var_s <- sum(wts * (d_s - m_s)^2)
      var_b <- sum(wts * (d_b - m_b)^2)
      cov_sb <- sum(wts * (d_s - m_s) * (d_b - m_b))
      raw_covariance <- matrix(c(var_s, cov_sb, cov_sb, var_b), 2, 2)
      coefficient_support_n <- n_pool
    }
  }

  competition_support_n <- competition_sigma$n %||% NA_real_
  if (!is.null(competition_sigma) &&
      is.matrix(competition_sigma$covariance) &&
      all(is.finite(competition_sigma$covariance))) {
    raw_covariance <- if (is.null(raw_covariance)) {
      competition_sigma$covariance
    } else {
      raw_covariance + competition_sigma$covariance
    }
  }

  support_candidates <- c(coefficient_support_n, competition_support_n)
  support_candidates <- support_candidates[is.finite(support_candidates)]
  coefficient_support_n <- if (length(support_candidates) > 0) {
    min(support_candidates)
  } else {
    NA_real_
  }

  if (isTRUE(lock_fixed_slope) && isTRUE(is_fixed_branch)) {
    if (is.null(raw_covariance) || !all(is.finite(raw_covariance))) {
      raw_covariance <- matrix(c(0, 0, 0, 1), nrow = 2, ncol = 2)
    } else {
      raw_covariance <- suppressWarnings(as.matrix(raw_covariance))
      raw_covariance[1, ] <- c(0, 0)
      raw_covariance[, 1] <- c(0, 0)
      raw_covariance[2, 2] <- pmax(raw_covariance[2, 2], 1e-8)
    }
  }

  if (!is.null(raw_covariance) && all(is.finite(raw_covariance))) {
    sigma_result <- rescale_coefficient_covariance(
      covariance = raw_covariance,
      length_grid = length_grid,
      pdf_weights = pdf_weights,
      q95_scalar = q_scalars_ts$q95
    )
    Sigma <- sigma_result$covariance
    shape_modifier <- sigma_result$shape_modifier
  } else {
    covariance_result <- calibrated_coefficient_covariance(
      length_grid = length_grid,
      pdf_weights = pdf_weights,
      q95_scalar = q_scalars_ts$q95
    )
    shape_modifier <- covariance_result$shape_modifier
    Sigma <- covariance_result$covariance
  }

  if (length(shape_modifier) != length(length_grid) || !all(is.finite(shape_modifier))) {
    shape_modifier <- rep(1, length(length_grid))
  }

  # For the displayed TS(L) panel, keep the selected policy as the center and
  # combine:
  # 1) a selected-policy residual half-width that varies with length, and
  # 2) an asymmetric offset from the accepted tie-policy curves relative to the
  #    selected line.
  target_u <- seq(0, 1, length.out = length(length_grid))
  resolve_ts_curve <- function(candidate_names, fallback = 0) {
    if (nrow(ts_curve_calibration) <= 0) {
      return(rep(fallback, length(length_grid)))
    }
    curve_source <- resolve_numeric_candidates(ts_curve_calibration, candidate_names)
    keep_curve <- is.finite(ts_curve_calibration$u) & is.finite(curve_source)
    if (sum(keep_curve) == 1L) {
      return(rep(curve_source[keep_curve][[1]], length(length_grid)))
    }
    if (sum(keep_curve) >= 2L) {
      return(stats::approx(
        x = ts_curve_calibration$u[keep_curve],
        y = curve_source[keep_curve],
        xout = target_u,
        rule = 2,
        ties = "ordered"
      )$y)
    }
    rep(fallback, length(length_grid))
  }
  # Build the selected-policy width layer. The default path preserves the
  # existing tail-shaped construction. The alternate path uses a single smooth
  # residual-scale shape and only lets the signed residual balance determine
  # the lower-vs-upper split.
  build_signed_shape <- function(lower_names, upper_names) {
    lower_raw <- abs(resolve_ts_curve(lower_names, fallback = NA_real_))
    upper_raw <- resolve_ts_curve(upper_names, fallback = NA_real_)
    lower_raw[!is.finite(lower_raw) | lower_raw <= 0] <- NA_real_
    upper_raw[!is.finite(upper_raw) | upper_raw <= 0] <- NA_real_
    if (!any(is.finite(lower_raw))) {
      lower_raw <- rep(1, length(length_grid))
    }
    if (!any(is.finite(upper_raw))) {
      upper_raw <- rep(1, length(length_grid))
    }
    lower_mean <- stats::weighted.mean(lower_raw, pdf_weights, na.rm = TRUE)
    upper_mean <- stats::weighted.mean(upper_raw, pdf_weights, na.rm = TRUE)
    if (!is.finite(lower_mean) || lower_mean <= 0) {
      lower_mean <- mean(lower_raw[is.finite(lower_raw)], na.rm = TRUE)
    }
    if (!is.finite(upper_mean) || upper_mean <= 0) {
      upper_mean <- mean(upper_raw[is.finite(upper_raw)], na.rm = TRUE)
    }
    if (!is.finite(lower_mean) || lower_mean <= 0) {
      lower_mean <- 1
    }
    if (!is.finite(upper_mean) || upper_mean <= 0) {
      upper_mean <- 1
    }
    total_mean <- lower_mean + upper_mean
    if (!is.finite(total_mean) || total_mean <= 0) {
      total_mean <- 2
    }
    list(
      lower_shape = lower_raw / lower_mean,
      upper_shape = upper_raw / upper_mean,
      lower_share = lower_mean / total_mean,
      upper_share = upper_mean / total_mean
    )
  }

  build_smooth_scale_shape <- function(lower_names, upper_names) {
    base_raw <- resolve_ts_curve(
      c("log_sigma_scale_fit", "q80_log_sigma_abs_dev", "q90_log_sigma_abs_dev"),
      fallback = NA_real_
    )
    base_raw[!is.finite(base_raw) | base_raw <= 0] <- NA_real_
    if (!any(is.finite(base_raw))) {
      base_raw <- rep(1, length(length_grid))
    }
    base_mean <- stats::weighted.mean(base_raw, pdf_weights, na.rm = TRUE)
    if (!is.finite(base_mean) || base_mean <= 0) {
      base_mean <- mean(base_raw[is.finite(base_raw)], na.rm = TRUE)
    }
    if (!is.finite(base_mean) || base_mean <= 0) {
      base_mean <- 1
    }

    lower_raw <- abs(resolve_ts_curve(lower_names, fallback = NA_real_))
    upper_raw <- resolve_ts_curve(upper_names, fallback = NA_real_)
    lower_raw[!is.finite(lower_raw) | lower_raw < 0] <- NA_real_
    upper_raw[!is.finite(upper_raw) | upper_raw < 0] <- NA_real_
    lower_mean <- stats::weighted.mean(lower_raw, pdf_weights, na.rm = TRUE)
    upper_mean <- stats::weighted.mean(upper_raw, pdf_weights, na.rm = TRUE)
    if (!is.finite(lower_mean) || lower_mean < 0) {
      lower_mean <- mean(lower_raw[is.finite(lower_raw)], na.rm = TRUE)
    }
    if (!is.finite(upper_mean) || upper_mean < 0) {
      upper_mean <- mean(upper_raw[is.finite(upper_raw)], na.rm = TRUE)
    }
    if (!is.finite(lower_mean) || lower_mean < 0) {
      lower_mean <- 0
    }
    if (!is.finite(upper_mean) || upper_mean < 0) {
      upper_mean <- 0
    }
    total_mean <- lower_mean + upper_mean
    if (!is.finite(total_mean) || total_mean <= 0) {
      lower_mean <- upper_mean <- 0.5
      total_mean <- 1
    }

    list(
      lower_shape = base_raw / base_mean,
      upper_shape = base_raw / base_mean,
      lower_share = lower_mean / total_mean,
      upper_share = upper_mean / total_mean
    )
  }

  shape_builder <- if (identical(ts_band_method, "smooth_scale_shape")) {
    build_smooth_scale_shape
  } else {
    build_signed_shape
  }

  shape80 <- shape_builder(
    c("q10_log_sigma_residual_smooth", "q10_log_sigma_residual"),
    c("q90_log_sigma_residual_smooth", "q90_log_sigma_residual")
  )
  shape90 <- shape_builder(
    c("q05_log_sigma_residual_smooth", "q05_log_sigma_residual"),
    c("q95_log_sigma_residual_smooth", "q95_log_sigma_residual")
  )
  shape95 <- shape_builder(
    c("q025_log_sigma_residual_smooth", "q025_log_sigma_residual"),
    c("q975_log_sigma_residual_smooth", "q975_log_sigma_residual")
  )
  shape99 <- shape_builder(
    c("q005_log_sigma_residual_smooth", "q005_log_sigma_residual"),
    c("q995_log_sigma_residual_smooth", "q995_log_sigma_residual")
  )

  lower80_log_sigma <- (2 * q_scalars$q80 * shape80$lower_share) * shape80$lower_shape
  upper80_log_sigma <- (2 * q_scalars$q80 * shape80$upper_share) * shape80$upper_shape
  lower90_log_sigma <- (2 * q_scalars$q90 * shape90$lower_share) * shape90$lower_shape
  upper90_log_sigma <- (2 * q_scalars$q90 * shape90$upper_share) * shape90$upper_shape
  lower95_log_sigma <- (2 * q_scalars$q95 * shape95$lower_share) * shape95$lower_shape
  upper95_log_sigma <- (2 * q_scalars$q95 * shape95$upper_share) * shape95$upper_shape
  lower99_log_sigma <- (2 * q_scalars$q99 * shape99$lower_share) * shape99$lower_shape
  upper99_log_sigma <- (2 * q_scalars$q99 * shape99$upper_share) * shape99$upper_shape

  q80_L_lower <- (10 / log(10)) * lower80_log_sigma
  q80_L_upper <- (10 / log(10)) * upper80_log_sigma
  q90_L_lower <- (10 / log(10)) * lower90_log_sigma
  q90_L_upper <- (10 / log(10)) * upper90_log_sigma
  q95_L_lower <- (10 / log(10)) * lower95_log_sigma
  q95_L_upper <- (10 / log(10)) * upper95_log_sigma
  q99_L_lower <- (10 / log(10)) * lower99_log_sigma
  q99_L_upper <- (10 / log(10)) * upper99_log_sigma

  q80_L <- 0.5 * (q80_L_lower + q80_L_upper)
  q90_L <- 0.5 * (q90_L_lower + q90_L_upper)
  q95_L <- 0.5 * (q95_L_lower + q95_L_upper)
  q99_L <- 0.5 * (q99_L_lower + q99_L_upper)
  ts_pred_raw <- slope_hat * log10(length_grid) + intercept_hat
  ts_center <- ts_pred_raw
  competitor_pool <- if (isTRUE(include_competition)) {
    selection_competition_pool(
      policy_tbl = policy_tbl %||% tibble::tibble(),
      row_now = row_now,
      config = config
    )
  } else {
    tibble::tibble()
  }
  comp_lo80 <- comp_hi80 <- rep(0, length(length_grid))
  comp_lo90 <- comp_hi90 <- rep(0, length(length_grid))
  comp_lo95 <- comp_hi95 <- rep(0, length(length_grid))
  comp_lo99 <- comp_hi99 <- rep(0, length(length_grid))
  if (nrow(competitor_pool) > 0 &&
      all(c("policy_slope_len", "policy_intercept_len") %in% names(competitor_pool))) {
    comp_slope <- suppressWarnings(as.numeric(competitor_pool$policy_slope_len))
    comp_intercept <- suppressWarnings(as.numeric(competitor_pool$policy_intercept_len))
    comp_score <- suppressWarnings(as.numeric(
      dplyr::coalesce(
        competitor_pool$.validation_error %||% NULL,
        competitor_pool$anchor_selection_validation_error %||% NULL
      )
    ))
    keep_comp <- is.finite(comp_slope) & is.finite(comp_intercept)
    if (sum(keep_comp) > 0) {
      comp_weights <- rep(1, sum(keep_comp))
      score_keep <- comp_score[keep_comp]
      if (length(score_keep) == length(comp_weights) && any(is.finite(score_keep))) {
        score_min <- min(score_keep, na.rm = TRUE)
        score_thr <- suppressWarnings(as.numeric(
          row_now$anchor_selection_validation_threshold[[1]] %||% NA_real_
        ))
        if (!is.finite(score_thr)) {
          score_thr <- max(score_keep, na.rm = TRUE)
        }
        score_scale <- score_thr - score_min
        if (!is.finite(score_scale) || score_scale <= 0) {
          score_scale <- stats::sd(score_keep, na.rm = TRUE)
        }
        if (!is.finite(score_scale) || score_scale <= 0) {
          score_scale <- 1
        }
        comp_weights <- exp(-(score_keep - score_min) / score_scale)
        comp_weights[!is.finite(comp_weights) | comp_weights <= 0] <- 1
      }
      competitor_ts_curves <- vapply(
        seq_len(sum(keep_comp)),
        function(j) comp_slope[keep_comp][[j]] * log10(length_grid) + comp_intercept[keep_comp][[j]],
        numeric(length(length_grid))
      )
      if (is.null(dim(competitor_ts_curves))) {
        competitor_ts_curves <- matrix(competitor_ts_curves, nrow = length(length_grid), ncol = 1)
      }
      deviation_matrix <- sweep(competitor_ts_curves, 1, ts_center, "-")
      # The selected policy line should remain the weighted 50th-quantile
      # reference of the policy-competition layer. Recenter using the weighted
      # median under the same score-proximity weights that define the outer
      # competition spread.
      comp_median <- apply(
        deviation_matrix,
        1,
        function(x) weighted_quantile_or_na(x, comp_weights, 0.5)
      )
      comp_median[!is.finite(comp_median)] <- 0
      centered_dev_matrix <- sweep(deviation_matrix, 1, comp_median, "-")
      build_signed_quantile <- function(prob, dev_matrix) {
        apply(dev_matrix, 1, function(x) weighted_quantile_or_na(x, comp_weights, prob))
      }
      comp_lo80 <- build_signed_quantile(0.10, centered_dev_matrix)
      comp_hi80 <- build_signed_quantile(0.90, centered_dev_matrix)
      comp_lo90 <- build_signed_quantile(0.05, centered_dev_matrix)
      comp_hi90 <- build_signed_quantile(0.95, centered_dev_matrix)
      comp_lo95 <- build_signed_quantile(0.025, centered_dev_matrix)
      comp_hi95 <- build_signed_quantile(0.975, centered_dev_matrix)
      comp_lo99 <- build_signed_quantile(0.005, centered_dev_matrix)
      comp_hi99 <- build_signed_quantile(0.995, centered_dev_matrix)
      comp_lo80[!is.finite(comp_lo80)] <- 0
      comp_hi80[!is.finite(comp_hi80)] <- 0
      comp_lo90[!is.finite(comp_lo90)] <- 0
      comp_hi90[!is.finite(comp_hi90)] <- 0
      comp_lo95[!is.finite(comp_lo95)] <- 0
      comp_hi95[!is.finite(comp_hi95)] <- 0
      comp_lo99[!is.finite(comp_lo99)] <- 0
      comp_hi99[!is.finite(comp_hi99)] <- 0
    }
  }
  ts_lo_80 <- ts_center + comp_lo80 - q80_L_lower
  ts_hi_80 <- ts_center + comp_hi80 + q80_L_upper
  ts_lo_90 <- ts_center + comp_lo90 - q90_L_lower
  ts_hi_90 <- ts_center + comp_hi90 + q90_L_upper
  ts_lo_95 <- ts_center + comp_lo95 - q95_L_lower
  ts_hi_95 <- ts_center + comp_hi95 + q95_L_upper
  ts_lo_99 <- ts_center + comp_lo99 - q99_L_lower
  ts_hi_99 <- ts_center + comp_hi99 + q99_L_upper
  top_candidate_fields <- c(
    "anchor_model_id",
    "slope_len",
    "intercept_len",
    "combined_distance",
    "w_adm"
  )
  top_row <- if (all(top_candidate_fields %in% names(anchor_scores))) {
    anchor_scores |>
      dplyr::filter(as.character(anchor_model_id) == anchor_id_chr) |>
      (\(df) {
        donor_id_col <- if ("model_id_chr" %in% names(df)) {
          "model_id_chr"
        } else if ("model_id" %in% names(df)) {
          "model_id"
        } else {
          NA_character_
        }
        if (is.na(donor_id_col)) {
          df
        } else {
          dplyr::filter(df, as.character(.data[[donor_id_col]]) != anchor_id_chr)
        }
      })() |>
      dplyr::mutate(
        combined_distance = suppressWarnings(as.numeric(combined_distance)),
        w_adm = suppressWarnings(as.numeric(w_adm))
      ) |>
      dplyr::filter(is.finite(slope_len), is.finite(intercept_len)) |>
      dplyr::arrange(combined_distance, dplyr::desc(w_adm)) |>
      dplyr::slice(1)
  } else {
    tibble::tibble()
  }
  ts_top_candidate <- if (nrow(top_row) == 1) {
    as.numeric(top_row$slope_len[[1]]) * log10(length_grid) + as.numeric(top_row$intercept_len[[1]])
  } else {
    rep(NA_real_, length(length_grid))
  }
  list(
    anchor_model_id = anchor_id_chr,
    anchor_species = as.character(row_now$anchor_species[[1]]),
    selected_policy = policy_name,
    length_cm = length_grid,
    u = seq(0, 1, length.out = length(length_grid)),
    ts_pred = ts_pred_raw,
    ts_pred_raw = ts_pred_raw,
    ts_center = ts_center,
    ts_anchor = ts_anchor,
    ts_top_candidate = ts_top_candidate,
    ts_lo_80 = ts_lo_80,
    ts_hi_80 = ts_hi_80,
    ts_lo_90 = ts_lo_90,
    ts_hi_90 = ts_hi_90,
    ts_lo_95 = ts_lo_95,
    ts_hi_95 = ts_hi_95,
    ts_lo_99 = ts_lo_99,
    ts_hi_99 = ts_hi_99,
    support_modifier = q95_L,
    q80_L = q80_L,
    q90_L = q90_L,
    q95_L = q95_L,
    q99_L = q99_L,
    pdf_weight = pdf_weights,
    anchor_genus = anchor_genus,
    anchor_family = anchor_family,
    anchor_frequency_khz = anchor_frequency_khz,
    anchor_length_min = min(length_grid, na.rm = TRUE),
    anchor_length_max = max(length_grid, na.rm = TRUE),
    anchor_depth_min = anchor_depth_min,
    anchor_depth_max = anchor_depth_max,
    ts_calibration_score_n = if ("ts_score_n" %in% names(ts_curve_calibration)) {
      suppressWarnings(as.numeric(stats::median(ts_curve_calibration$ts_score_n, na.rm = TRUE)))
    } else {
      NA_real_
    },
    ts_calibration_anchor_n = if ("ts_anchor_n" %in% names(ts_curve_calibration)) {
      suppressWarnings(as.numeric(stats::median(ts_curve_calibration$ts_anchor_n, na.rm = TRUE)))
    } else {
      NA_real_
    },
    coefficient_covariance = Sigma,
    coefficient_support_n = coefficient_support_n,
    policy_selection_competitor_n = competition_support_n
  )
}

coefficient_region_from_ts_envelope <- function(length_grid,
                                                ts_lo,
                                                ts_hi,
                                                fixed_slope = NA_real_,
                                                slope_floor = 0,
                                                intercept_ceiling = 0,
                                                tol = 1e-8) {
  x <- suppressWarnings(as.numeric(log10(length_grid)))
  lo <- suppressWarnings(as.numeric(ts_lo))
  hi <- suppressWarnings(as.numeric(ts_hi))
  keep <- is.finite(x) & is.finite(lo) & is.finite(hi)
  if (sum(keep) < 2L) {
    return(NULL)
  }
  x <- x[keep]
  lo <- lo[keep]
  hi <- hi[keep]
  if (any(hi < lo - tol)) {
    return(NULL)
  }

  lower_envelope <- function(m) {
    max(lo - m * x)
  }
  upper_envelope <- function(m) {
    min(c(hi - m * x, intercept_ceiling))
  }

  if (is.finite(fixed_slope)) {
    b_lo <- lower_envelope(fixed_slope)
    b_hi <- upper_envelope(fixed_slope)
    if (!is.finite(b_lo) || !is.finite(b_hi) || b_lo > b_hi + tol) {
      return(NULL)
    }
    return(list(
      slope_lo = fixed_slope,
      slope_hi = fixed_slope,
      intercept_lo = b_lo,
      intercept_hi = b_hi
    ))
  }

  slope_lo <- suppressWarnings(as.numeric(slope_floor))
  slope_hi <- Inf

  if (!is.finite(slope_lo)) {
    slope_lo <- 0
  }

  for (i in seq_along(x)) {
    if (x[[i]] <= tol) {
      if (lo[[i]] > intercept_ceiling + tol) {
        return(NULL)
      }
    } else {
      slope_lo <- max(slope_lo, (lo[[i]] - intercept_ceiling) / x[[i]])
    }
  }

  n_x <- length(x)
  for (i in seq_len(n_x)) {
    for (j in seq_len(n_x)) {
      dx <- x[[j]] - x[[i]]
      rhs <- hi[[j]] - lo[[i]]
      if (abs(dx) <= tol) {
        if (rhs < -tol) {
          return(NULL)
        }
      } else {
        bound <- rhs / dx
        if (dx > 0) {
          slope_hi <- min(slope_hi, bound)
        } else {
          slope_lo <- max(slope_lo, bound)
        }
      }
    }
  }

  if (!is.finite(slope_hi) || slope_lo > slope_hi + tol) {
    return(NULL)
  }

  candidate_m <- c(slope_lo, slope_hi, 0.5 * (slope_lo + slope_hi))
  if (n_x >= 2L) {
    for (i in seq_len(n_x - 1L)) {
      for (j in (i + 1L):n_x) {
        dx <- x[[j]] - x[[i]]
        if (abs(dx) > tol) {
          candidate_m <- c(
            candidate_m,
            (lo[[j]] - lo[[i]]) / dx,
            (hi[[j]] - hi[[i]]) / dx,
            hi[[i]] / x[[i]],
            hi[[j]] / x[[j]]
          )
        }
      }
    }
  }
  candidate_m <- unique(candidate_m[is.finite(candidate_m)])
  candidate_m <- candidate_m[candidate_m >= slope_lo - tol & candidate_m <= slope_hi + tol]
  if (length(candidate_m) == 0L) {
    return(NULL)
  }

  feasible_m <- candidate_m[vapply(candidate_m, function(m) {
    lower_envelope(m) <= upper_envelope(m) + tol
  }, logical(1))]
  if (length(feasible_m) == 0L) {
    return(NULL)
  }

  intercept_lo <- min(vapply(feasible_m, lower_envelope, numeric(1)))
  intercept_hi <- max(vapply(feasible_m, upper_envelope, numeric(1)))
  if (!is.finite(intercept_lo) || !is.finite(intercept_hi) || intercept_lo > intercept_hi + tol) {
    return(NULL)
  }

  list(
    slope_lo = slope_lo,
    slope_hi = slope_hi,
    intercept_lo = intercept_lo,
    intercept_hi = intercept_hi
  )
}

coefficient_bounds_from_ts_band <- function(ctx,
                                            branch_value,
                                            slope_hat,
                                            intercept_hat,
                                            level = 0.95,
                                            tol = 1e-10) {
  if (is.null(ctx)) {
    return(NULL)
  }

  suffix <- paste0("_", formatC(round(level * 100), format = "d", width = 0))
  lo_name <- paste0("ts_lo", suffix)
  hi_name <- paste0("ts_hi", suffix)
  if (!all(c("length_cm", lo_name, hi_name) %in% names(ctx))) {
    return(NULL)
  }

  length_cm <- suppressWarnings(as.numeric(ctx$length_cm))
  ts_lo <- suppressWarnings(as.numeric(ctx[[lo_name]]))
  ts_hi <- suppressWarnings(as.numeric(ctx[[hi_name]]))
  keep <- is.finite(length_cm) &
    length_cm > 0 &
    is.finite(ts_lo) &
    is.finite(ts_hi)
  if (sum(keep) < 2L) {
    return(NULL)
  }

  x <- log10(length_cm[keep])
  lo <- ts_lo[keep]
  hi <- ts_hi[keep]
  ord <- order(x)
  x <- x[ord]
  lo <- lo[ord]
  hi <- hi[ord]

  lower_fn <- function(m) {
    max(lo - m * x)
  }
  upper_fn <- function(m) {
    min(hi - m * x)
  }

  candidate_vertices <- tibble::tibble()

  if (identical(branch_value, "fixed20_only")) {
    b_lo <- max(lo - slope_hat * x)
    b_hi <- min(hi - slope_hat * x)
    if (!is.finite(b_lo) || !is.finite(b_hi) || b_lo > b_hi + tol) {
      return(NULL)
    }
    candidate_vertices <- tibble::tibble(
      slope = rep(slope_hat, 2L),
      intercept = c(b_lo, b_hi)
    )
    return(list(
      slope_lo = slope_hat,
      slope_hi = slope_hat,
      intercept_lo = b_lo,
      intercept_hi = b_hi,
      vertices = candidate_vertices
    ))
  }

  dx <- outer(x, x, FUN = function(x_i, x_j) x_j - x_i)
  rhs <- outer(lo, hi, FUN = function(lo_i, hi_j) hi_j - lo_i)

  upper_candidates <- rhs[dx > tol] / dx[dx > tol]
  lower_candidates <- rhs[dx < -tol] / dx[dx < -tol]
  slope_lo <- if (length(lower_candidates) > 0) max(lower_candidates, na.rm = TRUE) else -Inf
  slope_hi <- if (length(upper_candidates) > 0) min(upper_candidates, na.rm = TRUE) else Inf

  if (!is.finite(slope_lo) || !is.finite(slope_hi) || slope_lo > slope_hi + tol) {
    return(NULL)
  }

  lower_intersections <- {
    num <- outer(lo, lo, FUN = function(a, b) b - a)
    den <- dx
    vals <- num[abs(den) > tol] / den[abs(den) > tol]
    vals[is.finite(vals)]
  }
  upper_intersections <- {
    num <- outer(hi, hi, FUN = function(a, b) b - a)
    den <- dx
    vals <- num[abs(den) > tol] / den[abs(den) > tol]
    vals[is.finite(vals)]
  }

  candidate_m <- unique(c(
    slope_lo,
    slope_hi,
    lower_intersections,
    upper_intersections,
    slope_hat
  ))
  candidate_m <- candidate_m[
    is.finite(candidate_m) &
      candidate_m >= slope_lo - tol &
      candidate_m <= slope_hi + tol
  ]
  if (length(candidate_m) == 0L) {
    return(NULL)
  }

  lower_vals <- vapply(candidate_m, lower_fn, numeric(1))
  upper_vals <- vapply(candidate_m, upper_fn, numeric(1))
  feasible <- is.finite(lower_vals) &
    is.finite(upper_vals) &
    lower_vals <= upper_vals + tol
  if (!any(feasible)) {
    return(NULL)
  }

  candidate_m <- candidate_m[feasible]
  lower_vals <- lower_vals[feasible]
  upper_vals <- upper_vals[feasible]

  intercept_lo <- min(lower_vals, na.rm = TRUE)
  intercept_hi <- max(upper_vals, na.rm = TRUE)
  if (!is.finite(intercept_lo) || !is.finite(intercept_hi) || intercept_lo > intercept_hi + tol) {
    return(NULL)
  }

  candidate_vertices <- tibble::tibble(
    slope = c(candidate_m, candidate_m),
    intercept = c(lower_vals, upper_vals)
  )

  list(
    slope_lo = slope_lo,
    slope_hi = slope_hi,
    intercept_lo = intercept_lo,
    intercept_hi = intercept_hi,
    vertices = candidate_vertices
  )
}

coefficient_interval_from_context <- function(row_now,
                                              ctx,
                                              coefficient_calibration = NULL) {
  row_now <- tibble::as_tibble(row_now)
  if (nrow(row_now) == 0) {
    return(NULL)
  }

  branch_value <- selected_branches(row_now)[[1]]
  slope_hat <- suppressWarnings(as.numeric(row_now$policy_slope_len[[1]] %||% NA_real_))
  intercept_hat <- suppressWarnings(as.numeric(row_now$policy_intercept_len[[1]] %||% NA_real_))
  Sigma_ctx <- tryCatch(
    suppressWarnings(as.matrix(ctx$coefficient_covariance)),
    error = function(e) NULL
  )
  use_ctx_covariance <- !is.null(Sigma_ctx) &&
    is.matrix(Sigma_ctx) &&
    identical(dim(Sigma_ctx), c(2L, 2L)) &&
    all(is.finite(Sigma_ctx))
  crit <- stats::qnorm(0.975)

  if (isTRUE(use_ctx_covariance)) {
    Sigma <- (Sigma_ctx + t(Sigma_ctx)) / 2
    diag(Sigma) <- pmax(diag(Sigma), 0)
    slope_se <- if (identical(branch_value, "fixed20_only")) 0 else sqrt(Sigma[1, 1])
    intercept_se <- sqrt(Sigma[2, 2])
    slope_half_width <- crit * slope_se
    intercept_half_width <- crit * intercept_se
    corr <- if (identical(branch_value, "fixed20_only")) {
      0
    } else if (Sigma[1, 1] > 0 && Sigma[2, 2] > 0) {
      Sigma[1, 2] / sqrt(Sigma[1, 1] * Sigma[2, 2])
    } else {
      0
    }
    support_n <- suppressWarnings(as.numeric(
      ctx$coefficient_support_n %||%
        ctx$ts_calibration_anchor_n %||%
        NA_real_
    ))
  } else {
    slope_width_obj <- fit_centered_coefficient_width_model(
      coefficient_calibration = coefficient_calibration,
      row_now = row_now,
      response_col = "slope_resid",
      tau = 0.95
    )
    intercept_width_obj <- fit_centered_coefficient_width_model(
      coefficient_calibration = coefficient_calibration,
      row_now = row_now,
      response_col = "intercept_resid",
      tau = 0.95
    )
    if (is.null(intercept_width_obj)) {
      return(NULL)
    }

    slope_half_width <- if (is.null(slope_width_obj)) {
      NA_real_
    } else {
      suppressWarnings(as.numeric(slope_width_obj$width %||% NA_real_))
    }
    intercept_half_width <- suppressWarnings(as.numeric(intercept_width_obj$width %||% NA_real_))
    slope_se <- slope_half_width / crit
    intercept_se <- intercept_half_width / crit
    corr_pool <- dplyr::bind_rows(
      slope_width_obj$pool %||% tibble::tibble(),
      intercept_width_obj$pool %||% tibble::tibble()
    ) |>
      dplyr::distinct()
    corr <- if (!identical(branch_value, "fixed20_only") &&
                nrow(corr_pool) >= 2L &&
                all(c("slope_resid", "intercept_resid") %in% names(corr_pool))) {
      weighted_cor_or_na(
        x = corr_pool$slope_resid,
        y = corr_pool$intercept_resid,
        w = if ("locality_weight" %in% names(corr_pool)) corr_pool$locality_weight else rep(1, nrow(corr_pool))
      )
    } else {
      NA_real_
    }
    if (!is.finite(corr)) {
      corr <- 0
    }
    cov12 <- if (is.finite(slope_se) && is.finite(intercept_se)) {
      corr * slope_se * intercept_se
    } else {
      NA_real_
    }
    Sigma <- matrix(
      c(
        if (identical(branch_value, "fixed20_only")) 0 else slope_se^2,
        cov12,
        cov12,
        intercept_se^2
      ),
      nrow = 2,
      byrow = TRUE
    )
    if (identical(branch_value, "fixed20_only")) {
      Sigma[1, 2] <- 0
      Sigma[2, 1] <- 0
    }
    support_n <- suppressWarnings(as.numeric(
      intercept_width_obj$support_n %||%
        slope_width_obj$support_n %||%
        ctx$ts_calibration_anchor_n %||%
        ctx$coefficient_support_n %||%
        NA_real_
    ))
  }

  if (!is.finite(intercept_half_width)) {
    return(NULL)
  }
  if (!identical(branch_value, "fixed20_only") && !is.finite(slope_half_width)) {
    return(NULL)
  }

  slope_lo <- slope_hat - slope_half_width
  slope_hi <- slope_hat + slope_half_width
  intercept_lo <- intercept_hat - intercept_half_width
  intercept_hi <- intercept_hat + intercept_half_width
  if (!is.finite(intercept_lo) || !is.finite(intercept_hi) || intercept_lo > intercept_hi) {
    return(NULL)
  }
  if (!identical(branch_value, "fixed20_only") &&
      (!is.finite(slope_lo) || !is.finite(slope_hi) || slope_lo > slope_hi)) {
    return(NULL)
  }
  if (!is.finite(corr)) {
    corr <- 0
  }

  list(
    slope_hat = slope_hat,
    intercept_hat = intercept_hat,
    slope_lo = slope_lo,
    slope_hi = slope_hi,
    intercept_lo = intercept_lo,
    intercept_hi = intercept_hi,
    slope_se = slope_se,
    intercept_se = intercept_se,
    covariance = Sigma,
    correlation = corr,
    support_n = support_n,
    q95 = max(
      slope_half_width,
      intercept_half_width,
      na.rm = TRUE
    ),
    candidates = tibble::tibble(
      slope = c(slope_lo, slope_hat, slope_hi),
      intercept = c(intercept_lo, intercept_hat, intercept_hi),
      score = c(abs(slope_lo - slope_hat), 0, abs(slope_hi - slope_hat))
    )
  )
}

#' Build per-anchor TS conformal panel data
#'
#' Reconstructs selected-policy TS curves and their calibrated interval bands
#' for each anchor retained in a policy-selection table.
#'
#' @param selected_tbl Selected-policy table.
#' @param ts_calibration Optional selected-row TS residual calibration table.
#' @param coefficient_calibration Optional selected-row coefficient residual
#'   calibration table used as a fallback when TS calibration is unavailable.
#' @param candidate_models Candidate-model table.
#' @param anchor_scores Anchor-score table.
#' @param config Optional config list.
#' @param length_grid_n Number of grid points per anchor.
#'
#' @return A tibble.
#'
#' @keywords internal
build_ts_conformal_panel_data <- function(selected_tbl,
                                          ts_calibration,
                                          coefficient_calibration = NULL,
                                          candidate_models,
                                          anchor_scores,
                                          competition_policy_tbl = NULL,
                                          config = NULL,
                                          model_scores = NULL,
                                          species_lookup = NULL,
                                          policy_path = NULL,
                                          progress = NULL,
                                          length_grid_n = 400L,
                                          ts_band_method = c("current", "smooth_scale_shape")) {
  ts_band_method <- match.arg(ts_band_method)
  selected_tbl <- tibble::as_tibble(selected_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  anchor_scores <- tibble::as_tibble(anchor_scores)
  competition_policy_tbl <- tibble::as_tibble(competition_policy_tbl %||% tibble::tibble())
  if (nrow(selected_tbl) == 0 || nrow(candidate_models) == 0) {
    return(tibble::tibble())
  }
  policy_lookup <- policy_lookup_table(policy_path = policy_path)
  selected_tbl$selected_policy <- dplyr::coalesce(selected_tbl$selected_policy %||% NULL, selected_tbl$policy %||% NULL)
  selected_tbl$selected_equation_branch_filter <- selected_branches(selected_tbl)
  selected_tbl <- selected_tbl |>
    dplyr::filter(
      !is.na(anchor_model_id),
      !is.na(selected_policy),
      is.finite(policy_slope_len),
      is.finite(policy_intercept_len)
    )
  n_selected <- nrow(selected_tbl)
  purrr::map_dfr(seq_len(n_selected), function(i) {
    row_now <- selected_tbl[i, , drop = FALSE]
    report_progress(
      progress,
      "[Referee]   TS envelope [", i, "/", n_selected, "] ",
      as.character(row_now$anchor_species[[1]] %||% row_now$anchor_model_id[[1]] %||% NA_character_)
    )
    ctx <- strategy_uncertainty_context(
      row_now = row_now,
      candidate_models = candidate_models,
      anchor_scores = anchor_scores,
      policy_tbl = competition_policy_tbl,
      ts_calibration = ts_calibration,
      coefficient_calibration = coefficient_calibration,
      config = config,
      model_scores = model_scores,
      species_lookup = species_lookup,
      policy_lookup = policy_lookup,
      policy_path = policy_path,
      include_competition = TRUE,
      lock_fixed_slope = TRUE,
      length_grid_n = length_grid_n,
      ts_band_method = ts_band_method
    )
    if (is.null(ctx)) {
      return(tibble::tibble())
    }
    tibble::tibble(
      anchor_model_id = ctx$anchor_model_id,
      anchor_species = ctx$anchor_species,
      selected_policy = ctx$selected_policy,
      length_cm = ctx$length_cm,
      u = ctx$u,
      ts_pred = ctx$ts_pred,
      ts_pred_raw = ctx$ts_pred_raw,
      ts_center = ctx$ts_center,
      ts_anchor = ctx$ts_anchor,
      ts_top_candidate = ctx$ts_top_candidate,
      support_modifier = ctx$support_modifier,
      q80_log_length = ctx$q80_L,
      q90_log_length = ctx$q90_L,
      q95_log_length = ctx$q95_L,
      q99_log_length = ctx$q99_L,
      ts_lo_80 = ctx$ts_lo_80,
      ts_hi_80 = ctx$ts_hi_80,
      ts_lo_90 = ctx$ts_lo_90,
      ts_hi_90 = ctx$ts_hi_90,
      ts_lo_95 = ctx$ts_lo_95,
      ts_hi_95 = ctx$ts_hi_95,
      ts_lo_99 = ctx$ts_lo_99,
      ts_hi_99 = ctx$ts_hi_99
    )
  })
}

augment_policy_coefficient_intervals <- function(policy_tbl,
                                                 candidate_models,
                                                 anchor_scores,
                                                 competition_policy_tbl = NULL,
                                                 ts_calibration = NULL,
                                                 coefficient_calibration = NULL,
                                                 config = NULL,
                                                 model_scores = NULL,
                                                 species_lookup = NULL,
                                                 policy_path = NULL,
                                                 length_grid_n = 400L) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  competition_policy_tbl <- tibble::as_tibble(competition_policy_tbl %||% policy_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  anchor_scores <- tibble::as_tibble(anchor_scores)
  if (nrow(policy_tbl) == 0) {
    return(policy_tbl)
  }
  drop_cols <- grep(
    "^policy_(slope|intercept)_len_(se|lo_95|hi_95)(\\..+)?$|^policy_coefficient_(covariance|correlation|support_n|competitor_n)(\\..+)?$",
    names(policy_tbl),
    value = TRUE
  )
  if (length(drop_cols) > 0) {
    policy_tbl <- dplyr::select(policy_tbl, -dplyr::all_of(drop_cols))
  }
  if ("anchor_model_id" %in% names(policy_tbl)) {
    policy_tbl$anchor_model_id <- as.character(policy_tbl$anchor_model_id)
  }
  if ("policy" %in% names(policy_tbl)) {
    policy_tbl$policy <- as.character(policy_tbl$policy)
  }
  if ("equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$equation_branch_filter <- as.character(policy_tbl$equation_branch_filter)
  }
  if ("selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  if ("selected_equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$selected_equation_branch_filter <- as.character(policy_tbl$selected_equation_branch_filter)
  }
  policy_lookup <- policy_lookup_table(policy_path = policy_path)
  if (!"selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- if ("policy" %in% names(policy_tbl)) {
      as.character(policy_tbl$policy)
    } else {
      rep(NA_character_, nrow(policy_tbl))
    }
  } else if ("policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- dplyr::coalesce(
      as.character(policy_tbl$selected_policy),
      as.character(policy_tbl$policy)
    )
  } else {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  policy_tbl$selected_equation_branch_filter <- selected_branches(policy_tbl)
  conditional_cache <- new.env(parent = emptyenv())
  conditional_key <- function(row_now) {
    paste(
      as.character(row_now$anchor_model_id[[1]] %||% NA_character_),
      as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_),
      as.character(selected_branches(row_now)[[1]] %||% NA_character_),
      sep = "|"
    )
  }
  conditional_summary_for_row <- function(row_now) {
    key <- conditional_key(row_now)
    if (exists(key, envir = conditional_cache, inherits = FALSE)) {
      return(get(key, envir = conditional_cache, inherits = FALSE))
    }
    ctx_now <- strategy_uncertainty_context(
      row_now = row_now,
      candidate_models = candidate_models,
      anchor_scores = anchor_scores,
      policy_tbl = tibble::tibble(),
      ts_calibration = ts_calibration,
      coefficient_calibration = coefficient_calibration,
      config = config,
      model_scores = model_scores,
      species_lookup = species_lookup,
      policy_lookup = policy_lookup,
      policy_path = policy_path,
      include_competition = FALSE,
      lock_fixed_slope = TRUE,
      length_grid_n = length_grid_n
    )
    out <- coefficient_interval_from_context(
      row_now = row_now,
      ctx = ctx_now,
      coefficient_calibration = coefficient_calibration
    )
    assign(key, out, envir = conditional_cache)
    out
  }
  summaries <- purrr::map_dfr(seq_len(nrow(policy_tbl)), function(i) {
    row_now <- policy_tbl[i, , drop = FALSE]
    base_summary <- conditional_summary_for_row(row_now)
    if (is.null(base_summary)) {
      return(tibble::tibble(
        anchor_model_id = as.character(row_now$anchor_model_id[[1]]),
        policy = as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]]),
        equation_branch_filter = as.character(row_now$selected_equation_branch_filter[[1]] %||% row_now$equation_branch_filter[[1]]),
        policy_slope_len_se = NA_real_,
        policy_intercept_len_se = NA_real_,
        policy_slope_len_lo_95 = NA_real_,
        policy_slope_len_hi_95 = NA_real_,
        policy_intercept_len_lo_95 = NA_real_,
        policy_intercept_len_hi_95 = NA_real_,
        policy_coefficient_covariance = NA_real_,
        policy_coefficient_correlation = NA_real_,
        policy_coefficient_support_n = NA_real_,
        policy_coefficient_competitor_n = NA_real_
      ))
    }
    competitor_pool <- selection_competition_pool(
      policy_tbl = competition_policy_tbl,
      row_now = row_now,
      config = config
    )
    competitor_summaries <- purrr::map(
      seq_len(nrow(competitor_pool)),
      function(j) conditional_summary_for_row(competitor_pool[j, , drop = FALSE])
    )
    competitor_summaries <- competitor_summaries[!vapply(competitor_summaries, is.null, logical(1))]
    all_summaries <- c(list(base_summary), competitor_summaries)
    slope_lo_vals <- vapply(all_summaries, function(x) suppressWarnings(as.numeric(x$slope_lo %||% NA_real_)), numeric(1))
    slope_hi_vals <- vapply(all_summaries, function(x) suppressWarnings(as.numeric(x$slope_hi %||% NA_real_)), numeric(1))
    intercept_lo_vals <- vapply(all_summaries, function(x) suppressWarnings(as.numeric(x$intercept_lo %||% NA_real_)), numeric(1))
    intercept_hi_vals <- vapply(all_summaries, function(x) suppressWarnings(as.numeric(x$intercept_hi %||% NA_real_)), numeric(1))
    slope_lo_vals <- slope_lo_vals[is.finite(slope_lo_vals)]
    slope_hi_vals <- slope_hi_vals[is.finite(slope_hi_vals)]
    intercept_lo_vals <- intercept_lo_vals[is.finite(intercept_lo_vals)]
    intercept_hi_vals <- intercept_hi_vals[is.finite(intercept_hi_vals)]
    slope_lo <- if (length(slope_lo_vals) > 0) min(slope_lo_vals, na.rm = TRUE) else NA_real_
    slope_hi <- if (length(slope_hi_vals) > 0) max(slope_hi_vals, na.rm = TRUE) else NA_real_
    intercept_lo <- if (length(intercept_lo_vals) > 0) min(intercept_lo_vals, na.rm = TRUE) else NA_real_
    intercept_hi <- if (length(intercept_hi_vals) > 0) max(intercept_hi_vals, na.rm = TRUE) else NA_real_
    slope_se <- if (is.finite(slope_lo) && is.finite(slope_hi)) (slope_hi - slope_lo) / (2 * stats::qnorm(0.975)) else NA_real_
    intercept_se <- if (is.finite(intercept_lo) && is.finite(intercept_hi)) (intercept_hi - intercept_lo) / (2 * stats::qnorm(0.975)) else NA_real_
    Sigma <- base_summary$covariance
    tibble::tibble(
      anchor_model_id = as.character(row_now$anchor_model_id[[1]]),
      policy = as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]]),
      equation_branch_filter = as.character(row_now$selected_equation_branch_filter[[1]] %||% row_now$equation_branch_filter[[1]]),
      policy_slope_len_se = slope_se,
      policy_intercept_len_se = intercept_se,
      policy_slope_len_lo_95 = slope_lo,
      policy_slope_len_hi_95 = slope_hi,
      policy_intercept_len_lo_95 = intercept_lo,
      policy_intercept_len_hi_95 = intercept_hi,
      policy_coefficient_covariance = Sigma[1, 2],
      policy_coefficient_correlation = base_summary$correlation %||% NA_real_,
      policy_coefficient_support_n = suppressWarnings(as.numeric(base_summary$support_n %||% NA_real_)),
      policy_coefficient_competitor_n = length(all_summaries)
    )
  })
  join_cols <- intersect(c("anchor_model_id", "policy", "equation_branch_filter"), intersect(names(policy_tbl), names(summaries)))
  policy_tbl |>
    dplyr::left_join(summaries, by = join_cols)
}

augment_policy_conditional_coefficient_intervals <- function(policy_tbl,
                                                             candidate_models,
                                                             anchor_scores,
                                                             ts_calibration = NULL,
                                                             coefficient_calibration = NULL,
                                                             config = NULL,
                                                             model_scores = NULL,
                                                             species_lookup = NULL,
                                                             policy_path = NULL,
                                                             length_grid_n = 400L) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  anchor_scores <- tibble::as_tibble(anchor_scores)
  if (nrow(policy_tbl) == 0) {
    return(policy_tbl)
  }
  drop_cols <- grep(
    "^conditional_policy_(slope|intercept)_len_(se|lo_95|hi_95)$|^conditional_policy_coefficient_(covariance|correlation|support_n|competitor_n)$",
    names(policy_tbl),
    value = TRUE
  )
  if (length(drop_cols) > 0) {
    policy_tbl <- dplyr::select(policy_tbl, -dplyr::all_of(drop_cols))
  }
  if ("anchor_model_id" %in% names(policy_tbl)) {
    policy_tbl$anchor_model_id <- as.character(policy_tbl$anchor_model_id)
  }
  if ("policy" %in% names(policy_tbl)) {
    policy_tbl$policy <- as.character(policy_tbl$policy)
  }
  if ("equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$equation_branch_filter <- as.character(policy_tbl$equation_branch_filter)
  }
  if ("selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  if ("selected_equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$selected_equation_branch_filter <- as.character(policy_tbl$selected_equation_branch_filter)
  }
  policy_lookup <- policy_lookup_table(policy_path = policy_path)
  if (!"selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- if ("policy" %in% names(policy_tbl)) {
      as.character(policy_tbl$policy)
    } else {
      rep(NA_character_, nrow(policy_tbl))
    }
  } else if ("policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- dplyr::coalesce(
      as.character(policy_tbl$selected_policy),
      as.character(policy_tbl$policy)
    )
  } else {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  policy_tbl$selected_equation_branch_filter <- selected_branches(policy_tbl)
  conditional_cache <- new.env(parent = emptyenv())
  conditional_key <- function(row_now) {
    paste(
      as.character(row_now$anchor_model_id[[1]] %||% NA_character_),
      as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_),
      as.character(selected_branches(row_now)[[1]] %||% NA_character_),
      sep = "|"
    )
  }
  conditional_summary_for_row <- function(row_now) {
    key <- conditional_key(row_now)
    if (exists(key, envir = conditional_cache, inherits = FALSE)) {
      return(get(key, envir = conditional_cache, inherits = FALSE))
    }
    ctx_now <- strategy_uncertainty_context(
      row_now = row_now,
      candidate_models = candidate_models,
      anchor_scores = anchor_scores,
      policy_tbl = tibble::tibble(),
      ts_calibration = ts_calibration,
      coefficient_calibration = coefficient_calibration,
      config = config,
      model_scores = model_scores,
      species_lookup = species_lookup,
      policy_lookup = policy_lookup,
      policy_path = policy_path,
      include_competition = FALSE,
      lock_fixed_slope = TRUE,
      length_grid_n = length_grid_n
    )
    out <- coefficient_interval_from_context(
      row_now = row_now,
      ctx = ctx_now,
      coefficient_calibration = coefficient_calibration
    )
    assign(key, out, envir = conditional_cache)
    out
  }
  summaries <- purrr::map_dfr(seq_len(nrow(policy_tbl)), function(i) {
    row_now <- policy_tbl[i, , drop = FALSE]
    base_summary <- conditional_summary_for_row(row_now)
    if (is.null(base_summary)) {
      return(tibble::tibble(
        anchor_model_id = as.character(row_now$anchor_model_id[[1]]),
        policy = as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]]),
        equation_branch_filter = as.character(row_now$selected_equation_branch_filter[[1]] %||% row_now$equation_branch_filter[[1]]),
        conditional_policy_slope_len_se = NA_real_,
        conditional_policy_intercept_len_se = NA_real_,
        conditional_policy_slope_len_lo_95 = NA_real_,
        conditional_policy_slope_len_hi_95 = NA_real_,
        conditional_policy_intercept_len_lo_95 = NA_real_,
        conditional_policy_intercept_len_hi_95 = NA_real_,
        conditional_policy_coefficient_covariance = NA_real_,
        conditional_policy_coefficient_correlation = NA_real_,
        conditional_policy_coefficient_support_n = NA_real_,
        conditional_policy_coefficient_competitor_n = NA_real_
      ))
    }
    Sigma <- base_summary$covariance
    slope_se <- base_summary$slope_se
    intercept_se <- base_summary$intercept_se
    tibble::tibble(
      anchor_model_id = as.character(row_now$anchor_model_id[[1]]),
      policy = as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]]),
      equation_branch_filter = as.character(row_now$selected_equation_branch_filter[[1]] %||% row_now$equation_branch_filter[[1]]),
      conditional_policy_slope_len_se = slope_se,
      conditional_policy_intercept_len_se = intercept_se,
      conditional_policy_slope_len_lo_95 = base_summary$slope_lo,
      conditional_policy_slope_len_hi_95 = base_summary$slope_hi,
      conditional_policy_intercept_len_lo_95 = base_summary$intercept_lo,
      conditional_policy_intercept_len_hi_95 = base_summary$intercept_hi,
      conditional_policy_coefficient_covariance = Sigma[1, 2],
      conditional_policy_coefficient_correlation = base_summary$correlation %||% NA_real_,
      conditional_policy_coefficient_support_n = suppressWarnings(as.numeric(base_summary$support_n %||% NA_real_)),
      conditional_policy_coefficient_competitor_n = 1
    )
  })
  join_cols <- intersect(c("anchor_model_id", "policy", "equation_branch_filter"), intersect(names(policy_tbl), names(summaries)))
  policy_tbl |>
    dplyr::left_join(summaries, by = join_cols)
}

augment_policy_ts_envelope_summary <- function(policy_tbl,
                                               candidate_models,
                                               anchor_scores,
                                               ts_calibration = NULL,
                                               coefficient_calibration = NULL,
                                               config = NULL,
                                               model_scores = NULL,
                                               species_lookup = NULL,
                                               policy_path = NULL,
                                               length_grid_n = 400L) {
  policy_tbl <- tibble::as_tibble(policy_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  anchor_scores <- tibble::as_tibble(anchor_scores)
  if (nrow(policy_tbl) == 0) {
    return(policy_tbl)
  }

  drop_cols <- grep(
    "^candidate_ts_(q95|q99)_(mean|max)$|^candidate_ts_(hi99_max|lo99_min)$|^candidate_ts_interval99_mean$",
    names(policy_tbl),
    value = TRUE
  )
  if (length(drop_cols) > 0) {
    policy_tbl <- dplyr::select(policy_tbl, -dplyr::all_of(drop_cols))
  }

  if ("anchor_model_id" %in% names(policy_tbl)) {
    policy_tbl$anchor_model_id <- as.character(policy_tbl$anchor_model_id)
  }
  if ("policy" %in% names(policy_tbl)) {
    policy_tbl$policy <- as.character(policy_tbl$policy)
  }
  if ("equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$equation_branch_filter <- as.character(policy_tbl$equation_branch_filter)
  }
  if ("selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  if ("selected_equation_branch_filter" %in% names(policy_tbl)) {
    policy_tbl$selected_equation_branch_filter <- as.character(policy_tbl$selected_equation_branch_filter)
  }
  policy_lookup <- policy_lookup_table(policy_path = policy_path)
  if (!"selected_policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- if ("policy" %in% names(policy_tbl)) {
      as.character(policy_tbl$policy)
    } else {
      rep(NA_character_, nrow(policy_tbl))
    }
  } else if ("policy" %in% names(policy_tbl)) {
    policy_tbl$selected_policy <- dplyr::coalesce(
      as.character(policy_tbl$selected_policy),
      as.character(policy_tbl$policy)
    )
  } else {
    policy_tbl$selected_policy <- as.character(policy_tbl$selected_policy)
  }
  policy_tbl$selected_equation_branch_filter <- selected_branches(policy_tbl)

  summary_cache <- new.env(parent = emptyenv())
  summary_key <- function(row_now) {
    paste(
      as.character(row_now$anchor_model_id[[1]] %||% NA_character_),
      as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_),
      as.character(selected_branches(row_now)[[1]] %||% NA_character_),
      sep = "|"
    )
  }
  ts_summary_for_row <- function(row_now) {
    key <- summary_key(row_now)
    if (exists(key, envir = summary_cache, inherits = FALSE)) {
      return(get(key, envir = summary_cache, inherits = FALSE))
    }
    ctx_now <- strategy_uncertainty_context(
      row_now = row_now,
      candidate_models = candidate_models,
      anchor_scores = anchor_scores,
      policy_tbl = tibble::tibble(),
      ts_calibration = ts_calibration,
      coefficient_calibration = coefficient_calibration,
      config = config,
      model_scores = model_scores,
      species_lookup = species_lookup,
      policy_lookup = policy_lookup,
      policy_path = policy_path,
      include_competition = FALSE,
      lock_fixed_slope = TRUE,
      length_grid_n = length_grid_n
    )
    if (is.null(ctx_now)) {
      out <- NULL
    } else {
      weights <- suppressWarnings(as.numeric(ctx_now$pdf_weight %||% rep(NA_real_, length(ctx_now$length_cm))))
      if (!any(is.finite(weights) & weights > 0)) {
        weights <- rep(1 / length(ctx_now$length_cm), length(ctx_now$length_cm))
      } else {
        weights[!is.finite(weights) | weights < 0] <- 0
        weights <- weights / sum(weights, na.rm = TRUE)
      }
      q95_vals <- suppressWarnings(as.numeric(ctx_now$q95_L))
      q99_vals <- suppressWarnings(as.numeric(ctx_now$q99_L))
      hi99_vals <- suppressWarnings(as.numeric(ctx_now$ts_hi_99))
      lo99_vals <- suppressWarnings(as.numeric(ctx_now$ts_lo_99))
      out <- list(
        candidate_ts_q95_mean = weighted_mean_or_na(q95_vals, weights),
        candidate_ts_q99_mean = weighted_mean_or_na(q99_vals, weights),
        candidate_ts_q95_max = {
          q95_keep <- q95_vals[is.finite(q95_vals)]
          if (length(q95_keep) == 0L) NA_real_ else max(q95_keep)
        },
        candidate_ts_q99_max = {
          q99_keep <- q99_vals[is.finite(q99_vals)]
          if (length(q99_keep) == 0L) NA_real_ else max(q99_keep)
        },
        candidate_ts_hi99_max = {
          hi_keep <- hi99_vals[is.finite(hi99_vals)]
          if (length(hi_keep) == 0L) NA_real_ else max(hi_keep)
        },
        candidate_ts_lo99_min = {
          lo_keep <- lo99_vals[is.finite(lo99_vals)]
          if (length(lo_keep) == 0L) NA_real_ else min(lo_keep)
        }
      )
      out$candidate_ts_interval99_mean <- if (is.finite(out$candidate_ts_q99_mean)) {
        2 * out$candidate_ts_q99_mean
      } else {
        NA_real_
      }
    }
    assign(key, out, envir = summary_cache)
    out
  }

  summaries <- purrr::map_dfr(seq_len(nrow(policy_tbl)), function(i) {
    row_now <- policy_tbl[i, , drop = FALSE]
    ts_summary <- ts_summary_for_row(row_now)
    if (is.null(ts_summary)) {
      ts_summary <- list(
        candidate_ts_q95_mean = NA_real_,
        candidate_ts_q99_mean = NA_real_,
        candidate_ts_q95_max = NA_real_,
        candidate_ts_q99_max = NA_real_,
        candidate_ts_hi99_max = NA_real_,
        candidate_ts_lo99_min = NA_real_,
        candidate_ts_interval99_mean = NA_real_
      )
    }
    tibble::tibble(
      anchor_model_id = as.character(row_now$anchor_model_id[[1]]),
      policy = as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]]),
      equation_branch_filter = as.character(row_now$selected_equation_branch_filter[[1]] %||% row_now$equation_branch_filter[[1]]),
      candidate_ts_q95_mean = ts_summary$candidate_ts_q95_mean,
      candidate_ts_q99_mean = ts_summary$candidate_ts_q99_mean,
      candidate_ts_q95_max = ts_summary$candidate_ts_q95_max,
      candidate_ts_q99_max = ts_summary$candidate_ts_q99_max,
      candidate_ts_hi99_max = ts_summary$candidate_ts_hi99_max,
      candidate_ts_lo99_min = ts_summary$candidate_ts_lo99_min,
      candidate_ts_interval99_mean = ts_summary$candidate_ts_interval99_mean
    )
  })

  join_cols <- intersect(c("anchor_model_id", "policy", "equation_branch_filter"), intersect(names(policy_tbl), names(summaries)))
  policy_tbl |>
    dplyr::left_join(summaries, by = join_cols)
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


