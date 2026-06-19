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
  ts_cal <- standardize_policies(ts_error)
  ts_cal$policy <- resolve_policy_names(ts_cal)
  ts_cal <- ts_cal |>
    dplyr::group_by(policy, equation_branch_filter, u) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_ts_error = stats::median(ts_error, na.rm = TRUE),
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
                                              selected_tbl) {
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
    dplyr::distinct()
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
    ) |>
    dplyr::inner_join(
      selected_keys,
      by = c("policy", "equation_branch_filter")
    )
  if (nrow(selected_ts) == 0) {
    return(tibble::tibble())
  }

  selected_ts |>
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
  if (nrow(selected_tbl) == 0 || nrow(candidate_models) == 0) {
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

  truth_tbl <- candidate_models |>
    dplyr::transmute(
      anchor_model_id = as.character(.data[[id_col]]),
      anchor_species = if ("species_name" %in% names(candidate_models)) as.character(species_name) else rep(NA_character_, dplyr::n()),
      anchor_family = if ("family_name" %in% names(candidate_models)) as.character(family_name) else rep(NA_character_, dplyr::n()),
      anchor_slope_len = suppressWarnings(as.numeric(slope_len)),
      anchor_intercept_len = suppressWarnings(as.numeric(intercept_len))
    )

  selected_coefficients <- selected_tbl |>
    dplyr::transmute(
      anchor_model_id = as.character(anchor_model_id),
      anchor_species = if ("anchor_species" %in% names(selected_tbl)) {
        as.character(anchor_species)
      } else {
        rep(NA_character_, dplyr::n())
      },
      anchor_family = if ("anchor_family" %in% names(selected_tbl)) {
        as.character(anchor_family)
      } else {
        rep(NA_character_, dplyr::n())
      },
      policy = selected_policy_values,
      equation_branch_filter = selected_branches(selected_tbl),
      post_selection_support_bin = selected_support_bins,
      policy_slope_len = if ("policy_slope_len" %in% names(selected_tbl)) {
        suppressWarnings(as.numeric(policy_slope_len))
      } else {
        rep(NA_real_, dplyr::n())
      },
      policy_intercept_len = if ("policy_intercept_len" %in% names(selected_tbl)) {
        suppressWarnings(as.numeric(policy_intercept_len))
      } else {
        rep(NA_real_, dplyr::n())
      }
    )
  if ((!all(c("policy_slope_len", "policy_intercept_len") %in% names(selected_tbl)) ||
      !any(is.finite(selected_coefficients$policy_slope_len) & is.finite(selected_coefficients$policy_intercept_len))) &&
      !is.null(ts_error) && nrow(ts_error) > 0) {
    ts_error <- standardize_policies(ts_error)
    ts_error$policy <- resolve_policy_names(ts_error)
    if (all(c("policy_slope_len", "policy_intercept_len") %in% names(ts_error))) {
      derived_coefficients <- ts_error |>
        dplyr::mutate(
          anchor_model_id = as.character(anchor_model_id),
          policy_slope_len = suppressWarnings(as.numeric(policy_slope_len)),
          policy_intercept_len = suppressWarnings(as.numeric(policy_intercept_len))
        ) |>
        dplyr::group_by(anchor_model_id, policy, equation_branch_filter) |>
        dplyr::summarise(
          policy_slope_len = stats::median(policy_slope_len, na.rm = TRUE),
          policy_intercept_len = stats::median(policy_intercept_len, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::filter(
          is.finite(policy_slope_len),
          is.finite(policy_intercept_len)
        )
    } else if (all(c("length_cm", "ts_pred") %in% names(ts_error))) {
      derived_coefficients <- ts_error |>
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
        )
    } else {
      derived_coefficients <- tibble::tibble()
    }
    selected_coefficients <- selected_coefficients |>
      dplyr::select(-dplyr::any_of(c("policy_slope_len", "policy_intercept_len"))) |>
      dplyr::left_join(
        derived_coefficients,
        by = c("anchor_model_id", "policy", "equation_branch_filter")
      )
  }

  selected_coefficients |>
    dplyr::inner_join(truth_tbl, by = "anchor_model_id") |>
    dplyr::mutate(
      anchor_species = dplyr::coalesce(anchor_species.x, anchor_species.y),
      anchor_family = dplyr::coalesce(anchor_family.x, anchor_family.y)
    ) |>
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
      anchor_family,
      policy,
      equation_branch_filter,
      post_selection_support_bin,
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
      q80_ts_abs_dev = pmax(q80_ts_abs_dev, 0, na.rm = TRUE),
      q90_ts_abs_dev = pmax(q90_ts_abs_dev, q80_ts_abs_dev, 0, na.rm = TRUE),
      q95_ts_abs_dev = pmax(q95_ts_abs_dev, q90_ts_abs_dev, 0, na.rm = TRUE),
      q99_ts_abs_dev = pmax(q99_ts_abs_dev, q95_ts_abs_dev, 0, na.rm = TRUE)
    )
}

select_ts_calibration_curve <- function(ts_calibration,
                                        row_now,
                                        min_u = 4L,
                                        min_avg_scores_per_u = 2) {
  ts_calibration <- tibble::as_tibble(ts_calibration)
  row_now <- tibble::as_tibble(row_now)
  if (nrow(ts_calibration) == 0 || nrow(row_now) == 0) {
    return(tibble::tibble())
  }

  anchor_id_value <- as.character(row_now$anchor_model_id[[1]] %||% NA_character_)
  anchor_species_value <- as.character(row_now$anchor_species[[1]] %||% NA_character_)
  anchor_family_value <- as.character(row_now$anchor_family[[1]] %||% NA_character_)
  policy_value <- as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]] %||% NA_character_)
  branch_value <- selected_branches(row_now)[[1]]
  if ("anchor_model_id" %in% names(ts_calibration)) {
    ts_calibration$anchor_model_id <- as.character(ts_calibration$anchor_model_id)
  } else {
    ts_calibration$anchor_model_id <- NA_character_
  }
  if ("anchor_species" %in% names(ts_calibration)) {
    ts_calibration$anchor_species <- as.character(ts_calibration$anchor_species)
  } else {
    ts_calibration$anchor_species <- NA_character_
  }
  if ("anchor_family" %in% names(ts_calibration)) {
    ts_calibration$anchor_family <- as.character(ts_calibration$anchor_family)
  } else {
    ts_calibration$anchor_family <- NA_character_
  }
  if ("policy" %in% names(ts_calibration)) {
    ts_calibration$policy <- as.character(ts_calibration$policy)
  } else {
    ts_calibration$policy <- NA_character_
  }
  ts_calibration$equation_branch_filter <- standardize_branches(ts_calibration)

  distinct_u <- function(tbl) {
    length(unique(suppressWarnings(as.numeric(tbl$u[is.finite(suppressWarnings(as.numeric(tbl$u)))]))))
  }
  choose_curve <- function(tbl) {
    collapsed <- collapse_selected_ts_calibration(tbl)
    if (nrow(collapsed) == 0) {
      return(NULL)
    }
    n_distinct_u <- distinct_u(collapsed)
    total_n <- sum(suppressWarnings(as.numeric(collapsed$n)), na.rm = TRUE)
    if (n_distinct_u >= as.integer(min_u) &&
        is.finite(total_n) &&
        total_n >= as.numeric(min_avg_scores_per_u) * n_distinct_u) {
      return(collapsed)
    }
    NULL
  }

  subset_curve <- function(anchor_id = NA_character_,
                           anchor_species = NA_character_,
                           anchor_family = NA_character_,
                           policy = NA_character_,
                           branch = NA_character_) {
    out <- ts_calibration
    if (!is.na(anchor_id)) {
      out <- dplyr::filter(out, anchor_model_id == !!anchor_id)
    }
    if (!is.na(anchor_species)) {
      out <- dplyr::filter(out, anchor_species == !!anchor_species)
    }
    if (!is.na(anchor_family)) {
      out <- dplyr::filter(out, anchor_family == !!anchor_family)
    }
    if (!is.na(policy)) {
      out <- dplyr::filter(out, policy == !!policy)
    }
    if (!is.na(branch)) {
      out <- dplyr::filter(out, equation_branch_filter == !!branch)
    }
    out
  }

  candidate_tables <- list()
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    anchor_id = anchor_id_value,
    policy = policy_value,
    branch = branch_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    anchor_id = anchor_id_value,
    policy = policy_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    anchor_id = anchor_id_value,
    branch = branch_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    anchor_species = anchor_species_value,
    policy = policy_value,
    branch = branch_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    anchor_species = anchor_species_value,
    policy = policy_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    anchor_species = anchor_species_value,
    branch = branch_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    anchor_species = anchor_species_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    anchor_family = anchor_family_value,
    policy = policy_value,
    branch = branch_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    anchor_family = anchor_family_value,
    policy = policy_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    anchor_family = anchor_family_value,
    branch = branch_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    anchor_family = anchor_family_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    policy = policy_value,
    branch = branch_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    policy = policy_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- subset_curve(
    branch = branch_value
  )
  candidate_tables[[length(candidate_tables) + 1L]] <- ts_calibration

  best_curve <- tibble::tibble()
  best_u <- 0L
  for (tbl_now in candidate_tables) {
    curve_now <- collapse_selected_ts_calibration(tbl_now)
    u_now <- distinct_u(curve_now)
    if (u_now > best_u) {
      best_curve <- curve_now
      best_u <- u_now
    }
    chosen <- choose_curve(tbl_now)
    if (!is.null(chosen)) {
      return(chosen)
    }
  }

  best_curve
}

anchor_local_log_sigma_calibration <- function(row_now,
                                               ts_calibration,
                                               candidate_models,
                                               length_grid_n = 400L) {
  row_now <- tibble::as_tibble(row_now)
  candidate_models <- tibble::as_tibble(candidate_models)
  if (nrow(row_now) == 0 || nrow(candidate_models) == 0) {
    return(NULL)
  }
  curve <- select_ts_calibration_curve(
    ts_calibration = ts_calibration,
    row_now = row_now
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
                                             min_rows = 4L) {
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
  anchor_species_value <- as.character(row_now$anchor_species[[1]] %||% NA_character_)
  anchor_family_value <- as.character(row_now$anchor_family[[1]] %||% NA_character_)
  branch_value <- selected_branches(row_now)[[1]]
  subset_pool <- function(anchor_species = NA_character_,
                          anchor_family = NA_character_,
                          policy = NA_character_,
                          branch = NA_character_) {
    out <- coefficient_calibration
    if (!is.na(anchor_species)) {
      out <- dplyr::filter(out, anchor_species == !!anchor_species)
    }
    if (!is.na(anchor_family)) {
      out <- dplyr::filter(out, anchor_family == !!anchor_family)
    }
    if (!is.na(policy)) {
      out <- dplyr::filter(out, policy == !!policy)
    }
    if (!is.na(branch)) {
      out <- dplyr::filter(out, equation_branch_filter == !!branch)
    }
    out
  }

  candidate_pools <- list(
    subset_pool(anchor_species = anchor_species_value, policy = policy_value, branch = branch_value),
    subset_pool(anchor_species = anchor_species_value, policy = policy_value),
    subset_pool(anchor_species = anchor_species_value, branch = branch_value),
    subset_pool(anchor_species = anchor_species_value),
    subset_pool(anchor_family = anchor_family_value, policy = policy_value, branch = branch_value),
    subset_pool(anchor_family = anchor_family_value, policy = policy_value),
    subset_pool(anchor_family = anchor_family_value, branch = branch_value),
    subset_pool(anchor_family = anchor_family_value),
    subset_pool(policy = policy_value, branch = branch_value),
    subset_pool(policy = policy_value),
    subset_pool(branch = branch_value),
    coefficient_calibration
  )

  best_pool <- tibble::tibble()
  best_n <- 0L
  first_specific_pool <- tibble::tibble()
  specific_pool_count <- length(candidate_pools) - 2L
  for (i in seq_len(specific_pool_count)) {
    pool_now <- candidate_pools[[i]]
    n_now <- nrow(pool_now)
    if (n_now > best_n) {
      best_pool <- pool_now
      best_n <- n_now
    }
    if (n_now >= as.integer(min_rows)) {
      return(pool_now)
    }
    if (nrow(first_specific_pool) == 0L && n_now >= 2L) {
      first_specific_pool <- pool_now
    }
  }

  for (pool_now in candidate_pools[(specific_pool_count + 1L):length(candidate_pools)]) {
    n_now <- nrow(pool_now)
    if (n_now > best_n) {
      best_pool <- pool_now
      best_n <- n_now
    }
    if (n_now >= as.integer(min_rows)) {
      return(pool_now)
    }
    if (nrow(first_specific_pool) == 0L && n_now >= 2L) {
      first_specific_pool <- pool_now
    }
  }

  if (nrow(first_specific_pool) >= 2L) {
    return(first_specific_pool)
  }
  if (nrow(best_pool) >= 2L) {
    return(best_pool)
  }
  tibble::tibble()
}

selected_coefficient_covariance <- function(coefficient_calibration,
                                            row_now,
                                            min_rows = 4L) {
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

  uncertainty_rule <- normalize_uncertainty_rule(
    policy_selector_config_value(
      config,
      "uncertainty_rule",
      sections = c("selection", "policy")
    ) %||% "tolerance"
  )
  u_tol_rel <- suppressWarnings(as.numeric(
    policy_selector_config_value(
      config,
      "u_tol_rel",
      sections = c("selection", "policy")
    ) %||%
      policy_selector_config_value(
        config,
        "uncertainty_relative_tolerance",
        sections = c("selection", "policy")
      ) %||% NA_real_
  ))
  if (!is.finite(u_tol_rel)) {
    u_tol_rel <- 0.25
  }
  u_tol_abs <- suppressWarnings(as.numeric(
    policy_selector_config_value(
      config,
      "u_tol_abs",
      sections = c("selection", "policy")
    ) %||%
      policy_selector_config_value(
        config,
        "uncertainty_absolute_tolerance",
        sections = c("selection", "policy")
      ) %||% NA_real_
  ))
  if (!is.finite(u_tol_abs)) {
    u_tol_abs <- 0.05
  }

  validation_error <- dplyr::coalesce(
    if ("anchor_selection_validation_error" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$anchor_selection_validation_error)) else rep(NA_real_, nrow(policy_tbl)),
    if ("species_block_median_abs_log_error" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$species_block_median_abs_log_error)) else rep(NA_real_, nrow(policy_tbl)),
    if ("mean_species_median_abs_log" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$mean_species_median_abs_log)) else rep(NA_real_, nrow(policy_tbl)),
    if ("median_abs_log" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$median_abs_log)) else rep(NA_real_, nrow(policy_tbl)),
    if (".meta_predicted_score" %in% names(policy_tbl)) suppressWarnings(as.numeric(policy_tbl$.meta_predicted_score)) else rep(NA_real_, nrow(policy_tbl))
  )
  specificity_rank <- if ("specificity_rank" %in% names(policy_tbl)) {
    suppressWarnings(as.numeric(policy_tbl$specificity_rank))
  } else {
    policy_specificity_rank(
      policy = policy_tbl$policy %||% rep(NA_character_, nrow(policy_tbl)),
      candidate_pool = policy_tbl$candidate_pool %||% NULL,
      aggregation_method = policy_tbl$aggregation_method %||% NULL,
      policy_family = policy_tbl$policy_family %||% NULL
    )
  }
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
      .specificity_rank = specificity_rank,
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

  score_pool <- pool |>
    dplyr::filter(.validation_error <= min_validation_error + u_tol_abs)
  if (nrow(score_pool) == 0) {
    score_pool <- pool |>
      dplyr::filter(.validation_error <= min_validation_error + 1e-12)
  }
  if (nrow(score_pool) == 0) {
    return(pool[0, , drop = FALSE])
  }

  min_specificity_rank <- suppressWarnings(min(score_pool$.specificity_rank, na.rm = TRUE))
  if (is.finite(min_specificity_rank)) {
    more_specific_pool <- score_pool |>
      dplyr::filter(
        is.finite(.specificity_rank),
        .specificity_rank <= min_specificity_rank
      )
    if (nrow(more_specific_pool) > 0) {
      score_pool <- more_specific_pool
    }
  }

  width_threshold <- suppressWarnings(as.numeric(
    row_now$anchor_selection_uncertainty_threshold[[1]] %||% NA_real_
  ))
  if (!is.finite(width_threshold)) {
    min_width <- suppressWarnings(min(score_pool$.uncertainty_width, na.rm = TRUE))
    if (!is.finite(min_width)) {
      return(score_pool[0, , drop = FALSE])
    }
    width_threshold <- uncertainty_width_threshold(
      min_width = min_width,
      uncertainty_rule = uncertainty_rule,
      u_tol_abs = u_tol_abs,
      u_tol_rel = u_tol_rel
    )
  }

  score_pool |>
    dplyr::filter(.uncertainty_width <= width_threshold + 1e-12)
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
                                         length_grid_n = 400L) {
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
  ts_anchor <- anchor_slope * log10(length_grid) + anchor_intercept
  policy_name <- as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]])
  branch_value <- selected_branches(row_now)[[1]]
  is_fixed_branch <- identical(branch_value, "fixed20_only")
  q_scalars <- strategy_q_scalars(row_now)
  ts_curve_calibration <- select_ts_calibration_curve(
    ts_calibration = ts_calibration,
    row_now = row_now
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
      q95_scalar = q_scalars$q95
    )
    Sigma <- sigma_result$covariance
    shape_modifier <- sigma_result$shape_modifier
  } else {
    covariance_result <- calibrated_coefficient_covariance(
      length_grid = length_grid,
      pdf_weights = pdf_weights,
      q95_scalar = q_scalars$q95
    )
    shape_modifier <- covariance_result$shape_modifier
    Sigma <- covariance_result$covariance
  }

  if (length(shape_modifier) != length(length_grid) || !all(is.finite(shape_modifier))) {
    shape_modifier <- rep(1, length(length_grid))
  }
  q80_L <- q_scalars$q80 * shape_modifier
  q90_L <- q_scalars$q90 * shape_modifier
  q95_L <- q_scalars$q95 * shape_modifier
  q99_L <- q_scalars$q99 * shape_modifier
  ts_pred_raw <- slope_hat * log10(length_grid) + intercept_hat
  if (nrow(ts_curve_calibration) > 0) {
    target_u <- seq(0, 1, length.out = length(length_grid))
    approx_ts_calibration <- function(candidates) {
      values <- resolve_numeric_candidates(ts_curve_calibration, candidates)
      keep <- is.finite(ts_curve_calibration$u) & is.finite(values)
      if (!any(keep)) {
        return(rep(NA_real_, length(target_u)))
      }
      if (sum(keep) == 1) {
        return(rep(values[keep][[1]], length(target_u)))
      }
      stats::approx(
        x = ts_curve_calibration$u[keep],
        y = values[keep],
        xout = target_u,
        rule = 2,
        ties = "ordered"
      )$y
    }
    median_ts_error_curve <- dplyr::coalesce(
      approx_ts_calibration(c("median_ts_error")),
      0
    )
    q80_ts_dev <- pmax(approx_ts_calibration(c("q80_ts_abs_dev")), 0, na.rm = TRUE)
    q90_ts_dev <- pmax(approx_ts_calibration(c("q90_ts_abs_dev")), q80_ts_dev, 0, na.rm = TRUE)
    q95_ts_dev <- pmax(approx_ts_calibration(c("q95_ts_abs_dev")), q90_ts_dev, 0, na.rm = TRUE)
    q99_ts_dev <- pmax(approx_ts_calibration(c("q99_ts_abs_dev")), q95_ts_dev, 0, na.rm = TRUE)
    ts_center <- ts_pred_raw + median_ts_error_curve
    ts_lo_80 <- ts_center - q80_ts_dev
    ts_hi_80 <- ts_center + q80_ts_dev
    ts_lo_90 <- ts_center - q90_ts_dev
    ts_hi_90 <- ts_center + q90_ts_dev
    ts_lo_95 <- ts_center - q95_ts_dev
    ts_hi_95 <- ts_center + q95_ts_dev
    ts_lo_99 <- ts_center - q99_ts_dev
    ts_hi_99 <- ts_center + q99_ts_dev
    q95_scalar_local <- stats::weighted.mean(q95_ts_dev, pdf_weights, na.rm = TRUE)
    covariance_result <- if (!is.null(raw_covariance) && all(is.finite(raw_covariance))) {
      rescale_coefficient_covariance(
        covariance = raw_covariance,
        length_grid = length_grid,
        pdf_weights = pdf_weights,
        q95_scalar = q95_scalar_local
      )
    } else {
      calibrated_coefficient_covariance(
        length_grid = length_grid,
        pdf_weights = pdf_weights,
        q95_scalar = q95_scalar_local
      )
    }
    if (all(is.finite(covariance_result$covariance))) {
      Sigma <- covariance_result$covariance
    }
    if (length(covariance_result$shape_modifier) == length(length_grid) &&
        all(is.finite(covariance_result$shape_modifier))) {
      shape_modifier <- covariance_result$shape_modifier
    }
    if (!coefficient_covariance_is_sane(
      covariance = Sigma,
      slope_hat = slope_hat,
      intercept_hat = intercept_hat
    )) {
      fallback_covariance <- calibrated_coefficient_covariance(
        length_grid = length_grid,
        pdf_weights = pdf_weights,
        q95_scalar = q_scalars$q95
      )
      if (all(is.finite(fallback_covariance$covariance))) {
        Sigma <- fallback_covariance$covariance
      }
      if (length(fallback_covariance$shape_modifier) == length(length_grid) &&
          all(is.finite(fallback_covariance$shape_modifier))) {
        shape_modifier <- fallback_covariance$shape_modifier
      }
    }
    q80_L <- q80_ts_dev
    q90_L <- q90_ts_dev
    q95_L <- q95_ts_dev
    q99_L <- q99_ts_dev
  } else {
    sigma_pred <- 10^(ts_pred_raw / 10)
    q80_shift <- lognormal_mean_centered_shifts(q80_L, stats::qnorm(0.90))
    q90_shift <- lognormal_mean_centered_shifts(q90_L, stats::qnorm(0.95))
    q95_shift <- lognormal_mean_centered_shifts(q95_L, stats::qnorm(0.975))
    q99_shift <- lognormal_mean_centered_shifts(q99_L, stats::qnorm(0.995))
    ts_lo_80 <- 10 * log10(sigma_pred * exp(-q80_shift$lower))
    ts_hi_80 <- 10 * log10(sigma_pred * exp(q80_shift$upper))
    ts_lo_90 <- 10 * log10(sigma_pred * exp(-q90_shift$lower))
    ts_hi_90 <- 10 * log10(sigma_pred * exp(q90_shift$upper))
    ts_lo_95 <- 10 * log10(sigma_pred * exp(-q95_shift$lower))
    ts_hi_95 <- 10 * log10(sigma_pred * exp(q95_shift$upper))
    ts_lo_99 <- 10 * log10(sigma_pred * exp(-q99_shift$lower))
    ts_hi_99 <- 10 * log10(sigma_pred * exp(q99_shift$upper))
  }
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
    support_modifier = shape_modifier,
    q80_L = q80_L,
    q90_L = q90_L,
    q95_L = q95_L,
    q99_L = q99_L,
    coefficient_covariance = Sigma,
    coefficient_support_n = coefficient_support_n,
    policy_selection_competitor_n = competition_support_n
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
                                          config = NULL,
                                          model_scores = NULL,
                                          species_lookup = NULL,
                                          policy_path = NULL,
                                          length_grid_n = 400L) {
  selected_tbl <- tibble::as_tibble(selected_tbl)
  candidate_models <- tibble::as_tibble(candidate_models)
  anchor_scores <- tibble::as_tibble(anchor_scores)
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
  purrr::map_dfr(seq_len(nrow(selected_tbl)), function(i) {
    ctx <- strategy_uncertainty_context(
      row_now = selected_tbl[i, , drop = FALSE],
      candidate_models = candidate_models,
      anchor_scores = anchor_scores,
      ts_calibration = ts_calibration,
      coefficient_calibration = coefficient_calibration,
      config = config,
      model_scores = model_scores,
      species_lookup = species_lookup,
      policy_lookup = policy_lookup,
      policy_path = policy_path,
      length_grid_n = length_grid_n
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
  summaries <- purrr::map_dfr(seq_len(nrow(policy_tbl)), function(i) {
    row_now <- policy_tbl[i, , drop = FALSE]
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
      length_grid_n = length_grid_n
    )
    if (is.null(ctx) || any(!is.finite(ctx$coefficient_covariance))) {
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
    Sigma <- ctx$coefficient_covariance
    z95 <- coefficient_interval_critical_value(ctx$coefficient_support_n %||% NA_real_)
    slope_hat <- suppressWarnings(as.numeric(row_now$policy_slope_len[[1]]))
    intercept_hat <- suppressWarnings(as.numeric(row_now$policy_intercept_len[[1]]))
    slope_se <- sqrt(max(Sigma[1, 1], 0))
    intercept_se <- sqrt(max(Sigma[2, 2], 0))
    corr <- if (slope_se > 0 && intercept_se > 0) Sigma[1, 2] / (slope_se * intercept_se) else NA_real_
    tibble::tibble(
      anchor_model_id = as.character(row_now$anchor_model_id[[1]]),
      policy = as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]]),
      equation_branch_filter = as.character(row_now$selected_equation_branch_filter[[1]] %||% row_now$equation_branch_filter[[1]]),
      policy_slope_len_se = slope_se,
      policy_intercept_len_se = intercept_se,
      policy_slope_len_lo_95 = slope_hat - z95 * slope_se,
      policy_slope_len_hi_95 = slope_hat + z95 * slope_se,
      policy_intercept_len_lo_95 = intercept_hat - z95 * intercept_se,
      policy_intercept_len_hi_95 = intercept_hat + z95 * intercept_se,
      policy_coefficient_covariance = Sigma[1, 2],
      policy_coefficient_correlation = corr,
      policy_coefficient_support_n = suppressWarnings(as.numeric(ctx$coefficient_support_n %||% NA_real_)),
      policy_coefficient_competitor_n = suppressWarnings(as.numeric(ctx$policy_selection_competitor_n %||% NA_real_))
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
  summaries <- purrr::map_dfr(seq_len(nrow(policy_tbl)), function(i) {
    row_now <- policy_tbl[i, , drop = FALSE]
    ctx <- strategy_uncertainty_context(
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
    if (is.null(ctx) || any(!is.finite(ctx$coefficient_covariance))) {
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
    Sigma <- ctx$coefficient_covariance
    z95 <- coefficient_interval_critical_value(ctx$coefficient_support_n %||% NA_real_)
    slope_hat <- suppressWarnings(as.numeric(row_now$policy_slope_len[[1]]))
    intercept_hat <- suppressWarnings(as.numeric(row_now$policy_intercept_len[[1]]))
    slope_se <- sqrt(max(Sigma[1, 1], 0))
    intercept_se <- sqrt(max(Sigma[2, 2], 0))
    corr <- if (slope_se > 0 && intercept_se > 0) Sigma[1, 2] / (slope_se * intercept_se) else NA_real_
    tibble::tibble(
      anchor_model_id = as.character(row_now$anchor_model_id[[1]]),
      policy = as.character(row_now$selected_policy[[1]] %||% row_now$policy[[1]]),
      equation_branch_filter = as.character(row_now$selected_equation_branch_filter[[1]] %||% row_now$equation_branch_filter[[1]]),
      conditional_policy_slope_len_se = slope_se,
      conditional_policy_intercept_len_se = intercept_se,
      conditional_policy_slope_len_lo_95 = slope_hat - z95 * slope_se,
      conditional_policy_slope_len_hi_95 = slope_hat + z95 * slope_se,
      conditional_policy_intercept_len_lo_95 = intercept_hat - z95 * intercept_se,
      conditional_policy_intercept_len_hi_95 = intercept_hat + z95 * intercept_se,
      conditional_policy_coefficient_covariance = Sigma[1, 2],
      conditional_policy_coefficient_correlation = corr,
      conditional_policy_coefficient_support_n = suppressWarnings(as.numeric(ctx$coefficient_support_n %||% NA_real_)),
      conditional_policy_coefficient_competitor_n = suppressWarnings(as.numeric(ctx$policy_selection_competitor_n %||% NA_real_))
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


