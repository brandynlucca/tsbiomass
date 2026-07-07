#' Add key-metadata missingness for a [Candidates] object
#'
#' @name screen_missing_metadata.Candidates
#' @usage NULL
#'
#' @keywords internal
#' @noRd
S7::method(screen_missing_metadata, Candidates) <- function(candidate_models,
                                                            key_cols = NULL) {
  # Reuse the candidate key columns before delegating to the tabular
  # default method.
  key_cols <- key_cols %||% admissibility_key_metadata_cols(candidates_config_data(candidate_models))

  screen_missing_metadata(
    candidate_models = candidate_models@candidate_models,
    key_cols = key_cols
  )
}

#' Add key-metadata missingness for a [PolicySelector] object
#'
#' @name screen_missing_metadata.PolicySelector
#' @usage NULL
#'
#' @keywords internal
#' @noRd
S7::method(screen_missing_metadata, PolicySelector) <- function(candidate_models,
                                                                key_cols = NULL) {
  # Pull the selector's candidate layer and apply the same missingness screen.
  key_cols <- key_cols %||% admissibility_key_metadata_cols(candidates_config_data(candidate_models@candidates))

  screen_missing_metadata(
    candidate_models = candidate_models@candidates,
    key_cols = key_cols
  )
}

#' Build a species-block coverage table
#'
#' @param coverage_summary Pseudo-anchor conformal summary list or a
#'   [PolicySelector] object.
#' @param ... Method-specific arguments.
#'
#' @return A tibble.
#'
#' @examples
#' \dontrun{
#' construct_species_coverage(selector)
#' }
#'
#' @export
construct_species_coverage <- S7::new_generic("construct_species_coverage", "coverage_summary")

#' Build a species-block coverage table from conformal summary lists
#'
#' @name construct_species_coverage.default
#' @usage NULL
#' @param coverage_summary Pseudo-anchor conformal summary list.
#' @param species_sum Species-block conformal summary list.
#' @param bench_label Species-block benchmark label.
S7::method(construct_species_coverage, S7::class_any) <- function(coverage_summary,
                                                                  species_sum,
                                                                  bench_label = "species_block") {
  # Bind the conformal coverage summaries across benchmark schemes, then keep
  # only the requested benchmark label.
  dplyr::bind_rows(
    tibble::as_tibble(coverage_summary$overall),
    tibble::as_tibble(species_sum$overall)
  ) |>
    dplyr::filter(.data$benchmark_label == bench_label) |>
    normalize_policy_columns() |>
    dplyr::select("policy", "equation_branch_filter", "empirical_coverage", "median_interval_log_width")
}

#' Build a species-block coverage table from a [PolicySelector]
#'
#' @name construct_species_coverage.PolicySelector
#' @usage NULL
S7::method(construct_species_coverage, PolicySelector) <- function(coverage_summary,
                                                                   species_sum = NULL,
                                                                   bench_label = "species_block") {
  # Pull the stored uncertainty bundle from the selector, then reuse the
  # default summary-list method.
  if (length(coverage_summary@uncertainty) == 0) {
    stop("No uncertainty calibration is stored on this `PolicySelector`.", call. = FALSE)
  }
  uncertainty_obj <- coverage_summary@uncertainty

  construct_species_coverage(
    coverage_summary = uncertainty_obj$pseudo_sum %||% list(overall = tibble::tibble()),
    species_sum = uncertainty_obj$species_sum %||% list(overall = tibble::tibble()),
    bench_label = bench_label
  )
}

#' Build the anchor support audit
#'
#' Joins selected-policy intervals to global benchmark and conformal coverage
#' summaries, and optionally appends sensitivity-drift diagnostics.
#'
#' @param policy_intervals Selected-policy interval table or a [PolicyPredictions]
#'   object.
#' @param ... Method-specific arguments.
#'
#' @return A tibble.
#'
#' @examples
#' \dontrun{
#' predictions <- predict(selector)
#' construct_anchor_audit(predictions, selector = selector)
#' }
#'
#' @export
construct_anchor_audit <- S7::new_generic("construct_anchor_audit", "policy_intervals")

#' Build an anchor support audit from selected-policy interval tables
#'
#' @name construct_anchor_audit.default
#' @usage NULL
#' @param policy_intervals Selected-policy interval table.
#' @param select_ref Policy-selection reference table.
#' @param cover_tbl Policy-level coverage summary table.
#' @param sens_detail Optional sensitivity-detail table.
#' @param sens_tbl Optional full policy-sensitivity table.
#' @param baseline_label Baseline scenario label.
#' @param selector Optional [PolicySelector] object used when
#'   `policy_intervals` is a
#'   [PolicyPredictions] object.
S7::method(construct_anchor_audit, S7::class_any) <- function(policy_intervals,
                                                              select_ref,
                                                              cover_tbl,
                                                              sens_detail = NULL,
                                                              sens_tbl = NULL,
                                                              baseline_label = "baseline",
                                                              selector = NULL) {
  # Start from the selected-policy rows and layer on the global benchmark and
  # conformal coverage summaries by selected policy.
  policy_intervals <- tibble::as_tibble(policy_intervals)
  policy_intervals$selected_policy <- resolve_selected_policy_values(policy_intervals)
  policy_intervals$selected_policy_display <- resolve_selected_policy_names(policy_intervals)
  policy_intervals$selected_equation_branch_filter <- resolve_selected_policy_branches(policy_intervals)
  policy_intervals$equivalent_policy_set <- resolve_equivalent_policy_sets(policy_intervals)

  out <- policy_intervals |>
    dplyr::select(dplyr::any_of(c(
      "anchor_model_id", "anchor_species", "selected_policy", "selected_policy_display",
      "selected_equation_branch_filter",
      "equivalent_policy_set", "equivalent_policy_set_n",
      "equivalence_class_id", "equivalence_class_size", "equivalence_class_members",
      "multiplier_pred", "multiplier_lo", "multiplier_hi", "interval_log_width",
      "valid_prediction", "prediction_error_stage", "prediction_error_code",
      "prediction_error_message",
      "local_support_mass", "local_effective_support", "local_mean_combined_distance",
      "local_mean_length_overlap", "local_mean_depth_overlap", "local_weighted_missingness"
    ))) |>
    dplyr::left_join(
      {
        select_ref_tbl <- normalize_policy_columns(select_ref)
        select_ref_tbl$policy <- resolve_policy_names(select_ref_tbl)
        for (nm in c(
          "mean_species_median_abs_log",
          "acceptable_global",
          "equivalent_to_best_global",
          "bootstrap_prob_within_threshold",
          "bootstrap_median_rank"
        )) {
          if (!nm %in% names(select_ref_tbl)) {
            select_ref_tbl[[nm]] <- NA
          }
        }
        select_ref_tbl
      } |>
        dplyr::select(
          "policy", "equation_branch_filter", "mean_species_median_abs_log", "acceptable_global", "equivalent_to_best_global",
          "bootstrap_prob_within_threshold", "bootstrap_median_rank"
        ),
      by = c(
        "selected_policy" = "policy",
        "selected_equation_branch_filter" = "equation_branch_filter"
      )
    ) |>
    dplyr::left_join(
      {
        cover_tbl <- normalize_policy_columns(cover_tbl)
        cover_tbl$policy <- resolve_policy_names(cover_tbl)
        cover_tbl
      },
      by = c(
        "selected_policy" = "policy",
        "selected_equation_branch_filter" = "equation_branch_filter"
      )
    )

  # When sensitivity summaries are available, append per-anchor scenario-change
  # counts and multiplier-drift summaries.
  if (!is.null(sens_detail) && !is.null(sens_tbl) &&
    nrow(sens_detail) > 0 && nrow(sens_tbl) > 0) {
    base_mult <- tibble::as_tibble(sens_tbl) |>
      dplyr::filter(.data$scenario == baseline_label) |>
      dplyr::select("anchor_model_id", baseline_multiplier = "multiplier_pred")

    change_tbl <- tibble::as_tibble(sens_detail) |>
      dplyr::group_by(.data$anchor_model_id, .data$anchor_species) |>
      dplyr::summarise(
        n_policy_changed = sum(.data$policy_changed, na.rm = TRUE),
        n_display_changed = sum(.data$display_changed, na.rm = TRUE),
        n_equiv_set_changed = sum(.data$equiv_set_changed, na.rm = TRUE),
        .groups = "drop"
      )

    drift_tbl <- tibble::as_tibble(sens_tbl) |>
      dplyr::filter(.data$scenario != baseline_label) |>
      dplyr::left_join(base_mult, by = "anchor_model_id") |>
      dplyr::group_by(.data$anchor_model_id, .data$anchor_species) |>
      dplyr::summarise(
        n_scenarios = dplyr::n(),
        median_abs_delta_log_multiplier = stats::median(abs(log(.data$multiplier_pred / .data$baseline_multiplier)), na.rm = TRUE),
        max_abs_delta_log_multiplier = max(abs(log(.data$multiplier_pred / .data$baseline_multiplier)), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::left_join(change_tbl, by = c("anchor_model_id", "anchor_species"))

    out <- out |>
      dplyr::left_join(drift_tbl, by = c("anchor_model_id", "anchor_species"))
  }

  out
}

#' Build an anchor support audit from a [PolicyPredictions] object
#'
#' @name construct_anchor_audit.PolicyPredictions
#' @usage NULL
S7::method(construct_anchor_audit, PolicyPredictions) <- function(policy_intervals,
                                                                  select_ref = NULL,
                                                                  cover_tbl = NULL,
                                                                  sens_detail = NULL,
                                                                  sens_tbl = NULL,
                                                                  baseline_label = "baseline",
                                                                  selector = NULL) {
  if (is_s7_instance(select_ref, "PolicySelector") && is.null(selector)) {
    selector <- select_ref
    select_ref <- NULL
  }
  selector <- selector %||% select_ref
  if (!is_s7_instance(selector, "PolicySelector")) {
    stop(
      "When `policy_intervals` is a `PolicyPredictions` object, `selector` must be a `PolicySelector` object.",
      call. = FALSE
    )
  }

  # Pull the global selection summaries and policy-level coverage directly from
  # the selector unless the caller supplied explicit replacements.
  select_ref <- select_ref %||% (selector@selection)$final_ref %||% tibble::tibble()
  cover_tbl <- cover_tbl %||% construct_species_coverage(selector)

  construct_anchor_audit(
    policy_intervals = policy_intervals@selections,
    select_ref = select_ref,
    cover_tbl = cover_tbl,
    sens_detail = sens_detail,
    sens_tbl = sens_tbl,
    baseline_label = baseline_label
  )
}

#' Summarize key-field missingness
#'
#' Computes overall, field-level, and model-level key-field missingness from a
#' candidate-model table that already contains `key_metadata_missing_fraction`.
#'
#' @param candidate_models Candidate-model table, [Candidates] object, or
#'   [PolicySelector] object.
#' @param ... Method-specific arguments.
#'
#' @return A list of tibbles.
#'
#' @keywords internal
#' @noRd
summarize_key_missing <- S7::new_generic("summarize_key_missing", "candidate_models")

#' Summarize key-field missingness for candidate-model tables
#'
#' @name summarize_key_missing.default
#' @usage NULL
#' @param candidate_models Candidate-model table.
#' @param key_cols Character vector of key-field names.
#' @param threshold Missingness threshold.
#' @param model_id_col Model identifier column.
#' @param species_col Species label column.
#' @param common_col Optional common-name column.
#' @keywords internal
#' @noRd
S7::method(summarize_key_missing, S7::class_any) <- function(candidate_models,
                                                             key_cols,
                                                             threshold,
                                                             model_id_col = "model_id",
                                                             species_col = "species_name",
                                                             common_col = "common") {
  # Summarize key-field missingness at the overall, per-field, and per-model
  # levels from the already prepared candidate table.
  models_tbl <- tibble::as_tibble(candidate_models)
  key_cols <- intersect(as.character(key_cols), names(models_tbl))

  overall <- tibble::tibble(
    n_models = nrow(models_tbl),
    missing_key_metadata_max_fraction = threshold,
    n_models_exceeding_threshold = sum(models_tbl$key_metadata_missing_fraction > threshold, na.rm = TRUE),
    prop_models_exceeding_threshold = mean(models_tbl$key_metadata_missing_fraction > threshold, na.rm = TRUE),
    median_key_metadata_missing_fraction = stats::median(models_tbl$key_metadata_missing_fraction, na.rm = TRUE),
    max_key_metadata_missing_fraction = {
      v <- models_tbl$key_metadata_missing_fraction
      if (any(!is.na(v))) max(v, na.rm = TRUE) else NA_real_
    }
  )

  by_field <- tibble::tibble(
    field = key_cols,
    missing_n = purrr::map_int(key_cols, ~ sum(is.na(models_tbl[[.x]]))),
    nonmissing_n = purrr::map_int(key_cols, ~ sum(!is.na(models_tbl[[.x]]))),
    missing_fraction = purrr::map_dbl(key_cols, ~ mean(is.na(models_tbl[[.x]])))
  ) |>
    dplyr::arrange(dplyr::desc(.data$missing_fraction), .data$field)

  by_model <- models_tbl |>
    dplyr::select(
      dplyr::all_of(model_id_col),
      dplyr::all_of(species_col),
      dplyr::any_of(common_col),
      "key_metadata_missing_fraction"
    ) |>
    dplyr::arrange(dplyr::desc(.data$key_metadata_missing_fraction), .data[[species_col]], .data[[model_id_col]])

  list(overall = overall, by_field = by_field, by_model = by_model)
}

#' Summarize key-field missingness for a [Candidates] object
#'
#' @name summarize_key_missing.Candidates
#' @usage NULL
#' @keywords internal
#' @noRd
S7::method(summarize_key_missing, Candidates) <- function(candidate_models,
                                                          key_cols = NULL,
                                                          threshold = NA_real_,
                                                          model_id_col = "model_id",
                                                          species_col = "species_name",
                                                          common_col = "common") {
  # Materialize the keyed missingness screen first, then summarize that
  # prepared table through the default method.
  key_cols <- key_cols %||% candidates_similarity_key_cols(candidate_models)
  models_tbl <- screen_missing_metadata(
    candidate_models = candidate_models,
    key_cols = key_cols
  )

  summarize_key_missing(
    candidate_models = models_tbl,
    key_cols = key_cols,
    threshold = threshold,
    model_id_col = model_id_col,
    species_col = species_col,
    common_col = common_col
  )
}

#' Summarize key-field missingness for a [PolicySelector] object
#'
#' @name summarize_key_missing.PolicySelector
#' @usage NULL
#' @keywords internal
#' @noRd
S7::method(summarize_key_missing, PolicySelector) <- function(candidate_models,
                                                              key_cols = NULL,
                                                              threshold = NULL,
                                                              model_id_col = "model_id",
                                                              species_col = "species_name",
                                                              common_col = "common") {
  key_cols <- key_cols %||% candidates_similarity_key_cols(candidate_models@candidates)
  threshold <- threshold %||% policy_selector_anchor_config(candidate_models)$missing_key_metadata_max_fraction %||% NA_real_

  summarize_key_missing(
    candidate_models = candidate_models@candidates,
    key_cols = key_cols,
    threshold = threshold,
    model_id_col = model_id_col,
    species_col = species_col,
    common_col = common_col
  )
}

#' Summarize missingness gate outcomes
#'
#' @param adm_tbl Admissibility summary table.
#' @param ... Method-specific arguments.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
summarize_missing_gate <- S7::new_generic("summarize_missing_gate", "adm_tbl")

#' Summarize missingness gate outcomes for admissibility summary tables
#'
#' @name summarize_missing_gate.default
#' @usage NULL
#' @param adm_tbl Admissibility summary table.
#' @keywords internal
#' @noRd
S7::method(summarize_missing_gate, S7::class_any) <- function(adm_tbl) {
  # Reduce candidate-level or anchor-level admissibility results to the
  # missingness-gate view used in later reporting.
  out <- tibble::as_tibble(adm_tbl)

  if (nrow(out) == 0) {
    stop(
      "Missing-gate summarization requires a non-empty admissibility table.",
      call. = FALSE
    )
  }

  if (all(c("anchor_model_id", "anchor_species", "admissible", "gate_missing_key_metadata") %in% names(out))) {
    return(
      out |>
        dplyr::group_by(.data$anchor_model_id, .data$anchor_species) |>
        dplyr::summarise(
          n_candidates_total = dplyr::n(),
          n_candidates_admissible = sum(dplyr::coalesce(as.logical(.data$admissible), FALSE), na.rm = TRUE),
          prop_fail_missing_metadata = mean(!dplyr::coalesce(as.logical(.data$gate_missing_key_metadata), FALSE), na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(.data$anchor_species)
    )
  }

  # Support both the older anchor-level gate summary and the current
  # admissible-pool summary emitted by `screen_admissibility()`.
  if (all(c("n_candidates_total", "prop_admissible", "prop_fail_missing_metadata") %in% names(out))) {
    return(
      out |>
        dplyr::transmute(
          .data$anchor_model_id,
          .data$anchor_species,
          .data$n_candidates_total,
          n_candidates_admissible = round(.data$n_candidates_total * .data$prop_admissible),
          .data$prop_fail_missing_metadata
        ) |>
        dplyr::arrange(.data$anchor_species)
    )
  }

  if (all(c("anchor_model_id", "anchor_species", "n_admissible") %in% names(out))) {
    return(
      out |>
        dplyr::transmute(
          .data$anchor_model_id,
          .data$anchor_species,
          n_candidates_total = NA_real_,
          n_candidates_admissible = dplyr::coalesce(as.numeric(.data$n_admissible), 0),
          prop_fail_missing_metadata = NA_real_
        ) |>
        dplyr::arrange(.data$anchor_species)
    )
  }

  stop(
    paste0(
      "Missing-gate summarization requires one of the supported admissibility schemas. ",
      "Received columns: ",
      paste(names(out), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' Summarize missingness gate outcomes for a [Candidates] object
#'
#' @name summarize_missing_gate.Candidates
#' @usage NULL
#' @keywords internal
#' @noRd
S7::method(summarize_missing_gate, Candidates) <- function(adm_tbl) {
  scores_tbl <- if (!is_s7_instance(adm_tbl, "Candidates") ||
    length(adm_tbl@admissibility) == 0) {
    tibble::tibble()
  } else {
    tibble::as_tibble((adm_tbl@admissibility)$all_scores %||% tibble::tibble())
  }
  if (nrow(scores_tbl) == 0) {
    stop(
      "Candidates do not carry stored admissibility scores for missing-gate summarization.",
      call. = FALSE
    )
  }
  summarize_missing_gate(scores_tbl)
}

#' Summarize missingness gate outcomes for a [PolicySelector] object
#'
#' @name summarize_missing_gate.PolicySelector
#' @usage NULL
#' @keywords internal
#' @noRd
S7::method(summarize_missing_gate, PolicySelector) <- function(adm_tbl) {
  summarize_missing_gate(adm_tbl@candidates)
}

#' Bind uncertainty tables
#'
#' Binds anchor-level uncertainty-context and uncertainty-dropout rows across
#' anchors.
#'
#' @param ctx_rows List of uncertainty-context tables.
#' @param drop_rows List of uncertainty-dropout tables.
#'
#' @return A list with `context` and `dropout`.
#'
#' @keywords internal
#' @noRd
bind_uncertainty_rows <- function(ctx_rows,
                                  drop_rows) {
  # Bind the accumulated uncertainty context and dropout rows separately so
  # both tables remain available downstream.
  list(
    context = dplyr::bind_rows(ctx_rows),
    dropout = dplyr::bind_rows(drop_rows)
  )
}

#' Summarize slope dependence
#'
#' Builds the slope-dependence tables used in the paper script without
#' writing any files. The summaries are based on non-group equations and are
#' collapsed to the study-cell level before group-level comparisons are made.
#'
#' @param candidate_models Candidate-model table.
#' @param slope_col Column holding standardized TS-length slope values.
#' @param intercept_col Column holding standardized TS-length intercept values.
#' @param group_col Column flagging generalized/group models.
#' @param eq_col Column holding equation-form codes.
#' @param eq_type_col Column holding equation-form-type labels.
#' @param study_cell_col Column holding study-cell identifiers.
#' @param study_ref_col Column holding study-reference identifiers.
#' @param species_col Column holding species labels.
#' @param family_col Column holding family labels.
#' @param genus_col Column holding genus labels.
#' @param tax_genus_col Optional taxonomy-derived genus column.
#' @param tags_col Column holding tag annotations.
#' @param freq_col Column holding frequency labels.
#'
#' @return A list of slope-summary tibbles.
#'
#' @keywords internal
#' @noRd
summarize_slope_effect <- function(candidate_models,
                                   slope_col = "slope_len",
                                   intercept_col = "intercept_len",
                                   group_col = "is_group_model",
                                   eq_col = "equation_form",
                                   eq_type_col = "equation_form_type",
                                   study_cell_col = "study_cell_id",
                                   study_ref_col = "citation",
                                   species_col = "species_name",
                                   family_col = "family",
                                   genus_col = "genus",
                                   tax_genus_col = "tax_genus",
                                   tags_col = "tags",
                                   freq_col = "freq_label") {
  # Validate the key columns up front so downstream summaries fail with one
  # clear message rather than a chain of subsetting errors.
  models_tbl <- tibble::as_tibble(candidate_models)
  need_cols <- c(
    slope_col, intercept_col, eq_col, eq_type_col, study_cell_col,
    study_ref_col, species_col, family_col, genus_col, freq_col
  )
  missing_cols <- setdiff(need_cols, names(models_tbl))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required slope-summary column(s): %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Derive the generalized-model flag when the prepared model table does not
  # already carry one.
  if (!group_col %in% names(models_tbl)) {
    generic_labels <- c(
      "physoclists", "physostomes", "clupeoids", "clupeids",
      "general/mixed", "general", "mixed clupeids and smelts"
    )

    genus_chr <- stringr::str_squish(stringr::str_to_lower(dplyr::coalesce(as.character(models_tbl[[genus_col]]), "")))
    species_chr <- stringr::str_squish(stringr::str_to_lower(dplyr::coalesce(as.character(models_tbl[[species_col]]), "")))

    tags_chr <- if (tags_col %in% names(models_tbl)) {
      stringr::str_squish(stringr::str_to_lower(dplyr::coalesce(as.character(models_tbl[[tags_col]]), "")))
    } else {
      rep("", nrow(models_tbl))
    }

    swim_chr <- if ("swimbladder_type" %in% names(models_tbl)) {
      stringr::str_squish(stringr::str_to_lower(dplyr::coalesce(as.character(models_tbl$swimbladder_type), "")))
    } else {
      rep("", nrow(models_tbl))
    }

    has_species_identity <- nzchar(genus_chr) & nzchar(species_chr) & !species_chr %in% c("na", "na na")
    models_tbl[[group_col]] <- (species_chr %in% c("", "na", "na na")) |
      (!has_species_identity & tags_chr %in% generic_labels) |
      (!has_species_identity & swim_chr == "general/nonspecific")
  }

  # Restrict the analysis to usable non-group equations before classifying the
  # focal review groups and slope-support categories.
  slope_models <- models_tbl |>
    # Normalize the taxonomy strings on the full candidate table first so the
    # filtered slope subset inherits row-aligned classifier inputs.
    dplyr::mutate(
      review_genus_chr = stringr::str_to_lower(
        dplyr::coalesce(
          if (tax_genus_col %in% names(models_tbl)) as.character(.data[[tax_genus_col]]) else NA_character_,
          as.character(.data[[genus_col]]),
          ""
        )
      ),
      review_family_chr = stringr::str_to_lower(dplyr::coalesce(as.character(.data[[family_col]]), "")),
      review_tags_chr = if (tags_col %in% names(models_tbl)) stringr::str_to_lower(dplyr::coalesce(as.character(.data[[tags_col]]), "")) else ""
    ) |>
    dplyr::filter(
      !.data[[group_col]],
      is.finite(.data[[slope_col]]),
      is.finite(.data[[intercept_col]])
    ) |>
    dplyr::mutate(
      review_group = dplyr::case_when(
        review_tags_chr %in% c("scomber", "trachurus") ~ "Mackerel",
        review_tags_chr == "anchovies" ~ "Anchovies",
        review_tags_chr == "herrings" ~ "Herrings",
        review_tags_chr == "smelts" ~ "Smelts",
        review_tags_chr == "sardines" ~ "Sardines",
        review_family_chr %in% "engraulidae" ~ "Anchovies",
        review_family_chr %in% "osmeridae" |
          review_genus_chr %in% c("mallotus", "allosmerus", "hypomesus", "osmerus", "spirinchus", "thaleichthys") |
          review_tags_chr == "smelts" ~ "Smelts",
        review_genus_chr %in% c("sardinops", "sardinella", "sardina", "sprattus") |
          review_tags_chr == "sardines" ~ "Sardines",
        review_genus_chr %in% c("clupea", "alosa", "clupeonella") |
          review_tags_chr == "herrings" ~ "Herrings",
        review_genus_chr %in% c("scomber", "trachurus") ~ "Mackerel",
        TRUE ~ "Other"
      ),
      review_group = factor(
        .data$review_group,
        levels = c("Sardines", "Anchovies", "Herrings", "Smelts", "Mackerel", "Other")
      ),
      slope_deviation_from_20 = .data[[slope_col]] - 20,
      original_reference_class = dplyr::case_when(
        .data[[eq_col]] == "mlog10_kg" ~ "weight-referenced",
        !is.finite(slope_deviation_from_20) ~ NA_character_,
        abs(slope_deviation_from_20) < 1e-8 ~ "exactly 20",
        slope_deviation_from_20 < -2 ~ "< -2",
        slope_deviation_from_20 >= -2 & slope_deviation_from_20 < -1 ~ "-2 to -1",
        slope_deviation_from_20 >= -1 & slope_deviation_from_20 < 0 ~ "-1 to 0",
        slope_deviation_from_20 > 0 & slope_deviation_from_20 <= 1 ~ "0 to 1",
        slope_deviation_from_20 > 1 & slope_deviation_from_20 <= 2 ~ "1 to 2",
        slope_deviation_from_20 > 2 ~ "> 2",
        TRUE ~ NA_character_
      ),
      original_reference_class = factor(
        .data$original_reference_class,
        levels = c("< -2", "-2 to -1", "-1 to 0", "exactly 20", "0 to 1", "1 to 2", "> 2", "weight-referenced")
      ),
      slope_deviation_class = dplyr::case_when(
        !is.finite(slope_deviation_from_20) ~ NA_character_,
        abs(slope_deviation_from_20) < 1e-8 ~ "exactly 20",
        slope_deviation_from_20 < -2 ~ "< -2",
        slope_deviation_from_20 >= -2 & slope_deviation_from_20 < -1 ~ "-2 to -1",
        slope_deviation_from_20 >= -1 & slope_deviation_from_20 < 0 ~ "-1 to 0",
        slope_deviation_from_20 > 0 & slope_deviation_from_20 <= 1 ~ "0 to 1",
        slope_deviation_from_20 > 1 & slope_deviation_from_20 <= 2 ~ "1 to 2",
        slope_deviation_from_20 > 2 ~ "> 2",
        TRUE ~ NA_character_
      ),
      slope_deviation_class = factor(
        .data$slope_deviation_class,
        levels = c("< -2", "-2 to -1", "-1 to 0", "exactly 20", "0 to 1", "1 to 2", "> 2")
      ),
      slope_support_class = dplyr::case_when(
        .data[[eq_type_col]] == "fixed_slope" | abs(.data[[slope_col]] - 20) < 1e-8 ~ "Exact 20",
        .data[[slope_col]] >= 18 & .data[[slope_col]] <= 22 ~ "Near 20 (18-22)",
        .data[[slope_col]] < 18 ~ "Below 18",
        .data[[slope_col]] > 22 ~ "Above 22",
        TRUE ~ "Other"
      )
    )

  # Collapse repeated variants from the same study cell so papers with many
  # alternative fits do not dominate the slope-support summaries.
  study_cell_level <- slope_models |>
    dplyr::group_by(
      .data[[study_cell_col]], .data[[study_ref_col]], .data[[species_col]],
      .data$review_group, .data[[family_col]], .data[[genus_col]]
    ) |>
    dplyr::summarise(
      slope_len_cell = stats::median(.data[[slope_col]], na.rm = TRUE),
      intercept_len_cell = stats::median(.data[[intercept_col]], na.rm = TRUE),
      n_model_variants = dplyr::n(),
      n_equation_forms = dplyr::n_distinct(.data[[eq_type_col]], na.rm = TRUE),
      n_frequencies = dplyr::n_distinct(.data[[freq_col]], na.rm = TRUE),
      any_fixed_20 = any(.data[[eq_type_col]] == "fixed_slope", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      slope_deviation_from_20 = .data$slope_len_cell - 20,
      slope_deviation_class = dplyr::case_when(
        !is.finite(slope_deviation_from_20) ~ NA_character_,
        abs(slope_deviation_from_20) < 1e-8 ~ "exactly 20",
        slope_deviation_from_20 < -2 ~ "< -2",
        slope_deviation_from_20 >= -2 & slope_deviation_from_20 < -1 ~ "-2 to -1",
        slope_deviation_from_20 >= -1 & slope_deviation_from_20 < 0 ~ "-1 to 0",
        slope_deviation_from_20 > 0 & slope_deviation_from_20 <= 1 ~ "0 to 1",
        slope_deviation_from_20 > 1 & slope_deviation_from_20 <= 2 ~ "1 to 2",
        slope_deviation_from_20 > 2 ~ "> 2",
        TRUE ~ NA_character_
      ),
      slope_deviation_class = factor(
        .data$slope_deviation_class,
        levels = c("< -2", "-2 to -1", "-1 to 0", "exactly 20", "0 to 1", "1 to 2", "> 2")
      ),
      slope_support_class = dplyr::case_when(
        any_fixed_20 | abs(.data$slope_len_cell - 20) < 1e-8 ~ "Exact 20",
        slope_len_cell >= 18 & .data$slope_len_cell <= 22 ~ "Near 20 (18-22)",
        slope_len_cell < 18 ~ "Below 18",
        slope_len_cell > 22 ~ "Above 22",
        TRUE ~ "Other"
      )
    )

  # Summarize the study-cell slopes overall and by review group for later
  # reporting and plotting.
  group_summary <- study_cell_level |>
    dplyr::group_by(.data$review_group) |>
    dplyr::summarise(
      n_study_cells = dplyr::n(),
      n_species = dplyr::n_distinct(.data[[species_col]]),
      median_slope = stats::median(.data$slope_len_cell, na.rm = TRUE),
      mean_slope = mean(.data$slope_len_cell, na.rm = TRUE),
      q25_slope = stats::quantile(.data$slope_len_cell, 0.25, na.rm = TRUE, names = FALSE, type = 8),
      q75_slope = stats::quantile(.data$slope_len_cell, 0.75, na.rm = TRUE, names = FALSE, type = 8),
      prop_exact_20 = mean(.data$slope_support_class == "Exact 20", na.rm = TRUE),
      prop_near_20 = mean(.data$slope_len_cell >= 18 & .data$slope_len_cell <= 22, na.rm = TRUE),
      prop_above_22 = mean(.data$slope_len_cell > 22, na.rm = TRUE),
      prop_below_18 = mean(.data$slope_len_cell < 18, na.rm = TRUE),
      mean_equation_variants_per_cell = mean(.data$n_equation_forms, na.rm = TRUE),
      mean_model_variants_per_cell = mean(.data$n_model_variants, na.rm = TRUE),
      .groups = "drop"
    )

  overall_summary <- tibble::tibble(
    n_models = nrow(slope_models),
    n_study_cells = nrow(study_cell_level),
    n_species = dplyr::n_distinct(study_cell_level[[species_col]]),
    median_slope_model = stats::median(slope_models[[slope_col]], na.rm = TRUE),
    median_slope_study_cell = stats::median(study_cell_level$slope_len_cell, na.rm = TRUE),
    prop_exact_20_study_cell = mean(study_cell_level$slope_support_class == "Exact 20", na.rm = TRUE),
    prop_near_20_study_cell = mean(study_cell_level$slope_len_cell >= 18 & study_cell_level$slope_len_cell <= 22, na.rm = TRUE)
  )

  # Build the support tables used in the stacked support plot and in the
  # weighted deviation analysis.
  support_by_group <- study_cell_level |>
    dplyr::count(.data$review_group, .data$slope_support_class, name = "n_study_cells") |>
    dplyr::group_by(.data$review_group) |>
    dplyr::mutate(prop_study_cells = .data$n_study_cells / sum(.data$n_study_cells)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      slope_support_class = factor(
        .data$slope_support_class,
        levels = c("Below 18", "Near 20 (18-22)", "Exact 20", "Above 22", "Other")
      )
    )

  deviation_support_by_group <- slope_models |>
    dplyr::group_by(.data[[study_cell_col]]) |>
    dplyr::mutate(study_cell_variant_weight = 1 / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$original_reference_class)) |>
    dplyr::group_by(.data$review_group, .data$original_reference_class) |>
    dplyr::summarise(
      weighted_study_cell_count = sum(.data$study_cell_variant_weight, na.rm = TRUE),
      n_model_rows = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$review_group) |>
    dplyr::mutate(prop_study_cells = .data$weighted_study_cell_count / sum(.data$weighted_study_cell_count)) |>
    dplyr::ungroup()

  list(
    slope_models = slope_models,
    study_cell_level = study_cell_level,
    group_summary = group_summary,
    overall_summary = overall_summary,
    support_by_group = support_by_group,
    deviation_support_by_group = deviation_support_by_group
  )
}

#' Summarize FAO-area study counts
#'
#' Counts distinct studies by FAO area for later map and inset plotting.
#'
#' @param candidate_models Candidate-model table.
#' @param fao_col Column holding FAO-area codes.
#' @param study_col Column holding study identifiers.
#'
#' @return A tibble of study counts by FAO area.
#'
#' @keywords internal
#' @noRd
summarize_area_studies <- function(candidate_models,
                                   fao_col = "fao_area",
                                   study_col = "citation") {
  # Count distinct studies by FAO area after normalizing the codes to strings
  # so mixed numeric/character inputs do not split the same area.
  models_tbl <- tibble::as_tibble(candidate_models)
  if (!all(c(fao_col, study_col) %in% names(models_tbl))) {
    stop(
      sprintf(
        "Missing required FAO-summary column(s): %s",
        paste(setdiff(c(fao_col, study_col), names(models_tbl)), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  models_tbl |>
    dplyr::filter(!is.na(.data[[fao_col]]), as.character(.data[[fao_col]]) != "") |>
    dplyr::mutate(fao_area_chr = as.character(.data[[fao_col]])) |>
    dplyr::distinct(.data$fao_area_chr, .data[[.data$study_col]]) |>
    dplyr::count(.data$fao_area_chr, name = "n_studies") |>
    dplyr::arrange(dplyr::desc(.data$n_studies), .data$fao_area_chr)
}


#' Compute weighted quantiles
#'
#' @param x Numeric values.
#' @param w Numeric weights.
#' @param probs Numeric probability vector.
#'
#' @return Numeric vector.
#'
#' @keywords internal
#' @noRd
weighted_quantile <- function(x,
                              w,
                              probs = c(0.05, 0.50, 0.95)) {
  # Restrict the calculation to finite values with finite non-negative weights
  # before building the weighted empirical distribution.
  keep <- is.finite(x) & is.finite(w) & w >= 0
  x <- x[keep]
  w <- w[keep]

  if (length(x) == 0 || sum(w) <= 0) {
    return(rep(NA_real_, length(probs)))
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord] / sum(w)
  cw <- cumsum(w)

  # Interpolate each requested quantile on the weighted cumulative
  # distribution.
  vapply(probs, function(p) {
    idx <- which(cw >= p)[1]
    if (is.na(idx)) {
      return(x[[length(x)]])
    }
    x[[idx]]
  }, numeric(1))
}

#' Predict TS values on a shared length grid
#'
#' Builds a length-by-model matrix of TS values from the standardized slope and
#' intercept columns.
#'
#' @param models_df Model table.
#' @param lengths_cm Numeric length grid in cm.
#' @param slope_col Slope column name.
#' @param intercept_col Intercept column name.
#' @param id_col Model identifier column name.
#'
#' @return A numeric matrix with one column per model.
#'
#' @keywords internal
#' @noRd
predict_ts_matrix <- function(models_df,
                              lengths_cm,
                              slope_col = "slope_len",
                              intercept_col = "intercept_len",
                              id_col = "model_id") {
  # Evaluate each model on the shared length grid so downstream ribbon and
  # pivot summaries can work from one aligned TS matrix.
  mat <- vapply(
    seq_len(nrow(models_df)),
    function(i) {
      as.numeric(models_df[[slope_col]][[i]]) * log10(lengths_cm) +
        as.numeric(models_df[[intercept_col]][[i]])
    },
    numeric(length(lengths_cm))
  )

  # Keep the return value matrix-shaped even when only one model was supplied.
  if (is.null(dim(mat))) {
    mat <- matrix(mat, ncol = 1)
  }

  colnames(mat) <- as.character(models_df[[id_col]])
  mat
}

#' Summarize a weighted TS ribbon
#'
#' Combines model-specific TS curves into a weighted mean and normal-approximate
#' uncertainty band on a shared length grid.
#'
#' @param models_df Model table.
#' @param model_weights Numeric weight vector aligned to `models_df`.
#' @param lengths_cm Numeric length grid in cm.
#' @param slope_col Slope column name.
#' @param intercept_col Intercept column name.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
summarize_weighted_ts_curve <- function(models_df,
                                        model_weights,
                                        lengths_cm,
                                        slope_col = "slope_len",
                                        intercept_col = "intercept_len") {
  # Restrict the ribbon summary to models with finite coefficients and finite
  # weights before computing the weighted slope/intercept moments.
  valid <- which(
    is.finite(models_df[[slope_col]]) &
      is.finite(models_df[[intercept_col]]) &
      is.finite(model_weights)
  )

  if (length(valid) == 0) {
    return(tibble::tibble(
      length_cm = lengths_cm,
      ts_mean = NA_real_,
      ts_sd = NA_real_,
      ts_lo = NA_real_,
      ts_hi = NA_real_,
      band_method = "weighted_moment_normal_90",
      z_band = 1.645
    ))
  }

  slopes <- as.numeric(models_df[[slope_col]][valid])
  intercepts <- as.numeric(models_df[[intercept_col]][valid])
  w <- as.numeric(model_weights[valid])
  w_sum <- sum(w, na.rm = TRUE)
  if (!is.finite(w_sum) || w_sum <= 0) {
    stop("'model_weights' must sum to a finite positive value.", call. = FALSE)
  }
  w <- w / w_sum

  # Propagate the weighted slope/intercept moments over the log-length grid to
  # obtain the ribbon mean and variance at each length.
  mu_a <- sum(w * slopes)
  mu_b <- sum(w * intercepts)
  var_a <- sum(w * (slopes - mu_a)^2)
  var_b <- sum(w * (intercepts - mu_b)^2)
  cov_ab <- sum(w * (slopes - mu_a) * (intercepts - mu_b))

  x <- log10(lengths_cm)
  ts_mean <- mu_a * x + mu_b
  ts_var <- pmax(var_a * x^2 + 2 * cov_ab * x + var_b, 0)
  ts_sd <- sqrt(ts_var)
  z_band <- 1.645

  tibble::tibble(
    length_cm = lengths_cm,
    ts_mean = ts_mean,
    ts_sd = ts_sd,
    ts_lo = ts_mean - z_band * ts_sd,
    ts_hi = ts_mean + z_band * ts_sd,
    band_method = "weighted_moment_normal_90",
    z_band = z_band
  )
}

#' Compute a pivot profile
#'
#' Identifies the length where weighted between-model TS variance is minimized
#' and summarizes pairwise model intersection lengths over the same grid.
#'
#' @param models_df Model table.
#' @param model_weights Numeric model-weight vector keyed by model ID.
#' @param length_density Length-density tibble with `length_cm`.
#' @param slope_col Slope column name.
#' @param intercept_col Intercept column name.
#' @param id_col Model identifier column name.
#' @param species_col Species label column name.
#'
#' @return A list with `profile`, `pairwise`, and `summary`.
#'
#' @keywords internal
#' @noRd
compute_pivot_profile <- function(models_df,
                                  model_weights,
                                  length_density,
                                  slope_col = "slope_len",
                                  intercept_col = "intercept_len",
                                  id_col = "model_id",
                                  species_col = "species_name") {
  # Extract the shared length grid and align the model weights to the supplied
  # model rows before any variance or pairwise calculations begin.
  length_grid <- length_density$length_cm
  grid_min <- min(length_grid, na.rm = TRUE)
  grid_max <- max(length_grid, na.rm = TRUE)
  grid_span <- grid_max - grid_min
  w <- as.numeric(model_weights[as.character(models_df[[id_col]])])
  w[!is.finite(w)] <- 0

  if (sum(w) <= 0) {
    stop("No positive model weights available for pivot analysis.", call. = FALSE)
  }

  # Compute the weighted TS mean and between-model variance profile over the
  # shared grid, then identify the minimum-variance pivot length.
  ts_mat <- predict_ts_matrix(
    models_df = models_df,
    lengths_cm = length_grid,
    slope_col = slope_col,
    intercept_col = intercept_col,
    id_col = id_col
  )
  w_norm <- w / sum(w)
  ts_weighted_mean <- as.numeric(ts_mat %*% w_norm)
  ts_centered <- sweep(ts_mat, 1, ts_weighted_mean, FUN = "-")
  variance_v <- rowSums(sweep(ts_centered^2, 2, w_norm, FUN = "*"), na.rm = TRUE)

  pivot_idx <- which.min(variance_v)
  pivot_length_cm <- length_grid[pivot_idx]

  # Derive pairwise intersection lengths so boundary pivots can still be
  # diagnosed with an interior pairwise median and IQR.
  valid_rows <- which(is.finite(models_df[[slope_col]]) & is.finite(models_df[[intercept_col]]))
  pairwise <- purrr::map_dfr(utils::combn(valid_rows, 2, simplify = FALSE), function(idx) {
    i <- idx[[1]]
    j <- idx[[2]]
    m1 <- models_df[[slope_col]][i]
    m2 <- models_df[[slope_col]][j]
    b1 <- models_df[[intercept_col]][i]
    b2 <- models_df[[intercept_col]][j]

    if (!is.finite(m1) || !is.finite(m2) || !is.finite(b1) || !is.finite(b2) || abs(m1 - m2) < 1e-8) {
      return(NULL)
    }

    log10_lpivot <- (b2 - b1) / (m1 - m2)
    lpivot_cm <- 10^log10_lpivot
    if (!is.finite(lpivot_cm) || lpivot_cm <= 0) {
      return(NULL)
    }

    tibble::tibble(
      model_id_1 = models_df[[id_col]][i],
      model_id_2 = models_df[[id_col]][j],
      species_1 = models_df[[species_col]][i],
      species_2 = models_df[[species_col]][j],
      lpivot_cm = lpivot_cm,
      log10_lpivot = log10_lpivot,
      pair_weight = w_norm[i] * w_norm[j]
    )
  })

  pairwise <- pairwise |>
    dplyr::filter(
      is.finite(.data$lpivot_cm),
      is.finite(.data$pair_weight),
      .data$pair_weight > 0
    )

  # Clamp the raw pairwise pivots to a biologically plausible window around
  # the anchor domain before computing their weighted summaries.
  domain_lo <- max(grid_min * 0.5, 1e-6)
  domain_hi <- max(grid_max * 2, domain_lo * 1.01)
  if (nrow(pairwise) > 0) {
    pairwise <- pairwise |>
      dplyr::mutate(
        lpivot_raw_cm = .data$lpivot_cm,
        lpivot_cm = pmin(pmax(.data$lpivot_cm, domain_lo), domain_hi),
        log10_lpivot = log10(.data$lpivot_cm)
      ) |>
      dplyr::filter(is.finite(.data$lpivot_cm), .data$lpivot_cm > 0)
  }

  pairwise_q25 <- NA_real_
  pairwise_q50 <- NA_real_
  pairwise_q75 <- NA_real_
  if (nrow(pairwise) > 0) {
    pairwise_q <- weighted_quantile(
      x = pairwise$lpivot_cm,
      w = pairwise$pair_weight,
      probs = c(0.25, 0.50, 0.75)
    )
    pairwise_q25 <- pairwise_q[[1]]
    pairwise_q50 <- pairwise_q[[2]]
    pairwise_q75 <- pairwise_q[[3]]
  }

  # Mark boundary pivots explicitly and expose the display pivot that should be
  # used in later summaries and plots.
  edge_tol_cm <- max(grid_span * 0.025, 0.5)
  pivot_at_boundary <- isTRUE(
    is.finite(pivot_length_cm) &&
      (pivot_length_cm <= grid_min + edge_tol_cm || pivot_length_cm >= grid_max - edge_tol_cm)
  )
  display_pivot_length_cm <- if (pivot_at_boundary && is.finite(pairwise_q50)) {
    pairwise_q50
  } else {
    pivot_length_cm
  }
  display_pivot_source <- if (pivot_at_boundary && is.finite(pairwise_q50)) {
    "pairwise_weighted_median"
  } else {
    "ensemble_variance_minimum"
  }

  profile <- tibble::tibble(
    length_cm = length_grid,
    ts_weighted_mean = ts_weighted_mean,
    weighted_variance_v = variance_v
  )
  summary <- tibble::tibble(
    pivot_length_cm = pivot_length_cm,
    pivot_log10_length = log10(pivot_length_cm),
    pivot_min_variance_v = variance_v[pivot_idx],
    ts_consensus_at_pivot = ts_weighted_mean[pivot_idx],
    n_pairwise_intersections = nrow(pairwise),
    pairwise_pivot_q25_cm = pairwise_q25,
    pairwise_pivot_q50_cm = pairwise_q50,
    pairwise_pivot_q75_cm = pairwise_q75,
    pivot_at_boundary = pivot_at_boundary,
    pivot_display_length_cm = display_pivot_length_cm,
    pivot_display_source = display_pivot_source
  )

  list(profile = profile, pairwise = pairwise, summary = summary)
}

#' Compute the biological leverage profile
#'
#' Combines the anchor length distribution with the ensemble-mean
#' backscattering cross-section per unit mass so the package can identify the
#' length range where TS mismatch has the largest biomass consequence.
#'
#' @param models_df Model table containing standardized slopes and intercepts.
#' @param model_weights Named or aligned model-weight vector.
#' @param length_density Length-density tibble with `length_cm` and `f_len`.
#' @param pivot_summary Optional pivot summary table.
#' @param slope_col Slope column name.
#' @param intercept_col Intercept column name.
#' @param id_col Model identifier column name.
#' @param length_weight_a_col Length-weight `a` column name.
#' @param length_weight_b_col Length-weight `b` column name.
#'
#' @return A list containing leverage `profile` and `summary` tables.
#'
#' @keywords internal
#' @noRd
compute_biological_leverage <- function(models_df,
                                        model_weights,
                                        length_density,
                                        pivot_summary = NULL,
                                        slope_col = "slope_len",
                                        intercept_col = "intercept_len",
                                        id_col = "model_id",
                                        length_weight_a_col = "lw_a",
                                        length_weight_b_col = "lw_b") {
  # Align the model weights and length grid first so all later TS and biomass
  # calculations operate on one common support.
  length_grid <- length_density$length_cm
  f_len <- length_density$f_len
  w <- as.numeric(model_weights[as.character(models_df[[id_col]])])
  w[!is.finite(w)] <- 0

  if (sum(w) <= 0) {
    stop("No positive model weights available for leverage analysis.", call. = FALSE)
  }

  # Predict TS across the shared length grid, then convert the TS curves to
  # backscattering cross-sections for each candidate model.
  ts_mat <- predict_ts_matrix(
    models_df = models_df,
    lengths_cm = length_grid,
    slope_col = slope_col,
    intercept_col = intercept_col,
    id_col = id_col
  )
  sigma_mat <- 10^(ts_mat / 10)

  # Pull the length-weight parameters when they exist, otherwise fall back to
  # a conservative generic relation so the leverage profile can still be
  # computed for package-native candidate tables that omit those columns.
  lw_a <- if (length_weight_a_col %in% names(models_df)) {
    suppressWarnings(as.numeric(models_df[[length_weight_a_col]]))
  } else {
    rep(NA_real_, nrow(models_df))
  }
  lw_b <- if (length_weight_b_col %in% names(models_df)) {
    suppressWarnings(as.numeric(models_df[[length_weight_b_col]]))
  } else {
    rep(NA_real_, nrow(models_df))
  }
  lw_a[!is.finite(lw_a) | lw_a <= 0] <- 0.01
  lw_b[!is.finite(lw_b) | lw_b <= 0] <- 3.0

  # Standardize each model to sigma_bs per unit mass before averaging across
  # the admissible support set.
  phi_mat <- vapply(
    seq_len(nrow(models_df)),
    function(i) {
      sigma_mat[, i] / (lw_a[[i]] * (length_grid^lw_b[[i]]))
    },
    numeric(nrow(sigma_mat))
  )
  if (is.vector(phi_mat)) {
    phi_mat <- matrix(phi_mat, ncol = 1)
  }

  # Aggregate the per-model leverage kernels and quantify the biomass effect
  # of a one-decibel TS perturbation across length.
  w_norm <- w / sum(w)
  phi_bar <- as.numeric(phi_mat %*% w_norm)
  lambda_l <- phi_bar * f_len
  delta_1db <- lambda_l * (10^(1 / 10) - 1)
  peak_idx <- which.max(lambda_l)

  # Carry the pivot diagnostics forward when the caller provides them so later
  # plots can align leverage peaks against the pivot summaries.
  pivot_length_cm <- if (!is.null(pivot_summary) && "pivot_length_cm" %in% names(pivot_summary)) {
    pivot_summary$pivot_length_cm[[1]]
  } else {
    NA_real_
  }
  pivot_display_length_cm <- if (!is.null(pivot_summary) && "pivot_display_length_cm" %in% names(pivot_summary)) {
    pivot_summary$pivot_display_length_cm[[1]]
  } else {
    pivot_length_cm
  }
  pairwise_q25 <- if (!is.null(pivot_summary) && "pairwise_pivot_q25_cm" %in% names(pivot_summary)) {
    pivot_summary$pairwise_pivot_q25_cm[[1]]
  } else {
    NA_real_
  }
  pairwise_q75 <- if (!is.null(pivot_summary) && "pairwise_pivot_q75_cm" %in% names(pivot_summary)) {
    pivot_summary$pairwise_pivot_q75_cm[[1]]
  } else {
    NA_real_
  }
  pivot_display_source <- if (!is.null(pivot_summary) && "pivot_display_source" %in% names(pivot_summary)) {
    pivot_summary$pivot_display_source[[1]]
  } else {
    "ensemble_variance_minimum"
  }

  profile <- tibble::tibble(
    length_cm = length_grid,
    phi_bar = phi_bar,
    f_len = f_len,
    lambda_l = lambda_l,
    delta_biomass_1db = delta_1db
  )
  summary <- tibble::tibble(
    lambda_bar = sum(lambda_l, na.rm = TRUE),
    peak_length_cm = length_grid[[peak_idx]],
    peak_lambda = lambda_l[[peak_idx]],
    peak_delta_1db = delta_1db[[peak_idx]],
    pivot_length_cm = pivot_length_cm,
    pivot_display_length_cm = pivot_display_length_cm,
    pairwise_pivot_q25_cm = pairwise_q25,
    pairwise_pivot_q75_cm = pairwise_q75,
    pivot_display_source = pivot_display_source,
    pivot_offset_cm = if (is.finite(pivot_length_cm)) length_grid[[peak_idx]] - pivot_length_cm else NA_real_
  )

  list(profile = profile, summary = summary)
}

#' Summarize one evaluation object
#'
#' Computes consensus, weighted quantile, and spread summaries from an anchor
#' evaluation object.
#'
#' @param eval_obj Anchor evaluation object.
#' @param probs Quantile probabilities.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
summarize_evaluation <- function(eval_obj,
                                 probs = c(0.05, 0.50, 0.95)) {
  # Return a fully missing one-row summary when no admissible support exists.
  if (is.null(eval_obj) || nrow(eval_obj$admissible_df) == 0) {
    return(tibble::tibble(
      n_admissible = 0L,
      consensus_multiplier = NA_real_,
      multiplier_q05 = NA_real_,
      multiplier_q50 = NA_real_,
      multiplier_q95 = NA_real_,
      log_spread = NA_real_
    ))
  }

  # Compute weighted multiplier summaries from the admissible donor pool.
  q <- weighted_quantile(
    x = eval_obj$admissible_df$biomass_multiplier_if_replace,
    w = eval_obj$admissible_df$w_adm,
    probs = probs
  )

  tibble::tibble(
    n_admissible = nrow(eval_obj$admissible_df),
    consensus_multiplier = sum(
      eval_obj$admissible_df$w_adm * eval_obj$admissible_df$biomass_multiplier_if_replace,
      na.rm = TRUE
    ),
    multiplier_q05 = q[[1]],
    multiplier_q50 = q[[2]],
    multiplier_q95 = q[[3]],
    log_spread = if (is.finite(q[[1]]) && q[[1]] > 0 && is.finite(q[[3]]) && q[[3]] > 0) log(q[[3]] / q[[1]]) else NA_real_
  )
}

#' Summarize anchor missingness mix
#'
#' Computes weighted and unweighted missingness summaries for one admissible
#' anchor donor pool.
#'
#' @param admissible_df Admissible donor table.
#' @param miss_tbl Missingness table.
#' @param id_col Join identifier column.
#' @param miss_col Missingness fraction column.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
summarize_missing_mix <- function(admissible_df,
                                  miss_tbl,
                                  id_col = "model_id",
                                  miss_col = "missing_trait_fraction") {
  # Join missingness values onto the admissible donor pool before computing the
  # weighted and unweighted missingness summaries.
  tibble::as_tibble(admissible_df) |>
    dplyr::left_join(
      tibble::as_tibble(miss_tbl) |>
        dplyr::select(dplyr::all_of(id_col), dplyr::all_of(miss_col)),
      by = id_col
    ) |>
    dplyr::summarise(
      weighted_missingness = sum(.data$w_adm * dplyr::coalesce(.data[[miss_col]], 0), na.rm = TRUE),
      mean_missingness = mean(dplyr::coalesce(.data[[miss_col]], 0), na.rm = TRUE),
      .groups = "drop"
    )
}

#' Build anchor-level similarity ablation diagnostics
#'
#' Recomputes each stored anchor admissibility screen after zeroing one active
#' similarity component at a time, then stores the resulting spread, consensus,
#' and admissible-support deltas for downstream `plot(candidates, ...)`
#' diagnostics.
#'
#' @param candidates A [Candidates] object with stored similarity and
#'   admissibility state.
#' @param config Optional config or anchor-config overrides.
#' @param registry_path Optional trait-registry path.
#' @param progress Optional logical scalar.
#'
#' @return An updated [Candidates] object containing anchor-level uncertainty
#'   diagnostics in its admissibility results.
#'
#' @keywords internal
#' @noRd
build_candidates_uncertainty_diagnostics <- function(candidates,
                                                     config = NULL,
                                                     registry_path = NULL,
                                                     progress = NULL) {
  if (!is_s7_instance(candidates, "Candidates")) {
    stop("'candidates' must be a `Candidates` object.", call. = FALSE)
  }
  if (length(candidates) == 0) {
    stop(
      "No prepared similarity state is stored on this `Candidates` object. Run `prepare_similarities()` first.",
      call. = FALSE
    )
  }
  if (length(candidates@admissibility) == 0 || length((candidates@admissibility)$anchors %||% list()) == 0) {
    stop(
      "No anchor admissibility results are stored on this `Candidates` object. Run `screen_admissibility()` first.",
      call. = FALSE
    )
  }

  progress <- progress %||% FALSE
  sim_obj <- candidates@similarity_matrix
  tuning_obj <- candidates@similarity_tuning %||% list()
  cfg_base <- default_anchor_config(config %||% candidates)

  # Start from the operational admissibility configuration. Only the distance
  # hyperparameters fall back to the prepared similarity object because the
  # ablation reruns still need a concrete similarity surface to evaluate the
  # configured admissibility traits.
  cfg_base$species_traits <- as.list(cfg_base$species_traits %||% list())
  cfg_base$study_traits <- as.list(cfg_base$study_traits %||% list())
  cfg_base$alpha <- cfg_base$alpha %||% sim_obj$alpha %||% NULL
  cfg_base$k_species <- cfg_base$k_species %||% sim_obj$k_species %||% NULL
  cfg_base$k_study <- cfg_base$k_study %||% sim_obj$k_study %||% NULL

  component_tbl <- dplyr::bind_rows(
    tibble::tibble(
      component = names(cfg_base$species_traits),
      component_type = "species_trait",
      active_weight = suppressWarnings(as.numeric(unlist(cfg_base$species_traits)))
    ),
    tibble::tibble(
      component = names(cfg_base$study_traits),
      component_type = "study_trait",
      active_weight = suppressWarnings(as.numeric(unlist(cfg_base$study_traits)))
    ),
    tibble::tibble(
      component = c("length_coherence", "depth_coherence", "frequency_coherence"),
      component_type = "coherence",
      active_weight = c(
        suppressWarnings(as.numeric(cfg_base$length_overlap_weight)),
        suppressWarnings(as.numeric(cfg_base$depth_overlap_weight)),
        suppressWarnings(as.numeric(cfg_base$frequency_coherence_weight))
      )
    )
  ) |>
    dplyr::filter(!is.na(.data$component), nzchar(.data$component), is.finite(.data$active_weight), .data$active_weight > 0)

  if (nrow(component_tbl) == 0) {
    stop("No active similarity components were available for anchor ablation.", call. = FALSE)
  }

  global_component_order <- tibble::as_tibble(tuning_obj$component_impact_summary %||% tibble::tibble()) |>
    dplyr::filter(
      .data$component != "full_model",
      !is.na(.data$component),
      nzchar(.data$component)
    ) |>
    dplyr::arrange(dplyr::desc(.data$delta_rmse), .data$component) |>
    dplyr::pull(.data$component)
  global_component_order <- unique(c(global_component_order, component_tbl$component))
  component_rank <- stats::setNames(seq_along(global_component_order), global_component_order)

  candidate_models_scored <- screen_missing_metadata(
    candidate_models = candidates@candidate_models,
    key_cols = admissibility_key_metadata_cols(candidates_config_data(candidates))
  )
  excluded_model_ids <- if ("model_id" %in% names(candidates@reference_anchors)) {
    as.character(candidates@reference_anchors$model_id)
  } else if ("model_id" %in% names(candidates@reference_anchors)) {
    as.character(candidates@reference_anchors$model_id)
  } else {
    character(0)
  }
  excluded_model_ids <- unique(excluded_model_ids[!is.na(excluded_model_ids) & nzchar(excluded_model_ids)])
  points_missing_df <- tibble::as_tibble((candidates@ordination$model$points_missing %||% tibble::tibble()))
  anchor_results <- (candidates@admissibility)$anchors %||% list()

  report_progress(progress, "Building anchor-level similarity ablation diagnostics.")

  context_rows <- list()
  ablation_rows <- list()

  for (anchor_id in names(anchor_results)) {
    anchor_result <- anchor_results[[anchor_id]]
    anchor_row <- tibble::as_tibble(anchor_result$anchor %||% tibble::tibble())
    baseline_eval <- anchor_result$evaluation %||% NULL
    if (nrow(anchor_row) == 0 || is.null(baseline_eval)) {
      next
    }

    anchor_species <- as.character(anchor_row$species_name[[1]] %||% anchor_id)
    report_progress(progress, "Ablating similarity components for anchor '", anchor_species, "'.")

    baseline_summary <- summarize_evaluation(baseline_eval)
    baseline_missing <- if (nrow(points_missing_df) > 0 && nrow(tibble::as_tibble(baseline_eval$admissible_df)) > 0) {
      summarize_missing_mix(
        admissible_df = baseline_eval$admissible_df,
        miss_tbl = points_missing_df,
        id_col = "model_id",
        miss_col = "missing_trait_fraction"
      )
    } else {
      tibble::tibble(
        weighted_missingness = NA_real_,
        mean_missingness = NA_real_
      )
    }

    context_rows[[length(context_rows) + 1L]] <- tibble::tibble(
      anchor_model_id = as.character(anchor_id),
      anchor_species = anchor_species,
      weighted_missingness = baseline_missing$weighted_missingness[[1]],
      mean_missingness = baseline_missing$mean_missingness[[1]],
      n_admissible = baseline_summary$n_admissible[[1]],
      consensus_multiplier = baseline_summary$consensus_multiplier[[1]],
      multiplier_q05 = baseline_summary$multiplier_q05[[1]],
      multiplier_q50 = baseline_summary$multiplier_q50[[1]],
      multiplier_q95 = baseline_summary$multiplier_q95[[1]],
      log_spread = baseline_summary$log_spread[[1]]
    )

    baseline_consensus <- suppressWarnings(as.numeric(baseline_summary$consensus_multiplier[[1]]))
    baseline_n <- suppressWarnings(as.integer(baseline_summary$n_admissible[[1]]))
    baseline_spread <- suppressWarnings(as.numeric(baseline_summary$log_spread[[1]]))

    # Rebuild the anchor screen after dropping each active component so the
    # stored deltas are tied to the operational admissibility pipeline.
    for (j in seq_len(nrow(component_tbl))) {
      component_name <- as.character(component_tbl$component[[j]])
      component_type <- as.character(component_tbl$component_type[[j]])
      cfg_now <- cfg_base

      if (identical(component_type, "species_trait")) {
        cfg_now$species_traits[[component_name]] <- 0
      } else if (identical(component_type, "study_trait")) {
        cfg_now$study_traits[[component_name]] <- 0
      } else if (identical(component_name, "length_coherence")) {
        cfg_now$length_overlap_weight <- 0
      } else if (identical(component_name, "depth_coherence")) {
        cfg_now$depth_overlap_weight <- 0
      } else if (identical(component_name, "frequency_coherence")) {
        cfg_now$frequency_coherence_weight <- 0
      }

      eval_now <- tryCatch(
        screen_one_anchor_admissibility(
          anchor_row = anchor_row,
          candidate_models = candidates@candidate_models,
          config = cfg_now,
          registry_path = registry_path,
          sim_obj = candidates@similarity_matrix,
          dist_obj = candidates@gower_distances,
          candidate_models_scored = candidate_models_scored,
          excluded_model_ids = excluded_model_ids
        ),
        error = function(e) NULL
      )
      summary_now <- summarize_evaluation(eval_now)
      spread_now <- suppressWarnings(as.numeric(summary_now$log_spread[[1]]))
      consensus_now <- suppressWarnings(as.numeric(summary_now$consensus_multiplier[[1]]))
      n_now <- suppressWarnings(as.integer(summary_now$n_admissible[[1]]))

      delta_log_consensus <- if (is.finite(consensus_now) &&
        consensus_now > 0 &&
        is.finite(baseline_consensus) &&
        baseline_consensus > 0) {
        log(consensus_now) - log(baseline_consensus)
      } else {
        NA_real_
      }
      delta_log_spread <- spread_now - baseline_spread
      delta_n_admissible <- n_now - baseline_n

      ablation_rows[[length(ablation_rows) + 1L]] <- tibble::tibble(
        anchor_model_id = as.character(anchor_id),
        anchor_species = anchor_species,
        component = component_name,
        component_type = component_type,
        component_rank_global = component_rank[[component_name]] %||% length(component_rank) + 1L,
        baseline_log_spread = baseline_spread,
        baseline_consensus_multiplier = baseline_consensus,
        baseline_n_admissible = baseline_n,
        ablated_log_spread = spread_now,
        ablated_consensus_multiplier = consensus_now,
        ablated_n_admissible = n_now,
        delta_log_spread = delta_log_spread,
        delta_log_consensus = delta_log_consensus,
        delta_n_admissible = delta_n_admissible,
        importance_score = pmax(delta_log_spread, 0, na.rm = TRUE) +
          0.5 * abs(delta_log_consensus) +
          0.02 * pmax(-delta_n_admissible, 0, na.rm = TRUE)
      )
    }
  }

  anchor_ablation <- dplyr::bind_rows(ablation_rows) |>
    dplyr::arrange(anchor_species, .data$component_rank_global, dplyr::desc(.data$importance_score), .data$component)
  overall_tbl <- anchor_ablation |>
    dplyr::group_by(.data$component, .data$component_type, .data$component_rank_global) |>
    dplyr::summarise(
      importance_score = mean(.data$importance_score, na.rm = TRUE),
      delta_log_spread = mean(.data$delta_log_spread, na.rm = TRUE),
      delta_log_consensus = mean(.data$delta_log_consensus, na.rm = TRUE),
      delta_n_admissible = mean(.data$delta_n_admissible, na.rm = TRUE),
      n_anchors = dplyr::n_distinct(.data$anchor_model_id),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$component_rank_global, dplyr::desc(.data$importance_score), .data$component)

  report_progress(progress, "Finished anchor-level similarity ablation diagnostics.")

  updated_admissibility <- candidates@admissibility
  updated_admissibility$uncertainty_diagnostics <- list(
    context = dplyr::bind_rows(context_rows),
    anchor_ablation = anchor_ablation,
    overall = overall_tbl
  )
  candidates_with_admissibility(candidates, updated_admissibility)
}

#' Check empirical conformal coverage
#'
#' Computes the empirical fraction of calibration residuals that fall at or
#' below the conformal quantile and compares it against the nominal 1-alpha
#' target. The finite-sample guarantee is marginal coverage >=
#' 1 - alpha - 1/(n+1), so the function also returns that lower bound.
#'
#' @param calibration_residuals Numeric vector of absolute log-backscatter
#'   residuals from the calibration split (|log(sigma_pred / sigma_obs)|).
#' @param q Conformal quantile radius (scalar or per-row numeric vector).
#' @param alpha Miscoverage rate used to compute the nominal 1-alpha target.
#'   Defaults to 0.1 (90 percent nominal coverage).
#'
#' @return A one-row tibble with empirical and nominal coverage, their
#'   difference, and a logical `covered` flag.
#'
#' @keywords internal
#' @noRd
check_conformal_coverage <- function(calibration_residuals,
                                     q,
                                     alpha = 0.1) {
  residuals <- as.numeric(calibration_residuals)
  residuals <- residuals[is.finite(residuals)]
  n <- length(residuals)
  q_val <- suppressWarnings(as.numeric(q[[1]]))
  nominal <- 1 - as.numeric(alpha)
  finite_sample_floor <- max(0, nominal - 1 / (n + 1))

  if (n == 0 || !is.finite(q_val)) {
    return(tibble::tibble(
      n_calibration = n,
      q = q_val,
      nominal_coverage = nominal,
      finite_sample_floor = finite_sample_floor,
      empirical_coverage = NA_real_,
      coverage_gap = NA_real_,
      covered = NA
    ))
  }

  emp_cov <- mean(residuals <= q_val)
  tibble::tibble(
    n_calibration = n,
    q = q_val,
    nominal_coverage = nominal,
    finite_sample_floor = finite_sample_floor,
    empirical_coverage = emp_cov,
    coverage_gap = emp_cov - nominal,
    covered = emp_cov >= finite_sample_floor
  )
}
