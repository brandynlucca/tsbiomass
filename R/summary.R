#' Add key-metadata missingness for a [Candidates] object
#'
#' @name screen_missing_metadata.Candidates
#' @usage NULL
#'
#' @keywords internal
S7::method(screen_missing_metadata, Candidates) <- function(candidate_models,
                                                            key_cols = NULL) {
  # Reuse the staged candidate key columns before delegating to the tabular
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
#' @param pseudo_sum Pseudo-anchor conformal summary list or a
#'   [PolicySelector] object.
#' @param ... Method-specific arguments.
#'
#' @return A tibble.
#'
#' @examples
#' \dontrun{
#' build_species_coverage(selector)
#' }
#'
#' @export
build_species_coverage <- S7::new_generic("build_species_coverage", "pseudo_sum")

#' Build a species-block coverage table from conformal summary lists
#'
#' @name build_species_coverage.default
#' @usage NULL
#' @param pseudo_sum Pseudo-anchor conformal summary list.
#' @param species_sum Species-block conformal summary list.
#' @param bench_label Species-block benchmark label.
S7::method(build_species_coverage, S7::class_any) <- function(pseudo_sum,
                                                              species_sum,
                                                              bench_label = "species_block") {
  # Bind the conformal coverage summaries across benchmark schemes, then keep
  # only the requested benchmark label.
  dplyr::bind_rows(
    tibble::as_tibble(pseudo_sum$overall),
    tibble::as_tibble(species_sum$overall)
  ) |>
    dplyr::filter(benchmark_label == bench_label) |>
    standardize_policies() |>
    dplyr::select(policy, equation_branch_filter, empirical_coverage, median_interval_log_width)
}

#' Build a species-block coverage table from a [PolicySelector]
#'
#' @name build_species_coverage.PolicySelector
#' @usage NULL
S7::method(build_species_coverage, PolicySelector) <- function(pseudo_sum,
                                                               species_sum = NULL,
                                                               bench_label = "species_block") {
  # Pull the stored uncertainty bundle from the selector, then reuse the
  # default summary-list method.
  if (length(pseudo_sum@uncertainty) == 0) {
    stop("No uncertainty calibration is stored on this `PolicySelector`.", call. = FALSE)
  }
  uncertainty_obj <- pseudo_sum@uncertainty

  build_species_coverage(
    pseudo_sum = uncertainty_obj$pseudo_sum %||% list(overall = tibble::tibble()),
    species_sum = uncertainty_obj$species_sum %||% list(overall = tibble::tibble()),
    bench_label = bench_label
  )
}

#' Build the anchor support audit
#'
#' Joins selected-policy intervals to global benchmark and conformal coverage
#' summaries, and optionally appends sensitivity-drift diagnostics.
#'
#' @param sel_tbl Selected-policy interval table or a [PolicyPredictions]
#'   object.
#' @param ... Method-specific arguments.
#'
#' @return A tibble.
#'
#' @examples
#' \dontrun{
#' predictions <- predict(selector)
#' build_anchor_audit(predictions, selector = selector)
#' }
#'
#' @export
build_anchor_audit <- S7::new_generic("build_anchor_audit", "sel_tbl")

#' Build an anchor support audit from selected-policy interval tables
#'
#' @name build_anchor_audit.default
#' @usage NULL
#' @param sel_tbl Selected-policy interval table.
#' @param select_ref Policy-selection reference table.
#' @param cover_tbl Policy-level coverage summary table.
#' @param sens_detail Optional sensitivity-detail table.
#' @param sens_tbl Optional full policy-sensitivity table.
#' @param baseline_label Baseline scenario label.
#' @param selector Optional [PolicySelector] object used when `sel_tbl` is a
#'   [PolicyPredictions] object.
S7::method(build_anchor_audit, S7::class_any) <- function(sel_tbl,
                                                          select_ref,
                                                          cover_tbl,
                                                          sens_detail = NULL,
                                                          sens_tbl = NULL,
                                                          baseline_label = "baseline",
                                                          selector = NULL) {
  # Start from the selected-policy rows and layer on the global benchmark and
  # conformal coverage summaries by selected policy.
  sel_tbl <- tibble::as_tibble(sel_tbl)
  sel_tbl$selected_policy <- resolve_selected_policy_values(sel_tbl)
  sel_tbl$selected_policy_display <- resolve_selected_policy_names(sel_tbl)
  sel_tbl$selected_equation_branch_filter <- selected_branches(sel_tbl)
  sel_tbl$equivalent_policy_set <- resolve_equivalent_policy_sets(sel_tbl)

  out <- sel_tbl |>
    dplyr::select(dplyr::any_of(c(
      "anchor_model_id", "anchor_species", "selected_policy", "selected_policy_display",
      "selected_equation_branch_filter",
      "equivalent_policy_set", "equivalent_policy_set_n",
      "equivalence_class_id", "equivalence_class_size", "equivalence_class_members",
      "multiplier_pred", "multiplier_lo", "multiplier_hi", "interval_log_width",
      "local_support_mass", "local_effective_support", "local_mean_combined_distance",
      "local_mean_length_overlap", "local_mean_depth_overlap", "local_weighted_missingness"
    ))) |>
    dplyr::left_join(
      {
        select_ref_tbl <- standardize_policies(select_ref)
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
          policy, equation_branch_filter, mean_species_median_abs_log, acceptable_global, equivalent_to_best_global,
          bootstrap_prob_within_threshold, bootstrap_median_rank
        ),
      by = c(
        "selected_policy" = "policy",
        "selected_equation_branch_filter" = "equation_branch_filter"
      )
    ) |>
    dplyr::left_join(
      {
        cover_tbl <- standardize_policies(cover_tbl)
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
      dplyr::filter(scenario == baseline_label) |>
      dplyr::select(anchor_model_id, baseline_multiplier = multiplier_pred)

    change_tbl <- tibble::as_tibble(sens_detail) |>
      dplyr::group_by(anchor_model_id, anchor_species) |>
      dplyr::summarise(
        n_policy_changed = sum(policy_changed, na.rm = TRUE),
        n_display_changed = sum(display_changed, na.rm = TRUE),
        n_equiv_set_changed = sum(equiv_set_changed, na.rm = TRUE),
        .groups = "drop"
      )

    drift_tbl <- tibble::as_tibble(sens_tbl) |>
      dplyr::filter(scenario != baseline_label) |>
      dplyr::left_join(base_mult, by = "anchor_model_id") |>
      dplyr::group_by(anchor_model_id, anchor_species) |>
      dplyr::summarise(
        n_scenarios = dplyr::n(),
        median_abs_delta_log_multiplier = stats::median(abs(log(multiplier_pred / baseline_multiplier)), na.rm = TRUE),
        max_abs_delta_log_multiplier = max(abs(log(multiplier_pred / baseline_multiplier)), na.rm = TRUE),
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
#' @name build_anchor_audit.PolicyPredictions
#' @usage NULL
S7::method(build_anchor_audit, PolicyPredictions) <- function(sel_tbl,
                                                              select_ref = NULL,
                                                              cover_tbl = NULL,
                                                              sens_detail = NULL,
                                                              sens_tbl = NULL,
                                                              baseline_label = "baseline",
                                                              selector = NULL) {
  if ((inherits(select_ref, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(select_ref, PolicySelector), error = function(e) FALSE))) && is.null(selector)) {
    selector <- select_ref
    select_ref <- NULL
  }
  selector <- selector %||% select_ref
  if (!(inherits(selector, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(selector, PolicySelector), error = function(e) FALSE)))) {
    stop(
      "When `sel_tbl` is a `PolicyPredictions` object, `selector` must be a `PolicySelector` object.",
      call. = FALSE
    )
  }

  # Pull the global selection summaries and policy-level coverage directly from
  # the selector unless the caller supplied explicit replacements.
  select_ref <- select_ref %||% (selector@selection)$final_ref %||% tibble::tibble()
  cover_tbl <- cover_tbl %||% build_species_coverage(selector)

  build_anchor_audit(
    sel_tbl = sel_tbl@selections,
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
#' @examples
#' \dontrun{
#' summarize_key_missing(selector)
#' }
#'
#' @export
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
    dplyr::arrange(dplyr::desc(missing_fraction), field)

  by_model <- models_tbl |>
    dplyr::select(
      dplyr::all_of(model_id_col),
      dplyr::all_of(species_col),
      dplyr::any_of(common_col),
      key_metadata_missing_fraction
    ) |>
    dplyr::arrange(dplyr::desc(key_metadata_missing_fraction), .data[[species_col]], .data[[model_id_col]])

  list(overall = overall, by_field = by_field, by_model = by_model)
}

#' Summarize key-field missingness for a [Candidates] object
#'
#' @name summarize_key_missing.Candidates
#' @usage NULL
S7::method(summarize_key_missing, Candidates) <- function(candidate_models,
                                                          key_cols = NULL,
                                                          threshold = NA_real_,
                                                          model_id_col = "model_id",
                                                          species_col = "species_name",
                                                          common_col = "common") {
  # Materialize the keyed missingness screen first, then summarize that staged
  # table through the default method.
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
#' @examples
#' \dontrun{
#' summarize_missing_gate(selector)
#' }
#'
#' @export
summarize_missing_gate <- S7::new_generic("summarize_missing_gate", "adm_tbl")

#' Summarize missingness gate outcomes for admissibility summary tables
#'
#' @name summarize_missing_gate.default
#' @usage NULL
#' @param adm_tbl Admissibility summary table.
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
        dplyr::group_by(anchor_model_id, anchor_species) |>
        dplyr::summarise(
          n_candidates_total = dplyr::n(),
          n_candidates_admissible = sum(dplyr::coalesce(as.logical(admissible), FALSE), na.rm = TRUE),
          prop_fail_missing_metadata = mean(!dplyr::coalesce(as.logical(gate_missing_key_metadata), FALSE), na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(anchor_species)
    )
  }

  # Support both the older anchor-level gate summary and the current
  # admissible-pool summary emitted by `screen_admissibility()`.
  if (all(c("n_candidates_total", "prop_admissible", "prop_fail_missing_metadata") %in% names(out))) {
    return(
      out |>
        dplyr::transmute(
          anchor_model_id,
          anchor_species,
          n_candidates_total,
          n_candidates_admissible = round(n_candidates_total * prop_admissible),
          prop_fail_missing_metadata
        ) |>
        dplyr::arrange(anchor_species)
    )
  }

  if (all(c("anchor_model_id", "anchor_species", "n_admissible") %in% names(out))) {
    return(
      out |>
        dplyr::transmute(
          anchor_model_id,
          anchor_species,
          n_candidates_total = NA_real_,
          n_candidates_admissible = dplyr::coalesce(as.numeric(n_admissible), 0),
          prop_fail_missing_metadata = NA_real_
        ) |>
        dplyr::arrange(anchor_species)
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
S7::method(summarize_missing_gate, Candidates) <- function(adm_tbl) {
  scores_tbl <- if (!(inherits(adm_tbl, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(adm_tbl, Candidates), error = function(e) FALSE))) || length(adm_tbl@admissibility) == 0) {
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
#' @export
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
#' @export
summarize_slope_effect <- function(candidate_models,
                                   slope_col = "slope_len",
                                   intercept_col = "intercept_len",
                                   group_col = "is_group_model",
                                   eq_col = "equation_form",
                                   eq_type_col = "equation_form_type",
                                   study_cell_col = "study_cell_id",
                                   study_ref_col = "study_reference_id",
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
  # already carry one. This uses the current package table shape rather than
  # the old script's `common` column.
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
    models_tbl[[group_col]] <- dplyr::case_when(
      species_chr %in% c("", "na", "na na") ~ TRUE,
      !has_species_identity & tags_chr %in% generic_labels ~ TRUE,
      !has_species_identity & swim_chr == "general/nonspecific" ~ TRUE,
      TRUE ~ FALSE
    )
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
        review_group,
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
        original_reference_class,
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
        slope_deviation_class,
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
      review_group, .data[[family_col]], .data[[genus_col]]
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
      slope_deviation_from_20 = slope_len_cell - 20,
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
        slope_deviation_class,
        levels = c("< -2", "-2 to -1", "-1 to 0", "exactly 20", "0 to 1", "1 to 2", "> 2")
      ),
      slope_support_class = dplyr::case_when(
        any_fixed_20 | abs(slope_len_cell - 20) < 1e-8 ~ "Exact 20",
        slope_len_cell >= 18 & slope_len_cell <= 22 ~ "Near 20 (18-22)",
        slope_len_cell < 18 ~ "Below 18",
        slope_len_cell > 22 ~ "Above 22",
        TRUE ~ "Other"
      )
    )

  # Summarize the study-cell slopes overall and by review group for later
  # reporting and plotting.
  group_summary <- study_cell_level |>
    dplyr::group_by(review_group) |>
    dplyr::summarise(
      n_study_cells = dplyr::n(),
      n_species = dplyr::n_distinct(.data[[species_col]]),
      median_slope = stats::median(slope_len_cell, na.rm = TRUE),
      mean_slope = mean(slope_len_cell, na.rm = TRUE),
      q25_slope = stats::quantile(slope_len_cell, 0.25, na.rm = TRUE, names = FALSE, type = 8),
      q75_slope = stats::quantile(slope_len_cell, 0.75, na.rm = TRUE, names = FALSE, type = 8),
      prop_exact_20 = mean(slope_support_class == "Exact 20", na.rm = TRUE),
      prop_near_20 = mean(slope_len_cell >= 18 & slope_len_cell <= 22, na.rm = TRUE),
      prop_above_22 = mean(slope_len_cell > 22, na.rm = TRUE),
      prop_below_18 = mean(slope_len_cell < 18, na.rm = TRUE),
      mean_equation_variants_per_cell = mean(n_equation_forms, na.rm = TRUE),
      mean_model_variants_per_cell = mean(n_model_variants, na.rm = TRUE),
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
    dplyr::count(review_group, slope_support_class, name = "n_study_cells") |>
    dplyr::group_by(review_group) |>
    dplyr::mutate(prop_study_cells = n_study_cells / sum(n_study_cells)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      slope_support_class = factor(
        slope_support_class,
        levels = c("Below 18", "Near 20 (18-22)", "Exact 20", "Above 22", "Other")
      )
    )

  deviation_support_by_group <- slope_models |>
    dplyr::group_by(.data[[study_cell_col]]) |>
    dplyr::mutate(study_cell_variant_weight = 1 / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(original_reference_class)) |>
    dplyr::group_by(review_group, original_reference_class) |>
    dplyr::summarise(
      weighted_study_cell_count = sum(study_cell_variant_weight, na.rm = TRUE),
      n_model_rows = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(review_group) |>
    dplyr::mutate(prop_study_cells = weighted_study_cell_count / sum(weighted_study_cell_count)) |>
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
#' @export
summarize_area_studies <- function(candidate_models,
                                   fao_col = "fao_area",
                                   study_col = "study_reference_id") {
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
    dplyr::distinct(fao_area_chr, .data[[study_col]]) |>
    dplyr::count(fao_area_chr, name = "n_studies") |>
    dplyr::arrange(dplyr::desc(n_studies), fao_area_chr)
}

#' Build FAO inset tiles
#'
#' Builds the inset-tile table used for unknown or normalized inland FAO codes
#' in the FAO-area summary graphic.
#'
#' @param count_tbl FAO study-count table from [summarize_area_studies()].
#'
#' @return A tibble with tile bounds and joined study counts.
#'
#' @export
build_area_inset_tiles <- function(count_tbl) {
  # Join the study-count table onto the fixed inset layout so the plotting
  # layer can draw the normalized inland codes without more data wrangling.
  tibble::tribble(
    ~fao_area_chr, ~area_label, ~xmin, ~xmax, ~ymin, ~ymax,
    "0", "0\nUnknown/other", 198, 232, 30, 48,
    "2", "2\nNormalized inland", 198, 232, 6, 24,
    "4", "4\nNormalized inland", 198, 232, -18, 0,
    "5", "5\nNormalized inland", 198, 232, -42, -24
  ) |>
    dplyr::left_join(tibble::as_tibble(count_tbl), by = "fao_area_chr") |>
    dplyr::mutate(
      n_studies = dplyr::coalesce(n_studies, 0L),
      xmid = (xmin + xmax) / 2,
      ymid = (ymin + ymax) / 2
    )
}
