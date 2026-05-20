#' Resolve one configured field name
#'
#' @param config Anchor config list.
#' @param key Field-map key to resolve.
#'
#' @return Character scalar.
#'
#' @keywords internal
anchor_field <- function(config,
                         key) {
  # Keep all source-column lookups centralized so anchor screening never needs
  # to hard-code trait names in the function body.
  field_nm <- config$fields[[key]]

  if (!is.character(field_nm) || length(field_nm) != 1 || !nzchar(field_nm)) {
    stop(sprintf("Anchor config field '%s' must be a single column name.", key), call. = FALSE)
  }

  field_nm
}

#' Build an anchor length PDF
#'
#' Builds a length-density grid from the anchor study length interval and
#' falls back to the species maximum length when the study interval is absent.
#'
#' @param anchor_row One-row anchor table.
#' @param n Number of support points in the output grid.
#'
#' @return A tibble with `length_cm` and `f_len`.
#'
#' @keywords internal
build_anchor_density <- function(anchor_row,
                             config,
                             n = 400) {
  # Start from the canonical study length interval and fall back to the species
  # maximum length only when that interval is unavailable.
  len_min_col <- anchor_field(config, "length_min")
  len_max_col <- anchor_field(config, "length_max")
  len_fallback_col <- anchor_field(config, "length_fallback")

  mins <- suppressWarnings(as.numeric(anchor_row[[len_min_col]]))
  maxs <- suppressWarnings(as.numeric(anchor_row[[len_max_col]]))
  mins <- mins[is.finite(mins)]
  maxs <- maxs[is.finite(maxs)]

  if (length(mins) == 0 || length(maxs) == 0) {
    lmax_vals <- suppressWarnings(as.numeric(anchor_row[[len_fallback_col]]))
    lmax_vals <- lmax_vals[is.finite(lmax_vals) & lmax_vals > 0]
    if (length(lmax_vals) == 0) {
      stop(
        "No valid anchor length interval or species maximum length was available.",
        call. = FALSE
      )
    }
    lmax <- max(lmax_vals)
    mins <- lmax * 0.1
    maxs <- lmax
  }

  lmin <- floor(min(mins))
  lmax <- ceiling(max(maxs))
  if (lmin >= lmax) {
    lmin <- max(1, lmin - 1)
    lmax <- lmax + 1
  }

  grid <- seq(lmin, lmax, length.out = n)

  # Draw enough values per interval to keep the KDE stable even when only one
  # anchor interval is available.
  draws_per_range <- max(50L, ceiling(n / max(1L, length(mins))))
  sampled <- purrr::map2(
    mins,
    maxs,
    function(a, b) {
      if (!is.finite(a) || !is.finite(b)) {
        return(numeric(0))
      }
      if (a >= b) {
        return(rep((a + b) / 2, draws_per_range))
      }
      stats::runif(draws_per_range, a, b)
    }
  ) |>
    unlist(use.names = FALSE)
  sampled <- sampled[is.finite(sampled)]

  if (length(sampled) >= 2 && length(unique(sampled)) >= 2) {
    dens <- stats::density(sampled, from = lmin, to = lmax, n = n, bw = "nrd0")
    f <- pmax(dens$y, 0)
    f <- f / sum(f)
    return(tibble::tibble(length_cm = dens$x, f_len = f))
  }

  # Fall back to a deterministic uniform density when the sampled interval is
  # too degenerate for KDE bandwidth selection.
  f <- rep(0, length(grid))
  for (i in seq_along(mins)) {
    a <- mins[[i]]
    b <- maxs[[i]]
    if (!is.finite(a) || !is.finite(b)) {
      next
    }
    if (a >= b) {
      idx <- which.min(abs(grid - ((a + b) / 2)))
      f[idx] <- f[idx] + 1
    } else {
      f[grid >= a & grid <= b] <- f[grid >= a & grid <= b] + 1
    }
  }
  if (sum(f) <= 0) {
    f[] <- 1
  }
  f <- f / sum(f)

  tibble::tibble(length_cm = grid, f_len = f)
}

#' Compute one directional interval overlap
#'
#' Calculates the fraction of the anchor interval covered by the comparison
#' interval.
#'
#' @param a_min Anchor minimum.
#' @param a_max Anchor maximum.
#' @param b_min Candidate minimum.
#' @param b_max Candidate maximum.
#'
#' @return A numeric scalar.
#'
#' @keywords internal
calculate_range_overlap <- function(a_min, a_max, b_min, b_max) {
  if (!is.finite(a_min) || !is.finite(a_max) ||
      !is.finite(b_min) || !is.finite(b_max)) {
    return(NA_real_)
  }
  if (a_min > a_max || b_min > b_max) {
    return(NA_real_)
  }

  inter <- max(0, min(a_max, b_max) - max(a_min, b_min))
  a_len <- max(1e-9, a_max - a_min)
  inter / a_len
}

#' Compute one normalized frequency gap
#'
#' @param candidate_freq Candidate frequency vector.
#' @param anchor_freq Anchor frequency scalar.
#' @param freq_span Positive log-frequency span.
#' @param mode Penalty mode.
#'
#' @return Numeric vector.
#'
#' @keywords internal
calculate_frequency_gap <- function(candidate_freq,
                          anchor_freq,
                          freq_span,
                          mode = "numeric") {
  base_dist <- dplyr::case_when(
    identical(mode, "none") ~ NA_real_,
    is.finite(candidate_freq) & candidate_freq > 0 &
      is.finite(anchor_freq) & anchor_freq > 0 ~
      pmin(abs(log(candidate_freq / anchor_freq)) / freq_span, 1),
    TRUE ~ NA_real_
  )

  dplyr::case_when(
    identical(mode, "label") &
      is.finite(candidate_freq) & candidate_freq > 0 &
      is.finite(anchor_freq) & anchor_freq > 0 ~
      as.numeric(as.integer(round(candidate_freq)) != as.integer(round(anchor_freq))),
    !is.finite(base_dist) ~ NA_real_,
    mode == "soft_sqrt" ~ sqrt(base_dist),
    mode == "strict_squared" ~ pmin(base_dist^2, 1),
    TRUE ~ base_dist
  )
}

#' Add key-metadata missingness
#'
#' Appends the fraction of missing selected species/study traits for each model.
#'
#' @param candidate_models Candidate-model table.
#' @param key_cols Character vector of key trait columns.
#'
#' @return A tibble.
#'
#' @keywords internal
screen_missing_metadata <- function(candidate_models,
                               key_cols) {
  key_cols <- intersect(as.character(key_cols), names(candidate_models))
  out <- tibble::as_tibble(candidate_models)

  if (length(key_cols) == 0) {
    out$key_metadata_missing_fraction <- NA_real_
    return(out)
  }

  out |>
    dplyr::mutate(
      key_metadata_missing_fraction = rowMeans(is.na(dplyr::pick(dplyr::all_of(key_cols))))
    )
}

#' Add anchor-relative overlap fields
#'
#' Computes anchor-relative taxonomy and study-domain overlap fields for one
#' anchor versus the candidate model pool.
#'
#' @param candidate_models Candidate-model table.
#' @param anchor_row One-row anchor table.
#'
#' @return A tibble.
#'
#' @keywords internal
add_anchor_overlap <- function(candidate_models,
                               anchor_row,
                               config) {
  # Pull the anchor scalars once so the row-wise overlap calculations below can
  # work against simple local values.
  species_col <- anchor_field(config, "species_name")
  genus_col <- anchor_field(config, "genus")
  family_col <- anchor_field(config, "family")
  order_col <- anchor_field(config, "order")
  swim_col <- anchor_field(config, "swimbladder")
  fao_col <- anchor_field(config, "fao_area")
  basin_col <- anchor_field(config, "ocean_basin")
  eq_col <- anchor_field(config, "equation_form")
  deriv_col <- anchor_field(config, "derivation_type")
  len_min_col <- anchor_field(config, "length_min")
  len_max_col <- anchor_field(config, "length_max")
  dep_min_col <- anchor_field(config, "depth_min")
  dep_max_col <- anchor_field(config, "depth_max")

  anchor_species <- as.character(anchor_row[[species_col]][[1]])
  anchor_genus <- as.character(anchor_row[[genus_col]][[1]])
  anchor_family <- as.character(anchor_row[[family_col]][[1]])
  anchor_order <- as.character(anchor_row[[order_col]][[1]])
  anchor_swim <- as.character(anchor_row[[swim_col]][[1]])
  anchor_fao <- as.character(anchor_row[[fao_col]][[1]])
  anchor_basin <- as.character(anchor_row[[basin_col]][[1]])
  anchor_eq <- as.character(anchor_row[[eq_col]][[1]])
  anchor_derivation <- as.character(anchor_row[[deriv_col]][[1]])
  anchor_len_min <- suppressWarnings(as.numeric(anchor_row[[len_min_col]][[1]]))
  anchor_len_max <- suppressWarnings(as.numeric(anchor_row[[len_max_col]][[1]]))
  anchor_dep_min <- suppressWarnings(as.numeric(anchor_row[[dep_min_col]][[1]]))
  anchor_dep_max <- suppressWarnings(as.numeric(anchor_row[[dep_max_col]][[1]]))

  out <- tibble::as_tibble(candidate_models)
  out$overlap_same_species <- !is.na(out[[species_col]]) & out[[species_col]] == anchor_species
  out$overlap_same_genus <- !is.na(out[[genus_col]]) & out[[genus_col]] == anchor_genus
  out$overlap_same_family <- !is.na(out[[family_col]]) & out[[family_col]] == anchor_family
  out$overlap_same_order <- !is.na(out[[order_col]]) & out[[order_col]] == anchor_order
  out$overlap_same_swimbladder <- !is.na(out[[swim_col]]) & !is.na(anchor_swim) & out[[swim_col]] == anchor_swim
  out$overlap_same_fao_area <- !is.na(out[[fao_col]]) & !is.na(anchor_fao) & out[[fao_col]] == anchor_fao
  out$overlap_same_ocean_basin <- !is.na(out[[basin_col]]) & !is.na(anchor_basin) & out[[basin_col]] == anchor_basin
  out$overlap_same_equation_form <- !is.na(out[[eq_col]]) & !is.na(anchor_eq) & out[[eq_col]] == anchor_eq
  out$overlap_same_derivation <- !is.na(out[[deriv_col]]) & !is.na(anchor_derivation) & out[[deriv_col]] == anchor_derivation
  out$length_overlap_fraction <- purrr::map2_dbl(
    out[[len_min_col]],
    out[[len_max_col]],
    ~ calculate_range_overlap(anchor_len_min, anchor_len_max, .x, .y)
  )
  out$depth_overlap_fraction <- purrr::map2_dbl(
    out[[dep_min_col]],
    out[[dep_max_col]],
    ~ calculate_range_overlap(anchor_dep_min, anchor_dep_max, .x, .y)
  )

  out
}

#' Apply anchor admissibility gates
#'
#' Applies self, swimbladder, overlap, and key-metadata gates to the
#' anchor-relative candidate table.
#'
#' @param candidate_models Candidate-model table with overlap columns.
#' @param anchor_row One-row anchor table.
#' @param config Anchor-evaluation config list.
#'
#' @return A tibble.
#'
#' @keywords internal
apply_anchor_gates <- function(candidate_models,
                               anchor_row,
                               config) {
  # Apply the gate rules after overlap/coherence fields exist so the final
  # admissibility label is determined from one fully scored table.
  id_col <- anchor_field(config, "model_id_chr")
  swim_col <- anchor_field(config, "swimbladder")
  anchor_id <- anchor_model_id(anchor_row, config)
  anchor_swim <- as.character(anchor_row[[swim_col]][[1]])

  out <- tibble::as_tibble(candidate_models)
  out$gate_not_self <- out[[id_col]] != anchor_id
  out$gate_swimbladder <- dplyr::case_when(
    is.na(out[[swim_col]]) | is.na(anchor_swim) ~ FALSE,
    out[[swim_col]] == "unknown" | anchor_swim == "unknown" ~ FALSE,
    TRUE ~ out[[swim_col]] == anchor_swim
  )
  out$gate_frequency <- TRUE
  out$gate_length_overlap <- dplyr::case_when(
    is.na(out$length_overlap_fraction) ~ TRUE,
    TRUE ~ out$length_overlap_fraction >= config$min_length_overlap_fraction
  )
  out$gate_depth_overlap <- dplyr::case_when(
    is.na(out$depth_overlap_fraction) ~ TRUE,
    TRUE ~ out$depth_overlap_fraction >= config$min_depth_overlap_fraction
  )
  out$gate_missing_key_metadata <- dplyr::case_when(
    is.na(out$key_metadata_missing_fraction) ~ TRUE,
    TRUE ~ out$key_metadata_missing_fraction <= config$missing_key_metadata_max_fraction
  )
  out$admissible <- with(
    out,
    gate_not_self & gate_swimbladder & gate_frequency & gate_length_overlap & gate_depth_overlap & gate_missing_key_metadata
  )
  out$inadmissible_reason <- dplyr::case_when(
    !out$gate_not_self ~ "self",
    !out$gate_swimbladder ~ "swimbladder_mismatch",
    !out$gate_frequency ~ "frequency_mismatch",
    !out$gate_length_overlap ~ "length_domain_nonoverlap",
    !out$gate_depth_overlap ~ "depth_domain_nonoverlap",
    !out$gate_missing_key_metadata ~ "metadata_missing_excess",
    TRUE ~ NA_character_
  )

  out
}

#' Summarize anchor overlap structure
#'
#' Collapses one anchor's admissible weighted set to a compact overlap summary.
#'
#' @param admissible_df Admissible weighted candidate table.
#'
#' @return A one-row tibble.
#'
#' @export
summarize_anchor_overlap <- function(admissible_df) {
  # Ensure the overlap flags exist and are logical before computing the weighted
  # overlap mix summaries.
  out <- tibble::as_tibble(admissible_df)
  for (nm in c(
    "overlap_same_species", "overlap_same_family", "overlap_same_swimbladder",
    "overlap_same_fao_area", "overlap_same_ocean_basin"
  )) {
    if (!nm %in% names(out)) {
      out[[nm]] <- FALSE
    }
    out[[nm]][is.na(out[[nm]])] <- FALSE
  }

  if (nrow(out) == 0) {
    return(tibble::tibble(
      n_admissible = 0,
      w_same_species = NA_real_,
      w_same_family = NA_real_,
      w_same_swimbladder = NA_real_,
      w_same_fao = NA_real_,
      w_same_ocean_basin = NA_real_,
      mean_length_overlap_fraction = NA_real_,
      mean_depth_overlap_fraction = NA_real_
    ))
  }

  tibble::tibble(
    n_admissible = nrow(out),
    w_same_species = sum(out$w_adm[out$overlap_same_species], na.rm = TRUE),
    w_same_family = sum(out$w_adm[out$overlap_same_family], na.rm = TRUE),
    w_same_swimbladder = sum(out$w_adm[out$overlap_same_swimbladder], na.rm = TRUE),
    w_same_fao = sum(out$w_adm[out$overlap_same_fao_area], na.rm = TRUE),
    w_same_ocean_basin = sum(out$w_adm[out$overlap_same_ocean_basin], na.rm = TRUE),
    mean_length_overlap_fraction = stats::weighted.mean(out$length_overlap_fraction, out$w_adm, na.rm = TRUE),
    mean_depth_overlap_fraction = stats::weighted.mean(out$depth_overlap_fraction, out$w_adm, na.rm = TRUE)
  )
}

#' Rank anchor matches
#'
#' Produces the ranked admissible-candidate table for one anchor.
#'
#' @param eval_obj Anchor evaluation object from [evaluate_anchor_models()].
#'
#' @return A tibble.
#'
#' @export
rank_anchor_models <- function(eval_obj) {
  # Order the admissible weighted set by final admissible weight and expose the
  # key diagnostics used in downstream review tables.
  tibble::as_tibble(eval_obj$admissible_df) |>
    dplyr::arrange(dplyr::desc(w_adm)) |>
    dplyr::mutate(rank_by_weight = dplyr::row_number())
}

#' Build anchor scores
#'
#' Produces the full scored candidate table for one anchor, including anchor
#' identifiers and the admissible-support annotations when available.
#'
#' @param eval_obj Anchor evaluation object from [evaluate_anchor_models()].
#' @param anchor_row One-row anchor table.
#' @param config Optional anchor config list. When `NULL`, the defaults are
#'   used.
#'
#' @return A tibble.
#'
#' @export
collect_anchor_scores <- function(eval_obj,
                                  anchor_row,
                                  config = NULL) {
  # Inline the anchor-evaluation defaults here so the caller-facing helpers do
  # not depend on a separate default-config function.
  cfg <- merge_cfg(
    list(
      fields = list(
        model_id = "model_id",
        model_id_chr = "model_id_chr",
        species_name = "species_name",
        genus = "genus",
        family = "family",
        order = "order",
        swimbladder = "swimbladder_type",
        fao_area = "fao_area",
        ocean_basin = "ocean_basin",
        equation_form = "equation_form",
        derivation_type = "derivation_type",
        length_min = "study_length_min",
        length_max = "study_length_max",
        length_fallback = "species_length_max",
        depth_min = "study_depth_min",
        depth_max = "study_depth_max",
        frequency = "frequency",
        slope = "slope_len",
        intercept = "intercept_len",
        study_cell = "study_cell_id"
      ),
      min_length_overlap_fraction = 0.25,
      min_depth_overlap_fraction = 0.25,
      missing_key_metadata_max_fraction = 0.25,
      length_overlap_weight = 2,
      depth_overlap_weight = 3,
      frequency_coherence_weight = 2,
      frequency_coherence_mode = "numeric",
      core_weight_cutoff = 0.80
    ),
    config %||% list()
  )
  anchor_id <- anchor_model_id(anchor_row, cfg)
  anchor_species <- anchor_species_name(anchor_row, cfg)

  scored <- tibble::as_tibble(eval_obj$model_eval) |>
    dplyr::mutate(anchor_model_id = anchor_id, anchor_species = anchor_species)

  # Reattach the admissible-support columns only for rows that survived the
  # admissibility screen.
  support_cols <- intersect(
    c(
      "model_id_chr", "study_cell_id", "study_cell_n_models",
      "w_hybrid", "w_study_adj_raw", "w_adm",
      "cumulative_w_adm", "support_set"
    ),
    names(eval_obj$admissible_df)
  )

  if (length(support_cols) > 0) {
    scored <- scored |>
      dplyr::left_join(
        tibble::as_tibble(eval_obj$admissible_df) |>
          dplyr::select(dplyr::all_of(support_cols)),
        by = intersect(c("model_id_chr", "w_hybrid"), support_cols)
      )
  }

  scored
}

#' Count gate reasons
#'
#' Counts admissible and inadmissible rows by gate outcome for one anchor.
#'
#' @param scored_df Scored candidate table from [collect_anchor_scores()].
#' @param anchor_row One-row anchor table.
#' @param config Optional anchor config list. When `NULL`, the defaults are
#'   used.
#'
#' @return A tibble.
#'
#' @export
summarize_gate_counts <- function(scored_df,
                                  anchor_row,
                                  config = NULL) {
  # Inline the same anchor defaults here so the gate summary resolves column
  # mappings and thresholds without a config helper.
  cfg <- merge_cfg(
    list(
      fields = list(
        model_id = "model_id",
        model_id_chr = "model_id_chr",
        species_name = "species_name",
        genus = "genus",
        family = "family",
        order = "order",
        swimbladder = "swimbladder_type",
        fao_area = "fao_area",
        ocean_basin = "ocean_basin",
        equation_form = "equation_form",
        derivation_type = "derivation_type",
        length_min = "study_length_min",
        length_max = "study_length_max",
        length_fallback = "species_length_max",
        depth_min = "study_depth_min",
        depth_max = "study_depth_max",
        frequency = "frequency",
        slope = "slope_len",
        intercept = "intercept_len",
        study_cell = "study_cell_id"
      ),
      min_length_overlap_fraction = 0.25,
      min_depth_overlap_fraction = 0.25,
      missing_key_metadata_max_fraction = 0.25,
      length_overlap_weight = 2,
      depth_overlap_weight = 3,
      frequency_coherence_weight = 2,
      frequency_coherence_mode = "numeric",
      core_weight_cutoff = 0.80
    ),
    config %||% list()
  )
  anchor_id <- anchor_model_id(anchor_row, cfg)
  anchor_species <- anchor_species_name(anchor_row, cfg)

  tibble::as_tibble(scored_df) |>
    dplyr::mutate(inadmissible_reason = dplyr::coalesce(inadmissible_reason, "admissible")) |>
    dplyr::count(inadmissible_reason, name = "n_models") |>
    dplyr::mutate(anchor_model_id = anchor_id, anchor_species = anchor_species)
}

#' Summarize one anchor pool
#'
#' Produces a compact anchor-level summary from the admissible scored rows.
#'
#' @param scored_df Scored candidate table from [collect_anchor_scores()].
#'
#' @return A one-row tibble.
#'
#' @export
summarize_anchor_pool <- function(scored_df) {
  # Restrict the summary to admissible rows so the output reflects the final
  # candidate pool actually available to downstream decision support.
  out <- tibble::as_tibble(scored_df) |>
    dplyr::filter(admissible)

  if (nrow(out) == 0) {
    return(tibble::tibble(
      anchor_model_id = NA_character_,
      anchor_species = NA_character_,
      n_admissible = 0L,
      n_same_species_admissible = 0L,
      n_same_family_admissible = 0L,
      nearest_taxonomic_distance_admissible = NA_real_,
      q05_multiplier_admissible = NA_real_,
      q50_multiplier_admissible = NA_real_,
      q95_multiplier_admissible = NA_real_
    ))
  }

  tibble::tibble(
    anchor_model_id = dplyr::first(out$anchor_model_id),
    anchor_species = dplyr::first(out$anchor_species),
    n_admissible = nrow(out),
    n_same_species_admissible = sum(out$overlap_same_species, na.rm = TRUE),
    n_same_family_admissible = sum(out$overlap_same_family, na.rm = TRUE),
    nearest_taxonomic_distance_admissible = min(out$d_species, na.rm = TRUE),
    q05_multiplier_admissible = stats::quantile(out$biomass_multiplier_if_replace, 0.05, na.rm = TRUE),
    q50_multiplier_admissible = stats::quantile(out$biomass_multiplier_if_replace, 0.50, na.rm = TRUE),
    q95_multiplier_admissible = stats::quantile(out$biomass_multiplier_if_replace, 0.95, na.rm = TRUE)
  )
}

#' Evaluate one anchor against the model pool
#'
#' Runs the full anchor-by-anchor applicability workflow, including length-PDF
#' construction, species/study distance calculation, coherence penalties,
#' admissibility screening, and study-cell weight adjustment.
#'
#' @param anchor_row One-row anchor table.
#' @param candidate_models Candidate-model table.
#' @param config Optional JSON path or list with similarity/anchor settings.
#' @param registry_path Optional path to the trait-registry JSON.
#' @param sim_obj Optional prebuilt similarity object from
#'   [prepare_similarity_matrix()].
#' @param dist_obj Optional prebuilt distance object from
#'   [build_gower_distances()].
#' @param candidate_models_scored Optional candidate-model table that already
#'   contains `key_metadata_missing_fraction`.
#'
#' @return A list with `anchor_pdf`, `anchor_sigma`, `model_eval`,
#'   `admissible_df`, `sim_obj`, and `dist_obj`.
#'
#' Resolve one anchor identifier
#'
#' @param anchor_row One-row anchor table.
#'
#' @return Character scalar.
#'
#' @keywords internal
anchor_model_id <- function(anchor_row,
                            config) {
  # Prefer an existing character model identifier when present and otherwise
  # fall back to the primary model ID column.
  id_chr_col <- anchor_field(config, "model_id_chr")
  id_col <- anchor_field(config, "model_id")

  if (id_chr_col %in% names(anchor_row) &&
      length(anchor_row[[id_chr_col]]) > 0 &&
      !is.na(anchor_row[[id_chr_col]][[1]]) &&
      nzchar(as.character(anchor_row[[id_chr_col]][[1]]))) {
    return(as.character(anchor_row[[id_chr_col]][[1]]))
  }

  as.character(anchor_row[[id_col]][[1]])
}

#' Resolve one anchor species label
#'
#' @param anchor_row One-row anchor table.
#'
#' @return Character scalar.
#'
#' @keywords internal
anchor_species_name <- function(anchor_row,
                                config) {
  # Keep anchor species extraction in one place so downstream summaries and
  # diagnostics always use the same label source.
  as.character(anchor_row[[anchor_field(config, "species_name")]][[1]])
}

#' Normalize model identifiers for anchor screening
#'
#' @param candidate_models Candidate-model table.
#'
#' @return A tibble with `model_id_chr` present.
#'
#' @keywords internal
normalize_anchor_ids <- function(candidate_models,
                                 config) {
  # Standardize the model identifier column once before any joins or anchor
  # distance lookups depend on it.
  out <- tibble::as_tibble(candidate_models)

  id_col <- anchor_field(config, "model_id")
  id_chr_col <- anchor_field(config, "model_id_chr")

  if (!id_col %in% names(out)) {
    stop(sprintf("'candidate_models' must contain '%s'.", id_col), call. = FALSE)
  }

  if (!id_chr_col %in% names(out)) {
    out[[id_chr_col]] <- as.character(out[[id_col]])
    return(out)
  }

  out[[id_chr_col]] <- as.character(out[[id_chr_col]])
  fill_idx <- is.na(out[[id_chr_col]]) | !nzchar(out[[id_chr_col]])
  out[[id_chr_col]][fill_idx] <- as.character(out[[id_col]][fill_idx])

  out
}

#' Build the anchor scoring table
#'
#' @param candidate_models Candidate-model table.
#' @param anchor_pdf Anchor length PDF.
#'
#' @return Candidate-model table with `sigma_bs_model_mean`.
#'
#' @keywords internal
build_anchor_table <- function(candidate_models,
                               anchor_pdf,
                               config) {
  # Apply each standardized length-form model to the anchor length PDF so the
  # screening workflow works from one comparable sigma_bs quantity.
  slope_col <- anchor_field(config, "slope")
  intercept_col <- anchor_field(config, "intercept")

  normalize_anchor_ids(candidate_models, config) |>
    dplyr::mutate(
      sigma_bs_model_mean = purrr::map2_dbl(
        .data[[slope_col]],
        .data[[intercept_col]],
        function(s, b) {
          ts_vec <- s * log10(anchor_pdf$length_cm) + b
          phi <- 10^(ts_vec / 10)
          sum(phi * anchor_pdf$f_len, na.rm = TRUE) / sum(anchor_pdf$f_len, na.rm = TRUE)
        }
      )
    )
}

#' Extract one anchor sigma_bs value
#'
#' @param model_eval Candidate scoring table from [build_anchor_table()].
#' @param anchor_id Anchor model identifier.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
anchor_backscatter <- function(model_eval,
                            anchor_id,
                            config) {
  # Pull the anchor's own sigma_bs value from the scored table so all
  # replacement multipliers are expressed relative to the same reference model.
  anchor_sigma <- model_eval |>
    dplyr::filter(.data[[anchor_field(config, "model_id_chr")]] == anchor_id) |>
    dplyr::pull(sigma_bs_model_mean)

  if (length(anchor_sigma) == 0) {
    return(NA_real_)
  }

  as.numeric(anchor_sigma[[1]])
}

#' Add anchor distance columns
#'
#' @param model_eval Anchor scoring table.
#' @param dist_obj Distance object from [build_gower_distances()].
#' @param anchor_id Anchor model identifier.
#'
#' @return A tibble.
#'
#' @keywords internal
add_anchor_distances <- function(model_eval,
                                 dist_obj,
                                 anchor_id,
                                 config) {
  # Reindex the precomputed species and study distances onto the candidate
  # rows so every later scoring step can work off the row-wise table alone.
  model_ids <- model_eval[[anchor_field(config, "model_id_chr")]]
  model_eval$d_species <- as.numeric(dist_obj$species_dist_model[model_ids, anchor_id])
  model_eval$d_study <- as.numeric(dist_obj$study_dist[model_ids, anchor_id])

  model_eval
}

#' Add anchor coherence and kernel terms
#'
#' @param model_eval Anchor scoring table with overlap columns.
#' @param anchor_freq Anchor frequency scalar.
#' @param sim_obj Prepared similarity object.
#' @param config Anchor config list.
#'
#' @return A tibble.
#'
#' @keywords internal
add_anchor_terms <- function(model_eval,
                             anchor_freq,
                             sim_obj,
                             config) {
  # Translate overlap fractions and frequency offsets into coherence penalties,
  # then assemble the row-wise kernel terms used in the final weight.
  tibble::as_tibble(model_eval) |>
    dplyr::mutate(
      length_coherence_distance = dplyr::case_when(
        is.finite(length_overlap_fraction) ~ pmax(0, 1 - pmin(length_overlap_fraction, 1)),
        TRUE ~ NA_real_
      ),
      depth_coherence_distance = dplyr::case_when(
        is.finite(depth_overlap_fraction) ~ pmax(0, 1 - pmin(depth_overlap_fraction, 1)),
        TRUE ~ NA_real_
      ),
      frequency_coherence_distance = calculate_frequency_gap(
        candidate_freq = frequency,
        anchor_freq = anchor_freq,
        freq_span = sim_obj$frequency_span,
        mode = config$frequency_coherence_mode %||% "numeric"
      ),
      kernel_species_term = sim_obj$alpha * sim_obj$k_species * d_species,
      kernel_study_term = (1 - sim_obj$alpha) * sim_obj$k_study * d_study,
      kernel_length_term = dplyr::if_else(
        is.finite(length_coherence_distance),
        config$length_overlap_weight * length_coherence_distance,
        NA_real_
      ),
      kernel_depth_term = dplyr::if_else(
        is.finite(depth_coherence_distance),
        config$depth_overlap_weight * depth_coherence_distance,
        NA_real_
      ),
      kernel_frequency_term = dplyr::if_else(
        is.finite(frequency_coherence_distance),
        config$frequency_coherence_weight * frequency_coherence_distance,
        NA_real_
      )
    )
}

#' Combine anchor distances to final weights
#'
#' @param model_eval Anchor scoring table with kernel terms.
#' @param sim_obj Prepared similarity object.
#' @param config Anchor config list.
#'
#' @return A tibble.
#'
#' @keywords internal
weight_anchor_models <- function(model_eval,
                                 sim_obj,
                                 config) {
  # Collapse all active distance blocks to one normalized distance and one
  # exponential kernel weight per candidate model.
  model_eval <- tibble::as_tibble(model_eval) |>
    dplyr::mutate(
      combined_distance = (
        dplyr::coalesce(sim_obj$alpha * d_species, 0) +
          dplyr::coalesce((1 - sim_obj$alpha) * d_study, 0) +
          dplyr::coalesce(config$length_overlap_weight * length_coherence_distance, 0) +
          dplyr::coalesce(config$depth_overlap_weight * depth_coherence_distance, 0) +
          dplyr::coalesce(config$frequency_coherence_weight * frequency_coherence_distance, 0)
      ) / (
        dplyr::if_else(is.finite(d_species), sim_obj$alpha, 0) +
          dplyr::if_else(is.finite(d_study), 1 - sim_obj$alpha, 0) +
          dplyr::if_else(is.finite(length_coherence_distance), config$length_overlap_weight, 0) +
          dplyr::if_else(is.finite(depth_coherence_distance), config$depth_overlap_weight, 0) +
          dplyr::if_else(is.finite(frequency_coherence_distance), config$frequency_coherence_weight, 0)
      ),
      w_hybrid_raw = exp(
        -(
          dplyr::coalesce(kernel_species_term, 0) +
            dplyr::coalesce(kernel_study_term, 0) +
            dplyr::coalesce(kernel_length_term, 0) +
            dplyr::coalesce(kernel_depth_term, 0) +
            dplyr::coalesce(kernel_frequency_term, 0)
        )
      )
    )

  # Normalize the raw kernel values only when the anchor pool contains at least
  # one finite positive candidate weight.
  w_sum <- sum(model_eval$w_hybrid_raw, na.rm = TRUE)
  if (!is.finite(w_sum) || w_sum <= 0) {
    model_eval$w_hybrid <- NA_real_
    return(model_eval)
  }

  model_eval$w_hybrid <- model_eval$w_hybrid_raw / w_sum
  model_eval
}

#' Build the admissible anchor pool
#'
#' @param model_eval Full anchor scoring table.
#' @param config Anchor config list.
#'
#' @return A tibble.
#'
#' @keywords internal
build_admissible_pool <- function(model_eval,
                                  config) {
  # Restrict to admissible weighted rows first, then de-duplicate support at
  # the study-cell level before final admissible weights are normalized.
  study_cell_col <- anchor_field(config, "study_cell")
  id_chr_col <- anchor_field(config, "model_id_chr")

  admissible_df <- tibble::as_tibble(model_eval) |>
    dplyr::filter(admissible, is.finite(w_hybrid), w_hybrid > 0, is.finite(biomass_multiplier_if_replace)) |>
    dplyr::mutate(!!study_cell_col := dplyr::coalesce(.data[[study_cell_col]], .data[[id_chr_col]])) |>
    dplyr::arrange(dplyr::desc(w_hybrid))

  if (nrow(admissible_df) == 0) {
    return(
      admissible_df |>
        dplyr::mutate(
          study_cell_n_models = integer(0),
          w_study_adj_raw = numeric(0),
          w_adm = numeric(0),
          cumulative_w_adm = numeric(0),
          support_set = character(0)
        )
    )
  }

  admissible_df |>
    dplyr::group_by(.data[[study_cell_col]]) |>
    dplyr::mutate(
      study_cell_n_models = dplyr::n(),
      w_study_adj_raw = w_hybrid / study_cell_n_models
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(w_study_adj_raw), dplyr::desc(w_hybrid)) |>
    dplyr::mutate(
      w_adm = w_study_adj_raw / sum(w_study_adj_raw, na.rm = TRUE),
      cumulative_w_adm = cumsum(w_adm),
      support_set = dplyr::case_when(
        cumulative_w_adm <= config$core_weight_cutoff ~ "core",
        TRUE ~ "tail"
      )
    )
}

#' Evaluate one anchor against the model pool
#'
#' Runs the full anchor-by-anchor applicability workflow, including length-PDF
#' construction, species/study distance calculation, coherence penalties,
#' admissibility screening, and study-cell weight adjustment.
#'
#' @param anchor_row One-row anchor table.
#' @param candidate_models Candidate-model table.
#' @param config Optional JSON path or list with similarity/anchor settings.
#' @param registry_path Optional path to the trait-registry JSON.
#'
#' @return A list with `anchor_pdf`, `anchor_sigma`, `model_eval`,
#'   `admissible_df`, `sim_obj`, and `dist_obj`.
#'
#' @export
evaluate_anchor_models <- function(anchor_row,
                                   candidate_models,
                                   config = NULL,
                                   registry_path = NULL,
                                   sim_obj = NULL,
                                   dist_obj = NULL,
                                   candidate_models_scored = NULL) {
  # Resolve the similarity prep and distance objects once, then layer the
  # anchor-specific overlap and admissibility diagnostics on top of them.
  if (!is.data.frame(anchor_row) || nrow(anchor_row) != 1) {
    stop("'anchor_row' must be a one-row data frame.", call. = FALSE)
  }
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }

  cfg_user <- read_similarity_config(config)
  # Resolve the anchor defaults directly at the evaluation entry point so the
  # merged config is explicit and local to this function.
  cfg <- merge_cfg(
    list(
      fields = list(
        model_id = "model_id",
        model_id_chr = "model_id_chr",
        species_name = "species_name",
        genus = "genus",
        family = "family",
        order = "order",
        swimbladder = "swimbladder_type",
        fao_area = "fao_area",
        ocean_basin = "ocean_basin",
        equation_form = "equation_form",
        derivation_type = "derivation_type",
        length_min = "study_length_min",
        length_max = "study_length_max",
        length_fallback = "species_length_max",
        depth_min = "study_depth_min",
        depth_max = "study_depth_max",
        frequency = "frequency",
        slope = "slope_len",
        intercept = "intercept_len",
        study_cell = "study_cell_id"
      ),
      min_length_overlap_fraction = 0.25,
      min_depth_overlap_fraction = 0.25,
      missing_key_metadata_max_fraction = 0.25,
      length_overlap_weight = 2,
      depth_overlap_weight = 3,
      frequency_coherence_weight = 2,
      frequency_coherence_mode = "numeric",
      core_weight_cutoff = 0.80
    ),
    cfg_user
  )

  # Reuse prebuilt similarity and distance objects when the caller already
  # computed them once for a full anchor loop. Fall back to local construction
  # only for standalone anchor evaluation.
  if (is.null(sim_obj)) {
    sim_obj <- prepare_similarity_matrix(
      candidate_models = candidate_models,
      species_traits = cfg_user$species_traits %||% NULL,
      study_traits = cfg_user$study_traits %||% NULL,
      alpha = cfg_user$alpha %||% NULL,
      k_species = cfg_user$k_species %||% NULL,
      k_study = cfg_user$k_study %||% NULL,
      config = cfg_user,
      registry_path = registry_path,
      seed = cfg_user$seed %||% NULL
    )
  }
  if (is.null(dist_obj)) {
    dist_obj <- build_gower_distances(sim_obj)
  }

  # Reuse the model-level missingness screen when it was already computed
  # outside the anchor loop.
  if (is.null(candidate_models_scored)) {
    candidate_models <- screen_missing_metadata(
      candidate_models = sim_obj$candidate_models,
      key_cols = unique(c(sim_obj$species_traits, sim_obj$study_traits))
    )
  } else {
    candidate_models <- tibble::as_tibble(candidate_models_scored)
  }

  anchor_id <- anchor_model_id(anchor_row, cfg)
  anchor_pdf <- build_anchor_density(anchor_row, cfg)
  anchor_freq <- suppressWarnings(as.numeric(anchor_row[[anchor_field(cfg, "frequency")]][[1]]))

  # Score every candidate model at the anchor PDF, then derive the anchor's own
  # sigma_bs value from that common scoring table.
  model_eval <- build_anchor_table(
    candidate_models = candidate_models,
    anchor_pdf = anchor_pdf,
    config = cfg
  )
  anchor_sigma <- anchor_backscatter(model_eval, anchor_id, cfg)

  # Add distances, replacement multipliers, overlap fields, coherence terms,
  # and final kernel weights in distinct steps so each block stays readable.
  model_eval <- add_anchor_distances(
    model_eval = model_eval,
    dist_obj = dist_obj,
    anchor_id = anchor_id,
    config = cfg
  ) |>
    dplyr::mutate(
      biomass_multiplier_if_replace = dplyr::if_else(
        is.finite(anchor_sigma) & anchor_sigma > 0 &
          is.finite(sigma_bs_model_mean) & sigma_bs_model_mean > 0,
        anchor_sigma / sigma_bs_model_mean,
        NA_real_
      )
    ) |>
    add_anchor_overlap(anchor_row = anchor_row, config = cfg) |>
    add_anchor_terms(
      anchor_freq = anchor_freq,
      sim_obj = sim_obj,
      config = cfg
    ) |>
    weight_anchor_models(
      sim_obj = sim_obj,
      config = cfg
    ) |>
    apply_anchor_gates(anchor_row = anchor_row, config = cfg)

  admissible_df <- build_admissible_pool(
    model_eval = model_eval,
    config = cfg
  )

  list(
    anchor_pdf = anchor_pdf,
    anchor_sigma = anchor_sigma,
    model_eval = model_eval,
    admissible_df = admissible_df,
    sim_obj = sim_obj,
    dist_obj = dist_obj
  )
}

#' Evaluate a reference anchor set
#'
#' Evaluates every reference anchor against the candidate-model pool and
#' returns the same scored, overlap, gate, and summary tables that the old
#' applicability loop produced, but without any file writes.
#'
#' @param reference_anchors Anchor table, typically from
#'   [set_reference_anchors()].
#' @param candidate_models Candidate-model table.
#' @param config Optional JSON path or list with similarity/anchor settings.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#' @param registry_path Optional path to the trait-registry JSON.
#'
#' @return A list containing per-anchor results plus bound score/summary tables.
#'
#' @export
evaluate_anchor_set <- function(reference_anchors,
                                candidate_models,
                                config = NULL,
                                cache_path = NULL,
                                refresh = FALSE,
                                registry_path = NULL) {
  # Validate cache control and input tables before entering the anchor loop.
  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.data.frame(reference_anchors) || nrow(reference_anchors) == 0) {
    stop("'reference_anchors' must be a non-empty data frame.", call. = FALSE)
  }
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Inline the anchor defaults here as well so the cross-anchor evaluator does
  # not rely on a separate helper just to seed config values.
  cfg <- merge_cfg(
    list(
      fields = list(
        model_id = "model_id",
        model_id_chr = "model_id_chr",
        species_name = "species_name",
        genus = "genus",
        family = "family",
        order = "order",
        swimbladder = "swimbladder_type",
        fao_area = "fao_area",
        ocean_basin = "ocean_basin",
        equation_form = "equation_form",
        derivation_type = "derivation_type",
        length_min = "study_length_min",
        length_max = "study_length_max",
        length_fallback = "species_length_max",
        depth_min = "study_depth_min",
        depth_max = "study_depth_max",
        frequency = "frequency",
        slope = "slope_len",
        intercept = "intercept_len",
        study_cell = "study_cell_id"
      ),
      min_length_overlap_fraction = 0.25,
      min_depth_overlap_fraction = 0.25,
      missing_key_metadata_max_fraction = 0.25,
      length_overlap_weight = 2,
      depth_overlap_weight = 3,
      frequency_coherence_weight = 2,
      frequency_coherence_mode = "numeric",
      core_weight_cutoff = 0.80
    ),
    read_similarity_config(config)
  )
  all_scores <- list()
  all_overlap <- list()
  all_gates <- list()
  all_summary <- list()
  anchor_results <- list()

  # Build the similarity object, distance matrices, and model-level
  # missingness screen once for the full anchor set so they are not rebuilt for
  # every anchor in the loop below.
  sim_obj <- prepare_similarity_matrix(
    candidate_models = candidate_models,
    species_traits = cfg$species_traits %||% NULL,
    study_traits = cfg$study_traits %||% NULL,
    alpha = cfg$alpha %||% NULL,
    k_species = cfg$k_species %||% NULL,
    k_study = cfg$k_study %||% NULL,
    config = cfg,
    registry_path = registry_path,
    seed = cfg$seed %||% NULL
  )
  dist_obj <- build_gower_distances(sim_obj)
  candidate_models_scored <- screen_missing_metadata(
    candidate_models = sim_obj$candidate_models,
    key_cols = unique(c(sim_obj$species_traits, sim_obj$study_traits))
  )

  # Evaluate every reference anchor independently and retain both the per-anchor
  # tables and the bound cross-anchor summaries.
  for (i in seq_len(nrow(reference_anchors))) {
    anchor_row <- reference_anchors[i, , drop = FALSE]
    anchor_id <- anchor_model_id(anchor_row, cfg)
    anchor_species <- anchor_species_name(anchor_row, cfg)

    eval_obj <- evaluate_anchor_models(
      anchor_row = anchor_row,
      candidate_models = candidate_models,
      config = cfg,
      registry_path = registry_path,
      sim_obj = sim_obj,
      dist_obj = dist_obj,
      candidate_models_scored = candidate_models_scored
    )

    scored <- collect_anchor_scores(eval_obj, anchor_row, cfg)
    ranked <- rank_anchor_models(eval_obj)
    overlap <- summarize_anchor_overlap(eval_obj$admissible_df) |>
      dplyr::mutate(anchor_model_id = anchor_id, anchor_species = anchor_species)
    gates <- summarize_gate_counts(scored, anchor_row, cfg)
    summary <- summarize_anchor_pool(scored)

    anchor_results[[anchor_id]] <- list(
      anchor = anchor_row,
      evaluation = eval_obj,
      scored = scored,
      ranked = ranked,
      overlap = overlap,
      gates = gates,
      summary = summary
    )

    all_scores[[length(all_scores) + 1]] <- scored
    all_overlap[[length(all_overlap) + 1]] <- overlap
    all_gates[[length(all_gates) + 1]] <- gates
    all_summary[[length(all_summary) + 1]] <- summary
  }

  result <- list(
    anchors = anchor_results,
    all_scores = dplyr::bind_rows(all_scores),
    all_overlap = dplyr::bind_rows(all_overlap),
    all_gates = dplyr::bind_rows(all_gates),
    anchor_summary = dplyr::bind_rows(all_summary)
  )

  # Cache the full in-memory result so diagnostics can be reused without
  # re-running the full anchor-by-anchor screen.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  result
}
