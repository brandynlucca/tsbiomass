#' Prepare merged study and species traits
#'
#' Combines a species-level trait table with a study-level TS table, derives
#' midpoint and range fields where possible, and returns one enriched study-row
#' table. The study table is preserved as the row-level backbone, and any
#' species-trait columns that would collide with study-trait names are prefixed
#' with `species_` before the join.
#'
#' @param species_db Species-level trait table, typically produced by
#'   [enrich_species_db()].
#' @param study_db Study-level table, typically produced by [read_tsl_table()].
#' @param cache_path Optional path to an `.rds` cache file. When supplied, the
#'   function reads from that cache when it already exists and `refresh` is
#'   `FALSE`, and writes the prepared output before returning.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache file and
#'   rebuild the prepared table.
#' @param registry_path Optional path to a trait-registry JSON file. When
#'   `NULL`, the installed package registry is used.
#' @param missing_tokens Character vector of placeholder values that should be
#'   treated as missing in addition to `NA`.
#'
#' @return A tibble containing the full study table plus joined species traits.
#'
#' @examples
#' \dontrun{
#' prepare_traits(
#'   species_db = enrich_species_db(...),
#'   study_db = read_tsl_table("fishery_survey_tsl.xlsx")
#' )
#' }
#'
#' @export
prepare_traits <- function(species_db,
                        study_db,
                        cache_path = NULL,
                        refresh = FALSE,
                        registry_path = NULL,
                        missing_tokens = c("-9999")) {
  # Validate the core inputs and cache controls before any registry or merge
  # work begins.
  if (!is.data.frame(species_db)) {
    stop("'species_db' must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.data.frame(study_db)) {
    stop("'study_db' must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1)) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }

  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.character(missing_tokens)) {
    stop("'missing_tokens' must be a character vector.", call. = FALSE)
  }

  # Reuse an existing cache only when the caller explicitly supplied one and
  # requested non-refresh behavior.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Read the registry once so both the species and study trait handling follow
  # the installed JSON schema exactly.
  registry <- read_trait_registry(registry_path = registry_path)
  species_defs <- registry$species_traits
  study_defs <- registry$study_traits

  species_names <- vapply(species_defs, function(x) x$coded_name, character(1))
  study_names <- vapply(study_defs, function(x) x$coded_name, character(1))

  species_types <- stats::setNames(
    vapply(species_defs, function(x) x$data_type, character(1)),
    species_names
  )
  study_types <- stats::setNames(
    vapply(study_defs, function(x) x$data_type, character(1)),
    study_names
  )

  # Start from tibble inputs and normalize character whitespace up front so the
  # species join key and all categorical fields compare consistently.
  species_tbl <- tibble::as_tibble(species_db) |>
    dplyr::mutate(
      dplyr::across(where(is.character), ~ stringr::str_squish(.x))
    )
  study_tbl <- tibble::as_tibble(study_db) |>
    dplyr::mutate(
      dplyr::across(where(is.character), ~ stringr::str_squish(.x))
    )

  # Build genus/species keys consistently in both tables before any other
  # processing so the final join can work even when only species_name exists.
  if (!all(c("genus", "species") %in% names(species_tbl))) {
    if ("species_name" %in% names(species_tbl)) {
      species_tbl$genus <- stringr::word(species_tbl$species_name, start = 1, end = 1)
      species_tbl$species <- stringr::word(species_tbl$species_name, start = 2, end = 2)
    } else {
      stop("Species table must contain 'genus' and 'species' or 'species_name'.", call. = FALSE)
    }
  }
  if (!all(c("genus", "species") %in% names(study_tbl))) {
    if ("species_name" %in% names(study_tbl)) {
      study_tbl$genus <- stringr::word(study_tbl$species_name, start = 1, end = 1)
      study_tbl$species <- stringr::word(study_tbl$species_name, start = 2, end = 2)
    } else {
      stop("Study table must contain 'genus' and 'species' or 'species_name'.", call. = FALSE)
    }
  }
  if (!"species_name" %in% names(species_tbl)) {
    species_tbl$species_name <- stringr::str_trim(paste(species_tbl$genus, species_tbl$species))
  }
  if (!"species_name" %in% names(study_tbl)) {
    study_tbl$species_name <- stringr::str_trim(paste(study_tbl$genus, study_tbl$species))
  }

  # Standardize the species-trait table to registry-defined types and treat
  # explicit placeholder tokens as missing values. Preserve any additional
  # support fields that survived the species-source merge so later transforms,
  # such as TS-length conversion from length-weight coefficients, can still use
  # them without having to place them in the trait registry.
  support_species_cols <- setdiff(names(species_tbl), species_names)
  keep_species_cols <- c(intersect(names(species_tbl), species_names), support_species_cols)
  species_tbl <- species_tbl[, keep_species_cols, drop = FALSE]
  for (nm in keep_species_cols) {
    if (nm %in% names(species_types) && species_types[[nm]] == "numeric") {
      value_chr <- stringr::str_squish(as.character(species_tbl[[nm]]))
      value_chr[value_chr %in% missing_tokens] <- NA_character_
      value_num <- suppressWarnings(as.numeric(value_chr))
      value_num[!is.finite(value_num)] <- NA_real_
      species_tbl[[nm]] <- value_num
    } else if (nm %in% names(species_types) && species_types[[nm]] == "binary") {
      value_chr <- stringr::str_to_lower(stringr::str_squish(as.character(species_tbl[[nm]])))
      value_chr[value_chr %in% stringr::str_to_lower(missing_tokens)] <- NA_character_
      species_tbl[[nm]] <- dplyr::case_when(
        value_chr %in% c("1", "true", "yes", "y") ~ TRUE,
        value_chr %in% c("0", "false", "no", "n") ~ FALSE,
        TRUE ~ NA
      )
    } else {
      value_chr <- stringr::str_squish(as.character(species_tbl[[nm]]))
      value_chr[value_chr %in% missing_tokens] <- NA_character_
      value_chr[!nzchar(value_chr)] <- NA_character_

      # Support columns outside the registry are kept and typed by simple value
      # inspection so numeric internals like `lw_a_g` and `lw_b` stay numeric.
      numeric_try <- suppressWarnings(as.numeric(value_chr))
      numeric_idx <- !is.na(value_chr)
      if (any(numeric_idx) && all(!is.na(numeric_try[numeric_idx]))) {
        numeric_try[!is.finite(numeric_try)] <- NA_real_
        species_tbl[[nm]] <- numeric_try
      } else {
        species_tbl[[nm]] <- value_chr
      }
    }
  }

  # Collapse duplicate species rows within the species table so the join later
  # has exactly one row per species key.
  species_tbl <- species_tbl |>
    dplyr::mutate(.species_key = paste(genus, species)) |>
    dplyr::filter(!is.na(genus), !is.na(species))

  species_keys <- unique(species_tbl$.species_key)
  species_rows <- vector("list", length(species_keys))
  for (i in seq_along(species_keys)) {
    key <- species_keys[[i]]
    sub <- species_tbl[species_tbl$.species_key == key, , drop = FALSE]
    row <- vector("list", length(keep_species_cols))
    names(row) <- keep_species_cols

    for (nm in keep_species_cols) {
      values <- sub[[nm]]
      values <- values[!is.na(values)]
      if (length(values) == 0) {
        if (nm %in% names(species_types) && species_types[[nm]] == "numeric") {
          row[[nm]] <- NA_real_
        } else if (nm %in% names(species_types) && species_types[[nm]] == "binary") {
          row[[nm]] <- NA
        } else if (is.numeric(sub[[nm]])) {
          row[[nm]] <- NA_real_
        } else if (is.logical(sub[[nm]])) {
          row[[nm]] <- NA
        } else {
          row[[nm]] <- NA_character_
        }
      } else {
        row[[nm]] <- values[[1]]
      }
    }

    species_rows[[i]] <- tibble::as_tibble(row)
  }
  species_tbl <- dplyr::bind_rows(species_rows)

  # Seed any missing canonical study-trait columns so the downstream
  # standardization and derivation steps always see the registry-defined names.
  for (nm in study_names) {
    if (!nm %in% names(study_tbl)) {
      study_tbl[[nm]] <- if (study_types[[nm]] == "numeric") NA_real_ else if (study_types[[nm]] == "binary") NA else NA_character_
    }
  }

  # Standardize the canonical study-trait columns to registry-defined types and
  # treat explicit placeholder tokens as missing values.
  for (nm in study_names[study_names %in% names(study_tbl)]) {
    if (study_types[[nm]] == "numeric") {
      value_chr <- stringr::str_squish(as.character(study_tbl[[nm]]))
      value_chr[value_chr %in% missing_tokens] <- NA_character_
      value_num <- suppressWarnings(as.numeric(value_chr))
      value_num[!is.finite(value_num)] <- NA_real_
      study_tbl[[nm]] <- value_num
    } else if (study_types[[nm]] == "binary") {
      value_chr <- stringr::str_to_lower(stringr::str_squish(as.character(study_tbl[[nm]])))
      value_chr[value_chr %in% stringr::str_to_lower(missing_tokens)] <- NA_character_
      study_tbl[[nm]] <- dplyr::case_when(
        value_chr %in% c("1", "true", "yes", "y") ~ TRUE,
        value_chr %in% c("0", "false", "no", "n") ~ FALSE,
        TRUE ~ NA
      )
    } else {
      value_chr <- stringr::str_squish(as.character(study_tbl[[nm]]))
      value_chr[value_chr %in% missing_tokens] <- NA_character_
      value_chr[!nzchar(value_chr)] <- NA_character_
      study_tbl[[nm]] <- value_chr
    }
  }

  # Derive species midpoint/range fields after the source merge has been
  # resolved so these secondary traits reflect the final chosen values.
  species_tbl <- fill_interval_derivations(
    species_tbl,
    min_col = "depth_min",
    max_col = "depth_max",
    midpoint_col = "depth_midpoint",
    range_col = "depth_range"
  )
  species_tbl <- fill_interval_derivations(
    species_tbl,
    min_col = "temperature_min",
    max_col = "temperature_max",
    midpoint_col = "temperature_midpoint",
    range_col = "temperature_range"
  )
  species_tbl <- fill_interval_derivations(
    species_tbl,
    min_col = "length_min",
    max_col = "length_max",
    midpoint_col = "length_midpoint"
  )

  # Derive study midpoint/range fields after the canonical study columns have
  # been filled from the cleaned TSL-table columns.
  study_tbl <- fill_interval_derivations(
    study_tbl,
    min_col = "length_min",
    max_col = "length_max",
    midpoint_col = "length_midpoint",
    range_col = "length_range"
  )
  study_tbl <- fill_interval_derivations(
    study_tbl,
    min_col = "depth_min",
    max_col = "depth_max",
    midpoint_col = "depth_midpoint",
    range_col = "depth_range"
  )

  # Prefix any species columns that would otherwise overwrite existing study
  # columns when the two tables are joined.
  overlap_cols <- intersect(
    setdiff(names(species_tbl), c("genus", "species")),
    names(study_tbl)
  )
  if (length(overlap_cols) > 0) {
    names(species_tbl)[match(overlap_cols, names(species_tbl))] <- paste0("species_", overlap_cols)
  }

  out <- dplyr::left_join(
    study_tbl,
    species_tbl,
    by = c("genus", "species")
  )

  # Convert compatible TS models into a shared TS-length form after the study
  # and species traits have been merged. This preserves the old workflow's
  # standardized slope/intercept outputs in the post-ingestion step.
  out <- convert_to_length_form(out)

  # Materialize legacy-compatible column names as direct copies of the
  # canonical fields so downstream workflows can consume the literal TSLitReview
  # trait names without any translation logic in similarity code.
  alias_pairs <- c(
    body_shape_norm = "body_shape",
    tax_order = "order",
    tax_genus = "genus",
    temp_midpoint_c = "temperature_midpoint",
    temp_range_c = "temperature_range",
    troph = "trophic",
    length_metric_clean = "length_metric",
    frequency_khz = "frequency"
  )
  for (alias_nm in names(alias_pairs)) {
    source_nm <- alias_pairs[[alias_nm]]
    if (!alias_nm %in% names(out) && source_nm %in% names(out)) {
      out[[alias_nm]] <- out[[source_nm]]
    }
  }

  basin_levels <- c("atlantic", "pacific", "mediterranean", "indian", "southern", "arctic")
  if ("ocean_basin" %in% names(out)) {
    basin_chr <- stringr::str_to_lower(stringr::str_squish(as.character(out$ocean_basin)))
    basin_chr[!nzchar(basin_chr)] <- NA_character_
    for (basin_nm in basin_levels) {
      alias_nm <- paste0("basin_", basin_nm)
      if (!alias_nm %in% names(out)) {
        out[[alias_nm]] <- dplyr::case_when(
          is.na(basin_chr) ~ NA_real_,
          stringr::str_detect(basin_chr, paste0("(^|[;,: ]|\\b)", basin_nm, "($|[;,: ]|\\b)")) ~ 1,
          TRUE ~ 0
        )
      }
    }
  }

  # Build stable per-model identifiers from the prepared canonical fields so
  # downstream code can reference both a simple row ID and a reproducible UID.
  if (!"model_id" %in% names(out)) {
    out$model_id <- seq_len(nrow(out))
  }
  out$model_id_chr <- as.character(out$model_id)

  ref_short <- if ("reference_tsl_short" %in% names(out)) {
    as.character(out$reference_tsl_short)
  } else {
    rep(NA_character_, nrow(out))
  }
  ref_link <- if ("reference_tsl_link" %in% names(out)) {
    as.character(out$reference_tsl_link)
  } else {
    rep(NA_character_, nrow(out))
  }
  misc_chr <- if ("misc_factors" %in% names(out)) {
    as.character(out$misc_factors)
  } else {
    rep(NA_character_, nrow(out))
  }
  slope_chr <- if ("slope" %in% names(out)) {
    suppressWarnings(as.numeric(out$slope))
  } else {
    rep(NA_real_, nrow(out))
  }
  intercept_chr <- if ("intercept" %in% names(out)) {
    suppressWarnings(as.numeric(out$intercept))
  } else {
    rep(NA_real_, nrow(out))
  }

  # Generate the same coarse study-condition labels used in the old workflow,
  # but built from the canonical study fields now present in the prepared table.
  out$freq_label <- dplyr::case_when(
    !is.finite(out$frequency) ~ "unknown",
    TRUE ~ paste0("FREQ_", as.integer(round(out$frequency)))
  )
  out$pressure_corrected_flag <- dplyr::case_when(
    isTRUE(out$pressure_corrected) ~ "yes",
    isFALSE(out$pressure_corrected) ~ "no",
    TRUE ~ "unknown"
  )
  out$equation_form_type <- dplyr::case_when(
    out$equation_form == "20log10_ind" ~ "fixed_slope",
    out$equation_form %in% c("mlog10_ind", "mlog10_kg") ~ "variable_slope",
    TRUE ~ "unknown"
  )

  out$study_reference_id <- stringr::str_replace_all(
    stringr::str_squish(
      stringr::str_to_lower(
        dplyr::coalesce(ref_link, ref_short, paste0("row_", out$model_id_chr))
      )
    ),
    "[^a-z0-9]+",
    "_"
  )

  out$study_cell_id <- paste(
    out$study_reference_id,
    stringr::str_replace_all(
      stringr::str_squish(stringr::str_to_lower(out$species_name)),
      "[^a-z0-9]+",
      "_"
    ),
    dplyr::coalesce(out$freq_label, "unknown"),
    stringr::str_replace_all(
      stringr::str_squish(stringr::str_to_lower(dplyr::coalesce(misc_chr, "default_condition"))),
      "[^a-z0-9]+",
      "_"
    ),
    dplyr::coalesce(out$pressure_corrected_flag, "unknown"),
    ifelse(is.finite(out$length_min), format(round(out$length_min, 3), trim = TRUE), "na"),
    ifelse(is.finite(out$length_max), format(round(out$length_max, 3), trim = TRUE), "na"),
    sep = "__"
  )

  # Build a full model UID that distinguishes different parameterizations
  # within the same study cell.
  out$model_uid <- paste(
    out$study_cell_id,
    stringr::str_replace_all(
      stringr::str_squish(stringr::str_to_lower(dplyr::coalesce(out$equation_form, "unknown"))),
      "[^a-z0-9]+",
      "_"
    ),
    stringr::str_replace_all(
      stringr::str_squish(stringr::str_to_lower(dplyr::coalesce(out$derivation_type, "unknown"))),
      "[^a-z0-9]+",
      "_"
    ),
    ifelse(is.finite(slope_chr), format(round(slope_chr, 6), trim = TRUE), "na"),
    ifelse(is.finite(intercept_chr), format(round(intercept_chr, 6), trim = TRUE), "na"),
    sep = "__"
  )

  # Rename the shared interval traits in the final prepared table so the study
  # and species values are explicitly distinguished without carrying duplicate
  # unprefixed names forward into the analysis table.
  rename_map <- c(
    length_min = "study_length_min",
    length_max = "study_length_max",
    length_midpoint = "study_length_midpoint",
    length_range = "study_length_range",
    depth_min = "study_depth_min",
    depth_max = "study_depth_max",
    depth_midpoint = "study_depth_midpoint",
    depth_range = "study_depth_range"
  )
  for (old_nm in names(rename_map)) {
    new_nm <- rename_map[[old_nm]]
    if (old_nm %in% names(out)) {
      names(out)[match(old_nm, names(out))] <- new_nm
    }
  }

  # Persist the prepared merged table exactly as returned when the caller
  # explicitly supplies a cache path.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cache_path)
  }

  out
}

#' Fill interval midpoint and range fields
#'
#' Derives missing midpoint and range values from a paired minimum and maximum
#' column when those source columns are available in a table.
#'
#' @param tbl Data frame or tibble to update.
#' @param min_col Name of the minimum-value column.
#' @param max_col Name of the maximum-value column.
#' @param midpoint_col Optional name of the midpoint column to fill.
#' @param range_col Optional name of the range column to fill.
#'
#' @return The updated table.
#' @keywords internal
fill_interval_derivations <- function(tbl,
                                      min_col,
                                      max_col,
                                      midpoint_col = NULL,
                                      range_col = NULL) {
  required_cols <- c(min_col, max_col)
  if (!all(required_cols %in% names(tbl))) {
    return(tbl)
  }

  # Fill the interval range only where the source bounds are present and the
  # target range field exists but is still missing.
  if (!is.null(range_col) && range_col %in% names(tbl)) {
    fill_idx <- is.na(tbl[[range_col]]) &
      !is.na(tbl[[min_col]]) &
      !is.na(tbl[[max_col]])
    tbl[[range_col]][fill_idx] <- tbl[[max_col]][fill_idx] - tbl[[min_col]][fill_idx]
  }

  # Fill the interval midpoint only where the source bounds are present and
  # the target midpoint field exists but is still missing.
  if (!is.null(midpoint_col) && midpoint_col %in% names(tbl)) {
    fill_idx <- is.na(tbl[[midpoint_col]]) &
      !is.na(tbl[[min_col]]) &
      !is.na(tbl[[max_col]])
    tbl[[midpoint_col]][fill_idx] <- (tbl[[min_col]][fill_idx] + tbl[[max_col]][fill_idx]) / 2
  }

  tbl
}

#' Convert TS models to a common length form
#'
#' Standardizes compatible TS equations into `slope_len` and `intercept_len`
#' using the legacy workflow's conversion rules. Inverse-form equations flagged
#' in `misc_factors` are excluded from direct conversion.
#'
#' @param tbl Data frame or tibble containing TS-model fields.
#'
#' @return The updated table.
#' @keywords internal
convert_to_length_form <- function(tbl) {
  required_cols <- c("slope", "intercept", "equation_form")
  if (!all(required_cols %in% names(tbl))) {
    return(tbl)
  }

  misc_chr <- if ("misc_factors" %in% names(tbl)) {
    dplyr::coalesce(as.character(tbl$misc_factors), "")
  } else {
    rep("", nrow(tbl))
  }

  lw_a <- if ("lw_a_g" %in% names(tbl)) {
    suppressWarnings(as.numeric(tbl$lw_a_g))
  } else {
    rep(NA_real_, nrow(tbl))
  }
  lw_b <- if ("lw_b" %in% names(tbl)) {
    suppressWarnings(as.numeric(tbl$lw_b))
  } else {
    rep(NA_real_, nrow(tbl))
  }

  lw_a[!is.finite(lw_a) | is.na(lw_a)] <- 0.01
  lw_b[!is.finite(lw_b) | is.na(lw_b)] <- 3.0

  # Identify inverse-length equations first so they can be excluded from direct
  # TS-length conversion.
  tbl$inverse_length_equation_flag <- stringr::str_detect(
    stringr::str_to_lower(misc_chr),
    "reported as\\s*log\\s*l\\s*=|derived from equation.*log\\s*l\\s*="
  )

  slope_num <- suppressWarnings(as.numeric(tbl$slope))
  intercept_num <- suppressWarnings(as.numeric(tbl$intercept))
  eq_form <- stringr::str_to_lower(as.character(tbl$equation_form))

  # Apply the legacy weight-to-length conversion only to the weight-based model
  # form; otherwise preserve the original slope/intercept.
  tbl$slope_len <- dplyr::case_when(
    eq_form %in% c("20log10_ind", "mlog10_ind") ~ slope_num,
    eq_form == "mlog10_kg" ~ slope_num + 10 * lw_b,
    TRUE ~ slope_num
  )

  tbl$intercept_len <- dplyr::case_when(
    eq_form %in% c("20log10_ind", "mlog10_ind") ~ intercept_num,
    eq_form == "mlog10_kg" ~ intercept_num + 10 * (log10(lw_a) - 3),
    TRUE ~ intercept_num
  )

  tbl$slope_len[tbl$inverse_length_equation_flag %in% TRUE] <- NA_real_
  tbl$intercept_len[tbl$inverse_length_equation_flag %in% TRUE] <- NA_real_

  tbl
}
