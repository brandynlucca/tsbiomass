#' Resolve one configured field name
#'
#' @param config Anchor config list.
#' @param key Field-map key to resolve.
#'
#' @return Character scalar.
#'
#' @keywords internal
build_anchor_field <- function(config,
                               key) {
  # Keep all source-column lookups centralized so anchor screening never needs
  # to hard-code trait names in the function body.
  field_nm <- config$fields[[key]]

  if (!is.character(field_nm) || length(field_nm) != 1 || !nzchar(field_nm)) {
    stop(sprintf("Anchor config field '%s' must be a single column name.", key), call. = FALSE)
  }

  field_nm
}

#' Build the default anchor-evaluation config
#'
#' @param config Optional config overrides.
#'
#' @return Anchor-evaluation config list.
#' @keywords internal
default_anchor_config <- function(config = NULL) {
  config_ <- config
  if ((inherits(config_, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config_, Candidates), error = function(e) FALSE)))) {
    config_ <- candidates_config_data(config_)
  }
  if (is.list(config_) &&
    is.list(config_$fields %||% NULL) &&
    all(c("model_id_chr", "species_name", "frequency") %in% names(config_$fields)) &&
    all(c(
      "species_traits",
      "study_traits",
      "similarity_species_traits",
      "similarity_study_traits",
      "frequency_coherence_mode",
      "min_length_overlap_fraction",
      "min_depth_overlap_fraction",
      "missing_key_metadata_max_fraction"
    ) %in% names(config_))) {
    return(config_)
  }
  cfg_data <- if ((inherits(config_, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config_, Configurer), error = function(e) FALSE)))) {
    config_@data
  } else if (
    is.list(config_) &&
      (
        (
          all(c("paths", "execution", "tuning", "policies") %in% names(config_)) &&
            any(c("similarity", "policy", "admissibility") %in% names(config_))
        ) ||
          any(c("similarity", "policy", "admissibility") %in% names(config_))
      )
  ) {
    config_
  } else {
    list()
  }

  similarity_cfg <- cfg_data$similarity %||% list()
  admissibility_cfg <- cfg_data$admissibility %||% list()
  policy_cfg <- similarity_cfg$policy %||% cfg_data$policy %||% list()
  direct_cfg <- if (is.null(config_) || is.list(config_) || (is.character(config_) && length(config_) == 1)) {
    read_similarity_config(config_)
  } else {
    list()
  }
  direct_overrides <- if (is.list(direct_cfg) &&
    !any(c("paths", "execution", "tuning", "policies", "similarity", "policy", "admissibility") %in% names(direct_cfg))) {
    direct_cfg
  } else {
    list()
  }

  frequency_mode <- admissibility_cfg$frequency_mode %||%
    ((admissibility_cfg$coherence %||% list())$frequency %||% list())$mode %||%
    similarity_cfg$frequency_mode %||%
    policy_cfg$frequency_coherence_mode %||%
    "overlap"
  if (isTRUE(admissibility_cfg$exact_frequency %||% similarity_cfg$exact_frequency %||% policy_cfg$require_same_frequency_label %||% FALSE)) {
    frequency_mode <- "literal"
  }

  # direct_overrides$species_traits may carry the similarity weight list
  # (a named numeric list) rather than admissibility gate names.  Strip those
  # two keys so the correctly-typed admissibility character vectors computed
  # below cannot be overwritten by the numeric weight list during the final
  # merge.  The admissibility gate names are stored separately as
  # admissibility_species_traits / admissibility_study_traits.
  safe_overrides <- direct_overrides
  safe_overrides[["species_traits"]] <- NULL
  safe_overrides[["study_traits"]] <- NULL
  if (length(safe_overrides) > 0) {
    safe_overrides <- safe_overrides[!vapply(safe_overrides, is.null, logical(1))]
  }

  merge_cfg(
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
        length_midpoint = "study_length_midpoint",
        depth_min = "study_depth_min",
        depth_max = "study_depth_max",
        frequency = "frequency",
        slope = "slope_len",
        intercept = "intercept_len",
        study_cell = "study_cell_id"
      ),
      species_traits = as.character(admissibility_cfg$species_traits %||% direct_overrides$admissibility_species_traits %||% character(0)),
      study_traits = as.character(admissibility_cfg$study_traits %||% direct_overrides$admissibility_study_traits %||% character(0)),
      similarity_species_traits = similarity_cfg$species_traits %||% direct_overrides$species_traits %||% list(),
      similarity_study_traits = similarity_cfg$study_traits %||% direct_overrides$study_traits %||% list(),
      alpha = similarity_cfg$alpha %||% policy_cfg$alpha %||% NULL,
      k_species = similarity_cfg$kernel_scale %||% similarity_cfg$k_species %||% policy_cfg$k_species %||% NULL,
      k_study = similarity_cfg$kernel_scale %||% similarity_cfg$k_study %||% policy_cfg$k_study %||% NULL,
      min_length_overlap_fraction = admissibility_cfg$length_overlap_min %||%
        ((admissibility_cfg$coherence %||% list())$length %||% list())$min %||%
        policy_cfg$min_length_overlap_fraction %||%
        0.25,
      min_depth_overlap_fraction = admissibility_cfg$depth_overlap_min %||%
        ((admissibility_cfg$coherence %||% list())$depth %||% list())$min %||%
        policy_cfg$min_depth_overlap_fraction %||%
        0.25,
      missing_key_metadata_max_fraction = admissibility_cfg$key_metadata_max %||%
        policy_cfg$missing_key_metadata_max_fraction %||%
        0.25,
      length_overlap_weight = similarity_cfg$length_weight %||%
        policy_cfg$length_overlap_weight %||%
        2,
      depth_overlap_weight = similarity_cfg$depth_weight %||%
        policy_cfg$depth_overlap_weight %||%
        3,
      frequency_coherence_weight = similarity_cfg$frequency_weight %||%
        policy_cfg$frequency_coherence_weight %||%
        2,
      frequency_coherence_mode = frequency_mode,
      frequency_gap = admissibility_cfg$frequency_gap %||%
        ((admissibility_cfg$coherence %||% list())$frequency %||% list())$gap %||%
        similarity_cfg$frequency_gap %||%
        policy_cfg$max_frequency_gap_khz %||%
        NULL,
      exact_frequency = admissibility_cfg$exact_frequency %||%
        similarity_cfg$exact_frequency %||%
        policy_cfg$require_same_frequency_label %||%
        NULL,
      seed = cfg_data$tuning$seed %||% cfg_data$benchmark$seed %||% NULL
    ),
    safe_overrides
  )
}

#' Resolve the key metadata fields used by admissibility screening
#'
#' @param config Anchor config list or staged object.
#'
#' @return Character vector of column names.
#' @keywords internal
admissibility_key_metadata_cols <- function(config = NULL) {
  cfg <- default_anchor_config(config)
  trait_names <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }
    if (is.list(x)) {
      nm <- names(x)
      if (!is.null(nm) && any(!is.na(nm) & nzchar(nm))) {
        return(as.character(nm[!is.na(nm) & nzchar(nm)]))
      }
      return(as.character(unlist(x, use.names = FALSE)))
    }
    as.character(x)
  }

  key_cols <- trait_names(cfg$similarity_study_traits %||% character(0))
  if (length(key_cols) == 0) {
    key_cols <- trait_names(cfg$study_traits %||% character(0))
  }
  if (length(key_cols) == 0) {
    key_cols <- unique(c(
      trait_names(cfg$similarity_species_traits %||% character(0)),
      trait_names(cfg$similarity_study_traits %||% character(0))
    ))
  }
  if (length(key_cols) == 0) {
    key_cols <- unique(c(
      trait_names(cfg$species_traits %||% character(0)),
      trait_names(cfg$study_traits %||% character(0))
    ))
  }

  length_min <- build_anchor_field(cfg, "length_min")
  length_max <- build_anchor_field(cfg, "length_max")
  depth_min <- build_anchor_field(cfg, "depth_min")
  depth_max <- build_anchor_field(cfg, "depth_max")
  freq_col <- build_anchor_field(cfg, "frequency")

  min_length_overlap <- suppressWarnings(as.numeric(cfg$min_length_overlap_fraction %||% NA_real_))
  min_depth_overlap <- suppressWarnings(as.numeric(cfg$min_depth_overlap_fraction %||% NA_real_))
  frequency_mode <- stringr::str_to_lower(
    stringr::str_squish(as.character(cfg$frequency_coherence_mode %||% "overlap"))
  )[[1]]

  if (is.finite(min_length_overlap)) {
    key_cols <- c(key_cols, length_min, length_max)
  }
  if (is.finite(min_depth_overlap)) {
    key_cols <- c(key_cols, depth_min, depth_max)
  }
  if (!identical(frequency_mode, "none")) {
    key_cols <- c(key_cols, freq_col)
  }

  unique(key_cols[!is.na(key_cols) & nzchar(key_cols)])
}

#' Validate whether a stored admissibility bundle matches the current gate logic
#'
#' @param admissibility_bundle Stored admissibility result list.
#' @param config Anchor config list or staged object.
#'
#' @return Logical scalar.
#' @keywords internal
admissibility_bundle_is_current <- function(admissibility_bundle,
                                            config = NULL) {
  if (!is.list(admissibility_bundle)) {
    return(FALSE)
  }

  scores_tbl <- tibble::as_tibble(admissibility_bundle$all_scores %||% tibble::tibble())
  if (nrow(scores_tbl) == 0) {
    return(FALSE)
  }

  cfg <- default_anchor_config(config)
  expected_trait_cols <- unique(c(
    as.character(cfg$species_traits %||% character(0)),
    as.character(cfg$study_traits %||% character(0))
  ))
  expected_trait_cols <- expected_trait_cols[!is.na(expected_trait_cols) & nzchar(expected_trait_cols)]
  expected_gate_cols <- if (length(expected_trait_cols) == 0) {
    character(0)
  } else {
    paste0("gate_trait_", expected_trait_cols)
  }
  if (length(expected_gate_cols) > 0 && !all(expected_gate_cols %in% names(scores_tbl))) {
    return(FALSE)
  }

  freq_mode <- stringr::str_to_lower(
    stringr::str_squish(as.character(cfg$frequency_coherence_mode %||% "overlap"))
  )[[1]]
  if (!identical(freq_mode, "none")) {
    if (!"gate_frequency" %in% names(scores_tbl) ||
      !"frequency_coherence_distance" %in% names(scores_tbl)) {
      return(FALSE)
    }
  }

  expected_key_cols <- intersect(admissibility_key_metadata_cols(cfg), names(scores_tbl))
  if ("key_metadata_missing_fraction" %in% names(scores_tbl) &&
    length(expected_key_cols) > 0) {
    has_missing_inputs <- any(vapply(
      expected_key_cols,
      function(col_nm) any(is.na(scores_tbl[[col_nm]])),
      logical(1)
    ))
    if (has_missing_inputs &&
      !any(is.finite(scores_tbl$key_metadata_missing_fraction) & scores_tbl$key_metadata_missing_fraction > 0, na.rm = TRUE)) {
      return(FALSE)
    }
  }

  TRUE
}

#' Build an anchor length PDF
#'
#' Builds a length-density grid from the anchor study length metadata. When a
#' study length interval is available, the returned density is uniform over
#' that interval. When only a midpoint is available, the returned density is a
#' point mass at that midpoint. No species-maximum fallback is used.
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
  # Use only the study-reported anchor length metadata. Missing study lengths
  # remain missing rather than being imputed from species maxima.
  len_min_col <- build_anchor_field(config, "length_min")
  len_max_col <- build_anchor_field(config, "length_max")
  len_mid_col <- build_anchor_field(config, "length_midpoint")

  mins <- suppressWarnings(as.numeric(anchor_row[[len_min_col]]))
  maxs <- suppressWarnings(as.numeric(anchor_row[[len_max_col]]))
  mins <- mins[is.finite(mins)]
  maxs <- maxs[is.finite(maxs)]

  if (length(mins) > 0 && length(maxs) > 0) {
    lmin <- min(pmin(mins, maxs))
    lmax <- max(pmax(mins, maxs))

    if (!is.finite(lmin) || !is.finite(lmax) || lmin <= 0 || lmax <= 0) {
      stop("Anchor length interval must be positive and finite.", call. = FALSE)
    }

    if (lmax > lmin) {
      grid <- seq(lmin, lmax, length.out = n)
      return(tibble::tibble(
        length_cm = grid,
        f_len = rep(1 / length(grid), length(grid))
      ))
    }

    return(tibble::tibble(
      length_cm = lmin,
      f_len = 1
    ))
  }

  mids <- suppressWarnings(as.numeric(anchor_row[[len_mid_col]]))
  mids <- mids[is.finite(mids) & mids > 0]
  if (length(mids) > 0) {
    return(tibble::tibble(
      length_cm = mids[[1]],
      f_len = 1
    ))
  }
  stop(
    "No valid anchor study length interval or midpoint was available.",
    call. = FALSE
  )
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
compute_range_overlap <- function(a_min, a_max, b_min, b_max) {
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

#' Compute vectorized directional interval overlap
#'
#' @param a_min Anchor minimum scalar.
#' @param a_max Anchor maximum scalar.
#' @param b_min Candidate minimum vector.
#' @param b_max Candidate maximum vector.
#'
#' @return Numeric vector.
#'
#' @keywords internal
compute_range_overlap_vec <- function(a_min, a_max, b_min, b_max) {
  a_min_ <- suppressWarnings(as.numeric(a_min))
  a_max_ <- suppressWarnings(as.numeric(a_max))
  a_min_ <- if (length(a_min_) == 0) NA_real_ else a_min_[[1]]
  a_max_ <- if (length(a_max_) == 0) NA_real_ else a_max_[[1]]
  b_min_ <- suppressWarnings(as.numeric(b_min))
  b_max_ <- suppressWarnings(as.numeric(b_max))
  out <- rep(NA_real_, length(b_min_))

  if (!is.finite(a_min_) || !is.finite(a_max_) || a_min_ > a_max_) {
    return(out)
  }

  keep <- is.finite(b_min_) & is.finite(b_max_) & b_min_ <= b_max_
  if (!any(keep)) {
    return(out)
  }

  inter <- pmax(0, pmin(a_max_, b_max_[keep]) - pmax(a_min_, b_min_[keep]))
  a_len <- max(1e-9, a_max_ - a_min_)
  out[keep] <- inter / a_len
  out
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
compute_frequency_gap <- function(candidate_freq,
                                  anchor_freq,
                                  freq_span,
                                  mode = "overlap") {
  # Get valid frequencies
  valid_freqs <- is.finite(candidate_freq) & candidate_freq > 0 & is.finite(anchor_freq) & anchor_freq > 0

  # Handle literal mode early
  if (identical(mode, "literal")) {
    return(
      if (valid_freqs) {
        as.numeric(as.integer(round(candidate_freq)) != as.integer(round(anchor_freq)))
      } else {
        NA_real_
      }
    )
  }

  # Handle standard distance mode
  dplyr::case_when(
    identical(mode, "none") ~ NA_real_,
    valid_freqs ~ pmin(abs(log(candidate_freq / anchor_freq)) / freq_span, 1),
    TRUE ~ NA_real_
  )
}

#' Add key-metadata missingness
#'
#' Appends the fraction of missing selected species/study traits for each model.
#'
#' @param candidate_models Candidate-model table.
#' @param ... Method-specific arguments.
#'
#' @return A tibble.
#'
#' @keywords internal
screen_missing_metadata <- S7::new_generic("screen_missing_metadata", "candidate_models")

#' Add key-metadata missingness for tabular candidate-model inputs
#'
#' @name screen_missing_metadata.default
#' @usage NULL
#' @param candidate_models Candidate-model table.
#' @param key_cols Character vector of key trait columns.
#'
#' @keywords internal
S7::method(screen_missing_metadata, S7::class_any) <- function(candidate_models,
                                                               key_cols) {
  # Normalize the requested key columns first, then compute one row-wise
  # missingness fraction across the retained fields.
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

admissibility_overlap_trait_defs <- local({
  cache <- new.env(parent = emptyenv())

  function(registry_path = NULL) {
    cache_key <- if (is.null(registry_path) || !length(registry_path) || is.na(registry_path[[1]]) || !nzchar(as.character(registry_path[[1]]))) {
      "__default__"
    } else {
      as.character(registry_path[[1]])
    }

    if (exists(cache_key, envir = cache, inherits = FALSE)) {
      return(get(cache_key, envir = cache, inherits = FALSE))
    }

    registry <- read_trait_registry(registry_path = registry_path)
    trait_defs <- c(registry$species_traits %||% list(), registry$study_traits %||% list())
    trait_defs <- trait_defs[!duplicated(vapply(
      trait_defs,
      function(x) as.character(x$coded_name %||% NA_character_)[[1]],
      character(1)
    ))]

    assign(cache_key, trait_defs, envir = cache)
    trait_defs
  }
})

#' Prepare overlap-ready trait columns
#'
#' @param candidate_models Candidate-model table.
#' @param config Anchor config list.
#' @param registry_path Optional trait-registry path.
#'
#' @return Candidate-model tibble with temporary normalized overlap helper
#'   columns prefixed `.ov_`.
#'
#' @keywords internal
prepare_admissibility_overlap_columns <- function(candidate_models,
                                                  config,
                                                  registry_path = NULL) {
  # Normalize the candidate-side trait values once per benchmark/admissibility
  # scenario so anchor-level overlap checks do not keep re-parsing the same
  # strings inside every anchor worker.
  out <- tibble::as_tibble(candidate_models)
  trait_defs <- admissibility_overlap_trait_defs(registry_path = registry_path)
  target_traits <- unique(c(
    as.character(config$species_traits %||% character(0)),
    as.character(config$study_traits %||% character(0)),
    "swimbladder_type",
    "fao_area",
    "ocean_basin",
    "equation_form",
    "derivation_type"
  ))
  target_traits <- target_traits[!is.na(target_traits) & nzchar(target_traits)]
  if (length(target_traits) == 0L) {
    return(out)
  }

  trait_defs <- trait_defs[vapply(
    trait_defs,
    function(x) {
      trait_name <- as.character(x$coded_name %||% NA_character_)[[1]]
      !is.na(trait_name) && trait_name %in% target_traits && trait_name %in% names(out)
    },
    logical(1)
  )]
  if (length(trait_defs) == 0L) {
    return(out)
  }

  parse_set_values <- function(x) {
    x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
    x_chr <- x_chr[!is.na(x_chr) & nzchar(x_chr)]
    if (length(x_chr) == 0L) {
      return(character(0))
    }
    parts <- unlist(strsplit(x_chr, "[;,]", perl = TRUE), use.names = FALSE)
    parts <- stringr::str_squish(parts)
    unique(parts[!is.na(parts) & nzchar(parts)])
  }
  coerce_binary_vec <- function(x) {
    if (is.logical(x) || is.numeric(x) || is.integer(x)) {
      return(as.logical(x))
    }
    x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
    out <- rep(NA, length(x_chr))
    out[x_chr %in% c("1", "true", "yes", "y")] <- TRUE
    out[x_chr %in% c("0", "false", "no", "n")] <- FALSE
    out
  }

  for (trait_def in trait_defs) {
    trait_name <- as.character(trait_def$coded_name %||% NA_character_)[[1]]
    trait_type <- as.character(trait_def$data_type %||% NA_character_)[[1]]
    if (is.na(trait_name) || !nzchar(trait_name) || !trait_name %in% names(out)) {
      next
    }

    if (identical(trait_type, "set")) {
      out[[paste0(".ov_set_", trait_name)]] <- lapply(out[[trait_name]], parse_set_values)
    } else if (identical(trait_type, "binary")) {
      out[[paste0(".ov_bin_", trait_name)]] <- coerce_binary_vec(out[[trait_name]])
    } else if (identical(trait_type, "categorical")) {
      out[[paste0(".ov_cat_", trait_name)]] <- stringr::str_to_lower(
        stringr::str_squish(as.character(out[[trait_name]]))
      )
    }
  }

  out
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
                               config,
                               registry_path = NULL) {
  # Pull the anchor scalars once so the row-wise overlap calculations below can
  # work against simple local values.
  anchor_numeric_scalar <- function(df,
                                    col_nm) {
    if (!is.character(col_nm) ||
      length(col_nm) != 1 ||
      is.na(col_nm) ||
      !nzchar(col_nm) ||
      !col_nm %in% names(df)) {
      return(NA_real_)
    }
    value <- suppressWarnings(as.numeric(df[[col_nm]][[1]]))
    if (length(value) == 0) {
      return(NA_real_)
    }
    value[[1]]
  }
  candidate_numeric_col <- function(df,
                                    col_nm) {
    if (!is.character(col_nm) ||
      length(col_nm) != 1 ||
      is.na(col_nm) ||
      !nzchar(col_nm) ||
      !col_nm %in% names(df)) {
      return(rep(NA_real_, nrow(df)))
    }
    value <- suppressWarnings(as.numeric(df[[col_nm]]))
    if (length(value) == 0) {
      return(rep(NA_real_, nrow(df)))
    }
    value
  }
  species_col <- build_anchor_field(config, "species_name")
  genus_col <- build_anchor_field(config, "genus")
  family_col <- build_anchor_field(config, "family")
  order_col <- build_anchor_field(config, "order")

  len_min_col <- build_anchor_field(config, "length_min")
  len_max_col <- build_anchor_field(config, "length_max")
  dep_min_col <- build_anchor_field(config, "depth_min")
  dep_max_col <- build_anchor_field(config, "depth_max")

  anchor_species <- if (is.character(species_col) &&
    length(species_col) == 1L &&
    !is.na(species_col) &&
    nzchar(species_col) &&
    species_col %in% names(anchor_row)) {
    as.character(anchor_row[[species_col]][[1]])
  } else {
    NA_character_
  }
  anchor_genus <- if (is.character(genus_col) &&
    length(genus_col) == 1L &&
    !is.na(genus_col) &&
    nzchar(genus_col) &&
    genus_col %in% names(anchor_row)) {
    as.character(anchor_row[[genus_col]][[1]])
  } else {
    NA_character_
  }
  anchor_family <- if (is.character(family_col) &&
    length(family_col) == 1L &&
    !is.na(family_col) &&
    nzchar(family_col) &&
    family_col %in% names(anchor_row)) {
    as.character(anchor_row[[family_col]][[1]])
  } else {
    NA_character_
  }
  anchor_order <- if (is.character(order_col) &&
    length(order_col) == 1L &&
    !is.na(order_col) &&
    nzchar(order_col) &&
    order_col %in% names(anchor_row)) {
    as.character(anchor_row[[order_col]][[1]])
  } else {
    NA_character_
  }
  anchor_len_min <- anchor_numeric_scalar(anchor_row, len_min_col)
  anchor_len_max <- anchor_numeric_scalar(anchor_row, len_max_col)
  anchor_dep_min <- anchor_numeric_scalar(anchor_row, dep_min_col)
  anchor_dep_max <- anchor_numeric_scalar(anchor_row, dep_max_col)

  out <- tibble::as_tibble(candidate_models)
  # Guard overlap columns so trait ablations can remove taxonomy fields
  # without breaking anchor-relative screening.
  out$overlap_same_species <- if (is.character(species_col) &&
    length(species_col) == 1L &&
    !is.na(species_col) &&
    nzchar(species_col) &&
    species_col %in% names(out) &&
    species_col %in% names(anchor_row)) {
    !is.na(out[[species_col]]) & out[[species_col]] == anchor_species
  } else {
    rep(FALSE, nrow(out))
  }
  out$overlap_same_genus <- if (is.character(genus_col) &&
    length(genus_col) == 1L &&
    !is.na(genus_col) &&
    nzchar(genus_col) &&
    genus_col %in% names(out) &&
    genus_col %in% names(anchor_row)) {
    !is.na(out[[genus_col]]) & out[[genus_col]] == anchor_genus
  } else {
    rep(FALSE, nrow(out))
  }
  out$overlap_same_family <- if (is.character(family_col) &&
    length(family_col) == 1L &&
    !is.na(family_col) &&
    nzchar(family_col) &&
    family_col %in% names(out) &&
    family_col %in% names(anchor_row)) {
    !is.na(out[[family_col]]) & out[[family_col]] == anchor_family
  } else {
    rep(FALSE, nrow(out))
  }
  out$overlap_same_order <- if (is.character(order_col) &&
    length(order_col) == 1L &&
    !is.na(order_col) &&
    nzchar(order_col) &&
    order_col %in% names(out) &&
    order_col %in% names(anchor_row)) {
    !is.na(out[[order_col]]) & out[[order_col]] == anchor_order
  } else {
    rep(FALSE, nrow(out))
  }
  trait_defs <- admissibility_overlap_trait_defs(registry_path = registry_path)
  target_traits <- unique(c(
    as.character(config$species_traits %||% character(0)),
    as.character(config$study_traits %||% character(0)),
    "swimbladder_type",
    "fao_area",
    "ocean_basin",
    "equation_form",
    "derivation_type"
  ))
  target_traits <- target_traits[!is.na(target_traits) & nzchar(target_traits)]
  if (length(target_traits) > 0L) {
    trait_defs <- trait_defs[vapply(
      trait_defs,
      function(x) {
        trait_name <- as.character(x$coded_name %||% NA_character_)[[1]]
        !is.na(trait_name) && trait_name %in% target_traits
      },
      logical(1)
    )]
  }
  parse_set_values <- function(x) {
    x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
    x_chr <- x_chr[!is.na(x_chr) & nzchar(x_chr)]
    if (length(x_chr) == 0) {
      return(character(0))
    }
    parts <- unlist(strsplit(x_chr, "[;,]", perl = TRUE), use.names = FALSE)
    parts <- stringr::str_squish(parts)
    parts[!is.na(parts) & nzchar(parts)]
  }
  coerce_binary_vec <- function(x) {
    if (is.logical(x) || is.numeric(x) || is.integer(x)) {
      return(as.logical(x))
    }
    x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
    out <- rep(NA, length(x_chr))
    out[x_chr %in% c("1", "true", "yes", "y")] <- TRUE
    out[x_chr %in% c("0", "false", "no", "n")] <- FALSE
    out
  }
  coerce_binary_scalar <- function(x) {
    if (length(x) == 0 || is.na(x[[1]])) {
      return(NA)
    }
    if (is.logical(x) || is.numeric(x) || is.integer(x)) {
      return(as.logical(x[[1]]))
    }
    x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x[[1]])))
    if (x_chr %in% c("1", "true", "yes", "y")) {
      return(TRUE)
    }
    if (x_chr %in% c("0", "false", "no", "n")) {
      return(FALSE)
    }
    NA
  }
  skip_generic_traits <- c("species", "genus", "family", "order")
  for (trait_def in trait_defs) {
    trait_name <- as.character(trait_def$coded_name %||% NA_character_)[[1]]
    trait_type <- as.character(trait_def$data_type %||% NA_character_)[[1]]
    if (is.na(trait_name) || !nzchar(trait_name) || trait_name %in% skip_generic_traits) {
      next
    }
    if (!trait_name %in% names(out) || !trait_name %in% names(anchor_row)) {
      next
    }

    overlap_col <- paste0("overlap_same_", trait_name)
    if (identical(trait_type, "set")) {
      anchor_vals <- parse_set_values(anchor_row[[trait_name]])
      helper_col <- paste0(".ov_set_", trait_name)
      if (helper_col %in% names(out)) {
        out[[overlap_col]] <- vapply(
          out[[helper_col]],
          function(vals) length(intersect(anchor_vals, vals)) > 0,
          logical(1)
        )
      } else {
        out[[overlap_col]] <- vapply(
          out[[trait_name]],
          function(x) {
            vals <- parse_set_values(x)
            length(intersect(anchor_vals, vals)) > 0
          },
          logical(1)
        )
      }
    } else if (identical(trait_type, "binary")) {
      anchor_val <- coerce_binary_scalar(anchor_row[[trait_name]])
      candidate_val <- out[[paste0(".ov_bin_", trait_name)]] %||% coerce_binary_vec(out[[trait_name]])
      out[[overlap_col]] <- !is.na(candidate_val) & !is.na(anchor_val) & candidate_val == anchor_val
    } else if (identical(trait_type, "categorical")) {
      anchor_val <- stringr::str_to_lower(stringr::str_squish(as.character(anchor_row[[trait_name]][[1]])))
      candidate_val <- out[[paste0(".ov_cat_", trait_name)]] %||%
        stringr::str_to_lower(stringr::str_squish(as.character(out[[trait_name]])))
      out[[overlap_col]] <- !is.na(candidate_val) & nzchar(candidate_val) & !is.na(anchor_val) & nzchar(anchor_val) & candidate_val == anchor_val
    }
  }
  out$overlap_same_swimbladder <- if ("overlap_same_swimbladder_type" %in% names(out)) {
    out$overlap_same_swimbladder_type
  } else {
    rep(FALSE, nrow(out))
  }
  out$overlap_same_fao_area <- if ("overlap_same_fao_area" %in% names(out)) {
    out$overlap_same_fao_area
  } else {
    rep(FALSE, nrow(out))
  }
  out$overlap_same_ocean_basin <- if ("overlap_same_ocean_basin" %in% names(out)) {
    out$overlap_same_ocean_basin
  } else {
    rep(FALSE, nrow(out))
  }
  out$overlap_same_equation_form <- if ("overlap_same_equation_form" %in% names(out)) {
    out$overlap_same_equation_form
  } else {
    rep(FALSE, nrow(out))
  }
  out$overlap_same_derivation <- if ("overlap_same_derivation_type" %in% names(out)) {
    out$overlap_same_derivation_type
  } else {
    rep(FALSE, nrow(out))
  }
  out$length_overlap_fraction <- compute_range_overlap_vec(
    a_min = anchor_len_min,
    a_max = anchor_len_max,
    b_min = candidate_numeric_col(out, len_min_col),
    b_max = candidate_numeric_col(out, len_max_col)
  )
  out$depth_overlap_fraction <- compute_range_overlap_vec(
    a_min = anchor_dep_min,
    a_max = anchor_dep_max,
    b_min = candidate_numeric_col(out, dep_min_col),
    b_max = candidate_numeric_col(out, dep_max_col)
  )

  out
}

#' Apply anchor admissibility gates
#'
#' Applies self, configured trait, domain-overlap, optional frequency, and
#' key-metadata gates to the anchor-relative candidate table.
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
                               config,
                               registry_path = NULL) {
  # Apply the gate rules after overlap/coherence fields exist so the final
  # admissibility label is determined from one fully scored table. The active
  # configured trait gates are resolved from the registry at runtime so
  # categorical/binary traits use exact matching and set-valued traits use
  # overlap without re-hard-coding each allowable field here.
  id_col <- build_anchor_field(config, "model_id_chr")
  anchor_id <- build_anchor_model_id(anchor_row, config)

  out <- tibble::as_tibble(candidate_models)
  out$gate_not_self <- out[[id_col]] != anchor_id
  gate_specs <- data.frame(
    trait_name = c(
      as.character(config$species_traits %||% character(0)),
      as.character(config$study_traits %||% character(0))
    ),
    trait_scope = c(
      rep("species", length(config$species_traits %||% character(0))),
      rep("study", length(config$study_traits %||% character(0)))
    ),
    stringsAsFactors = FALSE
  )
  gate_specs <- gate_specs[!is.na(gate_specs$trait_name) & nzchar(gate_specs$trait_name), , drop = FALSE]
  if (nrow(gate_specs) > 0) {
    gate_specs <- gate_specs[!duplicated(gate_specs$trait_name), , drop = FALSE]
  }

  trait_reason <- rep(NA_character_, nrow(out))
  gate_cols <- character(0)
  registry <- NULL
  parse_set_value <- function(x) {
    x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
    if (length(x_chr) == 0 || is.na(x_chr[[1]]) || !nzchar(x_chr[[1]])) {
      return(character(0))
    }
    parts <- unlist(strsplit(x_chr[[1]], "[;,]", perl = TRUE), use.names = FALSE)
    parts <- stringr::str_squish(parts)
    parts[!is.na(parts) & nzchar(parts)]
  }
  coerce_binary_value <- function(x) {
    if (length(x) == 0 || is.na(x[[1]])) {
      return(NA)
    }
    if (is.logical(x) || is.numeric(x) || is.integer(x)) {
      return(as.logical(x[[1]]))
    }
    x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x[[1]])))
    if (x_chr %in% c("1", "true", "yes", "y")) {
      return(TRUE)
    }
    if (x_chr %in% c("0", "false", "no", "n")) {
      return(FALSE)
    }
    NA
  }

  for (row_idx in seq_len(nrow(gate_specs))) {
    trait_name <- gate_specs$trait_name[[row_idx]]
    trait_scope <- gate_specs$trait_scope[[row_idx]]
    if (!trait_name %in% names(out) || !trait_name %in% names(anchor_row)) {
      stop(
        sprintf("Configured admissibility trait '%s' was not present in the candidate-model table.", trait_name),
        call. = FALSE
      )
    }

    gate_col <- paste0("gate_trait_", trait_name)
    fail_reason <- paste0("trait_mismatch:", trait_name)
    overlap_col <- paste0("overlap_same_", trait_name)

    # Reuse precomputed overlap flags whenever they already exist so the gate
    # layer does not re-parse the same trait strings for every anchor.
    if (!identical(trait_name, "frequency") && overlap_col %in% names(out)) {
      gate_pass <- as.logical(out[[overlap_col]])
      gate_pass[is.na(gate_pass)] <- FALSE
      out[[gate_col]] <- gate_pass
      gate_cols <- c(gate_cols, gate_col)
      trait_reason[is.na(trait_reason) & !gate_pass] <- fail_reason
      next
    }

    if (is.null(registry)) {
      registry <- read_similarity_registry(registry_path = registry_path)
    }
    trait_defn <- if (identical(trait_scope, "species")) {
      registry$species_map[[trait_name]]
    } else {
      registry$study_map[[trait_name]]
    }
    trait_type <- trait_defn$data_type %||% "categorical"
    gate_pass <- rep(FALSE, nrow(out))

    if (identical(trait_name, "frequency")) {
      freq_mode <- stringr::str_to_lower(stringr::str_squish(as.character(config$frequency_coherence_mode %||% "overlap")))[[1]]
      candidate_freq <- suppressWarnings(as.numeric(out[[trait_name]]))
      anchor_freq <- suppressWarnings(as.numeric(anchor_row[[trait_name]][[1]]))
      if (identical(freq_mode, "none")) {
        gate_pass[] <- TRUE
      }
      if (identical(freq_mode, "literal")) {
        present_idx <- is.finite(candidate_freq) & candidate_freq > 0 & is.finite(anchor_freq) & anchor_freq > 0
        gate_pass[present_idx] <- as.integer(round(candidate_freq[present_idx])) == as.integer(round(anchor_freq))
      } else if (identical(freq_mode, "overlap")) {
        freq_gap <- suppressWarnings(as.numeric(config$frequency_gap %||% NA_real_))
        if (is.finite(freq_gap) && freq_gap >= 0) {
          present_idx <- is.finite(candidate_freq) & candidate_freq > 0 & is.finite(anchor_freq) & anchor_freq > 0
          gate_pass[present_idx] <- abs(candidate_freq[present_idx] - anchor_freq) <= freq_gap
        }
      }
      fail_reason <- "frequency_nonoverlap"
    } else if (identical(trait_type, "set")) {
      anchor_set <- parse_set_value(anchor_row[[trait_name]][[1]])
      if (length(anchor_set) > 0) {
        for (i in seq_len(nrow(out))) {
          candidate_set <- parse_set_value(out[[trait_name]][[i]])
          if (length(candidate_set) > 0) {
            gate_pass[[i]] <- any(candidate_set %in% anchor_set)
          }
        }
      }
    } else if (identical(trait_type, "binary")) {
      anchor_value <- coerce_binary_value(anchor_row[[trait_name]])
      candidate_value <- vapply(
        as.list(out[[trait_name]]),
        coerce_binary_value,
        logical(1)
      )
      present_idx <- !is.na(candidate_value) & !is.na(anchor_value)
      gate_pass[present_idx] <- candidate_value[present_idx] == anchor_value
    } else {
      anchor_value <- stringr::str_to_lower(stringr::str_squish(as.character(anchor_row[[trait_name]][[1]])))
      candidate_value <- stringr::str_to_lower(stringr::str_squish(as.character(out[[trait_name]])))
      present_idx <- !is.na(candidate_value) & nzchar(candidate_value) & !is.na(anchor_value) & nzchar(anchor_value)
      gate_pass[present_idx] <- candidate_value[present_idx] == anchor_value
    }

    out[[gate_col]] <- gate_pass
    gate_cols <- c(gate_cols, gate_col)
    trait_reason[is.na(trait_reason) & !gate_pass] <- fail_reason
  }

  out$gate_configured_traits <- if (length(gate_cols) == 0) {
    TRUE
  } else {
    Reduce(`&`, out[gate_cols])
  }
  out$gate_frequency <- if ("gate_trait_frequency" %in% names(out)) {
    out$gate_trait_frequency
  } else {
    freq_mode <- stringr::str_to_lower(
      stringr::str_squish(as.character(config$frequency_coherence_mode %||% "overlap"))
    )[[1]]
    freq_col <- build_anchor_field(config, "frequency")
    gate_pass <- if (identical(freq_mode, "none")) rep(TRUE, nrow(out)) else rep(FALSE, nrow(out))
    if (!identical(freq_mode, "none") &&
      freq_col %in% names(out) &&
      freq_col %in% names(anchor_row)) {
      candidate_freq <- suppressWarnings(as.numeric(out[[freq_col]]))
      anchor_freq <- suppressWarnings(as.numeric(anchor_row[[freq_col]][[1]]))
      present_idx <- is.finite(candidate_freq) &
        candidate_freq > 0 &
        is.finite(anchor_freq) &
        anchor_freq > 0
      if (identical(freq_mode, "literal")) {
        gate_pass[present_idx] <- as.integer(round(candidate_freq[present_idx])) == as.integer(round(anchor_freq))
      } else if (identical(freq_mode, "overlap")) {
        freq_gap <- suppressWarnings(as.numeric(config$frequency_gap %||% NA_real_))
        if (is.finite(freq_gap) && freq_gap >= 0) {
          gate_pass[present_idx] <- abs(candidate_freq[present_idx] - anchor_freq) <= freq_gap
        }
      }
    }
    gate_pass
  }
  # Resolve the remaining scalar gate thresholds with direct vectorized checks
  # so this hot path does not spend time in repeated case_when evaluation.
  min_length_overlap <- suppressWarnings(as.numeric(config$min_length_overlap_fraction %||% NA_real_))
  min_depth_overlap <- suppressWarnings(as.numeric(config$min_depth_overlap_fraction %||% NA_real_))
  max_missing_key <- suppressWarnings(as.numeric(config$missing_key_metadata_max_fraction %||% NA_real_))

  out$gate_length_overlap <- if (is.na(min_length_overlap)) {
    rep(TRUE, nrow(out))
  } else {
    !is.na(out$length_overlap_fraction) & out$length_overlap_fraction >= min_length_overlap
  }
  out$gate_depth_overlap <- if (is.na(min_depth_overlap)) {
    rep(TRUE, nrow(out))
  } else {
    !is.na(out$depth_overlap_fraction) & out$depth_overlap_fraction >= min_depth_overlap
  }
  out$gate_missing_key_metadata <- if (is.na(max_missing_key)) {
    rep(TRUE, nrow(out))
  } else {
    is.na(out$key_metadata_missing_fraction) | out$key_metadata_missing_fraction <= max_missing_key
  }
  out$admissible <- out$gate_not_self &
    out$gate_configured_traits &
    out$gate_frequency &
    out$gate_length_overlap &
    out$gate_depth_overlap &
    out$gate_missing_key_metadata
  out$inadmissible_reason <- trait_reason
  out$inadmissible_reason[!out$gate_not_self] <- "self"
  out$inadmissible_reason[is.na(out$inadmissible_reason) & !out$gate_frequency] <- "frequency_nonoverlap"
  out$inadmissible_reason[is.na(out$inadmissible_reason) & !out$gate_length_overlap] <- "length_domain_nonoverlap"
  out$inadmissible_reason[is.na(out$inadmissible_reason) & !out$gate_depth_overlap] <- "depth_domain_nonoverlap"
  out$inadmissible_reason[is.na(out$inadmissible_reason) & !out$gate_missing_key_metadata] <- "metadata_missing_excess"

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
#' @param eval_obj Anchor evaluation object from the internal single-anchor
#'   admissibility screen.
#'
#' @return A tibble.
#'
#' @export
rank_anchor_models <- function(eval_obj) {
  # Order the admissible weighted set by final admissible weight and expose the
  # key diagnostics used in downstream review tables.
  tibble::as_tibble(eval_obj$admissible_df) |>
    dplyr::arrange(dplyr::desc(.data$w_adm)) |>
    dplyr::mutate(rank_by_weight = dplyr::row_number())
}

#' Build anchor scores
#'
#' Produces the full scored candidate table for one anchor, including anchor
#' identifiers and the admissible-support annotations when available.
#'
#' @param eval_obj Anchor evaluation object from the internal single-anchor
#'   admissibility screen.
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
  cfg <- default_anchor_config(config)
  anchor_id <- build_anchor_model_id(anchor_row, cfg)
  anchor_species <- as.character(anchor_row[[build_anchor_field(cfg, "species_name")]][[1]])

  scored <- tibble::as_tibble(eval_obj$model_eval) |>
    dplyr::mutate(anchor_model_id = anchor_id, anchor_species = anchor_species)

  # Reattach the admissible-support columns only for rows that survived the
  # admissibility screen.
  support_cols <- intersect(
    c(
      "model_id_chr", "study_cell_id", "study_cell_n_models",
      "w_combined", "w_study_adj_raw", "w_adm",
      "cumulative_w_adm"
    ),
    names(eval_obj$admissible_df)
  )

  if (length(support_cols) > 0) {
    scored <- scored |>
      dplyr::left_join(
        tibble::as_tibble(eval_obj$admissible_df) |>
          dplyr::select(dplyr::all_of(support_cols)),
        by = intersect(c("model_id_chr", "w_combined"), support_cols)
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
  cfg <- default_anchor_config(config)
  anchor_id <- build_anchor_model_id(anchor_row, cfg)
  anchor_species <- as.character(anchor_row[[build_anchor_field(cfg, "species_name")]][[1]])

  tibble::as_tibble(scored_df) |>
    dplyr::mutate(inadmissible_reason = dplyr::coalesce(.data$inadmissible_reason, "admissible")) |>
    dplyr::count(.data$inadmissible_reason, name = "n_models") |>
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
    dplyr::filter(.data$admissible)

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

#' Resolve one anchor identifier
#'
#' @param anchor_row One-row anchor table.
#'
#' @return Character scalar.
#'
#' @keywords internal
build_anchor_model_id <- function(anchor_row,
                                  config) {
  # Prefer an existing character model identifier when present and otherwise
  # fall back to the primary model ID column.
  id_chr_col <- build_anchor_field(config, "model_id_chr")
  id_col <- build_anchor_field(config, "model_id")

  if (id_chr_col %in% names(anchor_row) &&
    length(anchor_row[[id_chr_col]]) > 0 &&
    !is.na(anchor_row[[id_chr_col]][[1]]) &&
    nzchar(as.character(anchor_row[[id_chr_col]][[1]]))) {
    return(as.character(anchor_row[[id_chr_col]][[1]]))
  }

  as.character(anchor_row[[id_col]][[1]])
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

  id_col <- build_anchor_field(config, "model_id")
  id_chr_col <- build_anchor_field(config, "model_id_chr")

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
  # screening pipeline works from one comparable sigma_bs quantity.
  slope_col <- build_anchor_field(config, "slope")
  intercept_col <- build_anchor_field(config, "intercept")
  out <- normalize_anchor_ids(candidate_models, config)
  slope_vals <- suppressWarnings(as.numeric(out[[slope_col]]))
  intercept_vals <- suppressWarnings(as.numeric(out[[intercept_col]]))
  log_len <- log10(anchor_pdf$length_cm)
  f_len <- as.numeric(anchor_pdf$f_len)
  f_sum <- sum(f_len, na.rm = TRUE)

  if (!is.finite(f_sum) || f_sum <= 0) {
    out$sigma_bs_model_mean <- NA_real_
    return(out)
  }

  weight_vec <- f_len / f_sum
  ts_mat <- tcrossprod(slope_vals, log_len)
  ts_mat <- sweep(ts_mat, 1L, intercept_vals, "+")
  phi_mat <- 10^(ts_mat / 10)
  out$sigma_bs_model_mean <- as.numeric(phi_mat %*% weight_vec)
  out
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
    dplyr::filter(.data[[build_anchor_field(config, "model_id_chr")]] == anchor_id) |>
    dplyr::pull(.data$sigma_bs_model_mean)

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
  model_eval_ <- model_eval
  model_ids <- model_eval_[[build_anchor_field(config, "model_id_chr")]]
  model_eval_$d_species <- as.numeric(dist_obj$species_dist_model[model_ids, anchor_id])
  model_eval_$d_study <- as.numeric(dist_obj$study_dist[model_ids, anchor_id])

  model_eval_
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
  out <- tibble::as_tibble(model_eval)
  len_overlap <- suppressWarnings(as.numeric(out$length_overlap_fraction))
  dep_overlap <- suppressWarnings(as.numeric(out$depth_overlap_fraction))
  d_species <- suppressWarnings(as.numeric(out$d_species))
  d_study <- suppressWarnings(as.numeric(out$d_study))
  alpha <- as.numeric(sim_obj$alpha)
  k_species <- as.numeric(sim_obj$k_species)
  k_study <- as.numeric(sim_obj$k_study)
  len_wt <- as.numeric(config$length_overlap_weight)
  dep_wt <- as.numeric(config$depth_overlap_weight)
  freq_wt <- as.numeric(config$frequency_coherence_weight)

  out$length_coherence_distance <- ifelse(
    is.finite(len_overlap),
    pmax(0, 1 - pmin(len_overlap, 1)),
    NA_real_
  )
  out$depth_coherence_distance <- ifelse(
    is.finite(dep_overlap),
    pmax(0, 1 - pmin(dep_overlap, 1)),
    NA_real_
  )
  out$frequency_coherence_distance <- compute_frequency_gap(
    candidate_freq = out$frequency,
    anchor_freq = anchor_freq,
    freq_span = sim_obj$frequency_span,
    mode = config$frequency_coherence_mode %||% "overlap"
  )
  out$kernel_species_term <- alpha * k_species * d_species
  out$kernel_study_term <- (1 - alpha) * k_study * d_study
  out$kernel_length_term <- ifelse(
    is.finite(out$length_coherence_distance),
    len_wt * out$length_coherence_distance,
    NA_real_
  )
  out$kernel_depth_term <- ifelse(
    is.finite(out$depth_coherence_distance),
    dep_wt * out$depth_coherence_distance,
    NA_real_
  )
  out$kernel_frequency_term <- ifelse(
    is.finite(out$frequency_coherence_distance),
    freq_wt * out$frequency_coherence_distance,
    NA_real_
  )
  out
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
  model_eval_ <- tibble::as_tibble(model_eval)
  alpha <- as.numeric(sim_obj$alpha)
  len_wt <- as.numeric(config$length_overlap_weight)
  dep_wt <- as.numeric(config$depth_overlap_weight)
  freq_wt <- as.numeric(config$frequency_coherence_weight)
  d_species <- suppressWarnings(as.numeric(model_eval_$d_species))
  d_study <- suppressWarnings(as.numeric(model_eval_$d_study))
  len_coh <- suppressWarnings(as.numeric(model_eval_$length_coherence_distance))
  dep_coh <- suppressWarnings(as.numeric(model_eval_$depth_coherence_distance))
  freq_coh <- suppressWarnings(as.numeric(model_eval_$frequency_coherence_distance))
  kernel_species <- suppressWarnings(as.numeric(model_eval_$kernel_species_term))
  kernel_study <- suppressWarnings(as.numeric(model_eval_$kernel_study_term))
  kernel_length <- suppressWarnings(as.numeric(model_eval_$kernel_length_term))
  kernel_depth <- suppressWarnings(as.numeric(model_eval_$kernel_depth_term))
  kernel_frequency <- suppressWarnings(as.numeric(model_eval_$kernel_frequency_term))

  combined_num <- ifelse(is.finite(d_species), alpha * d_species, 0) +
    ifelse(is.finite(d_study), (1 - alpha) * d_study, 0) +
    ifelse(is.finite(len_coh), len_wt * len_coh, 0) +
    ifelse(is.finite(dep_coh), dep_wt * dep_coh, 0) +
    ifelse(is.finite(freq_coh), freq_wt * freq_coh, 0)
  combined_den <- ifelse(is.finite(d_species), alpha, 0) +
    ifelse(is.finite(d_study), 1 - alpha, 0) +
    ifelse(is.finite(len_coh), len_wt, 0) +
    ifelse(is.finite(dep_coh), dep_wt, 0) +
    ifelse(is.finite(freq_coh), freq_wt, 0)
  trait_num <- ifelse(is.finite(d_species), alpha * d_species, 0) +
    ifelse(is.finite(d_study), (1 - alpha) * d_study, 0)
  trait_den <- ifelse(is.finite(d_species), alpha, 0) +
    ifelse(is.finite(d_study), 1 - alpha, 0)

  model_eval_$combined_distance <- ifelse(combined_den > 0, combined_num / combined_den, NA_real_)
  model_eval_$trait_gower_distance <- ifelse(trait_den > 0, trait_num / trait_den, NA_real_)
  model_eval_$w_combined_raw <- exp(-(
    ifelse(is.finite(kernel_species), kernel_species, 0) +
      ifelse(is.finite(kernel_study), kernel_study, 0) +
      ifelse(is.finite(kernel_length), kernel_length, 0) +
      ifelse(is.finite(kernel_depth), kernel_depth, 0) +
      ifelse(is.finite(kernel_frequency), kernel_frequency, 0)
  ))

  # Normalize the raw kernel values only when the anchor pool contains at least
  # one finite positive candidate weight.
  w_sum <- sum(model_eval_$w_combined_raw, na.rm = TRUE)
  if (!is.finite(w_sum) || w_sum <= 0) {
    model_eval_$w_combined <- NA_real_
    return(model_eval_)
  }

  model_eval_$w_combined <- model_eval_$w_combined_raw / w_sum
  model_eval_
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
  study_cell_col <- build_anchor_field(config, "study_cell")
  id_chr_col <- build_anchor_field(config, "model_id_chr")
  model_eval_ <- tibble::as_tibble(model_eval)
  if (!study_cell_col %in% names(model_eval_)) {
    model_eval_[[study_cell_col]] <- if (id_chr_col %in% names(model_eval_)) {
      as.character(model_eval_[[id_chr_col]])
    } else {
      rep(NA_character_, nrow(model_eval_))
    }
  }

  admissible_df <- model_eval_ |>
    dplyr::filter(.data$admissible, is.finite(.data$w_combined), .data$w_combined > 0, is.finite(.data$biomass_multiplier_if_replace)) |>
    dplyr::mutate(!!study_cell_col := dplyr::coalesce(.data[[study_cell_col]], .data[[id_chr_col]])) |>
    dplyr::arrange(dplyr::desc(.data$w_combined))

  if (nrow(admissible_df) == 0) {
    return(
      admissible_df |>
        dplyr::mutate(
          study_cell_n_models = integer(0),
          w_study_adj_raw = numeric(0),
          w_adm = numeric(0),
          cumulative_w_adm = numeric(0)
        )
    )
  }

  admissible_df |>
    dplyr::group_by(.data[[study_cell_col]]) |>
    dplyr::mutate(
      study_cell_n_models = dplyr::n(),
      w_study_adj_raw = .data$w_combined / .data$study_cell_n_models
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(.data$w_study_adj_raw), dplyr::desc(.data$w_combined)) |>
    dplyr::mutate(
      w_adm = .data$w_study_adj_raw / sum(.data$w_study_adj_raw, na.rm = TRUE),
      cumulative_w_adm = cumsum(.data$w_adm)
    )
}

#' Screen one anchor against the candidate-model pool
#'
#' Internal single-anchor worker used by [screen_admissibility()]. The public
#' API stays at one function while this helper handles the per-anchor loop body.
#'
#' @param anchor_row One-row anchor table.
#' @param candidate_models Candidate-model table or a [Candidates] object.
#' @param config Optional JSON path or list with similarity/anchor settings.
#' @param registry_path Optional path to the trait-registry JSON.
#' @param sim_obj Optional prebuilt similarity object from
#'   [prepare_similarity_matrix()].
#' @param dist_obj Optional prebuilt distance object from
#'   [build_gower_distances()].
#' @param candidate_models_scored Optional candidate-model table that already
#'   contains `key_metadata_missing_fraction`.
#' @param excluded_model_ids Optional character vector of model IDs that should
#'   be excluded from the donor pool after the anchor's own calibration terms
#'   are derived from the full scored table.
#'
#' @return A list with `anchor_pdf`, `anchor_sigma`, `model_eval`,
#'   `admissible_df`, `sim_obj`, and `dist_obj`.
#'
#' @keywords internal
screen_one_anchor_admissibility <- function(anchor_row,
                                            candidate_models,
                                            config = NULL,
                                            registry_path = NULL,
                                            sim_obj = NULL,
                                            dist_obj = NULL,
                                            candidate_models_scored = NULL,
                                            excluded_model_ids = NULL) {
  # Resolve the similarity prep and distance objects once, then layer the
  # anchor-specific overlap and admissibility diagnostics on top of them.
  if (!is.data.frame(anchor_row) || nrow(anchor_row) != 1) {
    stop("'anchor_row' must be a one-row data frame.", call. = FALSE)
  }
  candidates_obj <- if ((inherits(candidate_models, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidate_models, Candidates), error = function(e) FALSE)))) candidate_models else NULL
  if (!is.null(candidates_obj)) {
    # Reuse object-stored similarity state whenever it is already present and
    # the caller did not explicitly override it with `sim_obj`. This keeps the
    # staged pipeline from rebuilding the same matrices inside anchor loops.
    if (is.null(sim_obj) && length(candidates_obj@similarity_matrix) > 0) {
      sim_obj <- candidates_obj@similarity_matrix
    }
    # The same reuse rule applies to the Gower-distance bundle so policy
    # prediction and admissibility screens can share the precomputed object.
    if (is.null(dist_obj) && length(candidates_obj@gower_distances) > 0) {
      dist_obj <- candidates_obj@gower_distances
    }
    candidate_models <- tibble::as_tibble(candidates_obj@candidate_models)
  }
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }

  cfg <- default_anchor_config(config)
  anchor_id <- build_anchor_model_id(anchor_row, cfg)
  model_id_col <- build_anchor_field(cfg, "model_id_chr")

  # For held-out prediction screens, temporarily add the anchor row back into
  # the local scoring pool so self-backscatter and anchor-relative distances can
  # be computed against the same matrix basis as the donor rows.
  if (model_id_col %in% names(candidate_models)) {
    pool_ids <- as.character(candidate_models[[model_id_col]])
    if (!anchor_id %in% pool_ids) {
      candidate_models <- dplyr::bind_rows(
        tibble::as_tibble(candidate_models),
        tibble::as_tibble(anchor_row)
      )
      candidate_models_scored <- NULL
      sim_obj <- NULL
      dist_obj <- NULL
    }
  }

  # Reuse prebuilt similarity and distance objects when the caller already
  # computed them once for a larger admissibility screen. Fall back to local
  # construction only for standalone one-anchor screening.
  if (is.null(sim_obj)) {
    sim_obj <- prepare_similarity_matrix(
      candidate_models = candidate_models,
      species_traits = cfg$similarity_species_traits %||% NULL,
      study_traits = cfg$similarity_study_traits %||% NULL,
      alpha = cfg$alpha %||% NULL,
      k_species = cfg$k_species %||% NULL,
      k_study = cfg$k_study %||% NULL,
      config = cfg,
      registry_path = registry_path,
      seed = cfg$seed %||% NULL
    )
  }
  if (is.null(dist_obj)) {
    dist_obj <- build_gower_distances(sim_obj)
  }

  # Reuse the model-level missingness screen when it was already computed
  # outside the anchor loop.
  if (is.null(candidate_models_scored)) {
    candidate_models_prepared <- tibble::as_tibble(sim_obj$candidate_models %||% candidate_models)
    candidate_models <- screen_missing_metadata(
      candidate_models = candidate_models_prepared,
      key_cols = admissibility_key_metadata_cols(cfg)
    )
    candidate_models <- prepare_admissibility_overlap_columns(
      candidate_models = candidate_models,
      config = cfg,
      registry_path = registry_path
    )
  } else {
    candidate_models <- tibble::as_tibble(candidate_models_scored)
  }

  anchor_pdf <- build_anchor_density(anchor_row, cfg)
  anchor_freq <- suppressWarnings(as.numeric(anchor_row[[build_anchor_field(cfg, "frequency")]][[1]]))

  # Score every candidate model at the anchor PDF, then derive the anchor's own
  # sigma_bs value from that common scoring table.
  model_eval <- build_anchor_table(
    candidate_models = candidate_models,
    anchor_pdf = anchor_pdf,
    config = cfg
  )
  anchor_sigma <- anchor_backscatter(model_eval, anchor_id, cfg)

  # Keep the anchor rows available long enough to derive the reference
  # backscatter term, then drop any declared reference anchors before donor
  # distances, weights, and admissibility are computed.
  if (is.null(excluded_model_ids) &&
    !is.null(candidates_obj) &&
    nrow(tibble::as_tibble(candidates_obj@reference_anchors)) > 0) {
    excluded_model_ids <- if ("model_id_chr" %in% names(candidates_obj@reference_anchors)) {
      as.character(candidates_obj@reference_anchors$model_id_chr)
    } else if ("model_id" %in% names(candidates_obj@reference_anchors)) {
      as.character(candidates_obj@reference_anchors$model_id)
    } else {
      character(0)
    }
  }
  excluded_model_ids <- unique(as.character(excluded_model_ids %||% character(0)))
  excluded_model_ids <- excluded_model_ids[!is.na(excluded_model_ids) & nzchar(excluded_model_ids)]
  if (length(excluded_model_ids) > 0) {
    model_eval <- model_eval |>
      dplyr::filter(!(.data[[build_anchor_field(cfg, "model_id_chr")]] %in% excluded_model_ids))
  }

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
          is.finite(.data$sigma_bs_model_mean) & .data$sigma_bs_model_mean > 0,
        anchor_sigma / .data$sigma_bs_model_mean,
        NA_real_
      )
    ) |>
    add_anchor_overlap(anchor_row = anchor_row, config = cfg, registry_path = registry_path) |>
    add_anchor_terms(
      anchor_freq = anchor_freq,
      sim_obj = sim_obj,
      config = cfg
    ) |>
    weight_anchor_models(
      sim_obj = sim_obj,
      config = cfg
    ) |>
    apply_anchor_gates(anchor_row = anchor_row, config = cfg, registry_path = registry_path)

  # Drop temporary overlap-prep helpers before caching or returning the
  # scored table so downstream objects only carry the public diagnostics.
  helper_cols <- grep("^\\.ov_(set|bin|cat)_", names(model_eval), value = TRUE)
  if (length(helper_cols) > 0L) {
    model_eval <- model_eval[, !names(model_eval) %in% helper_cols, drop = FALSE]
  }

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

#' Screen admissibility across a reference-anchor set
#'
#' Screens every reference anchor against the candidate-model pool and returns
#' the bound score, overlap, gate, and anchor-summary tables.
#'
#' @param reference_anchors Anchor table, typically from
#'   [set_reference_anchors()]. When `candidate_models` is a [Candidates]
#'   object, this may be left `NULL` to use `candidate_models@reference_anchors`.
#' @param candidate_models Candidate-model table or a [Candidates] object.
#' @param config Optional JSON path or list with similarity/anchor settings.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#' @param progress Logical scalar controlling stage messages.
#' @param registry_path Optional path to the trait-registry JSON.
#'
#' @return When `candidate_models` is a [Candidates] object, returns that
#'   object with the admissibility-screen result stored in `@admissibility`.
#'   Otherwise, returns a list containing per-anchor results plus bound
#'   score/summary tables.
#'
#' @examples
#' \dontrun{
#' candidates <- as_candidates(list(
#'   study = list(path = "input.xlsx"),
#'   anchors = list(selector = list(regional_body = "SWFSC"))
#' ))
#' candidates <- prepare_similarity_matrix(candidates)
#' candidates <- build_gower_distances(candidates)
#' candidates <- screen_admissibility(candidate_models = candidates)
#' candidates@admissibility$anchor_summary
#' }
#'
#' @export
screen_admissibility <- function(reference_anchors = NULL,
                                 candidate_models,
                                 config = NULL,
                                 cache_path = NULL,
                                 refresh = NULL,
                                 progress = NULL,
                                 registry_path = NULL) {
  reference_anchors_ <- reference_anchors
  candidate_models_ <- if (missing(candidate_models)) NULL else candidate_models
  config_ <- config
  cache_path_ <- cache_path
  refresh_ <- refresh
  progress_ <- progress
  if (missing(candidate_models)) {
    if (inherits(reference_anchors_, "S7_object") &&
      exists("Alchemist", inherits = TRUE) &&
      isTRUE(tryCatch(S7::S7_inherits(reference_anchors_, Alchemist), error = function(e) FALSE))) {
      return(.screen_admissibility_alchemist(
        alchemist = reference_anchors_,
        config = config_,
        cache_path = cache_path_,
        refresh = refresh_,
        progress = progress_,
        registry_path = registry_path
      ))
    }
    if ((inherits(reference_anchors_, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(reference_anchors_, Candidates), error = function(e) FALSE)))) {
      candidate_models_ <- reference_anchors_
      reference_anchors_ <- NULL
    } else {
      stop(
        "'candidate_models' must be supplied unless the first argument is a `Candidates` object.",
        call. = FALSE
      )
    }
  }

  if (inherits(candidate_models_, "S7_object") &&
    exists("Alchemist", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(candidate_models_, Alchemist), error = function(e) FALSE))) {
    return(.screen_admissibility_alchemist(
      alchemist = candidate_models_,
      config = config_,
      cache_path = cache_path_,
      refresh = refresh_,
      progress = progress_,
      registry_path = registry_path
    ))
  }

  candidates_obj <- if ((inherits(candidate_models_, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidate_models_, Candidates), error = function(e) FALSE)))) candidate_models_ else NULL
  if (!is.null(candidates_obj)) {
    if (is.null(config_)) {
      config_ <- candidates_config_data(candidates_obj)
    }
    if (is.null(reference_anchors_)) {
      reference_anchors_ <- candidates_obj@reference_anchors
    }
    candidate_models_ <- tibble::as_tibble(candidates_obj@candidate_models)
  }
  cfg_data <- config_data(config_)
  cache_path_ <- cache_path_ %||% config_value(cfg_data, "cache_path", sections = "admissibility") %||% NULL
  refresh_ <- refresh_ %||% config_value(cfg_data, "refresh", sections = "admissibility") %||% FALSE
  progress_ <- progress_ %||% config_value(cfg_data, "progress", sections = "admissibility") %||% FALSE
  if (!is.null(cache_path_) &&
    (!is.character(cache_path_) || length(cache_path_) != 1 || !nzchar(cache_path_))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh_) || length(refresh_) != 1 || is.na(refresh_)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.data.frame(reference_anchors_) || nrow(reference_anchors_) == 0) {
    stop("'reference_anchors' must be a non-empty data frame.", call. = FALSE)
  }
  if (!is.data.frame(candidate_models_)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.null(cache_path_) && file.exists(cache_path_) && !refresh_) {
    report_progress(progress_, "Loading cached anchor admissibility from ", cache_path_, ".")
    cached_result <- readRDS(cache_path_)
    if (!admissibility_bundle_is_current(cached_result, config_)) {
      report_progress(
        progress_,
        "Cached anchor admissibility at ",
        cache_path_,
        " is stale under the current admissibility logic; rebuilding."
      )
    } else {
      if (!is.null(candidates_obj)) {
        return(candidates_with_admissibility(candidates_obj, cached_result))
      }
      return(cached_result)
    }
  }

  cfg <- default_anchor_config(config_)
  report_progress(
    progress,
    "Screening admissibility for ",
    nrow(reference_anchors),
    " reference anchors."
  )
  excluded_model_ids <- if ("model_id_chr" %in% names(reference_anchors)) {
    as.character(reference_anchors$model_id_chr)
  } else if ("model_id" %in% names(reference_anchors)) {
    as.character(reference_anchors$model_id)
  } else {
    character(0)
  }
  excluded_model_ids <- unique(excluded_model_ids[!is.na(excluded_model_ids) & nzchar(excluded_model_ids)])
  all_scores <- list()
  all_overlap <- list()
  all_gates <- list()
  all_summary <- list()
  anchor_results <- list()

  # Build the similarity object, distance matrices, and model-level
  # missingness screen once for the full anchor set so they are not rebuilt for
  # every anchor in the loop below. The similarity object remains the
  # post-gate donor-weighting surface; the admissibility gate traits are
  # configured independently above.
  if (!is.null(candidates_obj) &&
    length(candidates_obj@similarity_matrix) > 0) {
    sim_obj <- candidates_obj@similarity_matrix
  } else {
    sim_obj <- NULL
  }
  if (is.null(sim_obj)) {
    sim_obj <- prepare_similarity_matrix(
      candidate_models = candidate_models_,
      species_traits = cfg$similarity_species_traits %||% NULL,
      study_traits = cfg$similarity_study_traits %||% NULL,
      alpha = cfg$alpha %||% NULL,
      k_species = cfg$k_species %||% NULL,
      k_study = cfg$k_study %||% NULL,
      config = cfg,
      registry_path = registry_path,
      seed = cfg$seed %||% NULL
    )
  }
  if (!is.null(candidates_obj) &&
    length(candidates_obj@gower_distances) > 0 &&
    !is.null(sim_obj) &&
    identical(sim_obj, candidates_obj@similarity_matrix)) {
    dist_obj <- candidates_obj@gower_distances
  } else {
    dist_obj <- build_gower_distances(sim_obj)
  }
  candidate_models_prepared <- tibble::as_tibble(sim_obj$candidate_models %||% candidate_models_)
  candidate_models_scored <- screen_missing_metadata(
    candidate_models = candidate_models_prepared,
    key_cols = admissibility_key_metadata_cols(cfg)
  )
  candidate_models_scored <- prepare_admissibility_overlap_columns(
    candidate_models = candidate_models_scored,
    config = cfg,
    registry_path = registry_path
  )

  # Evaluate every reference anchor independently and retain both the per-anchor
  # tables and the bound cross-anchor summaries.
  for (i in seq_len(nrow(reference_anchors_))) {
    anchor_row <- reference_anchors_[i, , drop = FALSE]
    anchor_id <- build_anchor_model_id(anchor_row, cfg)
    anchor_species <- as.character(anchor_row[[build_anchor_field(cfg, "species_name")]][[1]])

    eval_obj <- screen_one_anchor_admissibility(
      anchor_row = anchor_row,
      candidate_models = candidate_models_,
      config = cfg,
      registry_path = registry_path,
      sim_obj = sim_obj,
      dist_obj = dist_obj,
      candidate_models_scored = candidate_models_scored,
      excluded_model_ids = excluded_model_ids
    )

    scored <- collect_anchor_scores(eval_obj, anchor_row, cfg)
    ranked <- rank_anchor_models(eval_obj)
    overlap <- summarize_anchor_overlap(eval_obj$admissible_df) |>
      dplyr::mutate(anchor_model_id = anchor_id, anchor_species = anchor_species)
    gates <- summarize_gate_counts(scored, anchor_row, cfg)
    summary <- summarize_anchor_pool(scored)

    # Build a compact stored evaluation to avoid duplicating the full dist_obj
    # (three NxN matrices) and sim_obj per anchor. Both are already on
    # @gower_distances and @similarity_matrix and are never read back from the
    # per-anchor evaluation slot. The model_eval column vectors are shared with
    # `scored` rather than copied - R's copy-on-modify preserves this sharing
    # both in-process and in saveRDS (which deduplicates shared objects).
    scored_extra_cols <- c(
      "anchor_model_id", "anchor_species",
      "study_cell_id", "study_cell_n_models",
      "w_study_adj_raw", "w_adm", "cumulative_w_adm"
    )
    stored_eval <- list(
      anchor_pdf = eval_obj$anchor_pdf,
      anchor_sigma = eval_obj$anchor_sigma,
      admissible_df = eval_obj$admissible_df,
      model_eval = scored[, !names(scored) %in% scored_extra_cols, drop = FALSE]
    )

    anchor_results[[anchor_id]] <- list(
      anchor = anchor_row,
      evaluation = stored_eval,
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
  report_progress(progress, "Completed anchor admissibility screening.")

  # Cache the full in-memory result so diagnostics can be reused without
  # re-running the full anchor-by-anchor screen.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  if (!is.null(candidates_obj)) {
    return(candidates_with_admissibility(candidates_obj, result))
  }

  result
}
