#' Normalize the candidate-model similarity inputs
#'
#' @param candidate_models Prepared candidate-model table.
#'
#' @return A tibble with the required identifier and coherence columns present.
#'
#' @keywords internal
normalize_similarity_data <- function(candidate_models) {
  # Validate the incoming table once and add the identifiers/coherence fields
  # that later preparation and scoring helpers assume are present.
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }

  out <- tibble::as_tibble(candidate_models)
  if (!"model_id" %in% names(out)) {
    # Create a simple row-based identifier when the caller has not already
    # supplied a persistent model identifier.
    out$model_id <- seq_len(nrow(out))
  }
  out$model_id_chr <- as.character(out$model_id)

  if (!"species_name" %in% names(out)) {
    if (all(c("genus", "species") %in% names(out))) {
      # Rebuild the species label from genus and species columns so later
      # species-level grouping is stable.
      out$species_name <- stringr::str_squish(paste(out$genus, out$species))
    } else {
      stop(
        "Candidate models must contain 'species_name' or both 'genus' and 'species'.",
        call. = FALSE
      )
    }
  }

  # Canonicalize caller-supplied species labels too so species-profile
  # collapsing and later matrix re-indexing use one stable key format.
  out$species_name <- stringr::str_squish(as.character(out$species_name))
  out$species_name[!nzchar(out$species_name)] <- NA_character_

  # Rename legacy column variants to their canonical trait coded_names so
  # config files can use the registry names regardless of source column naming.
  col_renames <- c(
    species_fao_area = "fao_area"
  )
  for (old_nm in names(col_renames)) {
    new_nm <- col_renames[[old_nm]]
    if (old_nm %in% names(out) && !new_nm %in% names(out)) {
      names(out)[names(out) == old_nm] <- new_nm
    }
  }

  out
}

#' Expand one trait block for similarity preparation
#'
#' @param df Input data frame.
#' @param weight_spec Named numeric trait-weight vector.
#' @param trait_defs Registry definitions for the selected traits.
#'
#' @return A list with `data`, `weights`, and `lookup`.
#'
#' @keywords internal
expand_trait_block <- function(df,
                               weight_spec,
                               trait_defs) {
  # Expand set-valued traits to binary membership columns and coerce numeric,
  # binary, and categorical traits into stable comparison-ready forms.
  expanded_df <- tibble::tibble(.row_id = seq_len(nrow(df)))
  expanded_weights <- numeric(0)
  expanded_lookup <- character(0)

  for (trait_nm in names(weight_spec)) {
    trait_defn <- trait_defs[[trait_nm]]
    trait_type <- trait_defn$data_type %||% "categorical"
    trait_weight <- weight_spec[[trait_nm]]
    raw_val <- df[[trait_nm]]

    if (trait_type == "set") {
      # Expand each allowed set member to its own indicator column so set
      # overlap can be represented explicitly in the prepared matrix.
      allowed_vals <- as.character(unlist(trait_defn$allowed_values %||% character(0)))
      if (length(allowed_vals) == 0) {
        next
      }

      raw_chr <- stringr::str_squish(as.character(raw_val))
      raw_chr[!nzchar(raw_chr)] <- NA_character_
      split_vals <- strsplit(raw_chr, ";", fixed = TRUE)

      for (allowed_val in allowed_vals) {
        col_nm <- paste(trait_nm, allowed_val, sep = "__")
        col_val <- rep(NA_real_, length(raw_chr))
        present_idx <- which(!is.na(raw_chr))
        if (length(present_idx) > 0) {
          # Mark set membership per allowed value while preserving missingness
          # for rows with no original set entry.
          col_val[present_idx] <- vapply(
            split_vals[present_idx],
            function(x) as.numeric(allowed_val %in% stringr::str_squish(x)),
            numeric(1)
          )
        }
        expanded_df[[col_nm]] <- col_val
        expanded_weights[col_nm] <- trait_weight / length(allowed_vals)
        expanded_lookup[col_nm] <- trait_nm
      }
    } else if (trait_type == "numeric") {
      # Numeric traits are coerced once here so later scoring code can assume
      # non-finite values have already been blanked out.
      col_val <- suppressWarnings(as.numeric(raw_val))
      col_val[!is.finite(col_val)] <- NA_real_
      expanded_df[[trait_nm]] <- col_val
      expanded_weights[trait_nm] <- trait_weight
      expanded_lookup[trait_nm] <- trait_nm
    } else if (trait_type == "binary") {
      # Support simple text encodings for binary traits in addition to logical
      # columns already supplied as TRUE/FALSE.
      if (is.character(raw_val)) {
        raw_low <- stringr::str_to_lower(stringr::str_squish(raw_val))
        col_val <- dplyr::case_when(
          raw_low %in% c("1", "true", "yes", "y") ~ TRUE,
          raw_low %in% c("0", "false", "no", "n") ~ FALSE,
          TRUE ~ NA
        )
      } else {
        col_val <- as.logical(raw_val)
      }
      expanded_df[[trait_nm]] <- col_val
      expanded_weights[trait_nm] <- trait_weight
      expanded_lookup[trait_nm] <- trait_nm
    } else {
      # Treat the remaining trait types as categorical strings with trimmed
      # whitespace and explicit missing values for blanks.
      col_val <- stringr::str_squish(as.character(raw_val))
      col_val[!nzchar(col_val)] <- NA_character_
      expanded_df[[trait_nm]] <- col_val
      expanded_weights[trait_nm] <- trait_weight
      expanded_lookup[trait_nm] <- trait_nm
    }
  }

  expanded_df$.row_id <- NULL
  list(data = expanded_df, weights = expanded_weights, lookup = expanded_lookup)
}

#' Collapse one prepared species block to species profiles
#'
#' @param models_df Normalized candidate-model table.
#' @param expanded_df Expanded species trait block.
#'
#' @return A tibble with one row per species.
#'
#' @keywords internal
collapse_species_profiles <- function(models_df,
                                      expanded_df) {
  # Collapse multiple model rows to one species profile so species-level
  # similarity is based on biology rather than publication count.
  species_split <- split(
    seq_len(nrow(models_df)),
    stringr::str_squish(as.character(models_df$species_name))
  )

  out_rows <- vector("list", length(species_split))
  out_names <- names(species_split)

  for (i in seq_along(species_split)) {
    idx <- species_split[[i]]
    sub <- expanded_df[idx, , drop = FALSE]
    row <- vector("list", ncol(sub))
    names(row) <- names(sub)

    # Keep deterministic modal tie handling by retaining the first value seen
    # in row order when several levels share the same frequency.
    mode_first <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) {
        return(NA)
      }
      tab <- table(x)
      winners <- names(tab)[tab == max(tab)]
      for (v in x) {
        if (as.character(v) %in% winners) {
          return(v)
        }
      }
      winners[[1]]
    }

    for (nm in names(sub)) {
      x <- sub[[nm]]
      if (is.numeric(x) || is.integer(x)) {
        # Average numeric trait encodings across models for the same species.
        keep <- x[is.finite(x)]
        row[[nm]] <- if (length(keep) == 0) NA_real_ else mean(keep)
      } else if (is.logical(x)) {
        # Use the modal logical value and keep first-seen tie order.
        keep <- x[!is.na(x)]
        row[[nm]] <- if (length(keep) == 0) NA else as.logical(mode_first(keep))
      } else {
        # Use the modal categorical label and keep first-seen tie order.
        keep <- as.character(x)
        keep <- keep[!is.na(keep) & nzchar(keep)]
        row[[nm]] <- if (length(keep) == 0) NA_character_ else as.character(mode_first(keep))
      }
    }

    out_rows[[i]] <- tibble::as_tibble(row)
  }

  out <- dplyr::bind_rows(out_rows)
  out$species_name <- out_names
  out[, c("species_name", setdiff(names(out), "species_name")), drop = FALSE]
}

#' Compute the log-frequency span
#'
#' @param frequency Numeric or coercible frequency vector.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
compute_frequency_span <- function(frequency) {
  # Frequency scaling later uses the observed positive span; when the span is
  # undefined, fall back to `1` to keep the distance term finite.
  freq_vals <- suppressWarnings(as.numeric(frequency))
  freq_vals <- freq_vals[is.finite(freq_vals) & freq_vals > 0]
  out <- if (length(freq_vals) >= 2) {
    max(log(freq_vals)) - min(log(freq_vals))
  } else {
    NA_real_
  }

  if (!is.finite(out) || out <= 0) {
    return(1)
  }

  out
}

#' Resolve the first available similarity column name
#'
#' @param tbl Data frame or tibble.
#' @param candidates Character vector of candidate column names in priority
#'   order.
#'
#' @return A single column name or `NA_character_`.
#'
#' @keywords internal
resolve_similarity_column_name <- function(tbl,
                                           candidates) {
  present <- candidates[candidates %in% names(tbl)]
  if (length(present) == 0) {
    return(NA_character_)
  }
  present[[1]]
}

#' Seed registry-defined trait columns
#'
#' @param models_tbl Candidate-model table.
#' @param registry_obj Registry lookup object returned by
#'   [read_similarity_registry()].
#'
#' @return A tibble with any missing registry-coded columns added.
#'
#' @keywords internal
seed_registry_traits <- function(models_tbl, registry_obj) {
  out <- tibble::as_tibble(models_tbl)
  trait_defs <- c(registry_obj$species_defs, registry_obj$study_defs)

  for (trait_defn in trait_defs) {
    trait_nm <- trait_defn$coded_name
    if (trait_nm %in% names(out)) {
      next
    }

    trait_type <- trait_defn$data_type %||% "categorical"
    if (identical(trait_type, "numeric")) {
      out[[trait_nm]] <- NA_real_
    } else if (identical(trait_type, "binary")) {
      out[[trait_nm]] <- NA
    } else {
      out[[trait_nm]] <- NA_character_
    }
  }

  out
}

#' Read a similarity-tuning config object
#'
#' @param config Optional JSON path or list.
#'
#' @return A list.
#'
#' @keywords internal
read_similarity_config <- function(config) {
  if (is.null(config)) {
    return(list())
  }

  if (is.character(config) && length(config) == 1) {
    return(read_json_file(config))
  }

  if (is.list(config)) {
    return(config)
  }

  stop("'config' must be NULL, a JSON file path, or a list.", call. = FALSE)
}

#' Resolve scalar similarity parameters
#'
#' @param alpha Optional starting alpha value.
#' @param k_species Optional starting species-kernel value.
#' @param k_study Optional starting study-kernel value.
#' @param seed Optional integer seed.
#' @param cfg_user Normalized user config list.
#'
#' @return A list with `alpha`, `k_species`, `k_study`, and `seed`.
#'
#' @keywords internal
resolve_similarity_inputs <- function(alpha,
                                      k_species,
                                      k_study,
                                      seed,
                                      cfg_user) {
  alpha <- alpha %||% cfg_user$alpha %||% 0.5
  k_species <- k_species %||% cfg_user$k_species %||% 2
  k_study <- k_study %||% cfg_user$k_study %||% 1
  seed <- seed %||% cfg_user$seed

  if (!is.numeric(alpha) || length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be one finite number strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(k_species) || length(k_species) != 1 || !is.finite(k_species) || k_species < 0) {
    stop("'k_species' must be one finite number >= 0.", call. = FALSE)
  }
  if (!is.numeric(k_study) || length(k_study) != 1 || !is.finite(k_study) || k_study < 0) {
    stop("'k_study' must be one finite number >= 0.", call. = FALSE)
  }

  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }
  if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed)) {
    stop("'seed' must be NULL or one finite numeric value.", call. = FALSE)
  }

  list(
    alpha = as.numeric(alpha),
    k_species = as.numeric(k_species),
    k_study = as.numeric(k_study),
    seed = as.integer(seed)
  )
}

#' Normalize similarity config options
#'
#' @param cfg_user Normalized user config list.
#' @param alpha Starting alpha value.
#' @param k_species Starting species-kernel value.
#' @param k_study Starting study-kernel value.
#'
#' @return A normalized config list.
#'
#' @keywords internal
resolve_similarity_setup <- function(cfg_user,
                                     alpha,
                                     k_species,
                                     k_study) {
  cfg <- utils::modifyList(
    list(
      length_coherence = list(method = "overlap", weight = 1),
      depth_coherence = list(method = "overlap", weight = 1),
      frequency_coherence = list(method = "numeric", weight = 1),
      alpha_grid = NULL,
      k_species_grid = NULL,
      k_study_grid = NULL
    ),
    cfg_user
  )

  cfg$length_coherence$method <- as.character(cfg$length_coherence$method %||% "overlap")[[1]]
  cfg$depth_coherence$method <- as.character(cfg$depth_coherence$method %||% "overlap")[[1]]
  cfg$frequency_coherence$method <- as.character(cfg$frequency_coherence$method %||% "numeric")[[1]]
  cfg$length_coherence$weight <- as.numeric(cfg$length_coherence$weight %||% 1)[[1]]
  cfg$depth_coherence$weight <- as.numeric(cfg$depth_coherence$weight %||% 1)[[1]]
  cfg$frequency_coherence$weight <- as.numeric(cfg$frequency_coherence$weight %||% 1)[[1]]

  cfg$alpha_grid <- sort(unique(as.numeric(unlist(cfg$alpha_grid %||% c(max(0.05, alpha - 0.2), alpha, min(0.95, alpha + 0.2))))))
  cfg$k_species_grid <- sort(unique(as.numeric(unlist(cfg$k_species_grid %||% c(max(0, k_species / 2), k_species, k_species * 2)))))
  cfg$k_study_grid <- sort(unique(as.numeric(unlist(cfg$k_study_grid %||% c(max(0, k_study / 2), k_study, k_study * 2)))))

  cfg
}

#' Normalize one trait-selection specification
#'
#' @param models_tbl Candidate-model table.
#' @param traits Trait specification supplied by the caller or config.
#' @param scope_names Valid coded names for the requested scope.
#' @param scope_map Named registry-definition lookup for the scope.
#' @param scope_label Label used in error messages.
#'
#' @return A list with `weights` and `defs`.
#'
#' @keywords internal
normalize_trait_weights <- function(models_tbl,
                                    traits,
                                    scope_names,
                                    scope_map,
                                    scope_label) {
  infer_trait_def <- function(col_name, col_data) {
    is_binary01 <- function(x) {
      vals <- unique(x[!is.na(x)])
      if (length(vals) == 0) {
        return(FALSE)
      }
      all(vals %in% c(0, 1, FALSE, TRUE))
    }

    data_type <- "categorical"
    if (is.logical(col_data) || is_binary01(col_data)) {
      data_type <- "binary"
    } else if (is.numeric(col_data) || is.integer(col_data)) {
      data_type <- "numeric"
    }

    list(
      coded_name = col_name,
      display_name = col_name,
      description = "Auto-inferred trait definition.",
      data_type = data_type,
      unit = NULL,
      multi_valued = FALSE,
      expandable = FALSE,
      allowed_values = NULL
    )
  }

  eligible <- scope_names[scope_names %in% names(models_tbl)]
  eligible <- eligible[vapply(eligible, function(nm) {
    x <- models_tbl[[nm]]
    if (is.character(x)) {
      x <- stringr::str_squish(x)
      x <- x[!is.na(x) & nzchar(x)]
    } else {
      x <- x[!is.na(x)]
    }
    length(unique(x)) > 0
  }, logical(1))]

  if (is.null(traits)) {
    weights <- stats::setNames(rep(1, length(eligible)), eligible)
    return(list(weights = weights, defs = scope_map[eligible]))
  }

  if (is.character(traits) && (is.null(names(traits)) || all(is.na(names(traits)) | !nzchar(names(traits))))) {
    weights <- stats::setNames(rep(1, length(traits)), traits)
  } else if ((is.numeric(traits) || is.character(traits)) && !is.null(names(traits)) && all(!is.na(names(traits))) && all(nzchar(names(traits)))) {
    weights <- suppressWarnings(as.numeric(unname(traits)))
    names(weights) <- names(traits)
  } else if (is.list(traits) && !is.data.frame(traits)) {
    weights <- suppressWarnings(as.numeric(unlist(traits, use.names = FALSE)))
    names(weights) <- names(traits)
  } else if (is.data.frame(traits)) {
    if (all(c("trait", "weight") %in% names(traits))) {
      weights <- suppressWarnings(as.numeric(traits$weight))
      names(weights) <- as.character(traits$trait)
    } else if (all(c("coded_name", "weight") %in% names(traits))) {
      weights <- suppressWarnings(as.numeric(traits$weight))
      names(weights) <- as.character(traits$coded_name)
    } else {
      stop(sprintf("'%s' data frames must contain 'trait'/'weight' or 'coded_name'/'weight'.", scope_label), call. = FALSE)
    }
  } else {
    stop(sprintf("'%s' must be NULL, a character vector, a named list/vector, or a trait-weight data frame.", scope_label), call. = FALSE)
  }

  if (is.null(names(weights)) || any(is.na(names(weights))) || any(!nzchar(names(weights)))) {
    stop(sprintf("'%s' weights must be named by trait.", scope_label), call. = FALSE)
  }

  unknown <- setdiff(names(weights), scope_names)
  if (length(unknown) > 0) {
    missing_in_models <- setdiff(unknown, names(models_tbl))
    if (length(missing_in_models) > 0) {
      stop(sprintf("Unknown %s trait(s): %s", scope_label, paste(missing_in_models, collapse = ", ")), call. = FALSE)
    }

    inferred_defs <- lapply(unknown, function(nm) infer_trait_def(nm, models_tbl[[nm]]))
    names(inferred_defs) <- unknown
    scope_map <- c(scope_map, inferred_defs)
    eligible <- unique(c(eligible, unknown))
  }

  unavailable <- setdiff(names(weights), eligible)
  if (length(unavailable) > 0) {
    stop(sprintf("%s trait(s) are not available in 'candidate_models': %s", scope_label, paste(unavailable, collapse = ", ")), call. = FALSE)
  }

  if (any(!is.finite(weights) | weights < 0)) {
    stop(sprintf("'%s' weights must be finite and >= 0.", scope_label), call. = FALSE)
  }

  list(weights = weights, defs = scope_map[names(weights)])
}

#' Prepare similarity inputs
#'
#' Selects registry-defined species and study traits, applies starting weights,
#' expands set-valued traits to binary membership columns, and returns the
#' prepared species-level and study-level matrices needed for later similarity
#' calculations.
#'
#' @param candidate_models Prepared candidate-model table.
#' @param species_traits Optional species-trait specification. Use `NULL` to
#'   use all eligible species traits at weight `1`; a character vector to use
#'   only those traits at weight `1`; a named list or named numeric vector to
#'   set explicit starting weights; or a data frame with `trait`/`weight`
#'   columns. When `NULL`, a config-supplied value is used when present.
#' @param study_traits Optional study-trait specification. Follows the same
#'   rules as `species_traits`. When `NULL`, a config-supplied value is used
#'   when present.
#' @param alpha Optional starting species-versus-study mixing parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param k_species Optional starting species-distance kernel parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param k_study Optional starting study-distance kernel parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param config Optional JSON path or list with similarity options. Supported
#'   entries are `species_traits`, `study_traits`, `alpha`, `k_species`,
#'   `k_study`, `seed`, `length_coherence`, `depth_coherence`,
#'   `frequency_coherence`, `alpha_grid`, `k_species_grid`, and
#'   `k_study_grid`.
#' @param registry_path Optional path to a trait-registry JSON file.
#' @param seed Optional integer seed. When `NULL`, a config-supplied value is
#'   used when present; otherwise one is generated and returned in the output
#'   object.
#'
#' @return A list containing the normalized tuning configuration, selected
#'   traits, starting weights, expanded species/study matrices, and collapsed
#'   species profiles.
#'
#' @export
prepare_similarity_matrix <- function(candidate_models,
                                      species_traits = NULL,
                                      study_traits = NULL,
                                      alpha = NULL,
                                      k_species = NULL,
                                      k_study = NULL,
                                      config = NULL,
                                      registry_path = NULL,
                                      seed = NULL) {
  # Normalize the candidate model table and resolve the trait registry first so
  # later helpers can assume a stable input schema.
  models_tbl <- normalize_similarity_data(candidate_models)
  registry_obj <- read_similarity_registry(registry_path)
  models_tbl <- seed_registry_traits(models_tbl, registry_obj)

  # Resolve the optional config and scalar starting values before trait
  # selection begins so the full preparation state is explicit.
  cfg_user <- read_similarity_config(config)
  scalar_obj <- resolve_similarity_inputs(
    alpha = alpha,
    k_species = k_species,
    k_study = k_study,
    seed = seed,
    cfg_user = cfg_user
  )
  cfg <- resolve_similarity_setup(
    cfg_user = cfg_user,
    alpha = scalar_obj$alpha,
    k_species = scalar_obj$k_species,
    k_study = scalar_obj$k_study
  )

  # Resolve the requested species and study traits into named weight vectors
  # plus the matching registry definitions.
  species_spec <- normalize_trait_weights(
    models_tbl = models_tbl,
    traits = species_traits %||% cfg_user$species_traits,
    scope_names = registry_obj$species_names,
    scope_map = registry_obj$species_map,
    scope_label = "species_traits"
  )
  study_spec <- normalize_trait_weights(
    models_tbl = models_tbl,
    traits = study_traits %||% cfg_user$study_traits,
    scope_names = registry_obj$study_names,
    scope_map = registry_obj$study_map,
    scope_label = "study_traits"
  )

  if (length(species_spec$weights) == 0 && length(study_spec$weights) == 0) {
    stop("No eligible species or study traits were selected.", call. = FALSE)
  }

  species_cols <- intersect(names(species_spec$weights), names(models_tbl))
  study_cols <- intersect(names(study_spec$weights), names(models_tbl))

  # First, collapse species profiles using the ORIGINAL (non-expanded) trait data
  # so that set-valued traits like ocean_basin are collapsed using mode/string-based
  # logic rather than averaging (which destroys binary structure).
  species_profiles_raw <- collapse_species_profiles(
    models_df = models_tbl,
    expanded_df = models_tbl
  )

  # Normalize ocean_basin and fao_area to match registry allowed_values before expansion.
  # ocean_basin comes in as full names ("Atlantic Ocean;Pacific Ocean") but registry
  # expects short codes ("atlantic;pacific").
  # fao_area may contain invalid codes like "0" or "37.4" that need to be filtered.
  species_profiles_normalized <- species_profiles_raw
  
  if ("ocean_basin" %in% names(species_profiles_normalized)) {
    basin_mapping <- c(
      "atlantic" = "atlantic",
      "pacific" = "pacific",
      "mediterranean" = "mediterranean",
      "indian" = "indian",
      "southern" = "southern",
      "arctic" = "arctic"
    )
    basin_raw <- stringr::str_to_lower(stringr::str_squish(as.character(species_profiles_normalized$ocean_basin)))
    basin_normalized <- character(length(basin_raw))
    
    for (i in seq_along(basin_raw)) {
      if (is.na(basin_raw[i]) || !nzchar(basin_raw[i])) {
        basin_normalized[i] <- NA_character_
      } else {
        # Split on semicolon/comma, detect which basin keywords match, and rebuild
        parts <- stringr::str_trim(stringr::str_split_1(basin_raw[i], "[;,]"))
        matched_basins <- character(0)
        for (basin_code in names(basin_mapping)) {
          if (any(stringr::str_detect(parts, paste0("\\b", basin_code, "\\b")))) {
            matched_basins <- c(matched_basins, basin_code)
          }
        }
        basin_normalized[i] <- if (length(matched_basins) > 0) {
          paste(sort(unique(matched_basins)), collapse = ";")
        } else {
          NA_character_
        }
      }
    }
    species_profiles_normalized$ocean_basin <- basin_normalized
  }
  
  if ("fao_area" %in% names(species_profiles_normalized)) {
    fao_allowed <- c("1", "2", "3", "4", "5", "6", "7", "8", "18", "21", "27", "31", 
                     "34", "37", "41", "47", "48", "51", "57", "58", "61", "67", "71", 
                     "77", "81", "87", "88")
    fao_raw <- stringr::str_to_lower(stringr::str_squish(as.character(species_profiles_normalized$fao_area)))
    fao_normalized <- character(length(fao_raw))
    
    for (i in seq_along(fao_raw)) {
      if (is.na(fao_raw[i]) || !nzchar(fao_raw[i])) {
        fao_normalized[i] <- NA_character_
      } else {
        # Split on semicolon, convert to integer to normalize format, filter valid codes
        parts <- stringr::str_trim(stringr::str_split_1(fao_raw[i], ";"))
        # Try to convert to integer and keep only valid FAO codes
        part_nums <- suppressWarnings(as.integer(parts))
        valid_codes <- as.character(part_nums[!is.na(part_nums) & as.character(part_nums) %in% fao_allowed])
        fao_normalized[i] <- if (length(valid_codes) > 0) {
          paste(sort(unique(valid_codes)), collapse = ";")
        } else {
          NA_character_
        }
      }
    }
    species_profiles_normalized$fao_area <- fao_normalized
  }

  # Build named trait definition lookups keyed by coded_name for expand_trait_block.
  species_defs_lookup <- stats::setNames(
    registry_obj$species_defs,
    vapply(registry_obj$species_defs, function(x) x$coded_name, character(1))
  )
  study_defs_lookup <- stats::setNames(
    registry_obj$study_defs,
    vapply(registry_obj$study_defs, function(x) x$coded_name, character(1))
  )

  # NOW expand set-valued traits AFTER collapsing so binary indicators
  # represent clean species-level set membership without averaging artifacts.
  # Preserve species_name before expansion, then re-attach after.
  species_name_backup <- species_profiles_normalized$species_name
  
  species_expanded <- expand_trait_block(
    df = species_profiles_normalized[, species_cols, drop = FALSE],
    weight_spec = species_spec$weights[species_cols],
    trait_defs = species_defs_lookup
  )
  study_expanded <- expand_trait_block(
    df = models_tbl[, study_cols, drop = FALSE],
    weight_spec = study_spec$weights[study_cols],
    trait_defs = study_defs_lookup
  )
  # Re-attach species_name to the expanded data
  species_profiles <- species_expanded$data
  species_profiles$species_name <- species_name_backup
  freq_span <- compute_frequency_span(models_tbl$frequency)

  # Build the model-level candidate table with all expanded trait columns so
  # run_ordination and add_ordination_missing can select by trait_cols.
  candidate_models_with_expanded <- models_tbl

  # 1. Process study trait columns: replace scalar (non-set) traits with their
  #    coerced forms from study_expanded$data and add new binary indicator
  #    columns for any set-valued traits (e.g. fao_area__1, fao_area__27).
  #    Set-valued raw columns (e.g. fao_area) are PRESERVED in the table so
  #    that subsequent calls to prepare_similarity_matrix on already-prepared
  #    data (e.g. inside parallel scoring workers) can still validate those
  #    traits without hitting an "all-NA" rejection.
  if (length(study_cols) > 0) {
    # Determine which study traits are set-type (expanded to binary columns).
    set_study_cols <- study_cols[vapply(study_cols, function(nm) {
      td <- study_defs_lookup[[nm]]
      identical(
        if (is.null(td)) "categorical" else (td$data_type %||% "categorical"),
        "set"
      )
    }, logical(1))]

    # Non-set scalars (numeric / categorical / binary) are replaced directly by
    # their coerced equivalents that come back inside study_expanded$data.
    non_set_study_cols <- setdiff(study_cols, set_study_cols)

    # Any binary-indicator columns from a previous prepare call that are
    # already in the table must be removed to avoid duplicate-column warnings
    # when this function is re-run on a pre-expanded candidate table.
    already_expanded_cols <- setdiff(names(study_expanded$data), study_cols)
    pre_existing_expanded <- intersect(already_expanded_cols, names(candidate_models_with_expanded))

    cols_to_drop <- unique(c(non_set_study_cols, pre_existing_expanded))
    if (length(cols_to_drop) > 0) {
      candidate_models_with_expanded <- candidate_models_with_expanded[
        , setdiff(names(candidate_models_with_expanded), cols_to_drop), drop = FALSE
      ]
    }
    candidate_models_with_expanded <- dplyr::bind_cols(
      candidate_models_with_expanded,
      study_expanded$data
    )
  }

  # 2. Join expanded species binary indicator columns (ocean_basin__*, etc.)
  #    from species_profiles back to each model row by species_name.
  #    Remove any pre-existing copies first so that re-running this function
  #    on an already-prepared table never produces duplicate columns.
  new_species_binary_cols <- setdiff(names(species_profiles), c("species_name", species_cols))
  if (length(new_species_binary_cols) > 0) {
    pre_existing_sp <- intersect(new_species_binary_cols, names(candidate_models_with_expanded))
    if (length(pre_existing_sp) > 0) {
      candidate_models_with_expanded <- candidate_models_with_expanded[
        , setdiff(names(candidate_models_with_expanded), pre_existing_sp), drop = FALSE
      ]
    }
    species_profiles_join <- species_profiles[
      , c("species_name", new_species_binary_cols), drop = FALSE
    ]
    candidate_models_with_expanded <- dplyr::left_join(
      candidate_models_with_expanded,
      species_profiles_join,
      by = "species_name"
    )
  }

  list(
    candidate_models = candidate_models_with_expanded,
    species_traits = names(species_spec$weights),
    study_traits = names(study_spec$weights),
    species_weights = species_spec$weights,
    study_weights = study_spec$weights,
    species_trait_defs = species_spec$defs,
    study_trait_defs = study_spec$defs,
    species_data = species_expanded$data,
    species_component_lookup = species_expanded$lookup,
    species_matrix_weights = species_expanded$weights,
    study_data = study_expanded$data,
    study_component_lookup = study_expanded$lookup,
    study_matrix_weights = study_expanded$weights,
    species_profiles = species_profiles,
    alpha = scalar_obj$alpha,
    k_species = scalar_obj$k_species,
    k_study = scalar_obj$k_study,
    config = cfg,
    seed = scalar_obj$seed,
    frequency_span = freq_span
  )
}

#' Compute a weighted Gower distance matrix
#'
#' Builds a Gower distance matrix from a prepared trait table and a named
#' weight vector. Constant or fully missing columns are removed before
#' distance calculation.
#'
#' @param df_traits Prepared trait table.
#' @param trait_weights Named numeric trait-weight vector.
#'
#' @return A numeric distance matrix.
#'
#' @keywords internal
compute_gower_matrix <- function(df_traits,
                                 trait_weights,
                                 trait_defs = NULL,
                                 component_lookup = NULL) {

  # Return a degenerate matrix immediately when no weighted columns were
  # supplied.
  if (length(trait_weights) == 0 || ncol(df_traits) == 0) {
    out <- matrix(NA_real_, nrow = nrow(df_traits), ncol = nrow(df_traits))
    diag(out) <- 0
    return(out)
  }

  mat <- tibble::as_tibble(df_traits)
  keep_cols <- intersect(names(trait_weights), names(mat))
  mat <- mat[, keep_cols, drop = FALSE]
  w <- trait_weights[keep_cols]

  source_lookup <- if (!is.null(component_lookup)) {
    as.character(component_lookup[keep_cols])
  } else {
    keep_cols
  }
  names(source_lookup) <- keep_cols
  source_lookup[is.na(source_lookup) | !nzchar(source_lookup)] <- keep_cols[is.na(source_lookup) | !nzchar(source_lookup)]

  defs_map <- trait_defs %||% list()
  defs_map <- defs_map[intersect(names(defs_map), unique(source_lookup))]

  infer_interval_pairs <- function(source_traits,
                                   defs_now) {
    if (length(source_traits) == 0) {
      return(list())
    }

    numeric_traits <- source_traits[vapply(source_traits, function(nm) {
      def <- defs_now[[nm]]
      if (is.null(def)) {
        return(TRUE)
      }
      identical(def$data_type, "numeric")
    }, logical(1))]

    suffix_pairs <- list(
      c("_minimum", "_maximum"),
      c("_min", "_max")
    )

    out <- list()
    seen_keys <- character(0)
    for (nm in numeric_traits) {
      for (pair_def in suffix_pairs) {
        left <- pair_def[[1]]
        right <- pair_def[[2]]
        if (!endsWith(nm, left)) {
          next
        }

        base <- substr(nm, 1, nchar(nm) - nchar(left))
        mate <- paste0(base, right)
        if (!mate %in% numeric_traits) {
          next
        }

        key <- paste(sort(c(nm, mate)), collapse = "||")
        if (key %in% seen_keys) {
          next
        }

        out[[length(out) + 1]] <- c(nm, mate)
        seen_keys <- c(seen_keys, key)
      }
    }

    out
  }

  infer_taxonomic_ranks <- function(source_traits,
                                    defs_now) {
    rank_levels <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
    out <- stats::setNames(rep(NA_character_, length(rank_levels)), rank_levels)

    for (nm in source_traits) {
      def <- defs_now[[nm]]
      if (!is.null(def) && !identical(def$data_type, "categorical")) {
        next
      }

      nm_low <- tolower(nm)
      for (rk in rank_levels) {
        if (identical(nm_low, rk) || identical(nm_low, paste0("tax_", rk))) {
          out[[rk]] <- nm
        }
      }
    }

    out[!is.na(out)]
  }

  # Compute one interval-overlap distance matrix for an observed min/max pair.
  compute_interval_distance_matrix <- function(df_now,
                                               min_col,
                                               max_col) {
    min_raw <- suppressWarnings(as.numeric(df_now[[min_col]]))
    max_raw <- suppressWarnings(as.numeric(df_now[[max_col]]))
    mins <- ifelse(is.finite(min_raw) & is.finite(max_raw), pmin(min_raw, max_raw), NA_real_)
    maxs <- ifelse(is.finite(min_raw) & is.finite(max_raw), pmax(min_raw, max_raw), NA_real_)

    n <- length(mins)
    out <- matrix(NA_real_, nrow = n, ncol = n)
    if (n == 0) {
      return(out)
    }
    diag(out) <- 0

    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i == j) {
          next
        }
        if (!is.finite(mins[[i]]) || !is.finite(maxs[[i]]) ||
            !is.finite(mins[[j]]) || !is.finite(maxs[[j]])) {
          out[i, j] <- NA_real_
        } else {
          inter <- max(0, min(maxs[[i]], maxs[[j]]) - max(mins[[i]], mins[[j]]))
          union <- max(maxs[[i]], maxs[[j]]) - min(mins[[i]], mins[[j]])
          union <- max(union, 1e-9)
          out[i, j] <- 1 - (inter / union)
        }
      }
    }

    out
  }

  # Combine component distances by weighted averaging only over finite entries
  # so partially missing components do not force NA for a pair.
  combine_distance_components <- function(components,
                                          n_rows) {
    if (length(components) == 0) {
      out <- matrix(NA_real_, nrow = n_rows, ncol = n_rows)
      diag(out) <- 0
      return(out)
    }

    num <- matrix(0, nrow = n_rows, ncol = n_rows)
    den <- matrix(0, nrow = n_rows, ncol = n_rows)

    for (comp in components) {
      mat_now <- comp$mat
      weight_now <- comp$weight
      keep <- is.finite(mat_now)
      num[keep] <- num[keep] + weight_now * mat_now[keep]
      den[keep] <- den[keep] + weight_now
    }

    out <- num / den
    out[!is.finite(out)] <- NA_real_
    diag(out) <- 0
    out
  }

  components <- list()

  interval_pairs <- infer_interval_pairs(unique(source_lookup), defs_map)

  for (pair_cols in interval_pairs) {
    if (all(pair_cols %in% source_lookup)) {
      pair_cols_mat <- names(source_lookup)[source_lookup %in% pair_cols]
      if (length(pair_cols_mat) < 2) {
        next
      }

      col_min <- pair_cols_mat[which(source_lookup[pair_cols_mat] == pair_cols[[1]])[[1]]]
      col_max <- pair_cols_mat[which(source_lookup[pair_cols_mat] == pair_cols[[2]])[[1]]]
      pair_weight <- sum(w[c(col_min, col_max)], na.rm = TRUE)
      if (is.finite(pair_weight) && pair_weight > 0) {
        interval_dist <- compute_interval_distance_matrix(mat, col_min, col_max)
        components[[length(components) + 1]] <- list(mat = interval_dist, weight = pair_weight)
      }

      keep_after_drop <- names(source_lookup)[!source_lookup %in% pair_cols]
      mat <- mat[, keep_after_drop, drop = FALSE]
      w <- w[keep_after_drop]
      source_lookup <- source_lookup[keep_after_drop]
    }
  }

  # Build one taxonomy/phylogeny component so taxonomic traits contribute once
  # rather than as several independent categorical fields.
  tax_ranks <- infer_taxonomic_ranks(unique(source_lookup), defs_map)
  if (length(tax_ranks) > 0) {
    tax_source <- as.character(tax_ranks)
    tax_cols <- names(source_lookup)[source_lookup %in% tax_source]

    if (length(tax_cols) > 0) {
      rank_levels <- names(tax_ranks)
      rank_cols <- stats::setNames(rep(NA_character_, length(rank_levels)), rank_levels)
      for (rk in rank_levels) {
        src_nm <- tax_ranks[[rk]]
        cand_cols <- names(source_lookup)[source_lookup == src_nm]
        if (length(cand_cols) > 0) {
          rank_cols[[rk]] <- cand_cols[[1]]
        }
      }
      rank_cols <- rank_cols[!is.na(rank_cols)]

      if (length(rank_cols) > 0) {
        rank_values <- lapply(rank_cols, function(col_nm) {
          x <- stringr::str_squish(as.character(mat[[col_nm]]))
          x[!nzchar(x)] <- NA_character_
          x
        })

        phylo_from_species <- function(species_vec,
                                       genus_vec = NULL) {
          if (!requireNamespace("rotl", quietly = TRUE) ||
              !requireNamespace("ape", quietly = TRUE)) {
            return(NULL)
          }

          sp_raw <- stringr::str_squish(as.character(species_vec))
          sp_raw[!nzchar(sp_raw)] <- NA_character_

          # Build a binomial vector for phylogeny matching. Prefer explicit
          # binomials when already present; otherwise combine genus + species
          # when both are available and valid.
          sp <- rep(NA_character_, length(sp_raw))
          has_binomial <- !is.na(sp_raw) & grepl("\\s+", sp_raw)
          sp[has_binomial] <- sp_raw[has_binomial]

          if (!is.null(genus_vec)) {
            genus_now <- stringr::str_squish(as.character(genus_vec))
            genus_now[!nzchar(genus_now)] <- NA_character_
            species_epithet <- sp_raw
            species_epithet[grepl("\\s+", species_epithet)] <- NA_character_

            can_build <- !is.na(genus_now) & !is.na(species_epithet)
            sp[can_build] <- stringr::str_squish(paste(genus_now[can_build], species_epithet[can_build]))
          }

          # If we cannot build any trustworthy binomials, skip phylogeny and
          # let the caller use the deterministic taxonomy-rank fallback.
          sp <- stringr::str_to_sentence(sp)
          sp[is.na(sp) | !nzchar(sp)] <- NA_character_
          if (sum(!is.na(sp)) < 2) {
            return(NULL)
          }

          n <- length(sp)
          out <- matrix(1, nrow = n, ncol = n)
          diag(out) <- 0

          uniq <- unique(sp[!is.na(sp)])
          if (length(uniq) < 2) {
            return(out)
          }

          phylo_mat <- tryCatch({
            tnrs <- suppressWarnings(
              rotl::tnrs_match_names(uniq, do_approximate_matching = FALSE)
            )
            tnrs_tbl <- tibble::as_tibble(tnrs)
            matched <- tnrs_tbl |>
              dplyr::mutate(search_string = as.character(search_string)) |>
              dplyr::filter(!is.na(ott_id)) |>
              dplyr::distinct(search_string, .keep_all = TRUE)

            # Require adequate match coverage. Sparse matches can produce a
            # highly distorted phylogeny component.
            match_frac <- nrow(matched) / length(uniq)
            if (!is.finite(match_frac) || match_frac < 0.7) {
              return(NULL)
            }

            if (nrow(matched) < 2) {
              return(NULL)
            }

            subtree <- suppressWarnings(
              rotl::tol_induced_subtree(ott_ids = matched$ott_id)
            )
            cophen <- suppressWarnings(ape::cophenetic.phylo(subtree))
            cophen_max <- max(cophen, na.rm = TRUE)
            if (!is.finite(cophen_max) || cophen_max <= 0) {
              cophen_max <- 1
            }

            labels <- suppressWarnings(rotl::strip_ott_ids(colnames(cophen)))
            labels <- gsub("_", " ", labels, fixed = TRUE)
            labels <- stringr::str_squish(tolower(labels))

            phy <- matrix(1, nrow = length(uniq), ncol = length(uniq), dimnames = list(uniq, uniq))
            diag(phy) <- 0

            keep <- labels %in% uniq
            if (sum(keep) >= 1) {
              labels_keep <- labels[keep]
              cophen_keep <- cophen[keep, keep, drop = FALSE]
              labels_unique <- !duplicated(labels_keep)
              labels_keep <- labels_keep[labels_unique]
              cophen_keep <- cophen_keep[labels_unique, labels_unique, drop = FALSE]
              if (length(labels_keep) >= 1) {
                dimnames(cophen_keep) <- list(labels_keep, labels_keep)
                phy[labels_keep, labels_keep] <- cophen_keep / cophen_max
              }
            }

            phy
          }, error = function(e) NULL)

          if (is.null(phylo_mat)) {
            return(NULL)
          }

          out_idx <- match(tolower(sp), rownames(phylo_mat))
          keep_idx <- which(!is.na(out_idx))
          if (length(keep_idx) > 0) {
            out[keep_idx, keep_idx] <- phylo_mat[out_idx[keep_idx], out_idx[keep_idx], drop = FALSE]
          }
          diag(out) <- 0
          out
        }

        n <- nrow(mat)
        tax_dist <- NULL
        species_rank_idx <- which(names(rank_values) == "species")
        genus_rank_idx <- which(names(rank_values) == "genus")
        if (length(species_rank_idx) == 1) {
          genus_vec <- if (length(genus_rank_idx) == 1) rank_values[[genus_rank_idx]] else NULL
          tax_dist <- phylo_from_species(rank_values[[species_rank_idx]], genus_vec = genus_vec)
        }

        if (is.null(tax_dist)) {
          tax_dist <- matrix(NA_real_, nrow = n, ncol = n)
          diag(tax_dist) <- 0
          n_ranks <- length(rank_values)

          for (i in seq_len(n)) {
            for (j in seq_len(n)) {
              if (i == j) {
                next
              }

              deepest_shared <- 0
              for (r in seq_len(n_ranks)) {
                xi <- rank_values[[r]][[i]]
                xj <- rank_values[[r]][[j]]
                if (!is.na(xi) && !is.na(xj) && identical(xi, xj)) {
                  deepest_shared <- r
                }
              }

              tax_dist[i, j] <- 1 - (deepest_shared / n_ranks)
            }
          }
        }

        tax_weight <- sum(w[tax_cols], na.rm = TRUE)
        if (is.finite(tax_weight) && tax_weight > 0) {
          components[[length(components) + 1]] <- list(mat = tax_dist, weight = tax_weight)
        }

        keep_after_drop <- names(source_lookup)[!source_lookup %in% tax_source]
        mat <- mat[, keep_after_drop, drop = FALSE]
        w <- w[keep_after_drop]
        source_lookup <- source_lookup[keep_after_drop]
      }
    }
  }

  if (ncol(mat) > 0) {
    # Convert character columns to factors so `daisy()` treats them as
    # categorical rather than attempting numeric coercion.
    for (nm in names(mat)) {
      if (is.character(mat[[nm]])) {
        mat[[nm]] <- as.factor(mat[[nm]])
      }
    }

    # Drop columns that are entirely missing or non-informative for pairwise
    # comparisons.
    keep_gower <- vapply(names(mat), function(nm) {
      x <- mat[[nm]]
      x <- x[!is.na(x)]
      length(unique(x)) > 1
    }, logical(1))
    mat <- mat[, keep_gower, drop = FALSE]
    w_gower <- w[names(mat)]

    if (ncol(mat) > 0) {
      is_binary01 <- function(x) {
        if (!(is.numeric(x) || is.integer(x) || is.logical(x))) {
          return(FALSE)
        }
        vals <- unique(x[!is.na(x)])
        length(vals) > 0 && all(vals %in% c(0, 1, FALSE, TRUE))
      }

      # Tell `daisy()` which prepared columns are symmetric binaries so those
      # dimensions are handled appropriately during the Gower calculation.
      binary_cols <- which(vapply(mat, is_binary01, logical(1)))
      daisy_type <- NULL
      if (length(binary_cols) > 0) {
        daisy_type <- list(symm = binary_cols)
      }

      g <- as.matrix(
        cluster::daisy(
          mat,
          metric = "gower",
          weights = w_gower,
          type = daisy_type
        )
      )
      g_weight <- sum(w_gower, na.rm = TRUE)
      if (is.finite(g_weight) && g_weight > 0) {
        components[[length(components) + 1]] <- list(mat = g, weight = g_weight)
      }
    }
  }

  combine_distance_components(components, nrow(df_traits))
}

#' Build Gower distance matrices
#'
#' Builds the species-level and study-level Gower distance matrices from a
#' prepared similarity object, expands the species matrix back to model rows,
#' and combines the two blocks with the prepared alpha value.
#'
#' @param sim_obj Prepared similarity object returned by
#'   [prepare_similarity_matrix()].
#'
#' @return A list containing `species_dist`, `study_dist`,
#'   `species_dist_model`, `combined_dist`, and `trait_cols`.
#'
#' @export
build_gower_distances <- function(sim_obj) {
  # Validate the prepared object shape once so downstream matrix construction
  # can assume the required pieces are present.
  required_fields <- c(
    "species_profiles", "species_matrix_weights", "study_data",
    "study_matrix_weights", "candidate_models", "alpha",
    "species_traits", "study_traits"
  )
  missing_fields <- setdiff(required_fields, names(sim_obj))
  if (length(missing_fields) > 0) {
    stop(
      sprintf(
        "'sim_obj' is missing required field(s): %s",
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Build the species-level matrix on the collapsed species profiles so
  # biology distances are not affected by repeated model rows per species.
  species_trait_cols <- setdiff(names(sim_obj$species_profiles), "species_name")

  species_dist <- if (nrow(sim_obj$species_profiles) > 0) {
    compute_gower_matrix(
      df_traits = sim_obj$species_profiles[, species_trait_cols, drop = FALSE],
      trait_weights = sim_obj$species_matrix_weights,
      trait_defs = sim_obj$species_trait_defs,
      component_lookup = sim_obj$species_component_lookup
    )
  } else {
    matrix(NA_real_, nrow = 0, ncol = 0)
  }

  if (nrow(sim_obj$species_profiles) > 0) {
    rownames(species_dist) <- sim_obj$species_profiles$species_name
    colnames(species_dist) <- sim_obj$species_profiles$species_name
  }

  # Build the study-level matrix directly on the model rows because the donor
  # pool is evaluated at the individual model level.
  study_dist <- compute_gower_matrix(
    df_traits = sim_obj$study_data,
    trait_weights = sim_obj$study_matrix_weights,
    trait_defs = sim_obj$study_trait_defs,
    component_lookup = sim_obj$study_component_lookup
  )
  model_ids <- sim_obj$candidate_models$model_id_chr
  rownames(study_dist) <- model_ids
  colnames(study_dist) <- model_ids

  # Expand the species-level matrix back to model rows using the candidate
  # models' species labels so the two distance blocks share one index.
  species_vec <- stringr::str_squish(as.character(sim_obj$candidate_models$species_name))

  # Fail fast when candidate-model species labels cannot be mapped back to the
  # collapsed species-profile matrix. Silent NA expansion here can distort the
  # final ordination geometry.
  missing_species <- unique(species_vec[is.na(species_vec) | !nzchar(species_vec)])
  if (length(missing_species) > 0) {
    stop(
      "'candidate_models$species_name' contains missing or blank values after normalization.",
      call. = FALSE
    )
  }

  unknown_species <- setdiff(unique(species_vec), rownames(species_dist))
  if (length(unknown_species) > 0) {
    stop(
      sprintf(
        "Species in 'candidate_models' were not found in 'species_profiles': %s",
        paste(unknown_species, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  species_dist_model <- species_dist[species_vec, species_vec, drop = FALSE]
  rownames(species_dist_model) <- model_ids
  colnames(species_dist_model) <- model_ids

  # Combine the species and study distance blocks with the prepared alpha
  # weight and force zero self-distance on the diagonal.
  #
  # When one component is missing for a pair, rescale by the available weight
  # instead of propagating NA into the hybrid matrix.
  # Note: use explicit ifelse guards so that 0 * NA -> 0 (not NA).
  w_species <- ifelse(is.finite(species_dist_model), sim_obj$alpha, 0)
  w_study <- ifelse(is.finite(study_dist), 1 - sim_obj$alpha, 0)
  w_total <- w_species + w_study

  species_contrib <- ifelse(is.finite(species_dist_model), w_species * species_dist_model, 0)
  study_contrib   <- ifelse(is.finite(study_dist),         w_study  * study_dist,         0)

  combined_dist <- matrix(NA_real_, nrow = nrow(study_dist), ncol = ncol(study_dist))
  use_idx <- w_total > 0
  combined_dist[use_idx] <- (species_contrib[use_idx] + study_contrib[use_idx]) / w_total[use_idx]

  rownames(combined_dist) <- model_ids
  colnames(combined_dist) <- model_ids
  diag(combined_dist) <- 0

  # Return the EXPANDED trait column names so callers get the binary indicators
  # for set-valued traits (ocean_basin__*, fao_area__*) instead of the original
  # semi-colon-delimited strings.
  expanded_species_cols <- setdiff(names(sim_obj$species_profiles), "species_name")
  expanded_study_cols <- names(sim_obj$study_data)
  trait_cols <- unique(c(expanded_species_cols, expanded_study_cols))

  list(
    species_dist = species_dist,
    study_dist = study_dist,
    species_dist_model = species_dist_model,
    combined_dist = combined_dist,
    trait_cols = trait_cols
  )
}

#' Build the empirical tuning subset
#'
#' Selects a representative per-species subset of candidate models for the
#' leave-one-out tuning pass.
#'
#' @param candidate_models Prepared candidate-model table.
#' @param species_weights Named numeric species-trait weight vector.
#' @param study_weights Named numeric study-trait weight vector.
#' @param max_models_per_species Maximum number of retained models per species.
#' @param seed Integer seed used for tie-breaking.
#'
#' @return A tibble.
#'
#' @keywords internal
build_tuning_subset <- function(candidate_models,
                                species_weights,
                                study_weights,
                                max_models_per_species,
                                seed) {
  # Accept either the current prepared interval names or the older cached
  # names so the tuning subset can still be built while caches are being
  # refreshed to the newer schema.
  study_length_min_col <- resolve_similarity_column_name(
    candidate_models,
    c("study_length_min", "length_minimum")
  )
  study_length_max_col <- resolve_similarity_column_name(
    candidate_models,
    c("study_length_max", "length_maximum")
  )
  study_length_mid_col <- resolve_similarity_column_name(
    candidate_models,
    c("study_length_midpoint", "length_midpoint")
  )
  species_length_max_col <- resolve_similarity_column_name(
    candidate_models,
    c("species_length_max", "length_max")
  )

  # Score row completeness using the traits that will actually participate in
  # tuning so the retained subset favors well-populated models.
  tune_cols <- unique(c(
    "species_name", "slope_len", "intercept_len",
    study_length_min_col, study_length_max_col, study_length_mid_col,
    species_length_max_col, "frequency",
    names(species_weights), names(study_weights)
  ))
  tune_cols <- intersect(tune_cols, names(candidate_models))

  set.seed(seed)

  # Within each species, keep the most complete rows first and use a seeded
  # random tie-break so repeated runs remain reproducible.
  out <- tibble::as_tibble(candidate_models) |>
    dplyr::filter(is.finite(slope_len), is.finite(intercept_len)) |>
    dplyr::mutate(
      .tune_complete = rowSums(!is.na(dplyr::pick(dplyr::all_of(tune_cols))), na.rm = TRUE),
      .tie_break = stats::runif(dplyr::n())
    ) |>
    dplyr::group_by(species_name) |>
    dplyr::arrange(dplyr::desc(.tune_complete), .tie_break, .by_group = TRUE) |>
    dplyr::slice_head(n = max_models_per_species) |>
    dplyr::ungroup() |>
    dplyr::select(-.tune_complete, -.tie_break)

  if (nrow(out) < 2) {
    stop("The tuning subset must contain at least two usable models.", call. = FALSE)
  }

  out
}

#' Build one resampled tuning subset
#'
#' @param candidate_models Prepared candidate-model table.
#' @param species_weights Named numeric species-trait weight vector.
#' @param study_weights Named numeric study-trait weight vector.
#' @param max_models_per_species Maximum number of retained models per species.
#' @param seed Integer seed used for the within-species resampling step.
#'
#' @return A tibble.
#'
#' @keywords internal
build_resample_subset <- function(candidate_models,
                                  species_weights,
                                  study_weights,
                                  max_models_per_species,
                                  seed) {
  # Mirror the single-pass tuning subset rules so resampling works across
  # either the new or legacy prepared interval naming scheme.
  study_length_min_col <- resolve_similarity_column_name(
    candidate_models,
    c("study_length_min", "length_minimum")
  )
  study_length_max_col <- resolve_similarity_column_name(
    candidate_models,
    c("study_length_max", "length_maximum")
  )
  study_length_mid_col <- resolve_similarity_column_name(
    candidate_models,
    c("study_length_midpoint", "length_midpoint")
  )
  species_length_max_col <- resolve_similarity_column_name(
    candidate_models,
    c("species_length_max", "length_max")
  )

  # Score row completeness using the same trait set as the single-run tuner so
  # the resampling step still favors models with better metadata coverage.
  tune_cols <- unique(c(
    "species_name", "slope_len", "intercept_len",
    study_length_min_col, study_length_max_col, study_length_mid_col,
    species_length_max_col, "frequency",
    names(species_weights), names(study_weights)
  ))
  tune_cols <- intersect(tune_cols, names(candidate_models))

  set.seed(seed)

  # Rank rows within species by tuning completeness first, then sample from a
  # small top-ranked pool so the resampled subsets stay plausible while still
  # varying across repeats.
  out <- tibble::as_tibble(candidate_models) |>
    dplyr::filter(is.finite(slope_len), is.finite(intercept_len)) |>
    dplyr::mutate(
      .tune_complete = rowSums(!is.na(dplyr::pick(dplyr::all_of(tune_cols))), na.rm = TRUE)
    ) |>
    dplyr::group_by(species_name) |>
    dplyr::arrange(dplyr::desc(.tune_complete), .by_group = TRUE) |>
    dplyr::group_modify(function(.x, .y) {
      n_take <- min(max_models_per_species, nrow(.x))
      if (nrow(.x) <= n_take) {
        return(.x)
      }

      # Sample from only the upper completeness-ranked rows so a resample does
      # not drift into clearly lower-quality models for the same species.
      top_pool_n <- min(max(n_take * 3L, n_take), nrow(.x))
      pool <- .x |>
        dplyr::slice_head(n = top_pool_n)

      pool |>
        dplyr::slice_sample(n = n_take, replace = FALSE)
    }) |>
    dplyr::ungroup() |>
    dplyr::select(-.tune_complete)

  if (nrow(out) < 2) {
    stop("The resampled tuning subset must contain at least two usable models.", call. = FALSE)
  }

  out
}

#' Compute one interval-coherence distance
#'
#' @param anchor_min Numeric scalar.
#' @param anchor_max Numeric scalar.
#' @param cand_min Numeric scalar.
#' @param cand_max Numeric scalar.
#' @param method Character scalar.
#'
#' @return A numeric scalar.
#'
#' @keywords internal
interval_overlap_distance <- function(anchor_min,
                                      anchor_max,
                                      cand_min,
                                      cand_max,
                                      method) {
  # Return `NA` when the coherence term is disabled or any interval endpoint is
  # unavailable so the caller can omit that component from the kernel.
  if (identical(method, "none")) {
    return(NA_real_)
  }

  if (!is.finite(anchor_min) || !is.finite(anchor_max) ||
      !is.finite(cand_min) || !is.finite(cand_max)) {
    return(NA_real_)
  }

  # Convert the anchor and candidate intervals to low/high form before
  # measuring their overlap relative to the anchor interval width.
  a_lo <- min(anchor_min, anchor_max)
  a_hi <- max(anchor_min, anchor_max)
  c_lo <- min(cand_min, cand_max)
  c_hi <- max(cand_min, cand_max)
  inter <- max(0, min(a_hi, c_hi) - max(a_lo, c_lo))
  a_len <- max(1e-9, a_hi - a_lo)

  1 - (inter / a_len)
}

#' Compute one frequency-coherence distance
#'
#' @param anchor_freq Numeric scalar.
#' @param cand_freq Numeric scalar.
#' @param method Character scalar.
#' @param freq_span Numeric scalar used for numeric-frequency scaling.
#'
#' @return A numeric scalar.
#'
#' @keywords internal
frequency_offset_distance <- function(anchor_freq,
                                      cand_freq,
                                      method,
                                      freq_span) {
  # Frequency coherence can be disabled, treated as label mismatch, or scaled
  # by actual magnitude difference on the log-frequency axis.
  if (identical(method, "none")) {
    return(NA_real_)
  }

  if (!is.finite(anchor_freq) || !is.finite(cand_freq) ||
      anchor_freq <= 0 || cand_freq <= 0) {
    return(NA_real_)
  }

  if (identical(method, "label")) {
    # Treat rounded frequency labels as categorical bins when the caller wants
    # mismatch-only frequency coherence.
    return(as.numeric(as.integer(round(anchor_freq)) != as.integer(round(cand_freq))))
  }

  # Otherwise scale the absolute log-frequency difference by the observed
  # positive span so the result stays on a comparable 0-1-ish scale.
  pmin(abs(log(cand_freq / anchor_freq)) / freq_span, 1)
}

#' Evaluate the reference length for one model row
#'
#' @param row_df One-row data frame.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
reference_length <- function(row_df) {
  study_length_mid_col <- resolve_similarity_column_name(
    row_df,
    c("study_length_midpoint", "length_midpoint")
  )
  study_length_min_col <- resolve_similarity_column_name(
    row_df,
    c("study_length_min", "length_minimum")
  )
  study_length_max_col <- resolve_similarity_column_name(
    row_df,
    c("study_length_max", "length_maximum")
  )
  species_length_max_col <- resolve_similarity_column_name(
    row_df,
    c("species_length_max", "length_max")
  )

  # Prefer an explicit midpoint when present, otherwise derive one from the
  # reported interval, and finally fall back to half of the species maximum.
  if (is.character(study_length_mid_col) && is.finite(row_df[[study_length_mid_col]][[1]])) {
    return(as.numeric(row_df[[study_length_mid_col]][[1]]))
  }

  if (is.character(study_length_min_col) &&
      is.character(study_length_max_col) &&
      is.finite(row_df[[study_length_min_col]][[1]]) &&
      is.finite(row_df[[study_length_max_col]][[1]])) {
    return(mean(c(row_df[[study_length_min_col]][[1]], row_df[[study_length_max_col]][[1]])))
  }

  if (is.character(species_length_max_col) &&
      is.finite(row_df[[species_length_max_col]][[1]]) &&
      row_df[[species_length_max_col]][[1]] > 0) {
    return(as.numeric(row_df[[species_length_max_col]][[1]]) / 2)
  }

  NA_real_
}

#' Score one fixed similarity configuration
#'
#' @param models_subset Candidate-model subset used for leave-one-out scoring.
#' @param species_weights Named numeric species-trait weight vector.
#' @param study_weights Named numeric study-trait weight vector.
#' @param alpha_now Numeric scalar.
#' @param k_species_now Numeric scalar.
#' @param k_study_now Numeric scalar.
#' @param cfg_now Normalized similarity config list.
#' @param registry_path Optional path to the trait-registry JSON.
#' @param seed_now Integer seed.
#'
#' @return A one-row tibble with `rmse`, `mae`, and `n_eval`.
#'
#' @keywords internal
score_similarity_config <- function(models_subset,
                                    species_weights,
                                    study_weights,
                                    alpha_now,
                                    k_species_now,
                                    k_study_now,
                                    cfg_now,
                                    registry_path,
                                    seed_now) {
  # Rebuild the prepared similarity inputs for the exact weight/config state
  # being scored so the leave-one-out pass sees the correct matrices.
  sim_obj <- prepare_similarity_matrix(
    candidate_models = models_subset,
    species_traits = as.list(species_weights),
    study_traits = as.list(study_weights),
    alpha = alpha_now,
    k_species = k_species_now,
    k_study = k_study_now,
    config = cfg_now,
    registry_path = registry_path,
    seed = seed_now
  )

  # Build the prepared species and study distance matrices only after the
  # similarity inputs have been resolved for the current scoring state.
  dist_obj <- build_gower_distances(sim_obj)
  species_dist_model <- dist_obj$species_dist_model
  study_dist <- dist_obj$study_dist

  model_ids <- sim_obj$candidate_models$model_id_chr
  model_n <- length(model_ids)

  # Resolve the study interval column names once so the scorer can work with
  # either the current prepared schema or older cached candidate-model tables.
  study_length_min_col <- resolve_similarity_column_name(
    sim_obj$candidate_models,
    c("study_length_min", "length_minimum")
  )
  study_length_max_col <- resolve_similarity_column_name(
    sim_obj$candidate_models,
    c("study_length_max", "length_maximum")
  )
  study_depth_min_col <- resolve_similarity_column_name(
    sim_obj$candidate_models,
    c("study_depth_min", "depth_minimum")
  )
  study_depth_max_col <- resolve_similarity_column_name(
    sim_obj$candidate_models,
    c("study_depth_max", "depth_maximum")
  )

  # Use the length-form TS equation directly during leave-one-out scoring so
  # each candidate prediction is evaluated at the same anchor reference length.
  ts_at_len <- function(slope, intercept, length_cm) {
    as.numeric(slope) * log10(length_cm) + as.numeric(intercept)
  }

  resolve_numeric_col <- function(df_now,
                                  col_nm) {
    if (!is.character(col_nm) || !nzchar(col_nm) || !(col_nm %in% names(df_now))) {
      return(rep(NA_real_, nrow(df_now)))
    }
    out <- suppressWarnings(as.numeric(df_now[[col_nm]]))
    out[!is.finite(out)] <- NA_real_
    out
  }

  # Precompute coherence-distance matrices once for the current scoring
  # configuration so leave-one-out evaluation can reuse the pairwise values.
  models_df <- sim_obj$candidate_models
  len_min_vals <- resolve_numeric_col(models_df, study_length_min_col)
  len_max_vals <- resolve_numeric_col(models_df, study_length_max_col)
  dep_min_vals <- resolve_numeric_col(models_df, study_depth_min_col)
  dep_max_vals <- resolve_numeric_col(models_df, study_depth_max_col)
  freq_vals <- resolve_numeric_col(models_df, "frequency")

  interval_distance_matrix <- function(min_vals,
                                       max_vals,
                                       method) {
    out <- matrix(NA_real_, nrow = model_n, ncol = model_n)
    diag(out) <- 0
    for (i in seq_len(model_n)) {
      out[, i] <- vapply(
        seq_len(model_n),
        function(j) {
          interval_overlap_distance(
            anchor_min = min_vals[[i]],
            anchor_max = max_vals[[i]],
            cand_min = min_vals[[j]],
            cand_max = max_vals[[j]],
            method = method
          )
        },
        numeric(1)
      )
    }
    out
  }

  frequency_distance_matrix <- matrix(NA_real_, nrow = model_n, ncol = model_n)
  diag(frequency_distance_matrix) <- 0
  for (i in seq_len(model_n)) {
    frequency_distance_matrix[, i] <- vapply(
      seq_len(model_n),
      function(j) {
        frequency_offset_distance(
          anchor_freq = freq_vals[[i]],
          cand_freq = freq_vals[[j]],
          method = cfg_now$frequency_coherence$method,
          freq_span = sim_obj$frequency_span
        )
      },
      numeric(1)
    )
  }

  length_distance_matrix <- interval_distance_matrix(
    min_vals = len_min_vals,
    max_vals = len_max_vals,
    method = cfg_now$length_coherence$method
  )
  depth_distance_matrix <- interval_distance_matrix(
    min_vals = dep_min_vals,
    max_vals = dep_max_vals,
    method = cfg_now$depth_coherence$method
  )

  errs <- numeric(0)

  # Evaluate each tuning-row as the held-out anchor, using all remaining rows
  # as candidate donor models.
  for (j in seq_len(model_n)) {
    target <- models_df[j, , drop = FALSE]
    anchor_id <- target$model_id_chr[[1]]
    length_cm <- reference_length(target)

    if (!is.finite(length_cm) || length_cm <= 0) {
      next
    }

    target_ts <- ts_at_len(target$slope_len[[1]], target$intercept_len[[1]], length_cm)
    if (!is.finite(target_ts)) {
      next
    }

    # Remove the held-out anchor row before computing donor weights.
    train <- models_df |>
      dplyr::filter(model_id_chr != anchor_id)

    train_idx <- which(model_ids != anchor_id)

    if (nrow(train) == 0) {
      next
    }

    # Evaluate species, study, and coherence distances separately so each
    # component can be weighted independently in the final kernel.
    d_species <- species_dist_model[train_idx, j]
    d_study <- study_dist[train_idx, j]
    d_length <- length_distance_matrix[train_idx, j]
    d_depth <- depth_distance_matrix[train_idx, j]
    d_freq <- frequency_distance_matrix[train_idx, j]

    kernel_species <- ifelse(is.finite(d_species), alpha_now * k_species_now * d_species, 0)
    kernel_study <- ifelse(is.finite(d_study), (1 - alpha_now) * k_study_now * d_study, 0)
    kernel_length <- ifelse(is.finite(d_length), cfg_now$length_coherence$weight * d_length, 0)
    kernel_depth <- ifelse(is.finite(d_depth), cfg_now$depth_coherence$weight * d_depth, 0)
    kernel_freq <- ifelse(is.finite(d_freq), cfg_now$frequency_coherence$weight * d_freq, 0)

    # Combine the species, study, and coherence penalties in one exponential
    # kernel, then normalize them to donor weights.
    w_raw <- exp(-(kernel_species + kernel_study + kernel_length + kernel_depth + kernel_freq))
    w_raw[!is.finite(w_raw)] <- 0
    w_sum <- sum(w_raw, na.rm = TRUE)
    if (!is.finite(w_sum) || w_sum <= 0) {
      next
    }
    w <- w_raw / w_sum

    pred_ts <- sum(ts_at_len(train$slope_len, train$intercept_len, length_cm) * w, na.rm = TRUE)
    if (!is.finite(pred_ts)) {
      next
    }

    # Store signed prediction errors so both RMSE and MAE can be derived from
    # one leave-one-out pass.
    errs <- c(errs, pred_ts - target_ts)
  }

  if (length(errs) == 0) {
    return(tibble::tibble(rmse = NA_real_, mae = NA_real_, n_eval = 0L))
  }

  # Summarize the held-out prediction errors only after the full leave-one-out
  # pass completes.
  tibble::tibble(
    rmse = sqrt(mean(errs^2)),
    mae = mean(abs(errs)),
    n_eval = length(errs)
  )
}

#' Run the local alpha-k tuning grid
#'
#' @param tune_models Tuning subset returned by [build_tuning_subset()].
#' @param base_sim Prepared similarity object returned by
#'   [prepare_similarity_matrix()].
#' @param registry_path Optional path to the trait-registry JSON.
#'
#' @return A list with `baseline`, `grid_scores`, `alpha_best`, `k_species_best`,
#'   and `k_study_best`.
#'
#' @keywords internal
run_tuning_grid_search <- function(tune_models,
                                   base_sim,
                                   registry_path,
                                   n_cores = 1L) {
  # Score the caller-supplied starting configuration first so the tuning
  # history preserves the baseline before any grid search occurs.
  base_score <- score_similarity_config(
    models_subset = tune_models,
    species_weights = base_sim$species_weights,
    study_weights = base_sim$study_weights,
    alpha_now = base_sim$alpha,
    k_species_now = base_sim$k_species,
    k_study_now = base_sim$k_study,
    cfg_now = base_sim$config,
    registry_path = registry_path,
    seed_now = base_sim$seed
  ) |>
    dplyr::mutate(
      stage = "baseline",
      alpha = base_sim$alpha,
      k_species = base_sim$k_species,
      k_study = base_sim$k_study
    )

  # Search the local alpha/k grid while holding the starting trait weights
  # fixed, then retain the best-performing configuration for the component
  # drop-out scan.
  grid <- expand.grid(
    alpha = base_sim$config$alpha_grid,
    k_species = base_sim$config$k_species_grid,
    k_study = base_sim$config$k_study_grid,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  parallel_lapply_internal <- function(X,
                                       FUN,
                                       n_cores,
                                       export = NULL,
                                       envir = parent.frame()) {
    n_cores <- suppressWarnings(as.integer(n_cores[[1]]))
    if (!is.finite(n_cores) || n_cores < 2L || length(X) < 2L) {
      return(lapply(X, FUN))
    }

    cl <- parallel::makeCluster(min(length(X), n_cores))
    on.exit(parallel::stopCluster(cl), add = TRUE)
    if (!is.null(export) && length(export) > 0) {
      parallel::clusterExport(cl, varlist = unique(export), envir = envir)
    }
    parallel::parLapply(cl, X = X, fun = FUN)
  }

  # Score each alpha/k combination independently, then bind the per-grid-row
  # results into one searchable tuning table.
  grid_scores <- parallel_lapply_internal(
    X = seq_len(nrow(grid)),
    FUN = function(i) {
    score_similarity_config(
      models_subset = tune_models,
      species_weights = base_sim$species_weights,
      study_weights = base_sim$study_weights,
      alpha_now = grid$alpha[[i]],
      k_species_now = grid$k_species[[i]],
      k_study_now = grid$k_study[[i]],
      cfg_now = base_sim$config,
      registry_path = registry_path,
      seed_now = base_sim$seed
    ) |>
      dplyr::mutate(
        stage = "alpha_k_grid",
        alpha = grid$alpha[[i]],
        k_species = grid$k_species[[i]],
        k_study = grid$k_study[[i]]
      )
    },
    n_cores = n_cores,
    export = c("grid", "tune_models", "base_sim", "registry_path", "score_similarity_config")
  )
  grid_scores <- dplyr::bind_rows(grid_scores)

  # Rank by RMSE first, then MAE, and finally by how many usable evaluations
  # each grid configuration produced.
  best_cfg <- grid_scores |>
    dplyr::arrange(rmse, mae, dplyr::desc(n_eval)) |>
    dplyr::slice(1)

  list(
    baseline = base_score,
    grid_scores = grid_scores,
    alpha_best = best_cfg$alpha[[1]],
    k_species_best = best_cfg$k_species[[1]],
    k_study_best = best_cfg$k_study[[1]]
  )
}

#' Run one-at-a-time component drop-out scans
#'
#' @param tune_models Tuning subset returned by [build_tuning_subset()].
#' @param base_sim Prepared similarity object returned by
#'   [prepare_similarity_matrix()].
#' @param alpha_best Tuned alpha value.
#' @param k_species_best Tuned species-kernel value.
#' @param k_study_best Tuned study-kernel value.
#' @param registry_path Optional path to the trait-registry JSON.
#'
#' @return A tibble.
#'
#' @keywords internal
run_component_dropout <- function(tune_models,
                                  base_sim,
                                  alpha_best,
                                  k_species_best,
                                  k_study_best,
                                  registry_path,
                                  n_cores = 1L) {
  # Start from the full tuned alpha/k setting, then drop one component at a
  # time to quantify how much performance degrades without it.
  full_score <- score_similarity_config(
    models_subset = tune_models,
    species_weights = base_sim$species_weights,
    study_weights = base_sim$study_weights,
    alpha_now = alpha_best,
    k_species_now = k_species_best,
    k_study_now = k_study_best,
    cfg_now = base_sim$config,
    registry_path = registry_path,
    seed_now = base_sim$seed
  ) |>
    dplyr::mutate(
      component = "full_model",
      component_type = "full_model",
      alpha = alpha_best,
      k_species = k_species_best,
      k_study = k_study_best
    )

  dropout_rows <- list(full_score)

  parallel_lapply_internal <- function(X,
                                       FUN,
                                       n_cores,
                                       export = NULL,
                                       envir = parent.frame()) {
    n_cores <- suppressWarnings(as.integer(n_cores[[1]]))
    if (!is.finite(n_cores) || n_cores < 2L || length(X) < 2L) {
      return(lapply(X, FUN))
    }

    cl <- parallel::makeCluster(min(length(X), n_cores))
    on.exit(parallel::stopCluster(cl), add = TRUE)
    if (!is.null(export) && length(export) > 0) {
      parallel::clusterExport(cl, varlist = unique(export), envir = envir)
    }
    parallel::parLapply(cl, X = X, fun = FUN)
  }

  tasks <- list()

  for (trait_nm in names(base_sim$species_weights)) {
    base_weight <- suppressWarnings(as.numeric(base_sim$species_weights[[trait_nm]]))
    if (is.finite(base_weight) && base_weight > 0) {
      tasks[[length(tasks) + 1]] <- list(component = trait_nm, component_type = "species_trait")
    }
  }

  for (trait_nm in names(base_sim$study_weights)) {
    base_weight <- suppressWarnings(as.numeric(base_sim$study_weights[[trait_nm]]))
    if (is.finite(base_weight) && base_weight > 0) {
      tasks[[length(tasks) + 1]] <- list(component = trait_nm, component_type = "study_trait")
    }
  }

  for (comp_nm in c("length_coherence", "depth_coherence", "frequency_coherence")) {
    if (isTRUE(base_sim$config[[comp_nm]]$weight > 0)) {
      tasks[[length(tasks) + 1]] <- list(component = comp_nm, component_type = "coherence")
    }
  }

  task_rows <- parallel_lapply_internal(
    X = tasks,
    FUN = function(task_now) {
      if (identical(task_now$component_type, "species_trait")) {
        species_weights_now <- base_sim$species_weights
        species_weights_now[[task_now$component]] <- 0
        return(
          score_similarity_config(
            models_subset = tune_models,
            species_weights = species_weights_now,
            study_weights = base_sim$study_weights,
            alpha_now = alpha_best,
            k_species_now = k_species_best,
            k_study_now = k_study_best,
            cfg_now = base_sim$config,
            registry_path = registry_path,
            seed_now = base_sim$seed
          ) |>
            dplyr::mutate(
              component = task_now$component,
              component_type = "species_trait",
              alpha = alpha_best,
              k_species = k_species_best,
              k_study = k_study_best
            )
        )
      }

      if (identical(task_now$component_type, "study_trait")) {
        study_weights_now <- base_sim$study_weights
        study_weights_now[[task_now$component]] <- 0
        return(
          score_similarity_config(
            models_subset = tune_models,
            species_weights = base_sim$species_weights,
            study_weights = study_weights_now,
            alpha_now = alpha_best,
            k_species_now = k_species_best,
            k_study_now = k_study_best,
            cfg_now = base_sim$config,
            registry_path = registry_path,
            seed_now = base_sim$seed
          ) |>
            dplyr::mutate(
              component = task_now$component,
              component_type = "study_trait",
              alpha = alpha_best,
              k_species = k_species_best,
              k_study = k_study_best
            )
        )
      }

      cfg_now <- base_sim$config
      cfg_now[[task_now$component]]$weight <- 0
      score_similarity_config(
        models_subset = tune_models,
        species_weights = base_sim$species_weights,
        study_weights = base_sim$study_weights,
        alpha_now = alpha_best,
        k_species_now = k_species_best,
        k_study_now = k_study_best,
        cfg_now = cfg_now,
        registry_path = registry_path,
        seed_now = base_sim$seed
      ) |>
        dplyr::mutate(
          component = task_now$component,
          component_type = "coherence",
          alpha = alpha_best,
          k_species = k_species_best,
          k_study = k_study_best
        )
    },
    n_cores = n_cores,
    export = c("tasks", "base_sim", "tune_models", "alpha_best", "k_species_best", "k_study_best", "registry_path", "score_similarity_config")
  )

  dropout_rows <- c(dropout_rows, task_rows)

  # Express every drop-out result relative to the full model so later weighting
  # code can work with simple performance deltas.
  component_impact_summary <- dplyr::bind_rows(dropout_rows)
  # Use the full-model score as the baseline for every component delta.
  full_rmse <- full_score$rmse[[1]]
  full_mae <- full_score$mae[[1]]

  component_impact_summary |>
    dplyr::mutate(
      delta_rmse = rmse - full_rmse,
      delta_mae = mae - full_mae
    )
}

#' Apply component-impact multipliers
#'
#' @param base_sim Prepared similarity object returned by
#'   [prepare_similarity_matrix()].
#' @param component_impact_summary Component-impact summary returned by
#'   [run_component_dropout()].
#'
#' @return A list with tuned species weights, study weights, and coherence
#'   config.
#'
#' @keywords internal
apply_component_weights <- function(base_sim,
                                    component_impact_summary) {
  # Convert positive RMSE degradation into bounded multipliers so more
  # influential components receive larger retained weights.
  positive_delta <- component_impact_summary$delta_rmse[
    component_impact_summary$component != "full_model" &
      is.finite(component_impact_summary$delta_rmse) &
      component_impact_summary$delta_rmse > 0
  ]
  max_delta <- if (length(positive_delta) == 0) 1 else max(positive_delta)

  # Map each dropped component onto a bounded multiplier, with non-positive
  # impact collapsing to the minimum retained weight.
  component_multiplier <- component_impact_summary |>
    dplyr::filter(component != "full_model") |>
    dplyr::transmute(
      component,
      multiplier = dplyr::if_else(
        delta_rmse > 0,
        0.5 + 1.5 * (delta_rmse / max_delta),
        0.5
      )
    )

  # Copy the starting weights/config so tuning scales them in place without
  # mutating the original prepared similarity object.
  species_weights_tuned <- base_sim$species_weights
  study_weights_tuned <- base_sim$study_weights
  cfg_tuned <- base_sim$config

  # Keep tuned trait weights numerically stable so one component cannot absorb
  # most of the total distance mass after dropout-based scaling.
  stabilize_weights <- function(weights_now,
                                base_weights,
                                max_trait_share) {
    w <- as.numeric(weights_now)
    names(w) <- names(weights_now)
    b <- as.numeric(base_weights)
    names(b) <- names(base_weights)

    w[!is.finite(w) | w < 0] <- 0
    b[!is.finite(b) | b < 0] <- 0

    target_sum <- sum(b, na.rm = TRUE)
    if (!is.finite(target_sum) || target_sum <= 0) {
      target_sum <- sum(w, na.rm = TRUE)
    }
    if (!is.finite(target_sum) || target_sum <= 0) {
      return(stats::setNames(w, names(weights_now)))
    }

    w_sum <- sum(w, na.rm = TRUE)
    if (!is.finite(w_sum) || w_sum <= 0) {
      return(stats::setNames(b, names(weights_now)))
    }

    # Preserve total trait mass so k/alpha tuning remains comparable.
    w <- w * (target_sum / w_sum)

    if (!is.finite(max_trait_share) || max_trait_share <= 0 || max_trait_share >= 1) {
      return(stats::setNames(w, names(weights_now)))
    }

    max_allowed <- max_trait_share * target_sum
    over_idx <- which(w > max_allowed)
    if (length(over_idx) == 0) {
      return(stats::setNames(w, names(weights_now)))
    }

    excess <- sum(w[over_idx] - max_allowed)
    w[over_idx] <- max_allowed

    under_idx <- which(w < max_allowed)
    if (length(under_idx) > 0 && excess > 0) {
      under_sum <- sum(w[under_idx], na.rm = TRUE)
      if (is.finite(under_sum) && under_sum > 0) {
        w[under_idx] <- w[under_idx] + excess * (w[under_idx] / under_sum)
      } else {
        w[under_idx] <- w[under_idx] + excess / length(under_idx)
      }
    }

    stats::setNames(w, names(weights_now))
  }

  # Keep stabilization internal and data-driven so tuning remains robust
  # without exposing another external hyperparameter.
  auto_max_share <- function(base_weights) {
    b <- as.numeric(base_weights)
    b <- b[is.finite(b) & b > 0]
    if (length(b) == 0) {
      return(0.15)
    }

    # Start from the baseline concentration and allow moderate flexibility,
    # while preventing single-trait dominance.
    base_share <- max(b) / sum(b)
    min(0.18, max(0.12, base_share * 1.1))
  }

  # Reweight each selected species trait by its component-specific multiplier.
  for (nm in names(species_weights_tuned)) {
    mult <- component_multiplier$multiplier[match(nm, component_multiplier$component)]
    if (length(mult) == 1 && is.finite(mult)) {
      species_weights_tuned[[nm]] <- species_weights_tuned[[nm]] * mult
    }
  }

  species_weights_tuned <- stabilize_weights(
    weights_now = species_weights_tuned,
    base_weights = base_sim$species_weights,
    max_trait_share = auto_max_share(base_sim$species_weights)
  )

  # Apply the same multiplier lookup to the selected study traits.
  for (nm in names(study_weights_tuned)) {
    mult <- component_multiplier$multiplier[match(nm, component_multiplier$component)]
    if (length(mult) == 1 && is.finite(mult)) {
      study_weights_tuned[[nm]] <- study_weights_tuned[[nm]] * mult
    }
  }

  study_weights_tuned <- stabilize_weights(
    weights_now = study_weights_tuned,
    base_weights = base_sim$study_weights,
    max_trait_share = auto_max_share(base_sim$study_weights)
  )

  # Scale the enabled coherence weights using the same component-impact table.
  for (nm in c("length_coherence", "depth_coherence", "frequency_coherence")) {
    mult <- component_multiplier$multiplier[match(nm, component_multiplier$component)]
    if (length(mult) == 1 && is.finite(mult)) {
      cfg_tuned[[nm]]$weight <- cfg_tuned[[nm]]$weight * mult
    }
  }

  # Return the tuned trait and coherence weights together so the caller can use
  # them as one coherent tuned similarity configuration.
  list(
    species_weights = species_weights_tuned,
    study_weights = study_weights_tuned,
    config = cfg_tuned
  )
}

#' Collect per-component tuned multipliers
#'
#' @param tune_obj Result returned by [tune_similarity_empirical()].
#' @param resample_id Integer resample identifier.
#'
#' @return A tibble.
#'
#' @keywords internal
collect_component_weights <- function(tune_obj,
                               resample_id) {
  # Pull the drop-out deltas once so every returned component row carries the
  # same empirical performance context used during tuning.
  impact_tbl <- tibble::as_tibble(tune_obj$component_impact_summary)

  # Build species-trait multiplier rows from the tuned-to-base weight ratios.
  species_tbl <- tibble::tibble(
    component = names(tune_obj$config_base$species_weights),
    component_type = "species_trait",
    base_weight = as.numeric(tune_obj$config_base$species_weights),
    tuned_weight = as.numeric(tune_obj$config_tuned$species_weights)
  )

  # Mirror the same ratio calculation for the selected study traits.
  study_tbl <- tibble::tibble(
    component = names(tune_obj$config_base$study_weights),
    component_type = "study_trait",
    base_weight = as.numeric(tune_obj$config_base$study_weights),
    tuned_weight = as.numeric(tune_obj$config_tuned$study_weights)
  )

  # Record coherence-term multipliers separately because those weights live
  # inside the config object rather than the trait-weight vectors.
  coherence_names <- intersect(
    c("length_coherence", "depth_coherence", "frequency_coherence"),
    names(tune_obj$config_base$coherence)
  )
  coherence_tbl <- tibble::tibble(
    component = coherence_names,
    component_type = "coherence",
    base_weight = vapply(
      coherence_names,
      function(nm) as.numeric(tune_obj$config_base$coherence[[nm]]$weight %||% NA_real_),
      numeric(1)
    ),
    tuned_weight = vapply(
      coherence_names,
      function(nm) as.numeric(tune_obj$config_tuned$coherence[[nm]]$weight %||% NA_real_),
      numeric(1)
    ),
    multiplier = vapply(
      coherence_names,
      function(nm) {
        base_w <- as.numeric(tune_obj$config_base$coherence[[nm]]$weight %||% NA_real_)
        tuned_w <- as.numeric(tune_obj$config_tuned$coherence[[nm]]$weight %||% NA_real_)
        if (!is.finite(base_w) || base_w == 0) {
          return(NA_real_)
        }
        tuned_w / base_w
      },
      numeric(1)
    )
  )

  # Combine all component types and attach the empirical drop-out deltas used
  # to derive the tuned multipliers.
  dplyr::bind_rows(species_tbl, study_tbl) |>
    dplyr::mutate(
      multiplier = dplyr::if_else(
        is.finite(base_weight) & base_weight != 0,
        tuned_weight / base_weight,
        NA_real_
      )
    ) |>
    dplyr::bind_rows(coherence_tbl) |>
    dplyr::left_join(
      impact_tbl |>
        dplyr::select(component, delta_rmse, delta_mae),
      by = "component"
    ) |>
    dplyr::mutate(resample_id = resample_id, .before = 1)
}

#' Empirically tune the similarity configuration
#'
#' Builds a reduced tuning subset, prepares the selected similarity inputs,
#' evaluates the supplied similarity
#' configuration by leave-one-out error, tunes the alpha and kernel parameters
#' on a local grid, then drops each selected trait and enabled coherence term
#' to derive tuned weight multipliers.
#'
#' @param candidate_models Prepared candidate-model table.
#' @param species_traits Optional species-trait specification. See
#'   [prepare_similarity_matrix()] for the accepted forms. When `NULL`, a
#'   config-supplied value is used when present.
#' @param study_traits Optional study-trait specification. See
#'   [prepare_similarity_matrix()] for the accepted forms. When `NULL`, a
#'   config-supplied value is used when present.
#' @param alpha Optional starting species-versus-study mixing parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param k_species Optional starting species-distance kernel parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param k_study Optional starting study-distance kernel parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param max_models_per_species Maximum number of retained tuning models per
#'   species. When `NULL`, a config-supplied value is used when present.
#' @param seed Optional integer seed. When `NULL`, a config-supplied value is
#'   used when present; otherwise one is generated and returned in the output
#'   object.
#' @param config Optional JSON path or list with similarity options. Supported
#'   entries are `species_traits`, `study_traits`, `alpha`, `k_species`,
#'   `k_study`, `max_models_per_species`, `seed`, `length_coherence`,
#'   `depth_coherence`, `frequency_coherence`, `alpha_grid`,
#'   `k_species_grid`, and `k_study_grid`.
#' @param cache_path Optional path to an `.rds` cache file.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache file.
#' @param registry_path Optional path to a trait-registry JSON file.
#'
#' @return A list containing the tuned configuration, tuning subset, score
#'   history, and per-trait component-impact summary.
#'
#' @export
tune_similarity_empirical <- function(candidate_models,
                                      species_traits = NULL,
                                      study_traits = NULL,
                                      alpha = NULL,
                                      k_species = NULL,
                                      k_study = NULL,
                                      max_models_per_species = NULL,
                                      n_resamples = NULL,
                                      seed = NULL,
                                      config = NULL,
                                      cache_path = NULL,
                                      refresh = FALSE,
                                      registry_path = NULL) {
  # Validate cache control first so cached and uncached runs share the same
  # argument rules.
  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Resolve the optional tuning config once so direct args can override it
  # consistently.
  cfg_user <- read_similarity_config(config)
  max_models_per_species <- max_models_per_species %||% cfg_user$max_models_per_species %||% 2L
  n_resamples <- n_resamples %||% cfg_user$n_resamples
  n_cores <- cfg_user$n_cores %||% 1L
  use_resample_tuning <- !is.null(n_resamples)
  if (!is.numeric(max_models_per_species) ||
      length(max_models_per_species) != 1 ||
      !is.finite(max_models_per_species) ||
      max_models_per_species < 1) {
    stop("'max_models_per_species' must be one integer >= 1.", call. = FALSE)
  }
  max_models_per_species <- as.integer(max_models_per_species)
  if (!is.numeric(n_cores) || length(n_cores) != 1 || !is.finite(n_cores) || n_cores < 1) {
    stop("When supplied, 'n_cores' must be one integer >= 1.", call. = FALSE)
  }
  n_cores <- as.integer(n_cores)

  if (isTRUE(use_resample_tuning)) {
    if (!is.numeric(n_resamples) ||
        length(n_resamples) != 1 ||
        !is.finite(n_resamples) ||
        n_resamples < 1) {
      stop("When supplied, 'n_resamples' must be one integer >= 1.", call. = FALSE)
    }
    n_resamples <- as.integer(n_resamples)
  }

  # Return the cached tuning object immediately when available unless the
  # caller explicitly requested a refresh.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Prepare the baseline similarity inputs once against the full candidate
  # table so the trait set, starting weights, and search grid are fixed before
  # subset selection begins.
  base_sim <- prepare_similarity_matrix(
    candidate_models = candidate_models,
    species_traits = species_traits,
    study_traits = study_traits,
    alpha = alpha,
    k_species = k_species,
    k_study = k_study,
    config = config,
    registry_path = registry_path,
    seed = seed
  )

  # Harden caller-supplied grids so invalid values do not fail deep inside the
  # scoring loop, and keep a deterministic fallback around the starting point.
  alpha_grid_clean <- base_sim$config$alpha_grid
  alpha_grid_clean <- alpha_grid_clean[is.finite(alpha_grid_clean) & alpha_grid_clean > 0 & alpha_grid_clean < 1]
  if (length(alpha_grid_clean) == 0) {
    alpha_grid_clean <- c(max(0.05, base_sim$alpha - 0.2), base_sim$alpha, min(0.95, base_sim$alpha + 0.2))
  }
  base_sim$config$alpha_grid <- sort(unique(as.numeric(alpha_grid_clean)))

  k_species_grid_clean <- base_sim$config$k_species_grid
  k_species_grid_clean <- k_species_grid_clean[is.finite(k_species_grid_clean) & k_species_grid_clean >= 0]
  if (length(k_species_grid_clean) == 0) {
    k_species_grid_clean <- c(max(0, base_sim$k_species / 2), base_sim$k_species, base_sim$k_species * 2)
  }
  base_sim$config$k_species_grid <- sort(unique(as.numeric(k_species_grid_clean)))

  k_study_grid_clean <- base_sim$config$k_study_grid
  k_study_grid_clean <- k_study_grid_clean[is.finite(k_study_grid_clean) & k_study_grid_clean >= 0]
  if (length(k_study_grid_clean) == 0) {
    k_study_grid_clean <- c(max(0, base_sim$k_study / 2), base_sim$k_study, base_sim$k_study * 2)
  }
  base_sim$config$k_study_grid <- sort(unique(as.numeric(k_study_grid_clean)))

  # Draw the representative per-species tuning subset before any leave-one-out
  # scoring so heavily parameterized species do not dominate the objective.
  tune_models <- build_tuning_subset(
    candidate_models = base_sim$candidate_models,
    species_weights = base_sim$species_weights,
    study_weights = base_sim$study_weights,
    max_models_per_species = max_models_per_species,
    seed = base_sim$seed
  )

  # Tune the alpha/kernel scalars first with fixed starting trait weights.
  grid_obj <- run_tuning_grid_search(
    tune_models = tune_models,
    base_sim = base_sim,
    registry_path = registry_path,
    n_cores = n_cores
  )

  # Quantify component importance by dropping each selected trait and enabled
  # coherence term one at a time under the best alpha/kernel setting.
  component_impact_summary <- run_component_dropout(
    tune_models = tune_models,
    base_sim = base_sim,
    alpha_best = grid_obj$alpha_best,
    k_species_best = grid_obj$k_species_best,
    k_study_best = grid_obj$k_study_best,
    registry_path = registry_path,
    n_cores = n_cores
  )

  # Optionally stabilize tuning with lightweight resamples: keep one alpha/k
  # search result, then aggregate dropout-derived weights across resampled
  # subsets instead of rerunning full tuning per resample.
  resample_summary <- tibble::tibble()
  if (isTRUE(use_resample_tuning) && n_resamples > 1L) {
    set.seed(base_sim$seed)
    resample_seeds <- sample.int(.Machine$integer.max, n_resamples)
    tuned_rows <- vector("list", n_resamples)
    summary_rows <- vector("list", n_resamples)

    for (i in seq_len(n_resamples)) {
      sampled_subset <- build_resample_subset(
        candidate_models = base_sim$candidate_models,
        species_weights = base_sim$species_weights,
        study_weights = base_sim$study_weights,
        max_models_per_species = max_models_per_species,
        seed = resample_seeds[[i]]
      )

      comp_now <- run_component_dropout(
        tune_models = sampled_subset,
        base_sim = base_sim,
        alpha_best = grid_obj$alpha_best,
        k_species_best = grid_obj$k_species_best,
        k_study_best = grid_obj$k_study_best,
        registry_path = registry_path,
        n_cores = n_cores
      )

      tuned_now <- apply_component_weights(
        base_sim = base_sim,
        component_impact_summary = comp_now
      )

      full_now <- comp_now |>
        dplyr::filter(component == "full_model") |>
        dplyr::slice_tail(n = 1)

      summary_rows[[i]] <- tibble::tibble(
        resample_id = i,
        seed = resample_seeds[[i]],
        n_models = nrow(sampled_subset),
        n_species = dplyr::n_distinct(sampled_subset$species_name),
        rmse = full_now$rmse[[1]],
        mae = full_now$mae[[1]],
        n_eval = full_now$n_eval[[1]]
      )

      tuned_rows[[i]] <- list(
        species_weights = tuned_now$species_weights,
        study_weights = tuned_now$study_weights,
        coherence = tuned_now$config
      )
    }

    resample_summary <- dplyr::bind_rows(summary_rows)

    aggregate_weight <- function(weight_name,
                                 weight_kind) {
      vals <- vapply(
        tuned_rows,
        function(x) {
          if (identical(weight_kind, "species")) {
            return(as.numeric(x$species_weights[[weight_name]] %||% NA_real_))
          }
          if (identical(weight_kind, "study")) {
            return(as.numeric(x$study_weights[[weight_name]] %||% NA_real_))
          }
          as.numeric(x$coherence[[weight_name]]$weight %||% NA_real_)
        },
        numeric(1)
      )
      vals <- vals[is.finite(vals)]
      if (length(vals) == 0) {
        return(NA_real_)
      }
      stats::median(vals)
    }

    species_weights_resampled <- base_sim$species_weights
    for (nm in names(species_weights_resampled)) {
      agg <- aggregate_weight(nm, "species")
      if (is.finite(agg)) {
        species_weights_resampled[[nm]] <- agg
      }
    }

    study_weights_resampled <- base_sim$study_weights
    for (nm in names(study_weights_resampled)) {
      agg <- aggregate_weight(nm, "study")
      if (is.finite(agg)) {
        study_weights_resampled[[nm]] <- agg
      }
    }

    cfg_resampled <- base_sim$config
    for (nm in c("length_coherence", "depth_coherence", "frequency_coherence")) {
      if (!nm %in% names(cfg_resampled)) {
        next
      }
      agg <- aggregate_weight(nm, "coherence")
      if (is.finite(agg)) {
        cfg_resampled[[nm]]$weight <- agg
      }
    }

    tuned_obj <- list(
      species_weights = species_weights_resampled,
      study_weights = study_weights_resampled,
      config = cfg_resampled
    )
  } else {
    # Translate the component-impact deltas into tuned trait/coherence
    # multipliers from the single representative subset.
    tuned_obj <- apply_component_weights(
      base_sim = base_sim,
      component_impact_summary = component_impact_summary
    )
  }

  # Re-score the configuration after applying the tuned multipliers so the
  # returned history includes the final post-tuning performance.
  final_score <- score_similarity_config(
    models_subset = tune_models,
    species_weights = tuned_obj$species_weights,
    study_weights = tuned_obj$study_weights,
    alpha_now = grid_obj$alpha_best,
    k_species_now = grid_obj$k_species_best,
    k_study_now = grid_obj$k_study_best,
    cfg_now = tuned_obj$config,
    registry_path = registry_path,
    seed_now = base_sim$seed
  ) |>
    dplyr::mutate(
      stage = "final_tuned",
      alpha = grid_obj$alpha_best,
      k_species = grid_obj$k_species_best,
      k_study = grid_obj$k_study_best
    )

  # Return both the starting and tuned configurations alongside the score
  # history needed to inspect how the empirical tuning behaved.
  result <- list(
    config_base = list(
      species_weights = base_sim$species_weights,
      study_weights = base_sim$study_weights,
      alpha = base_sim$alpha,
      k_species = base_sim$k_species,
      k_study = base_sim$k_study,
      coherence = base_sim$config
    ),
    config_tuned = list(
      species_weights = tuned_obj$species_weights,
      study_weights = tuned_obj$study_weights,
      alpha = grid_obj$alpha_best,
      k_species = grid_obj$k_species_best,
      k_study = grid_obj$k_study_best,
      coherence = tuned_obj$config
    ),
    tune_models = tune_models,
    tuning_history = dplyr::bind_rows(
      grid_obj$baseline,
      grid_obj$grid_scores,
      final_score
    ),
    component_impact_summary = component_impact_summary,
    resample_summary = resample_summary,
    n_resamples = if (isTRUE(use_resample_tuning)) n_resamples else NA_integer_,
    seed = base_sim$seed,
    max_models_per_species = max_models_per_species
  )

  # Persist the tuned object for reuse when a cache path was requested.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  result
}

#' Refit empirical tuning across resampled tuning subsets
#'
#' Repeats empirical similarity tuning across multiple resampled per-species
#' tuning subsets to assess how stable the tuned component multipliers are.
#'
#' @param candidate_models Prepared candidate-model table.
#' @param species_traits Optional species-trait specification. See
#'   [prepare_similarity_matrix()] for the accepted forms. When `NULL`, a
#'   config-supplied value is used when present.
#' @param study_traits Optional study-trait specification. See
#'   [prepare_similarity_matrix()] for the accepted forms. When `NULL`, a
#'   config-supplied value is used when present.
#' @param alpha Optional starting species-versus-study mixing parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param k_species Optional starting species-distance kernel parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param k_study Optional starting study-distance kernel parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param n_resamples Number of resampled tuning subsets. When `NULL`, a
#'   config-supplied value is used when present.
#' @param max_models_per_species Maximum number of retained models per species
#'   within each resampled tuning subset. When `NULL`, a config-supplied value
#'   is used when present.
#' @param seed Optional integer seed. When `NULL`, a config-supplied value is
#'   used when present; otherwise one is generated and returned in the output
#'   object.
#' @param config Optional JSON path or list with similarity options. Supported
#'   entries are the same as [tune_similarity_empirical()], plus
#'   `n_resamples`.
#' @param cache_path Optional path to an `.rds` cache file.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache file.
#' @param registry_path Optional path to a trait-registry JSON file.
#'
#' @return A list containing the resampled tuning subsets, resample summaries,
#'   and per-component tuned multipliers across resamples.
#'
#' @export
tune_similarity_resamples <- function(candidate_models,
                                      species_traits = NULL,
                                      study_traits = NULL,
                                      alpha = NULL,
                                      k_species = NULL,
                                      k_study = NULL,
                                      n_resamples = NULL,
                                      max_models_per_species = NULL,
                                      seed = NULL,
                                      config = NULL,
                                      cache_path = NULL,
                                      refresh = FALSE,
                                      registry_path = NULL) {
  # Validate cache control up front so cached and uncached calls follow the
  # same input rules.
  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Resolve the optional config once so the explicit function arguments can
  # override it consistently.
  cfg_user <- read_similarity_config(config)
  n_resamples <- n_resamples %||% cfg_user$n_resamples %||% 8L
  max_models_per_species <- max_models_per_species %||% cfg_user$max_models_per_species %||% 2L

  if (!is.numeric(n_resamples) ||
      length(n_resamples) != 1 ||
      !is.finite(n_resamples) ||
      n_resamples < 1) {
    stop("'n_resamples' must be one integer >= 1.", call. = FALSE)
  }
  n_resamples <- as.integer(n_resamples)

  if (!is.numeric(max_models_per_species) ||
      length(max_models_per_species) != 1 ||
      !is.finite(max_models_per_species) ||
      max_models_per_species < 1) {
    stop("'max_models_per_species' must be one integer >= 1.", call. = FALSE)
  }
  max_models_per_species <- as.integer(max_models_per_species)

  # Reuse an existing cache unless the caller explicitly requested a refresh.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Resolve the baseline similarity inputs once so every resample uses the
  # same selected traits, starting weights, and scalar defaults.
  base_sim <- prepare_similarity_matrix(
    candidate_models = candidate_models,
    species_traits = species_traits,
    study_traits = study_traits,
    alpha = alpha,
    k_species = k_species,
    k_study = k_study,
    config = config,
    registry_path = registry_path,
    seed = seed
  )

  # Generate one deterministic seed per resample so repeated runs can rebuild
  # the exact same resampled tuning subsets.
  set.seed(base_sim$seed)
  resample_seeds <- sample.int(.Machine$integer.max, n_resamples)

  summary_rows <- list()
  subset_rows <- list()
  multiplier_rows <- list()

  for (resample_id in seq_len(n_resamples)) {
    # Draw a new per-species tuning subset for the current resample before
    # running the single-pass empirical tuner on that subset.
    sampled_subset <- build_resample_subset(
      candidate_models = base_sim$candidate_models,
      species_weights = base_sim$species_weights,
      study_weights = base_sim$study_weights,
      max_models_per_species = max_models_per_species,
      seed = resample_seeds[[resample_id]]
    ) |>
      dplyr::mutate(resample_id = resample_id)

    tune_obj <- tune_similarity_empirical(
      candidate_models = sampled_subset,
      species_traits = as.list(base_sim$species_weights),
      study_traits = as.list(base_sim$study_weights),
      alpha = base_sim$alpha,
      k_species = base_sim$k_species,
      k_study = base_sim$k_study,
      max_models_per_species = max_models_per_species,
      seed = resample_seeds[[resample_id]],
      config = base_sim$config,
      cache_path = NULL,
      refresh = TRUE,
      registry_path = registry_path
    )

    # Record one summary row per resample from the final tuned score and the
    # tuned scalar parameters returned by the inner tuning run.
    final_row <- tune_obj$tuning_history |>
      dplyr::filter(stage == "final_tuned") |>
      dplyr::slice_tail(n = 1)

    summary_rows[[length(summary_rows) + 1]] <- tibble::tibble(
      resample_id = resample_id,
      n_models = nrow(sampled_subset),
      n_species = dplyr::n_distinct(sampled_subset$species_name),
      alpha = tune_obj$config_tuned$alpha,
      k_species = tune_obj$config_tuned$k_species,
      k_study = tune_obj$config_tuned$k_study,
      rmse = final_row$rmse[[1]],
      mae = final_row$mae[[1]],
      seed = resample_seeds[[resample_id]]
    )

    subset_rows[[length(subset_rows) + 1]] <- sampled_subset
    multiplier_rows[[length(multiplier_rows) + 1]] <- collect_component_weights(
      tune_obj = tune_obj,
      resample_id = resample_id
    )
  }

  result <- list(
    tuning_subset_members = dplyr::bind_rows(subset_rows),
    resample_summary = dplyr::bind_rows(summary_rows),
    component_multipliers = dplyr::bind_rows(multiplier_rows),
    seed = base_sim$seed,
    n_resamples = n_resamples,
    max_models_per_species = max_models_per_species
  )

  # Cache the assembled resampling result only after all resamples finish.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  result
}

