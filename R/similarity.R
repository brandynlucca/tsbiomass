#' Normalize the candidate-model similarity inputs
#'
#' @param candidate_models Prepared candidate-model table or a [Candidates]
#'   object.
#'
#' @return A tibble with the required identifier and coherence columns present.
#'
#' @keywords internal
#' @noRd
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
  out$model_id <- as.character(out$model_id)

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

  # Rename column variants to their canonical trait names
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

#' Normalize one frequency-coherence method label
#'
#' @param method Frequency-coherence method label.
#'
#' @return Canonical method label.
#'
#' @keywords internal
#' @noRd
normalize_similarity_frequency_method <- function(method) {
  method_value <- stringr::str_to_lower(stringr::str_squish(as.character(method %||% "overlap")))[[1]]
  switch(method_value,
    overlap = "overlap",
    literal = "literal",
    none = "none",
    method_value
  )
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
#' @noRd
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

        # Force evaluation
        force(raw_low)

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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#'   `read_similarity_registry()`.
#'
#' @return A tibble with any missing registry-coded columns added.
#'
#' @keywords internal
#' @noRd
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
#' @noRd
read_similarity_config <- function(config) {
  config_similarity_view <- function(cfg) {
    similarity_cfg <- cfg$similarity %||% list()
    tuning_cfg <- cfg$tuning %||% list()
    execution_cfg <- cfg$execution %||% list()
    coherence_cfg <- similarity_cfg$coherence %||% list()
    length_cfg <- coherence_cfg$length %||% list()
    depth_cfg <- coherence_cfg$depth %||% list()
    frequency_cfg <- coherence_cfg$frequency %||% list()
    length_mode <- length_cfg$mode %||% "overlap"
    depth_mode <- depth_cfg$mode %||% "overlap"
    frequency_mode <- frequency_cfg$mode %||% "overlap"
    frequency_mode <- normalize_similarity_frequency_method(frequency_mode)

    list(
      species_traits = similarity_cfg$species_traits %||% NULL,
      study_traits = similarity_cfg$study_traits %||% NULL,
      alpha = similarity_cfg$alpha %||% NULL,
      kernel_scale = similarity_cfg$kernel_scale %||% NULL,
      k_species = similarity_cfg$kernel_scale %||% NULL,
      k_study = similarity_cfg$kernel_scale %||% NULL,
      alpha_range = tuning_cfg$alpha_range %||% NULL,
      kernel_scale_range = tuning_cfg$kernel_scale_range %||% NULL,
      k_species_range = tuning_cfg$kernel_scale_range %||% NULL,
      k_study_range = tuning_cfg$kernel_scale_range %||% NULL,
      alpha_grid = tuning_cfg$alpha_grid %||% NULL,
      kernel_scale_grid = tuning_cfg$kernel_scale_grid %||% NULL,
      k_species_grid = tuning_cfg$kernel_scale_grid %||% NULL,
      k_study_grid = tuning_cfg$kernel_scale_grid %||% NULL,
      max_models_per_species = tuning_cfg$species_model_limit %||% NULL,
      n_resamples = tuning_cfg$resamples %||% NULL,
      n_cores = tuning_cfg$n_cores %||% NULL,
      seed = tuning_cfg$seed %||% NULL,
      grid_refinement_levels = tuning_cfg$grid_refinement_levels %||% NULL,
      equal_start_weights = tuning_cfg$equal_start_weights %||% FALSE,
      response_surface_top_n = tuning_cfg$response_surface_top_n %||% NULL,
      rmse_tolerance = tuning_cfg$rmse_tolerance %||% NULL,
      support_strata_bins = tuning_cfg$support_strata_bins %||% NULL,
      regularization = tuning_cfg$regularization %||% list(),
      progress = execution_cfg$progress %||% NULL,
      cache_path = similarity_cfg$cache_path %||% NULL,
      refresh = similarity_cfg$refresh %||% NULL,
      exact_frequency = NULL,
      frequency_gap = frequency_cfg$gap %||% NULL,
      length_coherence = list(
        method = length_mode,
        weight = length_cfg$weight %||% NULL,
        range = tuning_cfg$coherence$length$range %||% NULL,
        grid = tuning_cfg$coherence$length$grid %||% NULL
      ),
      depth_coherence = list(
        method = depth_mode,
        weight = depth_cfg$weight %||% NULL,
        range = tuning_cfg$coherence$depth$range %||% NULL,
        grid = tuning_cfg$coherence$depth$grid %||% NULL
      ),
      frequency_coherence = list(
        method = frequency_mode,
        weight = frequency_cfg$weight %||% NULL,
        range = tuning_cfg$coherence$frequency$range %||% NULL,
        grid = tuning_cfg$coherence$frequency$grid %||% NULL
      )
    )
  }

  if (is.null(config)) {
    return(list())
  }

  if (is_s7_instance(config, "Configurer")) {
    return(config_similarity_view(config@data))
  }

  if (is.character(config) && length(config) == 1) {
    return(read_json_file(config))
  }

  if (is.list(config)) {
    if (all(c("paths", "execution", "tuning", "policies") %in% names(config)) &&
      any(c("similarity", "policy") %in% names(config))) {
      return(config_similarity_view(config))
    }
    if (is.list(config$frequency_coherence)) {
      config$frequency_coherence$method <- normalize_similarity_frequency_method(
        config$frequency_coherence$method %||% "overlap"
      )
    }
    return(config)
  }

  stop("'config' must be NULL, a JSON file path, or a list.", call. = FALSE)
}

#' Resolve one similarity config source
#'
#' @param candidate_models Prepared candidate-model table or a [Candidates]
#'   object.
#' @param config Optional config object.
#'
#' @return A config object or `NULL`.
#'
#' @keywords internal
#' @noRd
resolve_similarity_config_source <- function(candidate_models,
                                             config) {
  if (!is.null(config)) {
    return(config)
  }

  if (is_s7_instance(candidate_models, "Candidates")) {
    return(candidates_config_data(candidate_models))
  }

  NULL
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
#' @noRd
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
#' @noRd
resolve_similarity_setup <- function(cfg_user,
                                     alpha,
                                     k_species,
                                     k_study) {
  normalize_grid_spec <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    if (is.list(x) && all(c("from", "to", "by") %in% names(x))) {
      return(seq(from = as.numeric(x$from), to = as.numeric(x$to), by = as.numeric(x$by)))
    }
    if (is.list(x) && all(c("from", "to", "length") %in% names(x))) {
      return(seq(from = as.numeric(x$from), to = as.numeric(x$to), length.out = as.integer(x$length)))
    }
    as.numeric(unlist(x, use.names = FALSE))
  }
  normalize_range_spec <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    if (is.list(x) && all(c("from", "to") %in% names(x))) {
      return(c(as.numeric(x$from), as.numeric(x$to)))
    }
    vals <- as.numeric(unlist(x, use.names = FALSE))
    vals[is.finite(vals)]
  }

  cfg <- utils::modifyList(
    list(
      length_coherence = list(method = "overlap", weight = 1),
      depth_coherence = list(method = "overlap", weight = 1),
      frequency_coherence = list(method = "overlap", weight = 1),
      alpha_range = NULL,
      kernel_scale_range = NULL,
      k_species_range = NULL,
      k_study_range = NULL,
      alpha_grid = NULL,
      k_species_grid = NULL,
      k_study_grid = NULL,
      grid_refinement_levels = 1L,
      equal_start_weights = FALSE,
      response_surface_top_n = 20L,
      rmse_tolerance = 0.01,
      support_strata_bins = 4L,
      regularization = list(
        alpha = 0.05,
        kernel_scale = 0.05,
        coherence_scale = 0.05,
        stability = 0.02
      )
    ),
    cfg_user
  )

  cfg$length_coherence$method <- as.character(cfg$length_coherence$method %||% "overlap")[[1]]
  cfg$depth_coherence$method <- as.character(cfg$depth_coherence$method %||% "overlap")[[1]]
  cfg$frequency_coherence$method <- normalize_similarity_frequency_method(
    as.character(cfg$frequency_coherence$method %||% "overlap")[[1]]
  )
  cfg$length_coherence$weight <- as.numeric(cfg$length_coherence$weight %||% 1)[[1]]
  cfg$depth_coherence$weight <- as.numeric(cfg$depth_coherence$weight %||% 1)[[1]]
  cfg$frequency_coherence$weight <- as.numeric(cfg$frequency_coherence$weight %||% 1)[[1]]
  cfg$length_coherence$range <- normalize_range_spec(cfg$length_coherence$range %||% c(max(0, cfg$length_coherence$weight / 2), max(1, cfg$length_coherence$weight * 2)))
  cfg$depth_coherence$range <- normalize_range_spec(cfg$depth_coherence$range %||% c(max(0, cfg$depth_coherence$weight / 2), max(1, cfg$depth_coherence$weight * 2)))
  cfg$frequency_coherence$range <- normalize_range_spec(cfg$frequency_coherence$range %||% c(max(0, cfg$frequency_coherence$weight / 2), max(1, cfg$frequency_coherence$weight * 2)))
  cfg$length_coherence$grid <- sort(unique(normalize_grid_spec(cfg$length_coherence$grid %||% cfg$length_coherence$range %||% cfg$length_coherence$weight)))
  cfg$depth_coherence$grid <- sort(unique(normalize_grid_spec(cfg$depth_coherence$grid %||% cfg$depth_coherence$range %||% cfg$depth_coherence$weight)))
  cfg$frequency_coherence$grid <- sort(unique(normalize_grid_spec(cfg$frequency_coherence$grid %||% cfg$frequency_coherence$range %||% cfg$frequency_coherence$weight)))
  cfg$length_coherence$range <- sort(unique(cfg$length_coherence$range[is.finite(cfg$length_coherence$range) & cfg$length_coherence$range >= 0]))
  cfg$depth_coherence$range <- sort(unique(cfg$depth_coherence$range[is.finite(cfg$depth_coherence$range) & cfg$depth_coherence$range >= 0]))
  cfg$frequency_coherence$range <- sort(unique(cfg$frequency_coherence$range[is.finite(cfg$frequency_coherence$range) & cfg$frequency_coherence$range >= 0]))
  cfg$length_coherence$grid <- sort(unique(cfg$length_coherence$grid[is.finite(cfg$length_coherence$grid) & cfg$length_coherence$grid >= 0]))
  cfg$depth_coherence$grid <- sort(unique(cfg$depth_coherence$grid[is.finite(cfg$depth_coherence$grid) & cfg$depth_coherence$grid >= 0]))
  cfg$frequency_coherence$grid <- sort(unique(cfg$frequency_coherence$grid[is.finite(cfg$frequency_coherence$grid) & cfg$frequency_coherence$grid >= 0]))

  cfg$alpha_range <- normalize_range_spec(cfg$alpha_range)
  if (length(cfg$alpha_range) < 2) {
    cfg$alpha_range <- c(max(0.05, alpha - 0.2), min(0.95, alpha + 0.2))
  }
  cfg$alpha_range <- sort(unique(cfg$alpha_range[is.finite(cfg$alpha_range) & cfg$alpha_range > 0 & cfg$alpha_range < 1]))
  if (length(cfg$alpha_range) < 2) {
    cfg$alpha_range <- c(max(0.05, alpha - 0.2), min(0.95, alpha + 0.2))
  }

  cfg$k_species_range <- normalize_range_spec(cfg$k_species_range)
  if (length(cfg$k_species_range) < 2) {
    cfg$k_species_range <- c(max(0, k_species / 2), k_species * 2)
  }
  cfg$k_species_range <- sort(unique(cfg$k_species_range[is.finite(cfg$k_species_range) & cfg$k_species_range >= 0]))
  if (length(cfg$k_species_range) < 2) {
    cfg$k_species_range <- c(max(0, k_species / 2), k_species * 2)
  }

  cfg$k_study_range <- normalize_range_spec(cfg$k_study_range)
  if (length(cfg$k_study_range) < 2) {
    cfg$k_study_range <- c(max(0, k_study / 2), k_study * 2)
  }

  cfg$kernel_scale_range <- normalize_range_spec(cfg$kernel_scale_range)
  if (length(cfg$kernel_scale_range) < 2) {
    cfg$kernel_scale_range <- c(
      min(c(cfg$k_species_range[[1]], cfg$k_study_range[[1]]), na.rm = TRUE),
      max(c(cfg$k_species_range[[2]], cfg$k_study_range[[2]]), na.rm = TRUE)
    )
  }
  cfg$kernel_scale_range <- sort(unique(cfg$kernel_scale_range[is.finite(cfg$kernel_scale_range) & cfg$kernel_scale_range >= 0]))
  if (length(cfg$kernel_scale_range) < 2) {
    scale_now <- max(k_species, k_study, 1, na.rm = TRUE)
    cfg$kernel_scale_range <- c(max(0, scale_now / 2), scale_now * 2)
  }
  cfg$k_study_range <- sort(unique(cfg$k_study_range[is.finite(cfg$k_study_range) & cfg$k_study_range >= 0]))
  if (length(cfg$k_study_range) < 2) {
    cfg$k_study_range <- c(max(0, k_study / 2), k_study * 2)
  }

  cfg$alpha_grid <- sort(unique(normalize_grid_spec(cfg$alpha_grid %||% cfg$alpha_range %||% c(max(0.05, alpha - 0.2), alpha, min(0.95, alpha + 0.2)))))
  cfg$k_species_grid <- sort(unique(normalize_grid_spec(cfg$k_species_grid %||% cfg$k_species_range %||% c(max(0, k_species / 2), k_species, k_species * 2))))
  cfg$k_study_grid <- sort(unique(normalize_grid_spec(cfg$k_study_grid %||% cfg$k_study_range %||% c(max(0, k_study / 2), k_study, k_study * 2))))
  cfg$grid_refinement_levels <- suppressWarnings(as.integer(cfg$grid_refinement_levels %||% 1L))
  if (!is.finite(cfg$grid_refinement_levels) || cfg$grid_refinement_levels < 0) {
    cfg$grid_refinement_levels <- 0L
  }
  cfg$response_surface_top_n <- suppressWarnings(as.integer(cfg$response_surface_top_n %||% 20L))
  if (!is.finite(cfg$response_surface_top_n) || cfg$response_surface_top_n < 1L) {
    cfg$response_surface_top_n <- 20L
  }
  cfg$support_strata_bins <- suppressWarnings(as.integer(cfg$support_strata_bins %||% 4L))
  if (!is.finite(cfg$support_strata_bins) || cfg$support_strata_bins < 1L) {
    cfg$support_strata_bins <- 4L
  }
  cfg$rmse_tolerance <- suppressWarnings(as.numeric(cfg$rmse_tolerance %||% 0.01))
  if (!is.finite(cfg$rmse_tolerance) || cfg$rmse_tolerance < 0) {
    cfg$rmse_tolerance <- 0.01
  }
  reg_cfg <- cfg$regularization %||% list()
  if (!is.list(reg_cfg)) {
    reg_cfg <- list()
  }
  reg_cfg <- utils::modifyList(
    list(
      alpha = 0.05,
      kernel_scale = 0.05,
      coherence_scale = 0.05,
      stability = 0.02,
      edge = 0.25,
      edge_margin = 0.15
    ),
    reg_cfg
  )
  for (field_name in c("alpha", "kernel_scale", "coherence_scale", "stability", "edge", "edge_margin")) {
    field_value <- suppressWarnings(as.numeric(reg_cfg[[field_name]] %||% NA_real_))
    if (!is.finite(field_value) || field_value < 0) {
      field_value <- 0
    }
    reg_cfg[[field_name]] <- field_value
  }
  cfg$regularization <- reg_cfg
  cfg$equal_start_weights <- isTRUE(cfg$equal_start_weights %||% FALSE)

  cfg
}

#' Replace one trait-weight map with equal starts
#'
#' @param trait_map Named numeric vector or list.
#'
#' @return Named numeric vector with all weights set to `1`.
#'
#' @keywords internal
#' @noRd
equal_start_weights <- function(trait_map) {
  trait_names <- names(trait_map)
  if (is.null(trait_names) || any(!nzchar(trait_names))) {
    stop("'trait_map' must be named.", call. = FALSE)
  }
  stats::setNames(rep(1, length(trait_names)), trait_names)
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
#' @noRd
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

  if (length(weights) == 0L) {
    return(list(weights = stats::setNames(numeric(0), character(0)), defs = list()))
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
#' @param candidate_models Prepared candidate-model table or a [Candidates]
#'   object.
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
#' @param progress Logical scalar controlling stage messages.
#'
#' @return When `candidate_models` is a [Candidates] object, returns that
#'   object with prepared similarity state and an expanded candidate-model
#'   table. Otherwise, returns a list containing the normalized
#'   tuning configuration, selected traits, starting weights, expanded
#'   species/study matrices, and collapsed species profiles.
#'
#' @examples
#' \dontrun{
#' candidates <- build_candidates(list(
#'   study = list(path = "input.xlsx"),
#'   anchors = list(selector = list(regional_body = "SWFSC"))
#' ))
#'
#' candidates <- prepare_similarities(
#'   candidate_models = candidates,
#'   config = build_configurer(list(
#'     paths = list(
#'       input_file = "input.xlsx",
#'       out_root = "outputs",
#'       cache_dir = "cache"
#'     ),
#'     execution = list(),
#'     tuning = list(),
#'     policy = list(
#'       alpha = 0.8,
#'       k_species = 4,
#'       k_study = 2,
#'       frequency_coherence_mode = "overlap",
#'       length_overlap_weight = 2,
#'       depth_overlap_weight = 2,
#'       frequency_coherence_weight = 1,
#'       species_traits = list(genus = 1, family = 1),
#'       study_traits = list(frequency = 1, fao_area = 1)
#'     ),
#'     policies = list(active = "closest_within_species")
#'   ))
#' )
#'
#' candidates
#' }
#'
#' @export
prepare_similarities <- function(candidate_models,
                                 species_traits = NULL,
                                 study_traits = NULL,
                                 alpha = NULL,
                                 k_species = NULL,
                                 k_study = NULL,
                                 config = NULL,
                                 registry_path = NULL,
                                 seed = NULL,
                                 progress = NULL) {
  # Preserve the prepared `Candidates` object boundary when present so prepared
  # similarity state is stored on the object rather than returned only as a
  # detached sidecar list.
  candidates_obj <- if (is_s7_instance(candidate_models, "Candidates")) {
    candidate_models
  } else {
    NULL
  }
  config <- resolve_similarity_config_source(candidate_models, config)
  candidate_models <- if (is_s7_instance(candidate_models, "Candidates")) {
    candidate_models@candidate_models
  } else {
    candidate_models
  }

  # Normalize the candidate model table and resolve the trait registry first so
  # later helpers can assume a stable input schema.
  models_tbl <- normalize_similarity_data(candidate_models)
  registry_obj <- read_similarity_registry(registry_path)
  models_tbl <- seed_registry_traits(models_tbl, registry_obj)

  # Resolve the optional config and scalar starting values before trait
  # selection begins so the full preparation state is explicit.
  cfg_user <- read_similarity_config(config)
  progress <- progress %||% cfg_user$progress %||% FALSE
  report_progress(progress, "Preparing similarity inputs.")
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
    basin_keywords <- c(
      "atlantic",
      "pacific",
      "mediterranean",
      "indian",
      "southern",
      "arctic",
      "inland"
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
        for (basin_code in basin_keywords) {
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
    fao_allowed <- c(
      "1", "2", "3", "4", "5", "6", "7", "8", "18", "21", "27", "31",
      "34", "37", "41", "47", "48", "51", "57", "58", "61", "67", "71",
      "77", "81", "87", "88"
    )
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
  #    that subsequent calls to prepare_similarities on already-prepared
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
        , setdiff(names(candidate_models_with_expanded), cols_to_drop),
        drop = FALSE
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
        , setdiff(names(candidate_models_with_expanded), pre_existing_sp),
        drop = FALSE
      ]
    }
    species_profiles_join <- species_profiles[
      , c("species_name", new_species_binary_cols),
      drop = FALSE
    ]
    candidate_models_with_expanded <- dplyr::left_join(
      candidate_models_with_expanded,
      species_profiles_join,
      by = "species_name"
    )
  }

  result <- list(
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
  report_progress(
    progress,
    "Prepared similarity inputs for ",
    nrow(models_tbl),
    " candidate models."
  )

  if (!is.null(candidates_obj)) {
    return(candidates_with_similarity_matrix(candidates_obj, result))
  }

  result
}

#' Build a phylogenetic distance matrix from a species name vector
#'
#' Uses the Open Tree of Life API (`rotl`) to compute cophenetic phylogenetic
#' distances for a vector of species names, returning a normalised 0 to 1
#' n x n matrix where `n = length(species_vec)`. Rows with missing or
#' unmatched species names default to distance 1 (maximum dissimilarity).
#' Returns `NULL` when fewer than two species can be matched, so the caller
#' can apply a rank-based fallback.
#'
#' @param species_vec Character vector of species names, one entry per model
#'   row. Binomials ("Gadus morhua") are preferred; single epithets
#'   ("morhua") are combined with `genus_vec` when provided.
#' @param genus_vec Optional character vector of genus names, same length as
#'   `species_vec`.
#'
#' @return Numeric n x n matrix with values between 0 and 1, or `NULL` on failure.
#'
#' @keywords internal
#' @noRd
normalize_phylo_species_label <- function(x) {
  stringr::str_to_lower(stringr::str_squish(gsub("_", " ", as.character(x), fixed = TRUE)))
}

#' Map a cophenetic matrix onto queried species names
#'
#' @param species_names Canonical queried species names.
#' @param cophenetic_labels Tip labels returned by Open Tree.
#' @param cophenetic_matrix Cophenetic distance matrix matching the tip labels.
#'
#' @return A normalized species distance matrix.
#'
#' @keywords internal
#' @noRd
map_phylo_cophenetic_distances <- function(species_names,
                                           cophenetic_labels,
                                           cophenetic_matrix) {
  species_keys <- normalize_phylo_species_label(species_names)
  tip_keys <- normalize_phylo_species_label(cophenetic_labels)
  out <- matrix(
    1,
    nrow = length(species_keys),
    ncol = length(species_keys),
    dimnames = list(species_keys, species_keys)
  )
  diag(out) <- 0
  if (length(tip_keys) == 0L || nrow(cophenetic_matrix) == 0L) {
    return(out)
  }

  keep <- tip_keys %in% species_keys
  if (!any(keep)) {
    return(out)
  }
  kept_keys <- tip_keys[keep]
  kept_matrix <- cophenetic_matrix[keep, keep, drop = FALSE]
  unique_keys <- !duplicated(kept_keys)
  kept_keys <- kept_keys[unique_keys]
  kept_matrix <- kept_matrix[unique_keys, unique_keys, drop = FALSE]
  dimnames(kept_matrix) <- list(kept_keys, kept_keys)
  match_idx <- match(species_keys, kept_keys)
  valid_idx <- which(!is.na(match_idx))
  if (length(valid_idx) > 0L) {
    out[valid_idx, valid_idx] <- kept_matrix[
      match_idx[valid_idx], match_idx[valid_idx],
      drop = FALSE
    ]
  }
  diag(out) <- 0
  out
}

build_phylo_dist_from_species <- function(species_vec, genus_vec = NULL) {
  sp_raw <- stringr::str_squish(as.character(species_vec))
  sp_raw[!nzchar(sp_raw)] <- NA_character_

  sp <- rep(NA_character_, length(sp_raw))
  has_binomial <- !is.na(sp_raw) & grepl("\\s+", sp_raw)
  sp[has_binomial] <- sp_raw[has_binomial]

  if (!is.null(genus_vec)) {
    genus_now <- stringr::str_squish(as.character(genus_vec))
    genus_now[!nzchar(genus_now)] <- NA_character_
    sp_epithet <- sp_raw
    sp_epithet[grepl("\\s+", sp_epithet)] <- NA_character_
    can_build <- !is.na(genus_now) & !is.na(sp_epithet)
    sp[can_build] <- stringr::str_squish(paste(genus_now[can_build], sp_epithet[can_build]))
  }

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

  phylo_mat <- tryCatch(
    {
      tnrs <- suppressWarnings(rotl::tnrs_match_names(uniq, do_approximate_matching = FALSE))
      matched <- tibble::as_tibble(tnrs) |>
        dplyr::mutate(search_string = as.character(.data$search_string)) |>
        dplyr::filter(!is.na(.data$ott_id)) |>
        dplyr::distinct(.data$search_string, .keep_all = TRUE)

      if (!is.finite(nrow(matched) / length(uniq)) || nrow(matched) / length(uniq) < 0.7) {
        return(NULL)
      }
      if (nrow(matched) < 2) {
        return(NULL)
      }

      subtree <- suppressWarnings(rotl::tol_induced_subtree(ott_ids = matched$ott_id))
      cophen <- suppressWarnings(ape::cophenetic.phylo(subtree))
      cophen_mx <- max(cophen, na.rm = TRUE)
      if (!is.finite(cophen_mx) || cophen_mx <= 0) cophen_mx <- 1

      labels <- suppressWarnings(rotl::strip_ott_ids(colnames(cophen)))
      map_phylo_cophenetic_distances(
        species_names = uniq,
        cophenetic_labels = labels,
        cophenetic_matrix = cophen / cophen_mx
      )
    },
    error = function(e) NULL
  )

  if (is.null(phylo_mat)) {
    return(NULL)
  }

  out_idx <- match(normalize_phylo_species_label(sp), rownames(phylo_mat))
  keep_idx <- which(!is.na(out_idx))
  if (length(keep_idx) > 0) {
    out[keep_idx, keep_idx] <- phylo_mat[out_idx[keep_idx], out_idx[keep_idx], drop = FALSE]
  }
  diag(out) <- 0
  out
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
#' @noRd
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
    interval_overlap_distance_matrix(mins, maxs, method = "literal")
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

        tax_dist <- NULL
        species_rank_idx <- which(names(rank_values) == "species")
        genus_rank_idx <- which(names(rank_values) == "genus")
        if (length(species_rank_idx) == 1) {
          genus_vec <- if (length(genus_rank_idx) == 1) rank_values[[genus_rank_idx]] else NULL
          tax_dist <- build_phylo_dist_from_species(rank_values[[species_rank_idx]], genus_vec = genus_vec)
        }

        if (is.null(tax_dist)) {
          tax_dist <- rank_distance_matrix(rank_values)
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
#' @param similarity Prepared similarity object returned by
#'   [prepare_similarities()] or a [Candidates] object with prepared
#'   similarity state.
#' @param progress Logical scalar controlling stage messages.
#'
#' @return When `similarity` is a [Candidates] object, returns that object with
#'   the Gower-distance bundle attached. Otherwise,
#'   returns a list containing `species_dist`, `study_dist`,
#'   `species_dist_model`, `combined_dist`, and `trait_cols`.
#'
#' @examples
#' \dontrun{
#' candidates <- build_candidates(list(
#'   study = list(path = "input.xlsx"),
#'   anchors = list(selector = list(regional_body = "SWFSC"))
#' ))
#' candidates <- prepare_similarities(candidate_models = candidates)
#' candidates <- construct_gower_distances(candidates)
#' candidates
#' }
#'
#' @export
construct_gower_distances <- function(similarity,
                                      progress = NULL) {
  # Preserve the prepared `Candidates` object boundary when present so the
  # distance bundle is stored on the object instead of returned only as a
  # detached list.
  candidates_obj <- if (is_s7_instance(similarity, "Candidates")) {
    similarity
  } else {
    NULL
  }
  if (!is.null(candidates_obj)) {
    progress <- progress %||% resolve_config_value(candidates_obj, "progress", sections = c("similarity", "ordination")) %||% FALSE
    if (length(candidates_obj@similarity_matrix) == 0) {
      stop(
        "Candidates object has no prepared similarity state. Run `prepare_similarities()` first.",
        call. = FALSE
      )
    }
    similarity <- candidates_obj@similarity_matrix
    if (is.null(similarity$candidate_models)) {
      # The stored similarity state on `Candidates` is intentionally slimmed
      # down to avoid caching a second full model table. Reattach the canonical
      # trimmed table only for the duration of this distance build.
      similarity$candidate_models <- tibble::as_tibble(candidates_obj@candidate_models)
    }
    if (is.null(similarity$study_data)) {
      # The stored `Candidates` object does not retain the expanded row-level
      # study matrix because it can be rebuilt deterministically from the
      # trimmed model table and the retained study-trait specification.
      study_expanded <- expand_trait_block(
        df = tibble::as_tibble(similarity$candidate_models),
        weight_spec = similarity$study_weights %||% numeric(0),
        trait_defs = similarity$study_trait_defs %||% list()
      )
      similarity$study_data <- study_expanded$data
      similarity$study_matrix_weights <- study_expanded$weights
      similarity$study_component_lookup <- study_expanded$lookup
    }
  }
  progress <- progress %||% FALSE
  report_progress(progress, "Building Gower distance matrices.")

  # Validate the prepared object shape once so downstream matrix construction
  # can assume the required pieces are present.
  required_fields <- c(
    "species_profiles", "species_matrix_weights", "study_data",
    "study_matrix_weights", "candidate_models", "alpha",
    "species_traits", "study_traits"
  )
  missing_fields <- setdiff(required_fields, names(similarity))
  if (length(missing_fields) > 0) {
    stop(
      sprintf(
        "'similarity' is missing required field(s): %s",
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Build the species-level matrix on the collapsed species profiles so
  # biology distances are not affected by repeated model rows per species.
  species_trait_cols <- setdiff(names(similarity$species_profiles), "species_name")

  species_dist <- if (nrow(similarity$species_profiles) > 0) {
    compute_gower_matrix(
      df_traits = similarity$species_profiles[, species_trait_cols, drop = FALSE],
      trait_weights = similarity$species_matrix_weights,
      trait_defs = similarity$species_trait_defs,
      component_lookup = similarity$species_component_lookup
    )
  } else {
    matrix(NA_real_, nrow = 0, ncol = 0)
  }

  if (nrow(similarity$species_profiles) > 0) {
    rownames(species_dist) <- similarity$species_profiles$species_name
    colnames(species_dist) <- similarity$species_profiles$species_name
  }

  # Build the study-level matrix directly on the model rows because the donor
  # pool is evaluated at the individual model level.
  study_dist <- compute_gower_matrix(
    df_traits = similarity$study_data,
    trait_weights = similarity$study_matrix_weights,
    trait_defs = similarity$study_trait_defs,
    component_lookup = similarity$study_component_lookup
  )
  model_ids <- similarity$candidate_models$model_id
  rownames(study_dist) <- model_ids
  colnames(study_dist) <- model_ids

  # Expand the species-level matrix back to model rows using the candidate
  # models' species labels so the two distance blocks share one index.
  species_vec <- stringr::str_squish(as.character(similarity$candidate_models$species_name))

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

  # Median-rescale each block by its median positive off-diagonal distance so
  # that alpha genuinely controls the species-vs-study mixture weight regardless
  # of absolute magnitude differences between the two Gower blocks. The raw
  # species-by-species matrix is kept unscaled for ordination; only the
  # model-row-expanded matrices used for kernel weighting are rescaled.
  pos_sp <- species_dist_model[is.finite(species_dist_model) & species_dist_model > 0]
  pos_st <- study_dist[is.finite(study_dist) & study_dist > 0]
  species_dist_scale <- if (length(pos_sp) > 0) stats::median(pos_sp) else 1
  study_dist_scale <- if (length(pos_st) > 0) stats::median(pos_st) else 1
  if (!is.finite(species_dist_scale) || species_dist_scale <= 0) species_dist_scale <- 1
  if (!is.finite(study_dist_scale) || study_dist_scale <= 0) study_dist_scale <- 1

  species_dist_model <- species_dist_model / species_dist_scale
  study_dist <- study_dist / study_dist_scale

  # Combine the rescaled species and study distance blocks with the prepared
  # alpha weight and force zero self-distance on the diagonal.
  #
  # When one component is missing for a pair, rescale by the available weight
  # instead of propagating NA into the combined matrix.
  # Note: use explicit ifelse guards so that 0 * NA -> 0 (not NA).
  w_species <- ifelse(is.finite(species_dist_model), similarity$alpha, 0)
  w_study <- ifelse(is.finite(study_dist), 1 - similarity$alpha, 0)
  w_total <- w_species + w_study

  species_contrib <- ifelse(is.finite(species_dist_model), w_species * species_dist_model, 0)
  study_contrib <- ifelse(is.finite(study_dist), w_study * study_dist, 0)

  combined_dist <- matrix(NA_real_, nrow = nrow(study_dist), ncol = ncol(study_dist))
  use_idx <- w_total > 0
  combined_dist[use_idx] <- (species_contrib[use_idx] + study_contrib[use_idx]) / w_total[use_idx]

  rownames(combined_dist) <- model_ids
  colnames(combined_dist) <- model_ids
  diag(combined_dist) <- 0

  # Return the EXPANDED trait column names so callers get the binary indicators
  # for set-valued traits (ocean_basin__*, fao_area__*) instead of the original
  # semi-colon-delimited strings.
  expanded_species_cols <- setdiff(names(similarity$species_profiles), "species_name")
  expanded_study_cols <- names(similarity$study_data)
  trait_cols <- unique(c(expanded_species_cols, expanded_study_cols))

  result <- list(
    distance_mode = "empirical_gower",
    species_dist = species_dist,
    study_dist = study_dist,
    species_dist_model = species_dist_model,
    combined_dist = combined_dist,
    trait_cols = trait_cols,
    species_dist_scale = species_dist_scale,
    study_dist_scale = study_dist_scale
  )
  report_progress(progress, "Built Gower distance matrices.")

  if (!is.null(candidates_obj)) {
    return(candidates_with_gower_distances(candidates_obj, result))
  }

  result
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
#' @noRd
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
    dplyr::filter(is.finite(.data$slope_len), is.finite(.data$intercept_len)) |>
    dplyr::mutate(
      .tune_complete = rowSums(!is.na(dplyr::pick(dplyr::all_of(tune_cols))), na.rm = TRUE),
      .tie_break = stats::runif(dplyr::n())
    ) |>
    dplyr::group_by(.data$species_name) |>
    dplyr::arrange(dplyr::desc(.data$.tune_complete), .data$.tie_break, .by_group = TRUE) |>
    dplyr::slice_head(n = max_models_per_species) |>
    dplyr::ungroup() |>
    dplyr::select(-tidyselect::any_of(c(".tune_complete", ".tie_break")))

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
#' @noRd
build_resample_subset <- function(candidate_models,
                                  species_weights,
                                  study_weights,
                                  max_models_per_species,
                                  seed) {
  # Mirror the single-pass tuning subset rules so resampling works across
  # prepared interval naming scheme.
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
    dplyr::filter(is.finite(.data$slope_len), is.finite(.data$intercept_len)) |>
    dplyr::mutate(
      .tune_complete = rowSums(!is.na(dplyr::pick(dplyr::all_of(tune_cols))), na.rm = TRUE)
    ) |>
    dplyr::group_by(.data$species_name) |>
    dplyr::arrange(dplyr::desc(.data$.tune_complete), .by_group = TRUE) |>
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
    dplyr::select(-tidyselect::all_of(".tune_complete"))

  if (nrow(out) < 2) {
    stop("The resampled tuning subset must contain at least two usable models.", call. = FALSE)
  }

  out
}

#' Report the current similarity-tuning cache version
#'
#' @return Integer scalar cache version.
#' @keywords internal
#' @noRd
similarity_tuning_cache_version <- function() {
  3L
}

#' Check whether one cached similarity-tuning result is current
#'
#' @param tuning_result Cached similarity-tuning object.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
similarity_tuning_cache_current <- function(tuning_result) {
  if (!is.list(tuning_result)) {
    return(FALSE)
  }
  if (!identical(as.integer(tuning_result$tuning_version %||% NA_integer_), similarity_tuning_cache_version())) {
    return(FALSE)
  }

  history_tbl <- tibble::as_tibble(tuning_result$tuning_history %||% tibble::tibble())
  if (!all(c("alpha", "k_species", "k_study", "kernel_scale") %in% names(history_tbl))) {
    return(FALSE)
  }
  if (nrow(history_tbl) == 0) {
    return(FALSE)
  }

  valid_rows <- history_tbl |>
    dplyr::filter(
      is.finite(.data$alpha),
      is.finite(.data$k_species),
      is.finite(.data$k_study),
      is.finite(.data$kernel_scale)
    )
  if (nrow(valid_rows) == 0) {
    return(FALSE)
  }

  all(abs(valid_rows$k_species - valid_rows$kernel_scale) < 1e-8) &&
    all(abs(valid_rows$k_study - valid_rows$kernel_scale) < 1e-8)
}

#' Compute one shared kernel-scale value
#'
#' @param k_species_now Species-side scale.
#' @param k_study_now Study-side scale.
#'
#' @return Numeric scalar.
#' @keywords internal
#' @noRd
kernel_scale_center <- function(k_species_now,
                                k_study_now) {
  species_scale_now <- suppressWarnings(as.numeric(k_species_now[[1]] %||% NA_real_))
  study_scale_now <- suppressWarnings(as.numeric(k_study_now[[1]] %||% NA_real_))
  scale_now <- sqrt(max(0, species_scale_now) * max(0, study_scale_now))
  if (!is.finite(scale_now) || scale_now < 0) {
    scale_now <- max(c(species_scale_now, study_scale_now, 0), na.rm = TRUE)
  }
  if (!is.finite(scale_now) || scale_now < 0) {
    scale_now <- 0
  }
  scale_now
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
#' @noRd
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

  if (identical(method, "literal")) {
    # Literal mode compares interval endpoints directly rather than collapsing
    # everything to overlap relative to the anchor interval length.
    union_span <- max(a_hi, c_hi) - min(a_lo, c_lo)
    union_span <- max(union_span, 1e-9)
    endpoint_shift <- abs(a_lo - c_lo) + abs(a_hi - c_hi)
    return(pmin(endpoint_shift / (2 * union_span), 1))
  }

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
#' @noRd
frequency_offset_distance <- function(anchor_freq,
                                      cand_freq,
                                      method,
                                      freq_span) {
  # Frequency coherence can be disabled, treated as literal mismatch, or
  # scaled by actual magnitude difference on the log-frequency axis.
  if (identical(method, "none")) {
    return(NA_real_)
  }

  if (!is.finite(anchor_freq) || !is.finite(cand_freq) ||
    anchor_freq <= 0 || cand_freq <= 0) {
    return(NA_real_)
  }

  if (identical(method, "literal")) {
    # Treat rounded frequency values as literal bins when the caller wants
    # mismatch-only frequency coherence.
    return(as.numeric(as.integer(round(anchor_freq)) != as.integer(round(cand_freq))))
  }

  # Otherwise scale the absolute log-frequency difference by the observed
  # positive span so the result stays on a comparable 0-1-ish scale.
  pmin(abs(log(cand_freq / anchor_freq)) / freq_span, 1)
}

#' Compute a full interval-coherence distance matrix
#'
#' @param min_vals Numeric vector of interval minima.
#' @param max_vals Numeric vector of interval maxima.
#' @param method Character scalar.
#'
#' @return Numeric matrix with donor rows and anchor columns.
#'
#' @keywords internal
#' @noRd
interval_overlap_distance_matrix <- function(min_vals,
                                             max_vals,
                                             method) {
  n <- length(min_vals)
  out <- matrix(NA_real_, nrow = n, ncol = n)
  if (n == 0L || identical(method, "none")) {
    return(out)
  }

  min_vals <- suppressWarnings(as.numeric(min_vals))
  max_vals <- suppressWarnings(as.numeric(max_vals))
  valid <- is.finite(min_vals) & is.finite(max_vals)
  diag(out) <- 0
  if (!any(valid)) {
    return(out)
  }

  lo <- pmin(min_vals, max_vals)
  hi <- pmax(min_vals, max_vals)
  valid_mask <- outer(valid, valid, `&`)

  if (identical(method, "literal")) {
    union_span <- outer(hi, hi, pmax) - outer(lo, lo, pmin)
    union_span <- pmax(union_span, 1e-9)
    endpoint_shift <- abs(outer(lo, lo, `-`)) + abs(outer(hi, hi, `-`))
    out[valid_mask] <- pmin(endpoint_shift[valid_mask] / (2 * union_span[valid_mask]), 1)
    diag(out) <- 0
    return(out)
  }

  inter <- pmax(0, outer(hi, hi, pmin) - outer(lo, lo, pmax))
  anchor_len <- pmax(1e-9, hi - lo)
  anchor_len_mat <- matrix(anchor_len, nrow = n, ncol = n, byrow = TRUE)
  out[valid_mask] <- 1 - (inter[valid_mask] / anchor_len_mat[valid_mask])
  diag(out) <- 0
  out
}

#' Compute a full frequency-coherence distance matrix
#'
#' @param freq_vals Numeric vector of frequencies.
#' @param method Character scalar.
#' @param freq_span Numeric scalar.
#'
#' @return Numeric matrix with donor rows and anchor columns.
#'
#' @keywords internal
#' @noRd
frequency_offset_distance_matrix <- function(freq_vals,
                                             method,
                                             freq_span) {
  n <- length(freq_vals)
  out <- matrix(NA_real_, nrow = n, ncol = n)
  if (n == 0L || identical(method, "none")) {
    return(out)
  }

  freq_vals <- suppressWarnings(as.numeric(freq_vals))
  valid <- is.finite(freq_vals) & freq_vals > 0
  diag(out) <- 0
  if (!any(valid)) {
    return(out)
  }

  valid_mask <- outer(valid, valid, `&`)
  if (identical(method, "literal")) {
    rounded <- as.integer(round(freq_vals))
    out[valid_mask] <- as.numeric(outer(rounded, rounded, `!=`))[valid_mask]
    diag(out) <- 0
    return(out)
  }

  if (!is.finite(freq_span) || freq_span <= 0) {
    return(out)
  }

  out[valid_mask] <- pmin(abs(log(outer(freq_vals, freq_vals, `/`)))[valid_mask] / freq_span, 1)
  diag(out) <- 0
  out
}

#' Compute rank-based taxonomy distance matrix
#'
#' @param rank_values Named list of rank vectors ordered from coarse to fine.
#'
#' @return Numeric matrix.
#'
#' @keywords internal
#' @noRd
rank_distance_matrix <- function(rank_values) {
  n_ranks <- length(rank_values)
  if (n_ranks == 0L) {
    return(NULL)
  }

  n <- length(rank_values[[1]])
  deepest_shared <- matrix(0L, nrow = n, ncol = n)
  for (r in seq_len(n_ranks)) {
    vals <- stringr::str_squish(as.character(rank_values[[r]]))
    vals[!nzchar(vals)] <- NA_character_
    agree <- outer(vals, vals, function(donor, anchor) {
      !is.na(donor) & !is.na(anchor) & donor == anchor
    })
    deepest_shared[agree] <- r
  }

  out <- 1 - deepest_shared / n_ranks
  diag(out) <- 0
  out
}

#' Evaluate the reference length for one model row
#'
#' @param row_df One-row data frame.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
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

#' Build a reusable similarity-scoring basis
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
#' @return A list containing precomputed distances, anchor targets, and
#'   donor-prediction matrices used by `score_similarity_basis()`.
#'
#' @keywords internal
#' @noRd
prepare_similarity_score_basis <- function(models_subset,
                                           species_weights,
                                           study_weights,
                                           alpha_now,
                                           k_species_now,
                                           k_study_now,
                                           cfg_now,
                                           registry_path,
                                           seed_now) {
  # Rebuild the prepared similarity inputs for the exact weight/config state
  # being scored so the downstream basis reflects the current trait and
  # coherence weights.
  similarity <- prepare_similarities(
    candidate_models = models_subset,
    species_traits = as.list(species_weights),
    study_traits = as.list(study_weights),
    alpha = alpha_now,
    k_species = k_species_now,
    k_study = k_study_now,
    config = cfg_now,
    registry_path = registry_path,
    seed = seed_now,
    progress = FALSE
  )

  # Build the prepared species and study distance matrices only once for the
  # current weight/config state; alpha and kernel values can then be rescored
  # cheaply without rebuilding these matrices.
  dist_obj <- construct_gower_distances(similarity, progress = FALSE)
  species_dist_model <- dist_obj$species_dist_model
  study_dist <- dist_obj$study_dist

  model_ids <- similarity$candidate_models$model_id
  model_n <- length(model_ids)

  # Resolve the study interval column names once so the scorer can work with
  # either the current prepared schema or older cached candidate-model tables.
  study_length_min_col <- resolve_similarity_column_name(
    similarity$candidate_models,
    c("study_length_min", "length_minimum")
  )
  study_length_max_col <- resolve_similarity_column_name(
    similarity$candidate_models,
    c("study_length_max", "length_maximum")
  )
  study_length_mid_col <- resolve_similarity_column_name(
    similarity$candidate_models,
    c("study_length_midpoint", "length_midpoint")
  )
  study_depth_min_col <- resolve_similarity_column_name(
    similarity$candidate_models,
    c("study_depth_min", "depth_minimum")
  )
  study_depth_max_col <- resolve_similarity_column_name(
    similarity$candidate_models,
    c("study_depth_max", "depth_maximum")
  )

  resolve_numeric_col <- function(df_now,
                                  col_nm) {
    if (!is.character(col_nm) || !nzchar(col_nm) || !(col_nm %in% names(df_now))) {
      return(rep(NA_real_, nrow(df_now)))
    }
    out <- suppressWarnings(as.numeric(df_now[[col_nm]]))
    out[!is.finite(out)] <- NA_real_
    out
  }

  build_tuning_anchor_pdf <- function(row_df,
                                      n = 400) {
    midpoint_vals <- resolve_numeric_col(row_df, study_length_mid_col)
    min_vals <- resolve_numeric_col(row_df, study_length_min_col)
    max_vals <- resolve_numeric_col(row_df, study_length_max_col)

    midpoint_vals <- midpoint_vals[is.finite(midpoint_vals) & midpoint_vals > 0]
    min_vals <- min_vals[is.finite(min_vals) & min_vals > 0]
    max_vals <- max_vals[is.finite(max_vals) & max_vals > 0]

    if (length(min_vals) > 0 && length(max_vals) > 0) {
      lo <- min(pmin(min_vals, max_vals))
      hi <- max(pmax(min_vals, max_vals))
      if (is.finite(lo) && is.finite(hi) && lo > 0 && hi > 0) {
        if (hi > lo) {
          grid <- seq(lo, hi, length.out = n)
          return(tibble::tibble(
            length_cm = grid,
            f_len = rep(1 / length(grid), length(grid))
          ))
        }

        return(tibble::tibble(
          length_cm = lo,
          f_len = 1
        ))
      }
    }

    if (length(midpoint_vals) > 0) {
      return(tibble::tibble(
        length_cm = midpoint_vals[[1]],
        f_len = 1
      ))
    }

    NULL
  }

  models_df <- similarity$candidate_models
  len_min_vals <- resolve_numeric_col(models_df, study_length_min_col)
  len_max_vals <- resolve_numeric_col(models_df, study_length_max_col)
  dep_min_vals <- resolve_numeric_col(models_df, study_depth_min_col)
  dep_max_vals <- resolve_numeric_col(models_df, study_depth_max_col)
  freq_vals <- resolve_numeric_col(models_df, "frequency")

  interval_distance_matrix <- function(min_vals,
                                       max_vals,
                                       method) {
    interval_overlap_distance_matrix(min_vals, max_vals, method)
  }

  frequency_distance_matrix <- frequency_offset_distance_matrix(
    freq_vals,
    cfg_now$frequency_coherence$method,
    similarity$frequency_span
  )

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

  slope_vals <- suppressWarnings(as.numeric(models_df$slope_len))
  intercept_vals <- suppressWarnings(as.numeric(models_df$intercept_len))
  slope_vals[!is.finite(slope_vals)] <- NA_real_
  intercept_vals[!is.finite(intercept_vals)] <- NA_real_

  # Score each held-out model against the full anchor length distribution
  # rather than a single reference length. This keeps tuning aligned with the
  # deployed PDF-integrated biomass-transfer objective.
  anchor_pdfs <- lapply(
    seq_len(model_n),
    function(j) build_tuning_anchor_pdf(models_df[j, , drop = FALSE])
  )

  donor_sigma_matrix <- matrix(NA_real_, nrow = model_n, ncol = model_n)
  target_sigma <- rep(NA_real_, model_n)
  valid_anchor <- rep(FALSE, model_n)
  species_labels <- stringr::str_squish(as.character(models_df$species_name))
  species_labels[!nzchar(species_labels)] <- NA_character_
  same_species_mask <- outer(
    species_labels,
    species_labels,
    function(donor_species, anchor_species) {
      !is.na(donor_species) &
        !is.na(anchor_species) &
        donor_species == anchor_species
    }
  )
  diag(same_species_mask) <- FALSE

  for (j in seq_len(model_n)) {
    anchor_pdf <- anchor_pdfs[[j]]
    if (is.null(anchor_pdf)) {
      next
    }

    donor_sigma_matrix[, j] <- vapply(
      seq_len(model_n),
      function(i) {
        equation_sigma_mean(
          slope = slope_vals[[i]],
          intercept = intercept_vals[[i]],
          anchor_pdf = anchor_pdf
        )
      },
      numeric(1)
    )

    target_sigma[[j]] <- donor_sigma_matrix[[j, j]]
    valid_anchor[[j]] <- is.finite(target_sigma[[j]]) && target_sigma[[j]] > 0
  }

  diag(donor_sigma_matrix) <- NA_real_

  list(
    species_penalty = ifelse(is.finite(species_dist_model), species_dist_model, 0),
    study_penalty = ifelse(is.finite(study_dist), study_dist, 0),
    length_penalty = ifelse(is.finite(length_distance_matrix), length_distance_matrix, 0),
    depth_penalty = ifelse(is.finite(depth_distance_matrix), depth_distance_matrix, 0),
    frequency_penalty = ifelse(is.finite(frequency_distance_matrix), frequency_distance_matrix, 0),
    length_weight = cfg_now$length_coherence$weight %||% 0,
    depth_weight = cfg_now$depth_coherence$weight %||% 0,
    frequency_weight = cfg_now$frequency_coherence$weight %||% 0,
    donor_sigma_matrix = donor_sigma_matrix,
    target_sigma = target_sigma,
    valid_anchor = valid_anchor,
    same_species_mask = same_species_mask,
    self_mask = diag(model_n) == 1,
    model_n = model_n,
    anchor_species = species_labels,
    anchor_model_id = model_ids
  )
}

#' Score one prepared similarity basis
#'
#' @param score_basis Prepared scoring basis returned by
#'   `prepare_similarity_score_basis()`.
#' @param alpha_now Numeric scalar.
#' @param k_species_now Numeric scalar.
#' @param k_study_now Numeric scalar.
#'
#' @return A one-row tibble with `rmse`, `mae`, and `n_eval`.
#'
#' @keywords internal
#' @noRd
score_similarity_basis <- function(score_basis,
                                   alpha_now,
                                   k_species_now,
                                   k_study_now,
                                   length_weight_now = NULL,
                                   depth_weight_now = NULL,
                                   frequency_weight_now = NULL,
                                   return_anchor_rows = FALSE) {
  model_n <- suppressWarnings(as.integer(score_basis$model_n %||% NA_integer_))
  if (!is.finite(model_n) || model_n < 1L) {
    model_n <- max(
      NROW(score_basis$species_penalty %||% numeric(0)),
      NROW(score_basis$study_penalty %||% numeric(0)),
      NROW(score_basis$length_penalty %||% numeric(0)),
      NROW(score_basis$depth_penalty %||% numeric(0)),
      NROW(score_basis$frequency_penalty %||% numeric(0)),
      NROW(score_basis$donor_sigma_matrix %||% numeric(0)),
      1L
    )
  }

  as_square_matrix <- function(x,
                               n) {
    if (is.matrix(x)) {
      return(x)
    }
    vals <- as.numeric(x)
    if (length(vals) == n * n) {
      return(matrix(vals, nrow = n, ncol = n))
    }
    if (length(vals) == n) {
      return(matrix(vals, nrow = n, ncol = 1))
    }
    matrix(vals, nrow = n)
  }

  species_penalty <- as_square_matrix(score_basis$species_penalty, model_n)
  study_penalty <- as_square_matrix(score_basis$study_penalty, model_n)
  length_penalty <- as_square_matrix(score_basis$length_penalty %||% 0, model_n)
  depth_penalty <- as_square_matrix(score_basis$depth_penalty %||% 0, model_n)
  frequency_penalty <- as_square_matrix(score_basis$frequency_penalty %||% 0, model_n)
  donor_sigma_matrix <- as_square_matrix(score_basis$donor_sigma_matrix, model_n)
  self_mask <- score_basis$self_mask
  if (!is.matrix(self_mask)) {
    self_mask <- as.logical(as_square_matrix(self_mask, model_n))
  }
  same_species_mask <- score_basis$same_species_mask
  if (!is.matrix(same_species_mask)) {
    same_species_mask <- matrix(FALSE, nrow = model_n, ncol = model_n)
  }
  target_sigma <- as.numeric(score_basis$target_sigma)
  valid_anchor <- as.logical(score_basis$valid_anchor)

  normalize_penalty_matrix <- function(x) {
    vals <- as.numeric(x)
    vals <- vals[is.finite(vals) & vals > 0]
    scale_now <- if (length(vals) == 0) NA_real_ else stats::median(vals, na.rm = TRUE)
    if (!is.finite(scale_now) || scale_now <= 0) {
      return(x)
    }
    x / scale_now
  }

  species_penalty <- normalize_penalty_matrix(species_penalty)
  study_penalty <- normalize_penalty_matrix(study_penalty)

  balance_now <- min(max(as.numeric(alpha_now), 1e-6), 1 - 1e-6)
  species_scale_now <- suppressWarnings(as.numeric(k_species_now[[1]] %||% NA_real_))
  study_scale_now <- suppressWarnings(as.numeric(k_study_now[[1]] %||% NA_real_))
  kernel_scale_now <- sqrt(max(0, species_scale_now) * max(0, study_scale_now))
  if (!is.finite(kernel_scale_now) || kernel_scale_now < 0) {
    kernel_scale_now <- max(c(species_scale_now, study_scale_now, 0), na.rm = TRUE)
  }
  if (!is.finite(kernel_scale_now) || kernel_scale_now < 0) {
    kernel_scale_now <- 0
  }
  length_weight_now <- suppressWarnings(as.numeric(length_weight_now %||% score_basis$length_weight %||% 0))
  depth_weight_now <- suppressWarnings(as.numeric(depth_weight_now %||% score_basis$depth_weight %||% 0))
  frequency_weight_now <- suppressWarnings(as.numeric(frequency_weight_now %||% score_basis$frequency_weight %||% 0))
  if (!is.finite(length_weight_now) || length_weight_now < 0) {
    length_weight_now <- 0
  }
  if (!is.finite(depth_weight_now) || depth_weight_now < 0) {
    depth_weight_now <- 0
  }
  if (!is.finite(frequency_weight_now) || frequency_weight_now < 0) {
    frequency_weight_now <- 0
  }

  species_kernel <- kernel_scale_now * balance_now * species_penalty
  study_kernel <- kernel_scale_now * (1 - balance_now) * study_penalty
  coherence_penalty <- (length_weight_now * length_penalty) +
    (depth_weight_now * depth_penalty) +
    (frequency_weight_now * frequency_penalty)
  total_penalty <- species_kernel + study_kernel + coherence_penalty
  total_penalty[same_species_mask] <- Inf
  total_penalty[self_mask] <- Inf

  w_raw <- exp(-total_penalty)
  w_raw[!is.finite(w_raw)] <- 0
  w_sum <- colSums(w_raw, na.rm = TRUE)

  valid_cols <- valid_anchor & is.finite(w_sum) & w_sum > 0
  if (!any(valid_cols)) {
    out <- tibble::tibble(rmse = NA_real_, mae = NA_real_, mse = NA_real_, se_mse = NA_real_, n_eval = 0L)
    if (isTRUE(return_anchor_rows)) {
      attr(out, "anchor_rows") <- tibble::tibble()
    }
    return(out)
  }

  w <- sweep(w_raw[, valid_cols, drop = FALSE], 2, w_sum[valid_cols], "/")
  pred_sigma <- colSums(donor_sigma_matrix[, valid_cols, drop = FALSE] * w, na.rm = TRUE)
  keep <- is.finite(pred_sigma) &
    pred_sigma > 0 &
    is.finite(target_sigma[valid_cols]) &
    target_sigma[valid_cols] > 0

  if (!any(keep)) {
    out <- tibble::tibble(rmse = NA_real_, mae = NA_real_, mse = NA_real_, se_mse = NA_real_, n_eval = 0L)
    if (isTRUE(return_anchor_rows)) {
      attr(out, "anchor_rows") <- tibble::tibble()
    }
    return(out)
  }

  errs <- log(pred_sigma[keep]) - log(target_sigma[valid_cols][keep])
  sq_errs <- errs^2
  out <- tibble::tibble(
    rmse = sqrt(mean(sq_errs)),
    mae = mean(abs(errs)),
    mse = mean(sq_errs),
    se_mse = if (length(sq_errs) > 1L) stats::sd(sq_errs) / sqrt(length(sq_errs)) else 0,
    n_eval = length(errs)
  )
  if (isTRUE(return_anchor_rows)) {
    anchor_ids <- which(valid_cols)[keep]
    effective_support <- 1 / colSums(w[, keep, drop = FALSE]^2, na.rm = TRUE)
    donor_weight_max <- apply(w[, keep, drop = FALSE], 2, max, na.rm = TRUE)
    anchor_rows <- tibble::tibble(
      anchor_index = anchor_ids,
      anchor_model_id = as.character((score_basis$anchor_model_id %||% character(0))[anchor_ids]),
      species_name = as.character((score_basis$anchor_species %||% character(0))[anchor_ids]),
      log_error = errs,
      abs_error = abs(errs),
      sq_error = sq_errs,
      effective_support = as.numeric(effective_support),
      donor_weight_max = as.numeric(donor_weight_max)
    )
    attr(out, "anchor_rows") <- anchor_rows
  }
  out
}

#' Score one tuning-search task
#'
#' @param task_now One tuning-task definition.
#' @param shared_data Shared worker data exported once per cluster.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
score_tuning_task <- function(task_now,
                              shared_data) {
  score_basis <- shared_data$score_basis
  kernel_scale_now <- suppressWarnings(as.numeric(task_now$kernel_scale[[1]] %||% NA_real_))
  if (!is.finite(kernel_scale_now)) {
    kernel_scale_now <- sqrt(max(0, as.numeric(task_now$k_species[[1]] %||% 0)) * max(0, as.numeric(task_now$k_study[[1]] %||% 0)))
  }

  score_similarity_basis(
    score_basis = score_basis,
    alpha_now = task_now$alpha,
    k_species_now = task_now$k_species,
    k_study_now = task_now$k_study,
    length_weight_now = task_now$length_weight %||% score_basis$length_weight,
    depth_weight_now = task_now$depth_weight %||% score_basis$depth_weight,
    frequency_weight_now = task_now$frequency_weight %||% score_basis$frequency_weight
  ) |>
    dplyr::mutate(
      stage = task_now$stage,
      alpha = task_now$alpha,
      kernel_scale = kernel_scale_now,
      coherence_scale = task_now$coherence_scale %||% 1,
      k_species = task_now$k_species,
      k_study = task_now$k_study,
      length_weight = task_now$length_weight %||% score_basis$length_weight,
      depth_weight = task_now$depth_weight %||% score_basis$depth_weight,
      frequency_weight = task_now$frequency_weight %||% score_basis$frequency_weight
    )
}

#' Score one component-dropout task
#'
#' @param task_now One component-dropout task definition.
#' @param shared_data Shared worker data exported once per cluster.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
score_dropout_task <- function(task_now,
                               shared_data) {
  tune_models <- shared_data$tune_models
  base_sim <- shared_data$base_sim
  alpha_best <- shared_data$alpha_best
  k_species_best <- shared_data$k_species_best
  k_study_best <- shared_data$k_study_best
  registry_path <- shared_data$registry_path

  if (identical(task_now$component_type, "species_trait")) {
    species_weights_now <- base_sim$species_weights
    species_weights_now[[task_now$component]] <- 0
    score_basis_now <- prepare_similarity_score_basis(
      models_subset = tune_models,
      species_weights = species_weights_now,
      study_weights = base_sim$study_weights,
      alpha_now = alpha_best,
      k_species_now = k_species_best,
      k_study_now = k_study_best,
      cfg_now = base_sim$config,
      registry_path = registry_path,
      seed_now = base_sim$seed
    )
    return(
      score_similarity_basis(
        score_basis = score_basis_now,
        alpha_now = alpha_best,
        k_species_now = k_species_best,
        k_study_now = k_study_best
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
    score_basis_now <- prepare_similarity_score_basis(
      models_subset = tune_models,
      species_weights = base_sim$species_weights,
      study_weights = study_weights_now,
      alpha_now = alpha_best,
      k_species_now = k_species_best,
      k_study_now = k_study_best,
      cfg_now = base_sim$config,
      registry_path = registry_path,
      seed_now = base_sim$seed
    )
    return(
      score_similarity_basis(
        score_basis = score_basis_now,
        alpha_now = alpha_best,
        k_species_now = k_species_best,
        k_study_now = k_study_best
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
  score_basis_now <- prepare_similarity_score_basis(
    models_subset = tune_models,
    species_weights = base_sim$species_weights,
    study_weights = base_sim$study_weights,
    alpha_now = alpha_best,
    k_species_now = k_species_best,
    k_study_now = k_study_best,
    cfg_now = cfg_now,
    registry_path = registry_path,
    seed_now = base_sim$seed
  )
  score_similarity_basis(
    score_basis = score_basis_now,
    alpha_now = alpha_best,
    k_species_now = k_species_best,
    k_study_now = k_study_best
  ) |>
    dplyr::mutate(
      component = task_now$component,
      component_type = "coherence",
      alpha = alpha_best,
      k_species = k_species_best,
      k_study = k_study_best
    )
}

#' Run similarity tasks in parallel
#'
#' @param tasks Task list.
#' @param worker_name Internal worker-function name.
#' @param workers Number of workers.
#' @param shared_data Shared worker data exported once per cluster.
#' @param warning_text Warning prefix used if the parallel path fails.
#'
#' @return A list of task results.
#'
#' @keywords internal
#' @noRd
score_tasks_parallel <- function(tasks,
                                 worker_name,
                                 workers,
                                 shared_data,
                                 warning_text) {
  run_serial <- function() {
    lapply(seq_along(tasks), function(task_id) {
      task_now <- tasks[[task_id]]
      tryCatch(
        get(worker_name, envir = asNamespace("tsbiomass"), inherits = FALSE)(
          task_now = task_now,
          shared_data = shared_data
        ),
        error = function(e) {
          stop(
            sprintf(
              "%s task %s failed: %s",
              worker_name,
              as.character(task_id),
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
    })
  }

  workers <- suppressWarnings(as.integer(workers[[1]]))
  if (!is.finite(workers) || workers < 2L || length(tasks) < 2L) {
    return(run_serial())
  }

  cluster_obj <- initialize_parallel_cluster(
    workers = min(length(tasks), workers)
  )
  on.exit(parallel::stopCluster(cluster_obj), add = TRUE)

  tsb_cluster_export(
    cluster_obj,
    c("tasks", "shared_data"),
    envir = environment()
  )

  tryCatch(
    parallel::parLapplyLB(
      cluster_obj,
      seq_along(tasks),
      function(task_id, worker_name_now) {
        task_now <- tasks[[task_id]]
        worker_fun <- get(worker_name_now, envir = asNamespace("tsbiomass"), inherits = FALSE)
        tryCatch(
          worker_fun(task_now = task_now, shared_data = shared_data),
          error = function(e) {
            stop(
              sprintf(
                "%s task %s failed: %s",
                worker_name_now,
                as.character(task_id),
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        )
      },
      worker_name_now = worker_name
    ),
    error = function(e) {
      warning(
        paste(
          warning_text,
          "Reason:",
          conditionMessage(e)
        ),
        call. = FALSE
      )
      run_serial()
    }
  )
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
#' @noRd
score_similarity_config <- function(models_subset,
                                    species_weights,
                                    study_weights,
                                    alpha_now,
                                    k_species_now,
                                    k_study_now,
                                    cfg_now,
                                    registry_path,
                                    seed_now) {
  score_basis <- prepare_similarity_score_basis(
    models_subset = models_subset,
    species_weights = species_weights,
    study_weights = study_weights,
    alpha_now = alpha_now,
    k_species_now = k_species_now,
    k_study_now = k_study_now,
    cfg_now = cfg_now,
    registry_path = registry_path,
    seed_now = seed_now
  )

  score_similarity_basis(
    score_basis = score_basis,
    alpha_now = alpha_now,
    k_species_now = k_species_now,
    k_study_now = k_study_now
  )
}

#' Run the local alpha-k tuning grid
#'
#' @param tune_models Tuning subset returned by `build_tuning_subset()`.
#' @param base_sim Prepared similarity object returned by
#'   [prepare_similarities()].
#' @param registry_path Optional path to the trait-registry JSON.
#'
#' @return A list with `baseline`, `grid_scores`, `alpha_best`, `k_species_best`,
#'   and `k_study_best`.
#'
#' @keywords internal
#' @noRd
run_tuning_grid_search <- function(tune_models,
                                   base_sim,
                                   registry_path,
                                   n_cores = 1L,
                                   progress = FALSE) {
  seed_base <- suppressWarnings(as.integer(base_sim$seed %||% 1L))
  if (!is.finite(seed_base) || is.na(seed_base)) {
    seed_base <- 1L
  }

  score_basis <- prepare_similarity_score_basis(
    models_subset = tune_models,
    species_weights = base_sim$species_weights,
    study_weights = base_sim$study_weights,
    alpha_now = base_sim$alpha,
    k_species_now = base_sim$k_species,
    k_study_now = base_sim$k_study,
    cfg_now = base_sim$config,
    registry_path = registry_path,
    seed_now = seed_base
  )

  base_score <- score_similarity_basis(
    score_basis = score_basis,
    alpha_now = base_sim$alpha,
    k_species_now = base_sim$k_species,
    k_study_now = base_sim$k_study
  ) |>
    dplyr::mutate(
      stage = "baseline",
      alpha = base_sim$alpha,
      kernel_scale = kernel_scale_center(base_sim$k_species, base_sim$k_study),
      k_species = base_sim$k_species,
      k_study = base_sim$k_study,
      length_weight = base_sim$config$length_coherence$weight %||% 0,
      depth_weight = base_sim$config$depth_coherence$weight %||% 0,
      frequency_weight = base_sim$config$frequency_coherence$weight %||% 0
    )
  regularization_cfg <- base_sim$config$regularization %||% list()

  active_coherence <- list()
  for (coherence_name in c("length_coherence", "depth_coherence", "frequency_coherence")) {
    cfg_now <- base_sim$config[[coherence_name]] %||% list()
    if (identical(cfg_now$method %||% "overlap", "none")) {
      next
    }

    center_weight <- suppressWarnings(as.numeric(cfg_now$weight %||% 1))
    if (!is.finite(center_weight) || center_weight < 0) {
      center_weight <- 0
    }

    range_now <- cfg_now$range
    if (length(range_now) < 2) {
      range_now <- c(max(0, center_weight / 2), max(1, center_weight * 2))
    }
    range_now <- sort(unique(as.numeric(range_now)))
    range_now <- range_now[is.finite(range_now) & range_now >= 0]
    if (length(range_now) < 2) {
      range_now <- c(max(0, center_weight / 2), max(1, center_weight * 2))
    }

    grid_now <- sort(unique(as.numeric(cfg_now$grid %||% range_now %||% center_weight)))
    grid_now <- grid_now[is.finite(grid_now) & grid_now >= 0]
    if (length(grid_now) == 0) {
      grid_now <- sort(unique(c(range_now, center_weight)))
    }

    active_coherence[[length(active_coherence) + 1L]] <- list(
      cfg_name = coherence_name,
      field = sub("_coherence$", "_weight", coherence_name),
      bounds = range(range_now, na.rm = TRUE),
      center = center_weight,
      grid = grid_now
    )
  }
  coherence_block <- local({
    if (length(active_coherence) == 0) {
      return(list(enabled = FALSE, center = 1, bounds = c(1, 1), grid = 1))
    }
    scale_lower <- numeric(0)
    scale_upper <- numeric(0)
    scale_grid <- 1
    for (comp_now in active_coherence) {
      center_now <- comp_now$center
      if (!is.finite(center_now) || center_now <= 0) {
        center_now <- 1
      }
      scale_lower <- c(scale_lower, comp_now$bounds[[1]] / center_now)
      scale_upper <- c(scale_upper, comp_now$bounds[[2]] / center_now)
      scale_grid <- c(scale_grid, comp_now$grid / center_now)
    }
    bounds_now <- c(max(scale_lower, na.rm = TRUE), min(scale_upper, na.rm = TRUE))
    if (!all(is.finite(bounds_now)) || bounds_now[[1]] > bounds_now[[2]]) {
      bounds_now <- c(1, 1)
    }
    scale_grid <- sort(unique(scale_grid[is.finite(scale_grid) & scale_grid >= bounds_now[[1]] & scale_grid <= bounds_now[[2]]]))
    if (length(scale_grid) == 0) {
      scale_grid <- sort(unique(c(bounds_now, 1)))
    }
    list(
      enabled = TRUE,
      center = 1,
      bounds = bounds_now,
      grid = scale_grid
    )
  })

  apply_coherence_scale <- function(tbl) {
    tbl <- tibble::as_tibble(tbl)
    if (!"coherence_scale" %in% names(tbl)) {
      tbl$coherence_scale <- 1
    }
    if (length(active_coherence) == 0) {
      if (!"length_weight" %in% names(tbl)) {
        tbl$length_weight <- base_sim$config$length_coherence$weight %||% 0
      }
      if (!"depth_weight" %in% names(tbl)) {
        tbl$depth_weight <- base_sim$config$depth_coherence$weight %||% 0
      }
      if (!"frequency_weight" %in% names(tbl)) {
        tbl$frequency_weight <- base_sim$config$frequency_coherence$weight %||% 0
      }
      return(tbl)
    }
    for (comp_now in active_coherence) {
      weight_now <- comp_now$center * tbl$coherence_scale
      weight_now <- pmax(comp_now$bounds[[1]], pmin(comp_now$bounds[[2]], weight_now))
      tbl[[comp_now$field]] <- weight_now
    }
    if (!"length_weight" %in% names(tbl)) {
      tbl$length_weight <- base_sim$config$length_coherence$weight %||% 0
    }
    if (!"depth_weight" %in% names(tbl)) {
      tbl$depth_weight <- base_sim$config$depth_coherence$weight %||% 0
    }
    if (!"frequency_weight" %in% names(tbl)) {
      tbl$frequency_weight <- base_sim$config$frequency_coherence$weight %||% 0
    }
    tbl
  }

  latin_hypercube_points <- function(n_points,
                                     n_dimensions,
                                     seed_now) {
    if (n_points < 1L || n_dimensions < 1L) {
      return(matrix(numeric(0), nrow = 0, ncol = n_dimensions))
    }

    set.seed(seed_now)
    out <- matrix(NA_real_, nrow = n_points, ncol = n_dimensions)
    for (dimension_id in seq_len(n_dimensions)) {
      lower_bounds <- (seq_len(n_points) - 1) / n_points
      upper_bounds <- seq_len(n_points) / n_points
      draws <- stats::runif(n_points, min = lower_bounds, max = upper_bounds)
      out[, dimension_id] <- sample(draws, size = n_points, replace = FALSE)
    }

    out
  }

  scale_search_value <- function(unit_value,
                                 lower_value,
                                 upper_value,
                                 logarithmic = FALSE) {
    if (!is.finite(lower_value) || !is.finite(upper_value)) {
      return(NA_real_)
    }
    if (identical(lower_value, upper_value)) {
      return(as.numeric(lower_value))
    }
    if (isTRUE(logarithmic)) {
      lower_log <- log1p(max(0, lower_value))
      upper_log <- log1p(max(0, upper_value))
      return(expm1(lower_log + unit_value * (upper_log - lower_log)))
    }

    lower_value + unit_value * (upper_value - lower_value)
  }

  build_screening_subset <- function(candidate_models,
                                     species_weights,
                                     study_weights,
                                     max_models_per_species,
                                     species_fraction,
                                     seed_now) {
    species_values <- unique(as.character(candidate_models$species_name))
    species_values <- species_values[!is.na(species_values) & nzchar(species_values)]
    if (length(species_values) < 2L || species_fraction >= 0.999) {
      return(tibble::as_tibble(candidate_models))
    }

    keep_species <- max(2L, min(length(species_values), ceiling(length(species_values) * species_fraction)))
    set.seed(seed_now)
    selected_species <- sample(species_values, size = keep_species, replace = FALSE)

    build_tuning_subset(
      candidate_models = dplyr::filter(candidate_models, .data$species_name %in% selected_species),
      species_weights = species_weights,
      study_weights = study_weights,
      max_models_per_species = max_models_per_species,
      seed = seed_now
    )
  }

  refinement_midpoint <- function(x,
                                  y,
                                  geometric = FALSE) {
    if (!is.finite(x) || !is.finite(y)) {
      return(NA_real_)
    }
    if (geometric && x > 0 && y > 0) {
      return(sqrt(x * y))
    }
    mean(c(x, y))
  }

  build_refined_grid <- function(best_row,
                                 alpha_values,
                                 kernel_scale_values,
                                 coherence_scale_values,
                                 levels) {
    if (!is.finite(levels) || levels < 1L) {
      return(tibble::tibble())
    }

    current_grid <- tibble::as_tibble(best_row)
    all_rows <- list()

    for (lvl in seq_len(levels)) {
      best_now <- current_grid |>
        dplyr::arrange(.data$rmse, .data$mae, dplyr::desc(.data$n_eval)) |>
        dplyr::slice(1)

      nearest_values <- function(values,
                                 center,
                                 geometric = FALSE) {
        values <- sort(unique(values[is.finite(values)]))
        lower <- values[values < center]
        upper <- values[values > center]
        out <- center
        if (length(lower) > 0) {
          out <- c(out, refinement_midpoint(max(lower), center, geometric = geometric))
        }
        if (length(upper) > 0) {
          out <- c(out, refinement_midpoint(center, min(upper), geometric = geometric))
        }
        sort(unique(out[is.finite(out)]))
      }

      alpha_local <- nearest_values(alpha_values, best_now$alpha[[1]], geometric = FALSE)
      scale_local <- nearest_values(kernel_scale_values, best_now$kernel_scale[[1]], geometric = TRUE)
      dim_grid <- list(
        alpha = alpha_local,
        kernel_scale = scale_local
      )
      if (isTRUE(coherence_block$enabled)) {
        dim_grid$coherence_scale <- nearest_values(
          coherence_scale_values %||% coherence_block$grid,
          best_now$coherence_scale[[1]] %||% coherence_block$center,
          geometric = TRUE
        )
      }

      refined_cube <- do.call(
        expand.grid,
        c(dim_grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      ) |>
        tibble::as_tibble() |>
        dplyr::mutate(
          k_species = .data$kernel_scale,
          k_study = .data$kernel_scale,
          stage = sprintf("search_refine_%s", lvl)
        )
      refined_cube <- apply_coherence_scale(refined_cube)

      all_rows[[length(all_rows) + 1L]] <- refined_cube
      current_grid <- dplyr::bind_rows(
        current_grid,
        refined_cube |>
          dplyr::mutate(rmse = NA_real_, mae = NA_real_, n_eval = NA_integer_)
      )

      alpha_values <- sort(unique(c(alpha_values, alpha_local)))
      kernel_scale_values <- sort(unique(c(kernel_scale_values, scale_local)))
      if (isTRUE(coherence_block$enabled)) {
        coherence_scale_values <- sort(unique(c(
          coherence_scale_values %||% numeric(0),
          dim_grid$coherence_scale
        )))
      }
    }

    dplyr::bind_rows(all_rows)
  }

  evaluate_grid_rows <- function(grid_tbl,
                                 score_basis) {
    if (nrow(grid_tbl) == 0) {
      return(tibble::tibble())
    }

    grid_tasks <- lapply(seq_len(nrow(grid_tbl)), function(i) {
      list(
        stage = grid_tbl$stage[[i]],
        alpha = grid_tbl$alpha[[i]],
        kernel_scale = grid_tbl$kernel_scale[[i]],
        coherence_scale = grid_tbl$coherence_scale[[i]] %||% 1,
        k_species = grid_tbl$kernel_scale[[i]],
        k_study = grid_tbl$kernel_scale[[i]],
        length_weight = grid_tbl$length_weight[[i]],
        depth_weight = grid_tbl$depth_weight[[i]],
        frequency_weight = grid_tbl$frequency_weight[[i]]
      )
    })

    score_tasks_parallel(
      tasks = grid_tasks,
      worker_name = "score_tuning_task",
      workers = n_cores,
      shared_data = list(score_basis = score_basis),
      warning_text = "Parallel similarity tuning failed and is falling back to serial evaluation."
    ) |>
      dplyr::bind_rows()
  }

  successive_halving <- function(design_tbl,
                                 base_sim,
                                 tune_models,
                                 registry_path,
                                 n_cores,
                                 progress = FALSE) {
    species_count <- max(1L, dplyr::n_distinct(tune_models$species_name))
    stage_specs <- list(
      list(stage = "search_screen_1", species_fraction = 0.4, model_limit = 1L),
      list(stage = "search_screen_2", species_fraction = 0.7, model_limit = max(1L, floor(nrow(tune_models) / species_count))),
      list(stage = "search_full", species_fraction = 1.0, model_limit = max(1L, ceiling(nrow(tune_models) / species_count)))
    )

    current_design <- tibble::as_tibble(design_tbl)
    score_rows <- list()

    for (stage_id in seq_along(stage_specs)) {
      stage_spec <- stage_specs[[stage_id]]
      models_now <- build_screening_subset(
        candidate_models = tune_models,
        species_weights = base_sim$species_weights,
        study_weights = base_sim$study_weights,
        max_models_per_species = stage_spec$model_limit,
        species_fraction = stage_spec$species_fraction,
        seed_now = seed_base + stage_id
      )

      report_progress(
        progress,
        "Similarity tuning stage ",
        stage_id,
        "/",
        length(stage_specs),
        ": evaluating ",
        nrow(current_design),
        " candidate setting(s) on ",
        nrow(models_now),
        " tuning model(s) across ",
        dplyr::n_distinct(models_now$species_name),
        " species."
      )

      stage_basis <- prepare_similarity_score_basis(
        models_subset = models_now,
        species_weights = base_sim$species_weights,
        study_weights = base_sim$study_weights,
        alpha_now = base_sim$alpha,
        k_species_now = base_sim$k_species,
        k_study_now = base_sim$k_study,
        cfg_now = base_sim$config,
        registry_path = registry_path,
        seed_now = seed_base
      )

      stage_design <- current_design |>
        dplyr::mutate(stage = stage_spec$stage)
      stage_scores <- evaluate_grid_rows(stage_design, stage_basis)
      score_rows[[length(score_rows) + 1L]] <- stage_scores

      if (!identical(stage_spec$stage, "search_full")) {
        survivors_now <- max(3L, ceiling(nrow(stage_scores) / 2))
        report_progress(
          progress,
          "Similarity tuning stage ",
          stage_id,
          ": retaining ",
          survivors_now,
          " of ",
          nrow(stage_scores),
          " candidate setting(s). Best RMSE so far = ",
          signif(min(stage_scores$rmse[is.finite(stage_scores$rmse)], na.rm = TRUE), 4),
          "."
        )
        current_design <- stage_scores |>
          dplyr::arrange(.data$rmse, .data$mae, dplyr::desc(.data$n_eval)) |>
          dplyr::slice_head(n = survivors_now) |>
          dplyr::select("alpha", "kernel_scale", "coherence_scale", "k_species", "k_study", "length_weight", "depth_weight", "frequency_weight") |>
          dplyr::distinct()
      } else {
        report_progress(
          progress,
          "Similarity tuning final screening stage completed. Best RMSE = ",
          signif(min(stage_scores$rmse[is.finite(stage_scores$rmse)], na.rm = TRUE), 4),
          "."
        )
      }
    }

    dplyr::bind_rows(score_rows)
  }

  alpha_bounds <- if (length(base_sim$config$alpha_range) >= 2) {
    range(base_sim$config$alpha_range, na.rm = TRUE)
  } else {
    c(max(0.05, base_sim$alpha - 0.2), min(0.95, base_sim$alpha + 0.2))
  }
  scale_bounds <- if (length(base_sim$config$kernel_scale_range) >= 2) {
    range(base_sim$config$kernel_scale_range, na.rm = TRUE)
  } else {
    scale_center_now <- kernel_scale_center(base_sim$k_species, base_sim$k_study)
    if (!is.finite(scale_center_now) || scale_center_now < 0) {
      scale_center_now <- max(c(base_sim$k_species, base_sim$k_study, 1), na.rm = TRUE)
    }
    c(max(0, scale_center_now / 2), max(1, scale_center_now * 2))
  }
  alpha_center <- base_sim$alpha
  scale_center <- kernel_scale_center(base_sim$k_species, base_sim$k_study)
  if (!is.finite(scale_center) || scale_center < 0) {
    scale_center <- max(base_sim$k_species, base_sim$k_study, 1, na.rm = TRUE)
  }
  build_search_design <- function(base_sim,
                                  alpha_bounds,
                                  scale_bounds,
                                  alpha_center,
                                  scale_center,
                                  n_points = 12L) {
    corner_dims <- list(
      alpha = unique(alpha_bounds),
      kernel_scale = unique(scale_bounds)
    )
    if (isTRUE(coherence_block$enabled)) {
      corner_dims$coherence_scale <- unique(coherence_block$bounds)
    }

    corner_grid <- do.call(
      expand.grid,
      c(corner_dims, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    ) |>
      tibble::as_tibble() |>
      dplyr::mutate(stage = "search_design_corner")

    center_row <- tibble::tibble(
      alpha = alpha_center,
      kernel_scale = scale_center
    )
    if (isTRUE(coherence_block$enabled)) {
      center_row$coherence_scale <- coherence_block$center
    }
    center_row$stage <- "search_design_center"

    lhs_matrix <- latin_hypercube_points(
      n_points = n_points,
      n_dimensions = 2L + if (isTRUE(coherence_block$enabled)) 1L else 0L,
      seed_now = seed_base
    )
    latin_rows <- tibble::tibble(
      alpha = vapply(lhs_matrix[, 1], scale_search_value, numeric(1), lower_value = alpha_bounds[[1]], upper_value = alpha_bounds[[2]], logarithmic = FALSE),
      kernel_scale = vapply(lhs_matrix[, 2], scale_search_value, numeric(1), lower_value = scale_bounds[[1]], upper_value = scale_bounds[[2]], logarithmic = TRUE)
    )
    if (isTRUE(coherence_block$enabled)) {
      latin_rows$coherence_scale <- vapply(
        lhs_matrix[, 3],
        scale_search_value,
        numeric(1),
        lower_value = coherence_block$bounds[[1]],
        upper_value = coherence_block$bounds[[2]],
        logarithmic = TRUE
      )
    }
    latin_rows$stage <- "search_design_latin"

    out <- dplyr::bind_rows(corner_grid, center_row, latin_rows) |>
      dplyr::filter(
        is.finite(.data$alpha),
        is.finite(.data$kernel_scale),
        .data$alpha > 0,
        .data$alpha < 1,
        .data$kernel_scale >= 0
      ) |>
      dplyr::mutate(
        k_species = .data$kernel_scale,
        k_study = .data$kernel_scale
      )
    out <- apply_coherence_scale(out)

    out |>
      dplyr::distinct(.data$alpha, .data$kernel_scale, .data$coherence_scale, .data$length_weight, .data$depth_weight, .data$frequency_weight, .keep_all = TRUE)
  }

  design_tbl <- build_search_design(
    base_sim = base_sim,
    alpha_bounds = alpha_bounds,
    scale_bounds = scale_bounds,
    alpha_center = alpha_center,
    scale_center = scale_center,
    n_points = 12L
  )
  grid_scores <- successive_halving(
    design_tbl = design_tbl,
    base_sim = base_sim,
    tune_models = tune_models,
    registry_path = registry_path,
    n_cores = n_cores,
    progress = progress
  )

  refined_grid <- build_refined_grid(
    best_row = grid_scores,
    alpha_values = sort(unique(c(base_sim$config$alpha_grid, alpha_bounds, grid_scores$alpha))),
    kernel_scale_values = sort(unique(c(
      base_sim$config$kernel_scale_range,
      base_sim$config$k_species_grid,
      base_sim$config$k_study_grid,
      scale_bounds,
      grid_scores$kernel_scale
    ))),
    coherence_scale_values = coherence_block$grid,
    levels = base_sim$config$grid_refinement_levels %||% 0L
  )

  if (nrow(refined_grid) > 0) {
    report_progress(
      progress,
      "Similarity tuning local refinement: evaluating ",
      nrow(refined_grid),
      " additional candidate setting(s)."
    )
    seen_keys <- paste(
      grid_scores$alpha,
      grid_scores$kernel_scale,
      grid_scores$coherence_scale %||% 1,
      sep = "::"
    )
    refined_keys <- paste(
      refined_grid$alpha,
      refined_grid$kernel_scale,
      refined_grid$coherence_scale %||% 1,
      sep = "::"
    )
    refined_grid <- refined_grid[!refined_keys %in% seen_keys, , drop = FALSE]
  }

  refined_scores <- evaluate_grid_rows(refined_grid, score_basis)
  grid_scores <- dplyr::bind_rows(grid_scores, refined_scores) |>
    dplyr::mutate(search_round = 1L, .before = 1)

  selection_pool <- grid_scores |>
    dplyr::filter(is.finite(.data$n_eval))
  if (nrow(selection_pool) > 0) {
    selection_pool <- selection_pool |>
      dplyr::filter(.data$n_eval == max(.data$n_eval, na.rm = TRUE))
  } else {
    selection_pool <- grid_scores |>
      dplyr::filter(is.finite(.data$rmse))
  }
  if (nrow(selection_pool) == 0) {
    selection_pool <- grid_scores
  }
  finite_mse <- is.finite(selection_pool$mse)
  if (any(finite_mse)) {
    best_mse <- min(selection_pool$mse[finite_mse], na.rm = TRUE)
    best_idx <- which.min(dplyr::if_else(finite_mse, selection_pool$mse, Inf))
    best_se <- selection_pool$se_mse[[best_idx]]
    if (!is.finite(best_se)) {
      best_se <- 0
    }
    mse_tolerance <- max(best_se, (base_sim$config$rmse_tolerance %||% 0)^2)
    eligible_cfg <- selection_pool |>
      dplyr::filter(is.finite(.data$mse), .data$mse <= best_mse + mse_tolerance)
  } else {
    eligible_cfg <- selection_pool |>
      dplyr::filter(is.finite(.data$rmse))
  }
  if (nrow(eligible_cfg) == 0) {
    eligible_cfg <- selection_pool |>
      dplyr::filter(is.finite(.data$rmse))
  }
  if (nrow(eligible_cfg) == 0) {
    eligible_cfg <- grid_scores |>
      dplyr::filter(is.finite(.data$rmse))
  }
  if (nrow(eligible_cfg) == 0) {
    eligible_cfg <- grid_scores
  }
  alpha_scale <- max(diff(alpha_bounds), 1e-8)
  kernel_scale_width <- max(diff(log1p(scale_bounds)), 1e-8)
  coherence_scale_width <- max(diff(log1p(coherence_block$bounds)), 1e-8)
  fit_scale <- max(sqrt(mse_tolerance %||% 0), base_sim$config$rmse_tolerance %||% 0, 1e-8)
  edge_margin <- max(regularization_cfg$edge_margin %||% 0.15, 1e-8)
  best_rmse_eligible <- suppressWarnings(min(eligible_cfg$rmse[is.finite(eligible_cfg$rmse)], na.rm = TRUE))
  if (!is.finite(best_rmse_eligible)) {
    best_rmse_eligible <- NA_real_
  }
  eligible_cfg <- eligible_cfg |>
    dplyr::mutate(
      coherence_scale = dplyr::coalesce(.data$coherence_scale, 1),
      alpha_deviation = abs(.data$alpha - alpha_center) / alpha_scale,
      kernel_scale_deviation = abs(log1p(.data$kernel_scale) - log1p(scale_center)) / kernel_scale_width,
      coherence_scale_deviation = if (isTRUE(coherence_block$enabled)) {
        abs(log1p(.data$coherence_scale) - log1p(coherence_block$center)) / coherence_scale_width
      } else {
        0
      },
      alpha_edge_proximity = pmin(.data$alpha - alpha_bounds[[1]], alpha_bounds[[2]] - .data$alpha) / alpha_scale,
      kernel_scale_edge_proximity = pmin(
        abs(log1p(.data$kernel_scale) - log1p(scale_bounds[[1]])),
        abs(log1p(scale_bounds[[2]]) - log1p(.data$kernel_scale))
      ) / kernel_scale_width,
      coherence_scale_edge_proximity = if (isTRUE(coherence_block$enabled)) {
        pmin(
          abs(log1p(.data$coherence_scale) - log1p(coherence_block$bounds[[1]])),
          abs(log1p(coherence_block$bounds[[2]]) - log1p(.data$coherence_scale))
        ) / coherence_scale_width
      } else {
        Inf
      },
      alpha_edge_penalty = pmax(0, edge_margin - .data$alpha_edge_proximity) / edge_margin,
      kernel_scale_edge_penalty = pmax(0, edge_margin - .data$kernel_scale_edge_proximity) / edge_margin,
      coherence_scale_edge_penalty = if (isTRUE(coherence_block$enabled)) {
        pmax(0, edge_margin - .data$coherence_scale_edge_proximity) / edge_margin
      } else {
        0
      },
      edge_penalty = .data$alpha_edge_penalty + .data$kernel_scale_edge_penalty + .data$coherence_scale_edge_penalty,
      stability_penalty = dplyr::if_else(is.finite(.data$se_mse), .data$se_mse, 0),
      fit_penalty = if (is.finite(best_rmse_eligible)) {
        pmax(0, .data$rmse - best_rmse_eligible) / fit_scale
      } else {
        0
      },
      complexity_penalty =
        (regularization_cfg$alpha %||% 0) * .data$alpha_deviation +
          (regularization_cfg$kernel_scale %||% 0) * .data$kernel_scale_deviation +
          (regularization_cfg$coherence_scale %||% 0) * .data$coherence_scale_deviation,
      regularized_objective = .data$rmse +
        .data$complexity_penalty +
        (regularization_cfg$stability %||% 0) * .data$stability_penalty +
        (regularization_cfg$edge %||% 0) * .data$edge_penalty,
      selection_objective = .data$fit_penalty +
        .data$complexity_penalty +
        (regularization_cfg$stability %||% 0) * .data$stability_penalty +
        (regularization_cfg$edge %||% 0) * .data$edge_penalty
    )
  best_cfg <- eligible_cfg |>
    # Once candidates are inside the one-standard-error pool, prefer interior,
    # stable settings over edge-hugging minima unless they are materially better.
    dplyr::arrange(.data$selection_objective, .data$edge_penalty, .data$complexity_penalty, .data$stability_penalty, .data$rmse, .data$mae, dplyr::desc(.data$n_eval), .data$alpha_deviation, .data$kernel_scale_deviation, .data$coherence_scale_deviation, .data$kernel_scale) |>
    dplyr::slice(1)
  response_surface <- eligible_cfg |>
    dplyr::arrange(.data$selection_objective, .data$edge_penalty, .data$complexity_penalty, .data$stability_penalty, .data$rmse, .data$mae, dplyr::desc(.data$n_eval), .data$alpha_deviation, .data$kernel_scale_deviation, .data$coherence_scale_deviation, .data$kernel_scale)
  top_candidates <- response_surface |>
    dplyr::slice_head(n = base_sim$config$response_surface_top_n %||% 20L)

  boundary_summary <- tibble::tibble(
    search_round = 1L,
    alpha_lower = abs(best_cfg$alpha[[1]] - alpha_bounds[[1]]) < 1e-8,
    alpha_upper = abs(best_cfg$alpha[[1]] - alpha_bounds[[2]]) < 1e-8,
    kernel_scale_lower = abs(best_cfg$kernel_scale[[1]] - scale_bounds[[1]]) < 1e-8,
    kernel_scale_upper = abs(best_cfg$kernel_scale[[1]] - scale_bounds[[2]]) < 1e-8,
    coherence_scale_lower = abs((best_cfg$coherence_scale[[1]] %||% 1) - coherence_block$bounds[[1]]) < 1e-8,
    coherence_scale_upper = abs((best_cfg$coherence_scale[[1]] %||% 1) - coherence_block$bounds[[2]]) < 1e-8,
    alpha_lower_bound = alpha_bounds[[1]],
    alpha_upper_bound = alpha_bounds[[2]],
    kernel_scale_lower_bound = scale_bounds[[1]],
    kernel_scale_upper_bound = scale_bounds[[2]],
    coherence_scale_lower_bound = coherence_block$bounds[[1]],
    coherence_scale_upper_bound = coherence_block$bounds[[2]],
    coherence_scale = best_cfg$coherence_scale[[1]] %||% 1,
    length_weight = best_cfg$length_weight[[1]],
    depth_weight = best_cfg$depth_weight[[1]],
    frequency_weight = best_cfg$frequency_weight[[1]],
    regularized_objective = best_cfg$regularized_objective[[1]],
    selection_objective = best_cfg$selection_objective[[1]],
    edge_penalty = best_cfg$edge_penalty[[1]]
  )

  hit_any_boundary <- abs(best_cfg$alpha[[1]] - alpha_bounds[[1]]) < 1e-8 ||
    abs(best_cfg$alpha[[1]] - alpha_bounds[[2]]) < 1e-8 ||
    abs(best_cfg$kernel_scale[[1]] - scale_bounds[[1]]) < 1e-8 ||
    abs(best_cfg$kernel_scale[[1]] - scale_bounds[[2]]) < 1e-8
  if (isTRUE(coherence_block$enabled)) {
    hit_any_boundary <- hit_any_boundary ||
      abs((best_cfg$coherence_scale[[1]] %||% 1) - coherence_block$bounds[[1]]) < 1e-8 ||
      abs((best_cfg$coherence_scale[[1]] %||% 1) - coherence_block$bounds[[2]]) < 1e-8
  }
  if (hit_any_boundary) {
    report_progress(
      progress,
      "Similarity tuning optimum landed on a configured boundary. Widen that parameter range explicitly if the edge value is scientifically defensible."
    )
  }

  report_progress(
    progress,
    "Similarity tuning search complete. Best alpha = ",
    signif(best_cfg$alpha[[1]], 4),
    ", kernel scale = ",
    signif(best_cfg$kernel_scale[[1]], 4),
    ", length weight = ",
    signif(best_cfg$length_weight[[1]], 4),
    ", depth weight = ",
    signif(best_cfg$depth_weight[[1]], 4),
    ", frequency weight = ",
    signif(best_cfg$frequency_weight[[1]], 4),
    ", RMSE = ",
    signif(best_cfg$rmse[[1]], 4),
    ". Candidate chosen by one-standard-error rule."
  )

  list(
    baseline = base_score,
    grid_scores = grid_scores,
    response_surface = response_surface,
    top_candidates = top_candidates,
    alpha_best = best_cfg$alpha[[1]],
    kernel_scale_best = best_cfg$kernel_scale[[1]],
    k_species_best = best_cfg$k_species[[1]],
    k_study_best = best_cfg$k_study[[1]],
    length_weight_best = best_cfg$length_weight[[1]],
    depth_weight_best = best_cfg$depth_weight[[1]],
    frequency_weight_best = best_cfg$frequency_weight[[1]],
    boundary_summary = boundary_summary
  )
}

#' Run one-at-a-time component drop-out scans
#'
#' @param tune_models Tuning subset returned by `build_tuning_subset()`.
#' @param base_sim Prepared similarity object returned by
#'   [prepare_similarities()].
#' @param alpha_best Tuned alpha value.
#' @param k_species_best Tuned species-kernel value.
#' @param k_study_best Tuned study-kernel value.
#' @param registry_path Optional path to the trait-registry JSON.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
run_component_dropout <- function(tune_models,
                                  base_sim,
                                  alpha_best,
                                  k_species_best,
                                  k_study_best,
                                  registry_path,
                                  n_cores = 1L) {
  base_basis <- prepare_similarity_score_basis(
    models_subset = tune_models,
    species_weights = base_sim$species_weights,
    study_weights = base_sim$study_weights,
    alpha_now = alpha_best,
    k_species_now = k_species_best,
    k_study_now = k_study_best,
    cfg_now = base_sim$config,
    registry_path = registry_path,
    seed_now = base_sim$seed
  )

  # Start from the full tuned alpha/k setting, then drop one component at a
  # time to quantify how much performance degrades without it.
  full_score <- score_similarity_basis(
    score_basis = base_basis,
    alpha_now = alpha_best,
    k_species_now = k_species_best,
    k_study_now = k_study_best
  ) |>
    dplyr::mutate(
      component = "full_model",
      component_type = "full_model",
      alpha = alpha_best,
      k_species = k_species_best,
      k_study = k_study_best
    )

  dropout_rows <- list(full_score)

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

  task_rows <- score_tasks_parallel(
    tasks = tasks,
    worker_name = "score_dropout_task",
    workers = n_cores,
    shared_data = list(
      tune_models = tune_models,
      base_sim = base_sim,
      alpha_best = alpha_best,
      k_species_best = k_species_best,
      k_study_best = k_study_best,
      registry_path = registry_path
    ),
    warning_text = "Parallel component tuning failed and is falling back to serial evaluation."
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
      delta_rmse = .data$rmse - full_rmse,
      delta_mae = .data$mae - full_mae
    )
}

#' Apply component-impact multipliers
#'
#' @param base_sim Prepared similarity object returned by
#'   [prepare_similarities()].
#' @param component_impact_summary Component-impact summary returned by
#'   `run_component_dropout()`.
#'
#' @return A list with tuned species weights, study weights, and coherence
#'   config.
#'
#' @keywords internal
#' @noRd
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
    dplyr::filter(.data$component != "full_model") |>
    dplyr::transmute(
      .data$component,
      multiplier = dplyr::if_else(
        .data$delta_rmse > 0,
        0.5 + 1.5 * (.data$delta_rmse / max_delta),
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
    range_now <- suppressWarnings(as.numeric(cfg_tuned[[nm]]$range %||% numeric(0)))
    range_now <- range_now[is.finite(range_now) & range_now >= 0]
    if (length(range_now) >= 2 && is.finite(cfg_tuned[[nm]]$weight)) {
      cfg_tuned[[nm]]$weight <- min(max(cfg_tuned[[nm]]$weight, min(range_now)), max(range_now))
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
#' @param tune_obj Result returned by [tune_similarities()].
#' @param resample_id Integer resample identifier.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
collect_component_weights <- function(tune_obj,
                                      resample_id) {
  # Pull the drop-out deltas once so every returned component row carries the
  # same empirical performance context used during tuning.
  impact_tbl <- tibble::as_tibble(tune_obj$component_impact_summary)
  if (!"component" %in% names(impact_tbl)) {
    impact_tbl$component <- character(nrow(impact_tbl))
  }
  if (!"delta_rmse" %in% names(impact_tbl)) {
    impact_tbl$delta_rmse <- NA_real_
  }
  if (!"delta_mae" %in% names(impact_tbl)) {
    impact_tbl$delta_mae <- NA_real_
  }

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
        is.finite(.data$base_weight) & .data$base_weight != 0,
        .data$tuned_weight / .data$base_weight,
        NA_real_
      )
    ) |>
    dplyr::bind_rows(coherence_tbl) |>
    dplyr::left_join(
      impact_tbl |>
        dplyr::select("component", "delta_rmse", "delta_mae"),
      by = "component"
    ) |>
    dplyr::mutate(resample_id = resample_id, .before = 1)
}

#' Summarize tuning stability across resamples
#'
#' @param tune_obj Result returned by [tune_similarities()].
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
summarize_tuning_stability <- function(tune_obj) {
  resample_tbl <- tibble::as_tibble(tune_obj$resample_summary %||% tibble::tibble())
  component_tbl <- tibble::as_tibble(tune_obj$component_weights %||% tibble::tibble())

  for (field_name in c("alpha", "k_species", "k_study", "rmse", "mae")) {
    if (!field_name %in% names(resample_tbl)) {
      resample_tbl[[field_name]] <- NA_real_
    }
  }
  if (!"kernel_scale" %in% names(resample_tbl)) {
    resample_tbl$kernel_scale <- dplyr::coalesce(resample_tbl$k_species, resample_tbl$k_study)
  }

  if (nrow(resample_tbl) == 0) {
    resample_tbl <- tibble::tibble(
      alpha = as.numeric(tune_obj$config_tuned$alpha %||% NA_real_),
      kernel_scale = as.numeric(tune_obj$config_tuned$kernel_scale %||% tune_obj$config_tuned$k_species %||% tune_obj$config_tuned$k_study %||% NA_real_),
      k_species = as.numeric(tune_obj$config_tuned$k_species %||% NA_real_),
      k_study = as.numeric(tune_obj$config_tuned$k_study %||% NA_real_),
      rmse = NA_real_,
      mae = NA_real_
    )
  }

  hyperparameter_summary <- tibble::tibble(
    component = c("alpha", "kernel_scale", "k_species", "k_study"),
    component_type = "hyperparameter",
    median_value = c(
      stats::median(resample_tbl$alpha, na.rm = TRUE),
      stats::median(resample_tbl$kernel_scale, na.rm = TRUE),
      stats::median(resample_tbl$k_species, na.rm = TRUE),
      stats::median(resample_tbl$k_study, na.rm = TRUE)
    ),
    lower_value = c(
      stats::quantile(resample_tbl$alpha, probs = 0.25, na.rm = TRUE, names = FALSE),
      stats::quantile(resample_tbl$kernel_scale, probs = 0.25, na.rm = TRUE, names = FALSE),
      stats::quantile(resample_tbl$k_species, probs = 0.25, na.rm = TRUE, names = FALSE),
      stats::quantile(resample_tbl$k_study, probs = 0.25, na.rm = TRUE, names = FALSE)
    ),
    upper_value = c(
      stats::quantile(resample_tbl$alpha, probs = 0.75, na.rm = TRUE, names = FALSE),
      stats::quantile(resample_tbl$kernel_scale, probs = 0.75, na.rm = TRUE, names = FALSE),
      stats::quantile(resample_tbl$k_species, probs = 0.75, na.rm = TRUE, names = FALSE),
      stats::quantile(resample_tbl$k_study, probs = 0.75, na.rm = TRUE, names = FALSE)
    ),
    n_resamples = nrow(resample_tbl)
  )

  if (nrow(component_tbl) == 0) {
    return(hyperparameter_summary)
  }

  weight_summary <- component_tbl |>
    dplyr::group_by(.data$component, .data$component_type) |>
    dplyr::summarise(
      median_value = stats::median(.data$tuned_weight, na.rm = TRUE),
      lower_value = stats::quantile(.data$tuned_weight, probs = 0.25, na.rm = TRUE, names = FALSE),
      upper_value = stats::quantile(.data$tuned_weight, probs = 0.75, na.rm = TRUE, names = FALSE),
      n_resamples = dplyr::n_distinct(.data$resample_id),
      .groups = "drop"
    )

  dplyr::bind_rows(hyperparameter_summary, weight_summary)
}

#' Summarize final tuning error by meaningful anchor strata
#'
#' @param anchor_rows Anchor-level error rows returned from
#'   `score_similarity_basis()`.
#' @param n_support_bins Number of effective-support bins.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
summarize_similarity_tuning_strata <- function(anchor_rows,
                                               n_support_bins = 4L) {
  anchor_rows <- tibble::as_tibble(anchor_rows %||% tibble::tibble())
  n_support_bins <- suppressWarnings(as.integer(n_support_bins %||% 4L))
  if (!is.finite(n_support_bins) || n_support_bins < 1L) {
    n_support_bins <- 4L
  }
  if (nrow(anchor_rows) == 0) {
    return(tibble::tibble())
  }

  anchor_rows$species_name <- stringr::str_squish(as.character(anchor_rows$species_name %||% NA_character_))
  anchor_rows$species_name[!nzchar(anchor_rows$species_name)] <- NA_character_

  support_source <- suppressWarnings(as.numeric(anchor_rows$effective_support %||% NA_real_))
  support_source[!is.finite(support_source)] <- NA_real_
  support_bins <- rep("Unknown support", length(support_source))
  valid_support <- is.finite(support_source)
  if (sum(valid_support) > 0) {
    support_rank <- dplyr::ntile(support_source[valid_support], n = min(n_support_bins, sum(valid_support)))
    support_labels <- paste("Support bin", support_rank)
    support_bins[valid_support] <- support_labels
  }
  anchor_rows$support_bin <- factor(
    support_bins,
    levels = c(paste("Support bin", seq_len(max(1L, min(n_support_bins, sum(valid_support))))), "Unknown support")
  )

  summarize_block <- function(data_now, group_col, stratum_type) {
    data_now |>
      dplyr::filter(!is.na(.data[[group_col]])) |>
      dplyr::group_by(.data[[group_col]]) |>
      dplyr::summarise(
        stratum_type = stratum_type,
        stratum = as.character(dplyr::first(.data[[group_col]])),
        n_anchor = dplyr::n(),
        rmse = sqrt(mean(.data$sq_error, na.rm = TRUE)),
        mae = mean(.data$abs_error, na.rm = TRUE),
        median_abs_error = stats::median(.data$abs_error, na.rm = TRUE),
        mean_effective_support = mean(.data$effective_support, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::select("stratum_type", "stratum", "n_anchor", "rmse", "mae", "median_abs_error", "mean_effective_support")
  }

  dplyr::bind_rows(
    summarize_block(anchor_rows, "species_name", "species"),
    summarize_block(anchor_rows, "support_bin", "support_bin")
  )
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
#'   [prepare_similarities()] for the accepted forms. When `NULL`, a
#'   config-supplied value is used when present.
#' @param study_traits Optional study-trait specification. See
#'   [prepare_similarities()] for the accepted forms. When `NULL`, a
#'   config-supplied value is used when present.
#' @param alpha Optional starting species-versus-study mixing parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param k_species Optional starting species-distance kernel parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param k_study Optional starting study-distance kernel parameter. When
#'   `NULL`, a config-supplied value is used when present.
#' @param max_models_per_species Maximum number of retained tuning models per
#'   species. When `NULL`, a config-supplied value is used when present.
#' @param n_resamples Optional number of resampled tuning subsets.
#' @param seed Optional integer seed. When `NULL`, a config-supplied value is
#'   used when present; otherwise one is generated and returned in the output
#'   object.
#' @param config Optional JSON path or list with similarity options. Supported
#'   entries are `species_traits`, `study_traits`, `alpha`, `k_species`,
#'   `k_study`, `max_models_per_species`, `seed`, `length_coherence`,
#'   `depth_coherence`, `frequency_coherence`, `alpha_grid`,
#'   `k_species_grid`, `k_study_grid`, and `grid_refinement_levels`. A
#'   [Configurer] object is also accepted.
#' @param cache_path Optional path to an `.rds` cache file.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache file.
#' @param progress Logical scalar controlling stage messages.
#' @param registry_path Optional path to a trait-registry JSON file.
#'
#' @return When `candidate_models` is a [Candidates] object, returns that
#'   object with the tuning result attached. Otherwise,
#'   returns a list containing the tuned configuration, tuning subset, score
#'   history, and per-trait component-impact summary.
#'
#' @examples
#' \dontrun{
#' candidates <- build_candidates(list(
#'   study = list(path = "input.xlsx"),
#'   anchors = list(selector = list(regional_body = "SWFSC"))
#' ))
#'
#' cfg_data <- build_configurer(list(
#'   paths = list(
#'     input_file = "input.xlsx",
#'     out_root = "outputs",
#'     cache_dir = "cache"
#'   ),
#'   execution = list(),
#'   tuning = list(
#'     max_models_per_species = 2L,
#'     n_resamples = 8L,
#'     grid_refinement_levels = 1L
#'   ),
#'   policy = list(
#'     alpha = 0.8,
#'     k_species = 4,
#'     k_study = 2,
#'     frequency_coherence_mode = "overlap",
#'     length_overlap_weight = 2,
#'     depth_overlap_weight = 2,
#'     frequency_coherence_weight = 1,
#'     species_traits = list(genus = 1, family = 1),
#'     study_traits = list(frequency = 1, fao_area = 1)
#'   ),
#'   policies = list(active = "closest_within_species")
#' ))
#'
#' tune_obj <- tune_similarities(
#'   candidate_models = candidates,
#'   config = cfg_data
#' )
#' tune_obj
#' }
#'
#' @export
tune_similarities <- function(candidate_models,
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
                              refresh = NULL,
                              progress = NULL,
                              registry_path = NULL) {
  apply_tuned_similarity_state <- function(candidates_obj,
                                           tuning_result) {
    tuned_cfg <- tuning_result$config_tuned %||% list()
    if (!is.list(tuned_cfg) || length(tuned_cfg) == 0) {
      return(candidates_with_similarity_tuning(candidates_obj, tuning_result))
    }

    prepared_candidates <- prepare_similarities(
      candidate_models = candidates_obj,
      species_traits = as.list(tuned_cfg$species_weights %||% list()),
      study_traits = as.list(tuned_cfg$study_weights %||% list()),
      alpha = tuned_cfg$alpha %||% NULL,
      k_species = tuned_cfg$kernel_scale %||% tuned_cfg$k_species %||% NULL,
      k_study = tuned_cfg$kernel_scale %||% tuned_cfg$k_study %||% NULL,
      config = tuned_cfg$coherence %||% list(),
      registry_path = registry_path,
      seed = tuning_result$seed %||% NULL,
      progress = FALSE
    )

    if (!is_s7_instance(prepared_candidates, "Candidates")) {
      return(candidates_with_similarity_tuning(candidates_obj, tuning_result))
    }

    candidates_with_similarity_tuning(prepared_candidates, tuning_result)
  }

  # Preserve the prepared `Candidates` object boundary when present so the
  # tuning result can be written back onto that object instead of returned as
  # a detached sidecar list.
  candidates_obj <- if (is_s7_instance(candidate_models, "Candidates")) {
    candidate_models
  } else {
    NULL
  }
  config <- resolve_similarity_config_source(candidate_models, config)
  candidate_models <- if (is_s7_instance(candidate_models, "Candidates")) {
    candidate_models@candidate_models
  } else {
    candidate_models
  }

  # Resolve the optional tuning config once so direct args can override it
  # consistently.
  cfg_user <- read_similarity_config(config)
  cache_path <- cache_path %||% cfg_user$cache_path
  refresh <- refresh %||% cfg_user$refresh %||% FALSE
  progress <- progress %||% cfg_user$progress %||% FALSE
  if (!is.null(cache_path) &&
    (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }
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
    report_progress(progress, "Loading cached similarity tuning from ", cache_path, ".")
    cached_result <- readRDS(cache_path)
    if (!isTRUE(similarity_tuning_cache_current(cached_result))) {
      report_progress(progress, "Cached similarity tuning is stale and will be rebuilt.")
    } else {
      if (!is.null(candidates_obj)) {
        return(apply_tuned_similarity_state(candidates_obj, cached_result))
      }
      return(cached_result)
    }
  }

  # Prepare the baseline similarity inputs once against the full candidate
  # table so the trait set, starting weights, and search grid are fixed before
  # subset selection begins. Unspecified traits already default to weight `1`
  # inside `prepare_similarities()`, so there is no separate equalization
  # pass here that would overwrite explicit starting weights.
  report_progress(progress, "Tuning similarity settings.")

  base_sim <- prepare_similarities(
    candidate_models = candidate_models,
    species_traits = species_traits,
    study_traits = study_traits,
    alpha = alpha,
    k_species = k_species,
    k_study = k_study,
    config = config,
    registry_path = registry_path,
    seed = seed,
    progress = FALSE
  )
  if (isTRUE(cfg_user$equal_start_weights)) {
    if (length(base_sim$species_weights %||% numeric(0)) > 0) {
      base_sim$species_weights <- equal_start_weights(base_sim$species_weights)
    }
    if (length(base_sim$study_weights %||% numeric(0)) > 0) {
      base_sim$study_weights <- equal_start_weights(base_sim$study_weights)
    }
  }

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
  report_progress(
    progress,
    "Similarity tuning subset: ",
    nrow(tune_models),
    " model(s) across ",
    dplyr::n_distinct(tune_models$species_name),
    " species."
  )

  # Tune the alpha/kernel scalars first with fixed starting trait weights.
  grid_obj <- run_tuning_grid_search(
    tune_models = tune_models,
    base_sim = base_sim,
    registry_path = registry_path,
    n_cores = n_cores,
    progress = progress
  )
  grid_tuned_sim <- base_sim
  grid_tuned_sim$config$length_coherence$weight <- grid_obj$length_weight_best
  grid_tuned_sim$config$depth_coherence$weight <- grid_obj$depth_weight_best
  grid_tuned_sim$config$frequency_coherence$weight <- grid_obj$frequency_weight_best

  # Quantify component importance by dropping each selected trait and enabled
  # coherence term one at a time under the best alpha/kernel setting.
  report_progress(progress, "Running similarity component drop-out scan.")
  component_impact_summary <- run_component_dropout(
    tune_models = tune_models,
    base_sim = grid_tuned_sim,
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
  component_weights_tbl <- tibble::tibble()
  if (isTRUE(use_resample_tuning) && n_resamples > 1L) {
    report_progress(
      progress,
      "Running ",
      n_resamples,
      " similarity tuning resample(s) for stability."
    )
    set.seed(base_sim$seed)
    resample_seeds <- sample.int(.Machine$integer.max, n_resamples)
    tuned_rows <- vector("list", n_resamples)
    summary_rows <- vector("list", n_resamples)

    for (i in seq_len(n_resamples)) {
      report_progress(progress, "Similarity tuning resample ", i, "/", n_resamples, ".")
      sampled_subset <- build_resample_subset(
        candidate_models = base_sim$candidate_models,
        species_weights = base_sim$species_weights,
        study_weights = base_sim$study_weights,
        max_models_per_species = max_models_per_species,
        seed = resample_seeds[[i]]
      )

      comp_now <- run_component_dropout(
        tune_models = sampled_subset,
        base_sim = grid_tuned_sim,
        alpha_best = grid_obj$alpha_best,
        k_species_best = grid_obj$k_species_best,
        k_study_best = grid_obj$k_study_best,
        registry_path = registry_path,
        n_cores = n_cores
      )

      tuned_now <- apply_component_weights(
        base_sim = grid_tuned_sim,
        component_impact_summary = comp_now
      )

      full_now <- comp_now |>
        dplyr::filter(.data$component == "full_model") |>
        dplyr::slice_tail(n = 1)

      summary_rows[[i]] <- tibble::tibble(
        resample_id = i,
        seed = resample_seeds[[i]],
        n_models = nrow(sampled_subset),
        n_species = dplyr::n_distinct(sampled_subset$species_name),
        alpha = grid_obj$alpha_best,
        kernel_scale = grid_obj$kernel_scale_best,
        k_species = grid_obj$k_species_best,
        k_study = grid_obj$k_study_best,
        length_weight = grid_obj$length_weight_best,
        depth_weight = grid_obj$depth_weight_best,
        frequency_weight = grid_obj$frequency_weight_best,
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
    component_weights_tbl <- dplyr::bind_rows(
      lapply(seq_along(tuned_rows), function(i) {
        collect_component_weights(
          tune_obj = list(
            config_base = list(
              species_weights = base_sim$species_weights,
              study_weights = base_sim$study_weights,
              coherence = grid_tuned_sim$config
            ),
            config_tuned = list(
              species_weights = tuned_rows[[i]]$species_weights,
              study_weights = tuned_rows[[i]]$study_weights,
              coherence = tuned_rows[[i]]$coherence
            ),
            component_impact_summary = component_impact_summary
          ),
          resample_id = i
        )
      })
    )

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

    species_weights_resampled <- grid_tuned_sim$species_weights
    for (nm in names(species_weights_resampled)) {
      agg <- aggregate_weight(nm, "species")
      if (is.finite(agg)) {
        species_weights_resampled[[nm]] <- agg
      }
    }

    study_weights_resampled <- grid_tuned_sim$study_weights
    for (nm in names(study_weights_resampled)) {
      agg <- aggregate_weight(nm, "study")
      if (is.finite(agg)) {
        study_weights_resampled[[nm]] <- agg
      }
    }

    cfg_resampled <- grid_tuned_sim$config
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
      base_sim = grid_tuned_sim,
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
      kernel_scale = grid_obj$kernel_scale_best,
      k_species = grid_obj$k_species_best,
      k_study = grid_obj$k_study_best,
      length_weight = tuned_obj$config$length_coherence$weight %||% grid_obj$length_weight_best,
      depth_weight = tuned_obj$config$depth_coherence$weight %||% grid_obj$depth_weight_best,
      frequency_weight = tuned_obj$config$frequency_coherence$weight %||% grid_obj$frequency_weight_best
    )
  final_anchor_rows <- attr(
    score_similarity_basis(
      score_basis = prepare_similarity_score_basis(
        models_subset = tune_models,
        species_weights = tuned_obj$species_weights,
        study_weights = tuned_obj$study_weights,
        alpha_now = grid_obj$alpha_best,
        k_species_now = grid_obj$k_species_best,
        k_study_now = grid_obj$k_study_best,
        cfg_now = tuned_obj$config,
        registry_path = registry_path,
        seed_now = base_sim$seed
      ),
      alpha_now = grid_obj$alpha_best,
      k_species_now = grid_obj$k_species_best,
      k_study_now = grid_obj$k_study_best,
      return_anchor_rows = TRUE
    ),
    "anchor_rows"
  )

  # Return both the starting and tuned configurations alongside the score
  # history needed to inspect how the empirical tuning behaved.
  result <- list(
    config_base = list(
      species_weights = base_sim$species_weights,
      study_weights = base_sim$study_weights,
      alpha = base_sim$alpha,
      kernel_scale = kernel_scale_center(base_sim$k_species, base_sim$k_study),
      k_species = base_sim$k_species,
      k_study = base_sim$k_study,
      coherence = base_sim$config
    ),
    config_tuned = list(
      species_weights = tuned_obj$species_weights,
      study_weights = tuned_obj$study_weights,
      alpha = grid_obj$alpha_best,
      kernel_scale = grid_obj$kernel_scale_best,
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
    response_surface = grid_obj$response_surface,
    top_candidates = grid_obj$top_candidates,
    component_impact_summary = component_impact_summary,
    component_weights = if (nrow(component_weights_tbl) > 0) {
      component_weights_tbl
    } else {
      collect_component_weights(
        tune_obj = list(
          config_base = list(
            species_weights = base_sim$species_weights,
            study_weights = base_sim$study_weights,
            coherence = grid_tuned_sim$config
          ),
          config_tuned = list(
            species_weights = tuned_obj$species_weights,
            study_weights = tuned_obj$study_weights,
            coherence = tuned_obj$config
          ),
          component_impact_summary = component_impact_summary
        ),
        resample_id = 1L
      )
    },
    resample_summary = resample_summary,
    n_resamples = if (isTRUE(use_resample_tuning)) n_resamples else NA_integer_,
    seed = base_sim$seed,
    max_models_per_species = max_models_per_species,
    tuning_version = similarity_tuning_cache_version(),
    boundary_summary = grid_obj$boundary_summary,
    anchor_validation = final_anchor_rows
  )
  result$stability_summary <- summarize_tuning_stability(result)
  result$strata_validation <- summarize_similarity_tuning_strata(
    anchor_rows = final_anchor_rows,
    n_support_bins = base_sim$config$support_strata_bins %||% 4L
  )
  report_progress(
    progress,
    "Finished similarity tuning. alpha=",
    signif(grid_obj$alpha_best, 4),
    ", kernel_scale=",
    signif(grid_obj$kernel_scale_best, 4),
    "."
  )

  # Persist the tuned object for reuse when a cache path was requested.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  if (!is.null(candidates_obj)) {
    return(apply_tuned_similarity_state(candidates_obj, result))
  }

  result
}

#' Refit similarity tuning across resampled tuning subsets
#'
#' Internal helper that reruns [tune_similarities()] across multiple
#' resampled per-species tuning subsets to summarize tuning stability.
#'
#' @param candidate_models Prepared candidate-model table or a [Candidates]
#'   object.
#' @param species_traits Optional species-trait specification.
#' @param study_traits Optional study-trait specification.
#' @param alpha Optional starting species-versus-study mixing parameter.
#' @param k_species Optional starting species-distance kernel parameter.
#' @param k_study Optional starting study-distance kernel parameter.
#' @param n_resamples Number of resampled tuning subsets.
#' @param max_models_per_species Maximum number of retained models per species
#'   within each resampled tuning subset.
#' @param seed Optional integer seed.
#' @param config Optional JSON path or list with similarity options.
#' @param cache_path Optional path to an `.rds` cache file.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache file.
#' @param registry_path Optional path to a trait-registry JSON file.
#'
#' @return A list containing the resampled tuning subsets, resample summaries,
#'   and per-component tuned multipliers across resamples.
#'
#' @keywords internal
#' @noRd
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
  candidate_models <- if (is_s7_instance(candidate_models, "Candidates")) {
    candidate_models@candidate_models
  } else {
    candidate_models
  }

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
  base_sim <- prepare_similarities(
    candidate_models = candidate_models,
    species_traits = species_traits,
    study_traits = study_traits,
    alpha = alpha,
    k_species = k_species,
    k_study = k_study,
    config = config,
    registry_path = registry_path,
    seed = seed,
    progress = FALSE
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

    tune_obj <- tune_similarities(
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
      dplyr::filter(.data$stage == "final_tuned") |>
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
