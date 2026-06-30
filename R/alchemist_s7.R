#' Learned-Distance Alchemist S7 Class
#'
#' `Alchemist` replaces the Gower-distance tuning pipeline with a supervised
#' metric-learning approach. A Super Learner is trained to predict pairwise
#' acoustic distances (log-sigma_bs differences integrated over the receiving
#' anchor's length distribution) from per-trait Gower distance features. The
#' resulting N x N learned distance matrix is the direct input to ordination,
#' admissibility kernel weighting, and policy selection strategies.
#'
#' Construction requires a fully ingested [Candidates] object. Configuration
#' is either inherited from the `Candidates` config (alchemist or
#' similarity section) or supplied directly as a list or YAML path.
#'
#' @examples
#' \dontrun{
#' alchemist <- as_alchemist(candidates)
#' alchemist <- forge_distances(alchemist)
#' alchemist <- distill_traits(alchemist)
#' alchemist <- run_ordination(alchemist)
#' alchemist <- screen_admissibility(alchemist)
#' selector <- as_selector(alchemist)
#' }
#'
#' @name Alchemist-class
#' @usage NULL
#' @aliases Alchemist
NULL

# - Internal predicates -

.is_alchemist <- function(x) {
  inherits(x, "S7_object") &&
    exists("Alchemist", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(x, Alchemist), error = function(e) FALSE))
}

.is_candidates_obj <- function(x) {
  inherits(x, "S7_object") &&
    exists("Candidates", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(x, Candidates), error = function(e) FALSE))
}

# - Config normalization -

alchemist_config_from_config <- function(source) {
  cfg <- if (.is_candidates_obj(source)) {
    candidates_configuration(source) %||% list()
  } else if (inherits(source, "S7_object") &&
    exists("Configurer", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(source, Configurer), error = function(e) FALSE))) {
    source@data
  } else if (is.list(source)) {
    source
  } else {
    list()
  }

  alch <- cfg$alchemist %||% list()
  sim <- cfg$similarity %||% list()
  ml <- cfg$metalearner %||% list()

  list(
    species_traits = alch$species_traits %||% sim$species_traits %||% list(),
    study_traits = alch$study_traits %||% sim$study_traits %||% list(),
    coherence = alch$coherence %||% sim$coherence %||% list(),
    taxonomic_distance = alch$taxonomic_distance %||% FALSE,
    feature_type = alch$feature_type %||% "gower",
    learner = list(
      methods = alch$learner$methods %||% ml$selection_super_methods %||% NULL,
      inner_folds = as.integer(alch$learner$inner_folds %||% ml$inner_folds %||% 5L),
      seed = if (!is.null(alch$learner$seed %||% ml$seed)) {
        as.integer(alch$learner$seed %||% ml$seed)
      } else {
        NULL
      },
      outcome_transform = alch$learner$outcome_transform %||% ml$outcome_transform %||% "identity",
      lambda_rule = alch$learner$lambda_rule %||% ml$lambda_rule %||% "min",
      oof_mode = alch$learner$oof_mode %||% "anchor_species",
      # Preserve the shared learner-family settings and let any Alchemist-
      # specific overrides replace only the fields they explicitly set.
      method_settings = merge_cfg(
        ml$method_settings %||% list(),
        alch$learner$method_settings %||% list()
      ),
      workers = as.integer(alch$learner$workers %||% ml$workers %||% 1L)
    ),
    distill_workers = as.integer(alch$distill_workers %||% 1L),
    registry_path = alch$registry_path %||% cfg$registry_path %||% NULL,
    progress = alch$progress %||% FALSE,
    config_data = cfg
  )
}

normalize_alchemist_config <- function(config, candidates = NULL) {
  if (is.null(config)) {
    config <- if (!is.null(candidates)) {
      alchemist_config_from_config(candidates)
    } else {
      list()
    }
  } else if (.is_candidates_obj(config) ||
    (inherits(config, "S7_object") &&
      exists("Configurer", inherits = TRUE) &&
      isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
    config <- alchemist_config_from_config(config)
  } else if (is.list(config) &&
    any(c("alchemist", "similarity", "metalearner", "policy", "policies") %in% names(config))) {
    config <- alchemist_config_from_config(config)
  } else if (is.character(config) && length(config) == 1 && file.exists(config)) {
    raw <- yaml::read_yaml(config)
    cfg_obj <- tryCatch(as_configurer(raw), error = function(e) NULL)
    config <- if (!is.null(cfg_obj)) alchemist_config_from_config(cfg_obj) else raw
  } else if (!is.list(config)) {
    stop(
      "'config' must be NULL, a list, a YAML path, a `Candidates`, or a `Configurer`.",
      call. = FALSE
    )
  }

  if (is.null(config$learner)) config$learner <- list()
  config$learner$inner_folds <- as.integer(config$learner$inner_folds %||% 5L)
  config$learner$seed <- if (!is.null(config$learner$seed)) as.integer(config$learner$seed) else NULL
  config$learner$outcome_transform <- config$learner$outcome_transform %||% "identity"
  config$learner$lambda_rule <- config$learner$lambda_rule %||% "min"
  config$learner$oof_mode <- config$learner$oof_mode %||% "anchor_species"
  config$learner$workers <- as.integer(config$learner$workers %||% 1L)
  config$distill_workers <- as.integer(config$distill_workers %||% 1L)
  config$feature_type <- config$feature_type %||% "gower"

  config
}

# - Class definition -

#' @export
Alchemist <- S7::new_class(
  "Alchemist",
  properties = list(
    candidates = S7::new_property(S7::class_any),
    config = S7::new_property(S7::class_list),
    learner = S7::new_property(S7::class_list),
    distance_matrix = S7::new_property(S7::class_list),
    trait_importance = S7::new_property(S7::class_list),
    ordination = S7::new_property(S7::class_list),
    admissibility = S7::new_property(S7::class_list)
  ),
  validator = function(self) {
    tryCatch(
      {
        if (!.is_candidates_obj(self@candidates)) {
          return("`candidates` must be a `Candidates` object.")
        }
        if (!is.list(self@config)) {
          return("`config` must be a list.")
        }
        if (!is.list(self@learner)) {
          return("`learner` must be a list.")
        }
        if (!is.list(self@distance_matrix)) {
          return("`distance_matrix` must be a list.")
        }
        if (!is.list(self@trait_importance)) {
          return("`trait_importance` must be a list.")
        }
        if (!is.list(self@ordination)) {
          return("`ordination` must be a list.")
        }
        if (!is.list(self@admissibility)) {
          return("`admissibility` must be a list.")
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(Alchemist)

# - Rebuild helpers -

alchemist_rebuild <- function(object,
                              candidates = object@candidates,
                              config = object@config,
                              learner = object@learner,
                              distance_matrix = object@distance_matrix,
                              trait_importance = object@trait_importance,
                              ordination = object@ordination,
                              admissibility = object@admissibility) {
  Alchemist(
    candidates = candidates,
    config = config,
    learner = learner,
    distance_matrix = distance_matrix,
    trait_importance = trait_importance,
    ordination = ordination,
    admissibility = admissibility
  )
}

# - Constructor -

#' Build an Alchemist from a Candidates object
#'
#' @param candidates A [Candidates] object. Must have `@candidate_models`
#'   populated; `@similarity_matrix` is used to inherit trait names when no
#'   explicit config is supplied.
#' @param config Optional alchemist config list, YAML path, or [Configurer]
#'   object. When `NULL`, trait names and learner settings are inherited from
#'   `candidates`.
#' @param ... Unused.
#'
#' @return An [Alchemist] object.
#'
#' @export
as_alchemist <- function(candidates, config = NULL, ...) {
  if (.is_alchemist(candidates)) {
    return(candidates)
  }

  if (!.is_candidates_obj(candidates)) {
    stop("'candidates' must be a `Candidates` object.", call. = FALSE)
  }

  config <- normalize_alchemist_config(config, candidates = candidates)

  if (length(config$species_traits) == 0 && length(candidates@similarity_matrix) > 0) {
    config$species_traits <- (candidates@similarity_matrix)$species_traits %||% list()
  }
  if (length(config$study_traits) == 0 && length(candidates@similarity_matrix) > 0) {
    config$study_traits <- (candidates@similarity_matrix)$study_traits %||% list()
  }

  Alchemist(
    candidates = candidates,
    config = config,
    learner = list(),
    distance_matrix = list(),
    trait_importance = list(),
    ordination = list(),
    admissibility = list()
  )
}

# - Trait normalization and set-expansion helpers -

# Map raw ocean_basin strings to canonical lowercase basin codes.
# "North Atlantic Ocean", "Atlantic Ocean", "atlantic" all map to "atlantic".
# Values containing no recognized basin keyword become NA.
# Multiple basins are preserved as ";"-joined codes ("atlantic;pacific").
normalize_alchemist_ocean_basin <- function(x) {
  keywords <- c("atlantic", "pacific", "mediterranean", "indian", "southern", "arctic")
  x_lower <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  vapply(x_lower, function(raw) {
    if (is.na(raw) || !nzchar(raw)) {
      return(NA_character_)
    }
    parts <- stringr::str_trim(strsplit(raw, "[;,]")[[1L]])
    matched <- character(0)
    for (kw in keywords) {
      if (any(grepl(paste0("\\b", kw, "\\b"), parts))) matched <- c(matched, kw)
    }
    if (length(matched) > 0L) {
      paste(sort(unique(matched)), collapse = ";")
    } else {
      NA_character_
    }
  }, character(1), USE.NAMES = FALSE)
}

# Validate fao_area strings and keep only recognised FAO major fishing area
# codes, dropping garbage values like "0", "37.4", or "hatchery".
normalize_alchemist_fao_area <- function(x) {
  allowed <- c(
    "1", "2", "3", "4", "5", "6", "7", "8", "18", "21", "27", "31",
    "34", "37", "41", "47", "48", "51", "57", "58", "61", "67", "71",
    "77", "81", "87", "88"
  )
  x_sq <- stringr::str_squish(as.character(x))
  vapply(x_sq, function(raw) {
    if (is.na(raw) || !nzchar(raw)) {
      return(NA_character_)
    }
    parts <- stringr::str_trim(strsplit(raw, ";")[[1L]])
    nums <- suppressWarnings(as.integer(parts))
    valid <- as.character(nums[!is.na(nums) & as.character(nums) %in% allowed])
    if (length(valid) > 0L) {
      paste(sort(unique(valid)), collapse = ";")
    } else {
      NA_character_
    }
  }, character(1), USE.NAMES = FALSE)
}

# Compute pairwise distance matrices for one trait column.
# Numeric and single-value categoricals return one named matrix.
# Set-type columns (any value contains ";") are split and each unique value
# becomes its own binary 0/1 indicator matrix, using the double-underscore
# naming convention shared with expand_trait_block() in similarity.R.
expand_multival_col <- function(x, col_fn, tr) {
  x_chr <- as.character(x)
  if (!any(grepl(";", x_chr, fixed = TRUE), na.rm = TRUE)) {
    return(stats::setNames(list(col_fn(x)), paste0(".dist_", tr)))
  }
  vals_list <- strsplit(trimws(x_chr), "\\s*;\\s*")
  all_vals <- sort(unique(trimws(unlist(vals_list, use.names = FALSE))))
  all_vals <- all_vals[!is.na(all_vals) & nzchar(all_vals)]
  mats <- lapply(all_vals, function(v) {
    binary <- as.integer(
      vapply(vals_list, function(rv) v %in% trimws(rv), logical(1))
    )
    col_fn(binary)
  })
  stats::setNames(mats, paste0(".dist_", tr, "__", gsub("[^A-Za-z0-9]", "_", all_vals)))
}

# - Pair-level training data -

#' Normalized Gower distance for one trait column
#'
#' @keywords internal
gower_col <- function(x) {
  if (is.numeric(x)) {
    finite_x <- x[is.finite(x)]
    r <- if (length(finite_x) >= 2L) diff(range(finite_x)) else 1
    if (!is.finite(r) || r <= 0) r <- 1
    mat <- outer(x, x, function(a, b) abs(a - b) / r)
    mat[!is.finite(mat)] <- 0.5
    mat
  } else {
    xc <- as.character(x)
    xc[is.na(x) | !nzchar(xc)] <- NA_character_
    outer(xc, xc, function(a, b) {
      ifelse(is.na(a) | is.na(b), 0.5, ifelse(a == b, 0, 1))
    })
  }
}

# Signed standardized difference matrix for one trait column.
# Numeric traits: (a - b) / sd(x), with 0 imputed for non-finite pairs.
# Categorical traits: delegates to gower_col (0 = match, 1 = mismatch).
diff_col <- function(x) {
  if (is.numeric(x)) {
    s <- sd(x[is.finite(x)], na.rm = TRUE)
    if (!is.finite(s) || s <= 0) s <- 1
    mat <- outer(x, x, function(a, b) (a - b) / s)
    mat[!is.finite(mat)] <- 0
    mat
  } else {
    gower_col(x)
  }
}

# Squared standardized difference matrix for one trait column (Mahalanobis).
# Numeric traits: ((a - b) / sd(x))^2, with 0 imputed for non-finite pairs.
# Categorical traits: Gower agreement squared (0^2=0, 1^2=1, 0.5^2=0.25).
squared_col <- function(x) {
  if (is.numeric(x)) {
    s <- sd(x[is.finite(x)], na.rm = TRUE)
    if (!is.finite(s) || s <= 0) s <- 1
    mat <- outer(x, x, function(a, b) ((a - b) / s)^2)
    mat[!is.finite(mat)] <- 0
    mat
  } else {
    gower_col(x)^2
  }
}

#' Build length PDF for one model row (uniform approximation)
#'
#' @keywords internal
build_anchor_pdf <- function(lo, hi, n = 400L) {
  lo <- suppressWarnings(as.numeric(lo))
  hi <- suppressWarnings(as.numeric(hi))
  if (!is.finite(lo) || !is.finite(hi) || lo <= 0 || hi <= 0) {
    return(NULL)
  }
  if (hi > lo) {
    g <- seq(lo, hi, length.out = n)
    tibble::tibble(length_cm = g, f_len = rep(1 / length(g), length(g)))
  } else {
    tibble::tibble(length_cm = lo, f_len = 1)
  }
}

#' Resolve trait names from a config list element
#'
#' @keywords internal
alchemist_trait_names <- function(traits, models_df) {
  nms <- if (!is.null(names(traits)) && any(nzchar(names(traits)))) {
    names(traits)
  } else {
    as.character(unlist(traits, use.names = FALSE))
  }
  nms <- nms[nzchar(nms) & nms %in% names(models_df)]
  unique(nms)
}

# Taxonomic rank levels ordered from broad to specific.
.ALCH_TAX_RANKS <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")

#' Compute a taxonomic/phylogenetic distance matrix for Alchemist pair features
#'
#' Attempts to build a continuous phylogenetic distance matrix using the Open
#' Tree of Life API (`rotl`), exactly as [build_phylo_dist_from_species()] does
#' in the similarity module. Falls back to a deterministic rank-based distance
#' (`1 - deepest_shared_rank / n_ranks`) when rotl is unavailable or fails to
#' match enough species.
#'
#' @param models_df Data frame of candidate models. Must contain at least one
#'   of `species_name`, `species`, `genus`. Recognised taxonomic rank columns
#'   (family, genus, species, etc.) are used for the rank-based fallback.
#' @param tax_col_map Named character vector mapping canonical rank names to
#'   actual column names in `models_df` (e.g. `c(family="family",
#'   genus="genus", species="species_name")`). Ordered broadest-to-specific.
#'
#' @return Numeric n x n matrix, values between 0 and 1, or `NULL` on total failure.
#'
#' @keywords internal
tax_dist_mat <- function(models_df, tax_col_map) {
  n <- nrow(models_df)

  # - Primary: Tree-of-Life phylogenetic distances -
  species_col <- tax_col_map[["species"]] %||% tax_col_map[["species_name"]] %||% NULL
  genus_col <- tax_col_map[["genus"]] %||% NULL

  if (!is.null(species_col) && species_col %in% names(models_df)) {
    sp_vec <- as.character(models_df[[species_col]])
    gn_vec <- if (!is.null(genus_col) && genus_col %in% names(models_df)) {
      as.character(models_df[[genus_col]])
    } else {
      NULL
    }
    phylo_mat <- tryCatch(
      build_phylo_dist_from_species(sp_vec, genus_vec = gn_vec),
      error = function(e) NULL
    )
    if (!is.null(phylo_mat)) {
      return(phylo_mat)
    }
  }

  # - Fallback: rank-based distance -
  rank_cols <- unname(tax_col_map[tax_col_map %in% names(models_df)])
  n_ranks <- length(rank_cols)
  if (n_ranks == 0L) {
    return(NULL)
  }

  deepest <- matrix(0L, nrow = n, ncol = n)
  for (r in seq_len(n_ranks)) {
    vals <- stringr::str_squish(as.character(models_df[[rank_cols[[r]]]]))
    vals[!nzchar(vals)] <- NA_character_
    agree <- outer(vals, vals, function(a, b) !is.na(a) & !is.na(b) & (a == b))
    deepest[agree] <- r
  }
  mat <- 1 - deepest / n_ranks
  diag(mat) <- 0
  mat
}

#' Compute pairwise coherence distance matrices
#'
#' Builds length-, depth-, and frequency-coherence feature matrices for all
#' candidate model pairs. Missing columns or `mode = "none"` silently return
#' `NULL` for that dimension. The asymmetric convention matches
#' [build_pair_data()]: `mat[i, j]` is the coherence distance when
#' model j is the anchor and model i is the donor.
#'
#' @param models_df Data frame of candidate models.
#' @param coherence_cfg Named list with sub-lists `length`, `depth`, and
#'   `frequency`, each carrying a `mode` character field (`"overlap"`,
#'   `"literal"`, or `"none"`).
#'
#' @return Named list of matrices (or `NULL` entries for disabled dimensions).
#'   Names: `length_coherence`, `depth_coherence`, `frequency_coherence`.
#'
#' @keywords internal
coherence_mats <- function(models_df, coherence_cfg) {
  len_mode <- as.character(coherence_cfg$length$mode %||% "none")
  dep_mode <- as.character(coherence_cfg$depth$mode %||% "none")
  freq_mode <- as.character(coherence_cfg$frequency$mode %||% "none")

  # source controls which column(s) back each interval dimension:
  #   "best"    - study columns if present, species columns as fallback (default)
  #   "study"   - study-level sampling range only
  #   "species" - species-level biological range only
  #   "both"    - compute independently for both; returns two keyed matrices
  #               (e.g. length_coherence_study + length_coherence_species)
  len_source <- as.character(coherence_cfg$length$source %||% "best")
  dep_source <- as.character(coherence_cfg$depth$source %||% "best")

  n <- nrow(models_df)

  resolve_col <- function(candidates) {
    col <- intersect(candidates, names(models_df))
    if (length(col) == 0L) NULL else col[[1L]]
  }
  resolve_num <- function(col) {
    if (is.null(col)) {
      return(rep(NA_real_, n))
    }
    v <- suppressWarnings(as.numeric(models_df[[col]]))
    v[!is.finite(v)] <- NA_real_
    v
  }

  interval_mat <- function(lo_vals, hi_vals, method) {
    if (identical(method, "none")) {
      return(NULL)
    }
    mat <- matrix(NA_real_, nrow = n, ncol = n)
    diag(mat) <- 0
    for (j in seq_len(n)) {
      for (i in seq_len(n)) {
        if (i == j) next
        mat[i, j] <- interval_overlap_distance(
          anchor_min = lo_vals[[j]], anchor_max = hi_vals[[j]],
          cand_min = lo_vals[[i]], cand_max = hi_vals[[i]],
          method = method
        )
      }
    }
    mat
  }

  # Returns a named list of zero, one, or two matrices for a given dimension.
  # For source != "both", the key is plain `{dim}_coherence`.
  # For source == "both", keys are `{dim}_coherence_study` and
  # `{dim}_coherence_species` (absent if the corresponding columns are missing).
  make_dim_mats <- function(mode, source,
                            study_lo, study_hi, sp_lo, sp_hi, dim_key) {
    base_key <- paste0(dim_key, "_coherence")
    if (identical(mode, "none")) {
      return(stats::setNames(list(NULL), base_key))
    }

    if (identical(source, "both")) {
      result <- list()
      lo_s <- resolve_num(resolve_col(study_lo))
      hi_s <- resolve_num(resolve_col(study_hi))
      if (any(is.finite(lo_s)) && any(is.finite(hi_s))) {
        result[[paste0(base_key, "_study")]] <- interval_mat(lo_s, hi_s, mode)
      }
      lo_sp <- resolve_num(resolve_col(sp_lo))
      hi_sp <- resolve_num(resolve_col(sp_hi))
      if (any(is.finite(lo_sp)) && any(is.finite(hi_sp))) {
        result[[paste0(base_key, "_species")]] <- interval_mat(lo_sp, hi_sp, mode)
      }
      if (length(result) == 0L) result[[base_key]] <- NULL
      return(result)
    }

    lo_candidates <- switch(source,
      study = study_lo,
      species = sp_lo,
      c(study_lo, sp_lo) # "best": prefer study
    )
    hi_candidates <- switch(source,
      study = study_hi,
      species = sp_hi,
      c(study_hi, sp_hi)
    )
    lo <- resolve_num(resolve_col(lo_candidates))
    hi <- resolve_num(resolve_col(hi_candidates))
    mat <- if (any(is.finite(lo)) && any(is.finite(hi))) interval_mat(lo, hi, mode) else NULL
    stats::setNames(list(mat), base_key)
  }

  len_mats <- make_dim_mats(
    len_mode, len_source,
    "study_length_min", "study_length_max",
    "species_length_min", "species_length_max",
    "length"
  )
  dep_mats <- make_dim_mats(
    dep_mode, dep_source,
    "study_depth_min", "study_depth_max",
    "species_depth_min", "species_depth_max",
    "depth"
  )

  freq_mat <- NULL
  if (!identical(freq_mode, "none")) {
    freq_vals <- resolve_num("frequency")
    valid_freq <- freq_vals[is.finite(freq_vals) & freq_vals > 0]
    if (length(valid_freq) >= 2L) {
      freq_span <- compute_frequency_span(valid_freq)
      if (is.finite(freq_span) && freq_span > 0) {
        freq_mat <- matrix(NA_real_, nrow = n, ncol = n)
        diag(freq_mat) <- 0
        for (j in seq_len(n)) {
          for (i in seq_len(n)) {
            if (i == j) next
            freq_mat[i, j] <- frequency_offset_distance(
              anchor_freq = freq_vals[[j]],
              cand_freq = freq_vals[[i]],
              method = freq_mode,
              freq_span = freq_span
            )
          }
        }
      }
    }
  }

  c(len_mats, dep_mats, list(frequency_coherence = freq_mat))
}

#' Build the pair-level supervised training table
#'
#' @param models_df Candidate-model data frame.
#' @param species_trait_names Character vector of species trait column names.
#' @param study_trait_names Character vector of study trait column names.
#' @param coherence_cfg Optional named list with sub-lists `length`, `depth`,
#'   `frequency`, each containing a `mode` field (`"overlap"`, `"literal"`,
#'   or `"none"`). When `NULL` all coherence terms are skipped.
#' @param taxonomic_distance Logical. When `TRUE`, detected taxonomic rank
#'   traits (family, genus, species, etc.) are additionally represented as a
#'   single `.dist_tax` rank-based distance feature alongside the individual
#'   Gower features.
#'
#' @return A list with `training_data`, `feature_cols`, `all_traits`,
#'   `species_trait_names`, `species_feature_cols`, `n_models`, `model_ids`,
#'   `donor_sigma_matrix`, `target_sigma`, and `trait_mats`.
#'
#' @keywords internal
build_pair_data <- function(models_df,
                            species_trait_names,
                            study_trait_names,
                            coherence_cfg = NULL,
                            taxonomic_distance = FALSE,
                            feature_type = c(
                              "gower", "difference",
                              "mahalanobis"
                            ),
                            progress = FALSE) {
  feature_type <- match.arg(feature_type)
  all_traits <- unique(c(species_trait_names, study_trait_names))
  if (length(all_traits) == 0L) {
    stop(
      "No configured traits found in `candidate_models`. Supply species_traits and/or study_traits.",
      call. = FALSE
    )
  }

  n <- nrow(models_df)

  col_fn <- switch(feature_type,
    difference = diff_col,
    mahalanobis = squared_col,
    gower_col
  )
  feat_label <- switch(feature_type,
    difference = "signed-difference",
    mahalanobis = "squared-difference (Mahalanobis)",
    "Gower"
  )

  report_progress(
    progress,
    "  [Alchemist] Computing per-trait ", feat_label, " matrices (",
    length(all_traits), " traits, ", n, " models)..."
  )

  # Normalize known set-type columns and expand any semicolon-separated
  # multi-value trait into one binary indicator matrix per unique value.
  # Column naming: .dist_ocean_basin__atlantic, .dist_fao_area__77, etc.
  # (double-underscore matches the expand_trait_block() convention in similarity.R)
  trait_mat_list <- lapply(all_traits, function(tr) {
    x <- models_df[[tr]]
    if (identical(tr, "ocean_basin")) {
      x <- normalize_alchemist_ocean_basin(x)
    } else if (identical(tr, "fao_area")) {
      x <- normalize_alchemist_fao_area(x)
    } else if (identical(tr, "species") && "genus" %in% names(models_df)) {
      # Use full binomial so same-epithet species in different genera are distinct
      g <- trimws(as.character(models_df[["genus"]]))
      s <- trimws(as.character(x))
      combined <- paste(g, s)
      combined[is.na(g) | g == "NA" | is.na(s) | s == "NA"] <- NA_character_
      x <- combined
    }
    expand_multival_col(x, col_fn, tr)
  })
  trait_mats <- do.call(c, trait_mat_list)

  n_feat_cols <- length(trait_mats)
  if (n_feat_cols > length(all_traits)) {
    report_progress(
      progress,
      "  [Alchemist]   Set-type expansion: ", length(all_traits), " traits -> ",
      n_feat_cols, " binary indicator columns."
    )
  }

  # - Optional phylogenetic/taxonomic distance -
  # When enabled, recognized taxonomic rank columns (family, genus, species,
  # etc.) are REPLACED by a single .dist_tax feature. The primary method is the
  # rotl/Tree of Life cophenetic distance; rank-based scoring is the fallback.
  if (isTRUE(taxonomic_distance)) {
    tax_ranks_found <- intersect(.ALCH_TAX_RANKS, tolower(species_trait_names))
    tax_col_map <- stats::setNames(
      vapply(tax_ranks_found, function(rk) {
        m <- species_trait_names[tolower(species_trait_names) == rk]
        if (length(m) > 0L) m[[1L]] else NA_character_
      }, character(1)),
      tax_ranks_found
    )
    tax_col_map <- tax_col_map[!is.na(tax_col_map) & tax_col_map %in% names(models_df)]

    if (length(tax_col_map) >= 1L) {
      report_progress(
        progress,
        "  [Alchemist] Computing phylogenetic distance (rotl, fallback: rank-based) for: ",
        paste(names(tax_col_map), collapse = ", "), "..."
      )
      tax_mat <- tax_dist_mat(models_df, tax_col_map)
      if (!is.null(tax_mat)) {
        trait_mats[[".dist_tax"]] <- tax_mat
        # Remove individual taxonomic Gower features - they are now redundant
        drop_cols <- paste0(".dist_", unname(tax_col_map))
        trait_mats <- trait_mats[setdiff(names(trait_mats), drop_cols)]
        report_progress(
          progress,
          "  [Alchemist]   Replaced ", paste(drop_cols, collapse = ", "),
          " with .dist_tax."
        )
      } else {
        report_progress(
          progress,
          "  [Alchemist]   WARNING: phylogenetic distance failed; keeping individual Gower features."
        )
      }
    }
  }

  # - Coherence features -
  if (!is.null(coherence_cfg) && length(coherence_cfg) > 0L) {
    report_progress(progress, "  [Alchemist] Computing coherence feature matrices...")
    coh_mats <- coherence_mats(models_df, coherence_cfg)
    for (coh_nm in names(coh_mats)) {
      if (!is.null(coh_mats[[coh_nm]])) {
        trait_mats[[paste0(".dist_", coh_nm)]] <- coh_mats[[coh_nm]]
        report_progress(progress, "    + ", coh_nm, " added.")
      }
    }
  }

  feature_cols <- names(trait_mats)

  # Match both simple columns (.dist_ocean_basin) and all expanded indicator
  # columns derived from a species trait (.dist_ocean_basin__atlantic, etc.)
  species_feature_cols <- unlist(lapply(species_trait_names, function(tr) {
    base <- paste0(".dist_", tr)
    prefix <- paste0(base, "__")
    c(
      if (base %in% names(trait_mats)) base else character(0),
      grep(paste0("^\\Q", prefix, "\\E"), names(trait_mats), value = TRUE)
    )
  }), use.names = FALSE)
  species_feature_cols <- c(
    species_feature_cols,
    if (".dist_tax" %in% names(trait_mats)) ".dist_tax" else character(0)
  )

  slope_vals <- suppressWarnings(as.numeric(models_df$slope_len))
  intercept_vals <- suppressWarnings(as.numeric(models_df$intercept_len))
  slope_vals[!is.finite(slope_vals)] <- NA_real_
  intercept_vals[!is.finite(intercept_vals)] <- NA_real_

  len_min_col <- intersect(c("study_length_min", "length_minimum"), names(models_df))[[1]] %||% NULL
  len_max_col <- intersect(c("study_length_max", "length_maximum"), names(models_df))[[1]] %||% NULL

  anchor_pdfs <- lapply(seq_len(n), function(j) {
    build_anchor_pdf(
      lo = if (!is.null(len_min_col)) models_df[[len_min_col]][[j]] else NA_real_,
      hi = if (!is.null(len_max_col)) models_df[[len_max_col]][[j]] else NA_real_
    )
  })
  n_valid_pdfs <- sum(!vapply(anchor_pdfs, is.null, logical(1)))

  report_progress(
    progress,
    "  [Alchemist] Building ", n, "x", n,
    " donor sigma matrix (", n_valid_pdfs, "/", n,
    " models have valid length ranges)..."
  )
  tick_at <- unique(floor(seq(0, n, length.out = 11L)))[-1L]
  donor_sigma_mat <- matrix(NA_real_, nrow = n, ncol = n)
  for (j in seq_len(n)) {
    pdf_j <- anchor_pdfs[[j]]
    if (is.null(pdf_j)) next
    # Evaluate every donor equation against the current anchor PDF in one
    # vectorized pass rather than calling equation_sigma_mean() once per donor.
    log_len <- log10(pdf_j$length_cm)
    f_len <- as.numeric(pdf_j$f_len)
    f_sum <- sum(f_len, na.rm = TRUE)
    if (!is.finite(f_sum) || f_sum <= 0) {
      next
    }
    weight_vec <- f_len / f_sum
    ts_mat <- tcrossprod(slope_vals, log_len)
    ts_mat <- sweep(ts_mat, 1L, intercept_vals, "+")
    phi_mat <- 10^(ts_mat / 10)
    donor_sigma_mat[, j] <- as.numeric(phi_mat %*% weight_vec)
    if (progress && j %in% tick_at) {
      report_progress(
        progress,
        "  [Alchemist]   sigma matrix: model ", j, "/", n,
        " (", round(j / n * 100), "%)"
      )
    }
  }
  target_sigma <- diag(donor_sigma_mat)
  diag(donor_sigma_mat) <- NA_real_

  species_names <- as.character(models_df$species_name %||% rep(NA_character_, n))
  species_names[is.na(species_names)] <- NA_character_

  model_ids <- as.character(
    models_df$model_id_chr %||% models_df$model_id %||% as.character(seq_len(n))
  )

  report_progress(
    progress,
    "  [Alchemist] Assembling donor-anchor model pairs (excluding same-species)..."
  )
  valid_mask <- is.finite(donor_sigma_mat) & donor_sigma_mat > 0
  diag(valid_mask) <- FALSE
  pair_idx <- which(valid_mask, arr.ind = TRUE)
  if (nrow(pair_idx) > 0L) {
    donor_idx <- pair_idx[, 1L]
    anchor_idx <- pair_idx[, 2L]
    keep_idx <- !(
      !is.na(species_names[donor_idx]) &
        !is.na(species_names[anchor_idx]) &
        species_names[donor_idx] == species_names[anchor_idx]
    )
    pair_idx <- pair_idx[keep_idx, , drop = FALSE]
  }

  if (nrow(pair_idx) == 0L) {
    stop("No valid donor-anchor pairs found for distance learning.", call. = FALSE)
  }

  donor_idx <- pair_idx[, 1L]
  anchor_idx <- pair_idx[, 2L]
  acoustic_dist <- abs(
    log(donor_sigma_mat[cbind(donor_idx, anchor_idx)]) -
      log(target_sigma[anchor_idx])
  )
  keep_idx <- is.finite(acoustic_dist)
  donor_idx <- donor_idx[keep_idx]
  anchor_idx <- anchor_idx[keep_idx]
  acoustic_dist <- acoustic_dist[keep_idx]
  k <- length(acoustic_dist)
  if (k == 0L) {
    stop("No valid donor-anchor pairs found for distance learning.", call. = FALSE)
  }

  report_progress(
    progress,
    "  [Alchemist] Materializing ", k, " training pairs..."
  )
  training_data <- data.frame(
    .anchor_idx = as.integer(anchor_idx),
    .donor_idx = as.integer(donor_idx),
    .anchor_species = species_names[anchor_idx],
    .donor_species = species_names[donor_idx],
    .split_group = species_names[anchor_idx],
    .outcome = as.numeric(acoustic_dist),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (length(feature_cols) > 0L) {
    for (fc in feature_cols) {
      training_data[[fc]] <- trait_mats[[fc]][cbind(donor_idx, anchor_idx)]
    }
  }
  training_data <- tibble::as_tibble(training_data)

  list(
    training_data = training_data,
    feature_cols = feature_cols,
    all_traits = all_traits,
    species_trait_names = species_trait_names,
    species_feature_cols = species_feature_cols,
    n_models = n,
    model_ids = model_ids,
    donor_sigma_matrix = donor_sigma_mat,
    target_sigma = target_sigma,
    trait_mats = trait_mats,
    feature_type = feature_type
  )
}

# - Alchemist distance learner -

normalize_alchemist_oof_mode <- function(mode) {
  match.arg(
    as.character(mode %||% "anchor_species"),
    c("anchor_species", "species_purged")
  )
}

build_alchemist_oof_splits <- function(training_data,
                                       inner_folds = 5L,
                                       seed = NULL,
                                       oof_mode = "anchor_species") {
  training_data <- tibble::as_tibble(training_data)
  n <- nrow(training_data)
  if (n < 2L) {
    return(NULL)
  }

  oof_mode <- normalize_alchemist_oof_mode(oof_mode)
  inner_folds <- as.integer(inner_folds)

  if (identical(oof_mode, "anchor_species")) {
    foldid <- if (".split_group" %in% names(training_data)) {
      grouped_foldid(training_data$.split_group, n_folds = inner_folds, seed = seed)
    } else {
      row_foldid(n, n_folds = inner_folds, seed = seed)
    }
    if (is.null(foldid)) {
      return(NULL)
    }
    return(lapply(sort(unique(foldid)), function(fold_now) {
      list(
        fold_id = as.integer(fold_now),
        holdout_groups = unique(as.character(training_data$.split_group[foldid == fold_now])),
        train_idx = which(foldid != fold_now),
        valid_idx = which(foldid == fold_now)
      )
    }))
  }

  required_cols <- c(".anchor_species", ".donor_species")
  missing_cols <- setdiff(required_cols, names(training_data))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf(
        "Strict Alchemist species-purged OOF requires column(s): %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  anchor_species <- as.character(training_data$.anchor_species)
  donor_species <- as.character(training_data$.donor_species)
  if (any(is.na(anchor_species) | !nzchar(anchor_species)) ||
    any(is.na(donor_species) | !nzchar(donor_species))) {
    stop(
      "Strict Alchemist species-purged OOF requires non-missing anchor and donor species labels.",
      call. = FALSE
    )
  }

  groups <- sort(unique(anchor_species))
  n_folds_eff <- min(inner_folds, length(groups))
  if (!is.finite(n_folds_eff) || n_folds_eff < 2L) {
    return(NULL)
  }
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  shuffled_groups <- sample(groups, length(groups), replace = FALSE)
  group_fold_tbl <- tibble::tibble(
    group = shuffled_groups,
    fold_id = rep(seq_len(n_folds_eff), length.out = length(shuffled_groups))
  )

  lapply(seq_len(n_folds_eff), function(fold_now) {
    holdout_groups <- as.character(group_fold_tbl$group[group_fold_tbl$fold_id == fold_now])
    valid_idx <- which(anchor_species %in% holdout_groups)
    train_idx <- which(
      !anchor_species %in% holdout_groups &
        !donor_species %in% holdout_groups
    )
    list(
      fold_id = as.integer(fold_now),
      holdout_groups = holdout_groups,
      train_idx = train_idx,
      valid_idx = valid_idx
    )
  })
}

#' Resolve and validate Alchemist base learner method names
#'
#' Accepts a character vector of method names and returns only those that are
#' recognised by [fit_base_learner()]. Unrecognised names are silently
#' dropped with a warning.
#'
#' @param methods Character vector of requested method names.
#'
#' @return Character vector of validated method names, length >= 1.
#'
#' @keywords internal
resolve_learner_methods <- function(methods,
                                    method_settings = NULL) {
  # Build the public Alchemist method namespace from the shared family catalog
  # so variant aliases are derived from config rather than hard-coded here.
  catalog <- meta_policy_method_catalog(method_settings = method_settings)
  supported_families <- c("glm", "glmnet", "gam", "rpart", "ranger", "xgboost", "quantreg")
  valid <- names(Filter(function(spec) spec$family %in% supported_families, catalog$specs))
  defaults <- catalog$default_super_methods[
    vapply(catalog$default_super_methods, function(method) {
      spec <- catalog$specs[[method]] %||% NULL
      is.list(spec) && spec$family %in% supported_families
    }, logical(1))
  ]

  methods <- methods %||% defaults
  methods <- unique(stringr::str_squish(as.character(unlist(methods, use.names = FALSE))))
  methods <- methods[!is.na(methods) & nzchar(methods)]
  unknown <- setdiff(methods, valid)
  if (length(unknown) > 0L) {
    stop(
      sprintf("Unknown Alchemist base learner method(s): %s", paste(unknown, collapse = ", ")),
      call. = FALSE
    )
  }
  if (length(methods) == 0L) {
    methods <- c("glmnet_elasticnet", "ranger", "xgboost")
  }
  methods
}

#' Build a numeric feature matrix from Alchemist training or prediction data
#'
#' All Gower distance features are numeric and bounded between 0 and 1. Missing values
#' are imputed at 0.5 (the Gower midpoint for unknown comparisons).
#'
#' @param data Tibble or data frame containing at least the columns named by
#'   `feature_cols`.
#' @param feature_cols Character vector of Gower distance column names.
#'
#' @return A numeric matrix with `nrow(data)` rows and `length(feature_cols)`
#'   columns.
#'
#' @keywords internal
feature_matrix <- function(data, feature_cols) {
  mat <- as.matrix(data[, feature_cols, drop = FALSE])
  mode(mat) <- "numeric"
  mat[!is.finite(mat)] <- 0.5
  mat
}

#' Derive per-family method settings from a shared settings list
#'
#' Extracts the settings block for one method family and applies any
#' variant-specific overrides when the method name has a `_variant` suffix.
#'
#' @param family Character. One of `"glm"`, `"glmnet"`, `"gam"`, `"rpart"`,
#'   `"ranger"`, `"xgboost"`.
#' @param method Character. Full method name, e.g. `"ranger_shallow"`.
#' @param method_settings Named list of per-family settings from config.
#'
#' @return Named list of resolved settings for this family+variant.
#'
#' @keywords internal
learner_method_settings <- function(family, method, method_settings) {
  ms <- as.list(normalize_meta_policy_method_settings(method_settings)[[family]] %||% list())
  variant <- sub(paste0("^", family, "_?"), "", method)
  if (nzchar(variant) && !is.null(ms$variants[[variant]])) {
    for (nm in names(ms$variants[[variant]])) {
      ms[[nm]] <- ms$variants[[variant]][[nm]]
    }
  }
  ms
}

#' Prepare one dense regression frame for linear-style Alchemist learners
#'
#' @param x_train Numeric feature matrix.
#'
#' @return A list with pruned `data` and retained `feature_cols`.
#'
#' @keywords internal
prepare_alchemist_regression_frame <- function(x_train) {
  # Drop constant and aliased columns once so both glm and quantreg use the
  # same stable predictor frame and do not emit singular-design warnings.
  df_train <- as.data.frame(x_train, check.names = FALSE)
  keep_cols <- names(df_train)[vapply(df_train, function(col) {
    vals <- col[is.finite(col)]
    length(vals) > 0L && !isTRUE(all(abs(vals - vals[[1]]) <= sqrt(.Machine$double.eps)))
  }, logical(1))]
  if (length(keep_cols) == 0L) {
    keep_cols <- names(df_train)[seq_len(min(1L, ncol(df_train)))]
  }
  qr_rank <- tryCatch(
    {
      design_mat <- stats::model.matrix(~ . - 1, data = df_train[, keep_cols, drop = FALSE])
      qr_obj <- qr(design_mat)
      colnames(design_mat)[qr_obj$pivot[seq_len(qr_obj$rank)]]
    },
    error = function(e) keep_cols
  )
  qr_rank <- intersect(keep_cols, qr_rank)
  if (length(qr_rank) == 0L) {
    qr_rank <- keep_cols
  }

  list(
    data = df_train[, qr_rank, drop = FALSE],
    feature_cols = qr_rank
  )
}

#' Fit one Alchemist base learner on a training matrix
#'
#' Dispatches on `family` derived from `method`. The outcome vector `y_train`
#' must already be transformed (e.g. `log1p`-ed) before calling this function.
#'
#' @param x_train Numeric feature matrix (training rows).
#' @param y_train Numeric outcome vector (training rows), in transformed space.
#' @param method Character method name. See [resolve_learner_methods()] for
#'   the supported set.
#' @param method_settings Named list of per-family tuning settings.
#' @param seed Integer random seed.
#' @param lambda_rule Character. One of `"lambda.1se"` or `"lambda.min"`.
#'   Only used by glmnet-family methods.
#'
#' @return A list with class `"BaseLearner"` and fields
#'   `fit`, `method`, `family`, `lambda_rule`.
#'
#' @keywords internal
fit_base_learner <- function(x_train, y_train, method,
                             method_settings = NULL,
                             seed = NULL,
                             lambda_rule = "lambda.1se") {
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  # Resolve the family from the shared method catalog so config-defined
  # variants such as xgboost_conservative and quantreg_q90 behave consistently.
  family <- meta_policy_method_spec(method, method_settings = method_settings)$family

  ms <- learner_method_settings(family, method, method_settings %||% list())
  feature_cols <- colnames(x_train)

  fit <- switch(family,
    glm = {
      # Prune the linear predictor frame before fitting so rank-deficient
      # columns do not leak warnings into downstream prediction calls.
      prep <- prepare_alchemist_regression_frame(x_train)
      feature_cols <- prep$feature_cols
      df_train <- prep$data
      df_train$.y <- y_train
      stats::lm(.y ~ ., data = df_train)
    },
    glmnet = {
      alpha_val <- switch(method,
        glmnet_ridge = 0,
        glmnet_lasso = 1,
        glmnet_elasticnet = as.numeric(ms$alpha %||% 0.25),
        0.25
      )
      glmnet::cv.glmnet(
        x = x_train,
        y = y_train,
        alpha = alpha_val,
        nfolds = 5L,
        standardize = isTRUE(ms$standardize %||% TRUE),
        type.measure = ms$type_measure %||% "mse"
      )
    },
    gam = {
      df_train <- as.data.frame(x_train, check.names = FALSE)
      df_train$.y <- y_train
      feat_nms <- setdiff(names(df_train), ".y")
      terms <- vapply(feat_nms, function(v) {
        n_unique <- length(unique(df_train[[v]][is.finite(df_train[[v]])]))
        if (n_unique <= 3L) {
          v
        } else {
          k_val <- min(5L, n_unique - 1L)
          sprintf("s(%s, bs='cr', k=%d)", v, k_val)
        }
      }, character(1))
      gam_formula <- stats::as.formula(
        paste(".y ~", paste(terms, collapse = " + "))
      )
      n_train <- nrow(df_train)
      if (n_train > 10000L) {
        mgcv::bam(
          formula = gam_formula,
          data = df_train,
          method = ms$fit_method %||% "fREML",
          discrete = TRUE
        )
      } else {
        mgcv::gam(
          formula = gam_formula,
          data = df_train,
          method = ms$fit_method %||% "REML",
          select = isTRUE(ms$select_terms %||% TRUE)
        )
      }
    },
    ranger = {
      rf <- ranger::ranger(
        x = as.data.frame(x_train, check.names = FALSE),
        y = y_train,
        seed = as.integer(seed),
        num.threads = 1L,
        verbose = FALSE,
        num.trees = as.integer(ms$num_trees %||% 500L),
        min.node.size = as.integer(ms$min_node_size %||% 5L),
        sample.fraction = as.numeric(ms$sample_fraction %||% 1.0),
        replace = isTRUE(ms$replace %||% TRUE),
        respect.unordered.factors = "order"
      )
      rf$predictions <- NULL
      rf$call <- NULL
      rf
    },
    rpart = {
      df_train <- as.data.frame(x_train, check.names = FALSE)
      df_train$.y <- y_train
      rpart::rpart(
        .y ~ .,
        data = df_train,
        control = rpart::rpart.control(
          cp = as.numeric(ms$cp %||% 0.01),
          minsplit = as.integer(ms$minsplit %||% 20L),
          minbucket = as.integer(ms$minbucket %||% 7L),
          maxdepth = as.integer(ms$maxdepth %||% 30L)
        )
      )
    },
    xgboost = {
      dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train)
      xgb_params <- list(
        eta = as.numeric(ms$eta %||% 0.3),
        max_depth = as.integer(ms$max_depth %||% 6L),
        min_child_weight = as.numeric(ms$min_child_weight %||% 1),
        subsample = as.numeric(ms$subsample %||% 1.0),
        colsample_bytree = as.numeric(ms$colsample_bytree %||% 1.0),
        lambda = as.numeric(ms$lambda %||% 1.0),
        alpha = as.numeric(ms$alpha %||% 0.0),
        objective = "reg:squarederror",
        nthread = 1L
      )
      xgboost::xgb.train(
        params = xgb_params,
        data = dtrain,
        nrounds = as.integer(ms$nrounds %||% 100L),
        verbose = 0L
      )
    },
    quantreg = {
      if (!requireNamespace("quantreg", quietly = TRUE)) {
        stop("Fitting Alchemist method 'quantreg' requires the suggested package 'quantreg' to be installed.", call. = FALSE)
      }
      # Reuse the same pruned predictor frame as glm so quantile fits do not
      # try to solve a singular design matrix.
      prep <- prepare_alchemist_regression_frame(x_train)
      feature_cols <- prep$feature_cols
      df_train <- prep$data
      df_train$.y <- y_train
      quantreg::rq(
        .y ~ .,
        data = df_train,
        tau = as.numeric(ms$tau %||% 0.50),
        method = as.character(ms$fit_method %||% "fn")
      )
    },
    stop(sprintf("Unsupported Alchemist base learner: '%s'.", method), call. = FALSE)
  )

  structure(
    list(
      fit = fit,
      method = method,
      family = family,
      lambda_rule = lambda_rule,
      feature_cols = feature_cols
    ),
    class = "BaseLearner"
  )
}

#' Predict from a fitted Alchemist base learner
#'
#' Returns raw predictions in the same (transformed) space that the learner was
#' trained in. Back-transformation to the original outcome scale is the
#' responsibility of the caller.
#'
#' @param object A `"BaseLearner"` from [fit_base_learner()].
#' @param x_new Numeric feature matrix (prediction rows).
#'
#' @return Numeric prediction vector, length `nrow(x_new)`.
#'
#' @keywords internal
predict_base_learner <- function(object, x_new) {
  if (!inherits(object, "BaseLearner")) {
    stop("'object' must be a 'tsb_alchemist_base_learner'.", call. = FALSE)
  }
  df_new <- as.data.frame(x_new, check.names = FALSE)
  if (!is.null(object$feature_cols) && length(object$feature_cols) > 0L) {
    missing_cols <- setdiff(object$feature_cols, names(df_new))
    if (length(missing_cols) > 0L) {
      stop(
        sprintf(
          "Prediction data is missing Alchemist feature column(s): %s",
          paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    df_new <- df_new[, object$feature_cols, drop = FALSE]
    x_new <- as.matrix(df_new)
    mode(x_new) <- "numeric"
  }
  # Use getFromNamespace throughout to bypass S3 dispatch - on PSOCK workers
  # imported packages are loaded as namespaces but not attached, so
  # stats::predict cannot find their S3 methods via global dispatch.
  switch(object$family,
    glm = {
      as.numeric(stats::predict.lm(object$fit, newdata = df_new))
    },
    glmnet = {
      pfn <- utils::getFromNamespace("predict.cv.glmnet", "glmnet")
      as.numeric(pfn(object$fit, newx = x_new, s = object$lambda_rule %||% "lambda.1se"))
    },
    gam = {
      pfn <- utils::getFromNamespace("predict.gam", "mgcv")
      as.numeric(pfn(object$fit, newdata = df_new, type = "response"))
    },
    ranger = {
      pfn <- utils::getFromNamespace("predict.ranger", "ranger")
      as.numeric(pfn(object$fit, data = df_new)$predictions)
    },
    rpart = {
      pfn <- utils::getFromNamespace("predict.rpart", "rpart")
      as.numeric(pfn(object$fit, newdata = df_new))
    },
    xgboost = {
      dtest <- xgboost::xgb.DMatrix(data = x_new)
      pfn <- utils::getFromNamespace("predict.xgb.Booster", "xgboost")
      as.numeric(pfn(object$fit, dtest))
    },
    quantreg = {
      pfn <- utils::getFromNamespace("predict.rq", "quantreg")
      as.numeric(pfn(object$fit, newdata = df_new))
    },
    stop(sprintf("Unsupported Alchemist base learner family: '%s'.", object$family),
      call. = FALSE
    )
  )
}

#' Run K-fold OOF predictions for one Alchemist base learner method
#'
#' This is the per-method worker function that parallel workers execute. Each
#' worker runs all K folds for its assigned method and returns a named list
#' with the OOF prediction vector and any error captured.
#'
#' @param method Character. Single base learner method name.
#' @param x_all Numeric feature matrix for the full training set.
#' @param y_all Numeric outcome vector (transformed), full training set.
#' @param foldid Integer fold-ID vector, length `nrow(x_all)`.
#' @param method_settings Named list of per-family settings.
#' @param seed Integer base seed. Each fold offsets this by its fold index.
#' @param lambda_rule Character. Lambda selection rule for glmnet methods.
#'
#' @return A named list with fields `method`, `oof_pred` (numeric vector or
#'   `NULL`), and `error` (character or `NULL`).
#'
#' @keywords internal
run_oof_method <- function(method, x_all, y_all, foldid = NULL,
                           fold_splits = NULL,
                           method_settings, seed, lambda_rule) {
  n <- length(y_all)
  oof_pred <- rep(NA_real_, n)
  ok <- TRUE
  err_msg <- NULL

  if (is.null(fold_splits)) {
    if (is.null(foldid)) {
      stop("Either `foldid` or `fold_splits` must be supplied.", call. = FALSE)
    }
    fold_splits <- lapply(sort(unique(foldid)), function(fold_now) {
      list(
        fold_id = as.integer(fold_now),
        train_idx = which(foldid != fold_now),
        valid_idx = which(foldid == fold_now)
      )
    })
  }

  for (fold_spec in fold_splits) {
    fold_now <- fold_spec$fold_id %||% NA_integer_
    train_idx <- as.integer(fold_spec$train_idx %||% integer(0))
    valid_idx <- as.integer(fold_spec$valid_idx %||% integer(0))
    if (length(train_idx) == 0L || length(valid_idx) == 0L) {
      ok <- FALSE
      break
    }

    fold_seed <- if (is.null(seed)) NULL else as.integer(seed) + as.integer(fold_now)
    learner <- tryCatch(
      fit_base_learner(
        x_train = x_all[train_idx, , drop = FALSE],
        y_train = y_all[train_idx],
        method = method,
        method_settings = method_settings,
        seed = fold_seed,
        lambda_rule = lambda_rule
      ),
      error = function(e) structure(conditionMessage(e), class = "try-error")
    )

    if (inherits(learner, "try-error")) {
      ok <- FALSE
      err_msg <- as.character(learner)
      break
    }

    preds <- tryCatch(
      predict_base_learner(learner, x_all[valid_idx, , drop = FALSE]),
      error = function(e) structure(conditionMessage(e), class = "try-error")
    )

    if (inherits(preds, "try-error") || length(preds) != length(valid_idx)) {
      ok <- FALSE
      err_msg <- if (inherits(preds, "try-error")) {
        as.character(preds)
      } else {
        "Prediction length mismatch."
      }
      break
    }

    oof_pred[valid_idx] <- preds
  }

  list(
    method = method,
    oof_pred = if (ok && all(is.finite(oof_pred))) oof_pred else NULL,
    error = err_msg
  )
}

#' Fit a Super Learner ensemble for Alchemist pairwise distance learning
#'
#' Trains a stacked ensemble over a library of base learners using K-fold
#' out-of-fold (OOF) cross-validation. Base learner fits are parallelised
#' across methods when `workers > 1`. NNLS stacking weights are computed from
#' the OOF predictions; a final round of base learner fits on the full training
#' set is then used for prediction at inference time.
#'
#' This function is entirely independent of the policy-learner path in
#' `meta_policy.R`. It is purpose-built for learning pairwise acoustic
#' distances from Gower trait features.
#'
#' @param training_data Tibble produced by [build_pair_data()].
#'   Must contain `.outcome`, optionally `.split_group`, and all columns named
#'   by `feature_cols`.
#' @param feature_cols Character vector of Gower distance column names
#'   (e.g. `.dist_swimbladder_type`).
#' @param methods Character vector of base learner method names. See
#'   [resolve_learner_methods()] for the supported set.
#' @param outcome_transform One of `"log1p"` or `"identity"`. Applied to
#'   `.outcome` before fitting; predictions are back-transformed before
#'   storage.
#' @param lambda_rule Lambda selection rule for glmnet-family base learners.
#'   One of `"lambda.1se"` or `"lambda.min"`.
#' @param inner_folds Integer number of cross-validation folds.
#' @param seed Integer random seed for fold assignment and base learner fits.
#' @param method_settings Named list of per-family tuning overrides (same
#'   structure as `metalearner.method_settings` in the config YAML).
#' @param oof_mode Cross-validation split mode. `"anchor_species"` keeps the
#'   current receiving-species grouping; `"species_purged"` removes the held-out
#'   species from both anchor and donor roles in each OOF fold.
#' @param workers Integer number of parallel workers. Workers are assigned one
#'   method each. `1L` runs sequentially.
#' @param progress Logical. Emit [tsb_message()] progress lines when `TRUE`.
#'
#' @return A list with class `"SuperLearner"` containing:
#'   \describe{
#'     \item{`fit`}{Named list of final base learner objects
#'       (`"BaseLearner"`).}
#'     \item{`weights`}{Named numeric vector of NNLS ensemble weights.}
#'     \item{`feature_cols`}{Character vector of feature column names.}
#'     \item{`outcome_transform`}{The transform applied during training.}
#'     \item{`oof_ensemble_prediction`}{Numeric vector of OOF ensemble
#'       predictions in the **original** (back-transformed) scale.}
#'     \item{`oof_predictions`}{Tibble of per-method OOF predictions in
#'       original scale.}
#'     \item{`oof_performance`}{Tibble with `method`, `rmse`, and `mae`
#'       columns.}
#'   }
#'
#' @keywords internal
fit_super_learner <- function(training_data,
                              feature_cols,
                              methods,
                              outcome_transform = "log1p",
                              lambda_rule = "lambda.1se",
                              inner_folds = 5L,
                              seed = NULL,
                              method_settings = NULL,
                              oof_mode = "anchor_species",
                              workers = 1L,
                              progress = FALSE) {
  training_data <- tibble::as_tibble(training_data)
  methods <- resolve_learner_methods(methods, method_settings = method_settings)
  inner_folds <- as.integer(inner_folds)
  seed <- if (!is.null(seed)) as.integer(seed) else NULL
  workers <- as.integer(workers)
  oof_mode <- normalize_alchemist_oof_mode(oof_mode)

  x_all <- feature_matrix(training_data, feature_cols)
  y_raw <- training_data$.outcome
  y_all <- if (identical(outcome_transform, "log1p")) log1p(y_raw) else y_raw

  fold_splits <- build_alchemist_oof_splits(
    training_data = training_data,
    inner_folds = inner_folds,
    seed = seed,
    oof_mode = oof_mode
  )
  if (is.null(fold_splits) || length(fold_splits) == 0L) {
    stop(
      "Alchemist learner requires at least two training rows for CV.",
      call. = FALSE
    )
  }

  report_progress(
    progress,
    "[Alchemist] Fitting ", length(methods), " base learner(s) with ",
    length(fold_splits), "-fold CV",
    " [mode=", oof_mode, "]",
    if (workers > 1L) paste0(" across ", workers, " workers") else " (sequential)",
    "..."
  )

  # ---- OOF phase: parallelise across methods --------------------------------
  if (workers > 1L && length(methods) > 1L) {
    cl <- initialize_parallel_cluster(workers = min(workers, length(methods)))
    if (!is.null(cl)) {
      on.exit(parallel::stopCluster(cl), add = TRUE)
      oof_results <- parallel::parLapplyLB(
        cl, methods,
        fun = run_oof_method,
        x_all = x_all,
        y_all = y_all,
        fold_splits = fold_splits,
        method_settings = method_settings,
        seed = seed,
        lambda_rule = lambda_rule
      )
    } else {
      oof_results <- lapply(
        methods, run_oof_method,
        x_all = x_all,
        y_all = y_all,
        fold_splits = fold_splits,
        method_settings = method_settings,
        seed = seed,
        lambda_rule = lambda_rule
      )
    }
  } else {
    oof_results <- lapply(
      methods, run_oof_method,
      x_all = x_all,
      y_all = y_all,
      fold_splits = fold_splits,
      method_settings = method_settings,
      seed = seed,
      lambda_rule = lambda_rule
    )
  }
  names(oof_results) <- methods

  # ---- Collect successful OOF predictions ----------------------------------
  ok_methods <- Filter(function(r) !is.null(r$oof_pred), oof_results)
  failed_methods <- Filter(function(r) is.null(r$oof_pred), oof_results)

  if (length(failed_methods) > 0L) {
    for (r in failed_methods) {
      report_progress(
        progress,
        "[Alchemist]   Method failed (excluded from ensemble): ", r$method,
        if (!is.null(r$error)) paste0(" - ", r$error) else ""
      )
    }
  }

  if (length(ok_methods) == 0L) {
    stop(
      "No Alchemist base learners produced complete OOF predictions.\n",
      paste(
        names(failed_methods),
        vapply(failed_methods, function(r) r$error %||% "unknown error", character(1)),
        sep = ": ", collapse = "\n"
      ),
      call. = FALSE
    )
  }

  oof_mat_transformed <- do.call(cbind, lapply(ok_methods, `[[`, "oof_pred"))
  colnames(oof_mat_transformed) <- names(ok_methods)

  # Back-transform OOF predictions for reporting and downstream use
  oof_mat <- if (identical(outcome_transform, "log1p")) {
    apply(oof_mat_transformed, 2, function(p) pmax(0, expm1(p)))
  } else {
    apply(oof_mat_transformed, 2, pmax, 0)
  }

  weights <- fit_super_learner_weights(oof_mat_transformed, y_all)
  names(weights) <- colnames(oof_mat_transformed)

  oof_ensemble_transformed <- as.numeric(
    oof_mat_transformed[, names(weights), drop = FALSE] %*% weights
  )
  oof_ensemble_pred <- if (identical(outcome_transform, "log1p")) {
    pmax(0, expm1(oof_ensemble_transformed))
  } else {
    pmax(0, oof_ensemble_transformed)
  }

  report_progress(
    progress,
    "[Alchemist] OOF ensemble RMSE: ",
    round(sqrt(mean((oof_ensemble_pred - y_raw)^2, na.rm = TRUE)), 4),
    " (", length(ok_methods), "/", length(methods), " methods succeeded)"
  )

  # ---- Final refit on full training set ------------------------------------
  refit_methods <- names(weights)
  report_progress(
    progress,
    "[Alchemist] Refitting ", length(refit_methods), " base learner(s) on full ",
    nrow(training_data), " pairs",
    if (workers > 1L) paste0(" (", min(workers, length(refit_methods)), " workers)") else "",
    "..."
  )

  run_refit <- function(m) {
    tryCatch(
      fit_base_learner(
        x_train = x_all,
        y_train = y_all,
        method = m,
        method_settings = method_settings,
        seed = seed,
        lambda_rule = lambda_rule
      ),
      error = function(e) NULL
    )
  }

  if (workers > 1L && length(refit_methods) > 1L) {
    cl_refit <- initialize_parallel_cluster(workers = min(workers, length(refit_methods)))
    if (!is.null(cl_refit)) {
      on.exit(parallel::stopCluster(cl_refit), add = TRUE)
      refit_list <- parallel::parLapplyLB(cl_refit, refit_methods, run_refit)
    } else {
      refit_list <- lapply(refit_methods, run_refit)
    }
  } else {
    refit_list <- lapply(refit_methods, run_refit)
  }
  names(refit_list) <- refit_methods

  final_learners <- Filter(Negate(is.null), refit_list)

  refit_failed <- setdiff(refit_methods, names(final_learners))
  if (length(refit_failed) > 0L) {
    report_progress(
      progress,
      "[Alchemist]   Refit failed for: ", paste(refit_failed, collapse = ", ")
    )
  }

  weights <- weights[names(final_learners)]
  if (length(final_learners) == 0L || length(weights) == 0L) {
    stop("No Alchemist base learners could be refit on the full training data.", call. = FALSE)
  }
  weights <- weights / sum(weights)

  # ---- Performance table ---------------------------------------------------
  perf_tbl <- dplyr::bind_rows(
    lapply(colnames(oof_mat), function(m) {
      p <- oof_mat[, m]
      tibble::tibble(
        method = m,
        rmse = sqrt(mean((p - y_raw)^2, na.rm = TRUE)),
        mae = mean(abs(p - y_raw), na.rm = TRUE),
        weight = weights[m] %||% NA_real_
      )
    }),
    tibble::tibble(
      method = "ensemble",
      rmse = sqrt(mean((oof_ensemble_pred - y_raw)^2, na.rm = TRUE)),
      mae = mean(abs(oof_ensemble_pred - y_raw), na.rm = TRUE),
      weight = 1
    )
  )

  structure(
    list(
      fit = final_learners,
      weights = weights,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      inner_fold_splits = fold_splits,
      oof_predictions = tibble::as_tibble(oof_mat),
      oof_ensemble_prediction = oof_ensemble_pred,
      oof_performance = perf_tbl
    ),
    class = "SuperLearner"
  )
}

#' Generate predictions from a fitted Alchemist ensemble
#'
#' Applies each base learner in the ensemble to `new_data`, combines the
#' predictions using the stored NNLS weights, and back-transforms the result to
#' the original outcome scale.
#'
#' @param object A `"SuperLearner"` object from
#'   [fit_super_learner()].
#' @param new_data Tibble or data frame containing at least the columns named by
#'   `object$feature_cols`.
#'
#' @return Numeric prediction vector, length `nrow(new_data)`, in the original
#'   (non-transformed) scale.
#'
#' @keywords internal
predict_distance <- function(object, new_data) {
  if (inherits(object, "Mahalanobis")) {
    new_data <- tibble::as_tibble(new_data)
    x_new <- feature_matrix(new_data, object$feature_cols)
    return(sqrt(pmax(0, as.numeric(x_new %*% object$weights))))
  }
  if (!inherits(object, "SuperLearner")) {
    stop(
      "'object' must be a 'tsb_alchemist_learner' or 'Mahalanobis'.",
      call. = FALSE
    )
  }
  new_data <- tibble::as_tibble(new_data)
  x_new <- feature_matrix(new_data, object$feature_cols)
  weights <- object$weights

  pred_mat <- vapply(names(object$fit), function(m) {
    predict_base_learner(object$fit[[m]], x_new)
  }, numeric(nrow(x_new)))

  if (is.null(dim(pred_mat))) {
    dim(pred_mat) <- c(nrow(x_new), length(names(object$fit)))
    colnames(pred_mat) <- names(object$fit)
  }

  pred_transformed <- as.numeric(
    pred_mat[, names(weights), drop = FALSE] %*% weights
  )
  pmax(0, if (identical(object$outcome_transform, "log1p")) {
    expm1(pred_transformed)
  } else {
    pred_transformed
  })
}

# Fit a diagonal Mahalanobis distance via NNLS on squared pairwise differences.
# Predicts e_ij^2 = sum_k w_k * delta_k^2; distance = sqrt(predicted value).
fit_mahalanobis <- function(training_data, feature_cols,
                            progress = FALSE) {
  x_all <- feature_matrix(training_data, feature_cols)
  y_raw <- training_data$.outcome
  y_sq <- y_raw^2

  report_progress(
    progress,
    "[Alchemist] Fitting diagonal Mahalanobis (NNLS) on ",
    nrow(x_all), " pairs x ", length(feature_cols), " features..."
  )

  result <- nnls::nnls(x_all, y_sq)
  weights <- result$x
  names(weights) <- feature_cols

  oof_pred <- sqrt(pmax(0, as.numeric(x_all %*% weights)))
  rmse_val <- sqrt(mean((oof_pred - y_raw)^2, na.rm = TRUE))
  mae_val <- mean(abs(oof_pred - y_raw), na.rm = TRUE)

  report_progress(
    progress,
    "[Alchemist] Mahalanobis fit: RMSE=", round(rmse_val, 4),
    " | MAE=", round(mae_val, 4),
    " | non-zero weights: ",
    sum(weights > 0), "/", length(weights)
  )

  structure(
    list(
      weights = weights,
      feature_cols = feature_cols,
      oof_predictions = oof_pred,
      oof_performance = tibble::tibble(
        method = "mahalanobis_nnls",
        rmse = rmse_val,
        mae = mae_val
      )
    ),
    class = "Mahalanobis"
  )
}

# - forge_distances -

S7::method(forge_distances, Alchemist) <- function(object,
                                                   progress = NULL,
                                                   feature_type = NULL,
                                                   ...) {
  config <- object@config
  progress <- progress %||% config$progress %||% FALSE

  models_df <- tibble::as_tibble(object@candidates@candidate_models)
  n_models <- nrow(models_df)
  if (n_models < 3L) {
    stop("At least 3 candidate models are required for distance learning.", call. = FALSE)
  }

  sp_names <- alchemist_trait_names(config$species_traits %||% list(), models_df)
  st_names <- alchemist_trait_names(config$study_traits %||% list(), models_df)

  report_progress(
    progress,
    "[Alchemist] forge_distances: ", n_models, " candidate models | ",
    length(sp_names), " species traits [",
    paste(sp_names, collapse = ", "), "] | ",
    length(st_names), " study traits [",
    paste(st_names, collapse = ", "), "]"
  )

  coherence_cfg <- config$coherence %||% list()
  taxonomic_distance <- isTRUE(config$taxonomic_distance)
  feature_type <- feature_type %||% config$feature_type %||% "gower"

  report_progress(
    progress,
    "[Alchemist] Pairwise feature type: ",
    switch(feature_type,
      difference = "signed standardized differences [(t_i - t_j) / sd(t); categorical: Gower]",
      mahalanobis = "squared standardized differences [(t_i - t_j)^2 / sd(t)^2; fit via NNLS -> diagonal Mahalanobis]",
      "Gower distances [|t_i - t_j| / range(t)]"
    )
  )

  has_coh <- any(vapply(
    c("length", "depth", "frequency"),
    function(k) !identical(as.character(coherence_cfg[[k]]$mode %||% "none"), "none"),
    logical(1)
  ))

  report_progress(
    progress,
    "[Alchemist] Stage 1/4: Building pairwise training data ",
    "(up to ~", n_models * (n_models - 1L), " cross-species pairs",
    if (has_coh) " + coherence features" else "",
    if (taxonomic_distance) " + taxonomic distance" else "",
    ")..."
  )
  pair_data <- build_pair_data(
    models_df,
    sp_names,
    st_names,
    coherence_cfg = if (has_coh) coherence_cfg else NULL,
    taxonomic_distance = taxonomic_distance,
    feature_type = feature_type,
    progress = progress
  )
  n_pairs <- nrow(pair_data$training_data)

  learner_cfg <- config$learner %||% list()
  methods_lbl <- learner_cfg$methods %||% NULL
  folds_lbl <- as.integer(learner_cfg$inner_folds %||% 5L)
  workers_lbl <- as.integer(learner_cfg$workers %||% 1L)
  oof_mode_lbl <- normalize_alchemist_oof_mode(learner_cfg$oof_mode %||% "anchor_species")

  if (identical(feature_type, "mahalanobis")) {
    report_progress(
      progress,
      "[Alchemist] Stage 2/4: Fitting diagonal Mahalanobis (NNLS) on ", n_pairs,
      " pairs..."
    )
    sl_fit <- fit_mahalanobis(
      training_data = pair_data$training_data,
      feature_cols = pair_data$feature_cols,
      progress = progress
    )
  } else {
    report_progress(
      progress,
      "[Alchemist] Stage 2/4: Fitting Alchemist ensemble on ", n_pairs,
      " pairs (", folds_lbl, "-fold CV, ", workers_lbl, " worker(s))..."
    )
    sl_fit <- fit_super_learner(
      training_data = pair_data$training_data,
      feature_cols = pair_data$feature_cols,
      methods = methods_lbl %||% c(
        "glmnet_elasticnet", "ranger_shallow", "xgboost_conservative"
      ),
      outcome_transform = learner_cfg$outcome_transform %||% "identity",
      lambda_rule = learner_cfg$lambda_rule %||% "lambda.1se",
      inner_folds = folds_lbl,
      seed = if (!is.null(learner_cfg$seed)) as.integer(learner_cfg$seed) else NULL,
      method_settings = learner_cfg$method_settings %||% NULL,
      oof_mode = oof_mode_lbl,
      workers = workers_lbl,
      progress = progress
    )
  }

  # Reconstruct NxN matrix: use OOF predictions for honest distance estimates
  report_progress(
    progress,
    "[Alchemist] Stage 3/4: Reconstructing ", n_models, "x", n_models,
    " distance matrix from OOF predictions..."
  )
  n <- pair_data$n_models
  model_ids <- pair_data$model_ids
  dist_mat <- matrix(NA_real_, nrow = n, ncol = n)
  rownames(dist_mat) <- model_ids
  colnames(dist_mat) <- model_ids
  diag(dist_mat) <- 0

  oof_preds <- sl_fit$oof_ensemble_prediction %||%
    sl_fit$oof_predictions %||%
    predict_distance(sl_fit, pair_data$training_data)
  oof_preds <- pmax(0, oof_preds)

  pair_rows <- pair_data$training_data
  for (row_k in seq_len(nrow(pair_rows))) {
    i_idx <- pair_rows$.donor_idx[[row_k]]
    j_idx <- pair_rows$.anchor_idx[[row_k]]
    dist_mat[[i_idx, j_idx]] <- oof_preds[[row_k]]
  }

  # Fill any remaining NAs (same-species or missing-PDF pairs) with the
  # column max so they sort to the far end of the distance ordering
  col_max <- apply(dist_mat, 2, function(col) max(col[is.finite(col)], na.rm = TRUE))
  col_max[!is.finite(col_max)] <- 1
  for (j in seq_len(n)) {
    na_idx <- which(is.na(dist_mat[, j]))
    dist_mat[na_idx, j] <- col_max[[j]]
  }

  report_progress(
    progress,
    "[Alchemist] Stage 4/4: Symmetrizing distance matrix..."
  )
  sym_mat <- (dist_mat + t(dist_mat)) / 2
  diag(sym_mat) <- 0

  dist_range <- round(range(sym_mat[sym_mat > 0], na.rm = TRUE), 4)
  report_progress(
    progress,
    "[Alchemist] forge_distances complete. Distance range: [",
    dist_range[[1]], ", ", dist_range[[2]], "]"
  )

  distance_matrix <- list(
    combined_dist = stats::as.dist(sym_mat),
    dist_matrix = sym_mat,
    model_ids = model_ids,
    all_traits = pair_data$all_traits,
    species_trait_names = sp_names,
    species_feature_cols = pair_data$species_feature_cols,
    trait_cols = pair_data$all_traits,
    oof_performance = sl_fit$oof_performance %||% tibble::tibble(),
    pair_data = pair_data$training_data,
    feature_cols = pair_data$feature_cols,
    trait_mats = pair_data$trait_mats,
    feature_type = feature_type,
    donor_sigma_matrix = pair_data$donor_sigma_matrix,
    target_sigma = pair_data$target_sigma
  )

  # A fresh learned distance matrix invalidates every downstream artifact that
  # depends on that geometry. Re-running forge_distances() on an existing
  # Alchemist must therefore clear trait importance, ordination, and
  # admissibility rather than silently carrying stale results forward.
  object <- alchemist_rebuild(
    object,
    distance_matrix = distance_matrix,
    trait_importance = list(),
    ordination = list(),
    admissibility = list()
  )
  alchemist_rebuild(object, learner = sl_fit)
}

# - distill_traits helpers -

#' Evaluate kernel-weighted sigma RMSE from predicted pairwise distances
#'
#' Reconstructs an nxn distance matrix from (donor_idx, anchor_idx, dist_vec)
#' triplets, then computes the log sigma RMSE of the kernel-weighted ensemble
#' sigma prediction - the same objective used by [score_similarity_basis()] in
#' the similarity tuning step.
#'
#' @param anchor_idx Integer vector of anchor indices (column into sigma matrix).
#' @param donor_idx Integer vector of donor indices (row into sigma matrix).
#' @param dist_vec Numeric vector of predicted distances, aligned with the index
#'   vectors.
#' @param donor_sigma_mat nxn matrix; `[i, j]` = sigma of donor equation i
#'   evaluated at anchor j's length PDF.
#' @param target_sigma Length-n numeric vector of each model's own sigma.
#' @param scale Positive numeric kernel bandwidth. Distances are fed into
#'   `exp(-dist / scale)`; pairs absent from pair_data receive `Inf` distance
#'   (weight 0).
#'
#' @return Scalar RMSE, or `NA_real_` when no valid anchors remain.
#'
#' @keywords internal
eval_sigma_rmse <- function(anchor_idx, donor_idx, dist_vec,
                            donor_sigma_mat, target_sigma, scale) {
  n <- length(target_sigma)
  dist_mat <- matrix(Inf, nrow = n, ncol = n)
  diag(dist_mat) <- 0
  dist_mat[cbind(donor_idx, anchor_idx)] <- pmax(0, dist_vec)

  w_raw <- exp(-dist_mat / scale)
  w_raw[!is.finite(w_raw)] <- 0
  diag(w_raw) <- 0

  w_sum <- colSums(w_raw, na.rm = TRUE)
  valid <- which(
    is.finite(target_sigma) & target_sigma > 0 &
      is.finite(w_sum) & w_sum > 0
  )
  if (length(valid) == 0L) {
    return(NA_real_)
  }

  w <- sweep(w_raw[, valid, drop = FALSE], 2, w_sum[valid], "/")
  pred <- colSums(donor_sigma_mat[, valid, drop = FALSE] * w, na.rm = TRUE)
  keep <- is.finite(pred) & pred > 0 &
    is.finite(target_sigma[valid]) & target_sigma[valid] > 0
  if (!any(keep)) {
    return(NA_real_)
  }

  errs <- log(pred[keep]) - log(target_sigma[valid][keep])
  sqrt(mean(errs^2))
}

#' Run sigma-dropout sensitivity for one trait column
#'
#' Sets the named feature column to zero in the pair data (equivalent to
#' removing that trait's contribution to pairwise distance), re-predicts
#' distances with the trained learner, and evaluates the change in
#' kernel-weighted sigma RMSE relative to the baseline.  This mirrors the
#' component-dropout logic in [score_dropout_task()], applied to the
#' Alchemist's learned distance function.
#'
#' @param fc Character. Feature column to zero out.
#' @param pair_data Tibble of training pairs.
#' @param anchor_idx Integer vector of anchor indices from `pair_data$.anchor_idx`.
#' @param donor_idx Integer vector of donor indices from `pair_data$.donor_idx`.
#' @param sl_fit A `"SuperLearner"` or `"Mahalanobis"` learner object.
#' @param donor_sigma_mat nxn donor sigma matrix from [build_pair_data()].
#' @param target_sigma Length-n target sigma vector.
#' @param baseline_sigma_rmse Scalar baseline sigma RMSE with no feature zeroed.
#' @param scale Kernel bandwidth passed to [eval_sigma_rmse()].
#'
#' @return A one-row [tibble::tibble()] with columns `trait`, `feature_col`,
#'   `delta_rmse`.
#'
#' @keywords internal
dropout_trait <- function(fc, pair_data, anchor_idx, donor_idx,
                          sl_fit, donor_sigma_mat, target_sigma,
                          baseline_sigma_rmse, scale) {
  zero_data <- pair_data
  zero_data[[fc]] <- 0
  dist_vec <- predict_distance(sl_fit, zero_data)
  dropout_rmse <- eval_sigma_rmse(
    anchor_idx, donor_idx, dist_vec,
    donor_sigma_mat, target_sigma, scale
  )
  tibble::tibble(
    trait = sub("^\\.dist_", "", fc),
    feature_col = fc,
    delta_rmse = (dropout_rmse %||% NA_real_) - baseline_sigma_rmse
  )
}

#' Evaluate dropout importance for a group of related feature columns
#'
#' Zeros all columns in `fcs` simultaneously so that one-hot dummy columns
#' belonging to the same categorical trait are dropped together. Returns the
#' change in kernel-weighted sigma RMSE relative to the full-feature baseline.
#'
#' @param fcs Character vector of feature column names to zero.
#' @param trait_name Display name for this trait group.
#' @param pair_data,anchor_idx,donor_idx,sl_fit,donor_sigma_mat,target_sigma,baseline_sigma_rmse,scale
#'   Passed through from [distill_traits()].
#'
#' @return A one-row [tibble::tibble()] with columns `trait`, `feature_col`,
#'   `delta_rmse`.
#'
#' @keywords internal
dropout_trait_group <- function(fcs, trait_name, pair_data, anchor_idx, donor_idx,
                                sl_fit, donor_sigma_mat, target_sigma,
                                baseline_sigma_rmse, scale) {
  zero_data <- pair_data
  for (fc in fcs) zero_data[[fc]] <- 0
  dist_vec <- predict_distance(sl_fit, zero_data)
  dropout_rmse <- eval_sigma_rmse(
    anchor_idx, donor_idx, dist_vec,
    donor_sigma_mat, target_sigma, scale
  )
  tibble::tibble(
    trait = trait_name,
    feature_col = paste(fcs, collapse = ", "),
    delta_rmse = (dropout_rmse %||% NA_real_) - baseline_sigma_rmse
  )
}

# - distill_traits -

S7::method(distill_traits, Alchemist) <- function(object, kernel_scale = 1,
                                                  workers = NULL, progress = NULL, ...) {
  if (length(object@learner) == 0L) {
    stop("Run `forge_distances()` before `distill_traits()`.", call. = FALSE)
  }
  if (length(object@distance_matrix) == 0L) {
    stop("Run `forge_distances()` before `distill_traits()`.", call. = FALSE)
  }

  donor_sigma_mat <- object@distance_matrix$donor_sigma_matrix
  target_sigma <- object@distance_matrix$target_sigma
  if (is.null(donor_sigma_mat) || is.null(target_sigma)) {
    stop(
      "`donor_sigma_matrix` not found in distance_matrix - re-run `forge_distances()`.",
      call. = FALSE
    )
  }

  progress <- progress %||% object@config$progress %||% FALSE
  workers <- as.integer(workers %||% object@config$distill_workers %||% 1L)

  sl_fit <- object@learner
  pair_data <- object@distance_matrix$pair_data
  feature_cols <- object@distance_matrix$feature_cols
  anchor_idx <- pair_data$.anchor_idx
  donor_idx <- pair_data$.donor_idx

  # Group one-hot dummy columns back to their parent trait so that all levels
  # of a categorical (e.g. season__summer, season__fall) are zeroed together.
  parent_names <- sub("__.*$", "", sub("^\\.dist_", "", feature_cols))
  trait_groups <- split(feature_cols, parent_names)
  n_groups <- length(trait_groups)
  n_features <- length(feature_cols)

  report_progress(
    progress,
    "[Alchemist] distill_traits: sigma dropout sensitivity over ",
    n_groups, " traits (", n_features, " feature columns, ", nrow(pair_data), " pairs)..."
  )

  # Baseline: full model, kernel-weighted sigma RMSE
  baseline_dist <- predict_distance(sl_fit, pair_data)
  med_d <- stats::median(
    baseline_dist[is.finite(baseline_dist) & baseline_dist > 0],
    na.rm = TRUE
  )
  if (!is.finite(med_d) || med_d <= 0) med_d <- 1
  scale_param <- med_d * kernel_scale
  baseline_sigma_rmse <- eval_sigma_rmse(
    anchor_idx, donor_idx, baseline_dist,
    donor_sigma_mat, target_sigma, scale_param
  )

  report_progress(
    progress,
    "[Alchemist] Baseline sigma RMSE: ", round(baseline_sigma_rmse, 4),
    " (kernel_scale = ", kernel_scale, ", bandwidth = ", round(scale_param, 4), ")"
  )

  use_parallel <- workers > 1L && n_groups > 1L
  cl <- NULL
  if (use_parallel) {
    cl <- initialize_parallel_cluster(workers = min(workers, n_groups))
    if (!is.null(cl)) {
      on.exit(parallel::stopCluster(cl), add = TRUE)
      tsb_cluster_export(
        cl,
        varlist = c(
          "pair_data", "anchor_idx", "donor_idx",
          "sl_fit", "donor_sigma_mat", "target_sigma",
          "baseline_sigma_rmse", "scale_param",
          "trait_groups"
        ),
        envir = environment()
      )
      report_progress(
        progress,
        "[Alchemist] Cluster ready: ", min(workers, n_groups), " workers."
      )
    }
  }

  trait_names_ordered <- names(trait_groups)

  run_dropout_group <- function(trait_name) {
    fcs <- trait_groups[[trait_name]]
    report_progress(
      progress,
      "[Alchemist]   Dropout: ", trait_name,
      if (length(fcs) > 1L) paste0(" [", length(fcs), " levels]") else "",
      "..."
    )
    dropout_trait_group(
      fcs = fcs,
      trait_name = trait_name,
      pair_data = pair_data,
      anchor_idx = anchor_idx,
      donor_idx = donor_idx,
      sl_fit = sl_fit,
      donor_sigma_mat = donor_sigma_mat,
      target_sigma = target_sigma,
      baseline_sigma_rmse = baseline_sigma_rmse,
      scale = scale_param
    )
  }

  importance_rows <- if (!is.null(cl)) {
    parallel::parLapplyLB(cl, trait_names_ordered, fun = function(trait_name) {
      fcs <- trait_groups[[trait_name]]
      dropout_trait_group <- utils::getFromNamespace("dropout_trait_group", "tsbiomass")
      dropout_trait_group(
        fcs = fcs,
        trait_name = trait_name,
        pair_data = pair_data,
        anchor_idx = anchor_idx,
        donor_idx = donor_idx,
        sl_fit = sl_fit,
        donor_sigma_mat = donor_sigma_mat,
        target_sigma = target_sigma,
        baseline_sigma_rmse = baseline_sigma_rmse,
        scale = scale_param
      )
    })
  } else {
    lapply(trait_names_ordered, run_dropout_group)
  }

  importance_tbl <- dplyr::bind_rows(importance_rows) |>
    dplyr::arrange(dplyr::desc(.data$delta_rmse)) |>
    dplyr::mutate(
      importance_score = pmax(0, delta_rmse),
      importance_pct = importance_score / max(sum(importance_score, na.rm = TRUE), 1e-12)
    )

  sp_feature_cols <- object@distance_matrix$species_feature_cols %||% character(0)
  sp_names <- object@distance_matrix$species_trait_names %||% character(0)
  importance_tbl$trait_set <- vapply(
    importance_tbl$trait,
    function(tn) {
      fcs_for_trait <- trait_groups[[tn]] %||% character(0)
      if (any(fcs_for_trait %in% sp_feature_cols) || tn %in% sp_names) "species" else "survey"
    },
    character(1L)
  )

  sp_total <- sum(importance_tbl$importance_score[importance_tbl$trait_set == "species"], na.rm = TRUE)
  all_total <- sum(importance_tbl$importance_score, na.rm = TRUE)
  alpha_equiv <- if (all_total > 0) sp_total / all_total else 0.5

  report_progress(
    progress,
    "[Alchemist] distill_traits complete. alpha-equivalent = ",
    round(alpha_equiv, 3),
    " (species: ", round(sp_total / max(all_total, 1e-12) * 100, 1), "%, ",
    "survey: ", round((all_total - sp_total) / max(all_total, 1e-12) * 100, 1), "%)"
  )

  trait_importance <- list(
    importance_tbl = importance_tbl,
    alpha_equiv = alpha_equiv,
    species_total = sp_total,
    survey_total = all_total - sp_total,
    baseline_sigma_rmse = baseline_sigma_rmse,
    kernel_scale = kernel_scale
  )

  slim_dm <- object@distance_matrix
  slim_dm$pair_data <- NULL
  slim_dm$trait_mats <- NULL
  alchemist_rebuild(
    object,
    trait_importance = trait_importance,
    learner = list(),
    distance_matrix = slim_dm
  )
}

# - run_ordination dispatch -

#' @keywords internal
.run_ordination_alchemist <- function(alchemist,
                                      nmds_args = NULL,
                                      include_loadings = NULL,
                                      include_centroids = NULL,
                                      envfit_args = NULL,
                                      reference_ids = NULL,
                                      join_cols = c(
                                        "species_name", "common",
                                        "swimbladder_type", "family",
                                        "regional_body", "is_group_model"
                                      ),
                                      cluster_args = list(),
                                      model_id_col = "model_id",
                                      progress = NULL) {
  config <- alchemist@config
  progress <- progress %||% config$progress %||% FALSE

  if (length(alchemist@distance_matrix) == 0L) {
    stop("Run `forge_distances()` before `run_ordination()`.", call. = FALSE)
  }

  cfg_data <- config$config_data %||% list()
  ordination_cfg <- cfg_data$ordination %||% list()
  nmds_args <- nmds_args %||% ordination_cfg$nmds_args %||% list()
  envfit_args <- envfit_args %||% ordination_cfg$envfit_args %||% list()
  include_loadings <- include_loadings %||% ordination_cfg$include_loadings %||% FALSE
  include_centroids <- include_centroids %||% ordination_cfg$include_centroids %||% FALSE

  report_progress(progress, "Running Alchemist ordination.")

  combined_dist <- alchemist@distance_matrix$combined_dist
  candidate_models <- tibble::as_tibble(alchemist@candidates@candidate_models)
  all_traits <- alchemist@distance_matrix$all_traits %||% character(0)
  trait_cols <- intersect(all_traits, names(candidate_models))

  model_trait_table <- if (length(trait_cols) > 0) {
    tbl <- dplyr::select(candidate_models, dplyr::all_of(trait_cols))
    if ("species" %in% names(tbl) && "genus" %in% names(candidate_models)) {
      g <- trimws(as.character(candidate_models[["genus"]]))
      s <- trimws(as.character(tbl[["species"]]))
      combined <- paste(g, s)
      combined[is.na(g) | g == "NA" | is.na(s) | s == "NA"] <- NA_character_
      tbl[["species"]] <- combined
    }
    tbl
  } else {
    NULL
  }

  # Coherence distances are pairwise and cannot be used directly by envfit.
  # Instead, add the per-model scalar columns that drive each configured
  # coherence dimension so they appear as continuous vectors in the ordination.
  # The set of columns depends on the `source` field in each coherence dimension:
  #   "best"    - whichever of study/species is available (study preferred)
  #   "study"   - study-level columns only
  #   "species" - species-level columns only
  #   "both"    - all available columns from both sources
  coherence_cfg <- config$coherence %||% list()
  coh_cols_for_dim <- function(mode, source, study_cols, sp_cols) {
    if (identical(as.character(mode %||% "none"), "none")) {
      return(character(0))
    }
    all_avail <- names(candidate_models)
    switch(as.character(source %||% "best"),
      study = intersect(study_cols, all_avail),
      species = intersect(sp_cols, all_avail),
      both = intersect(c(study_cols, sp_cols), all_avail),
      {
        # "best": first available study column, or fall back to species column
        s_match <- intersect(study_cols, all_avail)
        sp_match <- intersect(sp_cols, all_avail)
        if (length(s_match) > 0L) s_match else sp_match
      }
    )
  }
  coh_cols <- unique(c(
    coh_cols_for_dim(
      coherence_cfg$length$mode, coherence_cfg$length$source,
      c("study_length_min", "study_length_max"),
      c("species_length_min", "species_length_max")
    ),
    coh_cols_for_dim(
      coherence_cfg$depth$mode, coherence_cfg$depth$source,
      c("study_depth_min", "study_depth_max"),
      c("species_depth_min", "species_depth_max")
    ),
    coh_cols_for_dim(
      coherence_cfg$frequency$mode, NULL,
      "frequency", "frequency"
    )
  ))
  if (length(coh_cols) > 0L) {
    coh_extra <- dplyr::select(candidate_models, dplyr::all_of(coh_cols)) |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        ~ suppressWarnings(as.numeric(.x))
      ))
    model_trait_table <- if (!is.null(model_trait_table)) {
      dplyr::bind_cols(model_trait_table, coh_extra)
    } else {
      coh_extra
    }
  }

  if (is.null(reference_ids) && nrow(alchemist@candidates@reference_anchors) > 0) {
    anch <- alchemist@candidates@reference_anchors
    reference_ids <- if ("model_id_chr" %in% names(anch)) {
      as.character(anch$model_id_chr)
    } else if ("model_id" %in% names(anch)) {
      as.character(anch$model_id)
    } else {
      NULL
    }
  }

  model_cluster_col <- as.character(cluster_args$cluster_col %||% "nmds_cluster_id")

  model_ord <- run_ordination(
    dist_mat = combined_dist,
    trait_table = model_trait_table,
    nmds_args = nmds_args,
    include_loadings = include_loadings,
    include_centroids = include_centroids,
    envfit_args = envfit_args
  )

  model_points <- join_ordination_points(
    ordination_points = model_ord$points,
    candidate_models = candidate_models,
    reference_ids = reference_ids,
    model_id_col = model_id_col,
    join_cols = join_cols,
    cluster_args = cluster_args
  )

  model_scores <- extract_ordination_scores(
    points_df = model_points,
    cluster_col = model_cluster_col
  )

  model_hulls <- tryCatch(
    build_ordination_hulls(model_points, cluster_col = model_cluster_col),
    error = function(e) tibble::tibble()
  )

  ordination <- list(
    model = list(
      ordination = model_ord$ordination,
      points = tibble::as_tibble(model_points),
      loadings = model_ord$loadings %||% tibble::tibble(),
      centroids = model_ord$centroids %||% tibble::tibble(),
      scores = tibble::as_tibble(model_scores),
      hulls = tibble::as_tibble(model_hulls)
    )
  )

  alchemist_rebuild(alchemist, ordination = ordination)
}

# - screen_admissibility dispatch -

#' @keywords internal
.screen_admissibility_alchemist <- function(alchemist,
                                            config = NULL,
                                            cache_path = NULL,
                                            refresh = NULL,
                                            progress = NULL,
                                            registry_path = NULL) {
  if (length(alchemist@distance_matrix) == 0L) {
    stop("Run `forge_distances()` before `screen_admissibility()`.", call. = FALSE)
  }

  dm <- alchemist@distance_matrix

  gower_bundle <- list(
    combined_dist = dm$combined_dist,
    species_dist = dm$combined_dist,
    study_dist = as.matrix(dm$dist_matrix),
    species_dist_model = as.matrix(dm$dist_matrix),
    trait_cols = dm$trait_cols %||% dm$all_traits %||% character(0)
  )

  injected_candidates <- candidates_with_gower_distances(
    alchemist@candidates,
    gower_bundle
  )

  result <- screen_admissibility(
    candidate_models = injected_candidates,
    config = config %||% alchemist@config$config_data %||% NULL,
    cache_path = cache_path,
    refresh = refresh,
    progress = progress %||% alchemist@config$progress %||% FALSE,
    registry_path = registry_path %||% alchemist@config$registry_path %||% NULL
  )

  admissibility_result <- if (.is_candidates_obj(result)) {
    result@admissibility
  } else {
    result
  }

  alchemist_rebuild(alchemist, admissibility = admissibility_result)
}

# - as_selector -

#' Coerce a staged object to a PolicySelector
#'
#' Dispatches on the class of `object`:
#' - [Candidates] objects are passed directly to [as_policy_selector()].
#' - [Alchemist] objects inject the learned distance matrix into a `Candidates`
#'   shell before constructing the selector, so all downstream benchmark and
#'   prediction steps operate on the Alchemist distances transparently.
#'
#' @param object A [Candidates] or [Alchemist] object.
#' @param config Optional selector config list or [Configurer] object.
#' @param ... Unused.
#'
#' @return A [PolicySelector] object.
#'
#' @export
as_selector <- function(object, config = NULL, ...) {
  if (.is_alchemist(object)) {
    dm <- object@distance_matrix
    if (length(dm) == 0L) {
      stop("Run `forge_distances()` before `as_selector()`.", call. = FALSE)
    }

    gower_bundle <- list(
      combined_dist = dm$combined_dist,
      species_dist = dm$combined_dist,
      study_dist = as.matrix(dm$dist_matrix),
      species_dist_model = as.matrix(dm$dist_matrix),
      trait_cols = dm$trait_cols %||% dm$all_traits %||% character(0)
    )

    candidates_obj <- candidates_with_gower_distances(object@candidates, gower_bundle)

    if (length(object@admissibility) > 0L) {
      candidates_obj <- candidates_with_admissibility(candidates_obj, object@admissibility)
    }
    if (length(object@ordination) > 0L) {
      candidates_obj <- candidates_with_ordination(candidates_obj, object@ordination)
    }

    return(as_policy_selector(candidates_obj, config = config))
  }

  if (.is_candidates_obj(object)) {
    return(as_policy_selector(object, config = config))
  }

  stop("'object' must be a `Candidates` or `Alchemist`.", call. = FALSE)
}

# - print / show -

S7::method(print_generic, Alchemist) <- function(x, ...) {
  cat("Alchemist\n")
  cat("  candidates:       ", nrow(x@candidates@candidate_models), " models\n", sep = "")
  cat("  species_traits:   ", length(alchemist_trait_names(x@config$species_traits %||% list(), x@candidates@candidate_models)), "\n", sep = "")
  cat("  study_traits:     ", length(alchemist_trait_names(x@config$study_traits %||% list(), x@candidates@candidate_models)), "\n", sep = "")
  cat("  learner_ready:    ", if (length(x@learner) > 0L) "yes" else "no", "\n", sep = "")
  cat("  distances_ready:  ", if (length(x@distance_matrix) > 0L) "yes" else "no", "\n", sep = "")
  cat("  importance_ready: ", if (length(x@trait_importance) > 0L) "yes" else "no", "\n", sep = "")
  cat("  ordination_ready: ", if (length(x@ordination) > 0L) "yes" else "no", "\n", sep = "")
  cat("  admiss_ready:     ", if (length(x@admissibility) > 0L) "yes" else "no", "\n", sep = "")
  invisible(x)
}

S7::method(show_generic, Alchemist) <- function(object) {
  print(object)
  invisible(object)
}

# - plot dispatch -

#' Plot an `Alchemist`
#'
#' Dispatches `plot(alchemist, ...)` to one of several figure families based
#' on `type`. Mirrors the `plot.Candidates` interface so both object types
#' behave consistently at the REPL.
#'
#' @name plot.Alchemist
#'
#' @param x An [Alchemist] object.
#' @param y Unused.
#' @param type Figure family to draw. `"ordination"` requires a prior call to
#'   [run_ordination()]; `"trait_importance"` requires [distill_traits()];
#'   `"admissibility"` requires [screen_admissibility()].
#' @param view Secondary plot selector for `type = "ordination"` or
#'   `type = "admissibility"`. One of `"clusters"`, `"cluster_hulls"`,
#'   `"vectors"`, or `"centers"` for ordination; one of `"gate_composition"`
#'   or `"overlap_profile"` for admissibility.
#' @param include_hulls Logical. When `TRUE` (default) and hull data are
#'   available, `view = NULL` defaults to `"cluster_hulls"` for `type =
#'   "ordination"`.
#' @param ... Unused additional arguments.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' alchemist <- run_ordination(alchemist)
#' plot(alchemist)
#' plot(alchemist, type = "ordination", view = "vectors")
#' alchemist <- distill_traits(alchemist)
#' plot(alchemist, type = "trait_importance")
#' }
.plot_alchemist <- function(x,
                            y = NULL,
                            type = c("ordination", "trait_importance", "admissibility"),
                            view = NULL,
                            include_hulls = TRUE,
                            ...) {
  type <- match.arg(type)

  if (identical(type, "ordination")) {
    if (length(x@ordination) == 0L) {
      stop(
        "No ordination results stored. Run `run_ordination()` first.",
        call. = FALSE
      )
    }
    model_obj <- x@ordination$model %||% list()
    point_tbl <- tibble::as_tibble(model_obj$points %||% tibble::tibble())
    hull_tbl <- tibble::as_tibble(model_obj$hulls %||% tibble::tibble())
    loading_tbl <- tibble::as_tibble(model_obj$loadings %||% tibble::tibble())
    centroid_tbl <- tibble::as_tibble(model_obj$centroids %||% tibble::tibble())

    ord_view <- match.arg(
      view %||% if (isTRUE(include_hulls) && nrow(hull_tbl) > 0) "cluster_hulls" else "clusters",
      c("clusters", "cluster_hulls", "vectors", "centers")
    )

    if (identical(ord_view, "vectors") && nrow(loading_tbl) > 0) {
      return(plot_ordination_vectors(vec_tbl = loading_tbl, points_tbl = point_tbl))
    }
    if (identical(ord_view, "centers") && nrow(centroid_tbl) > 0) {
      return(plot_ordination_centers(fac_tbl = centroid_tbl, points_tbl = point_tbl))
    }
    if (identical(ord_view, "cluster_hulls") && nrow(hull_tbl) > 0) {
      return(plot_ordination_cluster_hulls(
        points_tbl = point_tbl,
        hull_tbl = hull_tbl,
        cluster_col = "nmds_cluster_id"
      ))
    }
    return(plot_ordination_clusters(
      points_tbl = point_tbl,
      cluster_col = "nmds_cluster_id"
    ))
  }

  if (identical(type, "admissibility")) {
    if (length(x@admissibility) == 0L) {
      stop(
        "No admissibility results stored. Run `screen_admissibility()` first.",
        call. = FALSE
      )
    }
    if (!admissibility_bundle_is_current(x@admissibility, x@config$config_data %||% x@candidates)) {
      stop(
        paste(
          "The stored admissibility bundle on this `Alchemist` object predates the current admissibility gate logic.",
          "Run `screen_admissibility()` again to rebuild it before plotting."
        ),
        call. = FALSE
      )
    }
    adm_view <- match.arg(view %||% "gate_composition", c("gate_composition", "overlap_profile"))
    if (identical(adm_view, "gate_composition")) {
      return(plot_gate_composition(
        x@admissibility$all_gates %||% tibble::tibble(),
        config = x@config$config_data %||% x@candidates
      ))
    }
    return(plot_overlap_heatmap(x@admissibility$all_overlap %||% tibble::tibble()))
  }

  if (identical(type, "trait_importance")) {
    if (length(x@trait_importance) == 0L) {
      stop(
        "No trait importance data stored. Run `distill_traits()` first.",
        call. = FALSE
      )
    }
    imp_tbl <- tibble::as_tibble(x@trait_importance$importance_tbl %||% tibble::tibble())
    if (nrow(imp_tbl) == 0L) {
      stop("Trait importance table is empty.", call. = FALSE)
    }
    trait_levels <- imp_tbl |>
      dplyr::arrange(.data$importance_score) |>
      dplyr::pull(.data$trait)
    imp_tbl <- dplyr::mutate(
      imp_tbl,
      trait = factor(.data$trait, levels = trait_levels),
      trait_set = factor(
        .data$trait_set,
        levels = c("species", "survey"),
        labels = c("Species trait", "Survey trait")
      )
    )
    ggplot2::ggplot(imp_tbl, ggplot2::aes(
      x = .data$trait,
      y = .data$importance_pct * 100,
      fill = .data$trait_set
    )) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = c("Species trait" = "#4575b4", "Survey trait" = "#d73027")) +
      ggplot2::labs(
        x = NULL,
        y = "Sigma dropout sensitivity (%)",
        fill = NULL
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "bottom")
  }
}

S7::method(plot_generic, Alchemist) <- .plot_alchemist
