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
#' @section Properties:
#' - `candidates`: Source [Candidates] object.
#' - `config`: Normalized Alchemist configuration.
#' - `learner`: Fitted distance learner metadata from [forge_distances()].
#' - `distance_matrix`: Learned model-by-model distance bundle from
#'   [forge_distances()].
#' - `trait_importance`: Trait-importance diagnostics from
#'   [distill_traits()].
#' - `ordination`: Ordination results from [run_ordination()].
#' - `admissibility`: Admissibility-screen results from
#'   [screen_admissibility()].
#'
#' @examples
#' \dontrun{
#' alchemist <- as_alchemist(candidates)
#' alchemist <- forge_distances(alchemist)
#' alchemist <- distill_traits(alchemist)
#' alchemist <- run_ordination(alchemist)
#' alchemist <- screen_admissibility(alchemist)
#' selector <- as_policyselector(alchemist)
#' }
#'
#' @name Alchemist-class
#' @usage NULL
#' @aliases Alchemist
NULL

# - Internal predicates -

#' Test whether an object is an `Alchemist`
#'
#' @param x Object to test.
#'
#' @return Logical scalar.
#'
#' @keywords internal
#' @noRd
.is_alchemist <- function(x) {
  is_s7_instance(x, "Alchemist")
}

#' Test whether an object is a `Candidates` instance
#'
#' @param x Object to test.
#'
#' @return Logical scalar.
#'
#' @keywords internal
#' @noRd
.is_candidates_obj <- function(x) {
  is_s7_instance(x, "Candidates")
}

# - Config normalization -

#' Extract Alchemist config fields from a broader config source
#'
#' @param source Candidates, Configurer, or config-like list.
#'
#' @return Normalized Alchemist config list.
#'
#' @keywords internal
#' @noRd
alchemist_config_from_config <- function(source) {
  cfg <- if (.is_candidates_obj(source)) {
    candidates_configuration(source) %||% list()
  } else if (is_s7_instance(source, "Configurer")) {
    source@data
  } else if (is.list(source)) {
    source
  } else {
    list()
  }

  alch <- cfg$alchemist %||% list()
  sim <- cfg$similarity %||% list()
  ml <- cfg$selection %||% list()
  method_defaults_path <- alch$learner$method_defaults_path %||%
    ml$method_defaults_path %||%
    NULL
  method_settings <- merge_config_sections(
    ml$method_settings %||% list(),
    alch$learner$method_settings %||% list()
  )
  method_settings <- attach_meta_policy_method_defaults_path(
    method_settings,
    method_defaults_path
  )
  cache_dir <- alch$cache_dir %||%
    (cfg$paths %||% list())$cache_dir %||%
    (cfg$cache %||% list())$folder %||%
    NULL
  refresh <- alch$refresh %||%
    (cfg$cache %||% list())$refresh %||%
    FALSE

  list(
    species_traits = alch$species_traits %||% sim$species_traits %||% list(),
    study_traits = alch$study_traits %||% sim$study_traits %||% list(),
    coherence = alch$coherence %||% sim$coherence %||% list(),
    taxonomic_distance = alch$taxonomic_distance %||% FALSE,
    feature_type = alch$feature_type %||% "gower",
    learner = list(
      methods = alch$learner$methods %||% ml$super_methods %||% NULL,
      inner_folds = as.integer(alch$learner$inner_folds %||%
        ml$inner_folds %||% 5L),
      seed = if (!is.null(alch$learner$seed %||% ml$seed)) {
        as.integer(alch$learner$seed %||% ml$seed)
      } else {
        NULL
      },
      outcome_transform = alch$learner$outcome_transform %||%
        ml$outcome_transform %||% "identity",
      lambda_rule = alch$learner$lambda_rule %||% ml$lambda_rule %||% "lambda.1se",
      weight_rule = alch$learner$weight_rule %||% "nnls",
      oof_mode = alch$learner$oof_mode %||% "anchor_species",
      # Preserve the shared learner-family settings and let any Alchemist-
      # specific overrides replace only the fields they explicitly set.
      method_defaults_path = method_defaults_path,
      method_settings = method_settings,
      workers = as.integer(alch$learner$workers %||% ml$workers %||% 1L)
    ),
    distill_workers = as.integer(alch$distill_workers %||% 1L),
    registry_path = alch$registry_path %||% cfg$registry_path %||% NULL,
    progress = (cfg$execution %||% list())$progress %||% FALSE,
    cache_dir = cache_dir,
    refresh = isTRUE(refresh),
    config_data = cfg
  )
}

#' Normalize the runtime Alchemist config
#'
#' @param config Optional config source.
#' @param candidates Optional [Candidates] object used for inheritance.
#'
#' @return Normalized config list.
#'
#' @keywords internal
#' @noRd
normalize_alchemist_config <- function(config, candidates = NULL) {
  if (is.null(config)) {
    config <- if (!is.null(candidates)) {
      alchemist_config_from_config(candidates)
    } else {
      list()
    }
  } else if (.is_candidates_obj(config) ||
    is_s7_instance(config, "Configurer")) {
    config <- alchemist_config_from_config(config)
  } else if (is.list(config) &&
    any(c("alchemist", "similarity", "selection", "policy", "policies")
    %in% names(config))) {
    config <- alchemist_config_from_config(config)
  } else if (is.character(config) && length(config) == 1 &&
    file.exists(config)) {
    raw <- yaml::read_yaml(config)
    cfg_obj <- tryCatch(build_configurer(raw), error = function(e) NULL)
    config <- if (!is.null(cfg_obj)) {
      alchemist_config_from_config(cfg_obj)
    } else {
      raw
    }
  } else if (!is.list(config)) {
    stop(
      "'config' must be NULL, a list, a YAML path, a `Candidates`, or a `Configurer`.",
      call. = FALSE
    )
  }

  if (is.null(config$learner)) config$learner <- list()
  config$learner$inner_folds <- as.integer(config$learner$inner_folds %||% 5L)
  config$learner$seed <- if (!is.null(config$learner$seed)) {
    as.integer(config$learner$seed)
  } else {
    NULL
  }
  config$learner$outcome_transform <- config$learner$outcome_transform %||%
    "identity"
  config$learner$lambda_rule <- normalize_alchemist_lambda_rule(
    config$learner$lambda_rule %||% "lambda.1se"
  )
  config$learner$weight_rule <- normalize_alchemist_weight_rule(
    config$learner$weight_rule %||% "nnls"
  )
  config$learner$oof_mode <- config$learner$oof_mode %||% "anchor_species"
  config$learner$workers <- as.integer(config$learner$workers %||% 1L)
  config$distill_workers <- as.integer(config$distill_workers %||% 1L)
  config$feature_type <- config$feature_type %||% "gower"
  config$refresh <- isTRUE(config$refresh %||% FALSE)

  config
}

#' Normalize Alchemist GLM lambda selection rule
#'
#' @keywords internal
#' @noRd
normalize_alchemist_lambda_rule <- function(x) {
  value <- stringr::str_to_lower(stringr::str_squish(as.character(x %||% "lambda.1se")[[1]]))
  aliases <- c(
    "min" = "lambda.min",
    "lambda_min" = "lambda.min",
    "lambda.min" = "lambda.min",
    "1se" = "lambda.1se",
    "lambda_1se" = "lambda.1se",
    "lambda.1se" = "lambda.1se"
  )
  out <- unname(aliases[value] %||% NA_character_)
  if (is.na(out) || !nzchar(out)) {
    stop(
      "'alchemist.learner.lambda_rule' must be 'lambda.min' or 'lambda.1se'.",
      call. = FALSE
    )
  }
  out
}

#' Normalize Alchemist Super Learner weight rule
#'
#' @keywords internal
#' @noRd
normalize_alchemist_weight_rule <- function(x) {
  value <- stringr::str_to_lower(stringr::str_squish(as.character(x %||% "nnls")[[1]]))
  aliases <- c(
    "nnls" = "nnls",
    "inverse_risk" = "inverse_risk",
    "inverse-risk" = "inverse_risk",
    "inverse_mse" = "inverse_risk",
    "inverse-mse" = "inverse_risk",
    "equal" = "equal"
  )
  out <- unname(aliases[value] %||% NA_character_)
  if (is.na(out) || !nzchar(out)) {
    stop(
      "'alchemist.learner.weight_rule' must be 'nnls', 'inverse_risk', or 'equal'.",
      call. = FALSE
    )
  }
  out
}

# - Class definition -

#' @export
Alchemist <- S7::new_class(
  "Alchemist",
  properties = list(
    candidates = S7::new_property(S7::class_any),
    config = S7::new_property(S7::class_list),
    # class_any: list() pre-fit, or a tsb_shared_distance_learner env after.
    learner = S7::new_property(S7::class_any),
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
        if (!is.list(self@learner) && !inherits(self@learner, "tsb_shared_distance_learner")) {
          return("`learner` must be a list or a wrapped distance-learner reference.")
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

#' Rebuild an `Alchemist` object
#'
#' @param object Existing [Alchemist] object.
#' @param candidates Candidates payload.
#' @param config Config payload.
#' @param learner Learner payload.
#' @param distance_matrix Distance-matrix payload.
#' @param trait_importance Trait-importance payload.
#' @param ordination Ordination payload.
#' @param admissibility Admissibility payload.
#'
#' @return An [Alchemist] object.
#'
#' @keywords internal
#' @noRd
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
#' @param candidates A [Candidates] object with candidate-model rows. Prepared
#'   similarity state is used to inherit trait names when no explicit config is
#'   supplied.
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

  if (length(config$species_traits) == 0 &&
    length(candidates@similarity_matrix) > 0) {
    config$species_traits <- (candidates@similarity_matrix)$species_traits %||%
      list()
  }
  if (length(config$study_traits) == 0 &&
    length(candidates@similarity_matrix) > 0) {
    config$study_traits <- (candidates@similarity_matrix)$study_traits %||%
      list()
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

#' Build an Alchemist stage cache path
#'
#' Resolves an object-level cache path from the normalized Alchemist config.
#' Returns `NULL` when no cache directory is configured.
#'
#' @param config Normalized Alchemist config list.
#' @param stage Cache stage name.
#' @param suffix Optional suffix used to distinguish stage variants.
#'
#' @return Cache file path, or `NULL`.
#' @keywords internal
#' @noRd
alchemist_stage_cache_path <- function(config,
                                       stage,
                                       suffix = NULL) {
  cache_dir <- config$cache_dir %||% NULL
  if (is.null(cache_dir) || !nzchar(as.character(cache_dir))) {
    return(NULL)
  }
  stage <- gsub("[^A-Za-z0-9_]+", "_", as.character(stage %||% "stage"))
  suffix <- if (is.null(suffix) || !nzchar(as.character(suffix))) {
    NULL
  } else {
    gsub("[^A-Za-z0-9_]+", "_", as.character(suffix))
  }
  cache_root <- file.path(as.character(cache_dir), "alchemist")
  dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
  file.path(
    cache_root,
    paste0(stage, if (!is.null(suffix)) paste0("_", suffix) else "", ".rds")
  )
}

# - Trait normalization and set-expansion helpers -

# Map raw ocean_basin strings to canonical lowercase basin codes.
# "North Atlantic Ocean", "Atlantic Ocean", "atlantic" all map to "atlantic".
# Values containing no recognized basin keyword become NA.
# Multiple basins are preserved as ";"-joined codes ("atlantic;pacific").
#'
#' @keywords internal
#' @noRd
normalize_alchemist_ocean_basin <- function(x) {
  keywords <- c(
    "atlantic", "pacific", "mediterranean", "indian", "southern",
    "arctic", "inland"
  )
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
#'
#' @keywords internal
#' @noRd
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
#'
#' @keywords internal
#' @noRd
expand_multival_col <- function(x, col_fn, tr) {
  x_chr <- as.character(x)
  if (!any(grepl(";", x_chr, fixed = TRUE), na.rm = TRUE)) {
    return(stats::setNames(list(col_fn(x)), paste0(".dist_", tr)))
  }
  missing_value <- is.na(x) | !nzchar(trimws(x_chr)) |
    toupper(trimws(x_chr)) %in% c("NA", "N/A", "UNKNOWN", "NULL")
  vals_list <- strsplit(trimws(x_chr), "\\s*;\\s*")
  all_vals <- sort(unique(trimws(unlist(vals_list, use.names = FALSE))))
  all_vals <- all_vals[!is.na(all_vals) & nzchar(all_vals)]
  mats <- lapply(all_vals, function(v) {
    binary <- vapply(seq_along(vals_list), function(i) {
      if (isTRUE(missing_value[[i]])) {
        return(NA_integer_)
      }
      as.integer(v %in% trimws(vals_list[[i]]))
    }, integer(1))
    col_fn(binary)
  })
  stats::setNames(mats, paste0(".dist_", tr, "__", gsub(
    "[^A-Za-z0-9]", "_",
    all_vals
  )))
}

# - Pair-level training data -

#' Normalized Gower distance for one trait column
#'
#' @keywords internal
#' @noRd
gower_col <- function(x, scale = NULL) {
  if (is.numeric(x)) {
    finite_x <- x[is.finite(x)]
    r <- suppressWarnings(as.numeric(scale)[[1]])
    if (!is.finite(r) || r <= 0) {
      r <- if (length(finite_x) >= 2L) diff(range(finite_x)) else 1
    }
    if (!is.finite(r) || r <= 0) r <- 1
    mat <- outer(x, x, function(a, b) abs(a - b) / r)
    mat[!is.finite(mat)] <- 1
    mat
  } else {
    xc <- as.character(x)
    xc[is.na(x) | !nzchar(xc)] <- NA_character_
    outer(xc, xc, function(a, b) {
      ifelse(is.na(a) | is.na(b), 1, ifelse(a == b, 0, 1))
    })
  }
}

# Signed standardized difference matrix for one trait column.
# Numeric traits: (a - b) / sd(x), with 0 imputed for non-finite pairs.
# Categorical traits: delegates to gower_col (0 = match, 1 = mismatch).
#'
#' @keywords internal
#' @noRd
diff_col <- function(x, scale = NULL) {
  if (is.numeric(x)) {
    s <- suppressWarnings(as.numeric(scale)[[1]])
    if (!is.finite(s) || s <= 0) {
      s <- sd(x[is.finite(x)], na.rm = TRUE)
    }
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
#'
#' @keywords internal
#' @noRd
squared_col <- function(x, scale = NULL) {
  if (is.numeric(x)) {
    s <- suppressWarnings(as.numeric(scale)[[1]])
    if (!is.finite(s) || s <= 0) {
      s <- sd(x[is.finite(x)], na.rm = TRUE)
    }
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
#' @noRd
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
#' @noRd
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
.ALCH_TAX_RANKS <- c(
  "kingdom", "phylum", "class", "order", "family", "genus",
  "species"
)

#' Compute a taxonomic/phylogenetic distance matrix for Alchemist pair features
#'
#' Builds a continuous phylogenetic distance matrix using Open Tree of Life.
#' The raw OpenTree Newick is parsed directly so singleton/internal taxon
#' placements are retained; this is required for taxa that are matched by TNRS
#' but represented in the synthetic tree at a higher node.
#'
#' @param models_df Data frame of candidate models. Must contain at least one
#'   of `species_name`, or `species` plus `genus`.
#' @param tax_col_map Named character vector mapping canonical rank names to
#'   actual column names in `models_df` (e.g. `c(family="family",
#'   genus="genus", species="species_name")`). Ordered broadest-to-specific.
#'
#' @return Numeric n x n matrix, values between 0 and 1, or `NULL` on total
#' failure.
#'
#' @keywords internal
#' @noRd
alchemist_species_labels <- function(models_df, tax_col_map) {
  n <- nrow(models_df)
  valid_binomial_label <- function(x) {
    parts <- strsplit(x, "\\s+")
    vapply(parts, function(z) {
      if (length(z) < 2L) {
        return(FALSE)
      }
      first_two <- z[seq_len(2L)]
      all(nzchar(first_two)) &&
        !any(toupper(first_two) %in% c("NA", "N/A", "UNKNOWN", "NULL"))
    }, logical(1))
  }
  species_name_col <- if ("species_name" %in% names(models_df)) {
    "species_name"
  } else if ("scientific_name" %in% names(models_df)) {
    "scientific_name"
  } else {
    NA_character_
  }
  labels <- rep(NA_character_, n)
  if (!is.na(species_name_col)) {
    labels <- stringr::str_squish(as.character(models_df[[species_name_col]]))
    labels[!nzchar(labels) | labels == "NA"] <- NA_character_
  }

  needs_constructed <- is.na(labels)
  species_col <- if ("species" %in% names(tax_col_map)) {
    unname(tax_col_map[["species"]])
  } else if ("species" %in% names(models_df)) {
    "species"
  } else {
    NA_character_
  }
  genus_col <- if ("genus" %in% names(tax_col_map)) {
    unname(tax_col_map[["genus"]])
  } else if ("genus" %in% names(models_df)) {
    "genus"
  } else {
    NA_character_
  }
  if (any(needs_constructed) &&
    !is.na(species_col) && species_col %in% names(models_df)) {
    sp <- stringr::str_squish(as.character(models_df[[species_col]]))
    sp[!nzchar(sp) | sp == "NA"] <- NA_character_
    constructed <- sp
    has_binomial <- !is.na(sp) & grepl("\\s+", sp)
    if (!is.na(genus_col) && genus_col %in% names(models_df)) {
      gn <- stringr::str_squish(as.character(models_df[[genus_col]]))
      gn[!nzchar(gn) | gn == "NA"] <- NA_character_
      can_build <- !has_binomial & !is.na(gn) & !is.na(sp)
      constructed[can_build] <- stringr::str_squish(paste(gn[can_build], sp[can_build]))
    }
    labels[needs_constructed] <- constructed[needs_constructed]
  }
  labels[!is.na(labels) & !valid_binomial_label(labels)] <- NA_character_
  labels
}

#' Fetch OpenTree node distances for submitted species labels
#'
#' @keywords internal
#' @noRd
alchemist_open_tree_node_distance <- function(species_labels) {
  species_labels <- stringr::str_squish(as.character(species_labels))
  species_labels[!nzchar(species_labels) | species_labels == "NA"] <- NA_character_
  n <- length(species_labels)
  out <- matrix(NA_real_, nrow = n, ncol = n)
  diag(out) <- 0
  if (n < 2L || sum(!is.na(species_labels)) < 2L) {
    attr(out, "taxonomic_distance_method") <- "open_tree_unavailable"
    return(out)
  }
  if (!requireNamespace("rotl", quietly = TRUE) ||
    !requireNamespace("ape", quietly = TRUE)) {
    attr(out, "taxonomic_distance_method") <- "open_tree_unavailable"
    return(out)
  }

  labels_key <- stringr::str_to_lower(species_labels)
  unique_labels <- unique(species_labels[!is.na(species_labels)])
  tnrs <- tryCatch(
    rotl::tnrs_match_names(unique_labels, do_approximate_matching = FALSE),
    error = function(e) NULL
  )
  if (is.null(tnrs)) {
    attr(out, "taxonomic_distance_method") <- "open_tree_failed"
    return(out)
  }
  matched <- tibble::as_tibble(tnrs)
  if (!all(c("search_string", "ott_id") %in% names(matched))) {
    attr(out, "taxonomic_distance_method") <- "open_tree_failed"
    return(out)
  }
  matched <- matched |>
    dplyr::mutate(
      .query_key = stringr::str_to_lower(stringr::str_squish(.data$search_string)),
      .ott_id_chr = as.character(.data$ott_id)
    ) |>
    dplyr::filter(!is.na(.data$ott_id), nzchar(.data$.ott_id_chr)) |>
    dplyr::distinct(.data$.query_key, .keep_all = TRUE)
  if (nrow(matched) < 2L) {
    attr(out, "taxonomic_distance_method") <- "open_tree_failed"
    return(out)
  }

  tree_file <- tempfile(fileext = ".tre")
  on.exit(unlink(tree_file), add = TRUE)
  ok_tree <- tryCatch(
    rotl::tol_induced_subtree(
      ott_ids = matched$ott_id,
      label_format = "name_and_id",
      file = tree_file
    ),
    error = function(e) FALSE
  )
  if (!isTRUE(ok_tree) || !file.exists(tree_file)) {
    attr(out, "taxonomic_distance_method") <- "open_tree_failed"
    return(out)
  }
  newick <- paste(readLines(tree_file, warn = FALSE), collapse = "")
  tree <- tryCatch(ape::read.tree(text = newick), error = function(e) NULL)
  if (is.null(tree) || is.null(tree$edge) || length(tree$tip.label) < 2L) {
    attr(out, "taxonomic_distance_method") <- "open_tree_failed"
    return(out)
  }
  tree <- tryCatch(ape::compute.brlen(tree, method = "Grafen"), error = function(e) tree)
  node_dist <- tryCatch(ape::dist.nodes(tree), error = function(e) NULL)
  if (is.null(node_dist) || !is.matrix(node_dist)) {
    attr(out, "taxonomic_distance_method") <- "open_tree_failed"
    return(out)
  }
  tree_labels <- c(tree$tip.label, tree$node.label)
  if (length(tree_labels) != nrow(node_dist)) {
    attr(out, "taxonomic_distance_method") <- "open_tree_failed"
    return(out)
  }
  dimnames(node_dist) <- list(tree_labels, tree_labels)

  find_label_by_ott <- function(ott_id) {
    ott_id <- as.character(ott_id)
    if (is.na(ott_id) || !nzchar(ott_id)) {
      return(NA_character_)
    }
    hit <- grep(paste0("ott", ott_id, "([^0-9]|$)"), tree_labels, value = TRUE)
    if (length(hit) > 0L) hit[[1L]] else NA_character_
  }
  find_label_by_node <- function(node_id) {
    node_id <- as.character(node_id)
    if (is.na(node_id) || !nzchar(node_id)) {
      return(NA_character_)
    }
    node_id_num <- sub("^ott", "", node_id)
    find_label_by_ott(node_id_num)
  }
  node_cache <- new.env(parent = emptyenv())
  node_id_for_ott <- function(ott_id) {
    key <- as.character(ott_id)
    if (exists(key, node_cache, inherits = FALSE)) {
      return(get(key, node_cache, inherits = FALSE))
    }
    info <- tryCatch(rotl::tol_node_info(ott_id = ott_id), error = function(e) NULL)
    node_id <- if (is.list(info) && !is.null(info$node_id)) {
      as.character(info$node_id)
    } else {
      NA_character_
    }
    assign(key, node_id, envir = node_cache)
    node_id
  }

  matched$distance_label <- vapply(matched$ott_id, find_label_by_ott, character(1))
  missing_label <- is.na(matched$distance_label) | !matched$distance_label %in% tree_labels
  if (any(missing_label)) {
    node_ids <- vapply(matched$ott_id[missing_label], node_id_for_ott, character(1))
    matched$distance_label[missing_label] <- vapply(node_ids, find_label_by_node, character(1))
  }

  label_lookup <- stats::setNames(matched$distance_label, matched$.query_key)
  row_labels <- unname(label_lookup[labels_key])
  valid <- !is.na(row_labels) & row_labels %in% tree_labels
  if (sum(valid) >= 2L) {
    out[valid, valid] <- node_dist[row_labels[valid], row_labels[valid], drop = FALSE]
    finite_positive <- out[is.finite(out) & out > 0]
    if (length(finite_positive) > 0L) {
      scale <- max(finite_positive, na.rm = TRUE)
      if (is.finite(scale) && scale > 0) {
        out[is.finite(out)] <- out[is.finite(out)] / scale
        attr(out, "taxonomic_distance_scale") <- scale
      }
    }
  }
  diag(out) <- 0
  attr(out, "taxonomic_distance_method") <- "open_tree_node_grafen"
  attr(out, "taxonomic_distance_labels") <- row_labels
  out
}

tax_dist_mat <- function(models_df, tax_col_map) {
  n <- nrow(models_df)

  tax_map_value <- function(rank) {
    if (!rank %in% names(tax_col_map)) {
      return(NULL)
    }
    value <- unname(tax_col_map[[rank]])
    if (length(value) == 0L || is.na(value) || !nzchar(value)) NULL else value
  }

  species_col <- tax_map_value("species") %||% tax_map_value("species_name")
  if (is.null(species_col) && !"species_name" %in% names(models_df)) {
    return(NULL)
  }

  labels <- alchemist_species_labels(models_df, tax_col_map)
  out <- alchemist_open_tree_node_distance(labels)
  dimnames(out) <- list(seq_len(n), seq_len(n))
  out
}

#' Compute pairwise coherence distance matrices
#'
#' Builds length-, depth-, and frequency-coherence feature matrices for all
#' candidate model pairs. Missing columns or `mode = "none"` silently return
#' `NULL` for that dimension. The asymmetric convention matches
#' `build_pair_data()`: `mat[i, j]` is the coherence distance when
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
#' @noRd
coherence_mats <- function(models_df, coherence_cfg) {
  len_mode <- as.character(coherence_cfg$length$mode %||% "none")
  dep_mode <- as.character(coherence_cfg$depth$mode %||% "none")
  freq_mode <- as.character(coherence_cfg$frequency$mode %||% "none")

  # source controls which column(s) back each interval dimension:
  #   "best"    - study columns if present, species columns as fallback [def]
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
    interval_overlap_distance_matrix(lo_vals, hi_vals, method)
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
        result[[paste0(base_key, "_species")]] <- interval_mat(
          lo_sp, hi_sp,
          mode
        )
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
    mat <- if (any(is.finite(lo)) && any(is.finite(hi))) {
      interval_mat(lo, hi, mode)
    } else {
      NULL
    }
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
        freq_mat <- frequency_offset_distance_matrix(
          freq_vals, freq_mode,
          freq_span
        )
      }
    }
  }

  c(len_mats, dep_mats, list(frequency_coherence = freq_mat))
}

#' Build directed pair-feature matrices for Alchemist distance prediction
#'
#' @keywords internal
#' @noRd
build_pair_feature_matrices <- function(models_df,
                                        species_trait_names,
                                        study_trait_names,
                                        coherence_cfg = NULL,
                                        taxonomic_distance = FALSE,
                                        feature_type = c(
                                          "gower", "difference", "mahalanobis"
                                        ),
                                        feature_normalization = NULL,
                                        progress = FALSE) {
  feature_type <- match.arg(feature_type)
  all_traits <- unique(c(species_trait_names, study_trait_names))
  if (length(all_traits) == 0L) {
    stop(
      "No configured traits found in `candidate_models`. Supply species_traits and/or study_traits.",
      call. = FALSE
    )
  }
  missing_traits <- setdiff(all_traits, names(models_df))
  if (length(missing_traits) > 0L) {
    stop(
      sprintf(
        "Candidate models are missing required Alchemist trait column(s): %s",
        paste(missing_traits, collapse = ", ")
      ),
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

  trait_values <- lapply(all_traits, function(tr) {
    x <- models_df[[tr]]
    if (identical(tr, "ocean_basin")) {
      x <- normalize_alchemist_ocean_basin(x)
    } else if (identical(tr, "fao_area")) {
      x <- normalize_alchemist_fao_area(x)
    } else if (identical(tr, "species") && "genus" %in% names(models_df)) {
      g <- trimws(as.character(models_df[["genus"]]))
      s <- trimws(as.character(x))
      combined <- paste(g, s)
      combined[is.na(g) | g == "NA" | is.na(s) | s == "NA"] <- NA_character_
      x <- combined
    }
    x
  })
  names(trait_values) <- all_traits
  normalization <- stats::setNames(vector("list", length(all_traits)), all_traits)
  trait_mat_list <- lapply(all_traits, function(tr) {
    x <- trait_values[[tr]]
    supplied_scale <- suppressWarnings(as.numeric(feature_normalization[[tr]]$scale %||% NA_real_)[[1]])
    scale <- supplied_scale
    if (is.numeric(x) && (!is.finite(scale) || scale <= 0)) {
      finite_x <- x[is.finite(x)]
      scale <- if (identical(feature_type, "gower")) {
        if (length(finite_x) >= 2L) diff(range(finite_x)) else 1
      } else {
        stats::sd(finite_x, na.rm = TRUE)
      }
    }
    if (!is.finite(scale) || scale <= 0) scale <- NA_real_
    normalization[[tr]] <<- list(scale = scale)
    expand_multival_col(x, function(values) col_fn(values, scale = scale), tr)
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

  if (isTRUE(taxonomic_distance)) {
    tax_ranks_found <- intersect(.ALCH_TAX_RANKS, tolower(species_trait_names))
    tax_col_map <- stats::setNames(
      vapply(tax_ranks_found, function(rk) {
        m <- species_trait_names[tolower(species_trait_names) == rk]
        if (length(m) > 0L) m[[1L]] else NA_character_
      }, character(1)),
      tax_ranks_found
    )
    tax_col_map <- tax_col_map[!is.na(tax_col_map) & tax_col_map %in%
      names(models_df)]
    if (length(tax_col_map) >= 1L) {
      report_progress(
        progress,
        "  [Alchemist] Computing phylogenetic distance (OpenTree node distances) for: ",
        paste(names(tax_col_map), collapse = ", "), "..."
      )
      tax_mat <- tax_dist_mat(models_df, tax_col_map)
      if (!is.null(tax_mat)) {
        tax_method <- attr(tax_mat, "taxonomic_distance_method") %||% "unknown"
        trait_mats[[".dist_tax"]] <- tax_mat
        drop_cols <- paste0(".dist_", unname(tax_col_map))
        trait_mats <- trait_mats[setdiff(names(trait_mats), drop_cols)]
        report_progress(
          progress,
          "  [Alchemist]   Replaced ", paste(drop_cols, collapse = ", "),
          " with .dist_tax (", tax_method, ")."
        )
      } else {
        report_progress(
          progress,
          "  [Alchemist]   WARNING: phylogenetic distance failed; keeping individual Gower features."
        )
      }
    }
  }

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

  list(
    trait_mats = trait_mats,
    feature_cols = names(trait_mats),
    all_traits = all_traits,
    species_feature_cols = species_feature_cols,
    feature_type = feature_type,
    feature_normalization = normalization
  )
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
#' @noRd
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
  feature_data <- build_pair_feature_matrices(
    models_df = models_df,
    species_trait_names = species_trait_names,
    study_trait_names = study_trait_names,
    coherence_cfg = coherence_cfg,
    taxonomic_distance = taxonomic_distance,
    feature_type = feature_type,
    progress = progress
  )
  all_traits <- feature_data$all_traits
  trait_mats <- feature_data$trait_mats
  feature_cols <- feature_data$feature_cols
  species_feature_cols <- feature_data$species_feature_cols
  n <- nrow(models_df)

  slope_col <- intersect(
    c("slope_standard", "slope_len"),
    names(models_df)
  )[[1]] %||% NULL
  intercept_col <- intersect(
    c("intercept_standard", "intercept_len"),
    names(models_df)
  )[[1]] %||% NULL
  if (is.null(slope_col) || !slope_col %in% names(models_df)) {
    stop("Required slope column was not found in candidate models.",
      call. = FALSE
    )
  }
  if (is.null(intercept_col) || !intercept_col %in% names(models_df)) {
    stop("Required intercept column was not found in candidate models.",
      call. = FALSE
    )
  }
  slope_vals <- suppressWarnings(as.numeric(models_df[[slope_col]]))
  intercept_vals <- suppressWarnings(as.numeric(models_df[[intercept_col]]))
  slope_vals[!is.finite(slope_vals)] <- NA_real_
  intercept_vals[!is.finite(intercept_vals)] <- NA_real_

  anchor_pdfs <- lapply(seq_len(n), function(j) {
    pdf_j <- build_anchor_length_pdf(
      models_df[j, , drop = FALSE],
      config = NULL,
      on_missing = "empty"
    )
    if (nrow(pdf_j) == 0L) {
      return(NULL)
    }
    pdf_j
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
  group_model <- generalized_model_indicator(models_df)

  model_ids <- if ("model_id" %in% names(models_df)) {
    as.character(models_df$model_id)
  } else if ("model_id" %in% names(models_df)) {
    as.character(models_df$model_id)
  } else {
    as.character(seq_len(n))
  }

  report_progress(
    progress,
    "  [Alchemist] Assembling non-self donor-anchor model pairs..."
  )
  valid_mask <- is.finite(donor_sigma_mat) & donor_sigma_mat > 0
  diag(valid_mask) <- FALSE
  if (any(group_model)) {
    valid_mask[group_model, ] <- FALSE
    valid_mask[, group_model] <- FALSE
  }
  pair_idx <- which(valid_mask, arr.ind = TRUE)

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
    feature_normalization = feature_data$feature_normalization,
    n_models = n,
    model_ids = model_ids,
    donor_sigma_matrix = donor_sigma_mat,
    target_sigma = target_sigma,
    trait_mats = trait_mats,
    feature_type = feature_type
  )
}

#' Materialize fitted Alchemist feature rows for query anchors
#'
#' @keywords internal
#' @noRd
alchemist_query_pair_features <- function(candidate_models,
                                          distance_state,
                                          donor_model_ids,
                                          anchor_model_ids) {
  candidate_models <- tibble::as_tibble(candidate_models)
  required_state <- c(
    "distance_learner", "species_trait_names", "study_trait_names",
    "feature_type", "coherence_config", "taxonomic_distance",
    "feature_normalization"
  )
  missing_state <- required_state[vapply(
    required_state,
    function(name) is.null(distance_state[[name]]),
    logical(1)
  )]
  if (length(missing_state) > 0L) {
    stop(
      sprintf(
        "Alchemist query-distance state is incomplete: %s. Rebuild the selector from a newly forged Alchemist object.",
        paste(missing_state, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!"model_id" %in% names(candidate_models)) {
    stop("Alchemist query-distance prediction requires `candidate_models$model_id`.", call. = FALSE)
  }
  model_ids <- as.character(candidate_models$model_id)
  if (anyDuplicated(model_ids)) {
    stop("Alchemist query-distance prediction requires unique candidate model IDs.", call. = FALSE)
  }

  feature_data <- build_pair_feature_matrices(
    models_df = candidate_models,
    species_trait_names = as.character(distance_state$species_trait_names),
    study_trait_names = as.character(distance_state$study_trait_names),
    coherence_cfg = distance_state$coherence_config,
    taxonomic_distance = isTRUE(distance_state$taxonomic_distance),
    feature_type = as.character(distance_state$feature_type),
    feature_normalization = distance_state$feature_normalization,
    progress = FALSE
  )
  learner <- resolve_distance_learner(distance_state$distance_learner)
  feature_cols <- as.character(learner$feature_cols)
  if (length(feature_cols) == 0L) {
    stop("The stored Alchemist learner has no feature columns.", call. = FALSE)
  }
  missing_features <- setdiff(feature_cols, names(feature_data$trait_mats))
  if (length(missing_features) > 0L) {
    stop(
      sprintf(
        "Query anchor metadata cannot construct required Alchemist feature(s): %s",
        paste(missing_features, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  donor_model_ids <- as.character(donor_model_ids)
  anchor_model_ids <- as.character(anchor_model_ids)
  donor_idx <- match(donor_model_ids, model_ids)
  anchor_idx <- match(anchor_model_ids, model_ids)
  if (anyNA(donor_idx) || anyNA(anchor_idx)) {
    stop("Query-distance model IDs were not found in `candidate_models`.", call. = FALSE)
  }
  pairs <- expand.grid(
    donor_idx = donor_idx,
    anchor_idx = anchor_idx,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  pairs <- pairs[pairs$donor_idx != pairs$anchor_idx, , drop = FALSE]
  out <- tibble::tibble(
    .donor_model_id = model_ids[pairs$donor_idx],
    .anchor_model_id = model_ids[pairs$anchor_idx]
  )
  for (feature_col in feature_cols) {
    out[[feature_col]] <- feature_data$trait_mats[[feature_col]][
      cbind(pairs$donor_idx, pairs$anchor_idx)
    ]
  }
  attr(out, "trait_mats") <- feature_data$trait_mats
  out
}

#' Add external query-anchor rows to a fitted Alchemist distance bundle
#'
#' @keywords internal
#' @noRd
augment_alchemist_query_distances <- function(candidate_models,
                                              distance_state,
                                              query_model_ids) {
  if (!identical(distance_state$distance_mode %||% "", "alchemist_super_learner")) {
    stop("Alchemist query-distance augmentation requires an Alchemist distance bundle.", call. = FALSE)
  }
  learner <- resolve_distance_learner(distance_state$distance_learner)
  if (is.null(learner)) {
    stop("Alchemist query-distance prediction requires the fitted distance learner.", call. = FALSE)
  }
  candidate_models <- tibble::as_tibble(candidate_models)
  if (!"model_id" %in% names(candidate_models)) {
    stop("Alchemist query-distance prediction requires `candidate_models$model_id`.", call. = FALSE)
  }
  model_ids <- as.character(candidate_models$model_id)
  query_model_ids <- unique(as.character(query_model_ids))
  if (anyNA(match(query_model_ids, model_ids))) {
    stop("Every external query anchor must be present in the augmented candidate table.", call. = FALSE)
  }

  existing_dist <- distance_state$learned_directed_dist
  if (is.null(existing_dist) || !is.matrix(existing_dist) ||
    is.null(rownames(existing_dist)) || is.null(colnames(existing_dist))) {
    stop("Alchemist query-distance prediction requires a stored directed learned-distance matrix.", call. = FALSE)
  }
  base_ids <- setdiff(model_ids, query_model_ids)
  if (!all(base_ids %in% rownames(existing_dist)) ||
    !all(base_ids %in% colnames(existing_dist))) {
    stop("The stored learned-distance matrix does not cover every existing candidate model.", call. = FALSE)
  }

  query_pairs <- alchemist_query_pair_features(
    candidate_models = candidate_models,
    distance_state = distance_state,
    donor_model_ids = model_ids,
    anchor_model_ids = query_model_ids
  )
  predicted <- predict_distance(learner, query_pairs)
  if (length(predicted) != nrow(query_pairs) || any(!is.finite(predicted))) {
    stop("The fitted Alchemist learner did not produce finite distances for every query pair.", call. = FALSE)
  }
  diagnostic_available <- isTRUE(attr(predicted, "distance_diagnostic_available"))
  disagreement <- attr(predicted, "distance_weighted_disagreement")
  if (inherits(learner, "SuperLearner") &&
    (!diagnostic_available || length(disagreement) != nrow(query_pairs) ||
      any(!is.finite(disagreement)))) {
    stop("The fitted Alchemist Super Learner did not produce query-distance disagreement diagnostics.", call. = FALSE)
  }

  learned_dist <- matrix(NA_real_,
    nrow = length(model_ids), ncol = length(model_ids),
    dimnames = list(model_ids, model_ids)
  )
  shared_ids <- intersect(model_ids, rownames(existing_dist))
  learned_dist[shared_ids, shared_ids] <- existing_dist[shared_ids, shared_ids, drop = FALSE]
  if (nrow(query_pairs) > 0L) {
    learned_dist[cbind(query_pairs$.donor_model_id, query_pairs$.anchor_model_id)] <-
      pmax(0, as.numeric(predicted))
  }
  diag(learned_dist) <- 0

  learned_disagreement <- NULL
  if (diagnostic_available) {
    learned_disagreement <- matrix(
      NA_real_,
      nrow = length(model_ids), ncol = length(model_ids),
      dimnames = list(model_ids, model_ids)
    )
    if (nrow(query_pairs) > 0L) {
      learned_disagreement[cbind(query_pairs$.donor_model_id, query_pairs$.anchor_model_id)] <-
        as.numeric(disagreement)
    }
    diag(learned_disagreement) <- 0
  }

  taxonomic_dist <- distance_state$taxonomic_dist_model %||% NULL
  if (!is.null(taxonomic_dist)) {
    trait_mats <- attr(query_pairs, "trait_mats")
    if (is.null(trait_mats[[".dist_tax"]])) {
      stop("The stored Alchemist taxonomic distance cannot be constructed for the query anchor.", call. = FALSE)
    }
    taxonomic_dist_new <- matrix(NA_real_,
      nrow = length(model_ids), ncol = length(model_ids),
      dimnames = list(model_ids, model_ids)
    )
    tax_shared_ids <- intersect(model_ids, rownames(taxonomic_dist))
    taxonomic_dist_new[tax_shared_ids, tax_shared_ids] <-
      taxonomic_dist[tax_shared_ids, tax_shared_ids, drop = FALSE]
    all_query_pairs <- alchemist_query_pair_features(
      candidate_models = candidate_models,
      distance_state = distance_state,
      donor_model_ids = model_ids,
      anchor_model_ids = query_model_ids
    )
    if (nrow(all_query_pairs) > 0L) {
      donor_idx <- match(all_query_pairs$.donor_model_id, model_ids)
      anchor_idx <- match(all_query_pairs$.anchor_model_id, model_ids)
      taxonomic_dist_new[cbind(all_query_pairs$.donor_model_id, all_query_pairs$.anchor_model_id)] <-
        trait_mats[[".dist_tax"]][cbind(donor_idx, anchor_idx)]
    }
    diag(taxonomic_dist_new) <- 0
    taxonomic_dist <- taxonomic_dist_new
  }

  out <- distance_state
  out$learned_directed_dist <- learned_dist
  out$learned_distance_disagreement <- learned_disagreement
  out$learned_distance_diagnostic_available <- diagnostic_available
  out$taxonomic_dist_model <- taxonomic_dist
  out$dist_matrix <- (learned_dist + t(learned_dist)) / 2
  diag(out$dist_matrix) <- 0
  out$combined_dist <- stats::as.dist(out$dist_matrix)
  out$species_dist <- out$combined_dist
  out
}

# - Alchemist distance learner -

#' Normalize the Alchemist out-of-fold split mode
#'
#' @param mode Requested OOF mode.
#'
#' @return Character scalar.
#'
#' @keywords internal
#' @noRd
normalize_alchemist_oof_mode <- function(mode) {
  match.arg(
    as.character(mode %||% "anchor_species"),
    c("anchor_species", "species_purged")
  )
}

#' Build out-of-fold split definitions for Alchemist training
#'
#' @param training_data Pair-level training table.
#' @param inner_folds Number of folds.
#' @param seed Optional random seed.
#' @param oof_mode Split mode.
#'
#' @return List of fold definitions, or `NULL`.
#'
#' @keywords internal
#' @noRd
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
        holdout_groups = if (".split_group" %in% names(training_data)) {
          unique(as.character(training_data$.split_group[foldid == fold_now]))
        } else {
          character(0)
        },
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
#' recognised by `fit_base_learner()`. Unrecognised names are silently
#' dropped with a warning.
#'
#' @param methods Character vector of requested method names.
#'
#' @return Character vector of validated method names, length >= 1.
#'
#' @keywords internal
#' @noRd
resolve_learner_methods <- function(methods,
                                    method_settings = NULL) {
  # Build the public Alchemist method namespace from the shared method catalog.
  catalog <- meta_policy_method_catalog(method_settings = method_settings)
  supported_families <- c(
    "glm",
    "glm_penalized",
    "gam",
    "mars",
    "rpart",
    "rf",
    "xgboost",
    "qreg"
  )
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
    methods <- c("glm_elastic", "rf", "xgboost")
  }
  methods
}

#' Build a numeric feature matrix from Alchemist training or prediction data
#'
#' All Gower distance features are numeric and bounded between 0 and 1. Missing values
#' are imputed at 1 so unknown metadata carries the maximum distance.
#'
#' @param data Tibble or data frame containing at least the columns named by
#'   `feature_cols`.
#' @param feature_cols Character vector of Gower distance column names.
#'
#' @return A numeric matrix with `nrow(data)` rows and `length(feature_cols)`
#'   columns.
#'
#' @keywords internal
#' @noRd
feature_matrix <- function(data, feature_cols) {
  mat <- as.matrix(data[, feature_cols, drop = FALSE])
  mode(mat) <- "numeric"
  mat[!is.finite(mat)] <- 1
  mat
}

#' Derive per-method settings from a shared settings list
#'
#' Extracts the settings block for one public learner method name.
#'
#' @param family Character. One of `"glm"`, `"glm_penalized"`, `"gam"`,
#'   `"rpart"`, `"rf"`, `"xgboost"`.
#' @param method Character. Full method name, e.g. `"glm_elastic"`.
#' @param method_settings Named list of per-method settings from config.
#'
#' @return Named list of resolved settings for this method.
#'
#' @keywords internal
#' @noRd
alchemist_method_spec <- function(method,
                                  method_settings = NULL) {
  method <- stringr::str_squish(as.character(method %||% ""))[[1]]
  catalog <- meta_policy_method_catalog(method_settings = method_settings)
  spec <- catalog$specs[[method]] %||% NULL
  if (!is.list(spec)) {
    family_map <- meta_policy_method_family_map()
    family <- family_map[[method]] %||% NULL
    if (!is.null(family)) {
      spec <- list(family = family, base = method, variant = NULL)
    }
  }
  supported_families <- c(
    "glm",
    "glm_penalized",
    "gam",
    "mars",
    "rpart",
    "rf",
    "xgboost",
    "qreg"
  )
  if (!is.list(spec) || !spec$family %in% supported_families) {
    stop(sprintf("Unknown Alchemist base learner method: %s", method), call. = FALSE)
  }
  c(list(name = method), spec)
}

#' Derive per-method settings from a shared settings list
#'
#' Extracts the settings block for one public learner method name.
#'
#' @param family Character. One of `"glm"`, `"glm_penalized"`, `"gam"`,
#'   `"rpart"`, `"rf"`, `"xgboost"`, `"mars"`, or `"qreg"`.
#' @param method Character. Full method name, e.g. `"glm_elastic"`.
#' @param method_settings Named list of per-method settings from config.
#'
#' @return Named list of resolved settings for this method.
#'
#' @keywords internal
#' @noRd
learner_method_settings <- function(family, method, method_settings) {
  method_settings <- normalize_meta_policy_method_settings(method_settings)
  method_spec <- alchemist_method_spec(method, method_settings = method_settings)
  family_cfg <- method_settings[[method_spec$family]] %||% list()
  base_cfg <- method_settings[[method_spec$base]] %||% list()
  variant_cfg <- if (!is.null(method_spec$variant)) {
    base_cfg$variants[[method_spec$variant]] %||% list()
  } else {
    list()
  }
  family_cfg$variants <- NULL
  base_cfg$variants <- NULL
  as.list(merge_config_sections(
    merge_config_sections(family_cfg, base_cfg),
    variant_cfg
  ))
}

#' Prepare one dense regression frame for linear-style Alchemist learners
#'
#' @param x_train Numeric feature matrix.
#'
#' @return A list with pruned `data` and retained `feature_cols`.
#'
#' @keywords internal
#' @noRd
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
#' @param method Character method name. See `resolve_learner_methods()` for
#'   the supported set.
#' @param method_settings Named list of per-method tuning settings.
#' @param seed Integer random seed.
#' @param lambda_rule Character. One of `"lambda.1se"` or `"lambda.min"`.
#'   Only used by glm-penalized methods.
#'
#' @return A list with class `"BaseLearner"` and fields
#'   `fit`, `method`, `family`, `lambda_rule`.
#'
#' @keywords internal
#' @noRd
fit_base_learner <- function(x_train, y_train, method,
                             method_settings = NULL,
                             seed = NULL,
                             lambda_rule = "lambda.1se") {
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  method_spec <- alchemist_method_spec(method, method_settings = method_settings)
  family <- method_spec$family

  ms <- learner_method_settings(family, method, method_settings %||% list())
  method_defaults <- meta_policy_method_default_arguments(
    method_spec$base,
    defaults_path = meta_policy_method_settings_defaults_path(method_settings)
  )
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
    glm_penalized = {
      alpha_val <- if (identical(method, "glm_elastic")) {
        as.numeric(ms$alpha %||% method_defaults$alpha %||% 0.25)
      } else {
        as.numeric(method_defaults$alpha %||% 0.25)
      }
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
    mars = {
      if (!requireNamespace("earth", quietly = TRUE)) {
        stop("Fitting Alchemist method 'mars' requires the suggested package 'earth' to be installed.", call. = FALSE)
      }
      earth_args <- list(
        x = x_train,
        y = y_train,
        degree = as.integer(ms$degree %||% 2L),
        penalty = as.numeric(ms$penalty %||% 3),
        pmethod = as.character(ms$pmethod %||% "backward")
      )
      if (!is.null(ms$nprune)) {
        earth_args$nprune <- as.integer(ms$nprune)
      }
      do.call(earth::earth, earth_args)
    },
    rf = {
      ranger_args <- list(
        x = as.data.frame(x_train, check.names = FALSE),
        y = y_train,
        num.threads = 1L,
        verbose = FALSE,
        num.trees = as.integer(ms$num_trees %||% 500L),
        min.node.size = as.integer(ms$min_node_size %||% 5L),
        sample.fraction = as.numeric(ms$sample_fraction %||% 1.0),
        replace = isTRUE(ms$replace %||% TRUE),
        respect.unordered.factors = as.character(
          ms$respect_unordered_factors %||% "order"
        )
      )
      if (!is.null(ms$mtry)) {
        ranger_args$mtry <- as.integer(ms$mtry)
      }
      # `as.integer(NULL)` is `integer(0)`, which Ranger's compiled interface
      # rejects. Leave the argument absent when the caller intentionally uses
      # Ranger's default RNG behavior.
      if (!is.null(seed)) {
        ranger_args$seed <- as.integer(seed)
      }
      if (!is.null(ms$max_depth)) {
        ranger_args$max.depth <- as.integer(ms$max_depth)
      }
      rf <- do.call(ranger::ranger, ranger_args)
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
    qreg = {
      if (!requireNamespace("quantreg", quietly = TRUE)) {
        stop("Fitting Alchemist method 'qreg' requires the suggested package 'quantreg' to be installed.", call. = FALSE)
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
#' @param object A `"BaseLearner"` from `fit_base_learner()`.
#' @param x_new Numeric feature matrix (prediction rows).
#'
#' @return Numeric prediction vector, length `nrow(x_new)`.
#'
#' @keywords internal
#' @noRd
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
    glm_penalized = {
      pfn <- utils::getFromNamespace("predict.cv.glmnet", "glmnet")
      as.numeric(pfn(object$fit, newx = x_new, s = object$lambda_rule %||% "lambda.1se"))
    },
    gam = {
      pfn <- utils::getFromNamespace("predict.gam", "mgcv")
      as.numeric(pfn(object$fit, newdata = df_new, type = "response"))
    },
    mars = {
      pfn <- utils::getFromNamespace("predict.earth", "earth")
      as.numeric(pfn(object$fit, newdata = x_new, type = "response"))
    },
    rf = {
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
    qreg = {
      pfn <- utils::getFromNamespace("predict.rq", "quantreg")
      as.numeric(pfn(object$fit, newdata = df_new))
    },
    stop(sprintf("Unsupported Alchemist base learner family: '%s'.", object$family),
      call. = FALSE
    )
  )
}

#' Run one Alchemist out-of-fold base learner task
#'
#' This is the method-fold worker used by the Super Learner OOF stage. Keeping
#' this unit at one method and one fold lets cloud workers drain a balanced task
#' queue instead of leaving one long-running learner to occupy a single core.
#'
#' @param task Named list with `method`, `fold_id`, and `fold_spec`.
#' @param x_all Numeric feature matrix for the full training set.
#' @param y_all Numeric outcome vector (transformed), full training set.
#' @param method_settings Named list of per-method settings.
#' @param seed Integer base seed. The fold offsets this by its fold index.
#' @param lambda_rule Character. Lambda selection rule for glm-penalized methods.
#'
#' @return A named list with fields `method`, `fold_id`, `valid_idx`, `pred`,
#'   `error`, and `oof_seconds`.
#'
#' @keywords internal
#' @noRd
run_oof_fold_task <- function(task, x_all, y_all,
                              method_settings, seed, lambda_rule) {
  timing_start <- proc.time()
  method <- as.character(task$method[[1]])
  fold_spec <- task$fold_spec
  fold_now <- as.integer(task$fold_id %||% fold_spec$fold_id %||% NA_integer_)
  train_idx <- as.integer(fold_spec$train_idx %||% integer(0))
  valid_idx <- as.integer(fold_spec$valid_idx %||% integer(0))
  pred <- NULL
  err_msg <- NULL

  if (length(train_idx) == 0L || length(valid_idx) == 0L) {
    err_msg <- "Fold has zero training or validation rows."
  } else {
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
      err_msg <- as.character(learner)
    } else {
      pred <- tryCatch(
        predict_base_learner(learner, x_all[valid_idx, , drop = FALSE]),
        error = function(e) structure(conditionMessage(e), class = "try-error")
      )

      if (inherits(pred, "try-error") || length(pred) != length(valid_idx)) {
        err_msg <- if (inherits(pred, "try-error")) {
          as.character(pred)
        } else {
          "Prediction length mismatch."
        }
        pred <- NULL
      }
    }
  }

  list(
    method = method,
    fold_id = fold_now,
    valid_idx = valid_idx,
    n_train = length(train_idx),
    n_valid = length(valid_idx),
    pred = pred,
    error = err_msg,
    oof_seconds = unname((proc.time() - timing_start)[["elapsed"]])
  )
}

# Namespace-local payload used by forked Alchemist OOF workers.
.alchemist_oof_payload <- new.env(parent = emptyenv())

#' Set the namespace-local Alchemist OOF payload
#'
#' Stores the shared feature matrix, outcome vector, and learner settings in a
#' namespace-local environment. Fork workers inherit this environment by
#' copy-on-write so the feature matrix is not re-serialized for every
#' method-fold task.
#'
#' @param payload Named list containing `x_all`, `y_all`, `method_settings`,
#'   `seed`, and `lambda_rule`.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
set_alchemist_oof_payload <- function(payload) {
  rm(
    list = ls(envir = .alchemist_oof_payload, all.names = TRUE),
    envir = .alchemist_oof_payload
  )
  list2env(payload, envir = .alchemist_oof_payload)
  invisible(NULL)
}

#' Clear the namespace-local Alchemist OOF payload
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
clear_alchemist_oof_payload <- function() {
  rm(
    list = ls(envir = .alchemist_oof_payload, all.names = TRUE),
    envir = .alchemist_oof_payload
  )
  invisible(NULL)
}

#' Run one Alchemist OOF method-fold task from an explicit payload
#'
#' @param task Named list with `method`, `fold_id`, and `fold_spec`.
#' @param payload Named list containing `x_all`, `y_all`, `method_settings`,
#'   `seed`, and `lambda_rule`.
#'
#' @return Same result shape as [run_oof_fold_task()].
#' @keywords internal
#' @noRd
run_alchemist_oof_payload_task <- function(task, payload) {
  run_oof_fold_task(
    task,
    x_all = payload$x_all,
    y_all = payload$y_all,
    method_settings = payload$method_settings,
    seed = payload$seed,
    lambda_rule = payload$lambda_rule
  )
}

#' Run one inherited Alchemist OOF method-fold task
#'
#' Fetches the feature matrix and learner settings from the namespace-local
#' fork-worker payload and evaluates one method-fold task.
#'
#' @param task Named list with `method`, `fold_id`, and `fold_spec`.
#'
#' @return Same result shape as [run_oof_fold_task()].
#' @keywords internal
#' @noRd
run_alchemist_oof_task <- function(task) {
  payload <- as.list(.alchemist_oof_payload)
  run_alchemist_oof_payload_task(task, payload)
}

#' Build a failed Alchemist OOF method-fold task result
#'
#' Creates the same result shape as a completed method-fold task when a worker
#' connection failure (for example, an OOM-killed forked worker) prevents the
#' task from returning its own diagnostics.
#'
#' @param task Named list with `method`, `fold_id`, and `fold_spec`.
#' @param message Failure message to attach.
#'
#' @return Named list matching [run_oof_fold_task()]'s result shape.
#' @keywords internal
#' @noRd
failed_alchemist_oof_task <- function(task, message) {
  fold_spec <- task$fold_spec
  list(
    method = as.character(task$method[[1]]),
    fold_id = as.integer(task$fold_id %||% fold_spec$fold_id %||% NA_integer_),
    valid_idx = as.integer(fold_spec$valid_idx %||% integer(0)),
    n_train = NA_integer_,
    n_valid = NA_integer_,
    pred = NULL,
    error = as.character(message %||% "method-fold task failed"),
    oof_seconds = NA_real_
  )
}

#' Format Alchemist OOF method-fold task labels
#'
#' @param tasks One task or a list of method-fold tasks.
#' @param max_labels Maximum number of labels to include.
#'
#' @return One comma-separated task-label string.
#' @keywords internal
#' @noRd
format_alchemist_oof_tasks <- function(tasks, max_labels = 12L) {
  if (is.null(tasks) || length(tasks) == 0L) {
    return("none")
  }
  if (!is.list(tasks[[1L]])) {
    tasks <- list(tasks)
  }
  max_labels <- max(1L, as.integer(max_labels %||% 12L))
  labels <- vapply(
    tasks,
    function(task) {
      sprintf(
        "fold=%d method=%s",
        as.integer(task$fold_id %||% task$fold_spec$fold_id %||% NA_integer_),
        as.character(task$method[[1]])
      )
    },
    character(1)
  )
  if (length(labels) > max_labels) {
    labels <- c(labels[seq_len(max_labels)], sprintf("... +%d more", length(labels) - max_labels))
  }
  paste(labels, collapse = ", ")
}

#' Log Alchemist OOF method-fold task completion
#'
#' @param result Method-fold task result.
#' @param completed_n Number of completed tasks.
#' @param total_tasks Total number of method-fold tasks.
#' @param progress Logical. Emit progress messages.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
log_alchemist_oof_task_result <- function(result, completed_n, total_tasks, progress = FALSE) {
  success_now <- is.null(result$error) && !is.null(result$pred)
  summary <- sprintf(
    "  %s Method-fold task complete: %d/%d [fold=%d method=%s success=%s].",
    if (success_now) "\u2705" else "\u274c",
    completed_n,
    total_tasks,
    as.integer(result$fold_id),
    as.character(result$method),
    success_now
  )
  if (success_now) {
    report_progress(progress, summary)
  } else {
    report_progress(
      progress,
      paste0(summary, "\n", as.character(result$error %||% "Unknown method-fold task failure."))
    )
  }
  invisible(NULL)
}

#' Split an Alchemist OOF method-fold suspect group
#'
#' Bisects a suspect task group after a worker-connection failure so only the
#' tasks that keep failing are narrowed further.
#'
#' @param tasks List of method-fold tasks.
#'
#' @return List of one or two task groups.
#' @keywords internal
#' @noRd
split_alchemist_oof_suspect_group <- function(tasks) {
  if (length(tasks) == 0L) {
    return(list())
  }
  if (length(tasks) <= 1L) {
    return(list(tasks))
  }
  cut <- floor(length(tasks) / 2L)
  list(tasks[seq_len(cut)], tasks[(cut + 1L):length(tasks)])
}

#' Run one live Alchemist OOF method-fold queue
#'
#' Submits method-fold tasks to worker slots, collects each result as soon as
#' it returns, and submits the next pending task to the freed worker. A socket
#' failure returns completed results plus the in-flight and unstarted tasks so
#' the caller can isolate the suspect set.
#'
#' @param tasks List of method-fold tasks.
#' @param payload Named list containing `x_all`, `y_all`, `method_settings`,
#'   `seed`, and `lambda_rule`.
#' @param workers Requested worker budget.
#' @param progress Logical. Emit progress messages.
#' @param completed_start Count of tasks already completed before this queue.
#' @param total_tasks Total number of method-fold tasks across all queues.
#' @param retry_mode Logical. `TRUE` when re-running an isolated suspect group.
#'
#' @return List with `completed`, `completed_n`, `failed_active`, `unstarted`,
#'   and `error`.
#' @keywords internal
#' @noRd
alchemist_oof_queue_once <- function(tasks,
                                     payload,
                                     workers,
                                     progress = FALSE,
                                     completed_start = 0L,
                                     total_tasks = length(tasks),
                                     retry_mode = FALSE) {
  if (length(tasks) == 0L) {
    return(list(
      completed = list(),
      completed_n = as.integer(completed_start),
      failed_active = list(),
      unstarted = list(),
      error = NULL
    ))
  }
  workers <- max(1L, min(as.integer(workers %||% 1L), length(tasks)))
  report_progress(
    progress,
    sprintf(
      "  Method-fold queue: dispatching %d %stask(s) across %d worker(s).",
      length(tasks),
      if (retry_mode) "retry " else "",
      workers
    )
  )

  run_parent_sequential <- workers <= 1L && .Platform$OS.type != "unix"
  if (run_parent_sequential) {
    completed <- list()
    completed_n <- as.integer(completed_start)
    for (task in tasks) {
      result <- tryCatch(
        run_alchemist_oof_payload_task(task, payload),
        error = function(e) failed_alchemist_oof_task(task, conditionMessage(e))
      )
      completed_n <- completed_n + 1L
      log_alchemist_oof_task_result(result, completed_n, total_tasks, progress)
      completed[[length(completed) + 1L]] <- result
    }
    return(list(
      completed = completed,
      completed_n = completed_n,
      failed_active = list(),
      unstarted = list(),
      error = NULL
    ))
  }

  set_alchemist_oof_payload(payload)
  cluster_obj <- if (workers <= 1L && .Platform$OS.type == "unix") {
    cl <- parallel::makeForkCluster(1L)
    attr(cl, "cluster_type") <- "fork"
    cl
  } else {
    initialize_parallel_cluster(workers = workers)
  }
  cluster_type <- attr(cluster_obj, "cluster_type", exact = TRUE) %||% "unknown"
  if (!identical(cluster_type, "fork")) {
    run_method_task <- function(task) {
      run_alchemist_oof_payload_task(task, payload)
    }
    environment(run_method_task) <- list2env(
      list(
        payload = payload,
        run_alchemist_oof_payload_task = run_alchemist_oof_payload_task,
        run_oof_fold_task = run_oof_fold_task
      ),
      parent = baseenv()
    )
    tsb_cluster_export(cluster_obj, c("run_method_task"), envir = environment())
  }

  pending <- tasks
  active <- vector("list", length(cluster_obj))
  names(active) <- as.character(seq_along(cluster_obj))
  parallel_send_call <- utils::getFromNamespace("sendCall", "parallel")
  parallel_recv_one_result <- utils::getFromNamespace("recvOneResult", "parallel")
  submit_task <- function(node_id, task) {
    if (identical(cluster_type, "fork")) {
      parallel_send_call(cluster_obj[[node_id]], run_alchemist_oof_task, list(task))
    } else {
      parallel_send_call(cluster_obj[[node_id]], run_method_task, list(task))
    }
    active[[as.character(node_id)]] <<- task
    invisible(NULL)
  }

  for (node_id in seq_along(cluster_obj)) {
    if (length(pending) == 0L) {
      break
    }
    submit_task(node_id, pending[[1L]])
    pending <- pending[-1L]
  }
  report_progress(
    progress,
    "  Method-fold queue active tasks: ",
    format_alchemist_oof_tasks(Filter(Negate(is.null), active))
  )

  completed <- list()
  completed_n <- as.integer(completed_start)
  queue_error <- NULL
  while (length(Filter(Negate(is.null), active)) > 0L) {
    received <- tryCatch(
      parallel_recv_one_result(cluster_obj),
      error = function(e) e
    )
    if (inherits(received, "error")) {
      queue_error <- conditionMessage(received)
      break
    }
    node_id <- as.character(received$node)
    task_done <- active[[node_id]]
    active[node_id] <- list(NULL)
    result <- if (inherits(received$value, "try-error") || inherits(received$value, "snow-try-error")) {
      failed_alchemist_oof_task(
        task_done,
        as.character(received$value %||% "worker returned an error")
      )
    } else {
      received$value
    }
    completed_n <- completed_n + 1L
    log_alchemist_oof_task_result(result, completed_n, total_tasks, progress)
    completed[[length(completed) + 1L]] <- result
    if (length(pending) > 0L) {
      submit_task(as.integer(node_id), pending[[1L]])
      pending <- pending[-1L]
    }
  }

  failed_active <- Filter(Negate(is.null), active)
  try(parallel::stopCluster(cluster_obj), silent = TRUE)
  clear_alchemist_oof_payload()
  list(
    completed = completed,
    completed_n = completed_n,
    failed_active = failed_active,
    unstarted = pending,
    error = queue_error
  )
}

#' Run Alchemist Super Learner OOF method-fold tasks through a worker queue
#'
#' Executes method-fold tasks as queue items. The scheduler submits work up to
#' the requested worker budget, collects each task as soon as a worker
#' returns, and immediately submits the next task to the freed worker. If a
#' worker connection fails (for example, a forked worker killed by the OS for
#' exceeding available memory), completed task results are retained and the
#' in-flight suspect set is bisected until only the failing task group is
#' isolated, so one worker crash degrades that task rather than aborting the
#' whole `forge_distances()` stage.
#'
#' @param tasks List of method-fold tasks.
#' @param payload Named list containing `x_all`, `y_all`, `method_settings`,
#'   `seed`, and `lambda_rule`.
#' @param workers Requested worker budget.
#' @param progress Logical. Emit progress messages.
#'
#' @return List of method-fold task results.
#' @keywords internal
#' @noRd
run_alchemist_oof_tasks <- function(tasks, payload, workers, progress = FALSE) {
  total_tasks <- length(tasks)
  if (total_tasks == 0L) {
    return(list())
  }
  requested_workers <- max(1L, min(as.integer(workers %||% 1L), total_tasks))
  report_progress(
    progress,
    sprintf(
      "Alchemist OOF method-fold scheduler: queue mode, %d total task(s), up to %d worker(s).",
      total_tasks,
      requested_workers
    )
  )

  completed_results <- list()
  pending_tasks <- tasks
  retry_groups <- list()
  completed_n <- 0L

  while (length(pending_tasks) > 0L || length(retry_groups) > 0L) {
    retry_mode <- length(retry_groups) > 0L
    if (retry_mode) {
      queue_tasks <- retry_groups[[1L]]
      retry_groups <- retry_groups[-1L]
      attempt_workers <- min(requested_workers, length(queue_tasks))
    } else {
      queue_tasks <- pending_tasks
      attempt_workers <- min(requested_workers, length(queue_tasks))
    }

    if (attempt_workers <= 1L && !retry_mode) {
      report_progress(
        progress,
        sprintf(
          "  Method-fold queue: running %d task(s) sequentially.",
          length(queue_tasks)
        )
      )
      for (task in queue_tasks) {
        result <- tryCatch(
          run_alchemist_oof_payload_task(task, payload),
          error = function(e) failed_alchemist_oof_task(task, conditionMessage(e))
        )
        completed_n <- completed_n + 1L
        log_alchemist_oof_task_result(result, completed_n, total_tasks, progress)
        completed_results[[length(completed_results) + 1L]] <- result
      }
      pending_tasks <- list()
      next
    }

    queue_result <- alchemist_oof_queue_once(
      tasks = queue_tasks,
      payload = payload,
      workers = attempt_workers,
      progress = progress,
      completed_start = completed_n,
      total_tasks = total_tasks,
      retry_mode = retry_mode
    )
    completed_results <- c(completed_results, queue_result$completed)
    completed_n <- queue_result$completed_n
    if (is.null(queue_result$error)) {
      if (retry_mode) {
        if (length(queue_result$unstarted) > 0L) {
          retry_groups <- c(retry_groups, list(queue_result$unstarted))
        }
      } else {
        pending_tasks <- list()
      }
    } else {
      isolated_failure <- retry_mode &&
        length(queue_result$failed_active) == 1L &&
        length(queue_tasks) == 1L
      if (isolated_failure) {
        result <- failed_alchemist_oof_task(
          queue_result$failed_active[[1L]],
          paste("socket failure isolated to this method-fold task:", queue_result$error)
        )
        completed_n <- completed_n + 1L
        completed_results[[length(completed_results) + 1L]] <- result
        report_progress(
          progress,
          paste0(
            sprintf(
              "  \u274c Method-fold task isolated after socket failure: %d/%d [fold=%d method=%s success=FALSE].",
              completed_n,
              total_tasks,
              as.integer(result$fold_id),
              as.character(result$method)
            ),
            "\n",
            as.character(result$error %||% "Unknown method-fold task failure.")
          )
        )
      } else {
        suspect_groups <- split_alchemist_oof_suspect_group(queue_result$failed_active)
        if (retry_mode) {
          retry_groups <- c(
            suspect_groups,
            if (length(queue_result$unstarted) > 0L) list(queue_result$unstarted) else list(),
            retry_groups
          )
        } else {
          retry_groups <- c(suspect_groups, retry_groups)
          pending_tasks <- queue_result$unstarted
        }
        report_progress(
          progress,
          sprintf(
            "  \u26a0 Method-fold queue failed while active tasks were [%s]; retaining completed tasks and splitting suspect set into %d group(s); %d pending task(s) remain at full worker budget: %s",
            format_alchemist_oof_tasks(queue_result$failed_active),
            length(suspect_groups),
            length(pending_tasks),
            queue_result$error
          )
        )
      }
    }
  }

  completed_results
}

#' Run K-fold OOF predictions for one Alchemist base learner method
#'
#' This sequential fallback runs all K folds for one assigned method and returns
#' a named list with the OOF prediction vector and any error captured.
#'
#' @param method Character. Single base learner method name.
#' @param x_all Numeric feature matrix for the full training set.
#' @param y_all Numeric outcome vector (transformed), full training set.
#' @param foldid Integer fold-ID vector, length `nrow(x_all)`.
#' @param method_settings Named list of per-method settings.
#' @param seed Integer base seed. Each fold offsets this by its fold index.
#' @param lambda_rule Character. Lambda selection rule for glm-penalized methods.
#'
#' @return A named list with fields `method`, `oof_pred` (numeric vector or
#'   `NULL`), and `error` (character or `NULL`).
#'
#' @keywords internal
#' @noRd
run_oof_method <- function(method, x_all, y_all, foldid = NULL,
                           fold_splits = NULL,
                           method_settings, seed, lambda_rule) {
  timing_start <- proc.time()
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

  fold_results <- lapply(fold_splits, function(fold_spec) {
    run_oof_fold_task(
      task = list(
        method = method,
        fold_id = fold_spec$fold_id %||% NA_integer_,
        fold_spec = fold_spec
      ),
      x_all = x_all,
      y_all = y_all,
      method_settings = method_settings,
      seed = seed,
      lambda_rule = lambda_rule
    )
  })

  for (fold_result in fold_results) {
    valid_idx <- as.integer(fold_result$valid_idx %||% integer(0))
    if (!is.null(fold_result$error) || is.null(fold_result$pred)) {
      ok <- FALSE
      err_msg <- paste0(
        "fold ", fold_result$fold_id %||% NA_integer_, ": ",
        fold_result$error %||% "unknown error"
      )
      break
    }
    if (length(valid_idx) == 0L || length(fold_result$pred) != length(valid_idx)) {
      ok <- FALSE
      err_msg <- paste0("fold ", fold_result$fold_id %||% NA_integer_, ": Prediction length mismatch.")
      break
    }
    oof_pred[valid_idx] <- fold_result$pred
  }

  list(
    method = method,
    oof_pred = if (ok && all(is.finite(oof_pred))) oof_pred else NULL,
    error = err_msg,
    oof_seconds = unname((proc.time() - timing_start)[["elapsed"]]),
    fold_timings = dplyr::bind_rows(lapply(fold_results, function(fold_result) {
      tibble::tibble(
        method = fold_result$method,
        fold_id = as.integer(fold_result$fold_id %||% NA_integer_),
        n_train = as.integer(fold_result$n_train %||% NA_integer_),
        n_valid = as.integer(fold_result$n_valid %||% NA_integer_),
        oof_seconds = suppressWarnings(as.numeric(fold_result$oof_seconds %||% NA_real_)),
        succeeded = is.null(fold_result$error) && !is.null(fold_result$pred),
        error = fold_result$error %||% NA_character_
      )
    }))
  )
}

#' Fit a Super Learner ensemble for Alchemist pairwise distance learning
#'
#' Trains a stacked ensemble over a library of base learners using K-fold
#' out-of-fold (OOF) cross-validation. Base learner fits are parallelised
#' across method-fold tasks when `workers > 1`. NNLS stacking weights are
#' computed from the OOF predictions; a final round of base learner fits on the
#' full training set is then used for prediction at inference time.
#'
#' This function is entirely independent of the policy-learner path in
#' `meta_policy.R`. It is purpose-built for learning pairwise acoustic
#' distances from Gower trait features.
#'
#' @param training_data Tibble produced by `build_pair_data()`.
#'   Must contain `.outcome`, optionally `.split_group`, and all columns named
#'   by `feature_cols`.
#' @param feature_cols Character vector of Gower distance column names
#'   (e.g. `.dist_swimbladder_type`).
#' @param methods Character vector of base learner method names. See
#'   `resolve_learner_methods()` for the supported set.
#' @param outcome_transform One of `"log1p"` or `"identity"`. Applied to
#'   `.outcome` before fitting; predictions are back-transformed before
#'   storage.
#' @param lambda_rule Lambda selection rule for glm-penalized base learners.
#'   One of `"lambda.1se"` or `"lambda.min"`.
#' @param inner_folds Integer number of cross-validation folds.
#' @param seed Integer random seed for fold assignment and base learner fits.
#' @param method_settings Named list of per-method tuning overrides (same
#'   structure as `selection.method_settings` in the config YAML).
#' @param oof_mode Cross-validation split mode. `"anchor_species"` keeps the
#'   current receiving-species grouping; `"species_purged"` removes the held-out
#'   species from both anchor and donor roles in each OOF fold.
#' @param workers Integer number of parallel workers. OOF workers are assigned
#'   one method-fold task each. `1L` runs sequentially.
#' @param progress Logical. Emit `tsb_message()` progress lines when `TRUE`.
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
#' @noRd
fit_super_learner <- function(training_data,
                              feature_cols,
                              methods,
                              outcome_transform = "log1p",
                              lambda_rule = "lambda.1se",
                              weight_rule = "nnls",
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
  lambda_rule <- normalize_alchemist_lambda_rule(lambda_rule)
  weight_rule <- normalize_alchemist_weight_rule(weight_rule)

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

  oof_tasks <- unlist(
    lapply(methods, function(method) {
      lapply(fold_splits, function(fold_spec) {
        list(
          method = method,
          fold_id = as.integer(fold_spec$fold_id %||% NA_integer_),
          fold_spec = fold_spec
        )
      })
    }),
    recursive = FALSE
  )
  oof_workers <- min(max(1L, workers), length(oof_tasks))

  report_progress(
    progress,
    "[Alchemist] Fitting ", length(methods), " base learner(s) with ",
    length(fold_splits), "-fold CV",
    " [mode=", oof_mode, "]",
    if (oof_workers > 1L) {
      paste0(" across ", oof_workers, " worker(s) over ", length(oof_tasks), " method-fold task(s)")
    } else {
      " (sequential)"
    },
    "..."
  )

  # ---- OOF phase: parallelise across method-fold tasks ----------------------
  # Dispatched through a fault-isolating queue (not a bare parLapplyLB) so a
  # single worker crash - e.g. a forked base-learner process killed by the OS
  # for exceeding available memory - degrades just that method-fold task
  # instead of taking down the whole forge_distances() stage.
  oof_task_results <- run_alchemist_oof_tasks(
    tasks = oof_tasks,
    payload = list(
      x_all = x_all,
      y_all = y_all,
      method_settings = method_settings,
      seed = seed,
      lambda_rule = lambda_rule
    ),
    workers = oof_workers,
    progress = progress
  )

  oof_fold_timings <- dplyr::bind_rows(lapply(oof_task_results, function(fold_result) {
    tibble::tibble(
      method = fold_result$method,
      fold_id = as.integer(fold_result$fold_id %||% NA_integer_),
      n_train = as.integer(fold_result$n_train %||% NA_integer_),
      n_valid = as.integer(fold_result$n_valid %||% NA_integer_),
      oof_seconds = suppressWarnings(as.numeric(fold_result$oof_seconds %||% NA_real_)),
      succeeded = is.null(fold_result$error) && !is.null(fold_result$pred),
      error = fold_result$error %||% NA_character_
    )
  }))

  oof_results <- stats::setNames(vector("list", length(methods)), methods)
  for (method in methods) {
    method_task_results <- Filter(
      function(fold_result) identical(as.character(fold_result$method), as.character(method)),
      oof_task_results
    )
    oof_pred <- rep(NA_real_, length(y_all))
    method_error <- NULL

    for (fold_result in method_task_results) {
      valid_idx <- as.integer(fold_result$valid_idx %||% integer(0))
      if (!is.null(fold_result$error) || is.null(fold_result$pred)) {
        method_error <- paste0(
          "fold ", fold_result$fold_id %||% NA_integer_, ": ",
          fold_result$error %||% "unknown error"
        )
        break
      }
      if (length(valid_idx) == 0L || length(fold_result$pred) != length(valid_idx)) {
        method_error <- paste0("fold ", fold_result$fold_id %||% NA_integer_, ": Prediction length mismatch.")
        break
      }
      oof_pred[valid_idx] <- fold_result$pred
    }

    if (is.null(method_error) && !all(is.finite(oof_pred))) {
      method_error <- "Incomplete OOF predictions."
    }

    oof_results[[method]] <- list(
      method = method,
      oof_pred = if (is.null(method_error)) oof_pred else NULL,
      error = method_error,
      oof_seconds = sum(vapply(
        method_task_results,
        function(fold_result) suppressWarnings(as.numeric(fold_result$oof_seconds %||% NA_real_)),
        numeric(1)
      ), na.rm = TRUE),
      fold_timings = dplyr::bind_rows(lapply(method_task_results, function(fold_result) {
        tibble::tibble(
          method = fold_result$method,
          fold_id = as.integer(fold_result$fold_id %||% NA_integer_),
          n_train = as.integer(fold_result$n_train %||% NA_integer_),
          n_valid = as.integer(fold_result$n_valid %||% NA_integer_),
          oof_seconds = suppressWarnings(as.numeric(fold_result$oof_seconds %||% NA_real_)),
          succeeded = is.null(fold_result$error) && !is.null(fold_result$pred),
          error = fold_result$error %||% NA_character_
        )
      }))
    )
  }

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

  weights <- fit_super_learner_weights(
    oof_mat_transformed,
    y_all,
    rule = weight_rule
  )
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
    timing_start <- proc.time()
    learner <- tryCatch(
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
    list(
      learner = learner,
      refit_seconds = unname((proc.time() - timing_start)[["elapsed"]])
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

  final_learners <- lapply(refit_list, `[[`, "learner")
  final_learners <- Filter(Negate(is.null), final_learners)

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

  learner_timings <- tibble::tibble(
    method = methods,
    oof_seconds = vapply(methods, function(method) {
      suppressWarnings(as.numeric(oof_results[[method]]$oof_seconds %||% NA_real_))
    }, numeric(1)),
    refit_seconds = vapply(methods, function(method) {
      if (!method %in% names(refit_list)) {
        return(NA_real_)
      }
      suppressWarnings(as.numeric(refit_list[[method]]$refit_seconds %||% NA_real_))
    }, numeric(1))
  ) |>
    dplyr::mutate(
      total_seconds = rowSums(
        cbind(.data$oof_seconds, .data$refit_seconds),
        na.rm = TRUE
      ),
      succeeded_oof = .data$method %in% names(ok_methods),
      succeeded_refit = .data$method %in% names(final_learners)
    )

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
      weight_rule = weight_rule,
      inner_fold_splits = fold_splits,
      oof_predictions = tibble::as_tibble(oof_mat),
      oof_ensemble_prediction = oof_ensemble_pred,
      oof_performance = perf_tbl,
      oof_fold_timings = oof_fold_timings,
      learner_timings = learner_timings
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
#'   `fit_super_learner()`.
#' @param new_data Tibble or data frame containing at least the columns named by
#'   `object$feature_cols`.
#'
#' @return Numeric prediction vector, length `nrow(new_data)`, in the original
#'   (non-transformed) scale.
#'
#' @keywords internal
#' @noRd
predict_distance <- function(object, new_data) {
  if (inherits(object, "Mahalanobis")) {
    new_data <- tibble::as_tibble(new_data)
    x_new <- feature_matrix(new_data, object$feature_cols)
    prediction <- sqrt(pmax(0, as.numeric(x_new %*% object$weights)))
    attr(prediction, "distance_weighted_disagreement") <- rep(NA_real_, length(prediction))
    attr(prediction, "distance_diagnostic_available") <- FALSE
    return(prediction)
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
  prediction <- pmax(0, if (identical(object$outcome_transform, "log1p")) {
    expm1(pred_transformed)
  } else {
    pred_transformed
  })
  base_predictions <- if (identical(object$outcome_transform, "log1p")) {
    expm1(pred_mat[, names(weights), drop = FALSE])
  } else {
    pred_mat[, names(weights), drop = FALSE]
  }
  base_predictions[base_predictions < 0] <- 0
  weighted_center <- as.numeric(base_predictions %*% weights)
  disagreement <- sqrt(rowSums(
    sweep(base_predictions, 1L, weighted_center, "-")^2 *
      rep(as.numeric(weights), each = nrow(base_predictions))
  ))
  attr(prediction, "distance_base_predictions") <- base_predictions
  attr(prediction, "distance_weighted_disagreement") <- disagreement
  attr(prediction, "distance_diagnostic_available") <- TRUE
  prediction
}

# Fit a diagonal Mahalanobis distance via NNLS on squared pairwise differences.
# Predicts e_ij^2 = sum_k w_k * delta_k^2; distance = sqrt(predicted value).
#'
#' @keywords internal
#' @noRd
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

# - shared distance-learner reference helpers -

#' Wrap a fitted Alchemist distance learner in a reference-semantic container
#'
#' The fitted Super Learner ensemble is large (can exceed 1 GB). It gets
#' threaded into `Candidates@gower_distances$distance_learner` at multiple
#' pipeline stages (`screen_admissibility()`, `as_policyselector()`), and each
#' of those `Candidates`/`PolicySelector`/`Referee` objects is reconstructed
#' via S7 constructors as the workflow progresses. Ordinary R copy-on-write
#' keeps repeated list/S7-slot assignments as one shared object only as long
#' as nothing in between forces a copy - a single cache round-trip
#' (`saveRDS()`/`readRDS()`) anywhere in the chain silently turns "the same
#' learner passed along" into a second physical copy. Wrapping it in an
#' environment at the point it's fit, instead, makes every copy built from
#' that point forward - within one live process, however many S7
#' reconstructions or cache hits happen along the way - resolve back to one
#' stored value, because environments have true reference semantics that R's
#' serialization format preserves explicitly.
#'
#' @param learner Fitted Alchemist distance-learner object, or `NULL`.
#' @param fingerprint Optional stable identity string for this fit (e.g. the
#'   `forge_distances()` cache path). Used by [canonicalize_distance_learner()]
#'   to collapse independently-reloaded copies of the *same* fit - from
#'   separate sessions - back onto one shared reference when artifacts get
#'   combined later. `NULL` is fine; it just means that collapsing can't
#'   happen for this particular value.
#'
#' @return An environment wrapping `learner`, or `NULL` if `learner` is `NULL`.
#' @keywords internal
#' @noRd
share_distance_learner <- function(learner, fingerprint = NULL) {
  if (is.null(learner)) {
    return(NULL)
  }
  if (inherits(learner, "tsb_shared_distance_learner")) {
    return(learner)
  }
  env <- new.env(parent = emptyenv())
  env$learner <- learner
  env$fingerprint <- fingerprint
  class(env) <- "tsb_shared_distance_learner"
  env
}

#' Unwrap a fitted Alchemist distance learner
#'
#' Accepts either the environment-wrapped form produced by
#' `share_distance_learner()` or a raw (unwrapped) learner object, so callers
#' that construct a `gower_distances`/`distance_state` list by hand (tests,
#' pre-fix cached objects) keep working unchanged.
#'
#' @param x Wrapped or raw distance-learner value, or `NULL`.
#'
#' @return The raw learner object, or `NULL`.
#' @keywords internal
#' @noRd
resolve_distance_learner <- function(x) {
  if (inherits(x, "tsb_shared_distance_learner")) {
    return(x$learner)
  }
  x
}

# Session-scoped registry for canonicalize_distance_learner() below.
.tsb_distance_learner_registry <- new.env(parent = emptyenv())

#' Collapse a distance learner onto its canonical shared reference, if known
#'
#' Within one continuous session, `share_distance_learner()` at the fit point
#' already guarantees a single reference. This function additionally handles
#' the case where the *same* underlying fit was independently reloaded from
#' disk in separate sessions (each `readRDS()`/`qs2::qs_read()` necessarily
#' creates its own new environment) and those separately-built artifacts are
#' now being combined in one later session. It only ever compares a short
#' fingerprint string - never the fitted learner's contents - so this is cheap
#' enough to call defensively on every reconstruction.
#'
#' @param x Wrapped or raw distance-learner value, or `NULL`.
#'
#' @return `x`, or the canonical wrapper already registered for the same
#'   fingerprint if one exists.
#' @keywords internal
#' @noRd
canonicalize_distance_learner <- function(x) {
  if (!inherits(x, "tsb_shared_distance_learner") || is.null(x$fingerprint)) {
    return(x)
  }
  key <- x$fingerprint
  existing <- .tsb_distance_learner_registry[[key]]
  # The fingerprint is a cache path, not a data hash
  if (is.null(existing) ||
    !identical(sort(existing$learner$feature_cols), sort(x$learner$feature_cols))) {
    .tsb_distance_learner_registry[[key]] <- x
    return(x)
  }
  existing
}

# - forge_distances -

#' Learn the distance matrix for an `Alchemist`
#'
#' Fits the Alchemist distance learner and writes the learned geometry back to
#' the object. The method expands candidate models into directed model pairs,
#' constructs trait and coherence features, trains either the configured
#' ensemble learner or diagonal Mahalanobis learner, and predicts pairwise
#' transfer distances for the full candidate set.
#'
#' The returned object contains the fitted learner and a distance bundle with
#' the learned matrix, pairwise training data, feature columns, trait matrices,
#' out-of-fold performance, and sigma matrices used by later trait-importance
#' and policy-support diagnostics. Re-running this method clears previous
#' trait-importance, ordination, and admissibility results because those layers
#' depend on the learned distance geometry.
#'
#' @name forge_distances.Alchemist
#' @usage NULL
#'
#' @param object An [Alchemist] object with candidate models and configured
#'   species/study traits.
#' @param progress Optional logical scalar controlling progress messages.
#' @param feature_type Optional pairwise feature representation. Supported
#'   values include the configured default, `"gower"`, `"difference"`, and
#'   `"mahalanobis"`.
#' @param cache_path Optional cache file path. When `NULL`, the method derives
#'   the cache path from the Alchemist object config inherited from the
#'   Configurer.
#' @param refresh Optional logical scalar. When `NULL`, the method inherits the
#'   refresh setting from the Alchemist object config.
#' @param ... Additional learner-specific controls forwarded to the distance
#'   fitting stage.
#'
#' @return An updated [Alchemist] object with fitted learner state and learned
#'   distance-matrix outputs.
S7::method(forge_distances, Alchemist) <- function(object,
                                                   progress = NULL,
                                                   feature_type = NULL,
                                                   cache_path = NULL,
                                                   refresh = NULL,
                                                   ...) {
  config <- object@config
  progress <- progress %||% config$progress %||% FALSE
  feature_type <- feature_type %||% config$feature_type %||% "gower"
  refresh <- if (is.null(refresh)) {
    isTRUE(config$refresh %||% FALSE)
  } else {
    if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
      stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
    }
    isTRUE(refresh)
  }

  if (!refresh &&
    length(object@distance_matrix) > 0 &&
    length(object@learner) > 0 &&
    identical(object@distance_matrix$feature_type %||% NULL, feature_type)) {
    report_progress(
      progress,
      "[Alchemist] Object already has forged distances for feature_type = ",
      feature_type, "; skipping recompute."
    )
    return(object)
  }

  cache_path <- cache_path %||% alchemist_stage_cache_path(
    config,
    stage = "forge_distances",
    # Pair-training version: v2 includes distinct conspecific model pairs
    # while still excluding identity pairs through `diag(valid_mask) <- FALSE`.
    # v3 also removes generalized equations from ordinary learned-distance
    # training so constructed "NA NA" identities cannot define nearest donors.
    # v4 trains replacement-error targets on the same stored/reference length
    # PDFs used by policy evaluation, rather than a separate uniform interval.
    suffix = paste(feature_type, "nonself_pairs_v4_no_generalized_reference_pdf", sep = "_")
  )
  if (!is.null(cache_path) && tsb_cache_exists(cache_path) && !refresh) {
    report_progress(progress, "[Alchemist] Loading cached forged distances: ", cache_path)
    cached <- tsb_cache_read(cache_path)
    if (.is_alchemist(cached)) {
      return(cached)
    }
    report_progress(progress, "[Alchemist] Cached forged distances were not an Alchemist object; rebuilding.")
  }

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
    "(up to ~", n_models * (n_models - 1L), " non-self pairs",
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
        "glm_elastic", "rf", "xgboost"
      ),
      outcome_transform = learner_cfg$outcome_transform %||% "identity",
      lambda_rule = learner_cfg$lambda_rule %||% "lambda.1se",
      weight_rule = learner_cfg$weight_rule %||% "nnls",
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
  directed_mat <- dist_mat
  learned_bandwidth <- stats::median(
    oof_preds[is.finite(oof_preds) & oof_preds > 0],
    na.rm = TRUE
  )
  if (!is.finite(learned_bandwidth) || learned_bandwidth <= 0) {
    stop("The Alchemist learner produced no positive finite directed distances.", call. = FALSE)
  }

  taxonomic_mat <- pair_data$trait_mats[[".dist_tax"]] %||% NULL
  if (!is.null(taxonomic_mat)) {
    dimnames(taxonomic_mat) <- list(model_ids, model_ids)
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
    directed_dist_matrix = directed_mat,
    taxonomic_dist_matrix = taxonomic_mat,
    taxonomic_distance_method = attr(taxonomic_mat, "taxonomic_distance_method") %||% NA_character_,
    learned_kernel_bandwidth = learned_bandwidth,
    model_ids = model_ids,
    all_traits = pair_data$all_traits,
    species_trait_names = sp_names,
    study_trait_names = st_names,
    species_feature_cols = pair_data$species_feature_cols,
    trait_cols = pair_data$all_traits,
    oof_performance = sl_fit$oof_performance %||% tibble::tibble(),
    pair_data = pair_data$training_data,
    feature_cols = pair_data$feature_cols,
    trait_mats = pair_data$trait_mats,
    feature_type = feature_type,
    coherence_config = coherence_cfg,
    taxonomic_distance = taxonomic_distance,
    feature_normalization = pair_data$feature_normalization,
    donor_sigma_matrix = pair_data$donor_sigma_matrix,
    target_sigma = pair_data$target_sigma
  )

  # A fresh learned distance matrix invalidates every downstream artifact that
  # depends on that geometry. Re-running forge_distances() on an existing
  # Alchemist must therefore clear trait importance, ordination, and
  # admissibility rather than silently carrying incompatible results forward.
  object <- alchemist_rebuild(
    object,
    distance_matrix = distance_matrix,
    trait_importance = list(),
    ordination = list(),
    admissibility = list()
  )
  # Wrap once, here, at the fit point (see share_distance_learner()).
  out <- alchemist_rebuild(object, learner = share_distance_learner(sl_fit, fingerprint = cache_path))
  if (!is.null(cache_path)) {
    tsb_cache_write(out, cache_path)
    report_progress(progress, "[Alchemist] Saved forged distances cache: ", cache_path)
  }
  out
}

# - distill_traits helpers -

#' Evaluate kernel-weighted sigma RMSE from predicted pairwise distances
#'
#' Reconstructs an nxn distance matrix from (donor_idx, anchor_idx, dist_vec)
#' triplets, then computes the log sigma RMSE of the kernel-weighted ensemble
#' sigma prediction - the same objective used by `score_similarity_basis()` in
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
#' @noRd
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
#' component-dropout logic in `score_dropout_task()`, applied to the
#' Alchemist's learned distance function.
#'
#' @param fc Character. Feature column to zero out.
#' @param pair_data Tibble of training pairs.
#' @param anchor_idx Integer vector of anchor indices from `pair_data$.anchor_idx`.
#' @param donor_idx Integer vector of donor indices from `pair_data$.donor_idx`.
#' @param sl_fit A `"SuperLearner"` or `"Mahalanobis"` learner object.
#' @param donor_sigma_mat nxn donor sigma matrix from `build_pair_data()`.
#' @param target_sigma Length-n target sigma vector.
#' @param baseline_sigma_rmse Scalar baseline sigma RMSE with no feature zeroed.
#' @param scale Kernel bandwidth passed to `eval_sigma_rmse()`.
#'
#' @return A one-row [tibble::tibble()] with columns `trait`, `feature_col`,
#'   `delta_rmse`.
#'
#' @keywords internal
#' @noRd
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
#' @noRd
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

# Namespace-local payload used by Alchemist trait-dropout workers. Fork workers
# inherit this environment by copy-on-write, avoiding socket export of the large
# fitted learner payload on Unix.
.alchemist_dropout_payload <- new.env(parent = emptyenv())

#' Set the namespace-local Alchemist dropout payload
#'
#' Stores the large fitted objects required by trait-dropout workers in a
#' namespace-local environment. Fork clusters inherit this environment by
#' copy-on-write, while PSOCK clusters can export from it once during setup.
#'
#' @param payload Named list of objects required by
#'   [run_alchemist_dropout_task()].
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
set_alchemist_dropout_payload <- function(payload) {
  rm(
    list = ls(envir = .alchemist_dropout_payload, all.names = TRUE),
    envir = .alchemist_dropout_payload
  )
  list2env(payload, envir = .alchemist_dropout_payload)
  invisible(NULL)
}

#' Clear the namespace-local Alchemist dropout payload
#'
#' Removes all stored trait-dropout payload objects after dropout sensitivity
#' finishes so large fitted objects are not retained longer than needed.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
clear_alchemist_dropout_payload <- function() {
  rm(
    list = ls(envir = .alchemist_dropout_payload, all.names = TRUE),
    envir = .alchemist_dropout_payload
  )
  invisible(NULL)
}

#' Get one Alchemist dropout payload value
#'
#' Looks up a trait-dropout payload object from the namespace-local payload
#' environment, falling back to the worker global environment for PSOCK workers
#' that received explicit exports.
#'
#' @param name Single object name to retrieve.
#'
#' @return The requested payload object.
#' @keywords internal
#' @noRd
get_alchemist_dropout_payload_value <- function(name) {
  if (exists(name, envir = .alchemist_dropout_payload, inherits = FALSE)) {
    return(get(name, envir = .alchemist_dropout_payload, inherits = FALSE))
  }
  get(name, envir = .GlobalEnv, inherits = FALSE)
}

#' Run one Alchemist trait-dropout task on a cluster worker
#'
#' Fork workers inherit payloads from the namespace-local dropout environment.
#' PSOCK workers receive the same payload once in their global environment.
#' Keeping this function in the package namespace avoids serializing the full
#' fitted Alchemist environment with every task.
#'
#' @param trait_name Trait group name.
#'
#' @return One-row trait-dropout tibble.
#' @keywords internal
#' @noRd
run_alchemist_dropout_task <- function(trait_name) {
  trait_groups <- get_alchemist_dropout_payload_value("trait_groups")
  pair_data <- get_alchemist_dropout_payload_value("pair_data")
  anchor_idx <- get_alchemist_dropout_payload_value("anchor_idx")
  donor_idx <- get_alchemist_dropout_payload_value("donor_idx")
  sl_fit <- get_alchemist_dropout_payload_value("sl_fit")
  donor_sigma_mat <- get_alchemist_dropout_payload_value("donor_sigma_mat")
  target_sigma <- get_alchemist_dropout_payload_value("target_sigma")
  baseline_sigma_rmse <- get_alchemist_dropout_payload_value("baseline_sigma_rmse")
  scale_param <- get_alchemist_dropout_payload_value("scale_param")

  dropout_trait_group(
    fcs = trait_groups[[trait_name]],
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

# - distill_traits -

#' Distill trait importance for an `Alchemist`
#'
#' Computes dropout-style trait importance from the fitted Alchemist distance
#' learner. For each trait group, the method zeros that trait's pairwise feature
#' columns, re-predicts learned distances, and measures how much
#' kernel-weighted sigma RMSE changes relative to the full-feature baseline.
#'
#' The result identifies which traits materially support the learned transfer
#' geometry. It requires [forge_distances()] because it uses the fitted learner,
#' pairwise training data, donor sigma matrix, and target sigma vector stored in
#' the Alchemist distance bundle.
#'
#' @name distill_traits.Alchemist
#' @usage NULL
#'
#' @param object An [Alchemist] object after [forge_distances()].
#' @param kernel_scale Positive numeric bandwidth used by the kernel-weighted
#'   sigma RMSE objective.
#' @param workers Optional worker count for parallel dropout calculations.
#' @param progress Optional logical scalar controlling progress messages.
#' @param ... Reserved for additional trait-importance controls.
#'
#' @return An updated [Alchemist] object with trait-importance diagnostics.
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

  sl_fit <- resolve_distance_learner(object@learner)
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

  dropout_payload <- list(
    pair_data = pair_data,
    anchor_idx = anchor_idx,
    donor_idx = donor_idx,
    sl_fit = sl_fit,
    donor_sigma_mat = donor_sigma_mat,
    target_sigma = target_sigma,
    baseline_sigma_rmse = baseline_sigma_rmse,
    scale_param = scale_param,
    trait_groups = trait_groups
  )
  set_alchemist_dropout_payload(dropout_payload)
  on.exit(clear_alchemist_dropout_payload(), add = TRUE)

  use_parallel <- workers > 1L && n_groups > 1L
  cl <- NULL
  if (use_parallel) {
    cl <- initialize_parallel_cluster(workers = min(workers, n_groups))
    if (!is.null(cl)) {
      on.exit(if (!is.null(cl)) parallel::stopCluster(cl), add = TRUE)
      cluster_type <- attr(cl, "cluster_type", exact = TRUE) %||% "unknown"
      export_ok <- TRUE
      if (!identical(cluster_type, "fork")) {
        export_ok <- tryCatch(
          {
            parallel::clusterExport(
              cl,
              varlist = names(dropout_payload),
              envir = .alchemist_dropout_payload
            )
            TRUE
          },
          error = function(e) {
            report_progress(
              progress,
              "[Alchemist] distill_traits parallel setup failed; falling back to sequential dropout: ",
              conditionMessage(e)
            )
            FALSE
          }
        )
      }
      report_progress(
        progress,
        "[Alchemist] Cluster ready: ", parallel_cluster_description(cl), "."
      )
      if (!isTRUE(export_ok)) {
        parallel::stopCluster(cl)
        cl <- NULL
      }
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
    parallel_rows <- tryCatch(
      parallel::parLapplyLB(cl, trait_names_ordered, fun = run_alchemist_dropout_task),
      error = function(e) {
        report_progress(
          progress,
          "[Alchemist] distill_traits parallel dropout failed; falling back to sequential dropout: ",
          conditionMessage(e)
        )
        NULL
      }
    )
    if (is.null(parallel_rows)) {
      lapply(trait_names_ordered, run_dropout_group)
    } else {
      parallel_rows
    }
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
#' @noRd
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
                                      cluster_args = NULL,
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
  cluster_args <- cluster_args %||% ordination_cfg$cluster_args %||% list()

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
  if (!is.null(model_trait_table)) {
    coh_cols <- setdiff(coh_cols, names(model_trait_table))
  }
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
    reference_ids <- if ("model_id" %in% names(anch)) {
      as.character(anch$model_id)
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

  model_points_missing <- if (length(trait_cols) > 0) {
    add_ordination_missing(
      points_df = model_points,
      candidate_models = candidate_models,
      trait_cols = trait_cols,
      model_id_col = model_id_col
    )
  } else {
    tibble::as_tibble(model_points) |>
      dplyr::mutate(
        missing_trait_count = NA_integer_,
        missing_trait_fraction = NA_real_,
        missingness_group = NA_character_
      )
  }

  model_hulls <- tryCatch(
    build_ordination_hulls(model_points, cluster_col = model_cluster_col),
    error = function(e) tibble::tibble()
  )
  model_scale <- compute_ordination_scale(model_points)

  ordination <- list(
    model = list(
      ordination = model_ord$ordination,
      points = tibble::as_tibble(model_points),
      loadings = model_ord$loadings %||% tibble::tibble(),
      centroids = model_ord$centroids %||% tibble::tibble(),
      model_scores = tibble::as_tibble(model_scores),
      scores = tibble::as_tibble(model_scores),
      points_missing = tibble::as_tibble(model_points_missing),
      hulls = tibble::as_tibble(model_hulls),
      scale = model_scale
    ),
    species = list(
      ordination = NULL,
      points = tibble::tibble(),
      loadings = tibble::tibble(),
      centroids = tibble::tibble(),
      pairwise_tests = tibble::tibble()
    ),
    species_lookup = list()
  )

  alchemist_rebuild(alchemist, ordination = ordination)
}

# - screen_admissibility dispatch -

#' @keywords internal
#' @noRd
.screen_admissibility_alchemist <- function(alchemist,
                                            config = NULL,
                                            cache_path = NULL,
                                            refresh = NULL,
                                            progress = NULL,
                                            registry_path = NULL,
                                            keep_training_data = FALSE) {
  if (length(alchemist@distance_matrix) == 0L) {
    stop("Run `forge_distances()` before `screen_admissibility()`.", call. = FALSE)
  }

  dm <- alchemist@distance_matrix

  gower_bundle <- list(
    combined_dist = dm$combined_dist,
    species_dist = dm$combined_dist,
    study_dist = as.matrix(dm$dist_matrix),
    species_dist_model = as.matrix(dm$dist_matrix),
    learned_directed_dist = dm$directed_dist_matrix %||% NULL,
    taxonomic_dist_model = dm$taxonomic_dist_matrix %||% NULL,
    learned_kernel_bandwidth = dm$learned_kernel_bandwidth %||% NULL,
    distance_mode = "alchemist_super_learner",
    trait_cols = dm$trait_cols %||% dm$all_traits %||% character(0),
    distance_learner = canonicalize_distance_learner(alchemist@learner),
    feature_cols = dm$feature_cols %||% resolve_distance_learner(alchemist@learner)$feature_cols %||% character(0),
    species_trait_names = dm$species_trait_names %||% character(0),
    study_trait_names = dm$study_trait_names %||% character(0),
    feature_type = dm$feature_type %||% NULL,
    coherence_config = dm$coherence_config %||% NULL,
    taxonomic_distance = dm$taxonomic_distance %||% NULL,
    feature_normalization = dm$feature_normalization %||% NULL
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

  # pair_data/trait_mats are training-only inputs to forge_distances(); once
  # admissibility screening has run, only distill_traits() still needs them.
  distance_matrix_out <- dm
  if (!isTRUE(keep_training_data)) {
    distance_matrix_out$pair_data <- NULL
    distance_matrix_out$trait_mats <- NULL
  }

  alchemist_rebuild(alchemist, admissibility = admissibility_result, distance_matrix = distance_matrix_out)
}

# - print / show -

#' Print an `Alchemist`
#'
#' @name print.Alchemist
#' @usage NULL
#'
#' @keywords internal
#' @noRd
S7::method(print_generic, Alchemist) <- function(x, ...) {
  cat("Alchemist\n")
  cat("  candidates:       ", nrow(x@candidates), " models\n", sep = "")
  cat("  species_traits:   ", length(alchemist_trait_names(x@config$species_traits %||% list(), x@candidates)), "\n", sep = "")
  cat("  study_traits:     ", length(alchemist_trait_names(x@config$study_traits %||% list(), x@candidates)), "\n", sep = "")
  cat("  learner_ready:    ", if (length(x@learner) > 0L) "yes" else "no", "\n", sep = "")
  cat("  distances_ready:  ", if (length(x@distance_matrix) > 0L) "yes" else "no", "\n", sep = "")
  cat("  importance_ready: ", if (length(x@trait_importance) > 0L) "yes" else "no", "\n", sep = "")
  cat("  ordination_ready: ", if (length(x@ordination) > 0L) "yes" else "no", "\n", sep = "")
  cat("  admiss_ready:     ", if (length(x@admissibility) > 0L) "yes" else "no", "\n", sep = "")
  invisible(x)
}

#' Show an `Alchemist`
#'
#' @name show.Alchemist
#' @usage NULL
#'
#' @keywords internal
#' @noRd
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
#' @param dissimilarity Ordination dissimilarity view. `"combined"` plots the
#'   model-level ordination; `"species"` plots the species-level ordination.
#' @param view Secondary plot selector for `type = "ordination"` or
#'   `type = "admissibility"`. One of `"clusters"`, `"cluster_hulls"`,
#'   `"vectors"`, or `"centers"` for combined ordination; species ordination
#'   also accepts `"overview"`. For admissibility, one of
#'   `"gate_composition"` or `"overlap_profile"`.
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
#' @usage
#' \method{plot}{Alchemist}(
#'   x,
#'   y = NULL,
#'   type = c("ordination", "trait_importance", "admissibility"),
#'   dissimilarity = c("combined", "species"),
#'   view = NULL,
#'   include_hulls = TRUE,
#'   ...
#' )
NULL

#' Current Alchemist plot type names
#'
#' @return Character vector of supported plot type names.
#' @keywords internal
#' @noRd
alchemist_plot_types <- function() {
  c("ordination", "trait_importance", "admissibility")
}

.plot_alchemist <- function(x,
                            y = NULL,
                            type = c("ordination", "trait_importance", "admissibility"),
                            dissimilarity = c("combined", "species"),
                            view = NULL,
                            include_hulls = TRUE,
                            ...) {
  valid_types <- alchemist_plot_types()
  type <- as.character(type %||% valid_types[[1]])[[1]]
  if (!type %in% valid_types) {
    return(plot_report_placeholder(
      title = "Alchemist Plot Unavailable",
      subtitle = sprintf(
        "Plot type '%s' is not a current Alchemist plot type.",
        type
      )
    ))
  }
  dissimilarity <- match.arg(dissimilarity)

  if (identical(type, "ordination")) {
    if (length(x@ordination) == 0L) {
      return(plot_report_placeholder(
        title = "Alchemist Ordination",
        subtitle = "No ordination results are stored on this Alchemist object."
      ))
    }
    reference_species <- ordination_reference_species(x@candidates@reference_anchors)

    if (identical(dissimilarity, "species")) {
      species_obj <- x@ordination$species %||% list()
      species_points <- mark_ordination_reference_species(
        species_obj$points %||% tibble::tibble(),
        reference_species = reference_species,
        preserve_existing = FALSE
      )
      ord_view <- match.arg(
        view %||% "overview",
        c("overview", "clusters", "cluster_hulls", "vectors", "centers")
      )
      if (identical(ord_view, "overview")) {
        return(plot_species_ordination(points_tbl = species_points))
      }
      if (identical(ord_view, "vectors")) {
        return(plot_ordination_vectors(
          vec_tbl = species_obj$loadings %||% tibble::tibble(),
          points_tbl = species_points
        ))
      }
      if (identical(ord_view, "centers")) {
        return(plot_ordination_centers(
          fac_tbl = species_obj$centroids %||% tibble::tibble(),
          points_tbl = species_points
        ))
      }
      if (identical(ord_view, "cluster_hulls")) {
        hull_tbl <- tryCatch(
          build_ordination_hulls(species_points, cluster_col = "species_cluster_id"),
          error = function(e) tibble::tibble()
        )
        return(plot_ordination_cluster_hulls(
          points_tbl = species_points,
          hull_tbl = hull_tbl,
          cluster_col = "species_cluster_id"
        ))
      }
      return(plot_ordination_clusters(
        points_tbl = species_points,
        cluster_col = "species_cluster_id",
        colorbar_name = "Species cluster",
        ...
      ))
    }

    model_obj <- x@ordination$model %||% list()
    point_tbl <- mark_ordination_reference_species(
      model_obj$points %||% tibble::tibble(),
      reference_species = reference_species,
      preserve_existing = FALSE
    )
    hull_tbl <- tibble::as_tibble(model_obj$hulls %||% tibble::tibble())
    loading_tbl <- tibble::as_tibble(model_obj$loadings %||% tibble::tibble())
    centroid_tbl <- tibble::as_tibble(model_obj$centroids %||% tibble::tibble())

    # Convex hulls may overlap for valid PAM partitions, which incorrectly
    # implies overlapping clusters. Use the point partition by default; hulls
    # remain available as an explicit diagnostic view.
    ord_view <- match.arg(
      view %||% "clusters",
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
      cluster_col = "nmds_cluster_id",
      ...
    ))
  }

  if (identical(type, "admissibility")) {
    if (length(x@admissibility) == 0L) {
      return(plot_report_placeholder(
        title = "Admissibility",
        subtitle = "No admissibility results are stored on this Alchemist object."
      ))
    }
    if (!admissibility_bundle_is_current(x@admissibility, x@config$config_data %||% x@candidates)) {
      return(plot_report_placeholder(
        title = "Current Admissibility Plot Unavailable",
        subtitle = paste(
          "Current-format admissibility diagnostics are not available on this object.",
          "Run screen_admissibility() before generating admissibility plots."
        )
      ))
    }
    adm_view <- match.arg(view %||% "gate_composition", c("gate_composition", "overlap_profile"))
    if (identical(adm_view, "gate_composition")) {
      return(plot_gate_composition(
        x@admissibility$all_gates %||% tibble::tibble(),
        config = x@config$config_data %||% x@candidates
      ))
    }
    return(plot_overlap_heatmap(
      x@admissibility$all_overlap %||% tibble::tibble(),
      config = x@config$config_data %||% x@config %||% NULL
    ))
  }

  if (identical(type, "trait_importance")) {
    if (length(x@trait_importance) == 0L) {
      return(plot_report_placeholder(
        title = "Trait Importance",
        subtitle = "No trait-importance data are stored on this Alchemist object."
      ))
    }
    imp_tbl <- tibble::as_tibble(x@trait_importance$importance_tbl %||% tibble::tibble())
    if (nrow(imp_tbl) == 0L) {
      return(plot_report_placeholder(
        title = "Trait Importance",
        subtitle = "The stored trait-importance table is empty."
      ))
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

#' Register the `Alchemist` plot method
#'
#' @name plot.Alchemist
#' @usage NULL
#'
#' @keywords internal
#' @noRd
plot.Alchemist <- .plot_alchemist
S7::method(plot_generic, Alchemist) <- .plot_alchemist


#' Resolve one configured field name
#'
#' @param config Anchor config list.
#' @param key Field-map key to resolve.
#'
#' @return Character scalar.
#'
#' @keywords internal
#' @noRd
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
#' @noRd
default_anchor_config <- function(config = NULL) {
  config_ <- config
  if (is_s7_instance(config_, "Candidates")) {
    config_ <- candidates_config_data(config_)
  }
  if (is.list(config_) &&
    is.list(config_$fields %||% NULL) &&
    all(c("model_id", "species_name", "frequency") %in% names(config_$fields)) &&
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
  cfg_data <- if (is_s7_instance(config_, "Configurer")) {
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

  merge_config_sections(
    list(
      fields = list(
        model_id = "model_id",
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
        slope = "slope_standard",
        intercept = "intercept_standard",
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
#' @param config Anchor config list or package object carrying configuration.
#'
#' @return Character vector of column names.
#' @keywords internal
#' @noRd
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
#' @param config Anchor config list or package object carrying configuration.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
admissibility_bundle_is_current <- function(admissibility_bundle,
                                            config = NULL) {
  if (!is.list(admissibility_bundle)) {
    return(FALSE)
  }
  logic_version <- admissibility_bundle$logic_version %||% NULL
  if (!is.null(logic_version) && !identical(logic_version, anchor_admissibility_logic_version())) {
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

  if (!is.null(admissibility_bundle$all_overlap)) {
    overlap_tbl <- tibble::as_tibble(admissibility_bundle$all_overlap)
    if (nrow(overlap_tbl) == 0 || !"anchor_species" %in% names(overlap_tbl)) {
      return(FALSE)
    }
    score_metric_cols <- names(scores_tbl)[
      startsWith(names(scores_tbl), "overlap_same_") |
        endsWith(names(scores_tbl), "_coherence_distance") |
        endsWith(names(scores_tbl), "_overlap_fraction")
    ]
    if (length(score_metric_cols) > 0L) {
      expected_overlap_metrics <- unique(c(
        paste0(
          "w_same_",
          sub("^overlap_same_", "", score_metric_cols[startsWith(score_metric_cols, "overlap_same_")])
        ),
        paste0(
          "mean_",
          sub(
            "_coherence_distance$",
            "_coherence",
            score_metric_cols[endsWith(score_metric_cols, "_coherence_distance")]
          )
        ),
        paste0("mean_", score_metric_cols[endsWith(score_metric_cols, "_overlap_fraction")])
      ))
      expected_overlap_metrics <- expected_overlap_metrics[nzchar(expected_overlap_metrics)]
      if (length(expected_overlap_metrics) > 0L &&
        !any(expected_overlap_metrics %in% names(overlap_tbl))) {
        return(FALSE)
      }
    }
  }

  TRUE
}

anchor_admissibility_logic_version <- function() {
  "reference_anchor_donor_exclusion_v1"
}

#' Signal that one anchor cannot be scored
#'
#' @param message Human-readable failure reason.
#' @param reason_code Stable machine-readable failure code.
#' @param stage Workflow stage where the anchor became unscorable.
#'
#' @keywords internal
#' @noRd
abort_unscorable_anchor <- function(message,
                                    reason_code,
                                    stage = "anchor_density") {
  condition <- structure(
    list(
      message = as.character(message)[[1]],
      call = NULL,
      reason_code = as.character(reason_code)[[1]],
      stage = as.character(stage)[[1]]
    ),
    class = c("tsbiomass_unscorable_anchor", "error", "condition")
  )
  stop(condition)
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
#' @noRd

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
#' @noRd
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

  a_len <- a_max_ - a_min_
  if (a_len == 0) {
    # A point-valued study range overlaps a donor interval when the observed
    # point is contained in that interval. It is not a zero-measure failure.
    out[keep] <- as.numeric(b_min_[keep] <= a_min_ & b_max_[keep] >= a_max_)
    return(out)
  }

  inter <- pmax(0, pmin(a_max_, b_max_[keep]) - pmax(a_min_, b_min_[keep]))
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
#' @noRd
compute_frequency_gap <- function(candidate_freq,
                                  anchor_freq,
                                  freq_span,
                                  mode = "overlap") {
  candidate_freq_ <- suppressWarnings(as.numeric(candidate_freq))
  n_out <- length(candidate_freq_)
  if (n_out == 0L) {
    return(numeric(0))
  }

  anchor_freq_ <- suppressWarnings(as.numeric(anchor_freq[[1]] %||% NA_real_))
  span_ <- suppressWarnings(as.numeric(freq_span[[1]] %||% NA_real_))
  mode_ <- as.character(mode[[1]] %||% "overlap")
  mode_ <- stringr::str_to_lower(stringr::str_squish(mode_))

  valid_freqs <- is.finite(candidate_freq_) &
    candidate_freq_ > 0 &
    is.finite(anchor_freq_) &
    anchor_freq_ > 0

  if (identical(mode_, "literal")) {
    out <- rep(NA_real_, n_out)
    out[valid_freqs] <- as.numeric(
      as.integer(round(candidate_freq_[valid_freqs])) !=
        as.integer(round(anchor_freq_))
    )
    return(out)
  }

  if (identical(mode_, "none")) {
    return(rep(NA_real_, n_out))
  }

  if (!is.finite(span_) || span_ <= 0) {
    return(rep(NA_real_, n_out))
  }

  out <- rep(NA_real_, n_out)
  out[valid_freqs] <- pmin(
    abs(log(candidate_freq_[valid_freqs] / anchor_freq_)) / span_,
    1
  )
  out
}

#' Add key-metadata missingness
#'
#' Appends the fraction of missing selected species/study traits for each model.
#'
#' @param candidate_models Candidate-model table.
#' @param ... Inputs forwarded to the table, [Candidates], or [PolicySelector]
#'   missingness-screen method.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
screen_missing_metadata <- S7::new_generic("screen_missing_metadata", "candidate_models")

#' Add key-metadata missingness for tabular candidate-model inputs
#'
#' @name screen_missing_metadata.default
#' @usage NULL
#' @param candidate_models Candidate-model table.
#' @param key_cols Character vector of key trait columns.
#'
#' @keywords internal
#' @noRd
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

  missing_mat <- key_metadata_missing_matrix(out, key_cols)
  out |>
    dplyr::mutate(
      key_metadata_missing_fraction = rowMeans(missing_mat)
    )
}

#' Build a key-metadata missingness matrix
#'
#' @param models_tbl Candidate-model table.
#' @param key_cols Key metadata columns to score.
#'
#' @return Logical matrix with rows matching `models_tbl` and columns matching
#'   retained `key_cols`.
#' @keywords internal
#' @noRd
key_metadata_missing_matrix <- function(models_tbl,
                                        key_cols) {
  models_tbl <- tibble::as_tibble(models_tbl)
  key_cols <- intersect(as.character(key_cols), names(models_tbl))
  if (length(key_cols) == 0L) {
    return(matrix(FALSE, nrow = nrow(models_tbl), ncol = 0L))
  }

  missing_mat <- as.matrix(is.na(models_tbl[, key_cols, drop = FALSE]))
  group_rows <- generalized_model_indicator(models_tbl)
  not_applicable_group_cols <- intersect(
    c("species_name", "species", "genus", "family", "body_shape"),
    key_cols
  )
  if (any(group_rows) && length(not_applicable_group_cols) > 0L) {
    missing_mat[group_rows, not_applicable_group_cols] <- FALSE
  }
  missing_mat
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
#' @noRd
prepare_admissibility_overlap_columns <- function(candidate_models,
                                                  config,
                                                  registry_path = NULL) {
  # Normalize the candidate-side trait values once per benchmark/admissibility
  # scenario so anchor-level overlap checks do not keep re-parsing the same
  # strings inside every anchor worker.
  out <- tibble::as_tibble(candidate_models)
  trait_defs <- admissibility_overlap_trait_defs(registry_path = registry_path)
  trait_defs <- trait_defs[vapply(
    trait_defs,
    function(x) {
      trait_name <- as.character(x$coded_name %||% NA_character_)[[1]]
      !is.na(trait_name) && trait_name %in% names(out)
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
#' @noRd
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
    !is_missing_species_identity(out[[species_col]]) &
      !is_missing_species_identity(anchor_species) &
      out[[species_col]] == anchor_species
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
  trait_defs <- trait_defs[vapply(
    trait_defs,
    function(x) {
      trait_name <- as.character(x$coded_name %||% NA_character_)[[1]]
      !is.na(trait_name) &&
        nzchar(trait_name) &&
        trait_name %in% names(out) &&
        trait_name %in% names(anchor_row)
    },
    logical(1)
  )]
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
#' @noRd
apply_anchor_gates <- function(candidate_models,
                               anchor_row,
                               config,
                               registry_path = NULL) {
  # Apply the gate rules after overlap/coherence fields exist so the final
  # admissibility label is determined from one fully scored table. The active
  # configured trait gates are resolved from the registry at runtime so
  # categorical/binary traits use exact matching and set-valued traits use
  # overlap without re-hard-coding each allowable field here.
  id_col <- build_anchor_field(config, "model_id")
  anchor_id <- build_anchor_model_id(anchor_row, config)

  out <- tibble::as_tibble(candidate_models)
  out$gate_not_self <- out[[id_col]] != anchor_id
  gate_species_traits <- config$admissibility_species_traits %||% config$species_traits %||% character(0)
  gate_study_traits <- config$admissibility_study_traits %||% config$study_traits %||% character(0)
  gate_specs <- data.frame(
    trait_name = c(
      as.character(gate_species_traits),
      as.character(gate_study_traits)
    ),
    trait_scope = c(
      rep("species", length(gate_species_traits)),
      rep("study", length(gate_study_traits))
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
  group_model <- generalized_model_indicator(out)
  trait_value_missing <- function(x) {
    x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
    is.na(x_chr) | !nzchar(x_chr) | x_chr %in% c(
      "na", "n/a", "unknown", "unknown unknown",
      "general", "nonspecific", "general/nonspecific"
    )
  }
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
      if (identical(trait_scope, "species")) {
        missing_group_trait <- group_model %in% TRUE & trait_value_missing(out[[trait_name]])
        gate_pass[missing_group_trait] <- TRUE
      }
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
    if (identical(trait_scope, "species")) {
      missing_group_trait <- group_model %in% TRUE & trait_value_missing(out[[trait_name]])
      gate_pass[missing_group_trait] <- TRUE
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

#' Extract configured trait names from an anchor trait specification
#'
#' @param x Trait specification from an anchor, admissibility, or similarity
#'   config section.
#'
#' @return Character vector of trait names.
#'
#' @keywords internal
#' @noRd
anchor_overlap_trait_names <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  x_names <- names(x)
  if (length(x_names) > 0L && any(nzchar(x_names))) {
    out <- as.character(x_names)
  } else {
    out <- as.character(unlist(x, recursive = FALSE, use.names = FALSE))
  }
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

#' Resolve overlap-column suffix candidates for one trait
#'
#' @param trait Configured trait name or public alias.
#' @param config Anchor config list.
#'
#' @return Character vector of candidate overlap suffixes in preference order.
#'
#' @keywords internal
#' @noRd
anchor_overlap_suffix_candidates <- function(trait,
                                             config) {
  trait <- as.character(trait %||% "")[[1]]
  trait <- stringr::str_squish(trait)
  if (!nzchar(trait)) {
    return(character(0))
  }
  alias_map <- c(
    fao = "fao_area",
    swimbladder = "swimbladder_type",
    derivation = "derivation_type",
    species_name = "species"
  )
  trait_key <- if (trait %in% names(alias_map)) unname(alias_map[[trait]]) else trait
  field_key <- switch(trait_key,
    species = "species_name",
    genus = "genus",
    family = "family",
    order = "order",
    swimbladder_type = "swimbladder",
    fao_area = "fao_area",
    ocean_basin = "ocean_basin",
    equation_form = "equation_form",
    derivation_type = "derivation_type",
    study_cell = "study_cell",
    trait_key
  )
  field_name <- tryCatch(
    if (field_key %in% names(config$fields %||% list())) build_anchor_field(config, field_key) else trait_key,
    error = function(e) trait_key
  )
  candidates <- unique(c(
    switch(trait_key,
      species = "species",
      swimbladder_type = c("swimbladder", "swimbladder_type"),
      derivation_type = c("derivation", "derivation_type"),
      fao_area = c("fao_area", "fao"),
      trait_key
    ),
    trait_key,
    field_name
  ))
  candidates[!is.na(candidates) & nzchar(candidates)]
}

#' Select configured overlap columns from an anchor table
#'
#' @param tbl Anchor-scored candidate table.
#' @param config Raw or defaulted anchor config. When `NULL`, all available
#'   overlap columns are retained for backwards-compatible direct use.
#'
#' @return Character vector of overlap columns to summarize.
#'
#' @keywords internal
#' @noRd
configured_anchor_overlap_columns <- function(tbl,
                                              config = NULL) {
  out <- tibble::as_tibble(tbl)
  available <- names(out)[startsWith(names(out), "overlap_same_")]
  available <- available[vapply(out[available], function(x) {
    is.logical(x) || is.numeric(x) || is.integer(x)
  }, logical(1))]
  if (length(available) == 0L || is.null(config)) {
    return(available)
  }
  cfg <- tryCatch(default_anchor_config(config), error = function(e) list())
  traits <- unique(c(
    anchor_overlap_trait_names(cfg$species_traits),
    anchor_overlap_trait_names(cfg$study_traits),
    anchor_overlap_trait_names(cfg$similarity_species_traits),
    anchor_overlap_trait_names(cfg$similarity_study_traits)
  ))
  if (length(traits) == 0L) {
    return(character(0))
  }
  selected <- character(0)
  for (trait in traits) {
    suffix_candidates <- anchor_overlap_suffix_candidates(trait, cfg)
    candidates <- paste0("overlap_same_", suffix_candidates)
    hit <- candidates[candidates %in% available]
    if (length(hit) > 0L) {
      canonical_suffix <- suffix_candidates[[1L]]
      selected <- c(selected, stats::setNames(hit[[1L]], canonical_suffix))
    }
  }
  selected[!duplicated(unname(selected))]
}

#' Select configured coherence columns from an anchor table
#'
#' @param tbl Anchor-scored candidate table.
#' @param config Raw or defaulted anchor config. When `NULL`, all available
#'   coherence columns are retained for backwards-compatible direct use.
#'
#' @return A named list with coherence-distance and overlap-fraction columns.
#'
#' @keywords internal
#' @noRd
configured_anchor_coherence_columns <- function(tbl,
                                                config = NULL) {
  out <- tibble::as_tibble(tbl)
  all_distance <- names(out)[endsWith(names(out), "_coherence_distance")]
  all_fraction <- names(out)[endsWith(names(out), "_overlap_fraction")]
  if (is.null(config)) {
    return(list(distance = all_distance, fraction = all_fraction))
  }
  raw_cfg <- admissibility_plot_config_data(config)
  adm <- raw_cfg$admissibility %||% raw_cfg
  sim <- raw_cfg$similarity %||% list()
  active <- character(0)
  for (dimension in c("length", "depth", "frequency")) {
    adm_block <- (adm$coherence %||% list())[[dimension]] %||% list()
    sim_block <- (sim$coherence %||% list())[[dimension]] %||% list()
    mode <- adm_block$mode %||% sim_block$mode %||% NULL
    min_value <- adm_block$min %||% NULL
    if (identical(dimension, "length")) {
      min_value <- min_value %||% adm$length_overlap_min %||% NULL
    }
    if (identical(dimension, "depth")) {
      min_value <- min_value %||% adm$depth_overlap_min %||% NULL
    }
    mode <- stringr::str_to_lower(stringr::str_squish(as.character(mode %||% "")))[[1]]
    if ((nzchar(mode) && !identical(mode, "none")) ||
      is.finite(suppressWarnings(as.numeric(min_value %||% NA_real_)))) {
      active <- c(active, dimension)
    }
  }
  list(
    distance = intersect(paste0(active, "_coherence_distance"), all_distance),
    fraction = intersect(paste0(active, "_overlap_fraction"), all_fraction)
  )
}

#' Summarize anchor overlap structure
#'
#' Collapses one anchor's admissible weighted set to a compact overlap summary.
#'
#' @param admissible_df Admissible weighted candidate table.
#' @param config Optional raw or defaulted anchor config used to restrict the
#'   summary to configured admissibility and similarity traits.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
summarize_anchor_overlap <- function(admissible_df,
                                     config = NULL) {
  out <- tibble::as_tibble(admissible_df)
  overlap_cols <- configured_anchor_overlap_columns(out, config = config)
  overlap_suffixes <- names(overlap_cols)
  if (length(overlap_suffixes) != length(overlap_cols)) {
    overlap_suffixes <- rep("", length(overlap_cols))
  }
  missing_names <- is.na(overlap_suffixes) | !nzchar(overlap_suffixes)
  overlap_suffixes[missing_names] <- sub("^overlap_same_", "", unname(overlap_cols[missing_names]))
  overlap_names <- paste0("w_same_", overlap_suffixes)
  overlap_names <- make.unique(overlap_names, sep = "_")
  coherence_config <- configured_anchor_coherence_columns(out, config = config)
  coherence_cols <- coherence_config$distance
  coherence_names <- paste0("mean_", sub("_coherence_distance$", "_coherence", coherence_cols))
  overlap_fraction_cols <- coherence_config$fraction
  overlap_fraction_names <- paste0("mean_", overlap_fraction_cols)

  empty_overlap_summary <- function() {
    summary_values <- c(
      stats::setNames(rep(NA_real_, length(overlap_names)), overlap_names),
      stats::setNames(rep(NA_real_, length(coherence_names)), coherence_names),
      stats::setNames(rep(NA_real_, length(overlap_fraction_names)), overlap_fraction_names)
    )
    tibble::as_tibble(c(list(n_admissible = 0L), as.list(summary_values)))
  }

  if (nrow(out) == 0) {
    return(empty_overlap_summary())
  }

  weights <- if ("w_adm" %in% names(out)) {
    suppressWarnings(as.numeric(out$w_adm))
  } else {
    rep(1, nrow(out))
  }
  weights[!is.finite(weights) | weights < 0] <- 0
  if (!any(weights > 0)) {
    return(empty_overlap_summary())
  }
  weights <- weights / sum(weights)
  weighted_mean_or_na <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    keep <- is.finite(x) & is.finite(weights) & weights > 0
    if (!any(keep)) {
      return(NA_real_)
    }
    stats::weighted.mean(x[keep], weights[keep], na.rm = TRUE)
  }
  overlap_values <- if (length(overlap_cols) == 0L) {
    stats::setNames(numeric(0), character(0))
  } else {
    values <- vapply(overlap_cols, function(nm) {
      flag <- dplyr::coalesce(as.logical(out[[nm]]), FALSE)
      sum(weights[flag], na.rm = TRUE)
    }, numeric(1))
    names(values) <- overlap_names[seq_along(values)]
    values
  }
  coherence_values <- if (length(coherence_cols) == 0L) {
    stats::setNames(numeric(0), character(0))
  } else {
    values <- vapply(coherence_cols, function(nm) {
      weighted_mean_or_na(pmax(0, 1 - suppressWarnings(as.numeric(out[[nm]]))))
    }, numeric(1))
    names(values) <- coherence_names[seq_along(values)]
    values
  }
  overlap_fraction_values <- if (length(overlap_fraction_cols) == 0L) {
    stats::setNames(numeric(0), character(0))
  } else {
    values <- vapply(overlap_fraction_cols, function(nm) {
      weighted_mean_or_na(out[[nm]])
    }, numeric(1))
    names(values) <- overlap_fraction_names[seq_along(values)]
    values
  }

  tibble::as_tibble(c(
    list(n_admissible = nrow(out)),
    as.list(overlap_values),
    as.list(coherence_values),
    as.list(overlap_fraction_values)
  ))
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
collect_anchor_scores <- function(eval_obj,
                                  anchor_row,
                                  config = NULL) {
  cfg <- default_anchor_config(config)
  id_col <- build_anchor_field(cfg, "model_id")
  anchor_id <- build_anchor_model_id(anchor_row, cfg)
  anchor_species <- as.character(anchor_row[[build_anchor_field(cfg, "species_name")]][[1]])

  scored <- tibble::as_tibble(eval_obj$model_eval) |>
    dplyr::mutate(anchor_model_id = anchor_id, anchor_species = anchor_species)

  # Reattach the admissible-support columns only for rows that survived the
  # admissibility screen.
  support_cols <- intersect(
    c(
      id_col, "study_cell_id", "study_cell_n_models",
      "w_study_adj_raw", "w_adm",
      "cumulative_w_adm"
    ),
    names(eval_obj$admissible_df)
  )

  if (id_col %in% names(scored) && id_col %in% support_cols) {
    scored <- scored |>
      dplyr::left_join(
        tibble::as_tibble(eval_obj$admissible_df) |>
          dplyr::select(dplyr::all_of(support_cols)),
        by = id_col
      )
  }

  scored
}

#' Count gate reasons
#'
#' Counts admissible and inadmissible rows by gate outcome for one anchor.
#'
#' @param scored_df Scored candidate table from `collect_anchor_scores()`.
#' @param anchor_row One-row anchor table.
#' @param config Optional anchor config list. When `NULL`, the defaults are
#'   used.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
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
#' @param scored_df Scored candidate table from `collect_anchor_scores()`.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
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
#' @noRd
build_anchor_model_id <- function(anchor_row,
                                  config) {
  # Prefer an existing character model identifier when present and otherwise
  # fall back to the primary model ID column.
  id_chr_col <- build_anchor_field(config, "model_id")
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
#' @return A tibble with `model_id` present.
#'
#' @keywords internal
#' @noRd
normalize_anchor_ids <- function(candidate_models,
                                 config) {
  # Standardize the model identifier column once before any joins or anchor
  # distance lookups depend on it.
  out <- tibble::as_tibble(candidate_models)

  id_col <- build_anchor_field(config, "model_id")
  id_chr_col <- build_anchor_field(config, "model_id")

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
#' @noRd
build_anchor_table <- function(candidate_models,
                               anchor_pdf,
                               config) {
  # Apply each standardized length-form model to the anchor length PDF so the
  # screening pipeline works from one comparable sigma_bs quantity.
  out <- normalize_anchor_ids(candidate_models, config)
  slope_col <- build_anchor_field(config, "slope")
  intercept_col <- build_anchor_field(config, "intercept")
  if (!slope_col %in% names(out)) {
    slope_col <- intersect(c("slope_standard", "slope_len"), names(out))[[1]] %||% NULL
  }
  if (!intercept_col %in% names(out)) {
    intercept_col <- intersect(c("intercept_standard", "intercept_len"), names(out))[[1]] %||% NULL
  }
  if (is.null(slope_col) || !slope_col %in% names(out)) {
    stop("Required slope column was not found in candidate models.", call. = FALSE)
  }
  if (is.null(intercept_col) || !intercept_col %in% names(out)) {
    stop("Required intercept column was not found in candidate models.", call. = FALSE)
  }
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
#' @param model_eval Candidate scoring table from `build_anchor_table()`.
#' @param anchor_id Anchor model identifier.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
anchor_backscatter <- function(model_eval,
                               anchor_id,
                               config) {
  # Pull the anchor's own sigma_bs value from the scored table so all
  # replacement multipliers are expressed relative to the same reference model.
  anchor_sigma <- model_eval |>
    dplyr::filter(.data[[build_anchor_field(config, "model_id")]] == anchor_id) |>
    dplyr::pull(.data$sigma_bs_model_mean)

  if (length(anchor_sigma) == 0) {
    return(NA_real_)
  }

  as.numeric(anchor_sigma[[1]])
}

#' Add anchor distance columns
#'
#' @param model_eval Anchor scoring table.
#' @param dist_obj Distance object from [construct_gower_distances()].
#' @param anchor_id Anchor model identifier.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
add_anchor_distances <- function(model_eval,
                                 dist_obj,
                                 anchor_id,
                                 config) {
  # Reindex the precomputed species and study distances onto the candidate
  # rows so every later scoring step can work off the row-wise table alone.
  model_eval_ <- model_eval
  model_ids <- model_eval_[[build_anchor_field(config, "model_id")]]
  if (identical(dist_obj$distance_mode %||% "", "alchemist_super_learner")) {
    learned_mat <- dist_obj$learned_directed_dist
    bandwidth <- suppressWarnings(as.numeric(dist_obj$learned_kernel_bandwidth)[[1]])
    if (is.null(learned_mat) || !is.matrix(learned_mat) ||
      !all(c(as.character(model_ids), as.character(anchor_id)) %in% rownames(learned_mat)) ||
      !all(c(as.character(model_ids), as.character(anchor_id)) %in% colnames(learned_mat)) ||
      !is.finite(bandwidth) || bandwidth <= 0) {
      stop("Alchemist policy scoring requires a directed learned-distance matrix and positive bandwidth.", call. = FALSE)
    }
    taxonomic_mat <- dist_obj$taxonomic_dist_model %||% NULL
    model_eval_$learned_distance <- as.numeric(learned_mat[model_ids, anchor_id])
    disagreement_mat <- dist_obj$learned_distance_disagreement %||% NULL
    model_eval_$learned_distance_disagreement <- if (!is.null(disagreement_mat) &&
      is.matrix(disagreement_mat) &&
      all(c(as.character(model_ids), as.character(anchor_id)) %in% rownames(disagreement_mat)) &&
      all(c(as.character(model_ids), as.character(anchor_id)) %in% colnames(disagreement_mat))) {
      as.numeric(disagreement_mat[model_ids, anchor_id])
    } else {
      NA_real_
    }
    model_eval_$learned_distance_diagnostic_available <- isTRUE(
      dist_obj$learned_distance_diagnostic_available
    )
    model_eval_$learned_kernel_bandwidth <- bandwidth
    model_eval_$d_species <- if (!is.null(taxonomic_mat) && is.matrix(taxonomic_mat) &&
      all(c(as.character(model_ids), as.character(anchor_id)) %in% rownames(taxonomic_mat)) &&
      all(c(as.character(model_ids), as.character(anchor_id)) %in% colnames(taxonomic_mat))) {
      as.numeric(taxonomic_mat[model_ids, anchor_id])
    } else {
      NA_real_
    }
    model_eval_$d_study <- NA_real_
    model_eval_$taxonomic_distance_to_anchor <- model_eval_$d_species
    return(model_eval_)
  }
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
#' @noRd
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
  if ("learned_distance" %in% names(out)) {
    # The Alchemist distance is already a fitted function of the taxonomic,
    # study-context, and coherence features. Retain the component diagnostics,
    # but do not inject any of them into the learned distance a second time.
    out$kernel_learned_term <- suppressWarnings(as.numeric(out$learned_distance)) /
      suppressWarnings(as.numeric(out$learned_kernel_bandwidth))
  }
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
#' @noRd
weight_anchor_models <- function(model_eval,
                                 sim_obj,
                                 config) {
  # Collapse all active distance blocks to one normalized distance and one
  # exponential kernel weight per candidate model.
  model_eval_ <- tibble::as_tibble(model_eval)
  if ("learned_distance" %in% names(model_eval_)) {
    learned_distance <- suppressWarnings(as.numeric(model_eval_$learned_distance))
    bandwidth <- suppressWarnings(as.numeric(model_eval_$learned_kernel_bandwidth))
    if (!all(is.finite(bandwidth) & bandwidth > 0, na.rm = TRUE)) {
      stop("Alchemist policy weighting requires a positive learned-distance bandwidth.", call. = FALSE)
    }
    model_eval_$combined_distance <- learned_distance
    model_eval_$trait_gower_distance <- NA_real_
    model_eval_$w_combined_raw <- ifelse(
      is.finite(learned_distance),
      exp(-learned_distance / bandwidth),
      0
    )
    w_sum <- sum(model_eval_$w_combined_raw, na.rm = TRUE)
    if (!is.finite(w_sum) || w_sum <= 0) {
      model_eval_$w_combined <- NA_real_
      return(model_eval_)
    }
    model_eval_$w_combined <- model_eval_$w_combined_raw / w_sum
    return(model_eval_)
  }
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
#' @noRd
build_admissible_pool <- function(model_eval,
                                  config,
                                  require_backscatter = TRUE) {
  # Restrict to admissible weighted rows first, then de-duplicate support at
  # the study-cell level before final admissible weights are normalized.
  study_cell_col <- build_anchor_field(config, "study_cell")
  id_chr_col <- build_anchor_field(config, "model_id")
  model_eval_ <- tibble::as_tibble(model_eval)
  if (!study_cell_col %in% names(model_eval_)) {
    model_eval_[[study_cell_col]] <- if (id_chr_col %in% names(model_eval_)) {
      as.character(model_eval_[[id_chr_col]])
    } else {
      rep(NA_character_, nrow(model_eval_))
    }
  }

  admissible_df <- if (isTRUE(require_backscatter)) {
    model_eval_ |>
      dplyr::filter(.data$admissible, is.finite(.data$w_combined), .data$w_combined > 0, is.finite(.data$biomass_multiplier_if_replace))
  } else {
    model_eval_ |>
      dplyr::filter(.data$admissible, is.finite(.data$w_combined), .data$w_combined > 0)
  }
  admissible_df <- admissible_df |>
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
#'   [prepare_similarities()].
#' @param dist_obj Optional prebuilt distance object from
#'   [construct_gower_distances()].
#' @param candidate_models_scored Optional candidate-model table that already
#'   contains `key_metadata_missing_fraction`.
#' @param excluded_model_ids Optional character vector of model IDs that should
#'   be excluded from the donor pool after the anchor's own calibration terms
#'   are derived from the full scored table.
#' @param require_backscatter Logical scalar. If `TRUE` (default), an anchor
#'   with no finite positive backscatter of its own aborts screening. Set
#'   `FALSE` to allow screening anchors with no TS-length model of their own.
#'
#' @return A list with `anchor_pdf`, `anchor_sigma`, `model_eval`,
#'   `admissible_df`, `sim_obj`, and `dist_obj`.
#'
#' @keywords internal
#' @noRd
screen_one_anchor_admissibility <- function(anchor_row,
                                            candidate_models,
                                            config = NULL,
                                            registry_path = NULL,
                                            sim_obj = NULL,
                                            dist_obj = NULL,
                                            candidate_models_scored = NULL,
                                            excluded_model_ids = NULL,
                                            require_backscatter = TRUE) {
  # Resolve the similarity prep and distance objects once, then layer the
  # anchor-specific overlap and admissibility diagnostics on top of them.
  if (!is.data.frame(anchor_row) || nrow(anchor_row) != 1) {
    stop("'anchor_row' must be a one-row data frame.", call. = FALSE)
  }
  candidates_obj <- if (is_s7_instance(candidate_models, "Candidates")) {
    candidate_models
  } else {
    NULL
  }
  if (!is.null(candidates_obj)) {
    # Reuse object-stored similarity state whenever it is already present and
    # the caller did not explicitly override it with `sim_obj`. This keeps the
    # prepared pipeline from rebuilding the same matrices inside anchor loops.
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
  model_id_col <- build_anchor_field(cfg, "model_id")

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
    sim_obj <- prepare_similarities(
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
    dist_obj <- construct_gower_distances(sim_obj)
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

  anchor_pdf <- build_anchor_length_pdf(anchor_row, cfg, on_missing = "error")
  anchor_freq <- suppressWarnings(as.numeric(anchor_row[[build_anchor_field(cfg, "frequency")]][[1]]))

  # Score every candidate model at the anchor PDF, then derive the anchor's own
  # sigma_bs value from that common scoring table.
  model_eval <- build_anchor_table(
    candidate_models = candidate_models,
    anchor_pdf = anchor_pdf,
    config = cfg
  )
  anchor_sigma <- anchor_backscatter(model_eval, anchor_id, cfg)
  anchor_pdf_status <- if ("length_pdf" %in% names(anchor_row)) {
    stringr::str_to_lower(stringr::str_squish(as.character(anchor_row$length_pdf[[1]] %||% "")))
  } else {
    ""
  }
  if ((!is.finite(anchor_sigma) || anchor_sigma <= 0) && isTRUE(require_backscatter)) {
    anchor_species <- if (build_anchor_field(cfg, "species_name") %in% names(anchor_row)) {
      as.character(anchor_row[[build_anchor_field(cfg, "species_name")]][[1]])
    } else {
      NA_character_
    }
    abort_unscorable_anchor(
      paste0(
        "Reference anchor ",
        anchor_id,
        if (!is.na(anchor_species) && nzchar(anchor_species)) paste0(" (", anchor_species, ")") else "",
        " has no finite positive anchor backscatter. ",
        "Check that the anchor has finite standardized TS-length coefficients ",
        "(`slope_standard`/`intercept_standard`) or the length-weight coefficients needed to derive them."
      ),
      reason_code = if (identical(anchor_pdf_status, "user")) {
        "invalid_anchor_backscatter_user_pdf"
      } else {
        "invalid_anchor_backscatter"
      },
      stage = "anchor_backscatter"
    )
  }

  # Keep the anchor rows available long enough to derive the reference
  # backscatter term, then drop any declared reference anchors before donor
  # distances, weights, and admissibility are computed.
  if (is.null(excluded_model_ids) &&
    !is.null(candidates_obj) &&
    nrow(tibble::as_tibble(candidates_obj@reference_anchors)) > 0) {
    excluded_model_ids <- if ("model_id" %in% names(candidates_obj@reference_anchors)) {
      as.character(candidates_obj@reference_anchors$model_id)
    } else if ("model_id_chr" %in% names(candidates_obj@reference_anchors)) {
      as.character(candidates_obj@reference_anchors$model_id_chr)
    } else {
      character(0)
    }
  }
  excluded_model_ids <- unique(as.character(excluded_model_ids %||% character(0)))
  excluded_model_ids <- excluded_model_ids[!is.na(excluded_model_ids) & nzchar(excluded_model_ids)]
  if (length(excluded_model_ids) > 0) {
    model_eval <- model_eval |>
      dplyr::filter(!(.data[[build_anchor_field(cfg, "model_id")]] %in% excluded_model_ids))
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
    config = cfg,
    require_backscatter = require_backscatter
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
#'   object, this may be left `NULL` to use its reference anchors.
#' @param candidate_models Candidate-model table or a [Candidates] object.
#' @param config Optional JSON path or list with similarity/anchor settings.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#' @param progress Logical scalar controlling stage messages.
#' @param registry_path Optional path to the trait-registry JSON.
#' @param keep_training_data Logical scalar. When `candidate_models` is an
#'   [Alchemist], the returned object drops `pair_data`/`trait_mats` (the
#'   distance-learner training-only inputs) by default; pass `TRUE` to keep
#'   them, e.g. to still call [distill_traits()] afterward.
#'
#' @return When `candidate_models` is a [Candidates] object, returns that
#'   object containing the admissibility-screen result.
#'   Otherwise, returns a list containing per-anchor results plus bound
#'   score/summary tables.
#'
#' @examples
#' \dontrun{
#' candidates <- build_candidates(list(
#'   study = list(path = "input.xlsx"),
#'   anchors = list(selector = list(regional_body = "SWFSC"))
#' ))
#' candidates <- prepare_similarities(candidates)
#' candidates <- forge_distances(candidates)
#' candidates <- screen_admissibility(candidate_models = candidates)
#' candidates
#' }
#'
#' @export
screen_admissibility <- function(reference_anchors = NULL,
                                 candidate_models,
                                 config = NULL,
                                 cache_path = NULL,
                                 refresh = NULL,
                                 progress = NULL,
                                 registry_path = NULL,
                                 keep_training_data = FALSE) {
  reference_anchors_ <- reference_anchors
  candidate_models_ <- if (missing(candidate_models)) NULL else candidate_models
  config_ <- config
  cache_path_ <- cache_path
  refresh_ <- refresh
  progress_ <- progress
  if (missing(candidate_models)) {
    if (is_s7_instance(reference_anchors_, "Alchemist")) {
      return(.screen_admissibility_alchemist(
        alchemist = reference_anchors_,
        config = config_,
        cache_path = cache_path_,
        refresh = refresh_,
        progress = progress_,
        registry_path = registry_path,
        keep_training_data = keep_training_data
      ))
    }
    if (is_s7_instance(reference_anchors_, "Candidates")) {
      candidate_models_ <- reference_anchors_
      reference_anchors_ <- NULL
    } else {
      stop(
        "'candidate_models' must be supplied unless the first argument is a `Candidates` object.",
        call. = FALSE
      )
    }
  }

  if (is_s7_instance(candidate_models_, "Alchemist")) {
    return(.screen_admissibility_alchemist(
      alchemist = candidate_models_,
      config = config_,
      cache_path = cache_path_,
      refresh = refresh_,
      progress = progress_,
      registry_path = registry_path,
      keep_training_data = keep_training_data
    ))
  }

  candidates_obj <- if (is_s7_instance(candidate_models_, "Candidates")) {
    candidate_models_
  } else {
    NULL
  }
  if (!is.null(candidates_obj)) {
    if (is.null(config_)) {
      config_ <- candidates_config_data(candidates_obj)
    }
    if (is.null(reference_anchors_)) {
      reference_anchors_ <- candidates_obj@reference_anchors
    }
    candidate_models_ <- tibble::as_tibble(candidates_obj@candidate_models)
  }
  cfg_data <- resolve_config_data(config_)
  cache_path_ <- cache_path_ %||% resolve_config_value(cfg_data, "cache_path", sections = "admissibility") %||% NULL
  refresh_ <- refresh_ %||% resolve_config_value(cfg_data, "refresh", sections = "admissibility") %||% FALSE
  progress_ <- progress_ %||% resolve_config_value(cfg_data, "progress", sections = "admissibility") %||% FALSE
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

  if (!is.null(cache_path_) && tsb_cache_exists(cache_path_) && !refresh_) {
    report_progress(progress_, "Loading cached anchor admissibility from ", cache_path_, ".")
    cached_result <- tsb_cache_read(cache_path_)
    if (!admissibility_bundle_is_current(cached_result, config_)) {
      report_progress(
        progress_,
        "Cached anchor admissibility at ",
        cache_path_,
        " does not match the current admissibility logic; rebuilding."
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
    progress_,
    "Screening admissibility for ",
    nrow(reference_anchors_),
    " reference anchors."
  )
  excluded_model_ids <- if ("model_id" %in% names(reference_anchors_)) {
    as.character(reference_anchors_$model_id)
  } else if ("model_id_chr" %in% names(reference_anchors_)) {
    as.character(reference_anchors_$model_id_chr)
  } else {
    character(0)
  }
  excluded_model_ids <- unique(excluded_model_ids[!is.na(excluded_model_ids) & nzchar(excluded_model_ids)])
  all_scores <- list()
  all_overlap <- list()
  all_gates <- list()
  all_summary <- list()
  anchor_results <- list()
  anchor_failures <- list()

  # Build the donor-weighting context once for the full anchor set. When the
  # object already carries precomputed learned/Gower distances, keep using
  # those distances rather than silently rebuilding the similarity path.
  use_precomputed_dist <- !is.null(candidates_obj) &&
    length(candidates_obj@gower_distances) > 0
  if (!is.null(candidates_obj) &&
    length(candidates_obj@similarity_matrix) > 0) {
    sim_obj <- candidates_obj@similarity_matrix
  } else if (use_precomputed_dist) {
    sim_obj <- list(
      alpha = cfg$alpha %||% NULL,
      k_species = cfg$k_species %||% NULL,
      k_study = cfg$k_study %||% NULL,
      frequency_span = compute_frequency_span(candidate_models_$frequency %||% numeric(0)),
      candidate_models = tibble::as_tibble(candidate_models_)
    )
  } else {
    sim_obj <- prepare_similarities(
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
  if (use_precomputed_dist) {
    dist_obj <- candidates_obj@gower_distances
  } else {
    dist_obj <- construct_gower_distances(sim_obj)
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

    screened_anchor <- tryCatch(
      list(
        evaluation = screen_one_anchor_admissibility(
          anchor_row = anchor_row,
          candidate_models = candidate_models_,
          config = cfg,
          registry_path = registry_path,
          sim_obj = sim_obj,
          dist_obj = dist_obj,
          candidate_models_scored = candidate_models_scored,
          excluded_model_ids = excluded_model_ids
        ),
        failure = NULL
      ),
      tsbiomass_unscorable_anchor = function(e) {
        list(
          evaluation = NULL,
          failure = tibble::tibble(
            anchor_model_id = anchor_id,
            anchor_species = anchor_species,
            failure_stage = as.character(e$stage %||% "anchor_screening"),
            failure_code = as.character(e$reason_code %||% "unscorable_anchor"),
            failure_message = conditionMessage(e)
          )
        )
      }
    )
    if (!is.null(screened_anchor$failure)) {
      anchor_failures[[length(anchor_failures) + 1L]] <- screened_anchor$failure
      report_progress(
        progress_,
        "Anchor ", anchor_id, " (", anchor_species,
        ") is unscorable: ", screened_anchor$failure$failure_message[[1]]
      )
      next
    }
    eval_obj <- screened_anchor$evaluation

    scored <- collect_anchor_scores(eval_obj, anchor_row, cfg)
    ranked <- rank_anchor_models(eval_obj)
    weighted_support <- scored |>
      dplyr::filter(
        .data$admissible,
        is.finite(.data$w_adm),
        .data$w_adm > 0
      )
    gate_admissible_n <- sum(scored$admissible %in% TRUE, na.rm = TRUE)
    weighted_support_n <- nrow(weighted_support)
    if (isTRUE(progress_) && gate_admissible_n > 0L && weighted_support_n == 0L) {
      gate_rows <- scored[scored$admissible %in% TRUE, , drop = FALSE]
      finite_w_combined_n <- if ("w_combined" %in% names(gate_rows)) {
        sum(is.finite(suppressWarnings(as.numeric(gate_rows$w_combined))), na.rm = TRUE)
      } else {
        NA_integer_
      }
      positive_w_combined_n <- if ("w_combined" %in% names(gate_rows)) {
        w_combined_now <- suppressWarnings(as.numeric(gate_rows$w_combined))
        sum(is.finite(w_combined_now) & w_combined_now > 0, na.rm = TRUE)
      } else {
        NA_integer_
      }
      finite_multiplier_n <- if ("biomass_multiplier_if_replace" %in% names(gate_rows)) {
        sum(is.finite(suppressWarnings(as.numeric(gate_rows$biomass_multiplier_if_replace))), na.rm = TRUE)
      } else {
        NA_integer_
      }
      positive_joined_w_adm_n <- if ("w_adm" %in% names(gate_rows)) {
        w_adm_now <- suppressWarnings(as.numeric(gate_rows$w_adm))
        sum(is.finite(w_adm_now) & w_adm_now > 0, na.rm = TRUE)
      } else {
        NA_integer_
      }
      support_pool_n <- nrow(tibble::as_tibble(eval_obj$admissible_df))
      support_pool_positive_w_adm_n <- if ("w_adm" %in% names(eval_obj$admissible_df)) {
        w_adm_pool_now <- suppressWarnings(as.numeric(eval_obj$admissible_df$w_adm))
        sum(is.finite(w_adm_pool_now) & w_adm_pool_now > 0, na.rm = TRUE)
      } else {
        NA_integer_
      }
      report_progress(
        progress_,
        "Anchor ",
        anchor_id,
        " (",
        anchor_species,
        ") had ",
        gate_admissible_n,
        " gate-admissible row(s), but zero weighted-support row(s). ",
        "Weighted support additionally requires finite positive w_combined and finite biomass_multiplier_if_replace. ",
        "Diagnostics: finite_w_combined=",
        finite_w_combined_n,
        ", positive_w_combined=",
        positive_w_combined_n,
        ", finite_multiplier=",
        finite_multiplier_n,
        ", support_pool_rows=",
        support_pool_n,
        ", support_pool_positive_w_adm=",
        support_pool_positive_w_adm_n,
        ", joined_positive_w_adm=",
        positive_joined_w_adm_n,
        "."
      )
    }
    overlap <- weighted_support |>
      summarize_anchor_overlap(config = config_) |>
      dplyr::mutate(anchor_model_id = anchor_id, anchor_species = anchor_species)
    gates <- summarize_gate_counts(scored, anchor_row, cfg)
    summary <- summarize_anchor_pool(scored)

    # Build a compact stored evaluation to avoid duplicating the full distance
    # and similarity objects per anchor. They are already available from the
    # parent Candidates object and are never read back from the per-anchor
    # evaluation slot. The model_eval column vectors are shared with `scored`
    # rather than copied - R's copy-on-modify preserves this sharing both
    # in-process and in saveRDS (which deduplicates shared objects).
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
    logic_version = anchor_admissibility_logic_version(),
    anchors = anchor_results,
    anchor_failures = dplyr::bind_rows(anchor_failures),
    all_scores = dplyr::bind_rows(all_scores),
    all_overlap = dplyr::bind_rows(all_overlap),
    all_gates = dplyr::bind_rows(all_gates),
    anchor_summary = dplyr::bind_rows(all_summary)
  )
  report_progress(progress, "Completed anchor admissibility screening.")

  # Cache the full in-memory result so diagnostics can be reused without
  # re-running the full anchor-by-anchor screen.
  if (!is.null(cache_path)) {
    tsb_cache_write(result, cache_path)
  }

  if (!is.null(candidates_obj)) {
    return(candidates_with_admissibility(candidates_obj, result))
  }

  result
}
