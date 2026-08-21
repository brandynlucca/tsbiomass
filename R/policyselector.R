#' Policy Selection S7 Classes
#'
#' `PolicySelector` is the object-oriented wrapper around the policy
#' benchmarking, uncertainty calibration, global policy selection, and
#' anchor-facing policy prediction layers.
#'
#' It is constructed from a [Candidates] object and then advanced
#' through four high-level methods:
#' - [benchmark()] to run the policy benchmark
#' - [calibrate_uncertainty()] to calibrate policy intervals
#' - [select_policies()] to summarize global policy performance
#' - [predict()] to generate per-anchor policy predictions
#'
#' The final `predict()` call returns a [PolicyPredictions] object rather than
#' modifying the selector in place.
#'
#' @examples
#' \dontrun{
#' selector <- as_policyselector(candidates)
#' selector <- benchmark(selector)
#' selector <- calibrate_uncertainty(selector)
#' selector <- select_policies(selector)
#' predictions <- predict(selector)
#' predictions
#' }
#'
#' @name PolicySelector-class
#' @usage NULL
#' @aliases PolicySelector
NULL

#' Policy prediction bundle
#'
#' Stores the full policy-interval table, the retained selected-policy rows,
#' and the donor-pool consensus summaries returned by [predict()] on a
#' [PolicySelector].
#'
#' @examples
#' \dontrun{
#' predictions <- predict(selector)
#' predictions
#' predictions
#' predictions
#' }
#'
#' @name PolicyPredictions-class
#' @usage NULL
#' @aliases PolicyPredictions
NULL

#' @export
PolicyPredictions <- S7::new_class(
  "PolicyPredictions",
  properties = list(
    intervals = S7::new_property(CandidatesDataFrame),
    selections = S7::new_property(CandidatesDataFrame),
    consensus = S7::new_property(CandidatesDataFrame)
  ),
  validator = function(self) {
    tryCatch(
      {
        if (!is.data.frame(self@intervals)) {
          return("`intervals` must be a data frame.")
        }
        if (!is.data.frame(self@selections)) {
          return("`selections` must be a data frame.")
        }
        if (!is.data.frame(self@consensus)) {
          return("`consensus` must be a data frame.")
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(PolicyPredictions)

#' @export
PolicySelector <- S7::new_class(
  "PolicySelector",
  properties = list(
    candidates = S7::new_property(Candidates),
    config = S7::new_property(S7::class_list),
    benchmark = S7::new_property(S7::class_list),
    uncertainty = S7::new_property(S7::class_list),
    selection = S7::new_property(S7::class_list)
  ),
  validator = function(self) {
    tryCatch(
      {
        if (!is_s7_instance(self@candidates, "Candidates")) {
          return("`candidates` must be a `Candidates` object.")
        }
        if (!is.list(self@config)) {
          return("`config` must be a list.")
        }
        if (!is.list(self@benchmark)) {
          return("`benchmark` must be a list.")
        }
        if (!is.list(self@uncertainty)) {
          return("`uncertainty` must be a list.")
        }
        if (!is.list(self@selection)) {
          return("`selection` must be a list.")
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(PolicySelector)

#' Test whether an object is a `PolicySelector` instance
#'
#' Normalize policy-selector config input
#'
#' @param config Optional config input.
#'
#' @return A list.
#'
#' @keywords internal
#' @noRd
policy_selector_config_data <- function(config) {
  if (is.null(config)) {
    return(list())
  }
  if (is_s7_instance(config, "PolicySelector")) {
    return(config@config)
  }
  if (is_s7_instance(config, "Configurer")) {
    return(config@data)
  }
  if (is.list(config)) {
    return(config)
  }

  stop(
    "'config' must be NULL, a list, a `Configurer` object, or a `PolicySelector`.",
    call. = FALSE
  )
}

#' Resolve one config value from nested sections
#'
#' @param cfg Config list.
#' @param key Value name to resolve.
#' @param sections Optional section names searched after the top level.
#'
#' @return The resolved value or `NULL`.
#'
#' @keywords internal
#' @noRd
policy_selector_config_value <- function(cfg,
                                         key,
                                         sections = character()) {
  if (!is.list(cfg)) {
    return(NULL)
  }
  if (identical(key, "progress")) {
    return(cfg$progress %||% (cfg$execution %||% list())$progress %||% NULL)
  }
  if (!is.null(cfg[[key]])) {
    return(cfg[[key]])
  }

  for (section in sections) {
    if (is.list(cfg[[section]]) && !is.null(cfg[[section]][[key]])) {
      return(cfg[[section]][[key]])
    }
  }

  NULL
}

#' Build a short, deterministic tag identifying an anchor set
#'
#' Used to keep prediction-cache files for different `reference_anchors`
#' requests (e.g. the stored anchors vs. a one-off query anchor) from
#' colliding on the same cache path.
#'
#' @param anchor_ids Character vector of anchor model IDs.
#'
#' @return A single filesystem-safe string, or `""` when `anchor_ids` is empty.
#' @keywords internal
#' @noRd
policy_selector_anchor_fingerprint <- function(anchor_ids) {
  ids <- sort(unique(as.character(anchor_ids)))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0L) {
    return("")
  }
  joined <- paste(ids, collapse = "-")
  safe <- gsub("[^A-Za-z0-9_-]", "_", joined)
  if (nchar(safe) <= 60L) {
    return(safe)
  }
  # Long or high-cardinality anchor sets: fall back to a short deterministic
  # checksum instead of a human-readable ID list.
  bytes <- as.integer(charToRaw(joined))
  checksum <- sprintf("%08x", sum(bytes * seq_along(bytes)) %% .Machine$integer.max)
  paste0("n", length(ids), "_", checksum)
}

#' Resolve a policy-prediction cache path
#'
#' @param cache_path Base prediction-cache path.
#' @param learner Optional [PolicyLearner] object.
#' @param anchor_ids Optional character vector of requested anchor model IDs.
#'   Included in the resolved path so different anchor sets (e.g. the stored
#'   reference anchors vs. a one-off query anchor) never share a cache file.
#'
#' @return A variant-specific cache path, or `NULL`.
#'
#' @keywords internal
#' @noRd
policy_selector_prediction_cache_path <- function(cache_path,
                                                  learner = NULL,
                                                  anchor_ids = NULL,
                                                  admissibility_tag = NULL) {
  if (is.null(cache_path)) {
    return(NULL)
  }
  if (!is.character(cache_path) || length(cache_path) != 1L || !nzchar(cache_path)) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  variant <- if (is_s7_instance(learner, "PolicyLearner")) {
    "learner"
  } else {
    "selector"
  }
  anchor_tag <- policy_selector_anchor_fingerprint(anchor_ids)
  if (nzchar(anchor_tag)) {
    variant <- paste0(variant, "_", anchor_tag)
  }
  if (!is.null(admissibility_tag) && length(admissibility_tag) == 1L &&
    !is.na(admissibility_tag) && nzchar(admissibility_tag)) {
    variant <- paste0(variant, "_adm_", admissibility_tag)
  }
  ext <- tools::file_ext(cache_path)
  stem <- if (nzchar(ext)) {
    sub(paste0("\\.", ext, "$"), "", basename(cache_path))
  } else {
    basename(cache_path)
  }
  file.path(
    dirname(cache_path),
    paste0(stem, "_", variant, if (nzchar(ext)) paste0(".", ext) else "")
  )
}

#' Normalize prediction-time admissibility overrides
#'
#' Extracts only donor-screening settings from either a compact anchor-config
#' list or an `admissibility` configuration fragment. Distance fitting and
#' policy-learning settings are intentionally not accepted here.
#'
#' @param override List of query-specific admissibility settings.
#'
#' @return A compact anchor-config override list.
#'
#' @keywords internal
#' @noRd
policy_selector_normalize_admissibility_override <- function(override) {
  if (is.null(override) || length(override) == 0L) return(list())
  if (!is.list(override)) {
    stop("'admissibility_override' must be NULL, a profile name, or a list.", call. = FALSE)
  }

  # `$` partially matches list names.  An inline
  # `admissibility_species_traits` field must not be mistaken for the nested
  # `admissibility` section.
  field <- function(x, name) {
    if (is.list(x) && name %in% names(x)) x[[name]] else NULL
  }
  adm <- field(override, "admissibility") %||% list()
  coherence <- field(override, "coherence") %||% field(adm, "coherence") %||% list()
  first_present <- function(...) {
    for (value in list(...)) if (!is.null(value)) return(value)
    NULL
  }
  exact_frequency <- first_present(field(override, "exact_frequency"), field(adm, "exact_frequency"))
  frequency_mode <- first_present(
    field(override, "frequency_coherence_mode"), field(override, "frequency_mode"),
    field(adm, "frequency_mode"), field(field(coherence, "frequency") %||% list(), "mode")
  )
  if (isTRUE(exact_frequency) && is.null(frequency_mode)) {
    frequency_mode <- "literal"
  }
  out <- list(
    min_length_overlap_fraction = first_present(
      field(override, "min_length_overlap_fraction"), field(override, "length_overlap_min"),
      field(adm, "length_overlap_min"), field(field(coherence, "length") %||% list(), "min")
    ),
    min_depth_overlap_fraction = first_present(
      field(override, "min_depth_overlap_fraction"), field(override, "depth_overlap_min"),
      field(adm, "depth_overlap_min"), field(field(coherence, "depth") %||% list(), "min")
    ),
    missing_key_metadata_max_fraction = first_present(
      field(override, "missing_key_metadata_max_fraction"), field(override, "key_metadata_max"),
      field(adm, "key_metadata_max")
    ),
    frequency_coherence_mode = frequency_mode,
    frequency_gap = first_present(
      field(override, "frequency_gap"), field(adm, "frequency_gap"),
      field(field(coherence, "frequency") %||% list(), "gap")
    ),
    exact_frequency = exact_frequency,
    admissibility_species_traits = first_present(
      field(override, "admissibility_species_traits"), field(adm, "species_traits")
    ),
    admissibility_study_traits = first_present(
      field(override, "admissibility_study_traits"), field(adm, "study_traits")
    )
  )
  out[!vapply(out, is.null, logical(1))]
}

#' Resolve one external-query admissibility override
#'
#' @param cfg Merged prediction configuration.
#' @param anchor_row One-row reference-anchor table.
#' @param override Optional inline override or profile name.
#'
#' @return List containing the profile label and compact override values.
#'
#' @keywords internal
#' @noRd
policy_selector_resolve_admissibility_override <- function(cfg,
                                                           anchor_row,
                                                           override = NULL) {
  profile_registry <- ((cfg$prediction %||% list())$admissibility_profiles %||% list())
  requested <- override
  if (is.null(requested) && "admissibility_profile" %in% names(anchor_row)) {
    requested <- as.character(anchor_row$admissibility_profile[[1]] %||% NA_character_)
    if (is.na(requested) || !nzchar(requested)) requested <- NULL
  }

  profile_name <- NA_character_
  profile_values <- list()
  inline_values <- list()
  if (is.character(requested)) {
    if (length(requested) != 1L || is.na(requested) || !nzchar(requested)) {
      stop("An admissibility profile name must be one non-empty string.", call. = FALSE)
    }
    profile_name <- requested
  } else if (is.list(requested)) {
    profile_name <- requested$profile %||% requested$admissibility_profile %||% NA_character_
    inline_values <- requested
    inline_values$profile <- NULL
    inline_values$admissibility_profile <- NULL
    if (!is.character(profile_name) || length(profile_name) != 1L || is.na(profile_name) || !nzchar(profile_name)) {
      profile_name <- NA_character_
    }
  } else if (!is.null(requested)) {
    stop("'admissibility_override' must be NULL, a profile name, or a list.", call. = FALSE)
  }

  if (!is.na(profile_name)) {
    if (!profile_name %in% names(profile_registry)) {
      stop(sprintf("Unknown prediction admissibility profile '%s'.", profile_name), call. = FALSE)
    }
    profile_values <- profile_registry[[profile_name]]
    if (!is.list(profile_values)) {
      stop(sprintf("Prediction admissibility profile '%s' must be a list.", profile_name), call. = FALSE)
    }
  }

  values <- merge_config_sections(
    policy_selector_normalize_admissibility_override(profile_values),
    policy_selector_normalize_admissibility_override(inline_values)
  )
  list(profile = profile_name, values = values, applied = length(values) > 0L)
}

#' Build a deterministic tag for prediction-time donor screens
#'
#' @param overrides Resolved per-anchor admissibility overrides.
#'
#' @return Character scalar or an empty string.
#'
#' @keywords internal
#' @noRd
policy_selector_admissibility_fingerprint <- function(overrides) {
  if (length(overrides) == 0L || !any(vapply(overrides, function(x) isTRUE(x$applied), logical(1)))) {
    return("")
  }
  bytes <- as.integer(serialize(overrides, connection = NULL, version = 2L))
  sprintf("%08x", sum(bytes * seq_along(bytes)) %% .Machine$integer.max)
}

#' Rebuild a `PolicySelector`
#'
#' @param object A [PolicySelector] object.
#' @param candidates Optional replacement [Candidates] object.
#' @param config Optional replacement config list.
#' @param benchmark Optional replacement benchmark result.
#' @param uncertainty Optional replacement uncertainty result.
#' @param selection Optional replacement policy-selection result.
#'
#' @return A `PolicySelector` object.
#'
#' @keywords internal
#' @noRd
policy_selector_rebuild <- function(object,
                                    candidates = object@candidates,
                                    config = object@config,
                                    benchmark = object@benchmark,
                                    uncertainty = object@uncertainty,
                                    selection = object@selection) {
  PolicySelector(
    candidates = candidates,
    config = config,
    benchmark = benchmark,
    uncertainty = uncertainty,
    selection = selection
  )
}

#' Build a `PolicySelector`
#'
#' @param candidates A [Candidates] or [Alchemist] object.
#' @param config Optional selector config list or [Configurer] object.
#'
#' @return A `PolicySelector` object.
#'
#' @examples
#' \dontrun{
#' selector <- as_policyselector(candidates)
#' selector
#' }
#'
#' @export
as_policyselector <- function(candidates,
                              config = NULL) {
  if (is_s7_instance(candidates, "Alchemist")) {
    alchemist <- candidates
    distance_matrix <- alchemist@distance_matrix
    if (length(distance_matrix) == 0L) {
      stop("Run `forge_distances()` before `as_policyselector()`.", call. = FALSE)
    }

    gower_bundle <- list(
      combined_dist = distance_matrix$combined_dist,
      species_dist = distance_matrix$combined_dist,
      study_dist = as.matrix(distance_matrix$dist_matrix),
      species_dist_model = as.matrix(distance_matrix$dist_matrix),
      learned_directed_dist = distance_matrix$directed_dist_matrix %||% NULL,
      taxonomic_dist_model = distance_matrix$taxonomic_dist_matrix %||% NULL,
      learned_kernel_bandwidth = distance_matrix$learned_kernel_bandwidth %||% NULL,
      distance_mode = "alchemist_super_learner",
      trait_cols = distance_matrix$trait_cols %||% distance_matrix$all_traits %||% character(0),
      distance_learner = canonicalize_distance_learner(alchemist@learner),
      feature_cols = distance_matrix$feature_cols %||% resolve_distance_learner(alchemist@learner)$feature_cols %||% character(0),
      species_trait_names = distance_matrix$species_trait_names %||% character(0),
      study_trait_names = distance_matrix$study_trait_names %||% character(0),
      feature_type = distance_matrix$feature_type %||% NULL,
      coherence_config = distance_matrix$coherence_config %||% NULL,
      taxonomic_distance = distance_matrix$taxonomic_distance %||% NULL,
      feature_normalization = distance_matrix$feature_normalization %||% NULL
    )
    candidates <- candidates_with_gower_distances(alchemist@candidates, gower_bundle)
    if (length(alchemist@admissibility) > 0L) {
      candidates <- candidates_with_admissibility(candidates, alchemist@admissibility)
    }
    if (length(alchemist@ordination) > 0L) {
      candidates <- candidates_with_ordination(candidates, alchemist@ordination)
    }
  }

  if (!is_s7_instance(candidates, "Candidates")) {
    stop("'candidates' must be a `Candidates` or `Alchemist` object.", call. = FALSE)
  }

  PolicySelector(
    candidates = candidates,
    config = policy_selector_config_data(config),
    benchmark = list(),
    uncertainty = list(),
    selection = list()
  )
}

#' Resolve similarity defaults from `Candidates`
#'
#' @param candidates A [Candidates] object.
#'
#' @return A list.
#'
#' @keywords internal
#' @noRd
policy_selector_similarity_defaults <- function(candidates) {
  if (length(candidates@similarity_tuning) > 0 &&
    is.list((candidates@similarity_tuning)$config_tuned %||% NULL)) {
    tuned <- (candidates@similarity_tuning)$config_tuned
    return(list(
      species_traits = as.list(tuned$species_weights %||% list()),
      study_traits = as.list(tuned$study_weights %||% list()),
      alpha = tuned$alpha %||% NULL,
      k_species = tuned$k_species %||% NULL,
      k_study = tuned$k_study %||% NULL,
      length_overlap_weight = tuned$coherence$length_coherence$weight %||% NULL,
      depth_overlap_weight = tuned$coherence$depth_coherence$weight %||% NULL,
      frequency_coherence_weight = tuned$coherence$frequency_coherence$weight %||% NULL,
      frequency_coherence_mode = tuned$coherence$frequency_coherence$method %||% NULL
    ))
  }

  if (length(candidates@similarity_matrix) > 0) {
    sim <- candidates@similarity_matrix
    return(list(
      species_traits = as.list(sim$species_weights %||% list()),
      study_traits = as.list(sim$study_weights %||% list()),
      alpha = sim$alpha %||% NULL,
      k_species = sim$k_species %||% NULL,
      k_study = sim$k_study %||% NULL,
      length_overlap_weight = sim$config$length_coherence$weight %||% NULL,
      depth_overlap_weight = sim$config$depth_coherence$weight %||% NULL,
      frequency_coherence_weight = sim$config$frequency_coherence$weight %||% NULL,
      frequency_coherence_mode = sim$config$frequency_coherence$method %||% NULL
    ))
  }

  candidate_cfg <- candidates_configuration(candidates) %||% list()
  if (is.list(candidate_cfg) && length(candidate_cfg) > 0) {
    cfg <- read_similarity_config(candidate_cfg)
    return(list(
      species_traits = cfg$species_traits %||% list(),
      study_traits = cfg$study_traits %||% list(),
      alpha = cfg$alpha %||% NULL,
      k_species = cfg$k_species %||% NULL,
      k_study = cfg$k_study %||% NULL,
      length_overlap_weight = cfg$length_coherence$weight %||% NULL,
      depth_overlap_weight = cfg$depth_coherence$weight %||% NULL,
      frequency_coherence_weight = cfg$frequency_coherence$weight %||% NULL,
      frequency_coherence_mode = cfg$frequency_coherence$method %||% NULL
    ))
  }

  cfg <- create_configuration_template()
  list(
    species_traits = as.list(cfg$similarity$species_traits %||% list()),
    study_traits = as.list(cfg$similarity$study_traits %||% list()),
    alpha = cfg$similarity$alpha %||% NULL,
    k_species = cfg$similarity$kernel_scale %||% cfg$similarity$k_species %||% NULL,
    k_study = cfg$similarity$kernel_scale %||% cfg$similarity$k_study %||% NULL,
    length_overlap_weight = cfg$similarity$coherence$length$weight %||% NULL,
    depth_overlap_weight = cfg$similarity$coherence$depth$weight %||% NULL,
    frequency_coherence_weight = cfg$similarity$coherence$frequency$weight %||% NULL,
    frequency_coherence_mode = cfg$similarity$coherence$frequency$method %||% NULL
  )
}

#' Resolve the benchmark/admissibility config for a `PolicySelector`
#'
#' @param object A [PolicySelector] object.
#' @param config Optional config overrides.
#'
#' @return A list.
#'
#' @keywords internal
#' @noRd
policy_selector_anchor_config <- function(object,
                                          config = NULL) {
  cfg <- merge_config_sections(object@config, policy_selector_config_data(config))
  defaults <- policy_selector_similarity_defaults(object@candidates)
  overrides <- list(
    species_traits = policy_selector_config_value(
      cfg, "species_traits",
      sections = c("similarity", "benchmark", "policy")
    ),
    study_traits = policy_selector_config_value(
      cfg, "study_traits",
      sections = c("similarity", "benchmark", "policy")
    ),
    admissibility_species_traits = policy_selector_config_value(
      cfg, "species_traits",
      sections = c("admissibility", "benchmark")
    ),
    admissibility_study_traits = policy_selector_config_value(
      cfg, "study_traits",
      sections = c("admissibility", "benchmark")
    ),
    alpha = policy_selector_config_value(
      cfg, "alpha",
      sections = c("similarity", "benchmark", "policy")
    ),
    k_species = policy_selector_config_value(
      cfg, "k_species",
      sections = c("similarity", "benchmark", "policy")
    ),
    k_study = policy_selector_config_value(
      cfg, "k_study",
      sections = c("similarity", "benchmark", "policy")
    ),
    length_overlap_weight = policy_selector_config_value(
      cfg, "length_overlap_weight",
      sections = c("similarity", "benchmark", "policy")
    ),
    depth_overlap_weight = policy_selector_config_value(
      cfg, "depth_overlap_weight",
      sections = c("similarity", "benchmark", "policy")
    ),
    frequency_coherence_weight = policy_selector_config_value(
      cfg, "frequency_coherence_weight",
      sections = c("similarity", "benchmark", "policy")
    ),
    frequency_coherence_mode = policy_selector_config_value(
      cfg, "frequency_coherence_mode",
      sections = c("admissibility", "benchmark", "policy", "similarity")
    ),
    frequency_gap = policy_selector_config_value(
      cfg, "frequency_gap",
      sections = c("admissibility", "benchmark", "policy", "similarity")
    ),
    min_length_overlap_fraction = policy_selector_config_value(
      cfg, "min_length_overlap_fraction",
      sections = c("admissibility", "policy", "benchmark")
    ),
    min_depth_overlap_fraction = policy_selector_config_value(
      cfg, "min_depth_overlap_fraction",
      sections = c("admissibility", "policy", "benchmark")
    ),
    missing_key_metadata_max_fraction = policy_selector_config_value(
      cfg, "missing_key_metadata_max_fraction",
      sections = c("admissibility", "policy", "benchmark")
    ),
    core_weight_cutoff = policy_selector_config_value(
      cfg, "core_weight_cutoff",
      sections = c("similarity", "policy", "benchmark")
    ),
    seed = policy_selector_config_value(
      cfg, "seed",
      sections = c("benchmark", "tuning")
    )
  )
  overrides <- overrides[!vapply(overrides, is.null, logical(1))]

  out <- merge_config_sections(
    defaults,
    overrides
  )

  for (nm in c(
    "species_traits",
    "study_traits",
    "admissibility_species_traits",
    "admissibility_study_traits"
  )) {
    if (!is.null(overrides[[nm]])) {
      out[[nm]] <- overrides[[nm]]
    }
  }

  out
}

#' Resolve the active policy set
#'
#' @param cfg Config list.
#' @param policies Optional explicit policy vector.
#'
#' @return Character vector or `NULL`.
#'
#' @keywords internal
#' @noRd
policy_selector_active_policies <- function(cfg,
                                            policies = NULL) {
  if (!is.null(policies)) {
    out <- as.character(unlist(policies, use.names = FALSE))
    out <- out[!is.na(out) & nzchar(out)]
    return(unique(out))
  }

  out <- policy_selector_config_value(cfg, "active", sections = "policies")
  if (is.null(out)) {
    out <- policy_selector_config_value(cfg, "active_policies", sections = "policies")
  }
  if (is.null(out)) {
    return(NULL)
  }

  out <- as.character(unlist(out, use.names = FALSE))
  unique(out[!is.na(out) & nzchar(out)])
}

#' Resolve reference anchors from a `PolicySelector`
#'
#' @param object A [PolicySelector] object.
#' @param reference_anchors Optional explicit anchor table.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
policy_selector_reference_anchors <- function(object,
                                              reference_anchors = NULL) {
  out <- if (is.null(reference_anchors)) {
    object@candidates@reference_anchors
  } else {
    reference_anchors
  }

  if (!is.data.frame(out) || nrow(out) == 0) {
    stop(
      "Reference anchors are required. Set them on `Candidates` first or supply `reference_anchors` explicitly.",
      call. = FALSE
    )
  }

  tibble::as_tibble(out)
}

#' Resolve one cached anchor evaluation
#'
#' @param object A [PolicySelector] object.
#' @param anchor_row One-row anchor table.
#' @param config_supplied Logical scalar.
#'
#' @return Cached anchor-evaluation object or `NULL`.
#'
#' @keywords internal
#' @noRd
policy_selector_cached_anchor_evaluation <- function(object,
                                                     anchor_row,
                                                     config_supplied) {
  if (length(object@candidates@admissibility) == 0) {
    return(NULL)
  }
  if (!is.list((object@candidates@admissibility)$anchors %||% NULL)) {
    return(NULL)
  }

  anchor_id <- if ("model_id" %in% names(anchor_row)) {
    as.character(anchor_row$model_id[[1]])
  } else {
    as.character(anchor_row$model_id[[1]])
  }

  ((object@candidates@admissibility)$anchors[[anchor_id]])$evaluation %||% NULL
}

#' Resolve one cached unscorable-anchor diagnostic
#'
#' @param object A [PolicySelector] object.
#' @param anchor_row One-row anchor table.
#'
#' @return One-row failure tibble or an empty tibble.
#'
#' @keywords internal
#' @noRd
policy_selector_cached_anchor_failure <- function(object,
                                                  anchor_row) {
  failures <- tibble::as_tibble(
    (object@candidates@admissibility %||% list())$anchor_failures %||%
      tibble::tibble()
  )
  if (nrow(failures) == 0L || !"anchor_model_id" %in% names(failures)) {
    return(tibble::tibble())
  }

  anchor_id <- if ("model_id" %in% names(anchor_row)) {
    as.character(anchor_row$model_id[[1]])
  } else {
    as.character(anchor_row$model_id[[1]])
  }
  failures |>
    dplyr::filter(as.character(.data$anchor_model_id) == anchor_id) |>
    dplyr::slice_head(n = 1L)
}

#' Canonicalize rows that are structurally equivalent across policies
#'
#' @param policy_tbl Policy candidate table.
#'
#' @return Tibble with stable equivalence fields resolved.
#'
#' @keywords internal
#' @noRd
canonicalize_equivalent_policy_rows <- function(policy_tbl) {
  policy_tbl <- normalize_policy_columns(policy_tbl)
  if (nrow(policy_tbl) == 0L ||
    !"realized_donor_fingerprint" %in% names(policy_tbl) ||
    !"multiplier_pred" %in% names(policy_tbl) ||
    !"policy_slope_len" %in% names(policy_tbl) ||
    !"policy_intercept_len" %in% names(policy_tbl)) {
    return(policy_tbl)
  }

  candidate_pool_values <- if ("candidate_pool" %in% names(policy_tbl)) policy_tbl$candidate_pool else NULL
  aggregation_values <- if ("aggregation_method" %in% names(policy_tbl)) policy_tbl$aggregation_method else NULL
  policy_family_values <- if ("policy_family" %in% names(policy_tbl)) policy_tbl$policy_family else NULL
  branch_values <- if ("equation_branch_filter" %in% names(policy_tbl)) policy_tbl$equation_branch_filter else NULL
  policy_tbl$specificity_rank <- policy_specificity_rank(
    policy = policy_tbl$policy,
    candidate_pool = candidate_pool_values,
    aggregation_method = aggregation_values,
    policy_family = policy_family_values,
    equation_branch_filter = branch_values
  )

  policy_tbl |>
    dplyr::mutate(
      .dedupe_multiplier = signif(suppressWarnings(as.numeric(.data$multiplier_pred)), 12),
      .dedupe_slope = signif(suppressWarnings(as.numeric(.data$policy_slope_len)), 12),
      .dedupe_intercept = signif(suppressWarnings(as.numeric(.data$policy_intercept_len)), 12)
    ) |>
    dplyr::group_by(
      .data$anchor_model_id,
      .data$realized_donor_fingerprint,
      .data$.dedupe_multiplier,
      .data$.dedupe_slope,
      .data$.dedupe_intercept
    ) |>
    dplyr::arrange(
      .data$specificity_rank,
      .data$policy,
      .by_group = TRUE
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(c(
      ".dedupe_multiplier",
      ".dedupe_slope",
      ".dedupe_intercept"
    )))
}

#' Benchmark a `PolicySelector`
#'
#' Runs the empirical benchmark stage for a selector. The method evaluates the
#' active policy set against pseudo-anchor and species-block holdout schemes,
#' optionally computes TS-error curves, and stores the resulting policy
#' performance tables on the selector.
#'
#' Benchmarking consumes the selector's [Candidates] object, learned similarity
#' context, active policy configuration, and reference-anchor identifiers. A new
#' benchmark invalidates any stored uncertainty calibration and policy
#' selection, so those downstream layers are cleared on the returned selector.
#'
#' @name benchmark.PolicySelector
#' @usage NULL
#'
#' @param object A [PolicySelector] object.
#' @param policies Optional character vector of policy names to evaluate.
#' @param policy_fun Optional policy-evaluation function used to score one
#'   anchor-policy combination.
#' @param curve_fun Optional curve-prediction function used for TS-error
#'   evaluation.
#' @param model_scores Optional ordination model-score table override.
#' @param species_lookup Optional ordination species-lookup table override.
#' @param reference_ids Optional reference-anchor id override.
#' @param policy_params Optional policy-parameter overrides applied during
#'   policy evaluation.
#' @param policy_path Optional policy-registry path.
#' @param config Optional config override.
#' @param include_ts_error Optional logical scalar controlling time-series error
#'   evaluation.
#' @param benchmark_schemes Character vector of benchmark scheme names.
#' @param workers Optional worker count.
#' @param engine Policy benchmark engine, `"r"` or `"cpp"`.
#' @param cache_path Optional cache path for reusable benchmark artifacts.
#' @param refresh Optional logical scalar controlling whether cached benchmark
#'   artifacts are ignored and rebuilt.
#' @param progress Optional logical scalar controlling progress messages.
#' @param group_block_col Optional blocking column for grouped species holds.
#' @param group_block_label Label attached to grouped holdout rows.
#' @param registry_path Optional trait-registry path.
#'
#' @return An updated [PolicySelector] object with benchmark policy-performance
#'   tables, species-block summaries, optional TS-error rows, and benchmark
#'   metadata.
S7::method(benchmark, PolicySelector) <- function(object,
                                                  policies = NULL,
                                                  policy_fun = evaluate_policies,
                                                  curve_fun = predict_policy_curve,
                                                  model_scores = NULL,
                                                  species_lookup = NULL,
                                                  reference_ids = NULL,
                                                  policy_params = list(),
                                                  policy_path = NULL,
                                                  config = NULL,
                                                  include_ts_error = NULL,
                                                  benchmark_schemes = c("pseudo_anchor", "species_block"),
                                                  workers = NULL,
                                                  engine = NULL,
                                                  cache_path = NULL,
                                                  refresh = NULL,
                                                  progress = NULL,
                                                  group_block_col = NULL,
                                                  group_block_label = "leave_one_group_out",
                                                  registry_path = NULL) {
  cfg <- merge_config_sections(object@config, policy_selector_config_data(config))
  # Resolve benchmark-stage controls before collecting the reusable ordination
  # context and reference-anchor identifiers.
  include_ts_error <- include_ts_error %||%
    policy_selector_config_value(cfg, "include_ts_error", sections = "benchmark") %||%
    TRUE
  workers <- workers %||%
    policy_selector_config_value(cfg, "workers", sections = "benchmark") %||%
    1L
  engine <- normalize_policy_benchmark_engine(
    engine %||% policy_selector_config_value(cfg, "engine", sections = "benchmark") %||% "r"
  )
  cache_path <- cache_path %||%
    policy_selector_config_value(cfg, "cache_path", sections = "benchmark")
  refresh <- refresh %||%
    policy_selector_config_value(cfg, "refresh", sections = "benchmark") %||%
    FALSE
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = "benchmark") %||%
    FALSE
  policy_params <- merge_config_sections(
    (((cfg$policies) %||% list())$policy_params %||% list()),
    merge_config_sections(
      list(
        slope_class = policy_selector_config_value(
          cfg, "slope_class",
          sections = "policies"
        )
      ),
      policy_params
    )
  )
  ordination_context <- policy_selector_ordination_context(
    object@candidates@ordination
  )
  model_scores <- model_scores %||% ordination_context$model_scores
  species_lookup <- species_lookup %||% ordination_context$species_lookup

  if (is.null(reference_ids) && nrow(object@candidates@reference_anchors) > 0) {
    if ("model_id" %in% names(object@candidates@reference_anchors)) {
      reference_ids <- as.character(object@candidates@reference_anchors$model_id)
    } else if ("model_id" %in% names(object@candidates@reference_anchors)) {
      reference_ids <- as.character(object@candidates@reference_anchors$model_id)
    }
  }

  # Execute the benchmark once the active policy set, anchor config, and
  # optional grouped holdout settings have all been resolved.
  benchmark_obj <- run_policy_benchmark(
    candidate_models = object@candidates,
    policy_fun = policy_fun,
    curve_fun = curve_fun,
    model_scores = model_scores,
    species_lookup = species_lookup,
    reference_ids = reference_ids,
    policies = policy_selector_active_policies(cfg, policies),
    policy_params = policy_params,
    policy_path = policy_path,
    config = cfg,
    include_ts_error = include_ts_error,
    benchmark_schemes = benchmark_schemes,
    workers = workers,
    engine = engine,
    cache_path = cache_path,
    refresh = refresh,
    progress = progress,
    group_block_col = group_block_col,
    group_block_label = group_block_label,
    registry_path = registry_path
  )
  report_progress(progress, "Completed policy benchmark.")

  # Rebuild the selector with a fresh benchmark bundle and clear downstream
  # layers that depended on any previous benchmark.
  policy_selector_rebuild(
    object,
    config = cfg,
    benchmark = benchmark_obj,
    uncertainty = list(),
    selection = list()
  )
}

#' Calibrate benchmark-based uncertainty for a `PolicySelector`
#'
#' Uses the selector's stored benchmark tables to estimate policy-transfer
#' uncertainty before final policy selection. The method combines pseudo-anchor
#' policy-performance rows, species-block performance rows, and optional
#' time-series error rows, then builds conformal calibration summaries for
#' multiplier intervals and TS-error envelopes.
#'
#' The returned selector keeps the original candidates and benchmark results and
#' replaces the uncertainty layer with a fresh calibration bundle. Downstream
#' calls to [select_policies()], [stats::predict()], and [as_referee()] use this
#' bundle to attach interval bounds, support diagnostics, and calibration
#' provenance to selected policies.
#'
#' @name calibrate_uncertainty.PolicySelector
#' @usage NULL
#'
#' @param object A [PolicySelector] object.
#' @param alpha Optional conformal alpha. Smaller values produce wider
#'   intervals; when omitted, the value is read from the selector
#'   configuration.
#' @param policy_perf Optional policy-performance table override. By default,
#'   the method uses the selector's stored benchmark policy-performance table.
#' @param species_performance_table Optional species-block performance table
#'   override used to estimate species-held-out residual behavior.
#' @param ts_error Optional time-series error table override used for
#'   functional TS-error envelopes.
#' @param pseudo_label Label assigned to pseudo-anchor calibration rows.
#' @param species_label Label assigned to species-block calibration rows.
#' @param config Optional config override.
#' @param cache_path Optional cache path for reusable calibration artifacts.
#' @param refresh Optional logical scalar controlling whether cached
#'   calibration artifacts are ignored and rebuilt.
#' @param progress Optional logical scalar controlling progress messages.
#'
#' @return An updated [PolicySelector] object whose uncertainty property
#'   contains conformal thresholds, calibration rows, TS-error summaries, and
#'   diagnostics derived from the benchmark tables.
S7::method(calibrate_uncertainty, PolicySelector) <- function(object,
                                                              alpha = NULL,
                                                              policy_perf = NULL,
                                                              species_performance_table = NULL,
                                                              ts_error = NULL,
                                                              pseudo_label = "pseudo_anchor",
                                                              species_label = "species_block",
                                                              config = NULL,
                                                              cache_path = NULL,
                                                              refresh = NULL,
                                                              progress = NULL) {
  cfg <- merge_config_sections(object@config, policy_selector_config_data(config))
  if (length(object@benchmark) == 0) {
    stop("No benchmark results are stored on this `PolicySelector`.", call. = FALSE)
  }
  benchmark_obj <- object@benchmark
  # Reuse the stored benchmark tables unless the caller supplied explicit
  # overrides for conformal calibration.
  cache_path <- cache_path %||%
    policy_selector_config_value(cfg, "cache_path", sections = c("uncertainty", "selection"))
  refresh <- refresh %||%
    policy_selector_config_value(cfg, "refresh", sections = c("uncertainty", "selection")) %||%
    FALSE
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = c("uncertainty", "selection")) %||%
    FALSE
  report_progress(progress, "Calibrating selector uncertainty.")

  uncertainty_obj <- run_anchor_conformal(
    policy_perf = policy_perf %||% benchmark_obj$policy_perf %||% tibble::tibble(),
    species_performance_table = species_performance_table %||% benchmark_obj$species_block_perf %||% NULL,
    ts_error = ts_error %||% benchmark_obj$policy_ts_error %||% NULL,
    alpha = alpha %||% policy_selector_config_value(
      cfg, "conformal_alpha",
      sections = c("selection", "uncertainty", "conformal", "policy")
    ) %||% policy_selector_config_value(
      cfg, "alpha",
      sections = c("selection", "uncertainty", "conformal")
    ) %||% 0.10,
    pseudo_label = pseudo_label,
    species_label = species_label,
    cache_path = cache_path,
    refresh = refresh
  )
  report_progress(progress, "Completed selector uncertainty calibration.")

  # Persist the conformal bundle without disturbing the benchmark layer.
  policy_selector_rebuild(
    object,
    config = cfg,
    uncertainty = uncertainty_obj
  )
}

#' Select benchmark-supported policies from a `PolicySelector`
#'
#' Applies the selector's policy-selection rules to the calibrated benchmark
#' evidence. The method starts from the species-block performance table, ranks
#' policies by benchmark error and specificity, applies equivalence and
#' one-standard-error style tolerance rules, and stores the selected policy set
#' plus selection diagnostics on the selector.
#'
#' This step expects [benchmark()] and [calibrate_uncertainty()] to have already
#' populated the selector. It does not rerun policy evaluation; it reduces the
#' existing benchmark and uncertainty layers to the policies considered
#' acceptable for prediction and scorecard reporting.
#'
#' @name select_policies.PolicySelector
#' @usage NULL
#'
#' @param object A [PolicySelector] object.
#' @param species_performance_table Optional species-block performance table
#'   override. When omitted, the selector's stored benchmark table is used.
#' @param config Optional config override.
#' @param cache_path Optional cache path for reusable selection artifacts.
#' @param refresh Optional logical scalar controlling whether cached selection
#'   artifacts are ignored and rebuilt.
#' @param progress Optional logical scalar controlling progress messages.
#'
#' @return An updated [PolicySelector] object with selected policies,
#'   selection reference tables, and anchor-level selection diagnostics.
S7::method(select_policies, PolicySelector) <- function(object,
                                                        species_performance_table = NULL,
                                                        config = NULL,
                                                        cache_path = NULL,
                                                        refresh = NULL,
                                                        progress = NULL) {
  cfg <- merge_config_sections(object@config, policy_selector_config_data(config))
  if (length(object@benchmark) == 0) {
    stop("No benchmark results are stored on this `PolicySelector`.", call. = FALSE)
  }
  benchmark_obj <- object@benchmark
  # Resolve the lightweight selection controls up front, then trim away any
  # unset values before handing the config to the selection runner.
  cache_path <- cache_path %||%
    policy_selector_config_value(cfg, "cache_path", sections = "selection")
  refresh <- refresh %||%
    policy_selector_config_value(cfg, "refresh", sections = "selection") %||%
    FALSE
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = "selection") %||%
    FALSE
  report_progress(progress, "Selecting benchmark-supported policies.")

  selection_cfg <- list(
    one_se_multiplier = policy_selector_config_value(cfg, "one_se_multiplier", sections = "selection"),
    equivalence_tolerance = policy_selector_config_value(cfg, "equivalence_tolerance", sections = "selection") %||%
      policy_selector_config_value(cfg, "tolerance", sections = "selection"),
    n_boot = policy_selector_config_value(cfg, "n_boot", sections = "selection"),
    seed = policy_selector_config_value(cfg, "seed", sections = "selection")
  )
  selection_cfg <- selection_cfg[!vapply(selection_cfg, is.null, logical(1))]

  selection_obj <- run_policy_selection(
    species_performance_table = species_performance_table %||% benchmark_obj$species_block_perf %||% tibble::tibble(),
    candidate_models = object@candidates@candidate_models %||% tibble::tibble(),
    config = selection_cfg,
    cache_path = cache_path,
    refresh = refresh,
    progress = progress
  )
  report_progress(progress, "Completed policy selection.")

  # Persist the selection summaries alongside the existing benchmark and
  # uncertainty layers.
  policy_selector_rebuild(
    object,
    config = cfg,
    selection = selection_obj
  )
}

#' Predict anchor-level policy outputs from a PolicySelector
#'
#' Scores each reference anchor under the active policy set, joins benchmark and
#' uncertainty summaries, and returns a [PolicyPredictions] bundle.
#'
#' @param object A [PolicySelector] object.
#' @param reference_anchors Optional anchor table override.
#' @param policies Optional character vector of policy names.
#' @param config Optional config override.
#' @param admissibility_override Optional query-time donor-screen override.
#'   Supply either a profile name defined in
#'   `prediction.admissibility_profiles`, or a list containing admissibility
#'   settings such as `min_length_overlap_fraction`. The override is applied
#'   only while screening the supplied anchors; it never refits stored
#'   distance, policy, or uncertainty models.
#' @param policy_params Optional policy-parameter overrides.
#' @param policy_path Optional policy-registry path.
#' @param registry_path Optional trait-registry path.
#' @param learner Optional fitted [PolicyLearner] used to rank anchor-policy
#'   rows before retaining the selected rows.
#' @param use_support_bin_intervals Logical scalar controlling whether the
#'   learner-backed path uses support-bin conformal lookups when available.
#' @param max_selection_tolerance Optional numeric tie tolerance for the
#'   learner-backed selection path.
#' @param reuse_admissibility Logical scalar. If `TRUE`, reuse admissibility
#'   results already stored on the selector's [Candidates] object.
#' @param cache_path Optional prediction-bundle cache path. When supplied, the
#'   method writes separate selector-only and learner-backed cache files derived
#'   from this base path.
#' @param refresh Optional logical scalar controlling whether cached prediction
#'   bundles are ignored and rebuilt.
#' @param progress Optional logical scalar controlling stage messages.
#'
#' @return A [PolicyPredictions] object.
#' @keywords internal
#' @noRd
.predict_policy_selector <- function(object,
                                     reference_anchors = NULL,
                                     policies = NULL,
                                     config = NULL,
                                     admissibility_override = NULL,
                                     policy_params = list(),
                                     policy_path = NULL,
                                     registry_path = NULL,
                                     learner = NULL,
                                     use_support_bin_intervals = NULL,
                                     max_selection_tolerance = NULL,
                                     reuse_admissibility = TRUE,
                                     cache_path = NULL,
                                     refresh = NULL,
                                     progress = NULL,
                                     ...) {
  dots <- list(...)
  if ("reference_anchors" %in% names(dots)) {
    reference_anchors <- dots[["reference_anchors"]]
  }
  if ("policies" %in% names(dots)) {
    policies <- dots[["policies"]]
  }
  if ("config" %in% names(dots)) {
    config <- dots[["config"]]
  }
  if ("admissibility_override" %in% names(dots)) {
    admissibility_override <- dots[["admissibility_override"]]
  }
  if ("policy_params" %in% names(dots)) {
    policy_params <- dots[["policy_params"]]
  }
  if ("policy_path" %in% names(dots)) {
    policy_path <- dots[["policy_path"]]
  }
  if ("registry_path" %in% names(dots)) {
    registry_path <- dots[["registry_path"]]
  }
  if ("learner" %in% names(dots)) {
    learner <- dots[["learner"]]
  }
  if ("use_support_bin_intervals" %in% names(dots)) {
    use_support_bin_intervals <- dots[["use_support_bin_intervals"]]
  }
  if ("max_selection_tolerance" %in% names(dots)) {
    max_selection_tolerance <- dots[["max_selection_tolerance"]]
  }
  if ("reuse_admissibility" %in% names(dots)) {
    reuse_admissibility <- dots[["reuse_admissibility"]]
  }
  if ("cache_path" %in% names(dots)) {
    cache_path <- dots[["cache_path"]]
  }
  if ("refresh" %in% names(dots)) {
    refresh <- dots[["refresh"]]
  }
  if ("progress" %in% names(dots)) {
    progress <- dots[["progress"]]
  }
  dot_names <- names(dots) %||% rep("", length(dots))
  positional_dots <- dots[!nzchar(dot_names)]
  if (is.null(reference_anchors) && length(positional_dots) > 0L) {
    reference_anchors <- positional_dots[[1L]]
  }
  # Resolve slot presence
  learner <- resolve_policy_learner(learner)
  cfg <- merge_config_sections(object@config, policy_selector_config_data(config))
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = c("selection", "benchmark")) %||%
    FALSE
  refresh <- refresh %||%
    policy_selector_config_value(cfg, "prediction_refresh", sections = c("selection", "prediction")) %||%
    policy_selector_config_value(cfg, "refresh", sections = c("prediction", "selection")) %||%
    FALSE
  cache_path <- cache_path %||%
    policy_selector_config_value(cfg, "prediction_cache_path", sections = c("selection", "prediction")) %||%
    policy_selector_config_value(cfg, "cache_path", sections = "prediction")
  policy_params <- merge_config_sections(
    (((cfg$policies) %||% list())$policy_params %||% list()),
    merge_config_sections(
      list(
        slope_class = policy_selector_config_value(
          cfg, "slope_class",
          sections = "policies"
        )
      ),
      policy_params
    )
  )
  if (length(object@uncertainty) == 0) {
    stop("No uncertainty calibration is stored on this `PolicySelector`.", call. = FALSE)
  }
  uncertainty_obj <- object@uncertainty
  selection_obj <- if (is_s7_instance(learner, "PolicyLearner")) {
    object@selection %||% list()
  } else {
    if (length(object@selection) == 0) {
      stop("No policy-selection summary is stored on this `PolicySelector`.", call. = FALSE)
    }
    object@selection
  }
  anchors_tbl <- policy_selector_reference_anchors(object, reference_anchors)
  active_policies <- policy_selector_active_policies(cfg, policies)
  if (is.null(policies) &&
    is_s7_instance(learner, "PolicyLearner")) {
    # The meta-learner should score the full benchmarked policy set by default.
    active_policies <- NULL
  }
  ordination_context <- policy_selector_ordination_context(
    object@candidates@ordination
  )
  model_scores <- ordination_context$model_scores
  species_lookup <- ordination_context$species_lookup
  anchor_cfg <- policy_selector_anchor_config(object, config = cfg)
  resolved_admissibility_overrides <- lapply(
    seq_len(nrow(anchors_tbl)),
    function(i) policy_selector_resolve_admissibility_override(
      cfg = cfg,
      anchor_row = anchors_tbl[i, , drop = FALSE],
      override = admissibility_override
    )
  )
  conf_tbl <- tibble::as_tibble(uncertainty_obj$conf_cal %||% tibble::tibble())
  select_tbl <- tibble::as_tibble(selection_obj$final_ref %||% tibble::tibble())
  benchmark_policy_tbl <- tibble::as_tibble((object@benchmark)$policy_perf %||% tibble::tibble())
  if (nrow(select_tbl) == 0L &&
    nrow(tibble::as_tibble((object@benchmark)$species_block_perf %||% tibble::tibble())) > 0L) {
    # Rebuild the benchmark-derived selection reference table
    selection_obj <- run_policy_selection(
      species_performance_table = tibble::as_tibble((object@benchmark)$species_block_perf %||% tibble::tibble()),
      candidate_models = object@candidates@candidate_models %||% tibble::tibble(),
      config = cfg,
      progress = FALSE
    )
    select_tbl <- tibble::as_tibble(selection_obj$final_ref %||% tibble::tibble())
  }
  infer_policy_bases <- function(tbl) {
    tbl <- tibble::as_tibble(tbl)
    if ("policy_base" %in% names(tbl)) {
      out <- as.character(tbl$policy_base)
    } else {
      policy_ids <- resolve_policy_names(tbl)
      policy_ids <- policy_ids[!is.na(policy_ids) & nzchar(policy_ids)]
      if (length(policy_ids) == 0) {
        return(character(0))
      }
      out <- policy_ids
    }
    unique(out[!is.na(out) & nzchar(out)])
  }
  infer_equation_branch_filters <- function(tbl) {
    tbl <- tibble::as_tibble(tbl)
    unique(resolve_policy_branch_filters(tbl))
  }
  if (is_s7_instance(learner, "PolicyLearner")) {
    # The meta-learner must score every benchmarked policy that could plausibly
    # produce a donor set for the anchor. Restricting prediction to the
    # deterministic `final_ref` subset can silently drop broad policy families
    # and leave some anchors with only zero-donor rows.
    if ((is.null(active_policies) || length(active_policies) == 0) &&
      length(object@benchmark) > 0) {
      active_policies <- infer_policy_bases((object@benchmark)$policy_perf %||% tibble::tibble())
      if (is.null(policy_params$slope_class) &&
        nrow((object@benchmark)$policy_perf %||% tibble::tibble()) > 0) {
        policy_params$slope_class <- infer_equation_branch_filters(
          (object@benchmark)$policy_perf %||% tibble::tibble()
        )
      }
    }
    if (is.null(active_policies) || length(active_policies) == 0) {
      active_policies <- infer_policy_bases(select_tbl)
      if (is.null(policy_params$slope_class) && nrow(select_tbl) > 0) {
        policy_params$slope_class <- infer_equation_branch_filters(select_tbl)
      }
    }
  } else {
    if (is.null(active_policies) || length(active_policies) == 0) {
      active_policies <- infer_policy_bases(select_tbl)
      if (is.null(policy_params$slope_class) && nrow(select_tbl) > 0) {
        policy_params$slope_class <- infer_equation_branch_filters(select_tbl)
      }
    }
    if ((is.null(active_policies) || length(active_policies) == 0) &&
      length(object@benchmark) > 0) {
      active_policies <- infer_policy_bases((object@benchmark)$policy_perf %||% tibble::tibble())
      if (is.null(policy_params$slope_class) &&
        nrow((object@benchmark)$policy_perf %||% tibble::tibble()) > 0) {
        policy_params$slope_class <- infer_equation_branch_filters(
          (object@benchmark)$policy_perf %||% tibble::tibble()
        )
      }
    }
  }
  active_policies <- active_policies[!is.na(active_policies) & nzchar(active_policies)]
  if (length(active_policies) == 0) {
    stop("No active policies were supplied for prediction.", call. = FALSE)
  }
  if (!is.null(learner) && !is_s7_instance(learner, "PolicyLearner")) {
    stop("'learner' must be NULL or a `PolicyLearner` object.", call. = FALSE)
  }
  requested_anchor_ids <- if ("model_id" %in% names(anchors_tbl)) {
    sort(unique(as.character(anchors_tbl$model_id)))
  } else {
    character(0)
  }
  prediction_cache_path <- policy_selector_prediction_cache_path(
    cache_path,
    learner = learner,
    anchor_ids = requested_anchor_ids,
    admissibility_tag = policy_selector_admissibility_fingerprint(resolved_admissibility_overrides)
  )
  if (!is.null(prediction_cache_path) && tsb_cache_exists(prediction_cache_path) && !isTRUE(refresh)) {
    report_progress(progress, "Loading cached policy predictions from ", prediction_cache_path, ".")
    cached_predictions <- tsb_cache_read(prediction_cache_path)
    if (!is_s7_instance(cached_predictions, "PolicyPredictions")) {
      stop(
        "Cached policy predictions are not a `PolicyPredictions` object; rerun with `refresh = TRUE`.",
        call. = FALSE
      )
    }
    # The cache path already encodes the requested anchor set, but guard
    # against stale files written before that behavior existed (or a rare
    # fingerprint collision) by also checking the cached content directly.
    cached_anchor_ids <- if ("anchor_model_id" %in% names(cached_predictions@selections)) {
      sort(unique(as.character(cached_predictions@selections$anchor_model_id)))
    } else {
      character(0)
    }
    if (length(requested_anchor_ids) > 0L && identical(requested_anchor_ids, cached_anchor_ids)) {
      return(cached_predictions)
    }
    report_progress(
      progress,
      "Cached policy predictions at ", prediction_cache_path,
      " were built for a different anchor set; recomputing instead of reusing them."
    )
  }
  learner_random_intercepts <- if (is_s7_instance(learner, "PolicyLearner")) {
    policy_learner_selection_random_intercepts(learner@config)
  } else {
    character(0)
  }
  # `.split_group` is synthesized from the cross-fitting group and exists only
  # during learner training. New anchors intentionally receive the LMER
  # population-level prediction, so they do not need this internal column.
  anchor_random_intercepts <- setdiff(learner_random_intercepts, ".split_group")
  missing_random_intercepts <- setdiff(anchor_random_intercepts, names(anchors_tbl))
  if (length(missing_random_intercepts) > 0L) {
    stop(
      sprintf(
        "Configured LMER random-intercept column(s) are absent from reference anchors: %s",
        paste(missing_random_intercepts, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Build one augmented distance basis when query anchors are not already in the
  # candidate pool. The selected distance family is fixed by the stored object:
  # learned Alchemist distances stay learned; empirical Gower stays empirical.
  prediction_candidates <- object@candidates
  prediction_excluded_model_ids <- character(0)
  external_anchor_ids <- character(0)
  id_col <- if ("model_id" %in% names(anchors_tbl) &&
    "model_id" %in% names(object@candidates@candidate_models)) {
    "model_id"
  } else {
    NULL
  }
  if ("model_id" %in% names(object@candidates@reference_anchors)) {
    prediction_excluded_model_ids <- c(
      prediction_excluded_model_ids,
      as.character(object@candidates@reference_anchors$model_id)
    )
  } else if ("model_id" %in% names(object@candidates@reference_anchors)) {
    prediction_excluded_model_ids <- c(
      prediction_excluded_model_ids,
      as.character(object@candidates@reference_anchors$model_id)
    )
  }
  if (is.null(id_col)) {
    stop("Query-anchor prediction requires `model_id` in both anchors and candidate models.", call. = FALSE)
  }
  anchor_ids_now <- as.character(anchors_tbl[[id_col]])
  if (anyNA(anchor_ids_now) || any(!nzchar(anchor_ids_now))) {
    stop("Query-anchor prediction requires non-missing, non-empty `model_id` values.", call. = FALSE)
  }
  if (anyDuplicated(anchor_ids_now)) {
    stop("Query-anchor prediction requires unique `model_id` values.", call. = FALSE)
  }
  candidate_ids_now <- unique(as.character(object@candidates@candidate_models[[id_col]]))
  external_anchor_rows <- anchors_tbl |>
    dplyr::filter(!(as.character(.data[[id_col]]) %in% candidate_ids_now))
  if (nrow(external_anchor_rows) > 0) {
    external_anchor_ids <- unique(as.character(external_anchor_rows[[id_col]]))
    prediction_excluded_model_ids <- c(
      prediction_excluded_model_ids,
      external_anchor_ids
    )
    augmented_candidates <- Candidates(
      spec = object@candidates@spec,
      study_db = object@candidates@study_db,
      species_vector = unique(c(
        object@candidates@species_vector,
        sort(unique(stats::na.omit(as.character(external_anchor_rows$species_name %||% character(0)))))
      )),
      source_dbs = object@candidates@source_dbs,
      species_db = object@candidates@species_db,
      candidate_models = dplyr::bind_rows(
        tibble::as_tibble(object@candidates@candidate_models),
        tibble::as_tibble(external_anchor_rows)
      ),
      reference_anchors = object@candidates@reference_anchors,
      similarity_matrix = list(),
      gower_distances = list(),
      ordination = object@candidates@ordination,
      admissibility = list(),
      similarity_tuning = object@candidates@similarity_tuning
    )
    distance_state <- object@candidates@gower_distances
    distance_mode <- distance_state$distance_mode %||% NULL
    if (is.null(distance_mode)) {
      stop(
        "External query-anchor prediction requires a stored distance mode. Rebuild the selector after preparing empirical Gower or forging Alchemist distances.",
        call. = FALSE
      )
    }
    if (identical(distance_mode, "alchemist_super_learner")) {
      learned_distances <- augment_alchemist_query_distances(
        candidate_models = augmented_candidates@candidate_models,
        distance_state = distance_state,
        query_model_ids = external_anchor_rows[[id_col]]
      )
      prediction_candidates <- candidates_with_gower_distances(
        augmented_candidates,
        learned_distances
      )
    } else if (identical(distance_mode, "empirical_gower")) {
      prepared_similarity <- object@candidates@similarity_matrix
      required_similarity <- c(
        "species_weights", "study_weights", "alpha", "k_species", "k_study"
      )
      missing_similarity <- required_similarity[vapply(
        required_similarity,
        function(name) is.null(prepared_similarity[[name]]),
        logical(1)
      )]
      if (length(missing_similarity) > 0L) {
        stop(
          sprintf(
            "External empirical-Gower prediction requires stored prepared similarity state: %s",
            paste(missing_similarity, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      sim_prediction <- prepare_similarities(
        candidate_models = augmented_candidates,
        species_traits = as.list(prepared_similarity$species_weights),
        study_traits = as.list(prepared_similarity$study_weights),
        alpha = prepared_similarity$alpha,
        k_species = prepared_similarity$k_species,
        k_study = prepared_similarity$k_study,
        config = anchor_cfg,
        registry_path = registry_path,
        seed = anchor_cfg$seed %||% NULL
      )
      prediction_candidates <- candidates_with_similarity_matrix(
        augmented_candidates,
        sim_prediction
      )
      prediction_candidates <- candidates_with_gower_distances(
        prediction_candidates,
        construct_gower_distances(sim_prediction)
      )
    } else {
      stop(
        sprintf("Unsupported stored distance mode for external query anchors: %s", distance_mode),
        call. = FALSE
      )
    }
  }
  prediction_excluded_model_ids <- unique(
    prediction_excluded_model_ids[
      !is.na(prediction_excluded_model_ids) &
        nzchar(prediction_excluded_model_ids)
    ]
  )

  # Resolve all reusable reference tables and scalar thresholds once so the
  # per-anchor loop only performs the work that actually depends on the anchor.
  conf_tbl <- normalize_policy_columns(conf_tbl)
  conf_join <- intersect(c("policy", "equation_branch_filter", "q_abs_log", "n", "median_abs_log"), names(conf_tbl))
  conf_ref_tbl <- if (all(c("policy", "equation_branch_filter", "q_abs_log") %in% conf_join)) {
    conf_tbl |>
      dplyr::select(dplyr::all_of(conf_join))
  } else {
    tibble::tibble()
  }
  select_cols <- intersect(
    c(
      "policy",
      "equation_branch_filter",
      "mean_species_median_abs_log",
      "acceptable_one_se",
      "acceptable_bootstrap",
      "acceptable_global",
      "equivalent_to_best_global",
      "paired_mean_diff_to_best",
      "best_mean_species_median_abs_log",
      "one_se_threshold",
      "bootstrap_prob_within_threshold",
      "bootstrap_prob_best",
      "bootstrap_median_rank",
      "coefficient_slope_q95",
      "coefficient_intercept_q95",
      "coefficient_stability_n",
      "specificity_rank",
      "equivalence_class_id",
      "equivalence_class_size",
      "equivalence_class_members"
    ),
    names(select_tbl)
  )
  select_tbl <- normalize_policy_columns(select_tbl)
  select_ref_tbl <- if (all(c("policy", "equation_branch_filter") %in% select_cols)) {
    select_tbl |>
      dplyr::select(dplyr::all_of(select_cols))
  } else {
    tibble::tibble()
  }
  learner_meta_ref_tbl <- if (is_s7_instance(learner, "PolicyLearner")) {
    benchmark_species_perf <- tryCatch(
      policy_learner_species_perf(learner),
      error = function(e) tibble::tibble()
    )
    benchmark_species_perf <- tryCatch(
      augment_species_block_meta_features(benchmark_species_perf),
      error = function(e) tibble::as_tibble(benchmark_species_perf)
    )
    meta_cols <- intersect(
      c(
        "policy",
        "equation_branch_filter",
        "anchor_species",
        "n_valid_folds",
        "prop_valid_folds",
        "policy_loco_mean_abs_log",
        "policy_loco_sd_abs_log",
        "policy_loco_q75_abs_log",
        "policy_loco_q90_abs_log"
      ),
      names(benchmark_species_perf)
    )
    if (all(c("policy", "equation_branch_filter", "anchor_species") %in% meta_cols)) {
      benchmark_species_perf |>
        normalize_policy_columns() |>
        dplyr::select(dplyr::all_of(meta_cols)) |>
        dplyr::distinct()
    } else {
      tibble::tibble()
    }
  } else {
    tibble::tibble()
  }
  coefficient_ref_tbl <- policy_coefficient_stability_summary(
    species_performance_table = benchmark_policy_tbl,
    candidate_models = object@candidates@candidate_models %||% tibble::tibble(),
    level = 0.95
  )
  structural_uncertainty_weight <- policy_selector_config_value(
    cfg, "structural_uncertainty_weight",
    sections = c("policy", "selection")
  ) %||% 1
  uncertainty_rule <- normalize_uncertainty_rule(policy_selector_config_value(
    cfg, "uncertainty_rule",
    sections = c("selection", "policy")
  ) %||% "tolerance")
  u_tol_rel <- policy_selector_config_value(
    cfg, "u_tol_rel",
    sections = c("selection", "policy")
  ) %||% policy_selector_config_value(
    cfg, "uncertainty_relative_tolerance",
    sections = c("selection", "policy")
  ) %||% 0.25
  u_tol_abs <- policy_selector_config_value(
    cfg, "u_tol_abs",
    sections = c("selection", "policy")
  ) %||% policy_selector_config_value(
    cfg, "uncertainty_absolute_tolerance",
    sections = c("selection", "policy")
  ) %||% 0.05
  one_se_multiplier <- policy_selector_config_value(
    cfg, "one_se_multiplier",
    sections = c("selection", "policy")
  ) %||% 1
  local_distance_tolerance <- policy_selector_config_value(
    cfg, "local_distance_tolerance",
    sections = c("selection", "policy")
  ) %||% 1e-12
  # A selected policy keeps its own calibrated interval
  apply_selected_interval_columns <- function(selected_row) selected_row

  all_policy_intervals <- list()
  selected_policy_rows <- list()
  consensus_multiplier_rows <- list()

  n_anchors <- nrow(anchors_tbl)
  report_progress(
    progress,
    "[Predict] Scoring ", n_anchors, " anchor",
    if (n_anchors != 1L) "s" else "", " across ", length(active_policies), " policies..."
  )

  # Re-evaluate or reuse each anchor-specific admissibility object, then apply
  # the active policy set and join the stored uncertainty and selection
  # summaries onto those anchor-policy predictions.
  for (i in seq_len(n_anchors)) {
    anchor_row <- anchors_tbl[i, , drop = FALSE]
    admissibility_override_i <- resolved_admissibility_overrides[[i]]
    anchor_cfg_i <- merge_config_sections(anchor_cfg, admissibility_override_i$values)
    reuse_admissibility_i <- isTRUE(reuse_admissibility) && !admissibility_override_i$applied
    anchor_id <- if ("model_id" %in% names(anchor_row)) {
      as.character(anchor_row$model_id[[1]])
    } else {
      as.character(anchor_row$model_id[[1]])
    }
    anchor_species <- as.character(anchor_row$species_name[[1]])
    anchor_is_external <- anchor_id %in% external_anchor_ids
    report_progress(
      progress,
      "[Predict]   [", i, "/", n_anchors, "] ", anchor_species, " (", anchor_id, ")"
    )

    cached_failure <- if (reuse_admissibility_i) {
      policy_selector_cached_anchor_failure(object, anchor_row)
    } else {
      tibble::tibble()
    }
    eval_obj <- if (nrow(cached_failure) > 0L) {
      structure(
        as.character(cached_failure$failure_message[[1]]),
        class = "try-error"
      )
    } else if (reuse_admissibility_i) {
      policy_selector_cached_anchor_evaluation(
        object = object,
        anchor_row = anchor_row,
        config_supplied = !is.null(config)
      )
    } else {
      NULL
    }
    if (is.null(eval_obj)) {
      eval_obj <- try(screen_one_anchor_admissibility(
        anchor_row = anchor_row,
        candidate_models = prediction_candidates,
        config = anchor_cfg_i,
        registry_path = registry_path,
        excluded_model_ids = prediction_excluded_model_ids,
        require_backscatter = FALSE
      ), silent = TRUE)
    }
    if (inherits(eval_obj, "try-error")) {
      # Record an invalid anchor result instead of aborting the whole
      # prediction pass when one held-out anchor cannot be screened.
      selected_policy_rows[[length(selected_policy_rows) + 1]] <- tibble::tibble(
        anchor_model_id = anchor_id,
        anchor_species = anchor_species,
        anchor_is_external = anchor_is_external,
        selected_policy = NA_character_,
        selected_policy_display = NA_character_,
        equation_branch_filter = NA_character_,
        multiplier_pred = NA_real_,
        multiplier_lo = NA_real_,
        multiplier_hi = NA_real_,
        valid_prediction = FALSE,
        prediction_error_stage = if (nrow(cached_failure) > 0L) {
          as.character(cached_failure$failure_stage[[1]])
        } else {
          NA_character_
        },
        prediction_error_code = if (nrow(cached_failure) > 0L) {
          as.character(cached_failure$failure_code[[1]])
        } else {
          "anchor_screening_error"
        },
        selection_tier = if (is_s7_instance(learner, "PolicyLearner")) {
          "meta_policy_min_predicted_score"
        } else {
          "deterministic_global_screen"
        },
        prediction_error_message = as.character(eval_obj)[[1]]
      ) |>
        dplyr::mutate(
          admissibility_profile = admissibility_override_i$profile,
          admissibility_override_applied = admissibility_override_i$applied
        )
      consensus_multiplier_rows[[length(consensus_multiplier_rows) + 1]] <- tibble::tibble(
        anchor_model_id = anchor_id,
        anchor_species = anchor_species,
        anchor_is_external = anchor_is_external,
        admissibility_profile = admissibility_override_i$profile,
        admissibility_override_applied = admissibility_override_i$applied
      )
      report_progress(
        progress,
        "[Predict]   Skipped [", i, "/", n_anchors, "] ", anchor_species, " (", anchor_id, "): ",
        as.character(eval_obj)[[1]]
      )
      next
    }

    ordination_info <- if (is.null(model_scores) || is.null(species_lookup)) {
      NULL
    } else {
      build_anchor_ordination(
        anchor_row = anchor_row,
        model_scores = model_scores,
        species_lookup = species_lookup
      )
    }

    policy_tbl <- evaluate_policies(
      eval_obj = eval_obj,
      ordination_info = ordination_info,
      policies = active_policies,
      policy_params = policy_params,
      policy_path = policy_path
    ) |>
      normalize_policy_columns()
    policy_tbl$admissibility_profile <- admissibility_override_i$profile
    policy_tbl$admissibility_override_applied <- admissibility_override_i$applied
    # Keep compact/mock policy tables usable: `dplyr::if_else()` evaluates
    # both branches, so a missing coefficient column cannot appear inside it.
    # Record the original schema; the following base `if` avoids touching
    # absent columns without adding artificial placeholder columns.
    has_policy_coefficients <- all(c(
      "policy_slope_len", "policy_intercept_len"
    ) %in% names(policy_tbl))
    policy_tbl <- policy_tbl |>
      dplyr::mutate(
        anchor_model_id = anchor_id,
        anchor_species = anchor_species,
        anchor_is_external = anchor_is_external,
        # External anchors have no anchor_sigma, so a finite coefficient counts as valid instead.
        valid_prediction = if (has_policy_coefficients) {
          dplyr::if_else(
            dplyr::coalesce(anchor_is_external, FALSE),
            is.finite(.data$policy_slope_len) & is.finite(.data$policy_intercept_len),
            is.finite(.data$multiplier_pred) & .data$multiplier_pred > 0
          )
        } else {
          is.finite(.data$multiplier_pred) & .data$multiplier_pred > 0
        }
      )
    for (random_intercept in anchor_random_intercepts) {
      policy_tbl[[random_intercept]] <- rep(
        as.character(anchor_row[[random_intercept]][[1]]),
        nrow(policy_tbl)
      )
    }

    # A PolicyLearner supplies the selected policy's conditional calibrated
    # radius. Do not attach the legacy pooled policy/branch radius first: it
    # would otherwise take precedence over the learner's calibrated value.
    if (!is_s7_instance(learner, "PolicyLearner") && nrow(conf_ref_tbl) > 0) {
      policy_tbl <- policy_tbl |>
        dplyr::left_join(conf_ref_tbl, by = c("policy", "equation_branch_filter"))
    } else {
      policy_tbl$q_abs_log <- NA_real_
    }

    policy_tbl <- add_policy_intervals(
      policy_tbl = policy_tbl,
      structural_uncertainty_weight = structural_uncertainty_weight
    )

    if (nrow(select_ref_tbl) > 0) {
      policy_tbl <- policy_tbl |>
        dplyr::left_join(select_ref_tbl, by = c("policy", "equation_branch_filter"))
    }
    if (nrow(learner_meta_ref_tbl) > 0) {
      # Reattach the same species-specific policy-fragility summaries used to
      # train the meta-learner so anchor-time scoring does not fall back to
      # median-imputed defaults for fold validity and leave-current-species-out
      # policy error history.
      policy_tbl <- policy_tbl |>
        dplyr::left_join(
          learner_meta_ref_tbl,
          by = c("policy", "equation_branch_filter", "anchor_species")
        )
    }
    if (nrow(coefficient_ref_tbl) > 0) {
      policy_tbl <- policy_tbl |>
        dplyr::left_join(
          coefficient_ref_tbl,
          by = c("policy", "equation_branch_filter"),
          suffix = c("", ".coefref")
        )
      if (!"coefficient_slope_q95.coefref" %in% names(policy_tbl)) {
        policy_tbl$coefficient_slope_q95.coefref <- NA_real_
      }
      if (!"coefficient_intercept_q95.coefref" %in% names(policy_tbl)) {
        policy_tbl$coefficient_intercept_q95.coefref <- NA_real_
      }
      if (!"coefficient_stability_n.coefref" %in% names(policy_tbl)) {
        policy_tbl$coefficient_stability_n.coefref <- NA_real_
      }
      policy_tbl <- policy_tbl |>
        dplyr::mutate(
          coefficient_slope_q95 = dplyr::coalesce(.data$coefficient_slope_q95, .data$coefficient_slope_q95.coefref),
          coefficient_intercept_q95 = dplyr::coalesce(.data$coefficient_intercept_q95, .data$coefficient_intercept_q95.coefref),
          coefficient_stability_n = dplyr::coalesce(.data$coefficient_stability_n, .data$coefficient_stability_n.coefref)
        ) |>
        dplyr::select(-dplyr::any_of(c(
          "coefficient_slope_q95.coefref",
          "coefficient_intercept_q95.coefref",
          "coefficient_stability_n.coefref"
        )))
    }
    # Do not collapse donor-equivalent policy rows before selection. Policy
    # labels can carry distinct benchmark and branch-calibration records even
    # when they happen to resolve to the same donor for this anchor. Replacing
    # one label with a more-specific label before scoring changes its learned
    # score and can remove a policy that is inside the anchor score band.
    # Keep every configured policy/branch available to the score-band and
    # burden rule; the selected row is the authoritative representation.

    # Keep the deterministic globally screened policy row
    if (is_s7_instance(learner, "PolicyLearner")) {
      policy_tbl <- stats::predict(
        learner,
        policy_tbl,
        use_support_bin_intervals = use_support_bin_intervals,
        max_selection_tolerance = max_selection_tolerance
      )
      selected_row <- select_anchor_policies(
        policy_tbl = policy_tbl,
        uncertainty_rule = uncertainty_rule,
        u_tol_rel = u_tol_rel,
        u_tol_abs = u_tol_abs,
        one_se_multiplier = one_se_multiplier,
        local_distance_tolerance = local_distance_tolerance
      ) |>
        dplyr::mutate(
          selected_policy_display = dplyr::coalesce(.data$selected_policy_display, .data$policy_display, .data$selected_policy),
          selection_tier = dplyr::coalesce(.data$selection_tier, "meta_policy_selection"),
          post_selection_calibration_status = "stored_policy_calibration"
        )
      if (".policy_row_id" %in% names(policy_tbl) && ".policy_row_id" %in% names(selected_row)) {
        selected_ids <- selected_row$.policy_row_id
        policy_tbl$is_selected <- policy_tbl$.policy_row_id %in% selected_ids
      } else {
        selected_keys <- selected_row |>
          dplyr::transmute(
            .selected_policy_key = as.character(.data$policy),
            .selected_branch_key = as.character(.data$equation_branch_filter)
          ) |>
          dplyr::distinct()
        policy_tbl <- policy_tbl |>
          dplyr::mutate(
            .selected_policy_key = as.character(.data$policy),
            .selected_branch_key = as.character(.data$equation_branch_filter)
          ) |>
          dplyr::left_join(
            selected_keys |>
              dplyr::mutate(is_selected_key = TRUE),
            by = c(".selected_policy_key", ".selected_branch_key")
          ) |>
          dplyr::mutate(is_selected = dplyr::coalesce(.data$is_selected_key, FALSE)) |>
          dplyr::select(-dplyr::any_of(c(
            ".selected_policy_key",
            ".selected_branch_key",
            "is_selected_key"
          )))
      }
    } else {
      if (!"policy_display" %in% names(policy_tbl)) {
        policy_tbl$policy_display <- policy_tbl$policy
      }
      selected_row <- select_anchor_policies(
        policy_tbl = policy_tbl,
        uncertainty_rule = uncertainty_rule,
        u_tol_rel = u_tol_rel,
        u_tol_abs = u_tol_abs,
        one_se_multiplier = one_se_multiplier,
        local_distance_tolerance = local_distance_tolerance
      ) |>
        dplyr::mutate(
          selected_policy_display = dplyr::coalesce(.data$selected_policy_display, .data$policy_display, .data$selected_policy),
          selection_tier = dplyr::coalesce(.data$selection_tier, "deterministic_global_screen")
        )
    }

    if (nrow(selected_row) == 0) {
      admissible_n <- nrow(tibble::as_tibble(eval_obj$admissible_df %||% tibble::tibble()))
      valid_policy_n <- sum(
        as.logical(policy_tbl$valid_prediction) %in% TRUE &
          is.finite(suppressWarnings(as.numeric(policy_tbl$multiplier_pred))) &
          suppressWarnings(as.numeric(policy_tbl$multiplier_pred)) > 0,
        na.rm = TRUE
      )
      prediction_error_code <- if (admissible_n == 0L) {
        "no_admissible_donors"
      } else if (valid_policy_n == 0L) {
        "no_valid_policy_prediction"
      } else {
        "no_selected_policy"
      }
      prediction_error_message <- switch(prediction_error_code,
        no_admissible_donors = "No donor satisfied all configured admissibility gates.",
        no_valid_policy_prediction = "The admissible donor pool produced no finite positive policy prediction.",
        no_selected_policy = "Valid policy predictions were available, but policy selection returned no row."
      )
      selected_row <- tibble::tibble(
        anchor_model_id = anchor_id,
        anchor_species = anchor_species,
        selected_policy = NA_character_,
        selected_policy_display = NA_character_,
        equation_branch_filter = NA_character_,
        multiplier_pred = NA_real_,
        multiplier_lo = NA_real_,
        multiplier_hi = NA_real_,
        valid_prediction = FALSE,
        prediction_error_stage = "policy_selection",
        prediction_error_code = prediction_error_code,
        prediction_error_message = prediction_error_message,
        selection_tier = if (is_s7_instance(learner, "PolicyLearner")) {
          "meta_policy_min_predicted_score"
        } else {
          "deterministic_global_screen"
        }
      )
    } else {
      selected_row <- apply_selected_interval_columns(selected_row)
    }
    selected_row$admissibility_profile <- rep(admissibility_override_i$profile, nrow(selected_row))
    selected_row$admissibility_override_applied <- rep(admissibility_override_i$applied, nrow(selected_row))

    consensus_row <- summarize_evaluation(eval_obj) |>
      dplyr::mutate(
        anchor_model_id = anchor_id,
        anchor_species = anchor_species,
        anchor_is_external = anchor_is_external,
        admissibility_profile = admissibility_override_i$profile,
        admissibility_override_applied = admissibility_override_i$applied,
        .before = 1
      )

    selected_row$anchor_is_external <- rep(anchor_is_external, nrow(selected_row))

    all_policy_intervals[[length(all_policy_intervals) + 1]] <- policy_tbl
    selected_policy_rows[[length(selected_policy_rows) + 1]] <- selected_row
    consensus_multiplier_rows[[length(consensus_multiplier_rows) + 1]] <- consensus_row
    report_progress(
      progress,
      "[Predict]   Completed [", i, "/", n_anchors, "] ", anchor_species, " (", anchor_id, ")"
    )
  }

  report_progress(progress, "[Predict] Completed predictions for ", n_anchors, " anchor", if (n_anchors != 1L) "s" else "", ".")

  predictions <- PolicyPredictions(
    intervals = dplyr::bind_rows(all_policy_intervals),
    selections = dplyr::bind_rows(selected_policy_rows),
    consensus = dplyr::bind_rows(consensus_multiplier_rows)
  )
  if (!is.null(prediction_cache_path)) {
    dir.create(dirname(prediction_cache_path), recursive = TRUE, showWarnings = FALSE)
    tsb_cache_write(predictions, prediction_cache_path)
    report_progress(progress, "Saved policy predictions cache to ", prediction_cache_path, ".")
  }
  predictions
}

#' Predict selected policy intervals
#'
#' @return A [PolicyPredictions] object.
#' @name predict.PolicySelector
#' @usage NULL
S7::method(predict_generic, PolicySelector) <- .predict_policy_selector

#' Print a `PolicySelector`
#'
#' @name print.PolicySelector
#' @usage NULL
#'
#' @param x A [PolicySelector] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @noRd
S7::method(print_generic, PolicySelector) <- function(x, ...) {
  benchmark_rows <- nrow(tibble::as_tibble((x@benchmark)$policy_perf %||% tibble::tibble()))
  uncertainty_rows <- nrow(tibble::as_tibble((x@uncertainty)$conf_cal %||% tibble::tibble()))
  selection_rows <- nrow(tibble::as_tibble((x@selection)$final_ref %||% tibble::tibble()))

  cat("PolicySelector\n")
  cat("  candidate_models: ", nrow(x@candidates), "\n", sep = "")
  cat("  anchors: ", nrow(x@candidates@reference_anchors), "\n", sep = "")
  cat("  benchmark_rows: ", benchmark_rows, "\n", sep = "")
  cat("  uncertainty_rows: ", uncertainty_rows, "\n", sep = "")
  cat("  selection_rows: ", selection_rows, "\n", sep = "")
  invisible(x)
}

#' Show a `PolicySelector`
#'
#' @name show.PolicySelector
#' @usage NULL
#'
#' @param object A [PolicySelector] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
#' @noRd
S7::method(show_generic, PolicySelector) <- function(object) {
  print(object)
  invisible(object)
}

#' Print a `PolicyPredictions`
#'
#' @name print.PolicyPredictions
#' @usage NULL
#'
#' @param x A [PolicyPredictions] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @noRd
S7::method(print_generic, PolicyPredictions) <- function(x, ...) {
  cat("PolicyPredictions\n")
  cat("  interval_rows: ", nrow(x@intervals), "\n", sep = "")
  cat("  selected_rows: ", nrow(x@selections), "\n", sep = "")
  cat("  consensus_rows: ", nrow(x@consensus), "\n", sep = "")
  invisible(x)
}

#' Show a `PolicyPredictions`
#'
#' @name show.PolicyPredictions
#' @usage NULL
#'
#' @param object A [PolicyPredictions] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
#' @noRd
S7::method(show_generic, PolicyPredictions) <- function(object) {
  print(object)
  invisible(object)
}

#' Coerce `PolicyPredictions` to a tibble
#'
#' Returns the canonical selected-policy result table.
#'
#' @param x A [PolicyPredictions] object.
#' @param ... Unused.
#'
#' @return Tibble of prediction selections.
#'
#' @examples
#' \dontrun{
#' predictions <- predict(selector)
#' tibble::as_tibble(predictions)
#' }
#'
#' @keywords internal
#' @noRd
as_tibble_policy_predictions <- function(x, ...) {
  tibble::as_tibble(x@selections)
}

#' Extract configured PolicySelector reference species
#'
#' @param x A [PolicySelector] object.
#'
#' @return Character vector of configured reference-anchor species names.
#' @keywords internal
#' @noRd
policy_selector_reference_anchor_species <- function(x) {
  if (!is_s7_instance(x, "PolicySelector")) {
    return(character(0))
  }
  anchors <- tibble::as_tibble(x@candidates@reference_anchors %||% tibble::tibble())
  species <- if ("species_name" %in% names(anchors)) {
    as.character(anchors$species_name)
  } else if ("anchor_species" %in% names(anchors)) {
    as.character(anchors$anchor_species)
  } else {
    character(0)
  }
  unique(species[!is.na(species) & nzchar(species)])
}

#' Resolve compact policy displays for PolicySelector plots
#'
#' @param x A [PolicySelector] object.
#' @param anchor_species Optional species names used to restrict the
#'   species-block benchmark before ranking policies.
#' @param max_policies Maximum number of displayed policies.
#'
#' @return Character vector of policy display labels without branch suffixes.
#' @keywords internal
#' @noRd
policy_selector_reference_policy_display_levels <- function(x,
                                                            anchor_species = NULL,
                                                            max_policies = 30L) {
  if (!is_s7_instance(x, "PolicySelector") || length(x@benchmark) == 0L) {
    return(character(0))
  }
  perf <- tibble::as_tibble((x@benchmark)$species_block_perf %||% tibble::tibble())
  if (nrow(perf) == 0L || !all(c("valid_prediction", "error_abs_log") %in% names(perf))) {
    return(character(0))
  }
  perf <- filter_plot_anchor_species(perf, anchor_species = anchor_species)
  perf$policy_display <- stringr::str_remove(
    resolve_policy_display_names(perf),
    "\\s*\\[[^]]+\\]$"
  )
  out <- perf |>
    dplyr::filter(
      .data$valid_prediction,
      is.finite(.data$error_abs_log),
      !is.na(.data$policy_display),
      nzchar(.data$policy_display)
    ) |>
    dplyr::group_by(.data$policy_display) |>
    dplyr::summarise(median_abs_log_error = stats::median(.data$error_abs_log, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$median_abs_log_error, .data$policy_display) |>
    dplyr::pull(.data$policy_display)
  max_policies <- normalize_plot_policy_limit(max_policies, default = 30L)
  if (is.finite(max_policies)) {
    out <- head(out, max_policies)
  }
  unique(out)
}

#' Current PolicySelector plot type names
#'
#' @return Character vector of supported plot type names.
#' @keywords internal
#' @noRd
policy_selector_plot_types <- function() {
  c("strategy_error_heatmap", "conformal_scores", "policy_benchmark")
}

#' Plot a `PolicySelector`
#'
#' Uses the package's S7 method on [base::plot()] so selector-stage
#' summaries can be drawn directly from the package object.
#'
#' @name plot.PolicySelector
#'
#' @param x A [PolicySelector] object.
#' @param y Unused.
#' @param type Figure family to draw.
#' @param anchor_species Optional anchor species to plot. When omitted, the
#'   selector's configured reference-anchor species are used when available.
#'   Use `anchor_species = NULL` explicitly to keep all species.
#' @param max_policies Maximum number of policies shown in dense benchmark and
#'   uncertainty plots.
#' @param show_values Optional logical scalar controlling cell labels in
#'   heatmaps. `NULL` lets the plotting helper decide from grid size.
#' @param ... Additional plotting arguments.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' selector <- benchmark(as_policyselector(candidates))
#' plot(selector, type = "policy_benchmark")
#' plot(selector, type = "strategy_error_heatmap")
#' }
#' @usage
#' \method{plot}{PolicySelector}(
#'   x,
#'   y = NULL,
#'   type = c(
#'     "strategy_error_heatmap",
#'     "conformal_scores",
#'     "policy_benchmark"
#'   ),
#'   anchor_species,
#'   max_policies = 30L,
#'   show_values = NULL,
#'   ...
#' )
NULL

.plot_policy_selector <- function(x,
                                  y = NULL,
                                  type = c(
                                    "strategy_error_heatmap",
                                    "conformal_scores",
                                    "policy_benchmark"
                                  ),
                                  anchor_species,
                                  max_policies = 30L,
                                  show_values = NULL,
                                  ...) {
  valid_types <- policy_selector_plot_types()
  type <- as.character(type %||% valid_types[[1]])[[1]]
  if (!type %in% valid_types) {
    return(plot_report_placeholder(
      title = "PolicySelector Plot Unavailable",
      subtitle = sprintf(
        "Plot type '%s' is not a current PolicySelector plot type.",
        type
      )
    ))
  }
  anchor_species <- if (missing(anchor_species)) {
    policy_selector_reference_anchor_species(x)
  } else {
    anchor_species
  }
  dots <- list(...)
  view <- as.character(dots$view %||% "policy")[[1]]

  if (type %in% c("strategy_error_heatmap", "policy_benchmark")) {
    if (length(x@benchmark) == 0) {
      return(plot_report_placeholder(
        title = "Policy Benchmark",
        subtitle = "No benchmark results are stored on this PolicySelector."
      ))
    }
    if (identical(type, "policy_benchmark")) {
      return(plot_policy_boxplot(
        (x@benchmark)$policy_perf %||% tibble::tibble(),
        anchor_species = anchor_species,
        max_policies = max_policies
      ))
    }
    if (identical(view, "components") || identical(view, "component")) {
      return(plot_policy_component_heatmap(
        (x@benchmark)$policy_perf %||% tibble::tibble(),
        anchor_species = anchor_species,
        show_values = show_values
      ))
    }
    return(plot_policy_heatmap(
      (x@benchmark)$species_block_perf %||% tibble::tibble(),
      anchor_species = anchor_species,
      max_policies = max_policies,
      show_values = show_values
    ))
  }

  if (length(x@uncertainty) == 0) {
    return(plot_report_placeholder(
      title = "Conformal Calibration Radius by Policy",
      subtitle = "No uncertainty results are stored on this PolicySelector."
    ))
  }
  policy_filter <- policy_selector_reference_policy_display_levels(
    x,
    anchor_species = anchor_species,
    max_policies = max_policies
  )
  plot_conformal_scores(
    (x@uncertainty)$conf_cal %||% tibble::tibble(),
    policy_filter = policy_filter,
    max_policies = max_policies,
    show_values = show_values
  )
}

#' Register the `PolicySelector` plot method
#'
#' @name plot.PolicySelector
#' @usage NULL
#'
#' @keywords internal
#' @noRd
plot.PolicySelector <- .plot_policy_selector
S7::method(plot_generic, PolicySelector) <- .plot_policy_selector

#' Plot a `PolicyPredictions`
#'
#' Uses the package's S7 method on [base::plot()] so prediction bundles can be
#' inspected directly without extracting the stored interval tables by hand.
#'
#' @name plot.PolicyPredictions
#'
#' @param x A [PolicyPredictions] object.
#' @param y Unused.
#' @param type Figure family to draw.
#' @param anchor_species Optional anchor species used to restrict
#'   `type = "strategy_competition"` to one reference.
#' @param reference_name Optional display label used when plotting one anchor's
#'   policy competition panel.
#' @param ... Unused additional arguments.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' predictions <- predict(selector)
#' plot(predictions, type = "selected_intervals")
#' plot(predictions, type = "strategy_competition")
#' }
#' @usage
#' \method{plot}{PolicyPredictions}(
#'   x,
#'   y = NULL,
#'   type = c("selected_intervals", "strategy_competition"),
#'   anchor_species = NULL,
#'   reference_name = NULL,
#'   ...
#' )
NULL

#' Current PolicyPredictions plot type names
#'
#' @return Character vector of supported plot type names.
#' @keywords internal
#' @noRd
policy_predictions_plot_types <- function() {
  c("selected_intervals", "strategy_competition")
}

.plot_policy_predictions <- function(x,
                                     y = NULL,
                                     type = c("selected_intervals", "strategy_competition"),
                                     anchor_species = NULL,
                                     reference_name = NULL,
                                     ...) {
  valid_types <- policy_predictions_plot_types()
  type <- as.character(type %||% valid_types[[1]])[[1]]
  if (!type %in% valid_types) {
    return(plot_report_placeholder(
      title = "PolicyPredictions Plot Unavailable",
      subtitle = sprintf(
        "Plot type '%s' is not a current PolicyPredictions plot type.",
        type
      )
    ))
  }

  if (identical(type, "selected_intervals")) {
    return(plot_selected_intervals(x@selections))
  }

  interval_tbl <- tibble::as_tibble(x@intervals)
  if (!is.null(anchor_species)) {
    interval_tbl <- interval_tbl |>
      dplyr::filter(.data$anchor_species == as.character(anchor_species[[1]]))
    return(plot_all_intervals(
      interval_tbl = interval_tbl,
      reference_name = reference_name %||% as.character(anchor_species[[1]])
    ))
  }
  plot_interval_panel(interval_tbl)
}

#' Register the `PolicyPredictions` plot method
#'
#' @name plot.PolicyPredictions
#' @usage NULL
#'
#' @keywords internal
#' @noRd
plot.PolicyPredictions <- .plot_policy_predictions
S7::method(plot_generic, PolicyPredictions) <- .plot_policy_predictions


#' Rank policy candidate-pool breadth
#'
#' @param policy Policy identifiers.
#' @param candidate_pool Optional candidate-pool labels.
#'
#' @return Integer vector.
#'
#' @keywords internal
#' @noRd
policy_pool_rank <- function(policy, candidate_pool = NULL) {
  policy_ <- stringr::str_to_lower(stringr::str_squish(as.character(policy)))
  candidate_pool_ <- stringr::str_to_lower(stringr::str_squish(as.character(candidate_pool %||% rep(NA_character_, length(policy_)))))

  policy_pool_rank <- dplyr::case_when(
    stringr::str_detect(policy_, "same_species|within_species|study_cell_species") ~ 1L,
    stringr::str_detect(policy_, "same_genus|within_genus|study_cell_genus|genus_fao|genus_") ~ 2L,
    stringr::str_detect(policy_, "same_family|within_family|study_cell_family|family_ocean_basin|family_") ~ 3L,
    stringr::str_detect(policy_, "same_order|within_order") ~ 4L,
    stringr::str_detect(policy_, "study_cell|within_study_cell|closest_study_cell") ~ 5L,
    stringr::str_detect(policy_, "same_fao|within_fao|_fao$") ~ 6L,
    stringr::str_detect(policy_, "same_ocean_basin|within_ocean_basin|ocean_basin") ~ 7L,
    stringr::str_detect(policy_, "all_admissible|across_all_admissible") ~ 8L,
    TRUE ~ 9L
  )
  candidate_pool_rank <- dplyr::case_when(
    candidate_pool_ %in% c("same_species", "within_species") ~ 1L,
    candidate_pool_ %in% c("same_genus", "within_genus") ~ 2L,
    candidate_pool_ %in% c("same_family", "within_family") ~ 3L,
    candidate_pool_ %in% c("same_order", "within_order") ~ 4L,
    candidate_pool_ %in% c("same_study_cell", "within_study_cell", "closest_study_cell") ~ 5L,
    candidate_pool_ %in% c("same_fao_area", "within_fao", "within_fao_area") ~ 6L,
    candidate_pool_ %in% c("same_ocean_basin", "within_ocean_basin") ~ 7L,
    candidate_pool_ %in% c("all_admissible", "across_all_admissible") ~ 8L,
    TRUE ~ 9L
  )
  pool_rank <- pmin(policy_pool_rank, candidate_pool_rank, na.rm = TRUE)
  pool_rank[!is.finite(pool_rank)] <- 9L
  pool_rank
}

#' Rank policy aggregation breadth
#'
#' @param policy Policy identifiers.
#' @param aggregation_method Optional aggregation labels.
#'
#' @return Integer vector.
#'
#' @keywords internal
#' @noRd
policy_aggregation_rank <- function(policy, aggregation_method = NULL) {
  policy_ <- stringr::str_to_lower(stringr::str_squish(as.character(policy)))
  aggregation_method_ <- stringr::str_to_lower(stringr::str_squish(as.character(aggregation_method %||% rep(NA_character_, length(policy_)))))
  aggregation_method_ <- dplyr::coalesce(
    aggregation_method_,
    dplyr::case_when(
      stringr::str_detect(policy_, "closest|distance|nearest") ~ "nearest",
      stringr::str_detect(policy_, "weighted_mean") ~ "weighted_mean",
      stringr::str_detect(policy_, "unweighted_mean") ~ "unweighted_mean",
      stringr::str_detect(policy_, "random_baseline|random_draw") ~ "random_baseline",
      TRUE ~ NA_character_
    )
  )

  aggregation_rank <- dplyr::case_when(
    aggregation_method_ %in% c(
      "nearest",
      "nearest_by_combined_distance",
      "nearest_by_trait_gower_distance",
      "nearest_by_taxonomic_distance",
      "nearest_by_species_distance",
      "nearest_study_then_model"
    ) ~ 1L,
    aggregation_method_ %in% c(
      "kernel_weighted_mean",
      "distance_weighted_mean",
      "study_kernel_weighted_mean",
      "weighted_mean"
    ) ~ 2L,
    aggregation_method_ %in% c(
      "study_equal_weight_mean",
      "unweighted_mean"
    ) ~ 3L,
    aggregation_method_ %in% c("random_baseline", "random_draw") ~ 4L,
    TRUE ~ 3L
  )
}

#' Flag overly broad aggregate policies
#'
#' @param policy Policy identifiers.
#' @param candidate_pool Optional candidate-pool labels.
#' @param aggregation_method Optional aggregation labels.
#' @param broad_pool_min_rank Minimum pool-rank treated as broad.
#'
#' @return Logical vector.
#'
#' @keywords internal
#' @noRd
policy_is_broad_aggregate <- function(policy,
                                      candidate_pool = NULL,
                                      aggregation_method = NULL,
                                      broad_pool_min_rank = 6L) {
  pool_rank <- policy_pool_rank(
    policy = policy,
    candidate_pool = candidate_pool
  )
  aggregation_rank <- policy_aggregation_rank(
    policy = policy,
    aggregation_method = aggregation_method
  )
  is.finite(pool_rank) &
    is.finite(aggregation_rank) &
    pool_rank >= as.integer(broad_pool_min_rank[[1]] %||% 6L) &
    aggregation_rank %in% c(2L, 3L)
}

#' Flag broad policy pools regardless of aggregation method
#'
#' @param policy Policy identifiers.
#' @param candidate_pool Optional candidate-pool labels.
#' @param broad_pool_min_rank Minimum pool-rank treated as broad.
#'
#' @return Logical vector.
#'
#' @keywords internal
#' @noRd
policy_is_broad_proxy <- function(policy,
                                  candidate_pool = NULL,
                                  broad_pool_min_rank = 6L) {
  pool_rank <- policy_pool_rank(
    policy = policy,
    candidate_pool = candidate_pool
  )
  is.finite(pool_rank) &
    pool_rank >= as.integer(broad_pool_min_rank[[1]] %||% 6L)
}

policy_specificity_rank <- function(policy,
                                    candidate_pool = NULL,
                                    aggregation_method = NULL,
                                    policy_family = NULL,
                                    equation_branch_filter = NULL) {
  policy_ <- stringr::str_to_lower(stringr::str_squish(as.character(policy)))
  policy_family_ <- stringr::str_to_lower(stringr::str_squish(as.character(policy_family %||% rep(NA_character_, length(policy_)))))
  equation_branch_filter_ <- stringr::str_to_lower(stringr::str_squish(as.character(equation_branch_filter %||% rep(NA_character_, length(policy_)))))
  pool_rank <- policy_pool_rank(
    policy = policy_,
    candidate_pool = candidate_pool
  )
  aggregation_rank <- policy_aggregation_rank(
    policy = policy_,
    aggregation_method = aggregation_method
  )

  branch_penalty <- dplyr::case_when(
    equation_branch_filter_ %in% c("fixed20_only", "free_slope_only") ~ 0L,
    is.na(equation_branch_filter_) | equation_branch_filter_ == "all" ~ 4L,
    TRUE ~ 2L
  )
  family_penalty <- dplyr::case_when(
    policy_family_ %in% c("single_model", "study_hierarchical_single_model") ~ 0L,
    policy_family_ %in% c("baseline", "random_baseline") ~ 20L,
    TRUE ~ 5L
  )

  pool_rank * 100L + aggregation_rank * 10L + branch_penalty + family_penalty
}

#' Normalize the configured uncertainty-selection rule
#'
#' @param x Raw rule value.
#' @param default Fallback rule used when `x` is missing.
#'
#' @return One of `"min"`, `"tolerance"`, or `"one_se"`.
#'
#' @keywords internal
#' @noRd
normalize_uncertainty_rule <- function(x, default = "tolerance") {
  # Normalize the rule labels to the supported runtime values.
  rule <- stringr::str_to_lower(stringr::str_squish(as.character(x %||% default)[[1]] %||% default))
  if (!nzchar(rule) || is.na(rule)) {
    rule <- default
  }
  if (identical(rule, "minimum_width")) {
    rule <- "min"
  }
  if (identical(rule, "within_width_tolerance")) {
    rule <- "tolerance"
  }
  if (identical(rule, "one_standard_error")) {
    rule <- "one_se"
  }
  if (!rule %in% c("min", "tolerance", "one_se")) {
    stop(
      "Selection field 'uncertainty_rule' must be one of: 'min', 'tolerance', 'one_se'.",
      call. = FALSE
    )
  }
  rule
}

#' Compute the standard error of interval widths
#'
#' @param x Numeric vector of interval widths.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
uncertainty_width_se <- function(x) {
  x_ <- suppressWarnings(as.numeric(x))
  x_ <- x_[is.finite(x_)]
  if (length(x_) < 2L) {
    return(0)
  }
  stats::sd(x_, na.rm = TRUE) / sqrt(length(x_))
}

#' Compute the conformal order statistic
#'
#' @param x Numeric score vector.
#' @param level Coverage level used to choose the order statistic.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
conformal_order_statistic <- function(x, level = 0.95) {
  x_ <- suppressWarnings(as.numeric(x))
  x_ <- x_[is.finite(x_)]
  n <- length(x_)
  if (n == 0L) {
    return(NA_real_)
  }
  k <- min(n, ceiling((n + 1) * level))
  sort(x_)[[k]]
}

#' Compute a weighted species-level mean
#'
#' @param x Numeric values.
#' @param w Non-negative weights.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
weighted_species_mean <- function(x,
                                  w) {
  x_ <- suppressWarnings(as.numeric(x))
  w_ <- suppressWarnings(as.numeric(w))
  ok <- is.finite(x_) & is.finite(w_) & w_ > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  stats::weighted.mean(x_[ok], w = w_[ok], na.rm = TRUE)
}

#' Compute a weighted species-level standard deviation
#'
#' @param x Numeric values.
#' @param w Non-negative weights.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
weighted_species_sd <- function(x,
                                w) {
  x_ <- suppressWarnings(as.numeric(x))
  w_ <- suppressWarnings(as.numeric(w))
  ok <- is.finite(x_) & is.finite(w_) & w_ > 0
  if (sum(ok) < 2L) {
    return(NA_real_)
  }
  x_ <- x_[ok]
  w_ <- w_[ok]
  mu <- stats::weighted.mean(x_, w = w_, na.rm = TRUE)
  w_sum <- sum(w_)
  w_sq_sum <- sum(w_^2)
  denom <- w_sum - (w_sq_sum / w_sum)
  if (!is.finite(denom) || denom <= 0) {
    return(NA_real_)
  }
  sqrt(sum(w_ * (x_ - mu)^2) / denom)
}

#' Compute a weighted species-level standard error
#'
#' @param x Numeric values.
#' @param w Non-negative weights.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
weighted_species_se <- function(x,
                                w) {
  x_ <- suppressWarnings(as.numeric(x))
  w_ <- suppressWarnings(as.numeric(w))
  ok <- is.finite(x_) & is.finite(w_) & w_ > 0
  if (sum(ok) < 2L) {
    return(NA_real_)
  }
  sd_now <- weighted_species_sd(x_[ok], w_[ok])
  if (!is.finite(sd_now)) {
    return(NA_real_)
  }
  eff_n <- (sum(w_[ok])^2) / sum(w_[ok]^2)
  if (!is.finite(eff_n) || eff_n <= 1) {
    return(NA_real_)
  }
  sd_now / sqrt(eff_n)
}

#' Add species-block meta-features for policy learning
#'
#' @param species_performance_table Species-block benchmark table.
#'
#' @return Tibble with fold-validity and leave-one-species-out summaries added.
#'
#' @keywords internal
#' @noRd
augment_species_block_meta_features <- function(species_performance_table) {
  # Standardize the incoming benchmark table before deriving fold summaries.
  species_tbl <- normalize_policy_columns(species_performance_table)
  species_tbl$policy <- resolve_policy_names(species_tbl)
  species_tbl <- tibble::as_tibble(species_tbl)
  if (nrow(species_tbl) == 0L ||
    !all(c("policy", "equation_branch_filter", "anchor_species") %in% names(species_tbl))) {
    return(species_tbl)
  }

  if (!"valid_prediction" %in% names(species_tbl)) {
    if ("multiplier_pred" %in% names(species_tbl)) {
      species_tbl$valid_prediction <- is.finite(species_tbl$multiplier_pred) &
        species_tbl$multiplier_pred > 0
    } else {
      species_tbl$valid_prediction <- FALSE
    }
  }
  if (!"error_abs_log" %in% names(species_tbl) && "multiplier_pred" %in% names(species_tbl)) {
    species_tbl$error_abs_log <- dplyr::if_else(
      is.finite(species_tbl$multiplier_pred) & species_tbl$multiplier_pred > 0,
      abs(log(species_tbl$multiplier_pred)),
      NA_real_
    )
  }

  n_total_folds <- dplyr::n_distinct(species_tbl$anchor_species)
  if (!is.finite(n_total_folds) || n_total_folds < 1L) {
    return(species_tbl)
  }

  # Collapse each policy-by-species fold once so the meta-learner can use
  # policy-level fold validity and leave-current-species-out error summaries
  # without rebuilding these quantities during every fit.
  species_fold_tbl <- species_tbl |>
    dplyr::filter(.data$valid_prediction, is.finite(.data$error_abs_log)) |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter, .data$anchor_species) |>
    dplyr::summarise(
      species_median_abs_log = stats::median(.data$error_abs_log, na.rm = TRUE),
      n_anchor_models = dplyr::n(),
      .groups = "drop"
    )
  if (nrow(species_fold_tbl) == 0L) {
    species_tbl$prop_valid_folds <- 0
    species_tbl$n_valid_folds <- 0L
    species_tbl$policy_loco_mean_abs_log <- NA_real_
    species_tbl$policy_loco_sd_abs_log <- NA_real_
    species_tbl$policy_loco_q75_abs_log <- NA_real_
    species_tbl$policy_loco_q90_abs_log <- NA_real_
    return(species_tbl)
  }

  fold_validity_tbl <- species_fold_tbl |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter) |>
    dplyr::summarise(
      n_valid_folds = dplyr::n_distinct(.data$anchor_species),
      prop_valid_folds = .data$n_valid_folds / n_total_folds,
      .groups = "drop"
    )

  loco_tbl <- species_fold_tbl |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter) |>
    dplyr::group_modify(function(.x_, .y) {
      n_now <- nrow(.x_)
      if (n_now <= 1L) {
        .x_$policy_loco_mean_abs_log <- NA_real_
        .x_$policy_loco_sd_abs_log <- NA_real_
        .x_$policy_loco_q75_abs_log <- NA_real_
        .x_$policy_loco_q90_abs_log <- NA_real_
        return(.x_)
      }

      x_now <- suppressWarnings(as.numeric(.x_$species_median_abs_log))
      w_now <- pmax(1, suppressWarnings(as.numeric(.x_$n_anchor_models)))
      sum_wx <- sum(w_now * x_now, na.rm = TRUE)
      sum_w <- sum(w_now, na.rm = TRUE)

      .x_$policy_loco_mean_abs_log <- vapply(seq_len(n_now), function(i) {
        denom <- sum_w - w_now[[i]]
        if (!is.finite(denom) || denom <= 0) {
          return(NA_real_)
        }
        (sum_wx - (w_now[[i]] * x_now[[i]])) / denom
      }, numeric(1))
      .x_$policy_loco_sd_abs_log <- vapply(seq_len(n_now), function(i) {
        weighted_species_sd(x_now[-i], w_now[-i])
      }, numeric(1))
      .x_$policy_loco_q75_abs_log <- vapply(seq_len(n_now), function(i) {
        stats::quantile(x_now[-i], probs = 0.75, na.rm = TRUE, names = FALSE, type = 8)
      }, numeric(1))
      .x_$policy_loco_q90_abs_log <- vapply(seq_len(n_now), function(i) {
        stats::quantile(x_now[-i], probs = 0.90, na.rm = TRUE, names = FALSE, type = 8)
      }, numeric(1))
      .x_
    }) |>
    dplyr::ungroup() |>
    dplyr::select(
      "policy",
      "equation_branch_filter",
      "anchor_species",
      "policy_loco_mean_abs_log",
      "policy_loco_sd_abs_log",
      "policy_loco_q75_abs_log",
      "policy_loco_q90_abs_log"
    )

  species_tbl |>
    dplyr::left_join(
      fold_validity_tbl,
      by = c("policy", "equation_branch_filter")
    ) |>
    dplyr::left_join(
      loco_tbl,
      by = c("policy", "equation_branch_filter", "anchor_species")
    ) |>
    dplyr::mutate(
      n_valid_folds = dplyr::coalesce(.data$n_valid_folds, 0L),
      prop_valid_folds = dplyr::coalesce(.data$prop_valid_folds, 0)
    )
}

#' Summarize policy coefficient stability by branch
#'
#' @param species_performance_table Species-block benchmark table.
#' @param candidate_models Candidate-model table containing truth coefficients.
#' @param level Coverage level used for residual quantiles.
#'
#' @return Tibble with branch-wise coefficient residual summaries.
#'
#' @keywords internal
#' @noRd
policy_coefficient_stability_summary <- function(species_performance_table,
                                                 candidate_models,
                                                 level = 0.95) {
  # Join predicted policy coefficients back to the held-out truth coefficients.
  perf_tbl <- normalize_policy_columns(species_performance_table)
  perf_tbl$policy <- resolve_policy_names(perf_tbl)
  perf_tbl <- tibble::as_tibble(perf_tbl)
  candidate_models_ <- tibble::as_tibble(candidate_models)
  if (nrow(perf_tbl) == 0 || nrow(candidate_models_) == 0) {
    return(tibble::tibble())
  }
  if (!all(c("anchor_model_id", "policy", "equation_branch_filter", "policy_slope_len", "policy_intercept_len") %in% names(perf_tbl))) {
    return(tibble::tibble())
  }

  id_col <- if ("model_id" %in% names(candidate_models_)) "model_id" else if ("model_id" %in% names(candidate_models_)) "model_id" else NA_character_
  if (is.na(id_col)) {
    return(tibble::tibble())
  }

  candidate_num_or_na <- function(nm) {
    if (!nm %in% names(candidate_models_)) {
      return(rep(NA_real_, nrow(candidate_models_)))
    }
    suppressWarnings(as.numeric(candidate_models_[[nm]]))
  }

  truth_tbl <- candidate_models_ |>
    dplyr::transmute(
      anchor_model_id = as.character(.data[[id_col]]),
      anchor_slope_len = candidate_num_or_na("slope_len"),
      anchor_intercept_len = candidate_num_or_na("intercept_len")
    ) |>
    dplyr::filter(
      is.finite(.data$anchor_slope_len),
      is.finite(.data$anchor_intercept_len)
    )
  if (nrow(truth_tbl) == 0) {
    return(tibble::tibble())
  }

  perf_tbl |>
    dplyr::transmute(
      anchor_model_id = as.character(.data$anchor_model_id),
      policy = as.character(.data$policy),
      equation_branch_filter = resolve_policy_branch_filters(perf_tbl),
      policy_slope_len = suppressWarnings(as.numeric(.data$policy_slope_len)),
      policy_intercept_len = suppressWarnings(as.numeric(.data$policy_intercept_len))
    ) |>
    dplyr::filter(
      !is.na(.data$policy),
      !is.na(.data$equation_branch_filter),
      is.finite(.data$policy_slope_len),
      is.finite(.data$policy_intercept_len)
    ) |>
    dplyr::group_by(.data$anchor_model_id, .data$policy, .data$equation_branch_filter) |>
    dplyr::summarise(
      policy_slope_len = stats::median(.data$policy_slope_len, na.rm = TRUE),
      policy_intercept_len = stats::median(.data$policy_intercept_len, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::inner_join(truth_tbl, by = "anchor_model_id") |>
    dplyr::mutate(
      coefficient_slope_abs_resid = abs(.data$anchor_slope_len - .data$policy_slope_len),
      coefficient_intercept_abs_resid = abs(.data$anchor_intercept_len - .data$policy_intercept_len)
    ) |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter) |>
    dplyr::summarise(
      coefficient_slope_q95 = conformal_order_statistic(.data$coefficient_slope_abs_resid, level = level),
      coefficient_intercept_q95 = conformal_order_statistic(.data$coefficient_intercept_abs_resid, level = level),
      coefficient_stability_n = dplyr::n(),
      .groups = "drop"
    )
}

#' Convert a minimum width into an admissible width threshold
#'
#' @param min_width Minimum observed width.
#' @param uncertainty_rule Width-selection rule.
#' @param u_tol_abs Absolute width tolerance.
#' @param u_tol_rel Relative width tolerance.
#' @param width_se Width standard error.
#' @param one_se_multiplier Multiplier applied under the one-SE rule.
#'
#' @return Numeric vector.
#'
#' @keywords internal
#' @noRd
uncertainty_width_threshold <- function(min_width,
                                        uncertainty_rule = "tolerance",
                                        u_tol_abs = 0.05,
                                        u_tol_rel = 0.25,
                                        width_se = NA_real_,
                                        one_se_multiplier = 1) {
  # Normalize all scalar controls before applying the requested width rule.
  uncertainty_rule_ <- normalize_uncertainty_rule(uncertainty_rule)
  u_tol_abs_ <- suppressWarnings(as.numeric(u_tol_abs)[[1]])
  u_tol_rel_ <- suppressWarnings(as.numeric(u_tol_rel)[[1]])
  width_se_ <- suppressWarnings(as.numeric(width_se)[[1]])
  one_se_multiplier_ <- suppressWarnings(as.numeric(one_se_multiplier)[[1]])
  if (!is.finite(u_tol_abs_)) {
    u_tol_abs_ <- 0
  }
  if (!is.finite(u_tol_rel_)) {
    u_tol_rel_ <- 0
  }
  if (!is.finite(width_se_)) {
    width_se_ <- 0
  }
  if (!is.finite(one_se_multiplier_)) {
    one_se_multiplier_ <- 1
  }
  min_width_ <- suppressWarnings(as.numeric(min_width))
  out <- rep(NA_real_, length(min_width_))
  finite_idx <- is.finite(min_width_)
  if (!any(finite_idx)) {
    return(out)
  }
  if (identical(uncertainty_rule_, "min")) {
    out[finite_idx] <- min_width_[finite_idx]
    return(out)
  }
  if (identical(uncertainty_rule_, "one_se")) {
    out[finite_idx] <- min_width_[finite_idx] + pmax(0, one_se_multiplier_ * width_se_)
    return(out)
  }
  out[finite_idx] <- min_width_[finite_idx] + pmax(
    u_tol_abs_,
    abs(min_width_[finite_idx]) * u_tol_rel_
  )
  out
}

#' Test whether exact branch-level conformal support is available
#'
#' @param source Conformal source label.
#' @param n_scores Number of calibration scores.
#' @param q_value Calibrated conformal radius.
#'
#' @return Logical vector.
#'
#' @keywords internal
#' @noRd
has_exact_branch_conformal_support <- function(source,
                                               n_scores,
                                               q_value) {
  source_ <- as.character(source %||% NA_character_)
  n_scores_ <- suppressWarnings(as.numeric(n_scores))
  q_value_ <- suppressWarnings(as.numeric(q_value))
  source_ %in% c("species_policy_branch", "family_policy_branch", "policy_branch") &
    is.finite(q_value_) &
    is.finite(n_scores_) &
    n_scores_ >= 2
}

#' Return the first non-missing character value
#'
#' @param x Character-like vector.
#'
#' @return Character scalar.
#'
#' @keywords internal
#' @noRd
first_non_missing_character <- function(x) {
  x_ <- stringr::str_squish(as.character(x))
  x_ <- x_[!is.na(x_) & nzchar(x_)]
  if (length(x_) == 0L) {
    return(NA_character_)
  }
  x_[[1]]
}

#' Add calibrated interval columns to anchor-policy rows
#'
#' Attaches the calibrated prediction interval to each policy row. The
#' displayed interval is the calibrated conformal log-radius (`q_col`). Donor
#' structural spread (`local_structural_q_abs_log`) is retained as an audit and
#' as a generic proxy-burden tie-breaker; it is not added to the calibrated
#' radius a second time.
#'
#' @param policy_tbl Anchor-policy prediction table.
#' @param structural_uncertainty_weight Deprecated and ignored. Structural
#'   disagreement is not combined with the calibrated policy radius.
#' @param q_col Name of the conformal log-radius column.
#' @param prediction_col Name of the predicted biomass multiplier column.
#'
#' @return `policy_tbl` with `q_abs_log_conformal`, `q_abs_log_structural`,
#'   `q_abs_log_total`, `multiplier_lo`, `multiplier_hi`, `interval_log_width`,
#'   and `uncertainty_cost_log_width`.
#'
#' @examples
#' interval_tbl <- add_policy_intervals(
#'   tibble::tibble(
#'     policy = "closest_within_species",
#'     multiplier_pred = 1.2,
#'     q_abs_log = 0.15,
#'     local_structural_q_abs_log = 0.05
#'   )
#' )
#' interval_tbl$interval_log_width
#'
#' @keywords internal
#' @noRd
add_policy_intervals <- function(policy_tbl,
                                 structural_uncertainty_weight = 1,
                                 q_col = "q_abs_log",
                                 prediction_col = "multiplier_pred") {
  out <- tibble::as_tibble(policy_tbl)

  if (!q_col %in% names(out)) {
    out[[q_col]] <- NA_real_
  }
  if (!"local_structural_q_abs_log" %in% names(out)) {
    out$local_structural_q_abs_log <- 0
  }
  if (!"valid_prediction" %in% names(out)) {
    if (!prediction_col %in% names(out)) {
      out$valid_prediction <- FALSE
    } else {
      out$valid_prediction <- is.finite(out[[prediction_col]]) &
        out[[prediction_col]] > 0
    }
  }
  if (!"uncertainty_cost_log_width" %in% names(out)) {
    out$uncertainty_cost_log_width <- NA_real_
  }

  q_conformal <- suppressWarnings(as.numeric(out[[q_col]]))
  q_structural_raw <- if ("local_structural_q_abs_log" %in% names(out)) {
    suppressWarnings(as.numeric(out$local_structural_q_abs_log))
  } else {
    rep(NA_real_, nrow(out))
  }
  q_total <- q_conformal

  out$q_abs_log_conformal <- q_conformal
  out$q_abs_log_structural <- q_structural_raw
  out$q_abs_log_total <- q_total
  # Display the selected policy's calibrated radius exactly once.
  q_display <- q_total
  out$multiplier_lo <- dplyr::if_else(
    out$valid_prediction & is.finite(q_display),
    out[[prediction_col]] * exp(-q_display),
    NA_real_
  )
  out$multiplier_hi <- dplyr::if_else(
    out$valid_prediction & is.finite(q_display),
    out[[prediction_col]] * exp(q_display),
    NA_real_
  )
  out$interval_log_width <- dplyr::if_else(
    is.finite(q_total),
    2 * q_total,
    NA_real_
  )
  out$uncertainty_cost_log_width <- dplyr::if_else(
    is.finite(out$interval_log_width),
    out$interval_log_width,
    out$uncertainty_cost_log_width
  )

  out
}

#' Select anchor-facing policies from calibrated policy predictions
#'
#' @param policy_tbl Anchor-policy prediction table.
#' @param uncertainty_rule Final uncertainty-screening rule. Use `"min"` for
#'   strict minimum calibrated width or `"tolerance"` to retain policies within
#'   an absolute/relative width band around that minimum.
#' @param u_tol_rel Relative width tolerance around the minimum calibrated
#'   interval width after empirical-score screening.
#' @param u_tol_abs Absolute log-width tolerance around the minimum calibrated
#'   interval width after empirical-score screening.
#' @param score_tol_abs Optional absolute benchmark-score tolerance.
#' @param one_se_multiplier Multiplier applied to one-standard-error selection
#'   thresholds.
#' @param local_distance_tolerance Absolute tolerance for retaining local
#'   support-distance ties after empirical-score and uncertainty screening.
#'
#' @return A tibble of selected policy rows.
#'
#' @examples
#' selected <- select_anchor_policies(
#'   tibble::tibble(
#'     policy = c("a", "b"),
#'     multiplier_pred = c(1.1, 1.2),
#'     valid_prediction = c(TRUE, TRUE),
#'     selection_valid = c(TRUE, TRUE),
#'     uncertainty_eligible = c(TRUE, TRUE),
#'     uncertainty_cost_log_width = c(0.20, 0.35),
#'     local_weighted_mean_combined_distance = c(0.05, 0.20)
#'   )
#' )
#' selected$selected_policy
#' @keywords internal
#' @noRd
select_anchor_policies <- function(policy_tbl,
                                   uncertainty_rule = "tolerance",
                                   u_tol_rel = 0.25,
                                   u_tol_abs = 0.05,
                                   score_tol_abs = NULL,
                                   one_se_multiplier = 1,
                                   local_distance_tolerance = 1e-12) {
  policy_tbl_ <- tibble::as_tibble(policy_tbl)
  if (nrow(policy_tbl_) == 0) {
    return(policy_tbl_)
  }

  if (!"valid_prediction" %in% names(policy_tbl_)) {
    policy_tbl_$valid_prediction <- is.finite(policy_tbl_$multiplier_pred) &
      policy_tbl_$multiplier_pred > 0
  }
  if ("selection_valid" %in% names(policy_tbl_)) {
    policy_tbl_$selection_valid <- as.logical(policy_tbl_$selection_valid)
  } else if ("n_valid_models" %in% names(policy_tbl_)) {
    policy_tbl_$selection_valid <- is.finite(policy_tbl_$n_valid_models) &
      policy_tbl_$n_valid_models > 0
  } else if ("n_models" %in% names(policy_tbl_)) {
    policy_tbl_$selection_valid <- is.finite(policy_tbl_$n_models) &
      policy_tbl_$n_models > 0
  } else {
    policy_tbl_$selection_valid <- policy_tbl_$valid_prediction
  }
  # A policy cannot be eligible for selection unless it yields a finite,
  # positive anchor-level multiplier prediction. Donor counts alone are not a
  # valid substitute once the selector reaches the final ranking stage.
  policy_tbl_$selection_valid <-
    dplyr::coalesce(policy_tbl_$selection_valid, FALSE) &
      dplyr::coalesce(policy_tbl_$valid_prediction, FALSE)
  if (!"uncertainty_eligible" %in% names(policy_tbl_)) {
    policy_tbl_$uncertainty_eligible <- FALSE
  }
  if (!"uncertainty_cost_log_width" %in% names(policy_tbl_)) {
    policy_tbl_$uncertainty_cost_log_width <- NA_real_
  }
  if (!"species_block_median_abs_log_error" %in% names(policy_tbl_)) {
    policy_tbl_$species_block_median_abs_log_error <- NA_real_
  }
  if (!"mean_species_median_abs_log" %in% names(policy_tbl_)) {
    policy_tbl_$mean_species_median_abs_log <- NA_real_
  }
  if (!"local_weighted_mean_combined_distance" %in% names(policy_tbl_)) {
    policy_tbl_$local_weighted_mean_combined_distance <- NA_real_
  }
  if (!"local_min_combined_distance" %in% names(policy_tbl_)) {
    policy_tbl_$local_min_combined_distance <- NA_real_
  }
  if (!"local_effective_support" %in% names(policy_tbl_)) {
    policy_tbl_$local_effective_support <- NA_real_
  }
  if (!"local_effective_species_support" %in% names(policy_tbl_)) {
    policy_tbl_$local_effective_species_support <- NA_real_
  }
  if (!"local_weighted_q90_combined_distance" %in% names(policy_tbl_)) {
    policy_tbl_$local_weighted_q90_combined_distance <- NA_real_
  }
  if (!"local_structural_q_abs_log" %in% names(policy_tbl_)) {
    policy_tbl_$local_structural_q_abs_log <- NA_real_
  }
  if (!"acceptable_global" %in% names(policy_tbl_)) {
    policy_tbl_$acceptable_global <- NA
  }
  if (!"equivalent_to_best_global" %in% names(policy_tbl_)) {
    policy_tbl_$equivalent_to_best_global <- NA
  }
  if (!"bootstrap_median_rank" %in% names(policy_tbl_)) {
    policy_tbl_$bootstrap_median_rank <- NA_real_
  }
  if (!"best_mean_species_median_abs_log" %in% names(policy_tbl_)) {
    policy_tbl_$best_mean_species_median_abs_log <- NA_real_
  }
  if (!"coefficient_slope_q95" %in% names(policy_tbl_)) {
    policy_tbl_$coefficient_slope_q95 <- NA_real_
  }
  if (!"coefficient_intercept_q95" %in% names(policy_tbl_)) {
    policy_tbl_$coefficient_intercept_q95 <- NA_real_
  }
  candidate_pool_values <- if ("candidate_pool" %in% names(policy_tbl_)) {
    policy_tbl_$candidate_pool
  } else {
    NULL
  }
  aggregation_values <- if ("aggregation_method" %in% names(policy_tbl_)) {
    policy_tbl_$aggregation_method
  } else {
    NULL
  }
  policy_family_values <- if ("policy_family" %in% names(policy_tbl_)) {
    policy_tbl_$policy_family
  } else {
    NULL
  }
  branch_values <- if ("equation_branch_filter" %in% names(policy_tbl_)) {
    policy_tbl_$equation_branch_filter
  } else {
    NULL
  }
  policy_tbl_$specificity_rank <- policy_specificity_rank(
    policy = policy_tbl_$policy,
    candidate_pool = candidate_pool_values,
    aggregation_method = aggregation_values,
    policy_family = policy_family_values,
    equation_branch_filter = branch_values
  )
  policy_tbl_$anchor_selection_broad_aggregate <- policy_is_broad_aggregate(
    policy = policy_tbl_$policy,
    candidate_pool = candidate_pool_values,
    aggregation_method = aggregation_values
  )
  policy_tbl_$anchor_selection_broad_proxy <- policy_is_broad_proxy(
    policy = policy_tbl_$policy,
    candidate_pool = candidate_pool_values
  )
  policy_family_screen_values <- if (is.null(policy_family_values)) {
    rep(NA_character_, nrow(policy_tbl_))
  } else {
    as.character(policy_family_values)
  }
  policy_tbl_$anchor_selection_aggregate_proxy <- !(policy_family_screen_values %in% c(
    "single_model",
    "study_hierarchical_single_model",
    "baseline",
    "random_baseline"
  ))
  local_distance <- dplyr::coalesce(
    policy_tbl_$local_weighted_mean_combined_distance,
    policy_tbl_$local_min_combined_distance
  )
  uses_meta_score <- ".meta_predicted_score" %in% names(policy_tbl_) &&
    any(
      is.finite(suppressWarnings(as.numeric(policy_tbl_$.meta_predicted_score))),
      na.rm = TRUE
    )
  benchmark_validation_error <- dplyr::coalesce(
    policy_tbl_$species_block_median_abs_log_error,
    policy_tbl_$mean_species_median_abs_log
  )
  validation_error <- if (uses_meta_score) {
    suppressWarnings(as.numeric(policy_tbl_$.meta_predicted_score))
  } else {
    benchmark_validation_error
  }
  policy_tbl_$anchor_selection_local_distance <- local_distance
  policy_tbl_$anchor_selection_benchmark_error <- benchmark_validation_error
  policy_tbl_$anchor_selection_validation_error <- validation_error
  policy_tbl_$anchor_selection_global_screen <- FALSE
  policy_tbl_$direct_branch_conformal_support <- has_exact_branch_conformal_support(
    source = if ("meta_q_abs_log_source" %in% names(policy_tbl_)) {
      policy_tbl_$meta_q_abs_log_source
    } else {
      NA_character_
    },
    n_scores = if ("meta_q_abs_log_n_scores" %in% names(policy_tbl_)) {
      policy_tbl_$meta_q_abs_log_n_scores
    } else {
      NA_real_
    },
    q_value = if ("meta_q_abs_log" %in% names(policy_tbl_)) {
      policy_tbl_$meta_q_abs_log
    } else {
      NA_real_
    }
  )

  candidate_tbl <- policy_tbl_ |>
    dplyr::filter(.data$selection_valid)
  tier <- "benchmark_screened_score_only"

  if (nrow(candidate_tbl) == 0) {
    selected <- policy_tbl_ |>
      dplyr::filter(.data$selection_valid) |>
      dplyr::arrange(
        .data$anchor_selection_validation_error,
        .data$bootstrap_median_rank,
        .data$policy
      ) |>
      dplyr::slice(1) |>
      dplyr::mutate(
        selected_policy = .data$policy,
        selected_policy_display = .data$policy,
        selection_tier = "fallback_valid_empirical_score",
        anchor_selection_min_uncertainty_width = NA_real_,
        anchor_selection_uncertainty_threshold = NA_real_,
        anchor_selection_min_validation_error = suppressWarnings(
          min(.data$anchor_selection_validation_error, na.rm = TRUE)
        ),
        anchor_selection_validation_threshold = NA_real_
      )
    return(selected)
  }

  # The predeclared global one-SE screen defines the deterministic benchmark
  # policy library. 
  if (!uses_meta_score &&
    "acceptable_global" %in% names(candidate_tbl) &&
    any(!is.na(candidate_tbl$acceptable_global))) {
    globally_acceptable <- candidate_tbl |>
      dplyr::filter(dplyr::coalesce(.data$acceptable_global, FALSE))
    if (nrow(globally_acceptable) > 0) {
      candidate_tbl <- globally_acceptable
      tier <- paste0(tier, "_acceptable_global")
    }
  }

  benchmark_ranked <- candidate_tbl |>
    dplyr::filter(is.finite(.data$anchor_selection_benchmark_error))
  # In the meta-policy path, `.meta_predicted_score` is already the learned
  # anchor-specific ranking target. Re-applying an anchor-level benchmark
  # one-SE gate on the global species-block summary can override the learned
  # local ranking and force broad, globally average policies to survive over
  # tighter local matches. Keep this secondary benchmark gate only for the
  # deterministic path where no learned local score is available.
  if (!uses_meta_score && nrow(benchmark_ranked) > 0) {
    benchmark_min <- suppressWarnings(
      min(benchmark_ranked$anchor_selection_benchmark_error, na.rm = TRUE)
    )
    benchmark_global_best <- suppressWarnings(
      min(policy_tbl_$best_mean_species_median_abs_log, na.rm = TRUE)
    )
    benchmark_global_threshold <- suppressWarnings(
      min(policy_tbl_$one_se_threshold, na.rm = TRUE)
    )
    benchmark_one_se_slack <- if (is.finite(benchmark_global_best) &&
      is.finite(benchmark_global_threshold)) {
      pmax(0, benchmark_global_threshold - benchmark_global_best)
    } else {
      0
    }
    benchmark_threshold <- benchmark_min + benchmark_one_se_slack

    candidate_tbl$anchor_selection_global_screen <-
      is.finite(candidate_tbl$anchor_selection_benchmark_error) &
        candidate_tbl$anchor_selection_benchmark_error <= benchmark_threshold

    globally_screened <- candidate_tbl |>
      dplyr::filter(.data$anchor_selection_global_screen)
    if (nrow(globally_screened) > 0) {
      candidate_tbl <- globally_screened
      tier <- paste0(tier, "_benchmark_one_se")
    }
  }

  score_ranked <- candidate_tbl |>
    dplyr::filter(is.finite(.data$anchor_selection_validation_error))
  if (nrow(score_ranked) == 0) {
    score_ranked <- candidate_tbl
  }

  min_validation_error <- suppressWarnings(
    min(score_ranked$anchor_selection_validation_error, na.rm = TRUE)
  )
  if (!is.finite(min_validation_error)) {
    min_validation_error <- NA_real_
  }
  score_slack <- suppressWarnings(as.numeric(score_tol_abs %||% NA_real_))
  if (!is.finite(score_slack)) {
    score_slack <- suppressWarnings(min(
      policy_tbl_$one_se_threshold - policy_tbl_$best_mean_species_median_abs_log,
      na.rm = TRUE
    ))
  }
  if (!is.finite(score_slack)) {
    score_slack <- 0
  }
  score_threshold <- min_validation_error + pmax(0, one_se_multiplier * score_slack)

  best_score_rows <- score_ranked |>
    dplyr::filter(.data$anchor_selection_validation_error <= score_threshold)

  selected <- order_policy_assumption_burden(best_score_rows)

  selected |>
    dplyr::slice(1) |>
    dplyr::mutate(
      selected_policy = .data$policy,
      selected_policy_display = .data$policy,
      selection_tier = paste0(tier, "_score_band_burden"),
      anchor_selection_min_validation_error = min_validation_error,
      anchor_selection_validation_threshold = score_threshold,
      anchor_selection_min_uncertainty_width = NA_real_,
      anchor_selection_uncertainty_threshold = NA_real_
    )
}

#' Order empirically equivalent policies by explicit proxy burden
#' @keywords internal
#' @noRd
order_policy_assumption_burden <- function(policy_tbl) {
  tbl <- tibble::as_tibble(policy_tbl)
  for (field in c(
    "local_weighted_q90_combined_distance",
    "local_structural_q_abs_log",
    "local_weighted_mean_combined_distance",
    "local_effective_species_support",
    "local_effective_support",
    "anchor_selection_validation_error",
    "bootstrap_median_rank"
  )) {
    if (!field %in% names(tbl)) {
      tbl[[field]] <- NA_real_
    }
  }
  finite_or_inf <- function(x) {
    out <- suppressWarnings(as.numeric(x))
    out[!is.finite(out)] <- Inf
    out
  }
  support <- suppressWarnings(as.numeric(tbl$local_effective_species_support))
  support[!is.finite(support) | support <= 0] <- suppressWarnings(as.numeric(tbl$local_effective_support))[!is.finite(support) | support <= 0]
  sparse_support <- 1 / sqrt(support)
  sparse_support[!is.finite(sparse_support)] <- Inf
  tbl$anchor_selection_burden_q90_distance <- finite_or_inf(tbl$local_weighted_q90_combined_distance)
  tbl$anchor_selection_burden_disagreement <- finite_or_inf(tbl$local_structural_q_abs_log)
  tbl$anchor_selection_burden_mean_distance <- finite_or_inf(tbl$local_weighted_mean_combined_distance)
  tbl$anchor_selection_burden_sparse_support <- sparse_support
  tbl |>
    dplyr::arrange(
      .data$anchor_selection_burden_q90_distance,
      .data$anchor_selection_burden_disagreement,
      .data$anchor_selection_burden_mean_distance,
      .data$anchor_selection_burden_sparse_support,
      .data$anchor_selection_validation_error,
      .data$bootstrap_median_rank,
      .data$policy
    )
}


#' Summarize one species-block benchmark table
#'
#' Collapses the leave-one-species-out benchmark rows to one species-level
#' summary per policy.
#'
#' @param species_performance_table Species-block benchmark table.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
species_performance <- function(species_performance_table) {
  # Restrict the summary to finite valid predictions before collapsing anchor
  # rows to one species-level error summary per policy.
  {
    species_tbl <- normalize_policy_columns(species_performance_table)
    species_tbl$policy <- resolve_policy_names(species_tbl)
    if (!"valid_prediction" %in% names(species_tbl)) {
      if ("multiplier_pred" %in% names(species_tbl)) {
        species_tbl$valid_prediction <- is.finite(species_tbl$multiplier_pred) &
          species_tbl$multiplier_pred > 0
      } else {
        species_tbl$valid_prediction <- FALSE
      }
    }
    if (!"error_abs_log" %in% names(species_tbl) && "multiplier_pred" %in% names(species_tbl)) {
      species_tbl$error_abs_log <- dplyr::if_else(
        is.finite(species_tbl$multiplier_pred) & species_tbl$multiplier_pred > 0,
        abs(log(species_tbl$multiplier_pred)),
        NA_real_
      )
    }
    species_tbl
  } |>
    dplyr::filter(
      !is_missing_species_identity(.data$anchor_species),
      .data$valid_prediction,
      is.finite(.data$error_abs_log)
    ) |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter, .data$anchor_species) |>
    dplyr::summarise(
      species_median_abs_log = stats::median(.data$error_abs_log, na.rm = TRUE),
      species_mean_abs_log = mean(.data$error_abs_log, na.rm = TRUE),
      n_anchor_models = dplyr::n(),
      candidate_pool = if ("candidate_pool" %in% names(species_tbl)) {
        first_non_missing_character(.data$candidate_pool)
      } else {
        NA_character_
      },
      aggregation_method = if ("aggregation_method" %in% names(species_tbl)) {
        first_non_missing_character(.data$aggregation_method)
      } else {
        NA_character_
      },
      policy_family = if ("policy_family" %in% names(species_tbl)) {
        first_non_missing_character(.data$policy_family)
      } else {
        NA_character_
      },
      .groups = "drop"
    )
}

#' Build the policy-selection reference table
#'
#' Converts the leave-one-species-out benchmark results to a global policy
#' comparison table using a one-standard-error rule plus bootstrap stability.
#'
#' @param species_performance_table Species-block benchmark table.
#' @param candidate_models Optional candidate-model metadata used to enrich
#'   policy labels.
#' @param one_se_multiplier Multiplier applied to one-standard-error
#'   thresholds.
#' @param equivalence_tolerance Practical equivalence tolerance used by paired
#'   comparisons.
#' @param n_boot Number of bootstrap resamples across species.
#' @param seed Integer bootstrap seed.
#' @param progress Logical scalar controlling stage messages.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
build_selection_table <- function(species_performance_table,
                                  candidate_models = NULL,
                                  one_se_multiplier = 1,
                                  equivalence_tolerance = 0.05,
                                  n_boot = 500L,
                                  seed = NULL,
                                  progress = FALSE) {
  # Summarize the benchmark at the species level first so the selection rule
  # aligns with the species-block validation design.
  species_level <- species_performance(species_performance_table)
  if (nrow(species_level) == 0) {
    return(tibble::tibble())
  }

  # Build the global per-policy summary that the later one-SE and bootstrap
  # rules operate on.
  select_ref <- species_level |>
    dplyr::group_by(.data$policy, .data$equation_branch_filter) |>
    dplyr::summarise(
      n_species = dplyr::n(),
      total_anchor_models = sum(.data$n_anchor_models, na.rm = TRUE),
      median_species_median_abs_log = stats::median(.data$species_median_abs_log, na.rm = TRUE),
      mean_species_median_abs_log = weighted_species_mean(.data$species_median_abs_log, .data$n_anchor_models),
      sd_species_median_abs_log = weighted_species_sd(.data$species_median_abs_log, .data$n_anchor_models),
      se_species_median_abs_log = weighted_species_se(.data$species_median_abs_log, .data$n_anchor_models),
      unweighted_mean_species_median_abs_log = mean(.data$species_median_abs_log, na.rm = TRUE),
      unweighted_sd_species_median_abs_log = stats::sd(.data$species_median_abs_log, na.rm = TRUE),
      unweighted_se_species_median_abs_log = dplyr::if_else(
        .data$n_species > 1,
        .data$unweighted_sd_species_median_abs_log / sqrt(.data$n_species),
        NA_real_
      ),
      candidate_pool = first_non_missing_character(.data$candidate_pool),
      aggregation_method = first_non_missing_character(.data$aggregation_method),
      policy_family = first_non_missing_character(.data$policy_family),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      specificity_rank = policy_specificity_rank(
        policy = .data$policy,
        candidate_pool = .data$candidate_pool,
        aggregation_method = .data$aggregation_method,
        policy_family = .data$policy_family,
        equation_branch_filter = .data$equation_branch_filter
      ),
      policy_display = policy_display_label(.data$policy, .data$equation_branch_filter)
    )

  coefficient_stability <- if (!is.null(candidate_models)) {
    policy_coefficient_stability_summary(
      species_performance_table = species_performance_table,
      candidate_models = candidate_models,
      level = 0.95
    )
  } else {
    tibble::tibble()
  }
  if (nrow(coefficient_stability) > 0) {
    select_ref <- select_ref |>
      dplyr::left_join(
        coefficient_stability,
        by = c("policy", "equation_branch_filter")
      )
  } else {
    select_ref <- select_ref |>
      dplyr::mutate(
        coefficient_slope_q95 = NA_real_,
        coefficient_intercept_q95 = NA_real_,
        coefficient_stability_n = NA_real_
      )
  }

  # Identify the best current policy and the one-SE acceptability threshold
  # before the bootstrap summaries are computed.
  best_row <- select_ref |>
    dplyr::arrange(
      .data$mean_species_median_abs_log,
      .data$median_species_median_abs_log,
      .data$specificity_rank,
      .data$policy,
      .data$equation_branch_filter
    ) |>
    dplyr::slice(1)
  if (is.na(best_row$se_species_median_abs_log[[1]])) {
    best_n_species <- as.integer(best_row$n_species[[1]])
    best_label <- as.character(best_row$policy_display[[1]])
    warning(
      sprintf(
        paste0(
          "The best-ranked policy/branch (%s) had valid species-block predictions for %d anchor species; ",
          "its SE is NA and the one-SE acceptance threshold collapses to its mean error. ",
          "This describes that policy's coverage, not the total benchmark species count."
        ),
        best_label,
        best_n_species
      ),
      call. = FALSE
    )
  }
  threshold <- best_row$mean_species_median_abs_log[[1]] +
    as.numeric(one_se_multiplier) * dplyr::coalesce(best_row$se_species_median_abs_log[[1]], 0)

  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }
  set.seed(as.integer(seed))

  n_policies_sel <- nrow(dplyr::distinct(species_level, .data$policy, .data$equation_branch_filter))
  n_species_sel <- nrow(dplyr::distinct(species_level, .data$anchor_species))
  report_progress(
    progress,
    "[Selection] Bootstrapping ", as.integer(n_boot),
    " resamples across ", n_policies_sel, " policies and ", n_species_sel, " species..."
  )

  # Pivot species_level to a wide matrix (species rows x policy columns) once
  # so all bootstrap resamples are vectorized across policies simultaneously.
  species_level_keyed <- dplyr::mutate(
    species_level,
    .policy_key = paste(
      as.character(.data$policy),
      normalize_policy_equation_branch_filters(.data$equation_branch_filter),
      sep = "|"
    )
  )
  key_lookup <- dplyr::distinct(
    species_level_keyed,
    .data$.policy_key,
    .data$policy,
    .data$equation_branch_filter,
    .data$candidate_pool,
    .data$aggregation_method,
    .data$policy_family
  )
  species_wide <- tidyr::pivot_wider(
    species_level_keyed,
    id_cols = "anchor_species",
    names_from = ".policy_key",
    values_from = "species_median_abs_log"
  )
  policy_key_order <- setdiff(names(species_wide), "anchor_species")
  policy_mat <- as.matrix(species_wide[, policy_key_order, drop = FALSE])
  n_spp_mat <- nrow(policy_mat)
  n_boot_int <- as.integer(n_boot)

  # Single bulk sample.int call produces all bootstrap indices; colMeans over
  # the resampled matrix gives bootstrap means for every policy in one pass.
  boot_idx <- matrix(sample.int(n_spp_mat, n_spp_mat * n_boot_int, replace = TRUE), nrow = n_spp_mat)
  boot_means_mat <- apply(boot_idx, 2, function(idx) {
    colMeans(policy_mat[idx, , drop = FALSE], na.rm = TRUE)
  })
  if (is.null(dim(boot_means_mat))) {
    boot_means_mat <- matrix(boot_means_mat, nrow = length(policy_key_order))
  }
  rownames(boot_means_mat) <- policy_key_order

  # boot_sum: per-policy quantiles and threshold probability from matrix rows.
  boot_sum <- dplyr::left_join(
    tibble::tibble(.policy_key = policy_key_order),
    key_lookup,
    by = ".policy_key"
  ) |>
    dplyr::mutate(
      bootstrap_mean_q05 = apply(
        boot_means_mat, 1, stats::quantile,
        probs = 0.05, na.rm = TRUE, names = FALSE, type = 8
      ),
      bootstrap_mean_q50 = apply(
        boot_means_mat, 1, stats::quantile,
        probs = 0.50, na.rm = TRUE, names = FALSE, type = 8
      ),
      bootstrap_mean_q95 = apply(
        boot_means_mat, 1, stats::quantile,
        probs = 0.95, na.rm = TRUE, names = FALSE, type = 8
      ),
      bootstrap_prob_within_threshold = rowMeans(boot_means_mat <= threshold, na.rm = TRUE)
    ) |>
    dplyr::select(-tidyselect::all_of(".policy_key"))

  # boot_rank: rank policies within each bootstrap draw using a tiny numeric
  # secondary key to break ties by specificity_rank (matches original ordering).
  spec_ranks <- dplyr::left_join(
    tibble::tibble(.policy_key = policy_key_order),
    key_lookup,
    by = ".policy_key"
  ) |>
    dplyr::mutate(
      specificity_rank = policy_specificity_rank(
        policy = .data$policy,
        candidate_pool = .data$candidate_pool,
        aggregation_method = .data$aggregation_method,
        policy_family = .data$policy_family,
        equation_branch_filter = .data$equation_branch_filter
      )
    ) |>
    dplyr::pull(.data$specificity_rank)
  secondary_key <- (spec_ranks / (max(spec_ranks, na.rm = TRUE) + 1L)) * 1e-10
  rank_mat <- apply(boot_means_mat + secondary_key, 2, rank, ties.method = "first")
  if (is.null(dim(rank_mat))) {
    rank_mat <- matrix(rank_mat, nrow = length(policy_key_order))
  }
  boot_rank <- dplyr::left_join(
    tibble::tibble(.policy_key = policy_key_order),
    key_lookup,
    by = ".policy_key"
  ) |>
    dplyr::mutate(
      bootstrap_prob_best = rowMeans(rank_mat == 1L, na.rm = TRUE),
      bootstrap_median_rank = apply(rank_mat, 1, stats::median, na.rm = TRUE)
    ) |>
    dplyr::select(-tidyselect::all_of(".policy_key"))

  # Merge the bootstrap diagnostics back onto the policy summary and record
  # the final acceptability calls.
  select_ref |>
    dplyr::left_join(boot_sum, by = c("policy", "equation_branch_filter")) |>
    dplyr::left_join(boot_rank, by = c("policy", "equation_branch_filter")) |>
    dplyr::mutate(
      best_policy_global = best_row$policy[[1]],
      best_equation_branch_filter_global = best_row$equation_branch_filter[[1]],
      best_mean_species_median_abs_log = best_row$mean_species_median_abs_log[[1]],
      one_se_multiplier = as.numeric(one_se_multiplier),
      one_se_threshold = threshold,
      acceptable_one_se = .data$mean_species_median_abs_log <= threshold,
      acceptable_bootstrap = dplyr::coalesce(.data$bootstrap_prob_within_threshold, 0) >= 0.50,
      acceptable_global = .data$acceptable_one_se,
      equivalence_tolerance = as.numeric(equivalence_tolerance)
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$acceptable_global),
      dplyr::desc(.data$acceptable_one_se),
      .data$mean_species_median_abs_log,
      .data$specificity_rank,
      .data$policy,
      .data$equation_branch_filter
    )
}

#' Build pairwise policy-equivalence summaries
#'
#' Compares paired species-block benchmark errors and records whether each
#' policy pair is practically and statistically indistinguishable.
#'
#' @param species_performance_table Species-block benchmark table.
#' @param select_ref Global policy-selection reference table.
#' @param tolerance Practical equivalence tolerance on mean paired error
#'   difference.
#' @param n_boot Number of paired bootstrap resamples across species.
#' @param seed Integer bootstrap seed.
#' @param progress Logical scalar controlling stage messages.
#' @param engine Pairwise bootstrap engine, `"cpp"` or `"r"`. The R engine is
#'   retained as the differential-test oracle.
#'
#' @return A list with `pairs` and `best_flags`.
#'
#' @keywords internal
#' @noRd
build_equivalence_table <- function(species_performance_table,
                                    select_ref,
                                    tolerance = 0.05,
                                    n_boot = 500L,
                                    seed = NULL,
                                    progress = FALSE,
                                    engine = c("cpp", "r")) {
  # Reuse the species-level benchmark summary so equivalence is assessed on the
  # same validation scale as the global selection table.
  species_level <- species_performance(species_performance_table) |>
    dplyr::select("policy", "equation_branch_filter", "anchor_species", "species_median_abs_log")

  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }
  engine <- match.arg(engine)

  if (nrow(species_level) == 0 || nrow(select_ref) == 0) {
    return(list(
      pairs = tibble::tibble(),
      best_flags = tibble::tibble(
        policy = character(0),
        equation_branch_filter = character(0),
        equivalent_to_best_global = logical(0),
        paired_mean_diff_to_best = numeric(0)
      )
    ))
  }

  policy_nodes <- species_level |>
    dplyr::distinct(.data$policy, .data$equation_branch_filter) |>
    dplyr::arrange(.data$policy, .data$equation_branch_filter)
  policy_keys <- paste(
    as.character(policy_nodes$policy),
    normalize_policy_equation_branch_filters(policy_nodes$equation_branch_filter),
    sep = "|"
  )

  # Pre-pivot species_level to a wide matrix once so each pair extracts two
  # columns directly instead of re-filtering and pivoting the full table.
  eq_wide_tbl <- species_level |>
    dplyr::mutate(
      .policy_key = paste(
        as.character(.data$policy),
        normalize_policy_equation_branch_filters(.data$equation_branch_filter),
        sep = "|"
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = "anchor_species",
      names_from = ".policy_key",
      values_from = "species_median_abs_log"
    )
  eq_wide_mat <- as.matrix(eq_wide_tbl[, setdiff(names(eq_wide_tbl), "anchor_species"), drop = FALSE])
  eq_wide_mat <- eq_wide_mat[, policy_keys, drop = FALSE]
  n_boot_int_eq <- as.integer(n_boot)

  if (nrow(policy_nodes) < 2) {
    return(list(
      pairs = tibble::tibble(),
      best_flags = policy_nodes |>
        dplyr::mutate(
          equivalent_to_best_global = TRUE,
          paired_mean_diff_to_best = 0
        )
    ))
  }

  # Identify the best current policy first so equivalence-to-best can be
  # derived directly from the pairwise comparison table later.
  best_policy <-
    {
      select_ref_tbl <- normalize_policy_columns(select_ref)
      select_ref_tbl$policy <- resolve_policy_names(select_ref_tbl)
      select_ref_tbl
    } |>
    dplyr::arrange(.data$mean_species_median_abs_log, .data$specificity_rank, .data$policy, .data$equation_branch_filter) |>
    dplyr::slice(1)
  best_key <- paste(
    as.character(best_policy$policy[[1]]),
    normalize_policy_equation_branch_filters(best_policy$equation_branch_filter[[1]]),
    sep = "|"
  )

  n_pairs <- choose(length(policy_keys), 2L)
  report_progress(
    progress,
    "[Selection] Computing pairwise equivalence across ", n_pairs, " policy pairs (",
    length(policy_keys), " policies, ", as.integer(n_boot), " bootstrap resamples each)..."
  )

  pair_tbl <- if (identical(engine, "cpp")) {
    tibble::as_tibble(cpp_policy_equivalence_pairs(
      policy_matrix = eq_wide_mat,
      policy = as.character(policy_nodes$policy),
      branch = normalize_policy_equation_branch_filters(policy_nodes$equation_branch_filter),
      policy_key = policy_keys,
      tolerance = as.numeric(tolerance),
      n_boot = n_boot_int_eq,
      seed = as.integer(seed)
    ))
  } else {
    utils::combn(policy_keys, 2, simplify = FALSE) |>
      purrr::map_dfr(function(pair) {
        lhs_key <- pair[[1]]
        rhs_key <- pair[[2]]
        lhs_idx <- match(lhs_key, policy_keys)
        rhs_idx <- match(rhs_key, policy_keys)
        lhs <- policy_nodes$policy[[lhs_idx]]
        lhs_branch <- policy_nodes$equation_branch_filter[[lhs_idx]]
        rhs <- policy_nodes$policy[[rhs_idx]]
        rhs_branch <- policy_nodes$equation_branch_filter[[rhs_idx]]

        # Look up the two policy columns directly from the pre-pivoted matrix.
        lhs_col <- eq_wide_mat[, lhs_key]
        rhs_col <- eq_wide_mat[, rhs_key]
        both_finite <- is.finite(lhs_col) & is.finite(rhs_col)
        n_common <- sum(both_finite)

        if (n_common == 0L) {
          return(tibble::tibble(
            policy_a = lhs,
            equation_branch_filter_a = lhs_branch,
            policy_b = rhs,
            equation_branch_filter_b = rhs_branch,
            n_species_common = 0L,
            paired_mean_diff = NA_real_,
            paired_median_diff = NA_real_,
            paired_boot_q025 = NA_real_,
            paired_boot_q975 = NA_real_,
            equivalent_pair = FALSE,
            pair_decision = "inconclusive",
            better_policy = NA_character_
          ))
        }

        # Vectorized bootstrap: single sample.int + matrix colMeans replaces
        # replicate(n_boot, { sample + mean }) for a ~20-50x speedup per pair.
        diff_vec <- (lhs_col - rhs_col)[both_finite]
        set.seed(as.integer(seed) + sum(utf8ToInt(paste(lhs_key, rhs_key, sep = "|"))))
        n_d <- length(diff_vec)
        boot_idx_eq <- matrix(sample.int(n_d, n_d * n_boot_int_eq, replace = TRUE), nrow = n_d)
        boot_means <- colMeans(matrix(diff_vec[boot_idx_eq], nrow = n_d), na.rm = TRUE)

        q025 <- stats::quantile(boot_means, probs = 0.025, na.rm = TRUE, names = FALSE, type = 8)
        q975 <- stats::quantile(boot_means, probs = 0.975, na.rm = TRUE, names = FALSE, type = 8)
        mean_diff <- mean(diff_vec, na.rm = TRUE)
        med_diff <- stats::median(diff_vec, na.rm = TRUE)

        # Treat overlap with the tolerance boundary as inconclusive. A pair is
        # equivalent only when the full bootstrap interval lies inside the
        # practical-equivalence band; a policy is better only when the full
        # interval lies beyond that band on one side.
        eq_pair <- is.finite(q025) &&
          is.finite(q975) &&
          q025 >= -tolerance &&
          q975 <= tolerance
        lhs_better <- is.finite(q975) && q975 < -tolerance
        rhs_better <- is.finite(q025) && q025 > tolerance

        # Force evaluation
        force(lhs_better)
        force(rhs_better)

        pair_decision <- dplyr::case_when(
          eq_pair ~ "equivalent",
          lhs_better ~ "lhs_better",
          rhs_better ~ "rhs_better",
          TRUE ~ "inconclusive"
        )

        better <- dplyr::case_when(
          pair_decision == "lhs_better" ~ lhs,
          pair_decision == "rhs_better" ~ rhs,
          TRUE ~ NA_character_
        )

        tibble::tibble(
          policy_a = lhs,
          equation_branch_filter_a = lhs_branch,
          policy_b = rhs,
          equation_branch_filter_b = rhs_branch,
          n_species_common = n_common,
          paired_mean_diff = mean_diff,
          paired_median_diff = med_diff,
          paired_boot_q025 = q025,
          paired_boot_q975 = q975,
          equivalent_pair = eq_pair,
          pair_decision = pair_decision,
          better_policy = better
        )
      })
  }

  pair_tbl <- pair_tbl |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "paired_mean_diff",
          "paired_median_diff",
          "paired_boot_q025",
          "paired_boot_q975"
        )),
        ~ signif(.x, 15)
      )
    )

  # Convert the pairwise table to one row per policy showing whether it is
  # equivalent to the global best policy.
  best_flags <- policy_nodes |>
    dplyr::mutate(
      .policy_key = paste(
        as.character(.data$policy),
        normalize_policy_equation_branch_filters(.data$equation_branch_filter),
        sep = "|"
      ),
      equivalent_to_best_global = purrr::map_lgl(.data$.policy_key, function(policy_key_now) {
        if (identical(policy_key_now, best_key)) {
          return(TRUE)
        }

        row <- pair_tbl |>
          dplyr::filter(
            (paste(
              as.character(.data$policy_a),
              normalize_policy_equation_branch_filters(.data$equation_branch_filter_a),
              sep = "|"
            ) == best_key &
              paste(
                as.character(.data$policy_b),
                normalize_policy_equation_branch_filters(.data$equation_branch_filter_b),
                sep = "|"
              ) == policy_key_now) |
              (paste(
                as.character(.data$policy_b),
                normalize_policy_equation_branch_filters(.data$equation_branch_filter_b),
                sep = "|"
              ) == best_key &
                paste(
                  as.character(.data$policy_a),
                  normalize_policy_equation_branch_filters(.data$equation_branch_filter_a),
                  sep = "|"
                ) == policy_key_now)
          ) |>
          dplyr::slice(1)

        nrow(row) == 1 && isTRUE(row$equivalent_pair[[1]])
      }),
      paired_mean_diff_to_best = purrr::map_dbl(.data$.policy_key, function(policy_key_now) {
        if (identical(policy_key_now, best_key)) {
          return(0)
        }

        row <- pair_tbl |>
          dplyr::filter(
            (paste(
              as.character(.data$policy_a),
              normalize_policy_equation_branch_filters(.data$equation_branch_filter_a),
              sep = "|"
            ) == best_key &
              paste(
                as.character(.data$policy_b),
                normalize_policy_equation_branch_filters(.data$equation_branch_filter_b),
                sep = "|"
              ) == policy_key_now) |
              (paste(
                as.character(.data$policy_b),
                normalize_policy_equation_branch_filters(.data$equation_branch_filter_b),
                sep = "|"
              ) == best_key &
                paste(
                  as.character(.data$policy_a),
                  normalize_policy_equation_branch_filters(.data$equation_branch_filter_a),
                  sep = "|"
                ) == policy_key_now)
          ) |>
          dplyr::slice(1)

        if (nrow(row) == 0 || !is.finite(row$paired_mean_diff[[1]])) {
          return(NA_real_)
        }

        if (identical(
          paste(
            as.character(row$policy_a[[1]]),
            normalize_policy_equation_branch_filters(row$equation_branch_filter_a[[1]]),
            sep = "|"
          ),
          best_key
        )) {
          return(row$paired_mean_diff[[1]])
        }

        -row$paired_mean_diff[[1]]
      }),
      best_policy_global = best_policy$policy[[1]],
      best_equation_branch_filter_global = best_policy$equation_branch_filter[[1]]
    ) |>
    dplyr::select(-tidyselect::all_of(".policy_key"))

  list(pairs = pair_tbl, best_flags = best_flags)
}

#' Build policy-equivalence classes
#'
#' Converts pairwise equivalence calls to connected equivalence classes.
#'
#' @param select_ref Global policy-selection reference table.
#' @param pair_tbl Pairwise policy-equivalence table.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
build_equivalence_sets <- function(select_ref,
                                   pair_tbl) {
  # Start from the policy list in the selection table so the class summary
  # always covers every benchmarked policy.
  select_ref <- normalize_policy_columns(select_ref)
  select_ref$policy <- resolve_policy_names(select_ref)
  policy_nodes <- select_ref |>
    dplyr::distinct(.data$policy, .data$equation_branch_filter) |>
    dplyr::arrange(.data$policy, .data$equation_branch_filter)
  if (nrow(policy_nodes) == 0) {
    return(tibble::tibble())
  }

  node_keys <- paste(
    as.character(policy_nodes$policy),
    normalize_policy_equation_branch_filters(policy_nodes$equation_branch_filter),
    sep = "|"
  )
  adjacency <- stats::setNames(vector("list", length(node_keys)), node_keys)
  for (policy_key_now in node_keys) {
    adjacency[[policy_key_now]] <- character(0)
  }

  # Build an undirected adjacency list from the pairwise equivalence calls.
  if (nrow(pair_tbl) > 0) {
    eq_pairs <- pair_tbl |>
      dplyr::filter(.data$equivalent_pair) |>
      dplyr::select("policy_a", "equation_branch_filter_a", "policy_b", "equation_branch_filter_b")

    if (nrow(eq_pairs) > 0) {
      for (i in seq_len(nrow(eq_pairs))) {
        lhs <- paste(
          as.character(eq_pairs$policy_a[[i]]),
          normalize_policy_equation_branch_filters(eq_pairs$equation_branch_filter_a[[i]]),
          sep = "|"
        )
        rhs <- paste(
          as.character(eq_pairs$policy_b[[i]]),
          normalize_policy_equation_branch_filters(eq_pairs$equation_branch_filter_b[[i]]),
          sep = "|"
        )
        adjacency[[lhs]] <- unique(c(adjacency[[lhs]], rhs))
        adjacency[[rhs]] <- unique(c(adjacency[[rhs]], lhs))
      }
    }
  }

  # Walk the connected components so each equivalence class is represented once
  # and each policy gets one membership row.
  visited <- stats::setNames(rep(FALSE, length(node_keys)), node_keys)
  class_rows <- list()
  class_id <- 0L

  for (root in node_keys) {
    if (visited[[root]]) {
      next
    }

    class_id <- class_id + 1L
    queue <- root
    members <- character(0)

    while (length(queue) > 0) {
      node <- queue[[1]]
      queue <- queue[-1]
      if (visited[[node]]) {
        next
      }

      visited[[node]] <- TRUE
      members <- c(members, node)
      nbrs <- adjacency[[node]] %||% character(0)
      queue <- unique(c(queue, nbrs[!visited[nbrs]]))
    }

    members <- sort(unique(members))
    member_idx <- match(members, node_keys)
    member_tbl <- policy_nodes[member_idx, , drop = FALSE]
    class_rows[[length(class_rows) + 1]] <- tibble::tibble(
      policy = member_tbl$policy,
      equation_branch_filter = member_tbl$equation_branch_filter,
      equivalence_class_id = paste0("class_", class_id),
      equivalence_class_size = length(members),
      equivalence_class_members = paste(
        policy_display_label(member_tbl$policy, member_tbl$equation_branch_filter),
        collapse = "; "
      )
    )
  }

  dplyr::bind_rows(class_rows) |>
    dplyr::arrange(.data$equivalence_class_id, .data$policy, .data$equation_branch_filter)
}

#' Run the policy-selection summary
#'
#' Builds the global policy-selection table, pairwise equivalence summary,
#' and equivalence-class table from the species-block benchmark results.
#'
#' @param species_performance_table Species-block benchmark table.
#' @param candidate_models Optional candidate-model metadata used to enrich
#'   policy labels.
#' @param config Optional JSON path or list with `tolerance`, `n_boot`, and
#'   `seed`. A [Configurer] object is also accepted.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#' @param progress Logical scalar controlling stage messages.
#'
#' @return A list containing the selection table, pairwise equivalence table,
#'   equivalence-class table, and merged final selection table.
#'
#' @examples
#' \dontrun{
#' selection_obj <- run_policy_selection(
#'   species_block_perf,
#'   config = list(tolerance = 0.01, n_boot = 100L)
#' )
#' selection_obj$final_ref
#' }
#'
#' @keywords internal
#' @noRd
run_policy_selection <- function(species_performance_table,
                                 candidate_models = NULL,
                                 config = NULL,
                                 cache_path = NULL,
                                 refresh = FALSE,
                                 progress = FALSE) {
  # Validate cache control and the benchmark input before any bootstrap work
  # begins.
  if (!is.data.frame(species_performance_table)) {
    stop("'species_performance_table' must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.null(cache_path) &&
    (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Reuse the cached selection object when available unless a refresh was
  # explicitly requested.
  if (!is.null(cache_path) && tsb_cache_exists(cache_path) && !refresh) {
    return(tsb_cache_read(cache_path))
  }

  # Inline the policy-selection defaults here so the function resolves its own
  # fallback settings without an extra config helper.
  config_values <- merge_config_sections(
    list(
      one_se_multiplier = 1,
      equivalence_tolerance = 0.05,
      tolerance = 0.05,
      n_boot = 500L,
      seed = NULL
    ),
    read_similarity_config(config)
  )
  config_values$equivalence_tolerance <- config_values$equivalence_tolerance %||% config_values$tolerance

  # Build the base selection summary first, then layer on the pairwise and
  # equivalence-class summaries before assembling the final merged table.
  select_ref <- build_selection_table(
    species_performance_table = species_performance_table,
    candidate_models = candidate_models,
    one_se_multiplier = config_values$one_se_multiplier,
    equivalence_tolerance = config_values$equivalence_tolerance,
    n_boot = config_values$n_boot,
    seed = config_values$seed,
    progress = progress
  )
  if (!all(c("policy", "equation_branch_filter") %in% names(select_ref))) {
    # Return typed empty selection objects so sparse benchmark folds can fall
    # through to later deterministic or learner-free fallback logic without
    # join-column crashes.
    empty_keys <- tibble::tibble(
      policy = character(0),
      equation_branch_filter = character(0)
    )
    return(list(
      select_ref = empty_keys,
      equiv_ref = list(
        pairs = tibble::tibble(
          policy_a = character(0),
          equation_branch_filter_a = character(0),
          policy_b = character(0),
          equation_branch_filter_b = character(0),
          equivalent_pair = logical(0),
          paired_mean_diff = numeric(0),
          paired_mean_diff_q05 = numeric(0),
          paired_mean_diff_q95 = numeric(0),
          n_species_overlap = integer(0)
        ),
        best_flags = tibble::tibble(
          policy = character(0),
          equation_branch_filter = character(0),
          equivalent_to_best_global = logical(0),
          paired_mean_diff_to_best = numeric(0)
        )
      ),
      equiv_sets = tibble::tibble(
        policy = character(0),
        equation_branch_filter = character(0),
        equivalence_class_id = integer(0),
        equivalence_class_size = integer(0),
        equivalence_class_members = character(0)
      ),
      final_ref = empty_keys
    ))
  }
  equiv_ref <- build_equivalence_table(
    species_performance_table = species_performance_table,
    select_ref = select_ref,
    tolerance = config_values$equivalence_tolerance,
    n_boot = config_values$n_boot,
    seed = if (!is.null(config_values$seed)) config_values$seed + 3L else NULL,
    progress = progress
  )
  equiv_sets <- build_equivalence_sets(
    select_ref = select_ref,
    pair_tbl = equiv_ref$pairs
  )

  final_ref <- select_ref |>
    dplyr::left_join(equiv_ref$best_flags, by = c("policy", "equation_branch_filter")) |>
    dplyr::left_join(equiv_sets, by = c("policy", "equation_branch_filter")) |>
    dplyr::mutate(
      equivalent_to_best_global = dplyr::coalesce(.data$equivalent_to_best_global, FALSE),
      paired_mean_diff_to_best = dplyr::coalesce(.data$paired_mean_diff_to_best, NA_real_)
    )

  result <- list(
    select_ref = select_ref,
    equiv_ref = equiv_ref,
    equiv_sets = equiv_sets,
    final_ref = final_ref
  )

  # Cache the in-memory selection summaries only when a cache path was
  # supplied.
  if (!is.null(cache_path)) {
    tsb_cache_write(result, cache_path)
  }

  result
}
