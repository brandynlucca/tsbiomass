#' Policy Selection S7 Classes
#'
#' `PolicySelector` is the object-oriented wrapper around the policy
#' benchmarking, uncertainty calibration, global policy selection, and
#' anchor-facing policy prediction layers.
#'
#' It is constructed from a staged [Candidates] object and then advanced
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
#' selector <- as_policy_selector(candidates)
#' selector <- benchmark(selector)
#' selector <- calibrate_uncertainty(selector)
#' selector <- select_policies(selector)
#' predictions <- predict(selector)
#' predictions@selections
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
#' predictions@intervals
#' predictions@selections
#' predictions@consensus
#' }
#'
#' @name PolicyPredictions-class
#' @usage NULL
#' @aliases PolicyPredictions
NULL

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
        if (!(inherits(self@candidates, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(self@candidates, Candidates), error = function(e) FALSE)))) {
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
policy_selector_config_data <- function(config) {
  if (is.null(config)) {
    return(list())
  }
  if ((inherits(config, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, PolicySelector), error = function(e) FALSE)))) {
    return(config@config)
  }
  if ((inherits(config, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
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
policy_selector_config_value <- function(cfg,
                                         key,
                                         sections = character()) {
  if (!is.list(cfg)) {
    return(NULL)
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
#' @param candidates A [Candidates] object.
#' @param config Optional selector config list or [Configurer] object.
#'
#' @return A `PolicySelector` object.
#'
#' @examples
#' \dontrun{
#' selector <- create_policy_selector(candidates)
#' selector
#' }
#'
#' @export
create_policy_selector <- function(candidates,
                                   config = NULL) {
  if ((inherits(candidates, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidates, PolicySelector), error = function(e) FALSE)))) {
    return(candidates)
  }

  if (!(inherits(candidates, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidates, Candidates), error = function(e) FALSE)))) {
    stop("'candidates' must be a `Candidates` object.", call. = FALSE)
  }

  PolicySelector(
    candidates = candidates,
    config = policy_selector_config_data(config),
    benchmark = list(),
    uncertainty = list(),
    selection = list()
  )
}

#' Coerce staged candidates to `PolicySelector`
#'
#' @param candidates A [Candidates] object.
#' @param config Optional selector config list or [Configurer] object.
#'
#' @return A `PolicySelector` object.
#'
#' @export
as_policy_selector <- function(candidates,
                               config = NULL) {
  create_policy_selector(
    candidates = candidates,
    config = config
  )
}

#' Resolve similarity defaults from `Candidates`
#'
#' @param candidates A [Candidates] object.
#'
#' @return A list.
#'
#' @keywords internal
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

  cfg <- default_config(use_canonical_names = TRUE)
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
policy_selector_anchor_config <- function(object,
                                          config = NULL) {
  cfg <- merge_cfg(object@config, policy_selector_config_data(config))
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

  merge_cfg(
    defaults,
    overrides
  )
}

#' Resolve the active policy set
#'
#' @param cfg Config list.
#' @param policies Optional explicit policy vector.
#'
#' @return Character vector or `NULL`.
#'
#' @keywords internal
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
policy_selector_cached_anchor_evaluation <- function(object,
                                                     anchor_row,
                                                     config_supplied) {
  if (length(object@candidates@admissibility) == 0) {
    return(NULL)
  }
  if (!is.list((object@candidates@admissibility)$anchors %||% NULL)) {
    return(NULL)
  }

  anchor_id <- if ("model_id_chr" %in% names(anchor_row)) {
    as.character(anchor_row$model_id_chr[[1]])
  } else {
    as.character(anchor_row$model_id[[1]])
  }

  ((object@candidates@admissibility)$anchors[[anchor_id]])$evaluation %||% NULL
}

canonicalize_equivalent_policy_rows <- function(policy_tbl) {
  policy_tbl <- standardize_policies(policy_tbl)
  if (nrow(policy_tbl) == 0L ||
    !"realized_donor_fingerprint" %in% names(policy_tbl) ||
    !"multiplier_pred" %in% names(policy_tbl) ||
    !"policy_slope_len" %in% names(policy_tbl) ||
    !"policy_intercept_len" %in% names(policy_tbl)) {
    return(policy_tbl)
  }

  if (!"specificity_rank" %in% names(policy_tbl)) {
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
  }

  policy_tbl |>
    dplyr::mutate(
      .dedupe_multiplier = signif(suppressWarnings(as.numeric(.data$multiplier_pred)), 12),
      .dedupe_slope = signif(suppressWarnings(as.numeric(.data$policy_slope_len)), 12),
      .dedupe_intercept = signif(suppressWarnings(as.numeric(.data$policy_intercept_len)), 12)
    ) |>
    dplyr::group_by(
      .data$anchor_model_id,
      .data$equation_branch_filter,
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
#' @name benchmark.PolicySelector
#' @usage NULL
#'
#' @param object A [PolicySelector] object.
#' @param policies Optional character vector of policy names.
#' @param policy_fun Optional policy-evaluation function.
#' @param curve_fun Optional curve-prediction function.
#' @param model_scores Optional ordination model-score table.
#' @param species_lookup Optional ordination species-lookup table.
#' @param reference_ids Optional reference-anchor id override.
#' @param policy_params Optional policy-parameter overrides.
#' @param policy_path Optional policy-registry path.
#' @param config Optional config override.
#' @param include_ts_error Optional logical scalar controlling time-series error
#'   evaluation.
#' @param benchmark_schemes Character vector of benchmark scheme names.
#' @param workers Optional worker count.
#' @param package_dir Optional package source directory for worker bootstrap.
#' @param cache_path Optional cache path.
#' @param refresh Optional logical scalar controlling cache refresh.
#' @param progress Optional logical scalar controlling progress messages.
#' @param group_block_col Optional blocking column for grouped species holds.
#' @param group_block_label Label attached to grouped holdout rows.
#' @param registry_path Optional trait-registry path.
#'
#' @return An updated [PolicySelector] object with stored benchmark results.
#'
#' Run the `PolicySelector` benchmark stage and store the resulting benchmark
#' bundle on the selector.
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
                                                  package_dir = NULL,
                                                  cache_path = NULL,
                                                  refresh = NULL,
                                                  progress = NULL,
                                                  group_block_col = NULL,
                                                  group_block_label = "leave_one_group_out",
                                                  registry_path = NULL) {
  cfg <- merge_cfg(object@config, policy_selector_config_data(config))
  # Resolve benchmark-stage controls before collecting the reusable ordination
  # context and reference-anchor identifiers.
  include_ts_error <- include_ts_error %||%
    policy_selector_config_value(cfg, "include_ts_error", sections = "benchmark") %||%
    TRUE
  workers <- workers %||%
    policy_selector_config_value(cfg, "workers", sections = "benchmark") %||%
    1L
  cache_path <- cache_path %||%
    policy_selector_config_value(cfg, "cache_path", sections = "benchmark")
  refresh <- refresh %||%
    policy_selector_config_value(cfg, "refresh", sections = "benchmark") %||%
    FALSE
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = "benchmark") %||%
    FALSE
  policy_params <- merge_cfg(
    (((cfg$policies) %||% list())$policy_params %||% list()),
    merge_cfg(
      list(
        equation_branch_filters = policy_selector_config_value(
          cfg, "equation_branch_filters",
          sections = "policies"
        )
      ),
      policy_params
    )
  )
  ordination_context <- if (length(object@candidates@ordination) == 0) {
    list(model_scores = NULL, species_lookup = NULL)
  } else {
    list(
      model_scores = ((object@candidates@ordination)$model)$model_scores %||% NULL,
      species_lookup = (object@candidates@ordination)$species_lookup %||% NULL
    )
  }
  model_scores <- model_scores %||% ordination_context$model_scores
  species_lookup <- species_lookup %||% ordination_context$species_lookup

  if (is.null(reference_ids) && nrow(object@candidates@reference_anchors) > 0) {
    if ("model_id_chr" %in% names(object@candidates@reference_anchors)) {
      reference_ids <- as.character(object@candidates@reference_anchors$model_id_chr)
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
    config = policy_selector_anchor_config(object, config = cfg),
    include_ts_error = include_ts_error,
    benchmark_schemes = benchmark_schemes,
    workers = workers,
    package_dir = package_dir,
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

#' Calibrate `PolicySelector` uncertainty
#'
#' @name calibrate_uncertainty.PolicySelector
#' @usage NULL
#'
#' @param object A [PolicySelector] object.
#' @param alpha Optional conformal alpha.
#' @param policy_perf Optional policy-performance table override.
#' @param species_performance_table Optional species-block performance table
#'   override.
#' @param ts_error Optional time-series error table override.
#' @param pseudo_label Pseudo-anchor benchmark label.
#' @param species_label Species-block benchmark label.
#' @param config Optional config override.
#' @param cache_path Optional cache path.
#' @param refresh Optional logical scalar controlling cache refresh.
#' @param progress Optional logical scalar controlling progress messages.
#'
#' @return An updated [PolicySelector] object with stored uncertainty results.
#'
#' Calibrate `PolicySelector` uncertainty from the stored benchmark tables and
#' attach the resulting calibration bundle to the selector.
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
  cfg <- merge_cfg(object@config, policy_selector_config_data(config))
  if (length(object@benchmark) == 0) {
    stop("No benchmark results are stored on this `PolicySelector`.", call. = FALSE)
  }
  benchmark_obj <- object@benchmark
  # Reuse the stored benchmark tables unless the caller supplied explicit
  # overrides for conformal calibration.
  cache_path <- cache_path %||%
    policy_selector_config_value(cfg, "cache_path", sections = c("uncertainty", "selection", "post_selection_conformal"))
  refresh <- refresh %||%
    policy_selector_config_value(cfg, "refresh", sections = c("uncertainty", "selection", "post_selection_conformal")) %||%
    FALSE
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = c("uncertainty", "selection", "post_selection_conformal")) %||%
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
      sections = c("selection", "uncertainty", "post_selection_conformal", "conformal")
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

#' Select policies from a `PolicySelector`
#'
#' @name select_policies.PolicySelector
#' @usage NULL
#'
#' @param object A [PolicySelector] object.
#' @param species_performance_table Optional species-block performance table
#'   override.
#' @param config Optional config override.
#' @param cache_path Optional cache path.
#' @param refresh Optional logical scalar controlling cache refresh.
#' @param progress Optional logical scalar controlling progress messages.
#'
#' @return An updated [PolicySelector] object with stored selection results.
#'
#' Select globally acceptable policies from the `PolicySelector` benchmark
#' table and store the resulting summaries on the selector.
S7::method(select_policies, PolicySelector) <- function(object,
                                                        species_performance_table = NULL,
                                                        config = NULL,
                                                        cache_path = NULL,
                                                        refresh = NULL,
                                                        progress = NULL) {
  cfg <- merge_cfg(object@config, policy_selector_config_data(config))
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
#'
#' @return A [PolicyPredictions] object.
#'
#' Predict anchor-level policy outputs from a PolicySelector
#'
#' Scores each reference anchor under the active policy set, joins benchmark and
#' uncertainty summaries, and returns a [PolicyPredictions] bundle.
#'
#' @param object A [PolicySelector] object.
#' @param reference_anchors Optional anchor table override.
#' @param policies Optional character vector of policy names.
#' @param config Optional config override.
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
#' @param progress Optional logical scalar controlling stage messages.
#'
.predict_policy_selector <- function(object,
                                     reference_anchors = NULL,
                                     policies = NULL,
                                     config = NULL,
                                     policy_params = list(),
                                     policy_path = NULL,
                                     registry_path = NULL,
                                     learner = NULL,
                                     use_support_bin_intervals = NULL,
                                     max_selection_tolerance = NULL,
                                     reuse_admissibility = TRUE,
                                     progress = NULL) {
  cfg <- merge_cfg(object@config, policy_selector_config_data(config))
  progress <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = c("selection", "benchmark")) %||%
    FALSE
  policy_params <- merge_cfg(
    (((cfg$policies) %||% list())$policy_params %||% list()),
    merge_cfg(
      list(
        equation_branch_filters = policy_selector_config_value(
          cfg, "equation_branch_filters",
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
  selection_obj <- if ((inherits(learner, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(learner, PolicyLearner), error = function(e) FALSE)))) {
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
    inherits(learner, "S7_object") &&
    exists("PolicyLearner", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(learner, PolicyLearner), error = function(e) FALSE))) {
    # The meta-learner should score the full benchmarked policy set by default.
    # Keeping the prediction pass pinned to `cfg$policies$active` silently drops
    # benchmarked policy families and can leave anchors with no non-empty donor
    # pool even though valid broad policies exist in the benchmark tables.
    active_policies <- NULL
  }
  ordination_context <- if (length(object@candidates@ordination) == 0) {
    list(model_scores = NULL, species_lookup = NULL)
  } else {
    list(
      model_scores = ((object@candidates@ordination)$model)$model_scores %||% NULL,
      species_lookup = (object@candidates@ordination)$species_lookup %||% NULL
    )
  }
  model_scores <- ordination_context$model_scores
  species_lookup <- ordination_context$species_lookup
  conf_tbl <- tibble::as_tibble(uncertainty_obj$conf_cal %||% tibble::tibble())
  select_tbl <- tibble::as_tibble(selection_obj$final_ref %||% tibble::tibble())
  benchmark_policy_tbl <- tibble::as_tibble((object@benchmark)$policy_perf %||% tibble::tibble())
  if (nrow(select_tbl) == 0L &&
    nrow(tibble::as_tibble((object@benchmark)$species_block_perf %||% tibble::tibble())) > 0L) {
    # Older selector checkpoints can reach prediction with uncertainty already
    # calibrated but without a persisted global selection summary. Rebuild the
    # benchmark-derived selection reference table on demand so anchor-time
    # scoring still has access to the one-SE acceptability and bootstrap
    # stability diagnostics.
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
  infer_branch_filters <- function(tbl) {
    tbl <- tibble::as_tibble(tbl)
    unique(standardize_branches(tbl))
  }
  if (inherits(learner, "S7_object") &&
    exists("PolicyLearner", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(learner, PolicyLearner), error = function(e) FALSE))) {
    # The meta-learner must score every benchmarked policy that could plausibly
    # produce a donor set for the anchor. Restricting prediction to the
    # deterministic `final_ref` subset can silently drop broad policy families
    # and leave some anchors with only zero-donor rows.
    if ((is.null(active_policies) || length(active_policies) == 0) &&
      length(object@benchmark) > 0) {
      active_policies <- infer_policy_bases((object@benchmark)$policy_perf %||% tibble::tibble())
      if (is.null(policy_params$equation_branch_filters) &&
        nrow((object@benchmark)$policy_perf %||% tibble::tibble()) > 0) {
        policy_params$equation_branch_filters <- infer_branch_filters(
          (object@benchmark)$policy_perf %||% tibble::tibble()
        )
      }
    }
    if (is.null(active_policies) || length(active_policies) == 0) {
      active_policies <- infer_policy_bases(select_tbl)
      if (is.null(policy_params$equation_branch_filters) && nrow(select_tbl) > 0) {
        policy_params$equation_branch_filters <- infer_branch_filters(select_tbl)
      }
    }
  } else {
    if (is.null(active_policies) || length(active_policies) == 0) {
      active_policies <- infer_policy_bases(select_tbl)
      if (is.null(policy_params$equation_branch_filters) && nrow(select_tbl) > 0) {
        policy_params$equation_branch_filters <- infer_branch_filters(select_tbl)
      }
    }
    if ((is.null(active_policies) || length(active_policies) == 0) &&
      length(object@benchmark) > 0) {
      active_policies <- infer_policy_bases((object@benchmark)$policy_perf %||% tibble::tibble())
      if (is.null(policy_params$equation_branch_filters) &&
        nrow((object@benchmark)$policy_perf %||% tibble::tibble()) > 0) {
        policy_params$equation_branch_filters <- infer_branch_filters(
          (object@benchmark)$policy_perf %||% tibble::tibble()
        )
      }
    }
  }
  active_policies <- active_policies[!is.na(active_policies) & nzchar(active_policies)]
  if (length(active_policies) == 0) {
    stop("No active policies were supplied for prediction.", call. = FALSE)
  }
  if (!is.null(learner) && !(inherits(learner, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(learner, PolicyLearner), error = function(e) FALSE)))) {
    stop("'learner' must be NULL or a `PolicyLearner` object.", call. = FALSE)
  }

  # Resolve all reusable reference tables and scalar thresholds once so the
  # per-anchor loop only performs the work that actually depends on the anchor.
  conf_tbl <- standardize_policies(conf_tbl)
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
  select_tbl <- standardize_policies(select_tbl)
  select_ref_tbl <- if (all(c("policy", "equation_branch_filter") %in% select_cols)) {
    select_tbl |>
      dplyr::select(dplyr::all_of(select_cols))
  } else {
    tibble::tibble()
  }
  learner_meta_ref_tbl <- if ((inherits(learner, "S7_object") &&
    exists("PolicyLearner", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(learner, PolicyLearner), error = function(e) FALSE)))) {
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
        standardize_policies() |>
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
    anchor_id <- if ("model_id_chr" %in% names(anchor_row)) {
      as.character(anchor_row$model_id_chr[[1]])
    } else {
      as.character(anchor_row$model_id[[1]])
    }
    anchor_species <- as.character(anchor_row$species_name[[1]])
    report_progress(
      progress,
      "[Predict]   [", i, "/", n_anchors, "] ", anchor_species, " (", anchor_id, ")"
    )

    eval_obj <- if (isTRUE(reuse_admissibility)) {
      policy_selector_cached_anchor_evaluation(
        object = object,
        anchor_row = anchor_row,
        config_supplied = !is.null(config)
      )
    } else {
      NULL
    }
    if (is.null(eval_obj)) {
      eval_obj <- screen_one_anchor_admissibility(
        anchor_row = anchor_row,
        candidate_models = object@candidates,
        config = cfg,
        registry_path = registry_path
      )
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
      standardize_policies() |>
      dplyr::mutate(
        anchor_model_id = anchor_id,
        anchor_species = anchor_species,
        valid_prediction = is.finite(.data$multiplier_pred) & .data$multiplier_pred > 0
      )

    if (nrow(conf_ref_tbl) > 0) {
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
    # Collapse exact donor-equivalent candidates before scoring or selection.
    # When multiple policy labels resolve to the same realized donor set and
    # therefore the same predicted multiplier/coefficients for this anchor,
    # keep only the most specific representation instead of letting broader
    # aliases compete as distinct options.
    policy_tbl <- canonicalize_equivalent_policy_rows(policy_tbl)

    # Either keep the deterministic globally screened policy row, or hand the
    # anchor-policy table to the meta-policy learner and retain the learner's
    # score-minimizing rows for this anchor.
    if ((inherits(learner, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(learner, PolicyLearner), error = function(e) FALSE)))) {
      policy_tbl <- stats::predict(
        learner,
        policy_tbl,
        use_support_bin_intervals = use_support_bin_intervals,
        max_selection_tolerance = max_selection_tolerance
      )
      selected_row <- policy_tbl |>
        dplyr::filter(.data$valid_prediction, .data$is_selected)
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
      selected_row <- tibble::tibble(
        anchor_model_id = anchor_id,
        anchor_species = anchor_species,
        selected_policy = NA_character_,
        selected_policy_display = NA_character_,
        equation_branch_filter = NA_character_,
        multiplier_pred = NA_real_,
        multiplier_lo = NA_real_,
        multiplier_hi = NA_real_,
        selection_tier = if ((inherits(learner, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(learner, PolicyLearner), error = function(e) FALSE)))) {
          "meta_policy_min_predicted_score"
        } else {
          "deterministic_global_screen"
        }
      )
    }

    consensus_row <- summarize_evaluation(eval_obj) |>
      dplyr::mutate(
        anchor_model_id = anchor_id,
        anchor_species = anchor_species,
        .before = 1
      )

    all_policy_intervals[[length(all_policy_intervals) + 1]] <- policy_tbl
    selected_policy_rows[[length(selected_policy_rows) + 1]] <- selected_row
    consensus_multiplier_rows[[length(consensus_multiplier_rows) + 1]] <- consensus_row
    report_progress(
      progress,
      "[Predict]   Completed [", i, "/", n_anchors, "] ", anchor_species, " (", anchor_id, ")"
    )
  }

  report_progress(progress, "[Predict] Completed predictions for ", n_anchors, " anchor", if (n_anchors != 1L) "s" else "", ".")

  PolicyPredictions(
    intervals = dplyr::bind_rows(all_policy_intervals),
    selections = dplyr::bind_rows(selected_policy_rows),
    consensus = dplyr::bind_rows(consensus_multiplier_rows)
  )
}

#' Predict selected policy intervals
#'
#' @return A [PolicyPredictions] object.
#' @name predict.PolicySelector
#' @usage NULL
S7::method(predict_generic, PolicySelector) <- .predict_policy_selector

methods::setMethod(
  "predict",
  signature(object = "tsbiomass::PolicySelector"),
  function(object,
           reference_anchors = NULL,
           policies = NULL,
           config = NULL,
           policy_params = list(),
           policy_path = NULL,
           registry_path = NULL,
           learner = NULL,
           use_support_bin_intervals = NULL,
           max_selection_tolerance = NULL,
           reuse_admissibility = TRUE,
           progress = NULL,
           ...) {
    .predict_policy_selector(
      object = object,
      reference_anchors = reference_anchors,
      policies = policies,
      config = config,
      policy_params = policy_params,
      policy_path = policy_path,
      registry_path = registry_path,
      learner = learner,
      use_support_bin_intervals = use_support_bin_intervals,
      max_selection_tolerance = max_selection_tolerance,
      reuse_admissibility = reuse_admissibility,
      progress = progress
    )
  }
)

# Base `predict()` falls through `UseMethod()` for these S7 class strings in an
# installed package, so provide an explicit S3 bridge on the literal class name.
`predict.tsbiomass::PolicySelector` <- function(object,
                                                reference_anchors = NULL,
                                                policies = NULL,
                                                config = NULL,
                                                policy_params = list(),
                                                policy_path = NULL,
                                                registry_path = NULL,
                                                learner = NULL,
                                                use_support_bin_intervals = NULL,
                                                max_selection_tolerance = NULL,
                                                reuse_admissibility = TRUE,
                                                progress = NULL,
                                                ...) {
  .predict_policy_selector(
    object = object,
    reference_anchors = reference_anchors,
    policies = policies,
    config = config,
    policy_params = policy_params,
    policy_path = policy_path,
    registry_path = registry_path,
    learner = learner,
    use_support_bin_intervals = use_support_bin_intervals,
    max_selection_tolerance = max_selection_tolerance,
    reuse_admissibility = reuse_admissibility,
    progress = progress
  )
}

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
S7::method(print_generic, PolicySelector) <- function(x, ...) {
  benchmark_rows <- nrow(tibble::as_tibble((x@benchmark)$policy_perf %||% tibble::tibble()))
  uncertainty_rows <- nrow(tibble::as_tibble((x@uncertainty)$conf_cal %||% tibble::tibble()))
  selection_rows <- nrow(tibble::as_tibble((x@selection)$final_ref %||% tibble::tibble()))

  cat("PolicySelector\n")
  cat("  candidate_models: ", nrow(x@candidates@candidate_models), "\n", sep = "")
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
S7::method(show_generic, PolicyPredictions) <- function(object) {
  print(object)
  invisible(object)
}

#' Plot a `PolicySelector`
#'
#' Uses the package's S7 method on [base::plot()] so selector-stage
#' summaries can be drawn directly from the staged object.
#'
#' @name plot.PolicySelector
#'
#' @param x A [PolicySelector] object.
#' @param y Unused.
#' @param type Figure family to draw.
#' @param ... Unused additional arguments.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' selector <- benchmark(as_policy_selector(candidates))
#' plot(selector, type = "policy_benchmark")
#' plot(selector, type = "strategy_error_heatmap")
#' }
.plot_policy_selector <- function(x,
                                  y = NULL,
                                  type = c(
                                    "strategy_error_heatmap",
                                    "conformal_scores",
                                    "policy_benchmark",
                                    "species_policy_ranked"
                                  ),
                                  ...) {
  type <- match.arg(type)

  if (type %in% c("strategy_error_heatmap", "policy_benchmark", "species_policy_ranked")) {
    if (length(x@benchmark) == 0) {
      stop(
        "No benchmark results are stored on this `PolicySelector`. Run `benchmark()` first.",
        call. = FALSE
      )
    }
    if (identical(type, "policy_benchmark")) {
      return(plot_policy_boxplot((x@benchmark)$policy_perf %||% tibble::tibble()))
    }
    if (identical(type, "species_policy_ranked")) {
      return(plot_species_policy_ranked((x@benchmark)$species_block_perf %||% tibble::tibble()))
    }
    return(plot_policy_heatmap((x@benchmark)$species_block_perf %||% tibble::tibble()))
  }

  if (length(x@uncertainty) == 0) {
    stop(
      "No uncertainty results are stored on this `PolicySelector`. Run `calibrate_uncertainty()` first.",
      call. = FALSE
    )
  }
  plot_conformal_scores((x@uncertainty)$conf_cal %||% tibble::tibble())
}

S7::method(plot_generic, PolicySelector) <- .plot_policy_selector

`plot.tsbiomass::PolicySelector` <- function(x, y = NULL, ...) {
  .plot_policy_selector(x, y, ...)
}

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
.plot_policy_predictions <- function(x,
                                     y = NULL,
                                     type = c("selected_intervals", "strategy_competition"),
                                     anchor_species = NULL,
                                     reference_name = NULL,
                                     ...) {
  type <- match.arg(type)

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

S7::method(plot_generic, PolicyPredictions) <- .plot_policy_predictions

`plot.tsbiomass::PolicyPredictions` <- function(x, y = NULL, ...) {
  .plot_policy_predictions(x, y, ...)
}
