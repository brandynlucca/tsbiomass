#' Run benchmark scenarios
#'
#' Reruns a caller-supplied benchmark function across a named scenario list and
#' returns a named list of scenario benchmark objects.
#'
#' @param sensitivity_specs Named list of scenario specifications. Each element must
#'   contain at least `candidate_models` and `config`.
#' @param benchmark_fun Benchmark function called once per scenario.
#' @param baseline_obj Optional precomputed baseline benchmark object.
#' @param benchmark_args Optional named list of extra arguments passed to
#'   `benchmark_fun` for every non-baseline scenario.
#' @param workers Number of parallel workers. Use `1` for sequential execution.
#' @param package_dir Optional package source directory used to load the
#'   development package on parallel workers when running from source.
#' @param config Optional JSON path or list with sensitivity settings.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#' @param progress Logical scalar. If `TRUE`, emit lightweight progress updates.
#'
#' @return Named list of benchmark objects.
#'
#' @export
run_sensitivity_tests <- function(sensitivity_specs,
                                  benchmark_fun,
                                  baseline_obj = NULL,
                                  benchmark_args = list(),
                                  workers = 1L,
                                  package_dir = NULL,
                                  config = NULL,
                                  cache_path = NULL,
                                  refresh = FALSE,
                                  progress = FALSE) {
  # Validate the scenario list, benchmark function, and cache settings before
  # entering the rerun loop.
  if (!is.list(sensitivity_specs) || is.null(names(sensitivity_specs)) || any(!nzchar(names(sensitivity_specs)))) {
    stop("'sensitivity_specs' must be a named list.", call. = FALSE)
  }
  if (!is.function(benchmark_fun)) {
    stop("'benchmark_fun' must be a function.", call. = FALSE)
  }
  if (!is.list(benchmark_args)) {
    stop("'benchmark_args' must be a named list.", call. = FALSE)
  }
  if (!is.null(cache_path) &&
    (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(workers) || length(workers) != 1 || !is.finite(workers) || workers < 1) {
    stop("'workers' must be one finite number >= 1.", call. = FALSE)
  }
  if (!is.null(package_dir) &&
    (!is.character(package_dir) || length(package_dir) != 1 || !nzchar(package_dir))) {
    stop("'package_dir' must be NULL or a single non-empty path.", call. = FALSE)
  }
  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("'progress' must be TRUE or FALSE.", call. = FALSE)
  }

  # Reuse a cached sensitivity benchmark map when available unless a refresh
  # was explicitly requested.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Inline the small set of sensitivity defaults here instead of routing them
  # through a separate wrapper function.
  config_values <- merge_cfg(
    list(
      baseline_label = "baseline",
      tolerance = 0.05,
      n_boot = 500L
    ),
    read_similarity_config(config)
  )
  sens_map <- list()

  # Seed the baseline object first when the caller supplied one so the
  # benchmark rerun loop can skip recomputing it.
  if (!is.null(baseline_obj)) {
    sens_map[[config_values$baseline_label]] <- baseline_obj
  }

  # Rerun each non-baseline scenario through the caller-supplied benchmark
  # function.
  scenario_names <- names(sensitivity_specs)
  if (!is.null(baseline_obj)) {
    scenario_names <- setdiff(scenario_names, config_values$baseline_label)
  }
  workers <- as.integer(workers)

  if (workers <= 1L) {
    for (scenario_nm in scenario_names) {
      spec <- sensitivity_specs[[scenario_nm]]
      spec_config <- spec$config %||% spec$cfg
      if (!is.list(spec) || is.null(spec$candidate_models) || is.null(spec_config)) {
        stop(sprintf("Scenario '%s' must contain 'candidate_models' and 'config'.", scenario_nm), call. = FALSE)
      }

      bench_args <- c(
        list(
          candidate_models = spec$candidate_models,
          config = spec_config,
          cfg = spec_config,
          tolerance = config_values$tolerance,
          n_boot = config_values$n_boot
        ),
        benchmark_args
      )
      bench_args <- bench_args[names(bench_args) %in% names(formals(benchmark_fun))]

      sens_map[[scenario_nm]] <- do.call(
        what = benchmark_fun,
        args = bench_args
      )

      if (isTRUE(progress)) {
        base::message(
          "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
          "Sensitivity progress: ", length(sens_map), "/", length(sensitivity_specs),
          " scenarios completed."
        )
      }
    }
  } else if (length(scenario_names) > 0) {
    cluster_obj <- initialize_parallel_cluster(
      workers = workers,
      package_dir = package_dir
    )
    on.exit(parallel::stopCluster(cluster_obj), add = TRUE)

    tsb_cluster_export(
      cluster_obj,
      c(
        "sensitivity_specs",
        "benchmark_fun",
        "benchmark_args",
        "config_values"
      ),
      envir = environment()
    )

    if (isTRUE(progress)) {
      base::message(
        "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
        "Sensitivity reruns running in parallel with ", workers, " workers."
      )
    }

    scenario_chunks <- split(
      scenario_names,
      ceiling(seq_along(scenario_names) / workers)
    )
    processed <- length(sens_map)

    for (scenario_chunk in scenario_chunks) {
      chunk_results <- parallel::parLapplyLB(
        cluster_obj,
        scenario_chunk,
        function(scenario_nm) {
          spec <- sensitivity_specs[[scenario_nm]]
          spec_config <- spec$config %||% spec$cfg
          if (!is.list(spec) || is.null(spec$candidate_models) || is.null(spec_config)) {
            stop(sprintf("Scenario '%s' must contain 'candidate_models' and 'config'.", scenario_nm), call. = FALSE)
          }

          bench_args <- c(
            list(
              candidate_models = spec$candidate_models,
              config = spec_config,
              cfg = spec_config,
              tolerance = config_values$tolerance,
              n_boot = config_values$n_boot
            ),
            benchmark_args
          )
          bench_args <- bench_args[names(bench_args) %in% names(formals(benchmark_fun))]

          list(
            scenario = scenario_nm,
            result = do.call(what = benchmark_fun, args = bench_args)
          )
        }
      )

      for (one_result in chunk_results) {
        sens_map[[one_result$scenario]] <- one_result$result
      }

      processed <- processed + length(scenario_chunk)
      if (isTRUE(progress)) {
        base::message(
          "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
          "Sensitivity progress: ", processed, "/", length(sensitivity_specs),
          " scenarios completed."
        )
      }
    }
  }

  # Cache the full scenario benchmark map only when a cache path was supplied.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(sens_map, cache_path)
  }

  sens_map
}

#' Build a scenario manifest
#'
#' Summarizes the scenario specifications and the corresponding benchmark
#' outputs into one scenario-level manifest table.
#'
#' @param sensitivity_specs Named scenario-specification list.
#' @param ... Method-specific arguments.
#'
#' @return A tibble.
#'
#' @examples
#' \dontrun{
#' simulator <- as_policy_simulator(selector)
#' simulator <- simulate(simulator)
#' build_sensitivity_table(simulator)
#' }
#'
#' @export
build_sensitivity_table <- S7::new_generic("build_sensitivity_table", "sensitivity_specs")

#' Build a scenario manifest from named sensitivity scenario lists
#'
#' @name build_sensitivity_table.default
#' @usage NULL
#' @param sensitivity_specs Named scenario-specification list.
#' @param sensitivity_map Named scenario benchmark map.
#' @param config Optional JSON path or list with sensitivity settings.
S7::method(build_sensitivity_table, S7::class_any) <- function(sensitivity_specs,
                                                               sensitivity_map,
                                                               config = NULL) {
  # Combine the scenario inputs and the benchmark outputs into one compact
  # scenario-level manifest for later audits.
  # Resolve the scenario-summary defaults directly at the call site to avoid
  # an extra config helper with only a few scalars.
  config_values <- merge_cfg(
    list(
      baseline_label = "baseline",
      tolerance = 0.05,
      n_boot = 500L
    ),
    read_similarity_config(config)
  )

  purrr::imap_dfr(sensitivity_specs, function(spec, scenario_nm) {
    bench_obj <- sensitivity_map[[scenario_nm]]
    selection_tbl <- tibble::as_tibble(bench_obj$selection_ref)
    spec_config <- spec$config %||% spec$cfg %||% list()

    # Handle undersized or failed scenarios cleanly so the manifest still
    # records the scenario inputs even when no benchmark reference could be built.
    if (nrow(selection_tbl) == 0) {
      best_policy <- NA_character_
      best_mean_error <- NA_real_
      eq_best <- tibble::tibble(policy = character(0))
    } else {
      selection_tbl$policy <- resolve_policy_names(selection_tbl)
      best_row <- selection_tbl |>
        dplyr::arrange(.data$mean_species_median_abs_log, .data$policy) |>
        dplyr::slice(1)
      best_policy <- best_row$policy[[1]]
      best_mean_error <- best_row$mean_species_median_abs_log[[1]]
      eq_best <- selection_tbl |>
        dplyr::filter(.data$equivalent_to_best_global) |>
        dplyr::arrange(.data$policy)
    }

    tibble::tibble(
      scenario = scenario_nm,
      n_candidate_models = nrow(spec$candidate_models),
      n_species = dplyr::n_distinct(spec$candidate_models$species_name),
      include_generalized_models = if ("is_group_model" %in% names(spec$candidate_models)) any(spec$candidate_models$is_group_model, na.rm = TRUE) else NA,
      alpha = spec_config$alpha %||% NA_real_,
      k_species = spec_config$k_species %||% NA_real_,
      k_study = spec_config$k_study %||% NA_real_,
      frequency_coherence_mode = spec_config$frequency_coherence_mode %||% "numeric",
      min_length_overlap_fraction = spec_config$min_length_overlap_fraction %||% NA_real_,
      min_depth_overlap_fraction = spec_config$min_depth_overlap_fraction %||% NA_real_,
      missing_key_metadata_max_fraction = spec_config$missing_key_metadata_max_fraction %||% NA_real_,
      benchmark_best_policy = best_policy,
      benchmark_best_mean_species_median_abs_log = best_mean_error,
      benchmark_equivalent_best_set = paste(eq_best$policy, collapse = "; "),
      benchmark_equivalent_best_set_n = nrow(eq_best),
      baseline_label = config_values$baseline_label
    )
  })
}

#' Build a scenario manifest from a [PolicySimulator]
#'
#' @name build_sensitivity_table.PolicySimulator
#' @usage NULL
S7::method(build_sensitivity_table, PolicySimulator) <- function(sensitivity_specs,
                                                                 sensitivity_map = NULL,
                                                                 config = NULL) {
  if (nrow(sensitivity_specs@manifest) > 0) {
    return(tibble::as_tibble(sensitivity_specs@manifest))
  }
  if (length(sensitivity_specs@scenarios) == 0 || length(sensitivity_specs@results) == 0) {
    return(tibble::tibble())
  }

  build_sensitivity_table(
    sensitivity_specs = sensitivity_specs@scenarios,
    sensitivity_map = sensitivity_specs@results,
    config = sensitivity_specs@config
  )
}

#' Bind scenario benchmark tables
#'
#' Binds selection, pairwise equivalence, equivalence-class, and conformal
#' tables across all scenario benchmark objects.
#'
#' @param sensitivity_map Named scenario benchmark map or a
#'   [PolicySimulator] object.
#' @param ... Method-specific arguments.
#'
#' @return A list of bound tibbles.
#'
#' @examples
#' \dontrun{
#' simulator <- as_policy_simulator(selector)
#' simulator <- simulate(simulator)
#' bind_sensitivity_data(simulator)
#' }
#'
#' @export
bind_sensitivity_data <- S7::new_generic("bind_sensitivity_data", "sensitivity_map")

#' Bind scenario benchmark tables from a named scenario-result map
#'
#' @name bind_sensitivity_data.default
#' @usage NULL
#' @param sensitivity_map Named scenario benchmark map.
S7::method(bind_sensitivity_data, S7::class_any) <- function(sensitivity_map) {
  # Bind the benchmark reference tables across scenarios while keeping the
  # scenario name as the leading key column.
  list(
    select_ref = purrr::imap_dfr(sensitivity_map, function(obj, scenario_nm) {
      tibble::as_tibble(obj$selection_ref) |>
        dplyr::mutate(scenario = scenario_nm, .before = 1)
    }),
    anchor_selected = purrr::imap_dfr(sensitivity_map, function(obj, scenario_nm) {
      tibble::as_tibble(obj$anchor_selected %||% tibble::tibble()) |>
        dplyr::mutate(scenario = scenario_nm, .before = 1)
    }),
    equiv_pairs = purrr::imap_dfr(sensitivity_map, function(obj, scenario_nm) {
      tibble::as_tibble(obj$equivalence_pairs) |>
        dplyr::mutate(scenario = scenario_nm, .before = 1)
    }),
    equiv_sets = purrr::imap_dfr(sensitivity_map, function(obj, scenario_nm) {
      tibble::as_tibble(obj$equivalence_classes) |>
        dplyr::mutate(scenario = scenario_nm, .before = 1)
    }),
    conf_cal = purrr::imap_dfr(sensitivity_map, function(obj, scenario_nm) {
      tibble::as_tibble(obj$conf_cal) |>
        dplyr::mutate(scenario = scenario_nm, .before = 1)
    })
  )
}

#' Bind scenario benchmark tables from a [PolicySimulator]
#'
#' @name bind_sensitivity_data.PolicySimulator
#' @usage NULL
S7::method(bind_sensitivity_data, PolicySimulator) <- function(sensitivity_map) {
  if (length(sensitivity_map@tables) > 0) {
    return(sensitivity_map@tables)
  }
  if (length(sensitivity_map@results) == 0) {
    return(list())
  }

  bind_sensitivity_data(sensitivity_map@results)
}

#' Summarize policy sensitivity
#'
#' Compares policy-selection outputs against the baseline scenario and
#' summarizes how often the selected policy, display label, and equivalent
#' policy set change. The input may be either:
#' 1. an anchor-level table with one selected row per anchor and scenario, or
#' 2. a scenario-level benchmark reference table with one row per policy and
#'    scenario.
#'
#' @param sensitivity_table Policy-sensitivity table with one row per
#'   anchor-scenario or one row per policy-scenario.
#' @param baseline_label Baseline scenario label.
#'
#' @return A list with `detail` and `summary`.
#'
#' @export
summarize_sensitivity <- function(sensitivity_table,
                                  baseline_label = "baseline") {
  out <- tibble::as_tibble(sensitivity_table)
  if (nrow(out) == 0) {
    return(list(detail = tibble::tibble(), summary = tibble::tibble()))
  }

  # Anchor-level sensitivity tables already contain one selected row per
  # anchor-scenario, so compare each non-baseline row against the baseline row
  # for the same anchor.
  if ("anchor_model_id" %in% names(out)) {
    out$selected_policy <- resolve_selected_policy_values(out)
    out$selected_policy_display <- resolve_selected_policy_names(out)
    out$equivalent_policy_set <- resolve_equivalent_policy_sets(out)

    detail <- out |>
      dplyr::filter(.data$scenario != baseline_label) |>
      dplyr::left_join(
        out |>
          dplyr::filter(.data$scenario == baseline_label) |>
          dplyr::select(
            "anchor_model_id",
            baseline_policy = "selected_policy",
            baseline_display = "selected_policy_display",
            baseline_equiv_set = "equivalent_policy_set"
          ),
        by = "anchor_model_id"
      ) |>
      dplyr::mutate(
        policy_changed = .data$selected_policy != .data$baseline_policy,
        display_changed = .data$selected_policy_display != .data$baseline_display,
        equiv_set_changed = .data$equivalent_policy_set != .data$baseline_equiv_set
      )

    # Summarize the cross-anchor change rates by scenario after the row-level
    # baseline comparison has been computed.
    summary <- detail |>
      dplyr::group_by(.data$scenario) |>
      dplyr::summarise(
        n_anchors = dplyr::n(),
        n_policy_changed = sum(.data$policy_changed, na.rm = TRUE),
        n_display_changed = sum(.data$display_changed, na.rm = TRUE),
        n_equiv_set_changed = sum(.data$equiv_set_changed, na.rm = TRUE),
        prop_policy_changed = mean(.data$policy_changed, na.rm = TRUE),
        prop_display_changed = mean(.data$display_changed, na.rm = TRUE),
        prop_equiv_set_changed = mean(.data$equiv_set_changed, na.rm = TRUE),
        .groups = "drop"
      )

    return(list(detail = detail, summary = summary))
  }

  # Scenario-level benchmark reference tables contain one row per policy and
  # scenario. Reduce each scenario to its best displayed policy and its full
  # equivalent-best set before comparing the scenarios against baseline.
  if (!all(c("scenario", "policy") %in% names(out))) {
    stop(
      "'sensitivity_table' must contain either anchor-level rows with 'anchor_model_id' ",
      "or scenario-level benchmark rows with 'scenario' and 'policy'.",
      call. = FALSE
    )
  }

  out$selected_policy <- resolve_selected_policy_values(out)
  out$selected_policy_display <- resolve_selected_policy_names(out)
  out$equivalent_policy_set <- resolve_equivalent_policy_sets(out)

  scenario_summary <- out |>
    dplyr::group_by(.data$scenario) |>
    dplyr::summarise(
      selected_policy = {
        best_rows <- dplyr::pick(
          dplyr::any_of(c(
            "selected_policy",
            "selected_policy_display",
            "policy",
            "acceptable_global",
            "equivalent_to_best_global",
            "mean_species_median_abs_log",
            "specificity_rank"
          ))
        ) |>
          dplyr::mutate(
            .acceptable_global = dplyr::coalesce(.data$acceptable_global, FALSE),
            .equivalent_to_best_global = dplyr::coalesce(.data$equivalent_to_best_global, FALSE),
            .mean_species_median_abs_log = dplyr::coalesce(.data$mean_species_median_abs_log, Inf),
            .specificity_rank = dplyr::coalesce(.data$specificity_rank, Inf)
          ) |>
          dplyr::arrange(
            dplyr::desc(.data$.acceptable_global),
            dplyr::desc(.data$.equivalent_to_best_global),
            .data$.mean_species_median_abs_log,
            .data$.specificity_rank,
            .data$policy
          ) |>
          dplyr::slice(1)
        dplyr::coalesce(best_rows$selected_policy[[1]], best_rows$policy[[1]])
      },
      selected_policy_display = {
        best_rows <- dplyr::pick(
          dplyr::any_of(c(
            "selected_policy",
            "selected_policy_display",
            "policy",
            "acceptable_global",
            "equivalent_to_best_global",
            "mean_species_median_abs_log",
            "specificity_rank"
          ))
        ) |>
          dplyr::mutate(
            .acceptable_global = dplyr::coalesce(.data$acceptable_global, FALSE),
            .equivalent_to_best_global = dplyr::coalesce(.data$equivalent_to_best_global, FALSE),
            .mean_species_median_abs_log = dplyr::coalesce(.data$mean_species_median_abs_log, Inf),
            .specificity_rank = dplyr::coalesce(.data$specificity_rank, Inf)
          ) |>
          dplyr::arrange(
            dplyr::desc(.data$.acceptable_global),
            dplyr::desc(.data$.equivalent_to_best_global),
            .data$.mean_species_median_abs_log,
            .data$.specificity_rank,
            .data$policy
          ) |>
          dplyr::slice(1)
        dplyr::coalesce(best_rows$selected_policy_display[[1]], best_rows$policy[[1]])
      },
      equivalent_policy_set = {
        equiv_rows <- dplyr::pick(
          dplyr::any_of(c("equivalent_to_best_global", "policy", "equivalence_class_members"))
        )
        if ("equivalence_class_members" %in% names(equiv_rows) &&
          any(is.finite(nchar(as.character(equiv_rows$equivalence_class_members))))) {
          members <- as.character(equiv_rows$equivalence_class_members)
          members <- members[is.finite(nchar(members)) & nzchar(members)]
          members[[1]] %||% NA_character_
        } else {
          equiv_set <- equiv_rows |>
            dplyr::filter(dplyr::coalesce(.data$equivalent_to_best_global, FALSE)) |>
            dplyr::arrange(.data$policy)
          paste(equiv_set$policy, collapse = "; ")
        }
      },
      best_mean_species_median_abs_log = suppressWarnings(
        min(.data$mean_species_median_abs_log, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  scenario_summary$best_mean_species_median_abs_log[
    !is.finite(scenario_summary$best_mean_species_median_abs_log)
  ] <- NA_real_

  detail <- scenario_summary |>
    dplyr::filter(.data$scenario != baseline_label) |>
    dplyr::cross_join(
      scenario_summary |>
        dplyr::filter(.data$scenario == baseline_label) |>
        dplyr::transmute(
          baseline_policy = .data$selected_policy,
          baseline_display = .data$selected_policy_display,
          baseline_equiv_set = .data$equivalent_policy_set,
          baseline_best_mean_species_median_abs_log = .data$best_mean_species_median_abs_log
        )
    ) |>
    dplyr::mutate(
      policy_changed = .data$selected_policy != .data$baseline_policy,
      display_changed = .data$selected_policy_display != .data$baseline_display,
      equiv_set_changed = .data$equivalent_policy_set != .data$baseline_equiv_set
    )

  summary <- detail |>
    dplyr::transmute(
      scenario = .data$scenario,
      n_anchors = 1L,
      n_policy_changed = as.integer(.data$policy_changed),
      n_display_changed = as.integer(.data$display_changed),
      n_equiv_set_changed = as.integer(.data$equiv_set_changed),
      prop_policy_changed = as.numeric(.data$policy_changed),
      prop_display_changed = as.numeric(.data$display_changed),
      prop_equiv_set_changed = as.numeric(.data$equiv_set_changed)
    )

  list(detail = detail, summary = summary)
}

#' Build policy-sensitivity scenarios
#'
#' Builds the standard scenario list used to stress-test policy selection
#' against overlap thresholds, generalized-model availability, missingness, and
#' frequency/coherence settings.
#'
#' @param candidate_models Candidate-model table.
#' @param config Policy/admissibility config list.
#'
#' @return A named list of scenario specifications.
#'
#' @export
build_policy_sensitivity_scenarios <- function(candidate_models,
                                               config) {
  # Normalize the config first so every derived scenario starts from one
  # consistent baseline representation.
  config_values <- read_similarity_config(config)
  models_tbl <- tibble::as_tibble(candidate_models)

  # Use the same key fields as the admissibility screen before any scenario
  # filtering so missingness summaries remain aligned with donor exclusion.
  models_tbl <- screen_missing_metadata(
    candidate_models = models_tbl,
    key_cols = admissibility_key_metadata_cols(config_values)
  )

  # Derive the generalized-model flag on the fly when the prepared model table
  # does not already carry it.
  if (!"is_group_model" %in% names(models_tbl)) {
    has_species <- "species_name" %in% names(models_tbl)
    has_genus <- "genus" %in% names(models_tbl)
    has_species_epithet <- "species" %in% names(models_tbl)

    # Force evalation
    force(has_species)
    force(has_genus)
    force(has_species_epithet)

    models_tbl$is_group_model <- dplyr::case_when(
      has_species & is.na(models_tbl$species_name) ~ TRUE,
      has_species & !nzchar(as.character(models_tbl$species_name)) ~ TRUE,
      has_species & as.character(models_tbl$species_name) == "NA NA" ~ TRUE,
      has_genus & has_species_epithet &
        (is.na(models_tbl$genus) | !nzchar(as.character(models_tbl$genus)) |
          is.na(models_tbl$species) | !nzchar(as.character(models_tbl$species))) ~ TRUE,
      TRUE ~ FALSE
    )
  }

  zero_species_environment <- function(weight_list) {
    out <- weight_list %||% list()
    for (nm in c("temperature_midpoint", "temperature_range", "ocean_basin", "trophic")) {
      if (nm %in% names(out)) {
        out[[nm]] <- 0
      }
    }
    out
  }

  list(
    baseline = list(candidate_models = models_tbl, config = config_values),
    stricter_overlap_0_35 = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        min_length_overlap_fraction = 0.35,
        min_depth_overlap_fraction = 0.35
      ))
    ),
    stricter_overlap_0_50 = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        min_length_overlap_fraction = 0.50,
        min_depth_overlap_fraction = 0.50
      ))
    ),
    no_generalized_models = list(
      candidate_models = models_tbl |>
        dplyr::filter(!dplyr::coalesce(.data$is_group_model, FALSE)),
      config = config_values
    ),
    no_high_missingness_0_10 = list(
      candidate_models = models_tbl |>
        dplyr::filter(dplyr::coalesce(.data$key_metadata_missing_fraction, 0) <= 0.10),
      config = config_values
    ),
    frequency_penalty_soft = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        frequency_coherence_mode = "soft_sqrt"
      ))
    ),
    frequency_penalty_strict = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        frequency_coherence_mode = "strict_squared"
      ))
    ),
    without_species_environment = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        species_traits = zero_species_environment(config_values$species_traits)
      ))
    ),
    without_anchor_length_coherence = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        length_overlap_weight = 0
      ))
    ),
    without_anchor_depth_coherence = list(
      candidate_models = models_tbl,
      config = utils::modifyList(config_values, list(
        depth_overlap_weight = 0
      ))
    )
  )
}

#' Decompose sensitivity variance
#'
#' Computes a two-component variance decomposition from the output of
#' [bind_sensitivity_data()]:
#'
#' - **Between-scenario** (`between_scenario`): per-anchor spread of the
#'   selected log-multiplier across scenarios. Quantifies how much the biomass
#'   correction changes when the analysis pipeline is stressed.
#' - **Within-scenario policy** (`within_scenario_policy`): per-scenario spread
#'   of `mean_species_median_abs_log` across policies. Quantifies how much the
#'   specific policy choice within a fixed scenario matters.
#'
#' A `variance_ratio` column in `summary` expresses the between-scenario SD as
#' a fraction of the within-scenario SD, giving a rough relative importance of
#' "how we set up the analysis" versus "which policy we pick given the setup".
#'
#' @param sens_data Named list returned by [bind_sensitivity_data()], with at
#'   least `anchor_selected` and `select_ref` elements.
#' @param log_multiplier_col Name of the log-scale central prediction column in
#'   `anchor_selected`. Defaults to `"log_multiplier_central"`.
#' @param policy_error_col Name of the policy-level mean error column in
#'   `select_ref`. Defaults to `"mean_species_median_abs_log"`.
#'
#' @return A list with three elements:
#'   - `between_scenario`: per-anchor SD and IQR of log-multiplier across
#'     scenarios.
#'   - `within_scenario_policy`: per-scenario SD and IQR of policy-level mean
#'     error across policies.
#'   - `summary`: one-row summary with global SDs and variance ratio.
#'
#' @export
bind_sensitivity_variance <- function(sens_data,
                                      log_multiplier_col = "log_multiplier_central",
                                      policy_error_col = "mean_species_median_abs_log") {
  anchor_sel <- tibble::as_tibble(sens_data$anchor_selected %||% tibble::tibble())
  select_ref <- tibble::as_tibble(sens_data$select_ref %||% tibble::tibble())

  # Between-scenario component: per-anchor spread of the selected log-multiplier
  # across scenarios. Falls back to log(multiplier_central) when a dedicated
  # log column is absent.
  between_scenario <- if (nrow(anchor_sel) > 0 && "anchor_model_id" %in% names(anchor_sel)) {
    mult_col <- if (log_multiplier_col %in% names(anchor_sel)) {
      log_multiplier_col
    } else if ("multiplier_central" %in% names(anchor_sel)) {
      "multiplier_central_log"
    } else {
      NULL
    }

    if (!is.null(mult_col)) {
      if (mult_col == "multiplier_central_log") {
        anchor_sel$multiplier_central_log <- suppressWarnings(
          log(as.numeric(anchor_sel$multiplier_central))
        )
      }
      anchor_sel |>
        dplyr::filter(is.finite(.data[[mult_col]])) |>
        dplyr::group_by(.data$anchor_model_id) |>
        dplyr::summarise(
          n_scenarios = dplyr::n_distinct(.data$scenario),
          between_sd_log_multiplier = stats::sd(.data[[mult_col]], na.rm = TRUE),
          between_iqr_log_multiplier = stats::IQR(.data[[mult_col]], na.rm = TRUE),
          between_range_log_multiplier = suppressWarnings(
            diff(range(.data[[mult_col]], na.rm = TRUE))
          ),
          .groups = "drop"
        )
    } else {
      tibble::tibble()
    }
  } else {
    tibble::tibble()
  }

  # Within-scenario component: per-scenario spread of policy-level mean error.
  within_scenario_policy <- if (nrow(select_ref) > 0 &&
    all(c("scenario", policy_error_col) %in% names(select_ref))) {
    select_ref |>
      dplyr::filter(is.finite(.data[[policy_error_col]])) |>
      dplyr::group_by(.data$scenario) |>
      dplyr::summarise(
        n_policies = dplyr::n_distinct(.data$policy),
        within_sd_policy_error = stats::sd(.data[[policy_error_col]], na.rm = TRUE),
        within_iqr_policy_error = stats::IQR(.data[[policy_error_col]], na.rm = TRUE),
        within_range_policy_error = suppressWarnings(
          diff(range(.data[[policy_error_col]], na.rm = TRUE))
        ),
        .groups = "drop"
      )
  } else {
    tibble::tibble()
  }

  # One-row global summary with variance ratio for quick reporting.
  global_between_sd <- if (nrow(between_scenario) > 0 &&
    "between_sd_log_multiplier" %in% names(between_scenario)) {
    suppressWarnings(mean(between_scenario$between_sd_log_multiplier, na.rm = TRUE))
  } else {
    NA_real_
  }
  global_within_sd <- if (nrow(within_scenario_policy) > 0 &&
    "within_sd_policy_error" %in% names(within_scenario_policy)) {
    suppressWarnings(mean(within_scenario_policy$within_sd_policy_error, na.rm = TRUE))
  } else {
    NA_real_
  }
  variance_ratio <- if (is.finite(global_between_sd) && is.finite(global_within_sd) && global_within_sd > 0) {
    global_between_sd / global_within_sd
  } else {
    NA_real_
  }
  summary <- tibble::tibble(
    n_scenarios = dplyr::n_distinct(select_ref$scenario %||% character()),
    n_anchors = if (nrow(between_scenario) > 0) nrow(between_scenario) else NA_integer_,
    global_between_sd_log_multiplier = global_between_sd,
    global_within_sd_policy_error = global_within_sd,
    variance_ratio_between_within = variance_ratio
  )

  list(
    between_scenario = between_scenario,
    within_scenario_policy = within_scenario_policy,
    summary = summary
  )
}

#' Rerun one policy benchmark scenario
#'
#' Rebuilds ordination support objects, reruns the benchmark layer, recalibrates
#' conformal uncertainty, and rebuilds the global policy-selection summaries for
#' one scenario-specific candidate-model table.
#'
#' @param candidate_models Scenario-specific candidate-model table.
#' @param config Scenario-specific policy/admissibility config list.
#' @param policies Active policy names.
#' @param reference_ids Optional reference-anchor model IDs.
#' @param tolerance Practical equivalence tolerance.
#' @param n_boot Number of bootstrap resamples used in policy selection.
#' @param candidate_template Optional staged [Candidates] object used to rebuild
#'   anchor-level predictions for each scenario.
#' @param reference_anchors Optional reference-anchor table override used for
#'   scenario prediction.
#' @param registry_path Optional trait-registry path.
#' @param include_ts_error Logical scalar. If `TRUE`, retain the TS-error
#'   benchmark table for the scenario rerun.
#' @param progress Logical scalar. If `TRUE`, show benchmark progress.
#'
#' @return A list containing ordination context, benchmark tables, conformal
#'   calibration, global policy-selection summaries, and anchor-level selected
#'   policy rows for the scenario.
#'
#' @export
run_policy_sensitivity_reference <- function(candidate_models,
                                             config,
                                             policies = NULL,
                                             reference_ids = NULL,
                                             tolerance = 0.05,
                                             n_boot = 500L,
                                             candidate_template = NULL,
                                             reference_anchors = NULL,
                                             registry_path = NULL,
                                             include_ts_error = FALSE,
                                             progress = FALSE) {
  # Rebuild the similarity object for this scenario from its own policy and
  # admissibility settings so the ordination and benchmark layers stay aligned.
  config_values <- read_similarity_config(config)
  candidate_models <- tibble::as_tibble(candidate_models)
  reference_key <- if ("model_id_chr" %in% names(candidate_models)) {
    as.character(candidate_models$model_id_chr)
  } else if ("model_id" %in% names(candidate_models)) {
    as.character(candidate_models$model_id)
  } else {
    character()
  }
  if (!is.null(reference_ids) && length(reference_key) > 0) {
    reference_ids <- intersect(as.character(reference_ids), reference_key)
    if (length(reference_ids) == 0) {
      reference_ids <- NULL
    }
  }
  coherence_config <- list(
    length_coherence = list(method = "overlap", weight = config_values$length_overlap_weight %||% 0),
    depth_coherence = list(method = "overlap", weight = config_values$depth_overlap_weight %||% 0),
    frequency_coherence = list(
      method = config_values$frequency_coherence_mode %||% "numeric",
      weight = config_values$frequency_coherence_weight %||% 0
    )
  )

  similarity_obj <- prepare_similarity_matrix(
    candidate_models = candidate_models,
    species_traits = as.list(config_values$species_traits %||% list()),
    study_traits = as.list(config_values$study_traits %||% list()),
    alpha = config_values$alpha,
    k_species = config_values$k_species,
    k_study = config_values$k_study,
    config = coherence_config,
    registry_path = registry_path
  )

  # Recompute the model-level and species-level ordinations for the scenario so
  # any ordination-dependent policies use a scenario-consistent neighborhood.
  distance_obj <- build_gower_distances(similarity_obj)
  ordination_obj <- run_ordination(
    dist_mat = distance_obj$combined_dist,
    trait_table = candidate_models |>
      dplyr::select(dplyr::all_of(distance_obj$envfit_trait_cols %||% distance_obj$trait_cols))
  )
  ordination_points <- join_ordination_points(
    ordination_points = ordination_obj$points,
    candidate_models = candidate_models,
    reference_ids = reference_ids
  )
  model_scores <- extract_ordination_scores(ordination_points)
  points_missing_df <- add_ordination_missing(
    points_df = ordination_points,
    candidate_models = candidate_models,
    trait_cols = distance_obj$missingness_trait_cols %||% distance_obj$trait_cols
  )

  species_ordination_obj <- run_ordination(
    dist_mat = distance_obj$species_dist,
    trait_table = similarity_obj$species_profiles |>
      dplyr::select(dplyr::all_of(
        intersect(
          distance_obj$envfit_trait_cols %||% distance_obj$trait_cols,
          names(similarity_obj$species_profiles)
        )
      ))
  )
  species_points <- assign_ordination_groups(
    points_df = species_ordination_obj$points |>
      dplyr::rename(species_name = .data$model_id),
    cluster_col = "species_cluster_id"
  )
  species_points <- refine_species_clusters(
    species_points_df = species_points,
    dist_mat = distance_obj$species_dist
  )$points
  species_lookup <- build_species_lookup(
    species_points_df = species_points,
    candidate_models = candidate_models
  )$lookup

  # Rerun the benchmark, conformal calibration, and policy selection from the
  # scenario-specific ordination and candidate-model state.
  benchmark_obj <- run_policy_benchmark(
    candidate_models = candidate_models,
    model_scores = model_scores,
    species_lookup = species_lookup,
    reference_ids = reference_ids,
    policies = policies,
    config = config_values,
    include_ts_error = include_ts_error,
    registry_path = registry_path,
    progress = progress
  )
  conformal_obj <- run_anchor_conformal(
    policy_perf = benchmark_obj$policy_perf,
    species_performance_table = benchmark_obj$species_block_perf,
    ts_error = benchmark_obj$policy_ts_error,
    alpha = config_values$conformal_alpha %||% 0.10
  )
  selection_obj <- run_policy_selection(
    species_performance_table = benchmark_obj$species_block_perf,
    config = list(
      tolerance = tolerance,
      n_boot = n_boot
    )
  )
  anchor_selected <- tibble::tibble()

  # Rebuild one selector from the scenario-specific benchmark state so the
  # sensitivity pipeline retains per-anchor selected policies and multipliers,
  # not just the global cross-species policy ranking.
  template_data <- if ((inherits(candidate_template, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidate_template, Candidates), error = function(e) FALSE)))) {
    list(
      spec = candidate_template@spec,
      study_db = candidate_template@study_db,
      species_vector = candidate_template@species_vector,
      source_dbs = candidate_template@source_dbs,
      species_db = candidate_template@species_db,
      similarity_tuning = candidate_template@similarity_tuning,
      reference_anchors = candidate_template@reference_anchors
    )
  } else if (is.list(candidate_template)) {
    candidate_template
  } else {
    list()
  }

  if (length(template_data) > 0) {
    anchors_tbl <- tibble::as_tibble(reference_anchors %||% template_data$reference_anchors %||% tibble::tibble())
    if (nrow(anchors_tbl) > 0 && !is.null(reference_ids)) {
      anchor_key <- if ("model_id_chr" %in% names(anchors_tbl)) {
        as.character(anchors_tbl$model_id_chr)
      } else if ("model_id" %in% names(anchors_tbl)) {
        as.character(anchors_tbl$model_id)
      } else {
        character()
      }
      if (length(anchor_key) > 0) {
        anchors_tbl <- anchors_tbl[anchor_key %in% as.character(reference_ids), , drop = FALSE]
      }
    }

    scenario_candidates <- Candidates(
      spec = template_data$spec,
      study_db = tibble::as_tibble(template_data$study_db %||% tibble::tibble()),
      species_vector = as.character(template_data$species_vector %||% character()),
      source_dbs = template_data$source_dbs %||% list(),
      species_db = tibble::as_tibble(template_data$species_db %||% tibble::tibble()),
      candidate_models = tibble::as_tibble(candidate_models),
      reference_anchors = anchors_tbl,
      similarity_matrix = similarity_obj,
      gower_distances = distance_obj,
      ordination = list(
        model = list(
          model_scores = tibble::as_tibble(model_scores),
          points_missing = tibble::as_tibble(points_missing_df)
        ),
        species_lookup = species_lookup
      ),
      admissibility = list(),
      similarity_tuning = template_data$similarity_tuning %||% list()
    )
    scenario_selector <- PolicySelector(
      candidates = scenario_candidates,
      config = config_values,
      benchmark = benchmark_obj,
      uncertainty = conformal_obj,
      selection = selection_obj
    )
    anchor_selected <- stats::predict(
      scenario_selector,
      reuse_admissibility = FALSE
    )@selections
  }

  list(
    ord_ctx = list(
      model_scores = model_scores,
      species_lookup = species_lookup,
      points_missing_df = points_missing_df
    ),
    policy_perf = benchmark_obj$policy_perf,
    species_block_perf = benchmark_obj$species_block_perf,
    conf_cal = conformal_obj$conf_cal,
    selection_ref = selection_obj$final_ref,
    anchor_selected = anchor_selected,
    equivalence_pairs = selection_obj$equiv_ref$pairs,
    equivalence_classes = selection_obj$equiv_sets
  )
}
