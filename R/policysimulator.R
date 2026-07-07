#' Policy Simulator S7 Class
#'
#' `PolicySimulator` wraps the policy-sensitivity pipeline around a
#' [PolicySelector]. It owns the scenario specifications, the rerun benchmark
#' results, and the scenario manifest/tables used for downstream sensitivity
#' summaries.
#'
#' @section Properties:
#' - `selector`: Source [PolicySelector].
#' - `config`: Simulator configuration.
#' - `scenarios`: Scenario specifications.
#' - `results`: Scenario benchmark results.
#' - `manifest`: Scenario manifest table.
#' - `tables`: Collected scenario result tables.
#'
#' The simulator is intentionally selector-centered: it rebuilds sensitivity
#' scenarios from the selector's current candidate pool and policy/admissibility
#' settings, then stores the scenario reruns for later inspection.
#'
#' @examples
#' \dontrun{
#' selector <- as_policyselector(candidates)
#' selector <- benchmark(selector)
#' selector <- calibrate_uncertainty(selector)
#' selector <- select_policies(selector)
#'
#' simulator <- as_policysimulator(selector)
#' simulator <- simulate(simulator)
#' construct_sensitivity_table(simulator)
#' collect_sensitivity_results(simulator)
#' }
#'
#' @name PolicySimulator-class
#' @usage NULL
#' @aliases PolicySimulator
NULL

PolicySimulator <- S7::new_class(
  "PolicySimulator",
  properties = list(
    selector = S7::new_property(S7::class_any),
    config = S7::new_property(S7::class_list),
    scenarios = S7::new_property(S7::class_list),
    results = S7::new_property(S7::class_list),
    manifest = S7::new_property(CandidatesDataFrame),
    tables = S7::new_property(S7::class_list)
  ),
  validator = function(self) {
    tryCatch(
      {
        if (!is_s7_instance(self@selector, "PolicySelector")) {
          return("`selector` must be a `PolicySelector` object.")
        }
        if (!is.list(self@config)) {
          return("`config` must be a list.")
        }
        if (!is.list(self@scenarios)) {
          return("`scenarios` must be a list.")
        }
        if (!is.list(self@results)) {
          return("`results` must be a list.")
        }
        if (!is.data.frame(self@manifest)) {
          return("`manifest` must be a data frame.")
        }
        if (!is.list(self@tables)) {
          return("`tables` must be a list.")
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(PolicySimulator)

#' Test whether an object is a `PolicySimulator` instance
#'
#' Rebuild a `PolicySimulator`
#'
#' @param object A [PolicySimulator] object.
#' @param selector Optional replacement [PolicySelector].
#' @param config Optional replacement config list.
#' @param scenarios Optional replacement scenario specification list.
#' @param results Optional replacement scenario benchmark map.
#' @param manifest Optional replacement scenario manifest.
#' @param tables Optional replacement bound scenario tables.
#'
#' @return A `PolicySimulator` object.
#'
#' @keywords internal
#' @noRd
policy_simulator_rebuild <- function(object,
                                     selector = object@selector,
                                     config = object@config,
                                     scenarios = object@scenarios,
                                     results = object@results,
                                     manifest = object@manifest,
                                     tables = object@tables) {
  PolicySimulator(
    selector = selector,
    config = config,
    scenarios = scenarios,
    results = results,
    manifest = tibble::as_tibble(manifest),
    tables = tables
  )
}

#' Build a `PolicySimulator`
#'
#' @param selector A [PolicySelector] object.
#' @param config Optional simulator config list or [Configurer] object.
#'
#' @return A `PolicySimulator` object.
#'
#' @examples
#' \dontrun{
#' simulator <- as_policysimulator(selector)
#' simulator
#' }
#'
#' @export
as_policysimulator <- function(selector,
                               config = NULL) {
  if (!is_s7_instance(selector, "PolicySelector")) {
    stop("'selector' must be a `PolicySelector` object.", call. = FALSE)
  }

  PolicySimulator(
    selector = selector,
    config = policy_selector_config_data(config),
    scenarios = list(),
    results = list(),
    manifest = tibble::tibble(),
    tables = list()
  )
}

#' Resolve the sensitivity benchmark config from a `PolicySimulator`
#'
#' @param object A [PolicySimulator] object.
#' @param config Optional config overrides.
#'
#' @return A list.
#'
#' @keywords internal
#' @noRd
policy_simulator_anchor_config <- function(object,
                                           config = NULL) {
  cfg <- merge_config_sections(
    object@selector@config,
    merge_config_sections(object@config, policy_selector_config_data(config))
  )

  # Start from the selector's policy/admissibility settings, then append the
  # conformal alpha needed by the scenario benchmark reruns.
  merge_config_sections(
    policy_selector_anchor_config(object@selector, config = cfg),
    list(
      conformal_alpha = policy_selector_config_value(
        cfg, "conformal_alpha",
        sections = c("policy", "uncertainty", "conformal")
      ) %||% policy_selector_config_value(
        cfg, "alpha",
        sections = c("uncertainty", "conformal")
      )
    )
  )
}

#' Simulate policy sensitivity scenarios from a PolicySimulator
#'
#' Re-runs the policy benchmark across configured sensitivity scenarios and
#' stores the resulting manifest and bound scenario tables on the object.
#'
#' @param object A [PolicySimulator] object.
#' @param scenario_specifications Optional explicit scenario specification
#'   list.
#' @param baseline_results Optional baseline benchmark bundle.
#' @param policies Optional policy override.
#' @param reference_ids Optional reference-anchor id override.
#' @param benchmark_args Optional additional benchmark arguments.
#' @param workers Optional worker override.
#' @param registry_path Optional trait-registry path.
#' @param config Optional config override.
#' @param cache_path Optional cache path.
#' @param refresh Logical scalar.
#' @param progress Logical scalar.
#'
#' @return An updated [PolicySimulator] object.
#' @name simulate_policy_simulator
.simulate_policy_simulator <- function(object,
                                       scenario_specifications = NULL,
                                       baseline_results = NULL,
                                       policies = NULL,
                                       reference_ids = NULL,
                                       benchmark_args = list(),
                                       workers = NULL,
                                       registry_path = NULL,
                                       config = NULL,
                                       cache_path = NULL,
                                       refresh = NULL,
                                       progress = NULL) {
  cfg <- merge_config_sections(
    object@selector@config,
    merge_config_sections(object@config, policy_selector_config_data(config))
  )
  selector <- object@selector
  sim_cfg <- policy_simulator_anchor_config(object, cfg)
  workers_ <- workers %||%
    policy_selector_config_value(cfg, "workers", sections = "simulation") %||%
    policy_selector_config_value(cfg, "workers", sections = "benchmark") %||%
    1L
  cache_path_ <- cache_path %||%
    policy_selector_config_value(cfg, "cache_path", sections = "simulation")
  refresh_ <- refresh %||%
    policy_selector_config_value(cfg, "refresh", sections = "simulation") %||%
    FALSE
  progress_ <- progress %||%
    policy_selector_config_value(cfg, "progress", sections = "simulation") %||%
    FALSE
  report_progress(progress_, "Running policy sensitivity scenarios.")

  # Build the scenario set from the selector's current candidate-model table
  # unless the caller supplied an explicit scenario list.
  scenario_specifications_ <- scenario_specifications %||% build_policy_sensitivity_scenarios(
    candidate_models = selector@candidates@candidate_models,
    config = sim_cfg
  )

  # Default the baseline bundle to the selector's currently stored benchmark
  # stack so only the non-baseline scenarios need to be recomputed.
  baseline_results_ <- baseline_results %||% {
    if (length(selector@benchmark) == 0) {
      stop("No benchmark results are stored on this `PolicySelector`.", call. = FALSE)
    }
    if (length(selector@uncertainty) == 0) {
      stop("No uncertainty calibration is stored on this `PolicySelector`.", call. = FALSE)
    }
    if (length(selector@selection) == 0) {
      stop("No policy-selection summary is stored on this `PolicySelector`.", call. = FALSE)
    }

    ordination_obj <- selector@candidates@ordination
    anchor_selected <- selector@selection$anchor_selected %||%
      selector@selection$selections %||%
      tibble::tibble()
    if (nrow(tibble::as_tibble(anchor_selected)) == 0) {
      anchor_selected <- tryCatch(
        stats::predict(selector, reuse_admissibility = TRUE)@selections,
        error = function(e) tibble::tibble()
      )
    }
    list(
      ord_ctx = list(
        model_scores = ordination_obj$model$model_scores %||% NULL,
        species_lookup = ordination_obj$species_lookup %||% NULL,
        points_missing_df = ordination_obj$model$points_missing %||% tibble::tibble()
      ),
      policy_perf = selector@benchmark$policy_perf %||% tibble::tibble(),
      species_block_perf = selector@benchmark$species_block_perf %||% tibble::tibble(),
      conf_cal = selector@uncertainty$conf_cal %||% tibble::tibble(),
      selection_ref = selector@selection$final_ref %||% tibble::tibble(),
      anchor_selected = anchor_selected,
      equivalence_pairs = selector@selection$equiv_ref$pairs %||% tibble::tibble(),
      equivalence_classes = selector@selection$equiv_sets %||% tibble::tibble()
    )
  }
  reference_ids_ <- reference_ids %||% {
    anchors_tbl <- selector@candidates@reference_anchors
    if (!is.data.frame(anchors_tbl) || nrow(anchors_tbl) == 0) {
      NULL
    } else if ("model_id" %in% names(anchors_tbl)) {
      as.character(anchors_tbl$model_id)
    } else if ("model_id" %in% names(anchors_tbl)) {
      as.character(anchors_tbl$model_id)
    } else {
      NULL
    }
  }
  active_policies <- policy_selector_active_policies(cfg, policies)

  scenario_results <- run_sensitivity_tests(
    sensitivity_specs = scenario_specifications_,
    benchmark_fun = run_policy_sensitivity_reference,
    baseline_obj = baseline_results_,
    benchmark_args = c(
      list(
        policies = active_policies,
        reference_ids = reference_ids_,
        candidate_template = list(
          spec = selector@candidates@spec,
          study_db = selector@candidates@study_db,
          species_vector = selector@candidates@species_vector,
          source_dbs = selector@candidates@source_dbs,
          species_db = selector@candidates,
          similarity_tuning = selector@candidates@similarity_tuning
        ),
        reference_anchors = selector@candidates@reference_anchors,
        include_ts_error = FALSE,
        registry_path = registry_path,
        progress = progress_
      ),
      benchmark_args
    ),
    workers = workers_,
    config = cfg,
    cache_path = cache_path_,
    refresh = refresh_,
    progress = progress_
  )

  # Collapse the scenario reruns into the standard manifest and bound
  # reference-table bundles retained on the simulator.
  manifest_tbl <- construct_sensitivity_table(
    scenario_specifications = scenario_specifications_,
    scenario_results = scenario_results,
    config = cfg
  )
  bound_tables <- collect_sensitivity_results(scenario_results)
  report_progress(progress_, "Completed policy sensitivity scenarios.")

  policy_simulator_rebuild(
    object,
    config = cfg,
    scenarios = scenario_specifications_,
    results = scenario_results,
    manifest = manifest_tbl,
    tables = bound_tables
  )
}

#' Register the `PolicySimulator` simulate method
#'
#' @name simulate_policy_simulator
#' @usage NULL
#'
#' @keywords internal
#' @noRd
S7::method(simulate_generic, PolicySimulator) <- .simulate_policy_simulator

#' Print a `PolicySimulator`
#'
#' @name print.PolicySimulator
#' @usage NULL
#'
#' @param x A [PolicySimulator] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @noRd
S7::method(print_generic, PolicySimulator) <- function(x, ...) {
  cat("PolicySimulator\n")
  cat("  scenario_count: ", length(x@scenarios), "\n", sep = "")
  cat("  result_count: ", length(x@results), "\n", sep = "")
  cat("  manifest_rows: ", nrow(x@manifest), "\n", sep = "")
  cat("  table_names: ", preview_values(names(x@tables)), "\n", sep = "")
  invisible(x)
}

#' Show a `PolicySimulator`
#'
#' @name show.PolicySimulator
#' @usage NULL
#'
#' @param object A [PolicySimulator] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
#' @noRd
S7::method(show_generic, PolicySimulator) <- function(object) {
  print(object)
  invisible(object)
}

#' Plot a `PolicySimulator`
#'
#' Uses the package's S7 method on [base::plot()] so stored policy-sensitivity
#' scenario summaries can be drawn directly from the simulator object.
#'
#' @name plot.PolicySimulator
#'
#' @param x A [PolicySimulator] object.
#' @param y Unused.
#' @param type Figure family to draw.
#' @param baseline_label Scenario label treated as the baseline reference.
#' @param ... Unused additional arguments.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' simulator <- simulate(as_policysimulator(selector))
#' plot(simulator)
#' plot(simulator, type = "multiplier_drift")
#' }
.plot_policy_simulator <- function(x,
                                   y = NULL,
                                   type = c(
                                     "sensitivity_overview",
                                     "policy_stability",
                                     "multiplier_drift"
                                   ),
                                   baseline_label = "baseline",
                                   ...) {
  type_ <- match.arg(type)
  sensitivity_tables <- collect_sensitivity_results(x)
  sensitivity_tbl <- tibble::as_tibble(sensitivity_tables$select_ref %||% tibble::tibble())
  if (nrow(sensitivity_tbl) == 0) {
    stop(
      "No scenario policy-selection summaries are stored on this `PolicySimulator`. Run `simulate()` first.",
      call. = FALSE
    )
  }
  baseline_tbl <- sensitivity_tbl |>
    dplyr::filter(as.character(.data$scenario) == as.character(baseline_label))
  if (nrow(baseline_tbl) == 0) {
    stop(
      sprintf("No '%s' rows were stored on this `PolicySimulator` for sensitivity plotting.", baseline_label),
      call. = FALSE
    )
  }

  if (identical(type_, "sensitivity_overview")) {
    sensitivity_summary <- summarize_sensitivity(
      sensitivity_table = sensitivity_tbl,
      baseline_label = baseline_label
    )$summary |>
      tidyr::pivot_longer(
        cols = dplyr::any_of(c(
          "prop_policy_changed",
          "prop_display_changed",
          "prop_equiv_set_changed"
        )),
        names_to = "metric",
        values_to = "value"
      ) |>
      dplyr::mutate(
        scenario_label = .data$scenario,
        panel = "Selection changes",
        metric = dplyr::recode(
          .data$metric,
          prop_policy_changed = "Policy changed",
          prop_display_changed = "Display changed",
          prop_equiv_set_changed = "Equivalent set changed"
        )
      )
    return(plot_sensitivity_overview(sensitivity_summary))
  }

  if (identical(type_, "policy_stability")) {
    anchor_selected_tbl <- tibble::as_tibble(sensitivity_tables$anchor_selected %||% tibble::tibble())
    if (nrow(anchor_selected_tbl) == 0) {
      stop(
        "No anchor-level scenario selections are stored on this `PolicySimulator`. Re-run `simulate()` with the current package code.",
        call. = FALSE
      )
    }
    sens_detail <- summarize_sensitivity(
      sensitivity_table = anchor_selected_tbl,
      baseline_label = baseline_label
    )$detail
    return(plot_policy_stability(
      sens_tbl = sens_detail,
      baseline_tbl = anchor_selected_tbl |>
        dplyr::filter(as.character(.data$scenario) == as.character(baseline_label))
    ))
  }

  if (identical(type, "multiplier_drift")) {
    anchor_selected_tbl <- tibble::as_tibble(sensitivity_tables$anchor_selected %||% tibble::tibble())
    if (nrow(anchor_selected_tbl) == 0) {
      stop(
        "No anchor-level scenario selections are stored on this `PolicySimulator`. Re-run `simulate()` with the current package code.",
        call. = FALSE
      )
    }
    return(plot_multiplier_drift(
      sens_tbl = anchor_selected_tbl |>
        dplyr::filter(as.character(.data$scenario) != as.character(baseline_label)),
      baseline_tbl = anchor_selected_tbl |>
        dplyr::filter(as.character(.data$scenario) == as.character(baseline_label))
    ))
  }

  plot_sensitivity_overview(tibble::tibble())
}

#' Register the `PolicySimulator` plot method
#'
#' @name plot.PolicySimulator
#' @usage NULL
#'
#' @keywords internal
#' @noRd
S7::method(plot_generic, PolicySimulator) <- .plot_policy_simulator
