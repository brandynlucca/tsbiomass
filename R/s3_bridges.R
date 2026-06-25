#' @importFrom stats predict simulate
#' @export
`print.tsbiomass::Alchemist` <- function(x, ...) {
  cat("Alchemist\n")
  cat("  candidates:       ", nrow(x@candidates@candidate_models), " models\n", sep = "")
  cat("  learner_ready:    ", if (length(x@learner) > 0L) "yes" else "no", "\n", sep = "")
  cat("  distances_ready:  ", if (length(x@distance_matrix) > 0L) "yes" else "no", "\n", sep = "")
  cat("  importance_ready: ", if (length(x@trait_importance) > 0L) "yes" else "no", "\n", sep = "")
  invisible(x)
}

#' @export
`print.tsbiomass::Candidates` <- function(x, ...) {
  cat("Candidates\n")
  cat("  study_rows: ", nrow(x@study_db), "\n", sep = "")
  cat("  species_count: ", length(x@species_vector), "\n", sep = "")
  cat("  candidate_models: ", nrow(x@candidate_models), "\n", sep = "")
  cat("  anchors: ", nrow(x@reference_anchors), "\n", sep = "")
  invisible(x)
}

#' @export
`print.tsbiomass::Configurer` <- function(x, ...) configurer_console_summary(x)

#' @export
`print.tsbiomass::Conjurer` <- function(x, ...) {
  cfg <- tryCatch(conjurer_analysis_config(x), error = function(e) NULL)
  configured_draws <- suppressWarnings(as.integer(cfg$n_draws %||% NA_integer_))
  reference_species <- if (nrow(x@summary) > 0 && "anchor_species" %in% names(x@summary)) {
    x@summary$anchor_species
  } else {
    anchors_tbl <- tibble::as_tibble(x@selector@candidates@reference_anchors)
    if ("anchor_species" %in% names(anchors_tbl)) {
      anchors_tbl$anchor_species
    } else if ("species_name" %in% names(anchors_tbl)) {
      anchors_tbl$species_name
    } else {
      character(0)
    }
  }
  reference_species <- unique(as.character(reference_species))
  reference_species <- reference_species[!is.na(reference_species) & nzchar(reference_species)]
  trait_values <- if (nrow(x@manifest) > 0 && "trait" %in% names(x@manifest)) {
    x@manifest$trait
  } else if (!is.null(cfg)) {
    names(cfg$study_weights %||% numeric(0))
  } else {
    character(0)
  }
  stage_label <- if (nrow(x@summary) > 0 || nrow(x@draws) > 0 || nrow(x@manifest) > 0) {
    "simulated"
  } else {
    "staged"
  }
  trait_status <- {
    manifest_tbl <- tibble::as_tibble(x@manifest)
    if (nrow(manifest_tbl) == 0 || !"status" %in% names(manifest_tbl)) {
      "none"
    } else {
      status_tbl <- dplyr::arrange(dplyr::count(manifest_tbl, status, name = "n_traits"), dplyr::desc(n_traits), status)
      paste0(status_tbl$status, "=", status_tbl$n_traits, collapse = ", ")
    }
  }
  cat("Conjurer\n")
  cat("  stage: ", stage_label, "\n", sep = "")
  cat("  reference_species: ", preview_values(reference_species), "\n", sep = "")
  cat("  analyzed_traits: ", preview_values(trait_values), "\n", sep = "")
  cat("  draws_per_trait: ", if (is.finite(configured_draws)) configured_draws else "unknown", "\n", sep = "")
  cat("  trait_status: ", trait_status, "\n", sep = "")
  cat("  largest_mean_abs_db_shift: ", conjurer_top_signal(x@summary, "mean_abs_db_shift", digits = 2), "\n", sep = "")
  cat("  largest_policy_switch_rate: ", conjurer_top_signal(x@summary, "switch_rate_vs_baseline", percent = TRUE), "\n", sep = "")
  invisible(x)
}

#' @export
`print.tsbiomass::PolicyLearner` <- function(x, ...) {
  fitted_method <- as.character((x@fitted_model)$method %||% "none")[[1]]
  calibration_rows <- nrow(tibble::as_tibble((x@calibration)$selected %||% tibble::tibble()))
  cat("PolicyLearner\n")
  cat("  training_rows: ", nrow(x@training_data), "\n", sep = "")
  cat("  fitted_method: ", fitted_method, "\n", sep = "")
  cat("  calibration_rows: ", calibration_rows, "\n", sep = "")
  invisible(x)
}

#' @export
`print.tsbiomass::PolicyPredictions` <- function(x, ...) {
  cat("PolicyPredictions\n")
  cat("  intervals: ", nrow(x@intervals), "\n", sep = "")
  cat("  selections: ", nrow(x@selections), "\n", sep = "")
  cat("  consensus: ", nrow(x@consensus), "\n", sep = "")
  invisible(x)
}

#' @export
`print.tsbiomass::PolicySelector` <- function(x, ...) {
  cat("PolicySelector\n")
  cat("  candidate_models: ", nrow(x@candidates@candidate_models), "\n", sep = "")
  cat("  anchors: ", nrow(x@candidates@reference_anchors), "\n", sep = "")
  cat("  benchmark_rows: ", nrow(tibble::as_tibble((x@benchmark)$policy_perf %||% tibble::tibble())), "\n", sep = "")
  cat("  selection_rows: ", nrow(tibble::as_tibble((x@selection)$final_ref %||% tibble::tibble())), "\n", sep = "")
  invisible(x)
}

#' @export
`print.tsbiomass::PolicySimulator` <- function(x, ...) {
  cat("PolicySimulator\n")
  cat("  scenario_count: ", length(x@scenarios), "\n", sep = "")
  cat("  result_count: ", length(x@results), "\n", sep = "")
  cat("  manifest_rows: ", nrow(x@manifest), "\n", sep = "")
  invisible(x)
}

#' @export
`print.tsbiomass::Referee` <- function(x, ...) {
  cat("Referee\n")
  cat("  anchors: ", nrow(x@selector@candidates@reference_anchors), "\n", sep = "")
  cat("  prediction_ready: ", if (is.null(x@predictions)) "no" else "yes", "\n", sep = "")
  cat("  selected_rows: ", nrow(x@scorecard@selected), "\n", sep = "")
  invisible(x)
}

#' @export
`print.tsbiomass::Scorecard` <- function(x, ...) scorecard_console_summary(x)

#' @method plot tsbiomass::Alchemist
#' @export
`plot.tsbiomass::Alchemist` <- function(x, y = NULL, ...) .plot_alchemist(x, y, ...)

#' @method plot tsbiomass::Candidates
#' @export
`plot.tsbiomass::Candidates` <- function(x, y = NULL, ...) .plot_candidates(x, y, ...)

#' @method plot tsbiomass::Conjurer
#' @export
`plot.tsbiomass::Conjurer` <- function(x, y = NULL, ...) .plot_conjurer(x, y, ...)

#' @method plot tsbiomass::PolicyLearner
#' @export
`plot.tsbiomass::PolicyLearner` <- function(x, y = NULL, ...) .plot_policy_learner(x, y, ...)

#' @method plot tsbiomass::PolicyPredictions
#' @export
`plot.tsbiomass::PolicyPredictions` <- function(x, y = NULL, ...) .plot_policy_predictions(x, y, ...)

#' @method plot tsbiomass::PolicySelector
#' @export
`plot.tsbiomass::PolicySelector` <- function(x, y = NULL, ...) .plot_policy_selector(x, y, ...)

#' @method plot tsbiomass::PolicySimulator
#' @export
`plot.tsbiomass::PolicySimulator` <- function(x, y = NULL, ...) .plot_policy_simulator(x, y, ...)

#' @method plot tsbiomass::Referee
#' @export
`plot.tsbiomass::Referee` <- function(x, y = NULL, ...) .plot_referee(x, y, ...)

#' @method plot tsbiomass::Scorecard
#' @export
`plot.tsbiomass::Scorecard` <- function(x, y = NULL, ...) .plot_scorecard(x, y, ...)

#' @method predict tsbiomass::PolicyLearner
#' @export
`predict.tsbiomass::PolicyLearner` <- function(object, ...) .predict_policy_learner(object, ...)

#' @method predict tsbiomass::PolicySelector
#' @export
`predict.tsbiomass::PolicySelector` <- function(object, ...) .predict_policy_selector(object, ...)

#' @method predict tsbiomass::Referee
#' @export
`predict.tsbiomass::Referee` <- function(object, ...) .predict_referee(object, ...)

#' @export
`simulate.tsbiomass::Conjurer` <- function(object, ...) .simulate_conjurer(object, ...)

#' @export
`simulate.tsbiomass::PolicySimulator` <- function(object, ...) .simulate_policy_simulator(object, ...)
