#' Generalized Configurer S7 Class
#'
#' `Configurer` stores one validated normalized configuration as an S7 object.
#' Callers can supply either a YAML path or a config list. Missing fields are
#' filled from the package defaults during normalization.
#'
#' @section Properties:
#' - `data`: Normalized configuration list.
#' - `base_dir`: Base directory used to resolve relative paths.
#' - `registry_path`: Trait-registry path used for validation, or
#'   `NA_character_` when the packaged registry is used.
#' - `policy_path`: Policy-registry path used for validation, or
#'   `NA_character_` when the packaged registry is used.
#'
#' @examples
#' cfg <- build_configurer(list(
#'   paths = list(
#'     input_file = "input.xlsx",
#'     out_root = "outputs",
#'     cache_dir = "cache",
#'     supplemental_dir = "supplemental",
#'     log_file = "outputs/run.log"
#'   ),
#'   execution = list(
#'     strict_length_pdf = FALSE,
#'     run_multiplier_model = FALSE,
#'     write_log = FALSE
#'   ),
#'   tuning = list(
#'     species_model_limit = 2L,
#'     resamples = 8L
#'   ),
#'   similarity = list(
#'     alpha = 0.8,
#'     kernel_scale = 4,
#'     core_weight_cutoff = 0.8,
#'     conformal_alpha = 0.1,
#'     species_traits = list(genus = 2, family = 1),
#'     study_traits = list(frequency = 1, fao_area = 1),
#'     coherence = list(
#'       length = list(mode = "overlap", weight = 2),
#'       depth = list(mode = "overlap", weight = 3),
#'       frequency = list(mode = "overlap", weight = 2, gap = 60)
#'     )
#'   ),
#'   admissibility = list(
#'     key_metadata_max = 0.25,
#'     coherence = list(
#'       length = list(mode = "overlap", min = 0.25),
#'       depth = list(mode = "overlap", min = 0.25),
#'       frequency = list(mode = "overlap")
#'     )
#'   ),
#'   policies = list(
#'     active = "closest_within_species"
#'   ),
#'   selection = list(
#'     method = "glm"
#'   )
#' ))
#' cfg
#'
#' \dontrun{
#' cfg <- build_configurer("path/to/config.yaml")
#' }
#'
#' @name Configurer-class
#' @aliases Configurer
#' @usage NULL
NULL

#' Resolve config data from supported package objects
#'
#' @param config Config input.
#'
#' @return A config list.
#'
#' @keywords internal
#' @noRd
resolve_config_data <- function(config) {
  # Normalize supported S7 wrappers to one plain config list.
  if (is_s7_instance(config, "Configurer")) {
    return(config@data)
  }
  if (is_s7_instance(config, "Candidates")) {
    return(candidates_config_data(config) %||% list())
  }
  if (is_s7_instance(config, "PolicySelector")) {
    return(config@config %||% list())
  }
  if (is_s7_instance(config, "PolicyLearner")) {
    return(policy_learner_config(config))
  }
  if (is_s7_instance(config, "PolicySimulator")) {
    return(merge_config_sections(
      config@selector@config,
      config@config
    ))
  }
  if (is.list(config)) {
    return(config)
  }

  list()
}

#' Resolve one config value from supported package objects
#'
#' @param config Config input.
#' @param key Setting name.
#' @param sections Optional nested sections searched after the top level.
#'
#' @return The resolved value or `NULL`.
#'
#' @keywords internal
#' @noRd
resolve_config_value <- function(config,
                                 key,
                                 sections = character()) {
  # Search top-level config first, then the requested nested sections.
  config <- resolve_config_data(config)
  if (!is.list(config)) {
    return(NULL)
  }

  if (identical(key, "progress")) {
    return(config$progress %||% (config$execution %||% list())$progress %||% NULL)
  }

  if (!is.null(config[[key]])) {
    return(config[[key]])
  }

  for (section in sections) {
    if (is.list(config[[section]]) && !is.null(config[[section]][[key]])) {
      return(config[[section]][[key]])
    }
  }

  NULL
}

#' Recursively merge config sections
#'
#' @param base_config Base list.
#' @param override_config Override list.
#'
#' @return A recursively merged list.
#'
#' @keywords internal
#' @noRd
merge_config_sections <- function(base_config,
                                  override_config) {
  # Merge nested config lists without dropping unspecified base values.
  if (is.null(override_config)) {
    return(base_config)
  }

  merged_config <- base_config
  for (name in names(override_config)) {
    base_value <- merged_config[[name]]
    override_value <- override_config[[name]]
    merged_config[[name]] <- if (is.list(base_value) && is.list(override_value)) {
      merge_config_sections(base_value, override_value)
    } else {
      override_value
    }
  }
  merged_config
}

#' Validate non-core top-level config sections
#'
#' @param config Config list to inspect.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
validate_extra_config_sections <- function(config) {
  required_sections <- c("paths", "execution", "tuning", "policy", "policies", "selection")
  extra_names <- setdiff(names(config), required_sections)

  # Validate the remaining top-level sections generically so the S7 object can
  # hold the full configuration payload without hard-coding study-specific sections.
  validate_node <- function(x,
                            path_label) {
    if (is.null(x)) {
      return(invisible(NULL))
    }

    if (is.list(x)) {
      if (length(x) > 0) {
        child_names <- names(x)
        if (is.null(child_names)) {
          for (i in seq_along(x)) {
            validate_node(x[[i]], paste0(path_label, "[[", i, "]]"))
          }
          return(invisible(NULL))
        }
        if (any(is.na(child_names)) || any(!nzchar(child_names))) {
          stop(sprintf("Config section '%s' contains invalid unnamed entries.", path_label), call. = FALSE)
        }
        for (nm in child_names) {
          validate_node(x[[nm]], paste(path_label, nm, sep = "."))
        }
      }
      return(invisible(NULL))
    }

    if (is.atomic(x)) {
      return(invisible(NULL))
    }

    stop(
      sprintf(
        "Config section '%s' must contain only nested lists, atomic vectors, or NULL.",
        path_label
      ),
      call. = FALSE
    )
  }

  for (nm in extra_names) {
    validate_node(config[[nm]], nm)
  }

  invisible(NULL)
}

#' Reject retired configuration fields
#'
#' @param config Raw caller-supplied configuration list.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
reject_retired_configuration_fields <- function(config) {
  if (!is.list(config)) {
    return(invisible(NULL))
  }

  retired_fields <- list(
    list(section = "paths", field = "input", replacement = "paths.input_file"),
    list(section = "paths", field = "output_root", replacement = "paths.out_root"),
    list(section = "paths", field = "cache_folder", replacement = "paths.cache_dir"),
    list(section = "paths", field = "support_folder", replacement = "paths.supplemental_dir"),
    list(section = "paths", field = "area_file", replacement = "paths.fao_polygon_csv"),
    list(section = "paths", field = "log_path", replacement = "paths.log_file"),
    list(section = "execution", field = "strict_pdf", replacement = "execution.strict_length_pdf"),
    list(section = "execution", field = "run_multiplier", replacement = "execution.run_multiplier_model"),
    list(section = NULL, field = "policy", replacement = "similarity and admissibility"),
    list(section = NULL, field = "k_species", replacement = "similarity.kernel_scale"),
    list(section = NULL, field = "k_study", replacement = "similarity.kernel_scale"),
    list(section = "similarity", field = "k_species", replacement = "similarity.kernel_scale"),
    list(section = "similarity", field = "k_study", replacement = "similarity.kernel_scale"),
    list(section = "similarity", field = "traits", replacement = "similarity.species_traits and similarity.study_traits"),
    list(section = "similarity", field = "length_mode", replacement = "similarity.coherence.length.mode"),
    list(section = "similarity", field = "depth_mode", replacement = "similarity.coherence.depth.mode"),
    list(section = "similarity", field = "frequency_mode", replacement = "similarity.coherence.frequency.mode"),
    list(section = "similarity", field = "length_weight", replacement = "similarity.coherence.length.weight"),
    list(section = "similarity", field = "depth_weight", replacement = "similarity.coherence.depth.weight"),
    list(section = "similarity", field = "frequency_weight", replacement = "similarity.coherence.frequency.weight"),
    list(section = "similarity", field = "frequency_gap", replacement = "similarity.coherence.frequency.gap"),
    list(section = "admissibility", field = "traits", replacement = "admissibility.species_traits and admissibility.study_traits"),
    list(section = "admissibility", field = "length_mode", replacement = "admissibility.coherence.length.mode"),
    list(section = "admissibility", field = "depth_mode", replacement = "admissibility.coherence.depth.mode"),
    list(section = "admissibility", field = "frequency_mode", replacement = "admissibility.coherence.frequency.mode"),
    list(section = "admissibility", field = "length_overlap_min", replacement = "admissibility.coherence.length.min"),
    list(section = "admissibility", field = "depth_overlap_min", replacement = "admissibility.coherence.depth.min"),
    list(section = "admissibility", field = "frequency_gap", replacement = "admissibility.coherence.frequency.gap"),
    list(section = "selection", field = "tolerance", replacement = "selection.equivalence_tolerance"),
    list(section = "tuning", field = "max_models_per_species", replacement = "tuning.species_model_limit"),
    list(section = "tuning", field = "n_resamples", replacement = "tuning.resamples")
  )

  for (spec in retired_fields) {
    container <- if (is.null(spec$section)) config else config[[spec$section]]
    if (is.list(container) && spec$field %in% names(container)) {
      path <- if (is.null(spec$section)) spec$field else paste(spec$section, spec$field, sep = ".")
      stop(
        sprintf("Configuration field '%s' is not supported. Use '%s'.", path, spec$replacement),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}

#' Normalize one explicit config payload
#'
#' @param config Config list supplied directly by the caller.
#' @param base_dir Base directory used to resolve relative paths.
#' @param registry_path Optional trait-registry path used during validation.
#' @param policy_path Optional policy-registry path used during validation.
#'
#' @return Normalized config list.
#'
#' @keywords internal
#' @noRd
normalize_explicit_config <- function(config,
                                      base_dir = getwd(),
                                      registry_path = NULL,
                                      policy_path = NULL) {
  if (!is.list(config)) {
    stop("'config' must be a list.", call. = FALSE)
  }
  if (!is.character(base_dir) || length(base_dir) != 1 || !nzchar(base_dir)) {
    stop("'base_dir' must be a single non-empty path.", call. = FALSE)
  }

  config <- normalize_similarity_config_shape(config)
  config <- apply_cache_defaults(config)
  config <- normalize_active_policy_names(config)
  config$selection <- normalize_learner_section(config$selection %||% list())
  config$uncertainty <- normalize_learner_section(config$uncertainty %||% list())
  config <- normalize_trait_sections(config)

  validate_config(
    config = config,
    registry_path = registry_path,
    policy_path = policy_path
  )
  validate_extra_config_sections(config)

  # Resolve path fields only after the explicit config has passed structural
  # validation so callers get schema errors before filesystem-normalization
  # errors.
  config$paths$input_file <- path_absolute(
    config$paths$input_file,
    base_dir = base_dir
  )
  config$paths$out_root <- path_absolute(
    config$paths$out_root,
    base_dir = base_dir
  )
  config$paths$cache_dir <- path_absolute(
    config$paths$cache_dir,
    base_dir = base_dir
  )
  if (!is.null(config$paths$log_file) && nzchar(config$paths$log_file)) {
    config$paths$log_file <- path_absolute(
      config$paths$log_file,
      base_dir = base_dir
    )
  } else {
    config$paths$log_file <- NULL
  }
  if (!is.null(config$paths$supplemental_dir)) {
    config$paths$supplemental_dir <- path_absolute(
      config$paths$supplemental_dir,
      base_dir = base_dir
    )
  }
  # Materialize the active config fields used by the current execution layer.
  config$alpha <- config$policy$alpha
  config$kernel_scale <- config$similarity$kernel_scale %||% NULL
  config$k_species <- config$similarity$kernel_scale %||% config$policy$k_species
  config$k_study <- config$similarity$kernel_scale %||% config$policy$k_study
  config$frequency_coherence_mode <- stringr::str_to_lower(
    stringr::str_squish(
      as.character(config$policy$frequency_coherence_mode %||% "overlap")
    )
  )[[1]]
  config$require_same_frequency_label <- config$policy$require_same_frequency_label
  config$max_frequency_gap_khz <- config$policy$max_frequency_gap_khz
  config$min_length_overlap_fraction <- config$policy$min_length_overlap_fraction
  config$min_depth_overlap_fraction <- config$policy$min_depth_overlap_fraction
  config$missing_key_metadata_max_fraction <- config$policy$missing_key_metadata_max_fraction
  config$length_overlap_weight <- config$policy$length_overlap_weight
  config$depth_overlap_weight <- config$policy$depth_overlap_weight
  config$frequency_coherence_weight <- config$policy$frequency_coherence_weight
  config$core_weight_cutoff <- config$policy$core_weight_cutoff
  config$conformal_alpha <- config$policy$conformal_alpha

  config
}

#' @export
Configurer <- S7::new_class(
  "Configurer",
  properties = list(
    data = S7::new_property(S7::class_list),
    base_dir = S7::new_property(S7::class_character, default = "."),
    registry_path = S7::new_property(S7::class_character, default = NA_character_),
    policy_path = S7::new_property(S7::class_character, default = NA_character_)
  ),
  validator = function(self) {
    tryCatch(
      {
        normalize_explicit_config(
          config = self@data,
          base_dir = self@base_dir,
          registry_path = if (length(self@registry_path) == 1 && is.na(self@registry_path)) NULL else self@registry_path,
          policy_path = if (length(self@policy_path) == 1 && is.na(self@policy_path)) NULL else self@policy_path
        )
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(Configurer)

#' Create a `Configurer`
#'
#' @param config A config list or a YAML path.
#' @param base_dir Base directory used to resolve relative paths.
#' @param registry_path Optional trait-registry path used during validation.
#' @param policy_path Optional policy-registry path used during validation.
#'
#' @return A validated [Configurer] object containing the normalized config.
#'
#' @examples
#' cfg <- build_configurer(list(
#'   paths = list(
#'     input_file = "input.xlsx",
#'     out_root = "outputs",
#'     cache_dir = "cache",
#'     supplemental_dir = "supplemental",
#'     log_file = "outputs/run.log"
#'   ),
#'   execution = list(
#'     strict_length_pdf = FALSE,
#'     run_multiplier_model = FALSE,
#'     write_log = FALSE
#'   ),
#'   tuning = list(
#'     species_model_limit = 2L,
#'     resamples = 8L
#'   ),
#'   similarity = list(
#'     alpha = 0.8,
#'     kernel_scale = 4,
#'     core_weight_cutoff = 0.8,
#'     conformal_alpha = 0.1,
#'     species_traits = list(genus = 2, family = 1),
#'     study_traits = list(frequency = 1, fao_area = 1),
#'     coherence = list(
#'       length = list(mode = "overlap", weight = 2),
#'       depth = list(mode = "overlap", weight = 3),
#'       frequency = list(mode = "overlap", weight = 2, gap = 60)
#'     )
#'   ),
#'   admissibility = list(
#'     key_metadata_max = 0.25,
#'     coherence = list(
#'       length = list(mode = "overlap", min = 0.25),
#'       depth = list(mode = "overlap", min = 0.25),
#'       frequency = list(mode = "overlap")
#'     )
#'   ),
#'   policies = list(
#'     active = "closest_within_species"
#'   ),
#'   selection = list(
#'     method = "glm"
#'   )
#' ))
#' cfg
#'
#' @export
build_configurer <- function(config,
                             base_dir = getwd(),
                             registry_path = NULL,
                             policy_path = NULL) {
  if (missing(config) || is.null(config)) {
    stop(
      paste(
        "'config' is required.",
        "Supply a config list or a YAML path."
      ),
      call. = FALSE
    )
  }

  if (is.character(config) &&
    length(config) == 1 &&
    file.exists(config) &&
    grepl("\\.(yaml|yml)$", config, ignore.case = TRUE)) {
    return(configurer_from_yaml(
      path = config,
      base_dir = dirname(path_absolute(config)),
      registry_path = registry_path,
      policy_path = policy_path
    ))
  }

  if (!is.list(config)) {
    stop(
      "'config' must be a config list or a YAML path.",
      call. = FALSE
    )
  }

  normalized_base_dir <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  normalized_config <- normalize_config(
    config = config,
    base_dir = normalized_base_dir,
    registry_path = registry_path,
    policy_path = policy_path
  )

  Configurer(
    data = normalized_config,
    base_dir = normalized_base_dir,
    registry_path = if (is.null(registry_path)) {
      NA_character_
    } else {
      if (!is.character(registry_path) || length(registry_path) != 1 || !nzchar(registry_path)) {
        stop("'registry_path' must be NULL or a single non-empty path.", call. = FALSE)
      }
      path_absolute(registry_path, base_dir = normalized_base_dir)
    },
    policy_path = if (is.null(policy_path)) {
      NA_character_
    } else {
      if (!is.character(policy_path) || length(policy_path) != 1 || !nzchar(policy_path)) {
        stop("'policy_path' must be NULL or a single non-empty path.", call. = FALSE)
      }
      path_absolute(policy_path, base_dir = normalized_base_dir)
    }
  )
}

#' Read a YAML config into `Configurer`
#'
#' @param path Config YAML path.
#' @param base_dir Base directory used to resolve relative paths. When omitted,
#'   the YAML file directory is used.
#' @param registry_path Optional trait-registry path used during validation.
#' @param policy_path Optional policy-registry path used during validation.
#'
#' @return A validated [Configurer] object.
#'
#' @examples
#' \dontrun{
#' cfg <- build_configurer("path/to/config.yaml")
#' cfg
#' }
#'
#' @keywords internal
#' @noRd
configurer_from_yaml <- function(path,
                                 base_dir = dirname(path_absolute(path)),
                                 registry_path = NULL,
                                 policy_path = NULL) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single YAML file path.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("Config file does not exist: %s", path), call. = FALSE)
  }

  build_configurer(
    config = yaml::read_yaml(path),
    base_dir = base_dir,
    registry_path = registry_path,
    policy_path = policy_path
  )
}

#' Build the console summary for a `Configurer`
#'
#' @param x A [Configurer] object.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @noRd
configurer_console_summary <- function(x) {
  # Pull the main sections up front so the summary uses the normalized config.
  data_list <- x@data
  policy_section <- data_list$policy %||% list()
  similarity_section <- data_list$similarity %||% list()
  execution_section <- data_list$execution %||% list()
  benchmark_section <- data_list$benchmark %||% list()
  selection_section <- data_list$selection %||% list()
  uncertainty_section <- data_list$uncertainty %||% list()

  # Resolve the trait and policy fields shown in the summary panel.
  species_traits <- unique(c(
    names(similarity_section$species_traits %||% list()),
    names(policy_section$species_traits %||% list())
  ))
  species_traits <- species_traits[!is.na(species_traits) & nzchar(species_traits)]

  study_traits <- unique(c(
    names(similarity_section$study_traits %||% list()),
    names(policy_section$study_traits %||% list())
  ))
  study_traits <- study_traits[!is.na(study_traits) & nzchar(study_traits)]

  active_policies <- as.character(unlist((data_list$policies %||% list())$active %||% character(0), use.names = FALSE))
  active_policies <- active_policies[!is.na(active_policies) & nzchar(active_policies)]
  slope_classes <- as.character(unlist(
    ((data_list$policies %||% list())$slope_class %||% character(0)),
    use.names = FALSE
  ))
  slope_classes <- slope_classes[!is.na(slope_classes) & nzchar(slope_classes)]

  enabled_sections <- names(data_list)[vapply(data_list, function(value) {
    is.list(value) && length(value) > 0
  }, logical(1))]
  enabled_sections <- enabled_sections[!is.na(enabled_sections) & nzchar(enabled_sections)]

  # Print the condensed config summary in the same order users inspect it.
  cat("Configurer\n")
  cat("  base_dir: ", x@base_dir, "\n", sep = "")
  cat("  input_file: ", data_list$paths$input_file %||% "", "\n", sep = "")
  cat("  out_root: ", data_list$paths$out_root %||% "", "\n", sep = "")
  cat("  cache_dir: ", data_list$paths$cache_dir %||% "", "\n", sep = "")
  cat("  species_traits: ", preview_values(species_traits), "\n", sep = "")
  cat("  study_traits: ", preview_values(study_traits), "\n", sep = "")
  cat("  active_policies: ", preview_values(active_policies), "\n", sep = "")
  cat("  slope_class: ", preview_values(slope_classes), "\n", sep = "")
  cat("  selection_method: ", selection_section$method %||% "none", "\n", sep = "")
  cat("  uncertainty_method: ", uncertainty_section$method %||% "none", "\n", sep = "")
  cat("  alpha: ", policy_section$alpha %||% similarity_section$alpha %||% NA_real_, "\n", sep = "")
  cat("  kernel_scale: ", similarity_section$kernel_scale %||% data_list$kernel_scale %||% NA_real_, "\n", sep = "")
  cat("  strict_length_pdf: ", execution_section$strict_length_pdf %||% NA, "\n", sep = "")
  cat("  refresh_benchmark: ", benchmark_section$refresh %||% NA, "\n", sep = "")
  cat("  sections: ", preview_values(enabled_sections), "\n", sep = "")
  invisible(x)
}

#' Print a `Configurer`
#'
#' @name print.Configurer
#' @usage NULL
#'
#' @param x A [Configurer] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @noRd
S7::method(print_generic, Configurer) <- function(x, ...) {
  configurer_console_summary(x)
}

#' Show a `Configurer`
#'
#' @name show.Configurer
#' @usage NULL
#'
#' @param object A [Configurer] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
#' @noRd
S7::method(show_generic, Configurer) <- function(object) {
  configurer_console_summary(object)
}

#' Extract learner-screening summary rows from scorecards
#'
#' @param scorecards A list of [Scorecard] objects.
#'
#' @return A tibble containing learner-screen summary rows.
#' @keywords internal
#' @noRd
learner_screen_scorecard_rows <- function(scorecards) {
  if (is.null(scorecards)) {
    return(tibble::tibble())
  }
  if (is_s7_instance(scorecards, "Scorecard")) {
    scorecards <- list(scorecards)
  }
  if (!is.list(scorecards)) {
    stop("'scorecards' must be a Scorecard or a list of Scorecard objects.", call. = FALSE)
  }
  rows <- lapply(scorecards, function(scorecard_now) {
    if (!is_s7_instance(scorecard_now, "Scorecard")) {
      stop("All learner-screening scorecards must be `Scorecard` objects.", call. = FALSE)
    }
    diag_rows <- tibble::as_tibble(scorecard_now@selection_diagnostics %||% tibble::tibble())
    if (nrow(diag_rows) == 0L) {
      return(tibble::tibble())
    }
    dplyr::filter(diag_rows, .data$scorecard_type == "learner_screen_summary")
  })
  dplyr::bind_rows(rows)
}

#' Update a `Configurer` from learner-screening scorecards
#'
#' Applies one or more learner-screening [Scorecard] objects to a [Configurer]
#' by replacing the corresponding parent `super_methods` vector with the
#' methods marked `recommended_keep` in each scorecard. Only
#' `selection$super_methods` and `uncertainty$super_methods` are modified;
#' learner method settings, folds, outcomes, and all other config entries are
#' preserved.
#'
#' @name update_learners.Configurer
#' @usage NULL
#'
#' @param object A [Configurer].
#' @param ... One or more [Scorecard] objects.
#' @param scorecards Optional list of [Scorecard] objects.
#' @param stages Optional stage filter. Defaults to all stages represented in
#'   the scorecards.
#' @param progress Logical scalar. If `TRUE`, emit update messages.
#'
#' @return A new [Configurer] with pruned Super Learner method libraries.
S7::method(update_learners, Configurer) <- function(object,
                                                    ...,
                                                    scorecards = list(...),
                                                    stages = NULL,
                                                    progress = TRUE) {
  rows <- learner_screen_scorecard_rows(scorecards)
  if (nrow(rows) == 0L) {
    warning("No learner-screening summary rows were found; returning the Configurer unchanged.", call. = FALSE)
    return(object)
  }
  if (!"stage" %in% names(rows) || !"method" %in% names(rows) || !"recommended_keep" %in% names(rows)) {
    stop("Learner-screening scorecards must contain 'stage', 'method', and 'recommended_keep' columns.", call. = FALSE)
  }

  valid_stages <- c("selection", "uncertainty")
  stages <- stages %||% unique(as.character(rows$stage))
  stages <- intersect(as.character(stages), valid_stages)
  if (length(stages) == 0L) {
    warning("No valid learner-screening stages were supplied; returning the Configurer unchanged.", call. = FALSE)
    return(object)
  }

  updated <- object@data
  for (stage_now in stages) {
    stage_rows <- rows |>
      dplyr::filter(.data$stage == stage_now) |>
      dplyr::arrange(.data$original_order, .data$method)
    if (nrow(stage_rows) == 0L) {
      next
    }
    retained <- stage_rows$method[stage_rows$recommended_keep %in% TRUE]
    retained <- retained[!is.na(retained) & nzchar(retained)]
    retained <- unique(retained)
    if (length(retained) == 0L) {
      warning(
        sprintf(
          "Learner-screening scorecard for '%s' retained no methods; leaving that section unchanged.",
          stage_now
        ),
        call. = FALSE
      )
      next
    }
    original <- as.character(unlist((updated[[stage_now]] %||% list())$super_methods %||% character(), use.names = FALSE))
    dropped <- setdiff(original, retained)
    updated[[stage_now]] <- updated[[stage_now]] %||% list()
    updated[[stage_now]]$super_methods <- retained
    report_progress(
      progress,
      sprintf(
        "Updated %s Super Learner library: retained %d/%d method(s)%s.",
        stage_now,
        length(retained),
        length(original),
        if (length(dropped) > 0L) {
          sprintf("; dropped: %s", paste(dropped, collapse = ", "))
        } else {
          ""
        }
      )
    )
  }

  Configurer(
    data = updated,
    base_dir = object@base_dir,
    registry_path = object@registry_path,
    policy_path = object@policy_path
  )
}


#' Read the packaged trait registry JSON
#'
#' Reads the trait-registry JSON file used by `tsbiomass` and validates the
#' minimal structure required for trait-name and trait-definition lookup.
#'
#' @param registry_path Optional path to a trait-registry JSON file. When not
#'   supplied, the packaged `inst/templates/trait_registry.json` file is used.
#'
#' @return A parsed registry list with `species_traits` and `study_traits`
#'   entries.
#' @keywords internal
#' @noRd
read_trait_registry <- local({
  cache_env <- new.env(parent = emptyenv())
  function(registry_path = NULL) {
    if (is.null(registry_path)) {
      registry_path <- system.file(
        "templates",
        "trait_registry.json",
        package = "tsbiomass"
      )
    }
    if (!nzchar(registry_path) || !file.exists(registry_path)) {
      stop(
        "Trait registry JSON could not be found. Supply 'registry_path' or reinstall the package.",
        call. = FALSE
      )
    }
    cache_key <- normalizePath(registry_path, winslash = "/", mustWork = FALSE)
    if (exists(cache_key, envir = cache_env, inherits = FALSE)) {
      return(get(cache_key, envir = cache_env, inherits = FALSE))
    }
    registry <- validate_trait_registry(read_json_file(registry_path))
    assign(cache_key, registry, envir = cache_env)
    registry
  }
})

#' Return allowed trait names
#'
#' Returns the coded trait names defined in the packaged trait-registry JSON.
#'
#' @param scope Trait scope to return. Use `"species"`, `"study"`, or `"all"`.
#' @param registry_path Optional path to a trait-registry JSON file.
#'
#' @return A character vector of coded trait names.
#'
#' @export
trait_names <- function(scope = c("all", "species", "study"),
                        registry_path = NULL) {
  scope <- match.arg(scope)
  registry <- read_trait_registry(registry_path = registry_path)

  # Pull the requested trait blocks directly from the validated registry.
  species_names <- vapply(
    registry$species_traits,
    function(x) x$coded_name %||% NA_character_,
    character(1)
  )
  study_names <- vapply(
    registry$study_traits,
    function(x) x$coded_name %||% NA_character_,
    character(1)
  )

  out <- switch(scope,
    species = species_names,
    study = study_names,
    all = c(species_names, study_names)
  )

  out[!is.na(out) & nzchar(out)]
}

#' Return one trait definition
#'
#' Looks up a single trait definition by coded name from the packaged
#' trait-registry JSON.
#'
#' @param coded_name Coded trait name to retrieve.
#' @param registry_path Optional path to a trait-registry JSON file.
#'
#' @return A named list describing the requested trait.
#'
#' @export
trait_definition <- function(coded_name, registry_path = NULL) {
  registry <- read_trait_registry(registry_path = registry_path)
  all_traits <- c(registry$species_traits, registry$study_traits)

  # Search the combined trait list once and fail clearly if the trait is not
  # present in the registry.
  matches <- Filter(
    f = function(x) identical(x$coded_name, coded_name),
    x = all_traits
  )

  if (length(matches) == 0) {
    stop(
      sprintf("Trait '%s' was not found in the trait registry.", coded_name),
      call. = FALSE
    )
  }

  if (length(matches) > 1) {
    stop(
      sprintf("Trait '%s' appears multiple times in the trait registry.", coded_name),
      call. = FALSE
    )
  }

  matches[[1]]
}

#' Validate a trait-registry object
#'
#' Checks that a parsed trait registry has the required top-level sections and
#' that each trait object contains the required validation fields.
#'
#' @param registry Parsed registry object.
#'
#' @return The validated registry object.
#'
#' @keywords internal
#' @noRd
validate_trait_registry <- function(registry) {
  # The top-level registry must be a list-like object with separate species and
  # study trait sections.
  if (!is.list(registry)) {
    stop("Trait registry must parse to a list.", call. = FALSE)
  }

  required_sections <- c("species_traits", "study_traits")
  missing_sections <- setdiff(required_sections, names(registry))
  if (length(missing_sections) > 0) {
    stop(
      sprintf(
        "Trait registry is missing required top-level field(s): %s",
        paste(missing_sections, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Validate each section independently so error messages point to the exact
  # part of the registry that needs correction.
  registry$species_traits <- validate_trait_section(
    section = registry$species_traits,
    section_name = "species_traits"
  )
  registry$study_traits <- validate_trait_section(
    section = registry$study_traits,
    section_name = "study_traits"
  )

  registry
}

#' Validate one trait-registry section
#'
#' Ensures every trait object in a registry section contains the required
#' validation fields and uses one of the supported data types.
#'
#' @param section Trait section to validate.
#' @param section_name Name of the trait section for error reporting.
#'
#' @return The validated section.
#'
#' @keywords internal
#' @noRd
validate_trait_section <- function(section, section_name) {
  required_fields <- c(
    "coded_name",
    "display_name",
    "description",
    "data_type",
    "unit",
    "multi_valued",
    "expandable",
    "allowed_values"
  )
  allowed_types <- c("numeric", "categorical", "binary", "set")

  # Each section must be a list of trait objects, even when the list is empty.
  if (!is.list(section)) {
    stop(
      sprintf("Trait registry section '%s' must be a list.", section_name),
      call. = FALSE
    )
  }

  for (i in seq_along(section)) {
    trait <- section[[i]]

    # Each trait must be a named object so downstream lookups can rely on the
    # same structure everywhere.
    if (!is.list(trait)) {
      stop(
        sprintf("Trait entry %d in '%s' must be an object.", i, section_name),
        call. = FALSE
      )
    }

    missing_fields <- setdiff(required_fields, names(trait))
    if (length(missing_fields) > 0) {
      stop(
        sprintf(
          "Trait '%s' in '%s' is missing required field(s): %s",
          trait$coded_name %||% paste0("#", i),
          section_name,
          paste(missing_fields, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # Restrict the accepted data types to the validation model currently used
    # by the package.
    if (!trait$data_type %in% allowed_types) {
      stop(
        sprintf(
          "Trait '%s' in '%s' has unsupported data_type '%s'.",
          trait$coded_name,
          section_name,
          trait$data_type
        ),
        call. = FALSE
      )
    }

    # `allowed_values` must either be NULL or a vector/list so categorical and
    # set-based traits can be validated against it later.
    allowed_values <- trait$allowed_values
    if (!is.null(allowed_values) &&
      !is.atomic(allowed_values) &&
      !is.list(allowed_values)) {
      stop(
        sprintf(
          "Trait '%s' in '%s' has invalid allowed_values; use NULL or an array.",
          trait$coded_name,
          section_name
        ),
        call. = FALSE
      )
    }

    if (!is.null(trait$retain_when_trimming) &&
      (!is.logical(trait$retain_when_trimming) ||
        length(trait$retain_when_trimming) != 1 ||
        is.na(trait$retain_when_trimming))) {
      stop(
        sprintf(
          "Trait '%s' in '%s' has invalid retain_when_trimming; use TRUE, FALSE, or omit it.",
          trait$coded_name,
          section_name
        ),
        call. = FALSE
      )
    }
  }

  section
}

#' Read a JSON file
#'
#' Reads a JSON file into an R list without simplifying nested arrays into data
#' frames. This keeps the trait-registry structure stable and also allows small
#' JSON config files to be layered onto package defaults.
#'
#' @param path Path to a JSON file.
#' @return A parsed list.
#' @keywords internal
#' @noRd
read_json_file <- function(path) {
  # Validate the JSON path before attempting to read it so failures are tied to
  # the supplied file rather than to downstream parsing.
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single JSON file path.", call. = FALSE)
  }

  if (!file.exists(path)) {
    stop(sprintf("JSON file does not exist: %s", path), call. = FALSE)
  }

  # Parse the JSON as a list so trait entries remain object-like rather than
  # collapsing into data frames.
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' Read a similarity-tuning config object
#'
#' Normalizes the optional similarity config input into a list so downstream
#' tuning helpers can consume one consistent representation.
#'
#' @param config Optional JSON path or list.
#'
#' @return A list.
#'
#' @keywords internal
#' @noRd
read_similarity_config <- function(config) {
  # Accept either an in-memory list or a JSON path so the tuner can be used
  # from interactive R code or from a serialized config object.
  if (is.null(config)) {
    return(list())
  }

  if (is.character(config) && length(config) == 1) {
    # Read a JSON file only when the caller passed a scalar path-like string.
    return(read_json_file(config))
  }

  if (is.list(config)) {
    # Treat pre-built lists as already parsed config objects.
    return(config)
  }

  stop("'config' must be NULL, a JSON file path, or a list.", call. = FALSE)
}

#' Read the similarity-trait registry lookups
#'
#' @param registry_path Optional path to the trait-registry JSON.
#'
#' @return A list of species/study registry definitions and name lookups.
#'
#' @keywords internal
#' @noRd
read_similarity_registry <- function(registry_path) {
  # Convert the registry into name-indexed lookups once so later helpers can
  # validate and expand selected traits without repeating this setup.
  registry <- read_trait_registry(registry_path = registry_path)
  species_defs <- registry$species_traits
  study_defs <- registry$study_traits

  # Pull the coded names out once so the preparation code can work with
  # lightweight character vectors instead of walking the registry repeatedly.
  species_names <- vapply(species_defs, function(x) x$coded_name, character(1))
  study_names <- vapply(study_defs, function(x) x$coded_name, character(1))

  list(
    species_defs = species_defs,
    study_defs = study_defs,
    species_names = species_names,
    study_names = study_names,
    species_map = stats::setNames(species_defs, species_names),
    study_map = stats::setNames(study_defs, study_names)
  )
}

#' Normalize the external similarity config surface
#'
#' @param config Config list.
#'
#' @return Config list with both `similarity` and internal `policy`
#'   representations populated.
#'
#' @keywords internal
#' @noRd
normalize_similarity_config_shape <- function(config) {
  if (!is.list(config)) {
    return(config)
  }

  # Materialize the runtime projection from the canonical similarity and
  # admissibility sections.
  similarity <- config$similarity %||% list()
  if (!is.list(similarity)) {
    similarity <- list()
  }
  admissibility <- config$admissibility %||% list()
  if (!is.list(admissibility)) {
    admissibility <- list()
  }

  coherence <- similarity$coherence %||% list()
  length_cfg <- coherence$length %||% list()
  depth_cfg <- coherence$depth %||% list()
  frequency_cfg <- coherence$frequency %||% list()

  similarity$length_mode <- length_cfg$mode %||% "overlap"
  similarity$depth_mode <- depth_cfg$mode %||% "overlap"
  similarity$frequency_mode <- frequency_cfg$mode %||% "overlap"
  similarity$length_weight <- length_cfg$weight %||% NULL
  similarity$depth_weight <- depth_cfg$weight %||% NULL
  similarity$frequency_weight <- frequency_cfg$weight %||% NULL
  similarity$frequency_gap <- frequency_cfg$gap %||% NULL
  similarity$kernel_scale <- similarity$kernel_scale %||% NULL

  admissibility_coherence <- admissibility$coherence %||% list()
  admissibility_length_cfg <- admissibility_coherence$length %||% list()
  admissibility_depth_cfg <- admissibility_coherence$depth %||% list()
  admissibility_frequency_cfg <- admissibility_coherence$frequency %||% list()
  admissibility$species_traits <- admissibility$species_traits %||% character(0)
  admissibility$study_traits <- admissibility$study_traits %||% character(0)
  admissibility$length_mode <- admissibility_length_cfg$mode %||% similarity$length_mode %||% "overlap"
  admissibility$depth_mode <- admissibility_depth_cfg$mode %||% similarity$depth_mode %||% "overlap"
  admissibility$frequency_mode <- admissibility_frequency_cfg$mode %||% similarity$frequency_mode %||% "overlap"
  admissibility$length_overlap_min <- admissibility_length_cfg$min %||% NULL
  admissibility$depth_overlap_min <- admissibility_depth_cfg$min %||% NULL
  admissibility$frequency_gap <- admissibility_frequency_cfg$gap %||% similarity$frequency_gap %||% NULL
  admissibility$key_metadata_max <- admissibility$key_metadata_max %||% NULL

  admissibility_frequency_mode_internal <- switch(stringr::str_to_lower(stringr::str_squish(as.character(admissibility$frequency_mode %||% "overlap")))[[1]],
    overlap = "overlap",
    literal = "literal",
    none = "none",
    "overlap"
  )
  admissibility_exact_frequency <- identical(admissibility_frequency_mode_internal, "literal")

  config$similarity <- similarity
  config$admissibility <- admissibility
  config$policy <- merge_config_sections(
    config$policy %||% list(),
    list(
      alpha = similarity$alpha %||% NULL,
      k_species = similarity$kernel_scale %||% NULL,
      k_study = similarity$kernel_scale %||% NULL,
      frequency_coherence_mode = admissibility_frequency_mode_internal,
      require_same_frequency_label = admissibility_exact_frequency,
      max_frequency_gap_khz = admissibility$frequency_gap %||% NULL,
      min_length_overlap_fraction = admissibility$length_overlap_min %||% NULL,
      min_depth_overlap_fraction = admissibility$depth_overlap_min %||% NULL,
      missing_key_metadata_max_fraction = admissibility$key_metadata_max %||% NULL,
      length_overlap_weight = similarity$length_weight %||% NULL,
      depth_overlap_weight = similarity$depth_weight %||% NULL,
      frequency_coherence_weight = similarity$frequency_weight %||% NULL,
      core_weight_cutoff = similarity$core_weight_cutoff %||% NULL,
      conformal_alpha = similarity$conformal_alpha %||% NULL,
      species_traits = similarity$species_traits %||% list(),
      study_traits = similarity$study_traits %||% list(),
      admissibility_species_traits = admissibility$species_traits %||% character(0),
      admissibility_study_traits = admissibility$study_traits %||% character(0)
    )
  )

  config
}

#' Read packaged cache defaults
#'
#' @param defaults_path Optional JSON override path.
#'
#' @return Named list of cache filenames.
#'
#' @keywords internal
#' @noRd
read_cache_defaults <- function(defaults_path = NULL) {
  if (is.null(defaults_path)) {
    defaults_path <- system.file("templates", "cache_defaults.json", package = "tsbiomass")
  }
  if (!nzchar(defaults_path) || !file.exists(defaults_path)) {
    stop("Cache-defaults JSON could not be found.", call. = FALSE)
  }
  read_json_file(defaults_path)
}

#' Apply cache defaults
#'
#' @param config Config list.
#'
#' @return Config list with cache defaults populated.
#'
#' @keywords internal
#' @noRd
apply_cache_defaults <- function(config) {
  if (!is.list(config)) {
    return(config)
  }

  cache_cfg <- config$cache %||% list()
  if (!is.list(cache_cfg)) {
    cache_cfg <- list()
  }
  defaults <- read_cache_defaults(cache_cfg$defaults_path %||% NULL)
  cache_names <- merge_config_sections(defaults, cache_cfg$names %||% list())
  cache_folder <- config$paths$cache_dir %||%
    cache_cfg$folder %||%
    "cache"
  cache_refresh <- cache_cfg$refresh %||% FALSE

  cache_file <- function(key) {
    file_name <- cache_names[[key]] %||% NULL
    if (is.null(file_name)) {
      return(NULL)
    }
    file.path(cache_folder, file_name)
  }

  config$cache <- merge_config_sections(
    list(
      folder = cache_folder,
      refresh = cache_refresh,
      names = cache_names
    ),
    cache_cfg
  )
  config$cache$folder <- cache_folder

  if (is.list(config$candidates)) {
    source_names <- names(config$candidates$sources %||% list())
    for (nm in source_names) {
      src <- config$candidates$sources[[nm]]
      src_key <- src$type %||% nm
      if (is.null(src$cache_path)) {
        src$cache_path <- cache_file(src_key)
      }
      if (is.null(src$refresh)) {
        src$refresh <- cache_refresh
      }
      config$candidates$sources[[nm]] <- src
    }
    if (is.null(config$candidates$enrich$cache_path)) {
      config$candidates$enrich$cache_path <- cache_file("species_enriched")
    }
    if (is.null(config$candidates$prepare$cache_path)) {
      config$candidates$prepare$cache_path <- cache_file("candidate_models")
    }
    if (is.null(config$candidates$prepare$refresh)) {
      config$candidates$prepare$refresh <- cache_refresh
    }
  }

  if (is.null(config$similarity$cache_path)) {
    config$similarity$cache_path <- cache_file("similarity_tuning")
  }
  if (is.null(config$similarity$refresh)) {
    config$similarity$refresh <- cache_refresh
  }
  if (is.null(config$admissibility$cache_path)) {
    config$admissibility$cache_path <- cache_file("anchor_admissibility")
  }
  if (is.null(config$admissibility$refresh)) {
    config$admissibility$refresh <- cache_refresh
  }
  if (is.null(config$benchmark$cache_path)) {
    config$benchmark$cache_path <- cache_file("policy_benchmark")
  }
  if (is.null(config$benchmark$refresh)) {
    config$benchmark$refresh <- cache_refresh
  }
  if (is.null(config$uncertainty$cache_path)) {
    config$uncertainty$cache_path <- cache_file("policy_conformal")
  }
  if (is.null(config$uncertainty$refresh)) {
    config$uncertainty$refresh <- cache_refresh
  }
  if (is.null(config$selection$cache_path)) {
    config$selection$cache_path <- cache_file("policy_selection")
  }
  if (is.null(config$selection$prediction_cache_path)) {
    config$selection$prediction_cache_path <- cache_file("policy_predictions")
  }
  if (is.null(config$selection$refresh)) {
    config$selection$refresh <- cache_refresh
  }
  if (is.null(config$simulation$cache_path)) {
    config$simulation$cache_path <- cache_file("policy_sensitivity")
  }
  if (is.null(config$simulation$refresh)) {
    config$simulation$refresh <- cache_refresh
  }

  config
}

#' Normalize constructor-driven config policy selections
#'
#' @param config Config list.
#' @param policy_path Optional policy-registry path.
#'
#' @return Config list with canonical active policy names plus any
#'   per-policy branch overrides derived from constructor specifications.
#'
#' @keywords internal
#' @noRd
normalize_active_policy_names <- function(config,
                                          policy_path = NULL) {
  if (!is.list(config) || !is.list(config$policies)) {
    return(config)
  }

  policies_section <- config$policies
  registry <- read_policy_registry(policy_path = policy_path)
  policy_defs <- registry$policies %||% list()

  slope_class_field <- policies_section$slope_class %||% NULL
  metric_field <- policies_section$metric %||% policies_section$metrics %||% NULL
  group_field <- policies_section$group %||% policies_section$groups %||% NULL

  canonicalize_metric <- function(values) {
    metric_aliases <- c(
      closest = "closest",
      similar = "closest",
      weighted = "weighted_mean",
      weighted_mean = "weighted_mean",
      ensemble_weighted = "weighted_mean",
      unweighted = "unweighted_mean",
      unweighted_mean = "unweighted_mean",
      ensemble_unweighted = "unweighted_mean",
      survey_distance = "survey_distance",
      taxon_distance = "taxon_distance",
      species_distance = "species_distance"
    )
    out <- stringr::str_to_lower(stringr::str_squish(as.character(unlist(values %||% character(0), use.names = FALSE))))
    out <- out[!is.na(out) & nzchar(out)]
    out <- unname(metric_aliases[out] %||% out)
    out[!is.na(out) & nzchar(out)]
  }
  canonicalize_group <- function(values) {
    out <- stringr::str_to_lower(stringr::str_squish(as.character(unlist(values %||% character(0), use.names = FALSE))))
    out <- gsub("(^|_)fao_area($|_)", "\\1fao\\2", out, perl = TRUE)
    out <- gsub("(^|_)derivation_type($|_)", "\\1derivation\\2", out, perl = TRUE)
    out <- gsub("(^|_)swimbladder_type($|_)", "\\1swimbladder\\2", out, perl = TRUE)
    out[!is.na(out) & nzchar(out)]
  }
  canonicalize_branch <- function(values) {
    normalize_policy_equation_branch_filters(values %||% NULL)
  }
  resolve_trait_names <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }
    if (!is.null(names(x)) && any(!is.na(names(x))) && any(nzchar(names(x)))) {
      out <- names(x)
    } else {
      out <- as.character(unlist(x, use.names = FALSE))
    }
    unique(out[!is.na(out) & nzchar(out)])
  }
  configured_policy_species_traits <- unique(c(
    resolve_trait_names(config$policy$species_traits %||% NULL),
    resolve_trait_names(config$similarity$species_traits %||% NULL),
    resolve_trait_names(config$admissibility$species_traits %||% NULL)
  ))
  configured_policy_study_traits <- unique(c(
    resolve_trait_names(config$policy$study_traits %||% NULL),
    resolve_trait_names(config$similarity$study_traits %||% NULL),
    resolve_trait_names(config$admissibility$study_traits %||% NULL)
  ))
  trait_registry <- read_trait_registry()
  policy_species_trait_keys <- vapply(
    trait_registry$species_traits %||% list(),
    function(x) as.character(x$coded_name %||% NA_character_),
    character(1)
  )
  policy_species_trait_keys <- policy_species_trait_keys[
    !is.na(policy_species_trait_keys) & nzchar(policy_species_trait_keys)
  ]
  policy_study_trait_keys <- vapply(
    trait_registry$study_traits %||% list(),
    function(x) as.character(x$coded_name %||% NA_character_),
    character(1)
  )
  policy_study_trait_keys <- policy_study_trait_keys[
    !is.na(policy_study_trait_keys) & nzchar(policy_study_trait_keys)
  ]
  trait_defs_for_order <- c(
    trait_registry$species_traits %||% list(),
    trait_registry$study_traits %||% list()
  )
  trait_keys_for_order <- vapply(
    trait_defs_for_order,
    function(x) as.character(x$coded_name %||% NA_character_),
    character(1)
  )
  trait_keys_for_order <- trait_keys_for_order[!is.na(trait_keys_for_order) & nzchar(trait_keys_for_order)]
  public_group_aliases <- c(
    fao_area = "fao",
    derivation_type = "derivation",
    swimbladder_type = "swimbladder"
  )
  canonical_joint_order <- unique(vapply(
    trait_keys_for_order,
    function(trait_key) {
      if (trait_key %in% names(public_group_aliases)) {
        unname(public_group_aliases[[trait_key]])
      } else {
        trait_key
      }
    },
    character(1)
  ))

  # Normalize flat active-policy declarations when no grouped declarations were
  # provided in the config YAML.
  if (is.null(group_field)) {
    active_values <- stringr::str_squish(as.character(unlist(policies_section$active %||% character(0), use.names = FALSE)))
    active_values <- active_values[!is.na(active_values) & nzchar(active_values)]
    policies_section$active <- active_values
    if (!is.null(slope_class_field)) {
      policies_section$slope_class <- canonicalize_branch(slope_class_field)
    }
    config$policies <- policies_section
    return(config)
  }

  if (!is.list(policy_defs) || length(policy_defs) == 0) {
    stop("The policy registry did not contain any constructor-generated policies.", call. = FALSE)
  }

  policy_tbl <- tibble::tibble(
    policy = vapply(policy_defs, function(x) as.character(x$coded_name %||% NA_character_), character(1)),
    grouping_key = vapply(policy_defs, function(x) as.character(x$grouping_key %||% NA_character_), character(1)),
    metric_key = vapply(policy_defs, function(x) as.character(x$metric_key %||% NA_character_), character(1)),
    policy_family = vapply(policy_defs, function(x) as.character(x$policy_family %||% NA_character_), character(1)),
    candidate_pool = vapply(policy_defs, function(x) as.character(x$candidate_pool %||% NA_character_), character(1))
  )
  policy_tbl <- dplyr::filter(policy_tbl, !is.na(.data$policy), nzchar(.data$policy))

  package_default_metrics <- c("closest", "weighted_mean", "unweighted_mean")
  package_default_branches <- "all"
  global_metrics <- canonicalize_metric(metric_field)
  global_branches <- if (is.null(slope_class_field)) package_default_branches else canonicalize_branch(slope_class_field)

  if (length(global_metrics) == 0) {
    global_metrics <- package_default_metrics
  }
  if (length(global_branches) == 0) {
    global_branches <- package_default_branches
  }

  group_specs <- list()
  if (!is.null(names(group_field)) && any(nzchar(names(group_field)))) {
    named_group_keys <- names(group_field)
    named_group_keys <- named_group_keys[!is.na(named_group_keys) & nzchar(named_group_keys)]
    canonical_named_keys <- canonicalize_group(named_group_keys)
    for (i in seq_along(canonical_named_keys)) {
      group_name <- canonical_named_keys[[i]]
      group_value <- group_field[[named_group_keys[[i]]]]

      if (is.null(group_value) || isTRUE(identical(group_value, NA)) || isTRUE(identical(group_value, TRUE))) {
        metrics_now <- global_metrics
        branches_now <- global_branches
        joint_variants <- NULL
        include_base <- TRUE
      } else if (is.list(group_value)) {
        metrics_now <- canonicalize_metric(group_value$metric %||% group_value$metrics %||% global_metrics)
        branches_now <- canonicalize_branch(group_value$slope_class %||% global_branches)
        joint_variants <- group_value$joint %||% group_value$join %||% group_value$joints %||% NULL
        include_base <- if (is.null(joint_variants)) {
          TRUE
        } else {
          !identical(group_value$include_base %||% TRUE, FALSE)
        }
      } else {
        metrics_now <- global_metrics
        branches_now <- global_branches
        joint_variants <- NULL
        include_base <- TRUE
      }

      if (length(metrics_now) == 0) {
        metrics_now <- global_metrics
      }
      if (length(branches_now) == 0) {
        branches_now <- global_branches
      }

      if (is.null(joint_variants)) {
        group_specs[[group_name]] <- list(metrics = metrics_now, branches = branches_now)
        next
      }

      # `joint` provides one or more explicit conjunction variants beneath a
      # root donor-pool group, e.g. `genus + fao + season`. When
      # `include_base` is TRUE (the default only when `joint` is present), the
      # plain root group is retained alongside the joint variants. The joint
      # traits are canonicalized into a fixed order so semantically identical
      # declarations do not create duplicate keys.
      raw_joint_sets <- if (is.list(joint_variants) && !is.data.frame(joint_variants)) {
        joint_variants
      } else {
        list(joint_variants)
      }
      variant_keys <- character(0)
      if (isTRUE(include_base)) {
        variant_keys <- c(variant_keys, group_name)
      }
      for (joint_now in raw_joint_sets) {
        joint_traits <- canonicalize_group(joint_now)
        joint_traits <- joint_traits[!joint_traits %in% group_name]
        joint_traits <- unique(joint_traits)
        if (length(joint_traits) == 0) {
          stop(
            sprintf(
              "Policy group '%s' declared an empty 'joint' variant after canonicalization.",
              group_name
            ),
            call. = FALSE
          )
        }
        known_joint <- joint_traits[joint_traits %in% canonical_joint_order]
        unknown_joint <- sort(setdiff(joint_traits, canonical_joint_order))
        ordered_joint <- c(
          canonical_joint_order[canonical_joint_order %in% known_joint],
          unknown_joint
        )
        variant_keys <- c(variant_keys, paste(c(group_name, ordered_joint), collapse = "_"))
      }
      variant_keys <- unique(variant_keys)
      for (variant_key in variant_keys) {
        group_specs[[variant_key]] <- list(metrics = metrics_now, branches = branches_now)
      }
    }
  } else {
    for (group_name in canonicalize_group(group_field)) {
      group_specs[[group_name]] <- list(
        metrics = global_metrics,
        branches = global_branches
      )
    }
  }

  # Policy constructor groups are an independent consumer of trait columns.
  # Resolve their registry-backed dependencies before applying the active
  # similarity-trait restrictions, then retain those dependencies explicitly
  # on the normalized `policies` section. This keeps policy availability fixed
  # when Sentinel excludes a trait from similarity estimation.
  declared_policy_traits <- unique(unlist(lapply(names(group_specs), function(group_name) {
    definition <- tryCatch(
      build_policy_group_definition(
        group_key = group_name,
        registry = registry
      ),
      error = function(e) NULL
    )
    if (!is.list(definition)) {
      return(character(0))
    }
    as.character(unlist(
      definition$fixed_parameters$match_traits %||% character(0),
      use.names = FALSE
    ))
  }), use.names = FALSE))
  declared_policy_traits <- declared_policy_traits[
    !is.na(declared_policy_traits) & nzchar(declared_policy_traits)
  ]
  declared_policy_species_traits <- intersect(
    declared_policy_traits,
    policy_species_trait_keys
  )
  declared_policy_study_traits <- intersect(
    declared_policy_traits,
    policy_study_trait_keys
  )
  configured_policy_species_traits <- unique(c(
    configured_policy_species_traits,
    declared_policy_species_traits
  ))
  configured_policy_study_traits <- unique(c(
    configured_policy_study_traits,
    declared_policy_study_traits
  ))
  policies_section$species_traits <- declared_policy_species_traits
  policies_section$study_traits <- declared_policy_study_traits

  if (length(group_specs) > 0) {
    canonical_specs <- list()
    for (group_name in names(group_specs)) {
      canonical_group_name <- tryCatch(
        as.character(build_policy_group_definition(
          group_name,
          registry = registry,
          active_species_traits = configured_policy_species_traits,
          active_study_traits = configured_policy_study_traits
        )$key %||% group_name)[[1]],
        error = function(e) group_name
      )
      spec_now <- group_specs[[group_name]] %||% list()
      if (!is.list(canonical_specs[[canonical_group_name]])) {
        canonical_specs[[canonical_group_name]] <- list(
          metrics = unique(as.character(spec_now$metrics %||% character(0))),
          branches = unique(as.character(spec_now$branches %||% character(0)))
        )
      } else {
        canonical_specs[[canonical_group_name]] <- list(
          metrics = unique(c(
            as.character(canonical_specs[[canonical_group_name]]$metrics %||% character(0)),
            as.character(spec_now$metrics %||% character(0))
          )),
          branches = unique(c(
            as.character(canonical_specs[[canonical_group_name]]$branches %||% character(0)),
            as.character(spec_now$branches %||% character(0))
          ))
        )
      }
    }
    group_specs <- canonical_specs
  }

  declared_groups <- names(group_specs)
  if (length(declared_groups) == 0) {
    stop("Policies must declare at least one group when using constructor-style policy selection.", call. = FALSE)
  }

  available_groups <- unique(policy_tbl$grouping_key)
  unknown_groups <- setdiff(declared_groups, available_groups)
  if (length(unknown_groups) > 0) {
    synthetic_defs <- list()
    for (group_name in unknown_groups) {
      metrics_now <- unique(c(global_metrics, group_specs[[group_name]]$metrics %||% character(0)))
      for (metric_name in metrics_now) {
        policy_def <- tryCatch(
          build_group_metric_policy(
            group_key = group_name,
            metric_key = metric_name,
            registry = registry,
            active_species_traits = configured_policy_species_traits,
            active_study_traits = configured_policy_study_traits
          ),
          error = function(e) NULL
        )
        if (is.list(policy_def)) {
          synthetic_defs[[length(synthetic_defs) + 1L]] <- policy_def
        }
      }
    }
    if (length(synthetic_defs) > 0) {
      policy_tbl <- dplyr::bind_rows(
        policy_tbl,
        tibble::tibble(
          policy = vapply(synthetic_defs, function(x) as.character(x$coded_name %||% NA_character_), character(1)),
          grouping_key = vapply(synthetic_defs, function(x) as.character(x$grouping_key %||% NA_character_), character(1)),
          metric_key = vapply(synthetic_defs, function(x) as.character(x$metric_key %||% NA_character_), character(1)),
          policy_family = vapply(synthetic_defs, function(x) as.character(x$policy_family %||% NA_character_), character(1)),
          candidate_pool = vapply(synthetic_defs, function(x) as.character(x$candidate_pool %||% NA_character_), character(1))
        )
      ) |>
        dplyr::distinct(.data$policy, .keep_all = TRUE)
      available_groups <- unique(policy_tbl$grouping_key)
      available_metrics <- unique(policy_tbl$metric_key)
    }
  }
  unknown_groups <- setdiff(declared_groups, available_groups)
  if (length(unknown_groups) > 0) {
    stop(
      sprintf("Unknown config policy group name(s): %s", paste(unknown_groups, collapse = ", ")),
      call. = FALSE
    )
  }

  all_requested_metrics <- unique(c(global_metrics, unlist(lapply(group_specs, `[[`, "metrics"), use.names = FALSE)))
  available_metrics <- unique(policy_tbl$metric_key)
  unknown_metrics <- setdiff(all_requested_metrics, available_metrics)
  if (length(unknown_metrics) > 0) {
    stop(
      sprintf("Unknown config policy metric name(s): %s", paste(unknown_metrics, collapse = ", ")),
      call. = FALSE
    )
  }

  selected_policies <- character(0)
  policy_param_overrides <- list()

  for (group_name in declared_groups) {
    spec_now <- group_specs[[group_name]]
    metrics_now <- spec_now$metrics
    if (length(metrics_now) == 0) {
      next
    }
    matches <- policy_tbl |>
      dplyr::filter(.data$grouping_key == group_name, .data$metric_key %in% metrics_now)
    if (nrow(matches) == 0) {
      stop(
        sprintf(
          "No constructor-generated policies matched group '%s' with metrics: %s",
          group_name,
          paste(metrics_now, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    selected_policies <- c(selected_policies, matches$policy)
    for (policy_name in matches$policy) {
      policy_param_overrides[[policy_name]] <- list(slope_class = spec_now$branches)
    }
  }

  selected_policies <- unique(selected_policies)
  if (length(selected_policies) == 0) {
    stop("Constructor policy selection did not resolve to any active policies.", call. = FALSE)
  }

  policies_section$active <- selected_policies
  policies_section$policy_params <- policy_param_overrides
  policies_section$slope_class <- unique(unlist(
    lapply(policy_param_overrides, function(x) x$slope_class %||% character(0)),
    use.names = FALSE
  ))
  if (length(policies_section$slope_class) == 0) {
    policies_section$slope_class <- global_branches
  }
  config$policies <- policies_section
  config
}

#' Normalize selection-owned conformal settings
#'
#' @param config Config list.
#'
#' @return Config list with post-selection conformal settings merged
#'   into `selection`.
#'
#' @keywords internal
#' @noRd
normalize_selection_config_shape <- function(config) {
  if (!is.list(config)) {
    return(config)
  }

  selection <- config$selection %||% list()
  if (!is.list(selection)) {
    selection <- list()
  }

  selection$conformal_alpha <- selection$conformal_alpha %||%
    (config$policy %||% list())$conformal_alpha %||%
    (config$similarity %||% list())$conformal_alpha %||%
    NULL
  selection$bin_alpha <- selection$bin_alpha %||% NULL
  selection$min_bin_scores <- selection$min_bin_scores %||% NULL
  selection$n_bins <- selection$n_bins %||% NULL
  selection$use_support_bin_intervals <- selection$use_support_bin_intervals %||% NULL
  selection$support_bin_labels <- selection$support_bin_labels %||% NULL

  config$selection <- selection
  config
}

#' Replace fully specified trait maps after recursive merge
#'
#' @param merged_cfg Merged config list.
#' @param input_cfg Caller-supplied config list after alias and shape
#'   normalization.
#'
#' @return Config list with explicit trait maps replaced rather than
#'   recursively merged with defaults.
#' @keywords internal
#' @noRd
replace_explicit_trait_maps <- function(merged_cfg,
                                        input_cfg) {
  if (!is.list(merged_cfg) || !is.list(input_cfg)) {
    return(merged_cfg)
  }

  input_similarity <- input_cfg$similarity %||% list()
  input_policy <- input_cfg$policy %||% list()
  input_admissibility <- input_cfg$admissibility %||% list()
  input_policies <- input_cfg$policies %||% list()

  if (is.list(input_similarity$species_traits)) {
    merged_cfg$similarity$species_traits <- input_similarity$species_traits
    merged_cfg$policy$species_traits <- input_similarity$species_traits
  } else if (is.list(input_policy$species_traits)) {
    merged_cfg$policy$species_traits <- input_policy$species_traits
  }

  if (is.list(input_similarity$study_traits)) {
    merged_cfg$similarity$study_traits <- input_similarity$study_traits
    merged_cfg$policy$study_traits <- input_similarity$study_traits
  } else if (is.list(input_policy$study_traits)) {
    merged_cfg$policy$study_traits <- input_policy$study_traits
  }

  if (!is.null(input_admissibility$species_traits)) {
    merged_cfg$admissibility$species_traits <- input_admissibility$species_traits
  }

  if (!is.null(input_admissibility$study_traits)) {
    merged_cfg$admissibility$study_traits <- input_admissibility$study_traits
  }

  # Replace constructor-style policy specifications wholesale so YAML entries
  # like `genus:` or `family:` survive the recursive merge instead of being
  # dropped when their values are NULL placeholders.
  if ("group" %in% names(input_policies)) {
    merged_cfg$policies$group <- input_policies$group
  }
  if ("metric" %in% names(input_policies)) {
    merged_cfg$policies$metric <- input_policies$metric
  }
  if ("slope_class" %in% names(input_policies)) {
    merged_cfg$policies$slope_class <- input_policies$slope_class
  }
  if ("active" %in% names(input_policies)) {
    if (!any(c("group", "metric", "slope_class") %in% names(input_policies))) {
      merged_cfg$policies$group <- NULL
      merged_cfg$policies$metric <- NULL
      merged_cfg$policies$slope_class <- NULL
    }
    merged_cfg$policies$active <- input_policies$active
  }

  merged_cfg
}

#' Create a configuration template
#'
#' Builds a pipeline-agnostic baseline configuration with neutral path
#' placeholders and registry-derived default trait and policy selections.
#'
#' @param input_file Placeholder input workbook path. The workbook is not read.
#' @param output_root Output root directory.
#' @param cache_folder Cache folder.
#' @param registry_path Optional trait-registry path used to derive default
#'   trait names.
#' @param policy_path Optional policy-registry path used to derive one default
#'   active policy.
#' @return A config list.
#'
#' @export
create_configuration_template <- function(input_file = "input.xlsx",
                                          output_root = "outputs",
                                          cache_folder = "cache",
                                          registry_path = NULL,
                                          policy_path = NULL) {
  # Derive the minimal required trait and policy defaults from the registries
  # so the fallback config stays pipeline-agnostic. The input path is only a
  # placeholder in the returned list; no workbook is read here.
  species_traits <- trait_names(scope = "species", registry_path = registry_path)
  study_traits <- trait_names(scope = "study", registry_path = registry_path)
  if (length(species_traits) == 0) {
    stop("No species traits were available in the trait registry.", call. = FALSE)
  }
  if (length(study_traits) == 0) {
    stop("No study traits were available in the trait registry.", call. = FALSE)
  }

  list(
    paths = list(
      input_file = input_file,
      out_root = output_root,
      cache_dir = cache_folder,
      supplemental_dir = "supplemental",
      log_file = file.path(output_root, "tsbiomass_run.log")
    ),
    execution = list(
      strict_length_pdf = FALSE,
      run_multiplier_model = FALSE,
      write_log = FALSE,
      progress = FALSE
    ),
    tuning = list(
      species_model_limit = 2L,
      resamples = 8L,
      n_cores = 1L,
      seed = NULL,
      alpha_range = list(from = 0.1, to = 0.9),
      kernel_scale_range = list(from = 1, to = 8),
      coherence = list(
        length = list(range = list(from = 0.5, to = 6)),
        depth = list(range = list(from = 0.5, to = 6)),
        frequency = list(range = list(from = 0.5, to = 6))
      ),
      grid_refinement_levels = 1L,
      response_surface_top_n = 20L,
      rmse_tolerance = 0.01,
      support_strata_bins = 4L,
      regularization = list(
        alpha = 0.05,
        kernel_scale = 0.05,
        coherence_scale = 0.05,
        stability = 0.02
      ),
      equal_start_weights = FALSE
    ),
    similarity = list(
      alpha = 0.8,
      kernel_scale = 4,
      species_traits = stats::setNames(list(1), species_traits[[1]]),
      study_traits = stats::setNames(list(1), study_traits[[1]]),
      coherence = list(
        length = list(mode = "overlap", weight = 2),
        depth = list(mode = "overlap", weight = 3),
        frequency = list(mode = "overlap", weight = 2, gap = 60)
      ),
      conformal_alpha = 0.1
    ),
    ordination = list(
      include_loadings = FALSE,
      include_centroids = FALSE
    ),
    policies = list(
      group = list("species"),
      metric = list("closest"),
      slope_class = list("all")
    ),
    cache = list(
      folder = cache_folder,
      refresh = FALSE,
      names = read_cache_defaults()
    ),
    benchmark = list(
      workers = 1L,
      engine = "cpp",
      include_ts_error = FALSE
    ),
    admissibility = list(
      species_traits = character(0),
      study_traits = character(0),
      coherence = list(
        length = list(mode = "overlap", min = 0.25),
        depth = list(mode = "overlap", min = 0.25),
        frequency = list(mode = "none", gap = 60)
      ),
      key_metadata_max = 0.25
    ),
    uncertainty = list(
      # Uncertainty-stage learner (self-contained; no shared metalearner section).
      method = "glm",
      super_methods = NULL,
      method_settings = default_meta_policy_method_settings(),
      n_folds = 5L,
      inner_folds = 5L,
      workers = 1L,
      outcome_col = "error_abs_log",
      outcome_clip_quantile = 0.99,
      outcome_transform = "log1p",
      lambda_rule = "lambda.1se",
      loss = "squared_error"
    ),
    selection = list(
      one_se_multiplier = 1,
      equivalence_tolerance = 0.05,
      n_boot = 500L,
      seed = NULL,
      uncertainty_rule = "tolerance",
      u_tol_rel = 0.25,
      u_tol_abs = 0.05,
      uncertainty_relative_tolerance = 0.25,
      uncertainty_absolute_tolerance = 0.05,
      local_distance_tolerance = 1e-12,
      conformal_alpha = 0.10,
      bin_alpha = 0.10,
      min_bin_scores = 10L,
      n_bins = 3L,
      use_support_bin_intervals = FALSE,
      support_bin_labels = default_post_selection_support_labels(3L),
      # Selection-stage learner (self-contained; no shared metalearner section).
      method = "glm",
      super_methods = NULL,
      method_settings = default_meta_policy_method_settings(),
      n_folds = 5L,
      inner_folds = 5L,
      workers = 1L,
      outcome_col = "error_abs_log",
      outcome_clip_quantile = 0.99,
      outcome_transform = "log1p",
      lambda_rule = "lambda.1se",
      loss = "squared_error",
      max_selection_tolerance = 1e-12,
      refresh = FALSE
    ),
    simulation = list(
      workers = 1L
    )
  )
}

#' Read a configuration YAML file
#'
#' Reads, validates, and normalizes a caller-supplied config YAML file at
#' ingestion time.
#'
#' @param path Config YAML path.
#' @param base_dir Base directory used to resolve relative paths. Defaults to
#'   the YAML file directory.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path.
#'
#' @return A validated normalized config list.
#'
#' @export
read_configuration <- function(path,
                               base_dir = dirname(path_absolute(path)),
                               registry_path = NULL,
                               policy_path = NULL) {
  # Require an explicit YAML path so the generic config reader never falls back
  # to a packaged analysis-specific file.
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single YAML file path.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("Config file does not exist: %s", path), call. = FALSE)
  }

  raw_config <- yaml::read_yaml(path)

  normalize_config(
    config = raw_config,
    base_dir = base_dir,
    registry_path = registry_path,
    policy_path = policy_path
  )
}

#' Normalize a config
#'
#' Merges user config onto the package defaults and resolves relative paths.
#'
#' @param config Config list.
#' @param base_dir Base directory for relative paths.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path.
#'
#' @return A normalized config list.
#'
#' @keywords internal
#' @noRd
normalize_config <- function(config,
                             base_dir = getwd(),
                             registry_path = NULL,
                             policy_path = NULL) {
  # Start from the package defaults so missing sections or fields do not force
  # every config YAML to restate the full config surface.
  if (!is.list(config)) {
    stop("'config' must be a list.", call. = FALSE)
  }
  if (!is.character(base_dir) || length(base_dir) != 1 || !nzchar(base_dir)) {
    stop("'base_dir' must be a single non-empty path.", call. = FALSE)
  }
  if (isTRUE(attr(config, "tsbiomass_normalized"))) {
    return(config)
  }

  reject_retired_configuration_fields(config)
  config <- normalize_similarity_config_shape(config)
  config <- normalize_selection_config_shape(config)

  normalized_config <- merge_config_sections(
    create_configuration_template(
      registry_path = registry_path,
      policy_path = policy_path
    ),
    config
  )
  normalized_config <- replace_explicit_trait_maps(normalized_config, config)
  normalized_config <- normalize_similarity_config_shape(normalized_config)
  normalized_config <- normalize_selection_config_shape(normalized_config)
  normalized_config <- apply_cache_defaults(normalized_config)
  normalized_config <- normalize_active_policy_names(normalized_config, policy_path = policy_path)
  normalized_config$selection <- normalize_learner_section(normalized_config$selection %||% list())
  normalized_config$uncertainty <- normalize_learner_section(normalized_config$uncertainty %||% list())
  normalized_config <- normalize_trait_sections(normalized_config)

  # Validate the fully merged config before resolving paths so structural
  # errors are reported against the config contents themselves.
  validate_config(
    config = normalized_config,
    registry_path = registry_path,
    policy_path = policy_path
  )

  # Resolve only the path fields once so downstream code can use
  # absolute normalized paths consistently.
  normalized_config$paths$input_file <- path_absolute(normalized_config$paths$input_file, base_dir = base_dir)
  normalized_config$paths$out_root <- path_absolute(normalized_config$paths$out_root, base_dir = base_dir)
  normalized_config$paths$cache_dir <- path_absolute(normalized_config$paths$cache_dir, base_dir = base_dir)
  if (!is.null(normalized_config$paths$log_file) && nzchar(normalized_config$paths$log_file)) {
    normalized_config$paths$log_file <- path_absolute(normalized_config$paths$log_file, base_dir = base_dir)
  } else {
    normalized_config$paths$log_file <- NULL
  }

  if (!is.null(normalized_config$paths$supplemental_dir)) {
    normalized_config$paths$supplemental_dir <- path_absolute(
      normalized_config$paths$supplemental_dir,
      base_dir = base_dir
    )
  }
  # Materialize the flat top-level fields that admissibility and nested
  # validation still read directly from the config object.
  normalized_config$alpha <- normalized_config$policy$alpha
  normalized_config$k_species <- normalized_config$policy$k_species
  normalized_config$k_study <- normalized_config$policy$k_study
  normalized_config$frequency_coherence_mode <-
    stringr::str_to_lower(
      stringr::str_squish(as.character(normalized_config$policy$frequency_coherence_mode %||% "overlap"))
    )[[1]]
  normalized_config$require_same_frequency_label <- normalized_config$policy$require_same_frequency_label
  normalized_config$max_frequency_gap_khz <- normalized_config$policy$max_frequency_gap_khz
  normalized_config$min_length_overlap_fraction <- normalized_config$policy$min_length_overlap_fraction
  normalized_config$min_depth_overlap_fraction <- normalized_config$policy$min_depth_overlap_fraction
  normalized_config$missing_key_metadata_max_fraction <- normalized_config$policy$missing_key_metadata_max_fraction
  normalized_config$length_overlap_weight <- normalized_config$policy$length_overlap_weight
  normalized_config$depth_overlap_weight <- normalized_config$policy$depth_overlap_weight
  normalized_config$frequency_coherence_weight <- normalized_config$policy$frequency_coherence_weight
  normalized_config$core_weight_cutoff <- normalized_config$policy$core_weight_cutoff
  normalized_config$conformal_alpha <- normalized_config$selection$conformal_alpha %||% normalized_config$policy$conformal_alpha
  attr(normalized_config, "tsbiomass_normalized") <- TRUE
  normalized_config
}

#' Validate a config
#'
#' Validates the structure and registry-linked content of a config YAML
#' configuration.
#'
#' @param config Config list.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path.
#'
#' @return The validated config.
#'
#' @keywords internal
#' @noRd
validate_config <- function(config,
                            registry_path = NULL,
                            policy_path = NULL) {
  if (is_s7_instance(config, "Configurer")) {
    config <- config@data
  }

  # Require the top-level sections first so all later field checks can assume a
  # stable nested config structure.
  if (!is.list(config)) {
    stop("'config' must be a list.", call. = FALSE)
  }

  config <- normalize_similarity_config_shape(config)
  config <- normalize_selection_config_shape(config)
  config <- apply_cache_defaults(config)
  config <- normalize_active_policy_names(config, policy_path = policy_path)
  config$selection <- normalize_learner_section(config$selection %||% list())
  config$uncertainty <- normalize_learner_section(config$uncertainty %||% list())
  config <- normalize_trait_sections(config)

  required_sections <- c("paths", "execution", "tuning", "similarity", "policy", "policies", "selection")
  missing_sections <- setdiff(required_sections, names(config))
  if (length(missing_sections) > 0) {
    stop(
      sprintf(
        "Config is missing required section(s): %s",
        paste(missing_sections, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  validate_config_paths(config$paths)
  validate_execution_flags(config$execution, config$paths)
  validate_tuning_section(config$tuning)
  validate_cache_section(config$cache %||% NULL)
  validate_similarity_section(config$similarity %||% NULL, registry_path = registry_path)
  validate_stage_section(config$ordination %||% NULL, "Ordination")
  validate_policy_section(config$policy, registry_path = registry_path)
  validate_policy_list_section(config$policies, policy_path = policy_path)
  validate_admissibility_section(config$admissibility %||% NULL, registry_path = registry_path)
  validate_benchmark_section(config$benchmark %||% NULL)
  validate_stage_section(config$uncertainty %||% NULL, "Uncertainty")
  validate_learner_section(config$uncertainty %||% NULL, "Uncertainty")
  validate_selection_section(config$selection %||% NULL)
  validate_learner_section(config$selection %||% NULL, "Selection")
  validate_stage_section(config$simulation %||% NULL, "Simulation", worker_field = "workers")
  validate_sentinel_section(config$sentinel %||% NULL)
  alchemist_method_settings <- config$alchemist$learner$method_settings %||% NULL
  if (!is.null(alchemist_method_settings)) {
    validate_metalearner_method_settings(merge_config_sections(
      config$selection$method_settings %||% list(),
      alchemist_method_settings
    ))
  }

  config
}

#' Normalize the learner fields of a selection/uncertainty section
#'
#' @param learner_section Config `selection` or `uncertainty` section.
#'
#' @return Section with normalized learner fields.
#' @keywords internal
#' @noRd
normalize_learner_section <- function(learner_section) {
  # Normalizes the learner fields of a self-contained selection/uncertainty
  # section. Non-learner fields (e.g. selection's policy-selection parameters)
  # are left untouched.
  if (is.null(learner_section) || !is.list(learner_section)) {
    return(learner_section)
  }
  learner_section$method <- learner_section[["method", exact = TRUE]] %||% "glm"
  learner_section$method_settings <- normalize_meta_policy_method_settings(
    learner_section$method_settings %||% NULL,
    defaults_path = learner_section$method_defaults_path %||% NULL
  )
  learner_section
}

#' Build config option values
#'
#' Returns a named list of options suitable for `options(...)`.
#'
#' @param config Normalized config list.
#'
#' @return Named list of option values.
#'
#' @keywords internal
#' @noRd
config_option_values <- function(config) {
  if (is_s7_instance(config, "Configurer")) {
    config <- config@data
  }

  # Expose the most commonly reused config scalars and paths through a small
  # options list so sourced scripts can read them consistently.
  validate_config(config)

  list(
    tsbiomass_input_file = config$paths$input_file,
    tsbiomass_out_root = config$paths$out_root,
    tsbiomass_cache_dir = config$paths$cache_dir,
    tsbiomass_supplemental_dir = config$paths$supplemental_dir,
    tsbiomass_alpha = config$policy$alpha,
    tsbiomass_k_species = config$policy$k_species,
    tsbiomass_k_study = config$policy$k_study,
    tsbiomass_conformal_alpha = config$policy$conformal_alpha
  )
}

#' Normalize one config trait map
#'
#' @param trait_map Optional named trait-weight map.
#'
#' @return Named numeric vector.
#' @keywords internal
#' @noRd
normalize_trait_map <- function(trait_map) {
  if (!is.null(trait_map)) {
    if (is.data.frame(trait_map) && all(c("trait", "weight") %in% names(trait_map))) {
      weights <- as.numeric(trait_map$weight)
      names(weights) <- as.character(trait_map$trait)
      return(stats::setNames(weights, names(weights)))
    }

    if (is.list(trait_map) || is.atomic(trait_map)) {
      trait_values <- unlist(trait_map, use.names = FALSE)
      trait_names <- names(trait_map)
      if (is.null(trait_names) || any(!nzchar(trait_names))) {
        trait_names <- stringr::str_squish(as.character(trait_values))
        trait_names <- trait_names[!is.na(trait_names) & nzchar(trait_names)]
        return(stats::setNames(rep(1, length(trait_names)), trait_names))
      }
      weights <- suppressWarnings(as.numeric(trait_values))
      return(stats::setNames(weights, trait_names))
    }
  }

  numeric(0)
}

#' Normalize config trait sections
#'
#' @param config Config list.
#'
#' @return Config list with normalized similarity weight maps and
#'   normalized admissibility gate-trait selections.
#' @keywords internal
#' @noRd
normalize_trait_sections <- function(config) {
  if (!is.list(config)) {
    return(config)
  }

  config$policy <- config$policy %||% list()
  config$similarity <- config$similarity %||% list()
  config$admissibility <- config$admissibility %||% list()

  config$policy$species_traits <- normalize_trait_map(config$policy$species_traits)
  config$policy$study_traits <- normalize_trait_map(config$policy$study_traits)
  config$similarity$species_traits <- normalize_trait_map(
    config$similarity$species_traits %||% config$policy$species_traits
  )
  config$similarity$study_traits <- normalize_trait_map(
    config$similarity$study_traits %||% config$policy$study_traits
  )
  config$admissibility$species_traits <- names(normalize_trait_map(config$admissibility$species_traits))
  config$admissibility$species_traits <- unique(config$admissibility$species_traits[!is.na(config$admissibility$species_traits) & nzchar(config$admissibility$species_traits)])
  config$admissibility$study_traits <- names(normalize_trait_map(config$admissibility$study_traits))
  config$admissibility$study_traits <- unique(config$admissibility$study_traits[!is.na(config$admissibility$study_traits) & nzchar(config$admissibility$study_traits)])

  config
}

#' Validate config paths
#'
#' @param paths_section Config `paths` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_config_paths <- function(paths_section) {
  # Check only type and presence here so template paths can remain placeholders
  # until the analysis is actually run.
  required_fields <- c("input_file", "out_root", "cache_dir")
  missing_fields <- setdiff(required_fields, names(paths_section))
  if (length(missing_fields) > 0) {
    stop(
      sprintf("Config paths are missing field(s): %s", paste(missing_fields, collapse = ", ")),
      call. = FALSE
    )
  }

  for (field_name in required_fields) {
    field_value <- paths_section[[field_name]]
    if (!is.character(field_value) || length(field_value) != 1 || !nzchar(field_value)) {
      stop(sprintf("Config path '%s' must be a single non-empty string.", field_name), call. = FALSE)
    }
  }

  # Allow `log_file` to be omitted entirely because console logging is already
  # available during command-line runs.
  if (!is.null(paths_section$log_file) &&
    (!is.character(paths_section$log_file) || length(paths_section$log_file) != 1 || !nzchar(paths_section$log_file))) {
    stop("Config path 'log_file' must be NULL or a single non-empty string.", call. = FALSE)
  }
}

#' Validate execution flags
#'
#' @param execution_section Config `execution` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_execution_flags <- function(execution_section,
                                     paths_section) {
  # Require the known execution booleans explicitly so any misshapen YAML values
  # fail before the script wrapper interprets them.
  flag_fields <- c("strict_length_pdf", "run_multiplier_model", "write_log", "progress")
  for (field_name in flag_fields) {
    field_value <- execution_section[[field_name]]
    if (!is.logical(field_value) || length(field_value) != 1 || is.na(field_value)) {
      stop(sprintf("Execution flag '%s' must be TRUE or FALSE.", field_name), call. = FALSE)
    }
  }

  # Require a log-file path only when file logging is explicitly enabled.
  if (isTRUE(execution_section$write_log) &&
    (is.null(paths_section$log_file) || !nzchar(paths_section$log_file))) {
    stop("A 'paths.log_file' value is required when 'execution.write_log = TRUE'.", call. = FALSE)
  }
}

#' Validate the tuning section
#'
#' @param tuning_section Config `tuning` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_tuning_section <- function(tuning_section) {
  # Restrict the tuning controls to finite positive numeric scalars.
  numeric_fields <- c("species_model_limit", "resamples")
  for (field_name in numeric_fields) {
    field_value <- tuning_section[[field_name]]
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1) {
      stop(sprintf("Tuning field '%s' must be one finite number >= 1.", field_name), call. = FALSE)
    }
  }

  optional_fields <- c("n_cores", "seed", "grid_refinement_levels", "response_surface_top_n", "support_strata_bins")
  for (field_name in optional_fields) {
    field_value <- tuning_section[[field_name]]
    if (is.null(field_value)) {
      next
    }
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 0) {
      stop(sprintf("Tuning field '%s' must be one finite number >= 0.", field_name), call. = FALSE)
    }
  }
  if (!is.null(tuning_section$rmse_tolerance) &&
    (!is.numeric(tuning_section$rmse_tolerance) || length(tuning_section$rmse_tolerance) != 1 ||
      !is.finite(tuning_section$rmse_tolerance) || tuning_section$rmse_tolerance < 0)) {
    stop("Tuning field 'rmse_tolerance' must be one finite number >= 0.", call. = FALSE)
  }
  if (!is.null(tuning_section$regularization)) {
    reg_now <- tuning_section$regularization
    if (!is.list(reg_now)) {
      stop("Tuning field 'regularization' must be a named list.", call. = FALSE)
    }
    allowed_fields <- c("alpha", "kernel_scale", "coherence_scale", "stability", "edge", "edge_margin")
    bad_fields <- setdiff(names(reg_now), allowed_fields)
    if (length(bad_fields) > 0) {
      stop(
        sprintf(
          "Tuning field 'regularization' contains unsupported field(s): %s",
          paste(bad_fields, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    for (field_name in allowed_fields) {
      field_value <- reg_now[[field_name]]
      if (is.null(field_value)) {
        next
      }
      if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 0) {
        stop(
          sprintf("Tuning field 'regularization.%s' must be one finite number >= 0.", field_name),
          call. = FALSE
        )
      }
    }
  }

  if (!is.null(tuning_section$equal_start_weights) &&
    (!is.logical(tuning_section$equal_start_weights) ||
      length(tuning_section$equal_start_weights) != 1 ||
      is.na(tuning_section$equal_start_weights))) {
    stop("Tuning field 'equal_start_weights' must be TRUE or FALSE.", call. = FALSE)
  }

  validate_tuning_range <- function(x, field_name, alpha_like = FALSE) {
    if (is.null(x)) {
      return(invisible(NULL))
    }
    values <- if (is.list(x) && all(c("from", "to") %in% names(x))) {
      c(x$from, x$to)
    } else {
      unlist(x, use.names = FALSE)
    }
    values <- suppressWarnings(as.numeric(values))
    values <- values[is.finite(values)]
    if (length(values) < 2L) {
      stop(sprintf("Tuning field '%s' must contain at least two finite numeric values or a from/to range.", field_name), call. = FALSE)
    }
    if (isTRUE(alpha_like)) {
      if (any(values <= 0 | values >= 1)) {
        stop(sprintf("Tuning field '%s' must stay strictly between 0 and 1.", field_name), call. = FALSE)
      }
    } else if (any(values < 0)) {
      stop(sprintf("Tuning field '%s' must be nonnegative.", field_name), call. = FALSE)
    }
    invisible(NULL)
  }
  validate_tuning_range(tuning_section$alpha_range, "alpha_range", alpha_like = TRUE)
  validate_tuning_range(tuning_section$kernel_scale_range, "kernel_scale_range")
  tuning_coherence <- tuning_section$coherence %||% list()
  for (field_name in c("length", "depth", "frequency")) {
    field_cfg <- tuning_coherence[[field_name]] %||% list()
    validate_tuning_range(field_cfg$range, sprintf("coherence.%s.range", field_name))
    validate_tuning_range(field_cfg$grid, sprintf("coherence.%s.grid", field_name))
  }
}

#' Validate the cache section
#'
#' @param cache_section Config `cache` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_cache_section <- function(cache_section) {
  if (is.null(cache_section)) {
    return(invisible(NULL))
  }
  if (!is.null(cache_section$folder) &&
    (!is.character(cache_section$folder) || length(cache_section$folder) != 1 || !nzchar(cache_section$folder))) {
    stop("Cache field 'folder' must be a single non-empty string.", call. = FALSE)
  }
  if (!is.null(cache_section$refresh) &&
    (!is.logical(cache_section$refresh) || length(cache_section$refresh) != 1 || is.na(cache_section$refresh))) {
    stop("Cache field 'refresh' must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(NULL)
}

#' Validate the similarity section
#'
#' @param similarity_section Config `similarity` section.
#' @param registry_path Optional trait-registry path.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_similarity_section <- function(similarity_section,
                                        registry_path = NULL) {
  validate_search_range <- function(x,
                                    field_name,
                                    alpha_like = FALSE) {
    if (is.null(x)) {
      return(invisible(NULL))
    }
    if (is.list(x) && all(c("from", "to") %in% names(x))) {
      values <- c(x$from, x$to)
    } else {
      values <- as.numeric(unlist(x, use.names = FALSE))
    }
    values <- values[is.finite(values)]
    if (length(values) < 2) {
      stop(sprintf("Similarity field '%s' must contain at least two finite numeric values or a from/to range.", field_name), call. = FALSE)
    }
    if (isTRUE(alpha_like)) {
      if (any(values <= 0 | values >= 1)) {
        stop(sprintf("Similarity field '%s' must stay strictly between 0 and 1.", field_name), call. = FALSE)
      }
    } else if (any(values < 0)) {
      stop(sprintf("Similarity field '%s' must be nonnegative.", field_name), call. = FALSE)
    }
    invisible(NULL)
  }

  if (is.null(similarity_section)) {
    return(invisible(NULL))
  }

  scalar_fields <- c("alpha", "kernel_scale", "conformal_alpha")
  for (field_name in scalar_fields) {
    field_value <- similarity_section[[field_name]]
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value)) {
      stop(sprintf("Similarity field '%s' must be one finite numeric value.", field_name), call. = FALSE)
    }
  }
  if (!is.null(similarity_section$key_metadata_max)) {
    stop(
      "Similarity field 'key_metadata_max' is unsupported; missing-metadata screening belongs under 'admissibility.key_metadata_max'.",
      call. = FALSE
    )
  }
  if (!is.null(similarity_section$core_weight_cutoff) &&
    (!is.numeric(similarity_section$core_weight_cutoff) || length(similarity_section$core_weight_cutoff) != 1 ||
      !is.finite(similarity_section$core_weight_cutoff))) {
    stop("Similarity field 'core_weight_cutoff' must be one finite numeric value.", call. = FALSE)
  }
  for (field_name in c("length_overlap_min", "depth_overlap_min")) {
    if (!is.null(similarity_section[[field_name]])) {
      stop(
        sprintf(
          "Similarity field '%s' is unsupported; length/depth overlap thresholds belong under 'admissibility.coherence'.",
          field_name
        ),
        call. = FALSE
      )
    }
  }
  if (similarity_section$alpha <= 0 || similarity_section$alpha >= 1) {
    stop("Similarity field 'alpha' must be strictly between 0 and 1.", call. = FALSE)
  }
  if (similarity_section$conformal_alpha <= 0 || similarity_section$conformal_alpha >= 1) {
    stop("Similarity field 'conformal_alpha' must be strictly between 0 and 1.", call. = FALSE)
  }

  similarity_species_n <- length(similarity_section$species_traits %||% list())
  similarity_study_n <- length(similarity_section$study_traits %||% list())
  if (similarity_species_n == 0L && similarity_study_n == 0L) {
    stop("Similarity trait weights must include at least one species or study trait.", call. = FALSE)
  }
  validate_weight_map(
    similarity_section$species_traits,
    scope = "species",
    registry_path = registry_path,
    allow_empty = TRUE
  )
  validate_weight_map(
    similarity_section$study_traits,
    scope = "study",
    registry_path = registry_path,
    allow_empty = TRUE
  )
  if (!is.null(similarity_section$alpha_range) || !is.null(similarity_section$alpha_grid) ||
    !is.null(similarity_section$kernel_scale_range) || !is.null(similarity_section$kernel_scale_grid)) {
    stop("Similarity tuning ranges belong under the top-level 'tuning' section.", call. = FALSE)
  }

  coherence <- similarity_section$coherence %||% list()
  for (field_name in c("length", "depth", "frequency")) {
    field_cfg <- coherence[[field_name]] %||% list()
    mode_value <- stringr::str_to_lower(stringr::str_squish(as.character(field_cfg$mode %||% "")))[[1]]
    if (!mode_value %in% c("overlap", "literal", "none")) {
      stop(sprintf("Similarity coherence field '%s.mode' must be one of: overlap, literal, none.", field_name), call. = FALSE)
    }
    if (field_name %in% c("length", "depth") && "min" %in% names(field_cfg)) {
      stop(
        sprintf(
          "Similarity coherence field '%s.min' is unsupported; length/depth overlap thresholds belong under 'admissibility.coherence.%s.min'.",
          field_name,
          field_name
        ),
        call. = FALSE
      )
    }
    for (numeric_name in intersect(c("weight", "gap"), names(field_cfg))) {
      field_value <- field_cfg[[numeric_name]]
      if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value)) {
        stop(sprintf("Similarity coherence field '%s.%s' must be one finite numeric value.", field_name, numeric_name), call. = FALSE)
      }
    }
    if (!is.null(field_cfg$range) || !is.null(field_cfg$grid)) {
      stop(sprintf("Similarity coherence tuning ranges belong under 'tuning.coherence.%s'.", field_name), call. = FALSE)
    }
  }

  invisible(NULL)
}

#' Validate the benchmark section
#'
#' @param benchmark_section Config `benchmark` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_benchmark_section <- function(benchmark_section) {
  if (is.null(benchmark_section)) {
    return(invisible(NULL))
  }

  if (!is.null(benchmark_section$workers) &&
    (!is.numeric(benchmark_section$workers) || length(benchmark_section$workers) != 1 ||
      !is.finite(benchmark_section$workers) || benchmark_section$workers < 1)) {
    stop("Benchmark field 'workers' must be one finite number >= 1.", call. = FALSE)
  }
  if (!is.null(benchmark_section$include_ts_error) &&
    (!is.logical(benchmark_section$include_ts_error) || length(benchmark_section$include_ts_error) != 1 ||
      is.na(benchmark_section$include_ts_error))) {
    stop("Benchmark field 'include_ts_error' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(benchmark_section$progress) &&
    (!is.logical(benchmark_section$progress) || length(benchmark_section$progress) != 1 ||
      is.na(benchmark_section$progress))) {
    stop("Benchmark field 'progress' must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(NULL)
}

#' Validate the Sentinel section
#'
#' @param sentinel_section Config `sentinel` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_sentinel_section <- function(sentinel_section) {
  if (is.null(sentinel_section)) {
    return(invisible(NULL))
  }
  if (!is.list(sentinel_section)) {
    stop("Sentinel config section must be a named list.", call. = FALSE)
  }

  logical_fields <- c(
    "include_ts_error", "throttle_inner_workers", "fast_validation",
    "progress", "logging", "save_case_artifacts"
  )
  for (field_name in logical_fields) {
    value <- sentinel_section[[field_name]]
    if (!is.null(value) &&
      (!is.logical(value) || length(value) != 1L || is.na(value))) {
      stop(
        sprintf("Sentinel field '%s' must be TRUE or FALSE.", field_name),
        call. = FALSE
      )
    }
  }

  path_fields <- c("cache_dir", "log_file")
  for (field_name in path_fields) {
    value <- sentinel_section[[field_name]]
    if (!is.null(value) &&
      (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(stringr::str_squish(value)))) {
      stop(
        sprintf("Sentinel field '%s' must be one non-empty path.", field_name),
        call. = FALSE
      )
    }
  }

  integer_fields <- c(
    "workers", "batch_size", "outer_repeats", "species_folds",
    "baseline_species_folds", "ablation_species_folds",
    "baseline_target_folds", "ablation_target_folds"
  )
  for (field_name in integer_fields) {
    value <- sentinel_section[[field_name]]
    minimum <- if (field_name %in% c("workers", "batch_size", "outer_repeats")) 1L else 2L
    if (!is.null(value) &&
      (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < minimum)) {
      stop(
        sprintf("Sentinel field '%s' must be one finite number >= %d.", field_name, minimum),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}

#' Validate a simple stage section
#'
#' @param stage_section Config stage section.
#' @param section_name Section name for error messages.
#' @param worker_field Optional worker-field name.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_stage_section <- function(stage_section,
                                   section_name,
                                   worker_field = NULL) {
  if (is.null(stage_section)) {
    return(invisible(NULL))
  }
  if (!is.null(stage_section$progress) &&
    (!is.logical(stage_section$progress) || length(stage_section$progress) != 1 ||
      is.na(stage_section$progress))) {
    stop(sprintf("%s field 'progress' must be TRUE or FALSE.", section_name), call. = FALSE)
  }
  if (!is.null(worker_field)) {
    field_value <- stage_section[[worker_field]]
    if (!is.null(field_value) &&
      (!is.numeric(field_value) || length(field_value) != 1 ||
        !is.finite(field_value) || field_value < 1)) {
      stop(
        sprintf("%s field '%s' must be one finite number >= 1.", section_name, worker_field),
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

#' Validate the admissibility section
#'
#' @param admissibility_section Config `admissibility` section.
#' @param registry_path Optional trait-registry path.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_admissibility_section <- function(admissibility_section,
                                           registry_path = NULL) {
  if (is.null(admissibility_section)) {
    return(invisible(NULL))
  }

  species_traits <- names(normalize_trait_map(admissibility_section$species_traits))
  species_traits <- species_traits[!is.na(species_traits) & nzchar(species_traits)]
  registry <- read_similarity_registry(registry_path = registry_path)
  unknown_species <- setdiff(species_traits, registry$species_names)
  if (length(unknown_species) > 0) {
    stop(
      sprintf(
        "Unknown config species trait name(s): %s",
        paste(unknown_species, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  study_traits <- names(normalize_trait_map(admissibility_section$study_traits))
  study_traits <- study_traits[!is.na(study_traits) & nzchar(study_traits)]
  unknown_study <- setdiff(study_traits, registry$study_names)
  if (length(unknown_study) > 0) {
    stop(
      sprintf(
        "Unknown config study trait name(s): %s",
        paste(unknown_study, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (trait_name in species_traits) {
    trait_defn <- registry$species_map[[trait_name]]
    trait_type <- trait_defn$data_type %||% "categorical"
    if (identical(trait_type, "numeric") && !identical(trait_name, "frequency")) {
      stop(
        sprintf(
          "Admissibility trait '%s' is numeric and cannot be used as a direct trait gate. Use the dedicated length/depth gates, the optional frequency gate, or categorical/set/binary traits instead.",
          trait_name
        ),
        call. = FALSE
      )
    }
  }
  for (trait_name in study_traits) {
    trait_defn <- registry$study_map[[trait_name]]
    trait_type <- trait_defn$data_type %||% "categorical"
    if (identical(trait_type, "numeric") && !identical(trait_name, "frequency")) {
      stop(
        sprintf(
          "Admissibility trait '%s' is numeric and cannot be used as a direct trait gate. Use the dedicated length/depth gates, the optional frequency gate, or categorical/set/binary traits instead.",
          trait_name
        ),
        call. = FALSE
      )
    }
  }

  for (field_name in c("key_metadata_max")) {
    field_value <- admissibility_section[[field_name]]
    if (is.null(field_value)) {
      next
    }
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) ||
      field_value < 0 || field_value > 1) {
      stop(
        sprintf("Admissibility field '%s' must be one finite numeric value in [0, 1].", field_name),
        call. = FALSE
      )
    }
  }

  coherence <- admissibility_section$coherence %||% list()
  for (field_name in c("length", "depth", "frequency")) {
    field_cfg <- coherence[[field_name]] %||% list()
    mode_value <- stringr::str_to_lower(stringr::str_squish(as.character(field_cfg$mode %||% "")))[[1]]
    if (!mode_value %in% c("overlap", "literal", "none")) {
      stop(sprintf("Admissibility coherence field '%s.mode' must be one of: overlap, literal, none.", field_name), call. = FALSE)
    }
    if ("weight" %in% names(field_cfg)) {
      stop(
        sprintf("Admissibility coherence field '%s.weight' is unsupported. Admissibility owns binary gate thresholds, not weights.", field_name),
        call. = FALSE
      )
    }
    for (numeric_name in intersect(c("min", "gap"), names(field_cfg))) {
      field_value <- field_cfg[[numeric_name]]
      if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 0) {
        stop(
          sprintf("Admissibility coherence field '%s.%s' must be one finite numeric value >= 0.", field_name, numeric_name),
          call. = FALSE
        )
      }
    }
  }

  if (!is.null(admissibility_section$progress) &&
    (!is.logical(admissibility_section$progress) || length(admissibility_section$progress) != 1 ||
      is.na(admissibility_section$progress))) {
    stop("Admissibility field 'progress' must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(NULL)
}

#' Validate the selection section
#'
#' @param selection_section Config `selection` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_selection_section <- function(selection_section) {
  if (is.null(selection_section)) {
    return(invisible(NULL))
  }

  numeric_fields <- c(
    "one_se_multiplier", "equivalence_tolerance", "n_boot", "seed",
    "u_tol_rel",
    "u_tol_abs",
    "uncertainty_relative_tolerance",
    "uncertainty_absolute_tolerance",
    "local_distance_tolerance"
  )
  for (field_name in numeric_fields) {
    field_value <- selection_section[[field_name]]
    if (is.null(field_value)) {
      next
    }
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 0) {
      stop(sprintf("Selection field '%s' must be one finite numeric value >= 0.", field_name), call. = FALSE)
    }
  }
  if (!is.null(selection_section$uncertainty_rule)) {
    rule <- normalize_uncertainty_rule(selection_section$uncertainty_rule)
    if (!rule %in% c("min", "tolerance", "one_se")) {
      stop(
        "Selection field 'uncertainty_rule' must be one of: 'min', 'tolerance', 'one_se'.",
        call. = FALSE
      )
    }
  }
  for (field_name in c("conformal_alpha", "bin_alpha")) {
    field_value <- selection_section[[field_name]]
    if (is.null(field_value)) {
      next
    }
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) ||
      field_value <= 0 || field_value >= 1) {
      stop(sprintf("Selection field '%s' must be strictly between 0 and 1.", field_name), call. = FALSE)
    }
  }
  for (field_name in c("min_bin_scores", "n_bins")) {
    field_value <- selection_section[[field_name]]
    if (is.null(field_value)) {
      next
    }
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1) {
      stop(sprintf("Selection field '%s' must be one finite number >= 1.", field_name), call. = FALSE)
    }
  }
  if (!is.null(selection_section$use_support_bin_intervals) &&
    (!is.logical(selection_section$use_support_bin_intervals) ||
      length(selection_section$use_support_bin_intervals) != 1 ||
      is.na(selection_section$use_support_bin_intervals))) {
    stop("Selection field 'use_support_bin_intervals' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(selection_section$support_bin_labels)) {
    resolve_post_selection_support_labels(
      labels = selection_section$support_bin_labels,
      n_bins = selection_section$n_bins %||% 3L
    )
  }
  if (!is.null(selection_section$progress) &&
    (!is.logical(selection_section$progress) || length(selection_section$progress) != 1 ||
      is.na(selection_section$progress))) {
    stop("Selection field 'progress' must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(NULL)
}

#' Validate the learner fields of a selection/uncertainty section
#'
#' Each stage now owns a self-contained learner block (there is no shared
#' metalearner section). Validates the `method`, `super_methods`,
#' `method_settings`, `loss`, fold/worker, and tolerance fields.
#'
#' @param learner_section Config `selection` or `uncertainty` section.
#' @param section_name Human-facing stage label for error messages.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_learner_section <- function(learner_section, section_name = "Selection") {
  if (is.null(learner_section)) {
    return(invisible(NULL))
  }

  integer_fields <- c("n_folds", "inner_folds", "workers")
  for (field_name in integer_fields) {
    field_value <- learner_section[[field_name]]
    if (is.null(field_value)) {
      next
    }
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1) {
      stop(sprintf("%s field '%s' must be one finite number >= 1.", section_name, field_name), call. = FALSE)
    }
  }

  if (!is.null(learner_section$outcome_clip_quantile)) {
    clip_q <- suppressWarnings(as.numeric(learner_section$outcome_clip_quantile)[[1]])
    if (!is.finite(clip_q) || clip_q <= 0 || clip_q > 1) {
      stop(
        sprintf("%s field 'outcome_clip_quantile' must be one finite number in (0, 1].", section_name),
        call. = FALSE
      )
    }
  }

  method_settings <- normalize_meta_policy_method_settings(
    learner_section$method_settings %||% NULL,
    defaults_path = learner_section$method_defaults_path %||% NULL
  )
  method_catalog <- meta_policy_method_catalog(method_settings = method_settings)
  allowed_methods <- c("super_learner", method_catalog$methods)

  method_value <- learner_section[["method", exact = TRUE]]
  if (!is.null(method_value)) {
    method_value <- stringr::str_squish(as.character(method_value))
    if (length(method_value) != 1 || !method_value %in% allowed_methods) {
      stop(
        sprintf(
          "%s field 'method' must be one of: %s.",
          section_name,
          paste(allowed_methods, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (!is.null(learner_section$loss)) {
    loss_now <- stringr::str_squish(as.character(learner_section$loss))
    loss_spec <- if (length(loss_now) == 1) {
      try(parse_super_learner_loss(loss_now), silent = TRUE)
    } else {
      NULL
    }
    if (is.null(loss_spec) || inherits(loss_spec, "try-error")) {
      stop(
        sprintf(
          paste(
            "%s field 'loss' must be 'squared_error', 'absolute_error', or a",
            "two-digit pinball loss such as 'pinball_q90'."
          ),
          section_name
        ),
        call. = FALSE
      )
    }
    if (identical(loss_spec$kind, "absolute_error") &&
      identical(
        stringr::str_squish(as.character(learner_section[["method", exact = TRUE]] %||% "")),
        "super_learner"
      )) {
      stop(
        sprintf(
          paste(
            "%s field 'loss' must be 'squared_error' or a pinball loss such as",
            "'pinball_q90' when 'method' is 'super_learner'."
          ),
          section_name
        ),
        call. = FALSE
      )
    }
  }

  if (!is.null(learner_section$super_methods)) {
    methods_now <- stringr::str_squish(as.character(unlist(
      learner_section$super_methods,
      use.names = FALSE
    )))
    bad_methods <- setdiff(methods_now, method_catalog$methods)
    if (length(bad_methods) > 0) {
      stop(
        sprintf(
          "%s field 'super_methods' contains unsupported method(s): %s",
          section_name,
          paste(bad_methods, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    method_now <- stringr::str_squish(as.character(learner_section[["method", exact = TRUE]] %||% ""))
    if (!identical(method_now, "super_learner")) {
      stop(
        sprintf("%s field 'super_methods' is only valid when 'method = \"super_learner\"'.", section_name),
        call. = FALSE
      )
    }
  }

  if (!is.null(learner_section$method_settings)) {
    validate_metalearner_method_settings(method_settings)
  }

  if (!is.null(learner_section$screen_learners)) {
    screen_cfg <- learner_section$screen_learners
    if (!is.list(screen_cfg)) {
      stop(sprintf("%s field 'screen_learners' must be a named list.", section_name), call. = FALSE)
    }
    bad_screen_fields <- setdiff(names(screen_cfg), c("n_folds", "seed"))
    if (length(bad_screen_fields) > 0L) {
      stop(
        sprintf(
          "%s field 'screen_learners' contains unsupported field(s): %s",
          section_name,
          paste(bad_screen_fields, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    if (is.null(screen_cfg$n_folds)) {
      stop(sprintf("%s field 'screen_learners.n_folds' is required.", section_name), call. = FALSE)
    }
    if (!is.numeric(screen_cfg$n_folds) ||
      length(screen_cfg$n_folds) != 1 ||
      !is.finite(screen_cfg$n_folds) ||
      screen_cfg$n_folds < 2) {
      stop(sprintf("%s field 'screen_learners.n_folds' must be one finite number >= 2.", section_name), call. = FALSE)
    }
    if (!is.null(screen_cfg$seed) &&
      (!is.numeric(screen_cfg$seed) ||
        length(screen_cfg$seed) != 1 ||
        !is.finite(screen_cfg$seed))) {
      stop(sprintf("%s field 'screen_learners.seed' must be NULL or one finite number.", section_name), call. = FALSE)
    }
  }

  if (!is.null(learner_section$max_selection_tolerance) &&
    (!is.numeric(learner_section$max_selection_tolerance) || length(learner_section$max_selection_tolerance) != 1 ||
      !is.finite(learner_section$max_selection_tolerance) || learner_section$max_selection_tolerance < 0)) {
    stop(sprintf("%s field 'max_selection_tolerance' must be one finite number >= 0.", section_name), call. = FALSE)
  }

  invisible(NULL)
}

#' Validate learner-specific method settings
#'
#' @param method_settings Metalearner `method_settings` list.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_metalearner_method_settings <- function(method_settings) {
  method_settings <- strip_meta_policy_method_settings_controls(method_settings)
  if (!is.list(method_settings)) {
    stop("Metalearner field 'method_settings' must be a named list.", call. = FALSE)
  }

  allowed_sections <- union(
    names(meta_policy_method_family_map()),
    unname(unique(meta_policy_method_family_map()))
  )
  bad_sections <- setdiff(names(method_settings), allowed_sections)
  if (length(bad_sections) > 0) {
    stop(
      sprintf(
        "Metalearner field 'method_settings' contains unsupported section(s): %s",
        paste(bad_sections, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  validate_empty_settings <- function(settings, field_label) {
    if (!is.list(settings)) {
      stop(sprintf("Metalearner field '%s' must be a named list.", field_label), call. = FALSE)
    }
  }

  validate_glm_penalized_settings <- function(settings, field_label) {
    validate_empty_settings(settings, field_label)
    if (!is.null(settings$alpha)) {
      alpha_now <- suppressWarnings(as.numeric(settings$alpha)[[1]])
      if (!is.finite(alpha_now) || alpha_now < 0 || alpha_now > 1) {
        stop(sprintf("Metalearner field '%s.alpha' must be one finite number between 0 and 1.", field_label), call. = FALSE)
      }
    }
    if (!is.null(settings$standardize) &&
      (!is.logical(settings$standardize) || length(settings$standardize) != 1 || is.na(settings$standardize))) {
      stop(sprintf("Metalearner field '%s.standardize' must be TRUE or FALSE.", field_label), call. = FALSE)
    }
    if (!is.null(settings$type_measure)) {
      type_measure <- stringr::str_squish(as.character(settings$type_measure))
      if (length(type_measure) != 1 || !type_measure %in% c("mae", "mse", "deviance")) {
        stop(sprintf("Metalearner field '%s.type_measure' must be one of: mae, mse, deviance.", field_label), call. = FALSE)
      }
    }
  }

  validate_gam_settings <- function(settings, field_label) {
    validate_empty_settings(settings, field_label)
    if (!is.null(settings$fit_method)) {
      fit_method <- stringr::str_squish(as.character(settings$fit_method))
      if (length(fit_method) != 1 || !fit_method %in% c("REML", "ML", "GCV.Cp")) {
        stop(sprintf("Metalearner field '%s.fit_method' must be one of: REML, ML, GCV.Cp.", field_label), call. = FALSE)
      }
    }
    if (!is.null(settings$select_terms) &&
      (!is.logical(settings$select_terms) || length(settings$select_terms) != 1 || is.na(settings$select_terms))) {
      stop(sprintf("Metalearner field '%s.select_terms' must be TRUE or FALSE.", field_label), call. = FALSE)
    }
  }

  validate_lmm_settings <- function(settings, field_label) {
    validate_empty_settings(settings, field_label)
    if (!is.null(settings$group_cols)) {
      stop(sprintf("Metalearner field '%s.group_cols' has been replaced by '%s.random_intercept'.", field_label, field_label), call. = FALSE)
    }
    if (!is.null(settings$fit_method)) {
      fit_method <- stringr::str_squish(as.character(settings$fit_method))
      if (length(fit_method) != 1 || !fit_method %in% c("REML", "ML")) {
        stop(sprintf("Metalearner field '%s.fit_method' must be one of: REML, ML.", field_label), call. = FALSE)
      }
    }
    if (!is.null(settings$random_intercept)) {
      random_intercept <- as.character(unlist(settings$random_intercept, use.names = FALSE))
      if (length(random_intercept) != 1L || anyNA(random_intercept) || !nzchar(stringr::str_squish(random_intercept))) {
        stop(sprintf("Metalearner field '%s.random_intercept' must contain exactly one non-empty column name.", field_label), call. = FALSE)
      }
    }
  }

  validate_qreg_settings <- function(settings, field_label) {
    validate_empty_settings(settings, field_label)
    if (!is.null(settings$tau)) {
      tau_now <- suppressWarnings(as.numeric(settings$tau)[[1]])
      if (!is.finite(tau_now) || tau_now <= 0 || tau_now >= 1) {
        stop(sprintf("Metalearner field '%s.tau' must be one finite number strictly between 0 and 1.", field_label), call. = FALSE)
      }
    }
    if (!is.null(settings$fit_method)) {
      fit_method <- stringr::str_squish(as.character(settings$fit_method))
      if (length(fit_method) != 1 || !fit_method %in% c("br", "fn", "fnb", "pfn")) {
        stop(sprintf("Metalearner field '%s.fit_method' must be one of: br, fn, fnb, pfn.", field_label), call. = FALSE)
      }
    }
  }

  validate_rpart_settings <- function(settings, field_label) {
    validate_empty_settings(settings, field_label)
    if (!is.null(settings$cp) && (!is.numeric(settings$cp) || length(settings$cp) != 1 || !is.finite(settings$cp) || settings$cp < 0)) {
      stop(sprintf("Metalearner field '%s.cp' must be one finite number >= 0.", field_label), call. = FALSE)
    }
    for (field_name in c("minsplit", "minbucket", "maxdepth")) {
      field_value <- settings[[field_name]]
      if (!is.null(field_value) && (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1)) {
        stop(sprintf("Metalearner field '%s.%s' must be one finite number >= 1.", field_label, field_name), call. = FALSE)
      }
    }
  }

  validate_rf_settings <- function(settings, field_label) {
    validate_empty_settings(settings, field_label)
    for (field_name in c("num_trees", "min_node_size", "max_depth")) {
      field_value <- settings[[field_name]]
      if (!is.null(field_value) && (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1)) {
        stop(sprintf("Metalearner field '%s.%s' must be one finite number >= 1.", field_label, field_name), call. = FALSE)
      }
    }
    for (field_name in c("mtry", "sample_fraction")) {
      field_value <- settings[[field_name]]
      if (!is.null(field_value) && (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value <= 0)) {
        stop(sprintf("Metalearner field '%s.%s' must be one finite number > 0.", field_label, field_name), call. = FALSE)
      }
    }
    if (!is.null(settings$sample_fraction) && settings$sample_fraction > 1) {
      stop(sprintf("Metalearner field '%s.sample_fraction' must be no greater than 1.", field_label), call. = FALSE)
    }
    if (!is.null(settings$replace) && (!is.logical(settings$replace) || length(settings$replace) != 1 || is.na(settings$replace))) {
      stop(sprintf("Metalearner field '%s.replace' must be TRUE or FALSE.", field_label), call. = FALSE)
    }
    if (!is.null(settings$respect_unordered_factors)) {
      respect_value <- stringr::str_squish(as.character(settings$respect_unordered_factors))
      if (length(respect_value) != 1 || !respect_value %in% c("ignore", "order", "partition")) {
        stop(sprintf("Metalearner field '%s.respect_unordered_factors' must be one of: ignore, order, partition.", field_label), call. = FALSE)
      }
    }
  }

  validate_xgboost_settings <- function(settings, field_label) {
    validate_empty_settings(settings, field_label)
    for (field_name in c("nrounds", "max_depth", "nthread")) {
      field_value <- settings[[field_name]]
      if (!is.null(field_value) && (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1)) {
        stop(sprintf("Metalearner field '%s.%s' must be one finite number >= 1.", field_label, field_name), call. = FALSE)
      }
    }
    for (field_name in c("eta", "min_child_weight", "subsample", "colsample_bytree", "lambda", "alpha")) {
      field_value <- settings[[field_name]]
      if (!is.null(field_value) && (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 0)) {
        stop(sprintf("Metalearner field '%s.%s' must be one finite number >= 0.", field_label, field_name), call. = FALSE)
      }
    }
  }

  validate_mars_settings <- function(settings, field_label) {
    validate_empty_settings(settings, field_label)
    for (field_name in c("degree", "nprune")) {
      field_value <- settings[[field_name]]
      if (!is.null(field_value) && (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1)) {
        stop(sprintf("Metalearner field '%s.%s' must be one finite number >= 1.", field_label, field_name), call. = FALSE)
      }
    }
    if (!is.null(settings$penalty) && (!is.numeric(settings$penalty) || length(settings$penalty) != 1 || !is.finite(settings$penalty) || settings$penalty < 0)) {
      stop(sprintf("Metalearner field '%s.penalty' must be one finite number >= 0.", field_label), call. = FALSE)
    }
    if (!is.null(settings$pmethod)) {
      pmethod <- stringr::str_squish(as.character(settings$pmethod))
      if (length(pmethod) != 1 || !pmethod %in% c("backward", "none", "exhaustive", "forward", "seqrep", "cv")) {
        stop(sprintf("Metalearner field '%s.pmethod' is not a supported earth pruning method.", field_label), call. = FALSE)
      }
    }
  }

  validate_with_variants <- function(settings, field_label, validator) {
    settings <- settings %||% list()
    if (!is.list(settings)) {
      stop(sprintf("Metalearner field '%s' must be a named list.", field_label), call. = FALSE)
    }
    variants <- settings$variants %||% NULL
    base_settings <- settings
    base_settings$variants <- NULL
    validator(base_settings, field_label)
    if (is.null(variants)) {
      return(invisible(NULL))
    }
    if (!is.list(variants) || is.null(names(variants)) || anyNA(names(variants)) ||
      any(!nzchar(names(variants)))) {
      stop(sprintf("Metalearner field '%s.variants' must be a named list.", field_label), call. = FALSE)
    }
    for (variant_name in names(variants)) {
      variant_settings <- variants[[variant_name]]
      if (!is.list(variant_settings)) {
        stop(
          sprintf("Metalearner field '%s.variants.%s' must be a named list.", field_label, variant_name),
          call. = FALSE
        )
      }
      validator(
        merge_config_sections(base_settings, variant_settings),
        sprintf("%s.variants.%s", field_label, variant_name)
      )
    }
    invisible(NULL)
  }

  validate_with_variants(method_settings$glm_penalized %||% list(), "method_settings.glm_penalized", validate_glm_penalized_settings)
  validate_with_variants(method_settings$glm_ridge %||% list(), "method_settings.glm_ridge", validate_glm_penalized_settings)
  validate_with_variants(method_settings$glm_lasso %||% list(), "method_settings.glm_lasso", validate_glm_penalized_settings)
  validate_with_variants(method_settings$glm_elastic %||% list(), "method_settings.glm_elastic", validate_glm_penalized_settings)
  validate_with_variants(method_settings$qreg %||% list(), "method_settings.qreg", validate_qreg_settings)
  validate_with_variants(method_settings$gam %||% list(), "method_settings.gam", validate_gam_settings)
  validate_with_variants(method_settings$lmm %||% list(), "method_settings.lmm", validate_lmm_settings)
  validate_with_variants(method_settings$rpart %||% list(), "method_settings.rpart", validate_rpart_settings)
  validate_with_variants(method_settings$rf %||% list(), "method_settings.rf", validate_rf_settings)
  validate_with_variants(method_settings$xgboost %||% list(), "method_settings.xgboost", validate_xgboost_settings)
  validate_with_variants(method_settings$mars %||% list(), "method_settings.mars", validate_mars_settings)
  validate_with_variants(method_settings$qrf %||% list(), "method_settings.qrf", validate_rf_settings)
  validate_with_variants(method_settings$knn %||% list(), "method_settings.knn", validate_empty_settings)
  validate_with_variants(method_settings$cubist %||% list(), "method_settings.cubist", validate_empty_settings)
  validate_with_variants(method_settings$svr %||% list(), "method_settings.svr", validate_empty_settings)
  validate_with_variants(method_settings$gpr %||% list(), "method_settings.gpr", validate_empty_settings)
  for (bart_method in c("bart", "xbart", "wsbart", "vfbart", "rebart")) {
    validate_with_variants(method_settings[[bart_method]] %||% list(), paste0("method_settings.", bart_method), validate_empty_settings)
  }

  invisible(NULL)
}

#' Validate the post-selection conformal section
#' Validate the policy section
#'
#' @param policy_section Config `policy` section.
#' @param registry_path Optional trait-registry path.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_policy_section <- function(policy_section,
                                    registry_path = NULL) {
  # Validate the scalar policy controls before checking the selected traits and
  # their weights against the trait registry.
  scalar_fields <- c(
    "alpha", "k_species", "k_study", "max_frequency_gap_khz",
    "min_length_overlap_fraction", "min_depth_overlap_fraction",
    "missing_key_metadata_max_fraction", "length_overlap_weight",
    "depth_overlap_weight", "frequency_coherence_weight",
    "conformal_alpha"
  )
  for (field_name in scalar_fields) {
    field_value <- policy_section[[field_name]]
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value)) {
      stop(sprintf("Policy field '%s' must be one finite numeric value.", field_name), call. = FALSE)
    }
  }
  if (!is.null(policy_section$core_weight_cutoff) &&
    (!is.numeric(policy_section$core_weight_cutoff) || length(policy_section$core_weight_cutoff) != 1 ||
      !is.finite(policy_section$core_weight_cutoff))) {
    stop("Policy field 'core_weight_cutoff' must be one finite numeric value when supplied.", call. = FALSE)
  }

  if (policy_section$alpha <= 0 || policy_section$alpha >= 1) {
    stop("Policy field 'alpha' must be strictly between 0 and 1.", call. = FALSE)
  }
  if (policy_section$conformal_alpha <= 0 || policy_section$conformal_alpha >= 1) {
    stop("Policy field 'conformal_alpha' must be strictly between 0 and 1.", call. = FALSE)
  }

  allowed_modes <- c("none", "literal", "overlap")
  mode_value <- stringr::str_to_lower(stringr::str_squish(as.character(policy_section$frequency_coherence_mode %||% "")))[[1]]
  if (!mode_value %in% allowed_modes) {
    stop("Policy field 'frequency_coherence_mode' must be one of: none, literal, overlap.", call. = FALSE)
  }
  if (!is.logical(policy_section$require_same_frequency_label) ||
    length(policy_section$require_same_frequency_label) != 1 ||
    is.na(policy_section$require_same_frequency_label)) {
    stop("Policy field 'require_same_frequency_label' must be TRUE or FALSE.", call. = FALSE)
  }

  policy_species_n <- length(policy_section$species_traits %||% list())
  policy_study_n <- length(policy_section$study_traits %||% list())
  if (policy_species_n == 0L && policy_study_n == 0L) {
    stop("Policy trait weights must include at least one species or study trait.", call. = FALSE)
  }

  validate_weight_map(
    weight_map = policy_section$species_traits,
    scope = "species",
    registry_path = registry_path,
    allow_empty = TRUE
  )
  validate_weight_map(
    weight_map = policy_section$study_traits,
    scope = "study",
    registry_path = registry_path,
    allow_empty = TRUE
  )
}

#' Validate the policy-list section
#'
#' @param policies_section Config `policies` section.
#' @param policy_path Optional policy-registry path.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_policy_list_section <- function(policies_section,
                                         policy_path = NULL) {
  # Confirm that the active policy list is non-empty and entirely drawn from
  # the policy registry or from a valid constructor-resolvable canonical name.
  stale_fields <- intersect(c("branch", "branches", "equation_branch_filters"), names(policies_section))
  if (length(stale_fields) > 0L) {
    stop(
      sprintf(
        "Unsupported policies field(s): %s. Use 'slope_class'.",
        paste(stale_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  active_values <- stringr::str_squish(as.character(unlist(policies_section$active %||% character(0), use.names = FALSE)))
  active_values <- active_values[!is.na(active_values) & nzchar(active_values)]
  policies_section$active <- active_values

  if (length(active_values) == 0) {
    stop("Policies must include at least one active policy.", call. = FALSE)
  }

  known_values <- available_policy_names(policy_path = policy_path)
  unknown_values <- Filter(
    function(policy_name) {
      !policy_name %in% known_values &&
        !is.list(build_policy_definition_from_name(policy_name, policy_path = policy_path))
    },
    active_values
  )
  if (length(unknown_values) > 0) {
    stop(
      sprintf("Unknown config policy name(s): %s", paste(unknown_values, collapse = ", ")),
      call. = FALSE
    )
  }

  if (!is.null(policies_section$slope_class)) {
    normalize_policy_equation_branch_filters(policies_section$slope_class)
  }
}

#' Validate one config weight map
#'
#' @param weight_map Named numeric weight map.
#' @param scope Trait scope passed to [trait_names()].
#' @param registry_path Optional trait-registry path.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
validate_weight_map <- function(weight_map,
                                scope,
                                registry_path = NULL,
                                allow_empty = FALSE) {
  # Require named finite nonnegative weights and check the names directly
  # against the relevant trait registry scope.
  if (length(weight_map) == 0) {
    if (isTRUE(allow_empty)) {
      return(invisible(NULL))
    }
    stop(sprintf("%s trait weights must include at least one trait.", scope), call. = FALSE)
  }

  weight_names <- names(weight_map)
  if (is.null(weight_names) || any(!nzchar(weight_names))) {
    stop(sprintf("%s trait weights must be named by trait.", scope), call. = FALSE)
  }

  weight_values <- suppressWarnings(as.numeric(unlist(weight_map, use.names = FALSE)))
  if (any(!is.finite(weight_values) | weight_values < 0)) {
    stop(sprintf("%s trait weights must be finite and >= 0.", scope), call. = FALSE)
  }

  unknown_values <- setdiff(weight_names, trait_names(scope = scope, registry_path = registry_path))
  if (length(unknown_values) > 0) {
    stop(
      sprintf(
        "Unknown config %s trait name(s): %s",
        scope,
        paste(unknown_values, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}
