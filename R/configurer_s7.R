#' Generalized Configurer S7 Class
#'
#' `Configurer` stores one fully specified configuration as a
#' validated S7 object. Construction is explicit: callers must supply either a
#' YAML path or a complete nested R list. No packaged analysis template or
#' study-specific default is injected during S7 construction.
#'
#' @examples
#' cfg <- create_configurer(list(
#'   paths = list(
#'     input = "input.xlsx",
#'     output_root = "outputs",
#'     cache_folder = "cache",
#'     support_folder = "supplemental",
#'     area_file = "fao_areas.csv",
#'     log_path = "outputs/run.log"
#'   ),
#'   execution = list(
#'     strict_pdf = FALSE,
#'     run_multiplier = FALSE,
#'     write_log = FALSE
#'   ),
#'   tuning = list(
#'     species_model_limit = 2L,
#'     resamples = 8L
#'   ),
#'   similarity = list(
#'     alpha = 0.8,
#'     kernel_scale = 4,
#'     key_metadata_max = 0.25,
#'     core_weight_cutoff = 0.8,
#'     conformal_alpha = 0.1,
#'     species_traits = list(genus = 2, family = 1),
#'     study_traits = list(frequency = 1, fao_area = 1),
#'     coherence = list(
#'       length = list(mode = "overlap", weight = 2, min = 0.25),
#'       depth = list(mode = "overlap", weight = 3, min = 0.25),
#'       frequency = list(mode = "overlap", weight = 2, gap = 60)
#'     )
#'   ),
#'   policies = list(
#'     active = "all_models_weighted"
#'   ),
#'   metalearner = list(
#'     selection_method = "glm"
#'   )
#' ))
#' cfg@data$similarity$alpha
#'
#' \dontrun{
#' cfg <- configurer_from_yaml("path/to/config.yaml")
#' }
#'
#' @name Configurer-class
NULL

validate_extra_config_sections <- function(config) {
  required_sections <- c("paths", "execution", "tuning", "policy", "policies", "metalearner")
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

  if (is.null(config$policies) && !is.null(config$strategies)) {
    config$policies <- config$strategies
  }
  config <- normalize_config_aliases(config)
  config <- normalize_similarity_config_shape(config)
  config <- apply_cache_defaults(config)
  config <- normalize_active_policy_names(config)
  config$metalearner <- normalize_metalearner_section(config$metalearner %||% list())
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
  config$paths$input_file <- path_absolute(config$paths$input_file, base_dir = base_dir)
  config$paths$out_root <- path_absolute(config$paths$out_root, base_dir = base_dir)
  config$paths$cache_dir <- path_absolute(config$paths$cache_dir, base_dir = base_dir)
  if (!is.null(config$paths$log_file) && nzchar(config$paths$log_file)) {
    config$paths$log_file <- path_absolute(config$paths$log_file, base_dir = base_dir)
  } else {
    config$paths$log_file <- NULL
  }
  if (!is.null(config$paths$supplemental_dir)) {
    config$paths$supplemental_dir <- path_absolute(config$paths$supplemental_dir, base_dir = base_dir)
  }
  if (!is.null(config$paths$fao_polygon_csv)) {
    config$paths$fao_polygon_csv <- path_absolute(config$paths$fao_polygon_csv, base_dir = base_dir)
  }

  # Materialize the existing compatibility fields so downstream code can read
  # the validated S7-backed config without changing the execution layer here.
  config$alpha <- config$policy$alpha
  config$kernel_scale <- config$similarity$kernel_scale %||% NULL
  config$k_species <- config$similarity$kernel_scale %||% config$policy$k_species
  config$k_study <- config$similarity$kernel_scale %||% config$policy$k_study
  config$frequency_coherence_mode <-
    stringr::str_to_lower(
      stringr::str_squish(as.character(config$policy$frequency_coherence_mode %||% "overlap"))
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
  config$species_trait_cols <- names(config$policy$species_traits)
  config$study_trait_cols <- names(config$policy$study_traits)
  config$species_trait_weights <- config$policy$species_traits
  config$study_trait_weights <- config$policy$study_traits

  config
}

#' @export
#' @rdname Configurer-class
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
#' @param config A complete nested config list or an existing
#'   [Configurer] object.
#' @param base_dir Base directory used to resolve relative paths.
#' @param registry_path Optional trait-registry path used during validation.
#' @param policy_path Optional policy-registry path used during validation.
#'
#' @return A validated [Configurer] object whose `@data` slot contains the
#'   normalized config list.
#'
#' @examples
#' cfg <- create_configurer(list(
#'   paths = list(
#'     input = "input.xlsx",
#'     output_root = "outputs",
#'     cache_folder = "cache",
#'     support_folder = "supplemental",
#'     area_file = "fao_areas.csv",
#'     log_path = "outputs/run.log"
#'   ),
#'   execution = list(
#'     strict_pdf = FALSE,
#'     run_multiplier = FALSE,
#'     write_log = FALSE
#'   ),
#'   tuning = list(
#'     species_model_limit = 2L,
#'     resamples = 8L
#'   ),
#'   similarity = list(
#'     alpha = 0.8,
#'     kernel_scale = 4,
#'     key_metadata_max = 0.25,
#'     core_weight_cutoff = 0.8,
#'     conformal_alpha = 0.1,
#'     species_traits = list(genus = 2, family = 1),
#'     study_traits = list(frequency = 1, fao_area = 1),
#'     coherence = list(
#'       length = list(mode = "overlap", weight = 2, min = 0.25),
#'       depth = list(mode = "overlap", weight = 3, min = 0.25),
#'       frequency = list(mode = "overlap", weight = 2, gap = 60)
#'     )
#'   ),
#'   policies = list(
#'     active = "all_models_weighted"
#'   ),
#'   metalearner = list(
#'     selection_method = "glm"
#'   )
#' ))
#' cfg@data$paths$out_root
#'
#' @export
create_configurer <- function(config,
                              base_dir = getwd(),
                              registry_path = NULL,
                              policy_path = NULL) {
  if ((inherits(config, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
    return(config)
  }

  if (missing(config) || is.null(config)) {
    stop(
      paste(
        "'config' is required.",
        "Supply a complete nested config list."
      ),
      call. = FALSE
    )
  }

  if (!is.list(config)) {
    stop(
      "'config' must be a complete nested config list or a `Configurer` object.",
      call. = FALSE
    )
  }

  normalized_base_dir <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  normalized_config <- normalize_explicit_config(
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
#' cfg <- configurer_from_yaml("path/to/config.yaml")
#' cfg@data$policies$active
#' }
#'
#' @export
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

  create_configurer(
    config = yaml::read_yaml(path),
    base_dir = base_dir,
    registry_path = registry_path,
    policy_path = policy_path
  )
}

configurer_console_summary <- function(x) {
  data_list <- x@data
  policy_section <- data_list$policy %||% list()
  similarity_section <- data_list$similarity %||% list()
  execution_section <- data_list$execution %||% list()
  benchmark_section <- data_list$benchmark %||% list()
  learner_settings <- data_list$metalearner %||% list()

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
  branch_filters <- as.character(unlist(
    ((data_list$policies %||% list())$branch_filters %||% (data_list$policies %||% list())$equation_branch_filters %||% character(0)),
    use.names = FALSE
  ))
  branch_filters <- branch_filters[!is.na(branch_filters) & nzchar(branch_filters)]

  enabled_sections <- names(data_list)[vapply(data_list, function(value) {
    is.list(value) && length(value) > 0
  }, logical(1))]
  enabled_sections <- enabled_sections[!is.na(enabled_sections) & nzchar(enabled_sections)]

  cat("Configurer\n")
  cat("  base_dir: ", x@base_dir, "\n", sep = "")
  cat("  input: ", data_list$paths$input %||% data_list$paths$input_file %||% "", "\n", sep = "")
  cat("  output_root: ", data_list$paths$output_root %||% data_list$paths$out_root %||% "", "\n", sep = "")
  cat("  cache_dir: ", data_list$paths$cache_dir %||% data_list$paths$cache_folder %||% "", "\n", sep = "")
  cat("  species_traits: ", preview_values(species_traits), "\n", sep = "")
  cat("  study_traits: ", preview_values(study_traits), "\n", sep = "")
  cat("  active_policies: ", preview_values(active_policies), "\n", sep = "")
  cat("  branch_filters: ", preview_values(branch_filters), "\n", sep = "")
  cat("  selection_method: ", learner_settings$selection_method %||% "none", "\n", sep = "")
  cat("  uncertainty_method: ", learner_settings$uncertainty_method %||% learner_settings$selection_method %||% "none", "\n", sep = "")
  cat("  alpha: ", policy_section$alpha %||% similarity_section$alpha %||% NA_real_, "\n", sep = "")
  cat("  kernel_scale: ", similarity_section$kernel_scale %||% data_list$kernel_scale %||% NA_real_, "\n", sep = "")
  cat("  strict_pdf: ", execution_section$strict_pdf %||% NA, "\n", sep = "")
  cat("  refresh_benchmark: ", benchmark_section$refresh %||% NA, "\n", sep = "")
  cat("  sections: ", preview_values(enabled_sections), "\n", sep = "")
  invisible(x)
}

#' Print a `Configurer`
#'
#' @name print.Configurer
#'
#' @param x A [Configurer] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
S7::method(print_generic, Configurer) <- function(x, ...) {
  configurer_console_summary(x)
}

#' Show a `Configurer`
#'
#' @name show.Configurer
#'
#' @param object A [Configurer] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
S7::method(show_generic, Configurer) <- function(object) {
  configurer_console_summary(object)
}

#' Coerce a complete config to `Configurer`
#'
#' @param config A complete nested config list, a YAML path, or an
#'   existing [Configurer] object.
#' @param base_dir Base directory used to resolve relative paths.
#' @param registry_path Optional trait-registry path used during validation.
#' @param policy_path Optional policy-registry path used during validation.
#'
#' @return A validated [Configurer] object whose `@data` slot contains the
#'   normalized config list.
#'
#' @export
as_configurer <- function(config,
                          base_dir = getwd(),
                          registry_path = NULL,
                          policy_path = NULL) {
  if ((inherits(config, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
    return(config)
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

  create_configurer(
    config = config,
    base_dir = base_dir,
    registry_path = registry_path,
    policy_path = policy_path
  )
}


