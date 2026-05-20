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
#'
#' @export
read_trait_registry <- function(registry_path = NULL) {
  if (is.null(registry_path)) {
    # Resolve the packaged registry file directly from the installed package.
    registry_path <- system.file(
      "templates",
      "trait_registry.json",
      package = "tsbiomass"
    )
  }

  # Fail clearly when the package file is unavailable or the supplied path does
  # not exist.
  if (!nzchar(registry_path) || !file.exists(registry_path)) {
    stop(
      "Trait registry JSON could not be found. Supply 'registry_path' or reinstall the package.",
      call. = FALSE
    )
  }

  registry <- read_json_file(registry_path)

  validate_trait_registry(registry)
}

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
  species_names <- extract_trait_names(registry$species_traits)
  study_names <- extract_trait_names(registry$study_traits)

  out <- switch(
    scope,
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
  }

  section
}

#' Extract coded names from a trait section
#'
#' Pulls coded trait names from a validated trait-section list while preserving
#' the order defined in the registry JSON.
#'
#' @param traits Trait section list.
#'
#' @return A character vector of coded trait names.
#'
#' @keywords internal
extract_trait_names <- function(traits) {
  vapply(
    X = traits,
    FUN = function(x) x$coded_name %||% NA_character_,
    FUN.VALUE = character(1)
  )
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
read_similarity_config <- function(config) {
  # Accept either an in-memory list or a JSON path so the tuner can be used
  # from interactive R code or from a serialized workflow config.
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

#' Return the default workflow config
#'
#' Builds the default YAML-backed workflow configuration used by the packaged
#' command-line interface.
#'
#' @param input_file Input workbook path.
#' @param out_root Output-root directory.
#' @param cache_dir Cache directory.
#'
#' @return A workflow-config list.
#'
#' @export
default_workflow_config <- function(input_file = "fishery_survey_tsl.xlsx",
                                    out_root = "outputs_swfscfish",
                                    cache_dir = "cache") {
  # Keep one canonical default config so both the YAML template and the command
  # line fallback mode resolve through the same baseline values.
  list(
    paths = list(
      input_file = input_file,
      out_root = out_root,
      cache_dir = cache_dir,
      supplemental_dir = "supplemental",
      fao_polygon_csv = "fao_areas.csv",
      log_file = file.path(out_root, "tsbiomass_workflow.log")
    ),
    workflow = list(
      strict_length_pdf = FALSE,
      run_multiplier_model = FALSE,
      write_log = FALSE
    ),
    tuning = list(
      max_models_per_species = 2L,
      n_resamples = 8L
    ),
    policy = list(
      alpha = 0.8,
      k_species = 4,
      k_study = 2,
      frequency_coherence_mode = "numeric",
      require_same_frequency_label = FALSE,
      max_frequency_gap_khz = 60,
      min_length_overlap_fraction = 0.25,
      min_depth_overlap_fraction = 0.25,
      missing_key_metadata_max_fraction = 0.25,
      length_overlap_weight = 2,
      depth_overlap_weight = 3,
      frequency_coherence_weight = 2,
      core_weight_cutoff = 0.8,
      conformal_alpha = 0.1,
      species_traits = list(
        swimbladder_type = 5,
        body_shape = 3,
        order = 2,
        family = 2,
        genus = 2,
        species = 4,
        temperature_midpoint = 1,
        temperature_range = 1,
        ocean_basin = 1,
        trophic = 1
      ),
      study_traits = list(
        length_metric = 1,
        frequency = 1,
        fao_area = 1
      )
    ),
    policies = list(
      active = c(
        "all_models_weighted",
        "top_support_subset_weighted",
        "same_species_closest",
        "closest_related_species",
        "same_genus_weighted",
        "same_family_weighted",
        "same_swimbladder_weighted",
        "same_cluster_weighted",
        "same_ellipse_closest",
        "same_ellipse_weighted",
        "closest_generalized_model",
        "generalized_models_weighted",
        "generalized_models_scan",
        "all_models_unweighted"
      )
    )
  )
}

#' Read a workflow YAML file
#'
#' Reads the packaged workflow YAML template or a caller-supplied workflow YAML
#' file without yet resolving relative paths.
#'
#' @param path Optional workflow YAML path.
#'
#' @return A workflow-config list.
#'
#' @export
read_workflow_config <- function(path = NULL) {
  # Use the packaged SWFSC fish template by default so the workflow has one
  # canonical serialized config example.
  if (is.null(path)) {
    path <- installed_template_path("swfscfish_config.yaml")
  }

  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be NULL or a single YAML file path.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("Workflow config does not exist: %s", path), call. = FALSE)
  }

  yaml::read_yaml(path)
}

#' Normalize a workflow config
#'
#' Merges user config onto the workflow defaults, converts legacy trait-weight
#' fields, resolves relative paths, and adds compatibility fields used by the
#' current workflow scripts.
#'
#' @param config Workflow-config list.
#' @param base_dir Base directory for relative paths.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path.
#'
#' @return A normalized workflow-config list.
#'
#' @export
normalize_workflow <- function(config,
                               base_dir = getwd(),
                               registry_path = NULL,
                               policy_path = NULL) {
  # Start from the package defaults so missing sections or fields do not force
  # every workflow YAML to restate the full config surface.
  if (!is.list(config)) {
    stop("'config' must be a list.", call. = FALSE)
  }
  if (!is.character(base_dir) || length(base_dir) != 1 || !nzchar(base_dir)) {
    stop("'base_dir' must be a single non-empty path.", call. = FALSE)
  }

  if (is.null(config$policies) && !is.null(config$strategies)) {
    config$policies <- config$strategies
  }

  workflow_config <- merge_cfg(default_workflow_config(), config)

  # Rebuild the policy trait maps from either the new compact fields or the
  # older column-plus-weight fields before validation.
  workflow_config$policy$species_traits <- normalize_trait_map(
    trait_map = workflow_config$policy$species_traits,
    trait_cols = workflow_config$policy$species_trait_cols,
    trait_weights = workflow_config$policy$species_trait_weights
  )
  workflow_config$policy$study_traits <- normalize_trait_map(
    trait_map = workflow_config$policy$study_traits,
    trait_cols = workflow_config$policy$study_trait_cols,
    trait_weights = workflow_config$policy$study_trait_weights
  )

  # Validate the fully merged config before resolving paths so structural
  # errors are reported against the config contents themselves.
  validate_workflow_config(
    config = workflow_config,
    registry_path = registry_path,
    policy_path = policy_path
  )

  # Resolve only the path fields once so downstream workflow code can use
  # absolute normalized paths consistently.
  workflow_config$paths$input_file <- path_absolute(workflow_config$paths$input_file, base_dir = base_dir)
  workflow_config$paths$out_root <- path_absolute(workflow_config$paths$out_root, base_dir = base_dir)
  workflow_config$paths$cache_dir <- path_absolute(workflow_config$paths$cache_dir, base_dir = base_dir)
  if (!is.null(workflow_config$paths$log_file) && nzchar(workflow_config$paths$log_file)) {
    workflow_config$paths$log_file <- path_absolute(workflow_config$paths$log_file, base_dir = base_dir)
  } else {
    workflow_config$paths$log_file <- NULL
  }

  if (!is.null(workflow_config$paths$supplemental_dir)) {
    workflow_config$paths$supplemental_dir <- path_absolute(
      workflow_config$paths$supplemental_dir,
      base_dir = base_dir
    )
  }
  if (!is.null(workflow_config$paths$fao_polygon_csv)) {
    workflow_config$paths$fao_polygon_csv <- path_absolute(
      workflow_config$paths$fao_polygon_csv,
      base_dir = base_dir
    )
  }

  # Add the flattened compatibility fields still used by the preserved workflow
  # scripts so the YAML can drive both old and refactored code paths.
  workflow_config$alpha <- workflow_config$policy$alpha
  workflow_config$k_species <- workflow_config$policy$k_species
  workflow_config$k_study <- workflow_config$policy$k_study
  workflow_config$frequency_coherence_mode <- normalize_frequency_mode(
    workflow_config$policy$frequency_coherence_mode
  )
  workflow_config$require_same_frequency_label <- workflow_config$policy$require_same_frequency_label
  workflow_config$max_frequency_gap_khz <- workflow_config$policy$max_frequency_gap_khz
  workflow_config$min_length_overlap_fraction <- workflow_config$policy$min_length_overlap_fraction
  workflow_config$min_depth_overlap_fraction <- workflow_config$policy$min_depth_overlap_fraction
  workflow_config$missing_key_metadata_max_fraction <- workflow_config$policy$missing_key_metadata_max_fraction
  workflow_config$length_overlap_weight <- workflow_config$policy$length_overlap_weight
  workflow_config$depth_overlap_weight <- workflow_config$policy$depth_overlap_weight
  workflow_config$frequency_coherence_weight <- workflow_config$policy$frequency_coherence_weight
  workflow_config$core_weight_cutoff <- workflow_config$policy$core_weight_cutoff
  workflow_config$conformal_alpha <- workflow_config$policy$conformal_alpha
  workflow_config$species_trait_cols <- names(workflow_config$policy$species_traits)
  workflow_config$study_trait_cols <- names(workflow_config$policy$study_traits)
  workflow_config$species_trait_weights <- workflow_config$policy$species_traits
  workflow_config$study_trait_weights <- workflow_config$policy$study_traits
  workflow_config
}

#' Validate a workflow config
#'
#' Validates the structure and registry-linked content of a workflow YAML
#' configuration.
#'
#' @param config Workflow-config list.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path.
#'
#' @return The validated workflow config.
#'
#' @export
validate_workflow_config <- function(config,
                                     registry_path = NULL,
                                     policy_path = NULL) {
  # Require the top-level sections first so all later field checks can assume a
  # stable nested config structure.
  if (!is.list(config)) {
    stop("'config' must be a list.", call. = FALSE)
  }

  if (is.null(config$policies) && !is.null(config$strategies)) {
    config$policies <- config$strategies
  }

  required_sections <- c("paths", "workflow", "tuning", "policy", "policies")
  missing_sections <- setdiff(required_sections, names(config))
  if (length(missing_sections) > 0) {
    stop(
      sprintf(
        "Workflow config is missing required section(s): %s",
        paste(missing_sections, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  validate_workflow_paths(config$paths)
  validate_workflow_flags(config$workflow, config$paths)
  validate_tuning_section(config$tuning)
  validate_policy_section(config$policy, registry_path = registry_path)
  validate_policy_list_section(config$policies, policy_path = policy_path)

  config
}

#' Build workflow option values
#'
#' Returns a named list of workflow options suitable for `options(...)`.
#'
#' @param config Normalized workflow-config list.
#'
#' @return Named list of option values.
#'
#' @export
workflow_option_values <- function(config) {
  # Expose the most commonly reused workflow scalars and paths through a small
  # options list so sourced scripts can read them consistently.
  validate_workflow_config(config)

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

#' Normalize one workflow trait map
#'
#' @param trait_map Optional named trait-weight map.
#' @param trait_cols Optional character vector of trait names.
#' @param trait_weights Optional named trait-weight map.
#'
#' @return Named numeric vector.
#' @keywords internal
normalize_trait_map <- function(trait_map,
                                trait_cols,
                                trait_weights) {
  # Start from an explicit trait map when present; otherwise rebuild one from
  # the older separate trait-column and trait-weight fields.
  if (!is.null(trait_map)) {
    if (is.data.frame(trait_map) && all(c("trait", "weight") %in% names(trait_map))) {
      weights <- as.numeric(trait_map$weight)
      names(weights) <- as.character(trait_map$trait)
      return(stats::setNames(weights, names(weights)))
    }

    if (is.list(trait_map) || is.atomic(trait_map)) {
      weights <- suppressWarnings(as.numeric(unlist(trait_map, use.names = FALSE)))
      names(weights) <- names(trait_map)
      return(stats::setNames(weights, names(weights)))
    }
  }

  # Treat the legacy trait-column vector as a weight-1 seed set, then let any
  # explicit legacy weight map override those defaults by name.
  weights <- numeric(0)
  if (!is.null(trait_cols)) {
    trait_cols <- stringr::str_squish(as.character(unlist(trait_cols, use.names = FALSE)))
    trait_cols <- trait_cols[!is.na(trait_cols) & nzchar(trait_cols)]
    weights <- stats::setNames(rep(1, length(trait_cols)), trait_cols)
  }

  if (!is.null(trait_weights)) {
    override_weights <- suppressWarnings(as.numeric(unlist(trait_weights, use.names = FALSE)))
    override_names <- names(trait_weights)
    if (is.null(override_names) || any(!nzchar(override_names))) {
      stop("Trait-weight maps must be named by trait.", call. = FALSE)
    }
    weights[override_names] <- override_weights
  }

  weights
}

#' Normalize one frequency mode
#'
#' @param mode Frequency coherence mode.
#'
#' @return Character scalar.
#' @keywords internal
normalize_frequency_mode <- function(mode) {
  # Accept either the current `numeric` label or the older `log_linear` label,
  # but preserve the legacy label expected by the preserved workflow scripts.
  mode <- stringr::str_to_lower(stringr::str_squish(as.character(mode %||% "numeric")))[[1]]
  if (identical(mode, "numeric")) {
    return("log_linear")
  }
  mode
}

#' Validate workflow paths
#'
#' @param paths_section Workflow `paths` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
validate_workflow_paths <- function(paths_section) {
  # Check only type and presence here so template paths can remain placeholders
  # until the workflow is actually run.
  required_fields <- c("input_file", "out_root", "cache_dir")
  missing_fields <- setdiff(required_fields, names(paths_section))
  if (length(missing_fields) > 0) {
    stop(
      sprintf("Workflow paths are missing field(s): %s", paste(missing_fields, collapse = ", ")),
      call. = FALSE
    )
  }

  for (field_name in required_fields) {
    field_value <- paths_section[[field_name]]
    if (!is.character(field_value) || length(field_value) != 1 || !nzchar(field_value)) {
      stop(sprintf("Workflow path '%s' must be a single non-empty string.", field_name), call. = FALSE)
    }
  }

  # Allow `log_file` to be omitted entirely because console logging is already
  # available during command-line runs.
  if (!is.null(paths_section$log_file) &&
      (!is.character(paths_section$log_file) || length(paths_section$log_file) != 1 || !nzchar(paths_section$log_file))) {
    stop("Workflow path 'log_file' must be NULL or a single non-empty string.", call. = FALSE)
  }
}

#' Validate workflow flags
#'
#' @param workflow_section Workflow `workflow` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
validate_workflow_flags <- function(workflow_section,
                                    paths_section) {
  # Require the known workflow booleans explicitly so any misshapen YAML values
  # fail before the workflow wrapper interprets them.
  flag_fields <- c("strict_length_pdf", "run_multiplier_model", "write_log")
  for (field_name in flag_fields) {
    field_value <- workflow_section[[field_name]]
    if (!is.logical(field_value) || length(field_value) != 1 || is.na(field_value)) {
      stop(sprintf("Workflow flag '%s' must be TRUE or FALSE.", field_name), call. = FALSE)
    }
  }

  # Require a log-file path only when file logging is explicitly enabled.
  if (isTRUE(workflow_section$write_log) &&
      (is.null(paths_section$log_file) || !nzchar(paths_section$log_file))) {
    stop("A 'paths.log_file' value is required when 'workflow.write_log = TRUE'.", call. = FALSE)
  }
}

#' Validate the tuning section
#'
#' @param tuning_section Workflow `tuning` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
validate_tuning_section <- function(tuning_section) {
  # Restrict the tuning controls to finite positive numeric scalars.
  numeric_fields <- c("max_models_per_species", "n_resamples")
  for (field_name in numeric_fields) {
    field_value <- tuning_section[[field_name]]
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1) {
      stop(sprintf("Tuning field '%s' must be one finite number >= 1.", field_name), call. = FALSE)
    }
  }
}

#' Validate the policy section
#'
#' @param policy_section Workflow `policy` section.
#' @param registry_path Optional trait-registry path.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
validate_policy_section <- function(policy_section,
                                    registry_path = NULL) {
  # Validate the scalar policy controls before checking the selected traits and
  # their weights against the trait registry.
  scalar_fields <- c(
    "alpha", "k_species", "k_study", "max_frequency_gap_khz",
    "min_length_overlap_fraction", "min_depth_overlap_fraction",
    "missing_key_metadata_max_fraction", "length_overlap_weight",
    "depth_overlap_weight", "frequency_coherence_weight",
    "core_weight_cutoff", "conformal_alpha"
  )
  for (field_name in scalar_fields) {
    field_value <- policy_section[[field_name]]
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value)) {
      stop(sprintf("Policy field '%s' must be one finite numeric value.", field_name), call. = FALSE)
    }
  }

  if (policy_section$alpha <= 0 || policy_section$alpha >= 1) {
    stop("Policy field 'alpha' must be strictly between 0 and 1.", call. = FALSE)
  }
  if (policy_section$conformal_alpha <= 0 || policy_section$conformal_alpha >= 1) {
    stop("Policy field 'conformal_alpha' must be strictly between 0 and 1.", call. = FALSE)
  }

  allowed_modes <- c("none", "label", "numeric", "log_linear")
  mode_value <- stringr::str_to_lower(stringr::str_squish(as.character(policy_section$frequency_coherence_mode %||% "")))[[1]]
  if (!mode_value %in% allowed_modes) {
    stop("Policy field 'frequency_coherence_mode' must be one of: none, label, numeric, log_linear.", call. = FALSE)
  }
  if (!is.logical(policy_section$require_same_frequency_label) ||
      length(policy_section$require_same_frequency_label) != 1 ||
      is.na(policy_section$require_same_frequency_label)) {
    stop("Policy field 'require_same_frequency_label' must be TRUE or FALSE.", call. = FALSE)
  }

  validate_weight_map(
    weight_map = policy_section$species_traits,
    scope = "species",
    registry_path = registry_path
  )
  validate_weight_map(
    weight_map = policy_section$study_traits,
    scope = "study",
    registry_path = registry_path
  )
}

#' Validate the policy-list section
#'
#' @param policies_section Workflow `policies` section.
#' @param policy_path Optional policy-registry path.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
validate_policy_list_section <- function(policies_section,
                                         policy_path = NULL) {
  # Confirm that the active policy list is non-empty and entirely drawn from
  # the policy registry.
  active_values <- stringr::str_squish(as.character(unlist(policies_section$active %||% character(0), use.names = FALSE)))
  active_values <- active_values[!is.na(active_values) & nzchar(active_values)]

  if (length(active_values) == 0) {
    stop("Workflow policies must include at least one active policy.", call. = FALSE)
  }

  unknown_values <- setdiff(active_values, policy_names(policy_path = policy_path))
  if (length(unknown_values) > 0) {
    stop(
      sprintf("Unknown workflow policy name(s): %s", paste(unknown_values, collapse = ", ")),
      call. = FALSE
    )
  }
}

#' Validate one workflow weight map
#'
#' @param weight_map Named numeric weight map.
#' @param scope Trait scope passed to [trait_names()].
#' @param registry_path Optional trait-registry path.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
validate_weight_map <- function(weight_map,
                                scope,
                                registry_path = NULL) {
  # Require named finite nonnegative weights and check the names directly
  # against the relevant trait registry scope.
  if (length(weight_map) == 0) {
    stop(sprintf("Workflow %s trait weights must include at least one trait.", scope), call. = FALSE)
  }

  weight_names <- names(weight_map)
  if (is.null(weight_names) || any(!nzchar(weight_names))) {
    stop(sprintf("Workflow %s trait weights must be named by trait.", scope), call. = FALSE)
  }

  weight_values <- suppressWarnings(as.numeric(unlist(weight_map, use.names = FALSE)))
  if (any(!is.finite(weight_values) | weight_values < 0)) {
    stop(sprintf("Workflow %s trait weights must be finite and >= 0.", scope), call. = FALSE)
  }

  unknown_values <- setdiff(weight_names, trait_names(scope = scope, registry_path = registry_path))
  if (length(unknown_values) > 0) {
    stop(
      sprintf(
        "Unknown workflow %s trait name(s): %s",
        scope,
        paste(unknown_values, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}
