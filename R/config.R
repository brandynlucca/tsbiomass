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

#' Return config aliases
#'
#' Defines the descriptive shorter config keys accepted at ingestion and the
#' canonical field names they map onto inside the current execution surface.
#'
#' @return Named list of alias maps by config section.
#' @keywords internal
config_aliases <- function() {
  list(
    paths = c(
      input = "input_file",
      output_root = "out_root",
      cache_folder = "cache_dir",
      support_folder = "supplemental_dir",
      area_file = "fao_polygon_csv",
      log_path = "log_file"
    ),
    execution = c(
      strict_pdf = "strict_length_pdf",
      run_multiplier = "run_multiplier_model"
    ),
    tuning = c(
      species_model_limit = "max_models_per_species",
      resamples = "n_resamples"
    ),
    policy = c(
      frequency_mode = "frequency_coherence_mode",
      exact_frequency = "require_same_frequency_label",
      frequency_gap = "max_frequency_gap_khz",
      length_overlap_min = "min_length_overlap_fraction",
      depth_overlap_min = "min_depth_overlap_fraction",
      key_metadata_max = "missing_key_metadata_max_fraction",
      length_weight = "length_overlap_weight",
      depth_weight = "depth_overlap_weight",
      frequency_weight = "frequency_coherence_weight"
    ),
    policies = c(
      branch_filters = "equation_branch_filters"
    )
  )
}

#' Return disallowed legacy config names
#'
#' @return Named list of legacy-to-current field-name mappings by section.
#' @keywords internal
legacy_config_names <- function() {
  list(
    paths = c(
      input_file = "input",
      out_root = "output_root",
      output_dir = "output_root",
      cache_dir = "cache_folder",
      supplemental_dir = "support_folder",
      support_dir = "support_folder",
      fao_polygon_csv = "area_file",
      log_file = "log_path"
    ),
    execution = c(
      strict_length_pdf = "strict_pdf",
      run_multiplier_model = "run_multiplier"
    ),
    tuning = c(
      max_models_per_species = "species_model_limit",
      n_resamples = "resamples"
    ),
    policy = c(
      frequency_coherence_mode = "frequency_mode",
      require_same_frequency_label = "exact_frequency",
      max_frequency_gap_khz = "frequency_gap",
      min_length_overlap_fraction = "length_overlap_min",
      min_depth_overlap_fraction = "depth_overlap_min",
      missing_key_metadata_max_fraction = "key_metadata_max",
      length_overlap_weight = "length_weight",
      depth_overlap_weight = "depth_weight",
      frequency_coherence_weight = "frequency_weight"
    ),
    policies = c(
      equation_branch_filters = "branch_filters"
    )
  )
}

#' Reject legacy config names
#'
#' @param config Raw config list.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
reject_legacy_config_names <- function(config) {
  if (!is.list(config)) {
    return(invisible(NULL))
  }

  if ("meta_policy" %in% names(config)) {
    stop(
      "Config section 'meta_policy' is no longer supported. Use 'metalearner' instead.",
      call. = FALSE
    )
  }

  legacy_map <- legacy_config_names()
  for (section_name in names(legacy_map)) {
    section <- config[[section_name]]
    if (is.null(section) || !is.list(section)) {
      next
    }
    for (legacy_name in names(legacy_map[[section_name]])) {
      if (!legacy_name %in% names(section)) {
        next
      }
      stop(
        sprintf(
          "Config section '%s' uses legacy field '%s'. Use '%s' instead.",
          section_name,
          legacy_name,
          legacy_map[[section_name]][[legacy_name]]
        ),
        call. = FALSE
      )
    }
  }

  metalearner_section <- config$metalearner
  if (is.list(metalearner_section)) {
    field_map <- c(
      method = "selection_method",
      super_methods = "selection_super_methods",
      width_method = "uncertainty_method",
      width_feature_cols = "uncertainty_feature_cols"
    )
    for (legacy_name in names(field_map)) {
      if (legacy_name %in% names(metalearner_section)) {
        stop(
          sprintf(
            "Config section 'metalearner' uses legacy field '%s'. Use '%s' instead.",
            legacy_name,
            field_map[[legacy_name]]
          ),
          call. = FALSE
        )
      }
    }
  }

  invisible(NULL)
}

#' Apply config aliases to one section
#'
#' @param section Named config subsection.
#' @param aliases Named character vector of `short_name = canonical_name`.
#' @param section_name Section label for error messages.
#'
#' @return Normalized subsection.
#' @keywords internal
apply_config_aliases <- function(section,
                                 aliases,
                                 section_name) {
  if (is.null(section) || !is.list(section)) {
    return(section)
  }

  for (short_name in names(aliases)) {
    canonical_name <- aliases[[short_name]]
    if (!short_name %in% names(section)) {
      next
    }
    if (canonical_name %in% names(section)) {
      stop(
        sprintf(
          "Config section '%s' cannot contain both '%s' and '%s'.",
          section_name,
          short_name,
          canonical_name
        ),
        call. = FALSE
      )
    }
    section[[canonical_name]] <- section[[short_name]]
    section[[short_name]] <- NULL
  }

  section
}

#' Normalize config aliases
#'
#' Rewrites accepted descriptive short keys onto the current canonical field
#' names before validation and downstream use.
#'
#' @param config Config list.
#'
#' @return Config list with canonical section keys.
#' @keywords internal
normalize_config_aliases <- function(config) {
  if (!is.list(config)) {
    return(config)
  }

  alias_map <- config_aliases()
  for (section_name in names(alias_map)) {
    if (section_name %in% names(config)) {
      config[[section_name]] <- apply_config_aliases(
        section = config[[section_name]],
        aliases = alias_map[[section_name]],
        section_name = section_name
      )
    }
  }

  config
}

#' Normalize the external similarity config surface
#'
#' @param config Config list.
#'
#' @return Config list with both `similarity` and internal `policy`
#'   representations populated.
#'
#' @keywords internal
normalize_similarity_config_shape <- function(config) {
  if (!is.list(config)) {
    return(config)
  }

  # Treat `similarity` as the distance-and-tuning surface and `admissibility`
  # as the binary gate surface. The older internal `policy` section is still
  # materialized for compatibility, but similarity weights stay owned by the
  # similarity section while admissibility owns only gate fields and thresholds.
  similarity <- config$similarity %||% config$policy %||% list()
  if (!is.list(similarity)) {
    similarity <- list()
  }
  admissibility <- config$admissibility %||% list()
  if (!is.list(admissibility)) {
    admissibility <- list()
  }

  policy_cfg <- config$policy %||% list()
  if (is.null(config$similarity) && is.list(policy_cfg)) {
    similarity <- merge_cfg(
      list(
        alpha = policy_cfg$alpha %||% NULL,
        k_species = policy_cfg$k_species %||% NULL,
        k_study = policy_cfg$k_study %||% NULL,
        species_traits = policy_cfg$species_traits %||% list(),
        study_traits = policy_cfg$study_traits %||% list(),
        conformal_alpha = policy_cfg$conformal_alpha %||% NULL,
        coherence = list(
          length = list(
            mode = "overlap",
            weight = policy_cfg$length_overlap_weight %||% NULL
          ),
          depth = list(
            mode = "overlap",
            weight = policy_cfg$depth_overlap_weight %||% NULL
          ),
          frequency = list(
            mode = if (identical(policy_cfg$frequency_coherence_mode %||% "overlap", "literal")) "literal" else "overlap",
            weight = policy_cfg$frequency_coherence_weight %||% NULL,
            gap = policy_cfg$max_frequency_gap_khz %||% NULL
          )
        )
      ),
      similarity
    )
  }
  if (is.null(config$admissibility) && is.list(policy_cfg)) {
    admissibility <- merge_cfg(
      list(
        species_traits = character(0),
        study_traits = character(0),
        key_metadata_max = policy_cfg$missing_key_metadata_max_fraction %||% NULL,
        coherence = list(
          length = list(
            mode = "overlap",
            min = policy_cfg$min_length_overlap_fraction %||% NULL
          ),
          depth = list(
            mode = "overlap",
            min = policy_cfg$min_depth_overlap_fraction %||% NULL
          ),
          frequency = list(
            mode = if (identical(policy_cfg$frequency_coherence_mode %||% "overlap", "literal")) "literal" else "overlap",
            gap = policy_cfg$max_frequency_gap_khz %||% NULL
          )
        )
      ),
      admissibility
    )
  }

  if (is.list(similarity$traits)) {
    similarity$species_traits <- similarity$species_traits %||% similarity$traits$species %||% list()
    similarity$study_traits <- similarity$study_traits %||% similarity$traits$study %||% list()
  }
  if (is.list(admissibility$traits)) {
    admissibility$species_traits <- admissibility$species_traits %||% admissibility$traits$species %||% list()
    admissibility$study_traits <- admissibility$study_traits %||% admissibility$traits$study %||% list()
  }

  coherence <- similarity$coherence %||% list()
  length_cfg <- coherence$length %||% list()
  depth_cfg <- coherence$depth %||% list()
  frequency_cfg <- coherence$frequency %||% list()

  similarity$length_mode <- similarity$length_mode %||% length_cfg$mode %||% coherence$mode %||% "overlap"
  similarity$depth_mode <- similarity$depth_mode %||% depth_cfg$mode %||% coherence$mode %||% "overlap"
  similarity$frequency_mode <- similarity$frequency_mode %||% frequency_cfg$mode %||% coherence$mode %||% "overlap"
  similarity$length_weight <- similarity$length_weight %||% length_cfg$weight %||% NULL
  similarity$depth_weight <- similarity$depth_weight %||% depth_cfg$weight %||% NULL
  similarity$frequency_weight <- similarity$frequency_weight %||% frequency_cfg$weight %||% NULL
  similarity$frequency_gap <- similarity$frequency_gap %||% frequency_cfg$gap %||% NULL
  similarity$kernel_scale <- similarity$kernel_scale %||% similarity$k_species %||% similarity$k_study %||% NULL

  frequency_mode_internal <- switch(stringr::str_to_lower(stringr::str_squish(as.character(similarity$frequency_mode %||% "overlap")))[[1]],
    overlap = "overlap",
    literal = "literal",
    none = "none",
    "overlap"
  )
  exact_frequency <- identical(frequency_mode_internal, "literal")

  admissibility_coherence <- admissibility$coherence %||% list()
  admissibility_length_cfg <- admissibility_coherence$length %||% list()
  admissibility_depth_cfg <- admissibility_coherence$depth %||% list()
  admissibility_frequency_cfg <- admissibility_coherence$frequency %||% list()
  admissibility$species_traits <- admissibility$species_traits %||% character(0)
  admissibility$study_traits <- admissibility$study_traits %||% character(0)
  admissibility$length_mode <- admissibility$length_mode %||% admissibility_length_cfg$mode %||% similarity$length_mode %||% "overlap"
  admissibility$depth_mode <- admissibility$depth_mode %||% admissibility_depth_cfg$mode %||% similarity$depth_mode %||% "overlap"
  admissibility$frequency_mode <- admissibility$frequency_mode %||% admissibility_frequency_cfg$mode %||% similarity$frequency_mode %||% "overlap"
  admissibility$length_overlap_min <- admissibility$length_overlap_min %||% admissibility_length_cfg$min %||% NULL
  admissibility$depth_overlap_min <- admissibility$depth_overlap_min %||% admissibility_depth_cfg$min %||% NULL
  admissibility$frequency_gap <- admissibility$frequency_gap %||% admissibility_frequency_cfg$gap %||% similarity$frequency_gap %||% NULL
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
  config$policy <- merge_cfg(
    config$policy %||% list(),
    list(
      alpha = similarity$alpha %||% NULL,
      k_species = similarity$kernel_scale %||% similarity$k_species %||% NULL,
      k_study = similarity$kernel_scale %||% similarity$k_study %||% NULL,
      frequency_coherence_mode = admissibility_frequency_mode_internal,
      require_same_frequency_label = admissibility_exact_frequency,
      max_frequency_gap_khz = admissibility$frequency_gap %||% NULL,
      min_length_overlap_fraction = admissibility$length_overlap_min %||% NULL,
      min_depth_overlap_fraction = admissibility$depth_overlap_min %||% NULL,
      missing_key_metadata_max_fraction = admissibility$key_metadata_max %||% NULL,
      length_overlap_weight = similarity$length_weight %||% NULL,
      depth_overlap_weight = similarity$depth_weight %||% NULL,
      frequency_coherence_weight = similarity$frequency_weight %||% NULL,
      core_weight_cutoff = similarity$core_weight_cutoff %||% policy_cfg$core_weight_cutoff %||% NULL,
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
apply_cache_defaults <- function(config) {
  if (!is.list(config)) {
    return(config)
  }

  cache_cfg <- config$cache %||% list()
  if (!is.list(cache_cfg)) {
    cache_cfg <- list()
  }
  defaults <- read_cache_defaults(cache_cfg$defaults_path %||% NULL)
  cache_names <- merge_cfg(defaults, cache_cfg$names %||% cache_cfg$files %||% list())
  cache_folder <- cache_cfg$folder %||% config$paths$cache_folder %||% "cache"
  cache_refresh <- cache_cfg$refresh %||% FALSE

  cache_file <- function(key) {
    file_name <- cache_names[[key]] %||% NULL
    if (is.null(file_name)) {
      return(NULL)
    }
    file.path(cache_folder, file_name)
  }

  config$cache <- merge_cfg(
    list(
      folder = cache_folder,
      refresh = cache_refresh,
      names = cache_names
    ),
    cache_cfg
  )

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
normalize_active_policy_names <- function(config,
                                          policy_path = NULL) {
  if (!is.list(config) || !is.list(config$policies)) {
    return(config)
  }

  policies_section <- config$policies
  registry <- read_policy_registry(policy_path = policy_path)
  policy_defs <- registry$policies %||% list()

  branch_field <- policies_section$branch %||%
    policies_section$branches %||%
    policies_section$branch_filters %||%
    policies_section$equation_branch_filters %||%
    NULL
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
  canonical_joint_order <- c(
    "class",
    "order",
    "family",
    "genus",
    "species",
    "swimbladder",
    "fao",
    "ocean_basin",
    "equation_form",
    "derivation",
    "length_metric",
    "pressure_corrected",
    "season",
    "diel"
  )

  # Preserve the flat active-policy syntax for backward compatibility when no
  # constructor-style group declarations were provided in the config YAML.
  if (is.null(group_field)) {
    active_values <- stringr::str_squish(as.character(unlist(policies_section$active %||% character(0), use.names = FALSE)))
    active_values <- active_values[!is.na(active_values) & nzchar(active_values)]
    policies_section$active <- active_values
    if (!is.null(branch_field)) {
      policies_section$equation_branch_filters <- canonicalize_branch(branch_field)
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
  policy_tbl <- dplyr::filter(policy_tbl, !is.na(policy), nzchar(policy))

  package_default_metrics <- c("closest", "weighted_mean", "unweighted_mean")
  package_default_branches <- "all"
  global_metrics <- canonicalize_metric(metric_field)
  global_branches <- if (is.null(branch_field)) package_default_branches else canonicalize_branch(branch_field)

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
        branches_now <- canonicalize_branch(group_value$branch %||% group_value$branches %||% global_branches)
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

  if (length(group_specs) > 0) {
    canonical_specs <- list()
    for (group_name in names(group_specs)) {
      canonical_group_name <- tryCatch(
        as.character(policy_group_definition(group_name, registry = registry)$key %||% group_name)[[1]],
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
          construct_policy_definition_for_group_metric(
            group_key = group_name,
            metric_key = metric_name,
            registry = registry
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
        dplyr::distinct(policy, .keep_all = TRUE)
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
      dplyr::filter(grouping_key == group_name, metric_key %in% metrics_now)
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
      policy_param_overrides[[policy_name]] <- list(equation_branch_filters = spec_now$branches)
    }
  }

  selected_policies <- unique(selected_policies)
  if (length(selected_policies) == 0) {
    stop("Constructor policy selection did not resolve to any active policies.", call. = FALSE)
  }

  policies_section$active <- selected_policies
  policies_section$policy_params <- policy_param_overrides
  policies_section$equation_branch_filters <- unique(unlist(
    lapply(policy_param_overrides, function(x) x$equation_branch_filters %||% character(0)),
    use.names = FALSE
  ))
  if (length(policies_section$equation_branch_filters) == 0) {
    policies_section$equation_branch_filters <- global_branches
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
normalize_selection_config_shape <- function(config) {
  if (!is.list(config)) {
    return(config)
  }

  selection <- config$selection %||% list()
  if (!is.list(selection)) {
    selection <- list()
  }
  legacy_conformal <- config$post_selection_conformal %||% list()
  if (!is.list(legacy_conformal)) {
    legacy_conformal <- list()
  }

  selection$conformal_alpha <- selection$conformal_alpha %||%
    legacy_conformal$conformal_alpha %||%
    legacy_conformal$alpha %||%
    (config$policy %||% list())$conformal_alpha %||%
    (config$similarity %||% list())$conformal_alpha %||%
    NULL
  selection$bin_alpha <- selection$bin_alpha %||% legacy_conformal$bin_alpha %||% NULL
  selection$min_bin_scores <- selection$min_bin_scores %||% legacy_conformal$min_bin_scores %||% NULL
  selection$n_bins <- selection$n_bins %||% legacy_conformal$n_bins %||% NULL
  selection$use_support_bin_intervals <- selection$use_support_bin_intervals %||%
    legacy_conformal$use_support_bin_intervals %||%
    NULL
  selection$support_bin_labels <- selection$support_bin_labels %||%
    legacy_conformal$support_bin_labels %||%
    NULL

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
  if ("branch" %in% names(input_policies)) {
    merged_cfg$policies$branch <- input_policies$branch
  }
  if ("branches" %in% names(input_policies)) {
    merged_cfg$policies$branches <- input_policies$branches
  }
  if ("branch_filters" %in% names(input_policies)) {
    merged_cfg$policies$branch_filters <- input_policies$branch_filters
  }
  if ("equation_branch_filters" %in% names(input_policies)) {
    merged_cfg$policies$equation_branch_filters <- input_policies$equation_branch_filters
  }
  if ("active" %in% names(input_policies)) {
    if (!any(c("group", "metric", "branch", "branches") %in% names(input_policies))) {
      merged_cfg$policies$group <- NULL
      merged_cfg$policies$metric <- NULL
      merged_cfg$policies$branch <- NULL
      merged_cfg$policies$branches <- NULL
    }
    merged_cfg$policies$active <- input_policies$active
  }

  merged_cfg
}

#' Return the default config
#'
#' Builds a pipeline-agnostic baseline configuration with neutral path
#' placeholders and registry-derived default trait and policy selections.
#'
#' @param input_file Input workbook path.
#' @param output_root Output root directory.
#' @param cache_folder Cache folder.
#' @param registry_path Optional trait-registry path used to derive default
#'   trait names.
#' @param policy_path Optional policy-registry path used to derive one default
#'   active policy.
#' @param use_canonical_names Logical scalar controlling whether canonical
#'   config names are used where legacy aliases are otherwise accepted.
#'
#' @return A config list.
#'
#' @export
default_config <- function(input_file = "input.xlsx",
                           output_root = "outputs",
                           cache_folder = "cache",
                           registry_path = NULL,
                           policy_path = NULL,
                           use_canonical_names = FALSE) {
  # Derive the minimal required trait and policy defaults from the registries
  # so the fallback config stays pipeline-agnostic.
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
      input = input_file,
      output_root = output_root,
      cache_folder = cache_folder,
      support_folder = "supplemental",
      area_file = "fao_areas.csv",
      log_path = file.path(output_root, "tsbiomass_run.log")
    ),
    execution = list(
      strict_pdf = FALSE,
      run_multiplier = FALSE,
      write_log = FALSE
    ),
    tuning = list(
      species_model_limit = 2L,
      resamples = 8L,
      n_cores = 1L,
      seed = NULL,
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
      equal_start_weights = FALSE,
      progress = FALSE
    ),
    similarity = list(
      alpha = 0.8,
      kernel_scale = 4,
      species_traits = stats::setNames(list(1), species_traits[[1]]),
      study_traits = stats::setNames(list(1), study_traits[[1]]),
      coherence = list(
        length = list(mode = "overlap", weight = 2, range = list(from = 0.5, to = 6)),
        depth = list(mode = "overlap", weight = 3, range = list(from = 0.5, to = 6)),
        frequency = list(mode = "overlap", weight = 2, range = list(from = 0.5, to = 6), gap = 60)
      ),
      conformal_alpha = 0.1,
      alpha_range = list(from = 0.1, to = 0.9),
      kernel_scale_range = list(from = 1, to = 8),
      progress = FALSE
    ),
    ordination = list(
      include_loadings = FALSE,
      include_centroids = FALSE,
      progress = FALSE
    ),
    policies = list(
      group = list("species"),
      metric = list("closest"),
      branch = list("all")
    ),
    cache = list(
      folder = cache_folder,
      refresh = FALSE,
      names = read_cache_defaults()
    ),
    benchmark = list(
      workers = 1L,
      include_ts_error = FALSE,
      progress = FALSE
    ),
    admissibility = list(
      species_traits = character(0),
      study_traits = character(0),
      coherence = list(
        length = list(mode = "overlap", min = 0.25),
        depth = list(mode = "overlap", min = 0.25),
        frequency = list(mode = "none", gap = 60)
      ),
      key_metadata_max = 0.25,
      progress = FALSE
    ),
    uncertainty = list(
      progress = FALSE
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
      progress = FALSE
    ),
    simulation = list(
      workers = 1L,
      progress = FALSE
    ),
    metalearner = list(
      selection_method = "glm",
      n_folds = 5L,
      inner_folds = 5L,
      workers = 1L,
      seed = NULL,
      outcome_col = "error_abs_log",
      outcome_clip_quantile = 0.99,
      outcome_transform = "log1p",
      lambda_rule = "lambda.1se",
      metalearner_loss = "squared_error",
      max_selection_tolerance = 1e-12,
      method_settings = default_meta_policy_method_settings(),
      progress = FALSE
    )
  ) |>
    (\(cfg_data) {
      if (isTRUE(use_canonical_names)) {
        normalize_config_aliases(cfg_data)
      } else {
        cfg_data
      }
    })()
}

#' Read a config YAML file
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
read_config <- function(path,
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
  reject_legacy_config_names(raw_config)

  normalize_config(
    config = raw_config,
    base_dir = base_dir,
    registry_path = registry_path,
    policy_path = policy_path
  )
}

#' Normalize a config
#'
#' Merges user config onto the package defaults, converts legacy trait-weight
#' fields, resolves relative paths, and adds compatibility fields used by the
#' packaged scripts.
#'
#' @param config Config list.
#' @param base_dir Base directory for relative paths.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path.
#'
#' @return A normalized config list.
#'
#' @export
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

  if (is.null(config$policies) && !is.null(config$strategies)) {
    config$policies <- config$strategies
  }
  if (!(is.list(config) &&
    "alpha" %in% names(config) &&
    "paths" %in% names(config) &&
    is.list(config$paths) &&
    all(c("input_file", "out_root", "cache_dir") %in% names(config$paths)))) {
    reject_legacy_config_names(config)
  }
  config <- normalize_config_aliases(config)
  config <- normalize_similarity_config_shape(config)
  config <- normalize_selection_config_shape(config)

  normalized_config <- merge_cfg(
    default_config(
      registry_path = registry_path,
      policy_path = policy_path,
      use_canonical_names = TRUE
    ),
    config
  )
  normalized_config <- replace_explicit_trait_maps(normalized_config, config)
  normalized_config <- normalize_config_aliases(normalized_config)
  normalized_config <- normalize_similarity_config_shape(normalized_config)
  normalized_config <- normalize_selection_config_shape(normalized_config)
  normalized_config <- apply_cache_defaults(normalized_config)
  normalized_config <- normalize_active_policy_names(normalized_config, policy_path = policy_path)
  normalized_config$metalearner <- normalize_metalearner_section(
    normalized_config$metalearner %||% list()
  )
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
  if (!is.null(normalized_config$paths$fao_polygon_csv)) {
    normalized_config$paths$fao_polygon_csv <- path_absolute(
      normalized_config$paths$fao_polygon_csv,
      base_dir = base_dir
    )
  }

  # Add the flattened compatibility fields still used by the preserved script
  # scripts so the YAML can drive both old and refactored code paths.
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
  normalized_config$species_trait_cols <- names(normalized_config$policy$species_traits)
  normalized_config$study_trait_cols <- names(normalized_config$policy$study_traits)
  normalized_config$species_trait_weights <- normalized_config$policy$species_traits
  normalized_config$study_trait_weights <- normalized_config$policy$study_traits
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
#' @export
validate_config <- function(config,
                            registry_path = NULL,
                            policy_path = NULL) {
  if ((inherits(config, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
    config <- config@data
  }

  # Require the top-level sections first so all later field checks can assume a
  # stable nested config structure.
  if (!is.list(config)) {
    stop("'config' must be a list.", call. = FALSE)
  }

  if (is.null(config$policies) && !is.null(config$strategies)) {
    config$policies <- config$strategies
  }
  config <- normalize_config_aliases(config)

  config <- normalize_similarity_config_shape(config)
  config <- normalize_selection_config_shape(config)
  config <- apply_cache_defaults(config)
  config <- normalize_active_policy_names(config, policy_path = policy_path)
  config$metalearner <- normalize_metalearner_section(config$metalearner %||% list())
  config <- normalize_trait_sections(config)

  required_sections <- c("paths", "execution", "tuning", "similarity", "policy", "policies", "metalearner")
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
  validate_selection_section(config$selection %||% NULL)
  validate_stage_section(config$simulation %||% NULL, "Simulation", worker_field = "workers")
  validate_metalearner_section(config$metalearner %||% NULL)
  validate_post_selection_conformal_section(config$post_selection_conformal %||% NULL)

  config
}

#' Normalize the metalearner section
#'
#' @param metalearner_section Config `metalearner` section.
#'
#' @return Normalized `metalearner` section.
#' @keywords internal
normalize_metalearner_section <- function(metalearner_section) {
  if (is.null(metalearner_section) || !is.list(metalearner_section)) {
    return(metalearner_section)
  }

  selection_method <- metalearner_section$selection_method %||% "glm"
  metalearner_section$method_settings <- normalize_meta_policy_method_settings(
    metalearner_section$method_settings %||% NULL
  )
  if (is.null(metalearner_section$uncertainty_method) ||
    (is.character(metalearner_section$uncertainty_method) &&
      length(metalearner_section$uncertainty_method) == 1 &&
      !nzchar(stringr::str_squish(metalearner_section$uncertainty_method)))) {
    metalearner_section$uncertainty_method <- selection_method
  }

  metalearner_section
}

#' Build config option values
#'
#' Returns a named list of options suitable for `options(...)`.
#'
#' @param config Normalized config list.
#'
#' @return Named list of option values.
#'
#' @export
config_option_values <- function(config) {
  if ((inherits(config, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
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
      trait_values <- unlist(trait_map, use.names = FALSE)
      trait_names_now <- names(trait_map)
      if (is.null(trait_names_now) || any(!nzchar(trait_names_now))) {
        trait_names_now <- stringr::str_squish(as.character(trait_values))
        trait_names_now <- trait_names_now[!is.na(trait_names_now) & nzchar(trait_names_now)]
        return(stats::setNames(rep(1, length(trait_names_now)), trait_names_now))
      }
      weights <- suppressWarnings(as.numeric(trait_values))
      return(stats::setNames(weights, trait_names_now))
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

#' Normalize config trait sections
#'
#' @param config Config list.
#'
#' @return Config list with normalized similarity weight maps and
#'   normalized admissibility gate-trait selections.
#' @keywords internal
normalize_trait_sections <- function(config) {
  if (!is.list(config)) {
    return(config)
  }

  config$policy <- config$policy %||% list()
  config$similarity <- config$similarity %||% list()
  config$admissibility <- config$admissibility %||% list()

  config$policy$species_traits <- normalize_trait_map(
    trait_map = config$policy$species_traits,
    trait_cols = config$policy$species_trait_cols,
    trait_weights = config$policy$species_trait_weights
  )
  config$policy$study_traits <- normalize_trait_map(
    trait_map = config$policy$study_traits,
    trait_cols = config$policy$study_trait_cols,
    trait_weights = config$policy$study_trait_weights
  )
  config$similarity$species_traits <- normalize_trait_map(
    trait_map = config$similarity$species_traits,
    trait_cols = names(config$policy$species_traits),
    trait_weights = config$policy$species_traits
  )
  config$similarity$study_traits <- normalize_trait_map(
    trait_map = config$similarity$study_traits,
    trait_cols = names(config$policy$study_traits),
    trait_weights = config$policy$study_traits
  )
  config$admissibility$species_traits <- names(normalize_trait_map(
    trait_map = config$admissibility$species_traits,
    trait_cols = NULL,
    trait_weights = NULL
  ))
  config$admissibility$species_traits <- unique(config$admissibility$species_traits[!is.na(config$admissibility$species_traits) & nzchar(config$admissibility$species_traits)])
  config$admissibility$study_traits <- names(normalize_trait_map(
    trait_map = config$admissibility$study_traits,
    trait_cols = NULL,
    trait_weights = NULL
  ))
  config$admissibility$study_traits <- unique(config$admissibility$study_traits[!is.na(config$admissibility$study_traits) & nzchar(config$admissibility$study_traits)])

  config
}

#' Validate config paths
#'
#' @param paths_section Config `paths` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
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
validate_execution_flags <- function(execution_section,
                                     paths_section) {
  # Require the known execution booleans explicitly so any misshapen YAML values
  # fail before the script wrapper interprets them.
  flag_fields <- c("strict_length_pdf", "run_multiplier_model", "write_log")
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
validate_tuning_section <- function(tuning_section) {
  # Restrict the tuning controls to finite positive numeric scalars.
  numeric_fields <- c("max_models_per_species", "n_resamples")
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
  if (!is.null(tuning_section$progress) &&
    (!is.logical(tuning_section$progress) ||
      length(tuning_section$progress) != 1 ||
      is.na(tuning_section$progress))) {
    stop("Tuning field 'progress' must be TRUE or FALSE.", call. = FALSE)
  }
}

#' Validate the cache section
#'
#' @param cache_section Config `cache` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
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

  if (!is.null(similarity_section$k_species) || !is.null(similarity_section$k_study) ||
    !is.null(similarity_section$k_species_range) || !is.null(similarity_section$k_study_range) ||
    !is.null(similarity_section$k_species_grid) || !is.null(similarity_section$k_study_grid)) {
    stop(
      "Similarity fields 'k_species'/'k_study' and their range/grid variants are unsupported legacy fields; use 'kernel_scale', 'kernel_scale_range', and optional 'kernel_scale_grid'.",
      call. = FALSE
    )
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

  validate_weight_map(similarity_section$species_traits, scope = "species", registry_path = registry_path)
  validate_weight_map(similarity_section$study_traits, scope = "study", registry_path = registry_path)
  validate_search_range(similarity_section$alpha_range %||% similarity_section$alpha_grid, "alpha_range", alpha_like = TRUE)
  validate_search_range(similarity_section$kernel_scale_range %||% similarity_section$kernel_scale_grid, "kernel_scale_range", alpha_like = FALSE)

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
    validate_search_range(field_cfg$range %||% field_cfg$weight_range %||% NULL, sprintf("coherence.%s.range", field_name), alpha_like = FALSE)
    validate_search_range(field_cfg$grid %||% field_cfg$weight_grid %||% NULL, sprintf("coherence.%s.grid", field_name), alpha_like = FALSE)
  }
  if (!is.null(similarity_section$progress) &&
    (!is.logical(similarity_section$progress) || length(similarity_section$progress) != 1 ||
      is.na(similarity_section$progress))) {
    stop("Similarity field 'progress' must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(NULL)
}

#' Validate the benchmark section
#'
#' @param benchmark_section Config `benchmark` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
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

#' Validate a simple stage section
#'
#' @param stage_section Config stage section.
#' @param section_name Section name for error messages.
#' @param worker_field Optional worker-field name.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
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
validate_admissibility_section <- function(admissibility_section,
                                           registry_path = NULL) {
  if (is.null(admissibility_section)) {
    return(invisible(NULL))
  }

  species_traits <- names(normalize_trait_map(
    trait_map = admissibility_section$species_traits,
    trait_cols = NULL,
    trait_weights = NULL
  ))
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

  study_traits <- names(normalize_trait_map(
    trait_map = admissibility_section$study_traits,
    trait_cols = NULL,
    trait_weights = NULL
  ))
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
validate_selection_section <- function(selection_section) {
  if (is.null(selection_section)) {
    return(invisible(NULL))
  }

  numeric_fields <- c(
    "tolerance", "one_se_multiplier", "equivalence_tolerance", "n_boot", "seed",
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

#' Validate the metalearner section
#'
#' @param metalearner_section Config `metalearner` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
validate_metalearner_section <- function(metalearner_section) {
  if (is.null(metalearner_section)) {
    return(invisible(NULL))
  }

  integer_fields <- c("n_folds", "inner_folds", "workers", "seed")
  for (field_name in integer_fields) {
    field_value <- metalearner_section[[field_name]]
    if (is.null(field_value)) {
      next
    }
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1) {
      stop(sprintf("Metalearner field '%s' must be one finite number >= 1.", field_name), call. = FALSE)
    }
  }

  if (!is.null(metalearner_section$outcome_clip_quantile)) {
    clip_q <- suppressWarnings(as.numeric(metalearner_section$outcome_clip_quantile)[[1]])
  if (!is.finite(clip_q) || clip_q <= 0 || clip_q > 1) {
      stop(
        "Metalearner field 'outcome_clip_quantile' must be one finite number in (0, 1].",
        call. = FALSE
      )
    }
  }
  normalized_method_settings <- normalize_meta_policy_method_settings(
    metalearner_section$method_settings %||% NULL
  )
  method_catalog <- meta_policy_method_catalog(
    method_settings = normalized_method_settings
  )
  allowed_methods <- c(
    "super_learner",
    method_catalog$methods
  )

  for (field_name in c("selection_method", "uncertainty_method")) {
    field_value <- metalearner_section[[field_name]]
    if (is.null(field_value)) {
      next
    }
    field_value <- stringr::str_squish(as.character(field_value))
    if (length(field_value) != 1 || !field_value %in% allowed_methods) {
      stop(
        sprintf(
          "Metalearner field '%s' must be one of: %s.",
          field_name,
          paste(allowed_methods, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  if (!is.null(metalearner_section$progress) &&
    (!is.logical(metalearner_section$progress) || length(metalearner_section$progress) != 1 ||
      is.na(metalearner_section$progress))) {
    stop("Metalearner field 'progress' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(metalearner_section$metalearner_loss)) {
    loss_now <- stringr::str_squish(as.character(metalearner_section$metalearner_loss))
    if (length(loss_now) != 1 || !loss_now %in% c("squared_error", "absolute_error")) {
      stop(
        "Metalearner field 'metalearner_loss' must be 'squared_error' or 'absolute_error'.",
        call. = FALSE
      )
    }
    if (identical(loss_now, "absolute_error") &&
      any(vapply(
        c("selection_method", "uncertainty_method"),
        function(field_name) {
          identical(
            stringr::str_squish(as.character(metalearner_section[[field_name]] %||% "")),
            "super_learner"
          )
        },
        logical(1)
      ))) {
      stop(
        paste(
          "Metalearner field 'metalearner_loss' must be 'squared_error' when",
          "either 'selection_method' or 'uncertainty_method' is 'super_learner'."
        ),
        call. = FALSE
      )
    }
  }

  if (!is.null(metalearner_section$selection_super_methods)) {
    methods_now <- stringr::str_squish(as.character(unlist(
      metalearner_section$selection_super_methods,
      use.names = FALSE
    )))
    allowed_super_methods <- method_catalog$methods
    bad_methods <- setdiff(methods_now, allowed_super_methods)
    if (length(bad_methods) > 0) {
      stop(
        sprintf(
          "Metalearner field 'selection_super_methods' contains unsupported method(s): %s",
          paste(bad_methods, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    selection_method_now <- stringr::str_squish(as.character(
      metalearner_section$selection_method %||% ""
    ))
    if (!identical(selection_method_now, "super_learner")) {
      stop(
        paste(
          "Metalearner field 'selection_super_methods' is only valid when",
          "'selection_method = \"super_learner\"'."
        ),
        call. = FALSE
      )
    }
  }

  if (!is.null(metalearner_section$method_settings)) {
    validate_metalearner_method_settings(normalized_method_settings)
  }

  if (!is.null(metalearner_section$max_selection_tolerance) &&
    (!is.numeric(metalearner_section$max_selection_tolerance) || length(metalearner_section$max_selection_tolerance) != 1 ||
      !is.finite(metalearner_section$max_selection_tolerance) || metalearner_section$max_selection_tolerance < 0)) {
    stop("Metalearner field 'max_selection_tolerance' must be one finite number >= 0.", call. = FALSE)
  }

  invisible(NULL)
}

#' Validate learner-specific method settings
#'
#' @param method_settings Metalearner `method_settings` list.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
validate_metalearner_method_settings <- function(method_settings) {
  if (!is.list(method_settings)) {
    stop("Metalearner field 'method_settings' must be a named list.", call. = FALSE)
  }

  allowed_sections <- c("glmnet", "quantreg", "gam", "rpart", "ranger", "xgboost")
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
  validate_variant_block <- function(settings, field_label, validator) {
    variants <- settings$variants %||% NULL
    if (is.null(variants)) {
      return(invisible(NULL))
    }
    if (!is.list(variants) || is.null(names(variants)) || anyNA(names(variants)) ||
      any(!nzchar(names(variants)))) {
      stop(
        sprintf("Metalearner field '%s.variants' must be a named list.", field_label),
        call. = FALSE
      )
    }
    for (variant_name in names(variants)) {
      variant_settings <- variants[[variant_name]]
      if (!is.list(variant_settings)) {
        stop(
          sprintf(
            "Metalearner field '%s.variants.%s' must be a named list.",
            field_label,
            variant_name
          ),
          call. = FALSE
        )
      }
      validator(variant_settings, paste0(field_label, ".variants.", variant_name))
    }
    invisible(NULL)
  }

  validate_glmnet_settings <- function(settings, field_label) {
    if (!is.list(settings)) {
      stop(sprintf("Metalearner field '%s' must be a named list.", field_label), call. = FALSE)
    }
    if (!is.null(settings$standardize) &&
      (!is.logical(settings$standardize) || length(settings$standardize) != 1 || is.na(settings$standardize))) {
      stop(sprintf("Metalearner field '%s.standardize' must be TRUE or FALSE.", field_label), call. = FALSE)
    }
    if (!is.null(settings$type_measure)) {
      type_measure <- stringr::str_squish(as.character(settings$type_measure))
      if (length(type_measure) != 1 || !type_measure %in% c("mae", "mse", "deviance")) {
        stop(
          sprintf(
            "Metalearner field '%s.type_measure' must be one of: mae, mse, deviance.",
            field_label
          ),
          call. = FALSE
        )
      }
    }
  }

  validate_gam_settings <- function(settings, field_label) {
    if (!is.list(settings)) {
      stop(sprintf("Metalearner field '%s' must be a named list.", field_label), call. = FALSE)
    }
    if (!is.null(settings$fit_method)) {
      fit_method <- stringr::str_squish(as.character(settings$fit_method))
      if (length(fit_method) != 1 || !fit_method %in% c("REML", "ML", "GCV.Cp")) {
        stop(
          sprintf(
            "Metalearner field '%s.fit_method' must be one of: REML, ML, GCV.Cp.",
            field_label
          ),
          call. = FALSE
        )
      }
    }
    if (!is.null(settings$select_terms) &&
      (!is.logical(settings$select_terms) || length(settings$select_terms) != 1 || is.na(settings$select_terms))) {
      stop(sprintf("Metalearner field '%s.select_terms' must be TRUE or FALSE.", field_label), call. = FALSE)
    }
  }

  validate_quantreg_settings <- function(settings, field_label) {
    if (!is.list(settings)) {
      stop(sprintf("Metalearner field '%s' must be a named list.", field_label), call. = FALSE)
    }
    if (!is.null(settings$tau)) {
      tau_now <- suppressWarnings(as.numeric(settings$tau)[[1]])
      if (!is.finite(tau_now) || tau_now <= 0 || tau_now >= 1) {
        stop(
          sprintf("Metalearner field '%s.tau' must be one finite number strictly between 0 and 1.", field_label),
          call. = FALSE
        )
      }
    }
    if (!is.null(settings$fit_method)) {
      fit_method <- stringr::str_squish(as.character(settings$fit_method))
      if (length(fit_method) != 1 || !fit_method %in% c("br", "fn", "fnb", "pfn")) {
        stop(
          sprintf(
            "Metalearner field '%s.fit_method' must be one of: br, fn, fnb, pfn.",
            field_label
          ),
          call. = FALSE
        )
      }
    }
    validate_variant_block(settings, field_label, validate_quantreg_settings)
  }

  validate_rpart_settings <- function(settings, field_label) {
    if (!is.list(settings)) {
      stop(sprintf("Metalearner field '%s' must be a named list.", field_label), call. = FALSE)
    }
    if (!is.null(settings$cp) &&
      (!is.numeric(settings$cp) || length(settings$cp) != 1 || !is.finite(settings$cp) || settings$cp < 0)) {
      stop(
        sprintf("Metalearner field '%s.cp' must be one finite number >= 0.", field_label),
        call. = FALSE
      )
    }
    for (field_name in c("minsplit", "minbucket", "maxdepth")) {
      field_value <- settings[[field_name]]
      if (!is.null(field_value) &&
        (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1)) {
        stop(
          sprintf("Metalearner field '%s.%s' must be one finite number >= 1.", field_label, field_name),
          call. = FALSE
        )
      }
    }
    validate_variant_block(settings, field_label, validate_rpart_settings)
  }

  validate_ranger_settings <- function(settings, field_label) {
    if (!is.list(settings)) {
      stop(sprintf("Metalearner field '%s' must be a named list.", field_label), call. = FALSE)
    }
    for (field_name in c("num_trees", "min_node_size")) {
      field_value <- settings[[field_name]]
      if (!is.null(field_value) &&
        (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1)) {
        stop(
          sprintf("Metalearner field '%s.%s' must be one finite number >= 1.", field_label, field_name),
          call. = FALSE
        )
      }
    }
    for (field_name in c("mtry", "sample_fraction")) {
      field_value <- settings[[field_name]]
      if (!is.null(field_value) &&
        (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value <= 0)) {
        stop(
          sprintf("Metalearner field '%s.%s' must be one finite number > 0.", field_label, field_name),
          call. = FALSE
        )
      }
    }
    if (!is.null(settings$replace) &&
      (!is.logical(settings$replace) || length(settings$replace) != 1 || is.na(settings$replace))) {
      stop(sprintf("Metalearner field '%s.replace' must be TRUE or FALSE.", field_label), call. = FALSE)
    }
    if (!is.null(settings$respect_unordered_factors)) {
      respect_value <- stringr::str_squish(as.character(settings$respect_unordered_factors))
      if (length(respect_value) != 1 || !respect_value %in% c("ignore", "order", "partition")) {
        stop(
          paste(
            sprintf("Metalearner field '%s.respect_unordered_factors'", field_label),
            "must be one of: ignore, order, partition."
          ),
          call. = FALSE
        )
      }
    }
    validate_variant_block(settings, field_label, validate_ranger_settings)
  }

  validate_xgboost_settings <- function(settings, field_label) {
    if (!is.list(settings)) {
      stop(sprintf("Metalearner field '%s' must be a named list.", field_label), call. = FALSE)
    }
    for (field_name in c("nrounds", "max_depth")) {
      field_value <- settings[[field_name]]
      if (!is.null(field_value) &&
        (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1)) {
        stop(
          sprintf("Metalearner field '%s.%s' must be one finite number >= 1.", field_label, field_name),
          call. = FALSE
        )
      }
    }
    for (field_name in c("eta", "min_child_weight", "subsample", "colsample_bytree", "lambda", "alpha")) {
      field_value <- settings[[field_name]]
      if (!is.null(field_value) &&
        (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 0)) {
        stop(
          sprintf("Metalearner field '%s.%s' must be one finite number >= 0.", field_label, field_name),
          call. = FALSE
        )
      }
    }
    validate_variant_block(settings, field_label, validate_xgboost_settings)
  }

  validate_quantreg_settings(method_settings$quantreg %||% list(), "method_settings.quantreg")
  validate_glmnet_settings(method_settings$glmnet %||% list(), "method_settings.glmnet")
  validate_gam_settings(method_settings$gam %||% list(), "method_settings.gam")
  validate_rpart_settings(method_settings$rpart %||% list(), "method_settings.rpart")
  validate_ranger_settings(method_settings$ranger %||% list(), "method_settings.ranger")
  validate_xgboost_settings(method_settings$xgboost %||% list(), "method_settings.xgboost")

  invisible(NULL)
}

#' Validate the post-selection conformal section
#'
#' @param conformal_section Config `post_selection_conformal` section.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
validate_post_selection_conformal_section <- function(conformal_section) {
  if (is.null(conformal_section)) {
    return(invisible(NULL))
  }

  for (field_name in c("alpha", "bin_alpha")) {
    field_value <- conformal_section[[field_name]]
    if (is.null(field_value)) {
      next
    }
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) ||
      field_value <= 0 || field_value >= 1) {
      stop(sprintf("Post-selection conformal field '%s' must be strictly between 0 and 1.", field_name), call. = FALSE)
    }
  }

  for (field_name in c("min_bin_scores", "n_bins")) {
    field_value <- conformal_section[[field_name]]
    if (is.null(field_value)) {
      next
    }
    if (!is.numeric(field_value) || length(field_value) != 1 || !is.finite(field_value) || field_value < 1) {
      stop(sprintf("Post-selection conformal field '%s' must be one finite number >= 1.", field_name), call. = FALSE)
    }
  }

  if (!is.null(conformal_section$use_support_bin_intervals) &&
    (!is.logical(conformal_section$use_support_bin_intervals) ||
      length(conformal_section$use_support_bin_intervals) != 1 ||
      is.na(conformal_section$use_support_bin_intervals))) {
    stop(
      "Post-selection conformal field 'use_support_bin_intervals' must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.null(conformal_section$support_bin_labels)) {
    resolve_post_selection_support_labels(
      labels = conformal_section$support_bin_labels,
      n_bins = conformal_section$n_bins %||% 3L
    )
  }

  invisible(NULL)
}

#' Validate the policy section
#'
#' @param policy_section Config `policy` section.
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
#' @param policies_section Config `policies` section.
#' @param policy_path Optional policy-registry path.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
validate_policy_list_section <- function(policies_section,
                                         policy_path = NULL) {
  # Confirm that the active policy list is non-empty and entirely drawn from
  # the policy registry or from a valid constructor-resolvable canonical name.
  active_values <- stringr::str_squish(as.character(unlist(policies_section$active %||% character(0), use.names = FALSE)))
  active_values <- active_values[!is.na(active_values) & nzchar(active_values)]
  policies_section$active <- active_values

  if (length(active_values) == 0) {
    stop("Policies must include at least one active policy.", call. = FALSE)
  }

  known_values <- policy_names(policy_path = policy_path)
  unknown_values <- Filter(
    function(policy_name) {
      !policy_name %in% known_values &&
        !is.list(construct_policy_definition_from_name(policy_name, policy_path = policy_path))
    },
    active_values
  )
  if (length(unknown_values) > 0) {
    stop(
      sprintf("Unknown config policy name(s): %s", paste(unknown_values, collapse = ", ")),
      call. = FALSE
    )
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
validate_weight_map <- function(weight_map,
                                scope,
                                registry_path = NULL) {
  # Require named finite nonnegative weights and check the names directly
  # against the relevant trait registry scope.
  if (length(weight_map) == 0) {
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


