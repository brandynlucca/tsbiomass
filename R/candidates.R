#' Candidate-Model Ingest and Preparation S7 Class
#'
#' `Candidates` stores the study records, species metadata, candidate models,
#' selected reference anchors, and downstream similarity/admissibility results
#' used by transferability analysis.
#'
#' Construction is explicit. Callers must provide either:
#' - a complete candidate-ingest config list,
#' - a YAML path containing that config, or
#' - a [Configurer] object containing the required candidate-ingest sections.
#'
#' @section Properties:
#' - `spec`: Normalized candidate-ingest specification.
#' - `study_db`: Study metadata table read from the configured input.
#' - `species_vector`: Character vector of species names queried during
#'   metadata enrichment.
#' - `source_dbs`: Named list of source-specific species tables.
#' - `species_db`: Consolidated species metadata table after source precedence
#'   rules are applied.
#' - `candidate_models`: Final candidate-model table used by similarity,
#'   admissibility, benchmarking, and policy selection.
#' - `reference_anchors`: Candidate-model rows selected as reference anchors.
#' - `similarity_matrix`: Prepared similarity state from
#'   [prepare_similarities()].
#' - `gower_distances`: Distance bundle from [construct_gower_distances()].
#' - `ordination`: Ordination results from [run_ordination()].
#' - `admissibility`: Admissibility-screen results from
#'   [screen_admissibility()].
#' - `similarity_tuning`: Similarity-tuning diagnostics from
#'   [tune_similarities()].
#'
#' @examples
#' cfg <- list(
#'   data = list(
#'     list(id = "study_metadata", path = "input.xlsx"),
#'     list(id = "worms", type = "remote", engine = "r_package"),
#'     list(id = "fishbase", type = "remote", engine = "r_package")
#'   ),
#'   enrich = list(
#'     precedence = c("fishbase", "worms")
#'   ),
#'   prepare = list(),
#'   anchors = list(
#'     selector = list(regional_body = "SWFSC")
#'   )
#' )
#'
#' # This example is not run because it expects real files and, optionally,
#' # live API access for remote sources.
#' \dontrun{
#' candidates <- build_candidates(cfg)
#' candidates
#' }
#'
#' @name Candidates-class
#' @usage NULL
#' @aliases Candidates
NULL

CandidatesDataFrame <- S7::new_S3_class("data.frame")

#' Test whether an object is a `Candidates` instance
#'
#' Resolve the key similarity trait columns stored on `Candidates`
#'
#' @param candidates A [Candidates] object.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
candidates_similarity_key_cols <- function(candidates) {
  if (!is_s7_instance(candidates, "Candidates")) {
    return(character(0))
  }

  # Prefer the tuned similarity weights because they reflect the actual
  # production configuration used downstream. Fall back to the prepared
  # similarity object when no tuning result has been stored.
  if (length(candidates@similarity_tuning) > 0 &&
    is.list((candidates@similarity_tuning)$config_tuned %||% NULL)) {
    tuned_cfg <- (candidates@similarity_tuning)$config_tuned
    return(unique(c(
      names(tuned_cfg$species_weights %||% list()),
      names(tuned_cfg$study_weights %||% list())
    )))
  }

  if (length(candidates@similarity_matrix) > 0) {
    sim_obj <- candidates@similarity_matrix
    return(unique(c(
      as.character(sim_obj$species_traits %||% character(0)),
      as.character(sim_obj$study_traits %||% character(0))
    )))
  }

  spec_now <- candidates@spec %||% list()
  configured <- configured_traits(spec_now, scope = "all")
  configured <- unique(as.character(configured[!is.na(configured) & nzchar(configured)]))
  if (length(configured) > 0) {
    return(configured)
  }

  character(0)
}

#' Validate whether stored admissibility scores match the current anchors
#'
#' @param candidates A [Candidates] object.
#' @param reference_anchors Anchor table to compare against.
#'
#' @return Logical scalar.
#'
#' @keywords internal
#' @noRd
candidate_admissibility_matches_anchors <- function(candidates,
                                                    reference_anchors) {
  if (!is_s7_instance(candidates, "Candidates") ||
    length(candidates@admissibility) == 0) {
    return(FALSE)
  }

  scores_tbl <- tibble::as_tibble((candidates@admissibility)$all_scores %||% tibble::tibble())
  anchors_tbl <- tibble::as_tibble(reference_anchors)
  required_score_cols <- c("anchor_model_id", "anchor_species")
  required_anchor_cols <- c("model_id", "species_name")

  if (!all(required_score_cols %in% names(scores_tbl)) ||
    !all(required_anchor_cols %in% names(anchors_tbl))) {
    return(FALSE)
  }

  score_keys <- scores_tbl |>
    dplyr::distinct(.data$anchor_model_id, .data$anchor_species) |>
    dplyr::arrange(anchor_model_id, .data$anchor_species)
  anchor_keys <- anchors_tbl |>
    dplyr::transmute(
      anchor_model_id = as.character(.data$model_id),
      anchor_species = as.character(.data$species_name)
    ) |>
    dplyr::distinct(.data$anchor_model_id, .data$anchor_species) |>
    dplyr::arrange(.data$anchor_model_id, .data$anchor_species)

  isTRUE(all.equal(score_keys, anchor_keys, check.attributes = FALSE))
}

#' Return the built-in candidate-source registry
#'
#' @return Named list of built-in source adapter definitions.
#'
#' @keywords internal
#' @noRd
candidate_source_registry <- function() {
  list(
    worms = list(
      ids = c("worms"),
      type = "remote",
      engine = "r_package",
      reader = fetch_worms,
      path_arg = NULL,
      path_kind = NULL
    ),
    fishbase = list(
      ids = c("fishbase"),
      type = "remote",
      engine = "r_package",
      reader = fetch_fishbase,
      path_arg = NULL,
      path_kind = NULL
    ),
    pelagic = list(
      ids = c("pelagic", "pelagictraits"),
      type = "directory",
      engine = "directory",
      reader = read_pelagic_db,
      path_arg = "dl_path",
      path_kind = "dir"
    ),
    azores = list(
      ids = c("azores", "azorestraits"),
      type = "single_file",
      engine = "single_file",
      reader = read_azores_db,
      path_arg = "db_path",
      path_kind = "file"
    ),
    continental = list(
      ids = c("continental", "continentaltraits"),
      type = "single_file",
      engine = "single_file",
      reader = read_continental_db,
      path_arg = "db_path",
      path_kind = "file"
    ),
    mstraits = list(
      ids = c("mstraits"),
      type = "local",
      engine = "rdata",
      reader = read_mstraits_db,
      path_arg = "db_path",
      path_kind = "file"
    )
  )
}

#' Resolve one built-in source adapter definition
#'
#' @param source_key Source id or alias.
#' @param source_type Optional external source type.
#' @param engine Optional external source engine.
#'
#' @return One built-in source adapter definition.
#'
#' @keywords internal
#' @noRd
candidate_source_definition <- function(source_key = NULL,
                                        source_type = NULL,
                                        engine = NULL) {
  registry <- candidate_source_registry()

  key_slug <- stringr::str_to_lower(stringr::str_squish(as.character(source_key %||% "")))
  key_slug <- gsub("[^a-z0-9]+", "", key_slug)
  if (nzchar(key_slug)) {
    for (nm in names(registry)) {
      ids <- stringr::str_to_lower(stringr::str_squish(as.character(registry[[nm]]$ids %||% nm)))
      ids <- gsub("[^a-z0-9]+", "", ids)
      if (key_slug %in% ids) {
        out <- registry[[nm]]
        out$name <- nm
        return(out)
      }
    }
  }

  type_slug <- stringr::str_to_lower(stringr::str_squish(as.character(source_type %||% "")))
  type_slug <- gsub("[^a-z0-9]+", "", type_slug)
  engine_slug <- stringr::str_to_lower(stringr::str_squish(as.character(engine %||% "")))
  engine_slug <- gsub("[^a-z0-9]+", "", engine_slug)
  matches <- names(Filter(
    function(def) {
      def_type <- stringr::str_to_lower(stringr::str_squish(as.character(def$type %||% "")))
      def_type <- gsub("[^a-z0-9]+", "", def_type)
      def_engine <- stringr::str_to_lower(stringr::str_squish(as.character(def$engine %||% "")))
      def_engine <- gsub("[^a-z0-9]+", "", def_engine)
      same_type <- !nzchar(type_slug) || identical(def_type, type_slug)
      same_engine <- !nzchar(engine_slug) || identical(def_engine, engine_slug)
      same_type && same_engine
    },
    registry
  ))

  if (length(matches) == 1) {
    out <- registry[[matches[[1]]]]
    out$name <- matches[[1]]
    return(out)
  }

  label_now <- source_key %||% source_type %||% engine %||% "<unknown>"
  if (length(matches) > 1) {
    stop(
      sprintf(
        paste(
          "Candidate data entry '%s' is ambiguous.",
          "Provide an id or alias that maps to one supported built-in adapter: %s"
        ),
        label_now,
        paste(names(registry), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  stop(
    sprintf(
      "Unknown candidate data entry '%s'. Supported built-in adapters are: %s",
      label_now,
      paste(names(registry), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' Validate a boolean refresh-like field
#'
#' @param x Value to validate.
#' @param field_name Field label used in error messages.
#'
#' @return Logical scalar.
#'
#' @keywords internal
#' @noRd
candidate_refresh_value <- function(x,
                                    field_name) {
  if (is.null(x)) {
    return(FALSE)
  }
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(sprintf("'%s' must be TRUE or FALSE.", field_name), call. = FALSE)
  }
  x
}

#' Resolve and validate an optional filesystem path
#'
#' @param path Raw path value.
#' @param field_name Field label used in error messages.
#' @param base_dir Base directory used to resolve relative paths.
#' @param must_dir Whether the resolved path must be a directory.
#' @param required Whether the field must be supplied.
#'
#' @return Normalized absolute path or `NULL`.
#'
#' @keywords internal
#' @noRd
candidate_optional_path <- function(path,
                                    field_name,
                                    base_dir,
                                    must_dir = FALSE,
                                    required = FALSE) {
  if (is.null(path)) {
    if (isTRUE(required)) {
      stop(sprintf("'%s' is required.", field_name), call. = FALSE)
    }
    return(NULL)
  }
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop(sprintf("'%s' must be NULL or a single non-empty path.", field_name), call. = FALSE)
  }

  resolved <- path_absolute(path, base_dir = base_dir)
  if (must_dir && !dir.exists(resolved)) {
    stop(sprintf("'%s' does not exist: %s", field_name, resolved), call. = FALSE)
  }
  if (!must_dir && !file.exists(resolved)) {
    stop(sprintf("'%s' does not exist: %s", field_name, resolved), call. = FALSE)
  }
  resolved
}

#' Reduce a `Configurer` to candidate-ingest sections
#'
#' @param config A [Configurer] object.
#'
#' @return Named list containing only the sections required to build
#'   `Candidates`.
#'
#' @keywords internal
#' @noRd
candidates_config_from_config <- function(config) {
  cfg <- config@data

  candidate_cfg <- cfg$candidates %||% list()
  study_cfg <- candidate_cfg$study %||% cfg$study %||% list()
  if (is.null(study_cfg$path)) {
    study_cfg$path <- cfg$paths$input %||% cfg$paths$input_file
  }

  list(
    study = study_cfg,
    data = candidate_cfg$data %||% cfg$data %||% list(),
    enrich = candidate_cfg$enrich %||% cfg$enrich %||% list(),
    prepare = candidate_cfg$prepare %||% cfg$prepare %||% list(),
    anchors = candidate_cfg$anchors %||% cfg$anchors %||% list(),
    registry_path = candidate_cfg$registry_path %||% cfg$registry_path %||% NULL,
    config_data = cfg
  )
}

#' Normalize candidate-ingest configuration
#'
#' @param config Candidate-ingest list, YAML path, or [Configurer] object.
#' @param base_dir Base directory used to resolve relative paths.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path used only when a full
#'   config must first be validated.
#'
#' @return A normalized candidate-ingest specification list.
#'
#' @keywords internal
#' @noRd
normalize_candidates_config <- function(config,
                                        base_dir = getwd(),
                                        registry_path = NULL,
                                        policy_path = NULL) {
  # Accept either a full config object, a YAML file, or a direct
  # candidate-ingest list and reduce everything to one common schema.
  if (is_s7_instance(config, "Configurer")) {
    config <- candidates_config_from_config(config)
  } else if (is.character(config) &&
    length(config) == 1 &&
    file.exists(config) &&
    grepl("\\.(yaml|yml)$", config, ignore.case = TRUE)) {
    config_path <- path_absolute(config, base_dir = base_dir)
    raw_cfg <- yaml::read_yaml(config_path)
    if (is.list(raw_cfg) &&
      all(c("paths", "execution", "tuning", "policies") %in% names(raw_cfg)) &&
      any(c("similarity", "policy") %in% names(raw_cfg))) {
      cfg_data <- build_configurer(
        raw_cfg,
        base_dir = dirname(config_path),
        registry_path = registry_path,
        policy_path = policy_path
      )
      config <- candidates_config_from_config(cfg_data)
    } else {
      config <- raw_cfg
      base_dir <- dirname(config_path)
      if (is.list(config$candidates)) {
        config <- merge_config_sections(
          config$candidates,
          list(
            config_data = config
          )
        )
      }
    }
  }

  if (!is.list(config)) {
    stop("'config' must be a list, YAML path, or `Configurer` object.", call. = FALSE)
  }
  if (!is.character(base_dir) || length(base_dir) != 1 || !nzchar(base_dir)) {
    stop("'base_dir' must be a single non-empty path.", call. = FALSE)
  }

  # Validate the top-level candidate-ingest sections before normalizing any
  # source-specific fields so schema errors are surfaced first.
  required_sections <- character(0)
  missing_sections <- setdiff(required_sections, names(config))
  if (length(missing_sections) > 0) {
    stop(
      sprintf(
        "Candidate config is missing required section(s): %s",
        paste(missing_sections, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (is.null(config$data)) {
    config$data <- list()
  }
  if (is.null(config$enrich)) {
    config$enrich <- list()
  }
  if (is.null(config$prepare)) {
    config$prepare <- list()
  }
  if (is.null(config$anchors)) {
    config$anchors <- list()
  }

  if (!is.list(config$data)) {
    stop("'data' must be a list.", call. = FALSE)
  }
  if (!is.list(config$enrich)) {
    stop("'enrich' must be a named list.", call. = FALSE)
  }
  if (!is.list(config$prepare)) {
    stop("'prepare' must be a named list.", call. = FALSE)
  }
  if (!is.list(config$anchors)) {
    stop("'anchors' must be a named list.", call. = FALSE)
  }

  # Normalize the study, enrich, and prepare sections into a compact internal
  # specification that downstream ingest code can consume directly.
  spec <- list(
    study = list(
      path = NULL,
      sheet = config$study$sheet %||% NULL
    ),
    sources = list(),
    enrich = list(
      precedence = NULL,
      cache_path = if (is.null(config$enrich$cache_path)) NULL else path_absolute(config$enrich$cache_path, base_dir = base_dir),
      missing_tokens = as.character(config$enrich$missing_tokens %||% c("-9999"))
    ),
    prepare = list(
      cache_path = if (is.null(config$prepare$cache_path)) NULL else path_absolute(config$prepare$cache_path, base_dir = base_dir),
      refresh = candidate_refresh_value(config$prepare$refresh %||% FALSE, "prepare.refresh"),
      missing_tokens = as.character(config$prepare$missing_tokens %||% c("-9999"))
    ),
    anchors = list(
      selector = config$anchors$selector %||% config$anchors$filter %||% NULL,
      model_ids = config$anchors$model_ids %||% NULL,
      model_id_col = as.character(config$anchors$model_id_col %||% "model_id")[[1]],
      require_selection = candidate_refresh_value(config$anchors$require_selection %||% TRUE, "anchors.require_selection")
    ),
    base_dir = normalizePath(base_dir, winslash = "/", mustWork = FALSE),
    config_data = config$config_data %||% NULL,
    registry_path = if (is.null(registry_path) && !is.null(config$registry_path)) {
      path_absolute(config$registry_path, base_dir = base_dir)
    } else if (!is.null(registry_path)) {
      path_absolute(registry_path, base_dir = base_dir)
    } else {
      NULL
    }
  )

  if (!is.null(config$study) && !is.list(config$study)) {
    stop("'study' must be a named list.", call. = FALSE)
  }
  if (!is.null(spec$study$sheet) &&
    (!(is.character(spec$study$sheet) || is.numeric(spec$study$sheet)) || length(spec$study$sheet) != 1)) {
    stop("'study.sheet' must be NULL or a single sheet name/index.", call. = FALSE)
  }
  if (!is.null(spec$anchors$model_ids) && !is.character(spec$anchors$model_ids)) {
    stop("'anchors.model_ids' must be a character vector when supplied.", call. = FALSE)
  }
  if (!is.null(spec$anchors$selector) && !is.list(spec$anchors$selector)) {
    stop("'anchors.selector' must be a named list when supplied.", call. = FALSE)
  }
  if (!is.character(spec$anchors$model_id_col) || length(spec$anchors$model_id_col) != 1 || !nzchar(spec$anchors$model_id_col)) {
    stop("'anchors.model_id_col' must be a single non-empty column name.", call. = FALSE)
  }

  # Support either explicit `study` plus a `data` source list, or a single
  # `data` list that also contains the study-metadata entry.
  study_entry <- config$study %||% list()

  data_entries <- config$data
  if (length(data_entries) > 0 && !is.null(names(data_entries)) && any(nzchar(names(data_entries)))) {
    data_entries <- unname(lapply(names(config$data), function(nm) {
      entry <- config$data[[nm]]
      if (is.list(entry) && is.null(entry$id)) {
        entry$id <- nm
      }
      entry
    }))
  }

  source_lookup <- stats::setNames(character(0), character(0))
  source_order <- character(0)
  for (i in seq_along(data_entries)) {
    src <- data_entries[[i]]
    if (!is.list(src)) {
      stop(sprintf("Data entry %d must be a named list.", i), call. = FALSE)
    }

    entry_id <- src$id %||% NULL
    if (!is.character(entry_id) || length(entry_id) != 1 || !nzchar(entry_id)) {
      stop(sprintf("Data entry %d must declare a single non-empty 'id'.", i), call. = FALSE)
    }
    entry_id <- stringr::str_to_lower(stringr::str_squish(as.character(entry_id)))
    entry_id <- gsub("[^a-z0-9]+", "", entry_id)
    alias_now <- src$alias %||% NULL
    alias_slug <- NULL
    if (!is.null(alias_now)) {
      alias_slug <- stringr::str_to_lower(stringr::str_squish(as.character(alias_now)))
      alias_slug <- gsub("[^a-z0-9]+", "", alias_slug)
    }
    if (!is.null(alias_slug) && !nzchar(alias_slug)) {
      alias_slug <- NULL
    }

    if (identical(entry_id, "studymetadata")) {
      study_entry <- merge_config_sections(study_entry, src)
      next
    }

    definition <- candidate_source_definition(
      source_key = alias_slug %||% entry_id,
      source_type = src$type %||% NULL,
      engine = src$engine %||% NULL
    )

    refresh <- candidate_refresh_value(src$refresh %||% FALSE, sprintf("data.%s.refresh", entry_id))
    cache_path <- if (is.null(src$cache_path)) NULL else path_absolute(src$cache_path, base_dir = base_dir)

    resolved_path <- NULL
    if (!is.null(definition$path_arg)) {
      raw_path <- src$path %||% src[[definition$path_arg]] %||% src$db_path %||% src$dl_path %||% NULL
      resolved_path <- candidate_optional_path(
        raw_path,
        sprintf("data.%s.path", entry_id),
        base_dir = base_dir,
        must_dir = identical(definition$path_kind, "dir"),
        required = TRUE
      )
    }

    reserved_fields <- c(
      "id", "alias", "type", "engine", "refresh", "cache_path",
      "path", "db_path", "dl_path", "params"
    )
    extra_params <- src[setdiff(names(src), reserved_fields)]
    explicit_params <- src$params %||% list()
    if (!is.list(explicit_params)) {
      stop(sprintf("Data entry '%s' field 'params' must be a named list.", entry_id), call. = FALSE)
    }
    params <- merge_config_sections(extra_params, explicit_params)

    source_name <- alias_slug %||% entry_id
    spec$sources[[source_name]] <- list(
      id = entry_id,
      alias = alias_slug,
      type = definition$type,
      engine = definition$engine,
      adapter = definition$name,
      refresh = refresh,
      cache_path = cache_path,
      path = resolved_path,
      params = params
    )
    source_lookup[entry_id] <- source_name
    if (!is.null(alias_slug)) {
      source_lookup[alias_slug] <- source_name
    }
    source_order <- c(source_order, source_name)
  }

  study_path_now <- study_entry$path %||% NULL
  spec$study$path <- candidate_optional_path(
    study_path_now,
    "study.path",
    base_dir = base_dir,
    must_dir = FALSE,
    required = TRUE
  )
  spec$study$sheet <- study_entry$sheet %||% spec$study$sheet

  # Lock the precedence order to the declared source set so enrichment always
  # has a complete and deterministic merge order. Precedence may reference
  # either the normalized `id` or the normalized `alias`.
  declared_sources <- unique(source_order)
  precedence <- config$enrich$precedence %||% declared_sources
  precedence <- stringr::str_to_lower(stringr::str_squish(as.character(precedence %||% "")))
  precedence <- gsub("[^a-z0-9]+", "", precedence)
  precedence <- precedence[!is.na(precedence) & nzchar(precedence)]
  if (length(precedence) > 0) {
    unknown_precedence <- setdiff(precedence, names(source_lookup))
    if (length(unknown_precedence) > 0) {
      stop(
        sprintf(
          "Unknown enrich.precedence token(s): %s",
          paste(unknown_precedence, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    precedence <- unname(source_lookup[precedence])
  }
  if (!setequal(precedence, declared_sources)) {
    stop(
      paste(
        "'enrich.precedence' must contain exactly the declared source ids or aliases.",
        "Set it explicitly or omit it."
      ),
      call. = FALSE
    )
  }
  spec$enrich$precedence <- precedence

  spec
}

#' Extract one stored configuration from `Candidates`
#'
#' @param candidates A [Candidates] object.
#'
#' @return Configuration list or `NULL`.
#'
#' @keywords internal
#' @noRd
candidates_configuration <- function(candidates) {
  if (!is_s7_instance(candidates, "Candidates")) {
    return(NULL)
  }
  cfg <- (candidates@spec)$config_data %||% NULL
  if (is.list(cfg)) {
    return(cfg)
  }
  NULL
}

#' Return the normalized config payload from `Candidates`
#'
#' @param candidates A [Candidates] object.
#'
#' @return Configuration list or `NULL`.
#'
#' @keywords internal
#' @noRd
candidates_config_data <- function(candidates) {
  candidates_configuration(candidates)
}

#' Resolve configured trait names from a candidate specification
#'
#' @param candidate_specification Normalized candidate specification.
#' @param scope Trait scope to resolve.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
configured_traits <- function(candidate_specification,
                              scope = c("all", "species", "study")) {
  scope <- match.arg(scope)
  config_data <- candidate_specification$config_data %||% list()
  if (!is.list(config_data) || length(config_data) == 0) {
    return(character(0))
  }

  similarity_section <- config_data$similarity %||% list()
  policy_section <- config_data$policy %||% list()

  resolve_trait_names <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }
    if (!is.null(names(x)) && any(!is.na(names(x))) && any(nzchar(names(x)))) {
      return(names(x))
    }
    as.character(unlist(x, use.names = FALSE))
  }

  species_traits <- resolve_trait_names(similarity_section$species_traits %||% policy_section$species_traits %||% list())
  study_traits <- resolve_trait_names(similarity_section$study_traits %||% policy_section$study_traits %||% list())

  species_traits <- unique(as.character(species_traits[!is.na(species_traits) & nzchar(species_traits)]))
  study_traits <- unique(as.character(study_traits[!is.na(study_traits) & nzchar(study_traits)]))

  switch(scope,
    species = species_traits,
    study = study_traits,
    all = unique(c(species_traits, study_traits))
  )
}

#' Resolve traits required by configured policy constructor groups
#'
#' @param candidate_specification Normalized candidate specification.
#' @param scope Trait scope to resolve.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
configured_policy_group_traits <- function(candidate_specification,
                                           scope = c("all", "species", "study")) {
  scope <- match.arg(scope)
  config_data <- candidate_specification$config_data %||% list()
  policies_section <- config_data$policies %||% list()
  if (!is.list(policies_section)) {
    return(character(0))
  }

  resolve_trait_names <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }
    if (!is.null(names(x)) && any(!is.na(names(x))) && any(nzchar(names(x)))) {
      return(names(x))
    }
    values <- as.character(unlist(x, use.names = FALSE))
    unique(values[!is.na(values) & nzchar(values)])
  }

  species_traits <- resolve_trait_names(policies_section$species_traits %||% NULL)
  study_traits <- resolve_trait_names(policies_section$study_traits %||% NULL)
  switch(scope,
    species = species_traits,
    study = study_traits,
    all = unique(c(species_traits, study_traits))
  )
}

#' Resolve configured admissibility trait names
#'
#' @param candidate_specification Normalized candidate specification.
#' @param scope Trait scope to resolve.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
configured_admissibility_traits <- function(candidate_specification,
                                            scope = c("all", "species", "study")) {
  scope <- match.arg(scope)
  config_data <- candidate_specification$config_data %||% list()
  if (!is.list(config_data) || length(config_data) == 0) {
    return(character(0))
  }

  admissibility_section <- config_data$admissibility %||% list()

  resolve_trait_names <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }
    if (!is.null(names(x)) && any(!is.na(names(x))) && any(nzchar(names(x)))) {
      return(names(x))
    }
    as.character(unlist(x, use.names = FALSE))
  }

  species_traits <- resolve_trait_names(admissibility_section$species_traits %||% list())
  study_traits <- resolve_trait_names(admissibility_section$study_traits %||% list())

  species_traits <- unique(as.character(species_traits[!is.na(species_traits) & nzchar(species_traits)]))
  study_traits <- unique(as.character(study_traits[!is.na(study_traits) & nzchar(study_traits)]))

  switch(scope,
    species = species_traits,
    study = study_traits,
    all = unique(c(species_traits, study_traits))
  )
}

#' Resolve configured anchor-selector field names
#'
#' @param candidate_specification Normalized candidate specification.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
configured_anchor_selector_fields <- function(candidate_specification) {
  selector <- (candidate_specification$anchors %||% list())$selector %||% NULL
  if (!is.list(selector) || length(selector) == 0 || is.null(names(selector))) {
    return(character(0))
  }
  fields <- names(selector)
  unique(as.character(fields[!is.na(fields) & nzchar(fields)]))
}

#' Resolve configured coherence stubs
#'
#' @param candidate_specification Normalized candidate specification.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
configured_coherence_stubs <- function(candidate_specification) {
  config_data <- candidate_specification$config_data %||% list()
  if (!is.list(config_data) || length(config_data) == 0) {
    return(character(0))
  }

  stubs <- character(0)
  for (section_name in c("similarity", "admissibility")) {
    section <- config_data[[section_name]] %||% list()
    coherence <- section$coherence %||% list()
    if (!is.list(coherence) || length(coherence) == 0) {
      next
    }
    for (stub in intersect(c("length", "depth", "frequency"), names(coherence))) {
      stub_cfg <- coherence[[stub]] %||% list()
      if (is.null(stub_cfg)) {
        next
      }
      stubs <- c(stubs, stub)
    }
  }

  unique(stubs)
}

#' Coalesce the first available candidate column
#'
#' @param data_table Candidate-model table.
#' @param columns Candidate source columns in precedence order.
#'
#' @return Vector with one value per row.
#'
#' @keywords internal
#' @noRd
candidate_coalesce_column <- function(data_table,
                                      columns) {
  cols_now <- intersect(columns, names(data_table))
  if (length(cols_now) == 0) {
    return(NULL)
  }

  out <- data_table[[cols_now[[1]]]]
  if (length(cols_now) == 1) {
    return(out)
  }

  is_missing_like <- function(x) {
    if (is.character(x)) {
      return(is.na(x) | !nzchar(stringr::str_squish(x)))
    }
    is.na(x)
  }

  for (nm in cols_now[-1]) {
    replacement <- data_table[[nm]]
    miss_idx <- is_missing_like(out)
    out[miss_idx] <- replacement[miss_idx]
  }

  out
}

#' Normalize ocean-basin values to the canonical registry set
#'
#' @param x Character vector.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
normalize_candidate_ocean_basin <- function(x) {
  if (length(x) == 0) {
    return(character(0))
  }

  vapply(
    as.character(x),
    function(one) {
      if (is.na(one) || !nzchar(stringr::str_squish(one))) {
        return(NA_character_)
      }
      pieces <- stringr::str_split(one, "[;,|]+")[[1]]
      pieces <- stringr::str_to_lower(stringr::str_squish(pieces))
      pieces <- pieces[nzchar(pieces)]
      if (length(pieces) == 0) {
        return(NA_character_)
      }

      mapped <- vapply(
        pieces,
        function(piece) {
          if (stringr::str_detect(piece, "hatchery")) {
            return(NA_character_)
          }
          if (stringr::str_detect(piece, "mediterranean")) {
            return("Mediterranean Sea")
          }
          if (stringr::str_detect(piece, "arctic")) {
            return("Arctic Ocean")
          }
          if (stringr::str_detect(piece, "southern")) {
            return("Southern Ocean")
          }
          if (stringr::str_detect(piece, "atlantic")) {
            return("Atlantic Ocean")
          }
          if (stringr::str_detect(piece, "pacific")) {
            return("Pacific Ocean")
          }
          if (stringr::str_detect(piece, "indian")) {
            return("Indian Ocean")
          }
          if (stringr::str_detect(piece, "inland")) {
            return("Inland")
          }
          stop(
            sprintf(
              "Unsupported ocean_basin value '%s'. Use only canonical basin/body-of-water values.",
              piece
            ),
            call. = FALSE
          )
        },
        character(1)
      )

      mapped <- unique(mapped[!is.na(mapped) & nzchar(mapped)])
      if (length(mapped) == 0) {
        return(NA_character_)
      }
      paste(mapped, collapse = ";")
    },
    character(1)
  )
}

#' Standardize candidate-model columns to the canonical working schema
#'
#' @param data_table Candidate-model table.
#'
#' @return Tibble.
#'
#' @keywords internal
#' @noRd
standardize_candidate_columns <- function(data_table) {
  if (!is.data.frame(data_table) || nrow(data_table) == 0) {
    return(tibble::as_tibble(data_table))
  }

  out <- tibble::as_tibble(data_table)

  species_name <- candidate_coalesce_column(out, c("species_name", "species", "species_species_name"))
  if (!is.null(species_name)) {
    out$species_name <- as.character(species_name)
  }

  genus <- candidate_coalesce_column(out, c("genus", "tax_genus"))
  if (!is.null(genus)) {
    out$genus <- as.character(genus)
  }

  family <- candidate_coalesce_column(out, c("family", "tax_family"))
  if (!is.null(family)) {
    out$family <- as.character(family)
  }

  ord <- candidate_coalesce_column(out, c("order", "tax_order"))
  if (!is.null(ord)) {
    out$order <- as.character(ord)
  }

  swimbladder <- candidate_coalesce_column(out, c("species_swimbladder_type", "swimbladder_type"))
  if (!is.null(swimbladder)) {
    out$swimbladder_type <- as.character(swimbladder)
  }

  body_shape <- candidate_coalesce_column(out, c("body_shape", "body_shape_norm"))
  if (!is.null(body_shape)) {
    out$body_shape <- as.character(body_shape)
  }

  trophic <- candidate_coalesce_column(out, c("trophic", "troph"))
  if (!is.null(trophic)) {
    out$trophic <- suppressWarnings(as.numeric(trophic))
  }

  citation <- candidate_coalesce_column(out, c("citation", "reference_tsl_short"))
  if (!is.null(citation)) {
    out$citation <- as.character(citation)
  }

  length_metric <- candidate_coalesce_column(out, c("length_metric", "length_metric_clean"))
  if (!is.null(length_metric)) {
    out$length_metric <- as.character(length_metric)
  }

  eq_form <- candidate_coalesce_column(out, c("equation_form", "equation_form_type"))
  if (!is.null(eq_form)) {
    out$equation_form <- as.character(eq_form)
  }

  slope_standard <- candidate_coalesce_column(out, c("slope_standard", "slope_len"))
  if (!is.null(slope_standard)) {
    out$slope_standard <- suppressWarnings(as.numeric(slope_standard))
  }

  intercept_standard <- candidate_coalesce_column(out, c("intercept_standard", "intercept_len"))
  if (!is.null(intercept_standard)) {
    out$intercept_standard <- suppressWarnings(as.numeric(intercept_standard))
  }

  freq <- candidate_coalesce_column(out, c("frequency", "frequency_khz"))
  if (!is.null(freq)) {
    out$frequency <- suppressWarnings(as.numeric(freq))
  }

  pressure <- candidate_coalesce_column(out, c("pressure_corrected", "pressure_corrected_flag"))
  if (!is.null(pressure)) {
    pressure_chr <- stringr::str_to_lower(stringr::str_squish(as.character(pressure)))
    out$pressure_corrected <- dplyr::case_when(
      pressure_chr %in% c("true", "t", "yes", "y", "1") ~ TRUE,
      pressure_chr %in% c("false", "f", "no", "n", "0") ~ FALSE,
      pressure_chr %in% c("yes", "no") ~ pressure_chr == "yes",
      TRUE ~ suppressWarnings(as.logical(pressure))
    )
  }

  basin_from_flags <- NULL
  basin_flag_map <- c(
    basin_arctic = "Arctic Ocean",
    basin_southern = "Southern Ocean",
    basin_atlantic = "Atlantic Ocean",
    basin_pacific = "Pacific Ocean",
    basin_indian = "Indian Ocean",
    basin_mediterranean = "Mediterranean Sea"
  )
  basin_flag_cols <- intersect(names(basin_flag_map), names(out))
  if (length(basin_flag_cols) > 0) {
    basin_from_flags <- vapply(
      seq_len(nrow(out)),
      function(i) {
        hits <- basin_flag_cols[vapply(
          basin_flag_cols,
          function(nm) isTRUE(as.logical(out[[nm]][[i]])),
          logical(1)
        )]
        if (length(hits) == 0) {
          return(NA_character_)
        }
        paste(unname(basin_flag_map[hits]), collapse = ";")
      },
      character(1)
    )
  }

  basin_now <- candidate_coalesce_column(out, c("ocean_basin"))
  if (!is.null(basin_now)) {
    out$ocean_basin <- normalize_candidate_ocean_basin(dplyr::coalesce(
      as.character(basin_now),
      basin_from_flags
    ))
  } else if (!is.null(basin_from_flags)) {
    out$ocean_basin <- normalize_candidate_ocean_basin(basin_from_flags)
  }

  if ("fao_area" %in% names(out)) {
    out$fao_area <- vapply(
      as.character(out$fao_area),
      function(one) {
        if (is.na(one) || !nzchar(stringr::str_squish(one))) {
          return(NA_character_)
        }
        codes <- stringr::str_extract_all(one, "\\d+")[[1]]
        codes <- unique(codes[nzchar(codes)])
        if (length(codes) == 0) {
          return(NA_character_)
        }
        paste(codes, collapse = ";")
      },
      character(1)
    )
  }

  if ("lw_a_g" %in% names(out) && !"lw_a" %in% names(out)) {
    out$lw_a <- suppressWarnings(as.numeric(out$lw_a_g))
  }

  length_pdf_data <- candidate_coalesce_column(out, c("length_pdf_data"))
  if (!is.null(length_pdf_data)) {
    out$length_pdf_data <- as.list(length_pdf_data)
  }

  out
}

#' Resolve the configured working columns for candidate models
#'
#' @param candidate_specification Normalized candidate specification.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
candidate_working_columns <- function(candidate_specification) {
  config_data <- candidate_specification$config_data %||% list()
  if (!is.list(config_data) || length(config_data) == 0) {
    return(character(0))
  }

  coherence_stubs <- configured_coherence_stubs(candidate_specification)
  configured_keep <- unique(c(
    configured_anchor_selector_fields(candidate_specification),
    configured_traits(candidate_specification, scope = "all"),
    configured_policy_group_traits(candidate_specification, scope = "all"),
    configured_admissibility_traits(candidate_specification, scope = "all")
  ))

  keep <- unique(c(
    "model_id",
    "species_name",
    "citation",
    "tags",
    "slope",
    "intercept",
    "equation_form",
    "length_metric",
    "frequency",
    "pressure_corrected",
    "study_reference_id",
    "study_cell_id",
    "model_uid",
    "slope_standard",
    "intercept_standard",
    "lw_a",
    "lw_b",
    "length_pdf_data",
    "length_pdf",
    configured_keep
  ))

  if ("length" %in% coherence_stubs || length(coherence_stubs) == 0) {
    keep <- c(
      keep,
      "study_length_min", "study_length_max", "study_length_midpoint", "study_length_range",
      "species_length_min", "species_length_max", "species_length_midpoint", "species_length_range"
    )
  }

  if ("depth" %in% coherence_stubs || length(coherence_stubs) == 0) {
    keep <- c(
      keep,
      "study_depth_min", "study_depth_max", "study_depth_midpoint", "study_depth_range",
      "species_depth_min", "species_depth_max", "species_depth_midpoint", "species_depth_range"
    )
  }

  if ("frequency" %in% coherence_stubs) {
    keep <- c(keep, "frequency")
  }

  unique(keep[!is.na(keep) & nzchar(keep)])
}

#' Trim one species-level table to configured and operational fields
#'
#' @param data_table Species-level data frame.
#' @param candidate_specification Normalized candidate specification.
#'
#' @return Tibble.
#'
#' @keywords internal
#' @noRd
trim_species_data <- function(data_table,
                              candidate_specification) {
  if (!is.data.frame(data_table) || nrow(data_table) == 0) {
    return(tibble::as_tibble(data_table))
  }

  registry_path <- candidate_specification$registry_path %||% NULL
  registry <- read_trait_registry(registry_path = registry_path)
  species_defs <- registry$species_traits
  registry_traits <- vapply(
    species_defs,
    function(x) x$coded_name %||% NA_character_,
    character(1)
  )
  retained_traits <- vapply(
    species_defs[vapply(species_defs, function(x) isTRUE(x$retain_when_trimming), logical(1))],
    function(x) x$coded_name %||% NA_character_,
    character(1)
  )
  support_names <- setdiff(names(data_table), registry_traits)

  keep_names <- unique(c(
    configured_traits(candidate_specification, scope = "species"),
    configured_policy_group_traits(candidate_specification, scope = "species"),
    retained_traits,
    support_names
  ))
  keep_names <- intersect(keep_names, names(data_table))

  tibble::as_tibble(data_table[, keep_names, drop = FALSE])
}

#' Trim one candidate-model table to configured and operational trait fields
#'
#' @param data_table Candidate-model data frame.
#' @param candidate_specification Normalized candidate specification.
#'
#' @return Tibble.
#'
#' @keywords internal
#' @noRd
trim_candidate_data <- function(data_table,
                                candidate_specification) {
  if (!is.data.frame(data_table) || nrow(data_table) == 0) {
    return(tibble::as_tibble(data_table))
  }

  config_data <- candidate_specification$config_data %||% list()
  out <- standardize_candidate_columns(data_table)
  expanded_drop_names <- grep("__", names(out), fixed = TRUE, value = TRUE)
  if (!is.list(config_data) || length(config_data) == 0) {
    if (length(expanded_drop_names) > 0) {
      out <- dplyr::select(out, -dplyr::all_of(expanded_drop_names))
    }
    return(tibble::as_tibble(out))
  }

  keep_cols <- intersect(candidate_working_columns(candidate_specification), names(out))
  out <- tibble::as_tibble(out[, keep_cols, drop = FALSE])
  expanded_drop_names <- intersect(expanded_drop_names, names(out))
  if (length(expanded_drop_names) > 0) {
    out <- dplyr::select(out, -dplyr::all_of(expanded_drop_names))
  }
  out
}

#' Build an empty species table from the trait registry
#'
#' @param registry_path Optional trait-registry path.
#'
#' @return Zero-row tibble with the registry-aligned species schema.
#'
#' @keywords internal
#' @noRd
candidate_empty_species_db <- function(registry_path = NULL) {
  registry <- read_trait_registry(registry_path = registry_path)
  species_defs <- registry$species_traits
  species_names <- vapply(species_defs, function(x) x$coded_name, character(1))
  species_types <- stats::setNames(
    vapply(species_defs, function(x) x$data_type, character(1)),
    species_names
  )

  out <- setNames(vector("list", length(species_names)), species_names)
  for (nm in species_names) {
    out[[nm]] <- if (species_types[[nm]] == "numeric") {
      numeric(0)
    } else if (species_types[[nm]] == "binary") {
      logical(0)
    } else {
      character(0)
    }
  }

  tibble::as_tibble(out)
}

#' Materialize one configured candidate source
#'
#' @param source_name Source instance name from the config.
#' @param source_spec Normalized source specification.
#' @param species_vector Character vector of species names to query.
#'
#' @return A prepared source tibble.
#'
#' @keywords internal
#' @noRd
candidate_materialize_source <- function(source_name,
                                         source_spec,
                                         species_vector) {
  definition <- candidate_source_definition(source_spec$adapter %||% source_spec$id %||% source_name)

  # Build the reader call from the normalized source spec so the ingest layer
  # depends on configured source adapters rather than on hard-coded source
  # names inside the payload builder.
  args <- list(
    species = species_vector,
    cache_path = source_spec$cache_path,
    refresh = source_spec$refresh
  )

  if (!is.null(definition$path_arg)) {
    args[[definition$path_arg]] <- source_spec$path
  }

  if (length(source_spec$params) > 0) {
    protected_args <- intersect(names(source_spec$params), names(args))
    if (length(protected_args) > 0) {
      stop(
        sprintf(
          "Source '%s' params cannot override reserved argument(s): %s",
          source_name,
          paste(protected_args, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    args <- c(args, source_spec$params)
  }

  do.call(definition$reader, args)
}

#' Materialize the full candidate-ingest payload
#'
#' @param spec Normalized candidate-ingest specification produced by
#'   `normalize_candidates_config()`.
#'
#' @return Named list containing the prepared study, source, species, and
#'   candidate-model tables.
#'
#' @keywords internal
#' @noRd
build_candidates_payload <- function(spec) {
  # Read the raw study table first because it defines the species set that all
  # enrichment sources must be queried against.
  study_db <- read_tsl_table(
    path = spec$study$path,
    sheet = spec$study$sheet
  )
  species_vector <- unique(stringr::str_trim(paste(study_db$genus, study_db$species)))
  species_vector <- species_vector[
    !is.na(species_vector) & nzchar(species_vector) & species_vector != "NA NA"
  ]
  species_vector <- sort(species_vector)
  if (length(species_vector) == 0) {
    stop("No valid species names were detected in the study table.", call. = FALSE)
  }

  source_dbs <- list()

  # Materialize each declared source independently so the final object keeps the
  # full prepared ingest products rather than only the final merged result. The
  # configured source name is just an instance label; the underlying adapter is
  # selected from the source spec itself.
  for (nm in spec$enrich$precedence) {
    src <- spec$sources[[nm]]
    source_dbs[[nm]] <- candidate_materialize_source(
      source_name = nm,
      source_spec = src,
      species_vector = species_vector
    )
    source_dbs[[nm]] <- trim_species_data(
      data_table = source_dbs[[nm]],
      candidate_specification = spec
    )
  }

  # Merge the prepared source tables into one enriched species table using the
  # caller-specified precedence order and optional registry overrides. When no
  # species sources are configured, keep going with an empty registry-shaped
  # species table so study-only pipelines remain valid.
  species_db <- if (length(source_dbs) == 0) {
    candidate_empty_species_db(registry_path = spec$registry_path)
  } else {
    enrich_species_db(
      db_list = source_dbs,
      precedence = spec$enrich$precedence,
      registry_path = spec$registry_path,
      cache_path = spec$enrich$cache_path,
      missing_tokens = spec$enrich$missing_tokens
    )
  }
  species_db <- trim_species_data(
    data_table = species_db,
    candidate_specification = spec
  )

  # Prepare the final candidate-model table that downstream similarity,
  # admissibility, and policy-selection steps operate on.
  candidate_models <- prepare_traits(
    species_db = species_db,
    study_db = study_db,
    cache_path = spec$prepare$cache_path,
    refresh = spec$prepare$refresh,
    registry_path = spec$registry_path,
    missing_tokens = spec$prepare$missing_tokens
  )
  candidate_models <- candidate_reconcile_species_ocean_basin(
    candidate_models = candidate_models,
    species_db = species_db
  )
  candidate_models <- trim_candidate_data(
    data_table = candidate_models,
    candidate_specification = spec
  )

  list(
    study_db = tibble::as_tibble(study_db),
    species_vector = as.character(species_vector),
    source_dbs = source_dbs,
    species_db = tibble::as_tibble(species_db),
    candidate_models = tibble::as_tibble(candidate_models)
  )
}

#' @export
Candidates <- S7::new_class(
  "Candidates",
  properties = list(
    spec = S7::new_property(S7::class_list),
    study_db = S7::new_property(CandidatesDataFrame),
    species_vector = S7::new_property(S7::class_character),
    source_dbs = S7::new_property(S7::class_list),
    species_db = S7::new_property(CandidatesDataFrame),
    candidate_models = S7::new_property(CandidatesDataFrame),
    reference_anchors = S7::new_property(CandidatesDataFrame),
    similarity_matrix = S7::new_property(S7::class_list),
    gower_distances = S7::new_property(S7::class_list),
    ordination = S7::new_property(S7::class_list),
    admissibility = S7::new_property(S7::class_list),
    similarity_tuning = S7::new_property(S7::class_list)
  ),
  validator = function(self) {
    tryCatch(
      {
        if (!all(c("study", "sources", "enrich", "prepare") %in% names(self@spec))) {
          return("`spec` is missing required sections.")
        }
        if (!is.data.frame(self@study_db)) {
          return("`study_db` must be a data frame.")
        }
        if (!is.character(self@species_vector)) {
          return("`species_vector` must be character.")
        }
        if (!is.list(self@source_dbs)) {
          return("`source_dbs` must be a list.")
        }
        if (!is.data.frame(self@species_db)) {
          return("`species_db` must be a data frame.")
        }
        if (!is.data.frame(self@candidate_models)) {
          return("`candidate_models` must be a data frame.")
        }
        if (!is.data.frame(self@reference_anchors)) {
          return("`reference_anchors` must be a data frame.")
        }
        if (!is.list(self@similarity_matrix)) {
          return("`similarity_matrix` must be a list.")
        }
        if (!is.list(self@gower_distances)) {
          return("`gower_distances` must be a list.")
        }
        if (!is.list(self@ordination)) {
          return("`ordination` must be a list.")
        }
        if (!is.list(self@admissibility)) {
          return("`admissibility` must be a list.")
        }
        if (!is.list(self@similarity_tuning)) {
          return("`similarity_tuning` must be a list.")
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(Candidates)

reference_anchor_id_column <- function(data) {
  if ("model_id" %in% names(data)) {
    return("model_id")
  }
  if ("model_id_chr" %in% names(data)) {
    return("model_id_chr")
  }
  stop("Reference anchors require a 'model_id' or 'model_id_chr' column.", call. = FALSE)
}

ensure_reference_pdf_columns <- function(data,
                                         anchor_ids = character(0)) {
  data <- tibble::as_tibble(data)
  if (!"length_pdf_data" %in% names(data)) {
    data$length_pdf_data <- rep(list(NULL), nrow(data))
  }
  if (!"length_pdf" %in% names(data)) {
    data$length_pdf <- NA_character_
  }
  id_col <- reference_anchor_id_column(data)
  row_ids <- as.character(data[[id_col]])
  target <- row_ids %in% as.character(anchor_ids)
  missing_status <- is.na(data$length_pdf) | !nzchar(data$length_pdf)
  data$length_pdf[target & missing_status] <- "uniform"
  data
}

set_reference_pdf_rows <- function(data,
                                   length_pdf) {
  data <- tibble::as_tibble(data)
  if (!is.list(length_pdf) || is.null(names(length_pdf)) ||
    any(!nzchar(names(length_pdf)))) {
    stop("'length_pdf' must be a named list keyed by reference model ID.", call. = FALSE)
  }
  id_col <- reference_anchor_id_column(data)
  row_ids <- as.character(data[[id_col]])
  data <- ensure_reference_pdf_columns(data)
  for (model_id in names(length_pdf)) {
    idx <- which(row_ids == as.character(model_id))
    if (length(idx) == 0) {
      stop(
        sprintf("No selected reference anchor matched model ID '%s'.", model_id),
        call. = FALSE
      )
    }
    pdf_now <- normalize_anchor_pdf_input(length_pdf[[model_id]])
    data$length_pdf_data[idx] <- rep(list(pdf_now), length(idx))
    data$length_pdf[idx] <- "user"
  }
  data
}

preserve_reference_pdf_columns <- function(data,
                                           source_data) {
  data <- tibble::as_tibble(data)
  source_data <- tibble::as_tibble(source_data)
  if (!all(c("length_pdf_data", "length_pdf") %in% names(source_data))) {
    return(data)
  }
  id_col_data <- reference_anchor_id_column(data)
  id_col_source <- reference_anchor_id_column(source_data)
  lookup <- source_data |>
    dplyr::transmute(
      .anchor_model_id = as.character(.data[[id_col_source]]),
      length_pdf_data = .data$length_pdf_data,
      length_pdf = .data$length_pdf
    )
  data <- ensure_reference_pdf_columns(data)
  data$.anchor_model_id <- as.character(data[[id_col_data]])
  data <- data |>
    dplyr::left_join(lookup, by = ".anchor_model_id", suffix = c("", ".src")) |>
    dplyr::mutate(
      length_pdf_data = purrr::pmap(
        list(.data$length_pdf_data, .data$length_pdf_data.src),
        function(current, source) current %||% source %||% NULL
      ),
      length_pdf = dplyr::coalesce(.data$length_pdf, .data$length_pdf.src)
    ) |>
    dplyr::select(
      -".anchor_model_id",
      -"length_pdf_data.src",
      -"length_pdf.src"
    )
  data
}

#' Rebuild a `Candidates` object with updated similarity preparation
#'
#' @param candidates A [Candidates] object.
#' @param similarity_matrix Prepared similarity object to store on the object.
#'
#' @return A `Candidates` object.
#'
#' @keywords internal
#' @noRd
candidates_with_similarity_matrix <- function(candidates,
                                              similarity_matrix) {
  # Rebuild the object explicitly so similarity preparation stays attached to
  # the Candidates object and the prepared expanded candidate-model table
  # becomes the default downstream table for later similarity steps.
  candidate_models_now <- trim_candidate_data(
    data_table = tibble::as_tibble(similarity_matrix$candidate_models),
    candidate_specification = candidates@spec
  )
  candidate_models_now <- preserve_reference_pdf_columns(
    candidate_models_now,
    candidates@candidate_models
  )
  Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = candidate_models_now,
    reference_anchors = candidates@reference_anchors,
    similarity_matrix = similarity_matrix,
    gower_distances = list(),
    ordination = list(),
    admissibility = list(),
    similarity_tuning = candidates@similarity_tuning
  )
}

#' Rebuild a `Candidates` object with updated Gower distances
#'
#' @param candidates A [Candidates] object.
#' @param gower_distances Prepared Gower-distance bundle to store on the
#'   object.
#'
#' @return A `Candidates` object.
#'
#' @keywords internal
#' @noRd
candidates_with_gower_distances <- function(candidates,
                                            gower_distances) {
  # Rebuild the object explicitly so the distance-building step stores its
  # matrix bundle on the Candidates object while preserving every other
  # prepared layer.
  Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = candidates@candidate_models,
    reference_anchors = candidates@reference_anchors,
    similarity_matrix = candidates@similarity_matrix,
    gower_distances = gower_distances,
    ordination = list(),
    admissibility = list(),
    similarity_tuning = candidates@similarity_tuning
  )
}

#' Rebuild a `Candidates` object with updated ordination results
#'
#' @param candidates A [Candidates] object.
#' @param ordination Ordination bundle to store on the object.
#'
#' @return A `Candidates` object.
#'
#' @keywords internal
#' @noRd
candidates_with_ordination <- function(candidates,
                                       ordination) {
  # Rebuild the object explicitly so the ordination step can attach one clean
  # downstream-ready ordination bundle without dropping any earlier prepared
  # state such as anchors, distances, or similarity tuning.
  Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = candidates@candidate_models,
    reference_anchors = candidates@reference_anchors,
    similarity_matrix = candidates@similarity_matrix,
    gower_distances = candidates@gower_distances,
    ordination = ordination,
    admissibility = candidates@admissibility,
    similarity_tuning = candidates@similarity_tuning
  )
}

#' Rebuild a `Candidates` object with updated admissibility results
#'
#' @param candidates A [Candidates] object.
#' @param admissibility Admissibility-screen result to store on the object.
#'
#' @return A `Candidates` object.
#'
#' @keywords internal
#' @noRd
candidates_with_admissibility <- function(candidates,
                                          admissibility) {
  # Rebuild the object explicitly so the admissibility screen stores one
  # consolidated result bundle on the Candidates object without dropping
  # any of the upstream prepared state it depends on.
  Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = candidates@candidate_models,
    reference_anchors = candidates@reference_anchors,
    similarity_matrix = candidates@similarity_matrix,
    gower_distances = candidates@gower_distances,
    ordination = candidates@ordination,
    admissibility = admissibility,
    similarity_tuning = candidates@similarity_tuning
  )
}

#' Rebuild a `Candidates` object with updated similarity tuning
#'
#' @param candidates A [Candidates] object.
#' @param similarity_tuning Similarity-tuning result to store on the object.
#'
#' @return A `Candidates` object.
#'
#' @keywords internal
#' @noRd
candidates_with_similarity_tuning <- function(candidates,
                                              similarity_tuning) {
  # Rebuild the object explicitly so the tuning path stores its result on the
  # Candidates object without dropping any previously computed payloads
  # or reference anchors.
  Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = candidates@candidate_models,
    reference_anchors = candidates@reference_anchors,
    similarity_matrix = candidates@similarity_matrix,
    gower_distances = candidates@gower_distances,
    ordination = candidates@ordination,
    admissibility = candidates@admissibility,
    similarity_tuning = similarity_tuning
  )
}

#' Build a `Candidates` object
#'
#' @param config Candidate-ingest config list, YAML path, or [Configurer]
#'   object.
#' @param base_dir Base directory used to resolve relative paths.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path, used only when `config` is
#'   a [Configurer] YAML/list and must first be validated as such.
#'
#' @return A fully materialized `Candidates` object.
#'
#' @examples
#' cfg <- list(
#'   study = list(path = "input.xlsx"),
#'   sources = list(
#'     list(id = "worms", type = "remote", engine = "r_package"),
#'     list(id = "fishbase", type = "remote", engine = "r_package")
#'   ),
#'   enrich = list(precedence = c("remote_fishbase", "remote_worms")),
#'   prepare = list(),
#'   anchors = list(
#'     selector = list(regional_body = "SWFSC")
#'   )
#' )
#'
#' \dontrun{
#' candidates <- build_candidates(cfg)
#' candidates
#' }
#'
#' @export
build_candidates <- function(config,
                             base_dir = getwd(),
                             registry_path = NULL,
                             policy_path = NULL) {
  if (missing(config) || is.null(config)) {
    stop(
      paste(
        "'config' is required.",
        "Supply a complete candidate-ingest list, YAML path, or `Configurer` object."
      ),
      call. = FALSE
    )
  }

  spec <- normalize_candidates_config(
    config = config,
    base_dir = base_dir,
    registry_path = registry_path,
    policy_path = policy_path
  )
  payload <- build_candidates_payload(spec)

  Candidates(
    spec = spec,
    study_db = payload$study_db,
    species_vector = payload$species_vector,
    source_dbs = payload$source_dbs,
    species_db = payload$species_db,
    candidate_models = payload$candidate_models,
    reference_anchors = ensure_reference_pdf_columns(
      payload$candidate_models[0, , drop = FALSE]
    ),
    similarity_matrix = list(),
    gower_distances = list(),
    ordination = list(),
    admissibility = list(),
    similarity_tuning = list()
  )
}


#' Read a candidate-ingest YAML file into `Candidates`
#'
#' @param path Candidate-ingest YAML path.
#' @param base_dir Base directory used to resolve relative paths. When omitted,
#'   the YAML file directory is used.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path.
#'
#' @return A fully materialized `Candidates` object.
#'
#' @examples
#' \dontrun{
#' candidates <- build_candidates("path/to/candidates.yaml")
#' candidates
#' }
#'
#' @keywords internal
#' @noRd
candidates_from_yaml <- function(path,
                                 base_dir = dirname(path_absolute(path)),
                                 registry_path = NULL,
                                 policy_path = NULL) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single YAML file path.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(sprintf("Candidate config does not exist: %s", path), call. = FALSE)
  }

  build_candidates(
    config = path,
    base_dir = base_dir,
    registry_path = registry_path,
    policy_path = policy_path
  )
}

#' Print a `Candidates`
#'
#' @name print.Candidates
#' @usage NULL
#'
#' @param x A [Candidates] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @noRd
S7::method(print_generic, Candidates) <- function(x, ...) {
  cat("Candidates\n")
  cat("  study_rows: ", nrow(x@study_db), "\n", sep = "")
  cat("  species_count: ", length(x@species_vector), "\n", sep = "")
  cat("  source_tables: ", length(x@source_dbs), "\n", sep = "")
  cat("  species_rows: ", nrow(x@species_db), "\n", sep = "")
  cat("  candidate_models: ", nrow(x@candidate_models), "\n", sep = "")
  cat("  candidate_columns: ", ncol(x@candidate_models), "\n", sep = "")
  cat("  anchors: ", nrow(x@reference_anchors), "\n", sep = "")
  cat("  similarity_ready: ", if (is.data.frame(x@similarity_matrix)) as.character(nrow(x@similarity_matrix)) else if (is.list(x@similarity_matrix)) if (length(x@similarity_matrix) > 0) "yes" else "no" else if (is.null(x@similarity_matrix)) "no" else "yes", "\n", sep = "")
  cat("  distance_ready: ", if (is.data.frame(x@gower_distances)) as.character(nrow(x@gower_distances)) else if (is.list(x@gower_distances)) if (length(x@gower_distances) > 0) "yes" else "no" else if (is.null(x@gower_distances)) "no" else "yes", "\n", sep = "")
  cat("  ordination_ready: ", if (is.data.frame(x@ordination)) as.character(nrow(x@ordination)) else if (is.list(x@ordination)) if (length(x@ordination) > 0) "yes" else "no" else if (is.null(x@ordination)) "no" else "yes", "\n", sep = "")
  cat("  admissibility_ready: ", if (is.data.frame(x@admissibility)) as.character(nrow(x@admissibility)) else if (is.list(x@admissibility)) if (length(x@admissibility) > 0) "yes" else "no" else if (is.null(x@admissibility)) "no" else "yes", "\n", sep = "")
  cat("  tuning_ready: ", if (is.data.frame(x@similarity_tuning)) as.character(nrow(x@similarity_tuning)) else if (is.list(x@similarity_tuning)) if (length(x@similarity_tuning) > 0) "yes" else "no" else if (is.null(x@similarity_tuning)) "no" else "yes", "\n", sep = "")
  invisible(x)
}

#' Show a `Candidates`
#'
#' @name show.Candidates
#' @usage NULL
#'
#' @param object A [Candidates] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
#' @noRd
S7::method(show_generic, Candidates) <- function(object) {
  print(object)
  invisible(object)
}

#' Plot a `Candidates`
#'
#' Uses the package's S7 method on [base::plot()] so Candidates objects
#' can be plotted directly with `plot(candidates, ...)`.
#'
#' @name plot.Candidates
#'
#' @param x A [Candidates] object.
#' @param y Unused.
#' @param type Figure family to draw.
#' @param count_type Count definition used for `type = "area_distribution"`.
#' @param dissimilarity Dissimilarity layer used for `type = "ordination"`.
#' @param view Secondary plot selector used for `type = "ordination"`,
#'   `type = "admissibility"`, `type = "candidate_review"`,
#'   `type = "uncertainty_importance"`, `type = "component_importance"`,
#'   `type = "similarity_tuning"`, and `type = "slope_support"`.
#' @param anchor_model_id Optional anchor model ID for anchor-specific candidate
#'   plots. When omitted, the first stored anchor is used.
#' @param anchor_species Optional anchor species used when `anchor_model_id` is
#'   not supplied.
#' @param include_hulls Logical scalar controlling whether the combined-distance
#'   or study-distance ordination uses cluster hulls when available.
#' @param ... Unused additional arguments.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' candidates <- build_candidates(config_data_s7)
#' plot(candidates)
#' plot(candidates, type = "ordination", dissimilarity = "species", view = "vectors")
#' plot(candidates, type = "admissibility", view = "overlap_profile")
#' plot(candidates, type = "candidate_review", view = "similarity_map")
#' }
.plot_candidates <- function(x,
                             y = NULL,
                             type = c(
                               "area_distribution",
                               "ordination",
                               "admissibility",
                               "candidate_review",
                               "uncertainty_importance",
                               "similarity_tuning",
                               "most_similar",
                               "candidate_biomass_response",
                               "model_weights",
                               "similarity_map",
                               "component_importance",
                               "tuning_variation",
                               "slope_support",
                               "slope_group"
                             ),
                             count_type = c("studies", "models"),
                             dissimilarity = c("combined", "species", "study"),
                             view = NULL,
                             anchor_model_id = NULL,
                             anchor_species = NULL,
                             include_hulls = TRUE,
                             ...) {
  type <- match.arg(type)
  count_type <- match.arg(count_type)
  dissimilarity <- match.arg(dissimilarity)

  # Keep the default `plot(candidates)` call aligned with the existing
  # area-distribution helper that already works directly on the model table.
  if (identical(type, "area_distribution")) {
    return(plot_area_distribution(
      model_data = x@candidate_models,
      count_type = count_type
    ))
  }

  # Route ordination views through the stored candidate ordination bundle, and
  # derive the study-only ordination on demand from the precomputed study
  # distance matrix so the dissimilarity choice stays explicit at the call site.
  if (identical(type, "ordination")) {
    ordination_view <- match.arg(
      view %||% if (identical(dissimilarity, "species")) "overview" else if (isTRUE(include_hulls)) "cluster_hulls" else "clusters",
      c("overview", "clusters", "cluster_hulls", "vectors", "centers")
    )

    if (identical(dissimilarity, "combined")) {
      if (length(x@ordination) == 0) {
        stop(
          "No ordination results are stored on this `Candidates` object. Run `run_ordination()` first.",
          call. = FALSE
        )
      }
      model_obj <- (x@ordination)$model %||% list()
      point_tbl <- tibble::as_tibble(model_obj$points %||% tibble::tibble())
      hull_tbl <- tibble::as_tibble(model_obj$hulls %||% tibble::tibble())

      if (identical(ordination_view, "vectors")) {
        return(plot_ordination_vectors(
          vec_tbl = model_obj$loadings %||% tibble::tibble(),
          points_tbl = point_tbl
        ))
      }
      if (identical(ordination_view, "centers")) {
        return(plot_ordination_centers(
          fac_tbl = model_obj$centroids %||% tibble::tibble(),
          points_tbl = point_tbl
        ))
      }
      if (identical(ordination_view, "cluster_hulls") && nrow(hull_tbl) > 0) {
        return(plot_ordination_cluster_hulls(
          points_tbl = point_tbl,
          hull_tbl = hull_tbl,
          cluster_col = "nmds_cluster_id"
        ))
      }
      return(plot_ordination_clusters(
        points_tbl = point_tbl,
        cluster_col = "nmds_cluster_id"
      ))
    }

    if (identical(dissimilarity, "species")) {
      if (length(x@ordination) == 0) {
        stop(
          "No ordination results are stored on this `Candidates` object. Run `run_ordination()` first.",
          call. = FALSE
        )
      }
      species_obj <- (x@ordination)$species %||% list()
      species_points <- tibble::as_tibble(species_obj$points %||% tibble::tibble())
      if (identical(ordination_view, "overview")) {
        return(plot_species_ordination(
          points_tbl = species_points
        ))
      }
      if (identical(ordination_view, "vectors")) {
        return(plot_ordination_vectors(
          vec_tbl = species_obj$loadings %||% tibble::tibble(),
          points_tbl = species_points
        ))
      }
      if (identical(ordination_view, "centers")) {
        return(plot_ordination_centers(
          fac_tbl = species_obj$centroids %||% tibble::tibble(),
          points_tbl = species_points
        ))
      }
      if (identical(ordination_view, "cluster_hulls")) {
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
        colorbar_name = "Species cluster"
      ))
    }

    if (length(x@gower_distances) == 0) {
      stop(
        "No Gower-distance bundle is stored on this `Candidates` object. Run `construct_gower_distances()` first.",
        call. = FALSE
      )
    }
    study_dist <- (x@gower_distances)$study_dist %||% NULL
    if (is.null(study_dist) || length(study_dist) == 0) {
      stop(
        "No study-distance matrix is stored on this `Candidates` object.",
        call. = FALSE
      )
    }

    study_trait_cols <- intersect(
      as.character((x@similarity_matrix)$study_traits %||% character(0)),
      names(x@candidate_models)
    )
    study_trait_tbl <- if (length(study_trait_cols) > 0) {
      tibble::as_tibble(x@candidate_models) |>
        dplyr::select(dplyr::all_of(study_trait_cols))
    } else {
      NULL
    }
    reference_ids <- if ("model_id" %in% names(x@reference_anchors)) {
      as.character(x@reference_anchors$model_id)
    } else if ("model_id" %in% names(x@reference_anchors)) {
      as.character(x@reference_anchors$model_id)
    } else {
      NULL
    }
    study_ordination <- run_ordination(
      dist_mat = study_dist,
      trait_table = study_trait_tbl,
      include_loadings = identical(ordination_view, "vectors"),
      include_centroids = identical(ordination_view, "centers")
    )
    study_points <- study_ordination$points |>
      join_ordination_points(
        candidate_models = tibble::as_tibble(x@candidate_models),
        reference_ids = reference_ids,
        model_id_col = if ("model_id" %in% names(x@candidate_models)) "model_id" else "model_id_chr",
        join_cols = c("species_name", "citation", "regional_body", "frequency", "fao_area"),
        cluster_args = list(cluster_col = "study_cluster_id")
      )
    if (identical(ordination_view, "vectors")) {
      return(plot_ordination_vectors(
        vec_tbl = study_ordination$loadings %||% tibble::tibble(),
        points_tbl = study_points,
        species_col = if ("citation" %in% names(study_points)) "citation" else "species_name"
      ))
    }
    if (identical(ordination_view, "centers")) {
      return(plot_ordination_centers(
        fac_tbl = study_ordination$centroids %||% tibble::tibble(),
        points_tbl = study_points,
        species_col = if ("citation" %in% names(study_points)) "citation" else "species_name"
      ))
    }
    if (identical(ordination_view, "cluster_hulls")) {
      hull_tbl <- tryCatch(
        build_ordination_hulls(study_points, cluster_col = "study_cluster_id"),
        error = function(e) tibble::tibble()
      )
      return(plot_ordination_cluster_hulls(
        points_tbl = study_points,
        hull_tbl = hull_tbl,
        cluster_col = "study_cluster_id",
        label_col = if ("citation" %in% names(study_points)) "citation" else "species_name"
      ))
    }
    return(plot_ordination_clusters(
      points_tbl = study_points,
      cluster_col = "study_cluster_id",
      species_col = if ("citation" %in% names(study_points)) "citation" else "species_name",
      colorbar_name = "Study cluster"
    ))
  }

  # The admissibility views consume the cross-anchor summary tables already
  # stored on the object after `screen_admissibility()`.
  if (identical(type, "admissibility")) {
    if (length(x@admissibility) == 0) {
      stop(
        "No admissibility results are stored on this `Candidates` object. Run `screen_admissibility()` first.",
        call. = FALSE
      )
    }
    if (!admissibility_bundle_is_current(x@admissibility, x)) {
      stop(
        paste(
          "The stored admissibility bundle on this `Candidates` object predates the current admissibility gate logic.",
          "Run `screen_admissibility()` again to rebuild it before plotting."
        ),
        call. = FALSE
      )
    }
    view <- match.arg(view %||% "gate_composition", c("gate_composition", "overlap_profile"))
    if (identical(view, "gate_composition")) {
      return(plot_gate_composition(
        (x@admissibility)$all_gates %||% tibble::tibble(),
        config = x
      ))
    }
    return(plot_overlap_heatmap((x@admissibility)$all_overlap %||% tibble::tibble()))
  }

  # The anchor-specific candidate review plots all depend on the same stored
  # per-anchor admissibility bundle, so resolve that anchor once and then route
  # to the requested figure family.
  if (identical(type, "candidate_review")) {
    type <- match.arg(
      view %||% "most_similar",
      c(
        "most_similar",
        "top_candidates",
        "candidate_biomass_response",
        "model_weights",
        "similarity_map"
      )
    )
    if (identical(type, "top_candidates")) {
      type <- "most_similar"
    }
  }
  if (type %in% c(
    "most_similar",
    "candidate_biomass_response",
    "model_weights",
    "similarity_map"
  )) {
    anchor_results <- (x@admissibility)$anchors %||% list()
    if (length(anchor_results) == 0) {
      stop(
        "No anchor-level admissibility bundle is stored on this `Candidates` object. Run `screen_admissibility()` first.",
        call. = FALSE
      )
    }

    anchor_result <- NULL
    if (!is.null(anchor_model_id)) {
      anchor_result <- anchor_results[[as.character(anchor_model_id[[1]])]]
    }
    if (is.null(anchor_result) && !is.null(anchor_species)) {
      matching_ids <- names(anchor_results)[vapply(
        anchor_results,
        function(one_result) {
          one_anchor <- tibble::as_tibble(one_result$anchor %||% tibble::tibble())
          nrow(one_anchor) > 0 &&
            "species_name" %in% names(one_anchor) &&
            identical(as.character(one_anchor$species_name[[1]]), as.character(anchor_species[[1]]))
        },
        logical(1)
      )]
      if (length(matching_ids) > 0) {
        anchor_result <- anchor_results[[matching_ids[[1]]]]
      }
    }
    if (is.null(anchor_result)) {
      anchor_result <- anchor_results[[1]]
    }

    anchor_label <- "Reference"
    anchor_tbl <- tibble::as_tibble(anchor_result$anchor %||% tibble::tibble())
    if (nrow(anchor_tbl) > 0 && "species_name" %in% names(anchor_tbl)) {
      anchor_label <- as.character(anchor_tbl$species_name[[1]])
    } else {
      scored_tbl <- tibble::as_tibble(anchor_result$scored %||% tibble::tibble())
      if (nrow(scored_tbl) > 0 && "anchor_species" %in% names(scored_tbl)) {
        anchor_label <- as.character(scored_tbl$anchor_species[[1]])
      }
    }

    if (identical(type, "most_similar")) {
      return(plot_top_models(
        top_tbl = anchor_result$ranked %||% tibble::tibble(),
        anchor_label = anchor_label
      ))
    }
    if (identical(type, "candidate_biomass_response")) {
      return(plot_biomass_candidate_map(
        candidate_tbl = anchor_result$scored %||% tibble::tibble(),
        anchor_label = anchor_label
      ))
    }
    if (identical(type, "similarity_map")) {
      similarity_tbl <- tibble::as_tibble(anchor_result$scored %||% tibble::tibble())
      if ("admissible" %in% names(similarity_tbl)) {
        similarity_tbl <- similarity_tbl |>
          dplyr::filter(.data$admissible)
      }
      return(plot_similarity_map(
        map_tbl = similarity_tbl,
        anchor_label = anchor_label
      ))
    }
    return(plot_model_weights(
      weight_tbl = anchor_result$scored %||% tibble::tibble(),
      anchor_label = anchor_label
    ))
  }

  if (identical(type, "uncertainty_importance")) {
    diag_tbl <- tibble::as_tibble((((x@admissibility %||% list())$uncertainty_diagnostics %||% list())$anchor_ablation %||% tibble::tibble()))
    overall_tbl <- tibble::as_tibble((((x@admissibility %||% list())$uncertainty_diagnostics %||% list())$overall %||% tibble::tibble()))
    type <- match.arg(view %||% "overall", c("overall", "driver_heatmap", "anchor_profile"))
    if (identical(type, "driver_heatmap")) {
      if (nrow(diag_tbl) == 0) {
        stop(
          paste(
            "No anchor-level uncertainty-driver diagnostics are stored on this `Candidates` object.",
            "Build them before requesting `plot(candidates, type = 'uncertainty_importance', view = 'driver_heatmap')`."
          ),
          call. = FALSE
        )
      }
      return(plot_uncertainty_heat(
        dropout_tbl = diag_tbl |>
          dplyr::transmute(
            anchor_species = as.character(anchor_species),
            block = as.character(.data$component),
            importance_score = as.numeric(.data$importance_score),
            component_rank_global = as.integer(.data$component_rank_global)
          )
      ))
    }
    if (identical(type, "anchor_profile")) {
      if (nrow(diag_tbl) == 0) {
        stop(
          paste(
            "No anchor-level uncertainty-driver diagnostics are stored on this `Candidates` object.",
            "Build them before requesting anchor-specific uncertainty profiles."
          ),
          call. = FALSE
        )
      }
      diag_tbl <- if (!is.null(anchor_model_id)) {
        diag_tbl |>
          dplyr::filter(as.character(.data$anchor_model_id) == as.character(.env$anchor_model_id[[1]]))
      } else if (!is.null(anchor_species)) {
        diag_tbl |>
          dplyr::filter(as.character(.data$anchor_species) == as.character(.env$anchor_species[[1]]))
      } else {
        diag_tbl |>
          dplyr::filter(as.character(.data$anchor_model_id) == as.character(diag_tbl$anchor_model_id[[1]]))
      }
      if (nrow(diag_tbl) == 0) {
        stop("The requested anchor was not present in the stored uncertainty-driver diagnostics.", call. = FALSE)
      }
      return(plot_uncertainty_blocks(
        dropout_tbl = diag_tbl |>
          dplyr::transmute(
            block = as.character(.data$component),
            importance_score = as.numeric(.data$importance_score),
            delta_log_spread = as.numeric(.data$delta_log_spread),
            component_rank_global = as.integer(.data$component_rank_global)
          ),
        anchor_label = as.character(diag_tbl$anchor_species[[1]])
      ))
    }
    if (nrow(overall_tbl) == 0) {
      stop(
        paste(
          "No aggregate uncertainty-driver diagnostics are stored on this `Candidates` object.",
          "Build them before requesting `view = 'overall'`."
        ),
        call. = FALSE
      )
    }
    return(plot_uncertainty_blocks(
      dropout_tbl = overall_tbl |>
        dplyr::transmute(
          block = as.character(.data$component),
          importance_score = as.numeric(.data$importance_score),
          delta_log_spread = as.numeric(.data$delta_log_spread),
          component_rank_global = as.integer(.data$component_rank_global)
        ),
      anchor_label = "All anchors"
    ))
  }

  if (identical(type, "similarity_tuning")) {
    type <- match.arg(view %||% "component_importance", c("component_importance", "tuning_variation"))
  }
  if (identical(type, "component_importance")) {
    component_view <- view %||% if (!is.null(anchor_model_id) || !is.null(anchor_species)) "anchor" else "overall"
    if (identical(component_view, "component_importance")) {
      component_view <- "overall"
    }
    component_view <- match.arg(component_view, c("overall", "anchor", "panel"))
    if (identical(component_view, "overall")) {
      impact_tbl <- ((x@similarity_tuning %||% list())$component_impact_summary %||% tibble::tibble())
      if (nrow(tibble::as_tibble(impact_tbl)) == 0) {
        stop(
          "No similarity-tuning component-importance summary is stored on this `Candidates` object. Run `tune_similarities()` first.",
          call. = FALSE
        )
      }
      return(plot_component_importance(impact_tbl))
    }

    local_tbl <- tibble::as_tibble((((x@admissibility %||% list())$uncertainty_diagnostics %||% list())$anchor_ablation %||% tibble::tibble()))
    if (nrow(local_tbl) == 0) {
      stop(
        paste(
          "No anchor-level component-ablation diagnostics are stored on this `Candidates` object.",
          "Build them before requesting anchor-level component-importance plots."
        ),
        call. = FALSE
      )
    }
    if (identical(component_view, "anchor")) {
      local_tbl <- if (!is.null(anchor_model_id)) {
        local_tbl |>
          dplyr::filter(as.character(.data$anchor_model_id) == as.character(.env$anchor_model_id[[1]]))
      } else if (!is.null(anchor_species)) {
        local_tbl |>
          dplyr::filter(as.character(.data$anchor_species) == as.character(.env$anchor_species[[1]]))
      } else {
        local_tbl |>
          dplyr::filter(as.character(.data$anchor_model_id) == as.character(local_tbl$anchor_model_id[[1]]))
      }
      if (nrow(local_tbl) == 0) {
        stop("The requested anchor was not present in the stored component-ablation diagnostics.", call. = FALSE)
      }
    }
    return(plot_component_importance(local_tbl))
  }
  if (identical(type, "tuning_variation")) {
    variation_tbl <- tibble::as_tibble(((x@similarity_tuning %||% list())$component_weights %||% tibble::tibble()))
    if (nrow(variation_tbl) == 0 || !all(c("component", "multiplier") %in% names(variation_tbl))) {
      stop(
        "No similarity-tuning resample multipliers are stored on this `Candidates` object. Run `tune_similarities()` with resampling enabled first.",
        call. = FALSE
      )
    }
    return(plot_tuning_variation(
      plot_tbl = variation_tbl |>
        dplyr::filter(is.finite(.data$multiplier)) |>
        dplyr::group_by(block = .data$component) |>
        dplyr::summarise(
          mean_multiplier = mean(.data$multiplier, na.rm = TRUE),
          q05_multiplier = stats::quantile(.data$multiplier, probs = 0.05, na.rm = TRUE, names = FALSE),
          q95_multiplier = stats::quantile(.data$multiplier, probs = 0.95, na.rm = TRUE, names = FALSE),
          sd_multiplier = stats::sd(.data$multiplier, na.rm = TRUE),
          .groups = "drop"
        )
    ))
  }

  if (type %in% c("slope_support", "slope_group")) {
    slope_summary <- tryCatch(
      summarize_slope_effect(x@candidate_models),
      error = function(e) {
        list(
          study_cell_level = tibble::tibble(),
          deviation_support_by_group = tibble::tibble()
        )
      }
    )
    if (identical(type, "slope_group")) {
      type <- "group"
    }
    if (identical(type, "slope_support")) {
      type <- match.arg(view %||% "support", c("support", "group"))
    }
    if (identical(type, "group")) {
      return(plot_slope_group(slope_summary$study_cell_level %||% tibble::tibble()))
    }
    return(plot_slope_support(slope_summary$deviation_support_by_group %||% tibble::tibble()))
  }

  plot_slope_support(tibble::tibble())
}

#' Register the `Candidates` plot method
#'
#' @name plot.Candidates
#' @usage NULL
#'
#' @keywords internal
#' @noRd
S7::method(plot_generic, Candidates) <- .plot_candidates


#' Filter reference-anchor rows from a candidate table
#'
#' @param candidate_models Data frame or tibble containing candidate models.
#' @param model_ids Character vector of model IDs to retain as reference
#'   anchors.
#' @param model_id_col Name of the model-ID column in `candidate_models`.
#'
#' @return A tibble containing only the selected reference-anchor rows.
#'
#' @keywords internal
#' @noRd
filter_reference_anchor_rows <- function(candidate_models,
                                         model_ids,
                                         model_id_col = "model_id") {
  # Validate the candidate table and the model-ID selection inputs before
  # attempting any filtering.
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.character(model_ids)) {
    stop("'model_ids' must be a character vector.", call. = FALSE)
  }

  if (!is.character(model_id_col) || length(model_id_col) != 1 || !nzchar(model_id_col)) {
    stop("'model_id_col' must be a single column name.", call. = FALSE)
  }

  if (!model_id_col %in% names(candidate_models)) {
    stop(
      sprintf("Column '%s' was not found in 'candidate_models'.", model_id_col),
      call. = FALSE
    )
  }

  # Standardize the requested model IDs so blanks and duplicates do not affect
  # the anchor selection.
  model_ids_ <- stringr::str_squish(model_ids)
  model_ids_ <- unique(model_ids_[!is.na(model_ids_) & nzchar(model_ids_)])

  if (length(model_ids_) == 0) {
    stop("No valid 'model_ids' were supplied.", call. = FALSE)
  }

  candidate_models_ <- tibble::as_tibble(candidate_models)
  candidate_models_[[model_id_col]] <- as.character(candidate_models_[[model_id_col]])

  # Retain only the requested model IDs and fail clearly when the resulting
  # anchor set is empty.
  anchor_models <- candidate_models_ |>
    dplyr::filter(.data[[model_id_col]] %in% model_ids_)

  if (nrow(anchor_models) == 0) {
    stop(
      sprintf(
        "No reference-anchor models matched the supplied IDs in column '%s'.",
        model_id_col
      ),
      call. = FALSE
    )
  }

  anchor_models
}

#' Normalize one anchor-selection rule
#'
#' @param rule Anchor-selection rule.
#' @param field_name Field label used in error messages.
#'
#' @return A normalized rule list.
#'
#' @keywords internal
#' @noRd
normalize_anchor_selector_rule <- function(rule,
                                           field_name) {
  # Support both the compact pipeline-style form `list(field = value)` and a
  # richer structured rule form with explicit matching options.
  if (!is.list(rule) || is.null(names(rule))) {
    return(list(
      mode = "in",
      values = rule,
      ignore_case = FALSE,
      negate = FALSE,
      require_non_missing = TRUE
    ))
  }

  mode <- as.character(rule$mode %||% "in")[[1]]
  mode <- stringr::str_to_lower(stringr::str_squish(mode))
  if (!mode %in% c("in", "regex", "fixed")) {
    stop(
      sprintf(
        "Anchor selector rule '%s' has unsupported mode '%s'.",
        field_name,
        mode
      ),
      call. = FALSE
    )
  }

  values <- rule$values %||% rule$value %||% rule$pattern %||% NULL
  if (mode %in% c("in", "fixed") && is.null(values)) {
    stop(
      sprintf("Anchor selector rule '%s' must supply 'values'.", field_name),
      call. = FALSE
    )
  }
  if (mode == "regex" && is.null(values)) {
    stop(
      sprintf("Anchor selector rule '%s' must supply 'pattern' or 'value'.", field_name),
      call. = FALSE
    )
  }

  list(
    mode = mode,
    values = values,
    ignore_case = isTRUE(rule$ignore_case),
    negate = isTRUE(rule$negate),
    require_non_missing = !isFALSE(rule$require_non_missing)
  )
}

#' Build a logical mask from an anchor selector
#'
#' @param candidate_models Data frame or tibble containing candidate models.
#' @param selector Named list of anchor-selection rules.
#'
#' @return Logical vector with one element per row in `candidate_models`.
#'
#' @keywords internal
#' @noRd
reference_anchor_selector_mask <- function(candidate_models,
                                           selector) {
  # Validate the selector specification before evaluating any row-level rules.
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.list(selector) || length(selector) == 0) {
    stop("'selector' must be a non-empty named list.", call. = FALSE)
  }
  if (is.null(names(selector)) || any(is.na(names(selector))) || any(!nzchar(names(selector)))) {
    stop("'selector' must be a named list.", call. = FALSE)
  }

  candidate_models_ <- tibble::as_tibble(candidate_models)
  keep <- rep(TRUE, nrow(candidate_models_))

  # Apply every configured rule cumulatively so the final anchor set reflects
  # the intersection of the requested column-level constraints.
  for (field_name in names(selector)) {
    if (!field_name %in% names(candidate_models_)) {
      stop(
        sprintf("Anchor selector field '%s' was not found in 'candidate_models'.", field_name),
        call. = FALSE
      )
    }

    rule <- normalize_anchor_selector_rule(selector[[field_name]], field_name)
    values <- rule$values
    column <- candidate_models_[[field_name]]
    match_vec <- rep(FALSE, length(column))

    # Evaluate the current rule according to the declared matching mode.
    if (rule$mode == "in") {
      values_chr <- as.character(values)
      values_chr <- stringr::str_squish(values_chr)
      values_chr <- unique(values_chr[!is.na(values_chr) & nzchar(values_chr)])
      if (length(values_chr) == 0) {
        stop(
          sprintf("Anchor selector field '%s' produced no valid values.", field_name),
          call. = FALSE
        )
      }

      column_chr <- stringr::str_squish(as.character(column))
      if (isTRUE(rule$ignore_case)) {
        match_vec <- !is.na(column_chr) &
          stringr::str_to_lower(column_chr) %in% stringr::str_to_lower(values_chr)
      } else {
        match_vec <- !is.na(column_chr) & column_chr %in% values_chr
      }
    } else if (rule$mode == "fixed") {
      values_chr <- as.character(values)
      values_chr <- values_chr[!is.na(values_chr)]
      if (length(values_chr) == 0) {
        stop(
          sprintf("Anchor selector field '%s' produced no valid fixed patterns.", field_name),
          call. = FALSE
        )
      }

      column_chr <- as.character(column)
      for (pattern in values_chr) {
        match_vec <- match_vec | (
          !is.na(column_chr) &
            stringr::str_detect(
              string = column_chr,
              pattern = stringr::fixed(pattern, ignore_case = rule$ignore_case)
            )
        )
      }
    } else if (rule$mode == "regex") {
      pattern <- as.character(values)[[1]]
      if (is.na(pattern) || !nzchar(pattern)) {
        stop(
          sprintf("Anchor selector field '%s' must supply a non-empty regex pattern.", field_name),
          call. = FALSE
        )
      }

      column_chr <- as.character(column)
      match_vec <- !is.na(column_chr) &
        stringr::str_detect(
          string = column_chr,
          pattern = stringr::regex(pattern, ignore_case = rule$ignore_case)
        )
    }

    # Apply the non-missing and negation modifiers after the base match rule.
    if (isTRUE(rule$require_non_missing)) {
      match_vec <- match_vec & !is.na(column)
    }
    if (isTRUE(rule$negate)) {
      match_vec <- !match_vec
      if (isTRUE(rule$require_non_missing)) {
        match_vec <- match_vec & !is.na(column)
      }
    }

    keep <- keep & match_vec
  }

  keep
}

#' Filter reference-anchor rows from a dynamic selector
#'
#' @param candidate_models Data frame or tibble containing candidate models.
#' @param selector Named list of anchor-selection rules.
#' @param require_selection Whether zero matching rows should raise an error.
#'
#' @return A tibble containing only the selected reference-anchor rows.
#'
#' @keywords internal
#' @noRd
filter_reference_anchor_rows_by_selector <- function(candidate_models,
                                                     selector,
                                                     require_selection = TRUE) {
  # Validate the selector mode before applying the anchor mask.
  if (!is.logical(require_selection) || length(require_selection) != 1 || is.na(require_selection)) {
    stop("'require_selection' must be TRUE or FALSE.", call. = FALSE)
  }

  candidate_models_ <- tibble::as_tibble(candidate_models)
  keep <- reference_anchor_selector_mask(
    candidate_models = candidate_models_,
    selector = selector
  )
  anchor_models <- candidate_models_[keep, , drop = FALSE]

  # Fail clearly when a required selector produces an empty anchor set.
  if (isTRUE(require_selection) && nrow(anchor_models) == 0) {
    stop("No reference-anchor models matched the supplied selector.", call. = FALSE)
  }

  anchor_models
}

#' Set reference anchors on a candidate table or `Candidates` object
#'
#' Filters a candidate-model table down to an explicit set of reference-anchor
#' model IDs. When `object` is a [Candidates] instance, the selected anchor
#' rows are stored on the returned object. When `object` is a data frame, the
#' filtered anchor rows are returned directly.
#'
#' @param object A candidate-model data frame/tibble or a [Candidates] object.
#' @param model_ids Character vector of model IDs to retain as reference
#'   anchors.
#' @param selector Optional named list of dynamic anchor-selection rules. A
#'   compact pipeline-style selector such as `list(regional_body = "SWFSC")`
#'   performs exact membership matching. Structured rules may also be supplied,
#'   for example `list(regional_body = list(mode = "regex", pattern = "SWFSC"))`.
#' @param model_id_col Name of the model-ID column.
#' @param require_selection Whether zero selected anchors should raise an
#'   error.
#'
#' @return If `object` is a data frame, a tibble containing only the selected
#'   reference-anchor rows. If `object` is a [Candidates] object, an updated
#'   [Candidates] object.
#'
#' @examples
#' anchor_tbl <- set_reference_anchors(
#'   tibble::tibble(model_id = c("12", "18", "24"), x = 1:3),
#'   model_ids = c("12", "24")
#' )
#'
#' dynamic_anchor_tbl <- set_reference_anchors(
#'   tibble::tibble(
#'     model_id = c("12", "18", "24"),
#'     regional_body = c("SWFSC", "AFSC", "SWFSC")
#'   ),
#'   selector = list(regional_body = "SWFSC")
#' )
#'
#' \dontrun{
#' set_reference_anchors(
#'   candidate_models,
#'   model_ids = c("12", "18", "24")
#' )
#'
#' candidates <- build_candidates(cfg)
#' candidates <- set_reference_anchors(
#'   candidates,
#'   selector = list(regional_body = "SWFSC")
#' )
#' fetch_reference_anchors(candidates)
#'
#' configured_candidates <- build_candidates(list(
#'   study = list(path = "input.xlsx"),
#'   anchors = list(
#'     selector = list(regional_body = "SWFSC")
#'   )
#' ))
#' configured_candidates <- set_reference_anchors(configured_candidates)
#' }
#'
#' @export
set_reference_anchors <- function(object = NULL,
                                  model_ids = NULL,
                                  selector = NULL,
                                  model_id_col = "model_id",
                                  require_selection = TRUE) {
  # Normalize the selector inputs and preserve the direct table/object dual use.
  object_ <- object
  selector_ <- selector
  model_ids_ <- model_ids
  model_id_col_ <- model_id_col
  require_selection_ <- require_selection
  if (is_s7_instance(object_, "Candidates")) {
    # When no selector is passed explicitly, fall back to the anchor-selection
    # spec stored on the `Candidates` object itself.
    if (is.null(model_ids_) && is.null(selector_)) {
      selector_ <- ((object_@spec)$anchors)$selector %||% NULL
      model_ids_ <- ((object_@spec)$anchors)$model_ids %||% NULL
      model_id_col_ <- ((object_@spec)$anchors)$model_id_col %||% model_id_col_
      require_selection_ <- ((object_@spec)$anchors)$require_selection %||% require_selection_
    }
  }

  # Enforce a single anchor selection mode per call.
  if (is.null(model_ids_) && is.null(selector_)) {
    stop(
      "Supply either 'model_ids' or 'selector', or store an anchor spec on the `Candidates` object.",
      call. = FALSE
    )
  }
  if (!is.null(model_ids_) && !is.null(selector_)) {
    stop(
      "Supply either 'model_ids' or 'selector', not both.",
      call. = FALSE
    )
  }

  # Rebuild the `Candidates` object when the caller passes a package object.
  if (is_s7_instance(object_, "Candidates")) {
    anchor_models <- if (!is.null(selector_)) {
      filter_reference_anchor_rows_by_selector(
        candidate_models = object_@candidate_models,
        selector = selector_,
        require_selection = require_selection_
      )
    } else {
      filter_reference_anchor_rows(
        candidate_models = object_@candidate_models,
        model_ids = model_ids_,
        model_id_col = model_id_col_
      )
    }
    anchor_ids <- as.character(anchor_models[[reference_anchor_id_column(anchor_models)]])
    anchor_models <- ensure_reference_pdf_columns(anchor_models, anchor_ids = anchor_ids)
    candidate_models_now <- ensure_reference_pdf_columns(
      object_@candidate_models,
      anchor_ids = anchor_ids
    )
    preserved_admissibility <- if (candidate_admissibility_matches_anchors(object_, anchor_models)) {
      object_@admissibility
    } else {
      list()
    }

    return(Candidates(
      spec = object_@spec,
      study_db = object_@study_db,
      species_vector = object_@species_vector,
      source_dbs = object_@source_dbs,
      species_db = object_@species_db,
      candidate_models = candidate_models_now,
      reference_anchors = tibble::as_tibble(anchor_models),
      similarity_matrix = object_@similarity_matrix,
      gower_distances = object_@gower_distances,
      ordination = list(),
      admissibility = preserved_admissibility,
      similarity_tuning = object_@similarity_tuning
    ))
  }

  # Support direct data-frame filtering for ad hoc workflows.
  if (!is.data.frame(object_)) {
    stop(
      "'object' must be a candidate-model data frame/tibble or a `Candidates` object.",
      call. = FALSE
    )
  }

  if (!is.null(selector_)) {
    out <- filter_reference_anchor_rows_by_selector(
      candidate_models = object_,
      selector = selector_,
      require_selection = require_selection_
    )
    anchor_ids <- as.character(out[[reference_anchor_id_column(out)]])
    return(ensure_reference_pdf_columns(out, anchor_ids = anchor_ids))
  }

  out <- filter_reference_anchor_rows(
    candidate_models = object_,
    model_ids = model_ids_,
    model_id_col = model_id_col_
  )
  anchor_ids <- as.character(out[[reference_anchor_id_column(out)]])
  ensure_reference_pdf_columns(out, anchor_ids = anchor_ids)
}

#' Fetch the selected reference anchors
#'
#' @param object A [Candidates] object.
#'
#' @return Tibble of selected anchor rows with a `length_pdf` status column.
#'
#' @examples
#' \dontrun{
#' anchors <- fetch_reference_anchors(candidates)
#' anchors[, c("model_id", "species_name", "length_pdf")]
#' }
#'
#' @export
fetch_reference_anchors <- function(object) {
  if (!is_s7_instance(object, "Candidates")) {
    stop("'object' must be a `Candidates` object.", call. = FALSE)
  }
  anchors <- ensure_reference_pdf_columns(object@reference_anchors)
  tibble::as_tibble(anchors)
}

#' Set user-supplied reference length PDFs
#'
#' @param object A [Candidates] object.
#' @param length_pdf Named list keyed by selected reference `model_id`.
#'
#' @return Updated [Candidates] object.
#'
#' @examples
#' \dontrun{
#' candidates <- set_reference_length_pdf(
#'   candidates,
#'   length_pdf = list("12" = c(10, 11, 12, 12, 13))
#' )
#' }
#'
#' @export
set_reference_length_pdf <- function(object,
                                     length_pdf) {
  if (!is_s7_instance(object, "Candidates")) {
    stop("'object' must be a `Candidates` object.", call. = FALSE)
  }
  anchors_now <- set_reference_pdf_rows(object@reference_anchors, length_pdf)
  model_ids <- as.character(anchors_now[[reference_anchor_id_column(anchors_now)]])
  candidate_models_now <- ensure_reference_pdf_columns(object@candidate_models)
  candidate_ids <- as.character(candidate_models_now[[reference_anchor_id_column(candidate_models_now)]])
  for (model_id in names(length_pdf)) {
    idx <- which(candidate_ids == as.character(model_id))
    if (length(idx) > 0) {
      pdf_now <- normalize_anchor_pdf_input(length_pdf[[model_id]])
      candidate_models_now$length_pdf_data[idx] <- rep(list(pdf_now), length(idx))
      candidate_models_now$length_pdf[idx] <- ifelse(
        candidate_ids[idx] %in% model_ids,
        "user",
        candidate_models_now$length_pdf[idx]
      )
    }
  }
  Candidates(
    spec = object@spec,
    study_db = object@study_db,
    species_vector = object@species_vector,
    source_dbs = object@source_dbs,
    species_db = object@species_db,
    candidate_models = candidate_models_now,
    reference_anchors = anchors_now,
    similarity_matrix = object@similarity_matrix,
    gower_distances = object@gower_distances,
    ordination = object@ordination,
    admissibility = object@admissibility,
    similarity_tuning = object@similarity_tuning
  )
}

candidate_reconcile_species_ocean_basin <- function(candidate_models,
                                                    species_db) {
  if (!all(c("genus", "species", "ocean_basin") %in% names(candidate_models)) ||
    !all(c("genus", "species", "ocean_basin") %in% names(species_db))) {
    return(candidate_models)
  }

  species_ocean <- tibble::as_tibble(species_db) |>
    dplyr::transmute(
      genus = .data$genus,
      species = .data$species,
      ocean_basin_species_refresh = dplyr::na_if(
        stringr::str_squish(as.character(.data$ocean_basin)),
        ""
      )
    ) |>
    dplyr::filter(!is.na(.data$ocean_basin_species_refresh)) |>
    dplyr::distinct(.data$genus, .data$species, .keep_all = TRUE)

  if (nrow(species_ocean) == 0) {
    return(candidate_models)
  }

  out <- tibble::as_tibble(candidate_models) |>
    dplyr::left_join(species_ocean, by = c("genus", "species")) |>
    dplyr::mutate(
      ocean_basin = dplyr::if_else(
        is.na(.data$ocean_basin) |
          !nzchar(stringr::str_squish(as.character(.data$ocean_basin))),
        .data$ocean_basin_species_refresh,
        .data$ocean_basin
      )
    ) |>
    dplyr::select(-"ocean_basin_species_refresh")

  candidate_models[] <- out
  candidate_models
}
