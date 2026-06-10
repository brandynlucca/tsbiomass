#' Candidate-Model Ingest and Preparation S7 Class
#'
#' `Candidates` is the main staged data object for downstream transferability
#' analysis. It reads the study table, ingests any configured species metadata
#' sources, enriches the species table, and prepares the final row-level
#' candidate-model table used by later workflow steps.
#'
#' Construction is explicit. Callers must provide either:
#' - a complete candidate-ingest config list,
#' - a YAML path containing that config, or
#' - a [Configurer] object whose `@data` contains the required candidate
#'   ingest sections.
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
#' candidates <- as_candidates(cfg)
#' candidates@candidate_models
#' }
#'
#' @name Candidates-class
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
candidates_similarity_key_cols <- function(candidates) {
  if (!(inherits(candidates, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidates, Candidates), error = function(e) FALSE)))) {
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
candidate_admissibility_matches_anchors <- function(candidates,
                                                    reference_anchors) {
  if (!(inherits(candidates, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidates, Candidates), error = function(e) FALSE))) || length(candidates@admissibility) == 0) {
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
    dplyr::distinct(anchor_model_id, anchor_species) |>
    dplyr::arrange(anchor_model_id, anchor_species)
  anchor_keys <- anchors_tbl |>
    dplyr::transmute(
      anchor_model_id = as.character(model_id),
      anchor_species = as.character(species_name)
    ) |>
    dplyr::distinct(anchor_model_id, anchor_species) |>
    dplyr::arrange(anchor_model_id, anchor_species)

  isTRUE(all.equal(score_keys, anchor_keys, check.attributes = FALSE))
}

#' Return the built-in candidate-source registry
#'
#' @return Named list of built-in source adapter definitions.
#'
#' @keywords internal
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
workflow_to_candidates_config <- function(config) {
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
    workflow_config = cfg
  )
}

#' Normalize candidate-ingest configuration
#'
#' @param config Candidate-ingest list, YAML path, or [Configurer] object.
#' @param base_dir Base directory used to resolve relative paths.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path used only when a full
#'   workflow config must first be validated.
#'
#' @return A normalized candidate-ingest specification list.
#'
#' @keywords internal
normalize_candidates_config <- function(config,
                                        base_dir = getwd(),
                                        registry_path = NULL,
                                        policy_path = NULL) {
  # Accept either a full workflow config object, a YAML file, or a direct
  # candidate-ingest list and reduce everything to one common schema.
  if ((inherits(config, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
    config <- workflow_to_candidates_config(config)
  } else if (is.character(config) &&
    length(config) == 1 &&
    file.exists(config) &&
    grepl("\\.(yaml|yml)$", config, ignore.case = TRUE)) {
    config_path <- path_absolute(config, base_dir = base_dir)
    raw_cfg <- yaml::read_yaml(config_path)
    if (is.list(raw_cfg) &&
      all(c("paths", "workflow", "tuning", "policies") %in% names(raw_cfg)) &&
      any(c("similarity", "policy") %in% names(raw_cfg))) {
      workflow_cfg <- as_configurer(
        raw_cfg,
        base_dir = dirname(config_path),
        registry_path = registry_path,
        policy_path = policy_path
      )
      config <- workflow_to_candidates_config(workflow_cfg)
    } else {
      config <- raw_cfg
      base_dir <- dirname(config_path)
      if (is.list(config$candidates)) {
        config <- merge_cfg(
          config$candidates,
          list(
            workflow_config = config
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
    workflow_config = config$workflow_config %||% NULL,
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
      study_entry <- merge_cfg(study_entry, src)
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
    params <- merge_cfg(extra_params, explicit_params)

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
candidates_configuration <- function(candidates) {
  if (!(inherits(candidates, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidates, Candidates), error = function(e) FALSE)))) {
    return(NULL)
  }
  cfg <- (candidates@spec)$workflow_config %||% NULL
  if (is.list(cfg)) {
    return(cfg)
  }
  NULL
}

candidates_workflow_config <- function(candidates) {
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
configured_traits <- function(candidate_specification,
                              scope = c("all", "species", "study")) {
  scope <- match.arg(scope)
  workflow_config <- candidate_specification$workflow_config %||% list()
  if (!is.list(workflow_config) || length(workflow_config) == 0) {
    return(character(0))
  }

  similarity_section <- workflow_config$similarity %||% list()
  policy_section <- workflow_config$policy %||% list()

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

#' Trim one species-level table to configured and operational fields
#'
#' @param data_table Species-level data frame.
#' @param candidate_specification Normalized candidate specification.
#'
#' @return Tibble.
#'
#' @keywords internal
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
trim_candidate_data <- function(data_table,
                                candidate_specification) {
  if (!is.data.frame(data_table) || nrow(data_table) == 0) {
    return(tibble::as_tibble(data_table))
  }

  registry_path <- candidate_specification$registry_path %||% NULL
  registry <- read_trait_registry(registry_path = registry_path)
  species_defs <- registry$species_traits
  study_defs <- registry$study_traits
  trait_pool <- unique(c(
    vapply(species_defs, function(x) x$coded_name %||% NA_character_, character(1)),
    vapply(study_defs, function(x) x$coded_name %||% NA_character_, character(1))
  ))
  keep_traits <- unique(c(
    configured_traits(candidate_specification, scope = "all"),
    vapply(
      species_defs[vapply(species_defs, function(x) isTRUE(x$retain_when_trimming), logical(1))],
      function(x) x$coded_name %||% NA_character_,
      character(1)
    ),
    vapply(
      study_defs[vapply(study_defs, function(x) isTRUE(x$retain_when_trimming), logical(1))],
      function(x) x$coded_name %||% NA_character_,
      character(1)
    )
  ))

  drop_names <- setdiff(intersect(names(data_table), trait_pool), keep_traits)
  expanded_drop_names <- grep("__", names(data_table), fixed = TRUE, value = TRUE)
  tibble::as_tibble(dplyr::select(data_table, -dplyr::all_of(unique(c(drop_names, expanded_drop_names)))))
}

#' Build an empty species table from the trait registry
#'
#' @param registry_path Optional trait-registry path.
#'
#' @return Zero-row tibble with the registry-aligned species schema.
#'
#' @keywords internal
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
#' @return A staged source tibble.
#'
#' @keywords internal
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
#'   [normalize_candidates_config()].
#'
#' @return Named list containing the staged study, source, species, and
#'   candidate-model tables.
#'
#' @keywords internal
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
  # full staged ingest products rather than only the final merged result. The
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

  # Merge the staged source tables into one enriched species table using the
  # caller-specified precedence order and optional registry overrides. When no
  # species sources are configured, keep going with an empty registry-shaped
  # species table so study-only workflows remain valid.
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
#' @rdname Candidates-class
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

#' Rebuild a `Candidates` object with updated similarity preparation
#'
#' @param candidates A [Candidates] object.
#' @param similarity_matrix Prepared similarity object to store on the object.
#'
#' @return A `Candidates` object.
#'
#' @keywords internal
candidates_with_similarity_matrix <- function(candidates,
                                              similarity_matrix) {
  # Rebuild the object explicitly so similarity preparation stays attached to
  # the staged candidate object and the prepared expanded candidate-model table
  # becomes the default downstream table for later similarity steps.
  candidate_models_now <- trim_candidate_data(
    data_table = tibble::as_tibble(similarity_matrix$candidate_models),
    candidate_specification = candidates@spec
  )
  stored_similarity <- similarity_matrix

  # Avoid persisting second full copies of the row-level model table and the
  # expanded intermediate matrices on the `Candidates` object. They can be
  # rebuilt from the trimmed `@candidate_models` plus the retained trait
  # specifications when a later step truly needs them.
  stored_similarity$candidate_models <- NULL
  stored_similarity$species_data <- NULL
  stored_similarity$study_data <- NULL

  Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = candidate_models_now,
    reference_anchors = candidates@reference_anchors,
    similarity_matrix = stored_similarity,
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
candidates_with_gower_distances <- function(candidates,
                                            gower_distances) {
  # Rebuild the object explicitly so the distance-building step stores its
  # matrix bundle on the staged candidate object while preserving every other
  # prepared layer.
  stored_distances <- gower_distances

  # Persist symmetric distance payloads in compact `dist` form on the object.
  # The anchor-screening code still keeps the asymmetric access paths it needs
  # (`study_dist` and `species_dist_model`) as matrices.
  if (is.matrix(stored_distances$combined_dist %||% NULL)) {
    stored_distances$combined_dist <- stats::as.dist(stored_distances$combined_dist)
  }
  if (is.matrix(stored_distances$species_dist %||% NULL)) {
    stored_distances$species_dist <- stats::as.dist(stored_distances$species_dist)
  }

  Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = candidates@candidate_models,
    reference_anchors = candidates@reference_anchors,
    similarity_matrix = candidates@similarity_matrix,
    gower_distances = stored_distances,
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
candidates_with_admissibility <- function(candidates,
                                          admissibility) {
  # Rebuild the object explicitly so the admissibility screen stores one
  # consolidated result bundle on the staged candidate object without dropping
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
candidates_with_similarity_tuning <- function(candidates,
                                              similarity_tuning) {
  # Rebuild the object explicitly so the tuning path stores its result on the
  # staged candidate object without dropping any previously computed payloads
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
#' candidates@species_db
#' }
#'
#' @export
build_candidates <- function(config,
                             base_dir = getwd(),
                             registry_path = NULL,
                             policy_path = NULL) {
  if ((inherits(config, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Candidates), error = function(e) FALSE)))) {
    return(config)
  }

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
    reference_anchors = payload$candidate_models[0, , drop = FALSE],
    similarity_matrix = list(),
    gower_distances = list(),
    ordination = list(),
    admissibility = list(),
    similarity_tuning = list()
  )
}

#' Coerce candidate-ingest input to `Candidates`
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
#' @export
as_candidates <- function(config,
                          base_dir = getwd(),
                          registry_path = NULL,
                          policy_path = NULL) {
  build_candidates(
    config = config,
    base_dir = base_dir,
    registry_path = registry_path,
    policy_path = policy_path
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
#' candidates <- candidates_from_yaml("path/to/candidates.yaml")
#' candidates@candidate_models
#' }
#'
#' @export
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

  as_candidates(
    config = path,
    base_dir = base_dir,
    registry_path = registry_path,
    policy_path = policy_path
  )
}

#' Print a `Candidates`
#'
#' @name print.Candidates
#'
#' @param x A [Candidates] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
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
#'
#' @param object A [Candidates] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
S7::method(show_generic, Candidates) <- function(object) {
  print(object)
  invisible(object)
}

#' Plot a `Candidates`
#'
#' Uses the package's S7 method on [base::plot()] so staged candidate objects
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
#' candidates <- as_candidates(workflow_config_s7)
#' plot(candidates)
#' plot(candidates, type = "ordination", dissimilarity = "species", view = "vectors")
#' plot(candidates, type = "admissibility", view = "overlap_profile")
#' plot(candidates, type = "candidate_review", view = "similarity_map")
#' }
S7::method(plot_generic, Candidates) <- function(x,
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
          cluster_col = "nmds_cluster_id",
          title = "Combined-Distance NMDS Clusters"
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
          cluster_col = "species_cluster_id",
          title = "Species-Distance NMDS Clusters"
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
        "No Gower-distance bundle is stored on this `Candidates` object. Run `build_gower_distances()` first.",
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
    reference_ids <- if ("model_id_chr" %in% names(x@reference_anchors)) {
      as.character(x@reference_anchors$model_id_chr)
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
        model_id_col = if ("model_id_chr" %in% names(x@candidate_models)) "model_id_chr" else "model_id",
        join_cols = c("species_name", "reference_tsl_short", "regional_body", "frequency", "fao_area"),
        cluster_args = list(cluster_col = "study_cluster_id")
      )
    if (identical(ordination_view, "vectors")) {
      return(plot_ordination_vectors(
        vec_tbl = study_ordination$loadings %||% tibble::tibble(),
        points_tbl = study_points,
        species_col = if ("reference_tsl_short" %in% names(study_points)) "reference_tsl_short" else "species_name"
      ))
    }
    if (identical(ordination_view, "centers")) {
      return(plot_ordination_centers(
        fac_tbl = study_ordination$centroids %||% tibble::tibble(),
        points_tbl = study_points,
        species_col = if ("reference_tsl_short" %in% names(study_points)) "reference_tsl_short" else "species_name"
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
        label_col = if ("reference_tsl_short" %in% names(study_points)) "reference_tsl_short" else "species_name",
        title = "Study-Distance NMDS Clusters"
      ))
    }
    return(plot_ordination_clusters(
      points_tbl = study_points,
      cluster_col = "study_cluster_id",
      species_col = if ("reference_tsl_short" %in% names(study_points)) "reference_tsl_short" else "species_name",
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
    view <- match.arg(view %||% "gate_composition", c("gate_composition", "overlap_profile"))
    if (identical(view, "gate_composition")) {
      return(plot_gate_composition((x@admissibility)$all_gates %||% tibble::tibble()))
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
          dplyr::filter(admissible)
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
            block = as.character(component),
            importance_score = as.numeric(importance_score),
            component_rank_global = as.integer(component_rank_global)
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
            block = as.character(component),
            importance_score = as.numeric(importance_score),
            delta_log_spread = as.numeric(delta_log_spread),
            component_rank_global = as.integer(component_rank_global)
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
          block = as.character(component),
          importance_score = as.numeric(importance_score),
          delta_log_spread = as.numeric(delta_log_spread),
          component_rank_global = as.integer(component_rank_global)
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
          "No similarity-tuning component-importance summary is stored on this `Candidates` object. Run `tune_similarity_matrix()` first.",
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
        "No similarity-tuning resample multipliers are stored on this `Candidates` object. Run `tune_similarity_matrix()` with resampling enabled first.",
        call. = FALSE
      )
    }
    return(plot_tuning_variation(
      plot_tbl = variation_tbl |>
        dplyr::filter(is.finite(multiplier)) |>
        dplyr::group_by(block = component) |>
        dplyr::summarise(
          mean_multiplier = mean(multiplier, na.rm = TRUE),
          q05_multiplier = stats::quantile(multiplier, probs = 0.05, na.rm = TRUE, names = FALSE),
          q95_multiplier = stats::quantile(multiplier, probs = 0.95, na.rm = TRUE, names = FALSE),
          sd_multiplier = stats::sd(multiplier, na.rm = TRUE),
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


