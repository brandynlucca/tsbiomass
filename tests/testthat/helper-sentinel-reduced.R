build_reduced_validation_fixture <- function(data,
                                             config = NULL,
                                             anchor_selector = NULL,
                                             extra_species = 0L,
                                             drop_missing_species = TRUE) {
  data <- tsbiomass:::sentinel_resolve_data(data)
  config <- tsbiomass:::sentinel_resolve_config(data = data, config = config)
  extra_species <- suppressWarnings(as.integer(extra_species)[[1]])

  if (!is.finite(extra_species) || extra_species < 0L) {
    stop("'extra_species' must be one non-negative integer.", call. = FALSE)
  }
  if (!is.logical(drop_missing_species) || length(drop_missing_species) != 1L || is.na(drop_missing_species)) {
    stop("'drop_missing_species' must be TRUE or FALSE.", call. = FALSE)
  }

  species_name <- if ("species_name" %in% names(data)) {
    "species_name"
  } else if ("anchor_species" %in% names(data)) {
    "anchor_species"
  } else {
    stop("Reduced validation fixtures require a `species_name` or `anchor_species` column.", call. = FALSE)
  }

  data[[species_name]] <- stringr::str_squish(as.character(data[[species_name]]))
  if (isTRUE(drop_missing_species)) {
    data <- data |>
      dplyr::filter(!is.na(.data[[species_name]]), nzchar(.data[[species_name]]))
  }
  if (nrow(data) == 0L) {
    return(data)
  }

  anchor_selector <- anchor_selector %||%
    ((((config$candidates) %||% list())$anchors %||% list())$selector %||%
      (((config$anchors) %||% list())$selector %||% NULL))

  anchor_rows <- if (is.null(anchor_selector)) {
    data
  } else {
    tryCatch(
      set_reference_anchors(
        data,
        selector = anchor_selector,
        require_selection = FALSE
      ),
      error = function(e) data[0, , drop = FALSE]
    )
  }

  anchor_species <- unique(as.character(anchor_rows[[species_name]] %||% character(0)))
  anchor_species <- anchor_species[!is.na(anchor_species) & nzchar(anchor_species)]
  extra_pool <- data |>
    dplyr::filter(!(.data[[species_name]] %in% anchor_species)) |>
    dplyr::count(.data[[species_name]], name = ".rows", sort = TRUE) |>
    dplyr::arrange(dplyr::desc(.data$.rows), .data[[species_name]])
  extra_species_names <- if (extra_species > 0L && nrow(extra_pool) > 0L) {
    utils::head(as.character(extra_pool[[species_name]]), extra_species)
  } else {
    character(0)
  }

  keep_species <- unique(c(anchor_species, extra_species_names))
  if (length(keep_species) == 0L) {
    return(data[0, , drop = FALSE])
  }

  data |>
    dplyr::filter(.data[[species_name]] %in% keep_species)
}

build_reduced_sentinel <- function(data,
                                   workflow_fn,
                                   config = NULL,
                                   deployment_target = NULL,
                                   split_mode = NULL,
                                   split_col = NULL,
                                   scenario_grid = NULL,
                                   output_dir = NULL,
                                   case_studies = character(0),
                                   options = NULL,
                                   extra_species = 0L,
                                   drop_missing_species = TRUE) {
  reduced_data <- build_reduced_validation_fixture(
    data = data,
    config = config,
    extra_species = extra_species,
    drop_missing_species = drop_missing_species
  )

  build_sentinel(
    data = reduced_data,
    workflow_fn = workflow_fn,
    config = config,
    deployment_target = deployment_target,
    split_mode = split_mode,
    split_col = split_col,
    scenario_grid = scenario_grid,
    output_dir = output_dir,
    case_studies = case_studies,
    options = options
  )
}

run_reduced_sentinel <- function(data,
                                 workflow_fn = NULL,
                                 config = NULL,
                                 deployment_target = NULL,
                                 split_mode = NULL,
                                 split_col = NULL,
                                 scenario_grid = NULL,
                                 output_dir = NULL,
                                 case_studies = character(0),
                                 options = NULL,
                                 extra_species = 0L,
                                 drop_missing_species = TRUE,
                                 refresh = FALSE,
                                 max_folds = NULL,
                                 progress = NULL) {
  sentinel <- if ((inherits(data, "S7_object") && exists("Sentinel", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(data, Sentinel), error = function(e) FALSE)))) {
    data
  } else {
    build_reduced_sentinel(
      data = data,
      workflow_fn = workflow_fn,
      config = config,
      deployment_target = deployment_target,
      split_mode = split_mode,
      split_col = split_col,
      scenario_grid = scenario_grid,
      output_dir = output_dir,
      case_studies = case_studies,
      options = options,
      extra_species = extra_species,
      drop_missing_species = drop_missing_species
    )
  }

  run_sentinel(
    object = sentinel,
    workflow_fn = workflow_fn,
    refresh = refresh,
    max_folds = max_folds,
    progress = progress
  )
}

run_reduced_sentinel_suite <- function(data,
                                       workflow_fn,
                                       config = NULL,
                                       split_specs = list(
                                         anchor_row_holdout = list(split_mode = "anchor_row_holdout"),
                                         study_holdout = list(split_mode = "study_holdout"),
                                         study_cell_holdout = list(split_mode = "study_cell_holdout"),
                                         species_holdout = list(split_mode = "species_holdout")
                                       ),
                                       output_dir = NULL,
                                       extra_species = 0L,
                                       drop_missing_species = TRUE,
                                       case_studies = character(0),
                                       options = NULL,
                                       refresh = FALSE,
                                       max_folds = NULL,
                                       progress = NULL) {
  if (!is.list(split_specs) || length(split_specs) == 0L || is.null(names(split_specs)) || any(!nzchar(names(split_specs)))) {
    stop("'split_specs' must be a non-empty named list.", call. = FALSE)
  }

  output_root <- output_dir
  if (is.null(output_root)) {
    config <- tsbiomass:::sentinel_resolve_config(data = data, config = config)
    output_root <- file.path(
      tsbiomass:::sentinel_resolve_output_dir(config, output_dir = NULL),
      "sentinel_reduced_suite"
    )
  }
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

  out <- vector("list", length(split_specs))
  names(out) <- names(split_specs)
  for (i in seq_along(split_specs)) {
    spec_name <- names(split_specs)[[i]]
    spec <- split_specs[[i]] %||% list()
    if (!is.list(spec)) {
      stop(sprintf("Split specification '%s' must be a list.", spec_name), call. = FALSE)
    }

    spec_stem <- stringr::str_squish(as.character(spec_name %||% "unknown"))
    spec_stem <- gsub("[^[:alnum:]_]+", "_", spec_stem)
    spec_stem <- gsub("^_+|_+$", "", spec_stem)
    if (!nzchar(spec_stem)) {
      spec_stem <- "unknown"
    }

    out[[i]] <- run_reduced_sentinel(
      data = data,
      workflow_fn = workflow_fn,
      config = config,
      deployment_target = spec$deployment_target %||% NULL,
      split_mode = spec$split_mode %||% "anchor_row_holdout",
      split_col = spec$split_col %||% NULL,
      scenario_grid = spec$scenario_grid %||% NULL,
      output_dir = file.path(output_root, spec_stem),
      case_studies = spec$case_studies %||% case_studies,
      options = merge_config_sections(options %||% list(), spec$options %||% list()),
      extra_species = extra_species,
      drop_missing_species = drop_missing_species,
      refresh = refresh,
      max_folds = max_folds,
      progress = progress
    )
  }

  out
}
