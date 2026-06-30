#' Default constructor-driven recommendation policies
#'
#' @param policy_path Optional path to the JSON policy registry.
#'
#' @return A tibble defining the canonical constructor-generated policy set.
#' @export
recommendation_policy_set <- function(policy_path = NULL) {
  registry <- read_policy_registry(policy_path = policy_path)
  policies <- registry$policies
  if (is.null(policies) || !is.list(policies) || length(policies) == 0) {
    stop(
      "The policy registry must define a non-empty constructor-generated 'policies' list.",
      call. = FALSE
    )
  }

  out <- purrr::map_dfr(policies, function(x) {
    tibble::tibble(
      policy = as.character(x$coded_name %||% NA_character_),
      display_name = as.character(x$display_name %||% NA_character_),
      candidate_pool = as.character(x$candidate_pool %||% NA_character_),
      aggregation = as.character(x$aggregation_method %||% NA_character_),
      requires_admissible = TRUE,
      plain_language_definition = as.character(x$plain_language_definition %||% x$description %||% NA_character_),
      fixed_parameters = list(x$fixed_parameters %||% list()),
      policy_family = as.character(x$policy_family %||% NA_character_),
      grouping_key = as.character(x$grouping_key %||% NA_character_),
      metric_key = as.character(x$metric_key %||% NA_character_)
    )
  })
  required <- c("policy", "candidate_pool", "aggregation", "requires_admissible")
  bad <- out |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(required), ~ is.na(.x) | !nzchar(as.character(.x))))
  if (nrow(bad) > 0) {
    stop("All recommendation policies must define coded_name, candidate_pool, aggregation_method, and requires_admissible.", call. = FALSE)
  }
  if (anyDuplicated(out$policy)) {
    stop("Recommendation policy coded_name values must be unique.", call. = FALSE)
  }
  out
}

#' Classify standardized equation branches
#'
#' @param rows Model rows with `slope_len`.
#' @param tolerance Numeric tolerance around slope 20.
#'
#' @return Character vector with `fixed20`, `free_slope`, or `unknown`.
#' @export
classify_equation_branch <- function(rows,
                                     tolerance = 1e-8) {
  rows_ <- tibble::as_tibble(rows)
  if (!"slope_len" %in% names(rows_)) {
    return(rep("unknown", nrow(rows_)))
  }
  dplyr::case_when(
    !is.finite(suppressWarnings(as.numeric(rows_$slope_len))) ~ "unknown",
    abs(suppressWarnings(as.numeric(rows_$slope_len)) - 20) <= tolerance ~ "fixed20",
    TRUE ~ "free_slope"
  )
}

#' Add source-cell pseudoreplication weights
#'
#' @param rows Model rows.
#' @param source_cols Columns defining an independent source cell.
#'
#' @return `rows` with `recommendation_source_cell` and
#'   `recommendation_source_cell_weight`.
#' @export
add_recommendation_source_weights <- function(rows,
                                              source_cols = c(
                                                "reference_tsl_short",
                                                "species_name",
                                                "frequency",
                                                "recommendation_equation_branch"
                                              )) {
  rows_ <- tibble::as_tibble(rows)
  if (!"recommendation_equation_branch" %in% names(rows_)) {
    rows_$recommendation_equation_branch <- classify_equation_branch(rows_)
  }
  present_cols <- intersect(source_cols, names(rows_))
  if (length(present_cols) == 0) {
    rows_$recommendation_source_cell <- seq_len(nrow(rows_))
    rows_$recommendation_source_cell_weight <- 1
    return(rows_)
  }

  cell_df <- rows_[present_cols]
  cell_df[] <- lapply(cell_df, function(x_now) {
    x_now <- dplyr::coalesce(as.character(x_now), "missing")
    x_now[!nzchar(x_now)] <- "missing"
    x_now
  })
  rows_$recommendation_source_cell <- do.call(paste, c(cell_df, sep = " | "))

  rows_ |>
    dplyr::group_by(.data$recommendation_source_cell) |>
    dplyr::mutate(recommendation_source_cell_weight = 1 / dplyr::n()) |>
    dplyr::ungroup()
}

#' Build a target survey row for recommendation
#'
#' @param target_species Scientific name.
#' @param survey_metadata Named list or one-row data frame of survey metadata.
#' @param candidate_models Candidate model table used to seed missing columns.
#'
#' @return One-row tibble.
#' @export
build_recommendation_target_row <- function(target_species,
                                            survey_metadata = list(),
                                            candidate_models) {
  if (!is.character(target_species) || length(target_species) != 1 || !nzchar(target_species)) {
    stop("'target_species' must be one non-empty species name.", call. = FALSE)
  }
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }

  cast_target_value <- function(value, prototype) {
    if (is.factor(prototype)) {
      return(factor(as.character(value), levels = levels(prototype)))
    }
    if (is.character(prototype)) {
      return(as.character(value))
    }
    if (is.integer(prototype)) {
      return(as.integer(value))
    }
    if (is.numeric(prototype)) {
      return(as.numeric(value))
    }
    if (is.logical(prototype)) {
      return(as.logical(value))
    }
    value
  }

  template <- tibble::as_tibble(candidate_models[0, , drop = FALSE])
  target <- template[1, , drop = FALSE]
  for (nm in names(target)) {
    target[[nm]] <- NA
  }
  if ("model_id" %in% names(target)) {
    if (is.numeric(candidate_models$model_id) || is.integer(candidate_models$model_id)) {
      current_ids <- suppressWarnings(as.numeric(candidate_models$model_id))
      target$model_id <- max(current_ids[is.finite(current_ids)], na.rm = TRUE) + 1
    } else {
      target$model_id <- ".target"
    }
  }
  target$model_id_chr <- as.character(target$model_id[[1]])
  target$species_name <- stringr::str_squish(target_species)

  parts <- strsplit(stringr::str_squish(target_species), "\\s+")[[1]]
  if (length(parts) >= 1 && "genus" %in% names(target)) {
    target$genus <- parts[[1]]
  }
  if (length(parts) >= 2 && "species" %in% names(target)) {
    target$species <- paste(parts[-1], collapse = " ")
  }

  metadata <- tibble::as_tibble(survey_metadata)
  if (nrow(metadata) > 1) {
    stop("'survey_metadata' must be a named list or one-row data frame.", call. = FALSE)
  }
  if (nrow(metadata) == 1) {
    for (nm in intersect(names(metadata), names(target))) {
      target[[nm]] <- cast_target_value(metadata[[nm]], candidate_models[[nm]])
    }
  }

  target
}

#' Prepare reusable recommendation context
#'
#' @param candidate_models Prepared candidate model table, a [Candidates]
#'   object, or a [PolicySelector] object.
#' @param config Similarity/admissibility config.
#' @param registry_path Optional trait registry path.
#'
#' @return List with normalized models and reusable similarity objects.
#'
#' @examples
#' \dontrun{
#' context <- prepare_recommendation_context(selector)
#' }
#'
#' @export
prepare_recommendation_context <- function(candidate_models,
                                           config = NULL,
                                           registry_path = NULL) {
  config_missing <- is.null(config)
  config_ <- if ((inherits(config, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
    config@data
  } else {
    config
  }
  selector_obj <- if ((inherits(candidate_models, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidate_models, PolicySelector), error = function(e) FALSE)))) candidate_models else NULL
  candidates_obj <- if ((inherits(candidate_models, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(candidate_models, Candidates), error = function(e) FALSE)))) candidate_models else NULL
  raw_config <- config_
  candidate_models_ <- candidate_models
  if (!is.null(selector_obj)) {
    if (config_missing) {
      raw_config <- candidates_config_data(selector_obj@candidates)
      config_ <- policy_selector_anchor_config(selector_obj)
    }
    candidates_obj <- selector_obj@candidates
    candidate_models_ <- candidates_obj@candidate_models
  } else if (!is.null(candidates_obj)) {
    if (config_missing) {
      raw_config <- candidates_config_data(candidates_obj)
      config_ <- merge_cfg(
        default_anchor_config(list()),
        policy_selector_similarity_defaults(candidates_obj)
      )
    }
    candidate_models_ <- candidates_obj@candidate_models
  }

  cfg <- read_similarity_config(config_)
  candidate_models_ <- add_recommendation_source_weights(candidate_models_)
  sim_obj <- if (config_missing &&
    !is.null(candidates_obj) &&
    length(candidates_obj@similarity_matrix) > 0) {
    candidates_obj@similarity_matrix
  } else {
    prepare_similarity_matrix(
      candidate_models = candidate_models_,
      species_traits = cfg$species_traits %||% NULL,
      study_traits = cfg$study_traits %||% NULL,
      alpha = cfg$alpha %||% NULL,
      k_species = cfg$k_species %||% NULL,
      k_study = cfg$k_study %||% NULL,
      config = cfg,
      registry_path = registry_path,
      seed = cfg$seed %||% NULL
    )
  }
  dist_obj <- if (config_missing &&
    !is.null(candidates_obj) &&
    length(candidates_obj@gower_distances) > 0) {
    candidates_obj@gower_distances
  } else {
    build_gower_distances(sim_obj)
  }
  candidate_models_prepared <- tibble::as_tibble(sim_obj$candidate_models %||% candidate_models_)
  candidate_models_scored <- screen_missing_metadata(
    candidate_models = candidate_models_prepared,
    key_cols = admissibility_key_metadata_cols(cfg)
  ) |>
    add_recommendation_source_weights()

  list(
    config = cfg,
    raw_config = raw_config,
    candidate_models = candidate_models_scored,
    sim_obj = sim_obj,
    dist_obj = dist_obj
  )
}

#' Restrict recommendation donors by equation branch
#'
#' @param rows Candidate donor rows.
#' @param equation_branch Which standardized equation branch to keep.
#'
#' @return Filtered tibble.
#'
#' @keywords internal
recommendation_branch_filter <- function(rows,
                                         equation_branch = NULL) {
  equation_branch_ <- if (is.null(equation_branch) || length(equation_branch) == 0) {
    "all"
  } else {
    equation_branch
  }
  equation_branch_ <- normalize_policy_equation_branch_filters(equation_branch_)[[1]]
  rows_ <- tibble::as_tibble(rows)
  branch_defs <- read_policy_registry()$policy_branches %||% list()
  filter_lookup <- stats::setNames(
    vapply(
      branch_defs,
      function(x) as.character(x$row_filter_value %||% NA_character_)[[1]],
      character(1)
    ),
    vapply(branch_defs, function(x) as.character(x$key %||% NA_character_), character(1))
  )
  if (!"recommendation_equation_branch" %in% names(rows_)) {
    rows_$recommendation_equation_branch <- classify_equation_branch(rows_)
  }

  filter_value <- unname(filter_lookup[equation_branch_])
  if (is.na(filter_value) || !nzchar(filter_value)) {
    return(rows_)
  }

  dplyr::filter(rows_, .data$recommendation_equation_branch == filter_value)
}

#' Select the donor pool for one recommendation policy
#'
#' @param admissible_rows Admissible donor rows.
#' @param all_rows All valid donor rows before the admissibility restriction.
#' @param policy_def One-row recommendation policy definition.
#' @param policy_params Optional named list of per-policy parameter overrides.
#' @param ordination_info Optional ordination-context list.
#'
#' @return Tibble of donor rows retained for the policy.
#'
#' @keywords internal
recommendation_candidate_pool <- function(admissible_rows,
                                          all_rows,
                                          policy_def,
                                          policy_params = list(),
                                          ordination_info = NULL) {
  pool <- if (isTRUE(policy_def$requires_admissible)) admissible_rows else all_rows
  policy_def_list <- list(
    coded_name = as.character(policy_def$policy[[1]] %||% NA_character_),
    display_name = as.character(policy_def$display_name[[1]] %||% NA_character_),
    candidate_pool = as.character(policy_def$candidate_pool[[1]] %||% NA_character_),
    aggregation_method = as.character(policy_def$aggregation[[1]] %||% NA_character_),
    fixed_parameters = policy_def$fixed_parameters[[1]] %||% list(),
    policy_family = as.character(policy_def$policy_family[[1]] %||% NA_character_),
    grouping_key = as.character(policy_def$grouping_key[[1]] %||% NA_character_),
    metric_key = as.character(policy_def$metric_key[[1]] %||% NA_character_)
  )
  params <- policy_parameters(
    policy_name = policy_def_list$coded_name,
    policy_def = policy_def_list,
    policy_params = policy_params
  )

  policy_rows(
    rows = valid_equation_rows(pool),
    policy_def = policy_def_list,
    policy_params = params,
    ordination_info = resolve_policy_context(ordination_info)
  )
}

#' Aggregate donor equations into one recommendation equation
#'
#' @param rows Donor rows with finite standardized coefficients.
#' @param policy_def One-row recommendation policy definition.
#'
#' @return One-row policy equation tibble.
#'
#' @keywords internal
recommendation_aggregate_equation <- function(rows,
                                              policy_def) {
  policy_def_list <- list(
    coded_name = as.character(policy_def$policy[[1]] %||% NA_character_),
    candidate_pool = as.character(policy_def$candidate_pool[[1]] %||% NA_character_),
    aggregation_method = as.character(policy_def$aggregation[[1]] %||% NA_character_),
    fixed_parameters = policy_def$fixed_parameters[[1]] %||% list()
  )
  policy_equation(valid_equation_rows(rows), policy_def = policy_def_list)
}

#' Resolve config-driven overlap-count diagnostics
#'
#' @param rows Donor rows retained for the policy.
#' @param config Optional similarity/policy config used to define the active
#'   candidate traits.
#'
#' @return One-row tibble with one donor-count column per active overlap trait.
#'
#' @keywords internal
recommendation_overlap_support_summary <- function(rows,
                                                   config = NULL) {
  rows_ <- tibble::as_tibble(rows)
  overlap_cols <- names(rows_)[startsWith(names(rows_), "overlap_same_")]
  if (length(overlap_cols) == 0) {
    return(tibble::tibble())
  }

  normalize_overlap_key <- function(x_now) {
    x_now <- tolower(as.character(x_now))
    x_now <- sub("_name$", "", x_now)
    x_now <- sub("_type$", "", x_now)
    x_now
  }
  resolve_trait_names <- function(x_now) {
    if (is.null(x_now)) {
      return(character(0))
    }
    if (!is.null(names(x_now)) && any(!is.na(names(x_now))) && any(nzchar(names(x_now)))) {
      return(names(x_now))
    }
    as.character(unlist(x_now, use.names = FALSE))
  }

  active_keys <- character(0)
  if (!is.null(config)) {
    config_ <- if ((inherits(config, "S7_object") && exists("Configurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
      config@data
    } else {
      config
    }
    similarity_section <- if (is.list(config_)) config_$similarity %||% list() else list()
    policy_section <- if (is.list(config_)) config_$policy %||% list() else list()
    active_traits <- unique(c(
      resolve_trait_names(
        similarity_section$species_traits %||%
          policy_section$species_traits %||%
          config_$species_traits %||%
          list()
      ),
      resolve_trait_names(
        similarity_section$study_traits %||%
          policy_section$study_traits %||%
          config_$study_traits %||%
          list()
      )
    ))
    active_traits <- active_traits[!is.na(active_traits) & nzchar(active_traits)]
    active_keys <- normalize_overlap_key(active_traits)
  }

  overlap_suffix <- sub("^overlap_same_", "", overlap_cols)
  overlap_keys <- normalize_overlap_key(overlap_suffix)
  keep <- if (length(active_keys) == 0) {
    rep(TRUE, length(overlap_cols))
  } else {
    overlap_keys %in% active_keys
  }
  overlap_cols <- overlap_cols[keep]
  overlap_suffix <- overlap_suffix[keep]
  overlap_keys <- overlap_keys[keep]
  if (length(overlap_cols) == 0) {
    return(tibble::tibble())
  }

  count_values <- vapply(
    overlap_cols,
    function(nm) as.integer(sum(as.logical(rows_[[nm]]), na.rm = TRUE)),
    integer(1)
  )
  count_names <- paste0("n_same_", overlap_keys, "_donors")
  count_tbl <- stats::setNames(as.list(unname(count_values)), count_names)
  tibble::as_tibble(count_tbl)
}

#' Summarize donor support for one recommendation policy
#'
#' @param rows Donor equation rows.
#' @param policy_def One-row recommendation policy definition.
#' @param equation_branch Standardized equation branch label.
#' @param pred One-row aggregated policy equation from
#'   [recommendation_aggregate_equation()].
#' @param anchor_pdf Anchor length-density support from the admissibility step.
#' @param config Optional similarity/policy config used to define which overlap
#'   traits are active for the candidate set.
#'
#' @return One-row tibble with donor-count, overlap, distance, and structural
#'   spread diagnostics.
#'
#' @keywords internal
recommendation_support_summary <- function(rows,
                                           policy_def,
                                           equation_branch,
                                           pred,
                                           anchor_pdf,
                                           config = NULL) {
  rows_ <- valid_equation_rows(rows)
  n_rows <- nrow(rows_)
  n_cells <- if ("recommendation_source_cell" %in% names(rows_)) {
    dplyr::n_distinct(rows_$recommendation_source_cell)
  } else {
    n_rows
  }
  w <- if ("recommendation_final_weight" %in% names(rows_)) {
    normalized_weights(rows_$recommendation_final_weight)
  } else {
    normalized_weights(rep(1, n_rows))
  }
  effective_n <- if (length(w) == 0) NA_real_ else 1 / sum(w^2)
  finite_min <- function(x_now) {
    x_now <- suppressWarnings(as.numeric(x_now))
    x_now <- x_now[is.finite(x_now)]
    if (length(x_now) == 0) {
      return(NA_real_)
    }
    min(x_now)
  }
  finite_weighted_mean <- function(x_now, weights_now) {
    x_now <- suppressWarnings(as.numeric(x_now))
    weights_now <- suppressWarnings(as.numeric(weights_now))
    ok <- is.finite(x_now) & is.finite(weights_now) & weights_now > 0
    if (!any(ok)) {
      return(NA_real_)
    }
    stats::weighted.mean(x_now[ok], weights_now[ok])
  }
  finite_count_branch <- function(branch) {
    if (!"recommendation_equation_branch" %in% names(rows_) || n_rows == 0) {
      return(0L)
    }
    as.integer(sum(rows_$recommendation_equation_branch == branch, na.rm = TRUE))
  }

  structural <- policy_structural_summary(
    rows = rows_,
    policy_def = list(aggregation_method = as.character(policy_def$aggregation[[1]] %||% NA_character_)),
    pred = pred,
    anchor_pdf = anchor_pdf
  )
  overlap_support <- recommendation_overlap_support_summary(
    rows = rows_,
    config = config
  )

  tibble::tibble(
    n_donor_models = n_rows,
    n_independent_source_cells = as.integer(n_cells),
    effective_donor_n = effective_n,
    min_combined_distance = if (n_rows > 0 && "combined_distance" %in% names(rows_)) finite_min(rows_$combined_distance) else NA_real_,
    weighted_mean_combined_distance = if (n_rows > 0 && "combined_distance" %in% names(rows_) && length(w) > 0) {
      finite_weighted_mean(rows_$combined_distance, w)
    } else {
      NA_real_
    },
    min_taxonomic_distance = if (n_rows > 0 && "taxonomic_distance_to_anchor" %in% names(rows_)) finite_min(rows_$taxonomic_distance_to_anchor) else NA_real_,
    weighted_mean_taxonomic_distance = if (n_rows > 0 && "taxonomic_distance_to_anchor" %in% names(rows_) && length(w) > 0) {
      finite_weighted_mean(rows_$taxonomic_distance_to_anchor, w)
    } else {
      NA_real_
    },
    min_length_overlap_fraction = if (n_rows > 0 && "length_overlap_fraction" %in% names(rows_)) finite_min(rows_$length_overlap_fraction) else NA_real_,
    min_depth_overlap_fraction = if (n_rows > 0 && "depth_overlap_fraction" %in% names(rows_)) finite_min(rows_$depth_overlap_fraction) else NA_real_,
    n_fixed20_donors = finite_count_branch("fixed20"),
    n_free_slope_donors = finite_count_branch("free_slope")
  ) |>
    dplyr::bind_cols(overlap_support) |>
    dplyr::bind_cols(structural)
}

#' Evaluate streamlined recommendation policies from an evaluation object
#'
#' This adapter is compatible with [run_policy_benchmark()] and lets the
#' transparent recommendation policy set be evaluated under pseudo-anchor and
#' species-block validation.
#'
#' @param eval_obj Output from the internal single-anchor admissibility screen.
#' @param ordination_info Optional ordination context passed through to
#'   constructor-defined candidate pools.
#' @param policies Optional policy names from [recommendation_policy_set()].
#' @param policy_params Optional list. Supported entries are
#'   `equation_branches` plus any per-policy parameter overrides accepted by
#'   the canonical constructor-defined policies.
#' @param policy_path Ignored; accepted for benchmark compatibility.
#'
#' @return Benchmark-ready policy table.
#' @export
evaluate_recommendation_policies <- function(eval_obj,
                                             ordination_info = NULL,
                                             policies = NULL,
                                             policy_params = list(),
                                             policy_path = NULL) {
  if (!is.list(eval_obj) || is.null(eval_obj$model_eval) || !is.data.frame(eval_obj$model_eval)) {
    stop("'eval_obj' must contain a 'model_eval' data frame.", call. = FALSE)
  }
  policy_defs <- recommendation_policy_set(policy_path = policy_path)
  if (!is.null(policies)) {
    policy_defs <- dplyr::filter(policy_defs, .data$policy %in% policies)
  }
  equation_branches <- policy_params$equation_branches %||%
    vapply(read_policy_registry()$policy_branches %||% list(), function(x) as.character(x$key %||% NA_character_), character(1))
  equation_branches <- normalize_policy_equation_branch_filters(equation_branches)

  all_rows <- tibble::as_tibble(eval_obj$model_eval) |>
    dplyr::filter(dplyr::coalesce(.data$gate_not_self, TRUE)) |>
    add_recommendation_source_weights()
  admissible_rows <- all_rows |>
    dplyr::filter(.data$admissible, is.finite(.data$w_combined), .data$w_combined > 0) |>
    dplyr::group_by(.data$recommendation_source_cell) |>
    dplyr::mutate(w_recommendation_cell_raw = .data$w_combined / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      w_adm = {
        denom <- sum(.data$w_recommendation_cell_raw, na.rm = TRUE)
        if (is.finite(denom) && denom > 0) {
          .data$w_recommendation_cell_raw / denom
        } else {
          rep(NA_real_, dplyr::n())
        }
      }
    )

  add_weights <- function(x_now) {
    x_now <- tibble::as_tibble(x_now)
    if (!"w_adm" %in% names(x_now)) {
      x_now$w_adm <- 1
    }
    x_now |>
      dplyr::mutate(
        recommendation_final_weight = dplyr::coalesce(.data$w_adm, 0) *
          dplyr::coalesce(.data$recommendation_source_cell_weight, 1)
      )
  }
  all_rows <- add_weights(all_rows)
  admissible_rows <- add_weights(admissible_rows)

  purrr::pmap_dfr(
    tidyr::crossing(
      policy_row = seq_len(nrow(policy_defs)),
      equation_branch = equation_branches
    ),
    function(policy_row, equation_branch) {
      policy_def <- policy_defs[policy_row, , drop = FALSE]
      branch_adm <- recommendation_branch_filter(admissible_rows, equation_branch)
      branch_all <- recommendation_branch_filter(all_rows, equation_branch)
      donors <- recommendation_candidate_pool(
        admissible_rows = branch_adm,
        all_rows = branch_all,
        policy_def = policy_def,
        policy_params = policy_params,
        ordination_info = ordination_info
      )
      pred <- policy_prediction(
        rows = donors,
        policy_def = list(
          coded_name = as.character(policy_def$policy[[1]] %||% NA_character_),
          candidate_pool = as.character(policy_def$candidate_pool[[1]] %||% NA_character_),
          aggregation_method = as.character(policy_def$aggregation[[1]] %||% NA_character_),
          fixed_parameters = policy_def$fixed_parameters[[1]] %||% list()
        ),
        anchor_sigma = eval_obj$anchor_sigma,
        anchor_pdf = eval_obj$anchor_pdf
      )
      diagnostics <- recommendation_support_summary(
        rows = donors,
        policy_def = policy_def,
        equation_branch = equation_branch,
        pred = pred,
        anchor_pdf = eval_obj$anchor_pdf,
        config = policy_params$config %||% NULL
      )
      policy_id <- paste(policy_def$policy[[1]], equation_branch, sep = "__")

      tibble::tibble(
        policy = policy_id,
        recommendation_policy = policy_def$policy[[1]],
        equation_branch = equation_branch,
        n_models = nrow(donors),
        policy_family = policy_def$policy_family[[1]],
        candidate_pool = policy_def$candidate_pool[[1]],
        aggregation_method = policy_def$aggregation[[1]],
        plain_language_definition = policy_def$plain_language_definition[[1]]
      ) |>
        dplyr::bind_cols(pred) |>
        dplyr::bind_cols(diagnostics)
    }
  )
}

#' Build validation summary for streamlined recommendation policies
#'
#' @param policy_perf Policy-performance table from [run_policy_benchmark()]
#'   using [evaluate_recommendation_policies()].
#'
#' @return Policy-by-branch validation summary.
#' @export
summarize_recommendation_validation <- function(policy_perf) {
  policy_perf_ <- tibble::as_tibble(policy_perf)
  if (!all(c("policy", "valid_prediction", "error_abs_log") %in% names(policy_perf_))) {
    return(tibble::tibble())
  }
  if (!"recommendation_policy" %in% names(policy_perf_)) {
    policy_perf_$recommendation_policy <- sub("__.*$", "", as.character(policy_perf_$policy))
  }
  if (!"equation_branch" %in% names(policy_perf_)) {
    policy_perf_$equation_branch <- sub("^.*__", "", as.character(policy_perf_$policy))
  }

  policy_perf_ |>
    dplyr::filter(.data$valid_prediction, is.finite(.data$error_abs_log)) |>
    dplyr::group_by(policy = .data$recommendation_policy, equation_branch = .data$equation_branch) |>
    dplyr::summarise(
      validation_n = dplyr::n(),
      validated_median_abs_log_error = stats::median(.data$error_abs_log, na.rm = TRUE),
      validated_mean_abs_log_error = mean(.data$error_abs_log, na.rm = TRUE),
      validated_q90_abs_log_error = stats::quantile(.data$error_abs_log, probs = 0.90, na.rm = TRUE, names = FALSE, type = 8),
      .groups = "drop"
    )
}

#' Rescale one support component onto a bounded score
#'
#' @param x Numeric vector to rescale.
#' @param direction Whether larger or smaller values imply stronger support.
#'
#' @return Numeric vector.
#'
#' @keywords internal
recommendation_support_component <- function(x,
                                             direction = c("high", "low")) {
  direction_ <- match.arg(direction)
  x_ <- suppressWarnings(as.numeric(x))
  out <- rep(NA_real_, length(x_))
  ok <- is.finite(x_)
  if (!any(ok)) {
    return(out)
  }
  if (identical(direction_, "high")) {
    out[ok] <- 1 - exp(-pmax(x_[ok], 0))
  } else {
    out[ok] <- 1 / (1 + pmax(x_[ok], 0))
  }
  out
}

#' Compute target-local support scores for recommendation intervals
#'
#' @param tbl Recommendation policy rows.
#'
#' @return Numeric vector approximately between 0 and 1; larger values indicate
#'   stronger transfer support.
#' @export
recommendation_local_support_score <- function(tbl) {
  tbl_ <- tibble::as_tibble(tbl)
  n <- nrow(tbl_)
  if (n == 0) {
    return(numeric())
  }
  num_col <- function(nm) {
    if (nm %in% names(tbl_)) {
      suppressWarnings(as.numeric(tbl_[[nm]]))
    } else {
      rep(NA_real_, n)
    }
  }
  first_finite <- function(...) {
    vals <- list(...)
    out <- rep(NA_real_, n)
    for (v in vals) {
      replace <- !is.finite(out) & is.finite(v)
      out[replace] <- v[replace]
    }
    out
  }

  distance <- first_finite(
    num_col("weighted_mean_combined_distance"),
    num_col("min_combined_distance"),
    num_col("anchor_selection_local_distance")
  )
  source_cells <- first_finite(
    num_col("n_independent_source_cells"),
    num_col("effective_donor_n"),
    num_col("n_donor_models"),
    num_col("n_models")
  )
  effective_n <- first_finite(
    num_col("effective_donor_n"),
    num_col("n_independent_source_cells"),
    num_col("n_donor_models"),
    num_col("n_models")
  )
  structural_spread <- first_finite(
    num_col("local_structural_q_abs_log"),
    num_col("donor_log_sigma_abs_dev_q90"),
    num_col("donor_curve_rmse_q90")
  )
  length_overlap <- first_finite(
    num_col("min_length_overlap_fraction"),
    num_col("length_overlap_fraction")
  )
  depth_overlap <- first_finite(
    num_col("min_depth_overlap_fraction"),
    num_col("depth_overlap_fraction")
  )

  components <- cbind(
    recommendation_support_component(distance, "low"),
    recommendation_support_component(source_cells / 5, "high"),
    recommendation_support_component(effective_n / 5, "high"),
    recommendation_support_component(structural_spread, "low"),
    pmin(pmax(length_overlap, 0), 1),
    pmin(pmax(depth_overlap, 0), 1)
  )
  score <- rowMeans(components, na.rm = TRUE)
  score[!is.finite(score)] <- NA_real_
  score
}

#' Assign support bins to recommendation rows
#'
#' @param tbl Recommendation policy rows.
#' @param cutpoints Optional fixed support-score cutpoints.
#' @param n_bins Number of quantile bins when `cutpoints` is not supplied.
#'
#' @return `tbl` with support-score and support-bin columns.
#'
#' @keywords internal
assign_recommendation_support_bins <- function(tbl,
                                               cutpoints = NULL,
                                               n_bins = 4L) {
  # Reuse the shared support-binning logic with recommendation-specific names.
  tbl_ <- tibble::as_tibble(tbl)
  if (nrow(tbl_) == 0) {
    tbl_$recommendation_support_score <- numeric()
    tbl_$recommendation_support_bin <- character()
    return(tbl_)
  }
  assign_support_bins(
    tbl = tbl_,
    score = recommendation_local_support_score(tbl_),
    score_col = "recommendation_support_score",
    bin_col = "recommendation_support_bin",
    cutpoints = cutpoints,
    n_bins = n_bins,
    all_bin = "support_all",
    missing_bin = "support_missing"
  )
}

#' Calibrate local recommendation conformal intervals
#'
#' @param policy_perf Recommendation policy-performance table.
#' @param alpha Miscoverage level.
#' @param n_support_bins Number of support bins.
#' @param min_policy_bin_scores Minimum scores required for policy-by-bin
#'   quantiles.
#' @param min_bin_scores Minimum scores required for support-bin fallback
#'   quantiles.
#'
#' @return A calibration object used by [add_local_recommendation_intervals()].
#' @export
calibrate_local_recommendation_conformal <- function(policy_perf,
                                                     alpha = 0.10,
                                                     n_support_bins = 4L,
                                                     min_policy_bin_scores = 15L,
                                                     min_bin_scores = 30L) {
  policy_perf_ <- tibble::as_tibble(policy_perf)
  if (!all(c("policy", "valid_prediction", "error_abs_log") %in% names(policy_perf_))) {
    stop("'policy_perf' must include policy, valid_prediction, and error_abs_log.", call. = FALSE)
  }
  valid <- policy_perf_ |>
    dplyr::filter(.data$valid_prediction, is.finite(.data$error_abs_log)) |>
    dplyr::mutate(
      recommendation_policy = dplyr::coalesce(
        .data$recommendation_policy,
        sub("__.*$", "", .data$policy)
      ),
      equation_branch = dplyr::coalesce(
        .data$equation_branch,
        sub("^.*__", "", .data$policy)
      )
    )
  valid <- assign_recommendation_support_bins(valid, n_bins = n_support_bins)
  cutpoints <- unique(stats::quantile(
    valid$recommendation_support_score,
    probs = seq(0, 1, length.out = as.integer(n_support_bins) + 1L),
    na.rm = TRUE,
    names = FALSE,
    type = 8
  ))

  finite_q <- function(x, alpha_now) {
    conformal_quantile(x, alpha = alpha_now)
  }
  global_q <- finite_q(valid$error_abs_log, alpha)

  bin_quantiles <- valid |>
    dplyr::group_by(.data$recommendation_support_bin) |>
    dplyr::summarise(
      n_scores = dplyr::n(),
      q_abs_log_raw = finite_q(.data$error_abs_log, alpha),
      median_abs_log = stats::median(.data$error_abs_log, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      q_abs_log = dplyr::if_else(
        .data$n_scores >= as.integer(min_bin_scores) & is.finite(.data$q_abs_log_raw),
        .data$q_abs_log_raw,
        global_q
      ),
      interval_source = dplyr::if_else(
        .data$n_scores >= as.integer(min_bin_scores) & is.finite(.data$q_abs_log_raw),
        "support_bin",
        "global"
      )
    )

  policy_quantiles <- valid |>
    dplyr::group_by(.data$recommendation_policy, .data$equation_branch) |>
    dplyr::summarise(
      n_scores = dplyr::n(),
      q_abs_log_raw = finite_q(.data$error_abs_log, alpha),
      median_abs_log = stats::median(.data$error_abs_log, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      q_abs_log = dplyr::if_else(is.finite(.data$q_abs_log_raw), .data$q_abs_log_raw, global_q)
    )

  policy_bin_quantiles <- valid |>
    dplyr::group_by(.data$recommendation_policy, .data$equation_branch, .data$recommendation_support_bin) |>
    dplyr::summarise(
      n_scores = dplyr::n(),
      q_abs_log_raw = finite_q(.data$error_abs_log, alpha),
      median_abs_log = stats::median(.data$error_abs_log, na.rm = TRUE),
      .groups = "drop"
    )

  policy_bin_quantiles <- policy_bin_quantiles |>
    dplyr::left_join(
      bin_quantiles |>
        dplyr::select(
          "recommendation_support_bin",
          bin_n_scores = "n_scores",
          bin_q_abs_log = "q_abs_log"
        ),
      by = "recommendation_support_bin"
    ) |>
    dplyr::left_join(
      policy_quantiles |>
        dplyr::select(
          "recommendation_policy",
          "equation_branch",
          policy_n_scores = "n_scores",
          policy_q_abs_log = "q_abs_log"
        ),
      by = c("recommendation_policy", "equation_branch")
    ) |>
    dplyr::mutate(
      q_abs_log = dplyr::case_when(
        .data$n_scores >= as.integer(min_policy_bin_scores) & is.finite(.data$q_abs_log_raw) ~ .data$q_abs_log_raw,
        is.finite(.data$bin_q_abs_log) ~ .data$bin_q_abs_log,
        is.finite(.data$policy_q_abs_log) ~ .data$policy_q_abs_log,
        TRUE ~ global_q
      ),
      interval_source = dplyr::case_when(
        .data$n_scores >= as.integer(min_policy_bin_scores) & is.finite(.data$q_abs_log_raw) ~ "policy_support_bin",
        is.finite(.data$bin_q_abs_log) ~ "support_bin",
        is.finite(.data$policy_q_abs_log) ~ "policy_branch",
        TRUE ~ "global"
      )
    )

  coverage_by_bin <- valid |>
    dplyr::left_join(
      bin_quantiles |>
        dplyr::select("recommendation_support_bin", "q_abs_log"),
      by = "recommendation_support_bin"
    ) |>
    dplyr::group_by(.data$recommendation_support_bin) |>
    dplyr::summarise(
      n = dplyr::n(),
      empirical_coverage = mean(.data$error_abs_log <= .data$q_abs_log, na.rm = TRUE),
      median_abs_log = stats::median(.data$error_abs_log, na.rm = TRUE),
      q_abs_log = dplyr::first(.data$q_abs_log),
      interval_factor = exp(dplyr::first(.data$q_abs_log)),
      .groups = "drop"
    )
  coverage_by_policy_bin <- valid |>
    dplyr::left_join(
      policy_bin_quantiles |>
        dplyr::select(
          "recommendation_policy",
          "equation_branch",
          "recommendation_support_bin",
          "q_abs_log",
          "interval_source"
        ),
      by = c("recommendation_policy", "equation_branch", "recommendation_support_bin")
    ) |>
    dplyr::group_by(.data$recommendation_policy, .data$equation_branch, .data$recommendation_support_bin, .data$interval_source) |>
    dplyr::summarise(
      n = dplyr::n(),
      empirical_coverage = mean(.data$error_abs_log <= .data$q_abs_log, na.rm = TRUE),
      median_abs_log = stats::median(.data$error_abs_log, na.rm = TRUE),
      q_abs_log = dplyr::first(.data$q_abs_log),
      interval_factor = exp(dplyr::first(.data$q_abs_log)),
      .groups = "drop"
    )

  list(
    calibration_method = "local_recommendation_conformal",
    alpha = alpha,
    target_coverage = 1 - alpha,
    support_cutpoints = cutpoints,
    n_support_bins = as.integer(n_support_bins),
    min_policy_bin_scores = as.integer(min_policy_bin_scores),
    min_bin_scores = as.integer(min_bin_scores),
    global_q_abs_log = global_q,
    global_interval_factor = exp(global_q),
    bin_quantiles = bin_quantiles,
    policy_quantiles = policy_quantiles,
    policy_bin_quantiles = policy_bin_quantiles,
    coverage_by_bin = coverage_by_bin,
    coverage_by_policy_bin = coverage_by_policy_bin,
    calibration_rows = valid
  )
}

#' Add local conformal interval factors to recommendation rows
#'
#' @param tbl Recommendation policy rows.
#' @param calibration_obj Output from
#'   [calibrate_local_recommendation_conformal()].
#'
#' @return `tbl` with local conformal interval columns.
#' @export
add_local_recommendation_intervals <- function(tbl,
                                               calibration_obj) {
  tbl <- tibble::as_tibble(tbl)
  if (nrow(tbl) == 0 || is.null(calibration_obj)) {
    return(tbl)
  }
  out <- tbl |>
    dplyr::mutate(
      recommendation_policy = dplyr::coalesce(
        .data$policy,
        sub("__.*$", "", .data$policy)
      ),
      equation_branch = dplyr::coalesce(
        .data$equation_branch,
        sub("^.*__", "", .data$policy)
      )
    )
  out <- assign_recommendation_support_bins(
    out,
    cutpoints = calibration_obj$support_cutpoints,
    n_bins = calibration_obj$n_support_bins %||% 4L
  )
  out |>
    dplyr::left_join(
      calibration_obj$policy_bin_quantiles |>
        dplyr::select(
          "recommendation_policy",
          "equation_branch",
          "recommendation_support_bin",
          local_conformal_n_scores = "n_scores",
          local_conformal_q_abs_log = "q_abs_log",
          local_conformal_source = "interval_source"
        ),
      by = c("recommendation_policy", "equation_branch", "recommendation_support_bin")
    ) |>
    dplyr::mutate(
      local_conformal_q_abs_log = dplyr::coalesce(
        .data$local_conformal_q_abs_log,
        calibration_obj$global_q_abs_log
      ),
      local_conformal_source = dplyr::coalesce(.data$local_conformal_source, "global"),
      q_abs_log_total = .data$local_conformal_q_abs_log,
      interval_factor = exp(.data$q_abs_log_total)
    )
}

#' Replace non-finite values for dominance comparisons
#'
#' @param x Numeric vector.
#' @param direction Whether smaller or larger values are preferred.
#'
#' @return Numeric vector with non-finite values mapped to the dominated side.
#'
#' @keywords internal
finite_for_dominance <- function(x,
                                 direction = c("min", "max")) {
  direction <- match.arg(direction)
  x <- suppressWarnings(as.numeric(x))
  if (identical(direction, "min")) {
    x[!is.finite(x)] <- Inf
  } else {
    x[!is.finite(x)] <- -Inf
  }
  x
}

#' Flag dominated recommendation rows
#'
#' @param tbl Recommendation summary table.
#' @param tolerance Named list of absolute tolerances used in the dominance
#'   comparisons.
#'
#' @return `tbl` with Pareto-dominance annotations.
#'
#' @keywords internal
flag_recommendation_dominance <- function(tbl,
                                          tolerance = list()) {
  tbl <- tibble::as_tibble(tbl)
  if (nrow(tbl) == 0) {
    tbl$pareto_non_dominated <- logical()
    tbl$dominated_by_policy <- character()
    tbl$dominance_reason <- character()
    return(tbl)
  }
  validation_tol <- tolerance$validation_abs %||% 0
  uncertainty_tol <- tolerance$uncertainty_abs %||% 0
  distance_tol <- tolerance$distance_abs %||% 1e-12
  structural_tol <- tolerance$structural_abs %||% 0
  support_tol <- tolerance$support_abs %||% 0

  validation <- finite_for_dominance(
    dplyr::coalesce(tbl$validated_median_abs_log_error, tbl$median_abs_log_error),
    "min"
  )
  uncertainty <- finite_for_dominance(tbl$q_abs_log_total, "min")
  distance <- finite_for_dominance(
    dplyr::coalesce(tbl$weighted_mean_combined_distance, tbl$min_combined_distance),
    "min"
  )
  structural <- finite_for_dominance(tbl$local_structural_q_abs_log, "min")
  support <- finite_for_dominance(tbl$n_independent_source_cells, "max")
  policy_label <- paste(tbl$policy, tbl$equation_branch, sep = " / ")

  dominated <- rep(FALSE, nrow(tbl))
  dominated_by <- rep(NA_character_, nrow(tbl))
  reason <- rep(NA_character_, nrow(tbl))

  for (i in seq_len(nrow(tbl))) {
    for (j in seq_len(nrow(tbl))) {
      if (identical(i, j)) {
        next
      }
      no_worse <- validation[j] <= validation[i] + validation_tol &&
        uncertainty[j] <= uncertainty[i] + uncertainty_tol &&
        distance[j] <= distance[i] + distance_tol &&
        structural[j] <= structural[i] + structural_tol
      strictly_better <- validation[j] < validation[i] - validation_tol ||
        uncertainty[j] < uncertainty[i] - uncertainty_tol ||
        distance[j] < distance[i] - distance_tol ||
        structural[j] < structural[i] - structural_tol ||
        support[j] > support[i] + support_tol
      if (isTRUE(no_worse && strictly_better)) {
        dominated[[i]] <- TRUE
        dominated_by[[i]] <- policy_label[[j]]
        reason[[i]] <- paste(
          "Dominated on validation error, interval width, transfer distance, structural spread, and independent source-cell support by",
          policy_label[[j]]
        )
        break
      }
    }
  }

  tbl$pareto_non_dominated <- !dominated
  tbl$dominated_by_policy <- dominated_by
  tbl$dominance_reason <- reason
  tbl
}

#' Finalize the displayed recommendation set
#'
#' @param selected Recommendation rows after interval attachment and validation
#'   joins.
#' @param dominance_tolerance Named list of absolute tolerances passed to
#'   [flag_recommendation_dominance()].
#'
#' @return Filtered and ranked recommendation table.
#'
#' @keywords internal
finalize_recommendation_selection <- function(selected,
                                              dominance_tolerance = list()) {
  selected <- flag_recommendation_dominance(selected, tolerance = dominance_tolerance)
  selected <- selected |>
    dplyr::filter(.data$pareto_non_dominated)
  if (nrow(selected) == 0) {
    return(selected)
  }
  if (!"validated_median_abs_log_error" %in% names(selected)) {
    selected$validated_median_abs_log_error <- NA_real_
  }
  if (!"median_abs_log_error" %in% names(selected)) {
    selected$median_abs_log_error <- NA_real_
  }
  selected <- selected |>
    dplyr::mutate(
      recommendation_validation_sort = dplyr::coalesce(
        .data$validated_median_abs_log_error,
        .data$median_abs_log_error
      )
    ) |>
    dplyr::arrange(
      .data$q_abs_log_total,
      .data$local_structural_q_abs_log,
      .data$weighted_mean_combined_distance,
      .data$min_combined_distance,
      .data$recommendation_validation_sort,
      dplyr::desc(.data$n_independent_source_cells),
      .data$policy,
      .data$equation_branch
    ) |>
    dplyr::mutate(
      selected_recommendation_rank = dplyr::row_number(),
      primary_recommendation = .data$selected_recommendation_rank == 1L,
      recommendation_role = dplyr::if_else(
        .data$primary_recommendation,
        "primary",
        "equivalent_non_dominated_alternative"
      )
    )
  selected
}

#' Evaluate transparent recommendation policies for one target
#'
#' @param target_species Scientific name.
#' @param survey_metadata Named list or one-row data frame.
#' @param candidate_models Prepared candidate model table, a [Candidates]
#'   object, or a [PolicySelector] object.
#' @param config Similarity/admissibility config with tuned values frozen.
#' @param context Optional output from [prepare_recommendation_context()].
#' @param policies Policy definition table.
#' @param equation_branches Branches to evaluate.
#' @param support_fraction Local-support ensemble fraction.
#' @param validation_summary Optional policy-by-branch validation table.
#' @param local_conformal_calibration Optional output from
#'   [calibrate_local_recommendation_conformal()].
#' @param selection_config List of support and tie thresholds.
#' @param registry_path Optional trait registry path.
#'
#' @return A list with ranked policies, selected recommendations, donor rows,
#'   and diagnostics.
#'
#' @examples
#' \dontrun{
#' recommendation <- recommend_ts_model(
#'   target_species = "Sardinops sagax",
#'   survey_metadata = list(frequency = 38),
#'   candidate_models = selector
#' )
#' recommendation$selected
#' }
#'
#' @export
recommend_ts_model <- function(target_species,
                               survey_metadata = list(),
                               candidate_models = NULL,
                               config = NULL,
                               context = NULL,
                               policies = recommendation_policy_set(),
                               equation_branches = NULL,
                               support_fraction = 0.80,
                               validation_summary = NULL,
                               local_conformal_calibration = NULL,
                               selection_config = list(),
                               registry_path = NULL) {
  if ((inherits(context, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(context, PolicySelector), error = function(e) FALSE))) || (inherits(context, "S7_object") && exists("Candidates", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(context, Candidates), error = function(e) FALSE)))) {
    candidate_models <- context
    context <- NULL
  }
  if (is.null(candidate_models) && is.null(context)) {
    stop(
      "Supply `candidate_models` as a table or staged object, or provide an explicit recommendation `context`.",
      call. = FALSE
    )
  }
  if (is.null(context)) {
    context <- prepare_recommendation_context(
      candidate_models = candidate_models,
      config = config,
      registry_path = registry_path
    )
  }
  if (is.null(equation_branches) || length(equation_branches) == 0) {
    equation_branches <- vapply(read_policy_registry()$policy_branches %||% list(), function(x) as.character(x$key %||% NA_character_), character(1))
  }
  equation_branches <- normalize_policy_equation_branch_filters(equation_branches)

  target_row <- build_recommendation_target_row(
    target_species = target_species,
    survey_metadata = survey_metadata,
    candidate_models = context$candidate_models
  )
  target_id <- as.character(target_row$model_id_chr[[1]])
  eval_models <- dplyr::bind_rows(context$candidate_models, target_row)
  eval_context <- prepare_recommendation_context(
    candidate_models = eval_models,
    config = config %||% context$config,
    registry_path = registry_path
  )
  eval_obj <- screen_one_anchor_admissibility(
    anchor_row = target_row,
    candidate_models = eval_context$candidate_models,
    config = config %||% context$config,
    registry_path = registry_path,
    sim_obj = eval_context$sim_obj,
    dist_obj = eval_context$dist_obj,
    candidate_models_scored = eval_context$candidate_models
  )

  all_rows <- tibble::as_tibble(eval_obj$model_eval) |>
    dplyr::filter(.data$model_id_chr != target_id) |>
    add_recommendation_source_weights()
  admissible_rows <- all_rows |>
    dplyr::filter(.data$admissible, is.finite(.data$w_combined), .data$w_combined > 0) |>
    dplyr::group_by(.data$recommendation_source_cell) |>
    dplyr::mutate(w_recommendation_cell_raw = .data$w_combined / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      w_adm = {
        denom <- sum(.data$w_recommendation_cell_raw, na.rm = TRUE)
        if (is.finite(denom) && denom > 0) {
          .data$w_recommendation_cell_raw / denom
        } else {
          rep(NA_real_, dplyr::n())
        }
      }
    )

  add_weights <- function(x) {
    x <- tibble::as_tibble(x)
    if (!"w_adm" %in% names(x)) {
      x$w_adm <- 1
    }
    x |>
      dplyr::mutate(
        recommendation_final_weight = dplyr::coalesce(.data$w_adm, 0) *
          dplyr::coalesce(.data$recommendation_source_cell_weight, 1)
      )
  }
  all_rows <- add_weights(all_rows)
  admissible_rows <- add_weights(admissible_rows)

  donor_store <- list()
  ranked <- purrr::pmap_dfr(
    tidyr::crossing(
      policy_row = seq_len(nrow(policies)),
      equation_branch = equation_branches
    ),
    function(policy_row, equation_branch) {
      policy_def <- policies[policy_row, , drop = FALSE]
      branch_adm <- recommendation_branch_filter(admissible_rows, equation_branch)
      branch_all <- recommendation_branch_filter(all_rows, equation_branch)
      donors <- recommendation_candidate_pool(
        admissible_rows = branch_adm,
        all_rows = branch_all,
        policy_def = policy_def,
        policy_params = list(),
        ordination_info = NULL
      )
      donor_key <- paste(policy_def$policy[[1]], equation_branch, sep = " / ")
      donor_store[[donor_key]] <<- donors

      pred <- policy_prediction(
        rows = donors,
        policy_def = list(
          coded_name = as.character(policy_def$policy[[1]] %||% NA_character_),
          candidate_pool = as.character(policy_def$candidate_pool[[1]] %||% NA_character_),
          aggregation_method = as.character(policy_def$aggregation[[1]] %||% NA_character_),
          fixed_parameters = policy_def$fixed_parameters[[1]] %||% list()
        ),
        anchor_sigma = eval_obj$anchor_sigma,
        anchor_pdf = eval_obj$anchor_pdf
      )
      diagnostics <- recommendation_support_summary(
        rows = donors,
        policy_def = policy_def,
        equation_branch = equation_branch,
        pred = pred,
        anchor_pdf = eval_obj$anchor_pdf,
        config = config %||% context$raw_config %||% context$config
      )

      tibble::tibble(
        target_species = target_species,
        policy = policy_def$policy[[1]],
        policy_family = policy_def$policy_family[[1]],
        equation_branch = equation_branch,
        candidate_pool = policy_def$candidate_pool[[1]],
        aggregation = policy_def$aggregation[[1]],
        plain_language_definition = policy_def$plain_language_definition[[1]]
      ) |>
        dplyr::bind_cols(pred) |>
        dplyr::bind_cols(diagnostics)
    }
  )

  if (!is.null(validation_summary) && nrow(validation_summary) > 0) {
    validation_summary <- tibble::as_tibble(validation_summary)
    ranked <- ranked |>
      dplyr::left_join(
        validation_summary,
        by = intersect(c("policy", "equation_branch"), names(validation_summary))
      )
  }

  if (!"q_abs_log_total" %in% names(ranked)) {
    ranked$q_abs_log_total <- NA_real_
  }
  if (!is.null(local_conformal_calibration)) {
    ranked <- add_local_recommendation_intervals(ranked, local_conformal_calibration)
  }

  min_source_cells_ensemble <- selection_config$min_source_cells_ensemble %||% 2L
  max_interval_factor <- selection_config$max_interval_factor %||% Inf
  practical_error_tolerance <- selection_config$practical_error_tolerance %||% 0.05
  uncertainty_rule <- normalize_uncertainty_rule(selection_config$uncertainty_rule %||% "tolerance")
  u_tol_rel <- selection_config$u_tol_rel %||% selection_config$uncertainty_relative_tolerance %||% 0.25
  u_tol_abs <- selection_config$u_tol_abs %||% selection_config$uncertainty_absolute_tolerance %||% 0.05
  one_se_multiplier <- selection_config$one_se_multiplier %||% 1
  dominance_tolerance <- selection_config$dominance_tolerance %||% list()

  ranked <- ranked |>
    dplyr::mutate(
      valid_equation = is.finite(.data$policy_slope_len) & is.finite(.data$policy_intercept_len),
      is_ensemble = !.data$policy_family %in% c("single_model", "study_hierarchical_single_model"),
      support_pass = .data$valid_equation &
        (!.data$is_ensemble | .data$n_independent_source_cells >= min_source_cells_ensemble),
      interval_factor = dplyr::if_else(
        is.finite(.data$q_abs_log_total),
        exp(.data$q_abs_log_total),
        NA_real_
      ),
      uncertainty_pass = dplyr::if_else(
        is.finite(.data$interval_factor),
        .data$interval_factor <= max_interval_factor,
        TRUE
      ),
      selection_eligible = .data$support_pass & .data$uncertainty_pass
    )

  rank_cols <- intersect(
    c(
      "validated_median_abs_log_error",
      "median_abs_log_error",
      "q_abs_log_total",
      "local_structural_q_abs_log",
      "weighted_mean_combined_distance",
      "min_combined_distance"
    ),
    names(ranked)
  )
  if (length(rank_cols) == 0) {
    rank_cols <- c("local_structural_q_abs_log", "weighted_mean_combined_distance", "min_combined_distance")
  }

  ranked <- ranked |>
    dplyr::arrange(
      !.data$selection_eligible,
      dplyr::across(dplyr::all_of(rank_cols)),
      dplyr::desc(.data$n_independent_source_cells),
      .data$policy,
      .data$equation_branch
    ) |>
    dplyr::mutate(recommendation_rank = dplyr::row_number())

  eligible <- dplyr::filter(ranked, .data$selection_eligible)
  selected <- if (nrow(eligible) == 0) {
    ranked[0, , drop = FALSE]
  } else {
    primary_col <- intersect(c("validated_median_abs_log_error", "median_abs_log_error"), names(eligible))
    if (length(primary_col) > 0 && any(is.finite(eligible[[primary_col[[1]]]]))) {
      best <- min(eligible[[primary_col[[1]]]], na.rm = TRUE)
      candidates <- dplyr::filter(eligible, .data[[primary_col[[1]]]] <= best + practical_error_tolerance)
      if (nrow(candidates) == 0) {
        candidates <- eligible
      }
      if (any(is.finite(candidates$q_abs_log_total))) {
        best_q <- min(candidates$q_abs_log_total, na.rm = TRUE)
        q_threshold <- uncertainty_width_threshold(
          min_width = best_q,
          uncertainty_rule = uncertainty_rule,
          u_tol_abs = u_tol_abs,
          u_tol_rel = u_tol_rel,
          width_se = uncertainty_width_se(candidates$q_abs_log_total),
          one_se_multiplier = one_se_multiplier
        )
        candidates <- candidates |>
          dplyr::filter(
            is.finite(.data$q_abs_log_total),
            .data$q_abs_log_total <= q_threshold
          )
      }
      candidates |>
        dplyr::arrange(
          .data$q_abs_log_total,
          .data$local_structural_q_abs_log,
          .data$weighted_mean_combined_distance,
          .data$min_combined_distance,
          .data[[primary_col[[1]]]],
          dplyr::desc(.data$n_independent_source_cells),
          .data$policy,
          .data$equation_branch
        )
    } else {
      dplyr::slice(eligible, 1)
    }
  }
  selected <- finalize_recommendation_selection(
    selected,
    dominance_tolerance = dominance_tolerance
  )

  list(
    target = target_row,
    ranked_policies = ranked,
    selected = selected,
    donors = donor_store,
    evaluation = eval_obj,
    selection_config = list(
      min_source_cells_ensemble = min_source_cells_ensemble,
      max_interval_factor = max_interval_factor,
      practical_error_tolerance = practical_error_tolerance,
      support_fraction = support_fraction
    )
  )
}

#' Summarize recommendation diagnostics
#'
#' @param recommendation Output from [recommend_ts_model()].
#'
#' @return A compact diagnostics list.
#' @export
summarize_recommendation_diagnostics <- function(recommendation) {
  ranked <- tibble::as_tibble(recommendation$ranked_policies)
  selected <- tibble::as_tibble(recommendation$selected)
  list(
    n_ranked = nrow(ranked),
    n_valid_equations = sum(ranked$valid_equation, na.rm = TRUE),
    n_selection_eligible = sum(ranked$selection_eligible, na.rm = TRUE),
    selected_policies = selected |>
      dplyr::select(
        dplyr::any_of(c(
          "target_species", "policy", "equation_branch", "policy_slope_len",
          "policy_intercept_len", "recommendation_rank",
          "n_donor_models", "n_independent_source_cells",
          "weighted_mean_combined_distance", "local_structural_q_abs_log"
        ))
      ),
    branch_summary = ranked |>
      dplyr::group_by(.data$equation_branch) |>
      dplyr::summarise(
        n_policies = dplyr::n(),
        n_valid = sum(.data$valid_equation, na.rm = TRUE),
        n_eligible = sum(.data$selection_eligible, na.rm = TRUE),
        median_structural_q_abs_log = stats::median(.data$local_structural_q_abs_log, na.rm = TRUE),
        .groups = "drop"
      ),
    policy_summary = ranked |>
      dplyr::group_by(.data$policy) |>
      dplyr::summarise(
        n_branches = dplyr::n(),
        n_valid = sum(.data$valid_equation, na.rm = TRUE),
        n_eligible = sum(.data$selection_eligible, na.rm = TRUE),
        best_rank = min(.data$recommendation_rank, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$best_rank)
  )
}
