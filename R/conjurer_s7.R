#' Conjurer S7 Class
#'
#' `Conjurer` runs a targeted missing-study-metadata uncertainty analysis on a
#' staged [PolicySelector] and an optional fitted [PolicyLearner]. It keeps the
#' fitted workflow fixed, disables only the missingness admissibility gate for
#' the auxiliary analysis, imputes one selected study trait at a time, and
#' reruns the downstream recommendation path across repeated stochastic draws.
#'
#' The resulting object stores the raw draw-level recommendation outputs and a
#' compact anchor-by-trait instability summary.
#'
#' @examples
#' \dontrun{
#' selector <- as_policy_selector(candidates)
#' selector <- benchmark(selector)
#' selector <- calibrate_uncertainty(selector)
#' selector <- select_policies(selector)
#' learner <- as_policy_learner(selector)
#' learner <- crossfit(learner)
#' learner <- fit(learner)
#' learner <- calibrate_uncertainty(learner)
#'
#' conjurer <- as_conjurer(selector, learner = learner)
#' conjurer <- simulate(conjurer)
#' conjurer@summary
#' }
#'
#' @name Conjurer-class
#' @aliases Conjurer
NULL

#' @rdname Conjurer-class
Conjurer <- S7::new_class(
  "Conjurer",
  properties = list(
    selector = S7::new_property(S7::class_any),
    learner = S7::new_property(S7::class_any),
    config = S7::new_property(S7::class_list),
    results = S7::new_property(S7::class_list),
    manifest = S7::new_property(CandidatesDataFrame),
    draws = S7::new_property(CandidatesDataFrame),
    summary = S7::new_property(CandidatesDataFrame)
  ),
  validator = function(self) {
    tryCatch(
      {
        if (!(inherits(self@selector, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(self@selector, PolicySelector), error = function(e) FALSE)))) {
          return("`selector` must be a `PolicySelector` object.")
        }
        if (!is.null(self@learner) && !(inherits(self@learner, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(self@learner, PolicyLearner), error = function(e) FALSE)))) {
          return("`learner` must be NULL or a `PolicyLearner` object.")
        }
        if (!is.list(self@config)) {
          return("`config` must be a list.")
        }
        if (!is.list(self@results)) {
          return("`results` must be a list.")
        }
        if (!is.data.frame(self@manifest)) {
          return("`manifest` must be a data frame.")
        }
        if (!is.data.frame(self@draws)) {
          return("`draws` must be a data frame.")
        }
        if (!is.data.frame(self@summary)) {
          return("`summary` must be a data frame.")
        }
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(Conjurer)

#' Test whether an object is a `Conjurer` instance
#'
#' Rebuild a `Conjurer`
#'
#' @param object A [Conjurer] object.
#' @param selector Optional replacement [PolicySelector].
#' @param learner Optional replacement [PolicyLearner].
#' @param config Optional replacement config list.
#' @param results Optional replacement results bundle.
#' @param manifest Optional replacement manifest table.
#' @param draws Optional replacement draw-level table.
#' @param summary Optional replacement summary table.
#'
#' @return A `Conjurer` object.
#'
#' @keywords internal
conjurer_rebuild <- function(object,
                             selector = object@selector,
                             learner = object@learner,
                             config = object@config,
                             results = object@results,
                             manifest = object@manifest,
                             draws = object@draws,
                             summary = object@summary) {
  Conjurer(
    selector = selector,
    learner = learner,
    config = config,
    results = results,
    manifest = tibble::as_tibble(manifest),
    draws = tibble::as_tibble(draws),
    summary = tibble::as_tibble(summary)
  )
}

#' Build a `Conjurer`
#'
#' @param selector A [PolicySelector] object.
#' @param learner Optional fitted [PolicyLearner].
#' @param config Optional conjurer config list or [Configurer] object.
#'
#' @return A `Conjurer` object.
#'
#' @examples
#' \dontrun{
#' conjurer <- as_conjurer(selector)
#' conjurer
#' }
#'
#' @export
create_conjurer <- function(selector,
                            learner = NULL,
                            config = NULL) {
  if ((inherits(selector, "S7_object") && exists("Conjurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(selector, Conjurer), error = function(e) FALSE)))) {
    return(selector)
  }
  if (!(inherits(selector, "S7_object") && exists("PolicySelector", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(selector, PolicySelector), error = function(e) FALSE)))) {
    stop("'selector' must be a `PolicySelector` object.", call. = FALSE)
  }
  if (!is.null(learner) && !(inherits(learner, "S7_object") && exists("PolicyLearner", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(learner, PolicyLearner), error = function(e) FALSE)))) {
    stop("'learner' must be NULL or a `PolicyLearner` object.", call. = FALSE)
  }

  Conjurer(
    selector = selector,
    learner = learner,
    config = policy_selector_config_data(config),
    results = list(),
    manifest = tibble::tibble(),
    draws = tibble::tibble(),
    summary = tibble::tibble()
  )
}

#' Coerce selector state to `Conjurer`
#'
#' @param selector A [PolicySelector] object.
#' @param learner Optional [PolicyLearner] object.
#' @param config Optional conjurer config list or [Configurer] object.
#'
#' @return A `Conjurer` object.
#'
#' @export
as_conjurer <- function(selector,
                        learner = NULL,
                        config = NULL) {
  create_conjurer(
    selector = selector,
    learner = learner,
    config = config
  )
}

#' Normalize one trait-weight map
#'
#' @param x Trait-weight input.
#'
#' @return Named numeric vector.
#'
#' @keywords internal
conjurer_weight_map <- function(x) {
  # Collapse list and vector inputs to one named numeric map.
  if (is.null(x)) {
    return(stats::setNames(numeric(0), character(0)))
  }

  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = TRUE)
  }

  x <- x[!is.na(names(x)) & nzchar(names(x))]
  if (length(x) == 0) {
    return(stats::setNames(numeric(0), character(0)))
  }

  out <- suppressWarnings(as.numeric(x))
  names(out) <- names(x)
  out <- out[is.finite(out)]
  out
}

#' Resolve the `Conjurer` analysis config
#'
#' @param object A [Conjurer] object.
#' @param config Optional config overrides.
#'
#' @return A list.
#'
#' @keywords internal
conjurer_analysis_config <- function(object,
                                     config = NULL) {
  cfg <- merge_cfg(
    object@selector@config,
    merge_cfg(object@config, policy_selector_config_data(config))
  )
  defaults <- policy_selector_similarity_defaults(object@selector@candidates)

  # Pull the selector defaults first so the analysis uses the fitted workflow.
  anchor_cfg <- policy_selector_anchor_config(object@selector, config = cfg)
  anchor_cfg$study_traits <- anchor_cfg$study_traits %||% defaults$study_traits %||% list()
  anchor_cfg$species_traits <- anchor_cfg$species_traits %||% defaults$species_traits %||% list()
  anchor_cfg$alpha <- anchor_cfg$alpha %||% defaults$alpha %||% NULL
  anchor_cfg$k_species <- anchor_cfg$k_species %||% defaults$k_species %||% NULL
  anchor_cfg$k_study <- anchor_cfg$k_study %||% defaults$k_study %||% NULL
  anchor_cfg$length_overlap_weight <- anchor_cfg$length_overlap_weight %||% defaults$length_overlap_weight %||% NULL
  anchor_cfg$depth_overlap_weight <- anchor_cfg$depth_overlap_weight %||% defaults$depth_overlap_weight %||% NULL
  anchor_cfg$frequency_coherence_weight <- anchor_cfg$frequency_coherence_weight %||% defaults$frequency_coherence_weight %||% NULL
  anchor_cfg$frequency_coherence_mode <- anchor_cfg$frequency_coherence_mode %||% defaults$frequency_coherence_mode %||% NULL

  # Normalize the selected study/species trait weight maps once.
  study_weights <- conjurer_weight_map(anchor_cfg$study_traits %||% list())
  species_weights <- conjurer_weight_map(anchor_cfg$species_traits %||% list())

  # Fill the small analysis-specific defaults directly here.
  list(
    study_weights = study_weights,
    species_weights = species_weights,
    alpha = anchor_cfg$alpha,
    k_species = anchor_cfg$k_species,
    k_study = anchor_cfg$k_study,
    anchor_config = anchor_cfg,
    n_draws = suppressWarnings(as.integer(
      policy_selector_config_value(cfg, "n_draws", sections = c("conjurer", "missingness"))
    )) %||% 50L,
    donor_pool_size = suppressWarnings(as.integer(
      policy_selector_config_value(cfg, "donor_pool_size", sections = c("conjurer", "missingness"))
    )) %||% 10L,
    progress = policy_selector_config_value(cfg, "progress", sections = c("conjurer", "missingness")) %||% FALSE,
    seed = suppressWarnings(as.integer(
      policy_selector_config_value(cfg, "seed", sections = c("conjurer", "missingness"))
    )) %||% NULL,
    disable_missingness_gate = isTRUE(
      policy_selector_config_value(cfg, "disable_missingness_gate", sections = c("conjurer", "missingness")) %||% TRUE
    ),
    rebuild_geometry = {
      rebuild_value <- policy_selector_config_value(
        cfg, "rebuild_geometry",
        sections = c("conjurer", "missingness")
      )
      if (is.null(rebuild_value) || length(rebuild_value) == 0) {
        length(object@selector@candidates@ordination) > 0
      } else {
        isTRUE(rebuild_value)
      }
    }
  )
}

#' Resolve the study traits analyzed by `Conjurer`
#'
#' @param object A [Conjurer] object.
#' @param traits Optional trait override.
#' @param config Optional config override.
#'
#' @return Character vector.
#'
#' @keywords internal
conjurer_trait_columns <- function(object,
                                   traits = NULL,
                                   config = NULL) {
  cfg <- conjurer_analysis_config(object, config)

  # Default to the fitted study-trait set rather than all study metadata.
  if (is.null(traits)) {
    traits <- names(cfg$study_weights)
  }

  traits <- unique(as.character(unlist(traits, use.names = FALSE)))
  traits <- traits[!is.na(traits) & nzchar(traits)]
  intersect(traits, names(object@selector@candidates@candidate_models))
}

#' Disable only the missingness gate in one config bundle
#'
#' @param cfg Config list.
#' @param disable_missingness_gate Logical scalar.
#'
#' @return Config list.
#'
#' @keywords internal
conjurer_draw_config <- function(cfg,
                                 disable_missingness_gate = TRUE) {
  # Preserve every operational rule except the missingness admissibility gate.
  if (!isTRUE(disable_missingness_gate)) {
    return(cfg)
  }

  merge_cfg(
    cfg,
    list(
      missing_key_metadata_max_fraction = Inf,
      policy = list(missing_key_metadata_max_fraction = Inf),
      benchmark = list(missing_key_metadata_max_fraction = Inf)
    )
  )
}

#' Refresh reference anchors from one imputed candidate-model table
#'
#' @param candidates Base [Candidates] object.
#' @param candidate_models Updated candidate-model table.
#'
#' @return Reference-anchor tibble.
#'
#' @keywords internal
conjurer_refresh_reference_anchors <- function(candidates,
                                               candidate_models) {
  # Update any anchor rows that were also present in the imputed model table.
  anchors_tbl <- tibble::as_tibble(candidates@reference_anchors)
  models_tbl <- tibble::as_tibble(candidate_models)

  if (nrow(anchors_tbl) == 0) {
    return(anchors_tbl)
  }

  join_col <- if ("model_id_chr" %in% names(anchors_tbl) && "model_id_chr" %in% names(models_tbl)) {
    "model_id_chr"
  } else if ("model_id" %in% names(anchors_tbl) && "model_id" %in% names(models_tbl)) {
    "model_id"
  } else {
    stop("Reference anchors could not be matched back to candidate models.", call. = FALSE)
  }

  anchors_tbl |>
    dplyr::select(dplyr::all_of(join_col)) |>
    dplyr::left_join(models_tbl, by = join_col)
}

#' Rebuild a `Candidates` object with updated model rows
#'
#' @param candidates Base [Candidates] object.
#' @param candidate_models Updated candidate-model table.
#' @param reference_anchors Updated reference-anchor table.
#'
#' @return A [Candidates] object.
#'
#' @keywords internal
conjurer_rebuild_candidates <- function(candidates,
                                        candidate_models,
                                        reference_anchors) {
  # Drop cached downstream layers so each draw starts from the imputed rows.
  Candidates(
    spec = candidates@spec,
    study_db = candidates@study_db,
    species_vector = candidates@species_vector,
    source_dbs = candidates@source_dbs,
    species_db = candidates@species_db,
    candidate_models = tibble::as_tibble(candidate_models),
    reference_anchors = tibble::as_tibble(reference_anchors),
    similarity_matrix = list(),
    gower_distances = list(),
    ordination = list(),
    admissibility = list(),
    similarity_tuning = candidates@similarity_tuning
  )
}

#' Optionally rebuild similarity geometry for one draw
#'
#' @param candidates Base [Candidates] object.
#' @param candidate_models Updated candidate-model table.
#' @param reference_anchors Updated anchor table.
#' @param cfg Analysis config list.
#' @param registry_path Optional trait-registry path.
#'
#' @return A [Candidates] object.
#'
#' @keywords internal
conjurer_prepare_draw_candidates <- function(candidates,
                                             candidate_models,
                                             reference_anchors,
                                             cfg,
                                             registry_path = NULL) {
  # Rebuild the staged candidate object before any optional geometry refresh.
  out <- conjurer_rebuild_candidates(
    candidates = candidates,
    candidate_models = candidate_models,
    reference_anchors = reference_anchors
  )

  # Stop early when the caller explicitly wants a fixed upstream geometry.
  if (!isTRUE(cfg$rebuild_geometry)) {
    return(out)
  }

  # Recompute the similarity state from the fitted selector settings.
  out <- prepare_similarity_matrix(
    candidate_models = out,
    species_traits = as.list(cfg$species_weights),
    study_traits = as.list(cfg$study_weights),
    alpha = cfg$alpha,
    k_species = cfg$k_species,
    k_study = cfg$k_study,
    config = cfg$anchor_config,
    registry_path = registry_path,
    seed = cfg$seed
  )

  # Recompute the distance bundle used by admissibility screening.
  out <- build_gower_distances(out)

  # Recompute ordination only when the base selector already had it.
  if (length(candidates@ordination) > 0) {
    out <- run_ordination(out)
  }

  out
}

#' Build one predictor wrapper for imputed draws
#'
#' @param selector A [PolicySelector] object.
#' Build one imputation distance matrix for a target trait
#'
#' @param object A [Conjurer] object.
#' @param models_tbl Candidate-model table.
#' @param trait_name Target study trait.
#' @param cfg Analysis config list.
#' @param registry_path Optional trait-registry path.
#'
#' @return Numeric matrix or `NULL`.
#'
#' @keywords internal
conjurer_trait_distance_matrix <- function(object,
                                           models_tbl,
                                           trait_name,
                                           cfg,
                                           registry_path = NULL) {
  # Drop the target trait from the study-weight map before building distances.
  study_weights_now <- cfg$study_weights
  study_weights_now <- study_weights_now[names(study_weights_now) != trait_name]
  study_weights_now <- study_weights_now[is.finite(study_weights_now) & study_weights_now > 0]
  species_weights_now <- cfg$species_weights[is.finite(cfg$species_weights) & cfg$species_weights > 0]

  # Fall back to a global donor pool when no context traits remain.
  if (length(study_weights_now) + length(species_weights_now) == 0) {
    return(NULL)
  }

  # Build one context-specific similarity bundle for the imputation donor map.
  sim_obj <- prepare_similarity_matrix(
    candidate_models = models_tbl,
    species_traits = as.list(species_weights_now),
    study_traits = as.list(study_weights_now),
    alpha = cfg$alpha,
    k_species = cfg$k_species,
    k_study = cfg$k_study,
    config = cfg$anchor_config,
    registry_path = registry_path,
    seed = cfg$seed
  )
  dist_obj <- build_gower_distances(sim_obj)
  dist_obj$combined_dist
}

#' Sample one donor value for each missing row of a target trait
#'
#' @param models_tbl Candidate-model table.
#' @param trait_name Target study trait.
#' @param dist_mat Optional numeric distance matrix.
#' @param donor_pool_size Maximum donor-pool size per missing row.
#' @param seed_now Integer seed.
#'
#' @return Imputed candidate-model tibble.
#'
#' @keywords internal
conjurer_impute_trait_draw <- function(models_tbl,
                                       trait_name,
                                       dist_mat = NULL,
                                       donor_pool_size = 10L,
                                       seed_now = NULL) {
  # Work on a tibble copy so each draw mutates only one local table.
  out <- tibble::as_tibble(models_tbl)
  if (!trait_name %in% names(out)) {
    return(out)
  }

  target_values <- out[[trait_name]]
  missing_idx <- which(is.na(target_values))
  observed_idx <- which(!is.na(target_values))

  # Leave the table unchanged when the trait is fully observed or fully missing.
  if (length(missing_idx) == 0 || length(observed_idx) == 0) {
    return(out)
  }

  # Fix the random stream once per draw so the donor sampling is reproducible.
  if (is.null(seed_now)) {
    seed_now <- sample.int(.Machine$integer.max, 1)
  }
  set.seed(seed_now)

  for (row_idx in missing_idx) {
    donor_idx <- observed_idx
    donor_prob <- rep(1, length(donor_idx))

    # Restrict donor sampling to the nearest observed rows when distances exist.
    if (is.matrix(dist_mat) &&
      nrow(dist_mat) >= row_idx &&
      ncol(dist_mat) >= max(observed_idx)) {
      donor_dist <- suppressWarnings(as.numeric(dist_mat[row_idx, observed_idx]))
      keep <- is.finite(donor_dist)
      if (any(keep)) {
        donor_idx <- observed_idx[keep]
        donor_dist <- pmax(donor_dist[keep], 0)
        ord <- order(donor_dist)
        donor_idx <- donor_idx[ord]
        donor_dist <- donor_dist[ord]
        donor_take <- min(length(donor_idx), max(1L, donor_pool_size))
        donor_idx <- donor_idx[seq_len(donor_take)]
        donor_dist <- donor_dist[seq_len(donor_take)]
        dist_scale <- stats::median(donor_dist[donor_dist > 0], na.rm = TRUE)
        if (!is.finite(dist_scale) || dist_scale <= 0) {
          dist_scale <- 1
        }
        donor_prob <- exp(-donor_dist / dist_scale)
      }
    }

    # Fall back to uniform donor sampling when the distance weights collapse.
    if (!all(is.finite(donor_prob)) || sum(donor_prob) <= 0) {
      donor_prob <- rep(1, length(donor_idx))
    }

    draw_idx <- sample.int(length(donor_idx), size = 1L, prob = donor_prob)
    out[[trait_name]][row_idx] <- out[[trait_name]][donor_idx[[draw_idx]]]
  }

  out
}

#' Build one trait-level draw result bundle
#'
#' @param object A [Conjurer] object.
#' @param trait_name Target study trait.
#' @param n_draws Number of stochastic imputations.
#' @param config Optional config override.
#' @param seed Integer seed.
#' @param progress Logical scalar.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path.
#'
#' @return A named list.
#'
#' @keywords internal
conjurer_trait_draw_results <- function(object,
                                        trait_name,
                                        n_draws,
                                        config = NULL,
                                        seed = NULL,
                                        progress = FALSE,
                                        registry_path = NULL,
                                        policy_path = NULL) {
  # Resolve the merged analysis settings once for this trait.
  cfg <- conjurer_analysis_config(object, config)
  if (is.null(seed)) {
    seed <- cfg$seed %||% sample.int(.Machine$integer.max, 1)
  }
  models_tbl <- tibble::as_tibble(object@selector@candidates@candidate_models)
  if (!trait_name %in% names(models_tbl)) {
    return(list(
      selected = tibble::tibble(),
      consensus = tibble::tibble(),
      manifest = tibble::tibble(
        trait = trait_name,
        n_missing = NA_integer_,
        n_observed = NA_integer_,
        n_draws = 0L,
        status = "trait_missing_from_models"
      )
    ))
  }

  # Count the observed and missing rows before any donor sampling begins.
  n_missing <- sum(is.na(models_tbl[[trait_name]]))
  n_observed <- sum(!is.na(models_tbl[[trait_name]]))
  if (n_missing == 0L) {
    return(list(
      selected = tibble::tibble(),
      consensus = tibble::tibble(),
      manifest = tibble::tibble(
        trait = trait_name,
        n_missing = n_missing,
        n_observed = n_observed,
        n_draws = 0L,
        status = "no_missing_rows"
      )
    ))
  }
  if (n_observed == 0L) {
    return(list(
      selected = tibble::tibble(),
      consensus = tibble::tibble(),
      manifest = tibble::tibble(
        trait = trait_name,
        n_missing = n_missing,
        n_observed = n_observed,
        n_draws = 0L,
        status = "no_observed_donors"
      )
    ))
  }

  # Build the donor-distance map once because it does not change across draws.
  workflow_progress(progress, "Conjurer: building donor map for trait '", trait_name, "'.")
  dist_mat <- conjurer_trait_distance_matrix(
    object = object,
    models_tbl = models_tbl,
    trait_name = trait_name,
    cfg = cfg,
    registry_path = registry_path
  )

  selected_rows <- list()
  consensus_rows <- list()

  for (draw_id in seq_len(n_draws)) {
    # Sample one imputed table for the target trait.
    draw_tbl <- conjurer_impute_trait_draw(
      models_tbl = models_tbl,
      trait_name = trait_name,
      dist_mat = dist_mat,
      donor_pool_size = cfg$donor_pool_size,
      seed_now = seed + draw_id
    )

    # Refresh anchor rows in case any anchor also carried the missing trait.
    anchors_tbl <- conjurer_refresh_reference_anchors(
      candidates = object@selector@candidates,
      candidate_models = draw_tbl
    )

    # Rebuild the staged candidates object for this imputed draw.
    draw_candidates <- conjurer_prepare_draw_candidates(
      candidates = object@selector@candidates,
      candidate_models = draw_tbl,
      reference_anchors = anchors_tbl,
      cfg = cfg,
      registry_path = registry_path
    )

    # Rebuild the selector shell while preserving the fitted benchmark layers.
    draw_selector <- policy_selector_rebuild(
      object@selector,
      candidates = draw_candidates
    )

    # Disable only the missingness gate for the uncertainty-propagation draw.
    draw_predictions <- stats::predict(
      draw_selector,
      learner = object@learner,
      config = conjurer_draw_config(
        cfg = merge_cfg(
          object@selector@config,
          merge_cfg(object@config, policy_selector_config_data(config))
        ),
        disable_missingness_gate = cfg$disable_missingness_gate
      ),
      policy_path = policy_path,
      registry_path = registry_path,
      reuse_admissibility = FALSE
    )

    # Attach the draw metadata to the selected rows and donor consensus rows.
    selected_rows[[length(selected_rows) + 1L]] <- tibble::as_tibble(draw_predictions@selections) |>
      dplyr::mutate(trait = trait_name, draw_id = draw_id, .before = 1)
    consensus_rows[[length(consensus_rows) + 1L]] <- tibble::as_tibble(draw_predictions@consensus) |>
      dplyr::mutate(trait = trait_name, draw_id = draw_id, .before = 1)
  }

  list(
    selected = dplyr::bind_rows(selected_rows),
    consensus = dplyr::bind_rows(consensus_rows),
    manifest = tibble::tibble(
      trait = trait_name,
      n_missing = n_missing,
      n_observed = n_observed,
      n_draws = n_draws,
      status = "ok"
    )
  )
}

#' Compute one normalized selection entropy
#'
#' @param x Character vector of selected policies.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
conjurer_selection_entropy <- function(x) {
  # Convert the empirical policy frequencies to a bounded entropy score.
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }

  p <- as.numeric(table(x)) / length(x)
  -sum(p * log(p))
}

#' Resolve the modal selected policy
#'
#' @param x Character vector of selected policies.
#'
#' @return Character scalar.
#'
#' @keywords internal
conjurer_modal_policy <- function(x) {
  # Return the most frequent non-missing policy label for one anchor-trait set.
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }

  names(sort(table(x), decreasing = TRUE))[[1]]
}

#' Convert multiplier shifts to dB shifts
#'
#' @param multiplier_pred Numeric vector of imputed recommended multipliers.
#' @param baseline_multiplier Numeric vector of baseline recommended multipliers.
#'
#' @return Numeric vector.
#'
#' @keywords internal
conjurer_db_shift <- function(multiplier_pred,
                              baseline_multiplier) {
  # Require positive baseline and draw multipliers before taking log ratios.
  out <- rep(NA_real_, length(multiplier_pred))
  keep <- is.finite(multiplier_pred) &
    is.finite(baseline_multiplier) &
    multiplier_pred > 0 &
    baseline_multiplier > 0
  out[keep] <- 10 * log10(multiplier_pred[keep] / baseline_multiplier[keep])
  out
}

#' Summarize one set of conjurer draws
#'
#' @param selected_draws Draw-level selected-policy table.
#' @param consensus_draws Draw-level donor-consensus table.
#' @param baseline_selected Baseline selected-policy table.
#'
#' @return A tibble.
#'
#' @keywords internal
conjurer_summarize_draws <- function(selected_draws,
                                     consensus_draws,
                                     baseline_selected) {
  selected_draws <- tibble::as_tibble(selected_draws)
  consensus_draws <- tibble::as_tibble(consensus_draws)
  baseline_tbl <- tibble::as_tibble(baseline_selected) |>
    dplyr::transmute(
      anchor_model_id,
      anchor_species,
      baseline_selected_policy = dplyr::coalesce(selected_policy_display, selected_policy),
      baseline_multiplier_pred = multiplier_pred
    )

  # Summarize policy switching and multiplier spread on the selected rows.
  selected_summary <- selected_draws |>
    dplyr::mutate(
      selected_policy_label = dplyr::coalesce(selected_policy_display, selected_policy)
    ) |>
    dplyr::left_join(baseline_tbl, by = c("anchor_model_id", "anchor_species")) |>
    dplyr::mutate(
      db_shift_from_baseline = conjurer_db_shift(multiplier_pred, baseline_multiplier_pred)
    ) |>
    dplyr::group_by(trait, anchor_model_id, anchor_species) |>
    dplyr::summarise(
      n_draws = dplyr::n(),
      baseline_multiplier_pred = dplyr::first(baseline_multiplier_pred),
      modal_selected_policy = conjurer_modal_policy(selected_policy_label),
      modal_selected_policy_prob = {
        policy_vals <- selected_policy_label[!is.na(selected_policy_label) & nzchar(selected_policy_label)]
        if (length(policy_vals) == 0) NA_real_ else max(as.numeric(table(policy_vals)) / length(policy_vals))
      },
      selection_entropy = conjurer_selection_entropy(selected_policy_label),
      mean_multiplier_pred = mean(multiplier_pred, na.rm = TRUE),
      sd_log_multiplier = stats::sd(log(multiplier_pred[multiplier_pred > 0]), na.rm = TRUE),
      mean_db_shift = mean(db_shift_from_baseline, na.rm = TRUE),
      mean_abs_db_shift = mean(abs(db_shift_from_baseline), na.rm = TRUE),
      sd_db_shift = stats::sd(db_shift_from_baseline, na.rm = TRUE),
      q05_db_shift = stats::quantile(db_shift_from_baseline, probs = 0.05, na.rm = TRUE, names = FALSE),
      q50_db_shift = stats::quantile(db_shift_from_baseline, probs = 0.50, na.rm = TRUE, names = FALSE),
      q95_db_shift = stats::quantile(db_shift_from_baseline, probs = 0.95, na.rm = TRUE, names = FALSE),
      q95_abs_db_shift = stats::quantile(abs(db_shift_from_baseline), probs = 0.95, na.rm = TRUE, names = FALSE),
      q05_multiplier_pred = stats::quantile(multiplier_pred, probs = 0.05, na.rm = TRUE, names = FALSE),
      q50_multiplier_pred = stats::quantile(multiplier_pred, probs = 0.50, na.rm = TRUE, names = FALSE),
      q95_multiplier_pred = stats::quantile(multiplier_pred, probs = 0.95, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      baseline_tbl |>
        dplyr::select(anchor_model_id, anchor_species, baseline_selected_policy),
      by = c("anchor_model_id", "anchor_species")
    ) |>
    dplyr::left_join(
      selected_draws |>
        dplyr::mutate(
          selected_policy_label = dplyr::coalesce(selected_policy_display, selected_policy)
        ) |>
        dplyr::left_join(
          baseline_tbl |>
            dplyr::select(anchor_model_id, anchor_species, baseline_selected_policy),
          by = c("anchor_model_id", "anchor_species")
        ) |>
        dplyr::group_by(trait, anchor_model_id, anchor_species) |>
        dplyr::summarise(
          switch_rate_vs_baseline = mean(
            selected_policy_label != baseline_selected_policy,
            na.rm = TRUE
          ),
          .groups = "drop"
        ),
      by = c("trait", "anchor_model_id", "anchor_species")
    )

  # Summarize the admissible donor-pool size and consensus multiplier spread.
  consensus_summary <- consensus_draws |>
    dplyr::group_by(trait, anchor_model_id, anchor_species) |>
    dplyr::summarise(
      mean_n_admissible = mean(n_admissible, na.rm = TRUE),
      sd_n_admissible = stats::sd(n_admissible, na.rm = TRUE),
      mean_consensus_multiplier = mean(consensus_multiplier, na.rm = TRUE),
      sd_log_consensus_multiplier = stats::sd(log(consensus_multiplier[consensus_multiplier > 0]), na.rm = TRUE),
      mean_log_spread = mean(log_spread, na.rm = TRUE),
      .groups = "drop"
    )

  selected_summary |>
    dplyr::left_join(
      consensus_summary,
      by = c("trait", "anchor_model_id", "anchor_species")
    ) |>
    dplyr::arrange(trait, anchor_species, anchor_model_id)
}

#' Run missingness-uncertainty draws from a `Conjurer`
#'
#' @param object A [Conjurer] object.
#' @param traits Optional study-trait override.
#' @param n_draws Optional number of stochastic imputations per trait.
#' @param config Optional config override.
#' @param seed Optional integer seed.
#' @param progress Optional logical scalar.
#' @param registry_path Optional trait-registry path.
#' @param policy_path Optional policy-registry path.
#'
#' @return An updated [Conjurer] object.
#' @name simulate.Conjurer
S7::method(simulate_generic, Conjurer) <- function(object,
                                                   traits = NULL,
                                                   n_draws = NULL,
                                                   config = NULL,
                                                   seed = NULL,
                                                   progress = NULL,
                                                   registry_path = NULL,
                                                   policy_path = NULL) {
  cfg <- conjurer_analysis_config(object, config)
  traits <- conjurer_trait_columns(object, traits = traits, config = config)
  n_draws <- n_draws %||% cfg$n_draws
  seed <- seed %||% cfg$seed
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }
  progress <- progress %||% cfg$progress

  # Validate the small analysis controls up front.
  if (!is.numeric(n_draws) || length(n_draws) != 1 || !is.finite(n_draws) || n_draws < 1) {
    stop("'n_draws' must be one finite number >= 1.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed)) {
    stop("'seed' must be one finite number.", call. = FALSE)
  }
  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("'progress' must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(traits) == 0) {
    stop("No study traits were available for conjurer analysis.", call. = FALSE)
  }

  # Build the operational baseline prediction once for later switch-rate totals.
  workflow_progress(progress, "Conjurer: building baseline predictions.")
  baseline_predictions <- stats::predict(object@selector, learner = object@learner)

  trait_results <- list()
  manifest_rows <- list()
  selected_draws <- list()
  consensus_draws <- list()

  for (trait_idx in seq_along(traits)) {
    trait_name <- traits[[trait_idx]]
    workflow_progress(
      progress,
      "Conjurer: running trait ", trait_idx, "/", length(traits),
      " (", trait_name, ")."
    )

    trait_seed <- as.integer(seed + trait_idx * 1000L)
    one_result <- conjurer_trait_draw_results(
      object = object,
      trait_name = trait_name,
      n_draws = as.integer(n_draws),
      config = config,
      seed = trait_seed,
      progress = progress,
      registry_path = registry_path,
      policy_path = policy_path
    )

    trait_results[[trait_name]] <- one_result
    manifest_rows[[length(manifest_rows) + 1L]] <- one_result$manifest
    selected_draws[[length(selected_draws) + 1L]] <- one_result$selected
    consensus_draws[[length(consensus_draws) + 1L]] <- one_result$consensus
  }

  draw_tbl <- dplyr::bind_rows(selected_draws)
  consensus_tbl <- dplyr::bind_rows(consensus_draws)
  summary_tbl <- if (nrow(draw_tbl) > 0) {
    conjurer_summarize_draws(
      selected_draws = draw_tbl,
      consensus_draws = consensus_tbl,
      baseline_selected = baseline_predictions@selections
    )
  } else {
    tibble::tibble()
  }

  workflow_progress(progress, "Conjurer: completed missingness uncertainty analysis.")

  conjurer_rebuild(
    object,
    results = list(
      baseline = baseline_predictions,
      by_trait = trait_results,
      consensus_draws = consensus_tbl
    ),
    manifest = dplyr::bind_rows(manifest_rows),
    draws = draw_tbl,
    summary = summary_tbl
  )
}

#' Resolve one top `Conjurer` signal for display
#'
#' @param summary_tbl Summary tibble.
#' @param metric Metric column name.
#' @param digits Number of digits to print.
#' @param percent Logical scalar indicating percentage formatting.
#'
#' @return Character scalar.
#'
#' @keywords internal
conjurer_top_signal <- function(summary_tbl,
                                metric,
                                digits = 2,
                                percent = FALSE) {
  # Surface the single largest finite anchor-trait signal in one readable line.
  summary_tbl <- tibble::as_tibble(summary_tbl)
  if (nrow(summary_tbl) == 0 || !all(c("anchor_species", "trait", metric) %in% names(summary_tbl))) {
    return("none")
  }

  top_row <- summary_tbl |>
    dplyr::filter(is.finite(.data[[metric]])) |>
    dplyr::arrange(dplyr::desc(.data[[metric]]), anchor_species, trait) |>
    dplyr::slice_head(n = 1)

  if (nrow(top_row) == 0) {
    return("none")
  }

  value_now <- top_row[[metric]][[1]]
  value_lab <- if (isTRUE(percent)) {
    paste0(round(100 * value_now, digits = 0), "%")
  } else {
    format(round(value_now, digits), nsmall = digits, trim = TRUE)
  }
  trait_lab <- stringr::str_replace_all(top_row$trait[[1]], "_", " ")

  paste0(top_row$anchor_species[[1]], " / ", trait_lab, " = ", value_lab)
}

#' Print a `Conjurer`
#'
#' @name print.Conjurer
#'
#' @param x A [Conjurer] object.
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
S7::method(print_generic, Conjurer) <- function(x, ...) {
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
      status_tbl <- manifest_tbl |>
        dplyr::count(status, name = "n_traits") |>
        dplyr::arrange(dplyr::desc(n_traits), status)
      paste0(status_tbl$status, "=", status_tbl$n_traits, collapse = ", ")
    }
  }

  cat("Conjurer\n")
  cat("  stage: ", stage_label, "\n", sep = "")
  cat("  reference_species: ", preview_values(reference_species), "\n", sep = "")
  cat("  reference_species_n: ", length(reference_species), "\n", sep = "")
  cat("  analyzed_traits: ", preview_values(trait_values), "\n", sep = "")
  cat("  draws_per_trait: ", if (is.finite(configured_draws)) configured_draws else "unknown", "\n", sep = "")
  cat("  trait_rows: ", nrow(x@manifest), "\n", sep = "")
  cat("  trait_status: ", trait_status, "\n", sep = "")
  cat("  selected_draw_rows: ", nrow(x@draws), "\n", sep = "")
  cat("  anchor_trait_rows: ", nrow(x@summary), "\n", sep = "")
  cat("  largest_mean_abs_db_shift: ", conjurer_top_signal(x@summary, "mean_abs_db_shift", digits = 2), "\n", sep = "")
  cat("  largest_policy_switch_rate: ", conjurer_top_signal(x@summary, "switch_rate_vs_baseline", percent = TRUE), "\n", sep = "")
  invisible(x)
}

#' Show a `Conjurer`
#'
#' @name show.Conjurer
#'
#' @param object A [Conjurer] object.
#'
#' @return Invisibly returns `object`.
#'
#' @keywords internal
S7::method(show_generic, Conjurer) <- function(object) {
  # Use the compact custom summary rather than the raw S7 object structure.
  print(object)
  invisible(object)
}


