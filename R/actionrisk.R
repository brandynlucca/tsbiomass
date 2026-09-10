#' Audited action-risk feature contract
#'
#' Defines the metadata-derived target-action quantities that are eligible when
#' a candidate action is scored. Donor/target coefficients, coefficient-derived
#' heterogeneity, realized multiplier errors, policy names, slope-branch labels,
#' and post-selection quantities are deliberately absent. Slope branch is used
#' only while constructing a policy's eligible action set. It must not change
#' the learned risk assigned to an otherwise identical realized action.
#'
#' @return A tibble describing the predeclared action-risk features.
#' @keywords internal
#' @noRd
action_risk_feature_contract <- function(action_outcomes = NULL) {
  base_contract <- tibble::tribble(
    ~feature, ~component, ~interpretation,
    "n_donors", "support", "number of donor rows in the action footprint",
    "effective_donor_count", "support", "inverse weight concentration of the action",
    "max_donor_weight", "support", "largest normalized donor weight",
    "donor_weight_hhi", "support", "Herfindahl concentration of donor weights",
    "min_combined_distance", "distance", "minimum learned distance across action donors",
    "weighted_mean_combined_distance", "distance", "action-weighted learned distance",
    "max_combined_distance", "distance", "maximum learned distance across action donors",
    "min_species_distance", "distance", "minimum configured species-component distance",
    "weighted_mean_species_distance", "distance", "action-weighted species-component distance",
    "max_species_distance", "distance", "maximum configured species-component distance",
    "min_survey_distance", "distance", "minimum configured survey-component distance",
    "weighted_mean_survey_distance", "distance", "action-weighted survey-component distance",
    "max_survey_distance", "distance", "maximum configured survey-component distance",
    "min_taxonomic_distance", "distance", "minimum phylogenetic distance when configured and available",
    "weighted_mean_taxonomic_distance", "distance", "action-weighted phylogenetic distance",
    "max_taxonomic_distance", "distance", "maximum phylogenetic distance",
    "min_frequency_coherence_distance", "coherence", "minimum configured frequency-coherence distance",
    "weighted_mean_frequency_coherence_distance", "coherence", "action-weighted frequency-coherence distance",
    "max_frequency_coherence_distance", "coherence", "maximum frequency-coherence distance",
    "min_length_coherence_distance", "coherence", "minimum configured length-coherence distance",
    "weighted_mean_length_coherence_distance", "coherence", "action-weighted length-coherence distance",
    "max_length_coherence_distance", "coherence", "maximum length-coherence distance",
    "min_depth_coherence_distance", "coherence", "minimum configured depth-coherence distance",
    "weighted_mean_depth_coherence_distance", "coherence", "action-weighted depth-coherence distance",
    "max_depth_coherence_distance", "coherence", "maximum depth-coherence distance",
    "min_learned_distance_disagreement", "distance", "minimum base-learner disagreement in learned distance",
    "weighted_mean_learned_distance_disagreement", "distance", "action-weighted learned-distance disagreement",
    "max_learned_distance_disagreement", "distance", "maximum learned-distance disagreement"
  )
  base_contract$default_predictor <- TRUE
  if (is.null(action_outcomes)) return(base_contract)
  action_outcomes <- tibble::as_tibble(action_outcomes)
  component_specs <- list(
    candidate_pools = list(prefix = "action_scope__", component = "policy_scope"),
    aggregation_methods = list(prefix = "action_aggregation__", component = "aggregation")
  )
  dynamic <- lapply(names(component_specs), function(source_col) {
    if (!source_col %in% names(action_outcomes)) return(tibble::tibble())
    values <- as.character(action_outcomes[[source_col]])
    tokens <- sort(unique(trimws(unlist(strsplit(values[!is.na(values)], ";", fixed = TRUE)))))
    tokens <- tokens[nzchar(tokens)]
    safe <- gsub("[^a-z0-9]+", "_", tolower(tokens))
    if (anyDuplicated(safe)) {
      stop(sprintf("Configured action-component labels in '%s' do not have unique safe names.", source_col), call. = FALSE)
    }
    tibble::tibble(
      feature = paste0(component_specs[[source_col]]$prefix, safe),
      component = component_specs[[source_col]]$component,
      interpretation = paste("configured action component:", tokens),
      default_predictor = FALSE,
      source_column = source_col,
      source_token = tokens
    )
  }) |>
    dplyr::bind_rows()
  target_context <- grep("^target_context__", names(action_outcomes), value = TRUE)
  if (length(target_context) > 0L) {
    dynamic <- dplyr::bind_rows(
      dynamic,
      tibble::tibble(
        feature = target_context,
        component = "target_context",
        interpretation = paste(
          "configured target metadata:",
          sub("^target_context__", "", target_context)
        ),
        default_predictor = TRUE,
        source_column = NA_character_,
        source_token = NA_character_
      )
    )
  }
  base_contract$source_column <- NA_character_
  base_contract$source_token <- NA_character_
  dplyr::bind_rows(base_contract, dynamic)
}

#' Resolve configured target-context fields for action-risk fitting
#'
#' Trait identities come from configuration. Length, depth, and frequency
#' fields are included only when the corresponding coherence dimension is
#' configured, using the configured study/species source. These are absolute
#' target descriptors; no target TS coefficients or realized outcomes enter.
#'
#' @keywords internal
#' @noRd
action_risk_target_context_columns <- function(config,
                                                candidate_models) {
  candidate_models <- tibble::as_tibble(candidate_models)
  config <- if (is_s7_instance(config, "Configurer")) config@data else config
  species_traits <- names(config$species_traits %||% numeric(0))
  study_traits <- names(config$study_traits %||% numeric(0))
  coherence <- config$coherence %||% list()
  context <- unique(c(species_traits, study_traits))
  add_interval <- function(dimension, study_prefix, species_prefix) {
    specification <- coherence[[dimension]] %||% NULL
    if (is.null(specification)) return(character(0))
    source <- tolower(as.character(specification$source %||% "study")[[1]])
    out <- character(0)
    if (source %in% c("study", "both")) {
      out <- c(out, paste0(study_prefix, c("_min", "_max")))
    }
    if (source %in% c("species", "both")) {
      out <- c(out, paste0(species_prefix, c("_min", "_max")))
    }
    out
  }
  context <- c(
    context,
    add_interval("length", "study_length", "species_length"),
    add_interval("depth", "study_depth", "species_depth")
  )
  if (!is.null(coherence$frequency)) context <- c(context, "frequency")
  context <- unique(context)
  absent <- setdiff(context, names(candidate_models))
  if (length(absent) > 0L) {
    stop(
      sprintf(
        "Configured target-context field(s) are absent from candidate metadata: %s",
        paste(absent, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  context
}

#' Attach configured target context to counterfactual action rows
#'
#' @keywords internal
#' @noRd
augment_action_risk_target_context <- function(action_outcomes,
                                                candidate_models,
                                                context_cols) {
  action_outcomes <- tibble::as_tibble(action_outcomes)
  candidate_models <- tibble::as_tibble(candidate_models)
  context_cols <- unique(as.character(context_cols))
  forbidden <- c(
    "slope", "intercept", "slope_len", "intercept_len",
    "slope_standard", "intercept_standard"
  )
  if (length(intersect(context_cols, forbidden)) > 0L) {
    stop("Target TS coefficients are forbidden action-risk context features.", call. = FALSE)
  }
  if (!all(c("anchor_model_id") %in% names(action_outcomes)) ||
      !"model_id" %in% names(candidate_models)) {
    stop("Target-context augmentation requires action anchor IDs and candidate model IDs.", call. = FALSE)
  }
  absent <- setdiff(context_cols, names(candidate_models))
  if (length(absent) > 0L) {
    stop(
      sprintf("Target-context field(s) are absent: %s", paste(absent, collapse = ", ")),
      call. = FALSE
    )
  }
  ids <- as.character(candidate_models$model_id)
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("Candidate model IDs must be complete and unique for target-context augmentation.", call. = FALSE)
  }
  idx <- match(as.character(action_outcomes$anchor_model_id), ids)
  if (anyNA(idx)) {
    stop("Every action-risk anchor must have one target-context metadata row.", call. = FALSE)
  }
  for (column in context_cols) {
    values <- candidate_models[[column]]
    # Match the Alchemist trait semantics exactly: the configured `species`
    # trait is the full binomial, never the specific epithet by itself. This
    # prevents unrelated taxa such as Scomber japonicus and Trachurus
    # japonicus (or Engraulis mordax and Osmerus mordax) from sharing a target
    # context merely because their epithets match.
    if (identical(column, "species") && "genus" %in% names(candidate_models)) {
      genus <- trimws(as.character(candidate_models$genus))
      epithet <- trimws(as.character(values))
      values <- paste(genus, epithet)
      values[is.na(genus) | genus == "NA" | !nzchar(genus) |
               is.na(epithet) | epithet == "NA" | !nzchar(epithet)] <- NA_character_
    }
    action_outcomes[[paste0("target_context__", column)]] <- values[idx]
  }
  attr(action_outcomes, "action_risk_target_context_cols") <- context_cols
  action_outcomes
}

materialize_action_risk_components <- function(action_outcomes,
                                                contract) {
  action_outcomes <- tibble::as_tibble(action_outcomes)
  dynamic <- contract |>
    dplyr::filter(!is.na(.data$source_column), !is.na(.data$source_token))
  for (i in seq_len(nrow(dynamic))) {
    values <- strsplit(
      as.character(action_outcomes[[dynamic$source_column[[i]]]]),
      ";",
      fixed = TRUE
    )
    token <- dynamic$source_token[[i]]
    action_outcomes[[dynamic$feature[[i]]]] <- vapply(
      values,
      function(x) token %in% trimws(x),
      logical(1)
    )
  }
  action_outcomes
}

#' Prepare the audited action-risk training table
#'
#' Every pseudoanchor contributes total case weight one, irrespective of how
#' many admissible actions it has. This is a sampling correction, not a trait
#' weight: it prevents dense action universes from redefining the estimand.
#'
#' @param action_outcomes Counterfactual action-outcome table.
#' @param feature_cols Optional strict subset of the feature contract.
#'
#' @return Model-ready action-risk table.
#' @keywords internal
#' @noRd
prepare_action_risk_data <- function(action_outcomes,
                                     feature_cols = NULL,
                                     outcome_target = c(
                                       "absolute_loss", "within_anchor_regret",
                                       "expected_member_absolute_loss",
                                       "ensemble_epistemic_risk",
                                       "ensemble_epistemic_regret"
                                     )) {
  outcome_target <- match.arg(outcome_target)
  action_outcomes <- tibble::as_tibble(action_outcomes)
  required <- c(
    "anchor_model_id", "anchor_species", "anchor_study_reference_id",
    "action_id", "selection_loss_abs_log", "outcome_estimable"
  )
  missing_required <- setdiff(required, names(action_outcomes))
  if (length(missing_required) > 0L) {
    stop(
      sprintf("Action outcomes lack required column(s): %s", paste(missing_required, collapse = ", ")),
      call. = FALSE
    )
  }
  if (identical(outcome_target, "expected_member_absolute_loss") &&
      !"expected_member_abs_log_loss" %in% names(action_outcomes)) {
    stop(
      "Expected-member action risk requires 'expected_member_abs_log_loss'.",
      call. = FALSE
    )
  }
  if (outcome_target %in% c(
      "ensemble_epistemic_risk", "ensemble_epistemic_regret"
    ) &&
      !"ensemble_epistemic_risk_abs_log" %in% names(action_outcomes)) {
    stop(
      "Ensemble epistemic action risk requires 'ensemble_epistemic_risk_abs_log'.",
      call. = FALSE
    )
  }
  if (anyNA(action_outcomes$anchor_study_reference_id) ||
      any(!nzchar(as.character(action_outcomes$anchor_study_reference_id)))) {
    stop("Every action outcome must declare 'anchor_study_reference_id'; no grouping fallback is allowed.", call. = FALSE)
  }
  action_outcomes <- action_outcomes |>
    dplyr::filter(.data$outcome_estimable, is.finite(.data$selection_loss_abs_log))
  if (identical(outcome_target, "expected_member_absolute_loss")) {
    action_outcomes <- action_outcomes |>
      dplyr::filter(is.finite(.data$expected_member_abs_log_loss))
  }
  if (outcome_target %in% c(
      "ensemble_epistemic_risk", "ensemble_epistemic_regret"
    )) {
    action_outcomes <- action_outcomes |>
      dplyr::filter(is.finite(.data$ensemble_epistemic_risk_abs_log))
  }
  action_outcomes$.evaluation_loss_input <- if (
    identical(outcome_target, "expected_member_absolute_loss")
  ) {
    as.numeric(action_outcomes$expected_member_abs_log_loss)
  } else if (outcome_target %in% c(
      "ensemble_epistemic_risk", "ensemble_epistemic_regret"
    )) {
    as.numeric(action_outcomes$ensemble_epistemic_risk_abs_log)
  } else {
    as.numeric(action_outcomes$selection_loss_abs_log)
  }
  duplicate_key <- duplicated(action_outcomes[c("anchor_model_id", "action_id")])
  if (any(duplicate_key)) {
    stop("Action-risk training keys must be unique within pseudoanchor.", call. = FALSE)
  }

  contract <- action_risk_feature_contract(action_outcomes)
  action_outcomes <- materialize_action_risk_components(action_outcomes, contract)
  feature_cols <- feature_cols %||% contract$feature[contract$default_predictor %in% TRUE]
  if ("equation_branch_filter" %in% feature_cols) {
    stop(
      paste(
        "Slope branch is construction-only and cannot be an action-risk",
        "predictor."
      ),
      call. = FALSE
    )
  }
  configured_trait_features <- grep(
    "^configured_trait_(min|weighted_mean|max|coverage)__[A-Za-z0-9_.]+$",
    names(action_outcomes),
    value = TRUE
  )
  configured_action_features <- grep(
    "^action_(scope|aggregation)__[a-z0-9_]+$",
    feature_cols,
    value = TRUE
  )
  # A held-out fold may be unable to realize a configured action component
  # that exists in the training contract (for example `same_species` under a
  # cold-start species split). Preserve the fitted one-hot schema explicitly:
  # absence of that component is represented by zero, never by dropping the
  # feature or substituting another scope.
  missing_action_features <- setdiff(
    configured_action_features,
    names(action_outcomes)
  )
  for (feature in missing_action_features) {
    action_outcomes[[feature]] <- 0
  }
  unknown <- setdiff(
    feature_cols,
    c(contract$feature, configured_trait_features, configured_action_features)
  )
  if (length(unknown) > 0L) {
    stop(
      sprintf("Feature(s) are outside the audited action-risk contract: %s", paste(unknown, collapse = ", ")),
      call. = FALSE
    )
  }
  absent <- setdiff(feature_cols, names(action_outcomes))
  if (length(absent) > 0L) {
    stop(
      sprintf("Contract feature(s) are absent from action outcomes: %s", paste(absent, collapse = ", ")),
      call. = FALSE
    )
  }

  action_counts <- action_outcomes |>
    dplyr::count(.data$anchor_model_id, name = ".anchor_action_count")
  out <- action_outcomes |>
    dplyr::left_join(action_counts, by = "anchor_model_id") |>
    dplyr::group_by(.data$anchor_model_id) |>
    dplyr::mutate(
      .anchor_id = as.character(.data$anchor_model_id),
      .split_group = as.character(.data$anchor_study_reference_id),
      .evaluation_loss = as.numeric(.data$.evaluation_loss_input),
      .outcome = if (outcome_target %in% c(
          "within_anchor_regret", "ensemble_epistemic_regret"
        )) {
        .data$.evaluation_loss - min(.data$.evaluation_loss)
      } else {
        .data$.evaluation_loss
      },
      .case_weight = 1 / .data$.anchor_action_count
    ) |>
    dplyr::ungroup()
  out$equation_branch_filter <- factor(out$equation_branch_filter)
  attr(out, "action_risk_feature_cols") <- feature_cols
  attr(out, "action_risk_feature_contract") <- contract
  attr(out, "action_risk_weight_estimand") <- "equal_total_weight_per_pseudoanchor"
  attr(out, "action_risk_outcome_target") <- outcome_target
  out
}

#' Balance provenance groups across folds
#'
#' A provenance group is indivisible. Groups are greedily assigned by their
#' number of distinct pseudoanchors so folds are balanced on the estimand, not
#' on the number of action rows.
#'
#' @param groups Mandatory provenance group for each row.
#' @param unit_ids Pseudoanchor identifier for each row.
#' @param n_folds Requested fold count.
#' @param seed Seed used only to break equal-size group ties.
#'
#' @return Integer fold identifier per row.
#' @keywords internal
#' @noRd
balanced_provenance_foldid <- function(groups,
                                       unit_ids,
                                       n_folds = 5L,
                                       seed = 1L) {
  groups <- as.character(groups)
  unit_ids <- as.character(unit_ids)
  if (length(groups) != length(unit_ids) || length(groups) == 0L) {
    stop("'groups' and 'unit_ids' must be non-empty vectors of equal length.", call. = FALSE)
  }
  if (anyNA(groups) || any(!nzchar(groups)) || anyNA(unit_ids) || any(!nzchar(unit_ids))) {
    stop("Provenance groups and pseudoanchor identifiers must be complete; no fallback is allowed.", call. = FALSE)
  }
  group_unit <- unique(data.frame(group = groups, unit = unit_ids, stringsAsFactors = FALSE))
  group_size <- stats::aggregate(unit ~ group, group_unit, function(x) length(unique(x)))
  names(group_size)[names(group_size) == "unit"] <- "n_units"
  n_folds <- min(as.integer(n_folds), nrow(group_size))
  if (!is.finite(n_folds) || n_folds < 2L) {
    stop("At least two provenance groups are required for grouped cross-fitting.", call. = FALSE)
  }
  set.seed(as.integer(seed))
  group_size$.tie <- stats::runif(nrow(group_size))
  group_size <- group_size[order(-group_size$n_units, group_size$.tie, group_size$group), , drop = FALSE]
  fold_load <- numeric(n_folds)
  fold_groups <- vector("list", n_folds)
  for (i in seq_len(nrow(group_size))) {
    eligible <- which(fold_load == min(fold_load))
    fold_now <- eligible[[1L]]
    fold_groups[[fold_now]] <- c(fold_groups[[fold_now]], group_size$group[[i]])
    fold_load[[fold_now]] <- fold_load[[fold_now]] + group_size$n_units[[i]]
  }
  group_to_fold <- stats::setNames(
    rep(seq_len(n_folds), lengths(fold_groups)),
    unlist(fold_groups, use.names = FALSE)
  )
  unname(group_to_fold[groups])
}

normalize_action_risk_weights <- function(w) {
  w <- as.numeric(w)
  if (any(!is.finite(w)) || any(w <= 0)) {
    stop("Action-risk case weights must be finite and strictly positive.", call. = FALSE)
  }
  w / mean(w)
}

action_risk_matrix <- function(data,
                               feature_cols,
                               blueprint = NULL,
                               terms_obj = NULL,
                               x_columns = NULL) {
  prep <- meta_policy_blueprint(data, feature_cols, blueprint = blueprint)
  mm <- meta_policy_model_matrix(prep$data, terms_obj = terms_obj, x_columns = x_columns)
  list(x = mm$x, blueprint = prep$blueprint, terms = mm$terms)
}

#' Fit one weighted action-risk learner
#'
#' @keywords internal
#' @noRd
fit_action_risk_base <- function(training_data,
                                 method,
                                 feature_cols = NULL,
                                 inner_folds = 4L,
                                 seed = 1L,
                                 monotone_features = character()) {
  feature_cols <- feature_cols %||% attr(training_data, "action_risk_feature_cols")
  training_data <- tibble::as_tibble(training_data)
  feature_cols <- as.character(feature_cols)
  valid_feature <- feature_cols %in% action_risk_feature_contract()$feature |
    grepl("^action_(scope|aggregation)__[a-z0-9_]+$", feature_cols) |
    grepl("^target_context__[A-Za-z0-9_.]+$", feature_cols) |
    grepl("^configured_trait_(min|weighted_mean|max|coverage)__[A-Za-z0-9_.]+$", feature_cols)
  invalid_feature <- unique(feature_cols[!valid_feature])
  if (length(invalid_feature)) {
    stop(
      sprintf(
        "Feature(s) are outside the audited action-risk contract: %s",
        paste(invalid_feature, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  absent_feature <- setdiff(feature_cols, names(training_data))
  if (length(absent_feature)) {
    stop(
      sprintf(
        "Contract feature(s) are absent from action-risk training data: %s",
        paste(absent_feature, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  monotone_features <- unique(as.character(monotone_features))
  unknown_monotone <- setdiff(monotone_features, feature_cols)
  if (length(unknown_monotone)) {
    stop(
      paste0(
        "Monotone action-risk features are outside the fitted feature contract: ",
        paste(unknown_monotone, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
  required <- c(".outcome", ".case_weight", ".split_group", ".anchor_id")
  if (length(feature_cols) == 0L || !all(required %in% names(training_data))) {
    stop("Action-risk fitting requires contract features and prepared outcome, weight, anchor, and group columns.", call. = FALSE)
  }
  method <- match.arg(as.character(method), c(
    "mean", "glm_elastic", "rpart", "mars", "rf", "xgboost",
    "xgboost_quantile", "xgboost_rank"
  ))
  learner_threads <- suppressWarnings(as.integer(getOption("tsbiomass.action_risk_threads", 1L)))
  if (length(learner_threads) != 1L || !is.finite(learner_threads) || learner_threads < 1L) {
    learner_threads <- 1L
  }
  xgboost_rounds <- suppressWarnings(as.integer(getOption("tsbiomass.action_risk_xgboost_rounds", 300L)))
  if (length(xgboost_rounds) != 1L || !is.finite(xgboost_rounds) || xgboost_rounds < 1L) {
    xgboost_rounds <- 300L
  }
  quantile_alpha <- suppressWarnings(as.numeric(getOption(
    "tsbiomass.action_risk_quantile_alpha", 0.75
  )))
  if (length(quantile_alpha) != 1L || !is.finite(quantile_alpha) ||
      quantile_alpha <= 0 || quantile_alpha >= 1) {
    stop("The action-risk quantile alpha must be one finite value strictly between zero and one.", call. = FALSE)
  }
  rf_trees <- suppressWarnings(as.integer(getOption("tsbiomass.action_risk_rf_trees", 500L)))
  if (length(rf_trees) != 1L || !is.finite(rf_trees) || rf_trees < 1L) {
    rf_trees <- 500L
  }
  y <- log1p(as.numeric(training_data$.outcome))
  w <- normalize_action_risk_weights(training_data$.case_weight)
  if (identical(method, "mean")) {
    return(structure(
      list(method = method, intercept = stats::weighted.mean(y, w), feature_cols = feature_cols),
      class = "tsb_action_risk_learner"
    ))
  }

  mm <- action_risk_matrix(training_data, feature_cols)
  variable <- vapply(seq_len(ncol(mm$x)), function(j) {
    x <- mm$x[, j]
    length(unique(x[is.finite(x)])) >= 2L
  }, logical(1))
  x <- mm$x[, variable, drop = FALSE]
  if (ncol(x) == 0L) {
    stop("No variable action-risk model-matrix columns remain.", call. = FALSE)
  }
  common <- list(
    method = method,
    feature_cols = feature_cols,
    monotone_features = monotone_features,
    blueprint = mm$blueprint,
    terms = mm$terms,
    x_columns = colnames(x)
  )

  calibrator <- NULL
  fit <- switch(method,
    glm_elastic = {
      foldid <- balanced_provenance_foldid(
        training_data$.split_group,
        training_data$.anchor_id,
        n_folds = inner_folds,
        seed = seed
      )
      glmnet::cv.glmnet(
        x = x,
        y = y,
        weights = w,
        alpha = 0.25,
        foldid = foldid,
        nfolds = length(unique(foldid)),
        family = "gaussian",
        type.measure = "mae",
        standardize = TRUE
      )
    },
    mars = earth::earth(
      x = x,
      y = y,
      weights = w,
      degree = 1L,
      penalty = 3,
      nk = min(40L, max(21L, ncol(x) + 1L)),
      fast.k = 20L,
      pmethod = "backward",
      trace = 0
    ),
    rpart = rpart::rpart(
      y ~ .,
      data = data.frame(y = y, x, check.names = FALSE),
      weights = w,
      method = "anova",
      control = rpart::rpart.control(
        cp = 0.01,
        minsplit = 20L,
        minbucket = 7L,
        maxdepth = 30L,
        xval = 0L,
        maxcompete = 0L,
        maxsurrogate = 0L
      )
    ),
    rf = ranger::ranger(
      x = as.data.frame(x),
      y = y,
      case.weights = w,
      num.threads = learner_threads,
      verbose = FALSE,
      num.trees = rf_trees,
      min.node.size = 10L,
      sample.fraction = 0.8,
      replace = FALSE,
      seed = as.integer(seed)
    ),
    xgboost = {
      dtrain <- xgboost::xgb.DMatrix(data = x, label = y, weight = w, nthread = learner_threads)
      monotone_constraints <- ifelse(colnames(x) %in% monotone_features, 1L, 0L)
      xgboost_params <- getOption("tsbiomass.action_risk_xgboost_params", list())
      xgboost::xgb.train(
        params = list(
          objective = "reg:squarederror",
          eta = xgboost_params$eta %||% 0.03,
          max_depth = xgboost_params$max_depth %||% 4L,
          min_child_weight = xgboost_params$min_child_weight %||% 10,
          subsample = xgboost_params$subsample %||% 0.8,
          colsample_bytree = xgboost_params$colsample_bytree %||% 0.8,
          lambda = xgboost_params$lambda %||% 2,
          alpha = xgboost_params$alpha %||% 0.1,
          monotone_constraints = paste0(
            "(", paste(monotone_constraints, collapse = ","), ")"
          ),
          nthread = learner_threads,
          seed = as.integer(seed)
        ),
        data = dtrain,
        nrounds = xgboost_rounds,
        verbose = 0L
      )
    },
    xgboost_quantile = {
      dtrain <- xgboost::xgb.DMatrix(data = x, label = y, weight = w, nthread = learner_threads)
      monotone_constraints <- ifelse(colnames(x) %in% monotone_features, 1L, 0L)
      xgboost::xgb.train(
        params = list(
          objective = "reg:quantileerror",
          quantile_alpha = quantile_alpha,
          eta = 0.03,
          max_depth = 4L,
          min_child_weight = 10,
          subsample = 0.8,
          colsample_bytree = 0.8,
          lambda = 2,
          alpha = 0.1,
          monotone_constraints = paste0(
            "(", paste(monotone_constraints, collapse = ","), ")"
          ),
          nthread = learner_threads,
          seed = as.integer(seed)
        ),
        data = dtrain,
        nrounds = xgboost_rounds,
        verbose = 0L
      )
    },
    xgboost_rank = {
      # Ranking groups must be contiguous. Relevance is a within-pseudoanchor
      # reversal of log-risk, so larger relevance means a safer action. Every
      # pseudoanchor receives one ranking-group weight irrespective of action
      # count; there is no trait or species weighting here.
      ord <- order(training_data$.anchor_id, as.character(training_data$action_id))
      anchor_ordered <- training_data$.anchor_id[ord]
      group_sizes <- as.integer(rle(anchor_ordered)$lengths)
      y_ordered <- y[ord]
      relevance <- unlist(
        lapply(split(y_ordered, anchor_ordered), function(z) max(z) - z),
        use.names = FALSE
      )
      # split() sorts group names, whereas `ord` uses the same lexical order;
      # assert the alignment rather than relying on that implementation detail.
      relevance_check <- ave(y_ordered, anchor_ordered, FUN = max) - y_ordered
      if (!isTRUE(all.equal(as.numeric(relevance), as.numeric(relevance_check), tolerance = 0))) {
        stop("Pairwise ranker relevance labels are not aligned with contiguous pseudoanchor groups.", call. = FALSE)
      }
      dtrain <- xgboost::xgb.DMatrix(data = x[ord, , drop = FALSE], label = relevance, nthread = learner_threads)
      xgboost::setinfo(dtrain, "group", group_sizes)
      xgboost::setinfo(dtrain, "weight", rep(1, length(group_sizes)))
      rank_fit <- xgboost::xgb.train(
        params = list(
          objective = "rank:pairwise",
          eta = 0.03,
          max_depth = 4L,
          min_child_weight = 10,
          subsample = 0.8,
          colsample_bytree = 0.8,
          lambda = 2,
          alpha = 0.1,
          nthread = learner_threads,
          seed = as.integer(seed)
        ),
        data = dtrain,
        nrounds = xgboost_rounds,
        verbose = 0L
      )
      training_margin <- as.numeric(stats::predict(rank_fit, x[ord, , drop = FALSE]))
      calibration_fit <- stats::lm.wfit(
        x = cbind(`(Intercept)` = 1, ranking_margin = training_margin),
        y = y_ordered,
        w = w[ord]
      )
      calibration_coef <- as.numeric(calibration_fit$coefficients)
      if (length(calibration_coef) != 2L || any(!is.finite(calibration_coef)) || calibration_coef[[2L]] >= 0) {
        stop("Pairwise ranking margin could not be monotonically calibrated to action risk.", call. = FALSE)
      }
      calibrator <- stats::setNames(calibration_coef, c("intercept", "slope"))
      rank_fit
    }
  )
  structure(c(common, list(fit = fit, calibrator = calibrator)), class = "tsb_action_risk_learner")
}

#' Predict natural-scale action risk
#'
#' @keywords internal
#' @noRd
predict_action_risk <- function(object,
                                new_data) {
  if (!inherits(object, "tsb_action_risk_learner")) {
    stop("'object' must be an action-risk learner.", call. = FALSE)
  }
  new_data <- tibble::as_tibble(new_data)
  if (identical(object$method, "mean")) {
    return(rep(pmax(0, expm1(object$intercept)), nrow(new_data)))
  }
  mm <- action_risk_matrix(
    new_data,
    object$feature_cols,
    blueprint = object$blueprint,
    terms_obj = object$terms,
    x_columns = object$x_columns
  )
  eta <- switch(object$method,
    glm_elastic = as.numeric(stats::predict(object$fit, newx = mm$x, s = "lambda.1se")),
    rpart = as.numeric(stats::predict(object$fit, newdata = data.frame(mm$x, check.names = FALSE))),
    mars = as.numeric(stats::predict(object$fit, newdata = mm$x)),
    rf = as.numeric(stats::predict(object$fit, data = as.data.frame(mm$x))$predictions),
    xgboost = as.numeric(stats::predict(object$fit, mm$x)),
    xgboost_quantile = as.numeric(stats::predict(object$fit, mm$x)),
    xgboost_rank = {
      margin <- as.numeric(stats::predict(object$fit, mm$x))
      object$calibrator[["intercept"]] + object$calibrator[["slope"]] * margin
    }
  )
  # Risk is nonnegative by definition. There is deliberately no empirical
  # upper cap, clipping quantile, trimming rule, or winsorization.
  pmax(0, expm1(eta))
}

simplex_weight_grid <- function(n_methods,
                                denominator = 20L) {
  n_methods <- as.integer(n_methods)
  denominator <- as.integer(denominator)
  if (n_methods < 1L || denominator < 1L) {
    stop("A simplex grid requires positive dimensions and denominator.", call. = FALSE)
  }
  compose <- function(total, parts) {
    if (parts == 1L) return(matrix(total, nrow = 1L))
    do.call(rbind, lapply(0:total, function(first) {
      cbind(first, compose(total - first, parts - 1L))
    }))
  }
  compose(denominator, n_methods) / denominator
}

action_risk_group_indices <- function(data,
                                      within_branch = FALSE) {
  key <- as.character(data$.anchor_id)
  if (isTRUE(within_branch)) {
    key <- paste(key, as.character(data$equation_branch_filter), sep = "\r")
  }
  split(seq_len(nrow(data)), key)
}

action_risk_evaluation_loss <- function(data) {
  if (".evaluation_loss" %in% names(data)) {
    as.numeric(data$.evaluation_loss)
  } else {
    as.numeric(data$.outcome)
  }
}

#' Resolve a competitive action set by an explicit lexicographic burden tuple
#'
#' There are no default burden fields, missing-value substitutions, numerical
#' tolerances, or class preferences. The caller must name every field and
#' direction. An invariant action identifier resolves only an exact remaining
#' tie after all declared burden fields.
#'
#' @keywords internal
#' @noRd
resolve_action_burden <- function(actions,
                                  burden_fields,
                                  directions,
                                  action_id_col = "action_id") {
  actions <- tibble::as_tibble(actions)
  burden_fields <- as.character(burden_fields)
  directions <- as.character(directions)
  if (nrow(actions) < 1L || length(burden_fields) < 1L ||
      length(directions) != length(burden_fields)) {
    stop("Burden resolution requires actions and equally sized explicit fields/directions.", call. = FALSE)
  }
  if (any(!directions %in% c("min", "max"))) {
    stop("Every burden direction must be explicitly 'min' or 'max'.", call. = FALSE)
  }
  required <- c(action_id_col, burden_fields)
  absent <- setdiff(required, names(actions))
  if (length(absent) > 0L) {
    stop(
      sprintf("Competitive actions lack configured burden field(s): %s", paste(absent, collapse = ", ")),
      call. = FALSE
    )
  }
  action_ids <- as.character(actions[[action_id_col]])
  if (anyNA(action_ids) || any(!nzchar(action_ids)) || anyDuplicated(action_ids)) {
    stop("Competitive action identifiers must be complete and unique.", call. = FALSE)
  }
  keys <- lapply(seq_along(burden_fields), function(i) {
    value <- suppressWarnings(as.numeric(actions[[burden_fields[[i]]]]))
    if (any(!is.finite(value))) {
      stop(
        sprintf("Configured burden field '%s' is not finite for every competitive action; no fallback is permitted.", burden_fields[[i]]),
        call. = FALSE
      )
    }
    if (identical(directions[[i]], "max")) -value else value
  })
  ordering <- do.call(order, c(keys, list(action_ids, method = "radix")))
  selected <- ordering[[1L]]
  audit <- tibble::tibble(
    burden_order = seq_along(burden_fields),
    burden_field = burden_fields,
    direction = directions,
    selected_value = vapply(
      burden_fields,
      function(field) as.numeric(actions[[field]][[selected]]),
      numeric(1)
    )
  )
  list(
    selected = actions[selected, , drop = FALSE],
    ordered = actions[ordering, , drop = FALSE],
    audit = audit,
    stable_key_used = nrow(actions) > 1L && all(vapply(keys, function(x) {
      length(unique(x)) == 1L
    }, logical(1)))
  )
}

#' Finite-sample conformal probability for grouped action calibration
#'
#' @keywords internal
#' @noRd
action_conformal_probability <- function(level,
                                         n_exchangeable_groups) {
  level <- suppressWarnings(as.numeric(level)[[1]])
  n_exchangeable_groups <- suppressWarnings(as.integer(n_exchangeable_groups)[[1]])
  if (!is.finite(level) || level <= 0 || level >= 1) {
    stop("'level' must be one finite value strictly between zero and one.", call. = FALSE)
  }
  if (is.na(n_exchangeable_groups) || n_exchangeable_groups < 1L) {
    stop("At least one exchangeable calibration group is required.", call. = FALSE)
  }
  min(
    1,
    ceiling(level * (n_exchangeable_groups + 1L)) /
      n_exchangeable_groups
  )
}

#' Exact weighted upper order statistic
#'
#' Unlike interpolated descriptive quantiles, this helper returns an observed
#' conformity score. Invalid observations are not substituted; an entirely
#' invalid calibration sample is explicitly non-estimable.
#'
#' @keywords internal
#' @noRd
action_weighted_order_quantile <- function(x,
                                           weights,
                                           probability) {
  x <- suppressWarnings(as.numeric(x))
  weights <- suppressWarnings(as.numeric(weights))
  probability <- suppressWarnings(as.numeric(probability)[[1]])
  if (length(x) != length(weights)) {
    stop("Conformity scores and weights must have equal lengths.", call. = FALSE)
  }
  if (!is.finite(probability) || probability < 0 || probability > 1) {
    stop("'probability' must be one finite value in [0, 1].", call. = FALSE)
  }
  keep <- is.finite(x) & is.finite(weights) & weights > 0
  if (!any(keep)) {
    stop("The conformal order statistic is not estimable from the supplied scores.", call. = FALSE)
  }
  x <- x[keep]
  weights <- weights[keep]
  ordering <- order(x, method = "radix")
  x <- x[ordering]
  weights <- weights[ordering] / sum(weights[ordering])
  crossing <- which(cumsum(weights) >= probability)
  if (length(crossing) == 0L) x[[length(x)]] else x[[crossing[[1L]]]]
}

#' Resolve one calibrated competitive action set
#'
#' Membership is determined only by predicted-risk distance from the fitted
#' minimum. The caller-supplied burden tuple resolves the resulting set
#' lexicographically. Predicted risk and invariant action identity are used
#' only after an exact tie on every burden field.
#'
#' @keywords internal
#' @noRd
resolve_action_competitive_set <- function(actions,
                                           radius,
                                           burden_fields,
                                           directions,
                                           predicted_risk_col = "predicted_risk",
                                           action_id_col = "action_id") {
  actions <- tibble::as_tibble(actions)
  radius <- suppressWarnings(as.numeric(radius)[[1]])
  burden_fields <- as.character(burden_fields)
  directions <- as.character(directions)
  if (length(burden_fields) < 1L || length(directions) != length(burden_fields)) {
    stop("Competitive-set resolution requires equally sized explicit burden fields and directions.", call. = FALSE)
  }
  required <- unique(c(
    predicted_risk_col, action_id_col, burden_fields
  ))
  absent <- setdiff(required, names(actions))
  if (length(absent) > 0L) {
    stop(
      sprintf("Action competitive-set input lacks required field(s): %s", paste(absent, collapse = ", ")),
      call. = FALSE
    )
  }
  if (nrow(actions) < 1L || !is.finite(radius) || radius < 0) {
    stop("Competitive-set resolution requires actions and a finite nonnegative radius.", call. = FALSE)
  }
  risk <- suppressWarnings(as.numeric(actions[[predicted_risk_col]]))
  if (any(!is.finite(risk))) {
    stop("Every candidate action must have a finite predicted risk.", call. = FALSE)
  }
  action_ids <- as.character(actions[[action_id_col]])
  if (anyNA(action_ids) || any(!nzchar(action_ids)) || anyDuplicated(action_ids)) {
    stop("Candidate action identifiers must be complete and unique within a target.", call. = FALSE)
  }
  minimum_risk <- min(risk)
  competitive_index <- which((risk - minimum_risk) <= radius)
  if (length(competitive_index) == 0L) {
    stop("The calibrated competitive action set is empty; no fallback is permitted.", call. = FALSE)
  }
  competitive <- actions[competitive_index, , drop = FALSE]
  remaining <- seq_len(nrow(competitive))
  audit <- vector("list", length(burden_fields))
  for (i in seq_along(burden_fields)) {
    field <- burden_fields[[i]]
    direction <- directions[[i]]
    if (!direction %in% c("min", "max")) {
      stop("Every burden direction must be explicitly 'min' or 'max'.", call. = FALSE)
    }
    value <- suppressWarnings(as.numeric(competitive[[field]]))
    if (any(!is.finite(value))) {
      stop(
        sprintf("Configured burden field '%s' is not finite for every competitive action; no fallback is permitted.", field),
        call. = FALSE
      )
    }
    target <- if (identical(direction, "min")) min(value[remaining]) else max(value[remaining])
    remaining <- remaining[value[remaining] == target]
    audit[[i]] <- tibble::tibble(
      burden_order = i,
      burden_field = field,
      direction = direction,
      selected_value = target,
      actions_remaining = length(remaining)
    )
  }
  if (length(remaining) > 1L) {
    tied_risk <- risk[competitive_index][remaining]
    remaining <- remaining[tied_risk == min(tied_risk)]
  }
  if (length(remaining) > 1L) {
    tied_ids <- as.character(competitive[[action_id_col]][remaining])
    remaining <- remaining[order(tied_ids, method = "radix")[[1L]]]
  }
  selected <- remaining[[1L]]
  competitive$.predicted_risk_gap <-
    risk[competitive_index] - minimum_risk
  competitive$.selected <- seq_len(nrow(competitive)) == selected
  list(
    selected = competitive[selected, , drop = FALSE],
    competitive = competitive,
    burden_audit = dplyr::bind_rows(audit),
    radius = radius,
    minimum_predicted_risk = minimum_risk,
    competitive_set_size = nrow(competitive)
  )
}

#' Calibrate and apply a source-balanced competitive action selector
#'
#' One conformity score is computed per pseudoanchor: the predicted-risk gap
#' between its retrospective oracle and its fitted-risk minimum. Literature
#' sources receive equal total calibration weight. OOF diagnostics use exact
#' leave-source-out radii; the deployment radius uses every calibration source.
#'
#' @keywords internal
#' @noRd
calibrate_action_competitive_selector <- function(oof_actions,
                                                  deployment_actions,
                                                  burden_fields,
                                                  directions,
                                                  level = 0.90,
                                                  anchor_col = ".anchor_id",
                                                  source_col = ".split_group",
                                                  predicted_risk_col = "predicted_risk",
                                                  evaluation_loss_col = ".evaluation_loss",
                                                  action_id_col = "action_id") {
  oof_actions <- tibble::as_tibble(oof_actions)
  deployment_actions <- tibble::as_tibble(deployment_actions)
  required_oof <- c(
    anchor_col, source_col, predicted_risk_col,
    evaluation_loss_col, action_id_col, burden_fields
  )
  required_deployment <- c(
    anchor_col, predicted_risk_col, action_id_col, burden_fields
  )
  absent_oof <- setdiff(required_oof, names(oof_actions))
  absent_deployment <- setdiff(required_deployment, names(deployment_actions))
  if (length(absent_oof) > 0L || length(absent_deployment) > 0L) {
    stop(
      paste0(
        "Competitive-selector input is incomplete.",
        if (length(absent_oof) > 0L) paste0(" OOF: ", paste(absent_oof, collapse = ", "), ".") else "",
        if (length(absent_deployment) > 0L) paste0(" Deployment: ", paste(absent_deployment, collapse = ", "), ".") else ""
      ),
      call. = FALSE
    )
  }
  oof_anchor <- as.character(oof_actions[[anchor_col]])
  oof_source <- as.character(oof_actions[[source_col]])
  if (anyNA(oof_anchor) || any(!nzchar(oof_anchor)) ||
      anyNA(oof_source) || any(!nzchar(oof_source))) {
    stop("Every OOF action requires explicit pseudoanchor and source identifiers.", call. = FALSE)
  }
  anchor_sources <- split(oof_source, oof_anchor)
  if (any(vapply(anchor_sources, function(x) length(unique(x)) != 1L, logical(1)))) {
    stop("Each pseudoanchor must map to exactly one calibration source.", call. = FALSE)
  }
  anchor_groups <- split(seq_len(nrow(oof_actions)), oof_anchor)
  gap_rows <- lapply(names(anchor_groups), function(anchor_id) {
    index <- anchor_groups[[anchor_id]]
    risk <- suppressWarnings(as.numeric(oof_actions[[predicted_risk_col]][index]))
    loss <- suppressWarnings(as.numeric(oof_actions[[evaluation_loss_col]][index]))
    ids <- as.character(oof_actions[[action_id_col]][index])
    if (any(!is.finite(risk)) || any(!is.finite(loss)) ||
        anyNA(ids) || any(!nzchar(ids))) {
      stop("OOF risk, retrospective loss, and action identity must be complete.", call. = FALSE)
    }
    oracle <- order(loss, ids, method = "radix")[[1L]]
    tibble::tibble(
      .anchor_key = anchor_id,
      .source_key = unique(oof_source[index]),
      oracle_action_id = ids[[oracle]],
      oracle_loss = loss[[oracle]],
      oracle_score_gap = risk[[oracle]] - min(risk)
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::add_count(.data$.source_key, name = ".source_n") |>
    dplyr::mutate(.source_weight = 1 / .data$.source_n)
  sources <- sort(unique(gap_rows$.source_key))
  if (length(sources) < 2L) {
    stop("Source-held-out calibration requires at least two distinct sources.", call. = FALSE)
  }
  select_groups <- function(data, radius) {
    keys <- as.character(data[[anchor_col]])
    groups <- split(seq_len(nrow(data)), keys)
    dplyr::bind_rows(lapply(names(groups), function(key) {
      result <- resolve_action_competitive_set(
        data[groups[[key]], , drop = FALSE],
        radius = radius,
        burden_fields = burden_fields,
        directions = directions,
        predicted_risk_col = predicted_risk_col,
        action_id_col = action_id_col
      )
      result$selected |>
        dplyr::mutate(
          .competitive_radius = result$radius,
          .minimum_predicted_risk = result$minimum_predicted_risk,
          .competitive_set_size = result$competitive_set_size
        )
    }))
  }
  oof_selected <- dplyr::bind_rows(lapply(sources, function(heldout_source) {
    calibration <- gap_rows[gap_rows$.source_key != heldout_source, , drop = FALSE]
    n_groups <- dplyr::n_distinct(calibration$.source_key)
    probability <- action_conformal_probability(level, n_groups)
    radius <- action_weighted_order_quantile(
      calibration$oracle_score_gap,
      calibration$.source_weight,
      probability
    )
    heldout_anchor <- gap_rows$.anchor_key[gap_rows$.source_key == heldout_source]
    selected <- select_groups(
      oof_actions[oof_anchor %in% heldout_anchor, , drop = FALSE],
      radius
    )
    selected$.calibration_heldout_source <- heldout_source
    selected$.calibration_sources <- n_groups
    selected$.conformal_probability <- probability
    selected
  }))
  selected_anchor <- as.character(oof_selected[[anchor_col]])
  oracle_index <- match(selected_anchor, gap_rows$.anchor_key)
  oof_selected$.oracle_action_id <- gap_rows$oracle_action_id[oracle_index]
  oof_selected$.oracle_loss <- gap_rows$oracle_loss[oracle_index]
  oof_selected$.selected_regret <-
    suppressWarnings(as.numeric(oof_selected[[evaluation_loss_col]])) -
    oof_selected$.oracle_loss
  if (any(!is.finite(oof_selected$.selected_regret)) ||
      any(oof_selected$.selected_regret < 0)) {
    stop("Selected-action regret must be complete and nonnegative.", call. = FALSE)
  }
  final_probability <- action_conformal_probability(level, length(sources))
  final_radius <- action_weighted_order_quantile(
    gap_rows$oracle_score_gap,
    gap_rows$.source_weight,
    final_probability
  )
  structure(
    list(
      level = level,
      calibration_unit = "source-balanced pseudoanchor conformity scores",
      conformity_score = "oracle predicted-risk gap from fitted-risk minimum",
      burden_fields = as.character(burden_fields),
      burden_directions = as.character(directions),
      calibration = gap_rows,
      oof_selections = oof_selected,
      deployment_radius = final_radius,
      deployment_probability = final_probability,
      deployment_selections = select_groups(deployment_actions, final_radius)
    ),
    class = "tsb_action_competitive_selector"
  )
}

#' Calibrate marginal selected-action biomass multiplier intervals
#'
#' The conformity response is the held-out excess absolute log biomass loss of
#' the selected action relative to the best admissible action. The resulting
#' radius is applied on the log-multiplier scale around the deployed selected
#' multiplier. This is uncertainty from the configured decision procedure; it
#' is not a prediction interval for total survey biomass truth.
#'
#' @keywords internal
#' @noRd
calibrate_action_marginal_multiplier_intervals <- function(selector,
                                                           levels = c(0.80, 0.90, 0.95, 0.99),
                                                           anchor_col = ".anchor_id",
                                                           source_col = ".split_group",
                                                           multiplier_col = "multiplier_pred",
                                                           response_col = ".selected_regret") {
  if (!inherits(selector, "tsb_action_competitive_selector")) {
    stop("'selector' must be a calibrated action competitive selector.", call. = FALSE)
  }
  oof <- tibble::as_tibble(selector$oof_selections)
  deployment <- tibble::as_tibble(selector$deployment_selections)
  levels <- sort(unique(suppressWarnings(as.numeric(levels))))
  if (length(levels) < 1L || any(!is.finite(levels)) ||
      any(levels <= 0 | levels >= 1)) {
    stop("Every interval level must be finite and strictly between zero and one.", call. = FALSE)
  }
  required_oof <- c(anchor_col, source_col, response_col)
  required_deployment <- c(anchor_col, multiplier_col)
  absent_oof <- setdiff(required_oof, names(oof))
  absent_deployment <- setdiff(required_deployment, names(deployment))
  if (length(absent_oof) > 0L || length(absent_deployment) > 0L) {
    stop(
      paste0(
        "Marginal multiplier calibration input is incomplete.",
        if (length(absent_oof) > 0L) paste0(" OOF: ", paste(absent_oof, collapse = ", "), ".") else "",
        if (length(absent_deployment) > 0L) paste0(" Deployment: ", paste(absent_deployment, collapse = ", "), ".") else ""
      ),
      call. = FALSE
    )
  }
  response <- suppressWarnings(as.numeric(oof[[response_col]]))
  source <- as.character(oof[[source_col]])
  if (any(!is.finite(response)) || any(response < 0) ||
      anyNA(source) || any(!nzchar(source))) {
    stop("Calibration regret and source identity must be complete; regret must be nonnegative.", call. = FALSE)
  }
  source_n <- ave(rep.int(1L, length(source)), source, FUN = sum)
  source_weight <- 1 / source_n
  sources <- sort(unique(source))
  if (length(sources) < 2L) {
    stop("Source-held-out interval assessment requires at least two distinct sources.", call. = FALSE)
  }
  calibration <- dplyr::bind_rows(lapply(levels, function(level) {
    probability <- action_conformal_probability(level, length(sources))
    radius <- action_weighted_order_quantile(response, source_weight, probability)
    tibble::tibble(
      level = level,
      n_pseudoanchors = length(response),
      n_sources = length(sources),
      conformal_probability = probability,
      conformal_radius = radius,
      multiplier_departure_factor = exp(radius)
    )
  }))
  heldout <- dplyr::bind_rows(lapply(sources, function(heldout_source) {
    train <- source != heldout_source
    n_groups <- length(unique(source[train]))
    dplyr::bind_rows(lapply(levels, function(level) {
      probability <- action_conformal_probability(level, n_groups)
      radius <- action_weighted_order_quantile(
        response[train], source_weight[train], probability
      )
      tibble::tibble(
        heldout_source = heldout_source,
        level = level,
        n_calibration_sources = n_groups,
        conformal_probability = probability,
        conformal_radius = radius,
        n_test_pseudoanchors = sum(!train),
        coverage = mean(response[!train] <= radius)
      )
    }))
  }))
  multiplier <- suppressWarnings(as.numeric(deployment[[multiplier_col]]))
  if (any(!is.finite(multiplier)) || any(multiplier <= 0)) {
    stop("Every deployed selected action requires a finite positive multiplier.", call. = FALSE)
  }
  intervals <- tidyr::crossing(
    .deployment_row = seq_len(nrow(deployment)),
    level = levels
  ) |>
    dplyr::left_join(
      calibration |>
        dplyr::select(
          "level", "conformal_probability", "conformal_radius",
          "multiplier_departure_factor"
        ),
      by = "level",
      relationship = "many-to-one"
    )
  deployment_expanded <- deployment[intervals$.deployment_row, , drop = FALSE]
  intervals <- dplyr::bind_cols(deployment_expanded, intervals) |>
    dplyr::mutate(
      multiplier_lo = .data[[multiplier_col]] /
        .data$multiplier_departure_factor,
      multiplier_hi = .data[[multiplier_col]] *
        .data$multiplier_departure_factor,
      selected_inside = .data[[multiplier_col]] >= .data$multiplier_lo &
        .data[[multiplier_col]] <= .data$multiplier_hi
    ) |>
    dplyr::select(-".deployment_row")
  source_summary <- heldout |>
    dplyr::group_by(.data$level) |>
    dplyr::summarise(
      source_balanced_coverage = mean(.data$coverage),
      minimum_source_coverage = min(.data$coverage),
      maximum_source_coverage = max(.data$coverage),
      .groups = "drop"
    )
  structure(
    list(
      estimand = paste(
        "held-out excess absolute log biomass loss of the selected action",
        "relative to the best admissible action"
      ),
      coverage_claim = "source-balanced marginal cross-conformal calibration",
      literal_biomass_truth_interval = FALSE,
      calibration = calibration,
      heldout_source_coverage = heldout,
      source_summary = source_summary,
      intervals = intervals
    ),
    class = "tsb_action_marginal_intervals"
  )
}

selected_action_rows <- function(data,
                                 predicted,
                                 within_branch = FALSE) {
  predicted <- as.numeric(predicted)
  if (length(predicted) != nrow(data) || any(!is.finite(predicted))) {
    stop("Action-risk predictions must be complete and finite.", call. = FALSE)
  }
  groups <- action_risk_group_indices(data, within_branch = within_branch)
  evaluation_loss <- action_risk_evaluation_loss(data)
  selected <- vapply(groups, function(idx) {
    idx[order(predicted[idx], as.character(data$action_id[idx]))[[1L]]]
  }, integer(1))
  oracle <- vapply(groups, function(idx) {
    idx[order(evaluation_loss[idx], as.character(data$action_id[idx]))[[1L]]]
  }, integer(1))
  tibble::tibble(
    group_key = names(groups),
    selected_row = selected,
    oracle_row = oracle,
    selected_loss = evaluation_loss[selected],
    oracle_loss = evaluation_loss[oracle],
    regret = evaluation_loss[selected] - evaluation_loss[oracle]
  )
}

#' Choose Super Learner weights by the operational selection estimand
#'
#' The finite 0.05 simplex is fully enumerated. The primary criterion is mean
#' pseudoanchor-level loss after selecting the minimum predicted-risk action;
#' maximum regret and row-wise weighted MAE resolve numerical ties.
#'
#' @keywords internal
#' @noRd
fit_action_risk_regret_weights <- function(pred_mat,
                                           data,
                                           denominator = 20L) {
  pred_mat <- as.matrix(pred_mat)
  data <- tibble::as_tibble(data)
  if (nrow(pred_mat) != nrow(data) || ncol(pred_mat) == 0L || any(!is.finite(pred_mat))) {
    stop("Complete OOF base predictions must align with action-risk rows.", call. = FALSE)
  }
  if (is.null(colnames(pred_mat))) colnames(pred_mat) <- paste0("method_", seq_len(ncol(pred_mat)))
  grid <- simplex_weight_grid(ncol(pred_mat), denominator = denominator)
  groups <- action_risk_group_indices(data)
  evaluation_loss <- action_risk_evaluation_loss(data)
  oracle_loss <- vapply(groups, function(idx) min(evaluation_loss[idx]), numeric(1))
  case_weight <- normalize_action_risk_weights(data$.case_weight)
  criterion <- matrix(NA_real_, nrow = nrow(grid), ncol = 3L)
  colnames(criterion) <- c("mean_selected_loss", "max_regret", "weighted_mae")
  for (i in seq_len(nrow(grid))) {
    pred <- as.numeric(pred_mat %*% grid[i, ])
    selected_loss <- vapply(groups, function(idx) {
      chosen <- idx[order(pred[idx], as.character(data$action_id[idx]))[[1L]]]
      evaluation_loss[[chosen]]
    }, numeric(1))
    criterion[i, ] <- c(
      mean(selected_loss),
      max(selected_loss - oracle_loss),
      stats::weighted.mean(abs(data$.outcome - pred), case_weight)
    )
  }
  order_key <- do.call(order, c(as.data.frame(criterion), list(seq_len(nrow(grid)))))
  best <- order_key[[1L]]
  weights <- grid[best, ]
  names(weights) <- colnames(pred_mat)
  list(
    weights = weights,
    selected_criterion = tibble::as_tibble_row(as.list(criterion[best, ])),
    grid_size = nrow(grid),
    denominator = denominator
  )
}

run_action_risk_fold_method <- function(method_now,
                                        method_index,
                                        train,
                                        test,
                                        inner_foldid,
                                        feature_cols,
                                        inner_folds,
                                        seed,
                                        outer_fold,
                                        monotone_features = character()) {
  started <- proc.time()[["elapsed"]]
  inner_pred <- rep(NA_real_, nrow(train))
  outer_pred <- rep(NA_real_, nrow(test))
  for (inner_now in sort(unique(inner_foldid))) {
    inner_train <- which(inner_foldid != inner_now)
    inner_valid <- which(inner_foldid == inner_now)
    fit_now <- try(
      fit_action_risk_base(
        train[inner_train, , drop = FALSE],
        method = method_now,
        feature_cols = feature_cols,
        inner_folds = inner_folds,
        seed = seed + 10000L * outer_fold + 100L * method_index + inner_now,
        monotone_features = monotone_features
      ),
      silent = TRUE
    )
    if (inherits(fit_now, "try-error")) {
      return(list(
        method = method_now,
        success = FALSE,
        inner_pred = inner_pred,
        outer_pred = outer_pred,
        diagnostic = tibble::tibble(
          outer_fold = outer_fold, method = method_now, success = FALSE,
          stage = paste0("inner_", inner_now), error = as.character(fit_now),
          seconds = proc.time()[["elapsed"]] - started
        )
      ))
    }
    inner_pred[inner_valid] <- predict_action_risk(fit_now, train[inner_valid, , drop = FALSE])
  }
  final_fit <- try(
    fit_action_risk_base(
      train,
      method = method_now,
      feature_cols = feature_cols,
      inner_folds = inner_folds,
      seed = seed + 10000L * outer_fold + 100L * method_index,
      monotone_features = monotone_features
    ),
    silent = TRUE
  )
  if (inherits(final_fit, "try-error")) {
    return(list(
      method = method_now,
      success = FALSE,
      inner_pred = inner_pred,
      outer_pred = outer_pred,
      diagnostic = tibble::tibble(
        outer_fold = outer_fold, method = method_now, success = FALSE,
        stage = "outer_refit", error = as.character(final_fit),
        seconds = proc.time()[["elapsed"]] - started
      )
    ))
  }
  outer_pred <- predict_action_risk(final_fit, test)
  list(
    method = method_now,
    success = all(is.finite(inner_pred)) && all(is.finite(outer_pred)),
    inner_pred = inner_pred,
    outer_pred = outer_pred,
    diagnostic = tibble::tibble(
      outer_fold = outer_fold, method = method_now,
      success = all(is.finite(inner_pred)) && all(is.finite(outer_pred)),
      stage = "complete", error = NA_character_,
      seconds = proc.time()[["elapsed"]] - started
    )
  )
}

#' Nested grouped cross-fitting for the audited action-risk learner
#'
#' @keywords internal
#' @noRd
crossfit_action_risk_learner <- function(training_data,
                                         methods = c("mean", "glm_elastic", "rpart", "rf", "xgboost", "xgboost_quantile", "xgboost_rank"),
                                         outer_folds = 5L,
                                         inner_folds = 4L,
                                         seed = 20260904L,
                                         grid_denominator = 20L,
                                         workers = 1L,
                                         progress = FALSE,
                                         monotone_features = character()) {
  feature_cols <- attr(training_data, "action_risk_feature_cols") %||% action_risk_feature_contract()$feature
  training_data <- tibble::as_tibble(training_data)
  methods <- unique(match.arg(methods, c(
    "mean", "glm_elastic", "rpart", "mars", "rf", "xgboost",
    "xgboost_quantile", "xgboost_rank"
  ), several.ok = TRUE))
  workers <- max(1L, min(as.integer(workers), length(methods)))
  cluster <- NULL
  if (workers > 1L) {
    cluster <- parallel::makePSOCKcluster(workers)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    source_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    worker_threads <- getOption("tsbiomass.action_risk_threads", 1L)
    worker_xgboost_rounds <- getOption("tsbiomass.action_risk_xgboost_rounds", 300L)
    worker_quantile_alpha <- getOption("tsbiomass.action_risk_quantile_alpha", 0.75)
    worker_rf_trees <- getOption("tsbiomass.action_risk_rf_trees", 500L)
    parallel::clusterCall(cluster, function(root, threads, xgboost_rounds, rf_trees, quantile_alpha) {
      setwd(root)
      options(tsbiomass.action_risk_threads = as.integer(threads))
      options(tsbiomass.action_risk_xgboost_rounds = as.integer(xgboost_rounds))
      options(tsbiomass.action_risk_quantile_alpha = as.numeric(quantile_alpha))
      options(tsbiomass.action_risk_rf_trees = as.integer(rf_trees))
      suppressPackageStartupMessages(devtools::load_all(root, quiet = TRUE))
      TRUE
    }, source_root, worker_threads, worker_xgboost_rounds, worker_rf_trees, worker_quantile_alpha)
  }
  outer_foldid <- balanced_provenance_foldid(
    training_data$.split_group,
    training_data$.anchor_id,
    n_folds = outer_folds,
    seed = seed
  )
  prediction_cols <- stats::setNames(
    replicate(length(methods) + 1L, rep(NA_real_, nrow(training_data)), simplify = FALSE),
    c(methods, "super_learner")
  )
  fold_weights <- list()
  fit_diagnostics <- list()
  for (fold_now in sort(unique(outer_foldid))) {
    if (isTRUE(progress)) message(sprintf("Checkpoint 06 outer fold %d/%d", fold_now, max(outer_foldid)))
    train_idx <- which(outer_foldid != fold_now)
    test_idx <- which(outer_foldid == fold_now)
    model_cols <- unique(c(
      feature_cols, ".outcome", ".case_weight", ".split_group", ".anchor_id",
      "action_id", "equation_branch_filter"
    ))
    train <- training_data[train_idx, model_cols, drop = FALSE]
    test <- training_data[test_idx, model_cols, drop = FALSE]
    inner_foldid <- balanced_provenance_foldid(
      train$.split_group,
      train$.anchor_id,
      n_folds = inner_folds,
      seed = seed + 100L * fold_now
    )
    inner_pred <- matrix(NA_real_, nrow = nrow(train), ncol = length(methods), dimnames = list(NULL, methods))
    outer_pred <- matrix(NA_real_, nrow = nrow(test), ncol = length(methods), dimnames = list(NULL, methods))
    tasks <- lapply(seq_along(methods), function(j) list(method = methods[[j]], index = j))
    run_task <- function(task) {
      tsbiomass:::run_action_risk_fold_method(
        method_now = task$method,
        method_index = task$index,
        train = train,
        test = test,
        inner_foldid = inner_foldid,
        feature_cols = feature_cols,
        inner_folds = inner_folds,
        seed = seed,
        outer_fold = fold_now,
        monotone_features = monotone_features
      )
    }
    method_results <- if (is.null(cluster)) {
      lapply(tasks, run_task)
    } else {
      parallel::clusterExport(
        cluster,
        c(
          "train", "test", "inner_foldid", "feature_cols", "inner_folds",
          "seed", "fold_now", "monotone_features"
        ),
        envir = environment()
      )
      parallel::parLapplyLB(cluster, tasks, run_task)
    }
    method_ok <- stats::setNames(rep(FALSE, length(methods)), methods)
    for (result in method_results) {
      method_now <- result$method
      j <- match(method_now, methods)
      method_ok[[method_now]] <- isTRUE(result$success)
      inner_pred[, j] <- result$inner_pred
      outer_pred[, j] <- result$outer_pred
      fit_diagnostics[[length(fit_diagnostics) + 1L]] <- result$diagnostic
      if (isTRUE(progress)) {
        message(sprintf(
          "  %s: %s (%.1f s)",
          method_now,
          if (isTRUE(result$success)) "complete" else "failed",
          result$diagnostic$seconds[[1]]
        ))
      }
    }
    keep <- names(method_ok)[method_ok & apply(inner_pred, 2L, function(x) all(is.finite(x))) & apply(outer_pred, 2L, function(x) all(is.finite(x)))]
    if (length(keep) == 0L) {
      stop(sprintf("No action-risk base learner completed outer fold %d.", fold_now), call. = FALSE)
    }
    # Keep four substantively distinct stack members on the finite 0.05 grid.
    # The simple regression tree remains a transparent diagnostic baseline;
    # including it as a fifth stack dimension would expand the exact grid from
    # 1,771 to 10,626 combinations without improving the limited-run audit.
    stack_keep <- setdiff(keep, c("mean", "rpart"))
    if (length(stack_keep) == 0L) stack_keep <- keep
    weight_fit <- fit_action_risk_regret_weights(
      inner_pred[, stack_keep, drop = FALSE],
      train,
      denominator = grid_denominator
    )
    for (method_now in keep) prediction_cols[[method_now]][test_idx] <- outer_pred[, method_now]
    prediction_cols$super_learner[test_idx] <- as.numeric(outer_pred[, stack_keep, drop = FALSE] %*% weight_fit$weights)
    fold_weights[[length(fold_weights) + 1L]] <- tibble::tibble(
      outer_fold = fold_now,
      method = stack_keep,
      weight = as.numeric(weight_fit$weights),
      grid_size = weight_fit$grid_size,
      grid_denominator = weight_fit$denominator,
      mean_selected_loss_inner = weight_fit$selected_criterion$mean_selected_loss,
      max_regret_inner = weight_fit$selected_criterion$max_regret,
      weighted_mae_inner = weight_fit$selected_criterion$weighted_mae
    )
  }
  complete_methods <- names(prediction_cols)[vapply(prediction_cols, function(x) all(is.finite(x)), logical(1))]
  if (!"super_learner" %in% complete_methods) {
    stop("The action-risk Super Learner did not produce complete outer-fold predictions.", call. = FALSE)
  }
  predictions <- training_data |>
    dplyr::mutate(outer_fold = outer_foldid)
  for (method_now in complete_methods) {
    predictions[[paste0("predicted_risk_", method_now)]] <- prediction_cols[[method_now]]
  }
  list(
    predictions = predictions,
    outer_foldid = outer_foldid,
    feature_cols = feature_cols,
    methods_requested = methods,
    methods_complete = setdiff(complete_methods, "super_learner"),
    fold_weights = dplyr::bind_rows(fold_weights),
    fit_diagnostics = dplyr::bind_rows(fit_diagnostics),
    seed = seed,
    outer_folds = outer_folds,
    inner_folds = inner_folds
  )
}

#' Audit action selection from held-out predictions
#'
#' @keywords internal
#' @noRd
evaluate_action_risk_crossfit <- function(crossfit_result) {
  data <- tibble::as_tibble(crossfit_result$predictions)
  pred_cols <- grep("^predicted_risk_", names(data), value = TRUE)
  selections <- list()
  metrics <- list()
  for (pred_col in pred_cols) {
    method <- sub("^predicted_risk_", "", pred_col)
    pred <- data[[pred_col]]
    for (within_branch in c(FALSE, TRUE)) {
      scope <- if (within_branch) "within_branch" else "all_actions"
      selected <- selected_action_rows(data, pred, within_branch = within_branch)
      selected$method <- method
      selected$selection_scope <- scope
      selected$anchor_model_id <- data$anchor_model_id[selected$selected_row]
      selected$anchor_species <- data$anchor_species[selected$selected_row]
      selected$equation_branch_filter <- data$equation_branch_filter[selected$selected_row]
      selected$selected_action_id <- data$action_id[selected$selected_row]
      selected$oracle_action_id <- data$action_id[selected$oracle_row]
      selected$selected_n_donors <- data$n_donors[selected$selected_row]
      selected$selected_donor_footprint <- data$donor_footprint[selected$selected_row]
      selected$selected_policy_aliases <- data$policy_aliases[selected$selected_row]
      selected$selected_predicted_risk <- pred[selected$selected_row]
      selected$oracle_hit <- selected$selected_action_id == selected$oracle_action_id
      groups <- action_risk_group_indices(data, within_branch = within_branch)
      selected$oracle_in_predicted_top5 <- vapply(seq_along(groups), function(i) {
        idx <- groups[[i]]
        top <- idx[order(pred[idx], as.character(data$action_id[idx]))][seq_len(min(5L, length(idx)))]
        selected$oracle_action_id[[i]] %in% data$action_id[top]
      }, logical(1))
      selected$rank_correlation <- vapply(groups, function(idx) {
        evaluation_loss <- action_risk_evaluation_loss(data)
        if (length(idx) < 3L || stats::sd(pred[idx]) == 0 || stats::sd(evaluation_loss[idx]) == 0) return(NA_real_)
        suppressWarnings(stats::cor(pred[idx], evaluation_loss[idx], method = "spearman"))
      }, numeric(1))
      selections[[length(selections) + 1L]] <- selected
      metrics[[length(metrics) + 1L]] <- tibble::tibble(
        method = method,
        selection_scope = scope,
        n_selection_groups = nrow(selected),
        mean_selected_loss = mean(selected$selected_loss),
        median_selected_loss = stats::median(selected$selected_loss),
        mean_oracle_loss = mean(selected$oracle_loss),
        mean_regret = mean(selected$regret),
        median_regret = stats::median(selected$regret),
        q90_regret = stats::quantile(selected$regret, 0.9, names = FALSE, type = 8),
        max_regret = max(selected$regret),
        oracle_hit_rate = mean(selected$oracle_hit),
        oracle_in_top5_rate = mean(selected$oracle_in_predicted_top5),
        median_within_group_spearman = stats::median(selected$rank_correlation, na.rm = TRUE),
        action_weighted_mae = stats::weighted.mean(abs(data$.outcome - pred), data$.case_weight),
        action_weighted_rmse = sqrt(stats::weighted.mean((data$.outcome - pred)^2, data$.case_weight))
      )
    }
  }
  list(
    selections = dplyr::bind_rows(selections),
    metrics = dplyr::bind_rows(metrics)
  )
}
