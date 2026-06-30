#' List context columns retained in meta-policy training tables
#'
#' @param tbl Data frame or tibble.
#' @param include_outcome Logical scalar. When `TRUE`, include `.outcome` when
#'   present.
#'
#' @return Character vector.
#'
#' @keywords internal
meta_policy_context_columns <- function(tbl,
                                        include_outcome = FALSE) {
  tbl <- tibble::as_tibble(tbl)
  out <- names(tbl)[vapply(names(tbl), function(name) {
    if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
      return(FALSE)
    }
    grepl("^(anchor_|reference_anchor_)", name) ||
      name %in% c("validation_scheme", "valid_prediction", ".split_group", "fold_id")
  }, logical(1))]
  if (isTRUE(include_outcome) && ".outcome" %in% names(tbl)) {
    out <- c(out, ".outcome")
  }
  unique(out)
}

#' Resolve the grouping column for cross-fitted meta-policy learning
#'
#' @param policy_perf Policy-performance table.
#' @param group_col Optional requested grouping column.
#'
#' @return Character scalar.
#'
#' @keywords internal
resolve_meta_policy_group_col <- function(policy_perf,
                                          group_col = NULL) {
  policy_perf <- tibble::as_tibble(policy_perf)
  if (!is.null(group_col)) {
    if (!is.character(group_col) || length(group_col) != 1 || !nzchar(group_col)) {
      stop("'group_col' must be NULL or a single non-empty column name.", call. = FALSE)
    }
    if (!group_col %in% names(policy_perf)) {
      stop(sprintf("Column '%s' was not found in 'policy_perf'.", group_col), call. = FALSE)
    }
    return(group_col)
  }

  context_cols <- meta_policy_context_columns(policy_perf)
  context_cols <- setdiff(context_cols, c("validation_scheme", "valid_prediction", ".split_group", "fold_id"))
  context_cols <- context_cols[!grepl("(^|_)model_id(_chr)?$", context_cols)]
  if (length(context_cols) == 0) {
    stop(
      "No default meta-policy grouping column could be inferred. Supply 'group_col' explicitly.",
      call. = FALSE
    )
  }
  context_cols[[1]]
}

#' Test whether one column is eligible as a default meta-policy feature
#'
#' @param name Column name.
#' @param x Column vector.
#'
#' @return Logical scalar.
#'
#' @keywords internal
is_default_meta_policy_feature <- function(name,
                                           x) {
  excluded_names <- c(
    "valid_prediction",
    ".outcome",
    ".outcome_raw",
    ".outcome_was_clipped",
    ".split_group",
    "fold_id"
  )
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    return(FALSE)
  }
  if (grepl("^(anchor_|reference_anchor_)", name) ||
    name %in% c("validation_scheme", "valid_prediction", ".split_group", "fold_id")) {
    return(FALSE)
  }
  if (name %in% excluded_names) {
    return(FALSE)
  }
  if (grepl("^meta_", name) ||
    grepl("^selected_", name) ||
    grepl("^post_selection_", name) ||
    grepl("^is_selected$", name) ||
    grepl("^dominated_", name) ||
    grepl("^multiplier_(pred|lo|hi)$", name) ||
    grepl("(^|_)error(_|$)", name) ||
    grepl("^relative_error$", name) ||
    grepl("^ts_error$", name) ||
    grepl("_covered$", name) ||
    grepl("^empirical_coverage$", name)) {
    return(FALSE)
  }
  if (!(is.numeric(x) || is.integer(x) || is.logical(x) || is.character(x) || is.factor(x))) {
    return(FALSE)
  }
  vals <- if (is.factor(x)) as.character(x) else x
  vals <- vals[!is.na(vals)]
  if (length(unique(vals)) < 2L) {
    return(FALSE)
  }
  if (is.character(x) || is.factor(x) || is.logical(x)) {
    n_unique <- length(unique(as.character(vals)))
    if (n_unique > max(50L, floor(0.5 * length(vals)))) {
      return(FALSE)
    }
    if (mean(nchar(as.character(vals)), na.rm = TRUE) > 80) {
      return(FALSE)
    }
  }
  TRUE
}

#' Default meta-policy feature columns
#'
#' Selects a generalized set of candidate feature columns from the supplied
#' policy table without relying on a analysis-specific hard-coded policy roster.
#'
#' @param tbl Policy-performance table.
#'
#' @return Character vector.
#' @keywords internal
default_meta_policy_features <- function(tbl) {
  tbl <- tibble::as_tibble(tbl)
  names(tbl)[vapply(
    names(tbl),
    function(nm) is_default_meta_policy_feature(nm, tbl[[nm]]),
    logical(1)
  )]
}

#' Remove policy-identity and post-hoc leakage fields from meta-policy features
#'
#' The score learner is meant to estimate anchor-conditional multiplier
#' replacement error from realized donor geometry, support, and benchmarked
#' behavior. It should not be allowed to memorize broad policy-class priors or
#' borrow direct uncertainty-width quantities that turn the score model into a
#' disguised interval selector.
#'
#' @param feature_cols Character vector of proposed feature columns.
#'
#' @return Character vector.
#' @keywords internal
sanitize_meta_policy_feature_cols <- function(feature_cols) {
  feature_cols <- unique(as.character(feature_cols %||% character()))
  drop_cols <- c(
    "policy",
    "policy_base",
    "policy_display",
    "candidate_pool",
    "aggregation_method",
    "policy_family",
    "candidate_pool_definition",
    "aggregation_definition",
    "plain_language_definition",
    "specificity_rank",
    "acceptable_global",
    "acceptable_bootstrap",
    "bootstrap_prob_best",
    "bootstrap_prob_within_threshold",
    "bootstrap_median_rank",
    "best_mean_species_median_abs_log",
    "one_se_threshold",
    "species_block_median_abs_log_error",
    "mean_species_median_abs_log",
    "median_abs_log",
    "policy_slope_len",
    "policy_slope_len_se",
    "policy_slope_len_lo_95",
    "policy_slope_len_hi_95",
    "policy_intercept_len",
    "policy_intercept_len_se",
    "policy_intercept_len_lo_95",
    "policy_intercept_len_hi_95",
    "policy_sigma_bs_mean",
    "policy_is_constructed_ensemble",
    "q_abs_log",
    "q_abs_log_conformal",
    "q_abs_log_total",
    "q_abs_log_structural",
    "n",
    "uncertainty_cost_log_width",
    "interval_log_width",
    "multiplier_interval_width",
    "multiplier_interval_score",
    "coefficient_slope_q95",
    "coefficient_intercept_q95",
    "coefficient_stability_n",
    "selected_policy",
    "selected_policy_display",
    "selection_tier",
    "meta_post_selection_multiplier_lo",
    "meta_post_selection_multiplier_hi",
    "meta_post_selection_interval_log_width"
  )
  setdiff(feature_cols, drop_cols)
}

#' Normalize meta-policy feature columns into a reusable blueprint
#'
#' @param data Input training or prediction table.
#' @param feature_cols Feature columns to retain.
#' @param blueprint Optional preprocessing blueprint to reuse at prediction
#'   time.
#'
#' @return A list with normalized feature data and the preprocessing blueprint.
#'
#' @keywords internal
meta_policy_blueprint <- function(data,
                                  feature_cols,
                                  blueprint = NULL) {
  data <- tibble::as_tibble(data)
  out <- data[, feature_cols, drop = FALSE]

  if (is.null(blueprint)) {
    numeric_medians <- list()
    factor_levels <- list()
    missing_indicators <- character()
    for (nm in feature_cols) {
      x <- out[[nm]]
      if (is.character(x) || is.logical(x) || is.factor(x)) {
        vals <- as.character(x)
        vals[is.na(vals) | !nzchar(vals)] <- "missing"
        levels_now <- sort(unique(vals))
        if (!"missing" %in% levels_now) {
          levels_now <- c(levels_now, "missing")
        }
        out[[nm]] <- factor(vals, levels = levels_now)
        factor_levels[[nm]] <- levels_now
      } else {
        vals <- suppressWarnings(as.numeric(x))
        miss <- !is.finite(vals)
        med <- stats::median(vals[is.finite(vals)], na.rm = TRUE)
        if (!is.finite(med)) {
          med <- 0
        }
        vals[miss] <- med
        out[[nm]] <- vals
        numeric_medians[[nm]] <- med
        if (any(miss)) {
          miss_nm <- paste0(nm, "_missing")
          out[[miss_nm]] <- miss
          missing_indicators <- c(missing_indicators, miss_nm)
        }
      }
    }
    blueprint <- list(
      feature_cols = feature_cols,
      numeric_medians = numeric_medians,
      factor_levels = factor_levels,
      missing_indicators = missing_indicators
    )
  } else {
    for (nm in feature_cols) {
      if (nm %in% names(blueprint$factor_levels)) {
        vals <- as.character(out[[nm]])
        vals[is.na(vals) | !nzchar(vals)] <- "missing"
        vals[!vals %in% blueprint$factor_levels[[nm]]] <- "missing"
        out[[nm]] <- factor(vals, levels = blueprint$factor_levels[[nm]])
      } else {
        vals <- suppressWarnings(as.numeric(out[[nm]]))
        miss <- !is.finite(vals)
        vals[miss] <- blueprint$numeric_medians[[nm]] %||% 0
        out[[nm]] <- vals
      }
    }
    for (miss_nm in blueprint$missing_indicators %||% character()) {
      base_nm <- sub("_missing$", "", miss_nm)
      vals <- if (base_nm %in% names(data)) {
        suppressWarnings(as.numeric(data[[base_nm]]))
      } else {
        rep(NA_real_, nrow(data))
      }
      out[[miss_nm]] <- !is.finite(vals)
    }
  }

  list(data = out, blueprint = blueprint)
}

#' Build a model matrix for meta-policy learners
#'
#' @param frame Preprocessed feature frame.
#' @param terms_obj Optional terms object to reuse.
#' @param x_columns Optional model-matrix columns to enforce.
#'
#' @return A list with the numeric model matrix and its terms object.
#'
#' @keywords internal
meta_policy_model_matrix <- function(frame,
                                     terms_obj = NULL,
                                     x_columns = NULL) {
  frame <- tibble::as_tibble(frame)
  if (is.null(terms_obj)) {
    terms_obj <- stats::terms(stats::as.formula("~ ."), data = frame)
  }
  x <- stats::model.matrix(terms_obj, data = frame)
  if ("(Intercept)" %in% colnames(x)) {
    x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  }
  if (!is.null(x_columns)) {
    missing_cols <- setdiff(x_columns, colnames(x))
    if (length(missing_cols) > 0) {
      add <- matrix(0, nrow = nrow(x), ncol = length(missing_cols))
      colnames(add) <- missing_cols
      x <- cbind(x, add)
    }
    x <- x[, x_columns, drop = FALSE]
  }
  list(x = x, terms = terms_obj)
}

#' Invert the modeled meta-policy outcome transformation
#'
#' @param pred Numeric prediction vector on the modeled scale.
#' @param outcome_transform Outcome transform used during fitting.
#' @param prediction_cap Upper cap applied after inversion.
#'
#' @return Numeric vector on the natural outcome scale.
#'
#' @keywords internal
inverse_meta_policy_outcome <- function(pred,
                                        outcome_transform,
                                        prediction_cap = Inf) {
  pred <- as.numeric(pred)
  if (identical(outcome_transform, "log1p")) {
    max_eta <- log(.Machine$double.xmax)
    pred <- pmin(pred, max_eta)
    pred <- pmax(0, expm1(pred))
  }
  prediction_cap <- suppressWarnings(as.numeric(prediction_cap)[[1]])
  if (!is.finite(prediction_cap) || prediction_cap <= 0) {
    prediction_cap <- .Machine$double.xmax
  }
  pred <- pmin(pred, prediction_cap)
  pred[!is.finite(pred)] <- prediction_cap
  pred
}

#' Derive a safe upper cap for meta-policy score predictions
#'
#' @param y Outcome vector on the natural scale.
#' @param multiplier Positive multiplicative cap applied to the observed
#'   maximum.
#'
#' @return Positive numeric scalar.
#'
#' @keywords internal
meta_policy_prediction_cap <- function(y,
                                       multiplier = 10) {
  y <- suppressWarnings(as.numeric(y))
  max_y <- suppressWarnings(max(y[is.finite(y)], na.rm = TRUE))
  if (!is.finite(max_y) || max_y <= 0) {
    return(1)
  }
  max(1, multiplier * max_y)
}

#' Build grouped fold assignments
#'
#' @param groups Exchangeable grouping labels.
#' @param n_folds Number of folds.
#' @param seed Integer seed.
#'
#' @return Integer vector or `NULL` when grouped folds are not feasible.
#'
#' @keywords internal
grouped_foldid <- function(groups,
                           n_folds = 5L,
                           seed = NULL) {
  groups <- as.character(groups)
  unique_groups <- sort(unique(stats::na.omit(groups)))
  if (length(unique_groups) < 2L) {
    return(NULL)
  }
  n_folds <- min(as.integer(n_folds), length(unique_groups))
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  fold_tbl <- tibble::tibble(
    group = sample(unique_groups, length(unique_groups), replace = FALSE),
    fold = rep(seq_len(n_folds), length.out = length(unique_groups))
  )
  fold_tbl$fold[match(groups, fold_tbl$group)]
}

#' Build row-wise fold assignments
#'
#' @param n Number of rows.
#' @param n_folds Number of folds.
#' @param seed Integer seed.
#'
#' @return Integer vector or `NULL`.
#'
#' @keywords internal
row_foldid <- function(n,
                       n_folds = 5L,
                       seed = NULL) {
  n <- as.integer(n)
  if (!is.finite(n) || n < 2L) {
    return(NULL)
  }
  n_folds <- min(as.integer(n_folds), n)
  if (!is.finite(n_folds) || n_folds < 2L) {
    return(NULL)
  }
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  sample(rep(seq_len(n_folds), length.out = n), n, replace = FALSE)
}

#' Resolve the available super-learner base methods
#'
#' @param methods Optional requested method names.
#' @param method_settings Optional learner method-settings list.
#'
#' @return Character vector of valid installed method names.
#'
#' @keywords internal
available_meta_policy_super_methods <- function(methods = NULL,
                                                method_settings = NULL) {
  catalog <- meta_policy_method_catalog(method_settings = method_settings)
  valid <- catalog$methods
  methods <- methods %||% catalog$default_super_methods
  methods <- unique(stringr::str_squish(as.character(unlist(methods, use.names = FALSE))))
  methods <- methods[!is.na(methods) & nzchar(methods)]
  unknown <- setdiff(methods, valid)
  if (length(unknown) > 0) {
    stop(
      sprintf("Unknown super-learner base method(s): %s", paste(unknown, collapse = ", ")),
      call. = FALSE
    )
  }
  if (length(methods) == 0) {
    methods <- "glm"
  }
  methods
}

#' Default learner-specific settings for meta-policy models
#'
#' Returns the shared method-specific settings used by both the selection and
#' conditional-uncertainty learners unless the config YAML overrides them.
#'
#' @return Named list of method-specific settings.
#'
#' @keywords internal
default_meta_policy_method_settings <- function() {
  list(
    glmnet = list(
      standardize = TRUE,
      type_measure = "mae"
    ),
    quantreg = list(
      tau = 0.50,
      fit_method = "fn",
      variants = list(
        q75 = list(
          tau = 0.75
        ),
        q90 = list(
          tau = 0.90
        )
      )
    ),
    gam = list(
      fit_method = "REML",
      select_terms = TRUE
    ),
    lmer = list(
      fit_method = "REML",
      group_cols = c(".split_group", "anchor_species", "anchor_family")
    ),
    rpart = list(
      cp = 0.01,
      minsplit = 20L,
      minbucket = 7L,
      maxdepth = 30L,
      variants = list(
        shallow = list(
          cp = 0.02,
          minsplit = 30L,
          minbucket = 10L,
          maxdepth = 3L
        ),
        deep = list(
          cp = 0.001,
          minsplit = 10L,
          minbucket = 5L,
          maxdepth = 10L
        )
      )
    ),
    ranger = list(
      num_trees = 500L,
      mtry = NULL,
      min_node_size = 5L,
      sample_fraction = 1,
      replace = TRUE,
      respect_unordered_factors = "order",
      variants = list(
        shallow = list(
          min_node_size = 15L,
          sample_fraction = 0.8
        ),
        deep = list(
          min_node_size = 3L,
          sample_fraction = 0.8
        )
      )
    ),
    xgboost = list(
      nrounds = 100L,
      eta = 0.30,
      max_depth = 6L,
      min_child_weight = 1,
      subsample = 1,
      colsample_bytree = 1,
      lambda = 1,
      alpha = 0,
      variants = list(
        conservative = list(
          nrounds = 200L,
          eta = 0.05,
          max_depth = 3L,
          min_child_weight = 5,
          subsample = 0.8,
          colsample_bytree = 0.8,
          lambda = 2,
          alpha = 0.5
        ),
        flexible = list(
          nrounds = 400L,
          eta = 0.05,
          max_depth = 6L,
          min_child_weight = 1,
          subsample = 0.9,
          colsample_bytree = 0.9,
          lambda = 1,
          alpha = 0
        )
      )
    )
  )
}

#' Merge learner-specific settings onto the package defaults
#'
#' @param method_settings Optional user-supplied method settings.
#'
#' @return Named list of merged method settings.
#'
#' @keywords internal
normalize_meta_policy_method_settings <- function(method_settings = NULL) {
  defaults <- default_meta_policy_method_settings()
  if (is.null(method_settings)) {
    return(defaults)
  }
  if (!is.list(method_settings)) {
    stop("'method_settings' must be a named list when supplied.", call. = FALSE)
  }

  merge_cfg(defaults, method_settings)
}

#' Catalog the supported meta-policy learner methods
#'
#' Builds the public method namespace used by both single-method fits and the
#' super-learner library. Named method variants are derived from the configured
#' family-specific `variants` blocks so the library can expose multiple
#' deliberately different learners from one model family.
#'
#' @param method_settings Optional learner method-settings list.
#'
#' @return A list with `methods`, `default_super_methods`, and `specs`.
#' @keywords internal
meta_policy_method_catalog <- function(method_settings = NULL) {
  method_settings <- normalize_meta_policy_method_settings(method_settings)
  specs <- list(
    glmnet_ridge = list(family = "glmnet", variant = NULL),
    glmnet_lasso = list(family = "glmnet", variant = NULL),
    glmnet_elasticnet = list(family = "glmnet", variant = NULL),
    quantreg = list(family = "quantreg", variant = NULL),
    glm = list(family = "glm", variant = NULL),
    gam = list(family = "gam", variant = NULL),
    lmer = list(family = "lmer", variant = NULL),
    rpart = list(family = "rpart", variant = NULL),
    ranger = list(family = "ranger", variant = NULL),
    xgboost = list(family = "xgboost", variant = NULL)
  )

  add_variant_specs <- function(family_name) {
    variant_defs <- (method_settings[[family_name]] %||% list())$variants %||% list()
    if (!is.list(variant_defs) || length(variant_defs) == 0) {
      return(invisible(NULL))
    }
    variant_names <- names(variant_defs)
    variant_names <- variant_names[!is.na(variant_names) & nzchar(variant_names)]
    for (variant_name in unique(variant_names)) {
      specs[[paste0(family_name, "_", variant_name)]] <<- list(
        family = family_name,
        variant = variant_name
      )
    }
    invisible(NULL)
  }

  for (family_name in c("quantreg", "lmer", "rpart", "ranger", "xgboost")) {
    add_variant_specs(family_name)
  }

  default_super_methods <- c(
    "glm",
    if ("quantreg_q75" %in% names(specs)) "quantreg_q75" else "quantreg",
    if ("quantreg_q90" %in% names(specs)) "quantreg_q90" else "quantreg",
    "gam",
    "glmnet_ridge",
    "glmnet_lasso",
    "glmnet_elasticnet",
    if ("rpart_shallow" %in% names(specs)) "rpart_shallow" else "rpart",
    if ("rpart_deep" %in% names(specs)) "rpart_deep" else "rpart",
    if ("ranger_shallow" %in% names(specs)) "ranger_shallow" else "ranger",
    if ("ranger_deep" %in% names(specs)) "ranger_deep" else "ranger",
    if ("xgboost_conservative" %in% names(specs)) "xgboost_conservative" else "xgboost",
    if ("xgboost_flexible" %in% names(specs)) "xgboost_flexible" else "xgboost"
  )
  default_super_methods <- unique(default_super_methods[default_super_methods %in% names(specs)])

  list(
    methods = names(specs),
    default_super_methods = default_super_methods,
    specs = specs
  )
}

#' Resolve one public meta-policy method name
#'
#' @param method One public learner method name.
#' @param method_settings Optional learner method-settings list.
#'
#' @return A list with `name`, `family`, and `variant`.
#' @keywords internal
meta_policy_method_spec <- function(method,
                                    method_settings = NULL) {
  method <- stringr::str_squish(as.character(method %||% ""))[[1]]
  catalog <- meta_policy_method_catalog(method_settings = method_settings)
  spec <- catalog$specs[[method]] %||% NULL
  if (!is.list(spec)) {
    stop(sprintf("Unknown meta-policy learner method: %s", method), call. = FALSE)
  }
  c(list(name = method), spec)
}

#' Resolve method-specific fit arguments for one learner family
#'
#' @param method Learner method.
#' @param method_settings Shared method settings list.
#'
#' @return Named list of arguments to pass into the learner fit.
#'
#' @keywords internal
meta_policy_method_arguments <- function(method,
                                         method_settings = NULL) {
  method_settings <- normalize_meta_policy_method_settings(method_settings)
  method_spec <- meta_policy_method_spec(method, method_settings = method_settings)
  compact_nulls <- function(x) {
    x[!vapply(x, is.null, logical(1))]
  }
  effective_family_settings <- function(family_name) {
    family_cfg <- method_settings[[family_name]] %||% list()
    variant_cfg <- list()
    if (!is.null(method_spec$variant) &&
      is.list(family_cfg$variants) &&
      method_spec$variant %in% names(family_cfg$variants)) {
      variant_cfg <- family_cfg$variants[[method_spec$variant]] %||% list()
    }
    family_cfg$variants <- NULL
    merge_cfg(family_cfg, variant_cfg)
  }

  if (identical(method_spec$family, "glmnet")) {
    return(effective_family_settings("glmnet"))
  }
  if (identical(method_spec$family, "quantreg")) {
    quantreg_cfg <- effective_family_settings("quantreg")
    return(compact_nulls(list(
      tau = as.numeric(quantreg_cfg$tau %||% 0.50),
      method = as.character(quantreg_cfg$fit_method %||% "fn")
    )))
  }
  if (identical(method_spec$family, "gam")) {
    gam_cfg <- effective_family_settings("gam")
    out <- list()
    if ("fit_method" %in% names(gam_cfg)) {
      out$method <- gam_cfg$fit_method
    }
    if ("select_terms" %in% names(gam_cfg)) {
      out$select <- isTRUE(gam_cfg$select_terms)
    }
    return(compact_nulls(out))
  }
  if (identical(method_spec$family, "lmer")) {
    lmer_cfg <- effective_family_settings("lmer")
    return(compact_nulls(list(
      fit_method = as.character(lmer_cfg$fit_method %||% "REML"),
      group_cols = as.character(unlist(
        lmer_cfg$group_cols %||% c(".split_group", "anchor_species", "anchor_family"),
        use.names = FALSE
      ))
    )))
  }
  if (identical(method_spec$family, "rpart")) {
    rpart_cfg <- effective_family_settings("rpart")
    out <- list(
      control = do.call(
        rpart::rpart.control,
        compact_nulls(list(
          cp = as.numeric(rpart_cfg$cp %||% 0.01),
          minsplit = as.integer(rpart_cfg$minsplit %||% 20L),
          minbucket = as.integer(rpart_cfg$minbucket %||% 7L),
          maxdepth = as.integer(rpart_cfg$maxdepth %||% 30L)
        ))
      )
    )
    return(compact_nulls(out))
  }
  if (identical(method_spec$family, "ranger")) {
    ranger_cfg <- effective_family_settings("ranger")
    out <- list(
      num.trees = as.integer(ranger_cfg$num_trees %||% 500L),
      mtry = ranger_cfg$mtry %||% NULL,
      min.node.size = as.integer(ranger_cfg$min_node_size %||% 5L),
      sample.fraction = as.numeric(ranger_cfg$sample_fraction %||% 1),
      replace = isTRUE(ranger_cfg$replace %||% TRUE),
      respect.unordered.factors = as.character(
        ranger_cfg$respect_unordered_factors %||% "order"
      )
    )
    return(compact_nulls(out))
  }
  if (identical(method_spec$family, "xgboost")) {
    xgboost_cfg <- effective_family_settings("xgboost")
    params <- list(
      objective = "reg:squarederror",
      eta = as.numeric(xgboost_cfg$eta %||% 0.30),
      max_depth = as.integer(xgboost_cfg$max_depth %||% 6L),
      min_child_weight = as.numeric(xgboost_cfg$min_child_weight %||% 1),
      subsample = as.numeric(xgboost_cfg$subsample %||% 1),
      colsample_bytree = as.numeric(xgboost_cfg$colsample_bytree %||% 1),
      lambda = as.numeric(xgboost_cfg$lambda %||% 1),
      alpha = as.numeric(xgboost_cfg$alpha %||% 0)
    )
    return(compact_nulls(list(
      params = params,
      nrounds = as.integer(xgboost_cfg$nrounds %||% 100L),
      verbose = 0
    )))
  }

  list()
}

#' Resolve one mixed-effects grouping column for the meta-policy learner
#'
#' @param training_data Prepared learner training table.
#' @param group_cols Candidate grouping columns in priority order.
#'
#' @return A list with the selected grouping column and normalized factor.
#'
#' @keywords internal
resolve_meta_policy_lmer_group <- function(training_data,
                                           group_cols) {
  training_data <- tibble::as_tibble(training_data)
  group_cols <- unique(as.character(unlist(group_cols %||% character(), use.names = FALSE)))
  group_cols <- group_cols[!is.na(group_cols) & nzchar(group_cols) & group_cols %in% names(training_data)]
  if (length(group_cols) == 0) {
    stop(
      "The mixed-effects meta-policy learner requires at least one available grouping column.",
      call. = FALSE
    )
  }

  # Use the first grouping column with at least two observed levels after
  # missing values are collapsed into one explicit level.
  for (group_col in group_cols) {
    group_values <- as.character(training_data[[group_col]])
    group_values[is.na(group_values) | !nzchar(group_values)] <- "missing"
    if (length(unique(group_values)) < 2L) {
      next
    }
    return(list(
      group_col = group_col,
      group_factor = factor(group_values)
    ))
  }

  stop(
    "No mixed-effects grouping column had at least two observed levels in the training data.",
    call. = FALSE
  )
}

#' Fit ensemble weights for the meta-policy super learner
#'
#' @param pred_mat Out-of-fold base-learner prediction matrix.
#' @param y Observed outcome vector.
#' @param loss Metalearner loss. The current NNLS combiner requires
#'   `"squared_error"`.
#'
#' @return Numeric nonnegative ensemble weight vector normalized to sum to one.
#'
#' @keywords internal
fit_super_learner_weights <- function(pred_mat,
                                      y,
                                      loss = c("squared_error", "absolute_error")) {
  loss <- match.arg(loss)
  if (!identical(loss, "squared_error")) {
    stop(
      paste(
        "The super learner now uses a true NNLS combiner, so",
        "'metalearner_loss' must be 'squared_error'."
      ),
      call. = FALSE
    )
  }
  pred_mat <- as.matrix(pred_mat)
  y <- as.numeric(y)
  ok <- is.finite(y) & stats::complete.cases(pred_mat)
  pred_mat <- pred_mat[ok, , drop = FALSE]
  y <- y[ok]
  if (ncol(pred_mat) == 0 || nrow(pred_mat) == 0) {
    stop("No complete out-of-fold prediction rows were available for the super learner.", call. = FALSE)
  }
  if (ncol(pred_mat) == 1L) {
    return(rep(1, 1))
  }

  nnls_fit <- try(
    nnls::nnls(pred_mat, y),
    silent = TRUE
  )
  if (!inherits(nnls_fit, "try-error")) {
    coef_now <- as.numeric(stats::coef(nnls_fit))
    coef_now[!is.finite(coef_now) | coef_now < 0] <- 0
    if (sum(coef_now) > 0) {
      return(coef_now / sum(coef_now))
    }
  }

  losses <- apply(pred_mat, 2, function(pred) {
    err <- y - pred
    mean(err^2, na.rm = TRUE)
  })
  inv <- 1 / pmax(losses, sqrt(.Machine$double.eps))
  inv / sum(inv)
}

#' Fit one base learner while suppressing hard failures
#'
#' @param training_data Model-ready training data.
#' @param method Base learner method.
#' @param feature_cols Feature columns.
#' @param outcome_transform Outcome transform.
#' @param lambda_rule Regularization rule.
#' @param inner_folds Inner cross-validation folds.
#' @param seed Integer seed.
#' @param method_settings Optional shared method-specific tuning settings.
#'
#' @return A fitted learner object or a `"try-error"` object.
#'
#' @keywords internal
fit_meta_policy_base_safely <- function(training_data,
                                        method,
                                        feature_cols,
                                        outcome_transform,
                                        lambda_rule,
                                        inner_folds,
                                        seed,
                                        method_settings = NULL) {
  try(
    fit_meta_policy_learner(
      training_data = training_data,
      method = method,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      alpha = NULL,
      inner_folds = inner_folds,
      seed = seed,
      method_settings = method_settings
    ),
    silent = TRUE
  )
}

#' Predict super-learner scores for one outer crossfit fold with streaming fits
#'
#' Runs the inner OOF loop to derive metalearner weights, then fits each base
#' learner on the full training split and predicts on the test split one at a
#' time, accumulating a weighted average before freeing each model. This keeps
#' at most one fitted base learner in memory per worker at any instant, avoiding
#' the ~11x model accumulation that causes the RAM spike during parallel
#' crossfit.
#'
#' @param train_data Model-ready training split for this fold.
#' @param test_data Test split for this fold.
#' @param feature_cols Feature columns.
#' @param outcome_transform Outcome transform.
#' @param lambda_rule Regularization rule.
#' @param inner_folds Inner CV folds.
#' @param seed Integer seed.
#' @param super_methods Optional base methods.
#' @param metalearner_loss Metalearner loss.
#' @param method_settings Optional method-specific settings.
#' @param progress Logical. Emit per-method progress messages.
#'
#' @return `test_data` with `.meta_predicted_score` appended.
#'
#' @keywords internal
stream_super_learner_fold <- function(train_data,
                                      test_data,
                                      feature_cols,
                                      outcome_transform,
                                      lambda_rule,
                                      inner_folds,
                                      seed,
                                      super_methods = NULL,
                                      metalearner_loss = c("squared_error", "absolute_error"),
                                      method_settings = NULL,
                                      progress = FALSE) {
  train_data <- tibble::as_tibble(train_data)
  test_data <- tibble::as_tibble(test_data)
  metalearner_loss <- match.arg(metalearner_loss)
  methods <- available_meta_policy_super_methods(super_methods, method_settings = method_settings)
  n_methods <- length(methods)

  # Strip context/anchor columns from train_data before the inner-fold loop.
  # Each train_data[tr_idx, ] subset copies the data frame; removing unused
  # columns reduces the size of every copy. Only strip when feature_cols is
  # explicitly provided; when NULL features are discovered from the data itself.
  if (!is.null(feature_cols) && length(feature_cols) > 0L) {
    keep_train <- intersect(
      unique(c(feature_cols, ".outcome", ".split_group")),
      names(train_data)
    )
    train_data <- train_data[, keep_train, drop = FALSE]
  }

  foldid <- if (".split_group" %in% names(train_data)) {
    grouped_foldid(train_data$.split_group, n_folds = inner_folds, seed = seed)
  } else {
    NULL
  }
  foldid <- foldid %||% row_foldid(nrow(train_data), n_folds = inner_folds, seed = seed)
  if (is.null(foldid)) {
    stop("stream_outer_fold: inner cross-validation requires at least two training rows.", call. = FALSE)
  }

  y <- train_data$.outcome
  pred_cols <- list()

  for (i_method in seq_along(methods)) {
    method_now <- methods[[i_method]]
    report_progress(progress, sprintf("  [%d/%d] OOF: %s", i_method, n_methods, method_now))
    pred_now <- rep(NA_real_, nrow(train_data))
    ok_method <- TRUE
    for (fold_now in sort(unique(foldid))) {
      tr_idx <- which(foldid != fold_now)
      vl_idx <- which(foldid == fold_now)
      if (length(tr_idx) == 0 || length(vl_idx) == 0) {
        ok_method <- FALSE
        next
      }
      fold_seed <- if (is.null(seed)) NULL else as.integer(seed) + as.integer(fold_now)
      bl <- fit_meta_policy_base_safely(
        training_data = train_data[tr_idx, , drop = FALSE],
        method = method_now,
        feature_cols = feature_cols,
        outcome_transform = outcome_transform,
        lambda_rule = lambda_rule,
        inner_folds = inner_folds,
        seed = fold_seed,
        method_settings = method_settings
      )
      if (inherits(bl, "try-error")) {
        ok_method <- FALSE
        break
      }
      sc <- try(predict_meta_policy_score(bl, train_data[vl_idx, , drop = FALSE]), silent = TRUE)
      if (inherits(sc, "try-error") || !".meta_predicted_score" %in% names(sc)) {
        ok_method <- FALSE
        break
      }
      pred_now[vl_idx] <- sc$.meta_predicted_score
      rm(bl, sc)
      gc()
    }
    if (ok_method && all(is.finite(pred_now))) {
      pred_cols[[method_now]] <- pred_now
    }
  }

  if (length(pred_cols) == 0) {
    stop("stream_outer_fold: no base methods produced complete OOF predictions.", call. = FALSE)
  }
  oof_mat <- do.call(cbind, pred_cols)
  colnames(oof_mat) <- names(pred_cols)
  weights <- fit_super_learner_weights(oof_mat, y, loss = metalearner_loss)
  names(weights) <- colnames(oof_mat)
  weights <- weights[weights > 0]
  if (length(weights) == 0) {
    stop("stream_outer_fold: metalearner produced zero-weight ensemble.", call. = FALSE)
  }
  weights <- weights / sum(weights)

  prediction_cap <- meta_policy_prediction_cap(train_data$.outcome)
  n_test <- nrow(test_data)
  acc_pred <- rep(0, n_test)
  weight_sum <- 0
  methods_final <- names(weights)

  report_progress(progress, sprintf("  Streaming %d final fits ...", length(methods_final)))
  for (i_final in seq_along(methods_final)) {
    method_now <- methods_final[[i_final]]
    w <- weights[[method_now]]
    if (!is.finite(w) || w <= 0) next
    report_progress(progress, sprintf("  [%d/%d] Final: %s (w=%.3f)", i_final, length(methods_final), method_now, w))
    bl <- fit_meta_policy_base_safely(
      training_data = train_data,
      method = method_now,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      inner_folds = inner_folds,
      seed = as.integer(seed),
      method_settings = method_settings
    )
    if (!inherits(bl, "try-error")) {
      sc <- try(predict_meta_policy_score(bl, test_data), silent = TRUE)
      if (!inherits(sc, "try-error") && ".meta_predicted_score" %in% names(sc)) {
        acc_pred <- acc_pred + w * sc$.meta_predicted_score
        weight_sum <- weight_sum + w
      }
      rm(bl, sc)
      gc()
    }
  }
  if (weight_sum > 0) {
    acc_pred <- acc_pred / weight_sum
  }
  acc_pred <- pmin(pmax(0, acc_pred), prediction_cap)
  test_data |> dplyr::mutate(.meta_predicted_score = acc_pred)
}

#' Fit the meta-policy super learner
#'
#' @param training_data Model-ready training data.
#' @param feature_cols Feature columns.
#' @param outcome_transform Outcome transform.
#' @param lambda_rule Regularization rule for glmnet-based base learners.
#' @param inner_folds Number of inner folds.
#' @param seed Integer seed.
#' @param super_methods Optional requested base methods.
#' @param metalearner_loss Metalearner loss.
#' @param method_settings Optional shared method-specific tuning settings.
#'
#' @return A fitted meta-policy learner object with super-learner weights and
#'   out-of-fold diagnostics.
#'
#' @keywords internal
fit_meta_policy_super_learner <- function(training_data,
                                          feature_cols,
                                          outcome_transform,
                                          lambda_rule,
                                          inner_folds,
                                          seed,
                                          super_methods = NULL,
                                          metalearner_loss = c("squared_error", "absolute_error"),
                                          method_settings = NULL,
                                          progress = FALSE) {
  training_data <- tibble::as_tibble(training_data)
  metalearner_loss <- match.arg(metalearner_loss)
  methods <- available_meta_policy_super_methods(super_methods, method_settings = method_settings)
  n_methods <- length(methods)

  # Strip context/anchor columns before inner-fold loop - only feature_cols +
  # .outcome + .split_group are needed, and every train_idx/valid_idx subset
  # copies the full data frame.
  keep_cols <- unique(c(feature_cols, ".outcome", ".split_group"))
  keep_cols <- intersect(keep_cols, names(training_data))
  training_data <- training_data[, keep_cols, drop = FALSE]

  foldid <- if (".split_group" %in% names(training_data)) {
    grouped_foldid(training_data$.split_group, n_folds = inner_folds, seed = seed)
  } else {
    NULL
  }
  foldid <- foldid %||% row_foldid(nrow(training_data), n_folds = inner_folds, seed = seed)
  if (is.null(foldid)) {
    stop("The super learner requires at least two training rows for inner cross-validation.", call. = FALSE)
  }

  y <- training_data$.outcome
  pred_cols <- list()
  learner_errors <- list()
  for (i_method in seq_along(methods)) {
    method_now <- methods[[i_method]]
    report_progress(progress, sprintf("  [%d/%d] OOF: %s", i_method, n_methods, method_now))
    pred_now <- rep(NA_real_, nrow(training_data))
    ok_method <- TRUE
    for (fold_now in sort(unique(foldid))) {
      train_idx <- which(foldid != fold_now)
      valid_idx <- which(foldid == fold_now)
      if (length(train_idx) == 0 || length(valid_idx) == 0) {
        ok_method <- FALSE
        next
      }
      fold_seed <- if (is.null(seed)) NULL else as.integer(seed) + as.integer(fold_now)
      learner <- fit_meta_policy_base_safely(
        training_data = training_data[train_idx, , drop = FALSE],
        method = method_now,
        feature_cols = feature_cols,
        outcome_transform = outcome_transform,
        lambda_rule = lambda_rule,
        inner_folds = inner_folds,
        seed = fold_seed,
        method_settings = method_settings
      )
      if (inherits(learner, "try-error")) {
        ok_method <- FALSE
        learner_errors[[method_now]] <- as.character(learner)
        break
      }
      scored <- try(
        predict_meta_policy_score(learner, training_data[valid_idx, , drop = FALSE]),
        silent = TRUE
      )
      if (inherits(scored, "try-error") || !".meta_predicted_score" %in% names(scored)) {
        ok_method <- FALSE
        learner_errors[[method_now]] <- as.character(scored)
        break
      }
      pred_now[valid_idx] <- scored$.meta_predicted_score
    }
    if (ok_method && all(is.finite(pred_now))) {
      pred_cols[[method_now]] <- pred_now
    }
  }

  if (length(pred_cols) == 0) {
    stop("No super-learner base methods produced complete out-of-fold predictions.", call. = FALSE)
  }
  oof_mat <- do.call(cbind, pred_cols)
  colnames(oof_mat) <- names(pred_cols)
  weights <- fit_super_learner_weights(oof_mat, y, loss = metalearner_loss)
  names(weights) <- colnames(oof_mat)

  report_progress(progress, sprintf("  Refitting %d base learners on full training data ...", length(weights)))
  final_learners <- list()
  final_method_names <- names(weights)
  for (i_final in seq_along(final_method_names)) {
    method_now <- final_method_names[[i_final]]
    report_progress(progress, sprintf("  [%d/%d] Final fit: %s", i_final, length(final_method_names), method_now))
    learner <- fit_meta_policy_base_safely(
      training_data = training_data,
      method = method_now,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      inner_folds = inner_folds,
      seed = if (is.null(seed)) NULL else as.integer(seed),
      method_settings = method_settings
    )
    if (!inherits(learner, "try-error")) {
      final_learners[[method_now]] <- learner
    }
  }
  weights <- weights[names(final_learners)]
  if (length(final_learners) == 0 || length(weights) == 0) {
    stop("No super-learner base methods could be refit on the full training data.", call. = FALSE)
  }
  weights <- weights / sum(weights)
  active <- weights > 0
  if (any(active)) {
    final_learners <- final_learners[names(weights)[active]]
    weights <- weights[active]
  }

  oof_pred <- as.numeric(oof_mat[, names(weights), drop = FALSE] %*% weights)
  oof_weights <- weights[colnames(oof_mat)]
  oof_weights[!is.finite(oof_weights)] <- 0
  structure(
    list(
      fit = final_learners,
      method = "super_learner",
      library = names(final_learners),
      weights = weights,
      metalearner_loss = metalearner_loss,
      method_settings = method_settings,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
      training_n = nrow(training_data),
      inner_foldid = foldid,
      oof_predictions = tibble::as_tibble(oof_mat),
      oof_ensemble_prediction = oof_pred,
      oof_performance = tibble::tibble(
        method = c(colnames(oof_mat), "super_learner"),
        mae = c(
          apply(oof_mat, 2, function(pred) mean(abs(y - pred), na.rm = TRUE)),
          mean(abs(y - oof_pred), na.rm = TRUE)
        ),
        rmse = c(
          apply(oof_mat, 2, function(pred) sqrt(mean((y - pred)^2, na.rm = TRUE))),
          sqrt(mean((y - oof_pred)^2, na.rm = TRUE))
        ),
        weight = c(oof_weights, 1)
      ),
      learner_errors = learner_errors
    ),
    class = "tsb_meta_policy_learner"
  )
}

#' Prepare training data for a meta-policy learner
#'
#' @param policy_perf Policy-performance table.
#' @param outcome_col Outcome column to model.
#' @param feature_cols Optional feature columns. `NULL` uses defaults.
#' @param outcome_clip_quantile Optional upper quantile used to clip extreme
#'   outcomes during training-data preparation.
#'
#' @return A tibble with model-ready features and `.outcome`.
#'
#' @examples
#' \dontrun{
#' training_data <- prepare_meta_policy_data(
#'   policy_perf = selector@benchmark$species_block_perf,
#'   outcome_col = "error_abs_log"
#' )
#' }
#' @export
prepare_meta_policy_data <- function(policy_perf,
                                     outcome_col = "error_abs_log",
                                     feature_cols = NULL,
                                     outcome_clip_quantile = NULL) {
  policy_perf <- tibble::as_tibble(policy_perf)
  if (!outcome_col %in% names(policy_perf)) {
    stop(sprintf("Outcome column '%s' was not found.", outcome_col), call. = FALSE)
  }
  policy_perf$policy <- resolve_policy_names(policy_perf)

  feature_cols <- feature_cols %||% default_meta_policy_features(policy_perf)
  feature_cols <- intersect(feature_cols, names(policy_perf))
  feature_cols <- sanitize_meta_policy_feature_cols(feature_cols)
  if (length(feature_cols) == 0) {
    stop("No usable meta-policy feature columns were supplied.", call. = FALSE)
  }

  out <- policy_perf |>
    dplyr::filter(.data$valid_prediction, is.finite(.data[[outcome_col]])) |>
    dplyr::select(
      dplyr::any_of(meta_policy_context_columns(policy_perf)),
      dplyr::all_of(feature_cols),
      .outcome = dplyr::all_of(outcome_col)
    )
  out[[outcome_col]] <- out$.outcome
  out$.outcome_raw <- out$.outcome

  for (nm in feature_cols) {
    if (is.character(out[[nm]]) || is.logical(out[[nm]])) {
      out[[nm]] <- as.factor(out[[nm]])
    }
  }

  outcome_clip_quantile <- suppressWarnings(as.numeric(outcome_clip_quantile %||% NA_real_)[[1]])
  if (is.finite(outcome_clip_quantile) && outcome_clip_quantile > 0 && outcome_clip_quantile < 1) {
    outcome_cap <- suppressWarnings(stats::quantile(
      out$.outcome_raw,
      probs = outcome_clip_quantile,
      na.rm = TRUE,
      names = FALSE,
      type = 8
    ))
    if (is.finite(outcome_cap)) {
      out$.outcome <- pmin(out$.outcome_raw, outcome_cap)
      out$.outcome_was_clipped <- out$.outcome_raw > out$.outcome
      attr(out, "outcome_clip_quantile") <- outcome_clip_quantile
      attr(out, "outcome_clip_cap") <- outcome_cap
    }
  }
  if (!".outcome_was_clipped" %in% names(out)) {
    out$.outcome_was_clipped <- FALSE
  }

  out
}

#' Fit a meta-policy learner
#'
#' @param training_data Output from `prepare_meta_policy_data()`.
#' @param method Learner method. `glm` is the simplest linear baseline;
#'   `gam` fits smooth additive terms; `rpart`, `ranger`, and `xgboost`
#'   provide tree-based nonlinear learners; the `glmnet_*` variants apply
#'   penalized Gaussian regression; and `super_learner` fits a convex ensemble
#'   over the supported base methods.
#' @param feature_cols Optional feature columns.
#' @param outcome_transform Outcome transform. `log1p` stabilizes heavy tails.
#' @param alpha Optional elastic-net mixing parameter.
#' @param lambda_rule Regularization rule for glmnet-based learners.
#' @param inner_folds Number of inner cross-validation folds.
#' @param seed Integer seed.
#' @param super_methods Optional super-learner base methods.
#' @param metalearner_loss Loss used to combine super-learner base predictions.
#' @param method_settings Optional shared method-specific tuning settings.
#' @param progress Logical. Emit [tsb_message()] progress lines when `TRUE`.
#'   Only active for `method = "super_learner"`.
#' @param ... Additional method-specific arguments.
#'
#' @return A fitted meta-policy learner object.
#'
#' @examples
#' \dontrun{
#' training_data <- prepare_meta_policy_data(selector@benchmark$species_block_perf)
#' learner <- fit_meta_policy_learner(
#'   training_data = training_data,
#'   method = "glm"
#' )
#' }
#' @export
fit_meta_policy_learner <- function(training_data,
                                    method = "super_learner",
                                    feature_cols = NULL,
                                    outcome_transform = c("log1p", "identity"),
                                    alpha = NULL,
                                    lambda_rule = c("lambda.1se", "lambda.min"),
                                    inner_folds = 5L,
                                    seed = NULL,
                                    super_methods = NULL,
                                    metalearner_loss = c("squared_error", "absolute_error"),
                                    method_settings = NULL,
                                    progress = FALSE,
                                    ...) {
  training_data <- tibble::as_tibble(training_data)
  method <- stringr::str_squish(as.character(method %||% ""))[[1]]
  outcome_transform <- match.arg(outcome_transform)
  lambda_rule <- match.arg(lambda_rule)
  metalearner_loss <- match.arg(metalearner_loss)
  method_catalog <- meta_policy_method_catalog(method_settings = method_settings)
  allowed_methods <- c(
    "super_learner",
    method_catalog$methods
  )
  if (!method %in% allowed_methods) {
    stop(
      sprintf("Unsupported meta-policy learner method '%s'. Allowed values are: %s", method, paste(allowed_methods, collapse = ", ")),
      call. = FALSE
    )
  }
  if (!".outcome" %in% names(training_data)) {
    stop("'training_data' must contain '.outcome'.", call. = FALSE)
  }

  feature_cols <- feature_cols %||% default_meta_policy_features(training_data)
  feature_cols <- intersect(feature_cols, names(training_data))
  feature_cols <- sanitize_meta_policy_feature_cols(feature_cols)
  if (length(feature_cols) == 0) {
    stop("No usable feature columns were supplied.", call. = FALSE)
  }

  if (identical(method, "super_learner")) {
    return(fit_meta_policy_super_learner(
      training_data = training_data,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      inner_folds = inner_folds,
      seed = seed,
      super_methods = super_methods,
      metalearner_loss = metalearner_loss,
      method_settings = method_settings,
      progress = progress
    ))
  }

  fit_arguments <- meta_policy_method_arguments(
    method = method,
    method_settings = method_settings
  )
  method_spec <- meta_policy_method_spec(method, method_settings = method_settings)

  y <- dplyr::case_when(
    outcome_transform == "log1p" ~ log1p(training_data$.outcome),
    TRUE ~ training_data$.outcome
  )
  prep <- meta_policy_blueprint(training_data, feature_cols)
  model_frame <- prep$data
  keep_model_col <- vapply(model_frame, function(x) {
    if (is.factor(x)) {
      return(nlevels(droplevels(x)) >= 2L)
    }
    vals <- suppressWarnings(as.numeric(x))
    vals <- vals[is.finite(vals)]
    length(unique(vals)) >= 2L
  }, logical(1))
  dropped_model_cols <- names(model_frame)[!keep_model_col]
  model_frame <- model_frame[, keep_model_col, drop = FALSE]
  if (ncol(model_frame) == 0) {
    stop("No non-constant meta-policy feature columns remained after preprocessing.", call. = FALSE)
  }

  if (identical(method_spec$family, "glmnet")) {
    alpha <- alpha %||% switch(method,
      glmnet_ridge = 0,
      glmnet_lasso = 1,
      glmnet_elasticnet = 0.25
    )
    mm <- meta_policy_model_matrix(model_frame)
    foldid <- if (".split_group" %in% names(training_data)) {
      grouped_foldid(training_data$.split_group, n_folds = inner_folds, seed = seed)
    } else {
      NULL
    }
    if (!is.null(foldid) && length(unique(stats::na.omit(foldid))) < 4L) {
      foldid <- row_foldid(nrow(mm$x), n_folds = min(max(4L, as.integer(inner_folds)), nrow(mm$x)), seed = seed)
    }
    nfolds <- if (is.null(foldid)) {
      min(max(4L, as.integer(inner_folds)), nrow(mm$x))
    } else {
      length(unique(stats::na.omit(foldid)))
    }
    fit <- glmnet::cv.glmnet(
      x = mm$x,
      y = y,
      alpha = alpha,
      foldid = foldid,
      nfolds = nfolds,
      family = "gaussian",
      type.measure = fit_arguments$type_measure %||% "mae",
      standardize = isTRUE(fit_arguments$standardize %||% TRUE),
      ...
    )
    lambda <- fit[[lambda_rule]]
    return(structure(
      list(
        fit = fit,
        method = method,
        method_family = method_spec$family,
        method_variant = method_spec$variant,
        alpha = alpha,
        lambda_rule = lambda_rule,
        lambda = lambda,
        method_settings = method_settings,
        method_arguments = fit_arguments,
        feature_cols = feature_cols,
        blueprint = prep$blueprint,
        dropped_model_cols = dropped_model_cols,
        terms = mm$terms,
        x_columns = colnames(mm$x),
        outcome_transform = outcome_transform,
        prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
        training_n = nrow(model_frame),
        model_matrix_ncol = ncol(mm$x)
      ),
      class = "tsb_meta_policy_learner"
    ))
  }

  model_data <- dplyr::bind_cols(model_frame, tibble::tibble(.outcome_model = y))
  formula_now <- stats::reformulate(
    termlabels = names(model_frame),
    response = ".outcome_model"
  )

  if (identical(method_spec$family, "quantreg")) {
    mm <- meta_policy_model_matrix(model_frame)
    qr_now <- qr(mm$x)
    keep_idx <- seq_len(qr_now$rank)
    if (length(keep_idx) == 0L) {
      stop("No non-collinear quantile-regression columns remained after preprocessing.", call. = FALSE)
    }
    x_fit <- mm$x[, qr_now$pivot[keep_idx], drop = FALSE]
    fit <- suppressWarnings(
      quantreg::rq.fit(
        x = x_fit,
        y = y,
        tau = as.numeric(fit_arguments$tau %||% 0.50),
        method = fit_arguments$method %||% "fn",
        ...
      )
    )
    return(structure(
      list(
        fit = fit,
        method = method,
        method_family = method_spec$family,
        method_variant = method_spec$variant,
        method_settings = method_settings,
        method_arguments = fit_arguments,
        feature_cols = feature_cols,
        blueprint = prep$blueprint,
        dropped_model_cols = dropped_model_cols,
        terms = mm$terms,
        x_columns = colnames(x_fit),
        outcome_transform = outcome_transform,
        prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
        training_n = nrow(model_frame),
        model_matrix_ncol = ncol(x_fit)
      ),
      class = "tsb_meta_policy_learner"
    ))
  }

  if (identical(method_spec$family, "xgboost")) {
    mm <- meta_policy_model_matrix(model_frame)
    dtrain <- xgboost::xgb.DMatrix(data = mm$x, label = y)
    xgboost_params <- fit_arguments$params %||% list(objective = "reg:squarederror")
    fit <- xgboost::xgb.train(
      params = xgboost_params,
      data = dtrain,
      nrounds = as.integer(fit_arguments$nrounds %||% 100L),
      verbose = as.integer(fit_arguments$verbose %||% 0L),
      ...
    )
    return(structure(
      list(
        fit = fit,
        method = method,
        method_family = method_spec$family,
        method_variant = method_spec$variant,
        method_settings = method_settings,
        method_arguments = fit_arguments,
        feature_cols = feature_cols,
        blueprint = prep$blueprint,
        dropped_model_cols = dropped_model_cols,
        terms = mm$terms,
        x_columns = colnames(mm$x),
        outcome_transform = outcome_transform,
        prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
        training_n = nrow(model_frame),
        model_matrix_ncol = ncol(mm$x)
      ),
      class = "tsb_meta_policy_learner"
    ))
  }

  if (identical(method_spec$family, "lmer")) {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop(
        "Fitting method 'lmer' requires the suggested package 'lme4' to be installed.",
        call. = FALSE
      )
    }

    # Resolve one conservative random-intercept grouping column before fitting.
    lmer_group <- resolve_meta_policy_lmer_group(
      training_data = training_data,
      group_cols = fit_arguments$group_cols %||% c(".split_group", "anchor_species", "anchor_family")
    )
    model_data[[lmer_group$group_col]] <- lmer_group$group_factor

    fixed_terms <- setdiff(names(model_frame), lmer_group$group_col)
    fixed_term_string <- if (length(fixed_terms) == 0) {
      "1"
    } else {
      paste(fixed_terms, collapse = " + ")
    }
    formula_now <- stats::as.formula(sprintf(
      ".outcome_model ~ %s + (1 | `%s`)",
      fixed_term_string,
      lmer_group$group_col
    ))

    fit <- lme4::lmer(
      formula_now,
      data = model_data,
      REML = identical(as.character(fit_arguments$fit_method %||% "REML"), "REML"),
      ...
    )
    return(structure(
      list(
        fit = fit,
        method = method,
        method_family = method_spec$family,
        method_variant = method_spec$variant,
        method_settings = method_settings,
        method_arguments = fit_arguments,
        feature_cols = feature_cols,
        blueprint = prep$blueprint,
        dropped_model_cols = dropped_model_cols,
        factor_levels = lapply(
          model_data[intersect(feature_cols, names(model_data))],
          function(x) if (is.factor(x)) levels(x) else NULL
        ),
        group_col = lmer_group$group_col,
        group_levels = levels(lmer_group$group_factor),
        outcome_transform = outcome_transform,
        prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
        training_n = nrow(model_data)
      ),
      class = "tsb_meta_policy_learner"
    ))
  }

  fit <- switch(method_spec$family,
    quantreg = quantreg::rq(
      formula_now,
      data = model_data,
      tau = as.numeric(fit_arguments$tau %||% 0.50),
      ...
    ),
    glm = stats::glm(formula_now, data = model_data, family = stats::gaussian()),
    gam = mgcv::gam(
      formula_now,
      data = model_data,
      family = stats::gaussian(),
      method = fit_arguments$method %||% "REML",
      select = isTRUE(fit_arguments$select %||% TRUE),
      ...
    ),
    rpart = rpart::rpart(
      formula_now,
      data = model_data,
      control = fit_arguments$control %||% rpart::rpart.control(),
      ...
    ),
    ranger = do.call(
      ranger::ranger,
      c(
        list(
          x = as.data.frame(model_frame),
          y = y,
          num.threads = 1L,
          verbose = FALSE,
          num.trees = as.integer(fit_arguments$num.trees %||% 500L),
          mtry = fit_arguments$mtry %||% NULL,
          min.node.size = as.integer(fit_arguments$min.node.size %||% 5L),
          sample.fraction = as.numeric(fit_arguments$sample.fraction %||% 1),
          replace = isTRUE(fit_arguments$replace %||% TRUE),
          respect.unordered.factors = fit_arguments$respect.unordered.factors %||% "order"
        ),
        if (!is.null(seed)) list(seed = as.integer(seed)) else list(),
        list(...)
      )
    )
  )

  if (identical(method_spec$family, "ranger")) {
    fit$predictions <- NULL
    fit$call <- NULL
  }

  structure(
    list(
      fit = fit,
      method = method,
      method_family = method_spec$family,
      method_variant = method_spec$variant,
      method_settings = method_settings,
      method_arguments = fit_arguments,
      feature_cols = feature_cols,
      blueprint = prep$blueprint,
      dropped_model_cols = dropped_model_cols,
      factor_levels = lapply(
        model_data[intersect(feature_cols, names(model_data))],
        function(x) if (is.factor(x)) levels(x) else NULL
      ),
      outcome_transform = outcome_transform,
      prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
      training_n = nrow(model_data)
    ),
    class = "tsb_meta_policy_learner"
  )
}

#' Predict policy-specific transferability score with a meta-policy learner
#'
#' @param object Fitted object from `fit_meta_policy_learner()`.
#' @param new_policy_tbl New policy table.
#'
#' @return `new_policy_tbl` with `.meta_predicted_score`.
#'
#' @examples
#' \dontrun{
#' scored <- predict_meta_policy_score(
#'   learner,
#'   new_policy_tbl = selector@predictions@intervals
#' )
#' }
#' @export
predict_meta_policy_score <- function(object,
                                      new_policy_tbl) {
  if (!inherits(object, "tsb_meta_policy_learner")) {
    stop("'object' must be a fitted meta-policy learner.", call. = FALSE)
  }
  new_policy_tbl <- tibble::as_tibble(new_policy_tbl)
  if (!all(object$feature_cols %in% names(new_policy_tbl))) {
    missing_cols <- setdiff(object$feature_cols, names(new_policy_tbl))
    for (nm in missing_cols) {
      new_policy_tbl[[nm]] <- NA
    }
  }

  prediction_tbl <- new_policy_tbl
  method_family <- object$method_family %||% object$method
  if (identical(object$method, "super_learner")) {
    if (length(object$fit) == 0 || length(object$weights) == 0) {
      stop("The super-learner object does not contain fitted base learners.", call. = FALSE)
    }
    pred_mat <- vapply(names(object$fit), function(method_now) {
      scored <- predict_meta_policy_score(object$fit[[method_now]], new_policy_tbl)
      scored$.meta_predicted_score
    }, numeric(nrow(new_policy_tbl)))
    pred <- as.numeric(pred_mat %*% object$weights[colnames(pred_mat)])
    pred <- pmin(pmax(0, pred), object$prediction_cap %||% Inf)
  } else if (identical(method_family, "glmnet") || identical(method_family, "xgboost") || identical(method_family, "quantreg")) {
    prep <- meta_policy_blueprint(
      prediction_tbl,
      object$feature_cols,
      blueprint = object$blueprint
    )
    pred_frame <- prep$data[, setdiff(names(prep$data), object$dropped_model_cols %||% character()), drop = FALSE]
    mm <- meta_policy_model_matrix(
      pred_frame,
      terms_obj = object$terms,
      x_columns = object$x_columns
    )
    pred <- if (identical(method_family, "glmnet")) {
      as.numeric(stats::predict(object$fit, newx = mm$x, s = object$lambda))
    } else if (identical(method_family, "quantreg")) {
      coef_now <- suppressWarnings(as.numeric(object$fit$coefficients %||% object$fit$coef))
      coef_now[!is.finite(coef_now)] <- 0
      as.numeric(mm$x %*% coef_now)
    } else {
      as.numeric(stats::predict(object$fit, newdata = xgboost::xgb.DMatrix(mm$x)))
    }
  } else if (identical(method_family, "ranger")) {
    prep <- meta_policy_blueprint(
      prediction_tbl,
      object$feature_cols,
      blueprint = object$blueprint
    )
    pred_frame <- prep$data[, setdiff(names(prep$data), object$dropped_model_cols %||% character()), drop = FALSE]
    pred <- as.numeric(stats::predict(object$fit, data = pred_frame)$predictions)
  } else {
    prep <- meta_policy_blueprint(
      prediction_tbl,
      object$feature_cols,
      blueprint = object$blueprint
    )
    pred_frame <- prep$data[, setdiff(names(prep$data), object$dropped_model_cols %||% character()), drop = FALSE]
    fit_xlevels <- tryCatch(
      object$fit$xlevels,
      error = function(e) NULL
    )
    for (nm in intersect(names(object$factor_levels %||% list()), names(pred_frame))) {
      levels_now <- fit_xlevels[[nm]] %||% object$factor_levels[[nm]]
      if (!is.null(levels_now)) {
        vals <- as.character(pred_frame[[nm]])
        fallback_level <- if ("missing" %in% levels_now) "missing" else levels_now[[1]]
        vals[is.na(vals) | !nzchar(vals) | !vals %in% levels_now] <- fallback_level
        pred_frame[[nm]] <- factor(vals, levels = levels_now)
      }
    }
    if (identical(method_family, "glm")) {
      mm <- stats::model.matrix(stats::delete.response(stats::terms(object$fit)), data = pred_frame)
      coef_now <- stats::coef(object$fit)
      coef_now[!is.finite(coef_now)] <- 0
      missing_cols <- setdiff(names(coef_now), colnames(mm))
      if (length(missing_cols) > 0) {
        add <- matrix(0, nrow = nrow(mm), ncol = length(missing_cols))
        colnames(add) <- missing_cols
        mm <- cbind(mm, add)
      }
      mm <- mm[, names(coef_now), drop = FALSE]
      pred <- as.numeric(mm %*% coef_now)
    } else if (identical(method_family, "lmer")) {
      # Carry the stored grouping column forward and allow new levels so cold
      # groups fall back to the fixed-effect mean rather than erroring.
      group_col <- as.character(object$group_col %||% "")[[1]]
      if (nzchar(group_col)) {
        group_values <- if (group_col %in% names(prediction_tbl)) {
          as.character(prediction_tbl[[group_col]])
        } else {
          rep("missing", nrow(pred_frame))
        }
        group_values[is.na(group_values) | !nzchar(group_values)] <- "missing"
        pred_frame[[group_col]] <- factor(
          group_values,
          levels = unique(c(object$group_levels %||% character(), group_values))
        )
      }
      pred <- as.numeric(stats::predict(object$fit, newdata = pred_frame, allow.new.levels = TRUE))
    } else {
      pred <- as.numeric(stats::predict(object$fit, newdata = pred_frame))
    }
  }
  if (!identical(object$method, "super_learner")) {
    pred <- inverse_meta_policy_outcome(
      pred,
      object$outcome_transform,
      prediction_cap = object$prediction_cap %||% Inf
    )
  }

  new_policy_tbl |>
    dplyr::mutate(.meta_predicted_score = pred)
}

#' Cross-fit a meta-policy learner on benchmark rows
#'
#' @param policy_perf Policy-performance table.
#' @param group_col Exchangeable grouping column. When `NULL`, the function
#'   infers the first available non-ID anchor-context column.
#' @param n_folds Number of cross-fitting folds.
#' @param method Learner method.
#' @param seed Integer seed.
#' @param feature_cols Optional feature columns.
#' @param outcome_col Outcome column to model.
#' @param outcome_clip_quantile Optional upper quantile used to clip extreme
#'   outcomes before fitting.
#' @param outcome_transform Outcome transform used during training.
#' @param lambda_rule Regularization rule for glmnet-based learners.
#' @param alpha Optional elastic-net mixing parameter.
#' @param inner_folds Number of inner cross-validation folds.
#' @param super_methods Optional super-learner base methods.
#' @param metalearner_loss Loss used to combine super-learner base predictions.
#' @param method_settings Optional shared method-specific tuning settings.
#' @param workers Number of workers used for cross-fitting.
#' @param package_dir Optional package source directory used to initialize
#'   worker processes.
#' @param progress Logical. Emit [tsb_message()] progress lines when `TRUE`.
#'
#' @return A list with cross-fitted predictions and fold assignments.
#'
#' @examples
#' \dontrun{
#' crossfit_obj <- crossfit_meta_policy_learner(
#'   policy_perf = selector@benchmark$species_block_perf,
#'   method = "glm"
#' )
#' }
#' @export
crossfit_meta_policy_learner <- function(policy_perf,
                                         group_col = NULL,
                                         n_folds = 5L,
                                         method = "glm",
                                         seed = NULL,
                                         feature_cols = NULL,
                                         outcome_col = "error_abs_log",
                                         outcome_clip_quantile = NULL,
                                         outcome_transform = "log1p",
                                         lambda_rule = "lambda.1se",
                                         alpha = NULL,
                                         inner_folds = 5L,
                                         super_methods = NULL,
                                         metalearner_loss = "squared_error",
                                         method_settings = NULL,
                                         workers = 1L,
                                         package_dir = NULL,
                                         progress = FALSE) {
  policy_perf <- tibble::as_tibble(policy_perf)
  group_col <- resolve_meta_policy_group_col(policy_perf, group_col = group_col)
  if (!is.numeric(workers) || length(workers) != 1 || !is.finite(workers) || workers < 1) {
    stop("'workers' must be one finite number >= 1.", call. = FALSE)
  }
  groups <- sort(unique(stats::na.omit(as.character(policy_perf[[group_col]]))))
  n_folds <- min(as.integer(n_folds), length(groups))
  if (!is.finite(n_folds) || n_folds < 2L) {
    stop("'n_folds' must be at least 2 and no larger than the number of groups.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  fold_tbl <- tibble::tibble(
    group_id = sample(groups, length(groups), replace = FALSE),
    fold_id = rep(seq_len(n_folds), length.out = length(groups))
  )

  data_all <- prepare_meta_policy_data(
    policy_perf = policy_perf,
    outcome_col = outcome_col,
    feature_cols = feature_cols,
    outcome_clip_quantile = outcome_clip_quantile
  ) |>
    dplyr::mutate(.split_group = as.character(.data[[group_col]])) |>
    dplyr::left_join(fold_tbl, by = c(".split_group" = "group_id"))

  if (!"fold_id" %in% names(data_all)) {
    if ("fold_id.y" %in% names(data_all)) {
      data_all$fold_id <- data_all$fold_id.y
    } else if ("fold_id.x" %in% names(data_all)) {
      data_all$fold_id <- data_all$fold_id.x
    }
  }
  data_all <- data_all |>
    dplyr::select(-dplyr::any_of(c("fold_id.x", "fold_id.y")))

  is_super <- identical(as.character(method), "super_learner")
  n_base_methods <- if (is_super) {
    length(available_meta_policy_super_methods(super_methods, method_settings = method_settings))
  } else {
    1L
  }

  # Pre-compute per-fold train/test splits. Strip context/anchor columns from the
  # training halves so each serialised fold_split element is as small as possible.
  # Only strip when feature_cols is explicitly provided; when NULL the feature
  # columns are determined dynamically by prepare_meta_policy_data and we must
  # keep all columns so downstream fitting can discover them.
  n_rows_data <- nrow(data_all)
  data_clip_quantile <- attr(data_all, "outcome_clip_quantile")
  data_clip_cap <- attr(data_all, "outcome_clip_cap")
  train_keep <- if (!is.null(feature_cols) && length(feature_cols) > 0L) {
    intersect(
      unique(c(feature_cols, ".outcome", ".split_group", "fold_id")),
      names(data_all)
    )
  } else {
    names(data_all)
  }
  fold_splits <- lapply(seq_len(n_folds), function(f) {
    mask_train <- !is.na(data_all$fold_id) & data_all$fold_id != f
    mask_test <- !is.na(data_all$fold_id) & data_all$fold_id == f
    list(
      train = data_all[mask_train, train_keep, drop = FALSE],
      test = data_all[mask_test, , drop = FALSE],
      fold_id = f
    )
  })
  rm(data_all)
  gc()

  # run_fold_split receives a pre-sliced list(train, test, fold_id). Using the
  # streaming path for super_learner avoids accumulating all 11 base learner
  # model objects simultaneously - only one model at a time is held in RAM.
  run_fold_split <- function(fold_split) {
    f <- fold_split$fold_id
    train <- fold_split$train
    test <- fold_split$test
    if (nrow(train) == 0 || nrow(test) == 0) {
      return(tibble::tibble())
    }
    outer_seed <- if (is.null(seed)) NULL else as.integer(seed) + f
    if (is_super) {
      stream_super_learner_fold <- utils::getFromNamespace(
        "stream_super_learner_fold",
        "tsbiomass"
      )
      preds <- stream_super_learner_fold(
        train_data = train,
        test_data = test,
        feature_cols = feature_cols,
        outcome_transform = outcome_transform,
        lambda_rule = lambda_rule,
        inner_folds = inner_folds,
        seed = outer_seed,
        super_methods = super_methods,
        metalearner_loss = metalearner_loss,
        method_settings = method_settings,
        progress = FALSE
      )
    } else {
      fit_meta_policy_learner <- utils::getFromNamespace("fit_meta_policy_learner", "tsbiomass")
      predict_meta_policy_score <- utils::getFromNamespace("predict_meta_policy_score", "tsbiomass")
      learner <- fit_meta_policy_learner(
        training_data = train,
        method = method,
        feature_cols = feature_cols,
        outcome_transform = outcome_transform,
        lambda_rule = lambda_rule,
        alpha = alpha,
        inner_folds = inner_folds,
        super_methods = super_methods,
        metalearner_loss = metalearner_loss,
        seed = outer_seed,
        method_settings = method_settings
      )
      preds <- predict_meta_policy_score(learner, test)
    }
    preds |> dplyr::mutate(fold_id = f)
  }

  # Replace the closure's captured environment with a minimal set of scalars.
  # Without this, run_fold_split's environment includes fold_splits (10 x data_all
  # in size) and that entire object is serialised into every PSOCK worker when
  # clusterExport sends the function. Workers receive their fold_split as an
  # argument from parLapplyLB - they do not need the full list in their namespace.
  environment(run_fold_split) <- list2env(
    list(
      is_super = is_super,
      feature_cols = feature_cols,
      method = method,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      alpha = alpha,
      inner_folds = inner_folds,
      super_methods = super_methods,
      metalearner_loss = metalearner_loss,
      method_settings = method_settings,
      seed = seed
    ),
    parent = baseenv()
  )

  workers <- as.integer(workers)
  n_workers_eff <- min(workers, n_folds)
  report_progress(
    progress,
    sprintf(
      "Starting %d-fold cross-fit: method=%s, %d base learners, %d workers, %d rows.",
      n_folds, method, n_base_methods, n_workers_eff, n_rows_data
    )
  )

  if (workers > 1L && n_folds > 1L) {
    cluster_obj <- initialize_parallel_cluster(
      workers = n_workers_eff,
      package_dir = package_dir
    )
    on.exit(parallel::stopCluster(cluster_obj), add = TRUE)
    tsb_cluster_export(
      cluster_obj,
      # fold_splits is intentionally absent: parLapplyLB distributes each
      # element as the function argument, and run_fold_split's closure
      # environment has already been stripped to scalars only.
      c("run_fold_split"),
      envir = environment()
    )
    pred_rows <- parallel::parLapplyLB(cluster_obj, fold_splits, run_fold_split) |>
      dplyr::bind_rows()
  } else {
    pred_rows <- purrr::map_dfr(seq_along(fold_splits), function(i) {
      report_progress(
        progress,
        sprintf(
          "  Outer fold %d/%d (%d train rows, %d test rows) ...",
          i, n_folds, nrow(fold_splits[[i]]$train), nrow(fold_splits[[i]]$test)
        )
      )
      run_fold_split(fold_splits[[i]])
    })
  }

  report_progress(progress, sprintf("Cross-fit complete: %d prediction rows collected.", nrow(pred_rows)))

  # Mirror the observed outcome onto the configured outcome name so downstream
  # code can work with one stable column regardless of whether it expects the
  # generic `.outcome` training field or the analysis-facing outcome label.
  if (!outcome_col %in% names(pred_rows) && ".outcome" %in% names(pred_rows)) {
    pred_rows[[outcome_col]] <- pred_rows$.outcome
  }

  list(
    fold_assignments = fold_tbl,
    predictions = pred_rows,
    outcome_clip_quantile = data_clip_quantile,
    outcome_clip_cap = data_clip_cap
  )
}

#' Summarize cross-fitted meta-policy score predictions
#'
#' @param predictions Output prediction table from [crossfit_meta_policy_learner()].
#' @param outcome_col Outcome column, usually `.outcome` or `error_abs_log`.
#'
#' @return One-row tibble of calibration and ranking diagnostics.
#'
#' @examples
#' \dontrun{
#' summarize_meta_policy_predictions(crossfit_obj$predictions)
#' }
#' @export
summarize_meta_policy_predictions <- function(predictions,
                                              outcome_col = ".outcome") {
  predictions <- tibble::as_tibble(predictions)
  if (!outcome_col %in% names(predictions) && ".outcome" %in% names(predictions)) {
    outcome_col <- ".outcome"
  }
  if (!all(c(outcome_col, ".meta_predicted_score") %in% names(predictions))) {
    return(tibble::tibble())
  }
  ok <- is.finite(predictions[[outcome_col]]) & is.finite(predictions$.meta_predicted_score)
  if (!any(ok)) {
    return(tibble::tibble())
  }
  obs <- predictions[[outcome_col]][ok]
  pred <- predictions$.meta_predicted_score[ok]
  out <- tibble::tibble(
    n = length(obs),
    mae = mean(abs(pred - obs), na.rm = TRUE),
    rmse = sqrt(mean((pred - obs)^2, na.rm = TRUE)),
    bias = mean(pred - obs, na.rm = TRUE),
    spearman = suppressWarnings(stats::cor(pred, obs, method = "spearman", use = "complete.obs")),
    pearson = suppressWarnings(stats::cor(pred, obs, method = "pearson", use = "complete.obs")),
    median_observed = stats::median(obs, na.rm = TRUE),
    median_predicted = stats::median(pred, na.rm = TRUE)
  )
  if (".outcome_raw" %in% names(predictions)) {
    raw_obs <- predictions$.outcome_raw[ok]
    out$raw_mae <- mean(abs(pred - raw_obs), na.rm = TRUE)
    out$raw_rmse <- sqrt(mean((pred - raw_obs)^2, na.rm = TRUE))
    out$raw_bias <- mean(pred - raw_obs, na.rm = TRUE)
    out$raw_spearman <- suppressWarnings(stats::cor(pred, raw_obs, method = "spearman", use = "complete.obs"))
    out$raw_pearson <- suppressWarnings(stats::cor(pred, raw_obs, method = "pearson", use = "complete.obs"))
    out$raw_median_observed <- stats::median(raw_obs, na.rm = TRUE)
  }
  if (".outcome_was_clipped" %in% names(predictions)) {
    out$clipped_fraction <- mean(as.logical(predictions$.outcome_was_clipped[ok]), na.rm = TRUE)
  }
  out
}
