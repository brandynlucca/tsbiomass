#' Learned-Distance Alchemist S7 Class
#'
#' `Alchemist` replaces the Gower-distance tuning pipeline with a supervised
#' metric-learning approach. A Super Learner is trained to predict pairwise
#' acoustic distances (log-sigma_bs differences integrated over the receiving
#' anchor's length distribution) from per-trait Gower distance features. The
#' resulting N x N learned distance matrix is the direct input to ordination,
#' admissibility kernel weighting, and policy selection strategies.
#'
#' Construction requires a fully ingested [Candidates] object. Configuration
#' is either inherited from the `Candidates` workflow config (alchemist or
#' similarity section) or supplied directly as a list or YAML path.
#'
#' @examples
#' \dontrun{
#' alchemist <- as_alchemist(candidates)
#' alchemist <- forge_distances(alchemist)
#' alchemist <- distill_traits(alchemist)
#' alchemist <- run_ordination(alchemist)
#' alchemist <- screen_admissibility(alchemist)
#' selector  <- as_selector(alchemist)
#' }
#'
#' @name Alchemist-class
#' @aliases Alchemist
NULL

# ── Internal predicates ────────────────────────────────────────────────────────

.is_alchemist <- function(x) {
  inherits(x, "S7_object") &&
    exists("Alchemist", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(x, Alchemist), error = function(e) FALSE))
}

.is_candidates_obj <- function(x) {
  inherits(x, "S7_object") &&
    exists("Candidates", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(x, Candidates), error = function(e) FALSE))
}

# ── Config normalization ───────────────────────────────────────────────────────

workflow_to_alchemist_config <- function(source) {
  cfg <- if (.is_candidates_obj(source)) {
    candidates_configuration(source) %||% list()
  } else if (inherits(source, "S7_object") &&
    exists("Configurer", inherits = TRUE) &&
    isTRUE(tryCatch(S7::S7_inherits(source, Configurer), error = function(e) FALSE))) {
    source@data
  } else if (is.list(source)) {
    source
  } else {
    list()
  }

  alch <- cfg$alchemist %||% list()
  sim  <- cfg$similarity %||% list()
  ml   <- cfg$metalearner %||% list()

  list(
    species_traits   = alch$species_traits %||% sim$species_traits %||% list(),
    study_traits     = alch$study_traits   %||% sim$study_traits   %||% list(),
    learner = list(
      methods           = alch$learner$methods           %||% ml$selection_super_methods %||% NULL,
      inner_folds       = as.integer(alch$learner$inner_folds %||% ml$inner_folds %||% 5L),
      seed              = as.integer(alch$learner$seed        %||% ml$seed        %||% 42L),
      outcome_transform = alch$learner$outcome_transform %||% ml$outcome_transform %||% "identity",
      lambda_rule       = alch$learner$lambda_rule       %||% ml$lambda_rule      %||% "min",
      method_settings   = alch$learner$method_settings   %||% ml$method_settings  %||% NULL,
      workers           = as.integer(alch$learner$workers %||% ml$workers %||% 1L)
    ),
    registry_path  = alch$registry_path %||% cfg$registry_path %||% NULL,
    progress       = alch$progress %||% FALSE,
    workflow_config = cfg
  )
}

normalize_alchemist_config <- function(config, candidates = NULL) {
  if (is.null(config)) {
    config <- if (!is.null(candidates)) {
      workflow_to_alchemist_config(candidates)
    } else {
      list()
    }
  } else if (.is_candidates_obj(config) ||
    (inherits(config, "S7_object") &&
      exists("Configurer", inherits = TRUE) &&
      isTRUE(tryCatch(S7::S7_inherits(config, Configurer), error = function(e) FALSE)))) {
    config <- workflow_to_alchemist_config(config)
  } else if (is.character(config) && length(config) == 1 && file.exists(config)) {
    raw <- yaml::read_yaml(config)
    cfg_obj <- tryCatch(as_configurer(raw), error = function(e) NULL)
    config <- if (!is.null(cfg_obj)) workflow_to_alchemist_config(cfg_obj) else raw
  } else if (!is.list(config)) {
    stop(
      "'config' must be NULL, a list, a YAML path, a `Candidates`, or a `Configurer`.",
      call. = FALSE
    )
  }

  if (is.null(config$learner)) config$learner <- list()
  config$learner$inner_folds       <- as.integer(config$learner$inner_folds %||% 5L)
  config$learner$seed              <- as.integer(config$learner$seed        %||% 42L)
  config$learner$outcome_transform <- config$learner$outcome_transform %||% "identity"
  config$learner$lambda_rule       <- config$learner$lambda_rule       %||% "min"
  config$learner$workers           <- as.integer(config$learner$workers %||% 1L)

  config
}

# ── Class definition ───────────────────────────────────────────────────────────

#' @rdname Alchemist-class
#' @export
Alchemist <- S7::new_class(
  "Alchemist",
  properties = list(
    candidates      = S7::new_property(S7::class_any),
    config          = S7::new_property(S7::class_list),
    learner         = S7::new_property(S7::class_list),
    distance_matrix = S7::new_property(S7::class_list),
    trait_importance = S7::new_property(S7::class_list),
    ordination      = S7::new_property(S7::class_list),
    admissibility   = S7::new_property(S7::class_list)
  ),
  validator = function(self) {
    tryCatch(
      {
        if (!.is_candidates_obj(self@candidates)) {
          return("`candidates` must be a `Candidates` object.")
        }
        if (!is.list(self@config))          return("`config` must be a list.")
        if (!is.list(self@learner))         return("`learner` must be a list.")
        if (!is.list(self@distance_matrix)) return("`distance_matrix` must be a list.")
        if (!is.list(self@trait_importance)) return("`trait_importance` must be a list.")
        if (!is.list(self@ordination))      return("`ordination` must be a list.")
        if (!is.list(self@admissibility))   return("`admissibility` must be a list.")
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
)

S7::S4_register(Alchemist)

# ── Rebuild helpers ────────────────────────────────────────────────────────────

alchemist_rebuild <- function(object,
                              candidates      = object@candidates,
                              config          = object@config,
                              learner         = object@learner,
                              distance_matrix = object@distance_matrix,
                              trait_importance = object@trait_importance,
                              ordination      = object@ordination,
                              admissibility   = object@admissibility) {
  Alchemist(
    candidates      = candidates,
    config          = config,
    learner         = learner,
    distance_matrix = distance_matrix,
    trait_importance = trait_importance,
    ordination      = ordination,
    admissibility   = admissibility
  )
}

# ── Constructor ────────────────────────────────────────────────────────────────

#' Build an Alchemist from a Candidates object
#'
#' @param candidates A [Candidates] object. Must have `@candidate_models`
#'   populated; `@similarity_matrix` is used to inherit trait names when no
#'   explicit config is supplied.
#' @param config Optional alchemist config list, YAML path, or [Configurer]
#'   object. When `NULL`, trait names and learner settings are inherited from
#'   `candidates`.
#' @param ... Unused.
#'
#' @return An [Alchemist] object.
#'
#' @export
as_alchemist <- function(candidates, config = NULL, ...) {
  if (.is_alchemist(candidates)) return(candidates)

  if (!.is_candidates_obj(candidates)) {
    stop("'candidates' must be a `Candidates` object.", call. = FALSE)
  }

  config <- normalize_alchemist_config(config, candidates = candidates)

  if (length(config$species_traits) == 0 && length(candidates@similarity_matrix) > 0) {
    config$species_traits <- (candidates@similarity_matrix)$species_traits %||% list()
  }
  if (length(config$study_traits) == 0 && length(candidates@similarity_matrix) > 0) {
    config$study_traits <- (candidates@similarity_matrix)$study_traits %||% list()
  }

  Alchemist(
    candidates      = candidates,
    config          = config,
    learner         = list(),
    distance_matrix = list(),
    trait_importance = list(),
    ordination      = list(),
    admissibility   = list()
  )
}

# ── Pair-level training data ───────────────────────────────────────────────────

#' Normalized Gower distance for one trait column
#'
#' @keywords internal
alchemist_gower_col <- function(x) {
  if (is.numeric(x)) {
    finite_x <- x[is.finite(x)]
    r <- if (length(finite_x) >= 2L) diff(range(finite_x)) else 1
    if (!is.finite(r) || r <= 0) r <- 1
    mat <- outer(x, x, function(a, b) abs(a - b) / r)
    mat[!is.finite(mat)] <- 0.5
    mat
  } else {
    xc <- as.character(x)
    xc[is.na(x) | !nzchar(xc)] <- NA_character_
    outer(xc, xc, function(a, b) {
      ifelse(is.na(a) | is.na(b), 0.5, ifelse(a == b, 0, 1))
    })
  }
}

#' Build length PDF for one model row (uniform approximation)
#'
#' @keywords internal
alchemist_anchor_pdf <- function(lo, hi, n = 400L) {
  lo <- suppressWarnings(as.numeric(lo))
  hi <- suppressWarnings(as.numeric(hi))
  if (!is.finite(lo) || !is.finite(hi) || lo <= 0 || hi <= 0) return(NULL)
  if (hi > lo) {
    g <- seq(lo, hi, length.out = n)
    tibble::tibble(length_cm = g, f_len = rep(1 / length(g), length(g)))
  } else {
    tibble::tibble(length_cm = lo, f_len = 1)
  }
}

#' Resolve trait names from a config list element
#'
#' @keywords internal
alchemist_trait_names <- function(traits, models_df) {
  nms <- if (!is.null(names(traits)) && any(nzchar(names(traits)))) {
    names(traits)
  } else {
    as.character(unlist(traits, use.names = FALSE))
  }
  nms <- nms[nzchar(nms) & nms %in% names(models_df)]
  unique(nms)
}

#' Build the pair-level supervised training table
#'
#' @param models_df Candidate-model data frame.
#' @param species_trait_names Character vector of species trait column names.
#' @param study_trait_names Character vector of study trait column names.
#'
#' @return A list with `training_data`, `feature_cols`, `all_traits`,
#'   `species_trait_names`, `n_models`, `model_ids`, `donor_sigma_matrix`,
#'   `target_sigma`, and `trait_mats`.
#'
#' @keywords internal
build_alchemist_pair_data <- function(models_df,
                                      species_trait_names,
                                      study_trait_names,
                                      progress = FALSE) {
  all_traits <- unique(c(species_trait_names, study_trait_names))
  if (length(all_traits) == 0L) {
    stop(
      "No configured traits found in `candidate_models`. Supply species_traits and/or study_traits.",
      call. = FALSE
    )
  }

  n <- nrow(models_df)

  workflow_progress(
    progress,
    "  [Alchemist] Computing per-trait Gower matrices (",
    length(all_traits), " traits, ", n, " models)..."
  )
  trait_mats <- stats::setNames(
    lapply(all_traits, function(tr) alchemist_gower_col(models_df[[tr]])),
    paste0(".dist_", all_traits)
  )
  feature_cols <- names(trait_mats)

  slope_vals     <- suppressWarnings(as.numeric(models_df$slope_len))
  intercept_vals <- suppressWarnings(as.numeric(models_df$intercept_len))
  slope_vals[!is.finite(slope_vals)]         <- NA_real_
  intercept_vals[!is.finite(intercept_vals)] <- NA_real_

  len_min_col <- intersect(c("study_length_min", "length_minimum"), names(models_df))[[1]] %||% NULL
  len_max_col <- intersect(c("study_length_max", "length_maximum"), names(models_df))[[1]] %||% NULL

  anchor_pdfs <- lapply(seq_len(n), function(j) {
    alchemist_anchor_pdf(
      lo = if (!is.null(len_min_col)) models_df[[len_min_col]][[j]] else NA_real_,
      hi = if (!is.null(len_max_col)) models_df[[len_max_col]][[j]] else NA_real_
    )
  })
  n_valid_pdfs <- sum(!vapply(anchor_pdfs, is.null, logical(1)))

  workflow_progress(
    progress,
    "  [Alchemist] Building ", n, "x", n,
    " donor sigma matrix (", n_valid_pdfs, "/", n,
    " models have valid length ranges)..."
  )
  tick_at <- unique(floor(seq(0, n, length.out = 11L)))[-1L]
  donor_sigma_mat <- matrix(NA_real_, nrow = n, ncol = n)
  for (j in seq_len(n)) {
    pdf_j <- anchor_pdfs[[j]]
    if (is.null(pdf_j)) next
    donor_sigma_mat[, j] <- vapply(
      seq_len(n),
      function(i) equation_sigma_mean(slope_vals[[i]], intercept_vals[[i]], pdf_j),
      numeric(1)
    )
    if (progress && j %in% tick_at) {
      workflow_progress(
        progress,
        "  [Alchemist]   sigma matrix: model ", j, "/", n,
        " (", round(j / n * 100), "%)"
      )
    }
  }
  target_sigma <- diag(donor_sigma_mat)
  diag(donor_sigma_mat) <- NA_real_

  species_names <- as.character(models_df$species_name %||% rep(NA_character_, n))
  species_names[is.na(species_names)] <- NA_character_

  model_ids <- as.character(
    models_df$model_id_chr %||% models_df$model_id %||% as.character(seq_len(n))
  )

  workflow_progress(
    progress,
    "  [Alchemist] Assembling donor-anchor model pairs (excluding same-species)..."
  )
  rows <- vector("list", n * n)
  k <- 0L
  for (j in seq_len(n)) {
    ts_j <- target_sigma[[j]]
    if (!is.finite(ts_j) || ts_j <= 0) next
    for (i in seq_len(n)) {
      if (i == j) next
      if (!is.na(species_names[[i]]) && !is.na(species_names[[j]]) &&
        species_names[[i]] == species_names[[j]]) next
      ds_ij <- donor_sigma_mat[[i, j]]
      if (!is.finite(ds_ij) || ds_ij <= 0) next
      acoustic_dist <- abs(log(ds_ij) - log(ts_j))
      if (!is.finite(acoustic_dist)) next
      k <- k + 1L
      trait_row <- vapply(
        feature_cols,
        function(fc) trait_mats[[fc]][[i, j]],
        numeric(1)
      )
      rows[[k]] <- c(
        list(
          .anchor_idx    = j,
          .donor_idx     = i,
          .split_group   = species_names[[j]],
          .outcome       = acoustic_dist
        ),
        as.list(stats::setNames(trait_row, feature_cols))
      )
    }
  }
  rows <- rows[seq_len(k)]

  if (k == 0L) {
    stop("No valid donor-anchor pairs found for distance learning.", call. = FALSE)
  }

  workflow_progress(
    progress,
    "  [Alchemist] Binding ", k, " training pairs into tibble..."
  )
  training_data <- dplyr::bind_rows(lapply(rows, tibble::as_tibble))
  rm(rows)

  list(
    training_data       = training_data,
    feature_cols        = feature_cols,
    all_traits          = all_traits,
    species_trait_names = species_trait_names,
    n_models            = n,
    model_ids           = model_ids,
    donor_sigma_matrix  = donor_sigma_mat,
    target_sigma        = target_sigma,
    trait_mats          = trait_mats
  )
}

# ── Alchemist distance learner ────────────────────────────────────────────────

#' Resolve and validate Alchemist base learner method names
#'
#' Accepts a character vector of method names and returns only those that are
#' recognised by [alchemist_fit_base()]. Unrecognised names are silently
#' dropped with a warning.
#'
#' @param methods Character vector of requested method names.
#'
#' @return Character vector of validated method names, length >= 1.
#'
#' @keywords internal
alchemist_resolve_methods <- function(methods) {
  supported <- c(
    "glm",
    "glmnet_ridge", "glmnet_lasso", "glmnet_elasticnet",
    "gam",
    "rpart_shallow", "rpart_deep",
    "ranger_shallow", "ranger_deep",
    "xgboost_conservative", "xgboost_flexible"
  )
  methods <- as.character(methods)
  methods <- methods[nzchar(methods)]
  unknown <- setdiff(methods, supported)
  if (length(unknown) > 0L) {
    warning(
      "Ignoring unrecognised Alchemist methods: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  valid <- intersect(methods, supported)
  if (length(valid) == 0L) {
    valid <- c("glmnet_elasticnet", "ranger_shallow", "xgboost_conservative")
    warning(
      "No valid Alchemist methods supplied; falling back to: ",
      paste(valid, collapse = ", "),
      call. = FALSE
    )
  }
  valid
}

#' Build a numeric feature matrix from Alchemist training or prediction data
#'
#' All Gower distance features are numeric and bounded [0, 1]. Missing values
#' are imputed at 0.5 (the Gower midpoint for unknown comparisons).
#'
#' @param data Tibble or data frame containing at least the columns named by
#'   `feature_cols`.
#' @param feature_cols Character vector of Gower distance column names.
#'
#' @return A numeric matrix with `nrow(data)` rows and `length(feature_cols)`
#'   columns.
#'
#' @keywords internal
alchemist_feature_matrix <- function(data, feature_cols) {
  mat <- as.matrix(data[, feature_cols, drop = FALSE])
  mode(mat) <- "numeric"
  mat[!is.finite(mat)] <- 0.5
  mat
}

#' Derive per-family method settings from a shared settings list
#'
#' Extracts the settings block for one method family and applies any
#' variant-specific overrides when the method name has a `_variant` suffix.
#'
#' @param family Character. One of `"glm"`, `"glmnet"`, `"gam"`, `"rpart"`,
#'   `"ranger"`, `"xgboost"`.
#' @param method Character. Full method name, e.g. `"ranger_shallow"`.
#' @param method_settings Named list of per-family settings from config.
#'
#' @return Named list of resolved settings for this family+variant.
#'
#' @keywords internal
alchemist_method_settings <- function(family, method, method_settings) {
  ms <- as.list(method_settings[[family]] %||% list())
  variant <- sub(paste0("^", family, "_?"), "", method)
  if (nzchar(variant) && !is.null(ms$variants[[variant]])) {
    for (nm in names(ms$variants[[variant]])) {
      ms[[nm]] <- ms$variants[[variant]][[nm]]
    }
  }
  ms
}

#' Fit one Alchemist base learner on a training matrix
#'
#' Dispatches on `family` derived from `method`. The outcome vector `y_train`
#' must already be transformed (e.g. `log1p`-ed) before calling this function.
#'
#' @param x_train Numeric feature matrix (training rows).
#' @param y_train Numeric outcome vector (training rows), in transformed space.
#' @param method Character method name. See [alchemist_resolve_methods()] for
#'   the supported set.
#' @param method_settings Named list of per-family tuning settings.
#' @param seed Integer random seed.
#' @param lambda_rule Character. One of `"lambda.1se"` or `"lambda.min"`.
#'   Only used by glmnet-family methods.
#'
#' @return A list with class `"tsb_alchemist_base_learner"` and fields
#'   `fit`, `method`, `family`, `lambda_rule`.
#'
#' @keywords internal
alchemist_fit_base <- function(x_train, y_train, method,
                                method_settings = NULL,
                                seed = 42L,
                                lambda_rule = "lambda.1se") {
  set.seed(as.integer(seed))
  family <- if (grepl("^glmnet", method)) "glmnet"
  else if (grepl("^ranger",  method)) "ranger"
  else if (grepl("^rpart",   method)) "rpart"
  else if (grepl("^xgboost", method)) "xgboost"
  else method

  ms <- alchemist_method_settings(family, method, method_settings %||% list())

  fit <- switch(
    family,
    glm = {
      df_train <- as.data.frame(x_train)
      df_train$.y <- y_train
      stats::lm(.y ~ ., data = df_train)
    },
    glmnet = {
      alpha_val <- switch(method,
        glmnet_ridge       = 0,
        glmnet_lasso       = 1,
        glmnet_elasticnet  = as.numeric(ms$alpha %||% 0.25),
        0.25
      )
      glmnet::cv.glmnet(
        x            = x_train,
        y            = y_train,
        alpha        = alpha_val,
        nfolds       = 5L,
        standardize  = isTRUE(ms$standardize %||% TRUE),
        type.measure = ms$type_measure %||% "mse"
      )
    },
    gam = {
      df_train <- as.data.frame(x_train)
      df_train$.y <- y_train
      feat_nms <- setdiff(names(df_train), ".y")
      smooth_terms <- vapply(feat_nms, function(v) {
        n_unique <- length(unique(df_train[[v]][is.finite(df_train[[v]])]))
        k_val <- min(10L, max(3L, n_unique - 1L))
        sprintf("s(%s, bs='tp', k=%d)", v, k_val)
      }, character(1))
      gam_formula <- stats::as.formula(
        paste(".y ~", paste(smooth_terms, collapse = " + "))
      )
      mgcv::gam(
        formula    = gam_formula,
        data       = df_train,
        method     = ms$fit_method %||% "REML",
        select     = isTRUE(ms$select_terms %||% TRUE)
      )
    },
    ranger = {
      df_train <- as.data.frame(x_train)
      df_train$.y <- y_train
      ranger::ranger(
        .y ~ .,
        data                      = df_train,
        seed                      = as.integer(seed),
        num.threads               = 1L,
        verbose                   = FALSE,
        num.trees                 = as.integer(ms$num_trees %||% 500L),
        min.node.size             = as.integer(ms$min_node_size %||% 5L),
        sample.fraction           = as.numeric(ms$sample_fraction %||% 1.0),
        replace                   = isTRUE(ms$replace %||% TRUE),
        respect.unordered.factors = "order"
      )
    },
    rpart = {
      df_train <- as.data.frame(x_train)
      df_train$.y <- y_train
      rpart::rpart(
        .y ~ .,
        data    = df_train,
        control = rpart::rpart.control(
          cp        = as.numeric(ms$cp        %||% 0.01),
          minsplit  = as.integer(ms$minsplit  %||% 20L),
          minbucket = as.integer(ms$minbucket %||% 7L),
          maxdepth  = as.integer(ms$maxdepth  %||% 30L)
        )
      )
    },
    xgboost = {
      dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train)
      xgboost::xgboost(
        data             = dtrain,
        nrounds          = as.integer(ms$nrounds          %||% 100L),
        eta              = as.numeric(ms$eta              %||% 0.3),
        max_depth        = as.integer(ms$max_depth        %||% 6L),
        min_child_weight = as.numeric(ms$min_child_weight %||% 1),
        subsample        = as.numeric(ms$subsample        %||% 1.0),
        colsample_bytree = as.numeric(ms$colsample_bytree %||% 1.0),
        lambda           = as.numeric(ms$lambda           %||% 1.0),
        alpha            = as.numeric(ms$alpha            %||% 0.0),
        objective        = "reg:squarederror",
        verbose          = 0L,
        nthread          = 1L
      )
    },
    stop(sprintf("Unsupported Alchemist base learner: '%s'.", method), call. = FALSE)
  )

  structure(
    list(
      fit         = fit,
      method      = method,
      family      = family,
      lambda_rule = lambda_rule
    ),
    class = "tsb_alchemist_base_learner"
  )
}

#' Predict from a fitted Alchemist base learner
#'
#' Returns raw predictions in the same (transformed) space that the learner was
#' trained in. Back-transformation to the original outcome scale is the
#' responsibility of the caller.
#'
#' @param object A `"tsb_alchemist_base_learner"` from [alchemist_fit_base()].
#' @param x_new Numeric feature matrix (prediction rows).
#'
#' @return Numeric prediction vector, length `nrow(x_new)`.
#'
#' @keywords internal
alchemist_predict_base <- function(object, x_new) {
  if (!inherits(object, "tsb_alchemist_base_learner")) {
    stop("'object' must be a 'tsb_alchemist_base_learner'.", call. = FALSE)
  }
  df_new <- as.data.frame(x_new)
  switch(
    object$family,
    glm = {
      as.numeric(stats::predict(object$fit, newdata = df_new))
    },
    glmnet = {
      as.numeric(stats::predict(
        object$fit, newx = x_new, s = object$lambda_rule %||% "lambda.1se"
      ))
    },
    gam = {
      as.numeric(stats::predict(object$fit, newdata = df_new, type = "response"))
    },
    ranger = {
      as.numeric(stats::predict(object$fit, data = df_new)$predictions)
    },
    rpart = {
      as.numeric(stats::predict(object$fit, newdata = df_new))
    },
    xgboost = {
      dtest <- xgboost::xgb.DMatrix(data = x_new)
      as.numeric(stats::predict(object$fit, newdata = dtest))
    },
    stop(sprintf("Unsupported Alchemist base learner family: '%s'.", object$family),
         call. = FALSE)
  )
}

#' Run K-fold OOF predictions for one Alchemist base learner method
#'
#' This is the per-method worker function that parallel workers execute. Each
#' worker runs all K folds for its assigned method and returns a named list
#' with the OOF prediction vector and any error captured.
#'
#' @param method Character. Single base learner method name.
#' @param x_all Numeric feature matrix for the full training set.
#' @param y_all Numeric outcome vector (transformed), full training set.
#' @param foldid Integer fold-ID vector, length `nrow(x_all)`.
#' @param method_settings Named list of per-family settings.
#' @param seed Integer base seed. Each fold offsets this by its fold index.
#' @param lambda_rule Character. Lambda selection rule for glmnet methods.
#'
#' @return A named list with fields `method`, `oof_pred` (numeric vector or
#'   `NULL`), and `error` (character or `NULL`).
#'
#' @keywords internal
alchemist_run_oof_method <- function(method, x_all, y_all, foldid,
                                      method_settings, seed, lambda_rule) {
  n         <- length(y_all)
  oof_pred  <- rep(NA_real_, n)
  ok        <- TRUE
  err_msg   <- NULL

  for (fold_now in sort(unique(foldid))) {
    train_idx <- which(foldid != fold_now)
    valid_idx <- which(foldid == fold_now)
    if (length(train_idx) == 0L || length(valid_idx) == 0L) {
      ok <- FALSE
      break
    }

    learner <- tryCatch(
      alchemist_fit_base(
        x_train         = x_all[train_idx, , drop = FALSE],
        y_train         = y_all[train_idx],
        method          = method,
        method_settings = method_settings,
        seed            = as.integer(seed) + as.integer(fold_now),
        lambda_rule     = lambda_rule
      ),
      error = function(e) structure(conditionMessage(e), class = "try-error")
    )

    if (inherits(learner, "try-error")) {
      ok      <- FALSE
      err_msg <- as.character(learner)
      break
    }

    preds <- tryCatch(
      alchemist_predict_base(learner, x_all[valid_idx, , drop = FALSE]),
      error = function(e) structure(conditionMessage(e), class = "try-error")
    )

    if (inherits(preds, "try-error") || length(preds) != length(valid_idx)) {
      ok      <- FALSE
      err_msg <- if (inherits(preds, "try-error")) as.character(preds) else
        "Prediction length mismatch."
      break
    }

    oof_pred[valid_idx] <- preds
  }

  list(
    method   = method,
    oof_pred = if (ok && all(is.finite(oof_pred))) oof_pred else NULL,
    error    = err_msg
  )
}

#' Fit a Super Learner ensemble for Alchemist pairwise distance learning
#'
#' Trains a stacked ensemble over a library of base learners using K-fold
#' out-of-fold (OOF) cross-validation. Base learner fits are parallelised
#' across methods when `workers > 1`. NNLS stacking weights are computed from
#' the OOF predictions; a final round of base learner fits on the full training
#' set is then used for prediction at inference time.
#'
#' This function is entirely independent of the policy-learner path in
#' `meta_policy.R`. It is purpose-built for learning pairwise acoustic
#' distances from Gower trait features.
#'
#' @param training_data Tibble produced by [build_alchemist_pair_data()].
#'   Must contain `.outcome`, optionally `.split_group`, and all columns named
#'   by `feature_cols`.
#' @param feature_cols Character vector of Gower distance column names
#'   (e.g. `.dist_swimbladder_type`).
#' @param methods Character vector of base learner method names. See
#'   [alchemist_resolve_methods()] for the supported set.
#' @param outcome_transform One of `"log1p"` or `"identity"`. Applied to
#'   `.outcome` before fitting; predictions are back-transformed before
#'   storage.
#' @param lambda_rule Lambda selection rule for glmnet-family base learners.
#'   One of `"lambda.1se"` or `"lambda.min"`.
#' @param inner_folds Integer number of cross-validation folds.
#' @param seed Integer random seed for fold assignment and base learner fits.
#' @param method_settings Named list of per-family tuning overrides (same
#'   structure as `metalearner.method_settings` in the workflow YAML).
#' @param workers Integer number of parallel workers. Workers are assigned one
#'   method each. `1L` runs sequentially.
#' @param progress Logical. Emit [tsb_message()] progress lines when `TRUE`.
#'
#' @return A list with class `"tsb_alchemist_learner"` containing:
#'   \describe{
#'     \item{`fit`}{Named list of final base learner objects
#'       (`"tsb_alchemist_base_learner"`).}
#'     \item{`weights`}{Named numeric vector of NNLS ensemble weights.}
#'     \item{`feature_cols`}{Character vector of feature column names.}
#'     \item{`outcome_transform`}{The transform applied during training.}
#'     \item{`oof_ensemble_prediction`}{Numeric vector of OOF ensemble
#'       predictions in the **original** (back-transformed) scale.}
#'     \item{`oof_predictions`}{Tibble of per-method OOF predictions in
#'       original scale.}
#'     \item{`oof_performance`}{Tibble with `method`, `rmse`, and `mae`
#'       columns.}
#'   }
#'
#' @keywords internal
fit_alchemist_learner <- function(training_data,
                                   feature_cols,
                                   methods,
                                   outcome_transform = "log1p",
                                   lambda_rule       = "lambda.1se",
                                   inner_folds       = 5L,
                                   seed              = 42L,
                                   method_settings   = NULL,
                                   workers           = 1L,
                                   progress          = FALSE) {
  training_data <- tibble::as_tibble(training_data)
  methods       <- alchemist_resolve_methods(methods)
  inner_folds   <- as.integer(inner_folds)
  seed          <- as.integer(seed)
  workers       <- as.integer(workers)

  x_all <- alchemist_feature_matrix(training_data, feature_cols)
  y_raw <- training_data$.outcome
  y_all <- if (identical(outcome_transform, "log1p")) log1p(y_raw) else y_raw

  foldid <- if (".split_group" %in% names(training_data)) {
    grouped_foldid(training_data$.split_group, n_folds = inner_folds, seed = seed)
  } else {
    row_foldid(nrow(training_data), n_folds = inner_folds, seed = seed)
  }
  if (is.null(foldid)) {
    stop(
      "Alchemist learner requires at least two training rows for CV.",
      call. = FALSE
    )
  }

  workflow_progress(
    progress,
    "[Alchemist] Fitting ", length(methods), " base learner(s) with ",
    inner_folds, "-fold CV",
    if (workers > 1L) paste0(" across ", workers, " workers") else " (sequential)",
    "..."
  )

  # ---- OOF phase: parallelise across methods --------------------------------
  if (workers > 1L && length(methods) > 1L) {
    cl <- initialize_parallel_cluster(workers = min(workers, length(methods)))
    if (!is.null(cl)) {
      on.exit(parallel::stopCluster(cl), add = TRUE)
      oof_results <- parallel::parLapplyLB(
        cl, methods,
        fun = alchemist_run_oof_method,
        x_all           = x_all,
        y_all           = y_all,
        foldid          = foldid,
        method_settings = method_settings,
        seed            = seed,
        lambda_rule     = lambda_rule
      )
    } else {
      oof_results <- lapply(
        methods, alchemist_run_oof_method,
        x_all           = x_all,
        y_all           = y_all,
        foldid          = foldid,
        method_settings = method_settings,
        seed            = seed,
        lambda_rule     = lambda_rule
      )
    }
  } else {
    oof_results <- lapply(
      methods, alchemist_run_oof_method,
      x_all           = x_all,
      y_all           = y_all,
      foldid          = foldid,
      method_settings = method_settings,
      seed            = seed,
      lambda_rule     = lambda_rule
    )
  }
  names(oof_results) <- methods

  # ---- Collect successful OOF predictions ----------------------------------
  ok_methods <- Filter(function(r) !is.null(r$oof_pred), oof_results)
  if (length(ok_methods) == 0L) {
    failed <- vapply(oof_results, function(r) r$error %||% "unknown error", character(1))
    stop(
      "No Alchemist base learners produced complete OOF predictions.\n",
      paste(names(failed), failed, sep = ": ", collapse = "\n"),
      call. = FALSE
    )
  }

  oof_mat_transformed <- do.call(cbind, lapply(ok_methods, `[[`, "oof_pred"))
  colnames(oof_mat_transformed) <- names(ok_methods)

  # Back-transform OOF predictions for reporting and downstream use
  oof_mat <- if (identical(outcome_transform, "log1p")) {
    apply(oof_mat_transformed, 2, function(p) pmax(0, expm1(p)))
  } else {
    apply(oof_mat_transformed, 2, pmax, 0)
  }

  weights <- fit_super_learner_weights(oof_mat_transformed, y_all)
  names(weights) <- colnames(oof_mat_transformed)

  oof_ensemble_transformed <- as.numeric(
    oof_mat_transformed[, names(weights), drop = FALSE] %*% weights
  )
  oof_ensemble_pred <- if (identical(outcome_transform, "log1p")) {
    pmax(0, expm1(oof_ensemble_transformed))
  } else {
    pmax(0, oof_ensemble_transformed)
  }

  workflow_progress(
    progress,
    "[Alchemist] OOF ensemble RMSE: ",
    round(sqrt(mean((oof_ensemble_pred - y_raw)^2, na.rm = TRUE)), 4),
    " (", length(ok_methods), "/", length(methods), " methods succeeded)"
  )

  # ---- Final refit on full training set ------------------------------------
  workflow_progress(progress, "[Alchemist] Refitting base learners on full data...")
  final_learners <- list()
  for (m in names(weights)) {
    fit_obj <- tryCatch(
      alchemist_fit_base(
        x_train         = x_all,
        y_train         = y_all,
        method          = m,
        method_settings = method_settings,
        seed            = seed,
        lambda_rule     = lambda_rule
      ),
      error = function(e) NULL
    )
    if (!is.null(fit_obj)) final_learners[[m]] <- fit_obj
  }

  weights <- weights[names(final_learners)]
  if (length(final_learners) == 0L || length(weights) == 0L) {
    stop("No Alchemist base learners could be refit on the full training data.", call. = FALSE)
  }
  weights <- weights / sum(weights)

  # ---- Performance table ---------------------------------------------------
  perf_tbl <- dplyr::bind_rows(
    lapply(colnames(oof_mat), function(m) {
      p <- oof_mat[, m]
      tibble::tibble(
        method = m,
        rmse   = sqrt(mean((p - y_raw)^2, na.rm = TRUE)),
        mae    = mean(abs(p - y_raw), na.rm = TRUE),
        weight = weights[m] %||% NA_real_
      )
    }),
    tibble::tibble(
      method = "ensemble",
      rmse   = sqrt(mean((oof_ensemble_pred - y_raw)^2, na.rm = TRUE)),
      mae    = mean(abs(oof_ensemble_pred - y_raw), na.rm = TRUE),
      weight = 1
    )
  )

  structure(
    list(
      fit                    = final_learners,
      weights                = weights,
      feature_cols           = feature_cols,
      outcome_transform      = outcome_transform,
      lambda_rule            = lambda_rule,
      inner_foldid           = foldid,
      oof_predictions        = tibble::as_tibble(oof_mat),
      oof_ensemble_prediction = oof_ensemble_pred,
      oof_performance        = perf_tbl
    ),
    class = "tsb_alchemist_learner"
  )
}

#' Generate predictions from a fitted Alchemist ensemble
#'
#' Applies each base learner in the ensemble to `new_data`, combines the
#' predictions using the stored NNLS weights, and back-transforms the result to
#' the original outcome scale.
#'
#' @param object A `"tsb_alchemist_learner"` object from
#'   [fit_alchemist_learner()].
#' @param new_data Tibble or data frame containing at least the columns named by
#'   `object$feature_cols`.
#'
#' @return Numeric prediction vector, length `nrow(new_data)`, in the original
#'   (non-transformed) scale.
#'
#' @keywords internal
predict_alchemist_score <- function(object, new_data) {
  if (!inherits(object, "tsb_alchemist_learner")) {
    stop("'object' must be a 'tsb_alchemist_learner'.", call. = FALSE)
  }
  new_data <- tibble::as_tibble(new_data)
  x_new    <- alchemist_feature_matrix(new_data, object$feature_cols)
  weights  <- object$weights

  pred_mat <- vapply(names(object$fit), function(m) {
    alchemist_predict_base(object$fit[[m]], x_new)
  }, numeric(nrow(x_new)))

  if (is.null(dim(pred_mat))) {
    dim(pred_mat) <- c(nrow(x_new), length(names(object$fit)))
    colnames(pred_mat) <- names(object$fit)
  }

  pred_transformed <- as.numeric(pred_mat[, names(weights), drop = FALSE] %*% weights)
  pmax(0, if (identical(object$outcome_transform, "log1p")) expm1(pred_transformed)
          else pred_transformed)
}

# ── forge_distances ────────────────────────────────────────────────────────────

S7::method(forge_distances, Alchemist) <- function(object, progress = NULL, ...) {
  config   <- object@config
  progress <- progress %||% config$progress %||% FALSE

  models_df <- tibble::as_tibble(object@candidates@candidate_models)
  n_models  <- nrow(models_df)
  if (n_models < 3L) {
    stop("At least 3 candidate models are required for distance learning.", call. = FALSE)
  }

  sp_names  <- alchemist_trait_names(config$species_traits %||% list(), models_df)
  st_names  <- alchemist_trait_names(config$study_traits   %||% list(), models_df)

  workflow_progress(
    progress,
    "[Alchemist] forge_distances: ", n_models, " candidate models | ",
    length(sp_names), " species traits [",
    paste(sp_names, collapse = ", "), "] | ",
    length(st_names), " study traits [",
    paste(st_names, collapse = ", "), "]"
  )

  workflow_progress(
    progress,
    "[Alchemist] Stage 1/4: Building pairwise training data ",
    "(up to ~", n_models * (n_models - 1L), " cross-species pairs)..."
  )
  pair_data <- build_alchemist_pair_data(models_df, sp_names, st_names, progress = progress)
  n_pairs   <- nrow(pair_data$training_data)

  learner_cfg <- config$learner %||% list()
  methods_lbl <- learner_cfg$methods %||% NULL
  folds_lbl   <- as.integer(learner_cfg$inner_folds %||% 5L)
  workers_lbl <- as.integer(learner_cfg$workers %||% 1L)

  workflow_progress(
    progress,
    "[Alchemist] Stage 2/4: Fitting Alchemist ensemble on ", n_pairs,
    " pairs (", folds_lbl, "-fold CV, ", workers_lbl, " worker(s))..."
  )

  sl_fit <- fit_alchemist_learner(
    training_data     = pair_data$training_data,
    feature_cols      = pair_data$feature_cols,
    methods           = methods_lbl %||% c(
      "glmnet_elasticnet", "ranger_shallow", "xgboost_conservative"
    ),
    outcome_transform = learner_cfg$outcome_transform %||% "identity",
    lambda_rule       = learner_cfg$lambda_rule       %||% "lambda.1se",
    inner_folds       = folds_lbl,
    seed              = as.integer(learner_cfg$seed   %||% 42L),
    method_settings   = learner_cfg$method_settings   %||% NULL,
    workers           = workers_lbl,
    progress          = progress
  )

  # Reconstruct N×N matrix: use OOF predictions for honest distance estimates
  workflow_progress(
    progress,
    "[Alchemist] Stage 3/4: Reconstructing ", n_models, "x", n_models,
    " distance matrix from OOF predictions..."
  )
  n         <- pair_data$n_models
  model_ids <- pair_data$model_ids
  dist_mat  <- matrix(NA_real_, nrow = n, ncol = n)
  rownames(dist_mat) <- model_ids
  colnames(dist_mat) <- model_ids
  diag(dist_mat) <- 0

  oof_preds <- sl_fit$oof_ensemble_prediction %||%
    predict_alchemist_score(sl_fit, pair_data$training_data)
  oof_preds <- pmax(0, oof_preds)

  pair_rows <- pair_data$training_data
  for (row_k in seq_len(nrow(pair_rows))) {
    i_idx <- pair_rows$.donor_idx[[row_k]]
    j_idx <- pair_rows$.anchor_idx[[row_k]]
    dist_mat[[i_idx, j_idx]] <- oof_preds[[row_k]]
  }

  # Fill any remaining NAs (same-species or missing-PDF pairs) with the
  # column max so they sort to the far end of the distance ordering
  col_max <- apply(dist_mat, 2, function(col) max(col[is.finite(col)], na.rm = TRUE))
  col_max[!is.finite(col_max)] <- 1
  for (j in seq_len(n)) {
    na_idx <- which(is.na(dist_mat[, j]))
    dist_mat[na_idx, j] <- col_max[[j]]
  }

  workflow_progress(
    progress,
    "[Alchemist] Stage 4/4: Symmetrizing distance matrix..."
  )
  sym_mat <- (dist_mat + t(dist_mat)) / 2
  diag(sym_mat) <- 0

  dist_range <- round(range(sym_mat[sym_mat > 0], na.rm = TRUE), 4)
  workflow_progress(
    progress,
    "[Alchemist] forge_distances complete. Distance range: [",
    dist_range[[1]], ", ", dist_range[[2]], "]"
  )

  distance_matrix <- list(
    combined_dist       = stats::as.dist(sym_mat),
    dist_matrix         = sym_mat,
    model_ids           = model_ids,
    all_traits          = pair_data$all_traits,
    species_trait_names = sp_names,
    trait_cols          = pair_data$all_traits,
    oof_performance     = sl_fit$oof_performance %||% tibble::tibble(),
    pair_data           = pair_data$training_data,
    feature_cols        = pair_data$feature_cols,
    trait_mats          = pair_data$trait_mats
  )

  object <- alchemist_rebuild(object, distance_matrix = distance_matrix)
  alchemist_rebuild(object, learner = sl_fit)
}

# ── distill_traits helpers ────────────────────────────────────────────────────

#' Run permutation importance for one trait column
#'
#' Shuffles `fc` in `pair_data` `n_permutations` times, predicts with the
#' fitted ensemble after each shuffle, and returns the mean and SD of the
#' resulting RMSE increases relative to `baseline_rmse`. This is the per-trait
#' worker function used by [distill_traits()] in both sequential and parallel
#' execution paths.
#'
#' @param fc Character. Name of the feature column to permute
#'   (e.g. `".dist_swimbladder_type"`).
#' @param pair_data Tibble of training pairs produced by
#'   [build_alchemist_pair_data()]. Must contain `.outcome` and `fc`.
#' @param y Numeric vector of observed outcomes (`pair_data$.outcome`). Passed
#'   explicitly to avoid re-extracting it inside each worker.
#' @param sl_fit A `"tsb_alchemist_learner"` object from
#'   [fit_alchemist_learner()].
#' @param baseline_rmse Numeric. RMSE of the ensemble on the unshuffled data.
#'   Used to compute the delta for each permutation.
#' @param n_permutations Integer. Number of independent shuffles per trait.
#' @param seed Integer. Base random seed. The trait's position index is added
#'   to this value so each trait gets a unique, reproducible seed sequence.
#' @param trait_idx Integer. Index of this trait in the full feature list, used
#'   only to offset `seed` for reproducibility.
#'
#' @return A [tibble::tibble()] with one row and columns `trait`,
#'   `feature_col`, `delta_rmse`, `sd_delta_rmse`.
#'
#' @keywords internal
alchemist_permute_trait <- function(fc, pair_data, y, sl_fit,
                                     baseline_rmse, n_permutations,
                                     seed, trait_idx) {
  trait_name <- sub("^\\.dist_", "", fc)
  set.seed(as.integer(seed) + as.integer(trait_idx))
  delta_rmse_vals <- vapply(seq_len(n_permutations), function(perm_i) {
    perm_data       <- pair_data
    perm_data[[fc]] <- sample(pair_data[[fc]])
    perm_pred       <- predict_alchemist_score(sl_fit, perm_data)
    perm_rmse       <- sqrt(mean((perm_pred - y)^2, na.rm = TRUE))
    perm_rmse - baseline_rmse
  }, numeric(1))
  tibble::tibble(
    trait         = trait_name,
    feature_col   = fc,
    delta_rmse    = mean(delta_rmse_vals),
    sd_delta_rmse = stats::sd(delta_rmse_vals)
  )
}

# ── distill_traits ─────────────────────────────────────────────────────────────

S7::method(distill_traits, Alchemist) <- function(object, n_permutations = 10L, seed = NULL,
                                                   workers = NULL, progress = NULL, ...) {
  if (length(object@learner) == 0L) {
    stop("Run `forge_distances()` before `distill_traits()`.", call. = FALSE)
  }
  if (length(object@distance_matrix) == 0L) {
    stop("Run `forge_distances()` before `distill_traits()`.", call. = FALSE)
  }

  progress <- progress %||% object@config$progress %||% FALSE
  workers  <- as.integer(workers %||% object@config$learner$workers %||% 1L)
  seed     <- as.integer(seed %||% object@config$learner$seed %||% 42L)

  sl_fit       <- object@learner
  pair_data    <- object@distance_matrix$pair_data
  feature_cols <- object@distance_matrix$feature_cols
  n_traits     <- length(feature_cols)
  n_pairs      <- nrow(pair_data)
  y            <- pair_data$.outcome

  workflow_progress(
    progress,
    "[Alchemist] distill_traits: permutation importance over ",
    n_traits, " traits x ", n_permutations, " permutations (",
    n_pairs, " pairs each)",
    if (workers > 1L) paste0(", ", min(workers, n_traits), " workers") else "",
    "..."
  )

  baseline <- sl_fit$oof_ensemble_prediction %||%
    predict_alchemist_score(sl_fit, pair_data)
  baseline_rmse <- sqrt(mean((baseline - y)^2, na.rm = TRUE))

  workflow_progress(
    progress,
    "[Alchemist] Baseline OOF RMSE: ", round(baseline_rmse, 4)
  )

  # ---- Dispatch: parallel across traits, or sequential with per-trait ticks -
  if (workers > 1L && n_traits > 1L) {
    workflow_progress(
      progress,
      "[Alchemist] Launching ", min(workers, n_traits),
      " parallel workers for trait permutation..."
    )
    cl <- initialize_parallel_cluster(workers = min(workers, n_traits))
    if (!is.null(cl)) {
      on.exit(parallel::stopCluster(cl), add = TRUE)
      importance_rows <- parallel::parLapplyLB(
        cl, seq_along(feature_cols),
        fun = function(fc_idx) {
          alchemist_permute_trait(
            fc            = feature_cols[[fc_idx]],
            pair_data     = pair_data,
            y             = y,
            sl_fit        = sl_fit,
            baseline_rmse = baseline_rmse,
            n_permutations = n_permutations,
            seed          = seed,
            trait_idx     = fc_idx
          )
        }
      )
    } else {
      importance_rows <- lapply(seq_along(feature_cols), function(fc_idx) {
        workflow_progress(
          progress,
          "[Alchemist]   Permuting trait ", fc_idx, "/", n_traits, ": ",
          sub("^\\.dist_", "", feature_cols[[fc_idx]]), "..."
        )
        alchemist_permute_trait(
          fc            = feature_cols[[fc_idx]],
          pair_data     = pair_data,
          y             = y,
          sl_fit        = sl_fit,
          baseline_rmse = baseline_rmse,
          n_permutations = n_permutations,
          seed          = seed,
          trait_idx     = fc_idx
        )
      })
    }
  } else {
    importance_rows <- lapply(seq_along(feature_cols), function(fc_idx) {
      workflow_progress(
        progress,
        "[Alchemist]   Permuting trait ", fc_idx, "/", n_traits, ": ",
        sub("^\\.dist_", "", feature_cols[[fc_idx]]), "..."
      )
      alchemist_permute_trait(
        fc            = feature_cols[[fc_idx]],
        pair_data     = pair_data,
        y             = y,
        sl_fit        = sl_fit,
        baseline_rmse = baseline_rmse,
        n_permutations = n_permutations,
        seed          = seed,
        trait_idx     = fc_idx
      )
    })
  }

  importance_tbl <- dplyr::bind_rows(importance_rows) |>
    dplyr::arrange(dplyr::desc(delta_rmse)) |>
    dplyr::mutate(
      importance_score = pmax(0, delta_rmse),
      importance_pct   = importance_score / max(sum(importance_score, na.rm = TRUE), 1e-12)
    )

  sp_names <- object@distance_matrix$species_trait_names %||% character(0)
  importance_tbl$trait_set <- ifelse(
    importance_tbl$trait %in% sp_names,
    "species",
    "survey"
  )

  sp_total    <- sum(importance_tbl$importance_score[importance_tbl$trait_set == "species"], na.rm = TRUE)
  all_total   <- sum(importance_tbl$importance_score, na.rm = TRUE)
  alpha_equiv <- if (all_total > 0) sp_total / all_total else 0.5

  workflow_progress(
    progress,
    "[Alchemist] distill_traits complete. alpha-equivalent = ",
    round(alpha_equiv, 3),
    " (species: ", round(sp_total / max(all_total, 1e-12) * 100, 1), "%, ",
    "survey: ", round((all_total - sp_total) / max(all_total, 1e-12) * 100, 1), "%)"
  )

  trait_importance <- list(
    importance_tbl  = importance_tbl,
    alpha_equiv     = alpha_equiv,
    species_total   = sp_total,
    survey_total    = all_total - sp_total,
    baseline_rmse   = baseline_rmse,
    n_permutations  = as.integer(n_permutations)
  )

  alchemist_rebuild(object, trait_importance = trait_importance)
}

# ── run_ordination dispatch ────────────────────────────────────────────────────

#' @keywords internal
.run_ordination_alchemist <- function(alchemist,
                                      nmds_args        = NULL,
                                      include_loadings = NULL,
                                      include_centroids = NULL,
                                      envfit_args      = NULL,
                                      reference_ids    = NULL,
                                      join_cols        = c(
                                        "species_name", "common",
                                        "swimbladder_type", "family",
                                        "regional_body", "is_group_model"
                                      ),
                                      cluster_args     = list(),
                                      model_id_col     = "model_id",
                                      progress         = NULL) {
  config   <- alchemist@config
  progress <- progress %||% config$progress %||% FALSE

  if (length(alchemist@distance_matrix) == 0L) {
    stop("Run `forge_distances()` before `run_ordination()`.", call. = FALSE)
  }

  workflow_cfg     <- config$workflow_config %||% list()
  ordination_cfg   <- workflow_cfg$ordination %||% list()
  nmds_args        <- nmds_args        %||% ordination_cfg$nmds_args        %||% list()
  envfit_args      <- envfit_args      %||% ordination_cfg$envfit_args      %||% list()
  include_loadings <- include_loadings %||% ordination_cfg$include_loadings %||% FALSE
  include_centroids <- include_centroids %||% ordination_cfg$include_centroids %||% FALSE

  workflow_progress(progress, "Running Alchemist ordination.")

  combined_dist    <- alchemist@distance_matrix$combined_dist
  candidate_models <- tibble::as_tibble(alchemist@candidates@candidate_models)
  all_traits       <- alchemist@distance_matrix$all_traits %||% character(0)
  trait_cols       <- intersect(all_traits, names(candidate_models))

  model_trait_table <- if (length(trait_cols) > 0) {
    dplyr::select(candidate_models, dplyr::all_of(trait_cols))
  } else {
    NULL
  }

  if (is.null(reference_ids) && nrow(alchemist@candidates@reference_anchors) > 0) {
    anch <- alchemist@candidates@reference_anchors
    reference_ids <- if ("model_id_chr" %in% names(anch)) {
      as.character(anch$model_id_chr)
    } else if ("model_id" %in% names(anch)) {
      as.character(anch$model_id)
    } else {
      NULL
    }
  }

  model_cluster_col <- as.character(cluster_args$cluster_col %||% "nmds_cluster_id")

  model_ord <- run_ordination(
    dist_mat          = combined_dist,
    trait_table       = model_trait_table,
    nmds_args         = nmds_args,
    include_loadings  = include_loadings,
    include_centroids = include_centroids,
    envfit_args       = envfit_args
  )

  model_points <- join_ordination_points(
    ordination_points = model_ord$points,
    candidate_models  = candidate_models,
    reference_ids     = reference_ids,
    model_id_col      = model_id_col,
    join_cols         = join_cols,
    cluster_args      = cluster_args
  )

  model_scores <- extract_ordination_scores(
    points_df   = model_points,
    cluster_col = model_cluster_col
  )

  ordination <- list(
    model = c(model_ord, list(points = model_points, scores = model_scores))
  )

  alchemist_rebuild(alchemist, ordination = ordination)
}

# ── screen_admissibility dispatch ──────────────────────────────────────────────

#' @keywords internal
.screen_admissibility_alchemist <- function(alchemist,
                                             config     = NULL,
                                             cache_path = NULL,
                                             refresh    = NULL,
                                             progress   = NULL,
                                             registry_path = NULL) {
  if (length(alchemist@distance_matrix) == 0L) {
    stop("Run `forge_distances()` before `screen_admissibility()`.", call. = FALSE)
  }

  dm <- alchemist@distance_matrix

  gower_bundle <- list(
    combined_dist     = dm$combined_dist,
    species_dist      = dm$combined_dist,
    study_dist        = as.matrix(dm$dist_matrix),
    species_dist_model = as.matrix(dm$dist_matrix),
    trait_cols        = dm$trait_cols %||% dm$all_traits %||% character(0)
  )

  injected_candidates <- candidates_with_gower_distances(
    alchemist@candidates,
    gower_bundle
  )

  result <- screen_admissibility(
    candidate_models = injected_candidates,
    config           = config %||% alchemist@config$workflow_config %||% NULL,
    cache_path       = cache_path,
    refresh          = refresh,
    progress         = progress %||% alchemist@config$progress %||% FALSE,
    registry_path    = registry_path %||% alchemist@config$registry_path %||% NULL
  )

  admissibility_result <- if (.is_candidates_obj(result)) {
    result@admissibility
  } else {
    result
  }

  alchemist_rebuild(alchemist, admissibility = admissibility_result)
}

# ── as_selector ────────────────────────────────────────────────────────────────

#' Coerce a staged object to a PolicySelector
#'
#' Dispatches on the class of `object`:
#' - [Candidates] objects are passed directly to [as_policy_selector()].
#' - [Alchemist] objects inject the learned distance matrix into a `Candidates`
#'   shell before constructing the selector, so all downstream benchmark and
#'   prediction steps operate on the Alchemist distances transparently.
#'
#' @param object A [Candidates] or [Alchemist] object.
#' @param config Optional selector config list or [Configurer] object.
#' @param ... Unused.
#'
#' @return A [PolicySelector] object.
#'
#' @export
as_selector <- function(object, config = NULL, ...) {
  if (.is_alchemist(object)) {
    dm <- object@distance_matrix
    if (length(dm) == 0L) {
      stop("Run `forge_distances()` before `as_selector()`.", call. = FALSE)
    }

    gower_bundle <- list(
      combined_dist      = dm$combined_dist,
      species_dist       = dm$combined_dist,
      study_dist         = as.matrix(dm$dist_matrix),
      species_dist_model = as.matrix(dm$dist_matrix),
      trait_cols         = dm$trait_cols %||% dm$all_traits %||% character(0)
    )

    candidates_obj <- candidates_with_gower_distances(object@candidates, gower_bundle)

    if (length(object@admissibility) > 0L) {
      candidates_obj <- candidates_with_admissibility(candidates_obj, object@admissibility)
    }
    if (length(object@ordination) > 0L) {
      candidates_obj <- candidates_with_ordination(candidates_obj, object@ordination)
    }

    return(as_policy_selector(candidates_obj, config = config))
  }

  if (.is_candidates_obj(object)) {
    return(as_policy_selector(object, config = config))
  }

  stop("'object' must be a `Candidates` or `Alchemist`.", call. = FALSE)
}

# ── print / show ───────────────────────────────────────────────────────────────

S7::method(print_generic, Alchemist) <- function(x, ...) {
  cat("Alchemist\n")
  cat("  candidates:       ", nrow(x@candidates@candidate_models), " models\n", sep = "")
  cat("  species_traits:   ", length(alchemist_trait_names(x@config$species_traits %||% list(), x@candidates@candidate_models)), "\n", sep = "")
  cat("  study_traits:     ", length(alchemist_trait_names(x@config$study_traits   %||% list(), x@candidates@candidate_models)), "\n", sep = "")
  cat("  learner_ready:    ", if (length(x@learner) > 0L) "yes" else "no", "\n", sep = "")
  cat("  distances_ready:  ", if (length(x@distance_matrix) > 0L) "yes" else "no", "\n", sep = "")
  cat("  importance_ready: ", if (length(x@trait_importance) > 0L) "yes" else "no", "\n", sep = "")
  cat("  ordination_ready: ", if (length(x@ordination) > 0L) "yes" else "no", "\n", sep = "")
  cat("  admiss_ready:     ", if (length(x@admissibility) > 0L) "yes" else "no", "\n", sep = "")
  invisible(x)
}

S7::method(show_generic, Alchemist) <- function(object) {
  print(object)
  invisible(object)
}
