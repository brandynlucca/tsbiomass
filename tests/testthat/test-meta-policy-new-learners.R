# Smoke tests for the learners added to the meta-policy Super Learner library:
# the stochtree BART family (bart/xbart/wsbart/vfbart/rebart), KNN, Cubist, SVR,
# the quantile regression forest, and Gaussian-process regression. Each learner
# is fit and used to predict on toy data, and is skipped when its optional
# backend package is unavailable.

new_learner_training_data <- function(n = 60L, seed = 20260707L) {
  set.seed(seed)
  tibble::tibble(
    .outcome = abs(stats::rnorm(n)) + 0.05,
    feature_a = stats::rnorm(n),
    feature_b = stats::runif(n),
    feature_c = factor(sample(c("x", "y", "z"), n, replace = TRUE)),
    .split_group = sample(paste0("g", seq_len(6L)), n, replace = TRUE)
  )
}

expect_new_learner_fits <- function(method) {
  training_data <- new_learner_training_data()
  learner <- NULL
  utils::capture.output(
    learner <- suppressMessages(suppressWarnings(
      fit_meta_policy_learner(
        training_data = training_data,
        method = method,
        feature_cols = c("feature_a", "feature_b", "feature_c"),
        inner_folds = 3L,
        seed = 20260707L
      )
    )),
    type = "output"
  )
  expect_s3_class(learner, "tsb_meta_policy_learner")
  expect_identical(learner$method, method)

  scored <- NULL
  utils::capture.output(
    scored <- suppressMessages(
      suppressWarnings(predict_meta_policy_score(learner, training_data))
    ),
    type = "output"
  )
  expect_length(scored$.meta_predicted_score, nrow(training_data))
  expect_true(all(is.finite(scored$.meta_predicted_score)))
}

test_that("stochtree BART learners fit and predict", {
  testthat::skip_if_not_installed("stochtree")
  for (method_now in c("bart", "xbart", "wsbart", "vfbart", "rebart")) {
    expect_new_learner_fits(method_now)
  }
})

test_that("stochtree BART learners predict after RDS reload", {
  testthat::skip_if_not_installed("stochtree")
  training_data <- new_learner_training_data(n = 40L)
  method_settings <- list(
    vfbart = list(
      num_trees = 5L,
      num_gfr = 0L,
      num_burnin = 5L,
      num_mcmc = 5L,
      variance_forest = TRUE,
      variance_forest_num_trees = 2L,
      keep_gfr = FALSE,
      random_effects = FALSE
    )
  )

  learner <- suppressWarnings(fit_meta_policy_learner(
    training_data = training_data,
    method = "vfbart",
    feature_cols = c("feature_a", "feature_b", "feature_c"),
    outcome_transform = "identity",
    method_settings = method_settings,
    seed = 20260707L
  ))
  expect_true(is.character(learner$fit_json))
  expect_true(nzchar(learner$fit_json))

  path <- tempfile(fileext = ".rds")
  saveRDS(learner, path)
  reloaded <- readRDS(path)
  scored <- predict_meta_policy_score(reloaded, training_data)

  expect_length(scored$.meta_predicted_score, nrow(training_data))
  expect_true(all(is.finite(scored$.meta_predicted_score)))
})

test_that("KNN meta-policy learner fits and predicts", {
  testthat::skip_if_not_installed("FNN")
  expect_new_learner_fits("knn")
})

test_that("Cubist meta-policy learner fits and predicts", {
  testthat::skip_if_not_installed("Cubist")
  expect_new_learner_fits("cubist")
})

test_that("kernlab SVR and GPR meta-policy learners fit and predict", {
  testthat::skip_if_not_installed("kernlab")
  for (method_now in c("svr", "gpr")) {
    expect_new_learner_fits(method_now)
  }
})

test_that("quantile regression forest meta-policy learner fits and predicts", {
  testthat::skip_if_not_installed("ranger")
  expect_new_learner_fits("qrf")
})

test_that("method-fold socket isolation logs the stored failure message", {
  tasks <- list(
    list(fold_id = 1L, method = "cubist"),
    list(fold_id = 1L, method = "gpr")
  )

  testthat::local_mocked_bindings(
    run_meta_policy_crossfit_method_queue_once = function(tasks,
                                                          payload,
                                                          workers,
                                                          progress = FALSE,
                                                          completed_start = 0L,
                                                          total_tasks = length(tasks),
                                                          retry_mode = FALSE) {
      list(
        completed = list(),
        completed_n = completed_start,
        failed_active = tasks,
        unstarted = list(),
        error = "error reading from connection"
      )
    },
    .package = "tsbiomass"
  )

  output <- utils::capture.output(
    result <- tsbiomass:::run_meta_policy_crossfit_method_tasks(
      tasks = tasks,
      payload = list(),
      workers = 2L,
      progress = TRUE
    ),
    type = "message"
  )

  expect_length(result, 2L)
  expect_true(all(!vapply(result, function(x) isTRUE(x$succeeded_oof), logical(1))))
  expect_true(any(grepl("isolated after socket failure", output, fixed = TRUE)))
  expect_true(any(grepl("socket failure isolated to this method-fold task", output, fixed = TRUE)))
  expect_true(any(grepl("error reading from connection", output, fixed = TRUE)))
})

test_that("stochtree method-fold tasks request the PSOCK-safe backend", {
  expect_false(tsbiomass:::meta_policy_method_tasks_require_psock(list(
    list(fold_id = 1L, method = "rf"),
    list(fold_id = 1L, method = "qrf")
  )))
  expect_true(tsbiomass:::meta_policy_method_tasks_require_psock(list(
    list(fold_id = 1L, method = "vfbart")
  )))
})

test_that("final Super Learner OOF scheduler sets fork payload before cluster creation", {
  on.exit(tsbiomass:::clear_meta_policy_crossfit_payload(), add = TRUE)
  tasks <- list(
    list(fold_id = 1L, method = "glm"),
    list(fold_id = 2L, method = "glm")
  )
  payload <- list(
    training_data = tibble::tibble(
      .outcome = c(0.1, 0.2, 0.3, 0.4),
      feature_a = c(1, 2, 3, 4)
    ),
    foldid = c(1L, 1L, 2L, 2L),
    feature_cols = "feature_a",
    outcome_transform = "identity",
    lambda_rule = "lambda.min",
    inner_folds = 2L,
    seed = 1L,
    method_settings = list()
  )

  testthat::local_mocked_bindings(
    initialize_parallel_cluster = function(workers) {
      testthat::expect_true(
        exists("foldid", envir = tsbiomass:::.meta_policy_crossfit_payload, inherits = FALSE)
      )
      testthat::expect_equal(
        get("foldid", envir = tsbiomass:::.meta_policy_crossfit_payload, inherits = FALSE),
        payload$foldid
      )
      stop("payload ordering check", call. = FALSE)
    },
    .package = "tsbiomass"
  )

  expect_error(
    tsbiomass:::run_meta_policy_super_oof_tasks(
      tasks = tasks,
      payload = payload,
      workers = 2L,
      progress = FALSE
    ),
    "payload ordering check"
  )
})
