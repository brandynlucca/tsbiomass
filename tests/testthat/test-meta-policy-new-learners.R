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
