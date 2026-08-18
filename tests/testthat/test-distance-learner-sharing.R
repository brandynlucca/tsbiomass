test_that("share_distance_learner wraps once and is idempotent on an already-wrapped value", {
  raw <- list(fit = "model", feature_cols = c("a", "b"))
  wrapped <- tsbiomass:::share_distance_learner(raw, fingerprint = "fp-1")

  expect_s3_class(wrapped, "tsb_shared_distance_learner")
  expect_identical(wrapped$learner, raw)
  expect_identical(wrapped$fingerprint, "fp-1")

  # Re-wrapping an already-wrapped value returns the same object, not a copy.
  rewrapped <- tsbiomass:::share_distance_learner(wrapped, fingerprint = "fp-2")
  expect_identical(rewrapped, wrapped)
  expect_identical(rewrapped$fingerprint, "fp-1")

  expect_null(tsbiomass:::share_distance_learner(NULL))
})

test_that("resolve_distance_learner unwraps a wrapped value and passes a raw value through", {
  raw <- list(fit = "model")
  wrapped <- tsbiomass:::share_distance_learner(raw, fingerprint = "fp-1")

  expect_identical(tsbiomass:::resolve_distance_learner(wrapped), raw)
  # Backward compatibility: tests/pre-fix cached objects store the raw value.
  expect_identical(tsbiomass:::resolve_distance_learner(raw), raw)
  expect_null(tsbiomass:::resolve_distance_learner(NULL))
})

test_that("canonicalize_distance_learner collapses independently-wrapped copies sharing a fingerprint", {
  # Simulates the cross-session case: the same underlying fit gets reloaded
  # (independently re-wrapped) in two separate sessions/artifacts, and those
  # artifacts are later combined in one process.
  fp <- paste0("fp-cross-session-", as.integer(Sys.time()), "-", sample.int(1e6, 1))
  raw <- list(fit = "model")
  wrap_a <- tsbiomass:::share_distance_learner(raw, fingerprint = fp)
  wrap_b <- tsbiomass:::share_distance_learner(raw, fingerprint = fp)

  # Before canonicalizing: two genuinely distinct wrapper environments, even
  # though they carry the same content and fingerprint.
  expect_false(identical(wrap_a, wrap_b))

  first_seen <- tsbiomass:::canonicalize_distance_learner(wrap_a)
  expect_identical(first_seen, wrap_a)

  collapsed <- tsbiomass:::canonicalize_distance_learner(wrap_b)
  expect_identical(collapsed, wrap_a)
  expect_false(identical(collapsed, wrap_b))
})

test_that("canonicalize_distance_learner leaves unfingerprinted or raw values alone", {
  expect_null(tsbiomass:::canonicalize_distance_learner(NULL))

  raw <- list(fit = "model")
  expect_identical(tsbiomass:::canonicalize_distance_learner(raw), raw)

  unfingerprinted <- tsbiomass:::share_distance_learner(raw, fingerprint = NULL)
  expect_identical(tsbiomass:::canonicalize_distance_learner(unfingerprinted), unfingerprinted)
})

test_that("forge_distances wraps the fitted learner once, at the point it's fit", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  alchemist <- as_alchemist(candidates)

  testthat::local_mocked_bindings(
    build_pair_data = function(models_df,
                               sp_names,
                               st_names,
                               coherence_cfg = NULL,
                               taxonomic_distance = FALSE,
                               feature_type = "gower",
                               progress = FALSE) {
      list(
        training_data = tibble::tibble(
          .donor_idx = c(1L, 1L, 2L, 2L),
          .anchor_idx = c(2L, 3L, 3L, 4L),
          .dist_family = c(0.1, 0.2, 0.3, 0.4)
        ),
        feature_cols = ".dist_family",
        all_traits = "family",
        species_feature_cols = ".dist_family",
        trait_mats = list(),
        donor_sigma_matrix = matrix(1, nrow = 4, ncol = 4),
        target_sigma = rep(1, 4),
        model_ids = as.character(seq_len(nrow(models_df))),
        n_models = nrow(models_df)
      )
    },
    fit_super_learner = function(...) {
      list(
        oof_ensemble_prediction = c(0.2, 0.3, 0.4, 0.5),
        oof_performance = tibble::tibble(method = "mock", rmse = 0.1, mae = 0.1),
        feature_cols = ".dist_family"
      )
    },
    .package = "tsbiomass"
  )

  rebuilt <- forge_distances(alchemist)

  expect_s3_class(rebuilt@learner, "tsb_shared_distance_learner")
  expect_true(is.list(tsbiomass:::resolve_distance_learner(rebuilt@learner)))
  expect_identical(
    tsbiomass:::resolve_distance_learner(rebuilt@learner)$feature_cols,
    ".dist_family"
  )
})
