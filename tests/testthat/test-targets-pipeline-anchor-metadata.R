test_that("SWFSC targets anchor length-weight metadata is keyed by species", {
  testthat::skip_if_not_installed("targets")

  env <- new.env(parent = globalenv())
  sys.source(tsbiomass_test_file("scripts", "targets_pipeline.R"), envir = env)

  anchor_map <- env$swfsc_targets_anchor_model_map()
  sard <- anchor_map[anchor_map$scientific_name == "Sardinops sagax", , drop = FALSE]
  trach <- anchor_map[anchor_map$scientific_name == "Trachurus symmetricus", , drop = FALSE]

  expect_equal(sard$model_id, "11")
  expect_equal(sard$lw_a_g, exp(-13.140) * 10^3.253)
  expect_equal(sard$lw_b, 3.253)
  expect_equal(trach$model_id, "16")
  expect_equal(trach$lw_a_g, exp(-13.36408) * 10^3.327691)
  expect_equal(trach$lw_b, 3.327691)
})
