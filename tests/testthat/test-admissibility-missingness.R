test_that("admissibility missingness uses study metadata plus support fields", {
  cfg <- tsbiomass:::default_anchor_config(list(
    similarity = list(
      study_traits = list(
        fao_area = 1,
        season = 1,
        diel = 1,
        pressure_corrected = 1,
        length_metric = 1
      )
    ),
    admissibility = list(
      species_traits = "swimbladder_type",
      study_traits = character(0),
      coherence = list(
        length = list(mode = "overlap", min = 0.01),
        depth = list(mode = "overlap", min = 0.01),
        frequency = list(mode = "overlap", gap = 60)
      ),
      key_metadata_max = 0.75
    )
  ))

  expect_setequal(
    tsbiomass:::admissibility_key_metadata_cols(cfg),
    c(
      "fao_area", "season", "diel", "pressure_corrected", "length_metric",
      "study_length_min", "study_length_max",
      "study_depth_min", "study_depth_max",
      "frequency"
    )
  )
})

test_that("admissibility gates reject missing required support and hard-gate metadata", {
  cfg <- tsbiomass:::default_anchor_config(list(
    similarity = list(
      study_traits = list(
        fao_area = 1,
        season = 1,
        diel = 1,
        pressure_corrected = 1,
        length_metric = 1
      )
    ),
    admissibility = list(
      species_traits = "swimbladder_type",
      study_traits = character(0),
      coherence = list(
        length = list(mode = "overlap", min = 0.01),
        depth = list(mode = "overlap", min = 0.01),
        frequency = list(mode = "overlap", gap = 60)
      ),
      key_metadata_max = 0.75
    )
  ))

  candidate_models <- tibble::tibble(
    model_id = 1:2,
    model_id_chr = c("1", "2"),
    species_name = c("A a", "B b"),
    genus = c("A", "B"),
    family = c("Fam", "Fam"),
    order = c("Ord", "Ord"),
    swimbladder_type = c(NA_character_, "physostome"),
    fao_area = c("61", "61"),
    ocean_basin = c("pacific", "pacific"),
    equation_form = c("20log10_ind", "20log10_ind"),
    derivation_type = c("direct", "direct"),
    season = c("summer", NA_character_),
    diel = c("day", "day"),
    pressure_corrected = c(TRUE, TRUE),
    length_metric = c("fork", "fork"),
    study_length_min = c(NA_real_, 10),
    study_length_max = c(NA_real_, 20),
    study_depth_min = c(NA_real_, 5),
    study_depth_max = c(NA_real_, 15),
    frequency = c(NA_real_, 38),
    length_overlap_fraction = c(NA_real_, 1),
    depth_overlap_fraction = c(NA_real_, 1)
  )

  anchor_row <- tibble::tibble(
    model_id = 99,
    model_id_chr = "99",
    species_name = "C c",
    genus = "C",
    family = "Fam",
    order = "Ord",
    swimbladder_type = "physostome",
    fao_area = "61",
    ocean_basin = "pacific",
    equation_form = "20log10_ind",
    derivation_type = "direct",
    study_length_min = 10,
    study_length_max = 20,
    study_depth_min = 5,
    study_depth_max = 15,
    frequency = 38
  )

  scored <- tsbiomass:::screen_missing_metadata(
    candidate_models,
    key_cols = tsbiomass:::admissibility_key_metadata_cols(cfg)
  )
  gated <- tsbiomass:::apply_anchor_gates(scored, anchor_row, cfg)

  expect_false(gated$gate_trait_swimbladder_type[[1]])
  expect_false(gated$gate_frequency[[1]])
  expect_false(gated$gate_length_overlap[[1]])
  expect_false(gated$gate_depth_overlap[[1]])
  expect_false(gated$admissible[[1]])

  expect_true(gated$gate_trait_swimbladder_type[[2]])
  expect_true(gated$gate_frequency[[2]])
  expect_true(gated$gate_length_overlap[[2]])
  expect_true(gated$gate_depth_overlap[[2]])
  expect_true(gated$gate_missing_key_metadata[[2]])
  expect_true(gated$admissible[[2]])
})

test_that("admissibility currentness does not require finite frequency distances", {
  cfg <- list(
    similarity = list(
      study_traits = list(frequency = 1, fao_area = 1, equation_form = 1)
    ),
    admissibility = list(
      species_traits = "swimbladder_type",
      coherence = list(
        frequency = list(mode = "overlap")
      )
    )
  )

  bundle <- list(
    all_scores = tibble::tibble(
      frequency = c(18, 38),
      fao_area = c("61", "61"),
      equation_form = c("standardized_length", "standardized_length"),
      swimbladder_type = c("physostome", "physostome"),
      study_length_min = c(10, 10),
      study_length_max = c(20, 20),
      study_depth_min = c(5, 5),
      study_depth_max = c(15, 15),
      gate_trait_swimbladder_type = c(TRUE, TRUE),
      gate_frequency = c(TRUE, TRUE),
      frequency_coherence_distance = c(NA_real_, NA_real_),
      key_metadata_missing_fraction = c(0, 0),
      admissible = c(TRUE, TRUE)
    )
  )

  expect_true(tsbiomass:::admissibility_bundle_is_current(bundle, cfg))
})

test_that("anchor overlap tolerates dropped taxonomy columns", {
  cfg <- tsbiomass:::default_anchor_config(list(
    similarity = list(
      species_traits = character(0),
      study_traits = character(0)
    )
  ))

  candidate_models <- tibble::tibble(
    model_id_chr = c("m1", "m2"),
    species_name = c("sp1", "sp2"),
    study_length_min = c(10, 12),
    study_length_max = c(20, 24),
    study_depth_min = c(5, 5),
    study_depth_max = c(15, 15)
  )
  anchor_row <- tibble::tibble(
    model_id = "a1",
    species_name = "sp1",
    study_length_min = 10,
    study_length_max = 20,
    study_depth_min = 5,
    study_depth_max = 15
  )

  out <- tsbiomass:::add_anchor_overlap(
    candidate_models = candidate_models,
    anchor_row = anchor_row,
    config = cfg
  )

  expect_true(all(c(
    "overlap_same_species",
    "overlap_same_genus",
    "overlap_same_family",
    "overlap_same_order"
  ) %in% names(out)))
  expect_identical(out$overlap_same_genus, c(FALSE, FALSE))
  expect_identical(out$overlap_same_family, c(FALSE, FALSE))
  expect_identical(out$overlap_same_order, c(FALSE, FALSE))
})

test_that("admissibility gates reuse precomputed overlap columns when available", {
  cfg <- tsbiomass:::default_anchor_config(list(
    admissibility = list(
      species_traits = "swimbladder_type",
      study_traits = character(0),
      coherence = list(
        frequency = list(mode = "none")
      )
    )
  ))

  anchor_row <- tibble::tibble(
    model_id = "a1",
    swimbladder_type = "physostome",
    frequency = 38
  )
  scored <- tibble::tibble(
    model_id = c("m1", "m2"),
    swimbladder_type = c("physostome", "swimbladderless"),
    overlap_same_swimbladder_type = c(TRUE, FALSE),
    length_overlap_fraction = c(1, 1),
    depth_overlap_fraction = c(1, 1),
    key_metadata_missing_fraction = c(0, 0)
  )

  testthat::local_mocked_bindings(
    read_similarity_registry = function(...) {
      stop("apply_anchor_gates() should reuse overlap columns before reading the registry")
    },
    .package = "tsbiomass"
  )

  gated <- tsbiomass:::apply_anchor_gates(scored, anchor_row, cfg)

  expect_identical(gated$gate_trait_swimbladder_type, c(TRUE, FALSE))
  expect_identical(gated$admissible, c(TRUE, FALSE))
})
test_that("point-valued anchor ranges use point-mass containment overlap", {
  expect_equal(tsbiomass:::compute_range_overlap(4, 4, 0, 100), 1)
  expect_equal(tsbiomass:::compute_range_overlap(4, 4, 5, 100), 0)
  expect_equal(
    tsbiomass:::compute_range_overlap_vec(4, 4, c(0, 4, 5), c(100, 4, 100)),
    c(1, 1, 0)
  )
})

test_that("generalized equations never satisfy same-species overlap", {
  cfg <- tsbiomass:::default_anchor_config(minimal_config_data())
  candidate_models <- tibble::tibble(
    model_id = c("1", "2"),
    species_name = c("NA NA", "Gadus morhua"),
    genus = c(NA_character_, "Gadus"),
    species = c(NA_character_, "morhua")
  )
  anchor_row <- candidate_models[1, , drop = FALSE]

  overlap <- tsbiomass:::add_anchor_overlap(
    candidate_models = candidate_models,
    anchor_row = anchor_row,
    config = cfg
  )

  expect_false(any(overlap$overlap_same_species))
})
