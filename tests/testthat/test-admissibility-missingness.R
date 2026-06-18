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
