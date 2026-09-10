test_that("admissibility missingness uses every configured trait plus support fields", {
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
      "swimbladder_type",
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

test_that("generalized models do not bypass configured metadata missingness", {
  rows <- tibble::tibble(
    model_id = c("generalized", "empirical"),
    species_name = c(NA_character_, "Alpha alpha"),
    genus = c(NA_character_, "Alpha"),
    species = c(NA_character_, "alpha"),
    family = c(NA_character_, "Alphaidae"),
    body_shape = c(NA_character_, "fusiform"),
    is_group_model = c(TRUE, FALSE),
    fao_area = c("77", NA_character_)
  )
  key_cols <- c("species_name", "genus", "species", "family", "body_shape", "fao_area")

  scored <- tsbiomass:::screen_missing_metadata(rows, key_cols = key_cols)
  summary <- tsbiomass:::summarize_key_missing(
    scored,
    key_cols = key_cols,
    threshold = 0.75
  )

  expect_equal(scored$key_metadata_missing_fraction[[1]], 5 / length(key_cols))
  expect_equal(scored$key_metadata_missing_fraction[[2]], 1 / length(key_cols))
  expect_equal(
    unname(summary$by_field$missing_n[match("family", summary$by_field$field)]),
    1
  )
  expect_equal(
    unname(summary$by_field$missing_n[match("fao_area", summary$by_field$field)]),
    1
  )
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
    logic_version = tsbiomass:::anchor_admissibility_logic_version(),
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
    ),
    all_gate_audit = tibble::tibble(gate = "frequency")
  )
  contract <- tsbiomass:::build_admissibility_contract(cfg)
  bundle$effective_contract <- contract
  bundle$effective_contract_fingerprint <- tsbiomass:::admissibility_audit_fingerprint(contract)

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

test_that("anchor overlap summary is driven by available overlap fields", {
  overlap <- tsbiomass:::summarize_anchor_overlap(tibble::tibble(
    w_adm = c(0.25, 0.75),
    overlap_same_species = c(TRUE, FALSE),
    overlap_same_custom_trait = c(FALSE, TRUE),
    pressure_coherence_distance = c(0.20, 0.60),
    custom_overlap_fraction = c(0.30, 0.90)
  ))

  expect_equal(overlap$n_admissible, 2L)
  expect_equal(overlap$w_same_species, 0.25)
  expect_equal(overlap$w_same_custom_trait, 0.75)
  expect_equal(overlap$mean_pressure_coherence, 0.5)
  expect_equal(overlap$mean_custom_overlap_fraction, 0.75)
  expect_false("w_same_family" %in% names(overlap))
  expect_false("w_same_swimbladder" %in% names(overlap))
  expect_false("w_same_ocean_basin" %in% names(overlap))
  expect_false("mean_length_overlap_fraction" %in% names(overlap))
  expect_false("mean_depth_overlap_fraction" %in% names(overlap))
})

test_that("anchor overlap summary returns an empty summary when admissible weights are degenerate", {
  overlap <- tsbiomass:::summarize_anchor_overlap(tibble::tibble(
    w_adm = c(NA_real_, 0),
    overlap_same_family = c(TRUE, FALSE),
    overlap_same_ocean_basin = c(TRUE, TRUE),
    length_overlap_fraction = c(0.25, 0.75)
  ))

  expect_equal(overlap$n_admissible, 0L)
  expect_true(is.na(overlap$w_same_family))
  expect_true(is.na(overlap$w_same_ocean_basin))
})

test_that("anchor overlap summary is restricted to configured traits", {
  scored <- tibble::tibble(
    w_adm = c(0.25, 0.75),
    overlap_same_family = c(TRUE, TRUE),
    overlap_same_order = c(TRUE, TRUE),
    overlap_same_swimbladder = c(TRUE, FALSE),
    overlap_same_swimbladder_type = c(TRUE, FALSE),
    overlap_same_derivation = c(TRUE, TRUE),
    length_overlap_fraction = c(0.50, 0.75)
  )
  cfg <- list(
    similarity = list(
      species_traits = c("family"),
      study_traits = character(0),
      coherence = list(length = list(mode = "overlap"))
    ),
    admissibility = list(
      species_traits = c("swimbladder_type"),
      study_traits = character(0),
      coherence = list(length = list(mode = "overlap", min = 0.01))
    )
  )

  overlap <- tsbiomass:::summarize_anchor_overlap(scored, config = cfg)

  expect_true("w_same_family" %in% names(overlap))
  expect_true("w_same_swimbladder" %in% names(overlap))
  expect_true("mean_length_overlap_fraction" %in% names(overlap))
  expect_false("w_same_order" %in% names(overlap))
  expect_false("w_same_derivation" %in% names(overlap))
  expect_false("w_same_swimbladder_type" %in% names(overlap))
})

test_that("anchor overlap aliases summarize onto canonical metrics", {
  scored <- tibble::tibble(
    w_adm = c(0.25, 0.75),
    overlap_same_fao = c(TRUE, FALSE),
    overlap_same_swimbladder_type = c(FALSE, TRUE),
    overlap_same_derivation_type = c(TRUE, TRUE)
  )
  cfg <- list(
    similarity = list(
      species_traits = character(0),
      study_traits = c("fao_area")
    ),
    admissibility = list(
      species_traits = c("swimbladder_type"),
      study_traits = c("derivation_type")
    )
  )

  overlap <- tsbiomass:::summarize_anchor_overlap(scored, config = cfg)

  expect_equal(overlap$w_same_fao_area, 0.25)
  expect_equal(overlap$w_same_swimbladder, 0.75)
  expect_equal(overlap$w_same_derivation, 1)
  expect_false("w_same_fao" %in% names(overlap))
  expect_false("w_same_swimbladder_type" %in% names(overlap))
  expect_false("w_same_derivation_type" %in% names(overlap))
})

test_that("anchor scored rows reattach admissible weights by stable model id", {
  cfg <- tsbiomass:::default_anchor_config(minimal_config_data())
  anchor_row <- tibble::tibble(
    model_id = "anchor",
    species_name = "Sardinops sagax"
  )
  eval_obj <- list(
    model_eval = tibble::tibble(
      model_id = c("donor_1", "donor_2"),
      w_combined = c(0.1234567, 0.2),
      admissible = c(TRUE, FALSE),
      overlap_same_species = c(TRUE, FALSE),
      overlap_same_genus = c(TRUE, FALSE)
    ),
    admissible_df = tibble::tibble(
      model_id = "donor_1",
      w_combined = 0.1234568,
      w_study_adj_raw = 0.5,
      w_adm = 1,
      cumulative_w_adm = 1
    )
  )

  scored <- tsbiomass:::collect_anchor_scores(eval_obj, anchor_row, cfg)
  overlap <- scored |>
    dplyr::filter(.data$admissible) |>
    tsbiomass:::summarize_anchor_overlap()

  expect_equal(scored$w_adm, c(1, NA_real_))
  expect_equal(overlap$w_same_species, 1)
  expect_equal(overlap$w_same_genus, 1)
})

test_that("admissibility cache currentness requires overlap summary metrics", {
  cfg <- list(
    admissibility = list(
      species_traits = character(0),
      study_traits = character(0),
      coherence = list(
        frequency = list(mode = "overlap")
      )
    )
  )
  stale_bundle <- list(
    logic_version = tsbiomass:::anchor_admissibility_logic_version(),
    all_scores = tibble::tibble(
      anchor_species = "Alpha alpha",
      admissible = TRUE,
      overlap_same_species = TRUE,
      length_overlap_fraction = 0.75,
      frequency_coherence_distance = 0.20,
      gate_frequency = TRUE,
      key_metadata_missing_fraction = 0
    ),
    all_overlap = tibble::tibble(
      anchor_species = "Alpha alpha",
      n_admissible = 1L
    ),
    all_gate_audit = tibble::tibble(gate = "frequency")
  )
  contract <- tsbiomass:::build_admissibility_contract(cfg)
  stale_bundle$effective_contract <- contract
  stale_bundle$effective_contract_fingerprint <- tsbiomass:::admissibility_audit_fingerprint(contract)
  current_bundle <- stale_bundle
  current_bundle$all_overlap <- tibble::tibble(
    anchor_species = "Alpha alpha",
    n_admissible = 1L,
    w_same_species = 1,
    mean_length_overlap_fraction = 0.75,
    mean_frequency_coherence = 0.80
  )

  expect_false(tsbiomass:::admissibility_bundle_is_current(stale_bundle, cfg))
  expect_true(tsbiomass:::admissibility_bundle_is_current(current_bundle, cfg))
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

test_that("generalized models obey the same configured species-trait gates", {
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
    model_id = "anchor",
    swimbladder_type = "physostome",
    frequency = 38
  )
  scored <- tibble::tibble(
    model_id = c("generalized_missing", "generalized_nonspecific", "generalized_mismatch", "ordinary_missing"),
    species_name = c(NA_character_, NA_character_, NA_character_, "Alpha alpha"),
    is_group_model = c(TRUE, TRUE, TRUE, FALSE),
    swimbladder_type = c(NA_character_, "general/nonspecific", "physoclist", NA_character_),
    overlap_same_swimbladder_type = c(FALSE, FALSE, FALSE, FALSE),
    length_overlap_fraction = c(1, 1, 1, 1),
    depth_overlap_fraction = c(1, 1, 1, 1),
    key_metadata_missing_fraction = c(0, 0, 0, 0)
  )

  gated <- tsbiomass:::apply_anchor_gates(scored, anchor_row, cfg)

  expect_false(gated$gate_trait_swimbladder_type[[1]])
  expect_false(gated$admissible[[1]])
  expect_match(gated$inadmissible_reasons[[1]], "trait_missing:swimbladder_type")
  expect_false(gated$gate_trait_swimbladder_type[[2]])
  expect_false(gated$admissible[[2]])
  expect_match(gated$inadmissible_reasons[[2]], "trait_missing:swimbladder_type")
  expect_false(gated$gate_trait_swimbladder_type[[3]])
  expect_false(gated$admissible[[3]])
  expect_false(gated$gate_trait_swimbladder_type[[4]])
  expect_false(gated$admissible[[4]])
})
test_that("point-valued anchor ranges use point-mass containment overlap", {
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

test_that("admissibility range gates compare study ranges only", {
  cfg <- tsbiomass:::default_anchor_config(list(
    similarity = list(species_traits = character(0), study_traits = character(0)),
    admissibility = list(
      species_traits = character(0),
      study_traits = character(0),
      coherence = list(
        length = list(mode = "overlap", min = 0.5),
        depth = list(mode = "overlap", min = 0.5),
        frequency = list(mode = "none")
      )
    )
  ))
  anchor <- tibble::tibble(
    model_id = "anchor",
    species_name = "Anchor species",
    study_length_min = 6,
    study_length_max = 10,
    species_length_min = 100,
    species_length_max = 200,
    study_depth_min = 20,
    study_depth_max = 40,
    species_depth_min = 500,
    species_depth_max = 1000
  )
  donors <- tibble::tibble(
    model_id = c("study_match", "species_match_only"),
    species_name = c("Donor one", "Donor two"),
    study_length_min = c(6, 100),
    study_length_max = c(10, 200),
    species_length_min = c(1, 100),
    species_length_max = c(2, 200),
    study_depth_min = c(20, 500),
    study_depth_max = c(40, 1000),
    species_depth_min = c(1, 500),
    species_depth_max = c(2, 1000)
  )

  scored <- donors |>
    tsbiomass:::screen_missing_metadata(key_cols = tsbiomass:::admissibility_key_metadata_cols(cfg)) |>
    tsbiomass:::add_anchor_overlap(anchor_row = anchor, config = cfg) |>
    tsbiomass:::apply_anchor_gates(anchor_row = anchor, config = cfg)

  expect_identical(scored$gate_length_overlap, c(TRUE, FALSE))
  expect_identical(scored$gate_depth_overlap, c(TRUE, FALSE))
  expect_identical(scored$admissible, c(TRUE, FALSE))
})

test_that("gate audit and table fingerprints are row-order invariant", {
  cfg <- tsbiomass:::default_anchor_config(list(
    similarity = list(species_traits = character(0), study_traits = character(0)),
    admissibility = list(
      species_traits = character(0),
      study_traits = character(0),
      coherence = list(
        length = list(mode = "none"),
        depth = list(mode = "none"),
        frequency = list(mode = "overlap", gap = 20)
      )
    )
  ))
  anchor <- tibble::tibble(model_id = "anchor", species_name = "Anchor species", frequency = 38)
  donors <- tibble::tibble(
    model_id = c("near", "far", "missing"),
    species_name = c("Near species", "Far species", "Missing species"),
    frequency = c(50, 70, NA_real_),
    length_overlap_fraction = 1,
    depth_overlap_fraction = 1,
    key_metadata_missing_fraction = c(0, 0, 1)
  )
  gate_once <- function(tbl) {
    gated <- tsbiomass:::apply_anchor_gates(tbl, anchor, cfg)
    tsbiomass:::build_admissibility_gate_audit(gated, anchor, cfg) |>
      dplyr::arrange(.data$donor_model_id, .data$gate)
  }

  expect_equal(gate_once(donors), gate_once(donors[3:1, ]))
  expect_identical(
    tsbiomass:::admissibility_table_fingerprint(donors),
    tsbiomass:::admissibility_table_fingerprint(donors[3:1, ])
  )
  audit <- gate_once(donors)
  expect_equal(
    audit$reason[audit$donor_model_id == "missing" & audit$gate == "frequency"],
    "frequency_missing"
  )
})

test_that("an unavailable configured anchor gate errors instead of falling back", {
  cfg <- tsbiomass:::default_anchor_config(list(
    admissibility = list(
      species_traits = "swimbladder_type",
      study_traits = character(0),
      coherence = list(
        length = list(mode = "none"),
        depth = list(mode = "none"),
        frequency = list(mode = "none")
      )
    )
  ))
  anchor <- tibble::tibble(model_id = "query", swimbladder_type = NA_character_)
  donors <- tibble::tibble(
    model_id = "donor",
    swimbladder_type = "physostome",
    overlap_same_swimbladder_type = FALSE,
    length_overlap_fraction = 1,
    depth_overlap_fraction = 1,
    key_metadata_missing_fraction = 0
  )
  expect_error(
    tsbiomass:::apply_anchor_gates(donors, anchor, cfg),
    "gate cannot be evaluated"
  )
})
