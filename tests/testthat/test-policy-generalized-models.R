test_that("generalized model rows include absent species identities", {
  rows <- tibble::tibble(
    model_id = as.character(1:6),
    species_name = c("NA NA", NA_character_, "Clupea harengus", "", "Unknown unknown", "Sardina pilchardus"),
    species = c(NA_character_, NA_character_, "harengus", "", NA_character_, "pilchardus"),
    slope_len = c(20, 21, 20, 19, 18, 20),
    intercept_len = c(-70, -68, -65, -72, -74, -66)
  )

  generalized <- tsbiomass:::group_model_rows(rows)

  expect_equal(generalized$model_id, as.character(c(1, 2, 4, 5)))
})

test_that("generalized model rows still honor explicit grouping metadata", {
  rows <- tibble::tibble(
    model_id = as.character(1:4),
    species_name = rep("Clupea harengus", 4),
    species = rep("harengus", 4),
    is_group_model = c(FALSE, TRUE, FALSE, FALSE),
    method_type = c("species", "species", "group", "generalized")
  )

  generalized <- tsbiomass:::group_model_rows(rows)

  expect_equal(generalized$model_id, as.character(c(2, 3, 4)))
})

test_that("candidate standardization flags generalized equations durably", {
  rows <- tibble::tibble(
    model_id = as.character(1:3),
    species_name = c("NA NA", "Unknown unknown", "Clupea pallasii"),
    slope_len = c(20, 20, 20),
    intercept_len = c(-68, -70, -72)
  )

  out <- tsbiomass:::standardize_candidate_columns(rows)

  expect_true(out$is_group_model[[1]])
  expect_true(out$is_group_model[[2]])
  expect_false(out$is_group_model[[3]])
  expect_true(is.na(out$species_name[[1]]))
  expect_true(is.na(out$species_name[[2]]))
  expect_false(any(out$species_name == "NA NA", na.rm = TRUE))
  expect_equal(out$model_species_label[[1]], "Generalized model")
})

test_that("species identity keys use binomials and model-specific generalized keys", {
  rows <- tibble::tibble(
    model_id = as.character(1:5),
    species_name = c(
      "Engraulis mordax",
      "Osmerus mordax",
      "Clupea harengus",
      "NA NA",
      NA_character_
    ),
    genus = c("Engraulis", "Osmerus", "Clupea", NA_character_, NA_character_),
    species = c("mordax", "mordax", "harengus", NA_character_, NA_character_),
    is_group_model = c(FALSE, FALSE, FALSE, TRUE, TRUE)
  )

  keys <- tsbiomass:::species_identity_key(rows)

  expect_equal(keys[[1]], "Engraulis mordax")
  expect_equal(keys[[2]], "Osmerus mordax")
  expect_false(identical(keys[[1]], keys[[2]]))
  expect_equal(keys[[4]], "<generalized-model:4>")
  expect_equal(keys[[5]], "<generalized-model:5>")
})

test_that("ordinary policy pools exclude generalized equations", {
  rows <- tibble::tibble(
    model_id = as.character(1:3),
    species_name = c("Clupea pallasii", NA_character_, "Sardinops sagax"),
    is_group_model = c(FALSE, TRUE, FALSE),
    slope_len = c(20, 20, 22),
    intercept_len = c(-68, -70, -72),
    combined_distance = c(0.4, 0.1, 0.5),
    w_adm = 1,
    valid_prediction = TRUE
  )

  ordinary <- tsbiomass:::policy_rows(
    rows,
    policy_def = list(candidate_pool = "all_admissible"),
    policy_params = list(),
    ordination_info = list()
  )
  generalized <- tsbiomass:::policy_rows(
    rows,
    policy_def = list(candidate_pool = "generalized_models_only"),
    policy_params = list(),
    ordination_info = list()
  )

  expect_equal(ordinary$model_id, c("1", "3"))
  expect_equal(generalized$model_id, "2")
})

test_that("Alchemist pair data excludes generalized equations", {
  rows <- tibble::tibble(
    model_id = as.character(1:4),
    species_name = c("Clupea pallasii", NA_character_, "Sardinops sagax", "Engraulis mordax"),
    is_group_model = c(FALSE, TRUE, FALSE, FALSE),
    slope_len = c(20, 20, 21, 19),
    intercept_len = c(-68, -70, -71, -66),
    study_length_min = c(10, 10, 10, 10),
    study_length_max = c(20, 20, 20, 20),
    mean_depth_m = c(40, 41, 70, 35)
  )

  pair_data <- tsbiomass:::build_pair_data(
    rows,
    species_trait_names = character(0),
    study_trait_names = "mean_depth_m",
    taxonomic_distance = FALSE,
    feature_type = "difference"
  )
  training <- pair_data$training_data

  expect_false(any(training$.anchor_idx == 2L))
  expect_false(any(training$.donor_idx == 2L))
  expect_false(any(is.na(training$.anchor_species)))
  expect_false(any(is.na(training$.donor_species)))
})

test_that("nearest-singleton policies are not flagged as constructed ensembles", {
  anchor_pdf <- tibble::tibble(
    length_cm = c(10, 20, 30),
    f_len = c(0.3, 0.4, 0.3)
  )
  rows <- tibble::tibble(
    model_id = "1",
    species_name = "Clupea pallasii",
    slope_len = 20,
    intercept_len = -68,
    combined_distance = 0.1,
    taxonomic_distance_to_anchor = 0.2,
    d_species = 0.2,
    w_adm = 1
  )
  pred <- tibble::tibble(
    policy_slope_len = 20,
    policy_intercept_len = -68,
    policy_sigma_bs_mean = tsbiomass:::equation_sigma_mean(20, -68, anchor_pdf)
  )

  for (method in c("nearest_by_taxonomic_distance", "nearest_by_species_distance")) {
    structural <- tsbiomass:::policy_structural_summary(
      rows = rows,
      policy_def = list(aggregation_method = method),
      pred = pred,
      anchor_pdf = anchor_pdf
    )

    expect_false(structural$policy_is_constructed_ensemble[[1]])
  }
})

test_that("effective species support does not collapse generalized models", {
  rows <- tibble::tibble(
    model_id = c("g1", "g2"),
    species_name = c("NA NA", "NA NA"),
    genus = c(NA_character_, NA_character_),
    species = c(NA_character_, NA_character_),
    is_group_model = c(TRUE, TRUE),
    slope_len = c(20, 20),
    intercept_len = c(-68, -69),
    combined_distance = c(0.1, 0.2),
    w_adm = c(0.5, 0.5),
    .structural_weight = c(0.5, 0.5)
  )

  support <- tsbiomass:::policy_support_summary(
    rows = rows,
    policy_def = list(aggregation_method = "unweighted_mean"),
    structural_rows = rows
  )

  expect_equal(support$local_effective_support[[1]], 2)
  expect_equal(support$local_effective_species_support[[1]], 2)
})
