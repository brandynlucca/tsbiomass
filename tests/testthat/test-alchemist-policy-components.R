test_that("Alchemist excludes empirical similarity weights transparently", {
  inherited <- tsbiomass:::alchemist_config_from_config(list(
    similarity = list(
      alpha = 0.7,
      species_traits = list(family = 1),
      coherence = list(
        frequency = list(mode = "literal", frequency_weight = 3, gap = 20)
      )
    )
  ))

  expect_null(inherited$coherence$frequency$frequency_weight)
  expect_setequal(
    inherited$excluded_non_distance_similarity_parameters,
    c(
      "similarity.alpha",
      "similarity.coherence.frequency.frequency_weight",
      "similarity.coherence.frequency.gap"
    )
  )
  expect_null(inherited$coherence$frequency$gap)

  expect_error(
    tsbiomass:::alchemist_config_from_config(list(
      alchemist = list(
        coherence = list(
          frequency = list(mode = "literal", frequency_weight = 3, gap = 20)
        )
      )
    )),
    "frequency gap belongs to admissibility"
  )
})

test_that("policy components average parents rather than expanded indicators", {
  ids <- c("a", "b")
  indicator_one <- matrix(
    c(0, 1, 1, 0),
    nrow = 2,
    dimnames = list(ids, ids)
  )
  indicator_two <- indicator_one
  numeric_zero <- matrix(
    0,
    nrow = 2,
    ncol = 2,
    dimnames = list(ids, ids)
  )

  out <- tsbiomass:::alchemist_policy_component_matrix(
    trait_mats = list(
      .dist_ocean_basin__atlantic = indicator_one,
      .dist_ocean_basin__pacific = indicator_two,
      .dist_body_size = numeric_zero
    ),
    feature_cols = c(
      ".dist_ocean_basin__atlantic",
      ".dist_ocean_basin__pacific",
      ".dist_body_size"
    ),
    model_ids = ids
  )

  expect_equal(out["a", "b"], 0.5)
  expect_equal(attr(out, "coverage_matrix")["a", "b"], 1)
  expect_setequal(
    attr(out, "parent_features"),
    c(".dist_ocean_basin", ".dist_body_size")
  )
})

test_that("set-Jaccard categorical distance is scalar and fold-invariant", {
  singleton_fold <- tibble::tibble(
    model_id = c("a", "b"),
    region = c("atlantic", "pacific")
  )
  multivalue_fold <- tibble::tibble(
    model_id = c("a", "b", "c"),
    region = c("atlantic", "atlantic;pacific", "pacific")
  )

  singleton <- tsbiomass:::build_pair_feature_matrices(
    models_df = singleton_fold,
    species_trait_names = "region",
    study_trait_names = character(0),
    categorical_distance = "set_jaccard"
  )
  multivalue <- tsbiomass:::build_pair_feature_matrices(
    models_df = multivalue_fold,
    species_trait_names = "region",
    study_trait_names = character(0),
    categorical_distance = "set_jaccard"
  )
  legacy <- tsbiomass:::build_pair_feature_matrices(
    models_df = multivalue_fold,
    species_trait_names = "region",
    study_trait_names = character(0),
    categorical_distance = "observed_indicators"
  )

  expect_identical(singleton$feature_cols, ".dist_region")
  expect_identical(multivalue$feature_cols, ".dist_region")
  expect_equal(multivalue$trait_mats$.dist_region[1, 2], 0.5)
  expect_equal(multivalue$trait_mats$.dist_region[1, 3], 1)
  expect_equal(multivalue$trait_mats$.dist_region[2, 3], 0.5)
  expect_true(all(grepl("^\\.dist_region__", legacy$feature_cols)))
})

test_that("categorical distance is explicit and validated", {
  expect_identical(
    tsbiomass:::normalize_alchemist_config(list(
      learner = list(), categorical_distance = "jaccard"
    ))$categorical_distance,
    "set_jaccard"
  )
  expect_error(
    tsbiomass:::normalize_alchemist_config(list(
      learner = list(), categorical_distance = "guess"
    )),
    "must be `observed_indicators`"
  )
})

test_that("registry indicators use the complete declared set vocabulary", {
  levels <- tsbiomass:::alchemist_registry_set_levels(
    c("ocean_basin", "body_shape")
  )
  expect_setequal(
    levels$ocean_basin,
    c("atlantic", "pacific", "indian", "mediterranean", "southern", "arctic", "inland")
  )
  expect_null(levels$body_shape)

  singleton <- tsbiomass:::build_pair_feature_matrices(
    models_df = tibble::tibble(
      model_id = c("a", "b"), ocean_basin = c("atlantic", "pacific")
    ),
    species_trait_names = "ocean_basin",
    study_trait_names = character(0),
    categorical_distance = "registry_indicators",
    categorical_levels = levels
  )
  multivalue <- tsbiomass:::build_pair_feature_matrices(
    models_df = tibble::tibble(
      model_id = c("a", "b"),
      ocean_basin = c("atlantic;pacific", "pacific")
    ),
    species_trait_names = "ocean_basin",
    study_trait_names = character(0),
    categorical_distance = "registry_indicators",
    categorical_levels = levels
  )

  expect_identical(singleton$feature_cols, multivalue$feature_cols)
  expect_length(singleton$feature_cols, 7L)
  expect_true(all(grepl("^\\.dist_ocean_basin__", singleton$feature_cols)))
  expect_error(
    tsbiomass:::build_pair_feature_matrices(
      models_df = tibble::tibble(
        model_id = c("a", "b"),
        region = c("atlantic", "unregistered")
      ),
      species_trait_names = "region",
      study_trait_names = character(0),
      categorical_distance = "registry_indicators",
      categorical_levels = list(region = c("atlantic", "pacific"))
    ),
    "absent from its registry"
  )
})

test_that("configured unavailable components reduce coverage without substitution", {
  ids <- c("a", "b")
  family <- matrix(
    c(0, 0.6, 0.6, 0),
    nrow = 2,
    dimnames = list(ids, ids)
  )

  out <- tsbiomass:::alchemist_policy_component_matrix(
    trait_mats = list(.dist_family = family),
    feature_cols = c(".dist_family", ".dist_frequency_coherence"),
    model_ids = ids
  )

  expect_equal(out["a", "b"], 0.6)
  expect_equal(attr(out, "coverage_matrix")["a", "b"], 0.5)
  expect_true(".dist_frequency_coherence" %in% attr(out, "feature_cols"))
})

test_that("policy component distances use Gower even when learner representation differs", {
  models <- tibble::tibble(
    model_id = c("a", "b", "c"),
    body_size = c(0, 10, 20)
  )

  component <- tsbiomass:::alchemist_policy_component_matrices(
    models_df = models,
    species_trait_names = "body_size",
    study_trait_names = character(0),
    model_ids = models$model_id
  )
  learned_features <- tsbiomass:::build_pair_feature_matrices(
    models_df = models,
    species_trait_names = "body_size",
    study_trait_names = character(0),
    feature_type = "difference"
  )

  expect_equal(component$species_dist_model["a", "b"], 0.5)
  expect_false(isTRUE(all.equal(
    component$species_dist_model["a", "b"],
    learned_features$trait_mats$.dist_body_size[1, 2]
  )))
  expect_identical(
    component$component_definition,
    "unweighted_gower_configured_parent_traits_coherence_replacement_v2"
  )
})

test_that("frequency coherence replaces raw frequency and gap remains admissibility-only", {
  models <- tibble::tibble(
    model_id = c("a", "b", "c"),
    frequency = c(38, 58, 70),
    vessel = c("v1", "v2", "v3")
  )
  overlap_20 <- list(
    length = list(mode = "none"),
    depth = list(mode = "none"),
    frequency = list(mode = "overlap", gap = 20)
  )
  overlap_200 <- overlap_20
  overlap_200$frequency$gap <- 200

  features_20 <- tsbiomass:::build_pair_feature_matrices(
    models_df = models,
    species_trait_names = character(0),
    study_trait_names = c("vessel", "frequency"),
    coherence_cfg = overlap_20,
    feature_type = "gower"
  )
  features_200 <- tsbiomass:::build_pair_feature_matrices(
    models_df = models,
    species_trait_names = character(0),
    study_trait_names = c("vessel", "frequency"),
    coherence_cfg = overlap_200,
    feature_type = "gower"
  )

  expect_false(".dist_frequency" %in% names(features_20$trait_mats))
  expect_true(".dist_frequency_coherence" %in% names(features_20$trait_mats))
  expect_equal(
    features_20$trait_mats$.dist_frequency_coherence,
    features_200$trait_mats$.dist_frequency_coherence
  )
  expect_identical(
    unname(features_20$coherence_feature_replacements[["frequency"]]),
    ".dist_frequency_coherence:overlap"
  )

  literal <- overlap_20
  literal$frequency <- list(mode = "literal")
  literal_features <- tsbiomass:::build_pair_feature_matrices(
    models_df = models,
    species_trait_names = character(0),
    study_trait_names = c("vessel", "frequency"),
    coherence_cfg = literal,
    feature_type = "gower"
  )
  expect_equal(literal_features$trait_mats$.dist_frequency_coherence[1, 2], 1)
  expect_equal(literal_features$trait_mats$.dist_frequency_coherence[1, 1], 0)
})

test_that("coherence dimensions enter their configured biological or survey component", {
  models <- tibble::tibble(
    model_id = c("a", "b", "c"),
    family = c("A", "B", "C"),
    vessel = c("v1", "v2", "v3"),
    frequency = c(38, 70, 120),
    study_length_min = c(5, 10, 15),
    study_length_max = c(10, 15, 20),
    species_length_min = c(2, 4, 6),
    species_length_max = c(20, 25, 30),
    study_depth_min = c(0, 10, 20),
    study_depth_max = c(50, 60, 70),
    species_depth_min = c(0, 100, 200),
    species_depth_max = c(500, 600, 700)
  )
  coherence <- list(
    length = list(mode = "overlap", source = "both"),
    depth = list(mode = "overlap", source = "both"),
    frequency = list(mode = "offset")
  )

  component <- tsbiomass:::alchemist_policy_component_matrices(
    models_df = models,
    species_trait_names = "family",
    study_trait_names = "vessel",
    coherence_cfg = coherence,
    model_ids = models$model_id
  )

  expect_setequal(
    component$coherence_component_map[
      grepl("length|depth|frequency", names(component$coherence_component_map))
    ],
    c("study", "species", "study", "species", "study")
  )
  expect_true(all(c(
    ".dist_length_coherence_species",
    ".dist_depth_coherence_species"
  ) %in% component$species_component_cols))
  expect_true(all(c(
    ".dist_length_coherence_study",
    ".dist_depth_coherence_study",
    ".dist_frequency_coherence"
  ) %in% component$study_component_cols))
})

test_that("query frequency coherence reuses the training normalization", {
  base <- tibble::tibble(frequency = c(38, 70))
  augmented <- tibble::tibble(frequency = c(38, 70, 200))
  coherence <- list(
    length = list(mode = "none"),
    depth = list(mode = "none"),
    frequency = list(mode = "offset")
  )

  training <- tsbiomass:::coherence_mats(base, coherence)
  query <- tsbiomass:::coherence_mats(
    augmented,
    coherence,
    normalization = attr(training, "normalization")
  )
  rescaled <- tsbiomass:::coherence_mats(augmented, coherence)

  expect_equal(
    query$frequency_coherence[1, 2],
    training$frequency_coherence[1, 2]
  )
  expect_false(isTRUE(all.equal(
    rescaled$frequency_coherence[1, 2],
    training$frequency_coherence[1, 2]
  )))
})

test_that("active coherence never substitutes an unspecified or unavailable range source", {
  models <- tibble::tibble(
    model_id = c("a", "b"),
    study_length_min = c(5, 10),
    study_length_max = c(10, 20)
  )
  no_source <- list(
    length = list(mode = "overlap"),
    depth = list(mode = "none"),
    frequency = list(mode = "none")
  )
  expect_error(
    tsbiomass:::coherence_mats(models, no_source),
    "explicit source"
  )

  unavailable_species <- no_source
  unavailable_species$length$source <- "both"
  expect_error(
    tsbiomass:::coherence_mats(models, unavailable_species),
    "source 'species'.*both configured range columns"
  )
})

test_that("literal frequency coherence tests the supplied numeric values exactly", {
  coherence <- list(
    length = list(mode = "none"),
    depth = list(mode = "none"),
    frequency = list(mode = "literal")
  )
  out <- tsbiomass:::coherence_mats(
    tibble::tibble(frequency = c(38.1, 38.4)),
    coherence
  )
  expect_equal(out$frequency_coherence[1, 2], 1)
})

test_that("failed phylogenetic distance is not replaced by raw taxonomy", {
  models <- tibble::tibble(
    model_id = c("a", "b"),
    family = c("Family one", "Family two")
  )
  testthat::local_mocked_bindings(
    tax_dist_mat = function(...) NULL,
    .package = "tsbiomass"
  )
  expect_error(
    tsbiomass:::build_pair_feature_matrices(
      models_df = models,
      species_trait_names = "family",
      study_trait_names = character(0),
      taxonomic_distance = TRUE,
      feature_type = "gower"
    ),
    "raw taxonomy is not substituted"
  )
})

test_that("query taxonomy is re-expressed on the frozen training scale", {
  tax <- matrix(c(0, 0.5, 0.5, 0), nrow = 2)
  attr(tax, "taxonomic_distance_scale") <- 10
  attr(tax, "taxonomic_distance_method") <- "open_tree_node_grafen"

  out <- tsbiomass:::alchemist_query_taxonomy_on_training_scale(
    tax,
    list(
      taxonomic_distance_scale = 5,
      taxonomic_distance_method = "open_tree_node_grafen"
    )
  )

  expect_equal(out[1, 2], 1)
  expect_equal(attr(out, "taxonomic_distance_scale"), 5)
  expect_error(
    tsbiomass:::alchemist_query_taxonomy_on_training_scale(
      tax,
      list(
        taxonomic_distance_scale = 5,
        taxonomic_distance_method = "different_method"
      )
    ),
    "different construction methods"
  )
})

test_that("Alchemist bundle preserves distinct component semantics", {
  candidates <- make_candidates(seed_similarity_tuning = FALSE)
  alchemist <- as_alchemist(candidates)
  ids <- as.character(candidates@candidate_models$model_id)
  n <- length(ids)
  make_mat <- function(value) {
    out <- matrix(value, nrow = n, ncol = n, dimnames = list(ids, ids))
    diag(out) <- 0
    out
  }
  alchemist <- tsbiomass:::alchemist_rebuild(
    alchemist,
    learner = structure(list(feature_cols = ".dist_family"), class = "Mahalanobis"),
    distance_matrix = list(
      combined_dist = stats::as.dist(make_mat(0.9)),
      dist_matrix = make_mat(0.9),
      directed_dist_matrix = make_mat(0.8),
      species_dist_matrix = make_mat(0.2),
      study_dist_matrix = make_mat(0.3),
      taxonomic_dist_matrix = make_mat(0.4),
      species_component_coverage = make_mat(1),
      study_component_coverage = make_mat(1),
      learned_kernel_bandwidth = 0.5,
      species_trait_names = "family",
      study_trait_names = "frequency",
      feature_cols = ".dist_family"
    )
  )

  bundle <- tsbiomass:::alchemist_policy_distance_bundle(alchemist)

  expect_equal(bundle$species_dist_model[1, 2], 0.2)
  expect_equal(bundle$study_dist[1, 2], 0.3)
  expect_equal(bundle$taxonomic_dist_model[1, 2], 0.4)
  expect_equal(as.matrix(bundle$combined_dist)[1, 2], 0.9)
})
