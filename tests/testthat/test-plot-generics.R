test_that("plot.Candidates dispatches through base plot without replacing it", {
  candidates <- make_candidates()
  seen <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    plot_area_distribution = function(model_data,
                                      count_type = "studies") {
      seen$n_models <- nrow(model_data)
      seen$count_type <- count_type
      ggplot2::ggplot()
    },
    .package = "tsbiomass"
  )

  p <- plot(candidates, count_type = "models")
  p_unavailable <- plot(candidates, type = "species_policy_ranked")

  expect_s3_class(p, "ggplot")
  expect_s3_class(p_unavailable, "ggplot")
  expect_equal(p_unavailable$labels$title, "Candidates Plot Unavailable")
  expect_equal(seen$n_models, nrow(candidates@candidate_models))
  expect_equal(seen$count_type, "models")
})

test_that("strategy component competition plot handles selected rows with no parseable components", {
  interval_tbl <- tibble::tibble(
    anchor_species = c("Alpha alpha", "Beta beta"),
    policy = c(NA_character_, NA_character_),
    is_selected = c(TRUE, TRUE)
  )

  p <- tsbiomass:::plot_strategy_component_competition(interval_tbl)

  expect_s3_class(p, "ggplot")
})

test_that("plot.Candidates renders admissibility and anchor-review plots", {
  anchor_row <- minimal_candidate_models()[1, , drop = FALSE]
  scored_tbl <- tibble::tibble(
    anchor_model_id = c("1", "1"),
    anchor_species = c("Alpha alpha", "Alpha alpha"),
    species_name = c("Beta beta", "Gamma gamma"),
    model_id_chr = c("3", "4"),
    admissible = c(TRUE, TRUE),
    gate_frequency = c(TRUE, TRUE),
    frequency = c(38, 70),
    frequency_coherence_distance = c(0.00, 0.12),
    gate_missing_key_metadata = c(TRUE, TRUE),
    key_metadata_missing_fraction = c(0, 0),
    biomass_multiplier_if_replace = c(1.20, 0.92),
    w_adm = c(0.62, 0.38),
    w_combined = c(0.58, 0.42),
    combined_distance = c(0.22, 0.31),
    d_species = c(0.12, 0.21),
    d_study = c(0.18, 0.26),
    overlap_same_species = c(FALSE, FALSE),
    swimbladder_type = c("physostome", "physoclist")
  )
  ranked_tbl <- tibble::tibble(
    candidate_label = c("Beta beta {m3}", "Gamma gamma {m4}"),
    species_name = c("Beta beta", "Gamma gamma"),
    model_id_chr = c("3", "4"),
    w_adm = c(0.62, 0.38),
    biomass_multiplier_if_replace = c(1.20, 0.92)
  )
  overlap_tbl <- tibble::tibble(
    anchor_species = "Alpha alpha",
    w_same_species = 0.70,
    w_same_family = 0.92,
    w_same_swimbladder = 0.81,
    w_same_fao = 0.66,
    w_same_ocean_basin = 0.66,
    mean_length_overlap_fraction = 0.54,
    mean_depth_overlap_fraction = 0.43
  )
  gates_tbl <- tibble::tibble(
    anchor_species = c("Alpha alpha", "Alpha alpha"),
    inadmissible_reason = c("admissible", "self"),
    n_models = c(3L, 1L)
  )
  candidates <- make_candidates(
    admissibility = list(
      anchors = list(
        "1" = list(
          anchor = anchor_row,
          scored = scored_tbl,
          ranked = ranked_tbl,
          overlap = overlap_tbl,
          gates = gates_tbl,
          summary = tibble::tibble(
            combined_consensus_multiplier = 1.10,
            combined_multiplier_q05 = 0.94,
            combined_multiplier_q95 = 1.28
          )
        )
      ),
      all_scores = scored_tbl,
      all_overlap = overlap_tbl,
      all_gates = gates_tbl,
      anchor_summary = tibble::tibble(
        anchor_model_id = "1",
        anchor_species = "Alpha alpha",
        q05_multiplier_admissible = 0.94,
        q50_multiplier_admissible = 1.08,
        q95_multiplier_admissible = 1.28
      )
    )
  )

  p_gate <- plot(candidates, type = "admissibility", view = "gate_composition")
  p_overlap <- plot(candidates, type = "admissibility", view = "overlap_profile")
  p_top <- plot(candidates, type = "most_similar", anchor_model_id = "1")
  p_top_alias <- plot(candidates, type = "candidate_review", view = "top_candidates", anchor_model_id = "1")
  p_biomass <- plot(candidates, type = "candidate_biomass_response", anchor_species = "Alpha alpha")
  p_weights <- plot(candidates, type = "model_weights", anchor_species = "Alpha alpha")
  p_similarity <- plot(candidates, type = "candidate_review", view = "similarity_map", anchor_species = "Alpha alpha")

  expect_s3_class(p_gate, "ggplot")
  expect_s3_class(p_overlap, "ggplot")
  expect_s3_class(p_top, "ggplot")
  expect_s3_class(p_top_alias, "ggplot")
  expect_s3_class(p_biomass, "ggplot")
  expect_s3_class(p_weights, "ggplot")
  expect_s3_class(p_similarity, "ggplot")
  expect_null(p_gate$labels$title)
  expect_null(p_overlap$labels$title)
})

test_that("overlap heatmap discovers available summary metrics", {
  overlap_tbl <- tibble::tibble(
    anchor_species = "Alpha alpha",
    w_same_custom_trait = 0.75,
    mean_pressure_coherence = 0.50
  )

  p <- plot_overlap_heatmap(overlap_tbl)

  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$layout$panel_params[[1]]$x.range, c(0.5, 2.5))
  expect_equal(built$layout$panel_params[[1]]$y.range, c(0.5, 1.5))
  expect_setequal(
    as.character(unique(p$data$metric)),
    c("Same Custom Trait", "Mean Pressure Coherence")
  )
})

test_that("overlap heatmap restricts stale wide summaries to configured metrics", {
  overlap_tbl <- tibble::tibble(
    anchor_species = "Alpha alpha",
    w_same_family = 0.75,
    w_same_order = 0,
    w_same_derivation = 0,
    w_same_diel = 0,
    w_same_swimbladder = 0.25,
    w_same_swimbladder_type = 0.25,
    mean_length_overlap_fraction = 0.50
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

  p <- plot_overlap_heatmap(overlap_tbl, config = cfg)

  expect_s3_class(p, "ggplot")
  expect_setequal(
    as.character(unique(p$data$metric)),
    c("Same Family", "Same Swimbladder", "Mean Length Overlap")
  )
  expect_false("Same Order" %in% as.character(unique(p$data$metric)))
  expect_false("Same Derivation" %in% as.character(unique(p$data$metric)))
  expect_false("Same Diel" %in% as.character(unique(p$data$metric)))
})

test_that("overlap heatmap resolves legacy FAO summaries to configured FAO area", {
  overlap_tbl <- tibble::tibble(
    anchor_species = "Alpha alpha",
    w_same_fao = 0.66
  )
  cfg <- list(
    similarity = list(
      species_traits = character(0),
      study_traits = c("fao_area")
    )
  )

  p <- plot_overlap_heatmap(overlap_tbl, config = cfg)

  expect_s3_class(p, "ggplot")
  expect_identical(as.character(unique(p$data$metric)), "Same FAO Area")
  expect_equal(unique(p$data$value), 0.66)
})

test_that("overlap heatmap handles rows without finite metric columns", {
  overlap_tbl <- tibble::tibble(
    anchor_species = "Alpha alpha",
    anchor_model_id = "1",
    n_admissible = 2L
  )

  p <- plot_overlap_heatmap(overlap_tbl)

  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)
  expect_match(p$labels$subtitle, "no finite overlap metrics", ignore.case = TRUE)
})

test_that("plot.Candidates exposes ordination, tuning, and slope diagnostics", {
  models <- minimal_candidate_models()
  ordination_bundle <- list(
    model = list(
      points = tibble::tibble(
        model_id = models$model_id_chr,
        MDS1 = c(-0.4, -0.2, 0.3, 0.5),
        MDS2 = c(0.4, 0.1, -0.1, -0.5),
        species_name = models$species_name,
        common = models$common,
        is_reference = c(TRUE, FALSE, FALSE, TRUE),
        nmds_cluster_id = c("cluster_1", "cluster_1", "cluster_2", "cluster_2")
      ),
      loadings = tibble::tibble(
        trait = c("frequency", "length_min"),
        MDS1 = c(0.7, -0.4),
        MDS2 = c(0.2, 0.6),
        p_value = c(0.01, 0.03),
        r2 = c(0.45, 0.31)
      ),
      centroids = tibble::tibble(
        trait = "fao_area",
        level = c("61", "67"),
        MDS1 = c(-0.2, 0.4),
        MDS2 = c(0.3, -0.2),
        n = c(3L, 1L)
      ),
      hulls = tibble::tibble(
        MDS1 = c(-0.45, -0.15, -0.3, 0.2, 0.55, 0.35),
        MDS2 = c(0.45, 0.15, -0.05, -0.15, -0.45, -0.55),
        nmds_cluster_id = c("cluster_1", "cluster_1", "cluster_1", "cluster_2", "cluster_2", "cluster_2")
      )
    ),
    species = list(
      points = tibble::tibble(
        species_name = c("Alpha alpha", "Beta beta", "Gamma gamma"),
        MDS1 = c(-0.3, 0.1, 0.4),
        MDS2 = c(0.4, -0.1, -0.4),
        species_cluster_id = c("species_1", "species_1", "species_2"),
        is_reference = c(TRUE, FALSE, TRUE)
      ),
      loadings = tibble::tibble(
        trait = "family",
        MDS1 = 0.5,
        MDS2 = 0.3,
        p_value = 0.02,
        r2 = 0.40
      ),
      centroids = tibble::tibble(
        trait = "order",
        level = c("Ord1", "Ord2"),
        MDS1 = c(-0.2, 0.3),
        MDS2 = c(0.2, -0.3),
        n = c(2L, 1L)
      )
    )
  )
  candidates <- make_candidates(ordination = ordination_bundle)

  p_combined_hulls <- plot(candidates, type = "ordination")
  p_combined_vectors <- plot(candidates, type = "ordination", dissimilarity = "combined", view = "vectors")
  p_species_centers <- plot(candidates, type = "ordination", dissimilarity = "species", view = "centers")
  p_tuning <- plot(candidates, type = "tuning_variation")
  p_component <- plot(candidates, type = "similarity_tuning", view = "component_importance")
  p_slope_group <- plot(candidates, type = "slope_support", view = "group")

  expect_s3_class(p_combined_hulls, "ggplot")
  expect_s3_class(p_combined_vectors, "ggplot")
  expect_s3_class(p_species_centers, "ggplot")
  expect_s3_class(p_tuning, "ggplot")
  expect_s3_class(p_component, "ggplot")
  expect_s3_class(p_slope_group, "ggplot")
})

test_that("plot_gate_composition derives configured gate labels and omits self", {
  gate_tbl <- tibble::tibble(
    anchor_species = c("Alpha alpha", "Alpha alpha", "Alpha alpha", "Alpha alpha"),
    inadmissible_reason = c(
      "admissible",
      "trait_mismatch:family",
      "length_domain_nonoverlap",
      "metadata_missing_excess"
    ),
    n_models = c(4L, 2L, 1L, 1L)
  )
  cfg <- list(
    admissibility = list(
      species_traits = c("family"),
      study_traits = character(0),
      coherence = list(
        length = list(mode = "overlap", min = 0.01),
        depth = list(mode = "overlap", min = 0.01),
        frequency = list(mode = "none", gap = 60)
      ),
      key_metadata_max = 0.75
    )
  )

  p_gate <- plot_gate_composition(gate_tbl, config = cfg)

  expect_s3_class(p_gate, "ggplot")
  expect_null(p_gate$labels$title)
  expect_equal(
    levels(p_gate$data$gate_label),
    c(
      "Admissible",
      "Family mismatch",
      "Length nonoverlap",
      "Depth nonoverlap",
      "Missing metadata"
    )
  )
  expect_false("Self" %in% levels(p_gate$data$gate_label))
})

test_that("plot.Alchemist exposes admissibility summaries", {
  gate_tbl <- tibble::tibble(
    anchor_species = c("Alpha alpha", "Alpha alpha"),
    inadmissible_reason = c("admissible", "trait_mismatch:swimbladder_type"),
    n_models = c(3L, 1L)
  )
  overlap_tbl <- tibble::tibble(
    anchor_species = "Alpha alpha",
    w_same_species = 0.70,
    w_same_family = 0.92,
    w_same_swimbladder = 0.81,
    w_same_fao = 0.66,
    w_same_ocean_basin = 0.66,
    mean_length_overlap_fraction = 0.54,
    mean_depth_overlap_fraction = 0.43
  )
  alchemist <- Alchemist(
    candidates = make_candidates(),
    config = list(
      config_data = list(
        admissibility = list(
          species_traits = c("swimbladder_type"),
          study_traits = character(0),
          coherence = list(
            length = list(mode = "overlap", min = 0.01),
            depth = list(mode = "overlap", min = 0.01),
            frequency = list(mode = "none", gap = 60)
          ),
          key_metadata_max = 0.75
        )
      )
    ),
    learner = list(),
    distance_matrix = list(),
    trait_importance = list(),
    ordination = list(),
    admissibility = list(
      all_scores = tibble::tibble(
        anchor_model_id = "1",
        anchor_species = "Alpha alpha",
        gate_trait_swimbladder_type = TRUE,
        gate_frequency = TRUE,
        frequency = 38,
        frequency_coherence_distance = 0,
        gate_missing_key_metadata = TRUE,
        key_metadata_missing_fraction = 0,
        admissible = TRUE
      ),
      all_gates = gate_tbl,
      all_overlap = overlap_tbl
    )
  )

  p_gate <- plot(alchemist, type = "admissibility")
  p_overlap <- plot(alchemist, type = "admissibility", view = "overlap_profile")
  p_unavailable <- plot(alchemist, type = "species_policy_ranked")

  expect_s3_class(p_gate, "ggplot")
  expect_s3_class(p_overlap, "ggplot")
  expect_s3_class(p_unavailable, "ggplot")
  expect_equal(p_unavailable$labels$title, "Alchemist Plot Unavailable")
})

test_that("plot.Alchemist overlap profile handles overlap rows without metric columns", {
  alchemist <- Alchemist(
    candidates = make_candidates(),
    config = list(
      config_data = list(
        admissibility = list(
          species_traits = c("swimbladder_type"),
          study_traits = character(0),
          coherence = list(
            length = list(mode = "overlap", min = 0.01),
            depth = list(mode = "overlap", min = 0.01),
            frequency = list(mode = "none", gap = 60)
          ),
          key_metadata_max = 0.75
        )
      )
    ),
    learner = list(),
    distance_matrix = list(),
    trait_importance = list(),
    ordination = list(),
    admissibility = list(
      all_scores = tibble::tibble(
        anchor_model_id = "1",
        anchor_species = "Alpha alpha",
        gate_trait_swimbladder_type = TRUE,
        gate_missing_key_metadata = TRUE,
        key_metadata_missing_fraction = 0,
        admissible = TRUE
      ),
      all_gates = tibble::tibble(
        anchor_species = "Alpha alpha",
        inadmissible_reason = "admissible",
        n_models = 1L
      ),
      all_overlap = tibble::tibble(
        anchor_species = "Alpha alpha",
        anchor_model_id = "1",
        n_admissible = 1L
      )
    )
  )

  p_overlap <- plot(alchemist, type = "admissibility", view = "overlap_profile")

  expect_s3_class(p_overlap, "ggplot")
  expect_null(p_overlap$labels$title)
  expect_match(p_overlap$labels$subtitle, "no finite overlap metrics", ignore.case = TRUE)
})

test_that("plot.Alchemist supports species ordination and reference-species highlighting", {
  candidates <- set_reference_anchors(make_candidates(), selector = list(regional_body = "SWFSC"))
  species_points <- tibble::tibble(
    species_name = c("Alpha alpha", "Beta beta", "Gamma gamma"),
    MDS1 = c(-0.3, 0.1, 0.4),
    MDS2 = c(0.4, -0.1, -0.4),
    species_cluster_id = c("species_1", "species_1", "species_2"),
    is_reference = FALSE
  )
  alchemist <- Alchemist(
    candidates = candidates,
    config = list(),
    learner = list(),
    distance_matrix = list(),
    trait_importance = list(),
    ordination = list(species = list(points = species_points)),
    admissibility = list()
  )

  marked <- mark_ordination_reference_species(
    species_points,
    reference_species = ordination_reference_species(candidates@reference_anchors)
  )
  p_species <- plot(alchemist, type = "ordination", dissimilarity = "species")

  expect_true(any(marked$is_reference))
  expect_setequal(marked$species_name[marked$is_reference], candidates@reference_anchors$species_name)
  expect_s3_class(p_species, "ggplot")
  expect_silent(ggplot2::ggplot_build(p_species))
})

test_that("ordination reference marking preserves model-id reference flags", {
  points <- tibble::tibble(
    model_id = c("11", "21", "31"),
    species_name = c("Alpha alpha", "Alpha alpha", "Beta beta"),
    MDS1 = c(0, 1, 2),
    MDS2 = c(0, 1, 2),
    is_reference = c(TRUE, FALSE, FALSE)
  )

  marked <- mark_ordination_reference_species(
    points,
    reference_species = "Alpha alpha"
  )

  expect_identical(marked$is_reference, c(TRUE, FALSE, FALSE))
})

test_that("ordination reference marking can add one plotting representative per missing reference species", {
  points <- tibble::tibble(
    model_id = c("11", "12", "21", "31", "41"),
    species_name = c("Alpha alpha", "Alpha alpha", "Beta beta", "Gamma gamma", "Delta delta"),
    MDS1 = seq_len(5),
    MDS2 = seq_len(5),
    is_reference = c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )

  marked <- mark_ordination_reference_species(
    points,
    reference_species = c("Alpha alpha", "Beta beta", "Gamma gamma"),
    preserve_existing = FALSE
  )

  expect_identical(marked$is_reference, c(TRUE, FALSE, TRUE, TRUE, FALSE))
})

test_that("ordination vector labels use reference species names", {
  points <- tibble::tibble(
    model_id = c("11", "21"),
    species_name = c("Scomber Scombrus", "Beta beta"),
    common = c("Alpha common", "Beta common"),
    MDS1 = c(0, 1),
    MDS2 = c(0, 1),
    is_reference = c(TRUE, FALSE)
  )
  vectors <- tibble::tibble(
    trait = c("species_depth_min", "study_depth_min"),
    MDS1 = c(0.5, -0.35),
    MDS2 = c(0.25, 0.4),
    r2 = c(0.5, 0.4),
    p_value = c(0.01, 0.02)
  )

  p <- plot_ordination_vectors(vectors, points)

  expect_s3_class(p, "ggplot")
  expect_equal(p$data$trait_label, c("Minimum depth", "Minimum depth"))
  expect_equal(p$data$trait_scope, c("Species trait", "Survey trait"))
  label_layers <- Filter(function(layer) {
    "anchor_label" %in% names(layer$data %||% data.frame())
  }, p$layers)
  expect_true(length(label_layers) > 0L)
  expect_true("anchor_label" %in% names(p$layers[[length(p$layers)]]$data %||% data.frame()))
  expect_true(any(vapply(
    label_layers,
    function(layer) "Scomber scombrus" %in% layer$data$anchor_label,
    logical(1)
  )))
  ref_layer <- label_layers[[1]]$data |>
    dplyr::filter(.data$anchor_label == "Scomber scombrus")
  expect_equal(ref_layer$MDS1, 0)
  expect_equal(ref_layer$MDS2, 0)
})

test_that("ordination centroid labels use registry display values without duplicated trait prefixes", {
  points <- tibble::tibble(
    model_id = c("11", "21"),
    species_name = c("Alpha alpha", "Beta beta"),
    MDS1 = c(0, 1),
    MDS2 = c(0, 1),
    is_reference = c(TRUE, FALSE)
  )
  centers <- tibble::tibble(
    trait = c("ocean_basin", "pressure_corrected", "body_shape", "species", "family"),
    level = c("ocean basin Indian Ocean", "TRUE", "fusiform / normal", "Scomber Scombrus", "scombridae"),
    MDS1 = c(0.2, 0.3, 0.4, 0.5, 0.6),
    MDS2 = c(0.2, 0.3, 0.4, 0.5, 0.6),
    n = c(1L, 1L, 1L, 1L, 1L),
    p_value = c(0.01, 0.02, 0.03, 0.04, 0.50)
  )

  p <- plot_ordination_centers(centers, points)

  expect_s3_class(p, "ggplot")
  expect_true("Indian Ocean" %in% p$data$centroid_label)
  expect_true("Yes" %in% p$data$centroid_label)
  expect_true("Fusiform" %in% p$data$centroid_label)
  expect_true("Scomber scombrus" %in% p$data$centroid_label)
  expect_false("scombridae" %in% p$data$centroid_label)
  expect_true(all(c("Ocean basin", "Pressure corrected", "Body shape", "Species") %in% as.character(p$data$trait_label)))
  expect_false("family" %in% as.character(p$data$trait_label))
  expect_false(any(grepl("ocean basin ocean basin", p$data$centroid_label, ignore.case = TRUE)))
  point_layers <- Filter(function(layer) {
    inherits(layer$geom, "GeomPoint")
  }, p$layers)
  expect_true(all(vapply(
    point_layers,
    function(layer) "anchor_label" %in% names(layer$data %||% data.frame()),
    logical(1)
  )))
})

test_that("ordination hull plot uses closed polygon outlines", {
  points <- tibble::tibble(
    model_id = as.character(seq_len(4)),
    species_name = c("Alpha alpha", "Beta beta", "Gamma gamma", "Delta delta"),
    MDS1 = c(0, 1, 1, 0),
    MDS2 = c(0, 0, 1, 1),
    nmds_cluster_id = "cluster_1",
    is_reference = c(TRUE, FALSE, FALSE, FALSE)
  )
  hull <- tibble::tibble(
    MDS1 = c(0, 1, 1, 0),
    MDS2 = c(0, 0, 1, 1),
    nmds_cluster_id = "cluster_1"
  )

  p <- plot_ordination_cluster_hulls(points, hull, cluster_col = "nmds_cluster_id")

  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(layer) inherits(layer$geom, "GeomPolygon"), logical(1))))
  expect_false(any(vapply(p$layers, function(layer) inherits(layer$geom, "GeomPath"), logical(1))))
})

test_that("plot.PolicySelector dispatches benchmark and uncertainty summaries", {
  selector <- make_selector(
    benchmark = list(
      policy_perf = minimal_policy_performance(),
      species_block_perf = minimal_policy_performance()
    ),
    uncertainty = minimal_uncertainty()
  )

  p_policy <- plot(selector, type = "policy_benchmark")
  p_heat <- plot(selector, type = "strategy_error_heatmap")
  p_conf <- plot(selector, type = "conformal_scores")

  expect_s3_class(p_policy, "ggplot")
  expect_s3_class(p_heat, "ggplot")
  expect_s3_class(p_conf, "ggplot")
})

test_that("plot.PolicySelector returns diagnostics for unavailable plot types", {
  selector <- make_selector(
    benchmark = list(
      policy_perf = minimal_policy_performance(),
      species_block_perf = minimal_policy_performance()
    ),
    uncertainty = minimal_uncertainty()
  )

  p <- plot(selector, type = "species_policy_ranked")

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "PolicySelector Plot Unavailable")
  expect_match(p$labels$subtitle, "not a current PolicySelector plot type", fixed = TRUE)
})

test_that("policy heatmap drops requested anchors and policies without finite cells", {
  perf <- minimal_policy_performance() |>
    dplyr::mutate(
      policy_display = dplyr::if_else(
        .data$policy == "weighted_mean_within_genus",
        "NA NA",
        NA_character_
      )
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        anchor_model_id = "7",
        anchor_species = "Unused species",
        policy = "closest_within_species",
        policy_display = NA_character_,
        equation_branch_filter = "all",
        error_abs_log = 0.01,
        multiplier_pred = 1.01,
        valid_prediction = TRUE,
        n_valid_models = 1L,
        local_weighted_mean_combined_distance = 0.01,
        local_effective_support = 1,
        local_structural_q_abs_log = 0.01
      )
    )

  p_heat <- plot_policy_heatmap(
    perf,
    anchor_species = c("Alpha alpha", "Gamma gamma", "Missing missing"),
    max_policies = Inf,
    show_values = TRUE
  )

  expect_s3_class(p_heat, "ggplot")
  expect_false(any(grepl("^NA NA", as.character(p_heat$data$policy))))
  expect_setequal(
    as.character(unique(p_heat$data$anchor_species)),
    c("Alpha alpha", "Gamma gamma")
  )
  expect_match(p_heat$labels$subtitle, "Dropped requested species", fixed = TRUE)
  expect_match(p_heat$labels$subtitle, "Missing missing", fixed = TRUE)
  species_has_value <- p_heat$data |>
    dplyr::group_by(.data$anchor_species) |>
    dplyr::summarise(any_finite = any(is.finite(.data$median_abs_log_error)), .groups = "drop")
  policy_has_value <- p_heat$data |>
    dplyr::group_by(.data$policy) |>
    dplyr::summarise(any_finite = any(is.finite(.data$median_abs_log_error)), .groups = "drop")
  expect_true(all(species_has_value$any_finite))
  expect_true(all(policy_has_value$any_finite))
  expect_false("Unused species" %in% as.character(p_heat$data$anchor_species))

  p_component <- plot_policy_component_heatmap(
    perf,
    anchor_species = c("Alpha alpha", "Gamma gamma"),
    show_values = TRUE
  )
  expect_s3_class(p_component, "ggplot")
  expect_true(all(c("component", "component_level", "median_abs_log_error") %in% names(p_component$data)))
  expect_false(any(grepl("^NA NA", as.character(p_component$data$component_level))))
})

test_that("component heatmap preserves row ids after dropping invalid rows", {
  perf <- tibble::tibble(
    anchor_species = c(
      "Alpha alpha",
      "Alpha alpha",
      "Beta beta"
    ),
    policy = c(
      "closest_generalized",
      "closest_generalized",
      "closest_within_genus"
    ),
    equation_branch_filter = "all",
    error_abs_log = c(NA_real_, 0.5, 0.25),
    valid_prediction = c(FALSE, TRUE, TRUE)
  )

  p_component <- plot_policy_component_heatmap(perf, show_values = TRUE)
  cell <- p_component$data |>
    dplyr::filter(
      .data$anchor_species == "Alpha alpha",
      .data$component == "Candidate pool",
      .data$component_level == "Generalized models"
    )

  expect_equal(nrow(cell), 1L)
  expect_equal(cell$median_abs_log_error[[1]], 0.5)
})

test_that("component heatmap keeps generalized cells when none are finite", {
  perf <- tibble::tibble(
    anchor_species = "Alpha alpha",
    policy = "closest_within_species",
    equation_branch_filter = "all",
    error_abs_log = 0.25,
    valid_prediction = TRUE
  )

  p_component <- plot_policy_component_heatmap(perf, show_values = TRUE)
  cell <- p_component$data |>
    dplyr::filter(
      .data$anchor_species == "Alpha alpha",
      .data$component == "Candidate pool",
      .data$component_level == "Generalized models"
    )

  expect_equal(nrow(cell), 1L)
  expect_true(is.na(cell$median_abs_log_error[[1]]))
})

test_that("policy heatmap includes each displayed species best policy under policy limits", {
  perf <- tibble::tibble(
    anchor_model_id = c("1", "1", "2", "2", "3"),
    anchor_species = c("Alpha alpha", "Alpha alpha", "Beta beta", "Beta beta", "Gamma gamma"),
    policy = c("policy_a", "policy_b", "policy_a", "policy_b", "policy_c"),
    equation_branch_filter = "all",
    error_abs_log = c(0.10, 0.11, 0.10, 0.11, 0.90),
    multiplier_pred = c(1.1, 1.2, 1.1, 1.2, 2.0),
    valid_prediction = TRUE,
    n_valid_models = 1L,
    local_weighted_mean_combined_distance = 0.1,
    local_effective_support = 1,
    local_structural_q_abs_log = 0.1
  )

  p_heat <- plot_policy_heatmap(
    perf,
    anchor_species = c("Alpha alpha", "Beta beta", "Gamma gamma"),
    max_policies = 2L,
    show_values = TRUE
  )

  gamma_cells <- p_heat$data |>
    dplyr::filter(as.character(.data$anchor_species) == "Gamma gamma")

  expect_true(any(is.finite(gamma_cells$median_abs_log_error)))
})

test_that("policy display resolvers rebuild unusable NA labels", {
  display <- resolve_policy_display_names(tibble::tibble(
    policy_display = "NA NA",
    policy = "closest_within_species",
    equation_branch_filter = "all"
  ))
  selected_display <- resolve_selected_policy_names(tibble::tibble(
    selected_policy_display = "NA NA",
    selected_policy = "closest_within_species",
    selected_equation_branch_filter = "all"
  ))
  selected_component_display <- resolve_selected_policy_names(tibble::tibble(
    selected_policy_display = "NA NA [fixed-slope]",
    selected_policy = "closest_within_species",
    candidate_pool = NA_character_,
    aggregation_method = NA_character_,
    selected_equation_branch_filter = "fixed_slope"
  ))

  expect_false(any(grepl("^NA NA", display)))
  expect_false(any(grepl("^NA NA", selected_display)))
  expect_false(any(grepl("^NA NA", selected_component_display)))
  expect_true(grepl("species", display[[1]], ignore.case = TRUE))
  expect_true(grepl("species", selected_display[[1]], ignore.case = TRUE))
  expect_true(grepl("species", selected_component_display[[1]], ignore.case = TRUE))
})

test_that("policy display resolver preserves complete stored labels without rebuilding", {
  testthat::local_mocked_bindings(
    policy_display_label = function(...) stop("fallback label construction should not run"),
    .package = "tsbiomass"
  )

  display <- resolve_policy_display_names(tibble::tibble(
    policy_display = c("Species nearest [all slopes]", "Family averaged [fixed slope]"),
    policy = c("closest_within_species", "unweighted_mean_within_family"),
    equation_branch_filter = c("all", "fixed20_only")
  ))

  expect_equal(display, c("Species nearest [all slopes]", "Family averaged [fixed slope]"))
})

test_that("plot.PolicyPredictions dispatches selected intervals and policy competition", {
  predictions <- PolicyPredictions(
    intervals = tibble::tibble(
      anchor_model_id = c("1", "1", "4", "4"),
      anchor_species = c("Alpha alpha", "Alpha alpha", "Gamma gamma", "Gamma gamma"),
      policy = c("closest_within_species", "weighted_mean_within_genus", "closest_within_species", "weighted_mean_within_genus"),
      multiplier_pred = c(1.10, 1.30, 1.20, 1.38),
      multiplier_lo = c(1.00, 1.12, 1.08, 1.18),
      multiplier_hi = c(1.22, 1.52, 1.34, 1.61),
      valid_prediction = TRUE,
      is_selected = c(TRUE, FALSE, TRUE, FALSE)
    ),
    selections = tibble::tibble(
      anchor_model_id = c("1", "4"),
      anchor_species = c("Alpha alpha", "Gamma gamma"),
      selected_policy = c("closest_within_species", "closest_within_species"),
      selected_policy_display = c("closest_within_species", "closest_within_species"),
      multiplier_pred = c(1.10, 1.20),
      multiplier_lo = c(1.00, 1.08),
      multiplier_hi = c(1.22, 1.34)
    ),
    consensus = tibble::tibble()
  )

  p_selected <- plot(predictions, type = "selected_intervals")
  p_competition <- plot(predictions, type = "strategy_competition", anchor_species = "Alpha alpha")
  p_unavailable <- plot(predictions, type = "species_policy_ranked")

  expect_s3_class(p_selected, "ggplot")
  expect_s3_class(p_competition, "ggplot")
  expect_s3_class(p_unavailable, "ggplot")
  expect_equal(p_unavailable$labels$title, "PolicyPredictions Plot Unavailable")
})

test_that("plot.Scorecard and plot.Referee expose post-prediction figures", {
  selected_tbl <- tibble::tibble(
    anchor_model_id = c("1", "4"),
    anchor_species = c("Alpha alpha", "Gamma gamma"),
    selected_policy = c("closest_within_species", "closest_within_species"),
    selected_policy_display = c("closest_within_species", "closest_within_species"),
    multiplier_pred = c(1.10, 1.20),
    multiplier_lo = c(1.00, 1.08),
    multiplier_hi = c(1.22, 1.34),
    meta_post_selection_multiplier_lo = c(1.00, 1.08),
    meta_post_selection_multiplier_hi = c(1.22, 1.34),
    expected_length_cm = c(20, 24),
    length_support_min_cm = c(10, 15),
    length_support_max_cm = c(30, 28),
    policy_slope_len = c(20.0, 19.5),
    policy_slope_len_lo_95 = c(19.0, 18.8),
    policy_slope_len_hi_95 = c(21.0, 20.2),
    policy_intercept_len = c(-70.0, -69.0),
    policy_intercept_len_lo_95 = c(-71.0, -70.1),
    policy_intercept_len_hi_95 = c(-69.0, -67.9)
  )
  intervals_tbl <- tibble::tibble(
    anchor_model_id = c("1", "1", "4", "4"),
    anchor_species = c("Alpha alpha", "Alpha alpha", "Gamma gamma", "Gamma gamma"),
    policy = c("closest_within_species", "weighted_mean_within_genus", "closest_within_species", "weighted_mean_within_genus"),
    multiplier_pred = c(1.10, 1.30, 1.20, 1.38),
    multiplier_lo = c(1.00, 1.12, 1.08, 1.18),
    multiplier_hi = c(1.22, 1.52, 1.34, 1.61),
    valid_prediction = TRUE,
    is_selected = c(TRUE, FALSE, TRUE, FALSE)
  )
  ts_panel_tbl <- tibble::tibble(
    anchor_model_id = rep(c("1", "4"), each = 2),
    anchor_species = rep(c("Alpha alpha", "Gamma gamma"), each = 2),
    selected_policy = rep(c("closest_within_species", "closest_within_species"), each = 2),
    length_cm = rep(c(10, 20), 2),
    ts_pred = c(-55, -50, -57, -52),
    ts_anchor = c(-54, -49, -56, -51),
    ts_top_candidate = c(-55.5, -50.4, -57.5, -52.4),
    ts_lo_99 = c(-58, -53, -60, -55),
    ts_hi_99 = c(-52, -47, -54, -49),
    ts_lo_95 = c(-57, -52, -59, -54),
    ts_hi_95 = c(-53, -48, -55, -50),
    ts_lo_90 = c(-56.5, -51.5, -58.5, -53.5),
    ts_hi_90 = c(-53.5, -48.5, -55.5, -50.5),
    ts_lo_80 = c(-56, -51, -58, -53),
    ts_hi_80 = c(-54, -49, -56, -51)
  )
  anchor_summary_tbl <- tibble::tibble(
    anchor_model_id = c("1", "4"),
    anchor_species = c("Alpha alpha", "Gamma gamma"),
    selected_policy_display = c("closest_within_species", "closest_within_species"),
    multiplier_pred = c(1.10, 1.20),
    multiplier_lo = c(1.00, 1.08),
    multiplier_hi = c(1.22, 1.34),
    combined_multiplier_q05 = c(0.95, 1.02),
    combined_multiplier_q50 = c(1.08, 1.16),
    combined_multiplier_q95 = c(1.24, 1.36)
  )
  anchor_audit_tbl <- tibble::tibble(
    anchor_species = c("Alpha alpha", "Gamma gamma"),
    selected_policy_display = c("closest_within_species", "closest_within_species"),
    empirical_coverage = c(0.90, 0.88),
    interval_log_width = c(0.20, 0.24),
    local_effective_support = c(3.0, 2.5),
    local_mean_combined_distance = c(0.12, 0.18)
  )
  field_missing_tbl <- tibble::tibble(
    field = c("family", "frequency"),
    missing_fraction = c(0.10, 0.04)
  )
  anchor_missing_tbl <- tibble::tibble(
    anchor_species = c("Alpha alpha", "Gamma gamma"),
    prop_fail_missing_metadata = c(0.08, 0.15)
  )
  status_tbl <- tibble::tibble(
    component = character(),
    status = character(),
    message = character()
  )
  scorecard <- Scorecard(
    intervals = intervals_tbl,
    selected = selected_tbl,
    ts_panel = ts_panel_tbl,
    recommendation_cards = tibble::tibble(),
    surrogate_rules = tibble::tibble(),
    consensus = tibble::tibble(),
    anchor_summary = anchor_summary_tbl,
    anchor_audit = anchor_audit_tbl,
    species_coverage = tibble::tibble(),
    selection_diagnostics = tibble::tibble(),
    key_missing_overall = tibble::tibble(),
    key_missing_by_field = field_missing_tbl,
    key_missing_by_model = tibble::tibble(),
    anchor_missing_gate = anchor_missing_tbl,
    status = status_tbl
  )
  candidates <- make_candidates(
    admissibility = list(
      all_scores = tibble::tibble(
        anchor_model_id = c("1", "1", "4", "4"),
        anchor_species = c("Alpha alpha", "Alpha alpha", "Gamma gamma", "Gamma gamma"),
        model_id_chr = c("2", "3", "1", "2"),
        species_name = c("Alpha alpha", "Beta beta", "Alpha alpha", "Alpha alpha"),
        admissible = c(TRUE, TRUE, TRUE, TRUE),
        biomass_multiplier_if_replace = c(1.00, 1.18, 1.12, 1.28),
        slope_len = c(18, 20, 20, 18),
        intercept_len = c(-68, -65, -70, -68),
        w_adm = c(0.55, 0.45, 0.60, 0.40),
        w_combined = c(0.52, 0.48, 0.58, 0.42),
        lw_a_g = c(0.013, 0.011, 0.012, 0.013),
        lw_b = c(3.1, 2.9, 3.0, 3.1)
      )
    )
  )
  selector <- make_selector(candidates = candidates)
  referee <- Referee(
    selector = selector,
    learner = NULL,
    predictions = NULL,
    config = list(),
    scorecard = scorecard
  )

  p_ts <- plot(scorecard, type = "ts_length_conformal")
  p_ts_top <- plot(scorecard, type = "ts_length_conformal", show_top_candidate = TRUE)
  p_ts_alias <- plot(scorecard, type = "ts_length", scale = "ts")
  p_mult <- plot(scorecard, type = "ts_length_multiplier")
  p_mult_alias <- plot(scorecard, type = "ts_length", scale = "multiplier")
  p_bands <- plot(scorecard, type = "ts_length_bands", anchor_species = "Alpha alpha")
  p_bands_top <- plot(scorecard, type = "ts_length_bands", anchor_species = "Alpha alpha", show_top_candidate = TRUE)
  p_coef <- plot(scorecard, type = "coefficient_uncertainty")
  p_selected <- plot(scorecard, type = "selected_intervals")
  p_selected_multiplier <- plot(scorecard, type = "selected_multiplier_summary")
  p_selected_counts <- plot(scorecard, type = "selected_policy_counts")
  p_selected_counts_anchor <- plot(scorecard, type = "selected_policy_counts", view = "by_anchor")
  p_competition_scorecard <- plot(scorecard, type = "strategy_competition", anchor_species = "Alpha alpha")
  p_competition_components <- plot(scorecard, type = "strategy_competition", view = "components")
  p_field_missing <- plot(scorecard, type = "field_missingness")
  p_summary <- plot(referee, type = "biomass_change")
  p_competition <- plot(referee, type = "strategy_competition", anchor_species = "Alpha alpha")
  p_referee_competition_components <- plot(referee, type = "strategy_competition", view = "components")
  p_competition_alias <- plot(referee, type = "biomass_change", view = "strategy_competition", anchor_species = "Alpha alpha")
  p_length_density <- plot(referee, type = "length_density", anchor_species = "Alpha alpha")
  p_length_density_panel <- plot(referee, type = "length_density")
  p_scorecard_unavailable <- plot(scorecard, type = "species_policy_ranked")
  p_referee_unavailable <- plot(referee, type = "species_policy_ranked")

  expect_s3_class(p_ts, "ggplot")
  expect_s3_class(p_ts_top, "ggplot")
  expect_s3_class(p_ts_alias, "ggplot")
  expect_s3_class(p_mult, "ggplot")
  expect_s3_class(p_mult_alias, "ggplot")
  expect_s3_class(p_bands, "ggplot")
  expect_s3_class(p_bands_top, "ggplot")
  expect_s3_class(p_coef, "ggplot")
  expect_s3_class(p_selected, "ggplot")
  expect_s3_class(p_selected_multiplier, "ggplot")
  expect_s3_class(p_selected_counts, "ggplot")
  expect_s3_class(p_selected_counts_anchor, "ggplot")
  expect_s3_class(p_competition_scorecard, "ggplot")
  expect_s3_class(p_competition_components, "ggplot")
  expect_s3_class(p_field_missing, "ggplot")
  expect_s3_class(p_summary, "ggplot")
  expect_s3_class(p_competition, "ggplot")
  expect_s3_class(p_referee_competition_components, "ggplot")
  expect_s3_class(p_competition_alias, "ggplot")
  expect_s3_class(p_length_density, "ggplot")
  expect_s3_class(p_length_density_panel, "ggplot")
  expect_s3_class(p_scorecard_unavailable, "ggplot")
  expect_s3_class(p_referee_unavailable, "ggplot")
  expect_equal(p_scorecard_unavailable$labels$title, "Scorecard Plot Unavailable")
  expect_equal(p_referee_unavailable$labels$title, "Referee Plot Unavailable")
  expect_true(all(c("component", "component_level", "n_selected") %in% names(p_competition_components$data)))
  expect_false(any(grepl("^NA NA", as.character(p_competition_components$data$component_level))))
  ts_text_layers <- Filter(function(layer) inherits(layer$geom, "GeomText"), p_ts$layers)
  expect_true(length(ts_text_layers) > 0L)
  expect_true(any(vapply(ts_text_layers, function(layer) {
    identical(layer$geom_params$parse, TRUE)
  }, logical(1))))
})

test_that("plot_selected_intervals uses the selected-policy interval columns", {
  plot_tbl <- tibble::tibble(
    anchor_species = "Alpha alpha",
    multiplier_pred = 2.0,
    multiplier_lo = 0.5,
    multiplier_hi = 5.0,
    meta_post_selection_multiplier_lo = 1.8,
    meta_post_selection_multiplier_hi = 2.2,
    selected_policy = "weighted_mean_within_family",
    selected_policy_display = "Weighted mean within family"
  )

  p <- plot_selected_intervals(plot_tbl)
  built <- ggplot2::ggplot_build(p)
  errorbar_data <- built[["data"]][[2]]

  expect_equal(errorbar_data$y[[1]], log10(2.0), tolerance = 1e-8)
  expect_equal(errorbar_data$ymin[[1]], log10(0.5), tolerance = 1e-8)
  expect_equal(errorbar_data$ymax[[1]], log10(5.0), tolerance = 1e-8)
})

test_that("TS interval display columns are centered on selected policy curve", {
  curve_tbl <- tibble::tibble(
    length_cm = c(10, 20),
    ts_pred = c(-60, -54),
    ts_center = c(-60, -54),
    ts_lo_80 = c(-61, -57),
    ts_hi_80 = c(-58, -53),
    ts_lo_90 = c(-62, -58),
    ts_hi_90 = c(-57, -52),
    ts_lo_95 = c(-63, -59),
    ts_hi_95 = c(-56, -51),
    ts_lo_99 = c(-64, -60),
    ts_hi_99 = c(-55, -50)
  )

  centered <- tsbiomass:::center_ts_interval_columns(curve_tbl)

  for (level in c("80", "90", "95", "99")) {
    lo <- centered[[paste0("ts_lo_", level)]]
    hi <- centered[[paste0("ts_hi_", level)]]
    expect_equal(centered$ts_center - lo, hi - centered$ts_center, tolerance = 1e-8)
  }
})

test_that("reference length-density plots display stored empirical PDFs as densities", {
  raw_lengths <- c(10, 10, 11, 12, 12, 12, 22, 22, 24, 25)
  support_tbl <- normalize_anchor_pdf_input(raw_lengths) |>
    dplyr::mutate(anchor_species = "Alpha alpha")

  p <- plot_length_density_panel(support_tbl)
  built <- ggplot2::ggplot_build(p)
  line_data <- built[["data"]][[1]]

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Reference Anchor Length Density")
  expect_gt(nrow(line_data), length(unique(raw_lengths)))
  expect_true(all(is.finite(line_data$x)))
  expect_true(all(is.finite(line_data$y)))
})

test_that("scorecard plots expose unavailable inputs instead of blank canvases", {
  p_selected <- plot_selected_intervals(tibble::tibble())
  p_coef <- plot_policy_coefficients(tibble::tibble())
  p_ts <- plot_ts_panel(tibble::tibble())
  p_multiplier <- plot_multiplier_length_spectrum(tibble::tibble())
  p_competition <- plot_interval_panel(tibble::tibble())

  expect_equal(p_selected$labels$title, "Selected Biomass Multiplier Intervals")
  expect_equal(p_coef$labels$title, "Selected Policy Coefficient Uncertainty")
  expect_equal(p_ts$labels$title, "TS Length Conformal Bands")
  expect_equal(p_multiplier$labels$title, "Length-Specific Biomass Multiplier")
  expect_equal(p_competition$labels$title, "Strategy Competition")
})

test_that("plot.Referee uses stored predictions when scorecards are not yet stored", {
  candidates <- set_reference_anchors(
    make_candidates(
      admissibility = list(
        all_scores = minimal_admissibility_scores() |>
          dplyr::mutate(
            biomass_multiplier_if_replace = c(1.00, 1.18, 1.12, 1.26, 1.09, 1.21)
          )
      )
    ),
    selector = list(regional_body = "SWFSC")
  )
  selector <- make_selector(
    candidates = candidates,
    benchmark = list(
      policy_perf = minimal_policy_performance(),
      species_block_perf = minimal_policy_performance()
    ),
    uncertainty = minimal_uncertainty(),
    selection = list(final_ref = minimal_selection_ref())
  )

  testthat::local_mocked_bindings(
    screen_one_anchor_admissibility = function(...) list(),
    evaluate_policies = function(...) {
      tibble::tibble(
        policy = c("closest_within_species", "weighted_mean_within_genus"),
        equation_branch_filter = "all",
        multiplier_pred = c(1.10, 1.30)
      )
    },
    summarize_evaluation = function(...) {
      tibble::tibble(
        consensus_multiplier = 1.20,
        multiplier_q05 = 1.00,
        multiplier_q50 = 1.20,
        multiplier_q95 = 1.40,
        local_support_mass = 0.80,
        local_effective_support = 3.00
      )
    },
    .package = "tsbiomass"
  )

  predictions <- predict(selector)
  referee <- as_referee(selector, predictions = predictions)

  expect_equal(nrow(referee@scorecard@selected), 0)

  p_summary <- plot(referee, type = "biomass_change")

  expect_s3_class(p_summary, "ggplot")
})

test_that("plot.PolicyLearner exposes calibration and residual diagnostics", {
  all_rows <- minimal_crossfit_predictions() |>
    dplyr::mutate(
      .outcome = error_abs_log,
      .outcome_raw = c(0.12, 0.21, 0.11, 0.23),
      .outcome_was_clipped = c(TRUE, FALSE, FALSE, TRUE),
      post_selection_support_bin = c("bin_1", "bin_2", "bin_1", "bin_2"),
      post_selection_support_label = c("Lower support", "Higher support", "Lower support", "Higher support"),
      policy_display = policy
    )
  selected_rows <- all_rows |>
    dplyr::group_by(anchor_model_id, anchor_species) |>
    dplyr::slice_min(.meta_predicted_score, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      selected_policy = policy,
      selected_policy_display = policy,
      selected_equation_branch_filter = equation_branch_filter
    )
  learner <- PolicyLearner(
    selector = make_selector(
      benchmark = list(
        species_block_perf = minimal_policy_performance()
      )
    ),
    config = list(),
    training_data = tibble::as_tibble(all_rows),
    crossfit = list(
      result = list(predictions = all_rows),
      outcome_col = "error_abs_log"
    ),
    fitted_model = list(method = "glm"),
    calibration = list(
      predictions = all_rows,
      selected = selected_rows,
      outcome_col = "error_abs_log",
      use_support_bin_intervals = TRUE
    )
  )

  p_predicted <- plot(learner, type = "predicted_vs_observed")
  p_calibration <- plot(learner, type = "calibration_curve")
  p_calibration_raw <- plot(learner, type = "calibration_curve", outcome = "raw")
  p_selected_calibration <- plot(learner, type = "calibration_curve", rows = "selected", outcome = "raw")
  p_residual_policy <- plot(learner, type = "residuals", view = "by_policy", outcome = "raw")
  p_residual_branch <- plot(learner, type = "residuals", view = "by_branch", outcome = "raw")
  p_score <- plot(learner, type = "score_by_policy")
  p_support <- plot(learner, type = "support_bin_error", outcome = "raw")
  p_counts <- plot(learner, type = "selected_policy_counts")
  p_counts_anchor <- plot(learner, type = "selected_policy_counts", view = "by_anchor")
  p_stability <- plot(learner, type = "recommendation_stability")
  p_unavailable <- plot(learner, type = "species_policy_ranked")

  expect_s3_class(p_predicted, "ggplot")
  expect_s3_class(p_calibration, "ggplot")
  expect_s3_class(p_calibration_raw, "ggplot")
  expect_s3_class(p_selected_calibration, "ggplot")
  expect_s3_class(p_residual_policy, "ggplot")
  expect_s3_class(p_residual_branch, "ggplot")
  expect_s3_class(p_score, "ggplot")
  expect_s3_class(p_support, "ggplot")
  expect_s3_class(p_counts, "ggplot")
  expect_s3_class(p_counts_anchor, "ggplot")
  expect_s3_class(p_stability, "ggplot")
  expect_s3_class(p_unavailable, "ggplot")
  expect_equal(p_unavailable$labels$title, "PolicyLearner Plot Unavailable")
  expect_silent(ggplot2::ggplot_build(p_stability))
})

test_that("plot.PolicyLearner filters to reference anchors and repairs NA labels", {
  all_rows <- minimal_crossfit_predictions() |>
    dplyr::mutate(
      .outcome = error_abs_log,
      .outcome_raw = error_abs_log,
      policy_display = "NA NA"
    )
  selected_rows <- all_rows |>
    dplyr::group_by(anchor_model_id, anchor_species) |>
    dplyr::slice_min(.meta_predicted_score, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      selected_policy = policy,
      selected_policy_display = "Unresolved policy [all]",
      selected_equation_branch_filter = NA_character_
    )
  candidates <- set_reference_anchors(make_candidates(), selector = list(regional_body = "SWFSC"))
  learner <- PolicyLearner(
    selector = make_selector(
      candidates = candidates,
      benchmark = list(species_block_perf = minimal_policy_performance())
    ),
    config = list(),
    training_data = tibble::as_tibble(all_rows),
    crossfit = list(result = list(predictions = all_rows), outcome_col = "error_abs_log"),
    fitted_model = list(method = "glm"),
    calibration = list(
      predictions = all_rows,
      selected = selected_rows,
      outcome_col = "error_abs_log",
      use_support_bin_intervals = FALSE
    )
  )

  p_counts <- plot(learner, type = "selected_policy_counts")
  p_stability <- plot(learner, type = "recommendation_stability")

  expect_setequal(
    as.character(unique(p_stability$data$anchor_species)),
    policy_learner_reference_anchor_species(learner)
  )
  expect_false(any(grepl("^NA NA", as.character(p_counts$data$selected_policy_display))))
  expect_false(any(grepl("^NA NA", as.character(p_stability$data$top_policy))))
  expect_false(any(grepl("^Unresolved policy", as.character(p_stability$data$top_policy))))
  p_support <- plot(learner, type = "support_bin_error")
  expect_s3_class(p_support, "ggplot")
  expect_match(p_support$labels$subtitle, "Support-bin intervals are not enabled", fixed = TRUE)
})

test_that("plot_tuning_variation builds resample labels", {
  p <- plot_tuning_variation(
    tibble::tibble(
      block = c("length", "depth"),
      mean_multiplier = c(1.10, 0.85),
      q05_multiplier = c(0.90, 0.70),
      q95_multiplier = c(1.25, 1.05),
      sd_multiplier = c(0.12, 0.08)
    )
  )

  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot.PolicySimulator exposes sensitivity summaries", {
  sensitivity_rows <- dplyr::bind_rows(
    minimal_selection_ref() |>
      dplyr::slice(1) |>
      dplyr::mutate(scenario = "baseline", .before = 1),
    minimal_selection_ref() |>
      dplyr::slice(2) |>
      dplyr::mutate(scenario = "stricter_overlap_0_50", .before = 1)
  )
  anchor_selected_rows <- tibble::tibble(
    scenario = c("baseline", "baseline", "stricter_overlap_0_50", "stricter_overlap_0_50"),
    anchor_model_id = c("1", "4", "1", "4"),
    anchor_species = c("Alpha alpha", "Gamma gamma", "Alpha alpha", "Gamma gamma"),
    selected_policy = c(
      "closest_within_species",
      "closest_within_genus",
      "closest_within_species",
      "closest_within_family"
    ),
    selected_policy_display = c(
      "closest_within_species",
      "closest_within_genus",
      "closest_within_species",
      "closest_within_family"
    ),
    multiplier_pred = c(1.10, 1.25, 1.18, 1.05),
    equivalence_class_members = c(
      "closest_within_species",
      "closest_within_genus",
      "closest_within_species",
      "closest_within_family"
    )
  )
  simulator <- PolicySimulator(
    selector = make_selector(),
    config = list(),
    scenarios = list(),
    results = list(),
    manifest = tibble::tibble(scenario = c("baseline", "stricter_overlap_0_50")),
    tables = list(
      select_ref = sensitivity_rows,
      anchor_selected = anchor_selected_rows,
      equiv_pairs = tibble::tibble(),
      equiv_sets = tibble::tibble(),
      conf_cal = tibble::tibble()
    )
  )

  p <- plot(simulator)
  p_stability <- plot(simulator, type = "policy_stability")
  p_drift <- plot(simulator, type = "multiplier_drift")
  p_unavailable <- plot(simulator, type = "species_policy_ranked")

  expect_s3_class(p, "ggplot")
  expect_s3_class(p_stability, "ggplot")
  expect_s3_class(p_drift, "ggplot")
  expect_s3_class(p_unavailable, "ggplot")
  expect_equal(p_unavailable$labels$title, "PolicySimulator Plot Unavailable")
})
