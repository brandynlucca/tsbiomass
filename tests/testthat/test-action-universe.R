test_that("action universe includes exhaustive donors and configured ensembles", {
  donors <- tibble::tibble(
    model_id = c("fixed", "free_a", "free_b"),
    species_name = c("Alpha alpha", "Beta beta", "Gamma gamma"),
    slope_len = c(20, 18, 22),
    intercept_len = c(-70, -65, -75),
    admissible = TRUE,
    combined_distance = c(0.1, 0.2, 0.3),
    d_study = c(0.3, 0.1, 0.2),
    d_species = c(0.2, 0.3, 0.1),
    taxonomic_distance_to_anchor = c(2, 3, 1),
    w_adm = c(0.6, 0.3, 0.1),
    study_cell_id = c("a", "b", "c"),
    is_group_model = FALSE
  )
  eval_obj <- list(admissible_df = donors)
  anchor <- tibble::tibble(model_id = "anchor", species_name = "Anchor species")
  policies <- c(
    "closest_across_all_admissible",
    "weighted_mean_across_all_admissible",
    "unweighted_mean_across_all_admissible"
  )
  params <- list(slope_class = c("all", "fixed20_only", "free_slope_only"))

  out <- tsbiomass:::enumerate_action_universe(
    eval_obj = eval_obj,
    anchor_row = anchor,
    policies = policies,
    policy_params = params
  )

  singleton_aliases <- dplyr::filter(
    out$aliases,
    .data$source_type == "exhaustive_singleton"
  )
  expect_setequal(
    unique(singleton_aliases$equation_branch_filter),
    c("all", "fixed20_only", "free_slope_only")
  )
  expect_equal(nrow(singleton_aliases), 6L)
  expect_true(all(c("fixed", "free_a", "free_b") %in% out$actions$donor_ids))
  expect_true(any(out$actions$n_donors == 3L))
  expect_true(any(out$actions$n_policy_aliases > 1L))
  expect_equal(nrow(out$invalid_policy_branches), 0L)
})

test_that("action identity and aliases are invariant to donor row order", {
  donors <- tibble::tibble(
    model_id = c("d3", "d1", "d2"),
    species_name = c("Gamma gamma", "Alpha alpha", "Beta beta"),
    slope_len = c(22, 20, 18),
    intercept_len = c(-75, -70, -65),
    admissible = TRUE,
    combined_distance = c(0.3, 0.1, 0.2),
    w_adm = c(0.1, 0.6, 0.3),
    study_cell_id = c("c", "a", "b"),
    is_group_model = FALSE
  )
  anchor <- tibble::tibble(model_id = "anchor", species_name = "Anchor species")
  policies <- c(
    "closest_across_all_admissible",
    "weighted_mean_across_all_admissible",
    "unweighted_mean_across_all_admissible"
  )
  params <- list(slope_class = c("all", "fixed20_only", "free_slope_only"))
  run <- function(x) {
    tsbiomass:::enumerate_action_universe(
      eval_obj = list(admissible_df = x),
      anchor_row = anchor,
      policies = policies,
      policy_params = params
    )
  }

  forward <- run(donors)
  reverse <- run(donors[rev(seq_len(nrow(donors))), , drop = FALSE])

  expect_identical(forward$actions, reverse$actions)
  expect_identical(forward$aliases, reverse$aliases)
  expect_identical(forward$invalid_policy_branches, reverse$invalid_policy_branches)
})

test_that("unavailable named distance invalidates only the corresponding branch", {
  donors <- tibble::tibble(
    model_id = c("a", "b"),
    species_name = c("Alpha alpha", "Beta beta"),
    slope_len = c(20, 18),
    intercept_len = c(-70, -65),
    admissible = TRUE,
    combined_distance = c(0.1, 0.2),
    d_study = NA_real_,
    w_adm = c(0.7, 0.3),
    is_group_model = FALSE
  )
  out <- tsbiomass:::enumerate_action_universe(
    eval_obj = list(admissible_df = donors),
    anchor_row = tibble::tibble(model_id = "anchor", species_name = "Anchor species"),
    policies = c(
      "closest_across_all_admissible",
      "survey_distance_across_all_admissible"
    ),
    policy_params = list(slope_class = "all")
  )

  expect_true("closest_across_all_admissible" %in% out$aliases$policy)
  expect_false("survey_distance_across_all_admissible" %in% out$aliases$policy)
  expect_equal(nrow(out$invalid_policy_branches), 1L)
  expect_equal(
    out$invalid_policy_branches$invalid_reason,
    "required_distance_unavailable:d_study"
  )
})

test_that("closest study cell does not substitute a row when identity is missing", {
  rows <- tibble::tibble(
    model_id = c("a", "b"),
    slope_len = c(20, 18),
    intercept_len = c(-70, -65),
    combined_distance = c(0.1, 0.2),
    study_cell_id = c(NA_character_, "cell_b"),
    is_group_model = FALSE
  )
  policy_def <- tsbiomass:::build_policy_definition_from_name(
    "closest_study_cell_species"
  )
  out <- tsbiomass:::policy_rows(
    rows = rows,
    policy_def = policy_def,
    policy_params = list(match_traits = character()),
    ordination_info = tsbiomass:::resolve_policy_context(NULL)
  )

  expect_equal(nrow(out), 0L)
})

test_that("implicit phylogenetic cascade is rejected", {
  rows <- tibble::tibble(
    model_id = "a",
    slope_len = 20,
    intercept_len = -70,
    combined_distance = 0.1,
    overlap_same_species = TRUE,
    overlap_same_genus = TRUE,
    overlap_same_family = TRUE,
    overlap_same_order = TRUE,
    is_group_model = FALSE
  )
  expect_error(
    tsbiomass:::policy_rows(
      rows = rows,
      policy_def = list(candidate_pool = "nearest_phylogenetic"),
      policy_params = list(),
      ordination_info = tsbiomass:::resolve_policy_context(NULL)
    ),
    "implicit species/genus/family/order cascade"
  )
})

test_that("an empty admissible set returns explicit branch failures", {
  donors <- tibble::tibble(
    model_id = character(),
    species_name = character(),
    slope_len = numeric(),
    intercept_len = numeric(),
    admissible = logical(),
    combined_distance = numeric(),
    w_adm = numeric()
  )
  out <- tsbiomass:::enumerate_action_universe(
    eval_obj = list(admissible_df = donors),
    anchor_row = tibble::tibble(model_id = "anchor", species_name = "Anchor species"),
    policies = c(
      "closest_across_all_admissible",
      "weighted_mean_across_all_admissible"
    ),
    policy_params = list(slope_class = c("all", "fixed20_only", "free_slope_only"))
  )

  expect_equal(nrow(out$actions), 0L)
  expect_equal(nrow(out$aliases), 0L)
  expect_equal(nrow(out$invalid_policy_branches), 6L)
  expect_true(all(out$invalid_policy_branches$invalid_reason == "no_admissible_donors"))
})

test_that("a gate-admissible donor with no finite equation is never silent", {
  donors <- tibble::tibble(
    model_id = c("usable", "missing_equation"),
    species_name = c("Alpha alpha", "Beta beta"),
    slope_len = c(20, NA_real_),
    intercept_len = c(-70, NA_real_),
    admissible = TRUE,
    combined_distance = c(0.1, 0.2),
    w_adm = c(0.7, 0.3),
    is_group_model = FALSE
  )
  out <- tsbiomass:::enumerate_action_universe(
    eval_obj = list(
      model_eval = donors,
      admissible_df = dplyr::filter(
        donors,
        is.finite(.data$slope_len), is.finite(.data$intercept_len)
      )
    ),
    anchor_row = tibble::tibble(model_id = "anchor", species_name = "Anchor species"),
    policies = "closest_across_all_admissible",
    policy_params = list(slope_class = "all")
  )

  expect_equal(out$invalid_singleton_donors$donor_model_id, "missing_equation")
  expect_equal(
    out$invalid_singleton_donors$invalid_reason,
    "nonfinite_standardized_equation"
  )
  expect_false(any(grepl("missing_equation", out$actions$donor_ids, fixed = TRUE)))
  expect_true(any(out$actions$donor_ids == "usable"))
})
