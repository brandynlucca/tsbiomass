test_that("taxonomic distance queries full binomials, not epithets", {
  models <- tibble::tibble(
    species_name = c("Engraulis mordax", "Osmerus mordax", "Engraulis japonicus"),
    family = c("Engraulidae", "Osmeridae", "Engraulidae"),
    genus = c("Engraulis", "Osmerus", "Engraulis"),
    species = c("mordax", "mordax", "japonicus")
  )
  phylo <- matrix(
    c(0, 1, 0.2, 1, 0, 1, 0.2, 1, 0),
    nrow = 3,
    byrow = TRUE
  )
  attr(phylo, "taxonomic_distance_method") <- "open_tree_node_grafen"

  testthat::local_mocked_bindings(
    alchemist_open_tree_node_distance = function(species_labels) {
      testthat::expect_equal(species_labels, models$species_name)
      phylo
    },
    .package = "tsbiomass"
  )
  out <- tsbiomass:::tax_dist_mat(
    models,
    c(family = "family", genus = "genus", species = "species")
  )

  expect_equal(out[1, 2], 1)
  expect_equal(out[1, 3], 0.2)
  expect_equal(attr(out, "taxonomic_distance_method"), "open_tree_node_grafen")
})

test_that("taxonomic distance builds binomials and does not rank-fallback to shared epithets", {
  models <- tibble::tibble(
    family = c("Engraulidae", "Osmeridae"),
    genus = c("Engraulis", "Osmerus"),
    species = c("mordax", "mordax")
  )
  phylo <- matrix(
    c(0, NA, NA, 0),
    nrow = 2,
    byrow = TRUE
  )
  attr(phylo, "taxonomic_distance_method") <- "open_tree_failed"

  testthat::local_mocked_bindings(
    alchemist_open_tree_node_distance = function(species_labels) {
      testthat::expect_equal(species_labels, c("Engraulis mordax", "Osmerus mordax"))
      phylo
    },
    .package = "tsbiomass"
  )
  out <- tsbiomass:::tax_dist_mat(
    models,
    c(family = "family", genus = "genus", species = "species")
  )

  expect_true(is.na(out[1, 2]))
  expect_equal(attr(out, "taxonomic_distance_method"), "open_tree_failed")
})

test_that("taxonomic distance is unavailable without a binomial species label", {
  models <- tibble::tibble(
    family = c("Clupeidae", "Engraulidae"),
    genus = c("Clupea", "Engraulis")
  )

  out <- tsbiomass:::tax_dist_mat(
    models,
    c(family = "family", genus = "genus")
  )

  expect_null(out)
})

test_that("taxonomic distance does not query literal NA binomials", {
  models <- tibble::tibble(
    species_name = c("NA NA", "Engraulis mordax"),
    genus = c(NA_character_, "Engraulis"),
    species = c(NA_character_, "mordax")
  )
  phylo <- matrix(c(0, NA, NA, 0), nrow = 2)
  attr(phylo, "taxonomic_distance_method") <- "open_tree_unavailable"

  testthat::local_mocked_bindings(
    alchemist_open_tree_node_distance = function(species_labels) {
      testthat::expect_equal(species_labels, c(NA_character_, "Engraulis mordax"))
      phylo
    },
    .package = "tsbiomass"
  )
  out <- tsbiomass:::tax_dist_mat(
    models,
    c(genus = "genus", species = "species")
  )

  expect_true(is.na(out[1, 2]))
})
