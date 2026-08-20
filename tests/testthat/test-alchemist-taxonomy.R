test_that("taxonomic distance tolerates ablated rank fields", {
  models <- tibble::tibble(
    family = c("Clupeidae", "Clupeidae", "Engraulidae"),
    genus = c("Alosa", "Clupea", "Engraulis"),
    species = c("aestivalis", "harengus", "encrasicolus")
  )

  without_genus <- tsbiomass:::tax_dist_mat(
    models,
    c(family = "family", species = "species")
  )
  without_species <- tsbiomass:::tax_dist_mat(
    models,
    c(family = "family", genus = "genus")
  )

  expect_equal(dim(without_genus), c(3L, 3L))
  expect_equal(dim(without_species), c(3L, 3L))
  expect_true(all(is.finite(without_genus)))
  expect_true(all(is.finite(without_species)))
})

test_that("taxonomic distance uses one rank scale after incomplete Open Tree resolution", {
  models <- tibble::tibble(
    family = c("Osmeridae", "Osmeridae", "Clupeidae"),
    genus = c("Allosmerus", "Osmerus", "Clupea"),
    species = c("elongatus", "mordax", "harengus")
  )
  phylo <- matrix(
    c(0, NA, 0.90, NA, 0, NA, 0.90, NA, 0),
    nrow = 3
  )
  attr(phylo, "phylo_resolved") <- c(TRUE, FALSE, TRUE)
  attr(phylo, "phylo_ott_id") <- c("1", NA_character_, "3")
  attr(phylo, "phylo_species_key") <- c("allosmerus elongatus", "osmerus mordax", "clupea harengus")

  testthat::local_mocked_bindings(
    build_phylo_dist_from_species = function(...) phylo,
    .package = "tsbiomass"
  )
  out <- tsbiomass:::tax_dist_mat(
    models,
    c(family = "family", genus = "genus", species = "species")
  )

  expect_equal(out[1, 2], 2 / 3)
  expect_equal(out[1, 3], 1)
  expect_equal(attr(out, "taxonomic_distance_method"), "rank_fallback")
})
