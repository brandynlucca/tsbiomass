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
