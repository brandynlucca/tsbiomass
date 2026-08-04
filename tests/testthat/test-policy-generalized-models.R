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
