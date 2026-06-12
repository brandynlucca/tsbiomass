test_that("Configurer validates list and YAML inputs", {
  cfg_list <- minimal_config_data()
  tmp_dir <- tempfile("config-")
  dir.create(tmp_dir)

  cfg <- as_configurer(
    cfg_list,
    base_dir = tmp_dir
  )

  expect_true(S7::S7_inherits(cfg, Configurer))
  expect_true(is.list(cfg@data))
  expect_true(grepl(normalizePath(tmp_dir, winslash = "/", mustWork = FALSE), cfg@data$paths$out_root, fixed = TRUE))
  expect_equal(unname(cfg@data$alpha), cfg@data$policy$alpha)
  expect_equal(cfg@data$similarity$coherence$length$mode, "overlap")
  expect_equal(cfg@data$species_trait_cols, c("genus", "family"))

  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(cfg_list, yaml_path)

  yaml_from_ingest <- read_config(
    yaml_path,
    base_dir = tmp_dir
  )
  cfg_from_yaml <- configurer_from_yaml(yaml_path, base_dir = tmp_dir)

  expect_true(S7::S7_inherits(cfg_from_yaml, Configurer))
  expect_true(is.list(yaml_from_ingest))
  expect_equal(yaml_from_ingest$similarity$coherence$frequency$mode, "overlap")
  expect_equal(yaml_from_ingest$policy$length_overlap_weight, 2)
  expect_equal(yaml_from_ingest$policies$equation_branch_filters, c("all", "fixed20_only"))
  expect_equal(unname(cfg_from_yaml@data$policy$study_traits[["fao_area"]]), 1)
  expect_equal(cfg_from_yaml@data$policies$active, cfg@data$policies$active)
  expect_equal(names(cfg_from_yaml@data$policy$species_traits), c("genus", "family"))
  expect_equal(cfg_from_yaml@data$metalearner$uncertainty_method, cfg_from_yaml@data$metalearner$selection_method)
})

test_that("config reduces cleanly to candidates config", {
  cfg <- as_configurer(
    minimal_config_data(),
    base_dir = tempdir()
  )

  candidate_cfg <- tsbiomass:::candidates_config_from_config(cfg)

  expect_equal(candidate_cfg$study$path, cfg@data$paths$input)
  expect_true(is.list(candidate_cfg$data))
  expect_true(is.list(candidate_cfg$prepare))
})

test_that("Configurer rejects incomplete configs", {
  bad_cfg <- minimal_config_data()
  bad_cfg$similarity <- NULL

  expect_error(
    as_configurer(
      bad_cfg,
      base_dir = tempdir()
    )
  )
})

test_that("config YAML is validated at ingestion time", {
  bad_config <- minimal_config_data()
  bad_config$similarity$alpha <- 2
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_config(yaml_path, base_dir = tempdir()),
    "alpha"
  )
})

test_that("legacy config names are rejected at ingestion", {
  bad_config <- minimal_config_data()
  bad_config$paths$input_file <- bad_config$paths$input
  bad_config$paths$input <- NULL
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_config(yaml_path, base_dir = tempdir()),
    "legacy field 'input_file'"
  )
})

test_that("metalearner super-methods require super_learner selection method", {
  bad_config <- minimal_config_data()
  bad_config$metalearner$selection_super_methods <- c("glm", "rpart")
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_config(yaml_path, base_dir = tempdir()),
    "selection_super_methods"
  )

  good_config <- minimal_config_data()
  good_config$metalearner$selection_method <- "super_learner"
  good_config$metalearner$selection_super_methods <- c("glm", "rpart")
  yaml::write_yaml(good_config, yaml_path)

  expect_no_error(
    read_config(yaml_path, base_dir = tempdir())
  )
})

test_that("super learner requires squared-error metalearner loss", {
  bad_config <- minimal_config_data()
  bad_config$metalearner$selection_method <- "super_learner"
  bad_config$metalearner$selection_super_methods <- c("glm", "rpart")
  bad_config$metalearner$metalearner_loss <- "absolute_error"
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_config(yaml_path, base_dir = tempdir()),
    "metalearner_loss"
  )
})

test_that("metalearner legacy width names are rejected at ingestion", {
  bad_config <- minimal_config_data()
  bad_config$metalearner$width_method <- "glm"
  bad_config$metalearner$uncertainty_method <- NULL
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_config(yaml_path, base_dir = tempdir()),
    "legacy field 'width_method'"
  )

  bad_config <- minimal_config_data()
  bad_config$metalearner$width_feature_cols <- c("local_effective_support")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_config(yaml_path, base_dir = tempdir()),
    "legacy field 'width_feature_cols'"
  )
})

test_that("uncertainty learner inherits the selection learner when omitted", {
  config_now <- minimal_config_data()
  config_now$metalearner$selection_method <- "ranger"
  config_now$metalearner$uncertainty_method <- NULL
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(config_now, yaml_path)

  cfg <- read_config(yaml_path, base_dir = tempdir())

  expect_equal(cfg$metalearner$selection_method, "ranger")
  expect_equal(cfg$metalearner$uncertainty_method, "ranger")
})

test_that("trait names without explicit weights default to one", {
  config_now <- minimal_config_data()
  config_now$similarity$species_traits <- c("genus", "family")
  config_now$similarity$study_traits <- c("frequency", "fao_area")
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(config_now, yaml_path)

  cfg <- read_config(yaml_path, base_dir = tempdir())

  expect_equal(cfg$similarity$species_traits, c(genus = 1, family = 1))
  expect_equal(cfg$similarity$study_traits, c(frequency = 1, fao_area = 1))
  expect_equal(cfg$policy$species_traits, c(genus = 1, family = 1))
  expect_equal(cfg$policy$study_traits, c(frequency = 1, fao_area = 1))
})

test_that("post-selection support bin labels are validated against n_bins", {
  bad_config <- minimal_config_data()
  bad_config$post_selection_conformal$n_bins <- 3L
  bad_config$post_selection_conformal$support_bin_labels <- c("Low", "Mid")
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_config(yaml_path, base_dir = tempdir()),
    "support_bin_labels"
  )

  good_config <- minimal_config_data()
  good_config$post_selection_conformal$n_bins <- 3L
  good_config$post_selection_conformal$support_bin_labels <- c("Lower support", "Moderate support", "Higher support")
  yaml::write_yaml(good_config, yaml_path)

  cfg <- read_config(yaml_path, base_dir = tempdir())
  expect_equal(
    cfg$post_selection_conformal$support_bin_labels,
    c("Lower support", "Moderate support", "Higher support")
  )
})

test_that("metalearner method_settings are validated at ingestion", {
  bad_config <- minimal_config_data()
  bad_config$metalearner$method_settings$xgboost$nrounds <- 0L
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_config(yaml_path, base_dir = tempdir()),
    "method_settings.xgboost.nrounds"
  )

  good_config <- minimal_config_data()
  yaml::write_yaml(good_config, yaml_path)

  expect_no_error(
    read_config(yaml_path, base_dir = tempdir())
  )
})


