test_that("Configurer validates list and YAML inputs", {
  cfg_list <- minimal_config_data()
  tmp_dir <- tempfile("config-")
  dir.create(tmp_dir)

  cfg <- build_configurer(
    cfg_list,
    base_dir = tmp_dir
  )

  expect_true(S7::S7_inherits(cfg, Configurer))
  expect_true(is.list(cfg@data))
  expect_true(grepl(normalizePath(tmp_dir, winslash = "/", mustWork = FALSE), cfg@data$paths$out_root, fixed = TRUE))
  expect_equal(unname(cfg@data$alpha), cfg@data$policy$alpha)
  expect_equal(cfg@data$similarity$coherence$length$mode, "overlap")

  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(cfg_list, yaml_path)

  yaml_from_ingest <- read_configuration(
    yaml_path,
    base_dir = tmp_dir
  )
  cfg_from_yaml <- configurer_from_yaml(yaml_path, base_dir = tmp_dir)

  expect_true(S7::S7_inherits(cfg_from_yaml, Configurer))
  expect_true(is.list(yaml_from_ingest))
  expect_equal(yaml_from_ingest$similarity$coherence$frequency$mode, "overlap")
  expect_equal(yaml_from_ingest$policy$length_overlap_weight, 2)
  expect_equal(yaml_from_ingest$policies$slope_class, c("all", "fixed20_only"))
  expect_equal(unname(cfg_from_yaml@data$policy$study_traits[["fao_area"]]), 1)
  expect_equal(cfg_from_yaml@data$policies$active, cfg@data$policies$active)
  expect_equal(names(cfg_from_yaml@data$policy$species_traits), c("genus", "family"))
  expect_equal(cfg_from_yaml@data$selection$method, "glm")
  expect_equal(cfg_from_yaml@data$uncertainty$method, "glm")
})

test_that("config reduces cleanly to candidates config", {
  cfg <- build_configurer(
    minimal_config_data(),
    base_dir = tempdir()
  )

  candidate_cfg <- tsbiomass:::candidates_config_from_config(cfg)

  expect_equal(candidate_cfg$study$path, cfg@data$paths$input)
  expect_true(is.list(candidate_cfg$data))
  expect_true(is.list(candidate_cfg$prepare))
})

test_that("config YAML is validated at ingestion time", {
  bad_config <- minimal_config_data()
  bad_config$similarity$alpha <- 2
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "alpha"
  )
})

test_that("selection super-methods require super_learner selection method", {
  bad_config <- minimal_config_data()
  bad_config$selection$super_methods <- c("glm", "rpart")
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "super_methods"
  )

  good_config <- minimal_config_data()
  good_config$selection$method <- "super_learner"
  good_config$selection$super_methods <- c("glm", "rpart")
  yaml::write_yaml(good_config, yaml_path)

  expect_no_error(
    read_configuration(yaml_path, base_dir = tempdir())
  )
})

test_that("super learner requires squared-error selection loss", {
  bad_config <- minimal_config_data()
  bad_config$selection$method <- "super_learner"
  bad_config$selection$super_methods <- c("glm", "rpart")
  bad_config$selection$loss <- "absolute_error"
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "loss"
  )
})

test_that("selection and uncertainty learners are configured independently", {
  config_now <- minimal_config_data()
  config_now$selection$method <- "rf"
  config_now$uncertainty$method <- "rpart"
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(config_now, yaml_path)

  cfg <- read_configuration(yaml_path, base_dir = tempdir())

  expect_equal(cfg$selection$method, "rf")
  expect_equal(cfg$uncertainty$method, "rpart")
})

test_that("stage-specific method settings are self-contained", {
  config_now <- minimal_config_data()
  config_now$selection$method_settings <- list(
    rf = list(num_trees = 50L, min_node_size = 2L)
  )
  config_now$uncertainty$method_settings <- list(
    rf = list(num_trees = 15L, min_node_size = 3L)
  )
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(config_now, yaml_path)

  cfg <- read_configuration(yaml_path, base_dir = tempdir())

  expect_equal(cfg$selection$method_settings$rf$num_trees, 50L)
  expect_equal(cfg$selection$method_settings$rf$min_node_size, 2L)
  expect_equal(cfg$uncertainty$method_settings$rf$num_trees, 15L)
  expect_equal(cfg$uncertainty$method_settings$rf$min_node_size, 3L)
})

test_that("uncertainty super-methods require super_learner uncertainty method", {
  bad_config <- minimal_config_data()
  bad_config$uncertainty$super_methods <- c("glm", "rpart")
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "super_methods"
  )

  good_config <- minimal_config_data()
  good_config$selection$method <- "glm"
  good_config$uncertainty$method <- "super_learner"
  good_config$uncertainty$super_methods <- c("glm", "rpart")
  yaml::write_yaml(good_config, yaml_path)

  expect_no_error(
    read_configuration(yaml_path, base_dir = tempdir())
  )
})

test_that("trait names without explicit weights default to one", {
  config_now <- minimal_config_data()
  config_now$similarity$species_traits <- c("genus", "family")
  config_now$similarity$study_traits <- c("frequency", "fao_area")
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(config_now, yaml_path)

  cfg <- read_configuration(yaml_path, base_dir = tempdir())

  expect_equal(cfg$similarity$species_traits, c(genus = 1, family = 1))
  expect_equal(cfg$similarity$study_traits, c(frequency = 1, fao_area = 1))
  expect_equal(cfg$policy$species_traits, c(genus = 1, family = 1))
  expect_equal(cfg$policy$study_traits, c(frequency = 1, fao_area = 1))
})

test_that("trait-weight validation permits one empty scope when the other has traits", {
  species_only <- minimal_config_data()
  species_only$similarity$species_traits <- list(genus = 1)
  species_only$similarity$study_traits <- list()

  expect_no_error(
    build_configurer(species_only, base_dir = tempdir())
  )

  study_only <- minimal_config_data()
  study_only$similarity$species_traits <- list()
  study_only$similarity$study_traits <- list(fao_area = 1)

  expect_no_error(
    build_configurer(study_only, base_dir = tempdir())
  )

  empty_similarity <- minimal_config_data()
  empty_similarity$similarity$species_traits <- list()
  empty_similarity$similarity$study_traits <- list()

  expect_error(
    build_configurer(empty_similarity, base_dir = tempdir()),
    "Similarity trait weights must include at least one species or study trait"
  )
})

test_that("policy group construction only allows active configured study traits", {
  registry <- tsbiomass:::read_policy_registry()

  expect_error(
    tsbiomass:::build_policy_group_definition(
      "genus_diel",
      registry = registry,
      active_study_traits = c("frequency", "fao_area", "equation_form")
    ),
    "unsupported filter trait"
  )

  expect_no_error(
    tsbiomass:::build_policy_group_definition(
      "genus_equation_form",
      registry = registry,
      active_study_traits = c("frequency", "fao_area", "equation_form")
    )
  )
})

test_that("policy constructor traits remain independent of similarity traits", {
  config_now <- minimal_config_data()
  config_now$similarity$species_traits <- list(genus = 1)
  config_now$policy$species_traits <- list(genus = 1)
  config_now$policies <- list(
    metric = "closest",
    group = list(
      genus = list(joint = list("ocean_basin")),
      ocean_basin = NULL
    ),
    slope_class = "all"
  )

  normalized <- tsbiomass:::normalize_active_policy_names(config_now)

  expect_setequal(
    normalized$policies$species_traits,
    c("genus", "ocean_basin")
  )
  expect_true(any(grepl("ocean_basin", normalized$policies$active, fixed = TRUE)))
  expect_false("ocean_basin" %in% names(normalized$similarity$species_traits))
})

test_that("post-selection support bin labels are validated against n_bins", {
  bad_config <- minimal_config_data()
  bad_config$selection$n_bins <- 3L
  bad_config$selection$support_bin_labels <- c("Low", "Mid")
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "support_bin_labels"
  )

  good_config <- minimal_config_data()
  good_config$selection$n_bins <- 3L
  good_config$selection$support_bin_labels <- c("Lower support", "Moderate support", "Higher support")
  yaml::write_yaml(good_config, yaml_path)

  cfg <- read_configuration(yaml_path, base_dir = tempdir())
  expect_equal(
    cfg$selection$support_bin_labels,
    c("Lower support", "Moderate support", "Higher support")
  )
})

test_that("selection method_settings are validated at ingestion", {
  bad_config <- minimal_config_data()
  bad_config$selection$method_settings$xgboost$nrounds <- 0L
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "method_settings.xgboost.nrounds"
  )

  good_config <- minimal_config_data()
  yaml::write_yaml(good_config, yaml_path)

  expect_no_error(
    read_configuration(yaml_path, base_dir = tempdir())
  )
})

test_that("Sentinel logging controls are validated at ingestion", {
  config_now <- minimal_config_data()
  config_now$sentinel <- list(
    logging = TRUE,
    cache_dir = "sentinel-cache",
    log_file = "sentinel.log"
  )
  config_now$execution$progress <- TRUE
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(config_now, yaml_path)

  cfg <- read_configuration(yaml_path, base_dir = tempdir())
  expect_true(cfg$execution$progress)
  expect_true(cfg$sentinel$logging)
  expect_equal(cfg$sentinel$cache_dir, "sentinel-cache")

  config_now$sentinel$logging <- "yes"
  yaml::write_yaml(config_now, yaml_path)
  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "Sentinel field 'logging' must be TRUE or FALSE"
  )
})

test_that("Alchemist rf controls are validated at ingestion", {
  config_now <- minimal_config_data()
  config_now$alchemist <- list(
    learner = list(
      method_settings = list(
        rf = list(
          max_depth = 15L,
          sample_fraction = 0.632,
          replace = FALSE,
          respect_unordered_factors = "ignore"
        )
      )
    )
  )
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(config_now, yaml_path)

  cfg <- read_configuration(yaml_path, base_dir = tempdir())
  expect_equal(cfg$alchemist$learner$method_settings$rf$max_depth, 15L)
  expect_equal(cfg$alchemist$learner$method_settings$rf$sample_fraction, 0.632)
  expect_false(cfg$alchemist$learner$method_settings$rf$replace)

  config_now$alchemist$learner$method_settings$rf$max_depth <- 0L
  yaml::write_yaml(config_now, yaml_path)
  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "method_settings.rf.max_depth"
  )
})

test_that("stale configuration field names are rejected", {
  bad_policy_config <- minimal_config_data()
  bad_policy_config$policies$branch <- "all"
  bad_policy_config$policies$slope_class <- NULL
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_policy_config, yaml_path)

  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "Use 'slope_class'"
  )

  bad_similarity_config <- minimal_config_data()
  bad_similarity_config$similarity$alpha_range <- list(from = 0.1, to = 0.9)
  yaml::write_yaml(bad_similarity_config, yaml_path)

  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "top-level 'tuning'"
  )
})

test_that("uncertainty lmm settings are validated at ingestion", {
  bad_config <- minimal_config_data()
  bad_config$uncertainty$method <- "lmm"
  bad_config$uncertainty$method_settings <- list(
    lmm = list(fit_method = "BAD", random_intercept = "study_group")
  )
  yaml_path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(bad_config, yaml_path)

  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "method_settings.lmm.fit_method"
  )

  good_config <- minimal_config_data()
  good_config$uncertainty$method <- "lmm"
  good_config$uncertainty$method_settings <- list(
    lmm = list(fit_method = "ML", random_intercept = "study_group")
  )
  yaml::write_yaml(good_config, yaml_path)

  cfg <- read_configuration(yaml_path, base_dir = tempdir())
  expect_equal(cfg$uncertainty$method, "lmm")
  expect_equal(
    cfg$uncertainty$method_settings$lmm$random_intercept,
    "study_group"
  )

  good_config$uncertainty$method_settings$lmm$random_intercept <- c(
    "study_group", "secondary_group"
  )
  yaml::write_yaml(good_config, yaml_path)
  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "random_intercept.*exactly one"
  )

  good_config$uncertainty$method_settings$lmm$random_intercept <- NULL
  good_config$uncertainty$method_settings$lmm$group_cols <- "study_group"
  yaml::write_yaml(good_config, yaml_path)
  expect_error(
    read_configuration(yaml_path, base_dir = tempdir()),
    "group_cols.*replaced by.*random_intercept"
  )
})

test_that("retired configuration fields are rejected at ingestion", {
  retired_cases <- list(
    list(path = c("paths", "input"), value = "input.xlsx"),
    list(path = c("execution", "strict_pdf"), value = FALSE),
    list(path = c("policy"), value = list(alpha = 0.8)),
    list(path = c("similarity", "k_species"), value = 4),
    list(path = c("similarity", "length_weight"), value = 2),
    list(path = c("admissibility", "frequency_gap"), value = 60),
    list(path = c("selection", "tolerance"), value = 0.05),
    list(path = c("tuning", "max_models_per_species"), value = 2L),
    list(path = c("tuning", "n_resamples"), value = 3L)
  )

  for (case_now in retired_cases) {
    config_now <- minimal_config_data()
    if (length(case_now$path) == 1L) {
      config_now[[case_now$path[[1]]]] <- case_now$value
    } else {
      config_now[[case_now$path[[1]]]][[case_now$path[[2]]]] <- case_now$value
    }
    expect_error(
      build_configurer(config_now, base_dir = tempdir()),
      "not supported"
    )
  }
})

test_that("command line requires an explicit config path", {
  expect_equal(
    tsbiomass:::parse_command_line(c("--config", "config.yaml")),
    list(action = "run", config_path = "config.yaml")
  )
  expect_error(tsbiomass:::parse_command_line(character()), "--config")
  expect_error(tsbiomass:::parse_command_line("input.xlsx"), "Unsupported command-line")
  expect_error(tsbiomass:::parse_command_line(c("--config", "config.yaml", "extra")), "--config")
})
