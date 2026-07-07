#!/usr/bin/env Rscript

# End-to-end Sentinel validation of the package-native Alchemist workflow.
# Sentinel owns outer splitting, trait ablation, fold-local caches, and result
# extraction. The user supplies only the workflow they want validated.

# Cross-platform shells may inject the Unix-only C.UTF-8 locale into Windows.
# Remove it before any PSOCK workers are launched; Windows R then uses its
# native UTF-8 locale without emitting one warning block per process.
if (.Platform$OS.type == "windows") {
  locale_names <- c("LANG", "LC_ALL", "LC_CTYPE", "LC_COLLATE", "LC_MONETARY", "LC_TIME")
  locale_values <- Sys.getenv(locale_names, unset = NA_character_)
  invalid_locale <- !is.na(locale_values) &
    toupper(gsub("[^A-Z0-9]", "", locale_values)) == "CUTF8"
  Sys.unsetenv(locale_names[invalid_locale])
}

output_root <- file.path("artifacts", "sentinel_alchemist_nested_grouped_pruned")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(output_root, "sentinel_run.log")
log_connection <- file(log_path, open = "at", encoding = "UTF-8")
sink(log_connection, append = TRUE, split = TRUE)
sink(log_connection, append = TRUE, type = "message")
cat(
  sprintf(
    "\n[%s] Sentinel driver started (pid=%d)\n",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    Sys.getpid()
  )
)

workspace_library <- Sys.getenv(
  "TSBIOMASS_LIBRARY",
  file.path(getwd(), ".r-lib")
)
if (dir.exists(workspace_library)) {
  .libPaths(unique(c(
    normalizePath(workspace_library, winslash = "/", mustWork = TRUE),
    .libPaths()
  )))
}
suppressPackageStartupMessages(library(tsbiomass))
loaded_package_path <- normalizePath(
  getNamespaceInfo(asNamespace("tsbiomass"), "path"),
  winslash = "/",
  mustWork = TRUE
)
if (dir.exists(workspace_library)) {
  expected_package_path <- normalizePath(
    file.path(workspace_library, "tsbiomass"),
    winslash = "/",
    mustWork = TRUE
  )
  if (!identical(loaded_package_path, expected_package_path)) {
    stop(
      sprintf(
        "Sentinel loaded tsbiomass from '%s', expected installed package '%s'.",
        loaded_package_path,
        expected_package_path
      ),
      call. = FALSE
    )
  }
}
cat(sprintf("[%s] Loaded installed tsbiomass: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), loaded_package_path))
`%||%` <- function(x, y) if (is.null(x)) y else x

pruned_configurer_path <- Sys.getenv(
  "TSBIOMASS_SENTINEL_CONFIGURER",
  file.path("artifacts", "swfscfish_pruned_superlearners", "pruned_configurer.rds")
)
if (!file.exists(pruned_configurer_path)) {
  stop(
    sprintf("Pruned Sentinel Configurer does not exist: %s", pruned_configurer_path),
    call. = FALSE
  )
}
# workflow_config_s7 <- readRDS(pruned_configurer_path)
workflow_config_s7 <- build_configurer(file.path(getwd(), "inst", "templates", "swfscfish_config.yaml"))

if (!S7::S7_inherits(workflow_config_s7, Configurer)) {
  stop("The pruned Sentinel configuration is not a Configurer object.", call. = FALSE)
}
cat(sprintf(
  "[%s] Loaded pruned Configurer: %s\n",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  normalizePath(pruned_configurer_path, winslash = "/", mustWork = TRUE)
))
cat(sprintf(
  "[%s] Pruned learners | distance: [%s] | selection: [%s] | uncertainty: [%s]\n",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  paste(workflow_config_s7@data$alchemist$learner$methods, collapse = ", "),
  paste(workflow_config_s7@data$selection$super_methods, collapse = ", "),
  paste(workflow_config_s7@data$uncertainty$super_methods, collapse = ", ")
))
candidates <- build_candidates(workflow_config_s7)
configured_cache_dir <- workflow_config_s7@data$paths$cache_dir %||% "cache"
sentinel_cache_root <- file.path(
  configured_cache_dir,
  basename(output_root)
)


# Get length distributions
library(atm)

# CPS species
cps.spp <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
             "Scomber japonicus","Trachurus symmetricus",
             "Etrumeus acuminatus")

dat.url <- "https://oceanview.pfeg.noaa.gov/erddap/tabledap/FRDCPSTrawlLHSpecimen.csvp?cruise%2Cship%2Chaul%2Ccollection%2Clatitude%2Clongitude%2Ctime%2Citis_tsn%2Cscientific_name%2Cspecimen_number%2Csex%2Cis_random_sample%2Cweight%2Cstandard_length%2Cfork_length%2Ctotal_length%2Cmantle_length&time%3E=2000-09-03T00%3A00%3A00Z&time%3C=2025-09-10T08%3A46%3A34Z"

dat <- readr::read_csv(dat.url, lazy = FALSE) |>
  dplyr::filter(scientific_name %in% cps.spp) |>
  dplyr::rename(
    fork_len = `fork_length (mm)`,
    std_len  = `standard_length (mm)`
  ) |>
  # Convert native length to total length, which us used to estimate weight
  dplyr::mutate(
    totalLength_mm = dplyr::case_when(
      scientific_name == "Clupea pallasii" ~
        convert_length("Clupea pallasii", fork_len, "FL", "TL"),
      scientific_name == "Engraulis mordax" ~
        convert_length("Engraulis mordax", std_len, "SL", "TL"),
      scientific_name == "Etrumeus acuminatus" ~
        convert_length("Etrumeus acuminatus", fork_len, "SL", "TL"),
      scientific_name == "Sardinops sagax" ~
        convert_length("Sardinops sagax", std_len, "SL", "TL"),
      scientific_name == "Scomber japonicus" ~
        convert_length("Scomber japonicus", fork_len, "FL", "TL"),
      scientific_name == "Trachurus symmetricus" ~
        convert_length("Trachurus symmetricus", fork_len, "FL", "TL")),
    length = totalLength_mm / 10)

# Set reference anchors
candidates <- set_reference_anchors(candidates)


# fetch_reference_anchors(candidates) |> dplyr::select("model_id", "species_name")
candidates <- set_reference_length_pdf(candidates,
                                       length_pdf = list("11" = dat |>
                                                           dplyr::filter(scientific_name == "Sardinops sagax") |>
                                                           dplyr::pull(length),
                                                         "12" = dat |>
                                                           dplyr::filter(scientific_name == "Clupea pallasii") |>
                                                           dplyr::pull(length),
                                                         "13" = dat |>
                                                           dplyr::filter(scientific_name == "Etrumeus acuminatus") |>
                                                           dplyr::pull(length),
                                                         "14" = dat |>
                                                           dplyr::filter(scientific_name == "Engraulis mordax") |>
                                                           dplyr::pull(length),
                                                         "15" = dat |>
                                                           dplyr::filter(scientific_name == "Scomber japonicus") |>
                                                           dplyr::pull(length),
                                                         "16" = dat |>
                                                           dplyr::filter(scientific_name == "Trachurus symmetricus") |>
                                                           dplyr::pull(length)))

workflow_fun_core <- function(candidates, workflow_config_s7) {
  alchemist <- as_alchemist(candidates, config = workflow_config_s7)
  alchemist <- forge_distances(alchemist)
  alchemist <- screen_admissibility(alchemist)

  sel <- as_policyselector(alchemist, config = workflow_config_s7)
  # Leave this unset so Sentinel's fold-local `include_ts_error` setting is
  # inherited from the workflow config.
  sel <- benchmark(sel)
  sel <- calibrate_uncertainty(sel)

  learn <- as_policylearner(sel, config = workflow_config_s7)
  learn <- crossfit(learn)
  learn <- fit(learn)
  learn <- calibrate_uncertainty(learn)

  sel <- select_policies(sel)
  pred_bundle <- predict(sel, learner = learn)

  ref <- as_referee(
    sel,
    learner = learn,
    predictions = pred_bundle,
    config = workflow_config_s7
  )
  sc <- predict(ref)
  referee_rebuild(ref, scorecard = sc)
}

# Keep profiling outside the Sentinel implementation and outside the workflow
# interface. Each fold receives a unique cache directory from Sentinel, so the
# wrapper can write collision-free profiler outputs while preserving the plain
# two-argument user workflow above.
workflow_fun <- local({
  core_workflow <- workflow_fun_core
  function(candidates, workflow_config_s7) {
    config_now <- workflow_config_s7@data
    cache_dir_now <- config_now$paths$cache_dir %||% tempdir()
    profile_dir <- file.path(cache_dir_now, "workflow_profile")
    dir.create(profile_dir, recursive = TRUE, showWarnings = FALSE)
    profile_file <- file.path(profile_dir, "workflow.Rprof")
    elapsed_file <- file.path(profile_dir, "workflow_elapsed.csv")

    started_at <- Sys.time()
    timing_start <- proc.time()
    utils::Rprof(profile_file, interval = 0.05)
    profile_active <- TRUE
    on.exit(if (profile_active) utils::Rprof(NULL), add = TRUE)

    result <- core_workflow(candidates, workflow_config_s7)

    utils::Rprof(NULL)
    profile_active <- FALSE
    elapsed_seconds <- unname((proc.time() - timing_start)[["elapsed"]])
    utils::write.csv(
      data.frame(
        started_at = format(started_at, "%Y-%m-%d %H:%M:%S %Z"),
        completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
        elapsed_seconds = elapsed_seconds
      ),
      elapsed_file,
      row.names = FALSE
    )

    profile_summary <- utils::summaryRprof(profile_file)
    write_profile_table <- function(value, filename) {
      value <- as.data.frame(value)
      if (nrow(value) == 0L) {
        return(invisible(NULL))
      }
      value <- cbind(function_name = rownames(value), value, row.names = NULL)
      utils::write.csv(
        value,
        file.path(profile_dir, filename),
        row.names = FALSE
      )
    }
    write_profile_table(profile_summary$by.total, "rprof_by_total.csv")
    write_profile_table(profile_summary$by.self, "rprof_by_self.csv")

    result
  }
})


# The headline baseline and trait-ablation estimands use different outer-fold
# resolutions. The ablation baseline remains inside the five-fold scenario grid
# so every trait delta is paired on exactly the same held-out species folds.
sentinel_cfg <- workflow_config_s7@data$sentinel %||% list()
baseline_species_folds <- as.integer(Sys.getenv(
  "TSBIOMASS_SENTINEL_BASELINE_FOLDS",
  as.character(sentinel_cfg$baseline_species_folds %||% 10L)
))
ablation_species_folds <- as.integer(Sys.getenv(
  "TSBIOMASS_SENTINEL_ABLATION_FOLDS",
  as.character(sentinel_cfg$ablation_species_folds %||% 5L)
))
outer_workers <- as.integer(Sys.getenv(
  "TSBIOMASS_SENTINEL_WORKERS",
  as.character(sentinel_cfg$workers %||% 6L)
))
batch_size <- as.integer(sentinel_cfg$batch_size %||% outer_workers)
# Set TSBIOMASS_SENTINEL_REFRESH=true to force a clean re-run. When TRUE the
# batched runner resets every fold to pending and drops the prior result index
# before executing, instead of resuming an already-completed manifest on disk.
force_refresh <- tolower(Sys.getenv("TSBIOMASS_SENTINEL_REFRESH", "false")) %in%
  c("1", "true", "yes")
baseline_sentinel <- build_sentinel(
  data = candidates,
  workflow_fn = workflow_fun,
  config = workflow_config_s7,
  split_mode = "species_holdout",
  scenario_grid = create_scenarios(),
  output_dir = file.path(output_root, "baseline"),
  cache_dir = sentinel_cache_root,
  options = list(
    seed = 20260702L,
    workers = outer_workers,
    species_folds = baseline_species_folds,
    throttle_inner_workers = TRUE,
    save_case_artifacts = FALSE
  )
)

# Trait ablation: SIMILARITY features only. Gate variables are deliberately not
# included here (that is a different estimand; see gate_sentinel below).
ablation_sentinel <- build_sentinel(
  data = candidates,
  workflow_fn = workflow_fun,
  config = workflow_config_s7,
  split_mode = "species_holdout",
  trait_ablations = TRUE,
  output_dir = file.path(output_root, "ablation"),
  cache_dir = sentinel_cache_root,
  options = list(
    seed = 20260702L,
    workers = outer_workers,
    species_folds = ablation_species_folds,
    throttle_inner_workers = TRUE,
    save_case_artifacts = FALSE
  )
)

# Gate ablation: its own separate scenario/run. Relaxes each admissibility gate
# (e.g. swimbladder_type) to quantify what the gate buys. Kept apart from the
# trait ablation so the trait-importance results stay traits-only.
gate_sentinel <- build_sentinel(
  data = candidates,
  workflow_fn = workflow_fun,
  config = workflow_config_s7,
  split_mode = "species_holdout",
  gate_ablations = TRUE,
  output_dir = file.path(output_root, "gate_ablation"),
  cache_dir = sentinel_cache_root,
  options = list(
    seed = 20260702L,
    workers = outer_workers,
    species_folds = ablation_species_folds,
    throttle_inner_workers = TRUE,
    save_case_artifacts = FALSE
  )
)

run_sentinel_batched <- function(object, batch_size, refresh = FALSE) {
  object <- tsbiomass:::resume_sentinel(object)
  if (isTRUE(refresh)) {
    # One-shot reset: build the manifest if needed, mark every fold pending, and
    # drop the cached result index. `max_folds = 0L` applies the reset without
    # executing any fold, so the batches below recompute from scratch.
    object <- run_sentinel(object, refresh = TRUE, max_folds = 0L)
  }
  repeat {
    completed_before <- if (nrow(object@manifest) == 0L) {
      0L
    } else {
      sum(as.character(object@manifest$status) == "completed")
    }
    object <- run_sentinel(object, max_folds = batch_size)
    status_now <- as.character(object@manifest$status)
    if (all(status_now == "completed")) {
      return(object)
     }
    failed_now <- which(status_now == "failed")
    if (length(failed_now) > 0L) {
      stop(
        sprintf(
          "Sentinel stopped after %d failed fold(s); inspect %s before resuming.",
          length(failed_now),
          file.path(object@output_dir, object@options$manifest_file)
        ),
        call. = FALSE
      )
    }
    completed_after <- sum(status_now == "completed")
    if (completed_after <= completed_before) {
      stop("Sentinel batch completed without advancing the manifest.", call. = FALSE)
    }
  }
}

baseline_sentinel <- run_sentinel_batched(baseline_sentinel, batch_size, refresh = force_refresh)
ablation_sentinel <- run_sentinel_batched(ablation_sentinel, batch_size, refresh = force_refresh)
gate_sentinel <- run_sentinel_batched(gate_sentinel, batch_size, refresh = force_refresh)

collect_profile_csv <- function(filename) {
  paths <- list.files(
    output_root,
    pattern = paste0("^", filename, "$"),
    recursive = TRUE,
    full.names = TRUE
  )
  rows <- lapply(paths, function(path) {
    value <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    value$profile_path <- path
    value
  })
  if (length(rows) == 0L) data.frame() else do.call(rbind, rows)
}

profile_total <- collect_profile_csv("rprof_by_total.csv")
profile_self <- collect_profile_csv("rprof_by_self.csv")
profile_elapsed <- collect_profile_csv("workflow_elapsed.csv")
utils::write.csv(profile_total, file.path(output_root, "sentinel_rprof_by_total.csv"), row.names = FALSE)
utils::write.csv(profile_self, file.path(output_root, "sentinel_rprof_by_self.csv"), row.names = FALSE)
utils::write.csv(profile_elapsed, file.path(output_root, "sentinel_workflow_elapsed.csv"), row.names = FALSE)

if (nrow(profile_total) > 0L) {
  aggregate_profile <- stats::aggregate(
    profile_total[c("total.time", "self.time")],
    by = list(function_name = profile_total$function_name),
    FUN = sum,
    na.rm = TRUE
  )
  aggregate_profile <- aggregate_profile[order(aggregate_profile$total.time, decreasing = TRUE), ]
  utils::write.csv(
    aggregate_profile,
    file.path(output_root, "sentinel_rprof_aggregate.csv"),
    row.names = FALSE
  )
}

validation_scorecard <- summary(baseline_sentinel, type = "validation")
coverage_scorecard <- summary(baseline_sentinel, type = "coverage")
ablation_scorecard <- summary(
  ablation_sentinel,
  type = "ablation",
  metric = "error_abs_log",
  bootstrap = TRUE,
  cluster_names = "anchor_species",
  strata_names = "outer_fold_id",
  bootstrap_method = "studentized",
  bootstrap_adjustment = "max_t",
  n_realizations = 1000L,
  seed = 20260702L
)
ablation_decomposition_scorecard <- summary(
  ablation_sentinel,
  type = "ablation_decomposition",
  bootstrap = TRUE,
  cluster_names = "anchor_species",
  strata_names = "outer_fold_id",
  bootstrap_method = "studentized",
  bootstrap_adjustment = "max_t",
  n_realizations = 1000L,
  seed = 20260702L
)
# Gate ablation: same paired-ablation machinery, separate sentinel/results.
gate_scorecard <- summary(
  gate_sentinel,
  type = "ablation",
  metric = "error_abs_log",
  bootstrap = TRUE,
  cluster_names = "anchor_species",
  strata_names = "outer_fold_id",
  bootstrap_method = "studentized",
  bootstrap_adjustment = "max_t",
  n_realizations = 1000L,
  seed = 20260702L
)
ggplot2::ggsave(
  file.path(output_root, "species_holdout_ranked_compact.png"),
  plot(
    validation_scorecard,
    type = "validation",
    view = "ranked",
    metric = "error_abs_log",
    metric_scale = "pseudo_log"
  ),
  width = 8.5,
  height = 5.5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(output_root, "species_holdout_coverage_conditional.png"),
  plot(coverage_scorecard, type = "coverage", estimand = "conditional"),
  width = 8,
  height = 3.5,
  dpi = 300
)

ggplot2::ggsave(
  file.path(output_root, "species_holdout_validation.png"),
  plot(
    validation_scorecard,
    type = "validation",
    view = "distribution",
    metric = "error_abs_log",
    metric_scale = "pseudo_log"
  ),
  width = 8,
  height = 5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(output_root, "trait_ablation.png"),
  plot(ablation_scorecard, type = "ablation"),
  width = 8,
  height = 5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(output_root, "trait_ablation_decomposition.png"),
  plot(ablation_decomposition_scorecard, type = "ablation_decomposition"),
  width = 12,
  height = 5.5,
  dpi = 300
)
ggplot2::ggsave(
  file.path(output_root, "gate_ablation.png"),
  plot(gate_scorecard, type = "ablation"),
  width = 8,
  height = 5,
  dpi = 300
)

saveRDS(
  list(
    baseline_sentinel = baseline_sentinel,
    ablation_sentinel = ablation_sentinel,
    gate_sentinel = gate_sentinel,
    validation_scorecard = validation_scorecard,
    coverage_scorecard = coverage_scorecard,
    ablation_scorecard = ablation_scorecard,
    ablation_decomposition_scorecard = ablation_decomposition_scorecard,
    gate_scorecard = gate_scorecard
  ),
  file.path(output_root, "nested_validation_grouped.rds")
)

cat(sprintf("[%s] Sentinel driver completed\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
sink(type = "message")
sink(type = "output")
close(log_connection)
