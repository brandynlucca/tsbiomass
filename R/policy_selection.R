#' Rank policy specificity
#'
#' Returns an ordinal specificity ranking used to break ties when multiple
#' policies have nearly identical benchmark error.
#'
#' @param policy Character vector of policy names.
#'
#' @return Integer vector.
#'
#' @export
policy_specificity_rank <- function(policy) {
  # Preserve the original domain-informed specificity order so tie-breaking in
  # the global selection table remains stable and interpretable.
  dplyr::case_when(
    policy == "same_species_closest" ~ 1L,
    policy == "closest_related_species" ~ 2L,
    policy %in% c("same_genus_closest", "same_genus_weighted") ~ 3L,
    policy %in% c("same_family_closest", "same_family_weighted") ~ 4L,
    policy %in% c("same_order_closest", "same_order_weighted") ~ 5L,
    policy %in% c(
      "same_swimbladder_closest",
      "same_swimbladder_weighted",
      "same_ellipse_closest",
      "same_ellipse_weighted",
      "same_cluster_closest",
      "same_cluster_weighted",
      "top_support_subset_weighted",
      "top_support_subset_custom_weighted",
      "top_support_subset_90_weighted",
      "top_support_subset_95_weighted",
      "all_models_weighted"
    ) ~ 6L,
    policy %in% c(
      "closest_generalized_model",
      "generalized_models_weighted",
      "generalized_models_scan"
    ) ~ 7L,
    policy %in% c(
      "all_models_unweighted",
      "all_models_median",
      "top_k_models_weighted",
      "top_k_models_unweighted",
      "top_k_models_median"
    ) ~ 8L,
    TRUE ~ 9L
  )
}

#' Summarize one species-block benchmark table
#'
#' Collapses the leave-one-species-out benchmark rows to one species-level
#' summary per policy.
#'
#' @param species_performance_table Species-block benchmark table.
#'
#' @return A tibble.
#'
#' @keywords internal
species_performance <- function(species_performance_table) {
  # Restrict the summary to finite valid predictions before collapsing anchor
  # rows to one species-level error summary per policy.
  {
    species_tbl <- tibble::as_tibble(species_performance_table)
    species_tbl$policy <- resolve_policy_names(species_tbl)
    species_tbl
  } |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log)) |>
    dplyr::group_by(policy, anchor_species) |>
    dplyr::summarise(
      species_median_abs_log = stats::median(error_abs_log, na.rm = TRUE),
      species_mean_abs_log = mean(error_abs_log, na.rm = TRUE),
      n_anchor_models = dplyr::n(),
      .groups = "drop"
    )
}

#' Build the policy-selection reference table
#'
#' Converts the leave-one-species-out benchmark results to a global policy
#' comparison table using a one-standard-error rule plus bootstrap stability.
#'
#' @param species_performance_table Species-block benchmark table.
#' @param tolerance Optional tolerance value retained in the output for
#'   reference.
#' @param n_boot Number of bootstrap resamples across species.
#' @param seed Integer bootstrap seed.
#'
#' @return A tibble.
#'
#' @export
build_selection_table <- function(species_performance_table,
                                  tolerance = 0.05,
                                  n_boot = 500L,
                                  seed = 20260512L) {
  # Summarize the benchmark at the species level first so the selection rule
  # aligns with the species-block validation design.
  species_level <- species_performance(species_performance_table)
  if (nrow(species_level) == 0) {
    return(tibble::tibble())
  }

  # Build the global per-policy summary that the later one-SE and bootstrap
  # rules operate on.
  select_ref <- species_level |>
    dplyr::group_by(policy) |>
    dplyr::summarise(
      n_species = dplyr::n(),
      median_species_median_abs_log = stats::median(species_median_abs_log, na.rm = TRUE),
      mean_species_median_abs_log = mean(species_median_abs_log, na.rm = TRUE),
      sd_species_median_abs_log = stats::sd(species_median_abs_log, na.rm = TRUE),
      se_species_median_abs_log = dplyr::if_else(
        n_species > 1,
        sd_species_median_abs_log / sqrt(n_species),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(specificity_rank = policy_specificity_rank(policy))

  # Identify the best current policy and the one-SE acceptability threshold
  # before the bootstrap summaries are computed.
  best_row <- select_ref |>
    dplyr::arrange(
      mean_species_median_abs_log,
      median_species_median_abs_log,
      specificity_rank,
      policy
    ) |>
    dplyr::slice(1)
  threshold <- best_row$mean_species_median_abs_log[[1]] +
    dplyr::coalesce(best_row$se_species_median_abs_log[[1]], 0)

  set.seed(as.integer(seed))

  # Bootstrap the species-level summaries rather than the anchor rows so the
  # stability rule stays aligned with the validation design.
  boot_tbl <- purrr::map_dfr(seq_len(as.integer(n_boot)), function(boot_id) {
    species_level |>
      dplyr::group_by(policy) |>
      dplyr::summarise(
        boot_mean_species_median_abs_log = mean(
          sample(species_median_abs_log, size = dplyr::n(), replace = TRUE),
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(boot_id = boot_id)
  })

  # Summarize the bootstrap distribution for each policy relative to the
  # one-SE threshold.
  boot_sum <- boot_tbl |>
    dplyr::group_by(policy) |>
    dplyr::summarise(
      bootstrap_mean_q05 = stats::quantile(
        boot_mean_species_median_abs_log,
        probs = 0.05,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),
      bootstrap_mean_q50 = stats::quantile(
        boot_mean_species_median_abs_log,
        probs = 0.50,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),
      bootstrap_mean_q95 = stats::quantile(
        boot_mean_species_median_abs_log,
        probs = 0.95,
        na.rm = TRUE,
        names = FALSE,
        type = 8
      ),
      bootstrap_prob_within_threshold = mean(
        boot_mean_species_median_abs_log <= threshold,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  # Rank policies within each bootstrap draw to estimate how often each one
  # is the best available choice.
  boot_rank <- boot_tbl |>
    dplyr::mutate(specificity_rank = policy_specificity_rank(policy)) |>
    dplyr::group_by(boot_id) |>
    dplyr::arrange(
      boot_mean_species_median_abs_log,
      specificity_rank,
      policy,
      .by_group = TRUE
    ) |>
    dplyr::mutate(rank_boot = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(policy) |>
    dplyr::summarise(
      bootstrap_prob_best = mean(rank_boot == 1, na.rm = TRUE),
      bootstrap_median_rank = stats::median(rank_boot, na.rm = TRUE),
      .groups = "drop"
    )

  # Merge the bootstrap diagnostics back onto the policy summary and record
  # the final acceptability calls.
  select_ref |>
    dplyr::left_join(boot_sum, by = "policy") |>
    dplyr::left_join(boot_rank, by = "policy") |>
    dplyr::mutate(
      best_policy_global = best_row$policy[[1]],
      best_mean_species_median_abs_log = best_row$mean_species_median_abs_log[[1]],
      one_se_threshold = threshold,
      acceptable_one_se = mean_species_median_abs_log <= threshold,
      acceptable_bootstrap = dplyr::coalesce(bootstrap_prob_within_threshold, 0) >= 0.50,
      acceptable_global = acceptable_one_se & acceptable_bootstrap,
      equivalence_tolerance = as.numeric(tolerance)
    ) |>
    dplyr::arrange(
      dplyr::desc(acceptable_global),
      dplyr::desc(acceptable_one_se),
      mean_species_median_abs_log,
      specificity_rank,
      policy
    )
}

#' Build pairwise policy-equivalence summaries
#'
#' Compares paired species-block benchmark errors and records whether each
#' policy pair is practically and statistically indistinguishable.
#'
#' @param species_performance_table Species-block benchmark table.
#' @param select_ref Global policy-selection reference table.
#' @param tolerance Practical equivalence tolerance on mean paired error
#'   difference.
#' @param n_boot Number of paired bootstrap resamples across species.
#' @param seed Integer bootstrap seed.
#'
#' @return A list with `pairs` and `best_flags`.
#'
#' @export
build_equivalence_table <- function(species_performance_table,
                                    select_ref,
                                    tolerance = 0.05,
                                    n_boot = 500L,
                                    seed = 20260515L) {
  # Reuse the species-level benchmark summary so equivalence is assessed on the
  # same validation scale as the global selection table.
  species_level <- species_performance(species_performance_table) |>
    dplyr::select(policy, anchor_species, species_median_abs_log)

  if (nrow(species_level) == 0 || nrow(select_ref) == 0) {
    return(list(
      pairs = tibble::tibble(),
      best_flags = tibble::tibble(
        policy = character(0),
        equivalent_to_best_global = logical(0),
        paired_mean_diff_to_best = numeric(0)
      )
    ))
  }

  policies <- sort(unique(species_level$policy))
  if (length(policies) < 2) {
    return(list(
      pairs = tibble::tibble(),
      best_flags = tibble::tibble(
        policy = policies,
        equivalent_to_best_global = TRUE,
        paired_mean_diff_to_best = 0
      )
    ))
  }

  # Identify the best current policy first so equivalence-to-best can be
  # derived directly from the pairwise comparison table later.
  best_policy <- {
      select_ref_tbl <- tibble::as_tibble(select_ref)
      select_ref_tbl$policy <- resolve_policy_names(select_ref_tbl)
      select_ref_tbl
    } |>
    dplyr::arrange(mean_species_median_abs_log, specificity_rank, policy) |>
    dplyr::slice(1) |>
    dplyr::pull(policy)

  pair_tbl <- utils::combn(policies, 2, simplify = FALSE) |>
    purrr::map_dfr(function(pair) {
      lhs <- pair[[1]]
      rhs <- pair[[2]]

      wide <- species_level |>
        dplyr::filter(policy %in% c(lhs, rhs)) |>
        dplyr::select(policy, anchor_species, species_median_abs_log) |>
        tidyr::pivot_wider(names_from = policy, values_from = species_median_abs_log) |>
        dplyr::filter(is.finite(.data[[lhs]]), is.finite(.data[[rhs]]))

      if (nrow(wide) == 0) {
        return(tibble::tibble(
          policy_a = lhs,
          policy_b = rhs,
          n_species_common = 0L,
          paired_mean_diff = NA_real_,
          paired_median_diff = NA_real_,
          paired_boot_q025 = NA_real_,
          paired_boot_q975 = NA_real_,
          equivalent_pair = FALSE,
          better_policy = NA_character_
        ))
      }

      # Bootstrap the paired species-level differences so equivalence reflects
      # both practical magnitude and bootstrap uncertainty.
      diff_vec <- wide[[lhs]] - wide[[rhs]]
      set.seed(as.integer(seed) + sum(utf8ToInt(paste(lhs, rhs, sep = "|"))))
      boot_means <- replicate(as.integer(n_boot), {
        idx <- sample.int(length(diff_vec), size = length(diff_vec), replace = TRUE)
        mean(diff_vec[idx], na.rm = TRUE)
      })

      q025 <- stats::quantile(boot_means, probs = 0.025, na.rm = TRUE, names = FALSE, type = 8)
      q975 <- stats::quantile(boot_means, probs = 0.975, na.rm = TRUE, names = FALSE, type = 8)
      mean_diff <- mean(diff_vec, na.rm = TRUE)
      med_diff <- stats::median(diff_vec, na.rm = TRUE)

      eq_pair <- is.finite(mean_diff) &&
        abs(mean_diff) <= tolerance &&
        is.finite(q025) &&
        is.finite(q975) &&
        q025 <= 0 &&
        q975 >= 0

      better <- dplyr::case_when(
        eq_pair ~ NA_character_,
        is.finite(mean_diff) && mean_diff < 0 ~ lhs,
        is.finite(mean_diff) && mean_diff > 0 ~ rhs,
        TRUE ~ NA_character_
      )

      tibble::tibble(
        policy_a = lhs,
        policy_b = rhs,
        n_species_common = nrow(wide),
        paired_mean_diff = mean_diff,
        paired_median_diff = med_diff,
        paired_boot_q025 = q025,
        paired_boot_q975 = q975,
        equivalent_pair = eq_pair,
        better_policy = better
      )
    })

  # Convert the pairwise table to one row per policy showing whether it is
  # equivalent to the global best policy.
  best_flags <- tibble::tibble(policy = policies) |>
    dplyr::mutate(
      equivalent_to_best_global = purrr::map_lgl(policy, function(policy_nm) {
        if (identical(policy_nm, best_policy)) {
          return(TRUE)
        }

        row <- pair_tbl |>
          dplyr::filter(
            (policy_a == best_policy & policy_b == policy_nm) |
              (policy_b == best_policy & policy_a == policy_nm)
          ) |>
          dplyr::slice(1)

        nrow(row) == 1 && isTRUE(row$equivalent_pair[[1]])
      }),
      paired_mean_diff_to_best = purrr::map_dbl(policy, function(policy_nm) {
        if (identical(policy_nm, best_policy)) {
          return(0)
        }

        row <- pair_tbl |>
          dplyr::filter(
            (policy_a == best_policy & policy_b == policy_nm) |
              (policy_b == best_policy & policy_a == policy_nm)
          ) |>
          dplyr::slice(1)

        if (nrow(row) == 0 || !is.finite(row$paired_mean_diff[[1]])) {
          return(NA_real_)
        }

        if (row$policy_a[[1]] == best_policy) {
          return(row$paired_mean_diff[[1]])
        }

        -row$paired_mean_diff[[1]]
      }),
      best_policy_global = best_policy
    )

  list(pairs = pair_tbl, best_flags = best_flags)
}

#' Build policy-equivalence classes
#'
#' Converts pairwise equivalence calls to connected equivalence classes.
#'
#' @param select_ref Global policy-selection reference table.
#' @param pair_tbl Pairwise policy-equivalence table.
#'
#' @return A tibble.
#'
#' @export
build_equivalence_sets <- function(select_ref,
                                   pair_tbl) {
  # Start from the policy list in the selection table so the class summary
  # always covers every benchmarked policy.
  select_ref <- tibble::as_tibble(select_ref)
  select_ref$policy <- resolve_policy_names(select_ref)
  policies <- sort(unique(select_ref$policy))
  if (length(policies) == 0) {
    return(tibble::tibble())
  }

  adjacency <- stats::setNames(vector("list", length(policies)), policies)
  for (policy_nm in policies) {
    adjacency[[policy_nm]] <- character(0)
  }

  # Build an undirected adjacency list from the pairwise equivalence calls.
  if (nrow(pair_tbl) > 0) {
    eq_pairs <- pair_tbl |>
      dplyr::filter(equivalent_pair) |>
      dplyr::select(policy_a, policy_b)

    if (nrow(eq_pairs) > 0) {
      for (i in seq_len(nrow(eq_pairs))) {
        lhs <- eq_pairs$policy_a[[i]]
        rhs <- eq_pairs$policy_b[[i]]
        adjacency[[lhs]] <- unique(c(adjacency[[lhs]], rhs))
        adjacency[[rhs]] <- unique(c(adjacency[[rhs]], lhs))
      }
    }
  }

  # Walk the connected components so each equivalence class is represented once
  # and each policy gets one membership row.
  visited <- stats::setNames(rep(FALSE, length(policies)), policies)
  class_rows <- list()
  class_id <- 0L

  for (root in policies) {
    if (visited[[root]]) {
      next
    }

    class_id <- class_id + 1L
    queue <- root
    members <- character(0)

    while (length(queue) > 0) {
      node <- queue[[1]]
      queue <- queue[-1]
      if (visited[[node]]) {
        next
      }

      visited[[node]] <- TRUE
      members <- c(members, node)
      nbrs <- adjacency[[node]] %||% character(0)
      queue <- unique(c(queue, nbrs[!visited[nbrs]]))
    }

    members <- sort(unique(members))
    class_rows[[length(class_rows) + 1]] <- tibble::tibble(
      policy = members,
      equivalence_class_id = paste0("class_", class_id),
      equivalence_class_size = length(members),
      equivalence_class_members = paste(members, collapse = "; ")
    )
  }

  dplyr::bind_rows(class_rows) |>
    dplyr::arrange(equivalence_class_id, policy)
}

#' Run the policy-selection summary
#'
#' Builds the global policy-selection table, pairwise equivalence summary,
#' and equivalence-class table from the species-block benchmark results.
#'
#' @param species_performance_table Species-block benchmark table.
#' @param config Optional JSON path or list with `tolerance`, `n_boot`, and
#'   `seed`.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#'
#' @return A list containing the selection table, pairwise equivalence table,
#'   equivalence-class table, and merged final selection table.
#'
#' @export
run_policy_selection <- function(species_performance_table,
                                 config = NULL,
                                 cache_path = NULL,
                                 refresh = FALSE) {
  # Validate cache control and the benchmark input before any bootstrap work
  # begins.
  if (!is.data.frame(species_performance_table)) {
    stop("'species_performance_table' must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Reuse the cached selection object when available unless a refresh was
  # explicitly requested.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Inline the policy-selection defaults here so the function resolves its own
  # fallback settings without an extra config helper.
  config_values <- merge_cfg(
    list(
      tolerance = 0.05,
      n_boot = 500L,
      seed = 20260512L
    ),
    read_similarity_config(config)
  )

  # Build the base selection summary first, then layer on the pairwise and
  # equivalence-class summaries before assembling the final merged table.
  select_ref <- build_selection_table(
    species_performance_table = species_performance_table,
    tolerance = config_values$tolerance,
    n_boot = config_values$n_boot,
    seed = config_values$seed
  )
  equiv_ref <- build_equivalence_table(
    species_performance_table = species_performance_table,
    select_ref = select_ref,
    tolerance = config_values$tolerance,
    n_boot = config_values$n_boot,
    seed = config_values$seed + 3L
  )
  equiv_sets <- build_equivalence_sets(
    select_ref = select_ref,
    pair_tbl = equiv_ref$pairs
  )

  final_ref <- select_ref |>
    dplyr::left_join(equiv_ref$best_flags, by = "policy") |>
    dplyr::left_join(equiv_sets, by = "policy") |>
    dplyr::mutate(
      equivalent_to_best_global = dplyr::coalesce(equivalent_to_best_global, FALSE),
      paired_mean_diff_to_best = dplyr::coalesce(paired_mean_diff_to_best, NA_real_)
    )

  result <- list(
    select_ref = select_ref,
    equiv_ref = equiv_ref,
    equiv_sets = equiv_sets,
    final_ref = final_ref
  )

  # Cache the in-memory selection summaries only when a cache path was
  # supplied.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  result
}
