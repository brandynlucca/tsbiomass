#' Run an NMDS ordination
#'
#' Runs a two-dimensional NMDS on a distance matrix and optionally fits trait
#' vectors and factor centroids with `vegan::envfit()`.
#'
#' @param dist_mat A distance matrix or `dist` object.
#' @param trait_table Optional trait table aligned to `dist_mat`.
#' @param nmds_args Optional named list of arguments passed to
#'   `vegan::metaMDS()`.
#' @param include_loadings Logical scalar. If `TRUE`, return envfit vector
#'   loadings for numeric traits.
#' @param include_centroids Logical scalar. If `TRUE`, return envfit centroids
#'   for factor traits.
#' @param envfit_args Optional named list of arguments passed to
#'   `vegan::envfit()`.
#'
#' @return A list with `ordination`, `points`, `loadings`, and `centroids`.
#'
#' @export
run_ordination <- function(dist_mat,
                     trait_table = NULL,
                     nmds_args = list(),
                     include_loadings = FALSE,
                     include_centroids = FALSE,
                     envfit_args = list()) {
  # Accept either a matrix or a `dist` object and normalize the ordination
  # arguments before calling `metaMDS()`.
  if (inherits(dist_mat, "dist")) {
    dist_obj <- dist_mat
  } else if (is.matrix(dist_mat)) {
    dist_obj <- stats::as.dist(dist_mat)
  } else {
    stop("'dist_mat' must be a matrix or a 'dist' object.", call. = FALSE)
  }

  if (!is.list(nmds_args)) {
    stop("'nmds_args' must be a list.", call. = FALSE)
  }
  if (!is.list(envfit_args)) {
    stop("'envfit_args' must be a list.", call. = FALSE)
  }
  if (!is.logical(include_loadings) || length(include_loadings) != 1 || is.na(include_loadings)) {
    stop("'include_loadings' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(include_centroids) || length(include_centroids) != 1 || is.na(include_centroids)) {
    stop("'include_centroids' must be TRUE or FALSE.", call. = FALSE)
  }

  # Start from the standard package defaults, then let the caller override only
  # the metaMDS arguments they actually care about.
  nmds_call <- c(
    list(comm = dist_obj, k = 2, trymax = 50, autotransform = FALSE, trace = FALSE),
    nmds_args
  )
  ord <- do.call(vegan::metaMDS, nmds_call)

  # Return the NMDS point coordinates with row identifiers preserved from the
  # distance object.
  pts <- as.data.frame(ord$points) |>
    tibble::rownames_to_column("model_id")
  if (ncol(pts) >= 3) {
    names(pts)[2:3] <- c("MDS1", "MDS2")
  }

  # Skip envfit entirely unless one or both optional outputs were requested and
  # a matching trait table was supplied.
  empty_loadings <- tibble::tibble(
    trait = character(),
    MDS1 = numeric(),
    MDS2 = numeric(),
    p_value = numeric(),
    r2 = numeric()
  )
  empty_centroids <- tibble::tibble(
    trait = character(),
    level = character(),
    MDS1 = numeric(),
    MDS2 = numeric(),
    p_value = numeric(),
    r2 = numeric()
  )

  if ((!include_loadings && !include_centroids) || is.null(trait_table)) {
    return(list(
      ordination = ord,
      points = pts,
      loadings = empty_loadings,
      centroids = empty_centroids
    ))
  }

  if (!is.data.frame(trait_table) || nrow(trait_table) != nrow(pts)) {
    stop("'trait_table' must be a data frame aligned to the ordination rows.", call. = FALSE)
  }

  # Coerce character columns to factors for envfit and drop columns that are
  # completely missing or non-informative.
  # Also coerce expanded binary indicator columns (names containing "__") that
  # only carry {0, 1, NA} values to factors so envfit places them in $factors
  # (centroids) rather than $vectors (loadings).
  fit_input <- tibble::as_tibble(trait_table) |>
    dplyr::mutate(dplyr::across(where(is.character), as.factor)) |>
    dplyr::mutate(dplyr::across(
      dplyr::matches("__"),
      function(x) {
        if (is.numeric(x) && all(is.na(x) | x %in% c(0, 1))) {
          factor(as.integer(x), levels = c(0L, 1L))
        } else {
          x
        }
      }
    ))

  keep_cols <- vapply(fit_input, function(x) {
    if (all(is.na(x))) {
      return(FALSE)
    }
    if (is.factor(x)) {
      return(dplyr::n_distinct(stats::na.omit(x)) > 1)
    }
    if (is.numeric(x)) {
      vals <- x[is.finite(x)]
      return(length(vals) > 1 && stats::sd(vals) > 0)
    }
    vals <- unique(x[!is.na(x)])
    length(vals) > 1
  }, logical(1))
  fit_input <- fit_input[, keep_cols, drop = FALSE]

  if (ncol(fit_input) == 0) {
    return(list(
      ordination = ord,
      points = pts,
      loadings = empty_loadings,
      centroids = empty_centroids
    ))
  }

  # Fit trait vectors and factor centroids only after the envfit inputs have
  # been reduced to informative columns.
  envfit_call <- c(
    list(ord = ord, env = fit_input, permutations = 999, na.rm = TRUE),
    envfit_args
  )
  fit <- do.call(vegan::envfit, envfit_call)

  loadings <- empty_loadings
  if (isTRUE(include_loadings) &&
      !is.null(fit$vectors) &&
      !is.null(fit$vectors$arrows)) {
    vec <- as.data.frame(fit$vectors$arrows) |>
      tibble::rownames_to_column("trait")
    if (ncol(vec) >= 3) {
      names(vec)[2:3] <- c("MDS1", "MDS2")
    }

    vec_pvals <- fit$vectors$pvals
    if (!is.null(vec_pvals) && is.null(names(vec_pvals))) {
      names(vec_pvals) <- vec$trait
    }
    vec_r2 <- fit$vectors$r
    if (!is.null(vec_r2) && is.null(names(vec_r2))) {
      names(vec_r2) <- vec$trait
    }

    pvals <- tibble::tibble(
      trait = names(vec_pvals),
      p_value = as.numeric(vec_pvals)
    )
    r2 <- tibble::tibble(
      trait = names(vec_r2),
      r2 = as.numeric(vec_r2)
    )

    loadings <- vec |>
      dplyr::left_join(pvals, by = "trait") |>
      dplyr::left_join(r2, by = "trait")
  }

  centroids <- empty_centroids
  if (isTRUE(include_centroids) &&
      !is.null(fit$factors) &&
      !is.null(fit$factors$centroids)) {
    ctr <- as.data.frame(fit$factors$centroids) |>
      tibble::rownames_to_column("trait_level")
    if (ncol(ctr) >= 3) {
      names(ctr)[2:3] <- c("MDS1", "MDS2")
    }

    # vegan::envfit concatenates trait name + level label with no separator
    # (e.g. "swimbladder_typephysoclist"). Recover the split by matching each
    # centroid rowname against the known factor trait names (from pvals).
    fac_pvals <- fit$factors$pvals   # named by factor column name
    fac_r2    <- fit$factors$r       # named by factor column name
    factor_trait_names <- names(fac_pvals)

    # Sort longest-first so longer names match before their shorter prefixes.
    sorted_traits <- factor_trait_names[order(nchar(factor_trait_names), decreasing = TRUE)]
    ctr$trait <- NA_character_
    ctr$level <- ctr$trait_level
    for (tn in sorted_traits) {
      matches <- startsWith(ctr$trait_level, tn) & is.na(ctr$trait)
      if (any(matches)) {
        ctr$trait[matches] <- tn
        ctr$level[matches] <- substring(ctr$trait_level[matches], nchar(tn) + 1L)
      }
    }
    # Fall back to the full label for any unmatched rows
    ctr$trait <- dplyr::coalesce(ctr$trait, ctr$trait_level)

    pvals <- tibble::tibble(
      trait = names(fac_pvals),
      p_value = as.numeric(fac_pvals)
    )
    r2 <- tibble::tibble(
      trait = names(fac_r2),
      r2 = as.numeric(fac_r2)
    )

    centroids <- ctr |>
      dplyr::select(trait, level, MDS1, MDS2) |>
      dplyr::left_join(pvals, by = "trait") |>
      dplyr::left_join(r2, by = "trait")
  }

  list(
    ordination = ord,
    points = pts,
    loadings = loadings,
    centroids = centroids
  )
}

#' Assign ordination clusters
#'
#' Assigns Ward hierarchical clusters to NMDS point coordinates using the best
#' mean silhouette width up to a maximum `k`.
#'
#' @param points_df NMDS point table with `MDS1` and `MDS2`.
#' @param max_k Maximum number of clusters to evaluate.
#' @param min_silhouette Minimum acceptable mean silhouette width.
#' @param cluster_col Name of the cluster-ID column to create.
#'
#' @return The input point table with cluster columns appended.
#'
#' @export
assign_ordination_groups <- function(points_df,
                                 max_k = 8,
                                 min_silhouette = 0.10,
                                 cluster_col = "nmds_cluster_id") {
  # Validate the point table and clustering arguments before building any
  # coordinate distance objects.
  if (!is.data.frame(points_df)) {
    stop("'points_df' must be a data frame or tibble.", call. = FALSE)
  }
  if (!all(c("MDS1", "MDS2") %in% names(points_df))) {
    stop("'points_df' must contain 'MDS1' and 'MDS2'.", call. = FALSE)
  }
  if (!is.numeric(max_k) || length(max_k) != 1 || !is.finite(max_k) || max_k < 1) {
    stop("'max_k' must be one number >= 1.", call. = FALSE)
  }
  if (!is.numeric(min_silhouette) || length(min_silhouette) != 1 || !is.finite(min_silhouette)) {
    stop("'min_silhouette' must be one finite numeric value.", call. = FALSE)
  }
  if (!is.character(cluster_col) || length(cluster_col) != 1 || !nzchar(cluster_col)) {
    stop("'cluster_col' must be a single column name.", call. = FALSE)
  }

  out <- tibble::as_tibble(points_df)
  if (nrow(out) < 3) {
    out[[cluster_col]] <- "cluster_1"
    out$nmds_cluster_k <- 1L
    out$nmds_cluster_sil <- NA_real_
    return(out)
  }

  # Use the two NMDS axes as the clustering coordinates and bail out early when
  # there is not enough unique geometry for a meaningful partition.
  coords <- as.matrix(out[, c("MDS1", "MDS2"), drop = FALSE])
  if (nrow(unique(coords)) < 3) {
    out[[cluster_col]] <- "cluster_1"
    out$nmds_cluster_k <- 1L
    out$nmds_cluster_sil <- NA_real_
    return(out)
  }

  d <- stats::dist(coords)
  hc <- stats::hclust(d, method = "ward.D2")
  max_k_local <- min(as.integer(max_k), nrow(out) - 1L)
  best_k <- 1L
  best_s <- -Inf

  # Search the admissible cluster counts and retain the partition with the
  # highest mean silhouette width.
  for (k in seq.int(2L, max_k_local)) {
    cl <- stats::cutree(hc, k = k)
    if (length(unique(cl)) < 2) {
      next
    }
    sil <- cluster::silhouette(cl, d)
    sil_mean <- mean(sil[, 3], na.rm = TRUE)
    if (is.finite(sil_mean) && sil_mean > best_s) {
      best_s <- sil_mean
      best_k <- k
    }
  }

  if (!is.finite(best_s) || best_s < min_silhouette) {
    best_k <- 1L
  }

  cl <- if (best_k > 1L) stats::cutree(hc, k = best_k) else rep(1L, nrow(out))
  out[[cluster_col]] <- paste0("cluster_", cl)
  out$nmds_cluster_k <- best_k
  out$nmds_cluster_sil <- if (best_k > 1L) best_s else NA_real_

  out
}

#' Join ordination points to model metadata
#'
#' Adds selected model metadata and an optional reference flag to the ordination
#' points, then assigns ordination clusters.
#'
#' @param ordination_points Ordination point table returned by [run_ordination()].
#' @param candidate_models Candidate-model table.
#' @param reference_ids Optional vector of model IDs to flag as references.
#' @param model_id_col Model-ID column name in `candidate_models`.
#' @param join_cols Additional metadata columns to join from `candidate_models`.
#' @param cluster_args Optional named list passed to [assign_ordination_groups()].
#'
#' @return A tibble.
#'
#' @export
join_ordination_points <- function(ordination_points,
                                   candidate_models,
                                   reference_ids = NULL,
                                   model_id_col = "model_id",
                                   join_cols = c("species_name", "common", "swimbladder_type", "family", "regional_body", "is_group_model"),
                                   cluster_args = list()) {
  # Validate the join inputs and standardize the model IDs before merging the
  # ordination points back to the candidate-model metadata.
  if (!is.data.frame(ordination_points) || !"model_id" %in% names(ordination_points)) {
    stop("'ordination_points' must be a data frame with a 'model_id' column.", call. = FALSE)
  }
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }
  if (!model_id_col %in% names(candidate_models)) {
    stop(sprintf("'%s' was not found in 'candidate_models'.", model_id_col), call. = FALSE)
  }
  if (!is.list(cluster_args)) {
    stop("'cluster_args' must be a list.", call. = FALSE)
  }

  join_cols <- unique(c(model_id_col, join_cols))
  join_cols <- intersect(join_cols, names(candidate_models))
  points_df <- tibble::as_tibble(ordination_points)
  meta_df <- tibble::as_tibble(candidate_models) |>
    dplyr::transmute(
      model_id = as.character(.data[[model_id_col]]),
      model_id_chr = as.character(.data[[model_id_col]]),
      dplyr::across(dplyr::all_of(setdiff(join_cols, model_id_col)))
    )

  # Add an explicit reference flag from the caller-supplied ID vector rather
  # than hard-coding any one regional body or anchor source.
  if (is.null(reference_ids)) {
    ref_tbl <- tibble::tibble(model_id = character(), is_reference = logical())
  } else {
    ref_tbl <- tibble::tibble(
      model_id = as.character(reference_ids),
      is_reference = TRUE
    ) |>
      dplyr::distinct(model_id, .keep_all = TRUE)
  }

  out <- points_df |>
    dplyr::left_join(meta_df, by = "model_id") |>
    dplyr::left_join(ref_tbl, by = "model_id") |>
    dplyr::mutate(is_reference = dplyr::coalesce(is_reference, FALSE))

  do.call(
      assign_ordination_groups,
    c(list(points_df = out), cluster_args)
  )
}

#' Extract NMDS model scores
#'
#' Extracts the compact model-score table commonly used downstream from a
#' clustered NMDS point table.
#'
#' @param points_df Clustered NMDS point table.
#' @param cluster_col Cluster-ID column name.
#' @param reference_col Reference-flag column name.
#'
#' @return A tibble.
#'
#' @export
extract_ordination_scores <- function(points_df,
                                cluster_col = "nmds_cluster_id",
                                reference_col = "is_reference") {
  # Keep only the compact score columns used by downstream neighborhood logic.
  if (!is.data.frame(points_df)) {
    stop("'points_df' must be a data frame or tibble.", call. = FALSE)
  }
  required_cols <- c("model_id_chr", "MDS1", "MDS2", cluster_col, "species_name", reference_col)
  missing_cols <- setdiff(required_cols, names(points_df))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("Missing NMDS score column(s): %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  tibble::as_tibble(points_df) |>
    dplyr::transmute(
      model_id_chr = model_id_chr,
      MDS1 = MDS1,
      MDS2 = MDS2,
      nmds_cluster = .data[[cluster_col]],
      species_name = species_name,
      is_reference = .data[[reference_col]]
    )
}

#' Count NMDS cluster membership
#'
#' @param points_df Clustered NMDS point table.
#' @param cluster_col Cluster-ID column name.
#' @param count_col Output count column name.
#'
#' @return A tibble.
#'
#' @export
count_ordination_groups <- function(points_df,
                                cluster_col = "nmds_cluster_id",
                                count_col = "cluster_n") {
  if (!is.data.frame(points_df) || !cluster_col %in% names(points_df)) {
    stop("'points_df' must contain the requested cluster column.", call. = FALSE)
  }

  tibble::as_tibble(points_df) |>
    dplyr::add_count(.data[[cluster_col]], name = count_col)
}

#' Build NMDS cluster hulls
#'
#' @param points_df Clustered NMDS point table.
#' @param cluster_col Cluster-ID column name.
#' @param min_points Minimum number of points required to form a hull.
#'
#' @return A tibble.
#'
#' @export
build_ordination_hulls <- function(points_df,
                             cluster_col = "nmds_cluster_id",
                             min_points = 3L) {
  # Compute one convex hull per cluster only when that cluster has enough
  # points to define a polygon.
  if (!is.data.frame(points_df)) {
    stop("'points_df' must be a data frame or tibble.", call. = FALSE)
  }
  if (!all(c(cluster_col, "MDS1", "MDS2") %in% names(points_df))) {
    stop("'points_df' must contain the requested cluster and NMDS axis columns.", call. = FALSE)
  }

  tibble::as_tibble(points_df) |>
    dplyr::group_by(.data[[cluster_col]]) |>
    dplyr::filter(dplyr::n() >= as.integer(min_points)) |>
    dplyr::slice(chull(MDS1, MDS2)) |>
    dplyr::ungroup()
}

#' Compute an NMDS plotting scale reference
#'
#' @param points_df NMDS point table with `MDS1` and `MDS2`.
#'
#' @return Numeric scalar.
#'
#' @export
compute_ordination_scale <- function(points_df) {
  if (!is.data.frame(points_df) || !all(c("MDS1", "MDS2") %in% names(points_df))) {
    stop("'points_df' must contain 'MDS1' and 'MDS2'.", call. = FALSE)
  }

  # Use the largest absolute coordinate value across both axes and fall back to
  # `1` when the point cloud is degenerate.
  scale_ref <- max(abs(c(points_df$MDS1, points_df$MDS2)), na.rm = TRUE)
  if (!is.finite(scale_ref) || scale_ref <= 0) {
    return(1)
  }

  scale_ref
}

#' Add NMDS missingness metadata
#'
#' @param points_df NMDS point table with `model_id`.
#' @param candidate_models Candidate-model table.
#' @param trait_cols Trait columns used to compute missingness.
#' @param model_id_col Model-ID column name in `candidate_models`.
#'
#' @return A tibble.
#'
#' @export
add_ordination_missing <- function(points_df,
                                 candidate_models,
                                 trait_cols,
                                 model_id_col = "model_id") {
  # Join per-model missing-trait summaries onto the NMDS points and collapse
  # them into low/medium/high missingness groups by tertiles.
  if (!is.data.frame(points_df) || !"model_id" %in% names(points_df)) {
    stop("'points_df' must be a data frame with a 'model_id' column.", call. = FALSE)
  }
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }
  if (!model_id_col %in% names(candidate_models)) {
    stop(sprintf("'%s' was not found in 'candidate_models'.", model_id_col), call. = FALSE)
  }

  trait_cols <- intersect(as.character(trait_cols), names(candidate_models))
  if (length(trait_cols) == 0) {
    stop("No valid 'trait_cols' were supplied.", call. = FALSE)
  }

  missing_df <- tibble::as_tibble(candidate_models) |>
    dplyr::transmute(
      model_id = as.character(.data[[model_id_col]]),
      missing_trait_count = rowSums(is.na(dplyr::pick(dplyr::all_of(trait_cols)))),
      missing_trait_fraction = rowMeans(is.na(dplyr::pick(dplyr::all_of(trait_cols))))
    )

  q1 <- stats::quantile(missing_df$missing_trait_fraction, 1 / 3, na.rm = TRUE, names = FALSE)
  q2 <- stats::quantile(missing_df$missing_trait_fraction, 2 / 3, na.rm = TRUE, names = FALSE)

  tibble::as_tibble(points_df) |>
    dplyr::left_join(missing_df, by = "model_id") |>
    dplyr::mutate(
      missingness_group = dplyr::case_when(
        missing_trait_fraction <= q1 ~ "low",
        missing_trait_fraction <= q2 ~ "medium",
        TRUE ~ "high"
      )
    )
}

#' Refine species clusters by separation tests
#'
#' Merges species-level NMDS clusters that are not significantly separated in
#' the supplied species distance matrix.
#'
#' @param species_points_df Species-level NMDS point table.
#' @param dist_mat Species-by-species distance matrix.
#' @param alpha Significance cutoff for pairwise separation.
#' @param permutations Number of permutations passed to `vegan::adonis2()`.
#' @param cluster_col Species-cluster column name.
#' @param species_col Species-name column name.
#'
#' @return A list with `points` and `pairwise_tests`.
#'
#' @export
refine_species_clusters <- function(species_points_df,
                                    dist_mat,
                                    alpha = 0.05,
                                    permutations = 999,
                                    cluster_col = "species_cluster_id",
                                    species_col = "species_name") {
  # Keep one finite NMDS point per species for the cluster-refinement step and
  # fail early when the required cluster/species columns are absent.
  if (!is.data.frame(species_points_df)) {
    stop("'species_points_df' must be a data frame or tibble.", call. = FALSE)
  }
  if (!all(c(species_col, cluster_col, "MDS1", "MDS2") %in% names(species_points_df))) {
    stop("'species_points_df' is missing required species-cluster columns.", call. = FALSE)
  }
  if (!is.matrix(dist_mat)) {
    stop("'dist_mat' must be a species-by-species matrix.", call. = FALSE)
  }

  out <- tibble::as_tibble(species_points_df) |>
    dplyr::filter(is.finite(MDS1), is.finite(MDS2)) |>
    dplyr::distinct(.data[[species_col]], .keep_all = TRUE)

  if (nrow(out) < 3 || !cluster_col %in% names(out)) {
    return(list(points = species_points_df, pairwise_tests = tibble::tibble()))
  }

  # Recompute all pairwise cluster tests after each merge so the refinement
  # step always evaluates the current cluster layout.
  compute_pairwise_tests <- function(df_now) {
    cluster_ids <- sort(unique(df_now[[cluster_col]]))
    if (length(cluster_ids) < 2) {
      return(tibble::tibble())
    }

    purrr::map_dfr(utils::combn(cluster_ids, 2, simplify = FALSE), function(pair_ids) {
      sub_df <- df_now |>
        dplyr::filter(.data[[cluster_col]] %in% pair_ids)

      group_sizes <- table(sub_df[[cluster_col]])
      centroid_tbl <- sub_df |>
        dplyr::group_by(.data[[cluster_col]]) |>
        dplyr::summarise(
          MDS1 = mean(MDS1, na.rm = TRUE),
          MDS2 = mean(MDS2, na.rm = TRUE),
          .groups = "drop"
        )
      centroid_dist <- sqrt(diff(centroid_tbl$MDS1)^2 + diff(centroid_tbl$MDS2)^2)

      p_val <- NA_real_
      if (length(group_sizes) == 2 && all(group_sizes >= 2)) {
        spp <- as.character(sub_df[[species_col]])
        sub_dist <- dist_mat[spp, spp, drop = FALSE]
        meta <- data.frame(cluster = factor(sub_df[[cluster_col]]))
        fit <- tryCatch(
          vegan::adonis2(stats::as.dist(sub_dist) ~ cluster, data = meta, permutations = permutations),
          error = function(e) NULL
        )
        if (!is.null(fit) && nrow(fit) >= 1) {
          p_val <- suppressWarnings(as.numeric(fit$`Pr(>F)`[[1]]))
        }
      }

      tibble::tibble(
        cluster_a = pair_ids[[1]],
        cluster_b = pair_ids[[2]],
        n_a = unname(group_sizes[[pair_ids[[1]]]] %||% 0L),
        n_b = unname(group_sizes[[pair_ids[[2]]]] %||% 0L),
        centroid_distance = centroid_dist,
        p_value = p_val
      )
    })
  }

  # Merge the least-separated non-significant pair each round until every
  # remaining pair is significantly separated or only one cluster remains.
  repeat {
    tests <- compute_pairwise_tests(out)
    non_sig <- tests |>
      dplyr::filter(!is.finite(p_value) | p_value >= alpha)

    if (nrow(non_sig) == 0) {
      break
    }

    merge_pair <- non_sig |>
      dplyr::arrange(dplyr::desc(dplyr::coalesce(p_value, 1)), centroid_distance) |>
      dplyr::slice(1)

    out <- out |>
      dplyr::mutate(
        !!cluster_col := dplyr::if_else(
          .data[[cluster_col]] == merge_pair$cluster_b[[1]],
          merge_pair$cluster_a[[1]],
          .data[[cluster_col]]
        )
      )

    if (dplyr::n_distinct(out[[cluster_col]]) <= 1) {
      break
    }
  }

  # Relabel the surviving clusters to a compact sequential cluster index.
  final_ids <- sort(unique(out[[cluster_col]]))
  relabel <- stats::setNames(paste0("cluster_", seq_along(final_ids)), final_ids)
  out <- out |>
    dplyr::mutate(!!cluster_col := unname(relabel[.data[[cluster_col]]]))

  final_tests <- compute_pairwise_tests(out) |>
    dplyr::mutate(significant = is.finite(p_value) & p_value < alpha)

  points_out <- tibble::as_tibble(species_points_df) |>
    dplyr::left_join(
      out |> dplyr::select(dplyr::all_of(c(species_col, cluster_col))),
      by = species_col,
      suffix = c("", "_refined")
    ) |>
    dplyr::mutate(
      !!cluster_col := dplyr::coalesce(.data[[paste0(cluster_col, "_refined")]], .data[[cluster_col]])
    ) |>
    dplyr::select(-dplyr::any_of(paste0(cluster_col, "_refined")))

  list(points = points_out, pairwise_tests = final_tests)
}

#' Build a species cluster lookup
#'
#' Maps each anchor species to the model identifiers belonging to species in
#' the same species-level ordination cluster.
#'
#' @param species_points_df Species-level NMDS point table.
#' @param candidate_models Candidate-model table.
#' @param level Stored level metadata for the manifest.
#' @param cluster_col Species-cluster column name.
#' @param species_col Species-name column name.
#' @param model_id_col Model-ID column name in `candidate_models`.
#'
#' @return A list with `lookup` and `manifest`.
#'
#' @export
build_species_lookup <- function(species_points_df,
                                 candidate_models,
                                 level = 0.80,
                                 cluster_col = "species_cluster_id",
                                 species_col = "species_name",
                                 model_id_col = "model_id_chr") {
  # Treat the historical "ellipse" neighborhood as a species-cluster lookup
  # keyed by anchor species.
  if (!is.data.frame(species_points_df)) {
    stop("'species_points_df' must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.data.frame(candidate_models)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }
  if (!all(c(species_col, cluster_col, "MDS1", "MDS2") %in% names(species_points_df))) {
    stop("'species_points_df' is missing required species-cluster columns.", call. = FALSE)
  }
  if (!all(c(species_col, model_id_col) %in% names(candidate_models))) {
    stop("'candidate_models' is missing the requested species/model ID columns.", call. = FALSE)
  }

  species_points_df <- tibble::as_tibble(species_points_df) |>
    dplyr::filter(is.finite(MDS1), is.finite(MDS2)) |>
    dplyr::distinct(.data[[species_col]], .keep_all = TRUE)

  species_levels <- sort(unique(stats::na.omit(as.character(species_points_df[[species_col]]))))
  lookup <- list()
  manifest <- list()

  for (spp in species_levels) {
    anchor_df <- species_points_df |>
      dplyr::filter(.data[[species_col]] == spp, is.finite(MDS1), is.finite(MDS2))

    if (nrow(anchor_df) == 0) {
      lookup[[spp]] <- character(0)
      manifest[[length(manifest) + 1]] <- tibble::tibble(
        species_name = spp,
        species_cluster_id = NA_character_,
        n_anchor_points = 0L,
        n_cluster_species = 0L,
        n_species_inside = 0L,
        n_ids_inside = 0L,
        ellipse_level = level,
        ellipse_method = "missing_anchor"
      )
      next
    }

    # Gather every candidate-model ID whose species falls inside the anchor's
    # species-level cluster.
    anchor_cluster <- as.character(anchor_df[[cluster_col]][[1]])
    cluster_species_df <- species_points_df |>
      dplyr::filter(.data[[cluster_col]] == anchor_cluster)
    candidate_species <- unique(as.character(cluster_species_df[[species_col]]))

    ids <- tibble::as_tibble(candidate_models) |>
      dplyr::filter(.data[[species_col]] %in% candidate_species) |>
      dplyr::pull(.data[[model_id_col]]) |>
      as.character() |>
      unique()

    lookup[[spp]] <- ids
    manifest[[length(manifest) + 1]] <- tibble::tibble(
      species_name = spp,
      species_cluster_id = anchor_cluster,
      n_anchor_points = nrow(anchor_df),
      n_cluster_species = dplyr::n_distinct(cluster_species_df[[species_col]]),
      n_species_inside = length(candidate_species),
      n_ids_inside = length(ids),
      ellipse_level = level,
      ellipse_method = "species_cluster_lookup"
    )
  }

  list(lookup = lookup, manifest = dplyr::bind_rows(manifest))
}

#' Build anchor-specific NMDS info
#'
#' @param anchor_row One-row anchor table.
#' @param model_scores Model-level NMDS score table.
#' @param species_lookup Species lookup returned by [build_species_lookup()].
#' @param anchor_id_col Anchor-ID column name.
#' @param score_id_col Model-score ID column name.
#' @param cluster_col Cluster column name in `model_scores`.
#' @param species_col Species-name column name.
#'
#' @return A list.
#'
#' @export
build_anchor_ordination <- function(anchor_row,
                              model_scores,
                              species_lookup,
                              anchor_id_col = "model_id",
                              score_id_col = "model_id_chr",
                              cluster_col = "nmds_cluster",
                              species_col = "species_name") {
  # Extract the anchor's ordination cluster and the precomputed same-cluster
  # model-ID lookup so later policy code can use both directly.
  if (!is.data.frame(anchor_row) || nrow(anchor_row) != 1) {
    stop("'anchor_row' must be a one-row data frame.", call. = FALSE)
  }
  if (!is.data.frame(model_scores)) {
    stop("'model_scores' must be a data frame or tibble.", call. = FALSE)
  }

  anchor_id <- as.character(anchor_row[[anchor_id_col]][[1]])
  anchor_species <- as.character(anchor_row[[species_col]][[1]])
  anchor_point <- tibble::as_tibble(model_scores) |>
    dplyr::filter(.data[[score_id_col]] == anchor_id) |>
    dplyr::slice(1)

  list(
    model_scores = model_scores,
    anchor_cluster = if (nrow(anchor_point) > 0) as.character(anchor_point[[cluster_col]][[1]]) else NA_character_,
    species_ellipse_ids = species_lookup[[anchor_species]] %||% character(0)
  )
}
