# Expand semicolon-delimited multi-valued character columns into binary
# {col}__{level} indicator columns before passing to vegan::envfit.
# Without this, "Atlantic Ocean; Pacific Ocean" is treated as a single factor
# level instead of two separate binary indicators.
#'
#' @keywords internal
#' @noRd
expand_envfit_multival_traits <- function(tbl) {
  char_cols <- names(tbl)[vapply(tbl, is.character, logical(1))]
  for (col in char_cols) {
    x <- tbl[[col]]
    if (!any(grepl(";", x, fixed = TRUE), na.rm = TRUE)) next
    vals_list <- strsplit(trimws(as.character(x)), "\\s*;\\s*")
    all_levels <- sort(unique(trimws(unlist(vals_list, use.names = FALSE))))
    all_levels <- all_levels[nzchar(all_levels) & !is.na(all_levels)]
    for (lv in all_levels) {
      safe_nm <- gsub("[^A-Za-z0-9]", "_", lv)
      tbl[[paste0(col, "__", safe_nm)]] <- as.integer(
        vapply(vals_list, function(v) lv %in% v, logical(1))
      )
    }
    tbl[[col]] <- NULL
  }
  tbl
}

# Synthetic interval-overlap columns are pairwise distance components, not
# row-level ordination envfit labels.
#'
#' @keywords internal
#' @noRd
ordination_synthetic_overlap_traits <- function() {
  unique(c(
    "depth_interval_overlap",
    "length_interval_overlap",
    "study_depth_interval_overlap",
    "study_length_interval_overlap",
    "species_depth_interval_overlap",
    "species_length_interval_overlap"
  ))
}

#' Drop synthetic overlap traits from ordination outputs
#'
#' @param x Object containing trait labels or columns.
#' @param trait_col Optional trait-name column in `x`.
#'
#' @return Object with synthetic overlap traits removed.
#'
#' @keywords internal
#' @noRd
drop_ordination_synthetic_overlap_traits <- function(x,
                                                     trait_col = NULL) {
  synthetic_overlap_traits <- ordination_synthetic_overlap_traits()
  if (is.null(x)) {
    return(x)
  }

  if (is.data.frame(x)) {
    out <- tibble::as_tibble(x)
    if (is.character(trait_col) && length(trait_col) == 1 && trait_col %in% names(out)) {
      trait_values <- as.character(out[[trait_col]])
      keep <- !(trait_values %in% synthetic_overlap_traits | endsWith(trait_values, "_interval_overlap"))
      return(out[keep, , drop = FALSE])
    }
    keep_cols <- !(names(out) %in% synthetic_overlap_traits | endsWith(names(out), "_interval_overlap"))
    return(out[, keep_cols, drop = FALSE])
  }

  x_chr <- as.character(x)
  x_chr[!(x_chr %in% synthetic_overlap_traits | endsWith(x_chr, "_interval_overlap"))]
}

#' Run an NMDS ordination
#'
#' Runs a two-dimensional NMDS on a distance matrix and optionally fits trait
#' vectors and factor centroids with `vegan::envfit()`.
#'
#' @param dist_mat A distance matrix, `dist` object, or a [Candidates] object
#'   with constructed Gower distances.
#' @param trait_table Optional trait table aligned to `dist_mat`.
#' @param nmds_args Optional named list of arguments passed to
#'   `vegan::metaMDS()`.
#' @param include_loadings Logical scalar. If `TRUE`, return envfit vector
#'   loadings for numeric traits.
#' @param include_centroids Logical scalar. If `TRUE`, return envfit centroids
#'   for factor traits.
#' @param envfit_args Optional named list of arguments passed to
#'   `vegan::envfit()`.
#' @param reference_ids Optional vector of reference model IDs. When `dist_mat`
#'   is a [Candidates] object and `reference_ids` is `NULL`, the stored
#'   reference-anchor table is used automatically.
#' @param join_cols Additional model metadata columns to join onto the stored
#'   model-level ordination points when `dist_mat` is a [Candidates] object.
#' @param cluster_args Optional named list passed to
#'   `assign_ordination_groups()` for the model-level ordination.
#' @param species_cluster_args Optional named list passed to
#'   `assign_ordination_groups()` for the species-level ordination.
#' @param species_refine_args Optional named list passed to
#'   `refine_species_clusters()` for the species-level ordination.
#' @param model_id_col Model-ID column name used when `dist_mat` is a
#'   [Candidates] object.
#' @param progress Logical scalar controlling stage messages.
#'
#' @return When `dist_mat` is a [Candidates] object, returns that object with a
#'   downstream-ready ordination bundle. Otherwise,
#'   returns a list with `ordination`, `points`, `loadings`, and `centroids`.
#'
#' @examples
#' \dontrun{
#' candidates <- build_candidates(list(
#'   study = list(path = "input.xlsx"),
#'   anchors = list(selector = list(regional_body = "SWFSC"))
#' ))
#' candidates <- prepare_similarities(candidate_models = candidates)
#' candidates <- construct_gower_distances(candidates)
#' candidates <- run_ordination(candidates)
#' candidates
#' }
#'
#' @export
run_ordination <- function(dist_mat,
                           trait_table = NULL,
                           nmds_args = NULL,
                           include_loadings = NULL,
                           include_centroids = NULL,
                           envfit_args = NULL,
                           reference_ids = NULL,
                           join_cols = c("species_name", "common", "swimbladder_type", "family", "regional_body", "is_group_model"),
                           cluster_args = list(),
                           species_cluster_args = list(cluster_col = "species_cluster_id"),
                           species_refine_args = list(cluster_col = "species_cluster_id"),
                           model_id_col = "model_id",
                           progress = NULL) {
  # Alchemist path: delegate to the dedicated internal handler so learned
  # distances feed directly into NMDS without requiring a Gower bundle.
  if (is_s7_instance(dist_mat, "Alchemist")) {
    return(.run_ordination_alchemist(
      alchemist = dist_mat,
      nmds_args = nmds_args,
      include_loadings = include_loadings,
      include_centroids = include_centroids,
      envfit_args = envfit_args,
      reference_ids = reference_ids,
      join_cols = join_cols,
      cluster_args = cluster_args,
      model_id_col = model_id_col,
      progress = progress
    ))
  }

  # Support the high-level `Candidates` path first so model and
  # species ordination support objects can be built in one call and stored back
  # onto the Candidates object.
  if (is_s7_instance(dist_mat, "Candidates")) {
    candidates_obj <- dist_mat
    cfg_data <- candidates_config_data(candidates_obj)
    ordination_cfg <- cfg_data$ordination %||% list()
    progress <- progress %||% ordination_cfg$progress %||% FALSE
    report_progress(progress, "Running ordination.")
    nmds_args <- nmds_args %||% ordination_cfg$nmds_args %||% list()
    envfit_args <- envfit_args %||% ordination_cfg$envfit_args %||% list()
    include_loadings <- include_loadings %||% ordination_cfg$include_loadings %||% FALSE
    include_centroids <- include_centroids %||% ordination_cfg$include_centroids %||% FALSE
    if (length(candidates_obj@gower_distances) == 0) {
      stop(
        "Candidates object has no Gower-distance bundle. Run `construct_gower_distances()` first.",
        call. = FALSE
      )
    }
    if (length(candidates_obj@similarity_matrix) == 0) {
      stop(
        "Candidates object has no prepared similarity state. Run `prepare_similarities()` first.",
        call. = FALSE
      )
    }

    if (!is.list(cluster_args)) {
      stop("'cluster_args' must be a list.", call. = FALSE)
    }
    if (!is.list(species_cluster_args)) {
      stop("'species_cluster_args' must be a list.", call. = FALSE)
    }
    if (!is.list(species_refine_args)) {
      stop("'species_refine_args' must be a list.", call. = FALSE)
    }
    if (!is.character(model_id_col) || length(model_id_col) != 1 || !nzchar(model_id_col)) {
      stop("'model_id_col' must be a single column name.", call. = FALSE)
    }

    distance_obj <- candidates_obj@gower_distances
    similarity_obj <- candidates_obj@similarity_matrix
    candidate_models <- tibble::as_tibble(candidates_obj@candidate_models)
    trait_cols <- as.character(distance_obj$trait_cols %||% character(0))
    model_cluster_col <- as.character(cluster_args$cluster_col %||% "nmds_cluster_id")
    species_cluster_col <- as.character(species_cluster_args$cluster_col %||% "species_cluster_id")

    # Default the reference ID set from the stored anchor table so callers do
    # not need to repeat the anchor selection step when running ordination.
    if (is.null(reference_ids) && nrow(candidates_obj@reference_anchors) > 0) {
      if ("model_id" %in% names(candidates_obj@reference_anchors)) {
        reference_ids <- as.character(candidates_obj@reference_anchors$model_id)
      } else if ("model_id" %in% names(candidates_obj@reference_anchors)) {
        reference_ids <- as.character(candidates_obj@reference_anchors$model_id)
      }
    }

    model_trait_cols <- intersect(trait_cols, names(candidate_models))
    model_trait_table <- if (length(model_trait_cols) > 0) {
      candidate_models |>
        dplyr::select(dplyr::all_of(model_trait_cols))
    } else {
      NULL
    }

    model_ordination <- run_ordination(
      dist_mat = distance_obj$combined_dist,
      trait_table = model_trait_table,
      nmds_args = nmds_args,
      include_loadings = include_loadings,
      include_centroids = include_centroids,
      envfit_args = envfit_args
    )

    model_points <- join_ordination_points(
      ordination_points = model_ordination$points,
      candidate_models = candidate_models,
      reference_ids = reference_ids,
      model_id_col = model_id_col,
      join_cols = join_cols,
      cluster_args = cluster_args
    )
    model_scores <- extract_ordination_scores(
      points_df = model_points,
      cluster_col = model_cluster_col
    )
    model_points_missing <- if (length(model_trait_cols) > 0) {
      add_ordination_missing(
        points_df = model_points,
        candidate_models = candidate_models,
        trait_cols = model_trait_cols,
        model_id_col = model_id_col
      )
    } else {
      tibble::as_tibble(model_points) |>
        dplyr::mutate(
          missing_trait_count = NA_integer_,
          missing_trait_fraction = NA_real_,
          missingness_group = NA_character_
        )
    }

    model_hulls <- tryCatch(
      build_ordination_hulls(model_points, cluster_col = model_cluster_col),
      error = function(e) tibble::tibble()
    )
    model_scale <- compute_ordination_scale(model_points)

    species_points_empty <- tibble::tibble(
      species_name = character(),
      MDS1 = numeric(),
      MDS2 = numeric(),
      cluster_id = character(),
      nmds_cluster_k = integer(),
      nmds_cluster_sil = numeric()
    ) |>
      dplyr::rename(!!species_cluster_col := .data$cluster_id)
    species_pairwise_tests <- tibble::tibble()
    species_lookup <- list()
    species_manifest <- tibble::tibble()
    species_ordination <- list(
      ordination = NULL,
      points = species_points_empty,
      loadings = tibble::tibble(),
      centroids = tibble::tibble()
    )

    species_dist <- distance_obj$species_dist
    species_trait_cols <- intersect(trait_cols, names(similarity_obj$species_profiles %||% tibble::tibble()))
    species_labels <- if (inherits(species_dist, "dist")) {
      attr(species_dist, "Labels") %||% character(0)
    } else {
      rownames(species_dist) %||% character(0)
    }
    species_size <- if (inherits(species_dist, "dist")) {
      as.integer(attr(species_dist, "Size") %||% 0L)
    } else if (is.matrix(species_dist)) {
      nrow(species_dist)
    } else {
      0L
    }
    can_run_species <- (inherits(species_dist, "dist") || is.matrix(species_dist)) &&
      species_size >= 2 &&
      length(unique(species_labels)) >= 2

    if (isTRUE(can_run_species)) {
      species_trait_table <- if (length(species_trait_cols) > 0) {
        tibble::as_tibble(similarity_obj$species_profiles) |>
          dplyr::select(dplyr::all_of(species_trait_cols))
      } else {
        NULL
      }

      species_ordination <- run_ordination(
        dist_mat = species_dist,
        trait_table = species_trait_table,
        nmds_args = nmds_args,
        include_loadings = include_loadings,
        include_centroids = include_centroids,
        envfit_args = envfit_args
      )

      species_points <- species_ordination$points |>
        dplyr::rename(species_name = .data$model_id)

      species_points <- do.call(
        assign_ordination_groups,
        c(list(points_df = species_points), species_cluster_args)
      )

      species_refined <- do.call(
        refine_species_clusters,
        c(
          list(
            species_points_df = species_points,
            dist_mat = species_dist
          ),
          species_refine_args
        )
      )
      species_points <- tibble::as_tibble(species_refined$points)
      species_pairwise_tests <- tibble::as_tibble(species_refined$pairwise_tests)

      lookup_model_id_col <- if (endsWith(model_id_col, "_chr")) {
        model_id_col
      } else if (paste0(model_id_col, "_chr") %in% names(candidate_models)) {
        paste0(model_id_col, "_chr")
      } else {
        model_id_col
      }

      species_lookup_obj <- build_species_lookup(
        species_points_df = species_points,
        candidate_models = candidate_models,
        cluster_col = species_cluster_col,
        model_id_col = lookup_model_id_col
      )
      species_lookup <- species_lookup_obj$lookup
      species_manifest <- species_lookup_obj$manifest

      species_ordination <- list(
        ordination = species_ordination$ordination,
        points = species_points,
        loadings = species_ordination$loadings,
        centroids = species_ordination$centroids
      )
    }

    ordination_bundle <- list(
      model = list(
        ordination = model_ordination$ordination,
        points = tibble::as_tibble(model_points),
        loadings = model_ordination$loadings,
        centroids = model_ordination$centroids,
        model_scores = tibble::as_tibble(model_scores),
        points_missing = tibble::as_tibble(model_points_missing),
        hulls = tibble::as_tibble(model_hulls),
        scale = model_scale
      ),
      species = list(
        ordination = species_ordination$ordination,
        points = tibble::as_tibble(species_ordination$points),
        loadings = tibble::as_tibble(species_ordination$loadings),
        centroids = tibble::as_tibble(species_ordination$centroids),
        pairwise_tests = tibble::as_tibble(species_pairwise_tests)
      ),
      species_lookup = species_lookup,
      species_manifest = tibble::as_tibble(species_manifest),
      reference_ids = as.character(reference_ids %||% character(0)),
      trait_cols = trait_cols
    )
    report_progress(progress, "Completed ordination.")

    return(candidates_with_ordination(candidates_obj, ordination_bundle))
  }

  nmds_args <- nmds_args %||% list()
  envfit_args <- envfit_args %||% list()
  include_loadings <- include_loadings %||% FALSE
  include_centroids <- include_centroids %||% FALSE

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
  # Keep the default restart budget modest because the package object may run
  # both model-level and species-level ordinations in one pass, and callers can
  # always raise these values explicitly when they want a more exhaustive NMDS
  # search.
  nmds_defaults <- list(
    comm = dist_obj,
    k = 2,
    try = 6,
    trymax = 12,
    autotransform = FALSE,
    trace = FALSE
  )
  nmds_override <- nmds_args
  if (length(nmds_override) > 0) {
    nmds_override <- nmds_override[!is.na(names(nmds_override)) & nzchar(names(nmds_override))]
  }
  if (length(nmds_override) > 0) {
    nmds_defaults[names(nmds_override)] <- NULL
  }
  nmds_call <- c(nmds_defaults, nmds_override)
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
  fit_input <- drop_ordination_synthetic_overlap_traits(trait_table) |>
    expand_envfit_multival_traits() |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), as.factor)) |>
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
  envfit_defaults <- list(ord = ord, env = fit_input, permutations = 999, na.rm = TRUE)
  envfit_override <- envfit_args
  if (length(envfit_override) > 0) {
    envfit_override <- envfit_override[!is.na(names(envfit_override)) & nzchar(names(envfit_override))]
  }
  if (length(envfit_override) > 0) {
    envfit_defaults[names(envfit_override)] <- NULL
  }
  envfit_call <- c(envfit_defaults, envfit_override)
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
    fac_pvals <- fit$factors$pvals # named by factor column name
    fac_r2 <- fit$factors$r # named by factor column name
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
      dplyr::select("trait", "level", "MDS1", "MDS2") |>
      dplyr::left_join(pvals, by = "trait") |>
      dplyr::left_join(r2, by = "trait")

    # Remap expanded binary centroids back to interpretable labels.
    # expand_envfit_multival_traits() produces columns named {trait}__{level};
    # vegan appends the factor level ("0" or "1") to produce centroids like
    # "ocean_basin__atlantic1". Here we:
    #   1. Drop absence centroids (level == "0") - only "presence" is meaningful
    #   2. Set trait = parent name (e.g., "ocean_basin")
    #   3. Set level = child value (e.g., "atlantic ocean")
    if (nrow(centroids) > 0L) {
      is_expanded <- grepl("__", centroids$trait, fixed = TRUE)
      if (any(is_expanded)) {
        centroids <- centroids[!is_expanded | centroids$level == "1", , drop = FALSE]
        is_expanded <- grepl("__", centroids$trait, fixed = TRUE)
        centroids$level[is_expanded] <- gsub(
          "_", " ",
          sub("^.*__", "", centroids$trait[is_expanded])
        )
        centroids$trait[is_expanded] <- sub("__.*$", "", centroids$trait[is_expanded])
      }
    }
  }

  list(
    ordination = ord,
    points = pts,
    loadings = loadings,
    centroids = centroids
  )
}

#' Select an automatic ordination cluster count
#'
#' @param cluster_scores Data frame with `k` and `silhouette` columns.
#' @param min_silhouette Minimum acceptable mean silhouette width.
#' @param selection_rule Cluster-count selection rule. `"granular_silhouette"`
#'   keeps the most resolved cluster count within `silhouette_tolerance` of the
#'   best silhouette. `"max_silhouette"` keeps the historical maximum-silhouette
#'   rule.
#' @param silhouette_tolerance Absolute silhouette-width tolerance used by
#'   `"granular_silhouette"`.
#'
#' @return Integer cluster count.
#'
#' @keywords internal
#' @noRd
select_ordination_cluster_k <- function(cluster_scores,
                                        min_silhouette = 0.10,
                                        selection_rule = c("granular_silhouette", "max_silhouette"),
                                        silhouette_tolerance = 0.05) {
  scores <- tibble::as_tibble(cluster_scores)
  selection_rule <- match.arg(selection_rule)
  if (nrow(scores) == 0 ||
    !all(c("k", "silhouette") %in% names(scores))) {
    return(1L)
  }
  scores <- scores |>
    dplyr::mutate(
      k = suppressWarnings(as.integer(.data$k)),
      silhouette = suppressWarnings(as.numeric(.data$silhouette))
    ) |>
    dplyr::filter(is.finite(.data$k), .data$k >= 2L, is.finite(.data$silhouette))
  if ("hull_overlap_n" %in% names(scores) &&
    any(is.finite(scores$hull_overlap_n) & scores$hull_overlap_n == 0L)) {
    scores <- scores |>
      dplyr::filter(is.finite(.data$hull_overlap_n), .data$hull_overlap_n == 0L)
  }
  if (nrow(scores) == 0) {
    return(1L)
  }

  best_s <- max(scores$silhouette, na.rm = TRUE)
  if (!is.finite(best_s) || best_s < min_silhouette) {
    return(1L)
  }
  if (identical(selection_rule, "max_silhouette")) {
    best_row <- scores |>
      dplyr::arrange(dplyr::desc(.data$silhouette), .data$k) |>
      dplyr::slice(1L)
    return(as.integer(best_row$k[[1]]))
  }

  tolerance <- suppressWarnings(as.numeric(silhouette_tolerance[[1]]))
  if (!is.finite(tolerance) || tolerance < 0) {
    tolerance <- 0
  }
  eligible <- scores |>
    dplyr::filter(.data$silhouette >= best_s - tolerance) |>
    dplyr::arrange(dplyr::desc(.data$k))
  as.integer(eligible$k[[1]])
}

#' Count overlapping convex hulls for ordination cluster labels
#'
#' @param coords Two-column matrix of ordination coordinates.
#' @param clusters Cluster labels.
#'
#' @return Integer count of overlapping hull pairs.
#' @keywords internal
#' @noRd
ordination_hull_overlap_count <- function(coords, clusters) {
  coords <- as.matrix(coords)
  clusters <- as.character(clusters)
  if (!is.matrix(coords) || ncol(coords) < 2 || length(clusters) != nrow(coords)) {
    return(NA_integer_)
  }
  point_in_poly <- function(px, py, poly) {
    n <- nrow(poly)
    if (n < 3L) {
      return(FALSE)
    }
    inside <- FALSE
    j <- n
    for (i in seq_len(n)) {
      yi <- poly[i, 2]
      yj <- poly[j, 2]
      xi <- poly[i, 1]
      xj <- poly[j, 1]
      crosses <- ((yi > py) != (yj > py)) &&
        (px < (xj - xi) * (py - yi) / ((yj - yi) + .Machine$double.eps) + xi)
      if (isTRUE(crosses)) {
        inside <- !inside
      }
      j <- i
    }
    inside
  }
  orientation <- function(a, b, c) {
    val <- (b[2] - a[2]) * (c[1] - b[1]) - (b[1] - a[1]) * (c[2] - b[2])
    if (abs(val) < 1e-10) {
      return(0L)
    }
    if (val > 0) 1L else 2L
  }
  on_segment <- function(a, b, c) {
    b[1] <= max(a[1], c[1]) + 1e-10 &&
      b[1] + 1e-10 >= min(a[1], c[1]) &&
      b[2] <= max(a[2], c[2]) + 1e-10 &&
      b[2] + 1e-10 >= min(a[2], c[2])
  }
  segments_intersect <- function(p1, q1, p2, q2) {
    o1 <- orientation(p1, q1, p2)
    o2 <- orientation(p1, q1, q2)
    o3 <- orientation(p2, q2, p1)
    o4 <- orientation(p2, q2, q1)
    if (o1 != o2 && o3 != o4) {
      return(TRUE)
    }
    if (o1 == 0L && on_segment(p1, p2, q1)) {
      return(TRUE)
    }
    if (o2 == 0L && on_segment(p1, q2, q1)) {
      return(TRUE)
    }
    if (o3 == 0L && on_segment(p2, p1, q2)) {
      return(TRUE)
    }
    if (o4 == 0L && on_segment(p2, q1, q2)) {
      return(TRUE)
    }
    FALSE
  }
  hulls <- lapply(split(seq_len(nrow(coords)), clusters), function(idx) {
    pts <- coords[idx, 1:2, drop = FALSE]
    pts <- pts[stats::complete.cases(pts), , drop = FALSE]
    if (nrow(unique(pts)) < 3L) {
      return(NULL)
    }
    pts[grDevices::chull(pts[, 1], pts[, 2]), , drop = FALSE]
  })
  hulls <- hulls[!vapply(hulls, is.null, logical(1))]
  if (length(hulls) < 2L) {
    return(0L)
  }
  overlap_n <- 0L
  for (i in seq_len(length(hulls) - 1L)) {
    for (j in (i + 1L):length(hulls)) {
      a <- hulls[[i]]
      b <- hulls[[j]]
      vertices_inside <- any(vapply(seq_len(nrow(a)), function(ii) point_in_poly(a[ii, 1], a[ii, 2], b), logical(1))) ||
        any(vapply(seq_len(nrow(b)), function(jj) point_in_poly(b[jj, 1], b[jj, 2], a), logical(1)))
      edge_cross <- FALSE
      if (!vertices_inside) {
        for (ai in seq_len(nrow(a))) {
          ai2 <- if (ai == nrow(a)) 1L else ai + 1L
          for (bi in seq_len(nrow(b))) {
            bi2 <- if (bi == nrow(b)) 1L else bi + 1L
            if (segments_intersect(a[ai, ], a[ai2, ], b[bi, ], b[bi2, ])) {
              edge_cross <- TRUE
              break
            }
          }
          if (edge_cross) break
        }
      }
      if (vertices_inside || edge_cross) {
        overlap_n <- overlap_n + 1L
      }
    }
  }
  overlap_n
}

#' Assign ordination clusters
#'
#' Assigns Ward hierarchical clusters to NMDS point coordinates using an
#' automatic silhouette-based cluster-count rule up to a maximum `k`.
#'
#' @param points_df NMDS point table with `MDS1` and `MDS2`.
#' @param k Optional fixed number of clusters. When `NULL`, the function
#'   selects a cluster count by mean silhouette width.
#' @param max_k Maximum number of clusters to evaluate.
#' @param min_silhouette Minimum acceptable mean silhouette width.
#' @param selection_rule Automatic cluster-count selection rule.
#' @param silhouette_tolerance Absolute silhouette-width tolerance for the
#'   granular automatic rule.
#' @param min_cluster_size Minimum allowed cluster size for automatic candidate
#'   partitions.
#' @param max_cluster_fraction Maximum fraction of points allowed in the largest
#'   automatic cluster. Values below 1 prevent outlier-vs-main-cloud partitions
#'   from winning solely by silhouette width.
#' @param cluster_col Name of the cluster-ID column to create.
#'
#' @return The input point table with cluster columns appended.
#'
#' @keywords internal
#' @noRd
assign_ordination_groups <- function(points_df,
                                     k = NULL,
                                     max_k = 8,
                                     min_silhouette = 0.10,
                                     cluster_method = c("pam", "kmeans", "ward"),
                                     selection_rule = c("granular_silhouette", "max_silhouette"),
                                     silhouette_tolerance = 0.05,
                                     min_cluster_size = 2L,
                                     max_cluster_fraction = 0.90,
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
  if (!is.null(k) &&
    (!is.numeric(k) || length(k) != 1 || !is.finite(k) || k < 1)) {
    stop("'k' must be NULL or one number >= 1.", call. = FALSE)
  }
  if (!is.numeric(min_silhouette) || length(min_silhouette) != 1 || !is.finite(min_silhouette)) {
    stop("'min_silhouette' must be one finite numeric value.", call. = FALSE)
  }
  cluster_method <- match.arg(cluster_method)
  selection_rule <- match.arg(selection_rule)
  if (!is.numeric(silhouette_tolerance) ||
    length(silhouette_tolerance) != 1 ||
    !is.finite(silhouette_tolerance) ||
    silhouette_tolerance < 0) {
    stop("'silhouette_tolerance' must be one finite number >= 0.", call. = FALSE)
  }
  if (!is.numeric(min_cluster_size) ||
    length(min_cluster_size) != 1 ||
    !is.finite(min_cluster_size) ||
    min_cluster_size < 1) {
    stop("'min_cluster_size' must be one number >= 1.", call. = FALSE)
  }
  if (!is.null(max_cluster_fraction) &&
    (!is.numeric(max_cluster_fraction) ||
      length(max_cluster_fraction) != 1 ||
      !is.finite(max_cluster_fraction) ||
      max_cluster_fraction <= 0 ||
      max_cluster_fraction > 1)) {
    stop("'max_cluster_fraction' must be NULL or one number in (0, 1].", call. = FALSE)
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
  cluster_for_k <- function(k_value) {
    k_value <- as.integer(k_value)
    if (k_value <= 1L) {
      return(rep(1L, nrow(out)))
    }
    if (identical(cluster_method, "ward")) {
      hc <- stats::hclust(d, method = "ward.D2")
      return(stats::cutree(hc, k = k_value))
    }
    if (identical(cluster_method, "kmeans")) {
      km <- stats::kmeans(coords, centers = k_value, nstart = 50)
      return(as.integer(km$cluster))
    }
    as.integer(cluster::pam(coords, k = k_value)$clustering)
  }
  max_k_local <- min(as.integer(max_k), nrow(out) - 1L)
  if (!is.null(k)) {
    fixed_k <- min(as.integer(k), max_k_local)
    fixed_k <- max(1L, fixed_k)
    cl <- cluster_for_k(fixed_k)
    sil_mean <- if (fixed_k > 1L) {
      sil <- cluster::silhouette(cl, d)
      mean(sil[, 3], na.rm = TRUE)
    } else {
      NA_real_
    }
    out[[cluster_col]] <- paste0("cluster_", cl)
    out$nmds_cluster_k <- fixed_k
    out$nmds_cluster_sil <- sil_mean
    return(out)
  }
  best_k <- 1L
  cluster_scores <- list()
  min_cluster_size <- as.integer(min_cluster_size)

  # Search the admissible cluster counts and retain their silhouette scores.
  for (k in seq.int(2L, max_k_local)) {
    cl <- cluster_for_k(k)
    if (length(unique(cl)) < 2) {
      next
    }
    if (any(tabulate(cl) < min_cluster_size)) {
      next
    }
    cluster_sizes <- tabulate(cl)
    largest_fraction <- max(cluster_sizes) / sum(cluster_sizes)
    if (!is.null(max_cluster_fraction) && largest_fraction > max_cluster_fraction) {
      next
    }
    sil <- cluster::silhouette(cl, d)
    sil_mean <- mean(sil[, 3], na.rm = TRUE)
    if (is.finite(sil_mean)) {
      cluster_scores[[length(cluster_scores) + 1L]] <- tibble::tibble(
        k = as.integer(k),
        silhouette = sil_mean,
        hull_overlap_n = ordination_hull_overlap_count(coords, cl)
      )
    }
  }

  best_k <- select_ordination_cluster_k(
    dplyr::bind_rows(cluster_scores),
    min_silhouette = min_silhouette,
    selection_rule = selection_rule,
    silhouette_tolerance = silhouette_tolerance
  )

  cl <- cluster_for_k(best_k)
  best_s <- if (best_k > 1L) {
    score_tbl <- dplyr::bind_rows(cluster_scores)
    match_idx <- match(best_k, score_tbl$k)
    if (!is.na(match_idx)) score_tbl$silhouette[[match_idx]] else NA_real_
  } else {
    NA_real_
  }
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
#' @param cluster_args Optional named list passed to `assign_ordination_groups()`.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
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
      model_id = as.character(.data[[model_id_col]]),
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
      dplyr::distinct(.data$model_id, .keep_all = TRUE)
  }

  out <- points_df |>
    dplyr::left_join(meta_df, by = "model_id") |>
    dplyr::left_join(ref_tbl, by = "model_id") |>
    dplyr::mutate(is_reference = dplyr::coalesce(.data$is_reference, FALSE))

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
#' @keywords internal
#' @noRd
extract_ordination_scores <- function(points_df,
                                      cluster_col = "nmds_cluster_id",
                                      reference_col = "is_reference") {
  # Keep only the compact score columns used by downstream neighborhood logic.
  if (!is.data.frame(points_df)) {
    stop("'points_df' must be a data frame or tibble.", call. = FALSE)
  }
  required_cols <- c("model_id", "MDS1", "MDS2", cluster_col, "species_name", reference_col)
  missing_cols <- setdiff(required_cols, names(points_df))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("Missing NMDS score column(s): %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  tibble::as_tibble(points_df) |>
    dplyr::transmute(
      model_id = .data$model_id,
      MDS1 = .data$MDS1,
      MDS2 = .data$MDS2,
      nmds_cluster = .data[[cluster_col]],
      species_name = .data$species_name,
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
    dplyr::slice(chull(.data$MDS1, .data$MDS2)) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data[[cluster_col]]) |>
    dplyr::group_modify(~ dplyr::bind_rows(.x, .x[1L, , drop = FALSE])) |>
    dplyr::ungroup()
}

#' Compute an NMDS plotting scale reference
#'
#' @param points_df NMDS point table with `MDS1` and `MDS2`.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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

  # Force evaluation
  force(q1)
  force(q2)

  tibble::as_tibble(points_df) |>
    dplyr::left_join(missing_df, by = "model_id") |>
    dplyr::mutate(
      missingness_group = dplyr::case_when(
        .data$missing_trait_fraction <= q1 ~ "low",
        .data$missing_trait_fraction <= q2 ~ "medium",
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
#' @keywords internal
#' @noRd
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
  if (!(is.matrix(dist_mat) || inherits(dist_mat, "dist"))) {
    stop("'dist_mat' must be a species-by-species matrix or 'dist' object.", call. = FALSE)
  }
  dist_lookup <- if (inherits(dist_mat, "dist")) as.matrix(dist_mat) else dist_mat

  out <- tibble::as_tibble(species_points_df) |>
    dplyr::filter(is.finite(.data$MDS1), is.finite(.data$MDS2)) |>
    dplyr::distinct(.data[[.data$species_col]], .keep_all = TRUE)

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
          MDS1 = mean(.data$MDS1, na.rm = TRUE),
          MDS2 = mean(.data$MDS2, na.rm = TRUE),
          .groups = "drop"
        )
      centroid_dist <- sqrt(diff(centroid_tbl$MDS1)^2 + diff(centroid_tbl$MDS2)^2)

      p_val <- NA_real_
      if (length(group_sizes) == 2 && all(group_sizes >= 2)) {
        spp <- as.character(sub_df[[species_col]])
        sub_dist <- dist_lookup[spp, spp, drop = FALSE]

        # Force evaluation
        force(sub_dist)

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
      dplyr::filter(!is.finite(.data$p_value) | .data$p_value >= alpha)

    if (nrow(non_sig) == 0) {
      break
    }

    merge_pair <- non_sig |>
      dplyr::arrange(dplyr::desc(dplyr::coalesce(.data$p_value, 1)), .data$centroid_distance) |>
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
    dplyr::mutate(significant = is.finite(.data$p_value) & .data$p_value < alpha)

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
#' @keywords internal
#' @noRd
build_species_lookup <- function(species_points_df,
                                 candidate_models,
                                 level = 0.80,
                                 cluster_col = "species_cluster_id",
                                 species_col = "species_name",
                                 model_id_col = "model_id") {
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
    dplyr::filter(is.finite(.data$MDS1), is.finite(.data$MDS2)) |>
    dplyr::distinct(.data[[.data$species_col]], .keep_all = TRUE)

  species_levels <- sort(unique(stats::na.omit(as.character(species_points_df[[species_col]]))))
  lookup <- list()
  manifest <- list()

  for (spp in species_levels) {
    anchor_df <- species_points_df |>
      dplyr::filter(.data[[species_col]] == spp, is.finite(.data$MDS1), is.finite(.data$MDS2))

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
#' @param species_lookup Species lookup returned by `build_species_lookup()`.
#' @param anchor_id_col Anchor-ID column name.
#' @param score_id_col Model-score ID column name.
#' @param cluster_col Cluster column name in `model_scores`.
#' @param species_col Species-name column name.
#'
#' @return A list.
#'
#' @keywords internal
#' @noRd
build_anchor_ordination <- function(anchor_row,
                                    model_scores,
                                    species_lookup,
                                    anchor_id_col = "model_id",
                                    score_id_col = "model_id",
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
