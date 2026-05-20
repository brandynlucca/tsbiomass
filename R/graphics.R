graphics_placeholder <- function(title,
                                 subtitle = "Required plotting fields were not available.",
                                 x = NULL,
                                 y = NULL) {
  ggplot2::ggplot() +
    ggplot2::labs(title = title, subtitle = subtitle, x = x, y = y) +
    ggplot2::theme_minimal(base_size = 11)
}

graphics_has_cols <- function(x,
                              cols) {
  all(cols %in% names(x))
}

#' Plot uncertainty blocks
#'
#' @param dropout_tbl Uncertainty dropout table.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_uncertainty_blocks <- function(dropout_tbl,
                                    anchor_label) {
  # Convert the block labels to a plotting order before building the chart so
  # the most important blocks appear first.
  plot_df <- tibble::as_tibble(dropout_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("block", "importance_score", "delta_log_spread"))) {
    return(graphics_placeholder(
      title = paste0("Local Dropout Sensitivity [", anchor_label, "]"),
      y = "Heuristic importance"
    ))
  }
  plot_df <- plot_df |>
    dplyr::mutate(block = factor(block, levels = rev(unique(block))))

  ggplot2::ggplot(plot_df, ggplot2::aes(x = block, y = importance_score, fill = delta_log_spread)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0) +
    ggplot2::labs(
      title = paste0("Local Dropout Sensitivity [", anchor_label, "]"),
      subtitle = "Composite from block-level changes in spread, consensus, and admissible support.",
      x = NULL,
      y = "Heuristic importance",
      fill = "Delta log-spread"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40"))
}

#' Plot the admissible similarity map
#'
#' @param map_tbl Admissible candidate table with distance and weight columns.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_similarity_map <- function(map_tbl,
                                anchor_label) {
  # Build the point map from the already filtered admissible donor table so the
  # plot function does only plotting work.
  plot_df <- tibble::as_tibble(map_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("d_species", "d_study", "w_hybrid", "overlap_same_species"))) {
    return(graphics_placeholder(
      title = paste0("Admissible Similarity Map [", anchor_label, "]"),
      x = "Species dissimilarity",
      y = "Study dissimilarity"
    ))
  }
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = d_species, y = d_study, size = w_hybrid, colour = overlap_same_species)
  ) +
    ggplot2::geom_point(alpha = 0.75) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "#b2182b", "FALSE" = "#2166ac")) +
    ggplot2::labs(
      title = paste0("Admissible Similarity Map [", anchor_label, "]"),
      subtitle = "Species vs study dissimilarity among admissible candidate models.",
      x = "Species dissimilarity",
      y = "Study dissimilarity",
      size = "Raw kernel weight",
      colour = "Same species"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40"))
}

#' Plot top candidate weights
#'
#' @param top_tbl Ranked top-candidate table.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_top_models <- function(top_tbl,
                            anchor_label) {
  # Expect the caller to supply the already ranked top-candidate table so this
  # function only handles plotting.
  plot_df <- tibble::as_tibble(top_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("candidate_label", "w_adm", "biomass_multiplier_if_replace"))) {
    return(graphics_placeholder(
      title = paste0("Top Candidate Models [", anchor_label, "]"),
      y = "Final weight"
    ))
  }
  plot_df <- plot_df |>
    dplyr::mutate(candidate_label = factor(candidate_label, levels = rev(candidate_label)))

  ggplot2::ggplot(plot_df, ggplot2::aes(x = candidate_label, y = w_adm, fill = biomass_multiplier_if_replace)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_viridis_c(trans = "log10") +
    ggplot2::labs(
      title = paste0("Top Candidate Models [", anchor_label, "]"),
      subtitle = "Bars show final study-cell-adjusted weight; fill is biomass multiplier.",
      x = NULL,
      y = "Final weight",
      fill = "Biomass multiplier"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40"))
}

#' Plot TS conformal ribbons
#'
#' @param band_tbl TS ribbon-band table.
#' @param curve_tbl TS summary curve table.
#' @param anchor_label Anchor label used in the title.
#' @param policy_label Selected policy label used in the subtitle.
#'
#' @return A ggplot object.
#'
#' @export
plot_ts_bands <- function(band_tbl,
                          curve_tbl,
                          anchor_label,
                          policy_label) {
  # Draw the conformal ribbons first, then overlay the anchor, selected, and
  # top-candidate curves from the precomputed summary tables.
  band_df <- tibble::as_tibble(band_tbl)
  curve_df <- tibble::as_tibble(curve_tbl)
  if (nrow(band_df) == 0 || nrow(curve_df) == 0 ||
      !graphics_has_cols(band_df, c("length_cm", "ymin", "ymax", "band")) ||
      !graphics_has_cols(curve_df, c("length_cm", "ts_anchor", "ts_top_candidate", "ts_pred"))) {
    return(graphics_placeholder(
      title = paste0("Length-Specific TS Prediction Uncertainty [", anchor_label, "]"),
      subtitle = paste0("Selected policy: ", policy_label),
      x = "Length (cm)",
      y = "TS (dB re 1 m^2)"
    ))
  }
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = band_df,
      ggplot2::aes(x = length_cm, ymin = ymin, ymax = ymax, fill = band),
      alpha = 0.28
    ) +
    ggplot2::scale_fill_manual(
      values = c("99%" = "#dadaeb", "95%" = "#bcbddc", "90%" = "#9e9ac8", "80%" = "#756bb1"),
      name = "Prediction band"
    ) +
    ggplot2::geom_line(
      data = curve_df,
      ggplot2::aes(x = length_cm, y = ts_anchor, colour = "Anchor", linetype = "Anchor"),
      linewidth = 0.85
    ) +
    ggplot2::geom_line(
      data = curve_df,
      ggplot2::aes(x = length_cm, y = ts_top_candidate, colour = "Top candidate", linetype = "Top candidate"),
      linewidth = 0.8,
      alpha = 0.9
    ) +
    ggplot2::geom_line(
      data = curve_df,
      ggplot2::aes(x = length_cm, y = ts_pred, colour = "Selected policy", linetype = "Selected policy"),
      linewidth = 0.95
    ) +
    ggplot2::scale_colour_manual(
      values = c("Selected policy" = "#54278f", "Anchor" = "#1b1b1b", "Top candidate" = "#238b45"),
      name = "Curve"
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Selected policy" = "solid", "Anchor" = "longdash", "Top candidate" = "dotdash"),
      name = "Curve"
    ) +
    ggplot2::labs(
      title = paste0("Length-Specific TS Prediction Uncertainty [", anchor_label, "]"),
      subtitle = paste0("Selected policy: ", policy_label),
      x = "Length (cm)",
      y = "TS (dB re 1 m^2)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40"))
}

#' Plot the overall slope distribution
#'
#' @param slope_tbl Study-cell slope-summary table.
#'
#' @return A ggplot object.
#'
#' @export
plot_slope_distribution <- function(slope_tbl) {
  # Plot the study-cell slope distribution directly from the prepared summary
  # table so this function is only responsible for rendering.
  plot_df <- tibble::as_tibble(slope_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, "slope_len_cell")) {
    return(graphics_placeholder(
      title = "Study-Cell Distribution of TS-Length Slopes",
      x = "Standardized TS-length slope",
      y = "Study-cell count"
    ))
  }
  ggplot2::ggplot(plot_df, ggplot2::aes(x = slope_len_cell)) +
    ggplot2::geom_histogram(binwidth = 0.5, fill = "#9ecae1", colour = "white") +
    ggplot2::geom_vline(xintercept = 20, colour = "#b2182b", linetype = "dashed", linewidth = 0.9) +
    ggplot2::labs(
      title = "Study-Cell Distribution of TS-Length Slopes",
      x = "Standardized TS-length slope",
      y = "Study-cell count"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot slope distributions by group
#'
#' @param slope_tbl Study-cell slope-summary table.
#'
#' @return A ggplot object.
#'
#' @export
plot_slope_group <- function(slope_tbl) {
  # Drop the catch-all group before plotting so the focal review groups remain
  # visually comparable on one axis.
  plot_df <- tibble::as_tibble(slope_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("review_group", "slope_len_cell"))) {
    return(graphics_placeholder(
      title = "TS-Length Slope by Species Group",
      y = "Standardized TS-length slope"
    ))
  }
  plot_df <- plot_df |>
    dplyr::filter(review_group != "Other")

  ggplot2::ggplot(plot_df, ggplot2::aes(x = review_group, y = slope_len_cell, fill = review_group)) +
    ggplot2::geom_hline(yintercept = 20, colour = "#b2182b", linetype = "dashed", linewidth = 0.9) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.70, width = 0.65) +
    ggplot2::geom_jitter(width = 0.16, height = 0, alpha = 0.45, size = 1.7, colour = "grey25") +
    ggplot2::labs(
      title = "TS-Length Slope by Species Group",
      x = NULL,
      y = "Standardized TS-length slope"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "none")
}

#' Plot slope-support proportions by group
#'
#' @param support_tbl Weighted slope-support table.
#'
#' @return A ggplot object.
#'
#' @export
plot_slope_support <- function(support_tbl) {
  # Apply the paper color mapping to the prepared support table before drawing
  # the stacked group-wise proportions.
  plot_df <- tibble::as_tibble(support_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("review_group", "prop_study_cells", "original_reference_class"))) {
    return(graphics_placeholder(
      title = "Support for 20log10 Dependence by Species Group",
      y = "Proportion of study-cells"
    ))
  }
  fill_vals <- c(
    "< -2" = "#3b4cc0",
    "-2 to -1" = "#7b9ff9",
    "-1 to 0" = "#c0d4f5",
    "exactly 20" = "#f7f7f7",
    "0 to 1" = "#f2cbb7",
    "1 to 2" = "#e37d6d",
    "> 2" = "#b40426",
    "weight-referenced" = "#6a3d9a"
  )

  ggplot2::ggplot(
    plot_df |>
      dplyr::filter(review_group != "Other"),
    ggplot2::aes(x = review_group, y = prop_study_cells, fill = original_reference_class)
  ) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(values = fill_vals, name = "Original form / deviation") +
    ggplot2::labs(
      title = "Support for 20log10 Dependence by Species Group",
      x = NULL,
      y = "Proportion of study-cells"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot NMDS clusters
#'
#' @param points_tbl NMDS point table.
#' @param cluster_col Cluster-label column.
#' @param reference_col Reference-flag column.
#' @param species_col Species-label column.
#' @param common_col Optional common-name column.
#'
#' @return A ggplot object.
#'
#' @export
plot_ordination_clusters <- function(points_tbl,
                                     cluster_col = "policy_cluster_id",
                                     reference_col = "is_reference",
                                     species_col = "species_name",
                                     common_col = "common",
                                     colorbar_name = "NMDS Cluster ID") {
  # Split the point table into the full cloud and the highlighted reference
  # subset before layering them in the ordination.
  plot_df <- tibble::as_tibble(points_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("MDS1", "MDS2"))) {
    return(graphics_placeholder(
      title = NULL,
      x = "NMDS1",
      y = "NMDS2"
    ))
  }

  # Process the clusters and shared column names
  cluster_name <- cluster_col
  if (!(cluster_name %in% names(plot_df))) {
    cluster_candidates <- c("nmds_cluster_id", "nmds_cluster", "species_cluster_id", "policy_cluster_id")
    cluster_name <- cluster_candidates[cluster_candidates %in% names(plot_df)][[1]]
  }
  if (is.null(cluster_name) || length(cluster_name) == 0 || !(cluster_name %in% names(plot_df))) {
    cluster_name <- "ordination_cluster"
    plot_df[[cluster_name]] <- "All models"
  }
  if (!(species_col %in% names(plot_df))) {
    species_col <- "model_id"
    if (!(species_col %in% names(plot_df))) {
      plot_df[[species_col]] <- ""
    }
  }
  if (common_col %in% names(plot_df)) {
    plot_df$anchor_label <- dplyr::coalesce(as.character(plot_df[[common_col]]), as.character(plot_df[[species_col]]))
  } else {
    plot_df$anchor_label <- as.character(plot_df[[species_col]])
  }

  # Process references
  if (reference_col %in% names(plot_df)) {
    ref_flag <- dplyr::coalesce(as.logical(plot_df[[reference_col]]), FALSE)
  } else {
    ref_flag <- rep(FALSE, nrow(plot_df))
  }
  scale_ref <- max(abs(c(plot_df$MDS1, plot_df$MDS2)), na.rm = TRUE)
  if (!is.finite(scale_ref) || scale_ref <= 0) scale_ref <- 1
  cluster_limits <- sort(unique(as.character(plot_df[[cluster_name]])))

  # Create base layer with the grid setup
  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = MDS1,
                                              y = MDS2,
                                              color = .data[[cluster_name]]),
                       data = plot_df) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  # Add model points
  p <- p +
    ggplot2::geom_point(data = plot_df[!ref_flag, , drop = FALSE],
                        alpha = 0.35,
                        size = 2.1)

  # Highlight references
  p <- p +
    ggplot2::geom_point(
      data = plot_df[ref_flag, , drop = FALSE],
      ggplot2::aes(fill = .data[[cluster_name]]),
      shape = 23,
      size = 3.8,
      stroke = 1.2,
      colour = "black"
    ) +
    ggrepel::geom_label_repel(
      data = plot_df[ref_flag, , drop = FALSE],
      ggplot2::aes(
        label = .data[[species_col]],
        fontface = "bold.italic"
      ),
      color = "black",
      max.overlaps = Inf,
      size = 3,
      box.padding = 1,
      point.padding = 1,
      direction = "both",
      min.segment.length = 0,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_brewer(
      palette = "Dark2",
      name = "Cluster",
      limits = cluster_limits,
      labels = function(x) stringr::str_to_title(stringr::str_replace_all(x, "_", " "))
    ) +
    ggplot2::scale_fill_brewer(palette = "Dark2",
                               guide = "none",
                               limits = cluster_limits)

  # Format axis labels
  p +
    ggplot2::labs(x = "NMDS1", y = "NMDS2")
}

#' Plot NMDS variable vectors
#'
#' @param vec_tbl NMDS vector table.
#' @param points_tbl NMDS point table.
#' @param reference_col Reference-flag column.
#' @param species_col Species-label column.
#' @param common_col Optional common-name column.
#'
#' @return A ggplot object.
#'
#' @export
plot_ordination_vectors <- function(vec_tbl,
                                    points_tbl,
                                    reference_col = "is_reference",
                                    species_col = "species_name",
                                    common_col = "common") {
  # Prepare the ordination cloud and the reference labels once so the vector
  # layer can be drawn over the same spatial context.
  vec_df <- tibble::as_tibble(vec_tbl)
  point_df <- tibble::as_tibble(points_tbl)
  if (nrow(vec_df) == 0 || nrow(point_df) == 0 ||
      !graphics_has_cols(vec_df, c("trait", "MDS1", "MDS2")) ||
      !graphics_has_cols(point_df, c("MDS1", "MDS2"))) {
    return(graphics_placeholder(
      title = "Global NMDS Variable Loadings",
      x = "NMDS1",
      y = "NMDS2"
    ))
  }
  if (!(species_col %in% names(point_df))) {
    species_col <- "model_id"
    if (!(species_col %in% names(point_df))) {
      point_df[[species_col]] <- ""
    }
  }
  if (common_col %in% names(point_df)) {
    point_df$anchor_label <- dplyr::coalesce(as.character(point_df[[common_col]]), as.character(point_df[[species_col]]))
  } else {
    point_df$anchor_label <- as.character(point_df[[species_col]])
  }
  if (reference_col %in% names(point_df)) {
    ref_flag <- dplyr::coalesce(as.logical(point_df[[reference_col]]), FALSE)
  } else {
    ref_flag <- rep(FALSE, nrow(point_df))
  }
  scale_ref <- max(abs(c(point_df$MDS1, point_df$MDS2)), na.rm = TRUE)
  if (!is.finite(scale_ref) || scale_ref <= 0) scale_ref <- 1

  ggplot2::ggplot(vec_df |>
                    dplyr::mutate(trait_label = stringr::str_replace_all(trait, "_", " ")),
                  ggplot2::aes(x = 0, y = 0)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey80") +
    ggplot2::geom_point(data = point_df, ggplot2::aes(x = MDS1, y = MDS2), inherit.aes = FALSE, colour = "grey75", alpha = 0.35, size = 1.8) +
    ggplot2::geom_point(
      data = point_df[ref_flag, , drop = FALSE],
      ggplot2::aes(x = MDS1, y = MDS2),
      inherit.aes = FALSE,
      shape = 23,
      size = 4.2,
      stroke = 1,
      fill = "#fdd0a2",
      colour = "black"
    ) +
    ggplot2::geom_text(
      data = point_df[ref_flag, , drop = FALSE],
      ggplot2::aes(x = MDS1, y = MDS2, label = anchor_label),
      inherit.aes = FALSE,
      size = 2.8,
      fontface = "bold",
      nudge_y = 0.03 * scale_ref,
      check_overlap = TRUE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = MDS1, yend = MDS2, linewidth = r2, colour = p_value),
      arrow = grid::arrow(length = grid::unit(0.18, "cm")),
      alpha = 0.9
    ) +
    ggplot2::geom_point(ggplot2::aes(x = MDS1, y = MDS2), size = 2.2, colour = "black") +
    ggrepel::geom_text_repel(
      ggplot2::aes(x = MDS1, y = MDS2, label = trait_label),
      size = 3.2,
      box.padding = 0.25,
      point.padding = 0.2,
      max.overlaps = Inf
    ) +
    ggplot2::scale_linewidth_continuous(range = c(0.5, 1.4), name = expression(R^2)) +
    ggplot2::scale_colour_gradient(low = "#cb181d", high = "#2171b5", trans = "reverse", name = "p-value") +
    ggplot2::labs(
      title = "Global NMDS Variable Loadings",
      x = "NMDS1",
      y = "NMDS2"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot species-level NMDS ordination
#'
#' @param points_tbl Species-level NMDS point table.
#' @param vec_tbl Optional NMDS vector table.
#' @param fac_tbl Optional NMDS factor-centroid table.
#' @param reference_col Reference-flag column.
#' @param species_col Species-label column.
#' @param group_col Grouping/fill column.
#' @param colorbar_name Colorbar title.
#' @param ellipse_level Optional ellipse interval bounded by 0 and 1.
#'
#' @return A ggplot object.
#'
#' @export
plot_species_ordination <- function(points_tbl,
                                    vec_tbl = NULL,
                                    fac_tbl = NULL,
                                    reference_col = "is_reference",
                                    species_col = "species_name",
                                    group_col = "species_cluster_id",
                                    colorbar_name = "Cluster ID & ellipse",
                                    ellipse_level = NULL) {
  # Separate background species from the highlighted reference subset before
  # layering optional significant vectors and factor centroids.
  pts <- tibble::as_tibble(points_tbl)
  if (nrow(pts) == 0 || !graphics_has_cols(pts, c("MDS1", "MDS2"))) {
    return(graphics_placeholder(
      title = NULL,
      x = "NMDS1",
      y = "NMDS2"
    ))
  }
  if (!(species_col %in% names(pts))) {
    species_col <- "model_id"
    if (!(species_col %in% names(pts))) {
      pts[[species_col]] <- ""
    }
  }

  # Infer group column when ambiguous
  infer_group_col <- function(df,
                              requested_col,
                              excluded_cols) {
    if (is.character(requested_col) && length(requested_col) == 1 && requested_col %in% names(df)) {
      return(requested_col)
    }

    max_levels <- max(2L, min(12L, floor(nrow(df) / 2)))
    candidate_cols <- setdiff(names(df), excluded_cols)
    if (length(candidate_cols) == 0) {
      return(NA_character_)
    }

    summarize_col <- function(nm) {
      x <- df[[nm]]
      keep <- !is.na(x)
      x <- x[keep]
      n_non_missing <- length(x)
      if (n_non_missing == 0) {
        return(c(score = -Inf, n_levels = Inf))
      }

      n_levels <- dplyr::n_distinct(x)
      if (n_levels < 2 || n_levels > max_levels) {
        return(c(score = -Inf, n_levels = n_levels))
      }

      is_integerish_numeric <- is.numeric(x) && all(abs(x - round(x)) < 1e-9)
      score <- if (is.factor(x) || is.character(x) || is.logical(x)) {
        3
      } else if (is_integerish_numeric) {
        2
      } else {
        1
      }

      c(score = score, n_levels = n_levels)
    }

    stats_mat <- vapply(candidate_cols, summarize_col, numeric(2))
    valid <- is.finite(stats_mat["score", ])
    if (!any(valid)) {
      return(NA_character_)
    }

    candidate_cols <- candidate_cols[valid]
    stats_mat <- stats_mat[, valid, drop = FALSE]

    best_idx <- order(-stats_mat["score", ], stats_mat["n_levels", ], candidate_cols)[[1]]
    candidate_cols[[best_idx]]
  }

  # Resolve the grouping
  resolved_group_col <- infer_group_col(
    df = pts,
    requested_col = group_col,
    excluded_cols = c("MDS1", "MDS2", reference_col, species_col, "model_id", "model_id_chr")
  )
  if (is.character(resolved_group_col) && nzchar(resolved_group_col)) {
    pts$group_val <- dplyr::coalesce(as.character(pts[[resolved_group_col]]), "unknown")
    group_name <- resolved_group_col
  } else {
    pts$group_val <- "unknown"
    group_name <- group_col
  }
  if (reference_col %in% names(pts)) {
    pts$ref_flag <- dplyr::coalesce(as.logical(pts[[reference_col]]), FALSE)
  } else {
    pts$ref_flag <- FALSE
  }
  pts$label_col <- ifelse(pts$ref_flag, "black", "grey30")
  scale_ref <- max(abs(c(pts$MDS1, pts$MDS2)), na.rm = TRUE)
  if (!is.finite(scale_ref) || scale_ref <= 0) scale_ref <- 1

  # Format points to prepare for plotting
  pts <- pts |>
    dplyr::mutate(.coord_group = paste(round(MDS1, 4), round(MDS2, 4), sep = ":")) |>
    dplyr::group_by(.coord_group) |>
    dplyr::mutate(
      .plot_n = dplyr::n(),
      .plot_i = dplyr::row_number(),
      .plot_angle = 2 * pi * (.plot_i - 1) / pmax(.plot_n, 1),
      .plot_radius = dplyr::if_else(.plot_n > 1, 0.08 * scale_ref * sqrt(.plot_i / .plot_n), 0),
      MDS1_plot = MDS1 + .plot_radius * cos(.plot_angle),
      MDS2_plot = MDS2 + .plot_radius * sin(.plot_angle)
    ) |>
    dplyr::ungroup()
  label_flag <- pts$ref_flag
  if (!any(label_flag, na.rm = TRUE)) {
    label_flag <- rep(TRUE, nrow(pts))
  }

  # Create base layer with the grid setup
  p <- ggplot2::ggplot(mapping = ggplot2::aes(MDS1_plot,
                                              y = MDS2_plot)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  # Optionally add grouped ellipses
  if (is.numeric(ellipse_level) && length(ellipse_level) == 1 &&
      is.finite(ellipse_level) && ellipse_level > 0 && ellipse_level < 1) {
    p <- p +
      ggplot2::stat_ellipse(
        data = pts,
        mapping = ggplot2::aes(x = MDS1,
                               y = MDS2,
                               fill = group_val,
                               color = group_val,
                               group = group_val),
        geom = "polygon",
        inherit.aes = FALSE,
        type = "t",
        level = ellipse_level,
        alpha = 0.12,
        linetype = "dashed",
        linewidth = 0.7
      )
  }


  # Optionally add significant loadings
  if (!is.null(vec_tbl) && nrow(vec_tbl) > 0) {
    sig_vec <- tibble::as_tibble(vec_tbl) |>
      dplyr::filter(is.finite(MDS1), is.finite(MDS2), !is.na(p_value), p_value < 0.05) |>
      dplyr::mutate(xend = MDS1 * scale_ref, yend = MDS2 * scale_ref)

    if (nrow(sig_vec) > 0) {
      p <- p +
        ggplot2::geom_segment(
          data = sig_vec,
          ggplot2::aes(x = 0, y = 0, xend = xend, yend = yend),
          inherit.aes = FALSE,
          arrow = grid::arrow(length = grid::unit(0.15, "cm")),
          colour = "#333333",
          linewidth = 0.55
        ) +
        ggplot2::geom_text(
          data = sig_vec,
          ggplot2::aes(x = xend * 1.08, y = yend * 1.08, label = trait),
          inherit.aes = FALSE,
          size = 2.8,
          colour = "#333333",
          check_overlap = TRUE
        )
    }
  }

  # Optionally add significant centroids
  # Draw significant factor centroids only when the factor table is supplied.
  if (!is.null(fac_tbl) && nrow(fac_tbl) > 0) {
    sig_fac <- tibble::as_tibble(fac_tbl) |>
      dplyr::filter(is.finite(MDS1), is.finite(MDS2), !is.na(p_value), p_value < 0.05) |>
      dplyr::mutate(fac_label = paste0(trait, ": ", level))

    if (nrow(sig_fac) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = sig_fac,
          ggplot2::aes(x = MDS1, y = MDS2),
          inherit.aes = FALSE,
          shape = 4,
          size = 2.8,
          stroke = 1,
          colour = "#7f2704"
        ) +
        ggplot2::geom_text(
          data = sig_fac,
          ggplot2::aes(x = MDS1, y = MDS2, label = fac_label),
          inherit.aes = FALSE,
          size = 2.5,
          colour = "#7f2704",
          nudge_y = 0.02 * scale_ref,
          check_overlap = TRUE
        )
    }
  }

  # Add species points and repelled species labels
  p <- p +
    ggplot2::geom_point(
      data = pts[!pts$ref_flag, , drop = FALSE],
      ggplot2::aes(fill = group_val, shape = "A"),
      size = 3,
      alpha = 0.55
    ) +
    ggplot2::geom_point(
      data = pts[pts$ref_flag, , drop = FALSE],
      ggplot2::aes(fill = group_val, shape = "B"),
      size = 3.5,
      stroke = 1.4,
      colour = "black"
    ) +
    ggplot2::scale_shape_manual(
      values = c("A" = 21, "B" = 23),
      labels = c("A" = "Candidate", "B" = "Reference"),
      name = NULL
    ) +
    ggrepel::geom_label_repel(
      data = pts,
      ggplot2::aes(
        label = .data[[species_col]],
        size = ifelse(ref_flag, 2.9, 2.2),
        fontface = ifelse(ref_flag, "bold.italic", "italic"),
        colour = ifelse(ref_flag, "black", "grey30")
      ),
      max.overlaps = Inf,
      box.padding = 0.4,
      point.padding = 0.4,
      direction = "both",
      min.segment.length = 0,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c("black" = "black", "grey30" = "grey30"),
                                 guide = "none") +
    ggplot2::scale_size_identity() +
    ggplot2::scale_discrete_identity(aesthetic = "fontface") +
    ggplot2::scale_fill_brewer(
      palette = "Set1",
      name = colorbar_name,
      labels = function(x) stringr::str_to_title(stringr::str_replace_all(x, "_", " "))
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(override.aes = list(shape = 21,
                                                       size = 3,
                                                       colour =
                                                         "grey40",
                                                       stroke = 0.4))
    )

  # Final axis labeling and figure
  p + ggplot2::labs(x = "NMDS1", y = "NMDS2")
}

#' Plot NMDS factor centroids
#'
#' @param fac_tbl NMDS factor-centroid table.
#' @param points_tbl NMDS point table.
#' @param reference_col Reference-flag column.
#' @param species_col Species-label column.
#' @param common_col Optional common-name column.
#'
#' @return A ggplot object.
#'
#' @export
plot_ordination_centers <- function(fac_tbl,
                                    points_tbl,
                                    reference_col = "is_reference",
                                    species_col = "species_name",
                                    common_col = "common") {
  # Build the centroid labels once, then overlay them on top of the ordination
  # cloud and highlighted reference points.
  fac_df <- tibble::as_tibble(fac_tbl)
  point_df <- tibble::as_tibble(points_tbl)
  if (nrow(fac_df) == 0 || nrow(point_df) == 0 ||
      !graphics_has_cols(fac_df, c("trait", "level", "MDS1", "MDS2", "n")) ||
      !graphics_has_cols(point_df, c("MDS1", "MDS2"))) {
    return(graphics_placeholder(
      title = "Global NMDS Factor Centroids",
      x = "NMDS1",
      y = "NMDS2"
    ))
  }
  fac_df <- fac_df |>
    dplyr::mutate(
      trait = factor(trait, levels = unique(trait)),
      centroid_label = paste0(stringr::str_replace_all(trait, "_", " "), ": ", level)
    )
  if (!(species_col %in% names(point_df))) {
    species_col <- "model_id"
    if (!(species_col %in% names(point_df))) {
      point_df[[species_col]] <- ""
    }
  }
  if (common_col %in% names(point_df)) {
    point_df$anchor_label <- dplyr::coalesce(as.character(point_df[[common_col]]), as.character(point_df[[species_col]]))
  } else {
    point_df$anchor_label <- as.character(point_df[[species_col]])
  }
  if (reference_col %in% names(point_df)) {
    ref_flag <- dplyr::coalesce(as.logical(point_df[[reference_col]]), FALSE)
  } else {
    ref_flag <- rep(FALSE, nrow(point_df))
  }
  scale_ref <- max(abs(c(point_df$MDS1, point_df$MDS2)), na.rm = TRUE)
  if (!is.finite(scale_ref) || scale_ref <= 0) scale_ref <- 1

  ggplot2::ggplot(fac_df, ggplot2::aes(x = MDS1, y = MDS2, colour = trait)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey80") +
    ggplot2::geom_point(data = point_df, ggplot2::aes(x = MDS1, y = MDS2), inherit.aes = FALSE, colour = "grey75", alpha = 0.35, size = 1.8) +
    ggplot2::geom_point(
      data = point_df[ref_flag, , drop = FALSE],
      ggplot2::aes(x = MDS1, y = MDS2),
      inherit.aes = FALSE,
      shape = 23,
      size = 4.2,
      stroke = 1,
      fill = "#fdd0a2",
      colour = "black"
    ) +
    ggplot2::geom_text(
      data = point_df[ref_flag, , drop = FALSE],
      ggplot2::aes(x = MDS1, y = MDS2, label = anchor_label),
      inherit.aes = FALSE,
      size = 2.8,
      fontface = "bold",
      nudge_y = 0.03 * scale_ref,
      check_overlap = TRUE
    ) +
    ggplot2::geom_point(ggplot2::aes(size = n), alpha = 0.9) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = centroid_label),
      size = 3,
      box.padding = 0.25,
      point.padding = 0.2,
      max.overlaps = Inf,
      show.legend = FALSE
    ) +
    ggplot2::scale_size_continuous(range = c(2.2, 5), name = "n") +
    ggplot2::labs(
      title = "Global NMDS Factor Centroids",
      x = "NMDS1",
      y = "NMDS2",
      colour = "Trait"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot overlap heatmap
#'
#' @param overlap_tbl Overlap-summary table.
#' @param metric_labs Optional named vector mapping metric codes to labels.
#'
#' @return A ggplot object.
#'
#' @export
plot_overlap_heatmap <- function(overlap_tbl,
                                 metric_labs = NULL) {
  # Reshape the overlap summary to a long heatmap table so all overlap metrics
  # can be compared across anchors on one scale.
  overlap_df <- tibble::as_tibble(overlap_tbl)
  if (nrow(overlap_df) == 0 || !"anchor_species" %in% names(overlap_df)) {
    return(graphics_placeholder(
      title = "Applicability Overlap Profile by Reference"
    ))
  }
  if (is.null(metric_labs)) {
    metric_labs <- c(
      w_same_species = "Same species",
      w_same_family = "Same family",
      w_same_swimbladder = "Same swimbladder",
      w_same_fao = "Same FAO area",
      w_same_ocean_basin = "Same ocean basin",
      mean_length_overlap_fraction = "Mean length overlap",
      mean_depth_overlap_fraction = "Mean depth overlap"
    )
  }

  plot_df <- overlap_df |>
    dplyr::select(anchor_species, dplyr::any_of(names(metric_labs))) |>
    tidyr::pivot_longer(cols = -anchor_species, names_to = "metric", values_to = "value") |>
    dplyr::mutate(
      anchor_species = factor(anchor_species, levels = sort(unique(anchor_species))),
      metric = factor(dplyr::recode(metric, !!!metric_labs), levels = unname(metric_labs))
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = metric, y = anchor_species, fill = value)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08306b", na.value = "grey90") +
    ggplot2::labs(
      title = "Applicability Overlap Profile by Reference",
      x = NULL,
      y = NULL,
      fill = "Weighted overlap"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Plot admissibility gate composition
#'
#' @param gate_tbl Admissibility gate-count table.
#' @param gate_labs Optional named vector mapping gate codes to labels.
#' @param gate_cols Optional named vector mapping gate labels to colors.
#'
#' @return A ggplot object.
#'
#' @export
plot_gate_composition <- function(gate_tbl,
                                  gate_labs = NULL,
                                  gate_cols = NULL) {
  # Normalize the gate ordering and labels before drawing the stacked
  # proportions so anchors remain directly comparable.
  plot_df <- tibble::as_tibble(gate_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("anchor_species", "inadmissible_reason", "n_models"))) {
    return(graphics_placeholder(
      title = "Admissibility Gate Composition by Reference",
      y = "Proportion of candidate models"
    ))
  }
  gate_levels <- c(
    "admissible",
    "swimbladder_mismatch",
    "length_domain_nonoverlap",
    "depth_domain_nonoverlap",
    "metadata_missing_excess",
    "self"
  )
  if (is.null(gate_labs)) {
    gate_labs <- c(
      admissible = "Admissible",
      swimbladder_mismatch = "Swimbladder mismatch",
      length_domain_nonoverlap = "Length nonoverlap",
      depth_domain_nonoverlap = "Depth nonoverlap",
      metadata_missing_excess = "Missing metadata",
      self = "Self"
    )
  }
  if (is.null(gate_cols)) {
    gate_cols <- c(
      "Admissible" = "#1b9e77",
      "Swimbladder mismatch" = "#d95f02",
      "Length nonoverlap" = "#7570b3",
      "Depth nonoverlap" = "#66a61e",
      "Missing metadata" = "#e6ab02",
      "Self" = "#666666"
    )
  }

  plot_df <- plot_df |>
    dplyr::mutate(
      anchor_species = factor(anchor_species, levels = unique(anchor_species)),
      inadmissible_reason = factor(inadmissible_reason, levels = gate_levels),
      gate_label = factor(dplyr::recode(as.character(inadmissible_reason), !!!gate_labs), levels = unname(gate_labs[gate_levels]))
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = anchor_species, y = n_models, fill = gate_label)) +
    ggplot2::geom_col(position = "fill", width = 0.78) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(values = gate_cols, drop = FALSE, name = "Gate outcome") +
    ggplot2::labs(
      title = "Admissibility Gate Composition by Reference",
      x = NULL,
      y = "Proportion of candidate models"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Plot admissible multiplier ranges
#'
#' @param range_tbl Anchor-level admissible-range summary table.
#'
#' @return A ggplot object.
#'
#' @export
plot_anchor_ranges <- function(range_tbl) {
  # Draw the admissible-range summary on a log scale so multiplicative changes
  # above and below one are visually comparable.
  plot_df <- tibble::as_tibble(range_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("anchor_species", "q50_multiplier_admissible", "q05_multiplier_admissible", "q95_multiplier_admissible"))) {
    return(graphics_placeholder(
      title = "Admissible Biomass Multiplier Range by Reference",
      y = "Biomass multiplier"
    ))
  }
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = reorder(anchor_species, q50_multiplier_admissible), y = q50_multiplier_admissible)
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = q05_multiplier_admissible, ymax = q95_multiplier_admissible), width = 0.15, colour = "#2171b5") +
    ggplot2::geom_point(size = 2.8, colour = "#b2182b") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = "Admissible Biomass Multiplier Range by Reference",
      x = NULL,
      y = "Biomass multiplier"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot pseudo-anchor policy errors
#'
#' @param perf_tbl Pseudo-anchor benchmark table.
#'
#' @return A ggplot object.
#'
#' @export
plot_policy_boxplot <- function(perf_tbl) {
  # Restrict the boxplot to valid finite predictions before ranking policies
  # by their median error.
  plot_df <- tibble::as_tibble(perf_tbl)
  if (!graphics_has_cols(plot_df, c("valid_prediction", "error_abs_log"))) {
    return(graphics_placeholder(
      title = "Pseudo-Anchor Policy Benchmark",
      x = "Policy",
      y = "|log(multiplier prediction)|"
    ))
  }
  plot_df$policy <- resolve_policy_names(plot_df)
  plot_df <- plot_df |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log))
  if (nrow(plot_df) == 0 || !"policy" %in% names(plot_df)) {
    return(graphics_placeholder(
      title = "Pseudo-Anchor Policy Benchmark",
      x = "Policy",
      y = "|log(multiplier prediction)|"
    ))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = reorder(policy, error_abs_log, FUN = stats::median), y = error_abs_log, fill = policy)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.18, width = 0.72) +
    ggplot2::labs(
      title = "Pseudo-Anchor Policy Benchmark",
      x = "Policy",
      y = "|log(multiplier prediction)|"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Plot species-blocked policy errors
#'
#' @param perf_tbl Species-block benchmark table.
#'
#' @return A ggplot object.
#'
#' @export
plot_species_boxplot <- function(perf_tbl) {
  # Plot the leave-one-species-out benchmark on the same error scale as the
  # pseudo-anchor benchmark for direct comparison.
  plot_df <- tibble::as_tibble(perf_tbl)
  if (!graphics_has_cols(plot_df, c("valid_prediction", "error_abs_log"))) {
    return(graphics_placeholder(
      title = "Species-Blocked Policy Benchmark",
      x = "Policy",
      y = "|log(multiplier prediction)|"
    ))
  }
  plot_df$policy <- resolve_policy_names(plot_df)
  plot_df <- plot_df |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log))
  if (nrow(plot_df) == 0 || !"policy" %in% names(plot_df)) {
    return(graphics_placeholder(
      title = "Species-Blocked Policy Benchmark",
      x = "Policy",
      y = "|log(multiplier prediction)|"
    ))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = reorder(policy, error_abs_log, FUN = stats::median), y = error_abs_log, fill = policy)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.18, width = 0.72) +
    ggplot2::labs(
      title = "Species-Blocked Policy Benchmark",
      x = "Policy",
      y = "|log(multiplier prediction)|"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Plot species-blocked policy heatmap
#'
#' @param perf_tbl Species-block benchmark table.
#' @param policy_labs Optional named vector mapping policy codes to labels.
#'
#' @return A ggplot object.
#'
#' @export
plot_policy_heatmap <- function(perf_tbl,
                                policy_labs = NULL) {
  # Collapse the held-out benchmark to one median error per species-policy
  # pair before drawing the heatmap.
  plot_df <- tibble::as_tibble(perf_tbl)
  if (!graphics_has_cols(plot_df, c("anchor_species", "valid_prediction", "error_abs_log"))) {
    return(graphics_placeholder(
      title = "Species-Blocked Benchmark Performance by Held-Out Species"
    ))
  }
  plot_df$policy <- resolve_policy_names(plot_df)
  plot_df <- plot_df |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log)) |>
    dplyr::group_by(anchor_species, policy) |>
    dplyr::summarise(median_abs_log_error = stats::median(error_abs_log, na.rm = TRUE), .groups = "drop")
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("anchor_species", "policy", "median_abs_log_error"))) {
    return(graphics_placeholder(
      title = "Species-Blocked Benchmark Performance by Held-Out Species"
    ))
  }

  if (!is.null(policy_labs)) {
    plot_df <- plot_df |>
      dplyr::mutate(policy = dplyr::recode(policy, !!!policy_labs))
  }

  policy_levels <- plot_df |>
    dplyr::group_by(policy) |>
    dplyr::summarise(global_median_abs_log = stats::median(median_abs_log_error, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(global_median_abs_log, policy) |>
    dplyr::pull(policy)

  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(
        policy = factor(policy, levels = policy_levels),
        anchor_species = factor(anchor_species, levels = sort(unique(anchor_species)))
      ),
    ggplot2::aes(x = policy, y = anchor_species, fill = median_abs_log_error)
  ) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::scale_fill_viridis_c(option = "C", direction = -1) +
    ggplot2::labs(
      title = "Species-Blocked Benchmark Performance by Held-Out Species",
      x = NULL,
      y = NULL,
      fill = "Median |log error|"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Plot conformal calibration by policy
#'
#' @param cal_tbl Policy-level conformal calibration table.
#'
#' @return A ggplot object.
#'
#' @export
plot_conformal_scores <- function(cal_tbl) {
  # Rank policies by their conformal radius before drawing the calibration
  # bars so the table is readable as a performance spectrum.
  plot_df <- tibble::as_tibble(cal_tbl)
  plot_df$policy <- resolve_policy_names(plot_df)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("policy", "q_abs_log"))) {
    return(graphics_placeholder(
      title = "Conformal Calibration Radius by Policy",
      x = "Policy",
      y = "Conformal q_abs_log"
    ))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = reorder(policy, q_abs_log), y = q_abs_log, fill = policy)
  ) +
    ggplot2::geom_col(alpha = 0.92) +
    ggplot2::labs(
      title = "Conformal Calibration Radius by Policy",
      x = "Policy",
      y = "Conformal q_abs_log"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Plot tuning component importance
#'
#' @param impact_tbl Tuning component-impact table.
#' @param label_map Optional named vector mapping component codes to labels.
#'
#' @return A ggplot object.
#'
#' @export
plot_component_importance <- function(impact_tbl,
                                      label_map = NULL) {
  # Drop the full-model reference row, apply any optional relabeling, and then
  # order the components by delta RMSE before plotting.
  plot_df <- tibble::as_tibble(impact_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("component", "delta_rmse", "delta_mae"))) {
    return(graphics_placeholder(
      title = "Empirical Component Importance for Similarity Weighting",
      y = "Delta RMSE after component dropout"
    ))
  }
  plot_df <- plot_df |>
    dplyr::filter(component != "full_model")
  if (nrow(plot_df) == 0) {
    return(graphics_placeholder(
      title = "Empirical Component Importance for Similarity Weighting",
      y = "Delta RMSE after component dropout"
    ))
  }

  if (!is.null(label_map)) {
    plot_df <- plot_df |>
      dplyr::mutate(component = dplyr::recode(component, !!!label_map))
  }

  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(component = forcats::fct_reorder(component, delta_rmse)),
    ggplot2::aes(x = component, y = delta_rmse, fill = delta_mae)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0) +
    ggplot2::labs(
      title = "Empirical Component Importance for Similarity Weighting",
      subtitle = "Positive delta RMSE means benchmark performance worsens when that component is dropped.",
      x = NULL,
      y = "Delta RMSE after component dropout",
      fill = "Delta MAE"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40"))
}

#' Plot uncertainty heatmap
#'
#' @param dropout_tbl Anchor-level dropout summary table.
#' @param block_labs Optional named vector mapping block codes to labels.
#'
#' @return A ggplot object.
#'
#' @export
plot_uncertainty_heat <- function(dropout_tbl,
                                  block_labs = NULL) {
  # Apply optional block relabeling before drawing the anchor-by-block
  # dropout-importance heatmap.
  plot_df <- tibble::as_tibble(dropout_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("block", "anchor_species", "importance_score"))) {
    return(graphics_placeholder(
      title = "Anchor-Specific Local Dropout Sensitivity"
    ))
  }
  if (!is.null(block_labs)) {
    plot_df <- plot_df |>
      dplyr::mutate(block = dplyr::recode(block, !!!block_labs))
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = block, y = anchor_species, fill = importance_score)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08306b", na.value = "grey90") +
    ggplot2::labs(
      title = "Anchor-Specific Local Dropout Sensitivity",
      x = NULL,
      y = NULL,
      fill = "Heuristic\nimportance"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Plot selected policy intervals
#'
#' @param sel_tbl Selected-policy interval table.
#' @param reference_label Reference label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_selected_intervals <- function(sel_tbl,
                                    reference_label = "Reference") {
  # Normalize the displayed policy label once before drawing the selected
  # interval summary.
  plot_df <- tibble::as_tibble(sel_tbl)
  plot_df$selected_policy_display <- resolve_selected_policy_names(plot_df)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("anchor_species", "multiplier_pred", "multiplier_lo", "multiplier_hi", "selected_policy_display"))) {
    return(graphics_placeholder(
      title = paste0("Selected Policy by ", reference_label),
      y = "Biomass multiplier"
    ))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = reorder(anchor_species, multiplier_pred),
      y = multiplier_pred,
      ymin = multiplier_lo,
      ymax = multiplier_hi,
      colour = selected_policy_display
    )
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_errorbar(width = 0.14) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = paste0("Selected Policy by ", reference_label),
      x = NULL,
      y = "Biomass multiplier",
      colour = "Displayed\nselection"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot integrated anchor summary
#'
#' @param integrated_tbl Integrated anchor-summary table.
#' @param score_tbl Admissible candidate-score table.
#' @param interval_tbl All-policy interval table.
#'
#' @return A ggplot object.
#'
#' @export
plot_anchor_summary <- function(integrated_tbl,
                                score_tbl,
                                interval_tbl) {
  # Align all three layers to the same anchor ordering before drawing the two
  # interval summaries and both point clouds on one log-scale axis.
  integrated_df <- tibble::as_tibble(integrated_tbl)
  if (nrow(integrated_df) == 0 ||
      !graphics_has_cols(integrated_df, c("anchor_species", "multiplier_pred", "multiplier_lo", "multiplier_hi", "hybrid_multiplier_q05", "hybrid_multiplier_q50", "hybrid_multiplier_q95"))) {
    return(graphics_placeholder(
      title = "Integrated Anchor-Level Biomass Multiplier Summary",
      y = "Biomass multiplier"
    ))
  }
  anchor_levels <- integrated_df |>
    dplyr::arrange(multiplier_pred) |>
    dplyr::pull(anchor_species)

  integrated_df <- integrated_df |>
    dplyr::mutate(anchor_species = factor(anchor_species, levels = anchor_levels), x_pos = as.numeric(anchor_species))
  red_df <- tibble::as_tibble(score_tbl)
  if (graphics_has_cols(red_df, c("anchor_species", "admissible", "biomass_multiplier_if_replace"))) {
    red_df <- red_df |>
      dplyr::filter(admissible, is.finite(biomass_multiplier_if_replace), biomass_multiplier_if_replace > 0) |>
      dplyr::mutate(anchor_species = factor(anchor_species, levels = anchor_levels), x_pos = as.numeric(anchor_species) + 0.12)
  } else {
    red_df <- tibble::tibble(x_pos = numeric(), biomass_multiplier_if_replace = numeric())
  }
  blue_df <- tibble::as_tibble(interval_tbl)
  if (graphics_has_cols(blue_df, c("anchor_species", "valid_prediction", "multiplier_pred"))) {
    blue_df <- blue_df |>
      dplyr::filter(valid_prediction, is.finite(multiplier_pred), multiplier_pred > 0) |>
      dplyr::mutate(anchor_species = factor(anchor_species, levels = anchor_levels), x_pos = as.numeric(anchor_species) - 0.12)
  } else {
    blue_df <- tibble::tibble(x_pos = numeric(), multiplier_pred = numeric())
  }

  ggplot2::ggplot(integrated_df, ggplot2::aes(x = x_pos)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_point(
      data = red_df,
      ggplot2::aes(x = x_pos, y = biomass_multiplier_if_replace),
      inherit.aes = FALSE,
      position = ggplot2::position_jitter(width = 0.035, height = 0),
      colour = "#b2182b",
      alpha = 0.18,
      size = 1.2
    ) +
    ggplot2::geom_point(
      data = blue_df,
      ggplot2::aes(x = x_pos, y = multiplier_pred),
      inherit.aes = FALSE,
      position = ggplot2::position_jitter(width = 0.028, height = 0),
      colour = "#2166ac",
      alpha = 0.22,
      size = 1.5
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = multiplier_lo, ymax = multiplier_hi),
      width = 0.12,
      colour = "#2166ac",
      position = ggplot2::position_nudge(x = -0.12)
    ) +
    ggplot2::geom_point(ggplot2::aes(y = multiplier_pred), colour = "#2166ac", size = 2.5, position = ggplot2::position_nudge(x = -0.12)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = hybrid_multiplier_q05, ymax = hybrid_multiplier_q95),
      width = 0.12,
      colour = "#b2182b",
      position = ggplot2::position_nudge(x = 0.12)
    ) +
    ggplot2::geom_point(ggplot2::aes(y = hybrid_multiplier_q50), colour = "#b2182b", size = 2.5, position = ggplot2::position_nudge(x = 0.12)) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_continuous(breaks = integrated_df$x_pos, labels = levels(integrated_df$anchor_species)) +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = "Integrated Anchor-Level Biomass Multiplier Summary",
      x = NULL,
      y = "Biomass multiplier"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot FAO study distribution map
#'
#' @param source_data Model metadata table.
#' @param count_type The type of count to distribute across FAO major regions.
#' This can either be 'studies' for the number of studies, or 'models' for the
#' number of models.
#'
#' @return A ggplot object.
#'
#' @export
plot_area_distribution <- function(model_data,
                                   count_type = "studies") {
  # Define helpers
  swap_xy_nested <- function(x) {
    if (is.matrix(x)) {
      x[, 1:2] <- x[, 2:1, drop = FALSE]
      return(x)
    }
    if (is.list(x)) {
      return(lapply(x, swap_xy_nested))
    }
    x
  }

  swap_xy_sfc_if_needed <- function(sfc_obj) {
    bb <- sf::st_bbox(sfc_obj)
    if (isTRUE(abs(bb[["ymax"]]) > 90 || abs(bb[["ymin"]]) > 90)) {
      swapped <- lapply(seq_along(sfc_obj), function(i) {
        gi <- sfc_obj[[i]]
        out <- swap_xy_nested(gi)
        class(out) <- class(gi)
        out
      })
      return(sf::st_sfc(swapped, crs = sf::st_crs(sfc_obj)))
    }
    sfc_obj
  }

  get_centroid <- function(geom) {
    # Force into flat geographic plane
    raw_wkb <- wk::as_wkb(geom)

    # Clean collection
    clean_geometry <- sf::st_as_sfc(
      raw_wkb,
      crs = sf::st_crs("+proj=longlat +datum=WGS84 +no_defs +over")
    )

    # Transform projections
    centroid_column <- clean_geometry |>
      sf::st_transform(3995) |>
      sf::st_make_valid() |>
      sf::st_centroid() |>
      sf::st_transform(4326)

    return(centroid_column)
  }

  # Validate count method
  if (!(count_type %in% c("studies", "models"))) {
    stop("Argument 'count_type' must either be 'studies' or 'models'.")
  }

  # Load FAO data
  data(fao_areas)

  # Process model data to get counts
  if (count_type == "studies") {
    model_agg <- model_data |>
      dplyr::nest_by(reference_tsl_short, fao_area) |>
      dplyr::group_by(fao_area) |>
      dplyr::reframe(n = length(reference_tsl_short))
  } else {
    model_agg <- model_data |>
      dplyr::group_by(fao_area) |>
      dplyr::reframe(n = length(fao_area))
  }

  # Convert
  fao_df <- sf::st_as_sf(
    fao_areas |>
      dplyr::mutate(geometry = swap_xy_sfc_if_needed(sf::st_as_sfc(the_geom, crs = 4326))),
    sf_column_name = "geometry",
    crs = 4326
  ) |>
    dplyr::filter(F_LEVEL == "MAJOR") |>
    dplyr::transmute(
      fao_area_chr = sub("^0+", "", as.character(F_CODE)),
      area_name = dplyr::coalesce(NAME_EN, F_NAME, F_CODE),
      geometry
    ) |>
    dplyr::left_join(model_agg |>
                       dplyr::mutate(fao_area_chr = as.character(fao_area)),
                     by = "fao_area_chr") |>
    dplyr::mutate(n = dplyr::coalesce(n, 0L)) |>
    sfheaders::sf_to_df(fill = TRUE) |>
    dplyr::group_by(fao_area_chr, sfg_id, multipolygon_id, polygon_id, linestring_id) |>
    dplyr::mutate(sequence_id = dplyr::row_number(),
                  UID = paste0(fao_area_chr, "-", area_name, "-", sfg_id, "-", multipolygon_id, "-", polygon_id)) |>
    dplyr::ungroup() |>
    dplyr::arrange(UID, linestring_id, sequence_id)

  # Get non-empty FAO areas
  nonempty <- fao_df |> dplyr::filter(n > 0) |> dplyr::reframe(fao = unique(fao_area_chr))

  # Get FAO count labels
  fao_labels <- sf::st_as_sf(
    fao_areas |>
      dplyr::mutate(geometry = swap_xy_sfc_if_needed(sf::st_as_sfc(the_geom, crs = 4326))),
    sf_column_name = "geometry",
    crs = 4326
  ) |>
    dplyr::mutate(fao_area = sub("^0+", "", as.character(F_CODE))) |>
    dplyr::filter(fao_area %in% nonempty$fao) |>
    dplyr::mutate(centroid = get_centroid(geometry)) |>
    dplyr::select(fao_area, centroid) |>
    sf::st_set_geometry("centroid") |>
    {\(.) cbind(sf::st_drop_geometry(.), sf::st_coordinates(.))}() |>
    dplyr::rename(longitude = X, latitude = Y) |>
    dplyr::left_join(model_agg |> dplyr::mutate(fao_area = as.character(fao_area)),
                     by = "fao_area")

  # Add polygons
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = fao_df,
      mapping = ggplot2::aes(x = x,
                             y = y,
                             group = UID,
                             subgroup = linestring_id,
                             fill = n),
      color = "black",
      linewidth = 0.5
    ) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(x = longitude, y = latitude, label = n),
      data = fao_labels,
      size = 4.0
    ) +
    scico::scale_fill_scico(palette = "imola",
                            trans = "log2",
                            na.value = "gray70") +
    ggplot2::labs(x = expression(Longitude~(degree)),
                  y = expression(Latitude~(degree)),
                  fill = bquote(italic(n)[.(count_type)])) +
    ggplot2::coord_cartesian(expand = FALSE)
}

#' Plot TS panel ribbons
#'
#' @param curve_tbl Combined per-reference TS ribbon table.
#' @param reference_col Facet-label column.
#'
#' @return A ggplot object.
#'
#' @export
plot_ts_panel <- function(curve_tbl,
                          reference_col = "anchor_species") {
  curve_tbl <- tibble::as_tibble(curve_tbl)

  # Return an empty placeholder plot when no per-reference TS ribbon tables
  # were available, rather than failing during the final summary stage.
  if (nrow(curve_tbl) == 0 || !reference_col %in% names(curve_tbl)) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          title = "Length-Specific TS Prediction Uncertainty",
          subtitle = "No conformal TS ribbon tables were available.",
          x = "Length (cm)",
          y = "TS (dB re 1 m^2)"
        ) +
        ggplot2::theme_minimal(base_size = 11)
    )
  }
  if (!graphics_has_cols(curve_tbl, c("length_cm", "ts_pred", "ts_anchor", "ts_top_candidate",
                                      "ts_lo_99", "ts_hi_99", "ts_lo_95", "ts_hi_95",
                                      "ts_lo_90", "ts_hi_90", "ts_lo_80", "ts_hi_80"))) {
    return(
      graphics_placeholder(
        title = "Length-Specific TS Prediction Uncertainty",
        x = "Length (cm)",
        y = "TS (dB re 1 m^2)"
      )
    )
  }

  # Reshape the stored ribbon bounds into one long band table so the four
  # conformal intervals can be drawn as stacked ribbons.
  band_tbl <- dplyr::bind_rows(
    curve_tbl |>
      dplyr::transmute(!!reference_col := .data[[reference_col]], length_cm, ts_pred, ts_anchor, ts_top_candidate, band = "99%", ymin = ts_lo_99, ymax = ts_hi_99),
    curve_tbl |>
      dplyr::transmute(!!reference_col := .data[[reference_col]], length_cm, ts_pred, ts_anchor, ts_top_candidate, band = "95%", ymin = ts_lo_95, ymax = ts_hi_95),
    curve_tbl |>
      dplyr::transmute(!!reference_col := .data[[reference_col]], length_cm, ts_pred, ts_anchor, ts_top_candidate, band = "90%", ymin = ts_lo_90, ymax = ts_hi_90),
    curve_tbl |>
      dplyr::transmute(!!reference_col := .data[[reference_col]], length_cm, ts_pred, ts_anchor, ts_top_candidate, band = "80%", ymin = ts_lo_80, ymax = ts_hi_80)
  ) |>
    dplyr::mutate(band = factor(band, levels = c("99%", "95%", "90%", "80%")))

    ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = band_tbl, ggplot2::aes(x = length_cm, ymin = ymin, ymax = ymax, fill = band), alpha = 0.28) +
    ggplot2::scale_fill_manual(values = c("99%" = "#dadaeb", "95%" = "#bcbddc", "90%" = "#9e9ac8", "80%" = "#756bb1"), name = "Prediction band") +
    ggplot2::geom_line(data = curve_tbl, ggplot2::aes(x = length_cm, y = ts_anchor, colour = "Anchor", linetype = "Anchor"), linewidth = 0.75) +
    ggplot2::geom_line(data = curve_tbl, ggplot2::aes(x = length_cm, y = ts_top_candidate, colour = "Top candidate", linetype = "Top candidate"), linewidth = 0.7, alpha = 0.9) +
    ggplot2::geom_line(data = curve_tbl, ggplot2::aes(x = length_cm, y = ts_pred, colour = "Selected policy", linetype = "Selected policy"), linewidth = 0.85) +
    ggplot2::scale_colour_manual(values = c("Selected policy" = "#54278f", "Anchor" = "#1b1b1b", "Top candidate" = "#238b45"), name = "Curve") +
    ggplot2::scale_linetype_manual(values = c("Selected policy" = "solid", "Anchor" = "longdash", "Top candidate" = "dotdash"), name = "Curve") +
    ggplot2::facet_wrap(stats::as.formula(paste("~", reference_col)), ncol = 2, scales = "free_x") +
    ggplot2::labs(
      title = "Length-Specific TS Prediction Uncertainty",
      x = "Length (cm)",
      y = "TS (dB re 1 m^2)"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot per-reference policy intervals
#'
#' @param interval_tbl All-policy interval table for one reference.
#' @param reference_name Reference label for the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_all_intervals <- function(interval_tbl,
                               reference_name) {
  # Order the policies by their predicted multiplier before drawing the
  # one-reference interval comparison.
  plot_df <- tibble::as_tibble(interval_tbl)
  if (!"is_selected" %in% names(plot_df)) {
    plot_df$is_selected <- FALSE
  }
  plot_df$policy <- resolve_policy_names(plot_df)
  plot_df$policy_display <- resolve_selected_policy_names(plot_df)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("policy_display", "multiplier_pred", "multiplier_lo", "multiplier_hi", "is_selected"))) {
    return(graphics_placeholder(
      title = paste0(reference_name, ": Conformal Biomass Multipliers Across Policies"),
      y = "Biomass multiplier"
    ))
  }
  plot_df <- plot_df |>
    dplyr::mutate(
      is_selected = dplyr::coalesce(is_selected, FALSE)
    ) |>
    dplyr::arrange(multiplier_pred, policy) |>
    dplyr::mutate(policy_display = factor(policy_display, levels = policy_display))

  selected_label <- plot_df |>
    dplyr::filter(is_selected) |>
    dplyr::distinct(selected_policy_display = policy_display) |>
    dplyr::pull(selected_policy_display)
  if (length(selected_label) == 0) selected_label <- NA_character_

  ggplot2::ggplot(plot_df, ggplot2::aes(x = policy_display, y = multiplier_pred, ymin = multiplier_lo, ymax = multiplier_hi)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_errorbar(ggplot2::aes(colour = is_selected, alpha = is_selected), width = 0.14, linewidth = 0.75) +
    ggplot2::geom_point(ggplot2::aes(colour = is_selected, alpha = is_selected), size = 2.4) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "#2166ac", `FALSE` = "grey65"), guide = "none") +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.45), guide = "none") +
    ggplot2::labs(
      title = paste0(reference_name, ": Conformal Biomass Multipliers Across Policies"),
      subtitle = paste0("Selected policy: ", ifelse(is.na(selected_label), "none", selected_label), "."),
      x = NULL,
      y = "Biomass multiplier"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40"))
}

#' Plot all-reference policy interval panel
#'
#' @param interval_tbl All-policy interval table across references.
#'
#' @return A ggplot object.
#'
#' @export
plot_interval_panel <- function(interval_tbl) {
  # Keep each facet ordered by the within-reference multiplier ranking before
  # drawing the combined panel.
  plot_df <- tibble::as_tibble(interval_tbl)
  if (!"is_selected" %in% names(plot_df)) {
    plot_df$is_selected <- FALSE
  }
  plot_df$policy <- resolve_policy_names(plot_df)
  plot_df$policy_display <- resolve_selected_policy_names(plot_df)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("anchor_species", "policy_display", "multiplier_pred", "multiplier_lo", "multiplier_hi", "is_selected"))) {
    return(graphics_placeholder(
      title = "Conformal Biomass Multipliers Across All Assessed Policies",
      y = "Biomass multiplier"
    ))
  }
  plot_df <- plot_df |>
    dplyr::mutate(
      is_selected = dplyr::coalesce(is_selected, FALSE)
    ) |>
    dplyr::group_by(anchor_species) |>
    dplyr::arrange(multiplier_pred, policy, .by_group = TRUE) |>
    dplyr::mutate(policy_display = factor(policy_display, levels = policy_display)) |>
    dplyr::ungroup()

  ggplot2::ggplot(plot_df, ggplot2::aes(x = policy_display, y = multiplier_pred, ymin = multiplier_lo, ymax = multiplier_hi)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_errorbar(ggplot2::aes(colour = is_selected, alpha = is_selected), width = 0.14, linewidth = 0.65) +
    ggplot2::geom_point(ggplot2::aes(colour = is_selected, alpha = is_selected), size = 2.1) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~anchor_species, ncol = 2, scales = "free_y") +
    ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "#2166ac", `FALSE` = "grey65"), guide = "none") +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.45), guide = "none") +
    ggplot2::labs(
      title = "Conformal Biomass Multipliers Across All Assessed Policies",
      subtitle = "Blue marks the displayed selected policy; grey marks the other assessed policies.",
      x = NULL,
      y = "Biomass multiplier"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40"), strip.text = ggplot2::element_text(face = "bold"))
}

#' Plot policy stability heatmap
#'
#' @param sens_tbl Policy-sensitivity detail table.
#' @param baseline_tbl Baseline scenario table.
#' @param scenario_labs Optional named vector mapping scenario codes to labels.
#'
#' @return A ggplot object.
#'
#' @export
plot_policy_stability <- function(sens_tbl,
                                  baseline_tbl,
                                  scenario_labs = NULL) {
  # Join the baseline multipliers once so the heatmap can show both selection
  # changes and multiplier drift against the same baseline scenario.
  plot_df <- tibble::as_tibble(sens_tbl)
  if (nrow(plot_df) == 0 || !"scenario" %in% names(plot_df) || !"anchor_species" %in% names(plot_df) || !"anchor_model_id" %in% names(plot_df) || !"multiplier_pred" %in% names(plot_df)) {
    return(graphics_placeholder(
      title = "Representative Policy Stability and Multiplier Drift"
    ))
  }
  if (!is.null(scenario_labs)) {
    plot_df <- plot_df |>
      dplyr::mutate(scenario_label = dplyr::recode(scenario, !!!scenario_labs))
  } else {
    plot_df <- plot_df |>
      dplyr::mutate(scenario_label = scenario)
  }

  base_df <- tibble::as_tibble(baseline_tbl) |>
    dplyr::select(anchor_model_id, baseline_multiplier = multiplier_pred)
  if (!"policy_changed" %in% names(plot_df)) plot_df$policy_changed <- FALSE
  if (!"display_changed" %in% names(plot_df)) plot_df$display_changed <- FALSE
  if (!"scenario_status" %in% names(plot_df)) plot_df$scenario_status <- "ok"

  # Accept either the old or the renamed equivalent-set change field so this
  # plot works against both sensitivity-table schemas.
  if (!"equivalent_set_changed" %in% names(plot_df)) {
    plot_df$equivalent_set_changed <- if ("equiv_set_changed" %in% names(plot_df)) {
      plot_df$equiv_set_changed
    } else {
      FALSE
    }
  }
  if (!"equiv_set_changed" %in% names(plot_df)) {
    plot_df$equiv_set_changed <- plot_df$equivalent_set_changed
  }

  plot_df <- plot_df |>
    dplyr::left_join(base_df, by = "anchor_model_id") |>
    dplyr::mutate(
      delta_log_multiplier = log(multiplier_pred / baseline_multiplier),
      label = dplyr::if_else(scenario_status == "ok", sprintf("%+.2f", delta_log_multiplier), "fail"),
      equiv_change = dplyr::coalesce(equivalent_set_changed, equiv_set_changed, FALSE)
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = scenario_label, y = anchor_species)) +
    ggplot2::geom_tile(ggplot2::aes(fill = delta_log_multiplier), colour = "white", linewidth = 0.6) +
    ggplot2::geom_point(
      data = plot_df |>
        dplyr::filter(policy_changed | display_changed | equiv_change | scenario_status != "ok"),
      shape = 21,
      size = 3.8,
      stroke = 1,
      fill = "transparent",
      colour = "black"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, labels = scales::number_format(accuracy = 0.01)) +
    ggplot2::labs(
      title = "Representative Policy Stability and Multiplier Drift",
      x = NULL,
      y = NULL,
      fill = "Delta log\nmultiplier"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}

#' Plot multiplier-drift heatmap
#'
#' @param sens_tbl Strategy-sensitivity detail table.
#' @param baseline_tbl Baseline scenario table.
#' @param scenario_labs Optional named vector mapping scenario codes to labels.
#'
#' @return A ggplot object.
#'
#' @export
plot_multiplier_drift <- function(sens_tbl,
                                  baseline_tbl,
                                  scenario_labs = NULL) {
  # Join baseline multipliers once and plot only the multiplier drift, without
  # the selection-change overlays used in the stability plot.
  plot_df <- tibble::as_tibble(sens_tbl)
  if (nrow(plot_df) == 0 || !"scenario" %in% names(plot_df) || !"anchor_species" %in% names(plot_df) || !"anchor_model_id" %in% names(plot_df) || !"multiplier_pred" %in% names(plot_df)) {
    return(graphics_placeholder(
      title = "Multiplier Sensitivity Relative to Baseline"
    ))
  }
  if (!is.null(scenario_labs)) {
    plot_df <- plot_df |>
      dplyr::mutate(scenario_label = dplyr::recode(scenario, !!!scenario_labs))
  } else {
    plot_df <- plot_df |>
      dplyr::mutate(scenario_label = scenario)
  }

  base_df <- tibble::as_tibble(baseline_tbl) |>
    dplyr::select(anchor_model_id, baseline_multiplier = multiplier_pred)
  plot_df <- plot_df |>
    dplyr::left_join(base_df, by = "anchor_model_id") |>
    dplyr::mutate(
      delta_log_multiplier = log(multiplier_pred / baseline_multiplier),
      label = sprintf("%+.2f", delta_log_multiplier)
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = scenario_label, y = anchor_species, fill = delta_log_multiplier)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, labels = scales::number_format(accuracy = 0.01)) +
    ggplot2::labs(
      title = "Multiplier Sensitivity Relative to Baseline",
      x = NULL,
      y = NULL,
      fill = "Delta log\nmultiplier"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}

#' Plot sensitivity summary
#'
#' @param plot_tbl Long sensitivity-summary table.
#'
#' @return A ggplot object.
#'
#' @export
plot_sensitivity_overview <- function(plot_tbl) {
  # Plot the already prepared long-form scenario summary so the function only
  # handles the segment-plus-point rendering.
  plot_df <- tibble::as_tibble(plot_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("value", "scenario_label", "metric", "panel"))) {
    return(graphics_placeholder(
      title = "Sensitivity Scenario Summary"
    ))
  }
  ggplot2::ggplot(plot_df, ggplot2::aes(x = value, y = forcats::fct_rev(scenario_label), colour = metric)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = value, yend = forcats::fct_rev(scenario_label)), linewidth = 0.9, alpha = 0.75) +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_x") +
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
    ggplot2::scale_color_manual(values = c(
      "Policy changed" = "#d95f02",
      "Display changed" = "#1b9e77",
      "Equivalent set changed" = "#7570b3",
      "Median abs delta log multiplier" = "#2b8cbe",
      "Max abs delta log multiplier" = "#de2d26"
    )) +
    ggplot2::labs(
      title = "Sensitivity Scenario Summary",
      x = NULL,
      y = NULL,
      colour = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), legend.position = "top")
}

#' Plot tuning resample variability
#'
#' @param plot_tbl Tuning-resample block-summary table.
#' @param block_col Block-name column.
#'
#' @return A ggplot object.
#'
#' @export
plot_tuning_variation <- function(plot_tbl,
                                  block_col = "block") {
  # Reorder the blocks by their mean multiplier before drawing the resample
  # variability intervals and point estimates.
  plot_df <- tibble::as_tibble(plot_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c(block_col, "mean_multiplier", "q05_multiplier", "q95_multiplier", "sd_multiplier"))) {
    return(graphics_placeholder(
      title = "Tuning Block Multipliers Across Resamples",
      x = "Block multiplier"
    ))
  }
  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(.block = forcats::fct_reorder(.data[[block_col]], mean_multiplier)),
    ggplot2::aes(x = mean_multiplier, y = .block)
  ) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = q05_multiplier, xmax = q95_multiplier), linewidth = 1.1, colour = "#6baed6") +
    ggplot2::geom_point(size = 3, colour = "#08519c") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("sd=%.2f", sd_multiplier)), nudge_y = 0.22, size = 3, colour = "#444444") +
    ggplot2::labs(
      title = "Tuning Block Multipliers Across Resamples",
      x = "Block multiplier",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Plot anchor policy audit
#'
#' @param audit_tbl Anchor support-audit table.
#'
#' @return A ggplot object.
#'
#' @export
plot_anchor_audit <- function(audit_tbl) {
  audit_tbl <- tibble::as_tibble(audit_tbl)
  if (nrow(audit_tbl) == 0) {
    return(
      graphics_placeholder(
        title = "Representative Policy Audit"
      )
    )
  }
  metric_labs <- c(
    empirical_coverage = "Species-block empirical coverage",
    interval_log_width = "Representative interval log width",
    local_effective_support = "Local effective support",
    local_mean_combined_distance = "Local mean combined distance",
    median_abs_delta_log_multiplier = "Median abs sensitivity drift"
  )
  keep_cols <- c("anchor_species", "selected_policy_display", intersect(names(metric_labs), names(audit_tbl)))

  # Return an empty placeholder plot when the audit table does not yet contain
  # any metric columns that can be faceted.
  audit_tbl$selected_policy_display <- resolve_selected_policy_names(audit_tbl)
  if (!all(c("anchor_species", "selected_policy_display") %in% names(audit_tbl)) ||
      length(setdiff(keep_cols, c("anchor_species", "selected_policy_display"))) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          title = "Representative Policy Audit",
          subtitle = "No audit metrics were available for plotting.",
          x = NULL,
          y = NULL
        ) +
        ggplot2::theme_minimal(base_size = 11)
    )
  }

  # Reshape the audit metrics to one long plotting table so each audit measure
  # can be shown on its own x-scale.
  plot_df <- audit_tbl |>
    dplyr::mutate(anchor_species = forcats::fct_inorder(anchor_species)) |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    tidyr::pivot_longer(cols = -c(anchor_species, selected_policy_display), names_to = "metric", values_to = "value") |>
    dplyr::mutate(
      metric = factor(
        dplyr::recode(metric, !!!metric_labs),
        levels = unname(metric_labs[names(metric_labs) %in% names(audit_tbl)])
      )
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = value, y = forcats::fct_rev(anchor_species), colour = selected_policy_display)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_wrap(~metric, scales = "free_x", ncol = 2) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
    ggplot2::labs(
      title = "Representative Policy Audit",
      x = NULL,
      y = NULL,
      colour = "Displayed policy"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), legend.position = "top")
}

#' Plot field-level missingness
#'
#' @param field_tbl Field-level missingness table.
#'
#' @return A ggplot object.
#'
#' @export
plot_field_missing <- function(field_tbl) {
  # Reorder the fields by missing fraction before drawing the one-dimensional
  # missingness audit bar chart.
  plot_df <- tibble::as_tibble(field_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("field", "missing_fraction"))) {
    return(graphics_placeholder(
      title = "Missingness Across Key Workflow Metadata Fields",
      x = "Missing fraction"
    ))
  }
  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(field = forcats::fct_reorder(field, missing_fraction)),
    ggplot2::aes(x = missing_fraction, y = field)
  ) +
    ggplot2::geom_col(fill = "#756bb1", width = 0.7) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "Missingness Across Key Workflow Metadata Fields",
      x = "Missing fraction",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot anchor-level missingness exclusions
#'
#' @param anchor_tbl Anchor-level missingness-gate summary table.
#'
#' @return A ggplot object.
#'
#' @export
plot_anchor_missing <- function(anchor_tbl) {
  # Order the anchor labels once and draw the fraction excluded by the
  # missingness gate for each reference species.
  plot_df <- tibble::as_tibble(anchor_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("anchor_species", "prop_fail_missing_metadata"))) {
    return(graphics_placeholder(
      title = "Candidate Exclusion from Missing Key Metadata",
      x = "Excluded by missingness gate"
    ))
  }
  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(anchor_species = forcats::fct_inorder(anchor_species)),
    ggplot2::aes(x = prop_fail_missing_metadata, y = forcats::fct_rev(anchor_species))
  ) +
    ggplot2::geom_col(fill = "#1c9099", width = 0.7) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "Candidate Exclusion from Missing Key Metadata",
      x = "Excluded by missingness gate",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot ordination clusters with hulls
#'
#' @param points_tbl Ordination point table.
#' @param hull_tbl Cluster-hull table.
#' @param cluster_col Cluster-label column.
#' @param reference_col Reference-flag column.
#' @param label_col Point-label column.
#' @param title Plot title.
#'
#' @return A ggplot object.
#'
#' @export
plot_ordination_cluster_hulls <- function(points_tbl,
                                          hull_tbl,
                                          cluster_col = "policy_cluster_id",
                                          reference_col = "is_reference",
                                          label_col = "species_name",
                                          title = "NMDS Clusters with Hulls") {
  # Normalize the point and hull tables once before layering the hull polygons,
  # ordination cloud, and highlighted reference labels.
  point_df <- tibble::as_tibble(points_tbl)
  hull_df <- tibble::as_tibble(hull_tbl)
  if (nrow(point_df) == 0 || !graphics_has_cols(point_df, c("MDS1", "MDS2"))) {
    return(graphics_placeholder(
      title = title,
      x = "NMDS1",
      y = "NMDS2"
    ))
  }
  cluster_name <- cluster_col
  if (!(cluster_name %in% names(point_df))) {
    cluster_candidates <- c("nmds_cluster_id", "nmds_cluster", "species_cluster_id", "policy_cluster_id")
    cluster_name <- cluster_candidates[cluster_candidates %in% names(point_df)][[1]]
  }
  if (is.null(cluster_name) || length(cluster_name) == 0 || !(cluster_name %in% names(point_df))) {
    cluster_name <- "ordination_cluster"
    point_df[[cluster_name]] <- "All models"
  }
  if (!(cluster_name %in% names(hull_df))) {
    hull_df[[cluster_name]] <- point_df[[cluster_name]][[1]]
  }
  if (reference_col %in% names(point_df)) {
    ref_flag <- dplyr::coalesce(as.logical(point_df[[reference_col]]), FALSE)
  } else {
    ref_flag <- rep(FALSE, nrow(point_df))
  }
  scale_ref <- max(abs(c(point_df$MDS1, point_df$MDS2)), na.rm = TRUE)
  if (!is.finite(scale_ref) || scale_ref <= 0) {
    scale_ref <- 1
  }
  if (!(label_col %in% names(point_df))) {
    label_col <- "species_name"
  }

  ggplot2::ggplot(point_df, ggplot2::aes(x = MDS1, y = MDS2, colour = .data[[cluster_name]])) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_polygon(
      data = hull_df,
      ggplot2::aes(fill = .data[[cluster_name]], group = .data[[cluster_name]]),
      inherit.aes = FALSE,
      alpha = 0.12,
      colour = NA
    ) +
    ggplot2::geom_path(
      data = hull_df,
      ggplot2::aes(group = .data[[cluster_name]]),
      inherit.aes = FALSE,
      linewidth = 0.8,
      alpha = 0.7
    ) +
    ggplot2::geom_point(data = point_df[!ref_flag, , drop = FALSE], alpha = 0.40, size = 2.1) +
    ggplot2::geom_point(
      data = point_df[ref_flag, , drop = FALSE],
      ggplot2::aes(fill = .data[[cluster_name]]),
      shape = 23,
      size = 4.8,
      stroke = 1.2,
      colour = "black"
    ) +
    ggplot2::geom_text(
      data = point_df[ref_flag, , drop = FALSE],
      ggplot2::aes(x = MDS1, y = MDS2, label = .data[[label_col]]),
      inherit.aes = FALSE,
      size = 3,
      fontface = "bold",
      nudge_y = 0.03 * scale_ref,
      check_overlap = TRUE
    ) +
    ggplot2::scale_colour_brewer(palette = "Dark2", name = "Cluster") +
    ggplot2::scale_fill_brewer(palette = "Dark2", name = "Cluster") +
    ggplot2::labs(title = title, x = "NMDS1", y = "NMDS2") +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot a length-density curve
#'
#' @param length_tbl Length-density table with `length_cm` and `f_len`.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_length_density <- function(length_tbl,
                                anchor_label) {
  # Draw the supplied length-density support directly so the function remains a
  # pure plotting helper.
  plot_df <- tibble::as_tibble(length_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("length_cm", "f_len"))) {
    return(graphics_placeholder(
      title = paste0("Length PDF [", anchor_label, "]"),
      x = "Length (cm)",
      y = "f(L)"
    ))
  }
  ggplot2::ggplot(plot_df, ggplot2::aes(x = length_cm, y = f_len)) +
    ggplot2::geom_line(linewidth = 0.8, colour = "#3182bd") +
    ggplot2::labs(
      title = paste0("Length PDF [", anchor_label, "]"),
      x = "Length (cm)",
      y = "f(L)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot a weighted TS ribbon
#'
#' @param ribbon_tbl Weighted TS summary table.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_ts_ribbon <- function(ribbon_tbl,
                           anchor_label) {
  # Use explicit ribbon bounds when they are present, otherwise fall back to
  # the mean plus-or-minus one supplied standard deviation.
  plot_df <- tibble::as_tibble(ribbon_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("length_cm", "ts_mean"))) {
    return(graphics_placeholder(
      title = paste0("Weighted TS Ribbon [reference: ", anchor_label, "]"),
      x = "Length (cm)",
      y = "TS (dB re 1 m^2)"
    ))
  }
  plot_df <- plot_df |>
    dplyr::mutate(
      ribbon_low = dplyr::coalesce(ts_lo, ts_mean - ts_sd),
      ribbon_high = dplyr::coalesce(ts_hi, ts_mean + ts_sd)
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = length_cm, y = ts_mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ribbon_low, ymax = ribbon_high), alpha = 0.2, fill = "#3182bd") +
    ggplot2::geom_line(linewidth = 0.9, colour = "#3182bd") +
    ggplot2::labs(
      title = paste0("Weighted TS Ribbon [reference: ", anchor_label, "]"),
      subtitle = "Smooth ribbon from the weighted slope/intercept distribution across admissible models.",
      x = "Length (cm)",
      y = "TS (dB re 1 m^2)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40"))
}

#' Plot model weights against distance
#'
#' @param weight_tbl Candidate-weight table.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_model_weights <- function(weight_tbl,
                               anchor_label) {
  # Prefer the final admissible weight when it exists; otherwise fall back to
  # the raw hybrid kernel weight.
  plot_df <- tibble::as_tibble(weight_tbl)
  if (!("w_adm" %in% names(plot_df) || "w_hybrid" %in% names(plot_df)) ||
      !("combined_distance" %in% names(plot_df) || "d_species" %in% names(plot_df))) {
    return(graphics_placeholder(
      title = paste0("Model Weights vs Distance [", anchor_label, "]"),
      x = "Distance to reference",
      y = "Model weight"
    ))
  }
  if (!"w_adm" %in% names(plot_df)) plot_df$w_adm <- NA_real_
  if (!"w_hybrid" %in% names(plot_df)) plot_df$w_hybrid <- NA_real_
  if (!"combined_distance" %in% names(plot_df)) plot_df$combined_distance <- NA_real_
  if (!"d_species" %in% names(plot_df)) plot_df$d_species <- NA_real_
  plot_df <- plot_df |>
    dplyr::mutate(
      plot_weight = dplyr::coalesce(w_adm, w_hybrid),
      plot_distance = dplyr::coalesce(combined_distance, d_species)
    ) |>
    dplyr::filter(is.finite(plot_weight), is.finite(plot_distance), plot_weight >= 0)
  if (nrow(plot_df) == 0) {
    return(graphics_placeholder(
      title = paste0("Model Weights vs Distance [", anchor_label, "]"),
      x = "Distance to reference",
      y = "Model weight"
    ))
  }
  if ("swimbladder_type" %in% names(plot_df)) {
    plot_df$group_val <- dplyr::coalesce(as.character(plot_df$swimbladder_type), "unknown")
  } else {
    plot_df$group_val <- "unknown"
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = plot_distance, y = plot_weight)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_point(
      ggplot2::aes(colour = group_val),
      size = 2.5,
      alpha = 0.8
    ) +
    ggplot2::scale_colour_brewer(palette = "Set1", name = "Swimbladder type") +
    ggplot2::labs(
      title = paste0("Model Weights vs Distance [", anchor_label, "]"),
      x = "Distance to reference",
      y = "Model weight"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot biomass-sensitivity distribution
#'
#' @param sensitivity_tbl Candidate-biomass table.
#' @param anchor_label Anchor label used in the title.
#' @param summary_tbl Optional one-row biomass-summary table.
#'
#' @return A ggplot object.
#'
#' @export
plot_biomass_sensitivity <- function(sensitivity_tbl,
                                     anchor_label,
                                     summary_tbl = NULL) {
  # Reduce the candidate table to finite positive biomass multipliers before
  # drawing the weighted histogram on the log scale.
  plot_df <- tibble::as_tibble(sensitivity_tbl)
  if (!"biomass_multiplier_if_replace" %in% names(plot_df)) {
    return(graphics_placeholder(
      title = paste0("Biomass Sensitivity [reference: ", anchor_label, "]"),
      x = "Biomass multiplier relative to the reference model",
      y = "Weighted model count"
    ))
  }
  if (!"w_adm" %in% names(plot_df)) plot_df$w_adm <- NA_real_
  if (!"w_hybrid" %in% names(plot_df)) plot_df$w_hybrid <- NA_real_
  plot_df <- plot_df |>
    dplyr::mutate(plot_weight = dplyr::coalesce(w_adm, w_hybrid, 0)) |>
    dplyr::filter(
      is.finite(biomass_multiplier_if_replace),
      biomass_multiplier_if_replace > 0,
      is.finite(plot_weight),
      plot_weight > 0
    )
  if (nrow(plot_df) == 0) {
    return(graphics_placeholder(
      title = paste0("Biomass Sensitivity [reference: ", anchor_label, "]"),
      x = "Biomass multiplier relative to the reference model",
      y = "Weighted model count"
    ))
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = biomass_multiplier_if_replace, weight = plot_weight)) +
    ggplot2::geom_histogram(bins = 30, fill = "#9ecae1", colour = "white", alpha = 0.95) +
    ggplot2::scale_x_log10() +
    ggplot2::geom_vline(xintercept = 1, colour = "#4d4d4d", linetype = "dashed", linewidth = 0.7) +
    ggplot2::labs(
      title = paste0("Biomass Sensitivity [reference: ", anchor_label, "]"),
      x = "Biomass multiplier relative to the reference model",
      y = "Weighted model count"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  # Overlay the consensus and interval markers when the caller supplies the
  # already summarized biomass interval table.
  if (!is.null(summary_tbl) && nrow(summary_tbl) > 0) {
    p <- p +
      ggplot2::geom_vline(xintercept = summary_tbl$hybrid_consensus_multiplier[[1]], colour = "#b2182b", linewidth = 1.2) +
      ggplot2::geom_vline(
        xintercept = c(summary_tbl$hybrid_multiplier_q05[[1]], summary_tbl$hybrid_multiplier_q95[[1]]),
        colour = "#2166ac",
        linetype = "dotted",
        linewidth = 1.0
      )
  }

  p
}

#' Plot candidate biomass response
#'
#' @param candidate_tbl Candidate-biomass table.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_biomass_candidate_map <- function(candidate_tbl,
                                       anchor_label) {
  # Build the scatter map from the already scored candidate table so the plot
  # reflects the final admissible weight and biomass multiplier per model.
  plot_df <- tibble::as_tibble(candidate_tbl)
  if (!"biomass_multiplier_if_replace" %in% names(plot_df) ||
      !("w_adm" %in% names(plot_df) || "w_hybrid" %in% names(plot_df))) {
    return(graphics_placeholder(
      title = paste0("Candidate Model Biomass Response [reference: ", anchor_label, "]"),
      x = "Model weight",
      y = "Biomass multiplier"
    ))
  }
  if (!"w_adm" %in% names(plot_df)) plot_df$w_adm <- NA_real_
  if (!"w_hybrid" %in% names(plot_df)) plot_df$w_hybrid <- NA_real_
  plot_df <- plot_df |>
    dplyr::mutate(plot_weight = dplyr::coalesce(w_adm, w_hybrid)) |>
    dplyr::filter(
      is.finite(plot_weight),
      plot_weight > 0,
      is.finite(biomass_multiplier_if_replace),
      biomass_multiplier_if_replace > 0
    )
  if (nrow(plot_df) == 0) {
    return(graphics_placeholder(
      title = paste0("Candidate Model Biomass Response [reference: ", anchor_label, "]"),
      x = "Model weight",
      y = "Biomass multiplier"
    ))
  }
  if ("common" %in% names(plot_df)) {
    plot_df$label <- dplyr::coalesce(as.character(plot_df$common), as.character(plot_df$species_name), as.character(plot_df$model_id_chr))
  } else {
    plot_df$label <- dplyr::coalesce(as.character(plot_df$species_name), as.character(plot_df$model_id_chr))
  }
  if ("swimbladder_type" %in% names(plot_df)) {
    plot_df$group_val <- dplyr::coalesce(as.character(plot_df$swimbladder_type), "unknown")
  } else {
    plot_df$group_val <- "unknown"
  }

  label_df <- plot_df |>
    dplyr::arrange(dplyr::desc(plot_weight)) |>
    dplyr::slice_head(n = 10)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = plot_weight, y = biomass_multiplier_if_replace)) +
    ggplot2::scale_y_log10(
      breaks = scales::breaks_log(n = 6),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::geom_hline(yintercept = 1, colour = "#4d4d4d", linetype = "dashed", linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(colour = group_val), alpha = 0.55, size = 2.2) +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(label = label),
      size = 2.8,
      nudge_x = 0.005 * max(plot_df$plot_weight, na.rm = TRUE),
      check_overlap = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::labs(
      title = paste0("Candidate Model Biomass Response [reference: ", anchor_label, "]"),
      subtitle = "X = model weight, Y = biomass multiplier on a log scale.",
      x = "Model weight",
      y = "Biomass multiplier",
      colour = "Swimbladder type"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40"))
}

#' Plot top-ten model weights
#'
#' @param top_tbl Ranked top-candidate table.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_top_ten_model_weights <- function(top_tbl,
                                       anchor_label) {
  # Build the ordered top-ten label set before drawing the ranking bar chart.
  plot_df <- tibble::as_tibble(top_tbl)
  if (nrow(plot_df) == 0 || !graphics_has_cols(plot_df, c("species_name", "model_id_chr", "w_adm"))) {
    return(graphics_placeholder(
      title = paste0("Top-10 Models by Weight [", anchor_label, "]"),
      y = "Model weight"
    ))
  }
  if ("common" %in% names(plot_df)) {
    common_suffix <- dplyr::if_else(!is.na(plot_df$common) & nzchar(plot_df$common), paste0(" [", plot_df$common, "]"), "")
  } else {
    common_suffix <- rep("", nrow(plot_df))
  }
  plot_df <- plot_df |>
    dplyr::mutate(
      label = paste0(species_name, common_suffix, " {m", model_id_chr, "}"),
      label = factor(label, levels = label)
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = label, y = w_adm)) +
    ggplot2::geom_col(fill = "#3182bd", alpha = 0.85) +
    ggplot2::labs(
      title = paste0("Top-10 Models by Weight [", anchor_label, "]"),
      x = NULL,
      y = "Model weight"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8, angle = 60, hjust = 1))
}

#' Plot pivot variance
#'
#' @param profile_tbl Pivot-variance profile table.
#' @param summary_tbl One-row pivot summary table.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_pivot_variance <- function(profile_tbl,
                                summary_tbl,
                                anchor_label) {
  # Resolve the pivot-display choice once so the line and optional pairwise
  # pivot band are plotted consistently.
  profile_df <- tibble::as_tibble(profile_tbl)
  summary_df <- tibble::as_tibble(summary_tbl)
  if (nrow(profile_df) == 0 || nrow(summary_df) == 0 || !graphics_has_cols(profile_df, c("length_cm", "weighted_variance_v"))) {
    return(graphics_placeholder(
      title = paste0("Pivot-Point Variance Profile [reference: ", anchor_label, "]"),
      x = "Length (cm)",
      y = "Weighted variance V(L)"
    ))
  }
  boundary_case <- identical(summary_df$pivot_display_source[[1]], "pairwise_weighted_median")
  plot_pivot <- if (boundary_case) summary_df$pivot_length_cm[[1]] else summary_df$pivot_display_length_cm[[1]]

  ggplot2::ggplot(profile_df, ggplot2::aes(x = length_cm, y = weighted_variance_v)) +
    {
      if (is.finite(summary_df$pairwise_pivot_q25_cm[[1]]) && is.finite(summary_df$pairwise_pivot_q75_cm[[1]])) {
        ggplot2::annotate(
          "rect",
          xmin = summary_df$pairwise_pivot_q25_cm[[1]],
          xmax = summary_df$pairwise_pivot_q75_cm[[1]],
          ymin = -Inf,
          ymax = Inf,
          fill = "#fddbc7",
          alpha = 0.18
        )
      }
    } +
    ggplot2::geom_line(linewidth = 0.9, colour = "#2166ac") +
    {
      if (is.finite(plot_pivot)) {
        ggplot2::geom_vline(
          xintercept = plot_pivot,
          colour = "#b2182b",
          linetype = if (boundary_case) "dashed" else "solid",
          linewidth = 1.0
        )
      }
    } +
    ggplot2::labs(
      title = paste0("Pivot-Point Variance Profile [reference: ", anchor_label, "]"),
      x = "Length (cm)",
      y = "Weighted variance V(L)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot pairwise pivot distribution
#'
#' @param pairwise_tbl Pairwise pivot table.
#' @param summary_tbl One-row pivot summary table.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_pairwise_pivot_histogram <- function(pairwise_tbl,
                                          summary_tbl,
                                          anchor_label) {
  # Use the display pivot from the summary table so the histogram aligns with
  # the pivot-variance plot.
  pairwise_df <- tibble::as_tibble(pairwise_tbl)
  summary_df <- tibble::as_tibble(summary_tbl)
  if (nrow(pairwise_df) == 0 || nrow(summary_df) == 0 || !graphics_has_cols(pairwise_df, c("lpivot_cm", "pair_weight"))) {
    return(graphics_placeholder(
      title = paste0("Pairwise Pivot-Length Distribution [reference: ", anchor_label, "]"),
      x = "Pairwise pivot length (cm)",
      y = "Weighted pair count"
    ))
  }
  plot_pivot <- summary_df$pivot_display_length_cm[[1]]
  boundary_case <- identical(summary_df$pivot_display_source[[1]], "pairwise_weighted_median")

  ggplot2::ggplot(pairwise_df, ggplot2::aes(x = lpivot_cm, weight = pair_weight)) +
    ggplot2::geom_histogram(bins = 30, fill = "#9ecae1", colour = "white") +
    {
      if (is.finite(plot_pivot)) {
        ggplot2::geom_vline(
          xintercept = plot_pivot,
          colour = "#b2182b",
          linetype = if (boundary_case) "dashed" else "solid",
          linewidth = 1.0
        )
      }
    } +
    ggplot2::labs(
      title = paste0("Pairwise Pivot-Length Distribution [reference: ", anchor_label, "]"),
      x = "Pairwise pivot length (cm)",
      y = "Weighted pair count"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot biological leverage
#'
#' @param profile_tbl Biological-leverage profile table.
#' @param summary_tbl One-row biological-leverage summary table.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @export
plot_biological_leverage <- function(profile_tbl,
                                     summary_tbl,
                                     anchor_label) {
  # Overlay the leverage peak and pivot diagnostics on the leverage profile so
  # the biologically influential size classes can be read directly from the
  # final figure.
  profile_df <- tibble::as_tibble(profile_tbl)
  summary_df <- tibble::as_tibble(summary_tbl)
  if (nrow(profile_df) == 0 || nrow(summary_df) == 0 || !graphics_has_cols(profile_df, c("length_cm", "lambda_l"))) {
    return(graphics_placeholder(
      title = paste0("Biological Leverage Profile [reference: ", anchor_label, "]"),
      x = "Length (cm)",
      y = "Biological leverage"
    ))
  }
  boundary_case <- identical(summary_df$pivot_display_source[[1]], "pairwise_weighted_median")
  plot_pivot <- if (boundary_case) summary_df$pivot_length_cm[[1]] else summary_df$pivot_display_length_cm[[1]]

  ggplot2::ggplot(profile_df, ggplot2::aes(x = length_cm, y = lambda_l)) +
    {
      if (is.finite(summary_df$pairwise_pivot_q25_cm[[1]]) && is.finite(summary_df$pairwise_pivot_q75_cm[[1]])) {
        ggplot2::annotate(
          "rect",
          xmin = summary_df$pairwise_pivot_q25_cm[[1]],
          xmax = summary_df$pairwise_pivot_q75_cm[[1]],
          ymin = -Inf,
          ymax = Inf,
          fill = "#fddbc7",
          alpha = 0.18
        )
      }
    } +
    ggplot2::geom_line(linewidth = 0.9, colour = "#4d9221") +
    {
      if (is.finite(plot_pivot)) {
        ggplot2::geom_vline(
          xintercept = plot_pivot,
          colour = "#b2182b",
          linetype = if (boundary_case) "dashed" else "solid",
          linewidth = 0.9
        )
      }
    } +
    {
      if (is.finite(summary_df$peak_length_cm[[1]])) {
        ggplot2::geom_vline(xintercept = summary_df$peak_length_cm[[1]], colour = "#1b1b1b", linetype = "dotted", linewidth = 0.9)
      }
    } +
    ggplot2::labs(
      title = paste0("Biological Leverage Profile [reference: ", anchor_label, "]"),
      x = "Length (cm)",
      y = expression(Lambda(L))
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
