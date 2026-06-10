#' Resolve one default label for a `Conjurer` metric
#'
#' @param metric Metric name.
#'
#' @return Character scalar.
#'
#' @keywords internal
conjurer_metric_label <- function(metric) {
  # Translate the stored summary metric names to concise display labels.
  metric_map <- c(
    mean_abs_db_shift = "Mean abs dB shift",
    q95_abs_db_shift = "95th pct abs dB shift",
    mean_db_shift = "Mean dB shift",
    sd_db_shift = "SD dB shift",
    policy_switch_rate = "Policy switch rate",
    switch_rate_vs_baseline = "Policy switch rate",
    sd_n_admissible = "SD n admissible",
    mean_n_admissible = "Mean n admissible",
    sd_log_multiplier = "SD log multiplier",
    q95_multiplier_pred = "95th pct multiplier",
    mean_multiplier_pred = "Mean multiplier"
  )
  out <- unname(metric_map[[metric]])
  if (is.null(out) || length(out) == 0 || is.na(out) || !nzchar(out)) {
    out <- stringr::str_replace_all(metric, "_", " ")
    out <- stringr::str_to_title(out)
  }
  out
}

#' Format `Conjurer` heatmap values
#'
#' @param values Numeric vector.
#' @param metric Metric name.
#'
#' @return Character vector.
#'
#' @keywords internal
conjurer_metric_value_labels <- function(values,
                                         metric) {
  # Format percentages separately so switch-rate cells read cleanly in figures.
  out <- rep("", length(values))
  keep <- is.finite(values)
  if (!any(keep)) {
    return(out)
  }
  if (metric %in% c("policy_switch_rate", "switch_rate_vs_baseline")) {
    out[keep] <- sprintf("%.0f%%", 100 * values[keep])
    return(out)
  }
  if (metric %in% c("mean_n_admissible", "sd_n_admissible")) {
    out[keep] <- sprintf("%.1f", values[keep])
    return(out)
  }
  out[keep] <- sprintf("%.2f", values[keep])
  out
}

#' Resolve one default trait-label map for `Conjurer`
#'
#' @param traits Character vector.
#'
#' @return Named character vector.
#'
#' @keywords internal
conjurer_trait_labels <- function(traits) {
  # Humanize the stored trait names without changing their underlying keys.
  traits <- unique(as.character(traits))
  traits <- traits[!is.na(traits) & nzchar(traits)]
  labels <- stringr::str_replace_all(traits, "_", " ")
  labels <- stringr::str_replace(labels, "^study ", "")
  labels <- stringr::str_to_title(labels)
  stats::setNames(labels, traits)
}

#' Save a ggplot and continue when rendering fails
#'
#' @param filename Output filename.
#' @param plot ggplot object.
#' @param width Plot width.
#' @param height Plot height.
#' @param dpi Output DPI.
#' @param ... Additional arguments passed to [ggplot2::ggsave()].
#'
#' @return Logical success flag, invisibly.
#' @keywords internal
save_plot_if_possible <- function(filename,
                                  plot,
                                  width,
                                  height,
                                  dpi = 300,
                                  ...) {
  if (inherits(plot, "ggplot")) {
    plot <- plot +
      ggplot2::labs(title = NULL, subtitle = NULL) +
      ggplot2::theme(
        plot.title = ggplot2::element_blank(),
        plot.subtitle = ggplot2::element_blank()
      )
    # Freeze ggplot objects to grobs before saving so repeated workflow
    # renders cannot accidentally reuse later plot state across files.
    plot <- ggplot2::ggplotGrob(plot)
  }
  ok <- tryCatch(
    {
      ggplot2::ggsave(
        filename = filename,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi,
        ...
      )
      TRUE
    },
    error = function(e) {
      tsb_message("Skipping plot {", filename, "}: ", conditionMessage(e))
      FALSE
    }
  )
  invisible(ok)
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
  if (nrow(plot_df) == 0 || !all(c("block", "importance_score", "delta_log_spread") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = paste0("Local Dropout Sensitivity [", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = NULL, y = "Heuristic importance") + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::mutate(block = factor(block, levels = rev(unique(block))))
  plot_df$block <- dplyr::recode(
    as.character(plot_df$block),
    length_coherence = "Length coherence",
    depth_coherence = "Depth coherence",
    frequency_coherence = "Frequency coherence",
    .default = stringr::str_to_title(stringr::str_replace_all(as.character(plot_df$block), "_", " "))
  )
  if ("component_rank_global" %in% names(plot_df)) {
    block_levels <- plot_df |>
      dplyr::arrange(component_rank_global, block) |>
      dplyr::distinct(block) |>
      dplyr::pull(block)
  } else {
    block_levels <- plot_df |>
      dplyr::arrange(importance_score, block) |>
      dplyr::pull(block) |>
      unique()
  }
  plot_df$block <- factor(plot_df$block, levels = rev(block_levels))

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
  if (nrow(plot_df) == 0 || !all(c("d_species", "d_study", "w_combined", "overlap_same_species") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = paste0("Admissible Similarity Map [", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Species dissimilarity", y = "Study dissimilarity") + ggplot2::theme_minimal(base_size = 11))
  }
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = d_species, y = d_study, size = w_combined, colour = overlap_same_species)
  ) +
    ggplot2::geom_point(alpha = 0.75) +
    ggplot2::scale_colour_manual(values = c("TRUE" = "#b2182b", "FALSE" = "#2166ac")) +
    ggplot2::labs(
      title = paste0("Admissible Similarity Map [", anchor_label, "]"),
      subtitle = "Species vs study dissimilarity among admissible candidate models.",
      x = "Species dissimilarity",
      y = "Study dissimilarity",
      size = "Combined kernel weight",
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
  # Expect the caller to supply the ranked admissible donor table so this
  # function only needs to build the closest-model review figure.
  plot_df <- tibble::as_tibble(top_tbl)
  if (!"w_adm" %in% names(plot_df)) {
    plot_df$w_adm <- if ("w_combined" %in% names(plot_df)) {
      dplyr::coalesce(plot_df$w_combined, NA_real_)
    } else {
      rep(NA_real_, nrow(plot_df))
    }
  }
  if (!"combined_distance" %in% names(plot_df)) {
    plot_df$combined_distance <- if ("d_species" %in% names(plot_df)) {
      dplyr::coalesce(plot_df$d_species, NA_real_)
    } else {
      rep(NA_real_, nrow(plot_df))
    }
  }
  if (!"species_name" %in% names(plot_df)) {
    plot_df$species_name <- NA_character_
  }
  if (!"rank_by_weight" %in% names(plot_df)) {
    plot_df$rank_by_weight <- seq_len(nrow(plot_df))
  }
  if (!"is_group_model" %in% names(plot_df)) {
    plot_df$is_group_model <- FALSE
  }
  if (!"model_id_chr" %in% names(plot_df)) {
    plot_df$model_id_chr <- dplyr::coalesce(plot_df$model_id, seq_len(nrow(plot_df)))
  }
  if (!"biomass_multiplier_if_replace" %in% names(plot_df)) {
    plot_df$biomass_multiplier_if_replace <- NA_real_
  }
  if (!"genus" %in% names(plot_df)) {
    plot_df$genus <- NA_character_
  }
  if (!"species" %in% names(plot_df)) {
    plot_df$species <- NA_character_
  }
  plot_df <- plot_df |>
    dplyr::filter(
      is.finite(w_adm),
      w_adm > 0,
      !is.na(model_id_chr)
    ) |>
    dplyr::arrange(dplyr::desc(w_adm), combined_distance) |>
    dplyr::slice_head(n = 10L) |>
    dplyr::mutate(
      species_label = dplyr::case_when(
        !is.na(species_name) & nzchar(species_name) & species_name != "NA NA" ~ species_name,
        !is.na(genus) & nzchar(genus) & genus != "NA" &
          !is.na(species) & species %in% c("NA", "sp", "sp.", "spp", "spp.") ~ paste0(genus, " sp."),
        !is.na(genus) & nzchar(genus) & genus != "NA" ~ paste0(genus, " sp."),
        TRUE ~ "Generic"
      ),
      species_expr = gsub("'", "\\\\'", species_label, fixed = TRUE),
      candidate_label = paste0(
        rank_by_weight,
        ".~italic('",
        species_expr,
        "')",
        dplyr::if_else(isTRUE(is_group_model), "~'[group]'", ""),
        "~'{m",
        as.character(model_id_chr),
        "}'"
      ),
      candidate_label = factor(candidate_label, levels = rev(candidate_label))
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = "Final weight") + ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = candidate_label, y = w_adm, fill = biomass_multiplier_if_replace)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_viridis_c(
      direction = -1,
      option = "C",
      trans = "log10",
      na.value = "grey80"
    ) +
    ggplot2::scale_x_discrete(labels = function(x) parse(text = x)) +
    ggplot2::labs(
      x = NULL,
      y = "Final weight",
      fill = "Biomass multiplier"
    ) +
    ggplot2::theme_minimal(base_size = 11)
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
                          policy_label,
                          show_top_candidate = FALSE) {
  # Draw the conformal ribbons first, then overlay the anchor, selected, and
  # top-candidate curves from the precomputed summary tables.
  band_df <- tibble::as_tibble(band_tbl)
  curve_df <- tibble::as_tibble(curve_tbl)
  if (nrow(band_df) == 0 || nrow(curve_df) == 0 ||
    !all(c("length_cm", "ymin", "ymax", "band") %in% names(band_df)) ||
    !all(c("length_cm", "ts_anchor", "ts_pred") %in% names(curve_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "Length (cm)", y = "TS (dB re 1 m^2)") + ggplot2::theme_minimal(base_size = 11))
  }
  p <- ggplot2::ggplot() +
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
      ggplot2::aes(x = length_cm, y = ts_pred, colour = "Selected policy", linetype = "Selected policy"),
      linewidth = 0.95
    )
  if (isTRUE(show_top_candidate) && "ts_top_candidate" %in% names(curve_df)) {
    p <- p +
      ggplot2::geom_line(
        data = curve_df,
        ggplot2::aes(x = length_cm, y = ts_top_candidate, colour = "Top candidate", linetype = "Top candidate"),
        linewidth = 0.85,
        alpha = 0.9
      )
  }
  p <- p +
    ggplot2::geom_line(
      data = curve_df,
      ggplot2::aes(x = length_cm, y = ts_anchor),
      linewidth = 1.45,
      colour = "white",
      linetype = "longdash",
      alpha = 0.95
    ) +
    ggplot2::geom_line(
      data = curve_df,
      ggplot2::aes(x = length_cm, y = ts_anchor, colour = "Anchor", linetype = "Anchor"),
      linewidth = 0.85
    )
  colour_values <- c(
    "Selected policy" = "#2166ac",
    "Anchor" = "#1b1b1b"
  )
  linetype_values <- c(
    "Selected policy" = "solid",
    "Anchor" = "longdash"
  )
  if (isTRUE(show_top_candidate) && "ts_top_candidate" %in% names(curve_df)) {
    colour_values <- c(colour_values, "Top candidate" = "#7f2704")
    linetype_values <- c(linetype_values, "Top candidate" = "dotdash")
  }

  p +
    ggplot2::scale_colour_manual(
      values = colour_values,
      name = "Curve"
    ) +
    ggplot2::scale_linetype_manual(
      values = linetype_values,
      name = "Curve"
    ) +
    ggplot2::labs(
      x = "Length (cm)",
      y = "TS (dB re 1 m^2)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
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
  if (nrow(plot_df) == 0 || !all("slope_len_cell" %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Study-Cell Distribution of TS-Length Slopes", subtitle = "Required plotting fields were not available.", x = "Standardized TS-length slope", y = "Study-cell count") + ggplot2::theme_minimal(base_size = 11))
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
  if (nrow(plot_df) == 0 || !all(c("review_group", "slope_len_cell") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "TS-Length Slope by Species Group", subtitle = "Required plotting fields were not available.", x = NULL, y = "Standardized TS-length slope") + ggplot2::theme_minimal(base_size = 11))
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
  if (nrow(plot_df) == 0 || !all(c("review_group", "prop_study_cells", "original_reference_class") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Support for 20log10 Dependence by Species Group", subtitle = "Required plotting fields were not available.", x = NULL, y = "Proportion of study-cells") + ggplot2::theme_minimal(base_size = 11))
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
  if (nrow(plot_df) == 0 || !all(c("MDS1", "MDS2") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "NMDS1", y = "NMDS2") + ggplot2::theme_minimal(base_size = 11))
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
  p <- ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = MDS1,
      y = MDS2,
      color = .data[[cluster_name]]
    ),
    data = plot_df
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  # Add model points
  p <- p +
    ggplot2::geom_point(
      data = plot_df[!ref_flag, , drop = FALSE],
      alpha = 0.35,
      size = 2.1
    )

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
    ggplot2::scale_fill_brewer(
      palette = "Dark2",
      guide = "none",
      limits = cluster_limits
    )

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
  vec_df <- drop_ordination_synthetic_overlap_traits(vec_tbl, trait_col = "trait")
  point_df <- tibble::as_tibble(points_tbl)
  if (nrow(vec_df) == 0 || nrow(point_df) == 0 ||
    !all(c("trait", "MDS1", "MDS2") %in% names(vec_df)) ||
    !all(c("MDS1", "MDS2") %in% names(point_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Global NMDS Variable Loadings", subtitle = "Required plotting fields were not available.", x = "NMDS1", y = "NMDS2") + ggplot2::theme_minimal(base_size = 11))
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

  ggplot2::ggplot(
    vec_df |>
      dplyr::mutate(trait_label = stringr::str_replace_all(trait, "_", " ")),
    ggplot2::aes(x = 0, y = 0)
  ) +
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
      fontface = "italic",
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
  if (nrow(pts) == 0 || !all(c("MDS1", "MDS2") %in% names(pts))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "NMDS1", y = "NMDS2") + ggplot2::theme_minimal(base_size = 11))
  }
  if (!(species_col %in% names(pts))) {
    species_col <- "model_id"
    if (!(species_col %in% names(pts))) {
      pts[[species_col]] <- ""
    }
  }
  pts[[species_col]] <- stringr::str_squish(as.character(pts[[species_col]]))
  label_species <- if ("species" %in% names(pts)) {
    stringr::str_squish(as.character(pts$species))
  } else {
    stringr::str_squish(stringr::word(pts[[species_col]], 2, sep = stringr::regex("\\s+")))
  }
  label_genus <- if ("genus" %in% names(pts)) {
    stringr::str_squish(as.character(pts$genus))
  } else {
    stringr::str_squish(stringr::word(pts[[species_col]], 1, sep = stringr::regex("\\s+")))
  }
  pts <- pts |>
    dplyr::mutate(
      plot_label = dplyr::case_when(
        is.na(.data[[species_col]]) | !nzchar(.data[[species_col]]) ~ dplyr::if_else(
          !is.na(label_genus) & nzchar(label_genus) & label_genus != "NA",
          paste0(label_genus, " sp."),
          "Generic"
        ),
        .data[[species_col]] == "NA NA" ~ "Generic",
        !is.na(label_genus) & nzchar(label_genus) & label_genus != "NA" &
          !is.na(label_species) & label_species %in% c("NA", "sp", "sp.", "spp", "spp.") ~ paste0(label_genus, " sp."),
        TRUE ~ .data[[species_col]]
      )
    ) |>
    dplyr::filter(
      !is.na(plot_label),
      nzchar(plot_label)
    )
  if (nrow(pts) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "NMDS1", y = "NMDS2") + ggplot2::theme_minimal(base_size = 11))
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
  label_df <- pts[label_flag, , drop = FALSE]

  # Create base layer with the grid setup
  p <- ggplot2::ggplot(mapping = ggplot2::aes(MDS1_plot,
    y = MDS2_plot
  )) +
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
        mapping = ggplot2::aes(
          x = MDS1,
          y = MDS2,
          fill = group_val,
          color = group_val,
          group = group_val
        ),
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
    sig_vec <- drop_ordination_synthetic_overlap_traits(vec_tbl, trait_col = "trait") |>
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
    sig_fac <- drop_ordination_synthetic_overlap_traits(fac_tbl, trait_col = "trait") |>
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
      data = label_df,
      ggplot2::aes(
        label = plot_label,
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
    ggplot2::scale_colour_manual(
      values = c("black" = "black", "grey30" = "grey30"),
      guide = "none"
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_discrete_identity(aesthetic = "fontface") +
    ggplot2::scale_fill_brewer(
      palette = "Set1",
      name = colorbar_name,
      labels = function(x) stringr::str_to_title(stringr::str_replace_all(x, "_", " "))
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(override.aes = list(
        shape = 21,
        size = 3,
        colour =
          "grey40",
        stroke = 0.4
      ))
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
    !all(c("trait", "level", "MDS1", "MDS2") %in% names(fac_df)) ||
    !all(c("MDS1", "MDS2") %in% names(point_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "NMDS1", y = "NMDS2") + ggplot2::theme_minimal(base_size = 11))
  }
  if (!"n" %in% names(fac_df)) {
    fac_df$n <- 1
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
      fontface = "italic",
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
    return(ggplot2::ggplot() + ggplot2::labs(title = "Applicability Overlap Profile by Reference", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }
  if (is.null(metric_labs)) {
    metric_labs <- c(
      w_same_species = "Same species",
      w_same_family = "Same family",
      w_same_swimbladder = "Same swimbladder",
      w_same_fao = "Same FAO area",
      w_same_ocean_basin = "Same ocean basin",
      mean_length_overlap_fraction = "Mean length overlap",
      mean_length_nonoverlap_fraction = "Mean length nonoverlap",
      mean_depth_overlap_fraction = "Mean depth overlap",
      mean_depth_nonoverlap_fraction = "Mean depth nonoverlap"
    )
  }
  if (!"mean_length_nonoverlap_fraction" %in% names(overlap_df) &&
    "mean_length_overlap_fraction" %in% names(overlap_df)) {
    overlap_df$mean_length_nonoverlap_fraction <- 1 - overlap_df$mean_length_overlap_fraction
  }
  if (!"mean_depth_nonoverlap_fraction" %in% names(overlap_df) &&
    "mean_depth_overlap_fraction" %in% names(overlap_df)) {
    overlap_df$mean_depth_nonoverlap_fraction <- 1 - overlap_df$mean_depth_overlap_fraction
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
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "inadmissible_reason", "n_models") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Admissibility Gate Composition by Reference", subtitle = "Required plotting fields were not available.", x = NULL, y = "Proportion of candidate models") + ggplot2::theme_minimal(base_size = 11))
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
    ) |>
    tidyr::complete(
      anchor_species,
      inadmissible_reason = factor(gate_levels, levels = gate_levels),
      fill = list(n_models = 0)
    ) |>
    dplyr::mutate(
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
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "q50_multiplier_admissible", "q05_multiplier_admissible", "q95_multiplier_admissible") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Admissible Biomass Multiplier Range by Reference", subtitle = "Required plotting fields were not available.", x = NULL, y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
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
  if (!all(c("valid_prediction", "error_abs_log") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = "|log(multiplier prediction)|") + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df$policy <- resolve_policy_display_names(plot_df)
  plot_df <- plot_df |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log)) |>
    dplyr::mutate(plot_error = pmax(error_abs_log, .Machine$double.xmin))
  if (nrow(plot_df) == 0 || !"policy" %in% names(plot_df)) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = "|log(multiplier prediction)|") + ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = reorder(policy, plot_error, FUN = stats::median), y = plot_error, fill = policy)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.18, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::annotation_logticks(sides = "b") +
    ggplot2::labs(
      x = NULL,
      y = "|log(multiplier prediction)|"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "none")
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
  if (!all(c("valid_prediction", "error_abs_log") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = "|log(multiplier prediction)|") + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df$policy <- resolve_policy_display_names(plot_df)
  plot_df <- plot_df |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log)) |>
    dplyr::mutate(plot_error = pmax(error_abs_log, .Machine$double.xmin))
  if (nrow(plot_df) == 0 || !"policy" %in% names(plot_df)) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = "|log(multiplier prediction)|") + ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = reorder(policy, plot_error, FUN = stats::median), y = plot_error, fill = policy)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.18, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::annotation_logticks(sides = "b") +
    ggplot2::labs(
      x = NULL,
      y = "|log(multiplier prediction)|"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "none")
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
  if (!all(c("anchor_species", "valid_prediction", "error_abs_log") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df$policy <- resolve_policy_display_names(plot_df)
  plot_df <- plot_df |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log)) |>
    dplyr::group_by(anchor_species, policy) |>
    dplyr::summarise(median_abs_log_error = stats::median(error_abs_log, na.rm = TRUE), .groups = "drop")
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "policy", "median_abs_log_error") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
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
    ggplot2::scale_y_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = "Median |log error|"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Build one `Conjurer` summary heatmap
#'
#' @param x A [Conjurer] object or `Conjurer@summary`-like tibble.
#' @param metric Summary metric to plot.
#' @param trait_labs Optional named vector mapping stored trait names to display labels.
#' @param anchor_order Optional anchor-species order.
#' @param trait_order Optional trait order.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param fill_lab Optional legend title.
#' @param show_values Logical scalar indicating whether to print cell labels.
#' @param na_value Tile color for missing cells.
#' @param tile_colour Tile border color.
#'
#' @return A ggplot object.
#'
#' @keywords internal
conjurer_heatmap_plot <- function(x,
                                  metric = "mean_abs_db_shift",
                                  trait_labs = NULL,
                                  anchor_order = NULL,
                                  trait_order = NULL,
                                  title = NULL,
                                  subtitle = NULL,
                                  fill_lab = NULL,
                                  show_values = TRUE,
                                  na_value = "grey90",
                                  tile_colour = "white") {
  # Accept either the staged class or a plain summary table for figure work.
  conjurer_obj <- (inherits(x, "S7_object") && exists("Conjurer", inherits = TRUE) && isTRUE(tryCatch(S7::S7_inherits(x, Conjurer), error = function(e) FALSE)))
  plot_df <- if (conjurer_obj) {
    tibble::as_tibble(x@summary)
  } else {
    tibble::as_tibble(x)
  }
  manifest_df <- if (conjurer_obj) tibble::as_tibble(x@manifest) else tibble::tibble()
  configured_traits <- if (conjurer_obj) {
    names(conjurer_analysis_config(x)$study_weights %||% numeric(0))
  } else {
    character(0)
  }
  configured_traits <- configured_traits[!is.na(configured_traits) & nzchar(configured_traits)]
  configured_anchors <- if (conjurer_obj) {
    anchors_tbl <- tibble::as_tibble(x@selector@candidates@reference_anchors)
    if ("species_name" %in% names(anchors_tbl)) {
      as.character(anchors_tbl$species_name)
    } else {
      character(0)
    }
  } else {
    character(0)
  }
  configured_anchors <- unique(configured_anchors[!is.na(configured_anchors) & nzchar(configured_anchors)])
  if (conjurer_obj && length(configured_traits) > 0) {
    if (!"anchor_species" %in% names(plot_df)) {
      plot_df$anchor_species <- NA_character_
    }
    if (!"trait" %in% names(plot_df)) {
      plot_df$trait <- NA_character_
    }
    if (!(metric %in% names(plot_df))) {
      plot_df[[metric]] <- NA_real_
    }
    status_df <- manifest_df |>
      dplyr::select(dplyr::any_of(c("trait", "status"))) |>
      dplyr::distinct()
    anchor_levels_full <- unique(c(configured_anchors, as.character(plot_df$anchor_species)))
    anchor_levels_full <- anchor_levels_full[!is.na(anchor_levels_full) & nzchar(anchor_levels_full)]
    trait_levels_full <- unique(c(configured_traits, as.character(plot_df$trait)))
    trait_levels_full <- trait_levels_full[!is.na(trait_levels_full) & nzchar(trait_levels_full)]
    if (length(anchor_levels_full) > 0 && length(trait_levels_full) > 0) {
      plot_df <- tidyr::expand_grid(
        anchor_species = anchor_levels_full,
        trait = trait_levels_full
      ) |>
        dplyr::left_join(plot_df, by = c("anchor_species", "trait"))
      if ("trait" %in% names(status_df)) {
        plot_df <- plot_df |>
          dplyr::left_join(status_df, by = "trait")
      } else {
        plot_df$status <- NA_character_
      }
      plot_df[[metric]] <- dplyr::case_when(
        is.finite(plot_df[[metric]]) ~ plot_df[[metric]],
        plot_df$status == "no_missing_rows" ~ 0,
        TRUE ~ plot_df[[metric]]
      )
    }
  }

  # Stop early with a placeholder when the required summary columns are absent.
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "trait", metric) %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = title %||% "Missing Study Metadata Uncertainty Heatmap", subtitle = subtitle %||% paste0("Required Conjurer summary columns were not available for metric: ", metric, "."), x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }

  # Resolve the display text from the selected summary metric.
  metric_lab <- conjurer_metric_label(metric)
  title <- title %||% "Missing Study Metadata Uncertainty Heatmap"
  subtitle <- subtitle %||% paste0(
    "Rows = anchor species, columns = study metadata traits, fill = ",
    tolower(metric_lab),
    "."
  )
  fill_lab <- fill_lab %||% metric_lab
  trait_labs <- trait_labs %||% conjurer_trait_labels(plot_df$trait)

  # Keep one row per anchor-trait pair before setting display order.
  plot_df <- plot_df |>
    dplyr::select(anchor_species, trait, value = dplyr::all_of(metric)) |>
    dplyr::mutate(
      trait_label = dplyr::recode(trait, !!!trait_labs),
      value_label = conjurer_metric_value_labels(value, metric)
    )

  # Order traits by overall magnitude unless the caller supplied one directly.
  if (is.null(trait_order)) {
    if (length(configured_traits) > 0) {
      trait_order <- unname(trait_labs[configured_traits])
      trait_order <- trait_order[!is.na(trait_order) & nzchar(trait_order)]
      trait_order <- unique(c(trait_order, setdiff(unique(as.character(plot_df$trait_label)), trait_order)))
    } else {
      trait_order <- plot_df |>
        dplyr::group_by(trait_label) |>
        dplyr::summarise(metric_mean = mean(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(metric_mean), trait_label) |>
        dplyr::pull(trait_label)
    }
  }

  # Order anchors alphabetically unless the caller supplied one directly.
  if (is.null(anchor_order)) {
    anchor_order <- if (length(configured_anchors) > 0) {
      unique(c(configured_anchors, sort(setdiff(unique(as.character(plot_df$anchor_species)), configured_anchors))))
    } else {
      sort(unique(plot_df$anchor_species))
    }
  }

  plot_df <- plot_df |>
    dplyr::mutate(
      anchor_species = factor(anchor_species, levels = anchor_order),
      trait_label = factor(trait_label, levels = trait_order)
    )

  # Build the heatmap around one continuous fill scale and optional cell labels.
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = trait_label, y = anchor_species, fill = value)) +
    ggplot2::geom_tile(colour = tile_colour) +
    ggplot2::scale_fill_gradientn(
      colours = if (metric %in% c("mean_n_admissible", "sd_n_admissible")) {
        c("#eef3ee", "#bfd7bf", "#6ea56e", "#2f6b3b")
      } else {
        c("#f3f1ea", "#d7c6a5", "#bf8f49", "#8e4b1f")
      },
      na.value = na_value
    ) +
    ggplot2::scale_y_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      fill = fill_lab
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40")
    )

  # Overlay formatted value labels when the caller wants a manuscript-style figure.
  if (isTRUE(show_values)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = value_label), size = 3.2)
  }

  p
}

#' Plot `Conjurer` missingness uncertainty summaries
#'
#' @name plot.Conjurer
#'
#' @param x A [Conjurer] object.
#' @param y Unused.
#' @param metric Summary metric to plot.
#' @param trait_labs Optional named vector mapping stored trait names to display labels.
#' @param anchor_order Optional anchor-species order.
#' @param trait_order Optional trait order.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param fill_lab Optional legend title.
#' @param show_values Logical scalar indicating whether to print cell labels.
#' @param na_value Tile color for missing cells.
#' @param tile_colour Tile border color.
#' @param ... Unused additional arguments.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' plot(conjurer)
#' plot(conjurer, metric = "q95_abs_db_shift")
#' plot(conjurer, metric = "switch_rate_vs_baseline")
#' }
S7::method(plot_generic, Conjurer) <- function(x,
                                               y = NULL,
                                               metric = "mean_abs_db_shift",
                                               trait_labs = NULL,
                                               anchor_order = NULL,
                                               trait_order = NULL,
                                               title = NULL,
                                               subtitle = NULL,
                                               fill_lab = NULL,
                                               show_values = TRUE,
                                               na_value = "grey90",
                                               tile_colour = "white",
                                               ...) {
  # Route `plot()` calls for Conjurer objects through the shared heatmap helper.
  conjurer_heatmap_plot(
    x = x,
    metric = metric,
    trait_labs = trait_labs,
    anchor_order = anchor_order,
    trait_order = trait_order,
    title = title,
    subtitle = subtitle,
    fill_lab = fill_lab,
    show_values = show_values,
    na_value = na_value,
    tile_colour = tile_colour
  )
}

#' Summarize species-policy benchmark performance
#'
#' @param perf_tbl Species-block benchmark table.
#'
#' @return A tibble with one row per held-out species and policy.
#' @export
summarize_species_policy_performance <- function(perf_tbl) {
  perf_tbl <- tibble::as_tibble(perf_tbl)
  if (!all(c("anchor_species", "policy", "valid_prediction", "error_abs_log") %in% names(perf_tbl))) {
    return(tibble::tibble())
  }

  perf_tbl$policy <- resolve_policy_display_names(perf_tbl)
  if (!"local_weighted_mean_combined_distance" %in% names(perf_tbl)) {
    perf_tbl$local_weighted_mean_combined_distance <- NA_real_
  }
  if (!"local_structural_q_abs_log" %in% names(perf_tbl)) {
    perf_tbl$local_structural_q_abs_log <- NA_real_
  }

  policy_grid <- tidyr::expand_grid(
    anchor_species = sort(unique(as.character(perf_tbl$anchor_species))),
    policy = sort(unique(as.character(perf_tbl$policy)))
  )
  row_counts <- perf_tbl |>
    dplyr::group_by(anchor_species, policy) |>
    dplyr::summarise(
      n_tested_anchor_rows = dplyr::n(),
      n_valid_anchor_models = sum(valid_prediction & is.finite(error_abs_log), na.rm = TRUE),
      .groups = "drop"
    )

  valid_summary <- perf_tbl |>
    dplyr::filter(valid_prediction, is.finite(error_abs_log)) |>
    dplyr::group_by(anchor_species, policy) |>
    dplyr::summarise(
      median_abs_log_error = stats::median(error_abs_log, na.rm = TRUE),
      mean_abs_log_error = mean(error_abs_log, na.rm = TRUE),
      q90_abs_log_error = stats::quantile(error_abs_log, probs = 0.90, na.rm = TRUE, names = FALSE, type = 8),
      median_predicted_multiplier = stats::median(multiplier_pred, na.rm = TRUE),
      median_local_combined_distance = stats::median(local_weighted_mean_combined_distance, na.rm = TRUE),
      median_structural_q_abs_log = stats::median(local_structural_q_abs_log, na.rm = TRUE),
      .groups = "drop"
    )

  policy_grid |>
    dplyr::left_join(row_counts, by = c("anchor_species", "policy")) |>
    dplyr::left_join(valid_summary, by = c("anchor_species", "policy")) |>
    dplyr::mutate(
      n_tested_anchor_rows = dplyr::coalesce(n_tested_anchor_rows, 0L),
      n_valid_anchor_models = dplyr::coalesce(n_valid_anchor_models, 0L),
      n_anchor_models = n_valid_anchor_models,
      has_valid_prediction = n_valid_anchor_models > 0
    ) |>
    dplyr::group_by(anchor_species) |>
    dplyr::arrange(
      !has_valid_prediction,
      median_abs_log_error,
      mean_abs_log_error,
      policy,
      .by_group = TRUE
    ) |>
    dplyr::mutate(rank_within_species = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(policy) |>
    dplyr::mutate(global_median_abs_log_error = stats::median(median_abs_log_error, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(anchor_species, rank_within_species)
}

#' Plot every species-policy benchmark rank
#'
#' @param perf_tbl Species-block benchmark table or output from
#'   [summarize_species_policy_performance()].
#'
#' @return A ggplot object.
#' @export
plot_species_policy_ranked <- function(perf_tbl) {
  plot_df <- tibble::as_tibble(perf_tbl)
  if (!all(c("rank_within_species", "median_abs_log_error") %in% names(plot_df))) {
    plot_df <- summarize_species_policy_performance(plot_df)
  }
  if (nrow(plot_df) == 0 ||
    !all(c("anchor_species", "policy", "rank_within_species", "median_abs_log_error") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Species-Blocked Policy Performance by Held-Out Species", subtitle = "Required plotting fields were not available.", x = "Median |log multiplier error|", y = "Policy") + ggplot2::theme_minimal(base_size = 11))
  }

  plot_df <- plot_df |>
    dplyr::arrange(anchor_species, dplyr::desc(rank_within_species)) |>
    dplyr::group_by(anchor_species) |>
    dplyr::mutate(
      invalid_x_position = {
        finite_x <- median_abs_log_error[is.finite(median_abs_log_error)]
        if (length(finite_x) == 0) 1 else max(finite_x, na.rm = TRUE) * 1.08
      },
      plot_abs_log_error = dplyr::if_else(
        is.finite(median_abs_log_error),
        median_abs_log_error,
        invalid_x_position
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      policy_species_key = paste(policy, anchor_species, sep = "__"),
      policy_species_key = factor(policy_species_key, levels = unique(policy_species_key))
    ) |>
    dplyr::filter(
      !is.na(anchor_species),
      !is.na(policy),
      !is.na(has_valid_prediction),
      is.finite(plot_abs_log_error),
      is.finite(n_anchor_models)
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Species-Blocked Policy Performance by Held-Out Species", subtitle = "Required plotting fields were not available.", x = "Median |log multiplier error|", y = "Policy") + ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = plot_abs_log_error,
      y = policy_species_key,
      color = median_structural_q_abs_log,
      size = n_anchor_models,
      shape = has_valid_prediction
    )
  ) +
    ggplot2::geom_point(alpha = 0.88) +
    ggplot2::facet_wrap(~anchor_species, scales = "free_y", ncol = 4) +
    ggplot2::scale_y_discrete(labels = function(x) sub("__.*$", "", x)) +
    ggplot2::scale_color_viridis_c(
      option = "C",
      na.value = "grey55",
      name = "Median structural\nq_abs_log"
    ) +
    ggplot2::scale_size_continuous(range = c(1.2, 3.2), name = "Anchor rows") +
    ggplot2::scale_shape_manual(
      values = c("TRUE" = 16, "FALSE" = 4),
      labels = c("FALSE" = "No valid prediction", "TRUE" = "Valid prediction"),
      name = "Benchmark row"
    ) +
    ggplot2::labs(
      title = "Species-Blocked Policy Performance by Held-Out Species",
      subtitle = "Each facet is ordered best-to-worst from top to bottom; no-valid-prediction policies are retained at the bottom.",
      x = "Median |log(multiplier prediction)|",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 8) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 8),
      axis.text.y = ggplot2::element_text(size = 5.5),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

#' Compare selected policies against species-block oracle ranks
#'
#' @param species_policy_tbl Output from
#'   [summarize_species_policy_performance()].
#' @param selected_tbl Anchor selected-policy table.
#' @param selection_source Label for the selection source.
#'
#' @return A tibble.
#' @export
compare_selected_policy_species_rank <- function(species_policy_tbl,
                                                 selected_tbl,
                                                 selection_source = "selected") {
  species_policy_tbl <- tibble::as_tibble(species_policy_tbl)
  selected_tbl <- tibble::as_tibble(selected_tbl)
  if (nrow(species_policy_tbl) == 0 || nrow(selected_tbl) == 0 ||
    !all(c("anchor_species", "policy", "rank_within_species") %in% names(species_policy_tbl))) {
    return(tibble::tibble())
  }
  if (!"selected_policy" %in% names(selected_tbl)) {
    selected_tbl$selected_policy <- resolve_selected_policy_values(selected_tbl)
  }
  selected_tbl$selected_policy_display <- resolve_selected_policy_names(selected_tbl)
  species_policy_tbl$policy_display <- as.character(species_policy_tbl$policy)

  best_tbl <- species_policy_tbl |>
    dplyr::filter(has_valid_prediction) |>
    dplyr::group_by(anchor_species) |>
    dplyr::arrange(rank_within_species, .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      anchor_species,
      species_oracle_best_policy = policy,
      species_oracle_best_median_abs_log_error = median_abs_log_error
    )

  selected_tbl |>
    dplyr::filter(
      !is.na(selected_policy_display),
      nzchar(as.character(selected_policy_display))
    ) |>
    dplyr::mutate(
      selected_policy = as.character(selected_policy),
      selected_policy_display = as.character(selected_policy_display),
      selection_source = selection_source
    ) |>
    dplyr::left_join(
      species_policy_tbl |>
        dplyr::select(
          anchor_species,
          selected_policy_display = policy_display,
          species_rank = rank_within_species,
          selected_species_median_abs_log_error = median_abs_log_error,
          selected_species_mean_abs_log_error = mean_abs_log_error,
          selected_species_q90_abs_log_error = q90_abs_log_error,
          selected_has_valid_species_prediction = has_valid_prediction,
          selected_n_valid_anchor_models = n_valid_anchor_models,
          selected_n_tested_anchor_rows = n_tested_anchor_rows
        ),
      by = c("anchor_species", "selected_policy_display")
    ) |>
    dplyr::left_join(best_tbl, by = "anchor_species") |>
    dplyr::mutate(
      selected_delta_to_species_oracle = selected_species_median_abs_log_error -
        species_oracle_best_median_abs_log_error
    ) |>
    dplyr::arrange(anchor_species, selection_source, species_rank, selected_policy_display)
}

#' Plot selected policies against species-block oracle ranks
#'
#' @param diagnostics_tbl Output from
#'   [compare_selected_policy_species_rank()].
#'
#' @return A ggplot object.
#' @export
plot_selected_policy_species_rank <- function(diagnostics_tbl) {
  plot_df <- tibble::as_tibble(diagnostics_tbl)
  if (nrow(plot_df) == 0 ||
    !all(c("anchor_species", "species_rank", "selection_source") %in% names(plot_df)) ||
    !any(c("selected_policy", "selected_policy_display") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "Species-block rank", y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df$selected_policy_display <- resolve_selected_policy_names(plot_df)
  plot_df <- plot_df |>
    dplyr::filter(
      !is.na(anchor_species),
      !is.na(selected_policy_display),
      nzchar(selected_policy_display),
      is.finite(species_rank)
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "Species-block rank", y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = species_rank,
      y = stats::reorder(anchor_species, species_rank, stats::median, na.rm = TRUE),
      color = selection_source
    )
  ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey55") +
    ggplot2::geom_point(size = 2.6, alpha = 0.9) +
    ggplot2::geom_text(
      ggplot2::aes(label = selected_policy_display),
      hjust = -0.05,
      size = 2.4,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(limits = c(1, NA), expand = ggplot2::expansion(mult = c(0.02, 0.35))) +
    ggplot2::scale_y_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
    ggplot2::labs(
      x = "Rank within held-out species",
      y = NULL,
      color = "Selection source"
    ) +
    ggplot2::theme_minimal(base_size = 10)
}

#' Plot conformal calibration by policy
#'
#' @param cal_tbl Policy-level conformal calibration table.
#'
#' @return A ggplot object.
#'
#' @export
plot_conformal_scores <- function(cal_tbl) {
  # Accept either the raw calibration tibble or the stored uncertainty bundle,
  # then draw the policy-by-branch calibration surface directly.
  if (is.list(cal_tbl) &&
    !inherits(cal_tbl, "data.frame") &&
    "conf_cal" %in% names(cal_tbl)) {
    cal_tbl <- cal_tbl$conf_cal
  }
  plot_df <- tibble::as_tibble(cal_tbl)
  if (nrow(plot_df) == 0 || !all(c("policy", "q_abs_log") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Conformal Calibration Radius by Policy", subtitle = "Required plotting fields were not available.", x = "Policy", y = "Conformal q_abs_log") + ggplot2::theme_minimal(base_size = 11))
  }
  if (!"equation_branch_filter" %in% names(plot_df)) {
    plot_df$equation_branch_filter <- "all"
  }
  if (!"n" %in% names(plot_df)) {
    plot_df$n <- NA_integer_
  }

  plot_df <- plot_df |>
    dplyr::mutate(
      policy_display = stringr::str_remove(
        resolve_policy_display_names(plot_df),
        "\\s*\\[[^]]+\\]$"
      ),
      branch_display = {
        branch_defs <- read_policy_registry()$policy_branches %||% list()
        branch_names <- stats::setNames(
          vapply(branch_defs, function(x) as.character(x$display_name %||% x$key %||% NA_character_), character(1)),
          vapply(branch_defs, function(x) as.character(x$key %||% NA_character_), character(1))
        )
        branch_vals <- as.character(equation_branch_filter)
        branch_labs <- unname(branch_names[branch_vals])
        branch_labs[is.na(branch_labs) | !nzchar(branch_labs)] <- branch_vals[is.na(branch_labs) | !nzchar(branch_labs)]
        branch_labs
      },
      label_colour = ifelse(
        q_abs_log >= stats::median(q_abs_log, na.rm = TRUE),
        "white",
        "black"
      )
    ) |>
    dplyr::filter(
      !is.na(policy_display),
      nzchar(policy_display),
      is.finite(q_abs_log)
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }

  policy_levels <- plot_df |>
    dplyr::group_by(policy_display) |>
    dplyr::summarise(mean_q_abs_log = mean(q_abs_log, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(mean_q_abs_log, policy_display) |>
    dplyr::pull(policy_display)
  branch_levels <- {
    branch_defs <- read_policy_registry()$policy_branches %||% list()
    registry_levels <- vapply(branch_defs, function(x) as.character(x$display_name %||% x$key %||% NA_character_), character(1))
    intersect(registry_levels, unique(plot_df$branch_display))
  }
  sigma_value <- max(0.2, stats::median(plot_df$q_abs_log, na.rm = TRUE) / 3)
  if (!is.finite(sigma_value) || sigma_value <= 0) {
    sigma_value <- 0.2
  }

  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(
        policy_display = factor(policy_display, levels = rev(policy_levels)),
        branch_display = factor(branch_display, levels = branch_levels)
      ),
    ggplot2::aes(x = branch_display, y = policy_display, fill = q_abs_log)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%.2f", q_abs_log),
        colour = label_colour
      ),
      size = 3.1,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      direction = -1,
      trans = scales::pseudo_log_trans(sigma = sigma_value)
    ) +
    ggplot2::labs(
      title = "Conformal Calibration Radius by Policy",
      x = NULL,
      y = NULL,
      fill = "q_abs_log"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 9)
    )
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
  # Support both the global benchmark dropout summary and the anchor-level
  # component-ablation table with one plotting surface.
  plot_df <- tibble::as_tibble(impact_tbl)
  if (nrow(plot_df) > 0 &&
    all(c("component", "delta_log_spread", "delta_log_consensus") %in% names(plot_df))) {
    if (!"anchor_species" %in% names(plot_df)) {
      plot_df$anchor_species <- "Anchor"
    }
    if (!"component_rank_global" %in% names(plot_df)) {
      plot_df$component_rank_global <- rank(-plot_df$importance_score, ties.method = "first")
    }
    component_levels <- plot_df |>
      dplyr::mutate(
        component_label = dplyr::recode(
          as.character(component),
          length_coherence = "Length coherence",
          depth_coherence = "Depth coherence",
          frequency_coherence = "Frequency coherence",
          .default = stringr::str_to_title(stringr::str_replace_all(as.character(component), "_", " "))
        )
      ) |>
      dplyr::arrange(component_rank_global, component_label) |>
      dplyr::distinct(component_label) |>
      dplyr::pull(component_label)
    plot_df <- plot_df |>
      dplyr::mutate(
        component_label = dplyr::recode(
          as.character(component),
          length_coherence = "Length coherence",
          depth_coherence = "Depth coherence",
          frequency_coherence = "Frequency coherence",
          .default = stringr::str_to_title(stringr::str_replace_all(as.character(component), "_", " "))
        ),
        component_label = factor(
          component_label,
          levels = component_levels
        ),
        anchor_label = paste0("italic('", gsub("'", "\\\\'", as.character(anchor_species), fixed = TRUE), "')")
      )

    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = component_label, y = delta_log_spread, fill = delta_log_consensus)
    ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0) +
      ggplot2::labs(
        title = "Anchor-Level Component Importance for Similarity Weighting",
        subtitle = "Positive delta log spread means the selected-anchor multiplier interval widens when that component is removed.",
        x = NULL,
        y = "Delta log spread after component dropout",
        fill = "Delta log\nconsensus"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40"))

    if (dplyr::n_distinct(plot_df$anchor_species) > 1) {
      p <- p +
        ggplot2::facet_wrap(
          ~anchor_label,
          ncol = 2,
          scales = "free_y",
          labeller = ggplot2::label_parsed
        ) +
        ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
    }
    return(p)
  }

  if (nrow(plot_df) == 0 || !all(c("component", "delta_rmse", "delta_mae") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Empirical Component Importance for Similarity Weighting", subtitle = "Required plotting fields were not available.", x = NULL, y = "Delta RMSE after component dropout") + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::filter(component != "full_model")
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Empirical Component Importance for Similarity Weighting", subtitle = "Required plotting fields were not available.", x = NULL, y = "Delta RMSE after component dropout") + ggplot2::theme_minimal(base_size = 11))
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
  # Apply optional relabeling and preserve the global component order so the
  # anchor-by-component heatmap lines up with the component-importance plots.
  plot_df <- tibble::as_tibble(dropout_tbl)
  if (nrow(plot_df) == 0 || !all(c("block", "anchor_species", "importance_score") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Anchor-Specific Local Dropout Sensitivity", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::mutate(
      block = dplyr::recode(
        as.character(block),
        !!!(block_labs %||% c()),
        length_coherence = "Length coherence",
        depth_coherence = "Depth coherence",
        frequency_coherence = "Frequency coherence",
        .default = stringr::str_to_title(stringr::str_replace_all(as.character(block), "_", " "))
      )
    )
  if ("component_rank_global" %in% names(plot_df)) {
    block_levels <- plot_df |>
      dplyr::arrange(component_rank_global, block) |>
      dplyr::distinct(block) |>
      dplyr::pull(block)
  } else {
    block_levels <- plot_df |>
      dplyr::group_by(block) |>
      dplyr::summarise(mean_importance = mean(importance_score, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(mean_importance), block) |>
      dplyr::pull(block)
  }

  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(block = factor(block, levels = block_levels)),
    ggplot2::aes(x = block, y = anchor_species, fill = importance_score)
  ) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08306b", na.value = "grey90") +
    ggplot2::scale_y_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
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
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "multiplier_pred", "multiplier_lo", "multiplier_hi", "selected_policy_display") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::filter(
      !is.na(anchor_species),
      is.finite(multiplier_pred),
      is.finite(multiplier_lo),
      is.finite(multiplier_hi),
      multiplier_pred > 0,
      multiplier_lo > 0,
      multiplier_hi > 0
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
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
    ggplot2::scale_x_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
    ggplot2::labs(
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
    !all(c("anchor_species", "multiplier_pred", "multiplier_lo", "multiplier_hi", "combined_multiplier_q05", "combined_multiplier_q50", "combined_multiplier_q95") %in% names(integrated_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Integrated Anchor-Level Biomass Multiplier Summary", subtitle = "Required plotting fields were not available.", x = NULL, y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
  }
  integrated_df$selected_policy_display <- resolve_selected_policy_names(integrated_df)
  anchor_levels <- integrated_df |>
    dplyr::group_by(anchor_species) |>
    dplyr::summarise(
      order_value = stats::median(multiplier_pred, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(order_value, anchor_species) |>
    dplyr::pull(anchor_species) |>
    unique()

  integrated_df <- integrated_df |>
    dplyr::filter(
      !is.na(anchor_species),
      is.finite(multiplier_pred),
      is.finite(multiplier_lo),
      is.finite(multiplier_hi),
      is.finite(combined_multiplier_q05),
      is.finite(combined_multiplier_q50),
      is.finite(combined_multiplier_q95),
      multiplier_pred > 0,
      multiplier_lo > 0,
      multiplier_hi > 0,
      combined_multiplier_q05 > 0,
      combined_multiplier_q50 > 0,
      combined_multiplier_q95 > 0
    ) |>
    dplyr::mutate(
      anchor_species = factor(anchor_species, levels = anchor_levels),
      x_pos = as.numeric(anchor_species),
      selected_policy_display = if ("selected_policy_display" %in% names(integrated_df)) {
        selected_policy_display
      } else {
        NA_character_
      }
    ) |>
    dplyr::group_by(anchor_species) |>
    dplyr::arrange(selected_policy_display, .by_group = TRUE) |>
    dplyr::mutate(
      policy_offset = if (dplyr::n() > 1) seq(-0.07, 0.07, length.out = dplyr::n()) else 0,
      selected_x_pos = x_pos - 0.12 + policy_offset
    ) |>
    dplyr::ungroup()
  if (nrow(integrated_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Integrated Anchor-Level Biomass Multiplier Summary", subtitle = "Required plotting fields were not available.", x = NULL, y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
  }
  red_df <- tibble::as_tibble(score_tbl)
  if (all(c("anchor_species", "admissible", "biomass_multiplier_if_replace") %in% names(red_df))) {
    red_df <- red_df |>
      dplyr::filter(admissible, is.finite(biomass_multiplier_if_replace), biomass_multiplier_if_replace > 0) |>
      dplyr::mutate(anchor_species = factor(anchor_species, levels = anchor_levels), x_pos = as.numeric(anchor_species) + 0.12)
  } else {
    red_df <- tibble::tibble(x_pos = numeric(), biomass_multiplier_if_replace = numeric())
  }
  blue_df <- tibble::as_tibble(interval_tbl)
  if (all(c("anchor_species", "valid_prediction", "multiplier_pred") %in% names(blue_df))) {
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
      ggplot2::aes(x = selected_x_pos, ymin = multiplier_lo, ymax = multiplier_hi),
      width = 0.12,
      colour = "#2166ac"
    ) +
    ggplot2::geom_point(ggplot2::aes(x = selected_x_pos, y = multiplier_pred), colour = "#2166ac", size = 2.5) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = combined_multiplier_q05, ymax = combined_multiplier_q95),
      width = 0.12,
      colour = "#b2182b",
      position = ggplot2::position_nudge(x = 0.12)
    ) +
    ggplot2::geom_point(ggplot2::aes(y = combined_multiplier_q50), colour = "#b2182b", size = 2.5, position = ggplot2::position_nudge(x = 0.12)) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_continuous(
      breaks = seq_along(anchor_levels),
      labels = function(x) {
        lab <- anchor_levels[match(x, seq_along(anchor_levels))]
        parse(text = paste0("italic('", lab, "')"))
      }
    ) +
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
    dplyr::left_join(
      model_agg |>
        dplyr::mutate(fao_area_chr = as.character(fao_area)),
      by = "fao_area_chr"
    ) |>
    dplyr::mutate(n = dplyr::coalesce(n, 0L)) |>
    sfheaders::sf_to_df(fill = TRUE) |>
    dplyr::group_by(fao_area_chr, sfg_id, multipolygon_id, polygon_id, linestring_id) |>
    dplyr::mutate(
      sequence_id = dplyr::row_number(),
      UID = paste0(fao_area_chr, "-", area_name, "-", sfg_id, "-", multipolygon_id, "-", polygon_id)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(UID, linestring_id, sequence_id)

  # Get non-empty FAO areas
  nonempty <- fao_df |>
    dplyr::filter(n > 0) |>
    dplyr::reframe(fao = unique(fao_area_chr))

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
    {
      \(.) cbind(sf::st_drop_geometry(.), sf::st_coordinates(.))
    }() |>
    dplyr::rename(longitude = X, latitude = Y) |>
    dplyr::left_join(model_agg |> dplyr::mutate(fao_area = as.character(fao_area)),
      by = "fao_area"
    )

  # Add polygons
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = fao_df,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        group = UID,
        subgroup = linestring_id,
        fill = n
      ),
      color = "black",
      linewidth = 0.5
    ) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(x = longitude, y = latitude, label = n),
      data = fao_labels,
      size = 4.0
    ) +
    scico::scale_fill_scico(
      palette = "imola",
      trans = "log2",
      na.value = "gray70"
    ) +
    ggplot2::labs(
      x = expression(Longitude ~ (degree)),
      y = expression(Latitude ~ (degree)),
      fill = bquote(italic(n)[.(count_type)])
    ) +
    ggplot2::coord_cartesian(expand = FALSE)
}

#' Plot TS panel ribbons
#'
#' @param curve_tbl Combined per-reference TS ribbon table.
#' @param reference_col Facet-label column.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' plot_ts_panel(panel_tbl)
#' }
#'
#' @export
plot_ts_panel <- function(curve_tbl,
                          reference_col = "anchor_species",
                          show_top_candidate = FALSE) {
  curve_tbl <- tibble::as_tibble(curve_tbl)

  # Return an empty placeholder plot when no per-reference TS ribbon tables
  # were available, rather than failing during the final summary stage.
  if (nrow(curve_tbl) == 0 || !reference_col %in% names(curve_tbl)) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          x = "Length (cm)",
          y = "TS (dB re 1 m^2)"
        ) +
        ggplot2::theme_minimal(base_size = 11)
    )
  }
  if (!all(c("length_cm", "ts_pred", "ts_anchor", "ts_lo_99", "ts_hi_99", "ts_lo_95", "ts_hi_95", "ts_lo_90", "ts_hi_90", "ts_lo_80", "ts_hi_80") %in% names(curve_tbl))) {
    return(
      ggplot2::ggplot() + ggplot2::labs(x = "Length (cm)", y = "TS (dB re 1 m^2)") + ggplot2::theme_minimal(base_size = 11)
    )
  }

  # Reshape the stored ribbon bounds into one long band table so the four
  # conformal intervals can be drawn as stacked ribbons.
  band_tbl <- dplyr::bind_rows(
    curve_tbl |>
      dplyr::transmute(!!reference_col := .data[[reference_col]], length_cm, ts_pred, ts_anchor, band = "99%", ymin = ts_lo_99, ymax = ts_hi_99),
    curve_tbl |>
      dplyr::transmute(!!reference_col := .data[[reference_col]], length_cm, ts_pred, ts_anchor, band = "95%", ymin = ts_lo_95, ymax = ts_hi_95),
    curve_tbl |>
      dplyr::transmute(!!reference_col := .data[[reference_col]], length_cm, ts_pred, ts_anchor, band = "90%", ymin = ts_lo_90, ymax = ts_hi_90),
    curve_tbl |>
      dplyr::transmute(!!reference_col := .data[[reference_col]], length_cm, ts_pred, ts_anchor, band = "80%", ymin = ts_lo_80, ymax = ts_hi_80)
  ) |>
    dplyr::mutate(band = factor(band, levels = c("99%", "95%", "90%", "80%")))

  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = band_tbl, ggplot2::aes(x = length_cm, ymin = ymin, ymax = ymax, fill = band), alpha = 0.28) +
    ggplot2::scale_fill_manual(values = c("99%" = "#dadaeb", "95%" = "#bcbddc", "90%" = "#9e9ac8", "80%" = "#756bb1"), name = "Prediction band") +
    ggplot2::geom_line(data = curve_tbl, ggplot2::aes(x = length_cm, y = ts_pred, colour = "Selected policy", linetype = "Selected policy"), linewidth = 0.85)
  if (isTRUE(show_top_candidate) && "ts_top_candidate" %in% names(curve_tbl)) {
    p <- p +
      ggplot2::geom_line(
        data = curve_tbl,
        ggplot2::aes(x = length_cm, y = ts_top_candidate, colour = "Top candidate", linetype = "Top candidate"),
        linewidth = 0.75,
        alpha = 0.9
      )
  }
  p <- p +
    ggplot2::geom_line(
      data = curve_tbl,
      ggplot2::aes(x = length_cm, y = ts_anchor),
      linewidth = 1.35,
      colour = "white",
      linetype = "longdash",
      alpha = 0.95
    ) +
    ggplot2::geom_line(
      data = curve_tbl,
      ggplot2::aes(x = length_cm, y = ts_anchor, colour = "Anchor", linetype = "Anchor"),
      linewidth = 0.75
    )
  colour_values <- c(
    "Selected policy" = "#2166ac",
    "Anchor" = "#1b1b1b"
  )
  linetype_values <- c(
    "Selected policy" = "solid",
    "Anchor" = "longdash"
  )
  if (isTRUE(show_top_candidate) && "ts_top_candidate" %in% names(curve_tbl)) {
    colour_values <- c(colour_values, "Top candidate" = "#7f2704")
    linetype_values <- c(linetype_values, "Top candidate" = "dotdash")
  }

  p +
    ggplot2::scale_colour_manual(values = colour_values, name = "Curve") +
    ggplot2::scale_linetype_manual(values = linetype_values, name = "Curve") +
    ggplot2::facet_wrap(stats::as.formula(paste("~", reference_col)), ncol = 2, scales = "free_x") +
    ggplot2::labs(
      x = "Length (cm)",
      y = "TS (dB re 1 m^2)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "italic"))
}

#' Plot selected-policy coefficient intervals
#'
#' @param coefficient_tbl Selected-policy table with coefficient interval columns.
#'
#' @return A ggplot object.
#'
#' @export
plot_policy_coefficients <- function(coefficient_tbl) {
  plot_df <- tibble::as_tibble(coefficient_tbl)
  resolve_one <- function(df, candidates) {
    for (nm in candidates) {
      if (nm %in% names(df)) {
        return(df[[nm]])
      }
    }
    rep(NA_real_, nrow(df))
  }
  if (nrow(plot_df) == 0 || !"anchor_species" %in% names(plot_df)) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df$slope_estimate <- resolve_one(plot_df, c("policy_slope_len"))
  plot_df$slope_lo <- resolve_one(plot_df, c("policy_slope_len_lo_95", "policy_slope_len_lo_95.x", "policy_slope_len_lo_95.y"))
  plot_df$slope_hi <- resolve_one(plot_df, c("policy_slope_len_hi_95", "policy_slope_len_hi_95.x", "policy_slope_len_hi_95.y"))
  plot_df$intercept_estimate <- resolve_one(plot_df, c("policy_intercept_len"))
  plot_df$intercept_lo <- resolve_one(plot_df, c("policy_intercept_len_lo_95", "policy_intercept_len_lo_95.x", "policy_intercept_len_lo_95.y"))
  plot_df$intercept_hi <- resolve_one(plot_df, c("policy_intercept_len_hi_95", "policy_intercept_len_hi_95.x", "policy_intercept_len_hi_95.y"))
  if (!all(c("slope_estimate", "slope_lo", "slope_hi", "intercept_estimate", "intercept_lo", "intercept_hi") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }
  long_df <- dplyr::bind_rows(
    plot_df |>
      dplyr::transmute(
        anchor_species,
        parameter = "Slope",
        estimate = slope_estimate,
        lo = slope_lo,
        hi = slope_hi
      ),
    plot_df |>
      dplyr::transmute(
        anchor_species,
        parameter = "Intercept",
        estimate = intercept_estimate,
        lo = intercept_lo,
        hi = intercept_hi
      )
  ) |>
    dplyr::filter(
      !is.na(anchor_species),
      is.finite(estimate),
      is.finite(lo),
      is.finite(hi)
    ) |>
    dplyr::mutate(
      anchor_species = factor(anchor_species, levels = unique(plot_df$anchor_species))
    )
  if (nrow(long_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(long_df, ggplot2::aes(y = anchor_species, x = estimate, xmin = lo, xmax = hi)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_errorbar(orientation = "y", width = 0.16, linewidth = 0.7, colour = "#4d4d4d") +
    ggplot2::geom_point(size = 2.4, colour = "#2166ac") +
    ggplot2::facet_wrap(~parameter, ncol = 1, scales = "free_x") +
    ggplot2::scale_y_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
    ggplot2::labs(
      x = "Coefficient value",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot selected biomass multiplier against expected anchor length
#'
#' @param anchor_tbl Selected-policy table with multiplier intervals and expected length.
#'
#' @return A ggplot object.
#'
#' @export
plot_multiplier_vs_expected_length <- function(anchor_tbl) {
  plot_df <- tibble::as_tibble(anchor_tbl)
  required_cols <- c(
    "anchor_species",
    "expected_length_cm",
    "multiplier_pred",
    "meta_post_selection_multiplier_lo",
    "meta_post_selection_multiplier_hi"
  )
  if (nrow(plot_df) == 0 || !all(required_cols %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Selected Biomass Multiplier vs Expected Anchor Length", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::filter(
      !is.na(anchor_species) & nzchar(anchor_species),
      is.finite(expected_length_cm),
      is.finite(multiplier_pred),
      is.finite(meta_post_selection_multiplier_lo),
      is.finite(meta_post_selection_multiplier_hi),
      meta_post_selection_multiplier_lo > 0,
      meta_post_selection_multiplier_hi > 0,
      multiplier_pred > 0
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Selected Biomass Multiplier vs Expected Anchor Length", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = expected_length_cm,
      y = multiplier_pred,
      ymin = meta_post_selection_multiplier_lo,
      ymax = meta_post_selection_multiplier_hi,
      label = anchor_species
    )
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_errorbar(width = 0.18, linewidth = 0.7, colour = "#6b6b6b") +
    ggplot2::geom_point(size = 2.6, colour = "#7f2704") +
    ggplot2::geom_text(vjust = -0.7, size = 3.1, check_overlap = TRUE) +
    ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::labs(
      title = "Selected Biomass Multiplier vs Expected Anchor Length",
      x = "Expected anchor length (cm)",
      y = "Biomass multiplier"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot admissible candidate multipliers and selected policy multipliers
#'
#' @param candidate_tbl Anchor candidate-score table with candidate-model multipliers.
#' @param selected_tbl Selected-policy table.
#'
#' @return A ggplot object.
#'
#' @export
plot_multiplier_candidate_lines <- function(candidate_tbl,
                                            selected_tbl) {
  candidate_tbl <- tibble::as_tibble(candidate_tbl)
  selected_tbl <- tibble::as_tibble(selected_tbl)
  candidate_needed <- c("anchor_model_id", "anchor_species", "biomass_multiplier_if_replace")
  selected_needed <- c("anchor_model_id", "anchor_species", "multiplier_pred")
  if (nrow(candidate_tbl) == 0 || nrow(selected_tbl) == 0 ||
    !all(candidate_needed %in% names(candidate_tbl)) ||
    !all(selected_needed %in% names(selected_tbl))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Candidate Model and Selected Policy Biomass Multipliers", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }

  candidate_plot <- candidate_tbl |>
    dplyr::filter(
      is.finite(biomass_multiplier_if_replace),
      biomass_multiplier_if_replace > 0
    ) |>
    dplyr::transmute(
      anchor_model_id = as.character(anchor_model_id),
      anchor_species = as.character(anchor_species),
      multiplier = biomass_multiplier_if_replace,
      line_type = "Candidate models"
    )

  selected_plot <- selected_tbl |>
    dplyr::filter(
      is.finite(multiplier_pred),
      multiplier_pred > 0
    ) |>
    dplyr::transmute(
      anchor_model_id = as.character(anchor_model_id),
      anchor_species = as.character(anchor_species),
      multiplier = multiplier_pred,
      line_type = "Selected policies"
    )

  plot_df <- dplyr::bind_rows(candidate_plot, selected_plot)
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Candidate Model and Selected Policy Biomass Multipliers", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(plot_df) +
    ggplot2::geom_segment(
      data = dplyr::filter(plot_df, line_type == "Candidate models"),
      ggplot2::aes(x = multiplier, xend = multiplier, y = 0, yend = 1),
      colour = "#b2182b",
      alpha = 0.30,
      linewidth = 0.45
    ) +
    ggplot2::geom_segment(
      data = dplyr::filter(plot_df, line_type == "Selected policies"),
      ggplot2::aes(x = multiplier, xend = multiplier, y = 0, yend = 1),
      colour = "#2166ac",
      alpha = 0.95,
      linewidth = 1.05
    ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey45") +
    ggplot2::scale_x_log10(labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL) +
    ggplot2::facet_wrap(~anchor_species, ncol = 2, scales = "free_x") +
    ggplot2::labs(
      title = "Candidate Model and Selected Policy Biomass Multipliers",
      subtitle = "Red lines are admissible candidate-model replacements; blue lines are the selected policy multiplier(s).",
      x = "Biomass multiplier relative to reference anchor",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 8, colour = "grey35"),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' Plot length-specific biomass multiplier spectrum
#'
#' @param curve_tbl TS panel table with anchor and selected-policy curves.
#' @param reference_col Facet-label column.
#'
#' @return A ggplot object.
#'
#' @export
plot_multiplier_length_spectrum <- function(curve_tbl,
                                            reference_col = "anchor_species") {
  curve_tbl <- tibble::as_tibble(curve_tbl)
  needed <- c(reference_col, "length_cm", "ts_anchor", "ts_pred", "ts_lo_95", "ts_hi_95")
  if (nrow(curve_tbl) == 0 || !all(needed %in% names(curve_tbl))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "Length (cm)", y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- curve_tbl |>
    dplyr::mutate(
      multiplier_at_length = 10^((ts_anchor - ts_pred) / 10),
      multiplier_lo_95 = 10^((ts_anchor - ts_hi_95) / 10),
      multiplier_hi_95 = 10^((ts_anchor - ts_lo_95) / 10)
    ) |>
    dplyr::filter(
      is.finite(multiplier_at_length),
      is.finite(multiplier_lo_95),
      is.finite(multiplier_hi_95),
      multiplier_at_length > 0,
      multiplier_lo_95 > 0,
      multiplier_hi_95 > 0
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "Length (cm)", y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = length_cm, y = multiplier_at_length)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = multiplier_lo_95, ymax = multiplier_hi_95), fill = "#9ebcda", alpha = 0.30) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey45") +
    ggplot2::geom_line(colour = "#08519c", linewidth = 0.9) +
    ggplot2::scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", reference_col)), ncol = 2, scales = "free_x") +
    ggplot2::labs(
      x = "Length (cm)",
      y = "Biomass multiplier"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "italic"))
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
  plot_df$is_selected <- dplyr::coalesce(
    if ("is_selected.y" %in% names(plot_df)) as.logical(plot_df$is_selected.y) else rep(NA, nrow(plot_df)),
    if ("is_selected" %in% names(plot_df)) as.logical(plot_df$is_selected) else rep(NA, nrow(plot_df)),
    if ("is_selected.x" %in% names(plot_df)) as.logical(plot_df$is_selected.x) else rep(NA, nrow(plot_df)),
    FALSE
  )
  plot_df$policy_display <- resolve_policy_display_names(plot_df)
  if (nrow(plot_df) == 0 || !all(c("policy_display", "multiplier_pred", "multiplier_lo", "multiplier_hi", "is_selected") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::filter(
      is.finite(multiplier_pred),
      is.finite(multiplier_lo),
      is.finite(multiplier_hi),
      multiplier_pred > 0,
      multiplier_lo > 0,
      multiplier_hi > 0
    ) |>
    dplyr::arrange(multiplier_pred, policy_display)
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
  }
  policy_levels <- unique(plot_df$policy_display)
  plot_df$policy_display <- factor(plot_df$policy_display, levels = rev(policy_levels))
  background_df <- plot_df |>
    dplyr::filter(!is_selected)
  selected_df <- plot_df |>
    dplyr::filter(is_selected)

  ggplot2::ggplot(plot_df, ggplot2::aes(y = policy_display)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_segment(
      data = background_df,
      ggplot2::aes(
        x = multiplier_lo,
        xend = multiplier_hi,
        yend = policy_display
      ),
      linewidth = 0.75,
      colour = "grey70",
      alpha = 0.65
    ) +
    ggplot2::geom_point(
      data = background_df,
      ggplot2::aes(x = multiplier_pred),
      size = 2.4,
      colour = "grey60",
      alpha = 0.75
    ) +
    ggplot2::geom_segment(
      data = selected_df,
      ggplot2::aes(
        x = multiplier_lo,
        xend = multiplier_hi,
        yend = policy_display
      ),
      linewidth = 1.05,
      colour = "#2166ac"
    ) +
    ggplot2::geom_point(
      data = selected_df,
      ggplot2::aes(x = multiplier_pred),
      size = 3.1,
      colour = "#2166ac"
    ) +
    ggplot2::scale_x_log10(labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::labs(
      x = "Biomass multiplier",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11)
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
  plot_df$is_selected <- dplyr::coalesce(
    if ("is_selected.y" %in% names(plot_df)) as.logical(plot_df$is_selected.y) else rep(NA, nrow(plot_df)),
    if ("is_selected" %in% names(plot_df)) as.logical(plot_df$is_selected) else rep(NA, nrow(plot_df)),
    if ("is_selected.x" %in% names(plot_df)) as.logical(plot_df$is_selected.x) else rep(NA, nrow(plot_df)),
    FALSE
  )
  plot_df$policy_display <- resolve_policy_display_names(plot_df)
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "policy_display", "multiplier_pred", "multiplier_lo", "multiplier_hi", "is_selected") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::filter(
      !is.na(anchor_species),
      is.finite(multiplier_pred),
      is.finite(multiplier_lo),
      is.finite(multiplier_hi),
      multiplier_pred > 0,
      multiplier_lo > 0,
      multiplier_hi > 0
    ) |>
    dplyr::ungroup()
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(x = NULL, y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
  }
  policy_levels <- plot_df |>
    dplyr::group_by(policy_display) |>
    dplyr::summarise(order_value = stats::median(multiplier_pred, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(order_value, policy_display) |>
    dplyr::pull(policy_display)
  plot_df <- plot_df |>
    dplyr::mutate(
      policy_display = factor(policy_display, levels = rev(policy_levels)),
      anchor_species = factor(anchor_species, levels = unique(anchor_species))
    )
  background_df <- plot_df |>
    dplyr::filter(!is_selected)
  selected_df <- plot_df |>
    dplyr::filter(is_selected)

  ggplot2::ggplot(plot_df, ggplot2::aes(y = policy_display)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_segment(
      data = background_df,
      ggplot2::aes(
        x = multiplier_lo,
        xend = multiplier_hi,
        yend = policy_display
      ),
      linewidth = 0.65,
      colour = "grey70",
      alpha = 0.65
    ) +
    ggplot2::geom_point(
      data = background_df,
      ggplot2::aes(x = multiplier_pred),
      size = 2.1,
      colour = "grey60",
      alpha = 0.75
    ) +
    ggplot2::geom_segment(
      data = selected_df,
      ggplot2::aes(
        x = multiplier_lo,
        xend = multiplier_hi,
        yend = policy_display
      ),
      linewidth = 0.95,
      colour = "#2166ac"
    ) +
    ggplot2::geom_point(
      data = selected_df,
      ggplot2::aes(x = multiplier_pred),
      size = 2.8,
      colour = "#2166ac"
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(anchor_species)) +
    ggplot2::scale_x_log10(labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::labs(
      x = "Biomass multiplier",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(face = "italic"),
      axis.text.y = ggplot2::element_text(size = 8),
      panel.spacing.x = grid::unit(0.4, "lines")
    )
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
  # Encode categorical policy-selection changes so this plot is distinct from
  # the numeric multiplier-drift heatmap.
  plot_df <- tibble::as_tibble(sens_tbl)
  if (nrow(plot_df) == 0 || !"scenario" %in% names(plot_df) || !"anchor_species" %in% names(plot_df)) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Representative Policy Stability and Multiplier Drift", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
  }
  if (!is.null(scenario_labs)) {
    plot_df <- plot_df |>
      dplyr::mutate(scenario_label = dplyr::recode(scenario, !!!scenario_labs))
  } else {
    plot_df <- plot_df |>
      dplyr::mutate(scenario_label = scenario)
  }
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

  anchor_order <- unique(c(
    as.character(tibble::as_tibble(baseline_tbl)$anchor_species),
    as.character(plot_df$anchor_species)
  ))
  anchor_order <- anchor_order[!is.na(anchor_order) & nzchar(anchor_order)]
  plot_df <- plot_df |>
    dplyr::mutate(
      scenario_label = factor(as.character(scenario_label), levels = unique(as.character(scenario_label))),
      anchor_species = factor(as.character(anchor_species), levels = anchor_order),
      equiv_change = dplyr::coalesce(equivalent_set_changed, equiv_set_changed, FALSE),
      stability_state = dplyr::case_when(
        as.character(scenario_status) != "ok" ~ "Scenario failed",
        dplyr::coalesce(policy_changed, FALSE) ~ "Selected policy changed",
        dplyr::coalesce(display_changed, FALSE) ~ "Displayed strategy changed",
        dplyr::coalesce(equiv_change, FALSE) ~ "Equivalent set changed",
        TRUE ~ "Selection unchanged"
      ),
      stability_state = factor(
        stability_state,
        levels = c(
          "Selection unchanged",
          "Equivalent set changed",
          "Displayed strategy changed",
          "Selected policy changed",
          "Scenario failed"
        )
      )
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = scenario_label, y = anchor_species)) +
    ggplot2::geom_tile(ggplot2::aes(fill = stability_state), colour = "white", linewidth = 0.6) +
    ggplot2::scale_fill_manual(
      values = c(
        "Selection unchanged" = "#d9d9d9",
        "Equivalent set changed" = "#9ecae1",
        "Displayed strategy changed" = "#6baed6",
        "Selected policy changed" = "#2171b5",
        "Scenario failed" = "#cb181d"
      ),
      drop = FALSE
    ) +
    ggplot2::scale_y_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
    ggplot2::labs(
      title = "Representative Policy Stability and Multiplier Drift",
      x = NULL,
      y = NULL,
      fill = "Selection status"
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
    return(ggplot2::ggplot() + ggplot2::labs(title = "Multiplier Sensitivity Relative to Baseline", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
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
  anchor_order <- unique(c(
    as.character(tibble::as_tibble(baseline_tbl)$anchor_species),
    as.character(plot_df$anchor_species)
  ))
  anchor_order <- anchor_order[!is.na(anchor_order) & nzchar(anchor_order)]
  plot_df <- plot_df |>
    dplyr::left_join(base_df, by = "anchor_model_id") |>
    dplyr::mutate(
      scenario_label = factor(as.character(scenario_label), levels = unique(as.character(scenario_label))),
      anchor_species = factor(as.character(anchor_species), levels = anchor_order),
      delta_log_multiplier = log(multiplier_pred / baseline_multiplier),
      label = sprintf("%+.2f", delta_log_multiplier)
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = scenario_label, y = anchor_species, fill = delta_log_multiplier)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, labels = scales::number_format(accuracy = 0.01)) +
    ggplot2::scale_y_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
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
  if (nrow(plot_df) == 0 || !all(c("value", "scenario_label", "metric", "panel") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Sensitivity Scenario Summary", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11))
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
  if (nrow(plot_df) == 0 || !all(c(block_col, "mean_multiplier", "q05_multiplier", "q95_multiplier", "sd_multiplier") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Tuning Block Multipliers Across Resamples", subtitle = "Required plotting fields were not available.", x = "Block multiplier", y = NULL) + ggplot2::theme_minimal(base_size = 11))
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
      ggplot2::ggplot() + ggplot2::labs(title = "Representative Policy Audit", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 11)
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
  if (nrow(plot_df) == 0 || !all(c("field", "missing_fraction") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Missingness Across Key Workflow Metadata Fields", subtitle = "Required plotting fields were not available.", x = "Missing fraction", y = NULL) + ggplot2::theme_minimal(base_size = 11))
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
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "prop_fail_missing_metadata") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Candidate Exclusion from Missing Key Metadata", subtitle = "Required plotting fields were not available.", x = "Excluded by missingness gate", y = NULL) + ggplot2::theme_minimal(base_size = 11))
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
  if (nrow(point_df) == 0 || !all(c("MDS1", "MDS2") %in% names(point_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "NMDS1", y = "NMDS2") + ggplot2::theme_minimal(base_size = 11))
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
      ggplot2::aes(x = MDS1, y = MDS2, fill = .data[[cluster_name]], group = .data[[cluster_name]]),
      inherit.aes = FALSE,
      alpha = 0.12,
      colour = NA
    ) +
    ggplot2::geom_path(
      data = hull_df,
      ggplot2::aes(x = MDS1, y = MDS2, colour = .data[[cluster_name]], group = .data[[cluster_name]]),
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
      fontface = "italic",
      nudge_y = 0.03 * scale_ref,
      check_overlap = TRUE
    ) +
    ggplot2::scale_colour_brewer(palette = "Dark2", name = "Cluster") +
    ggplot2::scale_fill_brewer(palette = "Dark2", name = "Cluster") +
    ggplot2::labs(x = "NMDS1", y = "NMDS2") +
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
  if (nrow(plot_df) == 0 || !all(c("length_cm", "f_len") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "Length (cm)", y = "f(L)") + ggplot2::theme_minimal(base_size = 11))
  }
  ggplot2::ggplot(plot_df, ggplot2::aes(x = length_cm, y = f_len)) +
    ggplot2::geom_line(linewidth = 0.8, colour = "#3182bd") +
    ggplot2::labs(
      x = "Length (cm)",
      y = "f(L)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot a panel of anchor length-density curves
#'
#' @param length_tbl Bound anchor length-density table.
#' @param reference_col Facet-label column.
#'
#' @return A ggplot object.
#'
#' @export
plot_length_density_panel <- function(length_tbl,
                                      reference_col = "anchor_species") {
  plot_df <- tibble::as_tibble(length_tbl)
  if (nrow(plot_df) == 0 || !all(c(reference_col, "length_cm", "f_len") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "Length (cm)", y = "f(L)") + ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = length_cm, y = f_len)) +
    ggplot2::geom_line(linewidth = 0.8, colour = "#3182bd") +
    ggplot2::facet_wrap(stats::as.formula(paste("~", reference_col)), ncol = 2, scales = "free_x") +
    ggplot2::labs(
      x = "Length (cm)",
      y = "f(L)"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "italic"))
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
  if (nrow(plot_df) == 0 || !all(c("length_cm", "ts_mean") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = paste0("Weighted TS Ribbon [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Length (cm)", y = "TS (dB re 1 m^2)") + ggplot2::theme_minimal(base_size = 11))
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
  # the raw combined kernel weight.
  plot_df <- tibble::as_tibble(weight_tbl)
  if (!("w_adm" %in% names(plot_df) || "w_combined" %in% names(plot_df)) ||
    !("combined_distance" %in% names(plot_df) || "d_species" %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "Distance to reference", y = "Model weight") + ggplot2::theme_minimal(base_size = 11))
  }
  if (!"w_adm" %in% names(plot_df)) plot_df$w_adm <- NA_real_
  if (!"w_combined" %in% names(plot_df)) plot_df$w_combined <- NA_real_
  if (!"combined_distance" %in% names(plot_df)) plot_df$combined_distance <- NA_real_
  if (!"d_species" %in% names(plot_df)) plot_df$d_species <- NA_real_
  plot_df <- plot_df |>
    dplyr::mutate(
      plot_weight = dplyr::coalesce(w_adm, w_combined),
      plot_distance = dplyr::coalesce(combined_distance, d_species)
    ) |>
    dplyr::filter(is.finite(plot_weight), is.finite(plot_distance), plot_weight >= 0)
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "Distance to reference", y = "Model weight") + ggplot2::theme_minimal(base_size = 11))
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
    return(ggplot2::ggplot() + ggplot2::labs(title = paste0("Biomass Sensitivity [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Biomass multiplier relative to the reference model", y = "Weighted model count") + ggplot2::theme_minimal(base_size = 11))
  }
  if (!"w_adm" %in% names(plot_df)) plot_df$w_adm <- NA_real_
  if (!"w_combined" %in% names(plot_df)) plot_df$w_combined <- NA_real_
  plot_df <- plot_df |>
    dplyr::mutate(plot_weight = dplyr::coalesce(w_adm, w_combined, 0)) |>
    dplyr::filter(
      is.finite(biomass_multiplier_if_replace),
      biomass_multiplier_if_replace > 0,
      is.finite(plot_weight),
      plot_weight > 0
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = paste0("Biomass Sensitivity [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Biomass multiplier relative to the reference model", y = "Weighted model count") + ggplot2::theme_minimal(base_size = 11))
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
      ggplot2::geom_vline(xintercept = summary_tbl$combined_consensus_multiplier[[1]], colour = "#b2182b", linewidth = 1.2) +
      ggplot2::geom_vline(
        xintercept = c(summary_tbl$combined_multiplier_q05[[1]], summary_tbl$combined_multiplier_q95[[1]]),
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
    !("w_adm" %in% names(plot_df) || "w_combined" %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "Model weight", y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
  }
  if (!"w_adm" %in% names(plot_df)) plot_df$w_adm <- NA_real_
  if (!"w_combined" %in% names(plot_df)) plot_df$w_combined <- NA_real_
  plot_df <- plot_df |>
    dplyr::mutate(plot_weight = dplyr::coalesce(w_adm, w_combined)) |>
    dplyr::filter(
      is.finite(plot_weight),
      plot_weight > 0,
      is.finite(biomass_multiplier_if_replace),
      biomass_multiplier_if_replace > 0
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(x = "Model weight", y = "Biomass multiplier") + ggplot2::theme_minimal(base_size = 11))
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
      x = "Model weight",
      y = "Biomass multiplier",
      colour = "Swimbladder type"
    ) +
    ggplot2::theme_minimal(base_size = 12)
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
  if (nrow(plot_df) == 0 || !all(c("species_name", "model_id_chr", "w_adm") %in% names(plot_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = paste0("Top-10 Models by Weight [", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = NULL, y = "Model weight") + ggplot2::theme_minimal(base_size = 11))
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
  if (nrow(profile_df) == 0 || nrow(summary_df) == 0 || !all(c("length_cm", "weighted_variance_v") %in% names(profile_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = paste0("Pivot-Point Variance Profile [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Length (cm)", y = "Weighted variance V(L)") + ggplot2::theme_minimal(base_size = 11))
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
  if (nrow(pairwise_df) == 0 || nrow(summary_df) == 0 || !all(c("lpivot_cm", "pair_weight") %in% names(pairwise_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = paste0("Pairwise Pivot-Length Distribution [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Pairwise pivot length (cm)", y = "Weighted pair count") + ggplot2::theme_minimal(base_size = 11))
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
  if (nrow(profile_df) == 0 || nrow(summary_df) == 0 || !all(c("length_cm", "lambda_l") %in% names(profile_df))) {
    return(ggplot2::ggplot() + ggplot2::labs(title = paste0("Biological Leverage Profile [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Length (cm)", y = "Biological leverage") + ggplot2::theme_minimal(base_size = 11))
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



