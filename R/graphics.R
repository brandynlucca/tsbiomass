#' Resolve one default label for a `Conjurer` metric
#'
#' @param metric Metric name.
#'
#' @return Character scalar.
#'
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
conjurer_trait_labels <- function(traits) {
  # Humanize the stored trait names without changing their underlying keys.
  traits_ <- unique(as.character(traits))
  traits_ <- traits_[!is.na(traits_) & nzchar(traits_)]
  labels <- stringr::str_replace_all(traits_, "_", " ")
  labels <- stringr::str_replace(labels, "^study ", "")
  labels <- stringr::str_to_title(labels)
  stats::setNames(labels, traits_)
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
#' @noRd
save_plot_if_possible <- function(filename,
                                  plot,
                                  width,
                                  height,
                                  dpi = 450,
                                  ...) {
  save_args <- list(...)
  plot_ <- if (inherits(plot, "ggplot")) {
    plot_tmp <- plot +
      ggplot2::labs(title = NULL, subtitle = NULL) +
      ggplot2::theme(
        plot.title = ggplot2::element_blank(),
        plot.subtitle = ggplot2::element_blank()
      )
    # Freeze ggplot objects to grobs before saving so repeated scripted
    # renders cannot accidentally reuse later plot state across files.
    ggplot2::ggplotGrob(plot_tmp)
  } else {
    plot
  }
  ok <- tryCatch(
    {
      if (grepl("\\.png$", filename, ignore.case = TRUE) &&
        is.null(save_args$device) &&
        requireNamespace("ragg", quietly = TRUE)) {
        save_args$device <- ragg::agg_png
      }
      do.call(
        ggplot2::ggsave,
        c(list(
          filename = filename,
          plot = plot_,
          width = width,
          height = height,
          dpi = dpi
        ), save_args)
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

#' Stack a custom legend above a plot grob
#'
#' @param plot A ggplot or grob.
#' @param legend_grob A grob to place above the plot.
#' @param legend_height Relative height allocated to the legend strip.
#'
#' @return A grob tree.
#' @keywords internal
#' @noRd
stack_plot_with_top_grob <- function(plot,
                                     legend_grob,
                                     legend_height = 0.16) {
  plot_grob <- if (inherits(plot, "ggplot")) {
    ggplot2::ggplotGrob(plot)
  } else {
    plot
  }
  plot_height <- 1 - legend_height
  grid::grobTree(
    grid::grobTree(
      legend_grob,
      vp = grid::viewport(
        x = 0.5,
        y = 1 - legend_height / 2,
        width = 1,
        height = legend_height,
        just = c("center", "center")
      )
    ),
    grid::grobTree(
      plot_grob,
      vp = grid::viewport(
        x = 0.5,
        y = plot_height / 2,
        width = 1,
        height = plot_height,
        just = c("center", "center")
      )
    )
  )
}

#' Build the custom biomass-change legend grob
#'
#' @return A grob tree.
#' @keywords internal
#' @noRd
biomass_change_legend_grob <- function() {
  black <- "#1b1b1b"
  red <- "#b2182b"
  light_red <- scales::alpha(red, 0.25)
  txt <- function(label, x, y, just = "left", fontsize = 14) {
    grid::textGrob(
      label,
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      just = just,
      gp = grid::gpar(col = "black", fontsize = fontsize)
    )
  }
  pt <- function(x, y, col, alpha = 1, size = 2.2) {
    grid::pointsGrob(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      pch = 16,
      size = grid::unit(size, "mm"),
      gp = grid::gpar(col = scales::alpha(col, alpha))
    )
  }
  seg <- function(x0, x1, y, col, lwd = 2.5, alpha = 1) {
    grid::segmentsGrob(
      x0 = grid::unit(x0, "npc"),
      x1 = grid::unit(x1, "npc"),
      y0 = grid::unit(y, "npc"),
      y1 = grid::unit(y, "npc"),
      gp = grid::gpar(col = scales::alpha(col, alpha), lwd = lwd, lineend = "butt")
    )
  }
  err <- function(x0, x1, y, col, lwd = 0.8, cap = 0.06) {
    grid::grobTree(
      grid::segmentsGrob(
        x0 = grid::unit(x0, "npc"),
        x1 = grid::unit(x1, "npc"),
        y0 = grid::unit(y, "npc"),
        y1 = grid::unit(y, "npc"),
        gp = grid::gpar(col = col, lwd = lwd, lineend = "butt")
      ),
      grid::segmentsGrob(
        x0 = grid::unit(c(x0, x1), "npc"),
        x1 = grid::unit(c(x0, x1), "npc"),
        y0 = grid::unit(c(y - cap, y - cap), "npc"),
        y1 = grid::unit(c(y + cap, y + cap), "npc"),
        gp = grid::gpar(col = col, lwd = lwd)
      )
    )
  }
  grid::grobTree(
    grid::rectGrob(
      x = 0.5,
      y = 0.5,
      width = 1,
      height = 1,
      gp = grid::gpar(fill = "white", col = NA)
    ),
    pt(0.06, 0.79, black, 0.55, 2.1),
    txt("Admissible models", 0.064, 0.79, fontsize = 14.5),
    seg(0.34, 0.40, 0.79, red, 1.8, 1),
    pt(0.37, 0.79, red, 1, 2.3),
    txt("Selected strategy", 0.405, 0.79, fontsize = 14.5),
    txt("Admissible model quantiles", 0.058, 0.49, fontsize = 14.5),
    pt(0.34, 0.49, red, 0.25, 2.1),
    txt("All tested strategies", 0.352, 0.49, fontsize = 14.5),
    seg(0.09, 0.122, 0.24, black, 3.0, 1),
    txt("80%", 0.106, 0.11, just = "center", fontsize = 13.0),
    seg(0.132, 0.164, 0.24, black, 1.5, 0.90),
    txt("90%", 0.152, 0.11, just = "center", fontsize = 13.0),
    err(0.178, 0.210, 0.24, black, 1.0, 0.032),
    txt("95%", 0.198, 0.11, just = "center", fontsize = 13.0)
  )
}

#' Plot uncertainty blocks
#'
#' @param dropout_tbl Uncertainty dropout table.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
plot_uncertainty_blocks <- function(dropout_tbl,
                                    anchor_label) {
  # Convert the block labels to a plotting order before building the chart so
  # the most important blocks appear first.
  plot_df <- tibble::as_tibble(dropout_tbl)
  if (nrow(plot_df) == 0 ||
    !all(c("block", "importance_score", "delta_log_spread") %in%
      names(plot_df))) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          title = paste0("Local Dropout Sensitivity [", anchor_label, "]"),
          subtitle = "Required plotting fields were not available.",
          x = NULL,
          y = "Heuristic importance"
        ) +
        ggplot2::theme_minimal(base_size = 11)
    )
  }
  plot_df <- plot_df |>
    dplyr::mutate(block = factor(.data$block, levels = rev(unique(.data$block))))
  plot_df$block <- dplyr::recode(
    as.character(plot_df$block),
    length_coherence = "Length coherence",
    depth_coherence = "Depth coherence",
    frequency_coherence = "Frequency coherence",
    .default = snake_title(as.character(plot_df$block))
  )
  if ("component_rank_global" %in% names(plot_df)) {
    block_levels <- plot_df |>
      dplyr::arrange(.data$component_rank_global, .data$block) |>
      dplyr::distinct(.data$block) |>
      dplyr::pull(.data$block)
  } else {
    block_levels <- plot_df |>
      dplyr::arrange(.data$importance_score, .data$block) |>
      dplyr::pull(.data$block) |>
      unique()
  }
  plot_df$block <- factor(plot_df$block, levels = rev(block_levels))

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$block,
      y = .data$importance_score,
      fill = .data$delta_log_spread
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_gradient2(
      low = "#2166ac",
      mid = "#f7f7f7",
      high = "#b2182b",
      midpoint = 0
    ) +
    ggplot2::labs(
      title = paste0("Local Dropout Sensitivity [", anchor_label, "]"),
      subtitle = paste(
        "Composite from block-level changes in spread, consensus,",
        "and admissible support."
      ),
      x = NULL,
      y = "Heuristic importance",
      fill = "Delta log-spread"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40")
    )
}

#' Plot the admissible similarity map
#'
#' @param map_tbl Admissible candidate table with distance and weight columns.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
plot_similarity_map <- function(map_tbl,
                                anchor_label) {
  # Build the point map from the already filtered admissible donor table so the
  # plot function does only plotting work.
  plot_df <- tibble::as_tibble(map_tbl)
  if (nrow(plot_df) == 0 ||
    !all(c(
      "d_species",
      "d_study",
      "w_combined",
      "overlap_same_species"
    ) %in% names(plot_df))) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          title = paste0("Admissible Similarity Map [", anchor_label, "]"),
          subtitle = "Required plotting fields were not available.",
          x = "Species dissimilarity",
          y = "Study dissimilarity"
        ) +
        ggplot2::theme_minimal(base_size = 11)
    )
  }
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$d_species,
      y = .data$d_study,
      size = .data$w_combined,
      colour = .data$overlap_same_species
    )
  ) +
    ggplot2::geom_point(alpha = 0.75) +
    ggplot2::scale_colour_manual(
      values = c("TRUE" = "#b2182b", "FALSE" = "#2166ac")
    ) +
    ggplot2::labs(
      title = paste0("Admissible Similarity Map [", anchor_label, "]"),
      subtitle = paste(
        "Species vs study dissimilarity among admissible",
        "candidate models."
      ),
      x = "Species dissimilarity",
      y = "Study dissimilarity",
      size = "Combined kernel weight",
      colour = "Same species"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40")
    )
}

#' Plot top candidate weights
#'
#' @param top_tbl Ranked top-candidate table.
#' @param anchor_label Anchor label used in the title.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
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
  if (!"model_id" %in% names(plot_df)) {
    plot_df$model_id <- seq_len(nrow(plot_df))
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
      is.finite(.data$w_adm),
      .data$w_adm > 0,
      !is.na(.data$model_id)
    ) |>
    dplyr::arrange(dplyr::desc(.data$w_adm), .data$combined_distance) |>
    dplyr::slice_head(n = 10L) |>
    dplyr::mutate(
      species_label = dplyr::case_when(
        !is.na(.data$species_name) &
          nzchar(.data$species_name) &
          .data$species_name != "NA NA" ~ .data$species_name,
        !is.na(.data$genus) & nzchar(.data$genus) & .data$genus != "NA" &
          !is.na(.data$species) &
          .data$species %in% c("NA", "sp", "sp.", "spp", "spp.") ~
          paste0(.data$genus, " sp."),
        !is.na(.data$genus) &
          nzchar(.data$genus) &
          .data$genus != "NA" ~ paste0(.data$genus, " sp."),
        TRUE ~ "Generic"
      ),
      species_expr = gsub("'", "\\\\'", .data$species_label, fixed = TRUE),
      candidate_label = paste0(
        .data$rank_by_weight,
        ".~italic('",
        .data$species_expr,
        "')",
        dplyr::if_else(isTRUE(.data$is_group_model), "~'[group]'", ""),
        "~'{m",
        as.character(.data$model_id),
        "}'"
      ),
      candidate_label = factor(
        .data$candidate_label,
        levels = rev(.data$candidate_label)
      )
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "Final weight") +
      ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$candidate_label,
      y = .data$w_adm,
      fill = .data$biomass_multiplier_if_replace
    )
  ) +
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
#' @param show_top_candidate Logical scalar controlling whether the top
#'   candidate curve is drawn when available.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
plot_ts_bands <- function(band_tbl,
                          curve_tbl,
                          anchor_label,
                          policy_label,
                          show_top_candidate = FALSE) {
  # Draw the conformal ribbons first, then overlay the anchor, selected, and
  # top-candidate curves from the precomputed summary tables.
  band_df <- tibble::as_tibble(band_tbl)
  curve_df <- tibble::as_tibble(curve_tbl)
  if ("ts_center" %in% names(curve_df)) {
    curve_df$ts_panel_center <- dplyr::coalesce(
      suppressWarnings(as.numeric(curve_df$ts_center)),
      suppressWarnings(as.numeric(curve_df$ts_pred))
    )
  } else {
    curve_df$ts_panel_center <- suppressWarnings(as.numeric(curve_df$ts_pred))
  }
  if (nrow(band_df) == 0 || nrow(curve_df) == 0 ||
    !all(c("length_cm", "ymin", "ymax", "band") %in% names(band_df)) ||
    !all(c("length_cm", "ts_anchor", "ts_panel_center") %in% names(curve_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "Length (cm)", y = "TS (dB re 1 m^2)") +
      ggplot2::theme_minimal(base_size = 11))
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = band_df,
      ggplot2::aes(x = .data$length_cm, ymin = .data$ymin, ymax = .data$ymax, fill = .data$band),
      alpha = 0.28
    ) +
    ggplot2::scale_fill_manual(
      values = c("99%" = "#eef2f7", "95%" = "#d9e0ea", "90%" = "#bcc7d6", "80%" = "#96a6bc"),
      name = "Prediction band"
    ) +
    ggplot2::geom_line(
      data = curve_df,
      ggplot2::aes(x = .data$length_cm, y = .data$ts_panel_center, colour = "Selected policy", linetype = "Selected policy"),
      linewidth = 0.95
    )
  if (isTRUE(show_top_candidate) && "ts_top_candidate" %in% names(curve_df)) {
    p <- p +
      ggplot2::geom_line(
        data = curve_df,
        ggplot2::aes(x = .data$length_cm, y = .data$ts_top_candidate, colour = "Top candidate", linetype = "Top candidate"),
        linewidth = 0.85,
        alpha = 0.9
      )
  }
  p <- p +
    ggplot2::geom_line(
      data = curve_df,
      ggplot2::aes(x = .data$length_cm, y = .data$ts_anchor),
      linewidth = 1.45,
      colour = "white",
      linetype = "longdash",
      alpha = 0.95
    ) +
    ggplot2::geom_line(
      data = curve_df,
      ggplot2::aes(x = .data$length_cm, y = .data$ts_anchor, colour = "Anchor", linetype = "Anchor"),
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
#' @keywords internal
#' @noRd
plot_slope_distribution <- function(slope_tbl) {
  # Plot the study-cell slope distribution directly from the prepared summary
  # table so this function is only responsible for rendering.
  plot_df <- tibble::as_tibble(slope_tbl)
  if (nrow(plot_df) == 0 || !all("slope_len_cell" %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Study-Cell Distribution of TS-Length Slopes", subtitle = "Required plotting fields were not available.", x = "Standardized TS-length slope", y = "Study-cell count") +
      ggplot2::theme_minimal(base_size = 11))
  }
  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$slope_len_cell)) +
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
#' @keywords internal
#' @noRd
plot_slope_group <- function(slope_tbl) {
  # Drop the catch-all group before plotting so the focal review groups remain
  # visually comparable on one axis.
  plot_df <- tibble::as_tibble(slope_tbl)
  if (nrow(plot_df) == 0 || !all(c("review_group", "slope_len_cell") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "TS-Length Slope by Species Group", subtitle = "Required plotting fields were not available.", x = NULL, y = "Standardized TS-length slope") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::filter(.data$review_group != "Other")

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$review_group, y = .data$slope_len_cell, fill = .data$review_group)) +
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
#' @keywords internal
#' @noRd
plot_slope_support <- function(support_tbl) {
  # Apply the paper color mapping to the prepared support table before drawing
  # the stacked group-wise proportions.
  plot_df <- tibble::as_tibble(support_tbl)
  if (nrow(plot_df) == 0 || !all(c("review_group", "prop_study_cells", "original_reference_class") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Support for 20log10 Dependence by Species Group", subtitle = "Required plotting fields were not available.", x = NULL, y = "Proportion of study-cells") +
      ggplot2::theme_minimal(base_size = 11))
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
      dplyr::filter(.data$review_group != "Other"),
    ggplot2::aes(x = .data$review_group, y = .data$prop_study_cells, fill = .data$original_reference_class)
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
#' @param colorbar_name Legend title for the cluster color scale.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
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
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "NMDS1", y = "NMDS2") +
      ggplot2::theme_minimal(base_size = 11))
  }

  # Process the clusters and shared column names
  cluster_name <- cluster_col
  if (!(cluster_name %in% names(plot_df))) {
    cluster_candidates <- c("nmds_cluster_id", "nmds_cluster", "species_cluster_id", "policy_cluster_id")
    matched <- cluster_candidates[cluster_candidates %in% names(plot_df)]
    cluster_name <- if (length(matched) > 0L) matched[[1L]] else NA_character_
  }
  if (is.null(cluster_name) || length(cluster_name) == 0 || is.na(cluster_name) || !(cluster_name %in% names(plot_df))) {
    cluster_name <- "ordination_cluster"
    plot_df[[cluster_name]] <- "All models"
  }
  species_col_ <- if (!(species_col %in% names(plot_df))) {
    sc <- "model_id"
    if (!(sc %in% names(plot_df))) {
      plot_df[[sc]] <- ""
    }
    sc
  } else {
    species_col
  }
  if (common_col %in% names(plot_df)) {
    plot_df$anchor_label <- dplyr::coalesce(as.character(plot_df[[common_col]]), as.character(plot_df[[species_col_]]))
  } else {
    plot_df$anchor_label <- as.character(plot_df[[species_col_]])
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
      x = .data$MDS1,
      y = .data$MDS2,
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
      labels = snake_title
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
#' @keywords internal
#' @noRd
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
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Global NMDS Variable Loadings", subtitle = "Required plotting fields were not available.", x = "NMDS1", y = "NMDS2") +
      ggplot2::theme_minimal(base_size = 11))
  }
  species_col_ <- if (!(species_col %in% names(point_df))) {
    sc <- "model_id"
    if (!(sc %in% names(point_df))) {
      point_df[[sc]] <- ""
    }
    sc
  } else {
    species_col
  }
  if (common_col %in% names(point_df)) {
    point_df$anchor_label <- dplyr::coalesce(as.character(point_df[[common_col]]), as.character(point_df[[species_col_]]))
  } else {
    point_df$anchor_label <- as.character(point_df[[species_col_]])
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
      dplyr::mutate(trait_label = stringr::str_replace_all(.data$trait, "_", " ")),
    ggplot2::aes(x = 0, y = 0)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey80") +
    ggplot2::geom_point(data = point_df, ggplot2::aes(x = .data$MDS1, y = .data$MDS2), inherit.aes = FALSE, colour = "grey75", alpha = 0.35, size = 1.8) +
    ggplot2::geom_point(
      data = point_df[ref_flag, , drop = FALSE],
      ggplot2::aes(x = .data$MDS1, y = .data$MDS2),
      inherit.aes = FALSE,
      shape = 23,
      size = 4.2,
      stroke = 1,
      fill = "#fdd0a2",
      colour = "black"
    ) +
    ggplot2::geom_text(
      data = point_df[ref_flag, , drop = FALSE],
      ggplot2::aes(x = .data$MDS1, y = .data$MDS2, label = .data$anchor_label),
      inherit.aes = FALSE,
      size = 2.8,
      fontface = "italic",
      nudge_y = 0.03 * scale_ref,
      check_overlap = TRUE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = .data$MDS1, yend = .data$MDS2, linewidth = .data$r2, colour = .data$p_value),
      arrow = grid::arrow(length = grid::unit(0.18, "cm")),
      alpha = 0.9
    ) +
    ggplot2::geom_point(ggplot2::aes(x = .data$MDS1, y = .data$MDS2), size = 2.2, colour = "black") +
    ggrepel::geom_text_repel(
      ggplot2::aes(x = .data$MDS1, y = .data$MDS2, label = .data$trait_label),
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
#' @keywords internal
#' @noRd
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
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "NMDS1", y = "NMDS2") +
      ggplot2::theme_minimal(base_size = 11))
  }
  species_col_ <- if (!(species_col %in% names(pts))) {
    sc <- "model_id"
    if (!(sc %in% names(pts))) {
      pts[[sc]] <- ""
    }
    sc
  } else {
    species_col
  }
  pts[[species_col_]] <- stringr::str_squish(as.character(pts[[species_col_]]))
  label_species_ <- if ("species" %in% names(pts)) {
    stringr::str_squish(as.character(pts$species))
  } else {
    stringr::str_squish(stringr::word(pts[[species_col_]], 2, sep = stringr::regex("\\s+")))
  }
  label_genus_ <- if ("genus" %in% names(pts)) {
    stringr::str_squish(as.character(pts$genus))
  } else {
    stringr::str_squish(stringr::word(pts[[species_col_]], 1, sep = stringr::regex("\\s+")))
  }

  # Force evaluation
  force(label_species_)
  force(label_genus_)

  pts <- pts |>
    dplyr::mutate(
      plot_label = dplyr::case_when(
        is.na(.data[[species_col_]]) | !nzchar(.data[[species_col_]]) ~ dplyr::if_else(
          !is.na(label_genus_) & nzchar(label_genus_) & label_genus_ != "NA",
          paste0(label_genus_, " sp."),
          "Generic"
        ),
        .data[[species_col_]] == "NA NA" ~ "Generic",
        !is.na(label_genus_) & nzchar(label_genus_) & label_genus_ != "NA" &
          !is.na(label_species_) & label_species_ %in% c("NA", "sp", "sp.", "spp", "spp.") ~ paste0(label_genus_, " sp."),
        TRUE ~ .data[[species_col_]]
      )
    ) |>
    dplyr::filter(
      !is.na(.data$plot_label),
      nzchar(.data$plot_label)
    )
  if (nrow(pts) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "NMDS1", y = "NMDS2") +
      ggplot2::theme_minimal(base_size = 11))
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
    excluded_cols = c("MDS1", "MDS2", reference_col, species_col_, "model_id", "model_id")
  )
  if (is.character(resolved_group_col) && nzchar(resolved_group_col)) {
    pts$group_val <- dplyr::coalesce(as.character(pts[[resolved_group_col]]), "unknown")
  } else {
    pts$group_val <- "unknown"
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
    dplyr::mutate(.coord_group = paste(round(.data$MDS1, 4), round(.data$MDS2, 4), sep = ":")) |>
    dplyr::group_by(.data$.coord_group) |>
    dplyr::mutate(
      .plot_n = dplyr::n(),
      .plot_i = dplyr::row_number(),
      .plot_angle = 2 * pi * (.data$.plot_i - 1) / pmax(.data$.plot_n, 1),
      .plot_radius = dplyr::if_else(.data$.plot_n > 1, 0.08 * scale_ref * sqrt(.data$.plot_i / .data$.plot_n), 0),
      MDS1_plot = .data$MDS1 + .data$.plot_radius * cos(.data$.plot_angle),
      MDS2_plot = .data$MDS2 + .data$.plot_radius * sin(.data$.plot_angle)
    ) |>
    dplyr::ungroup()
  label_flag <- pts$ref_flag
  if (!any(label_flag, na.rm = TRUE)) {
    label_flag <- rep(TRUE, nrow(pts))
  }
  label_df <- pts[label_flag, , drop = FALSE]

  # Create base layer with the grid setup
  p <- ggplot2::ggplot(mapping = ggplot2::aes(
    x = .data$MDS1_plot,
    y = .data$MDS2_plot
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
          x = .data$MDS1,
          y = .data$MDS2,
          fill = .data$group_val,
          color = .data$group_val,
          group = .data$group_val
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
      dplyr::filter(is.finite(.data$MDS1), is.finite(.data$MDS2), !is.na(.data$p_value), .data$p_value < 0.05) |>
      dplyr::mutate(xend = .data$MDS1 * scale_ref, yend = .data$MDS2 * scale_ref)

    if (nrow(sig_vec) > 0) {
      p <- p +
        ggplot2::geom_segment(
          data = sig_vec,
          ggplot2::aes(x = 0, y = 0, xend = .data$xend, yend = .data$yend),
          inherit.aes = FALSE,
          arrow = grid::arrow(length = grid::unit(0.15, "cm")),
          colour = "#333333",
          linewidth = 0.55
        ) +
        ggplot2::geom_text(
          data = sig_vec,
          ggplot2::aes(x = .data$xend * 1.08, y = .data$yend * 1.08, label = .data$trait),
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
      dplyr::filter(is.finite(.data$MDS1), is.finite(.data$MDS2), !is.na(.data$p_value), .data$p_value < 0.05) |>
      dplyr::mutate(fac_label = paste0(.data$trait, ": ", .data$level))

    if (nrow(sig_fac) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = sig_fac,
          ggplot2::aes(x = .data$MDS1, y = .data$MDS2),
          inherit.aes = FALSE,
          shape = 4,
          size = 2.8,
          stroke = 1,
          colour = "#7f2704"
        ) +
        ggplot2::geom_text(
          data = sig_fac,
          ggplot2::aes(x = .data$MDS1, y = .data$MDS2, label = .data$fac_label),
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
      ggplot2::aes(fill = .data$group_val, shape = "A"),
      size = 3,
      alpha = 0.55
    ) +
    ggplot2::geom_point(
      data = pts[pts$ref_flag, , drop = FALSE],
      ggplot2::aes(fill = .data$group_val, shape = "B"),
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
        label = .data$plot_label,
        size = ifelse(.data$ref_flag, 2.9, 2.2),
        fontface = ifelse(.data$ref_flag, "bold.italic", "italic"),
        colour = ifelse(.data$ref_flag, "black", "grey30")
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
      labels = snake_title
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
#' @keywords internal
#' @noRd
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
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "NMDS1", y = "NMDS2") +
      ggplot2::theme_minimal(base_size = 11))
  }
  if (!"n" %in% names(fac_df)) {
    fac_df$n <- 1
  }
  fac_df <- fac_df |>
    dplyr::mutate(
      trait = factor(.data$trait, levels = unique(.data$trait)),
      centroid_label = paste0(stringr::str_replace_all(.data$trait, "_", " "), ": ", .data$level)
    )
  species_col_ <- if (!(species_col %in% names(point_df))) {
    sc <- "model_id"
    if (!(sc %in% names(point_df))) {
      point_df[[sc]] <- ""
    }
    sc
  } else {
    species_col
  }
  if (common_col %in% names(point_df)) {
    point_df$anchor_label <- dplyr::coalesce(as.character(point_df[[common_col]]), as.character(point_df[[species_col_]]))
  } else {
    point_df$anchor_label <- as.character(point_df[[species_col_]])
  }
  if (reference_col %in% names(point_df)) {
    ref_flag <- dplyr::coalesce(as.logical(point_df[[reference_col]]), FALSE)
  } else {
    ref_flag <- rep(FALSE, nrow(point_df))
  }
  scale_ref <- max(abs(c(point_df$MDS1, point_df$MDS2)), na.rm = TRUE)
  if (!is.finite(scale_ref) || scale_ref <= 0) scale_ref <- 1

  ggplot2::ggplot(fac_df, ggplot2::aes(x = .data$MDS1, y = .data$MDS2, colour = .data$trait)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey80") +
    ggplot2::geom_point(data = point_df, ggplot2::aes(x = .data$MDS1, y = .data$MDS2), inherit.aes = FALSE, colour = "grey75", alpha = 0.35, size = 1.8) +
    ggplot2::geom_point(
      data = point_df[ref_flag, , drop = FALSE],
      ggplot2::aes(x = .data$MDS1, y = .data$MDS2),
      inherit.aes = FALSE,
      shape = 23,
      size = 4.2,
      stroke = 1,
      fill = "#fdd0a2",
      colour = "black"
    ) +
    ggplot2::geom_text(
      data = point_df[ref_flag, , drop = FALSE],
      ggplot2::aes(x = .data$MDS1, y = .data$MDS2, label = .data$anchor_label),
      inherit.aes = FALSE,
      size = 2.8,
      fontface = "italic",
      nudge_y = 0.03 * scale_ref,
      check_overlap = TRUE
    ) +
    ggplot2::geom_point(ggplot2::aes(size = .data$n), alpha = 0.9) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = .data$centroid_label),
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
#' @keywords internal
#' @noRd
plot_overlap_heatmap <- function(overlap_tbl,
                                 metric_labs = NULL) {
  # Reshape the overlap summary to a long heatmap table so all overlap metrics
  # can be compared across anchors on one scale.
  overlap_df <- tibble::as_tibble(overlap_tbl)
  if (nrow(overlap_df) == 0 || !"anchor_species" %in% names(overlap_df)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
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
  alias_cols <- c(
    w_same_fao = "w_same_fao_area",
    w_same_ocean_basin = "w_same_basin"
  )
  for (alias_nm in names(alias_cols)) {
    source_nm <- alias_cols[[alias_nm]]
    if (!alias_nm %in% names(overlap_df) && source_nm %in% names(overlap_df)) {
      overlap_df[[alias_nm]] <- overlap_df[[source_nm]]
    }
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
    dplyr::select("anchor_species", dplyr::any_of(names(metric_labs))) |>
    tidyr::pivot_longer(cols = !"anchor_species", names_to = "metric", values_to = "value") |>
    dplyr::mutate(
      anchor_species = factor(.data$anchor_species, levels = sort(unique(.data$anchor_species))),
      metric = factor(dplyr::recode(.data$metric, !!!metric_labs), levels = unname(metric_labs)),
      value = suppressWarnings(as.numeric(.data$value)),
      value = dplyr::if_else(is.finite(.data$value), .data$value, NA_real_)
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$metric, y = .data$anchor_species, fill = .data$value)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::scale_fill_gradient(
      low = "#f7fbff",
      high = "#08306b",
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25),
      labels = scales::percent_format(accuracy = 1),
      oob = scales::squish,
      na.value = "grey90"
    ) +
    ggplot2::scale_y_discrete(labels = function(x) parse(text = paste0("italic('", gsub("'", "\\\\'", x, fixed = TRUE), "')"))) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = "Weighted overlap"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Resolve raw admissibility plotting config
#'
#' @param config Raw config list or package object.
#'
#' @return Named list.
#' @keywords internal
#' @noRd
admissibility_plot_config_data <- function(config = NULL) {
  if (is_s7_instance(config, "Candidates")) {
    return(candidates_config_data(config) %||% list())
  }
  if (is_s7_instance(config, "Alchemist")) {
    return((config@config)$config_data %||% list())
  }
  if (is_s7_instance(config, "Configurer")) {
    return(config@data %||% list())
  }
  if (is.list(config) && is.list(config$config_data %||% NULL)) {
    return(config$config_data)
  }
  if (is.list(config)) {
    return(config)
  }
  list()
}

#' Resolve one raw admissibility section
#'
#' @param config Raw config list or package object.
#'
#' @return Named list.
#' @keywords internal
#' @noRd
admissibility_plot_section <- function(config = NULL) {
  raw_cfg <- admissibility_plot_config_data(config)
  if (is.list(raw_cfg$admissibility %||% NULL)) {
    return(raw_cfg$admissibility)
  }
  admissibility_keys <- c(
    "species_traits", "study_traits", "coherence", "key_metadata_max",
    "length_overlap_min", "depth_overlap_min", "frequency_mode",
    "frequency_gap", "exact_frequency", "missing_key_metadata_max_fraction"
  )
  if (is.list(raw_cfg) && any(admissibility_keys %in% names(raw_cfg))) {
    return(raw_cfg)
  }
  list()
}

#' Resolve one admissibility trait vector
#'
#' @param x Raw admissibility trait specification.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
admissibility_plot_trait_names <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  x_names <- names(x)
  if (length(x_names) > 0 && any(nzchar(x_names))) {
    out <- as.character(x_names)
  } else {
    out <- as.character(unlist(x, use.names = FALSE))
  }
  unique(out[!is.na(out) & nzchar(out)])
}

#' Resolve whether one admissibility coherence gate is active
#'
#' @param section Raw admissibility section.
#' @param dimension One of `"length"`, `"depth"`, or `"frequency"`.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
admissibility_plot_coherence_active <- function(section,
                                                dimension = c("length", "depth", "frequency")) {
  dimension <- match.arg(dimension)
  section <- section %||% list()
  coherence <- section$coherence %||% list()
  block <- coherence[[dimension]] %||% list()

  if (identical(dimension, "frequency")) {
    mode <- section$frequency_mode %||% block$mode %||% NULL
    if (isTRUE(section$exact_frequency %||% FALSE) && is.null(mode)) {
      mode <- "literal"
    }
    mode <- stringr::str_to_lower(stringr::str_squish(as.character(mode %||% "")))[[1]]
    return(nzchar(mode) && !identical(mode, "none"))
  }

  min_value <- if (identical(dimension, "length")) {
    block$min %||% section$length_overlap_min %||% NULL
  } else {
    block$min %||% section$depth_overlap_min %||% NULL
  }
  mode <- stringr::str_to_lower(stringr::str_squish(as.character(block$mode %||% "")))[[1]]
  if (nzchar(mode)) {
    return(!identical(mode, "none"))
  }
  is.finite(suppressWarnings(as.numeric(min_value %||% NA_real_)))
}

#' Resolve whether the missing-metadata gate is active
#'
#' @param section Raw admissibility section.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
admissibility_plot_missing_active <- function(section) {
  threshold <- section$key_metadata_max %||% section$missing_key_metadata_max_fraction %||% NA_real_
  is.finite(suppressWarnings(as.numeric(threshold)))
}

#' Humanize one admissibility trait label
#'
#' @param trait Trait code.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
admissibility_plot_trait_label <- function(trait) {
  trait <- as.character(trait %||% "")[[1]]
  if (!nzchar(trait)) {
    return("Trait")
  }
  out <- stringr::str_replace_all(trait, "_", " ")
  out <- stringr::str_squish(out)
  out <- stringr::str_replace(out, "(?i)\\btype$", "")
  out <- stringr::str_squish(out)
  out <- stringr::str_to_title(out)
  out <- stringr::str_replace_all(out, "\\bFao\\b", "FAO")
  out
}

#' Humanize one admissibility gate label
#'
#' @param reason Gate reason code.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
admissibility_plot_gate_label <- function(reason) {
  reason <- as.character(reason %||% "")[[1]]
  if (!nzchar(reason)) {
    return("")
  }
  if (startsWith(reason, "trait_mismatch:")) {
    trait <- sub("^trait_mismatch:", "", reason)
    return(paste0(admissibility_plot_trait_label(trait), " mismatch"))
  }
  label_map <- c(
    admissible = "Admissible",
    length_domain_nonoverlap = "Length nonoverlap",
    depth_domain_nonoverlap = "Depth nonoverlap",
    frequency_nonoverlap = "Frequency nonoverlap",
    metadata_missing_excess = "Missing metadata",
    self = "Self"
  )
  out <- unname(label_map[[reason]])
  if (!is.null(out) && nzchar(out)) {
    return(out)
  }
  snake_title(reason)
}

#' Resolve default colors for admissibility gate labels
#'
#' @param reasons Gate reason codes.
#'
#' @return Named character vector keyed by display label.
#' @keywords internal
#' @noRd
admissibility_plot_gate_colors <- function(reasons) {
  reasons <- unique(as.character(reasons))
  reasons <- reasons[!is.na(reasons) & nzchar(reasons)]

  base_cols <- c(
    admissible = "#1b9e77",
    length_domain_nonoverlap = "#7570b3",
    depth_domain_nonoverlap = "#66a61e",
    frequency_nonoverlap = "#1f78b4",
    metadata_missing_excess = "#e6ab02",
    self = "#666666"
  )

  out <- rep(NA_character_, length(reasons))
  names(out) <- reasons

  matched <- intersect(names(base_cols), reasons)
  out[matched] <- base_cols[matched]

  trait_reasons <- reasons[grepl("^trait_mismatch:", reasons)]
  if (length(trait_reasons) > 0) {
    out[trait_reasons] <- grDevices::hcl.colors(length(trait_reasons), "Warm")
  }

  remaining <- reasons[is.na(out) | !nzchar(out)]
  if (length(remaining) > 0) {
    out[remaining] <- grDevices::hcl.colors(length(remaining), "Set 2")
  }

  stats::setNames(unname(out), vapply(reasons, admissibility_plot_gate_label, character(1)))
}

#' Resolve admissibility gate specifications for plotting
#'
#' @param config Raw config list or package object.
#' @param observed_reasons Gate reason codes found in the data.
#' @param gate_labs Optional named vector mapping gate codes to labels.
#' @param gate_cols Optional named vector mapping gate labels to colors.
#'
#' @return Tibble with `reason`, `label`, and `color`.
#' @keywords internal
#' @noRd
admissibility_plot_gate_spec <- function(config = NULL,
                                         observed_reasons = character(0),
                                         gate_labs = NULL,
                                         gate_cols = NULL) {
  observed_reasons <- unique(as.character(observed_reasons))
  observed_reasons <- observed_reasons[!is.na(observed_reasons) & nzchar(observed_reasons)]

  section <- admissibility_plot_section(config)
  reasons <- "admissible"

  if (length(section) > 0) {
    trait_names <- unique(c(
      admissibility_plot_trait_names(section$species_traits),
      admissibility_plot_trait_names(section$study_traits)
    ))
    trait_names <- trait_names[!is.na(trait_names) & nzchar(trait_names)]
    trait_reasons <- if (length(trait_names) == 0) {
      character(0)
    } else {
      paste0("trait_mismatch:", trait_names)
    }
    reasons <- c(reasons, trait_reasons)
    if (admissibility_plot_coherence_active(section, "length")) {
      reasons <- c(reasons, "length_domain_nonoverlap")
    }
    if (admissibility_plot_coherence_active(section, "depth")) {
      reasons <- c(reasons, "depth_domain_nonoverlap")
    }
    if (admissibility_plot_coherence_active(section, "frequency")) {
      reasons <- c(reasons, "frequency_nonoverlap")
    }
    if (admissibility_plot_missing_active(section)) {
      reasons <- c(reasons, "metadata_missing_excess")
    }
  } else {
    reasons <- unique(c(reasons, observed_reasons[observed_reasons != "self"]))
  }

  reasons <- unique(c(reasons, setdiff(observed_reasons, c(reasons, "self"))))
  reasons <- reasons[!is.na(reasons) & nzchar(reasons) & reasons != "self"]

  labels <- vapply(reasons, admissibility_plot_gate_label, character(1))
  if (!is.null(gate_labs)) {
    gate_labs <- gate_labs[!is.na(names(gate_labs)) & nzchar(names(gate_labs))]
    gate_idx <- match(names(gate_labs), reasons, nomatch = 0L)
    if (any(gate_idx > 0L)) {
      labels[gate_idx[gate_idx > 0L]] <- unname(as.character(gate_labs[names(gate_labs)[gate_idx > 0L]]))
    }
  }

  colors <- admissibility_plot_gate_colors(reasons)
  color_lookup <- stats::setNames(unname(colors), names(colors))
  resolved_colors <- unname(color_lookup[labels])
  if (!is.null(gate_cols)) {
    gate_cols <- gate_cols[!is.na(names(gate_cols)) & nzchar(names(gate_cols))]
    replace_idx <- match(labels, names(gate_cols), nomatch = 0L)
    if (any(replace_idx > 0L)) {
      resolved_colors[replace_idx > 0L] <- unname(as.character(gate_cols[labels[replace_idx > 0L]]))
    }
  }

  tibble::tibble(
    reason = reasons,
    label = labels,
    color = resolved_colors
  )
}

#' Plot admissibility gate composition
#'
#' @param gate_tbl Admissibility gate-count table.
#' @param config Raw config list or package object used to derive the active gate
#'   legend.
#' @param gate_labs Optional named vector mapping gate codes to labels.
#' @param gate_cols Optional named vector mapping gate labels to colors.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
plot_gate_composition <- function(gate_tbl,
                                  config = NULL,
                                  gate_labs = NULL,
                                  gate_cols = NULL) {
  # Normalize the gate ordering and labels before drawing the stacked
  # proportions so anchors remain directly comparable.
  plot_df <- tibble::as_tibble(gate_tbl)
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "inadmissible_reason", "n_models") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "Proportion of candidate models") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- dplyr::filter(plot_df, dplyr::coalesce(as.character(.data$inadmissible_reason), "") != "self")
  gate_spec <- admissibility_plot_gate_spec(
    config = config,
    observed_reasons = plot_df$inadmissible_reason,
    gate_labs = gate_labs,
    gate_cols = gate_cols
  )
  gate_levels <- gate_spec$reason
  gate_label_levels <- unique(gate_spec$label)
  gate_label_lookup <- stats::setNames(gate_spec$label, gate_spec$reason)
  gate_color_lookup <- stats::setNames(gate_spec$color, gate_spec$label)

  plot_df <- plot_df |>
    dplyr::mutate(
      anchor_species = factor(.data$anchor_species, levels = unique(.data$anchor_species)),
      inadmissible_reason = factor(.data$inadmissible_reason, levels = gate_levels),
      gate_label = factor(
        dplyr::recode(as.character(.data$inadmissible_reason), !!!gate_label_lookup),
        levels = gate_label_levels
      )
    ) |>
    tidyr::complete(
      .data$anchor_species,
      inadmissible_reason = factor(gate_levels, levels = gate_levels),
      fill = list(n_models = 0)
    ) |>
    dplyr::mutate(
      gate_label = factor(
        dplyr::recode(as.character(.data$inadmissible_reason), !!!gate_label_lookup),
        levels = gate_label_levels
      )
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$anchor_species, y = .data$n_models, fill = .data$gate_label)) +
    ggplot2::geom_col(position = "fill", width = 0.78) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_fill_manual(values = gate_color_lookup, drop = FALSE, name = "Gate outcome") +
    ggplot2::labs(
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
#' @keywords internal
#' @noRd
plot_anchor_ranges <- function(range_tbl) {
  # Draw the admissible-range summary on a log scale so multiplicative changes
  # above and below one are visually comparable.
  plot_df <- tibble::as_tibble(range_tbl)
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "q50_multiplier_admissible", "q05_multiplier_admissible", "q95_multiplier_admissible") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Admissible Biomass Multiplier Range by Reference", subtitle = "Required plotting fields were not available.", x = NULL, y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = stats::reorder(.data$anchor_species, .data$q50_multiplier_admissible), y = .data$q50_multiplier_admissible)
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$q05_multiplier_admissible, ymax = .data$q95_multiplier_admissible), width = 0.15, colour = "#2171b5") +
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

#' Filter plotting rows to selected anchor species
#'
#' @param tbl Plotting table.
#' @param anchor_species Optional character vector of species names. `NULL`
#'   leaves the table unchanged.
#' @param species_col Species column name.
#'
#' @return Filtered tibble.
#' @keywords internal
#' @noRd
filter_plot_anchor_species <- function(tbl,
                                       anchor_species = NULL,
                                       species_col = "anchor_species") {
  out <- tibble::as_tibble(tbl)
  anchor_species <- unique(as.character(anchor_species %||% character(0)))
  anchor_species <- anchor_species[!is.na(anchor_species) & nzchar(anchor_species)]
  if (length(anchor_species) == 0L || !species_col %in% names(out)) {
    return(out)
  }
  out |>
    dplyr::filter(as.character(.data[[species_col]]) %in% .env$anchor_species)
}

#' Normalize a plotting policy limit
#'
#' @param max_policies Requested maximum policy count.
#' @param default Default value used when `max_policies` is `NULL` or invalid.
#'
#' @return Integer limit, or `Inf` when all policies should be retained.
#' @keywords internal
#' @noRd
normalize_plot_policy_limit <- function(max_policies,
                                        default = 30L) {
  if (is.null(max_policies) || length(max_policies) != 1L) {
    return(as.integer(default))
  }
  max_policies <- suppressWarnings(as.numeric(max_policies))
  if (is.na(max_policies) || max_policies <= 0) {
    return(as.integer(default))
  }
  if (is.infinite(max_policies)) {
    return(Inf)
  }
  as.integer(max_policies)
}

#' Select policy display levels for compact plots
#'
#' @param tbl Plotting table.
#' @param policy_col Policy display column.
#' @param value_col Numeric ranking column.
#' @param max_policies Maximum number of policy levels to keep.
#' @param decreasing Logical scalar. `TRUE` keeps largest values first;
#'   `FALSE` keeps smallest values first.
#'
#' @return Character vector of policy levels.
#' @keywords internal
#' @noRd
select_policy_levels_for_plot <- function(tbl,
                                          policy_col = "policy",
                                          value_col = "error_abs_log",
                                          max_policies = 30L,
                                          decreasing = FALSE) {
  tbl <- tibble::as_tibble(tbl)
  if (!all(c(policy_col, value_col) %in% names(tbl))) {
    return(character(0))
  }
  max_policies <- normalize_plot_policy_limit(max_policies, default = 30L)
  levels_tbl <- tbl |>
    dplyr::filter(!is.na(.data[[policy_col]]), nzchar(as.character(.data[[policy_col]]))) |>
    dplyr::group_by(.data[[policy_col]]) |>
    dplyr::summarise(plot_rank_value = stats::median(.data[[value_col]], na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(is.finite(.data$plot_rank_value))
  if (isTRUE(decreasing)) {
    levels_tbl <- dplyr::arrange(levels_tbl, dplyr::desc(.data$plot_rank_value), .data[[policy_col]])
  } else {
    levels_tbl <- dplyr::arrange(levels_tbl, .data$plot_rank_value, .data[[policy_col]])
  }
  out <- as.character(levels_tbl[[policy_col]])
  if (is.finite(max_policies)) {
    out <- head(out, max_policies)
  }
  out
}

#' Cluster a heatmap axis from a long plotting table
#'
#' @param tbl Long heatmap table.
#' @param row_col Row identifier column.
#' @param col_col Column identifier column to order.
#' @param value_col Numeric cell value column.
#' @param fallback_levels Levels returned when clustering is not possible.
#'
#' @return Character vector of clustered column levels.
#' @keywords internal
#' @noRd
cluster_heatmap_axis_levels <- function(tbl,
                                        row_col,
                                        col_col,
                                        value_col,
                                        fallback_levels) {
  tbl <- tibble::as_tibble(tbl)
  fallback_levels <- as.character(fallback_levels %||% character(0))
  if (!all(c(row_col, col_col, value_col) %in% names(tbl))) {
    return(fallback_levels)
  }
  wide <- tbl |>
    dplyr::select(
      row_id = dplyr::all_of(row_col),
      col_id = dplyr::all_of(col_col),
      value = dplyr::all_of(value_col)
    ) |>
    dplyr::filter(
      !is.na(.data$row_id),
      !is.na(.data$col_id),
      is.finite(.data$value)
    ) |>
    tidyr::pivot_wider(names_from = "col_id", values_from = "value")
  if (nrow(wide) < 2L || ncol(wide) < 3L) {
    return(fallback_levels)
  }
  mat <- as.matrix(wide[, setdiff(names(wide), "row_id"), drop = FALSE])
  if (ncol(mat) < 2L) {
    return(fallback_levels)
  }
  col_medians <- apply(mat, 2L, stats::median, na.rm = TRUE)
  for (j in seq_len(ncol(mat))) {
    bad <- !is.finite(mat[, j])
    if (any(bad)) {
      mat[bad, j] <- col_medians[[j]]
    }
  }
  hc <- try(stats::hclust(stats::dist(t(mat))), silent = TRUE)
  if (inherits(hc, "try-error")) {
    return(fallback_levels)
  }
  clustered <- colnames(mat)[hc$order]
  unique(c(clustered, setdiff(fallback_levels, clustered)))
}

#' Plot pseudo-anchor policy errors
#'
#' @param perf_tbl Pseudo-anchor benchmark table.
#' @param anchor_species Optional species names used to restrict rows before
#'   summarising policies.
#' @param max_policies Maximum number of displayed policies. `Inf` keeps all.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
plot_policy_boxplot <- function(perf_tbl,
                                anchor_species = NULL,
                                max_policies = 30L) {
  # Restrict the boxplot to valid finite predictions before ranking policies
  # by their median error.
  plot_df <- tibble::as_tibble(perf_tbl)
  if (!all(c("valid_prediction", "error_abs_log") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "|log(multiplier prediction)|") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- filter_plot_anchor_species(plot_df, anchor_species = anchor_species)
  plot_df$policy <- resolve_policy_display_names(plot_df)
  plot_df <- plot_df |>
    dplyr::filter(.data$valid_prediction, is.finite(.data$error_abs_log)) |>
    dplyr::mutate(plot_error = pmax(.data$error_abs_log, .Machine$double.xmin))
  if (nrow(plot_df) == 0 || !"policy" %in% names(plot_df)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "|log(multiplier prediction)|") +
      ggplot2::theme_minimal(base_size = 11))
  }
  policy_levels <- select_policy_levels_for_plot(
    plot_df,
    policy_col = "policy",
    value_col = "plot_error",
    max_policies = max_policies,
    decreasing = FALSE
  )
  plot_df <- plot_df |>
    dplyr::filter(.data$policy %in% .env$policy_levels)

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = factor(.data$policy, levels = policy_levels), y = .data$plot_error, fill = .data$policy)
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
#' @keywords internal
#' @noRd
plot_species_boxplot <- function(perf_tbl) {
  # Plot the leave-one-species-out benchmark on the same error scale as the
  # pseudo-anchor benchmark for direct comparison.
  plot_df <- tibble::as_tibble(perf_tbl)
  if (!all(c("valid_prediction", "error_abs_log") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "|log(multiplier prediction)|") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df$policy <- resolve_policy_display_names(plot_df)
  plot_df <- plot_df |>
    dplyr::filter(.data$valid_prediction, is.finite(.data$error_abs_log)) |>
    dplyr::mutate(plot_error = pmax(.data$error_abs_log, .Machine$double.xmin))
  if (nrow(plot_df) == 0 || !"policy" %in% names(plot_df)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "|log(multiplier prediction)|") +
      ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = stats::reorder(.data$policy, .data$plot_error, FUN = stats::median), y = .data$plot_error, fill = .data$policy)
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
#' @param anchor_species Optional species names used to restrict held-out
#'   species before summarising policies.
#' @param max_policies Maximum number of displayed policies. `Inf` keeps all.
#' @param cluster_policies Logical scalar. If `TRUE`, order policy columns by
#'   hierarchical clustering of the displayed species-policy error matrix when
#'   enough rows and columns are available.
#' @param show_values Logical scalar. If `NULL`, values are shown only for
#'   reasonably small heatmaps.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
plot_policy_heatmap <- function(perf_tbl,
                                policy_labs = NULL,
                                anchor_species = NULL,
                                max_policies = 40L,
                                cluster_policies = TRUE,
                                show_values = NULL) {
  # Collapse the held-out benchmark to one median error per species-policy
  # pair before drawing the heatmap.
  plot_df <- tibble::as_tibble(perf_tbl)
  if (!all(c("anchor_species", "valid_prediction", "error_abs_log") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- filter_plot_anchor_species(plot_df, anchor_species = anchor_species)
  plot_df$policy <- resolve_policy_display_names(plot_df)
  plot_df <- plot_df |>
    dplyr::filter(.data$valid_prediction, is.finite(.data$error_abs_log)) |>
    dplyr::group_by(.data$anchor_species, .data$policy) |>
    dplyr::summarise(median_abs_log_error = stats::median(.data$error_abs_log, na.rm = TRUE), .groups = "drop")
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "policy", "median_abs_log_error") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }

  if (!is.null(policy_labs)) {
    plot_df <- plot_df |>
      dplyr::mutate(policy = dplyr::recode(.data$policy, !!!policy_labs))
  }

  policy_levels <- plot_df |>
    dplyr::group_by(.data$policy) |>
    dplyr::summarise(global_median_abs_log = stats::median(.data$median_abs_log_error, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$global_median_abs_log, .data$policy) |>
    dplyr::pull(.data$policy)
  max_policies <- normalize_plot_policy_limit(max_policies, default = 40L)
  if (is.finite(max_policies)) {
    policy_levels <- head(policy_levels, max_policies)
    plot_df <- plot_df |>
      dplyr::filter(.data$policy %in% .env$policy_levels)
  }
  if (isTRUE(cluster_policies)) {
    policy_levels <- cluster_heatmap_axis_levels(
      plot_df,
      row_col = "anchor_species",
      col_col = "policy",
      value_col = "median_abs_log_error",
      fallback_levels = policy_levels
    )
  }
  show_values <- show_values %||% (length(unique(plot_df$anchor_species)) * length(policy_levels) <= 120L)

  p <- ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(
        policy = factor(.data$policy, levels = policy_levels),
        anchor_species = factor(.data$anchor_species, levels = sort(unique(.data$anchor_species)))
      ),
    ggplot2::aes(x = .data$policy, y = .data$anchor_species, fill = .data$median_abs_log_error)
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
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7))
  if (isTRUE(show_values)) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.2f", .data$median_abs_log_error)),
        size = 2.5,
        colour = "black"
      )
  }
  p
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
#' @noRd
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
  # Accept either the prepared class or a plain summary table for figure work.
  conjurer_obj <- is_s7_instance(x, "Conjurer")
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
    return(ggplot2::ggplot() +
      ggplot2::labs(title = title %||% "Missing Study Metadata Uncertainty Heatmap", subtitle = subtitle %||% paste0("Required Conjurer summary columns were not available for metric: ", metric, "."), x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
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
    dplyr::select("anchor_species", "trait", value = dplyr::all_of(metric)) |>
    dplyr::mutate(
      trait_label = dplyr::recode(.data$trait, !!!trait_labs),
      value_label = conjurer_metric_value_labels(.data$value, metric)
    )

  # Order traits by overall magnitude unless the caller supplied one directly.
  if (is.null(trait_order)) {
    if (length(configured_traits) > 0) {
      trait_order <- unname(trait_labs[configured_traits])
      trait_order <- trait_order[!is.na(trait_order) & nzchar(trait_order)]
      trait_order <- unique(c(trait_order, setdiff(unique(as.character(plot_df$trait_label)), trait_order)))
    } else {
      trait_order <- plot_df |>
        dplyr::group_by(.data$trait_label) |>
        dplyr::summarise(metric_mean = mean(.data$value, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(.data$metric_mean), .data$trait_label) |>
        dplyr::pull(.data$trait_label)
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
      anchor_species = factor(.data$anchor_species, levels = anchor_order),
      trait_label = factor(.data$trait_label, levels = trait_order)
    )

  # Build the heatmap around one continuous fill scale and optional cell labels.
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$trait_label, y = .data$anchor_species, fill = .data$value)) +
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
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$value_label), size = 3.2)
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
#' @usage
#' \method{plot}{Conjurer}(
#'   x,
#'   y = NULL,
#'   metric = "mean_abs_db_shift",
#'   trait_labs = NULL,
#'   anchor_order = NULL,
#'   trait_order = NULL,
#'   title = NULL,
#'   subtitle = NULL,
#'   fill_lab = NULL,
#'   show_values = TRUE,
#'   na_value = "grey90",
#'   tile_colour = "white",
#'   ...
#' )
NULL

.plot_conjurer <- function(x,
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

#' Register the `Conjurer` plot method
#'
#' @name plot.Conjurer
#' @usage NULL
#'
#' @keywords internal
#' @noRd
plot.Conjurer <- .plot_conjurer
S7::method(plot_generic, Conjurer) <- .plot_conjurer

#' Summarize species-policy benchmark performance
#'
#' @param perf_tbl Species-block benchmark table.
#'
#' @return A tibble with one row per held-out species and policy.
#' @keywords internal
#' @noRd
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
    dplyr::group_by(.data$anchor_species, .data$policy) |>
    dplyr::summarise(
      n_tested_anchor_rows = dplyr::n(),
      n_valid_anchor_models = sum(.data$valid_prediction & is.finite(.data$error_abs_log), na.rm = TRUE),
      .groups = "drop"
    )

  valid_summary <- perf_tbl |>
    dplyr::filter(.data$valid_prediction, is.finite(.data$error_abs_log)) |>
    dplyr::group_by(.data$anchor_species, .data$policy) |>
    dplyr::summarise(
      median_abs_log_error = stats::median(.data$error_abs_log, na.rm = TRUE),
      mean_abs_log_error = mean(.data$error_abs_log, na.rm = TRUE),
      q90_abs_log_error = stats::quantile(.data$error_abs_log, probs = 0.90, na.rm = TRUE, names = FALSE, type = 8),
      median_predicted_multiplier = stats::median(.data$multiplier_pred, na.rm = TRUE),
      median_local_combined_distance = stats::median(.data$local_weighted_mean_combined_distance, na.rm = TRUE),
      median_structural_q_abs_log = stats::median(.data$local_structural_q_abs_log, na.rm = TRUE),
      .groups = "drop"
    )

  policy_grid |>
    dplyr::left_join(row_counts, by = c("anchor_species", "policy")) |>
    dplyr::left_join(valid_summary, by = c("anchor_species", "policy")) |>
    dplyr::mutate(
      n_tested_anchor_rows = dplyr::coalesce(.data$n_tested_anchor_rows, 0L),
      n_valid_anchor_models = dplyr::coalesce(.data$n_valid_anchor_models, 0L),
      n_anchor_models = .data$n_valid_anchor_models,
      has_valid_prediction = .data$n_valid_anchor_models > 0
    ) |>
    dplyr::group_by(.data$anchor_species) |>
    dplyr::arrange(
      !.data$has_valid_prediction,
      .data$median_abs_log_error,
      .data$mean_abs_log_error,
      .data$policy,
      .by_group = TRUE
    ) |>
    dplyr::mutate(rank_within_species = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$policy) |>
    dplyr::mutate(global_median_abs_log_error = stats::median(.data$median_abs_log_error, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$anchor_species, .data$rank_within_species)
}

#' Plot every species-policy benchmark rank
#'
#' @param perf_tbl Species-block benchmark table or output from
#'   `summarize_species_policy_performance()`.
#' @param anchor_species Optional species names used to restrict held-out
#'   species before plotting.
#' @param max_policies Maximum number of ranked policies to show per species.
#' @param include_invalid Logical scalar. If `TRUE`, no-valid-prediction
#'   policies are retained at the bottom of each species facet.
#'
#' @return A ggplot object.
#' @keywords internal
#' @noRd
plot_species_policy_ranked <- function(perf_tbl,
                                       anchor_species = NULL,
                                       max_policies = 25L,
                                       include_invalid = FALSE) {
  plot_df <- tibble::as_tibble(perf_tbl)
  if (!all(c("rank_within_species", "median_abs_log_error") %in% names(plot_df))) {
    plot_df <- summarize_species_policy_performance(plot_df)
  }
  if (nrow(plot_df) == 0 ||
    !all(c("anchor_species", "policy", "rank_within_species", "median_abs_log_error") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Species-Blocked Policy Performance by Held-Out Species", subtitle = "Required plotting fields were not available.", x = "Median |log multiplier error|", y = "Policy") +
      ggplot2::theme_minimal(base_size = 11))
  }

  max_policies <- normalize_plot_policy_limit(max_policies, default = 25L)
  plot_df <- plot_df |>
    filter_plot_anchor_species(anchor_species = anchor_species) |>
    dplyr::filter(isTRUE(include_invalid) | .data$has_valid_prediction) |>
    dplyr::group_by(.data$anchor_species) |>
    dplyr::arrange(.data$rank_within_species, .by_group = TRUE)
  if (is.finite(max_policies)) {
    plot_df <- plot_df |>
      dplyr::slice_head(n = max_policies)
  }
  plot_df <- plot_df |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$anchor_species, dplyr::desc(.data$rank_within_species)) |>
    dplyr::group_by(.data$anchor_species) |>
    dplyr::mutate(
      invalid_x_position = {
        finite_x <- .data$median_abs_log_error[is.finite(.data$median_abs_log_error)]
        if (length(finite_x) == 0) 1 else max(finite_x, na.rm = TRUE) * 1.08
      },
      plot_abs_log_error = dplyr::if_else(
        is.finite(.data$median_abs_log_error),
        .data$median_abs_log_error,
        .data$invalid_x_position
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      policy_species_key = paste(.data$policy, .data$anchor_species, sep = "__"),
      policy_species_key = factor(.data$policy_species_key, levels = unique(.data$policy_species_key))
    ) |>
    dplyr::filter(
      !is.na(.data$anchor_species),
      !is.na(.data$policy),
      !is.na(.data$has_valid_prediction),
      is.finite(.data$plot_abs_log_error),
      is.finite(.data$n_anchor_models)
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Species-Blocked Policy Performance by Held-Out Species", subtitle = "Required plotting fields were not available.", x = "Median |log multiplier error|", y = "Policy") +
      ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$plot_abs_log_error,
      y = .data$policy_species_key,
      color = .data$median_structural_q_abs_log,
      size = .data$n_anchor_models,
      shape = .data$has_valid_prediction
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
      subtitle = if (isTRUE(include_invalid)) {
        "Each facet is ordered best-to-worst from top to bottom; no-valid-prediction policies are retained at the bottom."
      } else {
        "Each facet shows the top valid species-blocked policies, ordered best-to-worst from top to bottom."
      },
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
#'   `summarize_species_policy_performance()`.
#' @param selected_tbl Anchor selected-policy table.
#' @param selection_source Label for the selection source.
#'
#' @return A tibble.
#' @keywords internal
#' @noRd
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
    dplyr::filter(.data$has_valid_prediction) |>
    dplyr::group_by(.data$anchor_species) |>
    dplyr::arrange(.data$rank_within_species, .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      .data$anchor_species,
      species_oracle_best_policy = .data$policy,
      species_oracle_best_median_abs_log_error = .data$median_abs_log_error
    )

  selected_tbl |>
    dplyr::filter(
      !is.na(.data$selected_policy_display),
      nzchar(as.character(.data$selected_policy_display))
    ) |>
    dplyr::mutate(
      selected_policy = as.character(.data$selected_policy),
      selected_policy_display = as.character(.data$selected_policy_display),
      selection_source = selection_source
    ) |>
    dplyr::left_join(
      species_policy_tbl |>
        dplyr::select(
          "anchor_species",
          selected_policy_display = "policy_display",
          species_rank = "rank_within_species",
          selected_species_median_abs_log_error = "median_abs_log_error",
          selected_species_mean_abs_log_error = "mean_abs_log_error",
          selected_species_q90_abs_log_error = "q90_abs_log_error",
          selected_has_valid_species_prediction = "has_valid_prediction",
          selected_n_valid_anchor_models = "n_valid_anchor_models",
          selected_n_tested_anchor_rows = "n_tested_anchor_rows"
        ),
      by = c("anchor_species", "selected_policy_display")
    ) |>
    dplyr::left_join(best_tbl, by = "anchor_species") |>
    dplyr::mutate(
      selected_delta_to_species_oracle = .data$selected_species_median_abs_log_error -
        .data$species_oracle_best_median_abs_log_error
    ) |>
    dplyr::arrange(.data$anchor_species, .data$selection_source, .data$species_rank, .data$selected_policy_display)
}

#' Plot conformal calibration by policy
#'
#' @param cal_tbl Policy-level conformal calibration table.
#' @param policy_filter Optional policy display labels to retain.
#' @param max_policies Maximum number of displayed policies. `Inf` keeps all.
#' @param show_values Logical scalar. If `NULL`, values are shown only for
#'   reasonably small heatmaps.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
plot_conformal_scores <- function(cal_tbl,
                                  policy_filter = NULL,
                                  max_policies = 30L,
                                  show_values = NULL) {
  # Accept either the raw calibration tibble or the stored uncertainty bundle,
  # then draw the policy-by-branch calibration surface directly.
  if (is.list(cal_tbl) &&
    !inherits(cal_tbl, "data.frame") &&
    "conf_cal" %in% names(cal_tbl)) {
    cal_tbl <- cal_tbl$conf_cal
  }
  plot_df <- tibble::as_tibble(cal_tbl)
  if (nrow(plot_df) == 0 || !all(c("policy", "q_abs_log") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Conformal Calibration Radius by Policy", subtitle = "Required plotting fields were not available.", x = "Policy", y = "Conformal q_abs_log") +
      ggplot2::theme_minimal(base_size = 11))
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
        branch_vals <- as.character(.data$equation_branch_filter)
        branch_labs <- unname(branch_names[branch_vals])
        branch_labs[is.na(branch_labs) | !nzchar(branch_labs)] <- branch_vals[is.na(branch_labs) | !nzchar(branch_labs)]
        branch_labs
      },
      label_colour = ifelse(
        .data$q_abs_log >= stats::median(.data$q_abs_log, na.rm = TRUE),
        "white",
        "black"
      )
    ) |>
    dplyr::filter(
      !is.na(.data$policy_display),
      nzchar(.data$policy_display),
      is.finite(.data$q_abs_log)
    )
  policy_filter <- unique(as.character(policy_filter %||% character(0)))
  policy_filter <- policy_filter[!is.na(policy_filter) & nzchar(policy_filter)]
  if (length(policy_filter) > 0L) {
    plot_df <- plot_df |>
      dplyr::filter(.data$policy_display %in% .env$policy_filter)
  }
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }

  policy_levels <- plot_df |>
    dplyr::group_by(.data$policy_display) |>
    dplyr::summarise(mean_q_abs_log = mean(.data$q_abs_log, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$mean_q_abs_log, .data$policy_display) |>
    dplyr::pull(.data$policy_display)
  max_policies <- normalize_plot_policy_limit(max_policies, default = 30L)
  if (is.finite(max_policies)) {
    policy_levels <- head(policy_levels, max_policies)
    plot_df <- plot_df |>
      dplyr::filter(.data$policy_display %in% .env$policy_levels)
  }
  branch_levels <- {
    branch_defs <- read_policy_registry()$policy_branches %||% list()
    registry_levels <- vapply(branch_defs, function(x) as.character(x$display_name %||% x$key %||% NA_character_), character(1))
    intersect(registry_levels, unique(plot_df$branch_display))
  }
  sigma_value <- max(0.2, stats::median(plot_df$q_abs_log, na.rm = TRUE) / 3)
  if (!is.finite(sigma_value) || sigma_value <= 0) {
    sigma_value <- 0.2
  }
  show_values <- show_values %||% (length(policy_levels) * length(unique(plot_df$branch_display)) <= 90L)

  p <- ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(
        policy_display = factor(.data$policy_display, levels = rev(policy_levels)),
        branch_display = factor(.data$branch_display, levels = branch_levels)
      ),
    ggplot2::aes(x = .data$branch_display, y = .data$policy_display, fill = .data$q_abs_log)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
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
  if (isTRUE(show_values)) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(
          label = sprintf("%.2f", .data$q_abs_log),
          colour = .data$label_colour
        ),
        size = 2.7,
        show.legend = FALSE
      )
  }
  p
}

#' Plot tuning component importance
#'
#' @param impact_tbl Tuning component-impact table.
#' @param label_map Optional named vector mapping component codes to labels.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
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
          as.character(.data$component),
          length_coherence = "Length coherence",
          depth_coherence = "Depth coherence",
          frequency_coherence = "Frequency coherence",
          .default = snake_title(as.character(.data$component))
        )
      ) |>
      dplyr::arrange(.data$component_rank_global, .data$component_label) |>
      dplyr::distinct(.data$component_label) |>
      dplyr::pull(.data$component_label)
    plot_df <- plot_df |>
      dplyr::mutate(
        component_label = dplyr::recode(
          as.character(.data$component),
          length_coherence = "Length coherence",
          depth_coherence = "Depth coherence",
          frequency_coherence = "Frequency coherence",
          .default = snake_title(as.character(.data$component))
        ),
        component_label = factor(
          .data$component_label,
          levels = component_levels
        ),
        anchor_label = paste0("italic('", gsub("'", "\\\\'", as.character(.data$anchor_species), fixed = TRUE), "')")
      )

    p <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = .data$component_label, y = .data$delta_log_spread, fill = .data$delta_log_consensus)
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
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Empirical Component Importance for Similarity Weighting", subtitle = "Required plotting fields were not available.", x = NULL, y = "Delta RMSE after component dropout") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::filter(.data$component != "full_model")
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Empirical Component Importance for Similarity Weighting", subtitle = "Required plotting fields were not available.", x = NULL, y = "Delta RMSE after component dropout") +
      ggplot2::theme_minimal(base_size = 11))
  }

  if (!is.null(label_map)) {
    plot_df <- plot_df |>
      dplyr::mutate(component = dplyr::recode(.data$component, !!!label_map))
  }

  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(component = forcats::fct_reorder(.data$component, .data$delta_rmse)),
    ggplot2::aes(x = .data$component, y = .data$delta_rmse, fill = .data$delta_mae)
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
#' @keywords internal
#' @noRd
plot_uncertainty_heat <- function(dropout_tbl,
                                  block_labs = NULL) {
  # Apply optional relabeling and preserve the global component order so the
  # anchor-by-component heatmap lines up with the component-importance plots.
  plot_df <- tibble::as_tibble(dropout_tbl)
  if (nrow(plot_df) == 0 || !all(c("block", "anchor_species", "importance_score") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Anchor-Specific Local Dropout Sensitivity", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::mutate(
      block = dplyr::recode(
        as.character(.data$block),
        !!!(block_labs %||% c()),
        length_coherence = "Length coherence",
        depth_coherence = "Depth coherence",
        frequency_coherence = "Frequency coherence",
        .default = snake_title(as.character(.data$block))
      )
    )
  if ("component_rank_global" %in% names(plot_df)) {
    block_levels <- plot_df |>
      dplyr::arrange(.data$component_rank_global, .data$block) |>
      dplyr::distinct(.data$block) |>
      dplyr::pull(.data$block)
  } else {
    block_levels <- plot_df |>
      dplyr::group_by(.data$block) |>
      dplyr::summarise(mean_importance = mean(.data$importance_score, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$mean_importance), .data$block) |>
      dplyr::pull(.data$block)
  }

  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(block = factor(.data$block, levels = block_levels)),
    ggplot2::aes(x = .data$block, y = .data$anchor_species, fill = .data$importance_score)
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
#' @keywords internal
#' @noRd
plot_selected_intervals <- function(sel_tbl,
                                    reference_label = "Reference") {
  multiplier_axis_breaks <- function(values) {
    finite_vals <- values[is.finite(values) & values > 0]
    if (length(finite_vals) == 0) {
      return(c(0.1, 1, 10, 100))
    }
    lower_pow <- floor(min(log10(finite_vals), na.rm = TRUE))
    upper_pow <- ceiling(max(log10(finite_vals), na.rm = TRUE))
    10^seq(lower_pow, upper_pow, by = 1)
  }
  multiplier_axis_labels <- function(x) {
    vapply(x, function(value) {
      if (!is.finite(value) || value <= 0) {
        return(NA_character_)
      }
      if (abs(value - 1) < 1e-12) {
        return("1 (Reference)")
      }
      if (value < 1) {
        inv <- gsub("\\s+", "", formatC(1 / value, format = "fg", digits = 6))
        return(paste0("-", inv))
      }
      paste0("+", gsub("\\s+", "", formatC(value, format = "fg", digits = 6)))
    }, character(1))
  }
  # Normalize the displayed policy label once before drawing the selected
  # interval summary.
  plot_df <- tibble::as_tibble(sel_tbl)
  plot_df$selected_policy_display <- resolve_selected_policy_names(plot_df)
  q_log <- dplyr::coalesce(
    if ("meta_q_abs_log_total" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$meta_q_abs_log_total)) else rep(NA_real_, nrow(plot_df)),
    if ("q_abs_log_total" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$q_abs_log_total)) else rep(NA_real_, nrow(plot_df)),
    if ("meta_q_abs_log" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$meta_q_abs_log)) else rep(NA_real_, nrow(plot_df)),
    if ("q_abs_log" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$q_abs_log)) else rep(NA_real_, nrow(plot_df))
  )
  multiplier_pred <- suppressWarnings(as.numeric(plot_df$multiplier_pred))
  plot_df$multiplier_lo <- dplyr::coalesce(
    if ("meta_post_selection_multiplier_lo" %in% names(plot_df)) {
      suppressWarnings(as.numeric(plot_df$meta_post_selection_multiplier_lo))
    } else {
      rep(NA_real_, nrow(plot_df))
    },
    if ("multiplier_lo" %in% names(plot_df)) {
      suppressWarnings(as.numeric(plot_df$multiplier_lo))
    } else {
      rep(NA_real_, nrow(plot_df))
    }
  )
  derive_lo <- !is.finite(plot_df$multiplier_lo) &
    is.finite(multiplier_pred) & multiplier_pred > 0 &
    is.finite(q_log) & q_log > 0
  plot_df$multiplier_lo[derive_lo] <- multiplier_pred[derive_lo] * exp(-q_log[derive_lo])
  plot_df$multiplier_hi <- dplyr::coalesce(
    if ("meta_post_selection_multiplier_hi" %in% names(plot_df)) {
      suppressWarnings(as.numeric(plot_df$meta_post_selection_multiplier_hi))
    } else {
      rep(NA_real_, nrow(plot_df))
    },
    if ("multiplier_hi" %in% names(plot_df)) {
      suppressWarnings(as.numeric(plot_df$multiplier_hi))
    } else {
      rep(NA_real_, nrow(plot_df))
    }
  )
  derive_hi <- !is.finite(plot_df$multiplier_hi) &
    is.finite(multiplier_pred) & multiplier_pred > 0 &
    is.finite(q_log) & q_log > 0
  plot_df$multiplier_hi[derive_hi] <- multiplier_pred[derive_hi] * exp(q_log[derive_hi])
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "multiplier_pred", "multiplier_lo", "multiplier_hi", "selected_policy_display") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::filter(
      !is.na(.data$anchor_species),
      is.finite(.data$multiplier_pred),
      is.finite(.data$multiplier_lo),
      is.finite(.data$multiplier_hi),
      .data$multiplier_pred > 0,
      .data$multiplier_lo > 0,
      .data$multiplier_hi > 0
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::mutate(
      anchor_species = factor(
        .data$anchor_species,
        levels = plot_df |>
          dplyr::group_by(.data$anchor_species) |>
          dplyr::summarise(order_value = stats::median(.data$multiplier_pred, na.rm = TRUE), .groups = "drop") |>
          dplyr::arrange(.data$order_value, .data$anchor_species) |>
          dplyr::pull(.data$anchor_species) |>
          unique()
      )
    )
  axis_breaks <- multiplier_axis_breaks(c(
    plot_df$multiplier_lo,
    plot_df$multiplier_hi,
    plot_df$multiplier_pred
  ))

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$anchor_species,
      y = .data$multiplier_pred,
      ymin = .data$multiplier_lo,
      ymax = .data$multiplier_hi,
      colour = .data$selected_policy_display
    )
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_errorbar(width = 0.14) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::geom_vline(
      xintercept = seq_along(levels(plot_df$anchor_species)),
      linewidth = 0.35,
      colour = "grey88"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_log10(
      breaks = axis_breaks,
      labels = multiplier_axis_labels
    ) +
    ggplot2::annotation_logticks(sides = "b") +
    ggplot2::scale_x_discrete(
      labels = function(x) parse(text = paste0("italic('", x, "')")),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Biomass multiplier",
      colour = "Displayed\nselection"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 15.75),
      axis.text = ggplot2::element_text(size = 14, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.4, colour = "black"),
      legend.text = ggplot2::element_text(size = 14, colour = "black"),
      legend.title = ggplot2::element_text(size = 15.75, colour = "black"),
      legend.position = "top",
      legend.direction = "horizontal"
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE
      )
    )
}

#' Plot integrated anchor summary
#'
#' @param integrated_tbl Integrated anchor-summary table.
#' @param score_tbl Admissible candidate-score table.
#' @param interval_tbl All-policy interval table.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
plot_anchor_summary <- function(integrated_tbl,
                                score_tbl,
                                interval_tbl) {
  multiplier_axis_breaks <- function(values) {
    finite_vals <- values[is.finite(values) & values > 0]
    if (length(finite_vals) == 0) {
      return(c(0.1, 1, 10, 100))
    }
    lower_pow <- floor(min(log10(finite_vals), na.rm = TRUE))
    upper_pow <- ceiling(max(log10(finite_vals), na.rm = TRUE))
    10^seq(lower_pow, upper_pow, by = 1)
  }
  multiplier_axis_labels <- function(x) {
    vapply(x, function(value) {
      if (!is.finite(value) || value <= 0) {
        return(NA_character_)
      }
      if (abs(value - 1) < 1e-12) {
        return("1 (Reference)")
      }
      if (value < 1) {
        inv <- gsub("\\s+", "", formatC(1 / value, format = "fg", digits = 6))
        return(paste0("-", inv))
      }
      paste0("+", gsub("\\s+", "", formatC(value, format = "fg", digits = 6)))
    }, character(1))
  }
  # Align all three layers to the same anchor ordering before drawing the
  # selected-strategy interval, the all-tested strategy cloud, and the nested
  # admissible-pool intervals on one log-scale axis.
  integrated_df <- tibble::as_tibble(integrated_tbl)
  q_log <- dplyr::coalesce(
    if ("meta_q_abs_log_total" %in% names(integrated_df)) suppressWarnings(as.numeric(integrated_df$meta_q_abs_log_total)) else rep(NA_real_, nrow(integrated_df)),
    if ("q_abs_log_total" %in% names(integrated_df)) suppressWarnings(as.numeric(integrated_df$q_abs_log_total)) else rep(NA_real_, nrow(integrated_df)),
    if ("meta_q_abs_log" %in% names(integrated_df)) suppressWarnings(as.numeric(integrated_df$meta_q_abs_log)) else rep(NA_real_, nrow(integrated_df)),
    if ("q_abs_log" %in% names(integrated_df)) suppressWarnings(as.numeric(integrated_df$q_abs_log)) else rep(NA_real_, nrow(integrated_df))
  )
  multiplier_pred <- suppressWarnings(as.numeric(integrated_df$multiplier_pred))
  integrated_df$multiplier_lo <- dplyr::coalesce(
    if ("meta_post_selection_multiplier_lo" %in% names(integrated_df)) suppressWarnings(as.numeric(integrated_df$meta_post_selection_multiplier_lo)) else rep(NA_real_, nrow(integrated_df)),
    if ("multiplier_lo" %in% names(integrated_df)) suppressWarnings(as.numeric(integrated_df$multiplier_lo)) else rep(NA_real_, nrow(integrated_df))
  )
  miss_lo <- !is.finite(integrated_df$multiplier_lo) & is.finite(multiplier_pred) & multiplier_pred > 0 & is.finite(q_log) & q_log > 0
  integrated_df$multiplier_lo[miss_lo] <- multiplier_pred[miss_lo] * exp(-q_log[miss_lo])
  integrated_df$multiplier_hi <- dplyr::coalesce(
    if ("meta_post_selection_multiplier_hi" %in% names(integrated_df)) suppressWarnings(as.numeric(integrated_df$meta_post_selection_multiplier_hi)) else rep(NA_real_, nrow(integrated_df)),
    if ("multiplier_hi" %in% names(integrated_df)) suppressWarnings(as.numeric(integrated_df$multiplier_hi)) else rep(NA_real_, nrow(integrated_df))
  )
  miss_hi <- !is.finite(integrated_df$multiplier_hi) & is.finite(multiplier_pred) & multiplier_pred > 0 & is.finite(q_log) & q_log > 0
  integrated_df$multiplier_hi[miss_hi] <- multiplier_pred[miss_hi] * exp(q_log[miss_hi])
  if (nrow(integrated_df) == 0 ||
    !all(c("anchor_species", "multiplier_pred", "multiplier_lo", "multiplier_hi") %in% names(integrated_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Integrated Anchor-Level Biomass Multiplier Summary", subtitle = "Required plotting fields were not available.", x = NULL, y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  integrated_df$selected_policy_display <- resolve_selected_policy_names(integrated_df)
  anchor_levels <- integrated_df |>
    dplyr::group_by(.data$anchor_species) |>
    dplyr::summarise(
      order_value = stats::median(.data$multiplier_pred, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$order_value, .data$anchor_species) |>
    dplyr::pull(.data$anchor_species) |>
    unique()

  integrated_df <- integrated_df |>
    dplyr::filter(
      !is.na(.data$anchor_species),
      is.finite(.data$multiplier_pred),
      is.finite(.data$multiplier_lo),
      is.finite(.data$multiplier_hi),
      .data$multiplier_pred > 0,
      .data$multiplier_lo > 0,
      .data$multiplier_hi > 0
    ) |>
    dplyr::mutate(
      anchor_species = factor(.data$anchor_species, levels = anchor_levels),
      x_pos = as.numeric(.data$anchor_species),
      selected_policy_display = if ("selected_policy_display" %in% names(integrated_df)) {
        .data$selected_policy_display
      } else {
        NA_character_
      }
    ) |>
    dplyr::group_by(.data$anchor_species) |>
    dplyr::arrange(.data$selected_policy_display, .by_group = TRUE) |>
    dplyr::mutate(
      policy_offset = if (dplyr::n() > 1) seq(-0.07, 0.07, length.out = dplyr::n()) else 0,
      selected_x_pos = .data$x_pos - 0.12 + .data$policy_offset
    ) |>
    dplyr::ungroup()
  if (nrow(integrated_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Integrated Anchor-Level Biomass Multiplier Summary", subtitle = "Required plotting fields were not available.", x = NULL, y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  red_df <- tibble::as_tibble(score_tbl)
  if (all(c("anchor_species", "admissible", "biomass_multiplier_if_replace") %in% names(red_df))) {
    red_df <- red_df |>
      dplyr::filter(.data$admissible, is.finite(.data$biomass_multiplier_if_replace), .data$biomass_multiplier_if_replace > 0) |>
      dplyr::mutate(anchor_species = factor(.data$anchor_species, levels = anchor_levels), x_pos = as.numeric(.data$anchor_species) + 0.12)
  } else {
    red_df <- tibble::tibble(x_pos = numeric(), biomass_multiplier_if_replace = numeric())
  }
  blue_df <- tibble::as_tibble(interval_tbl)
  if (all(c("anchor_species", "valid_prediction", "multiplier_pred") %in% names(blue_df))) {
    blue_df <- blue_df |>
      dplyr::filter(.data$valid_prediction, is.finite(.data$multiplier_pred), .data$multiplier_pred > 0) |>
      dplyr::mutate(anchor_species = factor(.data$anchor_species, levels = anchor_levels), x_pos = as.numeric(.data$anchor_species) - 0.12)
  } else {
    blue_df <- tibble::tibble(x_pos = numeric(), multiplier_pred = numeric())
  }
  admissible_interval_df <- if (nrow(red_df) > 0) {
    red_df |>
      dplyr::group_by(.data$anchor_species, .data$x_pos) |>
      dplyr::summarise(
        q025 = stats::quantile(.data$biomass_multiplier_if_replace, probs = 0.025, na.rm = TRUE, names = FALSE, type = 8),
        q05 = stats::quantile(.data$biomass_multiplier_if_replace, probs = 0.05, na.rm = TRUE, names = FALSE, type = 8),
        q10 = stats::quantile(.data$biomass_multiplier_if_replace, probs = 0.10, na.rm = TRUE, names = FALSE, type = 8),
        q50 = stats::quantile(.data$biomass_multiplier_if_replace, probs = 0.50, na.rm = TRUE, names = FALSE, type = 8),
        q90 = stats::quantile(.data$biomass_multiplier_if_replace, probs = 0.90, na.rm = TRUE, names = FALSE, type = 8),
        q95 = stats::quantile(.data$biomass_multiplier_if_replace, probs = 0.95, na.rm = TRUE, names = FALSE, type = 8),
        q975 = stats::quantile(.data$biomass_multiplier_if_replace, probs = 0.975, na.rm = TRUE, names = FALSE, type = 8),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols = c("q10", "q05", "q025"),
        names_to = "lower_key",
        values_to = "ymin"
      ) |>
      dplyr::mutate(
        ymax = dplyr::case_when(
          .data$lower_key == "q10" ~ .data$q90,
          .data$lower_key == "q05" ~ .data$q95,
          .data$lower_key == "q025" ~ .data$q975,
          TRUE ~ NA_real_
        ),
        interval_level = dplyr::case_when(
          .data$lower_key == "q10" ~ "80%",
          .data$lower_key == "q05" ~ "90%",
          .data$lower_key == "q025" ~ "95%",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::select("anchor_species", "x_pos", "q50", "ymin", "ymax", "interval_level") |>
      dplyr::filter(
        is.finite(.data$ymin),
        is.finite(.data$ymax),
        .data$ymin > 0,
        .data$ymax > 0
      ) |>
      dplyr::mutate(
        interval_level = factor(.data$interval_level, levels = c("80%", "90%", "95%"))
      )
  } else {
    tibble::tibble()
  }
  axis_values <- c(
    integrated_df$multiplier_lo,
    integrated_df$multiplier_hi,
    admissible_interval_df$ymin,
    admissible_interval_df$ymax,
    red_df$biomass_multiplier_if_replace,
    blue_df$multiplier_pred
  )
  axis_breaks <- multiplier_axis_breaks(axis_values)

  ggplot2::ggplot(integrated_df, ggplot2::aes(x = .data$x_pos)) +
    ggplot2::geom_vline(
      xintercept = seq_along(anchor_levels),
      linewidth = 0.35,
      colour = "grey88"
    ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_point(
      data = red_df,
      ggplot2::aes(
        x = .data$x_pos,
        y = .data$biomass_multiplier_if_replace
      ),
      inherit.aes = FALSE,
      position = ggplot2::position_jitter(width = 0.035, height = 0),
      colour = "#1b1b1b",
      alpha = 0.14,
      size = 1.2
    ) +
    ggplot2::geom_point(
      data = blue_df,
      ggplot2::aes(
        x = .data$x_pos,
        y = .data$multiplier_pred
      ),
      inherit.aes = FALSE,
      position = ggplot2::position_jitter(width = 0.028, height = 0),
      colour = "#b2182b",
      alpha = 0.18,
      size = 1.5
    ) +
    ggplot2::geom_segment(
      data = admissible_interval_df |>
        dplyr::filter(.data$interval_level == "80%"),
      ggplot2::aes(
        x = .data$x_pos + 0.12,
        xend = .data$x_pos + 0.12,
        y = .data$ymin,
        yend = .data$ymax
      ),
      colour = "#1b1b1b",
      linewidth = 5.0,
      alpha = 1.00,
      lineend = "butt",
      show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      data = admissible_interval_df |>
        dplyr::filter(.data$interval_level == "90%"),
      ggplot2::aes(
        x = .data$x_pos + 0.12,
        xend = .data$x_pos + 0.12,
        y = .data$ymin,
        yend = .data$ymax
      ),
      colour = "#1b1b1b",
      linewidth = 2.5,
      alpha = 0.90,
      lineend = "butt",
      show.legend = FALSE
    ) +
    ggplot2::geom_errorbar(
      data = admissible_interval_df |>
        dplyr::filter(.data$interval_level == "95%"),
      ggplot2::aes(
        x = .data$x_pos + 0.12,
        ymin = .data$ymin,
        ymax = .data$ymax
      ),
      colour = "#1b1b1b",
      linewidth = 1.0,
      alpha = 0.80,
      width = 0.040,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = admissible_interval_df |>
        dplyr::filter(.data$interval_level == "80%"),
      ggplot2::aes(
        x = .data$x_pos + 0.12,
        y = .data$q50
      ),
      colour = "#1b1b1b",
      size = 1.6
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = .data$selected_x_pos,
        ymin = .data$multiplier_lo,
        ymax = .data$multiplier_hi
      ),
      colour = "#b2182b",
      width = 0.12,
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$selected_x_pos,
        y = .data$multiplier_pred
      ),
      colour = "#b2182b",
      size = 2.5
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_continuous(
      breaks = seq_along(anchor_levels),
      labels = function(x) {
        lab <- anchor_levels[match(x, seq_along(anchor_levels))]
        parse(text = paste0("italic('", lab, "')"))
      },
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_log10(
      breaks = axis_breaks,
      labels = multiplier_axis_labels
    ) +
    ggplot2::annotation_logticks(sides = "b") +
    ggplot2::labs(
      x = NULL,
      y = "Biomass multiplier"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 15.75),
      axis.text = ggplot2::element_text(size = 14, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.4, colour = "black"),
      legend.position = "none"
    )
}

#' Plot FAO study distribution map
#'
#' @param model_data Model metadata table.
#' @param count_type The type of count to distribute across FAO major regions.
#' This can either be 'studies' for the number of studies, or 'models' for the
#' number of models.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
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

  # Process model data to get counts
  if (count_type == "studies") {
    model_agg <- model_data |>
      dplyr::nest_by(.data$citation, .data$fao_area) |>
      dplyr::group_by(.data$fao_area) |>
      dplyr::reframe(n = length(.data$citation))
  } else {
    model_agg <- model_data |>
      dplyr::group_by(.data$fao_area) |>
      dplyr::reframe(n = length(.data$fao_area))
  }

  # Convert
  fao_df <- sf::st_as_sf(
    fao_areas |>
      dplyr::mutate(geometry = swap_xy_sfc_if_needed(sf::st_as_sfc(.data$the_geom, crs = 4326))),
    sf_column_name = "geometry",
    crs = 4326
  ) |>
    dplyr::filter(.data$F_LEVEL == "MAJOR") |>
    dplyr::transmute(
      fao_area_chr = sub("^0+", "", as.character(.data$F_CODE)),
      area_name = dplyr::coalesce(.data$NAME_EN, .data$F_NAME, .data$F_CODE),
      .data$geometry
    ) |>
    dplyr::left_join(
      model_agg |>
        dplyr::mutate(fao_area_chr = as.character(.data$fao_area)),
      by = "fao_area_chr"
    ) |>
    dplyr::mutate(n = dplyr::coalesce(.data$n, 0L)) |>
    sfheaders::sf_to_df(fill = TRUE) |>
    dplyr::group_by(.data$fao_area_chr, .data$sfg_id, .data$multipolygon_id, .data$polygon_id, .data$linestring_id) |>
    dplyr::mutate(
      sequence_id = dplyr::row_number(),
      UID = paste0(.data$fao_area_chr, "-", .data$area_name, "-", .data$sfg_id, "-", .data$multipolygon_id, "-", .data$polygon_id)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$UID, .data$linestring_id, .data$sequence_id)

  # Get non-empty FAO areas
  nonempty <- fao_df |>
    dplyr::filter(.data$n > 0) |>
    dplyr::reframe(fao = unique(.data$fao_area_chr))

  # Get FAO count labels
  fao_labels <- sf::st_as_sf(
    fao_areas |>
      dplyr::mutate(geometry = swap_xy_sfc_if_needed(sf::st_as_sfc(.data$the_geom, crs = 4326))),
    sf_column_name = "geometry",
    crs = 4326
  ) |>
    dplyr::mutate(fao_area = sub("^0+", "", as.character(.data$F_CODE))) |>
    dplyr::filter(.data$fao_area %in% nonempty$fao) |>
    dplyr::mutate(centroid = get_centroid(.data$geometry)) |>
    dplyr::select("fao_area", "centroid") |>
    sf::st_set_geometry("centroid") |>
    {
      \(.) cbind(sf::st_drop_geometry(.), sf::st_coordinates(.))
    }() |>
    dplyr::rename(longitude = .data$X, latitude = .data$Y) |>
    dplyr::left_join(model_agg |> dplyr::mutate(fao_area = as.character(.data$fao_area)),
      by = "fao_area"
    )

  # Add polygons
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = fao_df,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$UID,
        subgroup = .data$linestring_id,
        fill = .data$n
      ),
      color = "black",
      linewidth = 0.5
    ) +
    ggplot2::geom_label(
      mapping = ggplot2::aes(x = .data$longitude, y = .data$latitude, label = .data$n),
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
#' @param show_top_candidate Logical scalar controlling whether the top
#'   candidate curve is drawn when available.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' plot_ts_panel(panel_tbl)
#' }
#'
#' @keywords internal
#' @noRd
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
      ggplot2::ggplot() +
        ggplot2::labs(x = "Length (cm)", y = "TS (dB re 1 m^2)") +
        ggplot2::theme_minimal(base_size = 11)
    )
  }
  curve_tbl$ts_panel_center <- dplyr::coalesce(
    if ("ts_center" %in% names(curve_tbl)) suppressWarnings(as.numeric(curve_tbl$ts_center)) else rep(NA_real_, nrow(curve_tbl)),
    suppressWarnings(as.numeric(curve_tbl$ts_pred))
  )
  for (lev in c("80", "90", "95", "99")) {
    q_col <- paste0("q", lev, "_log_length")
    lo_col <- paste0("ts_lo_", lev)
    hi_col <- paste0("ts_hi_", lev)
    if (q_col %in% names(curve_tbl)) {
      q_now <- suppressWarnings(as.numeric(curve_tbl[[q_col]]))
      if (lo_col %in% names(curve_tbl)) {
        lo_now <- suppressWarnings(as.numeric(curve_tbl[[lo_col]]))
        miss_lo <- !is.finite(lo_now) & is.finite(curve_tbl$ts_panel_center) & is.finite(q_now)
        lo_now[miss_lo] <- curve_tbl$ts_panel_center[miss_lo] - q_now[miss_lo]
        curve_tbl[[lo_col]] <- lo_now
      }
      if (hi_col %in% names(curve_tbl)) {
        hi_now <- suppressWarnings(as.numeric(curve_tbl[[hi_col]]))
        miss_hi <- !is.finite(hi_now) & is.finite(curve_tbl$ts_panel_center) & is.finite(q_now)
        hi_now[miss_hi] <- curve_tbl$ts_panel_center[miss_hi] + q_now[miss_hi]
        curve_tbl[[hi_col]] <- hi_now
      }
    }
  }

  # Draw non-overlapping shells for the nested intervals. This avoids the
  # misleading mixed colours produced by stacking translucent full ribbons.
  band_tbl <- dplyr::bind_rows(
    curve_tbl |>
      dplyr::transmute(
        !!reference_col := .data[[reference_col]],
        .data$length_cm,
        band = "99%",
        shell = "lower",
        ymin = .data$ts_lo_99,
        ymax = .data$ts_lo_95
      ),
    curve_tbl |>
      dplyr::transmute(
        !!reference_col := .data[[reference_col]],
        .data$length_cm,
        band = "99%",
        shell = "upper",
        ymin = .data$ts_hi_95,
        ymax = .data$ts_hi_99
      ),
    curve_tbl |>
      dplyr::transmute(
        !!reference_col := .data[[reference_col]],
        .data$length_cm,
        band = "95%",
        shell = "lower",
        ymin = .data$ts_lo_95,
        ymax = .data$ts_lo_90
      ),
    curve_tbl |>
      dplyr::transmute(
        !!reference_col := .data[[reference_col]],
        .data$length_cm,
        band = "95%",
        shell = "upper",
        ymin = .data$ts_hi_90,
        ymax = .data$ts_hi_95
      ),
    curve_tbl |>
      dplyr::transmute(
        !!reference_col := .data[[reference_col]],
        .data$length_cm,
        band = "90%",
        shell = "lower",
        ymin = .data$ts_lo_90,
        ymax = .data$ts_lo_80
      ),
    curve_tbl |>
      dplyr::transmute(
        !!reference_col := .data[[reference_col]],
        .data$length_cm,
        band = "90%",
        shell = "upper",
        ymin = .data$ts_hi_80,
        ymax = .data$ts_hi_90
      ),
    curve_tbl |>
      dplyr::transmute(
        !!reference_col := .data[[reference_col]],
        .data$length_cm,
        band = "80%",
        shell = "center",
        ymin = .data$ts_lo_80,
        ymax = .data$ts_hi_80
      )
  ) |>
    dplyr::filter(is.finite(.data$ymin), is.finite(.data$ymax), .data$ymax > .data$ymin) |>
    dplyr::mutate(band = factor(.data$band, levels = c("99%", "95%", "90%", "80%")))

  label_tbl <- curve_tbl |>
    dplyr::group_by(.data[[reference_col]]) |>
    dplyr::summarise(
      x_min = min(.data$length_cm, na.rm = TRUE),
      x_max = max(.data$length_cm, na.rm = TRUE),
      label_expr = paste0("italic('", dplyr::first(.data[[reference_col]]), "')"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      x_label = .data$x_max - 0.04 * (.data$x_max - .data$x_min),
      y_label = -60
    )

  p <- ggplot2::ggplot()
  if (nrow(band_tbl) > 0) {
    p <- p +
      ggplot2::geom_ribbon(
        data = band_tbl,
        ggplot2::aes(
          x = .data$length_cm,
          ymin = .data$ymin,
          ymax = .data$ymax,
          fill = .data$band,
          group = interaction(.data[[reference_col]], .data$band, .data$shell)
        ),
        alpha = 1,
        colour = NA
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "99%" = "#eef2f7",
          "95%" = "#d9e0ea",
          "90%" = "#bcc7d6",
          "80%" = "#96a6bc"
        ),
        name = "Uncertainty interval"
      )
  }
  p <- p +
    ggplot2::geom_line(data = curve_tbl, ggplot2::aes(x = .data$length_cm, y = .data$ts_panel_center, colour = "Selected policy", linetype = "Selected policy"), linewidth = 0.85)
  if (isTRUE(show_top_candidate) && "ts_top_candidate" %in% names(curve_tbl)) {
    p <- p +
      ggplot2::geom_line(
        data = curve_tbl,
        ggplot2::aes(x = .data$length_cm, y = .data$ts_top_candidate, colour = "Top candidate", linetype = "Top candidate"),
        linewidth = 0.75,
        alpha = 0.9
      )
  }
  p <- p +
    ggplot2::geom_line(
      data = curve_tbl,
      ggplot2::aes(x = .data$length_cm, y = .data$ts_anchor, colour = "Anchor", linetype = "Anchor"),
      linewidth = 0.75
    ) +
    ggplot2::geom_text(
      data = label_tbl,
      ggplot2::aes(x = .data$x_label, y = .data$y_label, label = .data$label_expr),
      parse = TRUE,
      hjust = 1,
      vjust = -0.25,
      size = 5.2
    )
  colour_values <- c(
    "Selected policy" = "#0057b8",
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
    ggplot2::scale_colour_manual(
      values = colour_values,
      name = expression(italic(TS) ~ predictions),
      labels = c("Selected policy" = "Selected policy", "Anchor" = "SWFSC", "Top candidate" = "Top candidate")
    ) +
    ggplot2::scale_linetype_manual(
      values = linetype_values,
      name = expression(italic(TS) ~ predictions),
      labels = c("Selected policy" = "Selected policy", "Anchor" = "SWFSC", "Top candidate" = "Top candidate")
    ) +
    ggplot2::facet_wrap(stats::as.formula(paste("~", reference_col)), ncol = 2, scales = "free_x") +
    ggplot2::labs(
      x = "Length (cm)",
      y = expression(italic(TS) ~ (dB ~ re. ~ 1 ~ m^2))
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 15.75),
      axis.text = ggplot2::element_text(size = 14, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.4, colour = "black"),
      legend.title = ggplot2::element_text(size = 15.75),
      legend.text = ggplot2::element_text(size = 14)
    )
}

#' Plot selected-policy coefficient intervals
#'
#' @param coefficient_tbl Selected-policy table with coefficient interval columns.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
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
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df$estimate_slope <- resolve_one(plot_df, c("policy_slope_len"))
  plot_df$estimate_intercept <- resolve_one(plot_df, c("policy_intercept_len"))
  plot_df$anchor_slope <- suppressWarnings(as.numeric(resolve_one(
    plot_df,
    c("anchor_slope_standard", "anchor_slope_len")
  )))
  plot_df$anchor_intercept <- suppressWarnings(as.numeric(resolve_one(
    plot_df,
    c("anchor_intercept_standard", "anchor_intercept_len")
  )))
  post_df <- dplyr::bind_rows(
    plot_df |>
      dplyr::transmute(
        .data$anchor_species,
        interval_type = "Plausible competing strategies",
        parameter = "Slope",
        estimate = .data$estimate_slope,
        lo = resolve_one(plot_df, c("policy_slope_len_lo_95", "policy_slope_len_lo_95.x", "policy_slope_len_lo_95.y")),
        hi = resolve_one(plot_df, c("policy_slope_len_hi_95", "policy_slope_len_hi_95.x", "policy_slope_len_hi_95.y")),
        competitor_n = suppressWarnings(as.numeric(resolve_one(plot_df, c("policy_coefficient_competitor_n"))))
      ),
    plot_df |>
      dplyr::transmute(
        .data$anchor_species,
        interval_type = "Plausible competing strategies",
        parameter = "Intercept",
        estimate = .data$estimate_intercept,
        lo = resolve_one(plot_df, c("policy_intercept_len_lo_95", "policy_intercept_len_lo_95.x", "policy_intercept_len_lo_95.y")),
        hi = resolve_one(plot_df, c("policy_intercept_len_hi_95", "policy_intercept_len_hi_95.x", "policy_intercept_len_hi_95.y")),
        competitor_n = suppressWarnings(as.numeric(resolve_one(plot_df, c("policy_coefficient_competitor_n"))))
      )
  )
  conditional_available <- all(c(
    "conditional_policy_slope_len_lo_95",
    "conditional_policy_slope_len_hi_95",
    "conditional_policy_intercept_len_lo_95",
    "conditional_policy_intercept_len_hi_95"
  ) %in% names(plot_df))
  conditional_df <- if (isTRUE(conditional_available)) {
    dplyr::bind_rows(
      plot_df |>
        dplyr::transmute(
          .data$anchor_species,
          interval_type = "Selected strategy",
          parameter = "Slope",
          estimate = .data$estimate_slope,
          lo = .data$conditional_policy_slope_len_lo_95,
          hi = .data$conditional_policy_slope_len_hi_95,
          competitor_n = 1
        ),
      plot_df |>
        dplyr::transmute(
          .data$anchor_species,
          interval_type = "Selected strategy",
          parameter = "Intercept",
          estimate = .data$estimate_intercept,
          lo = .data$conditional_policy_intercept_len_lo_95,
          hi = .data$conditional_policy_intercept_len_hi_95,
          competitor_n = 1
        )
    )
  } else {
    tibble::tibble()
  }
  conditional_key <- if (nrow(conditional_df) > 0L) {
    conditional_df |>
      dplyr::select(
        "anchor_species",
        "parameter",
        conditional_lo = "lo",
        conditional_hi = "hi"
      )
  } else {
    tibble::tibble(
      anchor_species = character(),
      parameter = character(),
      conditional_lo = numeric(),
      conditional_hi = numeric()
    )
  }
  post_df <- post_df |>
    dplyr::left_join(conditional_key, by = c("anchor_species", "parameter")) |>
    dplyr::mutate(
      show_post = dplyr::coalesce(.data$competitor_n, 1) > 1 &
        (
          !is.finite(.data$conditional_lo) |
            !is.finite(.data$conditional_hi) |
            abs(.data$lo - .data$conditional_lo) > 1e-8 |
            abs(.data$hi - .data$conditional_hi) > 1e-8
        )
    ) |>
    dplyr::filter(.data$show_post) |>
    dplyr::select(-dplyr::any_of(c("conditional_lo", "conditional_hi", "show_post")))

  long_df <- dplyr::bind_rows(conditional_df, post_df) |>
    dplyr::filter(
      !is.na(.data$anchor_species),
      is.finite(.data$estimate),
      is.finite(.data$lo),
      is.finite(.data$hi)
    ) |>
    dplyr::mutate(
      anchor_species = factor(.data$anchor_species, levels = rev(unique(plot_df$anchor_species))),
      parameter = factor(.data$parameter, levels = c("Slope", "Intercept")),
      interval_type = factor(.data$interval_type, levels = c("Selected strategy", "Plausible competing strategies"))
    )
  swfsc_df <- dplyr::bind_rows(
    plot_df |>
      dplyr::transmute(
        .data$anchor_species,
        parameter = "Slope",
        estimate = .data$anchor_slope
      ),
    plot_df |>
      dplyr::transmute(
        .data$anchor_species,
        parameter = "Intercept",
        estimate = .data$anchor_intercept
      )
  ) |>
    dplyr::filter(
      !is.na(.data$anchor_species),
      is.finite(.data$estimate)
    ) |>
    dplyr::mutate(
      anchor_species = factor(.data$anchor_species, levels = rev(unique(plot_df$anchor_species))),
      parameter = factor(.data$parameter, levels = c("Slope", "Intercept"))
    )
  if (nrow(long_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }

  species_levels <- levels(long_df$anchor_species)
  base_pos <- stats::setNames(seq_along(species_levels), species_levels)
  long_df$y_pos <- unname(base_pos[as.character(long_df$anchor_species)])
  long_df$y_plot <- ifelse(
    identical(as.character(long_df$interval_type), "Plausible competing strategies"),
    long_df$y_pos - 0.14,
    long_df$y_pos
  )
  long_df$y_plot <- ifelse(
    as.character(long_df$interval_type) == "Plausible competing strategies",
    long_df$y_pos - 0.14,
    long_df$y_pos
  )
  cond_df <- long_df[as.character(long_df$interval_type) == "Selected strategy", , drop = FALSE]
  post_plot_df <- long_df[as.character(long_df$interval_type) == "Plausible competing strategies", , drop = FALSE]
  if (nrow(swfsc_df) > 0) {
    swfsc_df$y_pos <- unname(base_pos[as.character(swfsc_df$anchor_species)])
    swfsc_df$y_plot <- swfsc_df$y_pos
  }
  panel_label_df <- long_df |>
    dplyr::group_by(.data$parameter) |>
    dplyr::summarise(
      x_min = min(c(.data$lo, .data$estimate), na.rm = TRUE),
      x_max = max(c(.data$hi, .data$estimate), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      x = .data$x_min + 0.01 * (.data$x_max - .data$x_min),
      y = length(species_levels) + 0.42,
      label = as.character(.data$parameter)
    )
  slope_ref_df <- tibble::tibble(
    parameter = factor("Slope", levels = c("Slope", "Intercept")),
    xintercept = 20
  )

  ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept = seq_along(species_levels),
      linewidth = 0.35,
      colour = "grey88"
    ) +
    ggplot2::geom_vline(
      data = slope_ref_df,
      ggplot2::aes(xintercept = .data$xintercept),
      linetype = "dashed",
      linewidth = 0.5,
      colour = "grey65"
    ) +
    ggplot2::geom_errorbar(
      data = post_plot_df,
      ggplot2::aes(
        y = .data$y_plot,
        x = .data$estimate,
        xmin = .data$lo,
        xmax = .data$hi,
        colour = .data$interval_type
      ),
      orientation = "y",
      width = 0.14,
      linewidth = 0.8,
      show.legend = nrow(post_plot_df) > 0
    ) +
    ggplot2::geom_errorbar(
      data = cond_df,
      ggplot2::aes(
        y = .data$y_plot,
        x = .data$estimate,
        xmin = .data$lo,
        xmax = .data$hi,
        colour = .data$interval_type
      ),
      orientation = "y",
      width = 0.14,
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = cond_df,
      ggplot2::aes(
        x = .data$estimate,
        y = .data$y_plot,
        colour = .data$interval_type
      ),
      size = 2.4
    ) +
    ggplot2::geom_point(
      data = swfsc_df,
      ggplot2::aes(
        x = .data$estimate,
        y = .data$y_plot,
        colour = "SWFSC model"
      ),
      size = 2.4
    ) +
    ggplot2::facet_wrap(~parameter, ncol = 1, scales = "free_x") +
    ggplot2::geom_text(
      data = panel_label_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      size = 7
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(species_levels),
      labels = function(x) parse(text = paste0("italic('", species_levels[as.integer(x)], "')")),
      limits = c(0.5, length(species_levels) + 0.5),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Selected strategy" = "#1b1b1b",
        "Plausible competing strategies" = "#b2182b",
        "SWFSC model" = "#2166ac"
      ),
      name = NULL,
      drop = FALSE
    ) +
    ggplot2::labs(
      x = "Coefficient value",
      y = NULL
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 15.75),
      axis.text = ggplot2::element_text(size = 14, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.4, colour = "black"),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 14),
      legend.position = "top",
      legend.direction = "horizontal"
    )
}

#' Plot selected biomass multiplier against expected anchor length
#'
#' @param anchor_tbl Selected-policy table with multiplier intervals and expected length.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
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
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Selected Biomass Multiplier vs Expected Anchor Length", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::filter(
      !is.na(.data$anchor_species) & nzchar(.data$anchor_species),
      is.finite(.data$expected_length_cm),
      is.finite(.data$multiplier_pred),
      is.finite(.data$meta_post_selection_multiplier_lo),
      is.finite(.data$meta_post_selection_multiplier_hi),
      .data$meta_post_selection_multiplier_lo > 0,
      .data$meta_post_selection_multiplier_hi > 0,
      .data$multiplier_pred > 0
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Selected Biomass Multiplier vs Expected Anchor Length", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$expected_length_cm,
      y = .data$multiplier_pred,
      ymin = .data$meta_post_selection_multiplier_lo,
      ymax = .data$meta_post_selection_multiplier_hi,
      label = .data$anchor_species
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

#' Plot length-specific biomass multiplier spectrum
#'
#' @param curve_tbl TS panel table with anchor and selected-policy curves.
#' @param reference_col Facet-label column.
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
plot_multiplier_length_spectrum <- function(curve_tbl,
                                            reference_col = "anchor_species") {
  curve_tbl <- tibble::as_tibble(curve_tbl)
  curve_tbl$ts_panel_center <- dplyr::coalesce(
    if ("ts_center" %in% names(curve_tbl)) suppressWarnings(as.numeric(curve_tbl$ts_center)) else rep(NA_real_, nrow(curve_tbl)),
    suppressWarnings(as.numeric(curve_tbl$ts_pred))
  )
  if ("q95_log_length" %in% names(curve_tbl)) {
    q95_now <- suppressWarnings(as.numeric(curve_tbl$q95_log_length))
    if ("ts_lo_95" %in% names(curve_tbl)) {
      lo_now <- suppressWarnings(as.numeric(curve_tbl$ts_lo_95))
      miss_lo <- !is.finite(lo_now) & is.finite(curve_tbl$ts_panel_center) & is.finite(q95_now)
      lo_now[miss_lo] <- curve_tbl$ts_panel_center[miss_lo] - q95_now[miss_lo]
      curve_tbl$ts_lo_95 <- lo_now
    }
    if ("ts_hi_95" %in% names(curve_tbl)) {
      hi_now <- suppressWarnings(as.numeric(curve_tbl$ts_hi_95))
      miss_hi <- !is.finite(hi_now) & is.finite(curve_tbl$ts_panel_center) & is.finite(q95_now)
      hi_now[miss_hi] <- curve_tbl$ts_panel_center[miss_hi] + q95_now[miss_hi]
      curve_tbl$ts_hi_95 <- hi_now
    }
  }
  needed <- c(reference_col, "length_cm", "ts_anchor", "ts_pred", "ts_lo_95", "ts_hi_95")
  if (nrow(curve_tbl) == 0 || !all(needed %in% names(curve_tbl))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "Length (cm)", y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- curve_tbl |>
    dplyr::mutate(
      multiplier_at_length = 10^((.data$ts_anchor - .data$ts_pred) / 10),
      multiplier_lo_95 = 10^((.data$ts_anchor - .data$ts_hi_95) / 10),
      multiplier_hi_95 = 10^((.data$ts_anchor - .data$ts_lo_95) / 10)
    ) |>
    dplyr::filter(
      is.finite(.data$multiplier_at_length),
      is.finite(.data$multiplier_lo_95),
      is.finite(.data$multiplier_hi_95),
      .data$multiplier_at_length > 0,
      .data$multiplier_lo_95 > 0,
      .data$multiplier_hi_95 > 0
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "Length (cm)", y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$length_cm, y = .data$multiplier_at_length)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$multiplier_lo_95, ymax = .data$multiplier_hi_95), fill = "#9ebcda", alpha = 0.30) +
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
#' @keywords internal
#' @noRd
plot_all_intervals <- function(interval_tbl,
                               reference_name) {
  # Order the policies by their predicted multiplier before drawing the
  # one-reference interval comparison.
  plot_df <- tibble::as_tibble(interval_tbl)
  q_log <- dplyr::coalesce(
    if ("meta_q_abs_log_total" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$meta_q_abs_log_total)) else rep(NA_real_, nrow(plot_df)),
    if ("q_abs_log_total" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$q_abs_log_total)) else rep(NA_real_, nrow(plot_df)),
    if ("meta_q_abs_log" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$meta_q_abs_log)) else rep(NA_real_, nrow(plot_df)),
    if ("q_abs_log" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$q_abs_log)) else rep(NA_real_, nrow(plot_df))
  )
  multiplier_pred <- suppressWarnings(as.numeric(plot_df$multiplier_pred))
  plot_df$multiplier_lo <- dplyr::coalesce(
    if ("meta_post_selection_multiplier_lo" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$meta_post_selection_multiplier_lo)) else rep(NA_real_, nrow(plot_df)),
    if ("multiplier_lo" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$multiplier_lo)) else rep(NA_real_, nrow(plot_df))
  )
  miss_lo <- !is.finite(plot_df$multiplier_lo) & is.finite(multiplier_pred) & multiplier_pred > 0 & is.finite(q_log) & q_log > 0
  plot_df$multiplier_lo[miss_lo] <- multiplier_pred[miss_lo] * exp(-q_log[miss_lo])
  plot_df$multiplier_hi <- dplyr::coalesce(
    if ("meta_post_selection_multiplier_hi" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$meta_post_selection_multiplier_hi)) else rep(NA_real_, nrow(plot_df)),
    if ("multiplier_hi" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$multiplier_hi)) else rep(NA_real_, nrow(plot_df))
  )
  miss_hi <- !is.finite(plot_df$multiplier_hi) & is.finite(multiplier_pred) & multiplier_pred > 0 & is.finite(q_log) & q_log > 0
  plot_df$multiplier_hi[miss_hi] <- multiplier_pred[miss_hi] * exp(q_log[miss_hi])
  plot_df$is_selected <- dplyr::coalesce(
    if ("is_selected.y" %in% names(plot_df)) as.logical(plot_df$is_selected.y) else rep(NA, nrow(plot_df)),
    if ("is_selected" %in% names(plot_df)) as.logical(plot_df$is_selected) else rep(NA, nrow(plot_df)),
    if ("is_selected.x" %in% names(plot_df)) as.logical(plot_df$is_selected.x) else rep(NA, nrow(plot_df)),
    FALSE
  )
  plot_df$policy_display <- resolve_policy_display_names(plot_df)
  if (nrow(plot_df) == 0 || !all(c("policy_display", "multiplier_pred", "multiplier_lo", "multiplier_hi", "is_selected") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::filter(
      is.finite(.data$multiplier_pred),
      is.finite(.data$multiplier_lo),
      is.finite(.data$multiplier_hi),
      .data$multiplier_pred > 0,
      .data$multiplier_lo > 0,
      .data$multiplier_hi > 0
    ) |>
    dplyr::arrange(.data$multiplier_pred, .data$policy_display)
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  policy_levels <- unique(plot_df$policy_display)
  plot_df$policy_display <- factor(plot_df$policy_display, levels = rev(policy_levels))
  background_df <- plot_df |>
    dplyr::filter(!.data$is_selected)
  selected_df <- plot_df |>
    dplyr::filter(.data$is_selected)

  ggplot2::ggplot(plot_df, ggplot2::aes(y = .data$policy_display)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_segment(
      data = background_df,
      ggplot2::aes(
        x = .data$multiplier_lo,
        xend = .data$multiplier_hi,
        yend = .data$policy_display
      ),
      linewidth = 0.75,
      colour = "grey70",
      alpha = 0.65
    ) +
    ggplot2::geom_point(
      data = background_df,
      ggplot2::aes(x = .data$multiplier_pred),
      size = 2.4,
      colour = "grey60",
      alpha = 0.75
    ) +
    ggplot2::geom_segment(
      data = selected_df,
      ggplot2::aes(
        x = .data$multiplier_lo,
        xend = .data$multiplier_hi,
        yend = .data$policy_display
      ),
      linewidth = 1.05,
      colour = "#2166ac"
    ) +
    ggplot2::geom_point(
      data = selected_df,
      ggplot2::aes(x = .data$multiplier_pred),
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
#' @keywords internal
#' @noRd
plot_interval_panel <- function(interval_tbl) {
  # Keep each facet ordered by the within-reference multiplier ranking before
  # drawing the combined panel.
  plot_df <- tibble::as_tibble(interval_tbl)
  q_log <- dplyr::coalesce(
    if ("meta_q_abs_log_total" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$meta_q_abs_log_total)) else rep(NA_real_, nrow(plot_df)),
    if ("q_abs_log_total" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$q_abs_log_total)) else rep(NA_real_, nrow(plot_df)),
    if ("meta_q_abs_log" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$meta_q_abs_log)) else rep(NA_real_, nrow(plot_df)),
    if ("q_abs_log" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$q_abs_log)) else rep(NA_real_, nrow(plot_df))
  )
  multiplier_pred <- suppressWarnings(as.numeric(plot_df$multiplier_pred))
  plot_df$multiplier_lo <- dplyr::coalesce(
    if ("meta_post_selection_multiplier_lo" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$meta_post_selection_multiplier_lo)) else rep(NA_real_, nrow(plot_df)),
    if ("multiplier_lo" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$multiplier_lo)) else rep(NA_real_, nrow(plot_df))
  )
  miss_lo <- !is.finite(plot_df$multiplier_lo) & is.finite(multiplier_pred) & multiplier_pred > 0 & is.finite(q_log) & q_log > 0
  plot_df$multiplier_lo[miss_lo] <- multiplier_pred[miss_lo] * exp(-q_log[miss_lo])
  plot_df$multiplier_hi <- dplyr::coalesce(
    if ("meta_post_selection_multiplier_hi" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$meta_post_selection_multiplier_hi)) else rep(NA_real_, nrow(plot_df)),
    if ("multiplier_hi" %in% names(plot_df)) suppressWarnings(as.numeric(plot_df$multiplier_hi)) else rep(NA_real_, nrow(plot_df))
  )
  miss_hi <- !is.finite(plot_df$multiplier_hi) & is.finite(multiplier_pred) & multiplier_pred > 0 & is.finite(q_log) & q_log > 0
  plot_df$multiplier_hi[miss_hi] <- multiplier_pred[miss_hi] * exp(q_log[miss_hi])
  plot_df$is_selected <- dplyr::coalesce(
    if ("is_selected.y" %in% names(plot_df)) as.logical(plot_df$is_selected.y) else rep(NA, nrow(plot_df)),
    if ("is_selected" %in% names(plot_df)) as.logical(plot_df$is_selected) else rep(NA, nrow(plot_df)),
    if ("is_selected.x" %in% names(plot_df)) as.logical(plot_df$is_selected.x) else rep(NA, nrow(plot_df)),
    FALSE
  )
  plot_df$policy_display <- resolve_policy_display_names(plot_df)
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "policy_display", "multiplier_pred", "multiplier_lo", "multiplier_hi", "is_selected") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::filter(
      !is.na(.data$anchor_species),
      is.finite(.data$multiplier_pred),
      is.finite(.data$multiplier_lo),
      is.finite(.data$multiplier_hi),
      .data$multiplier_pred > 0,
      .data$multiplier_lo > 0,
      .data$multiplier_hi > 0
    ) |>
    dplyr::ungroup()
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = NULL, y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  policy_levels <- plot_df |>
    dplyr::group_by(.data$policy_display) |>
    dplyr::summarise(order_value = stats::median(.data$multiplier_pred, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$order_value, .data$policy_display) |>
    dplyr::pull(.data$policy_display)
  plot_df <- plot_df |>
    dplyr::mutate(
      policy_display = factor(.data$policy_display, levels = rev(policy_levels)),
      anchor_species = factor(.data$anchor_species, levels = unique(.data$anchor_species))
    )
  background_df <- plot_df |>
    dplyr::filter(!.data$is_selected)
  selected_df <- plot_df |>
    dplyr::filter(.data$is_selected)

  ggplot2::ggplot(plot_df, ggplot2::aes(y = .data$policy_display)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_segment(
      data = background_df,
      ggplot2::aes(
        x = .data$multiplier_lo,
        xend = .data$multiplier_hi,
        yend = .data$policy_display
      ),
      linewidth = 0.65,
      colour = "grey70",
      alpha = 0.65
    ) +
    ggplot2::geom_point(
      data = background_df,
      ggplot2::aes(x = .data$multiplier_pred),
      size = 2.1,
      colour = "grey60",
      alpha = 0.75
    ) +
    ggplot2::geom_segment(
      data = selected_df,
      ggplot2::aes(
        x = .data$multiplier_lo,
        xend = .data$multiplier_hi,
        yend = .data$policy_display
      ),
      linewidth = 0.95,
      colour = "#2166ac"
    ) +
    ggplot2::geom_point(
      data = selected_df,
      ggplot2::aes(x = .data$multiplier_pred),
      size = 2.8,
      colour = "#2166ac"
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$anchor_species)) +
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
#' @keywords internal
#' @noRd
plot_policy_stability <- function(sens_tbl,
                                  baseline_tbl,
                                  scenario_labs = NULL) {
  # Encode categorical policy-selection changes so this plot is distinct from
  # the numeric multiplier-drift heatmap.
  plot_df <- tibble::as_tibble(sens_tbl)
  if (nrow(plot_df) == 0 || !"scenario" %in% names(plot_df) || !"anchor_species" %in% names(plot_df)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Representative Policy Stability and Multiplier Drift", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }
  if (!is.null(scenario_labs)) {
    plot_df <- plot_df |>
      dplyr::mutate(scenario_label = dplyr::recode(.data$scenario, !!!scenario_labs))
  } else {
    plot_df <- plot_df |>
      dplyr::mutate(scenario_label = .data$scenario)
  }
  if (!"policy_changed" %in% names(plot_df)) plot_df$policy_changed <- FALSE
  if (!"display_changed" %in% names(plot_df)) plot_df$display_changed <- FALSE
  if (!"scenario_status" %in% names(plot_df)) plot_df$scenario_status <- "ok"

  # Accept the renamed equivalent-set change field
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
      scenario_label = factor(as.character(.data$scenario_label), levels = unique(as.character(.data$scenario_label))),
      anchor_species = factor(as.character(.data$anchor_species), levels = anchor_order),
      equiv_change = dplyr::coalesce(.data$equivalent_set_changed, .data$equiv_set_changed, FALSE),
      stability_state = dplyr::case_when(
        as.character(scenario_status) != "ok" ~ "Scenario failed",
        dplyr::coalesce(policy_changed, FALSE) ~ "Selected policy changed",
        dplyr::coalesce(display_changed, FALSE) ~ "Displayed strategy changed",
        dplyr::coalesce(equiv_change, FALSE) ~ "Equivalent set changed",
        TRUE ~ "Selection unchanged"
      ),
      stability_state = factor(
        .data$stability_state,
        levels = c(
          "Selection unchanged",
          "Equivalent set changed",
          "Displayed strategy changed",
          "Selected policy changed",
          "Scenario failed"
        )
      )
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$scenario_label, y = .data$anchor_species)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$stability_state), colour = "white", linewidth = 0.6) +
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
#' @keywords internal
#' @noRd
plot_multiplier_drift <- function(sens_tbl,
                                  baseline_tbl,
                                  scenario_labs = NULL) {
  # Join baseline multipliers once and plot only the multiplier drift, without
  # the selection-change overlays used in the stability plot.
  plot_df <- tibble::as_tibble(sens_tbl)
  if (nrow(plot_df) == 0 || !"scenario" %in% names(plot_df) || !"anchor_species" %in% names(plot_df) || !"anchor_model_id" %in% names(plot_df) || !"multiplier_pred" %in% names(plot_df)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Multiplier Sensitivity Relative to Baseline", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }
  if (!is.null(scenario_labs)) {
    plot_df <- plot_df |>
      dplyr::mutate(scenario_label = dplyr::recode(.data$scenario, !!!scenario_labs))
  } else {
    plot_df <- plot_df |>
      dplyr::mutate(scenario_label = .data$scenario)
  }

  base_df <- tibble::as_tibble(baseline_tbl) |>
    dplyr::select("anchor_model_id", baseline_multiplier = "multiplier_pred")
  anchor_order <- unique(c(
    as.character(tibble::as_tibble(baseline_tbl)$anchor_species),
    as.character(plot_df$anchor_species)
  ))
  anchor_order <- anchor_order[!is.na(anchor_order) & nzchar(anchor_order)]
  plot_df <- plot_df |>
    dplyr::left_join(base_df, by = "anchor_model_id") |>
    dplyr::mutate(
      scenario_label = factor(as.character(.data$scenario_label), levels = unique(as.character(.data$scenario_label))),
      anchor_species = factor(as.character(.data$anchor_species), levels = anchor_order),
      delta_log_multiplier = log(.data$multiplier_pred / .data$baseline_multiplier),
      label = sprintf("%+.2f", .data$delta_log_multiplier)
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$scenario_label, y = .data$anchor_species, fill = .data$delta_log_multiplier)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 3) +
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
#' @keywords internal
#' @noRd
plot_sensitivity_overview <- function(plot_tbl) {
  # Plot the already prepared long-form scenario summary so the function only
  # handles the segment-plus-point rendering.
  plot_df <- tibble::as_tibble(plot_tbl)
  if (nrow(plot_df) == 0 || !all(c("value", "scenario_label", "metric", "panel") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Sensitivity Scenario Summary", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }
  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$value, y = forcats::fct_rev(.data$scenario_label), colour = .data$metric)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$value, yend = forcats::fct_rev(.data$scenario_label)), linewidth = 0.9, alpha = 0.75) +
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
#' @keywords internal
#' @noRd
plot_tuning_variation <- function(plot_tbl,
                                  block_col = "block") {
  # Reorder the blocks by their mean multiplier before drawing the resample
  # variability intervals and point estimates.
  plot_df <- tibble::as_tibble(plot_tbl)
  if (nrow(plot_df) == 0 || !all(c(block_col, "mean_multiplier", "q05_multiplier", "q95_multiplier", "sd_multiplier") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Tuning Block Multipliers Across Resamples", subtitle = "Required plotting fields were not available.", x = "Block multiplier", y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }
  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(.block = forcats::fct_reorder(.data[[block_col]], .data$mean_multiplier)),
    ggplot2::aes(x = .data$mean_multiplier, y = .data$.block)
  ) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$q05_multiplier, xmax = .data$q95_multiplier), linewidth = 1.1, colour = "#6baed6") +
    ggplot2::geom_point(size = 3, colour = "#08519c") +
    ggplot2::geom_text(ggplot2::aes(label = .data$sprintf("sd=%.2f", .data$sd_multiplier)), nudge_y = 0.22, size = 3, colour = "#444444") +
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
#' @keywords internal
#' @noRd
plot_anchor_audit <- function(audit_tbl) {
  audit_tbl <- tibble::as_tibble(audit_tbl)
  if (nrow(audit_tbl) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "Representative Policy Audit", subtitle = "Required plotting fields were not available.", x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 11)
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
    dplyr::mutate(anchor_species = forcats::fct_inorder(.data$anchor_species)) |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    tidyr::pivot_longer(cols = -c("anchor_species", "selected_policy_display"), names_to = "metric", values_to = "value") |>
    dplyr::mutate(
      metric = factor(
        dplyr::recode(.data$metric, !!!metric_labs),
        levels = unname(metric_labs[names(metric_labs) %in% names(audit_tbl)])
      )
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$value, y = forcats::fct_rev(.data$anchor_species), colour = .data$selected_policy_display)) +
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
#' @keywords internal
#' @noRd
plot_field_missing <- function(field_tbl) {
  # Reorder the fields by missing fraction before drawing the one-dimensional
  # missingness audit bar chart.
  plot_df <- tibble::as_tibble(field_tbl)
  if (nrow(plot_df) == 0 || !all(c("field", "missing_fraction") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Missingness Across Key Metadata Fields", subtitle = "Required plotting fields were not available.", x = "Missing fraction", y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }
  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(field = forcats::fct_reorder(.data$field, .data$missing_fraction)),
    ggplot2::aes(x = .data$missing_fraction, y = .data$field)
  ) +
    ggplot2::geom_col(fill = "#756bb1", width = 0.7) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      title = "Missingness Across Key Metadata Fields",
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
#' @keywords internal
#' @noRd
plot_anchor_missing <- function(anchor_tbl) {
  # Order the anchor labels once and draw the fraction excluded by the
  # missingness gate for each reference species.
  plot_df <- tibble::as_tibble(anchor_tbl)
  if (nrow(plot_df) == 0 || !all(c("anchor_species", "prop_fail_missing_metadata") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Candidate Exclusion from Missing Key Metadata", subtitle = "Required plotting fields were not available.", x = "Excluded by missingness gate", y = NULL) +
      ggplot2::theme_minimal(base_size = 11))
  }
  ggplot2::ggplot(
    plot_df |>
      dplyr::mutate(anchor_species = forcats::fct_inorder(.data$anchor_species)),
    ggplot2::aes(x = .data$prop_fail_missing_metadata, y = forcats::fct_rev(.data$anchor_species))
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
#'
#' @return A ggplot object.
#'
#' @keywords internal
#' @noRd
plot_ordination_cluster_hulls <- function(points_tbl,
                                          hull_tbl,
                                          cluster_col = "policy_cluster_id",
                                          reference_col = "is_reference",
                                          label_col = "species_name") {
  # Normalize the point and hull tables once before layering the hull polygons,
  # ordination cloud, and highlighted reference labels.
  point_df <- tibble::as_tibble(points_tbl)
  hull_df <- tibble::as_tibble(hull_tbl)
  if (nrow(point_df) == 0 || !all(c("MDS1", "MDS2") %in% names(point_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "NMDS1", y = "NMDS2") +
      ggplot2::theme_minimal(base_size = 11))
  }
  cluster_name <- cluster_col
  if (!(cluster_name %in% names(point_df))) {
    cluster_candidates <- c("nmds_cluster_id", "nmds_cluster", "species_cluster_id", "policy_cluster_id")
    matched2 <- cluster_candidates[cluster_candidates %in% names(point_df)]
    cluster_name <- if (length(matched2) > 0L) matched2[[1L]] else NA_character_
  }
  if (is.null(cluster_name) || length(cluster_name) == 0 || is.na(cluster_name) || !(cluster_name %in% names(point_df))) {
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

  ggplot2::ggplot(point_df, ggplot2::aes(x = .data$MDS1, y = .data$MDS2, colour = .data[[cluster_name]])) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_polygon(
      data = hull_df,
      ggplot2::aes(x = .data$MDS1, y = .data$MDS2, fill = .data[[cluster_name]], group = .data[[cluster_name]]),
      inherit.aes = FALSE,
      alpha = 0.12,
      colour = NA
    ) +
    ggplot2::geom_path(
      data = hull_df,
      ggplot2::aes(x = .data$MDS1, y = .data$MDS2, colour = .data[[cluster_name]], group = .data[[cluster_name]]),
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
      ggplot2::aes(x = .data$MDS1, y = .data$MDS2, label = .data[[label_col]]),
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
#' @keywords internal
#' @noRd
plot_length_density <- function(length_tbl,
                                anchor_label) {
  # Draw the supplied length-density support directly so the function remains a
  # pure plotting helper.
  plot_df <- tibble::as_tibble(length_tbl)
  if (nrow(plot_df) == 0 || !all(c("length_cm", "f_len") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "Length (cm)", y = "f(L)") +
      ggplot2::theme_minimal(base_size = 11))
  }
  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$length_cm, y = .data$f_len)) +
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
#' @keywords internal
#' @noRd
plot_length_density_panel <- function(length_tbl,
                                      reference_col = "anchor_species") {
  plot_df <- tibble::as_tibble(length_tbl)
  if (nrow(plot_df) == 0 || !all(c(reference_col, "length_cm", "f_len") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "Length (cm)", y = "f(L)") +
      ggplot2::theme_minimal(base_size = 11))
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$length_cm, y = .data$f_len)) +
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
#' @keywords internal
#' @noRd
plot_ts_ribbon <- function(ribbon_tbl,
                           anchor_label) {
  # Use explicit ribbon bounds when they are present, otherwise fall back to
  # the mean plus-or-minus one supplied standard deviation.
  plot_df <- tibble::as_tibble(ribbon_tbl)
  if (nrow(plot_df) == 0 || !all(c("length_cm", "ts_mean") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = paste0("Weighted TS Ribbon [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Length (cm)", y = "TS (dB re 1 m^2)") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_df <- plot_df |>
    dplyr::mutate(
      ribbon_low = dplyr::coalesce(.data$ts_lo, .data$ts_mean - .data$ts_sd),
      ribbon_high = dplyr::coalesce(.data$ts_hi, .data$ts_mean + .data$ts_sd)
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$length_cm, y = .data$ts_mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ribbon_low, ymax = .data$ribbon_high), alpha = 0.2, fill = "#3182bd") +
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
#' @keywords internal
#' @noRd
plot_model_weights <- function(weight_tbl,
                               anchor_label) {
  # Prefer the final admissible weight when it exists; otherwise fall back to
  # the raw combined kernel weight.
  plot_df <- tibble::as_tibble(weight_tbl)
  if (!("w_adm" %in% names(plot_df) || "w_combined" %in% names(plot_df)) ||
    !("combined_distance" %in% names(plot_df) || "d_species" %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "Distance to reference", y = "Model weight") +
      ggplot2::theme_minimal(base_size = 11))
  }
  if (!"w_adm" %in% names(plot_df)) plot_df$w_adm <- NA_real_
  if (!"w_combined" %in% names(plot_df)) plot_df$w_combined <- NA_real_
  if (!"combined_distance" %in% names(plot_df)) plot_df$combined_distance <- NA_real_
  if (!"d_species" %in% names(plot_df)) plot_df$d_species <- NA_real_
  plot_df <- plot_df |>
    dplyr::mutate(
      plot_weight = dplyr::coalesce(.data$w_adm, .data$w_combined),
      plot_distance = dplyr::coalesce(.data$combined_distance, .data$d_species)
    ) |>
    dplyr::filter(is.finite(.data$plot_weight), is.finite(.data$plot_distance), .data$plot_weight >= 0)
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "Distance to reference", y = "Model weight") +
      ggplot2::theme_minimal(base_size = 11))
  }
  if ("swimbladder_type" %in% names(plot_df)) {
    plot_df$group_val <- dplyr::coalesce(as.character(plot_df$swimbladder_type), "unknown")
  } else {
    plot_df$group_val <- "unknown"
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$plot_distance, y = .data$plot_weight)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$group_val),
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
#' @keywords internal
#' @noRd
plot_biomass_sensitivity <- function(sensitivity_tbl,
                                     anchor_label,
                                     summary_tbl = NULL) {
  # Reduce the candidate table to finite positive biomass multipliers before
  # drawing the weighted histogram on the log scale.
  plot_df <- tibble::as_tibble(sensitivity_tbl)
  if (!"biomass_multiplier_if_replace" %in% names(plot_df)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = paste0("Biomass Sensitivity [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Biomass multiplier relative to the reference model", y = "Weighted model count") +
      ggplot2::theme_minimal(base_size = 11))
  }
  if (!"w_adm" %in% names(plot_df)) plot_df$w_adm <- NA_real_
  if (!"w_combined" %in% names(plot_df)) plot_df$w_combined <- NA_real_
  plot_df <- plot_df |>
    dplyr::mutate(plot_weight = dplyr::coalesce(.data$w_adm, .data$w_combined, 0)) |>
    dplyr::filter(
      is.finite(.data$biomass_multiplier_if_replace),
      .data$biomass_multiplier_if_replace > 0,
      is.finite(.data$plot_weight),
      .data$plot_weight > 0
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = paste0("Biomass Sensitivity [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Biomass multiplier relative to the reference model", y = "Weighted model count") +
      ggplot2::theme_minimal(base_size = 11))
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$biomass_multiplier_if_replace, weight = .data$plot_weight)) +
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
#' @keywords internal
#' @noRd
plot_biomass_candidate_map <- function(candidate_tbl,
                                       anchor_label) {
  # Build the scatter map from the already scored candidate table so the plot
  # reflects the final admissible weight and biomass multiplier per model.
  plot_df <- tibble::as_tibble(candidate_tbl)
  if (!"biomass_multiplier_if_replace" %in% names(plot_df) ||
    !("w_adm" %in% names(plot_df) || "w_combined" %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "Model weight", y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  if (!"w_adm" %in% names(plot_df)) plot_df$w_adm <- NA_real_
  if (!"w_combined" %in% names(plot_df)) plot_df$w_combined <- NA_real_
  plot_df <- plot_df |>
    dplyr::mutate(plot_weight = dplyr::coalesce(.data$w_adm, .data$w_combined)) |>
    dplyr::filter(
      is.finite(.data$plot_weight),
      .data$plot_weight > 0,
      is.finite(.data$biomass_multiplier_if_replace),
      .data$biomass_multiplier_if_replace > 0
    )
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::labs(x = "Model weight", y = "Biomass multiplier") +
      ggplot2::theme_minimal(base_size = 11))
  }
  if ("common" %in% names(plot_df)) {
    plot_df$label <- dplyr::coalesce(
      as.character(plot_df$common),
      as.character(plot_df$species_name),
      if ("model_id" %in% names(plot_df)) as.character(plot_df$model_id) else as.character(seq_len(nrow(plot_df)))
    )
  } else {
    plot_df$label <- dplyr::coalesce(
      as.character(plot_df$species_name),
      if ("model_id" %in% names(plot_df)) as.character(plot_df$model_id) else as.character(seq_len(nrow(plot_df)))
    )
  }
  if ("swimbladder_type" %in% names(plot_df)) {
    plot_df$group_val <- dplyr::coalesce(as.character(plot_df$swimbladder_type), "unknown")
  } else {
    plot_df$group_val <- "unknown"
  }

  label_df <- plot_df |>
    dplyr::arrange(dplyr::desc(.data$plot_weight)) |>
    dplyr::slice_head(n = 10)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$plot_weight, y = .data$biomass_multiplier_if_replace)) +
    ggplot2::scale_y_log10(
      breaks = scales::breaks_log(n = 6),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::geom_hline(yintercept = 1, colour = "#4d4d4d", linetype = "dashed", linewidth = 0.7) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$group_val), alpha = 0.55, size = 2.2) +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(label = .data$label),
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
#' @keywords internal
#' @noRd
plot_top_ten_model_weights <- function(top_tbl,
                                       anchor_label) {
  # Build the ordered top-ten label set before drawing the ranking bar chart.
  plot_df <- tibble::as_tibble(top_tbl)
  if (nrow(plot_df) == 0 || !all(c("species_name", "model_id", "w_adm") %in% names(plot_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = paste0("Top-10 Models by Weight [", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = NULL, y = "Model weight") +
      ggplot2::theme_minimal(base_size = 11))
  }
  if ("common" %in% names(plot_df)) {
    common_suffix <- dplyr::if_else(!is.na(plot_df$common) & nzchar(plot_df$common), paste0(" [", plot_df$common, "]"), "")
  } else {
    common_suffix <- rep("", nrow(plot_df))
  }
  plot_df <- plot_df |>
    dplyr::mutate(
      label = paste0(.data$species_name, common_suffix, " {m", .data$model_id, "}"),
      label = factor(.data$label, levels = .data$label)
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$label, y = .data$w_adm)) +
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
#' @keywords internal
#' @noRd
plot_pivot_variance <- function(profile_tbl,
                                summary_tbl,
                                anchor_label) {
  # Resolve the pivot-display choice once so the line and optional pairwise
  # pivot band are plotted consistently.
  profile_df <- tibble::as_tibble(profile_tbl)
  summary_df <- tibble::as_tibble(summary_tbl)
  if (nrow(profile_df) == 0 || nrow(summary_df) == 0 || !all(c("length_cm", "weighted_variance_v") %in% names(profile_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = paste0("Pivot-Point Variance Profile [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Length (cm)", y = "Weighted variance V(L)") +
      ggplot2::theme_minimal(base_size = 11))
  }
  boundary_case <- identical(summary_df$pivot_display_source[[1]], "pairwise_weighted_median")
  plot_pivot <- if (boundary_case) summary_df$pivot_length_cm[[1]] else summary_df$pivot_display_length_cm[[1]]

  ggplot2::ggplot(profile_df, ggplot2::aes(x = .data$length_cm, y = .data$weighted_variance_v)) +
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
#' @keywords internal
#' @noRd
plot_pairwise_pivot_histogram <- function(pairwise_tbl,
                                          summary_tbl,
                                          anchor_label) {
  # Use the display pivot from the summary table so the histogram aligns with
  # the pivot-variance plot.
  pairwise_df <- tibble::as_tibble(pairwise_tbl)
  summary_df <- tibble::as_tibble(summary_tbl)
  if (nrow(pairwise_df) == 0 || nrow(summary_df) == 0 || !all(c("lpivot_cm", "pair_weight") %in% names(pairwise_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = paste0("Pairwise Pivot-Length Distribution [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Pairwise pivot length (cm)", y = "Weighted pair count") +
      ggplot2::theme_minimal(base_size = 11))
  }
  plot_pivot <- summary_df$pivot_display_length_cm[[1]]
  boundary_case <- identical(summary_df$pivot_display_source[[1]], "pairwise_weighted_median")

  ggplot2::ggplot(pairwise_df, ggplot2::aes(x = .data$lpivot_cm, weight = .data$pair_weight)) +
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
#' @keywords internal
#' @noRd
plot_biological_leverage <- function(profile_tbl,
                                     summary_tbl,
                                     anchor_label) {
  # Overlay the leverage peak and pivot diagnostics on the leverage profile so
  # the biologically influential size classes can be read directly from the
  # final figure.
  profile_df <- tibble::as_tibble(profile_tbl)
  summary_df <- tibble::as_tibble(summary_tbl)
  if (nrow(profile_df) == 0 || nrow(summary_df) == 0 || !all(c("length_cm", "lambda_l") %in% names(profile_df))) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = paste0("Biological Leverage Profile [reference: ", anchor_label, "]"), subtitle = "Required plotting fields were not available.", x = "Length (cm)", y = "Biological leverage") +
      ggplot2::theme_minimal(base_size = 11))
  }
  boundary_case <- identical(summary_df$pivot_display_source[[1]], "pairwise_weighted_median")
  plot_pivot <- if (boundary_case) summary_df$pivot_length_cm[[1]] else summary_df$pivot_display_length_cm[[1]]

  ggplot2::ggplot(profile_df, ggplot2::aes(x = .data$length_cm, y = .data$lambda_l)) +
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
