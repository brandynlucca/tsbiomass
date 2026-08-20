#' Resolve canonical policy names from policy data
#'
#' @param policy_data Data frame or tibble with a `policy` column.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
resolve_policy_names <- function(policy_data) {
  # Normalize policy identifiers to canonical names when present.
  if ("policy" %in% names(policy_data)) {
    return(canonical_policy_name(as.character(policy_data$policy)))
  }
  rep(NA_character_, nrow(policy_data))
}

#' Convert snake-case policy text to title case
#'
#' @param x Character vector.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
snake_title <- function(x) {
  stringr::str_to_title(
    stringr::str_replace_all(as.character(x), "_", " ")
  )
}

#' Convert snake-case policy text to lower dashed text
#'
#' @param x Character vector.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
snake_lower_dash <- function(x) {
  stringr::str_to_lower(
    stringr::str_replace_all(as.character(x), "_", "-")
  )
}

#' Detect unusable display labels
#'
#' @param x Character vector of display labels.
#'
#' @return Logical vector indicating labels that should be rebuilt.
#' @keywords internal
#' @noRd
policy_display_value_missing <- function(x) {
  x <- as.character(x)
  clean <- stringr::str_squish(x)
  clean_no_branch <- stringr::str_squish(stringr::str_remove(clean, "\\s*\\[[^]]*\\]$"))
  is.na(clean) |
    !nzchar(clean) |
    stringr::str_to_lower(clean_no_branch) %in% c("na", "nan", "null", "none", "na na", "unresolved policy")
}

#' Build a policy-branch display-tag map
#'
#' @param branch_definitions Policy branch definitions.
#'
#' @return Named character vector.
#' @keywords internal
#' @noRd
policy_branch_tag_map <- function(branch_definitions) {
  stats::setNames(
    vapply(
      branch_definitions,
      function(value) {
        as.character(
          value$display_tag %||%
            value$key %||%
            NA_character_
        )
      },
      character(1)
    ),
    vapply(
      branch_definitions,
      function(value) as.character(value$key %||% NA_character_),
      character(1)
    )
  )
}

#' Resolve policy-branch display labels
#'
#' @param branch_values Character branch keys.
#' @param branch_tags Named branch-tag map.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
policy_branch_labels <- function(branch_values,
                                 branch_tags) {
  labels <- unname(branch_tags[branch_values])
  missing_labels <- policy_display_value_missing(labels)
  labels[missing_labels] <- branch_values[missing_labels]
  labels[policy_display_value_missing(labels)] <- "All slopes"
  labels
}

#' Resolve policy branch filters from policy data
#'
#' @param policy_data Data frame or tibble.
#' @param branch_column Branch-filter column name.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
resolve_policy_branch_filters <- function(policy_data,
                                          branch_column = "equation_branch_filter") {
  # Normalize policy branch filters and default missing values to `all`.
  if (branch_column %in% names(policy_data)) {
    branch_values <- as.character(policy_data[[branch_column]])
    branch_values[is.na(branch_values) | !nzchar(branch_values)] <- "all"
    return(normalize_policy_equation_branch_filters(branch_values))
  }
  rep("all", nrow(policy_data))
}

#' Normalize policy identity columns
#'
#' @param policy_data Data frame or tibble.
#' @param policy_column Policy column name.
#' @param branch_column Branch-filter column name.
#'
#' @return Tibble with normalized policy-identity columns.
#'
#' @keywords internal
#' @noRd
normalize_policy_columns <- function(policy_data,
                                     policy_column = "policy",
                                     branch_column = "equation_branch_filter") {
  # Standardize policy identifiers and branch filters together.
  policy_data <- if (is.data.frame(policy_data)) {
    policy_data
  } else {
    tibble::as_tibble(policy_data)
  }
  if (policy_column %in% names(policy_data)) {
    policy_data[[policy_column]] <- as.character(policy_data[[policy_column]])
  }
  policy_data[[branch_column]] <- resolve_policy_branch_filters(
    policy_data,
    branch_column = branch_column
  )
  policy_data
}

#' Resolve selected-policy branch filters from policy data
#'
#' @param policy_data Data frame or tibble.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
resolve_selected_policy_branches <- function(policy_data) {
  # Prefer selected-policy branches, then fall back to general branch filters.
  policy_data <- tibble::as_tibble(policy_data)

  if ("selected_equation_branch_filter" %in% names(policy_data)) {
    selected_values <- as.character(policy_data$selected_equation_branch_filter)
    if ("equation_branch_filter" %in% names(policy_data)) {
      branch_values <- as.character(policy_data$equation_branch_filter)
      selected_values[is.na(selected_values) | !nzchar(selected_values)] <-
        branch_values[is.na(selected_values) | !nzchar(selected_values)]
    }
    selected_values[is.na(selected_values) | !nzchar(selected_values)] <- "all"
    return(normalize_policy_equation_branch_filters(selected_values))
  }
  if ("equation_branch_filter" %in% names(policy_data)) {
    return(resolve_policy_branch_filters(
      policy_data,
      branch_column = "equation_branch_filter"
    ))
  }

  rep("all", nrow(policy_data))
}

#' Resolve selected-policy values from policy data
#'
#' @param policy_data Data frame or tibble.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
resolve_selected_policy_values <- function(policy_data) {
  # Extract selected-policy identifiers when present.
  policy_data <- tibble::as_tibble(policy_data)

  if ("selected_policy" %in% names(policy_data)) {
    return(as.character(policy_data$selected_policy))
  }

  rep(NA_character_, nrow(policy_data))
}

#' Resolve displayed policy names from policy data
#'
#' @param policy_data Data frame or tibble.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
resolve_policy_display_names <- function(policy_data) {
  # Build human-readable policy labels from explicit displays or policy fields.
  policy_data <- tibble::as_tibble(policy_data)
  branch_values <- resolve_policy_branch_filters(policy_data)
  fallback_values <- rep(NA_character_, nrow(policy_data))

  if (all(c("candidate_pool", "aggregation_method") %in% names(policy_data))) {
    branch_definitions <- read_policy_registry()$policy_branches %||% list()
    branch_tags <- policy_branch_tag_map(branch_definitions)
    candidate_pool_values <- as.character(policy_data$candidate_pool)
    aggregation_method_values <- as.character(policy_data$aggregation_method)
    pool_values <- dplyr::recode(
      candidate_pool_values,
      all_admissible = "All-models",
      closest_study_cell = "Study-cell",
      generalized_models_only = "Generalized",
      nearest_phylogenetic = "Phylogenetic",
      phylogenetic_neighborhood = "Related-species",
      same_derivation = "Derivation",
      same_equation_form = "Equation-form",
      same_family = "Family",
      same_fao_area = "FAO-area",
      same_genus = "Genus",
      same_ocean_basin = "Ocean-basin",
      same_order = "Order",
      same_species = "Species",
      .default = snake_title(candidate_pool_values)
    )
    aggregation_values <- dplyr::recode(
      aggregation_method_values,
      arithmetic_mean = "averaged",
      equal_weight_mean = "averaged",
      nearest = "nearest",
      nearest_by_combined_distance = "nearest",
      nearest_by_trait_gower_distance = "trait-nearest",
      nearest_study_then_model = "study-nearest",
      .default = snake_lower_dash(aggregation_method_values)
    )
    branch_labels <- policy_branch_labels(branch_values, branch_tags)
    alias_values <- paste0(
      pool_values,
      " ",
      aggregation_values,
      " [",
      branch_labels,
      "]"
    )
    missing_components <- policy_display_value_missing(candidate_pool_values) |
      policy_display_value_missing(aggregation_method_values)
    alias_values[missing_components] <- NA_character_
    fallback_values <- alias_values
  }

  if ("policy" %in% names(policy_data)) {
    policy_fallback <- policy_display_label(
      canonical_policy_name(as.character(policy_data$policy)),
      branch_values
    )
    missing_fallback <- policy_display_value_missing(fallback_values)
    fallback_values[missing_fallback] <- policy_fallback[missing_fallback]
  }

  if ("policy_display" %in% names(policy_data)) {
    display_values <- as.character(policy_data$policy_display)
    missing_display <- policy_display_value_missing(display_values)
    display_values[missing_display] <- fallback_values[missing_display]
    missing_display <- policy_display_value_missing(display_values)
    display_values[missing_display] <- paste0("Unresolved policy [", policy_branch_labels(branch_values, policy_branch_tag_map(read_policy_registry()$policy_branches %||% list()))[missing_display], "]")
    return(display_values)
  }

  missing_fallback <- policy_display_value_missing(fallback_values)
  fallback_values[missing_fallback] <- paste0("Unresolved policy [", policy_branch_labels(branch_values, policy_branch_tag_map(read_policy_registry()$policy_branches %||% list()))[missing_fallback], "]")
  fallback_values
}

#' Resolve displayed selected policy names from policy data
#'
#' @param policy_data Data frame or tibble.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
resolve_selected_policy_names <- function(policy_data) {
  # Build selected-policy display labels, preserving explicit labels when supplied.
  policy_data <- tibble::as_tibble(policy_data)
  branch_values <- if ("selected_equation_branch_filter" %in% names(policy_data)) {
    resolve_policy_branch_filters(
      policy_data,
      branch_column = "selected_equation_branch_filter"
    )
  } else {
    resolve_policy_branch_filters(policy_data)
  }
  branch_definitions <- read_policy_registry()$policy_branches %||% list()
  branch_tags <- policy_branch_tag_map(branch_definitions)
  branch_labels <- policy_branch_labels(branch_values, branch_tags)
  fallback_values <- rep(NA_character_, nrow(policy_data))
  if (all(c("candidate_pool", "aggregation_method") %in% names(policy_data))) {
    candidate_pool_values <- as.character(policy_data$candidate_pool)
    aggregation_method_values <- as.character(policy_data$aggregation_method)
    pool_values <- dplyr::recode(
      candidate_pool_values,
      all_admissible = "All-models",
      closest_study_cell = "Study-cell",
      generalized_models_only = "Generalized",
      nearest_phylogenetic = "Phylogenetic",
      phylogenetic_neighborhood = "Related-species",
      same_derivation = "Derivation",
      same_equation_form = "Equation-form",
      same_family = "Family",
      same_fao_area = "FAO-area",
      same_genus = "Genus",
      same_ocean_basin = "Ocean-basin",
      same_order = "Order",
      same_species = "Species",
      .default = snake_title(candidate_pool_values)
    )
    aggregation_values <- dplyr::recode(
      aggregation_method_values,
      arithmetic_mean = "averaged",
      equal_weight_mean = "averaged",
      nearest = "nearest",
      nearest_by_combined_distance = "nearest",
      nearest_by_trait_gower_distance = "trait-nearest",
      nearest_study_then_model = "study-nearest",
      .default = snake_lower_dash(aggregation_method_values)
    )
    branch_labels <- policy_branch_labels(branch_values, branch_tags)
    alias_values <- paste0(
      pool_values,
      " ",
      aggregation_values,
      " [",
      branch_labels,
      "]"
    )
    missing_components <- policy_display_value_missing(candidate_pool_values) |
      policy_display_value_missing(aggregation_method_values)
    alias_values[missing_components] <- NA_character_
    fallback_values <- alias_values
  }

  if ("selected_policy" %in% names(policy_data)) {
    policy_fallback <- policy_display_label(
      canonical_policy_name(as.character(policy_data$selected_policy)),
      branch_values
    )
    missing_fallback <- policy_display_value_missing(fallback_values)
    fallback_values[missing_fallback] <- policy_fallback[missing_fallback]
  }

  if ("selected_policy_display" %in% names(policy_data)) {
    display_values <- as.character(policy_data$selected_policy_display)
    missing_display <- policy_display_value_missing(display_values)
    display_values[missing_display] <- fallback_values[missing_display]
    needs_branch <- !policy_display_value_missing(display_values) &
      !grepl("\\s*\\[[^]]+\\]$", display_values)
    if (any(needs_branch, na.rm = TRUE)) {
      display_values[needs_branch] <- paste0(display_values[needs_branch], " [", branch_labels[needs_branch], "]")
    }
    missing_display <- policy_display_value_missing(display_values)
    display_values[missing_display] <- paste0("Unresolved policy [", branch_labels[missing_display], "]")
    return(display_values)
  }

  missing_fallback <- policy_display_value_missing(fallback_values)
  fallback_values[missing_fallback] <- paste0("Unresolved policy [", branch_labels[missing_fallback], "]")
  fallback_values
}

#' Build policy component labels for plotting
#'
#' @param policy_data Data frame or tibble containing policy definition columns.
#' @param policy_column Policy identifier column.
#' @param branch_column Equation-branch filter column.
#'
#' @return Tibble with one row per source row and policy component.
#' @keywords internal
#' @noRd
policy_component_labels <- function(policy_data,
                                    policy_column = "policy",
                                    branch_column = "equation_branch_filter") {
  # Resolve strategy components from explicit columns first, then from the
  # policy registry when a canonical policy identifier is available.
  policy_data <- tibble::as_tibble(policy_data)
  n_rows <- nrow(policy_data)
  if (n_rows == 0L) {
    return(tibble::tibble(.row_id = integer(), component = character(), component_level = character()))
  }

  policy_values <- if (policy_column %in% names(policy_data)) {
    canonical_policy_name(as.character(policy_data[[policy_column]]))
  } else {
    rep(NA_character_, n_rows)
  }
  lookup_tbl <- policy_lookup_table()
  registry_value <- function(field) {
    vapply(policy_values, function(policy_now) {
      if (policy_display_value_missing(policy_now)) {
        return(NA_character_)
      }
      policy_def <- lookup_tbl[[policy_now]] %||%
        build_policy_definition_from_name(policy_now) %||%
        NULL
      if (!is.list(policy_def)) {
        return(NA_character_)
      }
      as.character(policy_def[[field]] %||% NA_character_)[[1]]
    }, character(1))
  }

  candidate_pool_values <- if ("candidate_pool" %in% names(policy_data)) {
    as.character(policy_data$candidate_pool)
  } else {
    rep(NA_character_, n_rows)
  }
  missing_pool <- policy_display_value_missing(candidate_pool_values)
  candidate_pool_values[missing_pool] <- registry_value("candidate_pool")[missing_pool]

  aggregation_values <- if ("aggregation_method" %in% names(policy_data)) {
    as.character(policy_data$aggregation_method)
  } else {
    rep(NA_character_, n_rows)
  }
  missing_aggregation <- policy_display_value_missing(aggregation_values)
  aggregation_values[missing_aggregation] <- registry_value("aggregation_method")[missing_aggregation]

  branch_values <- resolve_policy_branch_filters(policy_data, branch_column = branch_column)
  branch_tags <- policy_branch_tag_map(read_policy_registry()$policy_branches %||% list())
  branch_values <- policy_branch_labels(branch_values, branch_tags)

  pool_labels <- dplyr::recode(
    candidate_pool_values,
    all_admissible = "All admissible",
    closest_study_cell = "Closest study cell",
    generalized_models_only = "Generalized models",
    nearest_phylogenetic = "Nearest taxon",
    phylogenetic_neighborhood = "Taxon neighborhood",
    same_family = "Same family",
    same_fao_area = "Same FAO area",
    same_genus = "Same genus",
    same_ocean_basin = "Same ocean basin",
    same_species = "Same species",
    .default = snake_title(candidate_pool_values)
  )
  aggregation_labels <- dplyr::recode(
    aggregation_values,
    arithmetic_mean = "Mean",
    equal_weight_mean = "Mean",
    nearest = "Nearest",
    nearest_by_combined_distance = "Nearest",
    nearest_by_trait_gower_distance = "Trait nearest",
    nearest_study_then_model = "Study nearest",
    weighted_mean = "Weighted mean",
    .default = snake_title(aggregation_values)
  )

  row_ids <- if (".row_id" %in% names(policy_data)) {
    policy_data$.row_id
  } else {
    seq_len(n_rows)
  }

  out <- dplyr::bind_rows(
    tibble::tibble(.row_id = row_ids, component = "Candidate pool", component_level = pool_labels),
    tibble::tibble(.row_id = row_ids, component = "Aggregation", component_level = aggregation_labels),
    tibble::tibble(.row_id = row_ids, component = "Slope class", component_level = branch_values)
  ) |>
    dplyr::filter(!policy_display_value_missing(.data$component_level))
  out$component <- factor(out$component, levels = c("Candidate pool", "Aggregation", "Slope class"))
  out
}

#' Resolve equivalent policy-set labels from policy data
#'
#' @param policy_data Data frame or tibble.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
resolve_equivalent_policy_sets <- function(policy_data) {
  # Reuse explicit equivalent-set labels when available.
  policy_data <- tibble::as_tibble(policy_data)

  if ("equivalent_policy_set" %in% names(policy_data)) {
    return(as.character(policy_data$equivalent_policy_set))
  }

  rep(NA_character_, nrow(policy_data))
}

#' Expand a constructor-based policy registry
#'
#' @param registry Parsed policy-registry list.
#'
#' @return Registry list with a materialized `policies` entry.
#' @keywords internal
#' @noRd
expand_policy_registry <- function(registry) {
  if (!is.list(registry) || !is.list(registry$policy_constructors)) {
    return(registry)
  }

  trait_registry <- read_trait_registry()
  trait_defs <- c(trait_registry$species_traits %||% list(), trait_registry$study_traits %||% list())
  trait_keys <- unique(vapply(trait_defs, function(x) as.character(x$coded_name %||% NA_character_), character(1)))
  trait_keys <- trait_keys[!is.na(trait_keys) & nzchar(trait_keys)]
  trait_lookup <- stats::setNames(
    trait_defs,
    vapply(trait_defs, function(x) as.character(x$coded_name %||% NA_character_), character(1))
  )

  # Public policy-group names should stay human-facing even when the underlying
  # trait columns use more verbose internal identifiers.
  public_group_aliases <- c(
    fao_area = "fao",
    derivation_type = "derivation",
    swimbladder_type = "swimbladder"
  )

  # Reuse optimized pool labels where they already exist, but otherwise fall
  # back to the generic overlap-based matcher so every nonnumeric trait in the
  # registry can participate in policy construction.
  direct_trait_pools <- c(
    species = "same_species",
    genus = "same_genus",
    family = "same_family",
    order = "same_order",
    swimbladder_type = "same_swimbladder",
    fao_area = "same_fao_area",
    ocean_basin = "same_ocean_basin",
    equation_form = "same_equation_form",
    derivation_type = "same_derivation"
  )
  groupable_trait_keys <- Filter(
    function(trait_key) {
      trait_def <- trait_lookup[[trait_key]] %||% NULL
      is.list(trait_def) && !identical(trait_def$data_type %||% "numeric", "numeric")
    },
    trait_keys
  )
  display_core <- function(trait_key) {
    trait_def <- trait_lookup[[trait_key]] %||% list()
    display_name <- stringr::str_squish(as.character(trait_def$display_name %||% trait_key)[[1]])
    display_name <- stringr::str_replace(display_name, "^Taxonomic\\s+", "")
    display_name <- stringr::str_replace(display_name, "^Study\\s+", "")
    display_name <- stringr::str_replace(display_name, "^FAO major fishing area$", "FAO area")
    display_name
  }
  dynamic_trait_groups <- lapply(groupable_trait_keys, function(trait_key) {
    trait_def <- trait_lookup[[trait_key]] %||% NULL
    if (!is.list(trait_def)) {
      return(NULL)
    }

    public_key <- if (trait_key %in% names(public_group_aliases)) {
      unname(public_group_aliases[trait_key])
    } else {
      trait_key
    }
    display_name <- display_core(trait_key)
    display_phrase <- paste("within", stringr::str_to_lower(display_name))
    display_phrase_title <- if (identical(display_phrase, "within fao area")) {
      "Within FAO area"
    } else {
      stringr::str_to_title(display_phrase)
    }

    list(
      key = public_key,
      coded_suffix = paste0("within_", public_key),
      candidate_pool = if (trait_key %in% names(direct_trait_pools)) {
        unname(direct_trait_pools[trait_key])
      } else {
        "match_all_traits"
      },
      display_phrase = display_phrase,
      display_phrase_title = display_phrase_title,
      fixed_parameters = list(match_traits = trait_key),
      candidate_pool_definition = sprintf(
        "Eligible donors are admissible models that match the target anchor on %s.",
        stringr::str_to_lower(display_name)
      )
    )
  })
  dynamic_trait_groups <- Filter(Negate(is.null), dynamic_trait_groups)

  # Compound groups are resolved dynamically from the requested policy group key
  # instead of being fully pre-expanded here. That keeps the registry surface
  # small and lets the semantic conjunction rules live in one parser.
  group_defs <- c(dynamic_trait_groups, registry$policy_groups %||% list())
  metric_defs <- registry$policy_metrics %||% list()
  if (!is.list(group_defs) || !is.list(metric_defs)) {
    stop(
      "A constructor-based policy registry must define top-level 'policy_groups' and 'policy_metrics' lists.",
      call. = FALSE
    )
  }

  group_lookup <- stats::setNames(
    group_defs,
    vapply(group_defs, function(x) as.character(x$key %||% NA_character_), character(1))
  )
  metric_lookup <- stats::setNames(
    metric_defs,
    vapply(metric_defs, function(x) as.character(x$key %||% NA_character_), character(1))
  )

  title_phrase <- function(group_def) {
    phrase <- as.character(group_def$display_phrase_title %||% group_def$display_phrase %||% NA_character_)[[1]]
    if (is.na(phrase) || !nzchar(phrase)) {
      phrase <- snake_title(group_def$key %||% "policy")
    }
    phrase
  }
  lower_phrase <- function(group_def) {
    phrase <- as.character(group_def$display_phrase %||% group_def$display_phrase_title %||% NA_character_)[[1]]
    if (is.na(phrase) || !nzchar(phrase)) {
      phrase <- stringr::str_to_lower(
        snake_title(group_def$key %||% "policy")
      )
    }
    phrase
  }
  render_template <- function(template,
                              values) {
    out <- as.character(template %||% "")
    for (nm in names(values)) {
      out <- gsub(
        pattern = paste0("\\{", nm, "\\}"),
        replacement = as.character(values[[nm]] %||% ""),
        x = out
      )
    }
    stringr::str_squish(out)
  }
  build_constructed_policy <- function(group_def,
                                       metric_def,
                                       constructor_def) {
    coded_name <- paste0(
      as.character(metric_def$coded_prefix %||% NA_character_)[[1]],
      as.character(group_def$coded_suffix %||% NA_character_)[[1]]
    )
    if (is.na(coded_name) || !nzchar(coded_name)) {
      stop("Constructed policies require non-empty metric coded_prefix and group coded_suffix values.", call. = FALSE)
    }

    display_name <- render_template(
      metric_def$display_template %||% "{group_phrase_title}",
      list(
        group_phrase = lower_phrase(group_def),
        group_phrase_title = title_phrase(group_def)
      )
    )
    candidate_pool <- as.character(
      constructor_def$candidate_pool %||%
        metric_def$candidate_pool %||%
        group_def$candidate_pool %||%
        NA_character_
    )[[1]]
    candidate_pool_definition <- stringr::str_squish(as.character(
      constructor_def$candidate_pool_definition %||%
        metric_def$candidate_pool_definition %||%
        group_def$candidate_pool_definition %||%
        NA_character_
    )[[1]])
    aggregation_definition <- stringr::str_squish(as.character(metric_def$aggregation_definition %||% NA_character_)[[1]])
    plain_language_definition <- render_template(
      metric_def$plain_language_template %||%
        "This policy targets {display_name}. {candidate_pool_definition} {aggregation_definition}",
      list(
        display_name = stringr::str_to_lower(display_name),
        candidate_pool_definition = candidate_pool_definition,
        aggregation_definition = aggregation_definition
      )
    )

    list(
      coded_name = coded_name,
      display_name = display_name,
      description = render_template(
        metric_def$description_template %||% "{display_name}.",
        list(display_name = display_name)
      ),
      policy_family = as.character(metric_def$policy_family %||% NA_character_)[[1]],
      candidate_pool = candidate_pool,
      aggregation_method = as.character(metric_def$aggregation_method %||% NA_character_)[[1]],
      fixed_parameters = metric_def$fixed_parameters %||% group_def$fixed_parameters %||% constructor_def$fixed_parameters %||% NULL,
      tunable_parameters = unique(c(
        as.character(unlist(metric_def$tunable_parameters %||% character(0), use.names = FALSE)),
        as.character(unlist(group_def$tunable_parameters %||% character(0), use.names = FALSE)),
        as.character(unlist(constructor_def$tunable_parameters %||% character(0), use.names = FALSE))
      )),
      grouping_key = as.character(group_def$key %||% NA_character_)[[1]],
      metric_key = as.character(metric_def$key %||% NA_character_)[[1]],
      candidate_pool_definition = candidate_pool_definition,
      aggregation_definition = aggregation_definition,
      plain_language_definition = plain_language_definition
    )
  }

  expanded <- list()
  for (constructor_def in registry$policy_constructors) {
    group_keys <- as.character(unlist(
      constructor_def$groups %||% constructor_def$groupings %||% character(0),
      use.names = FALSE
    ))
    if (any(group_keys %in% "__all_groups__")) {
      group_keys <- c(
        setdiff(group_keys, "__all_groups__"),
        names(group_lookup)
      )
    }
    exclude_group_keys <- as.character(unlist(constructor_def$exclude_groups %||% character(0), use.names = FALSE))
    if (length(exclude_group_keys) > 0) {
      group_keys <- setdiff(group_keys, exclude_group_keys)
    }
    group_keys <- unique(group_keys)
    metric_keys <- as.character(unlist(
      constructor_def$metrics %||% constructor_def$methods %||% character(0),
      use.names = FALSE
    ))
    if (length(group_keys) == 0 || length(metric_keys) == 0) {
      next
    }

    for (group_key in group_keys) {
      group_def <- group_lookup[[group_key]] %||% NULL
      if (!is.list(group_def)) {
        stop(sprintf("Unknown policy constructor group key: %s", group_key), call. = FALSE)
      }
      for (metric_key in metric_keys) {
        match_traits_now <- as.character(unlist((group_def$fixed_parameters %||% list())$match_traits %||% character(0), use.names = FALSE))
        if (identical(metric_key, "species_distance") &&
          (identical(group_key, "species") || "species" %in% match_traits_now)) {
          next
        }
        metric_def <- metric_lookup[[metric_key]] %||% NULL
        if (!is.list(metric_def)) {
          stop(sprintf("Unknown policy constructor metric key: %s", metric_key), call. = FALSE)
        }
        expanded[[length(expanded) + 1L]] <- build_constructed_policy(
          group_def = group_def,
          metric_def = metric_def,
          constructor_def = constructor_def
        )
      }
    }
  }

  registry$policies <- expanded
  registry
}

#' Build one constructed policy definition
#'
#' @param group_def One normalized policy-group definition.
#' @param metric_def One normalized policy-metric definition.
#' @param constructor_def Optional constructor overrides.
#'
#' @return One policy-definition list.
#' @keywords internal
#' @noRd
build_constructed_policy_definition <- function(group_def,
                                                metric_def,
                                                constructor_def = list()) {
  title_phrase <- function(group_def) {
    phrase <- as.character(group_def$display_phrase_title %||% group_def$display_phrase %||% NA_character_)[[1]]
    if (is.na(phrase) || !nzchar(phrase)) {
      phrase <- snake_title(group_def$key %||% "policy")
    }
    phrase
  }
  lower_phrase <- function(group_def) {
    phrase <- as.character(group_def$display_phrase %||% group_def$display_phrase_title %||% NA_character_)[[1]]
    if (is.na(phrase) || !nzchar(phrase)) {
      phrase <- stringr::str_to_lower(
        snake_title(group_def$key %||% "policy")
      )
    }
    phrase
  }
  render_template <- function(template,
                              values) {
    out <- as.character(template %||% "")
    for (nm in names(values)) {
      out <- gsub(
        pattern = paste0("\\{", nm, "\\}"),
        replacement = as.character(values[[nm]] %||% ""),
        x = out
      )
    }
    stringr::str_squish(out)
  }

  coded_name <- paste0(
    as.character(metric_def$coded_prefix %||% NA_character_)[[1]],
    as.character(group_def$coded_suffix %||% NA_character_)[[1]]
  )
  if (is.na(coded_name) || !nzchar(coded_name)) {
    stop("Constructed policies require non-empty metric coded_prefix and group coded_suffix values.", call. = FALSE)
  }

  display_name <- render_template(
    metric_def$display_template %||% "{group_phrase_title}",
    list(
      group_phrase = lower_phrase(group_def),
      group_phrase_title = title_phrase(group_def)
    )
  )
  candidate_pool <- as.character(
    constructor_def$candidate_pool %||%
      metric_def$candidate_pool %||%
      group_def$candidate_pool %||%
      NA_character_
  )[[1]]
  candidate_pool_definition <- stringr::str_squish(as.character(
    constructor_def$candidate_pool_definition %||%
      metric_def$candidate_pool_definition %||%
      group_def$candidate_pool_definition %||%
      NA_character_
  )[[1]])
  aggregation_definition <- stringr::str_squish(as.character(metric_def$aggregation_definition %||% NA_character_)[[1]])
  plain_language_definition <- render_template(
    metric_def$plain_language_template %||%
      "This policy targets {display_name}. {candidate_pool_definition} {aggregation_definition}",
    list(
      display_name = stringr::str_to_lower(display_name),
      candidate_pool_definition = candidate_pool_definition,
      aggregation_definition = aggregation_definition
    )
  )

  list(
    coded_name = coded_name,
    display_name = display_name,
    description = render_template(
      metric_def$description_template %||% "{display_name}.",
      list(display_name = display_name)
    ),
    policy_family = as.character(metric_def$policy_family %||% NA_character_)[[1]],
    candidate_pool = candidate_pool,
    aggregation_method = as.character(metric_def$aggregation_method %||% NA_character_)[[1]],
    fixed_parameters = metric_def$fixed_parameters %||% group_def$fixed_parameters %||% constructor_def$fixed_parameters %||% NULL,
    tunable_parameters = unique(c(
      as.character(unlist(metric_def$tunable_parameters %||% character(0), use.names = FALSE)),
      as.character(unlist(group_def$tunable_parameters %||% character(0), use.names = FALSE)),
      as.character(unlist(constructor_def$tunable_parameters %||% character(0), use.names = FALSE))
    )),
    grouping_key = as.character(group_def$key %||% NA_character_)[[1]],
    metric_key = as.character(metric_def$key %||% NA_character_)[[1]],
    candidate_pool_definition = candidate_pool_definition,
    aggregation_definition = aggregation_definition,
    plain_language_definition = plain_language_definition
  )
}

#' Resolve one dynamic policy-group definition
#'
#' @param group_key Canonical policy group key.
#' @param registry Parsed policy registry.
#'
#' @return One policy-group definition list.
#' @keywords internal
#' @noRd
build_policy_group_definition <- function(group_key,
                                          registry,
                                          active_species_traits = NULL,
                                          active_study_traits = NULL) {
  group_key <- stringr::str_squish(as.character(group_key %||% ""))[[1]]
  if (!nzchar(group_key)) {
    stop("'group_key' must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.list(registry)) {
    stop("'registry' must be a parsed policy registry list.", call. = FALSE)
  }

  trait_registry <- read_trait_registry()
  trait_defs <- c(trait_registry$species_traits %||% list(), trait_registry$study_traits %||% list())
  trait_lookup <- stats::setNames(
    trait_defs,
    vapply(trait_defs, function(x) as.character(x$coded_name %||% NA_character_), character(1))
  )
  groupable_trait_keys <- Filter(
    function(trait_key) {
      trait_def <- trait_lookup[[trait_key]] %||% NULL
      is.list(trait_def) && !identical(trait_def$data_type %||% "numeric", "numeric")
    },
    names(trait_lookup)
  )
  public_group_aliases <- c(
    fao_area = "fao",
    derivation_type = "derivation",
    swimbladder_type = "swimbladder"
  )
  public_trait_keys <- vapply(groupable_trait_keys, function(trait_key) {
    if (trait_key %in% names(public_group_aliases)) {
      unname(public_group_aliases[trait_key])
    } else {
      trait_key
    }
  }, character(1))
  names(public_trait_keys) <- groupable_trait_keys
  public_to_trait <- stats::setNames(names(public_trait_keys), public_trait_keys)
  resolve_public_trait <- function(token) {
    hit_idx <- match(token, names(public_to_trait))
    if (is.na(hit_idx)) {
      return(NA_character_)
    }
    as.character(unname(public_to_trait[[hit_idx]]))[[1]]
  }
  direct_trait_pools <- c(
    species = "same_species",
    genus = "same_genus",
    family = "same_family",
    order = "same_order",
    swimbladder_type = "same_swimbladder",
    fao_area = "same_fao_area",
    ocean_basin = "same_ocean_basin",
    equation_form = "same_equation_form",
    derivation_type = "same_derivation"
  )
  special_group_lookup <- stats::setNames(
    registry$policy_groups %||% list(),
    vapply(registry$policy_groups %||% list(), function(x) as.character(x$key %||% NA_character_), character(1))
  )

  display_core <- function(trait_key) {
    trait_def <- trait_lookup[[trait_key]] %||% list()
    display_name <- stringr::str_squish(as.character(trait_def$display_name %||% trait_key)[[1]])
    display_name <- stringr::str_replace(display_name, "^Taxonomic\\s+", "")
    display_name <- stringr::str_replace(display_name, "^Study\\s+", "")
    display_name <- stringr::str_replace(display_name, "^FAO major fishing area$", "FAO area")
    display_name
  }
  render_title <- function(x) {
    stringr::str_replace(stringr::str_to_title(x), "\\bFao\\b", "FAO")
  }
  join_phrase <- function(base_phrase,
                          trait_keys) {
    out <- base_phrase
    if (length(trait_keys) == 0) {
      return(out)
    }
    for (trait_key in trait_keys) {
      connector <- if (trait_key %in% c("fao_area", "ocean_basin")) " in same " else " with same "
      out <- paste0(out, connector, stringr::str_to_lower(display_core(trait_key)))
    }
    out
  }
  join_trait_text <- function(trait_keys) {
    labels <- vapply(trait_keys, function(trait_key) stringr::str_to_lower(display_core(trait_key)), character(1))
    if (length(labels) == 1) {
      return(labels[[1]])
    }
    if (length(labels) == 2) {
      return(paste(labels, collapse = " and "))
    }
    paste0(paste(labels[-length(labels)], collapse = ", "), ", and ", labels[[length(labels)]])
  }

  known_tokens <- unique(c(names(special_group_lookup), unname(public_trait_keys)))
  token_parts <- lapply(known_tokens, function(x) strsplit(x, "_", fixed = TRUE)[[1]])
  token_order <- order(vapply(token_parts, length, integer(1)), decreasing = TRUE)
  group_parts <- strsplit(group_key, "_", fixed = TRUE)[[1]]
  parse_tokens <- function(parts) {
    if (length(parts) == 0) {
      return(character(0))
    }
    for (idx in token_order) {
      token_now <- known_tokens[[idx]]
      token_bits <- token_parts[[idx]]
      n_bits <- length(token_bits)
      if (n_bits > length(parts)) {
        next
      }
      if (!identical(parts[seq_len(n_bits)], token_bits)) {
        next
      }
      remainder <- parse_tokens(parts[-seq_len(n_bits)])
      if (!is.null(remainder)) {
        return(c(token_now, remainder))
      }
    }
    NULL
  }
  tokens <- parse_tokens(group_parts)
  if (is.null(tokens) || length(tokens) == 0) {
    stop(sprintf("Unknown policy constructor group key: %s", group_key), call. = FALSE)
  }

  special_keys <- names(special_group_lookup)
  taxonomic_traits <- c("class", "order", "family", "genus", "species")
  species_trait_keys <- vapply(
    trait_registry$species_traits %||% list(),
    function(x) as.character(x$coded_name %||% NA_character_),
    character(1)
  )
  species_trait_keys <- species_trait_keys[!is.na(species_trait_keys) & nzchar(species_trait_keys)]
  study_trait_keys <- vapply(
    trait_registry$study_traits %||% list(),
    function(x) as.character(x$coded_name %||% NA_character_),
    character(1)
  )
  study_trait_keys <- study_trait_keys[!is.na(study_trait_keys) & nzchar(study_trait_keys)]
  if (!is.null(active_species_traits)) {
    active_species_traits <- unique(as.character(active_species_traits))
    active_species_traits <- active_species_traits[!is.na(active_species_traits) & nzchar(active_species_traits)]
    species_trait_keys <- intersect(species_trait_keys, active_species_traits)
  }
  if (!is.null(active_study_traits)) {
    active_study_traits <- unique(as.character(active_study_traits))
    active_study_traits <- active_study_traits[!is.na(active_study_traits) & nzchar(active_study_traits)]
    study_trait_keys <- intersect(study_trait_keys, active_study_traits)
  }
  conjunction_filter_traits <- unique(c(
    taxonomic_traits,
    species_trait_keys,
    study_trait_keys
  ))
  first_token <- tokens[[1]]
  if (length(tokens) == 1 && first_token %in% special_keys) {
    return(special_group_lookup[[first_token]])
  }

  special_tokens <- tokens[tokens %in% special_keys]
  trait_tokens_all <- tokens[tokens %in% names(public_to_trait)]
  trait_keys_all <- vapply(trait_tokens_all, resolve_public_trait, character(1))
  taxon_token_idx <- which(trait_keys_all %in% taxonomic_traits)

  root_kind <- "trait"
  root_token <- first_token
  if (length(special_tokens) > 0) {
    if (length(unique(special_tokens)) > 1L) {
      stop(
        sprintf(
          "Compound policy group '%s' contains multiple special root groups.",
          group_key
        ),
        call. = FALSE
      )
    }
    root_kind <- "special"
    root_token <- special_tokens[[1]]
  } else if (length(taxon_token_idx) == 1L) {
    root_token <- trait_tokens_all[[taxon_token_idx]]
  } else if (length(taxon_token_idx) > 1L) {
    stop(
      sprintf(
        "Compound policy group '%s' combines multiple taxonomic ranks, which is structurally redundant.",
        group_key
      ),
      call. = FALSE
    )
  }

  if (identical(root_kind, "special")) {
    if (identical(root_token, "all")) {
      stop("The 'all' group cannot be conjoined with additional trait groups.", call. = FALSE)
    }
    trait_tokens <- setdiff(tokens, root_token)
    if (!all(trait_tokens %in% names(public_to_trait))) {
      stop(sprintf("Unsupported compound policy group: %s", group_key), call. = FALSE)
    }
    trait_keys <- unique(vapply(trait_tokens, resolve_public_trait, character(1)))
    invalid_filters <- setdiff(trait_keys, conjunction_filter_traits)
    if (length(invalid_filters) > 0) {
      stop(
        sprintf(
          "Compound policy group '%s' used unsupported filter trait(s): %s",
          group_key,
          paste(invalid_filters, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    if (sum(trait_keys %in% taxonomic_traits) > 1L) {
      stop(
        sprintf(
          "Compound policy group '%s' combines multiple taxonomic ranks, which is structurally redundant.",
          group_key
        ),
        call. = FALSE
      )
    }
    ordered_trait_keys <- c(
      conjunction_filter_traits[conjunction_filter_traits %in% trait_keys],
      sort(setdiff(trait_keys, conjunction_filter_traits))
    )
    ordered_trait_tokens <- unname(public_trait_keys[ordered_trait_keys])
    canonical_key <- paste(c(root_token, ordered_trait_tokens), collapse = "_")
    base_group <- special_group_lookup[[root_token]]
    display_phrase <- join_phrase(
      stringr::str_to_lower(as.character(base_group$display_phrase %||% root_token)[[1]]),
      ordered_trait_keys
    )
    return(list(
      key = canonical_key,
      coded_suffix = canonical_key,
      candidate_pool = as.character(base_group$candidate_pool %||% NA_character_)[[1]],
      display_phrase = display_phrase,
      display_phrase_title = render_title(display_phrase),
      fixed_parameters = list(match_traits = ordered_trait_keys),
      candidate_pool_definition = sprintf(
        "%s These donors must also match the target anchor on %s.",
        as.character(base_group$candidate_pool_definition %||% NA_character_)[[1]],
        join_trait_text(ordered_trait_keys)
      )
    ))
  }

  if (!all(tokens %in% names(public_to_trait))) {
    stop(sprintf("Unsupported compound policy group: %s", group_key), call. = FALSE)
  }
  if (!root_token %in% names(public_to_trait)) {
    stop(sprintf("Unsupported compound policy group: %s", group_key), call. = FALSE)
  }
  root_trait_key <- resolve_public_trait(root_token)
  filter_tokens <- setdiff(tokens, root_token)
  filter_trait_keys <- unique(vapply(filter_tokens, resolve_public_trait, character(1)))
  ordered_filter_trait_keys <- c(
    conjunction_filter_traits[conjunction_filter_traits %in% filter_trait_keys],
    sort(setdiff(filter_trait_keys, conjunction_filter_traits))
  )
  trait_keys <- unique(c(root_trait_key, ordered_filter_trait_keys))
  if (length(trait_keys) > 1L) {
    invalid_filters <- setdiff(trait_keys[-1], conjunction_filter_traits)
    if (length(invalid_filters) > 0) {
      stop(
        sprintf(
          "Compound policy group '%s' used unsupported filter trait(s): %s",
          group_key,
          paste(invalid_filters, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  if (sum(trait_keys %in% taxonomic_traits) > 1L) {
    stop(
      sprintf(
        "Compound policy group '%s' combines multiple taxonomic ranks, which is structurally redundant.",
        group_key
      ),
      call. = FALSE
    )
  }
  first_trait <- trait_keys[[1]]
  base_phrase <- paste("within", stringr::str_to_lower(display_core(first_trait)))
  display_phrase <- join_phrase(base_phrase, trait_keys[-1])
  canonical_key <- paste(
    c(
      unname(public_trait_keys[first_trait]),
      unname(public_trait_keys[trait_keys[-1]])
    ),
    collapse = "_"
  )
  list(
    key = canonical_key,
    coded_suffix = paste0("within_", canonical_key),
    candidate_pool = if (length(trait_keys) == 1 && first_trait %in% names(direct_trait_pools)) {
      unname(direct_trait_pools[first_trait])
    } else {
      "match_all_traits"
    },
    display_phrase = display_phrase,
    display_phrase_title = render_title(display_phrase),
    fixed_parameters = list(match_traits = trait_keys),
    candidate_pool_definition = sprintf(
      "Eligible donors are admissible models that match the target anchor on %s.",
      join_trait_text(trait_keys)
    )
  )
}

#' Build one policy definition from group and metric keys
#'
#' @param group_key Canonical policy group key.
#' @param metric_key Canonical policy metric key.
#' @param registry Parsed policy registry.
#'
#' @return One policy-definition list.
#' @keywords internal
#' @noRd
build_group_metric_policy <- function(group_key,
                                      metric_key,
                                      registry,
                                      active_species_traits = NULL,
                                      active_study_traits = NULL) {
  metric_lookup <- stats::setNames(
    registry$policy_metrics %||% list(),
    vapply(registry$policy_metrics %||% list(), function(x) as.character(x$key %||% NA_character_), character(1))
  )
  metric_def <- metric_lookup[[metric_key]] %||% NULL
  if (!is.list(metric_def)) {
    stop(sprintf("Unknown policy constructor metric key: %s", metric_key), call. = FALSE)
  }

  group_def <- build_policy_group_definition(
    group_key = group_key,
    registry = registry,
    active_species_traits = active_species_traits,
    active_study_traits = active_study_traits
  )
  match_traits_now <- as.character(unlist((group_def$fixed_parameters %||% list())$match_traits %||% character(0), use.names = FALSE))
  if (identical(metric_key, "species_distance") &&
    (identical(group_key, "species") || "species" %in% match_traits_now)) {
    stop(
      sprintf("Metric '%s' is not valid for policy group '%s'.", metric_key, group_key),
      call. = FALSE
    )
  }

  build_constructed_policy_definition(
    group_def = group_def,
    metric_def = metric_def,
    constructor_def = list()
  )
}

#' Build one policy definition from its canonical name
#'
#' @param policy_name Canonical policy name.
#' @param policy_path Optional registry path.
#' @param registry Optional parsed policy registry.
#'
#' @return One policy-definition list or `NULL`.
#' @keywords internal
#' @noRd
build_policy_definition_from_name <- function(policy_name,
                                              policy_path = NULL,
                                              registry = NULL) {
  policy_name <- stringr::str_squish(as.character(policy_name %||% ""))[[1]]
  if (is.na(policy_name) || !nzchar(policy_name)) {
    return(NULL)
  }
  if (!is.list(registry)) {
    registry <- read_policy_registry(policy_path = policy_path)
  }
  metric_defs <- registry$policy_metrics %||% list()
  prefixes <- vapply(metric_defs, function(x) as.character(x$coded_prefix %||% NA_character_), character(1))
  order_idx <- order(nchar(prefixes), decreasing = TRUE)
  metric_def <- NULL
  suffix <- NULL
  for (idx in order_idx) {
    prefix_now <- prefixes[[idx]]
    if (!is.na(prefix_now) && nzchar(prefix_now) && startsWith(policy_name, prefix_now)) {
      metric_def <- metric_defs[[idx]]
      suffix <- substring(policy_name, nchar(prefix_now) + 1L)
      break
    }
  }
  if (!is.list(metric_def) || is.null(suffix) || !nzchar(suffix)) {
    return(NULL)
  }
  group_key <- if (startsWith(suffix, "within_")) {
    substring(suffix, nchar("within_") + 1L)
  } else {
    suffix
  }
  tryCatch(
    build_group_metric_policy(
      group_key = group_key,
      metric_key = as.character(metric_def$key %||% NA_character_)[[1]],
      registry = registry
    ),
    error = function(e) NULL
  )
}

#' Read the policy registry
#'
#' Reads the packaged policy registry JSON or a caller-supplied registry file.
#'
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return A parsed registry list.
#'
#' @keywords internal
#' @noRd
read_policy_registry <- local({
  cache_env <- new.env(parent = emptyenv())
  function(policy_path = NULL) {
    if (is.null(policy_path)) {
      package_root <- tryCatch(
        normalizePath(
          system.file(package = "tsbiomass"),
          winslash = "/",
          mustWork = TRUE
        ),
        error = function(e) ""
      )
      local_policy_path <- if (nzchar(package_root)) {
        file.path(package_root, "inst", "templates", "policy_registry.json")
      } else {
        file.path(getwd(), "inst", "templates", "policy_registry.json")
      }
      if (!file.exists(local_policy_path)) {
        local_policy_path <- file.path(getwd(), "inst", "templates", "policy_registry.json")
      }
      policy_path <- if (file.exists(local_policy_path)) {
        local_policy_path
      } else {
        system.file("templates", "policy_registry.json", package = "tsbiomass")
      }
    }

    if (!is.character(policy_path) || length(policy_path) != 1 || !nzchar(policy_path)) {
      stop("'policy_path' must be NULL or a single file path.", call. = FALSE)
    }
    if (!file.exists(policy_path)) {
      stop(sprintf("Policy registry not found: %s", policy_path), call. = FALSE)
    }

    cache_key <- normalizePath(policy_path, winslash = "/", mustWork = FALSE)
    if (exists(cache_key, envir = cache_env, inherits = FALSE)) {
      return(get(cache_key, envir = cache_env, inherits = FALSE))
    }

    registry <- read_json_file(policy_path)
    registry <- expand_policy_registry(registry)
    if (!is.list(registry) || is.null(registry$policies) || !is.list(registry$policies)) {
      stop(
        "The policy registry must contain a top-level 'policies' list or a constructor-based 'policy_constructors' section.",
        call. = FALSE
      )
    }

    validate_unique_policy_names <- function(policy_list, section_name) {
      if (is.null(policy_list) || !is.list(policy_list) || length(policy_list) == 0) {
        return(invisible(NULL))
      }
      coded_names <- vapply(
        policy_list,
        function(x) as.character(x$coded_name %||% NA_character_),
        character(1)
      )
      dup_names <- unique(coded_names[duplicated(coded_names) & !is.na(coded_names) & nzchar(coded_names)])
      if (length(dup_names) > 0) {
        stop(
          sprintf(
            "Policy registry section '%s' contains duplicate coded_name value(s): %s",
            section_name,
            paste(dup_names, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }

    validate_unique_policy_names(registry$policies, "policies")
    validate_policy_branch_section <- function(branch_defs) {
      if (is.null(branch_defs)) {
        return(invisible(NULL))
      }
      if (!is.list(branch_defs)) {
        stop("Policy registry field 'policy_branches' must be a list.", call. = FALSE)
      }
      if (length(branch_defs) == 0) {
        return(invisible(NULL))
      }

      branch_keys <- vapply(
        branch_defs,
        function(x) as.character(x$key %||% NA_character_),
        character(1)
      )
      if (any(is.na(branch_keys) | !nzchar(branch_keys))) {
        stop("Every policy branch definition must declare a non-empty 'key'.", call. = FALSE)
      }
      dup_keys <- unique(branch_keys[duplicated(branch_keys)])
      if (length(dup_keys) > 0) {
        stop(
          sprintf(
            "Policy registry field 'policy_branches' contains duplicate key value(s): %s",
            paste(dup_keys, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      alias_values <- unlist(lapply(branch_defs, function(x) x$aliases %||% character(0)), use.names = FALSE)
      if (length(alias_values) > 0) {
        alias_values <- as.character(alias_values)
        if (any(is.na(alias_values) | !nzchar(alias_values))) {
          stop("Policy branch aliases must all be non-empty strings.", call. = FALSE)
        }
      }

      row_filter_values <- vapply(
        branch_defs,
        function(x) {
          value <- x$row_filter_value %||% NA_character_
          as.character(value)[[1]]
        },
        character(1)
      )
      finite_filter_values <- row_filter_values[!is.na(row_filter_values) & nzchar(row_filter_values)]
      if (length(finite_filter_values) > 0 && anyDuplicated(finite_filter_values)) {
        dup_values <- unique(finite_filter_values[duplicated(finite_filter_values)])
        stop(
          sprintf(
            "Policy branch row_filter_value entries must be unique; duplicates found: %s",
            paste(dup_values, collapse = ", ")
          ),
          call. = FALSE
        )
      }

      invisible(NULL)
    }

    validate_policy_branch_section(registry$policy_branches %||% NULL)

    assign(cache_key, registry, envir = cache_env)
    registry
  }
})

#' List available policy components
#'
#' Returns the policy groups, aggregation metrics, slope branches, and
#' materialized policy definitions available from the policy registry.
#'
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return A named list with `groups`, `metrics`, `branches`, and `policies`
#'   tibbles.
#'
#' @examples
#' available_policies()
#'
#' @export
available_policies <- function(policy_path = NULL) {
  registry <- read_policy_registry(policy_path = policy_path)
  list_to_tibble <- function(items) {
    items <- items %||% list()
    field_names <- unique(unlist(lapply(items, names), use.names = FALSE))
    complex_fields <- field_names[vapply(field_names, function(field_name) {
      any(vapply(items, function(item) {
        value <- as.list(item)[[field_name]]
        is.null(value) || is.list(value) || length(value) != 1L
      }, logical(1)))
    }, logical(1))]

    purrr::map_dfr(items, function(item) {
      row <- as.list(item)
      row <- stats::setNames(lapply(field_names, function(field_name) {
        value <- row[[field_name]]
        if (field_name %in% complex_fields) {
          return(list(value))
        }
        if (is.null(value) || length(value) == 0L) {
          return(NA)
        }
        value
      }), field_names)
      tibble::as_tibble(row)
    })
  }

  policies <- list_to_tibble(registry$policies)
  groups <- list_to_tibble(registry$policy_groups)
  metrics <- list_to_tibble(registry$policy_metrics)
  branches <- list_to_tibble(registry$policy_branches)

  list(
    groups = groups,
    metrics = metrics,
    branches = branches,
    policies = policies
  )
}

#' Resolve available policy names
#'
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
available_policy_names <- function(policy_path = NULL) {
  registry <- read_policy_registry(policy_path = policy_path)
  unique(vapply(registry$policies, function(x) x$coded_name, character(1)))
}

#' Explain one or more policies
#'
#' Returns the canonical policy identifier, display label, policy family,
#' candidate-pool definition, aggregation definition, and plain-language
#' description for one or more policies defined in the policy registry.
#'
#' @param policy Character vector of canonical policy names.
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return A tibble with one row per requested policy.
#'
#' @examples
#' explain(c("closest_within_species", "closest_generalized"))
#'
#' @export
explain <- function(policy,
                    policy_path = NULL) {
  if (!is.character(policy) || length(policy) == 0) {
    stop("'policy' must be a non-empty character vector.", call. = FALSE)
  }

  lookup <- policy_lookup_table(policy_path = policy_path)
  policy_vals <- stringr::str_squish(as.character(policy))
  policy_vals <- policy_vals[!is.na(policy_vals) & nzchar(policy_vals)]
  if (length(policy_vals) == 0) {
    stop("'policy' must contain at least one non-empty policy name.", call. = FALSE)
  }

  out <- purrr::map_dfr(policy_vals, function(policy_now) {
    policy_def <- lookup[[policy_now]] %||%
      build_policy_definition_from_name(policy_now, policy_path = policy_path) %||%
      NULL
    if (!is.list(policy_def)) {
      stop(sprintf("Unknown policy name: %s", policy_now), call. = FALSE)
    }
    canonical_name <- as.character(policy_def$coded_name %||% policy_now)[[1]]
    tibble::tibble(
      requested_policy = policy_now,
      policy = canonical_name,
      display_name = as.character(policy_def$display_name %||% canonical_name)[[1]],
      policy_family = as.character(policy_def$policy_family %||% NA_character_)[[1]],
      candidate_pool = as.character(policy_def$candidate_pool %||% NA_character_)[[1]],
      aggregation_method = as.character(policy_def$aggregation_method %||% NA_character_)[[1]],
      grouping_key = as.character(policy_def$grouping_key %||% NA_character_)[[1]],
      metric_key = as.character(policy_def$metric_key %||% NA_character_)[[1]],
      candidate_pool_definition = as.character(policy_def$candidate_pool_definition %||% NA_character_)[[1]],
      aggregation_definition = as.character(policy_def$aggregation_definition %||% NA_character_)[[1]],
      plain_language_definition = as.character(policy_def$plain_language_definition %||% NA_character_)[[1]]
    )
  })

  out
}

#' Build a canonical policy-definition lookup
#'
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return Named list keyed by canonical policy coded name.
#' @keywords internal
#' @noRd
policy_lookup_table <- local({
  cache_env <- new.env(parent = emptyenv())

  function(policy_path = NULL) {
    cache_key <- if (is.null(policy_path)) {
      "<default>"
    } else {
      normalizePath(policy_path, winslash = "/", mustWork = FALSE)
    }
    if (exists(cache_key, envir = cache_env, inherits = FALSE)) {
      return(get(cache_key, envir = cache_env, inherits = FALSE))
    }

    registry <- read_policy_registry(policy_path = policy_path)
    lookup <- list()

    add_defs <- function(defs) {
      if (is.null(defs) || !is.list(defs)) {
        return(invisible(NULL))
      }
      for (def in defs) {
        coded_name <- as.character(def$coded_name %||% NA_character_)[[1]]
        if (!is.na(coded_name) && nzchar(coded_name) && is.null(lookup[[coded_name]])) {
          lookup[[coded_name]] <<- def
        }
      }
      invisible(NULL)
    }

    # Main benchmark policies define the canonical package behavior.
    add_defs(registry$policies)

    assign(cache_key, lookup, envir = cache_env)
    lookup
  }
})

#' Canonicalize policy identifiers
#'
#' @param policy_name Character vector of policy identifiers.
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return Character vector of canonical coded names.
#' @keywords internal
#' @noRd
canonical_policy_name <- local({
  .cache <- new.env(parent = emptyenv())
  function(policy_name,
           policy_path = NULL) {
    lookup <- policy_lookup_table(policy_path = policy_path)
    registry <- read_policy_registry(policy_path = policy_path)
    policy_vals <- as.character(policy_name)
    uniq_vals <- unique(policy_vals)
    cache_prefix <- if (is.null(policy_path)) "" else policy_path
    canon_uniq <- vapply(uniq_vals, function(policy_now) {
      cache_key <- paste0(cache_prefix, policy_now)
      cached <- .cache[[cache_key]]
      if (!is.null(cached)) {
        return(cached)
      }
      policy_def <- lookup[[policy_now]] %||%
        build_policy_definition_from_name(policy_now, registry = registry) %||%
        NULL
      result <- as.character(policy_def$coded_name %||% policy_now)[[1]]
      .cache[[cache_key]] <- result
      result
    }, character(1))
    unname(canon_uniq[policy_vals])
  }
})

#' Resolve policy ordination context
#'
#' Normalizes optional ordination context for the policy layer.
#'
#' @param ordination_info Optional ordination-context list.
#'
#' @return A normalized context list.
#'
#' @keywords internal
#' @noRd
resolve_policy_context <- function(ordination_info = NULL) {
  # Resolve the optional ordination context once, then fill the missing pieces
  # with empty defaults used by the policy selectors.
  context <- ordination_info %||% list()

  if (!is.list(context)) {
    stop("Ordination context must be NULL or a list.", call. = FALSE)
  }

  list(
    model_scores = context$model_scores %||% NULL,
    anchor_cluster = context$anchor_cluster %||% NA_character_,
    species_ellipse_ids = context$species_ellipse_ids %||% character(0)
  )
}

#' Normalize policy parameters
#'
#' Combines fixed registry parameters with caller-supplied overrides for one
#' policy. Caller overrides are expected as `policy_params[[policy_name]]`.
#'
#' @param policy_name Policy coded name.
#' @param policy_def One policy-definition list from the registry.
#' @param policy_params Optional named list of per-policy parameter overrides.
#'
#' @return A named list of resolved policy parameters.
#'
#' @keywords internal
#' @noRd
policy_parameters <- function(policy_name,
                              policy_def,
                              policy_params = NULL) {
  # Start from registry-fixed values, then merge caller overrides so policy
  # defaults stay centralized in the registry but remain overridable.
  params <- policy_def$fixed_parameters %||% list()
  if (!is.list(params)) {
    params <- list()
  }

  override <- policy_params[[policy_name]] %||% list()
  if (!is.list(override)) {
    stop(sprintf("Policy parameters for '%s' must be a list.", policy_name), call. = FALSE)
  }
  params <- utils::modifyList(params, override)

  # Fill the generic tunable defaults only when the registry or caller did not
  # already supply them for the selected policy.
  if (is.null(params$phylo_radius) && identical(policy_def$candidate_pool, "phylogenetic_neighborhood")) {
    params$phylo_radius <- NULL
  }

  params
}

#' Normalize model weights
#'
#' Converts one numeric weight vector to nonnegative normalized weights.
#'
#' @param weight_values Numeric vector.
#'
#' @return Numeric vector summing to one, or `numeric(0)` if unusable.
#'
#' @keywords internal
#' @noRd
normalized_weights <- function(weight_values) {
  # Clamp invalid weights to zero before normalization so later weighted
  # predictions do not have to repeat the same safety logic.
  weights <- as.numeric(weight_values)
  weights[!is.finite(weights) | weights < 0] <- 0
  weight_sum <- sum(weights, na.rm = TRUE)

  if (!is.finite(weight_sum) || weight_sum <= 0) {
    return(numeric(0))
  }

  weights / weight_sum
}

#' Filter valid multiplier rows
#'
#' Keeps only rows with a positive finite biomass multiplier.
#'
#' @param rows Candidate-policy row table.
#'
#' @return Tibble.
#'
#' @keywords internal
#' @noRd
valid_multiplier_rows <- function(rows) {
  # Centralize the multiplier-validity filter so all policy aggregators apply
  # the same numerical screening rules.
  tibble::as_tibble(rows) |>
    dplyr::filter(
      is.finite(.data$biomass_multiplier_if_replace),
      .data$biomass_multiplier_if_replace > 0
    )
}

#' Filter valid equation rows
#'
#' Keeps only rows with finite standardized length-form TS coefficients.
#'
#' @param rows Candidate-policy row table.
#'
#' @return Tibble.
#'
#' @keywords internal
#' @noRd
policy_equation_aliases <- function(rows) {
  out <- tibble::as_tibble(rows)
  if (!"slope_len" %in% names(out) && "slope_standard" %in% names(out)) {
    out$slope_len <- suppressWarnings(as.numeric(out$slope_standard))
  }
  if (!"intercept_len" %in% names(out) && "intercept_standard" %in% names(out)) {
    out$intercept_len <- suppressWarnings(as.numeric(out$intercept_standard))
  }
  out
}

#' Filter valid equation rows
#'
#' @param rows Candidate-policy row table.
#'
#' @return Tibble.
#'
#' @keywords internal
#' @noRd
valid_equation_rows <- function(rows) {
  rows <- policy_equation_aliases(rows)
  if (isTRUE(attr(rows, "tsb_valid_equations"))) {
    return(rows)
  }
  keep <- is.finite(rows$slope_len) & is.finite(rows$intercept_len)
  out <- rows[keep, , drop = FALSE]
  attr(out, "tsb_valid_equations") <- TRUE
  out
}

#' Identify generalized-model rows
#'
#' @param rows Candidate-policy row table.
#'
#' @return Tibble.
#'
#' @keywords internal
#' @noRd
group_model_rows <- function(rows) {
  # Treat explicit prepared flags or absent species identity as evidence that a
  # row represents a generalized or grouped model.
  out <- tibble::as_tibble(rows)

  # Evaluate the optional grouping flags against the materialized tibble so the
  # filter does not depend on the magrittr pronoun.
  has_group_flag <- "is_group_model" %in% names(out)
  has_method_flag <- "method_type" %in% names(out)
  generalized_flag <- rep(FALSE, nrow(out))

  if (has_group_flag) {
    generalized_flag <- generalized_flag | (as.logical(out$is_group_model) %in% TRUE)
  }
  if (has_method_flag) {
    method_type <- tolower(trimws(as.character(out$method_type)))
    generalized_flag <- generalized_flag | method_type %in% c("group", "generalized", "generalised")
  }

  species_cols <- intersect(
    c("species_name", "species", "species_species_name"),
    names(out)
  )
  if (length(species_cols) > 0) {
    species_missing <- Reduce(
      `|`,
      lapply(species_cols, function(col) {
        value <- trimws(as.character(out[[col]]))
        is.na(out[[col]]) |
          !nzchar(value) |
          tolower(value) %in% c("na", "n/a", "na na", "unknown", "unknown unknown")
      })
    )
    generalized_flag <- generalized_flag | species_missing
  }

  out |>
    dplyr::filter(.env$generalized_flag)
}

#' Identify canonical 20-log10 slope rows
#'
#' @param rows Candidate-policy row table.
#' @param tolerance Numeric slope tolerance around 20.
#'
#' @return Logical vector.
#' @keywords internal
#' @noRd
slope20_indicator <- function(rows,
                              tolerance = 1e-8) {
  rows <- policy_equation_aliases(rows)
  if (!"slope_len" %in% names(rows)) {
    return(rep(FALSE, nrow(rows)))
  }

  slope_vals <- suppressWarnings(as.numeric(rows$slope_len))
  is.finite(slope_vals) & abs(slope_vals - 20) <= tolerance
}

#' Normalize policy equation-branch filters
#'
#' @param filters Optional character vector of branch filters.
#'
#' @return Character vector of normalized branch-filter labels.
#' @keywords internal
#' @noRd
normalize_policy_equation_branch_filters <- local({
  cache_env <- new.env(parent = emptyenv())
  function(filters = NULL, policy_path = NULL) {
    if (is.null(filters)) {
      return("all")
    }

    out <- as.character(unlist(filters, use.names = FALSE))
    out <- out[!is.na(out) & nzchar(out)]
    if (length(out) == 0) {
      return("all")
    }

    cache_key <- if (is.null(policy_path)) "<default>" else normalizePath(policy_path, winslash = "/", mustWork = FALSE)
    branch_defs <- if (exists(cache_key, envir = cache_env, inherits = FALSE)) {
      get(cache_key, envir = cache_env, inherits = FALSE)
    } else {
      defs <- read_policy_registry(policy_path = policy_path)$policy_branches %||% list()
      assign(cache_key, defs, envir = cache_env)
      defs
    }
    if (!is.list(branch_defs) || length(branch_defs) == 0) {
      stop("Policy registry did not define any policy_branches.", call. = FALSE)
    }

    alias_map <- character(0)
    for (branch_def in branch_defs) {
      key_now <- as.character(branch_def$key %||% NA_character_)[[1]]
      if (is.na(key_now) || !nzchar(key_now)) {
        next
      }
      alias_now <- as.character(unlist(branch_def$aliases %||% character(0), use.names = FALSE))
      alias_now <- unique(c(key_now, alias_now))
      alias_now <- alias_now[!is.na(alias_now) & nzchar(alias_now)]
      if (length(alias_now) > 0) {
        alias_map[alias_now] <- key_now
      }
    }

    normalized <- unname(alias_map[out])

    if (anyNA(normalized)) {
      bad <- unique(out[is.na(normalized)])
      stop(
        sprintf(
          "Unsupported equation_branch_filter value(s): %s. Allowed canonical values are: %s",
          paste(bad, collapse = ", "),
          paste(unique(vapply(branch_defs, function(x) as.character(x$key %||% NA_character_), character(1))), collapse = ", ")
        ),
        call. = FALSE
      )
    }

    normalized
  }
})

#' Classify rows by standardized equation branch
#'
#' @param rows Candidate-policy row table.
#' @param tolerance Numeric slope tolerance around 20.
#'
#' @return Character vector with `fixed20`, `free_slope`, or `unknown`.
#' @keywords internal
#' @noRd
classify_equation_branch <- function(rows,
                                     tolerance = 1e-8) {
  rows <- policy_equation_aliases(rows)
  if (!"slope_len" %in% names(rows)) {
    return(rep("unknown", nrow(rows)))
  }
  slope_vals <- suppressWarnings(as.numeric(rows$slope_len))
  ifelse(
    !is.finite(slope_vals),
    "unknown",
    ifelse(abs(slope_vals - 20) <= tolerance, "fixed20", "free_slope")
  )
}

#' Filter donor rows by standardized equation branch
#'
#' @param rows Candidate-policy row table.
#' @param equation_branch_filter One normalized branch-filter label.
#'
#' @return Tibble.
#' @keywords internal
#' @noRd
policy_branch_filter <- function(rows,
                                 equation_branch_filter = "all") {
  rows <- tibble::as_tibble(rows)
  equation_branch_filter <- normalize_policy_equation_branch_filters(equation_branch_filter)[[1]]
  branch_defs <- read_policy_registry()$policy_branches %||% list()
  filter_lookup <- stats::setNames(
    vapply(
      branch_defs,
      function(x) as.character(x$row_filter_value %||% NA_character_)[[1]],
      character(1)
    ),
    vapply(branch_defs, function(x) as.character(x$key %||% NA_character_), character(1))
  )

  if (!"equation_branch_filter" %in% names(rows)) {
    rows$equation_branch_filter <- classify_equation_branch(rows)
  }

  filter_value <- unname(filter_lookup[equation_branch_filter])
  if (is.na(filter_value) || !nzchar(filter_value)) {
    return(rows)
  }

  dplyr::filter(rows, equation_branch_filter == filter_value)
}

#' Build one display label for a policy-branch combination
#'
#' @param policy_name Base policy name.
#' @param equation_branch_filter One or more normalized branch-filter labels.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
policy_display_label <- function(policy_name,
                                 equation_branch_filter = "all") {
  policy_vals <- as.character(policy_name)
  branch_vals <- normalize_policy_equation_branch_filters(equation_branch_filter)
  n_out <- max(length(policy_vals), length(branch_vals))

  if (length(policy_vals) == 0) {
    return(character(0))
  }
  if (length(branch_vals) == 0) {
    branch_vals <- "all"
  }

  policy_vals <- rep_len(policy_vals, n_out)
  branch_vals <- rep_len(branch_vals, n_out)

  vapply(seq_len(n_out), function(i) {
    policy_now <- policy_vals[[i]]
    branch_now <- branch_vals[[i]]
    if (policy_display_value_missing(policy_now)) {
      return(NA_character_)
    }
    if (policy_display_value_missing(branch_now)) {
      branch_now <- "all"
    }
    policy_def <- policy_lookup_table()[[policy_now]] %||%
      build_policy_definition_from_name(policy_now) %||%
      NULL

    if (is.list(policy_def)) {
      branch_defs <- read_policy_registry()$policy_branches %||% list()
      branch_tags <- stats::setNames(
        vapply(branch_defs, function(x) as.character(x$display_tag %||% x$key %||% NA_character_), character(1)),
        vapply(branch_defs, function(x) as.character(x$key %||% NA_character_), character(1))
      )
      branch_display <- unname(branch_tags[branch_now] %||% branch_now)
      display_name <- as.character(policy_def$display_name %||% NA_character_)[[1]]
      if (is.na(display_name) || !nzchar(display_name)) {
        display_name <- snake_title(
          as.character(policy_def$coded_name %||% policy_now)
        )
      }
      return(paste0(display_name, " [", branch_display, "]"))
    }

    paste(policy_now, branch_now, sep = " / ")
  }, character(1))
}

#' Select policy rows
#'
#' Filters the admissible donor pool for one policy according to its registry
#' candidate pool and any resolved parameters.
#'
#' @param rows Admissible candidate rows.
#' @param policy_def One policy-definition list.
#' @param policy_params Resolved parameters for the policy.
#' @param ordination_info Optional ordination-context list.
#'
#' @return Tibble.
#'
#' @keywords internal
#' @noRd
policy_rows <- function(rows,
                        policy_def,
                        policy_params,
                        ordination_info) {
  # Use the registry candidate-pool definition as the single policy-routing
  # switch so selection logic stays aligned with the registry names.
  pool_name <- as.character(policy_def$candidate_pool)[[1]]
  policy_rows_ <- tibble::as_tibble(rows)

  if (!"model_id" %in% names(policy_rows_) && "model_id" %in% names(policy_rows_)) {
    policy_rows_$model_id <- as.character(policy_rows_$model_id)
  }
  subset_flag <- function(flag_col) {
    if (!flag_col %in% names(policy_rows_)) {
      return(policy_rows_[0, , drop = FALSE])
    }
    keep <- as.logical(policy_rows_[[flag_col]]) %in% TRUE
    policy_rows_[keep, , drop = FALSE]
  }

  selected_rows <- NULL
  if (identical(pool_name, "all_admissible")) {
    selected_rows <- policy_rows_
  }
  if (is.null(selected_rows) && identical(pool_name, "all_valid_models")) {
    selected_rows <- policy_rows_
  }
  if (is.null(selected_rows) && identical(pool_name, "same_species")) {
    selected_rows <- subset_flag("overlap_same_species")
  }
  if (is.null(selected_rows) && identical(pool_name, "match_all_traits")) {
    selected_rows <- policy_rows_
  }
  if (is.null(selected_rows) && identical(pool_name, "nearest_phylogenetic")) {
    for (out in list(
      subset_flag("overlap_same_species"),
      policy_rows_[(as.logical(policy_rows_$overlap_same_genus) %in% TRUE) & !(as.logical(policy_rows_$overlap_same_species) %in% TRUE), , drop = FALSE],
      policy_rows_[(as.logical(policy_rows_$overlap_same_family) %in% TRUE) & !(as.logical(policy_rows_$overlap_same_genus) %in% TRUE), , drop = FALSE],
      policy_rows_[(as.logical(policy_rows_$overlap_same_order) %in% TRUE) & !(as.logical(policy_rows_$overlap_same_family) %in% TRUE), , drop = FALSE]
    )) {
      if (nrow(out) > 0) {
        selected_rows <- out
        break
      }
    }
    if (is.null(selected_rows)) {
      selected_rows <- policy_rows_[0, , drop = FALSE]
    }
  }
  if (is.null(selected_rows) && identical(pool_name, "phylogenetic_neighborhood")) {
    out <- policy_rows_[!(as.logical(policy_rows_$overlap_same_species) %in% TRUE), , drop = FALSE]
    if (!is.null(policy_params$phylo_radius)) {
      radius <- as.numeric(policy_params$phylo_radius)
      keep <- is.finite(out$taxonomic_distance_to_anchor) &
        suppressWarnings(as.numeric(out$taxonomic_distance_to_anchor)) <= radius
      out <- out[keep, , drop = FALSE]
    }
    selected_rows <- out
  }
  if (is.null(selected_rows) && identical(pool_name, "same_genus")) {
    selected_rows <- subset_flag("overlap_same_genus")
  }
  if (is.null(selected_rows) && identical(pool_name, "same_family")) {
    selected_rows <- subset_flag("overlap_same_family")
  }
  if (is.null(selected_rows) && identical(pool_name, "same_order")) {
    selected_rows <- subset_flag("overlap_same_order")
  }
  if (is.null(selected_rows) && identical(pool_name, "same_nmds_cluster")) {
    score_tbl <- tibble::as_tibble(ordination_info$model_scores %||% tibble::tibble())
    if (!all(c("nmds_cluster", "model_id") %in% names(score_tbl)) ||
      is.na(ordination_info$anchor_cluster)) {
      selected_rows <- policy_rows_[0, , drop = FALSE]
    } else {
      cluster_ids <- unique(as.character(score_tbl$model_id[score_tbl$nmds_cluster == ordination_info$anchor_cluster]))
      selected_rows <- policy_rows_[as.character(policy_rows_$model_id) %in% cluster_ids, , drop = FALSE]
    }
  }
  if (is.null(selected_rows) && identical(pool_name, "same_species_ellipse")) {
    keep <- as.character(policy_rows_$model_id) %in% ordination_info$species_ellipse_ids &
      !(as.logical(policy_rows_$overlap_same_species) %in% TRUE)
    selected_rows <- policy_rows_[keep, , drop = FALSE]
  }
  if (is.null(selected_rows) && identical(pool_name, "generalized_models_only")) {
    selected_rows <- group_model_rows(policy_rows_)
  }
  if (is.null(selected_rows) && identical(pool_name, "closest_study_cell")) {
    cell_col <- "study_cell_id"
    ranked <- valid_equation_rows(policy_rows_) |>
      dplyr::arrange(.data$combined_distance)
    if (nrow(ranked) == 0) {
      selected_rows <- policy_rows_[0, , drop = FALSE]
    } else if (!cell_col %in% names(ranked)) {
      selected_rows <- dplyr::slice_head(ranked, n = 1)
    } else {
      cell_id <- ranked[[cell_col]][[1]]
      if (is.na(cell_id) || !nzchar(as.character(cell_id))) {
        selected_rows <- dplyr::slice_head(ranked, n = 1)
      } else {
        selected_rows <- valid_equation_rows(policy_rows_) |>
          dplyr::filter(.data[[cell_col]] == cell_id)
      }
    }
  }
  if (is.null(selected_rows) && identical(pool_name, "same_fao_area")) {
    selected_rows <- subset_flag("overlap_same_fao_area")
  }
  if (is.null(selected_rows) && identical(pool_name, "same_ocean_basin")) {
    selected_rows <- subset_flag("overlap_same_ocean_basin")
  }
  if (is.null(selected_rows) && identical(pool_name, "same_equation_form")) {
    selected_rows <- subset_flag("overlap_same_equation_form")
  }
  if (is.null(selected_rows) && identical(pool_name, "same_derivation")) {
    selected_rows <- subset_flag("overlap_same_derivation")
  }

  if (is.null(selected_rows)) {
    stop(sprintf("Unsupported candidate pool: %s", pool_name), call. = FALSE)
  }

  # Apply any trait-match restrictions after the base pool is resolved so
  # compound policies such as generalized_fao or study_cell_species do not need
  # bespoke pool implementations.
  match_traits <- as.character(unlist(policy_params$match_traits %||% character(0), use.names = FALSE))
  match_traits <- match_traits[!is.na(match_traits) & nzchar(match_traits)]
  if (length(match_traits) > 0) {
    overlap_cols <- paste0("overlap_same_", match_traits)
    missing_cols <- setdiff(overlap_cols, names(selected_rows))
    if (length(missing_cols) > 0) {
      stop(
        sprintf(
          "Policy trait filtering requires overlap column(s): %s",
          paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    keep_idx <- rep(TRUE, nrow(selected_rows))
    for (col_name in overlap_cols) {
      keep_idx <- keep_idx & !is.na(selected_rows[[col_name]]) & as.logical(selected_rows[[col_name]])
    }
    selected_rows <- selected_rows[keep_idx, , drop = FALSE]
  }

  selected_rows
}

#' Build an equation row
#'
#' @param slope Numeric slope.
#' @param intercept Numeric intercept.
#'
#' @return One-row tibble.
#'
#' @keywords internal
#' @noRd
build_policy_equation_row <- function(slope,
                                      intercept) {
  list(
    policy_slope_len = as.numeric(slope),
    policy_intercept_len = as.numeric(intercept)
  )
}

#' Select one nearest policy equation by distance column
#'
#' @param candidate_rows Candidate-policy row table.
#' @param distance_column Optional distance column used as the primary ranking
#'   key. When `NULL`, `combined_distance` is used.
#' @param distance_as_tiebreak Logical scalar. When `TRUE`, `distance_column`
#'   is used only as a tiny tiebreak on top of `combined_distance`.
#'
#' @return One-row tibble.
#'
#' @keywords internal
#' @noRd
nearest_equation_by <- function(candidate_rows,
                                distance_column = NULL,
                                distance_as_tiebreak = FALSE) {
  # Filter to rows with usable coefficients before ranking by the requested
  # distance field.
  candidate_rows <- valid_equation_rows(candidate_rows)
  if (nrow(candidate_rows) == 0L) {
    return(build_policy_equation_row(NA_real_, NA_real_))
  }

  ranking_rows <- candidate_rows
  ranking_score <- suppressWarnings(as.numeric(candidate_rows$combined_distance))
  if (!is.null(distance_column) && distance_column %in% names(candidate_rows)) {
    primary_distance <- suppressWarnings(as.numeric(candidate_rows[[distance_column]]))
    if (isTRUE(distance_as_tiebreak)) {
      ranking_score <- ranking_score + primary_distance * 1e-9
    } else {
      keep <- is.finite(primary_distance)
      if (any(keep)) {
        ranking_rows <- candidate_rows[keep, , drop = FALSE]
        primary_distance <- primary_distance[keep]
      }
      ranking_score <- primary_distance + suppressWarnings(as.numeric(ranking_rows$combined_distance)) * 1e-9
    }
  }

  if (!any(is.finite(ranking_score))) {
    return(build_policy_equation_row(NA_real_, NA_real_))
  }

  row_index <- which.min(ranking_score)
  if (length(row_index) == 0L) {
    return(build_policy_equation_row(NA_real_, NA_real_))
  }

  build_policy_equation_row(
    ranking_rows$slope_len[[row_index]],
    ranking_rows$intercept_len[[row_index]]
  )
}

#' Compute one weighted equation
#'
#' @param rows Candidate-policy row table.
#'
#' @return One-row tibble.
#'
#' @keywords internal
#' @noRd
weighted_equation <- function(rows) {
  keep_rows <- valid_equation_rows(rows)
  keep_rows <- keep_rows[is.finite(keep_rows$w_adm) & keep_rows$w_adm > 0, , drop = FALSE]
  weights <- normalized_weights(keep_rows$w_adm)

  if (nrow(keep_rows) == 0 || length(weights) == 0) {
    return(build_policy_equation_row(NA_real_, NA_real_))
  }

  build_policy_equation_row(
    sum(weights * keep_rows$slope_len),
    sum(weights * keep_rows$intercept_len)
  )
}

#' Resolve the grouping columns for study-level policy aggregation
#'
#' @param rows Candidate donor rows.
#'
#' @return Character vector of grouping columns.
#'
#' @keywords internal
#' @noRd
study_group_columns <- function(rows) {
  rows <- tibble::as_tibble(rows)
  preferred <- c("citation", "species_name", "equation_form")
  fallback <- c("study_cell_id", "species_name", "equation_form")
  if (all(preferred %in% names(rows))) {
    return(preferred)
  }
  if (all(fallback %in% names(rows))) {
    return(fallback)
  }
  if ("citation" %in% names(rows)) {
    return("citation")
  }
  if ("study_cell_id" %in% names(rows)) {
    return("study_cell_id")
  }
  character(0)
}

#' Collapse donor rows to study-level ensembles
#'
#' @param rows Candidate donor rows.
#' @param group_weight Study-level weighting rule.
#' @param within_weight Within-study weighting rule.
#'
#' @return Tibble of study-level ensemble rows.
#'
#' @keywords internal
#' @noRd
study_ensemble_rows <- function(rows,
                                group_weight = c("max", "mean", "sum"),
                                within_weight = c("kernel", "equal")) {
  group_weight <- match.arg(group_weight)
  within_weight <- match.arg(within_weight)
  keep_rows <- valid_equation_rows(rows) |>
    dplyr::filter(is.finite(.data$w_adm), .data$w_adm > 0)
  if (!"combined_distance" %in% names(keep_rows)) {
    keep_rows$combined_distance <- NA_real_
  }
  if (!"trait_gower_distance" %in% names(keep_rows)) {
    keep_rows$trait_gower_distance <- NA_real_
  }
  group_cols <- study_group_columns(keep_rows)
  if (nrow(keep_rows) == 0 || length(group_cols) == 0) {
    return(keep_rows)
  }

  keep_rows |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      .study_group_raw_weight = if (group_weight == "sum") {
        sum(.data$w_adm, na.rm = TRUE)
      } else if (group_weight == "mean") {
        mean(.data$w_adm, na.rm = TRUE)
      } else {
        max(.data$w_adm, na.rm = TRUE)
      },
      .within_raw_weight = if (within_weight == "kernel") .data$w_adm else 1
    ) |>
    dplyr::mutate(
      .within_weight = .data$.within_raw_weight / sum(.data$.within_raw_weight, na.rm = TRUE),
      .study_slope_len = sum(.data$.within_weight * .data$slope_len, na.rm = TRUE),
      .study_intercept_len = sum(.data$.within_weight * .data$intercept_len, na.rm = TRUE),
      .study_within_slope_var = stats::weighted.mean(
        (.data$slope_len - .data$.study_slope_len)^2,
        .data$.within_weight,
        na.rm = TRUE
      ),
      .study_within_intercept_var = stats::weighted.mean(
        (.data$intercept_len - .data$.study_intercept_len)^2,
        .data$.within_weight,
        na.rm = TRUE
      ),
      .study_combined_distance = if (any(is.finite(.data$combined_distance))) {
        sum(.data$.within_weight * .data$combined_distance, na.rm = TRUE)
      } else {
        NA_real_
      },
      .study_trait_gower_distance = if (any(is.finite(.data$trait_gower_distance))) {
        sum(.data$.within_weight * .data$trait_gower_distance, na.rm = TRUE)
      } else {
        NA_real_
      },
      .study_n_models = dplyr::n()
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      slope_len = .data$.study_slope_len,
      intercept_len = .data$.study_intercept_len,
      combined_distance = dplyr::coalesce(.data$.study_combined_distance, .data$combined_distance),
      trait_gower_distance = dplyr::coalesce(.data$.study_trait_gower_distance, .data$trait_gower_distance),
      w_adm = .data$.study_group_raw_weight
    )
}

#' Resolve the donor rows used for a policy structural summary
#'
#' @param rows Candidate donor rows for one policy evaluation.
#' @param policy_def One policy-definition list.
#' @param summary_rows Optional precomputed method-specific donor summary rows.
#'
#' @return A tibble of donor rows carrying structural weights.
#'
#' @keywords internal
#' @noRd
policy_structural_rows <- function(rows,
                                   policy_def,
                                   summary_rows = NULL) {
  # Start from the valid donor equations once, then reuse any precomputed
  # method-specific summary rows so nearest-style policies do not resolve the
  # same winning donor twice.
  method_name <- as.character(policy_def$aggregation_method)[[1]]
  source_rows <- valid_equation_rows(rows)
  if (nrow(source_rows) == 0) {
    source_rows$.structural_weight <- numeric(0)
    return(source_rows)
  }

  if (method_name %in% c(
    "nearest_by_combined_distance",
    "nearest",
    "nearest_by_trait_gower_distance",
    "nearest_by_taxonomic_distance",
    "nearest_by_species_distance",
    "nearest_study_then_model"
  )) {
    out <- summary_rows %||% policy_summary_rows(source_rows, policy_def)
    out$.structural_weight <- 1
    return(out)
  }

  if (method_name %in% c("study_kernel_weighted_mean", "study_equal_weight_mean")) {
    source_rows <- source_rows |>
      dplyr::filter(is.finite(.data$w_adm), .data$w_adm > 0)
    group_cols <- study_group_columns(source_rows)
    if (nrow(source_rows) == 0 || length(group_cols) == 0) {
      source_rows$.structural_weight <- numeric(nrow(source_rows))
      return(source_rows)
    }

    return(
      source_rows |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::mutate(
          .study_group_raw_weight = dplyr::if_else(
            identical(method_name, "study_equal_weight_mean"),
            1,
            max(.data$w_adm, na.rm = TRUE)
          ),
          .within_raw_weight = .data$w_adm,
          .within_weight = .data$.within_raw_weight / sum(.data$.within_raw_weight, na.rm = TRUE),
          .structural_weight = .data$.study_group_raw_weight * .data$.within_weight
        ) |>
        dplyr::ungroup()
    )
  }

  # Equal-weight aggregation methods give every donor the same contribution to
  # the policy prediction, so the structural spread must also use equal weights.
  # Weighting by w_adm here would concentrate mass on high-admissibility donors
  # and artificially deflate structural_q for diverse unweighted pools.
  equal_weight_agg_methods <- c("arithmetic_mean", "equal_weight_mean", "source_cell_mean", "median")
  if (method_name %in% equal_weight_agg_methods) {
    source_rows$.structural_weight <- 1
  } else if ("w_adm" %in% names(source_rows) && any(is.finite(source_rows$w_adm) & source_rows$w_adm > 0)) {
    source_rows$.structural_weight <- dplyr::if_else(
      is.finite(source_rows$w_adm) & source_rows$w_adm > 0,
      source_rows$w_adm,
      NA_real_
    )
  } else {
    source_rows$.structural_weight <- 1
  }

  source_rows
}

#' Build a kernel-weighted study-level policy equation
#'
#' @param rows Candidate donor rows.
#'
#' @return One-row policy equation tibble.
#'
#' @keywords internal
#' @noRd
study_weighted_equation <- function(rows) {
  keep_rows <- study_ensemble_rows(rows, group_weight = "max", within_weight = "kernel") |>
    dplyr::filter(is.finite(.data$w_adm), .data$w_adm > 0)
  weights <- normalized_weights(keep_rows$w_adm)
  if (nrow(keep_rows) == 0 || length(weights) == 0) {
    return(build_policy_equation_row(NA_real_, NA_real_))
  }
  build_policy_equation_row(
    sum(weights * keep_rows$slope_len),
    sum(weights * keep_rows$intercept_len)
  )
}

#' Build an equal-weight study-level policy equation
#'
#' @param rows Candidate donor rows.
#'
#' @return One-row policy equation tibble.
#'
#' @keywords internal
#' @noRd
study_equal_equation <- function(rows) {
  keep_rows <- study_ensemble_rows(rows, group_weight = "max", within_weight = "kernel")
  if (nrow(keep_rows) == 0) {
    return(build_policy_equation_row(NA_real_, NA_real_))
  }
  build_policy_equation_row(
    mean(keep_rows$slope_len, na.rm = TRUE),
    mean(keep_rows$intercept_len, na.rm = TRUE)
  )
}

#' Select the nearest study, then the nearest model within that study
#'
#' @param rows Candidate donor rows.
#'
#' @return One-row policy equation tibble.
#'
#' @keywords internal
#' @noRd
nearest_study_then_model_equation <- function(rows) {
  keep_rows <- valid_equation_rows(rows)
  group_cols <- study_group_columns(keep_rows)
  if (nrow(keep_rows) == 0 || length(group_cols) == 0) {
    return(nearest_equation_by(
      candidate_rows = keep_rows,
      distance_column = "taxonomic_distance_to_anchor",
      distance_as_tiebreak = TRUE
    ))
  }
  study_rank <- keep_rows |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      study_distance = min(.data$combined_distance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$study_distance)
  if (nrow(study_rank) == 0 || !is.finite(study_rank$study_distance[[1]])) {
    return(build_policy_equation_row(NA_real_, NA_real_))
  }
  selected_study <- study_rank[1, group_cols, drop = FALSE]
  for (nm in group_cols) {
    selected_value <- selected_study[[nm]][[1]]
    keep_rows <- dplyr::filter(
      keep_rows,
      if (is.na(selected_value)) is.na(.data[[nm]]) else .data[[nm]] == selected_value
    )
  }
  nearest_equation_by(
    candidate_rows = keep_rows,
    distance_column = "taxonomic_distance_to_anchor",
    distance_as_tiebreak = TRUE
  )
}

#' Compute one arithmetic-mean equation
#'
#' @param rows Candidate-policy row table.
#'
#' @return One-row tibble.
#'
#' @keywords internal
#' @noRd
arithmetic_equation <- function(rows) {
  # Use the straight arithmetic mean of the standardized equation
  # coefficients for unweighted ensemble policies.
  keep_rows <- valid_equation_rows(rows)

  if (nrow(keep_rows) == 0) {
    return(build_policy_equation_row(NA_real_, NA_real_))
  }

  build_policy_equation_row(
    mean(keep_rows$slope_len, na.rm = TRUE),
    mean(keep_rows$intercept_len, na.rm = TRUE)
  )
}

#' Compute one median equation
#'
#' @param rows Candidate-policy row table.
#'
#' @return One-row tibble.
#'
#' @keywords internal
#' @noRd
median_equation <- function(rows) {
  # Use the componentwise median standardized coefficients to provide a robust
  # TS-equation ensemble. Biomass multipliers are computed only after this
  # equation has been constructed.
  keep_rows <- valid_equation_rows(rows)

  if (nrow(keep_rows) == 0) {
    return(build_policy_equation_row(NA_real_, NA_real_))
  }

  build_policy_equation_row(
    stats::median(keep_rows$slope_len, na.rm = TRUE),
    stats::median(keep_rows$intercept_len, na.rm = TRUE)
  )
}

#' Resolve the rows actually used by one policy aggregator
#'
#' @param rows Candidate-policy row table.
#' @param policy_def One policy-definition list.
#'
#' @return A tibble.
#' @keywords internal
#' @noRd
policy_summary_rows <- function(rows,
                                policy_def) {
  method_name <- as.character(policy_def$aggregation_method)[[1]]
  keep_rows <- valid_equation_rows(rows)

  if (method_name %in% c("nearest_by_combined_distance", "nearest")) {
    if (nrow(keep_rows) == 0) {
      return(keep_rows)
    }
    if ("taxonomic_distance_to_anchor" %in% names(keep_rows)) {
      ord <- order(keep_rows$combined_distance, keep_rows$taxonomic_distance_to_anchor, na.last = TRUE)
    } else {
      ord <- order(keep_rows$combined_distance, na.last = TRUE)
    }
    return(keep_rows[ord[1], , drop = FALSE])
  }

  if (identical(method_name, "nearest_by_trait_gower_distance")) {
    if (nrow(keep_rows) == 0) {
      return(keep_rows)
    }
    if ("trait_gower_distance" %in% names(keep_rows)) {
      ok <- is.finite(keep_rows$trait_gower_distance)
      sub <- if (any(ok)) keep_rows[ok, , drop = FALSE] else keep_rows
      ord <- order(sub$trait_gower_distance, sub$combined_distance, na.last = TRUE)
    } else {
      sub <- keep_rows
      ord <- order(sub$combined_distance, na.last = TRUE)
    }
    return(sub[ord[1], , drop = FALSE])
  }
  if (identical(method_name, "nearest_by_taxonomic_distance")) {
    if (nrow(keep_rows) == 0) {
      return(keep_rows)
    }
    if ("taxonomic_distance_to_anchor" %in% names(keep_rows)) {
      ok <- is.finite(keep_rows$taxonomic_distance_to_anchor)
      sub <- if (any(ok)) keep_rows[ok, , drop = FALSE] else keep_rows
      ord <- order(sub$taxonomic_distance_to_anchor, sub$combined_distance, na.last = TRUE)
    } else {
      sub <- keep_rows
      ord <- order(sub$combined_distance, na.last = TRUE)
    }
    return(sub[ord[1], , drop = FALSE])
  }
  if (identical(method_name, "nearest_by_species_distance")) {
    if (nrow(keep_rows) == 0) {
      return(keep_rows)
    }
    if ("d_species" %in% names(keep_rows)) {
      ok <- is.finite(keep_rows$d_species)
      sub <- if (any(ok)) keep_rows[ok, , drop = FALSE] else keep_rows
      ord <- order(sub$d_species, sub$combined_distance, na.last = TRUE)
    } else {
      sub <- keep_rows
      ord <- order(sub$combined_distance, na.last = TRUE)
    }
    return(sub[ord[1], , drop = FALSE])
  }
  if (identical(method_name, "nearest_study_then_model")) {
    group_cols <- study_group_columns(keep_rows)
    if (nrow(keep_rows) == 0 || length(group_cols) == 0) {
      if (nrow(keep_rows) == 0) {
        return(keep_rows)
      }
      ord <- order(keep_rows$combined_distance, na.last = TRUE)
      return(keep_rows[ord[1], , drop = FALSE])
    }
    study_rank <- keep_rows |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(study_distance = min(.data$combined_distance, na.rm = TRUE), .groups = "drop")
    study_rank <- study_rank[order(study_rank$study_distance, na.last = TRUE), , drop = FALSE]
    if (nrow(study_rank) == 0) {
      return(keep_rows[0, , drop = FALSE])
    }
    selected_study <- study_rank[1, group_cols, drop = FALSE]
    for (nm in group_cols) {
      selected_value <- selected_study[[nm]][[1]]
      keep_rows <- if (is.na(selected_value)) {
        keep_rows[is.na(keep_rows[[nm]]), , drop = FALSE]
      } else {
        keep_rows[!is.na(keep_rows[[nm]]) & keep_rows[[nm]] == selected_value, , drop = FALSE]
      }
    }
    ord <- order(keep_rows$combined_distance, na.last = TRUE)
    return(keep_rows[ord[1], , drop = FALSE])
  }
  if (method_name %in% c("study_kernel_weighted_mean", "study_equal_weight_mean")) {
    return(study_ensemble_rows(keep_rows, group_weight = "max", within_weight = "kernel"))
  }

  keep_rows
}

#' Summarize one policy donor pool
#'
#' @param rows Candidate-policy row table.
#' @param policy_def One policy-definition list.
#' @param summary_rows Optional precomputed output from
#'   `policy_summary_rows()`. When supplied, the support summary reuses those
#'   rows instead of recomputing the method-specific donor subset.
#'
#' @return A one-row tibble of local support diagnostics.
#' @keywords internal
#' @noRd
policy_support_summary <- function(rows,
                                   policy_def,
                                   summary_rows = NULL,
                                   structural_rows = NULL) {
  # These diagnostics expose how local the selected donor pool is for the
  # current anchor. Their weights must match the policy estimate itself:
  # using admissibility weights for an equal-weight mean hides distant donors
  # that materially contribute to the resulting equation.
  summary_rows <- summary_rows %||% policy_summary_rows(rows, policy_def)
  keep_rows <- structural_rows %||% policy_structural_rows(
    rows,
    policy_def,
    summary_rows = summary_rows
  )

  finite_min <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (length(x) == 0) NA_real_ else min(x)
  }
  finite_median <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (length(x) == 0) NA_real_ else stats::median(x)
  }
  weighted_mean_col <- function(value_col) {
    if (!value_col %in% names(keep_rows)) {
      return(NA_real_)
    }
    x <- suppressWarnings(as.numeric(keep_rows[[value_col]]))
    if (".structural_weight" %in% names(keep_rows)) {
      w <- suppressWarnings(as.numeric(keep_rows$.structural_weight))
    } else {
      w <- rep(1, length(x))
    }
    ok <- is.finite(x) & is.finite(w) & w > 0
    if (!any(ok)) {
      return(NA_real_)
    }
    stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
  }
  weighted_q_col <- function(value_col, prob = 0.90) {
    if (!value_col %in% names(keep_rows)) {
      return(NA_real_)
    }
    x <- suppressWarnings(as.numeric(keep_rows[[value_col]]))
    if (".structural_weight" %in% names(keep_rows)) {
      w <- suppressWarnings(as.numeric(keep_rows$.structural_weight))
    } else {
      w <- rep(1, length(x))
    }
    q <- weighted_quantile(x, w, probs = prob)
    if (length(q) == 0L) NA_real_ else as.numeric(q[[1]])
  }

  weights <- if (".structural_weight" %in% names(keep_rows)) {
    normalized_weights(keep_rows$.structural_weight)
  } else {
    normalized_weights(rep(1, nrow(keep_rows)))
  }
  donor_id_col <- if ("model_id" %in% names(keep_rows)) {
    "model_id"
  } else if ("model_id" %in% names(keep_rows)) {
    "model_id"
  } else {
    NA_character_
  }
  donor_fingerprint <- if (is.na(donor_id_col)) {
    NA_character_
  } else {
    donor_ids <- sort(unique(as.character(keep_rows[[donor_id_col]])))
    donor_ids <- donor_ids[!is.na(donor_ids) & nzchar(donor_ids)]
    if (length(donor_ids) == 0L) NA_character_ else paste(donor_ids, collapse = "|")
  }
  effective_n <- if (length(weights) == 0) {
    NA_real_
  } else {
    1 / sum(weights^2)
  }
  # Repeated equations from one species are not independent biological support.
  # Preserve model-level support above, and expose the species-collapsed value
  # separately for the anchor-equivalence burden tie-breaker.
  species_col <- c("species", "species_name", "scientific_name")
  species_col <- species_col[species_col %in% names(keep_rows)]
  species_col <- if (length(species_col) == 0L) NA_character_ else species_col[[1]]
  effective_species_n <- if (is.na(species_col) || length(weights) == 0L) {
    NA_real_
  } else {
    species_id <- as.character(keep_rows[[species_col]])
    species_id[is.na(species_id) | !nzchar(species_id)] <- "<missing-species>"
    species_weight <- rowsum(weights, group = species_id, reorder = FALSE)[, 1]
    species_weight <- normalized_weights(species_weight)
    if (length(species_weight) == 0L) NA_real_ else 1 / sum(species_weight^2)
  }

  slope20 <- slope20_indicator(keep_rows)
  base_summary <- list(
    n_valid_models = as.integer(nrow(keep_rows)),
    local_min_combined_distance = if ("combined_distance" %in% names(keep_rows)) finite_min(keep_rows$combined_distance) else NA_real_,
    local_median_combined_distance = if ("combined_distance" %in% names(keep_rows)) finite_median(keep_rows$combined_distance) else NA_real_,
    local_weighted_mean_combined_distance = weighted_mean_col("combined_distance"),
    local_weighted_q90_combined_distance = weighted_q_col("combined_distance"),
    local_weighted_mean_learned_distance_disagreement = weighted_mean_col("learned_distance_disagreement"),
    local_max_learned_distance_disagreement = if ("learned_distance_disagreement" %in% names(keep_rows)) {
      values <- suppressWarnings(as.numeric(keep_rows$learned_distance_disagreement))
      values <- values[is.finite(values)]
      if (length(values) == 0L) NA_real_ else max(values)
    } else {
      NA_real_
    },
    learned_distance_diagnostic_available = if ("learned_distance_diagnostic_available" %in% names(keep_rows)) {
      all(as.logical(keep_rows$learned_distance_diagnostic_available) %in% TRUE)
    } else {
      FALSE
    },
    local_min_trait_gower_distance = if ("trait_gower_distance" %in% names(keep_rows)) finite_min(keep_rows$trait_gower_distance) else NA_real_,
    local_weighted_mean_trait_gower_distance = weighted_mean_col("trait_gower_distance"),
    local_weighted_q90_trait_gower_distance = weighted_q_col("trait_gower_distance"),
    local_min_taxonomic_distance = if ("taxonomic_distance_to_anchor" %in% names(keep_rows)) finite_min(keep_rows$taxonomic_distance_to_anchor) else NA_real_,
    local_weighted_mean_taxonomic_distance = weighted_mean_col("taxonomic_distance_to_anchor"),
    local_weighted_q90_taxonomic_distance = weighted_q_col("taxonomic_distance_to_anchor"),
    local_min_species_distance = if ("d_species" %in% names(keep_rows)) finite_min(keep_rows$d_species) else NA_real_,
    local_weighted_mean_species_distance = weighted_mean_col("d_species"),
    local_mean_length_overlap = weighted_mean_col("length_overlap_fraction"),
    local_mean_depth_overlap = weighted_mean_col("depth_overlap_fraction"),
    local_effective_support = effective_n,
    local_effective_species_support = effective_species_n,
    local_max_weight = if (length(weights) == 0) NA_real_ else max(weights, na.rm = TRUE),
    realized_n_unique_donors = if (is.na(donor_id_col)) NA_integer_ else as.integer(length(unique(as.character(keep_rows[[donor_id_col]])))),
    realized_donor_fingerprint = donor_fingerprint
  )
  overlap_cols <- grep("^overlap_", names(keep_rows), value = TRUE)
  overlap_cols <- overlap_cols[!endsWith(overlap_cols, "_type")]
  if (length(overlap_cols) > 0) {
    overlap_counts <- lapply(overlap_cols, function(col) {
      as.integer(sum(keep_rows[[col]], na.rm = TRUE))
    })
    names(overlap_counts) <- paste0("local_n_", sub("^overlap_", "", overlap_cols))
    base_summary <- c(base_summary, overlap_counts)
  }
  if ("slope_len" %in% names(keep_rows)) {
    base_summary$local_n_slope20 <- as.integer(sum(slope20, na.rm = TRUE))
    base_summary$local_n_non_slope20 <- as.integer(sum(!slope20, na.rm = TRUE))
  }
  base_summary
}

#' Summarize structural spread in one policy equation
#'
#' @param rows Candidate-policy row table.
#' @param policy_def One policy-definition list.
#' @param pred One-row policy prediction from `policy_prediction()`.
#' @param anchor_pdf Anchor length-density table.
#' @param structural_rows Optional precomputed output from
#'   `policy_structural_rows()`. When supplied, the structural summary reuses
#'   those rows instead of resolving the donor subset again.
#'
#' @return A one-row tibble of equation-dispersion diagnostics.
#' @keywords internal
#' @noRd
policy_structural_summary <- function(rows,
                                      policy_def,
                                      pred,
                                      anchor_pdf,
                                      structural_rows = NULL) {
  # These diagnostics quantify disagreement among the donor equations that
  # actually enter a policy. They are intentionally computed after constructing
  # the policy equation so ensemble policies carry local structural uncertainty
  # rather than appearing precise only because coefficients were averaged.
  keep_rows <- structural_rows %||% policy_structural_rows(rows, policy_def)
  method_name <- as.character(policy_def$aggregation_method)[[1]]
  n_valid <- nrow(keep_rows)
  pred_scalar <- function(name) {
    if (!name %in% names(pred)) {
      return(NA_real_)
    }
    value <- suppressWarnings(as.numeric(pred[[name]][[1]]))
    if (length(value) == 0) {
      return(NA_real_)
    }
    value[[1]]
  }

  empty <- tibble::tibble(
    policy_is_constructed_ensemble = !method_name %in% c(
      "nearest_by_combined_distance",
      "nearest",
      "nearest_by_trait_gower_distance",
      "nearest_by_taxonomic_distance",
      "nearest_by_species_distance",
      "nearest_study_then_model"
    ),
    donor_slope_sd = NA_real_,
    donor_intercept_sd = NA_real_,
    donor_slope_iqr = NA_real_,
    donor_intercept_iqr = NA_real_,
    donor_log_multiplier_abs_dev_median = NA_real_,
    donor_log_multiplier_abs_dev_q90 = NA_real_,
    donor_log_sigma_abs_dev_median = NA_real_,
    donor_log_sigma_abs_dev_q90 = NA_real_,
    donor_curve_rmse_median = NA_real_,
    donor_curve_rmse_q90 = NA_real_,
    local_structural_q_abs_log = NA_real_
  )
  policy_slope <- pred_scalar("policy_slope_len")
  policy_intercept <- pred_scalar("policy_intercept_len")
  if (n_valid == 0 ||
    !is.finite(policy_slope) ||
    !is.finite(policy_intercept)) {
    return(empty)
  }
  weights <- if (".structural_weight" %in% names(keep_rows)) {
    normalized_weights(keep_rows$.structural_weight)
  } else if ("w_adm" %in% names(keep_rows)) {
    normalized_weights(keep_rows$w_adm)
  } else {
    rep(1 / n_valid, n_valid)
  }
  if (length(weights) == 0) {
    weights <- rep(1 / n_valid, n_valid)
  }

  finite_sd0 <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (length(x) <= 1) {
      return(0)
    }
    stats::sd(x)
  }
  finite_iqr0 <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (length(x) <= 1) {
      return(0)
    }
    stats::IQR(x, na.rm = TRUE, type = 8)
  }

  donor_sigma <- vapply(
    seq_len(n_valid),
    function(j) equation_sigma_mean(keep_rows$slope_len[[j]], keep_rows$intercept_len[[j]], anchor_pdf),
    numeric(1)
  )
  policy_sigma <- pred_scalar("policy_sigma_bs_mean")
  log_sigma_abs_dev <- if (is.finite(policy_sigma) && policy_sigma > 0) {
    abs(log(donor_sigma) - log(policy_sigma))
  } else {
    rep(NA_real_, n_valid)
  }
  # Deviation of each realized donor's biomass prediction from the policy
  # equation on the anchor length distribution. This is the kappa term in the
  # methods draft: donor disagreement around the selected strategy, not a
  # separate diagnostic against the anchor truth.
  log_multiplier_abs_dev <- log_sigma_abs_dev

  curve_rmse <- vapply(
    seq_len(n_valid),
    function(j) {
      s <- suppressWarnings(as.numeric(keep_rows$slope_len[[j]]))
      b <- suppressWarnings(as.numeric(keep_rows$intercept_len[[j]]))
      if (!is.finite(s) || !is.finite(b) ||
        is.null(anchor_pdf) ||
        !all(c("length_cm", "f_len") %in% names(anchor_pdf))) {
        return(NA_real_)
      }
      len <- as.numeric(anchor_pdf$length_cm)
      pdf_w <- as.numeric(anchor_pdf$f_len)
      ok <- is.finite(len) & len > 0 & is.finite(pdf_w) & pdf_w >= 0
      if (!any(ok) || sum(pdf_w[ok]) <= 0) {
        return(NA_real_)
      }
      donor_ts <- s * log10(len[ok]) + b
      policy_ts <- policy_slope * log10(len[ok]) + policy_intercept
      w <- pdf_w[ok] / sum(pdf_w[ok])
      sqrt(sum(w * (donor_ts - policy_ts)^2))
    },
    numeric(1)
  )

  log_multiplier_q <- weighted_quantile(log_multiplier_abs_dev, weights, probs = c(0.50, 0.90))
  log_sigma_q <- weighted_quantile(log_sigma_abs_dev, weights, probs = c(0.50, 0.90))
  curve_q <- weighted_quantile(curve_rmse, weights, probs = c(0.50, 0.90))
  # structural_q = 90th-percentile of |log(biomass_multiplier_if_replace)| weighted by
  # donor contributions. Deviation of each donor from the anchor's own truth (ref = 1.0).
  # Do NOT RSS with log_sigma_q - biomass_multiplier_if_replace = sigma_donor/sigma_anchor
  # already captures the same sigma deviation in log space; combining would double-count.
  structural_q <- if (is.finite(log_multiplier_q[[2]])) {
    log_multiplier_q[[2]]
  } else if (is.finite(log_sigma_q[[2]])) {
    log_sigma_q[[2]]
  } else if (is.finite(curve_q[[2]])) {
    (curve_q[[2]] * log(10)) / 10
  } else {
    NA_real_
  }

  tibble::tibble(
    policy_is_constructed_ensemble = !method_name %in% c(
      "nearest_by_combined_distance",
      "nearest",
      "nearest_by_trait_gower_distance",
      "nearest_study_then_model"
    ),
    donor_slope_sd = finite_sd0(keep_rows$slope_len),
    donor_intercept_sd = finite_sd0(keep_rows$intercept_len),
    donor_slope_iqr = finite_iqr0(keep_rows$slope_len),
    donor_intercept_iqr = finite_iqr0(keep_rows$intercept_len),
    donor_log_multiplier_abs_dev_median = log_multiplier_q[[1]],
    donor_log_multiplier_abs_dev_q90 = log_multiplier_q[[2]],
    donor_log_sigma_abs_dev_median = log_sigma_q[[1]],
    donor_log_sigma_abs_dev_q90 = log_sigma_q[[2]],
    donor_curve_rmse_median = curve_q[[1]],
    donor_curve_rmse_q90 = curve_q[[2]],
    local_structural_q_abs_log = structural_q
  )
}

#' Compute one equal-weight equation
#'
#' @param rows Candidate-policy row table.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
equal_equation <- function(rows) {
  # Equal-weight mean is distinct in the registry even when its equation
  # summary matches the arithmetic mean.
  arithmetic_equation(rows)
}

#' Compute a bootstrap-averaged random single-model selection
#'
#' Draws `n_draws` models uniformly at random (with replacement) from the
#' valid candidate pool and returns the mean slope/intercept across draws.
#' This provides a Monte Carlo estimate of what a purely random policy would
#' achieve, averaged over the bootstrap distribution.
#'
#' @param rows Candidate-policy row table.
#' @param n_draws Integer. Number of bootstrap draws (default 100).
#' @param seed Integer seed for reproducibility.
#'
#' @return One-row tibble.
#'
#' @keywords internal
#' @noRd
random_draw_equation <- function(rows, n_draws = 100L, seed = NULL) {
  keep_rows <- valid_equation_rows(rows)
  if (nrow(keep_rows) == 0L) {
    return(build_policy_equation_row(NA_real_, NA_real_))
  }
  n_draws <- as.integer(n_draws)
  if (!is.null(seed)) set.seed(seed)
  idx <- sample.int(nrow(keep_rows), size = n_draws, replace = TRUE)
  build_policy_equation_row(
    mean(keep_rows$slope_len[idx], na.rm = TRUE),
    mean(keep_rows$intercept_len[idx], na.rm = TRUE)
  )
}

#' Compute TS at length
#'
#' @param slope Numeric slope value.
#' @param intercept Numeric intercept value.
#' @param lengths_cm Numeric length vector in centimeters.
#'
#' @return Numeric vector of TS values.
#'
#' @keywords internal
#' @noRd
ts_at_length <- function(slope,
                         intercept,
                         lengths_cm) {
  # Evaluate the standardized TS-length model directly in log10-length space.
  as.numeric(slope) * log10(lengths_cm) + as.numeric(intercept)
}

#' Compute mean backscatter for an equation
#'
#' @param slope Numeric slope.
#' @param intercept Numeric intercept.
#' @param anchor_pdf Anchor length-density table.
#'
#' @return Numeric scalar.
#'
#' @keywords internal
#' @noRd
equation_sigma_mean <- function(slope,
                                intercept,
                                anchor_pdf,
                                anchor_pdf_precomp = NULL) {
  if (!is.finite(slope) || !is.finite(intercept)) {
    return(NA_real_)
  }
  if (!is.null(anchor_pdf_precomp)) {
    phi <- 10^((slope * anchor_pdf_precomp$log10_len + intercept) / 10)
    return(sum(phi * anchor_pdf_precomp$f_norm, na.rm = TRUE))
  }
  if (is.null(anchor_pdf) ||
    !all(c("length_cm", "f_len") %in% names(anchor_pdf))) {
    return(NA_real_)
  }
  ts_vec <- ts_at_length(slope, intercept, anchor_pdf$length_cm)
  phi <- 10^(ts_vec / 10)
  sum(phi * anchor_pdf$f_len, na.rm = TRUE) / sum(anchor_pdf$f_len, na.rm = TRUE)
}

#' Compute biomass multiplier for an equation
#'
#' @param slope Numeric slope.
#' @param intercept Numeric intercept.
#' @param anchor_sigma Anchor mean backscatter.
#' @param anchor_pdf Anchor length-density table.
#'
#' @return One-row tibble.
#'
#' @keywords internal
#' @noRd
equation_multiplier <- function(slope,
                                intercept,
                                anchor_sigma,
                                anchor_pdf,
                                anchor_pdf_precomp = NULL) {
  sigma_mean <- equation_sigma_mean(slope, intercept, anchor_pdf, anchor_pdf_precomp)
  multiplier <- if (is.finite(anchor_sigma) && anchor_sigma > 0 &&
    is.finite(sigma_mean) && sigma_mean > 0) {
    anchor_sigma / sigma_mean
  } else {
    NA_real_
  }

  list(
    policy_sigma_bs_mean = sigma_mean,
    multiplier_pred = multiplier
  )
}

#' Compute one policy equation
#'
#' @param rows Candidate-policy row table.
#' @param policy_def One policy-definition list.
#'
#' @return One-row tibble.
#'
#' @keywords internal
#' @noRd
policy_equation <- function(rows,
                            policy_def) {
  # Dispatch to the requested aggregation method after the donor pool has
  # already been filtered for the selected policy. This function returns a
  # standardized TS-length equation, not a biomass multiplier.
  method_name <- as.character(policy_def$aggregation_method)[[1]]

  if (identical(method_name, "nearest_by_combined_distance")) {
    return(nearest_equation_by(
      candidate_rows = rows,
      distance_column = "taxonomic_distance_to_anchor",
      distance_as_tiebreak = TRUE
    ))
  }
  if (identical(method_name, "nearest")) {
    return(nearest_equation_by(
      candidate_rows = rows,
      distance_column = "taxonomic_distance_to_anchor",
      distance_as_tiebreak = TRUE
    ))
  }
  if (identical(method_name, "nearest_by_trait_gower_distance")) {
    return(nearest_equation_by(candidate_rows = rows, distance_column = "trait_gower_distance"))
  }
  if (identical(method_name, "nearest_by_taxonomic_distance")) {
    return(nearest_equation_by(candidate_rows = rows, distance_column = "taxonomic_distance_to_anchor"))
  }
  if (identical(method_name, "nearest_by_species_distance")) {
    return(nearest_equation_by(candidate_rows = rows, distance_column = "d_species"))
  }
  if (identical(method_name, "nearest_study_then_model")) {
    return(nearest_study_then_model_equation(rows))
  }
  if (identical(method_name, "kernel_weighted_mean")) {
    return(weighted_equation(rows))
  }
  if (identical(method_name, "distance_weighted_mean")) {
    return(weighted_equation(rows))
  }
  if (identical(method_name, "study_kernel_weighted_mean")) {
    return(study_weighted_equation(rows))
  }
  if (identical(method_name, "study_equal_weight_mean")) {
    return(study_equal_equation(rows))
  }
  if (identical(method_name, "arithmetic_mean")) {
    return(arithmetic_equation(rows))
  }
  if (identical(method_name, "median")) {
    return(median_equation(rows))
  }
  if (identical(method_name, "equal_weight_mean")) {
    return(equal_equation(rows))
  }
  if (identical(method_name, "source_cell_mean")) {
    return(equal_equation(rows))
  }
  if (identical(method_name, "random_draw")) {
    n_draws <- as.integer(policy_def$n_random_draws %||% 100L)
    return(random_draw_equation(rows, n_draws = n_draws))
  }

  stop(sprintf("Unsupported aggregation method: %s", method_name), call. = FALSE)
}

#' Compute one policy prediction
#'
#' @param rows Candidate-policy row table.
#' @param policy_def One policy-definition list.
#' @param anchor_sigma Anchor mean backscatter.
#' @param anchor_pdf Anchor length-density table.
#' @param anchor_pdf_precomp Optional precomputed anchor-PDF terms reused across
#'   policies for the same anchor.
#' @param equation_row Optional precomputed equation row from
#'   `policy_equation()`. When supplied, the prediction reuses that equation
#'   instead of recomputing the aggregator output.
#'
#' @return One-row tibble.
#'
#' @keywords internal
#' @noRd
policy_prediction <- function(rows,
                              policy_def,
                              anchor_sigma,
                              anchor_pdf,
                              anchor_pdf_precomp = NULL,
                              equation_row = NULL) {
  # Reuse a precomputed policy equation when the caller already needed that
  # aggregation output for adjacent diagnostics in the same benchmark pass.
  eq <- equation_row %||% policy_equation(rows, policy_def)
  c(eq, equation_multiplier(
    slope = eq$policy_slope_len[[1]],
    intercept = eq$policy_intercept_len[[1]],
    anchor_sigma = anchor_sigma,
    anchor_pdf = anchor_pdf,
    anchor_pdf_precomp = anchor_pdf_precomp
  ))
}

#' Compute one policy curve
#'
#' @param rows Candidate-policy row table.
#' @param policy_def One policy-definition list.
#' @param lengths_cm Numeric length vector in centimeters.
#'
#' @return Numeric vector.
#'
#' @keywords internal
#' @noRd
build_policy_curve <- function(rows,
                               policy_def,
                               lengths_cm) {
  # Predict the curve from the selected/aggregated standardized TS equation so
  # curve outputs and biomass-multiplier outputs represent the same policy.
  eq <- policy_equation(rows, policy_def)
  if (!is.finite(eq$policy_slope_len[[1]]) || !is.finite(eq$policy_intercept_len[[1]])) {
    return(rep(NA_real_, length(lengths_cm)))
  }

  ts_at_length(eq$policy_slope_len[[1]], eq$policy_intercept_len[[1]], lengths_cm)
}

#' Build a policy execution plan
#'
#' @param policies Optional character vector of policy names. `NULL` means use
#'   all policies from the registry.
#' @param policy_params Optional named list of per-policy parameter overrides.
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return A list with a `plan` tibble plus shared registry metadata.
#'
#' @keywords internal
#' @noRd
build_policy_execution_plan <- function(policies = NULL,
                                        policy_params = list(),
                                        policy_path = NULL) {
  # Resolve the active policy set, per-policy parameters, and branch-filter
  # expansions once so anchor-level evaluation can focus only on donor rows.
  policy_lookup <- policy_lookup_table(policy_path = policy_path)
  selected <- stringr::str_squish(as.character(unlist(policies %||% names(policy_lookup), use.names = FALSE)))
  selected <- selected[!is.na(selected) & nzchar(selected)]

  if (!is.character(selected) || any(!nzchar(selected))) {
    stop("'policies' must be NULL or a character vector of policy names.", call. = FALSE)
  }

  shared_registry <- read_policy_registry(policy_path = policy_path)
  policy_defs <- stats::setNames(
    lapply(selected, function(policy_name) {
      policy_lookup[[policy_name]] %||%
        build_policy_definition_from_name(policy_name, registry = shared_registry) %||%
        NULL
    }),
    selected
  )
  unknown <- names(policy_defs)[!vapply(policy_defs, is.list, logical(1))]
  if (length(unknown) > 0) {
    stop(sprintf("Unknown policy name(s): %s", paste(unknown, collapse = ", ")), call. = FALSE)
  }

  default_equation_branch_filters <- normalize_policy_equation_branch_filters(
    policy_params$slope_class %||% NULL
  )
  branch_defs <- shared_registry$policy_branches %||% list()
  branch_display_tags <- stats::setNames(
    vapply(branch_defs, function(x) as.character(x$display_tag %||% x$key %||% NA_character_), character(1)),
    vapply(branch_defs, function(x) as.character(x$key %||% NA_character_), character(1))
  )

  plan_rows <- list()
  row_id <- 1L
  for (policy_name in selected) {
    # Expand each policy to one row per requested branch filter so the plan
    # captures exactly what anchor-level evaluation must execute.
    policy_def <- policy_defs[[policy_name]]
    params <- policy_parameters(policy_name, policy_def, policy_params = policy_params)
    equation_branch_filters_now <- normalize_policy_equation_branch_filters(
      params$slope_class %||%
        default_equation_branch_filters
    )
    canonical_name <- as.character(policy_def$coded_name %||% policy_name)[[1]]
    display_name <- as.character(policy_def$display_name %||% NA_character_)[[1]]
    if (is.na(display_name) || !nzchar(display_name)) {
      display_name <- snake_title(canonical_name)
    }
    match_traits <- as.character(unlist(params$match_traits %||% character(0), use.names = FALSE))
    match_traits <- match_traits[!is.na(match_traits) & nzchar(match_traits)]
    match_traits_sig <- paste(match_traits, collapse = ",")

    for (branch_filter in equation_branch_filters_now) {
      branch_display <- unname(branch_display_tags[branch_filter] %||% branch_filter)
      if (is.na(branch_display) || !nzchar(branch_display)) {
        branch_display <- branch_filter
      }

      plan_rows[[row_id]] <- tibble::tibble(
        requested_policy = policy_name,
        policy = canonical_name,
        policy_base = canonical_name,
        display_name = display_name,
        policy_display = paste0(display_name, " [", branch_display, "]"),
        equation_branch_filter = branch_filter,
        policy_family = as.character(policy_def$policy_family %||% NA_character_)[[1]],
        candidate_pool = as.character(policy_def$candidate_pool)[[1]],
        aggregation_method = as.character(policy_def$aggregation_method)[[1]],
        candidate_pool_definition = as.character(policy_def$candidate_pool_definition %||% NA_character_)[[1]],
        aggregation_definition = as.character(policy_def$aggregation_definition %||% NA_character_)[[1]],
        plain_language_definition = as.character(policy_def$plain_language_definition %||% NA_character_)[[1]],
        phylo_radius = suppressWarnings(as.numeric(params$phylo_radius %||% NA_real_)),
        match_traits_signature = match_traits_sig,
        donor_cache_key = paste(
          as.character(policy_def$candidate_pool)[[1]],
          branch_filter,
          as.character(params$phylo_radius %||% NA_real_),
          match_traits_sig,
          sep = "|"
        ),
        policy_def = list(policy_def),
        policy_params = list(params)
      )
      row_id <- row_id + 1L
    }
  }

  list(
    plan = dplyr::bind_rows(plan_rows),
    policy_defs = policy_defs,
    branch_display_tags = branch_display_tags
  )
}

#' Normalize the policy benchmark execution engine
#'
#' @param engine Requested engine name.
#'
#' @return `"r"` or `"cpp"`.
#'
#' @keywords internal
#' @noRd
normalize_policy_benchmark_engine <- function(engine = "r") {
  match.arg(as.character(engine %||% "r")[[1]], c("r", "cpp"))
}

#' Compile a policy execution plan for the C++ benchmark engine
#'
#' @param execution_plan Plan returned by the policy execution-plan builder.
#'
#' @return A serialization-safe compiled-plan list. The C++ payload contains
#'   atomic vectors only; `pool_specs` remains an R-side mask recipe.
#'
#' @keywords internal
#' @noRd
compile_policy_execution_plan_cpp <- function(execution_plan) {
  plan_tbl <- tibble::as_tibble(execution_plan$plan %||% tibble::tibble())
  if (nrow(plan_tbl) == 0L) {
    stop("Cannot compile an empty policy execution plan.", call. = FALSE)
  }

  aggregation_codes <- c(
    nearest_by_combined_distance = 1L,
    nearest_by_trait_gower_distance = 2L,
    nearest_by_taxonomic_distance = 3L,
    nearest_by_species_distance = 4L,
    kernel_weighted_mean = 5L,
    arithmetic_mean = 6L
  )
  methods <- as.character(plan_tbl$aggregation_method)
  unsupported <- setdiff(unique(methods), names(aggregation_codes))
  if (length(unsupported) > 0L) {
    stop(
      sprintf(
        "C++ policy benchmark does not yet support aggregation method(s): %s",
        paste(unsupported, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  pool_keys <- unique(as.character(plan_tbl$donor_cache_key))
  pool_id <- match(as.character(plan_tbl$donor_cache_key), pool_keys)
  pool_first <- match(pool_keys, as.character(plan_tbl$donor_cache_key))
  pool_specs <- lapply(pool_first, function(i) {
    list(
      key = as.character(plan_tbl$donor_cache_key[[i]]),
      branch_filter = as.character(plan_tbl$equation_branch_filter[[i]]),
      policy_def = plan_tbl$policy_def[[i]],
      policy_params = plan_tbl$policy_params[[i]]
    )
  })

  # Keep metadata outside the C++ payload. It is joined once after the kernel
  # returns instead of being copied through every donor computation.
  metadata_cols <- c(
    "policy", "policy_base", "policy_display", "equation_branch_filter",
    "policy_family", "candidate_pool", "aggregation_method",
    "candidate_pool_definition", "aggregation_definition",
    "plain_language_definition", "display_name", "requested_policy"
  )

  structure(
    list(
      schema_version = 1L,
      pool_keys = pool_keys,
      pool_specs = pool_specs,
      cpp = list(
        pool_id = as.integer(pool_id),
        aggregation_code = unname(as.integer(aggregation_codes[methods]))
      ),
      metadata = as.data.frame(
        plan_tbl[, intersect(metadata_cols, names(plan_tbl)), drop = FALSE],
        stringsAsFactors = FALSE
      )
    ),
    class = "tsb_compiled_policy_plan"
  )
}

#' Build unique donor-pool masks for one anchor
#'
#' @param eval_obj Anchor admissibility evaluation.
#' @param compiled_plan Compiled policy execution plan.
#' @param ordination_info Optional anchor ordination context.
#'
#' @return Logical matrix with one row per unique pool and one column per donor.
#'
#' @keywords internal
#' @noRd
policy_pool_masks_cpp <- function(eval_obj,
                                  compiled_plan,
                                  ordination_info = NULL) {
  donors <- tibble::as_tibble(eval_obj$admissible_df)
  donors$.cpp_donor_row <- seq_len(nrow(donors))
  masks <- matrix(
    FALSE,
    nrow = length(compiled_plan$pool_specs),
    ncol = nrow(donors)
  )

  branch_names <- unique(vapply(
    compiled_plan$pool_specs,
    function(x) x$branch_filter,
    character(1)
  ))
  branch_cache <- stats::setNames(
    lapply(branch_names, function(branch) policy_branch_filter(donors, branch)),
    branch_names
  )

  for (i in seq_along(compiled_plan$pool_specs)) {
    spec <- compiled_plan$pool_specs[[i]]
    selected <- policy_rows(
      rows = branch_cache[[spec$branch_filter]],
      policy_def = spec$policy_def,
      policy_params = spec$policy_params,
      ordination_info = resolve_policy_context(ordination_info)
    )
    selected_ids <- suppressWarnings(as.integer(selected$.cpp_donor_row))
    selected_ids <- selected_ids[is.finite(selected_ids)]
    masks[i, selected_ids] <- TRUE
  }
  masks
}

#' Extract the flat donor payload consumed by the C++ policy engine
#'
#' @param eval_obj Anchor admissibility evaluation.
#'
#' @return Named list of atomic donor vectors and overlap matrix.
#'
#' @keywords internal
#' @noRd
policy_donor_payload_cpp <- function(eval_obj) {
  donors <- policy_equation_aliases(tibble::as_tibble(eval_obj$admissible_df))
  n <- nrow(donors)
  numeric_col <- function(name, default = NA_real_) {
    if (name %in% names(donors)) {
      suppressWarnings(as.numeric(donors[[name]]))
    } else {
      rep(default, n)
    }
  }
  logical_col <- function(name, default = FALSE) {
    if (name %in% names(donors)) {
      as.logical(donors[[name]])
    } else {
      rep(default, n)
    }
  }
  character_col <- function(names_now) {
    matches <- intersect(names_now, names(donors))
    name <- if (length(matches) == 0L) NULL else matches[[1]]
    if (is.null(name)) rep(NA_character_, n) else as.character(donors[[name]])
  }

  overlap_cols <- grep("^overlap_", names(donors), value = TRUE)
  overlap_cols <- overlap_cols[!endsWith(overlap_cols, "_type")]
  overlap <- if (length(overlap_cols) == 0L) {
    matrix(FALSE, nrow = n, ncol = 0L)
  } else {
    out <- vapply(overlap_cols, function(name) {
      as.logical(donors[[name]]) %in% TRUE
    }, logical(n))
    if (is.null(dim(out))) matrix(out, nrow = n) else out
  }
  colnames(overlap) <- sub("^overlap_", "local_n_", overlap_cols)

  list(
    slope = numeric_col("slope_len"),
    intercept = numeric_col("intercept_len"),
    weight = numeric_col("w_adm"),
    combined_distance = numeric_col("combined_distance"),
    trait_distance = numeric_col("trait_gower_distance"),
    taxonomic_distance = numeric_col("taxonomic_distance_to_anchor"),
    species_distance = numeric_col("d_species"),
    learned_distance_disagreement = numeric_col("learned_distance_disagreement"),
    learned_distance_diagnostic_available = logical_col("learned_distance_diagnostic_available"),
    has_trait_distance = "trait_gower_distance" %in% names(donors),
    has_taxonomic_distance = "taxonomic_distance_to_anchor" %in% names(donors),
    has_species_distance = "d_species" %in% names(donors),
    has_learned_distance_diagnostic = "learned_distance_diagnostic_available" %in% names(donors),
    length_overlap = numeric_col("length_overlap_fraction"),
    depth_overlap = numeric_col("depth_overlap_fraction"),
    donor_multiplier = numeric_col("biomass_multiplier_if_replace"),
    donor_id = character_col(c("model_id", "model_id_chr")),
    donor_species = character_col(c("species", "species_name", "scientific_name")),
    overlap = overlap
  )
}

#' Evaluate a complete compiled policy plan for one anchor
#'
#' @param eval_obj Anchor admissibility evaluation.
#' @param compiled_plan Compiled plan.
#' @param ordination_info Optional ordination context.
#'
#' @return Policy prediction tibble matching the R policy-evaluation path.
#'
#' @keywords internal
#' @noRd
evaluate_policies_cpp <- function(eval_obj,
                                  compiled_plan,
                                  ordination_info = NULL) {
  if (!inherits(compiled_plan, "tsb_compiled_policy_plan")) {
    stop("'compiled_plan' must come from compile_policy_execution_plan_cpp().", call. = FALSE)
  }
  anchor_pdf <- eval_obj$anchor_pdf
  if (!is.null(anchor_pdf) && all(c("length_cm", "f_len") %in% names(anchor_pdf))) {
    length_cm <- suppressWarnings(as.numeric(anchor_pdf$length_cm))
    pdf_weight <- suppressWarnings(as.numeric(anchor_pdf$f_len))
  } else {
    length_cm <- numeric(0)
    pdf_weight <- numeric(0)
  }

  core <- cpp_evaluate_policy_plan(
    donors = policy_donor_payload_cpp(eval_obj),
    pool_masks = policy_pool_masks_cpp(eval_obj, compiled_plan, ordination_info),
    plan = compiled_plan$cpp,
    length_cm = length_cm,
    pdf_weight = pdf_weight,
    anchor_sigma = suppressWarnings(as.numeric(eval_obj$anchor_sigma %||% NA_real_))[[1]]
  )
  metadata <- tibble::as_tibble(compiled_plan$metadata)
  core <- tibble::as_tibble(core)
  if (nrow(core) != nrow(metadata)) {
    stop("C++ policy result row count did not match the compiled plan.", call. = FALSE)
  }
  dplyr::bind_cols(
    metadata,
    dplyr::rename(core, n_models = "n_models_pool")
  )
}

#' Dispatch one policy-plan evaluation to the selected engine
#'
#' @param engine `"r"` or `"cpp"`.
#' @param eval_obj Anchor admissibility evaluation.
#' @param ordination_info Optional ordination context.
#' @param policies,policy_params,policy_path R-engine policy inputs.
#' @param execution_plan R policy execution plan.
#' @param compiled_plan Optional compiled C++ plan.
#'
#' @return Policy prediction tibble.
#'
#' @keywords internal
#' @noRd
benchmark_evaluate_policies <- function(engine,
                                        eval_obj,
                                        ordination_info = NULL,
                                        policies = NULL,
                                        policy_params = list(),
                                        policy_path = NULL,
                                        execution_plan = NULL,
                                        compiled_plan = NULL) {
  engine <- normalize_policy_benchmark_engine(engine)
  if (identical(engine, "cpp")) {
    return(evaluate_policies_cpp(
      eval_obj = eval_obj,
      compiled_plan = compiled_plan,
      ordination_info = ordination_info
    ))
  }
  evaluate_policies(
    eval_obj = eval_obj,
    ordination_info = ordination_info,
    policies = policies,
    policy_params = policy_params,
    policy_path = policy_path,
    execution_plan = execution_plan
  )
}

#' Evaluate registered policies
#'
#' Evaluates one or more policies against the admissible candidate pool for one
#' anchor and returns benchmark-ready predictions.
#'
#' @param eval_obj Evaluation object returned by the internal single-anchor
#'   admissibility screen.
#' @param ordination_info Optional ordination-context list.
#' @param policies Optional character vector of policy names. `NULL` means use
#'   all policies from the registry.
#' @param policy_params Optional named list of per-policy parameter overrides.
#' @param policy_path Optional path to a policy registry JSON file.
#' @param execution_plan Optional prebuilt plan from
#'   `build_policy_execution_plan()`.
#'
#' @return A tibble with `policy`, `multiplier_pred`, and `n_models`.
#'
#' @keywords internal
#' @noRd
evaluate_policies <- function(eval_obj,
                              ordination_info = NULL,
                              policies = NULL,
                              policy_params = list(),
                              policy_path = NULL,
                              execution_plan = NULL) {
  # Validate the evaluation object once, then rely on a shared execution plan
  # so anchor-level evaluation avoids repeated registry and parameter work.
  if (!is.list(eval_obj) || is.null(eval_obj$admissible_df) || !is.data.frame(eval_obj$admissible_df)) {
    stop("'eval_obj' must contain an 'admissible_df' data frame.", call. = FALSE)
  }
  execution_plan <- execution_plan %||% build_policy_execution_plan(
    policies = policies,
    policy_params = policy_params,
    policy_path = policy_path
  )
  if (!is.list(execution_plan) || !is.data.frame(execution_plan$plan %||% NULL)) {
    stop("'execution_plan' must be NULL or a plan built by `build_policy_execution_plan()`.", call. = FALSE)
  }

  context <- resolve_policy_context(ordination_info = ordination_info)
  admissible_rows <- tibble::as_tibble(eval_obj$admissible_df)
  plan_tbl <- tibble::as_tibble(execution_plan$plan)
  equation_branch_filters <- unique(as.character(plan_tbl$equation_branch_filter))
  branch_cache <- stats::setNames(
    lapply(equation_branch_filters, function(branch_filter) {
      policy_branch_filter(admissible_rows, branch_filter)
    }),
    equation_branch_filters
  )
  donor_cache <- new.env(parent = emptyenv())

  anchor_pdf_precomp <- local({
    pdf <- eval_obj$anchor_pdf
    if (!is.null(pdf) && all(c("length_cm", "f_len") %in% names(pdf))) {
      len <- as.numeric(pdf$length_cm)
      f <- as.numeric(pdf$f_len)
      ok <- is.finite(len) & len > 0 & is.finite(f) & f >= 0
      if (any(ok) && sum(f[ok]) > 0) {
        list(log10_len = log10(len[ok]), f_norm = f[ok] / sum(f[ok]))
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  # Evaluate one planned policy-branch row at a time so anchor-level work only
  # resolves donor subsets and aggregators that the shared plan requested.
  rows_list <- vector("list", nrow(plan_tbl))
  for (pi in seq_len(nrow(plan_tbl))) {
    plan_row <- plan_tbl[pi, , drop = FALSE]
    policy_def <- plan_row$policy_def[[1]]
    params <- plan_row$policy_params[[1]]
    cache_key <- as.character(plan_row$donor_cache_key[[1]])
    branch_filter <- as.character(plan_row$equation_branch_filter[[1]])

    if (exists(cache_key, envir = donor_cache, inherits = FALSE)) {
      donor_rows <- get(cache_key, envir = donor_cache, inherits = FALSE)
    } else {
      donor_rows <- policy_rows(
        rows = branch_cache[[branch_filter]],
        policy_def = policy_def,
        policy_params = params,
        ordination_info = context
      )
      assign(cache_key, donor_rows, envir = donor_cache)
    }
    donor_rows_valid <- valid_equation_rows(donor_rows)

    # Compute the method-specific summary rows, structural rows, and equation
    # once so prediction and diagnostics do not re-scan the same donor pool.
    summary_rows <- policy_summary_rows(donor_rows_valid, policy_def)
    structural_rows <- policy_structural_rows(
      donor_rows_valid,
      policy_def,
      summary_rows = summary_rows
    )
    equation_row <- policy_equation(donor_rows_valid, policy_def)
    pred <- policy_prediction(
      donor_rows_valid,
      policy_def,
      anchor_sigma = eval_obj$anchor_sigma,
      anchor_pdf = eval_obj$anchor_pdf,
      anchor_pdf_precomp = anchor_pdf_precomp,
      equation_row = equation_row
    )

    rows_list[[pi]] <- c(
      list(
        policy = as.character(plan_row$policy[[1]]),
        policy_base = as.character(plan_row$policy_base[[1]]),
        policy_display = as.character(plan_row$policy_display[[1]]),
        equation_branch_filter = branch_filter,
        n_models = as.integer(nrow(donor_rows_valid)),
        policy_family = as.character(plan_row$policy_family[[1]]),
        candidate_pool = as.character(plan_row$candidate_pool[[1]]),
        aggregation_method = as.character(plan_row$aggregation_method[[1]]),
        candidate_pool_definition = as.character(plan_row$candidate_pool_definition[[1]]),
        aggregation_definition = as.character(plan_row$aggregation_definition[[1]]),
        plain_language_definition = as.character(plan_row$plain_language_definition[[1]]),
        display_name = as.character(plan_row$display_name[[1]]),
        requested_policy = as.character(plan_row$requested_policy[[1]])
      ),
      pred,
      policy_support_summary(
        donor_rows_valid,
        policy_def,
        summary_rows = summary_rows,
        structural_rows = structural_rows
      ),
      policy_structural_summary(
        rows = donor_rows_valid,
        policy_def = policy_def,
        pred = as.data.frame(pred),
        anchor_pdf = eval_obj$anchor_pdf,
        structural_rows = structural_rows
      )
    )
  }
  dplyr::bind_rows(rows_list)
}

#' Predict one policy TS curve
#'
#' Predicts one length-specific TS curve for a selected policy using the same
#' donor pool and aggregation rule as `evaluate_policies()`.
#'
#' @param policy Policy name.
#' @param eval_obj Evaluation object returned by the internal single-anchor
#'   admissibility screen.
#' @param lengths_cm Numeric length vector in centimeters.
#' @param equation_branch_filter Optional branch-filter label. When omitted,
#'   all donor equations are eligible.
#' @param ordination_info Optional ordination-context list.
#' @param policy_params Optional named list of per-policy parameter overrides.
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return Numeric vector.
#'
#' @keywords internal
#' @noRd
predict_policy_curve <- function(policy,
                                 eval_obj,
                                 lengths_cm,
                                 equation_branch_filter = "all",
                                 ordination_info = NULL,
                                 policy_params = list(),
                                 policy_path = NULL) {
  # Validate the requested policy first so any unsupported name fails before
  # touching the donor rows or the curve data.
  if (!is.character(policy) || length(policy) != 1 || !nzchar(policy)) {
    stop("'policy' must be a single policy name.", call. = FALSE)
  }
  if (!is.numeric(lengths_cm) || any(!is.finite(lengths_cm)) || any(lengths_cm <= 0)) {
    stop("'lengths_cm' must be a numeric vector of positive finite values.", call. = FALSE)
  }
  if (!is.list(eval_obj) || is.null(eval_obj$admissible_df) || !is.data.frame(eval_obj$admissible_df)) {
    stop("'eval_obj' must contain an 'admissible_df' data frame.", call. = FALSE)
  }

  policy_lookup <- policy_lookup_table(policy_path = policy_path)

  policy <- stringr::str_squish(as.character(policy))[[1]]
  policy_def <- policy_lookup[[policy]] %||%
    build_policy_definition_from_name(policy, policy_path = policy_path) %||%
    NULL
  if (!is.list(policy_def)) {
    stop(sprintf("Unknown policy name: %s", policy), call. = FALSE)
  }

  context <- resolve_policy_context(ordination_info = ordination_info)
  params <- policy_parameters(policy, policy_def, policy_params = policy_params)
  donor_rows <- policy_rows(
    rows = policy_branch_filter(eval_obj$admissible_df, equation_branch_filter),
    policy_def = policy_def,
    policy_params = params,
    ordination_info = context
  )

  build_policy_curve(donor_rows, policy_def, lengths_cm)
}


#' Resolve one benchmark field name
#'
#' @param config Benchmark config list.
#' @param key Field-map key to resolve.
#'
#' @return Character scalar.
#'
#' @keywords internal
#' @noRd
benchmark_field <- function(config,
                            key) {
  # Keep model-column lookup centralized so the benchmark layer can work with
  # remapped prepared-model tables.
  field_nm <- config$fields[[key]]

  if (!is.character(field_nm) || length(field_nm) != 1 || !nzchar(field_nm)) {
    stop(sprintf("Benchmark config field '%s' must be a single column name.", key), call. = FALSE)
  }

  field_nm
}

#' Read an optional benchmark field without substituting another column
#'
#' @param data Benchmark input table.
#' @param config Benchmark config list.
#' @param key Field-map key to resolve.
#'
#' @return Character vector with one value per input row. If the configured
#'   column is absent, every value is explicitly missing.
#'
#' @keywords internal
#' @noRd
benchmark_optional_field_values <- function(data,
                                            config,
                                            key) {
  data <- tibble::as_tibble(data)
  field_nm <- benchmark_field(config, key)
  if (!field_nm %in% names(data)) {
    return(rep(NA_character_, nrow(data)))
  }
  as.character(data[[field_nm]])
}

#' Normalize one reference-ID vector
#'
#' @param reference_ids Optional reference-model identifier vector.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
normalize_reference_ids <- function(reference_ids) {
  # Standardize the optional reference-model IDs once so benchmark annotations
  # do not depend on caller whitespace or duplicates.
  if (is.null(reference_ids)) {
    return(character(0))
  }

  ref_ids <- stringr::str_squish(as.character(reference_ids))
  unique(ref_ids[!is.na(ref_ids) & nzchar(ref_ids)])
}

#' Build one ordination-context object for benchmarking
#'
#' @param anchor_row One-row anchor table.
#' @param model_scores Optional model-score table from ordination output.
#' @param species_lookup Optional species lookup from ordination output.
#' @param config Benchmark config list.
#'
#' @return `NULL` or an ordination-context list.
#'
#' @keywords internal
#' @noRd
resolve_ordination_info <- function(anchor_row,
                                    model_scores,
                                    species_lookup,
                                    config) {
  # Only attempt the ordination-dependent policies when both the model-score table
  # and the species lookup are available.
  if (is.null(model_scores) || is.null(species_lookup)) {
    return(NULL)
  }

  build_anchor_ordination(
    anchor_row = anchor_row,
    model_scores = model_scores,
    species_lookup = species_lookup,
    anchor_id_col = benchmark_field(config, "model_id"),
    score_id_col = benchmark_field(config, "model_id"),
    species_col = benchmark_field(config, "species")
  )
}

#' Resolve benchmark candidate-pool labels
#'
#' @param policy_meta_lookup Optional policy-metadata lookup table assembled
#'   from one benchmark policy evaluation.
#' @param policies Optional vector of requested policy names.
#' @param policy_path Optional path to a policy-registry JSON file.
#'
#' @return Character vector of candidate-pool labels.
#'
#' @keywords internal
#' @noRd
benchmark_policy_candidate_pools <- function(policy_meta_lookup = NULL,
                                             policies = NULL,
                                             policy_path = NULL) {
  # Prefer the already-materialized policy metadata, then fall back to the
  # registry so the benchmark loop can skip ordination work when no active
  # policy pool depends on NMDS context.
  if (is.data.frame(policy_meta_lookup) &&
    "candidate_pool" %in% names(policy_meta_lookup)) {
    pools <- unique(as.character(policy_meta_lookup$candidate_pool))
    pools <- pools[!is.na(pools) & nzchar(pools)]
    if (length(pools) > 0L) {
      return(pools)
    }
  }

  requested <- unique(stringr::str_squish(as.character(unlist(policies %||% character(0), use.names = FALSE))))
  requested <- requested[!is.na(requested) & nzchar(requested)]
  if (length(requested) == 0L) {
    return(character(0))
  }

  lookup <- policy_lookup_table(policy_path = policy_path)
  registry <- read_policy_registry(policy_path = policy_path)
  pools <- vapply(requested, function(policy_name) {
    policy_def <- lookup[[policy_name]] %||%
      build_policy_definition_from_name(policy_name, registry = registry) %||%
      NULL
    as.character(policy_def$candidate_pool %||% NA_character_)[[1]]
  }, character(1))

  unique(pools[!is.na(pools) & nzchar(pools)])
}

#' Flag whether benchmark policies require ordination context
#'
#' @param policy_meta_lookup Optional policy-metadata lookup table assembled
#'   from one benchmark policy evaluation.
#' @param policies Optional vector of requested policy names.
#' @param policy_path Optional path to a policy-registry JSON file.
#'
#' @return Logical scalar.
#'
#' @keywords internal
#' @noRd
benchmark_needs_ordination_context <- function(policy_meta_lookup = NULL,
                                               policies = NULL,
                                               policy_path = NULL) {
  # Restrict the expensive per-anchor ordination reconstruction to candidate
  # pools that actually depend on cluster or ellipse geometry.
  ordination_pools <- c("same_nmds_cluster", "same_species_ellipse")
  pools <- benchmark_policy_candidate_pools(
    policy_meta_lookup = policy_meta_lookup,
    policies = policies,
    policy_path = policy_path
  )

  if (length(pools) == 0L) {
    return(TRUE)
  }

  any(pools %in% ordination_pools)
}

#' Choose the best policy for one anchor
#'
#' @param policy_tbl Policy table for one anchor.
#'
#' @return Character scalar or `NA`.
#'
#' @keywords internal
#' @noRd
pick_best_policy <- function(policy_tbl) {
  # Restrict the winner search to finite positive predictions so invalid
  # policies do not enter the benchmark ranking.
  valid <- normalize_policy_columns(policy_tbl)
  if (nrow(valid) == 0L || !"policy" %in% names(valid) || !"multiplier_pred" %in% names(valid)) {
    return(tibble::tibble(
      best_policy = NA_character_,
      best_equation_branch_filter = NA_character_
    ))
  }

  # Reuse existing derived columns when present, otherwise compute them once
  # with base vectors instead of a grouped tibble pipeline.
  pred_vals <- suppressWarnings(as.numeric(valid$multiplier_pred))
  valid_prediction <- if ("valid_prediction" %in% names(valid)) {
    as.logical(valid$valid_prediction)
  } else {
    is.finite(pred_vals) & pred_vals > 0
  }
  error_abs_log <- if ("error_abs_log" %in% names(valid)) {
    suppressWarnings(as.numeric(valid$error_abs_log))
  } else {
    out <- rep(NA_real_, length(pred_vals))
    out[valid_prediction] <- abs(log(pred_vals[valid_prediction]))
    out
  }

  keep <- which(valid_prediction %in% TRUE)
  keep <- keep[is.finite(error_abs_log[keep])]
  if (length(keep) == 0L) {
    return(tibble::tibble(
      best_policy = NA_character_,
      best_equation_branch_filter = NA_character_
    ))
  }

  policy_vals <- resolve_policy_names(valid)
  branch_vals <- resolve_policy_branch_filters(valid)
  best_idx <- keep[order(
    error_abs_log[keep],
    policy_vals[keep],
    branch_vals[keep]
  )][[1]]

  tibble::tibble(
    best_policy = policy_vals[[best_idx]],
    best_equation_branch_filter = branch_vals[[best_idx]]
  )
}

#' Build one benchmark feature row
#'
#' @param eval_obj Anchor evaluation object.
#' @param anchor_row One-row anchor table.
#' @param best_policy_name Best policy label.
#' @param best_equation_branch_filter Best branch-filter label.
#' @param is_reference Logical scalar.
#' @param config Benchmark config list.
#'
#' @return A one-row tibble.
#'
#' @keywords internal
#' @noRd
build_benchmark_row <- function(eval_obj,
                                anchor_row,
                                best_policy_name,
                                best_equation_branch_filter,
                                is_reference,
                                config) {
  # Collapse the admissible set to one row of anchor-level features used for
  # policy benchmarking and later policy-selection summaries.
  species_col <- benchmark_field(config, "species")
  id_col <- benchmark_field(config, "model_id")
  admissible <- tibble::as_tibble(eval_obj$admissible_df)
  n_admissible <- nrow(admissible)
  combined_distance <- suppressWarnings(as.numeric(admissible$combined_distance %||% numeric(0)))
  taxonomic_distance <- if ("taxonomic_distance_to_anchor" %in% names(admissible)) {
    suppressWarnings(as.numeric(admissible$taxonomic_distance_to_anchor))
  } else {
    numeric(0)
  }
  same_species <- if ("overlap_same_species" %in% names(admissible)) {
    as.logical(admissible$overlap_same_species)
  } else {
    rep(FALSE, n_admissible)
  }
  same_family <- if ("overlap_same_family" %in% names(admissible)) {
    as.logical(admissible$overlap_same_family)
  } else {
    rep(FALSE, n_admissible)
  }
  weights <- suppressWarnings(as.numeric(admissible$w_adm %||% numeric(0)))
  top_idx <- if (length(weights) > 0L) {
    head(order(weights, decreasing = TRUE, na.last = NA), 10L)
  } else {
    integer(0)
  }
  finite_min <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (length(x) == 0L) NA_real_ else min(x)
  }

  tibble::tibble(
    anchor_model_id = as.character(anchor_row[[id_col]][[1]]),
    anchor_species = as.character(anchor_row[[species_col]][[1]]),
    anchor_family = benchmark_optional_field_values(anchor_row, config, "family")[[1]],
    is_reference = is_reference,
    anchor_group = ifelse(is_reference, "reference", "candidate"),
    n_admissible = n_admissible,
    nearest_distance = finite_min(combined_distance),
    nearest_taxonomic_distance = finite_min(taxonomic_distance),
    nearest_same_species_distance = finite_min(combined_distance[same_species %in% TRUE]),
    nearest_same_family_distance = finite_min(combined_distance[same_family %in% TRUE]),
    top10_weight_same_species = if (length(top_idx) > 0L) sum(weights[top_idx][same_species[top_idx] %in% TRUE], na.rm = TRUE) else NA_real_,
    top10_weight_same_family = if (length(top_idx) > 0L) sum(weights[top_idx][same_family[top_idx] %in% TRUE], na.rm = TRUE) else NA_real_,
    best_policy = best_policy_name,
    best_equation_branch_filter = best_equation_branch_filter
  )
}

#' Compute TS-length values
#'
#' @param slope Numeric slope vector.
#' @param intercept Numeric intercept vector.
#' @param length_cm Numeric length vector.
#'
#' @return Numeric vector.
#'
#' @keywords internal
#' @noRd
ts_from_length <- function(slope,
                           intercept,
                           length_cm) {
  # Keep the TS-length transformation local to the benchmark layer so curve
  # error summaries do not depend on another source file being loaded.
  as.numeric(slope) * log10(length_cm) + as.numeric(intercept)
}

#' Build one policy TS-error table
#'
#' @param anchor_row One-row anchor table.
#' @param policy_tbl Policy table for the anchor.
#' @param config Benchmark config list.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
build_ts_errors <- function(anchor_row,
                            policy_tbl,
                            config) {
  empty_ts_tbl <- function() {
    tibble::tibble(
      anchor_model_id = character(),
      anchor_species = character(),
      policy = character(),
      equation_branch_filter = character(),
      policy_slope_len = numeric(),
      policy_intercept_len = numeric(),
      u = numeric(),
      length_cm = numeric(),
      ts_obs = numeric(),
      ts_pred = numeric(),
      ts_error = numeric(),
      abs_ts_error = numeric(),
      sigma_obs = numeric(),
      sigma_pred = numeric(),
      log_sigma_residual = numeric()
    )
  }

  # Skip the TS-length error summary entirely when no curve predictor was
  # supplied or the anchor lacks a usable standardized length-form equation.
  slope_col <- benchmark_field(config, "slope")
  intercept_col <- benchmark_field(config, "intercept")
  id_col <- benchmark_field(config, "model_id")
  species_col <- benchmark_field(config, "species")

  if (!all(c(slope_col, intercept_col) %in% names(anchor_row))) {
    return(empty_ts_tbl())
  }

  slope_val <- suppressWarnings(as.numeric(anchor_row[[slope_col]][[1]]))
  intercept_val <- suppressWarnings(as.numeric(anchor_row[[intercept_col]][[1]]))
  if (!is.finite(slope_val) || !is.finite(intercept_val)) {
    return(empty_ts_tbl())
  }

  # Use the anchor study-length span as the standardized evaluation domain,
  # then score all policies on the same 0-1 relative-length grid.
  lmin <- suppressWarnings(as.numeric(anchor_row$study_length_min[[1]]))
  lmax <- suppressWarnings(as.numeric(anchor_row$study_length_max[[1]]))
  if (!is.finite(lmin) || !is.finite(lmax) || lmax <= lmin) {
    return(empty_ts_tbl())
  }
  u_grid <- seq(0, 1, length.out = 11)
  eval_lengths <- lmin + u_grid * (lmax - lmin)

  ts_obs <- ts_from_length(slope_val, intercept_val, eval_lengths)
  if (!all(is.finite(ts_obs)) || any(ts_obs >= 0, na.rm = TRUE)) {
    anchor_id_val <- if (length(id_col) > 0 && id_col %in% names(anchor_row)) {
      as.character(anchor_row[[id_col]][[1]])
    } else {
      "unknown"
    }
    warning(
      sprintf(
        "Anchor '%s': TS values are non-negative or non-finite on the study length grid. ",
        anchor_id_val
      ),
      "TS must always be negative for real acoustic scatterers. ",
      "TS-error benchmarks for this anchor will be empty.",
      call. = FALSE
    )
    return(empty_ts_tbl())
  }

  policy_tbl_ <- policy_tbl |>
    dplyr::distinct(.data$policy, .data$equation_branch_filter, .keep_all = TRUE) |>
    dplyr::filter(
      is.finite(.data$policy_slope_len),
      is.finite(.data$policy_intercept_len)
    )
  if (nrow(policy_tbl_) == 0) {
    return(empty_ts_tbl())
  }

  slopes <- suppressWarnings(as.numeric(policy_tbl_$policy_slope_len))
  intercepts <- suppressWarnings(as.numeric(policy_tbl_$policy_intercept_len))
  log_lengths <- log10(eval_lengths)
  ts_pred_mat <- outer(log_lengths, slopes, "*") +
    matrix(intercepts, nrow = length(eval_lengths), ncol = length(intercepts), byrow = TRUE)
  valid_policy <- apply(
    ts_pred_mat,
    2,
    function(x) all(is.finite(x)) && !any(x >= 0, na.rm = TRUE)
  )
  if (!any(valid_policy)) {
    return(empty_ts_tbl())
  }

  policy_tbl_ <- policy_tbl_[valid_policy, , drop = FALSE]
  ts_pred_mat <- ts_pred_mat[, valid_policy, drop = FALSE]
  sigma_obs <- 10^(ts_obs / 10)
  out <- purrr::map_dfr(seq_len(ncol(ts_pred_mat)), function(i) {
    ts_pred <- ts_pred_mat[, i]
    sigma_pred <- 10^(ts_pred / 10)
    tibble::tibble(
      anchor_model_id = as.character(anchor_row[[id_col]][[1]]),
      anchor_species = as.character(anchor_row[[species_col]][[1]]),
      policy = as.character(policy_tbl_$policy[[i]]),
      equation_branch_filter = as.character(policy_tbl_$equation_branch_filter[[i]]),
      policy_slope_len = suppressWarnings(as.numeric(policy_tbl_$policy_slope_len[[i]])),
      policy_intercept_len = suppressWarnings(as.numeric(policy_tbl_$policy_intercept_len[[i]])),
      local_min_combined_distance = suppressWarnings(as.numeric(policy_tbl_$local_min_combined_distance[[i]] %||% NA_real_)),
      local_weighted_mean_combined_distance = suppressWarnings(as.numeric(policy_tbl_$local_weighted_mean_combined_distance[[i]] %||% NA_real_)),
      local_effective_support = suppressWarnings(as.numeric(policy_tbl_$local_effective_support[[i]] %||% NA_real_)),
      local_mean_length_overlap = suppressWarnings(as.numeric(policy_tbl_$local_mean_length_overlap[[i]] %||% NA_real_)),
      local_mean_depth_overlap = suppressWarnings(as.numeric(policy_tbl_$local_mean_depth_overlap[[i]] %||% NA_real_)),
      u = u_grid,
      length_cm = eval_lengths,
      ts_obs = ts_obs,
      ts_pred = ts_pred,
      ts_error = ts_pred - ts_obs,
      abs_ts_error = abs(ts_pred - ts_obs),
      sigma_obs = sigma_obs,
      sigma_pred = sigma_pred,
      log_sigma_residual = log(sigma_obs / sigma_pred)
    )
  })
  dplyr::filter(
    out,
    is.finite(.data$ts_error),
    is.finite(.data$abs_ts_error),
    is.finite(.data$log_sigma_residual)
  )
}

#' Build benchmark TS errors from a completed pseudo-anchor table
#'
#' @param candidate_models Candidate-model table.
#' @param policy_perf Pseudo-anchor policy-performance table.
#' @param config Benchmark config list.
#' @param progress Logical scalar. If `TRUE`, emit lightweight progress updates.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
build_benchmark_ts_error_table <- function(candidate_models,
                                           policy_perf,
                                           config,
                                           progress = FALSE) {
  # Build TS-error rows in a second pass so the main benchmark loop and its
  # caches stay focused on core policy-performance tables.
  perf_tbl <- normalize_policy_columns(policy_perf)
  perf_tbl$policy <- resolve_policy_names(perf_tbl)
  if (!is.data.frame(candidate_models) || nrow(candidate_models) == 0L ||
    nrow(perf_tbl) == 0L) {
    return(tibble::tibble())
  }
  if (!"anchor_model_id" %in% names(perf_tbl)) {
    return(tibble::tibble())
  }

  id_col <- benchmark_field(config, "model_id")
  candidate_tbl <- tibble::as_tibble(candidate_models)
  anchor_ids <- unique(as.character(perf_tbl$anchor_model_id))
  anchor_ids <- anchor_ids[!is.na(anchor_ids) & nzchar(anchor_ids)]
  if (length(anchor_ids) == 0L) {
    return(tibble::tibble())
  }

  progress_step <- max(1L, ceiling(length(anchor_ids) / 20L))
  t_start <- if (isTRUE(progress)) Sys.time() else NULL
  out_rows <- vector("list", length(anchor_ids))

  for (i in seq_along(anchor_ids)) {
    anchor_id <- anchor_ids[[i]]
    anchor_row <- candidate_tbl[as.character(candidate_tbl[[id_col]]) == anchor_id, , drop = FALSE]
    if (nrow(anchor_row) == 0L) {
      next
    }
    policy_rows_now <- perf_tbl[as.character(perf_tbl$anchor_model_id) == anchor_id, , drop = FALSE]
    out_rows[[i]] <- build_ts_errors(
      anchor_row = anchor_row[1, , drop = FALSE],
      policy_tbl = policy_rows_now,
      config = config
    ) |>
      dplyr::mutate(validation_scheme = "pseudo_anchor")

    if (isTRUE(progress) && (i %% progress_step == 0L || i == length(anchor_ids))) {
      elapsed_s <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
      rate <- i / max(elapsed_s, 0.001)
      eta_s <- if (i < length(anchor_ids)) (length(anchor_ids) - i) / rate else 0
      eta_str <- if (i >= length(anchor_ids)) "done" else if (eta_s < 60) sprintf("%.0fs", eta_s) else if (eta_s < 3600) sprintf("%.1fmin", eta_s / 60) else sprintf("%.1fh", eta_s / 3600)
      tsb_message(sprintf(
        "TS-error progress: %d/%d (%d%%) | elapsed: %.0fs | %.2f anchors/s | ETA: ~%s",
        i, length(anchor_ids), as.integer(100 * i / length(anchor_ids)),
        elapsed_s, rate, eta_str
      ))
    }
  }

  dplyr::bind_rows(out_rows)
}

#' Build benchmark TS errors with the compiled engine
#'
#' @param candidate_models Candidate-model table.
#' @param policy_perf Pseudo-anchor policy-performance table.
#' @param config Benchmark config list.
#'
#' @return A tibble matching the R benchmark TS-error table path.
#'
#' @keywords internal
#' @noRd
build_benchmark_ts_error_table_cpp <- function(candidate_models,
                                               policy_perf,
                                               config) {
  perf_tbl <- normalize_policy_columns(policy_perf)
  if (!is.data.frame(candidate_models) || nrow(candidate_models) == 0L ||
    nrow(perf_tbl) == 0L || !"anchor_model_id" %in% names(perf_tbl)) {
    return(tibble::tibble())
  }

  perf_tbl$policy <- resolve_policy_names(perf_tbl)
  perf_tbl$equation_branch_filter <- resolve_policy_branch_filters(perf_tbl)
  candidate_tbl <- tibble::as_tibble(candidate_models)
  id_col <- benchmark_field(config, "model_id")
  species_col <- benchmark_field(config, "species")
  slope_col <- benchmark_field(config, "slope")
  intercept_col <- benchmark_field(config, "intercept")

  candidate_numeric <- function(name) {
    if (name %in% names(candidate_tbl)) {
      suppressWarnings(as.numeric(candidate_tbl[[name]]))
    } else {
      rep(NA_real_, nrow(candidate_tbl))
    }
  }
  candidate_character <- function(name) {
    if (name %in% names(candidate_tbl)) {
      as.character(candidate_tbl[[name]])
    } else {
      rep(NA_character_, nrow(candidate_tbl))
    }
  }
  policy_numeric <- function(name) {
    if (name %in% names(perf_tbl)) {
      suppressWarnings(as.numeric(perf_tbl[[name]]))
    } else {
      rep(NA_real_, nrow(perf_tbl))
    }
  }

  out <- cpp_build_benchmark_ts_errors(
    anchors = list(
      anchor_model_id = candidate_character(id_col),
      anchor_species = candidate_character(species_col),
      anchor_slope = candidate_numeric(slope_col),
      anchor_intercept = candidate_numeric(intercept_col),
      study_length_min = candidate_numeric("study_length_min"),
      study_length_max = candidate_numeric("study_length_max")
    ),
    policies = list(
      anchor_model_id = as.character(perf_tbl$anchor_model_id),
      policy = as.character(perf_tbl$policy),
      equation_branch_filter = as.character(perf_tbl$equation_branch_filter),
      policy_slope_len = policy_numeric("policy_slope_len"),
      policy_intercept_len = policy_numeric("policy_intercept_len"),
      local_min_combined_distance = policy_numeric("local_min_combined_distance"),
      local_weighted_mean_combined_distance = policy_numeric("local_weighted_mean_combined_distance"),
      local_effective_support = policy_numeric("local_effective_support"),
      local_mean_length_overlap = policy_numeric("local_mean_length_overlap"),
      local_mean_depth_overlap = policy_numeric("local_mean_depth_overlap")
    ),
    grid_size = 11L
  )
  tibble::as_tibble(out) |>
    dplyr::mutate(validation_scheme = "pseudo_anchor")
}

#' Remove same-species support from one anchor evaluation
#'
#' @param eval_obj Anchor evaluation object.
#' @param anchor_row One-row anchor table.
#' @param config Benchmark config list.
#'
#' @return Modified anchor evaluation object.
#'
#' @keywords internal
#' @noRd
remove_species_support <- function(eval_obj,
                                   anchor_row,
                                   config) {
  # Rebuild the admissible support set after removing same-species donor rows
  # so the leave-one-species-out benchmark uses a properly renormalized pool.
  species_col <- benchmark_field(config, "species")
  anchor_species <- as.character(anchor_row[[species_col]][[1]])
  anchor_is_group <- if ("is_group_model" %in% names(anchor_row)) {
    isTRUE(as.logical(anchor_row$is_group_model[[1]]))
  } else {
    FALSE
  }
  anchor_has_species_identity <- !is_missing_species_identity(anchor_species)

  # Generalized equations do not represent one biological species. The
  # benchmark caller excludes them from species-block evaluation; retain this
  # guard so direct calls cannot collapse them into a synthetic species block.
  if (anchor_is_group || !anchor_has_species_identity) {
    return(eval_obj)
  }
  core_weight_cutoff <- suppressWarnings(as.numeric(config$core_weight_cutoff %||% 0.8))
  if (!is.finite(core_weight_cutoff)) {
    core_weight_cutoff <- 0.8
  }
  out <- eval_obj

  a_out <- tibble::as_tibble(eval_obj$admissible_df) |>
    dplyr::filter(!.data$overlap_same_species) |>
    dplyr::arrange(dplyr::desc(.data$w_adm))

  if (nrow(a_out) > 0) {
    a_out <- a_out |>
      dplyr::mutate(
        w_adm = .data$w_adm / sum(.data$w_adm, na.rm = TRUE),
        cumulative_w_adm = cumsum(.data$w_adm),
        support_set = dplyr::if_else(
          .data$cumulative_w_adm <= core_weight_cutoff,
          "core",
          "tail"
        )
      )
  }

  out$admissible_df <- a_out
  out$model_eval <- tibble::as_tibble(eval_obj$model_eval) |>
    dplyr::filter(.data[[species_col]] != anchor_species)

  out
}

#' Remove same-group support from one anchor evaluation
#'
#' @param eval_obj Anchor evaluation object.
#' @param anchor_row One-row anchor table.
#' @param group_col Grouping column to purge.
#'
#' @return Modified anchor evaluation object.
#'
#' @keywords internal
#' @noRd
remove_group_support <- function(eval_obj,
                                 anchor_row,
                                 group_col) {
  if (is.null(group_col) || !nzchar(group_col) ||
    !group_col %in% names(anchor_row) ||
    !group_col %in% names(eval_obj$model_eval)) {
    return(eval_obj)
  }
  anchor_group <- as.character(anchor_row[[group_col]][[1]])
  if (is.na(anchor_group) || !nzchar(anchor_group)) {
    return(eval_obj)
  }

  out <- eval_obj
  out$model_eval <- tibble::as_tibble(eval_obj$model_eval) |>
    dplyr::filter(as.character(.data[[group_col]]) != anchor_group)
  a_out <- tibble::as_tibble(eval_obj$admissible_df)
  if (group_col %in% names(a_out)) {
    a_out <- a_out |>
      dplyr::filter(as.character(.data[[group_col]]) != anchor_group) |>
      dplyr::arrange(dplyr::desc(.data$w_adm))
  }
  if (nrow(a_out) > 0 && "w_adm" %in% names(a_out)) {
    denom <- sum(a_out$w_adm, na.rm = TRUE)
    if (is.finite(denom) && denom > 0) {
      a_out <- a_out |>
        dplyr::mutate(
          w_adm = .data$w_adm / denom,
          cumulative_w_adm = cumsum(.data$w_adm)
        )
    }
  }
  out$admissible_df <- a_out
  out
}

#' Return cached admissibility evaluations for benchmarking
#'
#' @param candidates_obj A [Candidates] object.
#' @param config Optional config object used to validate cache freshness.
#'
#' @return Named list of cached anchor evaluations.
#'
#' @keywords internal
#' @noRd
benchmark_cached_admissibility_lookup <- function(candidates_obj,
                                                  config = NULL) {
  if (is.null(candidates_obj) || length(candidates_obj@admissibility) == 0L) {
    return(list())
  }

  admissibility_bundle <- candidates_obj@admissibility
  if (!admissibility_bundle_is_current(admissibility_bundle, config)) {
    return(list())
  }

  anchor_results <- admissibility_bundle$anchors %||% list()
  if (!is.list(anchor_results) || length(anchor_results) == 0L) {
    return(list())
  }

  evals <- lapply(anchor_results, function(x) x$evaluation %||% NULL)
  evals[!vapply(evals, is.null, logical(1))]
}

#' Look up one cached benchmark anchor evaluation
#'
#' @param cached_anchor_evals Named list of cached evaluations.
#' @param anchor_row One-row anchor table.
#' @param config Benchmark config object or list.
#'
#' @return Cached evaluation object or `NULL`.
#'
#' @keywords internal
#' @noRd
benchmark_cached_anchor_eval <- function(cached_anchor_evals,
                                         anchor_row,
                                         config) {
  if (!is.list(cached_anchor_evals) || length(cached_anchor_evals) == 0L) {
    return(NULL)
  }

  anchor_id <- build_anchor_model_id(
    anchor_row = anchor_row,
    config = default_anchor_config(config)
  )
  cached_anchor_evals[[anchor_id]] %||% NULL
}

#' Benchmark one anchor
#'
#' @param anchor_row One-row anchor table.
#' @param candidate_models Candidate-model table.
#' @param policy_fun Policy-extraction function.
#' @param curve_fun Optional policy-curve function. TS-error generation is
#'   handled in a separate benchmark pass.
#' @param model_scores Optional ordination score table.
#' @param species_lookup Optional species lookup table/list.
#' @param reference_ids Optional reference-model IDs.
#' @param policies Optional vector of policy names to evaluate.
#' @param policy_params Optional named list of extra policy parameters.
#' @param policy_path Optional path to a policy-registry JSON file.
#' @param policy_execution_plan Optional prebuilt policy execution plan from
#'   `build_policy_execution_plan()`. When supplied, the benchmark reuses the
#'   same registry and branch expansion across anchors.
#' @param engine Policy evaluation engine, `"r"` or `"cpp"`.
#' @param compiled_policy_plan Optional compiled C++ policy plan.
#' @param sim_obj Optional prebuilt similarity object for the full benchmark
#'   scenario.
#' @param dist_obj Optional prebuilt distance object for the full benchmark
#'   scenario.
#' @param candidate_models_scored Optional candidate-model table that already
#'   contains `key_metadata_missing_fraction`.
#' @param eval_obj Optional precomputed anchor-evaluation object. Supplying this
#'   avoids rebuilding the same admissible donor pool for multiple validation
#'   schemes.
#' @param config Benchmark config list.
#' @param registry_path Optional registry path.
#' @param scheme Validation-scheme label.
#' @param species_block Logical scalar. If `TRUE`, exclude same-species donors.
#' @param ordination_info Optional precomputed ordination context for the
#'   current anchor.
#' @param resolve_ordination Logical scalar. If `TRUE`, rebuild ordination
#'   context when `ordination_info` was not precomputed by the caller.
#'
#' @return A list with `perf`, `features`, and `ts_error`.
#'
#' @keywords internal
#' @noRd
benchmark_one_anchor <- function(anchor_row,
                                 candidate_models,
                                 policy_fun,
                                 curve_fun,
                                 model_scores,
                                 species_lookup,
                                 reference_ids,
                                 policies,
                                 policy_params,
                                 policy_path,
                                 policy_execution_plan = NULL,
                                 engine = "r",
                                 compiled_policy_plan = NULL,
                                 sim_obj,
                                 dist_obj,
                                 candidate_models_scored,
                                 eval_obj = NULL,
                                 config,
                                 registry_path,
                                 scheme,
                                 species_block = FALSE,
                                 group_block_col = NULL,
                                 ordination_info = NULL,
                                 resolve_ordination = TRUE) {
  species_col <- benchmark_field(config, "species")
  anchor_species_value <- as.character(anchor_row[[species_col]][[1]])
  # Species-block validation is undefined for generalized equations. They
  # remain available as donors and pseudo-anchors, but cannot constitute a
  # biological holdout species.
  if (isTRUE(species_block) && is_missing_species_identity(anchor_species_value)) {
    return(NULL)
  }

  # Evaluate one anchor first, optionally rebuild the donor pool without same-
  # species rows, then extract the policy benchmark tables.
  eval_obj_ <- eval_obj
  if (is.null(eval_obj_)) {
    eval_obj_ <- tryCatch(
      screen_one_anchor_admissibility(
        anchor_row = anchor_row,
        candidate_models = candidate_models,
        config = config,
        registry_path = registry_path,
        sim_obj = sim_obj,
        dist_obj = dist_obj,
        candidate_models_scored = candidate_models_scored
      ),
      error = function(e) NULL
    )
  }
  if (is.null(eval_obj_)) {
    return(NULL)
  }

  if (isTRUE(species_block)) {
    eval_obj_ <- remove_species_support(eval_obj_, anchor_row, config)
  }
  if (!is.null(group_block_col)) {
    eval_obj_ <- remove_group_support(eval_obj_, anchor_row, group_block_col)
  }

  # Rebuild the ordination context only when the active policy suite actually
  # contains ordination-dependent candidate pools.
  ordination_info_ <- ordination_info
  if (isTRUE(resolve_ordination) && is.null(ordination_info_)) {
    ordination_info_ <- resolve_ordination_info(
      anchor_row = anchor_row,
      model_scores = model_scores,
      species_lookup = species_lookup,
      config = config
    )
  }

  engine <- normalize_policy_benchmark_engine(engine)
  # The default evaluator has a centralized R/C++ dispatch. Custom policy
  # functions remain supported by the R engine only.
  if (isTRUE(identical(policy_fun, evaluate_policies))) {
    policy_tbl <- benchmark_evaluate_policies(
      engine = engine,
      eval_obj = eval_obj_,
      ordination_info = ordination_info_,
      policies = policies,
      policy_params = policy_params,
      policy_path = policy_path,
      execution_plan = policy_execution_plan,
      compiled_plan = compiled_policy_plan
    )
  } else {
    if (identical(engine, "cpp")) {
      stop("The C++ benchmark engine supports the packaged policy evaluator only.", call. = FALSE)
    }
    policy_args <- list(
      eval_obj = eval_obj_,
      ordination_info = ordination_info_,
      policies = policies,
      policy_params = policy_params,
      policy_path = policy_path,
      execution_plan = policy_execution_plan
    )
    policy_args <- policy_args[names(policy_args) %in% names(formals(policy_fun))]
    policy_tbl <- do.call(policy_fun, policy_args)
  }
  policy_tbl <- normalize_policy_columns(policy_tbl)
  if (!isTRUE(identical(policy_fun, evaluate_policies))) {
    policy_tbl$policy <- resolve_policy_names(policy_tbl)
  }

  if (!all(c("policy", "multiplier_pred") %in% names(policy_tbl))) {
    stop("'policy_fun' must return columns named 'policy' and 'multiplier_pred'.", call. = FALSE)
  }

  id_col <- benchmark_field(config, "model_id")
  anchor_id <- as.character(anchor_row[[id_col]][[1]])
  is_ref <- anchor_id %in% reference_ids

  # Add the benchmark annotations once so the same policy table can feed the
  # best-policy summary and any later conformal evaluation.
  policy_tbl$anchor_model_id <- anchor_id
  policy_tbl$anchor_species <- if (is_missing_species_identity(anchor_species_value)) {
    NA_character_
  } else {
    anchor_species_value
  }
  policy_tbl$anchor_family <- rep(
    benchmark_optional_field_values(anchor_row, config, "family")[[1]],
    nrow(policy_tbl)
  )
  policy_tbl$anchor_group_block <- if (!is.null(group_block_col) && group_block_col %in% names(anchor_row)) {
    as.character(anchor_row[[group_block_col]][[1]])
  } else {
    NA_character_
  }
  policy_tbl$is_reference <- is_ref
  policy_tbl$anchor_group <- if (is_ref) "reference" else "candidate"
  policy_tbl$validation_scheme <- scheme

  best_policy_row <- pick_best_policy(policy_tbl)
  feature_row <- build_benchmark_row(
    eval_obj = eval_obj_,
    anchor_row = anchor_row,
    best_policy_name = best_policy_row$best_policy[[1]],
    best_equation_branch_filter = best_policy_row$best_equation_branch_filter[[1]],
    is_reference = is_ref,
    config = config
  ) |>
    dplyr::mutate(
      validation_scheme = scheme,
      anchor_group_block = if (!is.null(group_block_col) && group_block_col %in% names(anchor_row)) {
        as.character(anchor_row[[group_block_col]][[1]])
      } else {
        NA_character_
      }
    )

  ts_error <- tibble::tibble()

  list(
    perf = policy_tbl,
    features = feature_row,
    ts_error = ts_error
  )
}

#' Build one best-policy table
#'
#' @param perf_tbl Policy-performance table.
#'
#' @return A tibble.
#'
#' @keywords internal
#' @noRd
bind_best_policy_rows <- function(perf_tbl) {
  # Pick the best valid policy per anchor and validation scheme using the
  # smallest absolute log-error, with policy name as the deterministic tiebreak.
  out <- normalize_policy_columns(perf_tbl)
  if (nrow(out) == 0 || !"valid_prediction" %in% names(out)) {
    return(tibble::tibble())
  }
  if (!"anchor_group_block" %in% names(out)) {
    out$anchor_group_block <- NA_character_
  }

  out$policy <- resolve_policy_names(out)
  keep <- which(as.logical(out$valid_prediction) %in% TRUE)
  if (length(keep) == 0L) {
    return(tibble::tibble())
  }

  key_tbl <- tibble::tibble(
    anchor_model_id = as.character(out$anchor_model_id[keep]),
    anchor_species = as.character(out$anchor_species[keep]),
    anchor_family = as.character(out$anchor_family[keep]),
    anchor_group_block = as.character(out$anchor_group_block[keep]),
    is_reference = as.logical(out$is_reference[keep]),
    anchor_group = as.character(out$anchor_group[keep]),
    validation_scheme = as.character(out$validation_scheme[keep]),
    best_policy = as.character(out$policy[keep]),
    best_equation_branch_filter = as.character(out$equation_branch_filter[keep]),
    best_multiplier_pred = suppressWarnings(as.numeric(out$multiplier_pred[keep])),
    best_error_abs_log = suppressWarnings(as.numeric(out$error_abs_log[keep]))
  )

  ord <- order(
    key_tbl$anchor_model_id,
    key_tbl$anchor_species,
    key_tbl$anchor_family,
    key_tbl$anchor_group_block,
    key_tbl$is_reference,
    key_tbl$anchor_group,
    key_tbl$validation_scheme,
    key_tbl$best_error_abs_log,
    key_tbl$best_policy,
    key_tbl$best_equation_branch_filter,
    na.last = TRUE
  )
  key_tbl <- key_tbl[ord, , drop = FALSE]

  group_key <- paste(
    key_tbl$anchor_model_id,
    key_tbl$anchor_species,
    key_tbl$anchor_family,
    key_tbl$anchor_group_block,
    key_tbl$is_reference,
    key_tbl$anchor_group,
    key_tbl$validation_scheme,
    sep = "\r"
  )
  key_tbl[!duplicated(group_key), , drop = FALSE]
}

#' Run the policy benchmark
#'
#' Evaluates every prepared model as a pseudo-anchor, applies a caller-supplied
#' policy extractor, and returns in-memory benchmark tables for both the full
#' donor pool and the leave-one-species-out donor pool.
#'
#' @param candidate_models Prepared candidate-model table or a [Candidates]
#'   object.
#' @param policy_fun Policy-extraction function. It must accept `eval_obj`
#'   plus optional ordination context, and return at least `policy` and
#'   `multiplier_pred`.
#' @param curve_fun Optional policy-curve function for TS-length error
#'   summaries. It must accept `policy`, `eval_obj`, optional ordination
#'   context, and `lengths_cm`.
#' @param model_scores Optional ordination score table.
#' @param species_lookup Optional species lookup object used by ordination-dependent
#'   policies.
#' @param reference_ids Optional vector of reference-model IDs used only to
#'   annotate the output tables.
#' @param policies Optional character vector of policy names to evaluate.
#' @param policy_params Optional named list of per-policy parameter overrides.
#' @param policy_path Optional policy-registry path.
#' @param config Optional JSON path or list with benchmark/admissibility
#'   settings.
#' @param include_ts_error Logical scalar. If `TRUE`, compute the relative-length
#'   TS error table used by the TS conformal summaries.
#' @param benchmark_schemes Validation schemes to compute. Supported values are
#'   `"pseudo_anchor"`, `"species_block"`, and `"group_block"`.
#' @param workers Number of parallel workers. Use `1` for sequential execution.
#' @param engine Policy evaluation engine, `"r"` or `"cpp"`.
#' @param cache_path Optional `.rds` cache path.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache.
#' @param progress Logical scalar. If `TRUE`, emit lightweight progress updates
#'   during the anchor loop.
#' @param group_block_col Optional grouping column for group-block validation.
#' @param group_block_label Label used for group-block validation outputs.
#' @param registry_path Optional path to the trait-registry JSON.
#'
#' @return A list containing full-pool and leave-one-species-out benchmark
#'   tables.
#'
#' @keywords internal
#' @noRd
run_policy_benchmark <- function(candidate_models,
                                 policy_fun = evaluate_policies,
                                 curve_fun = predict_policy_curve,
                                 model_scores = NULL,
                                 species_lookup = NULL,
                                 reference_ids = NULL,
                                 policies = NULL,
                                 policy_params = list(),
                                 policy_path = NULL,
                                 config = NULL,
                                 include_ts_error = TRUE,
                                 benchmark_schemes = c("pseudo_anchor", "species_block"),
                                 workers = 1L,
                                 engine = "r",
                                 cache_path = NULL,
                                 refresh = FALSE,
                                 progress = FALSE,
                                 group_block_col = NULL,
                                 group_block_label = "leave_one_group_out",
                                 registry_path = NULL) {
  # Support the prepared `Candidates` object directly so the benchmark layer can
  # reuse prepared similarity, distance, ordination, and reference-anchor
  # state when those objects already exist.
  candidates_obj <- if (is_s7_instance(candidate_models, "Candidates")) {
    candidate_models
  } else {
    NULL
  }
  candidate_models_ <- candidate_models
  model_scores_ <- model_scores
  species_lookup_ <- species_lookup
  reference_ids_ <- reference_ids
  if (!is.null(candidates_obj)) {
    ordination_context <- policy_selector_ordination_context(
      candidates_obj@ordination
    )
    if (is.null(model_scores_) &&
      is.data.frame(ordination_context$model_scores %||% NULL)) {
      model_scores_ <- ordination_context$model_scores
    }
    if (is.null(species_lookup_) &&
      is.list(ordination_context$species_lookup)) {
      species_lookup_ <- ordination_context$species_lookup
    }
    if (is.null(reference_ids_) && nrow(candidates_obj@reference_anchors) > 0) {
      if ("model_id" %in% names(candidates_obj@reference_anchors)) {
        reference_ids_ <- as.character(candidates_obj@reference_anchors$model_id)
      } else if ("model_id" %in% names(candidates_obj@reference_anchors)) {
        reference_ids_ <- as.character(candidates_obj@reference_anchors$model_id)
      }
    }
    candidate_models_ <- tibble::as_tibble(candidates_obj@candidate_models)
  }

  # Validate the benchmark inputs once before any anchor loop or cache work.
  if (!is.data.frame(candidate_models_)) {
    stop("'candidate_models' must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.function(policy_fun)) {
    stop("'policy_fun' must be a function.", call. = FALSE)
  }
  if (!is.null(curve_fun) && !is.function(curve_fun)) {
    stop("'curve_fun' must be NULL or a function.", call. = FALSE)
  }
  if (!is.null(cache_path) &&
    (!is.character(cache_path) || length(cache_path) != 1 || !nzchar(cache_path))) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("'progress' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(include_ts_error) || length(include_ts_error) != 1 || is.na(include_ts_error)) {
    stop("'include_ts_error' must be TRUE or FALSE.", call. = FALSE)
  }
  benchmark_schemes_ <- if (missing(benchmark_schemes) && !is.null(group_block_col)) {
    c("pseudo_anchor", "species_block", "group_block")
  } else {
    benchmark_schemes %||% c("pseudo_anchor", "species_block")
  }
  benchmark_schemes_ <- unique(as.character(benchmark_schemes_))
  valid_schemes <- c("pseudo_anchor", "species_block", "group_block", group_block_label)
  if (length(benchmark_schemes_) == 0 ||
    any(is.na(benchmark_schemes_) | !nzchar(benchmark_schemes_)) ||
    any(!benchmark_schemes_ %in% valid_schemes)) {
    stop(
      "'benchmark_schemes' must contain only 'pseudo_anchor', 'species_block', or 'group_block'.",
      call. = FALSE
    )
  }
  run_pseudo_anchor <- "pseudo_anchor" %in% benchmark_schemes_
  run_species_block <- "species_block" %in% benchmark_schemes_
  run_group_block <- !is.null(group_block_col) &&
    any(c("group_block", group_block_label) %in% benchmark_schemes_)
  if (!is.numeric(workers) || length(workers) != 1 || !is.finite(workers) || workers < 1) {
    stop("'workers' must be one finite number >= 1.", call. = FALSE)
  }
  workers_ <- as.integer(workers)
  engine_ <- normalize_policy_benchmark_engine(engine)
  if (identical(engine_, "cpp") && !isTRUE(identical(policy_fun, evaluate_policies))) {
    stop("The C++ benchmark engine cannot be combined with a custom `policy_fun`.", call. = FALSE)
  }
  # Reuse the cached benchmark object when available unless the caller asked
  # for a refresh.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    report_progress(progress, "Loading cached policy benchmark from ", cache_path, ".")
    return(readRDS(cache_path))
  }

  # Inline the benchmark defaults here so the benchmark layer does not carry a
  # separate default-config helper.
  config_values <- merge_config_sections(
    list(
      fields = list(
        model_id = "model_id",
        species = "species_name",
        family = "family",
        slope = "slope_standard",
        intercept = "intercept_standard"
      ),
      core_weight_cutoff = 0.8,
      species_block_label = "leave_one_species_out"
    ),
    default_anchor_config(config)
  )
  config_slope_class <- NULL
  config_policy_params <- list()
  if (is_s7_instance(config, "Configurer")) {
    config_slope_class <- (((config@data)$policies) %||% list())$slope_class %||% NULL
    config_policy_params <- (((config@data)$policies) %||% list())$policy_params %||% list()
  } else if (is.list(config)) {
    config_slope_class <- config$slope_class %||%
      config$policies$slope_class %||% NULL
    config_policy_params <- config$policy_params %||%
      config$policies$policy_params %||%
      list()
  }
  policy_params_ <- merge_config_sections(
    config_policy_params,
    merge_config_sections(
      list(slope_class = config_slope_class),
      policy_params
    )
  )
  ref_ids <- normalize_reference_ids(reference_ids_)
  id_col_nm <- benchmark_field(config_values, "model_id")

  # Build the scenario-level distance context once so every anchor in the
  # benchmark loop can reuse it. When a `Candidates` object already carries
  # precomputed learned/Gower distances, keep using that state even when a
  # config object is supplied. This preserves the Alchemist workflow boundary
  # instead of silently dropping back to similarity-prep.
  use_precomputed_dist <- !is.null(candidates_obj) &&
    length(candidates_obj@gower_distances) > 0
  if (!is.null(candidates_obj) &&
    length(candidates_obj@similarity_matrix) > 0) {
    sim_obj <- candidates_obj@similarity_matrix
  } else if (use_precomputed_dist) {
    sim_obj <- list(
      alpha = config_values$alpha %||% NULL,
      k_species = config_values$k_species %||% NULL,
      k_study = config_values$k_study %||% NULL,
      frequency_span = compute_frequency_span(candidate_models_$frequency %||% numeric(0)),
      candidate_models = tibble::as_tibble(candidate_models_)
    )
  } else {
    sim_obj <- prepare_similarities(
      candidate_models = candidate_models_,
      species_traits = config_values$species_traits %||% NULL,
      study_traits = config_values$study_traits %||% NULL,
      alpha = config_values$alpha %||% NULL,
      k_species = config_values$k_species %||% NULL,
      k_study = config_values$k_study %||% NULL,
      config = config_values,
      registry_path = registry_path,
      seed = config_values$seed %||% NULL
    )
  }
  if (use_precomputed_dist) {
    dist_obj <- candidates_obj@gower_distances
  } else {
    dist_obj <- construct_gower_distances(sim_obj)
  }
  candidate_models_prepared <- tibble::as_tibble(sim_obj$candidate_models %||% candidate_models_)
  candidate_models_scored <- screen_missing_metadata(
    candidate_models = candidate_models_prepared,
    key_cols = admissibility_key_metadata_cols(config_values)
  )
  candidate_models_scored <- prepare_admissibility_overlap_columns(
    candidate_models = candidate_models_scored,
    config = config_values,
    registry_path = registry_path
  )
  cached_anchor_evals <- benchmark_cached_admissibility_lookup(
    candidates_obj = candidates_obj,
    config = config_values
  )
  # Slim both objects to only what workers need before cluster creation.
  # screen_one_anchor_admissibility accesses: sim_obj$alpha, $k_species,
  # $k_study, $frequency_span (all scalars) and dist_obj$species_dist_model,
  # $study_dist (the two NxN matrices). Everything else is dead weight.
  # On fork clusters this also reduces the parent-process memory footprint.
  sim_obj <- list(
    alpha = sim_obj$alpha,
    k_species = sim_obj$k_species,
    k_study = sim_obj$k_study,
    frequency_span = sim_obj$frequency_span
  )
  dist_obj <- list(
    species_dist_model = dist_obj$species_dist_model,
    study_dist = dist_obj$study_dist,
    learned_directed_dist = dist_obj$learned_directed_dist %||% NULL,
    taxonomic_dist_model = dist_obj$taxonomic_dist_model %||% NULL,
    learned_kernel_bandwidth = dist_obj$learned_kernel_bandwidth %||% NULL,
    distance_mode = dist_obj$distance_mode %||% NULL
  )

  perf_rows <- list()
  feat_rows <- list()
  err_rows <- list()
  sb_perf_rows <- list()
  sb_feat_rows <- list()
  gb_perf_rows <- list()
  gb_feat_rows <- list()
  total_anchors <- nrow(candidate_models_)
  cache_shard_dir <- if (!is.null(cache_path)) paste0(cache_path, "_parts") else NULL
  if (!is.null(cache_shard_dir)) {
    dir.create(cache_shard_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Resolve one per-anchor cache file under a dedicated shard directory so
  # resumed benchmark runs can reuse completed anchors safely.
  .anchor_cache_file <- function(anchor_id) {
    safe_anchor_id <- gsub("[^[:alnum:]_]+", "_", as.character(anchor_id %||% "unknown"))
    file.path(cache_shard_dir, paste0("anchor_", safe_anchor_id, ".rds"))
  }

  # Read one cached anchor bundle only when shard caching is active and the
  # caller did not explicitly request a refresh.
  .read_anchor_cache <- function(anchor_id) {
    if (is.null(cache_shard_dir) || isTRUE(refresh)) {
      return(NULL)
    }
    cache_file_now <- .anchor_cache_file(anchor_id)
    if (!file.exists(cache_file_now)) {
      return(NULL)
    }
    readRDS(cache_file_now)
  }

  # Persist one anchor bundle atomically so interrupted runs do not leave
  # partial cache shards behind.
  .write_anchor_cache <- function(anchor_id,
                                  anchor_result) {
    if (is.null(cache_shard_dir)) {
      return(invisible(NULL))
    }
    cache_file_now <- .anchor_cache_file(anchor_id)
    cache_tmp_now <- paste0(cache_file_now, ".tmp")
    saveRDS(anchor_result, cache_tmp_now)
    if (file.exists(cache_file_now)) {
      file.remove(cache_file_now)
    }
    file.rename(cache_tmp_now, cache_file_now)
    invisible(NULL)
  }

  # Append one anchor bundle into the accumulating benchmark tables regardless
  # of whether the bundle was computed in this run or read from a cache shard.
  .append_anchor_result <- function(anchor_result) {
    if (is.null(anchor_result) || !is.list(anchor_result)) {
      return(invisible(NULL))
    }
    if (!is.null(anchor_result$bench)) {
      perf_rows[[length(perf_rows) + 1L]] <<- anchor_result$bench$perf
      feat_rows[[length(feat_rows) + 1L]] <<- anchor_result$bench$features
      err_rows[[length(err_rows) + 1L]] <<- anchor_result$bench$ts_error
    }
    if (!is.null(anchor_result$species_block)) {
      sb_perf_rows[[length(sb_perf_rows) + 1L]] <<- anchor_result$species_block$perf
      sb_feat_rows[[length(sb_feat_rows) + 1L]] <<- anchor_result$species_block$features
    }
    if (!is.null(anchor_result$group_block)) {
      gb_perf_rows[[length(gb_perf_rows) + 1L]] <<- anchor_result$group_block$perf
      gb_feat_rows[[length(gb_feat_rows) + 1L]] <<- anchor_result$group_block$features
    }
    invisible(NULL)
  }
  n_equation_branch_filters <- length(normalize_policy_equation_branch_filters(
    policy_params_$slope_class %||% NULL
  ))
  if (!is.finite(n_equation_branch_filters) || n_equation_branch_filters < 1L) {
    n_equation_branch_filters <- 1L
  }
  scheme_count <- sum(c(run_pseudo_anchor, run_species_block, run_group_block))
  estimated_policy_rows <- total_anchors * max(1L, length(policies)) * n_equation_branch_filters * max(1L, scheme_count)
  report_progress(
    progress,
    "Policy benchmark workload: ",
    total_anchors,
    " anchor(s), ",
    length(policies),
    " policy/ies, ",
    n_equation_branch_filters,
    " branch filter(s), ",
    max(1L, scheme_count),
    " validation scheme(s), ~",
    estimated_policy_rows,
    " anchor-policy evaluation row(s)."
  )

  # Static column sets stripped from the worker payload and rejoined in main.
  .policy_meta_cols <- c(
    "policy_base", "policy_display", "plain_language_definition",
    "policy_family", "aggregation_method", "aggregation_definition",
    "candidate_pool", "candidate_pool_definition",
    "display_name", "grouping_key", "metric_key", "requested_policy"
  )
  .anchor_meta_cols <- c(
    "anchor_species", "anchor_family", "anchor_group", "anchor_group_block", "is_reference"
  )

  # Build the anchor-metadata lookup from candidate_models_ (no computation needed).
  .spc_col_nm <- benchmark_field(config_values, "species")
  anchor_meta_lookup <- data.frame(
    anchor_model_id = as.character(candidate_models_[[id_col_nm]]),
    anchor_species = as.character(candidate_models_[[.spc_col_nm]]),
    anchor_family = benchmark_optional_field_values(candidate_models_, config_values, "family"),
    is_reference = as.character(candidate_models_[[id_col_nm]]) %in% ref_ids,
    stringsAsFactors = FALSE
  )
  anchor_meta_lookup$anchor_group <- ifelse(
    anchor_meta_lookup$is_reference, "reference", "candidate"
  )
  anchor_meta_lookup$anchor_group_block <- if (
    !is.null(group_block_col) && group_block_col %in% names(candidate_models_)
  ) {
    as.character(candidate_models_[[group_block_col]])
  } else {
    NA_character_
  }
  anchor_meta_lookup <- unique(anchor_meta_lookup)
  rm(.spc_col_nm)

  # Build the policy-metadata lookup by running policy_fun once on the first
  # admissible anchor. Policy metadata is identical across all anchors.
  policy_meta_lookup <- NULL
  for (.pi in seq_len(min(10L, total_anchors))) {
    .anchor_row <- candidate_models_[.pi, , drop = FALSE]
    .peobj <- benchmark_cached_anchor_eval(
      cached_anchor_evals = cached_anchor_evals,
      anchor_row = .anchor_row,
      config = config_values
    )
    if (is.null(.peobj)) {
      .peobj <- tryCatch(
        screen_one_anchor_admissibility(
          anchor_row = .anchor_row,
          candidate_models = candidate_models_,
          config = config_values,
          registry_path = registry_path,
          sim_obj = sim_obj,
          dist_obj = dist_obj,
          candidate_models_scored = candidate_models_scored
        ),
        error = function(e) NULL
      )
    }
    if (is.null(.peobj)) next
    .pargs <- list(
      eval_obj = .peobj,
      policies = policies,
      policy_params = policy_params_,
      policy_path = policy_path
    )
    .pargs <- .pargs[names(.pargs) %in% names(formals(policy_fun))]
    .ptbl <- tryCatch(do.call(policy_fun, .pargs), error = function(e) NULL)
    if (!is.null(.ptbl) && nrow(.ptbl) > 0L) {
      .keep <- intersect(
        c("policy", "equation_branch_filter", .policy_meta_cols), names(.ptbl)
      )
      policy_meta_lookup <- unique(as.data.frame(.ptbl[, .keep, drop = FALSE]))
      break
    }
  }
  suppressWarnings(rm(.pi, .peobj, .pargs, .ptbl))
  needs_ordination_context <- benchmark_needs_ordination_context(
    policy_meta_lookup = policy_meta_lookup,
    policies = policies,
    policy_path = policy_path
  )
  policy_execution_plan <- NULL
  if ("execution_plan" %in% names(formals(policy_fun))) {
    # Build the policy registry, branch expansion, and per-policy parameters
    # once so the anchor loop does not repeat the same setup work.
    policy_execution_plan <- build_policy_execution_plan(
      policies = policies,
      policy_params = policy_params_,
      policy_path = policy_path
    )
  }
  compiled_policy_plan <- if (identical(engine_, "cpp")) {
    compile_policy_execution_plan_cpp(policy_execution_plan)
  } else {
    NULL
  }
  if (!isTRUE(needs_ordination_context)) {
    model_scores_ <- NULL
    species_lookup_ <- NULL
  }

  worker_strip_cols <- c(
    .anchor_meta_cols,
    if (!is.null(policy_meta_lookup)) .policy_meta_cols else character(0L),
    "validation_scheme", "error_abs_log", "valid_prediction"
  )

  # Helper: rejoin stripped perf-table metadata after result collection.
  .rejoin_perf <- function(tbl, pmeta, ameta) {
    if (is.null(tbl) || nrow(tbl) == 0L) {
      return(tbl)
    }
    if (!is.null(pmeta) && nrow(pmeta) > 0L && "policy" %in% names(tbl)) {
      add_cols <- setdiff(names(pmeta), c("policy", "equation_branch_filter", names(tbl)))
      if (length(add_cols) > 0L) {
        tbl <- dplyr::left_join(
          tbl,
          pmeta[, c("policy", "equation_branch_filter", add_cols), drop = FALSE],
          by = c("policy", "equation_branch_filter")
        )
      }
    }
    if (!is.null(ameta) && nrow(ameta) > 0L && "anchor_model_id" %in% names(tbl)) {
      add_cols <- setdiff(names(ameta), c("anchor_model_id", names(tbl)))
      if (length(add_cols) > 0L) {
        tbl <- dplyr::left_join(
          tbl,
          ameta[, c("anchor_model_id", add_cols), drop = FALSE],
          by = "anchor_model_id"
        )
      }
    }
    tbl
  }

  progress_step <- max(1L, ceiling(total_anchors / 20L))
  t_start <- if (isTRUE(progress)) Sys.time() else NULL

  .progress_msg <- function(done) {
    elapsed_s <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    rate <- done / max(elapsed_s, 0.001)
    eta_s <- if (done < total_anchors) (total_anchors - done) / rate else 0
    eta_str <- if (done >= total_anchors) "done" else if (eta_s < 60) sprintf("%.0fs", eta_s) else if (eta_s < 3600) sprintf("%.1fmin", eta_s / 60) else sprintf("%.1fh", eta_s / 3600)
    tsb_message(sprintf(
      "Progress: %d/%d (%d%%) | elapsed: %.0fs | %.2f anchors/s | ETA: ~%s",
      done, total_anchors, as.integer(100 * done / total_anchors),
      elapsed_s, rate, eta_str
    ))
  }

  # Evaluate every candidate as an anchor under both the full donor pool and
  # the leave-one-species-out donor pool, optionally in parallel.
  if (workers_ <= 1L) {
    for (i in seq_len(total_anchors)) {
      anchor_row <- candidate_models_[i, , drop = FALSE]
      anchor_cache_id <- as.character(anchor_row[[id_col_nm]][[1]])
      cached_anchor_result <- .read_anchor_cache(anchor_cache_id)
      if (!is.null(cached_anchor_result)) {
        .append_anchor_result(cached_anchor_result)
        if (isTRUE(progress) && (i %% progress_step == 0L || i == total_anchors)) {
          .progress_msg(i)
        }
        next
      }
      base_eval_obj <- benchmark_cached_anchor_eval(
        cached_anchor_evals = cached_anchor_evals,
        anchor_row = anchor_row,
        config = config_values
      )
      if (is.null(base_eval_obj)) {
        base_eval_obj <- tryCatch(
          screen_one_anchor_admissibility(
            anchor_row = anchor_row,
            candidate_models = candidate_models_,
            config = config_values,
            registry_path = registry_path,
            sim_obj = sim_obj,
            dist_obj = dist_obj,
            candidate_models_scored = candidate_models_scored
          ),
          error = function(e) {
            if (isTRUE(progress)) {
              tsb_message(
                "Anchor ", i, " skipped (admissibility error): ",
                conditionMessage(e)
              )
            }
            NULL
          }
        )
      }
      if (is.null(base_eval_obj)) {
        if (isTRUE(progress) && (i %% progress_step == 0L || i == total_anchors)) {
          .progress_msg(i)
        }
        next
      }
      # Resolve the optional ordination context once per anchor only when one
      # of the requested policies actually requires it.
      ordination_info_now <- NULL
      if (isTRUE(needs_ordination_context)) {
        ordination_info_now <- resolve_ordination_info(
          anchor_row = anchor_row,
          model_scores = model_scores_,
          species_lookup = species_lookup_,
          config = config_values
        )
      }

      bench_obj <- if (isTRUE(run_pseudo_anchor)) {
        benchmark_one_anchor(
          anchor_row = anchor_row,
          candidate_models = candidate_models_,
          policy_fun = policy_fun,
          curve_fun = if (isTRUE(include_ts_error)) curve_fun else NULL,
          model_scores = model_scores_,
          species_lookup = species_lookup_,
          reference_ids = ref_ids,
          policies = policies,
          policy_params = policy_params_,
          policy_path = policy_path,
          policy_execution_plan = policy_execution_plan,
          engine = engine_,
          compiled_policy_plan = compiled_policy_plan,
          sim_obj = sim_obj,
          dist_obj = dist_obj,
          candidate_models_scored = candidate_models_scored,
          eval_obj = base_eval_obj,
          config = config_values,
          registry_path = registry_path,
          scheme = "pseudo_anchor",
          species_block = FALSE,
          ordination_info = ordination_info_now,
          resolve_ordination = isTRUE(needs_ordination_context)
        )
      } else {
        NULL
      }

      .seq_strip <- function(tbl) {
        if (is.null(tbl) || length(worker_strip_cols) == 0L) {
          return(tbl)
        }
        tbl[, !names(tbl) %in% worker_strip_cols, drop = FALSE]
      }

      if (!is.null(bench_obj)) {
        perf_rows[[length(perf_rows) + 1]] <- .seq_strip(bench_obj$perf)
        feat_rows[[length(feat_rows) + 1]] <- bench_obj$features
        err_rows[[length(err_rows) + 1]] <- bench_obj$ts_error
      }

      sb_obj <- if (isTRUE(run_species_block)) {
        benchmark_one_anchor(
          anchor_row = anchor_row,
          candidate_models = candidate_models_,
          policy_fun = policy_fun,
          curve_fun = NULL,
          model_scores = model_scores_,
          species_lookup = species_lookup_,
          reference_ids = ref_ids,
          policies = policies,
          policy_params = policy_params_,
          policy_path = policy_path,
          policy_execution_plan = policy_execution_plan,
          engine = engine_,
          compiled_policy_plan = compiled_policy_plan,
          sim_obj = sim_obj,
          dist_obj = dist_obj,
          candidate_models_scored = candidate_models_scored,
          eval_obj = base_eval_obj,
          config = config_values,
          registry_path = registry_path,
          scheme = config_values$species_block_label,
          species_block = TRUE,
          ordination_info = ordination_info_now,
          resolve_ordination = isTRUE(needs_ordination_context)
        )
      } else {
        NULL
      }

      if (!is.null(sb_obj)) {
        sb_perf_rows[[length(sb_perf_rows) + 1]] <- .seq_strip(sb_obj$perf)
        sb_feat_rows[[length(sb_feat_rows) + 1]] <- sb_obj$features
      }

      if (isTRUE(run_group_block)) {
        gb_obj <- benchmark_one_anchor(
          anchor_row = anchor_row,
          candidate_models = candidate_models_,
          policy_fun = policy_fun,
          curve_fun = NULL,
          model_scores = model_scores_,
          species_lookup = species_lookup_,
          reference_ids = ref_ids,
          policies = policies,
          policy_params = policy_params_,
          policy_path = policy_path,
          policy_execution_plan = policy_execution_plan,
          engine = engine_,
          compiled_policy_plan = compiled_policy_plan,
          sim_obj = sim_obj,
          dist_obj = dist_obj,
          candidate_models_scored = candidate_models_scored,
          eval_obj = base_eval_obj,
          config = config_values,
          registry_path = registry_path,
          scheme = group_block_label,
          species_block = FALSE,
          group_block_col = group_block_col,
          ordination_info = ordination_info_now,
          resolve_ordination = isTRUE(needs_ordination_context)
        )

        if (!is.null(gb_obj)) {
          gb_perf_rows[[length(gb_perf_rows) + 1]] <- .seq_strip(gb_obj$perf)
          gb_feat_rows[[length(gb_feat_rows) + 1]] <- gb_obj$features
        }
      }

      .write_anchor_cache(
        anchor_id = anchor_cache_id,
        anchor_result = list(
          bench = bench_obj,
          species_block = sb_obj,
          group_block = if (isTRUE(run_group_block)) gb_obj else NULL
        )
      )

      if (isTRUE(progress) && (i %% progress_step == 0L || i == total_anchors)) {
        .progress_msg(i)
      }
    }
  } else {
    bench_env <- environment()
    cluster_export_vars <- c(
      "candidate_models_",
      "cached_anchor_evals",
      "curve_fun",
      "model_scores_",
      "species_lookup_",
      "ref_ids",
      "policies",
      "policy_params_",
      "policy_path",
      "policy_execution_plan",
      "compiled_policy_plan",
      "engine_",
      "sim_obj",
      "dist_obj",
      "candidate_models_scored",
      "config_values",
      "registry_path",
      "include_ts_error",
      "run_pseudo_anchor",
      "run_species_block",
      "run_group_block",
      "group_block_col",
      "group_block_label",
      "needs_ordination_context",
      "worker_strip_cols",
      "policy_meta_lookup",
      "anchor_meta_lookup"
    )
    .build_cluster <- function() {
      cl <- initialize_parallel_cluster(workers = workers_)
      tsb_cluster_export(cl, cluster_export_vars, envir = bench_env)
      cl
    }
    cluster_obj <- .build_cluster()
    .kill_cluster_processes <- function(cl) {
      try(parallel::stopCluster(cl), silent = TRUE)
      self_pid <- Sys.getpid()
      child_pids <- tryCatch(
        as.integer(trimws(system2(
          "ps", c("--no-headers", "-o", "pid", "--ppid", as.character(self_pid)),
          stdout = TRUE, stderr = FALSE
        ))),
        error = function(e) integer(0)
      )
      for (pid in child_pids[is.finite(child_pids)]) {
        try(tools::pskill(pid, signal = tools::SIGKILL), silent = TRUE)
      }
      invisible(NULL)
    }
    on.exit(.kill_cluster_processes(cluster_obj), add = TRUE)

    if (isTRUE(progress)) {
      tsb_message("Policy benchmark running in parallel with ", workers_, " workers.")
    }

    chunk_step <- as.integer(workers_)
    pending_anchor_ids <- integer(0)
    processed <- 0L
    for (i in seq_len(total_anchors)) {
      anchor_cache_id <- as.character(candidate_models_[[id_col_nm]][[i]])
      cached_anchor_result <- .read_anchor_cache(anchor_cache_id)
      if (!is.null(cached_anchor_result)) {
        .append_anchor_result(cached_anchor_result)
        processed <- processed + 1L
      } else {
        pending_anchor_ids <- c(pending_anchor_ids, i)
      }
    }
    if (isTRUE(progress) && processed > 0L) {
      .progress_msg(processed)
    }

    worker_fun <- function(i) {
      screen_one_anchor_admissibility <- utils::getFromNamespace("screen_one_anchor_admissibility", "tsbiomass")
      benchmark_cached_anchor_eval <- utils::getFromNamespace("benchmark_cached_anchor_eval", "tsbiomass")
      resolve_ordination_info <- utils::getFromNamespace("resolve_ordination_info", "tsbiomass")
      benchmark_one_anchor <- utils::getFromNamespace("benchmark_one_anchor", "tsbiomass")
      evaluate_policies <- utils::getFromNamespace("evaluate_policies", "tsbiomass")
      anchor_row <- candidate_models_[i, , drop = FALSE]
      cached_eval_obj <- benchmark_cached_anchor_eval(
        cached_anchor_evals = cached_anchor_evals,
        anchor_row = anchor_row,
        config = config_values
      )
      adm_result <- if (!is.null(cached_eval_obj)) {
        list(obj = cached_eval_obj, err = NULL)
      } else {
        tryCatch(
          list(obj = screen_one_anchor_admissibility(
            anchor_row = anchor_row,
            candidate_models = candidate_models_,
            config = config_values,
            registry_path = registry_path,
            sim_obj = sim_obj,
            dist_obj = dist_obj,
            candidate_models_scored = candidate_models_scored
          ), err = NULL),
          error = function(e) list(obj = NULL, err = conditionMessage(e))
        )
      }
      if (is.null(adm_result$obj)) {
        return(list(
          bench = NULL, species_block = NULL, group_block = NULL,
          adm_error = adm_result$err, anchor_index = i
        ))
      }
      base_eval_obj <- adm_result$obj
      ordination_info_now <- NULL
      if (isTRUE(needs_ordination_context)) {
        ordination_info_now <- resolve_ordination_info(
          anchor_row = anchor_row,
          model_scores = model_scores_,
          species_lookup = species_lookup_,
          config = config_values
        )
      }

      bench_obj <- if (isTRUE(run_pseudo_anchor)) {
        benchmark_one_anchor(
          anchor_row = anchor_row,
          candidate_models = candidate_models_,
          policy_fun = evaluate_policies,
          curve_fun = if (isTRUE(include_ts_error)) curve_fun else NULL,
          model_scores = model_scores_,
          species_lookup = species_lookup_,
          reference_ids = ref_ids,
          policies = policies,
          policy_params = policy_params_,
          policy_path = policy_path,
          policy_execution_plan = policy_execution_plan,
          engine = engine_,
          compiled_policy_plan = compiled_policy_plan,
          sim_obj = sim_obj,
          dist_obj = dist_obj,
          candidate_models_scored = candidate_models_scored,
          eval_obj = base_eval_obj,
          config = config_values,
          registry_path = registry_path,
          scheme = "pseudo_anchor",
          species_block = FALSE,
          ordination_info = ordination_info_now,
          resolve_ordination = isTRUE(needs_ordination_context)
        )
      } else {
        NULL
      }

      sb_obj <- if (isTRUE(run_species_block)) {
        benchmark_one_anchor(
          anchor_row = anchor_row,
          candidate_models = candidate_models_,
          policy_fun = evaluate_policies,
          curve_fun = NULL,
          model_scores = model_scores_,
          species_lookup = species_lookup_,
          reference_ids = ref_ids,
          policies = policies,
          policy_params = policy_params_,
          policy_path = policy_path,
          policy_execution_plan = policy_execution_plan,
          engine = engine_,
          compiled_policy_plan = compiled_policy_plan,
          sim_obj = sim_obj,
          dist_obj = dist_obj,
          candidate_models_scored = candidate_models_scored,
          eval_obj = base_eval_obj,
          config = config_values,
          registry_path = registry_path,
          scheme = config_values$species_block_label,
          species_block = TRUE,
          ordination_info = ordination_info_now,
          resolve_ordination = isTRUE(needs_ordination_context)
        )
      } else {
        NULL
      }

      gb_obj <- NULL
      if (isTRUE(run_group_block)) {
        gb_obj <- benchmark_one_anchor(
          anchor_row = anchor_row,
          candidate_models = candidate_models_,
          policy_fun = evaluate_policies,
          curve_fun = NULL,
          model_scores = model_scores_,
          species_lookup = species_lookup_,
          reference_ids = ref_ids,
          policies = policies,
          policy_params = policy_params_,
          policy_path = policy_path,
          policy_execution_plan = policy_execution_plan,
          engine = engine_,
          compiled_policy_plan = compiled_policy_plan,
          sim_obj = sim_obj,
          dist_obj = dist_obj,
          candidate_models_scored = candidate_models_scored,
          eval_obj = base_eval_obj,
          config = config_values,
          registry_path = registry_path,
          scheme = group_block_label,
          species_block = FALSE,
          group_block_col = group_block_col,
          ordination_info = ordination_info_now,
          resolve_ordination = isTRUE(needs_ordination_context)
        )
      }

      .strip_cols <- function(tbl) {
        if (is.null(tbl) || length(worker_strip_cols) == 0L) {
          return(tbl)
        }
        drop <- names(tbl) %in% worker_strip_cols
        tbl[, !drop, drop = FALSE]
      }
      if (!is.null(bench_obj)) bench_obj$perf <- .strip_cols(bench_obj$perf)
      if (!is.null(sb_obj)) sb_obj$perf <- .strip_cols(sb_obj$perf)
      if (!is.null(gb_obj)) gb_obj$perf <- .strip_cols(gb_obj$perf)

      list(
        bench = bench_obj,
        species_block = sb_obj,
        group_block = gb_obj,
        anchor_cache_id = as.character(anchor_row[[config_values$fields$model_id]][[1]])
      )
    }

    # Anchor IDs are logged before dispatch since a cluster death can corrupt
    # the console connection before the stop() message reaches it.
    inflight_log_path <- if (!is.null(cache_shard_dir)) {
      file.path(dirname(cache_shard_dir), "benchmark_inflight_anchors.log")
    } else {
      file.path(tempdir(), "benchmark_inflight_anchors.log")
    }

    .run_chunk <- function(ids) {
      if (length(ids) == 0L) {
        return(list())
      }
      chunk_anchor_ids <- as.character(candidate_models_[[id_col_nm]][ids])
      cat(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " DISPATCHING ", length(ids),
        " anchor(s): ", paste(chunk_anchor_ids, collapse = ", "), "\n",
        file = inflight_log_path, append = TRUE, sep = ""
      )
      result <- tryCatch(
        parallel::parLapplyLB(cluster_obj, as.list(ids), worker_fun),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        .kill_cluster_processes(cluster_obj)
        stop(
          "Policy benchmark worker cluster died (", conditionMessage(result),
          ") while processing anchor(s): ", paste(chunk_anchor_ids, collapse = ", "),
          ". This list was also written to ", inflight_log_path,
          " before dispatch, in case this message does not make it out intact.",
          call. = FALSE
        )
      }
      cat(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " COMPLETED ", length(ids),
        " anchor(s): ", paste(chunk_anchor_ids, collapse = ", "), "\n",
        file = inflight_log_path, append = TRUE, sep = ""
      )
      result
    }

    chunk_index <- split(
      pending_anchor_ids,
      ceiling(seq_along(pending_anchor_ids) / chunk_step)
    )
    for (ids in chunk_index) {
      ids <- as.integer(ids)
      if (length(ids) == 0L) {
        next
      }
      chunk_results <- .run_chunk(ids)

      for (one_result in chunk_results) {
        if (!is.null(one_result$bench)) {
          .append_anchor_result(one_result)
          .write_anchor_cache(
            anchor_id = one_result$anchor_cache_id %||% NA_character_,
            anchor_result = one_result[c("bench", "species_block", "group_block")]
          )
        } else if (isTRUE(progress) && !is.null(one_result$adm_error)) {
          tsb_message(
            "Anchor ", one_result$anchor_index, " skipped (admissibility error): ",
            one_result$adm_error
          )
        }
        processed <- processed + 1L
      }
      if (isTRUE(progress)) .progress_msg(processed)
    }
  }

  perf_tbl <- dplyr::bind_rows(perf_rows)
  feat_tbl <- dplyr::bind_rows(feat_rows)
  err_tbl <- dplyr::bind_rows(err_rows)
  sb_perf_tbl <- dplyr::bind_rows(sb_perf_rows)
  sb_feat_tbl <- dplyr::bind_rows(sb_feat_rows)
  gb_perf_tbl <- dplyr::bind_rows(gb_perf_rows)
  gb_feat_tbl <- dplyr::bind_rows(gb_feat_rows)

  if (nrow(perf_tbl) > 0L) perf_tbl$validation_scheme <- "pseudo_anchor"
  if (nrow(sb_perf_tbl) > 0L) sb_perf_tbl$validation_scheme <- config_values$species_block_label
  if (nrow(gb_perf_tbl) > 0L) gb_perf_tbl$validation_scheme <- group_block_label

  if (ncol(perf_tbl) == 0L) {
    warning(
      "Policy benchmark produced no rows. All ", total_anchors, " anchor(s) ",
      "failed admissibility screening. Check that 'candidate_models' has valid ",
      "study length data (study_length_min / study_length_max) and that the ",
      "similarity traits in the config match the available columns. ",
      "Set 'progress = TRUE' and inspect per-anchor diagnostic messages.",
      call. = FALSE
    )
  }

  perf_tbl <- .rejoin_perf(perf_tbl, policy_meta_lookup, anchor_meta_lookup)
  sb_perf_tbl <- .rejoin_perf(sb_perf_tbl, policy_meta_lookup, anchor_meta_lookup)
  gb_perf_tbl <- .rejoin_perf(gb_perf_tbl, policy_meta_lookup, anchor_meta_lookup)

  .recompute_perf_derived <- function(tbl) {
    if (is.null(tbl) || nrow(tbl) == 0L || !"multiplier_pred" %in% names(tbl)) {
      return(tbl)
    }
    tbl$error_abs_log <- abs(log(tbl$multiplier_pred))
    tbl$valid_prediction <- is.finite(tbl$multiplier_pred) & tbl$multiplier_pred > 0
    tbl
  }
  perf_tbl <- .recompute_perf_derived(perf_tbl)
  sb_perf_tbl <- .recompute_perf_derived(sb_perf_tbl)
  gb_perf_tbl <- .recompute_perf_derived(gb_perf_tbl)

  if (isTRUE(include_ts_error) && is.function(curve_fun)) {
    err_tbl <- if (identical(engine_, "cpp")) {
      build_benchmark_ts_error_table_cpp(
        candidate_models = candidate_models_,
        policy_perf = perf_tbl,
        config = config_values
      )
    } else {
      build_benchmark_ts_error_table(
        candidate_models = candidate_models_,
        policy_perf = perf_tbl,
        config = config_values,
        progress = progress
      )
    }
    err_tbl <- err_tbl |>
      dplyr::mutate(validation_scheme = "pseudo_anchor") |>
      dplyr::select(dplyr::any_of(c(
        "anchor_model_id", "anchor_species", "policy", "equation_branch_filter",
        "policy_slope_len", "policy_intercept_len",
        "local_min_combined_distance",
        "local_weighted_mean_combined_distance",
        "local_effective_support",
        "local_mean_length_overlap",
        "local_mean_depth_overlap",
        "validation_scheme", "u", "ts_error", "log_sigma_residual"
      )))
  }

  if (nrow(err_tbl) > 0L && "anchor_model_id" %in% names(err_tbl) &&
    "anchor_species" %in% names(anchor_meta_lookup) &&
    !"anchor_species" %in% names(err_tbl)) {
    err_tbl <- dplyr::left_join(
      err_tbl,
      anchor_meta_lookup[, c("anchor_model_id", "anchor_species"), drop = FALSE],
      by = "anchor_model_id"
    )
  }

  result <- list(
    engine = engine_,
    policy_perf = perf_tbl,
    anchor_features = feat_tbl,
    best_policy = bind_best_policy_rows(perf_tbl),
    policy_ts_error = err_tbl,
    species_block_perf = sb_perf_tbl,
    species_block_features = sb_feat_tbl,
    species_block_best = bind_best_policy_rows(sb_perf_tbl),
    group_block_perf = gb_perf_tbl,
    group_block_features = gb_feat_tbl,
    group_block_best = bind_best_policy_rows(gb_perf_tbl),
    group_block_col = group_block_col,
    group_block_label = group_block_label
  )

  # Persist the in-memory benchmark object only when the caller requested a
  # cache path.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, cache_path)
  }

  result
}


#' List context columns retained in meta-policy training tables
#'
#' @param tbl Data frame or tibble.
#' @param include_outcome Logical scalar. When `TRUE`, include `.outcome` when
#'   present.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
meta_policy_context_columns <- function(tbl,
                                        include_outcome = FALSE) {
  tbl <- tibble::as_tibble(tbl)
  out <- names(tbl)[vapply(names(tbl), function(name) {
    if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
      return(FALSE)
    }
    grepl("^(anchor_|reference_anchor_)", name) ||
      name %in% c(
        "validation_scheme", "valid_prediction", ".split_group", "fold_id",
        # Policy identity is not a model feature (it is decomposed into
        # candidate_pool/aggregation_method/equation_branch_filter for that
        # purpose), but it must survive as bookkeeping so downstream
        # calibration (post-selection local-lookup cascade) can condition on
        # the selected strategy instead of silently receiving NA.
        "policy", "policy_family", "candidate_pool", "aggregation_method"
      )
  }, logical(1))]
  if (isTRUE(include_outcome) && ".outcome" %in% names(tbl)) {
    out <- c(out, ".outcome")
  }
  unique(out)
}

#' Resolve the grouping column for cross-fitted meta-policy learning
#'
#' @param policy_perf Policy-performance table.
#' @param group_col Optional requested grouping column.
#'
#' @return Character scalar.
#'
#' @keywords internal
#' @noRd
resolve_meta_policy_group_col <- function(policy_perf,
                                          group_col = NULL) {
  policy_perf <- tibble::as_tibble(policy_perf)
  if (!is.null(group_col)) {
    if (!is.character(group_col) || length(group_col) != 1 || !nzchar(group_col)) {
      stop("'group_col' must be NULL or a single non-empty column name.", call. = FALSE)
    }
    if (!group_col %in% names(policy_perf)) {
      stop(sprintf("Column '%s' was not found in 'policy_perf'.", group_col), call. = FALSE)
    }
    return(group_col)
  }

  # Meta-policy predictions are deployed to reference species that were not
  # observed as anchors during training. Keep every result from an anchor
  # species in the same outer fold so cross-fit performance has that meaning.
  if ("anchor_species" %in% names(policy_perf)) {
    anchor_species <- as.character(policy_perf$anchor_species)
    available_species <- anchor_species[!is.na(anchor_species) & nzchar(anchor_species)]
    if (dplyr::n_distinct(available_species) >= 2L) {
      return("anchor_species")
    }
  }

  context_cols <- meta_policy_context_columns(policy_perf)
  context_cols <- setdiff(context_cols, c("validation_scheme", "valid_prediction", ".split_group", "fold_id"))
  context_cols <- context_cols[!grepl("(^|_)model_id(_chr)?$", context_cols)]
  if (length(context_cols) == 0) {
    stop(
      "No default meta-policy grouping column could be inferred. Supply 'group_col' explicitly.",
      call. = FALSE
    )
  }
  context_cols[[1]]
}

#' Test whether one column is eligible as a default meta-policy feature
#'
#' @param name Column name.
#' @param x Column vector.
#'
#' @return Logical scalar.
#'
#' @keywords internal
#' @noRd
is_default_meta_policy_feature <- function(name,
                                           x) {
  excluded_names <- c(
    "valid_prediction",
    ".outcome",
    ".outcome_raw",
    ".outcome_was_clipped",
    ".split_group",
    "fold_id"
  )
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    return(FALSE)
  }
  if (grepl("^(anchor_|reference_anchor_)", name) ||
    name %in% c("validation_scheme", "valid_prediction", ".split_group", "fold_id")) {
    return(FALSE)
  }
  if (name %in% excluded_names) {
    return(FALSE)
  }
  if (grepl("^meta_", name) ||
    grepl("^selected_", name) ||
    grepl("^post_selection_", name) ||
    grepl("^is_selected$", name) ||
    grepl("^dominated_", name) ||
    grepl("^multiplier_(pred|lo|hi)$", name) ||
    grepl("(^|_)error(_|$)", name) ||
    grepl("^relative_error$", name) ||
    grepl("^ts_error$", name) ||
    grepl("_covered$", name) ||
    grepl("^empirical_coverage$", name)) {
    return(FALSE)
  }
  if (!(is.numeric(x) || is.integer(x) || is.logical(x) || is.character(x) || is.factor(x))) {
    return(FALSE)
  }
  vals <- if (is.factor(x)) as.character(x) else x
  vals <- vals[!is.na(vals)]
  if (length(unique(vals)) < 2L) {
    return(FALSE)
  }
  if (is.character(x) || is.factor(x) || is.logical(x)) {
    n_unique <- length(unique(as.character(vals)))
    if (n_unique > max(50L, floor(0.5 * length(vals)))) {
      return(FALSE)
    }
    if (mean(nchar(as.character(vals)), na.rm = TRUE) > 80) {
      return(FALSE)
    }
  }
  TRUE
}

#' Default meta-policy feature columns
#'
#' Selects a generalized set of candidate feature columns from the supplied
#' policy table without relying on a analysis-specific hard-coded policy roster.
#'
#' @param tbl Policy-performance table.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
default_meta_policy_features <- function(tbl) {
  tbl <- tibble::as_tibble(tbl)
  names(tbl)[vapply(
    names(tbl),
    function(nm) is_default_meta_policy_feature(nm, tbl[[nm]]),
    logical(1)
  )]
}

#' Remove policy-identity and post-hoc leakage fields from meta-policy features
#'
#' The score learner is meant to estimate anchor-conditional multiplier
#' replacement error from realized donor geometry, support, and benchmarked
#' behavior. It should not be allowed to memorize broad policy-class priors or
#' borrow direct uncertainty-width quantities that turn the score model into a
#' disguised interval selector.
#'
#' @param feature_cols Character vector of proposed feature columns.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
sanitize_meta_policy_feature_cols <- function(feature_cols) {
  feature_cols <- unique(as.character(feature_cols %||% character()))
  drop_cols <- c(
    "policy",
    "policy_base",
    "policy_display",
    "display_name",
    "requested_policy",
    "candidate_pool",
    "aggregation_method",
    "policy_family",
    "candidate_pool_definition",
    "aggregation_definition",
    "plain_language_definition",
    "specificity_rank",
    "acceptable_global",
    "acceptable_bootstrap",
    "bootstrap_prob_best",
    "bootstrap_prob_within_threshold",
    "bootstrap_median_rank",
    "best_mean_species_median_abs_log",
    "one_se_threshold",
    "species_block_median_abs_log_error",
    "mean_species_median_abs_log",
    "median_abs_log",
    "policy_slope_len",
    "policy_slope_len_se",
    "policy_slope_len_lo_95",
    "policy_slope_len_hi_95",
    "policy_intercept_len",
    "policy_intercept_len_se",
    "policy_intercept_len_lo_95",
    "policy_intercept_len_hi_95",
    "policy_sigma_bs_mean",
    "policy_is_constructed_ensemble",
    "q_abs_log",
    "q_abs_log_conformal",
    "q_abs_log_total",
    "q_abs_log_structural",
    "n",
    "uncertainty_cost_log_width",
    "interval_log_width",
    "multiplier_interval_width",
    "multiplier_interval_score",
    "coefficient_slope_q95",
    "coefficient_intercept_q95",
    "coefficient_stability_n",
    "realized_donor_fingerprint",
    "selected_policy",
    "selected_policy_display",
    "selection_tier",
    "meta_post_selection_multiplier_lo",
    "meta_post_selection_multiplier_hi",
    "meta_post_selection_interval_log_width"
  )
  setdiff(feature_cols, drop_cols)
}

#' Normalize meta-policy feature columns into a reusable blueprint
#'
#' @param data Input training or prediction table.
#' @param feature_cols Feature columns to retain.
#' @param blueprint Optional preprocessing blueprint to reuse at prediction
#'   time.
#'
#' @return A list with normalized feature data and the preprocessing blueprint.
#'
#' @keywords internal
#' @noRd
meta_policy_blueprint <- function(data,
                                  feature_cols,
                                  blueprint = NULL) {
  data <- tibble::as_tibble(data)
  out <- data[, feature_cols, drop = FALSE]

  if (is.null(blueprint)) {
    numeric_medians <- list()
    factor_levels <- list()
    missing_indicators <- character()
    for (nm in feature_cols) {
      x <- out[[nm]]
      if (is.character(x) || is.logical(x) || is.factor(x)) {
        vals <- as.character(x)
        vals[is.na(vals) | !nzchar(vals)] <- "missing"
        levels_now <- sort(unique(vals))
        if (!"missing" %in% levels_now) {
          levels_now <- c(levels_now, "missing")
        }
        out[[nm]] <- factor(vals, levels = levels_now)
        factor_levels[[nm]] <- levels_now
      } else {
        vals <- suppressWarnings(as.numeric(x))
        miss <- !is.finite(vals)
        med <- stats::median(vals[is.finite(vals)], na.rm = TRUE)
        if (!is.finite(med)) {
          med <- 0
        }
        vals[miss] <- med
        out[[nm]] <- vals
        numeric_medians[[nm]] <- med
        if (any(miss)) {
          miss_nm <- paste0(nm, "_missing")
          out[[miss_nm]] <- miss
          missing_indicators <- c(missing_indicators, miss_nm)
        }
      }
    }
    blueprint <- list(
      feature_cols = feature_cols,
      numeric_medians = numeric_medians,
      factor_levels = factor_levels,
      missing_indicators = missing_indicators
    )
  } else {
    for (nm in feature_cols) {
      if (nm %in% names(blueprint$factor_levels)) {
        vals <- as.character(out[[nm]])
        vals[is.na(vals) | !nzchar(vals)] <- "missing"
        vals[!vals %in% blueprint$factor_levels[[nm]]] <- "missing"
        out[[nm]] <- factor(vals, levels = blueprint$factor_levels[[nm]])
      } else {
        vals <- suppressWarnings(as.numeric(out[[nm]]))
        miss <- !is.finite(vals)
        vals[miss] <- blueprint$numeric_medians[[nm]] %||% 0
        out[[nm]] <- vals
      }
    }
    for (miss_nm in blueprint$missing_indicators %||% character()) {
      base_nm <- sub("_missing$", "", miss_nm)
      vals <- if (base_nm %in% names(data)) {
        suppressWarnings(as.numeric(data[[base_nm]]))
      } else {
        rep(NA_real_, nrow(data))
      }
      out[[miss_nm]] <- !is.finite(vals)
    }
  }

  list(data = out, blueprint = blueprint)
}

#' Build a model matrix for meta-policy learners
#'
#' @param frame Preprocessed feature frame.
#' @param terms_obj Optional terms object to reuse.
#' @param x_columns Optional model-matrix columns to enforce.
#'
#' @return A list with the numeric model matrix and its terms object.
#'
#' @keywords internal
#' @noRd
meta_policy_model_matrix <- function(frame,
                                     terms_obj = NULL,
                                     x_columns = NULL) {
  frame <- tibble::as_tibble(frame)
  if (is.null(terms_obj)) {
    terms_obj <- stats::terms(stats::as.formula("~ ."), data = frame)
  }
  x <- stats::model.matrix(terms_obj, data = frame)
  if ("(Intercept)" %in% colnames(x)) {
    x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  }
  if (!is.null(x_columns)) {
    missing_cols <- setdiff(x_columns, colnames(x))
    if (length(missing_cols) > 0) {
      add <- matrix(0, nrow = nrow(x), ncol = length(missing_cols))
      colnames(add) <- missing_cols
      x <- cbind(x, add)
    }
    x <- x[, x_columns, drop = FALSE]
  }
  list(x = x, terms = terms_obj)
}

#' Invert the modeled meta-policy outcome transformation
#'
#' @param pred Numeric prediction vector on the modeled scale.
#' @param outcome_transform Outcome transform used during fitting.
#' @param prediction_cap Upper cap applied after inversion.
#'
#' @return Numeric vector on the natural outcome scale.
#'
#' @keywords internal
#' @noRd
inverse_meta_policy_outcome <- function(pred,
                                        outcome_transform,
                                        prediction_cap = Inf) {
  pred <- as.numeric(pred)
  if (identical(outcome_transform, "log1p")) {
    max_eta <- log(.Machine$double.xmax)
    pred <- pmin(pred, max_eta)
    pred <- pmax(0, expm1(pred))
  }
  prediction_cap <- suppressWarnings(as.numeric(prediction_cap)[[1]])
  if (!is.finite(prediction_cap) || prediction_cap <= 0) {
    prediction_cap <- .Machine$double.xmax
  }
  pred <- pmin(pred, prediction_cap)
  pred[!is.finite(pred)] <- prediction_cap
  pred
}

#' Derive a safe upper cap for meta-policy score predictions
#'
#' @param y Outcome vector on the natural scale.
#' @param multiplier Positive multiplicative cap applied to the observed
#'   maximum.
#'
#' @return Positive numeric scalar.
#'
#' @keywords internal
#' @noRd
meta_policy_prediction_cap <- function(y,
                                       multiplier = 10) {
  y <- suppressWarnings(as.numeric(y))
  max_y <- suppressWarnings(max(y[is.finite(y)], na.rm = TRUE))
  if (!is.finite(max_y) || max_y <= 0) {
    return(1)
  }
  max(1, multiplier * max_y)
}

#' Build grouped fold assignments
#'
#' @param groups Exchangeable grouping labels.
#' @param n_folds Number of folds.
#' @param seed Integer seed.
#'
#' @return Integer vector or `NULL` when grouped folds are not feasible.
#'
#' @keywords internal
#' @noRd
grouped_foldid <- function(groups,
                           n_folds = 5L,
                           seed = NULL) {
  groups <- as.character(groups)
  unique_groups <- sort(unique(stats::na.omit(groups)))
  if (length(unique_groups) < 2L) {
    return(NULL)
  }
  n_folds <- min(as.integer(n_folds), length(unique_groups))
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  fold_tbl <- tibble::tibble(
    group = sample(unique_groups, length(unique_groups), replace = FALSE),
    fold = rep(seq_len(n_folds), length.out = length(unique_groups))
  )
  fold_tbl$fold[match(groups, fold_tbl$group)]
}

#' Build row-wise fold assignments
#'
#' @param n Number of rows.
#' @param n_folds Number of folds.
#' @param seed Integer seed.
#'
#' @return Integer vector or `NULL`.
#'
#' @keywords internal
#' @noRd
row_foldid <- function(n,
                       n_folds = 5L,
                       seed = NULL) {
  n <- as.integer(n)
  if (!is.finite(n) || n < 2L) {
    return(NULL)
  }
  n_folds <- min(as.integer(n_folds), n)
  if (!is.finite(n_folds) || n_folds < 2L) {
    return(NULL)
  }
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  sample(rep(seq_len(n_folds), length.out = n), n, replace = FALSE)
}

#' Resolve the available super-learner base methods
#'
#' @param methods Optional requested method names.
#' @param method_settings Optional learner method-settings list.
#'
#' @return Character vector of valid installed method names.
#'
#' @keywords internal
#' @noRd
available_meta_policy_super_methods <- function(methods = NULL,
                                                method_settings = NULL) {
  catalog <- meta_policy_method_catalog(method_settings = method_settings)
  valid <- catalog$methods
  methods <- methods %||% catalog$default_super_methods
  methods <- unique(stringr::str_squish(as.character(unlist(methods, use.names = FALSE))))
  methods <- methods[!is.na(methods) & nzchar(methods)]
  unknown <- setdiff(methods, valid)
  if (length(unknown) > 0) {
    stop(
      sprintf("Unknown super-learner base method(s): %s", paste(unknown, collapse = ", ")),
      call. = FALSE
    )
  }
  if (length(methods) == 0) {
    methods <- "glm"
  }
  methods
}

#' Default learner-specific settings for meta-policy models
#'
#' Returns the shared method-specific settings used by both the selection and
#' conditional-uncertainty learners unless the config YAML overrides them.
#'
#' @return Named list of method-specific settings.
#'
#' @keywords internal
#' @noRd
meta_policy_method_defaults_file <- function(defaults_path = NULL) {
  if (!is.null(defaults_path)) {
    defaults_path <- as.character(defaults_path)
    if (length(defaults_path) != 1L || is.na(defaults_path) || !nzchar(defaults_path)) {
      stop("'method_defaults_path' must be one non-empty file path.", call. = FALSE)
    }
    return(defaults_path)
  }
  system.file("learner_method_defaults.json", package = "tsbiomass", mustWork = TRUE)
}

read_meta_policy_method_defaults_registry <- function(defaults_path = NULL) {
  path <- meta_policy_method_defaults_file(defaults_path)
  if (!file.exists(path)) {
    stop(sprintf("Learner method defaults registry does not exist: %s", path), call. = FALSE)
  }
  registry <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!is.list(registry) || !is.list(registry$families) || !is.list(registry$methods)) {
    stop("Learner method defaults registry must contain named 'families' and 'methods' objects.", call. = FALSE)
  }
  registry
}

meta_policy_method_family_map <- function() {
  c(
    glm = "glm",
    glm_ridge = "glm_penalized",
    glm_lasso = "glm_penalized",
    glm_elastic = "glm_penalized",
    qreg = "qreg",
    gam = "gam",
    lmm = "lmm",
    rpart = "rpart",
    rf = "rf",
    xgboost = "xgboost",
    mars = "mars",
    bart = "bart",
    xbart = "bart",
    wsbart = "bart",
    vfbart = "bart",
    rebart = "bart",
    knn = "knn",
    cubist = "cubist",
    svr = "svr",
    qrf = "qrf",
    gpr = "gpr",
    mean = "mean"
  )
}

default_meta_policy_method_settings <- function(defaults_path = NULL) {
  registry <- read_meta_policy_method_defaults_registry(defaults_path)
  family_map <- meta_policy_method_family_map()
  defaults <- stats::setNames(vector("list", length(family_map)), names(family_map))
  for (method_name in names(family_map)) {
    defaults[[method_name]] <- merge_config_sections(
      registry$families[[family_map[[method_name]]]] %||% list(),
      registry$methods[[method_name]] %||% list()
    )
  }
  defaults
}

#' Merge learner-specific settings onto the package defaults
#'
#' @param method_settings Optional user-supplied method settings.
#'
#' @return Named list of merged method settings.
#'
#' @keywords internal
#' @noRd
meta_policy_method_settings_defaults_path <- function(method_settings = NULL,
                                                      defaults_path = NULL) {
  if (!is.null(defaults_path)) {
    return(defaults_path)
  }
  if (is.list(method_settings)) {
    method_settings$method_defaults_path %||%
      method_settings$.method_defaults_path %||%
      NULL
  } else {
    NULL
  }
}

strip_meta_policy_method_settings_controls <- function(method_settings = NULL) {
  if (is.list(method_settings)) {
    method_settings$method_defaults_path <- NULL
    method_settings$.method_defaults_path <- NULL
  }
  method_settings
}

attach_meta_policy_method_defaults_path <- function(method_settings = NULL,
                                                    defaults_path = NULL) {
  if (is.null(defaults_path)) {
    return(method_settings)
  }
  if (is.null(method_settings)) {
    method_settings <- list()
  }
  method_settings$.method_defaults_path <- defaults_path
  method_settings
}

normalize_meta_policy_method_settings <- function(method_settings = NULL,
                                                  defaults_path = NULL) {
  defaults_path <- meta_policy_method_settings_defaults_path(method_settings, defaults_path)
  method_settings <- strip_meta_policy_method_settings_controls(method_settings)
  defaults <- default_meta_policy_method_settings(defaults_path = defaults_path)
  defaults <- attach_meta_policy_method_defaults_path(defaults, defaults_path)
  if (is.null(method_settings)) {
    return(defaults)
  }
  if (!is.list(method_settings)) {
    stop("'method_settings' must be a named list when supplied.", call. = FALSE)
  }

  attach_meta_policy_method_defaults_path(
    merge_config_sections(defaults, method_settings),
    defaults_path
  )
}

meta_policy_method_default_arguments <- function(method,
                                                 defaults_path = NULL) {
  method <- as.character(method %||% "")
  if (length(method) != 1L || !nzchar(method)) {
    return(list())
  }
  defaults <- default_meta_policy_method_settings(defaults_path = defaults_path)
  defaults[[method]] %||% list()
}

#' Catalog the supported meta-policy learner methods
#'
#' Builds the public method namespace used by both single-method fits and the
#' super-learner library.
#'
#' @param method_settings Optional learner method-settings list.
#'
#' @return A list with `methods`, `default_super_methods`, and `specs`.
#' @keywords internal
#' @noRd
meta_policy_method_catalog <- function(method_settings = NULL) {
  method_settings <- normalize_meta_policy_method_settings(method_settings)
  family_map <- meta_policy_method_family_map()
  specs <- stats::setNames(vector("list", length(family_map)), names(family_map))
  for (method_name in names(family_map)) {
    specs[[method_name]] <- list(
      family = family_map[[method_name]],
      base = method_name,
      variant = NULL
    )
  }

  for (base_name in names(method_settings)) {
    settings <- method_settings[[base_name]]
    if (!is.list(settings)) {
      next
    }
    variants <- settings$variants %||% NULL
    if (!is.list(variants) || length(variants) == 0L) {
      next
    }
    base_spec <- specs[[base_name]] %||% NULL
    if (!is.list(base_spec)) {
      next
    }
    variant_names <- names(variants)
    variant_names <- variant_names[!is.na(variant_names) & nzchar(variant_names)]
    for (variant_name in variant_names) {
      method_name <- paste(base_name, variant_name, sep = "_")
      specs[[method_name]] <- list(
        family = base_spec$family,
        base = base_name,
        variant = variant_name
      )
    }
  }

  default_super_methods <- c(
    "glm",
    "xgboost"
  )
  default_super_methods <- unique(default_super_methods[default_super_methods %in% names(specs)])

  list(
    methods = names(specs),
    default_super_methods = default_super_methods,
    specs = specs
  )
}

#' List the Super Learner learners available in tsbiomass
#'
#' Prints the learner method names available to the package's Super Learners.
#'
#' @return A character vector of the available learner names.
#'
#' @examples
#' list_learners()
#'
#' @export
list_learners <- function() {
  print(sort(meta_policy_method_catalog()$methods))
}

#' Resolve one public meta-policy method name
#'
#' @param method One public learner method name.
#' @param method_settings Optional learner method-settings list.
#'
#' @return A list with `name`, `family`, and `variant`.
#' @keywords internal
#' @noRd
meta_policy_method_spec <- function(method,
                                    method_settings = NULL) {
  method <- stringr::str_squish(as.character(method %||% ""))[[1]]
  catalog <- meta_policy_method_catalog(method_settings = method_settings)
  spec <- catalog$specs[[method]] %||% NULL
  if (!is.list(spec)) {
    stop(sprintf("Unknown meta-policy learner method: %s", method), call. = FALSE)
  }
  c(list(name = method), spec)
}

#' Resolve method-specific fit arguments for one learner family
#'
#' @param method Learner method.
#' @param method_settings Shared method settings list.
#'
#' @return Named list of arguments to pass into the learner fit.
#'
#' @keywords internal
#' @noRd
meta_policy_method_arguments <- function(method,
                                         method_settings = NULL) {
  defaults_path <- meta_policy_method_settings_defaults_path(method_settings)
  method_settings <- normalize_meta_policy_method_settings(method_settings, defaults_path = defaults_path)
  method_spec <- meta_policy_method_spec(method, method_settings = method_settings)
  method_defaults <- meta_policy_method_default_arguments(method_spec$base, defaults_path = defaults_path)
  compact_nulls <- function(x) {
    x[!vapply(x, is.null, logical(1))]
  }
  effective_method_settings <- function() {
    family_cfg <- method_settings[[method_spec$family]] %||% list()
    base_cfg <- method_settings[[method_spec$base]] %||% list()
    variant_cfg <- if (!is.null(method_spec$variant)) {
      base_cfg$variants[[method_spec$variant]] %||% list()
    } else {
      list()
    }
    family_cfg$variants <- NULL
    base_cfg$variants <- NULL
    merge_config_sections(
      merge_config_sections(family_cfg, base_cfg),
      variant_cfg
    )
  }

  if (identical(method_spec$family, "glm_penalized")) {
    method_cfg <- effective_method_settings()
    method_cfg$alpha <- switch(method_spec$name,
      glm_ridge = 0,
      glm_lasso = 1,
      glm_elastic = method_cfg$alpha %||% method_defaults$alpha %||% 0.25,
      method_cfg$alpha %||% method_defaults$alpha %||% 0.25
    )
    return(merge_config_sections(method_defaults, method_cfg))
  }
  if (identical(method_spec$family, "qreg")) {
    quantreg_cfg <- effective_method_settings()
    return(compact_nulls(list(
      tau = as.numeric(quantreg_cfg$tau %||% 0.50),
      method = as.character(quantreg_cfg$fit_method %||% "fn")
    )))
  }
  if (identical(method_spec$family, "gam")) {
    gam_cfg <- effective_method_settings()
    out <- list()
    if ("fit_method" %in% names(gam_cfg)) {
      out$method <- gam_cfg$fit_method
    }
    if ("select_terms" %in% names(gam_cfg)) {
      out$select <- isTRUE(gam_cfg$select_terms)
    }
    return(compact_nulls(out))
  }
  if (identical(method_spec$family, "lmm")) {
    lmm_cfg <- effective_method_settings()
    random_intercept <- as.character(unlist(
      lmm_cfg$random_intercept %||% character(0),
      use.names = FALSE
    ))
    if (length(random_intercept) != 1L || is.na(random_intercept[[1]]) ||
      !nzchar(stringr::str_squish(random_intercept[[1]]))) {
      stop(
        "LMM method settings require one explicit `random_intercept` column.",
        call. = FALSE
      )
    }
    return(compact_nulls(list(
      fit_method = as.character(lmm_cfg$fit_method %||% "REML"),
      random_intercept = stringr::str_squish(random_intercept[[1]])
    )))
  }
  if (identical(method_spec$family, "rpart")) {
    rpart_cfg <- effective_method_settings()
    out <- list(
      control = do.call(
        rpart::rpart.control,
        compact_nulls(list(
          cp = as.numeric(rpart_cfg$cp %||% 0.01),
          minsplit = as.integer(rpart_cfg$minsplit %||% 20L),
          minbucket = as.integer(rpart_cfg$minbucket %||% 7L),
          maxdepth = as.integer(rpart_cfg$maxdepth %||% 30L),
          # `cp` is configured directly and the resulting cptable is never used
          # for post-hoc pruning, so rpart.control's default of xval = 10 would
          # spend ten internal cross-validations per fit building a table
          # nothing reads. Competitor and surrogate splits are diagnostics only,
          # and the meta-policy design matrix is complete, so surrogates never
          # fire either.
          xval = as.integer(rpart_cfg$xval %||% 0L),
          maxcompete = as.integer(rpart_cfg$maxcompete %||% 0L),
          maxsurrogate = as.integer(rpart_cfg$maxsurrogate %||% 0L)
        ))
      )
    )
    return(compact_nulls(out))
  }
  if (identical(method_spec$family, "rf")) {
    ranger_cfg <- effective_method_settings()
    out <- list(
      num.trees = as.integer(ranger_cfg$num_trees %||% 500L),
      mtry = ranger_cfg$mtry %||% NULL,
      min.node.size = as.integer(ranger_cfg$min_node_size %||% 5L),
      max.depth = if (is.null(ranger_cfg$max_depth)) {
        NULL
      } else {
        as.integer(ranger_cfg$max_depth)
      },
      sample.fraction = as.numeric(ranger_cfg$sample_fraction %||% 1),
      replace = isTRUE(ranger_cfg$replace %||% TRUE),
      respect.unordered.factors = as.character(
        ranger_cfg$respect_unordered_factors %||% "order"
      )
    )
    return(compact_nulls(out))
  }
  if (identical(method_spec$family, "xgboost")) {
    xgboost_cfg <- effective_method_settings()
    params <- list(
      objective = "reg:squarederror",
      eta = as.numeric(xgboost_cfg$eta %||% 0.30),
      max_depth = as.integer(xgboost_cfg$max_depth %||% 6L),
      min_child_weight = as.numeric(xgboost_cfg$min_child_weight %||% 1),
      subsample = as.numeric(xgboost_cfg$subsample %||% 1),
      colsample_bytree = as.numeric(xgboost_cfg$colsample_bytree %||% 1),
      lambda = as.numeric(xgboost_cfg$lambda %||% 1),
      alpha = as.numeric(xgboost_cfg$alpha %||% 0),
      nthread = as.integer(xgboost_cfg$nthread %||% 1L)
    )
    # `early_stopping_rounds` stays NULL by default, which keeps the fixed
    # `nrounds` schedule. When set, the fitter holds out `validation_fraction`
    # of the training rows to stop against.
    return(compact_nulls(list(
      params = params,
      nrounds = as.integer(xgboost_cfg$nrounds %||% 100L),
      early_stopping_rounds = if (is.null(xgboost_cfg$early_stopping_rounds)) {
        NULL
      } else {
        as.integer(xgboost_cfg$early_stopping_rounds)
      },
      validation_fraction = as.numeric(xgboost_cfg$validation_fraction %||% 0.2),
      verbose = 0
    )))
  }
  if (identical(method_spec$family, "mars")) {
    earth_cfg <- effective_method_settings()
    # Map the YAML-facing names onto earth::earth() argument names. Dropping
    # NULLs lets earth fall back to its own GCV-driven defaults (e.g. when
    # nprune is unset the backward pass selects the term count automatically,
    # and when nk is unset earth sizes the forward pass from the column count).
    # `nk` caps forward-pass terms and `fast_k` caps the queue of parent terms
    # it considers, which are the two levers on forward-pass cost; fast_k = 0
    # disables the fast-MARS heuristic entirely.
    return(compact_nulls(list(
      degree = as.integer(earth_cfg$degree %||% 2L),
      penalty = as.numeric(earth_cfg$penalty %||% 3),
      nk = if (is.null(earth_cfg$nk)) NULL else as.integer(earth_cfg$nk),
      nprune = if (is.null(earth_cfg$nprune)) NULL else as.integer(earth_cfg$nprune),
      fast.k = if (is.null(earth_cfg$fast_k)) NULL else as.integer(earth_cfg$fast_k),
      pmethod = as.character(earth_cfg$pmethod %||% "backward")
    )))
  }
  if (identical(method_spec$family, "bart")) {
    bart_cfg <- effective_method_settings()
    if (length(bart_cfg) == 0L) {
      stop(sprintf("No BART defaults were defined for learner '%s'.", method_spec$name), call. = FALSE)
    }
    return(compact_nulls(list(
      num_trees = as.integer(bart_cfg$num_trees %||% 75L),
      num_gfr = as.integer(bart_cfg$num_gfr),
      num_burnin = as.integer(bart_cfg$num_burnin),
      num_mcmc = as.integer(bart_cfg$num_mcmc),
      alpha = as.numeric(bart_cfg$alpha %||% 0.95),
      beta = as.numeric(bart_cfg$beta %||% 2),
      min_samples_leaf = as.integer(bart_cfg$min_samples_leaf %||% 5L),
      max_depth = as.integer(bart_cfg$max_depth %||% 10L),
      keep_gfr = isTRUE(bart_cfg$keep_gfr %||% TRUE),
      variance_forest = isTRUE(bart_cfg$variance_forest),
      variance_forest_num_trees = if (isTRUE(bart_cfg$variance_forest)) {
        as.integer(bart_cfg$variance_forest_num_trees %||% 50L)
      } else {
        NULL
      },
      random_effects = isTRUE(bart_cfg$random_effects),
      random_effects_group = if (isTRUE(bart_cfg$random_effects)) {
        as.character(bart_cfg$random_effects_group %||% ".split_group")
      } else {
        NULL
      }
    )))
  }
  if (identical(method_spec$family, "knn")) {
    knn_cfg <- effective_method_settings()
    return(compact_nulls(list(
      k = as.integer(knn_cfg$k %||% 10L)
    )))
  }
  if (identical(method_spec$family, "cubist")) {
    cubist_cfg <- effective_method_settings()
    return(compact_nulls(list(
      committees = as.integer(cubist_cfg$committees %||% 1L),
      neighbors = as.integer(cubist_cfg$neighbors %||% 0L)
    )))
  }
  if (identical(method_spec$family, "svr")) {
    svr_cfg <- effective_method_settings()
    return(compact_nulls(list(
      C = as.numeric(svr_cfg$C %||% 1),
      epsilon = as.numeric(svr_cfg$epsilon %||% 0.1),
      kernel = svr_cfg$kernel %||% NULL,
      sigma = if (is.null(svr_cfg$sigma)) NULL else as.numeric(svr_cfg$sigma),
      max_train_rows = if (is.null(svr_cfg$max_train_rows)) NULL else as.integer(svr_cfg$max_train_rows)
    )))
  }
  if (identical(method_spec$family, "qrf")) {
    qrf_cfg <- effective_method_settings()
    return(compact_nulls(list(
      num.trees = as.integer(qrf_cfg$num_trees %||% 500L),
      mtry = qrf_cfg$mtry %||% NULL,
      min.node.size = as.integer(qrf_cfg$min_node_size %||% 10L),
      max.depth = if (is.null(qrf_cfg$max_depth)) NULL else as.integer(qrf_cfg$max_depth),
      sample.fraction = as.numeric(qrf_cfg$sample_fraction %||% 1),
      replace = isTRUE(qrf_cfg$replace %||% TRUE),
      quantile = as.numeric(qrf_cfg$quantile %||% 0.9)
    )))
  }
  if (identical(method_spec$family, "gpr")) {
    gpr_cfg <- effective_method_settings()
    return(compact_nulls(list(
      var = as.numeric(gpr_cfg$var %||% 0.001),
      kernel = gpr_cfg$kernel %||% NULL,
      sigma = if (is.null(gpr_cfg$sigma)) NULL else as.numeric(gpr_cfg$sigma),
      max_train_rows = if (is.null(gpr_cfg$max_train_rows)) NULL else as.integer(gpr_cfg$max_train_rows)
    )))
  }
  # The "mean" family (and any family without explicit arguments) falls through
  # to an empty argument list; its fit needs nothing beyond the outcome column.
  list()
}

#' Resolve kernlab kernel settings for meta-policy learners
#'
#' Converts configured kernel learner arguments into the `kernel` and `kpar`
#' values expected by kernlab. A configured `sigma` disables kernlab's automatic
#' `sigest` path for RBF kernels.
#'
#' @param fit_arguments Method-specific argument list.
#'
#' @return List with `kernel` and `kpar` entries.
#' @keywords internal
#' @noRd
resolve_meta_policy_kernlab_settings <- function(fit_arguments) {
  kernel <- stringr::str_squish(as.character(fit_arguments$kernel %||% "rbfdot"))[[1]]
  kernel <- tolower(kernel)
  if (kernel %in% c("rdfbot", "rbf", "gaussian")) {
    kernel <- "rbfdot"
  }
  sigma <- fit_arguments$sigma %||% NULL
  if (!is.null(sigma)) {
    sigma <- as.numeric(sigma)
    if (length(sigma) != 1L || !is.finite(sigma) || sigma <= 0) {
      stop("'sigma' for kernlab learners must be one finite positive number.", call. = FALSE)
    }
    return(list(kernel = kernel, kpar = list(sigma = sigma)))
  }
  list(kernel = kernel, kpar = "automatic")
}

#' Sample training rows for exact kernel meta-policy learners
#'
#' Selects a deterministic row subset when `max_train_rows` is configured. This
#' bounds exact kernel memory use while still predicting every validation/test
#' row downstream.
#'
#' @param n Number of available training rows.
#' @param max_train_rows Optional maximum number of rows to retain.
#' @param seed Optional integer seed.
#'
#' @return Integer row indices.
#' @keywords internal
#' @noRd
sample_meta_policy_kernel_training_rows <- function(n,
                                                    max_train_rows = NULL,
                                                    seed = NULL) {
  n <- as.integer(n)
  if (is.null(max_train_rows)) {
    return(seq_len(n))
  }
  max_train_rows <- as.integer(max_train_rows)
  if (length(max_train_rows) != 1L || is.na(max_train_rows) || max_train_rows < 1L) {
    stop("'max_train_rows' for kernlab learners must be one positive integer.", call. = FALSE)
  }
  if (n <= max_train_rows) {
    return(seq_len(n))
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  sort(sample.int(n, max_train_rows))
}

#' Sample held-out rows for early-stopping meta-policy learners
#'
#' Selects a deterministic evaluation-row subset for learners that need an
#' internal validation set (xgboost early stopping). Returns no rows whenever
#' the split cannot leave at least one row on each side, which callers treat as
#' "early stopping is not available for this fit".
#'
#' @param n Number of available training rows.
#' @param fraction Share of rows to hold out, strictly between 0 and 1.
#' @param seed Optional integer seed.
#'
#' @return Integer row indices, possibly empty.
#' @keywords internal
#' @noRd
sample_meta_policy_holdout_rows <- function(n,
                                            fraction = 0.2,
                                            seed = NULL) {
  n <- as.integer(n)
  fraction <- as.numeric(fraction %||% 0.2)
  if (length(fraction) != 1L || !is.finite(fraction) || fraction <= 0 || fraction >= 1) {
    stop(
      "'validation_fraction' for early-stopping learners must be one number strictly between 0 and 1.",
      call. = FALSE
    )
  }
  if (length(n) != 1L || is.na(n)) {
    return(integer(0))
  }
  n_holdout <- as.integer(floor(n * fraction))
  if (n_holdout < 1L || (n - n_holdout) < 1L) {
    return(integer(0))
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  sort(sample.int(n, n_holdout))
}

#' Make categorical levels safe for Cubist's names file
#'
#' Cubist writes each predictor's levels into a Quinlan-format names file, where
#' `:`, `;`, `|`, and `,` are structural delimiters. The package escapes column
#' names (`escapes(names(varData))`) and outcome levels (`escapes(levels(y))`),
#' but `Cubist:::QuinlanAttributes.factor()` renders predictor levels with a
#' plain `paste()` and never calls `escapes()`. A level carrying a delimiter
#' therefore truncates the attribute definition, after which the data file no
#' longer agrees with the declared levels and Cubist's C parser reads past its
#' line buffer. On Linux glibc catches that and aborts the whole process
#' (`*** buffer overflow detected ***`), taking the worker with it; on Windows it
#' surfaces as an opaque `exit(1)`.
#'
#' This matters because multi-label trait fields are ordinary here: basin sets
#' arrive as `"Mediterranean Sea;Atlantic Ocean"`, survey bodies as
#' `"A; B"`. Substituting the delimiters keeps the levels legible in
#' Cubist's rules, which is the reason to prefer it over opaque surrogate labels.
#' Both the training and prediction frames must pass through this, or their
#' levels will not agree.
#'
#' @param x Character vector of levels.
#'
#' @return Character vector of the same length, delimiter-free and still unique.
#' @keywords internal
#' @noRd
sanitize_cubist_levels <- function(x) {
  clean <- gsub("[;:|,]", "_", as.character(x))
  # Substitution can collide two distinct levels ("a;b" and "a:b" both become
  # "a_b"); assigning duplicated levels back onto a factor would silently merge
  # them into one, so keep them distinct.
  if (anyDuplicated(clean)) {
    clean <- make.unique(clean, sep = "_")
  }
  clean
}

#' Make a data frame safe for Cubist
#'
#' Applies [sanitize_cubist_levels()] to every categorical column. Character
#' columns are converted to factors first so the level set is explicit and the
#' training and prediction frames sanitize identically.
#'
#' @param df Data frame of predictors.
#'
#' @return The data frame with delimiter-free categorical levels.
#' @keywords internal
#' @noRd
sanitize_cubist_frame <- function(df) {
  df <- as.data.frame(df)
  for (nm in names(df)) {
    if (is.character(df[[nm]]) || is.logical(df[[nm]])) {
      df[[nm]] <- factor(df[[nm]])
    }
    if (is.factor(df[[nm]])) {
      levels(df[[nm]]) <- sanitize_cubist_levels(levels(df[[nm]]))
    }
  }
  df
}

#' Resolve one mixed-effects grouping column for the meta-policy learner
#'
#' @param training_data Prepared learner training table.
#' @param random_intercept Exactly one explicitly configured random-intercept
#'   column.
#'
#' @return A list with the selected grouping column and normalized factor.
#'
#' @keywords internal
#' @noRd
resolve_meta_policy_lmm_group <- function(training_data,
                                          random_intercept) {
  training_data <- tibble::as_tibble(training_data)
  random_intercept <- as.character(unlist(
    random_intercept %||% character(0),
    use.names = FALSE
  ))
  if (length(random_intercept) != 1L || is.na(random_intercept[[1]]) ||
    !nzchar(stringr::str_squish(random_intercept[[1]]))) {
    stop(
      "The mixed-effects meta-policy learner requires one explicit `random_intercept` column.",
      call. = FALSE
    )
  }
  group_col <- stringr::str_squish(random_intercept[[1]])
  if (!group_col %in% names(training_data)) {
    stop(
      sprintf(
        "Configured LMM grouping column '%s' is absent from the training data.",
        group_col
      ),
      call. = FALSE
    )
  }

  group_values <- as.character(training_data[[group_col]])
  group_values[is.na(group_values) | !nzchar(group_values)] <- "missing"
  if (length(unique(group_values)) < 2L) {
    stop(
      sprintf(
        "Configured LMM grouping column '%s' has fewer than two observed levels.",
        group_col
      ),
      call. = FALSE
    )
  }

  list(
    group_col = group_col,
    group_factor = factor(group_values)
  )
}

#' Fit one stochtree BART variant for the meta-policy learner
#'
#' Localizes every stochtree API call so the five BART variants
#' (bart/xbart/wsbart/vfbart/rebart) map onto a single `stochtree::bart()`
#' invocation. The variant is already baked into `args` (num_gfr/num_mcmc plus the
#' variance-forest and random-effect flags) by `meta_policy_method_arguments()`.
#'
#' @param x Numeric model matrix of features.
#' @param y Numeric (already outcome-transformed) response vector.
#' @param args Resolved BART fit arguments from `meta_policy_method_arguments()`.
#' @param training_data Prepared learner training table (supplies the rebart
#'   grouping column).
#' @param seed Optional integer seed.
#'
#' @return A list with the fitted `model`, the resolved `rfx_group_col`
#'   (`NA_character_` unless rebart), and the training `rfx_levels`.
#'
#' @keywords internal
#' @noRd
fit_stochtree_bart <- function(x, y, args, training_data, seed) {
  # --- stochtree argument groups (verify names against installed stochtree) ---
  general_params <- list(
    random_seed = if (is.null(seed)) -1L else as.integer(seed),
    standardize = TRUE,
    # keep_gfr retains the grow-from-root draws alongside the MCMC draws.
    # stochtree ignores it when num_mcmc = 0, so xbart keeps its GFR sweeps
    # regardless (they are its only posterior). It therefore only bites on
    # warm-start schedules that run GFR *and* MCMC, where leaving it on mixes
    # pre-convergence sweeps into the reported posterior. Default to FALSE to
    # match stochtree rather than inverting it.
    keep_gfr = isTRUE(args$keep_gfr %||% FALSE),
    verbose = FALSE
  )
  mean_forest_params <- list(
    num_trees = as.integer(args$num_trees %||% 75L),
    alpha = as.numeric(args$alpha %||% 0.95),
    beta = as.numeric(args$beta %||% 2),
    min_samples_leaf = as.integer(args$min_samples_leaf %||% 5L),
    max_depth = as.integer(args$max_depth %||% 10L)
  )
  # A variance forest with num_trees = 0 is disabled; vfbart turns it on.
  variance_forest_params <- list(
    num_trees = if (isTRUE(args$variance_forest)) {
      as.integer(args$variance_forest_num_trees %||% 50L)
    } else {
      0L
    }
  )
  random_effects_params <- if (isTRUE(args$random_effects)) {
    list(model_spec = "intercept_only")
  } else {
    list()
  }

  # rebart: resolve an additive group random effect from the configured column.
  rfx_group_ids <- NULL
  rfx_levels <- NULL
  rfx_group_col <- NA_character_
  if (isTRUE(args$random_effects)) {
    grp <- resolve_meta_policy_lmm_group(
      training_data = training_data,
      random_intercept = args$random_effects_group %||% ".split_group"
    )
    rfx_group_col <- grp$group_col
    rfx_levels <- levels(grp$group_factor)
    # stochtree expects 1-based contiguous integer group ids.
    rfx_group_ids <- as.integer(grp$group_factor)
  }

  model <- withCallingHandlers(
    stochtree::bart(
      X_train = as.matrix(x),
      y_train = as.numeric(y),
      num_gfr = as.integer(args$num_gfr %||% 0L),
      num_burnin = as.integer(args$num_burnin %||% 100L),
      num_mcmc = as.integer(args$num_mcmc %||% 200L),
      general_params = general_params,
      mean_forest_params = mean_forest_params,
      variance_forest_params = variance_forest_params,
      random_effects_params = random_effects_params,
      rfx_group_ids_train = rfx_group_ids
    ),
    warning = function(w) {
      msg <- conditionMessage(w)
      is_gfr_diagnostic <- grepl(
        paste(
          "grow-from-root",
          "GFR algorithm",
          "fewer than 20 unique values",
          "repeated observations",
          "ratio of unique to overall observations",
          "appear to be binary but are currently treated",
          "Global error variance will not be sampled with a heteroskedasticity forest",
          sep = "|"
        ),
        msg
      )
      if (is_gfr_diagnostic) {
        invokeRestart("muffleWarning")
      }
    }
  )

  model_json <- tryCatch(
    stochtree::saveBARTModelToJsonString(model),
    error = function(e) {
      stop(
        "Failed to serialize stochtree BART model with stochtree::saveBARTModelToJsonString(): ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  list(
    model = model,
    model_json = model_json,
    rfx_group_col = rfx_group_col,
    rfx_levels = rfx_levels
  )
}

restore_stochtree_bart_fit <- function(object) {
  model_json <- object$fit_json %||% object$model_json %||% NULL
  if (!is.null(model_json) && length(model_json) == 1L && !is.na(model_json) && nzchar(model_json)) {
    return(stochtree::createBARTModelFromJsonString(model_json))
  }
  object$fit
}

#' Predict from a fitted stochtree BART variant
#'
#' Localizes stochtree's `predict()` call and normalizes its return value to a
#' single posterior-mean vector. Handles rebart's group ids and fails explicitly
#' when prediction contains random-effect levels not observed during training.
#'
#' @param object Fitted `tsb_meta_policy_learner` (BART family).
#' @param x Numeric prediction model matrix aligned to training columns.
#' @param prediction_tbl New policy table (supplies the rebart grouping column).
#'
#' @return Numeric posterior-mean prediction vector.
#'
#' @keywords internal
#' @noRd
predict_stochtree_bart <- function(object, x, prediction_tbl) {
  x <- as.matrix(x)
  rfx_group_ids <- NULL
  seen_group <- NULL
  rfx_group_col <- as.character(object$rfx_group_col %||% NA_character_)[[1]]
  if (!is.na(rfx_group_col) && nzchar(rfx_group_col)) {
    rfx_levels <- object$rfx_levels %||% character(0)
    group_values <- if (rfx_group_col %in% names(prediction_tbl)) {
      as.character(prediction_tbl[[rfx_group_col]])
    } else {
      rep(NA_character_, nrow(x))
    }
    ids <- match(group_values, rfx_levels)
    seen_group <- !is.na(ids)
    if (any(!seen_group)) {
      unseen_values <- unique(group_values[!seen_group])
      unseen_values <- unseen_values[!is.na(unseen_values) & nzchar(unseen_values)]
      example_values <- head(unseen_values, 5L)
      stop(
        sprintf(
          paste(
            "RE-BART prediction encountered %d row(s) with random-effect group levels",
            "not present during training for column '%s'. A true random-effects",
            "prediction cannot estimate held-out group effects. Unseen level example(s): %s"
          ),
          sum(!seen_group),
          rfx_group_col,
          if (length(example_values) > 0L) paste(example_values, collapse = ", ") else "<missing>"
        ),
        call. = FALSE
      )
    }
    rfx_group_ids <- as.integer(ids)
  }

  reduce_draws <- function(draws) {
    yhat <- if (is.list(draws)) {
      draws$y_hat %||% draws$mean_forest_predictions %||% draws[[1]]
    } else {
      draws
    }
    if (is.null(dim(yhat))) as.numeric(yhat) else as.numeric(rowMeans(yhat))
  }

  if (is.null(rfx_group_ids)) {
    return(reduce_draws(stats::predict(restore_stochtree_bart_fit(object), x)))
  }

  reduce_draws(stats::predict(
    restore_stochtree_bart_fit(object),
    x,
    rfx_group_ids = rfx_group_ids
  ))
}

#' Predict from a fitted KNN-regression meta-policy learner
#'
#' Reapplies the stored training column centers/scales to the new design matrix
#' (KNN distances are scale-sensitive) and returns the `FNN::knn.reg` local mean.
#'
#' @param object Fitted `tsb_meta_policy_learner` (knn family).
#' @param x Numeric prediction model matrix aligned to training columns.
#'
#' @return Numeric prediction vector.
#'
#' @keywords internal
#' @noRd
predict_knn_regression <- function(object, x) {
  train_x <- object$fit$x
  train_y <- object$fit$y
  x_std <- scale(x, center = object$fit$center, scale = object$fit$scale)
  # Never request more neighbours than training rows.
  k <- min(as.integer(object$fit$k %||% 10L), nrow(train_x))
  reg <- FNN::knn.reg(
    train = train_x,
    test = x_std,
    y = train_y,
    k = max(1L, k)
  )
  as.numeric(reg$pred)
}

#' Resolve configured LMM random-intercept columns that must survive pruning
#'
#' @param methods Active learner method names.
#' @param method_settings Shared method settings.
#'
#' @return Character vector of configured grouping columns, or `character()`
#'   when no active method belongs to the LMM family.
#'
#' @keywords internal
#' @noRd
active_meta_policy_lmm_random_intercepts <- function(methods,
                                                     method_settings = NULL) {
  methods <- unique(as.character(unlist(methods %||% character(), use.names = FALSE)))
  methods <- methods[!is.na(methods) & nzchar(methods)]
  lmm_methods <- methods[vapply(methods, function(method) {
    spec <- try(
      meta_policy_method_spec(method, method_settings = method_settings),
      silent = TRUE
    )
    !inherits(spec, "try-error") && identical(spec$family, "lmm")
  }, logical(1))]
  if (length(lmm_methods) == 0L) {
    return(character())
  }

  unique(unlist(lapply(lmm_methods, function(method) {
    args <- meta_policy_method_arguments(method, method_settings = method_settings)
    as.character(args$random_intercept)
  }), use.names = FALSE))
}

#' Parse a metalearner loss name for the meta-policy super learner
#'
#' Accepts `"squared_error"`, `"absolute_error"`, or a pinball loss written as
#' `"pinball_q<NN>"` with two digits (`"pinball_q90"` is the 0.90 quantile),
#' matching the `<base>_q<NN>` convention the learner variants already use.
#' Parsing deliberately does not judge whether the combiner supports the loss,
#' so callers that merely carry a loss name around keep working; that judgement
#' belongs to `fit_super_learner_weights()`, which is the only place the loss is
#' actually used.
#'
#' @param loss Loss name.
#'
#' @return A list with the normalized `name`, the `kind`
#'   (`"squared_error"`, `"absolute_error"`, or `"pinball"`), and `tau`
#'   (`NA_real_` for non-pinball losses).
#'
#' @keywords internal
#' @noRd
parse_super_learner_loss <- function(loss) {
  loss <- stringr::str_squish(as.character(loss %||% "squared_error"))
  if (length(loss) != 1L || is.na(loss[[1]]) || !nzchar(loss[[1]])) {
    stop("'metalearner_loss' must be exactly one non-empty loss name.", call. = FALSE)
  }
  loss <- loss[[1]]
  if (loss %in% c("squared_error", "absolute_error")) {
    return(list(name = loss, kind = loss, tau = NA_real_))
  }
  tau_digits <- stringr::str_match(loss, "^pinball_q([0-9]{2})$")[1, 2]
  if (!is.na(tau_digits)) {
    tau <- as.numeric(tau_digits) / 100
    if (tau <= 0 || tau >= 1) {
      stop(
        sprintf(
          "Pinball loss '%s' implies tau = %s, which must fall strictly between 0 and 1.",
          loss,
          format(tau)
        ),
        call. = FALSE
      )
    }
    return(list(name = loss, kind = "pinball", tau = tau))
  }
  stop(
    sprintf(
      paste(
        "Unsupported 'metalearner_loss' value '%s'. Use 'squared_error',",
        "'absolute_error', or a two-digit pinball loss such as 'pinball_q90'."
      ),
      loss
    ),
    call. = FALSE
  )
}

#' Fit ensemble weights for the meta-policy super learner
#'
#' @param pred_mat Out-of-fold base-learner prediction matrix.
#' @param y Observed outcome vector.
#' @param loss Metalearner loss. `"squared_error"` combines the base learners
#'   with nonnegative least squares. A `"pinball_q<NN>"` loss combines them with
#'   a nonnegativity-constrained quantile regression instead, which is what lets
#'   upper-tail base learners earn weight: under squared error a q90 learner is
#'   penalized for exactly the upward bias it was asked to produce, so it is
#'   driven toward zero weight no matter how good it is at its own job.
#'
#' @return Numeric nonnegative ensemble weight vector normalized to sum to one.
#'
#' @keywords internal
#' @noRd
fit_super_learner_weights <- function(pred_mat,
                                      y,
                                      loss = "squared_error") {
  loss_spec <- parse_super_learner_loss(loss)
  if (identical(loss_spec$kind, "absolute_error")) {
    stop(
      paste(
        "The super learner combines base learners with a constrained",
        "least-squares or quantile-regression fit, so 'metalearner_loss'",
        "cannot be 'absolute_error'. Use 'squared_error' or a pinball loss",
        "such as 'pinball_q90'."
      ),
      call. = FALSE
    )
  }
  pred_mat <- as.matrix(pred_mat)
  y <- as.numeric(y)
  ok <- is.finite(y) & stats::complete.cases(pred_mat)
  pred_mat <- pred_mat[ok, , drop = FALSE]
  y <- y[ok]
  if (ncol(pred_mat) == 0 || nrow(pred_mat) == 0) {
    stop("No complete out-of-fold prediction rows were available for the super learner.", call. = FALSE)
  }
  if (ncol(pred_mat) == 1L) {
    return(rep(1, 1))
  }

  normalize_weights <- function(coef_now) {
    coef_now <- as.numeric(coef_now)
    coef_now[!is.finite(coef_now) | coef_now < 0] <- 0
    if (sum(coef_now) > 0) coef_now / sum(coef_now) else NULL
  }

  combiner_fit <- if (identical(loss_spec$kind, "pinball")) {
    # Minimizing the pinball loss subject to nonnegative weights is a linearly
    # constrained quantile regression, which quantreg solves directly via the
    # constrained Frisch-Newton interior-point method (R %*% b >= r with R the
    # identity and r zero is exactly the nonnegativity constraint).
    if (requireNamespace("quantreg", quietly = TRUE)) {
      try(
        quantreg::rq.fit.fnc(
          x = pred_mat,
          y = y,
          R = diag(ncol(pred_mat)),
          r = rep(0, ncol(pred_mat)),
          tau = loss_spec$tau
        ),
        silent = TRUE
      )
    } else {
      NULL
    }
  } else {
    try(nnls::nnls(pred_mat, y), silent = TRUE)
  }
  if (!is.null(combiner_fit) && !inherits(combiner_fit, "try-error")) {
    weights_now <- normalize_weights(stats::coef(combiner_fit))
    if (!is.null(weights_now)) {
      return(weights_now)
    }
  }

  # Fallback: inverse per-learner loss, scored on whichever loss the combiner
  # was asked for, so tail learners are not ranked by a criterion they were
  # never trying to minimize.
  losses <- apply(pred_mat, 2, function(pred) {
    err <- y - pred
    if (identical(loss_spec$kind, "pinball")) {
      mean(pmax(loss_spec$tau * err, (loss_spec$tau - 1) * err), na.rm = TRUE)
    } else {
      mean(err^2, na.rm = TRUE)
    }
  })
  inv <- 1 / pmax(losses, sqrt(.Machine$double.eps))
  inv / sum(inv)
}

#' Fit one base learner while suppressing hard failures
#'
#' @param training_data Model-ready training data.
#' @param method Base learner method.
#' @param feature_cols Feature columns.
#' @param outcome_transform Outcome transform.
#' @param lambda_rule Regularization rule.
#' @param inner_folds Inner cross-validation folds.
#' @param seed Integer seed.
#' @param method_settings Optional shared method-specific tuning settings.
#'
#' @return A fitted learner object or a `"try-error"` object.
#'
#' @keywords internal
#' @noRd
fit_meta_policy_base_safely <- function(training_data,
                                        method,
                                        feature_cols,
                                        outcome_transform,
                                        lambda_rule,
                                        inner_folds,
                                        seed,
                                        method_settings = NULL) {
  try(
    fit_meta_policy_learner(
      training_data = training_data,
      method = method,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      alpha = NULL,
      inner_folds = inner_folds,
      seed = seed,
      method_settings = method_settings
    ),
    silent = TRUE
  )
}

#' Fit one ensemble base learner without hiding LMM configuration failures
#'
#' The Super Learner may tolerate numerical failure of an optional base
#' learner, but it must not silently change its library because an explicitly
#' configured random intercept is unavailable. LMM failures therefore remain
#' hard errors; other learner failures retain the existing safe-fit behavior.
#'
#' @keywords internal
#' @noRd
fit_meta_policy_ensemble_base <- function(training_data,
                                          method,
                                          feature_cols,
                                          outcome_transform,
                                          lambda_rule,
                                          inner_folds,
                                          seed,
                                          method_settings = NULL) {
  method_spec <- meta_policy_method_spec(method, method_settings = method_settings)
  if (identical(method_spec$family, "lmm")) {
    return(fit_meta_policy_learner(
      training_data = training_data,
      method = method,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      alpha = NULL,
      inner_folds = inner_folds,
      seed = seed,
      method_settings = method_settings
    ))
  }
  fit_meta_policy_base_safely(
    training_data = training_data,
    method = method,
    feature_cols = feature_cols,
    outcome_transform = outcome_transform,
    lambda_rule = lambda_rule,
    inner_folds = inner_folds,
    seed = seed,
    method_settings = method_settings
  )
}

#' Predict super-learner scores for one outer crossfit fold with streaming fits
#'
#' Runs the inner OOF loop to derive metalearner weights, then fits each base
#' learner on the full training split and predicts on the test split one at a
#' time, accumulating a weighted average before freeing each model. This keeps
#' at most one fitted base learner in memory per worker at any instant, avoiding
#' the ~11x model accumulation that causes the RAM spike during parallel
#' crossfit.
#'
#' @param train_data Model-ready training split for this fold.
#' @param test_data Test split for this fold.
#' @param feature_cols Feature columns.
#' @param outcome_transform Outcome transform.
#' @param lambda_rule Regularization rule.
#' @param inner_folds Inner CV folds.
#' @param seed Integer seed.
#' @param super_methods Optional base methods.
#' @param metalearner_loss Metalearner loss.
#' @param method_settings Optional method-specific settings.
#' @param progress Logical. Emit per-method progress messages.
#'
#' @return `test_data` with `.meta_predicted_score` appended.
#'
#' @keywords internal
#' @noRd
stream_super_learner_fold <- function(train_data,
                                      test_data,
                                      feature_cols,
                                      outcome_transform,
                                      lambda_rule,
                                      inner_folds,
                                      seed,
                                      super_methods = NULL,
                                      metalearner_loss = "squared_error",
                                      method_settings = NULL,
                                      progress = FALSE) {
  train_data <- tibble::as_tibble(train_data)
  test_data <- tibble::as_tibble(test_data)
  metalearner_loss <- parse_super_learner_loss(metalearner_loss)$name
  methods <- available_meta_policy_super_methods(super_methods, method_settings = method_settings)
  n_methods <- length(methods)
  lmm_random_intercepts <- active_meta_policy_lmm_random_intercepts(
    methods,
    method_settings = method_settings
  )

  # Strip context/anchor columns from train_data before the inner-fold loop.
  # Each train_data[tr_idx, ] subset copies the data frame; removing unused
  # columns reduces the size of every copy. Only strip when feature_cols is
  # explicitly provided; when NULL features are discovered from the data itself.
  if (!is.null(feature_cols) && length(feature_cols) > 0L) {
    keep_train <- intersect(
      unique(c(feature_cols, ".outcome", ".split_group", lmm_random_intercepts)),
      names(train_data)
    )
    train_data <- train_data[, keep_train, drop = FALSE]
  }

  foldid <- if (".split_group" %in% names(train_data)) {
    grouped_foldid(train_data$.split_group, n_folds = inner_folds, seed = seed)
  } else {
    NULL
  }
  foldid <- foldid %||% row_foldid(nrow(train_data), n_folds = inner_folds, seed = seed)
  if (is.null(foldid)) {
    stop("stream_outer_fold: inner cross-validation requires at least two training rows.", call. = FALSE)
  }

  y <- train_data$.outcome
  pred_cols <- list()
  oof_seconds <- stats::setNames(rep(NA_real_, n_methods), methods)

  for (i_method in seq_along(methods)) {
    method_now <- methods[[i_method]]
    timing_start <- proc.time()
    report_progress(progress, sprintf("  [%d/%d] OOF: %s", i_method, n_methods, method_now))
    pred_now <- rep(NA_real_, nrow(train_data))
    ok_method <- TRUE
    for (fold_now in sort(unique(foldid))) {
      tr_idx <- which(foldid != fold_now)
      vl_idx <- which(foldid == fold_now)
      if (length(tr_idx) == 0 || length(vl_idx) == 0) {
        ok_method <- FALSE
        next
      }
      fold_seed <- if (is.null(seed)) NULL else as.integer(seed) + as.integer(fold_now)
      bl <- fit_meta_policy_ensemble_base(
        training_data = train_data[tr_idx, , drop = FALSE],
        method = method_now,
        feature_cols = feature_cols,
        outcome_transform = outcome_transform,
        lambda_rule = lambda_rule,
        inner_folds = inner_folds,
        seed = fold_seed,
        method_settings = method_settings
      )
      if (inherits(bl, "try-error")) {
        ok_method <- FALSE
        break
      }
      sc <- try(predict_meta_policy_score(bl, train_data[vl_idx, , drop = FALSE]), silent = TRUE)
      if (inherits(sc, "try-error") || !".meta_predicted_score" %in% names(sc)) {
        ok_method <- FALSE
        break
      }
      pred_now[vl_idx] <- sc$.meta_predicted_score
      rm(bl, sc)
    }
    if (ok_method && all(is.finite(pred_now))) {
      pred_cols[[method_now]] <- pred_now
    }
    oof_seconds[[method_now]] <- unname(
      (proc.time() - timing_start)[["elapsed"]]
    )
  }

  if (length(pred_cols) == 0) {
    stop("stream_outer_fold: no base methods produced complete OOF predictions.", call. = FALSE)
  }
  oof_mat <- do.call(cbind, pred_cols)
  colnames(oof_mat) <- names(pred_cols)
  weights <- fit_super_learner_weights(oof_mat, y, loss = metalearner_loss)
  names(weights) <- colnames(oof_mat)
  weight_lookup <- stats::setNames(rep(0, length(methods)), methods)
  weight_lookup[names(weights)] <- weights
  weights <- weights[weights > 0]
  if (length(weights) == 0) {
    stop("stream_outer_fold: metalearner produced zero-weight ensemble.", call. = FALSE)
  }
  weights <- weights / sum(weights)
  weight_lookup[names(weights)] <- weights

  prediction_cap <- meta_policy_prediction_cap(train_data$.outcome)
  n_test <- nrow(test_data)
  acc_pred <- rep(0, n_test)
  weight_sum <- 0
  methods_final <- names(pred_cols)
  refit_seconds <- stats::setNames(rep(NA_real_, length(methods)), methods)
  method_test_errors <- stats::setNames(rep(NA_real_, length(methods)), methods)
  method_test_rmse <- stats::setNames(rep(NA_real_, length(methods)), methods)

  report_progress(progress, sprintf("  Streaming %d final fits ...", length(methods_final)))
  for (i_final in seq_along(methods_final)) {
    method_now <- methods_final[[i_final]]
    w <- if (method_now %in% names(weights)) weights[[method_now]] else 0
    timing_start <- proc.time()
    report_progress(progress, sprintf("  [%d/%d] Final: %s (w=%.3f)", i_final, length(methods_final), method_now, w))
    bl <- fit_meta_policy_ensemble_base(
      training_data = train_data,
      method = method_now,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      inner_folds = inner_folds,
      seed = as.integer(seed),
      method_settings = method_settings
    )
    if (!inherits(bl, "try-error")) {
      sc <- try(predict_meta_policy_score(bl, test_data), silent = TRUE)
      if (!inherits(sc, "try-error") && ".meta_predicted_score" %in% names(sc)) {
        if (".outcome" %in% names(test_data)) {
          method_test_errors[[method_now]] <- mean(abs(test_data$.outcome - sc$.meta_predicted_score), na.rm = TRUE)
          method_test_rmse[[method_now]] <- sqrt(mean((test_data$.outcome - sc$.meta_predicted_score)^2, na.rm = TRUE))
        }
        if (is.finite(w) && w > 0) {
          acc_pred <- acc_pred + w * sc$.meta_predicted_score
          weight_sum <- weight_sum + w
        }
      }
      rm(bl, sc)
    }
    refit_seconds[[method_now]] <- unname(
      (proc.time() - timing_start)[["elapsed"]]
    )
  }
  if (weight_sum > 0) {
    acc_pred <- acc_pred / weight_sum
  }
  acc_pred <- pmin(pmax(0, acc_pred), prediction_cap)
  out <- test_data |> dplyr::mutate(.meta_predicted_score = acc_pred)
  attr(out, "learner_timings") <- tibble::tibble(
    method = methods,
    oof_seconds = unname(oof_seconds[methods]),
    refit_seconds = unname(refit_seconds[methods]),
    weight = unname(weight_lookup[methods]),
    test_mae = unname(method_test_errors[methods]),
    test_rmse = unname(method_test_rmse[methods])
  ) |>
    dplyr::mutate(
      total_seconds = rowSums(
        cbind(.data$oof_seconds, .data$refit_seconds),
        na.rm = TRUE
      ),
      succeeded_oof = .data$method %in% names(pred_cols),
      succeeded_refit = .data$method %in% methods_final
    )
  out
}

#' Build a failed final Super Learner OOF task result
#'
#' Creates the result shape used by final-fit Super Learner OOF tasks when a
#' worker returns an error for one method-fold task.
#'
#' @param task List with `method` and `fold_id` entries.
#' @param message Failure message.
#'
#' @return List containing task diagnostics.
#' @keywords internal
#' @noRd
failed_meta_policy_super_oof_task <- function(task,
                                              message) {
  list(
    method = as.character(task$method),
    fold_id = as.integer(task$fold_id),
    valid_idx = integer(),
    pred = numeric(),
    seconds = NA_real_,
    success = FALSE,
    error = as.character(message %||% "final Super Learner OOF task failed")
  )
}

#' Run one final Super Learner OOF method-fold task
#'
#' Fits one base learner on all training rows outside one inner fold and
#' predicts the held-out inner fold. The caller combines all method-fold task
#' results into complete OOF prediction columns for Super Learner weight
#' estimation.
#'
#' @param task List with `method` and `fold_id` entries.
#' @param payload Named list containing training data, fold assignments, and
#'   learner settings.
#'
#' @return List containing held-out row indices, predictions, timing, and
#'   success diagnostics.
#' @keywords internal
#' @noRd
run_meta_policy_super_oof_payload_task <- function(task,
                                                   payload) {
  method_now <- as.character(task$method)
  fold_id <- as.integer(task$fold_id)
  task_start <- proc.time()
  training_data <- tibble::as_tibble(payload$training_data)
  foldid <- as.integer(payload$foldid)
  train_idx <- which(foldid != fold_id)
  valid_idx <- which(foldid == fold_id)
  if (length(train_idx) == 0L || length(valid_idx) == 0L) {
    return(failed_meta_policy_super_oof_task(task, "empty inner train/validation split"))
  }
  method_spec <- meta_policy_method_spec(method_now, method_settings = payload$method_settings)
  hard_fail <- identical(method_spec$family, "lmm")

  result <- tryCatch(
    {
      fold_seed <- if (is.null(payload$seed)) NULL else as.integer(payload$seed) + fold_id
      learner <- fit_meta_policy_ensemble_base(
        training_data = training_data[train_idx, , drop = FALSE],
        method = method_now,
        feature_cols = payload$feature_cols,
        outcome_transform = payload$outcome_transform,
        lambda_rule = payload$lambda_rule,
        inner_folds = payload$inner_folds,
        seed = fold_seed,
        method_settings = payload$method_settings
      )
      if (inherits(learner, "try-error")) {
        stop(as.character(learner)[[1]], call. = FALSE)
      }
      scored <- try(
        predict_meta_policy_score(learner, training_data[valid_idx, , drop = FALSE]),
        silent = TRUE
      )
      if (inherits(scored, "try-error")) {
        stop(as.character(scored)[[1]], call. = FALSE)
      }
      if (!".meta_predicted_score" %in% names(scored)) {
        stop("missing .meta_predicted_score", call. = FALSE)
      }
      pred <- as.numeric(scored$.meta_predicted_score)
      if (length(pred) != length(valid_idx) || any(!is.finite(pred))) {
        stop("non-finite or incomplete OOF predictions", call. = FALSE)
      }
      list(
        method = method_now,
        fold_id = fold_id,
        valid_idx = valid_idx,
        pred = pred,
        seconds = unname((proc.time() - task_start)[["elapsed"]]),
        success = TRUE,
        error = NA_character_
      )
    },
    error = function(e) {
      if (isTRUE(hard_fail)) {
        stop(conditionMessage(e), call. = FALSE)
      }
      failed_meta_policy_super_oof_task(task, conditionMessage(e))
    }
  )
  result
}

#' Run one inherited final Super Learner OOF task
#'
#' Fetches final-fit OOF task settings from the namespace-local worker payload
#' and evaluates one method-fold task.
#'
#' @param task List with `method` and `fold_id` entries.
#'
#' @return List containing held-out predictions and diagnostics.
#' @keywords internal
#' @noRd
run_meta_policy_super_oof_task <- function(task) {
  payload <- as.list(.meta_policy_crossfit_payload)
  run_meta_policy_super_oof_payload_task(task, payload)
}

#' Log a final Super Learner OOF task result
#'
#' Emits one progress line for a completed final-fit method-fold task. Failed
#' tasks print the failure details on the following line under the same task
#' update format used by the policy cross-fit scheduler.
#'
#' @param result Task result from [run_meta_policy_super_oof_payload_task()].
#' @param completed_n Number of tasks completed so far.
#' @param total_tasks Total number of scheduled tasks.
#' @param progress Logical. Emit progress messages.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
log_meta_policy_super_oof_result <- function(result,
                                             completed_n,
                                             total_tasks,
                                             progress = FALSE) {
  if (!isTRUE(progress)) {
    return(invisible(NULL))
  }
  icon <- if (isTRUE(result$success)) "\u2705" else "\u274c"
  base_msg <- sprintf(
    "  %s Final Super Learner OOF task complete: %d/%d [fold=%d method=%s success=%s].",
    icon,
    as.integer(completed_n),
    as.integer(total_tasks),
    as.integer(result$fold_id),
    as.character(result$method),
    if (isTRUE(result$success)) "TRUE" else "FALSE"
  )
  if (isTRUE(result$success)) {
    report_progress(TRUE, base_msg)
  } else {
    report_progress(
      TRUE,
      paste0(
        base_msg,
        "\n",
        as.character(result$error %||% "Unknown final Super Learner OOF task failure.")
      )
    )
  }
  invisible(NULL)
}

#' Run final Super Learner OOF method-fold tasks
#'
#' Dispatches the final-fit Super Learner OOF grid across workers. The task unit
#' is one base method and one inner fold, which keeps the OOF weight-estimation
#' stage parallel without changing the configured learner library or tuning
#' settings.
#'
#' @param tasks List of method-fold tasks.
#' @param payload Named list containing training data and learner settings.
#' @param workers Requested worker budget.
#' @param progress Logical. Emit progress messages.
#'
#' @return List of task results.
#' @keywords internal
#' @noRd
run_meta_policy_super_oof_tasks <- function(tasks,
                                            payload,
                                            workers,
                                            progress = FALSE) {
  total_tasks <- length(tasks)
  if (total_tasks == 0L) {
    return(list())
  }
  workers <- max(1L, min(as.integer(workers %||% 1L), total_tasks))
  report_progress(
    progress,
    sprintf(
      "Final Super Learner OOF scheduler: method-fold mode, %d task(s), %d worker(s).",
      total_tasks,
      workers
    )
  )

  if (workers <= 1L) {
    out <- vector("list", total_tasks)
    for (i in seq_along(tasks)) {
      out[[i]] <- run_meta_policy_super_oof_payload_task(tasks[[i]], payload)
      log_meta_policy_super_oof_result(out[[i]], i, total_tasks, progress)
    }
    return(out)
  }

  # Fork workers inherit namespace-local payloads at cluster creation time, so
  # the payload must be populated before makeForkCluster() is called.
  set_meta_policy_crossfit_payload(payload)
  cluster_obj <- NULL
  on.exit(
    {
      if (!is.null(cluster_obj)) {
        try(parallel::stopCluster(cluster_obj), silent = TRUE)
      }
      clear_meta_policy_crossfit_payload()
    },
    add = TRUE
  )
  cluster_obj <- initialize_parallel_cluster(workers = workers)
  cluster_type <- attr(cluster_obj, "cluster_type", exact = TRUE) %||% "unknown"

  if (identical(cluster_type, "fork")) {
    out <- parallel::parLapplyLB(cluster_obj, tasks, run_meta_policy_super_oof_task)
  } else {
    run_super_oof_task <- function(task) {
      run_meta_policy_super_oof_payload_task(task, payload)
    }
    environment(run_super_oof_task) <- list2env(
      list(
        payload = payload,
        run_meta_policy_super_oof_payload_task = run_meta_policy_super_oof_payload_task
      ),
      parent = baseenv()
    )
    tsb_cluster_export(cluster_obj, c("run_super_oof_task"), envir = environment())
    out <- parallel::parLapplyLB(cluster_obj, tasks, run_super_oof_task)
  }
  for (i in seq_along(out)) {
    log_meta_policy_super_oof_result(out[[i]], i, total_tasks, progress)
  }
  out
}

#' Fit the meta-policy super learner
#'
#' @param training_data Model-ready training data.
#' @param feature_cols Feature columns.
#' @param outcome_transform Outcome transform.
#' @param lambda_rule Regularization rule for glm-penalized base learners.
#' @param inner_folds Number of inner folds.
#' @param seed Integer seed.
#' @param super_methods Optional requested base methods.
#' @param metalearner_loss Metalearner loss.
#' @param method_settings Optional shared method-specific tuning settings.
#' @param workers Number of workers used for final Super Learner OOF
#'   method-fold tasks.
#' @param progress Logical. Emit progress messages.
#'
#' @return A fitted meta-policy learner object with super-learner weights and
#'   out-of-fold diagnostics.
#'
#' @keywords internal
#' @noRd
fit_meta_policy_super_learner <- function(training_data,
                                          feature_cols,
                                          outcome_transform,
                                          lambda_rule,
                                          inner_folds,
                                          seed,
                                          super_methods = NULL,
                                          metalearner_loss = "squared_error",
                                          method_settings = NULL,
                                          workers = 1L,
                                          progress = FALSE) {
  training_data <- tibble::as_tibble(training_data)
  metalearner_loss <- parse_super_learner_loss(metalearner_loss)$name
  methods <- available_meta_policy_super_methods(super_methods, method_settings = method_settings)
  n_methods <- length(methods)
  lmm_random_intercepts <- active_meta_policy_lmm_random_intercepts(
    methods,
    method_settings = method_settings
  )

  # Strip context/anchor columns before inner-fold loop - only feature_cols +
  # .outcome + split/configured random-effect groups are needed, and every
  # train_idx/valid_idx subset copies the full data frame.
  keep_cols <- unique(c(feature_cols, ".outcome", ".split_group", lmm_random_intercepts))
  keep_cols <- intersect(keep_cols, names(training_data))
  training_data <- training_data[, keep_cols, drop = FALSE]

  foldid <- if (".split_group" %in% names(training_data)) {
    grouped_foldid(training_data$.split_group, n_folds = inner_folds, seed = seed)
  } else {
    NULL
  }
  foldid <- foldid %||% row_foldid(nrow(training_data), n_folds = inner_folds, seed = seed)
  if (is.null(foldid)) {
    stop("The super learner requires at least two training rows for inner cross-validation.", call. = FALSE)
  }

  y <- training_data$.outcome
  pred_cols <- list()
  learner_errors <- list()
  oof_seconds <- stats::setNames(rep(NA_real_, n_methods), methods)
  fold_levels <- sort(unique(foldid))
  workers <- max(1L, as.integer(workers %||% 1L))
  if (workers > 1L && length(methods) * length(fold_levels) > 1L) {
    tasks <- unlist(
      lapply(fold_levels, function(fold_now) {
        lapply(methods, function(method_now) {
          list(fold_id = as.integer(fold_now), method = method_now)
        })
      }),
      recursive = FALSE
    )
    oof_results <- run_meta_policy_super_oof_tasks(
      tasks = tasks,
      payload = list(
        training_data = training_data,
        foldid = foldid,
        feature_cols = feature_cols,
        outcome_transform = outcome_transform,
        lambda_rule = lambda_rule,
        inner_folds = inner_folds,
        seed = seed,
        method_settings = method_settings
      ),
      workers = workers,
      progress = progress
    )
    for (method_now in methods) {
      method_results <- oof_results[vapply(
        oof_results,
        function(result) identical(as.character(result$method), method_now),
        logical(1)
      )]
      pred_now <- rep(NA_real_, nrow(training_data))
      ok_method <- length(method_results) == length(fold_levels)
      method_seconds <- numeric()
      method_error <- character()
      for (result in method_results) {
        if (isTRUE(result$success) &&
          length(result$valid_idx) > 0L &&
          length(result$pred) == length(result$valid_idx)) {
          pred_now[result$valid_idx] <- result$pred
        } else {
          ok_method <- FALSE
          method_error <- c(method_error, as.character(result$error %||% "OOF task failed"))
        }
        if (is.finite(result$seconds)) {
          method_seconds <- c(method_seconds, result$seconds)
        }
      }
      if (ok_method && all(is.finite(pred_now))) {
        pred_cols[[method_now]] <- pred_now
      } else {
        learner_errors[[method_now]] <- paste(unique(method_error), collapse = "\n")
      }
      oof_seconds[[method_now]] <- if (length(method_seconds) > 0L) sum(method_seconds) else NA_real_
    }
  } else {
    for (i_method in seq_along(methods)) {
      method_now <- methods[[i_method]]
      timing_start <- proc.time()
      report_progress(progress, sprintf("  [%d/%d] OOF: %s", i_method, n_methods, method_now))
      pred_now <- rep(NA_real_, nrow(training_data))
      ok_method <- TRUE
      for (fold_now in fold_levels) {
        train_idx <- which(foldid != fold_now)
        valid_idx <- which(foldid == fold_now)
        if (length(train_idx) == 0 || length(valid_idx) == 0) {
          ok_method <- FALSE
          next
        }
        fold_seed <- if (is.null(seed)) NULL else as.integer(seed) + as.integer(fold_now)
        learner <- fit_meta_policy_ensemble_base(
          training_data = training_data[train_idx, , drop = FALSE],
          method = method_now,
          feature_cols = feature_cols,
          outcome_transform = outcome_transform,
          lambda_rule = lambda_rule,
          inner_folds = inner_folds,
          seed = fold_seed,
          method_settings = method_settings
        )
        if (inherits(learner, "try-error")) {
          ok_method <- FALSE
          learner_errors[[method_now]] <- as.character(learner)
          break
        }
        scored <- try(
          predict_meta_policy_score(learner, training_data[valid_idx, , drop = FALSE]),
          silent = TRUE
        )
        if (inherits(scored, "try-error") || !".meta_predicted_score" %in% names(scored)) {
          ok_method <- FALSE
          learner_errors[[method_now]] <- as.character(scored)
          break
        }
        pred_now[valid_idx] <- scored$.meta_predicted_score
      }
      if (ok_method && all(is.finite(pred_now))) {
        pred_cols[[method_now]] <- pred_now
      }
      oof_seconds[[method_now]] <- unname(
        (proc.time() - timing_start)[["elapsed"]]
      )
    }
  }

  if (length(pred_cols) == 0) {
    stop("No super-learner base methods produced complete out-of-fold predictions.", call. = FALSE)
  }
  oof_mat <- do.call(cbind, pred_cols)
  colnames(oof_mat) <- names(pred_cols)
  weights <- fit_super_learner_weights(oof_mat, y, loss = metalearner_loss)
  names(weights) <- colnames(oof_mat)

  report_progress(progress, sprintf("  Refitting %d base learners on full training data ...", length(weights)))
  final_learners <- list()
  final_method_names <- names(weights)
  refit_seconds <- stats::setNames(rep(NA_real_, length(final_method_names)), final_method_names)
  for (i_final in seq_along(final_method_names)) {
    method_now <- final_method_names[[i_final]]
    timing_start <- proc.time()
    report_progress(progress, sprintf("  [%d/%d] Final fit: %s", i_final, length(final_method_names), method_now))
    learner <- fit_meta_policy_ensemble_base(
      training_data = training_data,
      method = method_now,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      inner_folds = inner_folds,
      seed = if (is.null(seed)) NULL else as.integer(seed),
      method_settings = method_settings
    )
    if (!inherits(learner, "try-error")) {
      final_learners[[method_now]] <- learner
    }
    refit_seconds[[method_now]] <- unname(
      (proc.time() - timing_start)[["elapsed"]]
    )
  }
  weights <- weights[names(final_learners)]
  if (length(final_learners) == 0 || length(weights) == 0) {
    stop("No super-learner base methods could be refit on the full training data.", call. = FALSE)
  }
  weights <- weights / sum(weights)
  active <- weights > 0
  if (any(active)) {
    final_learners <- final_learners[names(weights)[active]]
    weights <- weights[active]
  }

  oof_pred <- as.numeric(oof_mat[, names(weights), drop = FALSE] %*% weights)
  oof_weights <- weights[colnames(oof_mat)]
  oof_weights[!is.finite(oof_weights)] <- 0
  learner_timings <- tibble::tibble(
    method = methods,
    oof_seconds = unname(oof_seconds[methods]),
    refit_seconds = unname(refit_seconds[methods])
  ) |>
    dplyr::mutate(
      total_seconds = rowSums(
        cbind(.data$oof_seconds, .data$refit_seconds),
        na.rm = TRUE
      ),
      succeeded_oof = .data$method %in% colnames(oof_mat),
      succeeded_refit = .data$method %in% names(final_learners)
    )
  structure(
    list(
      fit = final_learners,
      method = "super_learner",
      library = names(final_learners),
      weights = weights,
      metalearner_loss = metalearner_loss,
      method_settings = method_settings,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
      training_n = nrow(training_data),
      inner_foldid = foldid,
      oof_predictions = tibble::as_tibble(oof_mat),
      oof_ensemble_prediction = oof_pred,
      oof_performance = tibble::tibble(
        method = c(colnames(oof_mat), "super_learner"),
        mae = c(
          apply(oof_mat, 2, function(pred) mean(abs(y - pred), na.rm = TRUE)),
          mean(abs(y - oof_pred), na.rm = TRUE)
        ),
        rmse = c(
          apply(oof_mat, 2, function(pred) sqrt(mean((y - pred)^2, na.rm = TRUE))),
          sqrt(mean((y - oof_pred)^2, na.rm = TRUE))
        ),
        weight = c(oof_weights, 1)
      ),
      learner_errors = learner_errors,
      learner_timings = learner_timings
    ),
    class = "tsb_meta_policy_learner"
  )
}

#' Prepare training data for a meta-policy learner
#'
#' @param policy_perf Policy-performance table.
#' @param outcome_col Outcome column to model.
#' @param feature_cols Optional feature columns. `NULL` uses defaults.
#' @param outcome_clip_quantile Optional upper quantile used to clip extreme
#'   outcomes during training-data preparation.
#' @param retain_cols Optional non-feature columns that downstream learners
#'   require, such as user-configured random-effect grouping columns.
#'
#' @return A tibble with model-ready features and `.outcome`.
#'
#' @examples
#' \dontrun{
#' training_data <- prepare_meta_policy_data(
#'   policy_perf = species_block_perf,
#'   outcome_col = "error_abs_log"
#' )
#' }
#' @keywords internal
#' @noRd
prepare_meta_policy_data <- function(policy_perf,
                                     outcome_col = "error_abs_log",
                                     feature_cols = NULL,
                                     outcome_clip_quantile = NULL,
                                     retain_cols = NULL) {
  policy_perf <- tibble::as_tibble(policy_perf)
  if (!outcome_col %in% names(policy_perf)) {
    stop(sprintf("Outcome column '%s' was not found.", outcome_col), call. = FALSE)
  }
  policy_perf$policy <- resolve_policy_names(policy_perf)

  feature_cols <- feature_cols %||% default_meta_policy_features(policy_perf)
  feature_cols <- intersect(feature_cols, names(policy_perf))
  feature_cols <- sanitize_meta_policy_feature_cols(feature_cols)
  if (length(feature_cols) == 0) {
    stop("No usable meta-policy feature columns were supplied.", call. = FALSE)
  }
  retain_cols <- intersect(
    unique(as.character(unlist(retain_cols %||% character(), use.names = FALSE))),
    names(policy_perf)
  )

  out <- policy_perf |>
    dplyr::filter(.data$valid_prediction, is.finite(.data[[outcome_col]])) |>
    dplyr::select(
      dplyr::any_of(c(meta_policy_context_columns(policy_perf), retain_cols)),
      dplyr::all_of(feature_cols),
      .outcome = dplyr::all_of(outcome_col)
    )
  out[[outcome_col]] <- out$.outcome
  out$.outcome_raw <- out$.outcome

  for (nm in feature_cols) {
    if (is.character(out[[nm]]) || is.logical(out[[nm]])) {
      out[[nm]] <- as.factor(out[[nm]])
    }
  }

  outcome_clip_quantile <- suppressWarnings(as.numeric(outcome_clip_quantile %||% NA_real_)[[1]])
  if (is.finite(outcome_clip_quantile) && outcome_clip_quantile > 0 && outcome_clip_quantile < 1) {
    outcome_cap <- suppressWarnings(stats::quantile(
      out$.outcome_raw,
      probs = outcome_clip_quantile,
      na.rm = TRUE,
      names = FALSE,
      type = 8
    ))
    if (is.finite(outcome_cap)) {
      out$.outcome <- pmin(out$.outcome_raw, outcome_cap)
      out$.outcome_was_clipped <- out$.outcome_raw > out$.outcome
      attr(out, "outcome_clip_quantile") <- outcome_clip_quantile
      attr(out, "outcome_clip_cap") <- outcome_cap
    }
  }
  if (!".outcome_was_clipped" %in% names(out)) {
    out$.outcome_was_clipped <- FALSE
  }

  out
}

#' Fit a meta-policy learner
#'
#' @param training_data Output from `prepare_meta_policy_data()`.
#' @param method Learner method. `glm` is the simplest linear baseline;
#'   `gam` fits smooth additive terms; `rpart`, `rf`, and `xgboost`
#'   provide tree-based nonlinear learners; the `glm_*` penalized variants apply
#'   penalized Gaussian regression; and `super_learner` fits a convex ensemble
#'   over the supported base methods.
#' @param feature_cols Optional feature columns.
#' @param outcome_transform Outcome transform. `log1p` stabilizes heavy tails.
#' @param alpha Optional elastic-net mixing parameter.
#' @param lambda_rule Regularization rule for glm-penalized learners.
#' @param inner_folds Number of inner cross-validation folds.
#' @param seed Integer seed.
#' @param super_methods Optional super-learner base methods.
#' @param metalearner_loss Loss used to combine super-learner base predictions.
#' @param method_settings Optional shared method-specific tuning settings.
#' @param workers Number of workers used by `method = "super_learner"` for the
#'   final inner OOF method-fold task grid.
#' @param progress Logical. Emit `tsb_message()` progress lines when `TRUE`.
#'   Only active for `method = "super_learner"`.
#' @param ... Additional method-specific arguments.
#'
#' @return A fitted meta-policy learner object.
#'
#' @examples
#' \dontrun{
#' training_data <- prepare_meta_policy_data(species_block_perf)
#' learner <- fit_meta_policy_learner(
#'   training_data = training_data,
#'   method = "glm"
#' )
#' }
#' @keywords internal
#' @noRd
fit_meta_policy_learner <- function(training_data,
                                    method = "super_learner",
                                    feature_cols = NULL,
                                    outcome_transform = c("log1p", "identity"),
                                    alpha = NULL,
                                    lambda_rule = c("lambda.1se", "lambda.min"),
                                    inner_folds = 5L,
                                    seed = NULL,
                                    super_methods = NULL,
                                    metalearner_loss = "squared_error",
                                    method_settings = NULL,
                                    workers = 1L,
                                    progress = FALSE,
                                    ...) {
  training_data <- tibble::as_tibble(training_data)
  method <- stringr::str_squish(as.character(method %||% ""))[[1]]
  outcome_transform <- match.arg(outcome_transform)
  lambda_rule <- match.arg(lambda_rule)
  metalearner_loss <- parse_super_learner_loss(metalearner_loss)$name
  method_catalog <- meta_policy_method_catalog(method_settings = method_settings)
  allowed_methods <- c(
    "super_learner",
    method_catalog$methods
  )
  if (!method %in% allowed_methods) {
    stop(
      sprintf("Unsupported meta-policy learner method '%s'. Allowed values are: %s", method, paste(allowed_methods, collapse = ", ")),
      call. = FALSE
    )
  }
  if (!".outcome" %in% names(training_data)) {
    stop("'training_data' must contain '.outcome'.", call. = FALSE)
  }

  feature_cols <- feature_cols %||% default_meta_policy_features(training_data)
  feature_cols <- intersect(feature_cols, names(training_data))
  feature_cols <- sanitize_meta_policy_feature_cols(feature_cols)
  if (length(feature_cols) == 0) {
    stop("No usable feature columns were supplied.", call. = FALSE)
  }

  if (identical(method, "super_learner")) {
    return(fit_meta_policy_super_learner(
      training_data = training_data,
      feature_cols = feature_cols,
      outcome_transform = outcome_transform,
      lambda_rule = lambda_rule,
      inner_folds = inner_folds,
      seed = seed,
      super_methods = super_methods,
      metalearner_loss = metalearner_loss,
      method_settings = method_settings,
      workers = workers,
      progress = progress
    ))
  }

  fit_arguments <- meta_policy_method_arguments(
    method = method,
    method_settings = method_settings
  )
  method_spec <- meta_policy_method_spec(method, method_settings = method_settings)

  y <- if (identical(outcome_transform, "log1p")) {
    log1p(training_data$.outcome)
  } else {
    training_data$.outcome
  }
  prep <- meta_policy_blueprint(training_data, feature_cols)
  model_frame <- prep$data
  keep_model_col <- vapply(model_frame, function(x) {
    if (is.factor(x)) {
      return(nlevels(droplevels(x)) >= 2L)
    }
    vals <- suppressWarnings(as.numeric(x))
    vals <- vals[is.finite(vals)]
    length(unique(vals)) >= 2L
  }, logical(1))
  dropped_model_cols <- names(model_frame)[!keep_model_col]
  model_frame <- model_frame[, keep_model_col, drop = FALSE]
  if (ncol(model_frame) == 0) {
    stop("No non-constant meta-policy feature columns remained after preprocessing.", call. = FALSE)
  }

  if (identical(method_spec$family, "glm_penalized")) {
    alpha <- alpha %||% fit_arguments$alpha %||% 0.25
    mm <- meta_policy_model_matrix(model_frame)
    foldid <- if (".split_group" %in% names(training_data)) {
      grouped_foldid(training_data$.split_group, n_folds = inner_folds, seed = seed)
    } else {
      NULL
    }
    if (!is.null(foldid) && length(unique(stats::na.omit(foldid))) < 4L) {
      foldid <- row_foldid(nrow(mm$x), n_folds = min(max(4L, as.integer(inner_folds)), nrow(mm$x)), seed = seed)
    }
    nfolds <- if (is.null(foldid)) {
      min(max(4L, as.integer(inner_folds)), nrow(mm$x))
    } else {
      length(unique(stats::na.omit(foldid)))
    }
    fit <- glmnet::cv.glmnet(
      x = mm$x,
      y = y,
      alpha = alpha,
      foldid = foldid,
      nfolds = nfolds,
      family = "gaussian",
      type.measure = fit_arguments$type_measure %||% "mae",
      standardize = isTRUE(fit_arguments$standardize %||% TRUE),
      ...
    )
    lambda <- fit[[lambda_rule]]
    return(structure(
      list(
        fit = fit,
        method = method,
        method_family = method_spec$family,
        method_variant = method_spec$variant,
        alpha = alpha,
        lambda_rule = lambda_rule,
        lambda = lambda,
        method_settings = method_settings,
        method_arguments = fit_arguments,
        feature_cols = feature_cols,
        blueprint = prep$blueprint,
        dropped_model_cols = dropped_model_cols,
        terms = mm$terms,
        x_columns = colnames(mm$x),
        outcome_transform = outcome_transform,
        prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
        training_n = nrow(model_frame),
        model_matrix_ncol = ncol(mm$x)
      ),
      class = "tsb_meta_policy_learner"
    ))
  }

  model_data <- dplyr::bind_cols(model_frame, tibble::tibble(.outcome_model = y))
  formula_now <- stats::reformulate(
    termlabels = names(model_frame),
    response = ".outcome_model"
  )

  if (identical(method_spec$family, "qreg")) {
    mm <- meta_policy_model_matrix(model_frame)
    qr_now <- qr(mm$x)
    keep_idx <- seq_len(qr_now$rank)
    if (length(keep_idx) == 0L) {
      stop("No non-collinear quantile-regression columns remained after preprocessing.", call. = FALSE)
    }
    x_fit <- mm$x[, qr_now$pivot[keep_idx], drop = FALSE]
    fit <- suppressWarnings(
      quantreg::rq.fit(
        x = x_fit,
        y = y,
        tau = as.numeric(fit_arguments$tau %||% 0.50),
        method = fit_arguments$method %||% "fn",
        ...
      )
    )
    return(structure(
      list(
        fit = fit,
        method = method,
        method_family = method_spec$family,
        method_variant = method_spec$variant,
        method_settings = method_settings,
        method_arguments = fit_arguments,
        feature_cols = feature_cols,
        blueprint = prep$blueprint,
        dropped_model_cols = dropped_model_cols,
        terms = mm$terms,
        x_columns = colnames(x_fit),
        outcome_transform = outcome_transform,
        prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
        training_n = nrow(model_frame),
        model_matrix_ncol = ncol(x_fit)
      ),
      class = "tsb_meta_policy_learner"
    ))
  }

  if (identical(method_spec$family, "xgboost")) {
    mm <- meta_policy_model_matrix(model_frame)
    xgboost_params <- fit_arguments$params %||% list(objective = "reg:squarederror")
    xgboost_nthread <- as.integer(xgboost_params$nthread %||% 1L)
    xgboost_nrounds <- as.integer(fit_arguments$nrounds %||% 100L)
    xgboost_verbose <- as.integer(fit_arguments$verbose %||% 0L)
    # Early stopping needs an evaluation set that xgboost cannot carve for
    # itself, so hold one out deterministically. The holdout is used only to
    # *discover* the round count: keeping the model fit on the reduced rows
    # trades away more accuracy (via the lost training data) than the stopping
    # buys back, so the discovered count is refit on every row. An empty split
    # means the fit is too small to spare evaluation rows, so fall back to the
    # fixed schedule.
    eval_rows <- if (is.null(fit_arguments$early_stopping_rounds)) {
      integer(0)
    } else {
      sample_meta_policy_holdout_rows(
        n = nrow(mm$x),
        fraction = fit_arguments$validation_fraction %||% 0.2,
        seed = seed
      )
    }
    if (length(eval_rows) > 0L) {
      search_fit <- xgboost::xgb.train(
        params = xgboost_params,
        data = xgboost::xgb.DMatrix(
          data = mm$x[-eval_rows, , drop = FALSE],
          label = y[-eval_rows],
          nthread = xgboost_nthread
        ),
        nrounds = xgboost_nrounds,
        evals = list(
          validation = xgboost::xgb.DMatrix(
            data = mm$x[eval_rows, , drop = FALSE],
            label = y[eval_rows],
            nthread = xgboost_nthread
          )
        ),
        early_stopping_rounds = as.integer(fit_arguments$early_stopping_rounds),
        verbose = xgboost_verbose,
        ...
      )
      # xgboost exposes the winning round under an `early_stop` R attribute
      # (base-1). Older and newer builds have moved this around, so fall back to
      # the C-level attribute, which is 0-based, and finally to the configured
      # cap if neither is readable.
      best_rounds <- suppressWarnings(as.integer(
        (attributes(search_fit)$early_stop %||% list())$best_iteration %||% NA_integer_
      ))
      if (length(best_rounds) != 1L || is.na(best_rounds) || best_rounds < 1L) {
        best_rounds <- suppressWarnings(
          as.integer(xgboost::xgb.attr(search_fit, "best_iteration")) + 1L
        )
      }
      if (length(best_rounds) != 1L || is.na(best_rounds) || best_rounds < 1L) {
        best_rounds <- xgboost_nrounds
      }
      xgboost_nrounds <- min(best_rounds, xgboost_nrounds)
    }
    dtrain <- xgboost::xgb.DMatrix(
      data = mm$x,
      label = y,
      nthread = xgboost_nthread
    )
    fit <- xgboost::xgb.train(
      params = xgboost_params,
      data = dtrain,
      nrounds = xgboost_nrounds,
      verbose = xgboost_verbose,
      ...
    )
    return(structure(
      list(
        fit = fit,
        method = method,
        method_family = method_spec$family,
        method_variant = method_spec$variant,
        method_settings = method_settings,
        method_arguments = fit_arguments,
        feature_cols = feature_cols,
        blueprint = prep$blueprint,
        dropped_model_cols = dropped_model_cols,
        terms = mm$terms,
        x_columns = colnames(mm$x),
        outcome_transform = outcome_transform,
        prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
        training_n = nrow(model_frame),
        model_matrix_ncol = ncol(mm$x)
      ),
      class = "tsb_meta_policy_learner"
    ))
  }

  if (identical(method_spec$family, "bart")) {
    if (!requireNamespace("stochtree", quietly = TRUE)) {
      stop(
        paste(
          "Fitting BART methods (bart/xbart/wsbart/vfbart/rebart) requires the",
          "suggested package 'stochtree' to be installed."
        ),
        call. = FALSE
      )
    }
    # stochtree consumes a numeric design matrix, so build one via the shared
    # model-matrix path (same as glmnet/xgboost) rather than a formula. The
    # rebart variant additionally needs the group column, resolved inside the
    # helper from `training_data`. All stochtree-specific arguments are confined
    # to fit_stochtree_bart() so the API surface lives in one place.
    mm <- meta_policy_model_matrix(model_frame)
    bart_fit <- fit_stochtree_bart(
      x = mm$x,
      y = y,
      args = fit_arguments,
      training_data = training_data,
      seed = seed
    )
    return(structure(
      list(
        fit = bart_fit$model,
        fit_json = bart_fit$model_json,
        method = method,
        method_family = method_spec$family,
        method_variant = method_spec$variant,
        method_settings = method_settings,
        method_arguments = fit_arguments,
        feature_cols = feature_cols,
        blueprint = prep$blueprint,
        dropped_model_cols = dropped_model_cols,
        terms = mm$terms,
        x_columns = colnames(mm$x),
        rfx_group_col = bart_fit$rfx_group_col,
        rfx_levels = bart_fit$rfx_levels,
        outcome_transform = outcome_transform,
        prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
        training_n = nrow(model_frame),
        model_matrix_ncol = ncol(mm$x)
      ),
      class = "tsb_meta_policy_learner"
    ))
  }

  if (identical(method_spec$family, "knn")) {
    if (!requireNamespace("FNN", quietly = TRUE)) {
      stop(
        "Fitting method 'knn' requires the suggested package 'FNN' to be installed.",
        call. = FALSE
      )
    }
    # KNN is a lazy learner: "fitting" standardizes the training design matrix
    # (KNN distances are scale-sensitive) and stashes it with y and k. The stored
    # column centers/scales are reapplied to new data at predict time.
    mm <- meta_policy_model_matrix(model_frame)
    center <- colMeans(mm$x)
    scale <- apply(mm$x, 2L, stats::sd)
    scale[!is.finite(scale) | scale == 0] <- 1
    x_std <- scale(mm$x, center = center, scale = scale)
    return(structure(
      list(
        fit = list(
          x = x_std,
          y = as.numeric(y),
          k = as.integer(fit_arguments$k %||% 10L),
          center = center,
          scale = scale
        ),
        method = method,
        method_family = method_spec$family,
        method_variant = method_spec$variant,
        method_settings = method_settings,
        method_arguments = fit_arguments,
        feature_cols = feature_cols,
        blueprint = prep$blueprint,
        dropped_model_cols = dropped_model_cols,
        terms = mm$terms,
        x_columns = colnames(mm$x),
        outcome_transform = outcome_transform,
        prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
        training_n = nrow(model_frame),
        model_matrix_ncol = ncol(mm$x)
      ),
      class = "tsb_meta_policy_learner"
    ))
  }

  if (identical(method_spec$family, "svr") || identical(method_spec$family, "gpr")) {
    if (!requireNamespace("kernlab", quietly = TRUE)) {
      stop(
        sprintf(
          "Fitting method '%s' requires the suggested package 'kernlab' to be installed.",
          method
        ),
        call. = FALSE
      )
    }
    kernel_settings <- resolve_meta_policy_kernlab_settings(fit_arguments)
    kernel_rows <- sample_meta_policy_kernel_training_rows(
      n = nrow(model_frame),
      max_train_rows = fit_arguments$max_train_rows %||% NULL,
      seed = seed
    )
    model_frame_fit <- model_frame[kernel_rows, , drop = FALSE]
    y_fit <- y[kernel_rows]
    if (length(kernel_rows) < nrow(model_frame)) {
      report_progress(
        progress,
        sprintf(
          "  Meta-policy %s: fitting kernlab learner on %d/%d training rows.",
          method,
          length(kernel_rows),
          nrow(model_frame)
        )
      )
    }
    mm <- meta_policy_model_matrix(model_frame_fit)
    keep_kernel_col <- vapply(seq_len(ncol(mm$x)), function(j) {
      vals <- mm$x[, j]
      vals <- vals[is.finite(vals)]
      length(unique(vals)) >= 2L
    }, logical(1))
    kernel_dropped_cols <- colnames(mm$x)[!keep_kernel_col]
    mm$x <- mm$x[, keep_kernel_col, drop = FALSE]
    if (ncol(mm$x) == 0L) {
      stop("No non-constant kernlab model-matrix columns remained after preprocessing.", call. = FALSE)
    }
    if (length(kernel_dropped_cols) > 0L) {
      shown_cols <- head(kernel_dropped_cols, 40L)
      warning(
        sprintf(
          "Meta-policy %s dropped %d constant kernlab model-matrix column(s): %s%s",
          method,
          length(kernel_dropped_cols),
          paste(shown_cols, collapse = ", "),
          if (length(kernel_dropped_cols) > length(shown_cols)) {
            sprintf(", ... +%d more", length(kernel_dropped_cols) - length(shown_cols))
          } else {
            ""
          }
        ),
        call. = FALSE
      )
    }
    # Both kernlab learners take a numeric matrix (built via the shared
    # model-matrix path). Constant columns are dropped before fitting because
    # kernlab's internal scaler warns on them in every fold. A configured sigma
    # is passed through kpar; otherwise kernlab uses its automatic sigest path.
    fit <- if (identical(method_spec$family, "svr")) {
      kernlab::ksvm(
        x = mm$x,
        y = as.numeric(y_fit),
        type = "eps-svr",
        kernel = kernel_settings$kernel,
        kpar = kernel_settings$kpar,
        C = as.numeric(fit_arguments$C %||% 1),
        epsilon = as.numeric(fit_arguments$epsilon %||% 0.1),
        scaled = TRUE
      )
    } else {
      kernlab::gausspr(
        x = mm$x,
        y = as.numeric(y_fit),
        type = "regression",
        kernel = kernel_settings$kernel,
        kpar = kernel_settings$kpar,
        var = as.numeric(fit_arguments$var %||% 0.001),
        scaled = TRUE
      )
    }
    return(structure(
      list(
        fit = fit,
        method = method,
        method_family = method_spec$family,
        method_variant = method_spec$variant,
        method_settings = method_settings,
        method_arguments = fit_arguments,
        feature_cols = feature_cols,
        blueprint = prep$blueprint,
        dropped_model_cols = c(dropped_model_cols, kernel_dropped_cols),
        terms = mm$terms,
        x_columns = colnames(mm$x),
        outcome_transform = outcome_transform,
        prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
        training_n = nrow(model_frame_fit),
        source_training_n = nrow(model_frame),
        training_row_index = kernel_rows,
        model_matrix_ncol = ncol(mm$x)
      ),
      class = "tsb_meta_policy_learner"
    ))
  }

  if (identical(method_spec$family, "lmm")) {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop(
        "Fitting method 'lmm' requires the suggested package 'lme4' to be installed.",
        call. = FALSE
      )
    }

    # Resolve one conservative random-intercept grouping column before fitting.
    lmm_group <- resolve_meta_policy_lmm_group(
      training_data = training_data,
      random_intercept = fit_arguments$random_intercept
    )
    model_data[[lmm_group$group_col]] <- lmm_group$group_factor

    fixed_terms <- setdiff(names(model_frame), lmm_group$group_col)
    fixed_term_string <- if (length(fixed_terms) == 0) {
      "1"
    } else {
      paste(fixed_terms, collapse = " + ")
    }
    formula_now <- stats::as.formula(sprintf(
      ".outcome_model ~ %s + (1 | `%s`)",
      fixed_term_string,
      lmm_group$group_col
    ))

    fit <- lme4::lmer(
      formula_now,
      data = model_data,
      REML = identical(as.character(fit_arguments$fit_method %||% "REML"), "REML"),
      ...
    )
    return(structure(
      list(
        fit = fit,
        method = method,
        method_family = method_spec$family,
        method_variant = method_spec$variant,
        method_settings = method_settings,
        method_arguments = fit_arguments,
        feature_cols = feature_cols,
        blueprint = prep$blueprint,
        dropped_model_cols = dropped_model_cols,
        factor_levels = lapply(
          model_data[intersect(feature_cols, names(model_data))],
          function(x) if (is.factor(x)) levels(x) else NULL
        ),
        group_col = lmm_group$group_col,
        group_levels = levels(lmm_group$group_factor),
        outcome_transform = outcome_transform,
        prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
        training_n = nrow(model_data)
      ),
      class = "tsb_meta_policy_learner"
    ))
  }

  fit <- switch(method_spec$family,
    qreg = quantreg::rq(
      formula_now,
      data = model_data,
      tau = as.numeric(fit_arguments$tau %||% 0.50),
      ...
    ),
    glm = stats::glm(formula_now, data = model_data, family = stats::gaussian()),
    gam = mgcv::gam(
      formula_now,
      data = model_data,
      family = stats::gaussian(),
      method = fit_arguments$method %||% "REML",
      select = isTRUE(fit_arguments$select %||% TRUE),
      ...
    ),
    mars = {
      # earth is an optional dependency; guard on it the same way the lmm
      # branch guards on lme4 so a missing package fails loudly rather than
      # silently degrading the library.
      if (!requireNamespace("earth", quietly = TRUE)) {
        stop(
          "Fitting method 'mars' requires the suggested package 'earth' to be installed.",
          call. = FALSE
        )
      }
      # earth derives its own hinge basis from the plain additive formula, so
      # the resolved degree/penalty/nprune/pmethod arguments are passed straight
      # through without any smooth-term construction on our side.
      do.call(
        earth::earth,
        c(list(formula_now, data = model_data), fit_arguments)
      )
    },
    # Intercept-only baseline: an ordinary least-squares fit on the constant
    # term stores the (transformed) training-set mean and predicts it for every
    # new row via the generic predict path below.
    mean = stats::lm(.outcome_model ~ 1, data = model_data),
    rpart = rpart::rpart(
      formula_now,
      data = model_data,
      control = fit_arguments$control %||% rpart::rpart.control(),
      ...
    ),
    rf = do.call(
      ranger::ranger,
      c(
        list(
          x = as.data.frame(model_frame),
          y = y,
          num.threads = 1L,
          verbose = FALSE,
          num.trees = as.integer(fit_arguments$num.trees %||% 500L),
          mtry = fit_arguments$mtry %||% NULL,
          min.node.size = as.integer(fit_arguments$min.node.size %||% 5L),
          sample.fraction = as.numeric(fit_arguments$sample.fraction %||% 1),
          replace = isTRUE(fit_arguments$replace %||% TRUE),
          respect.unordered.factors = fit_arguments$respect.unordered.factors %||% "order"
        ),
        if (!is.null(seed)) list(seed = as.integer(seed)) else list(),
        list(...)
      )
    ),
    cubist = {
      if (!requireNamespace("Cubist", quietly = TRUE)) {
        stop(
          "Fitting method 'cubist' requires the suggested package 'Cubist' to be installed.",
          call. = FALSE
        )
      }
      # Cubist uses an x/y interface (data frame of predictors); `neighbors` is a
      # predict-time correction, carried in method_arguments to the predict path.
      Cubist::cubist(
        x = sanitize_cubist_frame(as.data.frame(model_frame)),
        y = y,
        committees = as.integer(fit_arguments$committees %||% 1L)
      )
    },
    qrf = {
      if (!requireNamespace("ranger", quietly = TRUE)) {
        stop(
          "Fitting method 'qrf' requires the suggested package 'ranger' to be installed.",
          call. = FALSE
        )
      }
      # Quantile regression forest: quantreg + keep.inbag enable conditional
      # quantile prediction; num.threads pinned to 1 like the ranger learner.
      do.call(
        ranger::ranger,
        c(
          list(
            x = as.data.frame(model_frame),
            y = y,
            num.threads = 1L,
            verbose = FALSE,
            quantreg = TRUE,
            keep.inbag = TRUE,
            num.trees = as.integer(fit_arguments$num.trees %||% 500L),
            mtry = fit_arguments$mtry %||% NULL,
            min.node.size = as.integer(fit_arguments$min.node.size %||% 10L),
            sample.fraction = as.numeric(fit_arguments$sample.fraction %||% 1),
            replace = isTRUE(fit_arguments$replace %||% TRUE),
            max.depth = if (is.null(fit_arguments$max.depth)) NULL else as.integer(fit_arguments$max.depth)
          ),
          if (!is.null(seed)) list(seed = as.integer(seed)) else list()
        )
      )
    }
  )

  if (identical(method_spec$family, "rf")) {
    fit$predictions <- NULL
    fit$call <- NULL
  }

  structure(
    list(
      fit = fit,
      method = method,
      method_family = method_spec$family,
      method_variant = method_spec$variant,
      method_settings = method_settings,
      method_arguments = fit_arguments,
      feature_cols = feature_cols,
      blueprint = prep$blueprint,
      dropped_model_cols = dropped_model_cols,
      factor_levels = lapply(
        model_data[intersect(feature_cols, names(model_data))],
        function(x) if (is.factor(x)) levels(x) else NULL
      ),
      outcome_transform = outcome_transform,
      prediction_cap = meta_policy_prediction_cap(training_data$.outcome),
      training_n = nrow(model_data)
    ),
    class = "tsb_meta_policy_learner"
  )
}

#' Predict policy-specific transferability score with a meta-policy learner
#'
#' @param object Fitted object from `fit_meta_policy_learner()`.
#' @param new_policy_tbl New policy table.
#'
#' @return `new_policy_tbl` with `.meta_predicted_score`.
#'
#' @examples
#' \dontrun{
#' scored <- predict_meta_policy_score(
#'   learner,
#'   new_policy_tbl = selector@predictions
#' )
#' }
#' @keywords internal
#' @noRd
predict_meta_policy_score <- function(object,
                                      new_policy_tbl) {
  if (!inherits(object, "tsb_meta_policy_learner")) {
    stop("'object' must be a fitted meta-policy learner.", call. = FALSE)
  }
  new_policy_tbl <- tibble::as_tibble(new_policy_tbl)
  if (!all(object$feature_cols %in% names(new_policy_tbl))) {
    missing_cols <- setdiff(object$feature_cols, names(new_policy_tbl))
    for (nm in missing_cols) {
      new_policy_tbl[[nm]] <- NA
    }
  }

  prediction_tbl <- new_policy_tbl
  method_family <- object$method_family %||% object$method
  if (identical(object$method, "super_learner")) {
    if (length(object$fit) == 0 || length(object$weights) == 0) {
      stop("The super-learner object does not contain fitted base learners.", call. = FALSE)
    }
    pred_mat <- vapply(names(object$fit), function(method_now) {
      scored <- predict_meta_policy_score(object$fit[[method_now]], new_policy_tbl)
      scored$.meta_predicted_score
    }, numeric(nrow(new_policy_tbl)))
    if (is.null(dim(pred_mat))) {
      dim(pred_mat) <- c(nrow(new_policy_tbl), length(object$fit))
      colnames(pred_mat) <- names(object$fit)
    }
    weights <- object$weights[colnames(pred_mat)]
    if (any(!is.finite(weights))) {
      stop("The meta-policy Super Learner weights do not align with its fitted base learners.", call. = FALSE)
    }
    pred <- as.numeric(pred_mat %*% weights)
    pred <- pmin(pmax(0, pred), object$prediction_cap %||% Inf)
    meta_disagreement <- sqrt(rowSums(
      sweep(pred_mat, 1L, as.numeric(pred_mat %*% weights), "-")^2 *
        rep(as.numeric(weights), each = nrow(pred_mat))
    ))
    meta_diagnostic_available <- TRUE
  } else if (identical(method_family, "glm_penalized") || identical(method_family, "xgboost") || identical(method_family, "qreg") || identical(method_family, "bart") || identical(method_family, "knn") || identical(method_family, "svr") || identical(method_family, "gpr")) {
    prep <- meta_policy_blueprint(
      prediction_tbl,
      object$feature_cols,
      blueprint = object$blueprint
    )
    pred_frame <- prep$data[, setdiff(names(prep$data), object$dropped_model_cols %||% character()), drop = FALSE]
    mm <- meta_policy_model_matrix(
      pred_frame,
      terms_obj = object$terms,
      x_columns = object$x_columns
    )
    pred <- if (identical(method_family, "glm_penalized")) {
      glmnet_predict <- if (inherits(object$fit, "cv.glmnet")) {
        utils::getFromNamespace("predict.cv.glmnet", "glmnet")
      } else {
        utils::getFromNamespace("predict.glmnet", "glmnet")
      }
      as.numeric(glmnet_predict(object$fit, newx = mm$x, s = object$lambda))
    } else if (identical(method_family, "qreg")) {
      coef_now <- suppressWarnings(as.numeric(object$fit$coefficients %||% object$fit$coef))
      coef_now[!is.finite(coef_now)] <- 0
      as.numeric(mm$x %*% coef_now)
    } else if (identical(method_family, "bart")) {
      predict_stochtree_bart(object, mm$x, prediction_tbl)
    } else if (identical(method_family, "knn")) {
      predict_knn_regression(object, mm$x)
    } else if (identical(method_family, "svr") || identical(method_family, "gpr")) {
      # kernlab dispatches an S4 predict method for ksvm/gausspr; return the mean.
      as.numeric(kernlab::predict(object$fit, mm$x))
    } else {
      as.numeric(stats::predict(object$fit, newdata = xgboost::xgb.DMatrix(mm$x)))
    }
  } else if (identical(method_family, "qrf")) {
    prep <- meta_policy_blueprint(
      prediction_tbl,
      object$feature_cols,
      blueprint = object$blueprint
    )
    pred_frame <- prep$data[, setdiff(names(prep$data), object$dropped_model_cols %||% character()), drop = FALSE]
    ranger_predict <- utils::getFromNamespace("predict.ranger", "ranger")
    quantile_now <- as.numeric(object$method_arguments$quantile %||% 0.9)
    preds <- ranger_predict(
      object$fit,
      data = pred_frame,
      type = "quantiles",
      quantiles = quantile_now
    )$predictions
    pred <- as.numeric(if (is.null(dim(preds))) preds else preds[, 1L])
  } else if (identical(method_family, "rf")) {
    prep <- meta_policy_blueprint(
      prediction_tbl,
      object$feature_cols,
      blueprint = object$blueprint
    )
    pred_frame <- prep$data[, setdiff(names(prep$data), object$dropped_model_cols %||% character()), drop = FALSE]
    ranger_predict <- utils::getFromNamespace("predict.ranger", "ranger")
    pred <- as.numeric(ranger_predict(object$fit, data = pred_frame)$predictions)
  } else {
    prep <- meta_policy_blueprint(
      prediction_tbl,
      object$feature_cols,
      blueprint = object$blueprint
    )
    pred_frame <- prep$data[, setdiff(names(prep$data), object$dropped_model_cols %||% character()), drop = FALSE]
    fit_xlevels <- tryCatch(
      object$fit$xlevels,
      error = function(e) NULL
    )
    for (nm in intersect(names(object$factor_levels %||% list()), names(pred_frame))) {
      levels_now <- fit_xlevels[[nm]] %||% object$factor_levels[[nm]]
      if (!is.null(levels_now)) {
        vals <- as.character(pred_frame[[nm]])
        fallback_level <- if ("missing" %in% levels_now) "missing" else levels_now[[1]]
        vals[is.na(vals) | !nzchar(vals) | !vals %in% levels_now] <- fallback_level
        pred_frame[[nm]] <- factor(vals, levels = levels_now)
      }
    }
    if (identical(method_family, "glm")) {
      mm <- stats::model.matrix(stats::delete.response(stats::terms(object$fit)), data = pred_frame)
      coef_now <- stats::coef(object$fit)
      coef_now[!is.finite(coef_now)] <- 0
      missing_cols <- setdiff(names(coef_now), colnames(mm))
      if (length(missing_cols) > 0) {
        add <- matrix(0, nrow = nrow(mm), ncol = length(missing_cols))
        colnames(add) <- missing_cols
        mm <- cbind(mm, add)
      }
      mm <- mm[, names(coef_now), drop = FALSE]
      pred <- as.numeric(mm %*% coef_now)
    } else if (identical(method_family, "lmm")) {
      # Carry the stored grouping column forward and allow new levels so cold
      # groups fall back to the fixed-effect mean rather than erroring.
      group_col <- as.character(object$group_col %||% "")[[1]]
      group_values <- rep("missing", nrow(pred_frame))
      if (nzchar(group_col)) {
        group_values <- if (group_col %in% names(prediction_tbl)) {
          as.character(prediction_tbl[[group_col]])
        } else {
          rep("missing", nrow(pred_frame))
        }
        group_values[is.na(group_values) | !nzchar(group_values)] <- "missing"
        pred_frame[[group_col]] <- factor(
          group_values,
          levels = unique(c(object$group_levels %||% character(), group_values))
        )
      }
      # lme4's predict() builds the full fixed-effect design matrix from the
      # formula. When the training fit was rank deficient, fixef() omits
      # aliased columns but the new matrix retains them, causing
      # `X %*% fixef(object)` to be non-conformable. Align by coefficient name
      # explicitly, then add the fitted random intercept for known groups.
      fixed_formula <- lme4::nobars(stats::formula(object$fit))
      fixed_terms <- stats::delete.response(stats::terms(fixed_formula))
      fixed_matrix <- stats::model.matrix(fixed_terms, data = pred_frame)
      fixed_coef <- lme4::fixef(object$fit)
      missing_fixed <- setdiff(names(fixed_coef), colnames(fixed_matrix))
      if (length(missing_fixed) > 0L) {
        add <- matrix(0, nrow = nrow(fixed_matrix), ncol = length(missing_fixed))
        colnames(add) <- missing_fixed
        fixed_matrix <- cbind(fixed_matrix, add)
      }
      fixed_matrix <- fixed_matrix[, names(fixed_coef), drop = FALSE]
      pred <- as.numeric(fixed_matrix %*% fixed_coef)
      if (nzchar(group_col)) {
        # ranef() returns a data frame whose *rownames* carry the group levels,
        # and extracting the intercept column drops them. The names have to be
        # restored explicitly: without them the match() below returns NA for
        # every row and the zeroing guard silently discards the whole random
        # effect, for known and unknown groups alike.
        random_effects <- tryCatch(
          {
            ranef_now <- lme4::ranef(object$fit)[[group_col]]
            stats::setNames(
              as.numeric(ranef_now[, "(Intercept)"]),
              rownames(ranef_now)
            )
          },
          error = function(e) NULL
        )
        if (!is.null(random_effects)) {
          # Groups absent from training have no fitted intercept, so they
          # legitimately fall back to the fixed-effect mean.
          random_add <- unname(random_effects[match(group_values, names(random_effects))])
          random_add[!is.finite(random_add)] <- 0
          pred <- pred + random_add
        }
      }
    } else if (identical(method_family, "mars")) {
      earth_predict <- utils::getFromNamespace("predict.earth", "earth")
      pred <- as.numeric(earth_predict(object$fit, newdata = pred_frame))
    } else if (identical(method_family, "cubist")) {
      # `neighbors` (0-9) applies Cubist's instance-based prediction correction.
      # The prediction frame must be sanitized exactly as the training frame was,
      # or its levels will not match the fitted model's.
      cubist_predict <- utils::getFromNamespace("predict.cubist", "Cubist")
      pred <- as.numeric(cubist_predict(
        object$fit,
        sanitize_cubist_frame(as.data.frame(pred_frame)),
        neighbors = as.integer(object$method_arguments$neighbors %||% 0L)
      ))
    } else {
      pred <- as.numeric(stats::predict(object$fit, newdata = pred_frame))
    }
  }
  if (!identical(object$method, "super_learner")) {
    pred <- inverse_meta_policy_outcome(
      pred,
      object$outcome_transform,
      prediction_cap = object$prediction_cap %||% Inf
    )
  }

  if (!exists("meta_disagreement", inherits = FALSE)) {
    meta_disagreement <- rep(NA_real_, nrow(new_policy_tbl))
    meta_diagnostic_available <- FALSE
  }
  new_policy_tbl |>
    dplyr::mutate(
      .meta_predicted_score = pred,
      .meta_score_weighted_disagreement = meta_disagreement,
      .meta_score_diagnostic_available = meta_diagnostic_available
    )
}

# Namespace-local payload used by forked meta-policy cross-fit workers.
.meta_policy_crossfit_payload <- new.env(parent = emptyenv())

#' Set the namespace-local meta-policy cross-fit payload
#'
#' Stores cross-fit fold data and scalar learner settings in a namespace-local
#' environment. Fork workers inherit this environment by copy-on-write so only
#' fold indices need to be sent over worker connections.
#'
#' @param payload Named list containing `fold_splits` and learner settings.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
set_meta_policy_crossfit_payload <- function(payload) {
  rm(
    list = ls(envir = .meta_policy_crossfit_payload, all.names = TRUE),
    envir = .meta_policy_crossfit_payload
  )
  list2env(payload, envir = .meta_policy_crossfit_payload)
  invisible(NULL)
}

#' Clear the namespace-local meta-policy cross-fit payload
#'
#' Removes stored fold data and learner settings after cross-fitting so large
#' benchmark objects are not retained longer than needed.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
clear_meta_policy_crossfit_payload <- function() {
  rm(
    list = ls(envir = .meta_policy_crossfit_payload, all.names = TRUE),
    envir = .meta_policy_crossfit_payload
  )
  invisible(NULL)
}

#' Run one meta-policy cross-fit fold from an explicit payload
#'
#' Fits the configured meta-policy learner on one pre-sliced train/test fold and
#' returns predictions with fold and learner timing attributes attached.
#'
#' @param fold_split List with `train`, `test`, and `fold_id` entries.
#' @param payload Named list of learner settings.
#'
#' @return Tibble of fold predictions with timing attributes.
#' @keywords internal
#' @noRd
run_meta_policy_crossfit_payload_fold <- function(fold_split,
                                                  payload) {
  fold_start <- proc.time()
  f <- fold_split$fold_id
  train <- fold_split$train
  test <- fold_split$test
  if (nrow(train) == 0 || nrow(test) == 0) {
    out <- tibble::tibble()
    attr(out, "fold_timing") <- tibble::tibble(
      fold_id = as.integer(f),
      n_train = nrow(train),
      n_test = nrow(test),
      seconds = unname((proc.time() - fold_start)[["elapsed"]]),
      succeeded = FALSE
    )
    return(out)
  }
  seed <- payload$seed
  outer_seed <- if (is.null(seed)) NULL else as.integer(seed) + f
  if (isTRUE(payload$is_super)) {
    stream_super_learner_fold <- utils::getFromNamespace(
      "stream_super_learner_fold",
      "tsbiomass"
    )
    preds <- stream_super_learner_fold(
      train_data = train,
      test_data = test,
      feature_cols = payload$feature_cols,
      outcome_transform = payload$outcome_transform,
      lambda_rule = payload$lambda_rule,
      inner_folds = payload$inner_folds,
      seed = outer_seed,
      super_methods = payload$super_methods,
      metalearner_loss = payload$metalearner_loss,
      method_settings = payload$method_settings,
      progress = FALSE
    )
  } else {
    fit_meta_policy_learner <- utils::getFromNamespace("fit_meta_policy_learner", "tsbiomass")
    predict_meta_policy_score <- utils::getFromNamespace("predict_meta_policy_score", "tsbiomass")
    learner <- fit_meta_policy_learner(
      training_data = train,
      method = payload$method,
      feature_cols = payload$feature_cols,
      outcome_transform = payload$outcome_transform,
      lambda_rule = payload$lambda_rule,
      alpha = payload$alpha,
      inner_folds = payload$inner_folds,
      super_methods = payload$super_methods,
      metalearner_loss = payload$metalearner_loss,
      seed = outer_seed,
      method_settings = payload$method_settings
    )
    preds <- predict_meta_policy_score(learner, test)
  }
  learner_timings <- attr(preds, "learner_timings")
  out <- preds |> dplyr::mutate(fold_id = f)
  attr(out, "fold_timing") <- tibble::tibble(
    fold_id = as.integer(f),
    n_train = nrow(train),
    n_test = nrow(test),
    seconds = unname((proc.time() - fold_start)[["elapsed"]]),
    succeeded = TRUE
  )
  if (is.data.frame(learner_timings) && nrow(learner_timings) > 0L) {
    attr(out, "learner_timings") <- learner_timings |>
      dplyr::mutate(outer_fold = f)
  }
  out
}

#' Run one meta-policy super-learner method-fold task
#'
#' Fits one configured base learner for one outer cross-fit fold, including its
#' inner out-of-fold predictions on the outer training split and its refit
#' predictions on the outer test split. The caller combines the returned
#' method-level results into fold-level Super Learner weights.
#'
#' @param task List with `fold_id` and `method` entries.
#' @param payload Named list containing `fold_splits` and learner settings.
#'
#' @return List containing OOF predictions, test predictions, and timings.
#' @keywords internal
#' @noRd
run_meta_policy_crossfit_method_payload_task <- function(task,
                                                         payload) {
  fold_id <- as.integer(task$fold_id)
  method_now <- as.character(task$method)
  fold_split <- payload$fold_splits[[fold_id]]
  train_data <- tibble::as_tibble(fold_split$train)
  test_data <- tibble::as_tibble(fold_split$test)
  outer_seed <- if (is.null(payload$seed)) NULL else as.integer(payload$seed) + fold_id
  task_start <- proc.time()
  oof_seconds <- NA_real_
  refit_seconds <- NA_real_
  report_progress(
    isTRUE(payload$progress),
    sprintf(
      "  Method-fold task started: fold=%d method=%s pid=%d train=%d test=%d.",
      fold_id,
      method_now,
      Sys.getpid(),
      nrow(train_data),
      nrow(test_data)
    )
  )

  if (nrow(train_data) == 0L || nrow(test_data) == 0L) {
    return(list(
      fold_id = fold_id,
      method = method_now,
      train_n = nrow(train_data),
      test_n = nrow(test_data),
      oof_pred = NULL,
      test_pred = NULL,
      oof_seconds = oof_seconds,
      refit_seconds = refit_seconds,
      total_seconds = unname((proc.time() - task_start)[["elapsed"]]),
      succeeded_oof = FALSE,
      succeeded_refit = FALSE,
      error = "empty outer train/test split"
    ))
  }

  foldid <- if (".split_group" %in% names(train_data)) {
    grouped_foldid(train_data$.split_group, n_folds = payload$inner_folds, seed = outer_seed)
  } else {
    NULL
  }
  foldid <- foldid %||% row_foldid(nrow(train_data), n_folds = payload$inner_folds, seed = outer_seed)
  if (is.null(foldid)) {
    return(list(
      fold_id = fold_id,
      method = method_now,
      train_n = nrow(train_data),
      test_n = nrow(test_data),
      oof_pred = NULL,
      test_pred = NULL,
      oof_seconds = oof_seconds,
      refit_seconds = refit_seconds,
      total_seconds = unname((proc.time() - task_start)[["elapsed"]]),
      succeeded_oof = FALSE,
      succeeded_refit = FALSE,
      error = "inner cross-validation requires at least two training rows"
    ))
  }

  oof_start <- proc.time()
  oof_pred <- rep(NA_real_, nrow(train_data))
  ok_oof <- TRUE
  error_msg <- NA_character_
  for (fold_now in sort(unique(foldid))) {
    tr_idx <- which(foldid != fold_now)
    vl_idx <- which(foldid == fold_now)
    if (length(tr_idx) == 0L || length(vl_idx) == 0L) {
      ok_oof <- FALSE
      error_msg <- "empty inner train/validation split"
      break
    }
    fold_seed <- if (is.null(outer_seed)) NULL else as.integer(outer_seed) + as.integer(fold_now)
    bl <- fit_meta_policy_ensemble_base(
      training_data = train_data[tr_idx, , drop = FALSE],
      method = method_now,
      feature_cols = payload$feature_cols,
      outcome_transform = payload$outcome_transform,
      lambda_rule = payload$lambda_rule,
      inner_folds = payload$inner_folds,
      seed = fold_seed,
      method_settings = payload$method_settings
    )
    if (inherits(bl, "try-error")) {
      ok_oof <- FALSE
      error_msg <- as.character(bl)[[1]]
      break
    }
    sc <- try(predict_meta_policy_score(bl, train_data[vl_idx, , drop = FALSE]), silent = TRUE)
    if (inherits(sc, "try-error") || !".meta_predicted_score" %in% names(sc)) {
      ok_oof <- FALSE
      error_msg <- if (inherits(sc, "try-error")) as.character(sc)[[1]] else "missing .meta_predicted_score"
      break
    }
    oof_pred[vl_idx] <- sc$.meta_predicted_score
    rm(bl, sc)
  }
  oof_seconds <- unname((proc.time() - oof_start)[["elapsed"]])
  ok_oof <- ok_oof && all(is.finite(oof_pred))
  if (!ok_oof) {
    return(list(
      fold_id = fold_id,
      method = method_now,
      train_n = nrow(train_data),
      test_n = nrow(test_data),
      oof_pred = NULL,
      test_pred = NULL,
      oof_seconds = oof_seconds,
      refit_seconds = refit_seconds,
      total_seconds = unname((proc.time() - task_start)[["elapsed"]]),
      succeeded_oof = FALSE,
      succeeded_refit = FALSE,
      error = error_msg %||% "incomplete OOF predictions"
    ))
  }

  refit_start <- proc.time()
  test_pred <- NULL
  bl <- fit_meta_policy_ensemble_base(
    training_data = train_data,
    method = method_now,
    feature_cols = payload$feature_cols,
    outcome_transform = payload$outcome_transform,
    lambda_rule = payload$lambda_rule,
    inner_folds = payload$inner_folds,
    seed = outer_seed,
    method_settings = payload$method_settings
  )
  if (!inherits(bl, "try-error")) {
    sc <- try(predict_meta_policy_score(bl, test_data), silent = TRUE)
    if (!inherits(sc, "try-error") && ".meta_predicted_score" %in% names(sc)) {
      test_pred <- sc$.meta_predicted_score
    } else {
      error_msg <- if (inherits(sc, "try-error")) as.character(sc)[[1]] else "missing .meta_predicted_score"
    }
    rm(bl, sc)
  } else {
    error_msg <- as.character(bl)[[1]]
  }
  refit_seconds <- unname((proc.time() - refit_start)[["elapsed"]])
  ok_refit <- is.numeric(test_pred) &&
    length(test_pred) == nrow(test_data) &&
    all(is.finite(test_pred))

  list(
    fold_id = fold_id,
    method = method_now,
    train_n = nrow(train_data),
    test_n = nrow(test_data),
    oof_pred = if (ok_refit) oof_pred else NULL,
    test_pred = if (ok_refit) test_pred else NULL,
    oof_seconds = oof_seconds,
    refit_seconds = refit_seconds,
    total_seconds = unname((proc.time() - task_start)[["elapsed"]]),
    succeeded_oof = ok_oof,
    succeeded_refit = ok_refit,
    error = if (ok_refit) NA_character_ else error_msg %||% "refit prediction failed"
  )
}

#' Run one inherited meta-policy super-learner method-fold task
#'
#' Fetches fold splits and learner settings from the namespace-local fork-worker
#' payload and evaluates one method-fold task.
#'
#' @param task List with `fold_id` and `method` entries.
#'
#' @return List containing OOF predictions, test predictions, and timings.
#' @keywords internal
#' @noRd
run_meta_policy_crossfit_method_task <- function(task) {
  payload <- as.list(.meta_policy_crossfit_payload)
  run_meta_policy_crossfit_method_payload_task(task, payload)
}

#' Build a failed meta-policy method-fold task result
#'
#' Creates the same result shape as a completed method-fold task when a worker
#' connection failure prevents the task from returning its own diagnostics.
#'
#' @param task List with `fold_id` and `method` entries.
#' @param message Failure message to attach.
#'
#' @return List containing failed task diagnostics.
#' @keywords internal
#' @noRd
failed_meta_policy_crossfit_method_task <- function(task,
                                                    message) {
  list(
    fold_id = as.integer(task$fold_id),
    method = as.character(task$method),
    train_n = NA_integer_,
    test_n = NA_integer_,
    oof_pred = NULL,
    test_pred = NULL,
    oof_seconds = NA_real_,
    refit_seconds = NA_real_,
    total_seconds = NA_real_,
    succeeded_oof = FALSE,
    succeeded_refit = FALSE,
    error = as.character(message %||% "method-fold task failed")
  )
}

#' Format meta-policy method-fold task labels
#'
#' Produces compact fold/method labels for queue progress and failure messages.
#'
#' @param tasks One task or a list of method-fold tasks.
#' @param max_labels Maximum number of labels to include.
#'
#' @return One comma-separated task-label string.
#' @keywords internal
#' @noRd
format_meta_policy_crossfit_method_tasks <- function(tasks,
                                                     max_labels = 12L) {
  if (is.null(tasks) || length(tasks) == 0L) {
    return("none")
  }
  if (!is.list(tasks[[1L]])) {
    tasks <- list(tasks)
  }
  max_labels <- max(1L, as.integer(max_labels %||% 12L))
  labels <- vapply(
    tasks,
    function(task) {
      sprintf(
        "fold=%d method=%s",
        as.integer(task$fold_id),
        as.character(task$method)
      )
    },
    character(1)
  )
  if (length(labels) > max_labels) {
    labels <- c(labels[seq_len(max_labels)], sprintf("... +%d more", length(labels) - max_labels))
  }
  paste(labels, collapse = ", ")
}

#' Log meta-policy method-fold task completion
#'
#' Emits one compact completion line and, for failed task results, emits the
#' captured error on the next log line.
#'
#' @param result Method-fold task result.
#' @param completed_n Number of completed tasks.
#' @param total_tasks Total number of method-fold tasks.
#' @param progress Logical. Emit progress messages.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
#' @noRd
log_meta_policy_crossfit_method_result <- function(result,
                                                   completed_n,
                                                   total_tasks,
                                                   progress = FALSE) {
  success_now <- isTRUE(result$succeeded_oof) && isTRUE(result$succeeded_refit)
  summary <- sprintf(
    "  %s Method-fold task complete: %d/%d [fold=%d method=%s success=%s].",
    if (success_now) "\u2705" else "\u274c",
    completed_n,
    total_tasks,
    as.integer(result$fold_id),
    as.character(result$method),
    success_now
  )
  if (success_now) {
    report_progress(progress, summary)
  } else {
    report_progress(
      progress,
      paste0(
        summary,
        "\n",
        as.character(result$error %||% "Unknown method-fold task failure.")
      )
    )
  }
  invisible(NULL)
}

#' Split a meta-policy method-fold suspect group
#'
#' Bisects a suspect task group after a worker-connection failure so only the
#' tasks that keep failing are narrowed further.
#'
#' @param tasks List of method-fold tasks.
#'
#' @return List of one or two task groups.
#' @keywords internal
#' @noRd
split_meta_policy_crossfit_suspect_group <- function(tasks) {
  if (length(tasks) == 0L) {
    return(list())
  }
  if (length(tasks) <= 1L) {
    return(list(tasks))
  }
  cut <- floor(length(tasks) / 2L)
  list(tasks[seq_len(cut)], tasks[(cut + 1L):length(tasks)])
}

#' Run one live meta-policy method-fold queue
#'
#' Submits method-fold tasks to worker slots, collects each result as soon as it
#' returns, and submits the next pending task to the freed worker. A socket
#' failure returns completed results plus the in-flight and unstarted tasks so
#' the caller can isolate the suspect set.
#'
#' @param tasks List of method-fold tasks.
#' @param payload Named list containing fold splits and learner settings.
#' @param workers Worker budget for this queue pass.
#' @param progress Logical. Emit progress messages.
#' @param completed_start Number of tasks completed before this pass.
#' @param total_tasks Total number of tasks in the full scheduler run.
#' @param retry_mode Logical. Whether this pass is retrying a suspect group.
#'
#' @return List with `completed`, `completed_n`, `failed_active`, `unstarted`,
#'   and `error` entries.
#' @keywords internal
#' @noRd
run_meta_policy_crossfit_method_queue_once <- function(tasks,
                                                       payload,
                                                       workers,
                                                       progress = FALSE,
                                                       completed_start = 0L,
                                                       total_tasks = length(tasks),
                                                       retry_mode = FALSE) {
  if (length(tasks) == 0L) {
    return(list(
      completed = list(),
      completed_n = as.integer(completed_start),
      failed_active = list(),
      unstarted = list(),
      error = NULL
    ))
  }
  workers <- max(1L, min(as.integer(workers %||% 1L), length(tasks)))
  report_progress(
    progress,
    sprintf(
      "  Method-fold queue: dispatching %d %stask(s) across %d worker(s).",
      length(tasks),
      if (retry_mode) "retry " else "",
      workers
    )
  )

  run_parent_sequential <- workers <= 1L && .Platform$OS.type != "unix"
  if (run_parent_sequential) {
    completed <- list()
    completed_n <- as.integer(completed_start)
    for (task in tasks) {
      result <- tryCatch(
        run_meta_policy_crossfit_method_payload_task(task, payload),
        error = function(e) failed_meta_policy_crossfit_method_task(task, conditionMessage(e))
      )
      completed_n <- completed_n + 1L
      log_meta_policy_crossfit_method_result(result, completed_n, total_tasks, progress)
      completed[[length(completed) + 1L]] <- result
    }
    return(list(
      completed = completed,
      completed_n = completed_n,
      failed_active = list(),
      unstarted = list(),
      error = NULL
    ))
  }

  set_meta_policy_crossfit_payload(payload)
  cluster_obj <- if (workers <= 1L && .Platform$OS.type == "unix") {
    cl <- parallel::makeForkCluster(1L)
    attr(cl, "cluster_type") <- "fork"
    cl
  } else {
    initialize_parallel_cluster(workers = workers)
  }
  cluster_type <- attr(cluster_obj, "cluster_type", exact = TRUE) %||% "unknown"
  if (!identical(cluster_type, "fork")) {
    run_method_task <- function(task) {
      run_meta_policy_crossfit_method_payload_task(task, payload)
    }
    environment(run_method_task) <- list2env(
      list(
        payload = payload,
        run_meta_policy_crossfit_method_payload_task = run_meta_policy_crossfit_method_payload_task
      ),
      parent = baseenv()
    )
    tsb_cluster_export(cluster_obj, c("run_method_task"), envir = environment())
  }

  pending <- tasks
  active <- vector("list", length(cluster_obj))
  names(active) <- as.character(seq_along(cluster_obj))
  parallel_send_call <- utils::getFromNamespace("sendCall", "parallel")
  parallel_recv_one_result <- utils::getFromNamespace("recvOneResult", "parallel")
  submit_task <- function(node_id, task) {
    if (identical(cluster_type, "fork")) {
      parallel_send_call(cluster_obj[[node_id]], run_meta_policy_crossfit_method_task, list(task))
    } else {
      parallel_send_call(cluster_obj[[node_id]], run_method_task, list(task))
    }
    active[[as.character(node_id)]] <<- task
    invisible(NULL)
  }

  for (node_id in seq_along(cluster_obj)) {
    if (length(pending) == 0L) {
      break
    }
    submit_task(node_id, pending[[1L]])
    pending <- pending[-1L]
  }
  report_progress(
    progress,
    "  Method-fold queue active tasks: ",
    format_meta_policy_crossfit_method_tasks(Filter(Negate(is.null), active))
  )

  completed <- list()
  completed_n <- as.integer(completed_start)
  queue_error <- NULL
  while (length(Filter(Negate(is.null), active)) > 0L) {
    received <- tryCatch(
      parallel_recv_one_result(cluster_obj),
      error = function(e) e
    )
    if (inherits(received, "error")) {
      queue_error <- conditionMessage(received)
      break
    }
    node_id <- as.character(received$node)
    task_done <- active[[node_id]]
    active[node_id] <- list(NULL)
    result <- if (inherits(received$value, "try-error") || inherits(received$value, "snow-try-error")) {
      failed_meta_policy_crossfit_method_task(
        task_done,
        as.character(received$value %||% "worker returned an error")
      )
    } else {
      received$value
    }
    completed_n <- completed_n + 1L
    log_meta_policy_crossfit_method_result(result, completed_n, total_tasks, progress)
    completed[[length(completed) + 1L]] <- result
    if (length(pending) > 0L) {
      submit_task(as.integer(node_id), pending[[1L]])
      pending <- pending[-1L]
    }
  }

  failed_active <- Filter(Negate(is.null), active)
  try(parallel::stopCluster(cluster_obj), silent = TRUE)
  clear_meta_policy_crossfit_payload()
  list(
    completed = completed,
    completed_n = completed_n,
    failed_active = failed_active,
    unstarted = pending,
    error = queue_error
  )
}

#' Run meta-policy Super Learner method-fold tasks through a worker queue
#'
#' Executes method-fold tasks as queue items. The scheduler submits work up to
#' the requested worker budget, collects each task as soon as a worker returns,
#' and immediately submits the next task to the freed worker. If a worker
#' connection fails, completed task results are retained and the in-flight
#' suspect set is bisected until only the failing task group is isolated.
#'
#' @param tasks List of method-fold tasks.
#' @param payload Named list containing fold splits and learner settings.
#' @param workers Requested worker budget.
#' @param progress Logical. Emit progress messages.
#'
#' @return List of method-fold task results.
#' @keywords internal
#' @noRd
run_meta_policy_crossfit_method_tasks <- function(tasks,
                                                  payload,
                                                  workers,
                                                  progress = FALSE) {
  total_tasks <- length(tasks)
  if (total_tasks == 0L) {
    return(list())
  }
  requested_workers <- max(1L, min(as.integer(workers %||% 1L), total_tasks))
  report_progress(
    progress,
    sprintf(
      "Policy learner method-fold scheduler: queue mode, %d total task(s), up to %d worker(s).",
      total_tasks,
      requested_workers
    )
  )

  completed_results <- list()
  pending_tasks <- tasks
  retry_groups <- list()
  completed_n <- 0L

  while (length(pending_tasks) > 0L || length(retry_groups) > 0L) {
    retry_mode <- length(retry_groups) > 0L
    if (retry_mode) {
      queue_tasks <- retry_groups[[1L]]
      retry_groups <- retry_groups[-1L]
      attempt_workers <- min(requested_workers, length(queue_tasks))
    } else {
      queue_tasks <- pending_tasks
      attempt_workers <- min(requested_workers, length(queue_tasks))
    }

    if (attempt_workers <= 1L && !retry_mode) {
      report_progress(
        progress,
        sprintf(
          "  Method-fold queue: running %d %stask(s) sequentially.",
          length(queue_tasks),
          if (retry_mode) "retry " else ""
        )
      )
      for (task in queue_tasks) {
        result <- tryCatch(
          run_meta_policy_crossfit_method_payload_task(task, payload),
          error = function(e) failed_meta_policy_crossfit_method_task(task, conditionMessage(e))
        )
        completed_n <- completed_n + 1L
        log_meta_policy_crossfit_method_result(result, completed_n, total_tasks, progress)
        completed_results[[length(completed_results) + 1L]] <- result
      }
      if (retry_mode) {
        retry_groups <- list()
      } else {
        pending_tasks <- list()
      }
      next
    }

    queue_result <- run_meta_policy_crossfit_method_queue_once(
      tasks = queue_tasks,
      payload = payload,
      workers = attempt_workers,
      progress = progress,
      completed_start = completed_n,
      total_tasks = total_tasks,
      retry_mode = retry_mode
    )
    completed_results <- c(completed_results, queue_result$completed)
    completed_n <- queue_result$completed_n
    if (is.null(queue_result$error)) {
      if (retry_mode) {
        if (length(queue_result$unstarted) > 0L) {
          retry_groups <- c(retry_groups, list(queue_result$unstarted))
        }
      } else {
        pending_tasks <- list()
      }
    } else {
      isolated_failure <- retry_mode &&
        length(queue_result$failed_active) == 1L &&
        length(queue_tasks) == 1L
      if (isolated_failure) {
        result <- failed_meta_policy_crossfit_method_task(
          queue_result$failed_active[[1L]],
          paste("socket failure isolated to this method-fold task:", queue_result$error)
        )
        completed_n <- completed_n + 1L
        completed_results[[length(completed_results) + 1L]] <- result
        report_progress(
          progress,
          paste0(
            sprintf(
              "  \u274c Method-fold task isolated after socket failure: %d/%d [fold=%d method=%s success=FALSE].",
              completed_n,
              total_tasks,
              as.integer(result$fold_id),
              as.character(result$method)
            ),
            "\n",
            as.character(result$error %||% "Unknown method-fold task failure.")
          )
        )
      } else {
        suspect_groups <- split_meta_policy_crossfit_suspect_group(queue_result$failed_active)
        if (retry_mode) {
          retry_groups <- c(
            suspect_groups,
            if (length(queue_result$unstarted) > 0L) list(queue_result$unstarted) else list(),
            retry_groups
          )
        } else {
          retry_groups <- c(suspect_groups, retry_groups)
          pending_tasks <- queue_result$unstarted
        }
        report_progress(
          progress,
          sprintf(
            "  \u26a0 Method-fold queue failed while active tasks were [%s]; retaining completed tasks and splitting suspect set into %d group(s); %d pending task(s) remain at full worker budget: %s",
            format_meta_policy_crossfit_method_tasks(queue_result$failed_active),
            length(suspect_groups),
            length(pending_tasks),
            queue_result$error
          )
        )
      }
    }
  }

  completed_results
}

#' Combine method-fold tasks for one meta-policy Super Learner fold
#'
#' Estimates fold-specific metalearner weights from completed base-learner OOF
#' predictions, combines refit predictions on the outer test split, and attaches
#' fold and learner timing attributes.
#'
#' @param fold_split List with `train`, `test`, and `fold_id` entries.
#' @param task_results List of method-fold task results for this fold.
#' @param payload Named list of learner settings.
#'
#' @return Tibble of fold predictions with timing attributes.
#' @keywords internal
#' @noRd
combine_meta_policy_super_fold_tasks <- function(fold_split,
                                                 task_results,
                                                 payload) {
  fold_id <- as.integer(fold_split$fold_id)
  train_data <- tibble::as_tibble(fold_split$train)
  test_data <- tibble::as_tibble(fold_split$test)
  methods <- payload$active_methods %||%
    available_meta_policy_super_methods(payload$super_methods, method_settings = payload$method_settings)
  timing_rows <- dplyr::bind_rows(lapply(task_results, function(result) {
    tibble::tibble(
      method = result$method,
      oof_seconds = result$oof_seconds %||% NA_real_,
      refit_seconds = result$refit_seconds %||% NA_real_,
      total_seconds = result$total_seconds %||% NA_real_,
      succeeded_oof = isTRUE(result$succeeded_oof),
      succeeded_refit = isTRUE(result$succeeded_refit),
      error = result$error %||% NA_character_
    )
  }))
  if (nrow(timing_rows) == 0L) {
    timing_rows <- tibble::tibble(
      method = character(),
      oof_seconds = double(),
      refit_seconds = double(),
      total_seconds = double(),
      succeeded_oof = logical(),
      succeeded_refit = logical(),
      error = character()
    )
  }
  learner_timings <- tibble::tibble(method = methods) |>
    dplyr::left_join(timing_rows, by = "method") |>
    dplyr::mutate(
      succeeded_oof = dplyr::coalesce(.data$succeeded_oof, FALSE),
      succeeded_refit = dplyr::coalesce(.data$succeeded_refit, FALSE),
      outer_fold = fold_id
    )

  ok_results <- task_results[vapply(
    task_results,
    function(result) {
      isTRUE(result$succeeded_oof) &&
        isTRUE(result$succeeded_refit) &&
        is.numeric(result$oof_pred) &&
        is.numeric(result$test_pred) &&
        length(result$oof_pred) == nrow(train_data) &&
        length(result$test_pred) == nrow(test_data) &&
        all(is.finite(result$oof_pred)) &&
        all(is.finite(result$test_pred))
    },
    logical(1)
  )]
  if (length(ok_results) == 0L) {
    stop("meta-policy super learner fold had no completed base learner tasks.", call. = FALSE)
  }

  pred_cols <- stats::setNames(lapply(ok_results, `[[`, "oof_pred"), vapply(ok_results, `[[`, character(1), "method"))
  oof_mat <- do.call(cbind, pred_cols)
  colnames(oof_mat) <- names(pred_cols)
  weights <- fit_super_learner_weights(oof_mat, train_data$.outcome, loss = payload$metalearner_loss)
  names(weights) <- colnames(oof_mat)
  weights <- weights[weights > 0]
  if (length(weights) == 0L) {
    stop("meta-policy super learner fold produced zero metalearner weight.", call. = FALSE)
  }
  weights <- weights / sum(weights)
  weight_lookup <- stats::setNames(rep(0, length(methods)), methods)
  weight_lookup[names(weights)] <- weights

  prediction_cap <- meta_policy_prediction_cap(train_data$.outcome)
  acc_pred <- rep(0, nrow(test_data))
  weight_sum <- 0
  test_mae_lookup <- stats::setNames(rep(NA_real_, length(methods)), methods)
  test_rmse_lookup <- stats::setNames(rep(NA_real_, length(methods)), methods)
  for (result in ok_results) {
    if (".outcome" %in% names(test_data)) {
      test_mae_lookup[[result$method]] <- mean(abs(test_data$.outcome - result$test_pred), na.rm = TRUE)
      test_rmse_lookup[[result$method]] <- sqrt(mean((test_data$.outcome - result$test_pred)^2, na.rm = TRUE))
    }
    weight_now <- if (result$method %in% names(weights)) weights[[result$method]] else 0
    if (!is.finite(weight_now) || weight_now <= 0) {
      next
    }
    acc_pred <- acc_pred + weight_now * result$test_pred
    weight_sum <- weight_sum + weight_now
  }
  if (weight_sum <= 0) {
    stop("meta-policy super learner fold had no weighted refit predictions.", call. = FALSE)
  }
  acc_pred <- pmin(pmax(0, acc_pred / weight_sum), prediction_cap)
  out <- test_data |>
    dplyr::mutate(
      .meta_predicted_score = acc_pred,
      fold_id = fold_id
    )
  fold_seconds <- learner_timings$total_seconds
  fold_seconds <- fold_seconds[is.finite(fold_seconds)]
  attr(out, "learner_timings") <- learner_timings |>
    dplyr::mutate(
      weight = unname(weight_lookup[.data$method]),
      test_mae = unname(test_mae_lookup[.data$method]),
      test_rmse = unname(test_rmse_lookup[.data$method])
    )
  attr(out, "fold_timing") <- tibble::tibble(
    fold_id = fold_id,
    n_train = nrow(train_data),
    n_test = nrow(test_data),
    seconds = if (length(fold_seconds) > 0L) max(fold_seconds) else NA_real_,
    succeeded = TRUE
  )
  out
}

#' Run one inherited meta-policy cross-fit fold
#'
#' Fetches one fold split by index from the namespace-local fork-worker payload
#' and evaluates it with [run_meta_policy_crossfit_payload_fold()].
#'
#' @param fold_id Integer fold index.
#'
#' @return Tibble of fold predictions with timing attributes.
#' @keywords internal
#' @noRd
run_meta_policy_crossfit_task <- function(fold_id) {
  fold_splits <- get("fold_splits", envir = .meta_policy_crossfit_payload, inherits = FALSE)
  payload <- as.list(.meta_policy_crossfit_payload)
  payload$fold_splits <- NULL
  run_meta_policy_crossfit_payload_fold(
    fold_split = fold_splits[[as.integer(fold_id)]],
    payload = payload
  )
}

#' Prepare a meta-policy cross-fit task plan
#'
#' Builds the same grouped outer folds, prepared training rows, fold payload,
#' and Super Learner method-fold task list used by
#' [crossfit_meta_policy_learner()]. Workflow orchestrators can schedule the
#' returned method-fold tasks externally and then recombine them with
#' [combine_meta_policy_crossfit_plan()].
#'
#' @param policy_perf Policy-performance table.
#' @param group_col Exchangeable grouping column.
#' @param n_folds Number of outer folds.
#' @param method Learner method.
#' @param seed Integer seed.
#' @param feature_cols Optional feature columns.
#' @param outcome_col Outcome column.
#' @param outcome_clip_quantile Optional outcome clipping quantile.
#' @param outcome_transform Outcome transform.
#' @param lambda_rule Regularization rule.
#' @param alpha Optional elastic-net alpha.
#' @param inner_folds Number of inner folds.
#' @param super_methods Optional Super Learner base methods.
#' @param metalearner_loss Metalearner loss.
#' @param method_settings Optional method settings.
#' @param progress Logical. Emit progress messages.
#'
#' @return A list containing fold splits, payload, method-fold tasks, and
#'   cross-fit metadata.
#' @keywords internal
#' @noRd
prepare_meta_policy_crossfit_plan <- function(policy_perf,
                                              group_col = NULL,
                                              n_folds = 5L,
                                              method = "glm",
                                              seed = NULL,
                                              feature_cols = NULL,
                                              outcome_col = "error_abs_log",
                                              outcome_clip_quantile = NULL,
                                              outcome_transform = "log1p",
                                              lambda_rule = "lambda.1se",
                                              alpha = NULL,
                                              inner_folds = 5L,
                                              super_methods = NULL,
                                              metalearner_loss = "squared_error",
                                              method_settings = NULL,
                                              progress = FALSE) {
  policy_perf <- tibble::as_tibble(policy_perf)
  group_col <- resolve_meta_policy_group_col(policy_perf, group_col = group_col)
  groups <- sort(unique(stats::na.omit(as.character(policy_perf[[group_col]]))))
  n_folds <- min(as.integer(n_folds), length(groups))
  if (!is.finite(n_folds) || n_folds < 2L) {
    stop("'n_folds' must be at least 2 and no larger than the number of groups.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  fold_tbl <- tibble::tibble(
    group_id = sample(groups, length(groups), replace = FALSE),
    fold_id = rep(seq_len(n_folds), length.out = length(groups))
  )

  is_super <- identical(as.character(method), "super_learner")
  active_methods <- if (is_super) {
    available_meta_policy_super_methods(super_methods, method_settings = method_settings)
  } else {
    method
  }
  lmm_random_intercepts <- active_meta_policy_lmm_random_intercepts(
    active_methods,
    method_settings = method_settings
  )

  data_all <- prepare_meta_policy_data(
    policy_perf = policy_perf,
    outcome_col = outcome_col,
    feature_cols = feature_cols,
    outcome_clip_quantile = outcome_clip_quantile,
    retain_cols = c(group_col, lmm_random_intercepts)
  ) |>
    dplyr::mutate(.split_group = as.character(.data[[group_col]])) |>
    dplyr::left_join(fold_tbl, by = c(".split_group" = "group_id"))

  if (!"fold_id" %in% names(data_all)) {
    if ("fold_id.y" %in% names(data_all)) {
      data_all$fold_id <- data_all$fold_id.y
    } else if ("fold_id.x" %in% names(data_all)) {
      data_all$fold_id <- data_all$fold_id.x
    }
  }
  data_all <- data_all |>
    dplyr::select(-dplyr::any_of(c("fold_id.x", "fold_id.y")))

  n_rows_data <- nrow(data_all)
  data_clip_quantile <- attr(data_all, "outcome_clip_quantile")
  data_clip_cap <- attr(data_all, "outcome_clip_cap")
  train_keep <- if (!is.null(feature_cols) && length(feature_cols) > 0L) {
    intersect(
      unique(c(
        feature_cols, ".outcome", ".split_group", "fold_id", lmm_random_intercepts
      )),
      names(data_all)
    )
  } else {
    names(data_all)
  }
  fold_splits <- lapply(seq_len(n_folds), function(f) {
    mask_train <- !is.na(data_all$fold_id) & data_all$fold_id != f
    mask_test <- !is.na(data_all$fold_id) & data_all$fold_id == f
    list(
      train = data_all[mask_train, train_keep, drop = FALSE],
      test = data_all[mask_test, , drop = FALSE],
      fold_id = f
    )
  })

  fold_payload <- list(
    is_super = is_super,
    active_methods = active_methods,
    feature_cols = feature_cols,
    method = method,
    outcome_transform = outcome_transform,
    lambda_rule = lambda_rule,
    alpha = alpha,
    inner_folds = inner_folds,
    super_methods = super_methods,
    metalearner_loss = metalearner_loss,
    method_settings = method_settings,
    seed = seed,
    progress = progress
  )
  method_tasks <- if (is_super) {
    unlist(
      lapply(seq_along(fold_splits), function(f) {
        lapply(active_methods, function(method_now) {
          list(fold_id = f, method = method_now)
        })
      }),
      recursive = FALSE
    )
  } else {
    list()
  }

  list(
    fold_assignments = fold_tbl,
    fold_splits = fold_splits,
    payload = fold_payload,
    method_tasks = method_tasks,
    method = method,
    is_super = is_super,
    active_methods = active_methods,
    n_folds = n_folds,
    n_rows = n_rows_data,
    n_base_methods = if (is_super) length(active_methods) else 1L,
    outcome_col = outcome_col,
    outcome_clip_quantile = data_clip_quantile,
    outcome_clip_cap = data_clip_cap
  )
}

#' Run one externalized meta-policy cross-fit method-fold task
#'
#' Executes a single Super Learner base-method/outer-fold task from a
#' self-contained task specification. This is the task unit used by external
#' workflow schedulers such as `targets`.
#'
#' @param task_spec List containing `task`, `fold_split`, and `payload`.
#'
#' @return Method-fold task result with the task identifier attached.
#' @keywords internal
#' @noRd
run_meta_policy_crossfit_plan_method_task <- function(task_spec) {
  task <- task_spec$task
  payload <- task_spec$payload
  fold_id <- as.integer(task$fold_id)
  fold_splits <- vector("list", fold_id)
  fold_splits[[fold_id]] <- task_spec$fold_split
  payload$fold_splits <- fold_splits
  out <- tryCatch(
    run_meta_policy_crossfit_method_payload_task(task, payload),
    error = function(e) failed_meta_policy_crossfit_method_task(task, conditionMessage(e))
  )
  out$task_id <- as.character(task_spec$task_id %||% NA_character_)
  out$parent_id <- as.character(task_spec$parent_id %||% NA_character_)
  out$stage <- as.character(task_spec$stage %||% NA_character_)
  out
}

#' Combine an externalized meta-policy cross-fit task plan
#'
#' Reassembles externally scheduled fold or method-fold results into the same
#' cross-fit bundle returned by [crossfit_meta_policy_learner()].
#'
#' @param plan Result from [prepare_meta_policy_crossfit_plan()].
#' @param method_results List of Super Learner method-fold task results.
#' @param fold_results Optional list of complete fold prediction tables for
#'   non-Super Learner methods.
#' @param progress Logical. Emit progress messages.
#'
#' @return Cross-fit result list.
#' @keywords internal
#' @noRd
combine_meta_policy_crossfit_plan <- function(plan,
                                              method_results = list(),
                                              fold_results = NULL,
                                              progress = FALSE) {
  if (isTRUE(plan$is_super)) {
    fold_results <- lapply(seq_along(plan$fold_splits), function(i) {
      combine_meta_policy_super_fold_tasks(
        fold_split = plan$fold_splits[[i]],
        task_results = method_results[vapply(
          method_results,
          function(result) identical(as.integer(result$fold_id), as.integer(i)),
          logical(1)
        )],
        payload = plan$payload
      )
    })
  } else if (is.null(fold_results)) {
    fold_results <- lapply(plan$fold_splits, run_meta_policy_crossfit_payload_fold, payload = plan$payload)
  }

  learner_timings <- dplyr::bind_rows(lapply(
    fold_results,
    function(result) attr(result, "learner_timings") %||% tibble::tibble()
  ))
  fold_timings <- dplyr::bind_rows(lapply(
    fold_results,
    function(result) attr(result, "fold_timing") %||% tibble::tibble()
  ))
  pred_rows <- dplyr::bind_rows(fold_results)

  report_progress(
    progress,
    sprintf(
      "Cross-fit complete: %d prediction rows collected across %d fold task(s).",
      nrow(pred_rows),
      nrow(fold_timings)
    )
  )

  if (!plan$outcome_col %in% names(pred_rows) && ".outcome" %in% names(pred_rows)) {
    pred_rows[[plan$outcome_col]] <- pred_rows$.outcome
  }

  list(
    fold_assignments = plan$fold_assignments,
    predictions = pred_rows,
    learner_timings = learner_timings,
    fold_timings = fold_timings,
    outcome_clip_quantile = plan$outcome_clip_quantile,
    outcome_clip_cap = plan$outcome_clip_cap
  )
}

#' Cross-fit a meta-policy learner on benchmark rows
#'
#' @param policy_perf Policy-performance table.
#' @param group_col Exchangeable grouping column. When `NULL`, the function
#'   infers the first available non-ID anchor-context column.
#' @param n_folds Number of cross-fitting folds.
#' @param method Learner method.
#' @param seed Integer seed.
#' @param feature_cols Optional feature columns.
#' @param outcome_col Outcome column to model.
#' @param outcome_clip_quantile Optional upper quantile used to clip extreme
#'   outcomes before fitting.
#' @param outcome_transform Outcome transform used during training.
#' @param lambda_rule Regularization rule for glm-penalized learners.
#' @param alpha Optional elastic-net mixing parameter.
#' @param inner_folds Number of inner cross-validation folds.
#' @param super_methods Optional super-learner base methods.
#' @param metalearner_loss Loss used to combine super-learner base predictions.
#' @param method_settings Optional shared method-specific tuning settings.
#' @param workers Number of workers used for cross-fitting.
#' @param progress Logical. Emit `tsb_message()` progress lines when `TRUE`.
#'
#' @return A list with cross-fitted predictions and fold assignments.
#'
#' @examples
#' \dontrun{
#' crossfit_obj <- crossfit_meta_policy_learner(
#'   policy_perf = species_block_perf,
#'   method = "glm"
#' )
#' }
#' @keywords internal
#' @noRd
crossfit_meta_policy_learner <- function(policy_perf,
                                         group_col = NULL,
                                         n_folds = 5L,
                                         method = "glm",
                                         seed = NULL,
                                         feature_cols = NULL,
                                         outcome_col = "error_abs_log",
                                         outcome_clip_quantile = NULL,
                                         outcome_transform = "log1p",
                                         lambda_rule = "lambda.1se",
                                         alpha = NULL,
                                         inner_folds = 5L,
                                         super_methods = NULL,
                                         metalearner_loss = "squared_error",
                                         method_settings = NULL,
                                         workers = 1L,
                                         progress = FALSE) {
  policy_perf <- tibble::as_tibble(policy_perf)
  group_col <- resolve_meta_policy_group_col(policy_perf, group_col = group_col)
  if (!is.numeric(workers) || length(workers) != 1 || !is.finite(workers) || workers < 1) {
    stop("'workers' must be one finite number >= 1.", call. = FALSE)
  }
  groups <- sort(unique(stats::na.omit(as.character(policy_perf[[group_col]]))))
  n_folds <- min(as.integer(n_folds), length(groups))
  if (!is.finite(n_folds) || n_folds < 2L) {
    stop("'n_folds' must be at least 2 and no larger than the number of groups.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  fold_tbl <- tibble::tibble(
    group_id = sample(groups, length(groups), replace = FALSE),
    fold_id = rep(seq_len(n_folds), length.out = length(groups))
  )

  is_super <- identical(as.character(method), "super_learner")
  active_methods <- if (is_super) {
    available_meta_policy_super_methods(super_methods, method_settings = method_settings)
  } else {
    method
  }
  lmm_random_intercepts <- active_meta_policy_lmm_random_intercepts(
    active_methods,
    method_settings = method_settings
  )

  data_all <- prepare_meta_policy_data(
    policy_perf = policy_perf,
    outcome_col = outcome_col,
    feature_cols = feature_cols,
    outcome_clip_quantile = outcome_clip_quantile,
    retain_cols = c(group_col, lmm_random_intercepts)
  ) |>
    dplyr::mutate(.split_group = as.character(.data[[group_col]])) |>
    dplyr::left_join(fold_tbl, by = c(".split_group" = "group_id"))

  if (!"fold_id" %in% names(data_all)) {
    if ("fold_id.y" %in% names(data_all)) {
      data_all$fold_id <- data_all$fold_id.y
    } else if ("fold_id.x" %in% names(data_all)) {
      data_all$fold_id <- data_all$fold_id.x
    }
  }
  data_all <- data_all |>
    dplyr::select(-dplyr::any_of(c("fold_id.x", "fold_id.y")))

  n_base_methods <- if (is_super) {
    length(active_methods)
  } else {
    1L
  }

  # Pre-compute per-fold train/test splits. Strip context/anchor columns from the
  # training halves so each serialised fold_split element is as small as possible.
  # Only strip when feature_cols is explicitly provided; when NULL the feature
  # columns are determined dynamically by prepare_meta_policy_data and we must
  # keep all columns so downstream fitting can discover them.
  n_rows_data <- nrow(data_all)
  data_clip_quantile <- attr(data_all, "outcome_clip_quantile")
  data_clip_cap <- attr(data_all, "outcome_clip_cap")
  train_keep <- if (!is.null(feature_cols) && length(feature_cols) > 0L) {
    intersect(
      unique(c(
        feature_cols, ".outcome", ".split_group", "fold_id", lmm_random_intercepts
      )),
      names(data_all)
    )
  } else {
    names(data_all)
  }
  fold_splits <- lapply(seq_len(n_folds), function(f) {
    mask_train <- !is.na(data_all$fold_id) & data_all$fold_id != f
    mask_test <- !is.na(data_all$fold_id) & data_all$fold_id == f
    list(
      train = data_all[mask_train, train_keep, drop = FALSE],
      test = data_all[mask_test, , drop = FALSE],
      fold_id = f
    )
  })
  rm(data_all)

  fold_payload <- list(
    is_super = is_super,
    active_methods = active_methods,
    feature_cols = feature_cols,
    method = method,
    outcome_transform = outcome_transform,
    lambda_rule = lambda_rule,
    alpha = alpha,
    inner_folds = inner_folds,
    super_methods = super_methods,
    metalearner_loss = metalearner_loss,
    method_settings = method_settings,
    seed = seed,
    progress = progress
  )
  run_fold_split <- function(fold_split) {
    run_meta_policy_crossfit_payload_fold(fold_split, fold_payload)
  }
  environment(run_fold_split) <- list2env(
    list(
      fold_payload = fold_payload,
      run_meta_policy_crossfit_payload_fold = run_meta_policy_crossfit_payload_fold
    ),
    parent = baseenv()
  )
  super_tasks <- if (is_super) {
    unlist(
      lapply(seq_along(fold_splits), function(f) {
        lapply(active_methods, function(method_now) {
          list(fold_id = f, method = method_now)
        })
      }),
      recursive = FALSE
    )
  } else {
    list()
  }

  workers <- as.integer(workers)
  n_parallel_units <- if (is_super) length(super_tasks) else n_folds
  n_workers_eff <- min(workers, n_parallel_units)
  report_progress(
    progress,
    sprintf(
      "Starting %d-fold cross-fit: method=%s, %d base learners, %d workers, %d rows%s.",
      n_folds,
      method,
      n_base_methods,
      n_workers_eff,
      n_rows_data,
      if (is_super) sprintf(", %d method-fold task(s)", length(super_tasks)) else ""
    )
  )

  if (workers > 1L && n_parallel_units > 1L) {
    if (is_super) {
      report_progress(
        progress,
        sprintf(
          "Policy learner cross-fit backend: bounded method-fold scheduler, up to %d worker(s); method-local workers=1.",
          n_workers_eff
        )
      )
      super_task_results <- run_meta_policy_crossfit_method_tasks(
        tasks = super_tasks,
        payload = c(fold_payload, list(fold_splits = fold_splits)),
        workers = n_workers_eff,
        progress = progress
      )
      fold_results <- lapply(seq_along(fold_splits), function(i) {
        combine_meta_policy_super_fold_tasks(
          fold_split = fold_splits[[i]],
          task_results = super_task_results[vapply(
            super_task_results,
            function(result) identical(as.integer(result$fold_id), as.integer(i)),
            logical(1)
          )],
          payload = fold_payload
        )
      })
    } else {
      set_meta_policy_crossfit_payload(c(fold_payload, list(fold_splits = fold_splits)))
      on.exit(clear_meta_policy_crossfit_payload(), add = TRUE)
      cluster_obj <- initialize_parallel_cluster(
        workers = n_workers_eff
      )
      cluster_type <- attr(cluster_obj, "cluster_type", exact = TRUE) %||% "unknown"
      report_progress(
        progress,
        "Policy learner cross-fit backend: ",
        parallel_cluster_description(cluster_obj),
        "; parallel unit=outer fold; method-local workers=1."
      )
      on.exit(try(parallel::stopCluster(cluster_obj), silent = TRUE), add = TRUE)
      fold_results <- tryCatch(
        {
          if (identical(cluster_type, "fork")) {
            parallel::parLapplyLB(cluster_obj, seq_along(fold_splits), run_meta_policy_crossfit_task)
          } else {
            tsb_cluster_export(
              cluster_obj,
              # fold_splits is intentionally absent: parLapplyLB distributes each
              # element as the function argument, and run_fold_split's closure
              # environment has already been stripped to scalars only.
              c("run_fold_split"),
              envir = environment()
            )
            parallel::parLapplyLB(cluster_obj, fold_splits, run_fold_split)
          }
        },
        error = function(e) {
          report_progress(
            progress,
            "Policy learner parallel cross-fit failed; falling back to sequential outer folds: ",
            conditionMessage(e)
          )
          NULL
        }
      )
      if (is.null(fold_results)) {
        fold_results <- lapply(seq_along(fold_splits), function(i) {
          report_progress(
            progress,
            sprintf(
              "  Outer fold %d/%d (%d train rows, %d test rows) ...",
              i, n_folds, nrow(fold_splits[[i]]$train), nrow(fold_splits[[i]]$test)
            )
          )
          run_fold_split(fold_splits[[i]])
        })
      }
    }
  } else {
    fold_results <- lapply(seq_along(fold_splits), function(i) {
      report_progress(
        progress,
        sprintf(
          "  Outer fold %d/%d (%d train rows, %d test rows) ...",
          i, n_folds, nrow(fold_splits[[i]]$train), nrow(fold_splits[[i]]$test)
        )
      )
      run_fold_split(fold_splits[[i]])
    })
  }
  learner_timings <- dplyr::bind_rows(lapply(
    fold_results,
    function(result) attr(result, "learner_timings") %||% tibble::tibble()
  ))
  fold_timings <- dplyr::bind_rows(lapply(
    fold_results,
    function(result) attr(result, "fold_timing") %||% tibble::tibble()
  ))
  pred_rows <- dplyr::bind_rows(fold_results)

  report_progress(
    progress,
    sprintf(
      "Cross-fit complete: %d prediction rows collected across %d fold task(s).",
      nrow(pred_rows),
      nrow(fold_timings)
    )
  )

  # Mirror the observed outcome onto the configured outcome name so downstream
  # code can work with one stable column regardless of whether it expects the
  # generic `.outcome` training field or the analysis-facing outcome label.
  if (!outcome_col %in% names(pred_rows) && ".outcome" %in% names(pred_rows)) {
    pred_rows[[outcome_col]] <- pred_rows$.outcome
  }

  list(
    fold_assignments = fold_tbl,
    predictions = pred_rows,
    learner_timings = learner_timings,
    fold_timings = fold_timings,
    outcome_clip_quantile = data_clip_quantile,
    outcome_clip_cap = data_clip_cap
  )
}

#' Summarize cross-fitted meta-policy score predictions
#'
#' @param predictions Output prediction table from `crossfit_meta_policy_learner()`.
#' @param outcome_col Outcome column, usually `.outcome` or `error_abs_log`.
#'
#' @return One-row tibble of calibration and ranking diagnostics.
#'
#' @examples
#' \dontrun{
#' summarize_meta_policy_predictions(crossfit_obj$predictions)
#' }
#' @keywords internal
#' @noRd
summarize_meta_policy_predictions <- function(predictions,
                                              outcome_col = ".outcome") {
  predictions <- tibble::as_tibble(predictions)
  if (!outcome_col %in% names(predictions) && ".outcome" %in% names(predictions)) {
    outcome_col <- ".outcome"
  }
  if (!all(c(outcome_col, ".meta_predicted_score") %in% names(predictions))) {
    return(tibble::tibble())
  }
  ok <- is.finite(predictions[[outcome_col]]) & is.finite(predictions$.meta_predicted_score)
  if (!any(ok)) {
    return(tibble::tibble())
  }
  obs <- predictions[[outcome_col]][ok]
  pred <- predictions$.meta_predicted_score[ok]
  out <- tibble::tibble(
    n = length(obs),
    mae = mean(abs(pred - obs), na.rm = TRUE),
    rmse = sqrt(mean((pred - obs)^2, na.rm = TRUE)),
    bias = mean(pred - obs, na.rm = TRUE),
    spearman = suppressWarnings(stats::cor(pred, obs, method = "spearman", use = "complete.obs")),
    pearson = suppressWarnings(stats::cor(pred, obs, method = "pearson", use = "complete.obs")),
    median_observed = stats::median(obs, na.rm = TRUE),
    median_predicted = stats::median(pred, na.rm = TRUE)
  )
  if (".outcome_raw" %in% names(predictions)) {
    raw_obs <- predictions$.outcome_raw[ok]
    out$raw_mae <- mean(abs(pred - raw_obs), na.rm = TRUE)
    out$raw_rmse <- sqrt(mean((pred - raw_obs)^2, na.rm = TRUE))
    out$raw_bias <- mean(pred - raw_obs, na.rm = TRUE)
    out$raw_spearman <- suppressWarnings(stats::cor(pred, raw_obs, method = "spearman", use = "complete.obs"))
    out$raw_pearson <- suppressWarnings(stats::cor(pred, raw_obs, method = "pearson", use = "complete.obs"))
    out$raw_median_observed <- stats::median(raw_obs, na.rm = TRUE)
  }
  if (".outcome_was_clipped" %in% names(predictions)) {
    out$clipped_fraction <- mean(as.logical(predictions$.outcome_was_clipped[ok]), na.rm = TRUE)
  }
  out
}
