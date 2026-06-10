#' Expand a constructor-based policy registry
#'
#' @param registry Parsed policy-registry list.
#'
#' @return Registry list with a materialized `policies` entry.
#' @keywords internal
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
      phrase <- stringr::str_to_title(stringr::str_replace_all(as.character(group_def$key %||% "policy"), "_", " "))
    }
    phrase
  }
  lower_phrase <- function(group_def) {
    phrase <- as.character(group_def$display_phrase %||% group_def$display_phrase_title %||% NA_character_)[[1]]
    if (is.na(phrase) || !nzchar(phrase)) {
      phrase <- stringr::str_to_lower(stringr::str_replace_all(as.character(group_def$key %||% "policy"), "_", " "))
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
build_constructed_policy_definition <- function(group_def,
                                                metric_def,
                                                constructor_def = list()) {
  title_phrase <- function(group_def) {
    phrase <- as.character(group_def$display_phrase_title %||% group_def$display_phrase %||% NA_character_)[[1]]
    if (is.na(phrase) || !nzchar(phrase)) {
      phrase <- stringr::str_to_title(stringr::str_replace_all(as.character(group_def$key %||% "policy"), "_", " "))
    }
    phrase
  }
  lower_phrase <- function(group_def) {
    phrase <- as.character(group_def$display_phrase %||% group_def$display_phrase_title %||% NA_character_)[[1]]
    if (is.na(phrase) || !nzchar(phrase)) {
      phrase <- stringr::str_to_lower(stringr::str_replace_all(as.character(group_def$key %||% "policy"), "_", " "))
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
policy_group_definition <- function(group_key,
                                    registry) {
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
  conjunction_filter_traits <- c(
    taxonomic_traits,
    "fao_area",
    "ocean_basin",
    "equation_form",
    "derivation_type",
    "length_metric",
    "pressure_corrected",
    "season",
    "diel"
  )
  first_token <- tokens[[1]]
  if (length(tokens) == 1 && first_token %in% special_keys) {
    return(special_group_lookup[[first_token]])
  }

  special_tokens <- tokens[tokens %in% special_keys]
  trait_tokens_all <- tokens[tokens %in% names(public_to_trait)]
  trait_keys_all <- unname(public_to_trait[trait_tokens_all])
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
    trait_keys <- unname(public_to_trait[trait_tokens])
    if (anyDuplicated(trait_keys)) {
      stop(sprintf("Compound policy group '%s' contains duplicate traits.", group_key), call. = FALSE)
    }
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
  root_trait_key <- unname(public_to_trait[root_token])
  filter_tokens <- setdiff(tokens, root_token)
  filter_trait_keys <- unname(public_to_trait[filter_tokens])
  ordered_filter_trait_keys <- c(
    conjunction_filter_traits[conjunction_filter_traits %in% filter_trait_keys],
    sort(setdiff(filter_trait_keys, conjunction_filter_traits))
  )
  trait_keys <- c(root_trait_key, ordered_filter_trait_keys)
  if (anyDuplicated(trait_keys)) {
    stop(sprintf("Compound policy group '%s' contains duplicate traits.", group_key), call. = FALSE)
  }
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
construct_policy_definition_for_group_metric <- function(group_key,
                                                         metric_key,
                                                         registry) {
  metric_lookup <- stats::setNames(
    registry$policy_metrics %||% list(),
    vapply(registry$policy_metrics %||% list(), function(x) as.character(x$key %||% NA_character_), character(1))
  )
  metric_def <- metric_lookup[[metric_key]] %||% NULL
  if (!is.list(metric_def)) {
    stop(sprintf("Unknown policy constructor metric key: %s", metric_key), call. = FALSE)
  }

  group_def <- policy_group_definition(group_key = group_key, registry = registry)
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
construct_policy_definition_from_name <- function(policy_name,
                                                  policy_path = NULL,
                                                  registry = NULL) {
  policy_name <- stringr::str_squish(as.character(policy_name %||% ""))[[1]]
  if (!nzchar(policy_name)) {
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
    construct_policy_definition_for_group_metric(
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
#' @examples
#' \dontrun{
#' registry <- read_policy_registry()
#' registry$policies[[1]]
#' }
#'
#' @export
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

#' List policy names
#'
#' Returns the coded policy names defined in the policy registry.
#'
#' @param policy_path Optional path to a policy registry JSON file.
#'
#' @return Character vector of policy names.
#'
#' @examples
#' policy_names()
#'
#' @export
policy_names <- function(policy_path = NULL) {
  # Pull just the coded names so callers can validate policy selections without
  # reimplementing the registry parsing logic.
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
      construct_policy_definition_from_name(policy_now, policy_path = policy_path) %||%
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

    # Main benchmark policies define the canonical workflow behavior.
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
      if (!is.null(cached)) return(cached)
      policy_def <- lookup[[policy_now]] %||%
        construct_policy_definition_from_name(policy_now, registry = registry) %||%
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
valid_multiplier_rows <- function(rows) {
  # Centralize the multiplier-validity filter so all policy aggregators apply
  # the same numerical screening rules.
  tibble::as_tibble(rows) |>
    dplyr::filter(
      is.finite(biomass_multiplier_if_replace),
      biomass_multiplier_if_replace > 0
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
valid_equation_rows <- function(rows) {
  keep <- is.finite(rows$slope_len) & is.finite(rows$intercept_len)
  rows[keep, , drop = FALSE]
}

#' Identify generalized-model rows
#'
#' @param rows Candidate-policy row table.
#'
#' @return Tibble.
#'
#' @keywords internal
group_model_rows <- function(rows) {
  # Treat either the prepared boolean flag or the legacy method tag as evidence
  # that a row represents a generalized or grouped model.
  out <- tibble::as_tibble(rows)

  # Evaluate the optional grouping flags against the materialized tibble so the
  # filter does not depend on the magrittr pronoun.
  has_group_flag <- "is_group_model" %in% names(out)
  has_method_flag <- "method_type" %in% names(out)

  out |>
    dplyr::filter(
      (if (has_group_flag) is_group_model else FALSE) |
        (if (has_method_flag) method_type == "group" else FALSE)
    )
}

#' Identify canonical 20-log10 slope rows
#'
#' @param rows Candidate-policy row table.
#' @param tolerance Numeric slope tolerance around 20.
#'
#' @return Logical vector.
#' @keywords internal
slope20_indicator <- function(rows,
                              tolerance = 1e-8) {
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

#' Filter donor rows by standardized equation branch
#'
#' @param rows Candidate-policy row table.
#' @param equation_branch_filter One normalized branch-filter label.
#'
#' @return Tibble.
#' @keywords internal
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
    policy_def <- policy_lookup_table()[[policy_now]] %||%
      construct_policy_definition_from_name(policy_now) %||%
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
        display_name <- stringr::str_to_title(stringr::str_replace_all(as.character(policy_def$coded_name %||% policy_now), "_", " "))
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
policy_rows <- function(rows,
                        policy_def,
                        policy_params,
                        ordination_info) {
  # Use the registry candidate-pool definition as the single policy-routing
  # switch so selection logic stays aligned with the registry names.
  pool_name <- as.character(policy_def$candidate_pool)[[1]]
  policy_rows <- tibble::as_tibble(rows)

  if (!"model_id_chr" %in% names(policy_rows) && "model_id" %in% names(policy_rows)) {
    policy_rows$model_id_chr <- as.character(policy_rows$model_id)
  }

  selected_rows <- NULL
  if (identical(pool_name, "all_admissible")) {
    selected_rows <- policy_rows
  }
  if (is.null(selected_rows) && identical(pool_name, "all_valid_models")) {
    selected_rows <- policy_rows
  }
  if (is.null(selected_rows) && identical(pool_name, "same_species")) {
    selected_rows <- dplyr::filter(policy_rows, overlap_same_species)
  }
  if (is.null(selected_rows) && identical(pool_name, "match_all_traits")) {
    selected_rows <- policy_rows
  }
  if (is.null(selected_rows) && identical(pool_name, "nearest_phylogenetic")) {
    for (expr in list(
      quote(overlap_same_species),
      quote(overlap_same_genus & !overlap_same_species),
      quote(overlap_same_family & !overlap_same_genus),
      quote(overlap_same_order & !overlap_same_family)
    )) {
      out <- dplyr::filter(policy_rows, !!expr)
      if (nrow(out) > 0) {
        selected_rows <- out
        break
      }
    }
    if (is.null(selected_rows)) {
      selected_rows <- policy_rows[0, , drop = FALSE]
    }
  }
  if (is.null(selected_rows) && identical(pool_name, "phylogenetic_neighborhood")) {
    out <- dplyr::filter(policy_rows, !overlap_same_species)
    if (!is.null(policy_params$phylo_radius)) {
      out <- dplyr::filter(
        out,
        is.finite(taxonomic_distance_to_anchor),
        taxonomic_distance_to_anchor <= as.numeric(policy_params$phylo_radius)
      )
    }
    selected_rows <- out
  }
  if (is.null(selected_rows) && identical(pool_name, "same_genus")) {
    selected_rows <- dplyr::filter(policy_rows, overlap_same_genus)
  }
  if (is.null(selected_rows) && identical(pool_name, "same_family")) {
    selected_rows <- dplyr::filter(policy_rows, overlap_same_family)
  }
  if (is.null(selected_rows) && identical(pool_name, "same_order")) {
    selected_rows <- dplyr::filter(policy_rows, overlap_same_order)
  }
  if (is.null(selected_rows) && identical(pool_name, "same_nmds_cluster")) {
    score_tbl <- tibble::as_tibble(ordination_info$model_scores %||% tibble::tibble())
    if (!all(c("nmds_cluster", "model_id_chr") %in% names(score_tbl)) ||
      is.na(ordination_info$anchor_cluster)) {
      selected_rows <- policy_rows[0, , drop = FALSE]
    } else {
      cluster_ids <- score_tbl |>
        dplyr::filter(nmds_cluster == ordination_info$anchor_cluster) |>
        dplyr::pull(model_id_chr) |>
        unique()
      selected_rows <- dplyr::filter(policy_rows, model_id_chr %in% cluster_ids)
    }
  }
  if (is.null(selected_rows) && identical(pool_name, "same_species_ellipse")) {
    selected_rows <- dplyr::filter(
      policy_rows,
      model_id_chr %in% ordination_info$species_ellipse_ids,
      !overlap_same_species
    )
  }
  if (is.null(selected_rows) && identical(pool_name, "generalized_models_only")) {
    selected_rows <- group_model_rows(policy_rows)
  }
  if (is.null(selected_rows) && identical(pool_name, "closest_study_cell")) {
    cell_col <- "study_cell_id"
    ranked <- valid_equation_rows(policy_rows) |>
      dplyr::arrange(combined_distance)
    if (nrow(ranked) == 0) {
      selected_rows <- policy_rows[0, , drop = FALSE]
    } else if (!cell_col %in% names(ranked)) {
      selected_rows <- dplyr::slice_head(ranked, n = 1)
    } else {
      cell_id <- ranked[[cell_col]][[1]]
      if (is.na(cell_id) || !nzchar(as.character(cell_id))) {
        selected_rows <- dplyr::slice_head(ranked, n = 1)
      } else {
        selected_rows <- valid_equation_rows(policy_rows) |>
          dplyr::filter(.data[[cell_col]] == cell_id)
      }
    }
  }
  if (is.null(selected_rows) && identical(pool_name, "same_fao_area")) {
    selected_rows <- dplyr::filter(policy_rows, overlap_same_fao_area)
  }
  if (is.null(selected_rows) && identical(pool_name, "same_ocean_basin")) {
    selected_rows <- dplyr::filter(policy_rows, overlap_same_ocean_basin)
  }
  if (is.null(selected_rows) && identical(pool_name, "same_equation_form")) {
    selected_rows <- dplyr::filter(policy_rows, overlap_same_equation_form)
  }
  if (is.null(selected_rows) && identical(pool_name, "same_derivation")) {
    selected_rows <- dplyr::filter(policy_rows, overlap_same_derivation)
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
policy_equation_row <- function(slope,
                                intercept) {
  list(
    policy_slope_len = as.numeric(slope),
    policy_intercept_len = as.numeric(intercept)
  )
}

#' Compute one nearest equation
#'
#' @param rows Candidate-policy row table.
#'
#' @return One-row tibble.
#'
#' @keywords internal
nearest_equation <- function(rows) {
  keep_rows <- valid_equation_rows(rows)
  if (nrow(keep_rows) == 0) return(policy_equation_row(NA_real_, NA_real_))
  if ("taxonomic_distance_to_anchor" %in% names(keep_rows)) {
    i <- which.min(keep_rows$combined_distance + keep_rows$taxonomic_distance_to_anchor * 1e-9)
  } else {
    i <- which.min(keep_rows$combined_distance)
  }
  policy_equation_row(keep_rows$slope_len[[i]], keep_rows$intercept_len[[i]])
}

#' Compute one nearest trait-Gower equation
#'
#' @param rows Candidate-policy row table.
#'
#' @return One-row tibble.
#'
#' @keywords internal
nearest_trait_gower_equation <- function(rows) {
  keep_rows <- valid_equation_rows(rows)
  if (nrow(keep_rows) == 0) return(policy_equation_row(NA_real_, NA_real_))
  if ("trait_gower_distance" %in% names(keep_rows)) {
    ok <- is.finite(keep_rows$trait_gower_distance)
    sub <- if (any(ok)) keep_rows[ok, , drop = FALSE] else keep_rows
    i <- which.min(sub$trait_gower_distance + sub$combined_distance * 1e-9)
  } else {
    sub <- keep_rows
    i <- which.min(sub$combined_distance)
  }
  if (length(i) == 0) return(policy_equation_row(NA_real_, NA_real_))
  policy_equation_row(sub$slope_len[[i]], sub$intercept_len[[i]])
}

#' Compute one nearest taxonomic-distance equation
#'
#' @param rows Candidate-policy row table.
#'
#' @return One-row tibble.
#'
#' @keywords internal
nearest_taxonomic_equation <- function(rows) {
  keep_rows <- valid_equation_rows(rows)
  if (nrow(keep_rows) == 0) return(policy_equation_row(NA_real_, NA_real_))
  if ("taxonomic_distance_to_anchor" %in% names(keep_rows)) {
    ok <- is.finite(keep_rows$taxonomic_distance_to_anchor)
    sub <- if (any(ok)) keep_rows[ok, , drop = FALSE] else keep_rows
    i <- which.min(sub$taxonomic_distance_to_anchor + sub$combined_distance * 1e-9)
  } else {
    sub <- keep_rows
    i <- which.min(sub$combined_distance)
  }
  if (length(i) == 0) return(policy_equation_row(NA_real_, NA_real_))
  policy_equation_row(sub$slope_len[[i]], sub$intercept_len[[i]])
}

#' Compute one nearest species-distance equation
#'
#' @param rows Candidate-policy row table.
#'
#' @return One-row tibble.
#'
#' @keywords internal
nearest_species_distance_equation <- function(rows) {
  keep_rows <- valid_equation_rows(rows)
  if (nrow(keep_rows) == 0) return(policy_equation_row(NA_real_, NA_real_))
  if ("d_species" %in% names(keep_rows)) {
    ok <- is.finite(keep_rows$d_species)
    sub <- if (any(ok)) keep_rows[ok, , drop = FALSE] else keep_rows
    i <- which.min(sub$d_species + sub$combined_distance * 1e-9)
  } else {
    sub <- keep_rows
    i <- which.min(sub$combined_distance)
  }
  if (length(i) == 0) return(policy_equation_row(NA_real_, NA_real_))
  policy_equation_row(sub$slope_len[[i]], sub$intercept_len[[i]])
}

#' Compute one weighted equation
#'
#' @param rows Candidate-policy row table.
#'
#' @return One-row tibble.
#'
#' @keywords internal
weighted_equation <- function(rows) {
  keep_rows <- valid_equation_rows(rows)
  keep_rows <- keep_rows[is.finite(keep_rows$w_adm) & keep_rows$w_adm > 0, , drop = FALSE]
  weights <- normalized_weights(keep_rows$w_adm)

  if (nrow(keep_rows) == 0 || length(weights) == 0) {
    return(policy_equation_row(NA_real_, NA_real_))
  }

  policy_equation_row(
    sum(weights * keep_rows$slope_len),
    sum(weights * keep_rows$intercept_len)
  )
}

study_group_columns <- function(rows) {
  rows <- tibble::as_tibble(rows)
  preferred <- c("study_reference_id", "species_name", "equation_form")
  fallback <- c("study_cell_id", "species_name", "equation_form")
  if (all(preferred %in% names(rows))) {
    return(preferred)
  }
  if (all(fallback %in% names(rows))) {
    return(fallback)
  }
  if ("study_reference_id" %in% names(rows)) {
    return("study_reference_id")
  }
  if ("study_cell_id" %in% names(rows)) {
    return("study_cell_id")
  }
  character(0)
}

study_ensemble_rows <- function(rows,
                                group_weight = c("max", "mean", "sum"),
                                within_weight = c("kernel", "equal")) {
  group_weight <- match.arg(group_weight)
  within_weight <- match.arg(within_weight)
  keep_rows <- valid_equation_rows(rows) |>
    dplyr::filter(is.finite(w_adm), w_adm > 0)
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
      .study_group_raw_weight = dplyr::case_when(
        group_weight == "sum" ~ sum(w_adm, na.rm = TRUE),
        group_weight == "mean" ~ mean(w_adm, na.rm = TRUE),
        TRUE ~ max(w_adm, na.rm = TRUE)
      ),
      .within_raw_weight = if (within_weight == "kernel") w_adm else 1
    ) |>
    dplyr::mutate(
      .within_weight = .within_raw_weight / sum(.within_raw_weight, na.rm = TRUE),
      .study_slope_len = sum(.within_weight * slope_len, na.rm = TRUE),
      .study_intercept_len = sum(.within_weight * intercept_len, na.rm = TRUE),
      .study_within_slope_var = stats::weighted.mean(
        (slope_len - .study_slope_len)^2,
        .within_weight,
        na.rm = TRUE
      ),
      .study_within_intercept_var = stats::weighted.mean(
        (intercept_len - .study_intercept_len)^2,
        .within_weight,
        na.rm = TRUE
      ),
      .study_combined_distance = if (any(is.finite(combined_distance))) {
        sum(.within_weight * combined_distance, na.rm = TRUE)
      } else {
        NA_real_
      },
      .study_trait_gower_distance = if (any(is.finite(trait_gower_distance))) {
        sum(.within_weight * trait_gower_distance, na.rm = TRUE)
      } else {
        NA_real_
      },
      .study_n_models = dplyr::n()
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      slope_len = .study_slope_len,
      intercept_len = .study_intercept_len,
      combined_distance = dplyr::coalesce(.study_combined_distance, combined_distance),
      trait_gower_distance = dplyr::coalesce(.study_trait_gower_distance, trait_gower_distance),
      w_adm = .study_group_raw_weight
    )
}

policy_structural_rows <- function(rows,
                                   policy_def) {
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
    out <- policy_summary_rows(source_rows, policy_def)
    out$.structural_weight <- 1
    return(out)
  }

  if (method_name %in% c("study_kernel_weighted_mean", "study_equal_weight_mean")) {
    source_rows <- source_rows |>
      dplyr::filter(is.finite(w_adm), w_adm > 0)
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
            max(w_adm, na.rm = TRUE)
          ),
          .within_raw_weight = w_adm,
          .within_weight = .within_raw_weight / sum(.within_raw_weight, na.rm = TRUE),
          .structural_weight = .study_group_raw_weight * .within_weight
        ) |>
        dplyr::ungroup()
    )
  }

  if ("w_adm" %in% names(source_rows) && any(is.finite(source_rows$w_adm) & source_rows$w_adm > 0)) {
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

study_weighted_equation <- function(rows) {
  keep_rows <- study_ensemble_rows(rows, group_weight = "max", within_weight = "kernel") |>
    dplyr::filter(is.finite(w_adm), w_adm > 0)
  weights <- normalized_weights(keep_rows$w_adm)
  if (nrow(keep_rows) == 0 || length(weights) == 0) {
    return(policy_equation_row(NA_real_, NA_real_))
  }
  policy_equation_row(
    sum(weights * keep_rows$slope_len),
    sum(weights * keep_rows$intercept_len)
  )
}

study_equal_equation <- function(rows) {
  keep_rows <- study_ensemble_rows(rows, group_weight = "max", within_weight = "kernel")
  if (nrow(keep_rows) == 0) {
    return(policy_equation_row(NA_real_, NA_real_))
  }
  policy_equation_row(
    mean(keep_rows$slope_len, na.rm = TRUE),
    mean(keep_rows$intercept_len, na.rm = TRUE)
  )
}

nearest_study_then_model_equation <- function(rows) {
  keep_rows <- valid_equation_rows(rows)
  group_cols <- study_group_columns(keep_rows)
  if (nrow(keep_rows) == 0 || length(group_cols) == 0) {
    return(nearest_equation(keep_rows))
  }
  study_rank <- keep_rows |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      study_distance = min(combined_distance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(study_distance)
  if (nrow(study_rank) == 0 || !is.finite(study_rank$study_distance[[1]])) {
    return(policy_equation_row(NA_real_, NA_real_))
  }
  selected_study <- study_rank[1, group_cols, drop = FALSE]
  for (nm in group_cols) {
    selected_value <- selected_study[[nm]][[1]]
    keep_rows <- dplyr::filter(
      keep_rows,
      if (is.na(selected_value)) is.na(.data[[nm]]) else .data[[nm]] == selected_value
    )
  }
  nearest_equation(keep_rows)
}

#' Compute one arithmetic-mean equation
#'
#' @param rows Candidate-policy row table.
#'
#' @return One-row tibble.
#'
#' @keywords internal
arithmetic_equation <- function(rows) {
  # Use the straight arithmetic mean of the standardized equation
  # coefficients for unweighted ensemble policies.
  keep_rows <- valid_equation_rows(rows)

  if (nrow(keep_rows) == 0) {
    return(policy_equation_row(NA_real_, NA_real_))
  }

  policy_equation_row(
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
median_equation <- function(rows) {
  # Use the componentwise median standardized coefficients to provide a robust
  # TS-equation ensemble. Biomass multipliers are computed only after this
  # equation has been constructed.
  keep_rows <- valid_equation_rows(rows)

  if (nrow(keep_rows) == 0) {
    return(policy_equation_row(NA_real_, NA_real_))
  }

  policy_equation_row(
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
policy_summary_rows <- function(rows,
                                policy_def) {
  method_name <- as.character(policy_def$aggregation_method)[[1]]
  keep_rows <- valid_equation_rows(rows)

  if (method_name %in% c("nearest_by_combined_distance", "nearest")) {
    if (nrow(keep_rows) == 0) return(keep_rows)
    if ("taxonomic_distance_to_anchor" %in% names(keep_rows)) {
      ord <- order(keep_rows$combined_distance, keep_rows$taxonomic_distance_to_anchor, na.last = TRUE)
    } else {
      ord <- order(keep_rows$combined_distance, na.last = TRUE)
    }
    return(keep_rows[ord[1], , drop = FALSE])
  }

  if (identical(method_name, "nearest_by_trait_gower_distance")) {
    if (nrow(keep_rows) == 0) return(keep_rows)
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
    if (nrow(keep_rows) == 0) return(keep_rows)
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
    if (nrow(keep_rows) == 0) return(keep_rows)
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
      if (nrow(keep_rows) == 0) return(keep_rows)
      ord <- order(keep_rows$combined_distance, na.last = TRUE)
      return(keep_rows[ord[1], , drop = FALSE])
    }
    study_rank <- keep_rows |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(study_distance = min(combined_distance, na.rm = TRUE), .groups = "drop")
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
#'
#' @return A one-row tibble of local support diagnostics.
#' @keywords internal
policy_support_summary <- function(rows,
                                   policy_def) {
  # These diagnostics expose how local the selected donor pool is for the
  # current anchor. They are intentionally separate from historical conformal
  # calibration so final strategy selection can balance both quantities.
  keep_rows <- policy_summary_rows(rows, policy_def)

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
  weighted_mean_col <- function(value_col,
                                weight_col = "w_adm") {
    if (!value_col %in% names(keep_rows)) {
      return(NA_real_)
    }
    x <- suppressWarnings(as.numeric(keep_rows[[value_col]]))
    if (weight_col %in% names(keep_rows)) {
      w <- suppressWarnings(as.numeric(keep_rows[[weight_col]]))
    } else {
      w <- rep(1, length(x))
    }
    ok <- is.finite(x) & is.finite(w) & w > 0
    if (!any(ok)) {
      return(NA_real_)
    }
    stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
  }
  finite_sum_flag <- function(flag_col) {
    if (!flag_col %in% names(keep_rows)) {
      return(NA_integer_)
    }
    as.integer(sum(keep_rows[[flag_col]], na.rm = TRUE))
  }

  weights <- if ("w_adm" %in% names(keep_rows)) {
    normalized_weights(keep_rows$w_adm)
  } else {
    numeric(0)
  }
  effective_n <- if (length(weights) == 0) {
    NA_real_
  } else {
    1 / sum(weights^2)
  }

  slope20 <- slope20_indicator(keep_rows)
  base_summary <- list(
    n_valid_models = as.integer(nrow(keep_rows)),
    local_min_combined_distance = if ("combined_distance" %in% names(keep_rows)) finite_min(keep_rows$combined_distance) else NA_real_,
    local_median_combined_distance = if ("combined_distance" %in% names(keep_rows)) finite_median(keep_rows$combined_distance) else NA_real_,
    local_weighted_mean_combined_distance = weighted_mean_col("combined_distance"),
    local_min_trait_gower_distance = if ("trait_gower_distance" %in% names(keep_rows)) finite_min(keep_rows$trait_gower_distance) else NA_real_,
    local_weighted_mean_trait_gower_distance = weighted_mean_col("trait_gower_distance"),
    local_min_species_distance = if ("d_species" %in% names(keep_rows)) finite_min(keep_rows$d_species) else NA_real_,
    local_weighted_mean_species_distance = weighted_mean_col("d_species"),
    local_mean_length_overlap = weighted_mean_col("length_overlap_fraction"),
    local_mean_depth_overlap = weighted_mean_col("depth_overlap_fraction"),
    local_effective_support = effective_n,
    local_max_weight = if (length(weights) == 0) NA_real_ else max(weights, na.rm = TRUE)
  )
  overlap_cols <- grep("^overlap_", names(keep_rows), value = TRUE)
  if (length(overlap_cols) > 0) {
    overlap_counts <- lapply(overlap_cols, function(col) {
      as.integer(sum(keep_rows[[col]], na.rm = TRUE))
    })
    names(overlap_counts) <- paste0("local_n_", sub("^overlap_", "", overlap_cols))
    base_summary <- c(base_summary, overlap_counts)
  }
  if ("slope_len" %in% names(keep_rows)) {
    base_summary$local_n_slope20     <- as.integer(sum(slope20, na.rm = TRUE))
    base_summary$local_n_non_slope20 <- as.integer(sum(!slope20, na.rm = TRUE))
  }
  base_summary
}

#' Summarize structural spread in one policy equation
#'
#' @param rows Candidate-policy row table.
#' @param policy_def One policy-definition list.
#' @param pred One-row policy prediction from [policy_prediction()].
#' @param anchor_pdf Anchor length-density table.
#'
#' @return A one-row tibble of equation-dispersion diagnostics.
#' @keywords internal
policy_structural_summary <- function(rows,
                                      policy_def,
                                      pred,
                                      anchor_pdf) {
  # These diagnostics quantify disagreement among the donor equations that
  # actually enter a policy. They are intentionally computed after constructing
  # the policy equation so ensemble policies carry local structural uncertainty
  # rather than appearing precise only because coefficients were averaged.
  keep_rows <- policy_structural_rows(rows, policy_def)
  method_name <- as.character(policy_def$aggregation_method)[[1]]
  n_valid <- nrow(keep_rows)

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
  if (n_valid == 0 ||
    !is.finite(pred$policy_slope_len[[1]]) ||
    !is.finite(pred$policy_intercept_len[[1]])) {
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
  policy_sigma <- pred$policy_sigma_bs_mean[[1]]
  log_sigma_abs_dev <- if (is.finite(policy_sigma) && policy_sigma > 0) {
    abs(log(donor_sigma) - log(policy_sigma))
  } else {
    rep(NA_real_, n_valid)
  }
  donor_multiplier <- suppressWarnings(as.numeric(keep_rows$biomass_multiplier_if_replace %||% rep(NA_real_, n_valid)))
  policy_multiplier <- suppressWarnings(as.numeric(pred$multiplier_pred[[1]]))
  log_multiplier_abs_dev <- if (is.finite(policy_multiplier) && policy_multiplier > 0) {
    abs(log(donor_multiplier) - log(policy_multiplier))
  } else {
    rep(NA_real_, n_valid)
  }

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
      if (!any(ok) || sum(pdf_w[ok]) <= 0) return(NA_real_)
      donor_ts <- s * log10(len[ok]) + b
      policy_ts <- as.numeric(pred$policy_slope_len[[1]]) * log10(len[ok]) +
                   as.numeric(pred$policy_intercept_len[[1]])
      w <- pdf_w[ok] / sum(pdf_w[ok])
      sqrt(sum(w * (donor_ts - policy_ts)^2))
    },
    numeric(1)
  )

  log_multiplier_q <- weighted_quantile(log_multiplier_abs_dev, weights, probs = c(0.50, 0.90))
  log_sigma_q <- weighted_quantile(log_sigma_abs_dev, weights, probs = c(0.50, 0.90))
  curve_q <- weighted_quantile(curve_rmse, weights, probs = c(0.50, 0.90))
  structural_q <- if (is.finite(log_multiplier_q[[2]]) || is.finite(log_sigma_q[[2]])) {
    sqrt(dplyr::coalesce(log_multiplier_q[[2]], 0)^2 + dplyr::coalesce(log_sigma_q[[2]], 0)^2)
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
equal_equation <- function(rows) {
  # Equal-weight mean is distinct in the registry even when its equation
  # summary matches the arithmetic mean.
  arithmetic_equation(rows)
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
equation_sigma_mean <- function(slope,
                                intercept,
                                anchor_pdf,
                                anchor_pdf_precomp = NULL) {
  if (!is.finite(slope) || !is.finite(intercept)) return(NA_real_)
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
policy_equation <- function(rows,
                            policy_def) {
  # Dispatch to the requested aggregation method after the donor pool has
  # already been filtered for the selected policy. This function returns a
  # standardized TS-length equation, not a biomass multiplier.
  method_name <- as.character(policy_def$aggregation_method)[[1]]

  if (identical(method_name, "nearest_by_combined_distance")) {
    return(nearest_equation(rows))
  }
  if (identical(method_name, "nearest")) {
    return(nearest_equation(rows))
  }
  if (identical(method_name, "nearest_by_trait_gower_distance")) {
    return(nearest_trait_gower_equation(rows))
  }
  if (identical(method_name, "nearest_by_taxonomic_distance")) {
    return(nearest_taxonomic_equation(rows))
  }
  if (identical(method_name, "nearest_by_species_distance")) {
    return(nearest_species_distance_equation(rows))
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

  stop(sprintf("Unsupported aggregation method: %s", method_name), call. = FALSE)
}

#' Compute one policy prediction
#'
#' @param rows Candidate-policy row table.
#' @param policy_def One policy-definition list.
#' @param anchor_sigma Anchor mean backscatter.
#' @param anchor_pdf Anchor length-density table.
#'
#' @return One-row tibble.
#'
#' @keywords internal
policy_prediction <- function(rows,
                              policy_def,
                              anchor_sigma,
                              anchor_pdf,
                              anchor_pdf_precomp = NULL) {
  eq <- policy_equation(rows, policy_def)
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
policy_curve <- function(rows,
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
#'
#' @return A tibble with `policy`, `multiplier_pred`, and `n_models`.
#'
#' @export
evaluate_policies <- function(eval_obj,
                              ordination_info = NULL,
                              policies = NULL,
                              policy_params = list(),
                              policy_path = NULL) {
  # Validate the evaluation object and read the registry once before iterating
  # over the selected policy set.
  if (!is.list(eval_obj) || is.null(eval_obj$admissible_df) || !is.data.frame(eval_obj$admissible_df)) {
    stop("'eval_obj' must contain an 'admissible_df' data frame.", call. = FALSE)
  }

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
        construct_policy_definition_from_name(policy_name, registry = shared_registry) %||%
        NULL
    }),
    selected
  )
  unknown <- names(policy_defs)[!vapply(policy_defs, is.list, logical(1))]
  if (length(unknown) > 0) {
    stop(sprintf("Unknown policy name(s): %s", paste(unknown, collapse = ", ")), call. = FALSE)
  }

  context <- resolve_policy_context(ordination_info = ordination_info)
  admissible_rows <- tibble::as_tibble(eval_obj$admissible_df)
  default_branch_filters <- normalize_policy_equation_branch_filters(
    policy_params$equation_branch_filters %||% NULL
  )
  per_policy_branch_filters <- lapply(selected, function(policy_name) {
    policy_def <- policy_defs[[policy_name]]
    params <- policy_parameters(policy_name, policy_def, policy_params = policy_params)
    normalize_policy_equation_branch_filters(
      params$equation_branch_filters %||%
        params$branch_filters %||%
        default_branch_filters
    )
  })
  names(per_policy_branch_filters) <- selected
  equation_branch_filters <- unique(unlist(per_policy_branch_filters, use.names = FALSE))
  branch_cache <- stats::setNames(
    lapply(equation_branch_filters, function(branch_filter) {
      policy_branch_filter(admissible_rows, branch_filter)
    }),
    equation_branch_filters
  )
  donor_cache <- new.env(parent = emptyenv())

  branch_display_tags <- local({
    branch_defs <- shared_registry$policy_branches %||% list()
    tags <- stats::setNames(
      vapply(branch_defs, function(x) as.character(x$display_tag %||% x$key %||% NA_character_), character(1)),
      vapply(branch_defs, function(x) as.character(x$key %||% NA_character_), character(1))
    )
    tags
  })

  anchor_pdf_precomp <- local({
    pdf <- eval_obj$anchor_pdf
    if (!is.null(pdf) && all(c("length_cm", "f_len") %in% names(pdf))) {
      len <- as.numeric(pdf$length_cm)
      f   <- as.numeric(pdf$f_len)
      ok  <- is.finite(len) & len > 0 & is.finite(f) & f >= 0
      if (any(ok) && sum(f[ok]) > 0) {
        list(log10_len = log10(len[ok]), f_norm = f[ok] / sum(f[ok]))
      } else NULL
    } else NULL
  })

  # Evaluate each selected policy independently so the returned table is ready
  # for the benchmark layer without any additional reshaping.
  rows_list <- vector("list", length(selected))
  for (pi in seq_along(selected)) {
    policy_name <- selected[[pi]]
    policy_def <- policy_defs[[policy_name]]
    canonical_name <- as.character(policy_def$coded_name %||% policy_name)[[1]]
    params <- policy_parameters(policy_name, policy_def, policy_params = policy_params)
    branch_filters_now <- per_policy_branch_filters[[policy_name]] %||% default_branch_filters

    branch_list <- vector("list", length(branch_filters_now))
    for (bi in seq_along(branch_filters_now)) {
      branch_filter <- branch_filters_now[[bi]]
      cache_key <- paste(
        as.character(policy_def$candidate_pool)[[1]],
        branch_filter,
        as.character(params$phylo_radius %||% NA_real_),
        paste(as.character(unlist(params$match_traits %||% character(0), use.names = FALSE)), collapse = ","),
        sep = "|"
      )

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

      pred <- policy_prediction(
        donor_rows,
        policy_def,
        anchor_sigma = eval_obj$anchor_sigma,
        anchor_pdf = eval_obj$anchor_pdf,
        anchor_pdf_precomp = anchor_pdf_precomp
      )

      branch_display <- unname(branch_display_tags[branch_filter] %||% branch_filter)
      if (is.na(branch_display) || !nzchar(branch_display)) branch_display <- branch_filter
      display_name <- as.character(policy_def$display_name %||% NA_character_)[[1]]
      if (is.na(display_name) || !nzchar(display_name)) {
        display_name <- stringr::str_to_title(stringr::str_replace_all(canonical_name, "_", " "))
      }
      policy_display <- paste0(display_name, " [", branch_display, "]")

      branch_list[[bi]] <- c(
        list(
          policy = canonical_name,
          policy_base = canonical_name,
          policy_display = policy_display,
          equation_branch_filter = branch_filter,
          n_models = as.integer(nrow(donor_rows)),
          policy_family = as.character(policy_def$policy_family %||% NA_character_)[[1]],
          candidate_pool = as.character(policy_def$candidate_pool)[[1]],
          aggregation_method = as.character(policy_def$aggregation_method)[[1]],
          candidate_pool_definition = as.character(policy_def$candidate_pool_definition %||% NA_character_)[[1]],
          aggregation_definition = as.character(policy_def$aggregation_definition %||% NA_character_)[[1]],
          plain_language_definition = as.character(policy_def$plain_language_definition %||% NA_character_)[[1]]
        ),
        pred,
        policy_support_summary(donor_rows, policy_def)
      )
    }
    rows_list[[pi]] <- dplyr::bind_rows(branch_list)
  }
  dplyr::bind_rows(rows_list)
}

#' Predict one policy TS curve
#'
#' Predicts one length-specific TS curve for a selected policy using the same
#' donor pool and aggregation rule as [evaluate_policies()].
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
#' @export
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
    construct_policy_definition_from_name(policy, policy_path = policy_path) %||%
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

  policy_curve(donor_rows, policy_def, lengths_cm)
}


