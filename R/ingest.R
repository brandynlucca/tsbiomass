#' Fetch WoRMS metadata for species names
#'
#' Queries WoRMS with [`worrms::wm_records_name()`] and returns the raw WoRMS
#' metadata in a standardized tibble format. The output is restricted to the
#' species-level trait names that are present in the package trait registry and
#' can be supplied directly by WoRMS. The function does not rank, score, or
#' select among multiple returned records. If WoRMS returns multiple matches for
#' one queried species, all of them are retained.
#'
#' @param species Character vector of scientific species names to query.
#' @param cache_path Optional path to an `.rds` cache file. When supplied, the
#'   function reads from that cache when it already exists and `refresh` is
#'   `FALSE`.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache file and
#'   fetch fresh WoRMS results.
#'
#' @return A tibble containing only registry-aligned species trait columns that
#'   are defined in the trait registry. Direct name overlaps from WoRMS are
#'   filled automatically, and `genus` / `species` are derived from the queried
#'   or returned scientific name when needed.
#'
#' @examples
#' \dontrun{
#' fetch_worms(c("Clupea pallasii", "Sardinops sagax"))
#' }
#'
#' @export
fetch_worms <- function(species, cache_path = NULL, refresh = FALSE) {
  # Validate the optional cache control arguments before any filesystem or API
  # work begins.
  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1)) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }

  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Standardize the incoming species vector so blank and duplicate queries do
  # not trigger redundant API calls.
  species <- as.character(species)
  species <- stringr::str_squish(species)
  species <- unique(species[!is.na(species) & nzchar(species)])

  if (length(species) == 0) {
    stop("No valid species names were supplied.", call. = FALSE)
  }

  # Read the registry directly and use it to decide which species-level columns
  # this ingestor is allowed to return.
  registry <- read_trait_registry()
  species_traits <- registry$species_traits
  output_cols <- vapply(
    species_traits,
    function(x) x$coded_name,
    character(1)
  )
  trait_types <- stats::setNames(
    vapply(species_traits, function(x) x$data_type, character(1)),
    output_cols
  )

  # Build a typed missing-value template from the registry so every output row
  # has the full JSON-defined schema even when WoRMS does not supply a value.
  template <- as.list(rep(NA_character_, length(output_cols)))
  names(template) <- output_cols
  for (i in seq_along(species_traits)) {
    trait_name <- species_traits[[i]]$coded_name
    trait_type <- species_traits[[i]]$data_type
    if (trait_type == "numeric") {
      template[[trait_name]] <- NA_real_
    } else if (trait_type == "binary") {
      template[[trait_name]] <- NA
    }
  }

  # Load source-column aliases from a template file so WoRMS translation rules
  # live outside the function body and can be extended without code edits.
  alias_path <- system.file(
    "templates",
    "source_aliases.json",
    package = "tsbiomass"
  )
  source_aliases <- character(0)
  if (nzchar(alias_path) && file.exists(alias_path)) {
    alias_cfg <- read_json_file(alias_path)
    if (is.list(alias_cfg) &&
        "worrms" %in% names(alias_cfg) &&
        is.list(alias_cfg$worrms)) {
      source_aliases <- unlist(alias_cfg$worrms, use.names = TRUE)
    }
  }

  # Reuse an existing cache only when the caller explicitly supplied one and
  # requested non-refresh behavior.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  out <- lapply(species, function(sp) {
    # Fetch all candidate WoRMS records for the queried species. API errors are
    # converted into a single missing row so batch ingestion can continue.
    rec <- tryCatch(
      worrms::wm_records_name(sp),
      error = function(err) NULL
    )

    # Parse the queried scientific name once so it can fill genus/species when
    # WoRMS returns no rows or omits a scientific name.
    query_genus <- stringr::word(sp, start = 1, end = 1)
    query_species <- stringr::word(sp, start = 2, end = 2)
    query_genus[!nzchar(query_genus)] <- NA_character_
    query_species[!nzchar(query_species)] <- NA_character_

    if (is.null(rec) || nrow(rec) == 0) {
      out <- tibble::as_tibble(template)
      if ("genus" %in% names(out)) {
        out$genus <- query_genus
      }
      if ("species" %in% names(out)) {
        out$species <- query_species
      }
      return(out)
    }

    # Normalize the WoRMS response names once so downstream code can rely on a
    # stable snake_case schema.
    rec <- tibble::as_tibble(rec)
    names(rec) <- janitor::make_clean_names(names(rec))

    # Start from the full registry-defined output schema, then inspect every
    # WoRMS source column through direct-name and alias translation.
    out <- tibble::as_tibble(
      lapply(template, function(x) rep(x, nrow(rec)))
    )

    scientific_name <- rep(sp, nrow(rec))
    source_cols <- names(rec)

    if ("scientificname" %in% names(rec)) {
      scientific_name <- as.character(rec$scientificname)
      scientific_name[is.na(scientific_name) | !nzchar(scientific_name)] <- sp
    } else if ("valid_name" %in% names(rec)) {
      scientific_name <- as.character(rec$valid_name)
      scientific_name[is.na(scientific_name) | !nzchar(scientific_name)] <- sp
    }

    for (source_col in source_cols) {
      target_candidates <- unique(c(source_col, unname(source_aliases[source_col])))
      target_candidates <- target_candidates[!is.na(target_candidates) & nzchar(target_candidates)]

      target_col <- intersect(target_candidates, names(out))
      if (length(target_col) == 0) {
        next
      }
      target_col <- target_col[[1]]

      if (trait_types[[target_col]] == "numeric") {
        out[[target_col]] <- suppressWarnings(as.numeric(rec[[source_col]]))
      } else if (trait_types[[target_col]] == "binary") {
        out[[target_col]] <- as.logical(rec[[source_col]])
      } else {
        out[[target_col]] <- as.character(rec[[source_col]])
      }
    }

    # The registry uses explicit genus/species fields. Derive those two fields
    # from the returned scientific name when needed.
    if ("genus" %in% names(out)) {
      genus <- stringr::word(scientific_name, start = 1, end = 1)
      genus[!nzchar(genus)] <- query_genus
      out$genus <- genus
    }

    if ("species" %in% names(out)) {
      species_epithet <- stringr::word(scientific_name, start = 2, end = 2)
      species_epithet[!nzchar(species_epithet)] <- query_species
      out$species <- species_epithet
    }

    out
  })

  out <- dplyr::bind_rows(out)

  # Persist the fetched result exactly as returned so repeated calls can avoid
  # extra API traffic.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cache_path)
  }

  out
}

#' Read and clean the master TS-length workbook
#'
#' Reads the TS-length workbook, applies the same core cleaning steps used in
#' the original workflow, and parses the study-reported fitting length range
#' into numeric minimum, maximum, and range columns.
#'
#' @param path Path to the TS-length workbook.
#' @param sheet Optional sheet specification passed to
#'   [`readxl::read_excel()`]. When `NULL`, the first sheet is used.
#'
#' @return A cleaned tibble.
#'
#' @examples
#' \dontrun{
#' read_tsl_table("fishery_survey_tsl.xlsx")
#' }
#'
#' @export
read_tsl_table <- function(path, sheet = NULL) {
  # Validate the workbook path and optional sheet selector before reading.
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("'path' must be a single workbook path.", call. = FALSE)
  }

  if (!file.exists(path)) {
    stop("'path' does not exist.", call. = FALSE)
  }

  if (!is.null(sheet) &&
      (!(is.character(sheet) || is.numeric(sheet)) || length(sheet) != 1)) {
    stop("'sheet' must be NULL or a single sheet name/index.", call. = FALSE)
  }

  # Read the workbook and normalize column names once so downstream code can
  # rely on a stable snake_case schema.
  if (is.null(sheet)) {
    dat <- readxl::read_excel(path, .name_repair = "minimal")
  } else {
    dat <- readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")
  }

  dat <- dat |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(where(is.character), ~ stringr::str_squish(.x))
    )

  # Require the core TS-model columns up front so downstream preparation does
  # not silently proceed with an unusable study table.
  required_cols <- c("genus", "species", "equation_form", "slope", "intercept")
  missing_cols <- setdiff(required_cols, names(dat))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "TSL table is missing required column(s): %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Rebuild the species name internally for parsing and matching, even though
  # the returned study table keeps only the canonical analysis columns.
  species_name <- stringr::str_trim(paste(dat$genus, dat$species))

  if ("frequency_khz" %in% names(dat)) {
    dat$frequency_khz <- suppressWarnings(as.numeric(dat$frequency_khz))
    dat$frequency_khz[!is.finite(dat$frequency_khz)] <- NA_real_
    if ("frequency" %in% names(dat)) {
      dat$frequency <- suppressWarnings(as.numeric(dat$frequency))
      dat$frequency[!is.finite(dat$frequency)] <- NA_real_
      dat$frequency <- dplyr::coalesce(dat$frequency, dat$frequency_khz)
    } else {
      dat$frequency <- dat$frequency_khz
    }
  }

  dat$slope <- suppressWarnings(as.numeric(dat$slope))
  dat$slope[!is.finite(dat$slope)] <- NA_real_

  dat$intercept <- suppressWarnings(as.numeric(dat$intercept))
  dat$intercept[!is.finite(dat$intercept)] <- NA_real_

  if ("pressure_corrected" %in% names(dat)) {
    pressure_chr <- as.character(dat$pressure_corrected)
    dat$pressure_corrected <- dplyr::case_when(
      pressure_chr %in% c("1", "1.0", "TRUE", "true") ~ TRUE,
      pressure_chr %in% c("0", "0.0", "FALSE", "false") ~ FALSE,
      TRUE ~ NA
    )
  }

  if ("active_use" %in% names(dat)) {
    active_chr <- as.character(dat$active_use)
    dat$active_use <- dplyr::case_when(
      active_chr %in% c("1", "1.0", "TRUE", "true") ~ TRUE,
      active_chr %in% c("0", "0.0", "FALSE", "false") ~ FALSE,
      TRUE ~ FALSE
    )
  }

  if ("stock_assessment" %in% names(dat)) {
    stock_chr <- as.character(dat$stock_assessment)
    dat$stock_assessment <- dplyr::case_when(
      stock_chr %in% c("1", "1.0", "TRUE", "true") ~ TRUE,
      stock_chr %in% c("0", "0.0", "FALSE", "false") ~ FALSE,
      TRUE ~ FALSE
    )
  }

  if ("equation_form" %in% names(dat)) {
    dat$equation_form <- stringr::str_to_lower(dat$equation_form)
  }

  if ("derivation" %in% names(dat) && !"derivation_type" %in% names(dat)) {
    dat$derivation_type <- dat$derivation
  }

  # Parse the study fitting length range when that source column is present.
  # These are the study-reported fitting lengths, not species-level body-length
  # traits from external databases.
  if ("length_range_cm" %in% names(dat)) {
    range_chr <- dplyr::coalesce(as.character(dat$length_range_cm), "")
    range_chr <- stringr::str_squish(range_chr)
    split_mat <- stringr::str_split(
      stringr::str_replace_all(range_chr, "\\s", ""),
      ",",
      simplify = TRUE
    )

    if (ncol(split_mat) >= 2) {
      min_len <- suppressWarnings(as.numeric(split_mat[, 1]))
      max_len <- suppressWarnings(as.numeric(split_mat[, 2]))
    } else {
      min_len <- rep(NA_real_, nrow(dat))
      max_len <- rep(NA_real_, nrow(dat))
    }

    # Guard against spreadsheet artifacts where the length range was imported
    # as an Excel serial date or as another clearly non-physical large value.
    excel_serial_like <- stringr::str_detect(range_chr, "^[0-9]{5}(?:\\.0+)?$")
    nonphysical_len <- (is.finite(min_len) & min_len > 1000) |
      (is.finite(max_len) & max_len > 1000)
    bad_len_rows <- excel_serial_like | nonphysical_len
    min_len[bad_len_rows] <- NA_real_
    max_len[bad_len_rows] <- NA_real_

    dat$length_min <- pmin(min_len, max_len, na.rm = TRUE)
    dat$length_max <- pmax(min_len, max_len, na.rm = TRUE)
    dat$length_range <- dat$length_max - dat$length_min

    dat$length_min[!is.finite(dat$length_min)] <- NA_real_
    dat$length_max[!is.finite(dat$length_max)] <- NA_real_
    dat$length_range[!is.finite(dat$length_range)] <- NA_real_
    dat$length_midpoint <- ifelse(
      is.finite(dat$length_min) & is.finite(dat$length_max),
      (dat$length_min + dat$length_max) / 2,
      NA_real_
    )
  }

  # Retain only the canonical study-analysis columns needed downstream.
  keep_cols <- c(
    "fao_area",
    "regional_body",
    "genus",
    "species",
    "tags",
    "swimbladder_type",
    "source_type",
    "equation_form",
    "slope",
    "intercept",
    "length_metric",
    "derivation_type",
    "sample_size",
    "frequency",
    "pressure_corrected",
    "length_min",
    "length_max",
    "length_midpoint",
    "depth_min",
    "depth_max",
    "depth_midpoint",
    "reference_tsl_short"
  )

  # Add any missing canonical columns explicitly as NA so downstream code can
  # rely on a stable study-table schema without carrying the raw workbook
  # aliases around.
  for (nm in setdiff(keep_cols, names(dat))) {
    dat[[nm]] <- NA
  }

  # Coerce the canonical analysis columns to stable types before returning the
  # pared-down study table.
  for (nm in c(
    "slope", "intercept", "sample_size", "frequency",
    "length_min", "length_max", "length_midpoint",
    "depth_min", "depth_max", "depth_midpoint"
  )) {
    dat[[nm]] <- suppressWarnings(as.numeric(dat[[nm]]))
    dat[[nm]][!is.finite(dat[[nm]])] <- NA_real_
  }

  dat <- dat[, keep_cols, drop = FALSE]

  dat
}

#' Fetch FishBase metadata for species names
#'
#' Queries a small set of FishBase endpoints and returns registry-aligned
#' species metadata. The output schema is driven entirely by the species traits
#' defined in the package trait registry. Every fetched FishBase source column
#' is inspected. Direct name overlaps are used immediately, and non-matching
#' source names are routed through an internal alias translation step before
#' being compared against the registry-coded names.
#'
#' @param species Character vector of scientific species names to query.
#' @param cache_path Optional path to an `.rds` cache file. When supplied, the
#'   function reads from that cache when it already exists and `refresh` is
#'   `FALSE`.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache file and
#'   fetch fresh FishBase results.
#'
#' @return A tibble containing only registry-aligned species trait columns that
#'   are defined in the trait registry.
#'
#' @examples
#' \dontrun{
#' fetch_fishbase(c("Clupea pallasii", "Sardinops sagax"))
#' }
#'
#' @export
fetch_fishbase <- function(species, cache_path = NULL, refresh = FALSE) {
  # Validate the optional cache control arguments before any filesystem or API
  # work begins.
  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1)) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }

  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Standardize the incoming species vector so blank and duplicate queries do
  # not trigger redundant API calls.
  species <- as.character(species)
  species <- stringr::str_squish(species)
  species <- unique(species[!is.na(species) & nzchar(species)])

  if (length(species) == 0) {
    stop("No valid species names were supplied.", call. = FALSE)
  }

  # Read the registry directly and use it to decide which species-level columns
  # this ingestor is allowed to return and how their missing values should be
  # typed.
  registry <- read_trait_registry()
  species_traits <- registry$species_traits
  registry_cols <- vapply(
    species_traits,
    function(x) x$coded_name,
    character(1)
  )
  trait_types <- stats::setNames(
    vapply(species_traits, function(x) x$data_type, character(1)),
    registry_cols
  )
  trait_multi <- stats::setNames(
    vapply(species_traits, function(x) isTRUE(x$multi_valued), logical(1)),
    registry_cols
  )

  # Load source-column aliases from a template file so translation rules live
  # outside the function body and can be extended without code edits.
  alias_path <- system.file(
    "templates",
    "source_aliases.json",
    package = "tsbiomass"
  )
  source_aliases <- character(0)
  if (nzchar(alias_path) && file.exists(alias_path)) {
    alias_cfg <- read_json_file(alias_path)
    if (is.list(alias_cfg) &&
        "fishbase" %in% names(alias_cfg) &&
        is.list(alias_cfg$fishbase)) {
      source_aliases <- unlist(alias_cfg$fishbase, use.names = TRUE)
    }
  }

  # Allow the alias template to define additional support fields beyond the
  # trait registry so source-specific parameters like FishBase length-weight
  # coefficients can survive ingestion for later internal transforms.
  support_cols <- unique(unname(source_aliases))
  support_cols <- support_cols[!is.na(support_cols) & nzchar(support_cols)]
  support_cols <- setdiff(support_cols, registry_cols)
  output_cols <- c(registry_cols, support_cols)

  # Build the output template from the registry plus any alias-defined support
  # fields. Registry traits keep their declared types; support fields default
  # to character until the source values are inspected.
  template <- as.list(rep(NA_character_, length(output_cols)))
  names(template) <- output_cols
  for (nm in registry_cols) {
    if (trait_types[[nm]] == "numeric") {
      template[[nm]] <- NA_real_
    } else if (trait_types[[nm]] == "binary") {
      template[[nm]] <- NA
    }
  }

  # Reuse an existing cache only when the caller explicitly supplied one and
  # requested non-refresh behavior.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Seed one output row per queried species so direct genus/species identity is
  # preserved even when FishBase returns no usable endpoint match.
  out <- tibble::as_tibble(
    lapply(template, function(x) rep(x, length(species)))
  )
  out$species_name_query <- species
  if ("genus" %in% names(out)) {
    out$genus <- stringr::word(species, start = 1, end = 1)
    out$genus[!nzchar(out$genus)] <- NA_character_
  }
  if ("species" %in% names(out)) {
    out$species <- stringr::word(species, start = 2, end = 2)
    out$species[!nzchar(out$species)] <- NA_character_
  }

  # Fetch the FishBase endpoint tables needed for the current translation map.
  fetch_quietly <- function(expr) {
    suppressWarnings(
      suppressMessages(
        tryCatch(expr, error = function(err) NULL)
      )
    )
  }
  endpoints <- list(
    species       = fetch_quietly(rfishbase::species(species_list = species)),
    morphology    = fetch_quietly(rfishbase::morphology(species_list = species)),
    ecology       = fetch_quietly(rfishbase::ecology(species_list = species)),
    length_weight = fetch_quietly(rfishbase::length_weight(species_list = species)),
    popgrowth     = fetch_quietly(rfishbase::popgrowth(species_list = species)),
    stocks        = fetch_quietly(rfishbase::stocks(species_list = species)),
    faoareas      = fetch_quietly(rfishbase::faoareas(species_list = species))
  )

  for (endpoint_name in names(endpoints)) {
    dat <- endpoints[[endpoint_name]]
    if (is.null(dat) || nrow(dat) == 0) {
      next
    }

    # Normalize source names once so all translation aliases can be expressed in
    # one cleaned naming system.
    dat <- tibble::as_tibble(dat)
    names(dat) <- janitor::make_clean_names(names(dat))

    # Build a join key from the full species name when available. This stays
    # internal and is dropped from the returned output.
    if ("species" %in% names(dat)) {
      dat$species_name_query <- stringr::str_squish(as.character(dat$species))
    } else if (all(c("genus", "species") %in% names(dat))) {
      dat$species_name_query <- stringr::str_squish(
        paste(dat$genus, dat$species)
      )
    } else {
      next
    }

    source_cols <- setdiff(names(dat), "species_name_query")

    for (source_col in source_cols) {
      # First try the cleaned source column name itself, then try any known
      # FishBase alias for that column name.
      target_candidates <- unique(c(source_col, unname(source_aliases[source_col])))
      target_candidates <- target_candidates[!is.na(target_candidates) & nzchar(target_candidates)]
      target_col <- intersect(target_candidates, names(out))

      if (length(target_col) == 0) {
        next
      }
      target_col <- target_col[[1]]

      value_df <- dat[, c("species_name_query", source_col), drop = FALSE]
      names(value_df)[2] <- "value_raw"

      # Support fields defined only through the alias template, such as
      # FishBase length-weight coefficients, are not part of the registry.
      # Look those up safely so they can still flow through ingestion without
      # tripping the registry type map.
      target_type <- if (target_col %in% names(trait_types)) {
        trait_types[[target_col]]
      } else {
        NA_character_
      }
      target_multi <- if (target_col %in% names(trait_multi)) {
        trait_multi[[target_col]]
      } else {
        FALSE
      }

      # Normalize each translated field according to the registry type when it
      # exists; for alias-defined support fields, infer a sensible type from
      # the FishBase values themselves.
      if (identical(target_type, "numeric")) {
        value_df$value_raw <- suppressWarnings(as.numeric(value_df$value_raw))
      } else if (identical(target_type, "binary")) {
        value_df$value_raw <- as.logical(value_df$value_raw)
      } else {
        value_df$value_raw <- as.character(value_df$value_raw)
        value_df$value_raw <- stringr::str_squish(value_df$value_raw)
        value_df$value_raw[!nzchar(value_df$value_raw)] <- NA_character_

        # Treat support fields as numeric when their non-missing values parse
        # cleanly as numbers. This is what allows FishBase `a` and `b` to flow
        # through as real length-weight coefficients.
        if (is.na(target_type)) {
          numeric_try <- suppressWarnings(as.numeric(value_df$value_raw))
          numeric_idx <- !is.na(value_df$value_raw)
          if (any(numeric_idx) &&
              all(!is.na(numeric_try[numeric_idx]))) {
            value_df$value_raw <- numeric_try
            target_type <- "numeric"
          }
        }
      }

      if (isTRUE(target_multi)) {
        value_df <- value_df |>
          dplyr::filter(!is.na(value_raw)) |>
          dplyr::group_by(species_name_query) |>
          dplyr::summarise(
            value = paste(unique(value_raw), collapse = ";"),
            .groups = "drop"
          )
      } else {
        value_df <- value_df |>
          dplyr::filter(!is.na(value_raw)) |>
          dplyr::group_by(species_name_query) |>
          dplyr::summarise(
            value = dplyr::first(value_raw),
            .groups = "drop"
          )
      }

      names(value_df)[2] <- target_col
      out <- dplyr::left_join(out, value_df, by = "species_name_query", suffix = c("", "_new"))
      new_col <- paste0(target_col, "_new")

      # Fill only missing registry values so earlier successful translations are
      # preserved when later source aliases map to the same target trait.
      if (identical(target_type, "numeric")) {
        if (!is.numeric(out[[target_col]])) {
          out[[target_col]] <- suppressWarnings(as.numeric(out[[target_col]]))
        }
        out[[target_col]] <- dplyr::coalesce(out[[target_col]], out[[new_col]])
      } else if (identical(target_type, "binary")) {
        out[[target_col]] <- dplyr::coalesce(out[[target_col]], out[[new_col]])
      } else {
        missing_idx <- is.na(out[[target_col]]) | !nzchar(as.character(out[[target_col]]))
        out[[target_col]][missing_idx] <- out[[new_col]][missing_idx]
      }

      out[[new_col]] <- NULL
    }
  }

  out$species_name_query <- NULL

  # Persist the fetched result exactly as returned so repeated calls can avoid
  # extra API traffic.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cache_path)
  }

  out
}

#' Fetch pelagic trait-database metadata for species names
#'
#' Reads whichever pelagic trait-database CSV files are present in a directory
#' and returns registry-aligned species metadata. Source-column aliases are
#' loaded from the external alias template, and the trait registry determines
#' the final accepted output columns.
#'
#' @param species Character vector of scientific species names to query.
#' @param dl_path Directory containing the pelagic trait-database files.
#' @param cache_path Optional path to an `.rds` cache file. When supplied, the
#'   function reads from that cache when it already exists and `refresh` is
#'   `FALSE`.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache file and
#'   rebuild the result from the available source files.
#'
#' @return A tibble containing only registry-aligned species trait columns that
#'   are defined in the trait registry.
#'
#' @examples
#' \dontrun{
#' read_pelagic_db(
#'   c("Clupea pallasii", "Sardinops sagax"),
#'   dl_path = "/path/to/pelagic_trait_files"
#' )
#' }
#'
#' @export
read_pelagic_db <- function(species,
                            dl_path,
                            cache_path = NULL,
                            refresh = FALSE) {
  # Validate the directory and cache-control arguments before any file reads
  # begin.
  if (!is.character(dl_path) || length(dl_path) != 1 || !nzchar(dl_path)) {
    stop("'dl_path' must be a single directory path.", call. = FALSE)
  }

  if (!dir.exists(dl_path)) {
    stop("'dl_path' does not exist.", call. = FALSE)
  }

  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1)) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }

  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Standardize the incoming species vector so blank and duplicate queries do
  # not trigger redundant processing.
  species <- as.character(species)
  species <- stringr::str_squish(species)
  species <- unique(species[!is.na(species) & nzchar(species)])

  if (length(species) == 0) {
    stop("No valid species names were supplied.", call. = FALSE)
  }

  # Read the registry directly and use it to decide which species-level columns
  # this ingestor is allowed to return and how their missing values should be
  # typed.
  registry <- read_trait_registry()
  species_traits <- registry$species_traits
  output_cols <- vapply(
    species_traits,
    function(x) x$coded_name,
    character(1)
  )
  trait_types <- stats::setNames(
    vapply(species_traits, function(x) x$data_type, character(1)),
    output_cols
  )
  trait_multi <- stats::setNames(
    vapply(species_traits, function(x) isTRUE(x$multi_valued), logical(1)),
    output_cols
  )

  # Build a typed missing-value template from the registry so every output row
  # has the full JSON-defined schema even when a source file does not supply a
  # value.
  template <- as.list(rep(NA_character_, length(output_cols)))
  names(template) <- output_cols
  for (nm in output_cols) {
    if (trait_types[[nm]] == "numeric") {
      template[[nm]] <- NA_real_
    } else if (trait_types[[nm]] == "binary") {
      template[[nm]] <- NA
    }
  }

  # Reuse an existing cache only when the caller explicitly supplied one and
  # requested non-refresh behavior.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Seed one registry-aligned row per queried species so direct genus/species
  # identity is preserved even when none of the local source files contains a
  # match.
  out <- tibble::as_tibble(
    lapply(template, function(x) rep(x, length(species)))
  )
  out$species_name_query <- species
  if ("genus" %in% names(out)) {
    out$genus <- stringr::word(species, start = 1, end = 1)
    out$genus[!nzchar(out$genus)] <- NA_character_
  }
  if ("species" %in% names(out)) {
    out$species <- stringr::word(species, start = 2, end = 2)
    out$species[!nzchar(out$species)] <- NA_character_
  }

  # Load source-column aliases from the external template so translation rules
  # can evolve without modifying the function body.
  alias_path <- system.file(
    "templates",
    "source_aliases.json",
    package = "tsbiomass"
  )
  source_aliases <- character(0)
  if (nzchar(alias_path) && file.exists(alias_path)) {
    alias_cfg <- read_json_file(alias_path)
    if (is.list(alias_cfg) &&
        "pelagic_db" %in% names(alias_cfg) &&
        is.list(alias_cfg$pelagic_db)) {
      source_aliases <- unlist(alias_cfg$pelagic_db, use.names = TRUE)
    }
  }

  # Process whichever source files are present. Column translation comes from
  # the external alias template rather than any paired metadata CSV.
  file_manifest <- list(
    "1_pelagic_species_trait_database.csv",
    "3_habitat_behavior_traits.csv",
    "4_morphological_traits.csv",
    "7_nutritional_raw.csv",
    "8_population_status_traits.csv",
    "data_collection_9_traits.csv",
    "data_collection_10_morphometric_ratios.csv",
    "data_collection_11_nutritional_quality.csv"
  )

  for (data_file in file_manifest) {
    data_path <- file.path(dl_path, data_file)
    if (!file.exists(data_path)) {
      next
    }

    # Read the dataset and normalize its source column names once.
    dat <- tryCatch(
      readr::read_csv(data_path, show_col_types = FALSE, name_repair = "minimal"),
      error = function(err) NULL
    )
    if (is.null(dat) || nrow(dat) == 0) {
      next
    }
    dat <- tibble::as_tibble(dat)
    names(dat) <- janitor::make_clean_names(names(dat))

    # Build a join key from the full species name when available.
    if ("sci_name" %in% names(dat)) {
      dat$species_name_query <- stringr::str_squish(as.character(dat$sci_name))
    } else if (all(c("genus", "species") %in% names(dat))) {
      dat$species_name_query <- stringr::str_squish(
        paste(dat$genus, dat$species)
      )
    } else {
      next
    }

    source_cols <- setdiff(names(dat), "species_name_query")

    for (source_col in source_cols) {
      # First try the cleaned source column name itself, then try any known
      # pelagic-database alias for that column name.
      target_candidates <- unique(c(source_col, unname(source_aliases[source_col])))
      target_candidates <- target_candidates[!is.na(target_candidates) & nzchar(target_candidates)]
      target_col <- intersect(target_candidates, names(out))

      if (length(target_col) == 0) {
        next
      }
      target_col <- target_col[[1]]

      value_df <- dat[, c("species_name_query", source_col), drop = FALSE]
      names(value_df)[2] <- "value_raw"
      if ("life_stage" %in% names(dat)) {
        value_df$life_stage <- stringr::str_to_lower(stringr::str_squish(as.character(dat$life_stage)))
        value_df$life_stage[!nzchar(value_df$life_stage)] <- NA_character_
      }

      # Normalize each translated field according to the registry type and
      # whether the target trait can accept multiple values.
      if (target_col == "body_shape") {
        value_chr <- stringr::str_squish(as.character(value_df$value_raw))
        value_chr[!nzchar(value_chr)] <- NA_character_

        # Reconcile the three body-shape encodings used across the pelagic
        # databases: direct labels, ordinal codes, and one-hot indicators.
        if (source_col == "body_shape_ordinal") {
          value_num <- suppressWarnings(as.numeric(value_df$value_raw))
          value_chr <- dplyr::case_when(
            value_num == 1 ~ "eel_like",
            value_num == 2 ~ "elongated",
            value_num == 3 ~ "fusiform",
            value_num %in% c(4, 6) ~ "short_deep",
            value_num == 5 ~ "other",
            TRUE ~ NA_character_
          )
        } else if (source_col %in% c(
          "shape_eel_like",
          "shape_elongated",
          "shape_fusiform",
          "shape_globiform",
          "shape_compressiform",
          "shape_depressiform"
        )) {
          value_num <- suppressWarnings(as.numeric(value_df$value_raw))
          value_chr <- dplyr::case_when(
            value_num == 1 & source_col == "shape_eel_like" ~ "eel_like",
            value_num == 1 & source_col == "shape_elongated" ~ "elongated",
            value_num == 1 & source_col == "shape_fusiform" ~ "fusiform",
            value_num == 1 & source_col %in% c("shape_globiform", "shape_compressiform") ~ "short_deep",
            value_num == 1 & source_col == "shape_depressiform" ~ "other",
            TRUE ~ NA_character_
          )
        } else {
          value_low <- stringr::str_to_lower(value_chr)
          value_chr <- dplyr::case_when(
            stringr::str_detect(value_low, "eel") ~ "eel_like",
            stringr::str_detect(value_low, "elong") ~ "elongated",
            stringr::str_detect(value_low, "fusiform|normal|torpedo|streamline") ~ "fusiform",
            stringr::str_detect(value_low, "compress|deep|glob|short") ~ "short_deep",
            stringr::str_detect(value_low, "depress") ~ "other",
            TRUE ~ NA_character_
          )
        }

        value_df$value_raw <- value_chr
      } else if (trait_types[[target_col]] == "numeric") {
        value_df$value_raw <- suppressWarnings(as.numeric(value_df$value_raw))
        value_df$value_raw[!is.finite(value_df$value_raw)] <- NA_real_
      } else if (trait_types[[target_col]] == "binary") {
        value_df$value_raw <- as.logical(value_df$value_raw)
      } else {
        value_df$value_raw <- as.character(value_df$value_raw)
        value_df$value_raw <- stringr::str_squish(value_df$value_raw)
        value_df$value_raw[!nzchar(value_df$value_raw)] <- NA_character_
      }

      # When a pelagic source file provides separate life-stage rows, prefer
      # adult records for the canonical length traits before collapsing the
      # species-level values. This keeps juvenile or larval sizes from diluting
      # the adult morphology values that should anchor the species table.
      if (target_col %in% c("length_min", "length_max") &&
          "life_stage" %in% names(value_df)) {
        value_df <- value_df |>
          dplyr::group_by(species_name_query) |>
          dplyr::mutate(
            .adult_available = any(life_stage == "adult" & !is.na(value_raw))
          ) |>
          dplyr::ungroup() |>
          dplyr::filter(!.adult_available | life_stage == "adult") |>
          dplyr::select(-.adult_available)
      }

      if (isTRUE(trait_multi[[target_col]])) {
        value_df <- value_df |>
          dplyr::filter(!is.na(value_raw)) |>
          dplyr::group_by(species_name_query) |>
          dplyr::summarise(
            value = paste(unique(value_raw), collapse = ";"),
            .groups = "drop"
          )
      } else if (trait_types[[target_col]] == "numeric") {
        value_df <- value_df |>
          dplyr::group_by(species_name_query) |>
          dplyr::summarise(
            value = mean(value_raw[is.finite(value_raw)], na.rm = TRUE),
            .groups = "drop"
          )
        value_df$value[!is.finite(value_df$value)] <- NA_real_
      } else {
        value_df <- value_df |>
          dplyr::filter(!is.na(value_raw)) |>
          dplyr::group_by(species_name_query) |>
          dplyr::summarise(
            value = dplyr::first(value_raw),
            .groups = "drop"
          )
      }

      names(value_df)[2] <- target_col
      out <- dplyr::left_join(out, value_df, by = "species_name_query", suffix = c("", "_new"))
      new_col <- paste0(target_col, "_new")

      # Fill only missing registry values so earlier successful values from one
      # pelagic source file are preserved when a later file is processed. For
      # multi-valued set traits, merge the existing and new values together so
      # fields like `gregarious` can retain multiple reported categories.
      if (isTRUE(trait_multi[[target_col]])) {
        out[[target_col]] <- vapply(
          seq_len(nrow(out)),
          function(i) {
            parts <- c(
              stringr::str_split(as.character(out[[target_col]][[i]]), ";", simplify = FALSE)[[1]],
              stringr::str_split(as.character(out[[new_col]][[i]]), ";", simplify = FALSE)[[1]]
            )
            parts <- stringr::str_squish(parts)
            parts <- unique(parts[!is.na(parts) & nzchar(parts)])
            if (length(parts) == 0) {
              return(NA_character_)
            }
            paste(parts, collapse = ";")
          },
          character(1)
        )
      } else if (trait_types[[target_col]] == "numeric") {
        out[[target_col]] <- dplyr::coalesce(out[[target_col]], out[[new_col]])
      } else if (trait_types[[target_col]] == "binary") {
        out[[target_col]] <- dplyr::coalesce(out[[target_col]], out[[new_col]])
      } else {
        missing_idx <- is.na(out[[target_col]]) | !nzchar(as.character(out[[target_col]]))
        out[[target_col]][missing_idx] <- out[[new_col]][missing_idx]
      }

      out[[new_col]] <- NULL
    }
  }

  # Derive midpoint and range traits from the merged min/max values when the
  # registry expects them and no source provided them directly.
  if (all(c("depth_min", "depth_max", "depth_range") %in% names(out))) {
    missing_idx <- is.na(out$depth_range) & !is.na(out$depth_min) & !is.na(out$depth_max)
    out$depth_range[missing_idx] <- out$depth_max[missing_idx] - out$depth_min[missing_idx]
  }
  if (all(c("depth_min", "depth_max", "depth_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$depth_midpoint) & !is.na(out$depth_min) & !is.na(out$depth_max)
    out$depth_midpoint[missing_idx] <- (out$depth_min[missing_idx] + out$depth_max[missing_idx]) / 2
  }
  if (all(c("temperature_min", "temperature_max", "temperature_range") %in% names(out))) {
    missing_idx <- is.na(out$temperature_range) & !is.na(out$temperature_min) & !is.na(out$temperature_max)
    out$temperature_range[missing_idx] <- out$temperature_max[missing_idx] - out$temperature_min[missing_idx]
  }
  if (all(c("temperature_min", "temperature_max", "temperature_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$temperature_midpoint) & !is.na(out$temperature_min) & !is.na(out$temperature_max)
    out$temperature_midpoint[missing_idx] <- (out$temperature_min[missing_idx] + out$temperature_max[missing_idx]) / 2
  }
  if (all(c("length_min", "length_max", "length_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$length_midpoint) & !is.na(out$length_min) & !is.na(out$length_max)
    out$length_midpoint[missing_idx] <- (out$length_min[missing_idx] + out$length_max[missing_idx]) / 2
  }

  out$species_name_query <- NULL

  # Persist the fetched result exactly as returned so repeated calls can avoid
  # re-reading the same local source files.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cache_path)
  }

  out
}

#' Fetch Azores trait-database metadata for species names
#'
#' Reads the Azores fish trait database tab file and returns registry-aligned
#' species metadata. Source-column aliases are loaded from the external alias
#' template, and the trait registry determines the final accepted output
#' columns.
#'
#' @param species Character vector of scientific species names to query.
#' @param db_path Path to the `Azores_FishTraits_2023.tab` file.
#' @param cache_path Optional path to an `.rds` cache file. When supplied, the
#'   function reads from that cache when it already exists and `refresh` is
#'   `FALSE`.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache file and
#'   rebuild the result from the source file.
#'
#' @return A tibble containing only registry-aligned species trait columns that
#'   are defined in the trait registry.
#'
#' @examples
#' \dontrun{
#' read_azores_db(
#'   c("Clupea pallasii", "Sardinops sagax"),
#'   db_path = "/path/to/Azores_FishTraits_2023.tab"
#' )
#' }
#'
#' @export
read_azores_db <- function(species,
                           db_path,
                           cache_path = NULL,
                           refresh = FALSE) {
  # Validate the source-file and cache-control arguments before any file reads
  # begin.
  if (!is.character(db_path) || length(db_path) != 1 || !nzchar(db_path)) {
    stop("'db_path' must be a single file path.", call. = FALSE)
  }

  if (!file.exists(db_path)) {
    stop("'db_path' does not exist.", call. = FALSE)
  }

  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1)) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }

  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Standardize the incoming species vector so blank and duplicate queries do
  # not trigger redundant processing.
  species <- as.character(species)
  species <- stringr::str_squish(species)
  species <- unique(species[!is.na(species) & nzchar(species)])

  if (length(species) == 0) {
    stop("No valid species names were supplied.", call. = FALSE)
  }

  # Read the registry directly and use it to decide which species-level columns
  # this ingestor is allowed to return and how their missing values should be
  # typed.
  registry <- read_trait_registry()
  species_traits <- registry$species_traits
  output_cols <- vapply(
    species_traits,
    function(x) x$coded_name,
    character(1)
  )
  trait_types <- stats::setNames(
    vapply(species_traits, function(x) x$data_type, character(1)),
    output_cols
  )
  trait_multi <- stats::setNames(
    vapply(species_traits, function(x) isTRUE(x$multi_valued), logical(1)),
    output_cols
  )

  # Build a typed missing-value template from the registry so every output row
  # has the full JSON-defined schema even when the Azores file does not supply a
  # value.
  template <- as.list(rep(NA_character_, length(output_cols)))
  names(template) <- output_cols
  for (nm in output_cols) {
    if (trait_types[[nm]] == "numeric") {
      template[[nm]] <- NA_real_
    } else if (trait_types[[nm]] == "binary") {
      template[[nm]] <- NA
    }
  }

  # Reuse an existing cache only when the caller explicitly supplied one and
  # requested non-refresh behavior.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Seed one registry-aligned row per queried species so direct genus/species
  # identity is preserved even when the source file does not contain a match.
  out <- tibble::as_tibble(
    lapply(template, function(x) rep(x, length(species)))
  )
  out$species_name_query <- species
  if ("genus" %in% names(out)) {
    out$genus <- stringr::word(species, start = 1, end = 1)
    out$genus[!nzchar(out$genus)] <- NA_character_
  }
  if ("species" %in% names(out)) {
    out$species <- stringr::word(species, start = 2, end = 2)
    out$species[!nzchar(out$species)] <- NA_character_
  }

  # Load source-column aliases from the external template so translation rules
  # can evolve without modifying the function body.
  alias_path <- system.file(
    "templates",
    "source_aliases.json",
    package = "tsbiomass"
  )
  source_aliases <- character(0)
  if (nzchar(alias_path) && file.exists(alias_path)) {
    alias_cfg <- read_json_file(alias_path)
    if (is.list(alias_cfg) &&
        "azores_db" %in% names(alias_cfg) &&
        is.list(alias_cfg$azores_db)) {
      source_aliases <- unlist(alias_cfg$azores_db, use.names = TRUE)
    }
  }

  # Read the Azores tab file and normalize its source names once. The first 64
  # lines are descriptive metadata rather than the actual tabular header row.
  dat <- tryCatch(
    readr::read_tsv(db_path, skip = 64, show_col_types = FALSE, name_repair = "minimal"),
    error = function(err) NULL
  )
  if (is.null(dat) || nrow(dat) == 0) {
    out$species_name_query <- NULL
    return(out)
  }
  dat <- tibble::as_tibble(dat)
  names(dat) <- janitor::make_clean_names(names(dat))

  # Build a join key from the full species name when available.
  if ("species" %in% names(dat)) {
    dat$species_name_query <- stringr::str_squish(as.character(dat$species))
  } else if (all(c("genus", "species") %in% names(dat))) {
    dat$species_name_query <- stringr::str_squish(
      paste(dat$genus, dat$species)
    )
  } else {
    stop("The Azores file does not contain a usable species-name column.", call. = FALSE)
  }

  source_cols <- setdiff(names(dat), "species_name_query")

  for (source_col in source_cols) {
    # First try the cleaned source column name itself, then try any known
    # Azores alias for that column name.
    target_candidates <- unique(c(source_col, unname(source_aliases[source_col])))
    target_candidates <- target_candidates[!is.na(target_candidates) & nzchar(target_candidates)]
    target_col <- intersect(target_candidates, names(out))

    if (length(target_col) == 0) {
      next
    }
    target_col <- target_col[[1]]

    value_df <- dat[, c("species_name_query", source_col), drop = FALSE]
    names(value_df)[2] <- "value_raw"

    # Normalize each translated field according to the registry type. A few
    # Azores columns need light source-specific cleanup before they fit the
    # canonical registry field.
    if (target_col == "body_shape") {
      value_chr <- stringr::str_squish(as.character(value_df$value_raw))
      value_chr[!nzchar(value_chr)] <- NA_character_
      value_low <- stringr::str_to_lower(value_chr)
      value_df$value_raw <- dplyr::case_when(
        stringr::str_detect(value_low, "eel") ~ "eel_like",
        stringr::str_detect(value_low, "elong") ~ "elongated",
        stringr::str_detect(value_low, "fusiform|normal|torpedo|streamline") ~ "fusiform",
        stringr::str_detect(value_low, "compress|deep|glob|short") ~ "short_deep",
        stringr::str_detect(value_low, "depress") ~ "other",
        TRUE ~ NA_character_
      )
    } else if (target_col == "length_min" && source_col == "lfm_mm_life_history") {
      value_num <- suppressWarnings(as.numeric(value_df$value_raw))
      value_num[!is.finite(value_num)] <- NA_real_
      value_df$value_raw <- value_num / 10
    } else if (target_col == "length_max" &&
               source_col == "size_habitat_use_maximum_body_length") {
      value_chr <- stringr::str_squish(as.character(value_df$value_raw))
      value_chr[!nzchar(value_chr)] <- NA_character_
      value_df$value_raw <- suppressWarnings(as.numeric(
        stringr::str_extract(value_chr, "[0-9]+\\.?[0-9]*(?!.*[0-9])")
      ))
    } else if (trait_types[[target_col]] == "numeric") {
      value_df$value_raw <- suppressWarnings(as.numeric(value_df$value_raw))
      value_df$value_raw[!is.finite(value_df$value_raw)] <- NA_real_
    } else if (trait_types[[target_col]] == "binary") {
      value_df$value_raw <- as.logical(value_df$value_raw)
    } else {
      value_df$value_raw <- as.character(value_df$value_raw)
      value_df$value_raw <- stringr::str_squish(value_df$value_raw)
      value_df$value_raw[!nzchar(value_df$value_raw)] <- NA_character_
    }

    if (isTRUE(trait_multi[[target_col]])) {
      value_df <- value_df |>
        dplyr::filter(!is.na(value_raw)) |>
        dplyr::group_by(species_name_query) |>
        dplyr::summarise(
          value = paste(unique(value_raw), collapse = ";"),
          .groups = "drop"
        )
    } else if (trait_types[[target_col]] == "numeric") {
      value_df <- value_df |>
        dplyr::group_by(species_name_query) |>
        dplyr::summarise(
          value = mean(value_raw[is.finite(value_raw)], na.rm = TRUE),
          .groups = "drop"
        )
      value_df$value[!is.finite(value_df$value)] <- NA_real_
    } else {
      value_df <- value_df |>
        dplyr::filter(!is.na(value_raw)) |>
        dplyr::group_by(species_name_query) |>
        dplyr::summarise(
          value = dplyr::first(value_raw),
          .groups = "drop"
        )
    }

    names(value_df)[2] <- target_col
    out <- dplyr::left_join(out, value_df, by = "species_name_query", suffix = c("", "_new"))
    new_col <- paste0(target_col, "_new")

    # Fill only missing registry values so earlier successful translations are
    # preserved when multiple source columns map to the same target trait.
    if (trait_types[[target_col]] == "numeric") {
      out[[target_col]] <- dplyr::coalesce(out[[target_col]], out[[new_col]])
    } else if (trait_types[[target_col]] == "binary") {
      out[[target_col]] <- dplyr::coalesce(out[[target_col]], out[[new_col]])
    } else {
      missing_idx <- is.na(out[[target_col]]) | !nzchar(as.character(out[[target_col]]))
      out[[target_col]][missing_idx] <- out[[new_col]][missing_idx]
    }

    out[[new_col]] <- NULL
  }

  # Derive midpoint and range traits from the merged min/max values when the
  # registry expects them and no source provided them directly.
  if (all(c("depth_min", "depth_max", "depth_range") %in% names(out))) {
    missing_idx <- is.na(out$depth_range) & !is.na(out$depth_min) & !is.na(out$depth_max)
    out$depth_range[missing_idx] <- out$depth_max[missing_idx] - out$depth_min[missing_idx]
  }
  if (all(c("depth_min", "depth_max", "depth_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$depth_midpoint) & !is.na(out$depth_min) & !is.na(out$depth_max)
    out$depth_midpoint[missing_idx] <- (out$depth_min[missing_idx] + out$depth_max[missing_idx]) / 2
  }
  if (all(c("temperature_min", "temperature_max", "temperature_range") %in% names(out))) {
    missing_idx <- is.na(out$temperature_range) & !is.na(out$temperature_min) & !is.na(out$temperature_max)
    out$temperature_range[missing_idx] <- out$temperature_max[missing_idx] - out$temperature_min[missing_idx]
  }
  if (all(c("temperature_min", "temperature_max", "temperature_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$temperature_midpoint) & !is.na(out$temperature_min) & !is.na(out$temperature_max)
    out$temperature_midpoint[missing_idx] <- (out$temperature_min[missing_idx] + out$temperature_max[missing_idx]) / 2
  }
  if (all(c("length_min", "length_max", "length_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$length_midpoint) & !is.na(out$length_min) & !is.na(out$length_max)
    out$length_midpoint[missing_idx] <- (out$length_min[missing_idx] + out$length_max[missing_idx]) / 2
  }

  out$species_name_query <- NULL

  # Persist the fetched result exactly as returned so repeated calls can avoid
  # re-reading the same local source file.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cache_path)
  }

  out
}

#' Fetch continental shelf trait-database metadata for species names
#'
#' Reads the `TraitCollectionFishNAtlanticNEPacificContShelf.xlsx` workbook and
#' returns registry-aligned species metadata from the `Trait values` sheet.
#' Source-column aliases are loaded from the external alias template, and the
#' trait registry determines the final accepted output columns.
#'
#' @param species Character vector of scientific species names to query.
#' @param db_path Path to the
#'   `TraitCollectionFishNAtlanticNEPacificContShelf.xlsx` file.
#' @param cache_path Optional path to an `.rds` cache file. When supplied, the
#'   function reads from that cache when it already exists and `refresh` is
#'   `FALSE`.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache file and
#'   rebuild the result from the source file.
#'
#' @return A tibble containing only registry-aligned species trait columns that
#'   are defined in the trait registry.
#'
#' @examples
#' \dontrun{
#' read_continental_db(
#'   c("Clupea pallasii", "Sardinops sagax"),
#'   db_path = "/path/to/TraitCollectionFishNAtlanticNEPacificContShelf.xlsx"
#' )
#' }
#'
#' @export
read_continental_db <- function(species,
                                db_path,
                                cache_path = NULL,
                                refresh = FALSE) {
  # Validate the source-file and cache-control arguments before any file reads
  # begin.
  if (!is.character(db_path) || length(db_path) != 1 || !nzchar(db_path)) {
    stop("'db_path' must be a single file path.", call. = FALSE)
  }

  if (!file.exists(db_path)) {
    stop("'db_path' does not exist.", call. = FALSE)
  }

  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1)) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }

  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Standardize the incoming species vector so blank and duplicate queries do
  # not trigger redundant processing.
  species <- as.character(species)
  species <- stringr::str_squish(species)
  species <- unique(species[!is.na(species) & nzchar(species)])

  if (length(species) == 0) {
    stop("No valid species names were supplied.", call. = FALSE)
  }

  # Read the registry directly and use it to decide which species-level columns
  # this ingestor is allowed to return and how their missing values should be
  # typed.
  registry <- read_trait_registry()
  species_traits <- registry$species_traits
  output_cols <- vapply(
    species_traits,
    function(x) x$coded_name,
    character(1)
  )
  trait_types <- stats::setNames(
    vapply(species_traits, function(x) x$data_type, character(1)),
    output_cols
  )
  trait_multi <- stats::setNames(
    vapply(species_traits, function(x) isTRUE(x$multi_valued), logical(1)),
    output_cols
  )

  # Build a typed missing-value template from the registry so every output row
  # has the full JSON-defined schema even when the continental database does not
  # supply a value.
  template <- as.list(rep(NA_character_, length(output_cols)))
  names(template) <- output_cols
  for (nm in output_cols) {
    if (trait_types[[nm]] == "numeric") {
      template[[nm]] <- NA_real_
    } else if (trait_types[[nm]] == "binary") {
      template[[nm]] <- NA
    }
  }

  # Reuse an existing cache only when the caller explicitly supplied one and
  # requested non-refresh behavior.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Seed one registry-aligned row per queried species so direct genus/species
  # identity is preserved even when the source file does not contain a match.
  out <- tibble::as_tibble(
    lapply(template, function(x) rep(x, length(species)))
  )
  out$species_name_query <- species
  if ("genus" %in% names(out)) {
    out$genus <- stringr::word(species, start = 1, end = 1)
    out$genus[!nzchar(out$genus)] <- NA_character_
  }
  if ("species" %in% names(out)) {
    out$species <- stringr::word(species, start = 2, end = 2)
    out$species[!nzchar(out$species)] <- NA_character_
  }

  # Load source-column aliases from the external template so translation rules
  # can evolve without modifying the function body.
  alias_path <- system.file(
    "templates",
    "source_aliases.json",
    package = "tsbiomass"
  )
  source_aliases <- character(0)
  if (nzchar(alias_path) && file.exists(alias_path)) {
    alias_cfg <- read_json_file(alias_path)
    if (is.list(alias_cfg) &&
        "continental_db" %in% names(alias_cfg) &&
        is.list(alias_cfg$continental_db)) {
      source_aliases <- unlist(alias_cfg$continental_db, use.names = TRUE)
    }
  }

  # Read the trait-values sheet and normalize its source names once. The
  # explanations sheet documents the variables, but the ingestor only needs the
  # actual data sheet.
  dat <- tryCatch(
    readxl::read_excel(db_path, sheet = "Trait values", .name_repair = "minimal"),
    error = function(err) NULL
  )
  if (is.null(dat) || nrow(dat) == 0) {
    out$species_name_query <- NULL
    return(out)
  }
  dat <- tibble::as_tibble(dat)
  names(dat) <- janitor::make_clean_names(names(dat))

  # Build a join key from genus and species, which are present as separate
  # columns in the continental trait workbook.
  if (all(c("genus", "species") %in% names(dat))) {
    dat$species_name_query <- stringr::str_squish(
      paste(dat$genus, dat$species)
    )
  } else {
    stop("The continental database does not contain usable genus/species columns.", call. = FALSE)
  }

  source_cols <- setdiff(names(dat), "species_name_query")

  for (source_col in source_cols) {
    # First try the cleaned source column name itself, then try any known
    # continental-database alias for that column name.
    target_candidates <- unique(c(source_col, unname(source_aliases[source_col])))
    target_candidates <- target_candidates[!is.na(target_candidates) & nzchar(target_candidates)]
    target_col <- intersect(target_candidates, names(out))

    if (length(target_col) == 0) {
      next
    }
    target_col <- target_col[[1]]

    value_df <- dat[, c("species_name_query", source_col), drop = FALSE]
    names(value_df)[2] <- "value_raw"

    # Normalize each translated field according to the registry type. A few
    # continental-database fields need light source-specific cleanup first.
    if (target_col == "body_shape") {
      value_chr <- stringr::str_squish(as.character(value_df$value_raw))
      value_chr[!nzchar(value_chr)] <- NA_character_
      value_low <- stringr::str_to_lower(value_chr)
      value_df$value_raw <- dplyr::case_when(
        stringr::str_detect(value_low, "eel") ~ "eel_like",
        stringr::str_detect(value_low, "elong") ~ "elongated",
        stringr::str_detect(value_low, "fusiform") ~ "fusiform",
        stringr::str_detect(value_low, "compress|short and/or deep") ~ "short_deep",
        stringr::str_detect(value_low, "flat") ~ "other",
        TRUE ~ NA_character_
      )
    } else if (trait_types[[target_col]] == "numeric") {
      value_df$value_raw <- suppressWarnings(as.numeric(value_df$value_raw))
      value_df$value_raw[!is.finite(value_df$value_raw)] <- NA_real_
    } else if (trait_types[[target_col]] == "binary") {
      value_df$value_raw <- as.logical(value_df$value_raw)
    } else {
      value_df$value_raw <- as.character(value_df$value_raw)
      value_df$value_raw <- stringr::str_squish(value_df$value_raw)
      value_df$value_raw[!nzchar(value_df$value_raw)] <- NA_character_
    }

    if (isTRUE(trait_multi[[target_col]])) {
      value_df <- value_df |>
        dplyr::filter(!is.na(value_raw)) |>
        dplyr::group_by(species_name_query) |>
        dplyr::summarise(
          value = paste(unique(value_raw), collapse = ";"),
          .groups = "drop"
        )
    } else if (trait_types[[target_col]] == "numeric") {
      value_df <- value_df |>
        dplyr::group_by(species_name_query) |>
        dplyr::summarise(
          value = mean(value_raw[is.finite(value_raw)], na.rm = TRUE),
          .groups = "drop"
        )
      value_df$value[!is.finite(value_df$value)] <- NA_real_
    } else {
      value_df <- value_df |>
        dplyr::filter(!is.na(value_raw)) |>
        dplyr::group_by(species_name_query) |>
        dplyr::summarise(
          value = dplyr::first(value_raw),
          .groups = "drop"
        )
    }

    names(value_df)[2] <- target_col
    out <- dplyr::left_join(out, value_df, by = "species_name_query", suffix = c("", "_new"))
    new_col <- paste0(target_col, "_new")

    # Fill only missing registry values so earlier successful translations are
    # preserved when multiple source columns map to the same target trait.
    if (trait_types[[target_col]] == "numeric") {
      out[[target_col]] <- dplyr::coalesce(out[[target_col]], out[[new_col]])
    } else if (trait_types[[target_col]] == "binary") {
      out[[target_col]] <- dplyr::coalesce(out[[target_col]], out[[new_col]])
    } else {
      missing_idx <- is.na(out[[target_col]]) | !nzchar(as.character(out[[target_col]]))
      out[[target_col]][missing_idx] <- out[[new_col]][missing_idx]
    }

    out[[new_col]] <- NULL
  }

  # Derive midpoint and range traits from the merged min/max values when the
  # registry expects them and no source provided them directly.
  if (all(c("depth_min", "depth_max", "depth_range") %in% names(out))) {
    missing_idx <- is.na(out$depth_range) & !is.na(out$depth_min) & !is.na(out$depth_max)
    out$depth_range[missing_idx] <- out$depth_max[missing_idx] - out$depth_min[missing_idx]
  }
  if (all(c("depth_min", "depth_max", "depth_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$depth_midpoint) & !is.na(out$depth_min) & !is.na(out$depth_max)
    out$depth_midpoint[missing_idx] <- (out$depth_min[missing_idx] + out$depth_max[missing_idx]) / 2
  }
  if (all(c("temperature_min", "temperature_max", "temperature_range") %in% names(out))) {
    missing_idx <- is.na(out$temperature_range) & !is.na(out$temperature_min) & !is.na(out$temperature_max)
    out$temperature_range[missing_idx] <- out$temperature_max[missing_idx] - out$temperature_min[missing_idx]
  }
  if (all(c("temperature_min", "temperature_max", "temperature_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$temperature_midpoint) & !is.na(out$temperature_min) & !is.na(out$temperature_max)
    out$temperature_midpoint[missing_idx] <- (out$temperature_min[missing_idx] + out$temperature_max[missing_idx]) / 2
  }
  if (all(c("length_min", "length_max", "length_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$length_midpoint) & !is.na(out$length_min) & !is.na(out$length_max)
    out$length_midpoint[missing_idx] <- (out$length_min[missing_idx] + out$length_max[missing_idx]) / 2
  }

  out$species_name_query <- NULL

  # Persist the fetched result exactly as returned so repeated calls can avoid
  # re-reading the same local source file.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cache_path)
  }

  out
}

#' Read marineSpeciesTraits metadata for species names
#'
#' Reads a `marineSpeciesTraits.RData` file, extracts the species-level trait
#' columns that align with the package trait registry, and returns only the
#' processed output tibble. The source `.RData` object is loaded into a private
#' environment and removed immediately after the processed table has been built,
#' so the raw source database is not retained in memory.
#'
#' @param species Character vector of scientific species names to query.
#' @param db_path Path to the `marineSpeciesTraits.RData` file.
#' @param cache_path Optional path to an `.rds` cache file. When supplied, the
#'   function reads from that cache when it already exists and `refresh` is
#'   `FALSE`.
#' @param refresh Logical scalar. If `TRUE`, ignore any existing cache file and
#'   rebuild the result from the source file.
#'
#' @return A tibble containing only registry-aligned species trait columns that
#'   are defined in the trait registry.
#'
#' @examples
#' \dontrun{
#' read_mstraits_db(
#'   c("Clupea pallasii", "Sardinops sagax"),
#'   db_path = "/path/to/marineSpeciesTraits.RData"
#' )
#' }
#'
#' @export
read_mstraits_db <- function(species,
                             db_path,
                             cache_path = NULL,
                             refresh = FALSE) {
  # Validate the source-file and cache-control arguments before any file reads
  # begin.
  if (!is.character(db_path) || length(db_path) != 1 || !nzchar(db_path)) {
    stop("'db_path' must be a single file path.", call. = FALSE)
  }

  if (!file.exists(db_path)) {
    stop("'db_path' does not exist.", call. = FALSE)
  }

  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1)) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }

  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("'refresh' must be TRUE or FALSE.", call. = FALSE)
  }

  # Standardize the incoming species vector so blank and duplicate queries do
  # not trigger redundant processing.
  species <- as.character(species)
  species <- stringr::str_squish(species)
  species <- unique(species[!is.na(species) & nzchar(species)])

  if (length(species) == 0) {
    stop("No valid species names were supplied.", call. = FALSE)
  }

  # Read the registry directly and use it to decide which species-level columns
  # this ingestor is allowed to return and how their missing values should be
  # typed.
  registry <- read_trait_registry()
  species_traits <- registry$species_traits
  output_cols <- vapply(
    species_traits,
    function(x) x$coded_name,
    character(1)
  )
  trait_types <- stats::setNames(
    vapply(species_traits, function(x) x$data_type, character(1)),
    output_cols
  )
  trait_multi <- stats::setNames(
    vapply(species_traits, function(x) isTRUE(x$multi_valued), logical(1)),
    output_cols
  )

  # Build a typed missing-value template from the registry so every output row
  # has the full JSON-defined schema even when the source file does not supply a
  # value.
  template <- as.list(rep(NA_character_, length(output_cols)))
  names(template) <- output_cols
  for (nm in output_cols) {
    if (trait_types[[nm]] == "numeric") {
      template[[nm]] <- NA_real_
    } else if (trait_types[[nm]] == "binary") {
      template[[nm]] <- NA
    }
  }

  # Reuse an existing cache only when the caller explicitly supplied one and
  # requested non-refresh behavior.
  if (!is.null(cache_path) && file.exists(cache_path) && !refresh) {
    return(readRDS(cache_path))
  }

  # Seed one registry-aligned row per queried species so direct genus/species
  # identity is preserved even when the source file does not contain a match.
  out <- tibble::as_tibble(
    lapply(template, function(x) rep(x, length(species)))
  )
  out$species_name_query <- species
  if ("genus" %in% names(out)) {
    out$genus <- stringr::word(species, start = 1, end = 1)
    out$genus[!nzchar(out$genus)] <- NA_character_
  }
  if ("species" %in% names(out)) {
    out$species <- stringr::word(species, start = 2, end = 2)
    out$species[!nzchar(out$species)] <- NA_character_
  }

  # Load source-column aliases from the external template so translation rules
  # can evolve without modifying the function body.
  alias_path <- system.file(
    "templates",
    "source_aliases.json",
    package = "tsbiomass"
  )
  source_aliases <- character(0)
  if (nzchar(alias_path) && file.exists(alias_path)) {
    alias_cfg <- read_json_file(alias_path)
    if (is.list(alias_cfg) &&
        "mstraits_db" %in% names(alias_cfg) &&
        is.list(alias_cfg$mstraits_db)) {
      source_aliases <- unlist(alias_cfg$mstraits_db, use.names = TRUE)
    }
  }

  # Load the `.RData` into a private environment so the raw source object never
  # lands in the caller workspace and can be removed immediately after use.
  load_env <- new.env(parent = emptyenv())
  loaded_objects <- load(db_path, envir = load_env)

  # Keep only the first data-frame object from the private environment. The
  # source file currently stores `speciesList`, but the code does not assume the
  # object name so the reader is resilient to future renaming.
  object_names <- loaded_objects[vapply(
    loaded_objects,
    function(nm) is.data.frame(get(nm, envir = load_env, inherits = FALSE)),
    logical(1)
  )]

  if (length(object_names) == 0) {
    rm(list = ls(envir = load_env, all.names = TRUE), envir = load_env)
    stop("The RData file does not contain a usable data frame object.", call. = FALSE)
  }

  dat <- tibble::as_tibble(get(object_names[[1]], envir = load_env, inherits = FALSE))

  # Drop the raw loaded objects immediately after extracting the one working
  # table so only the processed output remains in memory.
  rm(list = ls(envir = load_env, all.names = TRUE), envir = load_env)
  rm(load_env)
  invisible(gc(FALSE))

  if (nrow(dat) == 0) {
    out$species_name_query <- NULL
    return(out)
  }

  # Normalize the source names once so alias translation can follow the same
  # cleaned naming convention used by the other ingestors.
  names(dat) <- janitor::make_clean_names(names(dat))

  # Build a join key from the most stable scientific-name column available in
  # the source file. Valid names take precedence over the original labels.
  name_cols <- intersect(
    c("valid_name", "scientific_name", "original_name"),
    names(dat)
  )
  if (length(name_cols) == 0) {
    stop("The marineSpeciesTraits file does not contain a usable species-name column.", call. = FALSE)
  }

  dat$species_name_query <- NA_character_
  for (nm in name_cols) {
    fill_idx <- is.na(dat$species_name_query) | !nzchar(dat$species_name_query)
    dat$species_name_query[fill_idx] <- stringr::str_squish(as.character(dat[[nm]][fill_idx]))
  }
  dat$species_name_query[!nzchar(dat$species_name_query)] <- NA_character_
  dat <- dat |>
    dplyr::filter(!is.na(species_name_query)) |>
    dplyr::filter(species_name_query %in% species)

  if (nrow(dat) == 0) {
    out$species_name_query <- NULL
    return(out)
  }

  source_cols <- setdiff(names(dat), "species_name_query")

  for (source_col in source_cols) {
    # First try the cleaned source column name itself, then try any known
    # marineSpeciesTraits alias for that column name.
    target_candidates <- unique(c(source_col, unname(source_aliases[source_col])))
    target_candidates <- target_candidates[!is.na(target_candidates) & nzchar(target_candidates)]
    target_col <- intersect(target_candidates, names(out))

    if (length(target_col) == 0) {
      next
    }
    target_col <- target_col[[1]]

    value_df <- dat[, c("species_name_query", source_col), drop = FALSE]
    names(value_df)[2] <- "value_raw"

    # Normalize each translated field according to the registry type and the
    # source-value syntax used in marineSpeciesTraits.
    if (trait_types[[target_col]] == "numeric") {
      value_df$value_raw <- suppressWarnings(as.numeric(value_df$value_raw))
      value_df$value_raw[!is.finite(value_df$value_raw)] <- NA_real_
    } else if (trait_types[[target_col]] == "binary") {
      value_df$value_raw <- as.logical(value_df$value_raw)
    } else {
      value_df$value_raw <- as.character(value_df$value_raw)
      value_df$value_raw <- stringr::str_squish(value_df$value_raw)
      value_df$value_raw[!nzchar(value_df$value_raw)] <- NA_character_
      value_df$value_raw <- stringr::str_replace_all(value_df$value_raw, "\\s*,\\s*", ";")
    }

    if (isTRUE(trait_multi[[target_col]])) {
      value_df <- value_df |>
        dplyr::filter(!is.na(value_raw)) |>
        dplyr::group_by(species_name_query) |>
        dplyr::summarise(
          value = paste(unique(unlist(strsplit(value_raw, ";", fixed = TRUE))), collapse = ";"),
          .groups = "drop"
        )
      value_df$value <- stringr::str_squish(value_df$value)
      value_df$value[!nzchar(value_df$value)] <- NA_character_
    } else {
      value_df <- value_df |>
        dplyr::filter(!is.na(value_raw)) |>
        dplyr::group_by(species_name_query) |>
        dplyr::summarise(
          value = dplyr::first(value_raw),
          .groups = "drop"
        )
    }

    names(value_df)[2] <- target_col
    out <- dplyr::left_join(out, value_df, by = "species_name_query", suffix = c("", "_new"))
    new_col <- paste0(target_col, "_new")

    # Fill only missing registry values so earlier successful translations are
    # preserved when multiple source columns map to the same target trait.
    if (trait_types[[target_col]] == "numeric") {
      out[[target_col]] <- dplyr::coalesce(out[[target_col]], out[[new_col]])
    } else if (trait_types[[target_col]] == "binary") {
      out[[target_col]] <- dplyr::coalesce(out[[target_col]], out[[new_col]])
    } else {
      missing_idx <- is.na(out[[target_col]]) | !nzchar(as.character(out[[target_col]]))
      out[[target_col]][missing_idx] <- out[[new_col]][missing_idx]
    }

    out[[new_col]] <- NULL
  }

  # The registry uses explicit genus/species fields. Derive those two fields
  # from the source species name after value filling so source taxonomy can
  # override the original query when the file contains a cleaner label.
  if ("genus" %in% names(out)) {
    genus_fill <- stringr::word(out$species_name_query, start = 1, end = 1)
    genus_fill[!nzchar(genus_fill)] <- NA_character_
    missing_idx <- is.na(out$genus) | !nzchar(out$genus)
    out$genus[missing_idx] <- genus_fill[missing_idx]
  }

  if ("species" %in% names(out)) {
    species_fill <- stringr::word(out$species_name_query, start = 2, end = 2)
    species_fill[!nzchar(species_fill)] <- NA_character_
    missing_idx <- is.na(out$species) | !nzchar(out$species)
    out$species[missing_idx] <- species_fill[missing_idx]
  }

  # Derive midpoint and range traits from the merged min/max values when the
  # registry expects them and no source provided them directly.
  if (all(c("depth_min", "depth_max", "depth_range") %in% names(out))) {
    missing_idx <- is.na(out$depth_range) & !is.na(out$depth_min) & !is.na(out$depth_max)
    out$depth_range[missing_idx] <- out$depth_max[missing_idx] - out$depth_min[missing_idx]
  }
  if (all(c("depth_min", "depth_max", "depth_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$depth_midpoint) & !is.na(out$depth_min) & !is.na(out$depth_max)
    out$depth_midpoint[missing_idx] <- (out$depth_min[missing_idx] + out$depth_max[missing_idx]) / 2
  }

  out$species_name_query <- NULL

  # Persist the processed result exactly as returned so repeated calls can
  # avoid re-reading the `.RData` source.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cache_path)
  }

  out
}

#' Format one manual database entry against the trait registry
#'
#' Takes a named list, checks that each supplied field exists in the trait
#' registry, coerces values to the registry-defined type, and returns a
#' one-row tibble with the full registry-aligned schema for the requested
#' scope.
#'
#' @param entry Named list containing one manual database entry.
#' @param scope Character scalar indicating which registry scope to use.
#'   Allowed values are `"species"`, `"study"`, and `"all"`.
#' @param registry_path Optional path to a trait-registry JSON file. When
#'   `NULL`, the installed package registry is used.
#' @param drop_unknown Logical scalar. If `TRUE`, input names that are not in
#'   the registry are silently dropped. If `FALSE`, the function errors on
#'   unknown names.
#'
#' @return A one-row tibble aligned to the requested registry scope.
#'
#' @examples
#' \dontrun{
#' format_db_entry(list(
#'   genus = "Clupea",
#'   species = "pallasii",
#'   body_shape = "fusiform",
#'   trophic = 3.8
#' ))
#' }
#'
#' @export
format_db_entry <- function(entry,
                            scope = c("species", "study", "all"),
                            registry_path = NULL,
                            drop_unknown = FALSE) {
  # Validate the entry object and the unknown-field handling mode before any
  # registry work begins.
  if (!is.list(entry) || is.data.frame(entry)) {
    stop("'entry' must be a named list.", call. = FALSE)
  }

  if (is.null(names(entry)) || any(is.na(names(entry))) || any(!nzchar(names(entry)))) {
    stop("'entry' must be a named list with non-empty names.", call. = FALSE)
  }

  if (!is.logical(drop_unknown) || length(drop_unknown) != 1 || is.na(drop_unknown)) {
    stop("'drop_unknown' must be TRUE or FALSE.", call. = FALSE)
  }

  scope <- match.arg(scope)

  # Read the registry and build the trait-definition set for the requested
  # scope so the formatter follows the installed JSON exactly.
  registry <- read_trait_registry(registry_path = registry_path)
  trait_defs <- switch(
    scope,
    species = registry$species_traits,
    study = registry$study_traits,
    all = c(registry$species_traits, registry$study_traits)
  )

  trait_names <- vapply(trait_defs, function(x) x$coded_name, character(1))
  trait_types <- stats::setNames(
    vapply(trait_defs, function(x) x$data_type, character(1)),
    trait_names
  )
  trait_multi <- stats::setNames(
    vapply(trait_defs, function(x) isTRUE(x$multi_valued), logical(1)),
    trait_names
  )
  trait_allowed <- stats::setNames(
    lapply(trait_defs, function(x) x$allowed_values %||% NULL),
    trait_names
  )

  # Reject unknown fields unless the caller explicitly asked to drop them.
  entry_names <- names(entry)
  unknown_names <- setdiff(entry_names, trait_names)
  if (length(unknown_names) > 0 && !drop_unknown) {
    stop(
      sprintf(
        "Unknown registry field(s): %s",
        paste(unknown_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  entry <- entry[intersect(entry_names, trait_names)]

  # Build a typed missing-value template from the registry so the output always
  # contains the full scope-specific schema.
  out <- as.list(rep(NA_character_, length(trait_names)))
  names(out) <- trait_names
  for (nm in trait_names) {
    if (trait_types[[nm]] == "numeric") {
      out[[nm]] <- NA_real_
    } else if (trait_types[[nm]] == "binary") {
      out[[nm]] <- NA
    }
  }

  # Coerce each supplied field to the registry-defined type and validate
  # categorical values against the registry when allowed values are present.
  for (nm in names(entry)) {
    value <- entry[[nm]]

    if (length(value) == 0) {
      next
    }

    if (!isTRUE(trait_multi[[nm]]) && length(value) > 1) {
      stop(
        sprintf("Field '%s' accepts only one value.", nm),
        call. = FALSE
      )
    }

    if (trait_types[[nm]] == "numeric") {
      value <- suppressWarnings(as.numeric(value))
      value[!is.finite(value)] <- NA_real_
      out[[nm]] <- if (isTRUE(trait_multi[[nm]])) value else value[[1]]
      next
    }

    if (trait_types[[nm]] == "binary") {
      if (is.character(value)) {
        value_low <- stringr::str_to_lower(stringr::str_squish(value))
        value <- dplyr::case_when(
          value_low %in% c("1", "true", "yes", "y") ~ TRUE,
          value_low %in% c("0", "false", "no", "n") ~ FALSE,
          TRUE ~ NA
        )
      } else {
        value <- as.logical(value)
      }
      out[[nm]] <- if (isTRUE(trait_multi[[nm]])) value else value[[1]]
      next
    }

    value <- stringr::str_squish(as.character(value))
    value[!nzchar(value)] <- NA_character_

    allowed_values <- trait_allowed[[nm]]
    if (!is.null(allowed_values) && length(allowed_values) > 0) {
      bad_values <- setdiff(stats::na.omit(value), allowed_values)
      if (length(bad_values) > 0) {
        stop(
          sprintf(
            "Field '%s' has invalid value(s): %s",
            nm,
            paste(bad_values, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }

    if (isTRUE(trait_multi[[nm]])) {
      out[[nm]] <- paste(unique(stats::na.omit(value)), collapse = ";")
      if (!nzchar(out[[nm]])) {
        out[[nm]] <- NA_character_
      }
    } else {
      out[[nm]] <- value[[1]]
    }
  }

  tibble::as_tibble(out)
}

#' Enrich species traits from multiple database inputs
#'
#' Takes a named list of species-level database tables, standardizes missing
#' placeholders, collapses duplicate rows within each input table, and merges
#' the species traits across tables by an explicit database precedence order.
#'
#' @param db_list Named list of species-level data frames or tibbles.
#' @param precedence Character vector giving the database precedence order. When
#'   `NULL`, the order of `db_list` is used.
#' @param registry_path Optional path to a trait-registry JSON file. When
#'   `NULL`, the installed package registry is used.
#' @param cache_path Optional path to an `.rds` cache file. When supplied, the
#'   enriched species table is written to that path before returning.
#' @param missing_tokens Character vector of placeholder values that should be
#'   treated as missing in addition to `NA`.
#'
#' @return A tibble containing one row per species and the registry-aligned
#'   species trait columns merged by database precedence.
#'
#' @examples
#' \dontrun{
#' enrich_species_db(
#'   list(
#'     worms = fetch_worms(c("Clupea pallasii")),
#'     fishbase = fetch_fishbase(c("Clupea pallasii"))
#'   ),
#'   precedence = c("worms", "fishbase")
#' )
#' }
#'
#' @export
enrich_species_db <- function(db_list,
                              precedence = NULL,
                              registry_path = NULL,
                              cache_path = NULL,
                              missing_tokens = c("-9999")) {
  # Validate the database-list container and the optional precedence vector
  # before any registry or merge work begins.
  if (!is.list(db_list) || length(db_list) == 0) {
    stop("'db_list' must be a non-empty named list.", call. = FALSE)
  }

  if (is.null(names(db_list)) ||
      any(is.na(names(db_list))) ||
      any(!nzchar(names(db_list)))) {
    stop("'db_list' must be a named list.", call. = FALSE)
  }

  if (length(unique(names(db_list))) != length(db_list)) {
    stop("Database names in 'db_list' must be unique.", call. = FALSE)
  }

  if (is.null(precedence)) {
    precedence <- names(db_list)
  }

  if (!is.character(precedence) || any(is.na(precedence)) || any(!nzchar(precedence))) {
    stop("'precedence' must be a character vector of database names.", call. = FALSE)
  }

  if (!setequal(precedence, names(db_list))) {
    stop(
      "'precedence' must contain exactly the same database names as 'db_list'.",
      call. = FALSE
    )
  }

  if (!is.character(missing_tokens)) {
    stop("'missing_tokens' must be a character vector.", call. = FALSE)
  }

  if (!is.null(cache_path) &&
      (!is.character(cache_path) || length(cache_path) != 1)) {
    stop("'cache_path' must be NULL or a single file path.", call. = FALSE)
  }

  # Read the species registry once so the canonical species fields are driven by
  # the JSON schema rather than by the incoming tables.
  registry <- read_trait_registry(registry_path = registry_path)
  species_traits <- registry$species_traits
  registry_cols <- vapply(
    species_traits,
    function(x) x$coded_name,
    character(1)
  )
  trait_types <- stats::setNames(
    vapply(species_traits, function(x) x$data_type, character(1)),
    registry_cols
  )

  if (!all(c("genus", "species") %in% registry_cols)) {
    stop("The species registry must contain 'genus' and 'species'.", call. = FALSE)
  }

  # Preserve any additional already-standardized support fields that the source
  # ingestors emit, such as FishBase length-weight coefficients, so later
  # package steps can use them without hard-coding them into the registry.
  support_cols <- unique(unlist(lapply(db_list, function(dat) {
    if (!is.data.frame(dat)) {
      return(character(0))
    }
    setdiff(names(dat), registry_cols)
  }), use.names = FALSE))
  support_cols <- setdiff(support_cols, c("species_name_query"))
  support_types <- stats::setNames(rep("character", length(support_cols)), support_cols)
  for (nm in support_cols) {
    support_values <- unlist(lapply(db_list, function(dat) {
      if (!is.data.frame(dat) || !nm %in% names(dat)) {
        return(NULL)
      }
      dat[[nm]]
    }), use.names = FALSE)

    if (is.logical(support_values)) {
      support_types[[nm]] <- "binary"
      next
    }

    support_chr <- stringr::str_squish(as.character(support_values))
    support_chr[support_chr %in% missing_tokens] <- NA_character_
    support_chr[!nzchar(support_chr)] <- NA_character_
    numeric_try <- suppressWarnings(as.numeric(support_chr))
    numeric_idx <- !is.na(support_chr)
    if (any(numeric_idx) && all(!is.na(numeric_try[numeric_idx]))) {
      support_types[[nm]] <- "numeric"
    }
  }
  field_types <- c(trait_types, support_types)
  output_cols <- c(registry_cols, support_cols)

  # Build a typed missing-value template from the registry plus any support
  # fields so every merged row has a stable schema even when some databases do
  # not supply a value.
  template <- as.list(rep(NA_character_, length(output_cols)))
  names(template) <- output_cols
  for (nm in output_cols) {
    if (field_types[[nm]] == "numeric") {
      template[[nm]] <- NA_real_
    } else if (field_types[[nm]] == "binary") {
      template[[nm]] <- NA
    }
  }

  # Collapse each source database to one row per species key before combining
  # databases. Within a database, the first non-missing value in row order wins.
  collapsed_dbs <- vector("list", length(db_list))
  names(collapsed_dbs) <- names(db_list)

  for (db_name in names(db_list)) {
    dat <- db_list[[db_name]]

    if (!is.data.frame(dat)) {
      stop(
        sprintf("Database '%s' must be a data frame or tibble.", db_name),
        call. = FALSE
      )
    }

    dat <- tibble::as_tibble(dat)
    keep_cols <- intersect(names(dat), output_cols)
    dat <- dat[, keep_cols, drop = FALSE]

    if (!all(c("genus", "species") %in% names(dat))) {
      stop(
        sprintf("Database '%s' must contain 'genus' and 'species' columns.", db_name),
        call. = FALSE
      )
    }

    # Standardize every retained column to the registry-defined type and treat
    # explicit placeholder tokens as missing. Support fields outside the
    # registry are retained too; they are typed by simple value inspection so
    # numeric parameters like `lw_a_g` and `lw_b` remain numeric.
    for (nm in names(dat)) {
      if (field_types[[nm]] == "numeric") {
        value_chr <- stringr::str_squish(as.character(dat[[nm]]))
        value_chr[value_chr %in% missing_tokens] <- NA_character_
        value_num <- suppressWarnings(as.numeric(value_chr))
        value_num[!is.finite(value_num)] <- NA_real_
        dat[[nm]] <- value_num
      } else if (field_types[[nm]] == "binary") {
        value_chr <- stringr::str_to_lower(stringr::str_squish(as.character(dat[[nm]])))
        value_chr[value_chr %in% stringr::str_to_lower(missing_tokens)] <- NA_character_
        dat[[nm]] <- dplyr::case_when(
          value_chr %in% c("1", "true", "yes", "y") ~ TRUE,
          value_chr %in% c("0", "false", "no", "n") ~ FALSE,
          TRUE ~ NA
        )
      } else {
        value_chr <- stringr::str_squish(as.character(dat[[nm]]))
        value_chr[value_chr %in% missing_tokens] <- NA_character_
        value_chr[!nzchar(value_chr)] <- NA_character_
        dat[[nm]] <- value_chr
      }
    }

    dat <- dat |>
      dplyr::filter(!is.na(genus), !is.na(species)) |>
      dplyr::mutate(.species_key = paste(genus, species))

    species_keys <- unique(dat$.species_key)
    rows <- vector("list", length(species_keys))

    for (i in seq_along(species_keys)) {
      key <- species_keys[[i]]
      sub <- dat[dat$.species_key == key, , drop = FALSE]
      row <- template

      for (nm in intersect(names(sub), output_cols)) {
        values <- sub[[nm]]
        values <- values[!is.na(values)]
        if (length(values) == 0) {
          next
        }
        row[[nm]] <- values[[1]]
      }

      rows[[i]] <- tibble::as_tibble(row)
    }

    collapsed_dbs[[db_name]] <- dplyr::bind_rows(rows)
  }

  # Build the full species universe across all collapsed databases so the final
  # merge covers every species represented anywhere in the input list.
  all_keys <- unique(unlist(lapply(collapsed_dbs, function(dat) {
    if (is.null(dat) || nrow(dat) == 0) {
      return(character(0))
    }
    paste(dat$genus, dat$species)
  })))

  if (length(all_keys) == 0) {
    return(tibble::as_tibble(template)[0, , drop = FALSE])
  }

  # Merge species traits by precedence. For each field, the first non-missing
  # value found in the precedence order is retained.
  merged_rows <- vector("list", length(all_keys))

  for (i in seq_along(all_keys)) {
    key <- all_keys[[i]]
    row <- template

    for (db_name in precedence) {
      dat <- collapsed_dbs[[db_name]]
      if (is.null(dat) || nrow(dat) == 0) {
        next
      }

      hit <- dat[paste(dat$genus, dat$species) == key, , drop = FALSE]
      if (nrow(hit) == 0) {
        next
      }

      for (nm in output_cols) {
        if (!is.na(row[[nm]]) && !(is.character(row[[nm]]) && !nzchar(row[[nm]]))) {
          next
        }

        value <- hit[[nm]][[1]]
        if (is.na(value)) {
          next
        }
        if (is.character(value) && !nzchar(value)) {
          next
        }

        row[[nm]] <- value
      }
    }

    merged_rows[[i]] <- tibble::as_tibble(row)
  }

  out <- dplyr::bind_rows(merged_rows)

  # Backfill ocean-basin labels from the resolved FAO-area codes only when the
  # merged table still lacks an ocean-basin value for that species.
  if (all(c("fao_area", "ocean_basin") %in% names(out))) {
    missing_idx <- is.na(out$ocean_basin) | !nzchar(out$ocean_basin)

    if (any(missing_idx)) {
      fao_to_basin <- c(
        "1" = "Inland Waters",
        "2" = "Inland Waters",
        "3" = "Inland Waters",
        "4" = "Inland Waters",
        "5" = "Inland Waters",
        "6" = "Inland Waters",
        "7" = "Inland Waters",
        "8" = "Inland Waters",
        "18" = "Arctic Ocean",
        "21" = "Atlantic Ocean",
        "27" = "North Atlantic Ocean",
        "31" = "Atlantic Ocean",
        "34" = "Atlantic Ocean",
        "37" = "Mediterranean",
        "41" = "Atlantic Ocean",
        "47" = "Indian Ocean",
        "48" = "Southern Ocean",
        "51" = "Indian Ocean",
        "57" = "Indian Ocean",
        "58" = "Southern Ocean",
        "61" = "Pacific Ocean",
        "67" = "Pacific Ocean",
        "71" = "Pacific Ocean",
        "77" = "Pacific Ocean",
        "81" = "Pacific Ocean",
        "87" = "Pacific Ocean",
        "88" = "Southern Ocean"
      )

      parsed_basins <- vapply(
        out$fao_area[missing_idx],
        function(x) {
          codes <- stringr::str_split(
            stringr::str_replace_all(as.character(x), "[^0-9,;| ]", " "),
            "[,;| ]+"
          )[[1]]
          codes <- stringr::str_squish(codes)
          codes <- unique(codes[nzchar(codes)])
          basins <- unique(unname(fao_to_basin[codes]))
          basins <- basins[!is.na(basins) & nzchar(basins)]
          if (length(basins) == 0) {
            return(NA_character_)
          }
          paste(basins, collapse = ";")
        },
        character(1)
      )

      out$ocean_basin[missing_idx] <- parsed_basins
      out$ocean_basin[!nzchar(out$ocean_basin)] <- NA_character_
    }
  }

  # Recompute derived interval traits after the precedence merge so mixed-source
  # min/max pairs still produce the expected canonical midpoint and range
  # values.
  if (all(c("depth_min", "depth_max", "depth_range") %in% names(out))) {
    missing_idx <- is.na(out$depth_range) & !is.na(out$depth_min) & !is.na(out$depth_max)
    out$depth_range[missing_idx] <- out$depth_max[missing_idx] - out$depth_min[missing_idx]
  }

  if (all(c("depth_min", "depth_max", "depth_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$depth_midpoint) & !is.na(out$depth_min) & !is.na(out$depth_max)
    out$depth_midpoint[missing_idx] <- (out$depth_min[missing_idx] + out$depth_max[missing_idx]) / 2
  }

  if (all(c("temperature_min", "temperature_max", "temperature_range") %in% names(out))) {
    missing_idx <- is.na(out$temperature_range) & !is.na(out$temperature_min) & !is.na(out$temperature_max)
    out$temperature_range[missing_idx] <- out$temperature_max[missing_idx] - out$temperature_min[missing_idx]
  }

  if (all(c("temperature_min", "temperature_max", "temperature_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$temperature_midpoint) & !is.na(out$temperature_min) & !is.na(out$temperature_max)
    out$temperature_midpoint[missing_idx] <- (out$temperature_min[missing_idx] + out$temperature_max[missing_idx]) / 2
  }

  if (all(c("length_min", "length_max", "length_midpoint") %in% names(out))) {
    missing_idx <- is.na(out$length_midpoint) & !is.na(out$length_min) & !is.na(out$length_max)
    out$length_midpoint[missing_idx] <- (out$length_min[missing_idx] + out$length_max[missing_idx]) / 2
  }

  # Persist the enriched species table exactly as returned when the caller
  # explicitly supplies a cache path.
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cache_path)
  }

  out
}
