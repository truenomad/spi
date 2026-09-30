#' Calculate the SPI directly from preceding reporting
#'
#' @description
#' Calculates the surveillance performance index (SPI) for each district and
#' assessment year from earlier NPAFP counts and population estimates,
#' without fitting the INLA spatial and temporal model. A district's expected count is its NPAFP rate over all preceding
#' years multiplied by its current population. When a district has little reporting history, the calculation also uses
#' reporting from other districts in its region or the rest of the country.
#' Supplying boundaries allows neighbouring districts to be used first. The district observed-to-expected
#' ratio is then divided by the national observed-to-expected ratio for the
#' same year.
#'
#' Inspect the inputs and calculation for one district with [spi_direct_explain()].
#'
#' @details
#' **Steps for each assessment year.** Only years before the assessment year
#' enter the expectation.
#'
#' 1. Add up each district's preceding NPAFP cases and preceding population
#'    (child-years), and divide to get its historical rate.
#' 2. Stabilise the historical rate when the district has little
#'    reporting history (see below).
#' 3. Expected cases = current population x stabilised rate.
#' 4. Divide observed by expected cases.
#' 5. Divide that ratio by the national ratio (all observed over all
#'    expected cases in the year). The result is the SPI.
#'
#' **Stabilisation.** Each district's reference rate is the pooled preceding
#' rate of the other districts in its region (`region_col`), or of the rest
#' of the country when no region is given or the region has no preceding
#' case. When `boundaries` are supplied, the districts that share a boundary
#' with it come first. The stabilised rate adds one case's worth of the
#' reference rate to the district's history:
#'
#' `stabilised_rate = (history_cases + 1) / (history_pop + 1 / reference_rate)`
#'
#' This is arithmetic, with nothing estimated. It is the same as a weighted
#' average, `information_score * history_rate + (1 - information_score) *
#' reference_rate`, where `information_score = information / (information +
#' 1)` and `information` is the number of cases the district would have
#' reported over its preceding child-years at the reference rate. A district
#' whose history covers many child-years keeps a stabilised rate close to its
#' own rate, including when its record is persistently low. A district with a
#' short history moves further towards its reference rate, and a new district
#' takes the reference rate. A district with preceding child-years but no
#' preceding case is not given an expected rate of zero. `history_info`
#' describes `information_score` in words: substantial (0.9 or more), moderate
#' (0.5 to 0.9), limited (below 0.5) or none (no preceding year).
#'
#' **Population check.** Expected cases depend on the population, so each
#' district's population series is screened before calculation. Each
#' year-on-year change is compared with the median change across districts in
#' the same year. A change is unusual when that difference lies outside
#' Tukey's far-out fences (three interquartile ranges beyond the quartiles)
#' of all such differences and is at least 10%, the smallest difference
#' flagged, so that small differences between smooth population projections
#' are not flagged. `population_qc` is `"check_spike"` when an unusual change
#' is reversed by an unusual change in the other direction within two years,
#' `"check_change"` for any other unusual change, `"check_history"` when the
#' district is missing from some years (for example a new or split district),
#' and `"ok"` otherwise. Flags identify unusual patterns for review; they do
#' not imply that the population is incorrect, and supplied populations are
#' never altered.
#'
#' @param data Data frame with one row per district and year: the district
#'   identifier (`id_col`), the year (`year_col`), the number of NPAFP cases
#'   (`count_col`) and the population under 15 (`pop_col`). A table
#'   with columns `district`, `year`, `npafp_cases` and `population_u15`, and
#'   optionally `province`, needs no other argument.
#' @param population Optional data frame with one row per district and year,
#'   for when the population is held in a separate table: the district
#'   identifier, the year and the population under 15 (`pop_col`). It
#'   then defines which districts and years are assessed; a district-year
#'   with no row in `data` is counted as zero cases. Default `NULL`: the
#'   population is taken from `data`.
#' @param region_col Character or `NULL`. Column in `data` (or in
#'   `population`) naming each district's region, for example a province.
#'   The region's preceding rate is the reference for a district with little
#'   history. Default `"province"`, used when the data have that column;
#'   without it, or with `NULL`, the rest of the country is the reference.
#' @param boundaries Optional `sf` polygon layer of the districts, with the
#'   district identifier. When given, the districts that share a boundary
#'   are used as the reference before the region. Default `NULL`.
#' @param first_assessment Integer. First assessment year. Default: the
#'   first year that has `min_history` preceding years.
#' @param last_assessment Integer. Last assessment year. Default: the last
#'   year in the data.
#' @param min_history Integer. Preceding years required before the first
#'   assessment year when `first_assessment` is not given. Default 3.
#' @param id_col,year_col,count_col,pop_col Character. Column names in
#'   `data`, `population` and `boundaries`. Defaults `"district"`, `"year"`,
#'   `"npafp_cases"` and `"population_u15"`.
#' @param stabilise Logical. Stabilise limited histories. Default `TRUE`.
#'   `FALSE` uses each district's historical rate unchanged; a district with
#'   no preceding case then has no expected count. Intended for research and
#'   replication.
#' @param per Numeric. Rates are reported per this many people. Default
#'   100000.
#' @param verbose Logical. Print progress and a summary. Default `TRUE`.
#'
#' @return An `spi_direct` object, a list with:
#'   * `summary`: one row per district and assessment year with `observed`,
#'     `pop`, `npafp_rate`, `history_years`, `history_cases`, `history_pop`
#'     (preceding child-years), `history_rate`, `reference_rate`,
#'     `reference_source`, `information_score`, `history_info`,
#'     `stabilised_rate`, `expected`, `oe`, `national_oe`, `spi` and
#'     `population_qc`. Rates are per `per`.
#'   * `national`: observed, expected and the national observed-to-expected
#'     ratio for each assessment year.
#'   * `population_qc`: the population check for each district.
#'   * `stabilisation`: for each assessment year, the number of districts
#'     with history.
#'   * `metadata`: the arguments and data range used.
#'
#'   `print()` gives a short country summary, `summary()` a fuller report and
#'   `as_tibble()` the `summary` table.
#'
#' @seealso [spi_direct_explain()], `vignette("spi-direct")`, and
#'   [spi_index()] for the model-based SPI.
#'
#' @examplesIf requireNamespace("sf", quietly = TRUE)
#' # annual NPAFP counts with each district's province
#' provinces <- sf::st_drop_geometry(synth_surveillance$boundaries)
#' annual <- synth_surveillance$cases |>
#'   dplyr::mutate(year = as.integer(format(month, "%Y"))) |>
#'   dplyr::summarise(count = sum(count), .by = c(adm2_guid, year)) |>
#'   dplyr::left_join(provinces[c("adm2_guid", "adm1_name")],
#'                    by = "adm2_guid")
#'
#' res <- spi_direct(
#'   data = annual,
#'   population = synth_surveillance$population,
#'   id_col = "adm2_guid",
#'   count_col = "count",
#'   pop_col = "pop_u15",
#'   region_col = "adm1_name",
#'   verbose = FALSE
#' )
#' res
#' @export
spi_direct <- function(
  data,
  population = NULL,
  id_col = "district",
  year_col = "year",
  count_col = "npafp_cases",
  pop_col = "population_u15",
  region_col = "province",
  boundaries = NULL,
  first_assessment = NULL,
  last_assessment = NULL,
  min_history = 3L,
  stabilise = TRUE,
  per = 100000,
  verbose = TRUE
) {
  # the default region column is used only when the data have it
  if (missing(region_col) &&
        !region_col %in% c(names(data), names(population))) {
    region_col <- NULL
  }
  .direct_check_args(
    data, population, boundaries, id_col, year_col, count_col,
    pop_col, region_col, stabilise, per, verbose
  )

  # --- annual panel -----------------------------------------------------
  if (verbose) cli::cli_alert_info("Reading district-year counts...")
  annual <- .direct_annual(
    data, population, id_col, year_col, count_col, pop_col, verbose
  )
  years_all <- sort(unique(annual$year))
  if (verbose) {
    cli::cli_alert_success(
      "{.val {length(unique(annual$district_id))}} districts, \\
       {.val {length(years_all)}} years ({min(years_all)}-{max(years_all)})."
    )
  }

  # --- assessment years -------------------------------------------------
  if (is.null(first_assessment)) {
    first_assessment <- min(years_all) + as.integer(min_history)
  }
  if (is.null(last_assessment)) last_assessment <- max(years_all)
  targets <- years_all[years_all >= first_assessment &
                         years_all <= last_assessment]
  if (length(targets) == 0L) {
    cli::cli_abort(c(
      "No assessment year falls between {first_assessment} and \\
       {last_assessment}.",
      "i" = "The data cover {min(years_all)}-{max(years_all)}."
    ))
  }
  if (any(targets <= min(years_all))) {
    cli::cli_abort(c(
      "The first assessment year must follow at least one year of data.",
      "i" = "The data start in {min(years_all)}."
    ))
  }

  # --- population check -------------------------------------------------
  if (verbose) cli::cli_alert_info("Checking population series...")
  pop_qc <- .direct_population_qc(annual, targets)
  if (verbose) {
    n_flag <- sum(pop_qc$by_district$population_qc != "ok")
    if (n_flag > 0) {
      cli::cli_alert_warning(
        "{.val {n_flag}} district{?s} with a population pattern to check \\
         (see {.field population_qc})."
      )
    } else {
      cli::cli_alert_success("No unusual population pattern found.")
    }
  }

  # --- reference areas --------------------------------------------------
  geo <- .direct_geography(data, population, boundaries, id_col, year_col,
                           region_col, unique(annual$district_id), verbose)

  # --- one assessment year at a time ------------------------------------
  if (verbose) {
    cli::cli_alert_info(
      "Calculating expected cases for {.val {length(targets)}} \\
       assessment year{?s} ({min(targets)}-{max(targets)})..."
    )
  }
  per_year <- lapply(targets, function(t) {
    .direct_one_year(annual, geo, t, stabilise)
  })
  summary_tbl <- dplyr::bind_rows(lapply(per_year, `[[`, "rows"))
  stab_tbl <- dplyr::bind_rows(lapply(per_year, `[[`, "stab"))

  # --- national centring ------------------------------------------------
  national <- .national_oe(summary_tbl, "observed", "expected", "year") |>
    dplyr::select("year", "national_observed", "national_expected",
                  "national_oe")
  summary_tbl <- summary_tbl |>
    dplyr::left_join(national |> dplyr::select("year", "national_oe"),
                     by = "year") |>
    dplyr::mutate(
      national_oe = dplyr::if_else(.data$national_oe > 0,
                                   .data$national_oe, NA_real_),
      spi = .data$oe / .data$national_oe
    ) |>
    dplyr::left_join(pop_qc$by_year, by = c("district_id", "year")) |>
    dplyr::mutate(dplyr::across(
      c("npafp_rate", "history_rate", "reference_rate", "stabilised_rate"),
      function(x) x * per
    )) |>
    dplyr::select(
      "district_id", "year", "observed", "pop", "npafp_rate",
      "history_years", "history_cases", "history_pop", "history_rate",
      "reference_rate", "reference_source", "information_score",
      "history_info", "stabilised_rate", "expected", "oe", "national_oe",
      "spi", "population_qc"
    ) |>
    dplyr::arrange(.data$year, .data$district_id)
  if (verbose) {
    cli::cli_alert_success(
      "SPI calculated for {.val {sum(is.finite(summary_tbl$spi))}} \\
       district-years."
    )
  }

  names(summary_tbl)[names(summary_tbl) == "district_id"] <- id_col
  pop_by_district <- pop_qc$by_district
  names(pop_by_district)[names(pop_by_district) == "district_id"] <- id_col

  out <- structure(
    list(
      summary = summary_tbl,
      national = national,
      population_qc = pop_by_district,
      stabilisation = stab_tbl,
      metadata = list(
        first_assessment = min(targets), last_assessment = max(targets),
        years_in_data = range(years_all), stabilise = stabilise,
        id_col = id_col, year_col = year_col, count_col = count_col,
        pop_col = pop_col,
        population_source = if (is.null(population)) "data" else
          "population",
        region_col = region_col, neighbours = !is.null(boundaries),
        per = per,
        n_districts = length(unique(annual$district_id))
      )
    ),
    class = "spi_direct"
  )

  if (verbose) .direct_report(out)
  out
}

# --- argument checks ----------------------------------------------------

#' @noRd
.direct_check_args <- function(data, population, boundaries, id_col,
                               year_col, count_col, pop_col,
                               region_col, stabilise, per, verbose) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (!is.null(population) && !is.data.frame(population)) {
    cli::cli_abort("{.arg population} must be a data frame or NULL.")
  }
  if (!is.null(boundaries) && !inherits(boundaries, "sf")) {
    cli::cli_abort(c(
      "{.arg boundaries} must be an {.cls sf} polygon layer or NULL.",
      "x" = "Got {.cls {class(boundaries)}}."
    ))
  }
  cols <- list(id_col = id_col, year_col = year_col, count_col = count_col,
               pop_col = pop_col)
  for (nm in names(cols)) {
    if (!rlang::is_string(cols[[nm]])) {
      cli::cli_abort("{.arg {nm}} must be a single string.")
    }
  }
  if (!is.null(region_col)) {
    if (!rlang::is_string(region_col)) {
      cli::cli_abort("{.arg region_col} must be a single string or NULL.")
    }
    in_data <- region_col %in% names(data) ||
      (!is.null(population) && region_col %in% names(population))
    if (!in_data) {
      cli::cli_abort(
        "Neither {.arg data} nor {.arg population} has a \\
         {.field {region_col}} column ({.arg region_col})."
      )
    }
  }
  if (!rlang::is_bool(stabilise) || !rlang::is_bool(verbose)) {
    cli::cli_abort(
      "{.arg stabilise} and {.arg verbose} must be TRUE or FALSE."
    )
  }
  if (!is.numeric(per) || length(per) != 1L || !is.finite(per) || per <= 0) {
    cli::cli_abort("{.arg per} must be a single positive number.")
  }
  invisible(TRUE)
}

# --- annual panel -------------------------------------------------------

# Population defines the district-years; a district-year with no case row
# counts as zero cases, and one with no population is dropped.
#' @noRd
.direct_annual <- function(data, population, id_col, year_col, count_col,
                           pop_col, verbose) {
  from_data <- is.null(population)
  if (from_data) {
    if (!pop_col %in% names(data)) {
      cli::cli_abort(c(
        "{.arg data} has no {.field {pop_col}} column.",
        "i" = "Name the population column with {.arg pop_col}, or \\
               supply a separate {.arg population} table."
      ))
    }
    population <- data
  }

  need <- list(
    data = c(id_col, year_col, count_col),
    population = c(id_col, year_col, pop_col)
  )
  inputs <- list(data = data, population = population)
  for (nm in names(need)) {
    miss <- setdiff(need[[nm]], names(inputs[[nm]]))
    if (length(miss) > 0) {
      cli::cli_abort(c(
        "{.arg {nm}} is missing column{?s} {.field {miss}}.",
        "i" = "Map your column names with {.arg id_col}, {.arg year_col}, \\
               {.arg count_col} and {.arg pop_col}."
      ))
    }
  }

  cs <- tibble::tibble(
    district_id = as.character(data[[id_col]]),
    year = data[[year_col]],
    observed = data[[count_col]]
  )
  ps <- tibble::tibble(
    district_id = as.character(population[[id_col]]),
    year = population[[year_col]],
    pop = population[[pop_col]]
  )
  checks <- if (from_data) list(list(cs, "data")) else
    list(list(cs, "data"), list(ps, "population"))
  for (tb in checks) {
    yr <- tb[[1]]$year
    if (!is.numeric(yr) || any(yr != floor(yr), na.rm = TRUE)) {
      cli::cli_abort(
        "{.field {year_col}} in {.arg {tb[[2]]}} must hold whole years."
      )
    }
    if (anyNA(tb[[1]]$district_id) || anyNA(yr)) {
      cli::cli_abort(
        "{.arg {tb[[2]]}} has missing district identifiers or years."
      )
    }
    if (anyDuplicated(tb[[1]][c("district_id", "year")]) > 0) {
      cli::cli_abort(c(
        "{.arg {tb[[2]]}} has duplicate district-year rows.",
        "i" = "Provide one row per district and year."
      ))
    }
  }
  cs$year <- as.integer(cs$year)
  ps$year <- as.integer(ps$year)

  if (!is.numeric(cs$observed)) {
    cli::cli_abort("{.field {count_col}} must be numeric.")
  }
  known <- cs$observed[!is.na(cs$observed)]
  if (any(known < 0 | known != floor(known))) {
    cli::cli_abort(
      "{.field {count_col}} must hold non-negative whole numbers."
    )
  }
  if (!is.numeric(ps$pop)) {
    cli::cli_abort("{.field {pop_col}} must be numeric.")
  }
  if (any(ps$pop < 0, na.rm = TRUE)) {
    cli::cli_abort("{.field {pop_col}} has negative values.")
  }

  bad_pop <- is.na(ps$pop) | ps$pop == 0
  if (any(bad_pop)) {
    cli::cli_warn(
      "{sum(bad_pop)} district-year{?s} with a missing or zero population \\
       {?was/were} dropped."
    )
    ps <- ps[!bad_pop, ]
  }
  no_pop <- dplyr::anti_join(cs, ps, by = c("district_id", "year"))
  if (nrow(no_pop) > 0 && !from_data) {
    cli::cli_warn(
      "{nrow(no_pop)} district-year{?s} with cases but no population \\
       {?was/were} dropped."
    )
  }
  annual <- dplyr::left_join(ps, cs, by = c("district_id", "year"))
  n_missing_count <- sum(is.na(annual$observed))
  if (n_missing_count > 0 && verbose) {
    cli::cli_alert_info(
      "{.val {n_missing_count}} district-year{?s} with no case count \\
       {?was/were} counted as zero cases."
    )
  }
  annual$observed <- dplyr::coalesce(annual$observed, 0)
  annual |> dplyr::select("district_id", "year", "observed", "pop")
}

# --- geography ----------------------------------------------------------

# Each district's region from the data (its most recent value), and
# neighbour pairs when boundaries are supplied.
#' @noRd
.direct_geography <- function(data, population, boundaries, id_col,
                              year_col, region_col, ids, verbose) {
  region <- NULL
  if (!is.null(region_col)) {
    src <- if (region_col %in% names(data)) data else population
    region <- tibble::tibble(
      district_id = as.character(src[[id_col]]),
      year = src[[year_col]],
      region = as.character(src[[region_col]])
    ) |>
      dplyr::filter(!is.na(.data$region))
    n_multi <- region |>
      dplyr::distinct(.data$district_id, .data$region) |>
      dplyr::count(.data$district_id) |>
      dplyr::filter(.data$n > 1L) |>
      nrow()
    if (n_multi > 0 && verbose) {
      cli::cli_alert_warning(
        "{.val {n_multi}} district{?s} changed region over time; the most \\
         recent region is used."
      )
    }
    region <- region |>
      dplyr::slice_max(.data$year, n = 1L, with_ties = FALSE,
                       by = "district_id") |>
      dplyr::select("district_id", "region")
  }

  edges <- NULL
  if (!is.null(boundaries)) {
    if (!id_col %in% names(boundaries)) {
      cli::cli_abort("{.arg boundaries} has no {.field {id_col}} column.")
    }
    nb <- suppressWarnings(suppressMessages(
      spi_adjacency(boundaries = boundaries, id_col = id_col)
    ))
    nb_ids <- as.character(attr(nb, "region.id"))
    idx <- unlist(nb)
    edges <- tibble::tibble(
      district_id = rep(nb_ids, lengths(nb)),
      neighbour = nb_ids[pmax(idx, 1L)]
    )
    edges <- edges[idx > 0, ]
    missing <- setdiff(ids, nb_ids)
    if (length(missing) > 0 && verbose) {
      cli::cli_alert_warning(
        "{.val {length(missing)}} district{?s} in the data {?is/are} not in \\
         {.arg boundaries}; {?it/they} will use the region or country rate."
      )
    }
  }
  if (verbose) {
    chain <- c(if (!is.null(edges)) "neighbouring districts",
               if (!is.null(region)) "region", "country")
    cli::cli_alert_info(
      "Reference for limited histories: {paste(chain, collapse = ', then ')}."
    )
  }
  list(edges = edges, region = region)
}

# --- one assessment year ------------------------------------------------

#' @noRd
.direct_one_year <- function(annual, geo, target, stabilise) {
  current <- annual[annual$year == target, ]
  hist <- annual[annual$year < target, ] |>
    dplyr::summarise(
      history_cases = sum(.data$observed),
      history_pop = sum(.data$pop),
      history_years = dplyr::n(),
      .by = "district_id"
    )

  # every district with history or a current count gets a reference rate
  tbl <- dplyr::full_join(
    current |> dplyr::select("district_id", "observed", "pop"),
    hist, by = "district_id"
  ) |>
    dplyr::mutate(
      history_cases = dplyr::coalesce(.data$history_cases, 0),
      history_pop = dplyr::coalesce(.data$history_pop, 0),
      history_years = dplyr::coalesce(.data$history_years, 0L)
    )

  if (!is.null(geo$edges)) {
    nbr <- geo$edges |>
      dplyr::inner_join(
        hist |> dplyr::select(neighbour = "district_id", "history_cases",
                              "history_pop"),
        by = "neighbour"
      ) |>
      dplyr::summarise(
        nbr_cases = sum(.data$history_cases),
        nbr_pop = sum(.data$history_pop),
        .by = "district_id"
      )
    tbl <- dplyr::left_join(tbl, nbr, by = "district_id")
  } else {
    tbl$nbr_cases <- NA_real_
    tbl$nbr_pop <- NA_real_
  }

  others_rate <- function(cases, pop) {
    oc <- sum(cases) - cases
    op <- sum(pop) - pop
    dplyr::if_else(op > 0 & oc > 0, oc / op, NA_real_)
  }
  if (!is.null(geo$region)) {
    tbl <- tbl |>
      dplyr::left_join(geo$region, by = "district_id") |>
      dplyr::mutate(
        region_rate = others_rate(.data$history_cases, .data$history_pop),
        .by = "region"
      )
  } else {
    tbl$region_rate <- NA_real_
  }
  tbl <- tbl |>
    dplyr::mutate(
      nbr_rate = dplyr::if_else(
        dplyr::coalesce(.data$nbr_cases, 0) > 0 &
          dplyr::coalesce(.data$nbr_pop, 0) > 0,
        .data$nbr_cases / .data$nbr_pop, NA_real_
      ),
      country_rate = others_rate(.data$history_cases, .data$history_pop),
      reference_rate = dplyr::coalesce(.data$nbr_rate, .data$region_rate,
                                       .data$country_rate),
      reference_source = dplyr::case_when(
        !is.na(.data$nbr_rate) ~ "neighbours",
        !is.na(.data$region_rate) ~ "region",
        !is.na(.data$country_rate) ~ "country",
        TRUE ~ NA_character_
      ),
      history_rate = dplyr::if_else(
        .data$history_pop > 0, .data$history_cases / .data$history_pop,
        NA_real_
      ),
      information = dplyr::coalesce(
        .data$history_pop * .data$reference_rate, 0
      )
    )

  if (stabilise) {
    # one case's worth of the reference rate added to the history; the same
    # as weighting the two rates by information / (information + 1)
    tbl <- tbl |>
      dplyr::mutate(
        information_score = .data$information /
          (.data$information + .DIRECT_BORROWED_CASES),
        stabilised_rate = dplyr::if_else(
          is.finite(.data$reference_rate) & .data$reference_rate > 0,
          (.data$history_cases + .DIRECT_BORROWED_CASES) /
            (.data$history_pop +
               .DIRECT_BORROWED_CASES / .data$reference_rate),
          .data$history_rate
        )
      )
  } else {
    tbl <- tbl |>
      dplyr::mutate(
        stabilised_rate = dplyr::if_else(.data$history_cases > 0,
                                         .data$history_rate, NA_real_),
        information_score = NA_real_
      )
  }
  stab <- tibble::tibble(
    year = target, districts_with_history = sum(hist$history_pop > 0)
  )

  rows <- tbl |>
    dplyr::filter(!is.na(.data$observed)) |>
    dplyr::mutate(
      year = target,
      npafp_rate = .data$observed / .data$pop,
      expected = .data$pop * .data$stabilised_rate,
      oe = dplyr::if_else(.data$expected > 0,
                          .data$observed / .data$expected, NA_real_),
      history_info = .direct_history_info(
        .data$history_years, .data$information_score, stabilise
      )
    )
  list(rows = rows, stab = stab)
}

# Plain label for how much information the district's own history holds;
# the cut-offs describe the history and do not enter the SPI.
#' @noRd
.direct_history_info <- function(years, score, stabilise) {
  if (!stabilise) {
    return(dplyr::if_else(years > 0, "own history only", "none"))
  }
  dplyr::case_when(
    years == 0 ~ "none",
    score >= 0.9 ~ "substantial",
    score >= 0.5 ~ "moderate",
    TRUE ~ "limited"
  )
}

# --- stabilisation constant ---------------------------------------------

# cases' worth of the reference rate added to each district's history
#' @noRd
.DIRECT_BORROWED_CASES <- 1

# --- population check ---------------------------------------------------

# Unusual changes are log year-on-year changes beyond Tukey's far-out fences
# of all districts' changes; the flags are for review, not correction.
#' @noRd
.direct_population_qc <- function(annual, targets, min_change = 0.10) {
  sorted <- annual[order(annual$district_id, annual$year), ]
  all_years <- sort(unique(sorted$year))
  # each change is compared with the median change across districts in the
  # same year, so a country-wide rebasing is not flagged
  changes <- sorted |>
    dplyr::mutate(
      log_change = log(.data$pop / dplyr::lag(.data$pop)),
      consecutive = .data$year - dplyr::lag(.data$year) == 1L,
      .by = "district_id"
    ) |>
    dplyr::filter(.data$consecutive %in% TRUE) |>
    dplyr::mutate(
      deviation = .data$log_change - stats::median(.data$log_change),
      .by = "year"
    )
  year_median <- changes |>
    dplyr::summarise(med = stats::median(.data$log_change), .by = "year")
  fence <- if (nrow(changes) >= 4L) {
    q <- stats::quantile(changes$deviation, c(0.25, 0.75), names = FALSE)
    c(q[1] - 3 * diff(q), q[2] + 3 * diff(q))
  } else {
    c(-Inf, Inf)
  }
  fence <- c(min(fence[1], -log(1 + min_change)),
             max(fence[2], log(1 + min_change)))

  screen <- function(yr, pop, upto) {
    keep <- yr <= upto
    yr <- yr[keep]
    pop <- pop[keep]
    span <- all_years[all_years <= upto]
    incomplete <- length(yr) < length(span)
    lc <- if (length(pop) > 1L) diff(log(pop)) else numeric(0)
    lc[diff(yr) != 1L] <- NA_real_
    med <- year_median$med[match(yr[-1], year_median$year)]
    dev <- lc - dplyr::coalesce(med, 0)
    unusual <- !is.na(dev) & (dev < fence[1] | dev > fence[2])
    spike <- FALSE
    for (i in which(unusual)) {
      later <- seq.int(i + 1L, length.out = 2L)
      later <- later[later <= length(dev)]
      if (any(unusual[later] & sign(dev[later]) != sign(dev[i]))) {
        spike <- TRUE
      }
    }
    status <- if (spike) {
      "check_spike"
    } else if (any(unusual)) {
      "check_change"
    } else if (incomplete) {
      "check_history"
    } else {
      "ok"
    }
    biggest <- if (any(!is.na(lc))) {
      exp(lc[which.max(abs(lc))]) - 1
    } else {
      NA_real_
    }
    list(status = status, n_unusual = sum(unusual), largest = biggest,
         years = length(yr))
  }

  groups <- split(sorted, sorted$district_id)
  by_year <- dplyr::bind_rows(lapply(targets, function(t) {
    tibble::tibble(
      district_id = names(groups),
      year = t,
      population_qc = unname(vapply(
        groups, function(g) screen(g$year, g$pop, t)$status, ""
      ))
    )
  }))
  last <- max(targets)
  by_district <- dplyr::bind_rows(lapply(names(groups), function(id) {
    g <- groups[[id]]
    s <- screen(g$year, g$pop, last)
    tibble::tibble(
      district_id = id, population_qc = s$status,
      unusual_changes = s$n_unusual, largest_change = s$largest,
      years = s$years
    )
  }))
  list(by_year = by_year, by_district = by_district)
}

# --- reports ------------------------------------------------------------

#' @noRd
.direct_report <- function(x) {
  p <- x$metadata
  sm <- x$summary
  per_lab <- format(p$per, big.mark = " ", scientific = FALSE)

  cli::cli_rule()
  cli::cli_h2("Direct SPI summary")

  cli::cli_h3("Formulas used")
  cli::cli_text(
    "history_rate = preceding cases / preceding population x {per_lab}"
  )
  if (p$stabilise) {
    cli::cli_text(
      "stabilised_rate = (preceding cases + 1) / (preceding population + \\
       1 / reference_rate) x {per_lab}"
    )
  } else {
    cli::cli_text("stabilised_rate = history_rate (no stabilisation)")
  }
  cli::cli_text("expected = pop x stabilised_rate / {per_lab}")
  cli::cli_text("oe = observed / expected")
  cli::cli_text("spi = oe / national_oe")
  cli::cli_rule()

  latest <- max(sm$year)
  sl <- sm[sm$year == latest, ]
  nat <- x$national[x$national$year == latest, ]
  nat_oe <- format(round(nat$national_oe, 2), nsmall = 2)
  expected_total <- format(round(sum(sl$expected, na.rm = TRUE), 1),
                           big.mark = " ")
  cli::cli_h3("Data overview ({latest})")
  cli::cli_text("Districts assessed: {nrow(sl)}")
  cli::cli_text(
    "NPAFP cases: {format(sum(sl$observed), big.mark = ' ')}; \\
     expected: {expected_total}"
  )
  cli::cli_text("National observed/expected: {nat_oe}")
  cli::cli_text(
    "Districts with SPI below 1: {sum(sl$spi < 1, na.rm = TRUE)} of \\
     {sum(is.finite(sl$spi))}"
  )
  if (p$stabilise) {
    info <- table(factor(sl$history_info,
                         levels = c("substantial", "moderate", "limited",
                                    "none")))
    cli::cli_text(
      "Historical information: {info[['substantial']]} substantial, \\
       {info[['moderate']]} moderate, {info[['limited']]} limited, \\
       {info[['none']]} none"
    )
  }
  cli::cli_text(
    "Population flags: {sum(sl$population_qc != 'ok', na.rm = TRUE)}"
  )
  n_missing <- sum(!is.finite(sl$spi))
  if (n_missing > 0) {
    cli::cli_alert_warning("{n_missing} district{?s} without an SPI.")
  }
  cli::cli_rule()

  cli::cli_h3("Results by year")
  by_year <- sm |>
    dplyr::summarise(
      districts = dplyr::n(),
      observed = sum(.data$observed),
      expected = round(sum(.data$expected, na.rm = TRUE), 1),
      median_spi = round(stats::median(.data$spi, na.rm = TRUE), 2),
      below_1 = sum(.data$spi < 1, na.rm = TRUE),
      .by = "year"
    ) |>
    dplyr::left_join(
      x$national |>
        dplyr::transmute(.data$year,
                         national_oe = round(.data$national_oe, 2)),
      by = "year"
    )
  print(by_year, n = Inf)
  cli::cli_rule()
  invisible(x)
}

# --- methods ------------------------------------------------------------

#' @export
print.spi_direct <- function(x, ...) {
  p <- x$metadata
  sm <- x$summary
  latest <- p$last_assessment
  sl <- sm[sm$year == latest & is.finite(sm$spi), ]
  n <- nrow(sl)
  band <- function(cut) {
    k <- sum(sl$spi < cut)
    sprintf("%d (%d%%)", k, if (n > 0) round(100 * k / n) else 0L)
  }
  lines <- c(
    sprintf("Assessment years:   %d-%d", p$first_assessment, latest),
    sprintf("Districts:          %d", p$n_districts),
    sprintf("Latest assessment:  %d", latest),
    sprintf("SPI < 1:            %s", band(1)),
    sprintf("SPI < 0.8:          %s", band(0.8)),
    sprintf("SPI < 0.6:          %s", band(0.6)),
    sprintf("Population flags:   %d",
            sum(x$population_qc$population_qc != "ok")),
    sprintf("Limited histories:  %d",
            sum(sm$history_info[sm$year == latest] %in%
                  c("limited", "none")))
  )
  cli::cli_h2(
    if (p$stabilise) "Direct SPI" else "Direct SPI (no stabilisation)"
  )
  cli::cli_verbatim(lines)
  cli::cli_text(cli::col_grey(
    "Use summary() for the full report, as_tibble() for the table and \\
     spi_direct_explain() for one district."
  ))
  invisible(x)
}

#' @export
summary.spi_direct <- function(object, ...) {
  .direct_report(object)
}

#' @export
as_tibble.spi_direct <- function(x, ...) {
  x$summary
}

#' Show how one district's direct SPI was calculated
#'
#' @description
#' Lists every component of one district's direct SPI for one year, in the
#' order of the calculation: current reporting, previous reporting, the
#' expected count, the SPI and the data checks. Each value can be checked by
#' hand from the ones above it.
#'
#' @param x An `spi_direct` object from [spi_direct()].
#' @param district The district identifier.
#' @param year Assessment year. Default: the last assessment year.
#' @param language Character. Language of the labels, as a two-letter code
#'   such as `"fr"`. Default `"en"`. Other languages are translated with
#'   `sntutils::translate_text_vec()`, which calls Google Translate through
#'   the gtranslate package. Both packages and an internet connection are
#'   needed; labels that cannot be translated stay in English. Numbers and `check_` flags are not
#'   translated.
#' @param print Logical. Print the breakdown. Default `TRUE`.
#'
#' @return Invisibly, a tibble with `section`, `component` and `value`, in
#'   the language requested.
#' @seealso [spi_direct()]
#' @examples
#' annual <- synth_surveillance$cases |>
#'   dplyr::mutate(year = as.integer(format(month, "%Y"))) |>
#'   dplyr::summarise(count = sum(count), .by = c(adm2_guid, year))
#' res <- spi_direct(
#'   data = annual,
#'   population = synth_surveillance$population,
#'   id_col = "adm2_guid",
#'   count_col = "count",
#'   pop_col = "pop_u15",
#'   verbose = FALSE
#' )
#' spi_direct_explain(res, res$summary$adm2_guid[1])
#' @export
spi_direct_explain <- function(x, district, year = NULL, language = "en",
                               print = TRUE) {
  if (!inherits(x, "spi_direct")) {
    cli::cli_abort("{.arg x} must be an {.cls spi_direct} object.")
  }
  if (!rlang::is_string(language)) {
    cli::cli_abort("{.arg language} must be a single string such as \"fr\".")
  }
  id_col <- x$metadata$id_col
  if (is.null(year)) year <- x$metadata$last_assessment
  r <- x$summary[x$summary[[id_col]] == district &
                   x$summary$year == year, ]
  if (nrow(r) != 1L) {
    cli::cli_abort("No row for district {.val {district}} in {.val {year}}.")
  }
  num <- function(v, d = 2) {
    if (is.na(v)) "NA" else formatC(v, format = "f", digits = d,
                                    big.mark = " ")
  }
  qc <- if (identical(r$population_qc, "ok")) "no flag" else r$population_qc

  out <- tibble::tibble(
    section = rep(
      c("Current reporting", "Previous reporting", "Expected reporting",
        "SPI", "Data checks"),
      c(3L, 4L, 5L, 3L, 1L)
    ),
    component = c(
      "Observed NPAFP cases", "Current population under 15", "NPAFP rate",
      "Previous years", "Previous NPAFP cases", "Previous child-years",
      "Previous rate",
      "Reference source", "Reference rate", "Historical information",
      "Rate used for expectation", "Expected NPAFP cases",
      "District observed / expected", "National observed / expected", "SPI",
      "Population check"
    ),
    value = c(
      num(r$observed, 0), num(r$pop, 0), num(r$npafp_rate),
      as.character(r$history_years), num(r$history_cases, 0),
      num(r$history_pop, 0), num(r$history_rate),
      dplyr::coalesce(r$reference_source, "none"), num(r$reference_rate),
      r$history_info, num(r$stabilised_rate), num(r$expected),
      num(r$oe), num(r$national_oe), num(r$spi),
      qc
    )
  )

  # labels and worded values are translated; numbers and flags are not
  worded <- !grepl("^check_|^[0-9 .]+$|^NA$", out$value)
  out$section <- .direct_translate(out$section, language)
  out$component <- .direct_translate(out$component, language)
  out$value[worded] <- .direct_translate(out$value[worded], language)
  district_label <- .direct_translate("District", language)

  if (print) {
    width <- max(nchar(out$component)) + 2L
    val_width <- max(nchar(out$value))
    cli::cli_h1("{district_label} {district}, {year}")
    for (s in unique(out$section)) {
      rows <- out[out$section == s, ]
      cli::cli_h2("{s}")
      cli::cli_verbatim(
        paste0(formatC(rows$component, width = -width),
               formatC(rows$value, width = val_width))
      )
    }
  }
  invisible(out)
}

# English labels translated with sntutils; returned unchanged for "en"
#' @noRd
.direct_translate <- function(text, language) {
  if (identical(tolower(language), "en") || length(text) == 0L) {
    return(text)
  }
  .check_pkg(
    c("sntutils", "gtranslate"), reason = "to translate the explanation"
  )
  unique_text <- unique(text)
  # gtranslate's request prints an encoding note for every label
  translated <- withCallingHandlers(
    sntutils::translate_text_vec(
      unique_text,
      target_language = tolower(language),
      source_language = "en"
    ),
    message = function(m) {
      if (grepl("No encoding supplied", conditionMessage(m), fixed = TRUE)) {
        invokeRestart("muffleMessage")
      }
    }
  )
  translated[match(text, unique_text)]
}
