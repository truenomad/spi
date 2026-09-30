#' Calculate the SPI directly from preceding reporting
#'
#' @description
#' Calculates the surveillance performance index (SPI) for each district and
#' assessment year from earlier NPAFP counts and population estimates,
#' without fitting the INLA spatial and temporal model. A district's expected
#' count is its NPAFP rate over all preceding years multiplied by its current
#' population. The district observed-to-expected ratio is then divided by the
#' national observed-to-expected ratio for the same year.
#'
#' Inspect the inputs and calculation for one district with [spi_direct_explain()].
#'
#' @details
#' **Steps for each assessment year.** Only years before the assessment year
#' enter the expectation.
#'
#' 1. Add up each district's preceding NPAFP cases and preceding population
#'    (child-years), and divide to get its historical rate.
#' 2. Expected cases = current population x historical rate.
#' 3. Divide observed by expected cases.
#' 4. Divide that ratio by the national ratio, using observed and expected
#'    totals from districts with a positive expected count. The result is SPI.
#'
#' **When the SPI cannot be calculated.** `history_check` records whether the
#' district's history supports the calculation. `"ok"`: a positive expected
#' count can be calculated. `"no previous case"`: the district has earlier years but no
#' NPAFP case in them, so its historical rate and expected count are 0 and the
#' observed-to-expected ratio and SPI are `NA`. `"no previous year"`: the
#' district has no earlier year in the data (for example a new district), so
#' its historical rate, expected count, observed-to-expected ratio and SPI are
#' all `NA`. A district with one earlier year gets an SPI; `history_years`,
#' `history_cases` and `history_pop` show how much history lies behind it.
#'
#' **National centring.** The national observed-to-expected ratio is
#' calculated from district-years with a positive expected count. Districts
#' for which the direct SPI cannot be calculated do not contribute to national
#' centring.
#' If the included districts report no cases in the assessment year, the
#' national ratio is zero and all centred SPI values are `NA`, even for
#' districts whose `history_check` is `"ok"`.
#'
#' **Population check.** Expected cases depend on the population, so each
#' district's population series is screened before calculation. Each
#' consecutive-year change in log population is compared with the median
#' change across districts in the same year. A change is unusual when its
#' deviation lies outside
#' Tukey's far-out fences (three interquartile ranges beyond the quartiles)
#' of all such deviations and exceeds `log(1.1)` in absolute magnitude.
#' This corresponds to a growth factor above 1.1 or below 1/1.1 times the
#' median growth factor. `population_qc` is `"check_spike"` when an unusual
#' deviation is followed by one in the opposite direction among the next
#' two recorded transitions (two years when the annual series is complete),
#' `"check_change"` for any other unusual change, `"check_history"` when the
#' population records do not cover all dataset years up to the assessment year,
#' and `"ok"` otherwise. Flags identify unusual patterns for review; they do
#' not imply that the population is incorrect, and supplied populations are
#' never altered.
#'
#' @param data Data frame with one row per district and year: the district
#'   identifier (`id_col`), the year (`year_col`), the number of NPAFP cases
#'   (`count_col`) and the population under 15 (`pop_col`). A table
#'   with columns `district`, `year`, `npafp_cases` and `population_u15`
#'   needs no other argument.
#' @param population Optional data frame with one row per district and year,
#'   for when the population is held in a separate table: the district
#'   identifier, the year and the population under 15 (`pop_col`). It
#'   then defines which districts and years are assessed; a district-year
#'   with no row in `data` is counted as zero cases. Default `NULL`: the
#'   population is taken from `data`.
#' @param first_assessment Integer. First assessment year. Default: the
#'   first year in the data plus `min_history`.
#' @param last_assessment Integer. Last assessment year. Default: the last
#'   year in the data.
#' @param min_history Integer. Calendar years after the first data year
#'   before assessment starts when `first_assessment` is not given. Default 3.
#'   This does not require every district to have that many earlier records.
#' @param id_col,year_col,count_col,pop_col Character. Column names in
#'   `data` and `population`. Defaults `"district"`, `"year"`,
#'   `"npafp_cases"` and `"population_u15"`.
#' @param per Numeric. Rates are reported per this many people. Default
#'   100000.
#' @param verbose Logical. Print progress and a summary. Default `TRUE`.
#'
#' @return An `spi_direct` object, a list with:
#'   * `summary`: one row per district and assessment year with `observed`,
#'     `pop`, `npafp_rate`, `history_years`, `history_cases`, `history_pop`
#'     (preceding child-years), `history_rate`, `history_check`, `expected`,
#'     `oe`, `national_oe`, `spi` and `population_qc`. Rates are per `per`.
#'   * `national`: observed, expected and the national observed-to-expected
#'     ratio for each assessment year.
#'   * `population_qc`: the population check for each district.
#'   * `metadata`: the arguments and data range used.
#'
#'   `print()` gives a short country summary, `summary()` a fuller report and
#'   `as_tibble()` the `summary` table.
#'
#' @seealso [spi_direct_explain()], `vignette("spi-direct")`, and
#'   [spi_index()] for the model-based SPI.
#'
#' @examples
#' annual <- synth_surveillance$cases |>
#'   dplyr::mutate(year = as.integer(format(month, "%Y"))) |>
#'   dplyr::summarise(count = sum(count), .by = c(adm2_guid, year))
#'
#' res <- spi_direct(
#'   data = annual,
#'   population = synth_surveillance$population,
#'   id_col = "adm2_guid",
#'   count_col = "count",
#'   pop_col = "pop_u15",
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
  first_assessment = NULL,
  last_assessment = NULL,
  min_history = 3L,
  per = 100000,
  verbose = TRUE
) {
  .direct_check_args(
    data, population, id_col, year_col, count_col, pop_col, per, verbose
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

  # --- one assessment year at a time ------------------------------------
  if (verbose) {
    cli::cli_alert_info(
      "Calculating expected cases for {.val {length(targets)}} \\
       assessment year{?s} ({min(targets)}-{max(targets)})..."
    )
  }
  summary_tbl <- dplyr::bind_rows(lapply(targets, function(t) {
    .direct_one_year(annual, t)
  }))

  # --- national centring ------------------------------------------------
  # only district-years with a positive expected count enter the ratio
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
      c("npafp_rate", "history_rate"),
      function(x) x * per
    )) |>
    dplyr::select(
      "district_id", "year", "observed", "pop", "npafp_rate",
      "history_years", "history_cases", "history_pop", "history_rate",
      "history_check", "expected", "oe", "national_oe", "spi",
      "population_qc"
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
      metadata = list(
        first_assessment = min(targets), last_assessment = max(targets),
        years_in_data = range(years_all),
        id_col = id_col, year_col = year_col, count_col = count_col,
        pop_col = pop_col,
        population_source = if (is.null(population)) "data" else
          "population",
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
.direct_check_args <- function(data, population, id_col, year_col,
                               count_col, pop_col, per, verbose) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (!is.null(population) && !is.data.frame(population)) {
    cli::cli_abort("{.arg population} must be a data frame or NULL.")
  }
  cols <- list(id_col = id_col, year_col = year_col, count_col = count_col,
               pop_col = pop_col)
  for (nm in names(cols)) {
    if (!rlang::is_string(cols[[nm]])) {
      cli::cli_abort("{.arg {nm}} must be a single string.")
    }
  }
  if (!rlang::is_bool(verbose)) {
    cli::cli_abort("{.arg verbose} must be TRUE or FALSE.")
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

# --- one assessment year ------------------------------------------------

# Expected cases from the district's own preceding rate. No preceding case:
# rate and expected 0, no SPI. No preceding year: rate and expected NA.
#' @noRd
.direct_one_year <- function(annual, target) {
  current <- annual[annual$year == target, ]
  hist <- annual[annual$year < target, ] |>
    dplyr::summarise(
      history_cases = sum(.data$observed),
      history_pop = sum(.data$pop),
      history_years = dplyr::n(),
      .by = "district_id"
    )

  current |>
    dplyr::select("district_id", "observed", "pop") |>
    dplyr::left_join(hist, by = "district_id") |>
    dplyr::mutate(
      year = target,
      history_years = dplyr::coalesce(.data$history_years, 0L),
      history_cases = dplyr::coalesce(.data$history_cases, 0),
      history_pop = dplyr::coalesce(.data$history_pop, 0),
      history_check = dplyr::case_when(
        .data$history_years == 0L ~ "no previous year",
        .data$history_cases == 0 ~ "no previous case",
        TRUE ~ "ok"
      ),
      npafp_rate = .data$observed / .data$pop,
      history_rate = dplyr::if_else(
        .data$history_years > 0L, .data$history_cases / .data$history_pop,
        NA_real_
      ),
      # population x cases / child-years, in the order of the hand calculation
      expected = dplyr::if_else(
        .data$history_years > 0L,
        .data$pop * .data$history_cases / .data$history_pop, NA_real_
      ),
      oe = dplyr::if_else(.data$history_check == "ok",
                          .data$observed / .data$expected, NA_real_)
    )
}

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
  cli::cli_text("expected = pop x history_rate / {per_lab}")
  cli::cli_text("oe = observed / expected")
  cli::cli_text("spi = oe / national_oe")
  cli::cli_rule()

  latest <- max(sm$year)
  sl <- sm[sm$year == latest, ]
  nat <- x$national[x$national$year == latest, ]
  nat_value <- function(column, digits = 1L) {
    if (nrow(nat) == 0L) return("not available")
    format(round(nat[[column]], digits), nsmall = digits, big.mark = " ")
  }
  cli::cli_h3("Data overview ({latest})")
  cli::cli_text("Districts assessed: {nrow(sl)}")
  cli::cli_text(
    "Reported NPAFP cases (all districts): \\
     {format(sum(sl$observed), big.mark = ' ')}"
  )
  cli::cli_text(
    "National comparison (positive expected counts only): \\
     {nat_value('national_observed', 0L)} reported; \\
     {nat_value('national_expected')} expected"
  )
  cli::cli_text("National observed/expected: {nat_value('national_oe', 2L)}")
  cli::cli_text(
    "Districts with SPI below 1: {sum(sl$spi < 1, na.rm = TRUE)} of \\
     {sum(is.finite(sl$spi))}"
  )
  n_no_case <- sum(sl$history_check == "no previous case")
  n_no_year <- sum(sl$history_check == "no previous year")
  n_no_national <- sum(sl$history_check == "ok" & !is.finite(sl$spi))
  cli::cli_text(
    "No SPI: {n_no_case} with no previous case, {n_no_year} with no \\
     previous year, {n_no_national} with no positive national ratio"
  )
  cli::cli_text(
    "Population flags: {sum(sl$population_qc != 'ok', na.rm = TRUE)}"
  )
  cli::cli_rule()

  cli::cli_h3("Results by year")
  by_year <- sm |>
    dplyr::summarise(
      districts = dplyr::n(),
      spi_available = sum(is.finite(.data$spi)),
      median_spi = round(stats::median(.data$spi, na.rm = TRUE), 2),
      below_1 = sum(.data$spi < 1, na.rm = TRUE),
      .by = "year"
    ) |>
    dplyr::left_join(
      x$national |>
        dplyr::transmute(.data$year,
                         observed = .data$national_observed,
                         expected = round(.data$national_expected, 1),
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
    sprintf("No SPI:             %d",
            sum(!is.finite(sm$spi[sm$year == latest])))
  )
  cli::cli_h2("Direct SPI")
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
#' hand from the ones above it. When the SPI cannot be calculated, the
#' breakdown says so and gives the reason.
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

  ok <- identical(r$history_check, "ok")
  available <- is.finite(r$spi)
  not_calc <- "not calculated"
  reason <- switch(
    r$history_check,
    "no previous case" = "no previous NPAFP case",
    "no previous year" = "no previous year of data",
    if (!available) "national observed / expected is unavailable or zero" else NULL
  )
  out <- tibble::tibble(
    section = c(
      rep("Current reporting", 3L), rep("Previous reporting", 4L),
      "Expected reporting", rep("SPI", 3L + !available), "Data checks"
    ),
    component = c(
      "Observed NPAFP cases", "Current population under 15", "NPAFP rate",
      "Previous years", "Previous NPAFP cases", "Previous child-years",
      "Previous rate", "Expected NPAFP cases",
      "District observed / expected", "National observed / expected", "SPI",
      if (!available) "Reason",
      "Population check"
    ),
    value = c(
      num(r$observed, 0), num(r$pop, 0), num(r$npafp_rate),
      as.character(r$history_years), num(r$history_cases, 0),
      num(r$history_pop, 0),
      if (is.na(r$history_rate)) "not available" else num(r$history_rate),
      if (is.na(r$expected)) "not available" else num(r$expected),
      if (ok) num(r$oe) else not_calc, num(r$national_oe),
      if (available) num(r$spi) else not_calc,
      reason,
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
