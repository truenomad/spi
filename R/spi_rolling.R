# The SPI read over a rolling window from one fit on the full history, its
# classification against the index threshold and the NPAFP rate target, and
# the entry point that chains the preparation steps with them.

# ---- window summary ---------------------------------------------------------

#' SPI over a rolling window from one fitted model
#'
#' @description
#' Reads the SPI over the months in `window` from a single [spi_expected()]
#' fit on the full history. Observed counts and expected counts are summed
#' over the window inside each posterior draw, divided, and centred on the
#' national observed-to-expected ratio of the same draw, so SPI = 1 is the
#' national level. With `parents`, a parent unit such as a province is the
#' sum of its districts inside each draw, so the two levels agree on how much
#' was expected. The summation and centring are those [spi_index()] uses.
#'
#' This differs from [spi_index()]. `spi_index()` refits the model once per
#' assessment year on the months before that year, with the year's own counts
#' masked, so each expected count is predicted from earlier reporting only.
#' `spi_window_summary()` refits nothing: it reads any window, of any length,
#' from one fit whose expected counts were estimated with the window's own
#' counts included. Use it to update a rolling window every month from a
#' fit that can be cached.
#'
#' @param fit An [spi_expected()] result with `keep_draws = TRUE`, or any
#'   list with a `draws` matrix and a `data` tibble (columns `id_col`,
#'   `month`, `count`, `pop`, one row per column of `draws`).
#' @param window Date. First-of-month dates to read over, usually
#'   [spi_window()].
#' @param id_col Character. District identifier column in `fit$data`.
#'   Default: `"adm2_guid"`.
#' @param parents Optional two-column data frame mapping `id_col` to a parent
#'   unit. The result is then one row per parent. Default: `NULL`.
#' @param person_time Character. How `pop_u15` is formed. The numerator is
#'   the sum, over the unit's rows in the window, of each district's annual
#'   population in `fit$data$pop`. With `"window"` (default) the denominator
#'   is the number of months in `window`; with `"observed"` it is the number
#'   of window months present in the fit. The two agree when every window
#'   month is in the fit.
#'
#' @return Tibble with one row per unit: the unit column, `observed`,
#'   `pop_u15` (annual denominator), `months` (window months in the fit),
#'   `expected_total` (posterior median expected count), `spi_median`,
#'   `spi_mean`, `spi_q05`, `spi_q10`, `spi_q90`, `spi_q95` and
#'   `national_oe` (summed observed over the median summed expected count).
#'
#' @seealso [spi_window_classify()], [spi_rolling_index()]
#' @family spi rolling window functions
#' @export
spi_window_summary <- function(fit, window, id_col = "adm2_guid",
                               parents = NULL,
                               person_time = c("window", "observed")) {
  person_time <- match.arg(person_time)
  .roll_need_fit(fit, id_col)
  if (!inherits(window, "Date") || length(window) == 0L) {
    cli::cli_abort("{.arg window} must be a non-empty Date vector.")
  }
  data <- tibble::as_tibble(fit$data) |>
    dplyr::mutate(.col = dplyr::row_number()) |>
    dplyr::filter(.data$month %in% window)
  if (nrow(data) == 0L) {
    cli::cli_abort("No month of {.arg window} is in the fit.")
  }
  unit_col <- id_col
  if (!is.null(parents)) {
    .prep_need_cols(parents, id_col, "parents")
    unit_col <- setdiff(names(parents), id_col)
    if (length(unit_col) != 1L) {
      cli::cli_abort("{.arg parents} must have exactly two columns.")
    }
    data <- dplyr::left_join(
      dplyr::select(data, -dplyr::any_of(unit_col)), parents, by = id_col
    )
  }

  summary <- .spi_aggregate(
    list(
      draws = fit$draws[, data$.col, drop = FALSE],
      data = dplyr::select(data, -".col"),
      id_col = unit_col
    ),
    level = "district_total",
    centre = "national",
    verbose = FALSE
  )$summary

  denominators <- data |>
    dplyr::summarise(
      pop_u15 = sum(.data$pop) / if (person_time == "window") {
        length(window)
      } else {
        dplyr::n_distinct(.data$month)
      },
      months = dplyr::n_distinct(.data$month),
      .by = dplyr::all_of(unit_col)
    )

  summary |>
    dplyr::select(-"pop_u15") |>
    dplyr::left_join(denominators, by = unit_col) |>
    dplyr::relocate("pop_u15", "months", .after = "observed")
}

#' Abort unless `fit` carries matching `data` and `draws`.
#' @noRd
.roll_need_fit <- function(fit, id_col) {
  if (!is.list(fit) || !is.data.frame(fit$data) || !is.matrix(fit$draws)) {
    cli::cli_abort(
      "{.arg fit} must hold a {.field data} table and a {.field draws} \\
      matrix, as returned by {.fn spi_expected} with \\
      {.code keep_draws = TRUE}."
    )
  }
  .prep_need_cols(fit$data, c(id_col, "month", "count", "pop"), "fit$data")
  if (ncol(fit$draws) != nrow(fit$data)) {
    cli::cli_abort(
      "{.arg fit} has {ncol(fit$draws)} draw column{?s} for \\
      {nrow(fit$data)} data row{?s}."
    )
  }
  invisible(fit)
}

# ---- classification ---------------------------------------------------------

#' Classify a window summary on the SPI and the NPAFP rate
#'
#' @description
#' Sets the conventional non-polio AFP rate against its target and the SPI
#' against its threshold, for a [spi_window_summary()] result.
#'
#' The rate has the observed count over the window as numerator and
#' `pop_u15 / rate_multiplier` as denominator. It is not annualised by
#' default, so it is a yearly rate when the window covers 12 months. With
#' `annualise = TRUE` the same ratio is multiplied by `months_per_year` and
#' divided by `months`. A unit meets the rate target when the rate is at or
#' above `rate_target`.
#'
#' A unit is flagged by the SPI, meaning low relative AFP reporting, when
#' `spi_median` is below `index_threshold` and the upper bound `spi_q95` is
#' below 1, so the whole 90% interval sits below the national level. A unit
#' whose `expected_total` is below `min_expected`, or whose SPI is missing,
#' carries too little information and is left unclassified (`NA`).
#'
#' This relates to [spi_compare_npafp()] as follows. With
#' `spi_rule = "interval"`, `spi_compare_npafp()` flags the SPI by the same
#' rule, but it takes a district-year [spi_index()] or [spi_simple()] result,
#' computes the rate as count / population x multiplier from a population
#' table, classifies every district-year without a `min_expected` rule, uses
#' the labels "Neither below", "SPI below threshold only", "NPAFP below
#' target only" and "Both below", and adds agreement statistics.
#' `spi_window_classify()` works on one rolling window, leaves low-information
#' units unclassified, and returns the classification column by column.
#'
#' @param summary A [spi_window_summary()] result.
#' @param rate_target Numeric. NPAFP rate target per `rate_multiplier`.
#'   Default: 3.
#' @param index_threshold Numeric. SPI below which a unit can be flagged.
#'   Default: 0.8.
#' @param min_expected Numeric. Smallest posterior median expected count at
#'   which a unit is classified. Default: 1.
#' @param rate_multiplier Numeric. Population scale of the rate. Default:
#'   100000.
#' @param annualise Logical. Scale the rate to `months_per_year`. Default:
#'   `FALSE`.
#' @param months_per_year Numeric. Months the rate is annualised to when
#'   `annualise = TRUE`. Default: 12.
#' @param agreement_levels Character, length 4. Labels for: both pass, both
#'   flag, rate passes and SPI flags, rate flags and SPI passes. Default:
#'   `c("both_pass", "both_flag", "spi_only", "rate_only")`.
#' @param focus_levels Character, length 2. Labels for units that meet the
#'   rate target but are flagged by the SPI, and for every other classified
#'   unit. Default: `c("focus", "other")`.
#'
#' @return `summary` with `npafp_rate`, `readable`, `rate_pass`, `ci_below`,
#'   `index_pass`, `agreement` (factor) and `focus_status` (factor) added.
#'
#' @family spi rolling window functions
#' @export
#' @examples
#' summary <- tibble::tibble(
#'   observed = c(3, 3), pop_u15 = 1e5, months = 12L,
#'   expected_total = 5, spi_median = c(0.9, 0.7), spi_q95 = c(1.1, 0.9)
#' )
#' spi_window_classify(summary)
spi_window_classify <- function(summary, rate_target = 3,
                                index_threshold = 0.8, min_expected = 1,
                                rate_multiplier = 1e5, annualise = FALSE,
                                months_per_year = 12,
                                agreement_levels = c(
                                  "both_pass", "both_flag", "spi_only",
                                  "rate_only"
                                ),
                                focus_levels = c("focus", "other")) {
  .prep_need_cols(
    summary,
    c("observed", "pop_u15", "months", "expected_total", "spi_median",
      "spi_q95"),
    "summary"
  )
  numbers <- list(
    rate_target = rate_target, index_threshold = index_threshold,
    min_expected = min_expected, rate_multiplier = rate_multiplier,
    months_per_year = months_per_year
  )
  bad <- names(numbers)[!purrr::map_lgl(
    numbers, \(x) is.numeric(x) && length(x) == 1L && !is.na(x)
  )]
  if (length(bad) > 0) {
    cli::cli_abort("{.arg {bad}} must be a single number.")
  }
  if (rate_multiplier <= 0 || months_per_year <= 0) {
    cli::cli_abort(
      "{.arg rate_multiplier} and {.arg months_per_year} must be positive."
    )
  }
  if (!isTRUE(annualise) && !isFALSE(annualise)) {
    cli::cli_abort("{.arg annualise} must be TRUE or FALSE.")
  }
  if (length(agreement_levels) != 4L || length(focus_levels) != 2L) {
    cli::cli_abort(
      "{.arg agreement_levels} needs four values and {.arg focus_levels} two."
    )
  }

  summary |>
    dplyr::mutate(
      npafp_rate = .data$observed / (.data$pop_u15 / rate_multiplier),
      npafp_rate = if (annualise) {
        .data$npafp_rate * months_per_year / .data$months
      } else {
        .data$npafp_rate
      },
      readable = .data$expected_total >= min_expected &
        !is.na(.data$spi_median),
      rate_pass = .data$npafp_rate >= rate_target,
      ci_below = .data$spi_q95 < 1,
      index_pass = !(.data$spi_median < index_threshold & .data$ci_below),
      agreement = dplyr::case_when(
        !.data$readable ~ NA_character_,
        .data$rate_pass & .data$index_pass ~ agreement_levels[1],
        !.data$rate_pass & !.data$index_pass ~ agreement_levels[2],
        .data$rate_pass & !.data$index_pass ~ agreement_levels[3],
        !.data$rate_pass & .data$index_pass ~ agreement_levels[4]
      ),
      agreement = factor(.data$agreement, levels = agreement_levels),
      focus_status = dplyr::case_when(
        !.data$readable ~ NA_character_,
        .data$rate_pass & !.data$index_pass ~ focus_levels[1],
        .default = focus_levels[2]
      ),
      focus_status = factor(.data$focus_status, levels = focus_levels)
    )
}

# ---- entry point ------------------------------------------------------------

#' Rolling SPI for AFP surveillance from POLIS-shaped tables
#'
#' @description
#' Builds the rolling-window SPI table for districts, or for a parent level
#' such as the province. The SPI describes low relative AFP reporting: it
#' compares the non-polio AFP cases each unit reported over the window with
#' the number expected from [spi_expected()], as an observed-to-expected
#' ratio centred on the national ratio. The table also sets the NPAFP rate
#' against its target. See [spi_window_summary()] for the window and how it
#' differs from [spi_index()], and [spi_window_classify()] for the
#' classification.
#'
#' Pass a `fit`, for example one the caller has cached, to skip the model.
#' With no `fit`, the function builds the case panel with
#' [spi_case_panel()] and the population panel with [spi_pop_panel()] over
#' [spi_panel_months()], and fits [spi_expected()] with its default model
#' (harmonic season, iid year effect, negative binomial). Supplying `cases`
#' also adds stool adequacy over the window from the `adequate_stool` column:
#' rows with a blank value are left out and a value in `adequate_values`
#' counts as adequate.
#'
#' The defaults reproduce the weekly polio review pipeline for Afghanistan.
#'
#' @param fit Optional [spi_expected()] result with draws kept. Default:
#'   `NULL`.
#' @param as_of Date. The reporting date.
#' @param window_months,lag_months,start_year See [spi_window()]. Defaults:
#'   12, 1 and 2016.
#' @param parents Optional two-column data frame mapping the district column
#'   to a parent unit. Default: `NULL`.
#' @param cases Data frame of AFP cases. Needed to fit the model, and used
#'   for stool adequacy when given. Default: `NULL`.
#' @param population Data frame of population estimates, needed to fit the
#'   model. See [spi_pop_panel()]. Default: `NULL`.
#' @param adjacency District boundaries as an `sf` object, passed to
#'   [spi_adjacency()], or an `nb` neighbour list. Needed to fit the model.
#'   Default: `NULL`.
#' @param districts Character. District identifiers of the panel. Defaults to
#'   those of `adjacency`.
#' @param cols Named list overriding the case column names; see
#'   [spi_case_panel()]. `adequate_stool` (default
#'   `"adequate_stool_with_condition"`) is read for stool adequacy.
#' @param polio_pattern,surveillance_types See [spi_case_panel()].
#' @param pop_year_col,pop_col Year and population columns of `population`.
#'   Defaults: `"year"` and `"u15_pop"`. The district column is `cols$id`.
#' @param adequate_values Character. Values of the stool adequacy column
#'   that count as adequate. Default: `"Yes"`.
#' @param person_time See [spi_window_summary()]. Default: `"window"`.
#' @param rate_target,index_threshold,min_expected,rate_multiplier,annualise
#'   See [spi_window_classify()]. Defaults: 3, 0.8, 1, 100000 and `FALSE`.
#' @param n_draws,seed Passed to [spi_expected()]. Defaults: 1000 and 42.
#' @param ... Further arguments passed to [spi_expected()].
#'
#' @return Tibble with one row per unit, the columns of
#'   [spi_window_summary()] and [spi_window_classify()], and with `cases`
#'   also `stool_cases`, `stool_adequate` and `stool_adequacy` (percent).
#'   Sorted by `spi_median`.
#'
#' @family spi rolling window functions
#' @export
#' @examples
#' \dontrun{
#' rolling <- spi_rolling_index(
#'   as_of = Sys.Date(), cases = afp, population = pop,
#'   adjacency = boundaries
#' )
#' }
spi_rolling_index <- function(fit = NULL, as_of, window_months = 12L,
                              lag_months = 1L, start_year = 2016L,
                              parents = NULL, cases = NULL,
                              population = NULL, adjacency = NULL,
                              districts = NULL, cols = list(),
                              polio_pattern = "WPV|VDPV|COMPATIBLE",
                              surveillance_types = "AFP",
                              pop_year_col = "year", pop_col = "u15_pop",
                              adequate_values = "Yes",
                              person_time = "window", rate_target = 3,
                              index_threshold = 0.8, min_expected = 1,
                              rate_multiplier = 1e5, annualise = FALSE,
                              n_draws = 1000L, seed = 42L, ...) {
  m <- .prep_cols(cols)
  window <- spi_window(as_of, window_months, lag_months)
  if (is.null(fit)) {
    if (is.null(cases) || is.null(population) || is.null(adjacency)) {
      cli::cli_abort(
        "Supply {.arg fit}, or {.arg cases}, {.arg population} and \\
        {.arg adjacency} to fit the model."
      )
    }
    districts <- districts %||% .roll_adjacency_ids(adjacency, m$id)
    months <- spi_panel_months(as_of, start_year, lag_months)
    if (inherits(adjacency, "sf")) {
      adjacency <- spi_adjacency(boundaries = adjacency, id_col = m$id)
    }
    fit <- spi_expected(
      cases = spi_case_panel(
        cases, districts, months, cols, polio_pattern, surveillance_types
      ),
      population = spi_pop_panel(
        population, districts, sort(unique(lubridate::year(months))),
        m$id, pop_year_col, pop_col
      ) |>
        dplyr::select(-"carried"),
      adjacency = adjacency,
      id_col = m$id,
      pop_col = "pop_u15",
      n_draws = n_draws,
      seed = seed,
      ...
    )
  }
  out <- fit |>
    spi_window_summary(
      window,
      id_col = m$id, parents = parents, person_time = person_time
    ) |>
    spi_window_classify(
      rate_target = rate_target, index_threshold = index_threshold,
      min_expected = min_expected, rate_multiplier = rate_multiplier,
      annualise = annualise
    )
  if (!is.null(cases)) {
    unit_col <- names(out)[1]
    stool <- .roll_stool_adequacy(
      cases, m, window, unit_col, adequate_values, surveillance_types
    )
    out <- dplyr::left_join(out, stool, by = unit_col)
  }
  dplyr::arrange(out, .data$spi_median)
}

#' District identifiers carried by an sf boundary layer or an nb list.
#' @noRd
.roll_adjacency_ids <- function(adjacency, id_col) {
  if (inherits(adjacency, "sf")) {
    .prep_need_cols(adjacency, id_col, "adjacency")
    return(sort(unique(as.character(adjacency[[id_col]]))))
  }
  ids <- attr(adjacency, "region.id")
  if (is.null(ids)) {
    cli::cli_abort(
      "Supply {.arg districts}: {.arg adjacency} carries no district ids."
    )
  }
  sort(unique(as.character(ids)))
}

#' Stool adequacy per unit over the window, on every AFP record.
#' @noRd
.roll_stool_adequacy <- function(cases, m, window, unit_col, adequate_values,
                                 surveillance_types) {
  .prep_need_cols(cases, m$adequate_stool, "cases")
  .prep_records(cases, m, NULL, surveillance_types, unit_col = unit_col) |>
    dplyr::mutate(.value = as.character(.data[[m$adequate_stool]])) |>
    dplyr::filter(
      .data$.month %in% window,
      !is.na(.data$.value), trimws(.data$.value) != ""
    ) |>
    dplyr::summarise(
      stool_cases = dplyr::n(),
      stool_adequate = sum(.data$.value %in% adequate_values),
      .by = ".unit"
    ) |>
    dplyr::mutate(
      stool_adequacy = 100 * .data$stool_adequate / .data$stool_cases
    ) |>
    dplyr::rename(!!unit_col := ".unit")
}
