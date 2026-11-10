# Preparation of POLIS-shaped AFP and population tables into the inputs of
# spi_expected(): the case date rule, the case panel, the population panel
# and the months the model is fitted on and the SPI is read over.

# ---- column maps and checks -------------------------------------------------

#' Default POLIS column names read by the preparation functions.
#' @noRd
.prep_default_cols <- function() {
  list(
    id = "adm2_guid",
    class = "classification_all",
    surveillance_type = "surveillance_type_name",
    onset_date = "paralysis_onset_date",
    case_date = "case_date",
    stool1_date = "stool1collection_date",
    adequate_stool = "adequate_stool_with_condition"
  )
}

#' Merge the default columns with user overrides.
#' @noRd
.prep_cols <- function(cols) {
  if (!is.list(cols)) {
    cli::cli_abort("{.arg cols} must be a list.")
  }
  unknown <- setdiff(names(cols), names(.prep_default_cols()))
  if (length(unknown) > 0) {
    cli::cli_abort("{.arg cols} has unknown name{?s} {.val {unknown}}.")
  }
  utils::modifyList(.prep_default_cols(), cols)
}

#' Abort unless `df` is a data frame carrying every named column.
#' @noRd
.prep_need_cols <- function(df, needed, arg) {
  if (!is.data.frame(df)) {
    cli::cli_abort("{.arg {arg}} must be a data frame.")
  }
  missing <- setdiff(needed, names(df))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg {arg}} is missing column{?s} {.val {missing}}.")
  }
  invisible(df)
}

#' Abort unless `x` is a single whole number of at least `min`.
#' @noRd
.prep_need_count <- function(x, arg, min = 1) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) &&
    x == round(x) && x >= min
  if (!ok) {
    cli::cli_abort("{.arg {arg}} must be a single whole number >= {min}.")
  }
  invisible(x)
}

# ---- case date and records --------------------------------------------------

#' Date each AFP case is placed on
#'
#' @description
#' Places each case on its paralysis onset date. A record that the
#' laboratory still holds can reach POLIS with no onset date, so the case
#' date is used next and the date the first stool was collected after that.
#'
#' @param cases Data frame of AFP cases, one row per case.
#' @param cols Named list overriding the column names. This function reads
#'   `onset_date` (default `"paralysis_onset_date"`), `case_date` (default
#'   `"case_date"`) and `stool1_date` (default `"stool1collection_date"`).
#'
#' @return Date vector with one element per row of `cases`. It is `NA` where
#'   none of the three dates is recorded.
#'
#' @seealso [spi_case_panel()]
#' @family spi preparation functions
#' @export
#' @examples
#' cases <- tibble::tibble(
#'   paralysis_onset_date = as.Date(c("2024-01-05", NA, NA)),
#'   case_date = as.Date(c(NA, "2024-02-01", NA)),
#'   stool1collection_date = as.Date(c(NA, NA, "2024-03-09"))
#' )
#' spi_case_date(cases)
spi_case_date <- function(cases, cols = list()) {
  m <- .prep_cols(cols)
  .prep_need_cols(cases, c(m$onset_date, m$case_date, m$stool1_date), "cases")
  dplyr::coalesce(
    as.Date(cases[[m$onset_date]]),
    as.Date(cases[[m$case_date]]),
    as.Date(cases[[m$stool1_date]])
  )
}

#' AFP records with a district and a placed month, and a polio flag.
#' @noRd
.prep_records <- function(cases, m, polio_pattern, surveillance_types,
                          unit_col = m$id) {
  .prep_need_cols(cases, unique(c(m$id, unit_col, m$class)), "cases")
  out <- dplyr::filter(cases, !is.na(.data[[m$id]]))
  if (!is.null(surveillance_types)) {
    .prep_need_cols(cases, m$surveillance_type, "cases")
    out <- dplyr::filter(
      out, .data[[m$surveillance_type]] %in% surveillance_types
    )
  }
  class_upper <- toupper(out[[m$class]])
  polio <- if (is.null(polio_pattern)) {
    rep(FALSE, nrow(out))
  } else {
    !is.na(class_upper) & grepl(polio_pattern, class_upper)
  }
  out |>
    dplyr::mutate(
      .unit = as.character(.data[[unit_col]]),
      .month = lubridate::floor_date(
        spi_case_date(out, m), "month"
      ),
      .polio = polio
    ) |>
    dplyr::filter(!is.na(.data$.month))
}

# ---- windows ----------------------------------------------------------------

#' Months the rolling SPI is read over and the model is fitted on
#'
#' @description
#' The running month carries few onsets at the reporting date and would read
#' as a shortfall everywhere, so both month sets stop `lag_months` before the
#' month that contains `as_of`. `spi_window()` returns the `window_months`
#' months ending there. `spi_panel_months()` returns every month from January
#' of `start_year` to the same last month, so the seasonal and spatial terms
#' of [spi_expected()] are estimated on the full history rather than on the
#' window.
#'
#' @param as_of Date. The reporting date.
#' @param window_months Integer. Length of the rolling window in months.
#'   Default: 12.
#' @param lag_months Integer. Closed months between the window and the month
#'   that contains `as_of`. Default: 1, so the window ends with the last full
#'   month.
#' @param start_year Integer. First year of the panel the model is fitted on.
#'   Default: 2016.
#'
#' @return Sorted Date vector of first-of-month dates.
#'
#' @family spi preparation functions
#' @export
#' @examples
#' spi_window(as.Date("2025-03-15"))
#' range(spi_panel_months(as.Date("2025-03-15"), start_year = 2024))
spi_window <- function(as_of, window_months = 12L, lag_months = 1L) {
  .prep_need_count(window_months, "window_months")
  last <- .prep_last_month(as_of, lag_months)
  seq(last, by = "-1 month", length.out = window_months) |> sort()
}

#' @rdname spi_window
#' @export
spi_panel_months <- function(as_of, start_year = 2016L, lag_months = 1L) {
  .prep_need_count(start_year, "start_year")
  last <- .prep_last_month(as_of, lag_months)
  first <- as.Date(paste0(start_year, "-01-01"))
  if (first > last) {
    cli::cli_abort(
      "{.arg start_year} {start_year} is after the last closed month."
    )
  }
  seq(first, last, by = "1 month")
}

#' The last closed month at `as_of`.
#' @noRd
.prep_last_month <- function(as_of, lag_months) {
  as_of <- tryCatch(as.Date(as_of), error = function(e) as.Date(NA))
  if (length(as_of) != 1L || is.na(as_of)) {
    cli::cli_abort("{.arg as_of} must be a single date.")
  }
  .prep_need_count(lag_months, "lag_months", min = 0)
  seq(
    lubridate::floor_date(as_of, "month"),
    by = "-1 month", length.out = lag_months + 1
  ) |>
    min()
}

# ---- case panel -------------------------------------------------------------

#' District by month panel of non-polio AFP cases
#'
#' @description
#' Counts the AFP cases in each district and month that are not classified
#' as poliovirus or compatible. Cases still awaiting a laboratory result
#' count as non-polio AFP. Each case is placed in the month of
#' [spi_case_date()]. The panel is complete: a district and month with no
#' case carries a count of zero, as [spi_expected()] requires.
#'
#' @param cases Data frame of AFP cases, one row per case, with POLIS column
#'   names by default.
#' @param districts Character. District identifiers the panel covers,
#'   usually those of the boundaries the model is fitted on.
#' @param months Date. First-of-month dates the panel covers, usually
#'   [spi_panel_months()].
#' @param cols Named list overriding the column names: `id` (default
#'   `"adm2_guid"`), `class` (default `"classification_all"`),
#'   `surveillance_type` (default `"surveillance_type_name"`) and the dates
#'   read by [spi_case_date()].
#' @param polio_pattern Character. Regular expression matched against the
#'   upper-cased classification. A match is a poliovirus or compatible case
#'   and is not counted. Default: `"WPV|VDPV|COMPATIBLE"`.
#' @param surveillance_types Character. Surveillance types kept. `NULL` keeps
#'   every row. Default: `"AFP"`.
#'
#' @return Tibble with one row per district and month: the district column
#'   (named as `cols$id`), `month` and an integer `count`, sorted by district
#'   and month. This is the `cases` input of [spi_expected()].
#'
#' @family spi preparation functions
#' @export
#' @examples
#' cases <- tibble::tibble(
#'   adm2_guid = c("D1", "D1", "D2"),
#'   classification_all = c("NPAFP", "WPV1", "PENDING"),
#'   surveillance_type_name = "AFP",
#'   paralysis_onset_date = as.Date(c("2024-01-03", "2024-01-09", NA)),
#'   case_date = as.Date(NA),
#'   stool1collection_date = as.Date(c(NA, NA, "2024-02-11"))
#' )
#' spi_case_panel(
#'   cases, c("D1", "D2"),
#'   seq(as.Date("2024-01-01"), by = "month", length.out = 2)
#' )
spi_case_panel <- function(cases, districts, months, cols = list(),
                           polio_pattern = "WPV|VDPV|COMPATIBLE",
                           surveillance_types = "AFP") {
  if (length(districts) == 0L || anyNA(districts)) {
    cli::cli_abort("{.arg districts} must be non-empty and have no NA.")
  }
  if (!inherits(months, "Date") || length(months) == 0L || anyNA(months)) {
    cli::cli_abort("{.arg months} must be a non-empty Date vector.")
  }
  m <- .prep_cols(cols)
  districts <- unique(as.character(districts))
  months <- sort(unique(months))

  counted <- .prep_records(cases, m, polio_pattern, surveillance_types) |>
    dplyr::filter(
      !.data$.polio, .data$.unit %in% districts, .data$.month %in% months
    ) |>
    dplyr::summarise(count = dplyr::n(), .by = c(".unit", ".month"))

  dplyr::cross_join(
    tibble::tibble(.unit = districts),
    tibble::tibble(.month = months)
  ) |>
    dplyr::left_join(counted, by = c(".unit", ".month")) |>
    dplyr::mutate(count = as.integer(dplyr::coalesce(.data$count, 0L))) |>
    dplyr::arrange(.data$.unit, .data$.month) |>
    dplyr::rename(!!m$id := ".unit", month = ".month")
}

# ---- population -------------------------------------------------------------

#' Under-15 population per district and year for the model
#'
#' @description
#' `spi_pop_index()` totals the under-15 population each year over the
#' districts that carry a positive population in every year requested, so a
#' denominator carried back in time is not compared against a changing set
#' of districts. `spi_pop_panel()` returns one denominator per district and
#' year. A district first drawn part way through the series has no
#' population for earlier years; those years take the district's first
#' population scaled by the index, or the first population unchanged where
#' the index is missing.
#'
#' @param population Data frame of population estimates. Rows for the same
#'   district and year are summed; missing and non-positive values are
#'   dropped.
#' @param districts Character. District identifiers the panel covers.
#' @param years Integer. Years the panel covers.
#' @param id_col,year_col,pop_col Character. Column names in `population`.
#'   Defaults: `"adm2_guid"`, `"year"`, `"u15_pop"`.
#'
#' @return `spi_pop_index()` returns a tibble with `year` and `total`.
#'   `spi_pop_panel()` returns a tibble with the district column (named as
#'   `id_col`), `year`, `pop_u15` and a logical `carried` that is `TRUE`
#'   where the value was carried back. Without `carried`, this is the
#'   `population` input of [spi_expected()].
#'
#' @family spi preparation functions
#' @export
#' @examples
#' pop <- tibble::tibble(
#'   adm2_guid = c("D1", "D1", "D2"),
#'   year = c(2023L, 2024L, 2024L),
#'   u15_pop = c(100, 110, 50)
#' )
#' spi_pop_index(pop, c("D1", "D2"), 2023:2024)
#' spi_pop_panel(pop, c("D1", "D2"), 2023:2024)
spi_pop_index <- function(population, districts, years,
                          id_col = "adm2_guid", year_col = "year",
                          pop_col = "u15_pop") {
  observed <- .prep_pop_observed(
    population, districts, years, id_col, year_col, pop_col
  )
  .prep_pop_index(observed, years)
}

#' @rdname spi_pop_index
#' @export
spi_pop_panel <- function(population, districts, years,
                          id_col = "adm2_guid", year_col = "year",
                          pop_col = "u15_pop") {
  observed <- .prep_pop_observed(
    population, districts, years, id_col, year_col, pop_col
  )
  index <- .prep_pop_index(observed, years)
  base <- observed |>
    dplyr::slice_min(.data$.year, n = 1, by = ".unit") |>
    dplyr::select(".unit", base_year = ".year", base_pop = "pop_u15")

  dplyr::cross_join(
    tibble::tibble(.unit = unique(as.character(districts))),
    tibble::tibble(.year = sort(unique(as.integer(years))))
  ) |>
    dplyr::left_join(observed, by = c(".unit", ".year")) |>
    dplyr::left_join(base, by = ".unit") |>
    dplyr::left_join(index, by = c(.year = "year")) |>
    dplyr::left_join(
      dplyr::rename(index, base_year = "year", base_total = "total"),
      by = "base_year"
    ) |>
    dplyr::mutate(
      carried = is.na(.data$pop_u15),
      pop_u15 = dplyr::coalesce(
        .data$pop_u15,
        as.numeric(.data$base_pop) * .data$total / .data$base_total,
        .data$base_pop
      )
    ) |>
    dplyr::select(!!id_col := ".unit", year = ".year", "pop_u15", "carried")
}

#' Positive under-15 population summed per district and year.
#' @noRd
.prep_pop_observed <- function(population, districts, years, id_col,
                               year_col, pop_col) {
  .prep_need_cols(population, c(id_col, year_col, pop_col), "population")
  if (length(years) == 0L || anyNA(years)) {
    cli::cli_abort("{.arg years} must be non-empty and have no NA.")
  }
  population |>
    dplyr::transmute(
      .unit = as.character(.data[[id_col]]),
      .year = as.integer(.data[[year_col]]),
      .value = as.numeric(.data[[pop_col]])
    ) |>
    dplyr::filter(
      .data$.unit %in% as.character(districts),
      .data$.year %in% as.integer(years),
      !is.na(.data$.value), .data$.value > 0
    ) |>
    dplyr::summarise(
      pop_u15 = sum(.data$.value, na.rm = TRUE),
      .by = c(".unit", ".year")
    )
}

#' Yearly totals over districts present in every year.
#' @noRd
.prep_pop_index <- function(observed, years) {
  observed |>
    dplyr::mutate(n_years = dplyr::n(), .by = ".unit") |>
    dplyr::filter(.data$n_years == length(unique(years))) |>
    dplyr::summarise(total = sum(.data$pop_u15), .by = ".year") |>
    dplyr::rename(year = ".year")
}
