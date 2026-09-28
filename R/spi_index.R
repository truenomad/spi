#' Compute the Surveillance Performance Index (SPI)
#'
#' @description
#' Computes posterior draws of the Surveillance Performance Index, the
#' ratio of observed to expected counts, centred by default on the national
#' ratio for each period. Each assessment year's expected counts are estimated
#' from preceding years only. For every assessment year, [spi_expected()] is
#' refitted on the months before it. The assessment year's counts are masked
#' (set to `NA`), so the model predicts them from the earlier months, and later
#' months are dropped. The year's observed counts are then compared with that
#' prediction. At least `min_history` years of history are required before the
#' first assessment year.
#'
#' The credible interval reflects uncertainty in expected counts. With national
#' centring, SPI ~= 1 means reporting matches the national pattern. With
#' `centre = "none"`, the reference is the district's own expected count.
#' Values below 1 mean reporting is below the chosen reference. The model
#' defaults are those of [spi_expected()], which are the paper specification.
#'
#' @param cases Tibble with the district identifier (see `id_col`), `month`
#'   (Date) and `count`. One row per district-month, covering the history and
#'   every assessment year.
#' @param population Tibble with the district identifier and the denominator
#'   column named by `pop_col`, keyed by `year` (integer) or by `month`
#'   (Date), as in [spi_expected()].
#' @param adjacency nb object or sf object, as in [spi_expected()].
#' @param first_assessment Integer. The first year to assess, labelled by the
#'   calendar year in which its window closes (see `year_end_month`). Its
#'   window must open at least `min_history` years after the first month in
#'   `cases`.
#' @param last_assessment Integer or `NULL`. The last year to assess. `NULL`
#'   (default) uses the year of the last month in `cases`.
#' @param min_history Integer. The number of years of history a fit needs
#'   before the window it assesses. Default: 3.
#' @param level Character. Aggregation level: "district_month" (raw, highest
#'   temporal resolution), "district_quarter" (calendar-quarter SPI per
#'   district), "district_year" (annual SPI per district, default),
#'   "district_total" (single SPI per district over all assessment years).
#' @param centre Character. "national" (default) divides each period's SPI
#'   draws and summaries by that period's national observed-to-expected
#'   ratio, so a country-wide reporting change does not move every district
#'   the same way. "none" leaves the raw ratio untouched.
#' @param year_end_month Integer 1 to 12. Month in which the assessment year
#'   closes, for `level = "district_year"`. The default, 12, gives calendar
#'   years. Any other value gives a non-calendar reporting year:
#'   `year_end_month = 4` assesses May through April, so a review can close on
#'   the month the decision was actually taken rather than on 31 December.
#'   Each window is labelled by the calendar year in which it closes, so May
#'   2024 to April 2025 reads as 2025, and is predicted from the months before
#'   it opens, here up to April 2024. The monthly offset still uses the
#'   calendar-year denominator. Ignored at every other level, which assess
#'   calendar years. For a rolling 12-month SPI updated every month, rerun
#'   with `year_end_month` set to the month that has just closed. Default: 12
#'   (calendar years).
#' @param covariates Optional tibble of covariates keyed by `year` or
#'   `month`, as in [spi_expected()]. Each fit receives only the rows up to the
#'   end of its assessment year. Default: `NULL`.
#' @param id_col Character. Name of the district identifier column. Default:
#'   "district_id".
#' @param pop_col Character. Name of the denominator column in `population`.
#'   Default: "pop_u15".
#' @param boundaries Optional `sf` object or data frame carrying the id column
#'   plus admin name columns (`adm1_name`, `adm2_name`, ...). When supplied,
#'   those names are joined onto the `summary` output immediately before the
#'   id column, so saved SPI tables carry human-readable labels next to the
#'   district id. Default `NULL`.
#' @param min_expected Numeric. Districts or district-periods with total
#'   expected count below this threshold are marked as low-information. SPI
#'   is still computed; inspect the counts and uncertainty before interpreting
#'   it. Default: 1.
#' @param predictive Logical. Reserved for a held-out predictive check. Only
#'   `FALSE` is supported at present. Default: `FALSE`.
#' @param verbose Logical. Progress messages via cli, with one step per
#'   assessment year. Default: TRUE.
#' @param ... Further arguments passed to [spi_expected()], such as `season`,
#'   `year_effect`, `overdispersion`, `n_draws` or `seed`. The arguments this
#'   function sets itself (`cases`, `population`, `adjacency`, `covariates`,
#'   `id_col`, `pop_col`, `keep_draws` and `verbose`) cannot be passed here.
#'
#' @return Object of class `spi_index`. A list containing:
#' \describe{
#'   \item{draws}{Matrix `[n_draws x n_groups]` of posterior SPI samples.}
#'   \item{summary}{Tibble with grouping variables and SPI summaries (median,
#'     mean, q05, q10, q90, q95) per district or district-period.}
#'   \item{level}{Aggregation level used.}
#'   \item{low_information}{Tibble of districts or district-periods marked
#'     as low-information.}
#'   \item{totals}{Tibble of overall observed and expected totals.}
#'   \item{id_col}{The id column name, echoed for downstream use.}
#'   \item{centre}{The `centre` argument used.}
#'   \item{national}{Tibble of the national observed-to-expected ratio per
#'     period (`national_observed`, `national_expected`, `national_oe`,
#'     `districts`), or `NULL` when `centre = "none"`.}
#'   \item{windows}{Tibble with one row per assessment year: `year`,
#'     `window_start`, `window_end`, `training_start`, `training_end` (first
#'     days of months), `months` and `districts`.}
#'   \item{call}{Matched call.}
#' }
#'
#' @details
#' For aggregated levels, observed counts are summed within each grouping,
#' expected counts are summed within each grouping per posterior draw, and
#' the ratio is computed per draw. This preserves the joint uncertainty in
#' the expected denominator.
#'
#' The draws of different assessment years come from separate fits. At
#' `level = "district_total"` and in the `totals` row, expected counts are
#' summed across years draw by draw, which gives a usable median but is not a
#' joint posterior.
#'
#' With `centre = "national"`, every district's SPI draws for a period are
#' further divided by that period's national observed-to-expected ratio
#' (summed observed over summed median-expected, across districts). This
#' compares the district ratio with the national ratio. District SPI can
#' stay unchanged when district and national ratios change together, so
#' review the national ratio separately. Because a draw-level quantile scales
#' with a positive constant, dividing `spi_median`, `spi_q05` and `spi_q95` by
#' the same ratio gives the same result as dividing the draws first and
#' re-summarising. A period with no reported cases nationally has no ratio to
#' divide by; its rows become `NA` and a warning names the affected periods.
#'
#' An assessment year with fewer than 12 months of data is kept, and a warning
#' names it. At `level = "district_year"`, the `n_months` column shows how many
#' months each row covers.
#'
#' @section Runtime:
#' Each assessment year needs its own INLA fit, so the run time grows with
#' the number of assessment years. Every fit uses the same `...` arguments,
#' including `n_draws`. Each call refits the model, so a second level or
#' centring setting repeats the fits.
#'
#' @seealso [spi_expected()], [spi_compare_npafp()]
#' @family spi core functions
#'
#' @export
#' @examples
#' \dontrun{
#' s <- synth_surveillance
#' spi_dy <- spi_index(
#'   cases = s$cases,
#'   population = s$population,
#'   adjacency = s$boundaries,
#'   first_assessment = 2018,
#'   id_col = "adm2_guid",
#'   boundaries = s$boundaries
#' )
#' print(spi_dy)
#' spi_dy$windows
#' }
spi_index <- function(
  cases,
  population,
  adjacency,
  first_assessment,
  last_assessment = NULL,
  min_history = 3L,
  level = c(
    "district_year",
    "district_month",
    "district_quarter",
    "district_total"
  ),
  centre = c("national", "none"),
  year_end_month = 12L,
  covariates = NULL,
  id_col = "district_id",
  pop_col = "pop_u15",
  boundaries = NULL,
  min_expected = 1,
  predictive = FALSE,
  verbose = TRUE,
  ...
) {
  # --- check required packages --------------------------
  .check_pkg(
    c("dplyr", "tibble", "lubridate", "matrixStats", "cli"),
    reason = "to compute the surveillance performance index"
  )

  # --- validate inputs ----------------------------------
  reserved <- intersect(
    names(list(...)),
    c(
      "keep_draws", "cases", "population", "adjacency", "covariates",
      "id_col", "pop_col", "verbose"
    )
  )
  if (length(reserved) > 0) {
    cli::cli_abort(
      "{.arg {reserved}} {?is/are} set by {.fn spi_index} and \\
       cannot be passed through {.arg ...}."
    )
  }
  if (isTRUE(predictive)) {
    cli::cli_abort(
      "{.code predictive = TRUE} is not yet supported by {.fn spi_index}."
    )
  }
  level <- match.arg(level)
  centre <- match.arg(centre)
  year_end_month <- suppressWarnings(as.integer(year_end_month))
  if (length(year_end_month) != 1L || is.na(year_end_month) ||
        year_end_month < 1L || year_end_month > 12L) {
    cli::cli_abort("{.arg year_end_month} must be a single month, 1 to 12.")
  }
  if (year_end_month != 12L && level != "district_year") {
    cli::cli_alert_warning(
      "{.arg year_end_month} only applies to {.val district_year}; ignored."
    )
    year_end_month <- 12L
  }
  .check_whole_year(first_assessment, "first_assessment")
  if (!is.null(last_assessment)) {
    .check_whole_year(last_assessment, "last_assessment")
  }
  .check_whole_year(min_history, "min_history")
  stopifnot(
    is.numeric(min_expected), min_expected >= 0,
    is.logical(predictive), is.logical(verbose)
  )
  missing_cols <- setdiff(c(id_col, "month", "count"), names(cases))
  if (length(missing_cols) > 0) {
    cli::cli_abort("{.arg cases} is missing {.val {missing_cols}}.")
  }
  if (!inherits(cases$month, "Date")) {
    cli::cli_abort("{.code cases$month} must be a Date.")
  }
  if (anyDuplicated(cases[c(id_col, "month")])) {
    cli::cli_abort("{.arg cases} has duplicate district-month rows.")
  }
  if (!is.numeric(cases$count) || any(
    !is.na(cases$count) & (!is.finite(cases$count) |
      cases$count < 0 | cases$count != floor(cases$count))
  )) {
    cli::cli_abort(
      "{.arg cases$count} must contain non-negative whole counts or NA."
    )
  }

  # --- assessment windows -------------------------------
  first_assessment <- as.integer(first_assessment)
  min_history <- as.integer(min_history)
  data_start <- lubridate::floor_date(min(cases$month), "month")
  data_end <- lubridate::floor_date(max(cases$month), "month")
  history_end <- lubridate::add_with_rollback(
    data_start, months(12L * min_history)
  )
  earliest <- as.integer(.window_year(history_end, year_end_month))
  if (.window_start(earliest, year_end_month) < history_end) {
    earliest <- earliest + 1L
  }
  if (first_assessment < earliest) {
    cli::cli_abort(
      "The first assessment year needs at least {min_history} years of \\
       history; the data start in {format(data_start, '%B %Y')}, so the \\
       earliest is {earliest}."
    )
  }
  latest <- as.integer(.window_year(data_end, year_end_month))
  last_assessment <- as.integer(last_assessment %||% latest)
  if (last_assessment > latest || first_assessment > last_assessment) {
    cli::cli_abort(
      "Assessment years must fall within the data, which end in \\
       {format(data_end, '%B %Y')}."
    )
  }
  targets <- seq.int(first_assessment, last_assessment)

  # --- one fit per assessment year ----------------------
  per_year <- vector("list", length(targets))
  for (i in seq_along(targets)) {
    target <- targets[[i]]
    opens <- .window_start(target, year_end_month)
    closes <- as.Date(sprintf("%d-%02d-01", target, year_end_month))
    training_end <- lubridate::add_with_rollback(opens, months(-1L))
    # a literal message, because cli re-reads glue fields when the step ends,
    # by which time `target` has moved on
    if (verbose) {
      cli::cli_progress_step(paste0(
        "Fitting ", target, ": training ", format(data_start, "%Y-%m"),
        " to ", format(training_end, "%Y-%m")
      ))
    }
    # later months are dropped and the window's counts masked, so the model
    # predicts the window it assesses
    window <- cases[cases$month <= closes, , drop = FALSE]
    window$count[window$month >= opens] <- NA
    fit <- spi_expected(
      cases = window,
      population = .up_to(population, closes),
      adjacency = adjacency,
      covariates = .up_to(covariates, closes),
      id_col = id_col,
      pop_col = pop_col,
      keep_draws = TRUE,
      verbose = FALSE,
      ...
    )
    in_window <- fit$data$month >= opens
    per_year[[i]] <- list(
      draws = fit$draws[, in_window, drop = FALSE],
      data = fit$data[in_window, , drop = FALSE],
      window = tibble::tibble(
        year = target,
        window_start = opens,
        window_end = closes,
        training_start = data_start,
        training_end = training_end,
        months = dplyr::n_distinct(fit$data$month[in_window]),
        districts = dplyr::n_distinct(fit$data[[id_col]][in_window])
      )
    )
  }
  if (verbose) {
    cli::cli_progress_done()
  }

  # --- SPI from the assessment windows ------------------
  # every district-month sits in exactly one window, so the windows bind into
  # one set of draws that is summed to `level` as a single fit would be; the
  # full cases table supplies the masked counts
  windows <- dplyr::bind_rows(lapply(per_year, `[[`, "window"))
  expected <- list(
    draws = do.call(cbind, lapply(per_year, `[[`, "draws")),
    data = dplyr::bind_rows(lapply(per_year, `[[`, "data")),
    id_col = id_col
  )
  short <- windows$months < 12L
  if (any(short)) {
    short_years <- windows$year[short]
    cli::cli_warn(
      "Assessment year{?s} {.val {short_years}} cover{?s/} fewer than 12 \\
       months."
    )
  }

  result <- .spi_aggregate(
    expected,
    cases = cases,
    boundaries = boundaries,
    level = level,
    centre = centre,
    min_expected = min_expected,
    year_end_month = year_end_month,
    verbose = verbose
  )
  result$windows <- windows
  result$call <- match.call()
  result
}

# --- internal helpers -------------------------------------

# SPI from a list carrying `draws`, `data` and `id_col`, as spi_index() binds
# from its assessment windows: observed counts are taken from `cases` when
# given, since the fit's own counts are masked in each window, then summed to
# `level`, divided by the expected draws and centred. The arguments are
# validated by spi_index().
# @noRd
.spi_aggregate <- function(
  expected,
  cases = NULL,
  boundaries = NULL,
  level = "district_year",
  centre = "national",
  min_expected = 1,
  year_end_month = 12L,
  verbose = TRUE
) {
  id_col <- if (is.null(expected$id_col)) "district_id" else expected$id_col

  # --- align observed counts to draws column ordering ---
  if (verbose) {
    cli::cli_progress_step("Aligning observed counts")
  }
  draws <- expected$draws
  fit_data <- expected$data |>
    dplyr::mutate(col_idx = dplyr::row_number())

  if (is.null(cases)) {
    observed <- fit_data$count
  } else {
    observed <- fit_data |>
      dplyr::select(dplyr::all_of(c(id_col, "month"))) |>
      dplyr::left_join(
        cases |> dplyr::select(dplyr::all_of(c(id_col, "month", "count"))),
        by = c(id_col, "month")
      ) |>
      dplyr::pull("count")

    n_missing <- sum(is.na(observed))
    if (n_missing > 0) {
      n_missing_s <- format(n_missing, big.mark = ",")
      month_phrase <- ngettext(
        n_missing,
        "district-month is",
        "district-months are"
      )
      cli::cli_alert_warning(
        "{n_missing_s} {month_phrase} missing observed counts \\
         (treated as 0)."
      )
      observed[is.na(observed)] <- 0
    }
  }

  # --- compute SPI at requested level -------------------
  if (verbose) {
    cli::cli_progress_step("Computing SPI at {.val {level}} level")
  }
  spi_obj <- switch(
    level,
    district_month = .spi_district_month(draws, observed, fit_data, id_col),
    district_quarter = .spi_district_quarter(
      draws, observed, fit_data, id_col
    ),
    district_year = .spi_district_year(
      draws, observed, fit_data, id_col, year_end_month
    ),
    district_total = .spi_district_total(draws, observed, fit_data, id_col)
  )

  # --- centre on the national observed-to-expected ratio ------
  national <- NULL
  if (centre == "national") {
    period_col <- switch(
      level,
      district_month = "month",
      district_quarter = "quarter",
      district_year = "year",
      district_total = character(0)
    )
    centred <- .centre_spi(spi_obj, period_col)
    spi_obj$draws <- centred$draws
    spi_obj$summary <- centred$summary
    national <- centred$national
  }

  # --- attach admin names (adm*_name) just before the id column ---
  spi_obj$summary <- .attach_admin_labels(spi_obj$summary, boundaries, id_col)

  # --- low-information flag and totals -----------------
  tot <- .spi_totals(spi_obj$summary, draws, min_expected)
  low_info <- tot$low_information
  totals <- tot$totals

  # --- assemble output ----------------------------------
  result <- structure(
    list(
      draws = spi_obj$draws,
      summary = spi_obj$summary,
      level = level,
      low_information = low_info,
      totals = totals,
      id_col = id_col,
      centre = centre,
      national = national
    ),
    class = "spi_index"
  )

  if (verbose) {
    cli::cli_progress_done()
    n_groups <- nrow(spi_obj$summary)
    n_groups_s <- format(n_groups, big.mark = ",")
    val_word <- ngettext(n_groups, "value", "values")
    cli::cli_alert_success(
      "Computed {n_groups_s} SPI {val_word}."
    )
    if (nrow(low_info) > 0) {
      n_low <- nrow(low_info)
      n_low_s <- format(n_low, big.mark = ",")
      group_word <- ngettext(n_low, "group", "groups")
      cli::cli_alert_warning(
        "{n_low_s} {group_word} marked low-information \\
         (expected < {min_expected})."
      )
    }
  }

  result
}



# the low-information rows and the totals row of a spi_index. Every
# district-month falls in exactly one summary row, so the summed `observed`
# column equals the summed monthly counts. `expected_draws` is any matrix of
# expected counts whose row sums give the total expected count per draw.
# @noRd
.spi_totals <- function(summary, expected_draws, min_expected) {
  low_info <- summary |>
    dplyr::filter(.data$expected_total < min_expected)
  totals <- tibble::tibble(
    total_observed = sum(summary$observed),
    total_expected_median = round(
      stats::median(matrixStats::rowSums2(expected_draws)), 1
    ),
    n_groups = nrow(summary),
    n_low_information = nrow(low_info)
  )
  list(low_information = low_info, totals = totals)
}

# summarise a draws matrix into median / mean / quantile columns
# in one pass via matrixStats. Returns a tibble with the columns
# expected_total, spi_median, spi_mean, spi_q05/q10/q90/q95.
# @noRd
.spi_summarise <- function(exp_draws, spi_draws) {
  qs <- matrixStats::colQuantiles(
    spi_draws,
    probs = c(0.05, 0.10, 0.50, 0.90, 0.95),
    na.rm = TRUE
  )
  tibble::tibble(
    expected_total = matrixStats::colMedians(exp_draws, na.rm = TRUE),
    spi_median = qs[, 3],
    spi_mean = matrixStats::colMeans2(spi_draws, na.rm = TRUE),
    spi_q05 = qs[, 1],
    spi_q10 = qs[, 2],
    spi_q90 = qs[, 4],
    spi_q95 = qs[, 5]
  )
}

# SPI at district-month level
# @noRd
.spi_district_month <- function(draws, observed, fit_data, id_col) {
  # spi_draws[i, j] = observed[j] / draws[i, j]
  spi_draws <- sweep(1 / draws, 2, observed, FUN = "*")

  base <- fit_data[, c(id_col, "month")]
  base$observed <- observed
  base$pop_u15 <- fit_data$pop

  summary_tbl <- dplyr::bind_cols(
    tibble::as_tibble(base),
    .spi_summarise(draws, spi_draws)
  )

  list(draws = spi_draws, summary = summary_tbl)
}

# SPI at district-quarter level
# quarter is the first day of the calendar quarter (e.g. 2016-01-01,
# 2016-04-01) so it sorts and groups correctly as a Date.
# @noRd
.spi_district_quarter <- function(draws, observed, fit_data, id_col) {
  fit_data$quarter <- lubridate::floor_date(fit_data$month, "quarter")

  groups <- fit_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_col, "quarter")))) |>
    dplyr::summarise(
      cols = list(.data$col_idx),
      obs_sum = sum(.env$observed[.data$col_idx]),
      pop_u15 = mean(.data$pop),
      .groups = "drop"
    )

  exp_sum_draws <- .sum_draws_by_group(draws, groups$cols)
  spi_draws <- sweep(1 / exp_sum_draws, 2, groups$obs_sum, FUN = "*")

  base <- groups[, c(id_col, "quarter")]
  base$observed <- groups$obs_sum
  base$pop_u15 <- groups$pop_u15

  summary_tbl <- dplyr::bind_cols(
    tibble::as_tibble(base),
    .spi_summarise(exp_sum_draws, spi_draws)
  )

  list(draws = spi_draws, summary = summary_tbl)
}

# SPI at district-year level
# @noRd
.spi_district_year <- function(draws, observed, fit_data, id_col,
                               year_end_month = 12L) {
  # each month is grouped under the assessment year it falls in, the same
  # label spi_index() gives the window it fitted
  fit_data$year <- .window_year(fit_data$month, year_end_month)

  groups <- fit_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_col, "year")))) |>
    dplyr::summarise(
      cols = list(.data$col_idx),
      obs_sum = sum(.env$observed[.data$col_idx]),
      pop_u15 = mean(.data$pop),
      n_months = dplyr::n(),
      .groups = "drop"
    )

  exp_sum_draws <- .sum_draws_by_group(draws, groups$cols)
  spi_draws <- sweep(1 / exp_sum_draws, 2, groups$obs_sum, FUN = "*")

  base <- groups[, c(id_col, "year")]
  base$observed <- groups$obs_sum
  base$pop_u15 <- groups$pop_u15
  # incomplete windows are kept but marked, since the last assessment year
  # can end before its closing month
  base$n_months <- groups$n_months

  summary_tbl <- dplyr::bind_cols(
    tibble::as_tibble(base),
    .spi_summarise(exp_sum_draws, spi_draws)
  )

  list(draws = spi_draws, summary = summary_tbl)
}

# SPI at district-total level
# @noRd
.spi_district_total <- function(draws, observed, fit_data, id_col) {
  groups <- fit_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) |>
    dplyr::summarise(
      cols = list(.data$col_idx),
      obs_sum = sum(.env$observed[.data$col_idx]),
      pop_u15 = mean(.data$pop),
      .groups = "drop"
    )

  exp_sum_draws <- .sum_draws_by_group(draws, groups$cols)
  spi_draws <- sweep(1 / exp_sum_draws, 2, groups$obs_sum, FUN = "*")

  base <- groups[, id_col, drop = FALSE]
  base$observed <- groups$obs_sum
  base$pop_u15 <- groups$pop_u15

  summary_tbl <- dplyr::bind_cols(
    tibble::as_tibble(base),
    .spi_summarise(exp_sum_draws, spi_draws)
  )

  list(draws = spi_draws, summary = summary_tbl)
}

# sum draws columns within each group; returns [n_draws x n_groups]
# @noRd
.sum_draws_by_group <- function(draws, cols_list) {
  n_draws <- nrow(draws)
  n_grp <- length(cols_list)
  out <- matrix(NA_real_, n_draws, n_grp)
  for (g in seq_len(n_grp)) {
    out[, g] <- matrixStats::rowSums2(
      draws[, cols_list[[g]], drop = FALSE]
    )
  }
  out
}

# the assessment year a month falls in: its calendar year once shifted
# forward so the year closes on `year_end_month`. With 4, April 2025 moves to
# December 2025 and May 2024 to January 2025, so both read as 2025.
# @noRd
.window_year <- function(month, year_end_month = 12L) {
  shift <- 12L - as.integer(year_end_month)
  if (shift == 0L) {
    return(lubridate::year(month))
  }
  lubridate::year(lubridate::add_with_rollback(month, months(shift)))
}

# first month of the assessment window that closes in `year` on
# `year_end_month`
# @noRd
.window_start <- function(year, year_end_month = 12L) {
  as.Date(sprintf(
    "%d-%02d-01",
    as.integer(year) - (year_end_month < 12L), year_end_month %% 12L + 1L
  ))
}

# calendar year of a Date vector, as integer
# @noRd
.year_of <- function(month) {
  as.integer(format(month, "%Y"))
}

# rows of a table keyed by `month` (Date) up to and including the month
# `closes`, or keyed by `year` up to and including its year; NULL passes
# through
# @noRd
.up_to <- function(tbl, closes) {
  if (is.null(tbl)) {
    return(NULL)
  }
  if ("month" %in% names(tbl) && inherits(tbl$month, "Date")) {
    keep <- tbl$month <= closes
  } else if ("year" %in% names(tbl)) {
    keep <- tbl$year <= .year_of(closes)
  } else {
    cli::cli_abort(
      "Each table needs a {.field month} (Date) or {.field year} column."
    )
  }
  tbl[keep, , drop = FALSE]
}

# a single whole number, for a year or a count of years
# @noRd
.check_whole_year <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != round(x)) {
    cli::cli_abort("{.arg {arg}} must be a single whole number.")
  }
  invisible(x)
}

# --- print method -----------------------------------------

#' @export
print.spi_index <- function(x, ...) {
  rlang::check_dots_empty()
  fmt_int <- function(v) format(v, big.mark = ",")

  n_obs_total <- x$totals$total_observed
  n_exp_med <- x$totals$total_expected_median
  n_groups <- x$totals$n_groups
  val_word <- ngettext(n_groups, "value", "values")

  cli::cli_h2("Surveillance Performance Index")
  cli::cli_bullets(c(
    "i" = "Level: {.val {x$level}}.",
    "i" = "{fmt_int(n_groups)} SPI {val_word} computed.",
    "i" = "Total observed: {fmt_int(n_obs_total)} | total expected \\
           (median): {fmt_int(n_exp_med)}."
  ))

  if (identical(x$centre, "national")) {
    rng <- round(range(x$national$national_oe, na.rm = TRUE), 2)
    cli::cli_bullets(c(
      "i" = "Centred on the national observed-to-expected ratio \\
             (range {rng[1]} to {rng[2]})."
    ))
  }

  cli::cli_h3("SPI distribution (medians)")
  q <- stats::quantile(
    x$summary$spi_median,
    probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
    na.rm = TRUE
  )
  cli::cli_bullets(c(
    "*" = "q05: {round(q[[1]], 2)}",
    "*" = "q25: {round(q[[2]], 2)}",
    "*" = "median: {round(q[[3]], 2)}",
    "*" = "q75: {round(q[[4]], 2)}",
    "*" = "q95: {round(q[[5]], 2)}"
  ))

  # descriptive SPI ranges
  spi_med <- x$summary$spi_median
  n_low <- sum(spi_med < 0.5, na.rm = TRUE)
  n_mid <- sum(spi_med >= 0.5 & spi_med < 0.8, na.rm = TRUE)
  n_ok <- sum(spi_med >= 0.8 & spi_med <= 1.2, na.rm = TRUE)
  n_high <- sum(spi_med > 1.2, na.rm = TRUE)

  cli::cli_h3("SPI ranges")
  cli::cli_bullets(c(
    "*" = "SPI < 0.5: {fmt_int(n_low)}",
    "*" = "SPI 0.5-0.8: {fmt_int(n_mid)}",
    "*" = "SPI 0.8-1.2: {fmt_int(n_ok)}",
    "*" = "SPI > 1.2: {fmt_int(n_high)}"
  ))

  n_low_info <- x$totals$n_low_information
  if (n_low_info > 0) {
    group_word <- ngettext(n_low_info, "group", "groups")
    cli::cli_alert_warning(
      "{fmt_int(n_low_info)} {group_word} marked low-information."
    )
  }

  invisible(x)
}

#' @export
summary.spi_index <- function(object, ...) {
  rlang::check_dots_empty()

  sm <- object$summary

  # core stats
  med_val <- stats::median(sm$spi_median, na.rm = TRUE)
  ratio_val <- object$totals$total_observed /
    object$totals$total_expected_median

  log_spi <- log(sm$spi_median[sm$spi_median > 0])
  skew_val <- mean((log_spi - mean(log_spi))^3) /
    stats::sd(log_spi)^3

  cri_val <- mean(
    sm$spi_q05 > 1 | sm$spi_q95 < 1,
    na.rm = TRUE
  )

  pct_severe <- mean(sm$spi_median < 0.5, na.rm = TRUE)
  pct_elevated <- mean(sm$spi_median > 1.5, na.rm = TRUE)

  q <- stats::quantile(
    sm$spi_median,
    probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
    na.rm = TRUE
  )

  # flag strings reused in both the tibble and the footnotes
  med_flag <- .flag_within(med_val, 0.9, 1.1)
  ratio_flag <- .flag_within(ratio_val, 0.9, 1.1)
  skew_flag <- .flag_within(skew_val, -0.5, 0.5)
  cri_flag <- .flag_within(cri_val, 0.05, 0.50)

  # values displayed with big.mark commas; tibble column type
  # becomes <chr> so large counts render as e.g. "3,870"
  fmt <- function(v) {
    format(v, big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  diagnostics <- tibble::tibble(
    metric = c(
      "median SPI",
      "calibration ratio (obs/exp)",
      "log-SPI skewness",
      "% with CrI excluding 1",
      "% SPI < 0.5",
      "% SPI > 1.5",
      "SPI q05", "SPI q25", "SPI q75", "SPI q95",
      "n groups",
      "n low_information"
    ),
    value = c(
      fmt(round(med_val, 3)),
      fmt(round(ratio_val, 3)),
      fmt(round(skew_val, 3)),
      fmt(round(cri_val * 100, 1)),
      fmt(round(pct_severe * 100, 1)),
      fmt(round(pct_elevated * 100, 1)),
      fmt(round(q[[1]], 3)),
      fmt(round(q[[2]], 3)),
      fmt(round(q[[4]], 3)),
      fmt(round(q[[5]], 3)),
      fmt(nrow(sm)),
      fmt(nrow(object$low_information))
    ),
    flag = c(
      med_flag, ratio_flag, skew_flag, cri_flag,
      NA, NA, NA, NA, NA, NA, NA, NA
    )
  )

  cli::cli_h2("SPI diagnostics")
  cli::cli_alert_info("Level: {.val {object$level}}.")
  cli::cli_alert_info(
    "Flags mark descriptive ranges; they are not validated quality or model-fit tests."
  )
  print(diagnostics)

  # --- footnotes: per-metric interpretation -------------
  cli::cli_h3("Footnotes")
  cli::cli_bullets(c(
    "i" = .note_median(med_val, med_flag),
    "i" = .note_ratio(ratio_val, ratio_flag),
    "i" = .note_skew(skew_val, skew_flag),
    "i" = .note_cri(cri_val, cri_flag)
  ))

  invisible(diagnostics)
}

# --- footnote builders ------------------------------------

# @noRd
.note_median <- function(val, flag) {
  if (!is.finite(val)) return("Median SPI is not available.")
  direction <- if (val < 1) "below" else "above"
  sprintf(
    paste0("Median SPI %.3f is %g%% %s 1. National centring does not ",
           "require the district median to equal 1."),
    val, round(abs(val - 1) * 100, 1), direction
  )
}

# @noRd
.note_ratio <- function(val, flag) {
  if (!is.finite(val)) return("Aggregate observed-to-expected ratio is not available.")
  direction <- if (val < 1) "below" else "above"
  sprintf(
    "Aggregate ratio %.3f -> %s. Total reported counts are %g%% %s modelled expectations.",
    val, flag, round(abs(val - 1) * 100, 1), direction
  )
}

# @noRd
.note_skew <- function(val, flag) {
  if (!is.finite(val)) return("Log-SPI skewness is not available.")
  shape <- if (flag == "pass") {
    "Distribution is roughly symmetric on the log scale."
  } else if (val < 0) {
    "Left tail is heavier: some districts have much lower relative reporting."
  } else {
    "Right tail is heavier: some districts have much higher relative reporting."
  }
  sprintf("Log-SPI skewness %.2f -> %s. %s", val, flag, shape)
}

# @noRd
.note_cri <- function(val, flag) {
  if (!is.finite(val)) return("The share of intervals excluding 1 is not available.")
  sprintf(
    paste0("%g%% of credible intervals exclude 1 -> %s. This describes ",
           "departures from the reference, not surveillance adequacy or model fit."),
    round(val * 100, 1), flag
  )
}

# pass / flag helper: returns "pass" if x is in [lo, hi], else "flag"
# @noRd
.flag_within <- function(x, lo, hi) {
  if (is.na(x)) {
    return(NA_character_)
  }
  if (x >= lo && x <= hi) "pass" else "flag"
}

#' @export
as_tibble.spi_index <- function(x, ...) {
  rlang::check_dots_empty()
  x$summary
}

#' @export
as.data.frame.spi_index <- function(x, ...) {
  rlang::check_dots_empty()
  as.data.frame(x$summary)
}

#' @export
plot.spi_index <- function(
  x,
  type = c("distribution", "funnel", "caterpillar", "calibration"),
  thresholds = c(0.5, 1, 1.5),
  npafp_target = 3,
  year = NULL,
  n_show = 50L,
  ...
) {
  rlang::check_dots_empty()
  .check_pkg("ggplot2", reason = "to plot a spi_index object")
  type <- match.arg(type)

  sm <- x$summary
  if (!is.null(year) && "year" %in% names(sm)) {
    sm <- sm |> dplyr::filter(.data$year == !!year)
  }

  switch(
    type,
    distribution = .plot_distribution(sm, thresholds),
    funnel = .plot_funnel(sm, thresholds, npafp_target),
    caterpillar = .plot_caterpillar(sm, n_show, thresholds),
    calibration = .plot_calibration(sm)
  )
}

# --- internal plot helpers --------------------------------

# package-level palette used by every plot.spi_* method
# so the visual identity stays consistent.
# @noRd
.spi_palette <- list(
  primary = "#4682B4",       # steelblue: histogram / point fill
  primary_dark = "#2C5E8A",  # darker steelblue: stroke / accent / high band
  burgundy = "#7B1D3D",      # threshold lines, calibration diagonal, low band
  grey_mid = "#5A5A5A",      # error bars
  grey_border = "grey30"     # panel border
)

# package-level theme: minimal base, panel border, padded axis titles
# @noRd
.spi_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        colour = .spi_palette$grey_border,
        fill = NA,
        linewidth = 0.4
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 12)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 12)
      ),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey30"),
      # align title and subtitle with the panel (x-axis), not the
      # outer plot edge; "panel" is the ggplot2 default
      plot.title.position = "panel",
      plot.margin = ggplot2::margin(12, 14, 12, 12)
    )
}

# SPI distribution histogram
# @noRd
.plot_distribution <- function(sm, thresholds) {
  n_total <- nrow(sm)
  n_fmt <- format(n_total, big.mark = ",")
  med <- round(stats::median(sm$spi_median, na.rm = TRUE), 2)
  pct_below_05 <- round(
    mean(sm$spi_median < 0.5, na.rm = TRUE) * 100, 1
  )
  pct_above_15 <- round(
    mean(sm$spi_median > 1.5, na.rm = TRUE) * 100, 1
  )

  # drop SPI = 0 so log10 doesn't introduce -Inf
  plot_df <- sm[is.finite(sm$spi_median) & sm$spi_median > 0, ]
  n_dropped <- n_total - nrow(plot_df)
  dropped_note <- if (n_dropped > 0) {
    glue::glue(" | dropped {n_dropped} SPI = 0")
  } else {
    ""
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$spi_median)
  ) +
    ggplot2::geom_histogram(
      bins = 50,
      fill = .spi_palette$primary,
      colour = .spi_palette$primary_dark,
      alpha = 0.7,
      linewidth = 0.3
    ) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::geom_vline(
      xintercept = thresholds[thresholds != 1],
      linetype = "dotted",
      colour = .spi_palette$burgundy,
      linewidth = 0.5
    ) +
    ggplot2::scale_x_log10(
      breaks = c(0.1, 0.25, 0.5, 1, 1.5, 2, 5)
    ) +
    ggplot2::labs(
      title = "SPI distribution",
      subtitle = glue::glue(
        "n = {n_fmt} | median = {med} | ",
        "<0.5: {pct_below_05}% | ",
        ">1.5: {pct_above_15}%{dropped_note}"
      ),
      x = "SPI (log scale)",
      y = "count"
    ) +
    .spi_theme()
}

# funnel plot: SPI vs expected total
# @noRd
.plot_funnel <- function(sm, thresholds, npafp_target = 3) {
  # drop SPI = 0 (log10 would push to -Inf and ggplot warns)
  plot_df <- sm[is.finite(sm$spi_median) & sm$spi_median > 0, ]
  # A small seeded horizontal-only jitter (a few percent of the expected count)
  # breaks the diagonal contours that integer observed counts trace on a log-log
  # funnel, so the dense core reads as a cloud rather than stripes. SPI (y) is
  # never moved.
  plot_df$expected_jit <- plot_df$expected_total *
    withr::with_seed(1L, exp(stats::runif(nrow(plot_df), -0.06, 0.06)))

  spi_cut <- thresholds[thresholds != 1]
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$expected_jit, y = .data$spi_median)
  )

  # Colour each district-year by whether its conventional NPAFP rate meets the
  # target (uses the population denominator carried in the summary). Adequate
  # points are faint so the dense core reads as a density gradient; below-target
  # points sit on top, larger and opaque, so the sparse group stays visible --
  # conventionally adequate districts scatter across the full SPI range, which
  # is the one-directional refinement the SPI adds. No error bars: with hundreds
  # of points they read as noise, and the funnel narrowing already shows that
  # uncertainty shrinks with the expected count.
  has_pop <- "pop_u15" %in% names(plot_df) && any(is.finite(plot_df$pop_u15))
  if (has_pop) {
    lab_adequate <- sprintf("NPAFP >= %g per 100,000", npafp_target)
    lab_below <- sprintf("NPAFP < %g per 100,000", npafp_target)
    plot_df$npafp_rate <- plot_df$observed / plot_df$pop_u15 * 1e5
    plot_df$adequacy <- factor(
      ifelse(plot_df$npafp_rate >= npafp_target, lab_adequate, lab_below),
      levels = c(lab_adequate, lab_below)
    )
    p <- p +
      ggplot2::geom_point(
        data = plot_df[plot_df$adequacy == lab_adequate, ],
        ggplot2::aes(colour = .data$adequacy),
        alpha = 0.35, size = 1.5, shape = 16, stroke = 0
      ) +
      ggplot2::geom_point(
        data = plot_df[plot_df$adequacy == lab_below, ],
        ggplot2::aes(colour = .data$adequacy),
        alpha = 0.9, size = 1.9, shape = 16, stroke = 0
      ) +
      ggplot2::scale_colour_manual(
        name = NULL,
        values = stats::setNames(
          c(.spi_palette$primary_dark, "#D55E00"),
          c(lab_adequate, lab_below)
        )
      ) +
      ggplot2::guides(colour = ggplot2::guide_legend(
        override.aes = list(size = 2.6, alpha = 1)
      ))
  } else {
    p <- p +
      ggplot2::geom_point(
        alpha = 0.4, size = 1.5, shape = 16, stroke = 0,
        colour = .spi_palette$primary_dark
      )
  }

  p +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_hline(
      yintercept = spi_cut, linetype = "dotted",
      colour = .spi_palette$burgundy, linewidth = 0.5
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 5)) +
    ggplot2::labs(
      title = "SPI funnel",
      subtitle = paste(
        "SPI vs expected count; each district-year coloured by conventional",
        "NPAFP adequacy"
      ),
      x = "expected count (log scale)",
      y = "SPI (log scale)"
    ) +
    .spi_theme() +
    ggplot2::theme(legend.position = "bottom")
}

# caterpillar: ranked SPI with credible intervals
# @noRd
.plot_caterpillar <- function(sm, n_show, thresholds) {
  # drop SPI = 0 (log scale)
  sm <- sm[is.finite(sm$spi_median) & sm$spi_median > 0, ]

  ranked <- sm |>
    dplyr::arrange(.data$spi_median)

  bottom <- utils::head(ranked, n_show) |>
    dplyr::mutate(band = "lowest")
  top <- utils::tail(ranked, n_show) |>
    dplyr::mutate(band = "highest")

  # re-index within the displayed subset so x runs 1..(2 * n_show)
  # instead of inheriting positions from the full ranking (which would
  # leave a giant gap in the middle of the plot)
  show <- dplyr::bind_rows(bottom, top) |>
    dplyr::mutate(plot_rank = dplyr::row_number())

  ggplot2::ggplot(
    show,
    ggplot2::aes(
      x = .data$plot_rank,
      y = .data$spi_median
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$spi_q05,
        ymax = .data$spi_q95
      ),
      width = 0,
      alpha = 0.55,
      colour = .spi_palette$grey_mid
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$band),
      size = 2.0
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        lowest = .spi_palette$burgundy,
        highest = .spi_palette$primary_dark
      )
    ) +
    # visual divider between the lowest and highest blocks
    ggplot2::geom_vline(
      xintercept = n_show + 0.5,
      linetype = "dotted",
      colour = "grey60"
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::geom_hline(
      yintercept = thresholds[thresholds != 1],
      linetype = "dotted",
      colour = .spi_palette$burgundy,
      linewidth = 0.5
    ) +
    ggplot2::scale_y_log10(
      breaks = c(0.1, 0.25, 0.5, 1, 2, 5)
    ) +
    ggplot2::labs(
      title = glue::glue(
        "{n_show} lowest and {n_show} highest SPI groups"
      ),
      x = "rank (within displayed subset)",
      y = "SPI (log scale)"
    ) +
    .spi_theme() +
    ggplot2::theme(legend.position = "none")
}

# observed vs expected calibration scatter
# @noRd
.plot_calibration <- function(sm) {
  # drop observed = 0 (log scale)
  plot_df <- sm[is.finite(sm$observed) & sm$observed > 0, ]

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$expected_total,
      y = .data$observed
    )
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = .spi_palette$burgundy,
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      alpha = 0.55,
      size = 1.1,
      colour = .spi_palette$primary_dark
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = "Observed vs expected",
      subtitle = "diagonal = perfect calibration",
      x = "expected (log scale)",
      y = "observed (log scale)"
    ) +
    .spi_theme()
}
