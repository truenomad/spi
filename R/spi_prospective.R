#' Compute the annually updated SPI, refitting the model each year
#'
#' @description
#' Computes a district-year Surveillance Performance Index in which each
#' assessment year's expected counts are estimated from preceding years only.
#' For every assessment year, [spi_expected()] is refitted on the years before
#' it. The assessment year's counts are masked (set to `NA`), so the model
#' predicts them from the earlier years, and later years are dropped. The
#' year's observed counts are then compared with that prediction through
#' [spi_index()]. At least `min_history` years of history are required before
#' the first assessment year.
#'
#' By default the index is centred on each assessment year's national
#' observed-to-expected ratio. The model defaults are those of
#' [spi_expected()], which are the paper specification.
#'
#' @param cases Tibble with the district identifier (see `id_col`), `month`
#'   (Date) and `count`. One row per district-month, covering the history and
#'   every assessment year.
#' @param population Tibble with the district identifier and the denominator
#'   column named by `pop_col`, keyed by `year` (integer) or by `month`
#'   (Date), as in [spi_expected()].
#' @param adjacency nb object or sf object, as in [spi_expected()].
#' @param first_assessment Integer. The first year to assess. It must be at
#'   least `min_history` years after the first year in `cases`.
#' @param last_assessment Integer or `NULL`. The last year to assess. `NULL`
#'   (default) uses the last year in `cases`.
#' @param min_history Integer. The number of years of history a fit needs
#'   before the year it assesses. Default: 3.
#' @param centre Character. "national" (default) divides each assessment
#'   year's SPI by that year's national observed-to-expected ratio. "none"
#'   leaves the raw ratio untouched. See [spi_index()].
#' @param covariates Optional tibble of covariates keyed by `year` or
#'   `month`, as in [spi_expected()]. Each fit receives only the rows up to its
#'   assessment year. Default: `NULL`.
#' @param id_col Character. Name of the district identifier column. Default:
#'   "district_id".
#' @param pop_col Character. Name of the denominator column in `population`.
#'   Default: "pop_u15".
#' @param boundaries Optional `sf` object or data frame carrying the id column
#'   plus admin name columns, joined onto the summary as in [spi_index()].
#'   Default: `NULL`.
#' @param min_expected Numeric. District-years with a total expected count
#'   below this threshold are flagged as low-information. Default: 1.
#' @param predictive Logical. Reserved for a held-out predictive check. Only
#'   `FALSE` is supported at present. Default: `FALSE`.
#' @param verbose Logical. Print one progress step per assessment year.
#'   Default: `TRUE`.
#' @param ... Further arguments passed to [spi_expected()], such as `season`,
#'   `year_effect`, `overdispersion`, `n_draws` or `seed`. The arguments this
#'   function sets itself (`cases`, `population`, `adjacency`, `covariates`,
#'   `id_col`, `pop_col`, `keep_draws` and `verbose`) cannot be passed here.
#'
#' @return Object of class `spi_index` at `level = "district_year"`, with
#'   the elements described in [spi_index()] (`draws`, `summary`,
#'   `low_information`, `totals`, `id_col`, `centre`, `national`, `call`),
#'   plus `prospective`: a tibble with one row per assessment year and the
#'   columns `year`, `training_start`, `training_end` and `districts`.
#'
#' @details
#' A district-year whose assessment year has fewer than 12 months of data is
#' kept, and its `n_months` column shows how many months it covers. A warning
#' names the affected years.
#'
#' The draws of different assessment years come from separate fits. The
#' `totals` row sums them draw by draw, which gives a usable median of the
#' total expected count but is not a joint posterior.
#'
#' @section Runtime:
#' Each assessment year needs its own INLA fit, so the run time grows with
#' the number of assessment years. Every fit uses the same `...` arguments,
#' including `n_draws`.
#'
#' @seealso [spi_expected()], [spi_index()], [spi_concordance()]
#' @family spi core functions
#'
#' @export
#' @examples
#' \dontrun{
#' s <- synth_surveillance
#' spi <- spi_prospective(
#'   cases = s$cases,
#'   population = s$population,
#'   adjacency = s$boundaries,
#'   first_assessment = 2018,
#'   id_col = "adm2_guid",
#'   boundaries = s$boundaries
#' )
#' spi$prospective
#' }
spi_prospective <- function(
  cases,
  population,
  adjacency,
  first_assessment,
  last_assessment = NULL,
  min_history = 3L,
  centre = c("national", "none"),
  covariates = NULL,
  id_col = "district_id",
  pop_col = "pop_u15",
  boundaries = NULL,
  min_expected = 1,
  predictive = FALSE,
  verbose = TRUE,
  ...
) {
  .check_pkg(
    c("dplyr", "tibble", "matrixStats", "cli"),
    reason = "to compute the annually updated surveillance performance index"
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
      "{.arg {reserved}} {?is/are} set by {.fn spi_prospective} and \\
       cannot be passed through {.arg ...}."
    )
  }
  if (isTRUE(predictive)) {
    cli::cli_abort(
      "{.code predictive = TRUE} is not yet supported by \\
       {.fn spi_prospective}."
    )
  }
  centre <- match.arg(centre)
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

  first_assessment <- as.integer(first_assessment)
  min_history <- as.integer(min_history)
  data_years <- range(.year_of(cases$month))
  last_assessment <- as.integer(last_assessment %||% data_years[[2]])
  if (first_assessment - data_years[[1]] < min_history) {
    cli::cli_abort(
      "The first assessment year needs at least {min_history} years of \\
       history; the data start in {data_years[[1]]}, so the earliest is \\
       {data_years[[1]] + min_history}."
    )
  }
  if (last_assessment > data_years[[2]] || first_assessment > last_assessment) {
    cli::cli_abort(
      "Assessment years must fall within the data, which end in \\
       {data_years[[2]]}."
    )
  }
  targets <- seq.int(first_assessment, last_assessment)

  # --- one fit per assessment year ----------------------
  per_year <- vector("list", length(targets))
  for (i in seq_along(targets)) {
    target <- targets[[i]]
    # a literal message, because cli re-reads glue fields when the step ends,
    # by which time `target` has moved on
    if (verbose) {
      cli::cli_progress_step(paste0(
        "Fitting ", target, ": training ", data_years[[1]], " to ", target - 1L
      ))
    }
    # later years are dropped and the target year's counts masked, so the
    # model predicts the year it assesses
    window <- .up_to_year(cases, target)
    window$count[.year_of(window$month) == target] <- NA
    fit <- spi_expected(
      cases = window,
      population = .up_to_year(population, target),
      adjacency = adjacency,
      covariates = .up_to_year(covariates, target),
      id_col = id_col,
      pop_col = pop_col,
      keep_draws = TRUE,
      verbose = FALSE,
      ...
    )
    # the full cases table supplies the target year's real counts
    spi <- spi_index(
      fit,
      cases = cases,
      level = "district_year",
      centre = centre,
      min_expected = min_expected,
      verbose = FALSE
    )
    keep <- spi$summary$year == target
    in_target <- .year_of(fit$data$month) == target
    per_year[[i]] <- list(
      summary = spi$summary[keep, , drop = FALSE],
      draws = spi$draws[, keep, drop = FALSE],
      expected_total = matrixStats::rowSums2(
        fit$draws[, in_target, drop = FALSE]
      ),
      national = if (!is.null(spi$national)) {
        spi$national[spi$national$year == target, , drop = FALSE]
      },
      window = tibble::tibble(
        year = target,
        training_start = data_years[[1]],
        training_end = target - 1L,
        districts = sum(keep)
      )
    )
  }
  if (verbose) {
    cli::cli_progress_done()
  }

  # --- bind the assessment years ------------------------
  summary_tbl <- dplyr::bind_rows(lapply(per_year, `[[`, "summary"))
  draws <- do.call(cbind, lapply(per_year, `[[`, "draws"))
  national <- if (centre == "national") {
    dplyr::bind_rows(lapply(per_year, `[[`, "national"))
  }
  prospective <- dplyr::bind_rows(lapply(per_year, `[[`, "window"))
  expected_draws <- do.call(cbind, lapply(per_year, `[[`, "expected_total"))

  summary_tbl <- .attach_admin_labels(summary_tbl, boundaries, id_col)
  tot <- .spi_totals(summary_tbl, expected_draws, min_expected)

  short <- summary_tbl$n_months < 12L
  if (any(short)) {
    n_short <- sum(short)
    short_years <- sort(unique(summary_tbl$year[short]))
    cli::cli_warn(
      "{n_short} district-year{?s} cover{?s/} fewer than 12 months, in \\
       {.val {short_years}}; see {.field n_months}."
    )
  }

  result <- structure(
    list(
      draws = draws,
      summary = summary_tbl,
      level = "district_year",
      low_information = tot$low_information,
      totals = tot$totals,
      id_col = id_col,
      centre = centre,
      national = national,
      prospective = prospective,
      call = match.call()
    ),
    class = "spi_index"
  )

  if (verbose) {
    n_groups <- nrow(summary_tbl)
    cli::cli_alert_success(
      "Computed {format(n_groups, big.mark = ',')} SPI \\
       {ngettext(n_groups, 'value', 'values')} across \\
       {length(targets)} assessment year{?s}."
    )
  }

  result
}

# --- internal helpers -------------------------------------

# calendar year of a Date vector, as integer
# @noRd
.year_of <- function(month) {
  as.integer(format(month, "%Y"))
}

# rows of a table keyed by `month` (Date) or `year` up to and including
# `target`; NULL passes through
# @noRd
.up_to_year <- function(tbl, target) {
  if (is.null(tbl)) {
    return(NULL)
  }
  yr <- if ("month" %in% names(tbl) && inherits(tbl$month, "Date")) {
    .year_of(tbl$month)
  } else if ("year" %in% names(tbl)) {
    tbl$year
  } else {
    cli::cli_abort(
      "Each table needs a {.field month} (Date) or {.field year} column."
    )
  }
  tbl[yr <= target, , drop = FALSE]
}

# a single whole number, for a year or a count of years
# @noRd
.check_whole_year <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != round(x)) {
    cli::cli_abort("{.arg {arg}} must be a single whole number.")
  }
  invisible(x)
}
