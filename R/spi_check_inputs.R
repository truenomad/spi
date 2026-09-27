#' Check model inputs
#'
#' Checks case counts, population denominators, and district boundaries before
#' fitting. Reports mismatched district IDs, missing months, invalid population
#' values, and geometry problems together.
#'
#' Issues have three severity levels:
#'
#' * **error** -- blocks the fit (missing columns, negative counts, negative
#'   population, duplicate district-months, case ids absent from the shapefile).
#' * **warning** -- needs review (panel gaps, zero population, partial coverage,
#'   shapefile districts with no case rows, invalid geometry, covariate ids
#'   absent from the case data).
#' * **note** -- additional information (adjacency components).
#'
#' [spi_expected()] calls this check and stops on errors. Run it separately
#' to review warnings before fitting.
#'
#' @param cases Tibble with the district identifier (see `id_col`), `month`
#'   (Date), and `count` (integer-valued).
#' @param population Tibble with the district identifier, the denominator
#'   column (see `pop_col`), and either a `month` (Date) or `year` (integer)
#'   column. Granularity is auto-detected.
#' @param shapefile An `sf` object of district polygons, or a pre-built `nb`
#'   neighbour object. Geometry validity is only checked for `sf` input.
#' @param covariates Optional tibble of district covariates. Only its ids are
#'   reconciled against the case panel.
#' @param id_col Character. Name of the district identifier column, shared by
#'   `cases`, `population`, `covariates`, and `shapefile`. Default
#'   `"district_id"`.
#' @param pop_col Character. Name of the denominator column in `population`.
#'   Default `"pop_u15"` (the under-15 at-risk population used for AFP).
#' @param verbose Logical. Print the report once built. Default `TRUE`.
#'
#' @return A `spi_input_check` object: a list with `issues` (a tibble of
#'   graded findings, one row each with `severity`, `code`, `message`, and the
#'   offending `ids`), `gaps` (a `district_id` x `month` tibble of missing
#'   district-months), `n_error` / `n_warning` /
#'   `n_note` counts, `ok` (`TRUE` when there are no error-level issues), and
#'   the panel dimensions. Has a `print` method.
#'
#' @seealso [spi_expected()], [spi_adjacency()]
#'
#' @examples
#' \dontrun{
#' rpt <- spi_check_inputs(
#'   cases = synth_surveillance$cases,
#'   population = synth_surveillance$population,
#'   shapefile = synth_surveillance$boundaries
#' )
#' rpt
#' rpt$gaps
#' }
#' @export
spi_check_inputs <- function(cases,
                            population,
                            shapefile,
                            covariates = NULL,
                            id_col = "district_id",
                            pop_col = "pop_u15",
                            verbose = TRUE) {
  stopifnot(
    is.character(id_col), length(id_col) == 1,
    is.character(pop_col), length(pop_col) == 1,
    is.data.frame(cases),
    is.data.frame(population),
    is.null(covariates) || is.data.frame(covariates)
  )
  if (!inherits(shapefile, "sf") && !inherits(shapefile, "nb")) {
    cli::cli_abort(
      "{.arg shapefile} must be an {.cls sf} object or a {.cls nb} \\
       neighbour object; got {.cls {class(shapefile)}}."
    )
  }

  # --- issue accumulator --------------------------------
  # Every check appends here and none short-circuit, so a report always
  # carries all findings at once -- the whole point of grading. Checks whose
  # prerequisites are missing (e.g. no `month` column) are skipped, not fatal.
  issues <- list()
  add <- function(severity, code, message, ids = NULL) {
    issues[[length(issues) + 1L]] <<- tibble::tibble(
      severity = severity,
      code = code,
      message = message,
      n = if (is.null(ids)) NA_integer_ else length(ids),
      ids = list(ids)
    )
  }

  # --- shapefile ids + geometry -------------------------
  # sf keeps the caller's id_col; nb stores ids in the region.id attribute.
  if (inherits(shapefile, "sf")) {
    if (!id_col %in% names(shapefile)) {
      add("error", "shapefile_no_id",
        .ic_msg("shapefile has no {.field {id_col}} column"))
      shp_ids <- character(0)
    } else {
      shp_ids <- as.character(shapefile[[id_col]])
    }
    valid <- sf::st_is_valid(shapefile)
    n_invalid <- sum(!valid | is.na(valid))
    if (n_invalid > 0) {
      add("warning", "geometry_invalid", .ic_msg(
        "{n_invalid} shapefile polygon{?s} {?is/are} invalid \\
         (reported, not repaired)"
      ))
    }
  } else {
    shp_ids <- as.character(attr(shapefile, "region.id"))
  }

  # --- normalise id + denominator to the internal names -
  # spi_expected() renames id_col -> district_id and pop_col -> pop before it
  # fits; mirror that here so a check catches the same problems a fit would.
  cases <- .ic_rename(cases, id_col, "district_id")
  population <- .ic_rename(population, id_col, "district_id")
  if (!is.null(covariates)) {
    covariates <- .ic_rename(covariates, id_col, "district_id")
  }

  # --- cases: columns, types, values --------------------
  if (nrow(cases) == 0) {
    add("error", "cases_empty", "cases has no rows")
  }
  case_missing <- setdiff(c("district_id", "month", "count"), names(cases))
  if (length(case_missing) > 0) {
    add("error", "cases_missing_cols", .ic_msg(
      "cases missing column{?s}: {.val {case_missing}}"
    ))
  }

  has_month <- "month" %in% names(cases) && inherits(cases$month, "Date")
  if ("month" %in% names(cases) && !inherits(cases$month, "Date")) {
    add("error", "cases_month_class", "cases$month must be Date class")
  }
  if ("count" %in% names(cases) && !is.numeric(cases$count)) {
    add("error", "cases_count_type",
      "cases$count must be numeric (integer-valued)")
  }

  if ("count" %in% names(cases) && is.numeric(cases$count)) {
    n_neg <- sum(cases$count < 0, na.rm = TRUE)
    if (n_neg > 0) {
      add("error", "cases_negative", .ic_msg(
        "{n_neg} case row{?s} {?has/have} negative counts"
      ))
    }
  }

  if ("district_id" %in% names(cases) && has_month) {
    dupes <- cases |>
      dplyr::count(district_id, month) |>
      dplyr::filter(.data$n > 1L)
    if (nrow(dupes) > 0) {
      add("error", "cases_duplicates", .ic_msg(
        "{nrow(dupes)} duplicate district-month row{?s}"
      ))
    }
  }

  case_ids <- if ("district_id" %in% names(cases)) {
    unique(as.character(cases$district_id))
  } else {
    character(0)
  }

  # --- id reconciliation vs the shapefile ---------------
  if (length(case_ids) > 0 && length(shp_ids) > 0) {
    not_in_shp <- setdiff(case_ids, shp_ids)
    if (length(not_in_shp) > 0) {
      add("error", "cases_id_not_in_shapefile", .ic_msg(
        "{length(not_in_shp)} case district id{?s} not found in the \\
         shapefile: {.val {utils::head(not_in_shp, 5)}}"
      ), ids = not_in_shp)
    }

    no_cases <- setdiff(shp_ids, case_ids)
    if (length(no_cases) > 0) {
      add("warning", "shapefile_no_cases", .ic_msg(
        "{length(no_cases)} shapefile district{?s} with no case rows \\
         (no SPI produced): {.val {utils::head(no_cases, 5)}}"
      ), ids = no_cases)
    }
  }

  # --- panel gaps ---------------------------------------
  # Missing cells of the balanced district x month panel, returned as a tibble
  # so a fill helper can consume it. Whether a gap is a true zero or genuine
  # non-reporting is an epidemiological call left to the user.
  gaps <- tibble::tibble(
    district_id = character(0), month = as.Date(character(0))
  )
  if (length(case_ids) > 0 && has_month) {
    all_months <- sort(unique(cases$month))
    all_ids <- sort(case_ids)
    full <- tibble::tibble(
      district_id = rep(all_ids, times = length(all_months)),
      month = rep(all_months, each = length(all_ids))
    )
    gaps <- dplyr::anti_join(
      full,
      dplyr::distinct(cases, district_id, month),
      by = c("district_id", "month")
    )
    if (nrow(gaps) > 0) {
      add("warning", "panel_gaps", .ic_msg(
        "{nrow(gaps)} district-month{?s} missing from the panel (gaps)"
      ))
    }
  }

  # --- population: columns, values, coverage ------------
  pop_res <- .ic_rename_col(population, pop_col, "pop")
  population <- pop_res$data
  if (!pop_res$found) {
    add("error", "pop_missing_denominator", .ic_msg(
      "population missing denominator column: {.val {pop_col}}"
    ))
  }
  if (!"district_id" %in% names(population)) {
    add("error", "pop_missing_id", .ic_msg(
      "population missing column: {.val {id_col}}"
    ))
  }
  pop_has_month <- "month" %in% names(population) &&
    inherits(population$month, "Date")
  pop_has_year <- "year" %in% names(population) && is.numeric(population$year)
  if (!pop_has_month && !pop_has_year) {
    add("error", "pop_no_grain",
      "population needs a month (Date) or year (integer) column")
  }

  if (pop_res$found && is.numeric(population$pop)) {
    n_neg_pop <- sum(population$pop < 0, na.rm = TRUE)
    if (n_neg_pop > 0) {
      neg_ids <- unique(as.character(population$district_id[
        !is.na(population$pop) & population$pop < 0
      ]))
      add("error", "pop_negative", .ic_msg(
        "{n_neg_pop} population row{?s} {?has/have} negative {pop_col}"
      ), ids = neg_ids)
    }
    zero_ids <- unique(as.character(population$district_id[
      !is.na(population$pop) & population$pop == 0
    ]))
    if (length(zero_ids) > 0) {
      add("warning", "pop_zero", .ic_msg(
        "{length(zero_ids)} district{?s} with {pop_col} == 0 \\
         (dropped from the panel, not floored): \\
         {.val {utils::head(zero_ids, 5)}}"
      ), ids = zero_ids)
    }
  }

  # coverage of the case panel by population, aligning grain
  if (pop_res$found && length(case_ids) > 0 && has_month &&
        (pop_has_month || pop_has_year)) {
    cov <- .ic_coverage(cases, population, pop_has_month)
    if (cov$pct < 100) {
      add("warning", "pop_coverage", .ic_msg(
        "population covers {cov$pct}% of {cov$grain}"
      ))
    }
  }

  # --- covariate ids off the panel ----------------------
  if (!is.null(covariates) && "district_id" %in% names(covariates) &&
        length(case_ids) > 0) {
    cov_extra <- setdiff(
      unique(as.character(covariates$district_id)), case_ids
    )
    if (length(cov_extra) > 0) {
      add("warning", "covariate_off_panel", .ic_msg(
        "{length(cov_extra)} covariate district id{?s} not in the case \\
         panel: {.val {utils::head(cov_extra, 5)}}"
      ), ids = cov_extra)
    }
  }

  # --- informational note: adjacency components ---------
  ncomp <- if (inherits(shapefile, "nb")) attr(shapefile, "ncomp")$nc else NA
  if (!is.na(ncomp) && ncomp > 1) {
    add("note", "adjacency_components", .ic_msg(
      "adjacency has {ncomp} disconnected components"
    ))
  }

  # --- assemble -----------------------------------------
  issues_tbl <- if (length(issues) > 0) {
    dplyr::bind_rows(issues)
  } else {
    tibble::tibble(
      severity = character(0), code = character(0),
      message = character(0), n = integer(0), ids = list()
    )
  }
  # stable severity order for display: errors first, then warnings, notes
  sev_rank <- c(error = 1L, warning = 2L, note = 3L)
  issues_tbl <- issues_tbl[order(sev_rank[issues_tbl$severity]), ]

  n_months <- if (has_month) dplyr::n_distinct(cases$month) else NA_integer_
  date_range <- if (has_month && !is.na(n_months) && n_months > 0) {
    rng <- range(cases$month)
    .ic_msg("{format(rng[1], '%Y-%m')} to {format(rng[2], '%Y-%m')}")
  } else {
    NA_character_
  }

  out <- structure(
    list(
      issues = issues_tbl,
      gaps = gaps,
      n_error = sum(issues_tbl$severity == "error"),
      n_warning = sum(issues_tbl$severity == "warning"),
      n_note = sum(issues_tbl$severity == "note"),
      ok = !any(issues_tbl$severity == "error"),
      dims = list(
        n_districts = length(union(case_ids, shp_ids)),
        n_months = n_months,
        date_range = date_range
      ),
      id_col = id_col,
      pop_col = pop_col,
      call = match.call()
    ),
    class = "spi_input_check"
  )

  if (verbose) {
    print(out)
  }
  invisible(out)
}

#' @export
print.spi_input_check <- function(x, ...) {
  cli::cli_h1("spi input check")

  d <- x$dims
  dim_str <- if (!is.na(d$n_months)) {
    .ic_msg(
      "{format(d$n_districts, big.mark = ',')} districts x \\
       {format(d$n_months, big.mark = ',')} months"
    )
  } else {
    .ic_msg("{format(d$n_districts, big.mark = ',')} districts")
  }
  if (!is.na(d$date_range)) {
    dim_str <- .ic_msg("{dim_str} ({d$date_range})")
  }

  if (nrow(x$issues) == 0) {
    cli::cli_alert_success("All input checks passed -- {dim_str}.")
    return(invisible(x))
  }

  cli::cli_alert_info("{dim_str}")
  for (i in seq_len(nrow(x$issues))) {
    msg <- x$issues$message[i]
    switch(
      x$issues$severity[i],
      error = cli::cli_alert_danger("{msg}"),
      warning = cli::cli_alert_warning("{msg}"),
      note = cli::cli_alert_info("{msg}")
    )
  }

  cli::cli_rule()
  if (x$ok) {
    cli::cli_alert_success(
      "No errors -- a fit can proceed \\
       ({x$n_warning} warning{?s}, {x$n_note} note{?s})."
    )
  } else {
    cli::cli_alert_danger(
      "{x$n_error} error{?s} -- resolve before fitting."
    )
  }
  invisible(x)
}

# ---------------------------------------------------------------------------
# internal helpers
# ---------------------------------------------------------------------------

# Abort with the error-level issues from a spi_check_inputs() report, so a fit
# fails fast with the same graded detail the standalone check would show.
.abort_input_errors <- function(report, call = rlang::caller_env()) {
  errs <- report$issues[report$issues$severity == "error", ]
  bullets <- stats::setNames(errs$message, rep("x", nrow(errs)))
  cli::cli_abort(
    c(
      "Input validation failed with {report$n_error} error{?s}:",
      bullets,
      "i" = "Run {.fn spi_check_inputs} on these inputs for the full report."
    ),
    call = call
  )
}

# Interpolate an inline cli string, folding "\\"-newline continuations first.
# format_inline() alone keeps the newline; the cli container functions fold it,
# so we reproduce that here to keep long messages wrapped at source.
.ic_msg <- function(template, .envir = parent.frame()) {
  cli::format_inline(gsub("\\\\\n[ \t]*", "", template), .envir = .envir)
}

# Leave the frame untouched when `from` is absent, so the caller's own
# missing-column check is what reports it.
.ic_rename <- function(df, from, to) {
  if (from == to || !from %in% names(df)) {
    return(df)
  }
  if (to %in% names(df)) {
    df <- df[, setdiff(names(df), to), drop = FALSE]
  }
  names(df)[names(df) == from] <- to
  df
}

# Like .ic_rename() but reports whether the source column was found, so the
# caller can raise a graded issue for a missing denominator.
.ic_rename_col <- function(df, from, to) {
  found <- from %in% names(df) || to %in% names(df)
  list(data = .ic_rename(df, from, to), found = found)
}

# Percent of case district-cells the population table covers, aligning grain
# by deriving year from month when the population is annual.
.ic_coverage <- function(cases, population, pop_has_month) {
  if (pop_has_month) {
    case_combos <- dplyr::distinct(cases, district_id, month)
    pop_combos <- dplyr::distinct(population, district_id, month)
    covered <- dplyr::inner_join(
      case_combos, pop_combos,
      by = c("district_id", "month")
    )
    grain <- "district-months"
  } else {
    case_combos <- cases |>
      dplyr::mutate(year = lubridate::year(month)) |>
      dplyr::distinct(district_id, year)
    pop_combos <- dplyr::distinct(population, district_id, year)
    covered <- dplyr::inner_join(
      case_combos, pop_combos,
      by = c("district_id", "year")
    )
    grain <- "district-years"
  }
  list(
    pct = round(nrow(covered) / nrow(case_combos) * 100, 1),
    grain = grain
  )
}
