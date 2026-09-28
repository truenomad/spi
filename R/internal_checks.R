#' Check that one or more suggested packages are installed
#'
#' Thin wrapper around [rlang::check_installed()] used throughout the package
#' to report missing packages (with an error message and an interactive
#' install prompt) when a suggested dependency is missing. Prefer this over
#' bare `requireNamespace()` checks so users learn about missing dependencies
#' at the start of a function rather than partway through an analysis.
#'
#' @param pkg Character vector of package names to check.
#' @param reason Short string explaining *why* the package is needed (used in
#'   the error message). Optional but strongly recommended.
#' @param call Calling environment, forwarded to [rlang::check_installed()] so
#'   the error is attributed to the user-facing function.
#'
#' @return `TRUE` invisibly if all packages are available. Errors otherwise.
#' @keywords internal
#' @noRd
.check_pkg <- function(pkg, reason = NULL, call = rlang::caller_env()) {
  rlang::check_installed(pkg, reason = reason, call = call)
  invisible(TRUE)
}

#' Attach administrative name columns before the id column
#'
#' Left-joins the standard admin-hierarchy name columns
#' (`adm0_name`/`adm1_name`/`adm2_name`) from a boundaries layer onto a
#' district-keyed table and relocates them to sit immediately before `id_col`,
#' so saved outputs carry human-readable labels next to the district id. A
#' no-op when `boundaries` is NULL or carries none of the name columns.
#'
#' @param df Data frame keyed by `id_col`.
#' @param boundaries `sf` object or data frame carrying `id_col` and one or
#'   more name columns. If NULL, `df` is returned unchanged.
#' @param id_col Name of the district id column, present in both `df` and
#'   `boundaries`.
#' @param cols Optional character vector naming which label columns to attach.
#'   Defaults to whichever of adm0/adm1/adm2_name are present in `boundaries`.
#' @return `df` with the label columns joined and moved before `id_col`.
#' @noRd
.attach_admin_labels <- function(df, boundaries, id_col, cols = NULL) {
  if (is.null(boundaries)) {
    return(df)
  }
  bnd <- boundaries
  if (inherits(bnd, "sf")) {
    .check_pkg("sf", reason = "to read admin names from an sf boundaries layer")
    bnd <- sf::st_drop_geometry(bnd)
  }
  if (!id_col %in% names(bnd)) {
    cli::cli_abort(
      "{.arg boundaries} must contain the id column {.val {id_col}}."
    )
  }
  label_cols <- cols %||%
    intersect(c("adm0_name", "adm1_name", "adm2_name"), names(bnd))
  # never re-attach the key or a column df already carries
  label_cols <- setdiff(label_cols, c(id_col, names(df)))
  if (length(label_cols) == 0L) {
    return(df)
  }

  df <- dplyr::left_join(df, bnd[, c(id_col, label_cols)], by = id_col)
  dplyr::relocate(
    df, dplyr::all_of(label_cols), .before = dplyr::all_of(id_col)
  )
}

#' validate cases tibble
#' @noRd
.validate_cases <- function(cases) {
  required <- c("district_id", "month", "count")
  missing <- setdiff(required, names(cases))

  if (length(missing) > 0) {
    cli::cli_abort("cases missing columns: {.val {missing}}")
  }

  if (!inherits(cases$month, "Date")) {
    cli::cli_abort("cases$month must be Date class")
  }

  if (!is.numeric(cases$count)) {
    cli::cli_abort("cases$count must be numeric (integer-valued)")
  }

  if (any(cases$count < 0, na.rm = TRUE)) {
    cli::cli_abort("cases$count has negative values")
  }
  known <- cases$count[!is.na(cases$count)]
  if (any(!is.finite(known) | known != floor(known))) {
    cli::cli_abort("cases$count must contain finite, integer-valued counts or NA")
  }

  # check for duplicates
  dupes <- cases |>
    dplyr::group_by(district_id, month) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  if (nrow(dupes) > 0) {
    cli::cli_abort("{nrow(dupes)} duplicate district-month rows")
  }

  invisible(TRUE)
}

#' validate population tibble (year-level OR month-level)
#' @noRd
.validate_population <- function(population, cases) {
  has_month <- "month" %in% names(population) &&
    inherits(population$month, "Date")
  has_year <- "year" %in% names(population) &&
    is.numeric(population$year)

  if (!has_month && !has_year) {
    cli::cli_abort(
      "population must have either a {.field month} (Date) column \\
       or a {.field year} (integer) column"
    )
  }

  required <- c("district_id", "pop")
  missing <- setdiff(required, names(population))
  if (length(missing) > 0) {
    cli::cli_abort("population missing columns: {.val {missing}}")
  }

  # coverage check: align grain by deriving year from month when needed
  if (has_month) {
    pop_combos <- population |>
      dplyr::distinct(district_id, month)

    case_combos <- cases |>
      dplyr::distinct(district_id, month)

    coverage <- dplyr::inner_join(
      case_combos, pop_combos,
      by = c("district_id", "month")
    )
    grain <- "district-months"
  } else {
    pop_combos <- population |>
      dplyr::distinct(district_id, year)

    case_combos <- cases |>
      dplyr::mutate(year = lubridate::year(month)) |>
      dplyr::distinct(district_id, year)

    coverage <- dplyr::inner_join(
      case_combos, pop_combos,
      by = c("district_id", "year")
    )
    grain <- "district-years"
  }

  pct <- round(nrow(coverage) / nrow(case_combos) * 100, 1)

  if (pct < 80) {
    cli::cli_abort("population covers only {pct}% of {grain}")
  }

  if (pct < 100) {
    cli::cli_alert_warning("population covers {pct}% of {grain}")
  }

  invisible(TRUE)
}
