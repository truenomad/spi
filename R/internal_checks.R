#' Check that one or more suggested packages are installed
#'
#' Thin wrapper around [rlang::check_installed()] used throughout the package
#' to fail fast (with a clear, actionable error message and an interactive
#' install prompt) when a suggested dependency is missing. Prefer this over
#' bare `requireNamespace()` checks so users learn about missing dependencies
#' at the start of a function rather than several minutes into a workflow.
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
