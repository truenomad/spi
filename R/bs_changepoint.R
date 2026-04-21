#' Detect Acute Surveillance Disruptions
#'
#' @description
#' Runs Bayesian online changepoint detection (BOCPD) on district monthly counts.
#'
#' @param cases Tibble with district_id, month, count.
#' @param model Character. "poisson" (default) or "normal".
#' @param alert_prob Numeric. Posterior probability threshold. Default: 0.80.
#' @param alert_window Integer. Months within which changepoint must occur. Default: 2L.
#' @param direction Character. "down" (default), "up", "both".
#' @param hazard Numeric. Prior hazard rate. Default: 1/120.
#' @param min_months Integer. Minimum months before alerting. Default: 6L.
#'
#' @return Tibble with district_id, month, cp_probability, run_length_map,
#'   cp_alert (logical), cp_direction (chr).
#'
#' @export
bs_changepoint <- function(cases,
                           model = "poisson",
                           alert_prob = 0.80,
                           alert_window = 2L,
                           direction = "down",
                           hazard = 1/120,
                           min_months = 6L) {
  cli::cli_abort("bs_changepoint() is not yet implemented. This is a stub function.")
}
