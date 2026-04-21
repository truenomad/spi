#' Estimate Onset-to-Notification Delay Distribution
#'
#' @description
#' Fits a parametric or empirical delay distribution from case-level timestamps.
#'
#' @param linelist Tibble with onset_date (Date), notification_date (Date).
#' @param distribution Character. "lognormal" (default), "gamma", "weibull", "empirical".
#' @param strata Character vector of column names to stratify by. Default: NULL.
#' @param truncation_days Numeric. Exclude delays beyond this. Default: 365.
#' @param zero_delay_action Character. "warn" (default), "exclude", "include".
#'
#' @return Object of class `blindspot_delay`.
#'
#' @export
bs_fit_delay <- function(linelist,
                         distribution = "lognormal",
                         strata = NULL,
                         truncation_days = 365,
                         zero_delay_action = "warn") {
  cli::cli_abort("bs_fit_delay() is not yet implemented. This is a stub function.")
}
