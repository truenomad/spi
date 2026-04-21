#' Detect Temporal Trends in SPI
#'
#' @description
#' Fits INLA first-order random walk (RW1) on monthly SPI to estimate trends
#' and flag sustained decline.
#'
#' @param spi Object of class `blindspot_spi`.
#' @param prior_precision Numeric vector c(U, alpha) for RW1 PC prior. Default: c(0.1, 0.01).
#' @param alert_prob Numeric. P(slope < 0) threshold for alert. Default: 0.90.
#' @param alert_months Integer. Consecutive months above threshold required. Default: 3L.
#' @param min_months Integer. Minimum months of data per district. Default: 12L.
#' @param verbose Logical. Default: TRUE.
#'
#' @return Tibble with district_id, month, trend_median, trend_q05, trend_q95,
#'   slope_median, slope_q05, slope_q95, p_declining, consecutive_declining,
#'   trend_alert (logical).
#'
#' @export
bs_trend <- function(spi,
                     prior_precision = c(0.1, 0.01),
                     alert_prob = 0.90,
                     alert_months = 3L,
                     min_months = 12L,
                     verbose = TRUE) {
  cli::cli_abort("bs_trend() is not yet implemented. This is a stub function.")
}
