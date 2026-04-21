#' Composite Operational Classification
#'
#' @description
#' Synthesises SPI categories, trend alerts, and changepoint alerts into
#' operational response categories.
#'
#' @param spi Object of class `blindspot_spi`.
#' @param trend Tibble from bs_trend() or NULL.
#' @param changepoints Tibble from bs_changepoint() or NULL.
#'
#' @return Tibble with district_id, month, classification (ordered factor),
#'   spi_median, spi_q05, spi_q95, trend_alert, cp_alert, recommended_action (chr).
#'
#' @details
#' **Classification rules:**
#' - green: adequate SPI, no alerts
#' - amber_trend: adequate SPI with trend alert
#' - amber_spi: uncertain SPI, no changepoint alert
#' - red: degraded SPI OR changepoint alert
#' - structural: persistent zero with high expected count
#'
#' @export
bs_classify <- function(spi,
                        trend = NULL,
                        changepoints = NULL) {
  cli::cli_abort("bs_classify() is not yet implemented. This is a stub function.")
}
