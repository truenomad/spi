#' Compute Surveillance Performance Index
#'
#' @description
#' Computes the surveillance performance index (SPI) as the ratio of
#' delay-adjusted observed counts to posterior draws of expected counts.
#' Returns full posterior distribution plus operational categories.
#'
#' @param expected Object of class `blindspot_expected` from [bs_expected()].
#' @param adjusted Tibble from [bs_adjust()] with delay-adjusted counts. If NULL,
#'   uses raw counts from expected$data as numerator (no delay correction).
#'   Default: NULL.
#' @param categories Named numeric vector of SPI thresholds for classification.
#'   Default: c(adequate = 0.8, uncertain = 0.5, degraded = 0). Can override
#'   with ROC-derived thresholds from [bs_validate()].
#' @param structural_months Integer. Trailing window of consecutive zero counts
#'   to trigger structural gap classification. Default: 24L.
#' @param structural_expected Numeric. Minimum expected count for structural gap.
#'   District must have expected >= this AND zero observed for structural_months
#'   consecutive months. Default: 3.0.
#'
#' @return Object of class `blindspot_spi`. A list containing:
#' \describe{
#'   \item{spi}{Tibble with district_id, month, observed, adjusted, expected_median,
#'     spi_median, spi_q05, spi_q10, spi_q90, spi_q95, category, p_adequate, p_degraded}
#'   \item{draws}{Matrix [n_draws x n_district_months] of SPI posterior draws}
#'   \item{categories}{Named numeric vector of thresholds used}
#' }
#'
#' @details
#' **SPI definition:**
#' SPI = (delay-adjusted observed count) / (expected count)
#'
#' The denominator uses posterior draws of the latent expected rate (exponentiated
#' linear predictor from INLA), NOT draws from the predictive distribution.
#' This ensures the denominator reflects uncertainty in the expected rate rather
#' than observation noise from the Poisson count process.
#'
#' **Categories:**
#' - Adequate: SPI >= 0.8
#' - Uncertain: 0.5 <= SPI < 0.8
#' - Degraded: SPI < 0.5
#' - Structural: consecutive zeros with high expected count
#'
#' **Structural gaps:**
#' Districts with >= structural_months consecutive zero counts AND
#' expected count >= structural_expected are flagged for network expansion.
#'
#' @references
#' The SPI adapts the standardised morbidity ratio (SMR) framework from
#' disease mapping to surveillance process monitoring.
#'
#' @seealso [bs_expected()], [bs_adjust()], [bs_classify()]
#'
#' @export
#' @examples
#' \dontrun{
#' # After fitting expected model
#' spi <- bs_spi(
#'   expected = fit,
#'   adjusted = adjusted_counts,
#'   categories = c(adequate = 0.8, uncertain = 0.5, degraded = 0)
#' )
#'
#' print(spi)
#' plot(spi, type = "spatial")
#' }
bs_spi <- function(expected,
                   adjusted = NULL,
                   categories = NULL,
                   structural_months = 24L,
                   structural_expected = 3.0) {

  # Function implementation will go here
  cli::cli_abort("bs_spi() is not yet implemented. This is a stub function.")
}
