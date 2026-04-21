#' End-to-End Surveillance Quality Pipeline
#'
#' @description
#' Chains all analysis steps from raw data to classified, actionable output.
#' One function call: adjacency → expected → delay → adjust → spi → trend →
#' changepoint → classify.
#'
#' @param cases Tibble with district_id, month, count (and optionally onset_date,
#'   notification_date for delay estimation).
#' @param population Tibble with district_id, year/month, pop.
#' @param boundaries sf object or nb object for spatial adjacency.
#' @param covariates Tibble with district_id, month/year, plus covariate columns.
#'   Default: NULL.
#' @param period Date vector c(start, end) to filter cases. Default: NULL.
#' @param linelist Tibble with onset_date, notification_date for delay fitting.
#'   If NULL, delay estimated from cases (requires onset_date + notification_date
#'   in cases). Default: NULL.
#' @param extraction_date Date when data were extracted. Default: Sys.Date().
#' @param skip_trend Logical. Skip RW1 trend analysis (faster). Default: FALSE.
#' @param skip_cp Logical. Skip BOCPD changepoint detection (faster). Default: FALSE.
#' @param seed Integer for reproducibility. Default: NULL.
#' @param verbose Logical. Progress messages. Default: TRUE.
#' @param ... Additional arguments passed to component functions.
#'
#' @return Object of class `blindspot_result`. A list containing:
#' \describe{
#'   \item{spi}{blindspot_spi object}
#'   \item{trend}{Tibble from bs_trend() or NULL if skipped}
#'   \item{changepoints}{Tibble from bs_changepoint() or NULL if skipped}
#'   \item{classified}{Tibble from bs_classify() with composite classification}
#'   \item{model}{blindspot_expected object}
#'   \item{delay}{blindspot_delay object}
#'   \item{metadata}{List with call, timestamp, versions, n_districts, n_months,
#'     countries, seed}
#' }
#'
#' @details
#' This function provides the simplest entry point to the blindspot framework.
#' For users who want control over individual steps, use the component functions
#' directly.
#'
#' **Workflow:**
#' 1. Filter cases to period if specified
#' 2. Build or validate adjacency matrix
#' 3. Fit expected rate model
#' 4. Estimate delay distribution
#' 5. Adjust recent counts for delays
#' 6. Compute SPI with uncertainty
#' 7. Detect temporal trends (optional)
#' 8. Detect changepoints (optional)
#' 9. Composite classification
#' 10. Package with metadata
#'
#' @seealso [bs_expected()], [bs_spi()], [bs_classify()], [bs_report()]
#'
#' @export
#' @examples
#' \dontrun{
#' data(synth_surveillance, package = "blindspot")
#'
#' result <- bs_pipeline(
#'   cases = synth_surveillance$cases,
#'   population = synth_surveillance$population,
#'   boundaries = synth_surveillance$boundaries,
#'   covariates = synth_surveillance$covariates,
#'   period = c("2020-01-01", "2024-12-31"),
#'   seed = 42L
#' )
#'
#' print(result)
#' summary(result)
#' plot(result)
#' }
bs_pipeline <- function(cases,
                        population,
                        boundaries,
                        covariates = NULL,
                        period = NULL,
                        linelist = NULL,
                        extraction_date = Sys.Date(),
                        skip_trend = FALSE,
                        skip_cp = FALSE,
                        seed = NULL,
                        verbose = TRUE,
                        ...) {

  # Function implementation will go here
  cli::cli_abort("bs_pipeline() is not yet implemented. This is a stub function.")
}
