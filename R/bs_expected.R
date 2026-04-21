#' Fit BYM2 Expected Rate Model
#'
#' @description
#' Fits an INLA BYM2 Poisson model to estimate expected case counts per
#' district-month. This is the core engine of the blindspot framework.
#'
#' @param cases Tibble with columns: `district_id` (character), `month` (Date),
#'   `count` (integer). One row per district-month. Zero-count rows must be
#'   present explicitly.
#' @param population Tibble with columns: `district_id` (character),
#'   `year` (integer) OR `month` (Date), `pop` (double). If year-level,
#'   constant within year. Monthly overrides annual if both present.
#' @param adjacency nb object OR sf object. If sf, adjacency computed internally
#'   via [bs_adjacency()]. district_id values must match `cases$district_id`.
#' @param covariates Tibble with columns: `district_id`, `month` (Date) OR
#'   `year` (int), plus one or more numeric covariate columns. If NULL, model
#'   uses offset + intercept + season + spatial. Covariates are standardised
#'   internally (mean=0, sd=1). Default: NULL.
#' @param season Character. Seasonal specification: "harmonic" (1st + 2nd order
#'   sin/cos, 4 terms), "rw2" (cyclic 2nd-order random walk, 12 knots),
#'   "monthly" (12 monthly fixed effects, January omitted), "none" (no seasonal
#'   component). Default: "harmonic".
#' @param overdispersion Character. Overdispersion mechanism: "iid" (Poisson-lognormal,
#'   iid N(0, sigma^2) on log scale per district-month, recommended), "nb"
#'   (negative binomial likelihood), "none" (plain Poisson, not recommended for
#'   sparse data). Default: "iid".
#' @param prior_phi Numeric vector of length 2. BYM2 mixing parameter PC prior:
#'   P(phi < U) = alpha. Default: c(0.5, 0.5) (agnostic, 50% chance phi below 0.5).
#' @param prior_precision Numeric vector of length 2. Marginal SD PC prior:
#'   P(1/sqrt(tau) > U) = alpha. Default: c(1, 0.01) (1% chance SD exceeds 1
#'   on log scale).
#' @param n_draws Integer. Posterior draws from joint latent field, used for
#'   SPI uncertainty propagation downstream. Use 1000 for analysis, 100 for
#'   quick checks. Default: 1000L.
#' @param log_transform Character vector. Covariate column names to log(1 + x)
#'   transform. Typical: c("facility_index", "conflict_events"). Default: NULL.
#' @param country_id Character. Column name in cases for country grouping. If
#'   provided, separate model per country. If NULL, single model across all
#'   districts. Default: NULL.
#' @param verbose Logical. Progress messages via cli. FALSE for batch jobs.
#'   Default: TRUE.
#' @param seed Integer. Random seed for `inla.posterior.sample()`. Default: NULL.
#'
#' @return Object of class `blindspot_expected`. A list containing:
#' \describe{
#'   \item{draws}{Matrix [n_draws x n_district_months] of posterior draws of exp(eta)}
#'   \item{summary}{Tibble with district_id, month, expected_median, expected_q05,
#'     expected_q10, expected_q90, expected_q95, expected_mean}
#'   \item{formula}{INLA formula used}
#'   \item{model}{Fitted INLA object}
#'   \item{adjacency}{spdep nb object}
#'   \item{hyperparameters}{Tibble with posterior summaries of tau, phi, sigma}
#'   \item{priors}{List of prior specs used}
#'   \item{data}{Input data tibble (for downstream functions)}
#'   \item{call}{Matched call}
#' }
#'
#' @details
#' **Model specification:**
#' The expected count model is a Poisson regression with:
#' - Log person-time offset
#' - BYM2 spatial random effects (Riebler et al. 2016)
#' - Optional seasonal component
#' - Optional overdispersion (iid or negative binomial)
#' - Optional covariates
#'
#' **Input validation:**
#' Executed before INLA runs:
#' 1. Required columns present and correct types
#' 2. No negative counts or NAs
#' 3. All district_ids in cases appear in adjacency
#' 4. No duplicate district_id x month combinations
#' 5. Population coverage check
#' 6. Adjacency graph connectivity check
#' 7. Covariate validation
#'
#' **Computational notes:**
#' - Posterior sampling can be time-consuming for large datasets
#' - Use fewer draws for initial exploration
#' - INLA uses sparse matrix methods for efficiency
#'
#' @references
#' Riebler A, et al. (2016). An intuitive Bayesian spatial model for disease
#' mapping that accounts for scaling. Statistical Methods in Medical Research,
#' 25(4), 1145-1165.
#'
#' @seealso [bs_spi()], [bs_adjacency()], [bs_pipeline()]
#'
#' @export
#' @examples
#' \dontrun{
#' data(synth_surveillance, package = "blindspot")
#'
#' fit <- bs_expected(
#'   cases = synth_surveillance$cases,
#'   population = synth_surveillance$population,
#'   adjacency = synth_surveillance$boundaries,
#'   covariates = synth_surveillance$covariates,
#'   season = "harmonic",
#'   seed = 42L
#' )
#'
#' print(fit)
#' plot(fit, type = "expected_vs_observed")
#' }
bs_expected <- function(cases,
                        population,
                        adjacency,
                        covariates = NULL,
                        season = "harmonic",
                        overdispersion = "iid",
                        prior_phi = c(0.5, 0.5),
                        prior_precision = c(1, 0.01),
                        n_draws = 1000L,
                        log_transform = NULL,
                        country_id = NULL,
                        verbose = TRUE,
                        seed = NULL) {

  # Function implementation will go here
  cli::cli_abort("bs_expected() is not yet implemented. This is a stub function.")
}
