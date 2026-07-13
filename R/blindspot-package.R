#' @keywords internal
#' @importFrom rlang .data .env :=
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Column names referenced inside data-masked dplyr verbs. Declaring them here
# keeps R CMD check's "no visible binding for global variable" note quiet
# without threading `.data$` through every call.
utils::globalVariables(c(
  "count", "district_id", "log_offset", "month", "month_num", "pop", "year"
))

#' blindspot: Bayesian Spatiotemporal Surveillance Quality Monitoring
#'
#' @description
#' blindspot estimates district-level expected case detection rates using
#' Bayesian spatiotemporal models (BYM2 via INLA) and classifies surveillance
#' quality into operational response categories.
#'
#' @section Core framework:
#'
#' **Expected rate model**
#' - Poisson regression with BYM2 spatial random effects (INLA)
#' - Log person-time offset
#' - Estimates background detection rate from data rather than fixed thresholds
#' - Posterior draws of expected counts become SPI denominator
#'
#' @section Key concepts:
#'
#' **Surveillance Performance Index (SPI)**
#' - Ratio of observed counts to expected counts
#' - Full posterior uncertainty quantification
#' - Adapts standardised morbidity ratio (SMR) framework to surveillance
#'   monitoring
#'
#' **Concordance and interpretation**
#' - Cross-classify SPI against the conventional NPAFP-rate threshold into a
#'   four-cell table (both adequate / true shortfall / false alarm / false
#'   reassurance).
#' - Read each district-year through the seven-signal field guide (S1-S7) to a
#'   FLAG / WATCH / No-action verdict.
#'
#' @section Main functions:
#'
#' - [bs_adjacency()]: Build spatial neighbour matrix
#' - [bs_expected()]: Fit BYM2 expected rate model
#' - [bs_compare_overdispersion()]: Compare Poisson vs negative-binomial fits
#' - [bs_spi()]: Compute surveillance performance index
#' - [bs_concordance()]: Cross-classify SPI against the NPAFP-rate threshold
#' - [bs_field_guide()]: Read the SPI to FLAG / WATCH / No-action verdicts
#' - [bs_field_guide_table()]: Render the field guide (gt / flextable)
#' - [bs_field_guide_help()]: Learn to read the field guide
#'
#' @section Design principles:
#' 1. One function, one job
#' 2. No side effects
#' 3. Tibbles in, tibbles out
#' 4. Fail early, fail clearly
#' 5. Uncertainty is first-class output
#' 6. Sensible defaults, full control
#' 7. Disease-agnostic core
#' 8. Consistent namespace (bs_ prefix)
#' 9. Reproducibility by default
#' 10. Progressive disclosure
#'
#' @section Applications:
#' Designed for AFP/polio surveillance but applicable to any case-based
#' disease surveillance system:
#' - Acute flaccid paralysis (AFP) surveillance
#' - Measles discard surveillance
#' - Neonatal tetanus reporting
#' - DHIS2 completeness monitoring
#' - AEFI pharmacovigilance
#'
#' @section Dependencies:
#' Core computational engine uses INLA for Bayesian inference. INLA is not on
#' CRAN and is distributed via
#' \url{https://inla.r-inla-download.org/R/stable/}. The package installation
#' automatically configures the INLA repository.
#'
#' @section Citation:
#' If you use blindspot in published work, please cite:
#'
#' Yusuf MA, Ahmed J (2026). blindspot: Bayesian spatiotemporal surveillance
#' quality monitoring. R package version 0.1.0.
#' \url{https://github.com/truenomad/blindspot}
#'
#' Associated paper:
#' Yusuf MA, et al. (2026). From threshold to model: a Bayesian spatiotemporal
#' framework for AFP surveillance quality monitoring in the polio endgame.
#' Lancet Global Health.
#'
#' @section Package philosophy:
#' blindspot replaces binary pass/fail threshold classifications with
#' continuous, uncertainty-quantified surveillance quality measures that map to
#' specific operational responses. The framework learns what each district
#' should be
#' detecting from the data rather than imposing fixed targets.
#'
#' @docType package
#' @name blindspot-package
NULL
