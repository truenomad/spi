#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' blindspot: Bayesian Spatiotemporal Surveillance Quality Monitoring
#'
#' @description
#' blindspot estimates district-level expected case detection rates using
#' Bayesian spatiotemporal models (BYM2 via INLA), adjusts for reporting
#' delays, monitors temporal trends via first-order random walk smoothing,
#' detects acute surveillance disruptions via Bayesian online changepoint
#' detection, and classifies surveillance quality into operational response
#' categories.
#'
#' @section Core framework:
#' The package implements a three-layer analytical framework:
#'
#' **Layer 1: Expected rate model**
#' - Poisson regression with BYM2 spatial random effects (INLA)
#' - Log person-time offset
#' - Estimates background detection rate from data rather than fixed thresholds
#' - Posterior draws of expected counts become SPI denominator
#'
#' **Layer 2: Delay adjustment and trend estimation**
#' - Truncation correction using empirical delay distributions
#' - First-order random walk (RW1) on monthly SPI
#' - Flags sustained decline before annual thresholds
#'
#' **Layer 3: Changepoint detection**
#' - Bayesian online changepoint detection (BOCPD)
#' - Detects acute disruptions within 1-2 months
#' - Complements gradual trend detection
#'
#' @section Key concepts:
#'
#' **Surveillance Performance Index (SPI)**
#' - Ratio of delay-adjusted observed counts to expected counts
#' - Full posterior uncertainty quantification
#' - Adapts standardised morbidity ratio (SMR) framework to surveillance monitoring
#'
#' **Operational classification**
#' - Green: adequate, no action required
#' - Amber-trend: declining, schedule supervision
#' - Amber-SPI: uncertain, investigate
#' - Red: degraded or acute disruption, immediate field investigation
#' - Structural: persistent zero with high expected count, expand network
#'
#' @section Main functions:
#'
#' **Core analysis functions:**
#' - [bs_expected()]: Fit BYM2 expected rate model
#' - [bs_spi()]: Compute surveillance performance index
#' - [bs_trend()]: Detect temporal trends
#' - [bs_changepoint()]: Detect acute disruptions
#' - [bs_classify()]: Composite operational classification
#'
#' **Data preparation:**
#' - [bs_fit_delay()]: Estimate delay distribution
#' - [bs_adjust()]: Delay-adjust recent counts
#' - [bs_adjacency()]: Build spatial neighbour matrix
#'
#' **Workflow functions:**
#' - [bs_pipeline()]: End-to-end analysis wrapper
#' - [bs_report()]: Generate Quarto desk review report
#'
#' **Utilities:**
#' - [bs_simulate()]: Generate synthetic surveillance data
#' - [bs_validate()]: Simulated degradation experiment
#' - [bs_diagnose()]: Model fit diagnostics
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
#' Core computational engine uses INLA for Bayesian inference. INLA is not
#' on CRAN and is distributed via \url{https://inla.r-inla-download.org/R/stable/}.
#' The package installation automatically configures the INLA repository.
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
#' blindspot replaces binary pass/fail threshold classifications with continuous,
#' uncertainty-quantified surveillance quality measures that map to specific
#' operational responses. The framework learns what each district should be
#' detecting from the data rather than imposing fixed targets.
#'
#' @docType package
#' @name blindspot-package
NULL
