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

#' spi: Bayesian Spatiotemporal Modelling of Relative AFP Reporting
#'
#' @description
#' spi estimates district-level expected reported case counts using
#' Bayesian spatiotemporal models (BYM2 via INLA) and summarises reporting
#' shortfalls for surveillance review.
#'
#' @section Model:
#'
#' **Expected rate model**
#' - Negative binomial or Poisson regression with BYM2 spatial effects (INLA)
#' - Log person-time offset
#' - Estimates background reported NPAFP rate from data rather than fixed thresholds
#' - Posterior draws of expected counts become SPI denominator
#'
#' @section Key concepts:
#'
#' **Surveillance Performance Index (SPI)**
#' - Ratio of observed to expected counts, nationally centred by default
#' - Credible intervals for uncertainty in expected counts
#' - Adapts standardised morbidity ratio (SMR) framework to surveillance
#'   monitoring
#'
#' **Concordance and interpretation**
#' - Cross-classify SPI against the conventional NPAFP-rate threshold into a
#'   four-cell table (neither flagged / SPI only / NPAFP only / both flagged).
#' - Review each district-year with an SPI below 1 through the five STEPS
#'   components (strength, timeliness, extent, persistence, stool adequacy)
#'   to a review priority / monitor / no SPI indication review label. These labels do not establish surveillance adequacy.
#'
#' @section Main functions:
#'
#' - [spi_adjacency()]: Build a neighbour graph
#' - [spi_expected()]: Fit expected case counts
#' - [spi_compare_overdispersion()]: Compare Poisson vs negative-binomial fits
#' - [spi_index()]: Compute surveillance performance index
#' - [spi_concordance()]: Cross-classify SPI against the NPAFP-rate threshold
#' - [spi_field_guide()]: Review a low SPI through the five STEPS components
#' - [spi_field_guide_table()]: Render the field guide (gt / flextable)
#' - [spi_field_guide_help()]: Learn to read the field guide
#'
#' @section Applications:
#' Designed for acute flaccid paralysis surveillance. Other case-based
#' surveillance applications need suitable counts, population denominators,
#' and model settings.
#'
#' @section Dependencies:
#' Model fitting uses INLA for Bayesian inference. INLA is not on
#' CRAN and is distributed via
#' \url{https://inla.r-inla-download.org/R/stable/}. The package installation
#' automatically configures the INLA repository.
#'
#' @section Citation:
#' If you use spi in published work, please cite:
#'
#' Yusuf MA, Ahmed J (2026). spi: Bayesian spatiotemporal modelling of
#' relative AFP reporting. R package version 0.1.0.
#' \url{https://github.com/truenomad/spi}
#'
#' Associated paper:
#' Yusuf MA, et al. (2026). From threshold to model: a Bayesian spatiotemporal
#' framework for AFP surveillance quality monitoring in the polio endgame.
#' Lancet Global Health.
#'
#' @section Interpretation:
#' Read the SPI alongside conventional reporting targets and other surveillance
#' indicators. With the default national centring, a low SPI indicates reporting below the
#' national pattern. With `centre = "none"`, it indicates reporting below the
#' district's own modelled expectation. Neither establishes the cause of the
#' shortfall or shows whether poliovirus is present.
#'
#' @docType package
#' @name spi-package
NULL
