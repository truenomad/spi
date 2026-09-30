# Column names referenced inside data-masked dplyr verbs. Declaring them here
# keeps R CMD check's "no visible binding for global variable" note quiet
# without threading `.data$` through every call.
utils::globalVariables(c(
  "count", "district_id", "log_offset", "month", "month_num", "pop", "year"
))

#' spi: Bayesian Spatiotemporal Modelling of Relative AFP Reporting
#'
#' @description
#' Compares reported non-polio acute flaccid paralysis (NPAFP) cases with
#' expected counts based on earlier reporting and population size. Use
#' [spi_index()] for a spatial and temporal model fitted with INLA, or
#' [spi_direct()] for an annual calculation that does not require INLA.
#' The direct calculation multiplies each district's current population by
#' its own rate over all earlier years. It returns no SPI when that district
#' has no earlier year or no earlier NPAFP case.
#'
#' @section Interpreting SPI:
#' The surveillance performance index (SPI) divides reported cases by
#' expected cases. By default, it then divides the district ratio by the
#' national ratio for the same period. SPI below 1 means the district's
#' ratio is below the national ratio. With `centre = "none"` in
#' [spi_index()], SPI below 1 means the district reported fewer cases than
#' its own expected count.
#'
#' The model-based SPI includes credible intervals for uncertainty in
#' expected counts. Reported counts are held fixed in these intervals.
#' Neither version of SPI estimates missed cases or establishes whether
#' poliovirus is present. Read SPI alongside the NPAFP rate, timeliness,
#' and stool adequacy.
#'
#' @section District review:
#' For model-based results, [spi_compare_npafp()] groups district-years by whether SPI is below the
#' chosen threshold and whether the NPAFP rate meets the programme target.
#' [spi_field_guide()] then uses the five STEPS components: strength,
#' timeliness, extent, persistence, and stool adequacy.
#'
#' The review labels are `Priority for review`, `Monitor`, and
#' `No SPI indication`. They help organise review; they do not establish
#' surveillance quality. Timeliness and stool adequacy are reported
#' separately and do not change the label.
#'
#' @section Main functions:
#' - [spi_check_inputs()]: Check case counts, population, and boundaries.
#' - [spi_adjacency()]: Identify neighbouring districts.
#' - [spi_expected()]: Estimate expected counts with INLA.
#' - [spi_compare_overdispersion()]: Compare models for count variation.
#' - [spi_index()]: Calculate model-based SPI and credible intervals.
#' - [spi_direct()]: Calculate annual SPI from earlier reporting.
#' - [spi_direct_explain()]: Show one district's calculation.
#' - [spi_compare_npafp()]: Compare SPI with the NPAFP target.
#' - [spi_field_guide()]: Review districts using STEPS.
#' - [spi_field_guide_table()]: Create a table of review findings.
#' - [spi_field_guide_pager()]: Create a one-page district report.
#' - [spi_field_guide_help()]: Read explanations and worked examples.
#'
#' @section Other surveillance data:
#' The package is designed for AFP surveillance. Other applications need
#' suitable case counts, population estimates, and model settings.
#'
#' @section Dependencies:
#' Model fitting requires INLA, available from
#' \url{https://inla.r-inla-download.org/R/stable/}.
#' [spi_direct()] requires neither INLA nor district boundaries.
#'
#' @section Citation:
#' Run `citation("spi")` for the package citation.
#'
#' @keywords internal
#' @importFrom rlang .data .env :=
"_PACKAGE"
