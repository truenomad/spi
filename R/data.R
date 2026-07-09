#' Synthetic AFP surveillance toy dataset
#'
#' A fully self-contained toy dataset that lets a user run the entire
#' `blindspot` chain -- `bs_adjacency()` -> `bs_expected()` -> `bs_spi()` ->
#' `bs_concordance()` -- without any real surveillance data. Every value is
#' simulated; no personally identifying or operationally sensitive information
#' is present.
#'
#' @format A named list with nine elements:
#' \describe{
#'   \item{cases}{Tibble, 12000 x 3. Columns: `adm2_guid` (character district
#'     id, `"GDR-001"`..`"GDR-100"`), `month` (Date, first-of-month), `count`
#'     (integer). Full 100 x 120 district-month grid with zero counts
#'     explicit.}
#'   \item{population}{Tibble, 1000 x 3. Columns: `adm2_guid`, `year`
#'     (integer, 2015-2024), `pop_u15` (numeric, under-15 population).
#'     Annual, grown at 2% per year from a lognormal baseline.}
#'   \item{boundaries}{`sf` object, 100 x 5 (+ geometry). Columns: `adm2_guid`,
#'     `adm2_name` (canonical or Sindarin-flavoured Gondor toponyms), `adm1_name`
#'     (one of `"Belfalas"`, `"Lossarnach"`, `"Lebennin"`, `"Anorien"`,
#'     `"Ithilien"`), `adm0_name` (`"Gondor"`), `geometry` (POLYGON, EPSG:4326).
#'     The 100 polygons are the North Carolina counties shipped with the
#'     `sf` package (Cressie's classic Bayesian-spatial teaching dataset) --
#'     a real, convincing shape base -- with Gondor labels overlaid so the
#'     data itself is unambiguously fictional.}
#'   \item{ward_boundaries}{`sf` object, ~640 x 6 (+ geometry). Columns:
#'     `adm3_guid` (`"GDR-001-W01"`..), `adm3_name` (parent adm2 name plus a
#'     suffix from a pool of directional or geographic descriptors --
#'     e.g. `"Osgiliath North Ward"`, `"Dol Amroth Ford"`), `adm2_guid`,
#'     `adm2_name`, `adm1_name`, `adm0_name`, `geometry`. Each adm2 is
#'     subdivided into 5-8 wards by nested Voronoi tessellation.}
#'   \item{virus_outcome}{Tibble, 1000 x 5. Columns: `adm2_guid`, `year`,
#'     `any_wpv1` (integer, 0/1; always 0 in this toy), `any_cvdpv2`
#'     (integer, 0/1), `any_virus` (integer, 0/1). Positive district-years
#'     are drawn one year after a planted blindspot activates, at rate
#'     `plogis(1)`; background rate `plogis(-4)`. Retained as an ad-hoc
#'     validation anchor even though the framework is descriptive, not
#'     predictive.}
#'   \item{es_sites}{`sf` object, 30 x 5 (+ geometry). Environmental
#'     surveillance sentinel sites. Columns: `es_site_id`, `site_name`,
#'     `adm3_guid`, `adm2_guid`, `adm1_name`, `geometry` (POINT). One site per
#'     selected district, placed at the centroid of a random ward.}
#'   \item{es_data}{Tibble, ~3600 x 4. Monthly ES sample-level rows. Columns:
#'     `es_site_id`, `adm2_guid`, `sample_date` (Date, first-of-month),
#'     `positive_cvdpv2` (integer, 0/1). Positivity probability shares the
#'     same true-vs-observed gap signal as `virus_outcome`, so the two ES
#'     proxies are internally consistent.}
#'   \item{es_district_year}{Tibble, 300 x 4. District-year ES rollup.
#'     Columns: `adm2_guid`, `year`, `n_samples`, `n_positive`.}
#'   \item{truth}{Tibble, 100 x 4. Ground-truth cheat sheet. Columns:
#'     `adm2_guid`, `is_blindspot` (logical), `planted_onset` (Date, NA
#'     when not a blindspot), `detection_ratio` (numeric, 1.0 for healthy
#'     districts, 0.2-0.4 for planted blindspots after their onset month).}
#' }
#'
#' @section Data-generating process:
#' Cases are drawn from a negative-binomial likelihood with mean
#' \deqn{\log \mu_{it} = \alpha + b_i + u_t + s(m_t) + \log(\mathrm{pop}_{u15,it} / 10^5),}
#' where `b_i` is a BYM2 spatial random effect (mixing parameter 0.6,
#' marginal precision 4), `u_t` is an iid annual random effect, and
#' `s(m_t)` is a 1st-order harmonic seasonal term. Ten districts are
#' planted as blindspots, each with a multiplicative detection ratio in
#' `{0.2, 0.3, 0.4}` activated at a random month in the second half of the
#' study window. `virus_outcome` marks the year after each blindspot
#' activates as cVDPV2-positive at elevated rate; kept as an optional
#' ground-truth anchor for validation-style diagnostics.
#'
#' @section Fictional geography:
#' Place names are drawn from J.R.R. Tolkien's Kingdom of Gondor -- five
#' canonical provinces (Belfalas, Lossarnach, Lebennin, Anorien, Ithilien)
#' populated with canonical toponyms (Osgiliath, Dol Amroth, Cair Andros,
#' Minas Ithil, Emyn Arnen, ...) and Sindarin-flavoured compounds
#' (`amon-`, `nen-`, `rath-`, `naith-` roots) where the canon runs out.
#' Purely for demonstrative colour; no lore relevance to surveillance.
#'
#' @section Attribution:
#' Geometry: 100 North Carolina counties from `sf::st_read(system.file(
#' "shape/nc.shp", package = "sf"))`. This is Cressie's classic
#' Bayesian-spatial-stats teaching dataset (Cressie 1993; also the running
#' example in Bivand, Pebesma & Gomez-Rubio 2013 and in the INLA tutorial).
#' Redistributed under the sf package's MIT license. Ordered west -> east
#' so the Gondor-province stripe cut lands sensibly.
#'
#' @section Reproducibility:
#' Regenerate the object with `Rscript --vanilla data-raw/synth_surveillance.R`.
#' The generator pins `set.seed(20260702)` so the shipped `.rda` is
#' byte-stable across runs.
#'
#' @examples
#' data("synth_surveillance", package = "blindspot")
#' names(synth_surveillance)
#' head(synth_surveillance$cases)
#' head(synth_surveillance$boundaries)
#'
#' \dontrun{
#' # end-to-end demo (needs INLA)
#' fit <- bs_expected(
#'   cases      = synth_surveillance$cases,
#'   population = synth_surveillance$population,
#'   adjacency  = synth_surveillance$boundaries,
#'   id_col     = "adm2_guid",
#'   n_draws    = 200
#' )
#' spi <- bs_spi(fit, level = "district_year")
#' conc <- bs_concordance(
#'   spi           = spi,
#'   cases         = synth_surveillance$cases,
#'   population    = synth_surveillance$population,
#'   spi_threshold = 0.80,
#'   npafp_target  = 3,
#'   strata        = c("year", "adm1_name"),
#'   boundaries    = synth_surveillance$boundaries
#' )
#' }
"synth_surveillance"
