#' Synthetic AFP surveillance toy dataset
#'
#' A fully self-contained toy dataset that lets a user run the entire
#' `blindspot` chain -- `bs_adjacency()` -> `bs_expected()` -> `bs_spi()` ->
#' `bs_concordance()` -- without any real surveillance data. Every value is
#' simulated; no personally identifying or operationally sensitive information
#' is present.
#'
#' @format A named list with ten elements:
#' \describe{
#'   \item{cases}{Tibble, 12000 x 3. Columns: `adm2_guid` (character district
#'     id, `"GDR-001"`..`"GDR-100"`), `month` (Date, first-of-month), `count`
#'     (integer). Full 100 x 120 district-month grid with zero counts
#'     explicit.}
#'   \item{population}{Tibble, 1000 x 3. Columns: `adm2_guid`, `year`
#'     (integer, 2015-2024), `pop_u15` (numeric, under-15 population).
#'     Annual, grown at 2% per year from a lognormal baseline.}
#'   \item{covariates}{Tibble, 1000 x 5. District-year covariate layers for the
#'     adjusted `bs_expected()` spec. Columns: `adm2_guid`, `year` (integer,
#'     2015-2024), `dtp3` (numeric, DTP3 immunisation coverage %, a
#'     health-system-reach proxy), `urban_prop` (numeric, 0-1 urban share,
#'     structural per district), `travel_time_min` (numeric, median minutes to
#'     the nearest health facility, an access-to-care proxy; right-skewed, a
#'     `log_transform` candidate). Correlated with the planted blindspots (lower
#'     coverage, worse access) and population (denser is more urban).}
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
#'     (integer, 0/1), `any_virus` (integer, 0/1). cVDPV2 detections surface
#'     the year after a district's detection completeness falls well below the
#'     contemporaneous norm (relative completeness < 0.6), at rate
#'     `plogis(-0.6)`; background rate `plogis(-4)`. Retained as an ad-hoc
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
#'   \item{truth}{Tibble, 100 x 6. Ground-truth cheat sheet. Columns:
#'     `adm2_guid`, `surveillance_profile` (character: `"resilient"`,
#'     `"early_improver"`, `"covid_transient"`, or `"persistent_laggard"`),
#'     `is_blindspot` (logical, TRUE for every non-resilient profile),
#'     `is_low_incidence` (logical, TRUE for the low-baseline clusters that
#'     seed the False alarm / True shortfall cells), `covid_nadir` (numeric,
#'     the district's relative detection completeness at the 2020 trough), and
#'     `recovered_2024` (logical, TRUE if completeness is back to adequate by
#'     2024).}
#' }
#'
#' @section Data-generating process:
#' True AFP burden is stable over time; observed counts are that burden thinned
#' by *detection completeness*. Counts are drawn from a negative-binomial
#' likelihood with mean
#' \deqn{\mu^{obs}_{it} = c_{it}\,\exp\!\big(\alpha + b_i + s(m_t) + \log(\mathrm{pop}_{u15,it} / 10^5)\big),}
#' where `b_i` is a BYM2 spatial random effect (mixing parameter 0.6, marginal
#' precision 4), `s(m_t)` is a 1st-order harmonic seasonal term, and
#' \eqn{c_{it} = g_t \cdot r_{it}} is detection completeness. The global factor
#' `g_t` improves through 2015-2019 to a pre-COVID peak, crashes ~30% in 2020
#' (COVID-19 disruption), and recovers by 2023-24. The relative factor `r_it`
#' is 1 for *resilient* districts (which track `g_t`) and dips below 1 for the
#' other profiles in specific windows: *early_improver* (poor 2015-17,
#' maturing away by 2019), *covid_transient* (extra hit 2020-21, recovered by
#' 2023), and *persistent_laggard* (hit in 2020 and still degraded in 2024).
#' Because the SPI model absorbs `g_t` into its year effect, SPI tracks `r_it`
#' -- so the SPI-flagged cells wax with the early-2010s immaturity, ebb to a
#' pre-COVID low, spike in 2020-21, and recede as districts recover, leaving a
#' residue of persistent laggards. `virus_outcome` marks the year after a
#' completeness gap as cVDPV2-positive at elevated rate.
#'
#' Two spatially-contiguous clusters (12 districts, `is_low_incidence`) are
#' given a low baseline (`b_i` overridden to about -4.7) and a denser
#' population, so their genuine per-capita NPAFP rate sits below the
#' conventional target while the context-conditional SPI scores the resilient
#' ones adequate -- the "False alarm" cell. Seven of the twelve are also
#' persistent laggards, so their completeness stays low from 2020 on and SPI
#' flags them -- the "True shortfall" cell. Normal-baseline laggards and
#' transients (raw rate above target, SPI flagged) supply the "False
#' reassurance" cell, so all four cells of the SPI x NPAFP crosstab are
#' populated and evolve realistically across the ten study years.
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

#' Precomputed SPI field guide on the synthetic bundle
#'
#' A [bs_field_guide()] result computed on [synth_surveillance], with all
#' seven signals active (S6 neighbour contrast, S7 seasonal, and S7 genomic
#' all supplied). It ships so that [bs_field_guide_help()], the
#' [bs_field_guide_table()] examples, and the package tests run instantly
#' without refitting the (INLA-based) upstream model. The full modelling chain
#' that produced it is `bs_adjacency()` -> `bs_expected()` (bare spec) ->
#' `bs_spi()` at district-year and district-month levels -> `bs_concordance()`
#' -> `bs_field_guide()`.
#'
#' @format An object of class `blindspot_field_guide`; see the Value section
#'   of [bs_field_guide()] for the element structure. Read year 2024; SPI cut
#'   0.80; NPAFP target 3 per 100,000 under-15-years.
#'
#' @section Reproducibility:
#' Regenerate with `Rscript data-raw/synth_field_guide.R` (needs INLA). The
#' generator pins `set.seed(20260710)` and the fit's `seed = 42L`.
#'
#' @seealso [bs_field_guide()], [bs_field_guide_table()],
#'   [bs_field_guide_help()], [synth_surveillance].
#'
#' @examples
#' print(synth_field_guide)
#' head(as_tibble(synth_field_guide))
"synth_field_guide"
