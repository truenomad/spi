#' Synthetic AFP surveillance data
#'
#' Example data for `spi_adjacency()`, `spi_expected()`, `spi_index()`, and
#' `spi_compare_npafp()`. Surveillance counts are simulated. Population counts
#' and the original boundary geometry come from the sources listed below.
#'
#' @format A named list with twelve elements:
#' \describe{
#'   \item{cases}{Tibble, 28,320 x 3. Columns: `adm2_guid` (character district
#'     id, a POLIS-style GUID such as
#'     `"{54CD979C-CF9D-6A65-567F-8976C9546137}"`), `month` (Date,
#'     first-of-month), `count` (integer). Full 236 x 120 district-month grid
#'     with zero counts explicit.}
#'   \item{population}{Tibble, 2,360 x 3. Columns: `adm2_guid`, `year`
#'     (integer, 2015-2024), `pop_u15` (numeric, under-15 population). Real
#'     annual under-15 counts extracted from the WorldPop 0-14 rasters over the
#'     source geometry (range ~19k-900k).}
#'   \item{covariates}{Tibble, 2,360 x 5. District-year covariate layers for the
#'     adjusted `spi_expected()` model. Columns: `adm2_guid`, `year` (integer,
#'     2015-2024), `dtp3` (numeric, DTP3 immunisation coverage %, a
#'     proxy for health service access), `urban_prop` (numeric, 0-1 urban share,
#'     structural per district), `travel_time_min` (numeric, median minutes to
#'     the nearest health facility, an access-to-care proxy; right-skewed, a
#'     `log_transform` candidate). Correlated with the planted blindspots (lower
#'     coverage, worse access) and population (denser is more urban).}
#'   \item{boundaries}{`sf` object, 236 x 4 (+ geometry). Columns: `adm2_guid`,
#'     `adm2_name` (fictional place names), `adm1_name` (one of
#'     36 fictional provinces), `adm0_name` (`"Harad"`), `geometry` (POLYGON /
#'     MULTIPOLYGON on a local grid; CRS deliberately undefined). Derived from
#'     real Lake Chad adm2 boundaries (geoBoundaries) across four countries,
#'     merged into one gap-free landmass, partitioned into 36 contiguous
#'     provinces, relabelled, and rotated onto a local grid with the CRS
#'     removed for use as fictional geography.}
#'   \item{ward_boundaries}{`sf` object, ~1,548 x 6 (+ geometry). Columns:
#'     `adm3_guid` (the parent `adm2_guid` GUID suffixed `-W01`, `-W02`, ...),
#'     `adm3_name` (parent adm2 name plus a
#'     suffix from a pool of directional or geographic descriptors --
#'     e.g. `"Khandoth North Ward"`, `"Nenad Ford"`), `adm2_guid`,
#'     `adm2_name`, `adm1_name`, `adm0_name`, `geometry`. Each adm2 is
#'     subdivided into 5-8 wards by nested Voronoi tessellation.}
#'   \item{virus_outcome}{Tibble, 2,360 x 5. Columns: `adm2_guid`, `year`,
#'     `any_wpv1` (integer, 0/1; always 0 in these data), `any_cvdpv2`
#'     (integer, 0/1), `any_virus` (integer, 0/1). cVDPV2 detections occur
#'     the year after a district's detection completeness falls well below the
#'     contemporaneous norm (relative completeness < 0.6), at rate
#'     `plogis(-0.6)`; background rate `plogis(-4)`. Included for comparison with SPI results.}
#'   \item{es_sites}{`sf` object, 160 x 5 (+ geometry). Environmental
#'     surveillance sentinel sites (~67% of districts host one). Columns:
#'     `es_site_id`, `site_name`, `adm3_guid`, `adm2_guid`, `adm1_name`,
#'     `geometry` (POINT). One site per selected district, placed at the
#'     centroid of a random ward.}
#'   \item{es_data}{Tibble, 19,200 x 4. Monthly ES sample-level rows. Columns:
#'     `es_site_id`, `adm2_guid`, `sample_date` (Date, first-of-month),
#'     `positive_cvdpv2` (integer, 0/1). Positivity probability shares the
#'     same true-vs-observed gap signal as `virus_outcome`, so the two ES
#'     proxies are internally consistent.}
#'   \item{es_district_year}{Tibble, 1,600 x 4. Annual ES totals by district.
#'     Columns: `adm2_guid`, `year`, `n_samples`, `n_positive`.}
#'   \item{afp_timeliness}{Tibble, 2,360 x 5. District-year AFP timeliness
#'     counts. Columns: `adm2_guid`, `year`, `afp_cases`, `n_assessable` (cases
#'     with a usable onset date), `n_within_7d` (of those, notified inside the
#'     window). Onset-to-notification is the one conventional AFP indicator
#'     POLIS does not publish. Calculate it from case data. These counts
#'     supply `onset_notify_pct` / `onset_notify_n` in
#'     [spi_field_guide_pager()]'s `indicators_df`.}
#'   \item{afp_process}{Tibble, 2,360 x 6. District-year AFP process counts
#'     for the STEPS timeliness and stool adequacy components. Columns:
#'     `adm2_guid`, `year`, `n_cases` (AFP cases), `n_adequate` (cases with
#'     adequate stool specimens), `n_transport` (cases with both the second
#'     stool collection and laboratory receipt dates), `n_transport_timely`
#'     (of those, received by the laboratory within 3 days). The shape
#'     [spi_field_guide()]'s `process` argument expects.}
#'   \item{simulation_truth}{Tibble, 236 x 6. Settings the simulation used
#'     for each district; they describe the synthetic data, not true
#'     surveillance performance. Columns:
#'     `adm2_guid`, `surveillance_profile` (character: `"resilient"`,
#'     `"early_improver"`, `"covid_transient"`, or `"persistent_laggard"`),
#'     `is_blindspot` (logical, TRUE for every non-resilient profile),
#'     `is_low_incidence` (logical, TRUE for the low-baseline clusters that
#'     seed the NPAFP below target only and Both below categories),
#'     `covid_nadir` (numeric,
#'     the district's relative detection completeness at the 2020 trough), and
#'     `recovered_2024` (logical, TRUE if completeness is back to adequate by
#'     2024).}
#' }
#'
#' @section Data-generating process:
#' True AFP burden is stable over time; observed counts are that burden thinned
#' by *detection completeness*. Counts are drawn from a negative-binomial
#' likelihood with mean
#' \deqn{\mu^{obs}_{it} = c_{it}\,\exp\!\big(\alpha + b_i + s(m_t) +
#'   \log(\mathrm{pop}_{u15,it} / 10^5)\big),}
#' where `b_i` is a BYM2 spatial random effect (mixing parameter 0.6, marginal
#' precision 4), `s(m_t)` is a 1st-order harmonic seasonal term, and
#' \eqn{c_{it} = g_t \cdot r_{it}} is detection completeness. The global factor
#' `g_t` improves through 2015-2019 to a pre-COVID peak, falls by about 30% in 2020
#' (COVID-19 disruption), and recovers by 2023-24. The relative factor `r_it`
#' is 1 for *resilient* districts (which track `g_t`) and dips below 1 for the
#' other profiles in specific windows: *early_improver* (poor 2015-17,
#' recovered by 2019), *covid_transient* (further decline in 2020-21,
#' recovered by 2023), and *persistent_laggard* (decline in 2020 that persists
#' through 2024). The SPI model's year effect accounts for `g_t`, so SPI
#' tracks `r_it`. Reporting shortfalls become less common before 2020,
#' increase in 2020-21, and decrease as districts recover. `virus_outcome` marks the year after a
#' completeness gap as cVDPV2-positive at elevated rate.
#'
#' Four spatially-contiguous clusters (25 districts, `is_low_incidence`) get a
#' low baseline (`b_i` overridden so their true rate sits around 1.5 per 100,000
#' under-15 per year), so their genuine per-capita NPAFP rate sits below the
#' conventional target while the resilient districts can have SPI at or above
#' 1, giving the "NPAFP below target only" category. Sixteen of the twenty-five are also
#' persistent laggards, so their completeness stays low from 2020 on and SPI
#' places them in the "Both below" category. Districts with higher
#' baseline reporting can meet the NPAFP target while SPI is below 1, giving
#' the "SPI below threshold only" category. The simulation includes all four combinations
#' across the ten study years; these categories do not establish adequacy.
#'
#' @section Fictional geography:
#' The fictional country Harad has 236 districts in 36 provinces. All place
#' names are invented. The source geometry has been rotated onto a local grid
#' and its CRS removed. The geography is used only for examples.
#'
#' @section Attribution:
#' Geometry is derived from real admin-2 boundaries for four bordering Lake
#' Chad countries (Nigeria, Niger, Chad, Cameroon) from **geoBoundaries**
#' gbOpen ADM2, redistributed under CC-BY 4.0 (Runfola et al. 2020,
#' \doi{10.1371/journal.pone.0231866}, <https://www.geoboundaries.org>). The
#' units are merged, cropped to a region around the basin, joined without
#' gaps, renamed, and rotated (see
#' `data-raw/synth_admin_polygons.R` and the shipped
#' `inst/extdata/synth_admin_polygons.provenance.txt`).
#'
#' Population (`pop_u15`) is real under-15 counts zonal-summed from
#' **WorldPop** unconstrained global age-structured rasters (0-14), keyed to
#' the source geometry and shipped as `inst/extdata/synth_admin_pop_u15.csv`.
#' WorldPop is released under CC-BY 4.0 (<https://www.worldpop.org>).
#'
#' @section Reproducibility:
#' Build the boundary layer with `Rscript data-raw/synth_admin_polygons.R`
#' (writes `inst/extdata/synth_admin_polygons.gpkg`), then regenerate this
#' object with `Rscript --vanilla data-raw/synth_surveillance.R`. The generator uses `set.seed(20260702)`.
#'
#' @examples
#' data("synth_surveillance", package = "spi")
#' names(synth_surveillance)
#' head(synth_surveillance$cases)
#' head(synth_surveillance$boundaries)
#'
#' \dontrun{
#' # end-to-end demo (needs INLA)
#' fit <- spi_expected(
#'   cases      = synth_surveillance$cases,
#'   population = synth_surveillance$population,
#'   adjacency  = synth_surveillance$boundaries,
#'   id_col     = "adm2_guid",
#'   n_draws    = 200
#' )
#' spi <- spi_index(fit, level = "district_year")
#' comparison <- spi_compare_npafp(
#'   spi = spi,
#'   cases = synth_surveillance$cases,
#'   population = synth_surveillance$population,
#'   spi_threshold = 0.80,
#'   npafp_target = 3,
#'   strata = c("year", "adm1_name"),
#'   boundaries = synth_surveillance$boundaries
#' )
#' }
"synth_surveillance"

#' Precomputed SPI field guide for the synthetic data
#'
#' A [spi_field_guide()] result computed on [synth_surveillance], with all
#' five STEPS components and the neighbour, seasonal and detection context
#' supplied. This lets [spi_field_guide_help()], the
#' [spi_field_guide_table()] examples, and tests run without refitting the model.
#' It was produced by `spi_adjacency()` -> `spi_expected()` (no covariates) ->
#' `spi_index()` at district-year and district-month levels ->
#' `spi_compare_npafp()` -> `spi_field_guide()`.
#'
#' @format An object of class `spi_field_guide`; see the Value section
#'   of [spi_field_guide()] for the element structure. Read year 2024; SPI cut
#'   1; NPAFP target 3 per 100,000 under-15-years.
#'
#' @section Reproducibility:
#' Regenerate with `Rscript data-raw/synth_field_guide.R` (needs INLA). The
#' generator uses `set.seed(20260710)` and the fit's `seed = 42L`.
#'
#' @seealso [spi_field_guide()], [spi_field_guide_table()],
#'   [spi_field_guide_help()], [synth_surveillance].
#'
#' @examples
#' print(synth_field_guide)
#' head(as_tibble(synth_field_guide))
"synth_field_guide"
