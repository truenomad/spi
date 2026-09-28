# Generate spi::synth_field_guide -- a precomputed SPI field guide on the
# synthetic bundle, so spi_field_guide_help(), the spi_field_guide_table()
# examples, and the render/help/print tests run instantly WITHOUT INLA. Run:
#   Rscript data-raw/synth_field_guide.R
# Writes data/synth_field_guide.rda (lazy-loaded via
# `data("synth_field_guide", package = "spi")`).
#
# Requires INLA locally (for the spi_index() fits). The shipped object
# is a `spi_field_guide` with all five STEPS components and every
# context signal active. Pinned seed keeps it byte-stable across runs.

suppressPackageStartupMessages({
  library(dplyr)
})

devtools::load_all(quiet = TRUE)

set.seed(20260710)

synth <- synth_surveillance

# 1. spatial adjacency ---------------------------------------------------------
adj <- spi_adjacency(synth$boundaries, id_col = "adm2_guid")

# 2. SPI at district-year and district-month levels, each assessment year fitted
# on the years before it (model without covariates) ---------------------------
spi_at <- function(level) {
  spi_index(
    cases = synth$cases,
    population = synth$population,
    adjacency = adj,
    first_assessment = 2018,
    level = level,
    id_col = "adm2_guid",
    season = "harmonic",
    year_effect = "iid",
    overdispersion = "iid",
    n_draws = 1000L,
    seed = 42L,
    verbose = TRUE
  )
}
spi_dy <- spi_at("district_year")
spi_dm <- spi_at("district_month")

# 3. concordance ---------------------------------------------------------------
conc <- spi_compare_npafp(
  spi = spi_dy,
  cases = synth$cases,
  population = synth$population,
  spi_threshold = 0.80,
  npafp_target = 3,
  boundaries = synth$boundaries
)

# 4. field guide (all STEPS components and context, both detection channels) --
# The bundle records both serotypes, so AFP detection records are stacked long with a
# serotype column rather than filtered to one: a district can then report what
# was actually found instead of the reading assuming a single serotype.
vo <- synth$virus_outcome
detections <- dplyr::bind_rows(
  vo |>
    dplyr::filter(any_cvdpv2 == 1) |>
    dplyr::transmute(adm2_guid, year, serotype = "cVDPV2"),
  vo |>
    dplyr::filter(any_wpv1 == 1) |>
    dplyr::transmute(adm2_guid, year, serotype = "WPV1")
)

synth_field_guide <- spi_field_guide(
  comparison = conc,
  process = synth$afp_process,
  adjacency = adj,
  spi_month = spi_dm,
  detections = detections,
  es = synth$es_district_year,
  es_col = "n_positive",
  serotype_col = "serotype",
  verbose = TRUE
)

# drop the bulky matched call so the shipped object stays small and stable
synth_field_guide$call <- quote(spi_field_guide(comparison = conc))

usethis::use_data(synth_field_guide, overwrite = TRUE, compress = "xz")

cli::cli_alert_success(
  "Wrote data/synth_field_guide.rda ({nrow(synth_field_guide$district_year)} \\
   district-years, read year {synth_field_guide$read_year})."
)
