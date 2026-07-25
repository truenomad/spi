# Generate blindspot::synth_field_guide -- a precomputed SPI field guide on the
# synthetic bundle, so bs_field_guide_help(), the bs_field_guide_table()
# examples, and the render/help/print tests run instantly WITHOUT INLA. Run:
#   Rscript data-raw/synth_field_guide.R
# Writes data/synth_field_guide.rda (lazy-loaded via
# `data("synth_field_guide", package = "blindspot")`).
#
# Requires INLA locally (for the one-off bs_expected() fit). The shipped object
# is a `blindspot_field_guide` with all seven signals active. Pinned seed keeps
# it byte-stable across runs.

suppressPackageStartupMessages({
  library(dplyr)
})

devtools::load_all(quiet = TRUE)

set.seed(20260710)

synth <- synth_surveillance

# 1. spatial adjacency ---------------------------------------------------------
adj <- bs_adjacency(synth$boundaries, id_col = "adm2_guid")

# 2. BYM2 expected-count fit (bare spec; matches the paper's PRIMARY_SPEC) ------
fit <- bs_expected(
  cases = synth$cases,
  population = synth$population,
  adjacency = adj,
  id_col = "adm2_guid",
  season = "harmonic",
  year_effect = "iid",
  overdispersion = "iid",
  n_draws = 1000L,
  seed = 42L,
  verbose = TRUE
)

# 3. SPI at district-year and district-month levels ----------------------------
spi_dy <- bs_spi(fit, level = "district_year")
spi_dm <- bs_spi(fit, level = "district_month")

# 4. concordance ---------------------------------------------------------------
conc <- bs_concordance(
  spi = spi_dy,
  cases = synth$cases,
  population = synth$population,
  spi_threshold = 0.80,
  npafp_target = 3,
  boundaries = synth$boundaries
)

# 5. field guide (all signals active, both detection channels) -----------------
# The bundle records both serotypes, so the genomic input is stacked long with a
# serotype column rather than filtered to one: a district can then report what
# was actually found instead of the reading assuming a single serotype.
vo <- synth$virus_outcome
genomic <- dplyr::bind_rows(
  vo |>
    dplyr::filter(any_cvdpv2 == 1) |>
    dplyr::transmute(adm2_guid, year, serotype = "cVDPV2"),
  vo |>
    dplyr::filter(any_wpv1 == 1) |>
    dplyr::transmute(adm2_guid, year, serotype = "WPV1")
)

synth_field_guide <- bs_field_guide(
  concordance = conc,
  adjacency = adj,
  spi_month = spi_dm,
  genomic = genomic,
  es = synth$es_district_year,
  es_col = "n_positive",
  serotype_col = "serotype",
  verbose = TRUE
)

# drop the bulky matched call so the shipped object stays small and stable
synth_field_guide$call <- quote(bs_field_guide(concordance = conc))

usethis::use_data(synth_field_guide, overwrite = TRUE, compress = "xz")

cli::cli_alert_success(
  "Wrote data/synth_field_guide.rda ({nrow(synth_field_guide$district_year)} \\
   district-years, read year {synth_field_guide$read_year})."
)
