# Generate spi::synth_field_guide -- a precomputed SPI field guide on the
# synthetic bundle, so spi_field_guide_help(), the spi_field_guide_table()
# examples, and the render/help/print tests run instantly WITHOUT INLA. Run:
#   Rscript data-raw/synth_field_guide.R
# Writes data/synth_field_guide.rda (lazy-loaded via
# `data("synth_field_guide", package = "spi")`).
#
# Requires INLA locally (for the one-off spi_expected() fit). The shipped object
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

# 2. BYM2 expected-count fit (model without covariates; matches the paper's PRIMARY_SPEC) ------
fit <- spi_expected(
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
spi_dy <- spi_index(fit, level = "district_year")
spi_dm <- spi_index(fit, level = "district_month")

# 4. concordance ---------------------------------------------------------------
conc <- spi_concordance(
  spi = spi_dy,
  cases = synth$cases,
  population = synth$population,
  spi_threshold = 0.80,
  npafp_target = 3,
  boundaries = synth$boundaries
)

# 5. field guide (all STEPS components and context, both detection channels) --
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
  concordance = conc,
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
synth_field_guide$call <- quote(spi_field_guide(concordance = conc))

usethis::use_data(synth_field_guide, overwrite = TRUE, compress = "xz")

cli::cli_alert_success(
  "Wrote data/synth_field_guide.rda ({nrow(synth_field_guide$district_year)} \\
   district-years, read year {synth_field_guide$read_year})."
)
