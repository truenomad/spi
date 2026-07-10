##################  blindspot -- reproduce the paper's analysis  ##############
#
# Runs the paper's modelling chain on the synthetic toy dataset shipped with
# the package (`blindspot::synth_surveillance`), so anyone can execute it
# end-to-end without WHO-restricted POLIS data.
#
# Mirrors:
#   blindspot-paper_v2/02_scripts/main/2e_calculate_spi.R
#   blindspot-paper_v2/02_scripts/main/2f_all_cvdpv2_retrospective.R
#
# Primary "bare" spec:
#   log(mu_it) = alpha + b_i (BYM2) + u_t (IID year) + s(month)
#              /+ log(pop / 1e5)
# a priori cuts:  SPI >= 0.80  vs  WHO NPAFP target of 3 per 100,000
# person-years.
#
# Locate this file after installation:
#   file <- system.file("examples/paper_analysis.R", package = "blindspot")
#   file.edit(file)
#
# Requires: INLA -- https://inla.r-inla-download.org/R/stable/
###############################################################################

cli::cli_h1("blindspot -- paper reproduction on synthetic data")

## ---------------------------------------------------------------------------##
# Setup and parameters ---------------------------------------------------------
## ---------------------------------------------------------------------------##

spi_cut <- 0.80 # paper's a priori SPI adequacy cut
npafp_target <- 3 # per 100,000 under-15 person-years
focal_year <- 2023 # year displayed on the three-panel map
seed <- 42L
n_draws <- 1000L

## ---------------------------------------------------------------------------##
# Import data ------------------------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Load synthetic bundle")

synth <- blindspot::synth_surveillance
cases <- synth$cases
population <- synth$population
boundaries <- synth$boundaries
truth <- synth$truth # cheat sheet, for the validation panel

cli::cli_alert_info(
  "cases: {format(nrow(cases), big.mark = ',')} district-months"
)
cli::cli_alert_info(
  "population: {format(nrow(population), big.mark = ',')} district-years"
)
cli::cli_alert_info(
  "boundaries: {nrow(boundaries)} polygons \\
   (EPSG:{sf::st_crs(boundaries)$epsg})"
)
cli::cli_alert_info(
  "truth: {sum(truth$is_blindspot)} planted blindspots"
)

## ---------------------------------------------------------------------------##
# Adjacency graph (paper: 2e lines 62-65) --------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Build spatial neighbour graph")

adj <- blindspot::bs_adjacency(
  boundaries = boundaries,
  id_col = "adm2_guid"
)

print(adj)

## ---------------------------------------------------------------------------##
# Fit BYM2 expected-count model (paper's primary "bare" spec) ------------------
## ---------------------------------------------------------------------------##

# The paper fits four specs (bare, adjusted, sens_no_climate, sens_conflict).
# The toy bundle has no covariates, so we fit only the bare -- which is the
# paper's PRIMARY_SPEC (see 00_config.R).

cli::cli_h2("Fit BYM2 expected-count model")

fit_bare <- blindspot::bs_expected(
  cases = cases,
  population = population,
  adjacency = adj,
  id_col = "adm2_guid",
  season = "harmonic",
  year_effect = "iid",
  overdispersion = "iid",
  n_draws = n_draws,
  seed = seed,
  verbose = TRUE
)

print(fit_bare)
summary(fit_bare)

## ---------------------------------------------------------------------------##
# Overdispersion sanity check (paper: bs_compare_overdispersion in 2e) ---------
## ---------------------------------------------------------------------------##

cli::cli_h2("Compare overdispersion specs")

overdisp <- blindspot::bs_compare_overdispersion(
  cases = cases,
  population = population,
  adjacency = adj,
  id_col = "adm2_guid",
  season = "harmonic",
  specs = c("none", "iid", "nb")
)

print(overdisp)

## ---------------------------------------------------------------------------##
# SPI at multiple aggregation levels (paper: 2e lines 257-315) -----------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Compute SPI at three aggregation levels")

spi_dy <- blindspot::bs_spi(fit_bare, level = "district_year")
spi_dm <- blindspot::bs_spi(fit_bare, level = "district_month")
spi_total <- blindspot::bs_spi(fit_bare, level = "district_total")

cli::cli_h3("District-year SPI (primary reporting level)")
print(spi_dy)
summary(spi_dy)

cli::cli_h3("District-month SPI (finest temporal grain)")
print(spi_dm)

cli::cli_h3("District-total SPI (single value per district)")
print(spi_total)

# Interactive-only so batch Rscript runs don't spawn a stray Rplots.pdf
# next to the script.
if (interactive()) {
  plot(spi_dy)
}

## ---------------------------------------------------------------------------##
# Concordance vs conventional NPAFP-rate threshold (paper: 2f) -----------------
## ---------------------------------------------------------------------------##

# Four-cell cross-classification of each district-year:
#   SPI adequacy     -- posterior median SPI >= 0.80
#   WHO conventional -- NPAFP rate >= 3 per 100,000 under-15 person-years
#
# Cells:
#   Both adequate      -- both metrics agree "OK"
#   True shortfall     -- both metrics agree "failing"
#   False reassurance  -- NPAFP OK but SPI failing   <- SPI-only catches
#   False alarm        -- NPAFP failing but SPI OK   <- NPAFP-only alerts
#
# Paper headline: a non-trivial share of "conventionally adequate"
# district-years land in "False reassurance" -- SPI catches under-detection
# that the raw threshold misses.

cli::cli_h2("SPI x NPAFP concordance (pooled)")

conc <- blindspot::bs_concordance(
  spi = spi_dy,
  cases = cases,
  population = population,
  spi_threshold = spi_cut,
  npafp_target = npafp_target,
  boundaries = boundaries,
  verbose = TRUE
)

print(conc)
summary(conc) # adds 2x2 crosstab + row-wise %
if (interactive()) {
  plot(conc)
} # four-quadrant scatter

## ---------------------------------------------------------------------------##
# Stratified concordance (by year, by province) --------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Stratified concordance")

conc_by_year <- blindspot::bs_concordance(
  spi = spi_dy,
  cases = cases,
  population = population,
  spi_threshold = spi_cut,
  npafp_target = npafp_target,
  strata = "year",
  boundaries = boundaries,
  verbose = FALSE
)

conc_by_prov <- blindspot::bs_concordance(
  spi = spi_dy,
  cases = cases,
  population = population,
  spi_threshold = spi_cut,
  npafp_target = npafp_target,
  strata = "adm1_name",
  boundaries = boundaries,
  verbose = FALSE
)

cli::cli_h3("By year")
print(conc_by_year$by_stratum, n = Inf)

cli::cli_h3("By province")
print(conc_by_prov$by_stratum, n = Inf)

## ---------------------------------------------------------------------------##
# Truth overlay -- validation-style sanity check (toy only) --------------------
## ---------------------------------------------------------------------------##

# Do planted blindspots land in the "False reassurance" quadrant? Not a
# paper step; a sanity check enabled by the toy's ground-truth cheat sheet.

cli::cli_h2("Truth overlay")

planted <- truth$adm2_guid[truth$is_blindspot]

overlay <- conc$district_year |>
  dplyr::mutate(is_planted = adm2_guid %in% planted) |>
  dplyr::count(concordance, is_planted) |>
  tidyr::pivot_wider(
    names_from = is_planted,
    values_from = n,
    values_fill = 0L,
    names_prefix = "planted_"
  )

cli::cli_h3("Concordance cell x planted-blindspot flag")
print(overlay)

n_planted <- function(cell) {
  sum(
    conc$district_year$concordance == cell &
      conc$district_year$adm2_guid %in% planted
  )
}

cli::cli_alert_info(
  "SPI-only catches (False reassurance) among planted: \\
   {n_planted('False reassurance')}"
)
cli::cli_alert_info(
  "Both-metric catches (True shortfall) among planted: \\
   {n_planted('True shortfall')}"
)

## ---------------------------------------------------------------------------##
# Three-panel concordance figure (paper Figure 2 style) ------------------------
## ---------------------------------------------------------------------------##

# Panel A: conventional NPAFP rate, POLIS canonical breaks, red below target
# Panel B: posterior median SPI, paper's operational breaks, red below cut
# Panel C: four-cell concordance -- where the two indicators disagree

cli::cli_h2("Three-panel concordance map for {focal_year}")

maps <- blindspot::bs_concordance_maps(
  concordance = conc,
  boundaries = boundaries,
  year = focal_year
)

if (interactive()) {
  print(maps)
}

# Save to disk for the manuscript / desk-review folder:
# ggplot2::ggsave(
#   "03_output/main/figures/concordance_maps_2023.png",
#   maps, width = 18, height = 8, dpi = 300, bg = "white"
# )

## ---------------------------------------------------------------------------##
# SPI field guide -- seven-signal reading + verdict (paper: 2t) ----------------
## ---------------------------------------------------------------------------##

# Reads every district-year through the seven signals (S1-S7) and assigns a
# FLAG / WATCH / No-action verdict. S6 needs the adjacency graph, S7 needs the
# monthly SPI (seasonal) and an orphan-poliovirus table (genomic).

cli::cli_h2("SPI field guide")

genomic <- dplyr::filter(synth$virus_outcome, any_cvdpv2 == 1)

fg <- blindspot::bs_field_guide(
  concordance = conc,
  adjacency = adj,
  spi_month = spi_dm,
  genomic = genomic[, c("adm2_guid", "year")],
  verbose = TRUE
)

print(fg)
summary(fg) # adds signal fire-counts + the seven-signal reference

# Learn to read the verdict, narrated on this run's worked example:
if (interactive()) {
  blindspot::bs_field_guide_help("all", guide = fg)
}

# Paper's teaching table (Table S15/S16 style): the seven signals for four
# rule-selected districts, cells shaded by concern.
worked <- blindspot::bs_field_guide_table(fg, engine = "gt", layout = "worked")
if (interactive()) {
  print(worked)
}

# Save for the manuscript / desk-review folder:
# blindspot::bs_field_guide_table(
#   fg, engine = "flextable", layout = "worked",
#   file = "03_output/main/tables/spi_field_guide_2023.docx"
# )

# Finished ---------------------------------------------------------------------

cli::cli_rule(
  left = "Complete",
  right = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
