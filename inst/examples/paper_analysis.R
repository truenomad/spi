##################  blindspot -- reproduce the paper's analysis  ##############
#
# A guided tour of the whole blindspot API on the synthetic toy dataset shipped
# with the package (`blindspot::synth_surveillance`), so anyone can execute it
# end-to-end without WHO-restricted POLIS data. It exercises every exported
# function:
#
#   bs_adjacency              spatial neighbour graph
#   bs_expected               BYM2 expected-count model (bare + adjusted specs)
#   bs_compare_overdispersion Poisson / iid / NB likelihood comparison
#   bs_spi                    surveillance performance index (3 grains)
#   bs_concordance            SPI vs conventional NPAFP threshold (+ strata)
#   bs_concordance_maps       three-panel choropleth
#   bs_field_guide            seven-signal reading -> FLAG / WATCH / No action
#   bs_field_guide_help       learn to read the verdict
#   bs_field_guide_table      publication-ready gt / flextable
#   as_tibble / print / summary / plot methods
#
# Mirrors:
#   blindspot-paper_v2/02_scripts/main/2e_calculate_spi.R
#   blindspot-paper_v2/02_scripts/main/2f_all_cvdpv2_retrospective.R
#
# Primary "bare" spec:
#   log(mu_it) = alpha + b_i (BYM2) + u_t (IID year) + s(month) + log(pop / 1e5)
# Adjusted spec adds district-level covariates (a sensitivity analysis).
# A priori cuts:  SPI >= 0.80  vs  WHO NPAFP target of 3 per 100,000
# person-years.
#
# Locate this file after installation:
#   file <- system.file("examples/paper_analysis.R", package = "blindspot")
#   file.edit(file)
#
# Requires: INLA -- https://inla.r-inla-download.org/R/stable/
# Note: this runs several INLA fits (bare, adjusted, three overdispersion
# specs); expect a few minutes on the toy data.
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
# Fit BYM2 expected-count model -- primary "bare" spec -------------------------
## ---------------------------------------------------------------------------##

# The paper's PRIMARY_SPEC (see 00_config.R): intercept + BYM2 spatial +
# IID year + harmonic season + log person-time offset. No covariates, so the
# SPI reads detection performance directly rather than net-of-covariates.

cli::cli_h2("Fit BYM2 expected-count model (bare)")

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

# Any blindspot object coerces to a tibble for joins / CSV export:
expected_tbl <- tibble::as_tibble(fit_bare)
cli::cli_alert_info(
  "as_tibble(fit_bare): {format(nrow(expected_tbl), big.mark = ',')} rows \\
   x {ncol(expected_tbl)} cols"
)

## ---------------------------------------------------------------------------##
# Adjusted spec: district-level covariates (paper: bare vs adjusted) -----------
## ---------------------------------------------------------------------------##

# The paper fits a bare spec and an "adjusted" spec that adds covariates. The
# toy bundle ships none, so we synthesise three district-year layers a real
# analysis would pull from DHS / HMIS / travel-time surfaces:
#
#   dtp3            DTP3 immunisation coverage (%)     - health-system reach
#   urban_prop      share of district that is urban    - structural, per district
#   travel_time_min median minutes to nearest facility - access to care
#
# They are correlated with the planted profiles and population so the adjusted
# model has real signal. travel_time_min is right-skewed, so we log-transform
# it. Covariates are standardised internally, so each effect reads as the rate
# ratio per one standard deviation.
#
# CAVEAT (and the teaching point): these proxy surveillance *access*, which is
# partly downstream of detection itself. Adjusting for them can attenuate the
# very gap the SPI is built to surface -- which is why the paper keeps the bare
# model PRIMARY and treats the adjusted fit as a sensitivity analysis.

cli::cli_h2("Adjusted spec: simulate + fit district-level covariates")

set.seed(seed)

ids <- boundaries$adm2_guid
# planted blindspots run lower coverage / worse access than resilient districts
lagging <- stats::setNames(truth$is_blindspot[match(ids, truth$adm2_guid)], ids)

# structural urbanicity: bigger districts skew urban, blindspots slightly rural
pop0 <- population |>
  dplyr::filter(year == min(year)) |>
  dplyr::arrange(match(adm2_guid, ids))
urban <- stats::setNames(
  plogis(
    0.9 * as.numeric(scale(log(pop0$pop_u15))) -
      0.7 * lagging[pop0$adm2_guid] +
      rnorm(length(ids), 0, 0.5)
  ),
  pop0$adm2_guid
)

# expand to district-year, add a secular trend and a 2020-21 COVID dip
covariates <- population |>
  dplyr::transmute(adm2_guid, year) |>
  dplyr::mutate(
    yrs = year - min(year),
    lag_i = as.numeric(lagging[adm2_guid]),
    urb = urban[adm2_guid],
    covid = as.numeric(year %in% c(2020L, 2021L)),
    dtp3 = round(pmin(99, pmax(40,
      86 + 0.8 * yrs - 14 * lag_i + 10 * (urb - 0.5) -
        6 * covid + rnorm(dplyr::n(), 0, 2)
    )), 1),
    travel_time_min = round(pmax(8, exp(
      3.6 - 1.1 * urb + 0.5 * lag_i + 0.3 * covid + rnorm(dplyr::n(), 0, 0.25)
    )), 1),
    urban_prop = round(urb, 3)
  ) |>
  dplyr::select(adm2_guid, year, dtp3, urban_prop, travel_time_min)

cli::cli_alert_info(
  "covariates: {format(nrow(covariates), big.mark = ',')} district-years x 3 \\
   layers (dtp3, urban_prop, travel_time_min)"
)

fit_adj <- blindspot::bs_expected(
  cases = cases,
  population = population,
  adjacency = adj,
  covariates = covariates,
  id_col = "adm2_guid",
  log_transform = "travel_time_min",
  season = "harmonic",
  year_effect = "iid",
  overdispersion = "iid",
  n_draws = n_draws,
  seed = seed,
  verbose = TRUE
)

# Covariate effects: rate ratio per SD, 95% credible interval, and a `signif`
# flag (CI excludes 1). summary() also prints the fit / calibration diagnostics.
sm_adj <- summary(fit_adj)

cli::cli_h3("Covariate rate ratios (per SD; travel time on log scale)")
print(sm_adj$effects)

# Does adjustment improve fit (lower DIC / WAIC)?
cli::cli_h3("Bare vs adjusted fit")
print(tibble::tibble(
  spec = c("bare", "adjusted"),
  dic = round(c(fit_bare$model$dic$dic, fit_adj$model$dic$dic)),
  waic = round(c(fit_bare$model$waic$waic, fit_adj$model$waic$waic))
))

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

# Downstream reporting uses the bare (primary) fit.

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

# Coerce to a tibble for downstream joins / export:
spi_tbl <- tibble::as_tibble(spi_dy)
cli::cli_alert_info(
  "as_tibble(spi_dy): {nrow(spi_tbl)} rows x {ncol(spi_tbl)} cols"
)

# Interactive-only so batch Rscript runs don't spawn a stray Rplots.pdf
# next to the script.
if (interactive()) {
  plot(spi_dy) # distribution / funnel / caterpillar / calibration
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

overlay <- table(
  concordance = conc$district_year$concordance,
  planted = ifelse(conc$district_year$adm2_guid %in% planted, "planted", "other")
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
# rule-selected districts, cells shaded by concern. "scan" lists every flagged
# district; "worked" walks the four archetypes.
scan <- blindspot::bs_field_guide_table(fg, engine = "gt", layout = "scan")
worked <- blindspot::bs_field_guide_table(fg, engine = "gt", layout = "worked")
if (interactive()) {
  print(worked)
}

# Save for the manuscript / desk-review folder (format inferred from extension):
# blindspot::bs_field_guide_table(
#   fg, engine = "flextable", layout = "worked",
#   file = "03_output/main/tables/spi_field_guide_2023.docx"
# )

# Finished ---------------------------------------------------------------------

cli::cli_rule(
  left = "Complete",
  right = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
