##################  spi -- reproduce the paper's analysis  ####################
#
# Run the main spi analysis steps on the bundled synthetic data
# (`spi::synth_surveillance`). No POLIS access is needed.
# Functions used:
#
#   spi_adjacency              spatial neighbour graph
#   spi_expected               BYM2 expected-count model (with and without covariates)
#   spi_compare_overdispersion Poisson / iid / NB likelihood comparison
#   spi_index                  surveillance performance index (3 aggregation levels)
#   spi_compare_npafp          SPI vs the NPAFP target (+ strata)
#   spi_compare_npafp_maps     three-panel choropleth
#   spi_field_guide            STEPS review -> priority / monitor / none
#   spi_field_guide_help       learn to read the review
#   spi_field_guide_table      formatted gt / flextable reports
#   as_tibble / print / summary / plot methods
#
# Mirrors:
#   blindspot-paper_v2/02_scripts/main/2e_calculate_spi.R
#   blindspot-paper_v2/02_scripts/main/2f_all_cvdpv2_retrospective.R
#
# Primary model, without covariates:
#   log(mu_it) = alpha + b_i (BYM2) + u_t (IID year) + s(month) + log(pop / 1e5)
# Adjusted spec adds district-level covariates (a sensitivity analysis).
# Example cutoffs: SPI >= 0.80 and NPAFP rate >= 3 per 100,000
# person-years.
#
# Locate this file after installation:
#   file <- system.file("examples/paper_analysis.R", package = "spi")
#   file.edit(file)
#
# Requires: INLA -- https://inla.r-inla-download.org/R/stable/
# Note: this runs several INLA fits (bare, adjusted, three overdispersion
# specs); expect a few minutes on the example data.
###############################################################################

cli::cli_h1("spi -- paper reproduction on synthetic data")

## ---------------------------------------------------------------------------##
# Setup and parameters ---------------------------------------------------------
## ---------------------------------------------------------------------------##

spi_cut <- 0.80 # SPI cutoff for this example
npafp_target <- 3 # per 100,000 under-15 person-years
focal_year <- 2023 # year displayed on the three-panel map
seed <- 42L
n_draws <- 1000L

## ---------------------------------------------------------------------------##
# Import data ------------------------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Load synthetic bundle")

synth <- spi::synth_surveillance
cases <- synth$cases
population <- synth$population
boundaries <- synth$boundaries
truth <- synth$simulation_truth # simulation settings, for the comparison panel

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
  "truth: {sum(truth$is_blindspot)} simulated reporting shortfalls"
)

## ---------------------------------------------------------------------------##
# Adjacency graph (paper: 2e lines 62-65) --------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Build spatial neighbour graph")

adj <- spi::spi_adjacency(
  boundaries = boundaries,
  id_col = "adm2_guid"
)

print(adj)

## ---------------------------------------------------------------------------##
# Fit the expected-count model: bare vs adjusted ------------------------------
## ---------------------------------------------------------------------------##

# Fit the model without covariates, then add covariates as a sensitivity check.
# The default model includes BYM2 spatial effects, independent year effects,
# harmonic seasonality, negative binomial overdispersion, and a person-time
# offset. The remaining analysis uses this fit.
#
# Covariates describe surveillance access. Adjusting for them can reduce the
# apparent shortfall, so compare the adjusted results with the default model.

cli::cli_h2("Option A -- model without covariates")

fit_bare <- spi::spi_expected(
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

# Any spi object coerces to a tibble for joins / CSV export:
expected_tbl <- tibble::as_tibble(fit_bare)
cli::cli_alert_info(
  "as_tibble(fit_bare): {format(nrow(expected_tbl), big.mark = ',')} rows \\
   x {ncol(expected_tbl)} cols"
)

## ---------------------------------------------------------------------------##
# Option B -- adjusted spec (adds district-level covariates) -------------------
## ---------------------------------------------------------------------------##

# Three covariates are available: dtp3 (DTP3 coverage %), urban_prop (urban
# share), and travel_time_min (minutes to the nearest facility). They are
# correlated with the simulated reporting shortfalls. Log-transform travel
# time because it is right-skewed. The model standardises covariates and
# reports rate ratios per standard deviation.

cli::cli_h2("Option B -- adjusted spec (covariates)")

covariates <- synth$covariates
print(utils::head(covariates))

# The ONLY change from the bare call is the covariates + log_transform args:
fit_adj <- spi::spi_expected(
  cases = cases,
  population = population,
  adjacency = adj,
  covariates = covariates, # <- district-year covariate layers
  log_transform = "travel_time_min", # <- right-skewed, so log(1 + x)
  id_col = "adm2_guid",
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

cli::cli_alert_info("Downstream steps use the bare (primary) fit.")

## ---------------------------------------------------------------------------##
# Compare models for count variation (paper: spi_compare_overdispersion in 2e) --------
## ---------------------------------------------------------------------------##

cli::cli_h2("Compare overdispersion specs")

overdisp <- spi::spi_compare_overdispersion(
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

# pass boundaries so saved SPI tables carry adm1/adm2 names next to the id
spi_dy <- spi::spi_index(fit_bare, level = "district_year",
                            boundaries = boundaries)
spi_dm <- spi::spi_index(fit_bare, level = "district_month",
                            boundaries = boundaries)
spi_total <- spi::spi_index(fit_bare, level = "district_total",
                               boundaries = boundaries)

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
# "False reassurance" identifies district-years that meet the NPAFP target
# but have an SPI below the threshold.

cli::cli_h2("SPI and NPAFP classification (pooled)")

conc <- spi::spi_compare_npafp(
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
# Stratified classification (by year, by province) -----------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Stratified classification")

conc_by_year <- spi::spi_compare_npafp(
  spi = spi_dy,
  cases = cases,
  population = population,
  spi_threshold = spi_cut,
  npafp_target = npafp_target,
  strata = "year",
  boundaries = boundaries,
  verbose = FALSE
)

conc_by_prov <- spi::spi_compare_npafp(
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
# Compare results with the simulation settings --------------------
## ---------------------------------------------------------------------------##

# Check which categories contain the simulated reporting shortfalls.
# This comparison uses the known simulation settings.

cli::cli_h2("Truth overlay")

planted <- truth$adm2_guid[truth$is_blindspot]

overlay <- table(
  comparison = conc$district_year$category,
  planted = ifelse(conc$district_year$adm2_guid %in% planted, "planted", "other")
)

cli::cli_h3("SPI and NPAFP category by simulated reporting shortfall")
print(overlay)

n_planted <- function(cell) {
  sum(
    conc$district_year$category == cell &
      conc$district_year$adm2_guid %in% planted
  )
}

cli::cli_alert_info(
  "SPI below threshold only among simulated shortfalls: \\
   {n_planted('SPI below threshold only')}"
)
cli::cli_alert_info(
  "Both below among simulated shortfalls: \\
   {n_planted('Both below')}"
)

## ---------------------------------------------------------------------------##
# Three-panel SPI and NPAFP figure (paper Figure 2 style) ----------------------
## ---------------------------------------------------------------------------##

# Panel A: conventional NPAFP rate, POLIS canonical breaks, red below target
# Panel B: posterior median SPI, paper's operational breaks, red below cut
# Panel C: the four SPI and NPAFP categories

cli::cli_h2("Three-panel SPI and NPAFP map for {focal_year}")

maps <- spi::spi_compare_npafp_maps(
  comparison = conc,
  boundaries = boundaries,
  year = focal_year
)

if (interactive()) {
  print(maps)
}

# Save to disk for the manuscript / desk-review folder. Keep the canvas
# wide and short to limit empty space around the maps -- match height to the boundaries' aspect ratio.
# ggplot2::ggsave(
#   "03_output/main/figures/concordance_maps_2023.png",
#   maps, width = 18, height = 6, dpi = 300, bg = "white"
# )

## ---------------------------------------------------------------------------##
# SPI field guide -- STEPS review and labels (paper: 2t) ---------------------
## ---------------------------------------------------------------------------##

# Reviews every district-year with an SPI below 1 through the five STEPS
# components (strength, timeliness, extent, persistence, stool adequacy) and
# assigns review priority / monitor / no SPI indication. Timeliness and stool
# adequacy need the AFP process counts; the adjacency graph, monthly SPI and
# AFP poliovirus detection table add context outside STEPS.

cli::cli_h2("SPI field guide")

detections <- synth$virus_outcome |>
  dplyr::filter(any_virus == 1) |>
  dplyr::select(adm2_guid, year)

fg <- spi::spi_field_guide(
  comparison = conc,
  process = synth$afp_process,
  adjacency = adj,
  spi_month = spi_dm,
  detections = detections,
  verbose = TRUE
)

print(fg)
summary(fg) # adds STEPS concern counts + the STEPS reference

# Explain the results using worked examples from this run:
if (interactive()) {
  spi::spi_field_guide_help("all", guide = fg)
}

# Field guide teaching table (Table 2 style): the five STEPS components for
# four rule-selected districts, cells shaded by concern. "scan" lists every
# district by review label; "worked" walks the four teaching cases.
scan <- spi::spi_field_guide_table(fg, engine = "gt", layout = "scan")
worked <- spi::spi_field_guide_table(fg, engine = "gt", layout = "worked")
if (interactive()) {
  print(worked)
}

# Save for the manuscript / desk-review folder (format inferred from extension):
# spi::spi_field_guide_table(
#   fg, engine = "flextable", layout = "worked",
#   file = "03_output/main/tables/spi_field_guide_2023.docx"
# )

# Finished ---------------------------------------------------------------------

cli::cli_rule(
  left = "Complete",
  right = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
