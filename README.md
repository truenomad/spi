# blindspot <img src="man/figures/logo.png" align="right" height="139" alt="" />

> **Find the districts where the surveillance system can't see**

_In the districts that report no cases, is the silence real?_

blindspot estimates, for each district in each month, how many cases a surveillance system should be detecting given its health facilities, population, care-seeking patterns, conflict exposure, and the detection history of its neighbours. The ratio of what is detected to what is expected defines a surveillance performance index (SPI) that replaces binary pass/fail threshold classifications with a continuous, uncertainty-quantified measure of detection capacity.

**Designed for polio AFP surveillance. Applicable to any case-based VPD surveillance system.**

## Installation

```r
# from r-universe (recommended)
install.packages(
  "blindspot",
  repos = c(
    "https://truenomad.r-universe.dev",
    "https://inla.r-inla-download.org/R/stable/",
    "https://cloud.r-project.org"
  )
)

# or from github
pak::pak("truenomad/blindspot")
```

## Minimal example

```r
# one function call: raw data in, classified output out
result <- blindspot::bs_pipeline(
  cases = afp_cases,
  population = pop_u15,
  boundaries = district_sf,
  covariates = covariate_df,
  period = c("2015-01-01", "2025-12-31")
)

# query outputs
result$spi          # SPI posterior summaries
result$trend        # trend direction + alerts
result$changepoints # acute disruption alerts
result$classified   # composite: green/amber/red

# generate desk review report
blindspot::bs_report(
  result,
  country = "Nigeria",
  output_dir = here::here("outputs")
)
```

## Step-by-step example

```r
# for users who want control over each layer

# step 1: build adjacency matrix from boundaries
adj <- blindspot::bs_adjacency(
  boundaries = district_sf,
  id_col = "district_id"
)

# step 2: fit the expected rate model
fit <- blindspot::bs_expected(
  cases = afp_cases,
  population = pop_u15,
  adjacency = adj,
  covariates = covariate_df
)

# step 3: estimate delay distribution
delay <- blindspot::bs_fit_delay(
  linelist = afp_linelist,
  strata = c("country_id", "year")
)

# step 4: adjust recent counts for reporting delays
adjusted <- blindspot::bs_adjust(
  cases = afp_cases,
  delay = delay,
  extraction_date = as.Date("2025-12-15")
)

# step 5: compute SPI
spi <- blindspot::bs_spi(
  expected = fit,
  adjusted = adjusted
)

# step 6: temporal monitoring
trend <- blindspot::bs_trend(spi = spi)
cp <- blindspot::bs_changepoint(cases = afp_cases)

# step 7: classify
classified <- blindspot::bs_classify(
  spi = spi,
  trend = trend,
  changepoints = cp
)

# step 8: diagnostics
diag <- blindspot::bs_diagnose(expected = fit)
plot(diag)
```

## Framework overview

**Layer 1: expected rate model.** Poisson regression with BYM2 spatial random effects (INLA). Log person-time offset. The intercept and covariates estimate the background detection rate from the data rather than imposing a fixed threshold. The model learns what each district should be seeing. Posterior draws of the expected count become the SPI denominator.

**Layer 2: delay adjustment + trend estimation.** Truncation correction using empirically estimated onset-to-notification delay distributions inflates recent counts to compensate for reporting incompleteness. A first-order random walk (RW1) in INLA on monthly SPI estimates the temporal trend, flagging sustained decline before the annual threshold would notice.

**Layer 3: changepoint detection.** Bayesian online changepoint detection (BOCPD) on the raw count series catches acute disruptions (officer transfers, conflict events, specimen transport failures) within 1-2 months. Complements the gradual-trend detector in Layer 2.

**Output:** A composite classification for each district-month: green (adequate, no action), amber-trend (declining, schedule supervision), amber-SPI (uncertain, investigate), red (degraded or acute disruption, immediate field investigation), structural (persistent zero with high expected count, expand surveillance network). Each category maps to a specific operational response.

## All exported functions

```r
bs_expected()      # fit BYM2 expected rate model
bs_adjust()        # delay-adjust recent counts
bs_spi()           # compute SPI + posterior intervals
bs_trend()         # RW1 trend + decline alerts
bs_changepoint()   # BOCPD acute disruption detection
bs_classify()      # composite classification
bs_pipeline()      # end-to-end wrapper
bs_report()        # Quarto desk review template
bs_validate()      # simulated degradation experiment
bs_fit_delay()     # estimate delay distribution
bs_adjacency()     # build neighbour matrix from sf
bs_simulate()      # synthetic example data generator
bs_diagnose()      # model fit diagnostics
```

## Applications

The framework is disease-agnostic. The core engine operates on case counts, population denominators, and spatial boundaries. Vignettes demonstrate: AFP/polio surveillance (primary), measles discard surveillance, DHIS2 reporting completeness monitoring, and AEFI pharmacovigilance. Changing the application requires changing the data and covariates, not the methodology.

## The name

A blindspot is a district where the surveillance system cannot see. The package finds them, quantifies the uncertainty, tracks whether they're getting better or worse, and tells you what to do about them.

## Citation

```r
Yusuf MA (2026). blindspot: Bayesian spatiotemporal
  surveillance quality monitoring. R package version 0.1.0.
  https://github.com/truenomad/blindspot
```

## Related packages

**poliprep:** POLIS data cleaning and preparation. Upstream of blindspot.

**AgePopDenom:** DHS-anchored age-structured population estimates. Provides the denominator input.

**EpiNow2:** Real-time Rt estimation. blindspot's delay adjustment was inspired by the truncation correction literature but does not use EpiNow2 directly.

## License

MIT © 2026 Mohamed A. Yusuf
