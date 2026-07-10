# blindspot <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/truenomad/blindspot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/truenomad/blindspot/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/truenomad/blindspot/graph/badge.svg)](https://app.codecov.io/gh/truenomad/blindspot)
[![lint](https://github.com/truenomad/blindspot/actions/workflows/lint.yaml/badge.svg)](https://github.com/truenomad/blindspot/actions/workflows/lint.yaml)
[![pkgdown](https://github.com/truenomad/blindspot/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/truenomad/blindspot/actions/workflows/pkgdown.yaml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://cran.r-project.org/)

<!-- badges: end -->

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

## Example

```r
library(blindspot)

# bundled synthetic surveillance system -- runs without restricted POLIS data
synth <- synth_surveillance

# 1. spatial adjacency from district boundaries
adj <- bs_adjacency(
  boundaries = synth$boundaries,
  id_col     = "adm2_guid"
)

# 2. fit BYM2 expected-count model (INLA):
#    BYM2 space + IID year + harmonic season + IID overdispersion
fit <- bs_expected(
  cases          = synth$cases,
  population     = synth$population,
  adjacency      = adj,
  id_col         = "adm2_guid",
  season         = "harmonic",
  year_effect    = "iid",
  overdispersion = "iid"
)

# 3. sanity-check: does the overdispersion term earn its keep?
bs_compare_overdispersion(
  cases      = synth$cases,
  population = synth$population,
  adjacency  = adj,
  id_col     = "adm2_guid",
  specs      = c("none", "iid", "nb")
)

# 4. surveillance performance index at any aggregation level
spi_dy <- bs_spi(fit, level = "district_year")   # annual per district
spi_dm <- bs_spi(fit, level = "district_month")  # monthly per district

# 5. cross-classify SPI against the conventional NPAFP-rate threshold
conc <- bs_concordance(
  spi        = spi_dy,
  cases      = synth$cases,
  population = synth$population,
  boundaries = synth$boundaries
)
summary(conc)

# 6. three-panel concordance map: NPAFP rate | SPI | where they disagree
bs_concordance_maps(conc, boundaries = synth$boundaries, year = 2023)
```

The full paper reproduction ships as a script:

```r
file.edit(system.file("examples/paper_analysis.R", package = "blindspot"))
```

## Framework

**Expected rate model.** Poisson regression with BYM2 spatial random effects (INLA). Log person-time offset. The intercept and covariates estimate the background detection rate from the data rather than imposing a fixed threshold. The model learns what each district should be seeing. Posterior draws of the expected count become the SPI denominator.

**Surveillance Performance Index.** SPI is the ratio of observed to expected counts, computed per posterior draw so the joint uncertainty in the denominator is preserved. SPI ~= 1 means detection matches expectation; SPI < 1 flags underdetection (a blindspot); SPI > 1 flags over-detection or genuine excess.

**Concordance against the conventional threshold.** Rather than a fixed pass/fail rule, each district-year's SPI is cross-classified against the conventional NPAFP-rate target (>= 3 non-polio AFP cases per 100,000 under-15 person-years) into a 2x2: _both adequate_, _true shortfall_ (both metrics flag failure), _false alarm_ (NPAFP fails but SPI is fine), and _false reassurance_ (NPAFP looks adequate but SPI flags under-detection). The false-reassurance cell is the blindspot a raw threshold misses.

## Exported functions

```r
bs_adjacency()              # spatial neighbour graph from sf boundaries
bs_expected()               # fit BYM2 expected-count model (INLA)
bs_compare_overdispersion() # none vs IID vs negative-binomial diagnostic
bs_spi()                    # surveillance performance index + posterior draws
bs_concordance()            # cross-classify SPI vs the NPAFP-rate threshold
bs_concordance_maps()       # three-panel concordance map (ggplot2/patchwork)
bs_field_guide()            # read SPI to FLAG / WATCH / No-action verdicts
bs_field_guide_table()      # render the field guide (gt / flextable)
bs_field_guide_help()       # learn to read the field guide (worked example)
```

`bs_expected()`, `bs_spi()`, `bs_concordance()`, and `bs_field_guide()` each
return a typed object with `print`, `summary`, and `as_tibble` methods; the
`bs_spi()` and `bs_concordance()` objects also have a `plot()` method.

## Applications

The framework is disease-agnostic. The core engine operates on case counts, population denominators, and spatial boundaries. Designed for AFP/polio surveillance in the polio endgame; applicable to measles discard surveillance, DHIS2 reporting completeness monitoring, and other case-based VPD surveillance systems. Changing the application requires changing the data and covariates, not the methodology.

## The name

A blindspot is a district where the surveillance system cannot see. The package finds them, quantifies the uncertainty, and tells you what to do about them.

## Citation

```r
Yusuf MA (2026). blindspot: Bayesian spatiotemporal
  surveillance quality monitoring. R package version 0.1.0.
  https://github.com/truenomad/blindspot
```

## Related packages

**poliprep:** POLIS data cleaning and preparation. Upstream of blindspot.

**AgePopDenom:** DHS-anchored age-structured population estimates. Provides the denominator input.

## License

MIT &copy; 2026 Mohamed A. Yusuf
