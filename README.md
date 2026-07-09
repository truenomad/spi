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

# 1. spatial adjacency from district boundaries
adj <- bs_adjacency(
  boundaries = district_sf,
  id_col     = "district_id"
)

# 2. fit BYM2 expected-count model (INLA)
fit <- bs_expected(
  cases      = afp_cases,
  population = pop_u15,
  adjacency  = adj,
  covariates = covariate_df,
  id_col     = "district_id"
)

# 3. sanity-check: is a negative-binomial fit warranted?
bs_compare_overdispersion(fit)

# 4. surveillance performance index at any aggregation level
spi_dy <- bs_spi(fit, level = "district_year")   # annual per district
spi_dm <- bs_spi(fit, level = "district_month")  # monthly per district

# 5. classify districts into operational response categories
cls <- bs_classify(spi_dy)
plot(cls)
```

## Framework

**Expected rate model.** Poisson regression with BYM2 spatial random effects (INLA). Log person-time offset. The intercept and covariates estimate the background detection rate from the data rather than imposing a fixed threshold. The model learns what each district should be seeing. Posterior draws of the expected count become the SPI denominator.

**Surveillance Performance Index.** SPI is the ratio of observed to expected counts, computed per posterior draw so the joint uncertainty in the denominator is preserved. SPI ~= 1 means detection matches expectation; SPI < 1 flags underdetection (a blindspot); SPI > 1 flags over-detection or genuine excess.

**Operational classification.** Each district maps to a category with a specific field response: green (adequate, no action), amber-SPI (uncertain, investigate), red (degraded, immediate field investigation), structural (persistent zero with high expected count, expand surveillance network).

## Exported functions

```r
bs_adjacency()              # spatial neighbour matrix from sf
bs_expected()               # fit BYM2 expected-count model (INLA)
bs_compare_overdispersion() # Poisson vs negative-binomial diagnostic
bs_spi()                    # surveillance performance index + posterior
bs_classify()               # operational classification of SPI
```

Each returns a typed object with `print`, `summary`, `plot`, and
`as_tibble` methods.

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
