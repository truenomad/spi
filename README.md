
<!-- README.md is generated from README.Rmd. Please edit that file, then run
     `devtools::build_readme()` (or knit) to regenerate README.md. -->

# spi <img src="man/figures/logo.png" align="right" height="139" alt="spi package logo" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/truenomad/spi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/truenomad/spi/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/truenomad/spi/graph/badge.svg?token=vBneu9acox)](https://codecov.io/gh/truenomad/spi)
[![pkgdown](https://github.com/truenomad/spi/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/truenomad/spi/actions/workflows/pkgdown.yaml)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R \>=
4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://cran.r-project.org/)

<!-- badges: end -->

spi is an R package for identifying relative shortfalls in acute flaccid
paralysis (AFP) reporting. It compares reported non-polio AFP (NPAFP)
counts with modelled expectations based on reporting history and
population, with district observed-to-expected ratios centred on the
national ratio by default. Use SPI alongside the conventional NPAFP
rate, timeliness, and specimen quality.

## Installation

Install from GitHub:

``` r
# Install pak if needed
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("truenomad/spi")
```

INLA is not on CRAN and must be installed separately:

``` r
install.packages(
  "INLA",
  repos = c("https://cloud.r-project.org", "https://inla.r-inla-download.org/R/stable/")
)
```

## Overview

Start with monthly NPAFP counts, annual under-15 population estimates,
and district boundaries. spi estimates expected counts, calculates
annual or monthly SPI with credible intervals, and produces plots and
maps. It can also compare SPI with the conventional NPAFP target and
organise district review through the STEPS field guide.

A historically high-reporting district can decline substantially and
still meet the conventional NPAFP target. SPI can identify that relative
shortfall, especially when expectations are estimated from preceding
years. Conversely, years of low reporting can become the model's
expected baseline, leaving SPI near 1 despite low absolute reporting.
The conventional target remains necessary to identify that persistent
shortfall.

## Quick start

The bundled `synth_surveillance` data cover 236 districts in the
fictional country Harad, from 2015 to 2024. All examples use synthetic
data and require no access to GPEI POLIS. This example fits all supplied
years together without covariates. For annually updated estimates based
only on preceding years, use `spi_prospective()`.

``` r
library(spi)
```

``` r
synth <- synth_surveillance
```

Fit expected monthly counts, passing the district boundaries directly:

``` r
fit_bare <- spi_expected(
  cases = synth$cases,
  population = synth$population,
  adjacency = synth$boundaries,
  id_col = "adm2_guid",
  n_draws = 500,
  seed = 42,
  verbose = FALSE
)
```

Calculate annual SPI and view the results:

``` r
spi_dy <- spi_index(fit_bare, level = "district_year", centre = "national")
```

``` r
spi_dy |>
  tibble::as_tibble() |>
  dplyr::select(adm2_guid, year, spi_median, spi_q05, spi_q95) |>
  dplyr::slice_head(n = 6)
#> # A tibble: 6 x 5
#>   adm2_guid                               year spi_median spi_q05 spi_q95
#>   <chr>                                  <dbl>      <dbl>   <dbl>   <dbl>
#> 1 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2015      0.666   0.460   0.972
#> 2 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2016      1.10    0.753   1.59 
#> 3 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2017      0       0       0    
#> 4 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2018      0.973   0.667   1.42 
#> 5 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2019      2.09    1.44    3.06 
#> 6 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2020      1.48    1.02    2.21
```

`spi_dy$national` contains the national observed-to-expected ratio. Set
`centre = "none"` for the uncentred district ratio. A district-year with
no reported cases has an SPI of 0 and a 0 to 0 interval, because the
interval reflects uncertainty in the expected count only; this is not
certainty about surveillance performance.

## Recommended workflow

1.  Check the inputs with `spi_check_inputs()`.
2.  Fit the expected-count model with `spi_expected()`.
3.  Calculate annual SPI: `spi_prospective()` for routine year-by-year
    assessment, or `spi_index()` to describe a fitted period.
4.  Compare SPI with the NPAFP target using `spi_compare_npafp()`.
5.  Check the credible intervals and the national observed-to-expected
    ratio.
6.  Organise district review with `spi_field_guide()` and the STEPS
    components.

## Learn more

The [package website](https://truenomad.github.io/spi/) includes five
articles:

- [Getting started with
  SPI](https://truenomad.github.io/spi/articles/spi-getting-started.html):
  fit the model, calculate SPI, inspect outputs, and produce prospective
  estimates.
- [How SPI
  works](https://truenomad.github.io/spi/articles/spi-methods.html): the
  model specification, DAG, national centring, and limitations.
- [Interpreting
  SPI](https://truenomad.github.io/spi/articles/spi-interpretation.html):
  understand SPI alongside the NPAFP rate, timeliness, and stool
  adequacy.
- [STEPS and district
  review](https://truenomad.github.io/spi/articles/spi-field-guide.html):
  compare SPI with the NPAFP target and organise district review.
- [Model options and
  sensitivity](https://truenomad.github.io/spi/articles/spi-model-options.html):
  explore covariates, overdispersion, spatial structure, and sensitivity
  analyses.

The [function reference](https://truenomad.github.io/spi/reference/)
documents all exported functions.

## Citation

``` r
Yusuf MA (2026). spi: Bayesian spatiotemporal
  modelling of relative AFP reporting. R package version 0.2.0.
  https://github.com/truenomad/spi
```

## Related package

[**POLISHED**](https://truenomad.github.io/polished/) retrieves and
prepares data from the GPEI Polio Information System (POLIS) and
calculates indicators such as the NPAFP rate, timeliness, and stool
adequacy.

## License

MIT (c) 2026 Mohamed A. Yusuf
