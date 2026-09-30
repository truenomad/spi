<!-- README.md is generated from README.Rmd. Please edit that file, then run
     `devtools::build_readme()` (or knit) to regenerate README.md. -->

# spi <img src="man/figures/logo.png" align="right" height="139" alt="spi package logo" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/truenomad/spi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/truenomad/spi/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/truenomad/spi/graph/badge.svg?token=vBneu9acox)](https://codecov.io/gh/truenomad/spi)
[![pkgdown](https://github.com/truenomad/spi/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/truenomad/spi/actions/workflows/pkgdown.yaml)
[![R >=
4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://cran.r-project.org/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23008378.svg)](https://doi.org/10.5281/zenodo.23008378)

<!-- badges: end -->

spi is an R package for comparing reported non-polio acute flaccid paralysis
(NPAFP) cases with the number expected from past reporting and population
size. The surveillance performance index (SPI) divides reported cases by
expected cases. By default, it then divides this district ratio by the
national ratio for the same period. Use SPI alongside the conventional NPAFP
rate, timeliness, and specimen quality.

## Installation

Install from GitHub:

```r
# Install pak if needed
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("truenomad/spi")
```

The model-based functions, such as `spi_expected()` and `spi_index()`, need INLA, which is not on CRAN. `spi_direct()` needs neither INLA nor district boundaries. Install INLA with:

```r
install.packages(
  "INLA",
  repos = c("https://cloud.r-project.org", "https://inla.r-inla-download.org/R/stable/")
)
```

## Overview

For the model-based SPI, start with monthly NPAFP counts, annual under-15
population estimates, and district boundaries. spi estimates expected counts,
calculates annual or monthly SPI with credible intervals, and produces plots
and maps. For the direct SPI, use annual counts and population estimates with
`spi_direct()`; district boundaries are optional. The package also compares
SPI with the conventional NPAFP target and brings the results together for
district review in the STEPS field guide.

A district that previously reported many cases can report far fewer and still
meet the conventional NPAFP target. SPI can help identify this decline because
it uses past reporting to estimate expected counts. However, if a district has
reported few cases for years, its expected count may also be low. Its SPI can
then be near 1 even while its NPAFP rate remains below the target.

## Quick start

The bundled `synth_surveillance` data cover 236 districts in the
fictional country Harad, from 2015 to 2024. All examples use synthetic
data and require no access to GPEI POLIS. `spi_index()` estimates each
assessment year's expected counts from the preceding years only, so it
refits the model once per year. This example assesses 2018 to 2024
without covariates.

```r
library(spi)
```

```r
synth <- synth_surveillance
```

Calculate annual SPI, passing the district boundaries directly:

```r
spi_dy <- spi_index(
  cases = synth$cases,
  population = synth$population,
  adjacency = synth$boundaries,
  first_assessment = 2018,
  id_col = "adm2_guid",
  n_draws = 500,
  seed = 42,
  verbose = FALSE
)
```

View the results:

```r
spi_dy |>
  tibble::as_tibble() |>
  dplyr::select(adm2_guid, year, spi_median, spi_q05, spi_q95) |>
  dplyr::slice_head(n = 6)
#> # A tibble: 6 x 5
#>   adm2_guid                               year spi_median spi_q05 spi_q95
#>   <chr>                                  <dbl>      <dbl>   <dbl>   <dbl>
#> 1 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2018      0.874   0.504   1.65
#> 2 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2019      1.98    1.13    3.71
#> 3 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2020      1.17    0.703   2.00
#> 4 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2021      1.08    0.621   1.91
#> 5 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2022      0.811   0.443   1.45
#> 6 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2023      0.361   0.205   0.593
```

`spi_dy$national` contains the national observed-to-expected ratio. Set
`centre = "none"` for the uncentred district ratio. A district-year with
no reported cases has an SPI of 0 and a 0 to 0 interval, because the
interval reflects uncertainty in the expected count only; this is not
certainty about surveillance performance.

## Recommended workflow

1.  Check the inputs with `spi_check_inputs()`.
2.  Calculate annual SPI with `spi_index()`, which fits the
    expected-count model for each assessment year from the preceding
    years.
3.  Compare SPI with the NPAFP target using `spi_compare_npafp()`.
4.  Check the credible intervals and the national observed-to-expected
    ratio.
5.  Organise district review with `spi_field_guide()` and the STEPS
    components.

## Learn more

The [package website](https://truenomad.github.io/spi/) includes these articles:

- [The direct SPI](https://truenomad.github.io/spi/articles/spi-direct.html): work through the annual calculation without INLA.
- [Preparing data for SPI](https://truenomad.github.io/spi/articles/spi-data-preparation.html): prepare counts, population estimates, and district identifiers.
- [Using SPI with POLIS data](https://truenomad.github.io/spi/articles/spi-polis.html): go from the POLIS AFP line list cleaned with polished to the direct and model-based SPI.
- [Getting started with SPI](https://truenomad.github.io/spi/articles/spi-getting-started.html): calculate the model-based SPI and inspect outputs.
- [How SPI works](https://truenomad.github.io/spi/articles/spi-methods.html): the model, its diagram, the national comparison, and limitations.
- [Interpreting SPI](https://truenomad.github.io/spi/articles/spi-interpretation.html): understand SPI alongside the NPAFP rate, timeliness, and stool adequacy.
- [Using SPI for district review](https://truenomad.github.io/spi/articles/spi-field-guide.html): compare SPI with the NPAFP target, run the STEPS field guide, and produce district reports.
- [Model options and sensitivity](https://truenomad.github.io/spi/articles/spi-model-options.html): explore covariates, overdispersion, spatial structure, and sensitivity analyses.

The [function reference](https://truenomad.github.io/spi/reference/) documents all exported functions.

## Citation

To cite `spi` in publications, run `citation("spi")` in R, or use:

> Yusuf, Mohamed A. and Nnanatu, Chibuzor Christopher (2026). _spi: Bayesian
> Spatiotemporal Modelling of Relative AFP Reporting_. R package version 0.2.0.
> <https://doi.org/10.5281/zenodo.23008378>

```
@Manual{spi,
  title  = {spi: Bayesian Spatiotemporal Modelling of Relative AFP Reporting},
  author = {Mohamed A. Yusuf and Chibuzor Christopher Nnanatu},
  year   = {2026},
  note   = {R package version 0.2.0},
  url    = {https://github.com/truenomad/spi},
  doi    = {10.5281/zenodo.23008378},
}
```

## Related package

For users with POLIS access under a GPEI data-sharing agreement,
[**polished**](https://truenomad.github.io/polished/) retrieves and
prepares data from the GPEI Polio Information System (POLIS) and
calculates indicators such as the NPAFP rate, timeliness, and stool
adequacy.

## License

MIT (c) 2026 Mohamed A. Yusuf
