
<!-- README.md is generated from README.Rmd. Please edit that file, then run
     `devtools::build_readme()` (or knit) to regenerate README.md. -->

# spi <img src="man/figures/logo.png" align="right" height="139" alt="spi package logo" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/truenomad/spi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/truenomad/spi/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/truenomad/spi/graph/badge.svg?token=vBneu9acox)](https://codecov.io/gh/truenomad/spi)
[![pkgdown](https://github.com/truenomad/spi/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/truenomad/spi/actions/workflows/pkgdown.yaml)
[![R \>=
4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://cran.r-project.org/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23008378.svg)](https://doi.org/10.5281/zenodo.23008378)

<!-- badges: end -->

spi is an R package for identifying changes in non-polio acute flaccid
paralysis (NPAFP) reporting relative to a district's preceding reporting
history. The surveillance performance index (SPI) compares reported
NPAFP cases with the number expected, then compares the district's
observed-to-expected ratio with the corresponding national ratio.
Expected counts can be estimated using the full spatial and temporal
model or directly from each district's own earlier reporting.

SPI complements the conventional NPAFP rate. The NPAFP rate measures the
absolute level of reporting, whereas SPI measures reporting relative to
preceding reporting and the national pattern. A district can therefore
continue to meet the NPAFP target while reporting substantially fewer
cases than expected from its history. Conversely, a district with
persistently low reporting can have an SPI near 1 while its NPAFP rate
remains below the target. SPI should therefore be interpreted alongside
the NPAFP rate, timeliness, specimen quality, and other surveillance
information.

## Installation

Install from GitHub:

``` r
# Install pak if needed
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("truenomad/spi")
```

The model-based functions, such as `spi_expected()` and `spi_index()`,
need INLA, which is not on CRAN. `spi_direct()` needs neither INLA nor
district boundaries. Install INLA with:

``` r
install.packages(
  "INLA",
  repos = c("https://cloud.r-project.org", "https://inla.r-inla-download.org/R/stable/")
)
```

## Overview

For the direct SPI, start with annual NPAFP counts and under-15
population estimates by district. `spi_direct()` calculates annual SPI
without INLA or district boundaries. For the model-based SPI, start with
monthly NPAFP counts, annual under-15 population estimates, and district
boundaries. `spi_index()` estimates expected counts with the spatial and
temporal model and calculates annual or monthly SPI with credible
intervals. The package also produces plots and maps, compares SPI with
the conventional NPAFP target, and brings the results together for
district review in the STEPS field guide.

## Quick start

The bundled `synth_surveillance` data cover 236 districts in the
fictional country Harad, from 2015 to 2024. All examples use synthetic
data and require no access to GPEI POLIS.

``` r
library(spi)
```

``` r
synth <- synth_surveillance
```

### The direct SPI

Use the direct SPI if you want SPI without fitting a statistical model.
It needs no INLA and no district boundaries, only annual counts and
population by district. The bundled counts are monthly, so add them up
to years first:

``` r
annual <- synth$cases |>
  dplyr::mutate(year = as.integer(format(month, "%Y"))) |>
  dplyr::summarise(count = sum(count), .by = c(adm2_guid, year))
```

Calculate annual SPI:

``` r
spi_direct_dy <- spi_direct(
  annual,
  population = synth$population,
  id_col = "adm2_guid",
  count_col = "count",
  pop_col = "pop_u15",
  first_assessment = 2018,
  verbose = FALSE
)
```

View the results:

``` r
spi_direct_dy$summary |>
  dplyr::select(adm2_guid, year, observed, expected, oe, spi) |>
  dplyr::slice_head(n = 6)
#> # A tibble: 6 x 6
#>   adm2_guid                               year observed expected    oe   spi
#>   <chr>                                  <int>    <dbl>    <dbl> <dbl> <dbl>
#> 1 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2018        2     1.08 1.86  1.69 
#> 2 {040649D0-A0BA-C005-ADEF-90E3E4358730}  2018        7     6.74 1.04  0.946
#> 3 {04CBADAF-5577-E1AB-12F7-49CD628AA70F}  2018       11    25.7  0.428 0.391
#> 4 {0673C3E5-5534-6578-70F4-B6B48BA0D331}  2018       52    40.6  1.28  1.17 
#> 5 {069B1B50-BB32-0C5A-DC20-C9643EC4583D}  2018        2     2.49 0.803 0.732
#> 6 {07BC4455-2DED-3E72-89F5-F1123A963742}  2018        4     9.65 0.415 0.378
```

`observed` and `expected` are the reported and expected cases, `oe` is
their ratio, and `spi` is that ratio divided by the national ratio for
the year. `spi_direct_explain()` shows how one district's value was
calculated:

``` r
spi_direct_explain(
  spi_direct_dy,
  district = spi_direct_dy$summary$adm2_guid[1],
  year = 2023
)
#> 
#> -- District {01325AA0-BEA1-66FE-9B5C-88AA603382F3}, 2023 -----------------------
#> 
#> -- Current reporting --
#> 
#> Observed NPAFP cases                  1
#> Current population under 15     205 717
#> NPAFP rate                         0.49
#> 
#> -- Previous reporting --
#> 
#> Previous years                        8
#> Previous NPAFP cases                 16
#> Previous child-years          1 526 968
#> Previous rate                      1.05
#> 
#> -- Expected reporting --
#> 
#> Expected NPAFP cases               2.16
#> 
#> -- SPI --
#> 
#> District observed / expected       0.46
#> National observed / expected       1.14
#> SPI                                0.41
#> 
#> -- Data checks --
#> 
#> Population check                no flag
```

The direct SPI gives one value per district and year, without credible
intervals. With a table that has the columns `district`, `year`,
`npafp_cases` and `population_u15`, `spi_direct(afp_data)` needs no
other argument.

For the full direct SPI workflow, see [The direct
SPI](https://truenomad.github.io/spi/articles/spi-direct.html).

### The model-based SPI

Use the model-based SPI when you need credible intervals or monthly
results. `spi_index()` estimates each assessment year's expected counts
from the preceding years only, so it refits the model once per year.
This example assesses 2018 to 2024 without covariates.

Calculate annual SPI, passing the district boundaries directly:

``` r
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

``` r
spi_dy |>
  tibble::as_tibble() |>
  dplyr::select(adm2_guid, year, spi_median, spi_q05, spi_q95) |>
  dplyr::slice_head(n = 6)
#> # A tibble: 6 x 5
#>   adm2_guid                               year spi_median spi_q05 spi_q95
#>   <chr>                                  <dbl>      <dbl>   <dbl>   <dbl>
#> 1 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2018      0.882   0.489   1.65 
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

For the full model-based SPI workflow, see [Getting started with
SPI](https://truenomad.github.io/spi/articles/spi-getting-started.html).

## Recommended workflow

For the direct SPI:

1.  Check the table with `spi_check_inputs(method = "direct")`.
2.  Calculate annual SPI with `spi_direct()`.
3.  Review the districts flagged in `population_qc` before interpreting
    their SPI.
4.  Check the national observed-to-expected ratio.
5.  Inspect individual districts with `spi_direct_explain()`.

For the model-based SPI:

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

The [package website](https://truenomad.github.io/spi/) includes these
articles:

- [The direct
  SPI](https://truenomad.github.io/spi/articles/spi-direct.html): work
  through the annual calculation without INLA.
- [Preparing data for
  SPI](https://truenomad.github.io/spi/articles/spi-data-preparation.html):
  prepare counts, population estimates, and district identifiers.
- [Using SPI with POLIS
  data](https://truenomad.github.io/spi/articles/spi-polis.html): go
  from the POLIS AFP line list cleaned with polished to the direct and
  model-based SPI.
- [Getting started with
  SPI](https://truenomad.github.io/spi/articles/spi-getting-started.html):
  calculate the model-based SPI and inspect outputs.
- [How SPI
  works](https://truenomad.github.io/spi/articles/spi-methods.html): the
  model, its diagram, the national comparison, and limitations.
- [Interpreting
  SPI](https://truenomad.github.io/spi/articles/spi-interpretation.html):
  understand SPI alongside the NPAFP rate, timeliness, and stool
  adequacy.
- [Using SPI for district
  review](https://truenomad.github.io/spi/articles/spi-field-guide.html):
  compare SPI with the NPAFP target, run the STEPS field guide, and
  produce district reports.
- [Model options and
  sensitivity](https://truenomad.github.io/spi/articles/spi-model-options.html):
  explore covariates, overdispersion, spatial structure, and sensitivity
  analyses.

The [function reference](https://truenomad.github.io/spi/reference/)
documents all exported functions.

## Citation

To cite `spi` in publications, run `citation("spi")` in R, or use:

> Yusuf, Mohamed A. and Nnanatu, Chibuzor Christopher (2026). *spi:
> Bayesian Spatiotemporal Modelling of Relative AFP Reporting*. R
> package version 0.2.0. <https://doi.org/10.5281/zenodo.23008378>

    @Manual{spi,
      title  = {spi: Bayesian Spatiotemporal Modelling of Relative AFP Reporting},
      author = {Mohamed A. Yusuf and Chibuzor Christopher Nnanatu},
      year   = {2026},
      note   = {R package version 0.2.0},
      url    = {https://github.com/truenomad/spi},
      doi    = {10.5281/zenodo.23008378},
    }

## Related package

For users with POLIS access under a GPEI data-sharing agreement,
[**polished**](https://truenomad.github.io/polished/) retrieves and
prepares data from the GPEI Polio Information System (POLIS) and
calculates indicators such as the NPAFP rate, timeliness, and stool
adequacy.

## License

MIT (c) 2026 Mohamed A. Yusuf
