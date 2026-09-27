
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
population, and centres the ratio on the national pattern by default.
Use SPI alongside the conventional NPAFP rate, timeliness, and specimen
quality.

## Installation

Install from GitHub:

``` r
# Install pak if needed
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("truenomad/spi")
```

Model fitting requires INLA. If it is not installed, run:

``` r
install.packages(
  "INLA",
  repos = c("https://cloud.r-project.org", "https://inla.r-inla-download.org/R/stable/")
)
```

## Overview

Start with monthly case counts, annual under-15 population estimates,
and district boundaries. spi fits expected counts, calculates annual or
monthly SPI with credible intervals, and produces plots and maps. It
also compares SPI with the conventional NPAFP target and brings
reporting volume, timeliness, and specimen quality together in a STEPS
field guide.

<details>

<summary>

SPI methods, interpretation, strengths, and limitations
</summary>

### Four complementary views of AFP surveillance

**Absolute volume + relative volume + timeliness + specimen quality.**

| Dimension | Indicator | Question |
|----|----|----|
| Absolute volume | Conventional NPAFP rate | Are enough non-polio AFP cases being reported for the under-15 population? |
| Relative volume | SPI | Is reporting in line with what is expected from this district's reporting history, relative to the national pattern? |
| Timeliness | Notification, investigation, and specimen transport | Are cases notified and investigated, and specimens transported, quickly enough? |
| Specimen quality | Stool adequacy | Are specimens adequate for reliable virological testing? |

A historically high-reporting district can decline substantially and
still meet the conventional NPAFP target. SPI can identify that relative
shortfall, especially when expectations are estimated from preceding
years. Conversely, years of low reporting can become the model's
expected baseline, leaving SPI near 1 despite low absolute reporting.
The conventional target remains necessary to identify that persistent
shortfall.

### Interpreting SPI

SPI = 1 is the natural reference point. Its meaning depends on the
`centre` setting in `spi_index()` or `spi_prospective()`:

| Setting | Comparison | What SPI below 1 means |
|----|----|----|
| `centre = "national"` (default) | District observed-to-expected ratio divided by the national observed-to-expected ratio for the same period | The district's observed-to-expected ratio is below the national ratio. The observed count need not be below its own modelled expectation. |
| `centre = "none"` | District observed count divided by its modelled expected count | Reporting is below the district's own modelled expectation, without national centring. |

With national centring, an SPI of 0.60 means the district's
observed-to-expected ratio is 60% of the national observed-to-expected
ratio for that period. With `centre = "none"`, SPI is the ordinary
district observed-to-expected ratio. Both versions measure relative
reporting volume; neither is an absolute surveillance-adequacy measure.
The examples below use the default national centring.

Read the posterior median with its 90% credible interval. The interval
reflects uncertainty in expected counts, holding observed counts fixed.
SPI does not directly measure missed cases, detection probability,
poliovirus circulation, or overall surveillance adequacy.

Values below 1 are descriptive, not validated action thresholds.
Programmes may choose lower values to focus review on larger shortfalls,
while continuing to consider the conventional NPAFP rate, timeliness,
and specimen quality.

### How SPI works

| Building block | What it contributes |
|----|----|
| Reporting history | Provides a reference for each district's reporting. `spi_prospective()` uses preceding years only; a retrospective `spi_expected()` fit uses the full period supplied, including the years being assessed. |
| Population adjustment | Uses the under-15 population as an offset when estimating expected counts. |
| Partial pooling | Pulls estimates with little supporting data towards patterns in the wider dataset, reducing variation due to sparse counts. |
| Neighbour borrowing | Lets neighbouring districts inform the spatial estimate, with more influence where a district's own data provide little information. |
| Seasonality and year effects | Accounts for recurring within-year patterns and differences between years. |
| National centring (default) | Divides the district's observed-to-expected ratio by the national ratio for the same period. Annual SPI therefore describes reporting relative to that year's national pattern. |
| Uncertainty | Provides posterior medians and credible intervals for the ratio, reflecting uncertainty in expected counts. |

In the accompanying study, preceding district reporting provided most of
the longitudinal reference; partial pooling and spatial borrowing
mattered more where district information was sparse.

#### Model specification

The default model without covariates estimates expected counts for each
district-month:

$$
Y_{it} \sim \text{NegBin}(\mu_{it}, \kappa)
$$

$$
\log \mu_{it} = \log(P_{it}/12) + \beta_0 + f(\text{month}_t) + \gamma_{y(t)} + u_i + v_i
$$

where

- $\log(P_{it}/12)$ is the log under-15 person-time offset
- $\beta_0$ is the background reported NPAFP rate, estimated from the
  data
- $f(\text{month}_t)$ is harmonic seasonality (12- and 6-month
  periodicity)
- $\gamma_{y(t)}$ is an exchangeable year effect
- $u_i + v_i$ is a BYM2 spatial random effect (Riebler et al. 2016)
- $\kappa$ is the negative binomial dispersion, the default
  `overdispersion = "nb"`

Optional covariates add $x_{it}^{\top}\beta$ to the linear predictor.
They are not part of the default SPI used in the accompanying study.

`overdispersion = "iid"` uses a Poisson model with an observation-level
term $\epsilon_{it}$ in the linear predictor. `"none"` fits a Poisson
model without overdispersion. Use `spi_compare_overdispersion()` to
compare the three options.

<img src="man/figures/spi-dag.svg" width="100%" alt="Directed acyclic graph of the SPI estimand" />

Reported counts reflect both underlying NPAFP incidence and surveillance
reporting. For annually updated SPI, `spi_prospective()` uses preceding
years only; retrospective fits use all supplied years.

**National centring.** For district $i$ and year $y$, let $O_{iy}$ be
the annual observed count and $E_{iy}$ the expected count summed across
months. The default index is:

$$
\text{SPI}_{iy} = \frac{O_{iy}/E_{iy}}{r_y}, \qquad
r_y = \frac{\sum_i O_{iy}}{\sum_i \widetilde{E}_{iy}}
$$

Here $\widetilde{E}_{iy}$ is the posterior median expected count. The
national ratio $r_y$ is held fixed when scaling posterior draws.
`centre = "none"` returns the uncentred observed-to-expected ratio.

### Annual and monthly summaries

Use annual SPI for review because monthly AFP counts are often sparse;
monthly SPI is more useful for descriptive trends. `year_end_month`
allows non-calendar reporting years, and incomplete periods should
normally be excluded.

### Reading results in context

The expected count is a statistical reference, not a reporting target or
an estimate of the true number of AFP cases. Interpret SPI within the
country where the model was fitted.

**Consider uncertainty and counts together.** Each estimate has a
posterior median and a 90% credible interval. An interval entirely below
1 provides stronger evidence of a shortfall. If it includes 1, reporting
in line with the relevant reference remains compatible with the data and
model. The interval reflects uncertainty in expected counts, holding
observed counts fixed. It does not include sampling variation in
observed counts or every source of error, such as population estimates
or model specification. When no cases are observed, the ratio and
interval are zero; this alone does not establish a surveillance failure.

**Check the data before interpreting a shortfall.** Review population
estimates, district boundaries, reporting delays, data completeness, and
the observed and expected counts. Small expected counts make ratios
unstable. A rising SPI can result from more reports, a lower
expectation, or both; it does not by itself show improvement.
Differences in underlying NPAFP occurrence can also contribute to a
shortfall.

Use a consistent model specification, centring setting, and training
approach when comparing years; do not mix retrospective and prospective
estimates in one series.

### Limitations

These limits follow from how the reference is estimated and how the
ratio is centred:

| Limitation | What to check |
|----|----|
| Persistent low reporting can become the baseline | A district with years of low reporting can have an SPI near 1. Keep the conventional NPAFP target as an external benchmark; persistent failure to meet it remains a reason for closer review. |
| National change is removed when `centre = "national"` | Review the national observed-to-expected ratio in `spi_dy$national` separately. SPI describes relative district differences, so a shared decline can leave district SPI values largely unchanged. Also inspect absolute reporting rates and counts. |
| Population and boundary errors affect the comparison | Population error affects both expected counts and the conventional rate. Check denominators and use consistent boundaries across years, especially after district splits or mergers. |
| Limited history means more reliance on shared information | Sparse reporting histories give the model less district-specific information. Review the available history and uncertainty before interpreting changes. |
| Spatial borrowing can smooth local differences | Sharing information improves stability but can pull an unusual district towards its neighbours. Check local counts and context when the fitted expectation seems implausible. |
| Results depend on model choices | The training period, spatial structure, seasonality, priors, and centring affect results. Keep these choices consistent when comparing years and assess sensitivity to plausible alternatives. |

SPI does not directly estimate missed cases, detection probability,
poliovirus circulation, outbreak risk, or overall surveillance quality.

### How STEPS supports review

`spi_field_guide()` applies the five STEPS components to relative
reporting shortfalls:

| Letter | Component | What it asks |
|----|----|----|
| S | Strength | How large and how certain is the shortfall? Read from the SPI value and whether its 90% credible interval lies entirely below 1. |
| T | Timeliness | Are specimens reaching the laboratory within 3 days? |
| E | Extent | Is reporting below expectation more common among other districts in the same area than nationally? |
| P | Persistence | Was the SPI also below 1 in the previous year? |
| S | Stool adequacy | Were two adequate stool specimens collected? |

STEPS is not a combined score. Extent and persistence provide supporting
context for a relative reporting shortfall; timeliness and stool
adequacy describe separate dimensions of surveillance. Timeliness and
stool adequacy do not change the label generated by the package.

The field guide generates three review labels:

- **Review priority:** the SPI is below the cutoff, its 90% interval is
  entirely below 1, and extent or persistence supports the shortfall. If
  `noise_alpha` is set, the sampling-noise check must also pass.
- **Monitor:** the SPI is below the chosen cutoff, but the conditions
  for the `Review priority` label are not all met.
- **No SPI indication:** the SPI is at or above the cutoff. Routine
  surveillance and review based on other indicators continue.

These labels organise review; they are not validated measures of
surveillance adequacy or automatic recommendations for field action.

### District reports

`spi_field_guide_pager()` creates an A4 district report combining SPI,
location, STEPS findings, and detection context.

### Focusing review on larger shortfalls

| Chosen SPI cutoff | Districts included | Interpretation with national centring |
|----|----|----|
| `1.00` (package default) | SPI below 1 | District O:E below national O:E |
| `0.80` | SPI below 0.80 | District O:E \>20% below national O:E |
| `0.60` | SPI below 0.60 | District O:E \>40% below national O:E |
| `0.40` | SPI below 0.40 | District O:E \>60% below national O:E |

This table assumes `centre = "national"`, the default. With
`centre = "none"`, the same cutoffs refer to the district's own
uncentred expectation. Lower cutoffs select more extreme volume
shortfalls. They are practical choices for prioritising review, not
validated boundaries between adequate and inadequate surveillance.
Continue to review concerns from timeliness, stool quality, or other
indicators even when a district is above the chosen SPI cutoff.

`spi_cut` also sets the threshold used for extent and persistence, so
both are reassessed at the chosen cutoff. The strength check still
compares the 90% credible interval with 1. To use the same cutoff in the
concordance table, also set `spi_threshold = 0.60` in
`spi_concordance()`. Record the cutoff with the results.

To label only shortfalls whose 90% credible interval lies entirely below
1, set `spi_rule = "interval"` in `spi_concordance()`.
`spi_field_guide()` uses the same rule by default, so a district-year
whose interval includes 1 receives `No SPI indication` rather than
`Monitor`. Record the rule with the results.

</details>

## Quick start

The bundled `synth_surveillance` data cover 236 districts in the
fictional country Harad, from 2015 to 2024. This example fits the
supplied years together, with no covariates. For annually updated
estimates based on preceding years only, use `spi_prospective()`.

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

`spi_dy$national` reports the national observed-to-expected ratio
separately. Use `centre = "none"` in the same call for the district-only
observed-to-expected ratio.

## Examples and options

The following examples use the objects created in the quick start and
require no POLIS access. A longer example is in
`inst/examples/paper_analysis.R`; open it with
`file.edit(system.file("examples/paper_analysis.R", package = "spi"))`.

### Check the inputs first

Use `spi_check_inputs()` to check case counts, population denominators,
and district boundaries before fitting the model. It reports mismatched
district IDs, missing months, and other issues as errors, warnings, or
notes. `spi_expected()` also runs this check and stops on errors.

``` r
spi_check_inputs(
  cases = synth$cases,
  population = synth$population,
  shapefile = synth$boundaries,
  id_col = "adm2_guid"
)
#>
#> -- spi input check -------------------------------------------------------------
#> v All input checks passed -- 236 districts x 120 months (2015-01 to 2024-12).
```

Here is the report for data with one negative count and three missing
months:

``` r
bad_cases <- synth$cases |>
  # Set the first count to -1 to simulate a data-entry error.
  dplyr::mutate(count = dplyr::if_else(dplyr::row_number() == 1, -1, count)) |>
  # Remove three district-months to simulate missing records.
  dplyr::slice(-(2:4))

spi_check_inputs(
  cases = bad_cases,
  population = synth$population,
  shapefile = synth$boundaries,
  id_col = "adm2_guid"
)
#>
#> -- spi input check -------------------------------------------------------------
#> i 236 districts x 120 months (2015-01 to 2024-12)
#> x 1 case row has negative counts
#> ! 3 district-months missing from the panel (gaps)
#> --------------------------------------------------------------------------------
#> x 1 error -- resolve before fitting.
```

### Expected counts

The fitted object contains posterior draws and summaries for each
district-month. View observed counts, expected medians, and 90% credible
intervals:

``` r
fit_bare |>
  tibble::as_tibble() |>
  dplyr::filter(count > 0) |>
  dplyr::select(
    adm2_guid, month, count,
    expected_median, expected_q05, expected_q95
  ) |>
  dplyr::slice_head(n = 6)
#> # A tibble: 6 x 6
#>   adm2_guid           month      count expected_median expected_q05 expected_q95
#>   <chr>               <date>     <int>           <dbl>        <dbl>        <dbl>
#> 1 {54CD979C-CF9D-6A6~ 2015-06-01     1           0.516        0.421        0.636
#> 2 {54CD979C-CF9D-6A6~ 2015-11-01     1           0.574        0.465        0.703
#> 3 {54CD979C-CF9D-6A6~ 2016-01-01     2           0.784        0.641        0.957
#> 4 {54CD979C-CF9D-6A6~ 2016-02-01     1           0.795        0.648        0.969
#> 5 {54CD979C-CF9D-6A6~ 2016-04-01     1           0.728        0.597        0.894
#> 6 {54CD979C-CF9D-6A6~ 2016-05-01     1           0.673        0.551        0.824
```

### SPI summaries and plots

The quick start calculated annual SPI. For monthly summaries:

``` r
spi_dm <- spi_index(fit_bare, level = "district_month")
```

``` r
spi_dy |>
  tibble::as_tibble() |>
  dplyr::select(adm2_guid, year, observed, spi_median, spi_q05, spi_q95) |>
  dplyr::slice_head(n = 6)
#> # A tibble: 6 x 6
#>   adm2_guid                             year observed spi_median spi_q05 spi_q95
#>   <chr>                                <dbl>    <int>      <dbl>   <dbl>   <dbl>
#> 1 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2015        1      0.666   0.460   0.972
#> 2 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2016        2      1.10    0.753   1.59
#> 3 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2017        0      0       0       0
#> 4 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2018        2      0.973   0.667   1.42
#> 5 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2019        5      2.09    1.44    3.06
#> 6 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2020        2      1.48    1.02    2.21
```

Use annual SPI for review and monthly values for descriptive trends. Set
`year_end_month` to use a reporting year other than January–December;
check `n_months` for incomplete periods.

`plot()` offers `funnel`, `distribution`, `caterpillar`, and
`calibration` views:

``` r
plot(spi_dy, type = "funnel")
```

<img src="man/figures/README-spi-funnel-1.png" width="100%" />

The default `distribution` view shows how SPI values are distributed
relative to the reference value of 1.

``` r
plot(spi_dy)
```

<img src="man/figures/README-spi-distribution-1.png" width="100%" />

### Concordance with the conventional NPAFP target

Compare each district-year's SPI with the NPAFP-rate target used here: 3
non-polio AFP cases per 100,000 under-15 person-years. The four
categories show where the two measures agree or differ. The `"SPI only"`
category identifies districts that meet the rate target but have an SPI
below 1.

``` r
conc <- spi_concordance(
  spi = spi_dy,
  cases = synth$cases,
  population = synth$population,
  boundaries = synth$boundaries,
  spi_threshold = 1,
  spi_rule = "median",
  npafp_target = 3,
  verbose = FALSE
)
```

Count the district-years in each category:

``` r
conc$district_year |>
  dplyr::count(concordance, name = "district_years", .drop = FALSE)
#> # A tibble: 4 x 2
#>   concordance     district_years
#>   <fct>                    <int>
#> 1 Neither flagged            972
#> 2 SPI only                  1060
#> 3 NPAFP only                  73
#> 4 Both flagged               255
```

`plot()` shows the four cells as a scatter of NPAFP rate against SPI,
split by the two thresholds (dashed). The `"SPI only"` points sit
bottom-right: the NPAFP target is met, but SPI is below 1.

``` r
plot(conc)
```

<img src="man/figures/README-concordance-scatter-1.png" width="100%" />

### The three-panel map

Three panels for one year: the conventional NPAFP rate, the posterior
median SPI, and where the two disagree.

``` r
spi_concordance_maps(conc, boundaries = synth$boundaries, year = 2023)
```

<img src="man/figures/README-maps-1.png" width="100%" />

### The STEPS field guide

Combine SPI and the conventional NPAFP rate with timeliness and stool
adequacy. The generated labels organise review; they are not validated
judgements of surveillance adequacy.

Supply AFP process counts through `process`. Optional `adjacency`,
`spi_month`, `detections`, and `es` inputs add neighbouring-district,
seasonal, and poliovirus detection context; they do not change the
review label.

``` r
# Optional AFP poliovirus detection records, used only as supporting context.
detections <- synth$virus_outcome |>
  dplyr::filter(any_cvdpv2 == 1) |>
  dplyr::select(adm2_guid, year)

fg <- spi_field_guide(
  concordance = conc,
  process = synth$afp_process,
  adjacency = fit_bare$adjacency,
  spi_month = spi_dm,
  detections = detections,
  es = synth$es_district_year,
  es_col = "n_positive",
  spi_cut = 1,
  verbose = FALSE
)

fg
#> # A tibble: 3 x 3
#>   verdict               n   pct
#>   <chr>             <int> <dbl>
#> 1 Review priority      83  35.2
#> 2 Monitor              60  25.4
#> 3 No SPI indication    93  39.4
#> # A tibble: 83 x 11
#>    district   obs   exp   spi cri       npafp extent persist transport adequacy
#>    <chr>    <int> <dbl> <dbl> <chr>     <dbl> <lgl>  <lgl>       <dbl>    <dbl>
#>  1 Khandor      0   2.4  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  2 Vasheth      0   0.8  0    0.00-0.00   0   TRUE   FALSE          NA       NA
#>  3 Arddor       0   1.7  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  4 Ardor        0   1.3  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  5 Doloth       0   1.1  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  6 Raenan       0   1.1  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  7 Chakis       0   1.9  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  8 Vashoth      0   2.3  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  9 Suzil        1   8.6  0.11 0.09-0.14   1.6 TRUE   TRUE          100      100
#> 10 Nentha       2  16.6  0.12 0.10-0.14   2   TRUE   FALSE         100      100
#> # i 73 more rows
#> # i 1 more variable: verdict <chr>
```

### Focus review on larger shortfalls

Set `spi_cut` to `0.80`, `0.60`, or `0.40` to focus on progressively
larger relative shortfalls. These are practical review choices, not
validated action thresholds.

``` r
fg_focused <- spi_field_guide(
  concordance = conc,
  process = synth$afp_process,
  spi_cut = 0.60
)
```

`spi_cut` also changes the extent and persistence comparisons. The
credible-interval check remains at 1.

`summary(fg)` reports how often each STEPS component raises concern.
`spi_field_guide_help()` explains the components and four worked
examples in the console.

Use `spi_field_guide_table()` to create a `gt` or `flextable` report.
The worked example selects four districts with different findings and
shows their STEPS components, with cells coloured by level of concern.
The image below preserves the table colours when viewed on GitHub.

``` r
spi_field_guide_table(fg, engine = "gt", layout = "worked")
```

<img src="man/figures/README-fg-table.png" alt="Field guide table: five STEPS components for four districts, cells shaded green for reassuring, amber for intermediate, and rose where the finding adds to concern." width="100%" />

Create an A4 report for one district, with its SPI trend, map, STEPS
findings, and detection context. Pass `indicators_df` to add the NPAFP
rate over time and tiles for stool adequacy, timeliness, and EV rate.
The example table below simulates the notify-to-investigation and EV
rate figures. Set `path` to save HTML and PNG files:

``` r
source(system.file("examples", "pager_indicators.R", package = "spi"))
indicators <- example_indicators(fg)

spi_field_guide_pager(
  fg, district = "Tirwen", boundaries = synth$boundaries,
  id_col = "adm2_guid", indicators_df = indicators, path = "reports/"
)
```

<details>

<summary>

One-page field pager for a review priority district
</summary>

<img src="man/figures/README-pager.png" alt="One-page SPI field pager: review label, an SPI chart with a credible interval and a location map, conventional AFP and ES indicators, the five STEPS components, and a judgement banner." width="100%" />

</details>

## Advanced fitting options

See [Model options](vignettes/spi-model-options.Rmd) for covariate
adjustment and overdispersion comparisons, or open
`vignette("spi-model-options", package = "spi")` after installing with
vignettes built.

## Exported functions

``` r
spi_check_inputs()           # check cases, population, and boundaries
spi_adjacency()              # spatial neighbour graph from sf boundaries
spi_expected()              # fit BYM2 expected-count model (INLA)
spi_compare_overdispersion() # none vs IID vs negative-binomial diagnostic
spi_index()                  # surveillance performance index + posterior draws
spi_prospective()            # annually updated index from past years only
spi_concordance()            # cross-classify SPI vs the NPAFP-rate threshold
spi_concordance_maps()       # three-panel concordance map (ggplot2/patchwork)
spi_field_guide()            # STEPS review: priority / monitor / no indication
spi_field_guide_table()      # render the field guide (gt / flextable)
spi_field_guide_pager()      # one-district A4 field pager (html / png)
spi_field_guide_help()       # learn to read the field guide (worked example)
```

`spi_expected()`, `spi_index()`, `spi_concordance()`, and
`spi_field_guide()` each return a typed object with `print` (and, where
useful, `summary` / `as_tibble`) methods; the `spi_index()` and
`spi_concordance()` objects also have a `plot()` method.

## Citation

``` r
Yusuf MA (2026). spi: Bayesian spatiotemporal
  modelling of relative AFP reporting. R package version 0.1.0.9000.
  https://github.com/truenomad/spi
```

## Related package

[**POLISHED**](https://truenomad.github.io/polished/) retrieves and
prepares data from the GPEI Polio Information System (POLIS) and
calculates indicators such as the NPAFP rate, timeliness, and stool
adequacy.

## License

MIT (c) 2026 Mohamed A. Yusuf
