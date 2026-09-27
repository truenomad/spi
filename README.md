
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

> **Identify relative shortfalls in AFP reporting**

spi estimates district-level relative AFP reporting by comparing
observed non-polio acute flaccid paralysis (NPAFP) counts with modelled
expectations from reporting history and population, with partial pooling
across districts and adjustment for seasonal and annual patterns. Use
SPI alongside the conventional NPAFP rate, timeliness, and specimen
quality.

**Built for polio AFP surveillance.** The model can also be used with
other case-based surveillance data.

## Four complementary views of AFP surveillance

**Absolute volume + relative volume + timeliness + specimen quality.**
Each answers a different question:

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

Use the NPAFP rate and SPI together to review reporting volume, then
consider timeliness and specimen quality. STEPS organises the review of
a low SPI by examining its strength, timeliness, extent, persistence,
and stool adequacy.

## Quick interpretation

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

## Installation

Install from GitHub:

``` r
# Install pak if needed
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("truenomad/spi")
```

## Quick start

This example fits the bundled synthetic data for the fictional country
Harad. It uses a retrospective fit over the supplied years, with no
covariates. The accompanying study's annually updated SPI uses preceding
years only; use `spi_prospective()` for that approach.

Model fitting requires INLA. If it is not installed, run:

``` r
install.packages(
  "INLA",
  repos = c("https://cloud.r-project.org", "https://inla.r-inla-download.org/R/stable/")
)
```

``` r
library(spi)
```

``` r
synth <- synth_surveillance
```

``` r
adj <- spi_adjacency(synth$boundaries, id_col = "adm2_guid")
```

``` r
fit_bare <- spi_expected(
  cases = synth$cases,
  population = synth$population,
  adjacency = adj,
  id_col = "adm2_guid",
  n_draws = 500,
  seed = 42,
  verbose = FALSE
)
```

``` r
spi_dy <- spi_index(fit_bare, level = "district_year", centre = "national")
```

``` r
head(as_tibble(spi_dy)[, c("adm2_guid", "year", "spi_median", "spi_q05", "spi_q95")])
#> # A tibble: 6 x 5
#>   adm2_guid                               year spi_median spi_q05 spi_q95
#>   <chr>                                  <dbl>      <dbl>   <dbl>   <dbl>
#> 1 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2015      0.675   0.469   0.944
#> 2 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2016      1.11    0.772   1.54
#> 3 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2017      0       0       0
#> 4 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2018      0.983   0.680   1.37
#> 5 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2019      2.12    1.46    2.92
#> 6 {01325AA0-BEA1-66FE-9B5C-88AA603382F3}  2020      1.50    1.04    2.13
```

`spi_dy$national` reports the national observed-to-expected ratio
separately. Use `centre = "none"` in the same call for the district-only
observed-to-expected ratio.

## How the SPI works

SPI compares observed NPAFP reporting with an expectation based on
population and recorded reporting history. The model shares information
across districts to stabilise that expectation, and the index is centred
on the national reporting pattern for the same period by default.

<details>

<summary>

Building blocks and model specification
</summary>

| Building block | What it contributes |
|----|----|
| Reporting history | Provides a reference for each district's reporting. `spi_prospective()` uses preceding years only; a retrospective `spi_expected()` fit uses the full period supplied, including the years being assessed. |
| Population adjustment | Uses the under-15 population as an offset when estimating expected counts. |
| Partial pooling | Pulls estimates with little supporting data towards patterns in the wider dataset, reducing variation due to sparse counts. |
| Neighbour borrowing | Lets neighbouring districts inform the spatial estimate, with more influence where a district's own data provide little information. |
| Seasonality and year effects | Accounts for recurring within-year patterns and differences between years. |
| National centring (default) | Divides the district's observed-to-expected ratio by the national ratio for the same period. Annual SPI therefore describes reporting relative to that year's national pattern. |
| Uncertainty | Provides posterior medians and credible intervals for the ratio, reflecting uncertainty in expected counts. |

In the accompanying study, the district's preceding reporting history
provides most of the longitudinal reference. Population adjustment,
partial pooling, spatial borrowing, seasonality, and year effects
stabilise and structure the expected count, particularly where district
information is sparse. The quick start demonstrates a retrospective fit;
use `spi_prospective()` for expectations based on preceding years only.

### Model specification

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

<img src="man/figures/spi-dag.png" width="100%" alt="Directed acyclic graph of the SPI estimand" />

Observed counts reflect both underlying NPAFP incidence and case finding
and reporting by the surveillance system. The model estimates expected
counts from reporting history, population, spatial structure,
seasonality, and annual variation, with optional covariates. For
annually updated SPI, `spi_prospective()` uses preceding reporting
history only; the retrospective fit uses all supplied years. The SPI
starts with observed ÷ expected and, by default, divides by the national
ratio for the same period. Poliovirus detections are used only for
external comparison; they are not part of the model.

**Why a Bayesian spatial model?** District-month AFP counts are often
small, which makes raw rates unstable. The BYM2 spatial effect shares
information between neighbouring districts. Posterior draws of expected
counts provide credible intervals for the SPI. The package fits the
model using INLA.

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

The model uses case counts, population denominators, and district
boundaries. The examples focus on polio AFP surveillance. Other
applications need suitable counts, denominators, and model settings.

</details>

## Interpretation and limitations

**SPI measures relative reporting volume.** Read it alongside absolute
NPAFP volume, timeliness, and specimen quality. Meeting one measure does
not establish that the other aspects of surveillance are adequate.

**State the comparison used.** With `centre = "national"` (default), SPI
compares the district's observed-to-expected ratio with the national
ratio for the same period. With `centre = "none"`, it compares reporting
directly with the district's modelled expectation. For example, a
district reporting 80% of its own expectation has an uncentred SPI of
0.80. If the national ratio is 0.50, its nationally centred SPI is 1.60:
below its own expectation, but above the national pattern. Record
`centre` with the results.

The expected count is a statistical reference drawn from recorded
surveillance data, rather than a reporting target or an estimate of the
true number of AFP cases. Interpret SPI within the country where the
model was fitted; equal values do not establish equal surveillance
performance across countries.

**Consider uncertainty and counts together.** Each estimate has a
posterior median and a 90% credible interval. An interval entirely below
1 provides stronger evidence of a shortfall. If it includes 1, reporting
at expectation remains compatible with the data and model. The interval
reflects uncertainty in expected counts, holding observed counts fixed.
It does not include sampling variation in observed counts or every
source of error, such as population estimates or model specification.
When no cases are observed, the ratio and interval are zero; this alone
does not establish a surveillance failure.

**Check the data before interpreting a shortfall.** Review population
estimates, district boundaries, reporting delays, data completeness, and
the observed and expected counts. Small expected counts make ratios
unstable. A rising SPI can result from more reports, a lower
expectation, or both; it does not by itself show improvement.
Differences in underlying NPAFP occurrence can also contribute to a
shortfall.

**Keep estimates comparable.** Record the model specification, centring
setting, training period, and assessment year. The examples below use
retrospective estimates from a common fit. `spi_prospective()` instead
refits on preceding years for each assessment year. Use a consistent
procedure for trends, and do not combine retrospective and annually
updated estimates into one series.

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
| Fitting and maintenance take more work | SPI requires model fitting, diagnostics, and documented settings in addition to the data needed for a conventional NPAFP rate. |

SPI is a relative reporting-volume measure. It does not directly
estimate detection probability, missed cases, poliovirus circulation,
outbreak risk, or overall surveillance quality. Use it to prioritise
review alongside the conventional NPAFP rate, timeliness, and stool
quality. STEPS brings these findings together; its components are not a
validated automatic trigger for field action.

## Examples and options

The following examples use the objects created in the quick start and
require no POLIS access. A longer example is in
`inst/examples/paper_analysis.R`; open it with
`file.edit(system.file("examples/paper_analysis.R", package = "spi"))`.

### The data

`synth_surveillance` contains data for the fictional country Harad: 236
districts in 36 provinces, monthly counts from 2015 to 2024, under-15
populations, district boundaries, simulated virus detections, and a
record of the simulated reporting shortfalls.

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
bad <- synth$cases
bad$count[1] <- -1                   # a data-entry slip
bad <- bad[-(2:4), ]                 # three missing district-months

spi_check_inputs(bad, synth$population, synth$boundaries, id_col = "adm2_guid")
#>
#> -- spi input check -------------------------------------------------------------
#> i 236 districts x 120 months (2015-01 to 2024-12)
#> x 1 case row has negative counts
#> ! 3 district-months missing from the panel (gaps)
#> --------------------------------------------------------------------------------
#> x 1 error -- resolve before fitting.
```

### Spatial neighbours

The quick start used `spi_adjacency()` to turn district polygons into a
neighbour graph. The model uses this graph to share information between
neighbouring districts.

### Expected counts

`spi_expected()` estimates expected monthly counts. The default model
has a BYM2 spatial effect, an independent year effect for changes across
the surveillance system, harmonic seasonality, negative binomial
overdispersion, and a log person-time offset. Set
`overdispersion = "auto"` to compare the three overdispersion options
and refit the recommended model.

The result includes posterior draws and summaries of expected counts for
every district-month. Here are the observed counts, expected medians,
and 90% credible intervals:

``` r
as_tibble(fit_bare) |>
  dplyr::filter(count > 0) |>
  dplyr::select(
    adm2_guid, month, count,
    expected_median, expected_q05, expected_q95
  ) |>
  head()
#> # A tibble: 6 x 6
#>   adm2_guid           month      count expected_median expected_q05 expected_q95
#>   <chr>               <date>     <int>           <dbl>        <dbl>        <dbl>
#> 1 {54CD979C-CF9D-6A6~ 2015-06-01     1           0.517        0.432        0.634
#> 2 {54CD979C-CF9D-6A6~ 2015-11-01     1           0.577        0.483        0.701
#> 3 {54CD979C-CF9D-6A6~ 2016-01-01     2           0.784        0.655        0.949
#> 4 {54CD979C-CF9D-6A6~ 2016-02-01     1           0.795        0.663        0.957
#> 5 {54CD979C-CF9D-6A6~ 2016-04-01     1           0.730        0.609        0.890
#> 6 {54CD979C-CF9D-6A6~ 2016-05-01     1           0.676        0.563        0.819
```

### Adjusting for covariates

Covariates are optional and are not part of the default SPI used in the
accompanying study. Pass district-year covariates to `spi_expected()`
through the `covariates` argument. The example data include DTP3
coverage, urban population share, and travel time to care:

``` r
head(synth$covariates)
#>                                adm2_guid year dtp3 urban_prop travel_time_min
#> 1 {01325AA0-BEA1-66FE-9B5C-88AA603382F3} 2015 84.8      0.566            15.9
#> 2 {01325AA0-BEA1-66FE-9B5C-88AA603382F3} 2016 89.9      0.566            16.5
#> 3 {01325AA0-BEA1-66FE-9B5C-88AA603382F3} 2017 89.9      0.566            17.1
#> 4 {01325AA0-BEA1-66FE-9B5C-88AA603382F3} 2018 87.3      0.566            23.2
#> 5 {01325AA0-BEA1-66FE-9B5C-88AA603382F3} 2019 89.9      0.566            19.3
#> 6 {01325AA0-BEA1-66FE-9B5C-88AA603382F3} 2020 87.6      0.566            22.1
```

Pass them in, log-transforming the skewed travel-time column:

``` r
fit_adj <- spi_expected(
  cases = synth$cases,
  population = synth$population,
  adjacency = adj,
  covariates = synth$covariates,
  log_transform = "travel_time_min",
  id_col = "adm2_guid",
  n_draws = 500,
  seed = 42,
  verbose = FALSE
)
```

Effects are reported as rate ratios per standard deviation of the
covariate. The model standardises covariates internally. Here are the
three covariate effects, excluding the seasonal terms:

``` r
eff <- summary(fit_adj)$effects
eff[eff$covariate %in% c("dtp3", "urban_prop", "travel_time_min"),
    c("covariate", "rr_median", "rr_q025", "rr_q975", "signif")]
#> # A tibble: 3 x 5
#>   covariate       rr_median rr_q025 rr_q975 signif
#>   <chr>               <dbl>   <dbl>   <dbl> <lgl>
#> 1 dtp3                1.00    0.953    1.05 FALSE
#> 2 urban_prop          1.03    0.938    1.13 FALSE
#> 3 travel_time_min     0.991   0.968    1.01 FALSE
```

Optional covariates can be used for sensitivity analyses or alternative
applications; the default SPI does not require them. The remaining
examples use `fit_bare`, the model without covariates.

### Compare overdispersion options

Compare Poisson, Poisson with an observation-level random effect, and
negative binomial models:

``` r
od <- spi_compare_overdispersion(
  cases = synth$cases,
  population = synth$population,
  adjacency = adj,
  id_col = "adm2_guid",
  specs = c("none", "iid", "nb"),
  n_draws = 100,
  verbose = FALSE
)
```

``` r
od$summary
#> # A tibble: 3 x 11
#>   spec  n_obs    dic   waic p_eff sd_spatial phi_spatial phi_pegged sd_extra
#>   <chr> <int>  <dbl>  <dbl> <dbl>      <dbl>       <dbl> <lgl>         <dbl>
#> 1 none  28320 81917. 81996.  241.      0.865       0.928 FALSE        NA
#> 2 iid   28320 79545. 79529. 5649.      0.863       0.925 FALSE         0.423
#> 3 nb    28320 80299. 80296.  240.      0.868       0.934 FALSE        NA
#> # i 2 more variables: cpo_valid <dbl>, pit_ks <dbl>
```

To skip the manual step, `spi_expected(overdispersion = "auto")` runs
this same comparison internally and refits with the recommended model.

### SPI summaries and plots

For each posterior draw, `spi_index()` divides the observed count by the
expected count, then centres the ratio nationally for the same period.
Annual values sum counts and expectations across months before
calculating the ratio. Set `centre = "none"` for the uncentred ratio.
The quick start calculated annual SPI. Monthly values are also
available:

``` r
spi_dm <- spi_index(fit_bare, level = "district_month")
```

``` r
head(as_tibble(spi_dy)[, c(
  "adm2_guid", "year", "observed", "spi_median", "spi_q05", "spi_q95"
)])
#> # A tibble: 6 x 6
#>   adm2_guid                             year observed spi_median spi_q05 spi_q95
#>   <chr>                                <dbl>    <int>      <dbl>   <dbl>   <dbl>
#> 1 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2015        1      0.675   0.469   0.944
#> 2 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2016        2      1.11    0.772   1.54
#> 3 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2017        0      0       0       0
#> 4 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2018        2      0.983   0.680   1.37
#> 5 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2019        5      2.12    1.46    2.92
#> 6 {01325AA0-BEA1-66FE-9B5C-88AA603382~  2020        2      1.50    1.04    2.13
```

**Use annual SPI for review.** Monthly AFP counts are often 0 or 1, so
monthly ratios vary widely. Annual counts give a more stable basis for
review. The monthly series is useful for trends and the field guide's
seasonal assessment.

The assessment year can end in any month.
`spi_index(level = "district_year", year_end_month = 4)` groups May
through April and labels each period by its ending year. Check
`n_months` for incomplete periods at either end of the series; these
should normally be excluded.

The `plot()` method provides four views: `distribution`, `funnel`,
`caterpillar`, and `calibration`. The funnel plot compares each
district-year's nationally centred SPI with its uncentred model-expected
count. Ratios tend to vary less when counts are larger, which helps
distinguish shortfalls from variation due to small counts.

``` r
plot(spi_dy, type = "funnel")
```

<img src="man/figures/README-spi-funnel-1.png" width="100%" />

The default `distribution` view shows how SPI values are distributed and
whether they are centred near 1.

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
  verbose = FALSE
)
```

Count the district-years in each category:

``` r
table(conc$district_year$concordance)
#>
#> Neither flagged        SPI only      NPAFP only    Both flagged
#>             972            1060              72             256
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

`spi_field_guide()` reviews relative reporting volume alongside
timeliness and specimen quality, with the conventional NPAFP rate shown
for comparison. It also checks whether the relative shortfall is shared
across an area or persists over time. By default, it applies the five
**STEPS** components where SPI is below 1:

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
stool adequacy do not change the label generated by the package. No
component establishes surveillance failure.

The field guide generates three review labels:

- **Review priority:** the SPI is below the cutoff, its 90% interval is
  entirely below 1, and extent or persistence supports the shortfall. If
  `noise_alpha` is set, the sampling-noise check must also pass.
- **Monitor:** the SPI is below the chosen cutoff, but the conditions
  for the `Review priority` label are not all met.
- **No SPI indication:** the SPI is at or above the cutoff. Routine
  surveillance and review based on other indicators continue.

These labels organise the findings for review; they are not validated
judgements of surveillance adequacy or automatic recommendations for
field action. Decisions require the underlying data and other
surveillance evidence. The examples below use synthetic data.

Timeliness and stool adequacy come from `process`, a district-year table
of AFP case counts (`n_cases`, `n_adequate`, `n_transport`,
`n_transport_timely`). Extent uses the `adm1_name` column that
`spi_concordance()` carries when given `boundaries`. Trend,
neighbouring-district context, seasonal patterns, and poliovirus
detections through AFP (`genomic`) or environmental surveillance (`es`)
are reported separately as supporting context.

``` r
genomic <- dplyr::filter(synth$virus_outcome, any_cvdpv2 == 1)

fg <- spi_field_guide(
  concordance = conc,
  process = synth$afp_process,
  adjacency = adj,
  spi_month = spi_dm,
  genomic = genomic[, c("adm2_guid", "year")],
  es = synth$es_district_year,
  es_col = "n_positive",
  verbose = FALSE
)

fg
#> # A tibble: 3 x 3
#>   verdict               n   pct
#>   <chr>             <int> <dbl>
#> 1 Review priority      84  35.6
#> 2 Monitor              59  25
#> 3 No SPI indication    93  39.4
#> # A tibble: 84 x 11
#>    district   obs   exp   spi cri       npafp extent persist transport adequacy
#>    <chr>    <int> <dbl> <dbl> <chr>     <dbl> <lgl>  <lgl>       <dbl>    <dbl>
#>  1 Khandor      0   2.4  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  2 Vasheth      0   0.8  0    0.00-0.00   0   TRUE   FALSE          NA       NA
#>  3 Arddor       0   1.7  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  4 Ardor        0   1.2  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  5 Doloth       0   1.2  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  6 Raenan       0   1    0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  7 Chakis       0   1.9  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  8 Vashoth      0   2.4  0    0.00-0.00   0   TRUE   TRUE           NA       NA
#>  9 Suzil        1   8.6  0.11 0.09-0.14   1.6 TRUE   TRUE          100      100
#> 10 Nentha       2  16.5  0.12 0.10-0.14   2   TRUE   FALSE         100      100
#> # i 74 more rows
#> # i 1 more variable: verdict <chr>
```

### Focusing review on larger shortfalls

When review capacity is limited, lower `spi_cut` to reduce the number of
districts assessed with STEPS. Values are ratios, so use `0.80`, `0.60`,
or `0.40`, rather than 80, 60, or 40.

| Chosen SPI cutoff | Districts included | Relative shortfall after national centring |
|----|----|----|
| `1.00` (package default) | SPI below 1 | Any relative shortfall |
| `0.80` | SPI below 0.80 | More than 20% below expectation |
| `0.60` | SPI below 0.60 | More than 40% below expectation |
| `0.40` | SPI below 0.40 | More than 60% below expectation |

This table assumes `centre = "national"`, the default. With
`centre = "none"`, the same cutoffs refer to the district's own
uncentred expectation. Lower cutoffs select more extreme volume
shortfalls. They are practical choices for prioritising review, not
validated boundaries between adequate and inadequate surveillance.
Continue to review concerns from timeliness, stool quality, or other
indicators even when a district is above the chosen SPI cutoff.

``` r
fg_focused <- spi_field_guide(
  concordance = conc,
  process = synth$afp_process,
  spi_cut = 0.60
)
```

`spi_cut` also sets the threshold used for extent and persistence, so
both are reassessed at the chosen cutoff. The strength check still
compares the 90% credible interval with 1. To use the same cutoff in the
concordance table, also set `spi_threshold = 0.60` in
`spi_concordance()`. Record the cutoff with the results.

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

Use `spi_field_guide_pager()` to create an A4 report for one district.
It includes the review label, an SPI chart with a 90% credible interval,
AFP and ES detection markers, a location map, the neighbour median, and
the five STEPS components. The report explains which findings determine
the label and notes when counts are small. The interval reflects
uncertainty in expected counts; it does not include sampling variation
in observed counts.

Pass the field guide and boundaries to build the report. `path` saves
files as `spi_<adm0>_<adm1>_<adm2>_field_pager.{html,png}`. Optional
arguments add regional context (`region`), an assessment-period label
(`year_label`), or the posterior probability `P(SPI < 1)`
(`prob_under`). Regional rank does not change the label.

``` r
spi_field_guide_pager(
  fg, district = "Tirwen", boundaries = synth$boundaries,
  id_col = "adm2_guid", path = "reports/"
)
```

<details>

<summary>

One-page field pager for a review priority district
</summary>

<img src="man/figures/README-pager.png" alt="One-page SPI field pager: review label, an SPI chart with a credible interval and a location map, the five STEPS components, and a judgement banner." width="100%" />

</details>

## Exported functions

``` r
spi_check_inputs()           # check cases, population, and boundaries
spi_adjacency()              # spatial neighbour graph from sf boundaries
spi_expected()               # fit BYM2 expected-count model (INLA)
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
