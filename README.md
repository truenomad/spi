
<!-- README.md is generated from README.Rmd. Please edit that file, then run
     `devtools::build_readme()` (or knit) to regenerate README.md. -->

# blindspot <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/truenomad/blindspot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/truenomad/blindspot/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/truenomad/blindspot/graph/badge.svg?token=vBneu9acox)](https://codecov.io/gh/truenomad/blindspot)
[![pkgdown](https://github.com/truenomad/blindspot/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/truenomad/blindspot/actions/workflows/pkgdown.yaml)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R \>=
4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://cran.r-project.org/)

<!-- badges: end -->

> **Find the districts where the surveillance system can't see**

*In the districts that report no cases, is the silence real?*

blindspot estimates, for each district in each month, how many cases a
surveillance system should be detecting given its health facilities,
population, care-seeking patterns, conflict exposure, and the detection
history of its neighbours. The ratio of what is detected to what is
expected defines a surveillance performance index (SPI): a continuous,
uncertainty-quantified measure of detection capacity that refines and
corroborates the conventional NPAFP-rate threshold (one example of the
flat indicators surveillance leans on) rather than replacing it. It is
diagnostic, not predictive: it grades current detection performance, and
does not forecast where poliovirus is circulating.

**Designed for polio AFP surveillance. Applicable to any case-based VPD
surveillance system.**

The expected count comes from a Bayesian spatiotemporal count model of
each district-month, and the SPI is the observed count over that
expectation:

$$
Y_{it} \sim \text{NegBin}(\mu_{it}, \kappa), \qquad \text{SPI}_{it} = \frac{Y_{it}}{\mu_{it}}
$$

$$
\log \mu_{it} = \log(P_{it}/12) + \beta_0 + f(\text{month}_t) + \gamma_{y(t)} + u_i + v_i + x_{it}^{\top}\beta
$$

where

- $\log(P_{it}/12)$ is the log under-15 person-time offset
- $\beta_0$ is the background non-polio AFP detection rate, estimated
  from the data
- $f(\text{month}_t)$ is harmonic seasonality (12- and 6-month
  periodicity)
- $\gamma_{y(t)}$ is an exchangeable year effect
- $u_i + v_i$ is a BYM2 spatial random effect (Riebler et al. 2016)
- $\kappa$ is the negative binomial dispersion, the default
  `overdispersion = "nb"`
- $x_{it}^{\top}\beta$ is the optional district-covariate term (DTP3
  coverage, urbanicity, travel time to care)

`overdispersion = "iid"` swaps the negative binomial likelihood for
Poisson plus an observation-level term $\epsilon_{it}$ added to the
linear predictor, and `"none"` drops overdispersion and fits a plain
Poisson; `bs_compare_overdispersion()` scores the three so the choice is
justified rather than assumed.

<details>

<summary>

<b>Method notes — estimand, model, and what the SPI is for</b> (click to
expand)
</summary>

<br>

<img src="man/figures/spi-dag.png" width="100%" alt="Directed acyclic graph of the SPI estimand" />

The observed count is the product of two unobserved processes (true
non-polio AFP incidence and detection probability), modelled as an
overdispersed count. Fitted to the data, the model estimates an
**expected count** (the level prevailing across comparable districts)
from a population offset and spatial, seasonal, and annual terms, with
context covariates as an optional refinement; the **SPI is observed ÷
expected**. Poliovirus circulation has no path into the model, so the
SPI is diagnostic of *surveillance performance* and does not estimate
transmission; it enters only as an independent reference for external
validation.

**Why a Bayesian spatial model.** District-month AFP counts are small
and noisy, so a raw rate is unstable and a single zero-count month can
look like a collapse. The BYM2 spatial effect borrows strength across
space, pulling each district toward its neighbours in proportion to how
little its own data can say, and returns a full posterior, so the SPI
carries a credible interval rather than a bare point estimate. Fitting
is by INLA rather than MCMC: for latent Gaussian spatial models of this
size, approximate-Bayesian inference is accurate and finishes in seconds
to minutes.

**What the SPI is for.** It refines the conventional NPAFP rate rather
than replacing it: instead of asking whether a district clears a fixed
target, it asks whether the district detects as much as a model of its
own context expects, with uncertainty attached. SPI = 1 means detection
matches expectation; below 1 flags under-detection (a blindspot). It is
diagnostic, not predictive: it grades whether the system *could* see,
not whether virus was there.

The core engine is disease-agnostic: it operates on case counts,
population denominators, and spatial boundaries. But polio AFP/NPAFP is
the key example throughout this package and its paper: the method was
built around the polio endgame's surveillance-quality problem, and the
NPAFP rate is the flat threshold every worked example refines. Other
case-based VPD indicators (measles discard surveillance, DHIS2 reporting
completeness, and the like) are supported by analogy: changing the
application changes the data and covariates, not the methodology.

A blindspot is a district where the surveillance system cannot see. The
package finds them, quantifies the uncertainty, and tells you what to do
about them.

</details>

## Installation

``` r
# from r-universe (recommended)
install.packages(
  "blindspot",
  repos = c(
    "https://truenomad.r-universe.dev",
    "https://cloud.r-project.org"
  )
)

# or from github
pak::pak("truenomad/blindspot")
```

## Walkthrough

This runs the whole chain on the bundled synthetic data, so you can
reproduce every step without POLIS access. It is the short version of
`inst/examples/paper_analysis.R`, which you can open with
`file.edit(system.file("examples/paper_analysis.R", package = "blindspot"))`.

``` r
library(blindspot)
```

### The data

`synth_surveillance` is a made-up country (Harad): 236 districts across
36 provinces, monthly counts from 2015 to 2024, an under-15 population
denominator, district polygons, a genomic outcome, and a ground-truth
sheet recording where the blindspots were planted.

``` r
synth <- synth_surveillance
names(synth)
#>  [1] "cases"            "population"       "covariates"       "boundaries"      
#>  [5] "ward_boundaries"  "virus_outcome"    "es_sites"         "es_data"         
#>  [9] "es_district_year" "detections"       "truth"

head(synth$cases)
#> # A tibble: 6 x 3
#>   adm2_guid                              month      count
#>   <chr>                                  <date>     <int>
#> 1 {54CD979C-CF9D-6A65-567F-8976C9546137} 2015-01-01     0
#> 2 {54CD979C-CF9D-6A65-567F-8976C9546137} 2015-02-01     0
#> 3 {54CD979C-CF9D-6A65-567F-8976C9546137} 2015-03-01     0
#> 4 {54CD979C-CF9D-6A65-567F-8976C9546137} 2015-04-01     0
#> 5 {54CD979C-CF9D-6A65-567F-8976C9546137} 2015-05-01     0
#> 6 {54CD979C-CF9D-6A65-567F-8976C9546137} 2015-06-01     1
```

### Check the inputs first

Before fitting anything, reconcile the three tables the model consumes:
the case counts, the population denominators, and the district
shapefile. `bs_check_inputs()` grades every mismatch at once (error,
warning, note), so a silent id misalignment or a hole in the monthly
panel surfaces here rather than as wrong numbers later. `bs_expected()`
runs it for you and stops on any error; run it yourself first to see the
warnings too.

``` r
bs_check_inputs(
  cases = synth$cases,
  population = synth$population,
  shapefile = synth$boundaries,
  id_col = "adm2_guid"
)
#> 
#> -- blindspot input check -------------------------------------------------------
#> v All input checks passed -- 236 districts x 120 months (2015-01 to 2024-12).
```

On a broken copy, one negative count and three dropped months, it
returns the error that blocks the fit alongside the warnings worth a
look:

``` r
bad <- synth$cases
bad$count[1] <- -1                   # a data-entry slip
bad <- bad[-(2:4), ]                 # three missing district-months

bs_check_inputs(bad, synth$population, synth$boundaries, id_col = "adm2_guid")
#> 
#> -- blindspot input check -------------------------------------------------------
#> i 236 districts x 120 months (2015-01 to 2024-12)
#> x 1 case row has negative counts
#> ! 3 district-months missing from the panel (gaps)
#> --------------------------------------------------------------------------------
#> x 1 error -- resolve before fitting.
```

### 1. Spatial neighbours

The model shares information between neighbouring districts, so the
first step turns the polygons into a neighbour graph.

``` r
adj <- bs_adjacency(synth$boundaries, id_col = "adm2_guid")
adj
#> Neighbour list object:
#> Number of regions: 236 
#> Number of nonzero links: 1,320 
#> Percentage nonzero weights: 2.3700 
#> Average number of links: 5.5932
```

### 2. Expected counts

`bs_expected()` is the core model. It estimates how many cases each
district should report each month, given its population, its neighbours,
and the season. The bare spec, and the paper's own, is a BYM2 spatial
term, an IID year effect (which soaks up system-wide shifts such as the
COVID drop), a harmonic season, and negative binomial overdispersion,
all on a log person-time offset. The `overdispersion` argument takes
`"nb"` (default), `"iid"`, or `"none"`; pass `"auto"` to have it run the
comparison in step 3 for you and refit with the best-calibrated spec.

``` r
fit_bare <- bs_expected(
  cases = synth$cases,
  population = synth$population,
  adjacency = adj,
  id_col = "adm2_guid",
  n_draws = 500,
  seed = 42,
  verbose = FALSE
)
```

The model output is the expected case count for every district-month,
with a full posterior. Displayed next to the observed count:

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

For those who want to use their own covariates, such as health-system
reach, urbanicity, or access to care, `bs_expected()` takes them as a
district-year tibble through the `covariates` argument. The toy ships
three you can use straight away:

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
fit_adj <- bs_expected(
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

Each effect comes back as a rate ratio per standard deviation (the
covariates are standardised inside the model). Here are the three
covariates, dropping the seasonal harmonic terms:

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

One caveat, and the table shows it: most of these effects are muted,
because the spatial, seasonal, and year terms already absorb the
structure the covariates would otherwise carry. These covariates also
track surveillance access, which is itself partly a function of
detection, so leaning on them can mask the shortfall the SPI is built to
catch. Treat the adjusted fit as a sensitivity check and keep the bare
model as the main one. The rest of this walk uses `fit_bare`.

### 3. Does the overdispersion term pay for itself?

A quick likelihood check across none, IID, and negative-binomial, so the
choice is justified rather than assumed.

``` r
od <- bs_compare_overdispersion(
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
#> 1 none  28320 83377. 83463.  232.      0.865       0.928 FALSE        NA    
#> 2 iid   28320 80358. 80238. 6373.      0.861       0.923 FALSE         0.457
#> 3 nb    28320 81148. 81144.  231.      0.867       0.936 FALSE        NA    
#> # i 2 more variables: cpo_valid <dbl>, pit_ks <dbl>
```

To skip the manual step, `bs_expected(overdispersion = "auto")` runs
this same comparison internally and refits with the recommended spec.

### 4. Surveillance Performance Index

The SPI is observed over expected, computed for every posterior draw so
the uncertainty in the denominator carries through. It can be summarised
at any grain:

``` r
spi_dy <- bs_spi(fit_bare, level = "district_year")  # annual per district
spi_dm <- bs_spi(fit_bare, level = "district_month")  # monthly per district
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

**Why the yearly SPI is the one we act on.** The index is defined at any
grain, but the verdict is annual. In most districts the monthly AFP
count is 0 or 1, so the monthly SPI is mostly structural zeros with
credible intervals too wide to act on, and reacting to it spends trust
on noise. The sample size only settles over a full year, so the year is
the unit we classify. We keep the monthly series to read the trend, and
to feed the seasonal signal in the field guide.

The reading year does not have to be a calendar year.
`bs_spi(level = "district_year", year_end_month = 4)` groups May through
April, so a review can close on the month the decision was actually
taken; each window is labelled by the calendar year it closes in, and
the returned `n_months` marks the partial windows at either end of the
series, which should normally be dropped.

The `plot()` method gives four diagnostic views (`distribution`,
`funnel`, `caterpillar`, `calibration`). The funnel plots each
district-year against its expected count (the paper's Figure 3):
detection scatters around 1, and the spread narrows as the expected
count grows, so a low SPI at a high expected count is a genuine
shortfall rather than small-number noise.

``` r
plot(spi_dy, type = "funnel")
```

<img src="man/figures/README-spi-funnel-1.png" alt="" width="100%" />

The default `distribution` view is the headline calibration check: a
well-fit SPI is roughly log-normal and centred near 1.

``` r
plot(spi_dy)
```

<img src="man/figures/README-spi-distribution-1.png" alt="" width="100%" />

### 5. Concordance against the conventional threshold

Instead of a hard pass/fail, each district-year's SPI is crossed with
the WHO NPAFP-rate target (at least 3 non-polio AFP per 100,000 under-15
person-years) into a 2x2. The cell that matters is false reassurance:
the NPAFP rate looks fine, but the SPI still flags under-detection. That
is the blindspot a plain threshold walks past.

``` r
conc <- bs_concordance(
  spi = spi_dy,
  cases = synth$cases,
  population = synth$population,
  boundaries = synth$boundaries,
  verbose = FALSE
)
```

Every district-year lands in one of the four cells:

``` r
table(conc$district_year$concordance)
#> 
#>     Both adequate    True shortfall False reassurance       False alarm 
#>              1348               233               684                95
```

`plot()` shows the four cells as a scatter of NPAFP rate against SPI,
split by the two thresholds (dashed). The false-reassurance points sit
bottom-right: adequate NPAFP rate, low SPI.

``` r
plot(conc)
```

<img src="man/figures/README-concordance-scatter-1.png" alt="" width="100%" />

### 6. The three-panel map

Three panels for one year: the conventional NPAFP rate, the posterior
median SPI, and where the two disagree.

``` r
bs_concordance_maps(conc, boundaries = synth$boundaries, year = 2023)
```

<img src="man/figures/README-maps-1.png" alt="" width="100%" />

### 7. The STEPS field guide

`bs_field_guide()` applies the SPI field guide's review framework to
each district-year. For districts with an SPI below 1, five components,
**STEPS**, help assess the wider surveillance picture:

| Letter | Component | What it asks |
|----|----|----|
| S | Strength | How large and how certain is the shortfall? Read from the SPI value and whether its 90% credible interval lies entirely below 1. |
| T | Timeliness | Are specimens reaching the laboratory within 3 days? |
| E | Extent | Are other districts in the same admin-1 area (other LGAs in the same state) also below expectation? |
| P | Persistence | Was the SPI also below 1 in the previous year? |
| S | Stool adequacy | Are stool specimens adequate? |

The components are interpreted together, not combined into a score, and
they do not carry the same weight. Extent is the strongest corroborator
of the SPI signal. Persistence also supports it, but is common.
Timeliness and stool adequacy are important surveillance dimensions, but
are not validated corroborators, so they are reported and never change
the judgement. Each district-year receives one of the field guide's
three review judgements: **Review priority** when the interval lies
entirely below 1 and extent or persistence corroborates the shortfall,
**Monitor** for any other SPI below 1, and **No SPI indication** at or
above 1.

Timeliness and stool adequacy come from `process`, a district-year table
of AFP case counts (`n_cases`, `n_adequate`, `n_transport`,
`n_transport_timely`). Extent uses the `adm1_name` column that
`bs_concordance()` carries when given `boundaries`. The trend, the
neighbour contrast, seasonal detection and any poliovirus found through
AFP (`genomic`) or environmental surveillance (`es`) are still computed
and reported as context outside STEPS.

The one-page infographic that summarises NPAFP, the SPI and STEPS ships
with the package at
`system.file("field-guide", "npafp_spi_steps_infographic.html", package = "blindspot")`.

<details>

<summary>

Using NPAFP, SPI and STEPS (infographic)
</summary>

<figure>
<img src="man/figures/README-steps-infographic.png"
alt="Infographic: using NPAFP, SPI and STEPS to assess AFP surveillance" />
<figcaption aria-hidden="true">Infographic: using NPAFP, SPI and STEPS
to assess AFP surveillance</figcaption>
</figure>

</details>

``` r
genomic <- dplyr::filter(synth$virus_outcome, any_cvdpv2 == 1)

fg <- bs_field_guide(
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
#>  9 Suzil        1   8.6  0.11 0.09-0.14   1.6 TRUE   TRUE           NA       NA
#> 10 Nentha       2  16.5  0.12 0.10-0.14   2   TRUE   FALSE          NA       NA
#> # i 74 more rows
#> # i 1 more variable: verdict <chr>
```

`summary(fg)` adds how often each STEPS component raises concern and the
STEPS reference table, and `bs_field_guide_help()` walks the field
guide's four worked examples in the console.

For a report, `bs_field_guide_table()` renders the worked example as a
publication-ready `gt` or `flextable`: four rule-picked districts (by
name) read down the five STEPS components, each cell shaded by concern.
(The image below is a snapshot; the live call returns a `gt` object
whose cell shading GitHub would otherwise strip.)

``` r
bs_field_guide_table(fg, engine = "gt", layout = "worked")
```

<img src="man/figures/README-fg-table.png" alt="Field guide table: four districts read down the five STEPS components, cells shaded green for reassuring, amber for intermediate, and rose where the finding adds to concern." width="100%" />

For the one district you are about to investigate,
`bs_field_guide_pager()` renders a single-district **field pager**: a
self-contained, print-ready A4 tear-sheet. Hand it the field guide and
the shapefile and it draws everything it needs from those two, with no
adjacency object required. The masthead carries the review judgement;
the chart plots the district's own SPI (with the 90% credible-interval
ribbon and AFP / ES detection markers) and a locator inset drawn from
the real geometry, with the neighbour median read out as a figure rather
than a line; the five STEPS components are read out one per row, with
the season, trend and detections shown as context; and a banner states
what the judgement rests on. The pager reports the reading and
recommends no follow-up, and it is explicit about the limits of the
credible interval: that interval carries uncertainty in the *expected*
count, not sampling variability in the observed one, so a shortfall
resting on a handful of cases says so on its face. `path` writes an
auto-named `spi_<adm0>_<adm1>_<adm2>_field_pager.{html,png}`. Three
optional arguments add context the field guide does not carry itself:
`region` places the district in its region with a rank that is triage
only, `year_label` names a reading window that is not a calendar year,
and `prob_under` prints the posterior `P(SPI < 1)` on the strength line,
which is otherwise only pass or fail.

``` r
bs_field_guide_pager(
  fg, district = "Tirwen", boundaries = synth$boundaries,
  id_col = "adm2_guid", path = "reports/"
)
```

<details>

<summary>

One-page field pager for a review priority district
</summary>

<img src="man/figures/README-pager.png" alt="One-page SPI field pager: masthead judgement, an SPI-over-time chart with a credible-interval ribbon and a locator inset, the five STEPS components, and a judgement banner." width="100%" />

</details>

<!-- Section 8 (Triangulation) is temporarily dropped from the rendered docs.
     To restore, delete this comment wrapper and drop the `eval = FALSE` chunk
     options below.
&#10;### 8. Triangulating the judgement against independent detection
&#10;The field guide judges the *net*, not the *fish*: a review priority says a silence may be untrustworthy, not that the silence hid virus. Every component it uses comes from the AFP stream itself, so it cannot corroborate its own judgement without arguing in a circle. `bs_triangulate()` crosses the judgement against the one largely-independent channel, environmental surveillance (ES), and against AFP detections, and sorts each district-year into a ten-class triage grid with a three-level priority.
&#10;
``` r
tri <- bs_triangulate(
  fg,
  detections = synth_surveillance$detections,
  detection_lag = 1L,
  verbose = FALSE
)
&#10;# the classes are a judgement x ES-status grid (AFP-detected is off-grid)
tri$district_year |>
  dplyr::filter(!afp_hit) |>
  dplyr::mutate(
    verdict = factor(verdict_chr, c("Review priority", "Monitor",
                                    "No SPI indication")),
    ES = factor(es_status, c("positive", "clear", "no site"))
  ) |>
  dplyr::count(verdict, ES, .drop = FALSE) |>
  tidyr::pivot_wider(names_from = ES, values_from = n, values_fill = 0)
```
&#10;Read as a grid, the field-guide judgement runs down the rows and the independent ES read across the columns, so every cell is one triage class. `detection_lag = 1L` tests the year-*t* judgement against year *t + 1* detections, so the reading is taken before the detection's own case-finding could inflate it. The cells that carry the weight sit off the reassuring bottom-right: **Review priority × positive** (confirmed blindspots, where ES caught what AFP missed), **No SPI indication × positive** (virus found where the guide saw no shortfall), **Review priority × no site** (a priority with no ES site to check, the highest-value place to deploy ES or an active search), and **Monitor × positive** (a monitored shortfall that the ES hit corroborates). The district-years where AFP itself already detected virus sit outside the grid.
&#10;
``` r
bs_triangulate_map(tri, synth_surveillance$boundaries, year = 2020)
```
&#10;The 2020 judgements (the COVID-crash trough, tested against 2021 detections) map the triage: reds and magenta are the districts to act on or instrument, greens the trustworthy silences. `bs_triangulate_table()` renders the same panel as a `gt` / `flextable` for a report.
&#10;
``` r
bs_triangulate_table(tri, engine = "gt", year = 2020) |>
  gt::as_raw_html()
```
&#10;-->

## Exported functions

``` r
bs_check_inputs()           # pre-flight: reconcile cases / population / shapefile
bs_adjacency()              # spatial neighbour graph from sf boundaries
bs_expected()               # fit BYM2 expected-count model (INLA)
bs_compare_overdispersion() # none vs IID vs negative-binomial diagnostic
bs_spi()                    # surveillance performance index + posterior draws
bs_concordance()            # cross-classify SPI vs the NPAFP-rate threshold
bs_concordance_maps()       # three-panel concordance map (ggplot2/patchwork)
bs_field_guide()            # STEPS review: priority / monitor / no indication
bs_field_guide_table()      # render the field guide (gt / flextable)
bs_field_guide_pager()      # one-district A4 field pager (html / png)
bs_field_guide_help()       # learn to read the field guide (worked example)
bs_triangulate()            # cross the judgement with ES / AFP detections
bs_triangulate_table()      # render the triangulation panel (gt / flextable)
bs_triangulate_map()        # map the triage classes over districts (ggplot2)
```

`bs_expected()`, `bs_spi()`, `bs_concordance()`, `bs_field_guide()`, and
`bs_triangulate()` each return a typed object with `print` (and, where
useful, `summary` / `as_tibble`) methods; the `bs_spi()` and
`bs_concordance()` objects also have a `plot()` method.

## Citation

``` r
Yusuf MA (2026). blindspot: Bayesian spatiotemporal
  surveillance quality monitoring. R package version 0.1.0.9000.
  https://github.com/truenomad/blindspot
```

## Related packages

**poliprep:** POLIS data cleaning and preparation. Upstream of
blindspot.

**AgePopDenom:** DHS-anchored age-structured population estimates.
Provides the denominator input.

## License

MIT (c) 2026 Mohamed A. Yusuf
