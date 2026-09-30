# spi (development version)

### Documentation

* Simplified language throughout the articles, function help, console output,
  and district reports. Clarified national centring, uncertainty, and review
  labels. Calculations and function arguments are unchanged.

### New features

* `spi_direct()` calculates SPI directly from preceding reporting, without
  fitting the INLA spatial and temporal model. It needs only NPAFP cases and
  the population under 15 by district and year. A district's expected count
  is its current population times its NPAFP rate over all earlier years.
  `history_check` records when the SPI cannot be calculated: a district with
  no earlier case, or no earlier year, has no SPI and does not contribute to
  the national ratio. Each district's population series is checked for
  unusual changes before calculation.

* `spi_direct()` needs no other argument when the table has the columns
  `district`, `year`, `npafp_cases` and `population_u15`.

* `spi_direct_explain()` shows how one district's direct SPI was calculated,
  from current and previous reporting to the expected count and the national
  comparison.

* `spi_direct_explain()` can show its labels in another language, for example
  `language = "fr"` for French. English is the default. Translation uses the
  sntutils and gtranslate packages and needs an internet connection.

* `spi_check_inputs()` also checks the inputs of the direct SPI with
  `method = "direct"`. Boundaries are not needed for that check.

* New article, "The direct SPI", explains the calculation step by step.

* New article, "Preparing data for SPI", shows what data each calculation
  needs and how to prepare them.

* New article, "Using SPI with POLIS data", shows the workflow from the POLIS
  AFP line list to the direct and model-based SPI.

# spi 0.2.0

### Bug fixes

* NPAFP comparisons now use the index's observed counts when `cases` is omitted,
  and reject missing or duplicate population records.

* SPI case overrides reject duplicate rows and invalid counts. Input checks now
  catch fractional and infinite counts before fitting.

* District reports and tables use IDs when names are absent. Missing SPI values
  no longer receive a `No SPI indication` review label.

* SPI summaries handle constant or missing values and describe reporting patterns
  without implying surveillance adequacy, missed cases, or outbreaks.

### Breaking changes

* `spi_concordance()` and `spi_concordance_maps()` are now `spi_compare_npafp()` and
  `spi_compare_npafp_maps()`, and the result has class `spi_compare_npafp`. In its
  `district_year` table, `concordance` is now `category` and `spi_flagged` is
  `spi_below_threshold`; the metric counts are `n_neither`, `n_spi_only`, `n_npafp_only`,
  and `n_both`. `spi_field_guide()` takes `comparison` in place of `concordance`.

* The four categories are now `Neither below`, `SPI below threshold only`,
  `NPAFP below target only`, and `Both below`.

* The review label `Review priority` is now `Priority for review`.

* `synth_surveillance$truth` is now `synth_surveillance$simulation_truth`.

* Renamed `blindspot` to `spi`; functions now use the `spi_` prefix instead of `bs_`.

* `bs_spi_prospective()` is now `spi_index()`, which fits each assessment year on the years
  before it and keeps every level and `year_end_month`. `bs_spi()`, which took a single fit
  of all years, is removed.

* Object classes now use `spi_` names. Recreate results saved with the old package.

* Renamed `genomic` and `genomic_col` to `detections` and `detection_col` in `spi_field_guide()`.

* Removed `spi_triangulate()`, `spi_triangulate_table()`, `spi_triangulate_map()`, and
  `synth_surveillance$detections`.

* Removed field-guide arguments `persistence`, `persistence_basis`, `min_corroborators`,
  `dedupe_temporal`, and `detection_corroborates`.

### Field guide

* STEPS now covers strength, timeliness, extent, persistence, and stool adequacy. The default
  `spi_cut` is 1.

* Replaced the old labels and count of supporting findings with `Priority for review`, `Monitor`,
  and `No SPI indication`.

* Added `process`, `extent_col`, `process_target`, and `process_min_cases` to check specimen
  handling and nearby districts.

* Added `spi_rule`, taken from the comparison settings. The interval rule uses `No SPI indication`
  when the interval includes 1.

* Trend, neighbours, seasonal patterns, and AFP/ES detections remain supporting context; they do not
  change review labels.

* `cri_excludes_1` now always means `spi_q95 < 1`; `gate_pass` also applies the optional
  `noise_alpha` check.

* Added `traj_alpha`, environmental surveillance data, and filters for virus types. Updated help,
  tables, examples, and the infographic.

### District reports

* Added `spi_field_guide_pager()` to create printable district reports with SPI, STEPS, detections,
  and a map.

* Choose a district by ID or name and set `path` to save HTML or PNG files. Reports no longer use
  `adjacency`.

* Added `indicators_df` for standard surveillance indicators. Results based on fewer than five cases
  show fractions instead of percentages.

* Added `region`, `year_label`, and `prob_under` for regional context, reporting-period labels, and
  `P(SPI < 1)`.

* Detection summaries show recorded virus types and count years with detections. `detection_label`
  defaults to NULL.

* Clarified what intervals mean and fixed colours for SPI below 1 but above the cutoff. Fixed long
  names and text running off the page.

* Footnotes are optional, small expected counts keep decimals, and reports say when detection data
  were not supplied.

* Reworded the district report in plain language. Numbers use a decimal point instead of a middle
  dot. The line beside the context heading describes the review label rather than the seasonal
  pattern and trend. At or above the cutoff, the report says SPI indicates no relative reporting
  shortfall and points to the other surveillance indicators.

### SPI, comparison with the NPAFP target, and inputs

* Added `spi_check_inputs()` and `check` to find problems with counts, population, IDs, missing
  records, and boundaries before fitting.

* Added `year_end_month` for reporting years that end in any month. Use `n_months` to find
  incomplete periods.

* Fixed the NPAFP population calculation for custom reporting years. Use the same `year_end_month`
  in SPI and the NPAFP comparison.

* Added `boundaries` to include district and area names in SPI results. The NPAFP comparison uses the same
  column order.

* Added national centring as the default and `$national` ratios. Use `centre = "none"` for uncentred
  observed-to-expected ratios.

* Periods with no reported cases nationally now return NA with a warning. Fixed map legend keys for
  empty categories.

### Model fitting

* Changed defaults to `overdispersion = "nb"` and `year_effect = "iid"`. Use `overdispersion =
  "iid"` and `year_effect = "none"` for the previous model.

* `seed` now controls both R and INLA sampling. Fitting checks the seed first and restores R's
  random-number state afterwards.

* Added `num_threads`, defaulting to `"1:1"` with a seed. Fixed seeds and one thread still allow
  small numerical differences.

### Data and documentation

* Added `afp_process` and `afp_timeliness` to the example data. Rebuilt `synth_field_guide` after
  the seed fix.

* Added five pkgdown articles and shortened the README. The reference index groups functions by
  task.

* Added `llms.txt` and Markdown pages for AI tools. Articles are built locally so the website can
  build without INLA.

# blindspot 0.1.0

* Added `bs_field_guide()` with seven review signals and FLAG, WATCH, and No-action labels.

* Added field-guide tables, console help, worked examples, and ready-to-use `synth_field_guide`
  data.

* Added `bs_triangulate()` to compare field-guide findings with AFP/ES detections, with a choice of
  time lag.

* Added `bs_triangulate_table()` and `bs_triangulate_map()` for tables and district maps.

* `bs_expected(overdispersion = "auto")` now compares observation models and fits the selected
  model.

* Expanded test coverage to about 99%, including model-fitting tests with INLA.

# blindspot 0.0.0.9000

* Initial development version.
