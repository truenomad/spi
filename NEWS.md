# spi 0.1.0.9000

### Breaking changes

* Renamed `blindspot` to `spi`; functions now use the `spi_` prefix instead of `bs_`.

* `bs_spi()` is now `spi_index()`; `bs_spi_prospective()` is now `spi_prospective()`.

* Object classes now use `spi_` names. Recreate results saved with the old package.

* Renamed `genomic` and `genomic_col` to `detections` and `detection_col` in `spi_field_guide()`.

* Removed `spi_triangulate()`, `spi_triangulate_table()`, `spi_triangulate_map()`, and
  `synth_surveillance$detections`.

* Removed field-guide arguments `persistence`, `persistence_basis`, `min_corroborators`,
  `dedupe_temporal`, and `detection_corroborates`.

### Field guide

* STEPS now covers strength, timeliness, extent, persistence, and stool adequacy. The default
  `spi_cut` is 1.

* Replaced the old labels and count of supporting findings with `Review priority`, `Monitor`, and
  `No SPI indication`.

* Added `process`, `extent_col`, `process_target`, and `process_min_cases` to check specimen
  handling and nearby districts.

* Added `spi_rule`, taken from the concordance settings. The interval rule uses `No SPI indication`
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

### SPI, concordance and inputs

* Added `spi_check_inputs()` and `check` to find problems with counts, population, IDs, missing
  records, and boundaries before fitting.

* Added `year_end_month` for reporting years that end in any month. Use `n_months` to find
  incomplete periods.

* Fixed the NPAFP population calculation for custom reporting years. Use the same `year_end_month`
  in SPI and concordance.

* Added `boundaries` to include district and area names in SPI results. Concordance uses the same
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
