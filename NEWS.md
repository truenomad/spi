# spi 0.3.0

### Bug fixes

* `spi_index()` centres each posterior draw on the national ratio from the
  same draw, then summarises. Uncertainty shared by all districts in a period
  no longer widens district intervals. Recalculate centred results from 0.2.0.

* Simple SPI reports explain missing SPI values when the national ratio is
  zero.

### New features

* `spi_simple()` calculates SPI from each district's earlier NPAFP rate,
  without INLA or boundaries.

* `spi_simple_explain()` shows one district's simple calculation, in English
  or another language.

* `spi_compare_npafp()` and `spi_field_guide()` accept simple SPI results.

* `spi_check_inputs(method = "simple")` checks simple SPI inputs.

### Documentation

* New articles on the simple SPI, the model-based SPI, data preparation and
  POLIS data. Simplified language throughout.

# spi 0.2.0

### Breaking changes

* Renamed the package from `blindspot` to `spi` and the `bs_` prefix to `spi_`.
  Recreate saved results.

* `spi_concordance()` is now `spi_compare_npafp()`, with renamed categories
  and columns.

* `bs_spi_prospective()` is now `spi_index()`. `bs_spi()` and the
  `spi_triangulate*()` functions are removed.

### New features

* National centring is the default, with ratios in `$national`.

* `spi_check_inputs()` checks inputs before fitting.

* `spi_field_guide_pager()` creates printable district reports.

* STEPS covers strength, timeliness, extent, persistence and stool adequacy.

* `year_end_month` supports reporting years ending in any month.

* Model defaults are now `overdispersion = "nb"` and `year_effect = "iid"`.
  `seed` controls both R and INLA sampling.

### Bug fixes

* Fixed NPAFP populations for custom reporting years, input validation and
  missing-name handling in reports.

# blindspot 0.1.0

* Added the field guide, triangulation with AFP/ES detections, and automatic
  choice of observation model.

# blindspot 0.0.0.9000

* Initial development version.
