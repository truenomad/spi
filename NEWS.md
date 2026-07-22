# blindspot 0.1.0.9000

* `bs_spi()` gained a `boundaries` argument. When supplied (an `sf` layer or a
  plain data frame keyed by the id column), the admin name columns
  (`adm1_name`, `adm2_name`, ...) are joined onto the `summary` output and
  placed immediately before the district id, so saved SPI tables carry
  human-readable labels next to `adm2_guid`. `bs_concordance()` already joined
  boundary names; those are now likewise moved to just before the id column,
  and `bs_field_guide()` inherits the ordering from the concordance table.
* `bs_field_guide()` gained three opt-in settings, all off by default so the
  published flag counts reproduce out of the box. `traj_alpha` turns S4
  (trajectory) into a significance-gated trend test: a trajectory reads
  "falling"/"rising" only if its OLS slope differs from zero at that two-sided
  level (needing three or more points), else "flat". This stops an endpoint
  drop or a single volatile year from reading as a sustained decline, matching
  the paper's S4 intent ("a sustained downward slope is a trend, not a single
  anomalous year"); `traj_alpha = 0.1` is the recommended setting and improves
  specificity on volatile series. `dedupe_temporal` counts a falling trajectory
  (S4) and a persistent sub-cut run (S5) as a single "temporal" corroborator
  rather than two, so a flag cannot rest on two readings of the same decline --
  the case that bites where neighbours drop out and the 2-of-4 rule reduces to
  one temporal fact counted twice. `detection_corroborates` promotes an AFP or
  ES detection to a counted, independent signal -- the recommended way to give
  a persistently-low district in a degraded neighbourhood a non-temporal leg
  (the KITI/KHULM absorption case), since it is the one corroborator not
  downstream of the AFP reporting being absorbed.
* `bs_field_guide()` also adds a `neighbourhood_shortfall` column on
  `district_year`, naming the absorption / self-benchmarking case (district and
  its neighbourhood both below the cut) that the boolean `neighbour_discordant`
  could not distinguish.
* `bs_field_guide_pager()`: the S6 reading now names a region-wide shortfall
  explicitly when a district and its neighbours are both below the cut; the
  chart's endpoint label peels a trailing "(qualifier)" onto its own line and
  shrinks the name to fit the right margin (no more collision with the SPI
  value); and the flag-rule sentence enumerates the actual corroborator axes,
  so it stays honest under `dedupe_temporal` / `detection_corroborates`.
* `bs_field_guide()` gained an `es` / `es_col` argument: a second, independent
  detection channel for S7 alongside `genomic` (AFP). It adds `es_years` /
  `es_detected` to `district_year`, mirroring the AFP `orphan_years` /
  `genomic_orphan`. Both channels are narrative corroboration and never change
  the verdict, matching the paper. The shipped `synth_field_guide` now carries
  the ES channel.
* Added `bs_field_guide_pager()`: renders one district's field-guide reading
  as a self-contained, print-ready HTML tear-sheet -- masthead verdict, an
  SPI-over-time chart of the district against its touching neighbours (real
  90% credible-interval ribbon), the seven signals laid out as gate /
  magnitude / corroboration, and a verdict banner. The accent colour tracks
  the verdict. It is self-contained: pass `boundaries` (the shapefile) and it
  builds the neighbour graph itself with `bs_adjacency()` and draws a locator
  inset from the real geometry -- the focal district ringed by its neighbours.
  S7 reports poliovirus found there through both channels, case-based (AFP)
  and environmental surveillance (`es`), each marked distinctly on the chart
  (AFP diamond, ES ring); detections are narrative corroboration only and
  never change the verdict. `district` resolves by id or name; `path` writes an
  auto-named `spi_<adm0>_<adm1>_<adm2>_field_pager.{html,png}` (PNG via
  \pkg{webshot2}). See `inst/examples/pager_demo.R`.
* Added `bs_check_inputs()`: a graded pre-flight that reconciles the three
  tables a fit consumes -- case counts, population denominators, and the
  district shapefile -- before any model runs. It reports id mismatches, panel
  gaps (returned as a `district_id` x `month` tibble), negative populations
  (error) and zero populations (warning, excluded rather than floored),
  partial coverage, shapefile districts with no case rows, and invalid geometry
  (reported, not repaired) all at once, graded error / warning / note.
* `bs_expected()` and `bs_compare_overdispersion()` gained a `check` argument
  (default `TRUE`) and now route through `bs_check_inputs()`, aborting on
  error-level issues before fitting. The check runs once per fit: the
  `overdispersion = "auto"` and comparison paths set `check = FALSE` on their
  inner fits.

# blindspot 0.1.0

* Added `bs_field_guide()`: reads every district-year through the paper's
  seven-signal interpretation protocol (S1-S7) and assigns a FLAG / WATCH /
  No-action verdict, degrading gracefully when the optional adjacency,
  monthly-SPI, or genomic inputs are absent.
* Added `bs_field_guide_table()` to render the field guide as a `gt` or
  `flextable` table (scan or worked-example layout, concern shading), with
  file export inferred from the extension (html / docx / pdf / rtf / png /
  pptx).
* Added `bs_field_guide_help()`, a console interpretation aid: the seven
  signals, the flag rule, common misreadings, and a live worked example
  narrated from real signal values.
* Added `bs_triangulate()`: crosses the field-guide verdict against
  independent virus detection (AFP and environmental surveillance) to
  separate a trustworthy silence from a probable blind spot, with a
  three-level triage priority and a `detection_lag` to read the verdict
  before any response amplified it.
* Added `bs_triangulate_table()` and `bs_triangulate_map()` to render the
  triangulation as a `gt` / `flextable` table or a district choropleth
  (ten-class or coarse-priority fill).
* Added the `synth_field_guide` dataset -- a precomputed field guide on
  `synth_surveillance` so the help, examples, and tests run without INLA.
* Added `bs_expected(overdispersion = "auto")`, which runs the
  `bs_compare_overdispersion()` comparison internally and refits with the
  best-calibrated specification (now noted in the README).
* Expanded the test suite to ~99% coverage: the downstream functions and
  their S3 methods now run off constructed fixtures with no INLA fit, and
  the model-fit path is exercised by small INLA-gated integration tests.

## blindspot 0.0.0.9000

* Initial development version
* Package framework established with core metadata and documentation structure
* Design principles documented
* S3 class specifications defined (blindspot_expected, blindspot_spi, blindspot_result)
* Function specifications documented for all 13 exported functions
* Package architecture and file structure planned