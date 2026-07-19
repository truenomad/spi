# blindspot 0.1.0.9000

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