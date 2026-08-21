# blindspot 0.1.0.9000

### Field guide

* The interpretation protocol is now five signals, STEPS (significance, trend,
  extent, persistence, surroundings), matching the paper. The flag rule is two
  of three corroborators (trend, persistence, surroundings), down from two of
  four. Seasonal blindness and AFP / ES detections became out-of-grid
  corroboration, reported but never counted. Default flag counts move against
  the seven-signal spec, and `synth_field_guide` is regenerated to match.
* New `REVIEW` verdict between `FLAG` and `WATCH`, for a credible shortfall
  with too few corroborators. `bs_triangulate()` grows from ten classes to
  thirteen, and `No action` now means only "at or above the cut".
* `cri_excludes_1` means `spi_q95 < 1`, always. With `noise_alpha` set it had
  silently meant the interval and the noise tail together while every string
  built from it named only the interval. The composite the verdict turns on is
  the new `gate_pass`. Default-path counts are unchanged, pinned by a test.
* Surroundings fires on a shortfall shared across the neighbourhood as well as
  on a contrast against healthy neighbours, capped at one corroborator. The old
  rule went quiet where no healthy neighbour was left to contrast against.
* New arguments, each defaulting to the published behaviour: `noise_alpha` (a
  Poisson tail on the shortfall), `persistence_basis` (`"trailing"` reads the
  run ending at the read year), `traj_alpha` (significance-gated trend),
  `dedupe_temporal` (counts trend and persistence once),
  `detection_corroborates` (promotes a detection to a counted signal),
  `es` / `es_col` (an ES detection channel), `serotype_col` and
  `detection_serotypes`.
* New `district_year` columns: `gate_pass`, `neighbourhood_shortfall`,
  `noise_tail`, `noise_plausible`, `trailing_run_below`, `es_years`,
  `es_detected`, `orphan_serotypes`, `es_serotypes`.

### Field pager

* Added `bs_field_guide_pager()`, which renders one district's reading as a
  self-contained, print-ready tear-sheet with a locator inset drawn from the
  real geometry. `district` resolves by id or name, and `path` writes an
  auto-named `spi_<adm0>_<adm1>_<adm2>_field_pager.{html,png}`. See
  `inst/examples/pager_demo.R`.
* It charts the focal district alone and places it in its country, so
  `adjacency` is ignored and neither `bs_adjacency()` nor spdep is called.
* It prescribes no follow-up. The banner's action column is gone, and the
  masthead states where the reading sits against the rule.
* `indicators_df` fills a context row with the non-polio AFP rate against its
  target, plus stool adequacy, the two timeliness percentages and the EV rate.
  Below five assessable cases a tile names its denominator and stays ungraded.
* `region`, `year_label` and `prob_under` add regional context, a label for a
  window that is not a calendar year, and the posterior `P(SPI < 1)` on the
  significance line.
* `detection_label` defaults to NULL and the page reads "poliovirus", since the
  guide records when a detection happened but not what was found.
* Honesty fixes: the interval claim now covers uncertainty in the expected
  count alone; a district above the cut whose interval lies wholly below one
  reads "Below expectation"; the banner names serotypes per channel and dates
  the detection it rests on; detection tiles count years rather than detections.
* Layout fixes: a long unit name is sized to the row and breaks over two lines;
  a unit sharing its parent's name keeps the parent; the page grows past one
  sheet when the masthead fills.
* Smaller fixes: `note` prints in the footer alone and is opt-in; persistence
  names the run ending at the read year; an expected count below ten keeps a
  decimal; an unsupplied channel reads as unsupplied.

### SPI, concordance and inputs

* Added `bs_check_inputs()`, a graded pre-flight that reconciles case counts,
  population denominators and the shapefile before any model runs, reporting id
  mismatches, panel gaps, bad populations, partial coverage and invalid geometry
  at once. `bs_expected()` and `bs_compare_overdispersion()` gained `check`
  (default `TRUE`) and route through it.
* `bs_spi()` gained `year_end_month`, so a reading year can close on the month a
  review closes. `year_end_month = 4` groups May through April, labelled by the
  year it closes in; `n_months` marks the partial window at each end.
* `bs_concordance()` gained `year_end_month` too. Pass the same value the SPI
  was computed with and the conventional NPAFP rate is grouped on the same
  rolling year; left at the calendar default against a rolling SPI, it counted
  part of the window against a whole-year denominator and understated the rate.
* `bs_spi()` gained `boundaries`, joining admin names immediately before the
  district id. `bs_concordance()` places its own name columns the same way.
* Concordance maps draw a legend key for a fill level no district fell into,
  which `geom_sf` had rendered as a blank swatch.

### Model fitting

* `bs_expected(seed = )` now reaches INLA's own RNG rather than R's alone.
  Seeding with `set.seed()` left `inla.posterior.sample()` unseeded, so two runs
  of identical seeded code disagreed by enough to move SPI and flip verdicts.
* `bs_expected()` gained `num_threads`, which defaults to `"1:1"` alongside a
  seed, since a multithreaded fit drifts. Pass `NULL` to inherit INLA's global
  setting and trade determinism for speed. `seed` is validated ahead of the INLA
  availability check, and the caller's RNG state is restored on exit.
* The guarantee is agreement to numerical tolerance, not bit-identity. INLA's
  mode-finding is not bit-stable even on one thread, so two seeded fits differ
  in the sixth significant figure, up to 1.6e-6 relative. That is four orders
  below the drift an unseeded fit produced, and well inside anything that could
  move a classification.

### Data

* `synth_surveillance` gained `afp_timeliness`, a district-year table of
  assessable and within-window AFP counts. Purely additive.
* `synth_field_guide` regenerated. It predated the seed fix, so it never matched
  its own build script; SPI moves by up to 0.12. Guides saved by an earlier
  version still render.

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
* Added the `synth_field_guide` dataset, a precomputed field guide on
  `synth_surveillance` so the help, examples, and tests run without INLA.
* Added `bs_expected(overdispersion = "auto")`, which runs the
  `bs_compare_overdispersion()` comparison internally and refits with the
  best-calibrated specification (now noted in the README).
* Expanded the test suite to ~99% coverage: the downstream functions and
  their S3 methods now run off constructed fixtures with no INLA fit, and
  the model-fit path is exercised by small INLA-gated integration tests.

# blindspot 0.0.0.9000

* Initial development version
* Package framework established with core metadata and documentation structure
* Design principles documented
* S3 class specifications defined (blindspot_expected, blindspot_spi,
  blindspot_result)
* Function specifications documented for all 13 exported functions
* Package architecture and file structure planned
