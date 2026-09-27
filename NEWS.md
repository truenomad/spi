# spi 0.1.0.9000

### Package renamed to spi

* The package is now `spi` (previously `blindspot`), at
  https://github.com/truenomad/spi. Exported functions take the `spi_` prefix
  in place of `bs_`: `bs_spi()` is `spi_index()`, `bs_spi_prospective()` is
  `spi_prospective()`, and every other `bs_x()` is `spi_x()`. S3 classes take
  the same prefix: `blindspot_spi` is `spi_index`, and every other
  `blindspot_x` is `spi_x`. The old names are not kept, so objects saved under
  the old classes need regenerating.

### Field guide

* STEPS now follows the SPI field guide: strength, timeliness, extent,
  persistence and stool adequacy, applied to districts with an SPI below 1.
  `spi_cut` defaults to 1 rather than the concordance cut. Extent compares the
  share of other districts in the same admin-1 area below the cut with the
  national share (`extent_col`, default `adm1_name`). Persistence is the SPI in
  the previous year. Timeliness and stool adequacy come from the new `process`
  argument, district-year AFP counts assessed on at least `process_min_cases`
  cases against `process_target` percent.
* The verdict is one of the field guide's three review judgements:
  `Review priority` (interval entirely below 1, corroborated by extent or
  persistence), `Monitor` (any other SPI below the cut) and
  `No SPI indication`. Timeliness and stool adequacy are reported but never
  move the judgement. `FLAG`, `REVIEW`, `WATCH`, `No action` and the
  corroborator count are gone, as are `persistence`, `persistence_basis`,
  `min_corroborators`, `dedupe_temporal` and `detection_corroborates`.
* The trend, neighbour contrast, seasonal detection and AFP / ES detections
  are still computed and reported as context outside STEPS.
* `cri_excludes_1` means `spi_q95 < 1`, always; the strength test the
  judgement turns on is `gate_pass`, which adds the Poisson noise tail when
  `noise_alpha` is set.
* Other arguments: `traj_alpha` (significance-gated trend), `es` / `es_col`
  (an ES detection channel), `serotype_col` and `detection_serotypes`.
* `spi_field_guide_help()`, `spi_field_guide_table()` and the misreadings use the
  field guide's wording, and the worked layout picks the field guide's four
  teaching cases.
* `spi_triangulate()` crosses the three judgements with ES status: ten classes,
  with `priority, ES clear`, `monitor, *` and `no indication, ES positive`
  replacing the flag, review and watch classes.
* `synth_surveillance` gains `afp_process`, drawn after every other table so
  the existing elements are unchanged. `synth_field_guide` is regenerated with
  it: for 2024, 60 review priority, 71 monitor and 105 no SPI indication.
* The field guide infographic ships at
  `system.file("field-guide", "npafp_spi_steps_infographic.html", package =
  "blindspot")`.

### Field pager

* Added `spi_field_guide_pager()`, which renders one district's reading as a
  self-contained, print-ready tear-sheet with a locator inset drawn from the
  real geometry. `district` resolves by id or name, and `path` writes an
  auto-named `spi_<adm0>_<adm1>_<adm2>_field_pager.{html,png}`. See
  `inst/examples/pager_demo.R`.
* It charts the focal district alone and places it in its country, so
  `adjacency` is ignored and neither `spi_adjacency()` nor spdep is called.
* It reads the five STEPS components one per row and prescribes no follow-up.
  The masthead states where the reading sits, and the banner names what the
  judgement rests on. The season, trend and detections show as context.
* `indicators_df` fills a context row with the non-polio AFP rate against its
  target, plus stool adequacy, the two timeliness percentages and the EV rate.
  Below five assessable cases a tile names its denominator and stays ungraded.
* `region`, `year_label` and `prob_under` add regional context, a label for a
  window that is not a calendar year, and the posterior `P(SPI < 1)` on the
  strength line.
* `detection_label` defaults to NULL and the page reads "poliovirus", since the
  guide records when a detection happened but not what was found.
* Honesty fixes: the interval claim now covers uncertainty in the expected
  count alone; a district above the cut whose interval lies wholly below one
  keeps a slate accent rather than green; detection tiles name serotypes per
  channel and count years rather than detections.
* Layout fixes: a long unit name is sized to the row and breaks over two lines;
  a unit sharing its parent's name keeps the parent; the page grows past one
  sheet when the masthead fills.
* Smaller fixes: `note` prints in the footer alone and is opt-in; an expected
  count below ten keeps a decimal; an unsupplied channel reads as unsupplied.

### SPI, concordance and inputs

* Added `spi_check_inputs()`, a graded pre-flight that reconciles case counts,
  population denominators and the shapefile before any model runs, reporting id
  mismatches, panel gaps, bad populations, partial coverage and invalid geometry
  at once. `spi_expected()` and `spi_compare_overdispersion()` gained `check`
  (default `TRUE`) and route through it.
* `spi_index()` gained `year_end_month`, so a reading year can close on the month a
  review closes. `year_end_month = 4` groups May through April, labelled by the
  year it closes in; `n_months` marks the partial window at each end.
* `spi_concordance()` gained `year_end_month` too. Pass the same value the SPI
  was computed with and the conventional NPAFP rate is grouped on the same
  rolling year; left at the calendar default against a rolling SPI, it counted
  part of the window against a whole-year denominator and understated the rate.
* `spi_index()` gained `boundaries`, joining admin names immediately before the
  district id. `spi_concordance()` places its own name columns the same way.
* `spi_index()` gained `centre` (default `"national"`). Every district's SPI for
  a period is divided by that period's national observed-to-expected ratio,
  so a country-wide reporting change no longer moves every district the same
  way. The result gains `$national`, the per-period ratio; `centre = "none"`
  keeps the raw ratio and sets `$national` to `NULL`. A period with no
  detections nationally has no ratio to divide by, so its rows are `NA`
  rather than `Inf`, with a warning naming the affected periods.
* Concordance maps draw a legend key for a fill level no district fell into,
  which `geom_sf` had rendered as a blank swatch.

### Model fitting

* `spi_expected()` defaults changed to the paper specification:
  `overdispersion = "nb"` (was `"iid"`) and `year_effect = "iid"` (was
  `"none"`). The previous bare spec still works, by passing both arguments
  explicitly. `spi_compare_overdispersion()` is unaffected, since it already
  sets `overdispersion` per spec.
* `spi_expected(seed = )` now reaches INLA's own RNG rather than R's alone.
  Seeding with `set.seed()` left `inla.posterior.sample()` unseeded, so two runs
  of identical seeded code disagreed by enough to move SPI and flip verdicts.
* `spi_expected()` gained `num_threads`, which defaults to `"1:1"` alongside a
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
