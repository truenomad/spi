# blindspot 0.1.0.9000

* **Surroundings stayed silent where the gap was widest.** The S rung counted
  `neighbour_discordant` alone — a short district against healthy neighbours,
  which is a local gap. But a district short inside an equally short
  neighbourhood is the harder case, not the easier one: no nearby
  well-performing district can act as a sentinel, and the spatial model absorbs
  an area-wide shortfall into the expectation, so the index understates it. The
  rung now fires on `neighbourhood_shortfall` too, with the pair capped at one
  corroborator, so the corroborator scale is unchanged and the same spatial
  evidence is not counted twice. The pager's surroundings line reads by the
  same rule.
* **`bs_spi(year_end_month = )`, for a reading year that is not a calendar
  year.** `level = "district_year"` always grouped January to December, so a
  review closing in April had to be reported against a window it had not used.
  `year_end_month = 4` groups May through April and labels each window by the
  calendar year it closes in. Only the aggregation moves: the fit is untouched,
  and the monthly offset still uses the calendar-year denominator it was fitted
  on. The summary now carries `n_months`, because the first and last windows of
  a series are almost always partial and should normally be dropped.
* **`bs_field_guide_pager()` gains `region`, `year_label` and `prob_under`.**
  `region` adds a header line placing the district in its region, with an
  optional rank among regions — triage only, never part of the verdict, since a
  rank exists whether or not anything is wrong. `year_label` replaces the bare
  year in the masthead when the window is not a calendar year. `prob_under`
  appends the posterior `P(SPI < 1)` to the significance line, which is
  otherwise only pass or fail; its tails print as "over 99%" and "under 1%"
  rather than a certainty the draws do not carry. The page now grows past one
  sheet instead of clipping when these lines push it over.
* **Empty concordance levels drew blank legend keys.** `geom_sf` takes its key
  glyph from the data, so a fill level no district fell into rendered as an
  empty swatch beside its label. Each map now pads its frame with one empty
  geometry per missing level, so every key draws in its own colour while
  nothing is added to the map.
* **`cri_excludes_1` meant two things.** With `noise_alpha` set it was silently
  the interval *and* the noise tail, while every string built from it said only
  "the credible interval excludes 1". It now means `spi_q95 < 1`, always; the
  composite the verdict turns on is the new `gate_pass`. On the default path
  the two are identical, so published FLAG and WATCH counts are unchanged and a
  regression test pins that. The cost was visible: with `noise_alpha = 0.05` a
  WATCH page printed an interval whose upper bound was below 1 and captioned it
  "still includes one" — chip, significance row, caption and banner, all four
  now branching on which half of the gate shut. `bs_field_guide_table()`
  carried the same wrong cell text.
* **New `REVIEW` verdict, between `FLAG` and `WATCH`.** A credible shortfall
  with too few corroborators used to fall to `No action`, beneath `WATCH`,
  whose defining property is that the evidence itself falls short. It is now
  its own tier: tables order by tier rather than flag-versus-rest, and
  `bs_triangulate()` grows from ten classes to thirteen (`review, ES positive`
  high, the other two medium). `No action` now means only "at or above the cut".
* **Detection tiles counted years and called them detections.** The guide
  records distinct years, never a count, so five ES-positive years read
  "5 detections" — a number the data cannot support. Now "5 years".
* **A unit sharing its parent's name lost the parent.** The hierarchy line
  dropped any admin level whose string matched the unit, so an LGA named after
  its state rendered as `BAUCHI · NIGERIA · district (admin-2)`,
  indistinguishable from the state's own page; the auto file name collided the
  same way. Both now key on which column names the unit.
* **The flag banner dated both channels by whichever found virus last.** "by
  AFP and ES, latest 2025" read as an AFP detection in 2025 when only ES
  reached it. Now "by AFP (2022) and ES (latest 2025)". Relatedly, "Sampling
  noise ruled out at 5%" fired whenever `noise_alpha` was set rather than when
  the tail cleared it.
* **`note` no longer prints in the eyebrow.** It is caller-supplied and can be
  long, and the eyebrow sits directly above the unit name at wide tracking, so
  a note like `"period Jun 2025 - May 2026"` wrapped to a second line and
  crowded the name. It now appears in the footer only, which has the width and
  is where provenance belongs.
* **The verdict banner held its first line short.** `text-wrap: balance`
  equalises line lengths, leaving a gap down the right; `pretty` fills the
  first line and still avoids a one-word last line.
* **`synth_field_guide` regenerated.** It predated the `bs_expected(seed = )`
  fix below, so it never matched its own build script; SPI moves by up to 0.12.
  It now carries `gate_pass` and the four-level verdict. Guides saved by an
  earlier version still render, falling back to `cri_excludes_1`.

* **`bs_expected(seed = ...)` now actually makes a fit reproducible, and
  gained `num_threads`.** The seed was applied with `set.seed()` alone. That
  reaches only R's RNG, which decides *which* posterior configuration each
  draw is taken from; the latent field drawn *within* that configuration comes
  from a separate RNG inside INLA's compiled code, and
  `inla.posterior.sample()` was never told the seed. Two runs of identical
  seeded code therefore disagreed. The drift was not confined to the last
  decimal: it moved SPI and flipped `bs_field_guide()` verdicts, and it could
  do so for a district whose own counts barely moved, because corroboration
  reads neighbouring districts' posteriors, which drifted too. Anyone who
  compared two runs — before and after adding a covariate, say — was reading
  part noise as part signal.

  Fixing the seed alone is not sufficient, so `num_threads` is new: `INLA::inla()`
  is not bit-reproducible multithreaded, since the hyperparameter mode and
  integration points shift with thread scheduling. It defaults to `"1:1"`
  whenever `seed` is set, which makes the default call reproducible at the cost
  of a serial fit; pass `num_threads = NULL` to inherit INLA's global thread
  setting and trade determinism back for speed. `seed` is now validated (a
  single non-negative whole number, or `NULL`) ahead of the INLA availability
  check, so a malformed argument is not reported as a missing package, and the
  caller's RNG state is restored on exit instead of being left displaced.
* **`bs_field_guide()` gained `noise_alpha`.** The SPI credible interval holds
  the observed count fixed, so at `observed = 0` it collapses to `(0, 0)` and
  `cri_excludes_1` is TRUE whatever the expected count. Setting `noise_alpha`
  also requires the Poisson tail `P(X <= observed | expected)` to fall at or
  below it, which subsumes a minimum-expected floor. New `noise_tail` /
  `noise_plausible` columns report the tail either way. Default NULL (published
  flag counts unchanged); `0.05` for an operational read.
* **`bs_field_guide()` gained `persistence_basis`.** `longest_run_below` is a
  running maximum, so persistence (P) can fire on a run that ended years ago.
  The new `trailing_run_below` is the run ending at each year, and `run_below`
  carries whichever gates. Default `"longest"`; `"trailing"` for an operational
  read.
* **`bs_field_guide_pager()` no longer overstates the credible interval.** "The
  shortfall is unlikely to be noise" was not licensed by an interval that holds
  the observed count fixed. The caption now claims only that uncertainty in the
  expected level does not account for the gap, and names the counts when chance
  alone could; a zero count reads as an empty count rather than an interval that
  survived something.
* **The pager gained a context row, and the five STEPS are five again.** Season,
  AFP detections and ES detections leave the grid for a row between the chart
  and the reading, so nothing outside the flag rule can be miscounted as a
  corroborator. AFP and ES get a box each, carrying the count and latest year;
  the chart already marks the years. The seasonal line shows only when it says
  something.
* **`bs_field_guide_pager()` gained `indicators_df`**, which fills that row with
  the non-polio AFP rate over the same years against its target, plus tiles for
  stool adequacy, the two timeliness percentages and the EV rate. The first
  three are pass rates over the district's own AFP cases, and a flagged district
  has few: below five assessable cases a tile spells out what it rests on ("1 of
  2 cases") and is left ungraded, and a district with none reads "none
  assessable". The EV rate is an ES measure, so it carries no case denominator
  and reads "no ES site" where the district has none. Default NULL.
* The pager prescribes no follow-up. The banner's action column ("Supervisory
  review", "active case search") is gone and the masthead states where the
  reading sits against the rule.
* The pager charts the focal district alone, and the locator inset now places it
  in its country rather than among its touching neighbours. `adjacency` is
  documented as ignored, and `bs_adjacency()` / \pkg{spdep} are not called.
* `bs_field_guide_pager()`'s `detection_label` defaults to NULL, and the page
  reads "poliovirus" rather than naming a serotype. The field guide records the
  years a detection occurred but not what was found, so the old `"cVDPV2"`
  default asserted a serotype the package was never told, and detections can as
  easily be cVDPV1, cVDPV3 or WPV1. Pass the label to declare what you filtered
  `genomic` / `es` down to; leave it NULL where the input mixes serotypes.
* **The pager no longer calls a district "Adequate" when its interval lies
  wholly below one.** Clearing the 0.80 operational cut is not evidence of
  adequate detection: a district can sit above the cut with its whole 90%
  interval below one, meaning it detects measurably less than the model expects.
  That case now reads "Below expectation" in neutral slate, and the banner says
  so instead of "the significance gate never opens".
* **`bs_field_guide()` gained `serotype_col` and `detection_serotypes`.** The
  guide recorded the years a detection occurred but not what was found, so a
  reading had to assume one serotype. With `serotype_col` the distinct serotypes
  seen up to each year are kept as `orphan_serotypes` / `es_serotypes`, and the
  pager names them instead of assuming. `detection_serotypes` restricts what
  counts as a detection at all: an ambiguous VDPV is not a confirmed circulating
  virus, so `c("WPV1", "cVDPV1", "cVDPV2", "cVDPV3")` keeps it out of the years,
  the flags and the serotype string together. Both default NULL.
* **The verdict banner no longer pools the serotypes across channels.** It
  named the union of what AFP and ES recorded and attributed it to both, so a
  district where AFP found five serotypes and ES found one read as "aVDPV2,
  cVDPV2, cVDPV3, VDPV2, VDPV3 detected by AFP and ES" — crediting ES with
  four it never saw. The tiles already name them per channel and remain the
  place to read them; the banner names the channels and the year. That also
  takes the one data-length-driven clause out of a fixed-width banner, and the
  label stacks the qualifier under the verdict instead of taking a quarter of
  the row from the reading.
* **A long unit name is fitted to the page instead of running off it.** Two
  places assumed a short one. The masthead is a flex row whose verdict column
  cannot shrink, so a name like KOLOKUMA/OPOKUMA pushed that column past the
  page edge and the sub-lines silently lost their last characters ("pop u15
  63,59"); the name is now sized to what the row can spare. The chart's endpoint
  label estimated Archivo at weight 900 a fifth narrower than it renders, so a
  name the arithmetic called a fit was still clipped ("OGBA/EGBEMA/NDC"); it now
  measures correctly and breaks the name over two lines, at a space or after a
  solidus, which is the only place names of that shape give.
* The verdict banner dates the detection it corroborates on. Detections are
  cumulative to the read year, so "with WPV1 detected by AFP" could rest on
  virus found five years earlier; it now reads "in 2020" or "latest 2022", as
  the detection tiles already did.
* `synth_surveillance` gained `afp_timeliness`, a district-year table of
  assessable and within-window AFP counts. Onset-to-notification is the one
  indicator POLIS does not publish, so the demo and test fixture derive that
  tile from these counts rather than simulating it. Purely additive: every other
  table in the bundle is unchanged.
* The masthead carries the under-15 population, the denominator the expected
  count is built on. The footer's "computed from bs_spi() posterior" line goes.
* Smaller pager fixes: persistence names the run ending at the read year; an
  expected count below ten keeps a decimal, so "1 case against 1 expected" no
  longer contradicts a stated SPI of 0·70; an unsupplied channel reads as
  unsupplied rather than as a finding, fixing an `NA` year string that rendered
  as "cVDPV2 detected in AFP (NA)"; the neighbour median is compared against the
  cut on the printed values; the endpoint label clears the locator badge; and
  the `note` provenance tag is opt-in.
* **The field guide is now framed as five signals -- STEPS (significance,
  trend, extent, persistence, surroundings)** -- matching the paper's
  "Interpreting and acting on the SPI" section. `bs_field_guide()`, its tables,
  the console help, and `bs_field_guide_pager()` all read out the five STEPS:
  significance is the entry point, extent grades depth (merging the old
  "depth of shortfall" and "observed vs expected" into one row), and trend,
  persistence and surroundings are the three corroborators. **The flag rule is
  now two of three corroborators (trend, persistence, surroundings)**, down
  from two of four: seasonal blindness is no longer a counted corroborator.
  Seasonal blindness and any AFP / ES detection are now **out-of-grid
  corroboration** -- computed and reported (in the scan table and the pager),
  strengthening a flag from outside the grid, but never one of the five STEPS
  and never in the count. This changes default flag counts against the previous
  seven-signal spec; the shipped `synth_field_guide` is regenerated to match.
* `bs_spi()` gained a `boundaries` argument. When supplied (an `sf` layer or a
  plain data frame keyed by the id column), the admin name columns
  (`adm1_name`, `adm2_name`, ...) are joined onto the `summary` output and
  placed immediately before the district id, so saved SPI tables carry
  human-readable labels next to `adm2_guid`. `bs_concordance()` already joined
  boundary names; those are now likewise moved to just before the id column,
  and `bs_field_guide()` inherits the ordering from the concordance table.
* `bs_field_guide()` gained three opt-in settings, all off by default so the
  published flag counts reproduce out of the box. `traj_alpha` turns the trend
  signal (T) into a significance-gated trend test: a trend reads
  "falling"/"rising" only if its OLS slope differs from zero at that two-sided
  level (needing three or more points), else "flat". This stops an endpoint
  drop or a single volatile year from reading as a sustained decline, matching
  the paper's trend (T) intent ("a sustained downward slope is a trend, not a
  single anomalous year"); `traj_alpha = 0.1` is the recommended setting and
  improves specificity on volatile series. `dedupe_temporal` counts a falling
  trend (T) and a persistent sub-cut run (P) as a single "temporal" corroborator
  rather than two, so a flag cannot rest on two readings of the same decline --
  the case that bites where neighbours drop out and the 2-of-3 rule reduces to
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