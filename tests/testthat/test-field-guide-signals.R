# The optional machinery of bs_field_guide() -- the timeliness and stool
# adequacy components and the neighbour / seasonal / genomic / ES context
# helpers -- only runs when the optional inputs are supplied. These build a field guide from
# constructed inputs (make_*, see helper-fixtures.R) so those branches run
# without an INLA fit.

ids6 <- paste0("FG", 1:6)
seasonal_map <- list(FG1 = "present", FG2 = "blind", FG3 = "muted",
                     FG4 = "present", FG5 = "none", FG6 = "present")

# the fixtures are engineered around a 0.8 cut, so the tests pass it
# explicitly rather than take the field guide's default of 1
build_full_guide <- function(genomic = make_genomic(), genomic_col = NULL,
                             es = make_es(), es_col = "n_positive",
                             process = make_process(), spi_cut = 0.8,
                             verbose = FALSE, ...) {
  conc <- make_concordance()
  nb <- make_nb(ids6)
  sm <- make_spi_month("adm2_guid", ids6, 2019:2024, seasonal_map)
  bs_field_guide(conc, process = process, adjacency = nb, spi_month = sm,
                 genomic = genomic, genomic_col = genomic_col, es = es,
                 es_col = es_col, spi_cut = spi_cut, verbose = verbose, ...)
}

test_that("all optional signals compute when the inputs are supplied", {
  fg <- build_full_guide()
  expect_s3_class(fg, "blindspot_field_guide")
  expect_true(all(fg$signals_active))

  dy <- fg$district_year
  # neighbour contrast produced real values (not all NA) and a discordance
  expect_false(all(is.na(dy$neighbour_spi)))
  expect_true(any(dy$neighbour_discordant, na.rm = TRUE))
  # seasonal produced blind / muted / present / not-assessed labels
  expect_true(any(dy$seasonally_blind, na.rm = TRUE))
  expect_true(all(c("blind", "muted", "present") %in% dy$seasonal))
  # genomic orphan attached to FG1 / FG2
  expect_true(any(dy$genomic_orphan, na.rm = TRUE))
  # every judgement reachable
  expect_setequal(
    as.character(dy$verdict),
    c("Review priority", "Monitor", "No SPI indication")
  )
})

test_that("genomic_col filters detections and validates its name", {
  fg <- build_full_guide(genomic_col = "any_cvdpv2")
  expect_true(any(fg$district_year$genomic_orphan, na.rm = TRUE))

  expect_error(build_full_guide(genomic_col = "missing_flag"), "genomic_col")
})

test_that("context signals never move the judgement", {
  full <- build_full_guide()
  bare <- bs_field_guide(make_concordance(), spi_cut = 0.8, verbose = FALSE)
  expect_identical(full$district_year$verdict, bare$district_year$verdict)
})

test_that("traj_alpha gates a volatile slope to flat, not falling", {
  years <- 2019:2024
  # a volatile series with a net-negative slope but no significant trend: the
  # KAHMARD case the review flagged (spikes and craters, not a steady decline)
  spi <- c(0.95, 0.90, 0.30, 0.85, 0.30, 0.75)
  dy <- tibble::tibble(
    adm2_guid = "V1", adm2_name = "Volatile", adm1_name = "P", year = years,
    observed = 2L, expected_total = 12, spi_median = spi,
    spi_q05 = pmax(spi - 0.15, 0.05), spi_q95 = spi + 0.10,
    npafp_rate = 5, npafp_adequate = TRUE
  )
  conc <- structure(
    list(
      district_year = dy,
      thresholds = list(spi = 0.8, npafp = 3, multiplier = 1e5),
      id_col = "adm2_guid", call = quote(bs_concordance())
    ),
    class = "blindspot_concordance"
  )

  foc <- function(fg) fg$focal[fg$focal$adm2_guid == "V1", ]
  # the default bare slope sign reads the endpoint drop as "falling"; the
  # recommended significance gate (traj_alpha = 0.1) calls it "flat"
  expect_identical(foc(bs_field_guide(conc, verbose = FALSE))$trajectory,
                   "falling")
  gated <- bs_field_guide(conc, traj_alpha = 0.1, verbose = FALSE)
  expect_identical(foc(gated)$trajectory, "flat")
})

test_that("neighbourhood_shortfall names the region-wide absorption case", {
  fg <- build_full_guide()
  dy <- fg$district_year
  expect_true("neighbourhood_shortfall" %in% names(dy))

  # FG1 is below the cut and so is its neighbour FG2: region-wide, not a local
  # discordance -- and the two states never overlap
  fg1 <- fg$focal[fg$focal$adm2_guid == "FG1", ]
  expect_true(fg1$neighbourhood_shortfall)
  expect_false(fg1$neighbour_discordant)
  expect_false(any(dy$neighbourhood_shortfall & dy$neighbour_discordant,
                   na.rm = TRUE))

  # without adjacency the column is present but NA (graceful degradation)
  bare <- bs_field_guide(make_concordance(), verbose = FALSE)
  expect_true(all(is.na(bare$district_year$neighbourhood_shortfall)))
})

test_that("process counts become timeliness and adequacy concerns", {
  f <- build_full_guide()$focal
  row <- function(id) f[f$adm2_guid == id, ]

  # FG1: 3 of 8 specimens within 3 days is below the 80% target
  expect_equal(row("FG1")$pct_transport_timely, 100 * 3 / 8)
  expect_true(row("FG1")$timeliness_concern)
  expect_false(row("FG1")$adequacy_concern)
  # FG3: 5 of 10 adequate stool specimens
  expect_true(row("FG3")$adequacy_concern)
  expect_false(row("FG3")$timeliness_concern)
  # FG2: three cases is below the five-case floor, so neither is assessed
  expect_false(row("FG2")$transport_assessed)
  expect_true(is.na(row("FG2")$timeliness_concern))
  expect_true(is.na(row("FG2")$adequacy_concern))
  # FG4 is absent from `process`, so it has no cases rather than no data
  expect_equal(row("FG4")$n_cases, 0L)
  expect_true(is.na(row("FG4")$adequacy_concern))

  bad <- make_process()
  bad$n_transport_timely <- NULL
  expect_error(build_full_guide(process = bad), "n_transport_timely")
})

test_that("bs_field_guide degrades and warns without optional inputs", {
  conc <- make_concordance()
  expect_message(
    bare <- bs_field_guide(conc, spi_cut = 0.8, verbose = TRUE),
    "Not computed"
  )
  expect_identical(
    names(bare$signals_active)[!bare$signals_active],
    c("timeliness", "adequacy", "surroundings", "seasonal", "detect_afp",
      "detect_es")
  )
  # admin-1 names are in the concordance, so extent is still computed
  expect_true(bare$signals_active[["extent"]])
  expect_true(all(is.na(bare$district_year$neighbour_spi)))
  expect_true(all(is.na(bare$district_year$seasonal)))

  expect_error(
    bs_field_guide(conc, extent_col = "state", verbose = FALSE), "extent_col"
  )
})

test_that("bs_field_guide validates read_year, columns, and spi_month level", {
  conc <- make_concordance()
  expect_error(bs_field_guide(conc, read_year = 1990L, verbose = FALSE),
               "no district-year rows")

  bad <- conc
  bad$district_year$expected_total <- NULL
  expect_error(bs_field_guide(bad, verbose = FALSE), "missing column")

  # spi_month must be a district_month bs_spi result
  wrong_level <- make_spi_dy(id_col = "adm2_guid")
  expect_error(
    bs_field_guide(conc, spi_month = wrong_level, verbose = FALSE),
    "district_month"
  )
})

test_that("bs_field_guide_help runs every topic and the synth fallback", {
  fg <- build_full_guide()
  expect_no_error(bs_field_guide_help("all"))            # expands to all topics

  # worked example narrated from a supplied guide: the four teaching cases
  w <- bs_field_guide_help("example", guide = fg)
  expect_s3_class(w, "tbl_df")
  expect_setequal(w$case_label, c(
    "At or above expectation", "Uncertain shortfall",
    "Large, corroborated shortfall",
    "Shortfall without spatial corroboration"
  ))

  # default guide argument loads the shipped synth_field_guide
  expect_no_error(bs_field_guide_help("example"))
})

test_that("worked-example selection handles explicit and missing districts", {
  fg <- build_full_guide()
  # explicit selection, including an id that does not exist -> dropped
  sel <- blindspot:::.fg_select_worked(
    fg$focal, fg$id_col, fg$thresholds$spi,
    districts = c("FG1", "NOPE")
  )
  expect_equal(sel$adm2_guid, "FG1")
})

test_that("field guide tables render and cover the cell factories", {
  skip_if_not_installed("gt")
  fg <- build_full_guide()

  # worked layout exercises .fg_cell_signals / .fg_cell_classes / s6 / s7 cells
  expect_s3_class(
    bs_field_guide_table(fg, engine = "gt", layout = "worked"), "gt_tbl"
  )
  expect_s3_class(
    bs_field_guide_table(fg, engine = "gt", layout = "scan"), "gt_tbl"
  )

  # a guide with no genomic input -> "genomic not assessed" cell branch
  fg_ng <- build_full_guide(genomic = NULL)
  expect_s3_class(
    bs_field_guide_table(fg_ng, engine = "gt", layout = "worked"), "gt_tbl"
  )

  # a focal year with no rows aborts
  expect_error(
    bs_field_guide_table(fg, engine = "gt", year = 1990L),
    "no district-year rows"
  )
})

test_that("neighbour / seasonal signals degrade on odd adjacency inputs", {
  conc <- make_concordance()

  # adjacency without a region.id attribute -> indices used as ids
  nb_noid <- make_nb(ids6)
  attr(nb_noid, "region.id") <- NULL
  expect_no_error(bs_field_guide(conc, adjacency = nb_noid, verbose = FALSE))

  # adjacency where every node is an island -> no edges at all
  nb_islands <- structure(
    lapply(seq_along(ids6), function(i) 0L),
    region.id = as.character(ids6),
    ncomp = list(nc = length(ids6)),
    class = c("blindspot_nb", "nb")
  )
  fg_isl <- bs_field_guide(conc, adjacency = nb_islands, verbose = FALSE)
  expect_true(all(is.na(fg_isl$district_year$neighbour_spi)))

  # spi_month whose summary lacks the seasonal columns -> seasonal stays NA
  sm_bad <- structure(
    list(
      summary = tibble::tibble(adm2_guid = "FG1", year = 2024L),
      level = "district_month", id_col = "adm2_guid"
    ),
    class = "blindspot_spi"
  )
  fg_bs <- bs_field_guide(conc, spi_month = sm_bad, verbose = FALSE)
  expect_true(all(is.na(fg_bs$district_year$seasonal)))
})

test_that("field-guide cell + narrative helpers cover their branches", {
  # shares that round alike print a decimal, so a concern never reads as a tie
  expect_equal(blindspot:::.fg_pct_pair(55.6, 55.5), c("55.6%", "55.5%"))
  expect_equal(blindspot:::.fg_pct_pair(60, 40), c("60%", "40%"))

  # process cells: not supplied, below the floor, and assessed
  expect_equal(blindspot:::.fg_process_cell(NA, NA, "x", 5L), "Not supplied")
  expect_match(blindspot:::.fg_process_cell(2, 3, "x", 5L), "not assessed")
  expect_match(blindspot:::.fg_process_cell(6, 8, "x", 5L), "6 of 8 x \\(75%\\)")

  # .fg_tag returns NULL for an empty pick
  expect_null(blindspot:::.fg_tag(NULL, "Monitor"))

  # .fg_narrate fallback for an unrecognised case label
  r <- list(case_label = "Other", verdict = "Monitor", year = 2024L,
            spi_median = 0.9, spi_q05 = 0.8, spi_q95 = 1.0,
            spi_previous = NA_real_, extent_others = 0L,
            expected_total = 5)
  expect_match(blindspot:::.fg_narrate(r, 1), "Judgement: Monitor")
  expect_match(blindspot:::.fg_narrate(r, 1), "no SPI for the previous year")
})

test_that("worked table aborts when no districts can be selected", {
  skip_if_not_installed("gt")
  fg <- build_full_guide()
  expect_error(
    bs_field_guide_table(fg, engine = "gt", layout = "worked",
                         districts = "NOPE"),
    "no districts"
  )
})

test_that("bs_field_guide_table save dispatch covers formats and errors", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  fg <- build_full_guide()

  f_html <- withr::local_tempfile(fileext = ".html")
  bs_field_guide_table(fg, engine = "flextable", layout = "scan",
                       file = f_html)
  expect_true(file.exists(f_html) && file.info(f_html)$size > 0)

  f_pptx <- withr::local_tempfile(fileext = ".pptx")
  bs_field_guide_table(fg, engine = "flextable", layout = "worked",
                       file = f_pptx)
  expect_true(file.exists(f_pptx))

  # an unsupported flextable extension aborts with a helpful message
  f_bad <- withr::local_tempfile(fileext = ".rtf")
  expect_error(
    bs_field_guide_table(fg, engine = "flextable", file = f_bad),
    "cannot save"
  )
})

test_that("gt save dispatch checks the docx dependency", {
  skip_if_not_installed("gt")
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  fg <- build_full_guide()
  f_docx <- withr::local_tempfile(fileext = ".docx")
  bs_field_guide_table(fg, engine = "gt", layout = "scan", file = f_docx)
  expect_true(file.exists(f_docx) && file.info(f_docx)$size > 0)
})

# --- the two count-basis gates --------------------------------------------

test_that("noise_alpha closes the gate a zero count opens for free", {
  # observed 0 against 2 expected: SPI = 0 / expected_draw is identically 0, so
  # the interval collapses to (0, 0) and clears one whatever the count basis
  conc <- make_count_concordance(list(
    ZERO = list(spi = c(0.5, 0.4, 0, 0, 0), q95 = c(0.6, 0.5, 0, 0, 0),
                observed = c(1, 1, 0, 0, 0), expected = 2)
  ))

  base <- bs_field_guide(conc, spi_cut = 0.8, verbose = FALSE)
  f <- base$focal
  expect_true(f$cri_excludes_1)
  expect_true(f$gate_pass)
  expect_identical(as.character(f$verdict), "Review priority")
  # the tail is reported even with no gate asked for
  expect_equal(f$noise_tail, stats::ppois(0, 2), tolerance = 1e-9)
  expect_true(f$noise_plausible)

  gated <- bs_field_guide(conc, noise_alpha = 0.05, spi_cut = 0.8,
                          verbose = FALSE)
  g <- gated$focal
  # the interval still excludes one -- it collapsed to (0, 0). What shut is the
  # noise gate, and the two are reported apart so a reading can say which
  expect_true(g$cri_excludes_1)
  expect_false(g$gate_pass)
  # below the cut with the gate shut is monitor, not review priority
  expect_identical(as.character(g$verdict), "Monitor")

  # a well-powered zero is untouched: P(X = 0 | 30) is vanishing
  powered <- make_count_concordance(list(
    ZERO = list(spi = c(0.5, 0.4, 0, 0, 0), q95 = c(0.6, 0.5, 0, 0, 0),
                observed = c(1, 1, 0, 0, 0), expected = 30)
  ))
  p <- bs_field_guide(powered, noise_alpha = 0.05, verbose = FALSE)$focal
  expect_false(p$noise_plausible)
  expect_true(p$cri_excludes_1)
  expect_true(p$gate_pass)
})

test_that("noise_alpha leaves cri_excludes_1 and the default read alone", {
  # the regression that matters: `noise_plausible` is computed whether or not
  # it gates, so folding it into `gate_pass` unconditionally would silently
  # apply a 5% noise gate to the published spec.
  conc <- make_concordance()
  base <- bs_field_guide(conc, verbose = FALSE)$district_year

  expect_identical(base$cri_excludes_1, base$spi_q95 < 1)
  expect_identical(base$gate_pass, base$cri_excludes_1)
  expect_true(any(base$noise_plausible))

  gated <- bs_field_guide(conc, noise_alpha = 0.05,
                          verbose = FALSE)$district_year
  # the interval column is the same object either way; only the gate moves
  expect_identical(gated$cri_excludes_1, base$cri_excludes_1)
  expect_identical(gated$gate_pass, gated$cri_excludes_1 &
                     !gated$noise_plausible)
})

test_that("a certain but uncorroborated shortfall reads Monitor", {
  # below the cut with the interval wholly below one, but the previous year was
  # at or above the cut and the district has no neighbours in its area, so
  # neither extent nor persistence corroborates
  conc <- make_count_concordance(list(
    LONE = list(spi = c(0.90, 0.60), q95 = c(1.10, 0.70),
                observed = 6, expected = 10)
  ))
  f <- bs_field_guide(conc, spi_cut = 0.8, verbose = FALSE)$focal

  expect_true(f$spi_below)
  expect_true(f$gate_pass)
  expect_false(f$persistence_concern)
  expect_true(is.na(f$extent_concern))
  expect_identical(as.character(f$verdict), "Monitor")
  expect_identical(
    levels(f$verdict), c("Review priority", "Monitor", "No SPI indication")
  )
})

test_that("noise_alpha must be a probability", {
  conc <- make_concordance()
  expect_error(bs_field_guide(conc, noise_alpha = 0, verbose = FALSE),
               "probability")
  expect_error(bs_field_guide(conc, noise_alpha = 1.5, verbose = FALSE),
               "probability")
})

test_that("persistence reads the previous year, not a historical run", {
  # three sub-cut years, then recovery, then one sub-cut year: the previous
  # year was above the cut, so persistence does not fire despite the old run
  conc <- make_count_concordance(list(
    STALE = list(spi = c(0.5, 0.5, 0.5, 1.1, 1.05, 0.7),
                 q95 = c(0.6, 0.6, 0.6, 1.3, 1.25, 0.75),
                 observed = 4, expected = 12)
  ))
  f <- bs_field_guide(conc, spi_cut = 0.8, verbose = FALSE)$focal

  expect_equal(f$spi_previous, 1.05)
  expect_false(f$persistence_concern)
  expect_identical(as.character(f$verdict), "Monitor")
  # both runs are still reported as context
  expect_equal(f$longest_run_below, 3L)
  expect_equal(f$trailing_run_below, 1L)
})

test_that("the defaults follow the field guide", {
  conc <- make_concordance()
  base <- bs_field_guide(conc, verbose = FALSE)
  # STEPS applies below 1 whatever cut the concordance used
  expect_equal(base$thresholds$spi, 1)
  expect_null(base$params$noise_alpha)
  expect_equal(base$params$process_target, 80)
  expect_equal(base$params$process_min_cases, 5L)
  expect_false(any(c("min_corroborators", "persistence_basis") %in%
                     names(base$params)))
})

test_that("detection_serotypes keeps ambiguous virus out of the channels", {
  conc <- make_count_concordance(list(
    D1 = list(spi = c(0.5, 0.5, 0.5), q95 = c(0.6, 0.6, 0.6),
              observed = 4, expected = 12)
  ))
  g <- tibble::tibble(
    adm2_guid = "D1",
    year = c(2023L, 2024L),
    serotype = c("aVDPV2", "cVDPV2")
  )

  # unfiltered, an ambiguous VDPV counts like any other row
  all_in <- bs_field_guide(
    conc, genomic = g, serotype_col = "serotype", verbose = FALSE
  )$focal
  expect_equal(all_in$orphan_serotypes, "aVDPV2, cVDPV2")
  expect_equal(all_in$orphan_years, "2023, 2024")

  # filtered, only the confirmed virus reaches the years, the flag and the
  # serotype string, so all three agree
  kept <- bs_field_guide(
    conc, genomic = g, serotype_col = "serotype",
    detection_serotypes = c("WPV1", "cVDPV1", "cVDPV2", "cVDPV3"),
    verbose = FALSE
  )$focal
  expect_equal(kept$orphan_serotypes, "cVDPV2")
  expect_equal(kept$orphan_years, "2024")
  expect_true(kept$genomic_orphan)

  # filtering everything out leaves no detection at all
  none <- bs_field_guide(
    conc, genomic = g, serotype_col = "serotype",
    detection_serotypes = "WPV1", verbose = FALSE
  )$focal
  expect_equal(none$orphan_years, "")
  expect_false(none$genomic_orphan)

  # and asking to filter without saying where the serotype lives is an error
  expect_error(
    bs_field_guide(
      conc, genomic = g, detection_serotypes = "cVDPV2", verbose = FALSE
    ),
    "serotype_col"
  )
})
