# The optional-signal machinery of bs_field_guide() -- the surroundings (S)
# neighbour helper and the out-of-grid seasonal / genomic / ES helpers -- only
# runs when the optional inputs are supplied. These build a field guide from
# constructed inputs (make_*, see helper-fixtures.R) so those branches run
# without an INLA fit.

ids6 <- paste0("FG", 1:6)
seasonal_map <- list(FG1 = "present", FG2 = "blind", FG3 = "muted",
                     FG4 = "present", FG5 = "none", FG6 = "present")

build_full_guide <- function(genomic = make_genomic(), genomic_col = NULL,
                             es = make_es(), es_col = "n_positive",
                             verbose = FALSE, ...) {
  conc <- make_concordance()
  nb <- make_nb(ids6)
  sm <- make_spi_month("adm2_guid", ids6, 2019:2024, seasonal_map)
  bs_field_guide(conc, adjacency = nb, spi_month = sm, genomic = genomic,
                 genomic_col = genomic_col, es = es, es_col = es_col,
                 verbose = verbose, ...)
}

test_that("all optional signals compute when the inputs are supplied", {
  fg <- build_full_guide()
  expect_s3_class(fg, "blindspot_field_guide")
  expect_true(all(fg$signals_active))

  dy <- fg$district_year
  # surroundings (S) produced real values (not all NA) and a discordance
  expect_false(all(is.na(dy$neighbour_spi)))
  expect_true(any(dy$neighbour_discordant, na.rm = TRUE))
  # out-of-grid seasonal produced blind / muted / present / not-assessed labels
  expect_true(any(dy$seasonally_blind, na.rm = TRUE))
  expect_true(all(c("blind", "muted", "present") %in% dy$seasonal))
  # out-of-grid genomic orphan attached to FG1 / FG2
  expect_true(any(dy$genomic_orphan, na.rm = TRUE))
  # every verdict level reachable
  expect_true(all(c("FLAG", "WATCH", "No action") %in%
                    as.character(dy$verdict)))
})

test_that("genomic_col filters detections and validates its name", {
  fg <- build_full_guide(genomic_col = "any_cvdpv2")
  expect_true(any(fg$district_year$genomic_orphan, na.rm = TRUE))

  expect_error(build_full_guide(genomic_col = "missing_flag"), "genomic_col")
})

test_that("dedupe_temporal collapses the trajectory + persistence pair", {
  base <- build_full_guide()
  dedup <- build_full_guide(dedupe_temporal = TRUE)

  # FG1 flags on a falling, persistent trajectory with quiet neighbours/season,
  # so its two temporal signals are the whole corroboration
  fg1 <- function(fg) fg$focal[fg$focal$adm2_guid == "FG1", ]
  expect_equal(fg1(base)$corroborators, 2L)
  expect_identical(as.character(fg1(base)$verdict), "FLAG")
  # counted once, the temporal pair no longer clears the 2-of-N rule
  expect_equal(fg1(dedup)$corroborators, 1L)
  expect_false(as.character(fg1(dedup)$verdict) == "FLAG")
  expect_true(dedup$params$dedupe_temporal)
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

test_that("detection_corroborates counts a detection as an extra signal", {
  base <- build_full_guide()
  withdet <- build_full_guide(detection_corroborates = TRUE)

  # FG2 carries a genomic orphan; counting it lifts the corroborator tally by 1
  fg2 <- function(fg) fg$focal[fg$focal$adm2_guid == "FG2", ]
  expect_true(fg2(base)$genomic_orphan)
  expect_equal(fg2(withdet)$corroborators, fg2(base)$corroborators + 1L)
  expect_true(withdet$params$detection_corroborates)
  # default keeps the detection out of the count (paper's spec)
  expect_false(base$params$detection_corroborates)
})

test_that("bs_field_guide degrades and warns without optional inputs", {
  conc <- make_concordance()
  bare <- bs_field_guide(conc, min_corroborators = 10L, verbose = TRUE)
  expect_false(any(bare$signals_active))
  expect_true(all(is.na(bare$district_year$neighbour_spi)))
  expect_true(all(is.na(bare$district_year$seasonal)))
  # min_corroborators above the assessable count means nothing can flag
  expect_false(any(bare$district_year$verdict == "FLAG"))
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

  # worked example narrated from a supplied guide (genomic -> corroborated flag)
  w <- bs_field_guide_help("example", guide = fg)
  expect_s3_class(w, "tbl_df")
  expect_true("Flag, corroborated" %in% w$case_label)

  # a guide with no genomic input falls back to the persistent-flag narrative
  fg_ng <- build_full_guide(genomic = NULL)
  w2 <- bs_field_guide_help("example", guide = fg_ng)
  expect_true("Flag, persistent" %in% w2$case_label)

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
  # .fg_s6_cell: island, unassessable neighbours, and a normal contrast
  expect_match(blindspot:::.fg_s6_cell(list(island = TRUE)), "no adjacent")
  expect_match(
    blindspot:::.fg_s6_cell(list(island = FALSE, neighbour_spi = NA_real_)),
    "not assessable"
  )
  expect_match(
    blindspot:::.fg_s6_cell(list(island = FALSE, neighbour_spi = 0.9,
                                 spi_median = 0.5)),
    "neighbour median"
  )

  # .fg_tag returns NULL for an empty pick
  expect_null(blindspot:::.fg_tag(NULL, "Watch"))

  # .fg_narrate fallback for an unrecognised case label
  r <- list(case_label = "Other", verdict = "No action", spi_median = 0.9,
            spi_q05 = 0.8, spi_q95 = 1.0, longest_run_below = 0L,
            expected_total = 5)
  expect_match(blindspot:::.fg_narrate(r, 0.8), "verdict")
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
