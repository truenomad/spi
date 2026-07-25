# bs_field_guide_pager() renders a single district's reading to self-contained
# HTML. Driven by the shipped synth_field_guide, so no INLA is needed. District
# names are derived from the data rather than hard-coded, so the tests survive
# a regeneration of the synthetic bundle.

flag_district <- function(fg) {
  foc <- fg$focal
  foc[[".name"]] <- foc[["adm2_name"]]
  foc[foc$verdict == "FLAG", ][[".name"]][1]
}

adequate_district <- function(fg) {
  foc <- fg$focal
  cut <- fg$thresholds$spi
  ok <- foc[foc$verdict == "No action" & foc$spi_median >= cut, ]
  ok[["adm2_name"]][1]
}

below_cut_no_action_district <- function(fg) {
  foc <- fg$focal
  ok <- foc[foc$verdict == "No action" & foc$spi_below, ]
  ok[["adm2_name"]][1]
}

test_that("a flag district renders a well-formed pager object", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))

  p <- bs_field_guide_pager(fg, district = d, verbose = FALSE)

  expect_s3_class(p, "blindspot_pager")
  expect_identical(p$verdict, "FLAG")
  expect_identical(as.character(p), p$html)
  expect_length(p$paths, 0L)

  html <- p$html
  expect_match(html, "<!DOCTYPE html>", fixed = TRUE)
  expect_match(html, "<svg", fixed = TRUE)
  # five STEPS rows; season and detections read in the context row instead
  expect_equal(lengths(regmatches(html, gregexpr("class=\"srow\"", html))), 5L)
  expect_match(html, "vbanner", fixed = TRUE)
  # flag accent is rose
  expect_match(html, "--accent:#c8102e", fixed = TRUE)
})

test_that("an adequate district switches the accent to green", {
  fg <- synth_field_guide
  d <- adequate_district(fg)
  skip_if(is.na(d))

  p <- bs_field_guide_pager(fg, district = d, verbose = FALSE)
  expect_identical(p$verdict, "No action")
  expect_match(p$html, "--accent:#1f6f43", fixed = TRUE)
  expect_match(p$html, "gate stays shut", fixed = TRUE)
})

test_that("a below-cut no-action district is not called adequate", {
  fg <- synth_field_guide
  d <- below_cut_no_action_district(fg)
  skip_if(is.na(d))

  p <- bs_field_guide_pager(fg, district = d, verbose = FALSE)
  expect_identical(p$verdict, "No action")
  h <- p$html
  # neutral slate, not green; tag is not "Adequate"
  expect_match(h, "--accent:#5a6883", fixed = TRUE)
  expect_no_match(h, "class=\"tag\">Adequate", fixed = TRUE)
  # honest caption: credible shortfall, too few corroborators
  expect_match(h, "too few signals corroborate", fixed = TRUE)
})

test_that("a detection is context, never a counted STEPS signal", {
  fg <- synth_field_guide
  foc <- fg$focal
  cand <- foc[foc$verdict == "FLAG" & foc$genomic_orphan %in% TRUE, ]
  skip_if(nrow(cand) == 0)
  d <- cand[["adm2_name"]][1]

  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  # it reads in the context row as a detection box
  expect_match(h, "<div class=\"itile det hit\">", fixed = TRUE)
  expect_match(h, "detection</div>")
  # and adds no STEPS row, so it cannot be counted against the flag rule
  expect_equal(lengths(regmatches(h, gregexpr("class=\"srow\"", h))), 5L)
  expect_no_match(h, "Seasonal &amp; detections", fixed = TRUE)
})

test_that("the pager prescribes no follow-up action", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))

  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  for (phrase in c("Supervisory review", "active case search",
                   "routine monitoring", "collect another year",
                   "no action is warranted", "class=\"va\"")) {
    expect_no_match(h, phrase, fixed = TRUE)
  }
})

test_that("a flag does not claim the interval rules out sampling noise", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))

  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  expect_no_match(h, "unlikely to be noise", fixed = TRUE)
  expect_match(h, "uncertainty in the expected level", fixed = TRUE)
})

test_that("a small-count flag names the sampling-variability caveat", {
  fg <- synth_field_guide
  foc <- fg$focal
  # a flag whose Poisson tail leaves chance a plausible explanation
  cand <- foc[
    foc$verdict == "FLAG" &
      stats::ppois(foc$observed, lambda = foc$expected_total) > 0.05,
  ]
  skip_if(nrow(cand) == 0)
  d <- cand[["adm2_name"]][1]

  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  expect_match(h, "by chance alone", fixed = TRUE)

  # a well-powered flag carries no such caveat
  solid <- foc[
    foc$verdict == "FLAG" &
      stats::ppois(foc$observed, lambda = foc$expected_total) < 0.01,
  ]
  if (nrow(solid) > 0) {
    hs <- bs_field_guide_pager(
      fg, district = solid[["adm2_name"]][1], verbose = FALSE
    )$html
    expect_no_match(hs, "by chance alone", fixed = TRUE)
  }
})

test_that("a zero count is not credited with clearing uncertainty", {
  fg <- synth_field_guide
  foc <- fg$focal
  cand <- foc[foc$observed == 0 & foc$spi_below %in% TRUE, ]
  skip_if(nrow(cand) == 0)
  d <- cand[["adm2_name"]][1]

  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  # the interval collapses to a point mass at zero; never call that a survival
  expect_no_match(h, "wholly below one: the shortfall holds", fixed = TRUE)
  expect_match(h, "the interval carries no evidence", fixed = TRUE)
  expect_match(h, "no cases detected against", fixed = TRUE)
  expect_no_match(h, "about 0 cases", fixed = TRUE)
})

test_that("a small expected count keeps a decimal so it matches the ratio", {
  fg <- synth_field_guide
  foc <- fg$focal
  # a district whose expected count would mislead once rounded to a whole case
  cand <- foc[foc$expected_total > 1 & foc$expected_total < 10, ]
  skip_if(nrow(cand) == 0)
  d <- cand[["adm2_name"]][1]
  exp_dot <- gsub(".", "·", sprintf("%.1f", cand$expected_total[1]),
                  fixed = TRUE)

  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  expect_match(h, paste0("roughly ", exp_dot, " expected"), fixed = TRUE)

  # a large count still reads as whole cases
  big <- foc[foc$expected_total > 100, ]
  if (nrow(big) > 0) {
    hb <- bs_field_guide_pager(
      fg, district = big[["adm2_name"]][1], verbose = FALSE
    )$html
    expect_match(hb, "expected for the district", fixed = TRUE)
    expect_no_match(hb, "·0 expected for", fixed = TRUE)
  }
})

test_that("persistence names the current run, not a historical one", {
  fg <- synth_field_guide
  dy <- fg$district_year
  cut <- fg$thresholds$spi
  # a district whose longest run ended before the read year
  trailing <- vapply(split(dy, dy[[fg$id_col]]), function(g) {
    g <- g[order(g$year), ]
    r <- rle(g$spi_median < cut)
    if (isTRUE(utils::tail(r$values, 1))) utils::tail(r$lengths, 1) else 0L
  }, integer(1))
  foc <- fg$focal
  tr <- trailing[as.character(foc[[fg$id_col]])]
  cand <- foc[!is.na(tr) & foc$longest_run_below > tr & tr > 0, ]
  skip_if(nrow(cand) == 0)
  i <- 1L
  d <- cand[["adm2_name"]][i]
  n_tr <- tr[!is.na(tr) & foc$longest_run_below > tr & tr > 0][i]

  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  # the run ending at the read year is what the sentence leads with
  expect_match(
    h, sprintf("%d consecutive year%s below the adequacy cut to %d",
               n_tr, if (n_tr == 1L) "" else "s", fg$read_year),
    fixed = TRUE
  )
  # and the counted run is disclosed as the panel's longest, with its end year
  expect_match(h, "The counted run is the panel's longest", fixed = TRUE)
})

test_that("the endpoint label clears the locator badge", {
  fg <- synth_field_guide
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  foc <- fg$focal
  # an endpoint high in the panel is the case that used to print under the badge
  cand <- foc[foc$spi_median > 1, ]
  skip_if(nrow(cand) == 0)
  d <- cand[["adm2_name"]][1]

  h <- bs_field_guide_pager(
    fg, district = d, boundaries = synth_surveillance$boundaries,
    id_col = "adm2_guid", verbose = FALSE
  )$html
  expect_match(h, "<div class=\"locbadge\">", fixed = TRUE)
  # the label's y sits below the badge's reach; the dot stays on the endpoint
  lab <- regmatches(
    h, regexpr("<text x=\"[0-9.]+\" y=\"[0-9.]+\" font-family=\"Archivo\"", h)
  )
  y <- as.numeric(sub('.*y="([0-9.]+)".*', "\\1", lab))
  expect_gt(y, 100)
})

test_that("an unsupplied channel reads as unsupplied, not as a finding", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))

  # strip the out-of-grid channels the way bs_field_guide() does when they are
  # not supplied: the flags go off and the year strings go empty
  bare <- fg
  bare$signals_active[c("seasonal", "detect_afp", "detect_es")] <- FALSE
  for (df in c("district_year", "focal")) {
    bare[[df]]$seasonal <- NA_character_
    bare[[df]]$orphan_years <- NA_character_
    bare[[df]]$es_years <- NA_character_
  }

  h <- bs_field_guide_pager(
    bare, district = d, detection_label = "cVDPV2", verbose = FALSE
  )$html
  # an unsupplied channel says so rather than reading as a clean search
  expect_equal(
    lengths(regmatches(h, gregexpr("channel not supplied", h))), 2L
  )
  expect_no_match(h, "no cVDPV2 found", fixed = TRUE)
  # and an unassessed season claims nothing at all
  expect_no_match(h, "through the expected peak", fixed = TRUE)

  # with the channels back on and quiet, it names what was searched
  quiet <- fg
  for (df in c("district_year", "focal")) {
    quiet[[df]]$orphan_years <- NA_character_
    quiet[[df]]$es_years <- NA_character_
  }
  hq <- bs_field_guide_pager(
    quiet, district = d, detection_label = "cVDPV2", verbose = FALSE
  )$html
  expect_equal(
    lengths(regmatches(hq, gregexpr("no cVDPV2 found", hq))), 2L
  )
  expect_no_match(hq, "channel not supplied", fixed = TRUE)
})

test_that("the masthead carries the under-15 denominator", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))
  pop <- fg$focal$pop_u15[fg$focal[["adm2_name"]] == d][1]
  skip_if(is.na(pop))

  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  expect_match(
    h,
    paste0("pop u15 ", formatC(round(pop), format = "d", big.mark = ",")),
    fixed = TRUE
  )

  # a guide without the column simply omits the figure
  bare <- fg
  bare$focal$pop_u15 <- NULL
  bare$district_year$pop_u15 <- NULL
  expect_no_match(
    bs_field_guide_pager(bare, district = d, verbose = FALSE)$html,
    "pop u15 ", fixed = TRUE
  )
})

test_that("note is untagged by default and opt-in when supplied", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))

  bare <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  expect_no_match(bare, "illustrative", fixed = TRUE)

  tagged <- bs_field_guide_pager(
    fg, district = d, note = "illustrative", verbose = FALSE
  )$html
  # eyebrow and footer both carry it
  expect_equal(
    lengths(regmatches(tagged, gregexpr("illustrative", tagged))), 2L
  )
})

test_that("indicators_df is off by default and out of the STEPS grid", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))

  # the context row always renders, since it carries the detection channels;
  # the indicator half is what `indicators_df` switches on
  bare <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  expect_match(bare, "<div class=\"istrip\">", fixed = TRUE)
  expect_no_match(bare, "class=\"ipanel\"", fixed = TRUE)
  expect_no_match(bare, "class=\"itiles\"", fixed = TRUE)

  with_ind <- bs_field_guide_pager(
    fg, district = d, indicators_df = make_indicators(fg), verbose = FALSE
  )$html
  expect_match(with_ind, "class=\"ipanel\"", fixed = TRUE)
  expect_match(with_ind, "class=\"itiles\"", fixed = TRUE)
  # the indicators add no STEPS row and no role chip, so they cannot be
  # miscounted as corroborators
  expect_equal(
    lengths(regmatches(with_ind, gregexpr("class=\"srow\"", with_ind))), 5L
  )
  expect_equal(
    lengths(regmatches(bare, gregexpr("class=\"role ", bare))),
    lengths(regmatches(with_ind, gregexpr("class=\"role ", with_ind)))
  )
})

test_that("a thin pass rate shows the cases behind it and is not graded", {
  fg <- synth_field_guide
  ind <- make_indicators(fg)
  foc <- ind[ind$year == fg$read_year, ]

  # a district whose percentages rest on one or two assessable cases
  thin <- foc[foc$afp_cases > 0 & foc$afp_cases < 5, ]
  skip_if(nrow(thin) == 0)
  h <- bs_field_guide_pager(
    fg, district = thin$name[1], indicators_df = ind, verbose = FALSE
  )$html
  strip <- regmatches(h, regexpr("class=\"istrip\".*", h))
  # the headline stays the pass rate, since that is what the indicator is
  expect_match(strip, "<div class=\"v\">[0-9]+%</div>")
  # with the cases it rests on spelled out beneath it
  n1 <- thin$afp_cases[1]
  expect_match(
    strip, sprintf("of %d case%s", n1, if (n1 == 1L) "" else "s"),
    fixed = TRUE
  )
  # and no grade, which a rate on a handful of cases cannot support
  expect_no_match(strip, "meets 80%", fixed = TRUE)
  expect_no_match(strip, "under 80%", fixed = TRUE)

  # a well-populated district keeps percentages and gets graded
  fat <- foc[foc$afp_cases > 50, ]
  skip_if(nrow(fat) == 0)
  hf <- bs_field_guide_pager(
    fg, district = fat$name[1], indicators_df = ind, verbose = FALSE
  )$html
  expect_match(hf, "%</div>")
  expect_match(hf, "of [0-9]+ cases")
})

test_that("no assessable cases reads as absent, never as zero", {
  fg <- synth_field_guide
  ind <- make_indicators(fg)
  foc <- ind[ind$year == fg$read_year, ]
  none <- foc[foc$afp_cases == 0, ]
  skip_if(nrow(none) == 0)

  h <- bs_field_guide_pager(
    fg, district = none$name[1], indicators_df = ind, verbose = FALSE
  )$html
  expect_match(h, "none assessable", fixed = TRUE)
  strip <- regmatches(h, regexpr("class=\"istrip\".*", h))
  expect_no_match(strip, "<div class=\"v\">0%", fixed = TRUE)
  expect_match(strip, "<div class=\"v\">--</div>", fixed = TRUE)
})

test_that("a district absent from the panel simply omits the strip", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))
  ind <- make_indicators(fg)

  gone <- ind[ind$name != d, ]
  h <- bs_field_guide_pager(
    fg, district = d, indicators_df = gone, verbose = FALSE
  )$html
  expect_no_match(h, "class=\"ipanel\"", fixed = TRUE)
  # the detections still have their say
  expect_match(h, "<div class=\"istrip\">", fixed = TRUE)
  expect_match(h, "ES detection", fixed = TRUE)
  # a panel missing its keys is an error, not a silent omission
  expect_error(
    bs_field_guide_pager(
      fg, district = d, indicators_df = ind[, c("name", "npafp_rate")],
      verbose = FALSE
    ),
    "year"
  )
})

test_that("path writes an auto-named html file", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))

  dir <- withr::local_tempdir()
  p <- bs_field_guide_pager(
    fg, district = d, path = dir, format = "html", verbose = FALSE
  )

  expect_length(p$paths, 1L)
  expect_true(file.exists(p$paths))
  # spi_<adm0>_<adm1>_<adm2>_field_pager.html
  expect_match(basename(p$paths), "^spi_.+_field_pager\\.html$")
})

test_that("an explicit file path is honoured", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))

  f <- withr::local_tempfile(fileext = ".html")
  p <- bs_field_guide_pager(fg, district = d, file = f, verbose = FALSE)
  expect_identical(p$paths, f)
  expect_true(file.exists(f))
})

test_that("an unsupported file extension is rejected", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))

  expect_error(
    bs_field_guide_pager(
      fg, district = d, file = tempfile(fileext = ".pdf"), verbose = FALSE
    ),
    "html"
  )
})

test_that("an unknown district errors with a suggestion", {
  fg <- synth_field_guide
  expect_error(
    bs_field_guide_pager(fg, district = "Nowhere-at-all", verbose = FALSE),
    "no district matches"
  )
})

test_that("the chart plots the focal district alone, with no neighbour lines", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))

  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html

  # no muted neighbour polylines, cluster label or legend key
  expect_no_match(h, "stroke=\"#94a0b3\"", fixed = TRUE)
  expect_no_match(h, "class=\"lk n\"", fixed = TRUE)
  expect_no_match(h, "neighbouring districts", fixed = TRUE)
  # neighbours still reach the page as figures, not lines: the masthead median
  # and the surroundings row
  nb_spi <- fg$focal$neighbour_spi[fg$focal[["adm2_name"]] == d][1]
  if (!is.na(nb_spi)) {
    expect_match(h, "neighbours [0-9]")
    expect_match(h, "against a neighbour median of", fixed = TRUE)
  }
})

test_that("adjacency is inert and says so", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")

  adj <- suppressMessages(
    bs_adjacency(synth_surveillance$boundaries, id_col = "adm2_guid")
  )
  with_adj <- bs_field_guide_pager(
    fg, district = d, adjacency = adj, verbose = FALSE
  )$html
  without <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  # the graph changes nothing on the page
  expect_identical(with_adj, without)
  # and a caller passing one is told it is redundant
  expect_message(
    bs_field_guide_pager(fg, district = d, adjacency = adj, verbose = TRUE),
    "no longer affects the pager"
  )
})

test_that("boundaries locate the district in the whole country", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))
  skip_if_not_installed("sf")
  b <- synth_surveillance$boundaries

  h <- bs_field_guide_pager(
    fg, district = d, boundaries = b, id_col = "adm2_guid", verbose = FALSE
  )$html

  # the inset div, not just the css rule
  expect_match(h, "<div class=\"locbadge\">", fixed = TRUE)
  # the country silhouette plus one shape per admin-1 unit, so the inset spans
  # the country rather than the old handful of touching neighbours
  n_poly <- lengths(regmatches(h, gregexpr("<polygon", h)))
  expect_gt(n_poly, length(unique(b$adm1_name)))
  # the focal district is filled in the verdict accent and ringed
  expect_match(h, "fill=\"#c8102e\"", fixed = TRUE)
  expect_match(h, "stroke-opacity=\"0.7\"", fixed = TRUE)

  # no shapefile -> no inset div
  without <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  expect_no_match(without, "<div class=\"locbadge\">", fixed = TRUE)
})

test_that("a multi-country layer is cut to the focal district's country", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))
  skip_if_not_installed("sf")
  b <- synth_surveillance$boundaries

  # a second country far to the east would otherwise scale the inset to both
  far <- b
  far$adm0_name <- "Elsewhere"
  far$adm2_guid <- paste0("far-", far$adm2_guid)
  sf::st_geometry(far) <- sf::st_geometry(far) + c(60, 0)
  two <- rbind(b, far)

  one_html <- bs_field_guide_pager(
    fg, district = d, boundaries = b, id_col = "adm2_guid", verbose = FALSE
  )$html
  two_html <- bs_field_guide_pager(
    fg, district = d, boundaries = two, id_col = "adm2_guid", verbose = FALSE
  )$html
  inset <- function(h) {
    regmatches(h, regexpr("(?<=lbmap\">).*?</svg>", h, perl = TRUE))
  }
  expect_identical(inset(one_html), inset(two_html))
})

test_that("ES detections from the field guide reach the context row and chart", {
  fg <- synth_field_guide
  foc <- fg$focal
  skip_if(!"es_detected" %in% names(foc))
  # a district the field guide records ES positives for
  d <- foc[foc$es_detected %in% TRUE, ][["adm2_name"]][1]
  skip_if(is.na(d))

  # no `es` argument -- ES is read straight from the field guide
  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  es_box <- regmatches(
    h, regexpr("<div class=\"k\">ES detection.*?</div></div>", h)
  )
  expect_match(es_box, "detection")
  expect_match(es_box, "latest [0-9]{4}")

  # a district with no ES positives says so, rather than dropping the box
  none <- foc[foc$es_detected %in% FALSE, ][["adm2_name"]][1]
  if (!is.na(none)) {
    hn <- bs_field_guide_pager(fg, district = none, verbose = FALSE)$html
    hn_box <- regmatches(
      hn, regexpr("<div class=\"k\">ES detection.*?</div></div>", hn)
    )
    expect_match(hn_box, "none", fixed = TRUE)
  }
})

test_that("a district resolves by id as well as by name", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))
  gid <- fg$focal[["adm2_guid"]][fg$focal[["adm2_name"]] == d][1]

  by_name <- bs_field_guide_pager(fg, district = d, verbose = FALSE)
  by_id <- bs_field_guide_pager(fg, district = gid, verbose = FALSE)
  expect_identical(by_id$id, by_name$id)
})

# reshape the adm2 synth guide into an adm1-only guide: the adm2 units become
# the operational level, keyed by adm1_guid with adm1_name, and every adm2_*
# column dropped -- reproducing province-level (adm1) inputs with no adm2_name.
as_adm1_guide <- function(fg) {
  reshape <- function(df) {
    df[["adm1_guid"]] <- df[["adm2_guid"]]
    df[["adm1_name"]] <- df[["adm2_name"]]
    df[, setdiff(names(df), c("adm2_guid", "adm2_name")), drop = FALSE]
  }
  fg$district_year <- reshape(fg$district_year)
  fg$focal <- reshape(fg$focal)
  fg$id_col <- "adm1_guid"
  fg
}

test_that("adm1-level inputs render without an adm2 column", {
  fg <- as_adm1_guide(synth_field_guide)
  d <- fg$focal[fg$focal$verdict == "FLAG", ][["adm1_name"]][1]
  skip_if(is.na(d))
  skip_if_not_installed("sf")

  b <- synth_surveillance$boundaries
  b[["adm1_guid"]] <- b[["adm2_guid"]]

  # the inset lookup used to hard-code adm2_name
  p <- bs_field_guide_pager(
    fg, district = d, boundaries = b, id_col = "adm1_guid",
    admin_label = "province", verbose = FALSE
  )
  expect_s3_class(p, "blindspot_pager")
  h <- p$html
  expect_match(h, "<!DOCTYPE html>", fixed = TRUE)
  # the unit name still labels the chart endpoint
  expect_match(h, d, fixed = TRUE)
})

test_that("unit_noun rewords the reading; default stays \"district\"", {
  fg <- as_adm1_guide(synth_field_guide)
  d <- fg$focal[fg$focal$verdict == "FLAG", ][["adm1_name"]][1]
  skip_if(is.na(d))

  prov <- bs_field_guide_pager(
    fg, district = d, id_col = "adm1_guid", unit_noun = "province",
    verbose = FALSE
  )$html
  expect_match(prov, "A province is flagged when", fixed = TRUE)
  expect_match(prov, "expected for the province", fixed = TRUE)
  expect_no_match(prov, "A district is flagged when", fixed = TRUE)

  # default is unchanged: adm2 output still reads "district"
  d2 <- flag_district(synth_field_guide)
  h2 <- bs_field_guide_pager(
    synth_field_guide, district = d2, verbose = FALSE
  )$html
  expect_match(h2, "A district is flagged when", fixed = TRUE)
})

test_that("adm1 auto-name keeps each admin level once", {
  fg <- as_adm1_guide(synth_field_guide)
  d <- fg$focal[fg$focal$verdict == "FLAG", ][["adm1_name"]][1]
  skip_if(is.na(d))

  dir <- withr::local_tempdir()
  p <- bs_field_guide_pager(
    fg, district = d, id_col = "adm1_guid", path = dir,
    format = "html", verbose = FALSE
  )
  slug <- gsub("^-+|-+$", "", gsub("[^a-z0-9]+", "-", tolower(d)))
  # adm1_name is both the unit and its own parent level; it must not repeat
  base <- basename(p$paths)
  expect_equal(lengths(regmatches(base, gregexpr(slug, base, fixed = TRUE))), 1L)
})

test_that("no serotype is named unless the caller names one", {
  fg <- synth_field_guide
  foc <- fg$focal
  d <- foc[foc$genomic_orphan %in% TRUE | foc$es_detected %in% TRUE, ]
  skip_if(nrow(d) == 0)
  d <- d[["adm2_name"]][1]

  # the guide records detection years but not serotypes, so the default page
  # must not assert one
  bare <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  expect_match(bare, "poliovirus", fixed = TRUE)
  expect_no_match(bare, "cVDPV", fixed = TRUE)
  expect_no_match(bare, "WPV", fixed = TRUE)

  # and it uses whatever the caller declares they filtered down to
  named <- bs_field_guide_pager(
    fg, district = d, detection_label = "cVDPV1", verbose = FALSE
  )$html
  expect_match(named, "cVDPV1", fixed = TRUE)
  expect_no_match(named, "poliovirus", fixed = TRUE)

  # an empty declaration is treated as no declaration
  blank <- bs_field_guide_pager(
    fg, district = d, detection_label = "", verbose = FALSE
  )$html
  expect_match(blank, "poliovirus", fixed = TRUE)
})

test_that("detection_label controls the serotype wording", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))

  h <- bs_field_guide_pager(
    fg, district = d, es = synth_surveillance$es_district_year,
    es_col = "n_positive", id_col = "adm2_guid",
    detection_label = "WPV1", verbose = FALSE
  )$html
  gid <- fg$focal[["adm2_guid"]][fg$focal[["adm2_name"]] == d][1]
  es <- synth_surveillance$es_district_year
  skip_if_not(any(es$adm2_guid == gid & es$n_positive > 0))
  expect_match(h, "WPV1 detected", fixed = TRUE)
})
