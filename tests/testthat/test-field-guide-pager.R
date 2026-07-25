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
  # five STEPS rows + one out-of-grid detection row, one legend, one banner
  expect_equal(lengths(regmatches(html, gregexpr("class=\"srow\"", html))), 6L)
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

test_that("a detection without seasonal blindness reads as external, not counted", {
  fg <- synth_field_guide
  foc <- fg$focal
  # a flag with detections but seasonal not "blind"
  cand <- foc[
    foc$verdict == "FLAG" & foc$genomic_orphan %in% TRUE &
      (is.na(foc$seasonal) | foc$seasonal != "blind"),
  ]
  skip_if(nrow(cand) == 0)
  d <- cand[["adm2_name"]][1]

  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  det <- regmatches(
    h, regexpr("Seasonal &amp; detections.*?</span>", h)
  )
  # the out-of-grid detection chip is "external", not the counted "corroborates"
  expect_match(det, "role external", fixed = TRUE)
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

  h <- bs_field_guide_pager(bare, district = d, verbose = FALSE)$html
  expect_match(h, "no monthly SPI supplied", fixed = TRUE)
  expect_match(h, "no detection channel supplied", fixed = TRUE)
  # neither an absent season nor an absent channel reads as a finding
  expect_no_match(h, "no cVDPV2 detected in", fixed = TRUE)
  expect_no_match(h, "no poliovirus detected there", fixed = TRUE)

  # with the channels back on and quiet, it names what was searched
  quiet <- fg
  for (df in c("district_year", "focal")) {
    quiet[[df]]$orphan_years <- NA_character_
    quiet[[df]]$es_years <- NA_character_
  }
  hq <- bs_field_guide_pager(quiet, district = d, verbose = FALSE)$html
  expect_match(hq, "no cVDPV2 detected in AFP or ES", fixed = TRUE)
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
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")

  adj <- suppressMessages(
    bs_adjacency(synth_surveillance$boundaries, id_col = "adm2_guid")
  )
  with_adj <- bs_field_guide_pager(
    fg, district = d, adjacency = adj, verbose = FALSE
  )$html
  without_adj <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html

  nb_spi <- fg$focal$neighbour_spi[fg$focal[["adm2_name"]] == d][1]
  for (h in list(with_adj, without_adj)) {
    # no muted neighbour polylines, cluster label or legend key
    expect_no_match(h, "stroke=\"#94a0b3\"", fixed = TRUE)
    expect_no_match(h, "class=\"lk n\"", fixed = TRUE)
    expect_no_match(h, "neighbouring districts", fixed = TRUE)
    # neighbours still reach the page as figures, not lines: the masthead
    # median and the surroundings row
    if (!is.na(nb_spi)) {
      expect_match(h, "neighbours [0-9]")
      expect_match(h, "against a neighbour median of", fixed = TRUE)
    }
  }
})

test_that("boundaries build the graph and draw a locator inset", {
  fg <- synth_field_guide
  d <- flag_district(fg)
  skip_if(is.na(d))
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")

  p <- bs_field_guide_pager(
    fg, district = d, boundaries = synth_surveillance$boundaries,
    id_col = "adm2_guid", verbose = FALSE
  )
  h <- p$html
  # locator inset drawn from real geometry (the div, not just the css rule)
  expect_match(h, "<div class=\"locbadge\">", fixed = TRUE)
  expect_gt(lengths(regmatches(h, gregexpr("<polygon", h))), 1L)

  # no shapefile -> no inset div
  without <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  expect_no_match(without, "<div class=\"locbadge\">", fixed = TRUE)
})

test_that("ES detections from the field guide enter the detection row and chart", {
  fg <- synth_field_guide
  foc <- fg$focal
  skip_if(!"es_detected" %in% names(foc))
  # a district the field guide records ES positives for
  d <- foc[foc$es_detected %in% TRUE, ][["adm2_name"]][1]
  skip_if(is.na(d))

  # no `es` argument -- ES is read straight from the field guide
  h <- bs_field_guide_pager(fg, district = d, verbose = FALSE)$html
  expect_match(h, "ES detection", fixed = TRUE)
  expect_match(h, "in ES \\(")

  # a district with no ES positives shows no ES entry
  none <- foc[foc$es_detected %in% FALSE, ][["adm2_name"]][1]
  if (!is.na(none)) {
    hn <- bs_field_guide_pager(fg, district = none, verbose = FALSE)$html
    expect_no_match(hn, "ES detection", fixed = TRUE)
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
  skip_if_not_installed("spdep")

  b <- synth_surveillance$boundaries
  b[["adm1_guid"]] <- b[["adm2_guid"]]
  adj <- suppressMessages(bs_adjacency(b, id_col = "adm1_guid"))

  # the adjacency branch of the neighbour lookup used to hard-code adm2_name
  p <- bs_field_guide_pager(
    fg, district = d, adjacency = adj, id_col = "adm1_guid",
    admin_label = "province", verbose = FALSE
  )
  expect_s3_class(p, "blindspot_pager")
  h <- p$html
  expect_match(h, "<!DOCTYPE html>", fixed = TRUE)
  # the unit name still labels the chart endpoint
  expect_match(h, d, fixed = TRUE)
})

test_that("unit_noun rewords the reading; default stays \"district\"", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  fg <- as_adm1_guide(synth_field_guide)
  d <- fg$focal[fg$focal$verdict == "FLAG", ][["adm1_name"]][1]
  skip_if(is.na(d))

  b <- synth_surveillance$boundaries
  b[["adm1_guid"]] <- b[["adm2_guid"]]
  adj <- suppressMessages(bs_adjacency(b, id_col = "adm1_guid"))

  prov <- bs_field_guide_pager(
    fg, district = d, adjacency = adj, id_col = "adm1_guid",
    unit_noun = "province", verbose = FALSE
  )$html
  expect_match(prov, "A province is flagged when", fixed = TRUE)
  expect_match(prov, "expected for the province", fixed = TRUE)
  expect_no_match(prov, "A district is flagged when", fixed = TRUE)

  # default is unchanged: adm2 output still reads "district"
  d2 <- flag_district(synth_field_guide)
  adj2 <- suppressMessages(
    bs_adjacency(synth_surveillance$boundaries, id_col = "adm2_guid")
  )
  h2 <- bs_field_guide_pager(
    synth_field_guide, district = d2, adjacency = adj2, verbose = FALSE
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
