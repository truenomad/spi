# The heavy upstream chain (bs_expected -> INLA) is exercised once in
# data-raw/synth_field_guide.R; the shipped `synth_field_guide` lets these
# tests run without INLA. Only the recompute test gates on INLA.

test_that("synth_field_guide has the expected structure", {
  data("synth_field_guide", package = "blindspot")

  expect_s3_class(synth_field_guide, "blindspot_field_guide")
  expect_named(
    synth_field_guide,
    c("district_year", "focal", "reference", "read_year", "thresholds",
      "params", "signals_active", "id_col", "call")
  )
  dy <- synth_field_guide$district_year
  expect_true(all(
    c("verdict", "corroborators", "s1_discordance", "longest_run_below",
      "trajectory", "neighbour_spi", "neighbour_discordant", "seasonal",
      "genomic_orphan") %in% names(dy)
  ))
  expect_s3_class(dy$verdict, "factor")
  expect_setequal(levels(dy$verdict), c("FLAG", "WATCH", "No action"))
  # all seven signals active in the shipped object
  expect_true(all(synth_field_guide$signals_active))
  # focal is the read-year slice, one row per district
  expect_equal(nrow(synth_field_guide$focal), 100L)
  expect_true(all(synth_field_guide$focal$year == synth_field_guide$read_year))
})

test_that("the flag rule matches the verdict column", {
  fg <- synth_field_guide
  dy <- fg$district_year
  cut <- fg$thresholds$spi
  minc <- fg$params$min_corroborators

  expect_flag <- dy$spi_median < cut & dy$spi_q95 < 1 &
    dy$corroborators >= minc
  expect_watch <- dy$spi_median < cut & !(dy$spi_q95 < 1)

  expect_equal(as.character(dy$verdict) == "FLAG", expect_flag)
  expect_equal(as.character(dy$verdict) == "WATCH", expect_watch)
})

test_that("verdicts rank by severity and honour the flag conditions", {
  fg <- synth_field_guide
  dy <- fg$district_year

  # flags are the deeper shortfalls: median SPI ordered FLAG < No action
  med <- tapply(dy$spi_median, as.character(dy$verdict), stats::median)
  expect_lt(med[["FLAG"]], med[["No action"]])

  # every FLAG satisfies all three conditions; every WATCH is the credible-1 case
  flagged <- dy[dy$verdict == "FLAG", ]
  expect_true(all(flagged$spi_median < fg$thresholds$spi))
  expect_true(all(flagged$spi_q95 < 1))
  expect_true(all(flagged$corroborators >= fg$params$min_corroborators))

  watched <- dy[dy$verdict == "WATCH", ]
  expect_true(all(watched$spi_median < fg$thresholds$spi))
  expect_true(all(watched$spi_q95 >= 1))
})

test_that("graceful degradation without optional inputs", {
  fg <- synth_field_guide
  # rebuild a minimal concordance-like object from the shipped guide so we can
  # call bs_field_guide() with no adjacency / spi_month / genomic
  conc <- structure(
    list(
      district_year = fg$district_year[, c(
        "adm2_guid", "year", "observed", "expected_total", "spi_median",
        "spi_q05", "spi_q95", "npafp_rate", "npafp_adequate"
      )],
      thresholds = list(spi = fg$thresholds$spi, npafp = fg$thresholds$npafp),
      id_col = "adm2_guid"
    ),
    class = "blindspot_concordance"
  )

  bare <- bs_field_guide(conc, verbose = FALSE)
  expect_false(any(bare$signals_active))
  expect_true(all(is.na(bare$district_year$neighbour_spi)))
  expect_true(all(is.na(bare$district_year$seasonal)))
  # no WATCH/FLAG relies on S6/S7 here; verdicts still computable from S1/S4/S5
  expect_s3_class(bare$district_year$verdict, "factor")
  # corroborators only ever from trajectory + persistence now (max 2)
  expect_lte(max(bare$district_year$corroborators), 2L)
})

test_that("bs_field_guide_table renders both layouts on both engines", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  fg <- synth_field_guide

  expect_s3_class(
    bs_field_guide_table(fg, engine = "gt", layout = "scan"), "gt_tbl"
  )
  expect_s3_class(
    bs_field_guide_table(fg, engine = "gt", layout = "worked"), "gt_tbl"
  )
  expect_s3_class(
    bs_field_guide_table(fg, engine = "flextable", layout = "scan"),
    "flextable"
  )
  expect_s3_class(
    bs_field_guide_table(fg, engine = "flextable", layout = "worked"),
    "flextable"
  )
})

test_that("bs_field_guide_table saves to file by extension", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  fg <- synth_field_guide

  f_html <- withr::local_tempfile(fileext = ".html")
  bs_field_guide_table(fg, engine = "gt", layout = "scan", file = f_html)
  expect_true(file.exists(f_html) && file.info(f_html)$size > 0)

  f_docx <- withr::local_tempfile(fileext = ".docx")
  bs_field_guide_table(fg, engine = "flextable", layout = "worked",
                       file = f_docx)
  expect_true(file.exists(f_docx) && file.info(f_docx)$size > 0)
})

test_that("worked-example selection honours explicit districts", {
  fg <- synth_field_guide
  ids <- fg$focal$adm2_guid[1:3]
  sel <- .fg_select_worked(fg$focal, fg$id_col, fg$thresholds$spi,
                           districts = ids)
  expect_equal(sel$adm2_guid, ids)
})

test_that("bs_field_guide_help runs each topic", {
  fg <- synth_field_guide
  expect_no_error(bs_field_guide_help("signals"))
  expect_no_error(bs_field_guide_help("verdict"))
  expect_no_error(bs_field_guide_help("misreadings"))
  w <- bs_field_guide_help("example", guide = fg)
  expect_s3_class(w, "tbl_df")
  expect_true("case_label" %in% names(w))
})

test_that("print / summary / as_tibble methods work", {
  fg <- synth_field_guide
  expect_no_error(print(fg))
  expect_no_error(summary(fg))
  expect_identical(print(fg), fg) # returns invisibly
  expect_s3_class(as_tibble(fg), "tbl_df")
  expect_equal(nrow(as_tibble(fg)), nrow(fg$district_year))
})

test_that("bs_field_guide can be recomputed end to end", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  data("synth_surveillance", package = "blindspot")
  s <- synth_surveillance

  adj <- bs_adjacency(s$boundaries, id_col = "adm2_guid")
  fit <- fit_or_skip(
    s$cases, s$population, adj,
    id_col = "adm2_guid", season = "harmonic", year_effect = "iid",
    overdispersion = "iid", n_draws = 200L, seed = 42L, verbose = FALSE
  )
  cy <- bs_spi(fit, level = "district_year", verbose = FALSE)
  cm <- bs_spi(fit, level = "district_month", verbose = FALSE)
  conc <- bs_concordance(
    cy, s$cases, s$population, boundaries = s$boundaries, verbose = FALSE
  )
  fg <- bs_field_guide(
    conc, adjacency = adj, spi_month = cm,
    genomic = dplyr::filter(s$virus_outcome, any_cvdpv2 == 1)[, c("adm2_guid", "year")],
    verbose = FALSE
  )
  expect_s3_class(fg, "blindspot_field_guide")
  expect_true(all(fg$signals_active))
})
