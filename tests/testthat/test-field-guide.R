# The heavy upstream chain (spi_expected -> INLA) is exercised once in
# data-raw/synth_field_guide.R; the shipped `synth_field_guide` lets these
# tests run without INLA. Only the recompute test gates on INLA.

test_that("synth_field_guide has the expected structure", {
  data("synth_field_guide", package = "spi")

  expect_s3_class(synth_field_guide, "spi_field_guide")
  expect_named(
    synth_field_guide,
    c("district_year", "focal", "reference", "read_year", "thresholds",
      "params", "signals_active", "id_col", "call")
  )
  dy <- synth_field_guide$district_year
  # the five STEPS components, plus the context signals kept beside them
  expect_true(all(
    c("verdict", "gate_pass", "pct_transport_timely", "timeliness_concern",
      "extent_pct", "national_pct", "extent_concern", "spi_previous",
      "persistence_concern", "pct_adequate", "adequacy_concern",
      "longest_run_below", "trajectory", "neighbour_spi", "seasonal",
      "genomic_orphan") %in% names(dy)
  ))
  expect_false(any(c("corroborators", "run_below") %in% names(dy)))
  expect_s3_class(dy$verdict, "factor")
  expect_setequal(
    levels(dy$verdict),
    c("Review priority", "Monitor", "No SPI indication")
  )
  # the interval and the gate it feeds are recorded apart
  expect_true(all(c("cri_excludes_1", "gate_pass") %in% names(dy)))
  # every component and context signal active in the shipped object
  expect_true(all(synth_field_guide$signals_active))
  # STEPS is applied below 1, as in the field guide
  expect_equal(synth_field_guide$thresholds$spi, 1)
  expect_equal(synth_field_guide$reference$signal, c(
    "S: Strength", "T: Timeliness", "E: Extent", "P: Persistence",
    "S: Stool adequacy"
  ))
  # focal is the read-year slice, one row per district
  expect_equal(
    nrow(synth_field_guide$focal),
    dplyr::n_distinct(synth_field_guide$district_year$adm2_guid)
  )
  expect_true(all(synth_field_guide$focal$year == synth_field_guide$read_year))
})

test_that("the review rule matches the verdict column", {
  fg <- synth_field_guide
  dy <- fg$district_year
  cut <- fg$thresholds$spi
  is_true <- function(x) !is.na(x) & x

  corroborated <- is_true(dy$extent_concern) | is_true(dy$persistence_concern)
  expect_priority <- dy$spi_median < cut & dy$spi_q95 < 1 & corroborated
  expect_monitor <- dy$spi_median < cut & !expect_priority

  expect_equal(as.character(dy$verdict) == "Review priority", expect_priority)
  expect_equal(as.character(dy$verdict) == "Monitor", expect_monitor)
  expect_equal(
    as.character(dy$verdict) == "No SPI indication", !(dy$spi_median < cut)
  )
})

test_that("the interval rule relabels shortfalls whose interval includes 1", {
  conc <- make_concordance()
  fg_med <- spi_field_guide(conc, verbose = FALSE)
  fg_int <- spi_field_guide(conc, spi_rule = "interval", verbose = FALSE)
  med <- as.character(fg_med$district_year$verdict)
  int <- as.character(fg_int$district_year$verdict)
  dy <- fg_int$district_year
  uncertain <- dy$spi_below & dy$spi_q95 >= 1

  expect_true(any(uncertain & med == "Monitor"))
  expect_true(all(int[uncertain] == "No SPI indication"))
  expect_equal(int[!uncertain], med[!uncertain])
  expect_identical(fg_med$thresholds$rule, "median")
  expect_identical(fg_int$thresholds$rule, "interval")

  # the rule given to spi_concordance() carries through by default
  conc$thresholds$rule <- "interval"
  expect_equal(
    spi_field_guide(conc, verbose = FALSE)$district_year$verdict,
    fg_int$district_year$verdict
  )
  expect_error(spi_field_guide(conc, spi_rule = "mean", verbose = FALSE))
})

test_that("timeliness and stool adequacy never move the judgement", {
  fg <- synth_field_guide
  dy <- fg$district_year
  conc <- structure(
    list(
      district_year = dy[, c(
        "adm2_guid", "adm1_name", "year", "observed", "expected_total",
        "spi_median", "spi_q05", "spi_q95", "npafp_rate", "npafp_adequate"
      )],
      thresholds = list(spi = 0.8, npafp = fg$thresholds$npafp),
      id_col = "adm2_guid"
    ),
    class = "spi_concordance"
  )
  with_process <- spi_field_guide(
    conc, process = synth_surveillance$afp_process, verbose = FALSE
  )
  without <- spi_field_guide(conc, verbose = FALSE)
  expect_identical(with_process$district_year$verdict,
                   without$district_year$verdict)
  expect_true(all(is.na(without$district_year$timeliness_concern)))
  expect_true(any(with_process$district_year$timeliness_concern %in% TRUE))
})

test_that("extent compares the area share with the national share", {
  fg <- synth_field_guide
  foc <- fg$focal
  below <- foc$spi_median < fg$thresholds$spi
  expect_equal(unique(foc$national_pct), 100 * mean(below))

  i <- which(foc$extent_others > 0)[1]
  same <- foc$adm1_name == foc$adm1_name[i] & seq_len(nrow(foc)) != i
  expect_equal(foc$extent_others[i], sum(same))
  expect_equal(foc$extent_others_below[i], sum(below[same]))
  expect_equal(foc$extent_concern[i],
               foc$extent_pct[i] > foc$national_pct[i])
})

test_that("persistence reads the previous year's SPI", {
  fg <- synth_field_guide
  dy <- fg$district_year
  d <- fg$focal$adm2_guid[1]
  prev <- dy$spi_median[dy$adm2_guid == d & dy$year == fg$read_year - 1L]
  expect_equal(fg$focal$spi_previous[1], prev)
  expect_equal(fg$focal$persistence_concern[1], prev < fg$thresholds$spi)
  first <- dy[dy$year == min(dy$year), ]
  expect_true(all(is.na(first$persistence_concern)))
})

test_that("graceful degradation without optional inputs", {
  fg <- synth_field_guide
  # rebuild a minimal concordance-like object from the shipped guide so we can
  # call spi_field_guide() with no process / admin-1 / adjacency / spi_month
  conc <- structure(
    list(
      district_year = fg$district_year[, c(
        "adm2_guid", "year", "observed", "expected_total", "spi_median",
        "spi_q05", "spi_q95", "npafp_rate", "npafp_adequate"
      )],
      thresholds = list(spi = fg$thresholds$spi, npafp = fg$thresholds$npafp),
      id_col = "adm2_guid"
    ),
    class = "spi_concordance"
  )

  bare <- spi_field_guide(conc, verbose = FALSE)
  expect_false(any(bare$signals_active))
  expect_true(all(is.na(bare$district_year$extent_concern)))
  expect_true(all(is.na(bare$district_year$pct_adequate)))
  expect_true(all(is.na(bare$district_year$neighbour_spi)))
  expect_true(all(is.na(bare$district_year$seasonal)))
  # without extent, only persistence can lift a certain shortfall
  pri <- bare$district_year[bare$district_year$verdict == "Review priority", ]
  expect_true(all(pri$persistence_concern))
})

test_that("spi_field_guide_table renders both layouts on both engines", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  fg <- synth_field_guide

  expect_s3_class(
    spi_field_guide_table(fg, engine = "gt", layout = "scan"), "gt_tbl"
  )
  expect_s3_class(
    spi_field_guide_table(fg, engine = "gt", layout = "worked"), "gt_tbl"
  )
  expect_s3_class(
    spi_field_guide_table(fg, engine = "flextable", layout = "scan"),
    "flextable"
  )
  expect_s3_class(
    spi_field_guide_table(fg, engine = "flextable", layout = "worked"),
    "flextable"
  )
})

test_that("spi_field_guide_table saves to file by extension", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  fg <- synth_field_guide

  f_html <- withr::local_tempfile(fileext = ".html")
  spi_field_guide_table(fg, engine = "gt", layout = "scan", file = f_html)
  expect_true(file.exists(f_html) && file.info(f_html)$size > 0)

  f_docx <- withr::local_tempfile(fileext = ".docx")
  spi_field_guide_table(fg, engine = "flextable", layout = "worked",
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

test_that("spi_field_guide_help runs each topic", {
  fg <- synth_field_guide
  expect_no_error(spi_field_guide_help("signals"))
  expect_no_error(spi_field_guide_help("verdict"))
  expect_no_error(spi_field_guide_help("misreadings"))
  w <- spi_field_guide_help("example", guide = fg)
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

test_that("spi_field_guide can be recomputed end to end", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  data("synth_surveillance", package = "spi")
  s <- synth_surveillance

  adj <- spi_adjacency(s$boundaries, id_col = "adm2_guid")
  fit <- fit_or_skip(
    s$cases, s$population, adj,
    id_col = "adm2_guid", season = "harmonic", year_effect = "iid",
    overdispersion = "iid", n_draws = 200L, seed = 42L, verbose = FALSE
  )
  cy <- spi_index(fit, level = "district_year", verbose = FALSE)
  cm <- spi_index(fit, level = "district_month", verbose = FALSE)
  conc <- spi_concordance(
    cy, s$cases, s$population, boundaries = s$boundaries, verbose = FALSE
  )
  fg <- spi_field_guide(
    conc, process = s$afp_process, adjacency = adj, spi_month = cm,
    detections = dplyr::filter(s$virus_outcome, any_cvdpv2 == 1)[, c("adm2_guid", "year")],
    es = s$es_district_year, es_col = "n_positive",
    verbose = FALSE
  )
  expect_s3_class(fg, "spi_field_guide")
  # every input supplied -> every component and context signal computable
  expect_true(all(fg$signals_active))
})
