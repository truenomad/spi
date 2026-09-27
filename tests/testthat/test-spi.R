# spi_index() and its methods run entirely off a constructed spi_expected
# fixture (make_expected(), see helper-fixtures.R) -- no INLA fit required.

test_that("spi_index computes every aggregation level", {
  fit <- make_expected()

  dm <- spi_index(fit, level = "district_month", verbose = FALSE)
  dq <- spi_index(fit, level = "district_quarter", verbose = FALSE)
  dy <- spi_index(fit, level = "district_year", verbose = FALSE)
  dt <- spi_index(fit, level = "district_total", verbose = FALSE)

  expect_s3_class(dy, "spi_index")
  # one row per district at the total level; per district-month at the finest
  expect_equal(nrow(dt$summary), 24L)
  expect_equal(nrow(dm$summary), nrow(fit$data))
  expect_equal(nrow(dy$summary), 24L * 2L)      # 24 districts x 2 years
  expect_equal(nrow(dq$summary), 24L * 8L)      # 24 districts x 8 quarters

  # draws matrix columns line up with the summary rows
  expect_equal(ncol(dy$draws), nrow(dy$summary))
  # SPI summary carries the full quantile set
  expect_true(all(c("spi_median", "spi_mean", "spi_q05", "spi_q10",
                    "spi_q90", "spi_q95", "expected_total") %in%
                    names(dy$summary)))
  # totals reconcile with the raw observed counts
  expect_equal(dt$totals$total_observed, sum(fit$data$count))
  expect_equal(dy$level, "district_year")
})

test_that("year_end_month rolls the reading year and marks partial windows", {
  # the fixture runs Jan 2015 to Dec 2016: two whole calendar years
  fit <- make_expected()

  cal <- spi_index(fit, level = "district_year", verbose = FALSE)
  rol <- spi_index(fit, level = "district_year", year_end_month = 4,
                verbose = FALSE)

  # closing in April cuts three windows out of the same 24 months -- Jan-Apr
  # 2015, May 2015-Apr 2016, May-Dec 2016 -- each labelled by the year it
  # closes in, so the series starts and ends on a partial one
  expect_equal(nrow(cal$summary), 24L * 2L)
  expect_equal(nrow(rol$summary), 24L * 3L)
  expect_equal(sort(unique(rol$summary$year)), c(2015, 2016, 2017))
  expect_equal(unique(cal$summary$n_months), 12L)
  expect_equal(
    vapply(split(rol$summary$n_months, rol$summary$year), unique, integer(1)),
    c("2015" = 4L, "2016" = 12L, "2017" = 8L)
  )

  # only the grouping moves: no observation is gained or lost
  expect_equal(sum(rol$summary$observed), sum(cal$summary$observed))

  # 12 is the default and means calendar years
  expect_equal(
    spi_index(fit, level = "district_year", year_end_month = 12,
           verbose = FALSE)$summary$observed,
    cal$summary$observed
  )

  # ignored with a note at every other level, and rejected if not a month
  expect_message(
    spi_index(
      fit, level = "district_total", year_end_month = 4, verbose = FALSE
    ),
    "district_year"
  )
  expect_error(
    spi_index(
      fit, level = "district_year", year_end_month = 13, verbose = FALSE
    ),
    "year_end_month"
  )
  expect_error(
    spi_index(
      fit, level = "district_year", year_end_month = NA, verbose = FALSE
    ),
    "year_end_month"
  )
})

test_that("spi_index attaches admin names just before the id column", {
  fit <- make_expected()
  id <- fit$id_col
  ids <- sort(unique(fit$data[[id]]))
  labels <- data.frame(
    ids,
    adm1_name = paste0("Prov-", substr(ids, 2, 3)),
    adm2_name = paste0("Dist-", ids),
    stringsAsFactors = FALSE
  )
  names(labels)[1] <- id

  s <- spi_index(fit, level = "district_year", boundaries = labels,
              verbose = FALSE)
  nm <- names(s$summary)
  gi <- match(id, nm)
  # the two names sit immediately before the id column, in adm1 -> adm2 order
  expect_equal(nm[gi - 2L], "adm1_name")
  expect_equal(nm[gi - 1L], "adm2_name")
  # names carry a value, not NA, for a known district
  expect_true(all(!is.na(s$summary$adm2_name)))
  # the low-information slice (derived from summary) carries them too
  expect_true(all(c("adm1_name", "adm2_name") %in% names(s$low_information)))

  # default (no boundaries) is unchanged
  s0 <- spi_index(fit, level = "district_year", verbose = FALSE)
  expect_false("adm2_name" %in% names(s0$summary))
})

test_that("spi_index honours an override cases table and imputes gaps to 0", {
  fit <- make_expected()
  id <- fit$id_col
  # supply counts for only part of the panel; the rest join to NA -> 0
  partial <- fit$data[1:200, c(id, "month", "count")]
  partial$count <- partial$count + 1L

  s <- spi_index(
    fit, cases = partial, level = "district_month", verbose = FALSE
  )
  expect_s3_class(s, "spi_index")
  # rows outside the supplied cases were imputed to 0 observed
  expect_equal(nrow(s$summary), nrow(fit$data))
  expect_true(s$totals$total_observed < sum(fit$data$count + 1L))
})

test_that("spi_index flags low-information groups", {
  fit <- make_expected()
  # a high threshold pushes many district-months under the expected floor
  s <- spi_index(fit, level = "district_month", min_expected = 5,
              verbose = FALSE)
  expect_gt(nrow(s$low_information), 0L)
  expect_equal(s$totals$n_low_information, nrow(s$low_information))
  expect_true(all(s$low_information$expected_total < 5))

  # none flagged when the floor is 0
  s0 <- spi_index(
    fit, level = "district_year", min_expected = 0, verbose = FALSE
  )
  expect_equal(nrow(s0$low_information), 0L)
})

test_that("spi_index verbose path runs (progress + low-info warning)", {
  fit <- make_expected()
  expect_no_error(
    suppressMessages(
      spi_index(fit, level = "district_month", min_expected = 5, verbose = TRUE)
    )
  )
})

test_that("spi_index validates its inputs", {
  fit <- make_expected()

  expect_error(spi_index(list()), "spi_expected|inherits")
  expect_error(spi_index(fit, min_expected = -1), "min_expected")
  expect_error(spi_index(fit, min_expected = "x"))
  expect_error(spi_index(fit, level = "nonsense"))

  # draws stripped out -> cannot compute SPI
  no_draws <- fit
  no_draws$draws <- NULL
  expect_error(spi_index(no_draws, verbose = FALSE), "draws")

  # cases without the id column
  bad_cases <- tibble::tibble(month = fit$data$month, count = fit$data$count)
  expect_error(spi_index(fit, cases = bad_cases, verbose = FALSE), "id column")
})

test_that("spi_index print / summary / coercion methods work", {
  fit <- make_expected()
  s <- spi_index(fit, level = "district_year", verbose = FALSE)

  expect_no_error(print(s))
  expect_identical(print(s), s)   # invisible return
  diag <- summary(s)
  expect_s3_class(diag, "tbl_df")
  expect_true(all(c("metric", "value", "flag") %in% names(diag)))
  expect_s3_class(as_tibble(s), "tbl_df")
  expect_equal(as_tibble(s), s$summary)
  expect_s3_class(as.data.frame(s), "data.frame")
})

test_that("spi_index print warns on low-information groups", {
  fit <- make_expected()
  s <- spi_index(
    fit, level = "district_month", min_expected = 5, verbose = FALSE
  )
  expect_gt(s$totals$n_low_information, 0L)
  expect_no_error(print(s))          # exercises the low-information warning
})

test_that("spi_index plots render for every type", {
  skip_if_not_installed("ggplot2")
  fit <- make_expected()
  dy <- spi_index(fit, level = "district_year", verbose = FALSE)

  expect_s3_class(plot(dy, type = "distribution"), "ggplot")
  expect_s3_class(plot(dy, type = "funnel"), "ggplot")
  expect_s3_class(plot(dy, type = "caterpillar"), "ggplot")
  expect_s3_class(plot(dy, type = "calibration"), "ggplot")

  # funnel without a population column falls back to the single-colour branch
  no_pop <- dy
  no_pop$summary$pop_u15 <- NULL
  expect_s3_class(plot(no_pop, type = "funnel"), "ggplot")

  # year filter path (summary carries a year column at district_year level)
  expect_s3_class(plot(dy, type = "distribution", year = 2015), "ggplot")

  # district-month has SPI = 0 groups -> the "dropped" note branch fires
  dm <- spi_index(fit, level = "district_month", verbose = FALSE)
  expect_s3_class(plot(dm, type = "distribution"), "ggplot")
})

test_that("SPI footnote + flag helpers cover every branch", {
  # .flag_within
  expect_equal(spi:::.flag_within(1.0, 0.9, 1.1), "pass")
  expect_equal(spi:::.flag_within(2.0, 0.9, 1.1), "flag")
  expect_true(is.na(spi:::.flag_within(NA_real_, 0.9, 1.1)))

  # .note_median: pass (under) and flag (over)
  expect_match(spi:::.note_median(0.95, "pass"), "underdetection")
  expect_match(spi:::.note_median(1.4, "flag"), "overdetection")

  # .note_ratio: pass and flag (below / above)
  expect_match(spi:::.note_ratio(0.97, "pass"), "pass")
  expect_match(spi:::.note_ratio(1.5, "flag"), "above")
  expect_match(spi:::.note_ratio(0.5, "flag"), "below")

  # .note_skew: pass, left tail (neg), right tail (pos)
  expect_match(spi:::.note_skew(0.1, "pass"), "symmetric")
  expect_match(spi:::.note_skew(-1.0, "flag"), "Left tail")
  expect_match(spi:::.note_skew(1.0, "flag"), "Right tail")

  # .note_cri: pass, over-smoothed (<5%), under-smoothed (>50%)
  expect_match(spi:::.note_cri(0.2, "pass"), "pass")
  expect_match(spi:::.note_cri(0.02, "flag"), "over-smoothed")
  expect_match(spi:::.note_cri(0.8, "flag"), "under-smoothed")
})

test_that("national centring divides by the period's summed O/E", {
  fit <- make_expected(id_col = "adm2_guid")
  raw <- spi_index(
    fit, level = "district_year", centre = "none", verbose = FALSE
  )
  cen <- spi_index(fit, level = "district_year", verbose = FALSE)
  expect_identical(cen$centre, "national")
  nat <- raw$summary |>
    dplyr::summarise(oe = sum(observed) / sum(expected_total), .by = "year")
  expect_equal(cen$national$national_oe, nat$oe)
  ratio <- nat$oe[match(cen$summary$year, nat$year)]
  for (col in c("spi_median", "spi_q05", "spi_q95")) {
    expect_equal(cen$summary[[col]], raw$summary[[col]] / ratio)
  }
  expect_equal(cen$draws, sweep(raw$draws, 2, ratio, "/"))
  # each year's centred index sums back to national parity
  chk <- cen$summary |>
    dplyr::summarise(
      oe = sum(observed) / sum(expected_total * national_oe), .by = "year"
    )
  expect_equal(chk$oe, rep(1, nrow(chk)))
})

test_that("centre = 'none' leaves the index untouched", {
  fit <- make_expected()
  raw <- spi_index(fit, centre = "none", verbose = FALSE)
  expect_null(raw$national)
  expect_false("national_oe" %in% names(raw$summary))
})

test_that("district_total centres on one national ratio", {
  fit <- make_expected()
  cen <- spi_index(fit, level = "district_total", verbose = FALSE)
  expect_equal(nrow(cen$national), 1L)
})

test_that("a year with no detections gives NA, not Inf", {
  fit <- make_expected()
  yr1 <- format(fit$data$month, "%Y") == "2015"
  fit$data$count[yr1] <- 0
  expect_warning(
    cen <- spi_index(fit, verbose = FALSE),
    "no detections"
  )
  expect_true(all(is.na(cen$summary$spi_median[cen$summary$year == 2015])))
  expect_false(any(is.infinite(cen$summary$spi_median)))
})
