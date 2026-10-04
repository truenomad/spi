# spi_index() refits spi_expected() once per assessment year. The unit tests
# stub spi_expected() with a fake fit built from the window it is handed, so
# the masking and dropping can be checked without INLA.

# a spi_expected built from the window it is handed; `data` carries the
# id column, month, count and pop, as the real fit's does
fake_fit <- function(cases, population, id_col, pop_col = "pop_u15", ...) {
  pop <- dplyr::rename(population, pop = dplyr::all_of(pop_col))
  if ("month" %in% names(pop)) {
    data <- dplyr::left_join(
      cases, pop[, c(id_col, "month", "pop")], by = c(id_col, "month")
    )
  } else {
    data <- cases |>
      dplyr::mutate(year = as.integer(format(.data$month, "%Y"))) |>
      dplyr::left_join(pop[, c(id_col, "year", "pop")], by = c(id_col, "year"))
  }
  n <- nrow(data)
  draws <- withr::with_seed(1, matrix(stats::rgamma(40 * n, 20, 20), 40, n))
  colnames(draws) <- paste(
    data[[id_col]], format(data$month, "%Y-%m"), sep = "|"
  )
  structure(
    list(draws = draws, data = data, id_col = id_col),
    class = "spi_expected"
  )
}

test_that("each window masks its target year and drops later years", {
  seen <- list()
  local_mocked_bindings(spi_expected = function(cases, population, ...) {
    seen[[length(seen) + 1L]] <<- list(cases = cases, population = population)
    fake_fit(cases, population, ...)
  })
  d <- toy_panel(years = 2015:2020)
  res <- spi_index(
    d$cases, d$population, d$adj,
    first_assessment = 2018, id_col = "adm2_guid", verbose = FALSE
  )
  expect_s3_class(res, "spi_index")
  expect_identical(res$level, "district_year")
  expect_length(seen, 3L)
  for (i in seq_along(seen)) {
    target <- 2017L + i
    yr <- as.integer(format(seen[[i]]$cases$month, "%Y"))
    expect_identical(max(yr), target)
    expect_identical(min(yr), 2015L)
    expect_true(all(is.na(seen[[i]]$cases$count[yr == target])))
    expect_false(anyNA(seen[[i]]$cases$count[yr < target]))
    expect_identical(max(seen[[i]]$population$year), target)
  }
  expect_equal(sort(unique(res$summary$year)), 2018:2020)
  # the index reads the real counts of the masked year
  truth <- d$cases |>
    dplyr::mutate(year = as.integer(format(month, "%Y"))) |>
    dplyr::summarise(observed = sum(count), .by = c("adm2_guid", "year"))
  got <- dplyr::inner_join(res$summary, truth, by = c("adm2_guid", "year"))
  expect_identical(nrow(got), nrow(res$summary))
  expect_equal(got$observed.x, got$observed.y)
  expect_identical(ncol(res$draws), nrow(res$summary))
  expect_identical(res$windows$year, 2018:2020)
  expect_identical(
    res$windows$window_start,
    as.Date(c("2018-01-01", "2019-01-01", "2020-01-01"))
  )
  expect_identical(
    res$windows$window_end,
    as.Date(c("2018-12-01", "2019-12-01", "2020-12-01"))
  )
  expect_identical(res$windows$training_start, rep(as.Date("2015-01-01"), 3))
  expect_identical(
    res$windows$training_end,
    as.Date(c("2017-12-01", "2018-12-01", "2019-12-01"))
  )
  expect_identical(res$windows$months, rep(12L, 3))
  expect_identical(res$windows$districts, rep(6L, 3))
  expect_equal(res$totals$total_observed, sum(res$summary$observed))
  expect_identical(res$totals$n_groups, nrow(res$summary))
})

test_that("year_end_month fits each rolling window on the months before it", {
  seen <- list()
  local_mocked_bindings(spi_expected = function(cases, population, ...) {
    seen[[length(seen) + 1L]] <<- cases
    fake_fit(cases, population, ...)
  })
  d <- toy_panel(years = 2015:2020)
  # May 2018 to April 2019 is the first window with three years before it;
  # the data end in December 2020, so the last window is May to December 2020
  expect_warning(
    res <- spi_index(
      d$cases, d$population, d$adj,
      first_assessment = 2019, year_end_month = 4, id_col = "adm2_guid",
      verbose = FALSE
    ),
    "fewer than 12 months"
  )
  opens <- as.Date(c("2018-05-01", "2019-05-01", "2020-05-01"))
  closes <- as.Date(c("2019-04-01", "2020-04-01", "2021-04-01"))
  expect_length(seen, 3L)
  for (i in seq_along(seen)) {
    m <- seen[[i]]$month
    expect_identical(max(m), min(closes[[i]], as.Date("2020-12-01")))
    expect_true(all(is.na(seen[[i]]$count[m >= opens[[i]]])))
    expect_false(anyNA(seen[[i]]$count[m < opens[[i]]]))
  }
  expect_identical(res$windows$window_start, opens)
  expect_identical(res$windows$window_end, closes)
  expect_identical(
    res$windows$training_end,
    as.Date(c("2018-04-01", "2019-04-01", "2020-04-01"))
  )
  expect_identical(res$windows$months, c(12L, 12L, 8L))
  expect_equal(sort(unique(res$summary$year)), 2019:2021)
  expect_equal(
    vapply(split(res$summary$n_months, res$summary$year), unique, integer(1)),
    c("2019" = 12L, "2020" = 12L, "2021" = 8L)
  )
  # each row sums the real counts of its May to April window
  truth <- d$cases |>
    dplyr::filter(month >= opens[[1]]) |>
    dplyr::mutate(
      year = as.integer(format(month, "%Y")) +
        as.integer(format(month, "%m") > "04")
    ) |>
    dplyr::summarise(observed = sum(count), .by = c("adm2_guid", "year"))
  got <- dplyr::inner_join(res$summary, truth, by = c("adm2_guid", "year"))
  expect_identical(nrow(got), nrow(res$summary))
  expect_equal(got$observed.x, got$observed.y)

  # the first window needs three years before it opens
  expect_error(
    spi_index(d$cases, d$population, d$adj,
      first_assessment = 2018, year_end_month = 4, id_col = "adm2_guid"),
    "earliest is 2019"
  )
})

test_that("every level covers the assessment years only", {
  local_mocked_bindings(spi_expected = function(cases, population, ...) {
    fake_fit(cases, population, ...)
  })
  d <- toy_panel(years = 2015:2020)
  args <- list(
    d$cases, d$population, d$adj,
    first_assessment = 2018, id_col = "adm2_guid", verbose = FALSE
  )
  assessed <- d$cases[d$cases$month >= as.Date("2018-01-01"), ]

  dm <- do.call(spi_index, c(args, level = "district_month"))
  expect_identical(nrow(dm$summary), nrow(assessed))
  expect_identical(min(dm$summary$month), as.Date("2018-01-01"))
  expect_equal(sum(dm$summary$observed), sum(assessed$count))

  dq <- do.call(spi_index, c(args, level = "district_quarter"))
  expect_identical(nrow(dq$summary), 6L * 12L)

  dt <- do.call(spi_index, c(args, level = "district_total"))
  expect_identical(nrow(dt$summary), 6L)
  per_district <- assessed |>
    dplyr::summarise(observed = sum(count), .by = "adm2_guid")
  got <- dplyr::inner_join(dt$summary, per_district, by = "adm2_guid")
  expect_equal(got$observed.x, got$observed.y)
  expect_identical(nrow(dt$national), 1L)
  expect_equal(dt$totals$total_observed, sum(assessed$count))

  # year_end_month is ignored away from district_year, so windows stay
  # calendar years
  expect_message(
    dm4 <- do.call(
      spi_index, c(args, level = "district_month", year_end_month = 4)
    ),
    "district_year"
  )
  expect_identical(dm4$windows$window_start, dm$windows$window_start)
})

test_that("last_assessment stops the refits early", {
  local_mocked_bindings(spi_expected = function(cases, population, ...) {
    fake_fit(cases, population, ...)
  })
  d <- toy_panel(years = 2015:2020)
  res <- spi_index(
    d$cases, d$population, d$adj,
    first_assessment = 2018, last_assessment = 2019,
    id_col = "adm2_guid", verbose = FALSE
  )
  expect_equal(sort(unique(res$summary$year)), 2018:2019)
})

test_that("min_history is enforced", {
  d <- toy_panel(years = 2015:2020)
  expect_error(
    spi_index(d$cases, d$population, d$adj,
      first_assessment = 2017, id_col = "adm2_guid"),
    "at least 3 years"
  )
})

test_that("an assessment year past the data aborts", {
  d <- toy_panel(years = 2015:2020)
  expect_error(
    spi_index(d$cases, d$population, d$adj,
      first_assessment = 2021, id_col = "adm2_guid"),
    "2020"
  )
})

test_that("arguments spi_index controls are refused in ...", {
  d <- toy_panel(years = 2015:2020)
  expect_error(
    spi_index(d$cases, d$population, d$adj,
      first_assessment = 2018, id_col = "adm2_guid", keep_draws = FALSE),
    "keep_draws"
  )
})

test_that("predictive = TRUE is not yet supported", {
  d <- toy_panel(years = 2015:2020)
  expect_error(
    spi_index(d$cases, d$population, d$adj,
      first_assessment = 2018, id_col = "adm2_guid", predictive = TRUE),
    "not yet supported"
  )
})

test_that("a part-year target is kept, flagged and warned about", {
  local_mocked_bindings(spi_expected = function(cases, population, ...) {
    fake_fit(cases, population, ...)
  })
  d <- toy_panel(years = 2015:2020)
  # drop July to December of the last year
  d$cases <- d$cases[d$cases$month < as.Date("2020-07-01"), ]
  expect_warning(
    res <- spi_index(
      d$cases, d$population, d$adj,
      first_assessment = 2018, id_col = "adm2_guid", verbose = FALSE
    ),
    "fewer than 12 months"
  )
  last <- res$summary[res$summary$year == 2020, ]
  expect_identical(nrow(last), 6L)
  expect_true(all(last$n_months == 6L))
  expect_true(all(res$summary$n_months[res$summary$year < 2020] == 12L))
})

test_that("monthly population is filtered by month", {
  max_pop_month <- list()
  local_mocked_bindings(spi_expected = function(cases, population, ...) {
    max_pop_month[[length(max_pop_month) + 1L]] <<- max(population$month)
    fake_fit(cases, population, ...)
  })
  d <- toy_panel(years = 2015:2020)
  pop_month <- dplyr::distinct(d$cases, adm2_guid, month) |>
    dplyr::mutate(pop_u15 = 1e5 / 12)
  res <- spi_index(
    d$cases, pop_month, d$adj,
    first_assessment = 2018, id_col = "adm2_guid", verbose = FALSE
  )
  expect_length(max_pop_month, 3L)
  got <- as.integer(format(do.call(c, max_pop_month), "%Y"))
  expect_identical(got, 2018:2020)
  expect_equal(sort(unique(res$summary$year)), 2018:2020)
})

test_that("centring is per assessment year", {
  local_mocked_bindings(spi_expected = function(cases, population, ...) {
    fake_fit(cases, population, ...)
  })
  d <- toy_panel(years = 2015:2020)
  args <- list(
    d$cases, d$population, d$adj,
    first_assessment = 2018, id_col = "adm2_guid", verbose = FALSE
  )
  cen <- do.call(spi_index, args)
  raw <- do.call(spi_index, c(args, centre = "none"))
  expect_identical(cen$centre, "national")
  expect_null(raw$national)
  expect_false("national_oe" %in% names(raw$summary))

  # one national ratio per assessment year, from that year's rows only
  nat_obs <- raw$summary |>
    dplyr::summarise(observed = sum(observed), .by = "year")
  expect_identical(cen$national$year, nat_obs$year)
  expect_equal(cen$national$national_observed, nat_obs$observed)
  key <- match(cen$summary$year, cen$national$year)
  expect_equal(cen$summary$national_oe, cen$national$national_oe[key])
  # each year's draws are divided by that year's per-draw national ratio
  exp_draws <- sweep(1 / raw$draws, 2, raw$summary$observed, "*")
  nat_draws <- vapply(seq_len(nrow(nat_obs)), \(k) {
    nat_obs$observed[k] / rowSums(exp_draws[, key == k, drop = FALSE])
  }, numeric(nrow(raw$draws)))
  expect_equal(cen$draws, raw$draws / nat_draws[, key])
  expect_equal(cen$summary$spi_median, matrixStats::colMedians(cen$draws))
})

test_that("boundaries attach admin names to the summary", {
  local_mocked_bindings(spi_expected = function(cases, population, ...) {
    fake_fit(cases, population, ...)
  })
  d <- toy_panel(years = 2015:2020)
  bnd <- tibble::tibble(
    adm2_guid = paste0("D", 1:6), adm2_name = paste("District", 1:6)
  )
  res <- spi_index(
    d$cases, d$population, d$adj,
    first_assessment = 2018, id_col = "adm2_guid", boundaries = bnd,
    verbose = FALSE
  )
  expect_identical(names(res$summary)[1:2], c("adm2_name", "adm2_guid"))
})

test_that("spi_index fits synth_surveillance end to end", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  s <- synth_surveillance
  yrs <- as.integer(format(s$cases$month, "%Y"))
  last_three <- max(yrs) - 2:0
  adj <- spi_adjacency(s$boundaries, id_col = "adm2_guid")
  res <- inla_or_skip(
    spi_index(
      s$cases, s$population, adj,
      first_assessment = last_three[[1]], id_col = "adm2_guid",
      n_draws = 200L, verbose = FALSE
    )
  )
  expect_s3_class(res, "spi_index")
  n_dist <- dplyr::n_distinct(s$cases$adm2_guid)
  expect_identical(nrow(res$summary), 3L * n_dist)
  expect_identical(
    nrow(dplyr::distinct(res$summary, adm2_guid, year)), 3L * n_dist
  )
  expect_equal(sort(unique(res$summary$year)), last_three)
  ok <- res$summary$expected_total >= 1
  expect_true(all(is.finite(res$summary$spi_median[ok])))
  conc <- spi_compare_npafp(
    res, cases = s$cases, population = s$population, verbose = FALSE
  )
  expect_s3_class(conc, "spi_compare_npafp")
})

test_that("the verbose path reports each year and inputs are validated", {
  local_mocked_bindings(spi_expected = function(cases, population, ...) {
    fake_fit(cases, population, ...)
  })
  d <- toy_panel(years = 2015:2020)
  expect_message(
    spi_index(d$cases, d$population, d$adj,
      first_assessment = 2020, id_col = "adm2_guid"),
    "Fitting 2020: training 2015-01 to 2019-12"
  )
  expect_error(
    spi_index(d$cases, d$population, d$adj,
      first_assessment = 2018.5, id_col = "adm2_guid"),
    "single whole number"
  )
  expect_error(
    spi_index(d$cases, d$population, d$adj,
      first_assessment = 2018, id_col = "district_id"),
    "missing"
  )
  expect_error(
    spi_index(d$cases, d$population, d$adj,
      first_assessment = 2019, last_assessment = 2018, id_col = "adm2_guid"),
    "within the data"
  )
})

test_that("spi_index validates the index arguments before fitting", {
  d <- toy_panel(years = 2015:2020)
  run <- function(...) {
    spi_index(d$cases, d$population, d$adj,
      first_assessment = 2018, id_col = "adm2_guid", ...)
  }
  expect_error(run(min_expected = -1), "min_expected")
  expect_error(run(min_expected = "x"))
  expect_error(run(level = "nonsense"))
  expect_error(run(year_end_month = 13), "year_end_month")
  expect_error(run(year_end_month = NA), "year_end_month")
  duplicated <- dplyr::bind_rows(d$cases, d$cases[1, ])
  expect_error(
    spi_index(duplicated, d$population, d$adj,
      first_assessment = 2018, id_col = "adm2_guid"),
    "duplicate district-month"
  )
  for (value in c(-1, 0.5, Inf, -Inf)) {
    invalid <- d$cases
    invalid$count[1] <- value
    expect_error(
      spi_index(invalid, d$population, d$adj,
        first_assessment = 2018, id_col = "adm2_guid"),
      "non-negative whole counts"
    )
  }
})

test_that("indices supply counts for NPAFP comparison", {
  local_mocked_bindings(spi_expected = fake_fit)
  d <- toy_panel(years = 2015:2020)
  index <- spi_index(d$cases, d$population, d$adj,
    first_assessment = 2018, id_col = "adm2_guid", verbose = FALSE)
  comparison <- spi_compare_npafp(index, population = d$population,
                                  verbose = FALSE)
  expect_equal(comparison$district_year$count_annual, index$summary$observed)
  expect_equal(nrow(comparison$district_year), nrow(index$summary))
})
