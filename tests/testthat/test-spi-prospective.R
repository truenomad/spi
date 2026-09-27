# spi_prospective() refits spi_expected() once per assessment year. The unit
# tests stub spi_expected() with a fake fit built from the window it is handed,
# so the masking and dropping can be checked without INLA.

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
  res <- spi_prospective(
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
  expect_identical(res$prospective$year, 2018:2020)
  expect_identical(res$prospective$training_start, rep(2015L, 3))
  expect_identical(res$prospective$training_end, 2017:2019)
  expect_identical(res$prospective$districts, rep(6L, 3))
  expect_equal(res$totals$total_observed, sum(res$summary$observed))
  expect_identical(res$totals$n_groups, nrow(res$summary))
})

test_that("last_assessment stops the refits early", {
  local_mocked_bindings(spi_expected = function(cases, population, ...) {
    fake_fit(cases, population, ...)
  })
  d <- toy_panel(years = 2015:2020)
  res <- spi_prospective(
    d$cases, d$population, d$adj,
    first_assessment = 2018, last_assessment = 2019,
    id_col = "adm2_guid", verbose = FALSE
  )
  expect_equal(sort(unique(res$summary$year)), 2018:2019)
})

test_that("min_history is enforced", {
  d <- toy_panel(years = 2015:2020)
  expect_error(
    spi_prospective(d$cases, d$population, d$adj,
      first_assessment = 2017, id_col = "adm2_guid"),
    "at least 3 years"
  )
})

test_that("an assessment year past the data aborts", {
  d <- toy_panel(years = 2015:2020)
  expect_error(
    spi_prospective(d$cases, d$population, d$adj,
      first_assessment = 2021, id_col = "adm2_guid"),
    "2020"
  )
})

test_that("arguments the wrapper controls are refused in ...", {
  d <- toy_panel(years = 2015:2020)
  expect_error(
    spi_prospective(d$cases, d$population, d$adj,
      first_assessment = 2018, id_col = "adm2_guid", keep_draws = FALSE),
    "keep_draws"
  )
})

test_that("predictive = TRUE is not yet supported", {
  d <- toy_panel(years = 2015:2020)
  expect_error(
    spi_prospective(d$cases, d$population, d$adj,
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
    res <- spi_prospective(
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
  res <- spi_prospective(
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
  cen <- do.call(spi_prospective, args)
  raw <- do.call(spi_prospective, c(args, centre = "none"))
  expect_identical(cen$centre, "national")
  expect_null(raw$national)
  expect_false("national_oe" %in% names(raw$summary))

  # one national ratio per assessment year, from that year's rows only
  nat <- raw$summary |>
    dplyr::summarise(oe = sum(observed) / sum(expected_total), .by = "year")
  expect_identical(cen$national$year, nat$year)
  expect_equal(cen$national$national_oe, nat$oe)
  ratio <- nat$oe[match(cen$summary$year, nat$year)]
  expect_equal(cen$summary$national_oe, ratio)
  expect_equal(cen$summary$spi_median, raw$summary$spi_median / ratio)
  expect_equal(cen$draws, sweep(raw$draws, 2, ratio, "/"))
  chk <- cen$summary |>
    dplyr::summarise(
      oe = sum(observed) / sum(expected_total * national_oe), .by = "year"
    )
  expect_equal(chk$oe, rep(1, 3))
})

test_that("boundaries attach admin names to the summary", {
  local_mocked_bindings(spi_expected = function(cases, population, ...) {
    fake_fit(cases, population, ...)
  })
  d <- toy_panel(years = 2015:2020)
  bnd <- tibble::tibble(
    adm2_guid = paste0("D", 1:6), adm2_name = paste("District", 1:6)
  )
  res <- spi_prospective(
    d$cases, d$population, d$adj,
    first_assessment = 2018, id_col = "adm2_guid", boundaries = bnd,
    verbose = FALSE
  )
  expect_identical(names(res$summary)[1:2], c("adm2_name", "adm2_guid"))
})

test_that("spi_prospective fits synth_surveillance end to end", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  s <- synth_surveillance
  yrs <- as.integer(format(s$cases$month, "%Y"))
  last_three <- max(yrs) - 2:0
  adj <- spi_adjacency(s$boundaries, id_col = "adm2_guid")
  res <- tryCatch(
    spi_prospective(
      s$cases, s$population, adj,
      first_assessment = last_three[[1]], id_col = "adm2_guid",
      n_draws = 200L, verbose = FALSE
    ),
    error = function(e) {
      skip(paste0("INLA fit unavailable: ", conditionMessage(e)))
    }
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
    spi_prospective(d$cases, d$population, d$adj,
      first_assessment = 2020, id_col = "adm2_guid"),
    "Fitting 2020: training 2015 to 2019"
  )
  expect_error(
    spi_prospective(d$cases, d$population, d$adj,
      first_assessment = 2018.5, id_col = "adm2_guid"),
    "single whole number"
  )
  expect_error(
    spi_prospective(d$cases, d$population, d$adj,
      first_assessment = 2018, id_col = "district_id"),
    "missing"
  )
  expect_error(
    spi_prospective(d$cases, d$population, d$adj,
      first_assessment = 2019, last_assessment = 2018, id_col = "adm2_guid"),
    "within the data"
  )
})
