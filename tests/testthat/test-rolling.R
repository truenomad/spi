# Rolling-window SPI: a fit-like object with fixed draws, so every value can
# be checked by hand. Three districts, two months, four identical draws.

roll_fit <- function(expected = c(1, 1, 2, 2, 0.5, 0.5)) {
  data <- tibble::tibble(
    adm2_guid = rep(c("D1", "D2", "D3"), each = 2),
    month = rep(as.Date(c("2024-01-01", "2024-02-01")), 3),
    count = c(1L, 1L, 2L, 2L, 0L, 1L),
    pop = rep(c(1e5, 2e5, 5e4), each = 2)
  )
  list(data = data, draws = matrix(rep(expected, each = 4), nrow = 4))
}

roll_parents <- function() {
  tibble::tibble(
    adm2_guid = c("D1", "D2", "D3"), adm1_guid = c("P1", "P1", "P2")
  )
}

test_that("spi_window_summary sums the window inside each draw", {
  fit <- roll_fit()
  months <- as.Date(c("2024-01-01", "2024-02-01"))
  r <- spi_window_summary(fit, months)
  expect_equal(r$adm2_guid, c("D1", "D2", "D3"))
  expect_true(all(c("observed", "pop_u15", "months", "expected_total",
                    "spi_median", "spi_q05", "spi_q95", "national_oe")
                  %in% names(r)))
  # national: 7 observed over 7 expected
  expect_equal(r$national_oe, rep(1, 3))
  expect_equal(r$observed, c(2, 4, 1))
  expect_equal(r$expected_total, c(2, 4, 1))
  expect_equal(r$pop_u15, c(1e5, 2e5, 5e4))
  expect_equal(r$spi_median, c(1, 1, 1))

  jan <- spi_window_summary(fit, months[1])
  expect_equal(jan$observed, c(1, 2, 0))
  expect_equal(jan$months, rep(1L, 3))
  expect_equal(jan$spi_median, c(1, 1, 0) / (3 / 3.5))

  p <- spi_window_summary(fit, months, parents = roll_parents())
  expect_equal(p$adm1_guid, c("P1", "P2"))
  expect_equal(p$observed, c(6, 1))
  expect_equal(p$pop_u15, c(3e5, 5e4))
  expect_equal(p$expected_total, c(6, 1))
})

test_that("an incomplete window keeps the pipeline's formulas by default", {
  fit <- roll_fit()
  # three window months, of which only January and February are in the fit
  window <- as.Date(c("2023-12-01", "2024-01-01", "2024-02-01"))
  got <- spi_rolling_index(fit, as.Date("2024-03-10"), window_months = 3)
  d1 <- dplyr::filter(got, .data$adm2_guid == "D1")
  # pop_u15: (100,000 + 100,000) / 3 window months
  expect_equal(d1$pop_u15, 2e5 / 3)
  expect_equal(d1$months, 2L)
  # npafp_rate: 2 cases / (pop_u15 / 100,000), not annualised
  expect_equal(d1$npafp_rate, 2 / ((2e5 / 3) / 1e5))

  alt <- spi_rolling_index(
    fit, as.Date("2024-03-10"), window_months = 3,
    person_time = "observed", annualise = TRUE
  ) |>
    dplyr::filter(.data$adm2_guid == "D1")
  expect_equal(alt$pop_u15, 1e5)
  expect_equal(alt$npafp_rate, 2 / (1e5 / 1e5) * 12 / 2)
  expect_equal(
    spi_window_summary(fit, window)$pop_u15, c(1e5, 2e5, 5e4) * 2 / 3
  )
  expect_error(spi_window_summary(fit, window, person_time = "x"))
})

test_that("spi_window_summary validates the fit and window", {
  fit <- roll_fit()
  months <- as.Date("2024-01-01")
  expect_error(spi_window_summary(list(), months), "fit")
  bad <- fit
  bad$draws <- bad$draws[, 1:5]
  expect_error(spi_window_summary(bad, months), "draw column")
  expect_error(spi_window_summary(fit, as.Date("2020-01-01")), "No month")
  expect_error(spi_window_summary(fit, "2024-01-01"), "window")
  expect_error(
    spi_window_summary(fit, months, parents = tibble::tibble(x = 1)),
    "missing column"
  )
})

test_that("spi_window_classify applies both thresholds at their edges", {
  summary <- tibble::tibble(
    adm2_guid = paste0("D", 1:7),
    observed = c(3, 3, 3, 2, 2, 3, 3),
    pop_u15 = 1e5,
    months = 12L,
    expected_total = c(5, 5, 5, 5, 5, 0.99, 1),
    spi_median = c(0.8, 0.79, 0.79, 0.79, 0.9, 0.5, NA),
    spi_q95 = c(0.9, 1, 0.99, 0.99, 0.95, 0.6, NA)
  )
  got <- spi_window_classify(summary)
  # a rate of exactly 3 meets the target; 2 does not
  expect_equal(got$rate_pass, c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
  # 0.8 is not below the threshold; an upper bound of exactly 1 is not below 1
  expect_equal(got$index_pass[1:5], c(TRUE, TRUE, FALSE, FALSE, TRUE))
  expect_equal(
    as.character(got$agreement),
    c("both_pass", "both_pass", "spi_only", "both_flag", "rate_only", NA, NA)
  )
  expect_equal(
    as.character(got$focus_status),
    c("other", "other", "focus", "other", "other", NA, NA)
  )
  expect_equal(got$readable, c(rep(TRUE, 5), FALSE, FALSE))
  half <- dplyr::mutate(summary, months = 6L)
  expect_equal(spi_window_classify(half)$npafp_rate[1], 3)
  expect_equal(spi_window_classify(half, annualise = TRUE)$npafp_rate[1], 6)
  stricter <- spi_window_classify(
    summary, rate_target = 3.1, index_threshold = 0.81
  )
  expect_equal(
    as.character(stricter$agreement[1:2]), c("both_flag", "rate_only")
  )
})

test_that("spi_window_classify validates its arguments", {
  full <- tibble::tibble(
    observed = 1, pop_u15 = 1, months = 12L, expected_total = 1,
    spi_median = 1, spi_q95 = 1
  )
  expect_error(
    spi_window_classify(dplyr::select(full, "observed")), "missing column"
  )
  expect_error(spi_window_classify(full, rate_target = "3"), "rate_target")
  expect_error(spi_window_classify(full, rate_multiplier = 0), "positive")
  expect_error(spi_window_classify(full, annualise = NA), "annualise")
  expect_error(
    spi_window_classify(full, focus_levels = "focus"), "focus_levels"
  )
})

test_that("spi_rolling_index adds stool adequacy at both levels", {
  fit <- roll_fit(expected = rep(1, 6))
  cases <- tibble::tibble(
    adm1_guid = c("P1", "P1", "P1", "P2", "P2"),
    adm2_guid = c("D1", "D1", "D2", "D3", NA),
    classification_all = "NPAFP",
    surveillance_type_name = c("AFP", "AFP", "AFP", "AFP", "AFP"),
    paralysis_onset_date = as.Date(
      c("2024-01-10", "2024-01-20", "2024-02-01", "2024-03-02", "2024-01-01")
    ),
    case_date = as.Date(NA),
    stool1collection_date = as.Date(NA),
    adequate_stool_with_condition = c("Yes", "No", "", "Yes", "Yes")
  )
  got <- spi_rolling_index(fit, as.Date("2024-03-15"), window_months = 2,
                           cases = cases)
  expect_equal(nrow(got), 3)
  expect_false(is.unsorted(got$spi_median))
  d1 <- dplyr::filter(got, .data$adm2_guid == "D1")
  expect_equal(d1$stool_cases, 2L)
  expect_equal(d1$stool_adequacy, 50)
  # D2's only value is blank; D3's case falls outside the window
  expect_true(all(is.na(got$stool_cases[got$adm2_guid != "D1"])))

  prov <- spi_rolling_index(fit, as.Date("2024-03-15"), window_months = 2,
                            parents = roll_parents(), cases = cases)
  expect_setequal(prov$adm1_guid, c("P1", "P2"))
  # P2's record with no district is left out
  expect_equal(prov$stool_cases[prov$adm1_guid == "P2"], NA_integer_)
  expect_error(spi_rolling_index(as_of = "2024-03-15"), "Supply")
})

test_that("spi_rolling_index fits the model when given data", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")

  square <- function(x, y) {
    sf::st_polygon(list(rbind(
      c(x, y), c(x + 1, y), c(x + 1, y + 1), c(x, y + 1), c(x, y)
    )))
  }
  boundaries <- sf::st_sf(
    adm2_guid = c("D1", "D2", "D3", "D4"),
    geometry = sf::st_sfc(
      square(0, 0), square(1, 0), square(0, 1), square(1, 1)
    )
  )
  set.seed(1)
  n <- 120
  cases <- tibble::tibble(
    adm2_guid = sample(boundaries$adm2_guid, n, replace = TRUE),
    classification_all = "NPAFP",
    surveillance_type_name = "AFP",
    paralysis_onset_date = as.Date("2022-01-01") + sample(0:729, n, TRUE),
    case_date = as.Date(NA),
    stool1collection_date = as.Date(NA),
    adequate_stool_with_condition = "Yes"
  )
  pop <- dplyr::cross_join(
    tibble::tibble(adm2_guid = boundaries$adm2_guid),
    tibble::tibble(year = 2022:2023)
  ) |>
    dplyr::mutate(u15_pop = 1e5)
  got <- inla_or_skip(spi_rolling_index(
    as_of = as.Date("2024-01-15"), start_year = 2022, cases = cases,
    population = pop, adjacency = boundaries, n_draws = 50L,
    verbose = FALSE
  ))
  expect_equal(nrow(got), 4)
  expect_true(all(got$expected_total > 0))
  expect_true(all(!is.na(got$agreement)))
})
