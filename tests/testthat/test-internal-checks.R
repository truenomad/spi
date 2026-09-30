# Direct unit tests for the validation helpers. spi_expected() exercises them
# on the happy path, but the individual abort branches are cheaper to hit here.

good_cases <- function() {
  tibble::tibble(
    district_id = rep(c("A", "B"), each = 3),
    month = rep(seq(as.Date("2015-01-01"), by = "month", length.out = 3), 2),
    count = 1L
  )
}

test_that(".validate_cases accepts a well-formed table", {
  expect_true(spi:::.validate_cases(good_cases()))
})

test_that(".validate_cases rejects each malformed input", {
  # missing a required column
  expect_error(
    spi:::.validate_cases(tibble::tibble(district_id = "A")),
    "missing columns"
  )
  # month not a Date
  bad_month <- good_cases()
  bad_month$month <- as.character(bad_month$month)
  expect_error(spi:::.validate_cases(bad_month), "must be Date")
  # count not numeric
  bad_num <- good_cases()
  bad_num$count <- as.character(bad_num$count)
  expect_error(spi:::.validate_cases(bad_num), "must be numeric")
  # negative counts
  bad_neg <- good_cases()
  bad_neg$count[1] <- -1L
  expect_error(spi:::.validate_cases(bad_neg), "negative")
  # duplicate district-month rows
  dup <- rbind(good_cases(), good_cases()[1, ])
  expect_error(spi:::.validate_cases(dup), "duplicate")
})

test_that(".validate_population handles both grains and rejects bad tables", {
  cases <- good_cases()

  # annual grain, full coverage
  pop_year <- tibble::tibble(
    district_id = c("A", "B"), year = 2015L, pop = 1e5
  )
  expect_true(spi:::.validate_population(pop_year, cases))

  # monthly grain, full coverage
  pop_month <- tibble::tibble(
    district_id = rep(c("A", "B"), each = 3),
    month = rep(seq(as.Date("2015-01-01"), by = "month", length.out = 3), 2),
    pop = 1e5
  )
  expect_true(spi:::.validate_population(pop_month, cases))

  # neither a month nor a year column
  expect_error(
    spi:::.validate_population(
      tibble::tibble(district_id = "A", pop = 1e5), cases
    ),
    "either"
  )
  # missing the pop column
  expect_error(
    spi:::.validate_population(
      tibble::tibble(district_id = "A", year = 2015L), cases
    ),
    "missing columns"
  )
  # coverage below 80% aborts
  expect_error(
    spi:::.validate_population(
      tibble::tibble(district_id = "A", year = 2015L, pop = 1e5), cases
    ),
    "covers only"
  )
})

test_that(".validate_population warns on partial (<100%) coverage", {
  # 5 districts in the cases, 4 covered -> 80% -> warns but does not abort
  cases <- tibble::tibble(
    district_id = rep(LETTERS[1:5], each = 2),
    month = rep(seq(as.Date("2015-01-01"), by = "month", length.out = 2), 5),
    count = 1L
  )
  pop <- tibble::tibble(district_id = LETTERS[1:4], year = 2015L, pop = 1e5)
  expect_true(spi:::.validate_population(pop, cases))
})

test_that(".check_pkg passes for an installed package", {
  expect_true(spi:::.check_pkg("stats"))
})

test_that("a missing INLA gives the install command and points to spi_direct()", {
  testthat::local_mocked_bindings(.has_pkg = function(pkg) FALSE)
  err <- expect_error(.check_inla(), class = "rlang_error")
  msg <- conditionMessage(err)
  expect_match(msg, "not on CRAN", fixed = TRUE)
  expect_match(msg, "inla.r-inla-download.org", fixed = TRUE)
  expect_match(msg, "spi_direct", fixed = TRUE)
})

test_that("an installed INLA passes the check", {
  testthat::local_mocked_bindings(.has_pkg = function(pkg) TRUE)
  expect_invisible(.check_inla())
})
