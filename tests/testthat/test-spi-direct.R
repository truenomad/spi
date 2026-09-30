# five districts in a strip and two provinces, one row per district-year
direct_toy <- function() {
  cells <- sf::st_make_grid(
    sf::st_as_sfc(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 5, ymax = 1))),
    n = c(5, 1)
  )
  boundaries <- sf::st_sf(district = LETTERS[1:5], geometry = cells)
  counts <- tibble::tribble(
    ~district, ~year, ~npafp_cases, ~population_u15,
    "A", 2022, 8, 100000, "A", 2023, 10, 105000, "A", 2024, 9, 110000,
    "A", 2025, 11, 115000, "A", 2026, 6, 120000,
    "B", 2025, 1, 40000, "B", 2026, 1, 42000,
    "C", 2022, 0, 250000, "C", 2023, 1, 250000, "C", 2024, 0, 250000,
    "C", 2025, 1, 250000, "C", 2026, 1, 255000,
    "D", 2022, 30, 300000, "D", 2023, 32, 310000, "D", 2024, 31, 320000,
    "D", 2025, 33, 330000, "D", 2026, 29, 340000,
    "E", 2022, 12, 150000, "E", 2023, 14, 155000, "E", 2024, 13, 160000,
    "E", 2025, 15, 165000, "E", 2026, 14, 170000
  )
  counts$province <- ifelse(counts$district %in% c("A", "B"), "P1", "P2")
  list(
    counts = counts,
    cases = counts[c("district", "year", "npafp_cases", "province")],
    population = counts[c("district", "year", "population_u15")],
    boundaries = boundaries
  )
}

# five districts are too few to estimate the stabilisation well, so the
# warning that says so is expected here
run_toy <- function(...) suppressWarnings(spi_direct(..., verbose = FALSE))

test_that("the SPI follows from its components", {
  toy <- direct_toy()
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  sm <- res$summary
  expect_s3_class(res, "spi_direct")
  expect_named(
    res, c("summary", "national", "population_qc", "stabilisation",
           "metadata")
  )
  expect_equal(nrow(sm), 5L)
  expect_equal(sm$expected, sm$pop * sm$stabilised_rate / 1e5)
  expect_equal(sm$oe, sm$observed / sm$expected)
  expect_equal(sm$spi, sm$oe / sm$national_oe)
  expect_equal(sum(sm$observed) / sum(sm$expected), sm$national_oe[1])
  expect_true(all(sm$information_score >= 0 & sm$information_score <= 1))
})

test_that("history is taken from preceding years only", {
  toy <- direct_toy()
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  a <- res$summary[res$summary$district == "A", ]
  expect_equal(a$history_cases, 38)
  expect_equal(a$history_pop, 430000)
  expect_equal(a$history_rate, 38 / 430000 * 1e5)
  expect_equal(a$history_years, 4L)
})

test_that("the reference is the country, the region or the neighbours", {
  toy <- direct_toy()
  hist <- toy$counts[toy$counts$year < 2026, ]
  others <- hist[hist$district != "A", ]

  country <- run_toy(toy$cases, toy$population, region_col = NULL,
                     first_assessment = 2026)
  a <- country$summary[country$summary$district == "A", ]
  expect_equal(a$reference_source, "country")
  expect_equal(a$reference_rate,
               sum(others$npafp_cases) / sum(others$population_u15) * 1e5)

  region <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  a <- region$summary[region$summary$district == "A", ]
  b_hist <- hist[hist$district == "B", ]
  expect_equal(a$reference_source, "region")
  expect_equal(a$reference_rate,
               sum(b_hist$npafp_cases) / sum(b_hist$population_u15) * 1e5)

  nbr <- run_toy(toy$cases, toy$population, boundaries = toy$boundaries,
                 first_assessment = 2026)
  a <- nbr$summary[nbr$summary$district == "A", ]
  expect_equal(a$reference_source, "neighbours")
  expect_equal(a$reference_rate, 1 / 40000 * 1e5)
})

test_that("one table with the standard names needs no other argument", {
  toy <- direct_toy()
  one <- suppressWarnings(spi_direct(toy$counts, verbose = FALSE))
  two <- run_toy(toy$cases, toy$population)
  expect_equal(one$summary$spi, two$summary$spi)
  expect_equal(one$metadata$population_source, "data")
  expect_equal(one$metadata$region_col, "province")
  expect_true("region" %in% one$summary$reference_source)
  expect_error(
    spi_direct(toy$cases, verbose = FALSE),
    "population_u15"
  )
})

test_that("the region is optional", {
  toy <- direct_toy()
  no_region <- toy$counts[names(toy$counts) != "province"]
  res <- run_toy(no_region, first_assessment = 2026)
  expect_null(res$metadata$region_col)
  expect_true(all(res$summary$reference_source == "country"))
})

test_that("column names can be mapped", {
  toy <- direct_toy()
  renamed <- toy$counts |>
    dplyr::rename(dist_name = district, yr = year, npafp = npafp_cases,
                  u15_pop = population_u15, province_name = province)
  res <- run_toy(renamed, id_col = "dist_name", year_col = "yr",
                 count_col = "npafp", population_col = "u15_pop",
                 region_col = "province_name", first_assessment = 2026)
  ref <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  expect_equal(res$summary$spi, ref$summary$spi)
  expect_true("dist_name" %in% names(res$summary))
})

test_that("a missing case row counts as zero cases", {
  toy <- direct_toy()
  cases <- toy$cases[!(toy$cases$district == "C" &
                         toy$cases$year == 2024), ]
  res <- run_toy(cases, toy$population, first_assessment = 2026)
  cc <- res$summary[res$summary$district == "C", ]
  expect_equal(cc$history_cases, 2)
  expect_equal(cc$history_years, 4L)
})

test_that("a well-observed history stays close to its own rate", {
  toy <- direct_toy()
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  d <- res$summary[res$summary$district == "D", ]
  expect_lt(abs(d$stabilised_rate / d$history_rate - 1), 0.1)
})

test_that("the simple version uses the historical rate unchanged", {
  toy <- direct_toy()
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026,
                 stabilise = FALSE)
  sm <- res$summary
  expect_equal(sm$stabilised_rate, sm$history_rate)
})

test_that("a history with no case keeps a positive expected count", {
  toy <- direct_toy()
  toy$cases$npafp_cases[toy$cases$district == "C" & toy$cases$year < 2026] <- 0
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  cc <- res$summary[res$summary$district == "C", ]
  expect_equal(cc$history_cases, 0)
  expect_gt(cc$expected, 0)
  expect_true(is.finite(cc$spi))
})

test_that("a district with no preceding year takes the reference rate", {
  toy <- direct_toy()
  res <- run_toy(toy$cases, toy$population, first_assessment = 2025,
                 last_assessment = 2025)
  b <- res$summary[res$summary$district == "B", ]
  expect_equal(b$history_years, 0L)
  expect_equal(b$history_info, "none")
  expect_equal(
    b$stabilised_rate,
    b$reference_rate * res$stabilisation$country_factor
  )
})

test_that("the population check flags a spike and a new district", {
  toy <- direct_toy()
  pop <- toy$population
  pop$population_u15[pop$district == "D" & pop$year == 2024] <- 600000
  res <- run_toy(toy$cases, pop, first_assessment = 2026)
  qc <- res$population_qc
  expect_equal(qc$population_qc[qc$district == "D"], "check_spike")
  expect_equal(qc$population_qc[qc$district == "B"], "check_boundary")
  expect_equal(qc$population_qc[qc$district == "A"], "ok")
})

test_that("a population that steps up and stays up is a change", {
  toy <- direct_toy()
  pop <- toy$population
  step <- pop$district == "E" & pop$year >= 2024
  pop$population_u15[step] <- pop$population_u15[step] * 3
  res <- run_toy(toy$cases, pop, first_assessment = 2026)
  qc <- res$population_qc
  expect_equal(qc$population_qc[qc$district == "E"], "check_change")
})

test_that("inputs are checked", {
  toy <- direct_toy()
  expect_error(
    spi_direct(toy$cases[c("district", "year")], toy$population,
               verbose = FALSE),
    "npafp_cases"
  )
  dup <- rbind(toy$cases, toy$cases[1, ])
  expect_error(
    spi_direct(dup, toy$population, verbose = FALSE),
    "duplicate"
  )
  expect_error(
    spi_direct(toy$cases, toy$population, boundaries = "not a map",
               verbose = FALSE),
    "boundaries"
  )
  expect_error(
    spi_direct(toy$cases, toy$population, region_col = "zone",
               verbose = FALSE),
    "zone"
  )
})

test_that("the explanation follows the calculation", {
  toy <- direct_toy()
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  out <- spi_direct_explain(res, "A", print = FALSE)
  expect_s3_class(out, "tbl_df")
  expect_equal(
    unique(out$section),
    c("Current reporting", "Previous reporting", "Expected reporting",
      "SPI", "Data checks")
  )
  expect_equal(out$value[out$component == "Previous NPAFP cases"], "38")
  expect_equal(
    out$component,
    c("Observed NPAFP cases", "Current population under 15", "NPAFP rate",
      "Previous years", "Previous NPAFP cases", "Previous child-years",
      "Previous rate", "Reference source", "Reference rate",
      "Historical information", "Stabilised rate", "Expected NPAFP cases",
      "District observed / expected", "National observed / expected", "SPI",
      "Population check")
  )
  expect_error(spi_direct_explain(res, "Z"), "No row")
})

test_that("verbose output reports each step", {
  toy <- direct_toy()
  expect_message(
    suppressWarnings(
      spi_direct(toy$cases, toy$population, first_assessment = 2026)
    ),
    "Reading district-year counts"
  )
})
