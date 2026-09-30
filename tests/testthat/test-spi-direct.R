# five districts, one row per district-year
direct_toy <- function() {
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
  list(
    counts = counts,
    cases = counts[c("district", "year", "npafp_cases")],
    population = counts[c("district", "year", "population_u15")]
  )
}

run_toy <- function(...) spi_direct(..., verbose = FALSE)

test_that("the SPI follows from its components", {
  toy <- direct_toy()
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  sm <- res$summary
  expect_s3_class(res, "spi_direct")
  expect_named(res, c("summary", "national", "population_qc", "metadata"))
  expect_equal(nrow(sm), 5L)
  expect_true(all(sm$history_check == "ok"))
  expect_equal(sm$expected, sm$pop * sm$history_rate / 1e5)
  expect_equal(sm$oe, sm$observed / sm$expected)
  expect_equal(sm$spi, sm$oe / sm$national_oe)
  expect_equal(sum(sm$observed) / sum(sm$expected), sm$national_oe[1])
})

test_that("District A's expected count is exact", {
  toy <- direct_toy()
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  a <- res$summary[res$summary$district == "A", ]
  expect_equal(a$expected, 120000 * 38 / 430000, tolerance = 0)
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

test_that("one table with the standard names needs no other argument", {
  toy <- direct_toy()
  one <- spi_direct(toy$counts, verbose = FALSE)
  two <- run_toy(toy$cases, toy$population)
  expect_equal(one$summary$spi, two$summary$spi)
  expect_equal(one$metadata$population_source, "data")
  expect_error(
    spi_direct(toy$cases, verbose = FALSE),
    "population_u15"
  )
})

test_that("the vignette's District A matches the hand calculation", {
  path <- test_path("..", "..", "vignettes", "spi-direct.Rmd")
  if (!file.exists(path)) {
    path <- system.file("doc", "spi-direct.Rmd", package = "spi")
  }
  skip_if_not(file.exists(path), "Vignette source is not installed")
  lines <- readLines(path, warn = FALSE)
  chunk <- function(label) {
    start <- grep(paste0("^```\\{r ", label, "[,}]"), lines)
    stopifnot(length(start) == 1L)
    end <- which(seq_along(lines) > start & lines == "```")[1]
    parse(text = lines[seq.int(start + 1L, end - 1L)])
  }

  withr::local_seed(1)
  example <- new.env(parent = environment())
  eval(chunk("example-country"), envir = example)
  eval(chunk("run"), envir = example)
  a <- example$spi_results$summary
  a <- a[a$district == "A" & a$year == 2026, ]
  expect_equal(nrow(a), 1L)
  expect_equal(a$expected, 120000 * 38 / 430000, tolerance = 0)
})

test_that("column names can be mapped", {
  toy <- direct_toy()
  renamed <- toy$counts |>
    dplyr::rename(dist_name = district, yr = year, npafp = npafp_cases,
                  u15_pop = population_u15)
  res <- run_toy(renamed, id_col = "dist_name", year_col = "yr",
                 count_col = "npafp", pop_col = "u15_pop",
                 first_assessment = 2026)
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

test_that("missing combined-table rows are omitted rather than filled with zeros", {
  toy <- direct_toy()
  afp <- toy$counts
  afp <- afp[!(afp$district == "C" & afp$year %in% c(2024, 2026)), ]

  combined <- run_toy(afp)
  c_ <- combined$summary[combined$summary$district == "C", ]
  expect_equal(c_$year, 2025L)
  expect_equal(c_$history_years, 2L)
  expect_equal(c_$history_pop, 500000)
  expect_equal(c_$history_cases, 1)
  expect_equal(c_$population_qc, "check_history")

  # A separate population table restores both missing district-years.
  separate <- run_toy(afp, population = toy$population)
  c_ <- separate$summary[separate$summary$district == "C" &
                          separate$summary$year == 2026, ]
  expect_equal(nrow(c_), 1L)
  expect_equal(c_$observed, 0)
  expect_equal(c_$history_years, 4L)
  expect_equal(c_$history_pop, 1000000)

  # An explicit zero in a combined table also retains the assessment row.
  complete <- toy$counts
  complete$npafp_cases[complete$district == "C" & complete$year == 2026] <- 0
  retained <- run_toy(complete)
  c_ <- retained$summary[retained$summary$district == "C" &
                          retained$summary$year == 2026, ]
  expect_equal(nrow(c_), 1L)
  expect_equal(c_$observed, 0)
  expect_gt(c_$expected, 0)
})

test_that("a history with no previous case gives no SPI", {
  toy <- direct_toy()
  toy$cases$npafp_cases[toy$cases$district == "C" & toy$cases$year < 2026] <- 0
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  cc <- res$summary[res$summary$district == "C", ]
  expect_equal(cc$history_check, "no previous case")
  expect_equal(cc$history_cases, 0)
  expect_equal(cc$history_rate, 0)
  expect_equal(cc$expected, 0)
  expect_true(is.na(cc$oe))
  expect_true(is.na(cc$spi))
})

test_that("a district with no previous year gives no SPI", {
  toy <- direct_toy()
  res <- run_toy(toy$cases, toy$population, first_assessment = 2025,
                 last_assessment = 2025)
  b <- res$summary[res$summary$district == "B", ]
  expect_equal(b$history_years, 0L)
  expect_equal(b$history_check, "no previous year")
  expect_true(is.na(b$history_rate))
  expect_true(is.na(b$expected))
  expect_true(is.na(b$oe))
  expect_true(is.na(b$spi))
})

test_that("one previous year is enough for an SPI", {
  toy <- direct_toy()
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  b <- res$summary[res$summary$district == "B", ]
  expect_equal(b$history_years, 1L)
  expect_equal(b$history_check, "ok")
  expect_true(is.finite(b$spi))
})

test_that("national centring uses only positive expected counts", {
  toy <- direct_toy()
  toy$cases$npafp_cases[toy$cases$district == "C" & toy$cases$year < 2026] <- 0
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  sm <- res$summary
  scored <- sm[is.finite(sm$expected) & sm$expected > 0, ]
  expect_equal(nrow(scored), 4L)
  expect_equal(
    res$national$national_oe,
    sum(scored$observed) / sum(scored$expected)
  )
  expect_equal(sm$spi, sm$oe / res$national$national_oe)
})

test_that("the population check flags a spike and a new district", {
  toy <- direct_toy()
  pop <- toy$population
  pop$population_u15[pop$district == "D" & pop$year == 2024] <- 600000
  res <- run_toy(toy$cases, pop, first_assessment = 2026)
  qc <- res$population_qc
  expect_equal(qc$population_qc[qc$district == "D"], "check_spike")
  expect_equal(qc$population_qc[qc$district == "B"], "check_history")
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
      "Previous rate", "Expected NPAFP cases",
      "District observed / expected", "National observed / expected", "SPI",
      "Population check")
  )
  expect_error(spi_direct_explain(res, "Z"), "No row")
})

test_that("the explanation says when the SPI is not calculated", {
  toy <- direct_toy()
  toy$cases$npafp_cases[toy$cases$district == "C" & toy$cases$year < 2026] <- 0
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  out <- spi_direct_explain(res, "C", print = FALSE)
  val <- function(x) out$value[out$component == x]
  expect_equal(val("Previous NPAFP cases"), "0")
  expect_equal(val("Previous rate"), "0.00")
  expect_equal(val("Expected NPAFP cases"), "0.00")
  expect_equal(val("District observed / expected"), "not calculated")
  expect_equal(val("SPI"), "not calculated")
  expect_equal(val("Reason"), "no previous NPAFP case")
})

test_that("verbose output reports each step", {
  toy <- direct_toy()
  expect_message(
    spi_direct(toy$cases, toy$population, first_assessment = 2026),
    "Reading district-year counts"
  )
})

test_that("the explanation can be translated", {
  skip_if_not_installed("sntutils")
  skip_if_not_installed("gtranslate")
  local_mocked_bindings(
    translate_text_vec = function(text, target_language, ...) {
      paste0(target_language, ":", text)
    },
    .package = "sntutils"
  )
  toy <- direct_toy()
  res <- run_toy(toy$cases, toy$population, first_assessment = 2026)
  en <- spi_direct_explain(res, "B", print = FALSE)
  fr <- spi_direct_explain(res, "B", language = "fr", print = FALSE)
  expect_equal(fr$section, paste0("fr:", en$section))
  expect_equal(fr$component, paste0("fr:", en$component))
  numbers <- grepl("^[0-9 .]+$", en$value)
  expect_equal(fr$value[numbers], en$value[numbers])
  expect_equal(
    fr$value[en$component == "Population check"], "check_history"
  )
  expect_error(spi_direct_explain(res, "B", language = 1), "language")
})
