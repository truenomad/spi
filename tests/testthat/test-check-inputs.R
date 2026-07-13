# bs_check_inputs() -- graded pre-flight reconciliation of the three input
# tables. Pure data checking, so every test runs without INLA. Most tests
# start from a clean fixture and mutate one thing, asserting that issue's code
# and severity in isolation; the mixed-severity test guards the "one report,
# everything at once" contract that single-mutation tests can't.

# --- raw-input builders ----------------------------------------------------
ic_ids <- sprintf("D%02d", 1:6)
ic_months <- seq(as.Date("2020-01-01"), by = "month", length.out = 6)

ic_cases <- function(ids = ic_ids, months = ic_months, count = 1L) {
  tibble::tibble(
    district_id = rep(ids, times = length(months)),
    month = rep(months, each = length(ids)),
    count = as.integer(count)
  )
}

ic_pop <- function(ids = ic_ids, months = ic_months, pop = 1e5) {
  tibble::tibble(
    district_id = rep(ids, times = length(months)),
    month = rep(months, each = length(ids)),
    pop_u15 = pop
  )
}

# a minimal valid sf of unit squares, one per id; flip `invalid_last` to make
# the final polygon a self-intersecting bow-tie
ic_sf <- function(ids = ic_ids, invalid_last = FALSE) {
  ring <- function(i) {
    if (invalid_last && i == length(ids)) {
      rbind(c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0)) + i
    } else {
      rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)) + i
    }
  }
  geom <- sf::st_sfc(lapply(seq_along(ids), function(i) {
    sf::st_polygon(list(ring(i)))
  }))
  sf::st_sf(district_id = ids, geometry = geom)
}

# pull the row for a given issue code, or NULL
ic_issue <- function(rpt, code) {
  hit <- rpt$issues[rpt$issues$code == code, ]
  if (nrow(hit) == 0) NULL else hit
}

# --- happy path ------------------------------------------------------------

test_that("clean inputs produce no errors or warnings", {
  rpt <- bs_check_inputs(
    ic_cases(), ic_pop(), make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  expect_s3_class(rpt, "blindspot_input_check")
  expect_true(rpt$ok)
  expect_equal(rpt$n_error, 0)
  expect_equal(rpt$n_warning, 0)
})

test_that("print method summarises a clean and a dirty report", {
  clean <- bs_check_inputs(
    ic_cases(), ic_pop(), make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  clean_out <- cli::cli_fmt(print(clean))
  expect_true(any(grepl("passed", clean_out)))

  cases <- ic_cases()
  cases$count[1] <- -1L
  dirty <- bs_check_inputs(
    cases, ic_pop(), make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  dirty_out <- cli::cli_fmt(print(dirty))
  expect_true(any(grepl("error", dirty_out)))
})

# --- error-level (block the fit) -------------------------------------------

test_that("case id absent from the shapefile is an error", {
  cases <- rbind(ic_cases(), ic_cases(ids = "ZZZ"))
  rpt <- bs_check_inputs(
    cases, ic_pop(), make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  iss <- ic_issue(rpt, "cases_id_not_in_shapefile")
  expect_false(rpt$ok)
  expect_equal(iss$severity, "error")
  expect_true("ZZZ" %in% iss$ids[[1]])
})

test_that("negative counts are an error", {
  cases <- ic_cases()
  cases$count[c(1, 5)] <- -1L
  rpt <- bs_check_inputs(
    cases, ic_pop(), make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  expect_equal(ic_issue(rpt, "cases_negative")$severity, "error")
  expect_false(rpt$ok)
})

test_that("negative population is an error", {
  pop <- ic_pop()
  pop$pop_u15[1] <- -5
  rpt <- bs_check_inputs(
    ic_cases(), pop, make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  expect_equal(ic_issue(rpt, "pop_negative")$severity, "error")
  expect_false(rpt$ok)
})

test_that("missing required columns are errors on each table", {
  rpt_c <- bs_check_inputs(
    ic_cases()[, c("district_id", "month")], ic_pop(),
    make_nb(ic_ids, island_last = FALSE), verbose = FALSE
  )
  expect_equal(ic_issue(rpt_c, "cases_missing_cols")$severity, "error")

  rpt_p <- bs_check_inputs(
    ic_cases(), ic_pop()[, c("district_id", "month")],
    make_nb(ic_ids, island_last = FALSE), verbose = FALSE
  )
  expect_equal(ic_issue(rpt_p, "pop_missing_denominator")$severity, "error")
})

test_that("duplicate district-months are an error", {
  cases <- rbind(ic_cases(), ic_cases()[1, ])
  rpt <- bs_check_inputs(
    cases, ic_pop(), make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  expect_equal(ic_issue(rpt, "cases_duplicates")$severity, "error")
})

test_that("wrong month class and non-numeric count are errors", {
  cases <- ic_cases()
  cases$month <- as.character(cases$month)
  rpt_m <- bs_check_inputs(
    cases, ic_pop(), make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  expect_equal(ic_issue(rpt_m, "cases_month_class")$severity, "error")

  cases2 <- ic_cases()
  cases2$count <- as.character(cases2$count)
  rpt_n <- bs_check_inputs(
    cases2, ic_pop(), make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  expect_equal(ic_issue(rpt_n, "cases_count_type")$severity, "error")
})

# --- warning-level (fit proceeds, flagged) ---------------------------------

test_that("zero population warns, names the district, and does not block", {
  pop <- ic_pop()
  pop$pop_u15[pop$district_id == "D03"] <- 0
  rpt <- bs_check_inputs(
    ic_cases(), pop, make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  iss <- ic_issue(rpt, "pop_zero")
  expect_equal(iss$severity, "warning")
  expect_true("D03" %in% iss$ids[[1]])
  expect_true(rpt$ok)
})

test_that("a dropped district-month is reported as a gap with the right row", {
  cases <- ic_cases()
  drop <- cases$district_id == "D02" & cases$month == ic_months[3]
  cases <- cases[!drop, ]
  rpt <- bs_check_inputs(
    cases, ic_pop(), make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  expect_equal(ic_issue(rpt, "panel_gaps")$severity, "warning")
  expect_equal(nrow(rpt$gaps), 1)
  expect_equal(rpt$gaps$district_id, "D02")
  expect_equal(rpt$gaps$month, ic_months[3])
  expect_true(rpt$ok)
})

test_that("partial population coverage warns", {
  pop <- ic_pop()
  pop <- pop[!(pop$district_id == "D04"), ]
  rpt <- bs_check_inputs(
    ic_cases(), pop, make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  expect_equal(ic_issue(rpt, "pop_coverage")$severity, "warning")
})

test_that("shapefile district with no case rows warns", {
  nb <- make_nb(c(ic_ids, "D99"), island_last = FALSE)
  rpt <- bs_check_inputs(ic_cases(), ic_pop(), nb, verbose = FALSE)
  iss <- ic_issue(rpt, "shapefile_no_cases")
  expect_equal(iss$severity, "warning")
  expect_true("D99" %in% iss$ids[[1]])
})

test_that("invalid geometry is reported, not repaired", {
  suppressWarnings(
    rpt <- bs_check_inputs(
      ic_cases(), ic_pop(), ic_sf(invalid_last = TRUE),
      verbose = FALSE
    )
  )
  expect_equal(ic_issue(rpt, "geometry_invalid")$severity, "warning")
  expect_true(rpt$ok)
})

test_that("covariate id off the panel warns", {
  cov <- tibble::tibble(district_id = c(ic_ids, "COV9"), dtp3 = 80)
  rpt <- bs_check_inputs(
    ic_cases(), ic_pop(), make_nb(ic_ids, island_last = FALSE),
    covariates = cov, verbose = FALSE
  )
  iss <- ic_issue(rpt, "covariate_off_panel")
  expect_equal(iss$severity, "warning")
  expect_true("COV9" %in% iss$ids[[1]])
})

# --- mixed severity (guards the graded contract) ---------------------------

test_that("one error and two warnings all surface, routing on the error", {
  cases <- ic_cases()
  cases$count[1] <- -1L                                   # error
  drop <- cases$district_id == "D05" & cases$month == ic_months[4]
  cases <- cases[!drop, ]                                 # warning: panel gap
  pop <- ic_pop()
  pop$pop_u15[pop$district_id == "D06"] <- 0              # warning: zero pop

  rpt <- bs_check_inputs(
    cases, pop, make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )

  codes <- rpt$issues$code
  expect_true(all(
    c("cases_negative", "panel_gaps", "pop_zero") %in% codes
  ))
  expect_equal(rpt$n_error, 1)
  expect_gte(rpt$n_warning, 2)
  expect_false(rpt$ok)
})

# --- edge cases ------------------------------------------------------------

test_that("an empty cases table yields a clean error, not an internal crash", {
  empty <- ic_cases()[0, ]
  rpt <- bs_check_inputs(
    empty, ic_pop(), make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  expect_equal(ic_issue(rpt, "cases_empty")$severity, "error")
  expect_false(rpt$ok)
})

test_that("a single district-month panel has no spurious gap warning", {
  one <- ic_cases(ids = "D01", months = ic_months[1])
  pop <- ic_pop(ids = "D01", months = ic_months[1])
  rpt <- bs_check_inputs(
    one, pop, make_nb("D01", island_last = FALSE),
    verbose = FALSE
  )
  expect_null(ic_issue(rpt, "panel_gaps"))
  expect_equal(nrow(rpt$gaps), 0)
})

test_that("annual-grain population validates", {
  pop <- tibble::tibble(
    district_id = rep(ic_ids, times = 1),
    year = 2020L,
    pop_u15 = 1e5
  )
  rpt <- bs_check_inputs(
    ic_cases(), pop, make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  expect_true(rpt$ok)
  expect_null(ic_issue(rpt, "pop_coverage"))
})

test_that("a non-default id_col is honoured across all three tables", {
  cases <- ic_cases()
  names(cases)[names(cases) == "district_id"] <- "adm2_guid"
  pop <- ic_pop()
  names(pop)[names(pop) == "district_id"] <- "adm2_guid"
  sf_obj <- ic_sf()
  names(sf_obj)[names(sf_obj) == "district_id"] <- "adm2_guid"

  rpt <- bs_check_inputs(
    cases, pop, sf_obj, id_col = "adm2_guid", verbose = FALSE
  )
  expect_true(rpt$ok)
})

test_that("sf and nb shapefile inputs validate identically", {
  rpt_sf <- bs_check_inputs(ic_cases(), ic_pop(), ic_sf(), verbose = FALSE)
  rpt_nb <- bs_check_inputs(
    ic_cases(), ic_pop(), make_nb(ic_ids, island_last = FALSE),
    verbose = FALSE
  )
  expect_equal(rpt_sf$ok, rpt_nb$ok)
  expect_equal(rpt_sf$n_error, rpt_nb$n_error)
})

test_that("a bad shapefile class is a hard argument error", {
  expect_error(
    bs_check_inputs(ic_cases(), ic_pop(), shapefile = list(), verbose = FALSE),
    "sf"
  )
})
