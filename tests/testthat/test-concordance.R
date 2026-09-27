# spi_concordance() runs off a hand-built district-year spi_index
# (make_spi_dy(), helper-fixtures.R) placed into all four concordance cells.

test_that("spi_concordance classifies all four cells and computes metrics", {
  spi <- make_spi_dy()
  pop <- make_population()
  conc <- spi_concordance(spi, population = pop, verbose = FALSE)

  expect_s3_class(conc, "spi_concordance")
  expect_setequal(
    levels(conc$district_year$concordance),
    c("Neither flagged", "SPI only", "NPAFP only", "Both flagged")
  )
  # every cell realised (two district-years each)
  expect_true(all(table(conc$district_year$concordance) == 2L))

  m <- conc$metrics
  expect_equal(m$n, nrow(conc$district_year))
  expect_true(is.finite(m$pct_agreement))
  expect_true(is.finite(m$cohens_kappa))
  # McNemar is defined here (both off-diagonals non-empty)
  expect_true(is.finite(m$mcnemar_p))
  expect_equal(conc$thresholds$spi, 1)
  # crosstab is a 2x2
  expect_equal(dim(conc$crosstab$counts), c(2L, 2L))
})

test_that("cells use the paper's cut and neutral labels", {
  conc <- spi_concordance(make_spi_dy(), population = make_population(),
    verbose = FALSE)
  expect_identical(levels(conc$district_year$concordance),
    c("Neither flagged", "SPI only", "NPAFP only", "Both flagged"))
  expect_identical(conc$thresholds$spi, 1)
  expect_identical(conc$thresholds$rule, "median")
  expect_true(all(c("n_neither_flagged", "n_spi_only", "n_npafp_only",
    "n_both_flagged") %in% names(conc$metrics)))
})

test_that("the interval rule also needs the 90% upper bound below 1", {
  spi <- make_spi_dy()
  spi$summary$spi_q95[spi$summary$spi_median < 1][1] <- 1.05
  med <- spi_concordance(spi, population = make_population(), verbose = FALSE)
  int <- spi_concordance(spi, population = make_population(),
    spi_rule = "interval", verbose = FALSE)
  expect_lt(sum(int$district_year$spi_flagged),
    sum(med$district_year$spi_flagged))
  expect_true(all(int$district_year$spi_q95[int$district_year$spi_flagged] < 1))
})

test_that("spi_concordance honours thresholds, strata, and case override", {
  spi <- make_spi_dy()
  pop <- make_population()

  # a stricter NPAFP target flips the adequacy calls
  strict <- spi_concordance(spi, population = pop, npafp_target = 20,
                           npafp_multiplier = 100000L, verbose = FALSE)
  expect_true(all(!strict$district_year$npafp_adequate))

  # stratified metrics by year
  by_year <- spi_concordance(spi, population = pop, strata = "year",
                            verbose = FALSE)
  expect_s3_class(by_year$by_stratum, "tbl_df")
  expect_true("year" %in% names(by_year$by_stratum))

  # missing stratum column errors
  expect_error(
    spi_concordance(spi, population = pop, strata = "not_a_col",
                   verbose = FALSE),
    "not in"
  )

  # explicit cases table overrides spi$data
  cases <- spi$data
  cases$count <- cases$count + 5L
  ov <- spi_concordance(spi, cases = cases, population = pop, verbose = FALSE)
  expect_true(all(ov$district_year$count_annual >= 6L))

  # verbose path
  expect_no_error(spi_concordance(spi, population = pop, verbose = TRUE))

  # a cases table keyed by month (not year) derives the year from the date
  cases_m <- tibble::tibble(
    district_id = rep(c("A", "B", "C", "D"), each = 2),
    month = rep(as.Date(c("2015-03-01", "2016-03-01")), times = 4),
    count = 4L
  )
  cm <- spi_concordance(spi, cases = cases_m, population = pop, verbose = FALSE)
  expect_true(all(cm$district_year$count_annual == 4L))
})

test_that("spi_concordance groups cases on the SPI's reading year", {
  spi <- make_spi_dy()
  pop <- make_population()

  # June 2015 and February 2016 are separate calendar years, but one rolling
  # year once the window closes in April: both fall in the year labelled 2016.
  cases_m <- tibble::tibble(
    district_id = rep(c("A", "B", "C", "D"), each = 2),
    month = rep(as.Date(c("2015-06-01", "2016-02-01")), times = 4),
    count = rep(c(3L, 4L), times = 4)
  )

  cal <- spi_concordance(spi, cases = cases_m, population = pop,
                        verbose = FALSE)
  expect_setequal(cal$district_year$year, c(2015L, 2016L))
  expect_equal(
    cal$district_year$count_annual[cal$district_year$year == 2016L],
    rep(4L, 4L)
  )

  rol <- spi_concordance(spi, cases = cases_m, population = pop,
                        year_end_month = 4, verbose = FALSE)
  expect_equal(unique(rol$district_year$year), 2016L)
  expect_equal(rol$district_year$count_annual, rep(7L, 4L))

  expect_error(
    spi_concordance(spi, population = pop, year_end_month = 13,
                   verbose = FALSE),
    "year_end_month"
  )
})

test_that("spi_concordance joins boundary strata (sf and plain frames)", {
  spi <- make_spi_dy(id_col = "adm2_guid")
  pop <- make_population(id_col = "adm2_guid")

  # plain data-frame boundaries: region label joined for stratification
  bnd_df <- tibble::tibble(
    adm2_guid = c("A", "B", "C", "D"),
    region = c("R1", "R1", "R2", "R2")
  )
  conc_df <- spi_concordance(spi, population = pop, id_col = "adm2_guid",
                            boundaries = bnd_df, strata = "region",
                            verbose = FALSE)
  expect_true("region" %in% names(conc_df$district_year))
  expect_equal(nrow(conc_df$by_stratum), 2L)

  skip_if_not_installed("sf")
  bnd_sf <- sf::st_as_sf(
    data.frame(adm2_guid = c("A", "B", "C", "D"), region = "R1",
               x = 1:4, y = 1:4),
    coords = c("x", "y")
  )
  conc_sf <- spi_concordance(spi, population = pop, id_col = "adm2_guid",
                            boundaries = bnd_sf, verbose = FALSE)
  expect_true("region" %in% names(conc_sf$district_year))
})

test_that("spi_concordance validates its inputs", {
  spi <- make_spi_dy()
  pop <- make_population()

  expect_error(spi_concordance(list(), population = pop), "spi_index|inherits")

  # id column absent from the summary
  bad_id <- spi
  names(bad_id$summary)[names(bad_id$summary) == "district_id"] <- "foo"
  expect_error(
    spi_concordance(bad_id, population = pop, id_col = "district_id",
                   verbose = FALSE),
    "lacks id column"
  )

  # not a district-year SPI (no year column)
  no_year <- spi
  no_year$summary$year <- NULL
  no_year$level <- "district_month"
  expect_error(spi_concordance(no_year, population = pop, verbose = FALSE),
               "district-year")

  # population missing the pop column
  expect_error(
    spi_concordance(spi, population = pop[, c("district_id", "year")],
                   verbose = FALSE)
  )

  # no cases anywhere
  no_data <- spi
  no_data$data <- NULL
  expect_error(spi_concordance(no_data, population = pop, verbose = FALSE),
               "no .*cases")

  # cases without a count column
  bad_cases <- tibble::tibble(district_id = "A", year = 2015L)
  expect_error(
    spi_concordance(spi, cases = bad_cases, population = pop, verbose = FALSE),
    "count"
  )
})

test_that("spi_concordance print / summary / as_tibble / plot", {
  spi <- make_spi_dy()
  pop <- make_population()
  conc <- spi_concordance(spi, population = pop, strata = "year",
                         verbose = FALSE)

  expect_no_error(print(conc))
  expect_identical(print(conc), conc)
  expect_no_error(summary(conc))
  expect_s3_class(as_tibble(conc), "tbl_df")

  skip_if_not_installed("ggplot2")
  expect_s3_class(plot(conc), "ggplot")
})

test_that("spi_concordance_maps renders the three-panel figure", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  spi <- make_spi_dy(id_col = "adm2_guid")
  pop <- make_population(id_col = "adm2_guid")
  conc <- spi_concordance(spi, population = pop, id_col = "adm2_guid",
                         verbose = FALSE)

  sq <- function(cx, cy) {
    sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0) + cx, c(0, 0, 1, 1, 0) + cy)))
  }
  bnd <- sf::st_sf(
    adm2_guid = c("A", "B", "C", "D"),
    adm1_name = c("P1", "P1", "P2", "P2"),
    geometry = sf::st_sfc(sq(0, 0), sq(1, 0), sq(0, 1), sq(1, 1))
  )

  p <- spi_concordance_maps(conc, bnd)               # default year + provinces
  expect_s3_class(p, "patchwork")
  p2 <- spi_concordance_maps(conc, bnd, year = 2015L, provinces = FALSE)
  expect_s3_class(p2, "patchwork")

  # a year with no rows aborts
  expect_error(
    spi_concordance_maps(conc, bnd, year = 1999L), "no district-year"
  )
})

test_that("concordance statistical helpers cover their edge cases", {
  # Cohen's kappa: empty input and degenerate p_exp = 1 both return NA
  expect_true(is.na(spi:::.cohens_kappa(integer(0), integer(0))))
  expect_true(is.na(spi:::.cohens_kappa(c(1L, 1L), c(1L, 1L))))
  expect_true(is.finite(spi:::.cohens_kappa(c(0L, 1L, 0L, 1L),
                                                  c(0L, 1L, 1L, 0L))))

  # McNemar: no off-diagonal discordance -> NA; otherwise a p-value
  lv <- c("Neither flagged", "SPI only", "NPAFP only", "Both flagged")
  concordant <- tibble::tibble(concordance = factor("Neither flagged", lv))
  expect_true(is.na(spi:::.mcnemar_p(concordant)))
  mixed <- tibble::tibble(
    concordance = factor(c("SPI only", "NPAFP only", "NPAFP only"), lv)
  )
  expect_true(is.finite(spi:::.mcnemar_p(mixed)))
})

test_that("interval-label and palette helpers cover their branches", {
  labs <- spi:::.interval_labels(c(-Inf, 1, 2, Inf))
  expect_equal(length(labs), 3L)
  expect_match(labs[1], "-Inf")
  expect_match(labs[length(labs)], "Inf\\]$")

  # exact-name match
  pal <- c(a = "#111111", b = "#222222")
  expect_equal(spi:::.align_palette(pal, c("a", "b")), pal[c("a", "b")])
  # equal length, names differ -> positional
  pos <- spi:::.align_palette(pal, c("x", "y"))
  expect_equal(names(pos), c("x", "y"))
  # length mismatch -> interpolated ramp
  ramp <- spi:::.align_palette(pal, c("x", "y", "z"))
  expect_equal(length(ramp), 3L)
})
