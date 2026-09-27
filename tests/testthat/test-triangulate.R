# spi_triangulate() crosses the shipped field guide against the detection
# channels. These tests stub a minimal spi_field_guide for the join /
# lag / classification logic, and use the shipped synth_field_guide +
# synth_surveillance$detections for the end-to-end and renderer checks.

# minimal field-guide stub: only what spi_triangulate() reads.
make_fg <- function(dy, read_year = 2020L) {
  structure(
    list(
      district_year = tibble::as_tibble(dy),
      read_year = as.integer(read_year),
      thresholds = list(spi = 1, npafp = 3),
      id_col = "adm2_guid"
    ),
    class = "spi_field_guide"
  )
}

test_that("all ten triangulation classes are reachable", {
  ids <- sprintf("D%02d", 1:10)
  dy <- tibble::tibble(
    adm2_guid = ids,
    year = 2020L,
    verdict = factor(
      c("Review priority", "Review priority", "Review priority",
        "Monitor", "Monitor", "Monitor",
        "No SPI indication", "No SPI indication", "No SPI indication",
        "No SPI indication"),
      levels = .FG_VERDICT_LEVELS
    )
  )
  # each judgement crossed against ES positive / clear / no site, in that
  # order, with one AFP hit at the end to reach "detected"
  det <- tibble::tibble(
    adm2_guid = ids,
    year = 2020L,
    afp_detected = c(rep(FALSE, 9), TRUE),
    es_detected = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
                    FALSE, FALSE),
    es_covered = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE,
                   FALSE, TRUE)
  )
  tri <- spi_triangulate(make_fg(dy), det, verbose = FALSE)
  cls <- tri$district_year
  got <- as.character(cls$triangulation[match(ids, cls$adm2_guid)])

  expect_equal(got, c(
    "confirmed blindspot", "priority, ES clear", "blind, unverified",
    "monitor, ES positive", "monitor, ES clear", "monitor, unverified",
    "no indication, ES positive", "corroborated clear",
    "uncorroborated clear", "detected"
  ))
  # every class realised, and no unclassified rows
  expect_setequal(as.character(unique(cls$triangulation)), .tri_levels)
  expect_false(anyNA(cls$triangulation))

  # priority rolls up as documented
  pr <- as.character(cls$priority[match(ids, cls$adm2_guid)])
  expect_equal(pr[1], "high")      # confirmed blindspot
  expect_equal(pr[2], "medium")    # priority, ES clear
  expect_equal(pr[3], "high")      # blind, unverified
  expect_equal(pr[4], "high")      # monitor, ES positive
  expect_equal(pr[5], "medium")    # monitor, ES clear
  expect_equal(pr[6], "medium")    # monitor, unverified
  expect_equal(pr[7], "medium")    # no indication, ES positive
  expect_equal(pr[8], "low")       # corroborated clear
  expect_equal(pr[10], "resolved") # detected

  # flag_preceded only set for the AFP-detected row (No SPI indication ->
  # FALSE)
  fp <- cls$flag_preceded[match(ids, cls$adm2_guid)]
  expect_true(all(is.na(fp[1:9])))
  expect_false(fp[10])
})

test_that("a Monitor judgement preceding an AFP detection counts as forewarned", {
  # Monitor is a shortfall the guide did report, so a detection in a district
  # it had already marked is not an unforewarned detection
  dy <- tibble::tibble(
    adm2_guid = c("D1", "D2"),
    year = 2020L,
    verdict = factor(c("Monitor", "No SPI indication"),
                     levels = .FG_VERDICT_LEVELS)
  )
  det <- tibble::tibble(
    adm2_guid = c("D1", "D2"), year = 2020L,
    afp_detected = TRUE, es_detected = FALSE, es_covered = TRUE
  )
  cls <- spi_triangulate(make_fg(dy), det, verbose = FALSE)$district_year
  fp <- cls$flag_preceded[match(c("D1", "D2"), cls$adm2_guid)]
  expect_true(fp[1])
  expect_false(fp[2])
})

test_that("detection_lag aligns verdict[t] with detection[t+lag]", {
  dy <- tibble::tibble(
    adm2_guid = "D1",
    year = c(2019L, 2020L),
    verdict = factor(c("Review priority", "No SPI indication"),
                     levels = .FG_VERDICT_LEVELS)
  )
  # one ES-positive detection in 2020
  det <- tibble::tibble(
    adm2_guid = "D1", year = 2020L,
    afp_detected = FALSE, es_detected = TRUE, es_covered = TRUE
  )

  tri0 <- spi_triangulate(make_fg(dy, 2020L), det, detection_lag = 0L,
                         verbose = FALSE)
  tri1 <- spi_triangulate(make_fg(dy, 2020L), det, detection_lag = 1L,
                         verbose = FALSE)

  cls0 <- tri0$district_year
  cls1 <- tri1$district_year

  # lag 0: the 2020 no-indication judgement meets the 2020 ES positive
  expect_equal(as.character(cls0$triangulation[cls0$year == 2020L]),
               "no indication, ES positive")
  # lag 1: the 2019 review priority now meets the 2020 ES positive
  expect_equal(as.character(cls1$triangulation[cls1$year == 2019L]),
               "confirmed blindspot")
  # and 2020 loses its aligned detection -> read as no site
  expect_equal(as.character(cls1$triangulation[cls1$year == 2020L]),
               "uncorroborated clear")
})

test_that("district-years absent from detections read as no detection", {
  dy <- tibble::tibble(
    adm2_guid = c("D1", "D2"),
    year = 2020L,
    verdict = factor(c("Review priority", "No SPI indication"),
                     levels = .FG_VERDICT_LEVELS)
  )
  # only D1 has a detection row; D2 is absent
  det <- tibble::tibble(
    adm2_guid = "D1", year = 2020L,
    afp_detected = FALSE, es_detected = TRUE, es_covered = TRUE
  )
  tri <- spi_triangulate(make_fg(dy), det, verbose = FALSE)
  cls <- tri$district_year
  expect_equal(as.character(cls$triangulation[cls$adm2_guid == "D1"]),
               "confirmed blindspot")
  expect_equal(as.character(cls$triangulation[cls$adm2_guid == "D2"]),
               "uncorroborated clear")
})

test_that("spi_triangulate validates its inputs", {
  dy <- tibble::tibble(
    adm2_guid = "D1", year = 2020L,
    verdict = factor("Review priority", levels = .FG_VERDICT_LEVELS)
  )
  det <- tibble::tibble(
    adm2_guid = "D1", year = 2020L,
    afp_detected = FALSE, es_detected = FALSE, es_covered = FALSE
  )

  expect_error(spi_triangulate(list(), det))                 # not a field guide
  expect_error(spi_triangulate(make_fg(dy), 1L))             # det not a df

  # missing a detection column
  expect_error(
    spi_triangulate(make_fg(dy), det[, c("adm2_guid", "year", "afp_detected")]),
    "missing"
  )
  # missing the verdict column on the guide
  bad_fg <- make_fg(dy)
  bad_fg$district_year$verdict <- NULL
  expect_error(spi_triangulate(bad_fg, det), "missing")

  # more than one detection row per district-year
  dup <- dplyr::bind_rows(det, det)
  expect_error(spi_triangulate(make_fg(dy), dup), ">1 row")
})

test_that("spi_triangulate runs end to end on the shipped bundle", {
  tri <- spi_triangulate(synth_field_guide, synth_surveillance$detections,
                        detection_lag = 1L, verbose = FALSE)

  expect_s3_class(tri, "spi_triangulation")
  # row count preserved through the join
  expect_equal(nrow(tri$district_year), nrow(synth_field_guide$district_year))
  expect_s3_class(tri$district_year$triangulation, "factor")
  expect_equal(levels(tri$district_year$triangulation), .tri_levels)
  expect_false(anyNA(tri$district_year$triangulation))
  # focal is the read-year slice
  expect_true(all(tri$focal$year == tri$read_year))
  # the legend covers all ten classes
  expect_setequal(tri$reference$class, .tri_levels)
})

test_that("print returns invisibly and the renderers produce objects", {
  tri <- spi_triangulate(synth_field_guide, synth_surveillance$detections,
                        verbose = FALSE)

  expect_no_error(print(tri))
  expect_identical(print(tri), tri) # returns invisibly

  skip_if_not_installed("gt")
  expect_s3_class(spi_triangulate_table(tri, engine = "gt"), "gt_tbl")

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  expect_s3_class(spi_triangulate_table(tri, engine = "flextable"), "flextable")
})

test_that("spi_triangulate_map returns a ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")
  tri <- spi_triangulate(synth_field_guide, synth_surveillance$detections,
                        verbose = FALSE)

  p <- spi_triangulate_map(tri, synth_surveillance$boundaries, year = 2020L)
  expect_s3_class(p, "ggplot")
  p2 <- spi_triangulate_map(tri, synth_surveillance$boundaries, by = "priority")
  expect_s3_class(p2, "ggplot")
})
