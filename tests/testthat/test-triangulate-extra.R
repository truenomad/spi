# Additional spi_triangulate() coverage: the verbose narration, the
# flag-coercion helper, the table file-save + no-rows paths, and the
# by = "triangulation" map branch. These run off the shipped
# synth_field_guide + synth_surveillance, so no INLA fit is needed.

test_that("spi_triangulate verbose path narrates the read", {
  expect_no_error(
    spi_triangulate(synth_field_guide, synth_surveillance$detections,
                   detection_lag = 1L, verbose = TRUE)
  )
})

test_that("spi_triangulate coerces numeric and character detection flags", {
  det <- synth_surveillance$detections
  det$afp_detected <- as.integer(det$afp_detected)          # 0/1 numeric
  det$es_detected <- ifelse(det$es_detected, "yes", "no")   # character
  det$es_covered <- ifelse(det$es_covered, "TRUE", "FALSE")

  tri <- spi_triangulate(synth_field_guide, det, verbose = FALSE)
  expect_s3_class(tri, "spi_triangulation")
  expect_false(anyNA(tri$district_year$triangulation))
})

test_that(".tri_lgl and .tri_priority_fill cover their branches", {
  expect_equal(spi:::.tri_lgl(c(0L, 2L, -1L)), c(FALSE, TRUE, FALSE))
  expect_equal(spi:::.tri_lgl(c("yes", "no", "pos")),
               c(TRUE, FALSE, TRUE))
  expect_equal(spi:::.tri_lgl(c(TRUE, FALSE)), c(TRUE, FALSE))

  fills <- spi:::.tri_priority_fill(
    factor(c("high", "medium", "low", "resolved"),
           levels = spi:::.tri_priority_levels)
  )
  expect_equal(unname(fills), c("warm", "amber", "cool", "none"))
})

test_that("spi_triangulate_table saves to file and rejects empty years", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  tri <- spi_triangulate(synth_field_guide, synth_surveillance$detections,
                        verbose = FALSE)

  f <- withr::local_tempfile(fileext = ".docx")
  spi_triangulate_table(tri, engine = "flextable", file = f)
  expect_true(file.exists(f) && file.info(f)$size > 0)

  expect_error(
    spi_triangulate_table(tri, engine = "flextable", year = 1990L),
    "no district-year rows"
  )
})

test_that("spi_triangulate_map draws the full triangulation-class panel", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")
  tri <- spi_triangulate(synth_field_guide, synth_surveillance$detections,
                        verbose = FALSE)

  p <- spi_triangulate_map(tri, synth_surveillance$boundaries,
                          by = "triangulation", year = 2020L)
  expect_s3_class(p, "ggplot")

  # a year absent from the panel aborts
  expect_error(
    spi_triangulate_map(tri, synth_surveillance$boundaries, year = 1990L),
    "no district-year rows"
  )
})
