test_that("the preparation vignette creates the inputs used by its direct examples", {
  path <- test_path("..", "..", "vignettes", "spi-data-preparation.Rmd")
  if (!file.exists(path)) {
    path <- system.file("doc", "spi-data-preparation.Rmd", package = "spi")
  }
  skip_if_not(file.exists(path), "Vignette source is not installed")
  lines <- readLines(path, warn = FALSE)
  example <- new.env(parent = environment())
  run_chunk <- function(label) {
    start <- grep(paste0("^```\\{r ", label, "[,}]"), lines)
    stopifnot(length(start) == 1L)
    end <- which(seq_along(lines) > start & lines == "```")[1]
    invisible(capture.output(
      eval(parse(text = lines[seq.int(start + 1L, end - 1L)]), example)
    ))
  }

  run_chunk("annual-example")
  run_chunk("standard")
  combined <- example$spi_results
  expect_s3_class(combined, "spi_direct")
  expect_true(all(is.finite(combined$summary$spi)))

  run_chunk("separate-pop")
  expect_equal(example$spi_results$summary, combined$summary)

  run_chunk("mapping")
  expect_equal(example$spi_results$summary$spi, combined$summary$spi)
  expect_equal(example$spi_results$metadata$id_col, "adm2_guid")
})
