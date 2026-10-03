test_that("the preparation vignette creates the inputs used by its simple examples", {
  # the source keeps the chunk labels; it is not part of the built package
  path <- test_path("..", "..", "vignettes", "spi-data-preparation.Rmd.orig")
  skip_if_not(file.exists(path), "Vignette source is not available")
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
  expect_s3_class(combined, "spi_simple")
  ok <- combined$summary$history_check == "ok"
  expect_true(all(is.finite(combined$summary$spi[ok])))
  expect_true(all(is.na(combined$summary$spi[!ok])))

  run_chunk("separate-pop")
  expect_equal(example$spi_results$summary, combined$summary)

  run_chunk("mapping")
  expect_equal(example$spi_results$summary$spi, combined$summary$spi)
  expect_equal(example$spi_results$metadata$id_col, "adm2_guid")
})
