# Skip only the known INLA binary failure. Package errors must fail the test.
inla_or_skip <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      message <- conditionMessage(e)
      if (grepl("the inla program failed and the maximum number of tries has been reached",
                message, fixed = TRUE)) {
        testthat::skip(paste0("INLA binary unavailable: ", message))
      }
      stop(e)
    }
  )
}

fit_or_skip <- function(...) inla_or_skip(spi_expected(...))
