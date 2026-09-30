# Skip known INLA executable failures. Package errors must fail the test.
inla_or_skip <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      message <- conditionMessage(e)
      binary_errors <- c(
        "the inla program failed and the maximum number of tries has been reached",
        "INLA installation error; no such file"
      )
      if (any(vapply(binary_errors, grepl, logical(1),
                     x = message, fixed = TRUE))) {
        testthat::skip(paste0("INLA binary unavailable: ", message))
      }
      stop(e)
    }
  )
}

fit_or_skip <- function(...) inla_or_skip(spi_expected(...))
