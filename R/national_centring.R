# The national observed-to-expected ratio per period: summed observed over
# summed posterior-median expected, on the rows that carry a finite, positive
# expected count. `period_col` is character(0) for a single ratio.
# @noRd
.national_oe <- function(tbl, observed_col, expected_col, period_col) {
  ok <- is.finite(tbl[[observed_col]]) &
    is.finite(tbl[[expected_col]]) & tbl[[expected_col]] > 0
  tbl[ok, , drop = FALSE] |>
    dplyr::summarise(
      national_observed = sum(.data[[observed_col]]),
      national_expected = sum(.data[[expected_col]]),
      national_oe = .data$national_observed / .data$national_expected,
      districts = dplyr::n(),
      .by = dplyr::all_of(period_col)
    )
}

# Divide each row's SPI draws and summaries by its period's national ratio, so
# a country-wide change in reporting does not move every district the same
# way. A period with no detections has no ratio to divide by; its rows are NA.
# @noRd
.centre_spi <- function(spi_obj, period_col) {
  sm <- spi_obj$summary
  national <- .national_oe(sm, "observed", "expected_total", period_col)
  ratio <- if (length(period_col) == 0L) {
    rep(national$national_oe, nrow(sm))
  } else {
    national$national_oe[match(sm[[period_col]], national[[period_col]])]
  }
  if (any(ratio == 0, na.rm = TRUE)) {
    cli::cli_warn(
      "{sum(national$national_oe == 0, na.rm = TRUE)} period{?s} had no \\
       detections nationally; {?its/their} centred SPI is NA."
    )
  }
  ratio[ratio == 0] <- NA_real_
  spi_cols <- c(
    "spi_median", "spi_mean", "spi_q05", "spi_q10", "spi_q90", "spi_q95"
  )
  sm[spi_cols] <- lapply(sm[spi_cols], \(x) x / ratio)
  sm$national_oe <- ratio
  list(
    draws = sweep(spi_obj$draws, 2, ratio, "/"),
    summary = sm,
    national = national
  )
}
