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

# Divide each row's SPI draws by its period's national ratio within the same
# posterior draw, so a country-wide change in reporting does not move every
# district the same way, and uncertainty shared by all districts in a period
# cancels before the summaries are taken. `spi_obj$expected` holds the summed
# expected draws behind `spi_obj$draws`. The national table reports summed
# observed over the median summed expected count. A period with no detections
# has no ratio to divide by; its rows are NA.
# @noRd
.centre_spi <- function(spi_obj, period_col) {
  sm <- spi_obj$summary
  expected <- spi_obj$expected
  national <- .national_oe(sm, "observed", "expected_total", period_col)
  key <- if (length(period_col) == 0L) {
    rep(1L, nrow(sm))
  } else {
    match(sm[[period_col]], national[[period_col]])
  }
  ok <- is.finite(sm$observed) &
    is.finite(sm$expected_total) & sm$expected_total > 0

  # summed expected count per draw and period, over the rows in the ratio
  nat_expected <- vapply(
    seq_len(nrow(national)),
    \(k) matrixStats::rowSums2(expected, cols = which(ok & key %in% k)),
    numeric(nrow(expected))
  )
  dim(nat_expected) <- c(nrow(expected), nrow(national))
  nat_draws <- sweep(1 / nat_expected, 2, national$national_observed, "*")

  national$national_expected <- matrixStats::colMedians(nat_expected)
  national$national_oe <- national$national_observed /
    national$national_expected
  zero <- national$national_observed == 0
  if (any(zero)) {
    cli::cli_warn(
      "{sum(zero)} period{?s} had no detections nationally; \\
       {?its/their} centred SPI is NA."
    )
  }
  nat_draws[, zero] <- NA_real_

  draws <- spi_obj$draws / nat_draws[, key, drop = FALSE]
  spi_cols <- c(
    "spi_median", "spi_mean", "spi_q05", "spi_q10", "spi_q90", "spi_q95"
  )
  sm[spi_cols] <- .spi_summarise(expected, draws)[spi_cols]
  na_row <- is.na(key) | key %in% which(zero)
  sm[na_row, spi_cols] <- NA_real_
  sm$national_oe <- national$national_oe[key]
  sm$national_oe[na_row] <- NA_real_
  list(draws = draws, summary = sm, national = national)
}
