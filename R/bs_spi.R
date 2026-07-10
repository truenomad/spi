#' Compute the Surveillance Performance Index (SPI)
#'
#' @description
#' Computes posterior draws of the Surveillance Performance Index, the
#' ratio of observed to model-expected detections, with full uncertainty
#' propagation. SPI ~= 1 indicates detection matches expectation; SPI < 1
#' indicates underdetection (a surveillance blind spot); SPI > 1 indicates
#' over-detection or genuine excess.
#'
#' @param expected Object of class `blindspot_expected` returned by
#'   [bs_expected()].
#' @param cases Optional tibble with the district identifier column (matching
#'   `expected$id_col`), `month` (Date), and `count`. If NULL (default), uses
#'   the observed counts already in the expected model fit.
#' @param level Character. Aggregation level: "district_month" (raw, highest
#'   temporal resolution), "district_quarter" (calendar-quarter SPI per
#'   district), "district_year" (annual SPI per district, default),
#'   "district_total" (single SPI per district over the full study period).
#' @param min_expected Numeric. Districts or district-periods with total
#'   expected count below this threshold are flagged as low-information. SPI
#'   is still computed but unreliable. Default: 1.
#' @param verbose Logical. Progress messages via cli. Default: TRUE.
#'
#' @return Object of class `blindspot_spi`. A list containing:
#' \describe{
#'   \item{draws}{Matrix `[n_draws x n_groups]` of posterior SPI samples.}
#'   \item{summary}{Tibble with grouping variables and SPI summaries (median,
#'     mean, q05, q10, q90, q95) per district or district-period.}
#'   \item{level}{Aggregation level used.}
#'   \item{low_information}{Tibble of districts or district-periods flagged
#'     as low-information.}
#'   \item{totals}{Tibble of overall observed and expected totals.}
#'   \item{id_col}{The id column name, echoed for downstream use.}
#'   \item{call}{Matched call.}
#' }
#'
#' @details
#' For aggregated levels, observed counts are summed within each grouping,
#' expected counts are summed within each grouping per posterior draw, and
#' the ratio is computed per draw. This preserves the joint uncertainty in
#' the expected denominator.
#'
#' @seealso [bs_expected()], [bs_concordance()]
#' @family blindspot core functions
#'
#' @export
#' @examples
#' \dontrun{
#' fit <- bs_expected(
#'   cases = cases,
#'   population = pop_u15,
#'   adjacency = adj,
#'   id_col = "adm2_guid",
#'   season = "harmonic",
#'   overdispersion = "nb"
#' )
#'
#' spi_dy <- bs_spi(expected = fit, level = "district_year")
#' print(spi_dy)
#' }
bs_spi <- function(
  expected,
  cases = NULL,
  level = c(
    "district_year",
    "district_month",
    "district_quarter",
    "district_total"
  ),
  min_expected = 1,
  verbose = TRUE
) {
  # --- check required packages --------------------------
  .check_pkg(
    c("dplyr", "tibble", "lubridate", "matrixStats", "cli"),
    reason = "to compute the surveillance performance index"
  )

  # --- validate inputs ----------------------------------
  stopifnot(
    inherits(expected, "blindspot_expected"),
    is.numeric(min_expected),
    min_expected >= 0
  )
  level <- match.arg(level)

  if (is.null(expected$draws)) {
    cli::cli_abort(
      "{.code expected$draws} is NULL. Refit with \\
       {.code keep_draws = TRUE} to compute SPI."
    )
  }

  id_col <- if (is.null(expected$id_col)) "district_id" else expected$id_col

  # --- align observed counts to draws column ordering ---
  if (verbose) {
    cli::cli_progress_step("Aligning observed counts")
  }
  draws <- expected$draws
  fit_data <- expected$data |>
    dplyr::mutate(col_idx = dplyr::row_number())

  if (is.null(cases)) {
    observed <- fit_data$count
  } else {
    if (!id_col %in% names(cases)) {
      cli::cli_abort(
        "{.arg cases} must contain the id column {.val {id_col}}."
      )
    }
    observed <- fit_data |>
      dplyr::select(dplyr::all_of(c(id_col, "month"))) |>
      dplyr::left_join(
        cases |> dplyr::select(dplyr::all_of(c(id_col, "month", "count"))),
        by = c(id_col, "month")
      ) |>
      dplyr::pull("count")

    n_missing <- sum(is.na(observed))
    if (n_missing > 0) {
      n_missing_s <- format(n_missing, big.mark = ",")
      month_phrase <- ngettext(
        n_missing,
        "district-month is",
        "district-months are"
      )
      cli::cli_alert_warning(
        "{n_missing_s} {month_phrase} missing observed counts \\
         (treated as 0)."
      )
      observed[is.na(observed)] <- 0
    }
  }

  # --- compute SPI at requested level -------------------
  if (verbose) {
    cli::cli_progress_step("Computing SPI at {.val {level}} level")
  }
  spi_obj <- switch(
    level,
    district_month = .spi_district_month(draws, observed, fit_data, id_col),
    district_quarter = .spi_district_quarter(
      draws, observed, fit_data, id_col
    ),
    district_year = .spi_district_year(draws, observed, fit_data, id_col),
    district_total = .spi_district_total(draws, observed, fit_data, id_col)
  )

  # --- low-information flag -----------------------------
  low_info <- spi_obj$summary |>
    dplyr::filter(.data$expected_total < min_expected)

  # --- totals -------------------------------------------
  totals <- tibble::tibble(
    total_observed = sum(observed),
    total_expected_median = round(
      stats::median(matrixStats::rowSums2(draws)), 1
    ),
    n_groups = nrow(spi_obj$summary),
    n_low_information = nrow(low_info)
  )

  # --- assemble output ----------------------------------
  result <- structure(
    list(
      draws = spi_obj$draws,
      summary = spi_obj$summary,
      level = level,
      low_information = low_info,
      totals = totals,
      id_col = id_col,
      call = match.call()
    ),
    class = "blindspot_spi"
  )

  if (verbose) {
    cli::cli_progress_done()
    n_groups <- nrow(spi_obj$summary)
    n_groups_s <- format(n_groups, big.mark = ",")
    val_word <- ngettext(n_groups, "value", "values")
    cli::cli_alert_success(
      "Computed {n_groups_s} SPI {val_word}."
    )
    if (nrow(low_info) > 0) {
      n_low <- nrow(low_info)
      n_low_s <- format(n_low, big.mark = ",")
      group_word <- ngettext(n_low, "group", "groups")
      cli::cli_alert_warning(
        "{n_low_s} {group_word} flagged low-information \\
         (expected < {min_expected})."
      )
    }
  }

  result
}

# --- internal helpers -------------------------------------

# summarise a draws matrix into median / mean / quantile columns
# in one pass via matrixStats. Returns a tibble with the columns
# expected_total, spi_median, spi_mean, spi_q05/q10/q90/q95.
# @noRd
.spi_summarise <- function(exp_draws, spi_draws) {
  qs <- matrixStats::colQuantiles(
    spi_draws,
    probs = c(0.05, 0.10, 0.50, 0.90, 0.95),
    na.rm = TRUE
  )
  tibble::tibble(
    expected_total = matrixStats::colMedians(exp_draws, na.rm = TRUE),
    spi_median = qs[, 3],
    spi_mean = matrixStats::colMeans2(spi_draws, na.rm = TRUE),
    spi_q05 = qs[, 1],
    spi_q10 = qs[, 2],
    spi_q90 = qs[, 4],
    spi_q95 = qs[, 5]
  )
}

# SPI at district-month level
# @noRd
.spi_district_month <- function(draws, observed, fit_data, id_col) {
  # spi_draws[i, j] = observed[j] / draws[i, j]
  spi_draws <- sweep(1 / draws, 2, observed, FUN = "*")

  base <- fit_data[, c(id_col, "month")]
  base$observed <- observed

  summary_tbl <- dplyr::bind_cols(
    tibble::as_tibble(base),
    .spi_summarise(draws, spi_draws)
  )

  list(draws = spi_draws, summary = summary_tbl)
}

# SPI at district-quarter level
# quarter is the first day of the calendar quarter (e.g. 2016-01-01,
# 2016-04-01) so it sorts and groups correctly as a Date.
# @noRd
.spi_district_quarter <- function(draws, observed, fit_data, id_col) {
  fit_data$quarter <- lubridate::floor_date(fit_data$month, "quarter")

  groups <- fit_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_col, "quarter")))) |>
    dplyr::summarise(
      cols = list(.data$col_idx),
      obs_sum = sum(.env$observed[.data$col_idx]),
      .groups = "drop"
    )

  exp_sum_draws <- .sum_draws_by_group(draws, groups$cols)
  spi_draws <- sweep(1 / exp_sum_draws, 2, groups$obs_sum, FUN = "*")

  base <- groups[, c(id_col, "quarter")]
  base$observed <- groups$obs_sum

  summary_tbl <- dplyr::bind_cols(
    tibble::as_tibble(base),
    .spi_summarise(exp_sum_draws, spi_draws)
  )

  list(draws = spi_draws, summary = summary_tbl)
}

# SPI at district-year level
# @noRd
.spi_district_year <- function(draws, observed, fit_data, id_col) {
  fit_data$year <- lubridate::year(fit_data$month)

  groups <- fit_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_col, "year")))) |>
    dplyr::summarise(
      cols = list(.data$col_idx),
      obs_sum = sum(.env$observed[.data$col_idx]),
      .groups = "drop"
    )

  exp_sum_draws <- .sum_draws_by_group(draws, groups$cols)
  spi_draws <- sweep(1 / exp_sum_draws, 2, groups$obs_sum, FUN = "*")

  base <- groups[, c(id_col, "year")]
  base$observed <- groups$obs_sum

  summary_tbl <- dplyr::bind_cols(
    tibble::as_tibble(base),
    .spi_summarise(exp_sum_draws, spi_draws)
  )

  list(draws = spi_draws, summary = summary_tbl)
}

# SPI at district-total level
# @noRd
.spi_district_total <- function(draws, observed, fit_data, id_col) {
  groups <- fit_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) |>
    dplyr::summarise(
      cols = list(.data$col_idx),
      obs_sum = sum(.env$observed[.data$col_idx]),
      .groups = "drop"
    )

  exp_sum_draws <- .sum_draws_by_group(draws, groups$cols)
  spi_draws <- sweep(1 / exp_sum_draws, 2, groups$obs_sum, FUN = "*")

  base <- groups[, id_col, drop = FALSE]
  base$observed <- groups$obs_sum

  summary_tbl <- dplyr::bind_cols(
    tibble::as_tibble(base),
    .spi_summarise(exp_sum_draws, spi_draws)
  )

  list(draws = spi_draws, summary = summary_tbl)
}

# sum draws columns within each group; returns [n_draws x n_groups]
# @noRd
.sum_draws_by_group <- function(draws, cols_list) {
  n_draws <- nrow(draws)
  n_grp <- length(cols_list)
  out <- matrix(NA_real_, n_draws, n_grp)
  for (g in seq_len(n_grp)) {
    out[, g] <- matrixStats::rowSums2(
      draws[, cols_list[[g]], drop = FALSE]
    )
  }
  out
}

# --- print method -----------------------------------------

#' @export
print.blindspot_spi <- function(x, ...) {
  rlang::check_dots_empty()
  fmt_int <- function(v) format(v, big.mark = ",")

  n_obs_total <- x$totals$total_observed
  n_exp_med <- x$totals$total_expected_median
  n_groups <- x$totals$n_groups
  val_word <- ngettext(n_groups, "value", "values")

  cli::cli_h2("Blindspot SPI")
  cli::cli_bullets(c(
    "i" = "Level: {.val {x$level}}.",
    "i" = "{fmt_int(n_groups)} SPI {val_word} computed.",
    "i" = "Total observed: {fmt_int(n_obs_total)} | total expected \\
           (median): {fmt_int(n_exp_med)}."
  ))

  cli::cli_h3("SPI distribution (medians)")
  q <- stats::quantile(
    x$summary$spi_median,
    probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
    na.rm = TRUE
  )
  cli::cli_bullets(c(
    "*" = "q05: {round(q[[1]], 2)}",
    "*" = "q25: {round(q[[2]], 2)}",
    "*" = "median: {round(q[[3]], 2)}",
    "*" = "q75: {round(q[[4]], 2)}",
    "*" = "q95: {round(q[[5]], 2)}"
  ))

  # preliminary classification preview
  spi_med <- x$summary$spi_median
  n_low <- sum(spi_med < 0.5, na.rm = TRUE)
  n_mid <- sum(spi_med >= 0.5 & spi_med < 0.8, na.rm = TRUE)
  n_ok <- sum(spi_med >= 0.8 & spi_med <= 1.2, na.rm = TRUE)
  n_high <- sum(spi_med > 1.2, na.rm = TRUE)

  cli::cli_h3("Preliminary classification")
  cli::cli_bullets(c(
    "*" = "SPI < 0.5 (severe): {fmt_int(n_low)}",
    "*" = "SPI 0.5-0.8 (moderate): {fmt_int(n_mid)}",
    "*" = "SPI 0.8-1.2 (acceptable): {fmt_int(n_ok)}",
    "*" = "SPI > 1.2 (elevated): {fmt_int(n_high)}"
  ))

  n_low_info <- x$totals$n_low_information
  if (n_low_info > 0) {
    group_word <- ngettext(n_low_info, "group", "groups")
    cli::cli_alert_warning(
      "{fmt_int(n_low_info)} {group_word} flagged low-information."
    )
  }

  invisible(x)
}

#' @export
summary.blindspot_spi <- function(object, ...) {
  rlang::check_dots_empty()

  sm <- object$summary

  # core stats
  med_val <- stats::median(sm$spi_median, na.rm = TRUE)
  ratio_val <- object$totals$total_observed /
    object$totals$total_expected_median

  log_spi <- log(sm$spi_median[sm$spi_median > 0])
  skew_val <- mean((log_spi - mean(log_spi))^3) /
    stats::sd(log_spi)^3

  cri_val <- mean(
    sm$spi_q05 > 1 | sm$spi_q95 < 1,
    na.rm = TRUE
  )

  pct_severe <- mean(sm$spi_median < 0.5, na.rm = TRUE)
  pct_elevated <- mean(sm$spi_median > 1.5, na.rm = TRUE)

  q <- stats::quantile(
    sm$spi_median,
    probs = c(0.05, 0.25, 0.50, 0.75, 0.95),
    na.rm = TRUE
  )

  # flag strings reused in both the tibble and the footnotes
  med_flag <- .flag_within(med_val, 0.9, 1.1)
  ratio_flag <- .flag_within(ratio_val, 0.9, 1.1)
  skew_flag <- .flag_within(skew_val, -0.5, 0.5)
  cri_flag <- .flag_within(cri_val, 0.05, 0.50)

  # values displayed with big.mark commas; tibble column type
  # becomes <chr> so large counts render as e.g. "3,870"
  fmt <- function(v) {
    format(v, big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  diagnostics <- tibble::tibble(
    metric = c(
      "median SPI",
      "calibration ratio (obs/exp)",
      "log-SPI skewness",
      "% with CrI excluding 1",
      "% SPI < 0.5 (severe)",
      "% SPI > 1.5 (elevated)",
      "SPI q05", "SPI q25", "SPI q75", "SPI q95",
      "n groups",
      "n low_information"
    ),
    value = c(
      fmt(round(med_val, 3)),
      fmt(round(ratio_val, 3)),
      fmt(round(skew_val, 3)),
      fmt(round(cri_val * 100, 1)),
      fmt(round(pct_severe * 100, 1)),
      fmt(round(pct_elevated * 100, 1)),
      fmt(round(q[[1]], 3)),
      fmt(round(q[[2]], 3)),
      fmt(round(q[[4]], 3)),
      fmt(round(q[[5]], 3)),
      fmt(nrow(sm)),
      fmt(nrow(object$low_information))
    ),
    flag = c(
      med_flag, ratio_flag, skew_flag, cri_flag,
      NA, NA, NA, NA, NA, NA, NA, NA
    )
  )

  cli::cli_h2("Blindspot SPI diagnostics")
  cli::cli_alert_info("Level: {.val {object$level}}.")
  print(diagnostics)

  # --- footnotes: per-metric interpretation -------------
  cli::cli_h3("Footnotes")
  cli::cli_bullets(c(
    "i" = .note_median(med_val, med_flag),
    "i" = .note_ratio(ratio_val, ratio_flag),
    "i" = .note_skew(skew_val, skew_flag),
    "i" = .note_cri(cri_val, cri_flag)
  ))

  invisible(diagnostics)
}

# --- footnote builders ------------------------------------

# @noRd
.note_median <- function(val, flag) {
  diff_pct <- round(abs(val - 1) * 100, 1)
  direction <- if (val < 1) "underdetection" else "overdetection"
  if (flag == "pass") {
    fmt <- paste0(
      "Median SPI %.3f -> pass. Small systematic %s (~%g%%), ",
      "well within range."
    )
  } else {
    fmt <- paste0(
      "Median SPI %.3f -> flag. Systematic %s of ~%g%% exceeds ",
      "the 10%% tolerance; check the offset and population units."
    )
  }
  sprintf(fmt, val, direction, diff_pct)
}

# @noRd
.note_ratio <- function(val, flag) {
  diff_pct <- round(abs(val - 1) * 100, 1)
  direction <- if (val < 1) "below" else "above"
  if (flag == "pass") {
    fmt <- paste0(
      "Aggregate ratio %.3f -> pass. Total observed matches ",
      "total expected within ~%g%%."
    )
    sprintf(fmt, val, diff_pct)
  } else {
    fmt <- paste0(
      "Aggregate ratio %.3f -> flag. Total %s expected by ~%g%%; ",
      "aggregate calibration is off."
    )
    sprintf(fmt, val, direction, diff_pct)
  }
}

# @noRd
.note_skew <- function(val, flag) {
  if (flag == "pass") {
    fmt <- paste0(
      "Log-SPI skewness %.2f -> pass. Distribution is roughly ",
      "symmetric on the log scale."
    )
  } else if (val < 0) {
    fmt <- paste0(
      "Log-SPI skewness %.2f -> flag. Left tail heavier than ",
      "right; a handful of districts have very low SPI relative ",
      "to others, consistent with surveillance blind spots."
    )
  } else {
    fmt <- paste0(
      "Log-SPI skewness %.2f -> flag. Right tail heavier than ",
      "left; a handful of districts have very high SPI, ",
      "consistent with outbreak signals or over-reporting."
    )
  }
  sprintf(fmt, val)
}

# @noRd
.note_cri <- function(val, flag) {
  pct <- round(val * 100, 1)
  if (flag == "pass") {
    fmt <- paste0(
      "%g%% of CrIs exclude 1 -> pass. Reasonable share of ",
      "districts show significant deviation from expectation."
    )
  } else if (val < 0.05) {
    fmt <- paste0(
      "%g%% of CrIs exclude 1 -> flag. Very few districts ",
      "deviate significantly; the model may be over-smoothed ",
      "(consider looser priors)."
    )
  } else {
    fmt <- paste0(
      "%g%% of CrIs exclude 1 -> flag. High share of significant ",
      "deviations; the model is highly informative but could also ",
      "be under-smoothed. Worth flagging as a sensitivity in the ",
      "methods section."
    )
  }
  sprintf(fmt, pct)
}

# pass / flag helper: returns "pass" if x is in [lo, hi], else "flag"
# @noRd
.flag_within <- function(x, lo, hi) {
  if (is.na(x)) {
    return(NA_character_)
  }
  if (x >= lo && x <= hi) "pass" else "flag"
}

#' @export
as_tibble.blindspot_spi <- function(x, ...) {
  rlang::check_dots_empty()
  x$summary
}

#' @export
as.data.frame.blindspot_spi <- function(x, ...) {
  rlang::check_dots_empty()
  as.data.frame(x$summary)
}

#' @export
plot.blindspot_spi <- function(
  x,
  type = c("distribution", "funnel", "caterpillar", "calibration"),
  thresholds = c(0.5, 1, 1.5),
  year = NULL,
  n_show = 50L,
  ...
) {
  rlang::check_dots_empty()
  .check_pkg("ggplot2", reason = "to plot a blindspot_spi object")
  type <- match.arg(type)

  sm <- x$summary
  if (!is.null(year) && "year" %in% names(sm)) {
    sm <- sm |> dplyr::filter(.data$year == !!year)
  }

  switch(
    type,
    distribution = .plot_distribution(sm, thresholds),
    funnel = .plot_funnel(sm, thresholds),
    caterpillar = .plot_caterpillar(sm, n_show, thresholds),
    calibration = .plot_calibration(sm)
  )
}

# --- internal plot helpers --------------------------------

# package-level palette used by every plot.blindspot_* method
# so the visual identity stays consistent.
# @noRd
.bs_palette <- list(
  primary = "#4682B4",       # steelblue: histogram / point fill
  primary_dark = "#2C5E8A",  # darker steelblue: stroke / accent / high band
  burgundy = "#7B1D3D",      # threshold lines, calibration diagonal, low band
  grey_mid = "#5A5A5A",      # error bars
  grey_border = "grey30"     # panel border
)

# package-level theme: minimal base, panel border, padded axis titles
# @noRd
.bs_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        colour = .bs_palette$grey_border,
        fill = NA,
        linewidth = 0.4
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 12)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 12)
      ),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey30"),
      # align title and subtitle with the panel (x-axis), not the
      # outer plot edge; "panel" is the ggplot2 default
      plot.title.position = "panel",
      plot.margin = ggplot2::margin(12, 14, 12, 12)
    )
}

# headline SPI distribution histogram
# @noRd
.plot_distribution <- function(sm, thresholds) {
  n_total <- nrow(sm)
  n_fmt <- format(n_total, big.mark = ",")
  med <- round(stats::median(sm$spi_median, na.rm = TRUE), 2)
  pct_below_05 <- round(
    mean(sm$spi_median < 0.5, na.rm = TRUE) * 100, 1
  )
  pct_above_15 <- round(
    mean(sm$spi_median > 1.5, na.rm = TRUE) * 100, 1
  )

  # drop SPI = 0 so log10 doesn't introduce -Inf
  plot_df <- sm[is.finite(sm$spi_median) & sm$spi_median > 0, ]
  n_dropped <- n_total - nrow(plot_df)
  dropped_note <- if (n_dropped > 0) {
    glue::glue(" | dropped {n_dropped} SPI = 0")
  } else {
    ""
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$spi_median)
  ) +
    ggplot2::geom_histogram(
      bins = 50,
      fill = .bs_palette$primary,
      colour = .bs_palette$primary_dark,
      alpha = 0.7,
      linewidth = 0.3
    ) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::geom_vline(
      xintercept = thresholds[thresholds != 1],
      linetype = "dotted",
      colour = .bs_palette$burgundy,
      linewidth = 0.5
    ) +
    ggplot2::scale_x_log10(
      breaks = c(0.1, 0.25, 0.5, 1, 1.5, 2, 5)
    ) +
    ggplot2::labs(
      title = "SPI distribution",
      subtitle = glue::glue(
        "n = {n_fmt} | median = {med} | ",
        "<0.5: {pct_below_05}% | ",
        ">1.5: {pct_above_15}%{dropped_note}"
      ),
      x = "SPI (log scale)",
      y = "count"
    ) +
    .bs_theme()
}

# funnel plot: SPI vs expected total
# @noRd
.plot_funnel <- function(sm, thresholds) {
  # drop SPI = 0 (log10 would push to -Inf and ggplot warns)
  plot_df <- sm[is.finite(sm$spi_median) & sm$spi_median > 0, ]

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$expected_total,
      y = .data$spi_median
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$spi_q05,
        ymax = .data$spi_q95
      ),
      width = 0,
      alpha = 0.15,
      colour = .bs_palette$grey_mid
    ) +
    ggplot2::geom_point(
      alpha = 0.6,
      size = 1.1,
      colour = .bs_palette$primary_dark
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::geom_hline(
      yintercept = thresholds[thresholds != 1],
      linetype = "dotted",
      colour = .bs_palette$burgundy,
      linewidth = 0.5
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10(
      breaks = c(0.1, 0.25, 0.5, 1, 2, 5)
    ) +
    ggplot2::labs(
      title = "SPI funnel",
      subtitle = "uncertainty should shrink with expected count",
      x = "expected count (log scale)",
      y = "SPI (log scale)"
    ) +
    .bs_theme()
}

# caterpillar: ranked SPI with credible intervals
# @noRd
.plot_caterpillar <- function(sm, n_show, thresholds) {
  # drop SPI = 0 (log scale)
  sm <- sm[is.finite(sm$spi_median) & sm$spi_median > 0, ]

  ranked <- sm |>
    dplyr::arrange(.data$spi_median)

  bottom <- utils::head(ranked, n_show) |>
    dplyr::mutate(band = "lowest")
  top <- utils::tail(ranked, n_show) |>
    dplyr::mutate(band = "highest")

  # re-index within the displayed subset so x runs 1..(2 * n_show)
  # instead of inheriting positions from the full ranking (which would
  # leave a giant gap in the middle of the plot)
  show <- dplyr::bind_rows(bottom, top) |>
    dplyr::mutate(plot_rank = dplyr::row_number())

  ggplot2::ggplot(
    show,
    ggplot2::aes(
      x = .data$plot_rank,
      y = .data$spi_median
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$spi_q05,
        ymax = .data$spi_q95
      ),
      width = 0,
      alpha = 0.55,
      colour = .bs_palette$grey_mid
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$band),
      size = 2.0
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        lowest = .bs_palette$burgundy,
        highest = .bs_palette$primary_dark
      )
    ) +
    # visual divider between the lowest and highest blocks
    ggplot2::geom_vline(
      xintercept = n_show + 0.5,
      linetype = "dotted",
      colour = "grey60"
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::geom_hline(
      yintercept = thresholds[thresholds != 1],
      linetype = "dotted",
      colour = .bs_palette$burgundy,
      linewidth = 0.5
    ) +
    ggplot2::scale_y_log10(
      breaks = c(0.1, 0.25, 0.5, 1, 2, 5)
    ) +
    ggplot2::labs(
      title = glue::glue(
        "{n_show} lowest and {n_show} highest SPI groups"
      ),
      x = "rank (within displayed subset)",
      y = "SPI (log scale)"
    ) +
    .bs_theme() +
    ggplot2::theme(legend.position = "none")
}

# observed vs expected calibration scatter
# @noRd
.plot_calibration <- function(sm) {
  # drop observed = 0 (log scale)
  plot_df <- sm[is.finite(sm$observed) & sm$observed > 0, ]

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$expected_total,
      y = .data$observed
    )
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = .bs_palette$burgundy,
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      alpha = 0.55,
      size = 1.1,
      colour = .bs_palette$primary_dark
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      title = "Observed vs expected",
      subtitle = "diagonal = perfect calibration",
      x = "expected (log scale)",
      y = "observed (log scale)"
    ) +
    .bs_theme()
}
