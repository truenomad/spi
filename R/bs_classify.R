#' Derive empirical SPI classification thresholds via ROC
#'
#' @description
#' Uses subsequent confirmed virus detection (cVDPV2 or WPV1) as the
#' operational outcome and identifies the SPI threshold that best
#' discriminates districts that later experienced virus circulation from
#' those that did not. Propagates posterior uncertainty in SPI through to
#' the threshold itself by recomputing the ROC at each of `n_bootstrap`
#' posterior draws.
#'
#' This turns SPI from a descriptive metric (with placeholder cut-points
#' of 0.5 / 0.8 / 1.2) into a defensible operational tool with empirical
#' thresholds derived from the data.
#'
#' @param spi Object of class `blindspot_spi` at `district_year` level.
#' @param virus_outcome Tibble with the district identifier column matching
#'   `spi$id_col`, `year` (integer), and at least one of `any_wpv1`,
#'   `any_cvdpv2`, `any_virus`.
#' @param lag Integer. Years between the SPI year and the outcome year. A
#'   surveillance failure in year T should manifest as detected
#'   transmission in T+`lag`. Default: 1L. Ignored when `lag_sweep` is
#'   non-NULL.
#' @param lag_sweep Optional. If non-NULL, runs the classification at
#'   multiple lags and returns a `blindspot_classify_sweep` object with
#'   per-lag fits and a summary table. Accepts:
#'   \itemize{
#'     \item `NULL` (default) — single-lag mode using `lag`.
#'     \item `TRUE` — sweeps over the default range `0:4`.
#'     \item An integer vector — sweeps over those lags (e.g.
#'       `c(0, 1, 2, 3, 4)`).
#'   }
#' @param outcome_type Character. Which virus detection to use as the
#'   outcome: "any" (default), "wpv1", or "cvdpv2".
#' @param threshold_method Character. "youden" (default, maximises
#'   sensitivity + specificity - 1), "f1" (maximises F1 score), or
#'   "fixed_specificity" (returns the threshold giving specificity closest
#'   to `target_specificity`).
#' @param target_specificity Numeric in (0, 1). Used only when
#'   `threshold_method = "fixed_specificity"`. Default: 0.90.
#' @param compare_baseline Logical. If TRUE (default), also computes a
#'   baseline ROC using raw observed counts inverted (so low counts
#'   predict positive outcome), for comparison against SPI.
#' @param baseline Optional data frame with columns `{id_col}`, `year`
#'   (the SPI year, not the outcome year), and `baseline_score`. When
#'   supplied, this external predictor replaces the default raw observed
#'   count as the baseline comparator. Default: NULL (raw observed count
#'   fallback).
#' @param baseline_direction Character. Direction for the baseline ROC
#'   computation: `"auto"` (default — runs both directions and keeps the
#'   higher AUC), `"low"` (low score predicts positive), or `"high"`
#'   (high score predicts positive). Passed to `.compute_roc()`.
#' @param n_bootstrap Integer. Posterior SPI draws used to compute
#'   threshold uncertainty. Default: 1000L.
#' @param auc_ci Logical. If TRUE (default), bootstraps a 95% CI on the
#'   ROC AUC by resampling district-years with replacement and
#'   recomputing the ROC at each resample. In lag-sweep mode this also
#'   adds error bars to the AUC-vs-lag plot, so overlapping CIs at
#'   different lags are visible at a glance. Set FALSE to skip the
#'   bootstrap for faster runs (the point AUC is still computed).
#' @param auc_ci_n Integer. Number of AUC bootstrap resamples per fit.
#'   Default: 1000L. Ignored when `auc_ci = FALSE`.
#' @param seed Integer or NULL for the bootstrap sample. Default: 42L.
#' @param verbose Logical. Default: TRUE.
#'
#' @return Object of class `blindspot_classify`. A list with:
#' \describe{
#'   \item{threshold}{Tibble with `method`, `median`, `q05`, `q95`, and
#'     `n_draws` of the posterior threshold distribution.}
#'   \item{roc_spi}{ROC summary for SPI (thresholds, tpr, fpr, auc).}
#'   \item{roc_baseline}{ROC summary for the raw-count baseline, or NULL.}
#'   \item{classification}{Tibble pairing each district-year with its
#'     empirical class label: `blind_spot`, `performing`, or
#'     `uncertain`.}
#'   \item{performance}{Tibble of sensitivity, specificity, PPV, NPV, and
#'     the 2x2 confusion counts at the median posterior threshold.}
#'   \item{n_outcome}{Count of positive outcomes in the paired data.}
#'   \item{n_total}{Total paired district-years.}
#'   \item{id_col}{The id column name, echoed for downstream use.}
#'   \item{call}{Matched call.}
#' }
#'
#' @details
#' Classification rule at the chosen threshold (`tau`):
#' \itemize{
#'   \item `blind_spot`: 95th percentile of posterior SPI < tau.
#'   \item `performing`: 5th percentile of posterior SPI > tau.
#'   \item `uncertain`: the credible interval straddles tau.
#' }
#'
#' The `uncertain` band is honest — these are district-years where the
#' model cannot make a confident call given current data. Reporting them
#' separately is preferable to pretending they don't exist.
#'
#' @seealso [bs_spi()], [bs_expected()]
#' @family blindspot core functions
#'
#' @export
#' @examples
#' \dontrun{
#' classify_wpv1 <- bs_classify(
#'   spi = spi_dy,
#'   virus_outcome = virus_outcome,
#'   lag = 1L,
#'   outcome_type = "wpv1",
#'   n_bootstrap = 1000L
#' )
#' print(classify_wpv1)
#' plot(classify_wpv1)
#' }
bs_classify <- function(
  spi,
  virus_outcome,
  lag = 1L,
  lag_sweep = NULL,
  outcome_type = c("any", "wpv1", "cvdpv2"),
  threshold_method = c("youden", "f1", "fixed_specificity"),
  target_specificity = 0.90,
  compare_baseline = TRUE,
  baseline = NULL,
  baseline_direction = c("auto", "low", "high"),
  n_bootstrap = 1000L,
  auc_ci = TRUE,
  auc_ci_n = 1000L,
  seed = 42L,
  verbose = TRUE
) {
  # --- check required packages --------------------------
  .check_pkg(
    c("dplyr", "tibble", "cli"),
    reason = "to derive empirical SPI classification thresholds"
  )

  # --- validate inputs ----------------------------------
  stopifnot(
    inherits(spi, "blindspot_spi"),
    is.numeric(lag),
    length(lag) == 1,
    lag >= 0,
    is.numeric(target_specificity),
    target_specificity > 0,
    target_specificity < 1,
    is.logical(compare_baseline),
    is.numeric(n_bootstrap),
    n_bootstrap > 0,
    is.logical(auc_ci),
    is.numeric(auc_ci_n),
    auc_ci_n > 0
  )
  outcome_type <- match.arg(outcome_type)
  threshold_method <- match.arg(threshold_method)
  baseline_direction <- match.arg(baseline_direction)
  n_bootstrap <- as.integer(n_bootstrap)
  auc_ci_n <- as.integer(auc_ci_n)

  # --- dispatch to lag sweep if requested --------------
  if (!is.null(lag_sweep)) {
    lags <- if (isTRUE(lag_sweep)) {
      0:4
    } else if (is.numeric(lag_sweep) && length(lag_sweep) >= 2) {
      sort(unique(as.integer(lag_sweep)))
    } else {
      cli::cli_abort(
        "{.arg lag_sweep} must be {.val TRUE}, NULL, or a numeric \\
         vector of length >= 2."
      )
    }
    if (any(lags < 0)) {
      cli::cli_abort(
        "{.arg lag_sweep} values must be non-negative integers."
      )
    }
    return(.run_lag_sweep(
      spi = spi,
      virus_outcome = virus_outcome,
      lags = lags,
      outcome_type = outcome_type,
      threshold_method = threshold_method,
      target_specificity = target_specificity,
      compare_baseline = compare_baseline,
      baseline = baseline,
      baseline_direction = baseline_direction,
      n_bootstrap = n_bootstrap,
      auc_ci = auc_ci,
      auc_ci_n = auc_ci_n,
      seed = seed,
      verbose = verbose,
      user_call = match.call()
    ))
  }

  if (spi$level != "district_year") {
    cli::cli_abort(
      "{.fn bs_classify} requires {.code spi$level == \"district_year\"}; \\
       got {.val {spi$level}}. Refit with \\
       {.code bs_spi(..., level = \"district_year\")}."
    )
  }

  id_col <- if (is.null(spi$id_col)) "district_id" else spi$id_col
  outcome_col <- switch(
    outcome_type,
    any = "any_virus",
    wpv1 = "any_wpv1",
    cvdpv2 = "any_cvdpv2"
  )

  if (!outcome_col %in% names(virus_outcome)) {
    cli::cli_abort(
      "{.arg virus_outcome} is missing column {.val {outcome_col}}."
    )
  }
  if (!id_col %in% names(virus_outcome)) {
    cli::cli_abort(
      "{.arg virus_outcome} must contain the id column \\
       {.val {id_col}} (to match {.code spi$id_col})."
    )
  }
  if (!"year" %in% names(virus_outcome)) {
    cli::cli_abort(
      "{.arg virus_outcome} must contain a {.field year} column."
    )
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # --- build outcome with lag --------------------------
  if (verbose) {
    lag_word <- ngettext(lag, "year", "years")
    cli::cli_progress_step(
      "Lagging outcome by {lag} {lag_word} (type: {.val {outcome_type}})"
    )
  }

  outcome_lagged <- virus_outcome |>
    dplyr::mutate(
      spi_year = .data$year - lag,
      outcome = as.integer(.data[[outcome_col]])
    ) |>
    dplyr::select(dplyr::all_of(c(id_col, "spi_year", "outcome")))

  # --- pair SPI summary with lagged outcome ------------
  if (verbose) {
    cli::cli_progress_step("Pairing SPI with virus outcome")
  }

  # drop spi-years whose implied outcome year falls outside the observable
  # window of virus_outcome. without this filter, lag k for high k pools
  # district-years whose outcome year is beyond the data and silently
  # coalesces their absent outcomes to zero, inflating the denominator
  # with false negatives.
  max_outcome_year_observed <- suppressWarnings(
    max(virus_outcome$year, na.rm = TRUE)
  )

  paired <- spi$summary |>
    dplyr::rename(spi_year = "year") |>
    dplyr::filter(.data$spi_year + lag <= max_outcome_year_observed) |>
    dplyr::left_join(
      outcome_lagged,
      by = c(id_col, "spi_year")
    ) |>
    dplyr::mutate(
      outcome = dplyr::if_else(
        is.na(.data$outcome),
        0L,
        as.integer(.data$outcome)
      )
    )

  # --- attach external baseline predictor if provided ----
  # baseline must be a data frame with columns {id_col, year, baseline_score}.
  # year is the SPI year (matched to paired$spi_year), not the outcome year.
  if (!is.null(baseline)) {
    required_cols <- c(id_col, "year", "baseline_score")
    missing_cols <- setdiff(required_cols, names(baseline))
    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "{.arg baseline} is missing required column(s): \\
         {.val {missing_cols}}."
      )
    }
    paired <- paired |>
      dplyr::left_join(
        baseline |>
          dplyr::select(dplyr::all_of(required_cols)) |>
          dplyr::rename(spi_year = "year"),
        by = c(id_col, "spi_year")
      )
  } else {
    paired$baseline_score <- paired$observed
  }

  n_outcome <- sum(paired$outcome)
  n_total <- nrow(paired)
  pct_outcome <- round(n_outcome / n_total * 100, 1)

  if (n_outcome == 0 || n_outcome == n_total) {
    cli::cli_abort(c(
      "Cannot fit ROC: need at least one of each class.",
      "i" = "Found {n_outcome} positive and \\
             {n_total - n_outcome} negative district-years.",
      "i" = "Check {.code outcome_type}, {.code lag}, and that \\
             {.arg virus_outcome} covers the right years."
    ))
  }

  if (verbose) {
    n_o_str <- format(n_outcome, big.mark = ",")
    n_t_str <- format(n_total, big.mark = ",")
    out_word <- ngettext(
      n_outcome,
      "positive outcome",
      "positive outcomes"
    )
    cli::cli_alert_info(
      "{n_o_str} {out_word} in {n_t_str} district-years \\
       ({pct_outcome}%)."
    )
  }
  if (n_outcome < 10) {
    cli::cli_alert_warning(
      "Fewer than 10 outcome events; threshold estimates will be \\
       unstable. Consider pooling outcome types or extending the \\
       study period."
    )
  }

  # --- bootstrap threshold via posterior draws ----------
  draws <- spi$draws
  n_draws <- nrow(draws)
  with_replacement <- n_bootstrap > n_draws
  sample_idx <- sample.int(
    n_draws,
    size = n_bootstrap,
    replace = with_replacement
  )

  if (verbose) {
    n_b_str <- format(n_bootstrap, big.mark = ",")
    suffix <- if (with_replacement) " (with replacement)" else ""
    cli::cli_progress_step(
      "Bootstrapping threshold over {n_b_str} posterior draws{suffix}"
    )
  }

  threshold_draws <- vapply(
    sample_idx,
    function(d) {
      .find_threshold(
        scores = draws[d, ],
        outcome = paired$outcome,
        method = threshold_method,
        target_specificity = target_specificity
      )
    },
    FUN.VALUE = numeric(1)
  )

  threshold_tbl <- tibble::tibble(
    method = threshold_method,
    median = stats::median(threshold_draws, na.rm = TRUE),
    q05 = unname(
      stats::quantile(threshold_draws, 0.05, na.rm = TRUE)
    ),
    q95 = unname(
      stats::quantile(threshold_draws, 0.95, na.rm = TRUE)
    ),
    n_draws = length(threshold_draws)
  )

  # --- ROC at posterior median SPI ---------------------
  if (verbose) {
    cli::cli_progress_step("Computing ROC and classification")
  }
  roc_spi <- .compute_roc(
    paired$spi_median,
    paired$outcome,
    direction = "low"
  )

  if (roc_spi$auc < 0.5) {
    cli::cli_alert_warning(
      "ROC AUC (SPI) is {round(roc_spi$auc, 3)} (< 0.5); this suggests \\
       higher SPI predicts the outcome, not lower. Check the lag \\
       direction and that the outcome column matches surveillance \\
       expectations."
    )
  }

  # --- baseline ROC ------------------------------------
  # `baseline` is the conventional comparator. when supplied by the
  # caller (data frame with id_col + year + baseline_score), use that
  # column; otherwise fall back to raw observed count. direction
  # defaults to "auto" so the sign of the comparator is an empirical
  # question rather than a baked-in assumption.
  roc_baseline <- if (compare_baseline) {
    valid <- !is.na(paired$baseline_score)
    .compute_roc(
      paired$baseline_score[valid],
      paired$outcome[valid],
      direction = baseline_direction
    )
  } else {
    NULL
  }

  # --- bootstrap 95% CI on the AUC ---------------------
  # resamples district-years with replacement and recomputes the ROC;
  # gives an honest spread around the AUC point estimate so overlapping
  # CIs across lags can be read off the AUC-vs-lag plot.
  if (auc_ci) {
    if (verbose) {
      ci_n_str <- format(auc_ci_n, big.mark = ",")
      cli::cli_progress_step(
        "Bootstrapping AUC 95% CI over {ci_n_str} resamples"
      )
    }
    spi_boot <- .bootstrap_auc_ci(
      paired$spi_median,
      paired$outcome,
      auc_ci_n,
      direction = "low"
    )
    roc_spi$auc_boot <- spi_boot$boot
    roc_spi$auc_ci <- spi_boot$ci
    if (compare_baseline) {
      valid <- !is.na(paired$baseline_score)
      base_boot <- .bootstrap_auc_ci(
        paired$baseline_score[valid],
        paired$outcome[valid],
        auc_ci_n,
        direction = baseline_direction
      )
      roc_baseline$auc_boot <- base_boot$boot
      roc_baseline$auc_ci <- base_boot$ci
    }
  }

  # --- classify each district-year ---------------------
  threshold_point <- threshold_tbl$median

  classification <- paired |>
    dplyr::mutate(
      class = dplyr::case_when(
        .data$spi_q95 < threshold_point ~ "blind_spot",
        .data$spi_q05 > threshold_point ~ "performing",
        TRUE ~ "uncertain"
      )
    )

  # --- operating characteristics at the median ---------
  predicted <- as.integer(paired$spi_median < threshold_point)
  performance <- .confusion_stats(predicted, paired$outcome)

  # --- assemble output ----------------------------------
  result <- structure(
    list(
      threshold = threshold_tbl,
      roc_spi = roc_spi,
      roc_baseline = roc_baseline,
      classification = classification,
      performance = performance,
      n_outcome = n_outcome,
      n_total = n_total,
      lag = as.integer(lag),
      outcome_type = outcome_type,
      id_col = id_col,
      call = match.call()
    ),
    class = "blindspot_classify"
  )

  if (verbose) {
    cli::cli_progress_done()
    .report_classify(result)
  }

  result
}

# --- internal helpers -------------------------------------

# compute ROC summary for a continuous score where LOW score
# Bootstrap 95% CI for ROC AUC by resampling rows with replacement
# n_boot times and recomputing the ROC at each resample. Iterations
# where the resample contains only one class contribute NA (no
# meaningful AUC). Returns a list with the bootstrap vector and the
# 2.5% / 97.5% quantiles.
# @noRd
.bootstrap_auc_ci <- function(scores, outcome, n_boot,
                              direction = c("low", "high", "auto")) {
  direction <- match.arg(direction)
  n <- length(scores)
  aucs <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    idx <- sample.int(n, n, replace = TRUE)
    o <- outcome[idx]
    n_pos <- sum(o == 1)
    if (n_pos == 0 || n_pos == n) {
      aucs[b] <- NA_real_
    } else {
      aucs[b] <- .compute_roc(scores[idx], o, direction = direction)$auc
    }
  }
  list(
    boot = aucs,
    ci = unname(
      stats::quantile(aucs, c(0.025, 0.975), na.rm = TRUE)
    )
  )
}

# predicts POSITIVE outcome from a continuous score.
#   direction = "low"  : low score = predicted positive (SPI, where low SPI is
#                        a blind spot). Inverts via -scores. Default.
#   direction = "high" : high score = predicted positive. No inversion.
#   direction = "auto" : compute both and return whichever gives higher AUC;
#                        used when the caller does not want to bake in a
#                        direction (e.g. a baseline predictor whose sign is
#                        an empirical question).
# Returns thresholds, tpr, fpr, auc, and the resolved `direction`.
# @noRd
.compute_roc <- function(scores, outcome,
                         direction = c("low", "high", "auto")) {
  direction <- match.arg(direction)

  if (direction == "auto") {
    low <- .compute_roc(scores, outcome, direction = "low")
    high <- .compute_roc(scores, outcome, direction = "high")
    return(if (high$auc >= low$auc) high else low)
  }

  scores_signed <- if (direction == "low") -scores else scores

  ord <- order(scores_signed, decreasing = TRUE)
  scores_ord <- scores_signed[ord]
  outcome_ord <- outcome[ord]

  tp <- cumsum(outcome_ord == 1)
  fp <- cumsum(outcome_ord == 0)

  n_pos <- sum(outcome == 1)
  n_neg <- sum(outcome == 0)

  tpr <- if (n_pos > 0) tp / n_pos else rep(0, length(tp))
  fpr <- if (n_neg > 0) fp / n_neg else rep(0, length(fp))

  fpr_diff <- diff(c(0, fpr))
  tpr_lag <- c(0, tpr[-length(tpr)])
  auc <- sum(fpr_diff * (tpr + tpr_lag) / 2)

  thresholds <- if (direction == "low") -scores_ord else scores_ord

  list(
    thresholds = thresholds,
    tpr = tpr,
    fpr = fpr,
    auc = auc,
    direction = direction
  )
}

# find the optimal threshold given outcome and method
# @noRd
.find_threshold <- function(
  scores,
  outcome,
  method,
  target_specificity
) {
  roc <- .compute_roc(scores, outcome)

  idx <- if (method == "youden") {
    which.max(roc$tpr - roc$fpr)
  } else if (method == "f1") {
    n_pos <- sum(outcome == 1)
    n_neg <- sum(outcome == 0)
    precision <- (roc$tpr * n_pos) /
      (roc$tpr * n_pos + roc$fpr * n_neg)
    f1 <- 2 * precision * roc$tpr / (precision + roc$tpr)
    f1[!is.finite(f1)] <- 0
    which.max(f1)
  } else {
    # fixed_specificity
    specificity <- 1 - roc$fpr
    which.min(abs(specificity - target_specificity))
  }

  roc$thresholds[idx]
}

# 2x2 confusion stats for a binary prediction
# @noRd
.confusion_stats <- function(predicted, outcome) {
  tp <- sum(predicted == 1 & outcome == 1)
  fp <- sum(predicted == 1 & outcome == 0)
  fn <- sum(predicted == 0 & outcome == 1)
  tn <- sum(predicted == 0 & outcome == 0)

  safe <- function(num, den) if (den == 0) NA_real_ else num / den

  tibble::tibble(
    sensitivity = safe(tp, tp + fn),
    specificity = safe(tn, tn + fp),
    ppv = safe(tp, tp + fp),
    npv = safe(tn, tn + fn),
    tp = tp,
    fp = fp,
    fn = fn,
    tn = tn
  )
}

# cli report block reused by the function-level verbose tail and
# print.blindspot_classify
# @noRd
.report_classify <- function(x) {
  fmt_int <- function(v) format(v, big.mark = ",")
  fmt_pct <- function(v) sprintf("%.1f%%", v * 100)

  th_med <- round(x$threshold$median, 3)
  th_lo <- round(x$threshold$q05, 3)
  th_hi <- round(x$threshold$q95, 3)

  # AUC with optional bootstrap 95% CI
  auc_spi_str <- if (!is.null(x$roc_spi$auc_ci)) {
    ci <- x$roc_spi$auc_ci
    sprintf(
      "%.3f (95%% CI %.3f-%.3f)",
      x$roc_spi$auc,
      ci[1],
      ci[2]
    )
  } else {
    sprintf("%.3f", x$roc_spi$auc)
  }

  cli::cli_h2("Blindspot classification")
  cli::cli_bullets(c(
    "i" = "Threshold method: {.val {x$threshold$method}}.",
    "i" = "Threshold (median): {th_med} (90% CrI {th_lo}-{th_hi}).",
    "i" = "ROC AUC (SPI): {auc_spi_str}."
  ))

  if (!is.null(x$roc_baseline)) {
    auc_base_str <- if (!is.null(x$roc_baseline$auc_ci)) {
      ci <- x$roc_baseline$auc_ci
      sprintf(
        "%.3f (95%% CI %.3f-%.3f)",
        x$roc_baseline$auc,
        ci[1],
        ci[2]
      )
    } else {
      sprintf("%.3f", x$roc_baseline$auc)
    }
    delta <- x$roc_spi$auc - x$roc_baseline$auc
    cli::cli_bullets(c(
      "i" = "ROC AUC (raw-count baseline): {auc_base_str}.",
      "i" = "AUC improvement: {sprintf('%+.3f', delta)}."
    ))
  }

  cli::cli_h3("Classification at posterior median threshold")
  class_tbl <- x$classification |>
    dplyr::count(.data$class) |>
    dplyr::mutate(
      pct = round(.data$n / sum(.data$n) * 100, 1)
    )
  print(class_tbl)

  cli::cli_h3("Operating characteristics")
  p <- x$performance
  cli::cli_bullets(c(
    "*" = "Sensitivity: {fmt_pct(p$sensitivity)}.",
    "*" = "Specificity: {fmt_pct(p$specificity)}.",
    "*" = "PPV: {fmt_pct(p$ppv)} | NPV: {fmt_pct(p$npv)}.",
    "*" = "TP: {fmt_int(p$tp)} | FP: {fmt_int(p$fp)} | \\
           FN: {fmt_int(p$fn)} | TN: {fmt_int(p$tn)}."
  ))

  invisible(x)
}

#' @export
print.blindspot_classify <- function(x, ...) {
  rlang::check_dots_empty()
  .report_classify(x)
  invisible(x)
}

#' Diagnostic summary for a `blindspot_classify` object
#'
#' @description
#' Produces a tibble of diagnostic statistics that flag classifier
#' quality (ROC AUC), threshold precision, improvement over baseline,
#' and outcome stability. Each metric gets a `pass` / `flag` indicator
#' and an interpretive footnote.
#'
#' Pass / flag rules:
#' \itemize{
#'   \item ROC AUC (SPI) >= 0.65 (informative discrimination)
#'   \item Threshold CrI width / median <= 0.5 (constrained)
#'   \item AUC delta (SPI - baseline) >= 0.05 (meaningful gain), when
#'     a baseline is available
#'   \item n outcome events >= 30 (stable bootstrap)
#' }
#'
#' @param object Object of class `blindspot_classify`.
#' @param ... Unused.
#'
#' @return A tibble of named diagnostics with pass / flag indicators,
#'   returned invisibly. Printing happens as a side effect.
#'
#' @family blindspot core functions
#' @export
summary.blindspot_classify <- function(object, ...) {
  rlang::check_dots_empty()

  th_med <- object$threshold$median
  th_width <- object$threshold$q95 - object$threshold$q05
  th_rel <- if (th_med > 0) th_width / th_med else NA_real_

  auc_spi <- object$roc_spi$auc
  auc_base <- if (!is.null(object$roc_baseline)) {
    object$roc_baseline$auc
  } else {
    NA_real_
  }
  auc_delta <- if (!is.na(auc_base)) auc_spi - auc_base else NA_real_

  p <- object$performance
  class_counts <- table(
    factor(
      object$classification$class,
      levels = c("blind_spot", "uncertain", "performing")
    )
  )

  # flags reused in both the tibble and the footnotes
  auc_flag <- .flag_within(auc_spi, 0.65, 1.0)
  width_flag <- .flag_within(th_rel, 0, 0.5)
  delta_flag <- if (is.na(auc_delta)) {
    NA_character_
  } else if (auc_delta >= 0.05) {
    "pass"
  } else {
    "flag"
  }
  n_outcome_flag <- if (object$n_outcome >= 30) "pass" else "flag"

  fmt <- function(v) {
    format(v, big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  diagnostics <- tibble::tibble(
    metric = c(
      "threshold (median)",
      "threshold 90% CrI width",
      "ROC AUC (SPI)",
      "ROC AUC (baseline)",
      "AUC delta (SPI - baseline)",
      "sensitivity (%)",
      "specificity (%)",
      "PPV (%)",
      "NPV (%)",
      "n outcome events",
      "n district-years",
      "outcome prevalence (%)",
      "n blind_spot",
      "n uncertain",
      "n performing"
    ),
    value = c(
      fmt(round(th_med, 3)),
      fmt(round(th_width, 3)),
      fmt(round(auc_spi, 3)),
      if (is.na(auc_base)) NA_character_ else fmt(round(auc_base, 3)),
      if (is.na(auc_delta)) NA_character_ else sprintf("%+.3f", auc_delta),
      fmt(round(p$sensitivity * 100, 1)),
      fmt(round(p$specificity * 100, 1)),
      fmt(round(p$ppv * 100, 1)),
      fmt(round(p$npv * 100, 1)),
      fmt(object$n_outcome),
      fmt(object$n_total),
      fmt(round(object$n_outcome / object$n_total * 100, 1)),
      fmt(unname(class_counts[["blind_spot"]])),
      fmt(unname(class_counts[["uncertain"]])),
      fmt(unname(class_counts[["performing"]]))
    ),
    flag = c(
      NA,
      width_flag,
      auc_flag,
      NA,
      delta_flag,
      NA,
      NA,
      NA,
      NA,
      n_outcome_flag,
      NA,
      NA,
      NA,
      NA,
      NA
    )
  )

  cli::cli_h2("Blindspot classification diagnostics")
  cli::cli_alert_info(
    "Method: {.val {object$threshold$method}}."
  )
  print(diagnostics)

  # --- footnotes ----------------------------------------
  cli::cli_h3("Footnotes")
  cli::cli_bullets(c(
    "i" = .note_auc(auc_spi, auc_flag),
    "i" = .note_threshold_width(th_med, th_width, th_rel, width_flag),
    "i" = .note_auc_delta(auc_spi, auc_base, auc_delta, delta_flag),
    "i" = .note_outcome_count(object$n_outcome, n_outcome_flag)
  ))

  invisible(diagnostics)
}

#' Coerce a `blindspot_classify` object to a tibble
#'
#' @param x Object of class `blindspot_classify`.
#' @param ... Unused.
#' @return The `classification` tibble (one row per district-year with
#'   observed counts, posterior SPI quantiles, and the empirical class
#'   label).
#' @family blindspot core functions
#' @export
as_tibble.blindspot_classify <- function(x, ...) {
  rlang::check_dots_empty()
  x$classification
}

#' @export
as.data.frame.blindspot_classify <- function(x, ...) {
  rlang::check_dots_empty()
  as.data.frame(x$classification)
}

# --- footnote builders ------------------------------------

# @noRd
.note_auc <- function(val, flag) {
  band <- if (val >= 0.80) "strong" else if (val >= 0.65) "moderate" else if (
    val >= 0.55
  )
    "weak" else "essentially random"

  fmt <- if (flag == "pass") {
    paste0(
      "ROC AUC %.3f -> pass. %s discrimination of subsequent ",
      "virus detection."
    )
  } else {
    paste0(
      "ROC AUC %.3f -> flag. %s discrimination; lag, outcome ",
      "type, or signal-to-noise may be limiting."
    )
  }
  sprintf(fmt, val, sub("^([a-z])", "\\U\\1", band, perl = TRUE))
}

# @noRd
.note_threshold_width <- function(med, width, rel, flag) {
  if (flag == "pass") {
    fmt <- paste0(
      "Threshold median %.3f, 90%% CrI width %.3f (~%g%% of ",
      "the median) -> pass. CrI is constrained enough to ",
      "support a confident operational cut-point."
    )
  } else {
    fmt <- paste0(
      "Threshold median %.3f, 90%% CrI width %.3f (~%g%% of ",
      "the median) -> flag. CrI is wide; treat the cut-point ",
      "as provisional and consider more posterior draws or a ",
      "larger outcome sample."
    )
  }
  sprintf(fmt, med, width, round(rel * 100, 1))
}

# @noRd
.note_auc_delta <- function(auc_spi, auc_base, delta, flag) {
  if (is.na(delta)) {
    return(
      "AUC delta vs baseline -> not computed (compare_baseline = FALSE)."
    )
  }
  if (flag == "pass") {
    fmt <- paste0(
      "AUC delta %+.3f (SPI %.3f vs baseline %.3f) -> pass. ",
      "Meaningful improvement over the raw-count baseline."
    )
  } else if (delta >= 0) {
    fmt <- paste0(
      "AUC delta %+.3f (SPI %.3f vs baseline %.3f) -> flag. ",
      "Improvement is marginal (< 0.05); SPI may not add much ",
      "over the raw count for this outcome."
    )
  } else {
    fmt <- paste0(
      "AUC delta %+.3f (SPI %.3f vs baseline %.3f) -> flag. ",
      "Baseline outperforms SPI; check the lag direction and ",
      "the outcome definition."
    )
  }
  sprintf(fmt, delta, auc_spi, auc_base)
}

# @noRd
.note_outcome_count <- function(n, flag) {
  if (flag == "pass") {
    fmt <- paste0(
      "%d outcome events -> pass. Adequate sample for stable ",
      "threshold bootstrap."
    )
  } else {
    fmt <- paste0(
      "%d outcome events -> flag. Bootstrap threshold CrI will ",
      "be unstable below ~30 events; consider pooling outcome ",
      "types or extending the study period."
    )
  }
  sprintf(fmt, n)
}

#' Plot ROC curves for a `blindspot_classify` object
#'
#' @param x Object of class `blindspot_classify`.
#' @param compare Logical. If TRUE (default), overlays the raw-count
#'   baseline ROC curve alongside the SPI ROC curve. Silently ignored if
#'   the baseline wasn't computed.
#' @param ... Unused.
#'
#' @return A ggplot object styled with the package theme (steelblue for
#'   SPI, burgundy for the baseline).
#'
#' @family blindspot core functions
#' @export
plot.blindspot_classify <- function(x, compare = TRUE, ...) {
  rlang::check_dots_empty()
  .check_pkg("ggplot2", reason = "to plot a blindspot_classify object")

  spi_curve <- tibble::tibble(
    fpr = x$roc_spi$fpr,
    tpr = x$roc_spi$tpr,
    metric = glue::glue(
      "SPI (AUC = {round(x$roc_spi$auc, 3)})"
    )
  )

  curves <- spi_curve
  if (compare && !is.null(x$roc_baseline)) {
    baseline_curve <- tibble::tibble(
      fpr = x$roc_baseline$fpr,
      tpr = x$roc_baseline$tpr,
      metric = glue::glue(
        "Raw count (AUC = {round(x$roc_baseline$auc, 3)})"
      )
    )
    curves <- dplyr::bind_rows(spi_curve, baseline_curve)
  }

  # ensure SPI is the first level so it gets the primary colour
  curves$metric <- factor(
    curves$metric,
    levels = unique(curves$metric)
  )
  pal <- c(.bs_palette$primary_dark, .bs_palette$burgundy)

  ggplot2::ggplot(
    curves,
    ggplot2::aes(
      x = .data$fpr,
      y = .data$tpr,
      colour = .data$metric
    )
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = "grey60",
      linewidth = 0.5
    ) +
    ggplot2::geom_step(linewidth = 0.9) +
    ggplot2::scale_colour_manual(
      values = pal[seq_len(nlevels(curves$metric))]
    ) +
    ggplot2::labs(
      title = "ROC: blindspot SPI vs raw-count baseline",
      subtitle = .roc_subtitle(x),
      x = "1 - specificity",
      y = "sensitivity",
      colour = NULL
    ) +
    .bs_theme() +
    ggplot2::theme(legend.position = "bottom")
}

# build the ROC plot subtitle from the lag + outcome stored on the
# blindspot_classify object. Falls back to a generic phrase if either
# slot is missing (e.g. when an older object is replayed).
# @noRd
.roc_subtitle <- function(x) {
  lag <- if (is.null(x$lag)) NA_integer_ else x$lag
  out <- if (is.null(x$outcome_type)) "virus" else x$outcome_type

  if (is.na(lag)) {
    "predicting subsequent virus detection"
  } else {
    lag_word <- ngettext(lag, "year", "years")
    sprintf(
      "predicting %s detection at lag %d %s",
      out,
      lag,
      lag_word
    )
  }
}

# --- lag sweep helpers and methods ------------------------

# run bs_classify() across a range of lags and return a
# blindspot_classify_sweep object. Sub-calls run quiet; sweep-level
# progress is reported here.
# @noRd
.run_lag_sweep <- function(
  spi,
  virus_outcome,
  lags,
  outcome_type,
  threshold_method,
  target_specificity,
  compare_baseline,
  baseline,
  baseline_direction,
  n_bootstrap,
  auc_ci,
  auc_ci_n,
  seed,
  verbose,
  user_call
) {
  if (verbose) {
    lag_str <- paste(lags, collapse = ", ")
    cli::cli_h2(
      "Lag sweep: outcome = {.val {outcome_type}}, lags = {lag_str}"
    )
  }

  fits <- vector("list", length(lags))
  names(fits) <- as.character(lags)

  for (i in seq_along(lags)) {
    l <- lags[i]
    if (verbose) {
      cli::cli_progress_step(
        "Fitting at lag = {l} ({i}/{length(lags)})"
      )
    }
    fits[[i]] <- bs_classify(
      spi = spi,
      virus_outcome = virus_outcome,
      lag = l,
      lag_sweep = NULL,
      outcome_type = outcome_type,
      threshold_method = threshold_method,
      target_specificity = target_specificity,
      compare_baseline = compare_baseline,
      baseline = baseline,
      baseline_direction = baseline_direction,
      n_bootstrap = n_bootstrap,
      auc_ci = auc_ci,
      auc_ci_n = auc_ci_n,
      seed = seed,
      verbose = FALSE
    )
  }
  if (verbose) {
    cli::cli_progress_done()
  }

  # per-lag summary row
  summary_tbl <- dplyr::bind_rows(lapply(fits, function(f) {
    base_auc <- if (!is.null(f$roc_baseline)) {
      f$roc_baseline$auc
    } else {
      NA_real_
    }
    delta <- if (!is.na(base_auc)) f$roc_spi$auc - base_auc else NA_real_
    spi_ci <- if (!is.null(f$roc_spi$auc_ci)) {
      f$roc_spi$auc_ci
    } else {
      c(NA_real_, NA_real_)
    }
    base_ci <- if (!is.null(f$roc_baseline$auc_ci)) {
      f$roc_baseline$auc_ci
    } else {
      c(NA_real_, NA_real_)
    }
    tibble::tibble(
      lag = f$lag,
      n_outcome = f$n_outcome,
      n_total = f$n_total,
      auc_spi = f$roc_spi$auc,
      auc_spi_q025 = spi_ci[1],
      auc_spi_q975 = spi_ci[2],
      auc_baseline = base_auc,
      auc_baseline_q025 = base_ci[1],
      auc_baseline_q975 = base_ci[2],
      auc_delta = delta,
      threshold_median = f$threshold$median,
      threshold_q05 = f$threshold$q05,
      threshold_q95 = f$threshold$q95,
      sensitivity = f$performance$sensitivity,
      specificity = f$performance$specificity
    )
  }))

  best_lag <- summary_tbl$lag[which.max(summary_tbl$auc_spi)]

  result <- structure(
    list(
      fits = fits,
      summary = summary_tbl,
      best_lag = best_lag,
      outcome_type = outcome_type,
      id_col = fits[[1]]$id_col,
      call = user_call
    ),
    class = "blindspot_classify_sweep"
  )

  if (verbose) {
    .report_classify_sweep(result)
  }

  result
}

# cli report block reused by the function-level verbose tail and
# print.blindspot_classify_sweep
# @noRd
.report_classify_sweep <- function(x) {
  fmt <- function(v) {
    format(v, big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  best <- x$summary[x$summary$lag == x$best_lag, ]
  best_auc <- round(best$auc_spi, 3)

  cli::cli_h2("Blindspot classification - lag sweep")
  cli::cli_bullets(c(
    "i" = "Outcome: {.val {x$outcome_type}}.",
    "i" = "Lags: {paste(x$summary$lag, collapse = ', ')}.",
    "i" = "Best lag (by SPI AUC): {.val {x$best_lag}} \\
           (AUC = {best_auc})."
  ))

  # display tibble: round numerics and add big-mark formatting
  display <- x$summary |>
    dplyr::mutate(
      auc_spi = round(.data$auc_spi, 3),
      auc_baseline = round(.data$auc_baseline, 3),
      auc_delta = round(.data$auc_delta, 3),
      threshold_median = round(.data$threshold_median, 3),
      threshold_q05 = round(.data$threshold_q05, 3),
      threshold_q95 = round(.data$threshold_q95, 3),
      sensitivity = round(.data$sensitivity * 100, 1),
      specificity = round(.data$specificity * 100, 1)
    )
  print(display)

  invisible(x)
}

#' @export
print.blindspot_classify_sweep <- function(x, ...) {
  rlang::check_dots_empty()
  .report_classify_sweep(x)
  invisible(x)
}

#' Coerce a `blindspot_classify_sweep` to a tibble
#'
#' @param x Object of class `blindspot_classify_sweep`.
#' @param ... Unused.
#' @return The per-lag summary tibble.
#' @family blindspot core functions
#' @export
as_tibble.blindspot_classify_sweep <- function(x, ...) {
  rlang::check_dots_empty()
  x$summary
}

#' @export
as.data.frame.blindspot_classify_sweep <- function(x, ...) {
  rlang::check_dots_empty()
  as.data.frame(x$summary)
}

#' Plot ROC curves across a lag sweep
#'
#' @description
#' Default `type = "roc"` produces a facet-wrapped ROC plot, one panel per
#' lag, with SPI and (optionally) the raw-count baseline overlaid. Each
#' facet strip includes the lag's SPI AUC so the eye can track how
#' discrimination evolves with lag. `type = "auc"` produces a line chart
#' of AUC against lag (useful for picking the operational lag).
#'
#' @param x Object of class `blindspot_classify_sweep`.
#' @param type Character. `"roc"` (default) for faceted ROC curves;
#'   `"auc"` for an AUC vs lag line plot.
#' @param ncol Integer. Number of facet columns when `type = "roc"`.
#'   Default: NULL (ggplot picks).
#' @param ... Unused.
#'
#' @return A ggplot object.
#'
#' @family blindspot core functions
#' @export
plot.blindspot_classify_sweep <- function(
  x,
  type = c("roc", "auc"),
  ncol = NULL,
  ...
) {
  rlang::check_dots_empty()
  .check_pkg(
    "ggplot2",
    reason = "to plot a blindspot_classify_sweep object"
  )
  type <- match.arg(type)

  if (type == "roc") {
    .plot_sweep_roc(x, ncol)
  } else {
    .plot_sweep_auc(x)
  }
}

# faceted ROC plot — one panel per lag, SPI + baseline overlay
# @noRd
.plot_sweep_roc <- function(x, ncol) {
  has_baseline <- any(!is.na(x$summary$auc_baseline))

  curves_list <- lapply(x$fits, function(f) {
    spi <- tibble::tibble(
      lag = f$lag,
      fpr = f$roc_spi$fpr,
      tpr = f$roc_spi$tpr,
      metric = "SPI"
    )
    base <- if (!is.null(f$roc_baseline)) {
      tibble::tibble(
        lag = f$lag,
        fpr = f$roc_baseline$fpr,
        tpr = f$roc_baseline$tpr,
        metric = "Raw count"
      )
    } else {
      NULL
    }
    dplyr::bind_rows(spi, base)
  })
  curves <- dplyr::bind_rows(curves_list)

  # facet label with SPI AUC (and baseline AUC if available)
  curves <- curves |>
    dplyr::left_join(
      dplyr::transmute(
        x$summary,
        lag = .data$lag,
        facet_label = .make_facet_label(
          .data$lag,
          .data$auc_spi,
          .data$auc_baseline
        )
      ),
      by = "lag"
    )
  curves$facet_label <- factor(
    curves$facet_label,
    levels = unique(curves$facet_label[order(curves$lag)])
  )

  metric_levels <- if (has_baseline) c("SPI", "Raw count") else "SPI"
  curves$metric <- factor(curves$metric, levels = metric_levels)
  pal <- c(
    "SPI" = .bs_palette$primary_dark,
    "Raw count" = .bs_palette$burgundy
  )

  ggplot2::ggplot(
    curves,
    ggplot2::aes(
      x = .data$fpr,
      y = .data$tpr,
      colour = .data$metric
    )
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = "grey60",
      linewidth = 0.5
    ) +
    ggplot2::geom_step(linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = pal[metric_levels]) +
    ggplot2::facet_wrap(~ .data$facet_label, ncol = ncol) +
    ggplot2::labs(
      title = "ROC curves across lag sweep",
      subtitle = sprintf(
        "predicting %s detection",
        x$outcome_type
      ),
      x = "1 - specificity",
      y = "sensitivity",
      colour = NULL
    ) +
    .bs_theme() +
    ggplot2::theme(legend.position = "bottom")
}

# AUC vs lag line plot
# @noRd
.plot_sweep_auc <- function(x) {
  has_baseline <- any(!is.na(x$summary$auc_baseline))
  has_ci <- any(!is.na(x$summary$auc_spi_q025))

  long <- dplyr::bind_rows(
    tibble::tibble(
      lag = x$summary$lag,
      auc = x$summary$auc_spi,
      auc_q025 = x$summary$auc_spi_q025,
      auc_q975 = x$summary$auc_spi_q975,
      metric = "SPI"
    ),
    if (has_baseline) {
      tibble::tibble(
        lag = x$summary$lag,
        auc = x$summary$auc_baseline,
        auc_q025 = x$summary$auc_baseline_q025,
        auc_q975 = x$summary$auc_baseline_q975,
        metric = "Raw count"
      )
    } else {
      NULL
    }
  )

  metric_levels <- if (has_baseline) c("SPI", "Raw count") else "SPI"
  long$metric <- factor(long$metric, levels = metric_levels)
  pal <- c(
    "SPI" = .bs_palette$primary_dark,
    "Raw count" = .bs_palette$burgundy
  )

  # dodge the two metrics horizontally so error bars don't overlap
  pd <- ggplot2::position_dodge(width = 0.25)

  p <- ggplot2::ggplot(
    long,
    ggplot2::aes(
      x = .data$lag,
      y = .data$auc,
      colour = .data$metric
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      colour = "grey60",
      linewidth = 0.5
    ) +
    ggplot2::geom_hline(
      yintercept = 0.65,
      linetype = "dotted",
      colour = .bs_palette$burgundy,
      linewidth = 0.5
    )

  if (has_ci) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = .data$auc_q025,
          ymax = .data$auc_q975
        ),
        width = 0.15,
        linewidth = 0.5,
        alpha = 0.85,
        position = pd
      )
  }

  p +
    ggplot2::geom_line(linewidth = 0.9, position = pd) +
    ggplot2::geom_point(size = 2.4, position = pd) +
    ggplot2::scale_colour_manual(values = pal[metric_levels]) +
    ggplot2::scale_x_continuous(breaks = x$summary$lag) +
    ggplot2::labs(
      title = "ROC AUC across lags",
      subtitle = sprintf(
        "predicting %s detection (bars = 95%% bootstrap CI)",
        x$outcome_type
      ),
      x = "lag (years)",
      y = "ROC AUC",
      colour = NULL
    ) +
    .bs_theme() +
    ggplot2::theme(legend.position = "bottom")
}

# facet label builder used by .plot_sweep_roc
# @noRd
.make_facet_label <- function(lag, auc_spi, auc_baseline) {
  base_part <- dplyr::if_else(
    is.na(auc_baseline),
    "",
    sprintf(" | baseline %.3f", auc_baseline)
  )
  sprintf(
    "Lag %d (SPI AUC %.3f%s)",
    lag,
    auc_spi,
    base_part
  )
}
