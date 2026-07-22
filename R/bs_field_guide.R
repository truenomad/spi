#' Read a low SPI through the seven-signal field guide
#'
#' @description
#' Turns a [bs_concordance()] result into an operational reading of every
#' district-year: the seven interpretation signals (S1-S7) and a
#' three-level verdict (`FLAG`, `WATCH`, `No action`). It automates the
#' paper's *"Interpreting and acting on the SPI"* field guide, which
#' separates a genuine, sustained detection shortfall from statistical
#' noise or a low-expectation artefact.
#'
#' The seven signals, each ruling out a different alternative explanation:
#' \itemize{
#'   \item **S1 Credible discordance** -- conventionally adequate yet
#'     sub-threshold on SPI with the 90% credible interval excluding 1.
#'   \item **S2 Depth of shortfall** -- how far the posterior median SPI
#'     sits below the cut.
#'   \item **S3 Observed vs expected** -- few observed cases against a
#'     substantial model-expected count.
#'   \item **S4 Trajectory** -- the SPI-vs-year slope over the last
#'     `traj_window` years (falling / flat / rising); set `traj_alpha` to
#'     significance-gate it into a trend test so a single anomalous year is not
#'     read as a decline.
#'   \item **S5 Persistence** -- the longest run of consecutive years below
#'     the cut up to that year.
#'   \item **S6 Neighbour contrast** -- under-detection while immediate
#'     neighbours perform adequately (needs `adjacency`).
#'   \item **S7 Seasonal and detection corroboration** -- detection flat
#'     through the expected peak months (needs `spi_month`), and any
#'     poliovirus detected there through the case-based (AFP) channel (needs
#'     `genomic`) or environmental surveillance (needs `es`).
#' }
#'
#' @details
#' A district-year is **flagged** when three conditions hold together: the
#' posterior median SPI is below `spi_cut`, the upper bound of the 90%
#' credible interval is below 1, and at least `min_corroborators` of four
#' corroborating signals fire (a falling trajectory, persistence of at least
#' `persistence` years, neighbour discordance, or seasonal blindness). A
#' district below the cut whose credible interval still reaches 1 is kept at
#' **watch** rather than flagged; everything else needs **no action**.
#' Detection corroboration (AFP or ES) strengthens a flag in the narrative but
#' is never one of the four corroborators, matching the paper.
#'
#' Three settings refine the reading; all are off by default, so the field
#' guide reproduces the paper's published spec out of the box (bare slope-sign
#' S4, four separately-counted signals). `dedupe_temporal` collapses the
#' trajectory (S4) and persistence (S5) signals into one, so a flag cannot rest
#' on two readings of the same decline; `detection_corroborates` promotes an
#' AFP or ES detection to a counted, independent signal; and `traj_alpha`
#' significance-gates S4 into a trend test (0.1 is the recommended value) so a
#' single anomalous or endpoint-only year no longer reads as a sustained
#' decline. Each changes flag counts against the published numbers, which is
#' why each is opt-in.
#'
#' Signals that need optional inputs degrade gracefully: without `adjacency`,
#' `spi_month`, `genomic`, or `es` the corresponding signal is `NA` and
#' contributes no corroborator. `signals_active` records which were computable.
#'
#' @param concordance A [bs_concordance()] result (class
#'   `blindspot_concordance`). Supplies fused SPI + NPAFP for every
#'   district-year plus the thresholds.
#' @param adjacency Optional spatial neighbour object from [bs_adjacency()]
#'   (class `blindspot_nb`). Enables S6. Default: NULL.
#' @param spi_month Optional [bs_spi()] result at `district_month` level.
#'   Enables the seasonal half of S7. Default: NULL.
#' @param genomic Optional tibble of case-based (AFP) poliovirus detections
#'   with the district id column and `year`. Enables the AFP half of the S7
#'   detection signal. Rows are detections; pass `genomic_col` to filter on a
#'   0/1 flag column. Default: NULL.
#' @param genomic_col Optional name of a logical / 0-1 column in `genomic`;
#'   only truthy rows count as detections. Default: NULL (every row counts).
#' @param es Optional tibble of environmental-surveillance (ES) detections with
#'   the district id column and `year`. Enables the ES half of the S7 detection
#'   signal. Rows are positives; pass `es_col` to filter on a count / flag
#'   column. Default: NULL.
#' @param es_col Optional name of a count / logical column in `es`; only rows
#'   with a positive count (or `TRUE`) count as detections. Default: NULL
#'   (every row counts).
#' @param read_year Integer focal year for `print()` and the default table /
#'   help rendering. Default: NULL (latest year present).
#' @param spi_cut Numeric SPI adequacy cut. Default: NULL (take
#'   `concordance$thresholds$spi`).
#' @param persistence Integer. Consecutive sub-cut years that corroborate a
#'   flag (S5). Default: 3.
#' @param traj_window Integer. Trajectory regression window in years, ending
#'   at each year (S4). Default: 5.
#' @param traj_tol Numeric slope dead-band per year below which a trajectory
#'   is "flat" (S4). Default: 0.01.
#' @param traj_alpha Optional numeric significance level for S4. When set, a
#'   trajectory is only "falling" or "rising" if its OLS slope differs from
#'   zero at this two-sided level (needing at least three points in the
#'   window); otherwise it is "flat". This guards against a single anomalous or
#'   volatile year reading as a sustained trend. Default: NULL (the paper's
#'   slope-sign-and-tolerance trajectory, which reproduces the published flag
#'   counts). `0.1` is the recommended setting, improving specificity on
#'   volatile series.
#' @param min_corroborators Integer. Corroborating signals required to flag.
#'   Default: 2.
#' @param dedupe_temporal Logical. Count a falling trajectory (S4) and a
#'   persistent sub-cut run (S5) as a single "temporal" corroborator rather
#'   than two, since over a short window they can re-read the same decline.
#'   Default: FALSE (they count separately, matching the paper).
#' @param detection_corroborates Logical. Count an AFP or ES detection (S7) as
#'   an independent corroborating signal. Default: FALSE (detections strengthen
#'   a flag in the narrative but never enter the count, matching the paper).
#' @param id_col Character district id column. Default: NULL (take
#'   `concordance$id_col`).
#' @param verbose Logical. Progress and degradation messages via cli.
#'   Default: TRUE.
#'
#' @return An object of class `blindspot_field_guide`. A list with:
#' \describe{
#'   \item{district_year}{Tibble, one row per district-year, carrying the
#'     inputs plus every signal value (`spi_below`, `cri_excludes_1`,
#'     `s1_discordance`, `longest_run_below`, `trajectory`, `neighbour_spi`,
#'     `neighbour_discordant`, `neighbourhood_shortfall`, `seasonal`,
#'     `orphan_years`, `genomic_orphan`, `es_years`, `es_detected`),
#'     `corroborators`, and `verdict` (factor).}
#'   \item{focal}{The `read_year` slice of `district_year`.}
#'   \item{reference}{The seven-signal reference tibble (what each asks /
#'     rules out).}
#'   \item{read_year, thresholds, params, signals_active, id_col, call}{
#'     Metadata: focal year, `spi_cut` / `npafp_target`, the tuning
#'     parameters, which optional signals were computed, the id column, and
#'     the matched call.}
#' }
#'
#' @seealso [bs_field_guide_table()] to render it, [bs_field_guide_help()]
#'   to learn to read it, and [bs_concordance()] for the input.
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' # a precomputed field guide on the synthetic bundle ships with the package
#' fg <- synth_field_guide
#' print(fg)
#' head(as_tibble(fg))
#' \dontrun{
#' # build one from scratch (needs INLA for the upstream fit)
#' adj  <- bs_adjacency(synth_surveillance$boundaries, id_col = "adm2_guid")
#' fit  <- bs_expected(
#'   synth_surveillance$cases, synth_surveillance$population, adj,
#'   id_col = "adm2_guid"
#' )
#' cy   <- bs_spi(fit, level = "district_year")
#' cm   <- bs_spi(fit, level = "district_month")
#' conc <- bs_concordance(
#'   cy, synth_surveillance$cases, synth_surveillance$population,
#'   boundaries = synth_surveillance$boundaries
#' )
#' fg <- bs_field_guide(
#'   conc,
#'   adjacency = adj,
#'   spi_month = cm,
#'   genomic = dplyr::filter(
#'     synth_surveillance$virus_outcome, any_cvdpv2 == 1
#'   ),
#'   es = synth_surveillance$es_district_year,
#'   es_col = "n_positive"
#' )
#' }
bs_field_guide <- function(
  concordance,
  adjacency = NULL,
  spi_month = NULL,
  genomic = NULL,
  genomic_col = NULL,
  es = NULL,
  es_col = NULL,
  read_year = NULL,
  spi_cut = NULL,
  persistence = 3L,
  traj_window = 5L,
  traj_tol = 0.01,
  traj_alpha = NULL,
  min_corroborators = 2L,
  dedupe_temporal = FALSE,
  detection_corroborates = FALSE,
  id_col = NULL,
  verbose = TRUE
) {
  .check_pkg(
    c("dplyr", "tibble", "lubridate", "cli"),
    reason = "to read the SPI through the field guide"
  )

  stopifnot(inherits(concordance, "blindspot_concordance"))

  id_col <- id_col %||% concordance$id_col %||% "district_id"
  spi_cut <- spi_cut %||% concordance$thresholds$spi
  npafp_target <- concordance$thresholds$npafp

  dy <- concordance$district_year
  required <- c(id_col, "year", "observed", "expected_total", "spi_median",
                "spi_q05", "spi_q95", "npafp_rate", "npafp_adequate")
  missing_cols <- setdiff(required, names(dy))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg concordance$district_year} is missing column{?s} \\
       {.val {missing_cols}}."
    )
  }

  if (is.null(read_year)) read_year <- max(dy$year, na.rm = TRUE)
  read_year <- as.integer(read_year)
  if (!read_year %in% dy$year) {
    cli::cli_abort(
      "{.arg read_year} = {.val {read_year}} has no district-year rows."
    )
  }

  # --- year-invariant flags -------------------------------------------
  dy <- dy |>
    dplyr::mutate(
      conventional_pass = .data$npafp_adequate,
      spi_below = .data$spi_median < spi_cut,
      cri_excludes_1 = .data$spi_q95 < 1,
      s1_discordance = .data$conventional_pass & .data$spi_below &
        .data$cri_excludes_1
    )

  # --- S5 persistence + S4 trajectory (per district, cumulative in year) ---
  dy <- .fg_add_persistence(dy, id_col, spi_cut)
  dy <- .fg_add_trajectory(dy, id_col, traj_window, traj_tol, traj_alpha)

  # --- S6 neighbour contrast ------------------------------------------
  have_adjacency <- !is.null(adjacency)
  if (have_adjacency) {
    dy <- .fg_add_neighbour(dy, adjacency, id_col, spi_cut)
  } else {
    dy$neighbour_spi <- NA_real_
    dy$island <- NA
    dy$neighbour_discordant <- NA
    dy$neighbourhood_shortfall <- NA
  }

  # --- S7 seasonal -----------------------------------------------------
  have_seasonal <- !is.null(spi_month)
  if (have_seasonal) {
    stopifnot(inherits(spi_month, "blindspot_spi"))
    if (!identical(spi_month$level, "district_month")) {
      cli::cli_abort(
        "{.arg spi_month} must be a {.fn bs_spi} result at \\
         {.val district_month} level; got {.val {spi_month$level}}."
      )
    }
    dy <- .fg_add_seasonal(dy, spi_month, id_col)
  } else {
    dy$seasonal <- NA_character_
  }
  dy$seasonally_blind <- !is.na(dy$seasonal) & dy$seasonal == "blind"

  # --- S7 detections: AFP (genomic) and ES -----------------------------
  # both are narrative corroboration; neither enters the corroborator count
  have_genomic <- !is.null(genomic)
  if (have_genomic) {
    dy <- .fg_add_genomic(dy, genomic, id_col, genomic_col)
  } else {
    dy$orphan_years <- NA_character_
    dy$genomic_orphan <- NA
  }
  have_es <- !is.null(es)
  if (have_es) {
    dy <- .fg_add_es(dy, es, id_col, es_col)
  } else {
    dy$es_years <- NA_character_
    dy$es_detected <- NA
  }

  # --- corroborators + verdict ----------------------------------------
  is_true <- function(x) !is.na(x) & x
  # the temporal axis (S4 falling, S5 persistence) is one corroborator when
  # deduped -- over a short window the two can re-read the same decline -- and
  # two otherwise (the paper's spec)
  temporal_falling <- dy$trajectory == "falling"
  temporal_persistent <- dy$longest_run_below >= persistence
  temporal <- if (dedupe_temporal) {
    as.integer(temporal_falling | temporal_persistent)
  } else {
    as.integer(temporal_falling) + as.integer(temporal_persistent)
  }
  # a detection is narrative corroboration by default; only when
  # detection_corroborates is it counted as an independent signal
  detection <- is_true(dy$genomic_orphan) | is_true(dy$es_detected)
  detection_corr <- if (detection_corroborates) as.integer(detection) else 0L
  dy$corroborators <- temporal +
    as.integer(is_true(dy$neighbour_discordant)) +
    as.integer(is_true(dy$seasonally_blind)) +
    detection_corr
  dy$verdict <- factor(
    dplyr::case_when(
      dy$spi_below & dy$cri_excludes_1 &
        dy$corroborators >= min_corroborators ~ "FLAG",
      dy$spi_below & !dy$cri_excludes_1 ~ "WATCH",
      TRUE ~ "No action"
    ),
    levels = c("FLAG", "WATCH", "No action")
  )

  signals_active <- c(
    s6_neighbour = have_adjacency,
    s7_seasonal = have_seasonal,
    s7_genomic = have_genomic,
    s7_es = have_es
  )

  if (verbose) {
    n_assessable <- (if (dedupe_temporal) 1L else 2L) +
      as.integer(have_adjacency) + as.integer(have_seasonal) +
      (if (detection_corroborates) as.integer(have_genomic || have_es) else 0L)
    off <- names(signals_active)[!signals_active]
    if (length(off) > 0) {
      cli::cli_alert_info(
        "Signals not computed (missing inputs): {.val {off}}."
      )
    }
    if (min_corroborators > n_assessable) {
      cli::cli_alert_warning(
        "{.arg min_corroborators} = {min_corroborators} exceeds the \\
         {n_assessable} assessable corroborator{?s}; no district can flag."
      )
    }
    foc <- dy[dy$year == read_year, ]
    cli::cli_alert_success(
      "Field guide read for {.val {read_year}}: \\
       {sum(foc$verdict == 'FLAG')} flag, \\
       {sum(foc$verdict == 'WATCH')} watch, \\
       {sum(foc$verdict == 'No action')} no-action."
    )
  }

  structure(
    list(
      district_year = tibble::as_tibble(dy),
      focal = tibble::as_tibble(dy[dy$year == read_year, ]),
      reference = .fg_reference(spi_cut),
      read_year = read_year,
      thresholds = list(spi = spi_cut, npafp = npafp_target),
      params = list(
        persistence = as.integer(persistence),
        traj_window = as.integer(traj_window),
        traj_tol = traj_tol,
        traj_alpha = traj_alpha,
        min_corroborators = as.integer(min_corroborators),
        dedupe_temporal = dedupe_temporal,
        detection_corroborates = detection_corroborates
      ),
      signals_active = signals_active,
      id_col = id_col,
      call = match.call()
    ),
    class = "blindspot_field_guide"
  )
}

# ---------------------------------------------------------------------------
# signal helpers
# ---------------------------------------------------------------------------

# S5: longest run of consecutive sub-cut years up to and including each year.
# @noRd
.fg_add_persistence <- function(dy, id_col, spi_cut) {
  dy |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) |>
    dplyr::arrange(.data$year, .by_group = TRUE) |>
    dplyr::mutate(
      longest_run_below = .fg_cum_max_run(.data$spi_median < spi_cut)
    ) |>
    dplyr::ungroup()
}

# running maximum consecutive-TRUE run length (NA treated as FALSE).
# @noRd
.fg_cum_max_run <- function(below) {
  below <- as.integer(below)
  below[is.na(below)] <- 0L
  out <- integer(length(below))
  cur <- 0L
  best <- 0L
  for (i in seq_along(below)) {
    cur <- if (below[i] == 1L) cur + 1L else 0L
    best <- max(best, cur)
    out[i] <- best
  }
  out
}

# S4: sign of the SPI-vs-year slope over the trailing `window` years. With
# `alpha` set, the slope must also be significantly different from zero (a real
# trend test) before it reads as falling / rising, else it is flat.
# @noRd
.fg_add_trajectory <- function(dy, id_col, window, tol, alpha = NULL) {
  slope_at <- function(year, val, focal) {
    w <- year >= (focal - window + 1L) & year <= focal
    yr <- year[w]
    vv <- val[w]
    ok <- is.finite(yr) & is.finite(vv)
    if (sum(ok) < 2L) return(NA_real_)
    unname(stats::coef(stats::lm(vv[ok] ~ yr[ok]))[2L])
  }
  # two-sided p-value for the slope; needs >= 3 points for a residual df
  pval_at <- function(year, val, focal) {
    w <- year >= (focal - window + 1L) & year <= focal
    yr <- year[w]
    vv <- val[w]
    ok <- is.finite(yr) & is.finite(vv)
    if (sum(ok) < 3L) return(NA_real_)
    fit <- stats::lm(vv[ok] ~ yr[ok])
    # a near-perfect fit warns ("summary may be unreliable") but its slope is
    # unambiguously significant; the warning is benign, so silence it
    tryCatch(
      suppressWarnings(stats::coef(summary(fit))[2L, 4L]),
      error = function(e) NA_real_
    )
  }
  dy <- dy |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) |>
    dplyr::mutate(
      traj_slope = vapply(
        .data$year,
        function(y) slope_at(.data$year, .data$spi_median, y),
        numeric(1)
      ),
      traj_p = if (is.null(alpha)) {
        NA_real_
      } else {
        vapply(
          .data$year,
          function(y) pval_at(.data$year, .data$spi_median, y),
          numeric(1)
        )
      }
    ) |>
    dplyr::ungroup()

  significant <- if (is.null(alpha)) {
    rep(TRUE, nrow(dy))
  } else {
    !is.na(dy$traj_p) & dy$traj_p < alpha
  }
  dy$trajectory <- dplyr::case_when(
    is.na(dy$traj_slope) ~ "flat",
    dy$traj_slope <= -tol & significant ~ "falling",
    dy$traj_slope >= tol & significant ~ "rising",
    TRUE ~ "flat"
  )
  dy
}

# S6: neighbour-median SPI per district-year, and discordance flag.
# @noRd
.fg_add_neighbour <- function(dy, adjacency, id_col, spi_cut) {
  ids <- attr(adjacency, "region.id")
  if (is.null(ids)) ids <- as.character(seq_along(adjacency))
  ids <- as.character(ids)

  edges <- do.call(rbind, lapply(seq_along(adjacency), function(i) {
    nb <- adjacency[[i]]
    if (length(nb) == 1L && nb == 0L) return(NULL)
    data.frame(
      focal = ids[i],
      neighbour = ids[nb],
      stringsAsFactors = FALSE
    )
  }))

  islands <- setdiff(ids, if (is.null(edges)) character(0) else edges$focal)

  if (is.null(edges)) {
    dy$neighbour_spi <- NA_real_
  } else {
    spi_lookup <- dy[, c(id_col, "year", "spi_median")]
    names(spi_lookup) <- c("neighbour", "year", "nb_spi")
    edges <- tibble::as_tibble(edges)
    # each neighbour spans many years and serves many focals: many-to-many
    nb_med <- dplyr::inner_join(
      edges, spi_lookup,
      by = "neighbour", relationship = "many-to-many"
    ) |>
      dplyr::group_by(.data$focal, .data$year) |>
      dplyr::summarise(
        neighbour_spi = stats::median(.data$nb_spi, na.rm = TRUE),
        .groups = "drop"
      )
    dy <- dplyr::left_join(
      dy, nb_med,
      by = stats::setNames(c("focal", "year"), c(id_col, "year"))
    )
  }

  dy |>
    dplyr::mutate(
      island = .data[[id_col]] %in% islands,
      neighbour_discordant = !is.na(.data$neighbour_spi) &
        .data$neighbour_spi >= spi_cut & .data$spi_median < spi_cut,
      # the absorption / self-benchmarking case: the district is short and so
      # is its neighbourhood, so the shortfall is region-wide, not local
      neighbourhood_shortfall = !is.na(.data$neighbour_spi) &
        .data$neighbour_spi < spi_cut & .data$spi_median < spi_cut
    )
}

# S7 seasonal: blind / muted / present through the expected peak months.
# @noRd
.fg_add_seasonal <- function(dy, spi_month, id_col) {
  sm <- spi_month$summary
  if (!all(c("observed", "expected_total", "month") %in% names(sm))) {
    dy$seasonal <- NA_character_
    return(dy)
  }
  sm$year <- lubridate::year(sm$month)
  seas <- sm |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_col, "year")))) |>
    dplyr::summarise(
      seasonal = .fg_seasonal_one(.data$observed, .data$expected_total),
      .groups = "drop"
    )
  dplyr::left_join(dy, seas, by = c(id_col, "year"))
}

# @noRd
.fg_seasonal_one <- function(observed, expected) {
  if (sum(expected, na.rm = TRUE) == 0) return("not assessed")
  peak_cut <- stats::quantile(expected, 2 / 3, names = FALSE, na.rm = TRUE)
  peak <- expected >= peak_cut
  obs_peak <- sum(observed[peak], na.rm = TRUE)
  exp_peak <- sum(expected[peak], na.rm = TRUE)
  if (obs_peak == 0) {
    "blind"
  } else if (exp_peak > 0 && obs_peak / exp_peak < 0.5) {
    "muted"
  } else {
    "present"
  }
}

# S7 genomic: orphan detection years up to and including each year.
# @noRd
.fg_add_genomic <- function(dy, genomic, id_col, genomic_col) {
  stopifnot(
    is.data.frame(genomic),
    id_col %in% names(genomic),
    "year" %in% names(genomic)
  )
  g <- genomic
  if (!is.null(genomic_col)) {
    if (!genomic_col %in% names(g)) {
      cli::cli_abort(
        "{.arg genomic_col} {.val {genomic_col}} not in {.arg genomic}."
      )
    }
    g <- g[as.logical(g[[genomic_col]]) %in% TRUE, , drop = FALSE]
  }
  orphan_map <- split(as.integer(g$year), as.character(g[[id_col]]))
  ids <- as.character(dy[[id_col]])
  yrs <- as.integer(dy$year)
  dy$orphan_years <- vapply(seq_along(ids), function(i) {
    y <- orphan_map[[ids[i]]]
    if (is.null(y)) return("")
    y <- sort(unique(y[y <= yrs[i]]))
    if (length(y) == 0L) "" else paste(y, collapse = ", ")
  }, character(1))
  dy$genomic_orphan <- nzchar(dy$orphan_years)
  dy
}

# S7 ES: environmental-surveillance positive years up to and including each
# year. `es_col`, when given, is a count (>0) or logical (TRUE) positive flag.
# @noRd
.fg_add_es <- function(dy, es, id_col, es_col) {
  stopifnot(
    is.data.frame(es),
    id_col %in% names(es),
    "year" %in% names(es)
  )
  g <- es
  if (!is.null(es_col)) {
    if (!es_col %in% names(g)) {
      cli::cli_abort("{.arg es_col} {.val {es_col}} not in {.arg es}.")
    }
    v <- g[[es_col]]
    keep <- if (is.logical(v)) v %in% TRUE else as.numeric(v) > 0
    g <- g[keep, , drop = FALSE]
  }
  es_map <- split(as.integer(g$year), as.character(g[[id_col]]))
  ids <- as.character(dy[[id_col]])
  yrs <- as.integer(dy$year)
  dy$es_years <- vapply(seq_along(ids), function(i) {
    y <- es_map[[ids[i]]]
    if (is.null(y)) return("")
    y <- sort(unique(y[y <= yrs[i]]))
    if (length(y) == 0L) "" else paste(y, collapse = ", ")
  }, character(1))
  dy$es_detected <- nzchar(dy$es_years)
  dy
}

# ---------------------------------------------------------------------------
# static reference content (paper tables S15 + S17)
# ---------------------------------------------------------------------------

# @noRd
.fg_reference <- function(spi_cut = 0.80) {
  tibble::tribble(
    ~signal, ~asks, ~rules_out,
    "S1 Credible discordance",
    "Is a conventionally adequate district nonetheless flagged sub-threshold, with the uncertainty in its estimate excluding true adequacy?",
    "Statistical noise: the shortfall is not an artefact of a wide credible interval that still touches 1.",
    "S2 Depth of shortfall",
    "How far below expectation does the district sit?",
    "A borderline miss: a posterior median far below the cut is a deep, not marginal, shortfall.",
    "S3 Observed versus expected",
    "Is a substantial expected detection burden met by few observed cases?",
    "A small-denominator artefact: the gap is against a real expected count, not a low-expectation district.",
    "S4 Trajectory",
    "Is performance deteriorating, stable, or recovering over the last five years?",
    "A one-off dip: a sustained downward slope is a trend, not a single anomalous year.",
    "S5 Persistence",
    "How many consecutive years has the district stayed below the adequacy threshold?",
    "Transient variation: a long unbroken run is chronic under-detection, not year-to-year fluctuation.",
    "S6 Neighbour contrast",
    "Does the district under-detect while its immediate neighbours perform adequately?",
    "A region-wide data problem: a localised gap against healthy neighbours points to a district-specific failure.",
    "S7 Seasonal and detection corroboration",
    "Is observed detection flat through the expected peak season, and has any poliovirus been detected there by AFP cases or environmental surveillance?",
    "Nothing on its own: corroboration strengthens a flag, but its absence never proves adequacy."
  )
}

# @noRd
.fg_misreadings <- function(spi_cut = 0.80) {
  tibble::tribble(
    ~misreading, ~correction,
    "The flag means the virus is there.",
    "It means detection is short of expectation: read it as a system signal, not a virus signal. A gap is worth closing precisely because that is where circulation would next be missed.",
    "Every flagged district needs investigating.",
    "Roughly a third of district-years flag, too many to chase; triage by depth, confidence, and persistence. The continuous score exists so capacity goes where the shortfall is largest and most certain.",
    "An SPI of 0.7 here equals 0.7 anywhere.",
    "The SPI is calibrated within each country's own data. Compare the pattern, not the raw value: which districts sit persistently below their own national expectation.",
    "This month dropped, so act now.",
    "Monthly counts in small districts are mostly structural zeros; classify annually and read months as trend. Reacting to monthly noise spends trust on movement that is not real.",
    sprintf("It is below %.2f, so it is a blind spot.", spi_cut),
    "Not if the credible interval includes 1: the interval is half the signal. A marginal point estimate with wide uncertainty is a watch, not a flag.",
    "A flagged district had no virus, so the SPI was wrong.",
    "The flag is about capacity, not presence: a quiet district can still be under-detecting. The verdict is whether the system could see, not whether there was anything to see."
  )
}

# ---------------------------------------------------------------------------
# S3 methods
# ---------------------------------------------------------------------------

#' @export
print.blindspot_field_guide <- function(x, ...) {
  cli::cli_h1("SPI field guide")
  cli::cli_inform(c(
    "Read year: {.val {x$read_year}} \\
     | SPI cut: {.val {x$thresholds$spi}} \\
     | flag rule: median < cut, 90% CrI upper < 1, \\
     {.val {x$params$min_corroborators}}+ corroborators"
  ))

  active <- names(x$signals_active)[x$signals_active]
  inactive <- names(x$signals_active)[!x$signals_active]
  active_str <- if (length(active)) active else "none"
  inactive_str <- if (length(inactive)) {
    paste0(" | inactive: ", paste(inactive, collapse = ", "))
  } else {
    ""
  }
  cli::cli_inform(
    "Optional signals active: {.val {active_str}}{inactive_str}"
  )

  foc <- x$focal
  cli::cli_h2("Verdicts for {x$read_year}")
  verdict_tbl <- tibble::tibble(
    verdict = c("FLAG", "WATCH", "No action"),
    n = c(sum(foc$verdict == "FLAG"), sum(foc$verdict == "WATCH"),
          sum(foc$verdict == "No action")),
    pct = round(100 * c(sum(foc$verdict == "FLAG"),
                        sum(foc$verdict == "WATCH"),
                        sum(foc$verdict == "No action")) / nrow(foc), 1)
  )
  print(verdict_tbl)

  flagged_detect <- foc[foc$verdict == "FLAG" & foc$observed > 1, ]
  n_show <- min(10L, nrow(flagged_detect))
  if (n_show > 0) {
    cli::cli_h2("Flagged districts with detections (top {n_show} by depth)")
    print(.fg_scan_tibble(x, flagged_detect), n = n_show)
  }
  cli::cli_alert_info(
    "See {.fn bs_field_guide_help} to learn the reading, \\
     {.fn bs_field_guide_table} to render it."
  )
  invisible(x)
}

#' @export
summary.blindspot_field_guide <- function(object, ...) {
  print(object, ...)
  foc <- object$focal
  cli::cli_h2("Signal fire counts for {object$read_year}")
  fires <- tibble::tibble(
    signal = c("S1 discordance", "S4 falling", "S5 persistent",
               "S6 neighbour", "S7 seasonal-blind", "S7 genomic-orphan"),
    n = c(
      sum(object$focal$s1_discordance, na.rm = TRUE),
      sum(foc$trajectory == "falling", na.rm = TRUE),
      sum(foc$longest_run_below >= object$params$persistence, na.rm = TRUE),
      sum(foc$neighbour_discordant, na.rm = TRUE),
      sum(foc$seasonally_blind, na.rm = TRUE),
      sum(foc$genomic_orphan, na.rm = TRUE)
    )
  )
  print(fires)
  cli::cli_h2("Seven-signal reference")
  print(object$reference)
  invisible(object)
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.blindspot_field_guide <- function(x, ...) {
  x$district_year
}

# compact per-district scan tibble for a given focal slice, ranked by depth.
# @noRd
.fg_scan_tibble <- function(x, foc) {
  id_col <- x$id_col
  label_col <- intersect(c("adm2_name", "adm1_name", id_col), names(foc))[1]
  out <- tibble::tibble(
    district = foc[[label_col]],
    obs = foc$observed,
    exp = round(foc$expected_total, 1),
    spi = round(foc$spi_median, 2),
    cri = sprintf("%.2f-%.2f", foc$spi_q05, foc$spi_q95),
    npafp = round(foc$npafp_rate, 1),
    run = foc$longest_run_below,
    traj = foc$trajectory,
    corrob = foc$corroborators,
    verdict = as.character(foc$verdict)
  )
  out[order(out$verdict != "FLAG", out$spi), ]
}


# ============================================================================
# Field guide help  (learn to read the guide)

#' Learn to read the SPI field guide
#'
#' @description
#' An interpretation aid for the console. Explains the seven signals and the
#' flag rule behind [bs_field_guide()], lists the common misreadings that
#' end a programme's trust in the score, and -- most usefully -- walks
#' through a **live worked example**: four archetype districts (reassuring,
#' watch, flag, corroborated flag) selected by rule from a real field guide
#' and narrated from their own signal values.
#'
#' @details
#' By default the worked example is narrated from `synth_field_guide`, the
#' precomputed field guide on the synthetic bundle, so it runs instantly with
#' no model fit. Pass `guide` to narrate your own [bs_field_guide()] result.
#'
#' @param topic One or more of `"all"`, `"signals"`, `"verdict"`,
#'   `"misreadings"`, `"example"`. Default: `"all"`.
#' @param guide A [bs_field_guide()] result to narrate for the worked example.
#'   Default: NULL (use `synth_field_guide`).
#' @param year Focal year for the worked example. Default: NULL
#'   (`guide$read_year`).
#'
#' @return The narrated worked-example tibble, invisibly.
#'
#' @seealso [bs_field_guide()], [bs_field_guide_table()].
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' bs_field_guide_help("signals")
#' bs_field_guide_help("verdict")
#' bs_field_guide_help("example")
bs_field_guide_help <- function(
  topic = c("all", "signals", "verdict", "misreadings", "example"),
  guide = NULL,
  year = NULL
) {
  .check_pkg("cli", reason = "to print the field-guide help")
  topic <- match.arg(topic, several.ok = TRUE)
  if ("all" %in% topic) {
    topic <- c("signals", "verdict", "misreadings", "example")
  }

  spi_cut <- if (!is.null(guide)) guide$thresholds$spi else 0.80

  cli::cli_h1("How to read a low SPI")

  if ("signals" %in% topic) {
    cli::cli_h2("The seven signals")
    cli::cli_text(
      "A low SPI marks a district for a closer look. Read it through seven \\
       signals, each ruling out a different innocent explanation; the \\
       reading rests on their convergence."
    )
    ref <- .fg_reference(spi_cut)
    for (i in seq_len(nrow(ref))) {
      cli::cli_h3(ref$signal[i])
      cli::cli_ul()
      cli::cli_li("{.strong Asks:} {ref$asks[i]}")
      cli::cli_li("{.strong Rules out:} {ref$rules_out[i]}")
      cli::cli_end()
    }
  }

  if ("verdict" %in% topic) {
    cli::cli_h2("The flag rule")
    cli::cli_text(
      "Each district-year gets one of three verdicts. The rule is \\
       deliberately conservative: uncertainty is half the signal."
    )
    cli::cli_ul()
    cli::cli_li(
      "{.strong FLAG} -- posterior median SPI below \\
       {sprintf('%.2f', spi_cut)}, the 90% credible interval upper bound \\
       below 1, and at least two of four corroborators fire (falling \\
       trajectory, persistence, neighbour discordance, seasonal blindness). \\
       Warrants supervisory review and active case search."
    )
    cli::cli_li(
      "{.strong WATCH} -- below the cut, but the credible interval still \\
       reaches 1. Kept under watch, not flagged; collect another year of data."
    )
    cli::cli_li(
      "{.strong No action} -- neither condition holds. No follow-up needed."
    )
    cli::cli_end()
    cli::cli_text(
      "Genomic corroboration strengthens a flag from outside the model, but \\
       is never required to make one."
    )
  }

  if ("misreadings" %in% topic) {
    cli::cli_h2("Common misreadings")
    mis <- .fg_misreadings(spi_cut)
    for (i in seq_len(nrow(mis))) {
      cli::cli_ul()
      cli::cli_li("{.emph {mis$misreading[i]}}")
      cli::cli_li("{.strong Actually:} {mis$correction[i]}")
      cli::cli_end()
    }
  }

  worked <- NULL
  if ("example" %in% topic) {
    guide <- guide %||% .fg_load_synth()
    stopifnot(inherits(guide, "blindspot_field_guide"))
    year <- as.integer(year %||% guide$read_year)
    foc <- guide$district_year[guide$district_year$year == year, ,
                               drop = FALSE]
    id_col <- guide$id_col
    name_col <- intersect(c("adm2_name", "adm1_name"), names(foc))[1] %||%
      id_col
    sel <- .fg_select_worked(foc, id_col, guide$thresholds$spi)

    cli::cli_h2("Worked example ({year})")
    cli::cli_text(
      "Four districts, chosen by rule, running from clearly adequate to a \\
       corroborated flag:"
    )
    for (i in seq_len(nrow(sel))) {
      r <- as.list(sel[i, ])
      narrative <- .fg_narrate(r, guide$thresholds$spi)
      action <- .fg_action(r)
      cli::cli_h3("{sel[[name_col]][i]} -- {sel$case_label[i]}")
      cli::cli_text(narrative)
      cli::cli_alert_info("Action: {action}")
    }
    worked <- sel
  }

  invisible(worked)
}

# @noRd
.fg_load_synth <- function() {
  e <- new.env()
  ok <- tryCatch({
    utils::data("synth_field_guide", package = "blindspot", envir = e)
    TRUE
  }, error = function(err) FALSE)
  if (!ok || !exists("synth_field_guide", envir = e)) {
    cli::cli_abort(
      "{.val synth_field_guide} is not available; pass a \\
       {.fn bs_field_guide} result via {.arg guide}."
    )
  }
  e$synth_field_guide
}

# one-sentence, data-driven reading of a district-year row.
# @noRd
.fg_narrate <- function(r, spi_cut) {
  label <- r$case_label %||% as.character(r$verdict)
  spi <- sprintf("%.2f", r$spi_median)
  q05 <- sprintf("%.2f", r$spi_q05)
  q95 <- sprintf("%.2f", r$spi_q95)
  run <- as.integer(r$longest_run_below)
  exp <- sprintf("%.1f", r$expected_total)

  switch(
    label,
    "Reassuring" = sprintf(
      paste0("Adequate: SPI %s with its whole 90%% credible interval (%s to ",
             "%s) above the cut, detecting about as many cases as expected."),
      spi, q05, q95
    ),
    "Watch" = sprintf(
      paste0("Falls just under the cut at SPI %s, but its credible interval ",
             "still reaches 1 (%s to %s) on a small expected count, so it is ",
             "kept under watch rather than flagged -- avoiding a threshold ",
             "artefact."),
      spi, q05, q95
    ),
    "Flag" = sprintf(
      paste0("The district the conventional rate misses: it clears the rate ",
             "target, yet SPI places it deep at %s, with %d consecutive year%s ",
             "below the cut against %s expected cases."),
      spi, run, if (run == 1L) "" else "s", exp
    ),
    "Flag, corroborated" = sprintf(
      paste0("Carries a flag on %d signals at once, with an orphan poliovirus ",
             "detected there (%s) confirming it from outside the model."),
      as.integer(r$corroborators), r$orphan_years
    ),
    "Flag, persistent" = sprintf(
      paste0("Carries a flag on %d signals at once, resting on the longest ",
             "run of sub-threshold years in the panel (%d)."),
      as.integer(r$corroborators), run
    ),
    # fallback
    sprintf("SPI %s; verdict %s.", spi, as.character(r$verdict))
  )
}

# @noRd
.fg_action <- function(r) {
  v <- as.character(r$verdict)
  switch(
    v,
    FLAG = "supervisory review and active case search.",
    WATCH = "collect another year of data before acting.",
    "no follow-up needed."
  )
}


# ============================================================================
# Field guide table (gt / flextable rendering)

#' Render the SPI field guide as a gt or flextable table
#'
#' @description
#' Turns a [bs_field_guide()] result into a publication-ready table, in
#' either of two layouts, using `gt` or `flextable`. Optionally saves to
#' file; the format is inferred from the extension (html / docx / pdf / rtf /
#' png / pptx).
#'
#' @details
#' Two layouts:
#' \itemize{
#'   \item `"scan"` -- one row per district for the focal year, with the key
#'     signals and the verdict; the verdict cell is shaded by concern.
#'   \item `"worked"` -- the paper's teaching layout: the seven signals plus
#'     the verdict as rows, a few districts as columns, every cell shaded
#'     adverse (rose) / intermediate (amber) / reassuring (green). When
#'     `districts` is NULL, four archetype districts (reassuring, watch, flag,
#'     corroborated flag) are chosen by rule.
#' }
#'
#' @param x A [bs_field_guide()] result (class `blindspot_field_guide`).
#' @param engine Table engine: `"gt"` or `"flextable"`.
#' @param layout `"scan"` (districts as rows) or `"worked"` (signals as rows).
#' @param year Focal year. Default: NULL (`x$read_year`).
#' @param districts Character vector of district ids for the `"worked"`
#'   layout. Default: NULL (rule-select four archetypes).
#' @param name_col Column to label districts by. Default: NULL (first of
#'   `adm2_name`, `adm1_name`, else the id column).
#' @param file Optional output path; the extension sets the format. Default:
#'   NULL (return the table object without saving).
#' @param shade Logical. Apply concern shading. Default: TRUE.
#' @param max_rows Integer. For the `"scan"` layout, cap the number of rows
#'   (ranked flags first, then by depth). Default: 50.
#' @param ... Passed to the underlying `gt::gtsave()` / flextable saver.
#'
#' @return The `gt_tbl` or `flextable` object, invisibly when `file` is set.
#'
#' @seealso [bs_field_guide()], [bs_field_guide_help()].
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' fg <- synth_field_guide
#' \dontrun{
#' bs_field_guide_table(fg, engine = "gt", layout = "worked")
#' bs_field_guide_table(fg, engine = "gt", layout = "scan",
#'                      file = tempfile(fileext = ".html"))
#' bs_field_guide_table(fg, engine = "flextable", layout = "worked",
#'                      file = tempfile(fileext = ".docx"))
#' }
bs_field_guide_table <- function(
  x,
  engine = c("gt", "flextable"),
  layout = c("scan", "worked"),
  year = NULL,
  districts = NULL,
  name_col = NULL,
  file = NULL,
  shade = TRUE,
  max_rows = 50L,
  ...
) {
  stopifnot(inherits(x, "blindspot_field_guide"))
  engine <- match.arg(engine)
  layout <- match.arg(layout)

  .check_pkg(
    if (engine == "gt") "gt" else c("flextable", "officer"),
    reason = sprintf("to render the field guide with %s", engine)
  )

  year <- as.integer(year %||% x$read_year)
  foc <- x$district_year[x$district_year$year == year, , drop = FALSE]
  if (nrow(foc) == 0L) {
    cli::cli_abort("no district-year rows for {.arg year} = {.val {year}}.")
  }

  id_col <- x$id_col
  name_col <- name_col %||%
    intersect(c("adm2_name", "adm1_name"), names(foc))[1] %||% id_col
  spi_cut <- x$thresholds$spi

  built <- if (layout == "scan") {
    .fg_build_scan(foc, engine, id_col, name_col, spi_cut, year, shade,
                   max_rows)
  } else {
    .fg_build_worked(x, foc, engine, id_col, name_col, spi_cut, year, shade,
                     districts)
  }

  if (is.null(file)) {
    return(built)
  }
  .fg_save(built, file, engine, ...)
  invisible(built)
}

# ---------------------------------------------------------------------------
# shading palette (matches the paper's Figure/Table S15)
# ---------------------------------------------------------------------------

# @noRd
FG_CLASS_FILL <- c(
  warm = "#f4cccc", # adverse
  amber = "#fce5cd", # intermediate
  cool = "#d9ead3", # reassuring
  none = "#ffffff" # neutral / not assessable
)

# @noRd
.fg_verdict_class <- function(verdict) {
  vapply(as.character(verdict), function(v) {
    switch(v, FLAG = "warm", WATCH = "amber", "cool")
  }, character(1))
}

# ---------------------------------------------------------------------------
# scan layout
# ---------------------------------------------------------------------------

# @noRd
.fg_build_scan <- function(foc, engine, id_col, name_col, spi_cut, year,
                           shade, max_rows) {
  foc <- foc[order(foc$verdict != "FLAG", foc$spi_median), , drop = FALSE]
  if (nrow(foc) > max_rows) foc <- foc[seq_len(max_rows), , drop = FALSE]

  df <- tibble::tibble(
    District = foc[[name_col]],
    SPI = round(foc$spi_median, 2),
    `90% CrI` = sprintf("%.2f-%.2f", foc$spi_q05, foc$spi_q95),
    NPAFP = round(foc$npafp_rate, 1),
    Trajectory = tools::toTitleCase(foc$trajectory),
    `Run (yr)` = as.integer(foc$longest_run_below),
    Seasonal = .fg_seasonal_word(foc$seasonal),
    Corrob = as.integer(foc$corroborators),
    Verdict = as.character(foc$verdict)
  )
  vclass <- .fg_verdict_class(foc$verdict)
  title <- sprintf("SPI field guide: district scan, %d", year)
  subtitle <- sprintf(
    "Verdict flags median SPI < %.2f with 90%% CrI upper < 1 and corroboration",
    spi_cut
  )

  if (engine == "gt") {
    g <- gt::gt(df)
    g <- gt::tab_header(g, title = title, subtitle = subtitle)
    if (shade) {
      for (cl in unique(vclass)) {
        g <- gt::tab_style(
          g,
          style = gt::cell_fill(color = FG_CLASS_FILL[[cl]]),
          locations = gt::cells_body(columns = "Verdict", rows = vclass == cl)
        )
      }
    }
    g
  } else {
    ft <- flextable::flextable(df)
    ft <- flextable::set_caption(ft, title)
    ft <- flextable::bold(ft, part = "header")
    if (shade) {
      for (i in seq_len(nrow(df))) {
        ft <- flextable::bg(
          ft, i = i, j = "Verdict", bg = FG_CLASS_FILL[[vclass[i]]]
        )
      }
    }
    flextable::autofit(ft)
  }
}

# ---------------------------------------------------------------------------
# worked layout (signals as rows, districts as columns)
# ---------------------------------------------------------------------------

# @noRd
.fg_build_worked <- function(x, foc, engine, id_col, name_col, spi_cut, year,
                             shade, districts) {
  sel <- .fg_select_worked(foc, id_col, spi_cut, districts)
  if (nrow(sel) == 0L) {
    cli::cli_abort("no districts available for the worked example.")
  }

  row_key <- c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "verdict")
  row_lab <- c(
    s1 = "S1 Credible discordance",
    s2 = "S2 Depth of shortfall",
    s3 = "S3 Observed vs expected",
    s4 = "S4 Trajectory",
    s5 = sprintf("S5 Persistence (yrs below %.2f)", spi_cut),
    s6 = "S6 Neighbour contrast",
    s7 = "S7 Seasonal / genomic",
    verdict = "Verdict"
  )

  cells <- lapply(seq_len(nrow(sel)), function(i) {
    .fg_cell_signals(as.list(sel[i, ]), spi_cut)
  })
  classes <- lapply(seq_len(nrow(sel)), function(i) {
    .fg_cell_classes(as.list(sel[i, ]))
  })

  col_ids <- paste0("d", seq_len(nrow(sel)))
  col_head <- sprintf("%s\n(%s)", sel[[name_col]], sel$case_label)

  df <- tibble::tibble(Signal = unname(row_lab[row_key]))
  for (j in seq_along(col_ids)) {
    df[[col_ids[j]]] <- unname(cells[[j]][row_key])
  }
  # class matrix aligned to df rows/cols
  cmat <- vapply(seq_along(col_ids), function(j) {
    unname(classes[[j]][row_key])
  }, character(length(row_key)))

  title <- sprintf(
    "Reading the SPI: the seven signals for %d districts, %d",
    nrow(sel), year
  )
  footer <- sprintf(
    paste0("Districts selected by rule from the field guide, ordered ",
           "reassuring to corroborated flag. Shading marks each signal as ",
           "adverse (rose), intermediate (amber), or reassuring (green). ",
           "Adequacy cut SPI %.2f; genomic corroboration strengthens a flag ",
           "but is never required."),
    spi_cut
  )

  if (engine == "gt") {
    names(df)[-1] <- col_head
    g <- gt::gt(df, rowname_col = "Signal")
    g <- gt::tab_header(g, title = title)
    g <- gt::tab_source_note(g, gt::md(paste0("*", footer, "*")))
    if (shade) {
      for (j in seq_along(col_ids)) {
        for (i in seq_len(nrow(df))) {
          cl <- cmat[i, j]
          if (cl != "none") {
            g <- gt::tab_style(
              g,
              style = gt::cell_fill(color = FG_CLASS_FILL[[cl]]),
              locations = gt::cells_body(columns = col_head[j], rows = i)
            )
          }
        }
      }
    }
    g
  } else {
    names(df)[-1] <- col_ids
    ft <- flextable::flextable(df)
    ft <- flextable::set_header_labels(
      ft,
      values = stats::setNames(as.list(c("", col_head)), c("Signal", col_ids))
    )
    ft <- flextable::set_caption(ft, title)
    ft <- flextable::bold(ft, j = 1, part = "body")
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::valign(ft, valign = "top", part = "body")
    ft <- flextable::add_footer_lines(ft, values = footer)
    if (shade) {
      for (j in seq_along(col_ids)) {
        for (i in seq_len(nrow(df))) {
          cl <- cmat[i, j]
          if (cl != "none") {
            ft <- flextable::bg(
              ft, i = i, j = col_ids[j], bg = FG_CLASS_FILL[[cl]]
            )
          }
        }
      }
    }
    flextable::autofit(ft)
  }
}

# ---------------------------------------------------------------------------
# worked-example district selection (ported from the paper, made robust)
# ---------------------------------------------------------------------------

# @noRd
.fg_select_worked <- function(foc, id_col, spi_cut, districts = NULL) {
  if (!is.null(districts)) {
    sel <- foc[match(districts, foc[[id_col]]), , drop = FALSE]
    sel <- sel[!is.na(sel[[id_col]]), , drop = FALSE]
    sel$case_label <- as.character(sel$verdict)
    return(sel)
  }

  pick <- function(df, ...) {
    df <- dplyr::arrange(df, ...)
    if (nrow(df) == 0L) NULL else df[1L, , drop = FALSE]
  }
  used <- character(0)
  remaining <- function() foc[!foc[[id_col]] %in% used, , drop = FALSE]

  # reassuring: adequate, whole CrI above cut, no sub-cut years, largest
  good <- pick(
    dplyr::filter(
      remaining(),
      .data$conventional_pass, .data$verdict == "No action",
      .data$spi_q05 >= spi_cut, .data$longest_run_below == 0L
    ),
    dplyr::desc(.data$expected_total)
  )
  good <- good %||% pick(
    dplyr::filter(remaining(), .data$verdict == "No action"),
    dplyr::desc(.data$spi_median)
  )
  if (!is.null(good)) used <- c(used, good[[id_col]])

  # watch: below cut but CrI still reaches 1, smallest expected (artefact)
  watch <- pick(
    dplyr::filter(
      remaining(),
      .data$spi_below, .data$spi_q95 >= 1
    ),
    .data$expected_total
  )
  if (!is.null(watch)) used <- c(used, watch[[id_col]])

  # flag: conventionally adequate blind spot, deepest
  flag <- pick(
    dplyr::filter(
      remaining(),
      .data$verdict == "FLAG", .data$conventional_pass
    ),
    .data$spi_median
  )
  flag <- flag %||% pick(
    dplyr::filter(remaining(), .data$verdict == "FLAG"),
    .data$spi_median
  )
  if (!is.null(flag)) used <- c(used, flag[[id_col]])

  # corroborated flag: genomic orphan, else deepest-persistence flag
  corrob <- pick(
    dplyr::filter(
      remaining(),
      .data$verdict == "FLAG", .data$genomic_orphan %in% TRUE
    ),
    .data$spi_median
  )
  corrob_label <- "Flag, corroborated"
  if (is.null(corrob)) {
    corrob <- pick(
      dplyr::filter(remaining(), .data$verdict == "FLAG"),
      dplyr::desc(.data$longest_run_below), .data$spi_median
    )
    corrob_label <- "Flag, persistent"
  }

  parts <- list(
    .fg_tag(good, "Reassuring"),
    .fg_tag(watch, "Watch"),
    .fg_tag(flag, "Flag"),
    .fg_tag(corrob, corrob_label)
  )
  dplyr::bind_rows(parts[!vapply(parts, is.null, logical(1))])
}

# @noRd
.fg_tag <- function(row, label) {
  if (is.null(row)) return(NULL)
  row$case_label <- label
  row
}

# ---------------------------------------------------------------------------
# cell text + shading class factories (plain-number formatting)
# ---------------------------------------------------------------------------

# @noRd
.fg_cell_signals <- function(r, spi_cut) {
  c(
    s1 = if (isTRUE(r$s1_discordance)) {
      sprintf("Fires: rate %.1f adequate, SPI %.2f, 90%% CrI upper %.2f",
              r$npafp_rate, r$spi_median, r$spi_q95)
    } else if (isTRUE(r$spi_below)) {
      sprintf("Does not fire: 90%% CrI (%.2f to %.2f) includes 1",
              r$spi_q05, r$spi_q95)
    } else {
      sprintf("Does not fire: at or above the cut (SPI %.2f)", r$spi_median)
    },
    s2 = sprintf("%.2f", r$spi_median),
    s3 = sprintf("%d observed vs %.1f expected",
                 as.integer(round(r$observed)), r$expected_total),
    s4 = tools::toTitleCase(r$trajectory),
    s5 = sprintf("%d consecutive yr%s", as.integer(r$longest_run_below),
                 if (as.integer(r$longest_run_below) == 1L) "" else "s"),
    s6 = .fg_s6_cell(r),
    s7 = sprintf("%s; %s", .fg_seasonal_word(r$seasonal), .fg_genomic_cell(r)),
    verdict = .fg_verdict_reason(r, spi_cut)
  )
}

# @noRd
.fg_cell_classes <- function(r) {
  c(
    s1 = if (isTRUE(r$s1_discordance)) "warm" else "cool",
    s2 = if (isTRUE(r$spi_below)) "warm" else "cool",
    s3 = if (isTRUE(r$spi_below) && r$observed < r$expected_total) "warm"
         else "cool",
    s4 = switch(r$trajectory, falling = "warm", flat = "amber",
                rising = "cool", "amber"),
    s5 = if (r$longest_run_below >= 3L) "warm"
         else if (r$longest_run_below >= 1L) "amber" else "cool",
    s6 = if (isTRUE(r$island) || is.na(r$neighbour_spi)) "none"
         else if (isTRUE(r$neighbour_discordant)) "warm"
         else if (!isTRUE(r$spi_below) && r$neighbour_spi >= 0.80) "cool"
         else "amber",
    s7 = if (isTRUE(r$seasonally_blind) || isTRUE(r$genomic_orphan)) "warm"
         else if (!is.na(r$seasonal) && r$seasonal == "muted") "amber"
         else "cool",
    verdict = switch(as.character(r$verdict), FLAG = "warm", WATCH = "amber",
                     "cool")
  )
}

# @noRd
.fg_s6_cell <- function(r) {
  if (isTRUE(r$island)) return("no adjacent district")
  if (is.na(r$neighbour_spi)) return("neighbours not assessable")
  sprintf("SPI %.2f vs neighbour median %.2f", r$spi_median, r$neighbour_spi)
}

# @noRd
.fg_seasonal_word <- function(x) {
  map <- c(blind = "Blind through peak", muted = "Muted",
           present = "Detects in peak", `not assessed` = "Not assessable")
  out <- unname(map[x])
  out[is.na(x) | is.na(out)] <- "Not assessed"
  out
}

# @noRd
.fg_genomic_cell <- function(r) {
  if (is.na(r$genomic_orphan)) return("genomic not assessed")
  if (isTRUE(r$genomic_orphan) && nzchar(r$orphan_years %||% "")) {
    return(paste0("orphan cVDPV ", r$orphan_years))
  }
  "orphan none"
}

# @noRd
.fg_verdict_reason <- function(r, spi_cut) {
  switch(
    as.character(r$verdict),
    FLAG = sprintf(
      "Flag: SPI < %.2f, 90%% CrI excludes 1, %d corroborating signals",
      spi_cut, as.integer(r$corroborators)
    ),
    WATCH = sprintf("Watch: SPI < %.2f but 90%% CrI includes 1", spi_cut),
    "No action"
  )
}

# ---------------------------------------------------------------------------
# save dispatch
# ---------------------------------------------------------------------------

# @noRd
.fg_save <- function(tbl, file, engine, ...) {
  ext <- tolower(tools::file_ext(file))
  if (engine == "gt") {
    if (ext %in% c("png", "pdf")) {
      .check_pkg("webshot2", reason = sprintf("to save a gt table as %s", ext))
    }
    if (ext == "docx") {
      .check_pkg("rmarkdown", reason = "to save a gt table as docx")
    }
    gt::gtsave(tbl, filename = file, ...)
  } else {
    switch(
      ext,
      docx = flextable::save_as_docx(tbl, path = file, ...),
      html = flextable::save_as_html(tbl, path = file, ...),
      pptx = flextable::save_as_pptx(tbl, path = file, ...),
      png = {
        .check_pkg("webshot2", reason = "to save a flextable as png")
        flextable::save_as_image(tbl, path = file, ...)
      },
      cli::cli_abort(
        "flextable cannot save {.val {ext}}; use docx / html / pptx / png, \\
         or engine = {.val gt}."
      )
    )
  }
  invisible(file)
}
