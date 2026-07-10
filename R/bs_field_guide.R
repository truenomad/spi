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
#'   \item **S4 Trajectory** -- the sign of the SPI-vs-year slope over the
#'     last `traj_window` years (falling / flat / rising).
#'   \item **S5 Persistence** -- the longest run of consecutive years below
#'     the cut up to that year.
#'   \item **S6 Neighbour contrast** -- under-detection while immediate
#'     neighbours perform adequately (needs `adjacency`).
#'   \item **S7 Seasonal and genomic corroboration** -- detection flat
#'     through the expected peak months (needs `spi_month`), and any orphan
#'     poliovirus detected there (needs `genomic`).
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
#' Genomic corroboration strengthens a flag in the narrative but is never one
#' of the four corroborators, matching the paper.
#'
#' Signals that need optional inputs degrade gracefully: without `adjacency`,
#' `spi_month`, or `genomic` the corresponding signal is `NA` and contributes
#' no corroborator. `signals_active` records which were computable.
#'
#' @param concordance A [bs_concordance()] result (class
#'   `blindspot_concordance`). Supplies fused SPI + NPAFP for every
#'   district-year plus the thresholds.
#' @param adjacency Optional spatial neighbour object from [bs_adjacency()]
#'   (class `blindspot_nb`). Enables S6. Default: NULL.
#' @param spi_month Optional [bs_spi()] result at `district_month` level.
#'   Enables the seasonal half of S7. Default: NULL.
#' @param genomic Optional tibble of orphan poliovirus detections with the
#'   district id column and `year`. Enables the genomic half of S7. Rows are
#'   detections; pass `genomic_col` to filter on a 0/1 flag column. Default:
#'   NULL.
#' @param genomic_col Optional name of a logical / 0-1 column in `genomic`;
#'   only truthy rows count as detections. Default: NULL (every row counts).
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
#' @param min_corroborators Integer. Corroborating signals required to flag.
#'   Default: 2.
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
#'     `neighbour_discordant`, `seasonal`, `orphan_years`, `genomic_orphan`),
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
#'   genomic   = dplyr::filter(synth_surveillance$virus_outcome, any_cvdpv2 == 1)
#' )
#' }
bs_field_guide <- function(
  concordance,
  adjacency = NULL,
  spi_month = NULL,
  genomic = NULL,
  genomic_col = NULL,
  read_year = NULL,
  spi_cut = NULL,
  persistence = 3L,
  traj_window = 5L,
  traj_tol = 0.01,
  min_corroborators = 2L,
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
  dy <- .fg_add_trajectory(dy, id_col, traj_window, traj_tol)

  # --- S6 neighbour contrast ------------------------------------------
  have_adjacency <- !is.null(adjacency)
  if (have_adjacency) {
    dy <- .fg_add_neighbour(dy, adjacency, id_col, spi_cut)
  } else {
    dy$neighbour_spi <- NA_real_
    dy$island <- NA
    dy$neighbour_discordant <- NA
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

  # --- S7 genomic ------------------------------------------------------
  have_genomic <- !is.null(genomic)
  if (have_genomic) {
    dy <- .fg_add_genomic(dy, genomic, id_col, genomic_col)
  } else {
    dy$orphan_years <- NA_character_
    dy$genomic_orphan <- NA
  }

  # --- corroborators + verdict ----------------------------------------
  is_true <- function(x) !is.na(x) & x
  dy <- dy |>
    dplyr::mutate(
      corroborators = (.data$trajectory == "falling") +
        (.data$longest_run_below >= persistence) +
        is_true(.data$neighbour_discordant) +
        is_true(.data$seasonally_blind),
      verdict = dplyr::case_when(
        .data$spi_below & .data$cri_excludes_1 &
          .data$corroborators >= min_corroborators ~ "FLAG",
        .data$spi_below & !.data$cri_excludes_1 ~ "WATCH",
        TRUE ~ "No action"
      ),
      verdict = factor(.data$verdict, levels = c("FLAG", "WATCH", "No action"))
    )

  signals_active <- c(
    s6_neighbour = have_adjacency,
    s7_seasonal = have_seasonal,
    s7_genomic = have_genomic
  )

  if (verbose) {
    n_assessable <- 2L + sum(have_adjacency, have_seasonal)
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
        min_corroborators = as.integer(min_corroborators)
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

# S4: sign of the SPI-vs-year slope over the trailing `window` years.
# @noRd
.fg_add_trajectory <- function(dy, id_col, window, tol) {
  slope_at <- function(year, val, focal) {
    w <- year >= (focal - window + 1L) & year <= focal
    yr <- year[w]
    vv <- val[w]
    ok <- is.finite(yr) & is.finite(vv)
    if (sum(ok) < 2L) return(NA_real_)
    unname(stats::coef(stats::lm(vv[ok] ~ yr[ok]))[2L])
  }
  dy |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) |>
    dplyr::mutate(
      traj_slope = vapply(
        .data$year,
        function(y) slope_at(.data$year, .data$spi_median, y),
        numeric(1)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      trajectory = dplyr::case_when(
        is.na(.data$traj_slope) ~ "flat",
        .data$traj_slope <= -tol ~ "falling",
        .data$traj_slope >= tol ~ "rising",
        TRUE ~ "flat"
      )
    )
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
        .data$neighbour_spi >= spi_cut & .data$spi_median < spi_cut
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
      cli::cli_abort("{.arg genomic_col} {.val {genomic_col}} not in {.arg genomic}.")
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
    "S7 Seasonal and genomic corroboration",
    "Is observed detection flat through the expected peak season, and has any orphan poliovirus been detected there?",
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
  cli::cli_inform(c(
    "Optional signals active: \\
     {.val {if (length(active)) active else 'none'}}\\
     {if (length(inactive)) paste0(' | inactive: ', paste(inactive, collapse = ', ')) else ''}"
  ))

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

  n_show <- min(10L, sum(foc$verdict == "FLAG"))
  if (n_show > 0) {
    cli::cli_h2("Flagged districts (top {n_show} by depth)")
    print(.fg_scan_tibble(x, foc), n = n_show)
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
