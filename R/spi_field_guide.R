# review judgements, strongest first. Every renderer, legend and summary
# orders off this, so a change reaches all of them at once.
# @noRd
.FG_VERDICT_LEVELS <- c("Priority for review", "Monitor", "No SPI indication")

# the strength gate as the guide recorded it. A guide built before the gate
# was split from the interval carries it under `cri_excludes_1`, which is what
# that column meant at the time, so a saved object still reads correctly
# instead of reporting an open gate on a row where the gate shut.
# @noRd
.fg_gate_pass <- function(r) {
  isTRUE(r$gate_pass %||% r$cri_excludes_1)
}

#' Review relative reporting shortfalls with STEPS
#'
#' @description
#' Reviews each district-year in a [spi_compare_npafp()] result using the five
#' **STEPS** components (strength,
#' timeliness, extent, persistence, stool adequacy) and one of the field
#' guide's three review labels (`Priority for review`, `Monitor`,
#' `No SPI indication`). STEPS is applied only to districts with an SPI below
#' `spi_cut` (1 by default, as in the field guide).
#'
#' SPI compares reported NPAFP cases with expected counts. STEPS adds
#' specimen transport and stool adequacy, and checks whether a low SPI also
#' appears elsewhere in the area or in the previous year.
#' The five components and what each asks:
#' \itemize{
#'   \item **S: Strength** -- how large is the shortfall, and how uncertain is the estimate? Read
#'     from the SPI value and whether its 90% credible interval lies entirely
#'     below 1.
#'   \item **T: Timeliness** -- are specimens reaching the laboratory within 3
#'     days? Needs `process`.
#'   \item **E: Extent** -- is a low SPI more common in the same region
#'     (such as a province or state) than nationally? Needs
#'     an `extent_col` in the comparison.
#'   \item **P: Persistence** -- was the SPI also below the cut in the previous
#'     year?
#'   \item **S: Stool adequacy** -- are stool specimens adequate? Needs
#'     `process`.
#' }
#'
#' The five STEPS components are reported separately. The review label
#' depends on strength, extent, and persistence. Timeliness and stool
#' adequacy describe specimen handling and do not change the label. These
#' labels are not validated measures of surveillance adequacy or automatic
#' recommendations for field action.
#'
#' Trend, neighbouring-district context, seasonal patterns, and poliovirus
#' detections through AFP or environmental surveillance are reported separately
#' as supporting context when inputs are supplied. They do not change the
#' generated review label.
#'
#' @details
#' Each district-year receives one of three review labels, using the field
#' guide's descriptions:
#' \itemize{
#'   \item **Priority for review** -- the SPI is below `spi_cut`, its 90%
#'     credible interval lies entirely below 1 (and, when `noise_alpha` is set,
#'     sampling noise alone is unlikely to produce the shortfall), and extent
#'     or persistence also raises concern. The label identifies districts
#'     to consider for closer review.
#'   \item **Monitor** -- the SPI is below `spi_cut`, but the interval includes
#'     1, the optional sampling-noise check fails, or neither extent nor
#'     persistence supports it. Uncertainty or limited
#'     supporting information favours reassessment as new data become
#'     available.
#'   \item **No SPI indication** -- the SPI is at or above `spi_cut`. There is
#'     no SPI indication for additional review; routine surveillance and review
#'     based on other indicators continue.
#' }
#'
#' With `spi_rule = "interval"`, a district-year whose 90% credible interval
#' includes 1 is labelled `No SPI indication` rather than `Monitor`, so only
#' shortfalls whose interval lies entirely below 1 are labelled.
#'
#' Extent compares the share of the other districts in the same `extent_col`
#' area with an SPI below the cut against the national share in the same year,
#' and raises concern when the area share is higher. Timeliness and stool
#' adequacy are assessed on at least `process_min_cases` cases against a target
#' of `process_target` percent; below that they are `NA` (not assessed).
#'
#' Without `process`
#' or an `extent_col` the corresponding component is `NA`. `signals_active`
#' records which components and additional findings could be calculated.
#'
#' @param comparison A [spi_compare_npafp()] result (class
#'   `spi_compare_npafp`). Supplies SPI and NPAFP results for every
#'   district-year.
#' @param process Optional district-year tibble of AFP process counts, keyed
#'   by the district id column and `year`, with columns `n_cases` (AFP cases),
#'   `n_adequate` (cases with adequate stool specimens), `n_transport` (cases
#'   with both the second stool collection and laboratory receipt dates, in
#'   order) and `n_transport_timely` (of those, received by the laboratory
#'   within 3 days of the second stool collection). Enables timeliness (T) and
#'   stool adequacy (S). District-years missing from `process` count as zero
#'   cases. Default: NULL.
#' @param extent_col Name of the admin-1 column in `comparison$district_year`
#'   that defines the area for extent (E). Default: NULL (`"adm1_name"` when
#'   present, as it is when [spi_compare_npafp()] was given `boundaries`).
#' @param process_target Numeric target, in percent, for timeliness and stool
#'   adequacy. Default: 80.
#' @param process_min_cases Integer. Fewest cases for timeliness or stool
#'   adequacy to be assessed. Default: 5.
#' @param adjacency Optional spatial neighbour object from [spi_adjacency()]
#'   (class `spi_nb`). Enables the neighbour contrast, reported as
#'   context. Default: NULL.
#' @param spi_month Optional [spi_index()] result at `district_month` level.
#'   Enables assessment of seasonal reporting patterns, reported as context. Default: NULL.
#' @param detections Optional tibble of case-based (AFP) poliovirus detections
#'   with the district id column and `year`. Reported as context. Rows are
#'   detections; pass `detection_col` to filter on a 0/1 flag column. Default:
#'   NULL.
#' @param detection_col Optional name of a logical / 0-1 column in `detections`;
#'   only `TRUE` or 1 rows count as detections. Default: NULL (every row counts).
#' @param es Optional tibble of environmental-surveillance (ES) detections with
#'   the district id column and `year`. Reported as context. Rows are
#'   positives; pass `es_col` to filter on a count / flag column. Default:
#'   NULL.
#' @param es_col Optional name of a count / logical column in `es`; only rows
#'   with a positive count (or `TRUE`) count as detections. Default: NULL
#'   (every row counts).
#' @param serotype_col Optional name of a serotype column in `detections` and / or
#'   `es`, e.g. holding `"cVDPV2"`, `"WPV1"`. When given, the distinct serotypes
#'   seen up to each year are recorded as `orphan_serotypes` / `es_serotypes`,
#'   so the report can name the serotypes found. Without it those columns are NA, which downstream reads as "not
#'   recorded" rather than "none found". Default: NULL.
#' @param detection_serotypes Optional character vector of the serotypes that
#'   count as a detection, matched against `serotype_col`, e.g.
#'   `c("WPV1", "cVDPV1", "cVDPV2", "cVDPV3")`. Rows outside the set are
#'   dropped before the detection years, flags and serotypes are computed.
#'   Requires `serotype_col`. Default: NULL (whatever the input contains
#'   counts).
#' @param read_year Integer year shown by `print()` and by default in tables and
#'   worked examples. Default: NULL (latest year present).
#' @param spi_cut Numeric SPI cutoff below which STEPS is applied. Lower
#'   values such as 0.80, 0.60, or 0.40 focus review on larger reporting
#'   shortfalls. The same cutoff is used for persistence and extent; the
#'   credible-interval check still compares with 1. A district above a lower
#'   cutoff may still need review based on other surveillance indicators.
#'   Default: 1.
#' @param spi_rule Character, `"median"` or `"interval"`. `"median"` labels
#'   every district-year with an SPI below `spi_cut`. `"interval"` also
#'   requires the 90% upper bound (`spi_q95`) to be below 1, so a district-year
#'   whose interval includes 1 is labelled `No SPI indication`. Extent and
#'   persistence still use the median. Default: NULL (the `spi_rule` given to
#'   [spi_compare_npafp()]).
#' @param traj_window Integer. Trend regression window in years, ending at
#'   each year (context). Default: 5.
#' @param traj_tol Numeric tolerance for the annual trend slope. A slope whose
#'   absolute value is below this tolerance is labelled "flat". Default: 0.01.
#' @param traj_alpha Optional numeric significance level for the trend. When
#'   set, a trajectory is only "falling" or "rising" if its OLS slope differs
#'   from zero at this two-sided level (needing at least three points in the
#'   window); otherwise it is "flat". Default: NULL (slope sign and tolerance
#'   only).
#' @param noise_alpha Optional numeric threshold for a sampling-noise check on
#'   strength (S). The SPI credible interval is posterior uncertainty in the
#'   *expected* count with the observed count held fixed, so it says nothing
#'   about sampling variation in the count itself. At `observed = 0` the ratio
#'   is identically zero in every draw, the interval collapses to `(0, 0)`, and
#'   `cri_excludes_1` is TRUE whatever the expected count. When `noise_alpha`
#'   is set, `gate_pass` additionally requires the Poisson reference tail
#'   \eqn{P(X \le observed \mid \lambda = expected)} to be at or below it.
#'   `cri_excludes_1` keeps its own meaning either way. The tail is always
#'   reported (`noise_tail`, `noise_plausible`). Default: NULL (no check).
#'   A value such as `0.05` adds this check; it is not a validated field-action
#'   threshold.
#' @param id_col Character district id column. Default: NULL (take
#'   `comparison$id_col`).
#' @param verbose Logical. Progress and missing-input messages via cli.
#'   Default: TRUE.
#'
#' @return An object of class `spi_field_guide`. A list with:
#' \describe{
#'   \item{district_year}{Tibble, one row per district-year, carrying the
#'     inputs plus every component: strength (`spi_below`, `cri_excludes_1`,
#'     `gate_pass`, `noise_tail`, `noise_plausible`), timeliness
#'     (`n_transport`, `n_transport_timely`, `pct_transport_timely`,
#'     `transport_assessed`, `timeliness_concern`), extent (`extent_area`,
#'     `extent_others`, `extent_others_below`, `extent_pct`, `national_pct`,
#'     `extent_concern`), persistence (`spi_previous`, `persistence_concern`),
#'     stool adequacy (`n_cases`, `n_adequate`, `pct_adequate`,
#'     `adequacy_assessed`, `adequacy_concern`), the context signals
#'     (`s1_discordance`, `longest_run_below`, `trailing_run_below`,
#'     `trajectory`, `neighbour_spi`, `neighbour_discordant`,
#'     `neighbourhood_shortfall`, `seasonal`, `orphan_years`,
#'     `genomic_orphan`, `orphan_serotypes`, `es_years`, `es_detected`,
#'     `es_serotypes`), and `verdict` (factor).}
#'   \item{focal}{The `read_year` slice of `district_year`.}
#'   \item{reference}{The STEPS reference tibble (what each component asks
#'     and how to interpret it).}
#'   \item{read_year, thresholds, params, signals_active, id_col, call}{
#'     Metadata: focal year, `spi_cut` / `npafp_target` / `spi_rule`, the tuning
#'     parameters, which optional components and context signals were
#'     computed, the id column, and the matched call.}
#' }
#'
#' @seealso [spi_field_guide_table()] to render it, [spi_field_guide_help()]
#'   to learn to read it, [spi_compare_npafp()] for the input, and the one-page
#'   infographic at
#'   `system.file("field-guide", "npafp_spi_steps_infographic.html", package =
#'   "spi")`.
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
#' adj  <- spi_adjacency(synth_surveillance$boundaries, id_col = "adm2_guid")
#' cy   <- spi_index(
#'   synth_surveillance$cases, synth_surveillance$population, adj,
#'   first_assessment = 2018, id_col = "adm2_guid"
#' )
#' cm   <- spi_index(
#'   synth_surveillance$cases, synth_surveillance$population, adj,
#'   first_assessment = 2018, level = "district_month", id_col = "adm2_guid"
#' )
#' comparison <- spi_compare_npafp(
#'   cy, synth_surveillance$cases, synth_surveillance$population,
#'   boundaries = synth_surveillance$boundaries
#' )
#' fg <- spi_field_guide(
#'   comparison,
#'   process = synth_surveillance$afp_process,
#'   adjacency = adj,
#'   spi_month = cm,
#'   detections = synth_surveillance$virus_outcome |>
#'     dplyr::filter(any_virus == 1) |>
#'     dplyr::select(adm2_guid, year),
#'   es = synth_surveillance$es_district_year,
#'   es_col = "n_positive"
#' )
#' }
spi_field_guide <- function(
  comparison,
  process = NULL,
  extent_col = NULL,
  process_target = 80,
  process_min_cases = 5L,
  adjacency = NULL,
  spi_month = NULL,
  detections = NULL,
  detection_col = NULL,
  es = NULL,
  es_col = NULL,
  serotype_col = NULL,
  detection_serotypes = NULL,
  read_year = NULL,
  spi_cut = 1,
  spi_rule = NULL,
  traj_window = 5L,
  traj_tol = 0.01,
  traj_alpha = NULL,
  noise_alpha = NULL,
  id_col = NULL,
  verbose = TRUE
) {
  .check_pkg(
    c("dplyr", "tibble", "lubridate", "cli"),
    reason = "to read the SPI through the field guide"
  )

  stopifnot(inherits(comparison, "spi_compare_npafp"))

  id_col <- id_col %||% comparison$id_col %||% "district_id"
  npafp_target <- comparison$thresholds$npafp
  spi_rule <- spi_rule %||% comparison$thresholds$rule %||% "median"
  spi_rule <- match.arg(spi_rule, c("median", "interval"))

  dy <- comparison$district_year
  required <- c(id_col, "year", "observed", "expected_total", "spi_median",
                "spi_q05", "spi_q95", "npafp_rate", "npafp_adequate")
  missing_cols <- setdiff(required, names(dy))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg comparison$district_year} is missing column{?s} \\
       {.val {missing_cols}}."
    )
  }
  if (any(!is.finite(dy$spi_median))) {
    cli::cli_abort(c(
      "SPI medians must be finite to assign review labels.",
      "i" = "Check or remove district-years with missing SPI before review."
    ))
  }

  if (is.null(read_year)) read_year <- max(dy$year, na.rm = TRUE)
  read_year <- as.integer(read_year)
  if (!read_year %in% dy$year) {
    cli::cli_abort(
      "{.arg read_year} = {.val {read_year}} has no district-year rows."
    )
  }

  if (!is.null(noise_alpha)) {
    noise_alpha <- as.numeric(noise_alpha)[1]
    if (!is.finite(noise_alpha) || noise_alpha <= 0 || noise_alpha >= 1) {
      cli::cli_abort("{.arg noise_alpha} must be a probability in (0, 1).")
    }
  }

  # --- S strength -----------------------------------------------------
  # `noise_plausible` asks the question the credible interval cannot: could
  # ordinary sampling variation in the *observed* count produce a shortfall
  # this deep on its own? SPI = observed / expected_draws, so the interval is
  # posterior uncertainty in the expectation with the observed count held
  # fixed. At observed = 0 the ratio is identically 0 in every draw, the
  # interval collapses to (0, 0), and `cri_excludes_1` is TRUE whatever the
  # expected count. The Poisson reference tail closes that hole. It
  # understates the spread of the negative-binomial likelihood actually
  # fitted, so it calls fewer readings noise-plausible than a fully
  # propagated posterior predictive interval would.
  dy <- dy |>
    dplyr::mutate(
      conventional_pass = .data$npafp_adequate,
      spi_below = .data$spi_median < spi_cut,
      noise_tail = .fg_noise_tail(.data$observed, .data$expected_total),
      noise_plausible = !is.na(.data$noise_tail) &
        .data$noise_tail > (noise_alpha %||% 0.05),
      # `cri_excludes_1` says only what its name says. `gate_pass` is the
      # strength test the judgement turns on: the interval, plus the noise
      # tail when `noise_alpha` asks for it.
      cri_excludes_1 = .data$spi_q95 < 1,
      gate_pass = if (is.null(noise_alpha)) {
        .data$spi_q95 < 1
      } else {
        .data$spi_q95 < 1 & !.data$noise_plausible
      },
      # context: meets the conventional target yet reads short on the SPI
      s1_discordance = .data$conventional_pass & .data$spi_below &
        .data$gate_pass
    )

  # --- P persistence: SPI also below the cut in the previous year -----
  dy <- .fg_add_previous(dy, id_col, spi_cut)

  # --- E extent: other districts in the same admin-1 area -------------
  extent_col <- extent_col %||%
    (if ("adm1_name" %in% names(dy)) "adm1_name" else NULL)
  have_extent <- !is.null(extent_col)
  if (have_extent) {
    if (!extent_col %in% names(dy)) {
      cli::cli_abort(
        "{.arg extent_col} = {.val {extent_col}} is not a column of \\
         {.arg comparison$district_year}."
      )
    }
    dy <- .fg_add_extent(dy, extent_col, spi_cut)
  } else {
    dy$extent_area <- NA_character_
    dy$extent_others <- NA_integer_
    dy$extent_others_below <- NA_integer_
    dy$extent_pct <- NA_real_
    dy$national_pct <- NA_real_
    dy$extent_concern <- NA
  }

  # --- T timeliness and S stool adequacy ------------------------------
  have_process <- !is.null(process)
  dy <- .fg_add_process(
    dy, process, id_col, process_target, process_min_cases
  )

  # --- context: runs below the cut, and trend -------------------------
  dy <- .fg_add_persistence(dy, id_col, spi_cut)
  dy <- .fg_add_trajectory(dy, id_col, traj_window, traj_tol, traj_alpha)

  # --- context: neighbour contrast ------------------------------------
  have_adjacency <- !is.null(adjacency)
  if (have_adjacency) {
    dy <- .fg_add_neighbour(dy, adjacency, id_col, spi_cut)
  } else {
    dy$neighbour_spi <- NA_real_
    dy$island <- NA
    dy$neighbour_discordant <- NA
    dy$neighbourhood_shortfall <- NA
  }

  # --- context: seasonal detection ------------------------------------
  have_seasonal <- !is.null(spi_month)
  if (have_seasonal) {
    stopifnot(inherits(spi_month, "spi_index"))
    if (!identical(spi_month$level, "district_month")) {
      cli::cli_abort(
        "{.arg spi_month} must be a {.fn spi_index} result at \\
         {.val district_month} level; got {.val {spi_month$level}}."
      )
    }
    dy <- .fg_add_seasonal(dy, spi_month, id_col)
  } else {
    dy$seasonal <- NA_character_
  }
  dy$seasonally_blind <- !is.na(dy$seasonal) & dy$seasonal == "blind"

  # --- context: AFP and ES detections ----------------------
  have_detections <- !is.null(detections)
  if (have_detections) {
    dy <- .fg_add_detections(
      dy, detections, id_col, detection_col, serotype_col, detection_serotypes
    )
  } else {
    dy$orphan_years <- NA_character_
    dy$genomic_orphan <- NA
    dy$orphan_serotypes <- NA_character_
  }
  have_es <- !is.null(es)
  if (have_es) {
    dy <- .fg_add_es(
      dy, es, id_col, es_col, serotype_col, detection_serotypes
    )
  } else {
    dy$es_years <- NA_character_
    dy$es_detected <- NA
    dy$es_serotypes <- NA_character_
  }

  # --- judgement ------------------------------------------------------
  # extent and persistence are the two components that support a low SPI in the review rule
  # signal, so either one lifts a certain shortfall to priority for review.
  # timeliness and stool adequacy are reported beside them and never move the
  # judgement.
  is_true <- function(x) !is.na(x) & x
  corroborated <- is_true(dy$extent_concern) | is_true(dy$persistence_concern)
  # under the interval rule a shortfall whose interval includes 1 is not
  # labelled, so monitor holds only the certain but uncorroborated ones
  labelled <- dy$spi_below &
    (spi_rule == "median" | is_true(dy$cri_excludes_1))
  dy$verdict <- factor(
    dplyr::case_when(
      dy$spi_below & dy$gate_pass & corroborated ~ "Priority for review",
      labelled ~ "Monitor",
      TRUE ~ "No SPI indication"
    ),
    levels = .FG_VERDICT_LEVELS
  )

  signals_active <- c(
    timeliness = have_process,
    extent = have_extent,
    adequacy = have_process,
    surroundings = have_adjacency,
    seasonal = have_seasonal,
    detect_afp = have_detections,
    detect_es = have_es
  )

  if (verbose) {
    off <- names(signals_active)[!signals_active]
    if (length(off) > 0) {
      cli::cli_alert_info(
        "Not computed (missing inputs): {.val {off}}."
      )
    }
    foc <- dy[dy$year == read_year, ]
    cli::cli_alert_success(
      "Field guide read for {.val {read_year}}: \\
       {sum(foc$verdict == 'Priority for review')} priority for review, \\
       {sum(foc$verdict == 'Monitor')} monitor, \\
       {sum(foc$verdict == 'No SPI indication')} no SPI indication."
    )
  }

  structure(
    list(
      district_year = tibble::as_tibble(dy),
      focal = tibble::as_tibble(dy[dy$year == read_year, ]),
      reference = .fg_reference(spi_cut),
      read_year = read_year,
      thresholds = list(spi = spi_cut, npafp = npafp_target, rule = spi_rule),
      params = list(
        extent_col = extent_col,
        process_target = process_target,
        process_min_cases = as.integer(process_min_cases),
        traj_window = as.integer(traj_window),
        traj_tol = traj_tol,
        traj_alpha = traj_alpha,
        noise_alpha = noise_alpha
      ),
      signals_active = signals_active,
      id_col = id_col,
      call = match.call()
    ),
    class = "spi_field_guide"
  )
}

# ---------------------------------------------------------------------------
# STEPS component helpers
# ---------------------------------------------------------------------------

# P (persistence): the district's SPI in the previous year, and whether it was
# below the cut. NA in a district's first year.
# @noRd
.fg_add_previous <- function(dy, id_col, spi_cut) {
  prev <- dy[, c(id_col, "year", "spi_median")]
  names(prev)[3] <- "spi_previous"
  prev$year <- prev$year + 1L
  dy <- dplyr::left_join(dy, prev, by = c(id_col, "year"))
  dy$persistence_concern <- dy$spi_previous < spi_cut
  dy
}

# E (extent): for each district-year, the other districts in the same area,
# how many of them were below the cut, their share, and the national share
# below the cut in that year. Concern when the area share is higher.
# @noRd
.fg_add_extent <- function(dy, extent_col, spi_cut) {
  below <- !is.na(dy$spi_median) & dy$spi_median < spi_cut
  dy |>
    dplyr::mutate(.below = below) |>
    dplyr::mutate(
      national_pct = 100 * mean(.data$.below),
      .by = "year"
    ) |>
    dplyr::mutate(
      extent_others = dplyr::n() - 1L,
      extent_others_below = sum(.data$.below) - .data$.below,
      .by = dplyr::all_of(c("year", extent_col))
    ) |>
    dplyr::mutate(
      extent_area = as.character(.data[[extent_col]]),
      extent_others_below = as.integer(.data$extent_others_below),
      extent_pct = dplyr::if_else(
        .data$extent_others > 0L,
        100 * .data$extent_others_below / .data$extent_others,
        NA_real_
      ),
      extent_concern = .data$extent_pct > .data$national_pct
    ) |>
    dplyr::select(-".below")
}

# T (timeliness) and S (stool adequacy) from district-year AFP process
# counts. Without `process` every column is NA; with it, a district-year
# missing from `process` has no cases.
# @noRd
.fg_add_process <- function(dy, process, id_col, target, min_cases) {
  count_cols <- c("n_cases", "n_adequate", "n_transport", "n_transport_timely")
  if (is.null(process)) {
    for (cl in count_cols) dy[[cl]] <- NA_integer_
    dy$pct_transport_timely <- NA_real_
    dy$pct_adequate <- NA_real_
    dy$transport_assessed <- NA
    dy$adequacy_assessed <- NA
    dy$timeliness_concern <- NA
    dy$adequacy_concern <- NA
    return(dy)
  }
  stopifnot(is.data.frame(process))
  missing_cols <- setdiff(c(id_col, "year", count_cols), names(process))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg process} is missing column{?s} {.val {missing_cols}}."
    )
  }
  proc <- process[, c(id_col, "year", count_cols)]
  proc$year <- as.integer(proc$year)
  dy <- dy[, setdiff(names(dy), count_cols)]
  dy |>
    dplyr::left_join(proc, by = c(id_col, "year")) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(count_cols),
        \(x) as.integer(dplyr::coalesce(x, 0L))
      ),
      pct_transport_timely = dplyr::if_else(
        .data$n_transport > 0L,
        100 * .data$n_transport_timely / .data$n_transport,
        NA_real_
      ),
      pct_adequate = dplyr::if_else(
        .data$n_cases > 0L,
        100 * .data$n_adequate / .data$n_cases,
        NA_real_
      ),
      transport_assessed = .data$n_transport >= min_cases,
      adequacy_assessed = .data$n_cases >= min_cases,
      timeliness_concern = dplyr::if_else(
        .data$transport_assessed, .data$pct_transport_timely < target, NA
      ),
      adequacy_concern = dplyr::if_else(
        .data$adequacy_assessed, .data$pct_adequate < target, NA
      )
    )
}

# ---------------------------------------------------------------------------
# signal helpers
# ---------------------------------------------------------------------------

# context: longest run of consecutive sub-cut years up to and including each
# year, and the run ending at each year.
# @noRd
.fg_add_persistence <- function(dy, id_col, spi_cut) {
  dy |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) |>
    dplyr::arrange(.data$year, .by_group = TRUE) |>
    dplyr::mutate(
      longest_run_below = .fg_cum_max_run(.data$spi_median < spi_cut),
      trailing_run_below = .fg_trailing_run(.data$spi_median < spi_cut)
    ) |>
    dplyr::ungroup()
}

# Poisson tail P(X <= observed | lambda = expected): the chance that sampling
# variation alone yields a count this low or lower against the model's
# expectation. Deliberately Poisson rather than negative-binomial. It
# understates the true spread, so gating on it errs towards keeping flags.
# @noRd
.fg_noise_tail <- function(observed, expected) {
  ok <- is.finite(observed) & is.finite(expected) & expected > 0
  out <- rep(NA_real_, length(observed))
  out[ok] <- stats::ppois(observed[ok], lambda = expected[ok])
  out
}

# consecutive-TRUE run ending at each year (NA treated as FALSE). unlike the
# running maximum this falls back to zero when a district recovers, so it can
# say whether the run is current rather than merely historical.
# @noRd
.fg_trailing_run <- function(below) {
  below <- as.integer(below)
  below[is.na(below)] <- 0L
  out <- integer(length(below))
  cur <- 0L
  for (i in seq_along(below)) {
    cur <- if (below[i] == 1L) cur + 1L else 0L
    out[i] <- cur
  }
  out
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

# context (trend): sign of the SPI-vs-year slope over the trailing `window`
# years. With `alpha` set, the slope must also be significantly different from
# zero (a real trend test) before it reads as falling / rising, else it is
# flat.
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

# context (neighbour contrast): neighbour-median SPI per district-year, and
# discordance flag.
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

# context seasonal: blind / muted / present through the expected peak
# months.
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

# AFP detection years up to and including each year.
# @noRd
.fg_add_detections <- function(dy, detections, id_col, detection_col,
                           serotype_col = NULL, keep_serotypes = NULL) {
  stopifnot(
    is.data.frame(detections),
    id_col %in% names(detections),
    "year" %in% names(detections)
  )
  g <- detections
  if (!is.null(detection_col)) {
    if (!detection_col %in% names(g)) {
      cli::cli_abort(
        "{.arg detection_col} {.val {detection_col}} not in {.arg detections}."
      )
    }
    g <- g[as.logical(g[[detection_col]]) %in% TRUE, , drop = FALSE]
  }
  g <- .fg_keep_serotypes(g, serotype_col, keep_serotypes, "detections")
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
  dy$orphan_serotypes <- .fg_serotypes_to(g, id_col, serotype_col, ids, yrs)
  dy
}

# keep only the serotypes that count as a detection. An ambiguous VDPV is not a
# confirmed circulating virus, so a programme reading usually wants WPV and
# cVDPV only; leaving this NULL counts whatever the input contains.
# @noRd
.fg_keep_serotypes <- function(g, serotype_col, keep, arg) {
  if (is.null(keep)) return(g)
  if (is.null(serotype_col)) {
    cli::cli_abort(
      "{.arg detection_serotypes} needs {.arg serotype_col} so the serotype \\
       of each detection is known."
    )
  }
  if (!serotype_col %in% names(g)) {
    cli::cli_abort(
      "{.arg serotype_col} {.val {serotype_col}} not in {.arg {arg}}."
    )
  }
  g[as.character(g[[serotype_col]]) %in% as.character(keep), , drop = FALSE]
}

# the distinct serotypes seen in a district up to and including each year, as a
# comma-joined string. NA throughout where the input names no serotype, so a
# reader downstream can tell "not recorded" from "none found".
# @noRd
.fg_serotypes_to <- function(g, id_col, serotype_col, ids, yrs) {
  if (is.null(serotype_col) || !serotype_col %in% names(g)) {
    return(rep(NA_character_, length(ids)))
  }
  keep <- !is.na(g[[serotype_col]]) & nzchar(as.character(g[[serotype_col]]))
  g <- g[keep, , drop = FALSE]
  by_id <- split(
    data.frame(
      year = as.integer(g$year),
      serotype = as.character(g[[serotype_col]]),
      stringsAsFactors = FALSE
    ),
    as.character(g[[id_col]])
  )
  vapply(seq_along(ids), function(i) {
    d <- by_id[[ids[i]]]
    if (is.null(d)) return("")
    v <- sort(unique(d$serotype[d$year <= yrs[i]]))
    if (length(v) == 0L) "" else paste(v, collapse = ", ")
  }, character(1))
}

# context ES: environmental-surveillance positive years up to and including
# each year. `es_col`, when given, is a count (>0) or logical (TRUE) positive
# flag.
# @noRd
.fg_add_es <- function(dy, es, id_col, es_col, serotype_col = NULL,
                      keep_serotypes = NULL) {
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
  g <- .fg_keep_serotypes(g, serotype_col, keep_serotypes, "es")
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
  dy$es_serotypes <- .fg_serotypes_to(g, id_col, serotype_col, ids, yrs)
  dy
}
# ---------------------------------------------------------------------------
# static reference content (field guide Table 1 STEPS + Table 3 misreadings)
# ---------------------------------------------------------------------------

# @noRd
.fg_reference <- function(spi_cut = 1) {
  tibble::tribble(
    ~signal, ~asks, ~interpret, ~corroborates,
    "S: Strength",
    "How large is the shortfall, and does the 90% credible interval lie entirely below the reference value of 1?",
    "A larger shortfall with an interval below 1 is stronger evidence. The difference between observed and expected counts is not a count of missed cases.",
    "No",
    "T: Timeliness",
    "Are specimens reaching the laboratory within 3 days?",
    "Measures how quickly specimens reach the laboratory after collection. It is reported separately and does not change the review label.",
    "No (independent)",
    "E: Extent",
    "Is an SPI below the chosen cutoff more common among other districts in the same admin-1 area than nationally?",
    "Checks whether a low SPI is more common in the same region than nationally.",
    "Yes",
    "P: Persistence",
    "Was SPI also below the chosen cutoff in the previous year?",
    "Checks whether the district also had a low SPI in the previous year.",
    "Yes (supporting)",
    "S: Stool adequacy",
    "Are stool specimens adequate?",
    "Reports the share of AFP cases with adequate stool specimens. It does not change the review label.",
    "No (independent)"
  )
}

# @noRd
.fg_misreadings <- function(spi_cut = 1) {
  cut <- format(spi_cut)
  tibble::tribble(
    ~misreading, ~correction,
    sprintf("SPI below %s means surveillance has failed.", cut),
    "Relative reporting was below the reference, which by default is the national observed-to-expected ratio. This does not establish surveillance failure. Review the size, uncertainty, persistence, and context of the shortfall.",
    sprintf("SPI of %s or above means surveillance is adequate.", cut),
    "The SPI cannot establish overall surveillance adequacy. Consider the conventional NPAFP rate, stool adequacy, timeliness, and other surveillance information.",
    "SPI below 1 means cases were missed.",
    "Relative reporting was below the reference. This does not establish why the shortfall occurred or how many cases, if any, were missed.",
    "A higher SPI always means surveillance improved.",
    "The SPI can rise because observed reporting increased, expected reporting decreased, or both. Examine the counts separately and seek other evidence of improvement.",
    "A district that meets the conventional NPAFP target does not need further review.",
    "A district can meet the minimum reporting target while reporting substantially fewer cases than expected from its population and reporting history. Interpret both indicators together.",
    "A low conventional NPAFP rate with an SPI near 1 is not concerning.",
    "Persistent low reporting can lower the modelled expectation. The SPI may not identify a long-standing shortfall. Persistent failure to meet the NPAFP target remains a reason for investigation.",
    "A low SPI means poliovirus is circulating.",
    "Poliovirus detections are not used to calculate the SPI. A low value identifies a relative reporting shortfall and does not establish whether poliovirus is present."
  )
}

# ---------------------------------------------------------------------------
# S3 methods
# ---------------------------------------------------------------------------

#' @export
print.spi_field_guide <- function(x, ...) {
  cli::cli_h1("SPI field guide")
  cli::cli_inform(c(
    "Review year: {.val {x$read_year}} \\
     | STEPS applied below SPI {.val {x$thresholds$spi}} \\
     ({x$thresholds$rule %||% 'median'} rule) \\
     | priority for review: 90% CrI below 1, with extent or \\
     persistence"
  ))

  active <- names(x$signals_active)[x$signals_active]
  inactive <- names(x$signals_active)[!x$signals_active]
  active_str <- if (length(active)) active else "none"
  inactive_str <- if (length(inactive)) {
    paste0(" | unavailable: ", paste(inactive, collapse = ", "))
  } else {
    ""
  }
  cli::cli_inform(
    "Available components and additional findings: {.val {active_str}}{inactive_str}"
  )

  foc <- x$focal
  cli::cli_h2("Review labels for {x$read_year}")
  n <- vapply(
    .FG_VERDICT_LEVELS,
    function(v) sum(foc$verdict == v),
    integer(1)
  )
  verdict_tbl <- tibble::tibble(
    verdict = .FG_VERDICT_LEVELS,
    n = unname(n),
    pct = round(100 * unname(n) / nrow(foc), 1)
  )
  print(verdict_tbl)

  priority <- foc[foc$verdict == "Priority for review", ]
  n_show <- min(10L, nrow(priority))
  if (n_show > 0) {
    cli::cli_h2(
      "Districts with priority for review (top {n_show} by size of shortfall)"
    )
    print(.fg_scan_tibble(x, priority), n = n_show)
  }
  cli::cli_alert_info(
    "See {.fn spi_field_guide_help} for worked examples, \\
     {.fn spi_field_guide_table} to render it."
  )
  invisible(x)
}

#' @export
summary.spi_field_guide <- function(object, ...) {
  print(object, ...)
  foc <- object$focal
  below <- foc[foc$spi_below %in% TRUE, ]
  cli::cli_h2(
    "STEPS concerns among {nrow(below)} district{?s} below the cut, \\
     {object$read_year}"
  )
  detection <- (below$genomic_orphan %in% TRUE) | (below$es_detected %in% TRUE)
  fires <- tibble::tibble(
    component = c("S Strength (90% CrI below 1)", "T Timeliness",
                  "E Extent", "P Persistence", "S Stool adequacy",
                  "+ Falling trend (context)", "+ Detection (context)"),
    n = c(
      sum(below$gate_pass, na.rm = TRUE),
      sum(below$timeliness_concern, na.rm = TRUE),
      sum(below$extent_concern, na.rm = TRUE),
      sum(below$persistence_concern, na.rm = TRUE),
      sum(below$adequacy_concern, na.rm = TRUE),
      sum(below$trajectory == "falling", na.rm = TRUE),
      sum(detection, na.rm = TRUE)
    )
  )
  print(fires)
  cli::cli_h2("STEPS reference")
  print(object$reference)
  invisible(object)
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.spi_field_guide <- function(x, ...) {
  x$district_year
}

# compact per-district scan tibble for a given focal slice, ranked by
# judgement then by size of shortfall.
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
    extent = foc$extent_concern,
    persist = foc$persistence_concern,
    transport = round(foc$pct_transport_timely),
    adequacy = round(foc$pct_adequate),
    verdict = as.character(foc$verdict)
  )
  out[order(factor(out$verdict, levels = .FG_VERDICT_LEVELS), out$spi), ]
}


# ============================================================================
# Field guide help  (learn to read the guide)

#' Learn to read the SPI field guide
#'
#' @description
#' Prints explanations in the R console. Describes the five STEPS components
#' and the review labels behind [spi_field_guide()], lists common errors in
#' interpreting SPI, and explains four districts selected from a field guide:
#' SPI at or above 1, an uncertain shortfall, a large shortfall supported by
#' extent and persistence, and a shortfall without nearby supporting evidence.
#'
#' @details
#' By default, the worked example uses `synth_field_guide`, the precomputed
#' result for the synthetic data, so no model fit is needed. Pass `guide`
#' to explain the results from your own [spi_field_guide()] call.
#'
#' @param topic One or more of `"all"`, `"signals"`, `"verdict"`,
#'   `"misreadings"`, `"example"`. Default: `"all"`.
#' @param guide A [spi_field_guide()] result to use for the worked example.
#'   Default: NULL (use `synth_field_guide`).
#' @param year Focal year for the worked example. Default: NULL
#'   (`guide$read_year`).
#'
#' @return The tibble of districts used in the worked example, returned invisibly.
#'
#' @seealso [spi_field_guide()], [spi_field_guide_table()].
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' spi_field_guide_help("signals")
#' spi_field_guide_help("verdict")
#' spi_field_guide_help("example")
spi_field_guide_help <- function(
  topic = c("all", "signals", "verdict", "misreadings", "example"),
  guide = NULL,
  year = NULL
) {
  .check_pkg("cli", reason = "to print the field-guide help")
  topic <- match.arg(topic, several.ok = TRUE)
  if ("all" %in% topic) {
    topic <- c("signals", "verdict", "misreadings", "example")
  }

  spi_cut <- if (!is.null(guide)) guide$thresholds$spi else 1

  cli::cli_h1("Reviewing districts with a low SPI")

  if ("signals" %in% topic) {
    cli::cli_h2("The five STEPS components")
    cli::cli_text(
      "For districts with an SPI below {format(spi_cut)}, five components \\
       help assess the wider surveillance picture: strength, timeliness, \\
       extent, persistence, and stool adequacy. They are interpreted \\
       together, not combined into a score, and they do not carry the same \\
       weight. No single component establishes a surveillance failure."
    )
    ref <- .fg_reference(spi_cut)
    for (i in seq_len(nrow(ref))) {
      cli::cli_h3(ref$signal[i])
      cli::cli_ul()
      cli::cli_li("{.strong Asks:} {ref$asks[i]}")
      cli::cli_li("{.strong Interpretation:} {ref$interpret[i]}")
      cli::cli_li("{.strong Adds evidence about the shortfall:} {ref$corroborates[i]}")
      cli::cli_end()
    }
  }

  if ("verdict" %in% topic) {
    cli::cli_h2("Review labels")
    cli::cli_text(
      "The review label uses the SPI value, its interval, and findings for \\
       extent and persistence. The labels help decide which districts to \\
       review first."
    )
    cli::cli_ul()
    cli::cli_li(
      "{.strong Priority for review} -- SPI below {format(spi_cut)}, the 90% \\
       credible interval entirely below 1, and extent or persistence also \\
       raises concern. If enabled, the sampling-noise check must also pass. \\
       These findings identify districts for closer review."
    )
    cli::cli_li(
      "{.strong Monitor} -- SPI below {format(spi_cut)}, but uncertainty or \\
       limited supporting information favours reassessment as new data \\
       become available."
    )
    cli::cli_li(
      "{.strong No SPI indication} -- no SPI indication for additional \\
       review. Routine surveillance and review based on other indicators \\
       continue."
    )
    cli::cli_end()
    if (identical(guide$thresholds$rule, "interval")) {
      cli::cli_text(
        "This guide uses the interval rule: a district-year whose 90% \\
         credible interval includes 1 receives no SPI indication."
      )
    }
    cli::cli_text(
      "Timeliness and stool adequacy are reported beside the review label but \\
       never change it. Trend, neighbouring-district context, seasonal patterns and \\
       any AFP or ES detection are context outside STEPS."
    )
  }

  if ("misreadings" %in% topic) {
    cli::cli_h2("Common interpretation errors")
    mis <- .fg_misreadings(spi_cut)
    for (i in seq_len(nrow(mis))) {
      cli::cli_ul()
      cli::cli_li("{.emph {mis$misreading[i]}}")
      cli::cli_li("{.strong What the SPI supports:} {mis$correction[i]}")
      cli::cli_end()
    }
  }

  worked <- NULL
  if ("example" %in% topic) {
    guide <- guide %||% .fg_load_synth()
    stopifnot(inherits(guide, "spi_field_guide"))
    year <- as.integer(year %||% guide$read_year)
    foc <- guide$district_year[guide$district_year$year == year, ,
                               drop = FALSE]
    id_col <- guide$id_col
    name_col <- intersect(c("adm2_name", "adm1_name"), names(foc))[1] %||%
      id_col
    sel <- .fg_select_worked(foc, id_col, guide$thresholds$spi)

    cli::cli_h2("Worked example ({year})")
    cli::cli_text(
      "Four districts selected to illustrate different SPI results and \\
       findings for the wider area and previous year:"
    )
    for (i in seq_len(nrow(sel))) {
      r <- as.list(sel[i, ])
      cli::cli_h3("{sel[[name_col]][i]} -- {sel$case_label[i]}")
      cli::cli_text(.fg_narrate(r, guide$thresholds$spi))
      action <- .fg_action(r)
      cli::cli_alert_info("Review label: {action}")
    }
    worked <- sel
  }

  invisible(worked)
}

# @noRd
.fg_load_synth <- function() {
  e <- new.env()
  ok <- tryCatch({
    utils::data("synth_field_guide", package = "spi", envir = e)
    TRUE
  }, error = function(err) FALSE)
  if (!ok || !exists("synth_field_guide", envir = e)) {
    cli::cli_abort(
      "{.val synth_field_guide} is not available; pass a \\
       {.fn spi_field_guide} result via {.arg guide}."
    )
  }
  e$synth_field_guide
}

# the area and national shares as printed. One decimal when the two round to
# the same whole number, so a concern never reads as "56% against 56%".
# @noRd
.fg_pct_pair <- function(area, national) {
  digits <- if (round(area) == round(national)) 1L else 0L
  sprintf(paste0("%.", digits, "f%%"), c(area, national))
}

# plain-language phrases for the persistence and extent findings
# @noRd
.fg_persist_phrase <- function(r, spi_cut) {
  if (is.na(r$spi_previous)) return("no SPI for the previous year")
  sprintf("its SPI was %s %s in %d (%.2f)",
          if (isTRUE(r$persistence_concern)) "also below" else "at or above",
          format(spi_cut), as.integer(r$year) - 1L, r$spi_previous)
}

# @noRd
.fg_extent_phrase <- function(r, spi_cut) {
  if (is.na(r$extent_others) || r$extent_others == 0L) {
    return("extent was not assessable")
  }
  pct <- .fg_pct_pair(r$extent_pct, r$national_pct)
  sprintf(
    "%d of %d other districts in %s were below %s (%s, against %s nationally)",
    as.integer(r$extent_others_below), as.integer(r$extent_others),
    r$extent_area, format(spi_cut), pct[1], pct[2]
  )
}

# one-paragraph, data-driven reading of a district-year row.
# @noRd
.fg_narrate <- function(r, spi_cut) {
  label <- r$case_label %||% as.character(r$verdict)
  spi <- sprintf("%.2f", r$spi_median)
  cri <- sprintf("90%% credible interval %.2f to %.2f", r$spi_q05, r$spi_q95)
  persist <- .fg_persist_phrase(r, spi_cut)
  extent <- .fg_extent_phrase(r, spi_cut)
  persist_cap <- paste0(toupper(substr(persist, 1, 1)), substring(persist, 2))

  switch(
    label,
    "SPI at or above cutoff" = paste0(
      sprintf("SPI %s (%s), so STEPS is not applied. ", spi, cri),
      if (is.na(r$spi_previous)) "" else sprintf(
        paste0("Its SPI was %.2f in %d, which shows why a single year at or ",
               "above %s should still be read alongside earlier values. "),
        r$spi_previous, as.integer(r$year) - 1L, format(spi_cut)
      ),
      "The SPI provides no current indication for additional review."
    ),
    "Uncertain shortfall" = paste0(
      sprintf(
        paste0("SPI %s, and its 90%% credible interval extends to %.2f, so ",
               "an SPI of 1 remains compatible with the data and model. %s, and %s. "),
        spi, r$spi_q95, persist_cap, extent
      ),
      if (identical(as.character(r$verdict), "Monitor")) {
        paste0("The interval includes 1. Review the result again when more data ",
               "are available.")
      } else {
        paste0("Under the interval rule, a shortfall whose interval includes ",
               "1 receives no SPI indication.")
      }
    ),
    "Large shortfall with supporting evidence" = sprintf(
      paste0("NPAFP rate %.1f, yet %d cases reported against %.1f expected, ",
             "giving an SPI of %s (%s). %s, and %s. The interval is below 1, ",
             "and the area or previous year also raises concern. These ",
             "findings support closer review."),
      r$npafp_rate, as.integer(round(r$observed)), r$expected_total, spi,
      cri, persist_cap, extent
    ),
    "Shortfall without wider area concern" = sprintf(
      paste0("SPI %s (%s); %s. However, %s. The shortfall warrants review, ",
             "and local explanations, including population estimates and ",
             "boundary changes, should be checked."),
      spi, cri, persist, extent
    ),
    sprintf("SPI %s (%s); %s; %s. Review label: %s.", spi, cri, persist, extent,
            as.character(r$verdict))
  )
}

# @noRd
.fg_action <- function(r) {
  switch(
    as.character(r$verdict),
    "Priority for review" = paste(
      "priority for review. The relative reporting shortfall is",
      "strong enough, with extent or persistence also raising concern, to",
      "warrant further investigation."
    ),
    "Monitor" = paste(
      "monitor. Uncertainty or limited supporting information favours",
      "reassessment as new data become available."
    ),
    paste(
      "no SPI indication for additional review. Routine surveillance and",
      "review based on other indicators continue."
    )
  )
}


# ============================================================================
# Field guide table (gt / flextable rendering)

#' Render the SPI field guide as a gt or flextable table
#'
#' @description
#' Turns a [spi_field_guide()] result into a formatted table, in
#' either of two layouts, using `gt` or `flextable`. Optionally saves to
#' file; the format is inferred from the extension (html / docx / pdf / rtf /
#' png / pptx).
#'
#' @details
#' Two layouts:
#' \itemize{
#'   \item `"scan"` -- one row per district for the focal year, with the
#'     STEPS components and the label; the label cell is shaded.
#'   \item `"worked"` -- the field guide's teaching layout: the five STEPS
#'     components, the conventional NPAFP rate and the label as rows, a
#'     few districts as columns, every cell shaded as adding to concern
#'     (rose), intermediate (amber) or reassuring (green). When `districts` is
#'     NULL, four districts (SPI at or above 1, an uncertain shortfall,
#'     a large shortfall with supporting evidence, and a shortfall without
#'     wider area concern) are selected.
#' }
#'
#' @param x A [spi_field_guide()] result (class `spi_field_guide`).
#' @param engine Table engine: `"gt"` or `"flextable"`.
#' @param layout `"scan"` (districts as rows) or `"worked"` (components as
#'   rows).
#' @param year Focal year. Default: NULL (`x$read_year`).
#' @param districts Character vector of district ids for the `"worked"`
#'   layout. Default: NULL (select four districts with different findings).
#' @param name_col Column to label districts by. Default: NULL (first of
#'   `adm2_name`, `adm1_name`, else the id column).
#' @param file Optional output path; the extension sets the format. Default:
#'   NULL (return the table object without saving).
#' @param shade Logical. Apply concern shading. Default: TRUE.
#' @param max_rows Integer. For the `"scan"` layout, cap the number of rows
#'   (priority for review first, then by size of shortfall). Default: 50.
#' @param ... Passed to the underlying `gt::gtsave()` / flextable saver.
#'
#' @return The `gt_tbl` or `flextable` object, invisibly when `file` is set.
#'
#' @seealso [spi_field_guide()], [spi_field_guide_help()].
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' fg <- synth_field_guide
#' \dontrun{
#' spi_field_guide_table(fg, engine = "gt", layout = "worked")
#' spi_field_guide_table(fg, engine = "gt", layout = "scan",
#'                      file = tempfile(fileext = ".html"))
#' spi_field_guide_table(fg, engine = "flextable", layout = "worked",
#'                      file = tempfile(fileext = ".docx"))
#' }
spi_field_guide_table <- function(
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
  stopifnot(inherits(x, "spi_field_guide"))
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
    intersect(c("adm2_name", "adm1_name", id_col), names(foc))[1]
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
# shading palette (matches the field guide's Table 2)
# ---------------------------------------------------------------------------

# @noRd
FG_CLASS_FILL <- c(
  warm = "#f4cccc", # adds to concern
  amber = "#fce5cd", # intermediate
  cool = "#d9ead3", # reassuring
  none = "#ffffff" # not assessable
)

# @noRd
.fg_verdict_class <- function(verdict) {
  vapply(as.character(verdict), function(v) {
    switch(v, "Priority for review" = "warm", "Monitor" = "amber", "cool")
  }, character(1))
}

# ---------------------------------------------------------------------------
# scan layout
# ---------------------------------------------------------------------------

# @noRd
.fg_yes_no <- function(x) {
  out <- ifelse(x, "Yes", "No")
  out[is.na(x)] <- "Not assessed"
  out
}

# @noRd
.fg_pct_word <- function(pct, assessed) {
  out <- sprintf("%.0f%%", pct)
  out[!(assessed %in% TRUE)] <- "Not assessed"
  out
}

# @noRd
.fg_build_scan <- function(foc, engine, id_col, name_col, spi_cut, year,
                           shade, max_rows) {
  # by judgement, then size of shortfall within it, so `max_rows` truncates
  # from the bottom of the triage rather than cutting across it
  foc <- foc[
    order(factor(as.character(foc$verdict), levels = .FG_VERDICT_LEVELS),
          foc$spi_median), ,
    drop = FALSE
  ]
  if (nrow(foc) > max_rows) foc <- foc[seq_len(max_rows), , drop = FALSE]

  df <- tibble::tibble(
    District = foc[[name_col]],
    SPI = round(foc$spi_median, 2),
    `90% CrI` = sprintf("%.2f-%.2f", foc$spi_q05, foc$spi_q95),
    NPAFP = round(foc$npafp_rate, 1),
    `Transport <=3d` = .fg_pct_word(foc$pct_transport_timely,
                                    foc$transport_assessed),
    Extent = .fg_yes_no(foc$extent_concern),
    `Previous yr below` = .fg_yes_no(foc$persistence_concern),
    `Stool adequacy` = .fg_pct_word(foc$pct_adequate, foc$adequacy_assessed),
    `Review label` = as.character(foc$verdict)
  )
  vclass <- .fg_verdict_class(foc$verdict)
  title <- sprintf("SPI field guide: district scan, %d", year)
  subtitle <- sprintf(
    paste0("STEPS applied below SPI %s; priority for review when the 90%% ",
           "CrI lies below 1 and extent or persistence raises concern"),
    format(spi_cut)
  )

  if (engine == "gt") {
    g <- gt::gt(df)
    g <- gt::tab_header(g, title = title, subtitle = subtitle)
    if (shade) {
      for (cl in unique(vclass)) {
        g <- gt::tab_style(
          g,
          style = gt::cell_fill(color = FG_CLASS_FILL[[cl]]),
          locations = gt::cells_body(columns = "Review label", rows = vclass == cl)
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
          ft, i = i, j = "Review label", bg = FG_CLASS_FILL[[vclass[i]]]
        )
      }
    }
    flextable::autofit(ft)
  }
}

# ---------------------------------------------------------------------------
# worked layout (components as rows, districts as columns)
# ---------------------------------------------------------------------------

# @noRd
.fg_build_worked <- function(x, foc, engine, id_col, name_col, spi_cut, year,
                             shade, districts) {
  sel <- .fg_select_worked(foc, id_col, spi_cut, districts)
  if (nrow(sel) == 0L) {
    cli::cli_abort("no districts available for the worked example.")
  }

  target <- x$params$process_target %||% 80
  min_cases <- x$params$process_min_cases %||% 5L
  row_key <- c("strength", "timeliness", "extent", "persistence",
               "adequacy", "conventional", "verdict")
  row_lab <- c(
    strength = "S: Strength (size and uncertainty of the shortfall)",
    timeliness = "T: Timeliness (specimen transport to the laboratory)",
    extent = "E: Extent (other districts in the same admin-1 area)",
    persistence = "P: Persistence (SPI in the previous year)",
    adequacy = "S: Stool adequacy",
    conventional = "Conventional NPAFP rate",
    verdict = "Review label"
  )

  cells <- lapply(seq_len(nrow(sel)), function(i) {
    .fg_cell_signals(as.list(sel[i, ]), spi_cut, min_cases)
  })
  classes <- lapply(seq_len(nrow(sel)), function(i) {
    .fg_cell_classes(as.list(sel[i, ]), spi_cut)
  })

  col_ids <- paste0("d", seq_len(nrow(sel)))
  col_head <- sprintf("%s\n(%s)", sel[[name_col]], sel$case_label)

  df <- tibble::tibble(Signal = unname(row_lab[row_key]))
  for (j in seq_along(col_ids)) {
    df[[col_ids[j]]] <- unname(cells[[j]][row_key])
  }
  cmat <- vapply(seq_along(col_ids), function(j) {
    unname(classes[[j]][row_key])
  }, character(length(row_key)))

  title <- sprintf(
    "Applying the five STEPS components to %d districts, %d",
    nrow(sel), year
  )
  footer <- sprintf(
    paste0("STEPS applies only where the SPI is below %s; values for a ",
           "district at or above it are shown for context. Timeliness is the ",
           "share of specimens received by the laboratory within 3 days of ",
           "the second stool collection, and stool adequacy the share of AFP ",
           "cases with adequate specimens, each assessed with at least %d AFP ",
           "cases against a target of %s%%. Extent compares other districts in ",
           "the same admin-1 area with the national share below %s. Shading ",
           "marks a finding that adds to concern (rose), an intermediate ",
           "finding (amber), or a reassuring finding (green); unshaded cells ",
           "are not assessable."),
    format(spi_cut), as.integer(min_cases), format(target), format(spi_cut)
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
# worked-example district selection (the field guide's four teaching cases)
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
  is_true <- function(x) !is.na(x) & x

  # at or above expectation: conventionally adequate, largest expected count
  good <- pick(
    dplyr::filter(
      remaining(),
      .data$verdict == "No SPI indication", !is_true(.data$spi_below),
      .data$conventional_pass
    ),
    dplyr::desc(.data$expected_total)
  )
  good <- good %||% pick(
    dplyr::filter(
      remaining(),
      .data$verdict == "No SPI indication", !is_true(.data$spi_below)
    ),
    dplyr::desc(.data$spi_median)
  )
  if (!is.null(good)) used <- c(used, good[[id_col]])

  # uncertain shortfall: below the cut, interval reaches 1, corroborated
  uncertain <- pick(
    dplyr::filter(
      remaining(),
      .data$spi_below, .data$spi_q95 >= 1
    ),
    dplyr::desc(is_true(.data$persistence_concern) &
                  is_true(.data$extent_concern)),
    .data$spi_median
  )
  if (!is.null(uncertain)) used <- c(used, uncertain[[id_col]])

  # large, corroborated shortfall: priority for review on both corroborators,
  # conventionally adequate where possible, largest shortfall
  corrob <- pick(
    dplyr::filter(
      remaining(),
      .data$verdict == "Priority for review"
    ),
    dplyr::desc(is_true(.data$extent_concern) &
                  is_true(.data$persistence_concern)),
    dplyr::desc(.data$conventional_pass),
    .data$spi_median
  )
  if (!is.null(corrob)) used <- c(used, corrob[[id_col]])

  # shortfall without spatial corroboration: certain and persistent, but the
  # area is no more often below the cut than the country
  local <- pick(
    dplyr::filter(
      remaining(),
      .data$spi_below, .data$gate_pass,
      is_true(.data$persistence_concern), !is_true(.data$extent_concern)
    ),
    .data$spi_median
  )

  parts <- list(
    .fg_tag(good, "SPI at or above cutoff"),
    .fg_tag(uncertain, "Uncertain shortfall"),
    .fg_tag(corrob, "Large shortfall with supporting evidence"),
    .fg_tag(local, "Shortfall without wider area concern")
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
.fg_process_cell <- function(ok, n, what, min_cases) {
  if (is.na(n)) return("Not supplied")
  if (n < min_cases) {
    return(sprintf("%d case%s; fewer than %d, not assessed", as.integer(n),
                   if (n == 1L) "" else "s", as.integer(min_cases)))
  }
  sprintf("%d of %d %s (%.0f%%)", as.integer(ok), as.integer(n), what,
          100 * ok / n)
}

# @noRd
.fg_cell_signals <- function(r, spi_cut, min_cases = 5L) {
  position <- if (r$spi_q95 < 1) {
    "entirely below 1"
  } else if (r$spi_q05 > 1) {
    "entirely above 1"
  } else {
    "includes 1"
  }
  strength <- sprintf(
    "SPI %.2f (90%% CrI %.2f to %.2f; %s); %d observed vs %.1f expected",
    r$spi_median, r$spi_q05, r$spi_q95, position,
    as.integer(round(r$observed)), r$expected_total
  )
  if (isTRUE(r$cri_excludes_1) && !.fg_gate_pass(r)) {
    strength <- paste0(strength, "; sampling noise not ruled out")
  }
  extent <- if (is.na(r$extent_others) || r$extent_others == 0L) {
    "Not assessable"
  } else {
    pct <- .fg_pct_pair(r$extent_pct, r$national_pct)
    sprintf("%d of %d other districts in %s below %s (%s; national %s)",
            as.integer(r$extent_others_below), as.integer(r$extent_others),
            r$extent_area, format(spi_cut), pct[1], pct[2])
  }
  persistence <- if (is.na(r$spi_previous)) {
    "No SPI for the previous year"
  } else {
    sprintf("SPI %.2f in %d, %s", r$spi_previous, as.integer(r$year) - 1L,
            if (isTRUE(r$persistence_concern)) {
              sprintf("below %s", format(spi_cut))
            } else {
              sprintf("at or above %s", format(spi_cut))
            })
  }
  c(
    strength = strength,
    timeliness = .fg_process_cell(
      r$n_transport_timely, r$n_transport,
      "specimens at the laboratory within 3 days", min_cases
    ),
    extent = extent,
    persistence = persistence,
    adequacy = .fg_process_cell(
      r$n_adequate, r$n_cases, "cases with adequate stool specimens",
      min_cases
    ),
    conventional = sprintf("NPAFP rate %.1f per 100 000; %s conventional target",
                           r$npafp_rate,
                           if (isTRUE(r$conventional_pass)) "meets" else
                             "below"),
    verdict = as.character(r$verdict)
  )
}

# @noRd
.fg_cell_classes <- function(r, spi_cut = 1) {
  concern_class <- function(x) {
    if (is.na(x)) "none" else if (isTRUE(x)) "warm" else "cool"
  }
  c(
    strength = if (isTRUE(r$spi_below) && .fg_gate_pass(r)) "warm"
               else if (isTRUE(r$spi_below)) "amber" else "cool",
    timeliness = concern_class(r$timeliness_concern),
    extent = concern_class(r$extent_concern),
    persistence = concern_class(r$persistence_concern),
    adequacy = concern_class(r$adequacy_concern),
    conventional = if (isTRUE(r$conventional_pass)) "cool" else "warm",
    # unname: vapply keeps the judgement string as the element name
    verdict = unname(.fg_verdict_class(r$verdict))
  )
}

# @noRd
.fg_seasonal_word <- function(x) {
  map <- c(blind = "Blind through peak", muted = "Muted",
           present = "Detects in peak", `not assessed` = "Not assessable")
  out <- unname(map[x])
  out[is.na(x) | is.na(out)] <- "Not assessed"
  out
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
