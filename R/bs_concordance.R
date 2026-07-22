#' Cross-classify SPI against the conventional NPAFP-rate threshold
#'
#' @description
#' Compares the model-based Surveillance Performance Index against the
#' WHO conventional NPAFP-rate threshold on the same district-year units
#' and summarises the four-cell concordance table. The four cells match
#' the paper's terminology:
#' \itemize{
#'   \item **Both adequate** -- NPAFP >= target AND SPI >= threshold. The
#'     surveillance system is doing what conventional monitoring expects
#'     and what the model expects.
#'   \item **True shortfall** -- NPAFP < target AND SPI < threshold. Both
#'     indicators agree the district is under-detecting; act on it.
#'   \item **False reassurance** -- NPAFP >= target BUT SPI < threshold.
#'     The conventional rate says OK, the model says failing. These are
#'     silent-failure districts the raw threshold misses. In the paper
#'     this is the headline refinement: SPI catches under-detection that
#'     NPAFP alone does not.
#'   \item **False alarm** -- NPAFP < target BUT SPI >= threshold. The
#'     conventional rate flags the district but the model, accounting
#'     for its size / rurality / neighbour profile, does not.
#' }
#'
#' Concordance is quantified as raw percent agreement and Cohen's kappa,
#' plus a McNemar chi-square testing marginal-shift.
#'
#' @param spi Object of class `blindspot_spi` at `district_year` level.
#' @param cases Optional tibble with the district id column and `count`
#'   (integer). Pass to override the counts stored in `spi$data`. Default:
#'   NULL (use `spi$data`).
#' @param population Tibble with the district id column, `year` (integer),
#'   and the population denominator column (see `pop_col`). Required.
#' @param spi_threshold Numeric. SPI median below this = "fail". Paper
#'   uses 0.80. Default: 0.80.
#' @param npafp_target Numeric. NPAFP rate below this per `npafp_multiplier`
#'   person-years = "fail". WHO's target for polio surveillance is
#'   traditionally 2 per 100,000 under-15; the paper uses 3 per 100,000.
#'   Default: 3.
#' @param npafp_multiplier Numeric. Denominator scaling for the NPAFP rate.
#'   Default: 100000 (per 100,000 under-15 person-years).
#' @param strata Character vector of column names to stratify concordance
#'   by. Any column present in the district-year summary is valid:
#'   `"year"`, `"adm1_name"`, `"adm0_name"`, or any joined covariate.
#'   Set NULL for pooled analysis. Default: NULL.
#' @param id_col Character. Name of the district id column. Inferred from
#'   `spi$id_col` if NULL. Default: NULL.
#' @param pop_col Character. Population denominator column name. Default:
#'   `"pop_u15"`.
#' @param boundaries Optional `sf` object with the district id column and
#'   any adm1/adm0 name columns. When supplied its non-geometry columns
#'   are joined onto the district-year table so `strata` can reference
#'   region/country labels. Default: NULL.
#' @param verbose Logical. Progress messages via cli. Default: TRUE.
#'
#' @return An object of class `blindspot_concordance`. A list with:
#' \describe{
#'   \item{district_year}{Tibble with per-district-year classification:
#'     `{id_col}`, `year`, `count_annual`, `pop_u15`, `npafp_rate`,
#'     `npafp_adequate` (logical), `spi_median`, `spi_q05`, `spi_q95`,
#'     `spi_pass` (logical), `concordance` (factor: Both adequate /
#'     True shortfall / False reassurance / False alarm).}
#'   \item{crosstab}{2x2 table of counts and row/column percentages.}
#'   \item{metrics}{Pooled scalar metrics: `n`, `pct_agreement`,
#'     `cohens_kappa`, `mcnemar_p`, plus per-cell counts.}
#'   \item{by_stratum}{Tibble of per-stratum metrics when `strata` is
#'     non-NULL; else NULL.}
#'   \item{thresholds}{Named list echoing the SPI + NPAFP cuts used.}
#'   \item{id_col}{The id column name.}
#'   \item{call}{Matched call.}
#' }
#'
#' @seealso [bs_spi()], [bs_expected()]
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' \dontrun{
#' spi_dy <- bs_spi(fit_bare, level = "district_year")
#'
#' conc <- bs_concordance(
#'   spi             = spi_dy,
#'   population      = synth_surveillance$population,
#'   spi_threshold   = 0.80,
#'   npafp_target    = 3,
#'   strata          = c("year", "adm1_name"),
#'   boundaries      = synth_surveillance$boundaries
#' )
#'
#' print(conc)
#' summary(conc)
#' plot(conc)
#' }
bs_concordance <- function(
  spi,
  cases = NULL,
  population,
  spi_threshold = 0.80,
  npafp_target = 3,
  npafp_multiplier = 100000L,
  strata = NULL,
  id_col = NULL,
  pop_col = "pop_u15",
  boundaries = NULL,
  verbose = TRUE
) {
  .check_pkg(c("dplyr", "tibble", "cli"),
             reason = "to run the SPI x NPAFP concordance analysis")

  stopifnot(inherits(spi, "blindspot_spi"))

  id_col <- id_col %||% spi$id_col %||% "district_id"
  if (!id_col %in% names(spi$summary)) {
    cli::cli_abort(
      "{.arg spi$summary} lacks id column {.val {id_col}}; set {.arg id_col}."
    )
  }
  if (!"year" %in% names(spi$summary)) {
    cli::cli_abort(
      "{.fn bs_concordance} expects a district-year SPI; got level \\
       {.val {spi$level}}. Re-run {.fn bs_spi} with \\
       {.code level = \"district_year\"}."
    )
  }
  stopifnot(is.data.frame(population),
            id_col %in% names(population),
            "year" %in% names(population),
            pop_col %in% names(population))

  # ---- 1. Build the district-year skeleton from spi + population ----
  spi_sum <- spi$summary |>
    dplyr::select(
      dplyr::all_of(c(id_col, "year")),
      dplyr::any_of(c("observed", "expected_total",
                      "spi_median", "spi_q05", "spi_q95"))
    )

  pop <- population |>
    dplyr::select(
      dplyr::all_of(c(id_col, "year")),
      pop_u15 = dplyr::all_of(pop_col)
    )

  # ---- 2. Annual NPAFP count from cases (spi$data if not supplied) ----
  cases_src <- cases %||% spi$data
  if (is.null(cases_src)) {
    cli::cli_abort(
      "no {.arg cases} supplied and {.code spi$data} is empty; \\
       pass an annual-summable case table."
    )
  }
  if (!"count" %in% names(cases_src) || !id_col %in% names(cases_src)) {
    cli::cli_abort(
      "{.arg cases} must have columns {.val {id_col}} and {.val count}."
    )
  }
  cases_annual <- cases_src |>
    dplyr::mutate(
      year = if ("year" %in% names(cases_src)) {
        as.integer(cases_src$year)
      } else {
        as.integer(format(cases_src$month, "%Y"))
      }
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_col, "year")))) |>
    dplyr::summarise(count_annual = sum(.data$count, na.rm = TRUE),
                     .groups = "drop")

  # ---- 3. Assemble and classify ----
  dy <- spi_sum |>
    dplyr::inner_join(cases_annual, by = c(id_col, "year")) |>
    dplyr::inner_join(pop, by = c(id_col, "year")) |>
    dplyr::mutate(
      npafp_rate = .data$count_annual / .data$pop_u15 * npafp_multiplier,
      npafp_adequate = .data$npafp_rate >= npafp_target,
      spi_pass = .data$spi_median >= spi_threshold,
      concordance = dplyr::case_when(
        .data$npafp_adequate & .data$spi_pass ~ "Both adequate",
        !.data$npafp_adequate & !.data$spi_pass ~ "True shortfall",
        .data$npafp_adequate & !.data$spi_pass ~ "False reassurance",
        !.data$npafp_adequate & .data$spi_pass ~ "False alarm"
      ),
      concordance = factor(
        .data$concordance,
        levels = c("Both adequate", "True shortfall",
                   "False reassurance", "False alarm")
      )
    )

  # optional join with boundaries so `strata` can reference adm1/adm0 labels
  if (!is.null(boundaries)) {
    bnd_flat <- boundaries
    if (inherits(bnd_flat, "sf")) bnd_flat <- sf::st_drop_geometry(bnd_flat)
    if (id_col %in% names(bnd_flat)) {
      # avoid duplicating columns already present in dy
      keep_cols <- setdiff(names(bnd_flat), setdiff(names(dy), id_col))
      dy <- dplyr::left_join(dy, bnd_flat[, keep_cols], by = id_col)
    }
  }

  # ---- 4. Pooled metrics + crosstab ----
  crosstab <- .concordance_crosstab(dy)
  metrics  <- .concordance_metrics(dy)

  # ---- 5. Optional stratified metrics ----
  by_stratum <- NULL
  if (!is.null(strata)) {
    strata <- as.character(strata)
    missing_cols <- setdiff(strata, names(dy))
    if (length(missing_cols) > 0) {
      cli::cli_abort(
        "{.arg strata} column(s) {.val {missing_cols}} not in the \\
         district-year table."
      )
    }
    by_stratum <- dy |>
      dplyr::group_by(dplyr::across(dplyr::all_of(strata))) |>
      dplyr::group_modify(~ .concordance_metrics(.x)) |>
      dplyr::ungroup()
  }

  if (verbose) {
    cli::cli_alert_info(
      "{format(metrics$n, big.mark = ',')} district-years | \\
       agreement {sprintf('%.1f%%', metrics$pct_agreement)} | \\
       kappa {sprintf('%.2f', metrics$cohens_kappa)}"
    )
  }

  structure(
    list(
      district_year = tibble::as_tibble(dy),
      crosstab = crosstab,
      metrics = metrics,
      by_stratum = by_stratum,
      thresholds = list(spi = spi_threshold, npafp = npafp_target,
                           multiplier = npafp_multiplier),
      id_col = id_col,
      call = match.call()
    ),
    class = "blindspot_concordance"
  )
}

# ---------------------------------------------------------------------------
# internal helpers
# ---------------------------------------------------------------------------

# 2x2 crosstab with row/column percentages
.concordance_crosstab <- function(dy) {
  tab <- table(
    NPAFP = factor(
      ifelse(dy$npafp_adequate, "adequate", "inadequate"),
      levels = c("adequate", "inadequate")
    ),
    SPI = factor(
      ifelse(dy$spi_pass, "pass", "fail"),
      levels = c("pass", "fail")
    )
  )
  n <- sum(tab)
  list(
    counts = tab,
    row_pct = round(100 * prop.table(tab, margin = 1), 1),
    col_pct = round(100 * prop.table(tab, margin = 2), 1),
    total_pct = round(100 * tab / n, 1)
  )
}

# pooled metrics; also used inside group_modify for stratified splits
.concordance_metrics <- function(dy) {
  n <- nrow(dy)
  cells <- c("Both adequate", "True shortfall",
             "False reassurance", "False alarm")
  counts <- vapply(cells, function(c) sum(dy$concordance == c), integer(1))

  # kappa on 0/1 verdict vectors
  who <- as.integer(dy$npafp_adequate)
  spi <- as.integer(dy$spi_pass)
  kappa <- .cohens_kappa(who, spi)

  # McNemar's chi-square: does one metric systematically shift verdicts
  # relative to the other? Uses the two off-diagonal cells.
  mcn <- .mcnemar_p(dy)

  agreement <- 100 * mean(who == spi)

  tibble::tibble(
    n = n,
    pct_agreement = agreement,
    cohens_kappa = kappa,
    mcnemar_p = mcn,
    n_both_adequate = counts[["Both adequate"]],
    n_true_shortfall = counts[["True shortfall"]],
    n_false_reassurance = counts[["False reassurance"]],
    n_false_alarm = counts[["False alarm"]]
  )
}

.cohens_kappa <- function(who_vec, spi_vec) {
  n <- length(who_vec)
  if (n == 0L) return(NA_real_)
  ct <- table(
    who = factor(who_vec, levels = c(0L, 1L)),
    spi = factor(spi_vec, levels = c(0L, 1L))
  )
  p_obs <- (ct[1L, 1L] + ct[2L, 2L]) / n
  row_marginals <- rowSums(ct) / n
  col_marginals <- colSums(ct) / n
  p_exp <- sum(row_marginals * col_marginals)
  if (isTRUE(all.equal(p_exp, 1))) return(NA_real_)
  (p_obs - p_exp) / (1 - p_exp)
}

.mcnemar_p <- function(dy) {
  # off-diagonals of the concordance table
  b <- sum(dy$concordance == "False reassurance")  # NPAFP-yes, SPI-no
  c <- sum(dy$concordance == "False alarm")        # NPAFP-no,  SPI-yes
  if (b + c < 1L) return(NA_real_)
  # exact binomial test on off-diagonals (McNemar exact)
  stats::binom.test(b, b + c, p = 0.5)$p.value
}

# ---------------------------------------------------------------------------
# S3 methods
# ---------------------------------------------------------------------------

#' @export
print.blindspot_concordance <- function(x, ...) {
  cli::cli_h1("SPI x NPAFP concordance")
  cli::cli_inform(c(
    "SPI cut: {.val {x$thresholds$spi}} \\
     | NPAFP target: {.val {x$thresholds$npafp}} per \\
     {format(x$thresholds$multiplier, big.mark = ',')} person-years"
  ))
  cli::cli_inform(c(
    "{format(x$metrics$n, big.mark = ',')} district-years | \\
     agreement {sprintf('%.1f%%', x$metrics$pct_agreement)} | \\
     kappa {sprintf('%.2f', x$metrics$cohens_kappa)} | \\
     McNemar p {ifelse(is.na(x$metrics$mcnemar_p), 'NA',
                        sprintf('%.4f', x$metrics$mcnemar_p))}"
  ))
  cli::cli_h2("Four-cell counts")
  cell_tbl <- tibble::tibble(
    cell = c("Both adequate", "True shortfall",
             "False reassurance", "False alarm"),
    n = c(x$metrics$n_both_adequate, x$metrics$n_true_shortfall,
             x$metrics$n_false_reassurance, x$metrics$n_false_alarm),
    pct = round(100 * c(x$metrics$n_both_adequate,
                         x$metrics$n_true_shortfall,
                         x$metrics$n_false_reassurance,
                         x$metrics$n_false_alarm) / x$metrics$n, 1)
  )
  print(cell_tbl)
  if (!is.null(x$by_stratum)) {
    cli::cli_h2("By stratum")
    print(x$by_stratum, n = Inf)
  }
  invisible(x)
}

#' @export
summary.blindspot_concordance <- function(object, ...) {
  print(object, ...)
  cli::cli_h2("2x2 crosstab (counts)")
  print(object$crosstab$counts)
  cli::cli_h2("Row percentages (within NPAFP class)")
  print(object$crosstab$row_pct)
  invisible(object)
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.blindspot_concordance <- function(x, ...) {
  x$district_year
}

#' @export
plot.blindspot_concordance <- function(x, ...) {
  .check_pkg(c("ggplot2"), reason = "to plot concordance")
  dy <- x$district_year
  spi_cut  <- x$thresholds$spi
  npafp_target <- x$thresholds$npafp
  pal <- c(
    "Both adequate" = "#2E7D32",
    "True shortfall" = "#C62828",
    "False reassurance" = "#F9A825",
    "False alarm" = "#1565C0"
  )
  # Legend labels carry each cell's share of all district-years (from
  # x$metrics).
  m <- x$metrics
  pct <- c(
    "Both adequate" = m$n_both_adequate,
    "True shortfall" = m$n_true_shortfall,
    "False reassurance" = m$n_false_reassurance,
    "False alarm" = m$n_false_alarm
  ) / m$n * 100
  cell_labels <- stats::setNames(
    sprintf("%s (%.1f%%)", names(pct), pct), names(pct)
  )
  # Darker shades for the in-plot corner labels so each quadrant name reads
  # clearly (a deep tone of its cell colour) over the faint quadrant tint.
  label_pal <- c(
    "Both adequate" = "#1B5E20",
    "True shortfall" = "#8E1B1B",
    "False reassurance" = "#B8860B",
    "False alarm" = "#0D47A1"
  )
  # Quadrant backdrop: a faint tint and a corner label per cell of the 2x2, so
  # the scatter reads as the crosstab the reader has already seen without them
  # hunting the legend. Rows split on the SPI cut, columns on the NPAFP target.
  # 0 (not -Inf) is the left x-bound: the axis is log1p, and log1p(-Inf) is NaN,
  # so an -Inf edge would drop the left column. 0 is the natural rate floor.
  quad <- data.frame(
    xmin = c(0, npafp_target, 0, npafp_target),
    xmax = c(npafp_target, Inf, npafp_target, Inf),
    ymin = c(-Inf, -Inf, spi_cut, spi_cut),
    ymax = c(spi_cut, spi_cut, Inf, Inf),
    cell = factor(
      c("True shortfall", "False reassurance", "False alarm", "Both adequate"),
      levels = names(pal)
    )
  )
  ggplot2::ggplot(
    dy,
    ggplot2::aes(x = .data$npafp_rate, y = .data$spi_median)
  ) +
    ggplot2::geom_rect(
      data = quad, inherit.aes = FALSE, alpha = 0.10,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = .data$ymin, ymax = .data$ymax, fill = .data$cell)
    ) +
    ggplot2::geom_vline(xintercept = npafp_target,
                        linetype = 2, colour = "grey40") +
    ggplot2::geom_hline(yintercept = spi_cut,
                        linetype = 2, colour = "grey40") +
    ggplot2::geom_point(ggplot2::aes(colour = .data$concordance),
                        alpha = 0.7) +
    # corner labels in a deep tone of each quadrant's colour
    ggplot2::annotate("text", x = 0, y = -Inf, label = "True shortfall",
                      hjust = -0.08, vjust = -1, size = 3.2, fontface = "bold",
                      alpha = 0.85, colour = label_pal[["True shortfall"]]) +
    ggplot2::annotate("text", x = Inf, y = -Inf, label = "False reassurance",
                      hjust = 1.08, vjust = -1, size = 3.2, fontface = "bold",
                      alpha = 0.85, colour = label_pal[["False reassurance"]]) +
    ggplot2::annotate("text", x = 0, y = Inf, label = "False alarm",
                      hjust = -0.08, vjust = 1.9, size = 3.2, fontface = "bold",
                      alpha = 0.85, colour = label_pal[["False alarm"]]) +
    ggplot2::annotate("text", x = Inf, y = Inf, label = "Both adequate",
                      hjust = 1.08, vjust = 1.9, size = 3.2, fontface = "bold",
                      alpha = 0.85, colour = label_pal[["Both adequate"]]) +
    ggplot2::scale_colour_manual(
      values = pal, drop = TRUE, labels = cell_labels,
      name = "Concordance (% of total)"
    ) +
    ggplot2::scale_fill_manual(values = pal, guide = "none") +
    ggplot2::scale_x_continuous(
      transform = "log1p",
      breaks = c(0, 1, 3, 10, 30, 100, 300, 1000, 3000),
      labels = c("0", "1", "3", "10", "30", "100", "300", "1,000", "3,000")
    ) +
    ggplot2::labs(
      x = sprintf("NPAFP rate (per %s person-years, log1p)",
                  format(x$thresholds$multiplier, big.mark = ",")),
      y = "SPI (posterior median)"
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        title.position = "top", title.hjust = 0.5, nrow = 1,
        override.aes = list(size = 2.25, alpha = 1)
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box.spacing = grid::unit(14, "pt"),
      legend.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 12)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 10))
    )
}


# ============================================================================
# Concordance maps  (three-panel choropleth)

#' Three-panel concordance map (NPAFP | SPI | disagreement)
#'
#' @description
#' Reproduces the paper's three-panel choropleth for a single year:
#' \itemize{
#'   \item **Panel A** -- conventional NPAFP rate per 100,000 under-15
#'     person-years, categorised on the canonical POLIS breaks
#'     (`<1, 1-2, 2-3, 3-6, 6-12, 12-24, >=24`). Values below the WHO
#'     conventional target are drawn on the red end of the palette.
#'   \item **Panel B** -- posterior median SPI, categorised on the
#'     paper's operational breaks (`<0.4, 0.4-0.6, 0.6-0.8, 0.8-1.0,
#'     1.0-1.5, 1.5-2.0, >=2.0`). Values below the SPI adequacy cut are
#'     red.
#'   \item **Panel C** -- where the two indicators disagree. LGAs are
#'     coloured by the four concordance cells (`Both adequate`,
#'     `True shortfall`, `False reassurance`, `False alarm`). This is
#'     where the SPI-vs-conventional refinement lives visually.
#' }
#'
#' The panels are composed via `patchwork` so the returned object plots
#' as a single figure but each panel is a full `ggplot` you can extract
#' or restyle.
#'
#' @param concordance A `blindspot_concordance` object. `$district_year`
#'   must contain `year`, `npafp_rate`, `spi_median`, and `concordance`.
#' @param boundaries `sf` object with the district id column matching
#'   `concordance$id_col` and a POLYGON geometry column.
#' @param year Integer. Which year to display. Defaults to the maximum
#'   year in the concordance table.
#' @param spi_threshold,npafp_target Numeric cuts used in the panel
#'   subtitles. If NULL (default) they are pulled from
#'   `concordance$thresholds`.
#' @param npafp_breaks,spi_breaks Numeric break vectors used for the
#'   categorical fills. Defaults match the paper.
#' @param id_col Character. Name of the district id column in
#'   `boundaries`. Inferred from `concordance$id_col` if NULL.
#' @param titles Optional character vector of length 3 to override the
#'   default panel titles.
#' @param year_label Character. What to call the displayed year in the
#'   panel titles. Defaults to the plain year (e.g. `"2023"`). Pass
#'   `sprintf("year T-1: %d", year)` to restore the paper's
#'   validation-narrative wording where the map sits beside the
#'   year-T detection panel.
#' @param provinces Logical. Overlay dissolved adm1 (province) outlines (dark
#'   grey) on the light-grey district choropleths? Default `TRUE`. Assumes the
#'   adm2 layer is a clean coverage (shared edges); on an imperfectly
#'   edge-matched layer the dissolve can leave sliver artefacts, so set `FALSE`.
#'
#' @return A `patchwork` object plotting the three panels side by side.
#'
#' @seealso [bs_concordance()]
#'
#' @export
#' @examples
#' \dontrun{
#' spi_dy <- bs_spi(fit_bare, level = "district_year")
#' conc <- bs_concordance(
#'   spi        = spi_dy,
#'   cases      = synth_surveillance$cases,
#'   population = synth_surveillance$population,
#'   boundaries = synth_surveillance$boundaries
#' )
#' bs_concordance_maps(conc, synth_surveillance$boundaries, year = 2023)
#' }
bs_concordance_maps <- function(
  concordance,
  boundaries,
  year = NULL,
  spi_threshold = NULL,
  npafp_target = NULL,
  npafp_breaks = c(-Inf, 1, 2, 3, 6, 12, 24, Inf),
  spi_breaks = c(-Inf, 0.4, 0.6, 0.8, 1.0, 1.5, 2.0, Inf),
  id_col = NULL,
  titles = NULL,
  year_label = NULL,
  provinces = TRUE
) {
  .check_pkg(c("ggplot2", "patchwork", "sf", "dplyr"),
             reason = "to draw the three-panel concordance map")
  stopifnot(inherits(concordance, "blindspot_concordance"),
            inherits(boundaries, "sf"))

  id_col        <- id_col        %||% concordance$id_col
  spi_threshold <- spi_threshold %||% concordance$thresholds$spi
  npafp_target  <- npafp_target  %||% concordance$thresholds$npafp

  dy <- concordance$district_year
  if (is.null(year)) year <- max(dy$year, na.rm = TRUE)
  yr <- as.integer(year)

  slice <- dy[dy$year == yr, ]
  if (nrow(slice) == 0L) {
    cli::cli_abort("no district-year rows for {.arg year} = {.val {yr}}")
  }

  # Paper's canonical palettes and labels. Reds/pinks tag "below threshold"
  # for both indicators; the concordance panel picks up amber for the SPI-only
  # catches and green for the NPAFP-only flags. All hex values chosen to
  # visually match the paper's Figure 2.
  npafp_labels <- c("<1", "1-2", "2-3", "3-6", "6-12", "12-24", ">=24")
  pal_a <- c(
    "<1" = "#B71C1C",
    "1-2" = "#E57373",
    "2-3" = "#FFCDD2",
    "3-6" = "#E3F2FD",
    "6-12" = "#90CAF9",
    "12-24" = "#1E88E5",
    ">=24" = "#0D47A1"
  )
  spi_labels <- c("<0.4", "0.4-0.6", "0.6-0.8", "0.8-1.0",
                  "1.0-1.5", "1.5-2.0", ">=2.0")
  pal_b <- c(
    "<0.4" = "#B71C1C",
    "0.4-0.6" = "#E53935",
    "0.6-0.8" = "#F57C00",
    "0.8-1.0" = "#E3F2FD",
    "1.0-1.5" = "#90CAF9",
    "1.5-2.0" = "#1E88E5",
    ">=2.0" = "#0D47A1"
  )
  # Panel C: paper's canonical concordance labels with the parenthetical
  # (conventional X, SPI Y) annotations spelled out on the legend chips.
  c_labels <- c(
    "Both adequate (both pass)" = "Both adequate",
    "True shortfall (both flag)" = "True shortfall",
    "False reassurance (conventional pass, SPI flag)" = "False reassurance",
    "False alarm (conventional flag, SPI pass)" = "False alarm"
  )
  pal_c <- c(
    "Both adequate (both pass)" = "#EEEEEE",
    "True shortfall (both flag)" = "#9E9E9E",
    "False reassurance (conventional pass, SPI flag)" = "#F9A825",
    "False alarm (conventional flag, SPI pass)" = "#2E7D32"
  )

  bnd_slice <- boundaries |>
    dplyr::inner_join(
      slice[, c(id_col, "npafp_rate", "spi_median", "concordance")],
      by = id_col
    )

  # adm1 (province) outline dissolved from the adm2 layer, drawn on top of each
  # panel so the provincial structure reads through the district choropleth
  # (matches the sntutils facetted-map convention).
  adm1_layer <- NULL
  if (isTRUE(provinces) && "adm1_name" %in% names(boundaries)) {
    adm1_outline <- boundaries |>
      dplyr::group_by(.data$adm1_name) |>
      dplyr::summarise(.groups = "drop")
    adm1_layer <- ggplot2::geom_sf(
      data = adm1_outline, fill = NA, colour = "grey20", linewidth = 0.3,
      inherit.aes = FALSE
    )
  }

  # ---- Panel titles + subtitles ----
  # Label by the displayed year. The "year T-1 / before N detections" framing
  # is only meaningful when the detections are on the figure (or it sits beside
  # the year-T detection panel), which this standalone map does not draw -- pass
  # `year_label` to restore that wording where the narrative supports it.
  yr_lbl <- year_label %||% sprintf("%d", yr)
  ttl_a <- (titles %||% NULL)[1] %||%
    sprintf("A. Conventional NPAFP rate (%s)", yr_lbl)
  ttl_b <- (titles %||% NULL)[2] %||%
    sprintf("B. Posterior median SPI (%s)", yr_lbl)
  ttl_c <- (titles %||% NULL)[3] %||%
    sprintf("C. Where the two indicators disagree (%s)", yr_lbl)

  sub_a <- sprintf(
    "Threshold = %g per 100 000 children under 15.", npafp_target
  )
  sub_b <- sprintf(
    "Threshold %g (20%% shortfall vs context-conditional expectation).",
    spi_threshold
  )
  sub_c <- paste0(
    "Amber: conventional pass, SPI flag (False reassurance). Green:\n",
    "conventional flag, SPI pass (False alarm). Concordant cells\nin grey."
  )

  # ---- Panel A fill category ----
  bnd_slice$npafp_cat <- factor(
    cut(bnd_slice$npafp_rate, breaks = npafp_breaks,
        labels = npafp_labels, right = FALSE, include.lowest = TRUE),
    levels = npafp_labels
  )

  base_theme <- ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = 12, margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 9, colour = "grey30", margin = ggplot2::margin(t = 0, b = 18)
      ),
      plot.margin = ggplot2::margin(6, 10, 6, 10),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(size = 8, face = "bold"),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.width = grid::unit(1.1, "lines"),
      legend.key.height = grid::unit(0.5, "lines"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.spacing.y = grid::unit(0.1, "lines")
    )

  p_a <- ggplot2::ggplot(bnd_slice) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$npafp_cat), colour = "grey82",
                     linewidth = 0.1) +
    adm1_layer +
    ggplot2::scale_fill_manual(
      values = pal_a, drop = FALSE, na.value = "grey85",
      name = sprintf(
        "NPAFP per 100 000 u15 (%d); red = below conventional threshold", yr
      )
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = 1, title.position = "top", title.hjust = 0,
      label.position = "bottom", keywidth = grid::unit(1.4, "lines")
    )) +
    ggplot2::labs(title = ttl_a, subtitle = sub_a) +
    base_theme

  # ---- Panel B fill category ----
  bnd_slice$spi_cat <- factor(
    cut(bnd_slice$spi_median, breaks = spi_breaks,
        labels = spi_labels, right = FALSE, include.lowest = TRUE),
    levels = spi_labels
  )
  p_b <- ggplot2::ggplot(bnd_slice) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$spi_cat), colour = "grey82",
                     linewidth = 0.1) +
    adm1_layer +
    ggplot2::scale_fill_manual(
      values = pal_b, drop = FALSE, na.value = "grey85",
      name = sprintf(
        "Posterior median SPI (%d); red = below adequacy of %g",
        yr, spi_threshold
      )
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = 1, title.position = "top", title.hjust = 0,
      label.position = "bottom", keywidth = grid::unit(1.4, "lines")
    )) +
    ggplot2::labs(title = ttl_b, subtitle = sub_b) +
    base_theme

  # ---- Panel C concordance category (with parenthetical legend chips) ----
  legend_lookup <- stats::setNames(names(c_labels), unname(c_labels))
  bnd_slice$concordance_legend <- factor(
    legend_lookup[as.character(bnd_slice$concordance)],
    levels = names(c_labels)
  )
  p_c <- ggplot2::ggplot(bnd_slice) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$concordance_legend),
                     colour = "grey82", linewidth = 0.1) +
    adm1_layer +
    ggplot2::scale_fill_manual(
      values = pal_c, drop = TRUE, na.value = "grey85",
      name = "Per-LGA agreement (conventional NPAFP x SPI)"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = 2, title.position = "top", title.hjust = 0, byrow = TRUE
    )) +
    ggplot2::labs(title = ttl_c, subtitle = sub_c) +
    base_theme

  # extra right margin gives the composed figure -- and panel C's legend --
  # breathing space at the right edge.
  patchwork::wrap_plots(p_a, p_b, p_c, ncol = 3) +
    patchwork::plot_layout(widths = c(1, 1, 1)) +
    patchwork::plot_annotation(
      theme = ggplot2::theme(plot.margin = ggplot2::margin(6, 22, 6, 6))
    )
}

# nicely-named intervals for use as factor levels + palette keys
.interval_labels <- function(breaks) {
  n <- length(breaks) - 1L
  labels <- character(n)
  for (i in seq_len(n)) {
    lo <- breaks[i]
    hi <- breaks[i + 1L]
    left  <- if (is.infinite(lo) && lo < 0) "-Inf" else format(lo, trim = TRUE)
    right <- if (is.infinite(hi) && hi > 0) "Inf"  else format(hi, trim = TRUE)
    labels[i] <- if (i == n) sprintf("[%s, %s]", left, right)
    else sprintf("[%s,%s)", left, right)
  }
  labels
}

# pick palette entries for the interval labels we actually produced. If keys
# differ (e.g. minor formatting), fall back to positional assignment.
.align_palette <- function(pal, labels) {
  if (all(labels %in% names(pal))) return(pal[labels])
  if (length(pal) == length(labels)) {
    stats::setNames(unname(pal), labels)
  } else {
    grDevices::colorRampPalette(
      c("#B71C1C", "#EF9A9A", "#BBDEFB", "#1E88E5", "#0D47A1")
    )(length(labels)) |>
      stats::setNames(labels)
  }
}
