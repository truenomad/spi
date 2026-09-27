# the four SPI and NPAFP categories, in display order. Every table, plot and
# metric reads the labels from here.
# @noRd
.npafp_categories <- c(
  neither = "Neither below",
  spi_only = "SPI below threshold only",
  npafp_only = "NPAFP below target only",
  both = "Both below"
)

#' Compare SPI with the NPAFP target
#'
#' @description
#' Classifies each district-year by whether its SPI is below the SPI threshold
#' and whether its NPAFP rate is below the NPAFP target. The default SPI
#' threshold is 1. The four categories are:
#' \itemize{
#'   \item **Neither below** -- NPAFP >= target AND SPI >= threshold.
#'   \item **Both below** -- NPAFP < target AND SPI < threshold.
#'   \item **SPI below threshold only** -- NPAFP >= target BUT SPI < threshold.
#'   \item **NPAFP below target only** -- NPAFP < target BUT SPI >= threshold.
#' }
#' The two measures answer different questions, so neither is a reference
#' standard for the other. The categories describe where they agree or
#' differ; they do not establish whether surveillance is adequate.
#'
#' Agreement is summarised as raw percent agreement and Cohen's kappa, plus a
#' McNemar test of whether one measure places more district-years below its
#' threshold than the other.
#'
#' @param spi Object of class `spi_index` at `district_year` level.
#' @param cases Optional tibble with the district id column and `count`
#'   (integer). Pass to override the counts stored in `spi$data`. Default:
#'   NULL (use `spi$data`).
#' @param population Tibble with the district id column, `year` (integer),
#'   and the population denominator column (see `pop_col`). Required.
#' @param year_end_month Integer 1 to 12. Month in which the reading year
#'   closes, matching the `year_end_month` the SPI was computed with. The
#'   conventional rate is grouped on the same rolling year, so the two sides of
#'   the comparison cover the same months. Default: 12 (calendar years).
#' @param spi_threshold Numeric. A posterior median below this value counts
#'   as below the threshold, subject to `spi_rule`. The reference is 1: with
#'   national centring, district and national observed-to-expected ratios are
#'   equal. Default: 1.
#' @param spi_rule Character. `"median"` uses `spi_median` alone.
#'   `"interval"` additionally requires the 90% upper bound (`spi_q95`) to
#'   be below 1. This requires stronger evidence of a relative reporting
#'   shortfall, not evidence of missed cases. The paper uses this rule for a
#'   sensitivity analysis. Default: `"median"`.
#' @param npafp_target Numeric. NPAFP reporting-rate target per
#'   `npafp_multiplier` person-years. Set this to the target used by the
#'   programme. Default: 3, as in the accompanying study.
#' @param npafp_multiplier Numeric. Denominator scaling for the NPAFP rate.
#'   Default: 100000 (per 100,000 under-15 person-years).
#' @param strata Character vector of column names to stratify the
#'   comparison by. Any column present in the district-year summary is valid:
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
#' @return An object of class `spi_compare_npafp`. A list with:
#' \describe{
#'   \item{district_year}{Tibble with per-district-year classification:
#'     `{id_col}`, `year`, `count_annual`, `pop_u15`, `npafp_rate`,
#'     `npafp_adequate` (logical: rate target met), `observed` (reported count
#'     from `spi$summary`, when present), `expected_total` (modelled
#'     expected count, when present), `spi_median`, `spi_q05`, `spi_q95`,
#'     `spi_below_threshold` (logical), `spi_pass` (logical,
#'     `!spi_below_threshold`), `category` (factor: Neither below / SPI below
#'     threshold only / NPAFP below target only / Both below).}
#'   \item{crosstab}{2x2 table of counts and row/column percentages.}
#'   \item{metrics}{Pooled scalar metrics: `n`, `pct_agreement`,
#'     `cohens_kappa`, `mcnemar_p`, plus per-cell counts.}
#'   \item{by_stratum}{Tibble of per-stratum metrics when `strata` is
#'     non-NULL; else NULL.}
#'   \item{thresholds}{Named list echoing the SPI cut, the `spi_rule`, and
#'     the NPAFP cuts used.}
#'   \item{id_col}{The id column name.}
#'   \item{call}{Matched call.}
#' }
#'
#' @seealso [spi_index()], [spi_expected()]
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' \dontrun{
#' spi_dy <- spi_index(fit_bare, level = "district_year")
#'
#' comparison <- spi_compare_npafp(
#'   spi = spi_dy,
#'   population = synth_surveillance$population,
#'   spi_threshold = 1,
#'   npafp_target = 3,
#'   strata = c("year", "adm1_name"),
#'   boundaries = synth_surveillance$boundaries
#' )
#'
#' print(comparison)
#' summary(comparison)
#' plot(comparison)
#' }
spi_compare_npafp <- function(
  spi,
  cases = NULL,
  population,
  year_end_month = 12L,
  spi_threshold = 1,
  spi_rule = c("median", "interval"),
  npafp_target = 3,
  npafp_multiplier = 100000L,
  strata = NULL,
  id_col = NULL,
  pop_col = "pop_u15",
  boundaries = NULL,
  verbose = TRUE
) {
  .check_pkg(c("dplyr", "tibble", "cli"),
             reason = "to compare SPI with the NPAFP target")
  spi_rule <- match.arg(spi_rule)

  stopifnot(inherits(spi, "spi_index"))

  id_col <- id_col %||% spi$id_col %||% "district_id"
  if (!id_col %in% names(spi$summary)) {
    cli::cli_abort(
      "{.arg spi$summary} lacks id column {.val {id_col}}; set {.arg id_col}."
    )
  }
  if (!"year" %in% names(spi$summary)) {
    cli::cli_abort(
      "{.fn spi_compare_npafp} expects a district-year SPI; got level \\
       {.val {spi$level}}. Re-run {.fn spi_index} with \\
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
  # The conventional rate must be grouped on the SAME years as the index. When
  # the SPI is read on a rolling year, grouping cases on calendar years counts
  # only part of the window against a whole-year denominator, which drives the
  # conventional rate down and turns the comparison into nonsense. Derive the
  # year from the month whenever a rolling window is in force.
  year_end_month <- as.integer(year_end_month)
  if (length(year_end_month) != 1L || is.na(year_end_month) ||
        year_end_month < 1L || year_end_month > 12L) {
    cli::cli_abort("{.arg year_end_month} must be a single month, 1 to 12.")
  }
  cases_annual <- cases_src |>
    dplyr::mutate(
      year = if ("year" %in% names(cases_src) && year_end_month == 12L) {
        as.integer(cases_src$year)
      } else if (year_end_month == 12L) {
        as.integer(format(cases_src$month, "%Y"))
      } else {
        as.integer(lubridate::year(lubridate::add_with_rollback(
          cases_src$month, months(12L - year_end_month)
        )))
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
      spi_below_threshold = .data$spi_median < spi_threshold &
        (spi_rule == "median" | .data$spi_q95 < 1),
      spi_pass = !.data$spi_below_threshold,
      category = factor(dplyr::case_when(
        npafp_adequate & !spi_below_threshold ~ .npafp_categories[["neither"]],
        npafp_adequate & spi_below_threshold ~ .npafp_categories[["spi_only"]],
        !npafp_adequate & !spi_below_threshold ~
          .npafp_categories[["npafp_only"]],
        !npafp_adequate & spi_below_threshold ~ .npafp_categories[["both"]]
      ), levels = unname(.npafp_categories))
    )

  # optional join with boundaries so `strata` can reference adm1/adm0 labels
  if (!is.null(boundaries)) {
    bnd_flat <- boundaries
    if (inherits(bnd_flat, "sf")) bnd_flat <- sf::st_drop_geometry(bnd_flat)
    if (id_col %in% names(bnd_flat)) {
      # avoid duplicating columns already present in dy
      keep_cols <- setdiff(names(bnd_flat), setdiff(names(dy), id_col))
      dy <- dplyr::left_join(dy, bnd_flat[, keep_cols], by = id_col)
      # surface the admin names next to the id, before the metric columns
      name_cols <- intersect(c("adm0_name", "adm1_name", "adm2_name"),
                             names(dy))
      if (length(name_cols)) {
        dy <- dplyr::relocate(dy, dplyr::all_of(name_cols),
                              .before = dplyr::all_of(id_col))
      }
    }
  }

  # ---- 4. Pooled metrics + crosstab ----
  crosstab <- .npafp_crosstab(dy)
  metrics <- .npafp_metrics(dy)

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
      dplyr::group_modify(~ .npafp_metrics(.x)) |>
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
      thresholds = list(spi = spi_threshold, rule = spi_rule,
                        npafp = npafp_target, multiplier = npafp_multiplier),
      id_col = id_col,
      call = match.call()
    ),
    class = "spi_compare_npafp"
  )
}

# ---------------------------------------------------------------------------
# internal helpers
# ---------------------------------------------------------------------------

# 2x2 crosstab with row/column percentages
.npafp_crosstab <- function(dy) {
  tab <- table(
    NPAFP = factor(
      ifelse(dy$npafp_adequate, "target met", "below target"),
      levels = c("target met", "below target")
    ),
    SPI = factor(
      ifelse(dy$spi_pass, "at or above threshold", "below threshold"),
      levels = c("at or above threshold", "below threshold")
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
.npafp_metrics <- function(dy) {
  n <- nrow(dy)
  counts <- vapply(
    .npafp_categories,
    function(cell) sum(dy$category == cell), integer(1)
  )

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
    n_neither = counts[["neither"]],
    n_spi_only = counts[["spi_only"]],
    n_npafp_only = counts[["npafp_only"]],
    n_both = counts[["both"]]
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
  # off-diagonals of the SPI and NPAFP table
  b <- sum(dy$category == .npafp_categories[["spi_only"]])
  c <- sum(dy$category == .npafp_categories[["npafp_only"]])
  if (b + c < 1L) return(NA_real_)
  # exact binomial test on off-diagonals (McNemar exact)
  stats::binom.test(b, b + c, p = 0.5)$p.value
}

# ---------------------------------------------------------------------------
# S3 methods
# ---------------------------------------------------------------------------

#' @export
print.spi_compare_npafp <- function(x, ...) {
  cli::cli_h1("SPI and NPAFP classification")
  cli::cli_inform(c(
    "SPI cut: {.val {x$thresholds$spi}} ({x$thresholds$rule} rule) \\
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
  cli::cli_h2("Category counts")
  counts <- c(x$metrics$n_neither, x$metrics$n_spi_only,
              x$metrics$n_npafp_only, x$metrics$n_both)
  cell_tbl <- tibble::tibble(
    category = unname(.npafp_categories),
    n = counts,
    pct = round(100 * counts / x$metrics$n, 1)
  )
  print(cell_tbl)
  if (!is.null(x$by_stratum)) {
    cli::cli_h2("By stratum")
    print(x$by_stratum, n = Inf)
  }
  invisible(x)
}

#' @export
summary.spi_compare_npafp <- function(object, ...) {
  print(object, ...)
  cli::cli_h2("2x2 crosstab (counts)")
  print(object$crosstab$counts)
  cli::cli_h2("Row percentages (within NPAFP class)")
  print(object$crosstab$row_pct)
  invisible(object)
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.spi_compare_npafp <- function(x, ...) {
  x$district_year
}

#' @export
plot.spi_compare_npafp <- function(x, ...) {
  .check_pkg(c("ggplot2"), reason = "to plot the SPI and NPAFP comparison")
  dy <- x$district_year
  spi_cut <- x$thresholds$spi
  npafp_target <- x$thresholds$npafp
  cats <- .npafp_categories
  pal <- stats::setNames(
    c("#2E7D32", "#C62828", "#F9A825", "#1565C0"),
    cats[c("neither", "both", "spi_only", "npafp_only")]
  )
  # Legend labels carry each cell's share of all district-years (from
  # x$metrics).
  m <- x$metrics
  pct <- stats::setNames(
    c(m$n_neither, m$n_both, m$n_spi_only, m$n_npafp_only),
    names(pal)
  ) / m$n * 100
  cell_labels <- stats::setNames(
    sprintf("%s (%.1f%%)", names(pct), pct), names(pct)
  )
  # Darker shades for the in-plot corner labels so each quadrant name reads
  # clearly (a deep tone of its cell colour) over the faint quadrant tint.
  label_pal <- stats::setNames(
    c("#1B5E20", "#8E1B1B", "#B8860B", "#0D47A1"),
    names(pal)
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
      cats[c("both", "spi_only", "npafp_only", "neither")],
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
    ggplot2::geom_point(ggplot2::aes(colour = .data$category),
                        alpha = 0.7) +
    # corner labels in a deep tone of each quadrant's colour. NPAFP below target
    # sits left (low rate), SPI below threshold sits low (below the cut).
    ggplot2::annotate("text", x = 0, y = -Inf, label = cats[["both"]],
                      hjust = -0.08, vjust = -1, size = 3.2, fontface = "bold",
                      alpha = 0.85, colour = label_pal[[cats[["both"]]]]) +
    ggplot2::annotate("text", x = Inf, y = -Inf, label = cats[["spi_only"]],
                      hjust = 1.08, vjust = -1, size = 3.2, fontface = "bold",
                      alpha = 0.85, colour = label_pal[[cats[["spi_only"]]]]) +
    ggplot2::annotate("text", x = 0, y = Inf, label = cats[["npafp_only"]],
                      hjust = -0.08, vjust = 1.9, size = 3.2, fontface = "bold",
                      alpha = 0.85,
                      colour = label_pal[[cats[["npafp_only"]]]]) +
    ggplot2::annotate("text", x = Inf, y = Inf, label = cats[["neither"]],
                      hjust = 1.08, vjust = 1.9, size = 3.2, fontface = "bold",
                      alpha = 0.85, colour = label_pal[[cats[["neither"]]]]) +
    ggplot2::scale_colour_manual(
      values = pal, drop = TRUE, labels = cell_labels,
      name = "Category (% of district-years)"
    ) +
    ggplot2::scale_fill_manual(values = pal, guide = "none") +
    ggplot2::scale_x_continuous(
      transform = "log1p",
      breaks = c(0, 1, 3, 10, 30, 100, 300, 1000, 3000),
      labels = c("0", "1", "3", "10", "30", "100", "300", "1,000", "3,000")
    ) +
    ggplot2::labs(
      title = "SPI and NPAFP classification",
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
# SPI and NPAFP maps (three-panel choropleth)

# A discrete fill level with no rows draws a blank legend key, because geom_sf
# takes its key glyph from the data. Padding the frame with one empty geometry
# per missing level gives every level a row, so its key renders in its own
# colour while nothing is added to the map.
# @noRd
.pad_fill_levels <- function(d, col) {
  lv <- levels(d[[col]])
  miss <- setdiff(lv, unique(as.character(d[[col]])))
  if (length(miss) == 0L) return(d)
  pad <- d[rep(1L, length(miss)), , drop = FALSE]
  pad[[col]] <- factor(miss, levels = lv)
  sf::st_geometry(pad) <- sf::st_sfc(
    rep(list(sf::st_polygon()), length(miss)), crs = sf::st_crs(d)
  )
  rbind(d, pad)
}

#' Map the NPAFP rate, SPI, and their classification
#'
#' @description
#' Maps the NPAFP rate, SPI, and their agreement for one year:
#' \itemize{
#'   \item **Panel A** -- conventional NPAFP rate per 100,000 under-15
#'     person-years, grouped into the reporting bands
#'     (`<1, 1-2, 2-3, 3-6, 6-12, 12-24, >=24`). Values below the
#'     selected NPAFP target are drawn on the red end of the palette.
#'   \item **Panel B** -- posterior median SPI, categorised on breaks
#'     approximately symmetric on the log scale around the reference of 1 (default
#'     `-Inf, 0.5, 0.75, 1, 1.33, 2, Inf`). Bins at or below the SPI
#'     threshold are warm; the rest are cool.
#'   \item **Panel C** -- the SPI and NPAFP category of each district
#'     (`Neither below`, `Both below`, `SPI below threshold only`,
#'     `NPAFP below target only`).
#' }
#'
#' The panels are composed via `patchwork` so the returned object plots
#' as a single figure but each panel is a full `ggplot` you can extract
#' or restyle.
#'
#' The maps keep their geographic aspect ratio. Choose image dimensions that
#' match the boundaries to limit empty space. For a wide region, try a 3:1
#' width-to-height ratio, such as `ggsave(width = 15, height = 5)`. A region
#' that is taller than it is wide needs a taller image.
#'
#' @param comparison A [spi_compare_npafp()] result. `$district_year`
#'   must contain `year`, `npafp_rate`, `spi_median`, and `category`.
#' @param boundaries `sf` object with the district id column matching
#'   `comparison$id_col` and a POLYGON geometry column.
#' @param year Integer. Which year to display. Defaults to the maximum
#'   year in the comparison table.
#' @param spi_threshold,npafp_target Numeric cuts used in the panel
#'   subtitles. If NULL (default) they are pulled from
#'   `comparison$thresholds`.
#' @param npafp_breaks,spi_breaks Numeric break vectors used for the
#'   categorical fills. Defaults match the paper.
#' @param id_col Character. Name of the district id column in
#'   `boundaries`. Inferred from `comparison$id_col` if NULL.
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
#' @seealso [spi_compare_npafp()]
#'
#' @export
#' @examples
#' \dontrun{
#' spi_dy <- spi_index(fit_bare, level = "district_year")
#' comparison <- spi_compare_npafp(
#'   spi = spi_dy,
#'   cases = synth_surveillance$cases,
#'   population = synth_surveillance$population,
#'   boundaries = synth_surveillance$boundaries
#' )
#' spi_compare_npafp_maps(
#'   comparison, synth_surveillance$boundaries, year = 2023
#' )
#' }
spi_compare_npafp_maps <- function(
  comparison,
  boundaries,
  year = NULL,
  spi_threshold = NULL,
  npafp_target = NULL,
  npafp_breaks = c(-Inf, 1, 2, 3, 6, 12, 24, Inf),
  spi_breaks = c(-Inf, 0.5, 0.75, 1, 1.33, 2, Inf),
  id_col = NULL,
  titles = NULL,
  year_label = NULL,
  provinces = TRUE
) {
  .check_pkg(c("ggplot2", "patchwork", "sf", "dplyr"),
             reason = "to draw the three-panel SPI and NPAFP map")
  stopifnot(inherits(comparison, "spi_compare_npafp"),
            inherits(boundaries, "sf"))

  id_col <- id_col %||% comparison$id_col
  spi_threshold <- spi_threshold %||% comparison$thresholds$spi
  npafp_target <- npafp_target %||% comparison$thresholds$npafp

  dy <- comparison$district_year
  if (is.null(year)) year <- max(dy$year, na.rm = TRUE)
  yr <- as.integer(year)

  slice <- dy[dy$year == yr, ]
  if (nrow(slice) == 0L) {
    cli::cli_abort("no district-year rows for {.arg year} = {.val {yr}}")
  }

  # Paper's canonical palettes and labels. Reds/pinks mark "below threshold"
  # for both indicators; the category panel uses amber for SPI below threshold
  # only and green for NPAFP below target only, matching the paper's Figure 2.
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
  # spi_labels are built from spi_breaks rather than hard-coded, so a custom
  # spi_breaks stays consistent with its own fill legend. Bins whose upper
  # edge sits at or below spi_threshold are warm (below threshold); the rest are
  # cool. Both ramps are interpolated from the same endpoint colours as the
  # original fixed palette, so a custom spi_breaks still reads on the same
  # warm/cool scale.
  spi_labels <- .interval_labels(spi_breaks)
  is_warm <- spi_breaks[-1] <= spi_threshold
  pal_b <- stats::setNames(character(length(spi_labels)), spi_labels)
  if (any(is_warm)) {
    pal_b[is_warm] <- grDevices::colorRampPalette(
      c("#B71C1C", "#E53935", "#F57C00")
    )(sum(is_warm))
  }
  if (any(!is_warm)) {
    pal_b[!is_warm] <- grDevices::colorRampPalette(
      c("#90CAF9", "#1E88E5", "#0D47A1")
    )(sum(!is_warm))
  }
  # Panel C: the category labels describe themselves, so the legend uses them
  # as they are
  cats <- .npafp_categories
  pal_c <- stats::setNames(
    c("#EEEEEE", "#9E9E9E", "#F9A825", "#2E7D32"),
    cats[c("neither", "both", "spi_only", "npafp_only")]
  )

  bnd_slice <- boundaries |>
    dplyr::inner_join(
      slice[, c(id_col, "npafp_rate", "spi_median", "category")],
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
    sprintf("C. SPI and NPAFP classification (%s)", yr_lbl)

  rule <- comparison$thresholds$rule %||% "median"

  # Wrap subtitles/legend titles to a fixed width: ggplot draws these
  # left-aligned and never wraps them, so a long single line spills out of a
  # narrow panel into its neighbour (the composed three-panel figure).
  sub_a <- .wrap_lines(sprintf(
    "NPAFP target = %g per 100 000 children under 15.", npafp_target
  ))
  sub_b <- .wrap_lines(paste0(
    sprintf("SPI threshold = %g.", spi_threshold),
    if (identical(rule, "interval")) {
      " Below the threshold also needs the 90% upper bound below 1."
    } else {
      ""
    }
  ))
  sub_c <- .wrap_lines(paste0(
    "Amber: SPI below threshold only. ",
    "Green: NPAFP below target only. ",
    "Grey: both below or neither below."
  ))

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
        size = 9, colour = "grey30", margin = ggplot2::margin(t = 0, b = 8)
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

  p_a <- ggplot2::ggplot(.pad_fill_levels(bnd_slice, "npafp_cat")) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$npafp_cat), colour = "grey82",
                     linewidth = 0.1) +
    adm1_layer +
    ggplot2::scale_fill_manual(
      values = pal_a, drop = FALSE, limits = names(pal_a),
      na.value = "grey85",
      name = .wrap_lines(sprintf(
        "NPAFP per 100 000 u15 (%d); red = below the NPAFP target", yr
      ))
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
  p_b <- ggplot2::ggplot(.pad_fill_levels(bnd_slice, "spi_cat")) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$spi_cat), colour = "grey82",
                     linewidth = 0.1) +
    adm1_layer +
    ggplot2::scale_fill_manual(
      values = pal_b, drop = FALSE, limits = names(pal_b),
      na.value = "grey85",
      name = .wrap_lines(sprintf(
        "Posterior median SPI (%d); red = below %g",
        yr, spi_threshold
      ))
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = 1, title.position = "top", title.hjust = 0,
      label.position = "bottom", keywidth = grid::unit(1.4, "lines")
    )) +
    ggplot2::labs(title = ttl_b, subtitle = sub_b) +
    base_theme

  # ---- Panel C SPI and NPAFP category ----
  bnd_slice$category <- factor(
    as.character(bnd_slice$category),
    levels = names(pal_c)
  )
  p_c <- ggplot2::ggplot(.pad_fill_levels(bnd_slice, "category")) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$category),
                     colour = "grey82", linewidth = 0.1) +
    adm1_layer +
    ggplot2::scale_fill_manual(
      values = pal_c, drop = FALSE, limits = names(pal_c),
      na.value = "grey85",
      name = .wrap_lines("District category (NPAFP target and SPI threshold)")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      # single column: the four cell labels are long, so a multi-column
      # layout spills past the right edge of the (rightmost) panel
      ncol = 1, title.position = "top", title.hjust = 0
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

# wrap a one-line label to <= `width` characters per line so long panel
# subtitles and legend titles do not overflow a narrow map panel into the
# neighbouring one. Operates on the finished string (post-sprintf).
.wrap_lines <- function(x, width = 36) {
  vapply(
    x,
    function(s) paste(strwrap(s, width = width), collapse = "\n"),
    character(1), USE.NAMES = FALSE
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
