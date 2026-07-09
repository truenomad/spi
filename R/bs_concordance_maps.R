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
#'   panel titles. Defaults to `sprintf("year T-1: %d", year)` -- the
#'   paper's convention.
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
  year          = NULL,
  spi_threshold = NULL,
  npafp_target  = NULL,
  npafp_breaks  = c(-Inf, 1, 2, 3, 6, 12, 24, Inf),
  spi_breaks    = c(-Inf, 0.4, 0.6, 0.8, 1.0, 1.5, 2.0, Inf),
  id_col        = NULL,
  titles        = NULL,
  year_label    = NULL
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
    "<1"    = "#B71C1C",
    "1-2"   = "#E57373",
    "2-3"   = "#FFCDD2",
    "3-6"   = "#E3F2FD",
    "6-12"  = "#90CAF9",
    "12-24" = "#1E88E5",
    ">=24"  = "#0D47A1"
  )
  spi_labels <- c("<0.4", "0.4-0.6", "0.6-0.8", "0.8-1.0",
                  "1.0-1.5", "1.5-2.0", ">=2.0")
  pal_b <- c(
    "<0.4"    = "#B71C1C",
    "0.4-0.6" = "#E53935",
    "0.6-0.8" = "#F57C00",
    "0.8-1.0" = "#E3F2FD",
    "1.0-1.5" = "#90CAF9",
    "1.5-2.0" = "#1E88E5",
    ">=2.0"   = "#0D47A1"
  )
  # Panel C: paper's canonical concordance labels with the parenthetical
  # (conventional X, SPI Y) annotations spelled out on the legend chips.
  c_labels <- c(
    "Both adequate (both pass)"                       = "Both adequate",
    "True shortfall (both flag)"                      = "True shortfall",
    "False reassurance (conventional pass, SPI flag)" = "False reassurance",
    "False alarm (conventional flag, SPI pass)"       = "False alarm"
  )
  pal_c <- c(
    "Both adequate (both pass)"                       = "#EEEEEE",
    "True shortfall (both flag)"                      = "#9E9E9E",
    "False reassurance (conventional pass, SPI flag)" = "#F9A825",
    "False alarm (conventional flag, SPI pass)"       = "#2E7D32"
  )

  bnd_slice <- boundaries |>
    dplyr::inner_join(
      slice[, c(id_col, "npafp_rate", "spi_median", "concordance")],
      by = id_col
    )

  # ---- Panel titles + subtitles (paper wording) ----
  yr_lbl <- year_label %||% sprintf("year T-1: %d", yr)
  ttl_a <- (titles %||% NULL)[1] %||%
    sprintf("A. Conventional NPAFP rate (%s)", yr_lbl)
  ttl_b <- (titles %||% NULL)[2] %||%
    sprintf("B. Posterior median SPI (%s)", yr_lbl)
  ttl_c <- (titles %||% NULL)[3] %||%
    sprintf("C. Where the two indicators disagree (%s)", yr_lbl)

  sub_a <- sprintf(
    "Threshold = %g per 100 000 children under 15. Indicators\nassessed before %d detections.",
    npafp_target, yr + 1L
  )
  sub_b <- sprintf(
    "Threshold %g (20%% shortfall vs context-conditional\nexpectation). Before %d detections.",
    spi_threshold, yr + 1L
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
      plot.title        = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle     = ggplot2::element_text(size = 9,
                                                colour = "grey30",
                                                margin = ggplot2::margin(b = 6)),
      plot.margin       = ggplot2::margin(6, 8, 6, 8),
      legend.position   = "bottom",
      legend.box        = "vertical",
      legend.title      = ggplot2::element_text(size = 8, face = "bold"),
      legend.text       = ggplot2::element_text(size = 8),
      legend.key.width  = grid::unit(1.1, "lines"),
      legend.key.height = grid::unit(0.5, "lines"),
      legend.margin     = ggplot2::margin(0, 0, 0, 0),
      legend.spacing.y  = grid::unit(0.1, "lines")
    )

  p_a <- ggplot2::ggplot(bnd_slice) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$npafp_cat), colour = "white",
                     linewidth = 0.1) +
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
    ggplot2::geom_sf(ggplot2::aes(fill = .data$spi_cat), colour = "white",
                     linewidth = 0.1) +
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
  legend_lookup <- setNames(names(c_labels), unname(c_labels))
  bnd_slice$concordance_legend <- factor(
    legend_lookup[as.character(bnd_slice$concordance)],
    levels = names(c_labels)
  )
  p_c <- ggplot2::ggplot(bnd_slice) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$concordance_legend),
                     colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(
      values = pal_c, drop = FALSE, na.value = "grey85",
      name = "Per-LGA agreement (conventional NPAFP x SPI)"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = 2, title.position = "top", title.hjust = 0, byrow = TRUE
    )) +
    ggplot2::labs(title = ttl_c, subtitle = sub_c) +
    base_theme

  patchwork::wrap_plots(p_a, p_b, p_c, ncol = 3) +
    patchwork::plot_layout(widths = c(1, 1, 1))
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
    setNames(unname(pal), labels)
  } else {
    grDevices::colorRampPalette(
      c("#B71C1C", "#EF9A9A", "#BBDEFB", "#1E88E5", "#0D47A1")
    )(length(labels)) |>
      setNames(labels)
  }
}
