#' Triangulate a field-guide judgement against independent virus detection
#'
#' @description
#' Cross-references the [spi_field_guide()] review judgement, which reads
#' whether AFP reporting in a district-year is below expectation, against
#' the two channels that record whether virus was actually *found*: AFP
#' detection and environmental surveillance (ES). Because ES is largely
#' independent of AFP surveillance quality, it is the channel that can confirm
#' or contradict a review priority without arguing in a circle. The result is a
#' per-district-year classification separating a trustworthy silence from a
#' probable blind spot, and a three-level priority for triage.
#'
#' @details
#' The field guide reads reporting against expectation from the AFP stream
#' alone, so a `Review priority` states that a silence may be untrustworthy
#' but not that the silence hid virus. This function adds the independent
#' read. For every district-year with no AFP detection it classifies the
#' judgement against ES status (`positive`, `clear` where a site exists and
#' found nothing, or `no site`):
#' \itemize{
#'   \item **confirmed blindspot** -- `Review priority`, AFP silent, ES
#'     positive. The review priority is supported by an independent
#'     detection; the strongest corroboration the data offer.
#'   \item **blind, unverified** -- `Review priority`, AFP silent, no ES site.
#'     A shortfall with no independent channel to check it; the highest-value
#'     place to deploy ES or an active search.
#'   \item **priority, ES clear** -- `Review priority`, AFP silent, ES
#'     negative. The concern stands, but the independent channel is quiet.
#'   \item **no indication, ES positive** -- `No SPI indication`, AFP silent,
#'     ES positive. The guide saw no shortfall yet virus was found by the
#'     independent channel.
#'   \item **corroborated / uncorroborated clear** -- `No SPI indication`, AFP
#'     silent, ES negative or absent. A silence confirmed or unconfirmed by
#'     ES.
#' }
#' `Monitor` judgements take the parallel `monitor, *` labels.
#' `monitor, ES positive` is high priority, since the ES hit supplies the
#' corroboration the judgement lacked; the other two monitor classes are
#' medium.
#'
#' A detection can inflate NPAFP through the active case finding it triggers,
#' which lifts SPI and makes the preceding judgement look retrospectively
#' correct. Set `detection_lag` to align the judgement in year *t* against
#' detections in year *t + detection_lag*, so the reading is taken before the
#' response contaminated it. `flag_preceded` records, for detected
#' district-years, whether that aligned judgement was already `Review
#' priority` or `Monitor`.
#'
#' District-years present in `field_guide` but absent from `detections` are
#' read as no detection and no ES site. Detections here are confirmed
#' poliovirus isolations by channel, distinct from the orphan-sequence signal
#' the field guide reports as detection context.
#'
#' @param field_guide A [spi_field_guide()] result (class
#'   `spi_field_guide`). Supplies the judgement per district-year.
#' @param detections A data frame at district-year level holding the id and
#'   `year` columns plus the three detection columns named below.
#' @param afp_detected Name of a logical / 0-1 column: poliovirus found
#'   through AFP surveillance in the district-year. Default: `"afp_detected"`.
#' @param es_detected Name of a logical / 0-1 column: poliovirus found through
#'   environmental surveillance. Default: `"es_detected"`.
#' @param es_covered Name of a logical / 0-1 column: an ES site drains this
#'   district-year. `es_detected` is only meaningful where this is true.
#'   Default: `"es_covered"`.
#' @param detection_lag Integer years to shift detections later than the
#'   judgement they are tested against, to avoid response-amplified readings.
#'   Default: 0.
#' @param verdict_col Name of the judgement column in the field guide.
#'   Default: `"verdict"`.
#' @param id_col Character district id column. Default: NULL (take
#'   `field_guide$id_col`).
#' @param verbose Logical. Progress and join-check messages via cli.
#'   Default: TRUE.
#'
#' @return An object of class `spi_triangulation`. A list with:
#' \describe{
#'   \item{district_year}{Tibble, one row per district-year, carrying the
#'     judgement (`verdict`), `es_status`, `afp_hit`, `flag_preceded`, `triangulation`
#'     (factor), and `priority` (factor).}
#'   \item{focal}{The `read_year` slice of `district_year`.}
#'   \item{reference}{Legend of the triangulation classes and their actions.}
#'   \item{read_year, thresholds, params, id_col, call}{Metadata.}
#' }
#'
#' @seealso [spi_field_guide()] for the judgement input,
#'   [spi_triangulate_table()] and [spi_triangulate_map()] for rendering the
#'   result, and [spi_field_guide_table()] for rendering the guide itself.
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' fg <- synth_field_guide
#' det <- synth_surveillance$detections # afp_detected, es_detected, es_covered
#' tri <- spi_triangulate(fg, det, detection_lag = 1L, verbose = FALSE)
#' print(tri)
#' dplyr::count(tri$district_year, triangulation)
spi_triangulate <- function(
  field_guide,
  detections,
  afp_detected = "afp_detected",
  es_detected = "es_detected",
  es_covered = "es_covered",
  detection_lag = 0L,
  verdict_col = "verdict",
  id_col = NULL,
  verbose = TRUE
) {
  .check_pkg(
    c("dplyr", "tibble", "cli"),
    reason = "to triangulate the field guide against detection"
  )

  stopifnot(inherits(field_guide, "spi_field_guide"))
  if (!is.data.frame(detections)) {
    cli::cli_abort("{.arg detections} must be a data frame.")
  }

  id_col <- id_col %||% field_guide$id_col %||% "district_id"
  detection_lag <- as.integer(detection_lag)

  district_year <- field_guide$district_year
  need_fg <- c(id_col, "year", verdict_col)
  miss_fg <- setdiff(need_fg, names(district_year))
  if (length(miss_fg) > 0) {
    cli::cli_abort(
      "{.arg field_guide$district_year} is missing {.val {miss_fg}}."
    )
  }
  need_det <- c(id_col, "year", afp_detected, es_detected, es_covered)
  miss_det <- setdiff(need_det, names(detections))
  if (length(miss_det) > 0) {
    cli::cli_abort("{.arg detections} is missing {.val {miss_det}}.")
  }

  # keyed detections, shifted back by the lag so verdict[t] meets det[t+lag]
  keyed <- tibble::tibble(
    join_id = as.character(detections[[id_col]]),
    join_year = as.integer(detections[["year"]]) - detection_lag,
    afp_hit = .tri_lgl(detections[[afp_detected]]),
    es_hit = .tri_lgl(detections[[es_detected]]),
    es_site = .tri_lgl(detections[[es_covered]])
  )
  if (anyDuplicated(keyed[c("join_id", "join_year")]) > 0) {
    cli::cli_abort("{.arg detections} has >1 row per district-year.")
  }

  # attach detection to every field-guide district-year, keeping row count
  n_before <- nrow(district_year)
  district_year <- district_year |>
    dplyr::mutate(
      join_id = as.character(.data[[id_col]]),
      join_year = as.integer(.data[["year"]])
    ) |>
    dplyr::left_join(keyed, by = c("join_id", "join_year"))
  stopifnot(nrow(district_year) == n_before)

  # absent detection rows read as no detection and no site
  district_year <- district_year |>
    dplyr::mutate(
      afp_hit = dplyr::coalesce(.data$afp_hit, FALSE),
      es_hit = dplyr::coalesce(.data$es_hit, FALSE),
      es_site = dplyr::coalesce(.data$es_site, FALSE),
      verdict_chr = as.character(.data[[verdict_col]]),
      es_status = dplyr::case_when(
        .data$es_site & .data$es_hit ~ "positive",
        .data$es_site & !.data$es_hit ~ "clear",
        !.data$es_site ~ "no site"
      )
    )

  district_year <- .tri_classify(district_year)

  if (verbose) {
    cli::cli_alert_info(
      "Triangulated {n_before} district-year{?s} at lag {detection_lag}."
    )
  }

  read_year <- field_guide$read_year %||% max(district_year$year, na.rm = TRUE)
  focal <- district_year[district_year$year == read_year, , drop = FALSE]

  if (verbose) {
    cli::cli_alert_success(
      "Read {.val {read_year}}: \\
       {sum(focal$triangulation == 'confirmed blindspot')} confirmed, \\
       {sum(focal$triangulation == 'blind, unverified')} blind/unverified, \\
       {sum(focal$priority == 'high')} high-priority."
    )
  }

  structure(
    list(
      district_year = tibble::as_tibble(district_year),
      focal = tibble::as_tibble(focal),
      reference = .tri_reference(),
      read_year = as.integer(read_year),
      thresholds = field_guide$thresholds,
      params = list(detection_lag = detection_lag),
      id_col = id_col,
      call = match.call()
    ),
    class = "spi_triangulation"
  )
}

# ---------------------------------------------------------------------------
# classification
# ---------------------------------------------------------------------------

# @noRd
.tri_levels <- c(
  "confirmed blindspot", "blind, unverified", "monitor, ES positive",
  "no indication, ES positive", "priority, ES clear", "monitor, ES clear",
  "monitor, unverified", "corroborated clear", "uncorroborated clear",
  "detected"
)

# @noRd
.tri_priority_levels <- c("high", "medium", "low", "resolved")

# @noRd
.tri_classify <- function(district_year) {
  district_year |>
    dplyr::mutate(
      triangulation = dplyr::case_when(
        .data$afp_hit ~ "detected",
        .data$verdict_chr == "Review priority" &
          .data$es_status == "positive" ~ "confirmed blindspot",
        .data$verdict_chr == "Review priority" &
          .data$es_status == "clear" ~ "priority, ES clear",
        .data$verdict_chr == "Review priority" &
          .data$es_status == "no site" ~ "blind, unverified",
        .data$verdict_chr == "Monitor" & .data$es_status == "positive" ~
          "monitor, ES positive",
        .data$verdict_chr == "Monitor" & .data$es_status == "clear" ~
          "monitor, ES clear",
        .data$verdict_chr == "Monitor" & .data$es_status == "no site" ~
          "monitor, unverified",
        .data$verdict_chr == "No SPI indication" &
          .data$es_status == "positive" ~ "no indication, ES positive",
        .data$verdict_chr == "No SPI indication" &
          .data$es_status == "clear" ~ "corroborated clear",
        .data$verdict_chr == "No SPI indication" &
          .data$es_status == "no site" ~ "uncorroborated clear",
        .default = NA_character_
      ),
      triangulation = factor(.data$triangulation, levels = .tri_levels),
      priority = dplyr::case_when(
        .data$triangulation == "detected" ~ "resolved",
        .data$triangulation %in% c(
          "confirmed blindspot", "blind, unverified", "monitor, ES positive"
        ) ~ "high",
        # "no indication, ES positive" is the expected subclinical floor, not a
        # false reassurance: AFP only sees the paralytic fraction (~1/200
        # infections), so a system reporting as expected misses most
        # circulation by design and ES picking it up is normal. Medium, not
        # high, so it does not outweigh confirmed blind spots in the triage.
        .data$triangulation %in% c(
          "priority, ES clear", "monitor, ES clear", "monitor, unverified",
          "no indication, ES positive"
        ) ~ "medium",
        .data$triangulation %in% c(
          "corroborated clear", "uncorroborated clear"
        ) ~ "low",
        .default = NA_character_
      ),
      priority = factor(.data$priority, levels = .tri_priority_levels),
      flag_preceded = dplyr::if_else(
        .data$afp_hit,
        .data$verdict_chr %in% c("Review priority", "Monitor"),
        NA
      )
    )
}

# coerce logical / 0-1 / character detection flags to logical.
# @noRd
.tri_lgl <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    return(x > 0)
  }
  tolower(as.character(x)) %in% c("1", "true", "yes", "y", "positive", "pos")
}

# @noRd
.tri_reference <- function() {
  tibble::tribble(
    ~class, ~means, ~action,
    "confirmed blindspot",
    "Review priority where ES independently found virus AFP missed.",
    "Act: the shortfall is supported; active case search and response.",
    "blind, unverified",
    "Review priority with no ES site to check it.",
    "Highest-value deployment: add ES or active search to gain a read.",
    "priority, ES clear",
    "Review priority, but the ES site found nothing.",
    "Review: the concern stands at lower urgency.",
    "no indication, ES positive",
    "No SPI indication, yet ES found virus.",
    "Review why a district reporting as expected missed a detected circulation.",
    "monitor, ES positive",
    "Monitored shortfall with an independent ES detection.",
    "Escalate toward review priority; the ES hit supplies corroboration.",
    "monitor, ES clear",
    "Monitored shortfall, but the covering ES site found nothing.",
    "Monitor; another year of data resolves the judgement.",
    "monitor, unverified",
    "Monitored shortfall with no ES site to check it.",
    "Monitor; ES would resolve whether the shortfall hides virus.",
    "corroborated clear",
    "No SPI indication and a covering ES site both quiet.",
    "None: silence independently confirmed.",
    "uncorroborated clear",
    "No SPI indication but no ES site to confirm it.",
    "None routine; ES would upgrade this to corroborated.",
    "detected",
    "Poliovirus already surfaced through AFP in the district-year.",
    "Outside triage; see flag_preceded for whether the guide warned first."
  )
}

# ---------------------------------------------------------------------------
# S3 print
# ---------------------------------------------------------------------------

#' @export
print.spi_triangulation <- function(x, ...) {
  cli::cli_h1("SPI triangulation")
  cli::cli_inform(c(
    "Read year: {.val {x$read_year}} \\
     | detection lag: {.val {x$params$detection_lag}} \\
     | judgement crossed against AFP and ES channels"
  ))

  focal <- x$focal
  cli::cli_h2("Priority for {x$read_year}")
  priority_tbl <- tibble::tibble(
    priority = factor(.tri_priority_levels, levels = .tri_priority_levels),
    n = vapply(
      .tri_priority_levels,
      function(p) sum(focal$priority == p, na.rm = TRUE),
      integer(1)
    )
  )
  print(priority_tbl)

  n_confirmed <- sum(focal$triangulation == "confirmed blindspot")
  n_blind <- sum(focal$triangulation == "blind, unverified")
  cli::cli_inform(c(
    "*" = "{n_confirmed} confirmed blindspot{?s} (ES caught what AFP missed)",
    "*" = "{n_blind} blind/unverified (review priority, no ES to check)"
  ))
  cli::cli_alert_info(
    "Class legend in {.code x$reference}; full panel in \\
     {.code x$district_year}."
  )
  invisible(x)
}

# ---------------------------------------------------------------------------
# table renderer (mirrors spi_field_guide_table's scan layout)
# ---------------------------------------------------------------------------

#' Render a triangulation panel as a publication table
#'
#' @description
#' Renders the [spi_triangulate()] classification for one focal year as a
#' `gt` or `flextable` table: one row per district, ordered by triage
#' priority, with the field-guide judgement, ES and AFP status, the resulting
#' triangulation class, and its priority. The triangulation cell is shaded by
#' priority (adverse / intermediate / reassuring), reusing the field-guide
#' palette so the two tables read alike.
#'
#' @param x A [spi_triangulate()] result (class `spi_triangulation`).
#' @param engine Table engine: `"gt"` or `"flextable"`.
#' @param year Focal year. Default: NULL (`x$read_year`).
#' @param name_col Column to label districts by. Default: NULL (first of
#'   `adm2_name`, `adm1_name`, else the id column).
#' @param file Optional output path; the extension sets the format. Default:
#'   NULL (return the table object without saving).
#' @param shade Logical. Apply priority shading. Default: TRUE.
#' @param max_rows Integer. Cap the number of rows (highest priority first).
#'   Default: 60.
#' @param ... Passed to the underlying `gt::gtsave()` / flextable saver.
#'
#' @return The `gt_tbl` or `flextable` object, invisibly when `file` is set.
#'
#' @seealso [spi_triangulate()], [spi_triangulate_map()].
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' \dontrun{
#' fg <- synth_field_guide
#' tri <- spi_triangulate(fg, synth_surveillance$detections, verbose = FALSE)
#' spi_triangulate_table(tri, engine = "gt")
#' spi_triangulate_table(tri, engine = "flextable",
#'                      file = tempfile(fileext = ".docx"))
#' }
spi_triangulate_table <- function(
  x,
  engine = c("gt", "flextable"),
  year = NULL,
  name_col = NULL,
  file = NULL,
  shade = TRUE,
  max_rows = 60L,
  ...
) {
  stopifnot(inherits(x, "spi_triangulation"))
  engine <- match.arg(engine)

  .check_pkg(
    if (engine == "gt") "gt" else c("flextable", "officer"),
    reason = sprintf("to render the triangulation with %s", engine)
  )

  year <- as.integer(year %||% x$read_year)
  foc <- x$district_year[x$district_year$year == year, , drop = FALSE]
  if (nrow(foc) == 0L) {
    cli::cli_abort("no district-year rows for {.arg year} = {.val {year}}.")
  }

  id_col <- x$id_col
  name_col <- name_col %||%
    intersect(c("adm2_name", "adm1_name"), names(foc))[1] %||% id_col

  # highest priority first, then class order, then alphabetically by district
  foc <- foc[order(foc$priority, foc$triangulation, foc[[name_col]]), ,
             drop = FALSE]
  if (nrow(foc) > max_rows) foc <- foc[seq_len(max_rows), , drop = FALSE]

  df <- tibble::tibble(
    District = foc[[name_col]],
    Judgement = as.character(foc$verdict_chr),
    ES = foc$es_status,
    AFP = ifelse(foc$afp_hit, "detected", "-"),
    Triangulation = as.character(foc$triangulation),
    Priority = as.character(foc$priority)
  )
  pclass <- .tri_priority_fill(foc$priority)
  title <- sprintf("SPI triangulation: district scan, %d", year)
  subtitle <- sprintf(
    "Field-guide verdict crossed against ES and AFP detection (lag %d)",
    x$params$detection_lag
  )

  built <- if (engine == "gt") {
    g <- gt::gt(df)
    g <- gt::tab_header(g, title = title, subtitle = subtitle)
    if (shade) {
      for (cl in unique(pclass)) {
        g <- gt::tab_style(
          g,
          style = gt::cell_fill(color = FG_CLASS_FILL[[cl]]),
          locations = gt::cells_body(
            columns = "Triangulation", rows = pclass == cl
          )
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
          ft, i = i, j = "Triangulation", bg = FG_CLASS_FILL[[pclass[i]]]
        )
      }
    }
    flextable::autofit(ft)
  }

  if (is.null(file)) {
    return(built)
  }
  .fg_save(built, file, engine, ...)
  invisible(built)
}

# map a triage priority to the field-guide shading tone.
# @noRd
.tri_priority_fill <- function(priority) {
  vapply(as.character(priority), function(p) {
    switch(p, high = "warm", medium = "amber", low = "cool", "none")
  }, character(1))
}

# ---------------------------------------------------------------------------
# choropleth (mirrors spi_concordance_maps, single panel)
# ---------------------------------------------------------------------------

# ten-class triage palette; keys match .tri_levels exactly. Built for the
# legend and the table, not the map: the map defaults to priority precisely
# because this many categorical fills collide on small polygons.
# @noRd
TRI_CLASS_FILL <- c(
  "confirmed blindspot" = "#B71C1C", # deep red: ES caught what AFP missed
  "blind, unverified" = "#AD1457", # magenta: priority, no channel to check
  "monitor, ES positive" = "#E65100", # orange-red: monitored + ES hit
  "no indication, ES positive" = "#8E24AA", # purple: no shortfall, ES hit
  "priority, ES clear" = "#F9A825", # amber
  "monitor, ES clear" = "#FBC02D", # amber-yellow
  "monitor, unverified" = "#FDD835", # yellow
  "corroborated clear" = "#2E7D32", # green: confirmed silence
  "uncorroborated clear" = "#A5D6A7", # light green: unconfirmed silence
  "detected" = "#1565C0" # blue: already surfaced through AFP
)

# @noRd
TRI_PRIORITY_FILL <- c(
  high = "#C62828",
  medium = "#F9A825",
  low = "#2E7D32",
  resolved = "#1565C0"
)

#' Map a triangulation panel over district polygons
#'
#' @description
#' Draws a single-panel choropleth of the [spi_triangulate()] classification
#' for one focal year, colouring each district by its triangulation class (the
#' full ten-class triage grid) or by its coarser priority. Mirrors
#' [spi_concordance_maps()]: the result object carries no geometry, so
#' `boundaries` is supplied separately and joined by the district id column.
#'
#' @param x A [spi_triangulate()] result (class `spi_triangulation`).
#' @param boundaries An `sf` polygon layer with the district id column.
#' @param year Focal year. Default: NULL (`x$read_year`).
#' @param by Fill by `"triangulation"` (ten classes) or `"priority"` (four
#'   levels). Default: `"triangulation"`.
#' @param id_col District id column joining `x` to `boundaries`. Default: NULL
#'   (`x$id_col`).
#' @param title Optional plot title. Default: NULL (a sensible default).
#' @param ... Currently unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [spi_triangulate()], [spi_triangulate_table()],
#'   [spi_concordance_maps()].
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' \dontrun{
#' fg <- synth_field_guide
#' tri <- spi_triangulate(fg, synth_surveillance$detections, verbose = FALSE)
#' spi_triangulate_map(tri, synth_surveillance$boundaries, year = 2021)
#' }
spi_triangulate_map <- function(
  x,
  boundaries,
  year = NULL,
  by = c("priority", "triangulation"),
  id_col = NULL,
  title = NULL,
  ...
) {
  .check_pkg(c("ggplot2", "sf", "dplyr"),
             reason = "to draw the triangulation map")
  stopifnot(inherits(x, "spi_triangulation"),
            inherits(boundaries, "sf"))
  by <- match.arg(by)
  id_col <- id_col %||% x$id_col

  dy <- x$district_year
  if (is.null(year)) year <- x$read_year
  yr <- as.integer(year)
  slice <- dy[dy$year == yr, , drop = FALSE]
  if (nrow(slice) == 0L) {
    cli::cli_abort("no district-year rows for {.arg year} = {.val {yr}}")
  }

  bnd_slice <- boundaries |>
    dplyr::inner_join(
      slice[, c(id_col, "triangulation", "priority", "es_status",
                "afp_hit", "verdict_chr")],
      by = id_col
    )

  # adm1 (province) outline over the district choropleth. The adm2 layer is a
  # clean coverage, so the dissolve leaves no sliver artefacts.
  adm1_layer <- NULL
  if ("adm1_name" %in% names(boundaries)) {
    adm1_outline <- boundaries |>
      dplyr::group_by(.data$adm1_name) |>
      dplyr::summarise(.groups = "drop")
    adm1_layer <- ggplot2::geom_sf(
      data = adm1_outline, fill = NA, colour = "grey25", linewidth = 0.3,
      inherit.aes = FALSE
    )
  }

  det_year <- yr + x$params$detection_lag
  subtitle <- sprintf(
    "Field-guide verdict (%d) crossed with ES and AFP detection (%d)",
    yr, det_year
  )
  map_theme <- ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12,
                                         margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey30",
                                            margin = ggplot2::margin(t = 0,
                                                                     b = 12)),
      plot.margin = ggplot2::margin(6, 18, 6, 8),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 8, face = "bold"),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.width = grid::unit(1.1, "lines"),
      legend.key.height = grid::unit(0.5, "lines")
    )

  if (by == "triangulation") {
    # Full ten-class detail (the labels survive in spi_triangulate_table()).
    bnd_slice$fill_cat <- bnd_slice$triangulation
    title <- title %||%
      sprintf("Triangulation classes, verdict %d vs detection %d", yr, det_year)
    return(
      ggplot2::ggplot(bnd_slice) +
        ggplot2::geom_sf(ggplot2::aes(fill = .data$fill_cat),
                         colour = "grey82", linewidth = 0.1) +
        adm1_layer +
        ggplot2::scale_fill_manual(values = TRI_CLASS_FILL, drop = TRUE,
                                   na.value = "grey85",
                                   name = "Triangulation class") +
        ggplot2::guides(fill = ggplot2::guide_legend(
          ncol = 4, byrow = TRUE, title.position = "top", title.hjust = 0)) +
        ggplot2::labs(title = title, subtitle = subtitle) +
        map_theme
    )
  }

  # Default: triage by priority. Ten categorical fills on hundreds of small
  # polygons collide, so map the three-level priority instead. Coverage is a
  # separate variable, so grey means one thing only -- no independent ES read --
  # and the "act here" signal rides a red ring on the review priority
  # districts. A priority district with no ES site (blind, unverified) then
  # shows grey fill (no read) under a red ring ("instrument here") instead of
  # hiding in the grey.
  # AFP-detected sits off-grid in a neutral grey.
  bnd_slice <- bnd_slice |>
    dplyr::mutate(
      map_fill = dplyr::case_when(
        .data$afp_hit ~ "detected",
        .data$es_status == "no site" ~ "no ES read",
        .default = as.character(.data$priority)
      ),
      map_fill = factor(
        .data$map_fill,
        levels = c("high", "medium", "low", "detected", "no ES read")
      ),
      flagged = .data$verdict_chr == "Review priority"
    )
  fill_pal <- c(
    high = "#b2182b", medium = "#ef8a62", low = "#4d9221",
    detected = "grey55", `no ES read` = "grey85"
  )
  title <- title %||%
    sprintf("Triangulation triage, verdict %d vs detection %d", yr, det_year)

  ggplot2::ggplot(bnd_slice) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$map_fill), colour = "grey82",
                     linewidth = 0.1) +
    adm1_layer +
    ggplot2::geom_sf(
      data = dplyr::filter(bnd_slice, .data$flagged),
      fill = NA, colour = "#b2182b", linewidth = 0.55, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = fill_pal, drop = FALSE,
                               name = "Triage priority (red ring = review priority)") +
    ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = 1, title.position = "top", title.hjust = 0)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    map_theme
}
