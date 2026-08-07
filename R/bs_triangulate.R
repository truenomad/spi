#' Triangulate a field-guide verdict against independent virus detection
#'
#' @description
#' Cross-references the [bs_field_guide()] verdict, which judges whether the
#' AFP surveillance system *could* see poliovirus in a district-year, against
#' the two channels that record whether virus was actually *found*: AFP
#' detection and environmental surveillance (ES). Because ES is largely
#' independent of AFP surveillance quality, it is the channel that can confirm
#' or contradict a capacity flag without arguing in a circle. The result is a
#' per-district-year classification separating a trustworthy silence from a
#' probable blind spot, and a three-level priority for triage.
#'
#' @details
#' The field guide answers a capacity question from signals internal to the
#' AFP stream, so a `FLAG` states that a silence is untrustworthy but not that
#' the silence hid virus. This function adds the independent read. For every
#' district-year with no AFP detection it classifies the verdict against ES
#' status (`positive`, `clear` where a site exists and found nothing, or
#' `no site`):
#' \itemize{
#'   \item **confirmed blindspot** -- `FLAG`, AFP silent, ES positive. The
#'     capacity flag is vindicated by an independent detection; the strongest
#'     corroboration the data offer.
#'   \item **blind, unverified** -- `FLAG`, AFP silent, no ES site. A capacity
#'     warning with no independent channel to check it; the highest-value
#'     place to deploy ES or an active search.
#'   \item **flagged, ES clear** -- `FLAG`, AFP silent, ES negative. The
#'     capacity concern stands, but the independent channel is quiet.
#'   \item **adequate, ES positive** -- `No action`, AFP silent, ES positive.
#'     A potential false-adequate: the guide saw nothing wrong yet virus was
#'     found by the independent channel.
#'   \item **corroborated / uncorroborated clear** -- `No action`, AFP silent,
#'     ES negative or absent. A trustworthy silence, confirmed or unconfirmed.
#' }
#' `REVIEW` and `WATCH` verdicts take the parallel `review, *` and `watch, *`
#' labels. `review, ES positive` is high priority on the same reasoning as
#' `watch, ES positive`; the other two review classes are medium.
#'
#' A detection can inflate NPAFP through the active case finding it triggers,
#' which lifts SPI and makes the preceding verdict look retrospectively
#' correct. Set `detection_lag` to align the verdict in year *t* against
#' detections in year *t + detection_lag*, so the capacity read is taken
#' before the response contaminated it. `flag_preceded` records, for detected
#' district-years, whether that aligned verdict was already `FLAG`, `REVIEW`
#' or `WATCH`.
#'
#' District-years present in `field_guide` but absent from `detections` are
#' read as no detection and no ES site. Detections here are confirmed
#' poliovirus isolations by channel, distinct from the orphan-sequence signal
#' the field guide uses as out-of-grid detection corroboration.
#'
#' @param field_guide A [bs_field_guide()] result (class
#'   `blindspot_field_guide`). Supplies the verdict per district-year.
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
#'   verdict they are tested against, to avoid response-amplified verdicts.
#'   Default: 0.
#' @param verdict_col Name of the verdict column in the field guide.
#'   Default: `"verdict"`.
#' @param id_col Character district id column. Default: NULL (take
#'   `field_guide$id_col`).
#' @param verbose Logical. Progress and join-check messages via cli.
#'   Default: TRUE.
#'
#' @return An object of class `blindspot_triangulation`. A list with:
#' \describe{
#'   \item{district_year}{Tibble, one row per district-year, carrying the
#'     verdict, `es_status`, `afp_hit`, `flag_preceded`, `triangulation`
#'     (factor), and `priority` (factor).}
#'   \item{focal}{The `read_year` slice of `district_year`.}
#'   \item{reference}{Legend of the triangulation classes and their actions.}
#'   \item{read_year, thresholds, params, id_col, call}{Metadata.}
#' }
#'
#' @seealso [bs_field_guide()] for the verdict input,
#'   [bs_triangulate_table()] and [bs_triangulate_map()] for rendering the
#'   result, and [bs_field_guide_table()] for rendering the guide itself.
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' fg <- synth_field_guide
#' det <- synth_surveillance$detections # afp_detected, es_detected, es_covered
#' tri <- bs_triangulate(fg, det, detection_lag = 1L, verbose = FALSE)
#' print(tri)
#' dplyr::count(tri$district_year, triangulation)
bs_triangulate <- function(
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

  stopifnot(inherits(field_guide, "blindspot_field_guide"))
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
    class = "blindspot_triangulation"
  )
}

# ---------------------------------------------------------------------------
# classification
# ---------------------------------------------------------------------------

# @noRd
.tri_levels <- c(
  "confirmed blindspot", "blind, unverified", "review, ES positive",
  "review, unverified", "watch, ES positive", "adequate, ES positive",
  "flagged, ES clear", "review, ES clear", "watch, ES clear",
  "watch, unverified", "corroborated clear", "uncorroborated clear",
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
        .data$verdict_chr == "FLAG" & .data$es_status == "positive" ~
          "confirmed blindspot",
        .data$verdict_chr == "FLAG" & .data$es_status == "clear" ~
          "flagged, ES clear",
        .data$verdict_chr == "FLAG" & .data$es_status == "no site" ~
          "blind, unverified",
        .data$verdict_chr == "REVIEW" & .data$es_status == "positive" ~
          "review, ES positive",
        .data$verdict_chr == "REVIEW" & .data$es_status == "clear" ~
          "review, ES clear",
        .data$verdict_chr == "REVIEW" & .data$es_status == "no site" ~
          "review, unverified",
        .data$verdict_chr == "WATCH" & .data$es_status == "positive" ~
          "watch, ES positive",
        .data$verdict_chr == "WATCH" & .data$es_status == "clear" ~
          "watch, ES clear",
        .data$verdict_chr == "WATCH" & .data$es_status == "no site" ~
          "watch, unverified",
        .data$verdict_chr == "No action" & .data$es_status == "positive" ~
          "adequate, ES positive",
        .data$verdict_chr == "No action" & .data$es_status == "clear" ~
          "corroborated clear",
        .data$verdict_chr == "No action" & .data$es_status == "no site" ~
          "uncorroborated clear",
        .default = NA_character_
      ),
      triangulation = factor(.data$triangulation, levels = .tri_levels),
      priority = dplyr::case_when(
        .data$triangulation == "detected" ~ "resolved",
        .data$triangulation %in% c(
          "confirmed blindspot", "blind, unverified", "review, ES positive",
          "watch, ES positive"
        ) ~ "high",
        # "adequate, ES positive" is the expected subclinical floor, not a
        # false-adequate: AFP only sees the paralytic fraction (~1/200
        # infections), so an adequate system misses most circulation by design
        # and ES picking it up is normal. Medium, not high, so it does not
        # outweigh confirmed blind spots in the triage.
        # the review classes sit at medium alongside their watch counterparts
        # rather than with the flags. The verdict already records that the
        # shortfall is credible; promoting them to high as well would double-
        # count that and swamp the confirmed blind spots in the triage.
        .data$triangulation %in% c(
          "flagged, ES clear", "review, ES clear", "review, unverified",
          "watch, ES clear", "watch, unverified", "adequate, ES positive"
        ) ~ "medium",
        .data$triangulation %in% c(
          "corroborated clear", "uncorroborated clear"
        ) ~ "low",
        .default = NA_character_
      ),
      priority = factor(.data$priority, levels = .tri_priority_levels),
      flag_preceded = dplyr::if_else(
        .data$afp_hit,
        .data$verdict_chr %in% c("FLAG", "REVIEW", "WATCH"),
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
    "Flagged capacity gap where ES independently found virus AFP missed.",
    "Act: the flag is vindicated; active case search and response.",
    "blind, unverified",
    "Flagged capacity gap with no ES site to check it.",
    "Highest-value deployment: add ES or active search to gain a read.",
    "flagged, ES clear",
    "Flagged capacity gap, but the ES site found nothing.",
    "Watch: capacity concern stands at lower urgency.",
    "adequate, ES positive",
    "Verdict said adequate, yet ES found virus: a possible false-adequate.",
    "Review why an adequate district missed a detected circulation.",
    "review, ES positive",
    "Credible but uncorroborated shortfall with an independent ES detection.",
    "Escalate toward flag; the ES hit supplies the missing corroboration.",
    "review, ES clear",
    "Credible but uncorroborated shortfall; the ES site found nothing.",
    "Review at lower urgency; the shortfall stands, corroboration does not.",
    "review, unverified",
    "Credible but uncorroborated shortfall with no ES site to check it.",
    "High-value deployment: ES would settle whether the shortfall hid virus.",
    "watch, ES positive",
    "Borderline verdict with an independent ES detection.",
    "Escalate toward flag; treat as a live signal.",
    "watch, ES clear",
    "Borderline verdict, but the covering ES site found nothing.",
    "Watch; another year of data resolves the verdict.",
    "watch, unverified",
    "Borderline verdict with no ES site to check it.",
    "Watch; ES would resolve whether the uncertainty hides virus.",
    "corroborated clear",
    "Adequate verdict and a covering ES site both quiet.",
    "None: trustworthy silence, independently confirmed.",
    "uncorroborated clear",
    "Adequate verdict but no ES site to confirm it.",
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
print.blindspot_triangulation <- function(x, ...) {
  cli::cli_h1("SPI triangulation")
  cli::cli_inform(c(
    "Read year: {.val {x$read_year}} \\
     | detection lag: {.val {x$params$detection_lag}} \\
     | verdict crossed against AFP and ES channels"
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
    "*" = "{n_blind} blind/unverified (flagged, no ES to check)"
  ))
  cli::cli_alert_info(
    "Class legend in {.code x$reference}; full panel in \\
     {.code x$district_year}."
  )
  invisible(x)
}

# ---------------------------------------------------------------------------
# table renderer (mirrors bs_field_guide_table's scan layout)
# ---------------------------------------------------------------------------

#' Render a triangulation panel as a publication table
#'
#' @description
#' Renders the [bs_triangulate()] classification for one focal year as a
#' `gt` or `flextable` table: one row per district, ordered by triage
#' priority, with the field-guide verdict, ES and AFP status, the resulting
#' triangulation class, and its priority. The triangulation cell is shaded by
#' priority (adverse / intermediate / reassuring), reusing the field-guide
#' palette so the two tables read alike.
#'
#' @param x A [bs_triangulate()] result (class `blindspot_triangulation`).
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
#' @seealso [bs_triangulate()], [bs_triangulate_map()].
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' \dontrun{
#' fg <- synth_field_guide
#' tri <- bs_triangulate(fg, synth_surveillance$detections, verbose = FALSE)
#' bs_triangulate_table(tri, engine = "gt")
#' bs_triangulate_table(tri, engine = "flextable",
#'                      file = tempfile(fileext = ".docx"))
#' }
bs_triangulate_table <- function(
  x,
  engine = c("gt", "flextable"),
  year = NULL,
  name_col = NULL,
  file = NULL,
  shade = TRUE,
  max_rows = 60L,
  ...
) {
  stopifnot(inherits(x, "blindspot_triangulation"))
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
    Verdict = as.character(foc$verdict_chr),
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
# choropleth (mirrors bs_concordance_maps, single panel)
# ---------------------------------------------------------------------------

# thirteen-class triage palette; keys match .tri_levels exactly. Built for the
# legend and the table, not the map: the map defaults to priority precisely
# because this many categorical fills collide on small polygons.
# @noRd
TRI_CLASS_FILL <- c(
  "confirmed blindspot" = "#B71C1C", # deep red: ES caught what AFP missed
  "blind, unverified" = "#AD1457", # magenta: flagged, no channel to check
  "review, ES positive" = "#BF360C", # rust: credible shortfall + ES hit
  "review, unverified" = "#D81B60", # light magenta: review, no channel
  "watch, ES positive" = "#E65100", # orange-red: borderline + ES hit
  "adequate, ES positive" = "#8E24AA", # purple: possible false-adequate
  "flagged, ES clear" = "#F9A825", # amber
  "review, ES clear" = "#FB8C00", # orange: review, ES quiet
  "watch, ES clear" = "#FBC02D", # amber-yellow
  "watch, unverified" = "#FDD835", # yellow
  "corroborated clear" = "#2E7D32", # green: trustworthy, confirmed silence
  "uncorroborated clear" = "#A5D6A7", # light green: trustworthy, unconfirmed
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
#' Draws a single-panel choropleth of the [bs_triangulate()] classification
#' for one focal year, colouring each district by its triangulation class (the
#' full ten-class triage grid) or by its coarser priority. Mirrors
#' [bs_concordance_maps()]: the result object carries no geometry, so
#' `boundaries` is supplied separately and joined by the district id column.
#'
#' @param x A [bs_triangulate()] result (class `blindspot_triangulation`).
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
#' @seealso [bs_triangulate()], [bs_triangulate_table()],
#'   [bs_concordance_maps()].
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' \dontrun{
#' fg <- synth_field_guide
#' tri <- bs_triangulate(fg, synth_surveillance$detections, verbose = FALSE)
#' bs_triangulate_map(tri, synth_surveillance$boundaries, year = 2021)
#' }
bs_triangulate_map <- function(
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
  stopifnot(inherits(x, "blindspot_triangulation"),
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
    # Full ten-class detail (the labels survive in bs_triangulate_table()).
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
  # and the "act here" signal rides a red ring on the flagged districts. A
  # flagged-but-unsited district (blind, unverified) then shows grey fill (no
  # read) under a red ring ("instrument here") instead of hiding in the grey.
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
      flagged = .data$verdict_chr == "FLAG"
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
                               name = "Triage priority (red ring = flagged)") +
    ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = 1, title.position = "top", title.hjust = 0)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    map_theme
}
