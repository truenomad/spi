#' Render a one-page SPI pager for a single district
#'
#' @description
#' Turns one district's [bs_field_guide()] reading into a self-contained,
#' print-ready HTML "pager": a masthead with the verdict, an SPI-over-time
#' chart of the district against its touching neighbours (with the 90%
#' credible-interval ribbon and any orphan-poliovirus detections marked), the
#' five-STEPS reading laid out as gate / magnitude / corroboration (with
#' seasonal and detection corroboration read out of grid), and a verdict banner
#' with the recommended action.
#'
#' It is the single-district companion to [bs_field_guide_table()]: where the
#' table scans many districts at once, the pager is the tear-sheet you hand to
#' a reviewer for the one district you are about to investigate.
#'
#' @details
#' The whole page is rendered server-side as static HTML and an inline SVG,
#' so the output file needs no JavaScript and no network access (the only
#' remote reference is a Google Fonts stylesheet, which degrades to system
#' fonts offline). Every number is read from `x`; nothing is simulated.
#'
#' Pass `adjacency` (from [bs_adjacency()]) to draw each touching neighbour as
#' its own muted line and to key the surroundings (S) reading off the same
#' graph the field guide used. Without it the chart falls back to the single
#' neighbour-median series already stored on the field guide, and the neighbour
#' lines are omitted.
#'
#' The out-of-grid detection row reports poliovirus found there through either
#' channel: the case-based (AFP) detections already carried by the field
#' guide's `genomic` input, and any environmental-surveillance (ES) positives
#' passed via `es`. Both are marked on the chart (AFP as a filled diamond, ES as
#' a hollow ring) and named in the reading. Detections are narrative
#' corroboration only; they never enter the STEPS count or change the verdict,
#' matching the paper.
#'
#' The accent colour tracks the verdict: rose for a flag, amber for a watch,
#' green for no action. A flag with an orphan-poliovirus detection is titled
#' *"Flag, corroborated"*.
#'
#' @param x A [bs_field_guide()] result (class `blindspot_field_guide`).
#' @param district District to profile: either an id (e.g. the admin-2 GUID)
#'   in the id column, or a district name matched (case-insensitively) against
#'   `name_col`. Ids are tried first, so an id is unambiguous.
#' @param adjacency Optional spatial neighbour object from [bs_adjacency()]
#'   (class `blindspot_nb`). Enables per-neighbour lines on the chart. Default:
#'   NULL.
#' @param boundaries Optional sf polygon layer keyed by `id_col`. Serves double
#'   duty: when `adjacency` is NULL the neighbour graph is built from it with
#'   [bs_adjacency()] (so the pager is self-contained -- field guide +
#'   shapefile, no pre-built graph), and its real geometry draws a locator
#'   inset on the chart, the focal district in the accent colour ringed by its
#'   touching neighbours. Default: NULL (no inset).
#' @param es Optional ad-hoc environmental-surveillance detections keyed by
#'   `id_col` and `year`, overriding the field guide's own ES channel. Normally
#'   ES is supplied once to [bs_field_guide()] (via its `es` argument) and read
#'   from there, so this is only needed for a field guide built without ES.
#'   Each row (or each `es_col`-positive row) is an ES positive. Default: NULL
#'   (use the field guide's `es_years`).
#' @param es_col Optional name of a count / logical column in `es`; only rows
#'   with a positive count (or `TRUE`) count as detections. Default: NULL
#'   (every row counts).
#' @param detection_label Serotype label used in the out-of-grid detection
#'   reading, e.g. `"cVDPV2"` or `"WPV1"`. Default: `"cVDPV2"`.
#' @param id_col District id column, shared by `adjacency` / `boundaries` /
#'   `es`. Default: NULL (`x$id_col`).
#' @param year Integer focal year for the reading. Default: NULL
#'   (`x$read_year`).
#' @param path Optional output directory. When set, the pager is written into
#'   it under an auto-generated name,
#'   `spi_<adm0>_<adm1>_<adm2>_field_pager.<ext>`, one file per `format`. The
#'   directory is created if it does not exist. Default: NULL.
#' @param file Optional explicit output path; the extension (`.html` or
#'   `.png`) sets the format. Takes precedence over `path` / `format`. Default:
#'   NULL (return the pager object without writing).
#' @param format Output format(s) written under `path`: any of `"html"` and
#'   `"png"`. PNG rendering needs \pkg{webshot2} and a headless Chrome. Default:
#'   both.
#' @param name_col Column to resolve `district` against and label the pager by.
#'   Default: NULL (first of `adm2_name`, `adm1_name`, else the id column).
#' @param admin_label Short description of the administrative level, shown
#'   under the district name. Default: `"admin-2 district"`.
#' @param unit_noun Singular common noun for the operational unit, used in the
#'   on-page reading (e.g. "expected for the `unit_noun`", "A `unit_noun` is
#'   flagged when..."). Set it to match `admin_label`, e.g. `"province"` for
#'   admin-1 inputs. Default: `"district"`.
#' @param note Provenance tag printed in the eyebrow and footer. Default:
#'   `"illustrative"`.
#' @param verbose Logical. Emit a cli summary on build. Default: TRUE.
#'
#' @return An object of class `blindspot_pager`: a list with the rendered
#'   `html` string, the resolved `district`, `year`, `verdict`, and `paths`
#'   (the files written, empty when neither `path` nor `file` is set). Printing
#'   it reports the verdict and any written files. Recover the markup with
#'   `as.character()`.
#'
#' @seealso [bs_field_guide()] for the reading, [bs_field_guide_table()] for
#'   the multi-district table, and [bs_adjacency()] for the neighbour graph.
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' fg <- synth_field_guide
#' pager <- bs_field_guide_pager(fg, district = "Tirwen")
#' pager
#' \dontrun{
#' # self-contained: hand it the shapefile and it builds the neighbour graph
#' # itself, draws a locator inset, and reads both detection channels (AFP +
#' # ES, already carried by the field guide) into the out-of-grid detection row
#' bs_field_guide_pager(
#'   fg,
#'   district = "Tirwen",
#'   boundaries = synth_surveillance$boundaries,
#'   id_col = "adm2_guid",
#'   path = "."
#' )
#' }
bs_field_guide_pager <- function(
  x,
  district,
  adjacency = NULL,
  boundaries = NULL,
  es = NULL,
  es_col = NULL,
  detection_label = "cVDPV2",
  year = NULL,
  path = NULL,
  file = NULL,
  format = c("html", "png"),
  name_col = NULL,
  id_col = NULL,
  admin_label = "admin-2 district",
  unit_noun = "district",
  note = "illustrative",
  verbose = TRUE
) {
  .check_pkg(c("dplyr", "cli"), reason = "to render the SPI pager")
  stopifnot(inherits(x, "blindspot_field_guide"))

  id_col <- id_col %||% x$id_col %||% "district_id"
  dy <- x$district_year
  name_col <- name_col %||%
    intersect(c("adm2_name", "adm1_name"), names(dy))[1] %||% id_col

  year <- as.integer(year %||% x$read_year)
  if (!year %in% dy$year) {
    cli::cli_abort("{.arg year} = {.val {year}} has no district-year rows.")
  }

  foc_id <- .pager_resolve(district, dy, id_col, name_col)

  series <- dy[dy[[id_col]] == foc_id & dy$year <= year, , drop = FALSE]
  series <- series[order(series$year), , drop = FALSE]
  focal <- series[series$year == year, , drop = FALSE]
  if (nrow(focal) != 1L) {
    cli::cli_abort(
      "district {.val {district}} has no row for {.arg year} = {.val {year}}."
    )
  }

  # build the neighbour graph from the shapefile when no graph is supplied
  if (is.null(adjacency) && !is.null(boundaries)) {
    adjacency <- suppressMessages(
      bs_adjacency(boundaries, id_col = id_col)
    )
  }
  neighbours <- .pager_neighbours(
    dy, adjacency, foc_id, id_col, series$year, name_col
  )
  nb_ids <- .pager_neighbour_ids(adjacency, foc_id, id_col)

  # AFP + ES detection years for the out-of-grid detection row; both channels
  # come from the field guide (orphan_years / es_years), with `es` an optional
  # ad-hoc override
  detections <- list(
    afp = .pager_parse_years(focal$orphan_years),
    es = if (!is.null(es)) {
      .pager_es_years(es, es_col, foc_id, id_col, year)
    } else {
      .pager_parse_years(focal$es_years %||% "")
    }
  )

  spi_cut <- x$thresholds$spi
  has_detection <- length(detections$afp) > 0 || length(detections$es) > 0
  vstyle <- .pager_verdict_style(focal, corroborated = has_detection)
  chart <- .pager_svg(
    series, focal, neighbours, vstyle$accent, spi_cut, detections, name_col
  )
  locator <- if (!is.null(boundaries)) {
    .pager_locator_svg(boundaries, foc_id, nb_ids, id_col, vstyle$accent)
  } else {
    ""
  }
  signals <- .pager_signals(
    focal, spi_cut, x$params, detections, detection_label, unit_noun
  )

  html <- .pager_html(
    focal = focal,
    series = series,
    neighbours = neighbours,
    chart = chart,
    locator = locator,
    signals = signals,
    vstyle = vstyle,
    spi_cut = spi_cut,
    params = x$params,
    detections = detections,
    detection_label = detection_label,
    year = year,
    name_col = name_col,
    id_col = id_col,
    admin_label = admin_label,
    unit_noun = unit_noun,
    note = note
  )

  targets <- .pager_targets(focal, name_col, path, file, format)
  for (t in targets) .pager_write(html, t)

  if (verbose) {
    cli::cli_alert_success(
      "Pager for {.val {focal[[name_col]]}} ({year}): \\
       verdict {.val {as.character(focal$verdict)}}."
    )
    for (t in targets) cli::cli_alert_info("Written to {.file {t}}.")
  }

  structure(
    list(
      html = html,
      district = focal[[name_col]],
      id = foc_id,
      year = year,
      verdict = as.character(focal$verdict),
      paths = targets
    ),
    class = "blindspot_pager"
  )
}

#' @export
print.blindspot_pager <- function(x, ...) {
  cli::cli_h1("SPI pager")
  cli::cli_inform(
    "{.strong {x$district}} | {x$year} | verdict {.val {x$verdict}}"
  )
  if (length(x$paths) > 0) {
    for (p in x$paths) cli::cli_alert_info("Written to {.file {p}}.")
  } else {
    cli::cli_alert_info(
      "Not written to disk; pass {.arg path} or {.arg file}, or use \\
       {.code writeLines(as.character(x), \"pager.html\")}."
    )
  }
  invisible(x)
}

#' @export
as.character.blindspot_pager <- function(x, ...) {
  x$html
}

# ---- internal helpers -----------------------------------------------------

# resolve a district name or id to a single id-column value.
# @noRd
.pager_resolve <- function(district, dy, id_col, name_col) {
  district <- as.character(district)[1]
  by_id <- dy[[id_col]] == district
  if (any(by_id, na.rm = TRUE)) {
    return(dy[[id_col]][which(by_id)[1]])
  }
  hits <- unique(dy[[id_col]][
    tolower(dy[[name_col]]) == tolower(district)
  ])
  hits <- hits[!is.na(hits)]
  if (length(hits) == 1L) return(hits)
  if (length(hits) > 1L) {
    cli::cli_abort(
      "district {.val {district}} matches {length(hits)} ids; \\
       pass an {.arg {id_col}} value instead."
    )
  }
  near <- unique(dy[[name_col]])
  stub <- substr(tolower(district), 1, 3)
  near <- near[order(!startsWith(tolower(near), stub))]
  cli::cli_abort(c(
    "no district matches {.val {district}}.",
    "i" = "did you mean {.val {utils::head(near, 5)}}?"
  ))
}

# resolve the set of files to write from `path` / `file` / `format`.
# `file` (explicit) wins; otherwise `path` + each `format` gives one
# auto-named file. empty when neither is set.
# @noRd
.pager_targets <- function(focal, name_col, path, file, format) {
  if (!is.null(file)) {
    ext <- tolower(tools::file_ext(file))
    if (!ext %in% c("html", "png")) {
      cli::cli_abort("{.arg file} must end in {.val .html} or {.val .png}.")
    }
    return(file)
  }
  if (is.null(path)) return(character(0))
  format <- match.arg(format, c("html", "png"), several.ok = TRUE)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  base <- .pager_autoname(focal, name_col)
  file.path(path, paste0(base, ".", format))
}

# build the auto file name spi_<adm0>_<adm1>_<adm2>_field_pager, slugging and
# dropping any missing admin component.
# @noRd
.pager_autoname <- function(focal, name_col) {
  parts <- c(
    focal$adm0_name %||% NA,
    focal$adm1_name %||% NA,
    focal[[name_col]] %||% NA
  )
  parts <- .pager_slug(parts[!is.na(parts) & nzchar(parts)])
  parts <- parts[nzchar(parts)]
  # adm1 data repeats the unit as both adm1 and name; keep each level once
  parts <- parts[!duplicated(parts)]
  paste0("spi_", paste(parts, collapse = "_"), "_field_pager")
}

# slug a label to lowercase alphanumerics joined by hyphens.
# @noRd
.pager_slug <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-+|-+$", "", x)
}

# write the html to one target; dispatch png through webshot2 + Chrome.
# @noRd
.pager_write <- function(html, target) {
  ext <- tolower(tools::file_ext(target))
  if (ext == "html") {
    writeLines(html, target)
    return(invisible(target))
  }
  if (ext == "png") {
    .check_pkg(
      "webshot2",
      reason = "to render the pager to png (needs a headless Chrome)"
    )
    tmp <- tempfile(fileext = ".html")
    on.exit(unlink(tmp), add = TRUE)
    writeLines(html, tmp)
    webshot2::webshot(
      tmp, target,
      vwidth = 854, vheight = 1183, delay = 1, zoom = 2
    )
    return(invisible(target))
  }
  cli::cli_abort("cannot write {.val {ext}}; use {.val .html} or {.val .png}.")
}

# neighbour spi series aligned to `years`. with an adjacency graph, one series
# per touching neighbour; without, the stored neighbour-median column.
# @noRd
.pager_neighbours <- function(dy, adjacency, foc_id, id_col, years, name_col) {
  if (is.null(adjacency)) {
    med <- dy[dy[[id_col]] == foc_id, c("year", "neighbour_spi")]
    vals <- med$neighbour_spi[match(years, med$year)]
    if (all(is.na(vals))) return(list())
    return(list(list(name = "neighbour median", series = vals)))
  }

  ids <- attr(adjacency, "region.id")
  if (is.null(ids)) ids <- as.character(seq_along(adjacency))
  ids <- as.character(ids)
  idx <- match(foc_id, ids)
  if (is.na(idx)) return(list())
  nb <- adjacency[[idx]]
  if (length(nb) == 1L && nb == 0L) return(list())
  nb_ids <- ids[nb]

  has_name <- name_col %in% names(dy)
  lapply(nb_ids, function(nid) {
    cols <- c("year", "spi_median", if (has_name) name_col)
    sub <- dy[dy[[id_col]] == nid, cols]
    if (nrow(sub) == 0L) return(NULL)
    label <- if (has_name) sub[[name_col]][1] %||% nid else nid
    list(name = label, series = sub$spi_median[match(years, sub$year)])
  }) |>
    Filter(f = Negate(is.null))
}

# touching-neighbour ids for the focal district from the adjacency graph.
# @noRd
.pager_neighbour_ids <- function(adjacency, foc_id, id_col) {
  if (is.null(adjacency)) return(character(0))
  ids <- attr(adjacency, "region.id")
  if (is.null(ids)) ids <- as.character(seq_along(adjacency))
  ids <- as.character(ids)
  idx <- match(as.character(foc_id), ids)
  if (is.na(idx)) return(character(0))
  nb <- adjacency[[idx]]
  if (length(nb) == 1L && nb == 0L) return(character(0))
  ids[nb]
}

# locator inset: the focal district (accent) ringed by its touching
# neighbours (muted), drawn from real boundary geometry into a square viewBox.
# @noRd
.pager_locator_svg <- function(boundaries, foc_id, nb_ids, id_col, accent,
                               size = 100, pad = 6) {
  .check_pkg("sf", reason = "to draw the locator inset")
  ids <- as.character(boundaries[[id_col]])
  keep <- ids %in% as.character(c(foc_id, nb_ids))
  if (!as.character(foc_id) %in% ids[keep]) return("")
  sub <- boundaries[keep, , drop = FALSE]
  geom <- sf::st_geometry(sub)

  allm <- sf::st_coordinates(geom)
  xr <- range(allm[, "X"], na.rm = TRUE)
  yr <- range(allm[, "Y"], na.rm = TRUE)
  span <- max(diff(xr), diff(yr))
  if (!is.finite(span) || span == 0) return("")
  scale <- (size - 2 * pad) / span
  ox <- (size - diff(xr) * scale) / 2
  oy <- (size - diff(yr) * scale) / 2
  tx <- function(x) ox + (x - xr[1]) * scale
  ty <- function(y) oy + (yr[2] - y) * scale

  ring_polys <- function(i, fill, stroke, sw) {
    m <- sf::st_coordinates(geom[i])
    lcols <- setdiff(colnames(m), c("X", "Y"))
    key <- do.call(paste, c(as.data.frame(m[, lcols, drop = FALSE]), sep = "-"))
    parts <- lapply(split(seq_len(nrow(m)), key), function(idx) {
      pts <- paste(
        sprintf("%.1f,%.1f", tx(m[idx, "X"]), ty(m[idx, "Y"])),
        collapse = " "
      )
      sprintf(
        paste0("<polygon points=\"%s\" fill=\"%s\" stroke=\"%s\" ",
               "stroke-width=\"%s\" stroke-linejoin=\"round\"/>"),
        pts, fill, stroke, sw
      )
    })
    paste(unlist(parts), collapse = "")
  }

  sub_ids <- as.character(sub[[id_col]])
  focal_i <- which(sub_ids == as.character(foc_id))
  nb_i <- setdiff(seq_len(nrow(sub)), focal_i)

  s <- sprintf(
    "<svg viewBox=\"0 0 %d %d\" xmlns=\"http://www.w3.org/2000/svg\">",
    size, size
  )
  for (i in nb_i) s <- paste0(s, ring_polys(i, "#eef0f4", "#b7bfcc", "0.7"))
  for (i in focal_i) s <- paste0(s, ring_polys(i, accent, "#fffdf8", "1"))
  paste0(s, "</svg>")
}

# naive english pluraliser for the unit noun in the legend (district ->
# districts, province -> provinces, county -> counties).
# @noRd
.pager_plural <- function(x) {
  if (grepl("[^aeiou]y$", x)) sub("y$", "ies", x) else paste0(x, "s")
}

# format a number with the pager's middle-dot decimal separator.
# @noRd
.pager_dot <- function(x, digits = 2) {
  gsub(".", "\u00b7", formatC(x, format = "f", digits = digits), fixed = TRUE)
}

# parse a cumulative "2021, 2022" detection-year string to integers.
# @noRd
.pager_parse_years <- function(years_str) {
  s <- years_str %||% ""
  if (!nzchar(s)) return(integer(0))
  as.integer(strsplit(s, ",\\s*")[[1]])
}

# environmental-surveillance positive years for one district, up to `year`.
# @noRd
.pager_es_years <- function(es, es_col, foc_id, id_col, year) {
  if (is.null(es)) return(integer(0))
  stopifnot(is.data.frame(es), id_col %in% names(es), "year" %in% names(es))
  g <- es[as.character(es[[id_col]]) == as.character(foc_id), , drop = FALSE]
  if (!is.null(es_col)) {
    if (!es_col %in% names(g)) {
      cli::cli_abort("{.arg es_col} {.val {es_col}} not in {.arg es}.")
    }
    v <- g[[es_col]]
    keep <- if (is.logical(v)) v %in% TRUE else as.numeric(v) > 0
    g <- g[keep, , drop = FALSE]
  }
  yrs <- sort(unique(as.integer(g$year)))
  yrs[yrs <= year]
}

# out-of-grid / banner detection clause, e.g. "cVDPV2 detected in AFP (2021,
# 2023) and ES (2022, 2024)"; "" when neither channel has a detection.
# @noRd
.pager_detection_phrase <- function(detections, label) {
  afp <- detections$afp
  es <- detections$es
  parts <- character(0)
  if (length(afp) > 0) {
    parts <- c(parts, sprintf("AFP (%s)", paste(afp, collapse = ", ")))
  }
  if (length(es) > 0) {
    parts <- c(parts, sprintf("ES (%s)", paste(es, collapse = ", ")))
  }
  if (length(parts) == 0) return("")
  sprintf("%s detected in %s", label, paste(parts, collapse = " and "))
}

# verdict-driven accent colour, tag text and action for the masthead / banner.
# @noRd
.pager_verdict_style <- function(focal, corroborated = FALSE) {
  verdict <- as.character(focal$verdict)
  switch(
    verdict,
    FLAG = list(
      verdict = "FLAG",
      accent = "#c8102e",
      tag = if (corroborated) "Flag \u00b7 corroborated" else "Flag",
      priority = "priority \u00b7 review and search",
      action = c("Supervisory review", "active case search")
    ),
    WATCH = list(
      verdict = "WATCH",
      accent = "#e87722",
      tag = "Watch",
      priority = "hold \u00b7 collect another year",
      action = c("Watch", "collect another year")
    ),
    # No action splits: a genuinely adequate district (green, "Adequate") vs a
    # sub-threshold district that simply lacked corroboration to flag (neutral
    # slate, "No action") -- so green never overclaims adequacy below the cut
    if (isTRUE(focal$spi_below)) {
      list(
        verdict = "No action",
        accent = "#5a6883",
        tag = "No action",
        priority = "sub-threshold \u00b7 monitor",
        action = c("No action", "routine monitoring")
      )
    } else {
      list(
        verdict = "No action",
        accent = "#1f6f43",
        tag = "Adequate",
        priority = "routine \u00b7 no action",
        action = c("No action", "routine monitoring")
      )
    }
  )
}

# ---- chart (inline svg, ported geometry) ----------------------------------

# render the SPI-over-time chart as a static inline SVG string.
# @noRd
.pager_svg <- function(series, focal, neighbours, accent, spi_cut, detections,
                       name_col) {
  years <- series$year
  n <- length(years)
  focal_series <- series$spi_median
  lo <- series$spi_q05
  hi <- series$spi_q95
  afp <- detections$afp
  es <- detections$es

  nb_all <- unlist(lapply(neighbours, function(z) z$series))
  line_top <- max(c(focal_series, nb_all, 1.0), na.rm = TRUE)
  ci_top <- max(c(hi, 1.0), na.rm = TRUE)
  # flexible axis: always fit the focal and neighbour lines with headroom (the
  # real clipping risk), and let the CI ribbon extend the panel too -- but cap
  # its reach at ~1.5x the line range so one very wide small-denominator
  # interval cannot blow the axis out and squash the operational zone. The
  # ribbon clips at the frame top in that rare case.
  y_max <- max(1.25, line_top * 1.08, min(ci_top, line_top * 1.5) * 1.02)

  w <- 734
  h <- 400
  ml <- 44
  mr <- 138
  mt <- 22
  mb <- 34
  px <- function(i) ml + (i / (n - 1)) * (w - ml - mr)
  py <- function(v) mt + (1 - v / y_max) * (h - mt - mb)

  fnum <- function(v) sprintf("%.1f", v)
  poly <- function(vals) {
    keep <- is.finite(vals)
    pts <- vapply(
      which(keep),
      function(i) paste0(fnum(px(i - 1)), ",", fnum(py(min(vals[i], y_max)))),
      character(1)
    )
    paste(pts, collapse = " ")
  }

  s <- paste0(
    "<svg viewBox=\"0 0 ", w, " ", h, "\" width=\"100%\" height=\"", h,
    "\" preserveAspectRatio=\"xMidYMid meet\">"
  )

  # gridlines every 0.5 (every 1.0 for very tall panels), 1.0 emphasised
  grid_step <- if (y_max <= 3) 0.5 else 1.0
  grid <- seq(0, floor(y_max / grid_step + 1e-9) * grid_step, by = grid_step)
  for (v in grid) {
    y <- py(v)
    strong <- v == 1.0
    s <- paste0(
      s, "<line x1=\"", ml, "\" y1=\"", fnum(y), "\" x2=\"", w - mr,
      "\" y2=\"", fnum(y), "\" stroke=\"",
      if (strong) "#15233b" else "#e4ddd0", "\" stroke-width=\"",
      if (strong) "1.2" else "0.8", "\"",
      if (strong) " stroke-dasharray=\"5 3\"" else "", " opacity=\"",
      if (strong) "0.65" else "0.9", "\"/>",
      "<text x=\"", ml - 8, "\" y=\"", fnum(y + 3),
      "\" text-anchor=\"end\" font-family=\"Spline Sans Mono\" ",
      "font-size=\"9\" fill=\"#5a6883\">", .pager_dot(v, 1), "</text>"
    )
  }

  # focal 90% credible-interval ribbon (real per-year q05-q95)
  up <- vapply(
    seq_len(n),
    function(i) paste0(fnum(px(i - 1)), ",", fnum(py(min(hi[i], y_max)))),
    character(1)
  )
  dn <- rev(vapply(
    seq_len(n),
    function(i) paste0(fnum(px(i - 1)), ",", fnum(py(lo[i]))),
    character(1)
  ))
  s <- paste0(
    s, "<path d=\"M ", paste(up, collapse = " L "), " L ",
    paste(dn, collapse = " L "), " Z\" fill=\"", accent,
    "\" fill-opacity=\"0.12\" stroke=\"none\"/>"
  )

  # neighbour lines (muted, thin)
  for (z in neighbours) {
    s <- paste0(
      s, "<polyline points=\"", poly(z$series),
      "\" fill=\"none\" stroke=\"#94a0b3\" stroke-width=\"1.3\" ",
      "opacity=\"0.75\"/>"
    )
  }

  # plot frame
  s <- paste0(
    s, "<rect x=\"", ml, "\" y=\"", mt, "\" width=\"", w - mr - ml,
    "\" height=\"", h - mb - mt,
    "\" fill=\"none\" stroke=\"#15233b\" stroke-width=\"1.6\"/>"
  )

  # x-axis year labels
  for (i in seq_len(n)) {
    s <- paste0(
      s, "<text x=\"", fnum(px(i - 1)), "\" y=\"", h - 14,
      "\" text-anchor=\"middle\" font-family=\"Spline Sans Mono\" ",
      "font-size=\"8.5\" fill=\"#5a6883\">'",
      substr(as.character(years[i]), 3, 4), "</text>"
    )
  }

  # neighbour cluster label, anchored to their last finite mean
  if (length(neighbours) > 0) {
    last_vals <- vapply(
      neighbours,
      function(z) z$series[max(which(is.finite(z$series)))],
      numeric(1)
    )
    ny <- py(stats::median(last_vals, na.rm = TRUE))
    s <- paste0(
      s, "<text x=\"", fnum(w - mr + 8), "\" y=\"", fnum(ny + 3),
      "\" font-family=\"Spline Sans Mono\" font-size=\"9\" ",
      "fill=\"#94a0b3\">neighbours</text>"
    )
  }

  # focal line, then detection markers (ES rings under AFP diamonds),
  # endpoint + label
  s <- paste0(
    s, "<polyline points=\"", poly(focal_series), "\" fill=\"none\" stroke=\"",
    accent, "\" stroke-width=\"3.2\" stroke-linejoin=\"round\" ",
    "stroke-linecap=\"round\"/>"
  )
  marker_xy <- function(yr) {
    i <- match(yr, years)
    if (is.na(i)) return(NULL)
    c(px(i - 1), py(min(focal_series[i], y_max)))
  }
  # ES positives: hollow ring
  for (yr in es) {
    p <- marker_xy(yr)
    if (is.null(p)) next
    s <- paste0(
      s, "<circle cx=\"", fnum(p[1]), "\" cy=\"", fnum(p[2]),
      "\" r=\"4\" fill=\"#fffdf8\" stroke=\"", accent, "\" ",
      "stroke-width=\"1.6\"/>"
    )
  }
  # AFP genomic positives: filled diamond
  for (yr in afp) {
    p <- marker_xy(yr)
    if (is.null(p)) next
    s <- paste0(
      s, "<rect x=\"", fnum(p[1] - 3.6), "\" y=\"", fnum(p[2] - 3.6),
      "\" width=\"7.2\" height=\"7.2\" fill=\"", accent,
      "\" stroke=\"#fffdf8\" stroke-width=\"1.2\" transform=\"rotate(45 ",
      fnum(p[1]), " ", fnum(p[2]), ")\"/>"
    )
  }
  ex <- px(n - 1)
  ey <- py(focal_series[n])
  # endpoint label: peel a trailing "(qualifier)" down to the mono sub-line and
  # shrink the main name to fit the right margin, so a long name like
  # "TURWO (TARWE)" neither collides with the SPI value nor runs off the frame
  lbl <- as.character(focal[[name_col]] %||% "")
  paren <- regmatches(lbl, regexec("^(.*?)\\s*\\(([^)]*)\\)\\s*$", lbl))[[1]]
  main <- if (length(paren) == 3L) paren[2] else lbl
  qual <- if (length(paren) == 3L) sprintf("(%s) \u00b7 ", paren[3]) else ""
  avail <- (w - 4) - (ex + 10)
  name_size <- max(9, min(14.5, avail / (0.62 * max(nchar(main), 1L))))
  s <- paste0(
    s, "<circle cx=\"", fnum(ex), "\" cy=\"", fnum(ey),
    "\" r=\"3.6\" fill=\"", accent, "\" stroke=\"#fffdf8\" ",
    "stroke-width=\"1.6\"/>",
    "<text x=\"", fnum(ex + 10), "\" y=\"", fnum(ey + 2),
    "\" font-family=\"Archivo\" font-weight=\"900\" font-size=\"",
    fnum(name_size), "\" fill=\"", accent, "\">", .pager_escape(main),
    "</text>",
    "<text x=\"", fnum(ex + 10), "\" y=\"", fnum(ey + 15),
    "\" font-family=\"Spline Sans Mono\" font-size=\"9.5\" fill=\"", accent,
    "\">", .pager_escape(qual), "SPI ", .pager_dot(focal$spi_median, 2),
    "</text></svg>"
  )
  s
}

# ---- STEPS reading rows ---------------------------------------------------

# build the five STEPS rows plus the out-of-grid season/detection row (code,
# name, reading, role) for the focal year.
# @noRd
.pager_signals <- function(
  focal, spi_cut, params, detections, detection_label, unit_noun = "district"
) {
  r <- as.list(focal)
  spi <- .pager_dot(r$spi_median, 2)
  q05 <- .pager_dot(r$spi_q05, 2)
  q95 <- .pager_dot(r$spi_q95, 2)
  npafp <- .pager_dot(r$npafp_rate, 1)
  run <- as.integer(r$longest_run_below)
  obs <- as.integer(round(r$observed))
  exp <- round(r$expected_total)
  nb <- if (is.na(r$neighbour_spi)) NA else .pager_dot(r$neighbour_spi, 2)
  win <- as.integer(params$traj_window)
  persistence <- as.integer(params$persistence)

  s1 <- if (isTRUE(r$s1_discordance)) {
    sprintf(
      paste0("Rate %s is adequate, yet SPI %s with a 90%% interval whose ",
             "upper bound (%s) excludes one."),
      npafp, spi, q95
    )
  } else if (!isTRUE(r$spi_below)) {
    sprintf("SPI %s sits at or above the adequacy cut; the gate stays shut.",
            spi)
  } else if (!isTRUE(r$cri_excludes_1)) {
    sprintf(
      paste0("SPI %s sits below the cut, but its 90%% interval (%s to %s) ",
             "still includes one."),
      spi, q05, q95
    )
  } else {
    sprintf(
      paste0("SPI %s with its 90%% interval (%s to %s) wholly below one is a ",
             "credible shortfall; the conventional rate is also short, so not ",
             "the special discordance case."),
      spi, q05, q95
    )
  }

  # extent (E) merges depth of shortfall and observed-vs-expected into one row
  depth <- spi_cut - r$spi_median
  extent_depth <- if (!isTRUE(r$spi_below)) {
    "at or above the adequacy cut"
  } else if (depth >= 0.15) {
    "a deep rather than borderline shortfall"
  } else if (depth <= 0.05) {
    "a borderline shortfall"
  } else {
    "a moderate shortfall"
  }
  extent <- sprintf(
    paste0("A posterior median SPI of %s, %s: about %d case%s detected ",
           "against roughly %d expected for the %s."),
    spi, extent_depth, obs, if (obs == 1L) "" else "s", as.integer(exp),
    unit_noun
  )

  s4 <- switch(
    r$trajectory,
    falling = sprintf("Detection has fallen over the last %d years.", win),
    rising = sprintf("Detection has recovered over the last %d years.", win),
    sprintf("Detection has held roughly flat over the last %d years.", win)
  )

  s5 <- if (run == 0L) {
    "No consecutive run below the adequacy cut."
  } else {
    sprintf("%d consecutive year%s reading below expectation.", run,
            if (run == 1L) "" else "s")
  }

  s6 <- if (isTRUE(r$island)) {
    sprintf("No adjacent %s to compare against.", unit_noun)
  } else if (is.na(r$neighbour_spi)) {
    "Neighbours not assessable."
  } else if (isTRUE(r$neighbour_discordant)) {
    sprintf(
      paste0("%s against a neighbour median of %s, so the gap is local ",
             "rather than regional."),
      spi, nb
    )
  } else if (isTRUE(r$spi_below)) {
    sprintf(
      paste0("%s against a neighbour median of %s, itself below the cut: a ",
             "region-wide shortfall rather than a local gap."),
      spi, nb
    )
  } else {
    sprintf(
      paste0("%s against a neighbour median of %s, in line with its ",
             "neighbours."),
      spi, nb
    )
  }

  seasonal_word <- switch(
    r$seasonal %||% "not assessed",
    blind = "Blind through the expected peak",
    muted = "Muted through the expected peak",
    present = "Detects through the expected peak",
    "Season not assessable"
  )
  detection <- .pager_detection_phrase(detections, detection_label)
  s7 <- if (nzchar(detection)) {
    sprintf("%s, with %s.", seasonal_word, detection)
  } else {
    sprintf("%s, with no poliovirus detected there.", seasonal_word)
  }

  falling <- r$trajectory == "falling"
  persistent <- run >= persistence
  discordant <- isTRUE(r$neighbour_discordant)
  # seasonal blindness and any detection are out-of-grid: never one of the
  # three STEPS corroborators. a detection is counted only when the guide was
  # built with detection_corroborates, otherwise it corroborates from outside
  # the grid and is flagged "external", so a reader counting the corroborating
  # chips does not over-count against the flag rule
  detected <- length(detections$afp) > 0 || length(detections$es) > 0
  detection_counts <- isTRUE(params$detection_corroborates) && detected
  det_role <- if (detection_counts) {
    "corr"
  } else if (detected) {
    "external"
  } else {
    "quiet"
  }
  det_label <- if (detection_counts) {
    "corroborates"
  } else if (detected) {
    "external"
  } else {
    "quiet"
  }

  list(
    list(code = "S", name = "Significance", reading = s1,
         role = "gate", label = "gate"),
    list(code = "T", name = "Trend", reading = s4,
         role = if (falling) "corr" else "quiet",
         label = if (falling) "corroborates" else "quiet"),
    list(code = "E", name = "Extent", reading = extent,
         role = "mag", label = "magnitude"),
    list(code = "P", name = "Persistence", reading = s5,
         role = if (persistent) "supp" else "quiet",
         label = if (persistent) "supports" else "quiet"),
    list(code = "S", name = "Surroundings", reading = s6,
         role = if (discordant) "corr" else "quiet",
         label = if (discordant) "corroborates" else "quiet"),
    list(code = "+", name = "Seasonal & detections", reading = s7,
         role = det_role, label = det_label)
  )
}

# ---- page assembly --------------------------------------------------------

# minimal html-escape for interpolated text nodes.
# @noRd
.pager_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# one-sentence caption describing the reading for this verdict.
# @noRd
.pager_caption <- function(focal, name) {
  verdict <- as.character(focal$verdict)
  discordant <- isTRUE(focal$neighbour_discordant)
  if (verdict == "FLAG") {
    tail <- if (discordant) {
      " and the gap widens while its neighbours read adequately"
    } else {
      ""
    }
    sprintf(
      paste0("%s detects fewer non-polio AFP cases than its size, place and ",
             "season lead the model to expect%s. Its 90%% credible interval ",
             "sits wholly below the expected level, so the shortfall is ",
             "unlikely to be noise."),
      name, tail
    )
  } else if (verdict == "WATCH") {
    sprintf(
      paste0("%s reads below the adequacy cut, but its 90%% credible interval ",
             "still reaches the expected level, so another year of data is ",
             "needed before the shortfall can be called real."),
      name
    )
  } else if (isTRUE(focal$spi_below)) {
    sprintf(
      paste0("%s reads below the adequacy cut with a credible shortfall, but ",
             "too few signals corroborate to flag it, so it is kept under ",
             "routine monitoring rather than flagged."),
      name
    )
  } else {
    sprintf(
      paste0("%s detects about as many non-polio AFP cases as the model ",
             "expects for its size, place and season; its SPI sits at or ",
             "above the adequacy cut."),
      name
    )
  }
}

# verdict-banner narrative summarising the fired corroborators.
# @noRd
.pager_banner_reading <- function(focal, spi_cut, params, detections, label) {
  verdict <- as.character(focal$verdict)
  if (verdict != "FLAG") {
    if (verdict == "WATCH") {
      return(paste0(
        "The SPI sits below the cut but its 90% interval still reaches the ",
        "expected level, so the reading is held at watch, not flagged."
      ))
    }
    if (isTRUE(focal$spi_below)) {
      return(paste0(
        "The SPI sits below the cut and its 90% interval excludes one, but ",
        "fewer than the required signals corroborate, so it is held at ",
        "no-action, not flagged."
      ))
    }
    return(paste0(
      "The SPI sits at or above the adequacy cut, with no corroborating ",
      "signal firing; no action is warranted."
    ))
  }
  # concise, bounded summary: the counted corroborators as a count (the STEPS
  # rows name which fired), plus the detection channels shown out-of-grid. the
  # base is three (trend, persistence, surroundings), or four when a detection
  # is promoted to a counted signal, so the "of N" stays honest
  dedupe <- isTRUE(params$dedupe_temporal)
  detection_counts <- isTRUE(params$detection_corroborates)
  n_axes <- (if (dedupe) 1L else 2L) + 1L + (if (detection_counts) 1L else 0L)
  n_corr <- as.integer(focal$corroborators)
  corr <- if (n_corr >= n_axes) {
    sprintf("all %d signals", n_axes)
  } else {
    sprintf("%d of %d signals", n_corr, n_axes)
  }
  channels <- c(
    if (length(detections$afp) > 0) "AFP" else NULL,
    if (length(detections$es) > 0) "ES" else NULL
  )
  detection <- if (length(channels) > 0) {
    sprintf(
      ", with %s detected by %s", label, paste(channels, collapse = " and ")
    )
  } else {
    ""
  }
  sprintf(
    paste0("Both the SPI and its 90%% interval sit below the cut, ",
           "corroborated by %s%s."),
    corr, detection
  )
}

# assemble the full self-contained html document.
# @noRd
.pager_html <- function(
  focal, series, neighbours, chart, locator, signals, vstyle, spi_cut, params,
  detections, detection_label, year, name_col, id_col, admin_label,
  unit_noun = "district", note
) {
  name <- .pager_escape(as.character(focal[[name_col]]))
  adm1 <- .pager_escape(as.character(focal$adm1_name %||% ""))
  adm0 <- .pager_escape(as.character(focal$adm0_name %||% ""))
  # drop any parent level that is the unit itself (adm1 data labels the unit
  # by adm1_name, so it would otherwise repeat in the hierarchy line)
  parent <- paste(
    c(
      adm1[nzchar(adm1) & adm1 != name],
      adm0[nzchar(adm0) & adm0 != name],
      admin_label
    ),
    collapse = " \u00b7 "
  )

  spi <- .pager_dot(focal$spi_median, 2)
  nb_txt <- if (is.na(focal$neighbour_spi)) {
    ""
  } else {
    sprintf(" \u00b7 neighbours %s", .pager_dot(focal$neighbour_spi, 2))
  }

  legend_neighbour <- if (length(neighbours) == 0L) {
    ""
  } else if (length(neighbours) == 1L && neighbours[[1]]$name ==
             "neighbour median") {
    "<span><span class=\"lk n\"></span> neighbour median</span>"
  } else {
    paste0(
      "<span><span class=\"lk n\"></span> neighbouring ",
      .pager_plural(unit_noun), "</span>"
    )
  }

  rows <- vapply(signals, function(s) {
    sprintf(
      paste0("<div class=\"srow\"><div class=\"sc\">%s</div>",
             "<div class=\"sn\">%s</div><div class=\"sr\">%s</div>",
             "<span class=\"role %s\">%s</span></div>"),
      s$code, .pager_escape(s$name), .pager_escape(s$reading), s$role, s$label
    )
  }, character(1))
  rows <- paste(rows, collapse = "")

  # enumerate the actual corroborator axes: the temporal pair collapses to one
  # under dedupe_temporal, and a detection joins the count under
  # detection_corroborates -- so the "of N" and the list stay honest. season
  # never counts; it corroborates from outside the grid
  dedupe <- isTRUE(params$dedupe_temporal)
  detection_counts <- isTRUE(params$detection_corroborates)
  axes <- c(
    if (dedupe) "trend or persistence" else "trend, persistence",
    "surroundings",
    if (detection_counts) "detections" else NULL
  )
  n_axes <- (if (dedupe) 1L else 2L) + 1L + (if (detection_counts) 1L else 0L)
  detect_clause <- if (detection_counts) {
    "."
  } else {
    "; season and detections corroborate from outside this count."
  }
  rule <- sprintf(
    paste0("A %s is flagged when its SPI sits below the %s adequacy cut, its ",
           "90%% credible interval lies wholly below one, and at least %d of ",
           "%d signals corroborate (%s)%s"),
    unit_noun, .pager_dot(spi_cut, 2), as.integer(params$min_corroborators),
    n_axes, paste(axes, collapse = ", "), detect_clause
  )
  caption <- .pager_escape(.pager_caption(focal, name))
  banner_reading <- .pager_escape(
    .pager_banner_reading(focal, spi_cut, params, detections, detection_label)
  )
  action <- paste(vstyle$action, collapse = " \u00b7<br>")
  legend_detect <- paste0(
    if (length(detections$afp) > 0) {
      "<span><span class=\"ld\"></span> AFP detection</span>"
    } else {
      ""
    },
    if (length(detections$es) > 0) {
      "<span><span class=\"lo\"></span> ES detection</span>"
    } else {
      ""
    }
  )
  eyebrow <- sprintf("blindspot \u00b7 SPI reading \u00b7 %s %d \u00b7 %s",
                     adm0, year, note)

  paste0(
    .pager_head(name, year, vstyle$accent),
    "<body><div class=\"page\"><div class=\"corridor\"></div>",
    "<div class=\"pad\">",
    # masthead
    "<div class=\"mast\"><div>",
    "<div class=\"eyebrow\">", eyebrow, "</div>",
    "<div class=\"unit\">", name, "</div>",
    "<div class=\"parent\">", parent, "</div></div>",
    "<div class=\"verdict\"><span class=\"tag\">", vstyle$tag, "</span>",
    "<div class=\"sub\">", vstyle$priority, "<br>SPI <b>", spi, "</b>",
    nb_txt, "</div></div></div>",
    # body
    "<div class=\"body\"><div class=\"cgroup\">",
    "<div class=\"sectlab\"><span>SPI over time \u2014 ", name,
    " against its touching neighbours</span>",
    "<span>observed \u00f7 model-expected non-polio AFP</span></div>",
    "<div class=\"caption\">", caption, "</div>",
    "<div class=\"chartbox\"><div class=\"ts\">", chart, "</div>",
    if (nzchar(locator)) {
      paste0("<div class=\"locbadge\"><div class=\"lbmap\">", locator,
             "</div></div>")
    } else {
      ""
    },
    "</div>",
    "<div class=\"legend\">",
    "<span><span class=\"lk\"></span> ", name, "</span>",
    "<span><span class=\"lband\"></span> 90% credible interval</span>",
    legend_neighbour,
    "<span><span class=\"lk exp\"></span> 1\u00b70, expected detection</span>",
    legend_detect,
    "</div></div>",
    # reading
    "<div class=\"reading\"><div class=\"sectlab\" ",
    "style=\"margin-bottom:6px\"><span>The reading \u2014 the five STEPS for ",
    name, "</span><span>gate \u00b7 magnitude \u00b7 corroboration</span></div>",
    "<div class=\"rrule\">", rule, "</div>",
    "<div>", rows, "</div></div>",
    # verdict banner
    "<div class=\"vbanner\"><div class=\"vt\">", vstyle$tag, "</div>",
    "<div class=\"vw\">", banner_reading, "</div>",
    "<div class=\"va\">", action, "</div></div>",
    "</div>",
    # footer
    "<div class=\"foot\"><div>computed from bs_spi() posterior \u00b7 ", note,
    "</div><div>verify against source systems before operational use</div>",
    "</div></div></div></body></html>"
  )
}

# document head: charset, title and the pager stylesheet, accent-substituted.
# @noRd
.pager_head <- function(name, year, accent) {
  paste0(
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, ",
    "initial-scale=1.0\"><title>", name, " \u2014 SPI reading ", year,
    "</title><style>", .pager_css(accent), "</style></head>"
  )
}

# the pager stylesheet (design tokens + layout), verbatim from the field
# guide's tear-sheet design plus a muted "quiet" role chip. `accent` drives
# the verdict colour (rose flag / amber watch / green no-action).
# @noRd
.pager_css <- function(accent) {
  paste0(
    "@import url('https://fonts.googleapis.com/css2?family=Archivo:wght@400;",
    "500;600;700;800;900&family=Spline+Sans+Mono:wght@400;500;600&family=",
    "Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,500;0,6..72,600;1,6..72,",
    "400;1,6..72,500&display=swap');",
    ":root{--ink:#15233b;--ink-soft:#5a6883;--paper:#fbf7f0;--line:#e4ddd0;",
    "--grid:#efe9dd;--nbr:#94a0b3;--blind:#c8102e;--buildup:#e87722;",
    "--watch:#2f9e8f;--assured:#1f6f43;--accent:", accent, "}",
    "*{margin:0;padding:0;box-sizing:border-box}",
    "body{background:#cfc6b6;font-family:'Archivo',sans-serif;padding:30px;",
    "display:flex;flex-direction:column;align-items:center;color:var(--ink)}",
    ".page{width:794px;height:1123px;background:var(--paper);position:",
    "relative;overflow:hidden;box-shadow:0 30px 70px -25px rgba(21,35,59,.45);",
    "display:flex;flex-direction:column}",
    ".corridor{height:7px;flex-shrink:0;background:linear-gradient(90deg,",
    "var(--assured) 0%,var(--watch) 34%,var(--buildup) 66%,var(--blind) ",
    "100%)}",
    ".pad{padding:20px 30px 15px;flex:1;display:flex;flex-direction:column;",
    "overflow:hidden}",
    ".mast{display:flex;justify-content:space-between;align-items:flex-start;",
    "gap:22px;flex-shrink:0}",
    ".eyebrow{font-family:'Spline Sans Mono',monospace;font-size:10px;",
    "letter-spacing:.2em;text-transform:uppercase;color:var(--ink-soft);",
    "font-weight:500}",
    ".unit{font-size:46px;font-weight:900;letter-spacing:-.025em;",
    "line-height:.9;margin-top:5px}",
    ".parent{font-family:'Newsreader',serif;font-size:14px;color:",
    "var(--ink-soft);font-style:italic;margin-top:6px}",
    ".verdict{text-align:right;flex-shrink:0}",
    ".verdict .tag{display:inline-block;font-family:'Spline Sans Mono',",
    "monospace;font-weight:600;font-size:12px;letter-spacing:.1em;",
    "text-transform:uppercase;color:#fff;padding:7px 13px;background:",
    "var(--accent)}",
    ".verdict .sub{font-family:'Spline Sans Mono',monospace;font-size:10px;",
    "color:var(--ink-soft);margin-top:9px;line-height:1.5}",
    ".verdict .sub b{color:var(--accent)}",
    ".sectlab{font-family:'Spline Sans Mono',monospace;font-size:9.5px;",
    "letter-spacing:.15em;text-transform:uppercase;color:var(--ink-soft);",
    "display:flex;justify-content:space-between;align-items:baseline;",
    "margin:16px 0 4px}",
    ".caption{font-family:'Newsreader',serif;font-size:13px;line-height:1.45;",
    "color:var(--ink);max-width:735px}",
    ".chartbox{flex-shrink:0;margin-top:8px;background:#fffdf8;border:1px ",
    "solid var(--line);padding:10px 12px 6px;position:relative}",
    ".ts{width:100%}.ts svg{width:100%;height:auto;display:block}",
    ".locbadge{position:absolute;top:-12px;right:-11px;width:112px;",
    "height:112px;background:#fffdf8;border:1px solid var(--line);",
    "box-shadow:0 10px 22px -8px rgba(21,35,59,.42);padding:7px;display:flex;",
    "align-items:center}",
    ".locbadge .lbmap{width:100%}",
    ".locbadge .lbmap svg{width:100%;height:auto;display:block}",
    ".legend{display:flex;gap:18px;flex-wrap:wrap;margin-top:8px;font-family:",
    "'Spline Sans Mono',monospace;font-size:9px;color:var(--ink-soft);",
    "align-items:center}",
    ".legend span{display:inline-flex;align-items:center;gap:6px}",
    ".lk{width:20px;height:0;border-top:3px solid var(--accent)}",
    ".lk.n{border-top:1.4px solid var(--nbr)}",
    ".lk.exp{border-top:1.4px dashed var(--ink)}",
    ".lband{width:18px;height:11px;background:var(--accent);opacity:.16;",
    "border-radius:2px}",
    ".ld{width:8px;height:8px;background:var(--accent);",
    "transform:rotate(45deg)}",
    ".lo{width:9px;height:9px;border:1.6px solid var(--accent);",
    "border-radius:50%;box-sizing:border-box}",
    ".body{flex:1;display:flex;flex-direction:column;justify-content:",
    "space-between;min-height:0}",
    ".cgroup{flex-shrink:0}.reading{margin-top:0;flex-shrink:0}",
    ".rrule{font-family:'Newsreader',serif;font-style:italic;font-size:12px;",
    "color:var(--ink-soft);margin-bottom:9px;line-height:1.4}",
    ".srow{display:grid;grid-template-columns:26px 148px 1fr 108px;gap:12px;",
    "align-items:center;padding:7px 0;border-top:1px solid var(--grid)}",
    ".srow .sc{font-family:'Spline Sans Mono',monospace;font-size:11px;",
    "font-weight:600;color:var(--ink)}",
    ".srow .sn{font-size:12px;font-weight:700;letter-spacing:-.01em}",
    ".srow .sr{font-family:'Newsreader',serif;font-size:12.5px;",
    "line-height:1.35;color:var(--ink)}",
    ".role{justify-self:end;font-family:'Spline Sans Mono',monospace;",
    "font-size:8px;letter-spacing:.07em;text-transform:uppercase;padding:3px ",
    "8px;border-radius:10px;white-space:nowrap}",
    ".role.gate{background:var(--ink);color:#fff}",
    ".role.mag{color:var(--ink-soft);border:1px solid var(--line)}",
    ".role.corr{background:var(--accent);color:#fff}",
    ".role.supp{color:var(--accent);border:1px solid var(--accent)}",
    ".role.quiet{color:var(--ink-soft);border:1px solid var(--line);",
    "opacity:.55}",
    ".role.external{color:var(--accent);border:1px dashed var(--accent)}",
    ".vbanner{display:grid;grid-template-columns:auto 1fr auto;gap:16px;",
    "align-items:center;background:var(--accent);color:#fff;padding:9px 15px;",
    "margin-top:9px}",
    ".vbanner .vt{font-family:'Spline Sans Mono',monospace;font-weight:600;",
    "font-size:12px;letter-spacing:.09em;text-transform:uppercase}",
    ".vbanner .vw{font-family:'Newsreader',serif;font-size:12.5px;",
    "line-height:1.4;color:rgba(255,255,255,.9)}",
    ".vbanner .va{font-family:'Spline Sans Mono',monospace;font-size:11px;",
    "font-weight:600;letter-spacing:.02em;text-align:right;line-height:1.4}",
    ".foot{display:flex;justify-content:space-between;align-items:center;",
    "margin-top:10px;padding-top:11px;border-top:1px solid var(--line);",
    "flex-shrink:0;font-family:'Spline Sans Mono',monospace;font-size:9px;",
    "color:var(--ink-soft)}",
    "svg{display:block}"
  )
}
