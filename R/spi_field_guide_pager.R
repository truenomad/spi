#' Render a one-page SPI pager for a single district
#'
#' @description
#' Creates a one-page district report from a [spi_field_guide()] result. It
#' includes the review judgement, an SPI chart with a 90% credible interval
#' and detection markers, the five STEPS components, and a summary of the
#' findings that support the judgement.
#'
#' @details
#' The report is static HTML with an SVG chart. It works offline, using system
#' fonts when Google Fonts is unavailable.
#'
#' The chart shows the district's SPI and any poliovirus detections. The
#' header reports the neighbour median. Pass `boundaries` to add a location
#' map with the district highlighted within its country and admin-1 divisions.
#'
#' Pass `indicators_df` to show the NPAFP rate, stool adequacy, timeliness,
#' and EV rate. Percentages based on fewer than five assessable cases are
#' shown as fractions and are not assessed against a target.
#'
#' AFP detections appear as filled diamonds and ES detections as hollow
#' circles. Seasonality and trend are also shown as context. These findings
#' and the optional indicators do not change the STEPS judgement.
#'
#' The report uses rose for review priority, amber for monitor, and green for
#' no SPI indication.
#'
#' @param x A [spi_field_guide()] result (class `spi_field_guide`).
#' @param district District to profile: either an id (e.g. the admin-2 GUID)
#'   in the id column, or a district name matched (case-insensitively) against
#'   `name_col`. Ids are tried first, so an id is unambiguous.
#' @param adjacency Deprecated and ignored. The locator inset places the focal
#'   district in its country rather than among its neighbours, so the pager no
#'   longer reads a neighbour graph. Default: NULL.
#' @param boundaries Optional sf polygon layer keyed by `id_col`, covering the
#'   whole country. Draws the locator inset from its real geometry. An
#'   `adm1_name` column, if present, supplies the regional outlines; an
#'   `adm0_name` column restricts a multi-country layer to the focal district's
#'   own country. Default: NULL (no inset).
#' @param prob_under Optional posterior probability that observed **non-polio
#'   AFP detection** fell below the model expectation, i.e. `P(SPI < 1)`, as a
#'   single number on the 0 to 1 scale. It is appended to the strength line
#'   as a percentage, with the tails printed as "over 99%" and "under 1%" so
#'   reported precision matches the posterior draws. This probability describes
#'   reporting relative to expectation, not the presence of poliovirus.
#'   Default: NULL (not shown).
#' @param year_label Optional label for the reading year, shown in the masthead
#'   in place of the bare year. Use it when the window is not a calendar year,
#'   e.g. `"rolling year to Apr 2025"`. Default: NULL (the year).
#' @param region Optional regional context for the header, as a named list or
#'   one-row data frame. Recognised fields: `name` (or `region`/`adm1`), `rank`
#'   (or `region_rank`), `n` (or `n_regions`) and `spi` (or `region_spi`). It
#'   renders as a header line reading `region SUD-OUEST`, `SPI 0.70` and
#'   `worst rank 1 of 22`, joined by middle dots. The rank provides context and does not change
#'   the judgement. Default: NULL (no regional line).
#' @param indicators_df Optional district-year panel of conventional AFP and ES
#'   indicators, keyed by `guid` (or `id_col`) and `year`. When supplied, a
#'   strip below the chart shows the non-polio AFP rate over the same years
#'   against its target, plus tiles for whichever of `stool_adequacy_cond_pct`,
#'   `inv_timeliness_pct`, `onset_notify_pct` and `ev_rate` are present. Pass
#'   the assessable counts (`inv_timeliness_n`, `onset_notify_n`) and they are
#'   shown under each figure; a percentage on fewer than five cases prints as a
#'   fraction instead, since a district below expectation often has only a
#'   handful. These indicators are context: they never change the judgement.
#'   Default NULL (no strip).
#' @param npafp_target Numeric non-polio AFP rate target for the strip's rate
#'   panel, per 100,000 under 15. Default: NULL (`x$thresholds$npafp`).
#' @param es Optional ad-hoc environmental-surveillance detections keyed by
#'   `id_col` and `year`, overriding the field guide's own ES channel. Normally
#'   ES is supplied once to [spi_field_guide()] (via its `es` argument) and read
#'   from there, so this is only needed for a field guide built without ES.
#'   Each row (or each `es_col`-positive row) is an ES positive. Default: NULL
#'   (use the field guide's `es_years`).
#' @param es_col Optional name of a count / logical column in `es`; only rows
#'   with a positive count (or `TRUE`) count as detections. Default: NULL
#'   (every row counts).
#' @param detection_label What the detections in `x` and `es` actually are,
#'   e.g. `"cVDPV2"` or `"WPV1"`. Use this to name the serotype selected in `genomic` / `es`
#'   when building the guide. Leave it NULL for mixed serotypes; the report
#'   then uses "poliovirus". Default: NULL.
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
#'   on-page reading (e.g. "other `unit_noun`s in", "STEPS is applied to a
#'   `unit_noun`..."). Set it to match `admin_label`, e.g. `"province"` for
#'   admin-1 inputs. Default: `"district"`.
#' @param note Optional provenance tag printed in the footer, e.g.
#'   `"illustrative"` for a worked example. Default: NULL (untagged).
#' @param verbose Logical. Emit a cli summary on build. Default: TRUE.
#'
#' @return An object of class `spi_pager`: a list with the rendered
#'   `html` string, the resolved `district`, `year`, `verdict` (the review
#'   judgement), and `paths` (the files written, empty when neither `path` nor
#'   `file` is set). Printing it reports the judgement and any written files. Recover the markup with
#'   `as.character()`.
#'
#' @seealso [spi_field_guide()] for the reading and [spi_field_guide_table()]
#'   for the multi-district table.
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' fg <- synth_field_guide
#' pager <- spi_field_guide_pager(fg, district = "Tirwen")
#' pager
#' \dontrun{
#' # Add a location map and the AFP / ES detections from the field guide.
#' spi_field_guide_pager(
#'   fg,
#'   district = "Tirwen",
#'   boundaries = synth_surveillance$boundaries,
#'   id_col = "adm2_guid",
#'   path = "."
#' )
#' }
spi_field_guide_pager <- function(
  x,
  district,
  adjacency = NULL,
  boundaries = NULL,
  region = NULL,
  year_label = NULL,
  prob_under = NULL,
  indicators_df = NULL,
  npafp_target = NULL,
  es = NULL,
  es_col = NULL,
  detection_label = NULL,
  year = NULL,
  path = NULL,
  file = NULL,
  format = c("html", "png"),
  name_col = NULL,
  id_col = NULL,
  admin_label = "admin-2 district",
  unit_noun = "district",
  note = NULL,
  verbose = TRUE
) {
  .check_pkg(c("dplyr", "cli"), reason = "to render the SPI pager")
  stopifnot(inherits(x, "spi_field_guide"))

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

  # the locator inset places the focal district in its country and draws no
  # neighbour tier, so a neighbour graph is no longer read here at all
  if (!is.null(adjacency) && verbose) {
    cli::cli_alert_info(
      "{.arg adjacency} no longer affects the pager and can be dropped; \\
       the locator inset is drawn from {.arg boundaries}."
    )
  }

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
  npafp_target <- npafp_target %||% x$thresholds$npafp %||% 3
  detection_label <- .pager_detection_label(detection_label)
  indicators <- .pager_indicators(
    indicators_df, foc_id, id_col, year, series$year
  )
  vstyle <- .pager_verdict_style(focal)
  # built before the chart: the badge overlays the chart's top-right corner, so
  # the endpoint label has to know whether it is there
  locator <- if (!is.null(boundaries)) {
    .pager_locator_svg(boundaries, foc_id, id_col, vstyle$accent)
  } else {
    ""
  }
  chart <- .pager_svg(
    series, focal, vstyle$accent, spi_cut, detections, name_col,
    has_locator = nzchar(locator), compact = !is.null(indicators)
  )
  # an ad-hoc `es` argument supplies the ES channel even when the guide lacked
  # it, so mark it live before the reading distinguishes silent from unsupplied
  active <- x$signals_active
  if (!is.null(es) && !is.null(active) && "detect_es" %in% names(active)) {
    active[["detect_es"]] <- TRUE
  }
  signals <- .pager_signals(
    focal, series, spi_cut, x$params, detections, detection_label, unit_noun,
    active, prob_under = prob_under
  )

  html <- .pager_html(
    focal = focal,
    series = series,
    chart = chart,
    indicators = indicators,
    npafp_target = npafp_target,
    active = active,
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
    note = note,
    region = region,
    year_label = year_label
  )

  targets <- .pager_targets(focal, name_col, path, file, format)
  for (t in targets) .pager_write(html, t)

  if (verbose) {
    cli::cli_alert_success(
      "Pager for {.val {focal[[name_col]]}} ({year}): \\
       {.val {as.character(focal$verdict)}}."
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
    class = "spi_pager"
  )
}

#' @export
print.spi_pager <- function(x, ...) {
  cli::cli_h1("SPI pager")
  cli::cli_inform(
    "{.strong {x$district}} | {x$year} | {.val {x$verdict}}"
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
as.character.spi_pager <- function(x, ...) {
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
  # skip the parent level that *is* the unit rather than de-duplicating slugs:
  # an adm2 sharing its parent's name is a different unit and needs a different
  # file, or the two pagers collide the moment they share an output folder
  parts <- c(
    if (identical(name_col, "adm0_name")) NA else focal[["adm0_name"]] %||% NA,
    if (identical(name_col, "adm1_name")) NA else focal[["adm1_name"]] %||% NA,
    focal[[name_col]] %||% NA
  )
  parts <- .pager_slug(parts[!is.na(parts) & nzchar(parts)])
  parts <- parts[nzchar(parts)]
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

# locator inset: the focal district in accent, placed in the whole country so a
# reader can see where in it the district sits. adm1 outlines carry the regional
# context. There is deliberately no neighbour tier: at badge scale a district is
# a few pixels across, so a second tone would not read, and the neighbour
# comparison is already on the page as a figure in the masthead and in the
# surroundings row.
# @noRd
.pager_locator_svg <- function(boundaries, foc_id, id_col, accent,
                               size = 130, pad = 5) {
  .check_pkg("sf", reason = "to draw the locator inset")
  ids <- as.character(boundaries[[id_col]])
  if (!as.character(foc_id) %in% ids) return("")
  # a layer spanning several countries would otherwise scale the inset to all of
  # them, shrinking the one country the reader needs
  if ("adm0_name" %in% names(boundaries)) {
    adm0 <- as.character(boundaries$adm0_name)
    boundaries <- boundaries[
      !is.na(adm0) & adm0 == adm0[match(as.character(foc_id), ids)], ,
      drop = FALSE
    ]
    ids <- as.character(boundaries[[id_col]])
  }
  focal_i <- which(ids == as.character(foc_id))
  # the inset is a flat drawing projected into a square viewBox, so planar is
  # what is wanted; dropping the CRS says so and spares the reader an
  # "assumes that they are planar" message per admin-1 union
  geom <- sf::st_set_crs(sf::st_geometry(boundaries), NA)

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

  # the regional tier: admin-1 where the layer names it, else the raw districts.
  # 30-odd provinces read at this size where several hundred districts blur.
  tier <- if ("adm1_name" %in% names(boundaries)) {
    lapply(
      split(seq_along(geom), as.character(boundaries$adm1_name)),
      function(i) sf::st_union(geom[i])
    )
  } else {
    lapply(seq_along(geom), function(i) geom[i])
  }

  s <- sprintf(
    "<svg viewBox=\"0 0 %d %d\" xmlns=\"http://www.w3.org/2000/svg\">",
    size, size
  )
  # country silhouette, the regional hairlines over it, then the focal district
  s <- paste0(
    s,
    .pager_rings(sf::st_union(geom), tx, ty, "#eef0f4", "#8e99ab", "0.8")
  )
  for (g in tier) {
    s <- paste0(s, .pager_rings(g, tx, ty, "none", "#c3cad6", "0.35"))
  }
  paste0(
    s,
    .pager_rings(geom[focal_i], tx, ty, accent, "#fffdf8", "0.8"),
    .pager_focal_halo(geom[focal_i], tx, ty, accent),
    "</svg>"
  )
}

# one geometry as svg polygons, one per ring, projected through `tx` / `ty`.
# @noRd
.pager_rings <- function(g, tx, ty, fill, stroke, sw) {
  m <- sf::st_coordinates(g)
  if (nrow(m) == 0L) return("")
  lcols <- setdiff(colnames(m), c("X", "Y"))
  key <- if (length(lcols) == 0L) {
    rep("1", nrow(m))
  } else {
    do.call(paste, c(as.data.frame(m[, lcols, drop = FALSE]), sep = "-"))
  }
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

# ring around the focal district, so a district only a few pixels across at
# country scale is still findable. sized off the district's own extent so it
# always encloses it, with a floor that keeps a tiny district visible.
# @noRd
.pager_focal_halo <- function(g, tx, ty, accent) {
  m <- sf::st_coordinates(g)
  if (nrow(m) == 0L) return("")
  px <- tx(m[, "X"])
  py <- ty(m[, "Y"])
  r <- max(6, max(diff(range(px)), diff(range(py))) / 2 + 3)
  sprintf(
    paste0("<circle cx=\"%.1f\" cy=\"%.1f\" r=\"%.1f\" fill=\"none\" ",
           "stroke=\"%s\" stroke-width=\"1.1\" stroke-opacity=\"0.7\"/>"),
    mean(range(px)), mean(range(py)), r, accent
  )
}

# format a number with the pager's middle-dot decimal separator.
# @noRd
.pager_dot <- function(x, digits = 2) {
  gsub(".", "\u00b7", formatC(x, format = "f", digits = digits), fixed = TRUE)
}

# parse a cumulative "2021, 2022" detection-year string to integers. an NA
# string means the channel was never supplied, not that a detection happened in
# an unknown year, so it must parse to no detections rather than to NA.
# @noRd
.pager_parse_years <- function(years_str) {
  s <- years_str %||% ""
  if (length(s) != 1L || is.na(s) || !nzchar(s)) return(integer(0))
  yrs <- suppressWarnings(as.integer(strsplit(s, ",\\s*")[[1]]))
  yrs[!is.na(yrs)]
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

# judgement-driven accent colour, tag text and state line for the masthead.
# the state line describes where the reading sits against the field guide's
# descriptions; the pager prescribes no follow-up, so no action strings are
# carried.
# @noRd
.pager_verdict_style <- function(focal) {
  verdict <- as.character(focal$verdict)
  switch(
    verdict,
    "Review priority" = list(
      verdict = verdict,
      accent = "#c8102e",
      tag = "Review priority",
      state = "below cut \u00b7 interval below 1 \u00b7 corroborated"
    ),
    "Monitor" = list(
      verdict = verdict,
      accent = "#e87722",
      tag = "Monitor",
      # a district is held at monitor for one of three reasons, and the page
      # prints the interval bounds a few lines down, so name the right one
      state = if (!isTRUE(focal$cri_excludes_1)) {
        "below cut \u00b7 interval includes 1"
      } else if (!.fg_gate_pass(focal)) {
        "below cut \u00b7 noise not ruled out"
      } else {
        "below cut \u00b7 not corroborated"
      }
    ),
    # at or above the cut. It still splits, so green never overclaims on a
    # district whose interval sits below 1 under a cut set below 1
    if (.pager_short_of_expectation(focal)) {
      list(
        verdict = verdict,
        accent = "#5a6883",
        tag = "No SPI indication",
        state = "at cut \u00b7 interval below 1"
      )
    } else {
      list(
        verdict = verdict,
        accent = "#1f6f43",
        tag = "No SPI indication",
        state = "at or above cut"
      )
    }
  )
}

# ---- chart (inline svg, ported geometry) ----------------------------------

# lowest chart-space y the locator badge reaches, in viewBox units: the badge is
# 130px square and hangs 12px above the chart box, whose 10px top padding the
# svg starts below, and the 734-unit viewBox renders about 708px wide. anything
# above this would print under the badge.
# @noRd
.pager_locator_band <- 130

# render the SPI-over-time chart as a static inline SVG string.
# @noRd
.pager_svg <- function(series, focal, accent, spi_cut, detections, name_col,
                       has_locator = FALSE, compact = FALSE) {
  years <- series$year
  n <- length(years)
  focal_series <- series$spi_median
  lo <- series$spi_q05
  hi <- series$spi_q95
  afp <- detections$afp
  es <- detections$es

  line_top <- max(c(focal_series, 1.0), na.rm = TRUE)
  ci_top <- max(c(hi, 1.0), na.rm = TRUE)
  # flexible axis: always fit the focal line with headroom (the real clipping
  # risk), and let the CI ribbon extend the panel too -- but cap its reach at
  # ~1.5x the line range so one very wide small-denominator interval cannot
  # blow the axis out and squash the operational zone. The ribbon clips at the
  # frame top in that rare case.
  y_max <- max(1.25, line_top * 1.08, min(ci_top, line_top * 1.5) * 1.02)

  w <- 734
  # the indicator strip takes about a hundred pixels below the chart, and the
  # page is a fixed A4, so the panel gives that back rather than overflowing
  h <- if (compact) 322 else 400
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
  fit <- .pager_label_lines(main, avail)
  name_size <- fit$size
  name_lines <- fit$lines
  lh <- name_size * 1.05
  extra <- (length(name_lines) - 1L) * lh
  # the locator badge is absolutely positioned over the top-right of the chart
  # box, so an endpoint high in the panel would print its label underneath it.
  # drop the label clear of that band; the dot stays on the endpoint.
  ly <- if (has_locator) max(ey, .pager_locator_band) else ey
  # a wrapped name pushes the SPI sub-line down, so an endpoint low in the
  # panel would print it over the x-axis. hold the whole block off the frame
  # foot rather than only its first line.
  ly <- min(ly, h - mb - 15 - extra)
  s <- paste0(
    s, "<circle cx=\"", fnum(ex), "\" cy=\"", fnum(ey),
    "\" r=\"3.6\" fill=\"", accent, "\" stroke=\"#fffdf8\" ",
    "stroke-width=\"1.6\"/>",
    paste(
      vapply(
        seq_along(name_lines),
        function(i) {
          paste0(
            "<text x=\"", fnum(ex + 10), "\" y=\"",
            fnum(ly + 2 + (i - 1L) * lh),
            "\" font-family=\"Archivo\" font-weight=\"900\" font-size=\"",
            fnum(name_size), "\" fill=\"", accent, "\">",
            .pager_escape(name_lines[i]), "</text>"
          )
        },
        character(1)
      ),
      collapse = ""
    ),
    "<text x=\"", fnum(ex + 10), "\" y=\"", fnum(ly + 15 + extra),
    "\" font-family=\"Spline Sans Mono\" font-size=\"9.5\" fill=\"", accent,
    "\">", .pager_escape(qual), "SPI ", .pager_dot(focal$spi_median, 2),
    "</text></svg>"
  )
  s
}

# the masthead is a flex row whose verdict column is flex-shrink:0, so a long
# unit name pushes that column past the page edge instead of wrapping and the
# sub-lines silently lose their last characters. Give the name only what the
# row can spare. The 734 is the pad's content width and 22 the flex gap, both
# set in the stylesheet; the ratios are per-character advance in em, and both
# run a little high so the estimate errs towards a smaller name rather than a
# clipped verdict.
# @noRd
.pager_unit_size <- function(name, tag, sub_lines, max_size = 46) {
  # the tag is a padded pill, the sub-lines are plain mono text
  tag_w <- nchar(tag) * (12 * 0.65 + 1.2) + 26
  sub_w <- max(c(nchar(sub_lines), 0L)) * 10 * 0.65
  avail <- 734 - 22 - max(tag_w, sub_w)
  min(max_size, avail / (0.74 * max(nchar(name), 1L)))
}

# where a unit name may be broken: at whitespace, and after a solidus, which is
# how names such as OGBA/EGBEMA/NDONI join their parts and the only place they
# give. A solidus stays on the line it ends, the way the name reads.
# @noRd
.pager_label_atoms <- function(main) {
  atoms <- regmatches(main, gregexpr("[^/[:space:]]+/*", main))[[1]]
  atoms[nzchar(atoms)]
}

# rejoin atoms into one line, closing up after a solidus and spacing otherwise,
# so a broken name reads exactly as the whole one did.
# @noRd
.pager_label_join <- function(atoms) {
  Reduce(
    function(acc, a) if (grepl("/$", acc)) paste0(acc, a) else paste(acc, a),
    atoms
  )
}

# fit the endpoint name to the right margin. Archivo at weight 900 runs about
# 0.78em per uppercase character, which the old 0.62 estimate under-measured by
# a fifth, so a name the arithmetic called a fit still ran off the frame. There
# is deliberately no floor under the size: any floor is a size that does not
# fit, and the masthead prints the name in full regardless. Wrapping at a space
# holds a two-word name at a readable size where shrinking alone would squeeze
# it, so it is preferred wherever the name has somewhere to break and one line
# would not sit comfortably.
# @noRd
.pager_label_lines <- function(main, avail, max_size = 14.5, ratio = 0.78,
                               comfortable = 12.5) {
  size_of <- function(parts) {
    min(max_size, avail / (ratio * max(nchar(parts), 1L)))
  }
  one <- size_of(main)
  atoms <- .pager_label_atoms(main)
  if (one >= comfortable || length(atoms) < 2L) {
    return(list(lines = main, size = one))
  }
  # break at the point leaving the longest line shortest, so a name splits
  # evenly rather than stranding one short piece on its own line
  pairs <- lapply(
    seq_len(length(atoms) - 1L),
    function(k) {
      c(
        .pager_label_join(atoms[seq_len(k)]),
        .pager_label_join(atoms[-seq_len(k)])
      )
    }
  )
  widest <- vapply(pairs, function(p) max(nchar(p)), numeric(1))
  parts <- pairs[[which.min(widest)]]
  two <- size_of(parts)
  if (two > one) {
    list(lines = parts, size = two)
  } else {
    list(lines = main, size = one)
  }
}

# ---- conventional AFP indicators (out of grid) ----------------------------

# the tiles, in render order. `n` names the column holding the assessable count
# behind the percentage, falling back to afp_cases. `target` is NA where no
# published threshold exists, so the tile states the figure without grading it.
# @noRd
# `fraction` marks the pass rates whose denominator really is the case count, so
# a thin one can be spelled out as "1 of 2 cases". EV isolation is a proportion
# of specimens rather than of cases, so counting cases would invent a numerator;
# it names the cases behind it without claiming how many passed.
# @noRd
.pager_tile_spec <- list(
  list(key = "stool_adequacy_cond_pct", label = "Stool adequacy",
       target = 80, n = NA_character_, fraction = TRUE),
  list(key = "inv_timeliness_pct", label = "Notify \u2192 invest \u22642d",
       target = 80, n = "inv_timeliness_n", fraction = TRUE),
  list(key = "onset_notify_pct", label = "Onset \u2192 notify \u22647d",
       target = NA_real_, n = "onset_notify_n", fraction = TRUE),
  list(key = "ev_rate", label = "EV rate (ES)",
       target = 50, n = NA_character_, fraction = FALSE)
)

# pull one district's indicator rows: the focal-year values and the rate series.
# matched on `guid` where the panel uses the polished-indicator name, else on
# the pager's own id column. returns NULL when the district or year is absent,
# so a panel that does not cover this district simply omits the strip.
# @noRd
.pager_indicators <- function(indicators, foc_id, id_col, year, years) {
  if (is.null(indicators)) return(NULL)
  stopifnot(is.data.frame(indicators))
  key <- intersect(c(id_col, "guid"), names(indicators))[1]
  if (is.na(key) || !"year" %in% names(indicators)) {
    cli::cli_abort(
      "{.arg indicators_df} needs a {.field year} column and either \\
       {.field {id_col}} or {.field guid}."
    )
  }
  d <- indicators[
    as.character(indicators[[key]]) == as.character(foc_id), ,
    drop = FALSE
  ]
  if (nrow(d) == 0L) return(NULL)
  d <- d[order(as.integer(d$year)), , drop = FALSE]
  focal <- d[as.integer(d$year) == year, , drop = FALSE]
  if (nrow(focal) == 0L) return(NULL)
  list(
    focal = as.list(focal[1L, , drop = FALSE]),
    rate = d$npafp_rate[match(years, as.integer(d$year))]
  )
}

# every one of these indicators is a pass rate: the share of the district's AFP
# cases meeting the criterion. So the value stays the percentage, and the count
# behind it goes underneath, where a rate resting on two cases can be seen for
# what it is rather than read as a settled figure.
# @noRd
.pager_tile_value <- function(pct, n, floor = 5L) {
  if (!isTRUE(is.finite(pct))) {
    return(list(value = "--", state = "miss"))
  }
  thin <- isTRUE(is.finite(n)) && n > 0 && n < floor
  list(
    value = sprintf("%.0f%%", pct),
    state = if (thin) "thin" else "ok"
  )
}

# clears the operational cut, yet the whole posterior sits below one: adequate
# by the rule, short of expectation in fact. Bound to one predicate so the chip,
# the banner, the caption and the significance row cannot drift apart.
# @noRd
.pager_short_of_expectation <- function(r) {
  !isTRUE(r$spi_below) && isTRUE(r$cri_excludes_1)
}

# can this figure be held to its target? only where there is a target, a value,
# and enough cases behind it to mean anything. Shared by the wording and the
# colouring so the two cannot drift apart.
# @noRd
.pager_tile_gradeable <- function(pct, target, state) {
  isTRUE(is.finite(target)) && isTRUE(is.finite(pct)) && identical(state, "ok")
}

# the meta line under a tile value: what the figure rests on, and how it reads
# against its target where it has one.
# @noRd
.pager_tile_meta <- function(pct, n, target, state, fraction = TRUE,
                             missing = NULL) {
  # spell the rate out as cases where the denominator is the case count and is
  # small enough that the reader should see it: "1 of 2 cases", not a bare 50%
  base <- if (identical(state, "miss") && !is.null(missing)) {
    missing
  } else if (!isTRUE(is.finite(n))) {
    ""
  } else if (n == 0) {
    "none assessable"
  } else if (isTRUE(fraction) && identical(state, "thin")) {
    sprintf(
      "%d of %d case%s", as.integer(round(pct / 100 * n)), as.integer(n),
      if (n == 1) "" else "s"
    )
  } else {
    sprintf("of %d cases", as.integer(n))
  }
  # grading a figure that rests on a handful of cases against a target asserts
  # the same false precision the fraction form exists to avoid
  gradeable <- .pager_tile_gradeable(pct, target, state)
  grade <- if (gradeable) {
    sprintf(if (pct >= target) "meets %g%%" else "under %g%%", target)
  } else if (isTRUE(is.finite(target)) && state == "ok") {
    sprintf("target %g%%", target)
  } else {
    ""
  }
  paste(Filter(nzchar, c(base, grade)), collapse = " \u00b7 ")
}

# the four indicator tiles as html.
# @noRd
.pager_tiles_html <- function(r, es_seen = FALSE) {
  cells <- vapply(.pager_tile_spec, function(t) {
    pct <- suppressWarnings(as.numeric(r[[t$key]] %||% NA))
    # only the case-based pass rates fall back to the case count; EV isolation
    # is measured off environmental samples, so counting cases behind it would
    # attach the wrong denominator
    n <- if (!is.na(t$n)) {
      suppressWarnings(as.numeric(r[[t$n]] %||% NA))
    } else if (isTRUE(t$fraction)) {
      suppressWarnings(as.numeric(r$afp_cases %||% NA))
    } else {
      NA_real_
    }
    v <- .pager_tile_value(pct, n)
    # a missing EV rate means no ES site only where the district has no ES
    # detection on record either; virus found there proves a site exists, and
    # the rate is simply unreported. The genuine gap is called out in red.
    miss <- t$missing
    gap <- FALSE
    if (identical(t$key, "ev_rate") && identical(v$state, "miss")) {
      if (isTRUE(es_seen)) {
        miss <- "rate not reported"
      } else {
        miss <- "No ES site"
        gap <- TRUE
      }
    }
    # a graded figure that misses its target reads in the blind-spot red, as
    # does a district with no ES site at all
    fail <- .pager_tile_gradeable(pct, t$target, v$state) && pct < t$target
    cls <- paste(
      c(v$state, if (fail) "fail", if (gap) "gap"), collapse = " "
    )
    sprintf(
      paste0("<div class=\"itile %s\"><div class=\"k\">%s</div>",
             "<div class=\"v\">%s</div><div class=\"m\">%s</div></div>"),
      cls,
      .pager_escape(t$label), v$value,
      .pager_escape(
        .pager_tile_meta(pct, n, t$target, v$state, t$fraction, miss)
      )
    )
  }, character(1))
  paste0("<div class=\"itiles\">", paste(cells, collapse = ""), "</div>")
}

# the non-polio AFP rate over the same years as the SPI chart, against its
# target. the rate's denominator is population rather than the case count, so
# unlike the tiles it stays meaningful in a district detecting almost nothing.
# @noRd
.pager_rate_svg <- function(rate, years, target, accent) {
  n <- length(years)
  w <- 300
  # tall enough to fill the panel the tile column sets, so the rate's shape is
  # readable rather than a flat squiggle in a half-empty box
  h <- 96
  ml <- 26
  mr <- 34
  mt <- 10
  mb <- 14
  top <- max(c(rate, target), na.rm = TRUE) * 1.15
  if (!is.finite(top) || top <= 0) top <- target * 1.5
  px <- function(i) ml + (i / max(n - 1, 1)) * (w - ml - mr)
  py <- function(v) mt + (1 - v / top) * (h - mt - mb)
  fnum <- function(v) sprintf("%.1f", v)

  s <- sprintf(
    "<svg viewBox=\"0 0 %d %d\" width=\"100%%\" height=\"%d\">", w, h, h
  )
  # the target line, and the frame
  s <- paste0(
    s,
    sprintf(
      paste0("<line x1=\"%d\" y1=\"%s\" x2=\"%d\" y2=\"%s\" ",
             "stroke=\"#15233b\" stroke-width=\"1\" ",
             "stroke-dasharray=\"4 3\" opacity=\"0.6\"/>"),
      ml, fnum(py(target)), w - mr, fnum(py(target))
    ),
    sprintf(
      paste0("<text x=\"%d\" y=\"%s\" text-anchor=\"end\" ",
             "font-family=\"Spline Sans Mono\" font-size=\"7.5\" ",
             "fill=\"#5a6883\">%g</text>"),
      ml - 4, fnum(py(target) + 2.5), target
    ),
    sprintf(
      paste0("<line x1=\"%d\" y1=\"%s\" x2=\"%d\" y2=\"%s\" ",
             "stroke=\"#15233b\" stroke-width=\"1\"/>"),
      ml, fnum(py(0)), w - mr, fnum(py(0))
    )
  )
  pts <- vapply(
    which(is.finite(rate)),
    function(i) paste0(fnum(px(i - 1)), ",", fnum(py(rate[i]))),
    character(1)
  )
  if (length(pts) > 1L) {
    s <- paste0(
      s, "<polyline points=\"", paste(pts, collapse = " "),
      "\" fill=\"none\" stroke=\"", accent,
      "\" stroke-width=\"1.8\" stroke-linejoin=\"round\"/>"
    )
  }
  last <- utils::tail(which(is.finite(rate)), 1)
  if (length(last) == 1L) {
    s <- paste0(
      s,
      sprintf(
        "<circle cx=\"%s\" cy=\"%s\" r=\"2.4\" fill=\"%s\"/>",
        fnum(px(last - 1)), fnum(py(rate[last])), accent
      ),
      sprintf(
        paste0("<text x=\"%s\" y=\"%s\" font-family=\"Spline Sans Mono\" ",
               "font-size=\"9\" font-weight=\"600\" fill=\"%s\">%s</text>"),
        fnum(px(last - 1) + 6), fnum(py(rate[last]) + 3), accent,
        .pager_dot(rate[last], 1)
      )
    )
  }
  paste0(s, "</svg>")
}

# how the district reads through its expected seasonal peak, distinguishing a
# channel that was never supplied from one that was and found nothing.
# @noRd
.pager_season_word <- function(r, active) {
  if (!is.null(active) && !isTRUE(unname(active["seasonal"]))) return("")
  switch(
    r$seasonal %||% "not assessed",
    blind = "Blind through the expected peak",
    muted = "Muted through the expected peak",
    # detecting through the peak is worth saying only where the district still
    # reads short: it is then a district that looks and still finds too little
    present = if (isTRUE(r$spi_below)) {
      "Detects through the expected peak, yet still reads short"
    } else {
      ""
    },
    ""
  )
}

# the trend over the trailing window, said only when detection has moved. It
# is context outside STEPS and never changes the judgement.
# @noRd
.pager_trend_word <- function(r, window = 5L) {
  switch(
    r$trajectory %||% "flat",
    falling = sprintf("Detection falling over the last %d years",
                      as.integer(window)),
    rising = sprintf("Detection rising over the last %d years",
                     as.integer(window)),
    ""
  )
}

# the serotype to name in the detection wording. The guide carries detection
# years but not serotypes, so an unset label must not be filled in with a guess:
# it falls back to the generic term instead.
# @noRd
.pager_detection_label <- function(label) {
  label <- if (is.null(label)) "" else trimws(as.character(label)[1])
  if (nzchar(label) && !is.na(label)) label else "poliovirus"
}

# the two detection boxes, one per channel. A channel that was never supplied
# says so rather than reading as a district that was searched and found clean.
# @noRd
.pager_detection_boxes <- function(detections, label, active,
                                   serotypes = list()) {
  on <- function(nm) is.null(active) || isTRUE(unname(active[nm]))
  box <- function(title, years, supplied, found) {
    if (!supplied) {
      v <- "--"
      m <- "channel not supplied"
      cls <- "miss"
    } else if (length(years) == 0L) {
      v <- "none"
      m <- "no detections"
      cls <- "quiet"
    } else {
      # `years` holds distinct years, not detections: a district with four
      # isolations in one year appears once. Count the years and say so -- the
      # guide never carries a detection count, so "4 detections" would be a
      # number the data cannot support. The chart still marks which years.
      # Name the serotypes the guide actually recorded; fall back to the
      # caller's declaration, and to nothing at all where neither is known.
      v <- sprintf(
        "%d year%s", length(years), if (length(years) == 1L) "" else "s"
      )
      what <- if (length(found) > 0L) {
        paste(found, collapse = ", ")
      } else {
        label
      }
      m <- paste(
        c(what[nzchar(what)], sprintf("latest %d", max(years))),
        collapse = " \u00b7 "
      )
      cls <- "hit"
    }
    sprintf(
      paste0("<div class=\"itile det %s\"><div class=\"k\">%s</div>",
             "<div class=\"v\">%s</div><div class=\"m\">%s</div></div>"),
      cls, title, .pager_escape(v), .pager_escape(m)
    )
  }
  paste0(
    "<div class=\"idets\">",
    box("AFP detection", detections$afp, on("detect_afp"), serotypes$afp),
    box("ES detection", detections$es, on("detect_es"), serotypes$es),
    "</div>"
  )
}

# serotypes the guide recorded for this district-year, per channel. An NA column
# means the guide was built without `serotype_col`, so nothing is named and the
# caller's `detection_label` stands in.
# @noRd
.pager_serotypes <- function(focal) {
  pull <- function(x) {
    x <- x %||% NA_character_
    if (length(x) != 1L || is.na(x) || !nzchar(x)) return(character(0))
    trimws(strsplit(x, ",\\s*")[[1]])
  }
  list(afp = pull(focal$orphan_serotypes), es = pull(focal$es_serotypes))
}

# the whole out-of-grid indicator strip. Deliberately its own block below the
# chart, with no role chip: these indicators support the reading from outside
# the five STEPS and never enter the corroborator count or the verdict.
# @noRd
.pager_strip_html <- function(ind, years, npafp_target, accent, focal,
                              detections, detection_label, active,
                              traj_window = 5L) {
  season <- paste(
    c(.pager_season_word(focal, active),
      .pager_trend_word(focal, traj_window)),
    collapse = " \u00b7 "
  )
  season <- sub("^ \u00b7 | \u00b7 $", "", season)
  dets <- .pager_detection_boxes(
    detections, detection_label, active, .pager_serotypes(focal)
  )
  # the indicator half is optional; the detection half always has something to
  # say, so the row renders either way
  left <- if (is.null(ind)) {
    ""
  } else {
    paste0(
      "<div class=\"ipanel\">",
      "<div class=\"ilab\">Non-polio AFP rate per 100,000 under 15 \u00b7 ",
      "target ", npafp_target, "</div>",
      .pager_rate_svg(ind$rate, years, npafp_target, accent),
      "</div>",
      .pager_tiles_html(ind$focal, es_seen = length(detections$es) > 0)
    )
  }
  paste0(
    "<div class=\"istrip\">",
    "<div class=\"sectlab\"><span>Context \u2014 conventional AFP and ES ",
    "indicators and detections</span>",
    if (nzchar(season)) {
      paste0("<span>", .pager_escape(season), "</span>")
    } else {
      ""
    },
    "</div>",
    "<div class=\"igrid", if (is.null(ind)) " dets-only" else "", "\">",
    left, dets, "</div></div>"
  )
}

# ---- STEPS reading rows ---------------------------------------------------

# build the five STEPS rows (code, name, reading, role) for the focal year.
# @noRd
.pager_signals <- function(
  focal, series, spi_cut, params, detections, detection_label,
  unit_noun = "district", active = NULL, prob_under = NULL
) {
  r <- as.list(focal)
  spi <- .pager_dot(r$spi_median, 2)
  q05 <- .pager_dot(r$spi_q05, 2)
  q95 <- .pager_dot(r$spi_q95, 2)
  cut <- .pager_dot(spi_cut, 2)
  obs <- as.integer(round(r$observed))
  exp <- .pager_count(r$expected_total)
  # a zero count makes the interval degenerate: SPI = 0 / expected_draw is
  # identically 0, so (q05, q95) collapses to (0, 0) and carries no evidence
  zero_count <- isTRUE(obs == 0L)
  detected <- if (zero_count) {
    "no cases"
  } else {
    sprintf("%d case%s", obs, if (obs == 1L) "" else "s")
  }

  # S strength: size and certainty of the shortfall
  strength <- if (.pager_short_of_expectation(r)) {
    sprintf(
      paste0("SPI %s is at or above the %s cut, but its 90%% interval (%s to ",
             "%s) lies wholly below one; STEPS is not applied."),
      spi, cut, q05, q95
    )
  } else if (!isTRUE(r$spi_below)) {
    sprintf("SPI %s is at or above the %s cut; STEPS is not applied.",
            spi, cut)
  } else if (!isTRUE(r$cri_excludes_1)) {
    sprintf(
      paste0("SPI %s, %s against %s expected, but its 90%% interval (%s to ",
             "%s) includes one, so the shortfall is uncertain."),
      spi, detected, exp, q05, q95
    )
  } else if (!.fg_gate_pass(r)) {
    sprintf(
      paste0("SPI %s with its 90%% interval (%s to %s) wholly below one, but ",
             "a count this small could fall this short by chance alone."),
      spi, q05, q95
    )
  } else if (zero_count) {
    sprintf(
      paste0("No cases against %s expected, so the SPI is zero by ",
             "construction and the interval carries no evidence.%s"),
      exp, .pager_noise_note(r, params)
    )
  } else {
    sprintf(
      paste0("SPI %s, %s against %s expected, with its 90%% interval (%s to ",
             "%s) wholly below one.%s"),
      spi, detected, exp, q05, q95, .pager_noise_note(r, params)
    )
  }
  # the posterior probability that observed non-polio AFP fell short of
  # expectation says how decisively the interval clears one
  pu_s <- suppressWarnings(as.numeric(prob_under %||% NA))
  if (isTRUE(is.finite(pu_s))) {
    pu_lab <- if (pu_s >= 0.995) {
      "over 99%"
    } else if (pu_s <= 0.005) {
      "under 1%"
    } else {
      sprintf("%.0f%%", 100 * pu_s)
    }
    strength <- paste0(strength, sprintf(" Chance AFP below expected %s.",
                                         pu_lab))
  }

  target <- params$process_target %||% 80
  min_cases <- params$process_min_cases %||% 5L
  process_row <- function(ok, n, what, concern, active_flag) {
    if (!isTRUE(active_flag) || is.na(n)) {
      return(list(reading = "Not supplied.", role = "quiet",
                  label = "not supplied"))
    }
    if (n < min_cases) {
      return(list(
        reading = sprintf("%d case%s; fewer than %d, not assessed.",
                          as.integer(n), if (n == 1L) "" else "s",
                          as.integer(min_cases)),
        role = "quiet", label = "not assessed"
      ))
    }
    list(
      reading = sprintf("%d of %d %s (%.0f%%; target %s%%).",
                        as.integer(ok), as.integer(n), what, 100 * ok / n,
                        format(target)),
      role = if (isTRUE(concern)) "supp" else "quiet",
      label = if (isTRUE(concern)) "concern" else "no concern"
    )
  }
  timeliness <- process_row(
    r$n_transport_timely, r$n_transport,
    "specimens at the laboratory within 3 days", r$timeliness_concern,
    active[["timeliness"]] %||% !is.na(r$n_transport)
  )
  adequacy <- process_row(
    r$n_adequate, r$n_cases, "cases with adequate stool specimens",
    r$adequacy_concern, active[["adequacy"]] %||% !is.na(r$n_cases)
  )

  # E extent: other districts in the same admin-1 area
  extent_ok <- !is.na(r$extent_others %||% NA) && r$extent_others > 0L
  extent <- if (!extent_ok) {
    "Not assessable."
  } else {
    pct <- gsub(".", "\u00b7", .fg_pct_pair(r$extent_pct, r$national_pct),
                fixed = TRUE)
    sprintf(
      "%d of %d other %ss in %s below %s (%s, against %s nationally).",
      as.integer(r$extent_others_below), as.integer(r$extent_others),
      unit_noun, r$extent_area, cut, pct[1], pct[2]
    )
  }

  # P persistence: SPI in the previous year
  prev <- suppressWarnings(as.numeric(r$spi_previous %||% NA))
  persistence <- if (!is.finite(prev)) {
    "No SPI for the previous year."
  } else {
    sprintf("SPI %s in %d, %s the cut.", .pager_dot(prev, 2),
            as.integer(r$year) - 1L,
            if (isTRUE(r$persistence_concern)) "also below" else
              "at or above")
  }

  list(
    list(code = "S", name = "Strength", reading = strength,
         role = "gate", label = "strength"),
    list(code = "T", name = "Timeliness", reading = timeliness$reading,
         role = timeliness$role, label = timeliness$label),
    list(code = "E", name = "Extent", reading = extent,
         role = if (isTRUE(r$extent_concern)) "corr" else "quiet",
         label = if (isTRUE(r$extent_concern)) "corroborates" else "quiet"),
    list(code = "P", name = "Persistence", reading = persistence,
         role = if (isTRUE(r$persistence_concern)) "corr" else "quiet",
         label = if (isTRUE(r$persistence_concern)) "supports" else "quiet"),
    list(code = "S", name = "Stool adequacy", reading = adequacy$reading,
         role = adequacy$role, label = adequacy$label)
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

# could ordinary sampling variation in the observed count produce a shortfall
# this deep on its own? the SPI credible interval cannot answer this: it carries
# posterior uncertainty in the *expected* count with the observed count held
# fixed, so at small expected counts it can be wholly below one while a single
# extra case would lift the ratio across the cut. this is the reference tail
# P(X <= observed | lambda = expected) under a Poisson, which understates the
# spread of the negative-binomial likelihood the model actually fits, so the
# caveat fires less often than a fully propagated interval would warrant.
# read from the guide when it carries the column, else computed here, so a guide
# built before `noise_alpha` existed still reads correctly.
# @noRd
.pager_noise_tail <- function(r) {
  carried <- suppressWarnings(as.numeric(r$noise_tail %||% NA))
  if (isTRUE(is.finite(carried))) return(carried)
  obs <- suppressWarnings(as.numeric(r$observed))
  exp <- suppressWarnings(as.numeric(r$expected_total))
  if (!isTRUE(is.finite(obs) && is.finite(exp) && exp > 0)) return(NA_real_)
  stats::ppois(obs, lambda = exp)
}

# @noRd
.pager_noise_plausible <- function(focal, alpha = 0.05) {
  tail <- .pager_noise_tail(as.list(focal))
  isTRUE(tail > alpha)
}

# significance-row clause on sampling noise: whether the gate tested it, said
# only when it changes the reading.
# @noRd
.pager_noise_note <- function(r, params) {
  alpha <- params$noise_alpha
  tail <- .pager_noise_tail(r)
  if (!is.finite(tail)) return("")
  if (!is.null(alpha)) {
    # only a gate that actually passed can claim the noise was ruled out. The
    # gate being switched on is not the same as it having cleared
    if (!isTRUE(tail <= alpha)) return("")
    return(sprintf(
      " Sampling noise ruled out at %s%% too.",
      formatC(alpha * 100, format = "g")
    ))
  }
  if (tail > 0.05) " Sampling noise untested." else ""
}

# format a model-expected count: whole cases once there are enough of them to
# round without distorting the ratio, one decimal below that, so a district
# with 1.9 expected never prints as "2" beside a stated SPI it contradicts.
# @noRd
.pager_count <- function(x) {
  if (!isTRUE(is.finite(x))) return("--")
  if (abs(x) >= 10) format(round(x), big.mark = ",") else .pager_dot(x, 1)
}

# one- or two-sentence caption describing the reading for this judgement.
# @noRd
.pager_caption <- function(focal, name, params = list()) {
  verdict <- as.character(focal$verdict)
  obs <- as.integer(round(focal$observed))
  exp <- .pager_count(focal$expected_total)
  zero <- isTRUE(obs == 0L)
  if (isTRUE(focal$spi_below)) {
    seen <- if (zero) {
      sprintf(
        paste0("%s reported no non-polio AFP cases against the %s the model ",
               "expects for its population, location and season."),
        name, exp
      )
    } else {
      sprintf(
        paste0("%s reports fewer non-polio AFP cases than expected: ",
               "%d against %s."),
        name, obs, exp
      )
    }
    # the interval speaks to uncertainty in the expectation, never to sampling
    # variability in the count, so name the latter when chance alone could
    # produce the shortfall, unless a noise gate already ruled it out
    caveat <- if (!is.null(params$noise_alpha) ||
                  !.pager_noise_plausible(focal)) {
      ""
    } else if (zero) {
      " Zero cases could occur by chance alone when so few are expected."
    } else {
      " This shortfall could occur by chance alone when so few cases are expected."
    }
    paste0(seen, caveat)
  } else if (.pager_short_of_expectation(focal)) {
    sprintf(
      paste0("%s is at or above the cut, but its 90%% credible interval lies ",
             "wholly below one: it reports measurably fewer non-polio AFP ",
             "cases than the model expects for its population, location and season."),
      name
    )
  } else {
    sprintf(
      paste0("%s reports about as many non-polio AFP cases as the model ",
             "expects for its size, place and season; its SPI is at or above ",
             "the cut."),
      name
    )
  }
}

# the banner label sits in an auto-width grid column, so a qualified tag like
# "Flag . corroborated" takes a quarter of the banner off the reading beside
# it. Break it at the separator so the qualifier stacks under the verdict. The
# masthead pill is unaffected: it has a line to itself and reads better whole.
# @noRd
.pager_tag_stacked <- function(tag) {
  sub(" \u00b7 ", "<br>", tag, fixed = TRUE)
}

# judgement-banner narrative: which STEPS findings the judgement rests on.
# @noRd
.pager_banner_reading <- function(focal, spi_cut, params, detections, label) {
  verdict <- as.character(focal$verdict)
  corroborators <- c(
    if (isTRUE(focal$extent_concern)) "extent",
    if (isTRUE(focal$persistence_concern)) "persistence"
  )
  process <- c(
    if (isTRUE(focal$timeliness_concern)) "specimen transport",
    if (isTRUE(focal$adequacy_concern)) "stool adequacy"
  )
  process_txt <- if (length(process) > 0L) {
    sprintf(" Weakness in %s adds to the concern.",
            paste(process, collapse = " and "))
  } else {
    ""
  }
  if (verdict == "Review priority") {
    return(paste0(
      sprintf(
        paste0("Reporting is below expectation, with support from %s. ",
               "The findings warrant further investigation."),
        paste(corroborators, collapse = " and ")
      ),
      process_txt
    ))
  }
  if (verdict == "Monitor") {
    reason <- if (!isTRUE(focal$cri_excludes_1)) {
      "the 90% interval still includes one"
    } else if (!.fg_gate_pass(focal)) {
      "a count this small could fall this short by chance alone"
    } else {
      "neither extent nor persistence supports the shortfall"
    }
    return(paste0(
      sprintf(
        paste0("Reporting is below expectation, but %s. The findings support ",
               "reassessment as new data become available."),
        reason
      ),
      process_txt
    ))
  }
  if (.pager_short_of_expectation(focal)) {
    return(paste0(
      "The SPI is at or above the cut, but its 90% interval lies wholly ",
      "below one: reporting is below expectation, but STEPS is not applied ",
      "at or above the cut."
    ))
  }
  paste0(
    "No SPI indication for additional review. Routine surveillance and ",
    "review based on other indicators continue."
  )
}

# assemble the full self-contained html document.
# @noRd
.pager_html <- function(
  focal, series, chart, locator, signals, vstyle, spi_cut, params,
  detections, detection_label, year, name_col, id_col, admin_label,
  unit_noun = "district", note = NULL, indicators = NULL, npafp_target = 3,
  active = NULL, region = NULL, year_label = NULL
) {
  # `[[` rather than `$`: a tibble warns on an absent column accessed with `$`,
  # and an admin level the caller simply does not carry is not a fault
  name <- .pager_escape(as.character(focal[[name_col]]))
  adm1 <- .pager_escape(as.character(focal[["adm1_name"]] %||% ""))
  adm0 <- .pager_escape(as.character(focal[["adm0_name"]] %||% ""))
  # drop the parent level that *is* the unit (adm1 data labels the unit by
  # adm1_name, so it would otherwise repeat in the hierarchy line). Keyed on
  # which column names the unit, not on the strings matching: an LGA that
  # shares its state's name is still inside that state, and dropping the
  # parent there left the page indistinguishable from the state's own.
  parent <- paste(
    c(
      adm1[nzchar(adm1) & !identical(name_col, "adm1_name")],
      adm0[nzchar(adm0) & !identical(name_col, "adm0_name")],
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
  # the under-15 denominator the expected count is built on. Without it a reader
  # cannot tell whether an expectation of 1.9 cases belongs to a district of
  # twenty thousand children or two hundred thousand.
  pop <- suppressWarnings(as.numeric(focal$pop_u15 %||% NA))
  pop_txt <- if (isTRUE(is.finite(pop))) {
    sprintf(
      "<br>pop u15 %s", formatC(round(pop), format = "d", big.mark = ",")
    )
  } else {
    ""
  }

  # prob_under is deliberately not read here: the masthead is already five
  # lines deep, and the number is the strength of the evidence the
  # significance gate tests, so it belongs on that STEPS line instead. See
  # .pager_signals().

  # regional context. The rank orders regions that are already short so a reader
  # can see whether an unremarkable district sits inside a badly performing
  # region; it is triage, never part of the verdict.
  region_txt <- ""
  if (!is.null(region)) {
    r <- as.list(region)
    r_name <- r$name %||% r$region %||% r$adm1 %||% NA_character_
    r_rank <- suppressWarnings(as.integer(r$rank %||% r$region_rank %||% NA))
    r_n <- suppressWarnings(as.integer(r$n %||% r$n_regions %||% NA))
    r_spi <- suppressWarnings(as.numeric(r$spi %||% r$region_spi %||% NA))
    bits <- character(0)
    if (length(r_name) == 1L && !is.na(r_name) && nzchar(r_name)) {
      bits <- c(bits, sprintf("region %s", r_name))
    }
    if (isTRUE(is.finite(r_spi))) {
      bits <- c(bits, sprintf("SPI %s", .pager_dot(r_spi, 2)))
    }
    if (isTRUE(is.finite(r_rank)) && isTRUE(is.finite(r_n))) {
      bits <- c(bits, sprintf("worst rank %d of %d", r_rank, r_n))
    }
    if (length(bits) > 0L) {
      region_txt <- paste0("<br>", paste(bits, collapse = " \u00b7 "))
    }
  }

  # the sub-lines as they read, so the name is sized against the column they
  # actually need rather than a worst case
  unit_size <- sprintf("%.1f", .pager_unit_size(
    name, vstyle$tag,
    c(vstyle$state, paste0("SPI ", spi, nb_txt),
      sub("^<br>", "", pop_txt), sub("^<br>", "", region_txt))
  ))

  rows <- vapply(signals, function(s) {
    sprintf(
      paste0("<div class=\"srow\"><div class=\"sc\">%s</div>",
             "<div class=\"sn\">%s</div><div class=\"sr\">%s</div>",
             "<span class=\"role %s\">%s</span></div>"),
      s$code, .pager_escape(s$name), .pager_escape(s$reading), s$role, s$label
    )
  }, character(1))
  rows <- paste(rows, collapse = "")

  rule <- sprintf(
    paste0("STEPS is applied to a %s with an SPI below %s. It is a review ",
           "priority when its 90%% credible interval lies wholly below one and ",
           "extent or persistence corroborates the shortfall. Timeliness and ",
           "stool adequacy are reported but do not change the judgement; the ",
           "trend, season and detections are context."),
    unit_noun, .pager_dot(spi_cut, 2)
  )
  caption <- .pager_escape(.pager_caption(focal, name, params))
  banner_reading <- .pager_escape(
    .pager_banner_reading(focal, spi_cut, params, detections, detection_label)
  )
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
  # the provenance tag is opt-in: the footer carries it only when `note` is
  # supplied, so a reading is never stamped with a placeholder by default
  note <- if (is.null(note)) "" else trimws(as.character(note)[1])
  note_tag <- if (nzchar(note)) {
    paste0(" \u00b7 ", .pager_escape(note))
  } else {
    ""
  }
  # The note stays OFF the eyebrow and appears only in the footer. It is
  # caller-supplied and can be long, and the eyebrow sits directly above the
  # unit name at .2em tracking -- so a long note wraps and crowds the name,
  # worst for exactly the units with long admin names. The footer has the width
  # for it and is where provenance belongs anyway.
  yr_lbl <- if (is.null(year_label)) {
    sprintf("%d", year)
  } else {
    as.character(year_label)
  }
  eyebrow <- paste(
    c("spi", "SPI reading", adm0[nzchar(adm0)], yr_lbl),
    collapse = " \u00b7 "
  )

  paste0(
    .pager_head(name, year, vstyle$accent),
    "<body><div class=\"page\"><div class=\"corridor\"></div>",
    "<div class=\"pad\">",
    # masthead
    "<div class=\"mast\"><div>",
    "<div class=\"eyebrow\">", eyebrow, "</div>",
    "<div class=\"unit\" style=\"font-size:", unit_size, "px\">",
    name, "</div>",
    "<div class=\"parent\">", parent, "</div></div>",
    "<div class=\"verdict\"><span class=\"tag\">", vstyle$tag, "</span>",
    "<div class=\"sub\">", vstyle$state, "<br>SPI <b>", spi, "</b>",
    nb_txt, pop_txt, region_txt, "</div></div></div>",
    # body
    "<div class=\"body\"><div class=\"cgroup\">",
    "<div class=\"sectlab\"><span>SPI over time \u2014 ", name, "</span>",
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
    "<span><span class=\"lk exp\"></span> 1\u00b70, expected detection</span>",
    legend_detect,
    "</div>",
    .pager_strip_html(
      indicators, series$year, npafp_target, vstyle$accent, focal,
      detections, detection_label, active,
      traj_window = params$traj_window %||% 5L
    ),
    "</div>",
    # reading
    "<div class=\"reading\"><div class=\"sectlab\" ",
    "style=\"margin-bottom:6px\"><span>Reading the SPI \u2014 the five STEPS ",
    "for ",
    name, "</span><span>strength \u00b7 process \u00b7 supporting evidence</span></div>",
    "<div class=\"rrule\">", rule, "</div>",
    "<div>", rows, "</div></div>",
    # verdict banner
    "<div class=\"vbanner\"><div class=\"vt\">",
    .pager_tag_stacked(vstyle$tag), "</div>",
    "<div class=\"vw\">", banner_reading, "</div></div>",
    "</div>",
    # footer
    "<div class=\"foot\"><div>", sub("^ \u00b7 ", "", note_tag),
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
# guide's report design plus a muted "quiet" role chip. `accent` drives
# the verdict colour (rose flag / plum review / amber watch / green no-action).
# @noRd
.pager_css <- function(accent) {
  paste0(
    "@import url('https://fonts.googleapis.com/css2?family=Archivo:wght@400;",
    "500;600;700;800;900&family=Spline+Sans+Mono:wght@400;500;600&family=",
    "Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,500;0,6..72,600;1,6..72,",
    "400;1,6..72,500&display=swap');",
    ":root{--ink:#15233b;--ink-soft:#5a6883;--paper:#fbf7f0;--line:#e4ddd0;",
    "--grid:#efe9dd;--blind:#c8102e;--buildup:#e87722;",
    "--watch:#2f9e8f;--assured:#1f6f43;--accent:", accent, "}",
    "*{margin:0;padding:0;box-sizing:border-box}",
    "body{background:#cfc6b6;font-family:'Archivo',sans-serif;padding:30px;",
    "display:flex;flex-direction:column;align-items:center;color:var(--ink)}",
    ".page{width:794px;min-height:1123px;height:auto;background:var(--paper);",
    "position:relative;overflow:hidden;",
    "box-shadow:0 30px 70px -25px rgba(21,35,59,.45);",
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
    # the locator badge hangs 12px above the chart box, so the box needs more
    # than that above it or the badge collides with the caption
    ".chartbox{flex-shrink:0;margin-top:24px;background:#fffdf8;border:1px ",
    "solid var(--line);padding:10px 12px 6px;position:relative}",
    ".ts{width:100%}.ts svg{width:100%;height:auto;display:block}",
    ".locbadge{position:absolute;top:-12px;right:-11px;width:130px;",
    "height:130px;background:#fffdf8;border:1px solid var(--line);",
    "box-shadow:0 10px 22px -8px rgba(21,35,59,.42);padding:7px;display:flex;",
    "align-items:center}",
    ".locbadge .lbmap{width:100%}",
    ".locbadge .lbmap svg{width:100%;height:auto;display:block}",
    ".legend{display:flex;gap:18px;flex-wrap:wrap;margin-top:8px;font-family:",
    "'Spline Sans Mono',monospace;font-size:9px;color:var(--ink-soft);",
    "align-items:center}",
    ".legend span{display:inline-flex;align-items:center;gap:6px}",
    ".lk{width:20px;height:0;border-top:3px solid var(--accent)}",
    ".lk.exp{border-top:1.4px dashed var(--ink)}",
    ".lband{width:18px;height:11px;background:var(--accent);opacity:.16;",
    "border-radius:2px}",
    ".ld{width:8px;height:8px;background:var(--accent);",
    "transform:rotate(45deg)}",
    ".lo{width:9px;height:9px;border:1.6px solid var(--accent);",
    "border-radius:50%;box-sizing:border-box}",
    ".istrip{flex-shrink:0;margin-top:20px}",
    ".igrid{display:grid;grid-template-columns:1fr 274px 120px;gap:8px}",
    ".igrid.dets-only{grid-template-columns:1fr}",
    ".idets{display:grid;grid-template-rows:1fr 1fr;gap:6px}",
    ".itile.det .v{font-size:13px;letter-spacing:-.01em;word-break:break-word}",
    ".itile.det.hit .v{color:var(--accent)}",
    ".ipanel{background:#fffdf8;border:1px solid var(--line);",
    "padding:6px 9px 2px}",
    ".ilab{font-family:'Spline Sans Mono',monospace;font-size:8px;",
    "letter-spacing:.11em;text-transform:uppercase;color:var(--ink-soft);",
    "margin-bottom:7px}",
    ".ipanel svg{width:100%;height:auto;display:block}",
    ".itiles{display:grid;grid-template-columns:1fr 1fr;gap:6px}",
    ".itile{background:#fffdf8;border:1px solid var(--line);",
    "padding:4px 8px 5px;",
    "display:flex;flex-direction:column;justify-content:center}",
    ".itile .k{font-family:'Spline Sans Mono',monospace;font-size:7.5px;",
    "letter-spacing:.07em;text-transform:uppercase;color:var(--ink-soft)}",
    ".itile .v{font-size:18px;font-weight:800;letter-spacing:-.025em;",
    "line-height:1.15;margin-top:5px}",
    ".itile .m{font-family:'Spline Sans Mono',monospace;font-size:7.5px;",
    "color:var(--ink-soft);line-height:1.3;margin-top:2px}",
    ".itile.thin .v,.itile.miss .v{font-size:13px;color:var(--ink-soft)}",
    ".itile.fail .v,.itile.fail .m,.itile.gap .v,.itile.gap .m{",
    "color:var(--blind)}",
    ".itile.gap .m{font-weight:600}",
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
    ".vbanner{display:grid;grid-template-columns:auto 1fr;gap:16px;",
    "align-items:center;background:var(--accent);color:#fff;padding:9px 15px;",
    "margin-top:9px}",
    ".vbanner .vt{font-family:'Spline Sans Mono',monospace;font-weight:600;",
    "font-size:12px;letter-spacing:.09em;text-transform:uppercase;",
    "line-height:1.35}",
    # `pretty`, not `balance`: balance equalises the line lengths, so on a
    # two-line reading it holds the first line short and leaves a gap down the
    # right of the banner. Fill the first line and let the last one fall where
    # it does -- pretty still avoids breaking to a one-word last line, which is
    # the fault balance was there to prevent.
    ".vbanner .vw{font-family:'Newsreader',serif;font-size:12.5px;",
    "line-height:1.4;color:rgba(255,255,255,.9);text-wrap:pretty}",
    ".foot{display:flex;justify-content:space-between;align-items:center;",
    "margin-top:10px;padding-top:11px;border-top:1px solid var(--line);",
    "flex-shrink:0;font-family:'Spline Sans Mono',monospace;font-size:9px;",
    "color:var(--ink-soft)}",
    "svg{display:block}"
  )
}
