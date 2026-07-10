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
      ft, values = stats::setNames(as.list(c("", col_head)), c("Signal", col_ids))
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
