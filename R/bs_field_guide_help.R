#' Learn to read the SPI field guide
#'
#' @description
#' An interpretation aid for the console. Explains the seven signals and the
#' flag rule behind [bs_field_guide()], lists the common misreadings that
#' end a programme's trust in the score, and -- most usefully -- walks
#' through a **live worked example**: four archetype districts (reassuring,
#' watch, flag, corroborated flag) selected by rule from a real field guide
#' and narrated from their own signal values.
#'
#' @details
#' By default the worked example is narrated from `synth_field_guide`, the
#' precomputed field guide on the synthetic bundle, so it runs instantly with
#' no model fit. Pass `guide` to narrate your own [bs_field_guide()] result.
#'
#' @param topic One or more of `"all"`, `"signals"`, `"verdict"`,
#'   `"misreadings"`, `"example"`. Default: `"all"`.
#' @param guide A [bs_field_guide()] result to narrate for the worked example.
#'   Default: NULL (use `synth_field_guide`).
#' @param year Focal year for the worked example. Default: NULL
#'   (`guide$read_year`).
#'
#' @return The narrated worked-example tibble, invisibly.
#'
#' @seealso [bs_field_guide()], [bs_field_guide_table()].
#'
#' @importFrom rlang %||%
#' @export
#' @examples
#' bs_field_guide_help("signals")
#' bs_field_guide_help("verdict")
#' bs_field_guide_help("example")
bs_field_guide_help <- function(
  topic = c("all", "signals", "verdict", "misreadings", "example"),
  guide = NULL,
  year = NULL
) {
  .check_pkg("cli", reason = "to print the field-guide help")
  topic <- match.arg(topic, several.ok = TRUE)
  if ("all" %in% topic) {
    topic <- c("signals", "verdict", "misreadings", "example")
  }

  spi_cut <- if (!is.null(guide)) guide$thresholds$spi else 0.80

  cli::cli_h1("How to read a low SPI")

  if ("signals" %in% topic) {
    cli::cli_h2("The seven signals")
    cli::cli_text(
      "A low SPI marks a district for a closer look. Read it through seven \\
       signals, each ruling out a different innocent explanation; the \\
       reading rests on their convergence."
    )
    ref <- .fg_reference(spi_cut)
    for (i in seq_len(nrow(ref))) {
      cli::cli_h3(ref$signal[i])
      cli::cli_ul()
      cli::cli_li("{.strong Asks:} {ref$asks[i]}")
      cli::cli_li("{.strong Rules out:} {ref$rules_out[i]}")
      cli::cli_end()
    }
  }

  if ("verdict" %in% topic) {
    cli::cli_h2("The flag rule")
    cli::cli_text(
      "Each district-year gets one of three verdicts. The rule is \\
       deliberately conservative: uncertainty is half the signal."
    )
    cli::cli_ul()
    cli::cli_li(
      "{.strong FLAG} -- posterior median SPI below {sprintf('%.2f', spi_cut)}, \\
       the 90% credible interval upper bound below 1, and at least two of \\
       four corroborators fire (falling trajectory, persistence, neighbour \\
       discordance, seasonal blindness). Warrants supervisory review and \\
       active case search."
    )
    cli::cli_li(
      "{.strong WATCH} -- below the cut, but the credible interval still \\
       reaches 1. Kept under watch, not flagged; collect another year of data."
    )
    cli::cli_li(
      "{.strong No action} -- neither condition holds. No follow-up needed."
    )
    cli::cli_end()
    cli::cli_text(
      "Genomic corroboration strengthens a flag from outside the model, but \\
       is never required to make one."
    )
  }

  if ("misreadings" %in% topic) {
    cli::cli_h2("Common misreadings")
    mis <- .fg_misreadings(spi_cut)
    for (i in seq_len(nrow(mis))) {
      cli::cli_ul()
      cli::cli_li("{.emph {mis$misreading[i]}}")
      cli::cli_li("{.strong Actually:} {mis$correction[i]}")
      cli::cli_end()
    }
  }

  worked <- NULL
  if ("example" %in% topic) {
    guide <- guide %||% .fg_load_synth()
    stopifnot(inherits(guide, "blindspot_field_guide"))
    year <- as.integer(year %||% guide$read_year)
    foc <- guide$district_year[guide$district_year$year == year, ,
                               drop = FALSE]
    id_col <- guide$id_col
    name_col <- intersect(c("adm2_name", "adm1_name"), names(foc))[1] %||%
      id_col
    sel <- .fg_select_worked(foc, id_col, guide$thresholds$spi)

    cli::cli_h2("Worked example ({year})")
    cli::cli_text(
      "Four districts, chosen by rule, running from clearly adequate to a \\
       corroborated flag:"
    )
    for (i in seq_len(nrow(sel))) {
      r <- as.list(sel[i, ])
      narrative <- .fg_narrate(r, guide$thresholds$spi)
      action <- .fg_action(r)
      cli::cli_h3("{sel[[name_col]][i]} -- {sel$case_label[i]}")
      cli::cli_text(narrative)
      cli::cli_alert_info("Action: {action}")
    }
    worked <- sel
  }

  invisible(worked)
}

# @noRd
.fg_load_synth <- function() {
  e <- new.env()
  ok <- tryCatch({
    utils::data("synth_field_guide", package = "blindspot", envir = e)
    TRUE
  }, error = function(err) FALSE)
  if (!ok || !exists("synth_field_guide", envir = e)) {
    cli::cli_abort(
      "{.val synth_field_guide} is not available; pass a \\
       {.fn bs_field_guide} result via {.arg guide}."
    )
  }
  e$synth_field_guide
}

# one-sentence, data-driven reading of a district-year row.
# @noRd
.fg_narrate <- function(r, spi_cut) {
  label <- r$case_label %||% as.character(r$verdict)
  spi <- sprintf("%.2f", r$spi_median)
  q05 <- sprintf("%.2f", r$spi_q05)
  q95 <- sprintf("%.2f", r$spi_q95)
  run <- as.integer(r$longest_run_below)
  exp <- sprintf("%.1f", r$expected_total)

  switch(
    label,
    "Reassuring" = sprintf(
      paste0("Adequate: SPI %s with its whole 90%% credible interval (%s to ",
             "%s) above the cut, detecting about as many cases as expected."),
      spi, q05, q95
    ),
    "Watch" = sprintf(
      paste0("Falls just under the cut at SPI %s, but its credible interval ",
             "still reaches 1 (%s to %s) on a small expected count, so it is ",
             "kept under watch rather than flagged -- avoiding a threshold ",
             "artefact."),
      spi, q05, q95
    ),
    "Flag" = sprintf(
      paste0("The district the conventional rate misses: it clears the rate ",
             "target, yet SPI places it deep at %s, with %d consecutive year%s ",
             "below the cut against %s expected cases."),
      spi, run, if (run == 1L) "" else "s", exp
    ),
    "Flag, corroborated" = sprintf(
      paste0("Carries a flag on %d signals at once, with an orphan poliovirus ",
             "detected there (%s) confirming it from outside the model."),
      as.integer(r$corroborators), r$orphan_years
    ),
    "Flag, persistent" = sprintf(
      paste0("Carries a flag on %d signals at once, resting on the longest ",
             "run of sub-threshold years in the panel (%d)."),
      as.integer(r$corroborators), run
    ),
    # fallback
    sprintf("SPI %s; verdict %s.", spi, as.character(r$verdict))
  )
}

# @noRd
.fg_action <- function(r) {
  v <- as.character(r$verdict)
  switch(
    v,
    FLAG = "supervisory review and active case search.",
    WATCH = "collect another year of data before acting.",
    "no follow-up needed."
  )
}
