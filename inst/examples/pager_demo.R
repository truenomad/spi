# Render one-page SPI pagers from the packaged synthetic field guide.
#
# The pager is self-contained: hand it the field guide and the shapefile and it
# draws the locator inset from the real geometry, placing the district in its
# country, and reads both detection channels (AFP + ES, already carried by the
# field guide) into the out-of-grid row. Each district is written as a
# self-contained HTML tear-sheet plus a PNG preview, auto-named
# spi_<adm0>_<adm1>_<adm2>_field_pager.{html,png}, into inst/examples/pager/ so
# the results can be reviewed in the repo. PNG output needs webshot2 and a
# headless Chrome.
#
#   source(system.file("examples/pager_demo.R", package = "blindspot"))
# or, from a source checkout:
#   Rscript inst/examples/pager_demo.R
#
# Operational note: the pager renders the trend, the corroborator count and the
# verdict straight from the field-guide object; it does not recompute them. So
# the levers that decide what the page says are all at the *build* of the guide,
# not at the pager call. Three matter:
#
#   fg <- bs_field_guide(conc, adjacency = adj, spi_month = cm,
#                        genomic = g, es = e,
#                        traj_alpha = 0.1,             # gate the trend test
#                        noise_alpha = 0.05,           # gate on sampling noise
#                        persistence_basis = "trailing")  # current run only
#
# All three default off, so the package reproduces the paper's published flag
# counts out of the box. For operational pagers that people act on, turn them
# on: traj_alpha stops a single volatile year reading as a sustained decline,
# noise_alpha stops a shortfall that chance alone could produce from opening the
# significance gate, and persistence_basis = "trailing" stops a sub-cut run that
# ended years ago from corroborating this year's reading. Keep the defaults for
# manuscript figures. This synthetic gallery renders from the precomputed
# (default-settings) `synth_field_guide` so it runs instantly without INLA.

devtools::load_all()

out_dir <- file.path("inst", "examples", "pager")

fg <- synth_field_guide
foc <- fg$focal
cut <- fg$thresholds$spi

# ---- illustrative indicator panel -----------------------------------------
# `indicators_df` fills the context row with conventional AFP and ES indicators,
# shaped as a district-year panel keyed by guid and year. In a real run this is
# your polished indicator table. Nothing like it ships with the package, so the
# gallery fabricates one to exercise the row: the case counts and rate come from
# the guide, and the quality percentages are SIMULATED. Do not read any meaning
# into the stool-adequacy, timeliness or EV figures in these PNGs.
#
# The percentages are drawn against the real assessable case counts on purpose.
# That is what puts a district with two AFP cases, and one with none at all,
# into the gallery -- the cases the strip has to report honestly rather than as
# a confident "100%".
set.seed(20260725)
dyi <- fg$district_year
nrows <- nrow(dyi)
afp_n <- as.integer(dyi$observed) + stats::rbinom(nrows, 2, 0.12)
pct_of <- function(k, p) {
  out <- rep(NA_real_, length(k))
  ok <- k > 0
  out[ok] <- 100 * stats::rbinom(sum(ok), size = k[ok], prob = p) / k[ok]
  out
}
n_notify <- stats::rbinom(nrows, size = afp_n, prob = 0.86)
n_onset <- stats::rbinom(nrows, size = afp_n, prob = 0.83)

indicators <- tibble::tibble(
  guid = dyi$adm2_guid,
  name = dyi$adm2_name,
  year = as.integer(dyi$year),
  afp_cases = afp_n,
  npafp_cases = as.integer(dyi$observed),
  npafp_rate = dyi$npafp_rate,
  # EV isolation comes off environmental samples, so it is missing where the
  # district has no ES site at all -- not where it happened to have few AFP
  # cases. Tying it to the wrong denominator would put "no ES site" on the page
  # beside a box reporting ES detections.
  ev_rate = ifelse(
    dyi$adm2_guid %in% synth_surveillance$es_sites$adm2_guid,
    pmin(95, pmax(5, 55 + stats::rnorm(nrows, 0, 14))),
    NA_real_
  ),
  stool_adequacy_cond_pct = pct_of(afp_n, 0.74),
  inv_timeliness_pct = pct_of(n_notify, 0.72),
  onset_notify_pct = pct_of(n_onset, 0.68),
  inv_timeliness_n = n_notify,
  onset_notify_n = n_onset
)

# ES is an optional out-of-grid channel; tolerate a guide built without it
es_hit <- if ("es_detected" %in% names(foc)) foc$es_detected %in% TRUE else FALSE
gen_hit <- foc$genomic_orphan %in% TRUE

# one district per verdict archetype, chosen by rule so the gallery survives a
# regeneration of the synthetic bundle. these are the four accent states the
# pager can show:
#  - FLAG, corroborated -- the deepest flag with a virus detection (rose)
#  - WATCH -- below the cut but the interval still reaches one (amber)
#  - No action, below cut -- a credible shortfall with too few corroborators
#    to flag, kept under monitoring (slate)
#  - Adequate -- a large district clearly above the cut (green)
deepest <- function(keep) {
  if (!any(keep, na.rm = TRUE)) return(character(0))
  foc$adm2_guid[keep][which.min(foc$spi_median[keep])]
}
largest <- function(keep) {
  if (!any(keep, na.rm = TRUE)) return(character(0))
  foc$adm2_guid[keep][which.max(foc$expected_total[keep])]
}

districts <- unique(c(
  deepest(foc$verdict == "FLAG" & (gen_hit | es_hit)),
  deepest(foc$verdict == "WATCH"),
  deepest(foc$verdict == "No action" & foc$spi_below),
  largest(foc$verdict == "No action" & foc$spi_median >= cut)
))

pagers <- lapply(districts, function(d) {
  bs_field_guide_pager(
    fg,
    district = d,
    boundaries = synth_surveillance$boundaries,
    id_col = "adm2_guid",
    indicators_df = indicators,
    # the guide above was built from a cVDPV2-only genomic input, so the page
    # may name that serotype; a mixed input would leave this NULL
    detection_label = "cVDPV2",
    path = out_dir
  )
})

invisible(pagers)
