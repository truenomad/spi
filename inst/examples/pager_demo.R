# Render one-page SPI pagers from the packaged synthetic field guide.
#
# Creates a district report with a location map, STEPS findings, and AFP / ES
# detections. Saves HTML and PNG files in inst/examples/pager/ as
# spi_<adm0>_<adm1>_<adm2>_field_pager.{html,png}.
# PNG output needs webshot2 and headless Chrome.
#
# Run from a source checkout:
#   Rscript inst/examples/pager_demo.R
#
# The pager uses the field guide's existing findings. Set trend and
# sampling-noise options when building the guide:
#
#   fg <- spi_field_guide(conc, process = p, adjacency = adj, spi_month = cm,
#                        detections = g, es = e,
#                        traj_alpha = 0.1,     # trend significance level
#                        noise_alpha = 0.05)   # sampling-noise threshold
#
# Both options are off by default. traj_alpha tests whether a trend differs
# from zero. noise_alpha checks whether the shortfall could arise by chance.
# This example uses the precomputed synth_field_guide with default settings,
# so it runs without fitting an INLA model.

devtools::load_all()

out_dir <- file.path("inst", "examples", "pager")

fg <- synth_field_guide
foc <- fg$focal
cut <- fg$thresholds$spi

# ---- illustrative indicator panel -----------------------------------------
# `indicators_df` contains conventional AFP and ES indicators by district-year.
# Counts and rates come from the synthetic field guide; onset-to-notification
# percentages come from the bundled timeliness data. The remaining percentages
# are simulated for this example and have no surveillance interpretation.
# Using the case counts as denominators also demonstrates reports for
# districts with very few or no AFP cases.
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

# onset-to-notification is the one AFP indicator POLIS does not publish, so it
# is derived from case data rather than simulated: the bundle carries the
# district-year assessable and within-window counts the percentage is built on
tl <- synth_surveillance$afp_timeliness[
  match(
    paste(dyi$adm2_guid, dyi$year),
    paste(
      synth_surveillance$afp_timeliness$adm2_guid,
      synth_surveillance$afp_timeliness$year
    )
  ),
]
onset_pct <- ifelse(tl$n_assessable > 0,
                    100 * tl$n_within_7d / tl$n_assessable, NA_real_)

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
  onset_notify_pct = onset_pct,
  inv_timeliness_n = n_notify,
  onset_notify_n = tl$n_assessable
)

# ES is an optional context channel; tolerate a guide built without it
es_hit <- if ("es_detected" %in% names(foc)) foc$es_detected %in% TRUE else FALSE
gen_hit <- foc$genomic_orphan %in% TRUE

# one district per judgement, chosen by rule so the gallery survives a
# regeneration of the synthetic bundle:
#  - Review priority -- the deepest priority with a virus detection (rose)
#  - Monitor, uncertain -- below the cut with an interval reaching 1 (amber)
#  - Monitor, uncorroborated -- certain, but neither extent nor persistence
#    supports it (amber)
#  - No SPI indication -- a large district clearly above the cut (green)
deepest <- function(keep) {
  if (!any(keep, na.rm = TRUE)) return(character(0))
  foc$adm2_guid[keep][which.min(foc$spi_median[keep])]
}
largest <- function(keep) {
  if (!any(keep, na.rm = TRUE)) return(character(0))
  foc$adm2_guid[keep][which.max(foc$expected_total[keep])]
}

districts <- unique(c(
  deepest(foc$verdict == "Review priority" & (gen_hit | es_hit)),
  deepest(foc$verdict == "Monitor" & !foc$cri_excludes_1),
  deepest(foc$verdict == "Monitor" & foc$gate_pass),
  largest(foc$verdict == "No SPI indication" & foc$spi_median >= cut)
))

pagers <- lapply(districts, function(d) {
  spi_field_guide_pager(
    fg,
    district = d,
    boundaries = synth_surveillance$boundaries,
    id_col = "adm2_guid",
    indicators_df = indicators,
    # the guide above was built from cVDPV2-only AFP detection records, so the page
    # may name that serotype; a mixed input would leave this NULL
    detection_label = "cVDPV2",
    path = out_dir
  )
})

invisible(pagers)
