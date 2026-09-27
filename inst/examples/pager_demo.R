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
# pager_indicators.R builds it and says which figures are simulated.
source(file.path("inst", "examples", "pager_indicators.R"))
indicators <- example_indicators(fg)

# ES is an optional context channel; tolerate a guide built without it
es_hit <- if ("es_detected" %in% names(foc)) foc$es_detected %in% TRUE else FALSE
gen_hit <- foc$genomic_orphan %in% TRUE

# one district per judgement, chosen by rule so the gallery survives a
# regeneration of the synthetic bundle:
#  - Priority for review -- the deepest priority with a virus detection (rose)
#  - Monitor, uncertain -- below the cut with an interval reaching 1 (amber)
#  - Monitor, uncorroborated -- certain, but neither extent nor persistence
#    supports it, with at least one case so the tiles have figures (amber)
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
  deepest(foc$verdict == "Priority for review" & (gen_hit | es_hit)),
  deepest(foc$verdict == "Monitor" & !foc$cri_excludes_1),
  deepest(foc$verdict == "Monitor" & foc$gate_pass & foc$observed > 0),
  largest(foc$verdict == "No SPI indication" & foc$spi_median >= cut)
))

pagers <- lapply(districts, function(d) {
  spi_field_guide_pager(
    fg,
    district = d,
    boundaries = synth_surveillance$boundaries,
    id_col = "adm2_guid",
    indicators_df = indicators,
    path = out_dir
  )
})

invisible(pagers)
