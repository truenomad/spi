# Render one-page SPI pagers from the packaged synthetic field guide.
#
# The pager is self-contained: hand it the field guide and the shapefile, and
# it builds the neighbour graph itself, draws a locator inset from the real
# geometry, and reads both detection channels (AFP + ES, already carried by the
# field guide) into S7. Each district is written as a self-contained HTML
# tear-sheet plus a PNG preview, auto-named
# spi_<adm0>_<adm1>_<adm2>_field_pager.{html,png}, into inst/examples/pager/ so
# the results can be reviewed in the repo. PNG output needs webshot2 and a
# headless Chrome.
#
#   source(system.file("examples/pager_demo.R", package = "blindspot"))
# or, from a source checkout:
#   Rscript inst/examples/pager_demo.R
#
# Operational note: the pager renders S4, the corroborator count and the
# verdict straight from the field-guide object -- it does not recompute them.
# So the one lever that keeps a volatile or endpoint-only year from reading as
# a "falling" trend on the page is `traj_alpha` at the *build* of the field
# guide, not at the pager call. `bs_field_guide()` defaults to `traj_alpha =
# NULL` (bare slope sign) to reproduce the paper's published flag counts; for
# operational pagers that people read, build the guide with the significance
# gate on so the trend test carries through:
#
#   fg <- bs_field_guide(conc, adjacency = adj, spi_month = cm,
#                        genomic = g, es = e, traj_alpha = 0.1)
#
# Keep bare slope for manuscript figures, gate the operational pagers. This
# synthetic gallery renders from the precomputed (bare-slope) `synth_field_guide`
# so it runs instantly without INLA; the one line above is the only change for
# a real operational run.

library(blindspot)

out_dir <- file.path("inst", "examples", "pager")

fg <- synth_field_guide
foc <- fg$focal
cut <- fg$thresholds$spi

# ES is an optional S7 channel; tolerate a field guide built without it
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
    path = out_dir
  )
})

invisible(pagers)
