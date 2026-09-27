# Example `indicators_df` table for the pagers in pager_demo.R and README.Rmd.
# The notify-to-investigation and EV rates are simulated for illustration.

example_indicators <- function(fg, synth = synth_surveillance) {
  withr::local_seed(20260725)
  dyi <- fg$district_year
  nrows <- nrow(dyi)
  pct_of <- function(k, p) {
    out <- rep(NA_real_, length(k))
    ok <- k > 0
    out[ok] <- 100 * stats::rbinom(sum(ok), size = k[ok], prob = p) / k[ok]
    out
  }
  n_notify <- stats::rbinom(nrows, size = dyi$n_cases, prob = 0.86)

  # onset-to-notification is not published by POLIS, so it comes from the
  # bundle's district-year timeliness counts rather than a simulation
  tl <- synth$afp_timeliness[
    match(
      paste(dyi$adm2_guid, dyi$year),
      paste(synth$afp_timeliness$adm2_guid, synth$afp_timeliness$year)
    ),
  ]
  onset_pct <- ifelse(
    tl$n_assessable > 0, 100 * tl$n_within_7d / tl$n_assessable, NA_real_
  )

  # the rate, case counts and stool adequacy come from the guide, so the tiles
  # agree with the STEPS components on the same page
  tibble::tibble(
    guid = dyi$adm2_guid,
    name = dyi$adm2_name,
    year = as.integer(dyi$year),
    afp_cases = as.integer(dyi$n_cases),
    npafp_cases = as.integer(dyi$observed),
    npafp_rate = dyi$npafp_rate,
    # EV isolation is measured on environmental samples, so it is missing only
    # where the district has no ES site
    ev_rate = ifelse(
      dyi$adm2_guid %in% synth$es_sites$adm2_guid,
      pmin(95, pmax(5, 55 + stats::rnorm(nrows, 0, 14))),
      NA_real_
    ),
    stool_adequacy_cond_pct = dyi$pct_adequate,
    inv_timeliness_pct = pct_of(n_notify, 0.72),
    onset_notify_pct = onset_pct,
    inv_timeliness_n = n_notify,
    onset_notify_n = tl$n_assessable
  )
}
