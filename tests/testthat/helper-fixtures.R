# Fixture constructors that build the blindspot S3 objects directly, so the
# downstream functions (bs_spi, bs_concordance, bs_field_guide) can be tested
# without a live INLA fit. Every constructor mirrors the exact object contract
# the real functions produce (see bs_expected.R / bs_spi.R for the shapes).

# --- a blindspot_expected fixture -----------------------------------------
# A 24-district x 24-month panel with a posterior-draw matrix, rich enough to
# drive bs_spi() at every level and the print / summary / diagnostics methods.
make_expected <- function(id_col = "district_id",
                          overdispersion = "iid",
                          season = "harmonic",
                          covariates = FALSE,
                          cpo = TRUE,
                          ncomp = 1L,
                          n_draws = 60L,
                          seed = 1L) {
  set.seed(seed)
  n_dist <- 24L
  n_mon <- 24L
  ids <- sprintf("D%02d", seq_len(n_dist))
  months <- seq(as.Date("2015-01-01"), by = "month", length.out = n_mon)

  grid <- expand.grid(id = ids, month = months, stringsAsFactors = FALSE)
  grid <- grid[order(grid$id, grid$month), ]
  n_obs <- nrow(grid)
  di <- match(grid$id, ids)

  # per-district monthly expected mean, and observed/expected ratio spread
  # across the SPI bands (severe / moderate / acceptable / elevated)
  lambda <- seq(0.3, 6, length.out = n_dist)[di]
  ratio <- rep_len(c(0.2, 0.6, 0.9, 1.1, 1.6, 2.0), n_dist)[di]
  observed <- stats::rpois(n_obs, lambda * ratio)

  # draws matrix: each column is n_draws gamma samples about the expected mean
  k <- 6
  draws <- matrix(
    stats::rgamma(n_draws * n_obs, shape = k, rate = rep(k / lambda, each = n_draws)),
    nrow = n_draws
  )
  colnames(draws) <- paste(grid$id, format(grid$month, "%Y-%m"), sep = "|")

  qs <- matrixStats::colQuantiles(draws, probs = c(0.05, 0.10, 0.50, 0.90, 0.95))
  data_tbl <- tibble::tibble(
    !!id_col := grid$id,
    month = grid$month,
    count = as.integer(observed),
    pop = 1e5
  )
  summary_tbl <- tibble::tibble(
    !!id_col := grid$id,
    month = grid$month,
    count = as.integer(observed),
    pop = 1e5,
    expected_median = qs[, 3],
    expected_mean = matrixStats::colMeans2(draws),
    expected_q05 = qs[, 1],
    expected_q10 = qs[, 2],
    expected_q90 = qs[, 4],
    expected_q95 = qs[, 5]
  )

  # fake INLA model as plain nested lists (the S3 methods only index into it)
  fixed <- if (covariates) {
    data.frame(
      mean = c(0.0, 0.20, -0.15),
      sd = c(0.05, 0.05, 0.30),
      `0.025quant` = c(-0.1, 0.10, -0.80),
      `0.5quant` = c(0.0, 0.20, -0.15),
      `0.975quant` = c(0.1, 0.30, 0.40),
      check.names = FALSE,
      row.names = c("(Intercept)", "dtp3", "travel_time_min")
    )
  } else {
    data.frame(
      mean = 0.0, sd = 0.05,
      `0.025quant` = -0.1, `0.5quant` = 0.0, `0.975quant` = 0.1,
      check.names = FALSE, row.names = "(Intercept)"
    )
  }
  model <- list(
    dic = list(dic = 1234.5, p.eff = 42.3),
    waic = list(waic = 1250.7),
    summary.fixed = fixed,
    cpu.used = c(Total = 3.14)
  )

  # hyperparameters: rows depend on the spec so the variance-component and
  # overdispersion branches of print()/summary() are all reachable
  hp <- tibble::tibble(
    parameter = "tau_spatial", mean = 4, sd = 1, q025 = 2, q500 = 4, q975 = 7
  )
  hp <- rbind(hp, tibble::tibble(
    parameter = "phi_spatial", mean = 0.6, sd = 0.1,
    q025 = 0.4, q500 = 0.6, q975 = 0.8
  ))
  if (overdispersion == "iid") {
    hp <- rbind(hp, tibble::tibble(
      parameter = "tau_obs", mean = 5, sd = 1, q025 = 3, q500 = 5, q975 = 8
    ))
  } else if (overdispersion == "nb") {
    hp <- rbind(hp, tibble::tibble(
      parameter = "nb_size", mean = 3, sd = 1, q025 = 1, q500 = 3, q975 = 6
    ))
  }
  if (season == "rw2") {
    hp <- rbind(hp, tibble::tibble(
      parameter = "tau_season", mean = 9, sd = 2, q025 = 5, q500 = 9, q975 = 14
    ))
  }

  cpo_tbl <- if (cpo) {
    tibble::tibble(
      !!id_col := grid$id,
      month = grid$month,
      cpo = stats::runif(n_obs, 0.1, 0.9),
      pit = stats::runif(n_obs),
      failure = 0
    )
  } else {
    NULL
  }

  adjacency <- structure(list(), ncomp = list(nc = ncomp))

  structure(
    list(
      draws = draws,
      summary = summary_tbl,
      spatial_re = NULL,
      cpo = cpo_tbl,
      formula = count ~ 1,
      model = model,
      adjacency = adjacency,
      hyperparameters = hp,
      priors = list(phi = list(U = 0.5, alpha = 0.5),
                    precision = list(U = 1, alpha = 0.01)),
      cov_params = if (covariates) {
        list(dtp3 = list(mean = 80, sd = 10),
             travel_time_min = list(mean = 60, sd = 30))
      } else {
        list()
      },
      season = season,
      overdispersion = overdispersion,
      offset = list(pop_col = "pop_u15", granularity = "annual",
                    transform = "log(pop / 12)"),
      data = data_tbl,
      id_col = id_col,
      pop_col = "pop_u15",
      call = quote(bs_expected())
    ),
    class = "blindspot_expected"
  )
}

# --- a district-year blindspot_spi placed into all four concordance cells --
make_spi_dy <- function(id_col = "district_id", years = 2015:2016) {
  # A: Both adequate | B: True shortfall | C: False reassurance | D: False alarm
  # pop = 1e5 so npafp_rate == annual count; target 3 => count >= 3 adequate
  spec <- tibble::tibble(
    id = c("A", "B", "C", "D"),
    count = c(10L, 1L, 10L, 1L),
    spi = c(0.90, 0.50, 0.50, 0.90)
  )
  grid <- expand_spec(spec, years)

  summary_tbl <- tibble::tibble(
    !!id_col := grid$id,
    year = grid$year,
    observed = grid$count,
    expected_total = grid$count / grid$spi,
    spi_median = grid$spi,
    spi_q05 = pmax(grid$spi - 0.15, 0.01),
    spi_q95 = grid$spi + 0.15
  )
  # spi$data: annual case rows, summed inside bs_concordance
  data_tbl <- tibble::tibble(
    !!id_col := grid$id,
    year = grid$year,
    count = grid$count
  )
  structure(
    list(
      draws = NULL,
      summary = summary_tbl,
      level = "district_year",
      low_information = summary_tbl[0, ],
      totals = tibble::tibble(total_observed = sum(grid$count)),
      data = data_tbl,
      id_col = id_col,
      call = quote(bs_spi())
    ),
    class = "blindspot_spi"
  )
}

# tiny expand.grid over a per-district spec x years
expand_spec <- function(spec, years) {
  out <- spec[rep(seq_len(nrow(spec)), times = length(years)), ]
  out$year <- rep(years, each = nrow(spec))
  out
}

# matching population table for make_spi_dy()
make_population <- function(id_col = "district_id", years = 2015:2016,
                            ids = c("A", "B", "C", "D")) {
  tibble::tibble(
    !!id_col := rep(ids, times = length(years)),
    year = rep(years, each = length(ids)),
    pop_u15 = 1e5
  )
}

# --- a hand-built blindspot_nb adjacency ----------------------------------
# chain over the given ids plus one deliberate island (last id, no neighbours).
make_nb <- function(ids, island_last = TRUE) {
  n <- length(ids)
  nb <- vector("list", n)
  for (i in seq_len(n)) {
    neigh <- integer(0)
    if (i > 1L) neigh <- c(neigh, i - 1L)
    if (i < n) neigh <- c(neigh, i + 1L)
    nb[[i]] <- as.integer(neigh)
  }
  if (island_last) {
    # detach the last node: it and its former neighbour lose the link
    prev <- nb[[n]][1]
    nb[[n]] <- 0L
    nb[[prev]] <- setdiff(nb[[prev]], n)
    if (length(nb[[prev]]) == 0L) nb[[prev]] <- 0L
  }
  attr(nb, "region.id") <- as.character(ids)
  attr(nb, "ncomp") <- list(nc = if (island_last) 2L else 1L)
  class(nb) <- c("blindspot_nb", "nb")
  nb
}

# --- a district-month blindspot_spi for the out-of-grid seasonal signal ----
# seasonal_map: named vector id -> "blind" | "muted" | "present" | "none"
make_spi_month <- function(id_col, ids, years, seasonal_map) {
  rows <- list()
  for (id in ids) {
    kind <- seasonal_map[[id]] %||% "present"
    for (yr in years) {
      months <- seq(as.Date(sprintf("%d-01-01", yr)), by = "month",
                    length.out = 12)
      peak <- lubridate::month(months) %in% 5:8
      expected <- ifelse(peak, 4, 1)
      if (kind == "none") expected <- rep(0, 12)
      observed <- switch(
        kind,
        blind = ifelse(peak, 0, 1),
        muted = round(0.3 * expected),
        none = rep(0, 12),
        round(expected) # present
      )
      rows[[length(rows) + 1]] <- tibble::tibble(
        !!id_col := id,
        month = months,
        observed = as.numeric(observed),
        expected_total = as.numeric(expected)
      )
    }
  }
  summary_tbl <- dplyr::bind_rows(rows)
  structure(
    list(summary = summary_tbl, level = "district_month", id_col = id_col),
    class = "blindspot_spi"
  )
}

# --- a blindspot_concordance for the field guide --------------------------
# Six districts across six years, engineered to reach FLAG / WATCH / No action
# and to light up the trend / persistence / surroundings and out-of-grid
# seasonal / detection signals.
make_concordance <- function(id_col = "adm2_guid", spi_cut = 0.8,
                             npafp_target = 3) {
  years <- 2019:2024
  ny <- length(years)

  # per-district SPI trajectories (length ny, 2019..2024)
  traj <- list(
    FG1 = seq(0.75, 0.45, length.out = ny),   # falling, persistent, adequate
    FG2 = seq(0.70, 0.40, length.out = ny),   # falling, persistent (no genomic)
    FG3 = rep(0.60, ny),                        # below cut, wide CrI -> WATCH
    FG4 = rep(1.05, ny),                        # comfortably adequate
    FG5 = seq(0.9, 0.5, length.out = ny),      # island, falling
    FG6 = rep(0.95, ny)                         # steady, just above
  )
  # conventional NPAFP adequacy (rate) per district
  adequate <- c(FG1 = TRUE, FG2 = FALSE, FG3 = TRUE, FG4 = TRUE,
                FG5 = FALSE, FG6 = TRUE)

  rows <- list()
  for (d in names(traj)) {
    spi <- traj[[d]]
    # FLAG districts get a tight CrI (q95 < 1); WATCH keeps CrI touching 1
    q95 <- if (d %in% c("FG1", "FG2", "FG5")) spi + 0.10 else spi + 0.45
    rate <- if (adequate[[d]]) 5 else 1.5
    rows[[d]] <- tibble::tibble(
      !!id_col := d,
      adm2_name = paste0(d, " District"),
      adm1_name = "Province 1",
      year = years,
      observed = ifelse(spi < spi_cut, 2L, 12L),
      expected_total = 12,
      spi_median = spi,
      spi_q05 = pmax(spi - 0.15, 0.05),
      spi_q95 = q95,
      npafp_rate = rate,
      npafp_adequate = adequate[[d]]
    )
  }
  dy <- dplyr::bind_rows(rows)

  structure(
    list(
      district_year = dy,
      crosstab = NULL,
      metrics = NULL,
      by_stratum = NULL,
      thresholds = list(spi = spi_cut, npafp = npafp_target,
                        multiplier = 1e5),
      id_col = id_col,
      call = quote(bs_concordance())
    ),
    class = "blindspot_concordance"
  )
}

# genomic orphan table: FG1 and FG2 carry orphan detections, so both the
# "corroborated" (genomic) and "persistent" corrob picks are reachable.
make_genomic <- function(id_col = "adm2_guid") {
  tibble::tibble(
    !!id_col := c("FG1", "FG1", "FG2"),
    year = c(2022L, 2023L, 2021L),
    any_cvdpv2 = c(1L, 1L, 1L)
  )
}

# environmental-surveillance positives keyed by district-year, with an
# `n_positive` count column (mirrors synth_surveillance$es_district_year). FG3
# and FG5 carry positives so the out-of-grid ES channel (detect_es) is
# exercised.
make_es <- function(id_col = "adm2_guid") {
  tibble::tibble(
    !!id_col := c("FG3", "FG5"),
    year = c(2022L, 2023L),
    n_positive = c(1L, 2L)
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# --- a concordance with hand-set counts -----------------------------------
# make_concordance() fixes expected_total at 12, which is well-powered; these
# gates turn on the count basis, so they need districts specified case by case.
# `spec` is a named list of list(spi=, q95=, observed=, expected=), years ending
# 2024.
make_count_concordance <- function(spec, spi_cut = 0.8,
                                   id_col = "adm2_guid") {
  rows <- lapply(names(spec), function(d) {
    s <- spec[[d]]
    n <- length(s$spi)
    tibble::tibble(
      !!id_col := d,
      adm2_name = d,
      adm1_name = "Province 1",
      year = seq.int(2024 - n + 1, 2024),
      observed = rep_len(s$observed, n),
      expected_total = rep_len(s$expected, n),
      spi_median = s$spi,
      spi_q05 = pmax(s$spi - 0.15, 0),
      spi_q95 = s$q95,
      npafp_rate = 1.5,
      npafp_adequate = FALSE
    )
  })
  structure(
    list(
      district_year = dplyr::bind_rows(rows),
      crosstab = NULL,
      metrics = NULL,
      by_stratum = NULL,
      thresholds = list(spi = spi_cut, npafp = 3, multiplier = 1e5),
      id_col = id_col,
      call = quote(bs_concordance())
    ),
    class = "blindspot_concordance"
  )
}

# --- a conventional AFP indicator panel ------------------------------------
# Shaped as polished_indicators_adm2 plus the two columns that table does not
# carry: onset_notify_pct (derived from the bundle's own AFP timeliness counts,
# since POLIS publishes no such indicator, and it has no GPEI
# threshold) and the assessable counts behind the timeliness percentages.
# Percentages rest on the real case counts, so a district with no AFP cases
# carries NA and a district with one or two carries a volatile figure -- which
# is the case the pager's strip has to survive.
make_indicators <- function(fg = synth_field_guide, seed = 20260725) {
  withr::local_seed(seed)
  dy <- fg$district_year
  n <- nrow(dy)
  afp <- as.integer(dy$observed) + stats::rbinom(n, size = 2, prob = 0.12)
  pct_on <- function(k, p) {
    out <- rep(NA_real_, length(k))
    hit <- k > 0
    out[hit] <- 100 *
      stats::rbinom(sum(hit), size = k[hit], prob = p) / k[hit]
    out
  }
  n_ni <- stats::rbinom(n, size = afp, prob = 0.86)
  # onset-to-notification is the one indicator POLIS does not publish, so it is
  # derived from the bundle's district-year timeliness counts, not simulated
  tl <- synth_surveillance$afp_timeliness[
    match(
      paste(dy$adm2_guid, dy$year),
      paste(
        synth_surveillance$afp_timeliness$adm2_guid,
        synth_surveillance$afp_timeliness$year
      )
    ),
  ]
  tibble::tibble(
    country_iso3code = "HRD",
    guid = dy$adm2_guid,
    name = dy$adm2_name,
    year = as.integer(dy$year),
    afp_cases = afp,
    npafp_cases = as.integer(dy$observed),
    npafp_rate = dy$npafp_rate,
    # EV isolation is an ES measure, so it is missing where there is no ES site
    ev_rate = ifelse(
      dy$adm2_guid %in% synth_surveillance$es_sites$adm2_guid, 58, NA_real_
    ),
    stool_adequacy_cond_pct = pct_on(afp, 0.72),
    inv_timeliness_pct = pct_on(n_ni, 0.7),
    onset_notify_pct = ifelse(
      tl$n_assessable > 0, 100 * tl$n_within_7d / tl$n_assessable, NA_real_
    ),
    inv_timeliness_n = n_ni,
    onset_notify_n = tl$n_assessable
  )
}
