# bs_expected() splits into (a) pure-R validation + data prep, (b) the INLA
# fit, and (c) S3 methods / diagnostics that only index into the fitted object.
# (a) is exercised by error paths that abort before the fit; (c) by the
# make_expected() fixture (helper-fixtures.R); the fit itself by a few small,
# INLA-gated integration fits at the bottom.

# minimal valid inputs (annual population, connected adjacency)
mk_valid <- function(n_dist = 4L, id_col = "district_id") {
  ids <- sprintf("D%02d", seq_len(n_dist))
  months <- seq(as.Date("2015-01-01"), by = "month", length.out = 12)
  cases <- tibble::tibble(
    !!id_col := rep(ids, each = 12L),
    month = rep(months, n_dist),
    count = 1L
  )
  pop <- tibble::tibble(
    !!id_col := ids, year = 2015L, pop_u15 = 1e5
  )
  list(cases = cases, pop = pop, adj = make_nb(ids, island_last = FALSE),
       ids = ids)
}

# ---------------------------------------------------------------------------
# (a) validation error paths -- these abort before INLA is ever called
# ---------------------------------------------------------------------------

test_that("bs_expected rejects malformed priors", {
  # bs_expected() checks for INLA before it validates; mock that check away so
  # the pure-R validation branches run on CI where INLA is not installed. The
  # abort fires long before any INLA call is reached.
  local_mocked_bindings(.check_pkg = function(...) invisible(TRUE))
  v <- mk_valid()
  expect_error(
    bs_expected(v$cases, v$pop, v$adj, prior_phi = "nope"),
    "must be a list"
  )
  expect_error(
    bs_expected(v$cases, v$pop, v$adj, prior_phi = list(U = 2, alpha = 0.5)),
    "U must be in"
  )
  expect_error(
    bs_expected(v$cases, v$pop, v$adj, prior_phi = list(U = 0.5, alpha = 5)),
    "alpha must be in"
  )
  expect_error(
    bs_expected(v$cases, v$pop, v$adj,
                prior_precision = list(U = -1, alpha = 0.01)),
    "U must be in"
  )
  # year prior only validated when a year effect is requested
  expect_error(
    bs_expected(v$cases, v$pop, v$adj, year_effect = "iid",
                prior_precision_year = list(U = 0, alpha = 0.01)),
    "U must be in"
  )
})

test_that("bs_expected rejects bad n_draws and unknown spec strings", {
  local_mocked_bindings(.check_pkg = function(...) invisible(TRUE))
  v <- mk_valid()
  expect_error(bs_expected(v$cases, v$pop, v$adj, n_draws = 0))
  expect_error(bs_expected(v$cases, v$pop, v$adj, n_draws = 2.5))
  expect_error(bs_expected(v$cases, v$pop, v$adj, season = "weekly"))
  expect_error(bs_expected(v$cases, v$pop, v$adj, overdispersion = "banana"))
})

test_that("bs_expected validates id / pop columns", {
  local_mocked_bindings(.check_pkg = function(...) invisible(TRUE))
  v <- mk_valid()
  expect_error(
    bs_expected(v$cases, v$pop, v$adj, id_col = "not_here"),
    "not found in cases"
  )
  # id present in cases but not population
  cases2 <- dplyr::rename(v$cases, adm2_guid = district_id)
  expect_error(
    bs_expected(cases2, v$pop, v$adj, id_col = "adm2_guid"),
    "not found in population"
  )
  # pop_col missing is caught by the pre-flight bs_check_inputs()
  expect_error(
    bs_expected(v$cases, v$pop, v$adj, pop_col = "ghost"),
    "denominator column"
  )
})

test_that("bs_expected validates counts, coverage, and adjacency ids", {
  local_mocked_bindings(.check_pkg = function(...) invisible(TRUE))
  v <- mk_valid()

  # non-integer counts
  frac <- v$cases
  frac$count <- frac$count + 0.5
  expect_error(bs_expected(frac, v$pop, v$adj), "integer-valued")

  # population covers < 80% of district-years
  thin_pop <- v$pop[1, ]
  expect_error(bs_expected(v$cases, thin_pop, v$adj), "covers only")

  # id present in cases but missing from the shapefile / adjacency graph:
  # the pre-flight bs_check_inputs() catches this first (check = TRUE default)
  v8 <- mk_valid(8L)
  adj7 <- make_nb(v8$ids[1:7], island_last = FALSE)
  expect_error(bs_expected(v8$cases, v8$pop, adj7), "not found in")

  # a district-year with no population row (coverage still >= 80%)
  drop_pop <- v8$pop[v8$pop$district_id != "D08", ]
  expect_error(bs_expected(v8$cases, drop_pop, v8$adj), "missing population")
})

test_that("bs_expected validates covariate log-transform names", {
  local_mocked_bindings(.check_pkg = function(...) invisible(TRUE))
  v <- mk_valid()
  cov <- tibble::tibble(district_id = v$ids, year = 2015L, xcov = 1:4)
  expect_error(
    bs_expected(v$cases, v$pop, v$adj, covariates = cov,
                log_transform = "not_a_cov"),
    "Log-transform column"
  )
})

# ---------------------------------------------------------------------------
# (b) pre-flight input check wiring (check = TRUE) + the check-once guard
# ---------------------------------------------------------------------------

test_that("bs_expected aborts on error-level inputs before fitting", {
  local_mocked_bindings(.check_pkg = function(...) invisible(TRUE))
  v <- mk_valid()
  bad <- v$cases
  bad$count[1] <- -1L
  expect_error(bs_expected(bad, v$pop, v$adj), "Input validation")
})

test_that("check = FALSE skips the pre-flight reconciliation", {
  local_mocked_bindings(.check_pkg = function(...) invisible(TRUE))
  # an id in cases missing from the adjacency: with check = FALSE the pre-flight
  # is skipped and the fit's own adjacency guard fires instead
  v8 <- mk_valid(8L)
  adj7 <- make_nb(v8$ids[1:7], island_last = FALSE)
  expect_error(
    bs_expected(v8$cases, v8$pop, adj7, check = FALSE),
    "adjacency"
  )
})

test_that("bs_compare_overdispersion checks inputs exactly once", {
  n <- 0L
  local_mocked_bindings(
    .check_pkg = function(...) invisible(TRUE),
    bs_check_inputs = function(...) {
      n <<- n + 1L
      structure(
        list(ok = TRUE, n_warning = 0L, n_error = 0L),
        class = "blindspot_input_check"
      )
    },
    bs_expected = function(...) NULL
  )
  v <- mk_valid()
  # downstream diagnostics choke on the NULL mock fits; we only assert that the
  # single pre-flight ran before the per-spec (check = FALSE) fits
  try(
    bs_compare_overdispersion(
      v$cases, v$pop, v$adj, specs = c("none", "iid", "nb")
    ),
    silent = TRUE
  )
  expect_equal(n, 1L)
})

test_that("bs_compare_overdispersion rejects conflicting arguments", {
  v <- mk_valid()
  expect_error(
    bs_compare_overdispersion(v$cases, v$pop, v$adj, specs = c("none")),
    "length"
  )
  expect_error(
    bs_compare_overdispersion(v$cases, v$pop, v$adj,
                              overdispersion = "iid"),
    "named arguments"
  )
})

# ---------------------------------------------------------------------------
# (c) S3 methods + diagnostics + helpers, driven by the fixture
# ---------------------------------------------------------------------------

test_that("print / summary handle the covariate and bare specs", {
  bare <- make_expected(covariates = FALSE, overdispersion = "iid")
  expect_no_error(print(bare))
  expect_identical(print(bare), bare)

  s_bare <- summary(bare)
  expect_s3_class(s_bare, "summary.blindspot_expected")
  expect_null(s_bare$effects)                 # no covariates
  expect_no_error(print(s_bare))

  cov <- make_expected(covariates = TRUE, overdispersion = "nb",
                       season = "rw2")
  expect_no_error(print(cov))
  s_cov <- summary(cov)
  expect_s3_class(s_cov$effects, "tbl_df")     # covariate effects present
  expect_true("signif" %in% names(s_cov$effects))
  expect_no_error(print(s_cov))

  # coercion methods
  expect_s3_class(as_tibble(bare), "tbl_df")
  expect_s3_class(as.data.frame(bare), "data.frame")
})

test_that("print / summary fall back when draws are absent", {
  fit <- make_expected()
  fit$draws <- NULL
  expect_no_error(print(fit))            # uses summary$expected_median
  expect_no_error(summary(fit))

  # a NULL effective-parameter count prints as "NA"
  no_peff <- make_expected()
  no_peff$model$dic$p.eff <- NULL
  expect_no_error(print(no_peff))
  s <- summary(no_peff)
  expect_true(is.na(s$diagnostics$value[s$diagnostics$metric == "p_eff"]))
})

test_that("print handles the none-overdispersion / no-cpo path", {
  fit <- make_expected(overdispersion = "none", cpo = FALSE)
  expect_no_error(print(fit))
  s <- summary(fit)
  # cpo diagnostics are NA without a cpo table
  expect_true(is.na(s$diagnostics$value[s$diagnostics$metric == "cpo_valid_pct"]))
})

test_that(".bs_expected_diagnostics covers its branches", {
  # iid: CPO skipped as structurally unreliable; phi ~0.6 -> mostly structured
  expect_no_error(blindspot:::.bs_expected_diagnostics(
    make_expected(overdispersion = "iid")
  ))
  # nb + disconnected adjacency + high phi -> the CPO-failure & component paths
  nb_fit <- make_expected(overdispersion = "nb", ncomp = 2L)
  nb_fit$cpo$failure <- rep(c(0, 1), length.out = nrow(nb_fit$cpo))  # 50% fail
  nb_fit$hyperparameters$q500[nb_fit$hyperparameters$parameter ==
                                "phi_spatial"] <- 0.95
  expect_no_error(blindspot:::.bs_expected_diagnostics(nb_fit))

  # phi <= 0.1 -> the "almost entirely iid" interpretation
  iid_phi <- make_expected(overdispersion = "iid")
  iid_phi$hyperparameters$q500[iid_phi$hyperparameters$parameter ==
                                 "phi_spatial"] <- 0.05
  expect_no_error(blindspot:::.bs_expected_diagnostics(iid_phi))

  # ratio off by >10x triggers the danger alert
  off <- make_expected(overdispersion = "none")
  off$summary$expected_median <- off$summary$expected_median * 100
  expect_no_error(blindspot:::.bs_expected_diagnostics(off))

  # median observed count of 0 -> ratio is NA (neither success nor danger)
  zero <- make_expected(overdispersion = "none")
  zero$summary$count <- 0L
  expect_no_error(blindspot:::.bs_expected_diagnostics(zero))
})

test_that("small numeric helpers behave", {
  # .fmt_ic: NULL / non-finite -> "NA"; finite -> rounded with commas
  expect_equal(blindspot:::.fmt_ic(NULL), "NA")
  expect_equal(blindspot:::.fmt_ic(Inf), "NA")
  expect_equal(blindspot:::.fmt_ic(1234.6), "1,235")

  # .pit_ks: NULL / too-few valid PITs -> NA; enough -> a KS statistic
  expect_true(is.na(blindspot:::.pit_ks(NULL)))
  few <- list(pit = runif(20), failure = rep(0, 20))
  expect_true(is.na(blindspot:::.pit_ks(few)))
  many <- list(pit = runif(200), failure = rep(0, 200))
  expect_true(is.finite(blindspot:::.pit_ks(many)))

  # .clean_hyper_names maps the verbose INLA labels
  cleaned <- blindspot:::.clean_hyper_names(
    c("Precision for idx_space", "Phi for idx_space",
      "size for the nbinomial observations")
  )
  expect_equal(cleaned, c("tau_spatial", "phi_spatial", "nb_size"))
})

test_that("overdispersion comparison helpers cover recommend branches", {
  # base row passes every diagnostic; `...` overrides individual columns
  ok <- function(spec, pit, ...) {
    base <- tibble::tibble(
      spec = spec, n_obs = 1000, dic = 100, waic = 100, p_eff = 50,
      sd_spatial = 0.5, phi_spatial = 0.5, phi_pegged = FALSE,
      sd_extra = 0.3, cpo_valid = 0.9, pit_ks = pit
    )
    ov <- list(...)
    for (nm in names(ov)) base[[nm]] <- ov[[nm]]
    base
  }
  # all pass -> chosen by best PIT calibration
  all_pass <- dplyr::bind_rows(ok("none", 0.10), ok("iid", 0.05))
  rec1 <- blindspot:::.recommend_overdispersion(all_pass)
  expect_equal(rec1$choice, "iid")
  expect_match(rec1$reasoning, "All specs passed")

  # each disqualifier fires; a survivor remains
  excl <- dplyr::bind_rows(
    ok("none", 0.2, cpo_valid = 0.3),          # broken CPO
    ok("iid", 0.2, p_eff = 500),                # overfit (p_eff/n > 0.2)
    ok("nb", 0.15, phi_pegged = TRUE),          # phi pegged
    ok("clean", 0.05)                            # survives
  )
  rec2 <- blindspot:::.recommend_overdispersion(excl)
  expect_equal(rec2$choice, "clean")
  expect_true(all(c("none", "iid", "nb") %in% rec2$excluded))

  # nothing survives
  none_ok <- dplyr::bind_rows(ok("none", 0.2, cpo_valid = 0.1),
                              ok("iid", 0.2, cpo_valid = 0.1))
  rec3 <- blindspot:::.recommend_overdispersion(none_ok)
  expect_true(is.na(rec3$choice))
  expect_match(rec3$reasoning, "No specification passed")

  # print paths for both a comparison object and the internal printer
  cmp <- structure(
    list(fits = list(), summary = excl, recommendation = rec2),
    class = "blindspot_comparison"
  )
  expect_no_error(print(cmp))
  expect_identical(print(cmp), cmp)

  # a recommendation with multiple survivors prints the "other survivors" line
  cmp_multi <- structure(
    list(fits = list(), summary = all_pass, recommendation = rec1),
    class = "blindspot_comparison"
  )
  expect_no_error(print(cmp_multi))

  # .extract_diagnostics over each overdispersion flavour (sd_extra branches)
  for (od in c("iid", "nb", "none")) {
    d <- blindspot:::.extract_diagnostics(
      make_expected(overdispersion = od), od
    )
    expect_equal(d$spec, od)
    if (od == "none") expect_true(is.na(d$sd_extra))
  }
})

# ---------------------------------------------------------------------------
# (b) INLA-gated integration fits -- exercise the formula build + fit + draws.
# Small subsets keep each fit fast; fit_or_skip() skips on a runtime crash.
# ---------------------------------------------------------------------------

# small subset of the shipped synthetic bundle
sub_inputs <- function(n = 6L, n_month = 24L) {
  s <- synth_surveillance
  ids <- utils::head(unique(s$cases$adm2_guid), n)
  keep_months <- sort(unique(s$cases$month))[seq_len(n_month)]
  yrs <- sort(unique(lubridate::year(keep_months)))
  list(
    cases = s$cases[s$cases$adm2_guid %in% ids &
                      s$cases$month %in% keep_months, ],
    pop = s$population[s$population$adm2_guid %in% ids &
                         s$population$year %in% yrs, ],
    cov = s$covariates[s$covariates$adm2_guid %in% ids &
                         s$covariates$year %in% yrs, ],
    bnd = s$boundaries[s$boundaries$adm2_guid %in% ids, ],
    ids = ids
  )
}

test_that("bs_expected fits with covariates, iid year, and annual pop", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  d <- sub_inputs(6L, 24L)

  cov <- d$cov[, c("adm2_guid", "year", "dtp3", "travel_time_min")]
  cov$const <- 1                       # zero-variance -> dropped with a warning
  pop <- d$pop
  pop$pop_u15[1] <- 0                  # <= 0 -> floored at 0.5 for the offset
  pop$pop <- 1                          # a stray `pop` column is dropped on rename
  cases <- d$cases
  cases$count <- as.numeric(cases$count)  # integer-valued double -> coerced

  fit <- fit_or_skip(
    cases = cases, population = pop, adjacency = d$bnd,
    covariates = cov, id_col = "adm2_guid",
    season = "harmonic", year_effect = "iid", overdispersion = "iid",
    log_transform = "travel_time_min", n_draws = 30L, seed = 1L,
    verbose = FALSE
  )
  expect_s3_class(fit, "blindspot_expected")
  expect_equal(fit$id_col, "adm2_guid")
  expect_equal(ncol(fit$draws), nrow(fit$data))
  # the constant covariate was dropped, the real ones kept
  expect_false("const" %in% names(fit$cov_params))
  expect_no_error(print(fit))
  expect_no_error(summary(fit))
  expect_s3_class(as_tibble(fit), "tbl_df")
})

test_that("bs_expected fits rw2 season, rw1 year, nb, keep_draws=FALSE", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  d <- sub_inputs(6L, 24L)

  fit <- fit_or_skip(
    cases = d$cases, population = d$pop, adjacency = d$bnd,
    id_col = "adm2_guid", season = "rw2", year_effect = "rw1",
    overdispersion = "nb", keep_draws = FALSE, n_draws = 30L,
    seed = 2L, verbose = TRUE, debug = TRUE
  )
  expect_s3_class(fit, "blindspot_expected")
  expect_null(fit$draws)                       # keep_draws = FALSE
  expect_equal(fit$overdispersion, "nb")
})

test_that("bs_expected fits monthly population, monthly season, no od", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  d <- sub_inputs(6L, 24L)

  # expand annual pop to a monthly grid so the offset takes the monthly branch
  pop_m <- merge(
    unique(d$cases[, c("adm2_guid", "month")]),
    transform(d$pop, year = d$pop$year),
    by = "adm2_guid"
  )
  pop_m <- pop_m[lubridate::year(pop_m$month) == pop_m$year,
                 c("adm2_guid", "month", "pop_u15")]

  fit <- fit_or_skip(
    cases = d$cases, population = pop_m, adjacency = d$bnd,
    id_col = "adm2_guid", season = "monthly", year_effect = "none",
    overdispersion = "none", n_draws = 30L, seed = 3L, verbose = FALSE
  )
  expect_s3_class(fit, "blindspot_expected")
  expect_equal(fit$offset$granularity, "monthly")
})

test_that("bs_compare_overdispersion and auto selection run end to end", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  d <- sub_inputs(4L, 12L)

  cmp <- tryCatch(
    bs_compare_overdispersion(
      d$cases, d$pop, d$bnd, specs = c("none", "iid"),
      id_col = "adm2_guid", season = "none", n_draws = 20L, verbose = TRUE
    ),
    error = function(e) skip(paste("INLA unavailable:", conditionMessage(e)))
  )
  expect_s3_class(cmp, "blindspot_comparison")
  expect_equal(nrow(cmp$summary), 2L)
  expect_no_error(print(cmp))

  auto <- fit_or_skip(
    cases = d$cases, population = d$pop, adjacency = d$bnd,
    id_col = "adm2_guid", season = "none", overdispersion = "auto",
    n_draws = 20L, seed = 4L, verbose = FALSE
  )
  expect_s3_class(auto, "blindspot_expected")
  expect_true(auto$overdispersion %in% c("none", "iid", "nb"))
})

# ---------------------------------------------------------------------------
# (c) determinism -- a seeded fit must be reproducible run to run.
# Regression guard: `seed` used to be applied with set.seed() only, which
# steers the config draw but not the latent-field draw inside INLA's compiled
# code, so two "seeded" runs silently disagreed by enough to move SPI and flip
# field-guide verdicts.
# ---------------------------------------------------------------------------

test_that("seed argument is validated", {
  d <- sub_inputs(4L, 12L)
  bad <- function(s) {
    bs_expected(
      cases = d$cases, population = d$pop, adjacency = d$bnd,
      id_col = "adm2_guid", seed = s, verbose = FALSE
    )
  }
  expect_error(bad(-1L), "non-negative")
  expect_error(bad(2.5), "non-negative")
  expect_error(bad(c(1L, 2L)), "non-negative")
  expect_error(bad(NA_integer_), "non-negative")
})

test_that("a seeded fit is reproducible and leaves the caller's RNG alone", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  d <- sub_inputs(6L, 24L)

  run <- function() {
    fit_or_skip(
      cases = d$cases, population = d$pop, adjacency = d$bnd,
      id_col = "adm2_guid", season = "none", n_draws = 40L, seed = 7L,
      verbose = FALSE
    )
  }

  set.seed(99L)
  before <- .Random.seed
  a <- run()
  # bs_expected must not displace the stream the caller is drawing from
  expect_identical(.Random.seed, before)

  b <- run()
  # Agreement to tolerance, not bit-identity. INLA's mode-finding is not
  # bit-stable even pinned to one thread: two seeded fits of the same data in
  # the same session disagree in the sixth significant figure, measured at up
  # to 1.6e-6 relative over repeated runs, which made an expect_identical here
  # fail about one run in three.
  #
  # 1e-4 is two orders of magnitude above that observed drift and four below
  # the ~1e-2 scale at which the unseeded bug moved SPI and flipped verdicts,
  # so it still catches a regression of that bug with room to spare while
  # sitting far inside anything that could move a reading off the 0.80 or 1.0
  # cuts. Tightening it to 1e-6 reintroduces the flake.
  tol <- 1e-4
  expect_equal(a$summary$expected_mean, b$summary$expected_mean,
               tolerance = tol)
  expect_equal(a$summary$expected_median, b$summary$expected_median,
               tolerance = tol)
  expect_equal(a$draws, b$draws, tolerance = tol)
  # the fit is pinned to one thread alongside a seed, else it drifts too
  expect_identical(a$model$.args$num.threads, "1:1")
})

test_that("num_threads = NULL opts out of the serial pin", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  d <- sub_inputs(4L, 12L)

  # pin the global to a multithreaded value so "did we inherit it?" is a
  # question with a distinguishable answer even on a single-core runner
  old <- INLA::inla.getOption("num.threads")
  on.exit(INLA::inla.setOption(num.threads = old), add = TRUE)
  INLA::inla.setOption(num.threads = "2:1")

  fit <- suppressWarnings(fit_or_skip(
    cases = d$cases, population = d$pop, adjacency = d$bnd,
    id_col = "adm2_guid", season = "none", n_draws = 20L, seed = 7L,
    num_threads = NULL, verbose = FALSE
  ))
  expect_false(identical(fit$model$.args$num.threads, "1:1"))
})
