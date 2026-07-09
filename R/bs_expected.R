#' Fit BYM2 Expected Rate Model
#'
#' @description
#' Fits an INLA BYM2 Poisson model to estimate expected case counts per
#' district-month. This is the core engine of the blindspot framework.
#'
#' @param cases Tibble with columns: the district identifier (see `id_col`,
#'   character), `month` (Date), `count` (integer). One row per district-month.
#'   Zero-count rows must be present explicitly.
#' @param population Tibble with the district identifier and `pop` (double).
#'   Granularity is auto-detected: if `month` (Date) is present, population is
#'   joined month-by-month and the offset is `log(pop)`; otherwise `year`
#'   (integer) is required and the offset is `log(pop / 12)`.
#' @param adjacency nb object OR sf object. If sf, adjacency is computed
#'   internally via [bs_adjacency()] using `id_col`. Values of the id column
#'   must match those in `cases`.
#' @param covariates Tibble with columns: the district identifier (see
#'   `id_col`), `month` (Date) OR `year` (int), plus one or more numeric
#'   covariate columns. If NULL, model uses offset + intercept + season +
#'   spatial. Covariates are standardised internally (mean=0, sd=1). Default:
#'   NULL.
#' @param id_col Character. Name of the district identifier column in `cases`,
#'   `population`, and `covariates`. Must match the `id_col` used when building
#'   `adjacency` via [bs_adjacency()]. The function renames this column to
#'   `district_id` internally and renames it back on output. Default:
#'   "district_id".
#' @param pop_col Character. Name of the denominator column in `population`.
#'   For AFP surveillance the at-risk population is under-15, so the default
#'   is `"pop_u15"`. Pass `"pop"` if your tibble carries total population,
#'   `"pop_u16"` for under-16, or any other column name to switch
#'   denominators without pre-renaming. The function renames this column to
#'   `pop` internally and aborts with a helpful hint if it isn't present.
#'   Default: "pop_u15".
#' @param season Character. Seasonal specification: "harmonic" (1st + 2nd order
#'   sin/cos, 4 terms), "rw2" (cyclic 2nd-order random walk, 12 knots),
#'   "monthly" (12 monthly fixed effects, January omitted), "none" (no seasonal
#'   component). Default: "harmonic".
#' @param overdispersion Character. Overdispersion mechanism: "iid"
#'   (Poisson-lognormal, iid N(0, sigma^2) on log scale per district-month,
#'   recommended), "nb" (negative binomial likelihood), "none" (plain Poisson,
#'   not recommended for sparse data). Default: "iid".
#' @param prior_phi Named list with elements `U` and `alpha` giving the BYM2
#'   mixing parameter PC prior `P(phi < U) = alpha`. Default:
#'   `list(U = 0.5, alpha = 0.5)` (agnostic, 50% chance phi below 0.5).
#' @param prior_precision Named list with elements `U` and `alpha` giving the
#'   marginal SD PC prior `P(1/sqrt(tau) > U) = alpha`. Default:
#'   `list(U = 1, alpha = 0.01)` (1% chance SD exceeds 1 on log scale).
#' @param n_draws Integer. Posterior draws from joint latent field, used for
#'   SPI uncertainty propagation downstream. Use 1000 for analysis, 100 for
#'   quick checks. Default: 1000L.
#' @param log_transform Character vector. Covariate column names to
#'   `log(1 + x)` transform. Typical: `c("facility_index", "conflict_events")`.
#'   Default: NULL.
#' @param keep_draws Logical. If TRUE (default), the full
#'   `n_draws x n_district_months` draws matrix is returned in
#'   `result$draws`. Set FALSE for large jobs to keep only the summary
#'   quantiles and save memory.
#' @param verbose Logical. Progress messages via cli. FALSE for batch jobs.
#'   Default: TRUE.
#' @param debug Logical. If TRUE, runs INLA in verbose mode (prints its raw
#'   stdout/stderr, including VB-correction notes), prints model
#'   diagnostics (CPO failure rate, hyperparameter posterior summary,
#'   sanity check that median expected count is comparable to median
#'   observed count), and skips muting INLA's compiled-binary messages.
#'   Use when results look off or when investigating convergence. Default:
#'   FALSE.
#' @param seed Integer. Random seed for `inla.posterior.sample()`, so posterior
#'   draws are reproducible across runs. Pass `NULL` to draw a fresh random
#'   seed each call. Default: 42L.
#'
#' @return Object of class `blindspot_expected`. A list containing:
#' \describe{
#'   \item{draws}{Matrix `[n_draws x n_district_months]` of posterior draws of
#'     `exp(eta)`, with `colnames` of the form `"<id>|YYYY-MM"`. NULL if
#'     `keep_draws = FALSE`.}
#'   \item{summary}{Tibble with the id column, `month`, `count`, `pop`,
#'     `expected_median`, `expected_q05`, `expected_q10`, `expected_q90`,
#'     `expected_q95`, `expected_mean`.}
#'   \item{formula}{INLA formula used.}
#'   \item{model}{Fitted INLA object.}
#'   \item{adjacency}{spdep nb object.}
#'   \item{hyperparameters}{Tibble with posterior summaries of tau, phi, sigma.}
#'   \item{priors}{List of prior specs used.}
#'   \item{cov_params}{Named list of `(mean, sd)` used to standardise each
#'     covariate, for round-tripping in downstream `predict()`.}
#'   \item{data}{Input data tibble (for downstream functions).}
#'   \item{id_col}{The id column name, echoed for downstream use.}
#'   \item{call}{Matched call.}
#' }
#'
#' @details
#' **Model specification.** The expected count model is a Poisson regression
#' with:
#' - Log person-time offset
#' - BYM2 spatial random effects (Riebler et al. 2016)
#' - Optional seasonal component
#' - Optional overdispersion (iid or negative binomial)
#' - Optional covariates
#'
#' **Input validation.** Executed before INLA runs:
#' 1. Required columns present and correct types
#' 2. No negative counts or NAs
#' 3. All district ids in cases appear in adjacency
#' 4. No duplicate district x month combinations
#' 5. Population coverage check
#' 6. Adjacency graph connectivity check
#' 7. Covariate validation
#'
#' **Computational notes.**
#' - Posterior sampling can be time-consuming for large datasets
#' - Use fewer draws for initial exploration
#' - INLA uses sparse matrix methods for efficiency
#'
#' **Interpreting INLA's diagnostic chatter (`debug = TRUE`).**
#' - `vb.correction aborted` / `iterative process seems to diverge` —
#'   INLA's variational Bayes correction is an *optional* refinement on
#'   top of the Laplace approximation. When it fails to converge, INLA
#'   returns the Laplace result, which is a valid (slightly less
#'   accurate) posterior. This message is informational, not an error.
#' - `Matrix is not positive definite` *during fitting* — usually means
#'   the BYM2 precision matrix is singular; check that the adjacency
#'   graph is symmetric and that disconnected components are handled
#'   (see [bs_adjacency()]).
#'
#' **CPO / PIT and `overdispersion = "iid"`.** When the model has an iid
#' effect per observation (the default), conditional predictive ordinate
#' (CPO) and the probability integral transform (PIT) are structurally
#' unreliable — INLA flags most observations as `failure = 1`. This is
#' an artifact of the model spec, not a sign of poor fit. For CPO-based
#' diagnostics, refit with `overdispersion = "nb"` or
#' `overdispersion = "none"`.
#'
#' @section Choosing a seasonal specification:
#' Surveillance counts often show within-year cycles driven by transmission
#' biology, climate, school terms, or reporting patterns. Picking the wrong
#' `season` term either under-fits (genuine cycles bleed into the spatial term
#' and bias risk estimates) or over-fits (noisy month-of-year effects steal
#' signal from the expected count).
#'
#' **Quick diagnostic.** Before fitting, plot the monthly average count to see
#' whether a cycle exists and what shape it has:
#'
#' \preformatted{
#' cases |>
#'   dplyr::mutate(m = lubridate::month(month)) |>
#'   dplyr::group_by(m) |>
#'   dplyr::summarise(mean = mean(count, na.rm = TRUE)) |>
#'   plot(type = "b")
#' }
#'
#' **How to choose:**
#' \itemize{
#'   \item *"none"* — no visible cycle, or counts look flat across months.
#'     Use for aseasonal outcomes (e.g. neonatal tetanus, DHIS2 reporting
#'     completeness when administratively driven).
#'   \item *"harmonic"* (default, 4 terms: sin/cos at 12 and 6 month periods)
#'     — one or two smooth peaks per year. Good for most VPDs. Most
#'     parsimonious option that still captures seasonality.
#'   \item *"rw2"* — cyclic 2nd-order random walk over 12 months. Smooth but
#'     arbitrary shape; lets the prior do the smoothing rather than imposing a
#'     sinusoid. Use when seasonality is real but asymmetric or multi-modal
#'     (e.g. cholera in some settings).
#'   \item *"monthly"* — 11 free monthly fixed effects (January as reference).
#'     Most flexible, no smoothing. Use only with plentiful data (~3+ years
#'     across most districts); otherwise monthly effects absorb noise.
#' }
#'
#' **If unsure, compare.** Fit two specifications and pick the lower DIC /
#' WAIC (printed in the verbose run). Differences > ~5 on either criterion are
#' meaningful; smaller is noise.
#'
#' **Disease-specific starting points** (acute case-based surveillance):
#' \tabular{ll}{
#'   AFP / polio                  \tab harmonic        \cr
#'   Measles                      \tab harmonic or rw2 \cr
#'   Meningitis (African belt)    \tab harmonic        \cr
#'   Yellow fever                 \tab harmonic        \cr
#'   Cholera                      \tab rw2 or monthly  \cr
#'   Neonatal tetanus             \tab none            \cr
#'   DHIS2 reporting completeness \tab none or rw2     \cr
#' }
#'
#' These are starting points, not prescriptions — always verify against the
#' monthly-average plot for your data and country, since seasonality varies by
#' climate zone and surveillance system.
#'
#' @references
#' Riebler A, et al. (2016). An intuitive Bayesian spatial model for disease
#' mapping that accounts for scaling. Statistical Methods in Medical Research,
#' 25(4), 1145-1165.
#'
#' @seealso [bs_spi()], [bs_adjacency()]
#'
#' @export
#' @examples
#' \dontrun{
#' data(synth_surveillance, package = "blindspot")
#'
#' fit <- bs_expected(
#'   cases = synth_surveillance$cases,
#'   population = synth_surveillance$population,
#'   adjacency = synth_surveillance$boundaries,
#'   covariates = synth_surveillance$covariates,
#'   season = "harmonic"
#' )
#'
#' print(fit)
#' }
bs_expected <- function(
  cases,
  population,
  adjacency,
  covariates = NULL,
  season = c("harmonic", "rw2", "monthly", "none"),
  overdispersion = c("iid", "nb", "none"),
  prior_phi = list(U = 0.5, alpha = 0.5),
  prior_precision = list(U = 1, alpha = 0.01),
  n_draws = 1000L,
  log_transform = NULL,
  id_col = "district_id",
  pop_col = "pop_u15",
  keep_draws = TRUE,
  verbose = TRUE,
  debug = FALSE,
  seed = 42L
) {
  # --- check required packages --------------------------
  .check_pkg(
    c(
      "INLA", "spdep", "dplyr", "tibble", "lubridate",
      "matrixStats", "cli"
    ),
    reason = "to fit the BYM2 expected rate model"
  )

  # --- argument validation ------------------------------
  season <- match.arg(season)
  overdispersion <- match.arg(overdispersion)

  .check_prior <- function(p, name, U_max = Inf) {
    if (
      !is.list(p) ||
        !all(c("U", "alpha") %in% names(p)) ||
        !is.numeric(p$U) ||
        !is.numeric(p$alpha)
    ) {
      cli::cli_abort(
        "{.arg {name}} must be a list with numeric {.field U} and \\
         {.field alpha} (e.g. {.code list(U = 0.5, alpha = 0.5)})."
      )
    }
    if (p$U <= 0 || p$U >= U_max) {
      cli::cli_abort(
        "{.arg {name}}$U must be in (0, {U_max}); got {p$U}."
      )
    }
    if (p$alpha <= 0 || p$alpha >= 1) {
      cli::cli_abort(
        "{.arg {name}}$alpha must be in (0, 1); got {p$alpha}."
      )
    }
  }
  .check_prior(prior_phi, "prior_phi", U_max = 1)
  .check_prior(prior_precision, "prior_precision", U_max = Inf)

  stopifnot(
    is.character(id_col),
    length(id_col) == 1,
    is.numeric(n_draws),
    length(n_draws) == 1,
    n_draws > 0,
    isTRUE(n_draws == round(n_draws)),
    is.logical(keep_draws),
    is.logical(verbose)
  )
  n_draws <- as.integer(n_draws)

  # --- normalise id column to internal name -------------
  # internally we always use "district_id"; rename inputs in
  # and rename back to the user's id_col on output
  if (id_col != "district_id") {
    if (!id_col %in% names(cases)) {
      cli::cli_abort("{.arg id_col} {.val {id_col}} not found in cases.")
    }
    if (!id_col %in% names(population)) {
      cli::cli_abort(
        "{.arg id_col} {.val {id_col}} not found in population."
      )
    }
    cases <- dplyr::rename(cases, district_id = !!id_col)
    population <- dplyr::rename(population, district_id = !!id_col)
    if (!is.null(covariates) && id_col %in% names(covariates)) {
      covariates <- dplyr::rename(covariates, district_id = !!id_col)
    }
  }

  # --- normalise pop column to internal name ------------
  # the pipeline always works on a column called `pop`; the user picks
  # which denominator (total, under-15, etc.) via pop_col. Validate
  # that the chosen column actually exists before we do anything else.
  stopifnot(is.character(pop_col), length(pop_col) == 1)
  if (!pop_col %in% names(population)) {
    avail <- setdiff(names(population), c("district_id", "month", "year"))
    cli::cli_abort(c(
      "{.arg pop_col} {.val {pop_col}} not found in {.arg population}.",
      "i" = "Available numeric columns: {.val {avail}}.",
      "i" = "Pass {.code pop_col = <name>} to pick the denominator."
    ))
  }
  if (pop_col != "pop") {
    # if a real `pop` column already exists, drop it so the rename
    # doesn't collide
    if ("pop" %in% names(population)) {
      population <- dplyr::select(population, -"pop")
    }
    population <- dplyr::rename(population, pop = !!pop_col)
  }

  # --- validate inputs ----------------------------------
  .validate_cases(cases)
  .validate_population(population, cases)

  # coerce count to integer (INLA Poisson expects integer-valued)
  if (!is.integer(cases$count)) {
    if (any(cases$count != round(cases$count), na.rm = TRUE)) {
      cli::cli_abort("{.code cases$count} must be integer-valued.")
    }
    cases$count <- as.integer(cases$count)
  }

  # --- handle sf adjacency input ------------------------
  if (inherits(adjacency, "sf")) {
    if (verbose) {
      cli::cli_alert_info("Computing adjacency from sf object.")
    }
    adjacency <- bs_adjacency(
      boundaries = adjacency,
      id_col = id_col
    )
  }

  stopifnot(inherits(adjacency, "nb"))

  district_ids <- attr(adjacency, "region.id")

  # check all case districts are in adjacency
  case_ids <- unique(cases$district_id)
  unmatched <- setdiff(case_ids, district_ids)

  if (length(unmatched) > 0) {
    cli::cli_abort(
      "{format(length(unmatched), big.mark = ',')} id(s) in cases not \\
       found in adjacency: {.val {utils::head(unmatched, 5)}}."
    )
  }

  # --- join population (auto-detect granularity) --------
  if (verbose) {
    cli::cli_progress_step("Preparing model data")
  }

  pop_monthly <- "month" %in%
    names(population) &&
    inherits(population$month, "Date")

  if (pop_monthly) {
    model_data <- dplyr::left_join(
      cases,
      population,
      by = c("district_id", "month")
    )
    model_data$year <- lubridate::year(model_data$month)
  } else {
    model_data <- cases |>
      dplyr::mutate(year = lubridate::year(month)) |>
      dplyr::left_join(population, by = c("district_id", "year"))
  }

  n_pop_miss <- sum(is.na(model_data$pop))
  if (n_pop_miss > 0) {
    cli::cli_abort(
      "{format(n_pop_miss, big.mark = ',')} district-months are missing \\
       population. Check coverage."
    )
  }

  # guard zero / negative population (log -> -Inf would break INLA)
  n_pop_zero <- sum(model_data$pop <= 0)
  if (n_pop_zero > 0) {
    cli::cli_alert_warning(
      "{format(n_pop_zero, big.mark = ',')} district-months have \\
       {.code pop <= 0}; flooring at 0.5 for the offset."
    )
    model_data$pop <- pmax(model_data$pop, 0.5)
  }

  # --- build indices and offset -------------------------
  offset_transform <- if (pop_monthly) "log(pop)" else "log(pop / 12)"
  pop_granularity <- if (pop_monthly) "monthly" else "annual"

  model_data <- model_data |>
    dplyr::mutate(
      idx_space = match(district_id, district_ids),
      idx_iid = dplyr::row_number(),
      log_offset = if (pop_monthly) log(pop) else log(pop / 12)
    )

  # --- seasonal terms -----------------------------------
  if (season == "harmonic") {
    model_data <- model_data |>
      dplyr::mutate(
        month_num = lubridate::month(month),
        sin12 = sin(2 * pi * month_num / 12),
        cos12 = cos(2 * pi * month_num / 12),
        sin6 = sin(4 * pi * month_num / 12),
        cos6 = cos(4 * pi * month_num / 12)
      )
  } else if (season == "monthly") {
    model_data <- model_data |>
      dplyr::mutate(
        month_factor = factor(lubridate::month(month), levels = 1:12)
      )
  } else if (season == "rw2") {
    model_data <- model_data |>
      dplyr::mutate(idx_season = lubridate::month(month))
  }

  # --- covariates ---------------------------------------
  cov_names <- NULL
  cov_params <- list()

  if (!is.null(covariates)) {
    join_by <- dplyr::intersect(names(model_data), names(covariates))

    model_data <- dplyr::left_join(model_data, covariates, by = join_by)

    cov_names <- setdiff(
      names(covariates),
      c("district_id", "month", "year")
    )

    # log transform specified columns
    if (!is.null(log_transform)) {
      for (lv in log_transform) {
        if (!lv %in% cov_names) {
          cli::cli_abort(
            "Log-transform column {.val {lv}} not found in covariates."
          )
        }
        model_data[[lv]] <- log(1 + model_data[[lv]])
      }
    }

    # standardise covariates (mean 0, sd 1); collect surviving names
    drop_cv <- character(0)
    for (cv in cov_names) {
      mu <- mean(model_data[[cv]], na.rm = TRUE)
      sd_val <- stats::sd(model_data[[cv]], na.rm = TRUE)
      if (sd_val < 1e-10) {
        cli::cli_alert_warning(
          "Covariate {.val {cv}} has zero variance; dropping."
        )
        drop_cv <- c(drop_cv, cv)
        next
      }
      model_data[[cv]] <- (model_data[[cv]] - mu) / sd_val
      cov_params[[cv]] <- list(mean = mu, sd = sd_val)
    }
    cov_names <- setdiff(cov_names, drop_cv)
  }

  # --- write adjacency graph for INLA -------------------
  adj_path <- tempfile(fileext = ".adj")
  on.exit(unlink(adj_path), add = TRUE)
  spdep::nb2INLA(adj_path, adjacency)
  adj_graph <- INLA::inla.read.graph(adj_path)

  # warn upfront if adjacency has disconnected components — the BYM2
  # `adjust.for.con.comp = TRUE` flag handles them, but it's worth
  # flagging that smoothing happens per component
  ncomp <- attr(adjacency, "ncomp")
  n_sub <- if (!is.null(ncomp)) ncomp$nc else 1L
  if (n_sub > 1 && verbose) {
    cli::cli_alert_info(
      "Adjacency has {n_sub} disjoint components; BYM2 will scale \\
       and smooth each separately."
    )
  }

  # --- build formula ------------------------------------
  if (verbose) {
    cli::cli_progress_step("Building INLA formula")
  }

  # bym2 spatial — adjust.for.con.comp handles disjoint subgraphs;
  # constr = TRUE imposes the sum-to-zero identifiability constraint
  form <- count ~ 1 +
    f(
      idx_space,
      model = "bym2",
      graph = adj_graph,
      scale.model = TRUE,
      constr = TRUE,
      adjust.for.con.comp = TRUE,
      hyper = list(
        prec = list(
          prior = "pc.prec",
          param = c(prior_precision$U, prior_precision$alpha)
        ),
        phi = list(
          prior = "pc",
          param = c(prior_phi$U, prior_phi$alpha)
        )
      )
    )

  # overdispersion
  if (overdispersion == "iid") {
    form <- stats::update(
      form,
      . ~ . +
        f(
          idx_iid,
          model = "iid",
          hyper = list(
            prec = list(prior = "pc.prec", param = c(1, 0.01))
          )
        )
    )
  }

  # seasonal component
  if (season == "harmonic") {
    form <- stats::update(form, . ~ . + sin12 + cos12 + sin6 + cos6)
  } else if (season == "monthly") {
    form <- stats::update(form, . ~ . + month_factor)
  } else if (season == "rw2") {
    form <- stats::update(
      form,
      . ~ . +
        f(
          idx_season,
          model = "rw2",
          cyclic = TRUE,
          hyper = list(
            prec = list(prior = "pc.prec", param = c(0.5, 0.01))
          )
        )
    )
  }

  # covariates as fixed effects
  if (length(cov_names) > 0) {
    form <- stats::update(
      form,
      stats::as.formula(
        paste(". ~ . +", paste(cov_names, collapse = " + "))
      )
    )
  }

  # --- fit INLA model -----------------------------------
  if (verbose) {
    cli::cli_progress_step("Fitting INLA model")
  }

  family <- if (overdispersion == "nb") "nbinomial" else "poisson"

  # silent = 2L mutes INLA's compiled-binary stdout (including the noisy
  # `vb.correction aborted` notes). debug = TRUE keeps INLA verbose so
  # users investigating fit issues can see everything.
  model <- tryCatch(
    INLA::inla(
      formula = form,
      family = family,
      data = as.data.frame(model_data),
      offset = log_offset,
      control.compute = list(
        dic = TRUE,
        waic = TRUE,
        cpo = TRUE,
        config = TRUE
      ),
      control.predictor = list(compute = TRUE, link = 1),
      verbose = debug,
      silent = if (debug) 0L else 2L
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      hints <- c(
        "i" = "Common causes: ill-specified priors, disconnected \\
               adjacency, zero-variance covariates, or sparse data."
      )
      if (grepl("positive definite", msg, ignore.case = TRUE)) {
        hints <- c(
          hints,
          "i" = "{.val Matrix is not positive definite} usually means \\
                 the BYM2 precision matrix is singular: check that the \\
                 adjacency is well-formed and that no covariate is \\
                 collinear with the intercept."
        )
      }
      cli::cli_abort(c(
        "INLA model fitting failed.",
        hints,
        "x" = msg
      ))
    }
  )

  # --- posterior draws ----------------------------------
  n_obs <- nrow(model_data)
  mb <- n_draws * n_obs * 8 / 1024^2
  if (mb > 500) {
    cli::cli_alert_warning(
      "Draws matrix is ~{format(round(mb), big.mark = ',')} MB; consider \\
       {.code keep_draws = FALSE} or fewer {.arg n_draws}."
    )
  }

  if (verbose) {
    cli::cli_progress_step(
      "Sampling {format(n_draws, big.mark = ',')} posterior draws \\
       ({format(n_obs, big.mark = ',')} obs)"
    )
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  post_samples <- INLA::inla.posterior.sample(
    n = n_draws,
    result = model
  )

  # hoist predictor row indices out of the loop
  pred_idx <- grep(
    "^Predictor:",
    rownames(post_samples[[1]]$latent)
  )[seq_len(n_obs)]

  # INLA's "Predictor:" rows already INCLUDE the offset (they are the
  # full linear predictor used in the likelihood). Do NOT add log_offset
  # again here — doing so would multiply expected counts by exp(offset)
  # = pop/12, which is catastrophically wrong.
  draw_matrix <- matrix(NA_real_, nrow = n_draws, ncol = n_obs)
  for (i in seq_len(n_draws)) {
    draw_matrix[i, ] <- exp(post_samples[[i]]$latent[pred_idx])
  }

  # label columns so users can identify them without model_data
  colnames(draw_matrix) <- paste(
    model_data$district_id,
    format(model_data$month, "%Y-%m"),
    sep = "|"
  )

  # --- build summary tibble (one pass via matrixStats) --
  if (verbose) {
    cli::cli_progress_step("Computing summaries")
  }

  qs <- matrixStats::colQuantiles(
    draw_matrix,
    probs = c(0.05, 0.10, 0.50, 0.90, 0.95)
  )
  expected_mean_vec <- matrixStats::colMeans2(draw_matrix)

  summary_tbl <- model_data |>
    dplyr::select(district_id, month, count, pop) |>
    dplyr::mutate(
      expected_median = qs[, 3],
      expected_mean = expected_mean_vec,
      expected_q05 = qs[, 1],
      expected_q10 = qs[, 2],
      expected_q90 = qs[, 4],
      expected_q95 = qs[, 5]
    )

  # hyperparameter summaries (relabelled to short names)
  hyper_tbl <- tibble::tibble(
    parameter = .clean_hyper_names(rownames(model$summary.hyperpar)),
    mean = model$summary.hyperpar$mean,
    sd = model$summary.hyperpar$sd,
    q025 = model$summary.hyperpar$`0.025quant`,
    q500 = model$summary.hyperpar$`0.5quant`,
    q975 = model$summary.hyperpar$`0.975quant`
  )

  # spatial random effects (BYM2 stores 2N rows: first N is the
  # combined effect, used for mapping)
  n_dist <- length(district_ids)
  sp_rand <- model$summary.random$idx_space
  spatial_re_tbl <- tibble::tibble(
    district_id = district_ids,
    spatial_re_mean = sp_rand$mean[seq_len(n_dist)],
    spatial_re_median = sp_rand$`0.5quant`[seq_len(n_dist)],
    spatial_re_q025 = sp_rand$`0.025quant`[seq_len(n_dist)],
    spatial_re_q975 = sp_rand$`0.975quant`[seq_len(n_dist)]
  )

  # CPO / PIT diagnostics (NULL-safe)
  cpo_tbl <- if (!is.null(model$cpo$cpo)) {
    tibble::tibble(
      district_id = model_data$district_id,
      month = model_data$month,
      cpo = model$cpo$cpo,
      pit = model$cpo$pit,
      failure = model$cpo$failure
    )
  } else {
    NULL
  }

  # --- rename canonical id back to user's id_col --------
  if (id_col != "district_id") {
    summary_tbl <- dplyr::rename(summary_tbl, !!id_col := district_id)
    model_data <- dplyr::rename(model_data, !!id_col := district_id)
    spatial_re_tbl <- dplyr::rename(spatial_re_tbl, !!id_col := district_id)
    if (!is.null(cpo_tbl)) {
      cpo_tbl <- dplyr::rename(cpo_tbl, !!id_col := district_id)
    }
  }

  # --- assemble output ----------------------------------
  result <- structure(
    list(
      draws = if (keep_draws) draw_matrix else NULL,
      summary = summary_tbl,
      spatial_re = spatial_re_tbl,
      cpo = cpo_tbl,
      formula = form,
      model = model,
      adjacency = adjacency,
      hyperparameters = hyper_tbl,
      priors = list(
        phi = prior_phi,
        precision = prior_precision
      ),
      cov_params = cov_params,
      season = season,
      overdispersion = overdispersion,
      offset = list(
        pop_col = pop_col,
        granularity = pop_granularity,
        transform = offset_transform
      ),
      data = model_data,
      id_col = id_col,
      pop_col = pop_col,
      call = match.call()
    ),
    class = "blindspot_expected"
  )

  if (verbose) {
    # close the last progress step before printing the summary line
    cli::cli_progress_done()

    n_d <- dplyr::n_distinct(model_data[[id_col]])
    n_m <- dplyr::n_distinct(model_data$month)
    dic_str <- .fmt_ic(model$dic$dic)
    waic_str <- .fmt_ic(model$waic$waic)
    n_d_str <- format(n_d, big.mark = ",")
    n_m_str <- format(n_m, big.mark = ",")

    cli::cli_alert_success(
      "Fitted {n_d_str} districts x {n_m_str} months | \\
       DIC: {dic_str} | WAIC: {waic_str}"
    )
  }

  # --- debug diagnostics --------------------------------
  if (debug) {
    .bs_expected_diagnostics(result)
  }

  result
}

# format DIC / WAIC with big marks, NA-safe
.fmt_ic <- function(x) {
  if (is.null(x) || !is.finite(x)) {
    return("NA")
  }
  format(round(x), big.mark = ",")
}

# Kolmogorov-Smirnov distance between PIT values and Uniform(0, 1).
# Returns NA when there are too few valid PIT values for a meaningful
# test (heuristic: < 100). NULL-safe via the caller.
# @noRd
.pit_ks <- function(cpo) {
  if (is.null(cpo) || is.null(cpo$pit) || is.null(cpo$failure)) {
    return(NA_real_)
  }
  pit_values <- cpo$pit[cpo$failure == 0]
  pit_values <- pit_values[!is.na(pit_values)]
  if (length(pit_values) < 100) {
    return(NA_real_)
  }
  suppressWarnings(
    stats::ks.test(pit_values, "punif")$statistic
  )
}

# map verbose INLA hyperparameter names to short labels
.clean_hyper_names <- function(nm) {
  replacements <- c(
    "Precision for idx_space" = "tau_spatial",
    "Phi for idx_space" = "phi_spatial",
    "Precision for idx_iid" = "tau_obs",
    "Precision for idx_season" = "tau_season",
    "size for the nbinomial observations" = "nb_size"
  )
  for (pat in names(replacements)) {
    nm <- sub(pat, replacements[[pat]], nm, fixed = TRUE)
  }
  nm
}

# debug diagnostics — print signals that tell the user whether the fit
# looks healthy. Run when bs_expected(..., debug = TRUE).
.bs_expected_diagnostics <- function(x) {
  fmt_int <- function(v) format(v, big.mark = ",")
  fmt_pct <- function(v) sprintf("%.1f%%", 100 * v)

  cli::cli_h2("Blindspot diagnostics")

  # --- observed vs predicted scale check ---
  med_obs <- stats::median(x$summary$count, na.rm = TRUE)
  med_exp <- stats::median(x$summary$expected_median, na.rm = TRUE)
  ratio <- if (med_obs == 0) NA_real_ else med_exp / med_obs

  cli::cli_h3("Scale sanity check (observed vs expected)")
  cli::cli_alert_info("Median observed count: {round(med_obs, 2)}.")
  cli::cli_alert_info("Median expected count: {round(med_exp, 2)}.")
  if (!is.na(ratio) && (ratio > 10 || ratio < 0.1)) {
    cli::cli_alert_danger(
      "Expected / observed ratio = {round(ratio, 2)} — model is off by \\
       >10x. Check the offset, family, and population units."
    )
  } else if (!is.na(ratio)) {
    cli::cli_alert_success(
      "Ratio = {round(ratio, 2)} (within an order of magnitude)."
    )
  }

  # --- CPO failures ---
  # CPO is unreliable when overdispersion = "iid" because one iid effect
  # per observation makes the leave-one-out approximation ill-defined.
  # In that case INLA flags ~all obs as failure = 1; that's structural,
  # NOT a sign of poor fit.
  if (!is.null(x$cpo)) {
    cli::cli_h3("CPO leave-one-out diagnostics")
    if (!is.null(x$overdispersion) && x$overdispersion == "iid") {
      cli::cli_alert_info(
        "Skipped: {.code overdispersion = \"iid\"} makes CPO/PIT \\
         structurally unreliable (one iid effect per observation). \\
         Refit with {.code overdispersion = \"nb\"} or \\
         {.code overdispersion = \"none\"} for CPO-based diagnostics."
      )
    } else {
      n_fail <- sum(x$cpo$failure > 0, na.rm = TRUE)
      pct <- n_fail / nrow(x$cpo)
      cli::cli_alert_info(
        "{fmt_int(n_fail)} of {fmt_int(nrow(x$cpo))} obs failed CPO \\
         ({fmt_pct(pct)})."
      )
      if (pct > 0.1) {
        cli::cli_alert_warning(
          "High CPO failure rate; the model fits some obs poorly. \\
           Consider checking outliers or tightening priors."
        )
      }
      pit <- x$cpo$pit[!is.na(x$cpo$pit)]
      if (length(pit) > 0) {
        pit_extreme <- mean(pit < 0.01 | pit > 0.99)
        cli::cli_alert_info(
          "Extreme PIT (<0.01 or >0.99): {fmt_pct(pit_extreme)} of obs."
        )
      }
    }
  }

  # --- hyperparameters: convergence-style summary ---
  cli::cli_h3("Hyperparameter posteriors")
  print(
    x$hyperparameters |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.numeric),
          \(v) signif(v, 3)
        )
      )
  )

  # --- BYM2 mixing interpretation ---
  phi_row <- x$hyperparameters[x$hyperparameters$parameter == "phi_spatial", ]
  if (nrow(phi_row) > 0) {
    phi_med <- phi_row$q500
    interp <- if (phi_med > 0.9) {
      "almost entirely structured (smooth)"
    } else if (phi_med > 0.5) {
      "mostly structured"
    } else if (phi_med > 0.1) {
      "mixed structured/iid"
    } else {
      "almost entirely iid (no spatial smoothing)"
    }
    cli::cli_alert_info(
      "BYM2 phi posterior median = {round(phi_med, 2)} ({interp})."
    )
  }

  # --- adjacency component reminder ---
  ncomp <- attr(x$adjacency, "ncomp")
  n_sub <- if (!is.null(ncomp)) ncomp$nc else 1L
  if (n_sub > 1) {
    cli::cli_alert_info(
      "Adjacency has {n_sub} disjoint components — spatial smoothing \\
       is per-component."
    )
  }

  # --- INLA timing ---
  if (!is.null(x$model$cpu.used)) {
    cli::cli_alert_info(
      "INLA CPU used: {round(x$model$cpu.used['Total'], 1)}s."
    )
  }

  invisible(x)
}

#' @export
print.blindspot_expected <- function(x, ...) {
  id_col <- if (is.null(x$id_col)) "district_id" else x$id_col
  fmt_int <- function(v) format(v, big.mark = ",")

  cli::cli_h2("Blindspot expected model")

  # --- fit metadata -------------------------------------
  n_obs <- nrow(x$data)
  n_dist <- dplyr::n_distinct(x$data[[id_col]])
  n_mon <- dplyr::n_distinct(x$data$month)

  cli::cli_alert_info(
    "Fit: {fmt_int(n_dist)} districts x {fmt_int(n_mon)} months \\
     | {fmt_int(n_obs)} observations."
  )

  season_str <- switch(
    if (is.null(x$season)) "harmonic" else x$season,
    none = "no seasonality",
    harmonic = "harmonic seasonality",
    rw2 = "rw2 seasonality",
    monthly = "monthly seasonality"
  )
  od_str <- switch(
    if (is.null(x$overdispersion)) "iid" else x$overdispersion,
    none = "Poisson",
    iid = "Poisson-lognormal",
    nb = "negative binomial"
  )
  cli::cli_alert_info(
    "Specification: BYM2 + {season_str} + {od_str}."
  )

  if (!is.null(x$offset)) {
    cli::cli_alert_info(
      "Offset: {.code {x$offset$transform}} from \\
       {.field {x$offset$pop_col}} ({x$offset$granularity})."
    )
  }

  if (!is.null(x$draws)) {
    cli::cli_alert_info(
      "Posterior draws: {fmt_int(nrow(x$draws))}."
    )
  }

  # --- calibration --------------------------------------
  cli::cli_h3("Calibration")

  total_obs <- sum(x$data$count, na.rm = TRUE)
  # draws are [n_draws x n_obs]; rowSums = total expected per draw
  total_exp <- if (!is.null(x$draws)) {
    stats::median(rowSums(x$draws))
  } else {
    sum(x$summary$expected_median, na.rm = TRUE)
  }
  ratio <- total_obs / total_exp
  ratio_pass <- if (is.finite(ratio) && abs(ratio - 1) < 0.1) {
    "pass"
  } else {
    "flag"
  }

  cli::cli_alert_info(
    "Observed: {fmt_int(total_obs)} | \\
     Expected (median): {fmt_int(round(total_exp))} | \\
     Ratio: {round(ratio, 2)} ({ratio_pass})."
  )

  dic_s <- .fmt_ic(x$model$dic$dic)
  waic_s <- .fmt_ic(x$model$waic$waic)
  p_eff <- x$model$dic$p.eff
  p_eff_str <- if (is.null(p_eff) || !is.finite(p_eff)) {
    "NA"
  } else {
    sprintf(
      "%s (%.1f%%)",
      fmt_int(round(p_eff)),
      p_eff / n_obs * 100
    )
  }
  cli::cli_alert_info(
    "DIC: {dic_s} | WAIC: {waic_s} | p_eff: {p_eff_str}."
  )

  if (!is.null(x$cpo)) {
    cpo_pct <- round(mean(x$cpo$failure == 0, na.rm = TRUE) * 100, 1)
    pit_ks <- .pit_ks(x$cpo)
    pit_str <- if (is.na(pit_ks)) "NA" else sprintf("%.3f", pit_ks)
    cli::cli_alert_info(
      "CPO valid: {cpo_pct}% | PIT KS: {pit_str}."
    )
  }

  # --- covariate effects --------------------------------
  fixed <- x$model$summary.fixed
  if (!is.null(fixed) && nrow(fixed) > 0) {
    eff <- tibble::as_tibble(fixed, rownames = "covariate") |>
      dplyr::filter(.data$covariate != "(Intercept)")

    if (nrow(eff) > 0) {
      cli::cli_h3("Covariate effects (rate ratios)")
      effects_tbl <- eff |>
        dplyr::transmute(
          covariate = .data$covariate,
          rr_median = round(exp(.data$`0.5quant`), 2),
          rr_95cri = paste0(
            round(exp(.data$`0.025quant`), 2),
            "-",
            round(exp(.data$`0.975quant`), 2)
          ),
          pct_change = round((exp(.data$`0.5quant`) - 1) * 100, 0),
          signif = dplyr::if_else(
            .data$`0.025quant` * .data$`0.975quant` > 0,
            "*",
            " "
          )
        )
      print(effects_tbl)
    }
  }

  # --- variance components ------------------------------
  cli::cli_h3("Variance components (SD on log scale)")

  hyper <- x$hyperparameters
  hyper_q500 <- function(name) {
    row <- hyper[hyper$parameter == name, ]
    if (nrow(row) == 0) NA_real_ else row$q500
  }

  tau_spatial <- hyper_q500("tau_spatial")
  phi_spatial <- hyper_q500("phi_spatial")
  tau_obs <- hyper_q500("tau_obs")
  nb_size <- hyper_q500("nb_size")
  tau_season <- hyper_q500("tau_season")

  if (!is.na(tau_spatial)) {
    cli::cli_alert_info(
      "Spatial (BYM2):    {round(1 / sqrt(tau_spatial), 2)}"
    )
  }
  if (!is.na(phi_spatial)) {
    cli::cli_alert_info(
      "Phi (proportion):  {round(phi_spatial, 2)}"
    )
  }
  if (!is.na(tau_season)) {
    cli::cli_alert_info(
      "Seasonal (rw2):    {round(1 / sqrt(tau_season), 2)}"
    )
  }
  if (!is.na(tau_obs)) {
    cli::cli_alert_info(
      "Overdispersion:    {round(1 / sqrt(tau_obs), 2)} (iid SD)"
    )
  }
  if (!is.na(nb_size)) {
    cli::cli_alert_info(
      "Overdispersion:    NB size = {round(nb_size, 2)}"
    )
  }

  cli::cli_alert_info(
    "Use {.fn summary} for full posterior intervals and diagnostic detail."
  )

  invisible(x)
}

#' Detailed summary of a fitted blindspot expected model
#'
#' @description
#' Returns an analysis-ready summary with two structured tibbles: covariate
#' effects on log and rate-ratio scale, and a calibration / fit diagnostics
#' table with pass/flag indicators. Mirrors the `lm()` / `glm()` pattern
#' where `print()` is a console-friendly headline and `summary()` is the
#' pipe-friendly object for downstream reporting.
#'
#' @param object Object of class `blindspot_expected`.
#' @param ... Ignored.
#'
#' @return A list of class `summary.blindspot_expected` containing:
#' \describe{
#'   \item{effects}{Tibble with one row per covariate: `log_median`,
#'     `log_q025`, `log_q975`, `rr_median`, `rr_q025`, `rr_q975`,
#'     `pct_change`, and a logical `signif` flag (TRUE when the 95\%
#'     credible interval excludes the null on the rate-ratio scale).
#'     NULL when the model has no covariates.}
#'   \item{diagnostics}{Tibble of fit and calibration metrics
#'     (`calibration_ratio`, `dic`, `waic`, `p_eff`, `p_eff_pct`,
#'     `cpo_valid_pct`, `pit_ks`, `sd_spatial`) with a logical `pass`
#'     column. `pass` is `NA` for metrics that have no pass/fail rule.}
#'   \item{call}{Matched call from the original fit.}
#' }
#'
#' @details
#' Pass rules used in the `diagnostics` table:
#' \itemize{
#'   \item `calibration_ratio`: pass if total observed / total expected is
#'     within 10\% of 1.
#'   \item `p_eff` / `p_eff_pct`: pass if effective parameters are < 20\% of
#'     the observation count (over-parameterisation flag).
#'   \item `cpo_valid_pct`: pass if > 50\% of observations have valid CPO.
#'   \item `pit_ks`: pass if Kolmogorov-Smirnov distance from uniform is
#'     < 0.30.
#' }
#'
#' CPO and PIT are structurally unreliable when `overdispersion = "iid"`
#' (one iid effect per observation); in that case `cpo_valid_pct` and
#' `pit_ks` will typically be `NA` or fail, and refitting with `"nb"` or
#' `"none"` is the way to get meaningful calibration diagnostics.
#'
#' @export
summary.blindspot_expected <- function(object, ...) {
  # --- covariate effects --------------------------------
  fixed <- object$model$summary.fixed
  effects <- NULL
  if (!is.null(fixed) && nrow(fixed) > 0) {
    eff <- tibble::as_tibble(fixed, rownames = "covariate") |>
      dplyr::filter(.data$covariate != "(Intercept)")
    if (nrow(eff) > 0) {
      effects <- eff |>
        dplyr::transmute(
          covariate = .data$covariate,
          log_median = .data$`0.5quant`,
          log_q025 = .data$`0.025quant`,
          log_q975 = .data$`0.975quant`,
          rr_median = exp(.data$`0.5quant`),
          rr_q025 = exp(.data$`0.025quant`),
          rr_q975 = exp(.data$`0.975quant`),
          pct_change = round((exp(.data$`0.5quant`) - 1) * 100, 1),
          signif = .data$`0.025quant` * .data$`0.975quant` > 0
        )
    }
  }

  # --- diagnostics --------------------------------------
  n_obs <- nrow(object$data)
  total_obs <- sum(object$data$count, na.rm = TRUE)
  total_exp <- if (!is.null(object$draws)) {
    stats::median(rowSums(object$draws))
  } else {
    sum(object$summary$expected_median, na.rm = TRUE)
  }
  calibration_ratio <- total_obs / total_exp

  p_eff <- object$model$dic$p.eff
  p_eff_pct <- if (is.null(p_eff) || !is.finite(p_eff)) {
    NA_real_
  } else {
    p_eff / n_obs * 100
  }

  cpo_valid_pct <- if (!is.null(object$cpo)) {
    mean(object$cpo$failure == 0, na.rm = TRUE) * 100
  } else {
    NA_real_
  }
  pit_ks <- .pit_ks(object$cpo)

  hyper <- object$hyperparameters
  hyper_q500 <- function(name) {
    row <- hyper[hyper$parameter == name, ]
    if (nrow(row) == 0) NA_real_ else row$q500
  }
  tau_spatial <- hyper_q500("tau_spatial")
  sd_spatial <- if (is.na(tau_spatial)) NA_real_ else 1 / sqrt(tau_spatial)

  safe_round <- function(v, digits = 0) {
    if (is.null(v) || length(v) == 0 || !is.finite(v)) {
      return(NA_real_)
    }
    round(v, digits)
  }

  diagnostics <- tibble::tibble(
    metric = c(
      "calibration_ratio",
      "dic",
      "waic",
      "p_eff",
      "p_eff_pct",
      "cpo_valid_pct",
      "pit_ks",
      "sd_spatial"
    ),
    value = c(
      safe_round(calibration_ratio, 3),
      safe_round(object$model$dic$dic),
      safe_round(object$model$waic$waic),
      safe_round(p_eff),
      safe_round(p_eff_pct, 2),
      safe_round(cpo_valid_pct, 1),
      safe_round(pit_ks, 3),
      safe_round(sd_spatial, 2)
    ),
    pass = c(
      is.finite(calibration_ratio) && abs(calibration_ratio - 1) < 0.1,
      NA,
      NA,
      if (is.na(p_eff_pct)) NA else p_eff_pct < 20,
      if (is.na(p_eff_pct)) NA else p_eff_pct < 20,
      if (is.na(cpo_valid_pct)) NA else cpo_valid_pct > 50,
      if (is.na(pit_ks)) NA else pit_ks < 0.30,
      NA
    )
  )

  out <- list(
    effects = effects,
    diagnostics = diagnostics,
    call = object$call
  )
  class(out) <- "summary.blindspot_expected"
  out
}

#' @export
print.summary.blindspot_expected <- function(x, ...) {
  cli::cli_h2("Covariate effects")
  if (is.null(x$effects)) {
    cli::cli_alert_info("No covariates in this fit (bare model).")
  } else {
    print(x$effects)
  }

  cli::cli_h2("Diagnostics")
  print(x$diagnostics)

  invisible(x)
}

#' Re-export tibble::as_tibble for method dispatch
#'
#' @importFrom tibble as_tibble
#' @name as_tibble
#' @export
NULL

#' Coerce a fitted blindspot expected model to a tibble
#'
#' @param x Object of class `blindspot_expected`.
#' @param ... Ignored.
#'
#' @return The `summary` tibble (one row per district-month with observed
#'   count, population, and posterior expected-count quantiles).
#'
#' @export
as_tibble.blindspot_expected <- function(x, ...) {
  x$summary
}

#' @export
as.data.frame.blindspot_expected <- function(x, ...) {
  as.data.frame(x$summary)
}

#' Compare blindspot expected models across overdispersion specifications
#'
#' @description
#' Fits the same model with different overdispersion specifications and
#' returns a side-by-side comparison of fit, complexity, and calibration
#' diagnostics. Used to justify the choice of overdispersion mechanism in
#' [bs_expected()].
#'
#' @param cases Tibble with the required columns for [bs_expected()].
#' @param population Tibble with population data.
#' @param adjacency nb object or sf object.
#' @param specs Character vector of overdispersion specifications to compare.
#'   Default: `c("none", "iid", "nb")`. Each must be a valid `overdispersion`
#'   argument to [bs_expected()].
#' @param ... Additional arguments passed to [bs_expected()] (e.g. `season`,
#'   `prior_phi`, `id_col`). Must not include `overdispersion`, `n_draws`,
#'   or `verbose`; those are controlled by this function.
#' @param n_draws Integer. Posterior draws per fit. Lower than the
#'   [bs_expected()] default for speed during comparison. Default: 200L.
#' @param verbose Logical. Print progress messages. Default: TRUE.
#'
#' @return Object of class `blindspot_comparison`. A list containing:
#' \describe{
#'   \item{fits}{Named list of `blindspot_expected` objects, keyed by spec.}
#'   \item{summary}{Tibble of key diagnostics, one row per specification.}
#'   \item{recommendation}{List with `choice` (recommended specification),
#'     `reasoning` (string explaining the choice), `excluded` (specs failing
#'     a diagnostic rule), and `survivors` (specs that passed all rules).}
#' }
#'
#' @details
#' For each spec the function refits [bs_expected()] and extracts: DIC, WAIC,
#' effective parameter count, spatial SD (`1/sqrt(tau_spatial)`), BYM2 phi
#' posterior median (with a "pegged" flag for boundary values), an
#' overdispersion SD (`1/sqrt(tau_obs)` for "iid", `1/sqrt(nb_size)` for "nb",
#' NA for "none"), the share of observations with valid CPO, and a
#' KS-distance-to-uniform PIT calibration statistic.
#'
#' **Decision rule.** The recommended spec is the one with the lowest WAIC,
#' but if a simpler spec (in the order `none > iid > nb`) is within 5 WAIC
#' units of the best, that simpler spec is preferred for parsimony.
#'
#' @seealso [bs_expected()]
#'
#' @export
#' @examples
#' \dontrun{
#' cmp <- bs_compare_overdispersion(
#'   cases = cases,
#'   population = pop_u15,
#'   adjacency = adj,
#'   id_col = "adm2_guid",
#'   season = "harmonic"
#' )
#' cmp$summary
#' cmp$recommendation
#' }
bs_compare_overdispersion <- function(
  cases,
  population,
  adjacency,
  specs = c("none", "iid", "nb"),
  ...,
  n_draws = 200L,
  verbose = TRUE
) {
  # --- check required packages --------------------------
  .check_pkg(
    c("purrr", "tibble", "dplyr", "cli"),
    reason = "to compare BYM2 fits across overdispersion specs"
  )

  stopifnot(
    all(specs %in% c("none", "iid", "nb")),
    length(specs) >= 2
  )

  # reject conflicting args in ...
  bad <- intersect(
    names(list(...)),
    c("overdispersion", "n_draws", "verbose")
  )
  if (length(bad) > 0) {
    cli::cli_abort(
      "Pass {.arg {bad}} as named arguments to \\
       {.fn bs_compare_overdispersion} directly, not via {.arg ...}."
    )
  }

  # --- fit each specification ---------------------------
  fits <- list()
  for (spec in specs) {
    if (verbose) {
      cli::cli_h2("Fitting overdispersion = {.val {spec}}")
    }

    fits[[spec]] <- bs_expected(
      cases = cases,
      population = population,
      adjacency = adjacency,
      overdispersion = spec,
      n_draws = n_draws,
      verbose = verbose,
      ...
    )
  }

  # --- extract diagnostics ------------------------------
  if (verbose) {
    cli::cli_h2("Comparing fits")
  }

  summary_tbl <- purrr::map_dfr(
    specs,
    \(spec) .extract_diagnostics(fits[[spec]], spec)
  )

  # --- apply decision rules -----------------------------
  recommendation <- .recommend_overdispersion(summary_tbl)

  # --- print comparison ---------------------------------
  if (verbose) {
    .print_comparison(summary_tbl, recommendation)
  }

  structure(
    list(
      fits = fits,
      summary = summary_tbl,
      recommendation = recommendation
    ),
    class = "blindspot_comparison"
  )
}

# extract key diagnostics from a single fit
# @noRd
.extract_diagnostics <- function(fit, spec) {
  model <- fit$model
  hyper <- fit$hyperparameters

  hyper_q500 <- function(name) {
    row <- hyper[hyper$parameter == name, ]
    if (nrow(row) == 0) NA_real_ else row$q500
  }

  tau_spatial <- hyper_q500("tau_spatial")
  phi_spatial <- hyper_q500("phi_spatial")
  tau_obs <- hyper_q500("tau_obs")
  nb_size <- hyper_q500("nb_size")

  sd_spatial <- if (!is.na(tau_spatial)) 1 / sqrt(tau_spatial) else NA_real_

  # extra SD on log scale (iid) or implied from nb size
  sd_extra <- if (!is.na(tau_obs)) {
    1 / sqrt(tau_obs)
  } else if (!is.na(nb_size)) {
    1 / sqrt(nb_size)
  } else {
    NA_real_
  }

  # phi pegged at boundary (0 or 1)
  phi_pegged <- !is.na(phi_spatial) &&
    (phi_spatial > 0.98 || phi_spatial < 0.02)

  # CPO validity and PIT calibration
  cpo_valid <- if (!is.null(fit$cpo)) {
    mean(fit$cpo$failure == 0, na.rm = TRUE)
  } else {
    NA_real_
  }

  pit_ks <- .pit_ks(fit$cpo)

  tibble::tibble(
    spec = spec,
    n_obs = nrow(fit$data),
    dic = model$dic$dic,
    waic = model$waic$waic,
    p_eff = model$dic$p.eff,
    sd_spatial = sd_spatial,
    phi_spatial = phi_spatial,
    phi_pegged = phi_pegged,
    sd_extra = sd_extra,
    cpo_valid = cpo_valid,
    pit_ks = pit_ks
  )
}

# sequential exclusion + best PIT calibration
#
# WAIC alone is misleading when one spec is degenerate (e.g. iid with one
# random effect per observation: p_eff approaches n, in-sample WAIC looks
# great, CPO is structurally broken). We exclude such specs first, then
# pick by calibration among the survivors.
#
# Rules (each independently disqualifying):
#   1. cpo_valid < 0.5     — CPO is unreliable for >half the obs
#   2. p_eff / n_obs > 0.2 — model has effectively one parameter per ~5 obs
#   3. phi_pegged          — BYM2 mixing parameter at boundary (model
#                            smell: variance forced into the wrong term)
#
# Among survivors, the lowest PIT KS distance from uniform wins.
# @noRd
.recommend_overdispersion <- function(summary_tbl) {
  spec <- summary_tbl$spec
  cpo_valid <- summary_tbl$cpo_valid
  p_eff <- summary_tbl$p_eff
  n_obs <- summary_tbl$n_obs
  phi_pegged <- summary_tbl$phi_pegged

  bad_cpo <- spec[!is.na(cpo_valid) & cpo_valid < 0.5]
  overfit <- spec[
    !is.na(p_eff) & !is.na(n_obs) & (p_eff / n_obs) > 0.2
  ]
  pegged <- spec[!is.na(phi_pegged) & phi_pegged]

  excluded <- unique(c(bad_cpo, overfit, pegged))
  survivors <- summary_tbl[!summary_tbl$spec %in% excluded, ]

  if (nrow(survivors) == 0) {
    return(list(
      choice = NA_character_,
      reasoning = paste0(
        "No specification passed diagnostic checks. Excluded: ",
        paste(excluded, collapse = ", "), "."
      ),
      excluded = excluded,
      survivors = character(0)
    ))
  }

  # best calibration among survivors (lowest PIT KS distance)
  survivors <- survivors[order(survivors$pit_ks), ]
  recommended <- survivors$spec[1]

  reasons <- character(0)
  if (length(bad_cpo) > 0) {
    reasons <- c(reasons, sprintf(
      "broken CPO (%s)", paste(bad_cpo, collapse = ", ")
    ))
  }
  if (length(overfit) > 0) {
    reasons <- c(reasons, sprintf(
      "overfit p_eff > 20%% of n (%s)",
      paste(overfit, collapse = ", ")
    ))
  }
  if (length(pegged) > 0) {
    reasons <- c(reasons, sprintf(
      "phi pegged at boundary (%s)",
      paste(pegged, collapse = ", ")
    ))
  }

  reasoning <- if (length(excluded) == 0) {
    sprintf(
      "All specs passed diagnostics; %s has the best PIT calibration.",
      recommended
    )
  } else {
    sprintf(
      "Excluded: %s. Best PIT calibration among survivors (%s): %s.",
      paste(reasons, collapse = "; "),
      paste(survivors$spec, collapse = ", "),
      recommended
    )
  }

  list(
    choice = recommended,
    reasoning = reasoning,
    excluded = excluded,
    survivors = survivors$spec
  )
}

# print the comparison table and the recommendation
# @noRd
.print_comparison <- function(summary_tbl, recommendation) {
  cli::cli_h2("Overdispersion comparison")

  # surface the overfit signal (p_eff / n) and hide n_obs from display
  display_tbl <- summary_tbl |>
    dplyr::mutate(p_eff_frac = .data$p_eff / .data$n_obs) |>
    dplyr::select(-"n_obs") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        \(v) signif(v, 4)
      )
    )
  print(display_tbl)

  cli::cli_h3("Recommendation")
  if (is.na(recommendation$choice)) {
    cli::cli_alert_danger(
      "No specification passed diagnostic checks."
    )
  } else {
    cli::cli_alert_success("Use {.val {recommendation$choice}}.")
  }
  cli::cli_alert_info(recommendation$reasoning)
  if (length(recommendation$survivors) > 1) {
    others <- setdiff(recommendation$survivors, recommendation$choice)
    if (length(others) > 0) {
      cli::cli_alert_info(
        "Other survivors: {.val {others}}."
      )
    }
  }
}

#' @export
print.blindspot_comparison <- function(x, ...) {
  .print_comparison(x$summary, x$recommendation)
  invisible(x)
}
