# Generate blindspot::synth_surveillance -- a self-contained toy AFP
# surveillance dataset for examples and tests. Run:
#   Rscript data-raw/synth_surveillance.R
# Writes data/synth_surveillance.rda. See ?synth_surveillance for the schema
# and the data-generating process.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(spdep)
  library(Matrix)
})

set.seed(20260702)

STUDY_YEAR_START <- 2015L
STUDY_YEAR_END   <- 2024L
N_DISTRICTS      <- 100L
# Low-incidence "False alarm" / "True shortfall" seeds: contiguous clusters
# below the NPAFP target, with a population boost for a stable SPI.
N_LOW_CLUSTERS   <- 2L
LOW_CLUSTER_SIZE <- 6L      # districts per contiguous cluster
LOW_BASELINE     <- -4.7    # log-scale baseline -> ~1.6 per 100k u15/yr (< 3)
LOW_POP_BOOST    <- 12      # denser population -> non-trivial absolute counts
N_LOW_LAGGARD    <- 7L      # low districts that never recover -> True shortfall

# Detection completeness = G_YEAR[t] * rel (profile-specific). G_YEAR improves
# to a pre-COVID peak, crashes ~30% in 2020, recovers by 2024.
G_YEAR <- c(0.78, 0.85, 0.90, 0.94, 0.97,   # 2015-2019: improving to a peak
            0.68, 0.72, 0.86, 0.93, 0.97)   # 2020 crash -> recovery by 2024
# Surveillance-profile counts (of N_DISTRICTS); "resilient" is the remainder.
N_EARLY_IMPROVER  <- 8L     # poor 2015-17, matured away by 2019
N_COVID_TRANSIENT <- 12L    # extra hit 2020-21, recovered by 2023
N_PERSIST_NORMAL  <- 6L     # normal-baseline laggards, degraded through 2024
WARDS_MIN        <- 5L      # each adm2 gets 5-8 wards
WARDS_MAX        <- 8L
N_ES_SITES       <- 30L     # ~30% of districts host an ES site
DHS_SHAPE_PATH   <- "data-raw/dhs_zz/subregional.shp"
COUNTRY_NAME     <- "Gondor"

# Ward-suffix pool, combined with the parent adm2 name (e.g. "Osgiliath Ford").
WARD_SUFFIXES <- c(
  "North Ward", "South Ward", "East Ward", "West Ward", "Central Ward",
  "Upper Ward", "Lower Ward", "Old Ward", "New Ward",
  "Ford", "Bridge", "Landing", "Crossing", "Reach", "Hollow",
  "Heights", "Vale", "Ridge", "Watch", "Marches",
  "Fields", "Woods", "Harbor", "Bend", "Foothills"
)

# 5 provinces of Gondor, ordered west -> east to match the longitude cut below.
PROVINCES <- c("Belfalas", "Lossarnach", "Lebennin", "Anorien", "Ithilien")

# Per-province district-name pools (canonical Tolkien toponyms + Sindarin-
# flavoured compounds); 25+ each so no province exhausts its pool.
DISTRICT_POOLS <- list(
  Belfalas = c(
    "Dol Amroth", "Edhellond", "Ras Morthil", "Andrast", "Cape Andrast",
    "Tolfalas", "Bay of Cobas", "Ost Amroth", "Cair Amroth", "Naith Belfalas",
    "Lond Amroth", "Amon Ereg", "Falas Ereg", "Emyn Belfalas", "Nen Amroth",
    "Lond Ereg", "Fen Amroth", "Rath Belfalas", "Emyn Amroth", "Amon Belfalas",
    "Cair Belfalas", "Nen Belfalas", "Naith Amroth", "Fen Belfalas",
    "Rath Amroth", "Lond Belfalas", "Falas Amroth", "Imlad Amroth"
  ),
  Lossarnach = c(
    "Imloth Melui", "Lossarnach Vale", "Nen Loss", "Amon Loss", "Ered Loss",
    "Rath Loss", "Ost Loss", "Fen Loss", "Vale of Snowbrook", "Cair Loss",
    "Emyn Loss", "Naith Loss", "Elensar", "Snowfoot", "Rath Elen",
    "Amon Elen", "Fen Elen", "Nen Elen", "Cair Elen", "Ost Elen",
    "Emyn Elen", "Naith Elen", "Falas Loss", "Lond Loss", "Imloth Baran",
    "Rath Baran", "Nen Baran", "Amon Baran"
  ),
  Lebennin = c(
    "Pelargir", "Linhir", "Ethring", "Erech", "Serni Fen",
    "Gilrain Ford", "Sirith Vale", "Ciril Estuary", "Ethir Anduin",
    "Naith Serni", "Lond Serni", "Cair Sirith", "Rath Ciril", "Ost Lebennin",
    "Nen Serni", "Fen Gilrain", "Erui Ford", "Emyn Gilrain", "Falas Sirith",
    "Amon Serni", "Fen Ciril", "Rath Serni", "Naith Ciril", "Ost Serni",
    "Amon Ciril", "Cair Ciril", "Lond Ciril", "Nen Ciril", "Emyn Serni",
    "Rath Gilrain"
  ),
  Anorien = c(
    "Amon Din", "Eilenach", "Nardol", "Erelas", "Min-Rimmon",
    "Calenhad", "Halifirien", "Amon Anwar", "Firien Wood", "Druadan Forest",
    "Mering Stream", "Cair Andros", "Anorien Field", "Ost Anor", "Fanuil Vale",
    "Amon Meth", "Naith Anduin", "Silvertine Vale", "Rath Celeb",
    "Ethir Sirith", "Fen Anor", "Nen Anor", "Amon Anor", "Rath Anor",
    "Naith Anor", "Cair Anor", "Lond Anor", "Emyn Anor", "Falas Anor",
    "Amon Fanuil"
  ),
  Ithilien = c(
    "Osgiliath", "Minas Ithil", "Emyn Arnen", "Henneth Annun", "Cormallen",
    "Ithilien Marches", "Poros", "Sarn Falath", "Cair Ithil", "Fen Hollen",
    "Imlad Morgul", "Rhun Ford", "Amon Ithil", "Nen Ithil", "Ephel Anwar",
    "Emyn Rhun", "Ithil Wood", "Naith Poros", "Ost Ithil", "Cair Andros South",
    "Amon Rhun", "Rath Ithil", "Fen Ithil", "Naith Ithil", "Emyn Ithil",
    "Lond Ithil", "Falas Ithil", "Ephel Rhun", "Amon Cormallen", "Nen Cormallen"
  )
)

# ---------------------------------------------------------------------------
# 1. Country outline -- REAL shape from sf::nc (100 counties of North Carolina)
# ---------------------------------------------------------------------------
# sf ships nc.shp as example data (Cressie's classic Bayesian-spatial teaching
# dataset). 100 polygons matches our target adm2 count exactly, licensing is
# permissive (MIT, redistributable), and no external fetch is needed. We keep
# the real shape (so the map looks convincing) but overlay Gondor labels (so
# the *data* is unambiguously fictional and no one confuses synthetic AFP
# counts with real North Carolina surveillance).
nc_path <- system.file("shape/nc.shp", package = "sf")
if (!nzchar(nc_path)) {
  stop("sf::nc not found (expected sf package to ship shape/nc.shp)")
}
nc <- sf::st_read(nc_path, quiet = TRUE)
nc <- sf::st_transform(nc, 4326)
if (nrow(nc) != N_DISTRICTS) {
  stop(sprintf("sf::nc has %d counties; expected %d", nrow(nc), N_DISTRICTS))
}

# Order counties west -> east by centroid longitude so the Gondor-province
# stripe cut (Belfalas westernmost, Ithilien easternmost) lands sensibly.
centroids_x <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(nc)))[, 1]
ord <- order(centroids_x)
nc_ordered <- nc[ord, ]
geom_final <- sf::st_geometry(nc_ordered)

# Assign each ordered polygon to a Gondor province by west-to-east longitude
# quantile. Cell 1 (westernmost) -> Belfalas; cell 100 (easternmost) -> Ithilien.
centroids_x_ord <- centroids_x[ord]
province_idx <- cut(centroids_x_ord, breaks = 5, labels = FALSE)
adm1_name <- PROVINCES[province_idx]

# Sample district names from each province's pool without replacement, so
# every polygon gets a canonical or plausible Gondor toponym.
adm2_name <- character(N_DISTRICTS)
for (p in PROVINCES) {
  in_p <- which(adm1_name == p)
  pool <- DISTRICT_POOLS[[p]]
  if (length(in_p) > length(pool)) {
    stop(sprintf(
      "province %s has %d cells but only %d pool names; enlarge the pool",
      p, length(in_p), length(pool)
    ))
  }
  adm2_name[in_p] <- sample(pool, length(in_p))
}

boundaries <- st_sf(
  adm2_guid = sprintf("GDR-%03d", seq_len(N_DISTRICTS)),
  adm2_name = adm2_name,
  adm1_name = adm1_name,
  adm0_name = COUNTRY_NAME,
  geometry  = geom_final
)

# ---------------------------------------------------------------------------
# 2b. Ward geometry -- subdivide each adm2 into 5-8 Voronoi wards
# ---------------------------------------------------------------------------
# Nested Voronoi: for each adm2 polygon, sample n_wards seed points inside,
# tessellate, clip. Ward ids are "<adm2_guid>-W<nn>"; names combine the
# parent adm2 name with a suffix from WARD_SUFFIXES.
build_wards <- function(adm2_row, ward_count) {
  parent_geom <- st_set_crs(st_geometry(adm2_row), NA)
  seeds <- st_sample(parent_geom, size = ward_count, type = "random")
  # top up if st_sample undershoots
  while (length(seeds) < ward_count) {
    seeds <- c(seeds, st_sample(parent_geom, size = ward_count - length(seeds),
                                type = "random"))
  }
  seeds <- seeds[seq_len(ward_count)]
  env <- st_as_sfc(st_bbox(parent_geom))
  vor <- st_sfc(st_collection_extract(
    st_voronoi(st_union(seeds), envelope = env), "POLYGON"
  ))
  clipped <- st_intersection(vor, parent_geom)
  # realign to seed order
  seed_geom <- st_geometry(seeds)
  idx <- integer(ward_count)
  for (i in seq_len(ward_count)) {
    hits <- st_intersects(clipped, seed_geom[i], sparse = FALSE)[, 1]
    idx[i] <- which(hits)[1]
  }
  st_set_crs(clipped[idx], 4326)
}

ward_rows <- vector("list", N_DISTRICTS)
for (i in seq_len(N_DISTRICTS)) {
  parent <- boundaries[i, ]
  n_wards <- sample(WARDS_MIN:WARDS_MAX, 1L)
  suffixes <- sample(WARD_SUFFIXES, n_wards)
  ward_geoms <- build_wards(parent, n_wards)
  ward_rows[[i]] <- st_sf(
    adm3_guid = sprintf("%s-W%02d", parent$adm2_guid, seq_len(n_wards)),
    adm3_name = paste(parent$adm2_name, suffixes),
    adm2_guid = parent$adm2_guid,
    adm2_name = parent$adm2_name,
    adm1_name = parent$adm1_name,
    adm0_name = parent$adm0_name,
    geometry  = ward_geoms
  )
}
ward_boundaries <- do.call(rbind, ward_rows)
row.names(ward_boundaries) <- NULL
message("wards built: ", nrow(ward_boundaries), " total (", WARDS_MIN, "-",
        WARDS_MAX, " per district)")

# ---------------------------------------------------------------------------
# 3. Adjacency (spdep::poly2nb directly -- same substrate as bs_adjacency())
# ---------------------------------------------------------------------------

adj_nb <- spdep::poly2nb(boundaries, queen = TRUE)
if (any(spdep::card(adj_nb) == 0)) {
  stop("Voronoi tessellation produced isolated cells -- reseed and retry")
}

# ICAR precision matrix Q = D - W, plus a small ridge so it's invertible.
W <- nb2mat(adj_nb, style = "B", zero.policy = TRUE)
D <- diag(rowSums(W))
Q <- D - W + diag(1e-4, N_DISTRICTS)

# ---------------------------------------------------------------------------
# 3b. Low-incidence clusters (False alarm / True shortfall seeds)
# ---------------------------------------------------------------------------
# Grow contiguous patches by breadth-first walk over the adjacency graph.
# Contiguity matters: an isolated low district is shrunk toward its neighbours
# by the ICAR prior (reads as a shortfall); a connected patch keeps the fitted
# expectation low too, so SPI matches the genuinely-low counts and passes.
grow_patch <- function(nb, seed, k) {
  patch <- seed
  frontier <- seed
  while (length(patch) < k) {
    nbrs <- setdiff(unique(unlist(nb[frontier])), patch)
    if (length(nbrs) == 0L) break
    take <- nbrs[seq_len(min(length(nbrs), k - length(patch)))]
    patch <- c(patch, take)
    frontier <- take
  }
  patch
}

low_seeds <- sample.int(N_DISTRICTS, N_LOW_CLUSTERS)
low_idx <- Reduce(
  union,
  lapply(low_seeds, grow_patch, nb = adj_nb, k = LOW_CLUSTER_SIZE)
)
low_ids <- boundaries$adm2_guid[low_idx]
message("low-incidence districts: ", length(low_idx), " across ",
        N_LOW_CLUSTERS, " clusters")

# ---------------------------------------------------------------------------
# 4. BYM2 spatial random effect
# ---------------------------------------------------------------------------

# structured (ICAR) component: sample from N(0, Q^-1), then re-scale so the
# geometric mean of marginal variances is 1 (Riebler et al. 2016).
Qinv <- solve(Q)
scale_factor <- exp(mean(log(diag(Qinv))))
Qinv_scaled <- Qinv / scale_factor

L <- chol(Qinv_scaled)                # upper triangular
phi_struct <- as.numeric(crossprod(L, rnorm(N_DISTRICTS)))
phi_iid    <- rnorm(N_DISTRICTS)

lambda <- 0.6                         # BYM2 mixing (0 = pure iid, 1 = pure ICAR)
tau_marginal <- 4.0                   # marginal precision
b_i <- (sqrt(lambda) * phi_struct + sqrt(1 - lambda) * phi_iid) / sqrt(tau_marginal)

# Override the low-incidence patch to a controlled low baseline (small jitter
# keeps the cluster spatially smooth). Overriding b_i -- rather than adding an
# offset to each district's random draw -- decouples the target rate from the
# spatial noise, so every low district lands below the conventional target.
b_i[low_idx] <- LOW_BASELINE + rnorm(length(low_idx), 0, 0.1)

# ---------------------------------------------------------------------------
# 5. Population (annual, 100 x 10)
# ---------------------------------------------------------------------------

pop_baseline <- rlnorm(N_DISTRICTS, meanlog = log(50000), sdlog = 0.6)
# Denser population in the low-incidence patch so a below-target per-capita
# rate still yields enough absolute counts for a stable (non-noisy) SPI.
pop_baseline[low_idx] <- pop_baseline[low_idx] * LOW_POP_BOOST
years <- STUDY_YEAR_START:STUDY_YEAR_END
population <- tidyr::expand_grid(
  adm2_guid = boundaries$adm2_guid,
  year      = years
) |>
  arrange(adm2_guid, year) |>
  mutate(
    yrs_since_start = year - STUDY_YEAR_START,
    pop_u15 = pop_baseline[match(adm2_guid, boundaries$adm2_guid)] *
      (1.02 ^ yrs_since_start)
  ) |>
  select(adm2_guid, year, pop_u15)

# ---------------------------------------------------------------------------
# 6. Surveillance-completeness surface + observed counts
# ---------------------------------------------------------------------------
# True incidence (alpha + b_i + season) is stable; the temporal signal lives in
# detection completeness = G_YEAR[t] * rel_it. The SPI model absorbs G_YEAR into
# its year effect, so SPI ~= rel_it: adequate when a district tracks the system
# (rel ~ 1), flagged when it falls behind (rel < ~0.8).

months <- seq(
  as.Date(sprintf("%d-01-01", STUDY_YEAR_START)),
  as.Date(sprintf("%d-12-01", STUDY_YEAR_END)),
  by = "1 month"
)
alpha <- log(15)                              # rate per 100k person-months (hot,
                                              #   so district-year SPI is stable)

# rel-completeness per profile, one value per study year (1 = tracks G_YEAR).
REL_TRAJECTORY <- list(
  resilient          = c(1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00),
  early_improver     = c(0.48, 0.60, 0.76, 0.90, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00),
  covid_transient    = c(1.00, 1.00, 1.00, 1.00, 1.00, 0.44, 0.52, 0.78, 1.00, 1.00),
  persistent_laggard = c(1.00, 1.00, 1.00, 1.00, 1.00, 0.40, 0.42, 0.48, 0.55, 0.62)
)

# Assign profiles: low-incidence laggards -> True shortfall, low-incidence
# resilient -> False alarm; normal-baseline laggards/transients/early-improvers
# -> False reassurance in their degraded windows.
low_laggard_ids <- sample(low_ids, N_LOW_LAGGARD)
picks <- sample(setdiff(boundaries$adm2_guid, low_ids),
                N_PERSIST_NORMAL + N_COVID_TRANSIENT + N_EARLY_IMPROVER)
profile <- setNames(rep("resilient", N_DISTRICTS), boundaries$adm2_guid)
profile[c(low_laggard_ids, picks[seq_len(N_PERSIST_NORMAL)])] <- "persistent_laggard"
profile[picks[N_PERSIST_NORMAL + seq_len(N_COVID_TRANSIENT)]] <- "covid_transient"
profile[picks[N_PERSIST_NORMAL + N_COVID_TRANSIENT + seq_len(N_EARLY_IMPROVER)]] <-
  "early_improver"

# district x year completeness matrix (jittered so severity varies), then long.
rel_mat <- t(vapply(profile, function(p) REL_TRAJECTORY[[p]], numeric(length(years))))
rel_mat <- pmin(rel_mat * exp(matrix(rnorm(length(rel_mat), 0, 0.05), nrow(rel_mat))), 1.05)
completeness_long <- tibble(
  adm2_guid    = rep(boundaries$adm2_guid, times = length(years)),
  year         = rep(years, each = N_DISTRICTS),
  rel          = as.numeric(rel_mat),
  completeness = pmin(pmax(as.numeric(sweep(rel_mat, 2, G_YEAR, `*`)), 0.05), 1.10)
)

grid <- tidyr::expand_grid(adm2_guid = boundaries$adm2_guid, month = months) |>
  mutate(
    year     = as.integer(format(month, "%Y")),
    month_no = as.integer(format(month, "%m"))
  ) |>
  left_join(population, by = c("adm2_guid", "year")) |>
  left_join(completeness_long, by = c("adm2_guid", "year")) |>
  mutate(
    b_i    = b_i[match(adm2_guid, boundaries$adm2_guid)],
    s_m    = 0.15 * sin(2 * pi * month_no / 12) + 0.10 * cos(2 * pi * month_no / 12),
    mu     = exp(alpha + b_i + s_m + log(pop_u15 / 1e5)),   # true burden
    mu_obs = mu * completeness,                             # what is detected
    count  = as.integer(rnbinom(dplyr::n(), size = 15, mu = mu_obs))
  )

cases <- grid |> select(adm2_guid, month, count)

# ---------------------------------------------------------------------------
# 7. Ground-truth cheat sheet
# ---------------------------------------------------------------------------
yr <- function(y) match(y, years)
truth <- tibble(
  adm2_guid            = boundaries$adm2_guid,
  surveillance_profile = unname(profile),
  is_blindspot         = unname(profile) != "resilient",
  is_low_incidence     = boundaries$adm2_guid %in% low_ids,
  covid_nadir          = round(rel_mat[, yr(2020L)], 3),
  recovered_2024       = rel_mat[, yr(2024L)] >= 0.8
)

# ---------------------------------------------------------------------------
# 8. Virus outcome -- cVDPV2 surfaces the year after a completeness gap
#    (rel < 0.6); background ~2%, post-gap ~35%.
# ---------------------------------------------------------------------------
virus_outcome <- completeness_long |>
  arrange(adm2_guid, year) |>
  group_by(adm2_guid) |>
  mutate(gap_prev = tidyr::replace_na(dplyr::lag(rel) < 0.6, FALSE)) |>
  ungroup() |>
  mutate(
    any_cvdpv2 = as.integer(rbinom(dplyr::n(), 1, ifelse(gap_prev, plogis(-0.6), plogis(-4)))),
    any_wpv1   = 0L,
    any_virus  = pmax(any_cvdpv2, any_wpv1)
  ) |>
  select(adm2_guid, year, any_wpv1, any_cvdpv2, any_virus)

# ---------------------------------------------------------------------------
# 9. Environmental surveillance (ES) -- N_ES_SITES sites (one per selected
#    district, at a random ward centroid), sampled monthly. Positivity shares
#    the completeness-gap signal with virus_outcome.
# ---------------------------------------------------------------------------
sites_per_province <- boundaries |>
  sf::st_drop_geometry() |>
  count(adm1_name, name = "n_districts") |>
  mutate(
    n_sites = pmax(1L, round(N_ES_SITES * n_districts / sum(n_districts)))
  )

es_hosts <- vector("list", nrow(sites_per_province))
for (i in seq_len(nrow(sites_per_province))) {
  p <- sites_per_province$adm1_name[i]
  k <- sites_per_province$n_sites[i]
  candidate_adm2 <- boundaries$adm2_guid[boundaries$adm1_name == p]
  es_hosts[[i]] <- sample(candidate_adm2, min(k, length(candidate_adm2)))
}
es_host_adm2 <- unlist(es_hosts)[seq_len(N_ES_SITES)]

# Pick a ward per host district; site geometry = centroid of that ward.
es_site_rows <- vector("list", N_ES_SITES)
for (i in seq_len(N_ES_SITES)) {
  guid <- es_host_adm2[i]
  wards_in <- ward_boundaries[ward_boundaries$adm2_guid == guid, ]
  pick <- sample(seq_len(nrow(wards_in)), 1L)
  pt <- st_centroid(st_geometry(wards_in[pick, ]))
  es_site_rows[[i]] <- st_sf(
    es_site_id = sprintf("ES-%03d", i),
    site_name  = paste(wards_in$adm3_name[pick], "ES Site"),
    adm3_guid  = wards_in$adm3_guid[pick],
    adm2_guid  = guid,
    adm1_name  = wards_in$adm1_name[pick],
    geometry   = pt
  )
}
es_sites <- do.call(rbind, es_site_rows)
row.names(es_sites) <- NULL

# Monthly sampling: one sample per site per month; positivity elevated where
# detection completeness is failing that year (rel < 0.6), shared signal with
# virus_outcome.
es_data <- tidyr::expand_grid(
  es_site_id  = es_sites$es_site_id,
  sample_date = months
) |>
  mutate(
    adm2_guid = es_sites$adm2_guid[match(es_site_id, es_sites$es_site_id)],
    year      = as.integer(format(sample_date, "%Y"))
  ) |>
  left_join(completeness_long |> select(adm2_guid, year, rel),
            by = c("adm2_guid", "year")) |>
  mutate(
    p_positive      = ifelse(rel < 0.6, plogis(-2), plogis(-5)),
    positive_cvdpv2 = as.integer(rbinom(dplyr::n(), 1, p_positive))
  ) |>
  select(es_site_id, adm2_guid, sample_date, positive_cvdpv2)

# District-year rollup (matches paper's es_district_year branch).
es_district_year <- es_data |>
  mutate(year = as.integer(format(sample_date, "%Y"))) |>
  group_by(adm2_guid, year) |>
  summarise(
    n_samples  = dplyr::n(),
    n_positive = sum(positive_cvdpv2),
    .groups    = "drop"
  )

# ---------------------------------------------------------------------------
# 9b. District-level covariates for the adjusted bs_expected() spec
# ---------------------------------------------------------------------------
# Three district-year layers a real analysis would pull from DHS / HMIS /
# travel-time surfaces:
#   dtp3            DTP3 immunisation coverage (%)     - health-system reach
#   urban_prop      share of district that is urban    - structural, per district
#   travel_time_min median minutes to nearest facility - access to care
# Correlated with the planted blindspots (lower coverage, worse access) and
# population (bigger -> more urban) so the adjusted spec has real signal.
# travel_time_min is right-skewed (a log_transform candidate). Generated last so
# the RNG stream feeding the other elements is untouched.

lagging <- setNames(truth$is_blindspot, boundaries$adm2_guid)

pop0 <- population |>
  filter(year == STUDY_YEAR_START) |>
  arrange(match(adm2_guid, boundaries$adm2_guid))
urban <- setNames(
  plogis(
    0.9 * as.numeric(scale(log(pop0$pop_u15))) -
      0.7 * lagging[pop0$adm2_guid] +
      rnorm(N_DISTRICTS, 0, 0.5)
  ),
  pop0$adm2_guid
)

covariates <- population |>
  transmute(adm2_guid, year) |>
  mutate(
    yrs   = year - STUDY_YEAR_START,
    lag_i = as.numeric(lagging[adm2_guid]),
    urb   = urban[adm2_guid],
    covid = as.numeric(year %in% c(2020L, 2021L)),
    dtp3  = round(pmin(99, pmax(40,
      86 + 0.8 * yrs - 14 * lag_i + 10 * (urb - 0.5) -
        6 * covid + rnorm(dplyr::n(), 0, 2)
    )), 1),
    travel_time_min = round(pmax(8, exp(
      3.6 - 1.1 * urb + 0.5 * lag_i + 0.3 * covid + rnorm(dplyr::n(), 0, 0.25)
    )), 1),
    urban_prop = round(urb, 3)
  ) |>
  select(adm2_guid, year, dtp3, urban_prop, travel_time_min)

# ---------------------------------------------------------------------------
# 10. Assemble + save
# ---------------------------------------------------------------------------

synth_surveillance <- list(
  cases            = cases,
  population       = population,
  covariates       = covariates,
  boundaries       = boundaries,
  ward_boundaries  = ward_boundaries,
  virus_outcome    = virus_outcome,
  es_sites         = es_sites,
  es_data          = es_data,
  es_district_year = es_district_year,
  truth            = truth
)

cat("cases:            ", nrow(cases), "rows,",
    "count range [", min(cases$count), ",", max(cases$count), "]\n")
cat("population:       ", nrow(population), "rows,",
    "pop_u15 range [", round(min(population$pop_u15)), ",",
    round(max(population$pop_u15)), "]\n")
cat("covariates:       ", nrow(covariates), "district-years x 3 layers",
    "(dtp3, urban_prop, travel_time_min)\n")
cat("boundaries:       ", nrow(boundaries), "adm2 polygons\n")
cat("ward_boundaries:  ", nrow(ward_boundaries), "adm3 polygons\n")
cat("virus_outcome:    ", nrow(virus_outcome), "district-years,",
    sum(virus_outcome$any_cvdpv2), "cVDPV2-positive\n")
cat("es_sites:         ", nrow(es_sites), "sites\n")
cat("es_data:          ", nrow(es_data), "samples,",
    sum(es_data$positive_cvdpv2), "cVDPV2-positive\n")
cat("es_district_year: ", nrow(es_district_year), "district-years\n")
cat("truth:            ", sum(truth$is_blindspot), "blindspots,",
    sum(truth$is_low_incidence), "low-incidence; profiles:",
    paste(names(table(truth$surveillance_profile)),
          table(truth$surveillance_profile), sep = "=", collapse = " "), "\n")

usethis::use_data(synth_surveillance, overwrite = TRUE, compress = "xz")
