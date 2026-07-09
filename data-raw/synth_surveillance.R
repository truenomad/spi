# Generate blindspot::synth_surveillance -- a self-contained toy AFP
# surveillance dataset for package examples, tests, and the paper's promised
# reproducibility artifact. Run:
#   Rscript data-raw/synth_surveillance.R
# Writes data/synth_surveillance.rda (lazy-loaded via
# `data("synth_surveillance", package = "blindspot")`).
#
# Bundle contents mirror the v2 pipeline's blindspot_inputs schema so it's a
# drop-in surrogate for the real Nigeria bundle:
#   $cases         tibble(adm2_guid, month [Date], count [int])   full grid
#   $population    tibble(adm2_guid, year, pop_u15)                 annual
#   $boundaries    sf(adm2_guid, adm2_name, adm1_name, geometry)    EPSG:4326
#   $virus_outcome tibble(adm2_guid, year, any_wpv1, any_cvdpv2, any_virus)
#   $truth         tibble(adm2_guid, is_blindspot, planted_onset,
#                         detection_ratio)
#
# Geometry: if data-raw/dhs_zz/subregional.shp exists (maintainer drops it in
# manually -- DHS's download page needs interactive session, not scriptable),
# its outline is used as the country boundary; otherwise a synthetic country
# outline is generated. Either way we tessellate 100 Voronoi cells inside the
# outline -- the DHS shape (if present) contributes only the country outline.

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
N_BLINDSPOTS     <- 10L
WARDS_MIN        <- 5L      # each adm2 gets 5-8 wards
WARDS_MAX        <- 8L
N_ES_SITES       <- 30L     # ~30% of districts host an ES site
DHS_SHAPE_PATH   <- "data-raw/dhs_zz/subregional.shp"
COUNTRY_NAME     <- "Gondor"

# Ward-suffix pool -- generic subdistrict descriptors combined with the parent
# adm2 name to make plausible sub-districts, e.g. "Osgiliath Ford",
# "Dol Amroth North Ward". Bilingual: cardinal-directional English + Gondorian
# geographic terms so the ward-name flavour stays consistent with adm2.
WARD_SUFFIXES <- c(
  "North Ward", "South Ward", "East Ward", "West Ward", "Central Ward",
  "Upper Ward", "Lower Ward", "Old Ward", "New Ward",
  "Ford", "Bridge", "Landing", "Crossing", "Reach", "Hollow",
  "Heights", "Vale", "Ridge", "Watch", "Marches",
  "Fields", "Woods", "Harbor", "Bend", "Foothills"
)

# 5 provinces of Gondor, ordered west -> east to match a west-to-east
# Voronoi quantile cut of the outline. West-coast province is Belfalas
# (Dol Amroth / Anfalas coast), east-of-Anduin province is Ithilien.
PROVINCES <- c("Belfalas", "Lossarnach", "Lebennin", "Anorien", "Ithilien")

# District-name pools, drawn per province. Canonical Tolkien toponyms first,
# then Sindarin-flavoured compounds using amon-/nen-/cair-/dol-/rath-/naith-/
# ost-/fen-/lond-/emyn-/falas-/imlad- roots so we have enough names for any
# per-province count the Voronoi cut lands on. 25+ per pool guarantees we
# never exhaust one province before assignment finishes.
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

# ---------------------------------------------------------------------------
# 5. Population (annual, 100 x 10)
# ---------------------------------------------------------------------------

pop_baseline <- rlnorm(N_DISTRICTS, meanlog = log(50000), sdlog = 0.6)
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
# 6. Time grid + linear predictor
# ---------------------------------------------------------------------------

months <- seq(
  as.Date(sprintf("%d-01-01", STUDY_YEAR_START)),
  as.Date(sprintf("%d-12-01", STUDY_YEAR_END)),
  by = "1 month"
)
n_months <- length(months)                    # 120
year_effect <- rnorm(length(years), 0, 0.05)  # IID year

# alpha tuned so few district-years land at zero observed counts; SPI floor
# at 0 confuses Youden on a small (100 x 10) panel, so the toy runs hotter
# than the paper's real NGA rate. Larger real datasets (Nigeria: 774 x 120)
# have enough signal that the floor doesn't dominate; the toy compensates by
# lifting alpha so median mu per month lands near 3-4.
alpha <- log(15 / 5)                          # rate per 100k person-months

grid <- tidyr::expand_grid(
  adm2_guid = boundaries$adm2_guid,
  month     = months
) |>
  mutate(
    year     = as.integer(format(month, "%Y")),
    month_no = as.integer(format(month, "%m"))
  ) |>
  left_join(population, by = c("adm2_guid", "year")) |>
  mutate(
    b_i   = b_i[match(adm2_guid, boundaries$adm2_guid)],
    u_t   = year_effect[match(year, years)],
    s_m   = 0.15 * sin(2 * pi * month_no / 12) +
              0.10 * cos(2 * pi * month_no / 12),
    log_mu = alpha + b_i + u_t + s_m + log(pop_u15 / 1e5),
    mu     = exp(log_mu)
  )

# ---------------------------------------------------------------------------
# 7. Plant blindspots + draw counts
# ---------------------------------------------------------------------------

blindspot_ids <- sample(boundaries$adm2_guid, N_BLINDSPOTS)
truth <- tibble(
  adm2_guid = boundaries$adm2_guid,
  is_blindspot = adm2_guid %in% blindspot_ids,
  detection_ratio = ifelse(
    is_blindspot,
    sample(c(0.2, 0.3, 0.4), N_DISTRICTS, replace = TRUE),
    1.0
  ),
  planted_onset = as.Date(NA)
)
# planted onset: random month in second half of the study window
half_start <- as.Date(sprintf("%d-01-01", (STUDY_YEAR_START + STUDY_YEAR_END) %/% 2))
onset_candidates <- months[months >= half_start]
truth$planted_onset[truth$is_blindspot] <- sample(
  onset_candidates, N_BLINDSPOTS, replace = TRUE
)

grid <- grid |>
  left_join(truth, by = "adm2_guid") |>
  mutate(
    detection = ifelse(
      is_blindspot & month >= planted_onset,
      detection_ratio,
      1.0
    ),
    mu_obs = mu * detection,
    count  = rnbinom(dplyr::n(), size = 4, mu = mu_obs)
  )

cases <- grid |> select(adm2_guid, month, count)

# ---------------------------------------------------------------------------
# 8. Synthetic virus outcome (drives bs_classify()'s ROC threshold)
# ---------------------------------------------------------------------------

# Rule: planted blindspot districts, in years including or after their planted
# onset, have a substantially higher probability of a subsequent cVDPV2
# detection. Background rate is ~2%; blindspot rate post-onset is ~30%. This
# gives bs_classify's ROC a clear signal to recover; without it, the AUC hovers
# at chance and Youden picks a threshold of zero.
year_annual_true_gap <- grid |>
  mutate(true_mu = mu, obs_mu = mu_obs) |>
  group_by(adm2_guid, year) |>
  summarise(
    true_total = sum(true_mu),
    obs_total  = sum(obs_mu),
    gap        = true_total - obs_total,
    .groups    = "drop"
  )

virus_outcome <- year_annual_true_gap |>
  left_join(
    truth |> select(adm2_guid, is_blindspot, planted_onset),
    by = "adm2_guid"
  ) |>
  mutate(
    # Strict > so virus manifests one year AFTER blindspot activation --
    # matches the paper's semantic: surveillance failure at year T is a leading
    # indicator, virus turns up at year T+1. bs_classify(lag = 1L) then pairs
    # SPI(T) with virus(T+1) correctly.
    virus_active = is_blindspot &
      !is.na(planted_onset) &
      year > as.integer(format(planted_onset, "%Y")),
    p_cvdpv2 = ifelse(virus_active, plogis(1), plogis(-4)),
    any_cvdpv2 = as.integer(rbinom(dplyr::n(), 1, p_cvdpv2)),
    any_wpv1   = 0L,
    any_virus  = pmax(any_cvdpv2, any_wpv1)
  ) |>
  select(adm2_guid, year, any_wpv1, any_cvdpv2, any_virus)

# ---------------------------------------------------------------------------
# 9. Environmental surveillance (ES) -- point sites + monthly samples
# ---------------------------------------------------------------------------
# Draws N_ES_SITES sites weighted toward larger provinces (Lebennin / Anorien /
# Lossarnach), placed at the centroid of a random ward within each selected
# district. Monthly samples per site over the full study window. Positivity
# probability shares the same underlying gap signal that drives
# virus_outcome, so both proxies are internally consistent.
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

# Monthly sampling: one sample per site per month, positivity driven by the
# annual true-vs-observed gap (shared signal with virus_outcome).
es_data <- tidyr::expand_grid(
  es_site_id  = es_sites$es_site_id,
  sample_date = months
) |>
  mutate(
    adm2_guid = es_sites$adm2_guid[match(es_site_id, es_sites$es_site_id)],
    year      = as.integer(format(sample_date, "%Y"))
  ) |>
  left_join(
    truth |> select(adm2_guid, is_blindspot, planted_onset),
    by = "adm2_guid"
  ) |>
  mutate(
    blindspot_active = is_blindspot &
      !is.na(planted_onset) &
      sample_date >= planted_onset,
    p_positive       = ifelse(blindspot_active, plogis(-2), plogis(-5)),
    positive_cvdpv2  = as.integer(rbinom(dplyr::n(), 1, p_positive))
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
# 10. Assemble + save
# ---------------------------------------------------------------------------

synth_surveillance <- list(
  cases            = cases,
  population       = population,
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
cat("boundaries:       ", nrow(boundaries), "adm2 polygons\n")
cat("ward_boundaries:  ", nrow(ward_boundaries), "adm3 polygons\n")
cat("virus_outcome:    ", nrow(virus_outcome), "district-years,",
    sum(virus_outcome$any_cvdpv2), "cVDPV2-positive\n")
cat("es_sites:         ", nrow(es_sites), "sites\n")
cat("es_data:          ", nrow(es_data), "samples,",
    sum(es_data$positive_cvdpv2), "cVDPV2-positive\n")
cat("es_district_year: ", nrow(es_district_year), "district-years\n")
cat("truth:            ", sum(truth$is_blindspot), "planted blindspots\n")

usethis::use_data(synth_surveillance, overwrite = TRUE, compress = "xz")
