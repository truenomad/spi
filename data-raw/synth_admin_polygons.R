##################  spi -- build the synthetic boundary layer  ################
#
# Creates the synthetic admin-2 shapefile shipped with the package
# (inst/extdata/synth_admin_polygons.gpkg): the fictional country "Harad", with
# 36 provinces (adm1) and ~230 districts (adm2).
#
# Districts are REAL adm2 polygons from four bordering Lake Chad countries
# -- Nigeria, Niger, Chad, Cameroon -- merged, cropped to an organic region
# around the basin, and dissolved across the international borders into one
# landmass. They are then PARTITIONED into 36 contiguous provinces (SKATER) and
# RELABELLED so the shipped data is unambiguously fictional (no real place name
# survives). Finally the layer is ROTATED onto a local grid with its CRS dropped
# so the shape cannot be traced back to the real region. The result is a single,
# fully-connected neighbour graph for exercising spi's spatial models
# (spi_adjacency / spi_expected BYM2). The schema matches
# synth_surveillance$boundaries: adm2_guid, adm2_name, adm1_name, adm0_name.
#
# Run from the project root:
#   Rscript data-raw/synth_admin_polygons.R
#
# Source geometry: geoBoundaries gbOpen ADM2 (CC-BY 4.0; Runfola et al. 2020,
# https://www.geoboundaries.org). Attribution ships alongside the layer in
# inst/extdata/synth_admin_polygons.provenance.txt. Source GeoJSON is fetched
# once via GitHub's LFS media endpoint and cached under data-raw/ (gitignored).
###############################################################################

cli::cli_h1("spi -- build synthetic boundary layer")

## ---------------------------------------------------------------------------##
# Setup and parameters ---------------------------------------------------------
## ---------------------------------------------------------------------------##

source_countries <- c("NGA", "NER", "TCD", "CMR") # merged, then relabelled
crop_center <- c(14, 12.8) # clip-blob centre (Lake Chad quad-border), EPSG:4326
target_adm2 <- 220L # grow the clip blob until it holds at least this many units
blob_rough <- 0.30 # radial roughness of the organic clip blob (0 = circle)
blob_verts <- 72L # vertices around the clip blob
n_adm1 <- 36L # contiguous provinces to partition the districts into
min_prov_size <- 4L # SKATER: min districts per province (balances sizes)
proj_crs <- 32633L # WGS84 / UTM 33N (metres); Lake Chad sits in zone 33N
snap_m <- 500 # poly2nb snap (m): bridges separately-digitised border seams
gap_fill_km2 <- 20 # mapshaper -clean: absorb coverage gaps below this area (km2)
country <- "Harad"
rotate_deg <- 90 # de-identify: rotate the whole layer and drop the CRS
seed <- 123L
cache_dir <- "data-raw/geoboundaries_cache"
out_path <- "inst/extdata/synth_admin_polygons.gpkg"
prov_path <- "inst/extdata/synth_admin_polygons.provenance.txt"

set.seed(seed)
sf::sf_use_s2(FALSE) # planar ops on projected coords; silences s2 warnings

# Deterministic POLIS-style place GUID: md5 of the district name, upper case,
# formatted 8-4-4-4-12 and wrapped in braces, e.g.
# {E747913C-A787-BF03-56E3-76919A1220D5}. Reproducible (no RNG) so the ids are
# byte-stable across runs.
make_guid <- function(keys) {
  s <- toupper(vapply(keys, function(k) as.character(openssl::md5(k)),
                      character(1)))
  sprintf(
    "{%s-%s-%s-%s-%s}", substr(s, 1, 8), substr(s, 9, 12), substr(s, 13, 16),
    substr(s, 17, 20), substr(s, 21, 32)
  )
}

## ---------------------------------------------------------------------------##
# Fetch source adm2 (geoBoundaries, cached) ------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Fetch source adm2 boundaries")

# geoBoundaries stores GeoJSON via Git LFS, so the plain raw URL returns a
# pointer file -- media.githubusercontent.com/media/ resolves the real content.
lfs_url <- function(iso) {
  sprintf(
    paste0(
      "https://media.githubusercontent.com/media/wmgeolab/geoBoundaries/",
      "main/releaseData/gbOpen/%s/ADM2/geoBoundaries-%s-ADM2.geojson"
    ),
    iso, iso
  )
}

dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

read_source <- function(iso) {
  dest <- file.path(cache_dir, sprintf("%s_ADM2.geojson", iso))
  if (!file.exists(dest) || file.size(dest) < 1000) {
    cli::cli_alert_info("downloading {iso} adm2")
    utils::download.file(lfs_url(iso), dest, quiet = TRUE, mode = "wb")
  }
  sf::st_read(dest, quiet = TRUE) |>
    dplyr::transmute(src_iso = shapeGroup)
}

all_adm2 <- sf::st_make_valid(
  do.call(rbind, lapply(source_countries, read_source))
)

cli::cli_alert_info(
  "source adm2: {nrow(all_adm2)} polygons across {length(source_countries)} \\
   countries"
)

## ---------------------------------------------------------------------------##
# Crop to an organic region around the basin -----------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Crop to an organic region")

# Clip with a smooth, irregular blob (low-frequency radial noise) rather than a
# rectangle, so the country outline is natural rather than boxy. Whole adm2
# units are kept by centroid-inside-blob (clipping the geometry would fabricate
# straight edges); the blob keeps a fixed shape and only grows in radius until
# it holds >= target_adm2 units. Project to metres first so the blob, adjacency,
# and snapping are all planar.
all_adm2 <- sf::st_transform(all_adm2, proj_crs)
centroids <- suppressWarnings(sf::st_centroid(sf::st_geometry(all_adm2)))
center_xy <- sf::st_coordinates(
  sf::st_transform(sf::st_sfc(sf::st_point(crop_center), crs = 4326), proj_crs)
)

phases <- runif(3, 0, 2 * pi) # fixed once, so only the blob's size changes below
clip_blob <- function(radius) {
  th <- seq(0, 2 * pi, length.out = blob_verts + 1)
  rad <- radius * (1 + blob_rough * (
    0.5 * sin(2 * th + phases[1]) +
      0.3 * sin(3 * th + phases[2]) +
      0.2 * sin(5 * th + phases[3])
  ))
  xy <- cbind(center_xy[1] + rad * cos(th), center_xy[2] + rad * sin(th))
  xy[blob_verts + 1, ] <- xy[1, ]
  sf::st_sfc(sf::st_polygon(list(xy)), crs = proj_crs)
}

radius <- 360000
repeat {
  in_blob <- lengths(sf::st_intersects(centroids, clip_blob(radius))) > 0
  if (sum(in_blob) >= target_adm2) break
  radius <- radius * 1.05
}
patch <- all_adm2[in_blob, ]

cli::cli_alert_info(
  "cropped: {nrow(patch)} adm2 units (blob radius {round(radius / 1000)} km)"
)

## ---------------------------------------------------------------------------##
# Reduce to one connected landmass ---------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Reduce to one connected component")

# The four national files are digitised independently, so raw adjacency splits
# into one component per country. snap = 500 m bridges the border seams; we then
# keep the largest component so the neighbour graph is a single object.
comp <- spdep::n.comp.nb(spdep::poly2nb(patch, queen = TRUE, snap = snap_m))
largest <- as.integer(names(which.max(table(comp$comp.id))))
patch <- patch[comp$comp.id == largest, ]

cli::cli_alert_info(
  "components before merge: {comp$nc}; kept largest ({nrow(patch)} units)"
)

## ---------------------------------------------------------------------------##
# Clean into a shared-edge coverage (mapshaper -clean) -------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Clean the coverage")

# The four national files are digitised independently, so adjacent adm2 units do
# not share exact edges -- they leave <100 m gaps/overlaps along the former
# international borders. A plain vertex snap connects the adjacency graph but
# still leaves sliver polygons, which surface as artefacts when adm1 (province)
# outlines dissolved from this layer are overlaid on the adm2 districts.
# mapshaper's `-clean` rebuilds a proper topological coverage: adjacent polygons
# share identical edges, and gaps below gap_fill_km2 are absorbed into a
# neighbour -- so dissolving to adm1 downstream is artefact-free (0 sliver
# holes). mapshaper works on planar coordinates, so the metric CRS is unchanged.
clean_coverage <- function(x, gap_fill_km2) {
  crs0 <- sf::st_crs(x)
  gj <- geojsonsf::sf_geojson(x, atomise = FALSE)
  # rmapshaper exposes no public `-clean`, so call its command runner directly.
  cleaned <- rmapshaper:::apply_mapshaper_commands(
    data = gj,
    command = sprintf("-clean gap-fill-area=%d", as.integer(gap_fill_km2 * 1e6)),
    sys = FALSE
  )
  out <- geojsonsf::geojson_sf(as.character(cleaned))
  sf::st_crs(out) <- crs0
  sf::st_make_valid(out)
}
n_before <- nrow(patch)
patch <- clean_coverage(patch, gap_fill_km2)
stopifnot(nrow(patch) == n_before)

cli::cli_alert_info(
  "cleaned coverage: {nrow(patch)} units, \\
   {spdep::n.comp.nb(spdep::poly2nb(patch, queen = TRUE))$nc} component(s)"
)

## ---------------------------------------------------------------------------##
# Partition into contiguous provinces (SKATER) ---------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Partition into {n_adm1} contiguous provinces")

# SKATER cuts the minimum spanning tree of the adjacency graph, so every
# province is a CONTIGUOUS group of districts -- how real adm1 units nest adm2.
# Dissimilarity is the district centroids, so provinces come out spatially
# compact; crit + vec.crit force at least min_prov_size districts per province
# so no state is a lone district.
coords <- sf::st_coordinates(
  suppressWarnings(sf::st_centroid(sf::st_geometry(patch)))
)
prov_nb <- spdep::poly2nb(patch, queen = TRUE, snap = snap_m)
prov_lw <- spdep::nb2listw(prov_nb, glist = spdep::nbcosts(prov_nb, coords),
                           style = "B")
prov_mst <- spdep::mstree(prov_lw)
prov_id <- spdep::skater(
  prov_mst[, 1:2], coords, ncuts = n_adm1 - 1L,
  crit = min_prov_size, vec.crit = rep(1L, nrow(patch))
)$groups

prov_sizes <- table(prov_id)
cli::cli_alert_info(
  "province sizes: min {min(prov_sizes)}, median {stats::median(prov_sizes)}, \\
   max {max(prov_sizes)} districts"
)

## ---------------------------------------------------------------------------##
# Fictionalise: relabel country, provinces, districts --------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Relabel as fictional country {country}")

# Drop every real identifier. Districts and provinces get invented
# Haradwaith-flavoured names from one shuffled pool (provinces first, then
# districts), so no real place name survives and no name is reused across
# levels.
roots <- c(
  "Har", "Umbar", "Khand", "Bel", "Suz", "Kir", "Nur", "Tul", "Gar", "Vash",
  "Zim", "Ard", "Ost", "Nen", "Poros", "Chak", "Mun", "Dol", "Raen", "Sarn",
  "Tir", "Yol"
)
suffixes <- c(
  "ad", "or", "an", "eth", "is", "un", "ar", "il", "oth", "ai", "esh", "ora",
  "wen", "dor", "mar", "tha"
)
name_pool <- sample(
  do.call(paste0, expand.grid(roots, suffixes, stringsAsFactors = FALSE))
)
if (length(name_pool) < n_adm1 + nrow(patch)) {
  cli::cli_abort("name pool ({length(name_pool)}) too small for provinces + districts")
}
prov_names <- name_pool[seq_len(n_adm1)]
dist_names <- name_pool[n_adm1 + seq_len(nrow(patch))]

boundaries <- patch |>
  dplyr::mutate(
    adm0_name = country,
    adm1_name = prov_names[prov_id],
    adm2_name = dist_names[dplyr::row_number()],
    adm2_guid = make_guid(.data$adm2_name)
  ) |>
  dplyr::select(adm2_guid, adm2_name, adm1_name, adm0_name)

## ---------------------------------------------------------------------------##
# De-identify: rotate and drop the georeference --------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Rotate {rotate_deg} deg and drop the CRS")

# Rotate the whole layer about its centroid, shift it into the positive quadrant
# from a clean origin, and drop the CRS. Rotation is an isometry, so validity
# and the neighbour graph are unchanged, but the shape can no longer be
# reprojected back onto its real Lake Chad location.
rotate_layer <- function(x, degrees) {
  theta <- degrees * pi / 180
  rot <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
  g <- sf::st_geometry(x)
  g <- (g - sf::st_centroid(sf::st_union(g))) * rot
  g <- g - sf::st_bbox(g)[c("xmin", "ymin")]
  sf::st_set_geometry(x, sf::st_set_crs(g, NA))
}

boundaries <- rotate_layer(boundaries, rotate_deg)

## ---------------------------------------------------------------------------##
# Validate geometry + adjacency ------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Validate")

# No snap here: after welding, the shipped layer must be connected on its own.
nb <- spdep::poly2nb(boundaries, queen = TRUE)
stopifnot(
  nrow(boundaries) >= target_adm2,
  length(unique(boundaries$adm1_name)) == n_adm1,
  spdep::n.comp.nb(nb)$nc == 1L,
  sum(spdep::card(nb) == 0) == 0L,
  !anyNA(boundaries$adm1_name),
  all(sf::st_is_valid(boundaries))
)

cli::cli_alert_success(
  "{nrow(boundaries)} districts, {n_adm1} provinces, \\
   1 connected component, 0 islands"
)
cli::cli_alert_info(
  "neighbours: mean {round(mean(spdep::card(nb)), 2)} \\
   (range {min(spdep::card(nb))}-{max(spdep::card(nb))})"
)

## ---------------------------------------------------------------------------##
# Write layer + provenance -----------------------------------------------------
## ---------------------------------------------------------------------------##

cli::cli_h2("Write GeoPackage + provenance")

# GeoPackage, not .shp: shapefiles truncate field names to 10 chars and split
# into four sidecar files -- exactly what bites synthetic test columns.
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
sf::st_write(boundaries, out_path, delete_dsn = TRUE, quiet = TRUE)

writeLines(
  c(
    "synth_admin_polygons.gpkg -- synthetic adm2 boundaries for spi",
    "",
    "FICTIONAL. Country, province, and district names are invented; they do",
    "not correspond to any real place. Geometry is DERIVED from real adm2",
    "boundaries under a permissive licence:",
    "",
    "  Source: geoBoundaries gbOpen ADM2 (CC-BY 4.0)",
    "  Runfola, D. et al. (2020). geoBoundaries: A global database of political",
    "  administrative boundaries. PLoS ONE 15(4): e0231866.",
    "  https://www.geoboundaries.org",
    "",
    sprintf(
      "  Source countries (merged): %s",
      paste(source_countries, collapse = ", ")
    ),
    "  Cropped to an organic region around the Lake Chad basin, projected to",
    sprintf(
      "  EPSG:%d, dissolved across international borders, partitioned into %d",
      proj_crs, n_adm1
    ),
    "  contiguous provinces, and relabelled as the fictional country 'Harad'.",
    sprintf(
      "  Finally rotated %g degrees onto a local grid with the CRS dropped, so",
      rotate_deg
    ),
    "  the layer cannot be reprojected back to the source location."
  ),
  prov_path
)

cli::cli_alert_success("wrote {out_path}")
cli::cli_alert_success("wrote {prov_path}")

# Quick visual check (interactive only, so batch runs don't emit Rplots.pdf).
if (interactive()) {
  plot(boundaries["adm1_name"], main = country, key.pos = NULL)
}

# Finished ---------------------------------------------------------------------

cli::cli_rule(
  left = "Complete",
  right = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
