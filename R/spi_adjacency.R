#' Build spatial neighbour matrix from sf boundaries
#'
#' @description constructs an nb (neighbourhood) object from an sf polygon
#' layer. handles islands by assigning nearest neighbour. returns an object
#' compatible with INLA's BYM2 specification.
#'
#' @details
#' **What this is.** An `nb` object (from \pkg{spdep}) is a list of length n
#' (one slot per district) where slot `i` holds the integer indices of
#' district `i`'s neighbours. It is the graph form of a sparse adjacency
#' matrix W with `W[i, j] = 1` iff districts i and j share a boundary.
#'
#' **Why the package needs it.** The BYM2 spatial random effect fitted inside
#' [spi_expected()] decomposes risk into a structured component (correlated
#' across neighbours) and an unstructured component. The structured part is
#' a Gaussian Markov random field whose precision matrix is built directly
#' from this neighbour graph. Districts with no neighbours contribute no
#' spatial smoothing -- which is why we attach islands to their nearest
#' mainland district by default.
#'
#' **Contiguity rule.** Queen contiguity treats districts as neighbours if
#' they share *any* boundary point (edge or corner); rook requires a shared
#' edge. Queen is the usual choice for administrative polygons.
#'
#' @section Inspecting the output:
#' The returned object has a print/summary method from \pkg{spdep}. Quick
#' checks:
#'
#' \preformatted{
#' summary(adj)                   # link counts, component count, extremes
#' length(adj)                    # number of districts
#' spdep::card(adj) |> table()    # distribution of neighbour counts
#' which(spdep::card(adj) == 0)   # remaining islands (should be empty)
#' attr(adj, "region.id")         # district ids in graph order
#' adj[[1]]                       # integer indices of district 1's neighbours
#' }
#'
#' Plot the graph over the polygons:
#' \preformatted{
#' ctr <- sf::st_centroid(sf::st_geometry(boundaries))
#' plot(sf::st_geometry(boundaries), border = "grey80")
#' plot(adj, sf::st_coordinates(ctr), add = TRUE, col = "steelblue")
#' }
#'
#' **What to look for.**
#' \itemize{
#'   \item *Average neighbours* of roughly 4-7 is typical for admin-2
#'     layers. <2 suggests a bad CRS or a topology problem; >10 may
#'     indicate slivers or duplicated geometry.
#'   \item *Disjoint connected subgraphs* > 1 means the country splits into
#'     separate components. BYM2 still fits, but each component is
#'     smoothed independently -- fine if the extra components are small
#'     (islands, enclaves), worth investigating if a large region is
#'     unexpectedly cut off.
#'   \item *Remaining islands* (`card == 0`) should be empty when
#'     `handle_islands = TRUE`.
#' }
#'
#' @param boundaries sf object with polygon geometries.
#' @param id_col character. column name containing the unique district
#'   identifier.
#' @param contiguity character. "queen" (default) or "rook" contiguity rule.
#' @param handle_islands logical. if TRUE (default), disconnected components
#'   are assigned their nearest neighbour.
#' @param snap_tolerance numeric. tolerance for boundary matching in CRS
#'   units. default 1e-5.
#'
#' @return Object of class `nb` (\pkg{spdep}). A list of length
#'   `nrow(boundaries)` where element `i` is an integer vector of neighbour
#'   indices for district `i`. Key attributes:
#'   \itemize{
#'     \item `region.id` -- character vector of district ids in graph order;
#'       downstream functions use this to align `cases$district_id` with
#'       the spatial index.
#'     \item `type` -- "queen" or "rook".
#'     \item `sym` -- logical, TRUE for symmetric graphs (always TRUE for
#'       contiguity graphs; may become FALSE after island attachment via
#'       knn).
#'     \item `ncomp` -- list describing the connected components.
#'   }
#'
#' @seealso [spi_expected()], [spdep::poly2nb()], [spdep::summary.nb()]
#'
#' @export
spi_adjacency <- function(
  boundaries,
  id_col,
  contiguity = "queen",
  handle_islands = TRUE,
  snap_tolerance = 1e-5
) {
  # --- check required packages ---
  .check_pkg(
    c("sf", "spdep", "cli"),
    reason = "to build the spatial neighbour graph"
  )

  # --- validate inputs ---
  stopifnot(
    inherits(boundaries, "sf"),
    is.character(id_col),
    id_col %in% names(boundaries),
    contiguity %in% c("queen", "rook")
  )

  ids <- boundaries[[id_col]]

  if (any(duplicated(ids))) {
    cli::cli_abort(
      "duplicate values in {.arg {id_col}}: {.val {ids[duplicated(ids)][1:3]}}"
    )
  }

  # --- build nb object ---
  # we detect and report islands ourselves below, so
  # poly2nb's "no neighbours / sub-graphs" warnings are noise
  nb <- suppressWarnings(
    spdep::poly2nb(
      boundaries,
      queen = contiguity == "queen",
      snap = snap_tolerance
    )
  )

  attr(nb, "region.id") <- as.character(ids)

  # --- handle islands ---
  n_islands <- sum(spdep::card(nb) == 0)

  if (n_islands > 0 && handle_islands) {
    # st_geometry() drops attributes so st_centroid doesn't
    # warn about constant-attribute assumptions
    coords <- sf::st_coordinates(
      sf::st_centroid(sf::st_geometry(boundaries))
    )
    knn <- spdep::knearneigh(coords, k = 1)
    # knn2nb reports sub-graphs; we only use single nearest
    # neighbours from it, so the global graph is irrelevant
    knn_nb <- suppressWarnings(spdep::knn2nb(knn))

    # symmetric attachment: for each island i, attach to nearest j AND
    # add i to nb[[j]]. one-directional attachment leaves the graph
    # asymmetric, which makes INLA's BYM2 precision matrix singular
    for (i in which(spdep::card(nb) == 0)) {
      nearest <- as.integer(knn_nb[[i]])
      nb[[i]] <- nearest
      for (j in nearest) {
        cur <- nb[[j]]
        if (length(cur) == 1L && cur == 0L) {
          nb[[j]] <- as.integer(i)
        } else {
          nb[[j]] <- sort(as.integer(unique(c(cur, i))))
        }
      }
    }

    # graph is now symmetric again; refresh attributes so downstream
    # consumers (incl. print, INLA via nb2INLA) see the correct state
    attr(nb, "sym")   <- TRUE
    attr(nb, "ncomp") <- spdep::n.comp.nb(nb)

    n_isl_str <- format(n_islands, big.mark = ",")
    cli::cli_alert_warning(
      "{n_isl_str} island(s) attached to nearest neighbour"
    )
  } else if (n_islands > 0) {
    cli::cli_alert_warning(
      "{format(n_islands, big.mark = ',')} disconnected component(s) detected"
    )
  }

  # --- report ---
  n_districts <- length(nb)
  n_links <- sum(spdep::card(nb))
  mean_nb <- round(mean(spdep::card(nb)), 1)

  cli::cli_alert_info(
    paste0(
      "{format(n_districts, big.mark = ',')} districts | ",
      "{format(n_links, big.mark = ',')} links | ",
      "mean {mean_nb} neighbours"
    )
  )

  class(nb) <- c("spi_nb", "nb")
  nb
}

#' @export
print.spi_nb <- function(x, ...) {
  n_regions <- length(x)
  cards <- spdep::card(x)
  n_links <- sum(cards)
  pct <- 100 * n_links / (n_regions^2)
  mean_lnk <- mean(cards)
  ncomp <- attr(x, "ncomp")
  n_sub <- if (!is.null(ncomp)) ncomp$nc else NA_integer_

  fmt_int <- function(v) format(v, big.mark = ",")
  fmt_num <- function(v, d = 4) {
    formatC(v, format = "f", big.mark = ",", digits = d)
  }

  cat("Neighbour list object:\n")
  cat("Number of regions:", fmt_int(n_regions), "\n")
  cat("Number of nonzero links:", fmt_int(n_links), "\n")
  cat("Percentage nonzero weights:", fmt_num(pct, 4), "\n")
  cat("Average number of links:", fmt_num(mean_lnk, 4), "\n")
  if (!is.na(n_sub) && n_sub > 1) {
    cat(fmt_int(n_sub), "disjoint connected subgraphs\n")
  }
  invisible(x)
}
