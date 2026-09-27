# spi_adjacency() island handling, contiguity rule, validation, and the
# spi_nb print method.

# a 3-polygon layer: A and B share an edge, C sits far away as an island
island_layer <- function(ids = c("A", "B", "C")) {
  skip_if_not_installed("sf")
  sq <- function(cx, cy) {
    sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0) + cx, c(0, 0, 1, 1, 0) + cy)))
  }
  sf::st_sf(
    id = ids,
    geometry = sf::st_sfc(sq(0, 0), sq(1, 0), sq(10, 10))
  )
}

test_that("spi_adjacency attaches islands to their nearest neighbour", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  bnd <- island_layer()

  adj <- suppressMessages(spi_adjacency(bnd, id_col = "id"))
  expect_s3_class(adj, "spi_nb")
  # after attachment no district is left without a neighbour
  expect_equal(sum(spdep::card(adj) == 0), 0L)
  expect_equal(attr(adj, "region.id"), c("A", "B", "C"))
})

test_that("spi_adjacency leaves islands disconnected when asked", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  bnd <- island_layer()

  adj <- suppressMessages(
    spi_adjacency(bnd, id_col = "id", handle_islands = FALSE)
  )
  # the far polygon stays an island
  expect_gt(sum(spdep::card(adj) == 0), 0L)
})

test_that("spi_adjacency attaches an island whose nearest is also an island", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  # A-B adjacent; C and D are separate islands, nearest to each other
  sq <- function(cx, cy) {
    sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0) + cx, c(0, 0, 1, 1, 0) + cy)))
  }
  bnd <- sf::st_sf(
    id = c("A", "B", "C", "D"),
    geometry = sf::st_sfc(sq(0, 0), sq(1, 0), sq(10, 10), sq(12, 10))
  )
  adj <- suppressMessages(spi_adjacency(bnd, id_col = "id"))
  expect_equal(sum(spdep::card(adj) == 0), 0L)   # both islands attached
})

test_that("spi_adjacency honours the rook contiguity rule", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  bnd <- island_layer()
  expect_no_error(
    suppressMessages(spi_adjacency(bnd, id_col = "id", contiguity = "rook"))
  )
})

test_that("spi_adjacency rejects duplicate ids and bad arguments", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  dup <- island_layer(ids = c("A", "A", "B"))
  expect_error(spi_adjacency(dup, id_col = "id"), "duplicate")

  bnd <- island_layer()
  expect_error(spi_adjacency(bnd, id_col = "missing"))     # id not a column
  expect_error(spi_adjacency(bnd, id_col = "id", contiguity = "bishop"))
  expect_error(spi_adjacency(list(), id_col = "id"))        # not sf
})

test_that("print.spi_nb reports regions, links, and subgraphs", {
  # a hand-built graph with two components exercises the subgraph line
  nb <- make_nb(c("A", "B", "C", "D"))     # island_last -> 2 components
  expect_output(print(nb), "Neighbour list object")
  expect_output(print(nb), "disjoint connected subgraphs")
  expect_identical(print(nb), nb)

  # a connected real graph takes the no-subgraph path
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  adj <- suppressMessages(spi_adjacency(island_layer(), id_col = "id"))
  expect_output(print(adj), "Number of regions")
})
