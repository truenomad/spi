test_that("synth_surveillance has the expected structure", {
  skip_if_not_installed("sf")

  data("synth_surveillance", package = "blindspot")

  expect_named(
    synth_surveillance,
    c("cases", "population", "boundaries", "ward_boundaries",
      "virus_outcome", "es_sites", "es_data", "es_district_year", "truth")
  )

  # wards: 5-8 per district
  n_wards_per_adm2 <- table(sf::st_drop_geometry(
    synth_surveillance$ward_boundaries
  )$adm2_guid)
  expect_true(all(n_wards_per_adm2 >= 5L & n_wards_per_adm2 <= 8L))

  # ES: 30 sites, monthly samples across the whole window
  expect_equal(nrow(synth_surveillance$es_sites), 30L)
  expect_equal(nrow(synth_surveillance$es_data), 30L * 120L)

  # cases: 100 districts x 120 months, zero-filled grid
  expect_equal(nrow(synth_surveillance$cases), 100 * 120)
  expect_named(synth_surveillance$cases, c("adm2_guid", "month", "count"))
  expect_type(synth_surveillance$cases$count, "integer")
  expect_true(all(synth_surveillance$cases$count >= 0))
  expect_s3_class(synth_surveillance$cases$month, "Date")

  # population: annual, under-15
  expect_equal(nrow(synth_surveillance$population), 100 * 10)
  expect_named(synth_surveillance$population, c("adm2_guid", "year", "pop_u15"))
  expect_true(all(synth_surveillance$population$pop_u15 > 0))

  # boundaries: sf with 100 polygons at EPSG:4326
  expect_s3_class(synth_surveillance$boundaries, "sf")
  expect_equal(nrow(synth_surveillance$boundaries), 100)
  expect_equal(sf::st_crs(synth_surveillance$boundaries)$epsg, 4326L)

  # virus_outcome: 100 x 10 district-years
  expect_equal(nrow(synth_surveillance$virus_outcome), 100 * 10)
  expect_named(
    synth_surveillance$virus_outcome,
    c("adm2_guid", "year", "any_wpv1", "any_cvdpv2", "any_virus")
  )

  # truth: surveillance profiles + low-incidence seeds
  expect_setequal(
    names(synth_surveillance$truth),
    c("adm2_guid", "surveillance_profile", "is_blindspot",
      "is_low_incidence", "covid_nadir", "recovered_2024")
  )
  expect_equal(sum(synth_surveillance$truth$is_low_incidence), 12L)
  expect_true(all(c("resilient", "early_improver", "covid_transient",
                    "persistent_laggard") %in%
                    synth_surveillance$truth$surveillance_profile))
  # is_blindspot is exactly the non-resilient set
  expect_true(all(
    synth_surveillance$truth$is_blindspot ==
      (synth_surveillance$truth$surveillance_profile != "resilient")
  ))
})

test_that("bs_adjacency runs on the synthetic boundaries", {
  skip_if_not_installed("spdep")

  data("synth_surveillance", package = "blindspot")
  adj <- bs_adjacency(synth_surveillance$boundaries, id_col = "adm2_guid")

  expect_s3_class(adj, "blindspot_nb")
  expect_equal(length(adj), 100L)
  # Voronoi tessellation should give a connected graph with no islands
  expect_equal(sum(spdep::card(adj) == 0), 0L)
})

test_that("full chain runs and recovers planted blindspots above chance", {
  skip_on_cran()
  skip_if_not_installed("INLA")
  skip_if_not_installed("sf")

  data("synth_surveillance", package = "blindspot")

  fit <- fit_or_skip(
    cases          = synth_surveillance$cases,
    population     = synth_surveillance$population,
    adjacency      = synth_surveillance$boundaries,
    id_col         = "adm2_guid",
    season         = "harmonic",
    year_effect    = "iid",
    overdispersion = "iid",
    n_draws        = 200L,
    seed           = 1L,
    verbose        = FALSE
  )
  expect_s3_class(fit, "blindspot_expected")

  spi <- bs_spi(fit, level = "district_year", verbose = FALSE)
  expect_s3_class(spi, "blindspot_spi")

  conc <- bs_concordance(
    spi           = spi,
    cases         = synth_surveillance$cases,
    population    = synth_surveillance$population,
    spi_threshold = 0.80,
    npafp_target  = 3,
    boundaries    = synth_surveillance$boundaries,
    verbose       = FALSE
  )
  expect_s3_class(conc, "blindspot_concordance")

  dy <- conc$district_year
  spi_flagged_cells <- c("True shortfall", "False reassurance")

  # Planted blindspots (non-resilient) should surface in an SPI-flagged cell in
  # some year at above-chance rates.
  called <- unique(dy$adm2_guid[dy$concordance %in% spi_flagged_cells])
  planted <- synth_surveillance$truth$adm2_guid[
    synth_surveillance$truth$is_blindspot
  ]
  expect_gt(mean(planted %in% called), 0.5)

  # Temporal narrative: SPI-flagged share peaks during the COVID window
  # (2020-21) and is lower both before (pre-COVID improvement) and after
  # (recovery). This is the whole point of the multi-year fixture.
  flagged <- tapply(
    dy$concordance %in% spi_flagged_cells, dy$year, sum
  )
  covid <- mean(flagged[c("2020", "2021")])
  expect_gt(covid, mean(flagged[c("2018", "2019")]))   # crash vs pre-COVID best
  expect_gt(covid, flagged[["2024"]])                  # crash vs recovered tail

  # Persistent laggards never fully recover: True shortfall / False reassurance
  # cells remain populated in the final year.
  expect_gt(flagged[["2024"]], 0)
})
