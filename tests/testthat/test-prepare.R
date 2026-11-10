# Preparation of POLIS-shaped tables: synthetic, hand-checkable rows only.

prep_cases <- function() {
  tibble::tibble(
    adm1_guid = c("P1", "P1", "P1", "P2", "P2", "P2", "P1"),
    adm2_guid = c("D1", "D1", "D2", "D3", "D3", NA, "D1"),
    classification_all = c(
      "NPAFP", "WPV1", "PENDING", NA, "cVDPV2", "NPAFP", "NPAFP"
    ),
    surveillance_type_name = c(rep("AFP", 6), "Contact"),
    paralysis_onset_date = as.Date(
      c("2024-01-10", "2024-01-20", NA, NA, "2024-02-03", "2024-01-01",
        "2024-01-05")
    ),
    case_date = as.Date(c(NA, NA, "2024-02-14", NA, NA, NA, NA)),
    stool1collection_date = as.Date(
      c(NA, NA, "2024-01-30", "2024-03-02", NA, NA, NA)
    ),
    adequate_stool_with_condition = c(
      "Yes", "No", "", "Yes", NA, "Yes", "Yes"
    )
  )
}

test_that("spi_case_date takes onset, then case date, then stool date", {
  got <- spi_case_date(prep_cases())
  expect_equal(
    got,
    as.Date(c("2024-01-10", "2024-01-20", "2024-02-14", "2024-03-02",
              "2024-02-03", "2024-01-01", "2024-01-05"))
  )
  renamed <- dplyr::rename(prep_cases(), onset = "paralysis_onset_date")
  expect_equal(spi_case_date(renamed, list(onset_date = "onset")), got)
  expect_error(spi_case_date(renamed), "missing column")
  expect_error(spi_case_date(prep_cases(), list(onset = "x")), "unknown")
  expect_error(spi_case_date(prep_cases(), "x"), "list")
})

test_that("spi_case_panel counts non-polio AFP on a complete grid", {
  months <- as.Date(c("2024-01-01", "2024-02-01", "2024-03-01"))
  panel <- spi_case_panel(prep_cases(), c("D3", "D1", "D2", "D4"), months)
  expect_equal(nrow(panel), 12)
  expect_named(panel, c("adm2_guid", "month", "count"))
  expect_type(panel$count, "integer")
  cnt <- function(d, mth) {
    panel$count[panel$adm2_guid == d & panel$month == as.Date(mth)]
  }
  # D1: the WPV1 case and the contact are not counted
  expect_equal(cnt("D1", "2024-01-01"), 1L)
  # D2: a pending case placed on its case date
  expect_equal(cnt("D2", "2024-02-01"), 1L)
  # D3: an unclassified case on its stool date; the cVDPV2 case is excluded
  expect_equal(cnt("D3", "2024-03-01"), 1L)
  expect_equal(cnt("D3", "2024-02-01"), 0L)
  # a district with no case carries zeros
  expect_equal(sum(panel$count[panel$adm2_guid == "D4"]), 0L)
  expect_equal(sum(panel$count), 3L)
  all_types <- spi_case_panel(
    prep_cases(), "D1", months[1], surveillance_types = NULL
  )
  expect_equal(all_types$count, 2L)
  wider <- spi_case_panel(prep_cases(), "D1", months[1],
                          polio_pattern = "VDPV")
  expect_equal(wider$count, 2L)
})

test_that("spi_case_panel validates its arguments", {
  months <- as.Date("2024-01-01")
  expect_error(spi_case_panel(list(), "D1", months), "data frame")
  expect_error(spi_case_panel(prep_cases(), character(), months), "districts")
  expect_error(spi_case_panel(prep_cases(), "D1", "2024-01-01"), "months")
})

test_that("spi_pop_index and spi_pop_panel carry populations back", {
  pop <- tibble::tibble(
    adm2_guid = c("D1", "D1", "D1", "D2", "D2", "D3", "D3", "D3"),
    year = c(2022L, 2023L, 2024L, 2022L, 2023L, 2023L, 2024L, 2024L),
    u15_pop = c(100, 110, 121, 50, 0, 40, 30, 14)
  )
  index <- spi_pop_index(pop, c("D1", "D2", "D3"), 2022:2024)
  # only D1 has a positive population in all three years
  expect_equal(index$year, 2022:2024)
  expect_equal(index$total, c(100, 110, 121))

  panel <- spi_pop_panel(pop, c("D1", "D2", "D3", "D4"), 2022:2024)
  expect_named(panel, c("adm2_guid", "year", "pop_u15", "carried"))
  expect_equal(nrow(panel), 12)
  val <- function(d, y) panel$pop_u15[panel$adm2_guid == d & panel$year == y]
  # D3 first drawn in 2023: 2022 is 40 scaled by 100 / 110
  expect_equal(val("D3", 2022), 40 * 100 / 110)
  # two rows for one district-year are summed
  expect_equal(val("D3", 2024), 44)
  # D2's zero in 2023 is dropped and carried from its 2022 base
  expect_equal(val("D2", 2023), 50 * 110 / 100)
  expect_equal(val("D2", 2024), 50 * 121 / 100)
  expect_true(all(is.na(panel$pop_u15[panel$adm2_guid == "D4"])))
  expect_equal(sum(panel$carried), 6L)

  renamed <- dplyr::rename(pop, g = "adm2_guid", p = "u15_pop")
  expect_equal(
    spi_pop_panel(renamed, "D1", 2022:2024, "g", "year", "p")$pop_u15,
    c(100, 110, 121)
  )
  expect_error(spi_pop_panel(renamed, "D1", 2022:2024), "missing column")
  expect_error(spi_pop_panel(pop, "D1", integer()), "years")
})

test_that("spi_window and spi_panel_months stop at the lag", {
  w <- spi_window(as.Date("2024-03-31"))
  expect_length(w, 12)
  expect_equal(range(w), as.Date(c("2023-03-01", "2024-02-01")))
  expect_equal(spi_window(as.Date("2024-03-01")), w)
  expect_equal(
    spi_window("2024-01-15", window_months = 1, lag_months = 0),
    as.Date("2024-01-01")
  )
  expect_equal(max(spi_window("2024-01-15")), as.Date("2023-12-01"))
  pm <- spi_panel_months(as.Date("2024-03-10"), start_year = 2023)
  expect_equal(range(pm), as.Date(c("2023-01-01", "2024-02-01")))
  expect_length(pm, 14)
  expect_error(spi_window(NA), "as_of")
  expect_error(spi_window("2024-01-01", window_months = 0), "window_months")
  expect_error(spi_window("2024-01-01", lag_months = -1), "lag_months")
  expect_error(spi_panel_months("2024-01-01", start_year = 2025), "start_year")
})
