context("Check rodent data summaries")

test_that("abundance returns expected results", {
  ab_notfilled <- abundance(path = ".", level = "Plot", type = "Rodents",
                            length = "all", unknowns = FALSE, incomplete = FALSE,
                            shape = "flat", time = "period", fillweight = FALSE,
                            na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                            min_plots = NULL, effort = FALSE)
  test_ab <- dplyr::filter(ab_notfilled, period %in% 400:450)
  expect_equal(nrow(test_ab), 23184)
  expect_true(sum(test_ab$abundance, na.rm = TRUE) == 10110)
  test_ab <- dplyr::filter(ab_notfilled, species == "DM", abundance > 0)
  expect_equal(max(test_ab$abundance, na.rm = TRUE), 17)
  expect_false(anyNA(test_ab))

  ab_filled <- abundance(path = ".", level = "Plot", type = "Rodents",
                         length = "all", unknowns = FALSE, incomplete = FALSE,
                         shape = "flat", time = "period", fillweight = TRUE,
                         na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                         min_plots = NULL, effort = FALSE)
  expect_equal(ab_notfilled, ab_filled)
})

test_that("biomass returns expected results", {

  biom_filled <- biomass(path = ".", level = "Plot", type = "Rodents",
                         length = "all", unknowns = FALSE, incomplete = FALSE,
                         shape = "flat", time = "period", fillweight = TRUE,
                         na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                         min_plots = NULL, effort = FALSE) %>%
    dplyr::filter(period %in% 400:450)

  biom_notfilled <- biomass(path = ".", level = "Plot", type = "Rodents",
                            length = "all", unknowns = FALSE, incomplete = FALSE,
                            shape = "flat", time = "period", fillweight = FALSE,
                            na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                            min_plots = NULL, effort = FALSE) %>%
    dplyr::filter(period %in% 400:450)

  biom_notfilled <- biomass(path = ".", level = "Plot", type = "Rodents",
                            length = "all", unknowns = FALSE, incomplete = FALSE,
                            shape = "flat", time = "period", fillweight = FALSE,
                            na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                            min_plots = NULL, effort = FALSE) %>%
    dplyr::filter(period %in% 400:450)

  expect_equal(nrow(biom_filled), 23184)
  expect_equal(dim(biom_filled), dim(biom_notfilled))
  expect_equal(sum(is.na(biom_filled$biomass)), 0)
  expect_equal(sum(is.na(biom_notfilled$biomass)), 0)
  expect_equal(biom_filled$species, biom_notfilled$species)
  expect_equal(floor(dplyr::filter(biom_notfilled, period == 447, plot == 3,
                                   species == "BA")$biomass), 15)
  expect_equal(floor(dplyr::filter(biom_filled, period == 447, plot == 3,
                                      species == "BA")$biomass), 24)
})

test_that("energy returns expected results", {

  energy_filled <- energy(path = ".", level = "Plot", type = "Rodents",
                          length = "all", unknowns = FALSE, incomplete = FALSE,
                          shape = "flat", time = "period", fillweight = TRUE,
                          na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                          min_plots = NULL, effort = FALSE) %>%
    dplyr::filter(period %in% 400:450)

  energy_notfilled <- energy(path = ".", level = "Plot", type = "Rodents",
                             length = "all", unknowns = FALSE, incomplete = FALSE,
                             shape = "flat", time = "period", fillweight = FALSE,
                             na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                             min_plots = NULL, effort = FALSE) %>%
    dplyr::filter(period %in% 400:450)

  expect_equal(nrow(energy_filled), 23184)
  expect_equal(dim(energy_filled), dim(energy_notfilled))
  expect_equal(sum(is.na(energy_filled$energy)), 0)
  expect_equal(sum(is.na(energy_notfilled$energy)), 0)
  expect_equal(energy_filled$species, energy_notfilled$species)
  expect_equal(floor(dplyr::filter(energy_notfilled, period == 447, plot == 3,
                                   species == "BA")$energy), 9)
  expect_equal(floor(dplyr::filter(energy_filled, period == 447, plot == 3,
                                   species == "BA")$energy), 14)
})

