context("Check rodent data summaries")

test_that("abundance returns expected results", {
  abundance.notfilled = abundance(path = '.', level = "Plot", type = "Rodents",
                        length = "all", unknowns = T, incomplete = T,
                        shape = "flat", time = "period", fillweight = F)
  test.abundance = dplyr::filter(abundance.notfilled, period %in% 400:450)
  expect_true(sum(test.abundance$abundance, na.rm = T) == 10622)
  test.abundance = dplyr::filter(abundance.notfilled, species == 'DM', abundance > 0)
  expect_equal(max(test.abundance$abundance, na.rm = T), 17)
  expect_false(anyNA(test.abundance))
  expect_equal(nrow(test.abundance), 4863)

  abundance.filled = abundance(path = '.', level = "Plot", type = "Rodents",
                               length = "all", unknowns = T, incomplete = T,
                               shape = "flat", time = "period", fillweight = T)
  expect_equal(abundance.notfilled, abundance.filled)

})

test_that("biomass returns expected results", {
  biomass.filled = biomass(path = '.', level = "Plot", type = "Rodents",
                           length = "all", unknowns = T, incomplete = T,
                           shape = "flat", time = "period", fillweight = T) %>%
    dplyr::filter(period %in% 400:450)
  biomass.notfilled = biomass(path = '.', level = "Plot", type = "Rodents",
                              length = "all", unknowns = T, incomplete = T,
                              shape = "flat", time = "period", fillweight = F) %>%
    dplyr::filter(period %in% 400:450)
  expect_equal(nrow(biomass.filled), 26928)
  expect_equal(dim(biomass.filled), dim(biomass.notfilled))

  expect_equal(sum(is.na(biomass.filled$biomass)), 1166)
  expect_equal(sum(is.na(biomass.notfilled$biomass)), 1166)
  expect_equal(biomass.notfilled$species, biomass.filled$species)

  expect_equal(dplyr::filter(biomass.notfilled,
                             period == 447, plot == 3, species == "BA")$biomass, 15)
  expect_equal(floor(dplyr::filter(biomass.filled,
                                   period == 447, plot == 3, species == "BA")$biomass), 24)

})

test_that("energy returns expected results", {
  energy.filled = energy(path = '.', level = "Plot", type = "Rodents",
                         length = "all", unknowns = T, incomplete = T,
                         shape = "flat", time = "period", fillweight = T) %>%
    dplyr::filter(period %in% 400:450)
  energy.notfilled = energy(path = '.', level = "Plot", type = "Rodents",
                            length = "all", unknowns = T, incomplete = T,
                            shape = "flat", time = "period", fillweight = F) %>%
    dplyr::filter(period %in% 400:450)
  expect_equal(nrow(energy.filled), 26928)
  expect_equal(dim(energy.filled), dim(energy.notfilled))

  expect_equal(sum(is.na(energy.filled$energy)), 1166)
  expect_equal(sum(is.na(energy.notfilled$energy)), 1166)
  expect_equal(energy.notfilled$species, energy.filled$species)

  expect_equal(floor(dplyr::filter(energy.notfilled, period == 447, plot == 3, species == "BA")$energy), 9)
  expect_equal(floor(dplyr::filter(energy.filled, period == 447, plot == 3, species == "BA")$energy), 14)

  })


