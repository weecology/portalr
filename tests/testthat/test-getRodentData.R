context("checks get_rodent_data functions")

test_that("user given path returns list length 5", {
  data = loadData("repo")
  expect_that(length(data), equals(5))
})

test_that("argument repo returns list length 5", {
  data = loadData("repo")
  expect_that(length(data), equals(5))
})

test_that("required column names in rodent df", {
  data = loadData("repo")
  rats = data[[1]]
  rat_cols = colnames(rats)
  expect_true('month' %in% rat_cols)
  expect_true('year' %in% rat_cols)
  expect_true('plot' %in% rat_cols)
  expect_true('species' %in% rat_cols)
  expect_true('period' %in% rat_cols)
  expect_true('day' %in% rat_cols)
})

test_that("required column names in species df", {
  data = loadData("repo")
  sp = data[[2]]
  sp_cols = colnames(sp)
  expect_true('species' %in% sp_cols)
  expect_true('unidentified' %in% sp_cols)
  expect_true('censustarget' %in% sp_cols)
  expect_true('granivore' %in% sp_cols)
})

test_that("abundance returns expected results", {
  abundance.notfilled = abundance(path = 'repo', level = "Plot", type = "Rodents",
                        length = "all", unknowns = T, incomplete = T,
                        shape = "flat", time = "period", fillweight = F)
  test.abundance = dplyr::filter(abundance.notfilled, period %in% 400:450)
  expect_true(sum(test.abundance$abundance, na.rm = T) == 10622)
  test.abundance = dplyr::filter(abundance.notfilled, species == 'DM', abundance > 0)
  expect_true(max(test.abundance$abundance, na.rm = T) == 17)
  expect_true(anyNA(test.abundance) == F)
  expect_true(nrow(test.abundance) == 4948)

  abundance.filled = abundance(path = 'repo', level = "Plot", type = "Rodents",
                               length = "all", unknowns = T, incomplete = T,
                               shape = "flat", time = "period", fillweight = T)
  expect_true(length(which(abundance.notfilled != abundance.filled)) == 0)

})

test_that("biomass returns expected results", {
  biomass.filled = biomass(path = 'repo', level = "Plot", type = "Rodents",
                           length = "all", unknowns = T, incomplete = T,
                           shape = "flat", time = "period", fillweight = T) %>%
    dplyr::filter(period %in% 400:450)
  biomass.notfilled = biomass(path = 'repo', level = "Plot", type = "Rodents",
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
  energy.filled = energy(path = 'repo', level = "Plot", type = "Rodents",
                         length = "all", unknowns = T, incomplete = T,
                         shape = "flat", time = "period", fillweight = T) %>%
    dplyr::filter(period %in% 400:450)
  energy.notfilled = energy(path = 'repo', level = "Plot", type = "Rodents",
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


