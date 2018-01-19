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
                        shape = "flat", time = "period", fillweight = T)
  biomass.test = dplyr::filter(biomass.filled, period %in% 400:450)
  biomass.notfilled = biomass(path = 'repo', level = "Plot", type = "Rodents",
                              length = "all", unknowns = T, incomplete = T,
                              shape = "flat", time = "period", fillweight = F)

  biomass.notfilledt = dplyr::filter(biomass.notfilled, period %in% 400:450)
  expect_true(nrow(biomass.test) == 26928)
  expect_true(length(which(dim(biomass.test) != dim(biomass.notfilledt))) == 0)

  expect_true(length(which(is.na(biomass.test$biomass))) == 1166)
  expect_true(length(which(is.na(biomass.notfilledt$biomass))) == 1166)
  expect_true(length(which(biomass.notfilledt$species != biomass.test$species)) == 0)

  expect_true(which(biomass.notfilledt$biomass != biomass.test$biomass)[2] == 1131)
  expect_true(as.numeric(biomass.notfilledt[1131, 'biomass']) == 15)
  expect_true(floor(as.numeric(biomass.test[1131, 'biomass'])) == 24)

  })

test_that("energy returns expected results", {
  energy.filled = energy(path = 'repo', level = "Plot", type = "Rodents",
                        length = "all", unknowns = T, incomplete = T,
                        shape = "flat", time = "period", fillweight = T)

  energy.test = dplyr::filter(energy.filled, period %in% 400:450)
  energy.notfilled = energy(path = 'repo', level = "Plot", type = "Rodents",
                              length = "all", unknowns = T, incomplete = T,
                              shape = "flat", time = "period", fillweight = F)

  energy.notfilledt = dplyr::filter(energy.notfilled, period %in% 400:450)
  expect_true(nrow(energy.test) == 26928)
  expect_true(length(which(dim(energy.test) != dim(energy.notfilledt))) == 0)

  expect_true(length(which(is.na(energy.test$energy))) == 1166)
  expect_true(length(which(is.na(energy.notfilledt$energy))) == 1166)
  expect_true(length(which(energy.notfilledt$species != energy.test$species)) == 0)

  expect_true(which(energy.notfilledt$energy != energy.test$energy)[2] == 1131)
  expect_true(floor(as.numeric(energy.notfilledt[1131, 'energy'])) == 9)
  expect_true(floor(as.numeric(energy.test[1131, 'energy'])) == 14)

  })


