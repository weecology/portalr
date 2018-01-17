context("checks data_processing functions")

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
  test.abundance = filter(abundance.notfilled, period %in% 400:450)
  expect_true(sum(test.abundance$abundance, na.rm = T) == 10622)
  test.abundance = filter(abundance, species == 'DM', abundance > 0)
  expect_true(max(test.abundance$abundance, na.rm = T) == 17)
  expect_true(anyNA(test.abundance) == F)
  expect_true(nrow(test.abundance) == 4948)

  abundance.filled = abundance(path = 'repo', level = "Plot", type = "Rodents",
                               length = "all", unknowns = T, incomplete = T,
                               shape = "flat", time = "period", fillweight = T)
  expect_true(length(which(abundance.notfilled != abundance.filled)) == 0)
})

test_that("biomass returns expected results", {
  biomass = biomass(path = 'repo', level = "Site", type = "Rodents",
                        length = "all", unknowns = T, incomplete = T,
                        shape = "flat", time = "period", fillweight = T)
})

test_that("energy returns expected results", {
  energy = energy(path = 'repo', level = "Site", type = "Rodents",
                        length = "all", unknowns = T, incomplete = T,
                        shape = "flat", time = "period", fillweight = T)
})


