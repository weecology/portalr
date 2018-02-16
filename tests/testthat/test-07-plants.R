context("Check plant data summaries")

test_that("plant_abundance returns expected results", {
  download_observations(".", release_only = FALSE)
  plantabundance.all = plant_abundance(path = '.', level = "Plot", type = "All",
                                  length = "all", unknowns = T, correct_sp= F,
                                  shape = "flat")
  test.plantabundance = dplyr::filter(plantabundance.all, year %in% 1996:1998)
  expect_true(sum(test.plantabundance$abundance, na.rm = T) == 88422)
  test.plantabundance = dplyr::filter(plantabundance.all, species == 'erod cicu', abundance > 0, year < 2017)
  expect_equal(max(test.plantabundance$abundance, na.rm = T), 3369)
  expect_false(anyNA(test.plantabundance))
  expect_equal(nrow(test.plantabundance), 592)

  plantabundance.fixnames = plant_abundance(path = '.', level = "Plot", type = "All",
                                            length = "all", unknowns = T, correct_sp= T,
                                            shape = "flat")
  expect_equal(sum(plantabundance.all$abundance,na.rm=T), sum(plantabundance.fixnames$abundance,na.rm=T))

})
