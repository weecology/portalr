context("Check plant data summaries")

test_that("plant_abundance returns expected results", {
  plants_all <- plant_abundance(path = '.', level = "Plot", type = "All",
                                plots = "all", unknowns = TRUE,
                                correct_sp = FALSE, shape = "flat")
  plants_9698 <- dplyr::filter(plants_all, year %in% 1996:1998)
  expect_true(sum(plants_9698$abundance, na.rm = TRUE) == 88422)

  plants_erod <- plants_all %>%
    dplyr::filter(species == 'erod cicu', abundance > 0, year < 2017)
  expect_equal(max(plants_erod$abundance, na.rm = TRUE), 3369)
  expect_false(anyNA(plants_erod))
  expect_equal(nrow(plants_erod), 592)

  plants_fix_names <- plant_abundance(path = '.', level = "Plot", type = "All",
                                      plots = "all", unknowns = TRUE,
                                      correct_sp = TRUE, shape = "flat")
  expect_equal(sum(plants_all$abundance, na.rm = TRUE),
               sum(plants_fix_names$abundance, na.rm = TRUE))

})
