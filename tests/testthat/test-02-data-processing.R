context("Check data processing")

data_tables <- loadData(".")

test_that("clean_rodent_data works", {
  expect_error(rodents <- clean_rodent_data(data_tables), NA)
})

test_that("clean_rodent_data has correct columns", {
  rodent_cols <- names(rodents)
  expect_true("species" %in% rodent_cols)
  expect_true("unidentified" %in% rodent_cols)
  expect_true("rodent" %in% rodent_cols)
  expect_true("granivore" %in% rodent_cols)
  expect_true("wgt" %in% rodent_cols)
  expect_true("energy" %in% rodent_cols)

  expect_is(rodents$species, "factor")
  expect_is(rodents$wgt, "numeric")
  expect_is(rodents$energy, "numeric")
  expect_equal(is.na(rodents$wgt), is.na(rodents$energy))
  expect_equal(rodents$wgt[!is.na(rodents$wgt)] ^ 0.75,
               rodents$energy[!is.na(rodents$energy)])
})

test_that("does fill_weight work properly?", {
  rodents_fillweight <- clean_rodent_data(data_tables, fillweight = TRUE)
  expect_lt(sum(is.na(rodents_fillweight$wgt)),
             sum(is.na(rodents$wgt)))
  expect_lt(sum(is.na(rodents_fillweight$energy)),
             sum(is.na(rodents$energy)))
})

test_that("does process_granivores work properly?", {
  rodents_granivores <- clean_rodent_data(data_tables, type = "Granivores")
  expect_true(all(rodents_granivores$granivore == 1))
  expect_false(all(rodents$granivore == 1))
})

test_that("does process_unknownsp work properly?", {
  rodents_with_unknowns <- clean_rodent_data(data_tables, unknowns = TRUE)
  expect_true("other" %in% rodents_with_unknowns$species)
  expect_false("other" %in% rodents$species)
})

test_that("does remove_incomplete_censuses work properly?", {
  rodents_with_incompletes <- clean_rodent_data(data_tables, incomplete = TRUE)
  expect_gt(nrow(filter(rodents_with_incompletes, plot == 24)),
            nrow(filter(rodents, plot == 24)))
})

test_that("does filter_plots work properly?", {
  rodents_longterm <- clean_rodent_data(data_tables, length = "longterm")
  expect_equal(sort(unique(rodents_longterm$plot)), c(3, 4, 10, 11, 14, 15, 16, 17, 19, 21, 23))
})
