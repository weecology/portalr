context("Check data processing")

data_tables <- load_data(".")

test_that("rodent data.frame has correct column names", {
  rats = data_tables[[1]]
  rat_cols = colnames(rats)
  expect_true('month' %in% rat_cols)
  expect_true('year' %in% rat_cols)
  expect_true('plot' %in% rat_cols)
  expect_true('species' %in% rat_cols)
  expect_true('period' %in% rat_cols)
  expect_true('day' %in% rat_cols)
})

test_that("species data.frame has correct column names", {
  sp = data_tables[[2]]
  sp_cols = colnames(sp)
  expect_true('species' %in% sp_cols)
  expect_true('unidentified' %in% sp_cols)
  expect_true('censustarget' %in% sp_cols)
  expect_true('granivore' %in% sp_cols)
})

test_that("clean_rodent_data works", {
  expect_error(rodents <- clean_rodent_data(data_tables), NA)
})

rodents <- clean_rodent_data(data_tables)

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
  expect_gt(nrow(dplyr::filter(rodents_with_incompletes, plot == 24)),
            nrow(dplyr::filter(rodents, plot == 24)))
})

test_that("does filter_plots work properly?", {
  rodents_longterm <- filter_plots(data_tables$trapping, length = "longterm")
  expect_equal(sort(unique(rodents_longterm$plot)), c(3, 4, 10, 11, 14, 15, 16, 17, 19, 21, 23))
})
