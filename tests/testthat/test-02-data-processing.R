# Check data processing

eps <- if (capabilities("long.double"))
  sqrt(.Machine$double.eps) else
    0.1

portal_data_path <- tempdir()
data_tables <- load_rodent_data(portal_data_path)

test_that("rodent data.frame has correct column names", {
  rats <- data_tables[[1]]
  rat_cols <- colnames(rats)
  expect_true('month' %in% rat_cols)
  expect_true('year' %in% rat_cols)
  expect_true('plot' %in% rat_cols)
  expect_true('species' %in% rat_cols)
  expect_true('period' %in% rat_cols)
  expect_true('day' %in% rat_cols)
})

test_that("species data.frame has correct column names", {
  sp <- data_tables[[2]]
  sp_cols <- colnames(sp)
  expect_true('species' %in% sp_cols)
  expect_true('unidentified' %in% sp_cols)
  expect_true('censustarget' %in% sp_cols)
  expect_true('granivore' %in% sp_cols)
})

test_that("clean_rodent_data works", {
  expect_error(rodents <- clean_rodent_data(data_tables$rodent_data,
                                            data_tables$species_table), NA)
})

rodents <- clean_rodent_data(data_tables$rodent_data,
                             data_tables$species_table)

test_that("clean_rodent_data has correct columns", {
  rodent_cols <- names(rodents)
  expect_true("species" %in% rodent_cols)
  expect_true("unidentified" %in% rodent_cols)
  expect_true("rodent" %in% rodent_cols)
  expect_true("granivore" %in% rodent_cols)
  expect_true("wgt" %in% rodent_cols)
  expect_true("energy" %in% rodent_cols)

  expect_s3_class(rodents$species, "factor")
  expect_type(rodents$wgt, "double")
  expect_type(rodents$energy, "double")
  expect_equal(is.na(rodents$wgt), is.na(rodents$energy), tolerance = eps)
  expect_equal(5.69 * rodents$wgt[!is.na(rodents$wgt)] ^ 0.75,
               rodents$energy[!is.na(rodents$energy)], tolerance = eps)
})

test_that("does fill_weight work properly?", {
  rodents_fillweight <- clean_rodent_data(data_tables$rodent_data,
                                          data_tables$species_table, fillweight = TRUE)
  expect_lt(sum(is.na(rodents_fillweight$wgt)),
             sum(is.na(rodents$wgt)))
  expect_lt(sum(is.na(rodents_fillweight$energy)),
             sum(is.na(rodents$energy)))
})

test_that("does process_granivores work properly?", {
  rodents_granivores <- clean_rodent_data(data_tables$rodent_data,
                                          data_tables$species_table, type = "Granivores")
  expect_equal(rodents_granivores$granivore, rep(1,length(rodents_granivores$granivore)))
  expect_false(isTRUE(all.equal(rodents$granivore, rep(1,length(rodents$granivore)))))
})

test_that("does process_unknownsp work properly?", {
  rodents_with_unknowns <- clean_rodent_data(data_tables$rodent_data,
                                             data_tables$species_table, unknowns = TRUE)
  expect_true("other" %in% rodents_with_unknowns$species)
  expect_false("other" %in% rodents$species)
})

test_that("does filter_plots work properly?", {
  rodents_longterm <- filter_plots(data_tables$trapping, plots = "longterm")
  expect_equal(sort(unique(rodents_longterm$plot)), c(3, 4, 10, 11, 14, 15, 16, 17, 19, 21, 23))
})

test_that("does find_incomplete_censuses work properly?", {
  trappings <- data_tables$trapping_table
  expect_no_error(fewer_than_24_plots <- find_incomplete_censuses(trappings, 24, 1))
  expect_equal(dim(dplyr::filter(fewer_than_24_plots, period < 100)), c(27, 1))
  expect_no_error(any_incomplete <- find_incomplete_censuses(trappings, 24, 49))
  expect_no_error(missing_traps <- find_incomplete_censuses(trappings, 24, 47))
  expect_gt(NROW(any_incomplete), NROW(missing_traps))
  periods_47_traps <- setdiff(any_incomplete$period, missing_traps$period)
  idx <- periods_47_traps > 320 & periods_47_traps < 450
  expect_equal(sum(idx), 114, tolerance = eps)
})

plant_tables <- load_plant_data(portal_data_path)

test_that("quadrat data.frame has correct column names", {
  quadrats <- plant_tables[[1]]
  quadrat_cols <- colnames(quadrats)
  expect_true('year' %in% quadrat_cols)
  expect_true('season' %in% quadrat_cols)
  expect_true('plot' %in% quadrat_cols)
  expect_true('quadrat' %in% quadrat_cols)
  expect_true('species' %in% quadrat_cols)
  expect_true('abundance' %in% quadrat_cols)
  expect_true('cover' %in% quadrat_cols)
})

test_that("species data.frame has correct column names", {
  sp <- plant_tables[[2]]
  sp_cols <- colnames(sp)
  expect_true('species' %in% sp_cols)
  expect_true('genus' %in% sp_cols)
  expect_true('sp' %in% sp_cols)
  expect_true('duration' %in% sp_cols)
  expect_true('community' %in% sp_cols)
})

test_that("clean_plant_data works", {
  expect_no_error(plants <- clean_plant_data(plant_tables))
})

plants <- clean_plant_data(plant_tables)

test_that("clean_plant_data has correct columns", {
  plant_cols <- names(plants)
  expect_true("year" %in% plant_cols)
  expect_true("season" %in% plant_cols)
  expect_true("plot" %in% plant_cols)
  expect_true("quadrat" %in% plant_cols)
  expect_true("species" %in% plant_cols)
  expect_true("abundance" %in% plant_cols)
  expect_true("cover" %in% plant_cols)
  expect_true("sp" %in% plant_cols)
  expect_true('duration' %in% plant_cols)
  expect_true('community' %in% plant_cols)

  expect_s3_class(plants$species, "factor")
  expect_type(plants$abundance, "integer")
  expect_type(plants$cover, "double")
})
