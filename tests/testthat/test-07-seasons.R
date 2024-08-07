# Check seasonal data summaries

eps <- if (capabilities("long.double"))
  sqrt(.Machine$double.eps) else
    0.1

portal_data_path <- tempdir()
rodents <- abundance(path = portal_data_path, level = "plot", shape = "flat")
test_rodents <- dplyr::filter(rodents, period %in% 21:191)
test_weather <- weather("monthly", path = portal_data_path) %>%
  dplyr::filter(year %in% 1989:2010)

test_that("add_seasons error checking works", {
  expect_error(add_seasons(test_rodents, level = "treatment", date_column = "period",
                           season_level = 3, path = portal_data_path),
               "`season_level` must equal 2, 4, or year")
  expect_error(add_seasons(test_rodents, level = "treatment", date_column = "period",
                           season_level = "month", path = portal_data_path),
               "`season_level` must equal 2, 4, or year")
  expect_error(add_seasons(test_rodents, level = "treatment", date_column = "period",
                           season_level = "decade", path = portal_data_path),
               "`season_level` must equal 2, 4, or year")
})

test_that("add_seasons returns expected results", {
  rodent_seasons = add_seasons(test_rodents, level = "treatment", date_column = "period",
                               season_level = 2, path = portal_data_path)
  expect_equal(sum(rodent_seasons$abundance, na.rm = T), 16452, tolerance = eps)

  rodent_seasons = add_seasons(test_rodents, level = "treatment", date_column = "period",
                               season_level = 2, summary_funs = "mean", path = portal_data_path)
  expect_equal(round(sum(rodent_seasons$abundance, na.rm = T), 4), 466.1245, tolerance = eps)

  weather_seasons = add_seasons(test_weather, date_column = "yearmon",
                                season_level = 4, summary_funs = "mean", path = portal_data_path)
  expect_equal(round(sum(weather_seasons$precipitation, na.rm = T), 3), 1774.485, tolerance = eps)
})

test_that("yearly returns expected results", {

  rodent_yearly = yearly(test_rodents, level = "plot", date_column = "period", path = portal_data_path)
  expect_equal(round(sum(rodent_yearly$abundance, na.rm = T), 3), 1533.19, tolerance = eps)

  weather_yearly = yearly(test_weather, date_column = "yearmon", path = portal_data_path)
  expect_equal(round(sum(weather_yearly$precipitation, na.rm = T), 4), 440.885, tolerance = eps)
})
