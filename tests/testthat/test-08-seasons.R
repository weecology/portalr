context("Check seasonal data summaries")

rodents = abundance(path = '.', level = "plot", shape = "flat")
test.rodents = dplyr::filter(rodents, period %in% 21:191)

test.weather = weather("monthly") %>% dplyr::filter(year %in% 1989:2010)

test_that("add_seasons returns expected results", {

  rodent_seasons = add_seasons(test.rodents,level = "treatment", date_column = "period",season_level=2)
  expect_true(expect_true(sum(rodent_seasons$abundance, na.rm = T) == 16768))

  rodent_seasons = add_seasons(test.rodents,level = "treatment", date_column = "period",season_level=2,summarize="mean")
  expect_true(expect_true(round(sum(rodent_seasons$abundance, na.rm = T),4) == 476.1733))

  weather_seasons = add_seasons(test.weather,date_column = "yearmon",season_level = 4, summarize = "mean")
  expect_true(expect_true(round(sum(weather_seasons$precipitation, na.rm = T),5) == 61.58203))

  })

test_that("yearly returns expected results", {

  rodent_yearly = yearly(test.rodents,level = "plot", date_column = "period")
  expect_true(expect_true(round(sum(rodent_yearly$abundance, na.rm = T),3) == 1531.998))

  weather_yearly = yearly(test.weather,date_column = "yearmon")
  expect_true(expect_true(round(sum(weather_yearly$precipitation, na.rm = T),5) == 15.5211))

})
