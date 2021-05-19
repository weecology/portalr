context("checks weather summary output")

portal_data_path <- tempdir()
daily_weather <- weather("daily", path = portal_data_path)
monthly_weather <- weather("monthly", path = portal_data_path)
newmoon_weather <- weather("newmoon", path = portal_data_path)

test_that("'Daily' option returns 13 columns", {
  expect_that(dim(daily_weather)[2], equals(13))
  expect_equal(colnames(daily_weather),
               c("date", "year", "month", "day", "mintemp", "maxtemp", "meantemp",
                 "precipitation", "locally_measured", "battery_low", "warm_days",
                 "cool_precip", "warm_precip"))
})

test_that("Daily temperatures ok", {
  expect_equal(length(which((daily_weather$mintemp <= daily_weather$maxtemp) == FALSE)),0)
  expect_equal(length(which((daily_weather$meantemp <= daily_weather$maxtemp) == FALSE)),0)
  expect_equal(length(which((daily_weather$mintemp <= daily_weather$meantemp) == FALSE)),0)
})

test_that("Monthly option returns 11 columns", {
  expect_equal(dim(monthly_weather)[2], 15)
  expect_equal(sum(colnames(monthly_weather) ==
                    c("year", "month", "mintemp", "maxtemp", "meantemp",
                      "precipitation", "locally_measured", "battery_low", "warm_days",
                      "cool_precip", "warm_precip", "anomaly_ppt", "anomaly_mint",
                      "anomaly_maxt", "anomaly_meant")), 15)
})

test_that("Monthly temperatures ok", {
  expect_equal(length(which((monthly_weather$mintemp <= monthly_weather$maxtemp) == FALSE)),0)
  expect_equal(length(which((monthly_weather$meantemp <= monthly_weather$maxtemp) == FALSE)),0)
  expect_equal(length(which((monthly_weather$mintemp <= monthly_weather$meantemp) == FALSE)),0)
})

test_that("Newmoon option returns 11 columns", {
  expect_equal(dim(newmoon_weather)[2], 11)
  expect_equal(sum(colnames(newmoon_weather) ==
                    c("newmoonnumber", "date", "mintemp", "maxtemp", "meantemp",
                      "precipitation", "locally_measured", "battery_low", "warm_days",
                      "cool_precip", "warm_precip")), 11)
})

test_that("Newmoon temperatures ok", {
  expect_equal(length(which((newmoon_weather$mintemp <= newmoon_weather$maxtemp) == FALSE)), 0)
  expect_equal(length(which((newmoon_weather$meantemp <= newmoon_weather$maxtemp) == FALSE)), 0)
  expect_equal(length(which((newmoon_weather$mintemp <= newmoon_weather$meantemp) == FALSE)), 0)
})
