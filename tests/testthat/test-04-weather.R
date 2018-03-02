context("checks weather summary output")

daily_weather = weather("daily",path = ".")
monthly_weather = weather("monthly",path = ".")

test_that("'Daily' option returns 9 columns" , {
  expect_that(dim(daily_weather)[2], equals(9))
  expect_that(sum(colnames(daily_weather)==c("year","month","day","mintemp","maxtemp","meantemp","precipitation","locally_measured","battery_low")), equals(9))
})

test_that("Daily temperatures ok" , {
  expect_that(length(which((daily_weather$mintemp<=daily_weather$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((daily_weather$meantemp<=daily_weather$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((daily_weather$mintemp<=daily_weather$meantemp)==FALSE)),equals(0))
})

test_that("Monthly option returns 9 columns", {
  expect_that(dim(monthly_weather)[2], equals(9))
  expect_that(sum(colnames(monthly_weather)==c("year","month","mintemp","maxtemp","meantemp","precipitation","ndvi","locally_measured","battery_low")), equals(9))
})

test_that("Monthly temperatures ok", {
  expect_that(length(which((monthly_weather$mintemp<=monthly_weather$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((monthly_weather$meantemp<=monthly_weather$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((monthly_weather$mintemp<=monthly_weather$meantemp)==FALSE)),equals(0))
})
