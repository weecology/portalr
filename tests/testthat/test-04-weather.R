context("checks weather summary output")

test_that("'Daily' option returns 9 columns" , {
  data = weather("Daily",path = ".")
  expect_that(dim(data)[2], equals(9))
  expect_that(sum(colnames(data)==c("year","month","day","mintemp","maxtemp","meantemp","precipitation","locally_measured","battery_low")), equals(9))
})

test_that("Daily temperatures ok" , {
  expect_that(length(which((data$mintemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$meantemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$mintemp<=data$meantemp)==FALSE)),equals(0))
})

test_that("Monthly option returns 9 columns", {
  data = weather("Monthly",path = ".")
  expect_that(dim(data)[2], equals(9))
  expect_that(sum(colnames(data)==c("year","month","mintemp","maxtemp","meantemp","precipitation","ndvi","locally_measured","battery_low")), equals(9))
})

test_that("Monthly temperatures ok", {
  expect_that(length(which((data$mintemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$meantemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$mintemp<=data$meantemp)==FALSE)),equals(0))
})
