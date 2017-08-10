context("checks weather summary output")

download_observations('.')

test_that("'Daily' option returns 7 columns" , {
  data = weather("Daily",".")
  expect_that(dim(data)[2], equals(7))
  expect_that(sum(colnames(data)==c("year","month","day","mintemp","maxtemp","meantemp","precipitation")), equals(7))
})

test_that("Monthly option returns 7 columns", {
  data = weather("Monthly",".")
  expect_that(dim(data)[2], equals(7))
  expect_that(sum(colnames(data)==c("year","month","mintemp","maxtemp","meantemp","precipitation","NDVI")), equals(7))
})


test_that("Daily temperatures ok" , {
  data = weather("Daily",".")
  expect_that(length(which((data$mintemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$meantemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$mintemp<=data$meantemp)==FALSE)),equals(0))
})

test_that("Monthly temperatures ok", {
  data = weather("Monthly",".")
  expect_that(length(which((data$mintemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$meantemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$mintemp<=data$meantemp)==FALSE)),equals(0))
})
