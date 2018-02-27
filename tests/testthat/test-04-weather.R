context("checks weather summary output")

download_observations('.')

test_that("'Daily' option returns 8 columns" , {
  data = weather("Daily",path = ".")
  expect_that(dim(data)[2], equals(8))
  expect_that(sum(colnames(data)==c("year","month","day","mintemp","maxtemp","meantemp","precipitation","flag")), equals(8))
})

test_that("Monthly option returns 8 columns", {
  data = weather("Monthly",path = ".")
  expect_that(dim(data)[2], equals(8))
  expect_that(sum(colnames(data)==c("year","month","mintemp","maxtemp","meantemp","precipitation","ndvi","flag")), equals(8))
})

test_that("Daily temperatures ok" , {
  data = weather("Daily",path = ".")
  expect_that(length(which((data$mintemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$meantemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$mintemp<=data$meantemp)==FALSE)),equals(0))
})

test_that("Monthly temperatures ok", {
  data = weather("Monthly",path = ".")
  expect_that(length(which((data$mintemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$meantemp<=data$maxtemp)==FALSE)),equals(0))
  expect_that(length(which((data$mintemp<=data$meantemp)==FALSE)),equals(0))
})
