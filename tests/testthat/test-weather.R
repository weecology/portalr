context("checks weather summary output")

download_observations('.')

test_that("'Daily' option returns 7 columns" , {
  data = weather("Daily",'.')
  expect_that(dim(data)[2], equals(7))
  expect_that(sum(colnames(data)==c("Year","Month","Day","MinTemp","MaxTemp","MeanTemp","Precipitation")), equals(7))
})

test_that("Monthly option returns 7 columns", {
  data = weather("Monthly",'.')
  expect_that(dim(data)[2], equals(7))
  expect_that(sum(colnames(data)==c("Year","Month","MinTemp","MaxTemp","MeanTemp","Precipitation","NDVI")), equals(7))
})


test_that("Daily temperatures ok" , {
  data = weather("Daily",'.')
  expect_that(length(which((data$MinTemp<=data$MaxTemp)==FALSE)),equals(0))
  expect_that(length(which((data$MeanTemp<=data$MaxTemp)==FALSE)),equals(0))
  expect_that(length(which((data$MinTemp<=data$MeanTemp)==FALSE)),equals(0))
})

test_that("Monthly temperatures ok", {
  data = weather("Monthly",'.')
  expect_that(length(which((data$MinTemp<=data$MaxTemp)==FALSE)),equals(0))
  expect_that(length(which((data$MeanTemp<=data$MaxTemp)==FALSE)),equals(0))
  expect_that(length(which((data$MinTemp<=data$MeanTemp)==FALSE)),equals(0))
})
