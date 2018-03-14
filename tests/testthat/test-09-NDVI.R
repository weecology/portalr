context("checks NDVI summary output")

monthly_ndvi = ndvi("monthly",path = ".")
newmoon_ndvi = ndvi("newmoon", path = ".")

test_that("Monthly option returns 2 columns", {
  expect_that(dim(monthly_ndvi)[2], equals(2))
  expect_that(sum(colnames(monthly_ndvi)==c("ndvi","date")), equals(2))
})

test_that("Newmoon option returns 2 columns", {
  expect_that(dim(newmoon_ndvi)[2], equals(2))
  expect_that(sum(colnames(newmoon_ndvi)==c("newmoonnumber","ndvi")), equals(2))
})