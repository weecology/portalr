context("Check get_future_moons")

portal_data_path <- tempdir()

test_that("get_future_moons returns expected results using current newmoons_table", {
  moons <- load_data(portal_data_path)$newmoons_table

  expect_error(newmoons <- get_future_moons(moons, num_future_moons = 12), NA)
  expect_equal(dim(newmoons), c(12, 4))
  expect_equal(colnames(newmoons), colnames(moons))
  expect_equal(newmoons$newmoonnumber, max(moons$newmoonnumber) + seq(12))
  expect_true(all(newmoons$newmoondate > as.Date(tail(moons$newmoondate, 1))))
  expect_equal(newmoons$period, rep(NA, 12))
  expect_equal(newmoons$censusdate, as.Date(rep(NA, 12)))
})

test_that("get_future_moons returns identical table using sample input", {
  moons <- data.frame(newmoonnumber = c(1, 2),
                      newmoondate = c("1977-07-16", "1977-08-14"),
                      period = c(1, 2),
                      censusdate = c("1977-07-16", "1977-08-19"))

  newmoons <- get_future_moons(moons, num_future_moons = 10)
  attributes(newmoons) <- attributes(newmoons)[sort(names(attributes(newmoons)))]
  expect_identical(digest::digest(newmoons), "bec7d2ca15a8dad4d44775daf5239c53")
})
