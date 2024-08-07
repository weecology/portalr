# Check get_future_newmoons

portal_data_path <- tempdir()

test_that("get_future_newmoons returns expected results using current newmoons_table", {
  moons <- load_trapping_data(portal_data_path)$newmoons_table

  expect_no_error(newmoons <- get_future_newmoons(moons, nfuture_newmoons = 12))
  expect_equal(dim(newmoons), c(12, 4))
  expect_equal(colnames(newmoons), colnames(moons))
  expect_equal(newmoons$newmoonnumber, max(moons$newmoonnumber) + seq(12))
  expect_true(all(newmoons$newmoondate > as.Date(tail(moons$newmoondate, 1))))
  expect_equal(newmoons$period, rep(NA, 12))
  expect_equal(newmoons$censusdate, as.Date(rep(NA, 12)))
})
