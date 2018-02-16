context("Check rodent stake data")

test_that("get_stake_data returns expected columns", {
  stake_data = get_stake_data(path = '.')
  expect_true(all(names(stake_data) == c("period","month","day","year","treatment","plot",
                                     "stake","species","sex","hfl","wgt","tag","ltag")))

  filled_stake_data = get_stake_data(path = '.', type = "granivores",
                               length = "all", unknowns = T, incomplete = T,
                               time = "newmoon", fillweight = T)
  expect_true(all(names(filled_stake_data) == c("newmoonnumber","month","day","year","treatment","plot",
                                         "stake","species","sex","hfl","wgt","tag","ltag")))

})
