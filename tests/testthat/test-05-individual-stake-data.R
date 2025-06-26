# Check rodent stake data

portal_data_path <- tempdir()

test_that("summarize_individual_rodents returns expected columns", {
  stake_data <- summarize_individual_rodents(path = portal_data_path)
  expect_named(stake_data, c("period","month","day","year","treatment","plot","stake","species",
                             "sex","reprod","age","testes","vagina","pregnant","nipples","lactation",
                             "hfl","wgt","tag","note2","ltag","note3","id"))

  filled_stake_data = summarize_individual_rodents(path = portal_data_path, type = "granivores",
                                                   length = "all", unknowns = T, min_plots = 1,
                                                   time = "newmoon", fillweight = T)
  expect_named(filled_stake_data, c("newmoonnumber","month","day","year","treatment","plot",
                                    "stake","species","sex","reprod","age","testes","vagina",
                                    "pregnant","nipples","lactation","hfl","wgt","tag",
                                    "note2","ltag","note3","id"))

})
