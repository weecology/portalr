context("checks phenocam summary output")

portal_data_path <- tempdir()
daily_pheno <- phenocam("daily", path = portal_data_path)
monthly_pheno <- phenocam("monthly", path = portal_data_path)
newmoon_pheno <- phenocam("newmoon", path = portal_data_path)

test_that("Daily option returns 32 columns", {
  expect_equal(NCOL(daily_pheno), 32)
  expect_equal(colnames(daily_pheno),
               c("date","year","doy","image_count","midday_filename","midday_r","midday_g","midday_b",
                 "midday_gcc","midday_rcc","r_mean","r_std","g_mean","g_std","b_mean","b_std","gcc_mean",
                 "gcc_std","gcc_50","gcc_75","gcc_90","rcc_mean","rcc_std","rcc_50","rcc_75","rcc_90",
                 "max_solar_elev","snow_flag","outlierflag_gcc_mean","outlierflag_gcc_50",
                 "outlierflag_gcc_75","outlierflag_gcc_90"))
})

test_that("Monthly option returns 7 columns", {
  expect_equal(NCOL(monthly_pheno), 7)
  expect_equal(colnames(monthly_pheno), c("year","month","mean_image_count","midday_gcc","midday_rcc",
                                          "gcc_mean","rcc_mean"))
})

test_that("Newmoon option returns 6 columns", {
  expect_that(dim(newmoon_pheno)[2], equals(6))
  expect_equal(colnames(newmoon_pheno), c("newmoonnumber","mean_image_count","midday_gcc","midday_rcc",
                                          "gcc_mean","rcc_mean"))
})
