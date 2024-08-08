# Checks phenocam summary output

portal_data_path <- tempdir()
daily_pheno <- tryCatch(phenocam("daily", path = portal_data_path), error = function(e) NULL)
monthly_pheno <- tryCatch(phenocam("monthly", path = portal_data_path), error = function(e) NULL)
newmoon_pheno <- tryCatch(phenocam("newmoon", path = portal_data_path), error = function(e) NULL)

test_that("Daily option returns 32 columns", {
  skip_if(is.null(daily_pheno))
  expect_equal(NCOL(daily_pheno), 32)
  expect_named(daily_pheno,
               c("date","year","doy","image_count","midday_filename","midday_r","midday_g","midday_b",
                 "midday_gcc","midday_rcc","r_mean","r_std","g_mean","g_std","b_mean","b_std","gcc_mean",
                 "gcc_std","gcc_50","gcc_75","gcc_90","rcc_mean","rcc_std","rcc_50","rcc_75","rcc_90",
                 "max_solar_elev","snow_flag","outlierflag_gcc_mean","outlierflag_gcc_50",
                 "outlierflag_gcc_75","outlierflag_gcc_90"))
})

test_that("Monthly option returns 7 columns", {
  skip_if(is.null(monthly_pheno))
  expect_equal(NCOL(monthly_pheno), 7)
  expect_named(monthly_pheno, c("year","month","mean_image_count","midday_gcc","midday_rcc",
                                "gcc_mean","rcc_mean"))
})

test_that("Newmoon option returns 6 columns", {
  skip_if(is.null(newmoon_pheno))
  expect_equal(dim(newmoon_pheno)[2], 6)
  expect_named(newmoon_pheno, c("newmoonnumber","mean_image_count","midday_gcc","midday_rcc",
                                "gcc_mean","rcc_mean"))
})
