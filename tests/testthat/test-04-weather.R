context("checks weather summary output")

portal_data_path <- tempdir()
daily_weather = weather("daily", path = portal_data_path)
monthly_weather = weather("monthly", path = portal_data_path)
newmoon_weather = weather("newmoon", path = portal_data_path)

test_that("'Daily' option returns 9 columns", {
  expect_that(dim(daily_weather)[2], equals(9))
  expect_equal(colnames(daily_weather),
               c("year", "month", "day", "mintemp", "maxtemp", "meantemp",
                 "precipitation", "locally_measured", "battery_low"))
})

test_that("Daily temperatures ok", {
  expect_that(length(which((daily_weather$mintemp <= daily_weather$maxtemp) == FALSE)),equals(0))
  expect_that(length(which((daily_weather$meantemp <= daily_weather$maxtemp) == FALSE)),equals(0))
  expect_that(length(which((daily_weather$mintemp <= daily_weather$meantemp) == FALSE)),equals(0))
})

test_that("Monthly option returns 8 columns", {
  expect_that(dim(monthly_weather)[2], equals(8))
  expect_that(sum(colnames(monthly_weather) == c("year", "month", "mintemp", "maxtemp", "meantemp", "precipitation", "locally_measured", "battery_low")), equals(8))
})

test_that("Monthly temperatures ok", {
  expect_that(length(which((monthly_weather$mintemp <= monthly_weather$maxtemp) == FALSE)),equals(0))
  expect_that(length(which((monthly_weather$meantemp <= monthly_weather$maxtemp) == FALSE)),equals(0))
  expect_that(length(which((monthly_weather$mintemp <= monthly_weather$meantemp) == FALSE)),equals(0))
})

test_that("Newmoon option returns 8 columns", {
  expect_that(dim(newmoon_weather)[2], equals(8))
  expect_that(sum(colnames(newmoon_weather) == c("newmoonnumber", "date", "mintemp", "maxtemp", "meantemp", "precipitation", "locally_measured", "battery_low")), equals(8))
})

test_that("Newmoon temperatures ok", {
  expect_that(length(which((newmoon_weather$mintemp <= newmoon_weather$maxtemp) == FALSE)),equals(0))
  expect_that(length(which((newmoon_weather$meantemp <= newmoon_weather$maxtemp) == FALSE)),equals(0))
  expect_that(length(which((newmoon_weather$mintemp <= newmoon_weather$meantemp) == FALSE)),equals(0))
})

test_that("`fill_missing_weather` works", {
  expect_error(daily_filled <- weather(fill = TRUE, path = portal_data_path), NA)

  # get regional data
  portal4sw <- read.csv(full_path('PortalData/Weather/Portal4sw_regional_weather.csv', portal_data_path),
                        na.strings = c(""), header = TRUE,
                        colClasses = c("character", rep("integer", 3), "character",
                                       "integer", rep("character", 3), "Date"))
  sansimon <- read.csv(full_path('PortalData/Weather/Sansimon_regional_weather.csv', portal_data_path),
                       na.strings = c(""), header = TRUE,
                       colClasses = c("character", rep("integer", 3), "character",
                                      "integer", rep("character", 3), "Date"))
  region <- dplyr::full_join(portal4sw, sansimon, by =
                               c("date", "year", "month", "day", "element")) %>%
    dplyr::filter(date >= "1980-01-01") %>%
    dplyr::group_by(date, element) %>%
    dplyr::summarize(value = mean(c(value.x, value.y), na.rm = TRUE) / 10) %>%
    dplyr::ungroup() %>%
    tidyr::spread(element, value) %>%
    dplyr::mutate(mintemp = TMIN,
                  maxtemp = TMAX,
                  meantemp = TOBS,
                  precipitation = PRCP,
                  year = lubridate::year(date),
                  month = lubridate::month(date),
                  day = lubridate::day(date)) %>%
    dplyr::select(year, month, day, mintemp, maxtemp, meantemp, precipitation)

  missing_vals <- daily_filled %>%
    dplyr::filter(is.na(mintemp) | is.na(maxtemp) | is.na(meantemp) |
                    is.na(precipitation)) %>%
    dplyr::left_join(region, by = c("year", "month", "day"))

  expect_true(all(which(is.na(missing_vals$mintemp.x)) %in%
                    which(is.na(missing_vals$mintemp.y))))
  expect_true(all(which(is.na(missing_vals$maxtemp.x)) %in%
                    which(is.na(missing_vals$maxtemp.x))))
  expect_true(all(which(is.na(missing_vals$meantemp.x)) %in%
                    which(is.na(missing_vals$meantemp.x))))
  expect_true(all(which(is.na(missing_vals$precipitation.x)) %in%
                    which(is.na(missing_vals$precipitation.x))))
})
