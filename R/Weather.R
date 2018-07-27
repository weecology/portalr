#' @importFrom magrittr "%>%"

#' @title Weather by day, calendar month, or lunar month
#'
#' @description Summarize hourly weather data to either daily, monthly, or lunar monthly level.
#'
#' @param level specify 'monthly', 'daily', or 'newmoon'
#' @param fill specify if missing data should be filled, passed to \code{fill_missing_weather}
#' @param path specify where to locate Portal data
#'
#' @export
#'
weather <- function(level = "daily", fill = FALSE, path = '~') {
  level <- tolower(level)
  weather_new <- read.csv(full_path('PortalData/Weather/Portal_weather.csv', path),
                       na.strings = c(""), stringsAsFactors = FALSE)
  weather_old <- read.csv(full_path('PortalData/Weather/Portal_weather_19801989.csv', path),
                       na.strings = c("-99"), stringsAsFactors = FALSE)
  moon_dates <- read.csv(full_path('PortalData/Rodents/moon_dates.csv', path),
                         na.strings = c(""), stringsAsFactors = FALSE)

  ###########Summarise by Day ----------------------
  days <- weather_new %>%
    dplyr::group_by(year, month, day) %>%
    dplyr::summarize(mintemp = min(airtemp), maxtemp = max(airtemp), meantemp = mean(airtemp), precipitation = sum(precipitation), battv = min(battv, na.rm = TRUE))

  weather <- dplyr::bind_rows(weather_old[1:3442, ], days)  %>%
    dplyr::mutate(locally_measured = TRUE, battv = ifelse(is.infinite(battv), NA, battv), battery_low = ifelse(battv<11, TRUE, FALSE)) %>%
    dplyr::select(year, month, day, mintemp, maxtemp, meantemp, precipitation, locally_measured, battery_low)

  if (fill)
  {
    weather <- fill_missing_weather(weather, path)
  }

  if (level == 'monthly') {

    ##########Summarise by Month -----------------

    weather = weather %>%
      dplyr::group_by(year, month) %>%
      dplyr::summarize(mintemp = min(mintemp, na.rm = TRUE), maxtemp = max(maxtemp, na.rm = TRUE), meantemp = mean(meantemp, na.rm = TRUE),
                       precipitation = sum(precipitation, na.rm = TRUE), locally_measured = all(locally_measured),
                       battery_low = all(battery_low, na.rm = TRUE)) %>%
      dplyr::arrange(year, month) %>%
      dplyr::select(year, month, mintemp, maxtemp, meantemp, precipitation, locally_measured, battery_low) %>%
      dplyr::mutate(battery_low = ifelse(year < 2003, NA, battery_low))
  } else if (level == 'newmoon') {

    ##########Summarise by lunar month -----------------

    weather$date <- as.Date(paste(weather$year, weather$month, weather$day, sep = "-"))
    newmoon_number <- moon_dates$newmoonnumber[-1]
    newmoon_start <- as.Date(moon_dates$newmoondate[-nrow(moon_dates)])
    newmoon_end <- as.Date(moon_dates$newmoondate[-1])
    newmoon_match_number <- NULL
    newmoon_match_date <- NULL
    for (i in seq(newmoon_number)) {
      temp_dates <- as.character(seq.Date(newmoon_start[i] + 1, newmoon_end[i], 1))
      temp_numbers <- rep(newmoon_number[i], length(temp_dates))
      newmoon_match_date <- c(newmoon_match_date, temp_dates)
      newmoon_match_number <- c(newmoon_match_number, temp_numbers)
    }
    newmoon_match_date <- as.Date(newmoon_match_date)
    weather$newmoonnumber <- newmoon_match_number[match(weather$date, newmoon_match_date)]

    weather <- weather %>%
      dplyr::group_by(newmoonnumber) %>%
      dplyr::summarize(date = max(date, na.rm = TRUE), mintemp = min(mintemp, na.rm = TRUE),
                       maxtemp = max(maxtemp, na.rm = TRUE), meantemp = mean(meantemp, na.rm = TRUE),
                       precipitation = sum(precipitation,  na.rm = TRUE),
                       locally_measured = all(locally_measured),
                       battery_low = all(battery_low, na.rm = TRUE)) %>%
      dplyr::arrange(newmoonnumber) %>%
      dplyr::select(newmoonnumber, date, mintemp, maxtemp, meantemp, precipitation,
                    locally_measured, battery_low) %>%
      dplyr::mutate(battery_low = ifelse(date < "2003-01-01", NA, battery_low))
  }

  return(weather)
}

#' @title Fill missing weather with regional data
#'
#' @description Use two weather stations in San Simon valley to fill in
#' missing weather data in the daily time series
#'
#' @param weather a dataframe of daily weather data
#' @param path specify where to locate regional data
#'
#' @noRd
#'
fill_missing_weather <- function(weather, path = "~") {
  portal4sw <- read.csv(full_path('PortalData/Weather/Portal4sw_regional_weather.csv', path),
                        na.strings = c(""), header = TRUE,
                        colClasses = c("character", rep("integer", 3), "character",
                                       "integer", rep("character", 3), "Date"))
  sansimon <- read.csv(full_path('PortalData/Weather/Sansimon_regional_weather.csv', path),
                       na.strings = c(""), header = TRUE,
                       colClasses = c("character", rep("integer", 3), "character",
                                      "integer", rep("character", 3), "Date"))
  region <- dplyr::full_join(portal4sw, sansimon, by =
                               c("date", "year", "month", "day", "element")) %>%
    dplyr::filter(date >= "1980-01-01")

  regionmeans <- region %>%
    dplyr::group_by(date, element) %>%
    dplyr::summarize(value = mean(c(value.x, value.y), na.rm = TRUE) / 10) %>%
    dplyr::ungroup() %>%
    tidyr::spread(element, value) %>%
    dplyr::mutate(tmin = TMIN,
                  tmax = TMAX,
                  tobs = TOBS,
                  precip = PRCP,
                  year = as.integer(lubridate::year(date)),
                  month = as.integer(lubridate::month(date)),
                  day = lubridate::day(date)) %>%
    dplyr::filter(!(is.na(tmin) & is.na(tmax) & is.na(tobs) & is.na(precip))) %>%
    dplyr::select(year, month, day, precip, tmin, tmax, tobs)

  filled_data <- dplyr::full_join(regionmeans, weather, by = c("year", "month", "day")) %>%
    dplyr::arrange(year, month, day) %>%
    dplyr::mutate(locally_measured = ifelse(any(is.na(c(mintemp, maxtemp, meantemp, precipitation))), FALSE, TRUE),
                  mintemp = ifelse(is.na(mintemp), tmin, mintemp),
                  maxtemp = ifelse(is.na(maxtemp), tmax, maxtemp),
                  meantemp = ifelse(is.na(meantemp), tobs, meantemp),
                  precipitation = ifelse(is.na(precipitation), precip, precipitation)) %>%
    dplyr::select(year, month, day, mintemp, maxtemp, meantemp, precipitation, locally_measured, battery_low)

  return(as.data.frame(filled_data))
}
