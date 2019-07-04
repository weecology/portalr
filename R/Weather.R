#' @title Weather by day, calendar month, or lunar month
#'
#' @description Summarize hourly weather data to either daily, monthly, or lunar monthly level.
#'
#' @param level specify 'monthly', 'daily', or 'newmoon'
#' @param fill specify if missing data should be filled, passed to \code{fill_missing_weather}
#' @inheritParams summarize_rodent_data
#'
#' @export
#'
weather <- function(level = "daily", fill = FALSE, path = get_default_data_path())
{
  level <- tolower(level)
  weather_new <- read.csv(full_path("PortalData/Weather/Portal_weather.csv", path),
                          na.strings = c(""), stringsAsFactors = FALSE)
  weather_old <- read.csv(full_path("PortalData/Weather/Portal_weather_19801989.csv", path),
                          na.strings = c("-99"), stringsAsFactors = FALSE)
  moon_dates <- read.csv(full_path("PortalData/Rodents/moon_dates.csv", path),
                         na.strings = c(""), stringsAsFactors = FALSE)

  ###########Summarise by Day ----------------------
  days <- weather_new %>%
    dplyr::group_by(.data$year, .data$month, .data$day) %>%
    dplyr::summarize(mintemp = min(.data$airtemp),
                     maxtemp = max(.data$airtemp),
                     meantemp = mean(.data$airtemp),
                     precipitation = sum(.data$precipitation),
                     battv = min(.data$battv, na.rm = TRUE))

  weather <- dplyr::bind_rows(weather_old[1:3442, ], days)  %>%
    dplyr::mutate(locally_measured = TRUE,
                  battv = ifelse(is.infinite(.data$battv), NA, .data$battv),
                  battery_low = ifelse(.data$battv < 11, TRUE, FALSE)) %>%
    dplyr::select(c("year", "month", "day", "mintemp", "maxtemp", "meantemp",
                    "precipitation", "locally_measured", "battery_low"))

  if (fill)
  {
    weather <- fill_missing_weather(weather, path)
  }

  if (level == "monthly") {

    ##########Summarise by Month -----------------

    weather = weather %>%
      dplyr::group_by(.data$year, .data$month) %>%
      dplyr::summarize(mintemp = min(.data$mintemp, na.rm = TRUE),
                       maxtemp = max(.data$maxtemp, na.rm = TRUE),
                       meantemp = mean(.data$meantemp, na.rm = TRUE),
                       precipitation = sum(.data$precipitation, na.rm = TRUE),
                       locally_measured = all(.data$locally_measured),
                       battery_low = all(.data$battery_low, na.rm = TRUE)) %>%
      dplyr::arrange(.data$year, .data$month) %>%
      dplyr::select(c("year", "month", "mintemp", "maxtemp", "meantemp",
                      "precipitation", "locally_measured", "battery_low")) %>%
      dplyr::mutate(battery_low = ifelse(.data$year < 2003, NA, .data$battery_low))
  } else if (level == "newmoon") {
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
      dplyr::summarize(date = max(.data$date, na.rm = TRUE),
                       mintemp = min(.data$mintemp, na.rm = TRUE),
                       maxtemp = max(.data$maxtemp, na.rm = TRUE),
                       meantemp = mean(.data$meantemp, na.rm = TRUE),
                       precipitation = sum(.data$precipitation,  na.rm = TRUE),
                       locally_measured = all(.data$locally_measured),
                       battery_low = all(.data$battery_low, na.rm = TRUE)) %>%
      dplyr::arrange(.data$newmoonnumber) %>%
      dplyr::select(c("newmoonnumber", "date", "mintemp", "maxtemp", "meantemp",
                      "precipitation", "locally_measured", "battery_low")) %>%
      dplyr::mutate(battery_low = ifelse(.data$date < "2003-01-01", NA, .data$battery_low))
  }

  return(weather)
}

#' @title Fill missing weather with regional data
#'
#' @description Use two weather stations in San Simon valley to fill in
#' missing weather data in the daily time series
#'
#' @param weather a dataframe of daily weather data
#' @inheritParams weather
#'
#' @noRd
#'
fill_missing_weather <- function(weather, path = get_default_data_path())
{
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
