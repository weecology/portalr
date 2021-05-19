#' @title Weather by day, calendar month, or lunar month
#'
#' @description Summarize hourly weather data to either daily, monthly, or lunar monthly level.
#'
#' @param level specify 'monthly', 'daily', or 'newmoon'
#' @param fill specify if missing data should be filled, passed to \code{fill_missing_weather}
#' @param horizon Horizon (number of days) to use when calculating cumulative values
#' (eg warm weather precip)
#' @param temperature_limit Temperature limit (in C) to use when calculating cumulative values
#' (eg warm weather precip)
#' @inheritParams summarize_rodent_data
#'
#' @export
#'
weather <- function(level = "daily", fill = FALSE, horizon = 365, temperature_limit = 4,
                      path = get_default_data_path())
{
  options(dplyr.summarise.inform = FALSE)
  level <- tolower(level)
  weather_new <- load_datafile("Weather/Portal_weather.csv", na.strings = c(""), path = path)
  weather_old <- load_datafile("Weather/Portal_weather_19801989.csv", na.strings = c("-99"), path = path)
  moon_dates <- load_datafile("Rodents/moon_dates.csv", na.strings = c(""), path = path)

  ###########Summarize by Day ----------------------
  days <- weather_new %>%
    dplyr::group_by(.data$year, .data$month, .data$day) %>%
    dplyr::summarize(mintemp = min(.data$airtemp),
                     maxtemp = max(.data$airtemp),
                     meantemp = mean(.data$airtemp),
                     precipitation = sum(.data$precipitation),
                     battv = ifelse(all(is.na(.data$battv)), NA, min(.data$battv, na.rm = TRUE))) %>%
    dplyr::ungroup()

  weather <- dplyr::bind_rows(weather_old[1:3442, ], days)  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(locally_measured = ifelse(all(is.na(c(.data$mintemp, .data$maxtemp, .data$meantemp,
                                                        .data$precipitation))), NA, TRUE),
#                  battv = ifelse(is.infinite(.data$battv), NA, .data$battv),
                  battery_low = ifelse(.data$battv < 11, TRUE, FALSE)) %>%
    dplyr::select(c("year", "month", "day", "mintemp", "maxtemp", "meantemp",
                    "precipitation", "locally_measured", "battery_low"))

  if (fill)
  {
    weather <- fill_missing_weather(weather, path)
  }

  weather <- weather %>%
    dplyr::mutate(date = as.Date(paste(.data$year, .data$month, .data$day, sep = "-"))) %>%
    tidyr::complete(date = tidyr::full_seq(.data$date, period = 1), fill = list(value = NA)) %>%
    dplyr::mutate(year = lubridate::year(.data$date), month = lubridate::month(.data$date)) %>%
    dplyr::mutate(warm_days = zoo::rollapplyr(.data$mintemp, width = horizon,
                             FUN = function(x) length(which(x >= temperature_limit)), partial = TRUE)) %>%
    dplyr::mutate(cool_precip = zoo::rollapplyr(ifelse(.data$mintemp < temperature_limit,
                                                       .data$precipitation, 0),
                                            width = horizon, FUN = sum, partial = TRUE, na.rm = TRUE)) %>%
    dplyr::mutate(warm_precip = zoo::rollapplyr(ifelse(.data$mintemp >= temperature_limit,
                                                       .data$precipitation, 0),
                                                width = horizon, FUN = sum, partial = TRUE, na.rm = TRUE))

  if (level == "monthly") {
    ##########Summarize by Month -----------------
    normals <- load_datafile("Weather/PRISM_normals.csv", na.strings = c(""), path = path) %>%
      dplyr::filter(.data$month != "Annual") %>%
      dplyr::mutate(month = match(.data$month, month.name)) %>%
      dplyr::select(-c("tdmean", "vpdmin", "vpdmax"))
    weather <- weather %>%
      dplyr::group_by(.data$year, .data$month) %>%
      dplyr::summarize(mintemp = mean(.data$mintemp, na.rm = TRUE),
                       maxtemp = mean(.data$maxtemp, na.rm = TRUE),
                       meantemp = mean(.data$meantemp, na.rm = TRUE),
                       precipitation = sum(.data$precipitation, na.rm = TRUE),
                       warm_days = mean(.data$warm_days, na.rm = TRUE),
                       cool_precip = mean(.data$cool_precip, na.rm = TRUE),
                       warm_precip = mean(.data$warm_precip, na.rm = TRUE),
                       locally_measured = all(.data$locally_measured),
                       battery_low = all(.data$battery_low, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(c("year", "month", "mintemp", "maxtemp", "meantemp",
                      "precipitation", "locally_measured", "battery_low",
                      "warm_days", "cool_precip", "warm_precip")) %>%
      dplyr::mutate(battery_low = ifelse(.data$year < 2003, NA, .data$battery_low),
      mintemp = ifelse(is.finite(.data$mintemp), .data$mintemp, NA),
      maxtemp = ifelse(is.finite(.data$maxtemp), .data$maxtemp, NA),
      meantemp = ifelse(is.finite(.data$meantemp), .data$meantemp, NA),
      locally_measured = ifelse(is.na(.data$locally_measured), FALSE, .data$locally_measured)) %>%
      dplyr::full_join(normals, by="month") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(anomaly_ppt = .data$precipitation/.data$ppt,
                    anomaly_mint = .data$mintemp - .data$tmin,
                    anomaly_maxt = .data$maxtemp - .data$tmax,
                    anomaly_meant = .data$meantemp - .data$tmean) %>%
      dplyr::select(-c("ppt", "tmin", "tmax", "tmean")) %>%
      dplyr::arrange(.data$year, .data$month)

    } else if (level == "newmoon") {
    ##########Summarize by lunar month -----------------

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

    newmoon_sums <- moon_dates %>%
      dplyr::mutate(date = as.Date(.data$newmoondate)) %>%
      dplyr::left_join(weather, by = "date") %>%
      dplyr::select(c("date", "newmoonnumber", "warm_days", "cool_precip", "warm_precip"))

    weather$newmoonnumber <- newmoon_match_number[match(weather$date, newmoon_match_date)]

    weather <- weather %>%
      dplyr::group_by(.data$newmoonnumber) %>%
      dplyr::summarize(date = max(.data$date, na.rm = TRUE),
                       mintemp = mean(.data$mintemp, na.rm = TRUE),
                       maxtemp = mean(.data$maxtemp, na.rm = TRUE),
                       meantemp = mean(.data$meantemp, na.rm = TRUE),
                       precipitation = sum(.data$precipitation, na.rm = TRUE),
                       locally_measured = all(.data$locally_measured),
                       battery_low = all(.data$battery_low, na.rm = TRUE)) %>%
      dplyr::arrange(.data$newmoonnumber) %>%
      dplyr::select(c("newmoonnumber", "date", "mintemp", "maxtemp", "meantemp",
                      "precipitation", "locally_measured", "battery_low")) %>%
      dplyr::mutate(battery_low = ifelse(.data$date < "2003-01-01", NA, .data$battery_low)) %>%
      dplyr::left_join(newmoon_sums, by = c("newmoonnumber", "date")) %>%
      dplyr::mutate(mintemp = ifelse(is.finite(.data$mintemp), .data$mintemp, NA),
                    maxtemp = ifelse(is.finite(.data$maxtemp), .data$maxtemp, NA),
                    meantemp = ifelse(is.finite(.data$meantemp), .data$meantemp, NA),
                    locally_measured = ifelse(is.na(.data$locally_measured), FALSE,
                                              .data$locally_measured)) %>%
      tidyr::drop_na(.data$newmoonnumber)
  }

  return(as.data.frame(weather))
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
  portal4sw <- read.csv(normalized_file_path(path, 'PortalData/Weather/Portal4sw_regional_weather.csv'),
                        na.strings = c(""), header = TRUE, stringsAsFactors = FALSE) %>%
    dplyr::select(c("year", "month", "day", "date", "prcp", "tmax", "tmin", "tobs")) %>%
    dplyr::arrange(.data$year,.data$month, .data$day) %>%
    dplyr::filter(.data$date >= "1980-01-01") %>%
    dplyr::rename(precipitation = .data$prcp, maxtemp = .data$tmax, mintemp = .data$tmin,
                  meantemp = .data$tobs)

  sansimon <- read.csv(normalized_file_path(path, 'PortalData/Weather/Sansimon_regional_weather.csv'),
                       na.strings = c(""), header = TRUE, stringsAsFactors = FALSE) %>%
    dplyr::select(c("year", "month", "day", "date", "prcp")) %>%
    dplyr::arrange(.data$year,.data$month, .data$day) %>%
    dplyr::filter(.data$date >= "1980-01-01") %>%
    dplyr::rename(precipitation = .data$prcp)

  rustys <- read.csv(normalized_file_path(path, 'PortalData/Weather/Rustys_regional_weather.csv'),
                       na.strings = c(""), header = TRUE, stringsAsFactors = FALSE) %>%
    dplyr::group_by(.data$year, .data$month, .data$day) %>%
    dplyr::summarize(mintemp = min(.data$templow),
                     maxtemp = max(.data$temphigh),
                     meantemp = mean(.data$tempavg),
                     precipitation = sum(.data$preciptotal)) %>%
    dplyr::select(c("year", "month", "day", "mintemp", "maxtemp", "meantemp","precipitation")) %>%
    dplyr::arrange(.data$year,.data$month, .data$day)

  rodeo <- read.csv(normalized_file_path(path, 'PortalData/Weather/Rodeo_regional_weather.csv'),
                     na.strings = c(""), header = TRUE, stringsAsFactors = FALSE) %>%
    dplyr::group_by(.data$year, .data$month, .data$day) %>%
    dplyr::summarize(mintemp = min(.data$templow),
                     maxtemp = max(.data$temphigh),
                     meantemp = mean(.data$tempavg),
                     precipitation = sum(.data$preciptotal)) %>%
    dplyr::select(c("year", "month", "day", "mintemp", "maxtemp", "meantemp","precipitation")) %>%
    dplyr::arrange(.data$year,.data$month, .data$day)

  region1 <- dplyr::full_join(rodeo, rustys, by = c("year", "month", "day")) %>%
    dplyr::group_by(.data$year, .data$month, .data$day) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(mintemp = mean(c(.data$mintemp.y, .data$mintemp.x), na.rm=TRUE),
                  maxtemp = mean(c(.data$maxtemp.y, .data$maxtemp.x), na.rm=TRUE),
                  meantemp = mean(c(.data$meantemp.y, .data$meantemp.x), na.rm=TRUE),
                  precipitation = mean(c(.data$precipitation.y, .data$precipitation.x), na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!(is.na(.data$mintemp) & is.na(.data$maxtemp) &
                      is.na(.data$meantemp) & is.na(.data$precipitation))) %>%
    dplyr::select(c("year", "month", "day", "precipitation", "mintemp", "maxtemp", "meantemp"))

  region2 <- dplyr::full_join(portal4sw, sansimon, by = c("date", "year", "month", "day")) %>%
    dplyr::group_by(.data$year, .data$month, .data$day, .data$date) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(precipitation = mean(c(.data$precipitation.x, .data$precipitation.y),
                                       na.rm = TRUE, trim = 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(precipitation = ifelse(is.nan(.data$precipitation), NA, .data$precipitation)) %>%
    dplyr::filter(!(is.na(.data$mintemp) & is.na(.data$maxtemp) &
                      is.na(.data$meantemp) & is.na(.data$precipitation))) %>%
    dplyr::select(c("year", "month", "day", "precipitation", "mintemp", "maxtemp", "meantemp"))

  regionmeans <- dplyr::full_join(region2, region1, by = c("year", "month", "day")) %>%
    dplyr::group_by(.data$year, .data$month, .data$day) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(mintemp = ifelse(is.na(.data$mintemp.y), .data$mintemp.x, .data$mintemp.y),
                  maxtemp = ifelse(is.na(.data$maxtemp.y), .data$maxtemp.x, .data$maxtemp.y),
                  meantemp = ifelse(is.na(.data$meantemp.y), .data$meantemp.x, .data$meantemp.y),
                  precipitation = ifelse(is.na(.data$precipitation.y), .data$precipitation.x,
                                         .data$precipitation.y)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(precipitation = ifelse(is.nan(.data$precipitation), NA, .data$precipitation),
                  locally_measured = FALSE, battery_low = NA) %>%
    dplyr::filter(!(is.na(.data$mintemp) & is.na(.data$maxtemp) &
                      is.na(.data$meantemp) & is.na(.data$precipitation))) %>%
    dplyr::select(c("year", "month", "day", "precipitation", "mintemp", "maxtemp", "meantemp",
                    "locally_measured", "battery_low"))

  filled_data <- dplyr::full_join(regionmeans, weather, by = c("year", "month", "day")) %>%
    dplyr::mutate(mintemp = ifelse(is.na(.data$mintemp.y), .data$mintemp.x, .data$mintemp.y),
                  maxtemp = ifelse(is.na(.data$maxtemp.y), .data$maxtemp.x, .data$maxtemp.y),
                  meantemp = ifelse(is.na(.data$meantemp.y), .data$meantemp.x, .data$meantemp.y),
                  precipitation = ifelse(is.na(.data$precipitation.y), .data$precipitation.x,
                                         .data$precipitation.y),
                  locally_measured = ifelse(is.na(.data$locally_measured.y), .data$locally_measured.x,
                                         .data$locally_measured.y),
                  battery_low = ifelse(is.na(.data$battery_low.y), .data$battery_low.x,
                                         .data$battery_low.y)) %>%
    dplyr::select(c("year", "month", "day", "mintemp", "maxtemp", "meantemp", "precipitation",
                    "locally_measured", "battery_low")) %>%
    dplyr::arrange(.data$year, .data$month, .data$day) %>%
    dplyr::distinct()

  return(as.data.frame(filled_data))
}
