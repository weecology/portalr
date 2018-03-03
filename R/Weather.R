#' @importFrom magrittr "%>%"

#' @title Weather by day, calendar month, or lunar month
#'
#' @description Summarize hourly weather data to either daily, monthly, or lunar monthly level.
#'
#' @param level specify 'monthly', 'daily', or 'lunar_monthly'
#' @param fill specify if missing data should be filled, passed to \code{fill_missing_weather}
#' @param path specify where to locate Portal data
#'
#' @export
#'
weather <- function(level = "daily", fill = FALSE, path = '~') {
  level = tolower(level)
  weather_new=read.csv(FullPath('PortalData/Weather/Portal_weather.csv', path), na.strings=c(""), stringsAsFactors = FALSE)
  weather_old=read.csv(FullPath('PortalData/Weather/Portal_weather_19801989.csv', path), na.strings=c("-99"), stringsAsFactors = FALSE)
  NDVI=read.csv(FullPath('PortalData/NDVI/monthly_NDVI.csv', path), na.strings=c("-99"), stringsAsFactors = FALSE)
  moon_dates <- read.csv(FullPath('PortalData/Rodents/moon_dates.csv', path), na.strings = c(""), stringsAsFactors = FALSE)

  ###########Summarise by Day ----------------------
  days = weather_new %>%
    dplyr::group_by(year, month, day) %>%
    dplyr::summarize(mintemp=min(airtemp),maxtemp=max(airtemp),meantemp=mean(airtemp),precipitation=sum(precipitation),battv=min(battv,na.rm=TRUE))

  weather=dplyr::bind_rows(weather_old[1:3442,],days)  %>%
    dplyr::mutate(locally_measured = TRUE, battv = ifelse(is.infinite(battv),NA,battv), battery_low = ifelse(battv<11,TRUE,FALSE)) %>%
    dplyr::select(year,month,day,mintemp,maxtemp,meantemp,precipitation,locally_measured,battery_low)

  weather = fill_missing_weather(weather, fill, path)

  if (level=='monthly') {

    ##########Summarise by Month -----------------

    if(!all(c("year", "month") %in% names(NDVI))) # make year and month column if necessary
    {
      NDVI$month <- lubridate::month(paste0(NDVI$date, "-01"))
      NDVI$year <- lubridate::year(paste0(NDVI$date, "-01"))
      NDVI$ndvi <- NDVI$NDVI
    }
    NDVI$ndvi[NDVI$ndvi == "NA"] <- NA

    weather = weather %>%
      dplyr::group_by(year, month) %>%
      dplyr::summarize(mintemp=min(mintemp,na.rm=T),maxtemp=max(maxtemp,na.rm=T),meantemp=mean(meantemp,na.rm=T),
                       precipitation=sum(precipitation,na.rm=T), locally_measured=all(locally_measured),
                       battery_low=all(battery_low,na.rm=TRUE)) %>%
      dplyr::full_join(NDVI, by = c("year", "month")) %>%
      dplyr::arrange(year,month) %>%
      dplyr::select(year, month, mintemp, maxtemp, meantemp, precipitation, ndvi, locally_measured, battery_low) %>%
      dplyr::mutate(ndvi = as.numeric(ndvi), battery_low=ifelse(year<2003,NA,battery_low))
  }

  if(level == 'lunar_monthly'){

    ##########Summarise by lunar month -----------------

    if (!all(c("year", "month") %in% names(NDVI))) {
      NDVI$month <- lubridate::month(paste0(NDVI$date, "-01"))
      NDVI$year <- lubridate::year(paste0(NDVI$date, "-01"))
      NDVI$date <- lubridate::date(paste0(NDVI$date, "-01"))
      NDVI$ndvi <- NDVI$NDVI
    }
    if(!"date" %in% names(NDVI)){
      NDVI$date <- as.Date(paste(NDVI$year, NDVI$month, "01", sep = "-"))
    }
    NDVI$ndvi[NDVI$ndvi == "NA"] <- NA
    weather$date <- as.Date(paste(weather$year, weather$month, weather$day, sep = "-"))
    newmoon_number <- moon_dates$newmoonnumber[-1]
    newmoon_start <- as.Date(moon_dates$newmoondate[-nrow(moon_dates)])
    newmoon_end <- as.Date(moon_dates$newmoondate[-1])
    newmoon_match_number <- NULL
    newmoon_match_date <- NULL   
    for(i in 1:length(newmoon_number)){
      temp_dates <- as.character(seq.Date(newmoon_start[i] + 1, newmoon_end[i], 1))
      temp_numbers <- rep(newmoon_number[i], length(temp_dates))
      newmoon_match_date <- c(newmoon_match_date, temp_dates)
      newmoon_match_number <- c(newmoon_match_number, temp_numbers)       
    }
    newmoon_match_date <- as.Date(newmoon_match_date)

    NDVI$newmoonnumber <- newmoon_match_number[match(NDVI$date, newmoon_match_date)]
    weather$newmoonnumber <- newmoon_match_number[match(weather$date, newmoon_match_date)]

    NDVI <- NDVI[ , -which(colnames(NDVI) == "date")]

    weather = weather %>% 
      dplyr::group_by(newmoonnumber) %>% 
      dplyr::summarize(date = max(date, na.rm = T), mintemp = min(mintemp, na.rm = T), 
                       maxtemp = max(maxtemp, na.rm = T), meantemp = mean(meantemp, na.rm = T), 
                       precipitation = sum(precipitation,  na.rm = T), 
                       locally_measured = all(locally_measured), 
                       battery_low = all(battery_low, na.rm = TRUE)) %>% 
      dplyr::full_join(NDVI, by = c("newmoonnumber")) %>% 
      dplyr::arrange(newmoonnumber) %>% 
      dplyr::select(newmoonnumber, date, mintemp, maxtemp, meantemp, precipitation,
                    ndvi, locally_measured, battery_low) %>% 
      dplyr::mutate(ndvi = as.numeric(ndvi), battery_low = ifelse(date < "2003-01-01", NA, battery_low))
  }

  return(weather)
}


#' @title Fill missing weather with regional data
#'
#' @description Use two weather stations in San Simon valley to fill in
#' missing weather data in the daily time series
#'
#' @param daily a dataframe of daily weather data
#' @param fill specify if missing data should be filled
#' @param path specify where to locate regional data
#'
#' @export
#'
fill_missing_weather <- function(daily, fill,path) {

  if(fill) {
    portal4sw = read.csv(FullPath('PortalData/Weather/Portal4sw_regional_weather.csv', path), na.strings=c(""), header=T,
                         colClasses=c("character", rep("integer",3), "character", "integer", rep("character",3), "Date"))
    sansimon = read.csv(FullPath('PortalData/Weather/Sansimon_regional_weather.csv', path), na.strings=c(""), header=T,
                        colClasses=c("character", rep("integer",3), "character", "integer", rep("character",3), "Date"))
    region = dplyr::full_join(portal4sw,sansimon,by=c("date","year","month","day","element")) %>% dplyr::filter(date >= "1980-01-01")

    precip = region %>% dplyr::filter(element == "PRCP") %>%
      dplyr::mutate(precip = rowMeans(cbind(value.x/10,value.y/10),na.rm=T)) %>%
      dplyr::select(year,month,day,precip)
    tmin = region %>% dplyr::filter(element == "TMIN") %>%
      dplyr::mutate(tmin = rowMeans(cbind(value.x/10,value.y/10),na.rm=T)) %>%
      dplyr::select(year,month,day,tmin)
    tmax = region %>% dplyr::filter(element == "TMAX") %>%
      dplyr::mutate(tmax = rowMeans(cbind(value.x/10,value.y/10),na.rm=T)) %>%
      dplyr::select(year,month,day,tmax)
    tobs = region %>% dplyr::filter(element == "TOBS") %>%
      dplyr::mutate(tobs = rowMeans(cbind(value.x/10,value.y/10),na.rm=T)) %>%
      dplyr::select(year,month,day,tobs)

    regionmeans = precip %>% dplyr::group_by(year,month,day) %>%
      dplyr::full_join(tmin,by = c("year", "month", "day")) %>% dplyr::full_join(tmax,by = c("year", "month", "day")) %>%
      dplyr::full_join(tobs,by = c("year", "month", "day")) %>% dplyr::arrange(year,month,day) %>% dplyr::distinct(.)

    filled_data = dplyr::full_join(regionmeans,daily,by = c("year", "month", "day")) %>% dplyr::arrange(year,month,day) %>%
      dplyr::mutate(locally_measured = ifelse(any(is.na(c(mintemp,maxtemp,meantemp,precipitation))),FALSE,TRUE),
                    mintemp = ifelse(is.na(mintemp),tmin,mintemp), maxtemp = ifelse(is.na(maxtemp),tmax,maxtemp),
                    meantemp = ifelse(is.na(meantemp),tobs,meantemp),
                    precipitation = ifelse(is.na(precipitation),precip,precipitation)) %>%
      dplyr::select(year,month,day,mintemp,maxtemp,meantemp,precipitation,locally_measured,battery_low)

    daily = filled_data
    }
  return(daily)
}
