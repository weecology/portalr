#' @importFrom magrittr "%>%"

#' @title Weather by day or month
#'
#' @description Summarize hourly weather data to either daily or monthly level.
#'
#' @param level specify 'Monthly' or 'Daily'
#' @param fill specify if missing data should be filled, passed to \code{fill_missing_weather}
#' @param path specify where to locate Portal data
#'
#' @export
#'
weather <- function(level = "Daily", fill = FALSE, path = '~') {
  weather_new=read.csv(FullPath('PortalData/Weather/Portal_weather.csv', path), na.strings=c(""), stringsAsFactors = FALSE)
  weather_old=read.csv(FullPath('PortalData/Weather/Portal_weather_19801989.csv', path), na.strings=c("-99"), stringsAsFactors = FALSE)
  NDVI=read.csv(FullPath('PortalData/NDVI/monthly_NDVI.csv', path), na.strings=c("-99"), stringsAsFactors = FALSE)

  ###########Summarise by Day ----------------------
  days = weather_new %>%
    dplyr::group_by(year, month, day) %>%
    dplyr::summarize(mintemp=min(airtemp),maxtemp=max(airtemp),meantemp=mean(airtemp),precipitation=sum(precipitation))

  weather=dplyr::bind_rows(weather_old[1:3442,],days) %>%
    dplyr::select(year,month,day,mintemp,maxtemp,meantemp,precipitation)

    weather = fill_missing_weather(weather, fill)

  if (level=='Monthly') {

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
      dplyr::summarize(mintemp=min(mintemp,na.rm=T),maxtemp=max(maxtemp,na.rm=T),meantemp=mean(meantemp,na.rm=T),precipitation=sum(precipitation,na.rm=T)) %>%
      dplyr::full_join(NDVI) %>%
      dplyr::arrange(year,month) %>%
      dplyr::select(year, month, mintemp, maxtemp, meantemp, precipitation, ndvi) %>%
      dplyr::mutate(ndvi = as.numeric(ndvi))
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
#'
#' @export
#'
fill_missing_weather <- function(daily, fill) {

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
      dplyr::mutate(mintemp = ifelse(is.na(mintemp),tmin,mintemp), maxtemp = ifelse(is.na(maxtemp),tmax,maxtemp),
             meantemp = ifelse(is.na(meantemp),tobs,meantemp), precipitation = ifelse(is.na(precipitation),precip,precipitation)) %>%
      dplyr::select(year,month,day,mintemp,maxtemp,meantemp,precipitation)
    daily = filled_data
    }
  return(daily)
}
