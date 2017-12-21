#' @importFrom magrittr "%>%"

#' @title Weather by day or month
#'
#' @description Summarize hourly weather data to either daily or monthly level.
#'
#' @param level specify 'Monthly' or 'Daily'
#' @param path specify where to locate Portal data
#'
#' @export
#'
weather <- function(level, path = '~') {


  weather_new=read.csv(FullPath('PortalData/Weather/Portal_weather.csv', path), na.strings=c(""), stringsAsFactors = FALSE)
  weather_old=read.csv(FullPath('PortalData/Weather/Portal_weather_19801989.csv', path), na.strings=c("-99"), stringsAsFactors = FALSE)
  NDVI=read.csv(FullPath('PortalData/NDVI/monthly_NDVI.csv', path), na.strings=c("-99"), stringsAsFactors = FALSE)

  ###########Summarise by Day ----------------------
  days = weather_new %>%
    dplyr::group_by(year, month, day) %>%
    dplyr::summarize(mintemp=min(airtemp),maxtemp=max(airtemp),meantemp=mean(airtemp),precipitation=sum(precipitation))

  weather=dplyr::bind_rows(weather_old[1:3442,],days) %>% dplyr::select(year,month,day,mintemp,maxtemp,meantemp,precipitation)

if (level=='Monthly') {

  ##########Summarise by Month -----------------

  weather = weather %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarize(mintemp=min(mintemp,na.rm=T),maxtemp=max(maxtemp,na.rm=T),meantemp=mean(meantemp,na.rm=T),precipitation=sum(precipitation,na.rm=T))

  weather=dplyr::full_join(weather,NDVI) %>% dplyr::select(-date, -X) %>% dplyr::arrange(year,month)
  weather$NDVI=as.numeric(weather$NDVI)
  }


  return(weather)
}
