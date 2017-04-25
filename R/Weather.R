#A function to summarize hourly weather data
#with options to summarize by day (level=daily) or month (level=monthly)

#library(dtplyr)
library(dplyr)
library(tidyr)
library(lubridate)

#' Return normalized path for all operating systems
#'
#' @param ReferencePath a path to join with current working directory
#' @param BasePath Current working directory else path given
#'
#' @return
#' @export
#' @examples
#' FullPath('PortalData/Rodents/Portal_rodent.csv')
#' FullPath('PortalData/Rodents/Portal_rodent.csv', '~')
FullPath <- function( ReferencePath, BasePath=getwd()){
  BasePath = normalizePath(BasePath)
  Path = normalizePath(file.path(BasePath, ReferencePath), mustWork = FALSE)
  return (Path)
}


weather <- function(level, path = '~') {


  weather_new=read.csv(FullPath('PortalData/Weather/Portal_weather.csv', path), na.strings=c(""), stringsAsFactors = FALSE)
  weather_old=read.csv(FullPath('PortdalData/Weather/Portal_weather_19801989.csv', path), na.strings=c("-99"), stringsAsFactors = FALSE)
  NDVI=read.csv(FullPath('PortalData/NDVI/monthly_NDVI.csv', path), na.strings=c("-99"), stringsAsFactors = FALSE)

  # Data cleanup
  ##TO DO: Fill in missing data with means/nearby station data
  
  NDVI$Month=as.numeric(gsub( ".*-", "", NDVI$Date )); NDVI$Year=as.numeric(gsub( "-.*$", "", NDVI$Date ))
  
  ###########Summarise by Day ----------------------
  days = weather_new %>% 
    group_by(Year, Month, Day) %>%
    summarize(MinTemp=min(TempAir),MaxTemp=max(TempAir),MeanTemp=mean(TempAir),Precipitation=sum(Precipitation))
  
  weather=bind_rows(weather_old[1:3442,],days)
  
if (level=='Monthly') {
  
  ##########Summarise by Month -----------------
  
  weather = weather %>% 
    group_by(Year, Month) %>%
    summarize(MinTemp=min(MinTemp,na.rm=T),MaxTemp=max(MaxTemp,na.rm=T),MeanTemp=mean(MeanTemp,na.rm=T),Precipitation=sum(Precipitation,na.rm=T))
  
  weather=full_join(weather,NDVI) %>% select(-Date, -X) %>% arrange(Year,Month)
  weather$NDVI=as.numeric(weather$NDVI)
  }

  
  return(weather)
}
