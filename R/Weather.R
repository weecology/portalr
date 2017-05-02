#A function to summarize hourly weather data
#with options to summarize by day (level=daily) or month (level=monthly)

`%>%` <- magrittr::`%>%`


weather <- function(level, path = '~') {


  weather_new=read.csv(FullPath('PortalData/Weather/Portal_weather.csv', path), na.strings=c(""), stringsAsFactors = FALSE)
  weather_old=read.csv(FullPath('PortalData/Weather/Portal_weather_19801989.csv', path), na.strings=c("-99"), stringsAsFactors = FALSE)
  NDVI=read.csv(FullPath('PortalData/NDVI/monthly_NDVI.csv', path), na.strings=c("-99"), stringsAsFactors = FALSE)

  # Data cleanup
  ##TO DO: Fill in missing data with means/nearby station data
  
  NDVI$Month=as.numeric(gsub( ".*-", "", NDVI$Date )); NDVI$Year=as.numeric(gsub( "-.*$", "", NDVI$Date ))
  
  ###########Summarise by Day ----------------------
  days = weather_new %>% 
    dplyr::group_by(Year, Month, Day) %>%
    dplyr::summarize(MinTemp=min(TempAir),MaxTemp=max(TempAir),MeanTemp=mean(TempAir),Precipitation=sum(Precipitation))
  
  weather=dplyr::bind_rows(weather_old[1:3442,],days) %>% dplyr::select(Year,Month,Day,MinTemp,MaxTemp,MeanTemp,Precipitation)
  
if (level=='Monthly') {
  
  ##########Summarise by Month -----------------
  
  weather = weather %>% 
    dplyr::group_by(Year, Month) %>%
    dplyr::summarize(MinTemp=min(MinTemp,na.rm=T),MaxTemp=max(MaxTemp,na.rm=T),MeanTemp=mean(MeanTemp,na.rm=T),Precipitation=sum(Precipitation,na.rm=T))
  
  weather=dplyr::full_join(weather,NDVI) %>% dplyr::select(-Date, -X) %>% dplyr::arrange(Year,Month)
  weather$NDVI=as.numeric(weather$NDVI)
  }

  
  return(weather)
}
