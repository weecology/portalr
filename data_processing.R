library(RCurl)

loadData = function(path){
  if (path == 'repo'){
    rodents=read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv"),
                     na.strings=c(""), colClasses=c('tag'='character'), stringsAsFactors = FALSE)
    species=read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_species.csv"),
                     na.strings=c(""))
    trapping=read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv"))
    newmoons=read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv"))
    plots=read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/SiteandMethods/new_Portal_plots.csv"))
  } else {
    rodents = read.csv(paste(path, "PortalData/Rodents/Portal_rodent.csv", 
                             sep=""),
                       na.strings = c(""), colClasses = c('tag' = 'character'), 
                       stringsAsFactors = FALSE)
    species = read.csv(paste(path, "PortalData/Rodents/Portal_rodent_species.csv", 
                             sep=""),
                       na.strings = c(""))
    colnames(species)[1]="species"
    trapping = read.csv(paste(path, "PortalData/Rodents/Portal_rodent_trapping.csv", 
                              sep=""))
    newmoons = read.csv(paste(path, "PortalData/Rodents/moon_dates.csv", 
                              sep=""))
    plots = read.csv(paste(path, "PortalData/SiteandMethods/new_Portal_plots.csv", 
                           sep=""))
  }
  return(list(rodents,species,trapping,newmoons,plots))
}

remove_suspect_entries = function(rodent_data) {
  #Remove suspect trapping periods
  rodent_data = rodent_data[rodent_data$period > 0, ]
  
  #Remove unknown plots
  rodent_data = rodent_data[!is.na(rodent_data$plot), ]
}
