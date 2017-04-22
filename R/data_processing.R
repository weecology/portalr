library(dplyr)
library(tidyr)


#' @title Loads Portal rodent data files. 
#' 
#' @description Loads main Portal rodent data files from either
#' a user defined path or the online Github repository.
#' 
#' @param path string Containing path to PortalData folder
#'              should include ending /; if path = 'repo', 
#'              data is pulled from PortalData GitHub repository.
#'              
#' @return       List of 5 dataframes:
#' \itemize{
#' \item rodent_data. raw data on rodent captures
#' \item species_table. species code, names, types
#' \item trapping_table. when each plot was trapped
#' \item newmoons_table. pairs census periods with newmoons
#' \item plots_table. rodent treatment assignments for each plot.
#' }
#' 
#' @examples 
#' loadData('repo')
#' loadData('../')
#' 
loadData = function(path) {
  library(RCurl)
  if (path == 'repo') {
    rodent_data = read.csv(
      text = getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv"),
      na.strings = c(""),
      colClasses = c('tag' = 'character'),
      stringsAsFactors = FALSE)
    species_table = read.csv(
      text = getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_species.csv"),
      na.strings = c(""))
    trapping_table = read.csv(
      text = getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv"))
    newmoons_table = read.csv(
      text = getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv"))
    plots_table = read.csv(
      text = getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/SiteandMethods/Portal_plots.csv"))
  } else {
    rodent_data = read.csv(
      file.path(path, "PortalData/Rodents/Portal_rodent.csv"),
      na.strings = c(""), colClasses = c('tag' = 'character'),
      stringsAsFactors = FALSE)
    species_table = read.csv(
      file.path(path, "PortalData/Rodents/Portal_rodent_species.csv"),
      na.strings = c(""))
    trapping_table = read.csv(
      file.path(path, "PortalData/Rodents/Portal_rodent_trapping.csv"))
    newmoons_table = read.csv(
      file.path(path, "PortalData/Rodents/moon_dates.csv"))
    plots_table = read.csv(file.path(path, "PortalData/SiteandMethods/Portal_plots.csv"))
  }
  colnames(species_table)[1] = "species"
  colnames(trapping_table) = c("dy", "mo","yr", "period", "plot", "Sampled")
  colnames(newmoons_table)[3] = "period"
  colnames(plots_table)[2] = "mo"
  return(list(rodent_data, 
              species_table, 
              trapping_table, 
              newmoons_table, 
              plots_table))
}

#' @title Remove suspect trapping periods and unknown plots.
#' 
#' @description 
#' Removes records with negative period code or 
#' with missing plot numbers.
#'   
#' @param rodent_data Data.table of raw rodent data.
#' 
#' @return Data.table with suspect data removed.
#' 
remove_suspect_entries = function(rodent_data) {
  rodent_data = rodent_data[rodent_data$period > 0,]
  rodent_data = rodent_data[!is.na(rodent_data$plot),]
}

#' @title Processes unknown species.
#' 
#' @description 
#' Removes any records for unidentified species if unknowns=FALSE. 
#' If unknowns=TRUE, then their designation in the output file is
#' given as 'other'.
#' 
#' @param rodent_data Data.table with raw rodent data.
#' @param species_table Data.table with species info.
#' @param unknowns String. If unknowns=False, unknown species removed.
#' 
#' @return Data.table with species info added and unknown species processed
#' according to the argument unknowns.
process_unknownsp = function(rodent_data, species_table, unknowns) {
  if (unknowns == F) {
    rodent_species_merge = rodent_data %>%
      left_join(species_table, rodent_data, by = "species") %>%
      filter(Rodent == 1, Unidentified == 0, Census.Target == 1)
  }
  #Rename all unknowns and non-target rodents to "Other"
  else {
    rodent_species_merge =
      left_join(species_table, rodent_data, by = "species") %>%
      filter(Rodent == 1) %>%
      mutate(species = replace(species, Unidentified == 1, "Other")) %>%
      mutate(species = replace(species, Census.Target == 0, "Other"))
  }
  return(rodent_species_merge)
}

#' @title Filters out non-granivores.
#' 
#' @param rodent_species_merge Data table with raw rodent records
#'                             merged with species attributes from
#'                             species_table.
#' @param type String. If type=Granivores', non-granivores removed.
#' 
#' @return data.table with granivores processed according to argument 'type'.
process_granivores = function(rodent_species_merge, type) {
  if (type %in% c("Granivores", "granivores")) {
    granivore_data = rodent_species_merge %>%
      filter(Granivore == 1)
    return(granivore_data)
  } else {
    return(rodent_species_merge)
  }
}

#' @title Period code for incomplete censuses
#' 
#' @param trapping_table Data table. Data on when each plot was trapped. 
#' 
#' @return Data.table of period codes when not all plots were trapped.
find_incomplete_censuses = function(trapping_table){
  incompsampling=trapping_table %>% filter(Sampled==0 ) %>% 
    filter(period > 26) %>% distinct(period)
}

#' @title Remove incomplete censuses
#' 
#' @details
#' In some months, not all plots are trapped. Using this data can result in
#' biased monthly data, especially if summarizing for site or treatment. 
#' 
#' @param trapping_table Data table. Data on when each plot was trapped.
#' @param rodent_species_merge Data table. Merge of raw rodent records and 
#'                             species information.
#' @param incomplete Boolean. Denotes if users wants to keep incomplete censuses.
#' 
#' @return Data.table of merged rodent records and species info with incomplete
#'         censuses processed according to argument imcomplete.
remove_incomplete_censuses = function(trapping_table,
                                      rodent_species_merge,
                                      incomplete) {
  if (incomplete == F) {
    incompsampling = find_incomplete_censuses(trapping_table)
    rodent_species_merge = filter(rodent_species_merge,
                                  !period %in% incompsampling$period)
  }
    return(rodent_species_merge)
}

#' @title Filter plots
#' 
#' @details
#' Removes plots not needed for analysis. Currently only returns long-term
#' plots but could be adjusted in the future to return other subsets as well.
#'  
#' @param data Data table. Any data with a plot column.
#' @param length Character. Denotes if user wants only long-term plots.
#' 
#' @return Data.table filtered to the desired subset of plots.
filter_plots = function(data, length) {
  if (length %in% c("Longterm", "longterm")) {
    if("plot" %in% colnames(data)){
      data = data %>% filter(plot %in%
                               c(3, 4, 10, 11, 14, 15, 16, 17, 19, 21, 23))}
  }
  return(data)
}

#' @title Join rodent and plot tables
#' 
#' @param rodent_data Data.table with raw rodent data.
#' @param plots_table Data_table of treatments for the plots.
#' 
#' @return Data.table of raw rodent data with treatment info added.
join_plots_to_rodents = function(rodent_table, plots_table){
  plots_table = plots_table %>% group_by(yr,plot) %>% 
    select(yr,mo, plot,treatment)
  rodent_table = left_join(rodent_table,plots_table, 
                           by=c("yr"="yr","mo"="mo","plot"="plot"))
  return(rodent_table)
}

#' @title Join rodent and trapping tables
#' 
#' @param rodent_data Data.table with raw rodent data.
#' @param trapping_table Data_table of when plots were censused.
#' 
#' @return Data.table of raw rodent data with trapping info added.
join_trapping_to_rodents = function(rodent_table, trapping_table, incomplete){
  if (incomplete== F){
    incompsampling = find_incomplete_censuses(trapping_table)
    trapping_table = filter(trapping_table, !period %in% incompsampling$period)
  }
  rodent_table = right_join(rodent_table, trapping_table,
                            by=c("period"="period","plot"="plot"))
  return(rodent_table)
}

#' @title Add NewMoon Codes
#' 
#' @details 
#' period codes denote the number of censuses that have occurred, but are
#' not the same as the number of censuses that should have occurred. Sometimes
#' censuses are missed (weather, transport issues,etc). You can't pick this
#' up with the period code. Because censues may not always occur monthly due to
#' the newmoon -  a new moon code was devised to give a standardized language
#' of time for forcasting in particular.
#' 
#' @param summary_table Data.table with summarized rodent data.
#' @param newmoon_table Data_table linking newmoon codes with period codes.
#' @param time Character. Denotes whether newmoon codes are desired.
#' 
#' @return Data.table of summarized rodent data with period or newmoon code
add_newmoon_code = function(summary_table, newmoon_table, time){
  if(time %in% c("NewMoon","Newmoon","newmoon")){
      summary_table = right_join(newmoon_table,summary_table,
                                by=c("period"="period")) %>% 
        filter(period <= max(period,na.rm=T)) %>% 
        select(-NewMoonDate,-period,-CensusDate)
    }
  return(summary_table)
}

make_crosstab = function(summary_data){
  summary_data = summary_data %>% 
    spread(species, abundance) %>%
    ungroup()
}
