#' @importFrom magrittr "%>%"


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
#' @export
#'
#' @examples
#' portal_data <- loadData("repo")
loadData <- function(path = "~") {
  if (path == 'repo') {
    rodent_data = read.csv(
      text = RCurl::getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv"),
      na.strings = c(""),
      colClasses = c('tag' = 'character'),
      stringsAsFactors = FALSE)
    species_table = read.csv(
      text = RCurl::getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_species.csv"),
      na.strings = c(""))
    trapping_table = read.csv(
      text = RCurl::getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv"))
    newmoons_table = read.csv(
      text = RCurl::getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv"))
    plots_table = read.csv(
      text = RCurl::getURL(
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
  colnames(trapping_table) = c("day", "month","year", "period", "plot", "sampled")
  colnames(newmoons_table)[3] = "period"
  colnames(plots_table)[2] = "month"
  return(list(rodent_data = rodent_data,
              species_table = species_table,
              trapping_table = trapping_table,
              newmoons_table = newmoons_table,
              plots_table = plots_table))
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
#' @export
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
#'
#' @export
#'
process_unknownsp = function(rodent_data, species_table, unknowns) {
  if (unknowns == F) {
    rodent_species_merge =
      dplyr::left_join(species_table, rodent_data, by = "species") %>%
      dplyr::filter(rodent == 1, unidentified == 0, censustarget == 1)
  }
  #Rename all unknowns and non-target rodents to "other"
  else {
    rodent_species_merge =
      dplyr::left_join(species_table, rodent_data, by = "species") %>%
      dplyr::filter(rodent == 1) %>%
      dplyr::mutate(species = replace(species, unidentified == 1, "other")) %>%
      dplyr::mutate(species = replace(species, censustarget == 0, "other"))
  }
  return(rodent_species_merge)
}

#' @title Filters out non-granivores.
#' @description If type=granivores, removes all non-granivore species.
#' @param rodent_species_merge Data table with raw rodent records
#'                             merged with species attributes from
#'                             species_table.
#' @param type String. If type=granivores', non-granivores removed.
#'
#' @return data.table with granivores processed according to argument 'type'.
#'
#' @export
#'
process_granivores = function(rodent_species_merge, type) {
  if (type %in% c("Granivores", "granivores")) {
    granivore_data = rodent_species_merge %>%
      dplyr::filter(granivore == 1)
    return(granivore_data)
  } else {
    return(rodent_species_merge)
  }
}

#' @title Period code for incomplete censuses
#' @description Determines incomplete censuses by finding dates when some plots were trapped, but others were not.
#' @param trapping_table Data table. Data on when each plot was trapped.
#'
#' @return Data.table of period codes when not all plots were trapped.
#'
#' @export
find_incomplete_censuses = function(trapping_table){
  incompsampling=trapping_table %>% dplyr::filter(sampled==0 ) %>%
    dplyr::filter(period > 26) %>% dplyr::distinct(period)
}

#' @title Remove incomplete censuses
#'
#' @description
#' In some months, not all plots are trapped. Using this data can result in
#' biased monthly data, especially if summarizing for site or treatment.
#'
#' @param rodent_species_merge Data table. Merge of raw rodent records and
#'                             species information.
#' @param trapping_table Data table. Data on when each plot was trapped.
#' @param incomplete Boolean. Denotes if users wants to keep incomplete censuses.
#'
#' @return Data.table of merged rodent records and species info with incomplete
#'         censuses processed according to argument imcomplete.
#'
#' @export
remove_incomplete_censuses = function(rodent_species_merge,
                                      trapping_table,
                                      incomplete) {
  if (incomplete == F) {
    incompsampling = find_incomplete_censuses(trapping_table)
    rodent_species_merge = dplyr::filter(rodent_species_merge,
                                  !period %in% incompsampling$period)
  }
    return(rodent_species_merge)
}

#' @title Filter plots
#'
#' @description
#' Removes plots not needed for analysis. Currently only returns long-term
#' plots but could be adjusted in the future to return other subsets as well.
#'
#' @param data Data table. Any data with a plot column.
#' @param length Character. Denotes if user wants only long-term plots.
#'
#' @return Data.table filtered to the desired subset of plots.
#'
#' @export
filter_plots = function(data, length) {
  if (length %in% c("Longterm", "longterm")) {
    if("plot" %in% colnames(data)){
      data = data %>% dplyr::filter(plot %in%
                               c(3, 4, 10, 11, 14, 15, 16, 17, 19, 21, 23))}
  }
  return(data)
}

#' @title Join rodent and plot tables
#' @description Joins rodent data with list of plot types, by year, month and plot
#' @param rodent_data Data.table with raw rodent data.
#' @param plots_table Data_table of treatments for the plots.
#'
#' @return Data.table of raw rodent data with treatment info added.
#'
#' @export
join_plots_to_rodents = function(rodent_data, plots_table){
  plots_table = plots_table %>% dplyr::group_by(year,plot) %>%
    dplyr::select(year,month, plot,treatment)
  rodent_table = dplyr::left_join(rodent_data,plots_table,
                           by=c("year"="year","month"="month","plot"="plot"))
  return(rodent_table)
}

#' @title Join rodent and trapping tables
#' @description Joins rodent data with list of trapping dates, by period and plot
#' @param rodent_data Data.table with raw rodent data.
#' @param trapping_table Data_table of when plots were censused.
#' @param incomplete Boolean. Denotes if users wants to keep incomplete censuses.
#'
#' @return Data.table of raw rodent data with trapping info added.
#'
#' @export
join_trapping_to_rodents = function(rodent_data, trapping_table, incomplete){
  if (incomplete== F){
    incompsampling = find_incomplete_censuses(trapping_table)
    trapping_table = dplyr::filter(trapping_table, !period %in% incompsampling$period)
  }
  rodent_table = dplyr::right_join(rodent_data, trapping_table,
                            by=c("period"="period","plot"="plot"))
  return(rodent_table)
}

#' @title Add NewMoon Codes
#'
#' @description
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
#'
#' @export
#'
add_newmoon_code = function(summary_table, newmoon_table, time){
  if(time %in% c("NewMoon","Newmoon","newmoon")){
      summary_table = dplyr::right_join(newmoon_table,summary_table,
                                by=c("period" = "period")) %>%
        dplyr::filter(period <= max(period,na.rm=T)) %>%
        dplyr::select(-newmoondate,-period,-censusdate)
    }
  return(summary_table)
}

#' @title Make Crosstab
#'
#' @description convert summarized rodent data to crosstab form
#'
#' @param summary_data summarized rodent data - must include 'abundance' column
#'
#' @export
make_crosstab = function(summary_data){
  summary_data = summary_data %>%
    tidyr::spread(species, abundance) %>%
    dplyr::ungroup()
}
