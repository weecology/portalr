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
      na.strings = c(""),
      stringsAsFactors = FALSE)
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
      na.strings = c(""),
      stringsAsFactors = FALSE)
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
  if (unknowns)
  {
    #Rename all unknowns and non-target rodents to "other"
    rodent_species_merge =
      dplyr::left_join(species_table, rodent_data, by = "species") %>%
      dplyr::filter(rodent == 1) %>%
      dplyr::mutate(species = replace(species, unidentified == 1, "other")) %>%
      dplyr::mutate(species = replace(species, censustarget == 0, "other"))
  } else {
    rodent_species_merge =
      dplyr::left_join(species_table, rodent_data, by = "species") %>%
      dplyr::filter(rodent == 1, unidentified == 0, censustarget == 1)
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

#' @title Add User-specified time column
#'
#' @description
#' period codes denote the number of censuses that have occurred, but are
#' not the same as the number of censuses that should have occurred. Sometimes
#' censuses are missed (weather, transport issues,etc). You can't pick this
#' up with the period code. Because censues may not always occur monthly due to
#' the newmoon -  a new moon code was devised to give a standardized language
#' of time for forcasting in particular. This function allows the user to decide
#' if they want to use the rodent period code, the new moon code, the date of
#' the rodent census, or have their data with all three time formats
#'
#' @param summary_table Data.table with summarized rodent data.
#' @param newmoon_table Data_table linking newmoon codes with period codes.
#' @param time Character. Denotes whether newmoon codes, period codes,
#' and/or date are desired.
#'
#' @return Data.table of summarized rodent data with user-specified time format
#'
#' @export
#'

add_time = function(summary_table, newmoon_table, time='period'){
  newmoon_table$censusdate = as.Date(newmoon_table$censusdate)
  join_summary_newmoon = dplyr::right_join(newmoon_table,summary_table,
                                           by=c("period" = "period")) %>%
    dplyr::filter(period <= max(period,na.rm=T))
  if(time %in% c("NewMoon","Newmoon","newmoon")){
    join_summary_newmoon = dplyr::select(join_summary_newmoon, -newmoondate,
                                         -period,-censusdate)
  } else if (time %in% c("Date","date")) {
    join_summary_newmoon = dplyr::select(join_summary_newmoon,-newmoondate,
                                         -period,-newmoonnumber)
  } else if (time %in% c("All","all")) {
    join_summary_newmoon = dplyr::select(join_summary_newmoon,-newmoondate)
  } else
    join_summary_newmoon = summary_table

  return(join_summary_newmoon)
}

#' @title Make Crosstab
#'
#' @description convert summarized rodent data to crosstab form
#'
#' @param summary_data summarized rodent data
#' @param variable_name what variable to spread (default is "abundance")
#' @param ... other arguments to pass on to tidyr::spread
#'
#' @export
make_crosstab <- function(summary_data, variable_name = quo(abundance), ...){
  summary_data %>%
    tidyr::spread(species, !!variable_name, ...) %>%
    dplyr::ungroup()
}


#' @title Fill Weight
#'
#' @description fill in missing weight values with either a recently recorded weight for that individual or species average
#'
#' @param rodent_data raw rodent data
#' @param tofill logical whether to fill in missing values or not
#' @param species_list species table
#'
#' @export
#'
fill_weight = function(rodent_data, tofill, species_list)
{
  if (!tofill) return(rodent_data)

  ## [1] filter for missing weight, but known species and tag
  missing_wgt_idx <- (is.na(rodent_data$wgt) | rodent_data$wgt <= 0) &
    (!is.na(rodent_data$species)) &
    (!is.na(rodent_data$tag) & rodent_data$tag != "0" &
       rodent_data$tag != "-1" & rodent_data$tag != "0.00E+00")

  ## [2] substitute from same species and tag and valid weights
  for(this_row in which(missing_wgt_idx))
  {
    rodents_with_wgt <- dplyr::filter(rodent_data,
                                    tag == rodent_data$tag[this_row],
                                    species == rodent_data$species[this_row],
                                    wgt > 0)

    # if there are weights for the same individual
    if(nrow(rodents_with_wgt) > 0) {
      period_dist <- abs(rodent_data$period[this_row] - rodents_with_wgt$period)
      closest_records <- rodents_with_wgt$wgt[which.min(period_dist)]
      rodent_data$wgt[this_row] <- mean(closest_records, na.rm = TRUE)
    }
  }

  ## [3] fill in species weight for all remaining missing weights
  #      (i) merge in species-level info
  rodent_data <- dplyr::left_join(
    rodent_data,
    dplyr::select(species_list, species, juvwgt, meanwgt),
    by = "species")

  #      (ii) see who is still missing weight
  missing_wgt_idx <- is.na(rodent_data$wgt) | rodent_data$wgt <= 0

  #      (iii) see who is a juvenile and species has juvenile weight
  juv_idx <- !is.na(rodent_data$age) & (rodent_data$age == 'J') &
    !is.na(rodent_data$juvwgt)

  #      (iv) fill in juvenile weight for known juveniles
  rodent_data$wgt[missing_wgt_idx & juv_idx] <- rodent_data$juvwgt[missing_wgt_idx & juv_idx]

  #      (v) fill in average weight for everyone else
  rodent_data$wgt[missing_wgt_idx & !juv_idx] <- rodent_data$meanwgt[missing_wgt_idx & !juv_idx]

  #      (vi) remove added columns for juvenile and average weight
  rodent_data <- dplyr::select(rodent_data, -juvwgt, -meanwgt)

  return(rodent_data)
}
