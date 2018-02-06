# WIP -------------
# TO DO:
#   - remove_suspect_entries_plants function does nothing.  I left it as a placeholder in case we need it





#' @title Loads Portal plant data files.
#'
#' @description Loads main Portal plant data files from either
#' a user defined path or the online Github repository.
#'
#' @param path string Containing path to PortalData folder
#'              should include ending /; if path = 'repo',
#'              data is pulled from PortalData GitHub repository.
#'
#' @return       List of 4 dataframes:
#' \itemize{
#' \item quadrat_data. raw plant quadrat data
#' \item species_table. species code, names, types
#' \item census_table. indicates whether each quadrat was counted in each census; area of each quadrat
#' \item date_table. start and end date of each plant census
#' \item plots_table. rodent treatment assignments for each plot.
#' }
#'
#' @export
#'
#' @examples
#' portal_plant_data <- loadPlantData("repo")
loadPlantData <- function(path = "~") {
  if (path == 'repo') {
    quadrat_data = read.csv(
      text = RCurl::getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Plants/Portal_plant_quadrats.csv"),
      na.strings = c(""),
      stringsAsFactors = FALSE)
    species_table = read.csv(
      text = RCurl::getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Plants/Portal_plant_species.csv"),
      na.strings = c(""),
      stringsAsFactors = FALSE)
    census_table = read.csv(
      text = RCurl::getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Plants/Portal_plant_censuses.csv"))
    date_table = read.csv(
      text = RCurl::getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Plants/Portal_plant_census_dates.csv"))
    plots_table = read.csv(
      text = RCurl::getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/SiteandMethods/Portal_plots.csv"))
  } else {
    quadrat_data = read.csv(
      file.path(path, "PortalData/Plants/Portal_plant_quadrats.csv"),
      na.strings = c(""),
      stringsAsFactors = FALSE)
    species_table = read.csv(
      file.path(path, "PortalData/Plants/Portal_plant_species.csv"),
      na.strings = c(""),
      stringsAsFactors = FALSE)
    census_table = read.csv(
      file.path(path, "PortalData/Plants/Portal_plant_censuses.csv"))
    date_table = read.csv(
      file.path(path, "PortalData/Plants/Portal_plant_census_dates.csv"))
    plots_table = read.csv(file.path(path, "PortalData/SiteandMethods/Portal_plots.csv"))
  }
  colnames(species_table)[1] = "species"
  colnames(species_table)[4] = "sp"
  return(list(quadrat_data = quadrat_data,
              species_table = species_table,
              census_table = census_table,
              date_table = date_table,
              plots_table = plots_table))
}


#' @title Remove suspect data
#'
#' @description
#'
#'
#' @param quadrat_data Data.table of raw plant quadrat data.
#'
#' @return Data.table with suspect data removed.
#'
#' @export
#'
remove_suspect_entries_plants = function(quadrat_data) {
  #rodent_data = rodent_data[rodent_data$period > 0,]
  #rodent_data = rodent_data[!is.na(rodent_data$plot),]
  return(quadrat_data)
}

#' @title Processes unknown species -- plant data.
#'
#' @description
#' Removes any records for unidentified species if unknowns=FALSE.
#' If unknowns=TRUE, then their designation in the output file is
#' given as 'other'.
#'
#' @param quadrat_data Data.table with raw plant quadrat data.
#' @param unknowns String. If unknowns=False, unknown species removed.
#'
#' @return Data.table with species info added and unknown species processed
#' according to the argument unknowns.
#'
#' @export
#'
process_unknownsp_plants = function(quadrat_data, unknowns) {
  if (unknowns)
  {
    #Rename all unknowns to "other"
    quadrat_species_merge = quadrat_data %>%
      dplyr::mutate(species = replace(species, commonname == "Unknown", "other"))
  } else {
    quadrat_species_merge = quadrat_data %>%
      dplyr::filter(commonname != "Unknown")
  }
  return(quadrat_species_merge)
}

#' @title Restricts species to annuals or non-woody as specified
#' @description If type=Annuals, removes all non-annual species.
#'              If type=Non-woody, removes shrub species
#' @param quadrat_sp_data Data table with raw quadrat plant data
#'                             merged with species attributes from
#'                             species_table.
#' @param type String. Either "Annuals" or "Non-woody" results in filtering
#'
#' @return data.table with species processed according to argument 'type'.
#'
#' @export
#'
process_annuals = function(quadrat_sp_data, type) {
  if (type %in% c("Annuals", "annuals")) {
    annual_data = quadrat_sp_data %>%
      dplyr::filter(duration == 'Annual')
    return(annual_data)
  } else if (type %in% c("Non-woody", "non-woody")) {
    nonwoody_data = quadrat_sp_data %>%
      dplyr::filter(!(community %in% c("Shrub","Subshrub")))
    return(nonwoody_data)
  } else {
    return(quadrat_sp_data)
  }
}

#' @title Finds incomplete (early) plant censuses
#' @description Determines incomplete censuses by finding dates when some quadrats were not counted.
#' @param census_table Data table. Data on when each plot and quadrat was counted.
#'
#' @return Data.table of dates when not all quadrats were counted.
#'
#' @export
find_incomplete_censuses_plants = function(census_table) {
  incompsampling = census_table %>% dplyr::filter(censused==0 ) #%>%
     dplyr::distinct(year,season)
}

#' @title Remove incomplete censuses -- plant censuses
#'
#' @description
#' Prior to 1983, fewer than the usual 16 quadrats per plot were counted
#'
#' @param quadrat_sp_merge Data table. Merge of raw plant data and
#'                             species information.
#' @param cesus_table Data table. Data on when each quadrat was counted
#' @param incomplete Boolean. Denotes if users wants to keep incomplete censuses.
#'
#' @return Data.table of merged plant quadrat data and species info with incomplete
#'         censuses processed according to argument imcomplete.
#'
#' @export
remove_incomplete_censuses_plants = function(quadrat_sp_merge,
                                      census_table,
                                      incomplete) {
  if (!incomplete) {
    incompsampling = find_incomplete_censuses_plants(census_table)
    quadrat_sp_merge = dplyr::filter(quadrat_sp_merge,
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



#' @name clean_plant_data
#'
#' @title Do basic cleaning of Portal plant data
#'
#' @description This function does basic quality control of the Portal plant
#'   data. It is mainly called from \code{\link{get_plant_data}}, with
#'   several arguments passed along.
#'
#'   The specific steps it does are, in order:
#'     (1) remove records with "bad" period codes or plot numbers via
#'         \code{\link{remove_suspect_entries}}
#'     (2) restrict species to annuals or non-woody via
#'         \code{link{process_annuals}}
#'     (3) remove records for unidentified species via
#'         \code{\link{process_unknownsp_plants}}
#'     (4) exclude incomplete trapping sessions via
#'         \code{\link{remove_incomplete_censuses}}
#'     (5) exclude the plots that aren't long-term treatments via
#'         \code{\link{filter_plots}}
#'
#' @param data_tables the list of data_tables, returned from calling
#'   \code{\link{loadPlantData}}
#' @param type specify subset of species; either "All" - includes annuals,
#'  perennials, and shrubs; "Annuals" - only annuals; or "Non-woody" - includes
#'  annuals and perennials but not shrubs
#' @param unknowns either removes all individuals not identified to species
#'   (unknowns = FALSE) or sums them in an additional column (unknowns = TRUE)
#' @param incomplete either removes all data from incomplete trapping sessions
#'   (incomplete = FALSE) or includes them (incomplete = TRUE)
#' @param length specify subset of plots; use "All" plots or only "Longterm"
#'   plots (plots that have had same treatment for entire time series)
#'
#' @export
#'
clean_plant_data <- function(data_tables, type = "All", unknowns = FALSE,
                             incomplete = FALSE, length = "all")
{
  data_tables$quadrat_data %>%
    dplyr::left_join(data_tables$species_table, by = "species") %>%
    remove_suspect_entries_plants() %>%
    process_annuals(type) %>%
    process_unknownsp_plants(unknowns) #%>%
    remove_incomplete_censuses_plants(data_tables$census_table, incomplete) %>%
    filter_plots(length) %>%
    dplyr::mutate(species = as.factor(species),
                  wgt = as.numeric(wgt),
                  energy = wgt ^ 0.75) %>%
    return()
}
