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
  #colnames(trapping_table) = c("day", "month","year", "period", "plot", "sampled","effort")
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
#' @param unknowns String. If unknowns=False, unknown species removed.
#'
#' @return Data.table with species info added and unknown species processed
#' according to the argument unknowns.
#'
#' @export
#'
process_unknownsp = function(rodent_data, unknowns) {
  if (unknowns)
  {
    #Rename all unknowns and non-target rodents to "other"
    rodent_species_merge = rodent_data %>%
      dplyr::filter(rodent == 1) %>%
      dplyr::mutate(species = replace(species, unidentified == 1, "other")) %>%
      dplyr::mutate(species = replace(species, censustarget == 0, "other"))
  } else {
    rodent_species_merge = rodent_data %>%
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
find_incomplete_censuses = function(trapping_table) {
  incompsampling = trapping_table %>% dplyr::filter(sampled==0 ) %>%
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
  if (!incomplete) {
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
    dplyr::select(year,month,plot,treatment)
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
                                   by=c("month"="month","year"="year","period"="period","plot"="plot"))
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
#'
#' @export
#'
fill_weight = function(rodent_data, tofill)
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
  #      (i) see who is still missing weight
  missing_wgt_idx <- is.na(rodent_data$wgt) | rodent_data$wgt <= 0

  #      (ii) see who is a juvenile and species has juvenile weight
  juv_idx <- !is.na(rodent_data$age) & (rodent_data$age == 'J') &
    !is.na(rodent_data$juvwgt)

  #      (iii) fill in juvenile weight for known juveniles
  rodent_data$wgt[missing_wgt_idx & juv_idx] <- rodent_data$juvwgt[missing_wgt_idx & juv_idx]

  #      (iv) fill in average weight for everyone else
  rodent_data$wgt[missing_wgt_idx & !juv_idx] <- rodent_data$meanwgt[missing_wgt_idx & !juv_idx]

  #      (v) remove added columns for juvenile and average weight
  rodent_data <- dplyr::select(rodent_data, -juvwgt, -meanwgt)

  #      (vi) if all else fails, convert to 0, so that sums will work correctly
  rodent_data$wgt[is.na(rodent_data$wgt)] <- 0

  return(rodent_data)
}

#' @name clean_rodent_data
#'
#' @title Do basic cleaning of Portal rodent data
#'
#' @description This function does basic quality control of the Portal rodent
#'   data. It is mainly called from \code{\link{get_rodent_data}}, with
#'   several arguments passed along.
#'
#'   The specific steps it does are, in order:
#'     (1) add in missing weight data via \code{\link{fill_weight}})
#'     (2) remove records with "bad" period codes or plot numbers via
#'         \code{\link{remove_suspect_entries}}
#'     (3) remove records for unidentified species via
#'         \code{\link{process_unknownsp}}
#'     (4) exclude non-granivores via \code{\link{process_granivores}}
#'     (5) exclude incomplete trapping sessions via
#'         \code{\link{remove_incomplete_censuses}}
#'     (6) exclude the plots that aren't long-term treatments via
#'         \code{\link{filter_plots}}
#'
#' @param data_tables the list of data_tables, returned from calling
#'   \code{\link{loadData}}
#' @param fillweight specify whether to fill in unknown weights with other
#'   records from that individual or species, where possible
#' @param type specify subset of species; either all "Rodents" or only
#'   "Granivores"
#' @param unknowns either removes all individuals not identified to species
#'   (unknowns = FALSE) or sums them in an additional column (unknowns = TRUE)
#' @param incomplete either removes all data from incomplete trapping sessions
#'   (incomplete = FALSE) or includes them (incomplete = TRUE)
#' @param length specify subset of plots; use "All" plots or only "Longterm"
#'   plots (plots that have had same treatment for entire time series)
#'
#' @export
#'
clean_rodent_data <- function(data_tables, fillweight = FALSE, type = "Rodents",
                              unknowns = FALSE, incomplete = FALSE, length = "all")
{
  data_tables$rodent_data %>%
    dplyr::left_join(data_tables$species_table, by = "species") %>%
    fill_weight(fillweight) %>%
    remove_suspect_entries() %>%
    process_unknownsp(unknowns) %>%
    process_granivores(type) %>%
    remove_incomplete_censuses(data_tables$trapping_table, incomplete) %>%
    filter_plots(length) %>%
    dplyr::mutate(species = as.factor(species),
                  wgt = as.numeric(wgt),
                  energy = wgt ^ 0.75) %>%
    return()
}

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
        "https://raw.githubusercontent.com/weecology/PortalData/master/Plants/Portal_plant_censuses.csv"),
      stringsAsFactors = FALSE)
    date_table = read.csv(
      text = RCurl::getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Plants/Portal_plant_census_dates.csv"),
      stringsAsFactors = FALSE,
      na.strings = c('','none','unknown'))
    plots_table = read.csv(
      text = RCurl::getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/SiteandMethods/Portal_plots.csv"),
      stringsAsFactors = FALSE)
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
      file.path(path, "PortalData/Plants/Portal_plant_censuses.csv"),
      stringsAsFactors = FALSE)
    date_table = read.csv(
      file.path(path, "PortalData/Plants/Portal_plant_census_dates.csv"),
      stringsAsFactors = FALSE,
      na.strings = c('','none','unknown'))
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


#' @title Rename plant species
#'
#' @description Several species are suspected to have been IDed
#' incorrectly until 2017, when voucher samples were collected.
#'     acac greg -> mimo acul
#'     tali angu -> tali aura
#'     lcyi torr -> lyci ande
#'
#'
#' @param quadrat_data Data.table of raw plant quadrat data.
#' @param correct_sp T/F whether or not to use likely corrected plant IDs
#'                   [see Methods.md for explanation]
#'
#' @return Data.table with suspected incorrect plant species names replaced
#'
#' @export
#'
rename_species_plants = function(quadrat_data, correct_sp) {
  if (correct_sp) {
    quadrat_data$species <- gsub('acac greg','mimo acul',quadrat_data$species)
    quadrat_data$species <- gsub('tali angu','tali aura',quadrat_data$species)
    quadrat_data$species <- gsub('lyci torr','lyci ande',quadrat_data$species)
  }

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

#' @title Join census, dates, and plot treatment tables
#' @description Joins plant census table, census date table, and plot treatment tables
#' @param census_table Data_table of plant censuses
#' @param date_table Data table of dates of plant censuses
#' @param plots_table Data_table of treatments for the plots.
#'
#' @return Data.table of quadrat data with treatment info added.
#'
#' @export
join_census_to_dates = function(census_table, date_table, plots_table){
  census_dates = dplyr::left_join(census_table,date_table,
                                  by=c("year"="year","season"="season"))
  census_plots = dplyr::left_join(census_dates,plots_table,
                                  by=c("year"="year","start_month"="month","plot"="plot"))
  return(census_plots)
}

#' @title Join quadrat and census tables
#' @description Joins quadrat data with list of census dates
#' @param quadrat_data Data.table with raw rodent data.
#' @param census_table Data_table of when plots were censused.
#'
#' @return Data.table of raw quadrat data with census info added.
#'
#' @export
join_census_to_quadrats = function(quadrat_data, census_table){
  quadrat_table = dplyr::right_join(quadrat_data, census_table,
                                    by=c("year"="year","season"="season",
                                         "plot"="plot","quadrat"="quadrat"))
  return(quadrat_table)
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
#' @param correct_sp T/F whether or not to use likely corrected plant IDs, passed to \code{rename_species_plants}
#' @param length specify subset of plots; use "All" plots or only "Longterm"
#'   plots (plots that have had same treatment for entire time series)
#'
#' @export
#'
clean_plant_data <- function(data_tables, type = "All", unknowns = FALSE,
                             correct_sp, length = "all")
{
  data_tables$quadrat_data %>%
    dplyr::left_join(data_tables$species_table, by = "species") %>%
    rename_species_plants(correct_sp) %>%
    process_annuals(type) %>%
    process_unknownsp_plants(unknowns) %>%
    filter_plots(length) %>%

    return()
}


