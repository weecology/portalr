

#' @title Remove data still under QA/QC.
#'
#' @description
#' Removes records that are still undergoing QA/QC (latest 12 months)
#'
#' @param full_data Data.table of raw data.
#' @param trapping_table Data.table with QCflag
#'
#' @return Data.table with latest 12 months of data removed.
#'
#' @noRd
clean_data <- function(full_data, trapping_table) {
  names <- colnames(full_data)
  full_data <- dplyr::left_join(full_data, trapping_table) %>%
    dplyr::filter(qcflag == 1) %>%
    dplyr::select(names) %>%
    unique()

  return(full_data)
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
#' @noRd
remove_suspect_entries <- function(rodent_data) {
  rodent_data %>%
    dplyr::filter(period > 0, !is.na(plot))
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
#' @noRd
process_unknownsp <- function(rodent_data, unknowns) {
  if (unknowns)
  {
    #Rename all unknowns and non-target rodents to "other"
    rodent_species_merge <- rodent_data %>%
      dplyr::filter(rodent == 1) %>%
      dplyr::mutate(species = replace(species, unidentified == 1, "other")) %>%
      dplyr::mutate(species = replace(species, censustarget == 0, "other"))
  } else {
    rodent_species_merge <- rodent_data %>%
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
#' @noRd
process_granivores <- function(rodent_species_merge, type) {
  if (type %in% c("Granivores", "granivores")) {
    granivore_data <- rodent_species_merge %>%
      dplyr::filter(granivore == 1)
    return(granivore_data)
  } else {
    return(rodent_species_merge)
  }
}

#' @title Period code for incomplete censuses
#' @description Determines incomplete censuses by finding dates when some plots were trapped, but others were not.
#' @param trapping_table Data table that contains sampled column (1 for sampled, 0 for unsampled)
#' @param min_plots minimum number of plots in a census for a census to count as sampled
#' @param min_traps minimum number of traps on a plot for a census to count as sampled
#'
#' @return Data.table of period codes when not all plots were trapped.
#'
#' @export
find_incomplete_censuses <- function(trapping_table, min_plots, min_traps) {

  trapping_table %>%
    dplyr::group_by(period) %>%
    dplyr::mutate(sampled = as.numeric(effort >= min_traps)) %>%
    dplyr::summarise(nplots = sum(sampled)) %>%
    dplyr::filter(nplots < min_plots) %>%
    dplyr::select(period)
}

#' @title Process incomplete censuses
#'
#' @description
#' In some months, not all plots are trapped. Using this data can result in
#' biased monthly data, especially if summarizing for site or treatment.
#'
#' @param rodent_species_merge Data table. Merge of raw rodent records and
#'                             species information.
#' @param fill_incomplete Logical. Denotes if users wants to keep incomplete censuses
#'         or fill with corrected estimates.
#'
#' @return Data.table of merged rodent records and species info with incomplete
#'         censuses processed according to argument fill_incomplete.
#'
#' @export
process_incomplete_censuses <- function(rodent_species_merge,
                                       fill_incomplete) {
  if (fill_incomplete) {
    #incompsampling <- find_incomplete_censuses(rodent_species_merge,trapping_table)
    warning("fill_incomplete not done")
    }
  return(rodent_species_merge)
}

#' @title Filter plots
#'
#' @description
#'   Removes plots not needed for analysis. Specific groups, such as "all" or
#'   "longterm" can be specified, as well as manual selection of plots.
#'
#' @param data any data.frame with a plot column.
#' @param plots specify subset of plots; can be a vector of plots, or specific
#'   sets: "all" plots or "Longterm" plots (plots that have had the same
#'   treatment for the entire time series)
#' @return Data.table filtered to the desired subset of plots.
#'
#' @noRd
filter_plots <- function(data, plots = NULL)
{
  if (is.character(plots))
  {
    plots <- tolower(plots)
    if (plots %in% c("longterm", "long-term"))
    {
      plots <- c(3, 4, 10, 11, 14, 15, 16, 17, 19, 21, 23)
    } else if (plots == "all") {
      plots <- NULL
    }
  }

  # if no selection then return unaltered data
  if (is.null(plots))
    return(data)

  # otherwise return filtered data
  dplyr::filter(data, plot %in% plots)
}

#' @title Join rodent and plot tables
#' @description Joins rodent data with list of plot types, by year, month and plot
#' @param rodent_data Data.table with raw rodent data.
#' @param plots_table Data_table of treatments for the plots.
#'
#' @return Data.table of raw rodent data with treatment info added.
#'
#' @noRd
join_plots_to_rodents <- function(rodent_data, plots_table) {
  plots_table <- plots_table %>%
    dplyr::group_by(year, plot) %>%
    dplyr::select(year, month, plot, treatment)

  join_by <- c(year = "year", month = "month", plot = "plot")
  dplyr::left_join(rodent_data, plots_table, by = join_by)
}

#' @title Join rodent and trapping tables
#' @description Joins rodent data with list of trapping dates, by period and plot
#' @param rodent_data Data.table with raw rodent data.
#' @param trapping_table Data_table of when plots were censused.
#' @param full_trapping Unfiltered data_table of when plots were censused.
#' @param min_plots minimum number of plots within a period for an
#'   observation to be included
#' @param min_traps minimum number of plots within a period for an
#'   observation to be included
#'
#' @return Data.table of raw rodent data with trapping info added.
#'
#' @noRd
join_trapping_to_rodents <- function(rodent_data, trapping_table,
                                     full_trapping, min_plots, min_traps) {

  incomplete_samples <- find_incomplete_censuses(full_trapping, min_plots, min_traps)
  trapping_table <- dplyr::filter(trapping_table, !period %in% incomplete_samples$period)

  dplyr::right_join(rodent_data, trapping_table,
                    by = c("month", "year", "period", "plot"))
}

#' Join plots and trapping tables
#'
#' @param trapping trapping_table from Portal data (can be filtered)
#' @param plots plots_table from Portal data
#'
#' @return trapping table with sampled column removed and treatment column
#'   added
#'
#' @noRd
join_plots_to_trapping <- function(trapping, plots) {

  plots_table <- plots %>%
    dplyr::group_by(year, plot) %>%
    dplyr::select(year, month, plot, treatment)

  join_by <- c(year = "year", month = "month", plot = "plot")
  dplyr::left_join(trapping, plots_table, by = join_by)
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
#' @noRd
add_time <- function(summary_table, newmoon_table, time = "period") {
  newmoon_table$censusdate <- as.Date(newmoon_table$censusdate)
  join_summary_newmoon <- dplyr::right_join(newmoon_table, summary_table,
                                           by = c("period" = "period")) %>%
    dplyr::filter(period <= max(period, na.rm = TRUE))

  if (tolower(time) == "newmoon") {
    join_summary_newmoon <- dplyr::select(join_summary_newmoon, -newmoondate,
                                         -period, -censusdate)
  } else if (tolower(time) == "date") {
    join_summary_newmoon <- dplyr::select(join_summary_newmoon, -newmoondate,
                                         -period, -newmoonnumber)
  } else if (tolower(time) == "all") {
    join_summary_newmoon <- dplyr::select(join_summary_newmoon, -newmoondate)
  } else
    join_summary_newmoon <- summary_table

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
#' @noRd
make_crosstab <- function(summary_data, variable_name = quo(abundance), ...){
  summary_data %>%
    tidyr::spread(species, !!variable_name, ...) %>%
    dplyr::ungroup()
}

#' @title Fill Weight
#'
#' @description fill in missing weight values with either a recently recorded
#'   weight for that individual or species average
#'
#' @param rodent_data raw rodent data
#' @param tofill logical whether to fill in missing values or not
#'
#' @noRd
fill_weight <- function(rodent_data, tofill)
{
  if (!tofill) return(rodent_data)

  ## [1] filter for missing weight, but known species and tag
  missing_wgt_idx <- (is.na(rodent_data$wgt) | rodent_data$wgt <= 0) &
    (!is.na(rodent_data$species)) &
    (!is.na(rodent_data$tag) & rodent_data$tag != "0" &
       rodent_data$tag != "-1" & rodent_data$tag != "0.00E+00")

  ## [2] substitute from same species and tag and valid weights
  for (this_row in which(missing_wgt_idx))
  {
    rodents_with_wgt <- dplyr::filter(rodent_data,
                                      tag == rodent_data$tag[this_row],
                                      species == rodent_data$species[this_row],
                                      wgt > 0)

    # if there are weights for the same individual
    if (nrow(rodents_with_wgt) > 0) {
      period_dist <- abs(rodent_data$period[this_row] - rodents_with_wgt$period)
      closest_records <- rodents_with_wgt$wgt[which.min(period_dist)]
      rodent_data$wgt[this_row] <- mean(closest_records, na.rm = TRUE)
    }
  }

  ## [3] fill in species weight for all remaining missing weights
  #      (i) see who is still missing weight
  missing_wgt_idx <- is.na(rodent_data$wgt) | rodent_data$wgt <= 0

  #      (ii) see who is a juvenile and species has juvenile weight
  juv_idx <- !is.na(rodent_data$age) & (rodent_data$age == "J") &
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
#'     (1) add in missing weight data
#'     (2) remove records with "bad" period codes or plot numbers
#'     (3) remove records for unidentified species
#'     (4) exclude non-granivores
#'     (5) exclude incomplete trapping sessions
#'     (6) exclude the plots that aren't long-term treatments
#'
#' @param data_tables the list of data_tables, returned from calling
#'   \code{\link{load_data}}
#' @param fillweight specify whether to fill in unknown weights with other
#'   records from that individual or species, where possible
#' @param type specify subset of species; either all "Rodents" or only
#'   "Granivores"
#' @param unknowns either removes all individuals not identified to species
#'   (unknowns = FALSE) or sums them in an additional column (unknowns = TRUE)
#' @param fill_incomplete Logical. Either reports raw data from incomplete trapping sessions
#'   (fill_incomplete = FALSE) or estimates corrected values (fill_incomplete = TRUE)
#'
#' @export
#'
clean_rodent_data <- function(data_tables, fillweight = FALSE, type = "Rodents",
                              unknowns = FALSE, fill_incomplete = FALSE)
{
  data_tables$rodent_data %>%
    dplyr::left_join(data_tables$species_table, by = "species") %>%
    fill_weight(fillweight) %>%
    remove_suspect_entries() %>%
    process_unknownsp(unknowns) %>%
    process_granivores(type) %>%
    process_incomplete_censuses(fill_incomplete) %>%
    dplyr::mutate(species = as.factor(species),
                  wgt = as.numeric(wgt),
                  energy = wgt ^ 0.75)
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
#' @noRd
rename_species_plants <- function(quadrat_data, correct_sp) {
  if (correct_sp) {
    quadrat_data$species <- gsub("acac greg", "mimo acul", quadrat_data$species)
    quadrat_data$species <- gsub("tali angu", "tali aura", quadrat_data$species)
    quadrat_data$species <- gsub("lyci torr", "lyci ande", quadrat_data$species)
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
#' @noRd
process_unknownsp_plants <- function(quadrat_data, unknowns) {
  if (unknowns)
  {
    #Rename all unknowns to "other"
    quadrat_species_merge <- quadrat_data %>%
      dplyr::mutate(species = replace(species, commonname == "Unknown", "other"))
  } else {
    quadrat_species_merge <- quadrat_data %>%
      dplyr::filter(commonname != "Unknown")
  }
  return(quadrat_species_merge)
}

#' @title Restricts species to specified community group
#' @description Filters the plant data to a specific group.
#' @param quadrat_sp_data Data table with raw quadrat plant data
#'   merged with species attributes from species_table.
#' @param type String.
#'              If `type == "Annuals"`, returns all annual species
#'              If `type == "Summer Annuals"`, returns all annual species that can be found in the summer
#'              If `type == "Winter Annuals"`, returns all annual species that can be found in the winter
#'              If `type == "Non-woody"`, removes shrub and subshrub species
#'              If `type == "Perennials"`, returns all perennial species (includes shrubs and subshrubs)
#'              If `type == "Shrubs"`, returns only shrubs and subshrubs
#'
#' @return data.table with species processed according to argument `type`.
#'
#' @noRd
process_annuals <- function(quadrat_sp_data, type) {
  if (type %in% c("annuals", "annual")) {
    return(dplyr::filter(quadrat_sp_data, duration == "Annual"))
  } else if (type %in% c("non-woody", "nonwoody")) {
    return(dplyr::filter(quadrat_sp_data, !community %in% c("Shrub", "Subshrub")))
  } else if (type %in% c("perennials", "perennial")) {
    return(dplyr::filter(quadrat_sp_data, duration == "Perennial"))
  } else if (type %in% c("shrubs", "shrub")) {
    return(dplyr::filter(quadrat_sp_data, community %in% c("Shrub", "Subshrub")))
  } else if (type %in% c("summer annual", "summer annuals", "summer-annual", "summer-annuals")) {
    return(dplyr::filter(quadrat_sp_data, community %in% c("Summer Annual",
                                                           "Summer and Winter Annual")))
  } else if (type %in% c("winter annual", "winter annuals", "winter-annual", "winter-annuals")) {
    return(dplyr::filter(quadrat_sp_data, community %in% c("Winter Annual",
                                                           "Summer and Winter Annual")))
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
#' @noRd
join_census_to_dates <- function(census_table, date_table, plots_table) {

  # add column to date_table for month for determining treatment
  date_table$treat_month <- date_table$start_month

  # start month was unknown for 1986-1987 but treatments don't change by month
  date_table$treat_month[date_table$year %in% c(1986, 1987)] <- 1

  # start month was unknown for 1985; plant treatment changed in August but other treatments were same
  date_table$treat_month[(date_table$year == 1985 & date_table$season == 'winter')] <- 3

  # Samson et al 1992 says the summer plant census of 1985 was in either august or september
  date_table$treat_month[(date_table$year == 1985 & date_table$season == 'summer')] <- 8

  # add column for number of quadrats censused per plot per census
  #   and join date and plot info
  census_table %>%
    dplyr::group_by(year, season, plot) %>%
    dplyr::summarize(nquads = sum(censused)) %>%
    dplyr::left_join(date_table, by = c(year = "year", season = "season")) %>%
    dplyr::left_join(plots_table, by = c(year = "year", treat_month = "month", plot = "plot"))
}

#' @title Join quadrat and census tables
#' @description Joins quadrat data with list of census dates
#' @param quadrat_data Data table with raw quadrat data.
#' @param census_table Data table of when plots were censused.
#'
#' @return Data table of raw quadrat data with census info added.
#'
#' @noRd
join_census_to_quadrats <- function(quadrat_data, census_table) {
  quadrat_data %>%
    dplyr::right_join(census_table,
                      by = c(year = "year", season = "season",
                             plot = "plot", quadrat = "quadrat"))
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
#'     (1) correct species names according to recent vouchers, if requested
#'     (2) restrict species to annuals or non-woody
#'     (3) remove records for unidentified species
#'     (5) exclude the plots that aren't long-term treatments
#'
#' @param data_tables the list of data_tables, returned from calling
#'   \code{\link{load_plant_data}}
#' @param type specify subset of species;
#'              If type=Annuals, removes all non-annual species.
#'              If type=Non-woody, removes shrub and subshrub species
#'              If type=Perennials, returns all perennial species (includes shrubs and subshrubs)
#'              If type=Shrubs, returns only shrubs and subshrubs
#'              If type=Winter-annual, returns all annuals found in winter
#'              IF type=Summer-annual, returns all annuals found in summer
#' @param unknowns either removes all individuals not identified to species
#'   (unknowns = FALSE) or sums them in an additional column (unknowns = TRUE)
#' @param correct_sp T/F whether or not to use likely corrected plant IDs,
#'   passed to \code{rename_species_plants}
#'
#' @export
#'
clean_plant_data <- function(data_tables, type = "All", unknowns = FALSE,
                             correct_sp = TRUE)
{
  data_tables$quadrat_data %>%
    dplyr::filter(!grepl(3,notes)) %>%
    dplyr::left_join(data_tables$species_table, by = "species") %>%
    rename_species_plants(correct_sp) %>%
    process_annuals(type) %>%
    process_unknownsp_plants(unknowns) %>%
    dplyr::mutate(species = as.factor(species))
}
