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
#'   data. It is mainly called from \code{\link{summarize_plant_data}}, with
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
