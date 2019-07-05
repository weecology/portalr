#' @title Plot-level plant data
#'
#' @param plant_data cleaned plant data
#' @param census_info_table table of plant census dates, with treatment column
#' @param output specify whether to return "abundance" or "cover"
#' @param min_quads minimum number of quadrats (out of 16) for a plot to be included
#'
#' @return fully crossed year x season x plot x species flat table of observations
#'   with effort (number of quadrats) and treatment columns. Any plot not
#'   sufficiently (as defined by min_quads) sampled is returned with NA
#'   for effort and the output value of interest
#'
#' @noRd
#'
make_plant_plot_data <- function(plant_data, census_info_table,
                                 output, min_quads = 1)
{

  grouping <- rlang::quos(.data$year, .data$season, .data$plot, .data$species)
  wt <- switch(output,
               "abundance" = rlang::quo(.data$abundance),
               "cover" = rlang::quo(.data$cover))
  filler <- list(n = as.integer(0))

  plant_data %>%
    dplyr::group_by(!!!grouping) %>%
    dplyr::summarise(n = sum(!!wt, na.rm = TRUE))  %>%
    dplyr::ungroup() %>%
    dplyr::right_join(census_info_table[, c("year","season","plot")],
                      by = c("year", "season", "plot")) %>%
    tidyr::complete(!!!grouping, fill = filler) %>%
    dplyr::full_join(census_info_table, by = c("year", "season", "plot")) %>%
    dplyr::select(c("year", "season", "plot", "species", "n", "nquads", "treatment")) %>%
    dplyr::filter(!is.na(.data$species)) %>%
    dplyr::mutate(n = replace(.data$n, .data$nquads < min_quads, NA),
                  nquads = replace(.data$nquads, .data$nquads < min_quads, NA)) %>%
    dplyr::rename(!!output := .data$n)
}

#' Plant data summarized at the relevant level (plot, treatment, site)
#'
#' @param plot_data plant data summarized at the plot level
#' @param level specify level of interest ("plot", "treatment", "site")
#' @param output specify whether to return "abundance" or "cover" [n.b. cover measurement started in 2015]
#' @param min_quads minimum number of quadrats (out of 16) for a plot to be included
#'
#' @return fully crossed flat table of observations with effort (number of
#'   quadrats). The crossing depends on the level:
#'   "plot" is year x season x treatment x plot x species, "treatment" is
#'   year x season x treatment x species, and "site" is period x species. Any
#'   observations not sufficiently (as defined by min_quads) sampled are returned with NA
#'   for nquads, nplots, and the output value of interest
#'
#' @noRd
make_plant_level_data <- function(plot_data, level, output,
                                  min_quads = 1) {

  plot_data <- dplyr::rename(plot_data, n := !!output)
  grouping <- switch(level,
                     "plot" = c("year", "season", "plot", "species"),
                     "treatment" = c("year", "season", "treatment", "species"),
                     "site" = c("year", "season", "species"))

  level_data <- dplyr::group_by_at(plot_data, grouping) %>%
    dplyr::summarize(n = sum(.data$n, na.rm = TRUE),
                     quads = sum(.data$nquads, na.rm = TRUE),
                     nplots = dplyr::n_distinct(.data$plot)) %>%
    dplyr::ungroup()

  if (level == "plot")
  {
    level_data <- level_data %>%
      dplyr::mutate(n = replace(.data$n, .data$quads < min_quads, NA))
  }

  level_data %>%
    dplyr::rename(!!output := .data$n) %>%
    tibble::as_tibble()
}

#' Plant data prepared for output
#'
#' @param level_data plant data summarized at the level of interest
#' @param effort logical as to whether or not the effort columns should be
#'   included in the output
#' @param na_drop logical, drop NA values (representing insufficient sampling)
#' @param zero_drop logical, drop 0s (representing sufficient sampling, but no
#'   detections)
#' @param shape return data as a "crosstab" or "flat" list
#' @param level specify level of interest ("plot", "treatment", "site")
#' @param output specify whether to return "abundance" or "cover"
#'
#' @return fully crossed flat table of observations with effort (number of
#'   traps and number of plots). The crossing depends on the level:
#'   "plot" is period x treatment x plot x species, "treatment" is
#'   period x treatment x species, and "site" is period x species. Any
#'   observations not sufficiently (as defined by min_plots, and
#'   hierarchically by min_traps) sampled are returned with NA
#'   for ntraps, nplots, and the output value of interest
#'
#' @noRd
#'
prep_plant_output <- function(level_data, effort, na_drop,
                              zero_drop, shape, level, output) {

  out_data <- level_data

  if (effort == FALSE) {
    out_data <- dplyr::select(out_data, -.data$nplots, -.data$quads)
  } else if (level %in% c("plot", "site")) {
    out_data <- dplyr::select(out_data, -.data$nplots)
  }

  if (na_drop) {
    out_data <- na.omit(out_data)
  }

  if (shape == "crosstab") {
    out_data <- make_crosstab(out_data, output, NA)
  }

  if (zero_drop) {
    if (shape == "crosstab") {
      species_names <- as.character(unique(level_data$species))
      out_data <- out_data %>%
        dplyr::filter(rowSums(dplyr::select(., species_names)) != 0)
    } else { # shape == "flat"
      out_data <- out_data %>%
        dplyr::filter(output != 0)
    }
  }

  return(out_data)
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
      dplyr::mutate(species = replace(.data$species, .data$commonname == "Unknown", "other"))
  } else {
    quadrat_species_merge <- quadrat_data %>%
      dplyr::filter(.data$commonname != "Unknown")
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
    return(dplyr::filter(quadrat_sp_data, .data$duration == "Annual"))
  } else if (type %in% c("non-woody", "nonwoody")) {
    return(dplyr::filter(quadrat_sp_data, !.data$community %in% c("Shrub", "Subshrub")))
  } else if (type %in% c("perennials", "perennial")) {
    return(dplyr::filter(quadrat_sp_data, .data$duration == "Perennial"))
  } else if (type %in% c("shrubs", "shrub")) {
    return(dplyr::filter(quadrat_sp_data, .data$community %in% c("Shrub", "Subshrub")))
  } else if (type %in% c("summer annual", "summer annuals", "summer-annual", "summer-annuals")) {
    return(dplyr::filter(quadrat_sp_data, .data$community %in% c("Summer Annual", "Summer and Winter Annual")))
  } else if (type %in% c("winter annual", "winter annuals", "winter-annual", "winter-annuals")) {
    return(dplyr::filter(quadrat_sp_data, .data$community %in% c("Winter Annual", "Summer and Winter Annual")))
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
    dplyr::group_by(.data$year, .data$season, .data$plot) %>%
    dplyr::summarize(nquads = sum(.data$censused)) %>%
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
    dplyr::filter(!grepl(3, .data$notes)) %>%
    dplyr::left_join(data_tables$species_table, by = "species") %>%
    rename_species_plants(correct_sp) %>%
    process_annuals(type) %>%
    process_unknownsp_plants(unknowns) %>%
    dplyr::mutate(species = as.factor(.data$species))
}
