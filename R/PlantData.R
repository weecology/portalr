#' @importFrom magrittr "%>%"
#' @importFrom rlang "!!"
#' @importFrom rlang "!!!"
#' @importFrom rlang ":="
#' @importFrom rlang quo
#' @importFrom rlang quos

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
make_plant_plot_data <- function(plant_data, census_info_table, output, min_quads = 1) {

  grouping <- rlang::quos(year, season, plot, species)
  wt <- switch(output,
               "abundance" = rlang::quo(abundance),
               "cover" = rlang::quo(cover))
  filler <- list(n = as.integer(0))



  test <- plant_data %>%
    dplyr::group_by(!!!grouping) %>%
    dplyr::summarise(n = sum(!!wt,na.rm=T))  %>%
    dplyr::right_join(census_info_table[,c("year","season","plot")], by = c("year", "season", "plot")) %>%
    tidyr::complete(!!!grouping, fill = filler) %>%
    dplyr::full_join(census_info_table, by = c("year", "season", "plot")) %>%
    dplyr::select(year, season, plot, species, n, nquads, treatment) %>%
    dplyr::filter(!is.na(species)) %>%
    dplyr::mutate(n = replace(n, nquads < min_quads, NA),
                  nquads = replace(nquads, nquads < min_quads, NA)) %>%
    dplyr::rename(!!output := n)
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
                     "plot" = rlang::quos(year, season, plot, species),
                     "treatment" = rlang::quos(year, season, treatment, species),
                     "site" = rlang::quos(year, season, species))

  level_data <- dplyr::group_by(plot_data, !!!grouping) %>%
    dplyr::summarise(n = sum(n, na.rm = TRUE),
                     quads = sum(nquads, na.rm = TRUE),
                     nplots = dplyr::n_distinct(plot))

  if (level == "plot")
  {
    level_data <- level_data %>%
      dplyr::mutate(n = replace(n, quads < min_quads, NA))
  }

  level_data %>%
    dplyr::rename(!!output := n) %>%
    dplyr::as.tbl()
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
    out_data <- dplyr::select(out_data, -nplots, -quads)
  } else if (level %in% c("plot","site")) {
    out_data <- dplyr::select(out_data, -nplots)
  }

  if (na_drop) {
    out_data <- na.omit(out_data)
  }

  if (shape == "crosstab") {
    out_data <- make_crosstab(out_data, output, NA)
  }

  if (zero_drop) {
    if (shape == "crosstab") {
      species <- as.character(unique(level_data$species))
      out_data <- out_data %>%
        dplyr::filter(rowSums(dplyr::select(., species)) != 0)
    } else { # shape == "flat"
      out_data <- out_data %>%
        dplyr::filter(output != 0)
    }
  }

  return(out_data)
}

#' @name get_plant_data
#' @aliases plant_abundance
#'
#' @title Generate summaries of Portal plant data
#'
#' @description This function is a generic interface into creating
#'   summaries of the Portal plant species data. It contains a number of
#'   arguments to specify both the kind of data to summarize, at what level of
#'   aggregation, various choices for dealing with data quality, and output
#'   format.
#'
#' @param path path to location of downloaded Portal data; or "repo" to
#'   retrieve data from github repo
#' @param level summarize by "Plot", "Treatment", or "Site"
#' @param type specify subset of species;
#'              If type=Annuals, removes all non-annual species.
#'              If type=Summer Annuals, returns all annual species that can be found in the summer
#'              If type=Winter Annuals, returns all annual species that can be found in the winter
#'              If type=Non-woody, removes shrub and subshrub species
#'              If type=Perennials, returns all perennial species (includes shrubs and subshrubs)
#'              If type=Shrubs, returns only shrubs and subshrubs
#' @param length specify subset of plots; use "All" plots or only "Longterm"
#'   plots (to be deprecated)
#' @param plots specify subset of plots; can be a vector of plots, or specific
#'   sets: "all" plots or "Longterm" plots (plots that have had the same
#'   treatment for the entire time series)
#' @param unknowns either removes all individuals not identified to species
#'   (unknowns = FALSE) or sums them in an additional column (unknowns = TRUE)
#' @param correct_sp correct species names suspected to be incorrect in early data (T/F)
#' @param shape return data as a "flat" list or "crosstab"
#' @param output specify whether to return "abundance", or "cover" [cover data
#'    starts in summer 2015]
#' @param na_drop logical, drop NA values (representing insufficient sampling)
#' @param zero_drop logical, drop 0s (representing sufficient sampling, but no
#'    detections)
#' @param min_quads numeric [1:16], minimum number of quadrats (out of 16) for a plot to be included
#' @param effort logical as to whether or not the effort columns should be
#'    included in the output
#' @inheritParams load_plant_data
#'
#' @return a data.frame in either "long" or "wide" format, depending on the
#'   value of `shape`
#'
#' @export
#'
get_plant_data <- function(path = '~', level = "Site", type = "All",
                           length = "all", plots = length, unknowns = FALSE,
                           correct_sp = TRUE,
                           shape = "flat", output = "abundance",
                           na_drop = switch(tolower(level),
                                            "plot" = FALSE,
                                            "treatment" = TRUE,
                                            "site" = TRUE),
                           zero_drop = switch(tolower(level),
                                              "plot" = FALSE,
                                              "treatment" = TRUE,
                                              "site" = TRUE),
                           min_quads = 1, effort = TRUE,
                           download_if_missing = TRUE)
{
  #### Clean inputs ----
  level <- tolower(level)
  type <- tolower(type)
  shape <- tolower(shape)
  output <- tolower(output)

  if (!missing("length"))
  {
    warning("The `length` argument is deprecated; please use `plots` instead.")
  }

  #### Get Data ----
  data_tables <- load_plant_data(path, download_if_missing = download_if_missing)

  #### Summarize data ----

  # make master census info table from census_table, date_table and plots_table
  census_info_table <- join_census_to_dates(data_tables$census_table,
                                            data_tables$date_table,
                                            data_tables$plots_table) %>%
                          filter_plots(plots = plots)

  #### Clean data and prepare output ----
  out_df <- clean_plant_data(data_tables, type,
                             unknowns, correct_sp) %>%
    make_plant_plot_data(census_info_table, output, min_quads) %>%
    make_plant_level_data(level, output, min_quads) %>%
    prep_plant_output(effort, na_drop, zero_drop, shape, level, output)

  return(out_df)
}


#' @rdname get_plant_data
#'
#' @description \code{plant_abundance} generates a table of plant abundance
#'
#' @param ... arguments passed to \code{\link{get_plant_data}}
#'
#' @examples
#' plant_abundance("repo")
#'
#' @export
#'
plant_abundance <- function(..., shape = "flat") {

  if(shape %in% c("Crosstab", "crosstab"))
  {
    get_plant_data(..., shape = "crosstab", output = "abundance")
  }
  else {
    get_plant_data(..., shape = "flat", output = "abundance") %>%
      dplyr::filter(abundance>0)
  }

}

#' @name shrub_cover
#'
#' @title Generate percent cover from Portal plant transect data
#'
#' @description This function calculates percent cover from transect data.
#' It handles the pre-2015 data differently from the current transects,
#' becase they are collected differently. But it returns a single time-series
#' with all years of transect data available. It also returns mean height
#' beginning in 2015.
#'
#' @param path path to location of downloaded Portal data; or "repo" to
#'   retrieve data from github repo
#' @param type specify subset of species;
#'              If type=Annuals, removes all non-annual species.
#'              If type=Summer Annuals, returns all annual species that can be found in the summer
#'              If type=Winter Annuals, returns all annual species that can be found in the winter
#'              If type=Non-woody, removes shrub and subshrub species
#'              If type=Perennials, returns all perennial species (includes shrubs and subshrubs)
#'              If type=Shrubs, returns only shrubs and subshrubs
#' @param plots specify subset of plots; can be a vector of plots, or specific
#'   sets: "all" plots or "Longterm" plots (plots that have had the same
#'   treatment for the entire time series)
#' @param unknowns either removes all individuals not identified to species
#'   (unknowns = FALSE) or sums them in an additional column (unknowns = TRUE)
#' @param correct_sp correct species names suspected to be incorrect in early data (T/F)
#'
#' @inheritParams load_plant_data
#'
#' @return a data.frame of percent cover and mean height
#'
#' @export
#'
shrub_cover <- function(path = '~', type = "Shrubs", plots = "all",
                        unknowns = FALSE, correct_sp = TRUE)
  {

  #### Clean inputs ----
  type <- tolower(type)

  #### Get Data ----
  data_tables <- load_plant_data(path)

  #### Do initial cleaning ----
  oldtransect_data = data_tables$oldtransect_data %>%
    dplyr::mutate("month" = 8) %>%
    dplyr::left_join(data_tables$species_table, by = "species") %>%
    dplyr::left_join(data_tables$plots_table, by = c("year","month","plot")) %>%
    rename_species_plants(correct_sp) %>%
    process_annuals(type) %>%
    process_unknownsp_plants(unknowns) %>%
    filter_plots(plots) %>%
    dplyr::mutate(treatment = as.character(treatment), species = as.factor(species)) %>%
    dplyr::group_by(year, treatment, plot, species) %>%
    dplyr::summarize(count=n()) %>%
    dplyr::mutate(cover = count/1000, height=NA, species = as.character(species)) %>%
    dplyr::select(-count)

  transect_data = data_tables$transect_data %>%
    dplyr::filter(!grepl(3,notes)) %>%
    dplyr::left_join(data_tables$species_table, by = "species") %>%
    dplyr::left_join(data_tables$plots_table, by = c("year","month","plot")) %>%
    rename_species_plants(correct_sp) %>%
    process_annuals(type) %>%
    process_unknownsp_plants(unknowns) %>%
    filter_plots(plots) %>%
    dplyr::mutate(stop = replace(stop, stop>7071.1, 7071.1)) %>%
    dplyr::mutate(treatment = as.character(treatment), species = as.factor(species), length=stop-start) %>%
    dplyr::group_by(year, treatment, plot, species) %>%
    dplyr::summarize(length=sum(length, na.rm=TRUE), height = mean(height, na.rm=TRUE)) %>%
    dplyr::mutate(cover = length/(2*7071.1), species = as.character(species)) %>%
    dplyr::select(year,treatment,plot,species,cover,height)

  return(dplyr::bind_rows(oldtransect_data,transect_data))

}
