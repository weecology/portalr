#' @name summarize_plant_data
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
#' @param type specify subset of species;
#'              If type=Annuals, removes all non-annual species.
#'              If type=Summer Annuals, returns all annual species that can be found in the summer
#'              If type=Winter Annuals, returns all annual species that can be found in the winter
#'              If type=Non-woody, removes shrub and subshrub species
#'              If type=Perennials, returns all perennial species (includes shrubs and subshrubs)
#'              If type=Shrubs, returns only shrubs and subshrubs
#' @param correct_sp correct species names suspected to be incorrect in early data (T/F)
#' @param output specify whether to return "abundance", or "cover" [cover data
#'    starts in summer 2015]
#' @param min_quads numeric [1:16], minimum number of quadrats (out of 16) for a plot to be included
#' @inheritParams load_plant_data
#' @inheritParams summarize_rodent_data
#'
#' @return a data.frame in either "long" or "wide" format, depending on the
#'   value of `shape`
#'
#' @export
#'
summarize_plant_data <- function(path = get_default_data_path(),
                                 level = "Site", type = "All",
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
                                 download_if_missing = TRUE,
                                 quiet = FALSE)
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
  data_tables <- load_plant_data(path, download_if_missing = download_if_missing,
                                 quiet = quiet)

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

#' @rdname summarize_plant_data
#'
#' @description \code{plant_abundance} generates a table of plant abundance
#'
#' @param ... arguments passed to \code{\link{summarize_plant_data}}
#'
#' @examples
#' \donttest{
#' plant_abundance("repo")
#' }
#' @export
#'
plant_abundance <- function(..., shape = "flat") {

  if (tolower(shape) == "crosstab")
  {
    summarize_plant_data(..., shape = "crosstab", output = "abundance")
  }
  else {
    summarize_plant_data(..., shape = "flat", output = "abundance") %>%
      dplyr::filter(abundance > 0)
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
#' @inheritParams summarize_plant_data
#'
#' @return a data.frame of percent cover and mean height
#'
#' @export
#'
shrub_cover <- function(path = get_default_data_path(),
                        type = "Shrubs", plots = "all",
                        unknowns = FALSE, correct_sp = TRUE,
                        download_if_missing = TRUE,
                        quiet = FALSE)
{

  #### Clean inputs ----
  type <- tolower(type)

  #### Get Data ----
  data_tables <- load_plant_data(path,
                                 download_if_missing = download_if_missing,
                                 quiet = quiet)

  clean_transect_data <- function(df)
  {
    df %>%
      dplyr::left_join(data_tables$species_table, by = "species") %>%
      dplyr::left_join(data_tables$plots_table, by = c("year", "month", "plot")) %>%
      rename_species_plants(correct_sp) %>%
      process_annuals(type) %>%
      process_unknownsp_plants(unknowns) %>%
      filter_plots(plots) %>%
      dplyr::mutate(treatment = as.character(treatment), species = as.factor(species)) %>%
      dplyr::group_by(year, treatment, plot, species)
  }

  #### Do initial cleaning ----
  oldtransect_data = data_tables$oldtransect_data %>%
    dplyr::mutate("month" = 8) %>%
    clean_transect_data() %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cover = count/1000, height = NA, species = as.character(species)) %>%
    dplyr::select(-count)

  transect_data = data_tables$transect_data %>%
    dplyr::filter(!grepl(3,notes)) %>%
    clean_transect_data() %>%
    dplyr::mutate(stop = replace(stop, stop > 7071.1, 7071.1),
                  length = stop - start) %>%
    dplyr::summarize(length=sum(length, na.rm=TRUE), height = mean(height, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cover = length/(2*7071.1), species = as.character(species)) %>%
    dplyr::select(year, treatment, plot, species, cover, height)

  dplyr::bind_rows(oldtransect_data, transect_data)
}

#' @rdname summarize_plant_data
#' @export
summarise_plant_data <- summarize_plant_data
