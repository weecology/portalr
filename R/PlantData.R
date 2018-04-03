#' @importFrom magrittr "%>%"
#' @importFrom rlang "!!"
#' @importFrom rlang "!!!"
#' @importFrom rlang ":="
#' @importFrom rlang quo
#' @importFrom rlang quos

#' @name get_plant_data

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
#' @param type specify subset of species; either "All" - includes annuals,
#'   perennials, and shrubs; "Annuals" - only annuals; or "Non-woody" - includes
#'   annuals and perennials, not shrubs or subshrubs
#' @param length specify subset of plots; use "All" plots or only "Longterm"
#'   plots (plots that have had same treatment for entire time series)
#' @param unknowns either removes all individuals not identified to species
#'   (unknowns = FALSE) or sums them in an additional column (unknowns = TRUE)
#' @param correct_sp correct species names suspected to be incorrect in early data (T/F)
#' @param shape return data as a "flat" list or "crosstab"
#' @param output specify whether to return "abundance", or "cover" [cover data
#'    starts in summer 2015]
#'
#' @return a data.frame in either "long" or "wide" format, depending on the
#'   value of `shape`
#'
#' @export
#'
get_plant_data <- function(path = '~', level = "Site", type = "All",
                           length = "all", unknowns = FALSE, correct_sp = TRUE,
                           shape = "flat", output = "abundance")
{
  #### Clean inputs ----
  output <- tolower(output)
  level <- tolower(level)

  #### Get Data ----
  data_tables <- load_plant_data(path)

  #### Do initial cleaning ----
  quadrats <- clean_plant_data(data_tables, type,
                               unknowns, correct_sp, length)

  #### Summarize data ----

  # make master census info table from census_table, date_table and plots_table [filters out un-censused quadrats]
  census_info_table <- join_census_to_dates(data_tables$census_table,
                                            data_tables$date_table,
                                            data_tables$plots_table) %>%
                          filter_plots(length = length) %>%
                          dplyr::filter(censused == 1)

  # join census info to quadrat data
  quadrats <- join_census_to_quadrats(quadrats, census_info_table)

  ## summarize over each plot
  if(level == "plot") {
    grouping <- quos(year, season, plot, species)
  } else if(level == "treatment") {
    grouping <- quos(year, season, treatment, species)
  } else { # level == "site"
    grouping <- quos(year, season, species)
  }

  ## [4] summarize
  out_df <- quadrats %>%
    dplyr::count(!!!grouping, wt = abundance) %>%
    dplyr::select(!!!grouping, n)

  ## [5] rename output variable correctly
  out_df <- dplyr::rename(out_df, !!output := n)

  ## [6] post-process
  # add column for number of quadrats that went into summary
  nquads <- census_info_table %>%
              dplyr::count(!!!head(grouping,-1),wt = censused)
  nquads <- dplyr::rename(nquads, "nquads"="n")
  out_df <- dplyr::left_join(out_df,nquads)


  #### Reshape data into crosstab ----
  if(shape %in% c("Crosstab", "crosstab"))
  {
    crosstab_fill <- if(output == "abundance") 0L else NA
    out_df <- make_crosstab(out_df, output, crosstab_fill)
  }

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



