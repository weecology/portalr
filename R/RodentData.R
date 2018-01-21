#' @importFrom magrittr "%>%"
#' @importFrom rlang "!!"
#' @importFrom rlang "!!!"
#' @importFrom rlang ":="
#' @importFrom rlang quo
#' @importFrom rlang quos

#' @name get_rodent_data
#' @aliases abundance biomass energy
#'
#' @title Generate summaries of Portal rodent data
#'
#' @description This function is a generic interface into creating (monthly)
#'   summaries of the Portal rodent species data. It contains a number of
#'   arguments to specify both the kind of data to summarize, at what level of
#'   aggregation, various choices for dealing with data quality, and output
#'   format.
#'
#' @param path path to location of downloaded Portal data; or "repo" to
#'   retrieve data from github repo
#' @param level summarize by "Plot", "Treatment", or "Site"
#' @param type specify subset of species; either all "Rodents" or only
#'   "Granivores"
#' @param length specify subset of plots; use "All" plots or only "Longterm"
#'   plots (plots that have had same treatment for entire time series)
#' @param unknowns either removes all individuals not identified to species
#'   (unknowns = FALSE) or sums them in an additional column (unknowns = TRUE)
#' @param incomplete either removes all data from incomplete trapping sessions
#'   (incomplete = FALSE) or includes them (incomplete = TRUE)
#'   [note that if level="plot" and incomplete=T, NAs will be included in
#'    periods where trapping was incomplete]
#' @param shape return data as a "crosstab" or "flat" list
#' @param time specify the format of the time index in the output, either
#'   "period" (sequential Portal surveys), "newmoon" (lunar cycle numbering),
#'   "date" (calendar date)
#' @param output specify whether to return "abundance", or "biomass", or "energy"
#' @param fillweight specify whether to fill in unknown weights with other
#'   records from that individual or species, where possible
#'
#' @return a data.frame in either "long" or "wide" format, depending on the
#'   value of `shape`
#'
#' @export
#'
get_rodent_data <- function(path = '~', level = "Site", type = "Rodents",
                            length = "all", unknowns = FALSE, incomplete = FALSE,
                            shape = "crosstab", time = "period", output = "abundance",
                            fillweight = (output != "abundance"))
{
  #### Clean inputs ----
  output <- tolower(output)
  level <- tolower(level)

  #### Get Data ----
  data_tables <- loadData(path)

  #### Do initial cleaning ----
  rodents <- clean_rodent_data(data_tables, fillweight, type,
                               unknowns, incomplete, length)

  #### Summarize data ----

  ## [1] if output == "energy", convert weight to energy
  # if(output == "energy")
  # {
  #   rodents$wgt <- rodents$wgt ^ 0.75          # convert to energy
  # }

  ## [2] select what to summarize, depending on output
  ##     if output == abundance then NULL     --> count entries
  ##                            else quo(wgt) --> sum up `wgt` column
  wt <- switch(output,
               "abundance" = NULL,
               "biomass" = quo(wgt),
               "energy" = quo(energy))

  ## [3] determine grouping variables
  ## summarize over each plot
  if(level == "plot") {
    trapping <- filter_plots(data_tables$trapping, length)
    rodents <- join_trapping_to_rodents(rodents, trapping, incomplete)
    grouping <- quos(period, plot, species)

    # remember which (period x plot) were not sampled
    sampled_LUT <- rodents %>%
      dplyr::filter(sampled == 0) %>%
      dplyr::select(period, plot, sampled) %>%
      dplyr::distinct()
  } else if(level == "treatment") {
    rodents = join_plots_to_rodents(rodents, data_tables$plots_table)
    grouping <- quos(period, treatment, species)
  } else { # level == "site"
    grouping <- quos(period, species)
  }

  ## [4] summarize
  out_df <- rodents %>%
    dplyr::count(!!!grouping, wt = !!wt) %>%
    dplyr::select(!!!grouping, n)

  ## [5] post-process
  if(level == "plot") {
    # replace values for non-sampled period x plot with NA
    out_df <- out_df %>%
      tidyr::complete(period, plot, species, fill = list(n = 0L)) %>%
      dplyr::filter(!is.na(species)) %>%
      dplyr::left_join(sampled_LUT, by = c("period", "plot")) %>%
      dplyr::mutate(n = replace(n, sampled == 0, NA)) %>%
      dplyr::select(period, plot, species, n)
  }

  ## [6] rename output variable correctly
  out_df <- dplyr::rename(out_df, !!output := n)

  #### Reshape data into crosstab ----
  if(shape %in% c("Crosstab", "crosstab"))
  {
    crosstab_fill <- if(output == "abundance") 0L else NA
    out_df <- make_crosstab(out_df, output, crosstab_fill)
  }

  #### use new moon number as time index if time == "newmoon" ----
  out_df = add_time(out_df, data_tables$newmoons, time)

  return(out_df)
}


#' @rdname get_rodent_data
#'
#' @description \code{abundance} generates a table of rodent abundance
#'
#' @param ... arguments passed to \code{\link{get_rodent_data}}
#'
#' @examples
#' abundance("repo")
#'
#' @export
#'
abundance <- function(...) {
  get_rodent_data(..., output = "abundance")
}

#' @rdname get_rodent_data
#'
#' @description * \code{biomass()} generates a table of rodent biomass
#'
#' @inheritParams abundance
#'
#' @examples
#' biomass("repo")
#'
#' @export
#'
biomass <- function(...) {
  get_rodent_data(..., output = "biomass")
}


#' @rdname get_rodent_data
#'
#' @description * \code{energy()} generates a table of rodent energy (computed as biomass ^ 0.75)
#'
#' @inheritParams abundance
#'
#' @examples
#' energy("repo")
#'
#' @export
#'
energy <- function(...) {
  get_rodent_data(..., output = "energy")
}

