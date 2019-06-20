#' @name summarize_rodent_data
#' @aliases abundance biomass energy
#'
#' @title Generate summaries of Portal rodent data
#'
#' @description This function is a generic interface into creating summaries
#'   of the Portal rodent species data. It contains a number of arguments
#'   to specify the kind of data to summarize (at what level of aggregation)
#'   and various choices for dealing with data quality, and output format.
#'
#' @param level summarize by "Plot", "Treatment", or "Site"
#' @param type specify subset of species; either all "Rodents" or only
#'   "Granivores"
#' @param length specify subset of plots; use "All" plots or only "Longterm"
#'   plots (to be deprecated)
#' @param plots specify subset of plots; can be a vector of plots, or specific
#'   sets: "all" plots or "Longterm" plots (plots that have had the same
#'   treatment for the entire time series)
#' @param unknowns either removes all individuals not identified to species
#'   (unknowns = FALSE) or sums them in an additional column (unknowns = TRUE)
#' @param shape return data as a "crosstab" or "flat" list
#' @param time specify the format of the time index in the output, either
#'   "period" (sequential Portal surveys), "newmoon" (lunar cycle numbering),
#'   "date" (calendar date)
#' @param output specify whether to return "abundance", or "biomass", or
#'   "energy"
#' @param fillweight specify whether to fill in unknown weights with other
#'   records from that individual or species, where possible
#' @param na_drop logical, drop NA values (representing insufficient sampling)
#' @param zero_drop logical, drop 0s (representing sufficient sampling, but no
#'   detections)
#' @param min_traps minimum number of traps for a plot to be included
#' @param min_plots minimum number of plots within a period for an
#'   observation to be included
#' @param effort logical as to whether or not the effort columns should be
#'   included in the output
#' @param quiet logical, whether to run without producing messages
#' @inheritParams load_rodent_data
#'
#' @return a data.frame in either "long" or "wide" format, depending on the
#'   value of `shape`
#'
#' @export
#'
summarize_rodent_data <- function(path = get_default_data_path(),
                                  clean = TRUE, level = "Site",
                                  type = "Rodents", length = "all", plots = length,
                                  unknowns = FALSE, shape = "crosstab",
                                  time = "period", output = "abundance",
                                  fillweight = (output != "abundance"),
                                  na_drop = TRUE,
                                  zero_drop = switch(tolower(level),
                                                     "plot" = FALSE,
                                                     "treatment" = TRUE,
                                                     "site" = TRUE),
                                  min_traps = 1, min_plots = 24, effort = FALSE,
                                  download_if_missing = TRUE, quiet = FALSE)
{
  data_tables <- load_rodent_data(path, download_if_missing = download_if_missing,
                                  clean = clean, quiet = quiet)

  level <- tolower(level)
  type <- tolower(type)
  shape <- tolower(shape)
  time <- tolower(time)
  output <- tolower(output)

  if (!missing("length"))
  {
    warning("The `length` argument is deprecated; please use `plots` instead.")
  }

  rodent_data <- data_tables$rodent_data
  species_table <- data_tables$species_table
  plots_table <- data_tables$plots_table
  trapping_table <- data_tables$trapping_table
  newmoons_table <- data_tables$newmoons_table

  trapping_data <- filter_plots(trapping_table, plots) %>%
    join_plots(plots_table)

  out <- clean_rodent_data(rodent_data, species_table, fillweight, type, unknowns) %>%
    make_plot_data(trapping_data, output, min_traps) %>%
    make_level_data(trapping_table, level, output, min_plots, min_traps) %>%
    add_time(newmoons_table, time) %>%
    prep_rodent_output(effort, na_drop, zero_drop, shape, level, output)

  return(out)
}


#' @rdname summarize_rodent_data
#'
#' @description \code{abundance} generates a table of rodent abundance
#'
#' @param ... arguments passed to \code{\link{summarize_rodent_data}}
#'
#' @examples
#' \donttest{
#' abundance("repo")
#' }
#' @export
#'
abundance <- function(...) {
  summarize_rodent_data(..., output = "abundance")
}

#' @rdname summarize_rodent_data
#'
#' @description * \code{biomass()} generates a table of rodent biomass
#'
#' @inheritParams abundance
#'
#' @examples
#' \donttest{
#' biomass("repo")
#' }
#' @export
#'
biomass <- function(...) {
  summarize_rodent_data(..., output = "biomass")
}


#' @rdname summarize_rodent_data
#'
#' @description * \code{energy()} generates a table of rodent energy
#'   (computed as 5.69 * (biomass ^ 0.75) after White et al 2004)
#'
#' @inheritParams abundance
#'
#' @examples
#' \donttest{
#' energy("repo")
#' }
#' @export
#'
energy <- function(...) {
  summarize_rodent_data(..., output = "energy")
}

#' @rdname summarize_rodent_data
#' @export
summarise_rodent_data <- summarize_rodent_data

