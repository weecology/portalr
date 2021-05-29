#' @name summarize_rodent_data
#' @aliases abundance biomass energy rates
#'
#' @title Generate summaries of Portal rodent data
#'
#' @description Generic interface into creating summaries of the Portal 
#'              rodent species data. It contains a number of arguments
#'              to specify the kind of data to summarize (at what level of 
#'              aggregation) and various choices for dealing with data 
#'              quality and output format. 
#'              \cr \cr
#'              \code{summarise_rodent_output} provides alternative spelling. 
#'              \cr \cr
#'              Functions for specific \code{output}: 
#'              \cr \cr
#'              \code{abundance} generates a table of rodent abundance.
#'              \cr \cr
#'              \code{biomass} generates a table of rodent biomass.
#'              \cr \cr
#'              \code{energy} generates a table of rodent energy
#'              (computed as 5.69 * (biomass ^ 0.75) after White et al. 2004).
#'              \cr \cr
#'              \code{rates} generates a table of rodent growth rates
#'              (computed as r = log(N[t+1]/N[t]).
#'
#' @param level summarize by "Plot", "Treatment", or "Site"
#'
#' @param type specify subset of species; either all "Rodents" or only
#'             "Granivores"
#'
#' @param length specify subset of plots; use "All" plots or only "Longterm"
#'               plots (to be deprecated)
#'
#' @param plots specify subset of plots; can be a vector of plots, or specific
#'              sets: "all" plots or "Longterm" plots (plots that have had 
#'              the same treatment for the entire time series)
#'
#' @param unknowns either removes all individuals not identified to species
#'                 (unknowns = FALSE) or sums them in an additional column 
#'                 (unknowns = TRUE)
#'
#' @param shape return data as a "crosstab" or "flat" list
#'
#' @param time specify the format of the time index in the output, either
#'             "period" (sequential Portal surveys), "newmoon" (lunar cycle 
#'             numbering), "date" (calendar date), or "all" (for all time 
#'             indices)
#'
#' @param output specify whether to return "abundance", or "biomass",
#'               "energy", or "rates"
#'
#' @param fillweight specify whether to fill in unknown weights with other
#'                   records from that individual or species, where possible
#'
#' @param na_drop logical, drop NA values (representing insufficient sampling)
#'                filling missing combinations of 
#'                year-month-treatment/plot-species with NA could
#'                represent one of a few slightly different meanings:
#'                \itemize{
#'                  \item that combo doesn't exist
#'                  \item that combo was skipped that month, or
#'                  \item that combo was trapped, but is unusable 
#'                        (a negative period code))
#'                }
#'
#' @param zero_drop logical, drop 0s (representing sufficient sampling, but 
#'                  no detections)
#'
#' @param min_traps minimum number of traps for a plot to be included
#'
#' @param min_plots minimum number of plots within a period for an
#'                  observation to be included
#'
#' @param effort logical as to whether or not the effort columns should be
#'               included in the output
#'
#' @param quiet logical, whether to run without producing messages
#'
#' @param include_unsampled logical, overrides settings for \code{na_drop} and
#'                          \code{zero_drop}, setting both to FALSE
#'
#' @param ... arguments passed to \code{\link{summarize_rodent_data}}
#'
#' @inheritParams load_rodent_data
#'
#' @return a data.frame in either "long" or "wide" format, depending on the
#'         value of \code{shape}.
#'
#' @references
#'  White, E. P., S. K. M. Ernest, and K. M. Thibault. 2004. Trade-offs in 
#'    community properties through time in a desert rodent community. 
#'    The American Naturalist 164:670-676. https://doi.org/10.1086/424766.
#'
#' @examples
#'  \donttest{
#'    abundance("repo")
#'    biomass("repo")
#'    energy("repo")
#'    rates("repo")
#'  }
#'
#' @export
#'
summarize_rodent_data <- function(path = get_default_data_path(),
                                  clean = TRUE, 
                                  level = "Site",
                                  type = "Rodents", 
                                  length = "all", 
                                  plots = length,
                                  unknowns = FALSE, 
                                  shape = "crosstab", 
                                  time = "period", 
                                  output = "abundance", 
                                  fillweight = (output != "abundance"), 
                                  na_drop = TRUE, 
                                  zero_drop = switch(tolower(level),
                                                     "plot" = FALSE,
                                                     "treatment" = TRUE,
                                                     "site" = TRUE),
                                  min_traps = 1,
                                  min_plots = 24, 
                                  effort = FALSE,
                                  download_if_missing = TRUE,
                                  quiet = FALSE,
                                  include_unsampled = FALSE) {

  if (include_unsampled) {
  
    na_drop <- FALSE
    zero_drop <- FALSE

  }

  data_tables <- load_rodent_data(path, 
                                  download_if_missing = download_if_missing,
                                  clean = clean, quiet = quiet)

  level <- tolower(level)
  type <- tolower(type)
  shape <- tolower(shape)
  time <- tolower(time)
  output <- tolower(output)

  if (!missing("length")) {
  
    warning("`length` is deprecated; use `plots` to specify subsets")

  }

  rodent_data <- data_tables$rodent_data
  species_table <- data_tables$species_table
  plots_table <- data_tables$plots_table
  trapping_table <- data_tables$trapping_table
  newmoons_table <- data_tables$newmoons_table

  trapping_data <- filter_plots(trapping_table, plots) %>%
    join_plots(plots_table)

  out <- clean_rodent_data(rodent_data, species_table, fillweight, type, 
                           unknowns) %>%
    make_plot_data(trapping_data, output, min_traps) %>%
    make_level_data(trapping_table, level, output, min_plots, min_traps) %>%
    add_time(newmoons_table, time) %>%
    prep_rodent_output(effort, na_drop, zero_drop, shape, level, output)

  return(out)
}


#' @rdname summarize_rodent_data
#'
#' @export
#'
summarise_rodent_data <- function(...) {
  summarize_rodent_data(...)
}

#' @rdname summarize_rodent_data
#'
#' @export
#'
abundance <- function(...) {
  summarize_rodent_data(..., output = "abundance")
}

#' @rdname summarize_rodent_data
#'
#' @export
#'
biomass <- function(...) {
  summarize_rodent_data(..., output = "biomass")
}

#' @rdname summarize_rodent_data
#'#' @export
#'
energy <- function(...) {
  summarize_rodent_data(..., output = "energy")
}

#' @rdname summarize_rodent_data
#'
#' @export
#'
rates <- function(...) {
  summarize_rodent_data(..., output = "rates")
}

