#' Length of non-missing values in a vector
#'
#' @param x vector of values
#' @return integer of how many non-NA values in x
#'
#' @export
#'
true_length <- function(x) {
 length(which(!is.na(x)))
}

#' Plot-level rodent data
#'
#' @param rodent_data cleaned rodent data
#' @param trapping_data trapping table with treatment column
#' @param output specify whether to return "abundance", or "biomass",
#'   or "energy"
#' @param min_traps minimum number of traps for a plot to be included
#'
#' @return fully crossed period x plot x species flat table of observations
#'   with effort (number of traps) and treatment columns. Any plots not
#'   sufficiently (as defined by min_traps) sampled is returned with NA
#'   for effort and the output value of interest
#'
#' @export
#'
make_plot_data <- function(rodent_data, trapping_data, output, min_traps = 1) {

  grouping <- rlang::quos(period, plot, species)
  wt <- switch(output,
          "abundance" = NULL,
          "biomass" = rlang::quo(wgt),
          "energy" = rlang::quo(energy))
  filler <- list(n = as.integer(0))
  plot_data <- rodent_data %>%
               dplyr::count(!!!grouping, wt = !!wt)  %>%
               tidyr::complete(!!!grouping, fill = filler) %>%
               dplyr::right_join(trapping_data, by = c("period", "plot")) %>%
               dplyr::select(period, plot, species, n, effort, treatment)
  naspecies <- which(is.na(plot_data$species))
  if (length(naspecies) > 0){
    plot_data <- plot_data[-naspecies, ]
  }
  insuff_plot <- which(plot_data$effort < min_traps)
  plot_data[insuff_plot, c("n", "effort")] <- c(NA, NA)

  plot_data <- dplyr::rename(plot_data, !!output := n)
  return(plot_data)
}

#' Rodent data summarized at the relevant level (plot, treatment, site)
#'
#' @param plot_data rodent data summarized at the plot level
#' @param level specify level of interest ("plot", "treatment", "site")
#' @param output specify whether to return "abundance", or "biomass",
#'   or "energy"
#' @param min_plots minimum number of plots within a period for an
#'   observation to be included
#'
#' @return fully crossed flat table of observations with effort (number of
#'   traps and number of plots). The crossing depends on the level:
#'   "plot" is period x treatment x plot x species, "treatment" is
#'   period x treatment x species, and "site" is period x species. Any
#'   observations not sufficiently (as defined by min_plots, and
#'   hierarchically by min_traps) sampled are returned with NA
#'   for ntraps, nplots, and the output value of interest
#'
#' @export
#'
make_level_data <- function(plot_data, level, output, min_plots){

  plot_data <- dplyr::rename(plot_data, n := !!output)
  grouping <- switch(level,
                "plot" = rlang::quos(period, treatment, plot, species),
                "treatment" = rlang::quos(period, treatment, species),
                "site" = rlang::quos(period, species))

  level_data <- dplyr::group_by(plot_data, !!!grouping) %>%
                dplyr::summarise(n = sum(n, na.rm = TRUE),
                 ntraps = sum(effort, na.rm = TRUE),
                 nplots = portalr::true_length(effort))

  if (length(min_plots) > 0){
    insuff_level <- which(level_data$nplots < min_plots)
    level_data[insuff_level, c("n", "ntraps", "nplots")] <- c(NA, NA, NA)
  }

  level_data <- dplyr::rename(level_data, !!output := n) %>%
                data.frame() %>%
                dplyr::as.tbl()

  return(level_data)
}

#' Rodent data prepared for output
#'
#' @param level_data rodent data summarized at the level of interest
#' @param data_tables read-in Portal data, passed through for moon data
#' @param time specify the format of the time index in the output, either
#'   "period" (sequential Portal surveys), "newmoon" (lunar cycle numbering),
#'   or "date" (calendar date)
#' @param effort logical as to whether or not the effort columns should be
#'   included in the output
#' @param na_drop logical, drop NA values (representing insufficient sampling)
#' @param zero_drop logica, drop 0s (representing sufficient sampling, but no
#'   detections)
#' @param shape return data as a "crosstab" or "flat" list
#' @param level specify level of interest ("plot", "treatment", "site")
#' @param output specify whether to return "abundance", or "biomass",
#'   or "energy"
#' @return fully crossed flat table of observations with effort (number of
#'   traps and number of plots). The crossing depends on the level:
#'   "plot" is period x treatment x plot x species, "treatment" is
#'   period x treatment x species, and "site" is period x species. Any
#'   observations not sufficiently (as defined by min_plots, and
#'   hierarchically by min_traps) sampled are returned with NA
#'   for ntraps, nplots, and the output value of interest
#'
#' @export
#'
prep_rodent_output <- function(level_data, data_tables, time, effort, na_drop,
                               zero_drop, shape, level, output){

  out_data <- portalr::add_time(level_data, data_tables$newmoons_table, time)
  if (effort == FALSE){
    out_data <- dplyr::select(out_data, -nplots, -ntraps)
  } else if (level == "plot"){
    out_data <- dplyr::select(out_data, -nplots)
  }
  if (length(na_drop) == 0){
    na_drop <- switch(level,
                   "plot" = FALSE,
                   "treatment" = TRUE,
                   "site" = TRUE)
  }
  if (na_drop == TRUE){
    out_data <- na.omit(out_data)
  }
  if (shape == "crosstab"){
    out_data <- portalr::make_crosstab(out_data, output, NA)
  }
  if (length(zero_drop) == 0) {
    zero_drop <- switch(level,
                   "plot" = FALSE,
                   "treatment" = TRUE,
                   "site" = TRUE)
  }
  if (zero_drop == TRUE) {
    if (shape == "flat") {
      values <- out_data[, output]
      zeroes <- which(values == 0)
    } else if (shape == "crosstab") {
        species <- as.character(unique(level_data$species))
        values <- out_data[, which(colnames(out_data) %in% species)]
        value_totals <- apply(values, 1, sum)
        zeroes <- which(value_totals == 0)
    }
    if (length(zeroes) > 0) {
      out_data <- out_data[-zeroes, ]
    }
  }
  return(out_data)
}


#' @name get_rodent_data
#' @aliases abundance biomass energy
#'
#' @title Generate summaries of Portal rodent data
#'
#' @description This function is a generic interface into creating summaries
#'   of the Portal rodent species data. It contains a number of arguments
#'   to specify the kind of data to summarize (at what level of aggregation)
#'   and various choices for dealing with data quality, and output format.
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
#' @param shape return data as a "crosstab" or "flat" list
#' @param time specify the format of the time index in the output, either
#'   "period" (sequential Portal surveys), "newmoon" (lunar cycle numbering),
#'   "date" (calendar date)
#' @param output specify whether to return "abundance", or "biomass", or
#'   "energy"
#' @param fillweight specify whether to fill in unknown weights with other
#'   records from that individual or species, where possible
#' @param na_drop logical, drop NA values (representing insufficient sampling)
#' @param zero_drop logica, drop 0s (representing sufficient sampling, but no
#'   detections)
#' @param min_traps minimum number of traps for a plot to be included
#' @param min_plots minimum number of plots within a period for an
#'   observation to be included
#' @param effort logical as to whether or not the effort columns should be
#'   included in the output
#'
#' @return a data.frame in either "long" or "wide" format, depending on the
#'   value of `shape`
#'
#' @export
#'
get_rodent_data <- function(path = "~", level = "Site", type = "Rodents",
                            length = "all", unknowns = FALSE,
                            incomplete = FALSE, shape = "crosstab",
                            time = "period", output = "abundance",
                            fillweight = (output != "abundance"),
                            na_drop = NULL, zero_drop = NULL,
                            min_traps = 1, min_plots = 1, effort = FALSE){

  data_tables <- portalr::load_data(path)

  level <- tolower(level)
  type <- tolower(type)
  length <- tolower(length)
  shape <- tolower(shape)
  time <- tolower(time)
  output <- tolower(output)

  trapping_data <- portalr::filter_plots(data_tables$trapping, length) %>%
    portalr::join_plots_to_trapping(data_tables$plots_table)

  out <- portalr::clean_rodent_data(data_tables, fillweight, type,
                                    unknowns, incomplete, length) %>%
    portalr::make_plot_data(trapping_data, output, min_traps) %>%
    portalr::make_level_data(level, output, min_plots) %>%
    portalr::prep_rodent_output(data_tables, time, effort, na_drop,
                                zero_drop, shape, level, output)

  return(out)
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
#' @description * \code{energy()} generates a table of rodent energy
#'   (computed as biomass ^ 0.75)
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

