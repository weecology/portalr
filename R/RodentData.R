#' @importFrom magrittr "%>%"
#' @importFrom rlang "!!"
#' @importFrom rlang "!!!"
#' @importFrom rlang ":="

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
#'   [note that if level="plot" and incomplete=T, NAs will be included in periods where trapping was incomplete]
#' @param shape return data as a "crosstab" or "flat" list
#' @param time specify the format of the time index in the output, either
#'   "period" (sequential Portal surveys), "newmoon" (lunar cycle numbering),
#'   "date" (calendar date)
#' @param output specify whether to return "abundance", or "biomass", or "energy"
#' @param fillweight specify whether to fill in unknown weights with other records from that individual or species, where possible
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
  data_tables = loadData(path)
  rodents = data_tables[[1]]
  species = data_tables[[2]]
  trapping = data_tables[[3]]
  newmoons = data_tables[[4]]
  plots = data_tables[[5]]

  #### Do initial cleaning ----
  rodents %>%
    fill_weight(fillweight, species) %>%       # add in missing biomass info
    remove_suspect_entries() %>%               # remove various "weird" data
    process_unknownsp(species, unknowns) %>%   # keep non-rodent, un-identified?
    process_granivores(type) %>%               # exclude granivores?
    remove_incomplete_censuses(trapping, incomplete) %>% # incomplete trapping sessions
    filter_plots(length) %>%                   # keep only the long-term treatments
    dplyr::mutate(species = factor(species)) %>%         # convert species to factor
    {.} -> rodents                             # re-assign back into rodents

  #### Summarise data ----

  rodents$wgt <- as.numeric(rodents$wgt)
  if(output == "energy")
  {
    rodents$wgt <- rodents$wgt ^ 0.75          # convert to energy
  }

  ## select what to summarize, depending on output
  ##   if output == abundance then NULL     --> count entries
  ##                          else quo(wgt) --> sum up `wgt` column
  wt <- if(output == "abundance") NULL else rlang::quo(wgt)

  ## summarize over each plot
  if(level == "plot") {
    trapping = filter_plots(trapping, length)
    rodents = join_trapping_to_rodents(rodents, trapping, incomplete)

    # which period x plot were not sampled?
    sampled_LUT <- rodents %>%
      dplyr::filter(sampled == 0) %>%
      dplyr::select(period, plot, sampled) %>%
      dplyr::distinct()

    # aggregate over period x plot; fill in 0s
    out_df <- rodents %>%
      dplyr::count(period, plot, species, wt = !!wt) %>%
      dplyr::select(period, plot, species, n)

    # replace values for non-sampled period x plot with NA
    out_df <- out_df %>%
      tidyr::complete(period, plot, species, fill = list(n = 0L)) %>%
      dplyr::filter(!is.na(species)) %>%
      dplyr::left_join(sampled_LUT, by = c("period", "plot")) %>%
      dplyr::mutate(n = replace(n, sampled == 0, NA)) %>%
      dplyr::select(period, plot, species, n) %>%
      dplyr::rename(!!output := n)

    ## summarize over the whole site or over each treatment
  } else {
    if(level == "treatment") {
      rodents = join_plots_to_rodents(rodents, plots)
      grouping <- rlang::quos(period, treatment, species)
    } else { # level == "site"
      grouping <- rlang::quos(period, species)
    }
    out_df <- rodents %>%
      dplyr::count(!!!grouping, wt = !!wt) %>%
      dplyr::select(!!!grouping, n) %>%
      dplyr::rename(!!output := n)
  }

  #### Reshape data into crosstab ----
  if(shape %in% c("Crosstab", "crosstab"))
  {
    crosstab_fill <- if(output == "abundance") 0L else NA
    out_df <- make_crosstab(out_df, output, crosstab_fill)
  }

  #### use new moon number as time index if time == "newmoon" ----
  out_df = add_time(out_df, newmoons, time)

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

