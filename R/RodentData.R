#' @importFrom magrittr "%>%"

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
#' @param time return data using the complete "newmoon" numbers of the original "period" numbers
#' @param output specify whether to return "abundance", or "biomass", or "energy"
#' @param fillweight specify whether to fill in unknown weights with other records from that individual or species, where possible
#'
#' @export
#'
get_rodent_data <- function(path = '~', level = "Site", type = "Rodents",
                            length = "all", unknowns = FALSE, incomplete = FALSE,
                            shape = "crosstab", time = "period", output = "abundance",
                            fillweight = !(output %in% c("abundance", "Abundance")))
{
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
    {.} -> rodents                             # re-assign back into rodents

  #### Summarise data ----

  rodents$wgt <- as.numeric(rodents$wgt)
  if(output %in% c("Energy", "energy"))
  {
    rodents$wgt <- rodents$wgt ^ 0.75          # convert to energy
  }


  if(level %in% c("Treatment", "treatment")) # group by treatments
  {
    rodents = join_plots_to_rodents(rodents, plots)

    if(output %in% c("Biomass", "biomass", "Energy", "energy")) {
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::group_by(period, treatment, species) %>%
        dplyr::summarize(biomass = sum(wgt, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::select(period, treatment, species, biomass) %>%
        tidyr::complete(period, treatment, species, fill = list(biomass = 0))
    } else { # abundance by default
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::group_by(period, treatment) %>%
        dplyr::do(data.frame(x = table(.$species))) %>%
        dplyr::ungroup() %>%
        dplyr::select(period, treatment, species = x.Var1, abundance = x.Freq)
    }
  } else if(level %in% c("Plot", "plot")) {    # group by plots
    trapping = filter_plots(trapping, length)
    rodents = join_trapping_to_rodents(rodents, trapping, incomplete)
    # reduce size of trapping table

    if(output %in% c("Biomass", "biomass", "Energy", "energy")) {
      # which period x plot were not sampled?
      sampled_LUT <- rodents %>%
        dplyr::filter(sampled == 0) %>%
        dplyr::select(period, plot, sampled) %>%
        dplyr::distinct()

      # aggregate over period x plot; fill in 0s
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::count(period, plot, species, wt = wgt) %>%
        dplyr::select(period, plot, species, biomass = n) %>%
        tidyr::complete(period, plot, species, fill = list(biomass = 0)) %>%
        dplyr::filter(!is.na(species))

      # replace abundancs for non-sampled period x plot with NA
      out_df <- out_df %>%
        dplyr::left_join(sampled_LUT, by = c("period", "plot")) %>%
        dplyr::mutate(biomass = replace(biomass, sampled == 0, NA)) %>%
        dplyr::select(period, plot, species, biomass)

    } else { # abundance by default
      # which period x plot were not sampled?
      sampled_LUT <- rodents %>%
        dplyr::filter(sampled == 0) %>%
        dplyr::select(period, plot, sampled) %>%
        dplyr::distinct()

      # aggregate over period x plot; fill in 0s
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::count(period, plot, species) %>%
        dplyr::select(period, plot, species, abundance = n) %>%
        tidyr::complete(period, plot, species, fill = list(abundance = 0L)) %>%
        dplyr::filter(!is.na(species))

      # replace abundancs for non-sampled period x plot with NA
      out_df <- out_df %>%
        dplyr::left_join(sampled_LUT, by = c("period", "plot")) %>%
        dplyr::mutate(abundance = replace(abundance, sampled == 0, NA)) %>%
        dplyr::select(period, plot, species, abundance)
    }
  } else if(level %in% c("Site", "site")) {    # group by the whole site
    if(output %in% c("Biomass", "biomass", "Energy", "energy")) {
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::group_by(period, species) %>%
        dplyr::summarize(biomass = sum(wgt, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::select(period, species, biomass) %>%
        tidyr::complete(period, species, fill = list(biomass = 0))
    } else { # abundance by default
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::group_by(period) %>%
        dplyr::do(data.frame(x = table(.$species))) %>%
        dplyr::ungroup() %>%
        dplyr::select(period, species = x.Var1, abundance = x.Freq)
    }
  }

  #### Reshape data into crosstab ----
  if(shape %in% c("Crosstab", "crosstab"))
  {
    if (output %in% c('Biomass', 'biomass', 'Energy', 'energy')) {
      out_df = make_crosstab(out_df, "biomass")
    } else {
      out_df = make_crosstab(out_df, "abundance")
    }
  } else { # flat output
    if(output %in% c("Biomass", "biomass", "Energy", "energy")) {
      if(level %in% c('Plot', 'plot')) {
        out_df %>%
          tidyr::complete(species, period, plot, fill = list(biomass = 0)) %>%
          dplyr::filter(!(is.na(species))) %>%
          dplyr::left_join(trapping[, c('period', 'plot', 'sampled')], by = c('period', 'plot')) %>%
          {.} -> out_df
        out_df[ which(out_df$sampled == 0), (ncol(out_df) - 1)] <- NA
        out_df = out_df[, (1:ncol(out_df) - 1)]
      }
    } else { # output == "abundance"
      if(level %in% c('Site', 'site')) out_df = tidyr::complete(out_df, species, period, fill = list(abundance = 0))
      if(level %in% c('Treatment', 'treatment')) out_df = tidyr::complete(out_df, species, period, treatment, fill = list(abundance = 0))
    }

    #### correct column name for "biomass" -> "energy" ----
    if(output %in% c("Energy", "energy"))
    {
      out_df <- dplyr::rename(out_df, energy = biomass) # rename output column
    }
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

