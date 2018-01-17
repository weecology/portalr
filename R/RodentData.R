#' @importFrom magrittr "%>%"

#' @name get_rodent_data
#' @aliases abundance biomass
#'
#' @title Retrieve Rodent Data
#'
#' @description A function to summarize monthly Portal rodent species data.
#'
#' @param path path to location of downloaded Portal data; or 'repo' to retrieve data from github repo
#' @param level summarize by "Plot", "Treatment", or "Site"
#' @param type specify subset of species; either all "Rodents" or only "Granivores"
#' @param length specify subset of plots; use "All" plots or only "Longterm" plots (plots that have had same treatment for entire time series)
#' @param unknowns either removes all individuals not identified to species (unknowns=F) or sums them in an additional column (unknowns=T)
#' @param incomplete either removes all data from incomplete trapping sessions (incomplete = F) or includes them (incomplete = T)
#'                  [note that if level="plot" and incomplete=T, NAs will be included in periods where trapping was incomplete]
#' @param shape return data as a "crosstab" or "flat" list
#' @param time return data using the complete "newmoon" numbers of the original "period" numbers
#' @param output specify whether to return "abundance", or "biomass", or "energy"
#' @param fillweight specify whether to fill in unknown weights with other records from that individual or species, where possible
#'
#' @export
#'


get_rodent_data <- function(path = '~', level = "Site", type = "Rodents",
                            length = "all", unknowns = FALSE, incomplete = FALSE,
                            shape = "crosstab", time = "period", output = "abundance", fillweight = TRUE)
{
  ##########Get Data
  data_tables = loadData(path)
  rodents = data_tables[[1]]
  species = data_tables[[2]]
  trapping = data_tables[[3]]
  newmoons = data_tables[[4]]
  plots = data_tables[[5]]

  rodents %>%
    ##########Fill in missing weights --------------------------------
  fill_weight(fillweight, species) %>%
    ##########Data cleanup --------------------------------
  remove_suspect_entries() %>%
    process_unknownsp(species, unknowns) %>%

    ###########Exclude non-granivores-----------------------
  process_granivores(type) %>%

    ###########Remove incomplete trapping sessions----------
  remove_incomplete_censuses(trapping, incomplete) %>%

    ###########Use only Long-term treatments --------------
  filter_plots(length) %>%

    ###########Re-assign back into `rodents` --------------
  {.} -> rodents

  ###########Summarise by Treatment ----------------------
  if(level %in% c("Treatment","treatment")) {
    #Name plot treatments in each time period

    rodents = join_plots_to_rodents(rodents, plots)

    if(output %in% c("Biomass", "biomass")) {
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::mutate(wgt = as.numeric(wgt)) %>%
        dplyr::group_by(period, treatment, species) %>%
        dplyr::summarize(biomass = sum(wgt, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::select(period, treatment, species, biomass)
    } else if (output %in% c('Energy', 'energy')) {
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::mutate(wgt = as.numeric(wgt)) %>%
        dplyr::mutate(ind.energy = wgt ^ .75, na.rm = T) %>%
        dplyr::group_by(period, treatment, species) %>%
        dplyr::summarize(energy = sum(ind.energy, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::select(period, treatment, species, energy)
    } else { # abundance by default
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::group_by(period, treatment) %>%
        dplyr::do(data.frame(x = table(.$species))) %>%
        dplyr::ungroup() %>%
        dplyr::select(period, treatment, species = x.Var1, abundance = x.Freq)
    }
  }

  ##########Summarise by plot ----------------------------
  if(level %in% c("Plot", "plot")) {
    trapping = filter_plots(trapping, length)
    rodents = join_trapping_to_rodents(rodents, trapping, incomplete)
    #  reduce size of trapping table

    if(output %in% c("Biomass", "biomass")) {
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::mutate(wgt = as.numeric(wgt)) %>%
        dplyr::group_by(period, plot, sampled, species) %>%
        dplyr::summarize(biomass = sum(wgt, na.rm = T)) %>%
        dplyr::mutate(biomass = replace(biomass, sampled == 0, NA)) %>% # 0->NA on untrapped plots
        dplyr::ungroup() %>%
        dplyr::select(period, plot, species, biomass)
    } else if (output %in% c('Energy', 'energy')) {
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::mutate(wgt = as.numeric(wgt)) %>%
        dplyr::mutate(ind.energy = wgt ^ .75, na.rm = T) %>%
        dplyr::group_by(period, plot, sampled, species) %>%
        dplyr::summarize(energy = sum(ind.energy, na.rm = T)) %>%
        dplyr::mutate(energy = replace(energy, sampled == 0, NA)) %>% # 0->NA on untrapped plots
        dplyr::ungroup() %>%
        dplyr::select(period, plot, species, energy)
    } else { # abundance by default
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::group_by(period, plot, sampled) %>%
        dplyr::do(data.frame(x = table(.$species))) %>%
        dplyr::mutate(x.Freq = replace(x.Freq, sampled == 0, NA)) %>% # 0->NA on untrapped plots
        dplyr::ungroup() %>%
        dplyr::select(period, plot, species = x.Var1, abundance = x.Freq)
    }
  }

  ##########Summarise site-wide --------------------------
  if(level %in% c("Site", "site")) {
    if(output %in% c("Biomass", "biomass")) {
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::mutate(wgt = as.numeric(wgt)) %>%
        dplyr::group_by(period, species) %>%
        dplyr::summarize(biomass = sum(wgt, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::select(period, species, biomass)
    } else if(output %in% c("Energy", 'energy')) {
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::mutate(wgt = as.numeric(wgt)) %>%
        dplyr::mutate(ind.energy = wgt ^ .75, na.rm = T) %>%
        dplyr::group_by(period, species) %>%
        dplyr::summarize(energy = sum(ind.energy, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::select(period, species, energy)
    } else { # abundance by default
      out_df <- rodents %>%
        dplyr::mutate(species = factor(species)) %>%
        dplyr::group_by(period) %>%
        dplyr::do(data.frame(x = table(.$species))) %>%
        dplyr::ungroup() %>%
        dplyr::select(period, species = x.Var1, abundance = x.Freq)
    }
  }

  ##########Convert data to crosstab ----------------------
  if(shape %in% c("Crosstab", "crosstab")){
    out_df = make_crosstab(out_df, variable_name = output)
    if (output %in% c('Biomass', 'biomass', 'Energy', 'energy')) {
      if (level %in% c('plot', 'Plot')) {
        if ("<NA>" %in% colnames(out_df)) out_df = out_df[,(1:ncol(out_df) - 1)]
        out_df[is.na(out_df)] <- 0
        out_df = dplyr::left_join(out_df, trapping[,c('period', 'plot', 'sampled')], by = c('period', 'plot') )
        out_df[ which(out_df$sampled == 0), 3:(ncol(out_df) - 1)] <- NA
        out_df = out_df[, (1:ncol(out_df) - 1)]
      } else {
        out_df[is.na(out_df)] <- 0
      }
    }
  } else {

    if(output %in% c('Biomass', 'biomass')) {
      if(level %in% c('Site', 'site')) out_df = tidyr::complete(out_df, species, period, fill = list(biomass = 0))
      if(level %in% c('Treatment', 'treatment')) out_df =  tidyr::complete(out_df, species, period, treatment, fill = list(biomass = 0))
      if(level %in% c('Plot', 'Plot')) {
        out_df =  tidyr::complete(out_df, species, period, plot, fill = list(biomass = 0))
        out_df = filter(out_df, !(is.na(species)))
        out_df = dplyr::left_join(out_df, trapping[,c('period', 'plot', 'sampled')], by = c('period', 'plot') )
        out_df[ which(out_df$sampled == 0), (ncol(out_df) - 1)] <- NA
        out_df = out_df[, (1:ncol(out_df) - 1)]
      }

    }
    if (output %in% c('Energy', 'energy')){
      if(level %in% c('Site', 'site')) out_df = tidyr::complete(out_df, species, period, fill = list(energy = 0))
      if(level %in% c('Treatment', 'treatment')) out_df =  tidyr::complete(out_df, species, period, treatment, fill = list(energy = 0))
      if(level %in% c('Plot', 'Plot')) {
        out_df =  tidyr::complete(out_df, species, period, plot, fill = list(energy = 0))
        out_df = filter(out_df, !(is.na(species)))
        out_df = dplyr::left_join(out_df, trapping[,c('period', 'plot', 'sampled')], by = c('period', 'plot') )
        out_df[ which(out_df$sampled == 0), (ncol(out_df) - 1)] <- NA
        out_df = out_df[, (1:ncol(out_df) - 1)]
      }
      if(output %in% c('Abundance', 'abundance')) {
        if(level %in% c('Site', 'site')) out_df = tidyr::complete(out_df, species, period, fill = list(abundance = 0))
        if(level %in% c('Treatment', 'treatment')) out_df =  tidyr::complete(out_df, species, period, treatment, fill = list(abundance = 0))
        if(level %in% c('Plot', 'plot')) {
          out_df =  tidyr::complete(out_df, species, period, plot, fill = list(abundance = 0))
          out_df = dplyr::left_join(out_df, trapping[,c('period', 'plot', 'sampled')], by = c('period', 'plot') )
          out_df[ which(out_df$sampled == 0), (ncol(out_df) - 1)] <- NA
          out_df = out_df[, (1:ncol(out_df) - 1)]
        }
      }
    }
  }



    ###########Switch to new moon number if time == 'newmoon'------------------
    out_df = add_time(out_df, newmoons, time)



    return(out_df)
  }


  #' @rdname get_rodent_data
  #'
  #' @description \code{abundance} essentially passes along all arguments to \code{get_rodent_data}, but fixes the output type as "abundance"
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
  #' @description \code{biomass} essentially passes along all arguments to \code{get_rodent_data}, but fixes the output type as "biomass"
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
  #' @description \code{energy} essentially passes along all arguments to \code{get_rodent_data}, but fixes the output type as "energy"
  #'
  #' @examples
  #' energy("repo")
  #'
  #' @export
  #'
  energy <- function(...) {
    get_rodent_data(..., output = "energy")
  }

