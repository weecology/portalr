
#' @title Ant Colony Presence Absence
#'
#' @description Get ant species presence/absence by year/plot/stake from colony census data
#'
#' Anomalies in ant colony census protocol over the years means that it can be difficult to discern true absences
#' of all species in all years.  This function uses information from Portal_ant_species.csv and Portal_ant_dataflags.csv
#' to predict true presence/absence of species per plot per year.  If a more conservative estimate is desired,
#' setting the argument 'rare_sp = T' will only include species we are confident were censused regularly. Setting
#' 'rare_sp = F' may include some false absences, since it is unknown if some rare species were censused in all years.
#' Unknowns may also be excluded from output if desired.
#'
#' @param path path to location of downloaded Portal data; or "repo" to
#'   retrieve data from github repo
#' @param level level at which to summarize data: 'Site', 'Plot', or 'Stake'
#' @param rare_sp include rare species (T) or not (F).
#'                    Rare species may or may not have been censused in all years. Setting rare_sp=F gives a more conservative estimate of presence/absence
#' @param unknowns include unknown species (T) or not (F). Unknowns include those only identified to genus.
#' @inheritParams load_ant_data
#'
#' @return data frame with year, species, (plot if applicable), and presence [1, 0, NA]
#'
#' @export
#'
colony_presence_absence <- function(path = get_default_data_path(),
                                    level = "Site",
                                    rare_sp = FALSE, unknowns = FALSE,
                                    download_if_missing = TRUE)
{
  level <- tolower(level)
  data_tables <- load_ant_data(path, download_if_missing = download_if_missing)

  colony <- data_tables$colony_data
  antsp <- data_tables$species_table

  # list of species to include
  # if unknowns == F, unkn and those identified only to genus will be removed
  # if rare_sp == F, the species list will be restricted to only the 9 species we absolutely know were censused consistently
  specieslist <- antsp$species
  if (unknowns == FALSE)
  {
    specieslist <- setdiff(specieslist, c("unkn", "camp sp", "phei sp", "novo sp"))
  }
  if (rare_sp == FALSE)
  {
    specieslist <- intersect(specieslist, c("cono bico", "cono insa", "irid prui",
                                            "myrm depi", "myrm mimi", "phei sita",
                                            "phei xero", "pogo dese", "sole xylo"))
  }

  # filter out duplicated data (flag=10)
  colonydat <- dplyr::filter(colony, species %in% specieslist, !flag %in% c(10))

  # summarize colony data by site, plot, or stake level, and converting
  #   observations into presence = 1
  if (level == "site")
  {
    colonypresence <- colonydat %>% dplyr::select(year, species) %>% dplyr::distinct()
    colonypresence$presence <- 1

    # data frame of all year/species
    colonypresabs <- colonypresence %>%
      tidyr::complete(year, species = specieslist,
                      fill = list(presence = 0))

  } else if (level == "plot") {
    colonypresence <- colonydat %>% dplyr::select(year, plot, species) %>% dplyr::distinct()
    colonypresence$presence <- 1

    # data frame of all sampled year-plot combinations and all species
    colonypresabs <- colonypresence %>%
      tidyr::complete(tidyr::nesting(year, plot), species = specieslist,
                      fill = list(presence = 0))

  } else if (level == "stake") {
    # filter out data taken only at plot level (flag=9) or rows where stake is missing (flag=1)
    colonypresence <- dplyr::filter(colonydat, !flag %in% c(9, 1), !is.na(stake)) %>%
      dplyr::select(year, plot, stake, species)
    colonypresence$presence <- 1

    # data frame of all sampled year-plot combinations, all stakes, and all species
    colonypresabs <- colonypresence %>%
      tidyr::complete(tidyr::nesting(year, plot), stake, species = specieslist,
                      fill = list(presence = 0))
  }

  # except "sole xylo" was not censused in 1978-1979, so those go back to NA
  colonypresabs$presence[(colonypresabs$species %in% c("sole xylo", "sole sp") & colonypresabs$year %in% c(1978, 1979))] = NA
  # in 1977, 1978, 1979, 1980, 1981 they sometimes included myrm depi in myrm mimi counts, so these too shouldn"t be considered true absences
  colonypresabs$presence[(colonypresabs$species == "myrm depi") & colonypresabs$year %in% seq(1977, 1981) & colonypresabs$presence == 0] = NA
  # species camp fest was probably not censused regularly (only one record of it)
  colonypresabs$presence[(colonypresabs$species == "camp fest") & colonypresabs$presence == 0] = NA

  return(colonypresabs)
}


#' @title Ant Bait Presence Absence
#'
#' @description Get ant species presence/absence by year/plot/stake from bait census data
#'
#' Bait census data is more consistent over time than the colony census data. This function assumes that all species
#' present in at least one census were censused in all years.
#'
#' @param path path to location of downloaded Portal data; or "repo" to
#'   retrieve data from github repo
#' @param level level at which to summarize data: 'Site', 'Plot', or 'Stake'
#' @inheritParams load_ant_data
#'
#' @return data frame with year, species, (plot if applicable), and presence [1, 0]
#'
#' @export
#'
bait_presence_absence <- function(path = get_default_data_path(),
                                  level = "Site",
                                  download_if_missing = TRUE)
{
  level <- tolower(level)
  bait <- load_datafile(file.path("Ants", "Portal_ant_bait.csv"),
                        na.strings = "", path = path,
                        download_if_missing = download_if_missing)

  grouping <- switch(level,
                     "site" = rlang::quos(year, species),
                     "plot" = rlang::quos(year, plot, species),
                     "stake" = rlang::quos(year, plot, stake, species))

  baitpresabs <- bait %>%
    dplyr::select(!!!grouping) %>%
    dplyr::distinct() %>%
    fill_presence(grouping)

  return(baitpresabs)
}

#' @title Fill in missing presence values based on grouping levels
#'
#' @description A helper function to be used by both
#'   \code\link{bait_presence_absence}} and \code{\link{colony_presence_absence}}
#'
#' @param df data.frame to modify
#' @param ... levels to pass to tidyr::complete()
#'
#' @return data.frame with 1s and 0s in a new presence column
#'
#' @noRd
#'
fill_presence <- function(df, grouping)
{
  df %>%
    dplyr::mutate(presence = 1) %>%
    tidyr::complete(!!!grouping,
                    fill = list(presence = 0))
}
