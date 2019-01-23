
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
colony_presence_absence <- function(path = '~', level = 'Site',
                                    rare_sp = F, unknowns = F,
                                    download_if_missing = TRUE) {

  data_tables <- load_ant_data(path, download_if_missing = download_if_missing)

  colony <- data_tables$colony_data
  antsp <- data_tables$species_table

  # list of species to include
  # if unknowns == F, unkn and those identified only to genus will be removed
  # if rare_sp == F, the species list will be restricted to only the 9 species we absolutely know were censused consistently
  specieslist <- antsp$species
  if (unknowns == FALSE)
  {
    specieslist <- setdiff(specieslist, c('unkn', 'camp sp', 'phei sp', 'novo sp'))
  }
  if (rare_sp == FALSE)
  {
    specieslist <- intersect(specieslist, c('cono bico', 'cono insa', 'irid prui',
                                            'myrm depi', 'myrm mimi', 'phei sita',
                                            'phei xero', 'pogo dese', 'sole xylo'))
  }


  # filter out duplicated data (flag=10)
  colonydat = dplyr::filter(colony, species %in% specieslist, !flag %in% c(10))
  # reduce colony data to list of year, plot, species
  if (level == 'Site') {
    colonypresence = colonydat %>% dplyr::select(year, species) %>% unique()
    colonypresence$presence = rep(1)

    # data frame of all year/species
    full_df = expand.grid(year = unique(colonypresence$year), species = specieslist)

  }
  if (level == 'Plot') {
    colonypresence = colonydat %>% dplyr::select(year, plot, species) %>% unique()
    colonypresence$presence = rep(1)

    # data frame of which plots were censused in which years
    df = colonypresence %>% dplyr::select(year, plot) %>% unique()
    full_df = expand.grid(year = unique(df$year), plot = unique(df$plot), species = specieslist)
    full_df = merge(df, full_df)
  }
  if (level == 'Stake') {
    # filter out data taken only at plot level (flag=9) or rows where stake is missing (flag=1)
    colonypresence = dplyr::filter(colonydat, !flag %in% c(9, 1), !is.na(stake)) %>%
      dplyr::select(year, plot, stake, species)
    colonypresence$presence = rep(1)

    #data frame of which plots were censused in which years
    df = colonypresence %>% dplyr::select(year, plot) %>% unique()
    full_df = expand.grid(year = unique(df$year), plot = unique(df$plot),
                          stake = c(11, 12, 13, 14, 15, 16, 17,
                                    21, 22, 23, 24, 25, 26, 27,
                                    31, 32, 33, 34, 35, 36, 37,
                                    41, 42, 43, 44, 45, 46, 47,
                                    51, 52, 53, 54, 55, 56, 57,
                                    61, 62, 63, 64, 65, 66, 67,
                                    71, 72, 73, 74, 75, 76, 77), species = specieslist)
    full_df = merge(df, full_df)
    full_df = full_df[order(full_df$year, full_df$plot, full_df$stake, full_df$species), ]
  }

  colonypresabs = merge(full_df, colonypresence, all = T)
  # fill NAs with absence '0'
  colonypresabs[is.na(colonypresabs)] = 0
  # except 'sole xylo' was not censused in 1978-1979, so those go back to NA
  colonypresabs$presence[(colonypresabs$species %in% c('sole xylo', 'sole sp') & colonypresabs$year %in% c(1978, 1979))] = NA
  # in 1977, 1978, 1979, 1980, 1981 they sometimes included myrm depi in myrm mimi counts, so these too shouldn't be considered true absences
  colonypresabs$presence[(colonypresabs$species == 'myrm depi') & colonypresabs$year %in% seq(1977, 1981) & colonypresabs$presence == 0] = NA
  # species camp fest was probably not censused regularly (only one record of it)
  colonypresabs$presence[(colonypresabs$species == 'camp fest') & colonypresabs$presence == 0] = NA

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
bait_presence_absence= function(path = '~', level = 'Site',
                                download_if_missing = TRUE)
{

  data_tables <- load_ant_data(path, download_if_missing = download_if_missing)

  bait <- data_tables$bait_data

  # list of species
  specieslist = unique(bait$species)

  # reduce data to list of year, plot, species
  if (level == 'Site') {
    baitpresence = bait %>% dplyr::select(year, species) %>% unique()
    baitpresence$presence = rep(1)

    # data frame of all year/species
    full_df = expand.grid(year = unique(baitpresence$year), species = specieslist)

  }
  if (level == 'Plot') {
    baitpresence = bait %>% dplyr::select(year, plot, species) %>% unique()
    baitpresence$presence = rep(1)

    # data frame of year/plot/species
    full_df = expand.grid(year = unique(bait$year), plot = seq(24), species = specieslist)
  }
  if (level == 'Stake') {
    baitpresence = bait %>% dplyr::select(year, plot, stake, species)
    baitpresence$presence = rep(1)

    #data frame of which plots were censused in which years
    full_df = expand.grid(year = unique(bait$year), plot = seq(24),
                          stake = c(11, 13, 15, 17,
                                    22, 24, 26,
                                    31, 33, 35, 37,
                                    42, 44, 46,
                                    51, 53, 55, 57,
                                    62, 64, 66,
                                    71, 73, 75, 77), species = specieslist)
  }

  baitpresabs = merge(full_df, baitpresence, all = T)
  # fill NAs with absence '0'
  baitpresabs[is.na(baitpresabs)] = 0

  return(baitpresabs)
}



