#' @name load_rodent_data
#' @aliases load_plant_data load_ant_data
#'
#' @title Read in the Portal data files
#'
#' @description Loads Portal data files from either a user-defined
#'   path or the online Github repository. If the user-defined path is un-
#'   available, the default option is to download to that location.
#'
#' @param path either the file path that contains the PortalData folder or
#'  "repo", which then pulls data from the PortalData GitHub repository
#' @param download_if_missing if the specified file path doesn't have the
#'   PortalData folder, then download it


#' @rdname load_rodent_data
#' @description \code{\link{load_rodent_data}} loads the rodent data files
#'
#' @param clean logical, load only QA/QC rodent data (TRUE) or all data (FALSE)
#' @param quiet logical, whether to run without version messages
#'
#' @return \code{\link{load_rodent_data}} returns a list of 5 dataframes:
#'   \tabular{ll}{
#'     \code{rodent_data} \tab raw data on rodent captures\cr
#'     \code{species_table} \tab species code, names, types\cr
#'     \code{trapping_table} \tab when each plot was trapped\cr
#'     \code{newmoons_table} \tab pairs census periods with newmoons\cr
#'     \code{plots_table} \tab rodent treatment assignments for each plot\cr
#'   }
#'
#' @examples
#' \donttest{
#' portal_data <- load_rodent_data("repo")
#' }
#' @export
#'
load_rodent_data <- function(path = get_default_data_path(),
                             download_if_missing = TRUE, clean = TRUE, quiet = FALSE)
{
  rodent_data <- load_datafile(file.path("Rodents", "Portal_rodent.csv"),
                               na.strings = "", path, download_if_missing,
                               version_message = !quiet)
  species_table <- load_datafile(file.path("Rodents", "Portal_rodent_species.csv"),
                                 na.strings = "", path, download_if_missing)
  trapping_table <- load_datafile(file.path("Rodents", "Portal_rodent_trapping.csv"),
                                  na.strings = "NA", path, download_if_missing)
  newmoons_table <- load_datafile(file.path("Rodents", "moon_dates.csv"),
                                  na.strings = "NA", path, download_if_missing)
  plots_table <- load_datafile(file.path("SiteandMethods", "Portal_plots.csv"),
                               na.strings = "NA", path, download_if_missing)

  # reformat species columns
  if (!"species" %in% names(species_table))
  {
    species_table <- dplyr::rename(species_table, species = speciescode)
  }

  # convert rodent tags to characters if not already
  rodent_data$tag <- as.character(rodent_data$tag)

  # remove data still under quality control
  if (clean)
  {
    rodent_data <- clean_data(rodent_data, trapping_table,
                              by = c("month", "day", "year", "period", "plot"))
    newmoons_table <- clean_data(newmoons_table, trapping_table,
                                 by = "period")
    plots_table <- clean_data(plots_table, trapping_table,
                              by = c("year", "month", "plot"))
    trapping_table <- dplyr::filter(trapping_table, qcflag == 1)
  }

  return(mget(c("rodent_data", "species_table", "trapping_table",
                "newmoons_table", "plots_table")))
}

#' @rdname load_rodent_data
#' @description \code{\link{load_plant_data}} loads the plant data files
#'
#' @return \code{\link{load_plant_data}} returns a list of 7 dataframes:
#'   \tabular{ll}{
#'     \code{quadrat_data} \tab raw plant quadrat data\cr
#'     \code{species_table} \tab species code, names, types\cr
#'     \code{census_table} \tab indicates whether each quadrat was counted in each
#'       census; area of each quadrat\cr
#'     \code{date_table} \tab start and end date of each plant census\cr
#'     \code{plots_table} \tab rodent treatment assignments for each plot\cr
#'     \code{transect_data} \tab raw plant transect data with length and height (2015-present)\cr
#'     \code{oldtransect_data} \tab raw plant transect data as point counts (1989-2009)\cr
#'   }
#'
#' @export
#'
#' @examples
#' \donttest{
#' portal_plant_data <- load_plant_data("repo")
#' }

load_plant_data <- function(path = get_default_data_path(),
                            download_if_missing = TRUE, quiet = FALSE)
{
  quadrat_data <- load_datafile(file.path("Plants", "Portal_plant_quadrats.csv"),
                                na.strings = "", path, download_if_missing,
                                version_message = !quiet)
  species_table <- load_datafile(file.path("Plants", "Portal_plant_species.csv"),
                                 na.strings = "", path, download_if_missing)
  census_table <- load_datafile(file.path("Plants", "Portal_plant_censuses.csv"),
                                na.strings = "NA", path, download_if_missing)
  date_table <- load_datafile(file.path("Plants", "Portal_plant_census_dates.csv"),
                              na.strings = c("", "none", "unknown"), path, download_if_missing)
  plots_table <- load_datafile(file.path("SiteandMethods", "Portal_plots.csv"),
                               na.strings = "NA", path, download_if_missing)
  transect_data <- load_datafile(file.path("Plants", "Portal_plant_transects_2015_present.csv"),
                                 na.strings = "", path, download_if_missing)
  oldtransect_data <- load_datafile(file.path("Plants", "Portal_plant_transects_1989_2009.csv"),
                                    na.strings = "", path, download_if_missing)

  # reformat species columns
  if (!"sp" %in% names(species_table))
  {
    species_table <- dplyr::rename(species_table,
                                   sp = species,
                                   species = speciescode)
  }

  return(mget(c("quadrat_data", "species_table", "census_table",
                "date_table", "plots_table",
                "transect_data", "oldtransect_data")))
}

#' @rdname load_rodent_data
#' @description \code{\link{load_ant_data}} loads the ant data files
#'
#' @return \code{\link{load_ant_data}} returns a list of 4 dataframes:
#'   \tabular{ll}{
#'     \code{bait_data} \tab raw ant bait data\cr
#'     \code{colony_data} \tab raw ant colony data\cr
#'     \code{species_table} \tab species code, names, types\cr
#'     \code{plots_table} \tab treatment assignments for each plot\cr
#'   }
#'
#' @export
#'
#' @examples
#' \donttest{
#' portal_ant_data <- load_ant_data("repo")
#' }

load_ant_data <- function(path = get_default_data_path(),
                          download_if_missing = TRUE, quiet = FALSE)
{
  bait_data <- load_datafile(file.path("Ants", "Portal_ant_bait.csv"),
                             na.strings = "", path, download_if_missing,
                             quiet = TRUE)
  colony_data <- load_datafile(file.path("Ants", "Portal_ant_colony.csv"),
                               na.strings = "", path, download_if_missing)
  species_table <- load_datafile(file.path("Ants", "Portal_ant_species.csv"),
                                 na.strings = "NA", path, download_if_missing)
  plots_table <- load_datafile(file.path("SiteandMethods", "Portal_plots.csv"),
                               na.strings = "NA", path, download_if_missing)

  # reformat species columns
  if (!"sp" %in% names(species_table))
  {
    species_table <- dplyr::rename(species_table,
                                   sp = species,
                                   species = speciescode)
  }

  return(mget(c("bait_data", "colony_data",
                "species_table", "plots_table")))

}

#' @rdname load_rodent_data
#' @description \code{\link{load_trapping_data}} loads just the rodent trapping files
#'
#' @inheritParams load_rodent_data
#'
#' @return \code{\link{load_trapping_data}} returns a list of 2 dataframes:
#'   \tabular{ll}{
#'     \code{trapping_table} \tab when each plot was trapped\cr
#'     \code{newmoons_table} \tab pairs census periods with newmoons\cr
#'   }
#'
#' @examples
#' \donttest{
#' trapping_data <- load_trapping_data("repo")
#' }
#' @export
load_trapping_data <- function(path = get_default_data_path(),
                               download_if_missing = TRUE, clean = TRUE)
{
  trapping_table <- load_datafile(file.path("Rodents", "Portal_rodent_trapping.csv"),
                                  na.strings = "NA", path, download_if_missing, version_message = TRUE)
  newmoons_table <- load_datafile(file.path("Rodents", "moon_dates.csv"),
                                  na.strings = "NA", path, download_if_missing)

  # remove data still under quality control
  if (clean)
  {
    newmoons_table <- clean_data(newmoons_table, trapping_table,
                                 by = "period")
  }

  return(mget(c("trapping_table", "newmoons_table")))
}

#' @name load_datafile
#'
#' @title read in a raw datafile from the downloaded data or the GitHub repo
#'
#' @description does checking for whether a particular datafile exists and then
#'   reads it in, using na_strings to determine what gets converted to NA. It
#'   can also download the dataset if it's missing locally.
#'
#' @param datafile the path to the datafile within the folder for Portal data
#' @param version_message whether to display a message about the data version
#'   being loaded
#' @inheritParams load_rodent_data
#' @inheritParams utils::read.table
#'
#' @examples
#' \donttest{
#' rodent_species <- load_datafile("Rodents/Portal_rodent_species.csv")
#' }
#' @export
load_datafile <- function(datafile, na.strings = "", path = get_default_data_path(),
                          download_if_missing = TRUE, version_message = FALSE)
{

  ## define file paths
  if (tolower(path) == "repo")
  {
    base_path <- "https://raw.githubusercontent.com/weecology/PortalData/master"
  } else {
    tryCatch(base_path <- file.path(normalizePath(path, mustWork = TRUE), "PortalData"),
             error = function(e) stop("Specified path ", path, "does not exist. Please create it first."),
             warning = function(w) w)
  }
  datafile <- file.path(base_path, datafile)

  ## check if files exist and download if appropriate
  if (tolower(path) != "repo" && !file.exists(datafile))
  {
    if (download_if_missing) {
      warning("Proceeding to download data into specified path", path, "\n")
      download_observations(path)
    } else {
      stop("Data files were not found in specified path", path, "\n")
    }
  }

  ## output message about data version
  if (version_message)
  {
    version_file <- file.path(base_path, "version.txt")
    if (tolower(path) != "repo" && !file.exists(version_file))
    {
      message("Loading in data version < 1.1.0")
    } else {
      message("Loading in data version ", read.table(version_file)[1, 1])
    }
  }

  ## read in the data table and return
  read.csv(datafile, na.strings = na.strings, stringsAsFactors = FALSE)
}
