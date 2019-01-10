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
load_rodent_data <- function(path = "~", download_if_missing = TRUE, clean = TRUE)
{
  # set up files and NA options
  data_files <- c("rodent_data" = file.path("Rodents", "Portal_rodent.csv"),
                  "species_table" = file.path("Rodents", "Portal_rodent_species.csv"),
                  "trapping_table" = file.path("Rodents", "Portal_rodent_trapping.csv"),
                  "newmoons_table" = file.path("Rodents", "moon_dates.csv"),
                  "plots_table" = file.path("SiteandMethods", "Portal_plots.csv"))
  na_strings <- list(c(""), c(""), c("NA"), c("NA"), c("NA"))

  # retrieve data
  data_tables <- load_generic_data(data_files, na_strings, path, download_if_missing)

  # reformat species columns
  if (!"species" %in% names(data_tables$species_table))
  {
    data_tables$species_table <- dplyr::rename(data_tables$species_table,
                                               species = speciescode)
  }

  # convert rodent tags to characters if not already
  data_tables$rodent_data$tag <- as.character(data_tables$rodent_data$tag)

  # remove data still under quality control
  if (clean)
  {
    data_tables$rodent_data <- clean_data(data_tables$rodent_data,
                                          data_tables$trapping_table,
                                          by = c("month", "day", "year", "period", "plot"))
    data_tables$newmoons_table <- clean_data(data_tables$newmoons_table,
                                             data_tables$trapping_table,
                                             by = "period")
    data_tables$plots_table <- clean_data(data_tables$plots_table,
                                          data_tables$trapping_table,
                                          by = c("year", "month", "plot"))
    data_tables$trapping_table <- dplyr::filter(data_tables$trapping_table,
                                                qcflag == 1)
  }

  return(data_tables)
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

load_plant_data <- function(path = "~", download_if_missing = TRUE)
{
  # set up files and NA options
  data_files <- c("quadrat_data" = file.path("Plants", "Portal_plant_quadrats.csv"),
                  "species_table" = file.path("Plants", "Portal_plant_species.csv"),
                  "census_table" = file.path("Plants", "Portal_plant_censuses.csv"),
                  "date_table" = file.path("Plants", "Portal_plant_census_dates.csv"),
                  "plots_table" = file.path("SiteandMethods", "Portal_plots.csv"),
                  "transect_data" = file.path("Plants", "Portal_plant_transects_2015_present.csv"),
                  "oldtransect_data" = file.path("Plants", "Portal_plant_transects_1989_2009.csv"))
  na_strings <- list(c(""), c(""), c("NA"), c("", "none", "unknown"), c("NA"),c(""), c(""))

  # retrieve data
  data_tables <- load_generic_data(data_files, na_strings, path, download_if_missing)

  # reformat species columns
  if (!"sp" %in% names(data_tables$species_table))
  {
    data_tables$species_table <- dplyr::rename(data_tables$species_table,
                                               sp = species,
                                               species = speciescode)
  }

  return(data_tables)
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

load_ant_data <- function(path = "~", download_if_missing = TRUE)
{
  # set up files and NA options
  data_files <- c("bait_data" = file.path("Ants", "Portal_ant_bait.csv"),
                  "colony_data" = file.path("Ants", "Portal_ant_colony.csv"),
                  "species_table" = file.path("Ants", "Portal_ant_species.csv"),
                  "plots_table" = file.path("SiteandMethods", "Portal_plots.csv"))
  na_strings <- list(c(""), c(""), c("NA"), c("NA"))

  # retrieve data
  data_tables <- load_generic_data(data_files, na_strings, path, download_if_missing)

  # reformat species columns
  if (!"sp" %in% names(data_tables$species_table))
  {
    data_tables$species_table <- dplyr::rename(data_tables$species_table,
                                               sp = species,
                                               species = speciescode)
  }

  return(data_tables)
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
load_trapping_data <- function(path = "~", download_if_missing = TRUE, clean = TRUE)
{
  # set up files and NA options
  data_files <- c("trapping_table" = file.path("Rodents", "Portal_rodent_trapping.csv"),
                  "newmoons_table" = file.path("Rodents", "moon_dates.csv"))
  na_strings <- list(c("NA"), c("NA"))

  # retrieve data
  data_tables <- load_generic_data(data_files, na_strings, path, download_if_missing)

  # remove data still under quality control
  if (clean)
  {
    data_tables$newmoons_table <- clean_data(data_tables$newmoons_table,
                                             data_tables$trapping_table,
                                             by = "period")
  }

  return(data_tables)
}

#' @title generic data loading function
#'
#' @description does checking for whether data exists and then reads it in,
#'   using na_strings to determine what gets converted to NA,
#'   and then returning a list of the data.frames as output
#'
#' @noRd
load_generic_data <- function(data_files, na_strings, path = "~", download_if_missing = TRUE)
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
  data_files <- sapply(data_files, function(x) file.path(base_path, x))

  ## check if files exist and download if appropriate
  if (tolower(path) != "repo" && any(!sapply(data_files, file.exists)))
  {
    if (download_if_missing) {
      warning("Proceeding to download data into specified path", path, "\n")
      download_observations(path)
    } else {
      stop("Data files were not found in specified path", path, "\n")
    }
  }
  stopifnot(length(na_strings) == length(data_files))

  ## output message about data version
  version_file <- file.path(base_path, "version.txt")
  if (tolower(path) != "repo" && !file.exists(version_file))
  {
    message("Loading in data version < 1.1.0")
  } else {
    message("Loading in data version ", read.table(version_file)[1, 1])
  }
  ## read in data tables
  data_tables <- lapply(seq(data_files), function(i) {
    read.csv(data_files[i], na.strings = na_strings[[i]], stringsAsFactors = FALSE)
  })
  names(data_tables) <- names(data_files)

  return(data_tables)
}
