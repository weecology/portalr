#' @importFrom graphics plot
#' @importFrom stats aggregate
#' @importFrom utils download.file read.csv unzip read.table tail

#' @title Full Path
#' @description Return normalized path for all operating systems
#' @param reference_path a path to join with current working directory
#' @param base_path Current working directory else path given
#'
#' @return Full path
#'
#' @examples
#' full_path('PortalData/Rodents/Portal_rodent.csv')
#' full_path('PortalData/Rodents/Portal_rodent.csv', '~')
#'
#' @noRd
full_path <- function(reference_path, base_path = getwd()) {
  base_path <- normalizePath(base_path)
  path <- normalizePath(file.path(base_path, reference_path), mustWork = FALSE)
  return(path)
}

#' @title Download the PortalData repo
#'
#' @description This downloads the latest portal data regardless if they are
#'   actually updated or not.
#'   TODO: incorporate data retriever into this when it's pointed at the github repo
#' @param base_folder Folder into which data will be downloaded
#' @param version Version of the data to download (default = "latest")
#'
#' @return None
#'
#' @examples
#' \donttest{
#'   download_observations()
#'   download_observations("~/old-data", version = "1.50.0")
#' }
#'
#' @export
download_observations <- function(base_folder = "~", version = "latest")
{
  # get version info
  releases <- get_data_versions(version == "latest", halt_on_error = TRUE)

  # match version
  if (version == "latest")
  {
    match_idx <- 1
  } else {
    # Normalize version number
    if (grepl("^[0-9]+\\.[0-9]+$", version))
    {
      version <- paste0(version, ".0")
    }
    if (!grepl("^[0-9]+\\.[0-9]+\\.0$", version))
    {
      stop("Invalid version number; given, ", version, call. = FALSE)
    }

    match_idx <- match(version, releases$version)
    if (length(match_idx) != 1 || is.na(match_idx))
    {
      stop("Did not find a version of the data matching, ", version, call. = FALSE)
    }
  }

  # Attemt to download the zip file
  message("Downloading version ", releases$version[match_idx], " of the data...")
  zip_download_path <- releases$zipball_url[match_idx]
  zip_download_dest <- full_path("PortalData.zip", tempdir())
  download.file(zip_download_path, zip_download_dest, quiet = TRUE, mode = "wb")

  final_data_folder <- full_path("PortalData", base_folder)

  # Clear out the old files in the data folder without doing potentially dangerous
  # recursive deleting.
  if (file.exists(final_data_folder)) {
    old_files <- list.files(
      final_data_folder,
      full.names = TRUE,
      all.files = TRUE,
      recursive = TRUE,
      include.dirs = FALSE
    )
    file.remove(normalizePath(old_files))
    unlink(final_data_folder, recursive = TRUE)
  }

  #Github serves this up with the -master extension. Unzip and rename to remove that.
  primary_data_folder <- unzip(zip_download_dest, list = TRUE)$Name[1]
  unzip(zip_download_dest, exdir = base_folder)
  Sys.sleep(10)
  file.remove(zip_download_dest)
  file.rename(full_path(primary_data_folder, base_folder), final_data_folder)
}

#' @title get version and download info for PortalData
#'
#' @description Check either Zenodo or GitHub for the version and download link
#'   for PortalData.
#'
#' @param from_zenodo logical; if `TRUE`, get info from Zenodo, otherwise GitHub
#' @param halt_on_error logical; if `FALSE`, return NULL on errors, otherwise
#'   whatever got returned (could be an error or warning)
#' @return A data.frame with two columns, `version` (string with the version #) and
#'   `zipball_url` (download URLs for the corresponding zipped release).
#'
#' @export
get_data_versions <- function(from_zenodo = TRUE, halt_on_error = FALSE)
{
  releases <- tryCatch(
    {
      if (from_zenodo)
      {
        get_zenodo_latest_release()
      } else {
        get_github_releases()
      }
    },
    error = function(e) {
      if (halt_on_error) {
        stop(e)
      } else {
        e
      }
    },
    warning = function(w) w
  )
  if (!is.data.frame(releases))
  {
    return(NULL)
  }
  return(releases)
}

#' @title get Zenodo download link for PortalData
#'
#' @description Check Zenodo for the link and version for the latest release of
#'   PortalData.
#'
#' @return A data.frame with two columns, `version` (string with the version #) and
#'   `zipball_url` (download URLs for the corresponding zipped release).
#'
#' @noRd
get_zenodo_latest_release <- function()
{
  # Try and parse the download link from Zenodo
  resp <- httr::GET("https://zenodo.org/record/1215988")
  if (httr::http_type(resp) != "text/html") # check for errors
  {
    stop("Zenodo response was not in text format", call. = FALSE)
  }
  page_content <- httr::content(resp, "text")
  match_pos <- regexec("https://zenodo.org/api/files/[0-9a-f\\-]+/weecology/[0-9a-zA-z.\\-]+zip",
                       page_content)
  match_text <- regmatches(page_content, match_pos)

  if (length(match_text) != 1 || length(match_text[[1]]) <= 0)
  {
    stop("Wasn't able to parse Zenodo for the download link.", call. = FALSE)
  }
  zip_download_path <- match_text[[1]][1]

  pattern <- "([0-9]+\\.[0-9]+\\.[0-9]+)\\.zip"
  match_pos <- regexec(pattern, zip_download_path)
  match_text <- regmatches(zip_download_path, match_pos)
  if (length(match_text) != 1 || length(match_text[[1]]) <= 0)
  {
    stop("Wasn't able to parse Zenodo for the version.", call. = FALSE)
  }

  return(data.frame(version = match_text[[1]][2],
                    zipball_url = zip_download_path,
                    stringsAsFactors = FALSE))
}

#' @title get GitHub Release Info for PortalData
#'
#' @description Use the GitHub API to get info about the releases of the
#'   PortalData repo.
#'
#' @return A data.frame with two columns, `version` (string with the version #) and
#'   `zipball_url` (download URLs for the corresponding zipped release).
#'
#' @noRd
get_github_releases <- function()
{
  pat <- Sys.getenv("GITHUB_PAT")
  if (identical(pat, ""))
  {
    github_auth <- NULL
  } else {
    github_auth <- httr::authenticate(pat, "x-oauth-basic", "basic")
  }

  ## try to get links to all versions from GitHub
  releases <- data.frame(tag_name = character(),
                         zipball_url = character())
  match_text <- "next"
  page_idx <- 1

  # keep getting info until no more `next` pages
  while (match_text == "next")
  {
    github_path <- paste0("https://api.github.com/repos/weecology/PortalData/releases?page=", page_idx)
    resp <- httr::GET(github_path, github_auth)

    # check if problems with retrieving info
    if (httr::headers(resp)$status == "403 Forbidden" &&
        httr::headers(resp)$"x-ratelimit-remaining" == "0") # rate limit exceeded
    {
      stop("Exceeded GitHub rate limit, please try again in an hour or consult the documentation for details.\n",
           "https://developer.github.com/v3/#rate-limiting")
    } else if (httr::http_type(resp) != "application/json") # check for errors
    {
      stop("GitHub response was not in JSON format", call. = FALSE)
    }

    # extract release info
    releases <- rbind(releases,
                      jsonlite::fromJSON(httr::content(resp, "text"))[, c("tag_name", "zipball_url")])

    # update page count
    page_idx <- page_idx + 1

    # check for next page of results
    link_str <- httr::headers(resp)$link
    match_pos <- regexec("^<.+>; rel=\"([a-z]+)\", <.+>; rel=\"([a-z]+)\"$", link_str)
    match_text <- regmatches(link_str, match_pos)[[1]][2]
  }
  names(releases) <- c("version", "zipball_url")
  return(releases)
}

#' @title Check for latest version of data files
#' @description Check the latest version against the data that exists on
#'   the GitHub repo
#' @param base_folder Folder in which data will be checked
#'
#' @return bool TRUE if there is a newer version of the data online
#'
#' @export
check_for_newer_data <- function(base_folder = "~")
{
  # first see if the folder for the data files exist
  tryCatch(base_path <- file.path(normalizePath(base_folder, mustWork = TRUE), "PortalData"),
           error = function(e) stop("Unable to use the specified path: ", base_folder, call. = FALSE),
           warning = function(w) w)

  # check for `version.txt``
  version_file <- file.path(base_path, "version.txt")
  if (!file.exists(version_file)) # old version of data is missing this metadata file
    return(TRUE)

  pattern <- "([0-9]+)\\.([0-9]+)\\.([0-9]+)"
  version_str <- as.character(read.table(version_file)[1, 1])
  local_version <- c(as.numeric(gsub(pattern, "\\1", version_str)),
                     as.numeric(gsub(pattern, "\\2", version_str)),
                     as.numeric(gsub(pattern, "\\3", version_str)))

  # pull github version from list of releases
  github_releases <- get_data_versions(from_zenodo = FALSE)
  if (is.null(github_releases)) # if unable to access github releases
  {
    return(FALSE)
  }
  github_version_str <- github_releases$version[1]
  github_version <- c(as.numeric(gsub(pattern, "\\1", github_version_str)),
                      as.numeric(gsub(pattern, "\\2", github_version_str)),
                      as.numeric(gsub(pattern, "\\3", github_version_str)))

  if (github_version[1] > local_version[1])
    return(TRUE)

  if (github_version[1] == local_version[1] &&
      github_version[2] > local_version[2])
    return(TRUE)

  if (github_version[1] == local_version[1] &&
      github_version[2] == local_version[2] &&
      github_version[3] > local_version[3])
    return(TRUE)

  return(FALSE)
}

#' @name load_data
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


#' @rdname load_data
#' @description \code{\link{load_data}} loads the rodent data files
#'
#' @param clean logical, load only QA/QC rodent data (TRUE) or all data (FALSE)
#'
#' @return \code{\link{load_data}} returns a list of 5 dataframes:
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
#' portal_data <- load_data("repo")
#' }
#' @export
#'
load_data <- function(path = "~", download_if_missing = TRUE, clean = TRUE)
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

#' @rdname load_data
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

#' @rdname load_data
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

#' @rdname load_data
#' @description \code{\link{load_trapping_data}} loads just the rodent trapping files
#'
#' @inheritParams load_data
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
