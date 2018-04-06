#' @importFrom graphics plot
#' @importFrom stats aggregate
#' @importFrom utils download.file read.csv unzip

#' @title Full Path
#' @description Return normalized path for all operating systems
#' @param ReferencePath a path to join with current working directory
#' @param BasePath Current working directory else path given
#'
#' @return Full path
#' @export
#' @examples
#' FullPath('PortalData/Rodents/Portal_rodent.csv')
#' FullPath('PortalData/Rodents/Portal_rodent.csv', '~')
FullPath <- function(ReferencePath, BasePath = getwd()) {
  BasePath = normalizePath(BasePath)
  Path = normalizePath(file.path(BasePath, ReferencePath), mustWork = FALSE)
  return (Path)
}

#' @title Download the PortalData repo
#'
#' @description This downloads the latest portal data regardless if they are
#'   actually updated or not.
#'   TODO: incorporate data retriever into this when it's pointed at the github repo
#' @param base_folder Folder into which data will be downloaded
#' @param release_only whether to download the "Release" version of the data. Set to FALSE
#'        to download the most up to date data.
#' @return None
#' @export
download_observations <- function(base_folder = '~', release_only = TRUE) {
  zip_download_path <- 'https://github.com/weecology/PortalData/archive/master.zip'
  if(release_only) # get download link to latest release
  {
    repo_url <- "https://api.github.com/repos/weecology/PortalData/releases/latest"
    pat <- Sys.getenv("GITHUB_PAT", unset = NA)
    if(!is.na(pat)) # use personal authentication token for GitHub if available
    {
      httr::GET(repo_url, httr::authenticate(pat, "x-oauth-basic", "basic")) -> resp
    } else {
      httr::GET(repo_url) -> resp
    }
    if (httr::http_type(resp) != "application/json") # check for errors
    {
      stop("GitHub response was not in json format", call. = FALSE)
    }
    zip_download_path <- httr::content(resp)$assets[[1]]$browser_download_url
  }
  zip_download_dest = FullPath('PortalData.zip', base_folder)
  download.file(zip_download_path, zip_download_dest, quiet = TRUE)

  final_data_folder = FullPath('PortalData', base_folder)

  #Clear out the old files in the data folder without doing potentially dangerous
  #recursive deleting.
  if (file.exists(final_data_folder)) {
    old_files = list.files(
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
  file.rename(FullPath(primary_data_folder, base_folder), final_data_folder)
}

#' @title Check for latest version of data files
#' @description Check the latest newmoonnumber against the data that exists on
#'   the web (as of now, this means against the latest release or latest commit
#'   on GitHub)
#' @param newmoons_table the read in data.frame from
#'   "PortalData/Rodents/moon_dates.csv"
#' @param release_only whether to check against the latest release or against
#'   the latest commit on GitHub (defaults to TRUE)
#' @return bool TRUE if there is a newer version of the data online
#' @export
#'
check_for_newer_data <- function(newmoons_table, release_only = TRUE)
{
  if(!release_only) # FALSE = use latest commit on GitHub
  {
    newmoons_table_link <- "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv"
  } else {
    release_url <- "https://github.com/weecology/PortalData/releases/latest/"
    pat <- Sys.getenv("GITHUB_PAT", unset = NA)
    if(!is.na(pat)) # use personal authentication token for GitHub if available
    {
      httr::GET(release_url, httr::authenticate(pat, "x-oauth-basic", "basic")) -> resp
    } else {
      httr::GET(release_url) -> resp
    }
    if (httr::http_type(resp) != "text/html") # check for errors
    {
      stop("GitHub response was not in text format", call. = FALSE)
    }
    page_content <- httr::content(resp, "text")
    match_pos <- regexec("weecology/PortalData/commit/([0-9a-f]+)", page_content)
    match_text <- regmatches(page_content, match_pos)
    if(length(match_text) != 1)
    {
      warning("Wasn't able to parse GitHub for the commit hash.")
      return(FALSE)
    }
    newmoons_table_link <- paste0("https://raw.githubusercontent.com/weecology/PortalData/",
                                  match_text[[1]][2],
                                  "/Rodents/moon_dates.csv")
  }

  online_newmoons_table <- read.csv(newmoons_table_link)
  return(max(online_newmoons_table$newmoonnumber, na.rm = TRUE) >
           max(newmoons_table$newmoonnumber, na.rm = TRUE))
}

#' #' @title Find new observations
#' #' @description Check if there are new rodent observations. This only checks the
#' #'   Portal_rodent.csv file. (If other data (non-rodent) are updated this function
#' #'   will not show that there is new data available.)
#' #' @param base_folder Folder into which data will be downloaded
#' #' @return bool True if new observations are available
#' #' @export
#' observations_are_new = function(base_folder = '~') {
#'   md5_file = './Portal_rodent.md5'
#'   rodent_file = FullPath('PortalData/Rodents/Portal_rodent.csv', base_folder)
#'   if (!file.exists(rodent_file))
#'     stop('Rodent observations not present. Please run download_observations()')
#'
#'   if (!file.exists(md5_file)) {
#'     old_md5 = ''
#'   } else {
#'     old_md5 = read.csv(md5_file, header = FALSE, stringsAsFactors = FALSE)$V1
#'   }
#'
#'   new_md5 = as.character(tools::md5sum(rodent_file))
#'
#'   if (old_md5 == new_md5) {
#'     return(FALSE)
#'   } else {
#'     sink(md5_file)
#'     writeLines(new_md5)
#'     sink()
#'     return(TRUE)
#'   }
#'
#' }
