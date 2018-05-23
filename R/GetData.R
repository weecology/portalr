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
#' @return None
#' @export
download_observations <- function(base_folder = '~')
{
  # Try and parse the download link from Zenodo
  resp <- httr::GET("https://zenodo.org/record/1215988")
  if (httr::http_type(resp) != "text/html") # check for errors
  {
    stop("Zenodo response was not in text format", call. = FALSE)
  }
  page_content <- httr::content(resp, "text")
  match_pos <- regexec("(https://zenodo.org/api/files/[0-9a-f\\-]+/weecology/[0-9a-zA-z.\\-]+)zip",
                       page_content)
  match_text <- regmatches(page_content, match_pos)

  if (length(match_text) != 1)
  {
    stop("Wasn't able to parse Zenodo for the download link.", call. = FALSE)
  }

  # Attemt to download the zip file
  zip_download_path <- match_text[[1]][1]
  zip_download_dest <- FullPath("PortalData.zip", tempdir())
  download.file(zip_download_path, zip_download_dest, quiet = TRUE)

  final_data_folder <- FullPath("PortalData", base_folder)

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
  file.rename(FullPath(primary_data_folder, base_folder), final_data_folder)
}
