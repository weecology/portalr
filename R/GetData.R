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
FullPath <- function( ReferencePath, BasePath=getwd()){
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
download_observations = function(base_folder='~'){
  zip_download_path='https://github.com/weecology/PortalData/archive/master.zip'
  zip_download_dest=FullPath('PortalData.zip', base_folder)
  download.file(zip_download_path, zip_download_dest, quiet = TRUE)

  final_data_folder=FullPath('PortalData', base_folder)

  #Clear out the old files in the data folder without doing potentially dangerous
  #recursive deleting.
  if(file.exists(final_data_folder)) {
    old_files=list.files(final_data_folder, full.names = TRUE, all.files = TRUE, recursive = TRUE, include.dirs = FALSE)
    file.remove(normalizePath(old_files))
    unlink(final_data_folder, recursive=TRUE)
  }

  #Github serves this up with the -master extension. Unzip and rename to remove that.
  primary_data_folder <- unzip(zip_download_dest, list = TRUE)$Name[1]
  unzip(zip_download_dest, exdir = base_folder)
  Sys.sleep(10)
  file.remove(zip_download_dest)
  file.rename(FullPath(primary_data_folder, base_folder), final_data_folder)
}

#' @title Find new observations
#' @description Check if there are new rodent observations. This only checks the
#'   Portal_rodent.csv file. (If other data (non-rodent) are updated this function
#'   will not show that there is new data available.)
#' @param base_folder Folder into which data will be downloaded
#' @return bool True if new observations are available
#' @export
observations_are_new = function(base_folder='~'){
  md5_file = './Portal_rodent.md5'
  rodent_file= FullPath('PortalData/Rodents/Portal_rodent.csv', base_folder)
  if(!file.exists(rodent_file)) stop('Rodent observations not present. Please run download_observations()')

  if(!file.exists(md5_file)) {
    old_md5=''
  } else {
    old_md5 = read.csv(md5_file, header = FALSE, stringsAsFactors = FALSE)$V1
  }

  new_md5 = as.character(tools::md5sum(rodent_file))

  if(old_md5 == new_md5){
    return(FALSE)
  } else {
    sink(md5_file)
    writeLines(new_md5)
    sink()
    return(TRUE)
  }

}
