#' Download the PortalData repo
#' 
#' This downloads the latest portal data regardless if they are
#' actually updated or not.
#' 
#' TODO: incorperate data retriever into this when it's pointed at the github repo
#' @return None
download_observations = function(base_folder='~/'){
  base_folder=path.expand(base_folder)
  zip_download_path='https://github.com/weecology/PortalData/archive/master.zip'
  zip_download_dest=paste0(base_folder,'PortalData.zip')
  download.file(zip_download_path, zip_download_dest, quiet = TRUE)
  
  final_data_folder=file.path(base_folder,'PortalData')
  
  #Clear out the old files in the data folder without doing potentially dangerous
  #recursive deleting.
  if(file.exists(final_data_folder)) {
    old_files=list.files(final_data_folder, full.names = TRUE, all.files = TRUE, recursive = TRUE, include.dirs = FALSE)
    file.remove(old_files)
    old_dirs=list.files(final_data_folder, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
    file.remove(old_dirs)
  }
  
  #Github serves this up with the -master extension. Unzip and rename to remove that. 
  unzip(zip_download_dest, exdir=base_folder)
  Sys.sleep(10)
  file.remove(zip_download_dest)
  file.rename(file.path(base_folder,'PortalData-master'), final_data_folder)
}

#' Check if there are new rodent observations. This only checks the
#' Portal_rodent.csv file. If other things are updated this function 
#' will not show that there is new data available.
#' 
#' @return bool True if new observations are available
observations_are_new = function(base_folder='~/'){
  base_folder=path.expand(base_folder)
  md5_file = './Portal_rodent.md5'
  rodent_file= path.expand(paste0(base_folder,'PortalData/Rodents/Portal_rodent.csv'))
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
