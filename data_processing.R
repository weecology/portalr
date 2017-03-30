library(RCurl)

loadData = function(path) {
  if (path == 'repo') {
    rodent_data = read.csv(
      text = getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv"
      ),
      na.strings = c(""),
      colClasses = c('tag' = 'character'),
      stringsAsFactors = FALSE
    )
    species_table = read.csv(
      text = getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_species.csv"
      ),
      na.strings = c("")
    )
    trapping_table = read.csv(
      text = getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv"
      )
    )
    newmoons_table = read.csv(
      text = getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv"
      )
    )
    plots_table = read.csv(
      text = getURL(
        "https://raw.githubusercontent.com/weecology/PortalData/master/SiteandMethods/new_Portal_plots.csv"
      )
    )
  } else {
    rodent_data = read.csv(
      paste(path, "PortalData/Rodents/Portal_rodent.csv",
            sep = ""),
      na.strings = c(""),
      colClasses = c('tag' = 'character'),
      stringsAsFactors = FALSE
    )
    species_table = read.csv(
      paste(
        path,
        "PortalData/Rodents/Portal_rodent_species.csv",
        sep = ""
      ),
      na.strings = c("")
    )
    trapping = read.csv(paste(
      path,
      "PortalData/Rodents/Portal_rodent_trapping.csv",
      sep = ""
    ))
    newmoons = read.csv(paste(path, "PortalData/Rodents/moon_dates.csv",
                              sep = ""))
    plots = read.csv(paste(
      path,
      "PortalData/SiteandMethods/new_Portal_plots.csv",
      sep = ""
    ))
  }
  colnames(species_table)[1] = "species"
  return(list(rodent_data, species_table, trapping, newmoons, plots))
}

remove_suspect_entries = function(rodent_data) {
  #Remove suspect trapping periods
  rodent_data = rodent_data[rodent_data$period > 0,]
  
  #Remove unknown plots
  rodent_data = rodent_data[!is.na(rodent_data$plot),]
}

process_unknownsp = function(rodent_data, species_table, unknowns) {
  if (unknowns == F) {
    rodent_species_merge = rodent_data %>%
      left_join(species_table, rodent_data, by = "species") %>%
      filter(Rodent == 1, Unidentified == 0, Census.Target == 1)
  }
  
  #Rename all unknowns and non-target rodents to "Other"
  if (unknowns == T) {
    rodent_species_merge =
      left_join(species_table, rodent_data, by = "species") %>%
      filter(Rodent == 1) %>%
      mutate(species = replace(species, Unidentified == 1, "Other")) %>%
      mutate(species = replace(species, Census.Target == 0, "Other"))
  }
  return(rodent_species_merge)
}

process_granivores = function(rodent_species_merge, type) {
  if (type %in% c("Granivores", "granivores")) {
    granivore_data = rodent_species_merge %>%
      filter(Granivore == 1)
    return(granivore_data)
  } else {
    return(rodent_species_merge)
  }
}