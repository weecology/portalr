# WORK IN PROGRESS

# thoughts on output options
# 1. list of two elements: 
#  1. table of rodent-level information, where one column is a caputre history
#     across a series of possible capture events
#  2. table describing the capture events
#
rodent_level_data <- function(path = get_default_data_path(),
                              download_if_missing = TRUE, plots = "all",
                              type = "Rodents", unknowns = FALSE, 
                              clean = TRUE, quiet = FALSE,
                              start_id = 1000001){

  data <- load_rodent_data(path = path, 
                           download_if_missing = download_if_missing,
                           clean = clean, quiet = quiet)
  
  rodents <- clean_rodent_data(rodent_data = data$rodent_data, 
                               species_table = data$species_table,
                               fillweight = FALSE, type = type,
                               unknowns = unknowns) %>%
             clean_tags(start_id = start_id)

# the rodent data are now in and cleaned at the tag level
# need to construct the capture histories based on trapping data and tag
# data

  trapping <- filter_plots(data = data$trapping_table, plots = plots) %>%
              join_plots(plots_table = data$plots_table)

}


#
# attempts to determine if a tag is a PIT tag by its structure
#  requires species input for comparison, returns a logical vector
#
PIT_tag <- function(tag, species){
  tagchar <- nchar(tag)
  sp_in_tag <- substr(tag, 5, 6) == as.character(species)
  non_pit_letters <- grepl("[HIMNOPRSTUX]", tag)
  out <- rep(FALSE, length(tagchar))
  out[which(tagchar == 6 &! sp_in_tag &! non_pit_letters)] <- TRUE
  out
}


# uses general heuristics to clean the raw rodent observations
# returns a rodents data frame with a new id column 
#
# assumes 
#   all 0 and -1 tags were equivalent to NAs
#   individuals with note4 = TA and no tag were tagged but not read
#   PIT tags are not repeated among individuals
#   tag 1782: OT, OT, OL, OT should have been all OT
#   repeated tags across species were different individuals
#   either note5 = D or note2/note3 = * could be used to indicate changing
#    individuals with the same tag number
#   a gap of more than a year with no captures means that the reused number
#    is a new individual (i.e. no individuals go missing for > 1 year and then
#    are found again)
#   an individual can only be captured in one plot per period
#     if a tag number shows up in multiple plots in a period, 
#     the replicate from the more commonly visited plot for the individual is
#     kept. if it's a tie, the first observation is the one that's kept
#     if they are in the same plot (weird!) the first one is kept
#
clean_tags <- function(rodents, start_id = 1000001){

  # append the tag type
  rodents$PIT_tag <- PIT_tag(rodents$tag, rodents$species)

  # replace all 0 and -1 tags with NAs
  rodents$tag[which(rodents$tag == 0)] <- NA
  rodents$ltag[which(rodents$ltag == 0)] <- NA
  rodents$tag[which(rodents$tag == -1)] <- NA
  rodents$ltag[which(rodents$ltag == -1)] <- NA

  # remove any asterisks where tag is NA
  rodents$note2[which(rodents$note2 == "*" & is.na(rodents$tag))] <- NA
  rodents$note3[which(rodents$note3 == "*" & is.na(rodents$ltag))] <- NA

  # create ID column
  rodents$id <- rodents$tag

  # remove individuals with TA in note4 and no tag, as they were tagged but
  #  the tag wasn't read so it's not a new ind
  TAs <- which(rodents$note4 == "TA" & is.na(rodents$tag))
  rodents <- rodents[-TAs, ]

  # give an id to individuals with no id
  unks <- which(is.na(rodents$tag) == TRUE)
  nunks <- length(unks)
  end_id <- start_id + nunks - 1
  rodents$id[unks] <- start_id:end_id

  # disentangle non-unique ids
  # making the assumption that PIT tag values are not repeated among 
  # individuals

  # split based on species code within tag
  
  # trying to account for individuals that might have been mis-identified 
  #  using the ltag also, there's only one individual that looks mis-id'ed
  # assuming that tag 1782 OT, OT -> OL -> OT should have been all OT
  rodents$species[which(rodents$recordID == 19533)] <- "OT"

  rodents$id <- paste0(rodents$id, "_", rodents$species) 
    
  # for a given tag and species, there still could be multiple individuals
  # break a set apart based either on note5 == "D" ("dead") or 
  # note2/note3 == * (new individual) 

  uids <- unique(rodents$id)
  nuids <- length(uids)
  for(i in 1:nuids){
    rodents_i <- rodents[which(rodents$id == uids[i]),]
    ast <- which(rodents_i$note2 == "*")
    D <- which(rodents_i$note5 == "D")
    status <- rep(NA, NROW(rodents_i))
    status[ast] <- "*"
    status[D] <- "D"
    status[NROW(rodents_i)] <- "D"
    status[1] <- "*"
    makeD <- which(status == "*") - 1
    makeD <- makeD[which(makeD > 0)]
    status[makeD] <- "D"

    ind <- rep(NA, NROW(rodents_i))
    newind <- which(status == "*")
    nnewind <- length(newind)
    for(j in 1:nnewind){
      spot1 <- which(status == "*")[j]
      spot1[is.na(spot1)] <- 1
      spot2 <- which(status == "D")[j]
      spot2[is.na(spot2)] <- NROW(rodents_i)
      ind[spot1:spot2] <- j
    }
    
    rodents_i$id <- paste0(rodents_i$id, "_", ind)
    rodents[which(rodents$id == uids[i]),] <- rodents_i    
  }
 
  # or an extended time window break for non-PIT tags
  #  assuming that if there's a gap of more than 1 year between records
  #  for a non-PIT tag, then that means it's a new individual

  uids <- unique(rodents$id)
  nuids <- length(uids)
  sp <- rep(NA, nuids)
  PIT_tagYN <- rep(NA, nuids)
  longev <- rep(NA, nuids)
  for(i in 1:nuids){
    rodents_i <- rodents[which(rodents$id == uids[i]),]
    rid <- paste0(rodents_i$year, "-", rodents_i$month, "-", rodents_i$day)
    rid <- as.Date(rid)
    sp[i] <- rodents_i$species[1]
    PIT_tagYN[i] <- rodents_i$PIT_tag[1]
    longev[i] <- as.numeric(difftime(max(rid), min(rid), unit = "days"))/365
  }

  whichlong <- which(longev > 1 & !PIT_tagYN)
  nlong <- length(whichlong)
  for(i in 1:nlong){
    longuid <- uids[whichlong[i]]
    rodents_i <- rodents[which(rodents$id == longuid),]
    rid <- paste0(rodents_i$year, "-", rodents_i$month, "-", rodents_i$day)
    rid <- as.Date(rid)
    ddiff <- c(0, as.numeric(diff(rid))/365)
    ind <- rep(NA, NROW(rodents_i))
    newind <- c(1, which(ddiff > 1))
    nnewind <- length(newind)
    lastofind <- c(newind - 1, NROW(rodents_i))
    lastofind <- lastofind[lastofind > 0]
    for(j in 1:nnewind){
      spot1 <- newind[j]
      spot2 <- lastofind[j]
      ind[spot1:spot2] <- j
    }
    rodents_i$id <- paste0(rodents_i$id, "_", letters[ind])
    rodents[which(rodents$id == uids[whichlong[i]]),] <- rodents_i    
  }

  # remove duplicates from the same period
  #  choosing the duplicate that is from a more common plot for the individual
  #  if it's a tie, the first observation is the one that's kept
  # if they are in the same plot (weird!) the first one is kept

  rodents<-rodentsx
  uids <- unique(rodents$id)
  nuids <- length(uids)
  for(i in 1:nuids){
    rodents_i <- rodents[which(rodents$id == uids[i]),]
    pds <- table(rodents_i$period)
    if(any(pds > 1)){
      mpds <- which(pds > 1)
      plts <- table(rodents_i$plot)/NROW(rodents_i)
      for(j in 1:length(mpds)){
        pd_j <- names(mpds)[j]
        uplts <- rodents_i$plot[rodents_i$period == pd_j]
        to_keep <- uplts[which(uplts == names(plts)[which.max(plts)])]
        if(length(to_keep) == 0){
          to_keep <- uplts[1]
          to_drop <- which(rodents_i$period == pd_j & 
                           rodents_i$plot != to_keep)        
          to_drop_RID <- rodents_i$recordID[to_drop]
        } else if (length(to_keep) == 1){
          to_drop <- which(rodents_i$period == pd_j & 
                           rodents_i$plot != to_keep)        
          to_drop_RID <- rodents_i$recordID[to_drop]
        } else {
          to_drop <- which(rodents_i$period == pd_j)[-1]
          to_drop_RID <- rodents_i$recordID[to_drop]
        }
        rodents <- rodents[-which(rodents$recordID %in% to_drop_RID),]
      }
    }

  }

  rodents
}



