################### A function to summarize monthly Portal rodent species abundance ################
###################
#level summarizes by "Plot", "Treatment", or "Site"
###################
#type uses all "Rodents" species or only "Granivores"
###################
#length uses "All" plots or only "Longterm" plots (plots that have had the same treatment over the entire time series)
###################
#unknowns either removes all individuals not identified to species (unknowns=F) or sums them in an additional column (unknowns=T)
###################
#incomplete either removes all data from incomplete trapping sessions (incomplete = F) or includes them (incomplete=T) 
#(note that if level="plot" and incomplete=T, NAs will be included in periods where trapping was incomplete)
###################
#shape returns data as a "crosstab" or "flat" list
###################
#time returns data using the complete "newmoon" numbers or the original "period" numbers
###################

library(dplyr)
library(tidyr)
source("data_processing.R")

abundance <- function(path = '~/', level="Site",type="Rodents",
                      length="all",unknowns=F,incomplete=F,
                      shape="crosstab",time="period") {

##########Get Data
  data_tables = loadData(path)
  rodents = data_tables[[1]]
  species = data_tables[[2]]
  trapping = data_tables[[3]]
  newmoons = data_tables[[4]]
  plots = data_tables[[5]]
  
##########Data cleanup --------------------------------
rodents = remove_suspect_entries(rodents)
rodents = process_unknownsp(rodents, species, unknowns)

###########Exclude non-granivores-----------------------
rodents = process_granivores(rodents, type)

###########Remove incomplete trapping sessions----------
rodents = remove_incomplete_censuses(trapping, rodents, incomplete)

###########Use only Long-term treatments --------------
rodents = filter_plots(rodents, length)

###########Summarise by Treatment ----------------------
if(level %in% c("Treatment","treatment")){
#Name plot treatments in each time period

  plots = filter_plots(plots, length)
  rodents = join_plots_to_rodents(rodents, plots)
# 
#   plots = plots %>% group_by(yr,plot) %>% 
#     select(yr,mo, plot,treatment)
#   rodents = left_join(rodents,plots, by=c("yr"="yr","mo"="mo","plot"="plot"))
  
abundances = rodents %>%
  mutate(species = factor(species)) %>% 
  group_by(period,treatment) %>%
  do(data.frame(x = table(.$species))) %>% 
  ungroup() %>% 
  select(period,treatment,species=x.Var1, abundance=x.Freq) 
}

##########Summarise by plot ----------------------------
if(level %in% c("Plot","plot")){
  if (length %in% c('Longterm', 'longterm')){
    trapping = trapping %>% 
      filter(plot %in% c(3,4,10,11,14,15,16,17,19,21,23))
  }
  #  reduce size of trapping table
  incompsampling = find_incomplete_censuses(trapping)
  trapping = filter(trapping, !Period %in% incompsampling$Period)
  
  abundances = right_join(rodents,trapping,by=c("period"="period","plot"="plot")) %>% 
  mutate(species = factor(species)) %>% 
  group_by(period,plot,Sampled) %>%                       
  do(data.frame(x = table(.$species))) %>%
  mutate(x.Freq=replace(x.Freq,Sampled==0,NA))  %>% #0->NA on untrapped plots
    ungroup() %>% 
  select(period,plot,species=x.Var1, abundance=x.Freq) 
}

##########Summarise site-wide --------------------------
if(level %in% c("Site","site")){

  abundances = rodents %>% 
  mutate(species = factor(species)) %>% 
  group_by(period) %>%
  do(data.frame(x = table(.$species))) %>% 
  ungroup() %>% 
  select(period,species=x.Var1, abundance=x.Freq) 
}

###########Switch to new moon number-----------------------
if(time %in% c("NewMoon","Newmoon","newmoon")){
  
  if(incomplete == T){
    abundances = left_join(newmoons,abundances,by=c("period"="period")) %>% filter(period <= max(period,na.rm=T)) %>% 
      select(-NewMoonDate,-period,-CensusDate)
  }
  
  if(incomplete == F){
  abundances = right_join(newmoons,abundances,by=c("period"="period")) %>% filter(period <= max(period,na.rm=T)) %>% 
    select(-NewMoonDate,-period,-CensusDate)
  }
}

##########Convert data to crosstab ----------------------
if(shape %in% c("Crosstab","crosstab")){
  
  abundances = abundances %>% 
    spread(species, abundance) %>%
    ungroup()
}


return(abundances)
}
