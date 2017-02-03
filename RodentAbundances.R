#A function to summarize monthly species abundance
#with options to summarize by "Plot", "Treatment", or "Site" (level)
#to use all plots or only plots that have had the same treatment over the entire time series
#length="all' or Longterm"
#to include all "Rodents" or only "Granivores" (type)
#to include unknowns in an additional column (unknowns=F/T)
#to include numbers from incomplete trapping sessions incomplete = F/T
#and to specify shape of data (shape="crosstab" or "flat")

library(dplyr)
library(tidyr)

abundance <- function(level="Site",type="Rodents",length="all",unknowns=F,incomplete=F,shape="crosstab") {

rodents=read.csv('~/PortalData/Rodents/Portal_rodent.csv', na.strings=c(""), colClasses=c('tag'='character'), stringsAsFactors = FALSE)
species=read.csv('~/PortalData/Rodents/Portal_rodent_species.csv',na.strings=c(""))
trapping=read.csv('~/PortalData/Rodents/Portal_rodent_trapping.csv',na.strings=c(""))
plots=read.csv('~/PortalData/SiteandMethods/Portal_plots.csv')
colnames(species)[1]="species"
  

##########Data cleanup --------------------------------

#Remove suspect trapping periods
rodents=rodents[rodents$period>0,]

#Remove unknown plots
rodents=rodents[!is.na(rodents$plot),]

######Remove bad species IDs and non-target animals, 
######or throw in unknown column-----------------------
#Just remove unknowns and non-target animals
if(unknowns == F) {
  rodents = rodents %>%             
  left_join(species,rodents, by="species") %>%
  filter(Rodent==1, Unidentified==0, Census.Target==1) 
}

#Rename all unknowns and non-target rodents to "Other"
if(unknowns == T) {
  rodents =             
  left_join(species,rodents, by="species") %>% 
  filter(Rodent==1) %>% 
  mutate(species=replace(species,Unidentified==1,"Other")) %>% 
  mutate(species=replace(species,Census.Target==0,"Other"))
}

###########Exclude non-granivores-----------------------
if(type %in% c("Granivores","granivores")){
  rodents = rodents %>%
    filter(Granivore==1)
}

###########Remove incomplete trapping sessions----------
if(incomplete == F) {
  incompsampling=trapping %>% filter(Sampled==0) %>% distinct(Period)
  rodents = filter(rodents, !period %in% incompsampling$Period)
  #reduce size of trapping table (in case of plot-level summary)
  trapping = filter(trapping, !Period %in% incompsampling$Period)
  }


###########Use only Long-term treatments --------------
if(length %in% c("Longterm","longterm")) {rodents = rodents %>% filter(plot %in% c(3,4,10,11,14,15,16,17,19,21,23))}

###########Summarise by Treatment ----------------------
if(level %in% c("Treatment","treatment")){
#Name plot treatments in each time period

rodents = left_join(rodents,plots)
  
abundances = rodents %>%
  mutate(species = factor(species)) %>% 
  group_by(period,treatment) %>%
  do(data.frame(x = table(.$species))) %>% 
  ungroup() %>% 
  select(period,treatment,species=x.Var1, abundance=x.Freq) 
}

##########Summarise by plot ----------------------------
if(level %in% c("Plot","plot")){

  abundances = right_join(rodents,trapping,by=c("period"="Period","plot"="Plot")) %>% 
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

##########Convert data to crosstab ----------------------
if(shape %in% c("Crosstab","crosstab")){
  
  abundances = abundances %>% 
    spread(species, abundance) %>%
    ungroup()
}


return(abundances)
}
