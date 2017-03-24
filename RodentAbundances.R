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

library(RCurl)
library(dplyr)
library(tidyr)

abundance <- function(level="Site",type="Rodents",length="all",unknowns=F,incomplete=F,shape="crosstab",time="period") {

##########Get Data
rodents=read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent.csv"),
                 na.strings=c(""), colClasses=c('tag'='character'), stringsAsFactors = FALSE)
species=read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_species.csv"),na.strings=c(""))
trapping=read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/Portal_rodent_trapping.csv"))
newmoons=read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/Rodents/moon_dates.csv"))
plots=read.csv(text=getURL("https://raw.githubusercontent.com/weecology/PortalData/master/SiteandMethods/new_Portal_plots.csv"), 
               stringsAsFactors = FALSE)
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
  incompsampling=trapping %>% filter(Sampled==0 ) %>% 
    filter(Period > 26) %>% distinct(Period)
  rodents = filter(rodents, !period %in% incompsampling$Period)
  #reduce size of trapping table (in case of plot-level summary)
  trapping = filter(trapping, !Period %in% incompsampling$Period)
  }

###########Use only Long-term treatments --------------
if(length %in% c("Longterm","longterm")) {
  rodents = rodents %>% filter(plot %in% c(3,4,10,11,14,15,16,17,19,21,23))
  }

###########Summarise by Treatment ----------------------
if(level %in% c("Treatment","treatment")){
#Name plot treatments in each time period
  if (length %in% c('Longterm', 'longterm')){
    plots = plots %>% 
      filter(plot %in% c(3,4,10,11,14,15,16,17,19,21,23))
  }
  plots = plots %>% group_by(yr,plot) %>% 
    select(yr,month, plot,treatment)
  rodents = left_join(rodents,plots, by=c("yr"="yr","mo"="month","plot"="plot"))
  
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
      filter(Plot %in% c(3,4,10,11,14,15,16,17,19,21,23))
  }
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

###########Switch to new moon number-----------------------
if(time %in% c("NewMoon","Newmoon","newmoon")){
  
  if(incomplete == T){
    abundances = left_join(newmoons,abundances,by=c("Period"="period")) %>% filter(Period <= max(Period,na.rm=T)) %>% 
      select(-NewMoonDate,-Period,-CensusDate)
  }
  
  if(incomplete == F){
  abundances = right_join(newmoons,abundances,by=c("Period"="period")) %>% filter(Period <= max(Period,na.rm=T)) %>% 
    select(-NewMoonDate,-Period,-CensusDate)
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
