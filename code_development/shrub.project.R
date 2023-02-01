shiny::runGitHub('portal-explorer', 'weecology', ref = 'main', subdir = 'app')

install.packages("rgl")
install.packages("shiny")
install.packages("shinythemes")
install.packages("shinyWidgets")
install.packages("dplyr")
library('dplyr')
install.packages("ggplot2")
install.packages("leaflet")
install.packages("sf")
install.packages("remotes")
library("tidyr")
install.packages("ggplot2")
library("ggplot2")

#started a new session here
options(rgl.useNULL= TRUE)
library("rgl")
install.packages("rgl")

install.packages("portalr")
library("portalr")

#load data tables
data_tables = load_plant_data("repo")

oldtransect_data = data_tables$oldtransect_data
transect_data = data_tables$transect_data

#get species names to align
oldtransect_data$species = as.factor(oldtransect_data$species)
levels(oldtransect_data$species)

transect_data$species = as.factor(transect_data$species)
levels(transect_data$species)

oldtransect_data=filter(oldtransect_data, species =="acac cons"|species=="acac greg"|
         species=="atri acan"|species=="carl line"|species== "cass bauh"|species== "comm erec"|
         species=="crot cory"|species=="cyli fulg"|species=="cyli sp"|species== "ephe trif"|
         species=="euro lana"|species=="flou cern"|species== "guti saro"|species== "hapl spin"|
         species== "hapl tenu"|species=="lyci ande"|species== "lyci torr"|species== "mach tanr"|
         species=="mimo acul"|species=="opun sp"|species=="part inca"|species=="pros glan"|
         species=="sola elea"|species== "spha hast"|species=="spha inca"|species=="spha laxa"|
         species=="tetr coul"|species=="unkn"|species=="xant spin"|species=="yucc elat"|
         species=="zinn gran"|species=="zinn pumi")

transect_data  = filter(transect_data,species!="unkn")
transect_data = filter(transect_data,year!=2015)

# REORGANIZING OLD DATA BY SUMMING ROWS #
old.data = oldtransect_data%>%
  count(year,plot,species)

old.data = rename(old.data,cover=n)

# FITTING NEW DATA TO OLD METHOD #
hits = seq(100,25000,100)
hits
start = 6588
stop = 6755
counts=sum(hits>=start & hits<=stop)
counts

new.data = transect_data%>%
  rowwise()|>
  mutate(counts=sum(hits>=start & hits<=stop))|>
  group_by(year,plot,species)|>
  summarise(cover=sum(counts))

#getting rid of 0 values
new.data=filter(new.data,cover!=0)

# stacking old and new data frame together
shrub.data = rbind(old.data,new.data)

#just cover sum and year
annual.cover=shrub.data%>%
  group_by(year)|>
  summarise_at(vars(cover),list(cover=sum))

#graph site level shrub cover
ggplot(annual.cover,aes(x=year,y=cover))+geom_point()+geom_line()+xlab("Year")+
  ylab("Shrub Cover")+ggtitle("Site-Level Shrub Cover from 1989-2021")+
  theme(plot.title = element_text(hjust = 0.5))

species.cover=shrub.data%>%
  group_by(year,species)|>
  summarise_at(vars(cover),list(cover=sum))

#graph site level cover by species
ggplot(species.cover,aes(x=year,y=cover,group=species,col=species))+geom_line()+
  xlab("Year")+ylab("Shrub Cover")+ggtitle("Site-Level Shrub Cover by Species")+
  theme(plot.title = element_text(hjust = 0.5))


# ^this still can't be right #


# CHECKING WITH METHODOLOGY SWITCH #
# PLOT 17 #

#isolating desired columns and fixing dates#
plot.17 = Plot17_transects_2012[,c(1,4,5)]
plot.17$Date[plot.17$Date=="10.13.12"]=2012

#filtering out unwanted species #
plot.17$species=as.factor(plot.17$species)
levels(plot.17$species)
#few species match the whole species list

plot.17=filter(plot.17,species=="acaccons"|species=="atriacan"|species=="ephetrif"|
                 species=="eurolana"|species=="floucern"|species=="yuccelat")

#filtering out species that don't match 17
shrub.data.17= filter(shrub.data,species=="acac cons"|species=="atri acan"|species=="ephe trif"|
                        species=="euro lana"|species=="flou cern"|species=="yucc elat",plot==17)

shrub.data.17$species = as.factor(shrub.data.17$species)
levels(shrub.data.17$species)

#year and cover of 17
annual.cover.adjusted = shrub.data.17%>%
  group_by(year)|>
  summarise_at(vars(cover),list(cover=sum))

ggplot(annual.cover.adjusted,aes(x=year,y=cover))+geom_point()+geom_line()

years = c(1989,1992,1995,1998,2001,2004,2009,2012,2016,2017,2018,2019,2021)
covers = c(13,103,67,93,122,75,164,127,27,32,26,21,33)
df = data.frame(years,covers)

ggplot(df,aes(x=years,y=covers))+geom_point()+geom_line()+ggtitle("Plot 17")+
  theme(plot.title = element_text(hjust = 0.5))


# PLOT 22 #

#isolating desired columns and fixing dates#
plot.22= Plot22_transects_2012[,c(1,4,5)]
plot.22$Date[plot.22$Date=="10.13.12"]=2012

plot.22$species=as.factor(plot.22$species)
levels(plot.22$species)

#only keeping species that match the original
plot.22=filter(plot.22,species=="acaccons"|species=="atriacan"|species=="ephetrif"|
                 species=="eurolana"|species=="floucern"|species=="yuccelat")

#making original dataset match plot 22
shrub.data.22= filter(shrub.data,species=="acac cons"|species=="atri acan"|species=="ephe trif"|
                        species=="euro lana"|species=="flou cern"|species=="yucc elat",plot==22)

annual.cover.adjusted = shrub.data.22%>%
  group_by(year)|>
  summarise_at(vars(cover),list(cover=sum))

ggplot(annual.cover.adjusted,aes(x=year,y=cover))+geom_line()+geom_point()

Year = c(1989,1992,1995,1998,2001,2004,2009,2012,2016,2017,2018,2019,2021)
Cover = c(28,77,45,61,80,45,136,117,34,44,30,23,44)
df = data.frame(Year,Cover)

ggplot(df,aes(x=Year,y=Cover))+geom_point()+geom_line()+ggtitle("Plot 22")+
  theme(plot.title = element_text(hjust = 0.5))


## TESTING INDIVIDUAL PLOTS ##

plot.cover=shrub.data%>%
  group_by(year,plot)|>
  summarise_at(vars(cover),list(cover=sum))

ggplot(plot.cover,aes(x=year,y=cover))+geom_line(aes(col=plot))+
  xlab("Year")+ylab("Shrub Cover")+ggtitle("Site-Level Shrub Cover by Plot")+
  theme(plot.title = element_text(hjust = 0.5))+facet_wrap(~plot)



# WEATHER DATA #

weatherdata = weather("Monthly")
weatherdata = weatherdata[-c(1:108,505:514),]

weatherdata = weatherdata%>%
  rowwise()|>
  group_by(year)|>
  mutate(avg_cool_prcp = mean(cool_precip))

annual.coolprecip =weatherdata[,c(1,10)]

annual.coolprecip=annual.coolprecip%>%
  group_by(year)|>
  summarise_at(vars(cool_precip),list(cool_precip=sum))

#plot annual cool precip by year #
ggplot(annual.coolprecip,aes(x=year,y=cool_precip))+geom_line()+xlab("Year")+ylab("Cool Precipitation")

total.cool.prcp = c(706.65687, 1032.42215, 483.11976, 1064.13622, 570.35563, 578.95070, 229.78985, 434.10552,
                    674.75710, 239.90199, 775.03841, 33.42365)
years = c(1989,1992,1995,1998,2001,2004,2009,2016,2017,2018,2019,2021)
data = data.frame(years, total.cool.prcp)
ggplot(data, aes(x=years, y=total.cool.prcp))+geom_line()+geom_point()


# is shrubification still a function of winter rains?
#plotting shrub cover by annual cool precipitation
total.covers = c(1934, 5418, 2832, 2970, 3199, 2956, 3640, 1227, 1271, 992, 893, 1167)
cover.precipitation = data.frame(total.cool.prcp, total.covers)
m1 = lm(total.covers~ total.cool.prcp, data= cover.precipitation)
summary(m1)

coef(m1)
beta1 <- coef(m1)
intercept = beta1[1]
slope = beta1[2]

plot(total.covers ~total.cool.prcp, data = cover.precipitation, xlab = "Total cool precipitation",
     ylab = "Site-level shrub cover")
abline(intercept, slope, col='red')


avg.cool.prcp = c(58.88807, 86.03518, 40.25998, 88.67802, 47.529636, 
                  48.245892, 19.149155, 36.175460, 56.229759, 19.991832, 64.586534, 2.785304)
cover.precipitation = cbind(cover.precipitation,avg.cool.prcp)
ggplot(cover.precipitation, aes(x=years, y=avg.cool.prcp))+geom_line()       

# is shrubification still a function of AVERAGE winter rains?
#plotting shrub cover by avg annual cool precipitation
m2 = lm(total.covers~ avg.cool.prcp, data= cover.precipitation)
summary(m2)

coef(m2)
beta2 <- coef(m2)
intercept = beta2[1]
slope = beta2[2]

plot(total.covers ~avg.cool.prcp, data = cover.precipitation, xlab = "Average cool precipitation",
     ylab = "Site-level shrub cover")
abline(intercept, slope, col='red')

weatherdata = weatherdata%>%
  rowwise()|>
  group_by(year)|>
  mutate(avg_ppt_anomaly = mean(anomaly_ppt))

ppt.anoms = c(0.4958266, 1.5645124, 0.6032139, 0.5002579, 0.7066728, 1.1824484,
              0.6769451, 0.9040523, 0.4870954, 0.6886818, 1.0917760, 0.5118883)
cover.precipitation = cbind(cover.precipitation,ppt.anoms)
m3 = lm(total.covers~ ppt.anoms, data= cover.precipitation)
summary(m3)

coef(m3)
beta3 <- coef(m3)
intercept = beta3[1]
slope = beta3[2]

plot(total.covers ~ppt.anoms, data = cover.precipitation, xlab = "Precipitation Deviations",
     ylab = "Site-level shrub cover")
abline(intercept, slope, col='red')

plot(ppt.anoms~years, xlab = "Year",ylab = "Precipitation Anomalies")
ggplot(cover.precipitation, aes(x= years, y= ppt.anoms))+labs(x="Year",y="Precipitation Anomalies")+
  geom_point()

cover.precipitation = cbind(cover.precipitation,years)
cover.precipitation = rename(cover.precipitation,cover=total.covers)


#creating a new lm with 1 year lag
cover.precipitation = cover.precipitation%>%
  mutate(lag1_cover = lag(cover, n=1, order_by = years))

m4 = lm(lag1_cover~ avg.cool.prcp, data= cover.precipitation)
beta4 = coef(m4)
intercept = beta4[1]
slope = beta4[2]

plot(lag1_cover ~avg.cool.prcp, data = cover.precipitation, xlab = "Cool precip (lag 1)",
     ylab = "Site-level shrub cover")
abline(intercept, slope, col='red')

# 5 year lag
cover.precipitation = cover.precipitation%>%
  mutate(lag5_cover = lag(cover,n=5,order_by = years))

m5 = lm(lag5_cover~ avg.cool.prcp, data= cover.precipitation)
beta5 = coef(m5)
intercept = beta5[1]
slope = beta5[2]

plot(lag5_cover ~avg.cool.prcp, data = cover.precipitation, xlab = "Cool precip (lag 5)",
     ylab = "Site-level shrub cover")
abline(intercept, slope, col='red')


#### daily table

daily_table = weather("daily")
weather(
  level = "daily",
  fill = FALSE,
  horizon = 30,
  temperature_limit = 4,
  path = get_default_data_path()
)

daily_table = daily_table[,-7]
daily_table = daily_table[-c(1:3288),]

daily_table[is.na(daily_table)] = 0

### COOL PRECIP ANOMALIES ###

##calculating "normals"
daily_table%>%
  group_by(month)|>
  drop_na(precipitation)|>
  summarise(cool_normal = mean(precipitation[mintemp<=4]))

#vector of the normal values repeated every year
vector = c(0.3965944,0.4520296,0.066476,0.1202957,0,0,0,0,0,0.4001017,0.6352744,0.5811777)
cool_normal = rep(vector, 33)

weatherdata = cbind(weatherdata, cool_normal)

#new data frame summary to bind to weather data table
cool_precip = daily_table%>%
  group_by(year,month)|>
  mutate(cool.precip = mean(precipitation[mintemp<=4],rm.na=TRUE))|>
  summarise_at(vars(cool.precip),list(cool.precip=mean))|>
  filter(year!=2022)

weatherdata = cbind(weatherdata,cool_precip)
weatherdata = weatherdata[,-c(19,20)]
weatherdata = rename(weatherdata,cool_normal=...18)
weatherdata = rename(weatherdata,year=year...1)
weatherdata = rename(weatherdata,month=month...2)
#add column for cool precip anomalies
weatherdata = weatherdata %>%
  mutate(cool_anom= cool.precip/cool_normal)
weatherdata$cool_anom[is.nan(weatherdata$cool_anom)] =0

weather1 = weatherdata%>%
  drop_na(cool_anom)

weather1%>%group_by(year)|>summarise(cool_anom=mean(cool_anom))

cool.anomalies = c(0.4492666, 1.364162, 0.1472316, 2.131772, 0.4999733, 3.393664,
                   0.8716858, 0.3804507, 0.4684261, 1.746281, 3.069127, 0.06946837)

cover.precipitation = cbind(cover.precipitation,cool.anomalies)


## new weather table going back to 1984 for 5 year rain lags ##
weath = weather("Monthly")
weath = weath[-c(1:48),]
weath = filter(weath,year!=2022)

weath = weath%>%
  rowwise()|>
  group_by(year)|>
  mutate(avg_cool_prcp = mean(cool_precip))

weath = weath[,-c(5,8,15)]
weath = weath[,-c(6)]

weath = weath%>%
  rowwise()|>
  group_by(year)|>
  mutate(avg_ppt_anomaly = mean(anomaly_ppt))

c_normal = rep(vector, 38)
weath = cbind(weath,c_normal)
weath=rename(weath, c_normal = ...14)

daily = weather("daily")
daily = daily[-c(1:1461),]
daily = filter(daily,year!=2022)

#new column for cool precip
c_precip = daily%>%
  group_by(year,month)|>
  mutate(cool.precip = mean(precipitation[mintemp<=4],rm.na=TRUE))|>
  summarise_at(vars(cool.precip),list(cool.precip=mean))

#add avg.cool precip and cool precip normals
weath = cbind(weath,c_precip$cool.precip)
weath=rename(weath, c_precip=...15)


#make column for anomalies
weath = weath %>%
  mutate(c_anom= c_precip/c_normal)

weath$c_anom[is.nan(weath$c_anom)] =0

#creating new data frame by binding vectors
year_ = c(1984:2021)

avg_c_precip = c(59.81183, 73.20509, 56.30735, 39.45230, 41.75160, 58.88807, 32.10142, 76.85390, 86.03518,
                 49.89422, 49.229121, 40.25998, 8.68702, 65.83152, 88.678018, 23.931375, 11.289253, 47.529636, 23.793738,
                 4.956794, 48.245892, 65.726396, 10.455655, 42.753507, 26.567086, 19.149155, 121.674592, 21.632515,
                 70.87005, 31.37184, 35.42807, 18.88612, 36.175460, 56.229759, 19.991832, 64.586534, 92.960522, 2.785304)
weath = weath%>%
  drop_na(c_anom)
weath%>%
  group_by(year)|>
  summarise(cool_anom=mean(c_anom))

c_anomalies = c(1.283425, 9.289134, 0.5424866, 2.568156,  0.3264278, 0.4492666,
                0.6445511, 1.187643, 1.364162, 0.460853, 1.701666, 0.1472316, 0.5352829,
                0.7091638, 2.131772, 0.4697619, 0.5385805, 0.4999733, 0.05822298,  0.2761388,
                3.393664,0.8767658, 0.6419783, 0.9839854, 0.09326431, 0.8716858, 0.8716858,0.3405747,
                0.8502964, 0.834095,0.07217945,0.1232316, 0.3804507, 0.4684261,1.746281,3.069127, 0.23032,
                0.06946837)
cover_ = c(NA,NA,NA,NA,NA,1934,NA,NA,5418,NA,NA,2832,NA,NA,2970,NA,NA,3199,NA,NA,2956,NA,NA,NA,NA,3640,NA,NA,NA,
           NA,NA,NA,1227,1271,992,893,NA,1167)

#group columns into a dataframe
weather_and_shrubs = data.frame(year_,avg_c_precip,c_anomalies,cover_)

#creating columns for lag
weather_and_shrubs = weather_and_shrubs%>%
  mutate(lag1_cover = lag(cover_, n=1, order_by = year_))
weather_and_shrubs = weather_and_shrubs%>%
  mutate(lag5_cover = lag(cover_, n=5, order_by = year_))

#linear modeling=
m8 = lm(lag1_cover~ avg_c_precip, data= weather_and_shrubs)
beta8 = coef(m8)
intercept = beta8[1]
slope = beta8[2]
plot(lag1_cover ~avg_c_precip, data = weather_and_shrubs, xlab = "Cool precip (lag 1)",
     ylab = "Site-level shrub cover")
abline(intercept, slope, col='red')

m9 = lm(lag5_cover~ avg_c_precip, data= weather_and_shrubs)
beta9 = coef(m9)
intercept = beta9[1]
slope = beta9[2]
plot(lag5_cover ~avg_c_precip, data = weather_and_shrubs, xlab = "Cool precip (lag 5)",
     ylab = "Site-level shrub cover")
abline(intercept, slope, col='red')
coef(m9)
summary(m9)


#### RANGELAND ANALYSIS PLATFORM ESTIMATES ##

RAP = RAP_site_estimate

ggplot(RAP, aes(x= Year, y=Averages))+geom_line()+
  ylab("Percent Cover")+ggtitle("RAP Estimate of Site Level Shrub Cover")+
  theme(plot.title = element_text(hjust = 0.5))


## converting 'hits' into percent cover to agree with RAP estimate ##
#old.data = oldtransect_data%>%
#count(year,plot,species)

#old.data=rename(old.data,cover=n)

old.data.1 = oldtransect_data%>%
  count(year,plot,species)

old.data.1=rename(old.data.1,cover=n)
old.data.1 = old.data.1%>%
  mutate(percent_cover = cover/2.5)
old.data.1 = old.data.1[,-4]

#old code block:
##new.data = transect_data%>%
#rowwise()|>
#mutate(counts=sum(hits>=start & hits<=stop))|>
#group_by(year,plot,species)|>
#summarise(cover=sum(counts))

#new transects with percent cover column
new.data.1 = transect_data%>%
  rowwise()|>
  mutate(counts=sum(hits>=start & hits<=stop))|>
  group_by(year,plot,species)|>
  summarise(percent_cover=sum(counts)/2.5)

shrub.data.1=rbind(old.data.1,new.data.1)

#summary of year and shrub cover
annual.cover.1=shrub.data.1%>%
  group_by(year)|>
  summarise(ave_percent_cover= mean(percent_cover))

ggplot(annual.cover.1, aes(x=year,y=ave_percent_cover))+geom_line()+geom_point()
