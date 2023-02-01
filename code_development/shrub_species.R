data_tables <- load_plant_data()
species <- data_tables$species_table
oldtransect_data <- data_tables$oldtransect_data %>%
    dplyr::left_join(species) %>%
    dplyr::filter(community=="Shrub") %>%
    dplyr::mutate(species=replace(species, species=="acac greg", "mimo acul"),
                  species=replace(species, species=="lyci torr", "lyci ande"),
                  species=replace(species, species=="pros sp", "pros glan"),
                  species=replace(species, species=="opun sp", "opun basi")) %>%
    dplyr::group_by(year,plot,species) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(cover = n/1000)

hits = c(seq(100,25000,100), seq(47000,71900,100))

transect_data <- data_tables$transect_data %>%
    dplyr::left_join(species) %>%
    dplyr::filter(community=="Shrub") %>%
    dplyr::mutate(species=replace(species, species=="acac greg", "mimo acul"),
                  species=replace(species, species=="lyci torr", "lyci ande"),
                  species=replace(species, species=="pros sp", "pros glan"),
                  species=replace(species, species=="opun sp", "opun basi")) %>%
    dplyr::mutate(start=10*start,stop=10*stop) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(counts=sum(hits>=start & hits<=stop)) %>%
    dplyr::group_by(year,plot,species) %>%
    dplyr::summarise(n = sum(counts)) %>%
    dplyr::mutate(cover = n/1000) %>%
    dplyr::filter(cover!=0, year!=2015)

shrub_data <- rbind(oldtransect_data,transect_data)

species_data <- shrub_data %>%
             dplyr::group_by(year,species) %>%
             dplyr::summarise(
                 dplyr::across(n, sum, na.rm = T),
                 dplyr::across(cover, mean, na.rm = T))
site_data <- species_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
        dplyr::across(n, sum, na.rm = T),
        dplyr::across(cover, sum, na.rm = T))

library(ggplot2)
ggplot(site_data,aes(x=year,y=cover))+geom_point()+geom_line()+xlab("Year")+
    ylab("Shrub Cover")+ggtitle("Site-Level Shrub Cover from 1989-2021")+
    theme(plot.title = element_text(hjust = 0.5))

ggplot(species_data,aes(x=year,y=cover,group=species,col=species))+geom_line()+
    xlab("Year")+ylab("Shrub Cover")+ggtitle("Site-Level Shrub Cover by Species")+
    theme(plot.title = element_text(hjust = 0.5))
