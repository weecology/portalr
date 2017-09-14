#### biomass through time on exclosure plots ####
# rough beginning (not following White 2004 & removing individuals)
# sum biomass
# by treatment
## per species
## per plt
## per period
## option for dates
# option to adjust based on # of plots sampled

library(portalr)
library(dplyr)
library(plyr)

adjusted_abundance <- function(period_first, period_last, selected_treatment, length, dates){

  portalData = loadData('repo')
  rodent_data = portalData[[1]]
  species_table = portalData[[2]]
  trapping_table = portalData[[3]]
  newmoons_table = portalData[[4]]
  plots_table = portalData[[5]]

  rodents = abundance(path = 'repo', level = 'Plot', type = 'Rodents', length = length, unknowns= F, incomplete = T, shape = 'flat', time = 'period')

  rodents <- filter(rodents, period >= period_first, period <= period_last)

  plots_trapped_treatment <- join(trapping_table, plots_table, by = c('year', 'month', 'plot'), type = 'left')

  rodents <- join(rodents, plots_trapped_treatment, by = c('period', 'plot'), type = 'left')

  rodents.treatment <- filter(rodents, treatment == selected_treatment)


  # per period you want abundance of each species, and how many plots were trapped that session
  rod.table <- matrix(data=NA, nrow = length(unique(rodents.treatment$period)), ncol = 3 + as.numeric(length(unique(rodents.treatment$species))))
  colnames(rod.table) <- c('period', 'nsampled', 'treatment', as.vector(unique(rodents.treatment$species)))
  rod.table[, 'period'] <- as.vector(unique(rodents.treatment$period))
  head(rod.table)

  for (p in 1:nrow(rod.table)) {
    period <- filter(rodents.treatment, period == rod.table[p, 'period'])

    splist <- as.vector(unique(period$species))

    for(sp in splist) {
      period.sp <- filter(period, species == sp)

      sp.sum <- as.numeric(sum(period.sp$abundance, na.rm = TRUE))

      rod.table[p,sp] <- sp.sum

    }

    period.plots <- filter(period, sampled == 1)
    nplots <- as.numeric(length(unique(period.plots$plot)))

    rod.table[p, 'nsampled'] <- nplots

    rod.table[p, 'treatment'] <- as.character(period[1, 'treatment'])

  }

  # adjust

  rod.table.adj <- rod.table

  ### better way to guess how many plots of x treatment there 'should' be?
  usual.n <- ceiling(mean(as.numeric(rod.table.adj[,'nsampled'])))

  for (n in 1:nrow(rod.table.adj)) {
    # this rounding method follows Erica's
    for (i in 4:24){
      rod.table.adj[n, i] <- round(as.numeric(rod.table.adj[n, i])/as.numeric(rod.table.adj[n, 'nsampled']) * usual.n)
    }
  }


  if (dates == TRUE) {
    dates <- newmoons_table[,c('period', 'censusdate')]
    dates <- filter(dates, period %in% (as.vector(unique(rodents.treatment$period))))
    censusdates <- as.vector(dates[,'censusdate'])

    rod.table.adj <- cbind(rod.table.adj, censusdates)
  }
  return(rod.table.adj)
}



# rodents <- create_rodent_table(1, 436, 'exclosure', 'longterm', TRUE)
