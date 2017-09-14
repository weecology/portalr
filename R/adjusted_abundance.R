library(portalr)
library(dplyr)

source('R/RodentAbundancesAdjustable.R')

adjusted_abundance <- function(period_first, period_last, selected_treatment, length, dates){

  portalData = loadData('repo')
  rodent_data = portalData[[1]]
  species_table = portalData[[2]]
  trapping_table = portalData[[3]]
  newmoons_table = portalData[[4]]
  plots_table = portalData[[5]]

  rodents = abundance.adjustable(path = 'repo', level = 'treatment.adj', type = 'Rodents', length = length, unknowns= F, incomplete = T, shape = 'flat', time = 'period')

  rodents.table = rodents %>%
    dplyr::filter(treatment == selected_treatment)

  ### better way to guess how many plots of x treatment there 'should' be?
  usual.n <- ceiling(mean(as.numeric(rodents.table$n)))


  # without adjustments - to compare
  rodents.table.noadj = rodents.table %>%
    # dplyr::mutate(abundance.adjusted = as.integer(round(abundance.perplot *usual.n))) %>%
    dplyr::select(period, species, abundance, n) %>%
    tidyr::spread(species, abundance)

  write.csv(rodents.table.noadj, 'R/not_adjusted.csv')

  rodents.table = rodents.table %>%
    dplyr::mutate(abundance.adjusted = (round(abundance.perplot *usual.n))) %>%
  dplyr::select(period, species, abundance.adjusted) %>%
  tidyr::spread(species, abundance.adjusted) %>%
    dplyr::filter(period >= period_first, period <= period_last)
write.csv(rodents.table, 'R/adjusted.csv')

  if (dates == TRUE) {
dates = newmoons_table %>%
  dplyr::select(period, censusdate) %>%
  dplyr::filter(period %in% (as.vector(unique(rodents.table$period))))

rodents.table = rodents.table %>%
  dplyr::left_join(dates, by = c('period'))

  }
  return(rodents.table)
}



rodents <- adjusted_abundance(1, 436, 'exclosure', 'longterm', TRUE)

write.csv(rodents, 'rodents_test.csv')
