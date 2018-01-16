#### Systematically testing abundance and biomass functions ####

#' level = site, treatment, plot
#' type = rodents, granivores
#' length = all, longterm
#' unknowns = true, false
#' incomplete = true, false
#' shape = flat, crosstab
#' time = period, newmoon
#' output = abundance, biomass
#' fillweight = true, false
#
#   a = get_rodent_data(path = 'repo', level = "Site", type = "Rodents",
#                       length = "all", unknowns = T, incomplete = T,
#                       shape = "crosstab", time = "period", output = "abundance", fillweight = TRUE)
# b = get_rodent_data(path = 'repo', level = "Site", type = "Rodents",
#                     length = "all", unknowns = T, incomplete = T,
#                     shape = "crosstab", time = "period", output = "biomass", fillweight = TRUE)
# c = get_rodent_data(path = 'repo', level = "Site", type = "Rodents",
#                     length = "all", unknowns = T, incomplete = T,
#                     shape = "crosstab", time = "period", output = "abundance", fillweight = F)
#
#
# dim(a) == dim(b)
# length(which(is.na(a[,c(which(colnames(a)!='other'))]) != which(is.na(b[,c(which(colnames(b)!='other'))])))) == 0
# length(which(colnames(a) != colnames(b))) == 0
# length(which(a != c)) == 0

library(dplyr)
source('R/data_processing.R')
source('R/RodentData.R')

tests = function(level = 'site', type = 'rodents', length = 'all', unknowns = T, incomplete = T, shape = 'crosstab', time = 'period') {
  a = get_rodent_data(path = 'repo', level, type,
                      length, unknowns, incomplete,
                      shape, time, output = "abundance", fillweight = TRUE)
  b = get_rodent_data(path = 'repo', level, type,
                      length, unknowns, incomplete,
                      shape, time, output = "biomass", fillweight = TRUE)
  c = get_rodent_data(path = 'repo', level, type,
                      length, unknowns, incomplete,
                      shape, time, output = "abundance", fillweight = F)

  if(level != 'Plot') {
    if(shape == 'crosstab') {
    if ((dim(a) == dim(b) &&
       (length(which(which(is.na(a[,c(which(colnames(a)!='other'))])) != which(is.na(b[,c(which(colnames(b)!='other'))])))) == 0) &&
       (length(which(colnames(a) != colnames(b))) == 0) &&
       (length(which(a != c)) == 0))) {
    return('pass')
  } else {
    return('fail')
  }
  } else {
    if ((dim(a) == dim(b) &&
         (length(which(is.na(a)) != which(is.na(b))) == 0) &&
         (length(which(unique(a$species) != unique(b$species))) == 0) &&
         (length(which(a != c)) == 0))) {
      return('pass')
    } else {
      return('fail')
    }
  }
  } else {
    if(shape =='crosstab') {
      # crosstab tests
      if (
        (length(which(dim(a) != dim(b))) == 0) &&
        (length(which(a != c)) == 0) &&
        (length(which(which(is.na(a[,c(which(colnames(a)!='other'))])) != which(is.na(b[,c(which(colnames(b)!='other'))])))) == 0) &&
        (length(which(which(a[,c(which(colnames(a)!='other'))] == 0) != which(b[,c(which(colnames(b)!='other'))] == 0))) == 0) &&
        (length(which(colnames(a) != colnames(b))) == 0)
      ) {
        return('pass')
      } else {
        return('fail')
      }
    } else {
      # flat tests
      if(shape == 'flat'){
        joint_to_compare = full_join(a, b, by = c('species', 'period', 'plot'))
        if (
          (length(which(dim(a) != dim(b))) == 0) &&
          (nrow(joint_to_compare) == nrow(a)) &&
          (length(which((is.na(joint_to_compare$abundance)) != (is.na(joint_to_compare$biomass)))) == 0) &&
          (length(which(filter(joint_to_compare, abundance == 0, species != 'other') != filter(joint_to_compare, biomass == 0, species != 'other'))) == 0) &&
          (length(which(unique(a$species) != unique(b$species))) == 0) &&
          (length(which(a != c)) == 0)
        )
        return('pass')
      } else {
        return('fail')
      }
    }
  }
}

# most inclusive
one = tests(level = "Site", type = "Rodents", length = "all", unknowns = T, incomplete = T, shape = "crosstab", time = "period")
one.flat = tests(level = "Site", type = "Rodents", length = "all", unknowns = T, incomplete = T, shape = "flat", time = "period")

two = tests(level = "Treatment", type = "Rodents", length = "all", unknowns = T, incomplete = T, shape = "crosstab", time = "period")
two.flat = tests(level = "Treatment", type = "Rodents", length = "all", unknowns = T, incomplete = T, shape = "flat", time = "period")

three = tests(level = "Plot", type = "Rodents", length = "all", unknowns = T, incomplete = T, shape = "crosstab", time = "period")
three.flat = tests(level = "Plot", type = "Rodents", length = "all", unknowns = T, incomplete = T, shape = "flat", time = "period")
