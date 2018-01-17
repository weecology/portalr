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
  e = get_rodent_data(path = 'repo', level, type,
                      length, unknowns, incomplete,
                      shape, time, output = "energy", fillweight = T)

  if(level != 'Plot') {
    if(shape == 'crosstab') {
    if ((length(which(dim(a) != dim(b))) == 0) &&
       (length(which(which(is.na(a[,c(which(colnames(a)!='other'))])) != which(is.na(b[,c(which(colnames(b)!='other'))])))) == 0) &&
       (length(which(colnames(a) != colnames(b))) == 0) &&
       (length(which(a != c)) == 0) &&
       (length(which(dim(a) != dim(e))) == 0) &&
       (length(which(which(is.na(a[,c(which(colnames(a)!='other'))])) != which(is.na(e[,c(which(colnames(e)!='other'))])))) == 0) &&
       (length(which(colnames(a) != colnames(e))) == 0))
  {
    return('pass')
  } else {
    return('fail')
  }
  } else {
    if ((dim(a) == dim(b)) &&
         (length(which(is.na(a)) != which(is.na(b))) == 0) &&
         (length(which(unique(a$species) != unique(b$species))) == 0) &&
         (length(which(a != c)) == 0) &&
        (dim(a) == dim(e)) &&
        (length(which(is.na(a)) != which(is.na(e))) == 0) &&
        (length(which(unique(a$species) != unique(e$species))) == 0) ){
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
        (length(which(dim(a) != dim(e))) == 0) &&
        (length(which(which(is.na(a[,c(which(colnames(a)!='other'))])) != which(is.na(e[,c(which(colnames(e)!='other'))])))) == 0) &&
        (length(which(which(a[,c(which(colnames(a)!='other'))] == 0) != which(e[,c(which(colnames(e)!='other'))] == 0))) == 0) &&
        (length(which(colnames(a) != colnames(e))) == 0)
      ) {
        return('pass')
      } else {
        return('fail')
      }
    } else {
      # flat tests
      if(shape == 'flat'){
        if (time == 'newmoon') time = 'newmoonnumber'
        joint_to_compare = full_join(a, b, by = c('species', time, 'plot'))
        joint_to_compare2 = full_join(a, e, by = c('species', time, 'plot'))
        if (
          (length(which(dim(a) != dim(b))) == 0) &&
          (nrow(joint_to_compare) == nrow(a)) &&
          (length(which((is.na(joint_to_compare$abundance)) != (is.na(joint_to_compare$biomass)))) == 0) &&
          (length(which(filter(joint_to_compare, abundance == 0, species != 'other') != filter(joint_to_compare, biomass == 0, species != 'other'))) == 0) &&
          (length(which(unique(a$species) != unique(b$species))) == 0) &&
          (length(which(a != c)) == 0)  &&
          (length(which(dim(a) != dim(e))) == 0) &&
          (nrow(joint_to_compare2) == nrow(a)) &&
          (length(which((is.na(joint_to_compare2$abundance)) != (is.na(joint_to_compare2$energy)))) == 0) &&
          (length(which(filter(joint_to_compare2, abundance == 0, species != 'other') != filter(joint_to_compare2, energy == 0, species != 'other'))) == 0) &&
          (length(which(unique(a$species) != unique(e$species))) == 0)
        )
        return('pass')
      } else {
        return('fail')
      }
    }
  }
}

# most inclusive
one = tests(level = "Site", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "crosstab", time = "period")
one.flat = tests(level = "Site", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "flat", time = "period")

two = tests(level = "Treatment", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "crosstab", time = "period")
two.flat = tests(level = "Treatment", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "flat", time = "period")

three = tests(level = "Plot", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "crosstab", time = "period")
three.flat = tests(level = "Plot", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "flat", time = "period")


# change time
four = tests(level = "Site", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "crosstab", time = "newmoon")
four.flat = tests(level = "Site", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "flat", time = "newmoon")

five = tests(level = "Treatment", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "crosstab", time = "newmoon")
five.flat = tests(level = "Treatment", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "flat", time = "newmoon")

six = tests(level = "Plot", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "crosstab", time = "newmoon")
six.flat = tests(level = "Plot", type = "Granivores", length = "all", unknowns = T, incomplete = T, shape = "flat", time = "newmoon")

# make longterm
seven = tests(level = "Site", type = "Granivores", length = "longterm", unknowns = T, incomplete = T, shape = "crosstab", time = "period")
seven.flat(level = "Site", type = "Granivores", length = "longterm", unknowns = T, incomplete = T, shape = "flat", time = "period")

eight = tests(level = "Treatment", type = "Granivores", length = "longterm", unknowns = T, incomplete = T, shape = "crosstab", time = "period")
eight.flat = tests(level = "Treatment", type = "Granivores", length = "longterm", unknowns = T, incomplete = T, shape = "flat", time = "period")

nine = tests(level = "Plot", type = "Granivores", length = "longterm", unknowns = T, incomplete = T, shape = "crosstab", time = "period")
nine.flat = tests(level = "Plot", type = "Granivores", length = "longterm", unknowns = T, incomplete = T, shape = "flat", time = "period")


# remove incompletes
ten = tests(level = "Site", type = "Granivores", length = "all", unknowns = T, incomplete = F, shape = "crosstab", time = "period")
ten.flat = tests(level = "Site", type = "Granivores", length = "all", unknowns = T, incomplete = F, shape = "flat", time = "period")

eleven = tests(level = "Treatment", type = "Granivores", length = "all", unknowns = T, incomplete = F, shape = "crosstab", time = "period")
eleven.flat = tests(level = "Treatment", type = "Granivores", length = "all", unknowns = T, incomplete = F, shape = "flat", time = "period")

twelve = tests(level = "Plot", type = "Granivores", length = "all", unknowns = T, incomplete = F, shape = "crosstab", time = "period")
twelve.flat = tests(level = "Plot", type = "Granivores", length = "all", unknowns = T, incomplete = F, shape = "flat", time = "period")


# remove unknowns and incompletes
thirteen = tests(level = "Site", type = "Granivores", length = "all", unknowns = F, incomplete = F, shape = "crosstab", time = "period")
thirteen.flat = tests(level = "Site", type = "Granivores", length = "all", unknowns = F, incomplete = F, shape = "flat", time = "period")

fourteen = tests(level = "Treatment", type = "Granivores", length = "all", unknowns = F, incomplete = F, shape = "crosstab", time = "period")
fourteen.flat = tests(level = "Treatment", type = "Granivores", length = "all", unknowns = F, incomplete = F, shape = "flat", time = "period")

fifteen = tests(level = "Plot", type = "Granivores", length = "all", unknowns = F, incomplete = F, shape = "crosstab", time = "period")
fifteen.flat = tests(level = "Plot", type = "Granivores", length = "all", unknowns = F, incomplete = F, shape = "flat", time = "period")

thirteen
thirteen.flat
fourteen
fourteen.flat
fifteen
fifteen.flat
