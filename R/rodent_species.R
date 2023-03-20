
#' @title Rodent species abbreviations
#'
#' @description Creates a simple \code{character} vector of abbreviations for the Portal \href{https://portal.naturecast.org/profiles.html}{Rodents}.
#'
#' @param path \code{character} Folder into which data will be downloaded.
#'
#' @param set \code{character} input of a specified set of species. Options include \code{"all"} (default, all species included) and \code{"forecasting"} (the species used in forecating pipelines). 
#'
#' @param total \code{logical} value indicating if \code{"total"} should be added or not.
#'
#' @param type \code{character} value indicating the output type. Current options include \code{'abbreviation'} (default, two-letter abbreviation), \code{'Latin'} (full scientific names), \code{'common'} (common names).
#'
#' @return \code{character} vector of species abbreviations.
#' 
#' @export
#'
rodent_species <- function (path = get_default_data_path( ),
                            type = "code",
                            set  = "all",
                            total = FALSE) {

  species_table <- load_datafile(datafile            = file.path("Rodents", "Portal_rodent_species.csv"),
                                 na.strings          = "", 
                                 path                = path, 
                                 download_if_missing = TRUE)

  if (tolower(set) %in% c("forecast", "forecasting")) {

    in_abb     <- c("BA", "DM", "DO", "DS", "NA", "OL", "OT", "PB", "PE", "PF", "PH", "PL", "PM", "PP", "RF", "RM", "RO", "SF", "SH", "SO")
    species_in <- species_table$speciescode %in% in_abb

  } else if (tolower(set) %in% c("all")) {

    species_in <- species_table$rodent == 1

  } else {

    stop("set not recognized, should be `all` or `forecasting`")

  }


  table_in   <- species_table[species_in, ]
  code       <- table_in$speciescode
  latin      <- table_in$scientificname
  common     <- table_in$commonname

  if (tolower(type) %in% c("code", "abbreviation", "abbr")) {
 
    out <- code

  } else if (tolower(type) %in% c("latin", "scientific")) {
 
    out <- latin

  } else if (tolower(type) %in% c("common")) {
 
    out <- common

  } else {

    stop("type not recognized, should be `code`, `latin`, or `common`")

  }

  if (total) {

    out <- c(out, "total")

  }

  out

}