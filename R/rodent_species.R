
#' @title Rodent species abbreviations
#'
#' @description Creates a simple \code{character} vector of abbreviations for the Portal \href{http://portal.naturecast.org/profiles.html}{Rodents}.
#'
#' @param path \code{character} Folder into which data will be downloaded.
#'
#' @param set \code{character} input of a specified set of species. Options include \code{"all"} (default, all species included) and \code{"forecasting"} (the species used in forecating pipelines).
#'
#' @param total \code{logical} value indicating if \code{"total"} should be added or not.
#'
#' @param type \code{character} value indicating the output type. Current options include \code{'abbreviation'} or \code{'code'} (default, two-letter abbreviation), \code{'g_species'} (abbreviated genus and species), \code{'Latin'} (full scientific names), \code{'common'} (common names), and  \code{'table'} (a \code{data.frame} of all the options).
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
  g_species  <- paste0(substr(latin, 1, 1), ". ", unlist(lapply(strsplit(latin, " "), getElement, 2)))


  if (total) {

    code      <- c(code, "total")
    latin     <- c(latin, "Total abundance")
    common    <- c(common, "Total abundance")
    g_species <- c(g_species, "Total")

  }

  table_out  <- data.frame(code = code, Latin = latin, common = common, g_species = g_species)

  if (tolower(type) %in% c("code", "abbreviation", "abbr")) {

    out <- code

  } else if (tolower(type) %in% c("latin", "scientific")) {

    out <- latin

  } else if (tolower(type) %in% c("common")) {

    out <- common

  } else if (tolower(type) %in% c("g_species")) {

    out <- g_species

  } else if (tolower(type) %in% c("table")) {

    out <- table_out

  } else {

    stop("type not recognized, should be `code`, `abbreviation`, `Latin`, `scientific`, `g_species`, `common`, or `table`")

  }


  out

}



#' @rdname rodent_species
#'
#' @export
#'
forecasting_species <- function (path  = get_default_data_path( ),
                                 total = FALSE,
                                 type  = "abbreviation") {

  rodent_species(path  = path,
                 set   = "forecasting",
                 total = total,
                 type  = type)

}




#' @title Conform NA entries to "NA" entries
#'
#' @description Given the species abbreviation \emph{Neotoma albigula} (NA), when data are read in, there can be an \code{NA} when it should be an \code{"NA"}. This function conforms the entries to be proper character values.
#'
#' @param dfv Either [1] a \code{data.frame} containing \code{colname} as a column with \code{NA}s that need to be conformed to \code{"NA"}s or [2] a vector with \code{NA}s that need to be conformed to \code{"NA"}s.
#'
#' @param colname \code{character} value of the column name in \code{tab} to conform the \code{NA}s to \code{"NA"}s.
#'
#' @return \code{x} with any \code{NA} in \code{colname} replaced with \code{"NA"}.
#'
#' @examples
#'  na_conformer(c("a", "b", NA, "c"))
#'
#' @export
#'
na_conformer <- function(dfv, colname = "species"){

  if (is.vector(dfv)) {

    naentries <- which(is.na(dfv))
    dfv[naentries] <- "NA"

  } else if (is.data.frame(dfv)) {

    nasppname <- which(is.na(dfv[ , colname]))
    class(dfv[ , colname]) <- "character"
    if (length(nasppname) > 0) {

      dfv[nasppname, colname] <- "NA"

    }

  }

  dfv

}
