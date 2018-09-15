#' @title Creates summaries of the Portal data
#'
#' @description This package is designed to be an interface to the Portal data, which resides online at \url{https://github.com/weecology/portalData}. Its contains a set of functions to download, clean, and summarize the data.
#'
#' @import ggplot2
#' @name portalr
#' @docType package
#' @keywords package
NULL

## quiets concerns of R CMD check re: variables used in NSE functions
if (getRversion() >= "2.15.1") utils::globalVariables(
  c(".", "airtemp", "battery_low", "battv", "censusdate", "censused",
    "censustarget", "commonname", "community", "count", "cover", "day", "day.x",
    "download_if_missing", "duration", "effort", "element", "flag", "granivore",
    "group", "height", "hfl", "juvwgt", "locally_measured", "ltag", "maxtemp",
    "meantemp", "meanwgt", "mintemp", "mo_diff", "month", "n", "newdate",
    "newmoondate", "newmoonnumber", "notes", "nplots", "nquads", "ntraps",
    "period", "phase", "PRCP", "precip", "precipitation", "qcflag", "quads",
    "rodent", "sampled", "season", "sex", "species", "speciescode", "stake",
    "start", "tag", "tmax", "TMAX", "tmin", "TMIN", "tobs", "TOBS", "treatment",
    "unidentified", "value", "value.x", "value.y", "values", "wgt", "year",
    "yearmon"))
