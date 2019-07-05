#' @title Creates summaries of the Portal data
#'
#' @description This package is designed to be an interface to the Portal data, which resides online at \url{https://github.com/weecology/portalData}. Its contains a set of functions to download, clean, and summarize the data.
#'
#' @name portalr
#' @docType package
#' @keywords package
#'
#' @importFrom lubridate "%m+%"
#' @importFrom rlang "!!" "!!!" ":=" .data
#' @importFrom utils head download.file read.csv unzip read.table tail
#' @importFrom stats median na.omit

NULL

## quiets concerns of R CMD check re: variables used in NSE functions
if (getRversion() >= "2.15.1") utils::globalVariables(
  c(".", "n")
  )
