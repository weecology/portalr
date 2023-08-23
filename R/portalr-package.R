#' @keywords internal
#' @title Creates summaries of the Portal data
#'
#' @description This package is designed to be an interface to the Portal data, which resides online at \url{https://github.com/weecology/portalData}. Its contains a set of functions to download, clean, and summarize the data.
#'
#' @name portalr
#'
#' @importFrom lubridate "%m+%"
#' @importFrom rlang "!!" "!!!" ":=" .data
#' @importFrom utils head download.file read.csv unzip read.table tail
#' @importFrom stats median na.omit
#' @importFrom httr content GET stop_for_status

"_PACKAGE"

## quiets concerns of R CMD check re: variables used in NSE functions
if (getRversion() >= "2.15.1") utils::globalVariables(
    c(".", "n")
)

#' @title Return Citation for Portal Data
#'
#' @return An object of class "citation". For more details, see `citation()`
#' @export
get_dataset_citation <- function()
{
    path <- system.file("CITATION-PORTAL-DATA",
                        package = "portalr", mustWork = TRUE)
    utils::readCitationFile(path)
}
