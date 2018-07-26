#' @importFrom magrittr "%>%"
#' @importFrom rlang quos
#' @importFrom utils head
#'
#' @title Add Seasons
#'
#' @description Higher-order data summaries, by 6-month seasons, 3-month seasons, or year.
#' Also applies specified functions to the specified summary level.
#'
#' @param data data frame containing columns: date, period, newmoonnumber, or year and month
#' @param level "plot, "treatment" or "site"
#' @param season_level either year,
#'                       2: winter = Oct-March
#'                          summer = April-Sept
#'                       4: winter = Dec-Feb
#'                          spring = March-May
#'                          summer = Jun-Aug
#'                          fall = Sep-Nov
#' @param date_column either "date" (must be in format "y-m-d"), "period", "newmoonnumber",
#'                    or "yearmon" (data must contain "year" and "month")
#' @param summarize A function or list of functions specified by their name (e.g. "mean").
#'                  Default is NA (returned with seasons added but not summarized).
#' @param path path to location of downloaded Portal data; or "repo" to
#'             retrieve data from github repo
#' @inheritParams load_data
#'
#' @return a data.frame with additional "season" and "year" column, and other columns summarized as specified
#'
#'
#' @export
#'
add_seasons <- function(data, level = "site", season_level = 2,
                        date_column = "yearmon", summarize = NA, path = "~",
                        download_if_missing = TRUE, clean = TRUE)
{

  date_column <- tolower(date_column)
  summarize <- tolower(summarize)
  if (!is.na(summarize)) {sumfun <- get(summarize)}
  if (level == "plot") {grouping <- quos(year, season, treatment, plot)
     } else if (level=="treatment") {grouping <- quos(year, season, treatment)
     } else {grouping <- quos(year,season)}
  if("species" %in% colnames(data)) {grouping <- c(grouping,quos(species))}

  data_tables <- load_data(path, download_if_missing = download_if_missing,
                           clean = clean)

  #### Get month and year column
  if (date_column == "period") {
    full_data = data %>% dplyr::left_join(data_tables$newmoons_table, by="period")  %>%
      dplyr::mutate(year = lubridate::year(censusdate),
                    month = lubridate::month(censusdate)) %>%
      dplyr::select(-newmoonnumber, -newmoondate, -censusdate) %>%
      dplyr::group_by(year, month)

    } else if(date_column == "newmoonnumber") {
    full_data = data %>% dplyr::left_join(data_tables$newmoons_table, by="newmoonnumber")  %>%
      dplyr::mutate(year = lubridate::year(censusdate),
                    month = lubridate::month(censusdate)) %>%
      dplyr::select(-newmoondate, -censusdate, -period) %>%
      dplyr::group_by(year, month)

    } else if(date_column == "date") {
    full_data = data %>%
      dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
      dplyr::group_by(year,month)

    } else if(date_column == "yearmon") {
    full_data = data %>%
      dplyr::group_by(year,month)

    } else {
    print("date_column must equal period, newmoonnumber, date, or yearmon")
  }

 #### Add seasons and summarize
  if (season_level == 4) {

    seasons <- rep(c("winter","spring","summer","fall"), each = 3)
    names(seasons) <- c(12,1:11)

    full_data$season <- seasons[match(unlist(full_data$month), names(seasons))]

    if(!(is.na(summarize))) {
      full_data = full_data %>% dplyr::group_by(!!!grouping) %>%
        dplyr::summarize_all(dplyr::funs(sumfun), na.rm = TRUE) %>%
        dplyr::select(-month, -dplyr::contains("day"), -dplyr::contains("newmoonnumber"),
                      -dplyr::contains("date"), -dplyr::contains("period"))
    }
  } else if (season_level == 2) {

      seasons <- rep(c("winter","summer"), each = 6)
      names(seasons) <- c(11:12,1:10)

      full_data$season <- seasons[match(unlist(full_data$month), names(seasons))]

      if(!(is.na(summarize))) {
        full_data = full_data %>% dplyr::group_by(!!!grouping) %>%
          dplyr::summarize_all(dplyr::funs(sumfun), na.rm = TRUE) %>%
          dplyr::select(-month,-dplyr::contains("day"), -dplyr::contains("newmoonnumber"),
                        -dplyr::contains("date"), -dplyr::contains("period"))
      }
    } else if (season_level == "year" && !(is.na(summarize))) {

    full_data = full_data %>% dplyr::group_by(!!!grouping[-2]) %>%
      dplyr::summarize_all(dplyr::funs(sumfun), na.rm = TRUE) %>%
      dplyr::select(-month, -dplyr::contains("day"), -dplyr::contains("newmoonnumber"),
                    -dplyr::contains("date"), -dplyr::contains("period"))

  } else {
    print("season_level must equal 2, 4, or year")
  }
  return(full_data)
}

#' @rdname add_seasons
#'
#' @description \code{yearly} generates a table of yearly means
#'
#' @param ... arguments passed to \code{\link{add_seasons}}
#'
#' @examples yearly(abundance(path = "repo", time = "newmoon"), date_column = "newmoonnumber", path = "repo")
#'
#' @export
#'
yearly <- function(...) {
  add_seasons(..., season_level="year", summarize="mean")
}
