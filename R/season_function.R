#' @title Add Seasons
#'
#' @description Higher-order data summaries, by 6-month seasons, 3-month seasons, or year.
#' Also applies specified functions to the specified summary level.
#'
#' @param data data frame containing columns: date, period, newmoonnumber, or year and month
#' @param season_level either year,
#'                       2: winter = Oct-March
#'                          summer = April-Sept
#'                       4: winter = Dec-Feb
#'                          spring = March-May
#'                          summer = Jun-Aug
#'                          fall = Sep-Nov
#' @param date_column either "date" (must be in format "y-m-d"), "period", "newmoonnumber",
#'                    or "yearmon" (data must contain "year" and "month")
#' @param summary_funs A function specified by its name (e.g. "mean").
#'                     Default is NA (returned with seasons added but not summarized).
#' @inheritParams summarize_rodent_data
#'
#' @return a data.frame with additional "season" and "year" column, and other columns summarized
#'         as specified. If no summary function is specified, "season" and "year" columns are
#'         added to original dataframe, as well as a "seasonyear" column which correctly assigns
#'         months to seasons for grouping (eg December 2000 in winter 2001, rather than winter 2000).
#'
#'
#' @export
#'
add_seasons <- function(data, level = "site", season_level = 2,
                        date_column = "yearmon", summary_funs = NA,
                        path = get_default_data_path(),
                        download_if_missing = TRUE, clean = TRUE)
{

  date_column <- tolower(date_column)
  if (!is.na(summary_funs)) {sumfun <- get(summary_funs)}
  grouping <- switch(level,
                     "plot" = c("seasonyear", "treatment", "plot"),
                     "treatment" = c("seasonyear", "treatment"),
                     "site" = c("seasonyear"))
  if("species" %in% colnames(data)) {grouping <- c(grouping, "species")}

  newmoons_table <- load_datafile(file.path("Rodents", "moon_dates.csv"),
                                  na.strings = "NA", path, download_if_missing)
  #### Get month and year column
  if (date_column == "period" || date_column == "newmoonnumber") {

    date_vars <- setdiff(c("newmoonnumber", "newmoondate", "censusdate", "period"),
                         date_column)
    full_data <- data %>%
      dplyr::left_join(newmoons_table, by = date_column)  %>%
      dplyr::mutate(year = lubridate::year(.data$censusdate),
                    month = lubridate::month(.data$censusdate)) %>%
      dplyr::select(-tidyselect::any_of(date_vars))

  } else if (date_column == "date") {
    full_data <- data %>%
      dplyr::mutate(year = lubridate::year(.data$date),
                    month = lubridate::month(.data$date))

    } else if (date_column == "yearmon") {
    full_data <- data

    } else {
    print("date_column must equal period, newmoonnumber, date, or yearmon")
    }

  #### Add seasons and summarize
  if (season_level == 2 || season_level == 4) {

    full_data$wateryear <- full_data$year

    if (season_level == 4)
    {
      seasons <- rep(c("winter", "spring", "summer", "fall"), each = 3)
      names(seasons) <- c(12, 1:11)
      full_data$wateryear[full_data$month == 12] <- full_data$year[full_data$month == 12] + 1

    } else if (season_level == 2) {
      seasons <- rep(c("winter", "summer"), each = 6)
      names(seasons) <- c(11:12, 1:10)
      full_data$wateryear[full_data$month %in% c(11,12)] <-
        full_data$year[full_data$month %in% c(11,12)] + 1
    }

    full_data$season <- seasons[match(unlist(full_data$month), names(seasons))]
    full_data$seasonyear <- paste(full_data$season, full_data$wateryear)
    full_data <- full_data %>% dplyr::select(-.data$wateryear) %>%
      dplyr::mutate(season = factor(.data$season, unique(seasons))) %>%
      dplyr::arrange(.data$year, .data$month)

    if (!is.na(summary_funs))
    {
      date_vars <- c("month", "day", "date", "newmoonnumber", "period",
                     "year", "season")
      full_data <- full_data %>% dplyr::ungroup() %>%
        dplyr::select(-tidyselect::any_of(date_vars)) %>%
        dplyr::group_by_at(grouping) %>%
        dplyr::summarize_all(list(sumfun), na.rm = TRUE) %>%
        dplyr::mutate(season = sub( " .*$", "", .data$seasonyear ),
                      year = sub( ".* ", "", .data$seasonyear )) %>%
        dplyr::mutate(season = factor(.data$season, unique(seasons))) %>%
        dplyr::group_by(.data$year, .data$season) %>%
        dplyr::select(-.data$seasonyear) %>%
        dplyr::arrange(.data$year, .data$season)
    }

  } else if (season_level == "year" && !(is.na(summary_funs))) {
    grouping <- grouping[-1]

      date_vars <- c("month", "day", "date", "newmoonnumber", "period")
      full_data <- full_data %>% dplyr::ungroup() %>%
        dplyr::select(-tidyselect::any_of(date_vars)) %>%
        dplyr::group_by_at(c("year", grouping)) %>%
        dplyr::summarize_all(list(sumfun), na.rm = TRUE) %>%
        dplyr::arrange(.data$year)

  } else {
    stop("`season_level` must equal 2, 4, or year")
  }

  return(full_data)
}

#' @rdname add_seasons
#'
#' @description \code{yearly} generates a table of yearly means
#'
#' @param ... arguments passed to \code{\link{add_seasons}}
#'
#' @examples
#' \donttest{
#' yearly(abundance(path = "repo", time = "newmoon"),
#'        date_column = "newmoonnumber", path = "repo")
#' }
#' @export
#'
yearly <- function(...) {
  add_seasons(..., season_level = "year", summary_funs = "mean")
}
